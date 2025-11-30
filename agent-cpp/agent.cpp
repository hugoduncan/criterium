#include "jni.h"
#include <algorithm>
#include <array>
#include <condition_variable>
#include <cstdlib>
#include <cstring>
#include <functional>
#include <iostream>
#include <jvmti.h>
#include <map>
#include <memory>
#include <mutex>
#include <pthread.h>
#include <queue>
#include <string>
#include <thread>
#include <utility>
#include <variant>

#ifdef DEBUG
#define DEBUG_PRINT(...) (std::cout << __VA_ARGS__)
#define DEBUG_PRINTLN(...) (std::cout << __VA_ARGS__ << '\n')
#else
#define DEBUG_PRINT(...)
#define DEBUG_PRINTLN(...)
#endif

const jint MAX_FRAMES = 1024;

// NOLINTNEXTLINE(bugprone-branch-clone)
void debug_print_jvmti_err([[maybe_unused]] jvmtiError err) {
#ifdef DEBUG
  switch (err) {
  case JVMTI_ERROR_NONE:
    break;
  case JVMTI_ERROR_INVALID_THREAD:
    DEBUG_PRINT(">> Invalid thread\n");
    break;
  case JVMTI_ERROR_NULL_POINTER:
    DEBUG_PRINT(">> Invalid NULL jvmtiEnv or argument\n");
    break;
  case JVMTI_ERROR_INVALID_ENVIRONMENT:
    DEBUG_PRINT(">> JVMTI environment is invalid\n");
    break;
  case JVMTI_ERROR_ILLEGAL_ARGUMENT:
    DEBUG_PRINT(">> Invalid parameters (e.g., non-NULL thread for global event)\n");
    break;
  case JVMTI_ERROR_WRONG_PHASE:
    DEBUG_PRINT(">> JVM is in wrong phase (e.g., after VM death)\n");
    break;
  case JVMTI_ERROR_INVALID_EVENT_TYPE:
    DEBUG_PRINT(">> Invalid event type\n");
    break;
  case JVMTI_ERROR_INVALID_CLASS:
    DEBUG_PRINT(">> Invalid class\n");
    break;
  case JVMTI_ERROR_THREAD_NOT_ALIVE:
    DEBUG_PRINT(">> Thread not alive\n");
    break;
  case JVMTI_ERROR_MUST_POSSESS_CAPABILITY:
    DEBUG_PRINT(">> Must possess capability\n");
    break;
  case JVMTI_ERROR_ABSENT_INFORMATION:
    DEBUG_PRINT(">> Absent information\n");
    break;
  case JVMTI_ERROR_UNATTACHED_THREAD:
    DEBUG_PRINT(">> Unattached thread\n");
    break;
  case JVMTI_ERROR_NATIVE_METHOD:
    DEBUG_PRINT(">> Native Thread\n");
    break;
  case JVMTI_ERROR_INTERNAL:
    DEBUG_PRINT(">> Internal JVM error occurred\n");
    break;
  default:
    DEBUG_PRINTLN(">> Unexpected error " << err);
    break;
  }
#endif
}

static constexpr char const* const allocation_start_marker =
  "Lcriterium/agent/Agent$AllocationStartMarker;";

static constexpr char const* const allocation_finish_marker =
  "Lcriterium/agent/Agent$AllocationFinishMarker;";

static constexpr char const* const allocation_class_name =
  "Lcriterium/agent/Allocation;";

static constexpr char const* IFn  = "clojure/lang/IFn";

static constexpr char const *invoke_sig =
  "(Ljava/lang/Object;"
  "Ljava/lang/Object;"
  "Ljava/lang/Object;"
  "Ljava/lang/Object;"
  "Ljava/lang/Object;"
  "Ljava/lang/Object;"
  "Ljava/lang/Object;"
  ")Ljava/lang/Object;";

static constexpr char const *agent_allocation_class_args =
  "(Ljava/lang/String;"
  "JLjava/lang/String;"
  "Ljava/lang/String;"
  "Ljava/lang/String;"
  "JLjava/lang/String;"
  "Ljava/lang/String;"
  "Ljava/lang/String;"
  "JJJ)V";

static constexpr char const* data8_sig =
  "(Ljava/lang/Object;"
  "Ljava/lang/Object;"
  "Ljava/lang/Object;"
  "Ljava/lang/Object;"
  "Ljava/lang/Object;"
  "Ljava/lang/Object;"
  "Ljava/lang/Object;"
  "Ljava/lang/Object;)V";

static constexpr char const *no_file_name = "NO_SOURCE";

jclass ifn(JNIEnv* env) {
  auto *ifn = (env)->FindClass(IFn);
  if (ifn == nullptr) {
    std::cout << "Ifn not found\n";
  }else {
    // printf("IFn found \n");
  }
  return ifn;
}

jmethodID invoke_method_id(JNIEnv* env) {
  auto *klass = ifn(env);
  auto *invoke = (env)->GetMethodID(klass, "invoke", invoke_sig);
  if (invoke == NULL) {
      std::cout << "invoke method not found\n";
    } else {
    // DEBUG_PRINT("invoke method found\n");
    }
  return invoke;
}

jmethodID class_invoke_method_id(JNIEnv* env, jclass klass) {
  auto *invoke = (env)->GetMethodID(klass, "invoke", invoke_sig);
  if (invoke == NULL) {
    std::cout << "invoke method not found\n";
    } else {
    // DEBUG_PRINT("invoke method found\n");
    }
  return invoke;
}

// NOLINTBEGIN(performance-enum-size)
// Using unscoped enums for implicit conversion to jlong
enum States : jlong {  // NOLINT(cppcoreguidelines-use-enum-class)
  passive = 0,
  allocation_tracing_starting = 10,
  allocation_tracing_active = 11,
  allocation_tracing_stopping = 15,
  allocation_tracing_flushing = 16,
  allocation_tracing_flushed = 17,
  allocation_tracing_reporting = 18,
  allocation_tracing_reported = 19,
};

enum Commands : jlong {  // NOLINT(cppcoreguidelines-use-enum-class)
  ping = 0,
  sync_state = 1,
  start_allocation_tracing = 10,
  stop_allocation_tracing = 11,
  report_allocation_tracing = 12
};
// NOLINTEND(performance-enum-size)

// Event/Command structures
struct AllocationEvent {
  jobject object;
  jclass object_klass;
  jthread thread;
  jlong size;
  jlong tag;

  void delete_global_refs(JNIEnv* env) const {
    env->DeleteGlobalRef(object_klass);
    env->DeleteGlobalRef(object);
    env->DeleteGlobalRef(thread);
  }
};

struct ObjectFreeEvent {
    jlong tag;
};

struct Command {
    jlong cmd;
};

// Queue message type
using Message = std::variant<AllocationEvent, ObjectFreeEvent, Command>;

// Thread-safe queue
class MessageQueue {
    std::queue<Message> queue;
    std::mutex mutex;
    std::condition_variable cond;
    bool stopped = false;

public:
    void push(Message msg) {
        std::lock_guard<std::mutex> lock(mutex);
        queue.push(msg);
        cond.notify_one();
    }

    bool pop(Message& msg) {
        std::unique_lock<std::mutex> lock(mutex);
        while (queue.empty() && !stopped) {
            cond.wait(lock);
        }
        if (stopped && queue.empty()) {
            return false;
        }
        msg = queue.front();
        queue.pop();
        return true;
    }

    void stop() {
        std::lock_guard<std::mutex> lock(mutex);
        stopped = true;
        cond.notify_all();
    }
};

// Structure used to record allocations
struct alloc_rec {
  std::string obj_class;
  jlong obj_size;

  std::string call_class;
  std::string call_method;
  std::string call_file;
  jlong call_line;

  std::string alloc_class;
  std::string alloc_method;
  std::string alloc_file;
  jlong alloc_line;

  jlong thread_id;
  jlong freed{};

  jlong tag;
  bool start_marker{};
  bool disable_marker{};

  alloc_rec(char const * obj_class,
            jlong obj_size,
	    // NOLINTNEXTLINE(bugprone-easily-swappable-parameters)
            char const * call_class,
            char const * call_method,
            char const * call_file,
            jlong call_line,
	    // NOLINTNEXTLINE(bugprone-easily-swappable-parameters)
            char const * alloc_class,
            char const * alloc_method,
            char const * alloc_file,
	    // NOLINTNEXTLINE(bugprone-easily-swappable-parameters)
            jlong alloc_line,
            jlong thread_id,
            jlong tag)
    : obj_class(obj_class),
      obj_size(obj_size),
      call_class(call_class == nullptr ? "" : call_class),
      call_method(call_method == nullptr ? "" : call_method),
      call_file(call_file == NULL ? no_file_name : call_file),
      call_line(call_line),
      alloc_class(alloc_class == nullptr ? "" : alloc_class),
      alloc_method(alloc_method == nullptr ? "" : alloc_method),
      alloc_file(alloc_file == NULL ? no_file_name : alloc_file),
      alloc_line(alloc_line),
      thread_id(thread_id),
      tag(tag)
  { }
};

typedef std::vector<std::unique_ptr<alloc_rec>> allocs_t;
typedef std::map<jlong, alloc_rec*> allocs_by_tag_t;
/* static allocs_t allocs; */
/* static auto allocs_by_tag = allocs_by_tag_t(); */


auto all_tags(allocs_t& allocs) {
  auto tags=std::vector<jlong>(allocs.size());
  std::transform(allocs.begin(),
                   allocs.end(),
                   std::back_inserter(tags),
                   std::mem_fn(&alloc_rec::tag));
  return tags;
}


class VMContext {
private:
  bool _vm_dead = true;
  JavaVM* cached_vm = nullptr;

  VMContext() {}

public:
  static VMContext& getInstance() {
    static VMContext instance;
    return instance;
  }

  bool vm_dead() const {
    return _vm_dead;
  }

  void set_vm_dead() {
    _vm_dead = true;
  }

  JavaVM* vm() const {
    return cached_vm;
  }

  void cache_vm(JNIEnv *env) {
    env->GetJavaVM(&cached_vm);
    _vm_dead = false;
  }

  template <typename T>
  class local_ref {
    T _ref;
    JNIEnv* _env;
  public:
    local_ref(const local_ref &) = delete;
    local_ref &operator=(const local_ref &) = delete;
    local_ref &operator=(local_ref &&) = delete;
    local_ref(JNIEnv *env, T ref) : _ref(std::move(ref)), _env(env) {}
    local_ref(JNIEnv* env) : _ref(NULL), _env(env) {}
    local_ref(local_ref&& other) noexcept
      : _ref(std::exchange(other._ref, NULL)),
	_env(std::exchange(other._env, NULL))
    {}
    ~local_ref() {
      if (_ref != NULL && !VMContext::getInstance().vm_dead()) {
	_env->DeleteLocalRef(_ref);
	_ref = NULL;
      }
    }
    local_ref& operator = (T ref) { _ref = ref; return *this; }
    T* operator & () { return &_ref; }
    operator T& () { return _ref; }
  };

  template <typename T>
  class global_ref {
    T _ref;
    JNIEnv* _env;
  public:
    global_ref(const global_ref &) = delete;
    global_ref(global_ref &&) = delete;
    global_ref &operator=(const global_ref &) = delete;
    global_ref &operator=(global_ref &&) = delete;
    global_ref(JNIEnv *env, T ref)
        : _ref(static_cast<T>(env->NewGlobalRef(ref))), _env(env) {}
    ~global_ref() {
      if (_ref != NULL && !VMContext::getInstance().vm_dead()) {
	_env->DeleteGlobalRef(_ref);
	_ref = NULL;
      }
    }
    T* operator & () { return &_ref; }
    operator T& () { return _ref; }
  };

  template <typename T> local_ref<T> mk_local_ref(JNIEnv* env, T value) {
    return local_ref<T>(env, value);
  }

  void attach_current_thread_as_daemon(JNIEnv **env) const {
    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-reinterpret-cast)
    auto *penv = reinterpret_cast<void **>(env);
    // NOLINTNEXTLINE(clang-analyzer-core.CallAndMessage)
    auto res = cached_vm->AttachCurrentThreadAsDaemon(penv, nullptr);
    if (res != JNI_OK) {
      DEBUG_PRINT("Failed to attach queue consumer thread to JVM\n");
      return;
    }
  }
};

void start_queue_consumer();

class AgentContext {

private:
  jvmtiEnv* jvmti = nullptr;

  static jvmtiEnv* get_jvmti() { return getInstance().jvmti; }

public:
  template <typename T>
  class allocated  {
    T _ptr;
  public:
    allocated() noexcept : _ptr(0) {}
    allocated(const allocated &) = delete;
    allocated &operator=(const allocated &) = delete;
    allocated &operator=(allocated &&) = delete;
    allocated(allocated<T> &&other) noexcept
        : _ptr(std::exchange(other._ptr, static_cast<T>(0))) {}
    ~allocated() { get_jvmti()->Deallocate((unsigned char*)_ptr);}
    operator T& () { return _ptr; }
    T* operator & () { return &_ptr; }
  };

  class raw_monitor {
    jrawMonitorID id;
  public:
    raw_monitor(const raw_monitor &) = default;
    raw_monitor(raw_monitor &&) = delete;
    raw_monitor &operator=(const raw_monitor &) = default;
    raw_monitor &operator=(raw_monitor &&) = delete;
    raw_monitor(jrawMonitorID mid)
        : id(mid)
          { get_jvmti()->RawMonitorEnter(mid); }
    ~raw_monitor() {
      get_jvmti()->RawMonitorExit(id);
    }
  };

private:
  MessageQueue message_queue;

  // Locks
  jrawMonitorID tag_lock{};

  // Method/class refs
  jmethodID thread_getId_method{};

  jlong next_object_tag = 0; // read and inc with next_tag()

  jlong next_tag() {
    auto monitor = std::make_unique<raw_monitor>(tag_lock);
    return next_object_tag++;
  }

  static void set_callbacks(jvmtiEventCallbacks& callbacks);

  AgentContext() = default;

public:
  static AgentContext& getInstance() {
    static AgentContext instance;
    return instance;
  }

  void initialize(JavaVM *jvm);

  MessageQueue& get_message_queue() { return message_queue; }

  void vm_death([[maybe_unused]] jvmtiEnv* jvmti_env,
                [[maybe_unused]] JNIEnv* env) {
    VMContext::getInstance().set_vm_dead();
    message_queue.stop();
    jvmti = nullptr;
  }

  jint on_load(JavaVM* jvm,
               [[maybe_unused]] char* options,
               [[maybe_unused]] void* reserved) {
    std::cout << "Loading criterium agent\n";

    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-reinterpret-cast)
    jvm->GetEnv(reinterpret_cast<void **>(&jvmti), JVMTI_VERSION_1_0);

    jvmti->CreateRawMonitor("tag_lock", &tag_lock);

    jvmtiCapabilities capabilities = {};
    capabilities.can_generate_sampled_object_alloc_events = 1;
    capabilities.can_generate_field_modification_events = 1;
    capabilities.can_get_line_numbers = 1;
    capabilities.can_get_source_file_name = 1;
    capabilities.can_tag_objects = 1;
    capabilities.can_generate_object_free_events = 1;

    {
      auto err = jvmti->AddCapabilities(&capabilities);
      if (err != JVMTI_ERROR_NONE) {
	std::cout << "Failed to add capabilities: " << err << '\n';
	debug_print_jvmti_err(err);
      }
    }

    jvmtiEventCallbacks callbacks = {};
    set_callbacks(callbacks);
    jvmti->SetEventCallbacks(&callbacks, sizeof(callbacks));
    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-vararg)
    jvmti->SetEventNotificationMode(JVMTI_ENABLE,
				    JVMTI_EVENT_FIELD_MODIFICATION,
				    nullptr);

    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-vararg)
    jvmti->SetEventNotificationMode(JVMTI_ENABLE,
				    JVMTI_EVENT_VM_INIT,
				    nullptr);
    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-vararg)
    jvmti->SetEventNotificationMode(JVMTI_ENABLE,
				    JVMTI_EVENT_VM_DEATH,
				    nullptr);

    // Set this as early as possible
    set_sampling_interval(0);

    DEBUG_PRINT("\nallocation sampler loaded\n");

    return JNI_OK;
  }

  jint on_attach(JavaVM* jvm, char* options, void* reserved) {
    if (jvmti != NULL) {
      return 0;
    }
    return on_load(jvm, options, reserved);
  }

  void vm_init([[maybe_unused]] jvmtiEnv* jvmti_env,
               JNIEnv* env, jthread thread) {
    // set this as early as possible.  repeated here, as not sure it works from
    // OnLoad.
    set_sampling_interval(0);

    // Get JavaVM pointer
    VMContext::getInstance().cache_vm(env);

    jclass thread_klass = env->GetObjectClass(thread);
    thread_getId_method = env->GetMethodID(thread_klass, "getId", "()J");

    // Start queue consumer thread
    start_queue_consumer();
    message_queue.push(Command{sync_state});
  }

  void sampled_object_alloc(jvmtiEnv* jvmti, JNIEnv* env,
			    jthread thread, jobject object,
			    jclass object_klass, jlong size) {
    auto tag = next_tag();
    auto err = jvmti->SetTag(object, tag);
    if (err != JVMTI_ERROR_NONE) {
      std::cout << "Failed to tag object: " << err << '\n';
      debug_print_jvmti_err(err);
    }
    message_queue.push(AllocationEvent{
	env->NewGlobalRef(object),
	// NOLINTNEXTLINE(cppcoreguidelines-pro-type-reinterpret-cast)
	reinterpret_cast<jclass>(env->NewGlobalRef(object_klass)),
	static_cast<jthread>(env->NewGlobalRef(thread)),
	size,
	tag
      });
  }

  void object_free([[maybe_unused]] jvmtiEnv* jvmti_env, jlong tag) {
    message_queue.push(ObjectFreeEvent{tag});
  }

  void agent_command([[maybe_unused]] JNIEnv* env,
                     [[maybe_unused]] jclass klass,
                     jlong cmd) {
    if (cmd != 1) {
      DEBUG_PRINTLN("Agent command: " << cmd);
    }
    message_queue.push(Command{cmd});
  }

  jlong thread_id(JNIEnv *env, jthread thread) {
    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-vararg)
    return env->CallLongMethod(thread, thread_getId_method);
  }

  bool get_class_signature(jclass klass, char** class_sig) {
    auto err = jvmti->GetClassSignature(klass, class_sig, NULL);
    if ( err != 0) {
      std::cout << "Failed to get class name\n";
      debug_print_jvmti_err(err);
      return false;
    }
    return true;
  }

  bool get_source_file_name(jclass klass, char** source_name) {
    auto err = jvmti->GetSourceFileName(klass, source_name);
    if (err != 0) {
      std::cout << "Failed to get source file name\n";
      debug_print_jvmti_err(err);
      return false;
    }
    return true;
  }

  bool get_method_declaring_class(jmethodID method,
                                         jclass* declaring_class) {
    auto err = jvmti->GetMethodDeclaringClass(method, declaring_class);
    if (err != 0) {
      std::cout << "Failed to get method declaring class\n";
      debug_print_jvmti_err(err);
      return false;
    }
    return true;
  }

  bool get_method_name(jmethodID method, char** method_name) {
    auto err = (jvmti)->GetMethodName(method, method_name, NULL, NULL);
    if (err != 0) {
      std::cout << "Failed to get method name\n";
      debug_print_jvmti_err(err);
      return false;
    }
    return true;
  }

  bool get_stack_trace(jthread thread, jvmtiFrameInfo *frames,
                              jint* count) {
    auto err = jvmti->GetStackTrace(thread, 0, MAX_FRAMES, frames, count);
    if (err != 0) {
      std::cout << "Failed to get stack\n";
      debug_print_jvmti_err(err);
      return false;
    }
    return true;
  }

  bool get_line_number_table(jmethodID method, jint *entry_count,
                                    jvmtiLineNumberEntry** line_table) {
    auto err = jvmti->GetLineNumberTable(method, entry_count, line_table);
    if (err != 0) {
      if (err != JVMTI_ERROR_NATIVE_METHOD) {
	std::cout << "Failed to get line number table\n";
	debug_print_jvmti_err(err);
      }
      return false;
    }
    return true;
  }

  bool set_tag(jobject object, [[maybe_unused]] jlong tag) {
    auto err = jvmti->SetTag(object, 0);
    if (err != 0) {
      std::cout << "Failed to set tag\n";
      debug_print_jvmti_err(err);
      return false;
    }
    return true;
  }

  bool get_objects_with_tags(jint ntags, jlong *tag_data, jint *count,
                                    jobject** objects, jlong** tags) {
    auto err = jvmti->GetObjectsWithTags(ntags, tag_data, count, objects, tags);
    if (err != 0) {
      std::cout << "Failed to get objects with tags\n";
      debug_print_jvmti_err(err);
      return false;
    }
    return true;
  }

  static void call_static_void_method(JNIEnv *env, jclass klass,
                                      jmethodID method, jobject arg) {
    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-vararg)
    env->CallStaticVoidMethod(klass, method, arg);
  }

  template <typename... Args>
  static jobject new_object(JNIEnv *env, jclass klass, jmethodID method,
			    Args... args) {
    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-vararg)
    return env->NewObject(klass, method, args...);
  }

  void set_sampling_interval(jint n) {
    // NOLINTNEXTLINE(clang-analyzer-core.CallAndMessage)
    auto err = jvmti->SetHeapSamplingInterval(n);
    if (err != JVMTI_ERROR_NONE) {
      std::cout << "Failed to set the sampling interval: " << err << '\n';
      debug_print_jvmti_err(err);
    }
  }

  void enable_sampled_object_alloc() {
    DEBUG_PRINT("Enabling JVMTI_EVENT_SAMPLED_OBJECT_ALLOC\n");
    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-vararg)
    auto err = jvmti->SetEventNotificationMode(JVMTI_ENABLE,
					       JVMTI_EVENT_SAMPLED_OBJECT_ALLOC,
					       nullptr);
    if (err != JVMTI_ERROR_NONE) {
      std::cout << "Failed to enable allocation sampling " << err << '\n';
      debug_print_jvmti_err(err);
    }
  }

  void enable_object_free() {
    DEBUG_PRINT("Enabling JVMTI_EVENT_OBJECT_FREE\n");
    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-vararg)
    auto err = jvmti->SetEventNotificationMode(JVMTI_ENABLE,
					  JVMTI_EVENT_OBJECT_FREE,
					  nullptr);
    if (err != JVMTI_ERROR_NONE) {
      std::cout << "Failed to enable object free notifications " << err << '\n';
      debug_print_jvmti_err(err);
    }
  }

  void disable_sampled_object_alloc() {
    DEBUG_PRINT("Enabling JVMTI_EVENT_SAMPLED_OBJECT_ALLOC\n");
    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-vararg)
    auto err = jvmti->SetEventNotificationMode(JVMTI_DISABLE,
					       JVMTI_EVENT_SAMPLED_OBJECT_ALLOC,
					       nullptr);
    if (err != JVMTI_ERROR_NONE) {
      std::cout << "Failed to disable allocation sampling " << err << '\n';
      debug_print_jvmti_err(err);
    }
  }

  void disable_object_free() {
    DEBUG_PRINT("Enabling JVMTI_EVENT_OBJECT_FREE\n");
    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-vararg)
    auto err = jvmti->SetEventNotificationMode(JVMTI_DISABLE,
					  JVMTI_EVENT_OBJECT_FREE,
					  nullptr);
    if (err != JVMTI_ERROR_NONE) {
      std::cout << "Failed to disable object free notifications "
                << err
		<< '\n';
      debug_print_jvmti_err(err);
    }
  }

};


// Modified event callbacks to use queue
void JNICALL SampledObjectAlloc(jvmtiEnv* jvmti, JNIEnv* env,
                               jthread thread, jobject object,
                               jclass object_klass, jlong size) {
  auto& context = AgentContext::getInstance();
  context.sampled_object_alloc(jvmti, env, thread, object, object_klass, size);
}

void JNICALL ObjectFree(jvmtiEnv *jvmti, jlong tag) {
  // DEBUG_PRINT("ObjectFree\n");
  auto& context = AgentContext::getInstance();
  context.object_free(jvmti, tag);
}

void JNICALL VMInit(jvmtiEnv* jvmti, JNIEnv* env, jthread thread) {
  auto& context = AgentContext::getInstance();
  context.vm_init(jvmti, env, thread);
}

void JNICALL VMDeath(jvmtiEnv* jvmti, JNIEnv* env) {
  auto& context = AgentContext::getInstance();
  context.vm_death(jvmti, env);
}

JNIEXPORT jint JNICALL
Agent_OnLoad(JavaVM* jvm, char* options, void* reserved) {
  DEBUG_PRINT("Loading criterium agent\n");
  auto& context = AgentContext::getInstance();
  return context.on_load(jvm, options, reserved);
}

JNIEXPORT jint JNICALL
Agent_OnAttach(JavaVM* jvm, char* options, void* reserved) {
  auto& context = AgentContext::getInstance();
  return context.on_attach(jvm, options, reserved);
}

void AgentContext::set_callbacks(jvmtiEventCallbacks& callbacks) {
  callbacks.SampledObjectAlloc = SampledObjectAlloc;
  callbacks.ObjectFree = ObjectFree;
  callbacks.VMInit = VMInit;
  callbacks.VMDeath = VMDeath;
}

namespace java {
  VMContext::local_ref<jstring> string(JNIEnv* env, const char* str) {
    return VMContext::getInstance().mk_local_ref(env, (env)->NewStringUTF(str));
  }
  VMContext::local_ref<jstring> string(JNIEnv* env, const std::string& str) {
    return VMContext::getInstance()
        .mk_local_ref(env, (env)->NewStringUTF(str.c_str()));
  }
}

/* Call sent by java Agent class */
void JNICALL Agent_command(JNIEnv* env, jclass klass, jlong cmd) {
  if (cmd != 1) {
    DEBUG_PRINTLN("Agent command: " << cmd);
  }
  auto& context = AgentContext::getInstance();
  context.agent_command(env, klass, cmd);
}

struct PrefixEntry {
    const char* prefix;
    size_t length;
};

constexpr std::array<PrefixEntry, 6> SYSTEM_PREFIXES{{
    {"Ljava/", 6},
    {"Lcom/sun/", 9},
    {"Ljdk/", 5},
    {"Ljavax/", 7},
    {"Lsun/management", 15},
    {"Lclojure/", 9}
}};

bool is_system_class(const char* class_name) {
    return std::any_of(SYSTEM_PREFIXES.begin(), SYSTEM_PREFIXES.end(),
        [class_name](const PrefixEntry& entry) {
            return strncmp(class_name, entry.prefix, entry.length) == 0;
        });
}

// State management class
class AgentState {
private:
  // NOLINTNEXTLINE(cppcoreguidelines-avoid-const-or-ref-data-members)
  VMContext &vm_context;
  // NOLINTNEXTLINE(cppcoreguidelines-avoid-const-or-ref-data-members)
  AgentContext& agent_context;

  jlong agent_state = passive;
  std::vector<std::unique_ptr<alloc_rec>> allocs;
  std::map<jlong, alloc_rec*> allocs_by_tag;

  std::unique_ptr<VMContext::global_ref<jclass>> agent_class;
  std::unique_ptr<VMContext::global_ref<jclass>> agent_allocation_start_marker_class;
  std::unique_ptr<VMContext::global_ref<jclass>> agent_allocation_finish_marker_class;
  std::unique_ptr<VMContext::global_ref<jclass>> agent_allocation_class;
  jmethodID agent_allocation_ctor{};
  jmethodID agent_data1_method{};
  jmethodID agent_data8_method{};
  jfieldID agent_state_field{};

  void set_state(jlong state) {
    agent_state = state;
    DEBUG_PRINTLN("In state " << state);
  }

  void set_state(JNIEnv* env, jlong state) {
    env->SetStaticLongField(*agent_class, agent_state_field, state);
    set_state(state);
  }

  void enable_allocation_tracing(JNIEnv* env) {
    // DEBUG_PRINTF("Enable allocation tracing\n");
    set_state(env, allocation_tracing_starting);

    {
      allocs.clear();
      allocs_by_tag.clear();
    }

    // here just for good measure, should already be set
    agent_context.set_sampling_interval(0);
    agent_context.enable_sampled_object_alloc();
    agent_context.enable_object_free();
  }

  void disable_allocation_tracing(JNIEnv* env) {
    set_state(env, allocation_tracing_stopping);
  }

  void untag_objects(allocs_t &allocs, allocs_by_tag_t &allocs_by_tag);
  auto calling_frame(JNIEnv *env, jvmtiFrameInfo *frames, jint num_frames);
  auto frame_detail(JNIEnv *env, jvmtiFrameInfo &frame);
  std::unique_ptr<alloc_rec> allocation_record(JNIEnv* env,
					       const char* class_sig,
					       jlong size,
					       jthread thread,
					       jlong tag);
  std::unique_ptr<alloc_rec> allocation_record(JNIEnv *env,
                                               const char *class_sig,
                                               jlong size,
					       jthread thread, jint num_frames,
					       jvmtiFrameInfo *frames, jlong tag);

  void allocation_tracing_report(JNIEnv *env, allocs_t &allocs,
				 allocs_by_tag_t &allocs_by_tag);

public:
  AgentState(VMContext& vm_context, AgentContext& agent_context)
      : vm_context(vm_context),	agent_context(agent_context) {}


  void init(JNIEnv* env) {
    auto klass = vm_context.mk_local_ref(env,
                                         env->FindClass("criterium/agent/Agent"));
    if (klass == nullptr) {
      std::cout << "Failed to find Agent class\n";
      return;
    }

    auto allocation_start_marker_klass =
      vm_context.mk_local_ref(env, env->FindClass(allocation_start_marker));
    if (allocation_start_marker_klass == nullptr) {
      std::cout << "Failed to find Agent$AllocationStartMarker class\n";
      return;
    }

    auto allocation_finish_marker_klass =
      vm_context.mk_local_ref(env, env->FindClass(allocation_finish_marker));
    if (allocation_finish_marker_klass == nullptr) {
      std::cout << "Failed to find Agent$AllocationFinishMarker class\n";
      return;
    }

    auto allocation_klass =
      vm_context.mk_local_ref(env, env->FindClass(allocation_class_name));
    if (allocation_klass == nullptr) {
      std::cout << "Failed to find Allocation class\n";
      return;
    }

    static std::array<JNINativeMethod, 1> registry = {{
      {
        // NOLINTNEXTLINE(cppcoreguidelines-pro-type-const-cast)
        const_cast<char *>("command"),
        // NOLINTNEXTLINE(cppcoreguidelines-pro-type-const-cast)
        const_cast<char *>("(J)V"),
        // NOLINTNEXTLINE(cppcoreguidelines-pro-type-reinterpret-cast)
        reinterpret_cast<void*>(Agent_command)
      }
    }};

    auto err = env->RegisterNatives(klass, registry.data(), 1);
    if (err != JVMTI_ERROR_NONE ) {
      std::cout << "Registration of native methods on Agent failed "
          << err << '\n';
      return;
    }

    auto *data1_method =
      env->GetStaticMethodID(klass, "data1", "(Ljava/lang/Object;)V");
    if (data1_method == nullptr ) {
      std::cout << "failed to find Agent.data1 method\n";
      return;
    }
    auto *data8_method =
      env->GetStaticMethodID(klass, "data8", data8_sig);
    if (data8_method == nullptr ) {
      std::cout << "failed to find Agent.data8 method\n";
      return;
    }

    auto *state_field = env->GetStaticFieldID(klass, "state", "J");
    if (state_field == nullptr ) {
      std::cout << "failed to find Agent.state field\n";
      return;
    }


    agent_class = std::make_unique<VMContext::global_ref<jclass>>(env, klass);
    agent_allocation_start_marker_class =
      std::make_unique<VMContext::global_ref<jclass>>(env, allocation_start_marker_klass);
    agent_allocation_finish_marker_class =
      std::make_unique<VMContext::global_ref<jclass>>(env, allocation_finish_marker_klass);
    agent_allocation_class =
      std::make_unique<VMContext::global_ref<jclass>>(env, allocation_klass);

    agent_allocation_ctor = env->GetMethodID(*agent_allocation_class,
					     "<init>",
					     agent_allocation_class_args);
    if (agent_allocation_ctor == nullptr) {
      std::cout << "Failed to get Allocation constructor\n";
    }

    agent_data1_method = data1_method;
    agent_data8_method = data8_method;
    agent_state_field = state_field;
  }

  void process_allocation_event(JNIEnv* env, const AllocationEvent& event) {
    auto class_sig = AgentContext::allocated<char*>();
    if (!agent_context.get_class_signature(event.object_klass, &class_sig)) {
        return;
    }

    auto starting =
      agent_state == allocation_tracing_starting
      && 0 == std::strcmp(class_sig, allocation_start_marker);

    auto stopping =
      agent_state == allocation_tracing_stopping
      && 0 == std::strcmp(class_sig, allocation_finish_marker);

    auto internal = !(starting || stopping);

    std::array<jvmtiFrameInfo, MAX_FRAMES> frames = {};
    jint count=0;

    if (!internal) {
      if (!agent_context.get_stack_trace(event.thread, frames.data(), &count)) {
      	return;
      }
    }

    // NOLINTNEXTLINE(clang-analyzer-cplusplus.NewDeleteLeaks)
    auto rec = internal ? allocation_record(env, class_sig, event.size,
                                            event.thread, event.tag)
                        : allocation_record(env, class_sig, event.size,
                                            event.thread, count,
                                            frames.data(), event.tag);

    if (starting) {
      // DEBUG_PRINT("Start marker seen\n");
      rec->start_marker = true;
    }

    if (stopping) {
	DEBUG_PRINT("Disabling JVMTI_EVENT_SAMPLED_OBJECT_ALLOC\n");
	agent_context.disable_sampled_object_alloc();
	rec->disable_marker = true;
	set_state(env, allocation_tracing_flushing);
      }

    allocs_by_tag.emplace(rec->tag, rec.get());
    allocs.push_back(std::move(rec));
  }  // NOLINT(clang-analyzer-cplusplus.NewDeleteLeaks)

  void process_object_free_event(JNIEnv* env, const ObjectFreeEvent& event) {
    // DEBUG_PRINT("Free\n");
    try {
      alloc_rec* rec = allocs_by_tag.at(event.tag);
      rec->freed = 1;
      // DEBUG_PRINT("Free %d %d\n", rec->start_marker, rec->disable_marker);

      if (rec->start_marker && agent_state == start_allocation_tracing) {
	// set the state to allow the sampler to know that we have
	// actually activated
	DEBUG_PRINT("Start marker seen in Free\n");
	set_state(env, allocation_tracing_active);
      }
      if (agent_state == allocation_tracing_flushing && rec->disable_marker) {
	DEBUG_PRINT("Disabling JVMTI_EVENT_OBJECT_FREE\n");
	agent_context.disable_object_free();
	DEBUG_PRINT("Disabled\n");
	set_state(env, allocation_tracing_flushed);
      }
    } catch(const std::out_of_range&) {
      DEBUG_PRINT("Tag not found in map\n");
    }
  }

  void process_command(JNIEnv* env, const Command& cmd) {
    switch (cmd.cmd) {
    case start_allocation_tracing:
      enable_allocation_tracing(env);
      break;
    case stop_allocation_tracing:
      disable_allocation_tracing(env);
      break;
    case report_allocation_tracing:
      set_state(env, allocation_tracing_reporting);
      allocation_tracing_report(env, allocs, allocs_by_tag);
      set_state(env, allocation_tracing_reported);
      break;
    case ping:
      AgentContext::call_static_void_method(env,
                                            *agent_class,
					    agent_data1_method,
					    env->NewStringUTF("Alive"));
      break;
    case sync_state:
      set_state(env, agent_state);
      break;
    default:
      std::cout << "Invalid command: " << cmd.cmd << '\n';
    }
  }
};

auto AgentState::calling_frame(JNIEnv* env,
			       jvmtiFrameInfo* frames,
			       jint num_frames) {
  jint framei = 0;
  auto class_name = AgentContext::allocated<char*>();

  for (; framei < num_frames; framei++) {
    auto declaring_class = VMContext::local_ref<jclass>(env);
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    if (agent_context.get_method_declaring_class(frames[framei].method,
							&declaring_class)) {
      if (agent_context.get_class_signature(declaring_class, &class_name)) {
        // printf("class : %d %s\n", framei, (char*)class_name);
        // TODO make the filters configurable
        if (is_system_class(class_name)) {
          break;
        }
      }
    }
  }
  if (framei>=num_frames) {
    framei = 0;
  }
  return std::make_tuple(framei, std::move(class_name));
}

auto AgentState::frame_detail(JNIEnv* env, jvmtiFrameInfo& frame) {
  auto declaring_class = VMContext::local_ref<jclass>(env);
  agent_context.get_method_declaring_class(frame.method, &declaring_class);

  auto class_name = AgentContext::allocated<char*>();
  agent_context.get_class_signature(declaring_class, &class_name);

  auto method_name = AgentContext::allocated<char*>();
  agent_context.get_method_name(frame.method, &method_name);

  jint entry_count = 0;
  auto line_table = AgentContext::allocated<jvmtiLineNumberEntry*>();

  jint line_num = -1;

  if (agent_context.get_line_number_table(frame.method, &entry_count,
                                          &line_table)) {
    // NOLINTBEGIN(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    line_num = line_table[0].line_number;
    for ( auto i = 1 ; i < entry_count ; i++ ) {
      if ( frame.location < line_table[i].start_location) {
        break;
      }
      line_num = line_table[i].line_number;
    }
    // NOLINTEND(cppcoreguidelines-pro-bounds-pointer-arithmetic)
  }

  auto source_name = AgentContext::allocated<char *>();
  agent_context.get_source_file_name(declaring_class, &source_name);

  return std::make_tuple(std::move(class_name),
                         std::move(method_name),
                         std::move(source_name),
                         line_num);
}

std::unique_ptr<alloc_rec> AgentState::allocation_record(JNIEnv* env,
							 const char* class_sig,
							 jlong size,
							 jthread thread,
							 jint num_frames,
							 jvmtiFrameInfo* frames,
							 jlong tag) {
  jint framei=0;

  auto [f0_class_name, f0_method, f0_source, f0_line]
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
      = frame_detail(env, frames[0]);

  auto cframe = calling_frame(env, frames, num_frames);
  framei = std::get<0>(cframe);

  auto [fi_class_name, fi_method, fi_source, fi_line]
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
   = frame_detail(env, frames[framei]);

  jlong tid = AgentContext::getInstance().thread_id(env, thread);

  return std::make_unique<alloc_rec>(class_sig,
                                     size,
                                     fi_class_name,
                                     fi_method,
                                     fi_source,
                                     static_cast<jlong>(fi_line),
                                     f0_class_name,
                                     f0_method,
                                     f0_source,
                                     static_cast<jlong>(f0_line),
                                     tid,
                                     tag);
}

std::unique_ptr<alloc_rec> AgentState::allocation_record(JNIEnv* env,
							 const char* class_sig,
							 jlong size,
							 jthread thread,
							 jlong tag) {
  jlong tid = agent_context.thread_id(env, thread);
  return std::make_unique<alloc_rec>(class_sig,
                                     size,
                                     nullptr,
                                     nullptr,
                                     nullptr,
                                     -1,
                                     nullptr,
                                     nullptr,
                                     nullptr,
                                     -1,
                                     tid,
                                     tag);
}

void AgentState::untag_objects(allocs_t &allocs,
                               allocs_by_tag_t& allocs_by_tag) {
  auto tags = all_tags(allocs);
  if (!tags.empty()) {
    jint count = 0;
    auto objects = AgentContext::allocated<jobject*>();
    auto object_tags = AgentContext::allocated<jlong*>();
    agent_context.get_objects_with_tags(static_cast<jint>(tags.size()),
					tags.data(),
					&count,
					&objects,
					&object_tags);
    for (jint i=0; i< count; ++i) {
      // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
      agent_context.set_tag(objects[i], 0);
    }
  }
  // DEBUG_PRINT("remove tags done\n");

  allocs_by_tag.clear();
}

void AgentState::allocation_tracing_report(JNIEnv *env, allocs_t &allocs,
                                           allocs_by_tag_t& allocs_by_tag) {
  for (auto& alloc : allocs) {
    auto class_jstr = java::string(env, alloc->obj_class);

    auto alloc_class_jstr = java::string(env, alloc->alloc_class);
    auto alloc_method_jstr = java::string(env, alloc->alloc_method);
    auto alloc_file_jstr = java::string(env, alloc->alloc_file);

    auto call_class_jstr = java::string(env, alloc->call_class);
    auto call_method_jstr = java::string(env, alloc->call_method);
    auto call_file_jstr = java::string(env, alloc->call_file);

    auto rec = VMContext::local_ref<jobject>
      (env, AgentContext::new_object
       (env,
	*agent_allocation_class,
	agent_allocation_ctor,
	(jstring)class_jstr,
	alloc->obj_size,
	(jstring)call_class_jstr,
	(jstring)call_method_jstr,
	(jstring)call_file_jstr,
	alloc->call_line,
	(jstring)alloc_class_jstr,
	(jstring)alloc_method_jstr,
	(jstring)alloc_file_jstr,
	alloc->alloc_line,
	alloc->thread_id,
	alloc->freed));

    AgentContext::call_static_void_method(env, *agent_class, agent_data1_method,
                                          (jobject&)rec);
  }

  untag_objects(allocs, allocs_by_tag);
  allocs.clear();
}

// Queue consumer thread
void queue_consumer_thread() {
  JNIEnv *env = nullptr;
  VMContext& vm_context = VMContext::getInstance();
  AgentContext& agent_context = AgentContext::getInstance();
  // Attach thread to JVM
  vm_context.attach_current_thread_as_daemon(&env);

  AgentState state(vm_context, agent_context);
  state.init(env);
  Message msg;

  while (!vm_context.vm_dead() && agent_context.get_message_queue().pop(msg)) {
    std::visit([&](auto&& arg) {
      using T = std::decay_t<decltype(arg)>;
      if constexpr (std::is_same_v<T, AllocationEvent>) {
	// DEBUG_PRINT("Process AllocationEvent\n");
	state.process_allocation_event(env, arg);
	arg.delete_global_refs(env);
      }
      else if constexpr (std::is_same_v<T, ObjectFreeEvent>) {
	// DEBUG_PRINT("Process ObjectFreeEvent\n");
	state.process_object_free_event(env, arg);
      }
      else if constexpr (std::is_same_v<T, Command>) {
	DEBUG_PRINT("Process command\n");
	state.process_command(env, arg);
      }
    }, msg);
  }
  // Detach thread from JVM
  if (vm_context.vm() != nullptr) {
    vm_context.vm()->DetachCurrentThread();
  }
}

void start_queue_consumer() {
  std::thread consumer(queue_consumer_thread);
  consumer.detach();
}
