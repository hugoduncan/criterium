#include "jni.h"
#include "include/agent_state.h"
#include "include/agent_types.h"
#include "include/alloc_rec.h"
#include "include/call_tree.h"
#include "include/jni_operations.h"
#include "include/jvmti_operations.h"
#include "include/message_queue.h"
#include "include/state_transitions.h"
#include "include/utils.h"
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

using criterium::MAX_FRAMES;

// State enum values
using criterium::passive;
using criterium::allocation_tracing_starting;
using criterium::allocation_tracing_active;
using criterium::allocation_tracing_stopping;
using criterium::allocation_tracing_flushing;
using criterium::allocation_tracing_flushed;
using criterium::allocation_tracing_reporting;
using criterium::allocation_tracing_reported;
using criterium::method_tracing_starting;
using criterium::method_tracing_active;
using criterium::method_tracing_stopping;
using criterium::method_tracing_stopped;
using criterium::method_tracing_reporting;
using criterium::method_tracing_reported;

// Command enum values
using criterium::ping;
using criterium::sync_state;
using criterium::start_allocation_tracing;
using criterium::stop_allocation_tracing;
using criterium::report_allocation_tracing;
using criterium::start_method_tracing;
using criterium::stop_method_tracing;
using criterium::report_method_tracing;

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

using criterium::AllocRec;
using criterium::CallTreeNode;
using criterium::ThreadCallState;

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

// States and Commands enums are defined in include/agent_types.h
// Event/Command structures are defined in include/agent_state.h

using criterium::AllocationEvent;
using criterium::ObjectFreeEvent;
using criterium::MethodEntryEvent;
using criterium::MethodExitEvent;
using criterium::Command;

// Queue message type
using Message = std::variant<AllocationEvent, ObjectFreeEvent,
                             MethodEntryEvent, MethodExitEvent, Command>;
using MessageQueue = criterium::MessageQueue<Message>;

using allocs_t = std::vector<std::unique_ptr<AllocRec>>;
using allocs_by_tag_t = std::map<jlong, AllocRec*>;

using criterium::all_tags;


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
  std::unique_ptr<criterium::IJvmtiOperations> jvmti_ops_;
  std::unique_ptr<criterium::IJniOperations> jni_ops_;

  static jvmtiEnv* get_jvmti() { return getInstance().jvmti; }
  static criterium::IJvmtiOperations& get_jvmti_ops() {
    return *getInstance().jvmti_ops_;
  }

public:
  criterium::IJvmtiOperations& jvmti_ops() { return *jvmti_ops_; }
  criterium::IJniOperations& jni_ops() { return *jni_ops_; }

  /// Set JVMTI operations for testing. Takes ownership of the pointer.
  static void set_jvmti_ops_for_testing(
      std::unique_ptr<criterium::IJvmtiOperations> ops) {
    getInstance().jvmti_ops_ = std::move(ops);
  }

  /// Set JNI operations for testing. Takes ownership of the pointer.
  static void set_jni_ops_for_testing(
      std::unique_ptr<criterium::IJniOperations> ops) {
    getInstance().jni_ops_ = std::move(ops);
  }

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
    ~allocated() {
      // NOLINTNEXTLINE(cppcoreguidelines-pro-type-reinterpret-cast)
      get_jvmti_ops().deallocate(reinterpret_cast<unsigned char*>(_ptr));
    }
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

    jvmti_ops_ = std::make_unique<criterium::JvmtiOperations>(jvmti);
    jni_ops_ = std::make_unique<criterium::JniOperations>();

    jvmti->CreateRawMonitor("tag_lock", &tag_lock);

    jvmtiCapabilities capabilities = {};
    capabilities.can_generate_sampled_object_alloc_events = 1;
    capabilities.can_generate_field_modification_events = 1;
    capabilities.can_get_line_numbers = 1;
    capabilities.can_get_source_file_name = 1;
    capabilities.can_tag_objects = 1;
    capabilities.can_generate_object_free_events = 1;
    capabilities.can_generate_method_entry_events = 1;
    capabilities.can_generate_method_exit_events = 1;

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

  void sampled_object_alloc([[maybe_unused]] jvmtiEnv* jvmti_env, JNIEnv* env,
                            jthread thread, jobject object,
                            jclass object_klass, jlong size) {
    auto tag = next_tag();
    jvmti_ops_->set_tag(object, tag);

    // Capture stack trace synchronously while still on the allocating thread
    std::array<jvmtiFrameInfo, MAX_FRAMES> frames = {};
    jint frame_count = 0;
    if (!jvmti_ops_->get_stack_trace(thread, 0, MAX_FRAMES,
                                     frames.data(), &frame_count)) {
      frame_count = 0;
    }

    message_queue.push(AllocationEvent{
        jni_ops_->new_global_ref(env, object),
        // NOLINTNEXTLINE(cppcoreguidelines-pro-type-reinterpret-cast)
        reinterpret_cast<jclass>(jni_ops_->new_global_ref(env, object_klass)),
        static_cast<jthread>(jni_ops_->new_global_ref(env, thread)),
        size,
        tag,
        frames,
        frame_count});
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
    return jni_ops_->call_long_method(env, thread, thread_getId_method);
  }

  jmethodID get_thread_getId_method() const { return thread_getId_method; }

  bool get_class_signature(jclass klass, char** class_sig) {
    return jvmti_ops_->get_class_signature(klass, class_sig);
  }

  bool get_source_file_name(jclass klass, char** source_name) {
    return jvmti_ops_->get_source_file_name(klass, source_name);
  }

  bool get_method_declaring_class(jmethodID method, jclass* declaring_class) {
    return jvmti_ops_->get_method_declaring_class(method, declaring_class);
  }

  bool get_method_name(jmethodID method, char** method_name) {
    return jvmti_ops_->get_method_name(method, method_name);
  }

  bool get_stack_trace(jthread thread, jvmtiFrameInfo *frames, jint* count) {
    return jvmti_ops_->get_stack_trace(thread, 0, MAX_FRAMES, frames, count);
  }

  bool get_line_number_table(jmethodID method, jint *entry_count,
                             jvmtiLineNumberEntry** line_table) {
    return jvmti_ops_->get_line_number_table(method, entry_count, line_table);
  }

  bool set_tag(jobject object, jlong tag) {
    return jvmti_ops_->set_tag(object, tag);
  }

  bool get_objects_with_tags(jint ntags, jlong *tag_data, jint *count,
                             jobject** objects, jlong** tags) {
    return jvmti_ops_->get_objects_with_tags(ntags, tag_data, count, objects,
                                             tags);
  }

  void call_static_void_method(JNIEnv *env, jclass klass,
                               jmethodID method, jobject arg) {
    jni_ops_->call_static_void_method(env, klass, method, arg);
  }

  jobject new_object_a(JNIEnv *env, jclass klass, jmethodID ctor,
                       const jvalue* args) {
    return jni_ops_->new_object_a(env, klass, ctor, args);
  }

  void set_sampling_interval(jint n) {
    jvmti_ops_->set_heap_sampling_interval(n);
  }

  void enable_sampled_object_alloc() {
    DEBUG_PRINT("Enabling JVMTI_EVENT_SAMPLED_OBJECT_ALLOC\n");
    jvmti_ops_->set_event_notification_mode(JVMTI_ENABLE,
                                            JVMTI_EVENT_SAMPLED_OBJECT_ALLOC,
                                            nullptr);
  }

  void enable_object_free() {
    DEBUG_PRINT("Enabling JVMTI_EVENT_OBJECT_FREE\n");
    jvmti_ops_->set_event_notification_mode(JVMTI_ENABLE,
                                            JVMTI_EVENT_OBJECT_FREE,
                                            nullptr);
  }

  void disable_sampled_object_alloc() {
    DEBUG_PRINT("Disabling JVMTI_EVENT_SAMPLED_OBJECT_ALLOC\n");
    jvmti_ops_->set_event_notification_mode(JVMTI_DISABLE,
                                            JVMTI_EVENT_SAMPLED_OBJECT_ALLOC,
                                            nullptr);
  }

  void disable_object_free() {
    DEBUG_PRINT("Disabling JVMTI_EVENT_OBJECT_FREE\n");
    jvmti_ops_->set_event_notification_mode(JVMTI_DISABLE,
                                            JVMTI_EVENT_OBJECT_FREE,
                                            nullptr);
  }

  void enable_method_entry() {
    DEBUG_PRINT("Enabling JVMTI_EVENT_METHOD_ENTRY\n");
    jvmti_ops_->set_event_notification_mode(JVMTI_ENABLE,
                                            JVMTI_EVENT_METHOD_ENTRY,
                                            nullptr);
  }

  void enable_method_exit() {
    DEBUG_PRINT("Enabling JVMTI_EVENT_METHOD_EXIT\n");
    jvmti_ops_->set_event_notification_mode(JVMTI_ENABLE,
                                            JVMTI_EVENT_METHOD_EXIT,
                                            nullptr);
  }

  void disable_method_entry() {
    DEBUG_PRINT("Disabling JVMTI_EVENT_METHOD_ENTRY\n");
    jvmti_ops_->set_event_notification_mode(JVMTI_DISABLE,
                                            JVMTI_EVENT_METHOD_ENTRY,
                                            nullptr);
  }

  void disable_method_exit() {
    DEBUG_PRINT("Disabling JVMTI_EVENT_METHOD_EXIT\n");
    jvmti_ops_->set_event_notification_mode(JVMTI_DISABLE,
                                            JVMTI_EVENT_METHOD_EXIT,
                                            nullptr);
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

void JNICALL MethodEntry(jvmtiEnv* jvmti, JNIEnv* env,
                         jthread thread, jmethodID method) {
  (void)jvmti; // Unused parameter
  auto& context = AgentContext::getInstance();
  context.get_message_queue().push(MethodEntryEvent{
      static_cast<jthread>(context.jni_ops().new_global_ref(env, thread)),
      method});
}

void JNICALL MethodExit(jvmtiEnv* jvmti, JNIEnv* env,
                        jthread thread, jmethodID method,
                        jboolean was_popped_by_exception, jvalue return_value) {
  (void)jvmti; // Unused parameter
  (void)was_popped_by_exception; // Unused for now
  (void)return_value; // Unused for now
  auto& context = AgentContext::getInstance();
  context.get_message_queue().push(MethodExitEvent{
      static_cast<jthread>(context.jni_ops().new_global_ref(env, thread)),
      method});
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
  callbacks.MethodEntry = MethodEntry;
  callbacks.MethodExit = MethodExit;
  callbacks.VMInit = VMInit;
  callbacks.VMDeath = VMDeath;
}

namespace java {
  VMContext::local_ref<jstring> string(JNIEnv* env, const char* str) {
    return VMContext::getInstance().mk_local_ref(
        env, AgentContext::getInstance().jni_ops().new_string_utf(env, str));
  }
  VMContext::local_ref<jstring> string(JNIEnv* env, const std::string& str) {
    return VMContext::getInstance().mk_local_ref(
        env,
        AgentContext::getInstance().jni_ops().new_string_utf(env, str.c_str()));
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

using criterium::is_system_class;

// State management class
class AgentState {
private:
  // NOLINTNEXTLINE(cppcoreguidelines-avoid-const-or-ref-data-members)
  VMContext &vm_context;
  // NOLINTNEXTLINE(cppcoreguidelines-avoid-const-or-ref-data-members)
  criterium::IJvmtiOperations& jvmti_ops_;
  // NOLINTNEXTLINE(cppcoreguidelines-avoid-const-or-ref-data-members)
  criterium::IJniOperations& jni_ops_;
  jmethodID thread_getId_method_;

  jlong agent_state = passive;
  std::vector<std::unique_ptr<AllocRec>> allocs;
  std::map<jlong, AllocRec*> allocs_by_tag;

  // Method tracing state
  std::unique_ptr<CallTreeNode> call_tree_root_;
  std::map<jlong, ThreadCallState> thread_call_states_;

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
    if (agent_class) {
      jni_ops_.set_static_long_field(env, *agent_class, agent_state_field,
                                     state);
    }
    set_state(state);
  }

  bool initialized() const {
    return agent_class != nullptr;
  }

  void enable_allocation_tracing(JNIEnv* env) {
    // DEBUG_PRINTF("Enable allocation tracing\n");
    set_state(env, allocation_tracing_starting);

    {
      allocs.clear();
      allocs_by_tag.clear();
    }

    // here just for good measure, should already be set
    jvmti_ops_.set_heap_sampling_interval(0);
    DEBUG_PRINT("Enabling JVMTI_EVENT_SAMPLED_OBJECT_ALLOC\n");
    jvmti_ops_.set_event_notification_mode(JVMTI_ENABLE,
                                           JVMTI_EVENT_SAMPLED_OBJECT_ALLOC,
                                           nullptr);
    DEBUG_PRINT("Enabling JVMTI_EVENT_OBJECT_FREE\n");
    jvmti_ops_.set_event_notification_mode(JVMTI_ENABLE,
                                           JVMTI_EVENT_OBJECT_FREE, nullptr);
  }

  void disable_allocation_tracing(JNIEnv* env) {
    set_state(env, allocation_tracing_stopping);
  }

  void enable_method_tracing(JNIEnv* env) {
    set_state(env, method_tracing_starting);

    // Initialize call tree with a synthetic root node
    call_tree_root_ = std::make_unique<CallTreeNode>();
    call_tree_root_->class_name = "<root>";
    call_tree_root_->method_name = "<root>";
    thread_call_states_.clear();

    // Enable method entry/exit events
    DEBUG_PRINT("Enabling JVMTI_EVENT_METHOD_ENTRY\n");
    jvmti_ops_.set_event_notification_mode(JVMTI_ENABLE,
                                           JVMTI_EVENT_METHOD_ENTRY,
                                           nullptr);
    DEBUG_PRINT("Enabling JVMTI_EVENT_METHOD_EXIT\n");
    jvmti_ops_.set_event_notification_mode(JVMTI_ENABLE,
                                           JVMTI_EVENT_METHOD_EXIT,
                                           nullptr);

    set_state(env, method_tracing_active);
  }

  void disable_method_tracing(JNIEnv* env) {
    set_state(env, method_tracing_stopping);

    // Disable method entry/exit events
    DEBUG_PRINT("Disabling JVMTI_EVENT_METHOD_ENTRY\n");
    jvmti_ops_.set_event_notification_mode(JVMTI_DISABLE,
                                           JVMTI_EVENT_METHOD_ENTRY,
                                           nullptr);
    DEBUG_PRINT("Disabling JVMTI_EVENT_METHOD_EXIT\n");
    jvmti_ops_.set_event_notification_mode(JVMTI_DISABLE,
                                           JVMTI_EVENT_METHOD_EXIT,
                                           nullptr);

    set_state(env, method_tracing_stopped);
  }

  /// Resolve method information from a jmethodID.
  /// Returns tuple of (class_name, method_name, source_file, line_number).
  auto resolve_method_info(jmethodID method) {
    std::string class_name;
    std::string method_name_str;
    std::string source_file;
    jint line_number = -1;

    // Get declaring class
    auto declaring_class = AgentContext::allocated<jclass>();
    if (jvmti_ops_.get_method_declaring_class(method, &declaring_class)) {
      // Get class signature
      auto class_sig = AgentContext::allocated<char*>();
      if (jvmti_ops_.get_class_signature(declaring_class, &class_sig)) {
        class_name = class_sig;
      }

      // Get source file name
      auto source_name = AgentContext::allocated<char*>();
      if (jvmti_ops_.get_source_file_name(declaring_class, &source_name)) {
        source_file = source_name;
      }
    }

    // Get method name
    auto method_name_ptr = AgentContext::allocated<char*>();
    if (jvmti_ops_.get_method_name(method, &method_name_ptr)) {
      method_name_str = method_name_ptr;
    }

    // Get line number from line number table (first entry as approximation)
    jint entry_count = 0;
    auto line_table = AgentContext::allocated<jvmtiLineNumberEntry*>();
    if (jvmti_ops_.get_line_number_table(method, &entry_count, &line_table)) {
      if (entry_count > 0) {
        // Use the first line number as the method's line
        // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
        line_number = line_table[0].line_number;
      }
    }

    return std::make_tuple(std::move(class_name), std::move(method_name_str),
                           std::move(source_file), line_number);
  }

  void method_tracing_report([[maybe_unused]] JNIEnv* env) {
    // Call tree data is available in call_tree_root_.
    // Reporting to Java will be implemented in task #508.
    // For now, just log that report was requested.
    DEBUG_PRINT("Method tracing report - call tree ready\n");
    if (call_tree_root_) {
      DEBUG_PRINTLN("  Total nodes: " << call_tree_root_->node_count());
      DEBUG_PRINTLN("  Max depth: " << call_tree_root_->max_depth());
    }
  }

  void untag_objects(allocs_t &allocs, allocs_by_tag_t &allocs_by_tag);
  auto calling_frame(JNIEnv *env, jvmtiFrameInfo *frames, jint num_frames);
  auto frame_detail(JNIEnv *env, jvmtiFrameInfo &frame);
  std::unique_ptr<AllocRec> allocation_record(JNIEnv* env,
					       const char* class_sig,
					       jlong size,
					       jthread thread,
					       jlong tag);
  std::unique_ptr<AllocRec> allocation_record(JNIEnv *env,
                                               const char *class_sig,
                                               jlong size,
					       jthread thread, jint num_frames,
					       jvmtiFrameInfo *frames, jlong tag);

  void allocation_tracing_report(JNIEnv *env, allocs_t &allocs,
				 allocs_by_tag_t &allocs_by_tag);

public:
  AgentState(VMContext& vm_context, criterium::IJvmtiOperations& jvmti_ops,
             criterium::IJniOperations& jni_ops, jmethodID thread_getId_method)
      : vm_context(vm_context),
        jvmti_ops_(jvmti_ops),
        jni_ops_(jni_ops),
        thread_getId_method_(thread_getId_method) {}


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
    using namespace criterium::state_transitions;

    auto class_sig = AgentContext::allocated<char*>();
    if (!jvmti_ops_.get_class_signature(event.object_klass, &class_sig)) {
      return;
    }

    auto starting = is_start_marker_allocation(agent_state, class_sig);
    auto stopping = is_finish_marker_allocation(agent_state, class_sig);
    auto internal = !(starting || stopping);

    // Use the pre-captured stack frames from the allocation event
    // NOLINTNEXTLINE(clang-analyzer-cplusplus.NewDeleteLeaks)
    auto rec =
        (internal && event.frame_count > 0)
            ? allocation_record(
                  env, class_sig, event.size, event.thread, event.frame_count,
                  // NOLINTNEXTLINE(cppcoreguidelines-pro-type-const-cast)
                  const_cast<jvmtiFrameInfo*>(event.frames.data()), event.tag)
            : allocation_record(env, class_sig, event.size, event.thread,
                                event.tag);

    if (starting) {
      rec->start_marker = true;
    }

    if (stopping) {
      DEBUG_PRINT("Disabling JVMTI_EVENT_SAMPLED_OBJECT_ALLOC\n");
      jvmti_ops_.set_event_notification_mode(
          JVMTI_DISABLE, JVMTI_EVENT_SAMPLED_OBJECT_ALLOC, nullptr);
      rec->disable_marker = true;
      auto new_state = next_state_after_allocation(agent_state, class_sig);
      set_state(env, new_state);
    }

    allocs_by_tag.emplace(rec->tag, rec.get());
    allocs.push_back(std::move(rec));
  }  // NOLINT(clang-analyzer-cplusplus.NewDeleteLeaks)

  void process_object_free_event(JNIEnv* env, const ObjectFreeEvent& event) {
    using namespace criterium::state_transitions;

    try {
      AllocRec* rec = allocs_by_tag.at(event.tag);
      rec->freed = 1;

      auto new_state =
          next_state_after_object_free(agent_state, rec->start_marker,
                                       rec->disable_marker);
      if (new_state != agent_state) {
        if (rec->start_marker) {
          DEBUG_PRINT("Start marker freed, transitioning to active\n");
        }
        if (rec->disable_marker) {
          DEBUG_PRINT("Disabling JVMTI_EVENT_OBJECT_FREE\n");
          jvmti_ops_.set_event_notification_mode(JVMTI_DISABLE,
                                                 JVMTI_EVENT_OBJECT_FREE,
                                                 nullptr);
          DEBUG_PRINT("Disabled\n");
        }
        set_state(env, new_state);
      }
    } catch (const std::out_of_range&) {
      DEBUG_PRINT("Tag not found in map\n");
    }
  }

  void process_method_entry_event(JNIEnv* env,
                                  const MethodEntryEvent& event) {
    using namespace criterium::method_tracing_transitions;

    if (!is_method_tracing_active(agent_state)) {
      return;
    }

    if (!call_tree_root_) {
      return;
    }

    // Get thread ID
    jlong thread_id = jni_ops_.call_long_method(env, event.thread,
                                                thread_getId_method_);

    // Get or create thread state
    auto& thread_state = thread_call_states_[thread_id];

    // Resolve method info
    auto [class_name, method_name, source_file, line_number] =
        resolve_method_info(event.method);

    // Get current node (root if stack empty)
    CallTreeNode* current = thread_state.empty()
        ? call_tree_root_.get()
        : thread_state.current();

    // Find or create child for this method call
    CallTreeNode* child = current->find_or_create_child(
        class_name, method_name, source_file, line_number);
    child->call_count++;

    // Push child onto stack
    thread_state.push(child);
  }

  void process_method_exit_event(JNIEnv* env,
                                 const MethodExitEvent& event) {
    using namespace criterium::method_tracing_transitions;

    if (!is_method_tracing_active(agent_state)) {
      return;
    }

    // Get thread ID
    jlong thread_id = jni_ops_.call_long_method(env, event.thread,
                                                thread_getId_method_);

    // Find thread state and pop from stack
    auto iter = thread_call_states_.find(thread_id);
    if (iter != thread_call_states_.end()) {
      iter->second.pop();
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
    case start_method_tracing:
      enable_method_tracing(env);
      break;
    case stop_method_tracing:
      disable_method_tracing(env);
      break;
    case report_method_tracing:
      set_state(env, method_tracing_reporting);
      method_tracing_report(env);
      set_state(env, method_tracing_reported);
      break;
    case ping:
      if (agent_class) {
        jni_ops_.call_static_void_method(env, *agent_class, agent_data1_method,
                                         jni_ops_.new_string_utf(env, "Alive"));
      }
      break;
    case sync_state:
      set_state(env, agent_state);
      break;
    default:
      std::cout << "Invalid command: " << cmd.cmd << '\n';
    }
  }

  /// Returns the current agent state. Used for testing.
  jlong get_state() const { return agent_state; }
};

auto AgentState::calling_frame(JNIEnv* env, jvmtiFrameInfo* frames,
                               jint num_frames) {
  jint framei = 0;
  auto class_name = AgentContext::allocated<char*>();

  for (; framei < num_frames; framei++) {
    auto declaring_class = VMContext::local_ref<jclass>(env);
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    if (jvmti_ops_.get_method_declaring_class(frames[framei].method,
                                              &declaring_class)) {
      if (jvmti_ops_.get_class_signature(declaring_class, &class_name)) {
        // printf("class : %d %s\n", framei, (char*)class_name);
        // TODO make the filters configurable
        if (is_system_class(class_name)) {
          break;
        }
      }
    }
  }
  if (framei >= num_frames) {
    framei = 0;
  }
  return std::make_tuple(framei, std::move(class_name));
}

auto AgentState::frame_detail(JNIEnv* env, jvmtiFrameInfo& frame) {
  auto declaring_class = VMContext::local_ref<jclass>(env);
  jvmti_ops_.get_method_declaring_class(frame.method, &declaring_class);

  auto class_name = AgentContext::allocated<char*>();
  jvmti_ops_.get_class_signature(declaring_class, &class_name);

  auto method_name = AgentContext::allocated<char*>();
  jvmti_ops_.get_method_name(frame.method, &method_name);

  jint entry_count = 0;
  auto line_table = AgentContext::allocated<jvmtiLineNumberEntry*>();

  jint line_num = -1;

  if (jvmti_ops_.get_line_number_table(frame.method, &entry_count,
                                       &line_table)) {
    // NOLINTBEGIN(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    line_num = line_table[0].line_number;
    for (auto i = 1; i < entry_count; i++) {
      if (frame.location < line_table[i].start_location) {
        break;
      }
      line_num = line_table[i].line_number;
    }
    // NOLINTEND(cppcoreguidelines-pro-bounds-pointer-arithmetic)
  }

  auto source_name = AgentContext::allocated<char*>();
  jvmti_ops_.get_source_file_name(declaring_class, &source_name);

  return std::make_tuple(std::move(class_name), std::move(method_name),
                         std::move(source_name), line_num);
}

std::unique_ptr<AllocRec> AgentState::allocation_record(
    JNIEnv* env, const char* class_sig, jlong size, jthread thread,
    jint num_frames, jvmtiFrameInfo* frames, jlong tag) {
  jint framei = 0;

  auto [f0_class_name, f0_method, f0_source, f0_line] =
      // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
      frame_detail(env, frames[0]);

  auto cframe = calling_frame(env, frames, num_frames);
  framei = std::get<0>(cframe);

  auto [fi_class_name, fi_method, fi_source, fi_line] =
      // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
      frame_detail(env, frames[framei]);

  jlong tid = jni_ops_.call_long_method(env, thread, thread_getId_method_);

  return std::make_unique<AllocRec>(
      class_sig, size, fi_class_name, fi_method, fi_source,
      static_cast<jlong>(fi_line), f0_class_name, f0_method, f0_source,
      static_cast<jlong>(f0_line), tid, tag);
}

std::unique_ptr<AllocRec> AgentState::allocation_record(JNIEnv* env,
                                                         const char* class_sig,
                                                         jlong size,
                                                         jthread thread,
                                                         jlong tag) {
  jlong tid = jni_ops_.call_long_method(env, thread, thread_getId_method_);
  return std::make_unique<AllocRec>(class_sig, size, nullptr, nullptr, nullptr,
                                     -1, nullptr, nullptr, nullptr, -1, tid,
                                     tag);
}

void AgentState::untag_objects(allocs_t& allocs, allocs_by_tag_t& allocs_by_tag) {
  auto tags = all_tags(allocs);
  if (!tags.empty()) {
    jint count = 0;
    auto objects = AgentContext::allocated<jobject*>();
    auto object_tags = AgentContext::allocated<jlong*>();
    jvmti_ops_.get_objects_with_tags(static_cast<jint>(tags.size()), tags.data(),
                                     &count, &objects, &object_tags);
    for (jint i = 0; i < count; ++i) {
      // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
      jvmti_ops_.set_tag(objects[i], 0);
    }
  }
  // DEBUG_PRINT("remove tags done\n");

  allocs_by_tag.clear();
}

void AgentState::allocation_tracing_report(JNIEnv* env, allocs_t& allocs,
                                           allocs_by_tag_t& allocs_by_tag) {
  if (agent_class && agent_allocation_class) {
    for (auto& alloc : allocs) {
      auto class_jstr = java::string(env, alloc->obj_class);

      auto alloc_class_jstr = java::string(env, alloc->alloc_class);
      auto alloc_method_jstr = java::string(env, alloc->alloc_method);
      auto alloc_file_jstr = java::string(env, alloc->alloc_file);

      auto call_class_jstr = java::string(env, alloc->call_class);
      auto call_method_jstr = java::string(env, alloc->call_method);
      auto call_file_jstr = java::string(env, alloc->call_file);

      // Build jvalue array for NewObjectA
      // Signature:
      // (String,long,String,String,String,long,String,String,String,long,long,long)V
      static constexpr size_t ALLOCATION_CTOR_ARG_COUNT = 12;
      // NOLINTBEGIN(cppcoreguidelines-avoid-magic-numbers,readability-magic-numbers)
      std::array<jvalue, ALLOCATION_CTOR_ARG_COUNT> args = {};
      args[0].l = static_cast<jstring>(class_jstr);
      args[1].j = alloc->obj_size;
      args[2].l = static_cast<jstring>(call_class_jstr);
      args[3].l = static_cast<jstring>(call_method_jstr);
      args[4].l = static_cast<jstring>(call_file_jstr);
      args[5].j = alloc->call_line;
      args[6].l = static_cast<jstring>(alloc_class_jstr);
      args[7].l = static_cast<jstring>(alloc_method_jstr);
      args[8].l = static_cast<jstring>(alloc_file_jstr);
      args[9].j = alloc->alloc_line;
      args[10].j = alloc->thread_id;
      args[11].j = alloc->freed;
      // NOLINTEND(cppcoreguidelines-avoid-magic-numbers,readability-magic-numbers)

      auto rec = VMContext::local_ref<jobject>(
          env, jni_ops_.new_object_a(env, *agent_allocation_class,
                                     agent_allocation_ctor, args.data()));

      jni_ops_.call_static_void_method(env, *agent_class, agent_data1_method,
                                       static_cast<jobject&>(rec));
    }
  }

  untag_objects(allocs, allocs_by_tag);
  allocs.clear();
}

// Queue consumer thread
void queue_consumer_thread() {
  JNIEnv* env = nullptr;
  VMContext& vm_context = VMContext::getInstance();
  AgentContext& agent_context = AgentContext::getInstance();
  // Attach thread to JVM
  vm_context.attach_current_thread_as_daemon(&env);

  AgentState state(vm_context, agent_context.jvmti_ops(), agent_context.jni_ops(),
                   agent_context.get_thread_getId_method());
  state.init(env);
  Message msg;

  while (!vm_context.vm_dead() && agent_context.get_message_queue().pop(msg)) {
    std::visit([&](auto&& arg) {
      using T = std::decay_t<decltype(arg)>;
      if constexpr (std::is_same_v<T, AllocationEvent>) {
        // DEBUG_PRINT("Process AllocationEvent\n");
        state.process_allocation_event(env, arg);
        arg.delete_global_refs(env, agent_context.jni_ops());
      }
      else if constexpr (std::is_same_v<T, ObjectFreeEvent>) {
        // DEBUG_PRINT("Process ObjectFreeEvent\n");
        state.process_object_free_event(env, arg);
      }
      else if constexpr (std::is_same_v<T, MethodEntryEvent>) {
        // DEBUG_PRINT("Process MethodEntryEvent\n");
        state.process_method_entry_event(env, arg);
        arg.delete_global_refs(env, agent_context.jni_ops());
      }
      else if constexpr (std::is_same_v<T, MethodExitEvent>) {
        // DEBUG_PRINT("Process MethodExitEvent\n");
        state.process_method_exit_event(env, arg);
        arg.delete_global_refs(env, agent_context.jni_ops());
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
