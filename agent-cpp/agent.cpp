#include <algorithm>
#include <cstdlib>
#include <cstring>
#include <functional>
#include <iostream>
#include <jvmti.h>
#include <map>
#include <pthread.h>
#include <sstream>
#include <string>
#include <thread>
#include <vector>

#ifdef DEBUG
#define DEBUG_PRINT(...) printf(__VA_ARGS__)
#else
#define DEBUG_PRINT(...)
#endif

// global ref to the JVMTI environment
static jvmtiEnv* jvmti = nullptr;
static JavaVM* cached_vm = nullptr;
static bool vm_dead = false;

template <typename T>
class allocated  {
  T _ptr;
public:
  allocated() noexcept : _ptr(0) {}
  allocated(allocated<T>&& l) noexcept
    : _ptr(std::exchange(l._ptr, static_cast<T>(0)))
  {}
  ~allocated() { jvmti->Deallocate((unsigned char*)_ptr);}
  operator T& () { return _ptr; }
  T* operator & () { return &_ptr; }
};

template <typename T>
class local_ref {
  T _ref;
  JNIEnv* _env;
public:
  local_ref(JNIEnv* env, T ref) : _ref(std::move(ref)), _env(env) {}
  local_ref(JNIEnv* env) : _ref(NULL), _env(env) {}
  local_ref(local_ref&& l)
    : _ref(std::exchange(l._ref, NULL)),
      _env(std::exchange(l._env, NULL))
  {}
  ~local_ref() {
    if (_ref != NULL && !vm_dead) {
      _env->DeleteLocalRef(_ref);
      _ref = NULL;
    }
  }
  auto operator = (T ref) { _ref = ref; return *this; }
  T* operator & () { return &_ref; }
  operator T& () { return _ref; }
};

template <typename T>
class global_ref {
  T _ref;
  JNIEnv* _env;
public:
  global_ref(JNIEnv* env, T ref)
    : _env(env) {
    _ref = static_cast<T>(env->NewGlobalRef(ref));
  }
  ~global_ref() {
    if (_ref != NULL && !vm_dead) {
      _env->DeleteGlobalRef(_ref);
      _ref = NULL;
    }
  }
  T* operator & () { return &_ref; }
  operator T& () { return _ref; }
};

template <typename T> local_ref<T> mk_local_ref(JNIEnv* env, T t) {
  return local_ref<T>(env, t);
}

// Event/Command structures
struct AllocationEvent {
  jobject object;
  jclass object_klass;
  jthread thread;
  jlong size;
  jlong tag;

  void delete_global_refs(JNIEnv* env) {
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
        queue.push(std::move(msg));
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
        msg = std::move(queue.front());
        queue.pop();
        return true;
    }

    void stop() {
        std::lock_guard<std::mutex> lock(mutex);
        stopped = true;
        cond.notify_all();
    }
};

static MessageQueue message_queue;


static jrawMonitorID control_lock;
static bool disable_object_free_event = false;

class raw_monitor {
  jrawMonitorID id;
public:
  raw_monitor(jrawMonitorID id) : id(id) {
    jvmti->RawMonitorEnter(id);
  }
  ~raw_monitor() {
    jvmti->RawMonitorExit(id);
  }
};


static jrawMonitorID sample_lock;
static jrawMonitorID tag_lock;

static jmethodID thread_getId_method = NULL;

static std::unique_ptr<global_ref<jclass>> agent_class;
static std::unique_ptr<global_ref<jclass>> agent_allocation_start_marker_class;
static std::unique_ptr<global_ref<jclass>> agent_allocation_finish_marker_class;
static std::unique_ptr<global_ref<jclass>> agent_allocation_class;

static jmethodID agent_allocation_ctor = NULL;
static char const* agent_allocation_class_args =
  "(Ljava/lang/String;JLjava/lang/String;Ljava/lang/String;Ljava/lang/String;JLjava/lang/String;Ljava/lang/String;Ljava/lang/String;JJJ)V";

static jmethodID agent_data1_method = NULL;
static jmethodID agent_data8_method = NULL;
static jfieldID agent_state_field = NULL;

enum States : jlong {
  passive,
  allocation_tracing_starting = 10,
  allocation_tracing_active = 11,
  allocation_tracing_stopping = 15,
  allocation_tracing_flushing = 16,
  allocation_tracing_flushed = 17,
  allocation_tracing_reporting = 18,
  allocation_tracing_reported = 19,
};

static char const* allocation_start_marker =
  "Lcriterium/agent/Agent$AllocationStartMarker;";

static char const* allocation_finish_marker =
  "Lcriterium/agent/Agent$AllocationFinishMarker;";

static char const* allocation_class_name =
  "Lcriterium/agent/Allocation;";

static std::string allocation_sampler_name("Lcriterium/agent/core/AllocationSampler");

static jlong next_object_tag = 0; // read and inc with next_tag()

const char *IFn = "clojure/lang/IFn";

jclass ifn(JNIEnv* env) {
  auto ifn = (env)->FindClass(IFn);
  if (ifn == NULL) {
    printf("Ifn not found\n");
  }else {
    // printf("IFn found \n");
  }
  return ifn;
}

const char *invoke_sig = "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;";

jmethodID invoke_method_id(JNIEnv* env) {
  auto klass = ifn(env);
  auto invoke = (env)->GetMethodID(klass, "invoke", invoke_sig);
  if (invoke == NULL) {
      printf("invoke method not found\n");
    } else {
    // DEBUG_PRINT("invoke method found\n");
    }
  return invoke;
}

jmethodID class_invoke_method_id(JNIEnv* env, jclass klass) {
  auto invoke = (env)->GetMethodID(klass, "invoke", invoke_sig);
  if (invoke == NULL) {
      printf("invoke method not found\n");
    } else {
    // DEBUG_PRINT("invoke method found\n");
    }
  return invoke;
}

jlong next_tag() {
  auto monitor = std::make_unique<raw_monitor>(tag_lock);
  return next_object_tag++;
}

static char const *no_file_name = "NO_SOURCE";

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
  jlong freed;

  jlong tag;
  bool start_marker;
  bool disable_marker;

  alloc_rec(char const * obj_class,
            jlong obj_size,
            char const * call_class,
            char const * call_method,
            char const * call_file,
            jlong call_line,
            char const * alloc_class,
            char const * alloc_method,
            char const * alloc_file,
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
      tag(tag),
      freed(false),
      start_marker(false),
      disable_marker(false)
  { }
};

typedef std::vector<std::unique_ptr<alloc_rec>> allocs_t;
typedef std::map<jlong, alloc_rec*> allocs_by_tag_t;
/* static allocs_t allocs; */
/* static auto allocs_by_tag = allocs_by_tag_t(); */

enum Commands : jlong {
  ping,
  sync_state,
  start_allocation_tracing = 10,
  stop_allocation_tracing = 11,
  report_allocation_tracing = 12
};


namespace java {
  local_ref<jstring> string(JNIEnv* env, const char* s) {
    return mk_local_ref(env, (env)->NewStringUTF(s));
  }
  local_ref<jstring> string(JNIEnv* env, const std::string& s) {
    return mk_local_ref(env, (env)->NewStringUTF(s.c_str()));
  }
}

void debug_print_jvmti_err(jvmtiError err) {
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

  case JVMTI_ERROR_INTERNAL:
    DEBUG_PRINT(">> Internal JVM error occurred\n");
    break;

  default:
    DEBUG_PRINT(">> Unexpected error %d\n", err);
    break;
  }

}

auto all_tags(allocs_t& allocs) {
  auto tags=std::vector<jlong>(allocs.size());
  std::transform(allocs.begin(),
                   allocs.end(),
                   std::back_inserter(tags),
                   std::mem_fn(&alloc_rec::tag));
  return tags;
}

void untag_objects(jvmtiEnv* jvmti, allocs_t& allocs, allocs_by_tag_t& allocs_by_tag) {
  // auto monitor = std::make_unique<raw_monitor>(sample_lock);
  auto tags = all_tags(allocs);
  if (tags.size()>0) {
    jint count;
    auto objects = allocated<jobject*>();
    auto object_tags = allocated<jlong*>();
    auto err = jvmti->GetObjectsWithTags(tags.size(),
                                         tags.data(),
                                         &count,
                                         &objects,
                                         &object_tags);
    if (err!= JVMTI_ERROR_NONE) {
      printf("problem %d", err);
    }
    // printf("found %d objects to untag\n", count);
    for (jint i=0; i< count; ++i) {
      jvmti->SetTag(objects[i], 0);
    }

    // TODO unreference the objects
  }
  // printf("remove tags done\n");

  allocs_by_tag.clear();
  // printf("Disabled\n");
}

void allocation_tracing_report(JNIEnv* env, allocs_t& allocs, allocs_by_tag_t& allocs_by_tag) {

  auto monitor = std::make_unique<raw_monitor>(sample_lock);

  for (auto& alloc : allocs) {
    auto class_jstr = java::string(env, alloc->obj_class);

    auto alloc_class_jstr = java::string(env, alloc->alloc_class);
    auto alloc_method_jstr = java::string(env, alloc->alloc_method);
    auto alloc_file_jstr = java::string(env, alloc->alloc_file);

    auto call_class_jstr = java::string(env, alloc->call_class);
    auto call_method_jstr = java::string(env, alloc->call_method);
    auto call_file_jstr = java::string(env, alloc->call_file);

    local_ref<jobject> rec =
      local_ref<jobject>(env,
                         env->NewObject(*agent_allocation_class,
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

    /* printf("calling data 1\n"); */
    env->CallStaticVoidMethod(*agent_class,
                              agent_data1_method,
                              (jobject&)rec);
  }

  untag_objects(jvmti, allocs, allocs_by_tag);
  allocs.clear();
}

#define MAX_FRAMES 1024

auto calling_frame(jvmtiEnv* jvmti,
                   JNIEnv* env,
                   jvmtiFrameInfo* frames,
                   jint num_frames) {
  jint framei = 0;
  auto class_name = allocated<char*>();

  for (; framei < num_frames; framei++) {
    auto declaring_class = local_ref<jclass>(env);
    auto err = jvmti->GetMethodDeclaringClass(frames[framei].method,
                                              &declaring_class);
    if (err!=JVMTI_ERROR_NONE) {
      printf("Error gettimg declaring class: %d\n", err);
    } else {
      err = jvmti->GetClassSignature(declaring_class,
                                     &class_name,
                                     NULL);
      if (err!=JVMTI_ERROR_NONE) {
        printf("Error gettimg declaring class name: %d\n", err);
      } else {
        // printf("class : %d %s\n", framei, (char*)class_name);
        // TODO make the filters configurable
        if (strncmp(class_name, "Ljava/", 6)!=0
            && strncmp(class_name, "Lcom/sun/", 9) != 0
            && strncmp(class_name, "Ljdk/", 5) != 0
            && strncmp(class_name, "Ljavax/", 7)!=0
            && strncmp(class_name, "Lsun/management", 15) != 0
            && strncmp(class_name, "Lclojure/", 9) != 0
            ) {
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


auto frame_detail(jvmtiEnv* jvmti, JNIEnv* env, jvmtiFrameInfo& frame) {
  auto declaring_class = local_ref<jclass>(env);
  auto err = jvmti->GetMethodDeclaringClass(frame.method, &declaring_class);

  auto class_name = allocated<char*>();
  err = jvmti->GetClassSignature(declaring_class,
                                 &class_name,
                                 NULL);

  auto method_name = allocated<char*>();
  err = (jvmti)->GetMethodName(frame.method,
                                    &method_name,
                                    NULL,
                                    NULL);

  jint entry_count;
  auto line_table = allocated<jvmtiLineNumberEntry*>();

  auto error = (jvmti)->GetLineNumberTable(frame.method,
                                           &entry_count,
                                           &line_table);
  jint line_num;

  if (error == JVMTI_ERROR_NONE) {
    line_num = line_table[0].line_number;
    for ( auto i = 1 ; i < entry_count ; i++ ) {
      if ( frame.location < line_table[i].start_location) {
        break;
      }
      line_num = line_table[i].line_number;
    }
  } else {
    line_num = -1;
  }

  auto source_name = allocated<char*>();
  err = jvmti->GetSourceFileName(declaring_class, &source_name);
  if (err == JVMTI_ERROR_ABSENT_INFORMATION) {
    // do nothing - should have source_name == 0
  } else if (err!=JVMTI_ERROR_NONE) {
    printf("Failed to get source file name: %d\n", err);
  }

  return std::make_tuple(std::move(class_name),
                         std::move(method_name),
                         std::move(source_name),
                         line_num);
}

void set_sampling_interval(long n)
{
  auto err = jvmti->SetHeapSamplingInterval(n);
  if (err != JVMTI_ERROR_NONE) {
    printf("Failed to set the sampling interval: %d\n", err);
    debug_print_jvmti_err(err);
  }
}

auto allocation_record(jvmtiEnv* jvmti,
                       JNIEnv* env,
                       const char* class_sig,
                       jlong size,
                       jthread thread,
                       jint num_frames,
                       jvmtiFrameInfo* frames,
		       jlong tag) {


  jint framei=0;

  auto [f0_class_name, f0_method, f0_source, f0_line ]
    = frame_detail(jvmti, env, frames[0]);


  auto cf = calling_frame(jvmti, env, frames, num_frames);
  framei = std::get<0>(cf);

  auto [fi_class_name, fi_method, fi_source, fi_line ]
   = frame_detail(jvmti, env, frames[framei]);

  jlong tid = env->CallLongMethod(thread, thread_getId_method);

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

auto allocation_record(JNIEnv* env,
                       const char* class_sig,
                       jlong size,
                       jthread thread,
		       jlong tag) {
  jlong tid = env->CallLongMethod(thread, thread_getId_method);
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

bool is_allocs_empty(const allocs_t& allocs) {
  auto monitor = std::make_unique<raw_monitor>(sample_lock);
  return allocs.empty();
}

// State management class
class AgentState {
  jlong agent_state = passive;
  std::vector<std::unique_ptr<alloc_rec>> allocs;
  std::map<jlong, alloc_rec*> allocs_by_tag;

  void set_state(jlong state) {
    agent_state = state;
    DEBUG_PRINT("In state %ld\n", state);
  }

  void set_state(JNIEnv* env, jlong state) {
    env->SetStaticLongField(*agent_class, agent_state_field, state);
    set_state(state);
  }

  void enable_allocation_tracing(JNIEnv* env) {
    // DEBUG_PRINTF("Enable allocation tracing\n");
    set_state(env, allocation_tracing_starting);

    {
      auto monitor = std::make_unique<raw_monitor>(sample_lock);
      allocs.clear();
      allocs_by_tag.clear();
    }

    // here just for good measure, should already be set
    set_sampling_interval(0);

    DEBUG_PRINT("Enabling JVMTI_EVENT_SAMPLED_OBJECT_ALLOC\n");
    auto err = jvmti->SetEventNotificationMode(JVMTI_ENABLE,
					       JVMTI_EVENT_SAMPLED_OBJECT_ALLOC,
					       nullptr);
    if (err != JVMTI_ERROR_NONE) {
      printf("Failed to enable allocation sampling %d\n", err);
      debug_print_jvmti_err(err);
    }
    DEBUG_PRINT("Enabling JVMTI_EVENT_OBJECT_FREE\n");
    err = jvmti->SetEventNotificationMode(JVMTI_ENABLE,
					  JVMTI_EVENT_OBJECT_FREE,
					  nullptr);
    if (err != JVMTI_ERROR_NONE) {
      printf("Failed to enable object free notifications %d\n", err);
      debug_print_jvmti_err(err);
    }
  }

  void disable_allocation_tracing(JNIEnv* env) {
    set_state(env, allocation_tracing_stopping);
  }

public:
  void process_allocation_event(JNIEnv* env, const AllocationEvent& event) {
    auto class_sig = allocated<char*>();
    {
      auto err = jvmti->GetClassSignature(event.object_klass, &class_sig, NULL);
      if ( err != 0) {
	printf("Failed to get class name\n" );
	debug_print_jvmti_err(err);
	return;
      }
    }

    auto starting =
      agent_state == allocation_tracing_starting
      && 0 == std::strcmp(class_sig, allocation_start_marker);

    auto stopping =
      agent_state == allocation_tracing_stopping
      && 0 == std::strcmp(class_sig, allocation_finish_marker);

    auto internal = !(starting || stopping);

    jvmtiFrameInfo frames[MAX_FRAMES];
    jint count=0;

    if (!internal) {
      auto err = jvmti->GetStackTrace(event.thread, 0, MAX_FRAMES, frames, &count);
      if (err != 0) {
	printf("Failed to get stack %s\n", static_cast<const char*>(class_sig));
	return;
      }
    }

    auto rec =
      internal ?
      allocation_record(env, class_sig, event.size, event.thread, event.tag) :
      allocation_record(jvmti, env, class_sig, event.size, event.thread, count, frames, event.tag);

    if (starting) {
      // DEBUG_PRINT("Start marker seen\n");
      rec->start_marker = true;
    }

    if (stopping) {
	DEBUG_PRINT("Disabling JVMTI_EVENT_SAMPLED_OBJECT_ALLOC\n");
	jvmti->SetEventNotificationMode(JVMTI_DISABLE,
					JVMTI_EVENT_SAMPLED_OBJECT_ALLOC,
					nullptr);
	rec->disable_marker = true;
	set_state(env, allocation_tracing_flushing);
      }

    auto monitor = std::make_unique<raw_monitor>(sample_lock);
    allocs_by_tag.emplace(rec->tag, rec.get());
    allocs.push_back(std::move(rec));
  }

  void process_object_free_event(JNIEnv* env, const ObjectFreeEvent& event) {
    // DEBUG_PRINT("Free\n");
    auto monitor = std::make_unique<raw_monitor>(sample_lock);
    try {
      alloc_rec* rec = allocs_by_tag.at(event.tag);
      rec->freed = true;
      // DEBUG_PRINT("Free %d %d\n", rec->start_marker, rec->disable_marker);

      if (rec->start_marker && agent_state == start_allocation_tracing) {
	// set the state to allow the sampler to know that we have
	// actually activated
	DEBUG_PRINT("Start marker seen in Free\n");
	set_state(env, allocation_tracing_active);
      }
      if (agent_state == allocation_tracing_flushing && rec->disable_marker) {
	DEBUG_PRINT("Disabling JVMTI_EVENT_OBJECT_FREE\n");
	auto err = jvmti->SetEventNotificationMode(JVMTI_DISABLE,
						   JVMTI_EVENT_OBJECT_FREE,
						   nullptr);
	if (err != JVMTI_ERROR_NONE) {
	  printf("Disable JVMTI_EVENT_OBJECT_FREE failed %d\n", err);
	  debug_print_jvmti_err(err);
	}
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
      env->CallStaticVoidMethod(*agent_class,
				agent_data1_method,
				env->NewStringUTF("Alive"));
      break;
    case sync_state:
      set_state(env, agent_state);
      break;
    }
  }
};

// Queue consumer thread
void queue_consumer_thread(JavaVM* jvm) {
    JNIEnv* env;
    // Attach thread to JVM
    jint res = jvm->AttachCurrentThreadAsDaemon((void**)&env, NULL);
    if (res != JNI_OK) {
        DEBUG_PRINT("Failed to attach queue consumer thread to JVM\n");
        return;
    }

    AgentState state;
    Message msg;

    while (!vm_dead && message_queue.pop(msg)) {
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
    if (jvm != nullptr) {
        jvm->DetachCurrentThread();
    }
}

// Modified event callbacks to use queue
void JNICALL SampledObjectAlloc(jvmtiEnv* jvmti, JNIEnv* env,
                               jthread thread, jobject object,
                               jclass object_klass, jlong size) {
  auto tag = next_tag();
  auto err = jvmti->SetTag(object, tag);
  if (err != JVMTI_ERROR_NONE) {
    printf("Failed to tag object: %d\n", err);
    debug_print_jvmti_err(err);
  }
  message_queue.push(AllocationEvent{
      static_cast<jobject>(env->NewGlobalRef(object)),
      static_cast<jclass>(env->NewGlobalRef(object_klass)),
      static_cast<jthread>(env->NewGlobalRef(thread)),
      size,
      tag
    });
}

void JNICALL ObjectFree(jvmtiEnv *jvmti, jlong tag) {
  // DEBUG_PRINT("ObjectFree\n");
  message_queue.push(ObjectFreeEvent{tag});
}

/* Call sent by java Agent class */
void JNICALL Agent_command(JNIEnv* env, jclass klass, jlong cmd) {
  if (cmd != 1) {
    DEBUG_PRINT("Agent command: %ld\n", cmd);
  }
  message_queue.push(Command{cmd});
}

static char* terminate_string(char* class_name) {
    class_name[strlen(class_name) - 1] = 0;
    return class_name + 1;
}


void watch_field(jvmtiEnv* jvmti,
                 JNIEnv* env,
                 jthread thread,
                 jclass klass,
                 const char* field_name,
                 const char* field_sig=NULL) {
  jfieldID field_id = (env)->GetFieldID(klass, field_name, field_sig);
  if (field_id != NULL) {
    // printf("found field\n");
  } else {
    printf("field not found\n");
  }

  auto err = (jvmti)->SetFieldModificationWatch(klass, field_id);
}

void JNICALL FieldModification(jvmtiEnv* jvmti,
                               JNIEnv* env,
                               jthread thread,
                               jmethodID method,
                               jlocation location,
                               jclass field_klass,
                               jobject object,
                               jfieldID field,
                               char signature_type,
                               jvalue new_value) {
  // printf("\nField modification\n");
  // bool value = new_value.z;
  // // printf("got modified field value %d\n", value);

  // if (!!value) {
  //   enable_allocation_tracing(env, thread, object);
  // } else {
  //   disable_allocation_tracing(jvmti, env, thread, object);
  // }
}

void JNICALL ClassLoad(jvmtiEnv* jvmti,
                       JNIEnv* env,
                       jthread thread,
                       jclass klass) {
  char *className;
  int err = (jvmti)->GetClassSignature(klass, &className, NULL);
  if (className != NULL) {
    terminate_string(className);
    if (allocation_sampler_name == className) {
      // printf("\nFound allocation sampler\n");
      watch_field(jvmti, env, thread, klass, "enabled", "Z");
    }
  }
}

void JNICALL VMInit(jvmtiEnv* jvmti, JNIEnv* env, jthread thread) {

  // set this as early as possible.  repeated here, as not sure it works from
  // OnLoad.
  set_sampling_interval(0);

  // Get JavaVM pointer
  env->GetJavaVM(&cached_vm);

  auto monitor = std::make_unique<raw_monitor>(sample_lock);

  jclass thread_klass = env->GetObjectClass(thread);
  thread_getId_method = env->GetMethodID(thread_klass, "getId", "()J");

  auto klass = mk_local_ref(env, env->FindClass("criterium/agent/Agent"));
  if (klass == NULL) {
    printf("Failed to find Agent class\n");
    return;
  }

  auto allocation_start_marker_klass =
    mk_local_ref(env, env->FindClass(allocation_start_marker));
  if (allocation_start_marker_klass == NULL) {
    printf("Failed to find Agent$AllocationStartMarker class\n");
    return;
  }

  auto allocation_finish_marker_klass =
    mk_local_ref(env, env->FindClass(allocation_finish_marker));
  if (allocation_finish_marker_klass == NULL) {
    printf("Failed to find Agent$AllocationFinishMarker class\n");
    return;
  }

  auto allocation_klass =
    mk_local_ref(env, env->FindClass(allocation_class_name));
  if (allocation_klass == NULL) {
    printf("Failed to find Allocation class\n");
    return;
  }

  static JNINativeMethod registry[1] = {
    {const_cast<char*>("command"),
     const_cast<char*>("(J)V"),
     (void*)Agent_command}
  };

  auto err = env->RegisterNatives(klass, registry, 1);
  if (err != JVMTI_ERROR_NONE ) {
    printf("Registration of native methods on Agent failed %d\n", err);
    return;
  }

  auto data1_method =
    env->GetStaticMethodID(klass, "data1", "(Ljava/lang/Object;)V");
  if (data1_method == NULL ) {
    printf("failed to find Agent.data1 method\n");
    return;
  }
  auto data8_method =
    env->GetStaticMethodID(klass, "data8", "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)V");
  if (data8_method == NULL ) {
    printf("failed to find Agent.data8 method\n");
    return;
  }

  auto state_field = env->GetStaticFieldID(klass, "state", "J");
  if (state_field == NULL ) {
    printf("failed to find Agent.state field\n");
    return;
  }


  agent_class = std::make_unique<global_ref<jclass>>(env, klass);
  agent_allocation_start_marker_class =
    std::make_unique<global_ref<jclass>>(env, allocation_start_marker_klass);
  agent_allocation_finish_marker_class =
    std::make_unique<global_ref<jclass>>(env, allocation_finish_marker_klass);
  agent_allocation_class =
    std::make_unique<global_ref<jclass>>(env, allocation_klass);

  agent_allocation_ctor = env->GetMethodID(*agent_allocation_class,
                                           "<init>",
                                           agent_allocation_class_args);
  if (agent_allocation_ctor == NULL) {
    printf("Failed to get Allocation constructor\n");
  }

  agent_data1_method = data1_method;
  agent_data8_method = data8_method;
  agent_state_field = state_field;

  // Start queue consumer thread
  std::thread consumer(queue_consumer_thread, cached_vm);
    consumer.detach();
    message_queue.push(Command{sync_state});
}

void JNICALL VMDeath(jvmtiEnv* jvmti, JNIEnv* env) {
  vm_dead = true;
  message_queue.stop();
}

void parse_options(jvmtiEnv* jvmti, char* options) {
  /* if (options != NULL && options[0] >= '0' && options[0] <= '9') { */
  /*   jvmti->SetHeapSamplingInterval(std::atoi(options)); */
  /* } */
}

JNIEXPORT jint JNICALL
Agent_OnLoad(JavaVM* vm, char* options, void* reserved) {
  printf("Loading criterium agent\n");

  vm->GetEnv((void**) &jvmti, JVMTI_VERSION_1_0);

  jvmti->CreateRawMonitor("sample_lock", &sample_lock);
  jvmti->CreateRawMonitor("tag_lock", &tag_lock);
  jvmti->CreateRawMonitor("control_lock", &control_lock);

  jvmtiCapabilities capabilities = {0};
  capabilities.can_generate_sampled_object_alloc_events = 1;
  capabilities.can_generate_field_modification_events = 1;
  capabilities.can_get_line_numbers = 1;
  capabilities.can_get_source_file_name = 1;
  capabilities.can_tag_objects = 1;
  capabilities.can_generate_object_free_events = 1;

  {
    auto err = jvmti->AddCapabilities(&capabilities);
    if (err != JVMTI_ERROR_NONE) {
      printf("Failed to add capabilities: %d\n", err);
      debug_print_jvmti_err(err);
    }
  }

  jvmtiEventCallbacks callbacks = {0};
  callbacks.SampledObjectAlloc = SampledObjectAlloc;
  callbacks.ObjectFree = ObjectFree;
  callbacks.FieldModification = FieldModification;
  callbacks.VMInit = VMInit;
  callbacks.VMDeath = VMDeath;
  jvmti->SetEventCallbacks(&callbacks, sizeof(callbacks));
  jvmti->SetEventNotificationMode(JVMTI_ENABLE,
                                  JVMTI_EVENT_FIELD_MODIFICATION,
                                  nullptr);

  jvmti->SetEventNotificationMode(JVMTI_ENABLE,
                                  JVMTI_EVENT_VM_INIT,
                                  nullptr);
  jvmti->SetEventNotificationMode(JVMTI_ENABLE,
				  JVMTI_EVENT_VM_DEATH,
                                  nullptr);

  // Set this as early as possible
  set_sampling_interval(0);

  DEBUG_PRINT("\nallocation sampler loaded\n");

  return JNI_OK;
}

JNIEXPORT jint JNICALL
Agent_OnAttach(JavaVM* vm, char* options, void* reserved) {
  if (jvmti != NULL) {
    return 0;
  }
  return Agent_OnLoad(vm, options, reserved);
}

JNIEXPORT void JNICALL Java_criterium_agent_sayHello(JNIEnv *) {
  printf("hello\n");
}
