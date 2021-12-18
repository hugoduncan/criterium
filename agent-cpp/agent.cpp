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


// global ref to the JVMTI environment
static jvmtiEnv* jvmti = NULL;
static bool vm_dead = false;

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
  /* global_ref(local_ref<T>&& l) */
  /*   : _ref(std::exchange(l._ref, NULL)), */
  /*     _env(std::exchange(l._env, NULL)) */
  /* {} */
  // global_ref(JNIEnv* env) : _ref(0), _env(env) {}
  ~global_ref() {
    if (_ref != NULL && !vm_dead) {
      _env->DeleteGlobalRef(_ref);
      _ref = NULL;
    }
  }
  // global_ref<T>&  operator = (T& ref) {
  //   _ref = _env->NewGlobalRef(ref);
  //   return *this;
  // }
  T* operator & () { return &_ref; }
  operator T& () { return _ref; }
};

template <typename T> local_ref<T> mk_local_ref(JNIEnv* env, T t) {
  return local_ref<T>(env, t);
}

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
  allocation_tracing_flushed = 17
};

static jlong agent_state;

void set_state(jlong state) {
  agent_state = state;
  // printf("In state %ld\n", state);
}

void set_state(JNIEnv* env, jlong state) {
  env->SetStaticLongField(*agent_class, agent_state_field, state);
  set_state(state);
}

static char const* allocation_start_marker =
  "Lcriterium/agent/Agent$AllocationStartMarker;";

static char const* allocation_finish_marker =
  "Lcriterium/agent/Agent$AllocationFinishMarker;";

static char const* allocation_class_name =
  "Lcriterium/agent/Allocation;";

// static jobject the_sampler = NULL;
// static jobject the_sample_fn = NULL;
// static jmethodID the_sample_fn_invoke = NULL;
// static jfieldID the_sample_handshake = NULL;
// static jthread the_sample_thread = NULL;
// static bool sampler_enabled = false;

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
    // printf("invoke method found\n");
    }
  return invoke;
}

jmethodID class_invoke_method_id(JNIEnv* env, jclass klass) {
  auto invoke = (env)->GetMethodID(klass, "invoke", invoke_sig);
  if (invoke == NULL) {
      printf("invoke method not found\n");
    } else {
    // printf("invoke method found\n");
    }
  return invoke;
}

jlong next_tag() {
  auto monitor = std::make_unique<raw_monitor>(tag_lock);
  return next_object_tag++;
}

static char const *no_file_name = "NO_SOURCC";

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
      call_class(call_class),
      call_method(call_method),
      call_file(call_file == NULL ? no_file_name : call_file),
      call_line(call_line),
      alloc_class(alloc_class),
      alloc_method(alloc_method),
      alloc_file(alloc_file == NULL ? no_file_name : alloc_file),
      alloc_line(alloc_line),
      thread_id(thread_id),
      tag(tag),
      freed(false),
      disable_marker(false)
  { }
};

typedef std::vector<std::unique_ptr<alloc_rec>> allocs_t;
typedef std::map<jlong, alloc_rec*> allocs_by_tag_t;
static allocs_t allocs;
static auto allocs_by_tag = allocs_by_tag_t();

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

auto allocation_record(jvmtiEnv* jvmti,
                       JNIEnv* env,
                       const char* class_sig,
                       jlong size,
                       jthread thread,
                       jint num_frames,
                       jvmtiFrameInfo* frames) {


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
                                     next_tag());
}

bool is_allocs_empty() {
  auto monitor = std::make_unique<raw_monitor>(sample_lock);
  return allocs.empty();
}

void JNICALL SampledObjectAlloc(jvmtiEnv* jvmti,
                                JNIEnv* env,
                                jthread thread,
                                jobject object,
                                jclass object_klass,
                                jlong size) {

  auto class_sig = allocated<char*>();
  {
    auto err = jvmti->GetClassSignature(object_klass, &class_sig, NULL);
    if ( err != 0) {
      printf("Failed to get class name\n" );
      return;
    }
  }

  if (is_allocs_empty() && agent_state == allocation_tracing_starting) {
    if (0 == std::strcmp(class_sig, allocation_start_marker)) {
      // set the state to allow the sampler to know that we have
      // actually activated
      set_state(env, allocation_tracing_active);
    }
  } else {

    jvmtiFrameInfo frames[MAX_FRAMES];

    jint count;
    {
      auto err = jvmti->GetStackTrace(thread, 0, MAX_FRAMES, frames, &count);
      if (err != 0) {
        printf("Failed to get stack\n");
        return;
      }
    }

    auto rec =
      allocation_record(jvmti, env, class_sig, size, thread, count, frames);

    // TODO use the alloc_rec pointer as the tag?
    jvmti->SetTag(object, rec->tag);

    if (agent_state == allocation_tracing_stopping
        &&
        (0 == std::strcmp(class_sig, allocation_finish_marker))) {

      jvmti->SetEventNotificationMode(JVMTI_DISABLE,
                                      JVMTI_EVENT_SAMPLED_OBJECT_ALLOC,
                                      NULL);
      rec->disable_marker = true;
      set_state(env, allocation_tracing_flushing);
    }

    auto monitor = std::make_unique<raw_monitor>(sample_lock);
    allocs_by_tag.emplace(rec->tag, rec.get());
    allocs.push_back(std::move(rec));
  }
}

// void turn_off_allocation_tracing(jvmtiEnv* jvmti,
//                                  JNIEnv* env,
//                                  jthread thread,
//                                  jobject object) {
//   jvmti->SetEventNotificationMode(JVMTI_DISABLE,
//                                   JVMTI_EVENT_OBJECT_FREE,
//                                   NULL);
//   auto monitor = std::make_unique<raw_monitor>(sample_lock);

//   // Remove tags
//   {
//     // printf("remove tags\n");
//     auto tags=std::vector<jlong>(allocs.size());
//     std::transform(allocs.begin(),
//                    allocs.end(),
//                    std::back_inserter(tags),
//                    std::mem_fn(&alloc_rec::tag));

//     // printf("found %ld tags\n", tags.size());

//     if (tags.size()>0) {
//       // auto count = allocated<jint>();
//       // auto objects = allocated<jobject*>();
//       // auto object_tags = allocated<jlong*>();
//       jint count;
//       jobject* objects;
//       jlong* object_tags;
//       auto err = jvmti->GetObjectsWithTags(tags.size(),
//                                            tags.data(),
//                                            &count,
//                                            &objects,
//                                            &object_tags);
//       if (err!= JVMTI_ERROR_NONE) {
//         printf("problem %d", err);
//       }
//       // printf("found %d objects to untag\n", count);
//       for (jint i=0; i< count; ++i) {
//         jvmti->SetTag(objects[i], NULL);
//       }

//       // TODO unreference the objects
//     }
//     // printf("remove tags done\n");
//   }

//   allocs_by_tag.clear();
//   // printf("Disabled\n");
// }

auto all_tags(allocs_t& allocs) {
  auto tags=std::vector<jlong>(allocs.size());
  std::transform(allocs.begin(),
                   allocs.end(),
                   std::back_inserter(tags),
                   std::mem_fn(&alloc_rec::tag));
  return tags;
}

void untag_objects(jvmtiEnv* jvmti) {
  // auto monitor = std::make_unique<raw_monitor>(sample_lock);
  auto tags = all_tags(allocs);
  if (tags.size()>0) {
    jint count;
    auto objects = allocated<jobject*>();
    auto object_tags = allocated<jlong*>();
    // jint count;
    // jobject* objects;
    // jlong* object_tags;
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


void JNICALL
ObjectFree(jvmtiEnv *jvmti, jlong tag) {
  auto monitor = std::make_unique<raw_monitor>(sample_lock);
  try {
    alloc_rec* rec = allocs_by_tag.at(tag);
    rec->freed = true;
    if (agent_state == allocation_tracing_flushing && rec->disable_marker) {
      jvmti->SetEventNotificationMode(JVMTI_DISABLE,
                                      JVMTI_EVENT_OBJECT_FREE,
                                      NULL);
      set_state(allocation_tracing_flushed);
    }

  } catch(const std::out_of_range&) {
  }
}

namespace java {
  local_ref<jstring> string(JNIEnv* env, const char* s) {
    return mk_local_ref(env, (env)->NewStringUTF(s));
  }
  local_ref<jstring> string(JNIEnv* env, const std::string& s) {
    return mk_local_ref(env, (env)->NewStringUTF(s.c_str()));
  }
}

void allocation_tracing_report(JNIEnv* env) {

  auto monitor = std::make_unique<raw_monitor>(sample_lock);

  for (auto& alloc : allocs) {
    auto class_jstr = java::string(env, alloc->obj_class);

    auto alloc_class_jstr = java::string(env, alloc->alloc_class);
    auto alloc_method_jstr = java::string(env, alloc->alloc_method);
    auto alloc_file_jstr = java::string(env, alloc->alloc_file);

    auto call_class_jstr = java::string(env, alloc->call_class);
    auto call_method_jstr = java::string(env, alloc->call_method);
    auto call_file_jstr = java::string(env, alloc->call_file);

    /* printf("calling constructor %ld  %ld  %ld  %ld  %ld\n", */
    /*        alloc->obj_size, */
    /*        alloc->call_line, */
    /*        alloc->alloc_line, */
    /*        alloc->thread_id, */
    /*        alloc->freed */
    /*        ); */
    /* printf("calling constructor %s  %s  %s\n", */
    /*        alloc->alloc_class.c_str(), */
    /*        alloc->alloc_method.c_str(), */
    /*        alloc->alloc_file.c_str() */
    /*        ); */
    /* printf("calling constructor %s  %s  %s\n", */
    /*        alloc->call_class.c_str(), */
    /*        alloc->call_method.c_str(), */
    /*        alloc->call_file.c_str() */
    /*        ); */
    /* printf("class %ld\n", */
    /*        (long*)(static_cast<jclass>(*agent_allocation_class))); */

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

  untag_objects(jvmti);
  allocs.clear();
}

static char* terminate_string(char* class_name) {
    class_name[strlen(class_name) - 1] = 0;
    return class_name + 1;
}
static std::string allocation_sampler_name("Lcriterium/agent/core/AllocationSampler");


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


void enable_allocation_tracing(JNIEnv* env) {
  // printf("Enable allocation tracing\n");
  set_state(env, allocation_tracing_starting);

  {
    auto monitor = std::make_unique<raw_monitor>(sample_lock);
    allocs.clear();
    allocs_by_tag.clear();
  }

  jvmti->SetHeapSamplingInterval(0);

  auto err = jvmti->SetEventNotificationMode(JVMTI_ENABLE,
                                             JVMTI_EVENT_SAMPLED_OBJECT_ALLOC,
                                             NULL);
  if (err != JVMTI_ERROR_NONE) {
    printf("Failed to enable allocation sampling %d\n", err);
  }
  err = jvmti->SetEventNotificationMode(JVMTI_ENABLE,
                                        JVMTI_EVENT_OBJECT_FREE,
                                        NULL);
  if (err != JVMTI_ERROR_NONE) {
    printf("Failed to enable objrct free notifivations %d\n", err);
  }

  // printf("Enabled\n");
}

void disable_allocation_tracing(JNIEnv* env) {
  set_state(env, allocation_tracing_stopping);
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
enum Commands : jlong {
  ping,
  sync_state,
  start_allocation_tracing = 10,
  stop_allocation_tracing = 11,
  report_allocation_tracing = 12
};


void JNICALL Agent_command(JNIEnv* env, jclass klass, jlong cmd) {
  // Dispatch commands from the Agent class.
  // printf("received command %ld in state %ld\n", cmd, agent_state);


  if (ping == cmd) {
    env->CallStaticVoidMethod(*agent_class,
                        agent_data1_method,
                        env->NewStringUTF("Alive"));
  } else if (sync_state == cmd) {
    set_state(env, agent_state);
  } else if (start_allocation_tracing == cmd) {
    enable_allocation_tracing(env);
  } else if (stop_allocation_tracing == cmd) {
    disable_allocation_tracing(env);
  } else if (report_allocation_tracing == cmd) {
    allocation_tracing_report(env);
  } else {
    printf("Received unknown command: %ld\n", cmd);
  }
}


void JNICALL VMInit(jvmtiEnv* jvmti, JNIEnv* env, jthread thread) {
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

  /* assert((jclass)allocation_klass == NULL); */
  /* assert(allocation_klass == NULL); */

  agent_allocation_ctor = env->GetMethodID(*agent_allocation_class,
                                           "<init>",
                                           agent_allocation_class_args);
  if (agent_allocation_ctor == NULL) {
    printf("Failed to get Allocation constructor\n");
  }

  agent_data1_method = data1_method;
  agent_data8_method = data8_method;
  agent_state_field = state_field;

  // initialise the state field
  set_state(env, passive);
}

void JNICALL VMDeath(jvmtiEnv* jvmti, JNIEnv* env) {
  vm_dead = true;
}

void parse_options(jvmtiEnv* jvmti, char* options) {
  if (options != NULL && options[0] >= '0' && options[0] <= '9') {
    jvmti->SetHeapSamplingInterval(std::atoi(options));
  }
}

JNIEXPORT jint JNICALL
Agent_OnLoad(JavaVM* vm, char* options, void* reserved) {
  printf("Loading criterium agent\n");

  vm->GetEnv((void**) &jvmti, JVMTI_VERSION_1_0);

  jvmti->CreateRawMonitor("sample_lock", &sample_lock);
  jvmti->CreateRawMonitor("tag_lock", &tag_lock);

  jvmtiCapabilities capabilities = {0};
  capabilities.can_generate_sampled_object_alloc_events = 1;
  capabilities.can_generate_field_modification_events = 1;
  capabilities.can_get_line_numbers = 1;
  capabilities.can_get_source_file_name = 1;
  capabilities.can_tag_objects = 1;
  capabilities.can_generate_object_free_events = 1;
  jvmti->AddCapabilities(&capabilities);

  jvmtiEventCallbacks callbacks = {0};
  callbacks.SampledObjectAlloc = SampledObjectAlloc;
  // callbacks.ClassLoad = ClassLoad;
  callbacks.ObjectFree = ObjectFree;
  callbacks.FieldModification = FieldModification;
  callbacks.VMInit = VMInit;
  callbacks.VMDeath = VMDeath;
  jvmti->SetEventCallbacks(&callbacks, sizeof(callbacks));
  // jvmti->SetEventNotificationMode(JVMTI_ENABLE,
  //                                 JVMTI_EVENT_SAMPLED_OBJECT_ALLOC,
  //                                 NULL);
  // jvmti->SetEventNotificationMode(JVMTI_ENABLE,
  //                                 JVMTI_EVENT_CLASS_LOAD,
  //                                 NULL);
  jvmti->SetEventNotificationMode(JVMTI_ENABLE,
                                  JVMTI_EVENT_FIELD_MODIFICATION,
                                  NULL);

  jvmti->SetEventNotificationMode(JVMTI_ENABLE,
                                  JVMTI_EVENT_VM_INIT,
                                  NULL);
  jvmti->SetEventNotificationMode(JVMTI_ENABLE,
                                JVMTI_EVENT_VM_DEATH,
                                  NULL);

  jvmti->SetHeapSamplingInterval(0);

  parse_options(jvmti, options);
  // printf("\nallocation sampler loaded\n");

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
