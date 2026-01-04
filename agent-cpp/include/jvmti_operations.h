#ifndef CRITERIUM_JVMTI_OPERATIONS_H
#define CRITERIUM_JVMTI_OPERATIONS_H

#include <jvmti.h>

namespace criterium {

/// Interface for JVMTI operations to enable unit testing with mocks.
/// Abstracts the jvmtiEnv* calls used during runtime event processing.
class IJvmtiOperations {
public:
  virtual ~IJvmtiOperations() = default;

  // Class and method introspection
  virtual bool get_class_signature(jclass klass, char** class_sig) = 0;
  virtual bool get_source_file_name(jclass klass, char** source_name) = 0;
  virtual bool get_method_declaring_class(jmethodID method,
                                          jclass* declaring_class) = 0;
  virtual bool get_method_name(jmethodID method, char** method_name) = 0;

  // Stack trace
  virtual bool get_stack_trace(jthread thread, jint start_depth,
                               jint max_frame_count, jvmtiFrameInfo* frames,
                               jint* count) = 0;
  virtual bool get_line_number_table(jmethodID method, jint* entry_count,
                                     jvmtiLineNumberEntry** line_table) = 0;

  // Object tagging
  virtual bool set_tag(jobject object, jlong tag) = 0;
  virtual bool get_objects_with_tags(jint tag_count, const jlong* tags,
                                     jint* count, jobject** objects,
                                     jlong** object_tags) = 0;

  // Sampling and event control
  virtual bool set_heap_sampling_interval(jint sampling_interval) = 0;
  virtual bool set_event_notification_mode(jvmtiEventMode mode,
                                           jvmtiEvent event_type,
                                           jthread event_thread) = 0;

  // Memory deallocation for JVMTI-allocated memory
  virtual void deallocate(unsigned char* mem) = 0;
};

/// Production implementation wrapping a real jvmtiEnv*.
class JvmtiOperations : public IJvmtiOperations {
private:
  jvmtiEnv* jvmti_;

public:
  explicit JvmtiOperations(jvmtiEnv* jvmti) : jvmti_(jvmti) {}

  bool get_class_signature(jclass klass, char** class_sig) override {
    auto err = jvmti_->GetClassSignature(klass, class_sig, nullptr);
    return err == JVMTI_ERROR_NONE;
  }

  bool get_source_file_name(jclass klass, char** source_name) override {
    auto err = jvmti_->GetSourceFileName(klass, source_name);
    return err == JVMTI_ERROR_NONE;
  }

  bool get_method_declaring_class(jmethodID method,
                                  jclass* declaring_class) override {
    auto err = jvmti_->GetMethodDeclaringClass(method, declaring_class);
    return err == JVMTI_ERROR_NONE;
  }

  bool get_method_name(jmethodID method, char** method_name) override {
    auto err = jvmti_->GetMethodName(method, method_name, nullptr, nullptr);
    return err == JVMTI_ERROR_NONE;
  }

  bool get_stack_trace(jthread thread, jint start_depth, jint max_frame_count,
                       jvmtiFrameInfo* frames, jint* count) override {
    auto err =
        jvmti_->GetStackTrace(thread, start_depth, max_frame_count, frames, count);
    return err == JVMTI_ERROR_NONE;
  }

  bool get_line_number_table(jmethodID method, jint* entry_count,
                             jvmtiLineNumberEntry** line_table) override {
    auto err = jvmti_->GetLineNumberTable(method, entry_count, line_table);
    return err == JVMTI_ERROR_NONE;
  }

  bool set_tag(jobject object, jlong tag) override {
    auto err = jvmti_->SetTag(object, tag);
    return err == JVMTI_ERROR_NONE;
  }

  bool get_objects_with_tags(jint tag_count, const jlong* tags, jint* count,
                             jobject** objects, jlong** object_tags) override {
    auto err =
        jvmti_->GetObjectsWithTags(tag_count, tags, count, objects, object_tags);
    return err == JVMTI_ERROR_NONE;
  }

  bool set_heap_sampling_interval(jint sampling_interval) override {
    auto err = jvmti_->SetHeapSamplingInterval(sampling_interval);
    return err == JVMTI_ERROR_NONE;
  }

  bool set_event_notification_mode(jvmtiEventMode mode, jvmtiEvent event_type,
                                   jthread event_thread) override {
    auto err = jvmti_->SetEventNotificationMode(mode, event_type, event_thread);
    return err == JVMTI_ERROR_NONE;
  }

  void deallocate(unsigned char* mem) override { jvmti_->Deallocate(mem); }
};

} // namespace criterium

#endif // CRITERIUM_JVMTI_OPERATIONS_H
