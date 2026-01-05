#ifndef CRITERIUM_TEST_MOCKS_H
#define CRITERIUM_TEST_MOCKS_H

#include <gmock/gmock.h>
#include "include/jni_operations.h"
#include "include/jvmti_operations.h"

namespace criterium {
namespace test {

/// GMock implementation of IJvmtiOperations for unit testing.
class MockJvmtiOperations : public IJvmtiOperations {
public:
  // Class and method introspection
  MOCK_METHOD(bool, get_class_signature, (jclass klass, char** class_sig),
              (override));
  MOCK_METHOD(bool, get_source_file_name, (jclass klass, char** source_name),
              (override));
  MOCK_METHOD(bool, get_method_declaring_class,
              (jmethodID method, jclass* declaring_class), (override));
  MOCK_METHOD(bool, get_method_name, (jmethodID method, char** method_name),
              (override));

  // Stack trace
  MOCK_METHOD(bool, get_stack_trace,
              (jthread thread, jint start_depth, jint max_frame_count,
               jvmtiFrameInfo* frames, jint* count),
              (override));
  MOCK_METHOD(bool, get_line_number_table,
              (jmethodID method, jint* entry_count,
               jvmtiLineNumberEntry** line_table),
              (override));

  // Object tagging
  MOCK_METHOD(bool, set_tag, (jobject object, jlong tag), (override));
  MOCK_METHOD(bool, get_objects_with_tags,
              (jint tag_count, const jlong* tags, jint* count, jobject** objects,
               jlong** object_tags),
              (override));

  // Sampling and event control
  MOCK_METHOD(bool, set_heap_sampling_interval, (jint sampling_interval),
              (override));
  MOCK_METHOD(bool, set_event_notification_mode,
              (jvmtiEventMode mode, jvmtiEvent event_type, jthread event_thread),
              (override));

  // Memory deallocation
  MOCK_METHOD(void, deallocate, (unsigned char* mem), (override));
};

/// GMock implementation of IJniOperations for unit testing.
class MockJniOperations : public IJniOperations {
public:
  // Reference management
  MOCK_METHOD(jobject, new_global_ref, (JNIEnv* env, jobject obj), (override));
  MOCK_METHOD(void, delete_global_ref, (JNIEnv* env, jobject obj), (override));

  // String operations
  MOCK_METHOD(jstring, new_string_utf, (JNIEnv* env, const char* str),
              (override));

  // Method invocation
  MOCK_METHOD(jlong, call_long_method, (JNIEnv* env, jobject obj, jmethodID method),
              (override));
  MOCK_METHOD(void, call_static_void_method,
              (JNIEnv* env, jclass klass, jmethodID method, jobject arg),
              (override));

  // Object creation
  MOCK_METHOD(jobject, new_object_a,
              (JNIEnv* env, jclass klass, jmethodID ctor, const jvalue* args),
              (override));

  // Field access
  MOCK_METHOD(void, set_static_long_field,
              (JNIEnv* env, jclass klass, jfieldID field, jlong value),
              (override));
};

} // namespace test
} // namespace criterium

#endif // CRITERIUM_TEST_MOCKS_H
