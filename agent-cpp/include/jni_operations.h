#ifndef CRITERIUM_JNI_OPERATIONS_H
#define CRITERIUM_JNI_OPERATIONS_H

#include <jni.h>

namespace criterium {

/// Interface for JNI operations to enable unit testing with mocks.
/// Abstracts the JNIEnv* calls used during runtime event processing.
/// Each method takes JNIEnv* as first parameter since JNIEnv is thread-local.
class IJniOperations {
public:
  virtual ~IJniOperations() = default;

  // Reference management
  virtual jobject new_global_ref(JNIEnv* env, jobject obj) = 0;
  virtual void delete_global_ref(JNIEnv* env, jobject obj) = 0;

  // String operations
  virtual jstring new_string_utf(JNIEnv* env, const char* str) = 0;

  // Method invocation
  virtual jlong call_long_method(JNIEnv* env, jobject obj, jmethodID method) = 0;
  virtual void call_static_void_method(JNIEnv* env, jclass klass,
                                       jmethodID method, jobject arg) = 0;

  // Object creation using jvalue array (NewObjectA)
  virtual jobject new_object_a(JNIEnv* env, jclass klass, jmethodID ctor,
                               const jvalue* args) = 0;

  // Field access
  virtual void set_static_long_field(JNIEnv* env, jclass klass, jfieldID field,
                                     jlong value) = 0;
};

/// Production implementation delegating to JNIEnv*.
class JniOperations : public IJniOperations {
public:
  JniOperations() = default;

  jobject new_global_ref(JNIEnv* env, jobject obj) override {
    return env->NewGlobalRef(obj);
  }

  void delete_global_ref(JNIEnv* env, jobject obj) override {
    env->DeleteGlobalRef(obj);
  }

  jstring new_string_utf(JNIEnv* env, const char* str) override {
    return env->NewStringUTF(str);
  }

  jlong call_long_method(JNIEnv* env, jobject obj, jmethodID method) override {
    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-vararg)
    return env->CallLongMethod(obj, method);
  }

  void call_static_void_method(JNIEnv* env, jclass klass, jmethodID method,
                               jobject arg) override {
    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-vararg)
    env->CallStaticVoidMethod(klass, method, arg);
  }

  jobject new_object_a(JNIEnv* env, jclass klass, jmethodID ctor,
                       const jvalue* args) override {
    return env->NewObjectA(klass, ctor, args);
  }

  void set_static_long_field(JNIEnv* env, jclass klass, jfieldID field,
                             jlong value) override {
    env->SetStaticLongField(klass, field, value);
  }
};

} // namespace criterium

#endif // CRITERIUM_JNI_OPERATIONS_H
