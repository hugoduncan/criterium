(ns criterium.agent.method-call-test
  "Tests for the MethodCall Java class.

  Tests verify the MethodCall data class can be loaded and instantiated,
  and that its fields are accessible from Clojure. These tests validate
  the Java bridge for method tracing before the Clojure wrapper is implemented."
  (:require
   [clojure.test :refer [deftest is testing]]))

;;; MethodCall class access

(def ^:private method-call-class
  "Lazily resolved MethodCall class, or nil if not available."
  (delay
    (try
      (Class/forName "criterium.agent.MethodCall")
      (catch ClassNotFoundException _
        nil))))

(deftest method-call-class-loading-test
  ;; Verifies the MethodCall class is available on the classpath.
  (testing "MethodCall class"
    (testing "is loadable"
      (is (some? @method-call-class)
          "MethodCall class should be loadable"))))

(deftest method-call-field-access-test
  ;; Verifies MethodCall fields are accessible via reflection.
  (testing "MethodCall fields"
    (when @method-call-class
      (testing "declares expected public fields"
        (let [fields (->> (.getDeclaredFields ^Class @method-call-class)
                          (map #(.getName ^java.lang.reflect.Field %))
                          set)]
          (is (contains? fields "class_name")
              "Should have class_name field")
          (is (contains? fields "method_name")
              "Should have method_name field")
          (is (contains? fields "source_file")
              "Should have source_file field")
          (is (contains? fields "line_number")
              "Should have line_number field")
          (is (contains? fields "call_count")
              "Should have call_count field")
          (is (contains? fields "children")
              "Should have children field"))))))

(deftest method-call-instantiation-test
  ;; Verifies MethodCall can be constructed with expected parameters.
  (testing "MethodCall instantiation"
    (when @method-call-class
      (testing "can be instantiated with valid parameters"
        (let [ctor (.getConstructor
                    ^Class @method-call-class
                    (into-array Class [String String String Long/TYPE Long/TYPE
                                       (Class/forName
                                        "[Lcriterium.agent.MethodCall;")]))
              empty-children (make-array @method-call-class 0)
              instance (.newInstance
                        ctor
                        (object-array ["Ltest/Class;" "testMethod" "Test.java"
                                       (long 42) (long 5) empty-children]))]
          (is (some? instance)
              "Should create MethodCall instance")

          ;; Verify field values via reflection
          (let [class-field (.getField ^Class @method-call-class "class_name")
                method-field (.getField ^Class @method-call-class "method_name")
                source-field (.getField ^Class @method-call-class "source_file")
                line-field (.getField ^Class @method-call-class "line_number")
                count-field (.getField ^Class @method-call-class "call_count")
                children-field (.getField ^Class @method-call-class "children")]
            (is (= "Ltest/Class;" (.get class-field instance))
                "class_name should match")
            (is (= "testMethod" (.get method-field instance))
                "method_name should match")
            (is (= "Test.java" (.get source-field instance))
                "source_file should match")
            (is (= 42 (.get line-field instance))
                "line_number should match")
            (is (= 5 (.get count-field instance))
                "call_count should match")
            (is (= 0 (alength ^objects (.get children-field instance)))
                "children should be empty array")))))))

(deftest method-call-nested-children-test
  ;; Verifies MethodCall can hold nested children.
  (testing "MethodCall with children"
    (when @method-call-class
      (testing "supports nested structure"
        (let [ctor (.getConstructor
                    ^Class @method-call-class
                    (into-array Class [String String String Long/TYPE Long/TYPE
                                       (Class/forName
                                        "[Lcriterium.agent.MethodCall;")]))
              empty-children (make-array @method-call-class 0)
              child (.newInstance
                     ctor
                     (object-array ["Lchild/Class;" "childMethod" "Child.java"
                                    (long 10) (long 3) empty-children]))
              children-array (make-array @method-call-class 1)
              _ (aset ^objects children-array 0 child)
              parent (.newInstance
                      ctor
                      (object-array
                       ["Lparent/Class;" "parentMethod" "Parent.java"
                        (long 5) (long 1) children-array]))]
          (is (some? parent)
              "Should create parent MethodCall")
          (let [children-field (.getField ^Class @method-call-class "children")
                retrieved-children (.get children-field parent)]
            (is (= 1 (alength ^objects retrieved-children))
                "Parent should have one child")
            (let [retrieved-child (aget ^objects retrieved-children 0)
                  method-field (.getField
                                ^Class @method-call-class
                                "method_name")]
              (is (= "childMethod" (.get method-field retrieved-child))
                  "Child method name should match"))))))))
