#include <gtest/gtest.h>
#include "include/utils.h"

using criterium::is_system_class;

// Tests for is_system_class() which filters out JVM system classes
// from allocation tracking based on class name prefixes.

class IsSystemClassTest : public ::testing::Test {};

// System class prefixes that should be filtered
TEST_F(IsSystemClassTest, RecognizesJavaClasses) {
  EXPECT_TRUE(is_system_class("Ljava/lang/String;"));
  EXPECT_TRUE(is_system_class("Ljava/util/HashMap;"));
  EXPECT_TRUE(is_system_class("Ljava/io/File;"));
}

TEST_F(IsSystemClassTest, RecognizesComSunClasses) {
  EXPECT_TRUE(is_system_class("Lcom/sun/proxy/$Proxy0;"));
  EXPECT_TRUE(is_system_class("Lcom/sun/management/VMOption;"));
}

TEST_F(IsSystemClassTest, RecognizesJdkClasses) {
  EXPECT_TRUE(is_system_class("Ljdk/internal/misc/Unsafe;"));
  EXPECT_TRUE(is_system_class("Ljdk/nashorn/api/scripting/NashornScriptEngine;"));
}

TEST_F(IsSystemClassTest, RecognizesJavaxClasses) {
  EXPECT_TRUE(is_system_class("Ljavax/swing/JFrame;"));
  EXPECT_TRUE(is_system_class("Ljavax/management/MBeanServer;"));
}

TEST_F(IsSystemClassTest, RecognizesSunManagementClasses) {
  EXPECT_TRUE(is_system_class("Lsun/management/VMManagementImpl;"));
  EXPECT_TRUE(is_system_class("Lsun/management/ManagementFactory;"));
}

TEST_F(IsSystemClassTest, RecognizesClojureClasses) {
  EXPECT_TRUE(is_system_class("Lclojure/lang/PersistentVector;"));
  EXPECT_TRUE(is_system_class("Lclojure/core$map;"));
}

// Non-system classes that should NOT be filtered
TEST_F(IsSystemClassTest, AllowsUserClasses) {
  EXPECT_FALSE(is_system_class("Lcom/example/MyClass;"));
  EXPECT_FALSE(is_system_class("Lorg/apache/commons/StringUtils;"));
  EXPECT_FALSE(is_system_class("Lmyapp/Service;"));
}

TEST_F(IsSystemClassTest, AllowsOrgClasses) {
  EXPECT_FALSE(is_system_class("Lorg/junit/Test;"));
  EXPECT_FALSE(is_system_class("Lorg/slf4j/Logger;"));
}

TEST_F(IsSystemClassTest, AllowsNetClasses) {
  EXPECT_FALSE(is_system_class("Lnet/sf/cglib/Enhancer;"));
}

// Edge cases
TEST_F(IsSystemClassTest, HandlesPrefixSubstrings) {
  // "Ljavax" starts with "Ljava" but should be handled correctly
  EXPECT_TRUE(is_system_class("Ljavax/swing/JButton;"));

  // "Ljava" prefix should not match classes starting with "Ljavax"
  // when we're specifically testing "Ljava/" (with slash)
  // This tests that the prefix matching includes the trailing slash
  EXPECT_TRUE(is_system_class("Ljava/awt/Color;"));
}

TEST_F(IsSystemClassTest, RejectsPartialPrefixMatches) {
  // "Ljavax" without the trailing slash in prefix would incorrectly match
  // but our prefix is "Ljavax/" with the slash
  EXPECT_FALSE(is_system_class("Ljavaxyz/Custom;"));
}

TEST_F(IsSystemClassTest, HandlesSunButNotSunManagement) {
  // "Lsun/" is NOT a system prefix, only "Lsun/management"
  EXPECT_FALSE(is_system_class("Lsun/misc/Unsafe;"));
  EXPECT_FALSE(is_system_class("Lsun/nio/ch/FileChannelImpl;"));
}

TEST_F(IsSystemClassTest, HandlesEmptyString) {
  EXPECT_FALSE(is_system_class(""));
}

TEST_F(IsSystemClassTest, HandlesMinimalValidInput) {
  // Just the prefix without any class name
  EXPECT_TRUE(is_system_class("Ljava/"));
  EXPECT_TRUE(is_system_class("Lclojure/"));
}

TEST_F(IsSystemClassTest, HandlesPrimitiveArrays) {
  // Array descriptors start with '[', not 'L'
  EXPECT_FALSE(is_system_class("[I"));
  EXPECT_FALSE(is_system_class("[Ljava/lang/String;"));
}
