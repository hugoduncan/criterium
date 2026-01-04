#include <gtest/gtest.h>
#include <gmock/gmock.h>

// Placeholder tests to verify GoogleTest and GMock setup works
TEST(SetupTest, GoogleTestWorks) {
    EXPECT_TRUE(true);
}

TEST(SetupTest, GMockWorks) {
    std::vector<int> v = {1, 2, 3};
    EXPECT_THAT(v, testing::Contains(2));
}
