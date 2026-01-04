#include <gtest/gtest.h>
#include "include/alloc_rec.h"

using criterium::AllocRec;
using criterium::AllocsT;

// Tests for all_tags() which extracts tag values from allocation records.
// The function is used to collect tags for batch JVMTI operations.

class AllTagsTest : public ::testing::Test {
protected:
  // Helper to create a minimal AllocRec with just a tag value
  static std::unique_ptr<AllocRec> make_alloc(int64_t tag) {
    return std::make_unique<AllocRec>(
        "Ltest/Class;",  // obj_class
        100,             // obj_size
        "Ltest/Caller;", // call_class
        "method",        // call_method
        "file.java",     // call_file
        42,              // call_line
        "Ltest/Alloc;",  // alloc_class
        "alloc",         // alloc_method
        "alloc.java",    // alloc_file
        10,              // alloc_line
        1,               // thread_id
        tag              // tag
    );
  }
};

TEST_F(AllTagsTest, ReturnsEmptyVectorForEmptyInput) {
  AllocsT allocs;
  auto tags = criterium::all_tags(allocs);
  EXPECT_TRUE(tags.empty());
}

TEST_F(AllTagsTest, ExtractsSingleTag) {
  AllocsT allocs;
  allocs.push_back(make_alloc(42));

  auto tags = criterium::all_tags(allocs);

  ASSERT_EQ(tags.size(), 1);
  EXPECT_EQ(tags[0], 42);
}

TEST_F(AllTagsTest, ExtractsMultipleTags) {
  AllocsT allocs;
  allocs.push_back(make_alloc(10));
  allocs.push_back(make_alloc(20));
  allocs.push_back(make_alloc(30));

  auto tags = criterium::all_tags(allocs);

  ASSERT_EQ(tags.size(), 3);
  EXPECT_EQ(tags[0], 10);
  EXPECT_EQ(tags[1], 20);
  EXPECT_EQ(tags[2], 30);
}

TEST_F(AllTagsTest, PreservesOrderOfTags) {
  AllocsT allocs;
  allocs.push_back(make_alloc(300));
  allocs.push_back(make_alloc(100));
  allocs.push_back(make_alloc(200));

  auto tags = criterium::all_tags(allocs);

  ASSERT_EQ(tags.size(), 3);
  EXPECT_EQ(tags[0], 300);
  EXPECT_EQ(tags[1], 100);
  EXPECT_EQ(tags[2], 200);
}

TEST_F(AllTagsTest, HandlesDuplicateTags) {
  AllocsT allocs;
  allocs.push_back(make_alloc(42));
  allocs.push_back(make_alloc(42));
  allocs.push_back(make_alloc(42));

  auto tags = criterium::all_tags(allocs);

  ASSERT_EQ(tags.size(), 3);
  EXPECT_EQ(tags[0], 42);
  EXPECT_EQ(tags[1], 42);
  EXPECT_EQ(tags[2], 42);
}

TEST_F(AllTagsTest, HandlesZeroTag) {
  AllocsT allocs;
  allocs.push_back(make_alloc(0));

  auto tags = criterium::all_tags(allocs);

  ASSERT_EQ(tags.size(), 1);
  EXPECT_EQ(tags[0], 0);
}

TEST_F(AllTagsTest, HandlesNegativeTags) {
  AllocsT allocs;
  allocs.push_back(make_alloc(-1));
  allocs.push_back(make_alloc(-100));

  auto tags = criterium::all_tags(allocs);

  ASSERT_EQ(tags.size(), 2);
  EXPECT_EQ(tags[0], -1);
  EXPECT_EQ(tags[1], -100);
}

TEST_F(AllTagsTest, HandlesLargeTagValues) {
  AllocsT allocs;
  // Max int64_t value
  allocs.push_back(make_alloc(INT64_MAX));
  // Min int64_t value
  allocs.push_back(make_alloc(INT64_MIN));

  auto tags = criterium::all_tags(allocs);

  ASSERT_EQ(tags.size(), 2);
  EXPECT_EQ(tags[0], INT64_MAX);
  EXPECT_EQ(tags[1], INT64_MIN);
}

TEST_F(AllTagsTest, DoesNotModifyInput) {
  AllocsT allocs;
  allocs.push_back(make_alloc(42));

  // Capture original tag
  int64_t original_tag = allocs[0]->tag;

  criterium::all_tags(allocs);

  // Verify input unchanged
  EXPECT_EQ(allocs[0]->tag, original_tag);
  EXPECT_EQ(allocs.size(), 1);
}

TEST_F(AllTagsTest, WorksWithLargeCollection) {
  AllocsT allocs;
  constexpr int COUNT = 1000;

  for (int i = 0; i < COUNT; i++) {
    allocs.push_back(make_alloc(i));
  }

  auto tags = criterium::all_tags(allocs);

  ASSERT_EQ(tags.size(), COUNT);
  for (int i = 0; i < COUNT; i++) {
    EXPECT_EQ(tags[i], i);
  }
}
