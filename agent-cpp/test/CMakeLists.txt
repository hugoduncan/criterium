# Test executable
add_executable(criterium_tests
    test_main.cpp
)

target_link_libraries(criterium_tests
    GTest::gtest_main
    GTest::gmock
)

target_compile_options(criterium_tests PRIVATE ${WARNING_FLAGS})

include(GoogleTest)
gtest_discover_tests(criterium_tests)
