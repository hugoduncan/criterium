# Test executable
add_executable(criterium_tests
    test_main.cpp
    agent_state_test.cpp
    all_tags_test.cpp
    call_tree_test.cpp
    integration_test.cpp
    is_system_class_test.cpp
    message_queue_test.cpp
    method_tracing_state_transitions_test.cpp
    state_transitions_test.cpp
)

target_include_directories(criterium_tests PRIVATE
    ${CMAKE_SOURCE_DIR}
)

target_link_libraries(criterium_tests
    GTest::gtest_main
    GTest::gmock
)

target_compile_options(criterium_tests PRIVATE ${WARNING_FLAGS})

include(GoogleTest)
gtest_discover_tests(criterium_tests)
