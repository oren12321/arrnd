cmake_minimum_required(VERSION 3.24.4)

project(sample
    VERSION 1.0.0
    DESCRIPTION "A header-only C++ sample project"
    HOMEPAGE_URL "https://github.com/oren12321/cpp-header-only-sample-library")

if (NOT WIN32)
    if (CMAKE_BUILD_TYPE STREQUAL "Debug")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=address -Wall")
    endif()
endif()

include(GNUInstallDirs)

add_library(${PROJECT_NAME} INTERFACE)
target_include_directories(${PROJECT_NAME} INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>)
set_property(TARGET ${PROJECT_NAME} PROPERTY CXX_STANDARD 20)
add_library(${PROJECT_NAME}::${PROJECT_NAME} ALIAS ${PROJECT_NAME})

if (WIN32)
    file(GLOB_RECURSE HEADER_SRCS CONFIGURE_DEPENDS "${PROJECT_SOURCE_DIR}/include/*.h")
    add_library(${PROJECT_NAME}_ ${HEADER_SRCS})
    set_target_properties(${PROJECT_NAME}_ PROPERTIES LINKER_LANGUAGE CXX)
    target_link_libraries(${PROJECT_NAME}_ ${PROJECT_NAME}::${PROJECT_NAME})
    set_property(TARGET ${PROJECT_NAME}_ PROPERTY CXX_STANDARD 20)
endif()

install(TARGETS ${PROJECT_NAME}
    EXPORT ${PROJECT_NAME}_Targets
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMALE_INSTALL_BINDIR})

include(CMakePackageConfigHelpers)
write_basic_package_version_file("${PROJECT_NAME}ConfigVersion.cmake"
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY SameMajorVersion)

configure_package_config_file(
    "${PROJECT_SOURCE_DIR}/cmake/${PROJECT_NAME}Config.cmake.in"
    "${PROJECT_BINARY_DIR}/${PROJECT_NAME}Config.cmake"
    INSTALL_DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/${PROJECT_NAME}/cmake)

install(
    EXPORT ${PROJECT_NAME}_Targets
    FILE ${PROJECT_NAME}Targets.cmake
    NAMESPACE ${PROJECT_NAME}::
    DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/${PROJECT_NAME}/cmake)

install(
    FILES "${PROJECT_BINARY_DIR}/${PROJECT_NAME}Config.cmake" "${PROJECT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake"
    DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/${PROJECT_NAME}/cmake)

install(DIRECTORY ${PROJECT_SOURCE_DIR}/include/${PROJECT_NAME} DESTINATION include)

include(FetchContent)

FetchContent_Declare(
    googletest
    GIT_REPOSITORY https://github.com/google/googletest.git
    GIT_TAG release-1.11.0
    FIND_PACKAGE_ARGS NAMES GTest)
FetchContent_MakeAvailable(googletest)

FetchContent_Declare(
    benchmark
    GIT_REPOSITORY https://github.com/google/benchmark.git
    GIT_TAG v1.6.1
    FIND_PACKAGE_ARGS)
FetchContent_MakeAvailable(benchmark)

file(GLOB_RECURSE TESTS_SRCS CONFIGURE_DEPENDS "${PROJECT_SOURCE_DIR}/tests/*.cpp")
add_executable(${PROJECT_NAME}_tests ${TESTS_SRCS})
target_include_directories(${PROJECT_NAME}_tests PUBLIC ${CMAKE_SOURCE_DIR}/include)
target_link_libraries(${PROJECT_NAME}_tests ${PROJECT_NAME}::${PROJECT_NAME} GTest::gtest GTest::gtest_main)
set_property(TARGET ${PROJECT_NAME}_tests PROPERTY CXX_STANDARD 20)

file(GLOB_RECURSE BENCHMARK_SRCS CONFIGURE_DEPENDS "${PROJECT_SOURCE_DIR}/benchmark/*.cpp")
add_executable(${PROJECT_NAME}_benchmark ${BENCHMARK_SRCS})
target_include_directories(${PROJECT_NAME}_benchmark PUBLIC ${CMAKE_SOURCE_DIR}/include)
target_link_libraries(${PROJECT_NAME}_benchmark ${PROJECT_NAME}::${PROJECT_NAME} benchmark::benchmark)
set_property(TARGET ${PROJECT_NAME}_benchmark PROPERTY CXX_STANDARD 20)

add_custom_target(library_tests COMMAND ${PROJECT_NAME}_tests DEPENDS ${PROJECT_NAME}_tests)
add_custom_target(library_benchmark COMMAND ${PROJECT_NAME}_benchmark DEPENDS ${PROJECT_NAME}_benchmark)
