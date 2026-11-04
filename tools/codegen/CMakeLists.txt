cmake_minimum_required(VERSION 3.22)
project(vf64_codegen LANGUAGES Fortran CXX)
set(FORTGEN_SOURCE_DIR "" CACHE PATH "FortGen source with typed scalar IR")
if(NOT EXISTS "${FORTGEN_SOURCE_DIR}/CMakeLists.txt")
    message(FATAL_ERROR "Set FORTGEN_SOURCE_DIR to a FortGen checkout")
endif()
get_filename_component(default_shader_source_dir
    "${CMAKE_CURRENT_SOURCE_DIR}/../../Sources/VF64Metal/Shaders" ABSOLUTE)
set(VF64_SHADER_SOURCE_DIR "${default_shader_source_dir}" CACHE PATH
    "VF64 shader sources used by the regeneration check")
set(FORTGEN_BUILD_TESTING OFF CACHE BOOL "" FORCE)
add_subdirectory("${FORTGEN_SOURCE_DIR}" fortgen)
add_executable(generate_division generate_division.f90)
target_link_libraries(generate_division PRIVATE fortgen::fortgen)
enable_testing()
add_test(NAME division_regenerate COMMAND ${CMAKE_COMMAND}
    -DGENERATOR=$<TARGET_FILE:generate_division>
    -DSHADER_SOURCE_DIR=${VF64_SHADER_SOURCE_DIR}
    -DBINARY_DIR=${CMAKE_CURRENT_BINARY_DIR}
    -P ${CMAKE_CURRENT_SOURCE_DIR}/check-generated.cmake)
add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/CertifiedDivision.cuh
    COMMAND generate_division ${CMAKE_CURRENT_BINARY_DIR}/CertifiedDivision.metal
        ${CMAKE_CURRENT_BINARY_DIR}/CertifiedDivision.cuh
    DEPENDS generate_division VERBATIM)
add_executable(test_division test_division.cpp ${CMAKE_CURRENT_BINARY_DIR}/CertifiedDivision.cuh)
target_include_directories(test_division PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
target_compile_features(test_division PRIVATE cxx_std_20)
if(CMAKE_CXX_COMPILER_ID MATCHES "Clang|GNU")
    target_compile_options(test_division PRIVATE -fno-fast-math -ffp-contract=off
        -fsanitize=undefined,float-cast-overflow -fno-sanitize-recover=all)
    target_link_options(test_division PRIVATE -fsanitize=undefined,float-cast-overflow)
endif()
add_test(NAME division_host COMMAND test_division)
