execute_process(COMMAND "${GENERATOR}" "${BINARY_DIR}/CertifiedDivision.metal" "${BINARY_DIR}/CertifiedDivision.cuh" RESULT_VARIABLE status)
if(NOT status EQUAL 0)
    message(FATAL_ERROR "Division generation failed")
endif()
execute_process(COMMAND "${CMAKE_COMMAND}" -E compare_files
    "${BINARY_DIR}/CertifiedDivision.metal"
    "${SHADER_SOURCE_DIR}/IEEE/CertifiedDivision.metal"
    RESULT_VARIABLE status)
if(NOT status EQUAL 0)
    message(FATAL_ERROR "CertifiedDivision.metal needs regeneration")
endif()
