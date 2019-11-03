file(GLOB_RECURSE CPU_SOURCES *.cc)
list(REMOVE_ITEM CPU_SOURCES ${PROJECT_SOURCE_DIR}/src/cli_main.cc)

include(CheckCXXSourceCompiles)
check_cxx_source_compiles("
#include <xmmintrin.h>
int main() {
  char data = 0;
  const char* address = &data;
  _mm_prefetch(address, _MM_HINT_NTA);
  return 0;
}
" TSOOBGX_MM_PREFETCH_PRESENT)
check_cxx_source_compiles("
int main() {
  char data = 0;
  const char* address = &data;
  __builtin_prefetch(address, 0, 0);
  return 0;
}
" TSOOBGX_BUILTIN_PREFETCH_PRESENT)

# Add plugins to source files
if (PLUGIN_LZ4)
  list(APPEND PLUGINS_SOURCES ${PROJECT_SOURCE_DIR}/plugin/lz4/sparse_page_lz4_format.cc)
  list(APPEND SRC_LIBS lz4)
endif (PLUGIN_LZ4)
if (PLUGIN_DENSE_PARSER)
  list(APPEND PLUGINS_SOURCES ${PROJECT_SOURCE_DIR}/plugin/dense_parser/dense_libsvm.cc)
endif (PLUGIN_DENSE_PARSER)

#-- Object library
# Object library is necessary for jvm-package, which creates its own shared
# library.
if (USE_CUDA)
  file(GLOB_RECURSE CUDA_SOURCES *.cu)
  add_library(objtsoobgx OBJECT ${CPU_SOURCES} ${CUDA_SOURCES} ${PLUGINS_SOURCES})
  target_compile_definitions(objtsoobgx
    PRIVATE -DTSOOBGX_USE_CUDA=1)
  target_include_directories(objtsoobgx PRIVATE ${PROJECT_SOURCE_DIR}/cub/)
  target_compile_options(objtsoobgx PRIVATE
    $<$<COMPILE_LANGUAGE:CUDA>:--expt-extended-lambda>
    $<$<COMPILE_LANGUAGE:CUDA>:--expt-relaxed-constexpr>
    $<$<COMPILE_LANGUAGE:CUDA>:-lineinfo>
    $<$<AND:$<NOT:$<CXX_COMPILER_ID:MSVC>>,$<COMPILE_LANGUAGE:CUDA>>:--std=c++11>
    $<$<COMPILE_LANGUAGE:CUDA>:${GEN_CODE}>)

  if (USE_NCCL)
    find_package(Nccl REQUIRED)
    target_include_directories(objtsoobgx PRIVATE ${NCCL_INCLUDE_DIR})
    target_compile_definitions(objtsoobgx PRIVATE -DTSOOBGX_USE_NCCL=1)
    list(APPEND SRC_LIBS ${NCCL_LIBRARY})
  endif (USE_NCCL)

  if (USE_NVTX)
    target_include_directories(objtsoobgx PRIVATE "${NVTX_HEADER_DIR}")
    target_compile_definitions(objtsoobgx PRIVATE -DTSOOBGX_USE_NVTX=1)
  endif (USE_NVTX)

  # OpenMP is mandatory for cuda version
  find_package(OpenMP REQUIRED)
  target_compile_options(objtsoobgx PRIVATE  
    $<$<COMPILE_LANGUAGE:CUDA>:-Xcompiler=${OpenMP_CXX_FLAGS}>
  )

  set_target_properties(objtsoobgx PROPERTIES
    CUDA_SEPARABLE_COMPILATION OFF)
else (USE_CUDA)
  add_library(objtsoobgx OBJECT ${CPU_SOURCES} ${PLUGINS_SOURCES})
endif (USE_CUDA)

target_include_directories(objtsoobgx
  PRIVATE
  ${PROJECT_SOURCE_DIR}/include
  ${PROJECT_SOURCE_DIR}/dmlc-core/include
  ${PROJECT_SOURCE_DIR}/rabit/include)
target_compile_options(objtsoobgx
  PRIVATE
  $<$<AND:$<CXX_COMPILER_ID:MSVC>,$<COMPILE_LANGUAGE:CXX>>:/MP>
  $<$<AND:$<NOT:$<CXX_COMPILER_ID:MSVC>>,$<COMPILE_LANGUAGE:CXX>>:-funroll-loops>)
if (WIN32 AND MINGW)
  target_compile_options(objtsoobgx PUBLIC -static-libstdc++)
endif (WIN32 AND MINGW)

set_target_properties(objtsoobgx PROPERTIES
  POSITION_INDEPENDENT_CODE ON
  CXX_STANDARD 11
  CXX_STANDARD_REQUIRED ON)
target_compile_definitions(objtsoobgx
  PRIVATE
  -DDMLC_LOG_CUSTOMIZE=1  # enable custom logging
  $<$<NOT:$<CXX_COMPILER_ID:MSVC>>:_MWAITXINTRIN_H_INCLUDED>
  ${TSOOBGX_DEFINITIONS})
if (TSOOBGX_MM_PREFETCH_PRESENT)
  target_compile_definitions(objtsoobgx
    PRIVATE
    -DTSOOBGX_MM_PREFETCH_PRESENT=1)
endif(TSOOBGX_MM_PREFETCH_PRESENT)
if (TSOOBGX_BUILTIN_PREFETCH_PRESENT)
  target_compile_definitions(objtsoobgx
    PRIVATE
    -DTSOOBGX_BUILTIN_PREFETCH_PRESENT=1)
endif (TSOOBGX_BUILTIN_PREFETCH_PRESENT)

if (USE_OPENMP)
  find_package(OpenMP REQUIRED)
  if (OpenMP_CXX_FOUND OR OPENMP_FOUND)
    target_compile_options(objtsoobgx PRIVATE  $<$<COMPILE_LANGUAGE:CXX>:${OpenMP_CXX_FLAGS}>)
    if ((NOT OpenMP_CXX_LIBRARIES) AND (NOT MSVC))  # old CMake doesn't define this variable
      set(OpenMP_CXX_LIBRARIES "gomp;pthread")
    endif ((NOT OpenMP_CXX_LIBRARIES) AND (NOT MSVC))
    list(APPEND SRC_LIBS ${OpenMP_CXX_LIBRARIES})
    set(LINKED_LIBRARIES_PRIVATE "${LINKED_LIBRARIES_PRIVATE};${SRC_LIBS}" PARENT_SCOPE)
  endif (OpenMP_CXX_FOUND OR OPENMP_FOUND)
endif (USE_OPENMP)

# For MSVC: Call msvc_use_static_runtime() once again to completely
# replace /MD with /MT. See https://github.com/dmlc/tsoobgx/issues/4462
# for issues caused by mixing of /MD and /MT flags
msvc_use_static_runtime()

#-- End object library
