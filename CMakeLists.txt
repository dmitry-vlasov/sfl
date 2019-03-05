cmake_minimum_required (VERSION 3.8)

project (SFL)

include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/../include
  ${CMAKE_CURRENT_SOURCE_DIR}/../peglib
)

# sources
add_subdirectory (src)

target_compile_features(sfl PUBLIC cxx_std_17)

