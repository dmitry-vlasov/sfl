
include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/../include
  ${CMAKE_CURRENT_SOURCE_DIR}/../peglib
)

add_executable(sfl sema.cpp parser.cpp main.cpp)

target_link_libraries(sfl stdc++fs)