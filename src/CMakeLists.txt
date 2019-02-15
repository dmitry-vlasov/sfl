
include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/../include
  ${CMAKE_CURRENT_SOURCE_DIR}/../peglib
)

add_executable(spl sema.cpp parser.cpp main.cpp)

target_link_libraries(spl stdc++fs)