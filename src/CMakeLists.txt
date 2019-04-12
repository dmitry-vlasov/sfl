
include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/../include
  ${CMAKE_CURRENT_SOURCE_DIR}/../peglib
)

add_executable(sfl 
	sfl/sfl_sema.cpp 
	sfl/sfl_parser.cpp 
	sfl/sfl_unify.cpp 
	main.cpp
)

