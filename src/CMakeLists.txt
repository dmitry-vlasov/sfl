
include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/../include
  ${CMAKE_CURRENT_SOURCE_DIR}/../peglib
)

add_executable(sfl
	sfl/sfl_elim_closure.cpp 
	sfl/sfl_sema.cpp 
	sfl/sfl_parser.cpp 
	sfl/sfl_unify.cpp
	
	sbl/sbl_sema.cpp 
	sbl/sbl_parser.cpp 
	main.cpp
)

