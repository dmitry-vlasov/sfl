# sfl
Simple Functional Language - for compiler learning purpose.

Dependencies: cmake, c++ compiler (g++, clang, etc.)

To build SFL:
1. create a directory build/debug: 'mkdir build; cd build; mkdir debug; cd debug'
2. run 'cmake ../../ -DCMAKE_BUILD_TYPE=Debug'
3. run 'make'

The executable will be in build/debug/src directory. To build release version.
do the same, but change 'debug/Debug' onto 'release/Release'

To run tests: 
1. from the top level dir, 'cd test'
2. run '../build/debug/src/sfl test1.sfl'
