# Example: build a C equation against the INSTALLED AUTO engine (W5, Goal 9).
#
# This is a standalone downstream project -- NOT part of the AUTO build. After
# installing AUTO (cmake --install), configure this with AUTO on the prefix path:
#
#   cmake -S . -B build -DCMAKE_PREFIX_PATH=<auto-install-prefix>
#   cmake --build build
#   cd build && cp ../../ab/c.ab fort.2 && ./ab      # writes b/s/d files
#
cmake_minimum_required(VERSION 3.13)
project(auto_clink_example C Fortran)

find_package(AUTO REQUIRED)

# PROGRAM AUTO (the entry point) lives in the engine, so a C equation links as
# an executable; the Fortran runtime is pulled in via the Fortran linker.
add_executable(ab ab.c)
target_link_libraries(ab AUTO::auto AUTO::auto_c)
# The engine is Fortran (and PROGRAM AUTO is the entry point), so link with the
# Fortran driver to pull in the Fortran runtime -- same as cmds.make uses $(FC)
# to link C equations. (AUTO::auto already adds -fopenmp for libgomp.)
set_target_properties(ab PROPERTIES LINKER_LANGUAGE Fortran)
