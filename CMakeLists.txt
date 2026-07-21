cmake_minimum_required(VERSION 3.13)

project(AUTO07p VERSION 0.0.1 LANGUAGES C CXX Fortran)

# --- Build options ----------------------------------------------------------
option(BUILD_DOCUMENTATION "Build the project documentation" OFF)
option(USE_EXTERNAL_LIBRARY "Use optional external BLAS/LAPACK" OFF)
option(BUILD_DEMOS "Build the demos" OFF)          # W6: needs demos/CMakeLists.txt
option(BUILD_2D_GUI "Build 2D plot application" OFF)
option(BUILD_3D_GUI "Build 3D plot application" OFF)
option(BUILD_PYTHON "Build python wheel" OFF)      # W4/W9
option(WITH_MPI "Build with MPI support" OFF)
option(WITH_CUDA "Build with CUDA support" OFF)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

if(WITH_MPI)
  find_package(MPI REQUIRED)
endif()

# --- Core engine (produces lib/*.o + lib/libauto_c.a; see src/CMakeLists.txt)
add_subdirectory(src)

# --- Optional components -----------------------------------------------------
# demos (W6), the 2D plotter (plaut, pure Fortran), and the 3D GUI (plaut04,
# Qt/Coin3D/SoQt) all have CMake support.
if(BUILD_DEMOS)
  add_subdirectory(demos)
endif()
if(BUILD_2D_GUI)
  add_subdirectory(plaut)
endif()
if(BUILD_3D_GUI)
  add_subdirectory(plaut04)
endif()
if(BUILD_DOCUMENTATION)
  add_subdirectory(doc)
endif()
