# AUTO-07p core engine build (CMake).
#
# Goal (refactor-plan.md W0): reproduce EXACTLY the autotools lib/ layout that
# the Python CLI (runAUTO.py) and cmds/cmds.make depend on at runtime:
#   - one loose object per engine source, deposited in <root>/lib/  (the CLI
#     links `$AUTO_DIR/lib/*.o` wholesale, including main.o = PROGRAM AUTO)
#   - lib/libauto_c.a built from user_c.f90 (linked via -lauto_c for C equations)
#   - user_c.o is NOT left loose in lib/ (it lives only inside libauto_c.a)
#   - fcon.f is NOT built here: it is the standalone PROGRAM AUTCON, compiled
#     on demand by cmds.make. Putting it in lib/*.o would inject a second
#     PROGRAM and break every user-equation link.
# See src/Makefile.in for the authoritative object list this mirrors.

set(LIB_DIR "${CMAKE_SOURCE_DIR}/lib")

# @MPIFILE@ selection: nompi.f90 stub by default, real mpi.f90 with WITH_MPI.
if(WITH_MPI)
  set(AUTO_MPI_SRC mpi.f90)
else()
  set(AUTO_MPI_SRC nompi.f90)
endif()

# Engine sources, grouped by component (mirrors src/Makefile.in; NOT user_c.f90,
# NOT fcon.f). Kept in one OBJECT library so the deposited lib/*.o set is
# identical to autotools; the grouping documents the real components.
set(AUTO_CORE_SOURCES              # continuation/bifurcation engine core
  ae.f90 auto_constants.f90 bvp.f90 equilibrium.f90 f2003.f90 floquet.f90
  homcont.f90 interfaces.f90 io.f90 main.f90 maps.f90 mesh.f90
  optimization.f90 parabolic.f90 periodic.f90 solvebv.f90 support.f90
  timeint.f90 toolboxae.f90 toolboxbv.f90)
set(AUTO_MATH_SOURCES blas.f lapack.f)          # vendored BLAS/LAPACK
set(AUTO_PARALLEL_SOURCES ${AUTO_MPI_SRC})      # MPI stub (nompi) or real mpi
set(AUTO_ENGINE_SOURCES
  ${AUTO_CORE_SOURCES} ${AUTO_MATH_SOURCES} ${AUTO_PARALLEL_SOURCES})

# Shared module dir so auto_c (user_c.f90) can `use` engine modules (support, ...).
set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/modules")

# Match the autotools flags (src build used: gfortran -g -fopenmp -O).
set(AUTO_FFLAGS -g -fopenmp -O)

# Generate the CLI's build-config module (replaces cmds.make; W3): the compiler
# and flags runAUTO uses to compile user equations. Written into the package.
# Flags mirror what ./configure sets for the autotools build (the baseline), so
# a demo compiles identically under either build system -- in particular C
# equations get -O2 (not -O0).
set(AUTO_CFG_CC "${CMAKE_C_COMPILER}")
set(AUTO_CFG_FC "${CMAKE_Fortran_COMPILER}")
set(AUTO_CFG_CFLAGS "-g -O2")
set(AUTO_CFG_FFLAGS "-g -fopenmp")
set(AUTO_CFG_OPT "-O")
configure_file(
  "${CMAKE_SOURCE_DIR}/cmake/compilers.py.in"
  "${CMAKE_SOURCE_DIR}/python/auto/compilers.py" @ONLY)

# --- Engine as an OBJECT library -> the loose lib/*.o the CLI globs ----------
add_library(auto_engine OBJECT ${AUTO_ENGINE_SOURCES})
target_include_directories(auto_engine PRIVATE "${CMAKE_SOURCE_DIR}/include")
target_compile_options(auto_engine PRIVATE ${AUTO_FFLAGS})

# Deposit the loose objects into <root>/lib/ (CLI links $AUTO_DIR/lib/*.o).
# OBJECT libraries can't carry POST_BUILD commands, so use a custom target that
# runs after auto_engine. CMake names objects <src>.o (e.g. main.f90.o); the
# *.o glob still matches.
add_custom_target(deposit_engine_objects ALL
  COMMAND "${CMAKE_COMMAND}" -E make_directory "${LIB_DIR}"
  COMMAND "${CMAKE_COMMAND}" -E copy_if_different $<TARGET_OBJECTS:auto_engine> "${LIB_DIR}/"
  COMMAND_EXPAND_LISTS
  COMMENT "Depositing engine objects into lib/ (CLI links lib/*.o)")
add_dependencies(deposit_engine_objects auto_engine)

# --- C-interface archive: lib/libauto_c.a from user_c.f90 -------------------
add_library(auto_c STATIC user_c.f90)
add_dependencies(auto_c auto_engine)          # needs support.mod etc.
target_include_directories(auto_c PRIVATE "${CMAKE_SOURCE_DIR}/include")
target_compile_options(auto_c PRIVATE ${AUTO_FFLAGS})
set_target_properties(auto_c PROPERTIES
  OUTPUT_NAME auto_c
  ARCHIVE_OUTPUT_DIRECTORY "${LIB_DIR}")

# --- Optional PIC shared engine: libauto.so for the Python/ctypes path (W4) --
# A position-independent build of the whole engine PLUS user_c.f90 (the
# func/stpnt/... dispatchers, the `user` table, and the runtime setters), with
# NO compiled equation object.  With no C `const user` to win at link, the
# Fortran `user` module variable is the (mutable) definition that Python fills
# at runtime via auto_set_user().  It exports auto_main_c / auto_set_user /
# auto_reset_user.  Opt-in (-DAUTO_BUILD_SHARED=ON): the default CLI build and
# the autotools-parity lib/*.o layout are untouched.  Its own module dir keeps
# its PIC .mod files from racing auto_engine's non-PIC ones.
option(AUTO_BUILD_SHARED "Build libauto.so (PIC) for the Python callback path" OFF)
# Building the wheel (top-level BUILD_PYTHON) implies the shared engine.
# Target name auto_shared (file: libauto.so) -- the ergonomic name `auto` is the
# static engine below (AUTO::auto), which is what C/C++ users link (W5).
if(AUTO_BUILD_SHARED OR BUILD_PYTHON)
  add_library(auto_shared SHARED ${AUTO_ENGINE_SOURCES} user_c.f90)
  set_target_properties(auto_shared PROPERTIES
    POSITION_INDEPENDENT_CODE ON
    OUTPUT_NAME auto
    Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/modules_shared"
    LIBRARY_OUTPUT_DIRECTORY "${LIB_DIR}")
  target_include_directories(auto_shared PRIVATE "${CMAKE_SOURCE_DIR}/include")
  target_compile_options(auto_shared PRIVATE ${AUTO_FFLAGS})
  target_link_options(auto_shared PRIVATE -fopenmp)

  # For the wheel: also drop libauto.so into the package tree so setuptools
  # bundles it as package data (auto.callback.find_library() then locates it
  # in-package -- no AUTO_DIR needed at runtime).
  if(BUILD_PYTHON)
    add_custom_command(TARGET auto_shared POST_BUILD
      COMMAND "${CMAKE_COMMAND}" -E copy_if_different
              "$<TARGET_FILE:auto_shared>" "${CMAKE_SOURCE_DIR}/python/auto/libauto.so"
      COMMENT "Bundling libauto.so into python/auto/ for the wheel")
  endif()
endif()

# ============================================================================
# Installable engine for C/C++ users (refactor-plan.md W5, Goal 9)
# ----------------------------------------------------------------------------
# The successor to linking `$AUTO_DIR/lib/*.o`.  Mirrors the cmds.make model
# exactly, replacing the object glob with real libraries:
#     Fortran equation:  gfortran eqn.o -lauto
#     C equation:        gfortran eqn.o -lauto -lauto_c
# libauto.a is the engine as one static archive (same objects as lib/*.o --
# engine core + main.o = PROGRAM AUTO; NOT user_c, NOT fcon), reusing the
# already-compiled auto_engine objects.  libauto_c.a (target auto_c, above) is
# the C-interface dispatcher archive.
add_library(auto STATIC $<TARGET_OBJECTS:auto_engine>)
add_dependencies(auto auto_engine)
set_target_properties(auto PROPERTIES
  OUTPUT_NAME auto
  ARCHIVE_OUTPUT_DIRECTORY "${LIB_DIR}")

include(GNUInstallDirs)

# Interface include dir so downstream `#include <auto_f2c.h>` resolves against
# the installed headers (installed to <prefix>/include/auto).
target_include_directories(auto INTERFACE
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/auto>)
target_include_directories(auto_c INTERFACE
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/auto>)
# The engine is built with OpenMP; propagate -fopenmp so downstream links libgomp.
target_link_options(auto INTERFACE -fopenmp)

set(AUTO_PUBLIC_HEADERS
  "${CMAKE_SOURCE_DIR}/include/auto_f2c.h"   # C equation interface
  "${CMAKE_SOURCE_DIR}/include/auto.h"       # NPARX and engine constants
  "${CMAKE_SOURCE_DIR}/include/fcon.h")      # fcon converter interface

install(TARGETS auto auto_c
        EXPORT AUTOTargets
        ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}")
install(FILES ${AUTO_PUBLIC_HEADERS}
        DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/auto")
# The shared engine (libauto.so) is deliberately NOT installed into the system
# libdir: with libauto.a beside it, `-lauto` would pick the .so, and a compiled
# equation's `const user` cannot override the .so's own `user` without
# -rdynamic -- so the C link path must resolve `user` statically. The shared
# engine is instead distributed inside the Python wheel (W4).

# CMake package config: downstream `find_package(AUTO)` -> AUTO::auto / AUTO::auto_c.
install(EXPORT AUTOTargets
        FILE AUTOTargets.cmake
        NAMESPACE AUTO::
        DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/AUTO")

include(CMakePackageConfigHelpers)
configure_package_config_file(
  "${CMAKE_SOURCE_DIR}/cmake/AUTOConfig.cmake.in"
  "${CMAKE_BINARY_DIR}/AUTOConfig.cmake"
  INSTALL_DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/AUTO")
write_basic_package_version_file(
  "${CMAKE_BINARY_DIR}/AUTOConfigVersion.cmake"
  VERSION "${PROJECT_VERSION}"
  COMPATIBILITY SameMajorVersion)
install(FILES
  "${CMAKE_BINARY_DIR}/AUTOConfig.cmake"
  "${CMAKE_BINARY_DIR}/AUTOConfigVersion.cmake"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/AUTO")

# pkg-config: `pkg-config --cflags --libs auto` for plain Makefile users.
set(PKGCONFIG_LIBDIR "${CMAKE_INSTALL_FULL_LIBDIR}")
set(PKGCONFIG_INCLUDEDIR "${CMAKE_INSTALL_FULL_INCLUDEDIR}")
configure_file(
  "${CMAKE_SOURCE_DIR}/cmake/auto.pc.in"
  "${CMAKE_BINARY_DIR}/auto.pc" @ONLY)
install(FILES "${CMAKE_BINARY_DIR}/auto.pc"
        DESTINATION "${CMAKE_INSTALL_LIBDIR}/pkgconfig")
