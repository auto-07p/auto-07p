# plaut -- 2D bifurcation/solution plotting utility (PostScript output).
#
# Despite the "2D GUI" option name this is a pure-Fortran program (no Motif/X11),
# so the CMake port just mirrors plaut/Makefile.in. Enabled by -DBUILD_2D_GUI=ON.
#
# plautmain.f does USE COMPAT, whose module comes from the engine's
# src/f2003.f90; plautdev.f INCLUDEs plautcom.f from this directory (resolved
# by the compiler relative to the source file, so no extra include path needed).

add_executable(plaut
  plautdev.f plautint.f plautlib.f plautmain.f
  "${CMAKE_SOURCE_DIR}/src/f2003.f90")     # provides module compat

set_target_properties(plaut PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/bin"
  Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules")
target_compile_options(plaut PRIVATE -O)

# Package: install the executable (cmake --install).
include(GNUInstallDirs)
install(TARGETS plaut RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
