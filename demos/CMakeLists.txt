# Build every demo's equation into an executable in one shot (refactor-plan.md W6).
#
# Enabled with -DBUILD_DEMOS=ON. Each demo compiles its equation source and
# links the engine (target `auto`, which carries PROGRAM AUTO); C equations also
# link `auto_c`. A single `cmake --build` then produces the engine libraries AND
# every demo executable, so a build break in any demo is caught up front.
# Running a demo still uses its c.<name> constants at run time (unchanged).

# Subdirectories here that are not compiled-equation demos.
set(AUTO_NON_DEMO_DIRS clink pycallback python)

file(GLOB _entries RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}"
     "${CMAKE_CURRENT_SOURCE_DIR}/*")

set(_built "")
foreach(_name ${_entries})
  set(_dir "${CMAKE_CURRENT_SOURCE_DIR}/${_name}")
  if(NOT IS_DIRECTORY "${_dir}" OR _name IN_LIST AUTO_NON_DEMO_DIRS)
    continue()
  endif()

  # Pick the equation source: prefer <name>.f90 / <name>.c, else the sole source
  # in the dir (handles fhh -> fhn.f90, fnb -> fhn.c).
  set(_src "")
  set(_is_c FALSE)
  if(EXISTS "${_dir}/${_name}.f90")
    set(_src "${_dir}/${_name}.f90")
  elseif(EXISTS "${_dir}/${_name}.c")
    set(_src "${_dir}/${_name}.c")
    set(_is_c TRUE)
  else()
    file(GLOB _cand "${_dir}/*.f90" "${_dir}/*.c")
    list(LENGTH _cand _n)
    if(_n EQUAL 1)
      list(GET _cand 0 _src)
      if(_src MATCHES "\\.c$")
        set(_is_c TRUE)
      endif()
    endif()
  endif()
  if(_src STREQUAL "")
    continue()
  endif()

  # Target demo_<name> -> executable named <name> in build/demos/<name>/.
  add_executable(demo_${_name} "${_src}")
  set_target_properties(demo_${_name} PROPERTIES
    OUTPUT_NAME "${_name}"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/${_name}"
    Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules/${_name}")
  target_include_directories(demo_${_name} PRIVATE "${CMAKE_SOURCE_DIR}/include")
  # `auto` carries PROGRAM AUTO + the -fopenmp interface link option; C equations
  # add the dispatcher archive and link with the Fortran driver (Fortran runtime).
  if(_is_c)
    target_link_libraries(demo_${_name} auto auto_c)
    set_target_properties(demo_${_name} PROPERTIES LINKER_LANGUAGE Fortran)
  else()
    target_link_libraries(demo_${_name} auto)
  endif()
  list(APPEND _built ${_name})
endforeach()

list(LENGTH _built _count)
message(STATUS "AUTO demos: ${_count} demo executables configured")
