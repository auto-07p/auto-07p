Plan: I'll outline a safe, minimal refactor so AUTO keeps running standalone and also exposes a callable entry point usable from Python.

Recommendation (short)

Yes — refactor the PROGRAM logic into a callable Fortran subroutine (e.g. auto_main) and keep PROGRAM AUTO that simply calls it. Add a small C-bind wrapper subroutine (with bind(C, name="auto_main_c")) that calls auto_main. Build both the executable and a shared library exposing auto_main_c. Python can then load the shared library and call auto_main_c (via ctypes or cffi).
Why this pattern

Keeps backward compatibility: existing PROGRAM AUTO unchanged in behavior.
Exposes a stable C ABI symbol for Python to call.
Avoids binary hacks and respects Fortran/C name-mangling rules.
Lets you build both executable and shared library from same sources.
Concrete steps & examples

Move logic into a subroutine (example patch you can apply to main.f90):

module auto_api  implicit nonecontains  subroutine auto_main()    ! -- move the body of your PROGRAM AUTO here --    ! (initializations, main loop, etc.)  end subroutine auto_main  subroutine auto_main_c() bind(C, name="auto_main_c")    use iso_c_binding    implicit none    call auto_main()  end subroutine auto_main_cend module auto_apiprogram AUTO  use auto_api  call auto_main()end program AUTO
Notes:

Ensure auto_main does not call exit() or call stop in ways that terminate the hosting Python process; instead return error codes or raise exceptions through return values.
If main uses command-line arguments, either (a) change it to accept an argument list, or (b) keep the program PROGRAM AUTO to parse argv, and for the callable API provide parameters via module variables or a new subroutine interface.
CMake: build both an executable and a shared library

# add library target (shared)add_library(auto_shared SHARED ${AUTO_SOURCES})set_target_properties(auto_shared PROPERTIES POSITION_INDEPENDENT_CODE ON)target_include_directories(auto_shared PRIVATE ${CMAKE_SOURCE_DIR}/include)target_link_libraries(auto_shared PRIVATE <dependencies>)# existing executableadd_executable(auto_exec ${AUTO_SOURCES})target_link_libraries(auto_exec PRIVATE <dependencies>)
Make sure to compile with PIC for shared objects. Use POSITION_INDEPENDENT_CODE ON or -fPIC.
If you want one target producing both, keep sources centralized and create both targets from same source list.
Python example (ctypes)

from ctypes import CDLLlib = CDLL("/path/to/libauto_shared.so")# if auto_main_c returns void: call directlylib.auto_main_c()# If you create a signature with int return:# lib.auto_main_c.restype = ctypes.c_int# rc = lib.auto_main_c()
Tests & considerations
Thread-safety / reentrancy: Fortran code often uses globals and COMMON blocks; repeated calls need to be safe or require reinitialization.
I/O: Fortran may print to stdout/stderr — that’s fine but be aware in notebooks.
Signals/exit: convert calls to stop/exit into return values, or wrap them to avoid killing Python.
If you prefer auto-generated Python bindings, f2py or f90wrap can expose typed wrappers; still useful to have a simple C ABI wrapper if you want to call the whole app.
Would you like me to:

Implement the auto_main + auto_main_c refactor in main.f90 and update CMakeLists.txt to add the shared-library target, and add a short PyAuto ctypes example (I can run the build/test here)? (I can do it now.)
Or just produce the exact patch/diff for you to review?
