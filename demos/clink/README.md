# C/C++ equation linked against the installed engine (W5)

This is the successor to the old workflow of compiling an equation and linking
the loose `$AUTO_DIR/lib/*.o` objects.  After **installing** AUTO, a C/C++ user
writes their equation, includes the installed header, and links against the
installed `libauto` — no object glob, no `$AUTO_DIR`.

[ab.c](ab.c) is the A→B reaction written in C (compare [../ab/ab.f90](../ab/ab.f90)).

## 1. Install the engine

```sh
cmake -S . -B build              # from the repo root
cmake --build build -j
cmake --install build --prefix /your/prefix
```

Installs `libauto.a` + `libauto_c.a`, the headers under `<prefix>/include/auto`,
a CMake package config, and `auto.pc`.

## 2a. Build against it — CMake (`find_package`)

```cmake
find_package(AUTO REQUIRED)
add_executable(ab ab.c)
target_link_libraries(ab AUTO::auto AUTO::auto_c)
set_target_properties(ab PROPERTIES LINKER_LANGUAGE Fortran)  # pull Fortran runtime
```
```sh
cmake -S . -B build -DCMAKE_PREFIX_PATH=/your/prefix
cmake --build build
```

## 2b. Build against it — pkg-config / plain Makefile

```sh
cc       $(pkg-config --cflags auto) -c ab.c -o ab.o
gfortran ab.o $(pkg-config --libs auto) -fopenmp -o ab.exe
```

(`gfortran` links because the engine is Fortran and `PROGRAM AUTO` is the entry
point — the same reason `cmds.make` links C equations with `$(FC)`.)

## 3. Run

```sh
cp ../ab/c.ab fort.2
./ab                 # writes fort.7/8/9 (b/s/d files)
```

Verified against the compiled Fortran demo by
[test/test_clink.sh](../../test/test_clink.sh).

## Fortran vs C equations

- **Fortran** equation (defines `SUBROUTINE FUNC/STPNT/...`): link `-lauto` only.
- **C** equation (this example; the header defines the `user` table read by the
  dispatchers): link `-lauto -lauto_c`.

The shared `libauto.so` is intentionally **not** installed to the system libdir
(it would make `-lauto` ambiguous and break the C `user`-table resolution); it
ships inside the Python wheel instead (W4).
