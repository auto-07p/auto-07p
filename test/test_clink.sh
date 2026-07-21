#!/bin/sh
# Regression test for the installable C/C++ engine (refactor-plan.md W5, Goal 9).
#
# Installs the AUTO engine to a temporary prefix, then builds demos/clink (a C
# equation) against it via find_package(AUTO) -- with NO reference to
# $AUTO_DIR/lib/*.o -- runs it, and checks the b-file matches the compiled
# Fortran ab demo linked against the same installed engine.
#
# Skips cleanly (exit 0) if cmake or gfortran is unavailable.
set -e

ROOT=$(cd "$(dirname "$0")/.." && pwd)

command -v cmake    >/dev/null 2>&1 || { echo "SKIP: cmake not available";    exit 0; }
command -v gfortran >/dev/null 2>&1 || { echo "SKIP: gfortran not available"; exit 0; }

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
BUILD="$WORK/build"; PREFIX="$WORK/prefix"; DOWN="$WORK/down"; REF="$WORK/ref"
export OMPI_MCA_btl="^openib"

echo "== install the engine to a temp prefix =="
cmake -S "$ROOT" -B "$BUILD" -DBUILD_DEMOS=OFF -DCMAKE_INSTALL_PREFIX="$PREFIX" >/dev/null
cmake --build "$BUILD" -j >/dev/null
cmake --install "$BUILD" >/dev/null
for f in lib/libauto.a lib/libauto_c.a include/auto/auto_f2c.h lib/cmake/AUTO/AUTOConfig.cmake; do
    test -f "$PREFIX/$f" || { echo "FAIL: $f not installed"; exit 1; }
done

echo "== build the C equation downstream via find_package(AUTO) =="
mkdir -p "$DOWN"
cp "$ROOT/demos/clink/ab.c" "$ROOT/demos/clink/CMakeLists.txt" "$DOWN"/
cmake -S "$DOWN" -B "$DOWN/build" -DCMAKE_PREFIX_PATH="$PREFIX" >/dev/null
cmake --build "$DOWN/build" >/dev/null
test -x "$DOWN/build/ab" || { echo "FAIL: downstream exe not built"; exit 1; }
cp "$ROOT/demos/ab/c.ab" "$DOWN/build/fort.2"
( cd "$DOWN/build" && ./ab >/dev/null 2>&1 )

echo "== compiled-Fortran reference (ab.f90 + installed -lauto) =="
mkdir -p "$REF"
cp "$ROOT/demos/ab/ab.f90" "$ROOT/demos/ab/c.ab" "$REF"/
gfortran -O -fopenmp -I"$ROOT/include" -o "$REF/ab.exe" "$REF/ab.f90" \
    -L"$PREFIX/lib" -lauto
( cd "$REF" && cp c.ab fort.2 && ./ab.exe >/dev/null 2>&1 )

if diff -q "$DOWN/build/fort.7" "$REF/fort.7" >/dev/null; then
    echo "PASS clink  C equation via installed AUTO::auto == compiled Fortran (b-file byte-identical)"
    exit 0
fi
echo "FAIL clink  b-files differ"
exit 1
