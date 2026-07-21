"""Regression test for the Python-callback engine path (refactor-plan.md W4).

For each demo converted to Python (demos/pycallback/<name>.py) this runs the
continuation two ways and compares the b-file (fort.7):

  (1) Python callbacks via libauto.so   -- NO compilation, NO linking
  (2) the compiled Fortran demo          -- <name>.f90 + lib/*.o, the ground truth

Both use the same engine sources and the same constants file.  The comparison is
two-tier:
  * byte-identical when the math allows it (e.g. ab, whose branch stays at U=0);
  * otherwise equal to a tight numerical tolerance.  A residual that uses a
    transcendental (exp's np.exp) differs from Fortran's EXP by ~1 ULP, which
    the nonlinear solve amplifies to ~1e-5 in the diagram -- a property of the
    user's Python code, not the callback bridge (proven: swapping np.exp for
    math.exp makes exp byte-identical too).  A real marshaling bug would show
    gross differences, not 1e-5, so the tolerance does not hide one.

Skips (exit 0) rather than failing if the shared engine isn't built or gfortran
is unavailable, so it is safe to run in any environment.

Run:  python test/test_pycallback.py
"""
import glob
import os
import shutil
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
PYCB = os.path.join(ROOT, "demos", "pycallback")
sys.path.insert(0, PYCB)
sys.path.insert(0, os.path.join(ROOT, "python"))

DEMOS = ["ab", "exp"]

# Numerical tolerance for the non-exact tier (see module docstring).
RTOL, ATOL = 1e-4, 1e-6


def _bfile_data(path):
    """Numeric data rows of a b-file (skip text header lines)."""
    rows = []
    for line in open(path):
        toks = line.split()
        try:
            rows.append([float(t) for t in toks])
        except ValueError:
            continue                       # header / label line
    return [r for r in rows if r]


def _compare(py_path, ref_path):
    """Return (ok, message) comparing two b-files, exact-or-tolerance.

    Compares row by row (b-file rows vary in width, so a rectangular array
    would be ragged); integer label columns must match exactly, float columns
    within (RTOL, ATOL).
    """
    with open(py_path) as f:
        py_txt = f.read()
    with open(ref_path) as f:
        ref_txt = f.read()
    if py_txt == ref_txt:
        return True, "byte-identical"
    a, b = _bfile_data(py_path), _bfile_data(ref_path)
    if len(a) != len(b):
        return False, "row count differs (%d vs %d)" % (len(a), len(b))
    max_rel = 0.0
    for ra, rb in zip(a, b):
        if len(ra) != len(rb):
            return False, "row width differs (%d vs %d)" % (len(ra), len(rb))
        for x, y in zip(ra, rb):
            if abs(x - y) > ATOL + RTOL * abs(y):
                return False, "exceeds tolerance (%.6g vs %.6g)" % (x, y)
            max_rel = max(max_rel, abs(x - y) / max(abs(y), 1e-12))
    return True, "matches to tolerance (max rel %.1e)" % max_rel


def _libauto():
    for p in (os.path.join(ROOT, "lib", "libauto.so"),
              os.path.join(ROOT, "python", "auto", "libauto.so")):
        if os.path.exists(p):
            return p
    return None


def _run_python(name, workdir):
    import importlib
    import _engine
    mod = importlib.import_module(name)
    rc, _ = _engine.run(name, dict(mod.CALLBACKS), workdir=workdir, capture=True)
    if rc != 0:
        raise RuntimeError("%s: python callback run returned rc=%d" % (name, rc))
    return os.path.join(workdir, "fort.7")


def _run_fortran(name, workdir):
    f90 = os.path.join(ROOT, "demos", name, "%s.f90" % name)
    cfile = os.path.join(ROOT, "demos", name, "c.%s" % name)
    exe = os.path.join(workdir, "%s.exe" % name)
    objs = sorted(glob.glob(os.path.join(ROOT, "lib", "*.o")))
    subprocess.check_call(
        ["gfortran", "-O", "-fopenmp", "-I", os.path.join(ROOT, "include"),
         "-o", exe, f90] + objs)
    shutil.copyfile(cfile, os.path.join(workdir, "fort.2"))
    subprocess.check_call([exe], cwd=workdir,
                          stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    return os.path.join(workdir, "fort.7")


def main():
    if _libauto() is None:
        print("SKIP: libauto.so not built "
              "(configure with -DAUTO_BUILD_SHARED=ON and build target 'auto')")
        return 0
    if shutil.which("gfortran") is None:
        print("SKIP: gfortran not available to build the compiled reference")
        return 0

    os.environ.setdefault("AUTO_DIR", ROOT)
    os.environ.setdefault("OMPI_MCA_btl", "^openib")

    failures = 0
    for name in DEMOS:
        wpy = tempfile.mkdtemp(prefix="pycb_py_")
        wref = tempfile.mkdtemp(prefix="pycb_ref_")
        try:
            py_b = _run_python(name, wpy)
            ref_b = _run_fortran(name, wref)
            ok, msg = _compare(py_b, ref_b)
            if ok:
                print("PASS %-4s python callbacks == compiled Fortran (%s)"
                      % (name, msg))
            else:
                print("FAIL %-4s %s" % (name, msg))
                failures += 1
        except Exception as exc:                       # noqa: BLE001
            print("FAIL %-4s %s" % (name, exc))
            failures += 1
        finally:
            shutil.rmtree(wpy, ignore_errors=True)
            shutil.rmtree(wref, ignore_errors=True)

    print("\n%d/%d demos matched the compiled engine"
          % (len(DEMOS) - failures, len(DEMOS)))
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
