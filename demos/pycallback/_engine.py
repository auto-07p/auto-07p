"""Shared runner for the Python-callback demos (refactor-plan.md W4).

Each demo in this directory defines its equations as plain Python functions and
a CALLBACKS dict; this helper stages the constants file, registers the
callbacks into the compiled engine (libauto.so) at runtime, and runs one
continuation -- with no Fortran compilation or linking.
"""
import os
import shutil
import sys

_HERE = os.path.dirname(os.path.abspath(__file__))
_ROOT = os.path.dirname(os.path.dirname(_HERE))          # repo root


def _ensure_auto_on_path():
    try:
        import auto  # noqa: F401
    except ImportError:
        sys.path.insert(0, os.path.join(_ROOT, "python"))


def demo_constants(name):
    """Path to the sibling Fortran demo's constants file (demos/<name>/c.<name>).

    Reusing the exact same constants the compiled demo uses keeps the two paths
    comparable (see test/test_pycallback.py).
    """
    return os.path.join(_ROOT, "demos", name, "c.%s" % name)


def run(name, callbacks, constants=None, workdir=None, capture=True):
    """Run one continuation of `name` from Python callbacks.

    Stages the constants file as fort.2 in `workdir` (cwd by default), registers
    `callbacks` (a no-op pvls is added if absent, since the engine always calls
    it), runs, and returns (rc, captured_output_text).
    """
    _ensure_auto_on_path()
    from auto import callback as cb

    callbacks = dict(callbacks)
    callbacks.setdefault("pvls", lambda u, par: None)
    constants = constants or demo_constants(name)
    workdir = workdir or os.getcwd()
    shutil.copyfile(constants, os.path.join(workdir, "fort.2"))

    reg = cb.Registry(cb.load_engine())
    reg.register(**callbacks)

    prev = os.getcwd()
    os.chdir(workdir)
    try:
        if capture:
            with cb.capture_output() as out:
                rc = reg.run()
            return rc, out.text
        return reg.run(), ""
    finally:
        os.chdir(prev)
