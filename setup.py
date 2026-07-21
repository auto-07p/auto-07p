"""setuptools shim (W4 step 6).

All packaging metadata lives in pyproject.toml; this file exists only to mark
the wheel as *platform-specific*, because it bundles the compiled engine
(python/auto/libauto.so).  Without this, setuptools would tag the wheel
``py3-none-any`` -- wrong for a wheel carrying a native shared library.

A future scikit-build-core migration (refactor-plan.md P2) would build the .so
during ``pip wheel`` and make this shim unnecessary.
"""
from setuptools import setup
from setuptools.dist import Distribution


class BinaryDistribution(Distribution):
    def has_ext_modules(self):        # force platlib -> platform wheel tag
        return True


setup(distclass=BinaryDistribution)
