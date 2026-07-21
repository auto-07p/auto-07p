# AUTO-07p Restructuring Plan

## Context
- **Repo**: `a07p.git`, an ODE continuation/bifurcation package forked from auto-07p. Core engine is Fortran 77/90; utilities are Python, C/C++, and sh scripts.
- **Build**: core Fortran/C/C++ builds with GNU autotools/make (authoritative). **CMake is an in-progress, unfinished change** (`MyCMakeLists.txt`, `src/My_CMakeLists.txt`) — it *attempts* the core engine (`add_library(auto_f …)`) plus options for demos/GUI/wheel, but diverges from autotools (see W0) and does not yet reproduce the `lib/` layout the CLI needs.
- **Docs / how to build & run**: `doc/` — `doc/auto.tex` (user manual: install, build, run, the `lib/*.o` link workflow), `doc/tutorial.tex`, `doc/user_guide.tex`, `doc/README`, `doc/update_plan.md` (existing Fortran-callable design), and `doc/python-callback-api.md` (W4 runtime user-function registration API). Consult these before changing build/run behavior.
- **Test**: no unit tests. Coverage is:
  - `demos/` — integration tests (per-demo Fortran + expected output), the primary safety net.
  - `test/` — Python CLI tests (`test/test.py`, `test/parse_test.py`, `*.auto` scripts).
  - CI: `.github/workflows/test.yml` (compile + run) and `pylint.yml` (lint gate, `.pylintrc`).
- **Components**:
  - core engine — `src/`, `util/`, `include/`
  - python CLI — `python/`, `test/`
  - plot utilities — `plaut/`, `plaut04/`, `gui/`
  - integration tests — `demos/`
  - legacy `@`-command CLI (sh scripts) — `cmds/` (78 files, of which 70 are `@`-scripts; the rest are `cmds.make`, `auto.env`, etc.)
- **Work already in flight** (fold in, don't restart):
  - CMake: `MyCMakeLists.txt`, `src/My_CMakeLists.txt` (rename to canonical `CMakeLists.txt` once trusted).
  - Packaging: `setup.py` + `setup.cfg` (name `auto_07p`), `python/auto_07p.egg-info/`.
  - Python-callable engine: the Fortran `auto_main`/`auto_main_c` design in [doc/update_plan.md](doc/update_plan.md) and the runtime callback API in [doc/python-callback-api.md](doc/python-callback-api.md). (Note: `PyAuto/` is unfinished scratch work, git-ignored — do not treat it as a source of truth; W4 starts fresh from the design docs.)

## Goals (ranked)
1. Keep the core engine in place; build it cleanly with **both** CMake and autotools/make. One build produces the engine libraries **and all demos in one shot** (see W6).
2. Rewrite the Python CLI as a proper installable **package** (managed by a package manager, buildable into a wheel).
3. **Obsolete** the legacy `cmds/` `@`-scripts.
4. **New capability**: wrap the core engine as a Python-callable library so users write Python and drive AUTO from a script or Jupyter notebook.
5. keep plaut and plaut04 as independ utility apps.
6. **Migrate the Python codebase from Python 2 to Python 3** (drop Python 2). Prerequisite for a modern package/wheel (Goal 2).
7. **The Python package performs NO runtime compilation** — it never shells out to `make`/`cmake`/`gcc`/`gfortran`. User equations (`func`/`stpnt`/`bcnd`/`icnd`/`fopt`/`pvls`) are supplied as **Python functions**, registered into the engine's existing C function-pointer table ([auto_f2c.h](include/auto_f2c.h) `user_function_list`) via runtime callbacks. This replaces today's compile-and-link model entirely for the Python path.
8. **Distribute as a wheel** installable in any Python environment; the wheel bundles the pre-compiled engine as a binary extension. User `pip install`s it, writes Python functions, and runs the analysis — no toolchain required.
9. **The core engine is separately installable** as a system library (`libauto` shared + static, public headers, a CMake package config / pkg-config). C/C++ users write their own equation code, compile it, and link against the *installed* engine — the clean successor to today's fragile `lib/*.o` glob.

## Target architecture (end-state)
One CMake build → three outputs, from the same engine sources:
1. **Python wheel** (Goals 4, 7, 8) — pre-compiled engine + Python callback layer. Zero runtime compilation. User defines equations in Python.
2. **Installable core engine** (Goal 9) — `libauto` + headers + package config, for C/C++ users who compile & link their own code.
3. **CLI + demos** (Goals 1–3) — the existing workflow, kept working through the transition.

Trajectory: today's runtime-compilation machinery (`cmds/cmds.make`, `runAUTO.__make`, the `lib/*.o` glob, `make -e …exe`) is **fully retired in the end-state**. W0 preserves it for parity; W4 replaces the Python path with callbacks; the C/C++ path migrates from the `lib/*.o` glob to linking installed `libauto`.

## Scope split — two regimes, two rule-sets
The original single set of invariants ("public API unchanged, no new behavior") is self-contradictory: Goals 4/7/8/9 *are* new behavior. Split the work:

- **Regime A — behavior-preserving** (Goals 1, 2, 3, 5, 6): restructuring, repackaging, deprecating, Python 2→3. Strict invariants below apply — same inputs produce same outputs.
- **Regime B — additive capability** (Goals 4, 7, 8, 9): net-new Python-callable engine, no-compile wheel, and installable C/C++ engine. Must not change existing behavior, but *is* new surface area, developed behind a feature flag / separate targets with their own tests. Strict "no new behavior" does not apply to the new APIs.

## Invariants (Regime A)
- Public CLI behavior and file formats unchanged (b-files, s-files, d-files, constants files).
- The `demos/` + `test/` suites stay green — this is the definition of "no regression."
- One logical change per commit, each independently green in CI.
- No new runtime dependency added to Regime A without an explicit note in the step.

## Prerequisite decisions (settle before the workstreams they gate)
P1 and P2 are now decided (below); P3 and P4 are still open:
- **P1 — Python 2 vs 3.** *Decided (now Goal 6): drop Python 2.* CI (`test.yml`) still provisions Python 2.7 and the code has py2-isms (`print`, etc.); a modern wheel requires Python 3. Migration is Workstream W1. Without it, "build a wheel" is blocked.
- **P2 — Package/build tooling.** *Leaning decided:* the wheel ships a compiled engine extension (Goals 7, 8), so use a **CMake-aware build backend (`scikit-build-core`)** with `pyproject.toml` (PEP 621) — one CMake build drives the engine, the wheel extension, and demos (Goal 1/9). Confirm `scikit-build-core` before W2 packaging work.
- **P3 — Package manager.** Which one (uv / poetry / pip+build)? Pick one and standardize contributor + CI flow around it.
- **P4 — `cmds/` consumers.** Audit who still calls the `@`-scripts (demos? docs? external users?) before deprecating. Determines whether Goal 3 needs a compatibility shim.

## Safety net first (do before any refactor)
The top risk is refactoring against a suite that isn't reliably green. This is the principle; it is *operationalized* by **W0 step 1** (autotools build baseline) and **W1 steps 1–2** (test/CI baseline + local repro) — not a separate fourth workstream. Establish the baseline:
1. Make `test.yml` pass on a clean checkout on the target Python (per P1). Record the green SHA as the baseline. *(= W1 step 1.)*
2. Write a one-command local repro of CI (script under `test/` or a `make check` target) so every commit can be verified locally before push. *(= W1 step 2.)*
3. If demo verification is loose (diff tolerances, missing expected outputs), tighten it *before* refactoring — otherwise regressions pass silently.

## Workstreams & ordered steps
Do roughly in order. **W0 (CMake parity) comes first** per the plan owner's decision — it is the foundation the rest builds on. W1 (baseline + Python 3) runs alongside and gates W2. W0 gates the shared-library/installable-engine work (W4, W5). W3's final deletion is gated by **both** W4 and W5. W6 (one-shot build) folds into W0/W1.

> Note: W0 still assumes the **Safety net first** section above is in place — you cannot verify "CMake matches autotools" without a green demo suite to compare against. Establishing the baseline is step 1 of W0.

### W0 — Make CMake match autotools, then consolidate (Goal 1) — FIRST
Goal: the CMake build must produce the **same libraries and same behavior** as the autotools build before anything else changes. **Guiding rule: `cmds.make` + the original Python are authoritative; wherever CMake diverges, fix CMake — not the CLI.** Only once they match, refactor the build to combine related code into cohesive library targets / Fortran modules.

**Decided topology (audited against [cmds/cmds.make](cmds/cmds.make)):**
| Question | Answer (matches make) | CMake fix needed |
|---|---|---|
| Is `fcon.f` part of the engine lib? | **No.** It's a standalone `PROGRAM AUTCON` ([src/fcon.f:2](src/fcon.f#L2)), compiled from source on demand into its own `fcon` executable. | It's currently compiled **into** `auto_f` — remove it; build a separate `fcon` target (or keep source-compiled per cmds.make). Folding a second `PROGRAM` into the archive is a bug. |
| Separate `auto_c` archive? | **Yes.** C user code links `-lauto_c` (`libauto_c.a` from `user_c.f90`). | `auto_c` is commented out and `user_c.f90` folded into `auto_f` — restore the separate `auto_c` library. |
| What does the CLI actually link? | `LIBS = $(AUTO_DIR)/lib/*.o` — the user-equation exe links **every loose `.o` in `lib/`**, plus `-lauto_c` for C users. | A single `.a` doesn't reproduce the `lib/*.o` layout the CLI globs — CMake must populate `lib/` with the objects/archive the CLI expects. |

**Confirmed `lib/` contract** — the CLI depends on this at runtime via *two* code paths ([runAUTO.py:200](python/auto/runAUTO.py#L200) demo `make`, and [runAUTO.py:314-334](python/auto/runAUTO.py#L314) native `__make` which `glob.glob`s `lib/*.o`) and it's the documented manual build ([doc/auto.tex:1121](doc/auto.tex#L1121)). Whatever CMake does, `$(AUTO_DIR)/lib/` must end up containing:
- **Loose `.o` for every engine source** (`ae, auto_constants, blas, bvp, compat(f2003), equilibrium, floquet, homcont, interfaces, io, lapack, main, maps, mesh, mpi(nompi), optimization, parabolic, periodic, solvebv, support, timeint, toolboxae, toolboxbv`). These are globbed and linked wholesale — including `main.o` (`PROGRAM AUTO`).
- **`libauto_c.a`** (from `user_c.f90`), linked only via `-lauto_c`. `user_c.o` must **not** be a loose object in `lib/`.
- **No `fcon`/`user_c` `PROGRAM` in the glob set** — a second `PROGRAM` among `lib/*.o` breaks every user-equation link.
- `libauto_f.a` may also be shipped for other consumers, but the CLI paths use the loose `.o`, not this archive.
> CMake gotcha: CMake emits archives/shared libs by default, not a stable directory of loose `.o`. Reproducing this needs an OBJECT library plus an install/copy step that deposits the objects into `lib/`. This is the most likely reason the in-progress CMake build doesn't yet drive the CLI.

1. Establish the build baseline: demos green under **autotools** on a clean checkout; record the SHA. This is the reference CMake must reproduce.
2. Make CMake match autotools exactly, applying the three fixes above: separate `fcon` executable, restored `auto_c` archive, and a `lib/` layout the CLI's link step resolves against. Confirm demos pass identically under CMake.
3. **Then consolidate (where it doesn't change behavior):** group related Fortran sources into coherent library targets / modules (e.g. engine core vs. C-interface wrapper `auto_c` vs. vendored BLAS/LAPACK math), so the build expresses real components rather than one flat list. Fold duplicated build logic (`My*`, `*WorkCopy`, `.txt_` scratch copies) into single canonical files. Any consolidation that changes what the CLI links must be matched by the CLI change in the same commit.
4. Promote `MyCMakeLists.txt` → `CMakeLists.txt`, `src/My_CMakeLists.txt` → `src/CMakeLists.txt`; delete the scratch copies. (Note: `src/Makefile` is generated/git-ignored and currently stale — it references the removed `auto_entry.f90`; only `Makefile.in` is source-of-truth. `PROGRAM AUTO` now lives in `main.f90`.)
5. Add a CMake build lane to `test.yml` alongside the make lane; keep autotools working until CMake has parity in CI (don't remove yet).

### W1 — Baseline safety net & Python 2→3 (Goals 6, and baseline for all)
1. Get `test.yml` + `pylint.yml` green on a clean checkout → tag baseline SHA.
2. Write a one-command local repro of CI so every commit is verifiable before push.
3. Run `2to3`/`pyupgrade` incrementally on `python/auto/`; one module per commit, suite green each time.
4. Drop Python 2 from the CI matrix; update docs.

### W2 — Python CLI as a package (Goal 2) — needs W1
1. Migrate `setup.cfg` metadata → `pyproject.toml` (backend per P2); remove py2 remnants.
2. Make the tree import cleanly as an installed package (fix implicit relative imports, `__main__`, entry points / console_scripts to replace `@`-command usage).
3. `pip install -e .` works; `test/` runs against the installed package, not `$AUTO_DIR`-relative paths.
4. Produce a wheel in CI as an artifact.

### W3 — Obsolete `cmds/` (Goal 3) — final deletion gated by W4+W5
**Status (2026-07-20): DONE.** The CLI's compile/link was migrated off
`cmds.make` and the `lib/*.o` glob: it now reads compiler flags from a
build-generated `python/auto/compilers.py` (autotools `src/Makefile` rule +
CMake `configure_file`) and links the installable engine archive
`lib/libauto.a` (+ `lib/libauto_c.a` for C equations) by explicit path. `cmds/`
(the 70 `@`-scripts, `cmds.make`, `auto.env`) was deleted; `configure.ac` /
`configure` / `Makefile.in` no longer reference it; the `@`→Python mapping and
`auto.env`→`pip install` guidance moved to
[doc/migrating-from-cmds.md](doc/migrating-from-cmds.md). This also closes
**W5 step 4**. Follow-up (docs): `doc/auto.tex` still shows the old `@`-syntax /
`auto.env` setup — a manual pass, not blocking.
**Audit result:** the `demos/`/`test/` suites do **not** execute `@`-scripts (README/`autorc` mentions are documentation of old→new Python equivalents). The 70 `@`-scripts are thin shell wrappers with documented Python API equivalents — low-risk to shim/remove. The real couplings are two:
- [interactiveBindings.py:29](python/auto/interactiveBindings.py#L29) enumerates `$AUTO_DIR/cmds` at runtime to generate the `@`-aliases.
- [runAUTO.py:266](python/auto/runAUTO.py#L266) / [AUTOCommands.py:192](python/auto/AUTOCommands.py#L192) read `cmds/cmds.make` — the recipe that compiles user code. This is what **W4 (Python path) and W5 (C/C++ path)** replace, so removing `cmds.make` is gated by **both**.

1. Deprecate/shim the 70 `@`-scripts (forward to the package entry point; print a one-line "use `auto ...`"). Safe now.
2. Stop `interactiveBindings` from enumerating `cmds/`; register commands from the package instead.
3. Keep `cmds.make` until W4 (Python callbacks) *and* W5 (installable engine) both land; then retire it.
4. Migrate docs (READMEs) off `@`-syntax; update `Makefile.in` install rules (installs `cmds/@*`, `cmds.make`).
5. Delete `cmds/` once no consumer remains.

### W4 — No-compile Python-callable engine (Goals 4, 7, 8, Regime B — needs W0)
**Foundation (audited):** the engine already dispatches user functions through a C function-pointer table — [auto_f2c.h](include/auto_f2c.h) `user_function_list { func, stpnt, bcnd, icnd, fopt, pvls, uses_fortran }`, called from Fortran via `c_f_procpointer` ([user_c.f90:111](src/user_c.f90#L111)). Today that table is filled at link time by compiled C. The Python path fills it **at runtime from Python** — no `gcc`/`make`.
**Concrete API drafted in [doc/python-callback-api.md](doc/python-callback-api.md)** — exact `auto_set_user`/`auto_reset_user` signatures, the Fortran setter, the ctypes `CFUNCTYPE` prototypes, the NumPy marshaling adapter, and the hazard list. Follow that doc for steps 2–3.
1. Land the `auto_main` / `auto_main_c` refactor from [doc/update_plan.md](doc/update_plan.md): extract `PROGRAM AUTO` body into a subroutine; `PROGRAM AUTO` just calls it. Behavior-preserving — demos must stay green.
2. Make the `user` table **settable at runtime**: add `auto_set_user(const user_function_list*)` / `auto_reset_user()`; make libauto own `user` (mutable) for the Python build while keeping the `extern const user` path for compiled-C users (Goal 9). Build a PIC shared library exporting `auto_main_c` + the setters. (See doc §2.)
3. **Python callback layer**: ctypes `CFUNCTYPE` prototypes matching the ABI, a Pythonic adapter that views the raw `double*` args as NumPy arrays **without copying**, writes results back, and keeps callback objects alive against GC. Jacobians (`dfdu`/`dfdp`/`dbc`/`dint`) are **Fortran column-major**; `par` is sized by `NPARX`, not `ndim`; honor `ijac` (analytic vs. finite-difference). (See doc §3–4.)
4. **Spike (R&D, highest uncertainty): reentrancy.** COMMON blocks / module globals + `stop`/`exit`. Prove a clean re-init path before promising repeated in-process runs; convert `stop`/`exit` to return codes so a Python/Jupyter host survives. Timebox; fallback is subprocess-per-run.
5. Redirect Fortran stdout/stderr sensibly for notebook use.
6. Ship the shared lib inside the wheel (P2 = `scikit-build-core`). Result: `pip install`, write Python functions, run — zero runtime compilation.

### W5 — Installable core engine for C/C++ users (Goal 9, Regime B — needs W0)
Successor to the `lib/*.o` glob: a real installable library C/C++ users link against.
1. Add `install(TARGETS …)` for `libauto` (shared + static) and `install(FILES …)` for public headers ([auto_f2c.h](include/auto_f2c.h), `fcon.h`, etc.).
2. Export a CMake package config (`AUTOConfig.cmake`) and/or pkg-config so downstream projects `find_package(AUTO)` / `pkg-config --libs auto`.
3. Provide a documented example: user writes `equation.c` implementing `user_function_list`, compiles, links `-lauto`, runs — **without** the `$AUTO_DIR/lib/*.o` glob.
4. Once W4 (Python) and W5 (C/C++) both work, retire `cmds/cmds.make`, `runAUTO.__make`, and the `lib/*.o` layout (closes the W3 gate).

### W6 — One-shot build of engine + demos (Goal 1/9, folds into W0/W1)
1. Top-level CMake adds all `demos/` as build targets so a single `cmake --build` produces engine libs, the wheel extension, **and** every demo.
2. Wire the demo builds into `test.yml` so "build everything" and "run the suite" are one flow.

## Done when (per workstream)
- **W0**: CMake matches autotools (same `lib/` contract); demos pass under both; `src/CMakeLists.txt` canonical.
- **W1**: CI green on Python 3 only; Python 2 dropped; baseline SHA tagged.
- **W2**: `pip install auto_07p` gives a working CLI; wheel built in CI; `test/` passes against the installed package.
- **W3**: `cmds/` removed (or reduced to shims); no consumer references remain (gated by W4+W5).
- **W4**: a Python/Jupyter example defines equations as Python functions and runs a continuation with **no** `make`/`gcc` invoked; repeated in-process runs behave correctly (or the limitation is documented).
- **W5**: a C/C++ user builds and links against the **installed** `libauto` (no `lib/*.o` glob) and runs an analysis.
- **W6**: one `cmake --build` produces engine + wheel + all demos.
- **Global**: pylint gate passes; diffs in Regime A are behavior-preserving on the demo suite.

## Risks
- **Reentrancy of the Fortran engine** (W4) — global/COMMON state + `stop`/`exit`; may not support repeated in-process calls without deep changes. Highest uncertainty; spike early, subprocess-per-run fallback.
- **Python-callback marshaling** (W4) — `double*` ↔ NumPy views, Fortran column-major Jacobians, `ijac`/finite-difference semantics, and per-evaluation callback overhead (many calls per continuation). Get a micro-benchmark early; offer a compiled/`numba` fast path later if needed.
- **Runtime `user` table is `extern const`** (W4) — making it settable at runtime is an ABI change to the equation interface; keep the compiled-C path working (Goal 9) alongside the new setter.
- **Python 2→3** (W1) — touches every module.
- **Silent demo-suite gaps** — if verification is loose, refactors regress undetected. Tighten first.
- **`cmds/` external users** — deleting `@`-scripts / `lib/*.o` glob may break downstream workflows; prefer shims + deprecation window; the glob only retires once W4+W5 land.
- **Two build systems drifting** — CMake diverged from autotools on `fcon.f` placement, the `auto_c` archive, and the `lib/*.o` layout. Topology is now **decided** (see W0 table); bend CMake back to match. Keep both at parity in CI until autotools is retired; treat the generated (git-ignored) `src/Makefile` as untrusted — regenerate from `Makefile.in`.
- **W3↔W4/W5 coupling** — `cmds.make` + `lib/*.o` can't be removed until *both* the Python callback path (W4) and the installable engine (W5) replace them.

## If something's off
- A commit turns the demo/CLI suite red → revert that commit; the tree must always return to the last green baseline SHA.
- CMake vs. make disagree on demo output → treat as a build regression, not a test-tolerance issue; fix the build, don't loosen the check.
- W4 reentrancy spike shows in-process re-runs aren't feasible → fall back to a subprocess-per-run callable API and document the constraint, rather than blocking the whole workstream.
- Scope creep → if a step isn't in a workstream above, it needs its own line and its own green commit before merging.
