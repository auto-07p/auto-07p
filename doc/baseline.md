# Test baseline — AUTO-07p refactor

The green reference the refactor must preserve (plan *Safety net first* / W0 step 1 / W1 step 1).
Update this file whenever the baseline is re-established on a new SHA or environment.

## Baseline SHA
- **`242db40`** (branch `pylint`) — "fix: restore missing AUTOatof import in parseB.py".
  - This commit fixes a regression from `a5c741e`: `AUTOatof` was deduplicated into
    `parseCommon.py` but never imported in `parseB.py`, breaking all b-file parsing and
    verification. HEAD before this fix (`d518df8`) was **not** green.

## Environment used
- OS: Linux (WSL2), gfortran 13.3.0, gcc/g++ 13.3.
- Python: 3.10.19 (`PyAuto/.venv310`), numpy 1.23.5. **matplotlib not installed.**
- Build: pre-existing autotools build (`./configure` + `make`); `lib/` has 24 objects +
  `libauto_f.a` + `libauto_c.a`; `bin/` has `auto`, `autox` only.

## How to reproduce
```sh
export AUTO_DIR=/home/puppy/a07p.git
export OMPI_MCA_btl="^openib"
cd test
<python-with-numpy> ../python/auto all.auto
# results land in test/verification/ ; summary printed at the end
```
(Runs ~40 demos / ~1065 individual continuations.)

## Result (2026-07-19, SHA 242db40, py3.10)
| Bucket | Count | Notes |
|---|---|---|
| ✅ Clean ("No errors found") | **56 demos** | ab abc abcb apbp brc brf bru bvp c2c chu dd2 enz exp ext ezp ffn fhn fnc frc fsh hen int ivp kdv lin log lor lrz man nag nep non obv opt p2c pd1 pd2 ph1 pla plp pp2 pp3 ppp pvl python python/n-body san she sib sspg stw tim tor um2 um3 wav |
| ⚠️ Minor drift | 17 demos | cir fhh fnb kar kpr lcbp mtn ops pcl pen phs r3b rev snh spb tfc vhb — numerical tolerance vs a reference (`hyper.all`) built on another compiler. Framework-classified "minor". **Accepted as caveat.** |
| ❌ Major | 1 (`cusp`) | **Environmental**: no matplotlib → plot object is `None` → `p.config()` raises. The continuation math ran fine. |
| ❌ Runtime sub-test | `double.exe` (in `ab` fullTest) | **Build gap**: `bin/double` was never built (only `auto`/`autox` present). Ties to W6 "build everything in one shot". |

## Accepted caveats (per owner decision)
1. The 17 minor-drift demos are treated as cross-compiler numerical noise, not regressions.
2. `cusp`/`plotter` failures are due to missing matplotlib in this env, not code.
3. `double.exe` / other utility binaries are not built by the current partial build.

To retire caveats 2–3, set up a CI-like environment (install matplotlib; full build of all
utilities) and re-baseline — do **not** loosen verification tolerances to hide them.

## Known follow-ups surfaced while baselining (not yet done)
- **W1 (py2→3):** `parseB.py` still mixes idioms — bare `map(...)` inside `N.array(map(...), 'd')`
  at [parseB.py:327,338](../python/auto/parseB.py#L327) (py2). Not hit by the passing demos, but
  fix during the py3 pass.
- **W2 (package):** `import auto` fails under py3 (`import pointtest7` implicit relative import in
  [parseBandS.py:141](../python/auto/parseBandS.py#L141)); the CLI only works run as a script dir
  (`python3 ../python/auto ...`), not as an installed package.
