#!/bin/sh
# One-command local reproduction of the CI demo-suite test.
# See doc/baseline.md for the recorded baseline this reproduces.
#
# Usage:
#   test/run_ci.sh [selec|all]      # selec = fast 10-demo subset (default), all = full suite
#   PYTHON=/path/to/python test/run_ci.sh all
#
# Exit status: 0 if the run matches the accepted baseline (no unexpected "Major"
# errors and no unexpected Python tracebacks); non-zero otherwise. "Minor" numerical
# drift is accepted (see doc/baseline.md). Known-environmental majors are allowlisted.

set -eu

AUTO_DIR=$(cd "$(dirname "$0")/.." && pwd)
export AUTO_DIR
export OMPI_MCA_btl="^openib"

SUITE="${1:-selec}"
case "$SUITE" in
    selec) SCRIPT=selec.auto ;;
    all)   SCRIPT=all.auto ;;
    *) echo "usage: $0 [selec|all]" >&2; exit 2 ;;
esac

# Pick a Python that has numpy: honor $PYTHON, else the project venv, else python3.
if [ -z "${PYTHON:-}" ]; then
    if [ -x "$AUTO_DIR/PyAuto/.venv310/bin/python3" ]; then
        PYTHON="$AUTO_DIR/PyAuto/.venv310/bin/python3"
    else
        PYTHON=python3
    fi
fi
if ! "$PYTHON" -c "import numpy" >/dev/null 2>&1; then
    echo "ERROR: '$PYTHON' cannot import numpy. Set PYTHON=... to a numpy-enabled interpreter." >&2
    exit 2
fi

# Majors known to be environmental (not code regressions) — see doc/baseline.md.
#   cusp: requires matplotlib (plot object is None without it)
ALLOWED_MAJOR="cusp"

echo "AUTO_DIR = $AUTO_DIR"
echo "PYTHON   = $PYTHON ($("$PYTHON" --version 2>&1))"
echo "SUITE    = $SUITE ($SCRIPT)"
echo "------------------------------------------------------------"

LOG=$(mktemp)
trap 'rm -f "$LOG"' EXIT

cd "$AUTO_DIR/test"
rm -f ./*_log07p 2>/dev/null || true
"$PYTHON" ../python/auto "$SCRIPT" >"$LOG" 2>&1 || true

echo "=== summary ==="
grep -E "No errors found in:|Minor errors found in:|Major errors found in:" "$LOG" || true

status=0

# Unexpected Python tracebacks (the two known ones are matplotlib-driven 'config' on None).
tb=$(grep -c "Traceback (most recent call last):" "$LOG" || true)
env_tb=$(grep -c "AttributeError: 'NoneType' object has no attribute 'config'" "$LOG" || true)
if [ "$tb" -gt "$env_tb" ]; then
    echo "FAIL: $tb tracebacks found ($env_tb are known matplotlib env issues); unexpected ones present." >&2
    grep -A3 "Traceback (most recent call last):" "$LOG" | tail -40 >&2
    status=1
fi

# Major errors beyond the allowlist.
majors=$(sed -n 's/^Major errors found in: //p' "$LOG" | tr ' ' '\n' | grep -v '^$' || true)
for d in $majors; do
    case " $ALLOWED_MAJOR " in
        *" $d "*) : ;;                       # allowlisted environmental major
        *) echo "FAIL: unexpected major error in demo '$d'." >&2; status=1 ;;
    esac
done

if [ "$status" -eq 0 ]; then
    echo "OK: run matches accepted baseline (minor drift + allowlisted env majors only)."
fi
exit "$status"
