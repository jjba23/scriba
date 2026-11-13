#!/usr/bin/env bash
# Reproduce the cross-language logging comparison in docs/comparisons.org.
#
#   guix shell -m manifest.scm -- ./benchmarks/compare/run.sh
#
# Scriba needs Guile with guile-json on the load path, which the project's own
# shell provides. The other two runtimes are set up on demand: loguru in a venv
# under $TMPDIR and pino in a scratch prefix, so nothing lands in the repository
# except the numbers on stdout. A runtime that cannot be set up (no pip, no npm,
# no network) is reported as skipped and the rest still runs.
set -uo pipefail

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
scratch=${TMPDIR:-/tmp}/scriba-bench

echo "impl	workload	calls	seconds	ns/op"

if command -v guile >/dev/null; then
  guile -L "$root/src" "$here/scriba-bench.scm" || echo "# scriba: failed" >&2
else
  echo "# scriba: skipped, no guile on PATH" >&2
fi

if command -v python3 >/dev/null; then
  python3 "$here/python-bench.py" || echo "# python logging: failed" >&2
  venv=$scratch/venv
  if [ ! -x "$venv/bin/python" ]; then
    python3 -m venv "$venv" >/dev/null 2>&1 &&
      "$venv/bin/pip" install --quiet loguru >/dev/null 2>&1
  fi
  if [ -x "$venv/bin/python" ]; then
    "$venv/bin/python" "$here/python-bench.py" loguru ||
      echo "# loguru: failed" >&2
  else
    echo "# loguru: skipped, could not create a venv with loguru" >&2
  fi
else
  echo "# python: skipped, no python3 on PATH" >&2
fi

if command -v node >/dev/null; then
  prefix=$scratch/node
  if [ ! -d "$prefix/node_modules/pino" ]; then
    mkdir -p "$prefix" &&
      npm install --silent --prefix "$prefix" pino >/dev/null 2>&1
  fi
  if [ -d "$prefix/node_modules/pino" ]; then
    NODE_PATH=$prefix/node_modules node "$here/node-bench.cjs" ||
      echo "# pino: failed" >&2
  else
    echo "# pino: skipped, could not install pino" >&2
  fi
else
  echo "# node: skipped, no node on PATH" >&2
fi
