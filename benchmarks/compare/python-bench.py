#!/usr/bin/env python3
"""The CPython half of the cross-language comparison in docs/comparisons.org.

python-bench.py runs the standard library's logging module and, when loguru is
importable, loguru as well; both write to /dev/null with the same message, the
same call counts and the same three-runs-then-median protocol as the Scriba and
Node programs, so the printed ns/op figures are read side by side.

    python3 benchmarks/compare/python-bench.py          # stdlib only
    /tmp/scriba-bench-venv/bin/python benchmarks/compare/python-bench.py   # + loguru
"""

import statistics
import sys
import time


def report(name, calls, thunk, runs=3):
    """Prints the median nanoseconds per call of THUNK over CALLS calls."""
    seconds = []
    for _ in range(runs):
        start = time.perf_counter()
        for _ in range(calls):
            thunk()
        seconds.append(time.perf_counter() - start)
    median = statistics.median(seconds)
    print(
        "python-%s\t%s\t%d\t%.3f\t%.1f"
        % (runner, name, calls, median, median * 1e9 / calls)
    )


def stdlib_logger():
    """A stdlib logger writing to /dev/null, level INFO."""
    import logging

    logger = logging.getLogger("bench")
    logger.propagate = False
    handler = logging.StreamHandler(open("/dev/null", "w"))
    handler.setFormatter(
        logging.Formatter("%(levelname)s %(asctime)s %(name)s %(message)s")
    )
    logger.addHandler(handler)
    logger.setLevel(logging.INFO)
    return logger


def loguru_logger():
    """A loguru logger writing to /dev/null, level INFO."""
    from loguru import logger

    logger.remove()
    logger.add("/dev/null", level="INFO")
    return logger


runner = "logging"
if len(sys.argv) > 1 and sys.argv[1] == "loguru":
    runner = "loguru"
    log = loguru_logger()
    # loguru formats with str.format, the stdlib with percent signs.
    args_message = "user {} took {} ms"
else:
    log = stdlib_logger()
    args_message = "user %s took %s ms"

info = log.info
debug = log.debug

report("text literal", 200000, lambda: info("user 42 logged in"))
report("text 2 args", 100000, lambda: info(args_message, 42, 7))
report("muted debug", 4000000, lambda: debug(args_message, 42, 7))
