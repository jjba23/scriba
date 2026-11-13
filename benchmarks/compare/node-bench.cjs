#!/usr/bin/env node
// The Node.js half of the cross-language comparison in docs/comparisons.org.
//
// Writes pino lines to /dev/null with the same message, the same call counts and
// the same three-runs-then-median protocol as the Scriba and Python programs,
// so the printed ns/op figures are read side by side. CommonJS, because pino is
// installed into a scratch prefix and NODE_PATH is honoured by require().

const pino = require("pino");

const logger = pino({ level: "info" }, pino.destination("/dev/null"));

function report(name, calls, thunk, runs = 3) {
  const seconds = [];
  for (let run = 0; run < runs; run++) {
    const start = process.hrtime.bigint();
    for (let call = 0; call < calls; call++) thunk();
    seconds.push(Number(process.hrtime.bigint() - start) / 1e9);
  }
  seconds.sort((a, b) => a - b);
  const median = seconds[Math.floor(runs / 2)];
  console.log(
    `node-pino\t${name}\t${calls}\t${median.toFixed(3)}\t${(
      (median * 1e9) /
      calls
    ).toFixed(1)}`
  );
}

report("text literal", 200000, () => logger.info("user 42 logged in"));
report("text 2 args", 100000, () =>
  logger.info("user %d took %d ms", 42, 7)
);
report("muted debug", 4000000, () => logger.debug("user %d took %d ms", 42, 7));
