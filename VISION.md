# Vision

Scriba is a structured logging framework for GNU Guile, built on one small
conviction: a logger is a value you construct, and a line you did not ask for
should cost nothing. It has five destinations, one API and no daemon. This
document says where that design should go next — which gaps are worth closing,
which are worth keeping open on purpose, and what a logging library owes a
program that runs for months.

## Where scriba is today

Nine modules under `src/scriba/` — the engine, the layout, the environment-driven
auto-logger and the exception reporter, plus five backends: `console`,
`color-console`, `json`, `syslog` and `file`. The runtime dependency list is
Guile and `guile-json`; there are no threads, no fibers and no daemons.

Four decisions carry the rest.

- **Filtering is compiled, not checked.** `make-filtered-logger` binds every
  level below the threshold to a no-op. A muted statement therefore costs a
  procedure call and nothing else — measured at 55 ns against CPython's 115 ns
  and loguru's 155 ns, and the benchmark that says so is in the tree.
- **A logger is a memoized value, not a registry entry.** The memo key is the
  open arguments, not a name, so a logger built for one port is a different
  logger from one built for another. That is what makes a parameterised port,
  level or layout safe, and it is why there is no per-package hierarchy: there
  is no name to hang one on.
- **Context is the language's own dynamic scope.** `with-log-context` and
  `with-log-tags` parameterize; they nest, they unwind with the body, and they
  need no adapter passed through every call.
- **A layout is a checked value.** `#:layout '(level time message context)` is a
  list of field names validated at construction against the fields that backend
  publishes, so a typo is an error rather than a silent blank column.

The public API settled at 1.0.0: the exports of the modules, the `SCRIBA_*`
variables and the layout field names. `docs/comparisons.org` states the limits
as plainly as the strengths: no OpenTelemetry exporter, no sampling, no rate
limiting, no asynchronous I/O, no hot reconfiguration, no level hierarchy, and
JSON writes slower than `pino`'s. `scriba-syslog-logger` formats an RFC 5424
line and writes it to a port; it does not open a socket. The exception reporter
costs a millisecond or two at `#:depth 8`, dominated by Guile reading the source
of a frame, which is why the depth is bounded.

## The vision

1. **A log statement survives the trip off the machine.** A syslog sink that
   speaks to `/dev/log`, a UDP collector or a TCP stream with framing and
   reconnect; and one shipper that writes OTLP-shaped JSON over HTTP. Both
   optional, neither a dependency, and both testable without a collector.
2. **A running service can be re-levelled without a restart.** Hot
   reconfiguration is today's deliberate omission, and the price of the no-op
   binding. The answer is not to check on every call: it is an indirection the
   muted path pays for once, or a supervisor that swaps the logger at a
   boundary, and the choice should be made by measurement rather than taste.
3. **Volume control is part of the library.** Sampling and rate limiting decided
   at construction, reported rather than silent: a line that says "and 4 209
   others like this were suppressed" is worth more than either all of them or
   none.
4. **Guile's whole ecosystem logs the same shape.** byggsteg, pingwing, kracht
   and their neighbours should produce lines that agree on the fields a
   collector groups by — request id, task id, duration, level — because they
   share one logger rather than four conventions that happen to be JSON.
5. **The cost of a line is a number the project can defend.** Every release keeps
   the benchmark in `docs/performance.org` current, including the muted path and
   the exception path, because a logging library that cannot state its overhead
   is asking to be trusted rather than measured.

## Features that are in scope and not built yet

- **A real syslog sender.** The line format exists and RFC 5424 framing is
  followed; what is missing is a sink that connects. The hard part is not the
  datagram: it is what to do when the collector is down, since blocking is
  forbidden and dropping silently is what makes syslog untrustworthy.
- **Asynchronous, buffered I/O.** Named as absent in the comparison page. The
  hard part is Guile's own arithmetic: no event loop, so a buffer needs a flusher
  that is either a thread (which the project refuses) or a finalizer plus a
  `flush` the program must call — and the flush-on-exit story decides whether a
  crash loses the last hundred lines.
- **Hot reconfiguration.** The memo key is the argument list, so "change the
  level of the logger called `app`" has no handle to hold. A resolution seam —
  a parameter holding the current logger, swapped by a signal handler — is the
  plausible shape, and the muted path must stay a no-op after it.
- **Per-package level inheritance.** The thing every other library has, and the
  thing this design gives up deliberately. Adding it means reintroducing a name
  as a key, which reintroduces the surprising singleton the memo avoids; the
  worth of the trade is an open question, not a settled one.
- **Sampling and rate limiting.** Absent from the API and named as absent in
  the documentation. The hard part is that both must be decided before
  formatting, or the cost they were meant to save has already been paid.
- **A structured log context that carries a scope name**, so a JSON consumer can
  group by module without a tag convention agreed by hand in four projects.

## New features that would be worth having

- **A rotation policy that is not size-based**: time-based rotation and rotation
  on `SIGHUP`, which is what a service manager actually asks for.
- **A `with-log-timing` macro** that logs a duration on exit and on a raised
  exception, since every service writes that helper badly at least once.
- **A redaction layer**: a list of context keys whose values are masked before
  they reach a backend, so a token cannot be logged by accident. This belongs in
  the engine, before the layout, not in each template.
- **A log-line budget for tests**: a logger that captures into a string port and
  an assertion that a body emitted exactly the lines expected, which turns "did
  it log the failure" into a test rather than an eyeball.
- **A journald sink**, writing the native journal fields rather than a plain
  line into stdout, so `journalctl -o json` can filter by field on a systemd
  host.
- **A JSON layout that can nest a context alist one level deep**, because a
  collector that receives `{"request": "…"}` can group by it and one that
  receives `{"request.id": "…"}` cannot without a mapping rule.
- **A benchmark page that runs the same workload against the previous release**,
  so a regression in the hot path is caught by a number rather than a feeling.

## Contributions that would move the needle

1. **A syslog sender with a documented drop policy and a test that kills the
   collector mid-run.** The smallest change that makes the backend usable for
   its actual purpose, and it forces the failure semantics to be written down.
2. **Rate limiting and sampling at construction**, with the dropped count
   reported. It is the feature whose absence makes the library unusable in a
   loop, and it fits the existing compiled-filter design rather than fighting it.
3. **`with-log-timing` plus its test.** Half a day, and it removes the most
   commonly rewritten helper in every service that uses scriba.
4. **A redaction list in the engine**, with a suite asserting that a masked key
   never reaches any of the five backends. Small, security-relevant, and exactly
   the sort of thing a logging library should own.
5. **A journald sink.** One module, one page of code, and it makes scriba the
   obvious choice on a systemd host.
6. **The benchmark re-run against the previous release**, with the table in
   `docs/performance.org` regenerated by a task rather than pasted by hand.
7. **A second language in `docs/comparisons.org`**: a section for a Guile
   neighbour's logging, so the comparison is about this ecosystem and not only
   about other languages.

## Compared to related projects

| Project | What it is | Where scriba differs |
| --- | --- | --- |
| Python `logging` | The standard library's registry of named loggers, handlers and formatters | Hierarchy and configuration files, at the price of a check per call and a `LoggerAdapter` for context; scriba has no names, no hierarchy and no per-call check |
| loguru | A Python logger that is one callable singleton with `bind` and callable sinks | The ergonomics are the lesson — one object, no boilerplate; but a global singleton, no layout as a value, and a level check on every call |
| Go `log/slog` | Structured logging with a handler interface and `With` for attributes | The handler interface is the extension point scriba does not have; levels are atomic variables, so they can change at runtime, which scriba deliberately gives up |
| zap, zerolog | Go encoders designed around typed fields and allocation-free JSON | Faster JSON than scriba, and a richer field model; both assume a compiled struct and neither carries dynamically scoped context |
| Rust `log`, `tracing` | A facade plus spans, where a span is a region of execution that fields attach to | Spans are the richer model for async code; `tracing`'s callsite interest is the nearest relative of scriba's compiled filter |
| pino, winston | Node loggers, JSON first, with child loggers and transports | `pino` is the benchmark to beat on JSON and on the cost of a muted line (7 ns, a property read); both rely on worker threads for I/O |
| SLF4J + Logback | A compile-time facade over an implementation configured in XML, with an MDC | Appenders, layouts and levels are configured rather than constructed, and the MDC is a thread-local map that async code loses; scriba's context is a parameter |
| Elixir `Logger` | Metadata-carrying logging with `:logger` handlers | The closest in spirit to context-as-language-feature; but a process-based runtime scriba does not have, and no layout value to pass |
| Guile's `(ice-9 format)` and `pk` | What a Guile program does before it has a logging library | No levels, no destinations, no rotation and no structure; scriba's whole reason to exist is that `display` calls stop scaling at the second service |

The libraries above are ahead on three things that matter: ecosystem
integration (OpenTelemetry, tracing, metrics bridges), asynchronous I/O with a
worker or an event loop, and the operational furniture — sampling, rate limits,
configuration files that can be reloaded. The first is out of reach without
adding a dependency the project does not want; the second and third are the
substance of the vision section, and both are smaller jobs in Guile than in the
languages that solved them with a runtime scriba does not have.

Where scriba is ahead, and should stay ahead: a muted line is a no-op rather
than a check; the layout is a value validated when the logger is built rather
than a format string parsed at run time; context needs no propagation because it
is the language's dynamic scope; the whole library is small enough to read in an
afternoon; and every claim about cost is a benchmark in the tree with the
machine and the versions beside it. Those five are the identity of the project,
and a change that trades any of them for a feature should say so out loud.

## In the long run

- **One logging library for the ecosystem.** The durable win is not a feature but
  a convention: every jointhefreeworld service producing the same fields, so one
  collector and one query answer questions across projects.
- **Cost as a contract.** The measured overhead of an emitted line and of a
  muted one is a published number per release, and a regression is a release
  blocker rather than a footnote.
- **Retention where the lines are.** Sampling, rate limits and rotation together
  decide what a program keeps. A library that owns all three owns the answer to
  "why is my service quiet", which is otherwise a question for four tools.
- **Exception reports that are worth their millisecond.** Frames, source
  locations and the values in scope, bounded by a depth the caller chooses, is
  the shape; the cost is the thing to keep attacking.
- **Fibers, when the ecosystem uses them.** Guile 3 has a scheduler and the
  neighbours are starting to use it. Context that follows dynamic extent is most
  of the answer already; what needs writing down is what a backend does when a
  flush would block a scheduler.

## Closing

The order that follows from the above: a sender that connects, then volume
control, then re-levelling — each one decided by the measurement it exists to
protect. Nothing about the library's shape has to change to get there, which is
the strongest argument that the shape was right.

*This document describes intent, not commitments.*
