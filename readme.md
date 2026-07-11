# perf-circuits-parser

Benchmark harness for the markup-parsing hot path, comparing three parsers and
cross-checking two measurement methods.

## The three parsers

All expose the same `markup_`/`tokenize`/`markdown_` API over strict `ByteString`
with `Standard = Html | Xml`, but differ underneath:

- **`Circuit.Markup`** (from `circuits-parser`) — markup layer chart-svg migrated to.
- **current `markup-parse`** — separate package, its own `Markup`/`Token` types,
  but depends on `circuits-parser` for the `Circuit.Parser` engine.
- **`markup-parse` v0.2.2.0 (flatparse era)** — the last version before the
  circuits-parser migration (`git tag v0.2.2.0` @ `040c48a`), backed by
  `flatparse` + an internal `MarkupParse.Internal.FlatParse`. Pinned in
  `cabal.project` via `source-repository-package`.

`Circuit.Markup` and current `markup-parse` share the `Trace Either (->)` engine
(hence byte-identical token counts and near-identical speed — they are NOT the
same modules, just the same engine). The flatparse `v0.2.2.0` is the real
baseline: a different engine entirely.

## The two harnesses

Both measure identical closures (`bench/Workload.hs`) over identical inputs
(`bench/Corpus.hs` calibrated shapes + real chart-svg SVG in `corpus/`):

- `markup-criterion` — criterion.
- `markup-meter` — circuits-meter's `ticksN`.

## Findings (GHC 9.14.1, -O1, aarch64)

### 1. circuits-meter is validated against criterion

Within ~1-5% on all but the smallest input, identical rankings. On ~100 µs
inputs the fixed-iteration mean reads a little high vs criterion's adaptive
regression — expected. **circuits-meter holds up against its main competitor.**

### 2. Circuit.Markup ≈ current markup-parse

Both on the `Circuit.Parser` engine: ratio 0.98x-1.11x across all shapes/sizes.
Byte-identical token counts (2659 on the wheel SVG). An earlier "35% slower"
result was a broken-harness artifact (wrong `Standard`, mis-forced results).

### 3. flatparse CRUSHES the Circuit.Parser engine

On the 141 KB chart-wheel SVG (both produce 2659 tokens — same work):

| stage    | flatparse v0.2.2.0 | Circuit.Markup | slowdown |
|----------|-------------------:|---------------:|---------:|
| tokenize | ~0.5 ms            | ~14 ms         | **~27x** |
| markup   | ~0.66 ms           | ~1.0 s         | **~1400x** |

Confirmed by BOTH harnesses (criterion: 660 µs vs 1.01 s markup; meter: 668 µs
vs 0.93 s). The gap scales with size and tree depth:

| synthetic shape          | markup slowdown |
|--------------------------|----------------:|
| contentHeavy-500x40      | ~550x |
| flat-2000 (wide/shallow) | ~350x |
| wideAttrs-500x8          | ~260x |
| nested-500 (deep)        | ~106x |

The gather/tree-building stage is where the orders-of-magnitude gap opens; raw
tokenize is "only" ~10-30x. This is the real cost of the flatparse→Circuit.Parser
migration: sub-millisecond becomes ~1 second per large SVG.

## Pitfalls this harness encodes (learned the hard way)

- **`Standard` matters.** These parsers reject attributes under `Xml` but accept
  them under `Html` (`ParserLeftover`). SVG parses cleanly as `Html`. Passing
  `Xml` yields `These [warning] []` — zero tokens — a benchmark that times
  nothing. Always confirm non-zero token counts before trusting numbers.
- **`Warn a = These [warning] a`.** Result is in the `That`/`These` right slot;
  `This` carries warnings only. Decode all branches, force the real payload.
- **Force to a scalar sink.** `Workload` returns `Int` counts so the full spine
  is walked; forcing the `These` shell alone under-measures.

## Running

```
cabal run --enable-benchmarks markup-meter
cabal run --enable-benchmarks markup-criterion -- --time-limit 2
```
