# perf-circuits-parser

Benchmark harness for the markup-parsing hot path in `markup-parse`, over a
calibrated synthetic corpus plus real chart-svg SVG artifacts.

## The parser

`markup-parse` parses and prints a subset of common XML & HTML structured data,
from and to strict bytestrings, on top of the `circuits-parser` engine.

## The two harnesses

Both measure identical closures (`bench/Workload.hs`) over identical inputs
(`bench/Corpus.hs` calibrated shapes + real chart-svg SVG in `corpus/`):

- `markup-criterion` — criterion.
- `markup-meter` — circuits-meter's `ticksN`.

## Running

```
cabal run --enable-benchmarks markup-meter
cabal run --enable-benchmarks markup-criterion -- --time-limit 2
```
