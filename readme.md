# perf-circuits-parser

Benchmark harness comparing `markup-parse` 0.3 (circuits-parser engine) with
`markup-parse` 0.2.2.0 (flatparse engine), over a calibrated synthetic corpus
plus real chart-svg SVG artifacts.

## The two parsers

- **`markup-parse` 0.3** — current, on the `circuits-parser` engine.
- **`markup-parse` 0.2.2.0** — last flatparse-backed release (`git tag v0.2.2.0`).

Both expose the same `markup`/`tokenize`/`markdown` API over strict
`ByteString` with `Standard = Html | Xml`.

## The two harnesses

Both measure identical closures (`bench/Workload.hs`) over identical inputs
(`bench/Corpus.hs` calibrated shapes + real chart-svg SVG in `corpus/`):

- `markup-criterion` — criterion.
- `markup-meter` — circuits-meter's `ticksN`.

## Latest run (GHC 9.14.1, -O1, aarch64)

On the 141 KB chart-wheel SVG, parsed as `Html`:

| stage    | markup-parse 0.3 | markup-parse 0.2.2.0 | slowdown |
|----------|-----------------:|---------------------:|---------:|
| tokenize | ~87 ms           | ~0.50 ms             | **~175x** |
| markup   | ~89 ms           | ~0.65 ms             | **~138x** |

Both harnesses agree within ~1–3%.

## Running

```
cabal run --enable-benchmarks markup-meter
cabal run --enable-benchmarks markup-criterion -- --time-limit 2
```
