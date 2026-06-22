# harmonia

Functional harmony helpers.

This crate provides deterministic, pitch-class-based helpers for roman-numeral
labels, tonicization candidates, and relative-major/minor pivots.

## Public Invariants

- Pitch-class only: `C# == Db` in the API.
- Deterministic: same inputs produce same outputs, including ordering.
- Candidates, not truth: outputs are plausible labels; ambiguity is expected.
- No modulation inference: `V/x` is a local hint, not a detected key change.

## Non-Goals

- Audio / ML
- Full roman-text parsing (`.rntxt`)
- Enharmonic spelling, voice leading, figured bass

## CLI Examples

Single chord in a key (secondary dominant):

```bash
cargo run --features cli --bin harmonia -- \
  --key C:maj chord --chord "E G# B D"
```

Progression in a key (cadence hints are heuristic):

```bash
cargo run --features cli --bin harmonia -- \
  --key C:maj prog --prog "G B D; A C E; C E G"
```
