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

## Library Example

```rust
use harmonia::{analyze_chord_in_key, AnalyzeChordOptions, Key, KeyMode, PitchClass};

let key = Key { tonic: PitchClass::parse("C").unwrap(), mode: KeyMode::Major };
let chord: Vec<PitchClass> = ["E", "G#", "B", "D"]
    .iter()
    .map(|n| PitchClass::parse(n).unwrap())
    .collect();

let labels: Vec<String> = analyze_chord_in_key(&key, &chord, &AnalyzeChordOptions::default())
    .into_iter()
    .map(|a| a.label)
    .collect();
assert!(labels.contains(&"V7/vi".to_string()));
```

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

## License

Licensed under either the [Apache License, Version 2.0](LICENSE-APACHE) or
the [MIT license](LICENSE-MIT), at your option.
