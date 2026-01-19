# harmonia

Symbolic helpers for **functional harmony** (roman numerals, tonicization, relative-major/minor pivots).

This is currently an MVP crate for symbolic functional harmony.

## Public invariants (short)

- **Pitch-class only**: `C# == Db` in the API.
- **Deterministic**: same inputs → same outputs (including ordering).
- **Candidates, not truth**: outputs are plausible labels; ambiguity is expected.
- **No modulation inference**: `V/x` is a local hint, not a detected key change.

## Non-goals (for now)

- Audio / ML
- Full roman-text parsing (`.rntxt`)
- Enharmonic spelling, voice leading, figured bass

## CLI examples

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

