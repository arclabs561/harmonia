//! `--json` output must be valid JSON with the documented keys.

use std::process::Command;

fn run_json(args: &[&str]) -> serde_json::Value {
    let out = Command::new(env!("CARGO_BIN_EXE_harmonia"))
        .args(args)
        .output()
        .expect("run harmonia");
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8(out.stdout).expect("utf-8 stdout");
    serde_json::from_str(&stdout).unwrap_or_else(|e| panic!("invalid JSON ({e}): {stdout}"))
}

#[test]
fn chord_json_parses_and_has_schema_keys() {
    let v = run_json(&["--json", "--key", "C:maj", "chord", "--chord", "E G# B D"]);
    assert_eq!(v["schema_version"], 1);
    assert_eq!(v["key"], "C:maj");
    assert_eq!(v["chord_pcs"], serde_json::json!([2, 4, 8, 11]));
    let labels = v["labels"].as_array().expect("labels array");
    assert!(labels.iter().any(|l| l == "V7/vi"), "{labels:?}");
}

#[test]
fn progression_json_parses_including_non_ascii_cadence_detail() {
    // Cadence details contain U+2192 and labels can contain U+00B0.
    let v = run_json(&[
        "--json",
        "--key",
        "A:min",
        "prog",
        "--prog",
        "G# B D F; E G# B D; A C E",
    ]);
    assert_eq!(v["schema_version"], 1);
    assert_eq!(v["chords_pcs"].as_array().map(Vec::len), Some(3));
    assert_eq!(v["labels"].as_array().map(Vec::len), Some(3));
    let cadences = v["cadences"].as_array().expect("cadences array");
    assert!(
        cadences
            .iter()
            .any(|c| c.as_str().is_some_and(|s| s.contains('→'))),
        "{cadences:?}"
    );
}
