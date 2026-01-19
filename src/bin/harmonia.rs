//! Minimal CLI for `harmonia-theory`.
//!
//! This binary is intentionally tiny: it exists mainly as a debugging lens for the library.

use clap::Parser;

use harmonia_theory::{AnalyzeChordOptions, CadenceHint, Key, KeyMode, PitchClass, ProgressionAnalysis};

#[derive(Debug, Parser)]
#[command(name = "harmonia")]
#[command(about = "Functional harmony helpers (MVP).")]
struct Cli {
    /// Key, e.g. `C:maj` or `A:min`.
    #[arg(long)]
    key: String,

    /// Emit a minimal JSON object.
    #[arg(long)]
    json: bool,

    #[command(subcommand)]
    cmd: Command,
}

#[derive(Debug, clap::Subcommand)]
enum Command {
    /// Analyze a single chord in a key.
    Chord {
        /// Chord tones as note names (pitch classes), e.g. `E G# B D`.
        ///
        /// Enharmonics are accepted but are reduced to pitch classes in this MVP.
        #[arg(long)]
        chord: String,

        /// Disable tonicization candidates (V/x).
        #[arg(long, default_value_t = false)]
        no_tonicization: bool,
    },

    /// Analyze a progression in a key.
    ///
    /// The format is `;`-separated chords, where each chord is whitespace-separated notes:
    /// `--prog "G B D; A C E; F A C; G B D; C E G"`
    Prog {
        #[arg(long)]
        prog: String,

        /// Disable tonicization candidates (V/x).
        #[arg(long, default_value_t = false)]
        no_tonicization: bool,
    },
}

fn main() -> Result<(), String> {
    let cli = Cli::parse();

    let key = parse_key(&cli.key)?;
    match cli.cmd {
        Command::Chord {
            chord,
            no_tonicization,
        } => {
            let chord_pcs = parse_chord_pcs(&chord)?;
            let opts = AnalyzeChordOptions {
                allow_tonicization: !no_tonicization,
            };
            let analyses = harmonia_theory::analyze_chord_in_key(&key, &chord_pcs, &opts);
            print_chord_result(cli.json, &key, &chord_pcs, &analyses);
        }
        Command::Prog {
            prog,
            no_tonicization,
        } => {
            let chords = parse_progression(&prog)?;
            let opts = AnalyzeChordOptions {
                allow_tonicization: !no_tonicization,
            };
            let pa = harmonia_theory::analyze_progression_in_key(&key, &chords, &opts);
            print_prog_result(cli.json, &key, &chords, &pa);
        }
    };

    Ok(())
}

fn print_chord_result(
    json: bool,
    key: &Key,
    chord_pcs: &[PitchClass],
    analyses: &[harmonia_theory::Analysis],
) {
    if json {
        // MVP schema: stable keys; values are intentionally simple.
        let labels: Vec<String> = analyses.iter().map(|a| a.label.clone()).collect();
        let pcs: Vec<u8> = chord_pcs.iter().map(|pc| pc.0).collect();
        println!(
            "{{\"schema_version\":1,\"key\":\"{}\",\"chord_pcs\":{:?},\"labels\":{:?}}}",
            key.display(),
            pcs,
            labels
        );
        return;
    }

    println!("Key: {}", key.display());
    println!(
        "Chord pitch classes: {}",
        chord_pcs
            .iter()
            .map(|pc| pc.display_sharp())
            .collect::<Vec<_>>()
            .join(" ")
    );
    if analyses.is_empty() {
        println!("No analyses (under current MVP rules).");
    } else {
        println!("Analyses:");
        for a in analyses {
            println!("- {}", a.label);
            if !a.reason.is_empty() {
                println!("  {}", a.reason);
            }
        }
    }
}

fn print_prog_result(json: bool, key: &Key, chords: &[Vec<PitchClass>], pa: &ProgressionAnalysis) {
    if json {
        let chord_pcs: Vec<Vec<u8>> = chords
            .iter()
            .map(|c| c.iter().map(|pc| pc.0).collect::<Vec<_>>())
            .collect();
        let labels: Vec<Vec<String>> = pa
            .per_chord
            .iter()
            .map(|as_| as_.iter().map(|a| a.label.clone()).collect::<Vec<_>>())
            .collect();
        let cadences: Vec<String> = pa
            .cadence_hints
            .iter()
            .map(|h| format!("{}:{}:{}", h.resolves_at, cadence_kind_str(h.kind), h.detail))
            .collect();
        println!(
            "{{\"schema_version\":1,\"key\":\"{}\",\"chords_pcs\":{:?},\"labels\":{:?},\"cadences\":{:?}}}",
            key.display(),
            chord_pcs,
            labels,
            cadences
        );
        return;
    }

    println!("Key: {}", key.display());
    for (i, analyses) in pa.per_chord.iter().enumerate() {
        let pcs = chords[i]
            .iter()
            .map(|pc| pc.display_sharp())
            .collect::<Vec<_>>()
            .join(" ");
        println!("Chord {i}: {pcs}");
        if analyses.is_empty() {
            println!("  (no analyses)");
        } else {
            println!("  {}", analyses[0].label);
        }
    }
    if !pa.cadence_hints.is_empty() {
        println!("Cadence hints:");
        for CadenceHint {
            resolves_at,
            kind,
            detail,
        } in &pa.cadence_hints
        {
            println!("- at {resolves_at}: {} ({detail})", cadence_kind_str(*kind));
        }
    }
}

fn cadence_kind_str(k: harmonia_theory::CadenceKind) -> &'static str {
    match k {
        harmonia_theory::CadenceKind::Authentic => "authentic",
        harmonia_theory::CadenceKind::Deceptive => "deceptive",
    }
}

fn parse_key(s: &str) -> Result<Key, String> {
    let (tonic_str, mode_str) = s
        .split_once(':')
        .ok_or_else(|| format!("invalid --key {s:?}; expected e.g. C:maj or A:min"))?;
    let tonic = PitchClass::parse(tonic_str)?;
    let mode = match mode_str {
        "maj" | "major" => KeyMode::Major,
        "min" | "minor" => KeyMode::Minor,
        _ => {
            return Err(format!(
                "invalid --key {s:?}; mode must be maj/major or min/minor"
            ))
        }
    };
    Ok(Key { tonic, mode })
}

fn parse_chord_pcs(s: &str) -> Result<Vec<PitchClass>, String> {
    let mut out = Vec::new();
    for token in s.split_whitespace() {
        out.push(PitchClass::parse(token)?);
    }
    if out.is_empty() {
        return Err("empty --chord; provide note names like \"E G# B D\"".to_string());
    }
    Ok(harmonia_theory::dedup_sort_pcs(out))
}

fn parse_progression(s: &str) -> Result<Vec<Vec<PitchClass>>, String> {
    let mut chords = Vec::new();
    for chunk in s.split(';') {
        let chunk = chunk.trim();
        if chunk.is_empty() {
            continue;
        }
        chords.push(parse_chord_pcs(chunk)?);
    }
    if chords.is_empty() {
        return Err("empty --prog; expected e.g. \"G B D; C E G\"".to_string());
    }
    Ok(chords)
}

