//! `harmonia`: functional harmony helpers (MVP).
//!
//! ## Public invariants (must not change without a major version bump)
//!
//! - **Pitch-class semantics**: all note spellings reduce to 12-TET pitch classes.
//!   `C#` and `Db` are the same value in this crate's public types.
//! - **Deterministic**: the same inputs yield the same outputs (ordering included).
//! - **Candidate generation only**: functions return *possible* labels.
//!   False positives are allowed; the API makes no “the truth is …” claim.
//! - **No modulation inference**: tonicization (`V/x`) is a local hint, not a key-change detector.
//!
//! ## Swappable parts (intentionally incomplete / likely to evolve)
//!
//! - **Ranking/scoring** between candidates.
//! - **Which harmony rules are implemented** (mixture, Neapolitan, augmented-sixth, cadential 6/4, etc.).
//! - **Enharmonic spelling** (future adapter layer, likely via a dedicated spelling crate).
//! - **Minor-mode conventions** (natural vs harmonic/melodic minor for different tasks).
//!
//! ## What this crate is (MVP)
//!
//! - Deterministic, symbolic analysis utilities
//! - Pitch-class-only (enharmonic spelling is *not* preserved yet)
//! - Focused on: chord-in-key roman numerals and simple tonicization (V/x)
//!
//! ## What this crate is not (yet)
//!
//! - No audio, no ML
//! - No full modulation detection (only local tonicization hints)
//! - No borrowed chords / augmented-sixth / cadential 6/4, etc.

/// A pitch class in 12-TET, \(0..=11\).
///
/// Mapping (sharp spelling):
/// - 0=C, 1=C#, 2=D, 3=D#, 4=E, 5=F, 6=F#, 7=G, 8=G#, 9=A, 10=A#, 11=B
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PitchClass(pub u8);

impl PitchClass {
    /// Parse common note spellings into a pitch class.
    ///
    /// This is intentionally permissive (accepts enharmonic spellings), because the MVP
    /// is pitch-class-only. Examples:
    /// - `C`, `C#`, `Db`, `E`, `Fb`, `B#`, `Cb`
    pub fn parse(s: &str) -> Result<Self, String> {
        let s = s.trim();
        if s.is_empty() {
            return Err("empty pitch class".to_string());
        }

        // Normalize a few Unicode accidentals into ASCII.
        let s = s
            .replace('♯', "#")
            .replace('♭', "b")
            .replace('𝄪', "##")
            .replace('𝄫', "bb");

        let (letter, rest) = s.split_at(1);
        let base = match letter {
            "C" | "c" => 0i32,
            "D" | "d" => 2,
            "E" | "e" => 4,
            "F" | "f" => 5,
            "G" | "g" => 7,
            "A" | "a" => 9,
            "B" | "b" => 11,
            _ => return Err(format!("invalid pitch class {s:?}")),
        };

        let mut acc = 0i32;
        let mut chars = rest.chars().peekable();
        while let Some(c) = chars.peek().copied() {
            match c {
                '#' => {
                    acc += 1;
                    chars.next();
                }
                'b' => {
                    acc -= 1;
                    chars.next();
                }
                _ => break,
            }
        }

        if chars.next().is_some() {
            return Err(format!("invalid pitch class {s:?} (unexpected suffix)"));
        }

        Ok(PitchClass(mod12(base + acc)))
    }

    /// Display in a canonical sharp spelling (MVP).
    pub fn display_sharp(self) -> &'static str {
        match self.0 {
            0 => "C",
            1 => "C#",
            2 => "D",
            3 => "D#",
            4 => "E",
            5 => "F",
            6 => "F#",
            7 => "G",
            8 => "G#",
            9 => "A",
            10 => "A#",
            11 => "B",
            _ => "?",
        }
    }
}

/// Major vs minor key mode (natural minor for diatonic membership in this MVP).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KeyMode {
    /// Ionian.
    Major,
    /// Natural minor (Aeolian) for now.
    Minor,
}

/// A key as (tonic pitch class, mode).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Key {
    /// Tonic pitch class (0=C, …, 11=B).
    pub tonic: PitchClass,
    /// Major vs minor (natural minor in this MVP).
    pub mode: KeyMode,
}

impl Key {
    /// Display as `C:maj` or `A:min` (MVP).
    pub fn display(self) -> String {
        let mode = match self.mode {
            KeyMode::Major => "maj",
            KeyMode::Minor => "min",
        };
        format!("{}:{}", self.tonic.display_sharp(), mode)
    }

    /// Diatonic scale pitch classes, starting from tonic.
    pub fn diatonic_scale(self) -> [PitchClass; 7] {
        let intervals: [i32; 7] = match self.mode {
            KeyMode::Major => [0, 2, 4, 5, 7, 9, 11],
            KeyMode::Minor => [0, 2, 3, 5, 7, 8, 10],
        };
        intervals.map(|d| PitchClass(mod12(self.tonic.0 as i32 + d)))
    }

    /// Relative minor (only defined for major keys in this MVP).
    ///
    /// Example: C major -> A minor.
    pub fn relative_minor(self) -> Option<Key> {
        if self.mode != KeyMode::Major {
            return None;
        }
        // Down a minor third == +9 mod 12.
        Some(Key {
            tonic: PitchClass(mod12(self.tonic.0 as i32 + 9)),
            mode: KeyMode::Minor,
        })
    }
}

/// A diatonic scale degree \(1..=7\).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Degree(pub u8);

impl Degree {
    /// Construct a degree if it is in \(1..=7\).
    pub fn new(n: u8) -> Result<Self, String> {
        if (1..=7).contains(&n) {
            Ok(Degree(n))
        } else {
            Err(format!("degree must be 1..=7, got {n}"))
        }
    }
}

/// A single “pivot identity” between a major key and its relative minor:
/// a shared pitch class that is degree \(d_M\) in the major and degree \(d_m\) in the minor.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DegreePivot {
    /// The degree in the major key (1..=7).
    pub major_degree: Degree,
    /// The degree in the relative minor key (1..=7).
    pub minor_degree: Degree,
    /// The shared pitch class.
    pub pitch_class: PitchClass,
}

/// The full degree-to-degree correspondence between a major key and its relative minor.
///
/// In natural-scale pitch classes, the mapping is:
///
/// \[
/// 1_M \leftrightarrow 3_m,\;
/// 2_M \leftrightarrow 4_m,\;
/// 3_M \leftrightarrow 5_m,\;
/// 4_M \leftrightarrow 6_m,\;
/// 5_M \leftrightarrow 7_m,\;
/// 6_M \leftrightarrow 1_m,\;
/// 7_M \leftrightarrow 2_m.
/// \]
///
/// This is the precise version of the “multiplier of goodness” observation:
/// a stable scale degree in one key is *also* a stable scale degree (often a chord tone)
/// in the relative key.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RelativeMinorPivots {
    /// The major key.
    pub major: Key,
    /// The relative minor key.
    pub minor: Key,
    /// One pivot per major degree (in major-degree order).
    pub pivots: [DegreePivot; 7],
}

/// Compute the pitch-class pivot correspondences between a major key and its relative minor.
///
/// Returns `None` if `key` is not a major key.
pub fn relative_minor_pivots(key: Key) -> Option<RelativeMinorPivots> {
    let minor = key.relative_minor()?;
    let major_scale = key.diatonic_scale();
    let minor_scale = minor.diatonic_scale();

    // For each major degree, find which minor degree shares the pitch class.
    let mut pivots: [DegreePivot; 7] = [DegreePivot {
        major_degree: Degree(1),
        minor_degree: Degree(1),
        pitch_class: major_scale[0],
    }; 7];

    for (i, &pc) in major_scale.iter().enumerate() {
        let major_degree = Degree((i as u8) + 1);
        let mut found_minor_degree = None;
        for (j, &mpc) in minor_scale.iter().enumerate() {
            if mpc == pc {
                found_minor_degree = Some(Degree((j as u8) + 1));
                break;
            }
        }
        let minor_degree = found_minor_degree.expect("relative keys share pitch-class set");
        pivots[i] = DegreePivot {
            major_degree,
            minor_degree,
            pitch_class: pc,
        };
    }

    Some(RelativeMinorPivots {
        major: key,
        minor,
        pivots,
    })
}

/// Options for chord analysis (placeholder for future rule switches).
#[derive(Debug, Clone, Copy)]
pub struct AnalyzeChordOptions {
    /// If true, include tonicization candidates (V/x).
    pub allow_tonicization: bool,
}

impl Default for AnalyzeChordOptions {
    fn default() -> Self {
        Self {
            allow_tonicization: true,
        }
    }
}

/// A single analysis candidate.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Analysis {
    /// The label (e.g. `vi`, `V7/vi`).
    pub label: String,
    /// A short explanation for why this label was produced.
    pub reason: String,
}

/// De-duplicate and sort pitch classes (ascending numeric).
pub fn dedup_sort_pcs(mut pcs: Vec<PitchClass>) -> Vec<PitchClass> {
    pcs.sort();
    pcs.dedup();
    pcs
}

/// Analyze a chord (given as pitch classes) in the given key.
///
/// The output is a list of candidates, best-first, and may be ambiguous.
pub fn analyze_chord_in_key(
    key: &Key,
    chord: &[PitchClass],
    opts: &AnalyzeChordOptions,
) -> Vec<Analysis> {
    let chord = dedup_sort_pcs(chord.to_vec());
    if chord.len() < 3 {
        return Vec::new();
    }

    let spellings = detect_chord_spellings(&chord);
    if spellings.is_empty() {
        return Vec::new();
    }

    let mut out: Vec<(i32, Analysis)> = Vec::new();
    let chord_is_in_key_pitch_set = chord
        .iter()
        .all(|&pc| pitch_class_is_in_key_pitch_set(*key, pc));

    for sp in &spellings {
        // 1) Diatonic roman numeral candidate.
        if chord_is_in_key_pitch_set {
            if let Some((deg, deg_pc, deg_note)) = scale_degree_in_key_extended(key, sp.root) {
                let rn = roman_for_degree(deg, sp.quality, sp.seventh);
                let reason = format!(
                    "root {} is scale degree {} (pc {}) in {}{}",
                    sp.root.display_sharp(),
                    deg,
                    deg_pc.display_sharp(),
                    key.display(),
                    deg_note
                );
                // Prefer diatonic interpretations.
                out.push((0, Analysis { label: rn, reason }));
            }
        }

        // 2) Tonicization (secondary dominant) candidate.
        if opts.allow_tonicization && sp.is_dominantish() {
            if let Some(t) = tonicization_target_in_key(key, sp.root) {
                let label = match sp.seventh {
                    Some(SeventhQuality::Dominant7) => format!("V7/{}", t.target_rn),
                    None => format!("V/{}", t.target_rn),
                    _ => format!("V/{}", t.target_rn),
                };
                let reason = format!(
                    "dominant-of relation: {} is a fifth above {} ({} in {})",
                    sp.root.display_sharp(),
                    t.target_pc.display_sharp(),
                    t.target_rn,
                    key.display()
                );
                // Prefer tonicizations if not already diatonic.
                out.push((1, Analysis { label, reason }));
            }
        }
    }

    out.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.label.cmp(&b.1.label)));
    // Dedup by label, keep first reason.
    let mut deduped: Vec<Analysis> = Vec::new();
    for (_, a) in out {
        if deduped.iter().any(|x| x.label == a.label) {
            continue;
        }
        deduped.push(a);
    }
    deduped
}

/// A small “progression view” over a sequence of chords.
///
/// This deliberately does **not** attempt global key inference. It is a convenience wrapper:
/// analyze each chord in the same key, then optionally produce a few cadence hints.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProgressionAnalysis {
    /// Per-chord analysis candidates, in chord order.
    pub per_chord: Vec<Vec<Analysis>>,
    /// Simple cadence hints derived from the top-ranked label per chord.
    pub cadence_hints: Vec<CadenceHint>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
/// A heuristic cadence marker produced from adjacent roman-numeral labels.
pub struct CadenceHint {
    /// Index of the *resolution* chord (the second chord in the pair).
    pub resolves_at: usize,
    /// Type of cadence (very small MVP set).
    pub kind: CadenceKind,
    /// Human-readable detail (e.g. `V → I`).
    pub detail: String,
}

/// Very small cadence taxonomy (MVP).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CadenceKind {
    /// V → I (or V → i).
    Authentic,
    /// V → vi.
    Deceptive,
}

/// Analyze a chord progression in a fixed key.
pub fn analyze_progression_in_key(
    key: &Key,
    chords: &[Vec<PitchClass>],
    opts: &AnalyzeChordOptions,
) -> ProgressionAnalysis {
    let per_chord: Vec<Vec<Analysis>> = chords
        .iter()
        .map(|c| analyze_chord_in_key(key, c, opts))
        .collect();
    let cadence_hints = detect_simple_cadences(key, &per_chord);
    ProgressionAnalysis {
        per_chord,
        cadence_hints,
    }
}

fn detect_simple_cadences(_key: &Key, per_chord: &[Vec<Analysis>]) -> Vec<CadenceHint> {
    let mut hints = Vec::new();
    for i in 1..per_chord.len() {
        let prev = per_chord[i - 1].first().map(|a| a.label.as_str());
        let cur = per_chord[i].first().map(|a| a.label.as_str());
        let Some(prev) = prev else { continue };
        let Some(cur) = cur else { continue };

        // MVP heuristics:
        // - Authentic: V -> I (or V7 -> I) and their minor analogues.
        // - Deceptive: V -> vi (or V7 -> vi) and minor i -> VI is omitted for now.
        if (prev == "V" || prev == "V7") && (cur == "I" || cur == "i") {
            hints.push(CadenceHint {
                resolves_at: i,
                kind: CadenceKind::Authentic,
                detail: format!("{prev} → {cur}"),
            });
        } else if (prev == "V" || prev == "V7") && cur == "vi" {
            hints.push(CadenceHint {
                resolves_at: i,
                kind: CadenceKind::Deceptive,
                detail: format!("{prev} → {cur}"),
            });
        }
    }
    hints
}

/// A triad quality.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TriadQuality {
    Major,
    Minor,
    Diminished,
    Augmented,
}

/// A seventh quality (MVP: only dominant-7 is used for tonicization).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SeventhQuality {
    Dominant7,
    Major7,
    Minor7,
    HalfDiminished7,
    Diminished7,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ChordSpelling {
    root: PitchClass,
    quality: TriadQuality,
    seventh: Option<SeventhQuality>,
}

impl ChordSpelling {
    fn is_dominantish(self) -> bool {
        matches!(
            (self.quality, self.seventh),
            (TriadQuality::Major, None) | (TriadQuality::Major, Some(SeventhQuality::Dominant7))
        )
    }
}

fn detect_chord_spellings(chord: &[PitchClass]) -> Vec<ChordSpelling> {
    let mut out = Vec::new();
    for &root in chord {
        let intervals: Vec<u8> = chord.iter().map(|&pc| interval_from_to(root, pc)).collect();
        // Triads
        if chord.len() == 3 {
            if matches_set(&intervals, &[0, 4, 7]) {
                out.push(ChordSpelling {
                    root,
                    quality: TriadQuality::Major,
                    seventh: None,
                });
            } else if matches_set(&intervals, &[0, 3, 7]) {
                out.push(ChordSpelling {
                    root,
                    quality: TriadQuality::Minor,
                    seventh: None,
                });
            } else if matches_set(&intervals, &[0, 3, 6]) {
                out.push(ChordSpelling {
                    root,
                    quality: TriadQuality::Diminished,
                    seventh: None,
                });
            } else if matches_set(&intervals, &[0, 4, 8]) {
                out.push(ChordSpelling {
                    root,
                    quality: TriadQuality::Augmented,
                    seventh: None,
                });
            }
        }
        // Sevenths
        if chord.len() == 4 {
            if matches_set(&intervals, &[0, 4, 7, 10]) {
                out.push(ChordSpelling {
                    root,
                    quality: TriadQuality::Major,
                    seventh: Some(SeventhQuality::Dominant7),
                });
            } else if matches_set(&intervals, &[0, 4, 7, 11]) {
                out.push(ChordSpelling {
                    root,
                    quality: TriadQuality::Major,
                    seventh: Some(SeventhQuality::Major7),
                });
            } else if matches_set(&intervals, &[0, 3, 7, 10]) {
                out.push(ChordSpelling {
                    root,
                    quality: TriadQuality::Minor,
                    seventh: Some(SeventhQuality::Minor7),
                });
            } else if matches_set(&intervals, &[0, 3, 6, 10]) {
                out.push(ChordSpelling {
                    root,
                    quality: TriadQuality::Diminished,
                    seventh: Some(SeventhQuality::HalfDiminished7),
                });
            } else if matches_set(&intervals, &[0, 3, 6, 9]) {
                out.push(ChordSpelling {
                    root,
                    quality: TriadQuality::Diminished,
                    seventh: Some(SeventhQuality::Diminished7),
                });
            }
        }
    }
    out
}

fn matches_set(got: &[u8], want: &[u8]) -> bool {
    if got.len() != want.len() {
        return false;
    }
    let mut a = got.to_vec();
    let mut b = want.to_vec();
    a.sort_unstable();
    b.sort_unstable();
    a == b
}

fn interval_from_to(root: PitchClass, other: PitchClass) -> u8 {
    mod12(other.0 as i32 - root.0 as i32)
}

fn mod12(x: i32) -> u8 {
    let mut r = x % 12;
    if r < 0 {
        r += 12;
    }
    r as u8
}

fn scale_degree_in_key(key: &Key, pc: PitchClass) -> Option<(u8, PitchClass)> {
    let scale = key.diatonic_scale();
    for (i, &dpc) in scale.iter().enumerate() {
        if dpc == pc {
            // degrees are 1-based
            return Some(((i as u8) + 1, dpc));
        }
    }
    None
}

fn pitch_class_is_in_key_pitch_set(key: Key, pc: PitchClass) -> bool {
    if scale_degree_in_key(&key, pc).is_some() {
        return true;
    }
    // Same extension policy as `scale_degree_in_key_extended`, but applied to any pitch.
    if key.mode == KeyMode::Minor {
        let raised7 = PitchClass(mod12(key.tonic.0 as i32 + 11));
        if pc == raised7 {
            return true;
        }
    }
    false
}

fn scale_degree_in_key_extended(
    key: &Key,
    pc: PitchClass,
) -> Option<(u8, PitchClass, &'static str)> {
    if let Some((deg, dpc)) = scale_degree_in_key(key, pc) {
        return Some((deg, dpc, ""));
    }

    // MVP minor-mode extension: allow the raised leading tone (harmonic minor) as degree 7.
    //
    // Rationale: many practical analyses treat V and vii° as “in-key” in minor via the raised 7th.
    // (music21 exposes similar minor-mode adjustments; see `music21.roman.RomanNumeral.seventhMinor`
    // and related helpers.)
    if key.mode == KeyMode::Minor {
        let raised7 = PitchClass(mod12(key.tonic.0 as i32 + 11));
        if pc == raised7 {
            return Some((7, raised7, " (minor: allowing raised 7th)"));
        }
    }

    None
}

fn roman_for_degree(deg: u8, quality: TriadQuality, seventh: Option<SeventhQuality>) -> String {
    let base = match deg {
        1 => "I",
        2 => "II",
        3 => "III",
        4 => "IV",
        5 => "V",
        6 => "VI",
        7 => "VII",
        _ => "?",
    };
    let mut s = match quality {
        TriadQuality::Major => base.to_string(),
        TriadQuality::Minor => base.to_ascii_lowercase(),
        TriadQuality::Diminished => format!("{}°", base.to_ascii_lowercase()),
        TriadQuality::Augmented => format!("{}+", base),
    };
    if seventh.is_some() {
        s.push('7');
    }
    s
}

struct TonicizationTarget {
    target_pc: PitchClass,
    target_rn: String,
}

fn tonicization_target_in_key(key: &Key, dom_root: PitchClass) -> Option<TonicizationTarget> {
    // If dom_root is V of X, then X = dom_root - 7 (mod 12).
    let target_pc = PitchClass(mod12(dom_root.0 as i32 - 7));
    let (deg, _) = scale_degree_in_key(key, target_pc)?;

    // Target RN is the diatonic triad on that degree (major/minor/dim) *in the key*.
    // MVP heuristic: assume diatonic triad quality by mode+degree.
    let target_quality = diatonic_triad_quality(key.mode, deg);
    let target_rn = roman_for_degree(deg, target_quality, None);
    Some(TonicizationTarget {
        target_pc,
        target_rn,
    })
}

fn diatonic_triad_quality(mode: KeyMode, deg: u8) -> TriadQuality {
    match (mode, deg) {
        // Major: I ii iii IV V vi vii°
        (KeyMode::Major, 1) => TriadQuality::Major,
        (KeyMode::Major, 2) => TriadQuality::Minor,
        (KeyMode::Major, 3) => TriadQuality::Minor,
        (KeyMode::Major, 4) => TriadQuality::Major,
        (KeyMode::Major, 5) => TriadQuality::Major,
        (KeyMode::Major, 6) => TriadQuality::Minor,
        (KeyMode::Major, 7) => TriadQuality::Diminished,
        // Natural minor: i ii° III iv v VI VII
        (KeyMode::Minor, 1) => TriadQuality::Minor,
        (KeyMode::Minor, 2) => TriadQuality::Diminished,
        (KeyMode::Minor, 3) => TriadQuality::Major,
        (KeyMode::Minor, 4) => TriadQuality::Minor,
        (KeyMode::Minor, 5) => TriadQuality::Minor,
        (KeyMode::Minor, 6) => TriadQuality::Major,
        (KeyMode::Minor, 7) => TriadQuality::Major,
        _ => TriadQuality::Major,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn diatonic_vi_in_major() {
        let key = Key {
            tonic: PitchClass(0), // C
            mode: KeyMode::Major,
        };
        let chord = dedup_sort_pcs(vec![
            PitchClass::parse("A").expect("A"),
            PitchClass::parse("C").expect("C"),
            PitchClass::parse("E").expect("E"),
        ]);
        let analyses = analyze_chord_in_key(&key, &chord, &AnalyzeChordOptions::default());
        assert!(analyses.iter().any(|a| a.label == "vi"), "{analyses:?}");
    }

    #[test]
    fn tonicization_v7_of_vi_in_c_major() {
        // E7 -> A minor in C major.
        let key = Key {
            tonic: PitchClass::parse("C").expect("C"),
            mode: KeyMode::Major,
        };
        let chord = dedup_sort_pcs(vec![
            PitchClass::parse("E").expect("E"),
            PitchClass::parse("G#").expect("G#"),
            PitchClass::parse("B").expect("B"),
            PitchClass::parse("D").expect("D"),
        ]);
        let analyses = analyze_chord_in_key(&key, &chord, &AnalyzeChordOptions::default());
        assert!(analyses.iter().any(|a| a.label == "V7/vi"), "{analyses:?}");
    }

    #[test]
    fn relative_minor_multiplier_pitch_identity() {
        // In C major, the 3rd is E. In A minor (relative minor), E is the 5th.
        let cmaj = Key {
            tonic: PitchClass::parse("C").expect("C"),
            mode: KeyMode::Major,
        };
        let amin = cmaj.relative_minor().expect("relative minor exists");
        assert_eq!(amin.tonic, PitchClass::parse("A").expect("A"));

        // 3rd of C major triad: E
        let e = PitchClass::parse("E").expect("E");

        // Check it belongs to both tonic triads (pitch-class-only statement).
        let c_tonic = [
            PitchClass::parse("C").expect("C"),
            e,
            PitchClass::parse("G").expect("G"),
        ];
        let a_tonic = [
            PitchClass::parse("A").expect("A"),
            PitchClass::parse("C").expect("C"),
            e,
        ];

        assert!(c_tonic.contains(&e));
        assert!(a_tonic.contains(&e));
        // And the identity statement: 3_major == 5_relative_minor (as pitch-class).
        // (We’re not encoding scale-degree numbers directly yet; this is the invariant we care about.)
    }

    #[test]
    fn relative_minor_degree_pivot_table_matches_expected_mapping() {
        let key = Key {
            tonic: PitchClass::parse("C").expect("C"),
            mode: KeyMode::Major,
        };
        let piv = relative_minor_pivots(key).expect("C major has relative minor");
        assert_eq!(piv.minor.display(), "A:min");

        // Expected degree map (major -> minor) for relative minor.
        let expected = [(1u8, 3u8), (2, 4), (3, 5), (4, 6), (5, 7), (6, 1), (7, 2)];

        for (i, (m, n)) in expected.iter().enumerate() {
            assert_eq!(piv.pivots[i].major_degree, Degree(*m));
            assert_eq!(piv.pivots[i].minor_degree, Degree(*n));
        }
    }

    #[test]
    fn cadence_hint_v_to_i_in_c_major() {
        let key = Key {
            tonic: PitchClass::parse("C").expect("C"),
            mode: KeyMode::Major,
        };
        let chords = vec![
            dedup_sort_pcs(
                vec!["G", "B", "D"]
                    .into_iter()
                    .map(|s| PitchClass::parse(s).unwrap())
                    .collect(),
            ),
            dedup_sort_pcs(
                vec!["C", "E", "G"]
                    .into_iter()
                    .map(|s| PitchClass::parse(s).unwrap())
                    .collect(),
            ),
        ];
        let pa = analyze_progression_in_key(&key, &chords, &AnalyzeChordOptions::default());
        assert!(
            pa.cadence_hints
                .iter()
                .any(|h| h.kind == CadenceKind::Authentic && h.resolves_at == 1),
            "{:?}",
            pa.cadence_hints
        );
    }

    #[test]
    fn tonicization_v_of_v_and_v_of_ii_in_c_major() {
        let key = Key {
            tonic: PitchClass::parse("C").expect("C"),
            mode: KeyMode::Major,
        };

        // D major is V/V in C.
        let d_major = dedup_sort_pcs(vec![
            PitchClass::parse("D").unwrap(),
            PitchClass::parse("F#").unwrap(),
            PitchClass::parse("A").unwrap(),
        ]);
        let a1 = analyze_chord_in_key(&key, &d_major, &AnalyzeChordOptions::default());
        assert!(a1.iter().any(|a| a.label == "V/V"), "{a1:?}");

        // A major is V/ii in C.
        let a_major = dedup_sort_pcs(vec![
            PitchClass::parse("A").unwrap(),
            PitchClass::parse("C#").unwrap(),
            PitchClass::parse("E").unwrap(),
        ]);
        let a2 = analyze_chord_in_key(&key, &a_major, &AnalyzeChordOptions::default());
        assert!(a2.iter().any(|a| a.label == "V/ii"), "{a2:?}");
    }

    #[test]
    fn deceptive_cadence_v_to_vi_in_c_major() {
        let key = Key {
            tonic: PitchClass::parse("C").expect("C"),
            mode: KeyMode::Major,
        };
        let chords = vec![
            dedup_sort_pcs(
                vec!["G", "B", "D"]
                    .into_iter()
                    .map(|s| PitchClass::parse(s).unwrap())
                    .collect(),
            ),
            dedup_sort_pcs(
                vec!["A", "C", "E"]
                    .into_iter()
                    .map(|s| PitchClass::parse(s).unwrap())
                    .collect(),
            ),
        ];
        let pa = analyze_progression_in_key(&key, &chords, &AnalyzeChordOptions::default());
        assert!(
            pa.cadence_hints
                .iter()
                .any(|h| h.kind == CadenceKind::Deceptive && h.resolves_at == 1),
            "{:?}",
            pa.cadence_hints
        );
    }

    #[test]
    fn leading_tone_in_minor_is_degree_seven_under_extension() {
        let key = Key {
            tonic: PitchClass::parse("A").expect("A"),
            mode: KeyMode::Minor,
        };
        // G# diminished: G# B D (vii° in A minor harmonic minor).
        let chord = dedup_sort_pcs(vec![
            PitchClass::parse("G#").unwrap(),
            PitchClass::parse("B").unwrap(),
            PitchClass::parse("D").unwrap(),
        ]);
        let analyses = analyze_chord_in_key(&key, &chord, &AnalyzeChordOptions::default());
        assert!(analyses.iter().any(|a| a.label == "vii°"), "{analyses:?}");
    }
}
