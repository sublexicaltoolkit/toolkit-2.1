import { VOWEL_SET } from "./phonologyMeasures";

// Unit-by-unit mappings exported from the R toolkit's scored_words datasets
// (scripts/frontend-export/export_word_mappings.R), stored in the word data as
// "map_<grain>_<field>" columns with units joined by "|".

export type Grain = "PG" | "OR";
export type UnitValue = "PG" | "GP" | "PG_freq" | "P_freq" | "G_freq";
export type Position = "wi" | "si" | "sm" | "sf" | "wf";

export type MappingUnit = {
  phoneme: string;
  grapheme: string;
  position: Position;
} & Record<UnitValue, number | null>;

export const UNIT_VALUES: UnitValue[] = ["PG", "GP", "PG_freq", "P_freq", "G_freq"];

const FIELDS = ["phonemes", "graphemes", "positions", ...UNIT_VALUES] as const;

function toNumber(value: string | undefined): number | null {
  if (value === undefined || value === "") return null;
  const number = Number(value);
  return Number.isFinite(number) ? number : null;
}

/** Read one grain's units from a word-data row; null when the word was not mappable. */
export function parseMapping(
  row: Record<string, unknown>,
  grain: Grain,
): MappingUnit[] | null {
  const [phonemes, graphemes, positions, ...values] = FIELDS.map((field) => {
    const cell = row[`map_${grain}_${field}`];
    return typeof cell === "string" && cell !== "" ? cell.split("|") : [];
  });
  if (phonemes.length === 0) return null;

  return phonemes.map((phoneme, index) => {
    const unit = {
      phoneme,
      grapheme: graphemes[index] ?? "",
      position: (positions[index] ?? "") as Position,
    } as MappingUnit;
    UNIT_VALUES.forEach((key, valueIndex) => {
      unit[key] = toNumber(values[valueIndex]?.[index]);
    });
    return unit;
  });
}

/** Split PG units into syllables: a new syllable starts at each syllable-initial unit. */
export function splitSyllables(units: MappingUnit[]): MappingUnit[][] {
  const syllables: MappingUnit[][] = [];
  for (const unit of units) {
    if (syllables.length === 0 || unit.position === "si") syllables.push([]);
    syllables[syllables.length - 1].push(unit);
  }
  return syllables;
}

/** Onset = units before the vowel, nucleus = the vowel, coda = units after it. */
export function splitOnsetRime(syllable: MappingUnit[]): {
  onset: MappingUnit[];
  nucleus: MappingUnit | null;
  coda: MappingUnit[];
} {
  const vowel = syllable.findIndex((unit) => VOWEL_SET.has(unit.phoneme));
  if (vowel === -1) return { onset: syllable, nucleus: null, coda: [] };
  return {
    onset: syllable.slice(0, vowel),
    nucleus: syllable[vowel],
    coda: syllable.slice(vowel + 1),
  };
}

/** The unit with the lowest value, written "phoneme+grapheme" (first one on ties). */
export function whichMin(units: MappingUnit[], key: UnitValue): string | null {
  let min: MappingUnit | null = null;
  for (const unit of units) {
    const value = unit[key];
    if (value !== null && (min === null || value < (min[key] as number))) min = unit;
  }
  return min ? `${min.phoneme}+${min.grapheme}` : null;
}

/** Split-digraph and silent-e graphemes (a_e, _e, _el) mark a final E. */
export function hasFinalE(syllable: MappingUnit[]): boolean {
  return syllable.some((unit) => unit.grapheme.includes("_"));
}
