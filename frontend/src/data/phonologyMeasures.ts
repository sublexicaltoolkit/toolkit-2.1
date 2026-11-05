import type { WordEntry } from "./wordData";

// In-house phoneme code, copied from map_PG() in PROCESSING_SCRIPTS.R.
// Order matters: multi-character phonemes are matched before single characters.
const VOWELS = [
  "5", "O", "8", "je", "j3r", "ju", "jU", "o", "2",
  "@", "a", "e", "E", "3r", "i", "1", "c", "u", "U", "^",
];
const CONSONANTS = [
  "G", "gz", "kS", "nj", "C", "T", "D", "Z", "N",
  "b", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p",
  "r", "s", "S", "t", "v", "w", "z",
];
const ALL_PHONEMES = [...VOWELS, ...CONSONANTS];
export const VOWEL_SET = new Set(VOWELS);

// Split a pronunciation like "skul" into phonemes ["s", "k", "u", "l"].
export function splitPhonemes(pronunciation: string): string[] {
  const phonemes: string[] = [];
  let rest = pronunciation;
  while (rest.length > 0) {
    const match = ALL_PHONEMES.find((p) => rest.startsWith(p)) ?? rest[0];
    phonemes.push(match);
    rest = rest.slice(match.length);
  }
  return phonemes;
}

// Onset = consonants before the first vowel, rime = everything after.
// Exact for one-syllable words; for longer words the "rime" is the rest of the word.
export function onsetRime(pronunciation: string): { onset: string; rime: string } {
  const phonemes = splitPhonemes(pronunciation);
  const firstVowel = phonemes.findIndex((p) => VOWEL_SET.has(p));
  if (firstVowel === -1) return { onset: pronunciation, rime: "" };
  return {
    onset: phonemes.slice(0, firstVowel).join(""),
    rime: phonemes.slice(firstVowel).join(""),
  };
}

export type ResultColumn = {
  header: string;
  value: (entry: WordEntry) => number | string | null;
  // Columns sharing a group sit under one spanning header (e.g. a measure over Mean/Min)
  group?: string;
  // Consistency values are proportions, shown as percentages on screen
  percent?: boolean;
};

// Whole-word variables shown at the top of the toolkit page, with where each comes from.
export type GeneralVariable = {
  id: string;
  label: string;
  source: string;
  column: ResultColumn;
};

export const GENERAL_VARIABLES: GeneralVariable[] = [
  {
    id: "frequency",
    label: "Whole-Word Frequency",
    source: "SUBTLEX-US word counts (Brysbaert & New, 2009)",
    column: { header: "Whole-Word Frequency", group: "General", value: (e) => e.freq },
  },
  {
    id: "orthographic-length",
    label: "Orthographic Length",
    source: "Number of letters in the spelling",
    column: {
      header: "Orthographic Length",
      group: "General",
      value: (e) => e.spelling.length,
    },
  },
  {
    id: "phonological-length",
    label: "Phonological Length",
    source: "Number of phonemes in the Toolkit pronunciation",
    column: {
      header: "Phonological Length",
      group: "General",
      value: (e) => splitPhonemes(e.pronunciation).length,
    },
  },
];
