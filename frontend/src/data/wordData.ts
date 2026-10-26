import allMeasuresUrl from "../../../data/precalculated/all_measures.csv?url";
import wordlistUrl from "../../../data/wordlist.csv?url";
import { parseRowsForWords, type CsvRow } from "./csvParsing";

export type WordEntry = {
  spelling: string;
  pronunciation: string;
  freq: number | null;
  syllables: number | null;
  // Every numeric column from all_measures.csv, keyed by its column name
  // (e.g. "PG_default_PG.mean", "pp_mean").
  measures: Record<string, number | null>;
};

export type LookupResult = {
  entries: WordEntry[];
  notFound: string[];
};

const NON_MEASURE_COLUMNS = new Set(["rowId", "spelling", "pronunciation"]);

function toNumber(value: CsvRow[string] | undefined): number | null {
  return typeof value === "number" ? value : null;
}

function entryKey(spelling: unknown, pronunciation: unknown): string {
  return `${String(spelling).toLowerCase()}|${String(pronunciation)}`;
}

/**
 * Look up words in the wordlist and precalculated measures CSVs.
 * A word with several pronunciations returns one entry per pronunciation,
 * in the order the words were requested.
 */
export async function lookupWords(words: string[]): Promise<LookupResult> {
  const wanted = new Set(words.map((word) => word.trim().toLowerCase()).filter(Boolean));
  if (wanted.size === 0) return { entries: [], notFound: [] };

  const [wordlistRows, measureRows] = await Promise.all([
    parseRowsForWords(wordlistUrl, wanted, { download: true }),
    parseRowsForWords(allMeasuresUrl, wanted, { download: true }),
  ]);

  const measuresByKey = new Map<string, Record<string, number | null>>();
  for (const row of measureRows) {
    const measures: Record<string, number | null> = {};
    for (const [column, value] of Object.entries(row)) {
      if (!NON_MEASURE_COLUMNS.has(column)) measures[column] = toNumber(value);
    }
    measuresByKey.set(entryKey(row.spelling, row.pronunciation), measures);
  }

  const entriesBySpelling = new Map<string, WordEntry[]>();
  for (const row of wordlistRows) {
    const spelling = String(row.spelling).toLowerCase();
    const pronunciation = String(row.pronunciation);
    const list = entriesBySpelling.get(spelling) ?? [];
    list.push({
      spelling,
      pronunciation,
      freq: toNumber(row.freq),
      syllables: toNumber(row.syllables),
      measures: measuresByKey.get(entryKey(spelling, pronunciation)) ?? {},
    });
    entriesBySpelling.set(spelling, list);
  }

  const entries: WordEntry[] = [];
  const notFound: string[] = [];
  for (const word of wanted) {
    const found = entriesBySpelling.get(word);
    if (found) entries.push(...found);
    else notFound.push(word);
  }

  return { entries, notFound };
}
