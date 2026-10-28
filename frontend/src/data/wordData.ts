import { parseCsvRows, type CsvRow } from "./csvParsing";

// Per-letter CSVs built from ../data by scripts/build-word-data.mjs
// (runs automatically before `npm run dev` and `npm run build`).
const WORD_DATA_URL = `${import.meta.env.BASE_URL}word-data/`;

export type WordEntry = {
  spelling: string;
  pronunciation: string;
  freq: number | null;
  syllables: number | null;
  // Every mean/min measure column (and pp_*), keyed by its all_measures.csv
  // column name (e.g. "PG_default_PG.mean", "pp_mean").
  measures: Record<string, number | null>;
};

export type LookupResult = {
  entries: WordEntry[];
  notFound: string[];
};

type Shard = Map<string, WordEntry[]>;

const NON_MEASURE_COLUMNS = new Set(["spelling", "pronunciation", "freq", "syllables"]);

// Each letter is downloaded and parsed once, then reused for later searches.
const shardCache = new Map<string, Promise<Shard>>();

function toNumber(value: CsvRow[string] | undefined): number | null {
  return typeof value === "number" ? value : null;
}

function shardKey(word: string): string {
  const first = word[0];
  return first >= "a" && first <= "z" ? first : "other";
}

function toEntry(row: CsvRow): WordEntry {
  const measures: Record<string, number | null> = {};
  for (const [column, value] of Object.entries(row)) {
    if (!NON_MEASURE_COLUMNS.has(column)) measures[column] = toNumber(value);
  }
  return {
    spelling: String(row.spelling),
    pronunciation: String(row.pronunciation),
    freq: toNumber(row.freq),
    syllables: toNumber(row.syllables),
    measures,
  };
}

function loadShard(key: string): Promise<Shard> {
  let shard = shardCache.get(key);
  if (!shard) {
    shard = parseCsvRows(`${WORD_DATA_URL}${key}.csv`, { download: true }).then((rows) => {
      const bySpelling: Shard = new Map();
      for (const row of rows) {
        const entry = toEntry(row);
        const list = bySpelling.get(entry.spelling) ?? [];
        list.push(entry);
        bySpelling.set(entry.spelling, list);
      }
      return bySpelling;
    });
    // Let a failed download be retried on the next search.
    shard.catch(() => shardCache.delete(key));
    shardCache.set(key, shard);
  }
  return shard;
}

/**
 * Look up words in the word data. A word with several pronunciations
 * returns one entry per pronunciation, in the order the words were requested.
 */
export async function lookupWords(words: string[]): Promise<LookupResult> {
  const wanted = [...new Set(words.map((word) => word.trim().toLowerCase()).filter(Boolean))];
  const keys = [...new Set(wanted.map(shardKey))];
  const shards = new Map(
    await Promise.all(keys.map(async (key) => [key, await loadShard(key)] as const)),
  );

  const entries: WordEntry[] = [];
  const notFound: string[] = [];
  for (const word of wanted) {
    const found = shards.get(shardKey(word))?.get(word);
    if (found) entries.push(...found);
    else notFound.push(word);
  }

  return { entries, notFound };
}
