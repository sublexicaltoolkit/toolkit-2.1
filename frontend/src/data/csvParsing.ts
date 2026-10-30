import Papa from "papaparse";

export type CsvValue = string | number | boolean | null;
export type CsvRow = Record<string, CsvValue>;

// Columns that look numeric but must stay as text
// (e.g. the pronunciation "8" is a phoneme code, not the number 8).
const TEXT_COLUMNS = new Set(["spelling", "pronunciation"]);

const BYTE_ORDER_MARK = String.fromCharCode(0xfeff);

// The R exports name the row-number column "" and may start with a BOM.
function cleanHeader(header: string): string {
  const trimmed = header.replace(BYTE_ORDER_MARK, "").trim();
  return trimmed === "" ? "rowId" : trimmed;
}

// R writes missing values as "NA"; blank them so dynamicTyping turns them into null.
function cleanValue(value: string): string {
  return value === "NA" ? "" : value;
}

// A first cell like "word" or "Spelling" is treated as a header and skipped.
const WORD_HEADERS = new Set(["word", "words", "spelling", "item", "items", "stimulus", "stimuli"]);

/**
 * Read the words in the first column of an uploaded .csv or .txt file
 * (one word per line also works; PapaParse guesses the delimiter).
 */
export function parseWordFile(file: File): Promise<string[]> {
  return new Promise((resolve, reject) => {
    Papa.parse<string[]>(file, {
      skipEmptyLines: true,
      complete: ({ data }) => {
        const words = data
          .map((row) => String(row[0] ?? "").replace(BYTE_ORDER_MARK, "").trim())
          .filter(Boolean);
        if (words.length > 0 && WORD_HEADERS.has(words[0].toLowerCase())) words.shift();
        resolve(words);
      },
      error: (err: Error) => reject(err),
    });
  });
}

const baseConfig = {
  header: true,
  skipEmptyLines: true,
  transformHeader: cleanHeader,
  transform: cleanValue,
  // "map_" columns hold unit lists like "h|E|l|o" (a single unit can look numeric, e.g. "8")
  dynamicTyping: (field: string | number) =>
    !TEXT_COLUMNS.has(String(field)) && !String(field).startsWith("map_"),
} satisfies Papa.ParseConfig<CsvRow>;

/**
 * Stream a CSV with PapaParse row-by-row (`step`), keeping the rows
 * that pass `keep` (all rows by default).
 *
 * `input` is a URL when `download` is true, otherwise CSV text or a File.
 */
export function parseCsvRows(
  input: string | File,
  {
    download = false,
    keep = () => true,
  }: { download?: boolean; keep?: (row: CsvRow) => boolean } = {},
): Promise<CsvRow[]> {
  return new Promise((resolve, reject) => {
    const rows: CsvRow[] = [];
    const callbacks = {
      step: ({ data }: Papa.ParseStepResult<CsvRow>) => {
        if (keep(data)) rows.push(data);
      },
      complete: () => resolve(rows),
      error: (err: Error) => reject(err),
    };

    if (download && typeof input === "string") {
      Papa.parse<CsvRow>(input, { ...baseConfig, ...callbacks, download: true });
    } else {
      Papa.parse<CsvRow>(input, { ...baseConfig, ...callbacks });
    }
  });
}
