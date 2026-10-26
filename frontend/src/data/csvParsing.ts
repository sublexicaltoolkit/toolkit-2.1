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

const baseConfig = {
  header: true,
  skipEmptyLines: true,
  transformHeader: cleanHeader,
  transform: cleanValue,
  dynamicTyping: (field: string | number) => !TEXT_COLUMNS.has(String(field)),
} satisfies Papa.ParseConfig<CsvRow>;

/**
 * Stream a CSV with PapaParse and keep only the rows whose `spelling`
 * is in `words`. Streaming row-by-row (`step`) avoids holding the whole
 * 80MB+ measures file in memory at once.
 *
 * `input` is a URL when `download` is true, otherwise CSV text or a File.
 */
export function parseRowsForWords(
  input: string | File,
  words: Set<string>,
  { download = false }: { download?: boolean } = {},
): Promise<CsvRow[]> {
  return new Promise((resolve, reject) => {
    const rows: CsvRow[] = [];
    const callbacks = {
      step: ({ data }: Papa.ParseStepResult<CsvRow>) => {
        const spelling = String(data.spelling ?? "").toLowerCase();
        if (words.has(spelling)) rows.push(data);
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
