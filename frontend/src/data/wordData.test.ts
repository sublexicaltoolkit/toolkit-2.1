import { readFileSync } from "node:fs";
import { join } from "node:path";
import { beforeEach, describe, expect, it, vi } from "vitest";
import { parseCsvRows, type CsvRow } from "./csvParsing";
import { MEASURE_GROUPS, STATS, columnName } from "./measureColumns";
import { PHONOLOGY_COLUMNS } from "./phonologyMeasures";
import { lookupWords } from "./wordData";

// Uses the real files from scripts/build-word-data.mjs (run by `npm test`),
// read from disk instead of downloaded.
const WORD_DATA_DIR = join(import.meta.dirname, "..", "..", "public", "word-data");

vi.mock("./csvParsing", async (importOriginal) => {
  const original = await importOriginal<typeof import("./csvParsing")>();
  const { readFileSync } = await import("node:fs");
  const { join } = await import("node:path");
  const dir = join(import.meta.dirname, "..", "..", "public", "word-data");
  return {
    ...original,
    parseCsvRows: vi.fn((url: string | File, options?: { keep?: (row: CsvRow) => boolean }) => {
      const file = String(url).split("/").pop() ?? "";
      return original.parseCsvRows(readFileSync(join(dir, file), "utf8"), {
        keep: options?.keep,
      });
    }),
  };
});

const parseSpy = vi.mocked(parseCsvRows);

describe("lookupWords", () => {
  beforeEach(() => {
    parseSpy.mockClear();
  });

  it("finds a word with its data", async () => {
    const { entries, notFound } = await lookupWords(["cat"]);

    expect(notFound).toEqual([]);
    expect(entries).toHaveLength(1);
    expect(entries[0]).toMatchObject({ spelling: "cat", pronunciation: "k@t", freq: 3383, syllables: 1 });
    expect(entries[0].measures["PG_default_PG.mean"]).toBeCloseTo(0.878033, 5);
  });

  it("returns every pronunciation of a word", async () => {
    const { entries } = await lookupWords(["a"]);
    expect(entries.map((entry) => entry.pronunciation)).toEqual(["8", "e"]);
  });

  it("ignores case, spaces, and duplicates and keeps the requested order", async () => {
    const { entries } = await lookupWords([" School", "light", "school", ""]);
    expect(entries.map((entry) => entry.spelling)).toEqual(["school", "light"]);
  });

  it("reports words that are not in the data", async () => {
    const { entries, notFound } = await lookupWords(["cat", "qwzx", "1234"]);
    expect(entries.map((entry) => entry.spelling)).toEqual(["cat"]);
    expect(notFound).toEqual(["qwzx", "1234"]);
  });

  it("downloads each letter file only once", async () => {
    await lookupWords(["zebra"]);
    await lookupWords(["zoo", "zero"]);
    const zLoads = parseSpy.mock.calls.filter(([url]) => String(url).endsWith("/z.csv"));
    expect(zLoads).toHaveLength(1);
  });
});

describe("data columns used by the pages", () => {
  const header = readFileSync(join(WORD_DATA_DIR, "a.csv"), "utf8").split("\n")[0];
  const columns = new Set(header.split(",").map((column) => column.replaceAll('"', "")));

  it("include every frequency & consistency column", () => {
    const missing = MEASURE_GROUPS.flatMap((group) =>
      STATS.map((stat) => columnName(group.base, stat)),
    ).filter((column) => !columns.has(column));
    expect(missing).toEqual([]);
  });

  it("give values for every phonology option with data", async () => {
    const { entries } = await lookupWords(["cat"]);
    const values = Object.values(PHONOLOGY_COLUMNS)
      .flat()
      .map((column) => [column.header, column.value(entries[0])]);

    expect(Object.fromEntries(values)).toEqual({
      "Frequency (SUBTLEX-US count)": 3383,
      "Orthographic Length": 3,
      "Phonological Length": 3,
      "First Phoneme": "k",
      "PG Reading Consistency — Mean": entries[0].measures["PG_default_PG.mean"],
      "PG Spelling Consistency — Mean": entries[0].measures["PG_default_GP.mean"],
    });
  });
});
