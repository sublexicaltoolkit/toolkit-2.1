// Builds small per-letter CSVs for the frontend from the raw R exports.
//
//   ../data/wordlist.csv                    (spelling, pronunciation, freq, syllables)
//   ../data/precalculated/all_measures.csv  (84MB, 400+ measure columns)
//     -> public/word-data/<a-z|other>.csv
//
// Only the columns the site uses are kept (mean/min measures and pp_*),
// "NA" becomes an empty cell, and the R row-number column is dropped.
// Skips the work when the outputs are newer than the inputs.
import { existsSync, mkdirSync, readFileSync, statSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import Papa from "papaparse";

const frontendDir = join(dirname(fileURLToPath(import.meta.url)), "..");
const dataDir = join(frontendDir, "..", "data");
const wordlistPath = join(dataDir, "wordlist.csv");
const measuresPath = join(dataDir, "precalculated", "all_measures.csv");
const outDir = join(frontendDir, "public", "word-data");
const stampPath = join(outDir, ".built");

const SHARD_KEYS = [..."abcdefghijklmnopqrstuvwxyz", "other"];
const KEEP_MEASURE = /\.(mean|min)$|^pp_/;

function shardKey(spelling) {
  const first = spelling[0];
  return first >= "a" && first <= "z" ? first : "other";
}

function isUpToDate() {
  if (!existsSync(stampPath)) return false;
  const built = statSync(stampPath).mtimeMs;
  return [wordlistPath, measuresPath, fileURLToPath(import.meta.url)].every(
    (input) => statSync(input).mtimeMs < built,
  );
}

function readCsv(path) {
  const text = readFileSync(path, "utf8").replace(String.fromCharCode(0xfeff), "");
  return Papa.parse(text, { header: true, skipEmptyLines: true }).data;
}

// Shorten long R decimals (0.878033333333333 -> 0.878033) to cut file size.
function cleanNumber(value) {
  if (value === undefined || value === "" || value === "NA") return "";
  const number = Number(value);
  return Number.isFinite(number) ? String(Number(number.toFixed(6))) : "";
}

if (isUpToDate() && !process.argv.includes("--force")) {
  console.log("word-data is up to date");
  process.exit(0);
}

const started = Date.now();
const measureRows = readCsv(measuresPath);
const measureColumns = Object.keys(measureRows[0]).filter((column) => KEEP_MEASURE.test(column));

const measuresByKey = new Map();
for (const row of measureRows) {
  measuresByKey.set(`${row.spelling.toLowerCase()}|${row.pronunciation}`, row);
}

const header = ["spelling", "pronunciation", "freq", "syllables", ...measureColumns];
const shards = new Map(SHARD_KEYS.map((key) => [key, [header]]));

for (const row of readCsv(wordlistPath)) {
  const spelling = row.spelling.toLowerCase();
  const measures = measuresByKey.get(`${spelling}|${row.pronunciation}`) ?? {};
  shards.get(shardKey(spelling)).push([
    spelling,
    row.pronunciation,
    cleanNumber(row.freq),
    cleanNumber(row.syllables),
    ...measureColumns.map((column) => cleanNumber(measures[column])),
  ]);
}

mkdirSync(outDir, { recursive: true });
let totalBytes = 0;
for (const [key, rows] of shards) {
  // Quote every cell so pronunciations like "8" or "@t" stay text-safe.
  const csv = Papa.unparse(rows, { quotes: (_, index) => index < 2 });
  writeFileSync(join(outDir, `${key}.csv`), csv);
  totalBytes += Buffer.byteLength(csv);
}
writeFileSync(stampPath, new Date().toISOString());

console.log(
  `word-data: ${SHARD_KEYS.length} files, ${(totalBytes / 1e6).toFixed(1)} MB, ` +
    `${measureColumns.length} measure columns, ${Date.now() - started} ms`,
);
