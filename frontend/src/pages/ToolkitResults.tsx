import { useEffect, useState } from "react";
import { Link, useLocation } from "react-router-dom";
import Layout from "../components/Layout";
import WordBreakdown from "../components/WordBreakdown";
import { columnParts } from "../data/measureColumns";
import { GENERAL_VARIABLES, type ResultColumn } from "../data/phonologyMeasures";
import { lookupWords, type LookupResult, type WordEntry } from "../data/wordData";
import "./EnglishToolkit.css";

type ResultsState = {
  words?: string[];
  phonemes?: string[];
  // GENERAL_VARIABLES ids, e.g. "frequency"
  general?: string[];
  // all_measures.csv column names, e.g. "PG_default_PG.mean"
  measures?: string[];
  multiplePronunciations?: "Yes" | "No";
};

type TableRow = {
  word: string;
  pronunciation: string;
  values: string[];
  csvValues: string[];
  // A word with several pronunciations, listed in the homograph table instead
  isHomograph?: boolean;
};

type HeaderGroup = { label: string; span: number };

const DEFAULT_GROUP = "Selected Variables";

const TITLE = "Toolkit Results";
const SEARCH_PATH = "/english";
const DEMO_GENERAL = GENERAL_VARIABLES.map((variable) => variable.id);
const DEMO_MEASURES = ["PG_default_PG.mean", "PG_default_GP.mean", "pp_mean"];

function escapeCsv(value: string) {
  return `"${value.replaceAll('"', '""')}"`;
}

function formatValue(value: number | string | null | undefined): string {
  if (value === null || value === undefined || value === "") return "—";
  if (typeof value === "string") return value;
  return Number.isInteger(value) ? String(value) : value.toFixed(4);
}

function formatDisplay(value: number | string | null | undefined, percent?: boolean): string {
  if (percent && typeof value === "number") return `${(value * 100).toFixed(2)}%`;
  return formatValue(value);
}

// Merge neighbouring columns with the same group into one spanning header.
function headerGroups(columns: ResultColumn[]): HeaderGroup[] {
  const groups: HeaderGroup[] = [];
  for (const column of columns) {
    const label = column.group ?? DEFAULT_GROUP;
    const last = groups[groups.length - 1];
    if (last && last.label === label) last.span += 1;
    else groups.push({ label, span: 1 });
  }
  return groups;
}

function csvHeader(column: ResultColumn): string {
  return column.group ? `${column.group} — ${column.header}` : column.header;
}

// Keep only the first listed pronunciation of each word.
function firstPronunciations(entries: WordEntry[]): WordEntry[] {
  const seen = new Set<string>();
  return entries.filter((entry) => {
    if (seen.has(entry.spelling)) return false;
    seen.add(entry.spelling);
    return true;
  });
}

// General variables come first, then the Toolkit measure columns.
function resolveColumns(general: string[], measures: string[]): ResultColumn[] {
  const generalColumns = GENERAL_VARIABLES.filter((variable) =>
    general.includes(variable.id),
  ).map((variable) => variable.column);
  const measureColumns = measures.map((column): ResultColumn => {
    const parts = columnParts(column);
    return {
      header: parts?.statLabel ?? column,
      group: parts?.group.label,
      percent:
        parts?.group.measureType === "Consistency" ||
        parts?.group.measureType === "Parsing Probability",
      value: (entry) => entry.measures[column] ?? null,
    };
  });
  return [...generalColumns, ...measureColumns];
}

export default function ToolkitResults() {
  const location = useLocation();
  const state = (location.state ?? {}) as ResultsState;
  const isDemo = new URLSearchParams(location.search).has("demo");
  const hasSelection = Boolean(state.general?.length || state.measures?.length);
  const general = hasSelection ? (state.general ?? []) : DEMO_GENERAL;
  const measures = hasSelection ? (state.measures ?? []) : DEMO_MEASURES;
  const words =
    state.words && state.words.length > 0
      ? state.words
      : isDemo
        ? ["cat", "light", "school"]
        : [];
  const columns = resolveColumns(general, measures);
  const inputColumnCount = 2;

  const wordsKey = words.join("\n");
  const [lookup, setLookup] = useState<LookupResult | null>(null);
  const [loadError, setLoadError] = useState<string | null>(null);

  useEffect(() => {
    if (!wordsKey) return;
    let cancelled = false;
    setLookup(null);
    setLoadError(null);
    lookupWords(wordsKey.split("\n"))
      .then((result) => {
        if (!cancelled) setLookup(result);
      })
      .catch((err: Error) => {
        if (!cancelled) setLoadError(err.message);
      });
    return () => {
      cancelled = true;
    };
  }, [wordsKey]);

  const isLoading = words.length > 0 && !lookup && !loadError;

  const entries = lookup
    ? state.multiplePronunciations === "Yes"
      ? lookup.entries
      : firstPronunciations(lookup.entries)
    : [];
  const tableRows: TableRow[] = entries.map((entry) => ({
    word: entry.spelling,
    pronunciation: entry.pronunciation,
    values: columns.map((column) => formatDisplay(column.value(entry), column.percent)),
    csvValues: columns.map((column) => formatValue(column.value(entry))),
  }));
  const groups = headerGroups(columns);

  // A word with several pronunciations gets one "homograph"
  // row in the main table, and its pronunciations are listed in a table of their own.
  const showHomographs = state.multiplePronunciations === "Yes";
  const pronunciationCounts = new Map<string, number>();
  for (const entry of entries) {
    pronunciationCounts.set(entry.spelling, (pronunciationCounts.get(entry.spelling) ?? 0) + 1);
  }
  const isHomograph = (spelling: string) =>
    showHomographs && (pronunciationCounts.get(spelling) ?? 0) > 1;
  const mainRows: TableRow[] = [];
  const homographTableRows: (TableRow & { number: number })[] = [];
  for (const row of tableRows) {
    if (!isHomograph(row.word)) {
      mainRows.push(row);
      continue;
    }
    const number = homographTableRows.filter((other) => other.word === row.word).length + 1;
    if (number === 1) {
      mainRows.push({ ...row, pronunciation: "homograph", isHomograph: true });
    }
    homographTableRows.push({ ...row, number });
  }

  // Which word the breakdown cards show
  const [selected, setSelected] = useState({ wordsKey: "", index: 0 });
  const selectedIndex = selected.wordsKey === wordsKey ? selected.index : 0;
  const showBreakdown = entries.length > 0;

  const notFound = lookup?.notFound ?? [];
  const nothingFound = lookup !== null && tableRows.length === 0;
  const rowCount = tableRows.length;

  const downloadCsv = () => {
    const headers = ["Word", "Pronunciation", ...columns.map(csvHeader)];
    const rows = tableRows.map((row) => [
      row.word,
      row.pronunciation,
      ...row.csvValues.map((value) => (value === "—" ? "" : value)),
    ]);
    const csv = [headers, ...rows]
      .map((row) => row.map(escapeCsv).join(","))
      .join("\n");
    const url = URL.createObjectURL(new Blob([csv], { type: "text/csv" }));
    const anchor = document.createElement("a");
    anchor.href = url;
    anchor.download = "toolkit-results.csv";
    anchor.click();
    URL.revokeObjectURL(url);
  };

  let emptyMessage = "Return to the search page and enter words to populate this table.";
  if (isLoading) emptyMessage = "Loading measures…";
  else if (loadError) emptyMessage = `Could not load the measures data: ${loadError}`;
  else if (nothingFound)
    emptyMessage =
      "Nothing was found for this query. Try fine-tuning your words, or contact the team listed on the About page.";

  return (
    <Layout variant="app" pageTitle={TITLE}>
      <main className="results-page">
        <header className="results-header">
          <div className="results-meta">
            <span>
              Multiple pronunciations:{" "}
              <strong>{state.multiplePronunciations ?? "No"}</strong>
            </span>
            {isDemo && <span className="demo-badge">Demo data</span>}
          </div>
        </header>

        {showBreakdown && (
          <WordBreakdown
            entries={entries}
            selectedIndex={selectedIndex}
            onSelect={(index) => setSelected({ wordsKey, index })}
            multiplePronunciations={state.multiplePronunciations ?? "No"}
            sortByOnsetRime={false}
          />
        )}

        <section className="results-table-card" aria-label={TITLE}>
          <div className="results-table-heading">
            <h2>Results</h2>
            <span>{rowCount} {rowCount === 1 ? "row" : "rows"}</span>
          </div>

          {notFound.length > 0 && !nothingFound && (
            <p className="results-not-found">Not found: {notFound.join(", ")}</p>
          )}
          <div className="results-table-scroll">
            <table className="results-table">
              <thead>
                <tr className="results-group-header">
                  <th colSpan={inputColumnCount} aria-label="Input" />
                  {groups.map((group, index) => (
                    <th key={`${group.label}-${index}`} colSpan={group.span}>
                      {group.label}
                    </th>
                  ))}
                </tr>
                <tr className="results-subheader">
                  <th>Word Inputs</th>
                  <th>Pronunciation</th>
                  {columns.map((column, index) => (
                    <th key={`${csvHeader(column)}-${index}`}>{column.header}</th>
                  ))}
                </tr>
              </thead>
              <tbody>
                {rowCount > 0 ? (
                  mainRows.map((row, index) => (
                    <tr key={`${row.word}-${row.pronunciation}-${index}`}>
                      <td className="results-word">{row.word}</td>
                      <td className={row.isHomograph ? "results-homograph" : undefined}>
                        {row.pronunciation}
                      </td>
                      {row.values.map((value, columnIndex) => (
                        <td key={columnIndex}>{row.isHomograph ? "—" : value}</td>
                      ))}
                    </tr>
                  ))
                ) : (
                  <tr>
                    <td
                      className="results-empty"
                      colSpan={inputColumnCount + columns.length}
                    >
                      {emptyMessage}
                    </td>
                  </tr>
                )}
              </tbody>
            </table>
          </div>
        </section>

        {homographTableRows.length > 0 && (
          <section className="results-table-card homograph-card" aria-label="Homographs">
            <div className="results-table-scroll">
              <table className="results-table homograph-table">
                <thead>
                  <tr className="results-group-header">
                    <th colSpan={3} aria-label="Homograph" />
                    {groups.map((group, index) => (
                      <th key={`${group.label}-${index}`} colSpan={group.span}>
                        {group.label}
                      </th>
                    ))}
                  </tr>
                  <tr className="results-subheader">
                    <th>Spelling</th>
                    <th>Homographs</th>
                    <th>Pronunciation</th>
                    {columns.map((column, index) => (
                      <th key={`${csvHeader(column)}-${index}`}>{column.header}</th>
                    ))}
                  </tr>
                </thead>
                <tbody>
                  {homographTableRows.map((row) => (
                    <tr key={`${row.word}-${row.pronunciation}`}>
                      <td>{row.word}</td>
                      <td>{row.number}</td>
                      <td>{row.pronunciation}</td>
                      {row.values.map((value, columnIndex) => (
                        <td key={columnIndex}>{value}</td>
                      ))}
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </section>
        )}

        <div className="results-actions">
          <button
            className="download-button"
            type="button"
            onClick={downloadCsv}
            disabled={isLoading}
          >
            Download results
          </button>
          <Link className="back-button" to={SEARCH_PATH}>
            Back
          </Link>
        </div>
      </main>
    </Layout>
  );
}
