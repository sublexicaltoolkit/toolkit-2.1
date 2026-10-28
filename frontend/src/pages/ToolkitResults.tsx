import { useEffect, useState } from "react";
import { Link, useLocation } from "react-router-dom";
import Layout from "../components/Layout";
import { columnLabel } from "../data/measureColumns";
import {
  PHONOLOGY_COLUMNS,
  onsetRime,
  type ResultColumn,
} from "../data/phonologyMeasures";
import { lookupWords, type LookupResult, type WordEntry } from "../data/wordData";
import "./EnglishToolkit.css";

type ResultVariant = "frequency-consistency" | "standard" | "onset-rime";

type ResultsState = {
  words?: string[];
  phonemes?: string[];
  measures?: string[];
  multiplePronunciations?: "Yes" | "No";
};

type ToolkitResultsProps = {
  variant: ResultVariant;
};

type TableRow = {
  word: string;
  pronunciation: string;
  onset: string;
  rime: string;
  values: string[];
};

const resultConfig: Record<
  ResultVariant,
  { eyebrow: string; title: string; searchPath: string; fallbackMeasures: string[] }
> = {
  "frequency-consistency": {
    eyebrow: "Frequency & Consistency",
    title: "Frequency and Consistency Log Results",
    searchPath: "/english/frequency-consistency/search",
    fallbackMeasures: ["PG_default_PG.mean", "PG_default_GP.mean", "PG_default_PG_freq.mean"],
  },
  standard: {
    eyebrow: "Phonology & Orthography",
    title: "Phonology and Orthography Results",
    searchPath: "/english/phonology-orthography/search",
    fallbackMeasures: [
      "General — Frequency",
      "Orthographic — Orthographic Length",
      "Phonological — Phonological Length",
      "Ortho-Phon — Consistency",
    ],
  },
  "onset-rime": {
    eyebrow: "Phonology & Orthography",
    title: "Onset and Rime Results",
    searchPath: "/english/phonology-orthography/search",
    fallbackMeasures: [
      "Phonological — First Phoneme",
      "Phonological — Phonological Length",
      "Ortho-Phon — Consistency",
    ],
  },
};

function escapeCsv(value: string) {
  return `"${value.replaceAll('"', '""')}"`;
}

function formatValue(value: number | string | null | undefined): string {
  if (value === null || value === undefined || value === "") return "—";
  if (typeof value === "string") return value;
  return Number.isInteger(value) ? String(value) : value.toFixed(4);
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

// Frequency & consistency measures are all_measures.csv column names;
// phonology options are "Group — Option" labels mapped in PHONOLOGY_COLUMNS.
function resolveColumns(
  variant: ResultVariant,
  measures: string[],
): { columns: ResultColumn[]; unavailable: string[] } {
  if (variant === "frequency-consistency") {
    return {
      columns: measures.map((column) => ({
        header: columnLabel(column),
        value: (entry) => entry.measures[column] ?? null,
      })),
      unavailable: [],
    };
  }

  return {
    columns: measures.flatMap((measure) => PHONOLOGY_COLUMNS[measure] ?? []),
    unavailable: measures.filter((measure) => !PHONOLOGY_COLUMNS[measure]),
  };
}

export default function ToolkitResults({ variant }: ToolkitResultsProps) {
  const location = useLocation();
  const state = (location.state ?? {}) as ResultsState;
  const isDemo = new URLSearchParams(location.search).has("demo");
  const config = resultConfig[variant];
  const measures =
    state.measures && state.measures.length > 0
      ? state.measures
      : config.fallbackMeasures;
  const words =
    state.words && state.words.length > 0
      ? state.words
      : isDemo
        ? ["cat", "light", "school"]
        : [];
  const hasOnsetRimeColumns = variant === "onset-rime";
  const inputColumnCount = hasOnsetRimeColumns ? 4 : 2;
  const { columns, unavailable } = resolveColumns(variant, measures);

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
    ...onsetRime(entry.pronunciation),
    values: columns.map((column) => formatValue(column.value(entry))),
  }));

  const notFound = lookup?.notFound ?? [];
  const nothingFound = lookup !== null && tableRows.length === 0;
  const rowCount = tableRows.length;

  const downloadCsv = () => {
    const headers = [
      "Word",
      "Pronunciation",
      ...(hasOnsetRimeColumns ? ["Onset", "Rime"] : []),
      ...columns.map((column) => column.header),
    ];
    const rows = tableRows.map((row) => [
      row.word,
      row.pronunciation,
      ...(hasOnsetRimeColumns ? [row.onset, row.rime] : []),
      ...row.values.map((value) => (value === "—" ? "" : value)),
    ]);
    const csv = [headers, ...rows]
      .map((row) => row.map(escapeCsv).join(","))
      .join("\n");
    const url = URL.createObjectURL(new Blob([csv], { type: "text/csv" }));
    const anchor = document.createElement("a");
    anchor.href = url;
    anchor.download = `${variant}-results.csv`;
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
    <Layout variant="app" pageTitle={config.title}>
      <main className="results-page">
        <header className="results-header">
          <div className="results-meta">
            <span>
              Multiple pronunciations:{" "}
              <strong>{state.multiplePronunciations ?? "No"}</strong>
            </span>
            {isDemo && <span className="demo-badge">Demo data</span>}
          </div>

          <div className="results-actions">
            <Link className="secondary-button" to={config.searchPath}>
              Edit search
            </Link>
            <button
              className="action-button"
              type="button"
              onClick={downloadCsv}
              disabled={isLoading}
            >
              Download CSV
            </button>
          </div>
        </header>

        <section className="results-table-card" aria-label={config.title}>
          <div className="results-table-heading">
            <h2>Results</h2>
            <span>{rowCount} {rowCount === 1 ? "row" : "rows"}</span>
          </div>

          {notFound.length > 0 && !nothingFound && (
            <p className="results-not-found">Not found: {notFound.join(", ")}</p>
          )}
          {unavailable.length > 0 && (
            <p className="results-not-found">
              Not available yet: {unavailable.join(", ")}
            </p>
          )}

          <div className="results-table-scroll">
            <table className="results-table">
              <thead>
                <tr className="results-group-header">
                  <th colSpan={inputColumnCount}>Input</th>
                  {columns.length > 0 && (
                    <th colSpan={columns.length}>Selected Variables</th>
                  )}
                </tr>
                <tr>
                  <th>Word</th>
                  <th>Pronunciation</th>
                  {hasOnsetRimeColumns && (
                    <>
                      <th>Onset</th>
                      <th>Rime</th>
                    </>
                  )}
                  {columns.map((column) => (
                    <th key={column.header}>{column.header}</th>
                  ))}
                </tr>
              </thead>
              <tbody>
                {rowCount > 0 ? (
                  tableRows.map((row, index) => (
                    <tr key={`${row.word}-${row.pronunciation}-${index}`}>
                      <td>{row.word}</td>
                      <td>{row.pronunciation}</td>
                      {hasOnsetRimeColumns && (
                        <>
                          <td>{row.onset || "—"}</td>
                          <td>{row.rime || "—"}</td>
                        </>
                      )}
                      {row.values.map((value, columnIndex) => (
                        <td key={columns[columnIndex].header}>{value}</td>
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
      </main>
    </Layout>
  );
}
