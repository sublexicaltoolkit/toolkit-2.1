import { Link, useLocation } from "react-router-dom";
import Layout from "../components/Layout";
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

const resultConfig: Record<
  ResultVariant,
  { eyebrow: string; title: string; searchPath: string; fallbackMeasures: string[] }
> = {
  "frequency-consistency": {
    eyebrow: "Frequency & Consistency",
    title: "Frequency and Consistency Log Results",
    searchPath: "/english/frequency-consistency/search",
    fallbackMeasures: ["Frequency", "Contextual Diversity", "PG Consistency"],
  },
  standard: {
    eyebrow: "Phonology & Orthography",
    title: "Phonology and Orthography Results",
    searchPath: "/english/phonology-orthography/search",
    fallbackMeasures: [
      "General — Frequency",
      "Orthographic — Orthographic Length",
      "Phonological — Phonological Length",
      "Semantic — Concreteness",
    ],
  },
  "onset-rime": {
    eyebrow: "Phonology & Orthography",
    title: "Onset and Rime Results",
    searchPath: "/english/phonology-orthography/search",
    fallbackMeasures: [
      "Orthographic — Orthographic Neighborhood",
      "Phonological — Phonological Neighborhood",
      "Orthography → Phonology — Consistency",
    ],
  },
};

function escapeCsv(value: string) {
  return `"${value.replaceAll('"', '""')}"`;
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
  const phonemes =
    state.phonemes && state.phonemes.length > 0
      ? state.phonemes
      : isDemo
        ? ["/kæt/", "/laɪt/", "/skuːl/"]
        : [];
  const rowCount = Math.max(words.length, phonemes.length);
  const hasOnsetRimeColumns = variant === "onset-rime";
  const inputColumnCount = hasOnsetRimeColumns ? 4 : 2;

  const downloadCsv = () => {
    const headers = [
      "Word",
      "IPA Transcription",
      ...(hasOnsetRimeColumns ? ["Onset", "Rime"] : []),
      ...measures,
    ];
    const rows = Array.from({ length: rowCount }, (_, index) => [
      words[index] ?? "",
      phonemes[index] ?? "",
      ...(hasOnsetRimeColumns ? ["", ""] : []),
      ...measures.map(() => ""),
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
            <button className="action-button" type="button" onClick={downloadCsv}>
              Download CSV
            </button>
          </div>
        </header>

        <section className="results-table-card" aria-label={config.title}>
          <div className="results-table-heading">
            <h2>Results</h2>
            <span>{rowCount} {rowCount === 1 ? "word" : "words"}</span>
          </div>

          <div className="results-table-scroll">
            <table className="results-table">
              <thead>
                <tr className="results-group-header">
                  <th colSpan={inputColumnCount}>Input</th>
                  <th colSpan={measures.length}>Selected Variables</th>
                </tr>
                <tr>
                  <th>Word</th>
                  <th>IPA Transcription</th>
                  {hasOnsetRimeColumns && (
                    <>
                      <th>Onset</th>
                      <th>Rime</th>
                    </>
                  )}
                  {measures.map((measure) => (
                    <th key={measure}>{measure}</th>
                  ))}
                </tr>
              </thead>
              <tbody>
                {rowCount > 0 ? (
                  Array.from({ length: rowCount }, (_, index) => (
                    <tr key={`${words[index] ?? "word"}-${index}`}>
                      <td>{words[index] || "—"}</td>
                      <td>{phonemes[index] || "—"}</td>
                      {hasOnsetRimeColumns && (
                        <>
                          <td>{isDemo ? ["k", "l", "sk"][index] : "—"}</td>
                          <td>{isDemo ? ["æt", "aɪt", "uːl"][index] : "—"}</td>
                        </>
                      )}
                      {measures.map((measure, measureIndex) => (
                        <td key={measure}>
                          {isDemo
                            ? (0.42 + index * 0.11 + measureIndex * 0.07).toFixed(2)
                            : "—"}
                        </td>
                      ))}
                    </tr>
                  ))
                ) : (
                  <tr>
                    <td
                      className="results-empty"
                      colSpan={inputColumnCount + measures.length}
                    >
                      Return to the search page and enter words to populate this table.
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
