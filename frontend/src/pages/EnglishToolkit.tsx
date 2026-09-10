import { useEffect, useMemo, useState } from "react";
import Papa from "papaparse";
import "./EnglishToolkit.css";
import measureCatalog from "../measure_catalog_data/measureCatalog.json";

// ---------- Types ----------
type UnitType = "PG" | "OR" | "OC" | "ONC" | "Syllable";
type MeasureType = "Frequency" | "Consistency";
type Directionality = "Reading" | "Spelling" | null;
type Weighting = "Token" | "Type";

type CatalogMeasure = {
  id: string;
  unitType: UnitType;
  measureType: MeasureType;
  directionality: Directionality;
  weighting: Weighting;
  position?: "default" | "noposition" | "freq" | null;
  target?: string | null;
  stat: "mean" | "median" | "max" | "min" | "sd";
  label: string;
  description?: string;
};

type TableRowStats = {
  mean: boolean;
  min: boolean;
  max: boolean;
  median: boolean;
  standardDeviation: boolean;
};

type TableRowsState = Record<
  string,
  {
    id: string; // base catalog id, e.g. "PG_default_PG" — used to find the real CSV columns
    measureType: MeasureType;
    isPercent: boolean; // Consistency measures are stored 0-1 and displayed as %
    selected: boolean;
    stats: TableRowStats;
    description?: string;
  }
>;

// Raw allMeasures.csv row — every value comes in as a string
type MeasureRow = Record<string, string>;

type ColumnSpec = {
  measureId: string;
  label: string;
  stat: keyof TableRowStats;
  isPercent: boolean;
};

type MainResultRow = {
  word: string;
  found: boolean;
  isHomograph: boolean;
  values: Record<string, string>; // key: `${measureId}__${stat}`, already formatted
};

// A homograph word has one BreakdownRow per pronunciation. Each pronunciation
// gets the SAME set of selected measure/stat values as a normal word would —
// plus the pronunciation-specific "phoneme-pronunciation" frequency stats,
// which only make sense when a spelling has more than one pronunciation.
type BreakdownRow = {
  spelling: string;
  homographIndex: number;
  homographCount: number;
  pronunciation: string;
  values: Record<string, string>; // key: `${measureId}__${stat}`, same shape as MainResultRow.values
  ppMean: string;
  ppMin: string;
};

// ---------- Static config ----------
const STAT_SUFFIX: Record<keyof TableRowStats, string> = {
  mean: "mean",
  min: "min",
  max: "max",
  median: "median",
  standardDeviation: "sd",
};

const STAT_ORDER: (keyof TableRowStats)[] = ["mean", "min", "max", "median", "standardDeviation"];

const STAT_HEADER_LABEL: Record<keyof TableRowStats, string> = {
  mean: "Mean",
  min: "Min",
  max: "Max",
  median: "Median",
  standardDeviation: "SD",
};

// Path to the raw allMeasures.csv, served as a static asset.
// Adjust this if your build serves it from somewhere else (e.g. an import with ?url).
const ALL_MEASURES_CSV_PATH = "/data/precalculated/all_measures.csv";

// ---------- Helpers ----------
function makeDisplayLabel(m: CatalogMeasure): string {
  if (m.measureType === "Consistency") {
    const dir = m.directionality ?? "Reading";
    return `${m.unitType} ${dir} Consistency`;
  }
  const clean = (m.label || m.id).replace(/\.(mean|min|max|median|sd)$/i, "");
  return clean;
}

function baseKey(m: CatalogMeasure): string {
  return m.id.replace(/\.mean$|\.min$|\.max$|\.median$|\.sd$/i, "");
}

function parseWordInput(raw: string): string[] {
  return raw
    .split(/[\s,;\n]+/)
    .map((w) => w.trim().toLowerCase())
    .filter(Boolean);
}

function getSelectedColumns(rows: TableRowsState): ColumnSpec[] {
  const cols: ColumnSpec[] = [];
  for (const [label, data] of Object.entries(rows)) {
    if (!data.selected) continue;
    STAT_ORDER.forEach((stat) => {
      if (data.stats[stat]) {
        cols.push({ measureId: data.id, label, stat, isPercent: data.isPercent });
      }
    });
  }
  return cols;
}

function groupColumnsByLabel(cols: ColumnSpec[]) {
  const groups = new Map<string, ColumnSpec[]>();
  cols.forEach((c) => {
    const arr = groups.get(c.label) ?? [];
    arr.push(c);
    groups.set(c.label, arr);
  });
  return groups;
}

function formatValue(raw: string | undefined, isPercent: boolean): string {
  if (raw === undefined) return "-";
  const trimmed = raw.trim();
  if (trimmed === "" || trimmed.toUpperCase() === "NA" || trimmed.toUpperCase() === "NAN") return "-";
  const num = Number(trimmed);
  if (Number.isNaN(num)) return trimmed;
  return isPercent ? `${(num * 100).toFixed(2)}%` : num.toFixed(4);
}

// Build the `${measureId}__${stat}` -> formatted value map for a single CSV row,
// using whatever measures/stats are currently selected. Used for BOTH the
// single-pronunciation main-row case and every pronunciation of a homograph.
function buildRowValues(row: MeasureRow, columns: ColumnSpec[]): Record<string, string> {
  const values: Record<string, string> = {};
  columns.forEach((c) => {
    const csvKey = `${c.measureId}.${STAT_SUFFIX[c.stat]}`;
    values[`${c.measureId}__${c.stat}`] = formatValue(row[csvKey], c.isPercent);
  });
  return values;
}

function buildResults(
  words: string[],
  index: Map<string, MeasureRow[]>,
  columns: ColumnSpec[]
): { mainRows: MainResultRow[]; breakdown: BreakdownRow[] } {
  const mainRows: MainResultRow[] = [];
  const breakdown: BreakdownRow[] = [];

  words.forEach((word) => {
    const matches = index.get(word) ?? [];

    if (matches.length === 0) {
      mainRows.push({ word, found: false, isHomograph: false, values: {} });
      return;
    }

    if (matches.length > 1) {
      // Homograph: the main table can't show one set of numbers for a word
      // with multiple pronunciations, so it stays blank there. Instead, every
      // pronunciation gets its own fully-populated row (same measures/stats
      // the user selected) in the breakdown table below.
      mainRows.push({ word, found: true, isHomograph: true, values: {} });
      matches.forEach((row, i) => {
        breakdown.push({
          spelling: word,
          homographIndex: i,
          homographCount: matches.length,
          pronunciation: row.pronunciation ?? "-",
          values: buildRowValues(row, columns),
          ppMean: formatValue(row.pp_mean, false),
          ppMin: formatValue(row.pp_min, false),
        });
      });
      return;
    }

    const row = matches[0];
    mainRows.push({ word, found: true, isHomograph: false, values: buildRowValues(row, columns) });
  });

  return { mainRows, breakdown };
}

function downloadCsv(filename: string, rows: Record<string, unknown>[]) {
  if (rows.length === 0) return;
  const csv = Papa.unparse(rows);
  const blob = new Blob([csv], { type: "text/csv;charset=utf-8;" });
  const url = URL.createObjectURL(blob);
  const link = document.createElement("a");
  link.href = url;
  link.download = filename;
  document.body.appendChild(link);
  link.click();
  document.body.removeChild(link);
  URL.revokeObjectURL(url);
}

// ---------- Component ----------
export default function EnglishToolkit() {
  // ----- Word input -----
  const [wordInputText, setWordInputText] = useState("");
  const [fileName, setFileName] = useState<string | null>(null);
  const [uploadedWords, setUploadedWords] = useState<string[] | null>(null);

  // ----- allMeasures.csv -----
  const [allMeasuresRows, setAllMeasuresRows] = useState<MeasureRow[]>([]);
  const [allMeasuresError, setAllMeasuresError] = useState<string | null>(null);
  const [allMeasuresLoading, setAllMeasuresLoading] = useState(true);

  useEffect(() => {
    fetch(ALL_MEASURES_CSV_PATH)
      .then(async (res) => {
        const text = await res.text();

        if (!res.ok) {
          setAllMeasuresError(`HTTP ${res.status} fetching ${ALL_MEASURES_CSV_PATH} — the file isn't where the app expects it.`);
          setAllMeasuresLoading(false);
          return;
        }

        const parsed = Papa.parse<MeasureRow>(text, { header: true, skipEmptyLines: true });

        if (parsed.data.length === 0) {
          setAllMeasuresError("CSV fetched but parsed to 0 rows — check the file isn't empty.");
        } else if (!("spelling" in parsed.data[0])) {
          setAllMeasuresError(
            `Fetched a file, but it has no "spelling" column (saw: ${Object.keys(parsed.data[0]).slice(0, 5).join(", ")}). ` +
            `This usually means your dev server returned index.html instead of the CSV.`
          );
        } else {
          setAllMeasuresRows(parsed.data);
        }
        setAllMeasuresLoading(false);
      })
      .catch((err) => {
        setAllMeasuresError(String(err));
        setAllMeasuresLoading(false);
      });
  }, []);

  const allMeasuresIndex = useMemo(() => {
    const map = new Map<string, MeasureRow[]>();
    allMeasuresRows.forEach((row) => {
      const key = (row.spelling ?? "").trim().toLowerCase();
      if (!key) return;
      const arr = map.get(key) ?? [];
      arr.push(row);
      map.set(key, arr);
    });
    return map;
  }, [allMeasuresRows]);

  // ----- Filters -----
  const [filteredMeasures, setFilteredMeasures] = useState<CatalogMeasure[]>([]);
  const [showResults, setShowResults] = useState(false);
  const [showResultsModal, setShowResultsModal] = useState(false);

  const [unitTypes, setUnitTypes] = useState<UnitType[]>([]);
  const [measureTypes, setMeasureTypes] = useState<MeasureType[]>([]);
  const [directionalities, setDirectionalities] = useState<Exclude<Directionality, null>[]>([]);
  const [weightings, setWeightings] = useState<Weighting[]>([]);
  const [multiplePronunciations, setMultiplePronunciations] = useState<"Yes" | "No">("No");

  const [rows, setRows] = useState<TableRowsState>({});

  // ----- Results (snapshotted at the moment "View Results" is clicked) -----
  const [resultColumns, setResultColumns] = useState<ColumnSpec[]>([]);
  const [resultMainRows, setResultMainRows] = useState<MainResultRow[]>([]);
  const [resultBreakdown, setResultBreakdown] = useState<BreakdownRow[]>([]);

  const handleFileChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const file = e.target.files?.[0];
    if (!file) return;
    setFileName(file.name);
    const reader = new FileReader();
    reader.onload = () => {
      const text = String(reader.result ?? "");
      const words = text
        .split(/\r?\n/)
        .map((line) => line.split(",")[0].trim().toLowerCase())
        .filter(Boolean);
      setUploadedWords(words);
    };
    reader.readAsText(file);
  };

  const handleMeasureToggle = (label: string) => {
    setRows((prev) => ({
      ...prev,
      [label]: { ...prev[label], selected: !prev[label].selected },
    }));
  };

  const handleStatToggle = (label: string, stat: keyof TableRowStats) => {
    setRows((prev) => ({
      ...prev,
      [label]: {
        ...prev[label],
        stats: { ...prev[label].stats, [stat]: !prev[label].stats[stat] },
      },
    }));
  };

  // -------- Apply --------
  const handleApply = () => {
    const filtered = (measureCatalog as CatalogMeasure[]).filter(
      (m) =>
        (unitTypes.length === 0 || unitTypes.includes(m.unitType)) &&
        (measureTypes.length === 0 || measureTypes.includes(m.measureType)) &&
        (weightings.length === 0 || weightings.includes(m.weighting)) &&
        (directionalities.length === 0 ||
          m.directionality === null ||
          directionalities.includes(m.directionality as Exclude<Directionality, null>))
    );
    setFilteredMeasures(filtered);

    const next: TableRowsState = {};
    const groups = new Map<string, CatalogMeasure[]>();

    for (const m of filtered) {
      const key = baseKey(m);
      const arr = groups.get(key) ?? [];
      arr.push(m);
      groups.set(key, arr);
    }

    for (const [, arr] of groups) {
      const representative = arr[0];
      const label = makeDisplayLabel(representative);

      const stats: TableRowStats = {
        mean: arr.some((x) => x.stat === "mean"),
        min: arr.some((x) => x.stat === "min"),
        max: arr.some((x) => x.stat === "max"),
        median: arr.some((x) => x.stat === "median"),
        standardDeviation: arr.some((x) => x.stat === "sd"),
      };

      next[label] = {
        id: baseKey(representative),
        measureType: representative.measureType,
        isPercent: representative.measureType === "Consistency",
        selected: true,
        stats,
        description: representative.description,
      };
    }

    setRows(next);
  };

  // -------- View Results --------
  const handleViewResults = () => {
    const words = uploadedWords && uploadedWords.length > 0 ? uploadedWords : parseWordInput(wordInputText);
    const columns = getSelectedColumns(rows);
    const { mainRows, breakdown } = buildResults(words, allMeasuresIndex, columns);

    setResultColumns(columns);
    setResultMainRows(mainRows);
    setResultBreakdown(breakdown);
    setShowResultsModal(true);
  };

  const handleDownloadResults = () => {
    const flatMain = resultMainRows.map((r) => {
      const out: Record<string, unknown> = {
        word: r.word,
        status: r.isHomograph ? "homograph" : r.found ? "ok" : "not found",
      };
      resultColumns.forEach((c) => {
        out[`${c.label} (${STAT_HEADER_LABEL[c.stat]})`] = r.values[`${c.measureId}__${c.stat}`] ?? "-";
      });
      return out;
    });
    downloadCsv("results.csv", flatMain);

    if (resultBreakdown.length > 0) {
      downloadCsv(
        "results_homographs.csv",
        resultBreakdown.map((b) => {
          const out: Record<string, unknown> = {
            spelling: b.spelling,
            pronunciation: b.pronunciation,
          };
          resultColumns.forEach((c) => {
            out[`${c.label} (${STAT_HEADER_LABEL[c.stat]})`] = b.values[`${c.measureId}__${c.stat}`] ?? "-";
          });
          out["Phoneme-Pronunciation Log Frequency (Mean)"] = b.ppMean;
          out["Phoneme-Pronunciation Log Frequency (Min)"] = b.ppMin;
          return out;
        })
      );
    }
  };

  const groupedResultCols = useMemo(() => groupColumnsByLabel(resultColumns), [resultColumns]);
  const flatResultCols = useMemo(() => [...groupedResultCols.values()].flat(), [groupedResultCols]);

  return (
    <main className="main-container">
      <aside className="left-sidebar">
        <div className="input-section">
          <h5 className="section-title">Letter Input</h5>
          <textarea
            className="textarea-input"
            placeholder="Enter input here..."
            rows={4}
            value={wordInputText}
            onChange={(e) => {
              setWordInputText(e.target.value);
              setUploadedWords(null); // typed input takes over from a prior upload
            }}
          />
        </div>
        <div className="input-section">
          <h5 className="section-title">Phoneme Input (optional)</h5>
          <textarea className="textarea-input" placeholder="Enter input here..." rows={4} />
        </div>
        <div className="input-section">
          <h5 className="file-upload-title">
            Upload a csv/txt file containing a list of words in the first column only:
          </h5>
          <div className="file-upload-container">
            <label className="file-upload-label">
              <input
                type="file"
                accept=".csv,.txt"
                className="file-upload-input"
                onChange={handleFileChange}
              />
              <div className="upload-arrow">↑</div>
              <span className="upload-text">Drag and drop files to upload</span>
              <span className="upload-or">or</span>
              <span className="browse-button">Browse</span>
            </label>
          </div>
          {fileName && <p className="file-name">Uploaded: {fileName}</p>}
        </div>
      </aside>

      <section className="right-content">
        <h2 className="main-title">Variables</h2>

        {/* Unit Type */}
        <div className="variable-section">
          <h3 className="variable-title">Unit Type</h3>
          <div className="checkbox-group">
            {(["PG", "OR", "OC", "ONC", "Syllable"] as UnitType[]).map((option) => (
              <label key={option} className="checkbox-item">
                <input
                  type="checkbox"
                  checked={unitTypes.includes(option)}
                  onChange={(e) => {
                    if (e.target.checked) setUnitTypes([...unitTypes, option]);
                    else setUnitTypes(unitTypes.filter((u) => u !== option));
                  }}
                  className="checkbox-input"
                />
                <span>{option}</span>
              </label>
            ))}
          </div>
        </div>

        {/* Measure Type */}
        <div className="variable-section">
          <h3 className="variable-title">Measure Type</h3>
          <div className="checkbox-group">
            {(["Frequency", "Consistency"] as MeasureType[]).map((option) => (
              <label key={option} className="checkbox-item">
                <input
                  type="checkbox"
                  checked={measureTypes.includes(option)}
                  onChange={(e) => {
                    if (e.target.checked) setMeasureTypes([...measureTypes, option]);
                    else setMeasureTypes(measureTypes.filter((m) => m !== option));
                  }}
                  className="checkbox-input"
                />
                <span>{option}</span>
              </label>
            ))}
          </div>
        </div>

        {/* Directionality */}
        <div className="variable-section">
          <h3 className="variable-title">Directionality</h3>
          <div className="checkbox-group">
            {(["Reading", "Spelling"] as Exclude<Directionality, null>[]).map((option) => (
              <label key={option} className="checkbox-item">
                <input
                  type="checkbox"
                  checked={directionalities.includes(option)}
                  onChange={(e) => {
                    if (e.target.checked) setDirectionalities([...directionalities, option]);
                    else setDirectionalities(directionalities.filter((d) => d !== option));
                  }}
                  className="checkbox-input"
                />
                <span>{option}</span>
              </label>
            ))}
          </div>
        </div>

        {/* Weighting */}
        <div className="variable-section">
          <h3 className="variable-title">Weighting</h3>
          <div className="checkbox-group">
            {(["Token", "Type"] as Weighting[]).map((option) => (
              <label key={option} className="checkbox-item">
                <input
                  type="checkbox"
                  checked={weightings.includes(option)}
                  onChange={(e) => {
                    if (e.target.checked) setWeightings([...weightings, option]);
                    else setWeightings(weightings.filter((w) => w !== option));
                  }}
                  className="checkbox-input"
                />
                <span>{option}</span>
              </label>
            ))}
          </div>
        </div>

        {/* Multiple Pronunciations */}
        <div className="variable-section">
          <h3 className="variable-title">Multiple Pronunciations?</h3>
          <div className="radio-group">
            {["Yes", "No"].map((option) => (
              <label key={option} className="radio-item">
                <input
                  type="radio"
                  name="multiplePronunciations"
                  value={option}
                  checked={multiplePronunciations === option}
                  onChange={(e) => setMultiplePronunciations(e.target.value as "Yes" | "No")}
                  className="radio-input"
                />
                <span>{option}</span>
              </label>
            ))}
          </div>
        </div>

        {/* Action Buttons */}
        <div className="action-buttons">
          <button className="action-button" onClick={handleApply}>
            Apply
          </button>
          <button
            className="action-button"
            onClick={handleViewResults}
            disabled={allMeasuresLoading || Object.values(rows).every((r) => !r.selected)}
          >
            {allMeasuresLoading ? "Loading data…" : "View Results"}
          </button>
        </div>

        {allMeasuresError && (
          <div style={{ background: "#fdecea", border: "1px solid #f5c2c0", borderRadius: 8, padding: "12px 16px", margin: "12px 0", color: "#b3261e", fontSize: 14 }}>
            <strong>Couldn't load the measures CSV:</strong> {allMeasuresError}
          </div>
        )}

        {/* Summary Statistics */}
        <div className="statistics-container">
          <div className="statistics-header">
            <h3 className="statistics-title">Select Summary Statistics</h3>
          </div>
          <div className="table-container">
            <table className="statistics-table">
              <thead className="table-header">
                <tr>
                  <th>Measure</th>
                  <th>Mean</th>
                  <th>Min</th>
                  <th>Max</th>
                  <th>Median</th>
                  <th>Standard Deviation</th>
                </tr>
              </thead>
              <tbody>
                {Object.entries(rows).map(([label, data]) => (
                  <tr key={label} className={`table-row ${data.selected ? "selected" : ""}`}>
                    <td className="table-cell">
                      <label className="measure-label" title={data.description ?? ""}>
                        <input
                          type="checkbox"
                          checked={data.selected}
                          onChange={() => handleMeasureToggle(label)}
                          className="checkbox-input"
                        />
                        <span className="measure-text">{label}</span>
                      </label>
                    </td>
                    {(["mean", "min", "max", "median", "standardDeviation"] as (keyof TableRowStats)[]).map(
                      (statKey) => (
                        <td key={statKey} className="table-cell center">
                          <input
                            type="checkbox"
                            checked={data.stats[statKey]}
                            onChange={() => handleStatToggle(label, statKey)}
                            className="checkbox-input"
                          />
                        </td>
                      )
                    )}
                  </tr>
                ))}
                {Object.keys(rows).length === 0 && (
                  <tr>
                    <td className="table-cell" colSpan={6}>
                      Pick variables and click <strong>Apply</strong> to populate measures.
                    </td>
                  </tr>
                )}
              </tbody>
            </table>
          </div>
        </div>

        {/* Filtered Results (catalog debug view) */}
        {showResults && filteredMeasures.length > 0 && (
          <div className="statistics-container">
            <div className="statistics-header">
              <h3 className="statistics-title">Filtered Catalog Results</h3>
            </div>
            <div className="table-container">
              <table className="statistics-table">
                <thead>
                  <tr>
                    <th>ID</th>
                    <th>Label</th>
                    <th>Unit Type</th>
                    <th>Measure Type</th>
                    <th>Directionality</th>
                    <th>Weighting</th>
                    <th>Stat</th>
                  </tr>
                </thead>
                <tbody>
                  {filteredMeasures.map((m) => (
                    <tr key={m.id}>
                      <td>{m.id}</td>
                      <td>{m.label}</td>
                      <td>{m.unitType}</td>
                      <td>{m.measureType}</td>
                      <td>{m.directionality ?? "-"}</td>
                      <td>{m.weighting}</td>
                      <td>{m.stat}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>
        )}
      </section>

      {showResultsModal && (
        <div className="modal-overlay" onClick={() => setShowResultsModal(false)}>
          <div className="results-modal" onClick={(e) => e.stopPropagation()}>
            <div className="modal-header">
              <div>
                <h2>Frequency and Consistency Log Results</h2>
                <p>
                  {resultMainRows.length} word{resultMainRows.length === 1 ? "" : "s"} processed
                </p>
              </div>
              <button className="modal-close" onClick={() => setShowResultsModal(false)}>
                ✕
              </button>
            </div>

            <div className="modal-body">
              {resultColumns.length === 0 ? (
                <p>Select at least one measure and statistic, then click Apply before viewing results.</p>
              ) : (
                <table className="results-table">
                  <thead>
                    <tr>
                      <th rowSpan={2}>Word Inputs</th>
                      {[...groupedResultCols.entries()].map(([label, cols]) => (
                        <th key={label} colSpan={cols.length}>
                          {label}
                        </th>
                      ))}
                    </tr>
                    <tr>
                      {flatResultCols.map((c) => (
                        <th key={`${c.measureId}-${c.stat}`}>{STAT_HEADER_LABEL[c.stat]}</th>
                      ))}
                    </tr>
                  </thead>
                  <tbody>
                    {resultMainRows.map((r, i) => (
                      <tr key={`${r.word}-${i}`}>
                        <td>
                          {i + 1} {r.word}
                        </td>
                        {flatResultCols.map((c, ci) => (
                          <td key={`${c.measureId}-${c.stat}`}>
                            {!r.found
                              ? "not found"
                              : r.isHomograph
                              ? ci === 0
                                ? "homograph"
                                : "-"
                              : r.values[`${c.measureId}__${c.stat}`] ?? "-"}
                          </td>
                        ))}
                      </tr>
                    ))}
                  </tbody>
                </table>
              )}

              {resultBreakdown.length > 0 && (
                <table className="results-table" style={{ marginTop: "1.5rem" }}>
                  <thead>
                    <tr>
                      <th rowSpan={2}>Spelling</th>
                      <th rowSpan={2}>Homographs</th>
                      <th rowSpan={2}>Pronunciation</th>
                      {[...groupedResultCols.entries()].map(([label, cols]) => (
                        <th key={`bd-${label}`} colSpan={cols.length}>
                          {label}
                        </th>
                      ))}
                      <th colSpan={2}>Phoneme-Pronunciation Log Frequency</th>
                    </tr>
                    <tr>
                      {flatResultCols.map((c) => (
                        <th key={`bd-${c.measureId}-${c.stat}`}>{STAT_HEADER_LABEL[c.stat]}</th>
                      ))}
                      <th>Mean</th>
                      <th>Min</th>
                    </tr>
                  </thead>
                  <tbody>
                    {resultBreakdown.map((b, i) => (
                      <tr key={`${b.spelling}-${b.pronunciation}-${i}`}>
                        <td>{b.spelling}</td>
                        <td>{b.homographIndex === 0 ? b.homographCount : ""}</td>
                        <td>{b.pronunciation}</td>
                        {flatResultCols.map((c) => (
                          <td key={`${c.measureId}-${c.stat}`}>{b.values[`${c.measureId}__${c.stat}`] ?? "-"}</td>
                        ))}
                        <td>{b.ppMean}</td>
                        <td>{b.ppMin}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              )}
            </div>

            <div className="modal-footer">
              <button className="action-button" onClick={handleDownloadResults}>
                Download results
              </button>
              <button className="action-button" onClick={() => setShowResultsModal(false)}>
                Back
              </button>
            </div>
          </div>
        </div>
      )}
    </main>
  );
}