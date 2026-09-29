import { useState } from "react";
import { useNavigate } from "react-router-dom";
import "./EnglishToolkit.css";
import measureCatalog from "../measure_catalog_data/measureCatalog.json";
import Layout from "../components/Layout";

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
  { selected: boolean; stats: TableRowStats; description?: string }
>;

type EnglishToolkitProps = {
  mode?: "frequency-consistency" | "phonology-orthography";
};

const toolkitInstructions =
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.";

const phonologyOptionGroups = [
  {
    title: "General",
    options: [
      "Frequency",
      "Contextual Diversity",
      "Familiarity",
      "Age of Acquisition",
    ],
  },
  {
    title: "Orthographic",
    options: [
      "Orthographic Length",
      "Graphotactic Probability",
      "Orthographic Neighborhood",
    ],
  },
  {
    title: "Phonological",
    options: [
      "Phonological Length",
      "Phonotactic Probability",
      "Phonological Neighborhood",
      "First Phoneme",
      "IPA Transcription",
    ],
  },
  {
    title: "Semantic",
    options: [
      "Concreteness",
      "Polysemy",
      "Number of Features",
      "Specific Semantic Features",
      "Affect",
      "Semantic Neighborhood",
      "Vector Representation",
    ],
  },
  {
    title: "Orthography → Phonology",
    options: ["Phonographic Neighborhood", "Consistency"],
  },
  {
    title: "Phonology → Orthography",
    options: ["Phonographic Neighborhood", "Consistency"],
  },
  {
    title: "Morphology",
    options: [
      "Morphological Length",
      "Frequency",
      "Family Size",
      "Percent More Frequent",
      "Affix Length",
      "Affix Productivity",
    ],
  },
  {
    title: "Response Variables",
    options: [
      "Visual Lexical Decision",
      "Auditory Lexical Decision",
      "Reading Aloud",
      "Semantic Decision",
      "Recognition Memory",
    ],
  },
] as const;

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
  // Remove stat suffix from id
  const id = m.id
    .replace(/\.mean$|\.min$|\.max$|\.median$|\.sd$/i, "");

  return id; // every unique measure id (minus stats) is its own group
}

// ---------- Component ----------
export default function EnglishToolkit({
  mode = "frequency-consistency",
}: EnglishToolkitProps) {
  const navigate = useNavigate();
  const [fileName, setFileName] = useState<string | null>(null);
  const [letterInput, setLetterInput] = useState("");
  const [phonemeInput, setPhonemeInput] = useState("");

  // Multi-select state
  const [unitTypes, setUnitTypes] = useState<UnitType[]>([]);
  const [measureTypes, setMeasureTypes] = useState<MeasureType[]>([]);
  const [directionalities, setDirectionalities] = useState<Exclude<Directionality, null>[]>([]);
  const [weightings, setWeightings] = useState<Weighting[]>([]);
  const [multiplePronunciations, setMultiplePronunciations] = useState<"Yes" | "No">("No");
  const [phonologyFormat, setPhonologyFormat] = useState<"standard" | "onset-rime">("standard");
  const [phonologyOptions, setPhonologyOptions] = useState<string[]>([]);

  const [rows, setRows] = useState<TableRowsState>({});

  const handleFileChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    if (e.target.files && e.target.files.length > 0) {
      setFileName(e.target.files[0].name);
    }
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
    if (mode === "phonology-orthography") {
      const next = Object.fromEntries(
        phonologyOptions.map((optionId) => [
          optionId.replace("::", " — "),
          {
            selected: true,
            stats: {
              mean: true,
              min: false,
              max: false,
              median: false,
              standardDeviation: false,
            },
          },
        ]),
      );

      setRows(next);
      return;
    }

    const filtered = (measureCatalog as CatalogMeasure[]).filter((m) =>
      // Unit Type filter
      (unitTypes.length === 0 || unitTypes.includes(m.unitType)) &&

      // Measure Type filter
      (measureTypes.length === 0 || measureTypes.includes(m.measureType)) &&

      // Weighting filter
      (weightings.length === 0 || weightings.includes(m.weighting)) &&

      // Directionality filter: only apply if selected, but allow null (Frequency rows)
      (
        directionalities.length === 0 ||
        m.directionality === null || // allow measures with no directionality (Freq)
        directionalities.includes(m.directionality as Exclude<Directionality, null>)
      )
    );

    

    // Group by base (ignoring stat) and build table rows with stat checkboxes
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
        selected: true,
        stats,
        description: representative.description,
      };
    }
    
    console.log("Final groups:", Object.keys(next).length);
    setRows(next);
  };

  const handleViewResults = () => {
    const selectedMeasures = Object.entries(rows)
      .filter(([, data]) => data.selected)
      .map(([label]) => label);
    const pendingPhonologyMeasures = phonologyOptions.map((optionId) =>
      optionId.replace("::", " — "),
    );
    const searchState = {
      words: letterInput
        .split(/[\n,]+/)
        .map((word) => word.trim())
        .filter(Boolean),
      phonemes: phonemeInput
        .split(/[\n,]+/)
        .map((phoneme) => phoneme.trim())
        .filter(Boolean),
      measures:
        selectedMeasures.length > 0
          ? selectedMeasures
          : mode === "phonology-orthography"
            ? pendingPhonologyMeasures
            : [],
      multiplePronunciations,
    };

    if (mode === "frequency-consistency") {
      navigate("/english/frequency-consistency/results", { state: searchState });
      return;
    }

    navigate(
      phonologyFormat === "onset-rime"
        ? "/english/phonology-orthography/results/onset-rime"
        : "/english/phonology-orthography/results/standard",
      { state: searchState },
    );
  };

  return (
    <Layout
      variant="app"
      pageTitle="How to use this toolkit:"
      pageIntro={toolkitInstructions}
    >
    <main className="main-container">
      <aside className="left-sidebar">
        <div className="input-section">
          <h5 className="section-title">Letter Input</h5>
          <textarea
            className="textarea-input"
            placeholder="Enter input here..."
            rows={4}
            value={letterInput}
            onChange={(event) => setLetterInput(event.target.value)}
          />
        </div>
        <div className="input-section">
          <h5 className="section-title">Phoneme Input (optional)</h5>
          <textarea
            className="textarea-input"
            placeholder="Enter input here..."
            rows={4}
            value={phonemeInput}
            onChange={(event) => setPhonemeInput(event.target.value)}
          />
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

        {mode === "phonology-orthography" && (
          <div className="sidebar-options">
            <div className="input-section">
              <h5 className="section-title">Multiple Pronunciations?</h5>
              <div className="sidebar-radio-row">
                {["Yes", "No"].map((option) => (
                  <label key={option} className="radio-item">
                    <input
                      type="radio"
                      name="multiplePronunciations"
                      value={option}
                      checked={multiplePronunciations === option}
                      onChange={(e) =>
                        setMultiplePronunciations(e.target.value as "Yes" | "No")
                      }
                      className="radio-input"
                    />
                    <span>{option}</span>
                  </label>
                ))}
              </div>
            </div>

            <div className="input-section">
              <h5 className="section-title">Sort by Onset and Rime?</h5>
              <div className="sidebar-radio-row">
                {[
                  ["onset-rime", "Yes"],
                  ["standard", "No"],
                ].map(([value, label]) => (
                  <label key={value} className="radio-item">
                    <input
                      type="radio"
                      name="phonologyFormat"
                      value={value}
                      checked={phonologyFormat === value}
                      onChange={() =>
                        setPhonologyFormat(value as "standard" | "onset-rime")
                      }
                      className="radio-input"
                    />
                    <span>{label}</span>
                  </label>
                ))}
              </div>
            </div>
          </div>
        )}
      </aside>

      <section className="right-content">
        <h2 className="main-title">Variables</h2>

        {mode === "frequency-consistency" ? (
          <>
            <div className="variable-section">
              <h3 className="variable-title">Unit Type</h3>
              <div className="checkbox-group">
                {(["PG", "OR", "OC", "ONC", "Syllable"] as UnitType[]).map((option) => (
                  <label key={option} className="checkbox-item">
                    <input
                      type="checkbox"
                      checked={unitTypes.includes(option)}
                      onChange={(e) =>
                        setUnitTypes(
                          e.target.checked
                            ? [...unitTypes, option]
                            : unitTypes.filter((item) => item !== option),
                        )
                      }
                      className="checkbox-input"
                    />
                    <span>{option}</span>
                  </label>
                ))}
              </div>
            </div>

            <div className="variable-section">
              <h3 className="variable-title">Measure Type</h3>
              <div className="checkbox-group">
                {(["Frequency", "Consistency"] as MeasureType[]).map((option) => (
                  <label key={option} className="checkbox-item">
                    <input
                      type="checkbox"
                      checked={measureTypes.includes(option)}
                      onChange={(e) =>
                        setMeasureTypes(
                          e.target.checked
                            ? [...measureTypes, option]
                            : measureTypes.filter((item) => item !== option),
                        )
                      }
                      className="checkbox-input"
                    />
                    <span>{option}</span>
                  </label>
                ))}
              </div>
            </div>

            <div className="variable-section">
              <h3 className="variable-title">Directionality</h3>
              <div className="checkbox-group">
                {(["Reading", "Spelling"] as Exclude<Directionality, null>[]).map((option) => (
                  <label key={option} className="checkbox-item">
                    <input
                      type="checkbox"
                      checked={directionalities.includes(option)}
                      onChange={(e) =>
                        setDirectionalities(
                          e.target.checked
                            ? [...directionalities, option]
                            : directionalities.filter((item) => item !== option),
                        )
                      }
                      className="checkbox-input"
                    />
                    <span>{option}</span>
                  </label>
                ))}
              </div>
            </div>

            <div className="variable-section">
              <h3 className="variable-title">Weighting</h3>
              <div className="checkbox-group">
                {(["Token", "Type"] as Weighting[]).map((option) => (
                  <label key={option} className="checkbox-item">
                    <input
                      type="checkbox"
                      checked={weightings.includes(option)}
                      onChange={(e) =>
                        setWeightings(
                          e.target.checked
                            ? [...weightings, option]
                            : weightings.filter((item) => item !== option),
                        )
                      }
                      className="checkbox-input"
                    />
                    <span>{option}</span>
                  </label>
                ))}
              </div>
            </div>

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
                      onChange={(e) =>
                        setMultiplePronunciations(e.target.value as "Yes" | "No")
                      }
                      className="radio-input"
                    />
                    <span>{option}</span>
                  </label>
                ))}
              </div>
            </div>
          </>
        ) : (
          <div className="measure-option-groups">
            {phonologyOptionGroups.map((group) => (
              <fieldset className="measure-option-group" key={group.title}>
                <legend>{group.title}</legend>
                <div className="measure-option-list">
                  {group.options.map((option) => {
                    const optionId = `${group.title}::${option}`;

                    return (
                      <label className="checkbox-item" key={optionId}>
                        <input
                          type="checkbox"
                          checked={phonologyOptions.includes(optionId)}
                          onChange={(e) =>
                            setPhonologyOptions(
                              e.target.checked
                                ? [...phonologyOptions, optionId]
                                : phonologyOptions.filter((item) => item !== optionId),
                            )
                          }
                          className="checkbox-input"
                        />
                        <span>{option}</span>
                      </label>
                    );
                  })}
                </div>
              </fieldset>
            ))}
          </div>
        )}

        {/* Action Buttons */}
        <div className="action-buttons">
          <button className="action-button" onClick={handleApply}>Apply</button>
          <button className="action-button" onClick={handleViewResults}>View Results</button>
        </div>

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
                    {(["mean", "min", "max", "median", "standardDeviation"] as (keyof TableRowStats)[])
                      .map((statKey) => (
                        <td key={statKey} className="table-cell center">
                          <input
                            type="checkbox"
                            checked={data.stats[statKey]}
                            onChange={() => handleStatToggle(label, statKey)}
                            className="checkbox-input"
                          />
                        </td>
                    ))}
                  </tr>
                ))}
                {Object.keys(rows).length === 0 && (
                  <tr>
                    <td className="table-cell" colSpan={6}>
                      Choose options and click <strong>Apply</strong> to populate measures.
                    </td>
                  </tr>
                )}
              </tbody>
            </table>
          </div>
        </div>
      </section>
    </main>
    </Layout>
  );
}
