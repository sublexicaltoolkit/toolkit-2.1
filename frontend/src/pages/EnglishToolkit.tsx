import { useState } from "react";
import { useNavigate } from "react-router-dom";
import "./EnglishToolkit.css";
import Layout from "../components/Layout";
import { parseWordFile } from "../data/csvParsing";
import {
  MEASURE_GROUPS,
  STATS,
  columnName,
  type Directionality,
  type MeasureType,
  type Stat,
  type UnitType,
  type Weighting,
} from "../data/measureColumns";

// ---------- Types ----------
type TableRowStats = Record<Stat, boolean>;

// Keyed by measure column base (e.g. "PG_default_PG")
type TableRowsState = Record<
  string,
  { label: string; selected: boolean; stats: TableRowStats; description?: string }
>;

type EnglishToolkitProps = {
  mode?: "frequency-consistency" | "phonology-orthography";
};

const unitTypeOptions: { value: UnitType; label: string }[] = [
  { value: "PG", label: "Phoneme-Grapheme (PG)" },
  { value: "OR", label: "Onset-Rime (OR)" },
  { value: "OC", label: "Onset-Coda (OC)" },
  { value: "ONC", label: "Onset-Nucleus-Coda (ONC)" },
];

const directionOptions: { value: Directionality; label: string; detail: string }[] = [
  { value: "Reading", label: "Reading", detail: "(Grapheme → Phoneme)" },
  { value: "Spelling", label: "Spelling", detail: "(Phoneme → Grapheme)" },
];

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
      "Graphotactic Probabilities",
      "Orthographic Neighborhood",
    ],
  },
  {
    title: "Phonological",
    options: [
      "Phonological Length",
      "Phonotactic Probabilities",
      "Phonological Neighborhood",
      "First Phoneme",
      "IPA Transcription",
    ],
  },
  {
    title: "Semantic",
    options: [
      "Concreteness/Imageability",
      "Polysemy",
      "Number of Features",
      "Specific Semantic Features",
      "Affect",
      "Semantic Neighborhood",
      "Vector Representation",
    ],
  },
  {
    title: "Ortho-Phon",
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
      "Visual Lexicon Decision",
      "Auditory Lexical Decision",
      "Reading Aloud",
      "Semantic Decision",
      "Recognition Memory",
    ],
  },
] as const;

// ---------- Component ----------
export default function EnglishToolkit({
  mode = "frequency-consistency",
}: EnglishToolkitProps) {
  const navigate = useNavigate();
  const [fileName, setFileName] = useState<string | null>(null);
  const [uploadedWords, setUploadedWords] = useState<string[]>([]);
  const [uploadError, setUploadError] = useState<string | null>(null);
  const [isDragging, setIsDragging] = useState(false);
  const [letterInput, setLetterInput] = useState("");
  const [phonemeInput, setPhonemeInput] = useState("");

  // Multi-select state
  const [unitTypes, setUnitTypes] = useState<UnitType[]>([]);
  const [measureTypes, setMeasureTypes] = useState<MeasureType[]>([]);
  const [directionalities, setDirectionalities] = useState<Directionality[]>([]);
  const [weightings, setWeightings] = useState<Weighting[]>([]);
  const [multiplePronunciations, setMultiplePronunciations] = useState<"Yes" | "No">("No");
  const [phonologyFormat, setPhonologyFormat] = useState<"standard" | "onset-rime">("standard");
  const [phonologyOptions, setPhonologyOptions] = useState<string[]>([]);

  const [rows, setRows] = useState<TableRowsState>({});

  const handleFile = (file: File) => {
    setFileName(file.name);
    setUploadedWords([]);
    setUploadError(null);
    parseWordFile(file)
      .then((words) => {
        setUploadedWords(words);
        if (words.length === 0) setUploadError("No words found in the first column.");
      })
      .catch((err: Error) => setUploadError(`Could not read file: ${err.message}`));
  };

  const handleFileChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    if (e.target.files && e.target.files.length > 0) {
      handleFile(e.target.files[0]);
    }
    // Allow picking the same file again after removing it
    e.target.value = "";
  };

  const handleFileDrop = (e: React.DragEvent<HTMLLabelElement>) => {
    e.preventDefault();
    setIsDragging(false);
    const file = e.dataTransfer.files[0];
    if (file) handleFile(file);
  };

  const handleRemoveFile = () => {
    setFileName(null);
    setUploadedWords([]);
    setUploadError(null);
  };

  const handleMeasureToggle = (key: string) => {
    setRows((prev) => ({
      ...prev,
      [key]: { ...prev[key], selected: !prev[key].selected },
    }));
  };

  const handleStatToggle = (key: string, stat: Stat) => {
    setRows((prev) => ({
      ...prev,
      [key]: {
        ...prev[key],
        stats: { ...prev[key].stats, [stat]: !prev[key].stats[stat] },
      },
    }));
  };

  // -------- Apply --------
  const handleApply = () => {
    const filtered = MEASURE_GROUPS.filter((m) =>
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
        directionalities.includes(m.directionality)
      )
    );

    // One table row per measure, with a checkbox for each summary statistic
    const next: TableRowsState = {};
    for (const m of filtered) {
      next[m.base] = {
        label: m.label,
        selected: true,
        stats: { mean: true, min: false },
        description: m.base,
      };
    }
    setRows(next);
  };

  const handleViewResults = () => {
    // Column names in all_measures.csv, e.g. "PG_default_PG.mean"
    const selectedMeasures = Object.entries(rows)
      .filter(([, data]) => data.selected)
      .flatMap(([base, data]) =>
        STATS.filter((stat) => data.stats[stat]).map((stat) => columnName(base, stat)),
      );
    const pendingPhonologyMeasures = phonologyOptions.map((optionId) =>
      optionId.replace("::", " — "),
    );
    const searchState = {
      words: [
        ...letterInput
          .split(/[\n,]+/)
          .map((word) => word.trim())
          .filter(Boolean),
        ...uploadedWords,
      ],
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
      pageClassName={mode === "phonology-orthography" ? "page-phonology" : undefined}
      pageTitle="How to use this toolkit:"
      pageIntro={toolkitInstructions}
    >
    <main className={`main-container${mode === "frequency-consistency" ? " toolkit-frequency" : " toolkit-phonology"}`}>
      <aside className="left-sidebar">
        <div className="input-section">
          <h5 className="section-title">
            {mode === "phonology-orthography" ? "Input" : "Letter Input"}
          </h5>
          <textarea
            className="textarea-input"
            placeholder="Enter input here..."
            rows={4}
            value={letterInput}
            onChange={(event) => setLetterInput(event.target.value)}
          />
        </div>
        {mode === "frequency-consistency" && (
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
        )}
        <div className="input-section">
          <h5 className="file-upload-title">
            Upload a csv/txt file containing a list of words in the first column only:
          </h5>
          <div className="file-upload-container">
            <label
              className={`file-upload-label${isDragging ? " dragging" : ""}`}
              onDragOver={(e) => {
                e.preventDefault();
                setIsDragging(true);
              }}
              onDragLeave={() => setIsDragging(false)}
              onDrop={handleFileDrop}
            >
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
          {fileName && (
            <p className="file-name">
              Uploaded: {fileName}
              {uploadedWords.length > 0 &&
                ` (${uploadedWords.length} ${uploadedWords.length === 1 ? "word" : "words"})`}
              <button type="button" className="file-remove" onClick={handleRemoveFile}>
                Remove
              </button>
            </p>
          )}
          {uploadError && <p className="file-error">{uploadError}</p>}
        </div>

        {mode === "phonology-orthography" && (
          <div className="sidebar-options">
            <div className="input-section">
              <h5 className="section-title">
                <span className="required-mark">*</span>Multiple Pronunciations?
              </h5>
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
              <h5 className="section-title">
                <span className="required-mark">*</span>Sort By Onset and Rime?
              </h5>
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
        <div className="right-body">
        <h2 className="main-title">Variables</h2>

        {mode === "frequency-consistency" ? (
          <>
            <div className="variable-section">
              <h3 className="variable-title">Unit Type</h3>
              <div className="checkbox-group">
                {unitTypeOptions.map((option) => (
                  <label key={option.value} className="checkbox-item">
                    <input
                      type="checkbox"
                      checked={unitTypes.includes(option.value)}
                      onChange={(e) =>
                        setUnitTypes(
                          e.target.checked
                            ? [...unitTypes, option.value]
                            : unitTypes.filter((item) => item !== option.value),
                        )
                      }
                      className="checkbox-input"
                    />
                    <span>{option.label}</span>
                  </label>
                ))}
              </div>
            </div>

            <div className="variable-row">
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
            </div>

            <div className="variable-section">
              <h3 className="variable-title">Directionality</h3>
              <div className="checkbox-group">
                {directionOptions.map((option) => (
                  <label key={option.value} className="checkbox-item">
                    <input
                      type="checkbox"
                      checked={directionalities.includes(option.value)}
                      onChange={(e) =>
                        setDirectionalities(
                          e.target.checked
                            ? [...directionalities, option.value]
                            : directionalities.filter((item) => item !== option.value),
                        )
                      }
                      className="checkbox-input"
                    />
                    <span>{option.label}</span>
                    <span className="option-detail">{option.detail}</span>
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
          </>
        ) : (
          <div className="phonology-variables">
            {phonologyOptionGroups.map((group) => (
              <section className="variable-section" key={group.title}>
                <h3 className="variable-title">{group.title}</h3>
                <div className="checkbox-group">
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
              </section>
            ))}
          </div>
        )}

        {mode === "frequency-consistency" ? (
          <>
        <div className="action-buttons">
          <button className="action-button" onClick={handleApply}>Apply</button>
          <button className="action-button" onClick={handleViewResults}>View Results</button>
        </div>

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
                </tr>
              </thead>
              <tbody>
                {Object.entries(rows).map(([key, data]) => (
                  <tr key={key} className={`table-row ${data.selected ? "selected" : ""}`}>
                    <td className="table-cell">
                      <label className="measure-label" title={data.description ?? ""}>
                        <input
                          type="checkbox"
                          checked={data.selected}
                          onChange={() => handleMeasureToggle(key)}
                          className="checkbox-input"
                        />
                        <span className="measure-text">{data.label}</span>
                      </label>
                    </td>
                    {STATS.map((statKey) => (
                      <td key={statKey} className="table-cell center">
                        <input
                          type="checkbox"
                          checked={data.stats[statKey]}
                          onChange={() => handleStatToggle(key, statKey)}
                          className="checkbox-input"
                        />
                      </td>
                    ))}
                  </tr>
                ))}
                {Object.keys(rows).length === 0 && (
                  <tr>
                    <td className="table-cell" colSpan={STATS.length + 1}>
                      Choose options and click <strong>Apply</strong> to populate measures.
                    </td>
                  </tr>
                )}
              </tbody>
            </table>
          </div>
        </div>
          </>
        ) : (
          <div className="action-buttons submit-row">
            <button className="action-button" type="button" onClick={handleViewResults}>
              Submit
            </button>
          </div>
        )}
        </div>
      </section>
    </main>
    </Layout>
  );
}
