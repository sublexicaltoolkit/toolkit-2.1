import { useState } from "react";
import "./EnglishToolkit.css";

export default function EnglishToolkit() {
  const [fileName, setFileName] = useState<string | null>(null);
  const [unitType, setUnitType] = useState("OC");
  const [measureType, setMeasureType] = useState("Consistency");
  const [directionality, setDirectionality] = useState("Spelling");
  const [weighting, setWeighting] = useState("Type-weighted");
  const [multiplePronunciations, setMultiplePronunciations] = useState("No");

  // Each measure row has its own stats object
  const [measures, setMeasures] = useState<
    Record<string, { selected: boolean; stats: Record<string, boolean> }>
  >({
    "OC Spelling Consistency": {
      selected: true,
      stats: { mean: true, min: true, max: true, median: true, standardDeviation: true },
    },
    "OC Reading Consistency": {
      selected: false,
      stats: { mean: false, min: false, max: false, median: false, standardDeviation: false },
    },
    "OC PG Unit Freq": {
      selected: true,
      stats: { mean: true, min: true, max: true, median: true, standardDeviation: true },
    },
    "OC Grapheme Freq": {
      selected: false,
      stats: { mean: false, min: false, max: false, median: false, standardDeviation: false },
    },
    "OC Phoneme Freq": {
      selected: false,
      stats: { mean: false, min: false, max: false, median: false, standardDeviation: false },
    },
  });

  const handleFileChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    if (e.target.files && e.target.files.length > 0) {
      setFileName(e.target.files[0].name);
    }
  };

  const handleMeasureToggle = (measure: string) => {
    setMeasures((prev) => ({
      ...prev,
      [measure]: { ...prev[measure], selected: !prev[measure].selected },
    }));
  };

  const handleStatToggle = (measure: string, stat: string) => {
    setMeasures((prev) => ({
      ...prev,
      [measure]: {
        ...prev[measure],
        stats: { ...prev[measure].stats, [stat]: !prev[measure].stats[stat] },
      },
    }));
  };

  return (
    <main className="main-container">
      {/* Left Column - fixed width vertical section */}
      <aside className="left-sidebar">
        {/* Letter Input */}
        <div className="input-section">
          <h5 className="section-title">Letter Input</h5>
          <textarea
            className="textarea-input"
            placeholder="Enter input here..."
            rows={4}
          />
        </div>

        {/* Phoneme Input */}
        <div className="input-section">
          <h5 className="section-title">Phoneme Input (optional)</h5>
          <textarea
            className="textarea-input"
            placeholder="Enter input here..."
            rows={4}
          />
        </div>

        {/* File Upload */}
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

      {/* Right Column - main content */}
      <section className="right-content">
        <h2 className="main-title">Variables</h2>

        {/* Unit Type */}
        <div className="variable-section">
          <h3 className="variable-title">Unit Type</h3>
          <div className="radio-group">
            {[
              { value: "PG", label: "Phoneme-Grapheme (PG)" },
              { value: "OR", label: "Onset-Rime (OR)" },
              { value: "OC", label: "Onset-Coda (OC)" },
              { value: "ONC", label: "Onset-Nucleus-Coda (ONC)" },
              { value: "Syllable", label: "Syllable" },
            ].map((option) => (
              <label key={option.value} className="radio-item">
                <input
                  type="radio"
                  name="unitType"
                  value={option.value}
                  checked={unitType === option.value}
                  onChange={(e) => setUnitType(e.target.value)}
                  className="radio-input"
                />
                <span>{option.label}</span>
              </label>
            ))}
          </div>
        </div>

        {/* Measure Type + Directionality */}
        <div className="variable-section">
          <h3 className="variable-title">Measure Type</h3>
          <div className="radio-group">
            {["Frequency", "Consistency"].map((option) => (
              <label key={option} className="radio-item">
                <input
                  type="radio"
                  name="measureType"
                  value={option}
                  checked={measureType === option}
                  onChange={(e) => setMeasureType(e.target.value)}
                  className="radio-input"
                />
                <span>{option}</span>
              </label>
            ))}
          </div>

          <div className="directionality-section">
            <h4 className="directionality-title">Directionality</h4>
            <div className="radio-group">
              {[
                { value: "Reading", label: "Reading (Grapheme → Phoneme)" },
                { value: "Spelling", label: "Spelling (Phoneme → Grapheme)" },
              ].map((option) => (
                <label key={option.value} className="radio-item">
                  <input
                    type="radio"
                    name="directionality"
                    value={option.value}
                    checked={directionality === option.value}
                    onChange={(e) => setDirectionality(e.target.value)}
                    className="radio-input"
                  />
                  <span>{option.label}</span>
                </label>
              ))}
            </div>
          </div>
        </div>

        {/* Weighting */}
        <div className="variable-section">
          <h3 className="variable-title">Weighting</h3>
          <div className="radio-group">
            {["Token-weighted", "Type-weighted"].map((option) => (
              <label key={option} className="radio-item">
                <input
                  type="radio"
                  name="weighting"
                  value={option}
                  checked={weighting === option}
                  onChange={(e) => setWeighting(e.target.value)}
                  className="radio-input"
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
                  onChange={(e) => setMultiplePronunciations(e.target.value)}
                  className="radio-input"
                />
                <span>{option}</span>
              </label>
            ))}
          </div>
        </div>

        {/* Action Buttons */}
        <div className="action-buttons">
          <button className="action-button">Apply</button>
          <button className="action-button">View Results</button>
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
                {Object.entries(measures).map(([measure, data]) => (
                  <tr key={measure} className={`table-row ${data.selected ? "selected" : ""}`}>
                    <td className="table-cell">
                      <label className="measure-label">
                        <input
                          type="checkbox"
                          checked={data.selected}
                          onChange={() => handleMeasureToggle(measure)}
                          className="checkbox-input"
                        />
                        <span className="measure-text">{measure}</span>
                      </label>
                    </td>
                    {Object.keys(data.stats).map((stat) => (
                      <td key={stat} className="table-cell center">
                        <input
                          type="checkbox"
                          checked={data.stats[stat]}
                          onChange={() => handleStatToggle(measure, stat)}
                          className="checkbox-input"
                        />
                      </td>
                    ))}
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>
      </section>
    </main>
  );
}