import type { WordEntry } from "../data/wordData";
import {
  hasFinalE,
  splitOnsetRime,
  splitSyllables,
  whichMin,
  type Grain,
  type MappingUnit,
  type UnitValue,
} from "../data/wordMapping";

// The Phonology & Orthography result cards from the Figma: entered data, average
// results, the per-syllable phonology/orthography grids, and calculated results.

type WordBreakdownProps = {
  entries: WordEntry[];
  selectedIndex: number;
  onSelect: (index: number) => void;
  multiplePronunciations: "Yes" | "No";
  sortByOnsetRime: boolean;
};

type ResultMeasure = { key: UnitValue; label: string; percent: boolean };

const MEASURES: ResultMeasure[] = [
  { key: "PG", label: "p(G|P)", percent: true },
  { key: "GP", label: "p(P|G)", percent: true },
  { key: "PG_freq", label: "PG Freq", percent: false },
  { key: "P_freq", label: "P Freq", percent: false },
  { key: "G_freq", label: "G Freq", percent: false },
];

// Sorting by onset and rime switches to the onset-rime grain, which the Figma
// shows with the two consistency measures only.
const ONSET_RIME_MEASURES = MEASURES.slice(0, 2);

// The Figma grids always show at least six syllables and six units.
const MIN_ROWS = 6;
const MIN_UNITS = 6;
const MIN_ONSETS = 3;
const MIN_CODAS = 3;

function formatMeasure(value: number | null | undefined, percent: boolean): string {
  if (value === null || value === undefined) return "—";
  return percent ? `${(value * 100).toFixed(2)}%` : value.toFixed(3);
}

function capitalize(word: string): string {
  return word.charAt(0).toUpperCase() + word.slice(1);
}

function range(length: number): number[] {
  return Array.from({ length }, (_, index) => index);
}

function Card({
  title,
  tone,
  className,
  children,
}: {
  title: string;
  tone: "blue" | "slate";
  className?: string;
  children: React.ReactNode;
}) {
  return (
    <section className={`breakdown-card breakdown-card-${tone}${className ? ` ${className}` : ""}`}>
      <h3 className="breakdown-card-title">{title}</h3>
      {children}
    </section>
  );
}

function measureValue(entry: WordEntry, grain: Grain, key: UnitValue, stat: "mean" | "min") {
  // Word-level summaries come from all_measures.csv, e.g. "PG_default_GP.min"
  return entry.measures[`${grain}_default_${key}.${stat}`] ?? null;
}

function ResultsTable({
  entry,
  grain,
  measures,
  units,
}: {
  entry: WordEntry;
  grain: Grain;
  measures: ResultMeasure[];
  units?: MappingUnit[] | null;
}) {
  return (
    <table className="breakdown-table">
      <thead>
        <tr>
          <th aria-label="Statistic" />
          {measures.map((measure) => (
            <th key={measure.key}>{measure.label}</th>
          ))}
        </tr>
      </thead>
      <tbody>
        {(["mean", "min"] as const).map((stat) => (
          <tr key={stat}>
            <th scope="row">{stat === "mean" ? "Mean" : "Min"}</th>
            {measures.map((measure) => (
              <td key={measure.key}>
                {formatMeasure(measureValue(entry, grain, measure.key, stat), measure.percent)}
              </td>
            ))}
          </tr>
        ))}
        {units !== undefined && (
          <tr>
            <th scope="row">Which Min</th>
            {measures.map((measure) => (
              <td key={measure.key}>{(units && whichMin(units, measure.key)) ?? "—"}</td>
            ))}
          </tr>
        )}
      </tbody>
    </table>
  );
}

function StandardGrids({ syllables }: { syllables: MappingUnit[][] }) {
  const rows = range(Math.max(MIN_ROWS, syllables.length));
  const columns = range(Math.max(MIN_UNITS, ...syllables.map((syllable) => syllable.length)));

  return (
    <>
      <Card title="Phonology" tone="slate" className="breakdown-grid">
        <table className="breakdown-table">
          <thead>
            <tr>
              <th aria-label="Syllable" />
              {columns.map((column) => (
                <th key={column}>P{column + 1}</th>
              ))}
            </tr>
          </thead>
          <tbody>
            {rows.map((row) => (
              <tr key={row}>
                <th scope="row">Syllable {row + 1}</th>
                {columns.map((column) => (
                  <td key={column}>{syllables[row]?.[column]?.phoneme ?? ""}</td>
                ))}
              </tr>
            ))}
          </tbody>
        </table>
      </Card>

      <Card title="Orthography" tone="slate" className="breakdown-grid">
        <table className="breakdown-table">
          <thead>
            <tr>
              <th aria-label="Syllable" />
              {columns.map((column) => (
                <th key={column}>G{column + 1}</th>
              ))}
              <th>Final E?</th>
            </tr>
          </thead>
          <tbody>
            {rows.map((row) => (
              <tr key={row}>
                <th scope="row">Syllable {row + 1}</th>
                {columns.map((column) => (
                  <td key={column}>{syllables[row]?.[column]?.grapheme ?? ""}</td>
                ))}
                <td>{syllables[row] ? (hasFinalE(syllables[row]) ? "Yes" : "No") : ""}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </Card>
    </>
  );
}

function OnsetRimeGrids({ syllables }: { syllables: MappingUnit[][] }) {
  const split = syllables.map(splitOnsetRime);
  const rows = range(Math.max(MIN_ROWS, syllables.length));
  const onsets = range(Math.max(MIN_ONSETS, ...split.map((part) => part.onset.length)));
  const codas = range(Math.max(MIN_CODAS, ...split.map((part) => part.coda.length)));
  const graphemes = (units: (MappingUnit | null)[]) =>
    units.map((unit) => unit?.grapheme ?? "").join("");

  return (
    <>
      <Card title="Phonology" tone="slate" className="breakdown-grid">
        <table className="breakdown-table">
          <thead>
            <tr className="breakdown-group-row">
              <th aria-label="Syllable" />
              <th colSpan={onsets.length}>Onset</th>
              <th colSpan={codas.length + 1}>Rime</th>
            </tr>
            <tr>
              <th aria-label="Syllable" />
              {onsets.map((index) => (
                <th key={`onset-${index}`}>Onset{index + 1}</th>
              ))}
              <th className="breakdown-rime-start">Nucleus</th>
              {codas.map((index) => (
                <th key={`coda-${index}`}>Coda{index + 1}</th>
              ))}
            </tr>
          </thead>
          <tbody>
            {rows.map((row) => (
              <tr key={row}>
                <th scope="row">Syllable {row + 1}</th>
                {onsets.map((index) => (
                  <td key={`onset-${index}`}>{split[row]?.onset[index]?.phoneme ?? ""}</td>
                ))}
                <td className="breakdown-rime-start">{split[row]?.nucleus?.phoneme ?? ""}</td>
                {codas.map((index) => (
                  <td key={`coda-${index}`}>{split[row]?.coda[index]?.phoneme ?? ""}</td>
                ))}
              </tr>
            ))}
          </tbody>
        </table>
      </Card>

      <Card title="Orthography" tone="slate" className="breakdown-grid breakdown-grid-narrow">
        <table className="breakdown-table">
          <thead>
            <tr>
              <th aria-label="Syllable" />
              <th>Onset</th>
              <th>Rime</th>
            </tr>
          </thead>
          <tbody>
            {rows.map((row) => (
              <tr key={row}>
                <th scope="row">Syllable {row + 1}</th>
                <td>{split[row] ? graphemes(split[row].onset) : ""}</td>
                <td>{split[row] ? graphemes([split[row].nucleus, ...split[row].coda]) : ""}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </Card>
    </>
  );
}

export default function WordBreakdown({
  entries,
  selectedIndex,
  onSelect,
  multiplePronunciations,
  sortByOnsetRime,
}: WordBreakdownProps) {
  const entry = entries[selectedIndex] ?? entries[0];
  if (!entry) return null;

  const grain: Grain = sortByOnsetRime ? "OR" : "PG";
  const measures = sortByOnsetRime ? ONSET_RIME_MEASURES : MEASURES;
  // Syllables always come from the phoneme-grapheme units; "which min" uses the grain's units.
  const syllables = entry.mapping.PG ? splitSyllables(entry.mapping.PG) : [];
  const word = `“${capitalize(entry.spelling)}”`;

  return (
    <div className="word-breakdown">
      <div className="breakdown-summary-row">
        <Card title="Entered Data" tone="blue" className="breakdown-entered">
          <dl className="breakdown-entered-list">
            <div>
              <dt>Word Input Spelling:</dt>
              <dd>
                {entries.length > 1 ? (
                  <select
                    className="breakdown-word-select"
                    aria-label="Word to show"
                    value={selectedIndex}
                    onChange={(event) => onSelect(Number(event.target.value))}
                  >
                    {entries.map((option, index) => (
                      <option key={`${option.spelling}-${option.pronunciation}`} value={index}>
                        {option.spelling} /{option.pronunciation}/
                      </option>
                    ))}
                  </select>
                ) : (
                  entry.spelling
                )}
              </dd>
            </div>
            <div>
              <dt>Multiple Pronunciations?:</dt>
              <dd>{multiplePronunciations}</dd>
            </div>
            <div>
              <dt>Sort by Onset and Rime?:</dt>
              <dd>{sortByOnsetRime ? "Yes" : "No"}</dd>
            </div>
          </dl>
        </Card>

        <span className="breakdown-arrow" aria-hidden="true">⇨</span>

        <Card title={`${word} Average Results`} tone="blue" className="breakdown-average">
          <ResultsTable entry={entry} grain={grain} measures={measures} />
        </Card>
      </div>

      {entry.mapping.PG === null ? (
        <p className="results-not-found">
          The toolkit could not map “{entry.spelling}” /{entry.pronunciation}/ into units, so
          there is no syllable breakdown for it.
        </p>
      ) : sortByOnsetRime ? (
        <OnsetRimeGrids syllables={syllables} />
      ) : (
        <StandardGrids syllables={syllables} />
      )}

      <Card title={`${word} Calculated Results`} tone="blue" className="breakdown-calculated">
        <ResultsTable
          entry={entry}
          grain={grain}
          measures={measures}
          units={entry.mapping[grain]}
        />
      </Card>
    </div>
  );
}
