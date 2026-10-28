// Decodes the all_measures.csv column names, following the professor's notes:
//   <unit>_<variant>_<target>.<stat>
//   unit:    PG | ONC | OC | OR
//   variant: default | noposition | freq | freq_noposition
//            ("freq" = token weighted, otherwise type; "noposition" ignores syllable position)
//   target:  PG | GP            -> consistency (directional)
//            PG_freq | G_freq | P_freq -> frequency (bidirectional)
//   stat:    only mean and min are shown

export type UnitType = "PG" | "OR" | "OC" | "ONC";
export type MeasureType = "Frequency" | "Consistency";
export type Directionality = "Reading" | "Spelling";
export type Weighting = "Token" | "Type";
export type Stat = "mean" | "min";

export type MeasureGroup = {
  // Column name without the stat, e.g. "PG_default_PG"
  base: string;
  unitType: UnitType;
  measureType: MeasureType;
  directionality: Directionality | null;
  weighting: Weighting;
  positional: boolean;
  label: string;
};

export const STATS: Stat[] = ["mean", "min"];

const UNITS: UnitType[] = ["PG", "ONC", "OC", "OR"];

const VARIANTS: { key: string; weighting: Weighting; positional: boolean }[] = [
  { key: "default", weighting: "Type", positional: true },
  { key: "noposition", weighting: "Type", positional: false },
  { key: "freq", weighting: "Token", positional: true },
  { key: "freq_noposition", weighting: "Token", positional: false },
];

// Per the professor's notes "P->G reading, G->P spelling".
// TODO: confirm with the professor; the CSV descriptions read the other way round.
const CONSISTENCY_TARGETS: Record<string, Directionality> = {
  PG: "Reading",
  GP: "Spelling",
};

const FREQUENCY_TARGETS: Record<string, string> = {
  PG_freq: "Phoneme-Grapheme",
  G_freq: "Grapheme",
  P_freq: "Phoneme",
};

function describe(
  unit: UnitType,
  variant: (typeof VARIANTS)[number],
  detail: string,
): string {
  const position = variant.positional ? "Positional" : "Non-positional";
  return `${unit} ${detail} (${variant.weighting}, ${position})`;
}

export const MEASURE_GROUPS: MeasureGroup[] = UNITS.flatMap((unitType) =>
  VARIANTS.flatMap((variant) => [
    ...Object.entries(CONSISTENCY_TARGETS).map(([target, directionality]) => ({
      base: `${unitType}_${variant.key}_${target}`,
      unitType,
      measureType: "Consistency" as const,
      directionality,
      weighting: variant.weighting,
      positional: variant.positional,
      label: describe(unitType, variant, `${directionality} Consistency`),
    })),
    ...Object.entries(FREQUENCY_TARGETS).map(([target, name]) => ({
      base: `${unitType}_${variant.key}_${target}`,
      unitType,
      measureType: "Frequency" as const,
      directionality: null,
      weighting: variant.weighting,
      positional: variant.positional,
      label: describe(unitType, variant, `${name} Frequency`),
    })),
  ]),
);

export function columnName(base: string, stat: Stat): string {
  return `${base}.${stat}`;
}

const GROUPS_BY_BASE = new Map(MEASURE_GROUPS.map((group) => [group.base, group]));

// "PG_default_PG.mean" -> "PG Reading Consistency (Type, Positional) — Mean"
export function columnLabel(column: string): string {
  const [base, stat] = column.split(".");
  const group = GROUPS_BY_BASE.get(base);
  if (!group) return column;
  return `${group.label} — ${stat === "mean" ? "Mean" : "Min"}`;
}
