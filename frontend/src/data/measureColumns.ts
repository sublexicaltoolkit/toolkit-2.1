// Decodes the all_measures.csv column names, following the professor's notes:
//   <unit>_<variant>_<target>.<stat>
//   unit:    PG | ONC | OC | OR
//   variant: default | noposition | freq | freq_noposition
//            ("freq" = token weighted, otherwise type; "noposition" ignores syllable position)
//   target:  PG | GP            -> consistency (directional)
//            PG_freq | G_freq | P_freq -> frequency (bidirectional)
//   stat:    only mean and min are shown
//
// Parsing probability (PP) columns are named differently: pp_<stat> (type weighted)
// and pp_freq_<stat> (token weighted). They have no unit type or direction.

export type UnitType = "PG" | "OR" | "OC" | "ONC";
export type MeasureType = "Frequency" | "Consistency" | "Parsing Probability";
export type Directionality = "Reading" | "Spelling";
export type Weighting = "Token" | "Type";
export type Stat = "mean" | "min";

export type MeasureGroup = {
  // Column name without the stat, e.g. "PG_default_PG"
  base: string;
  // null for parsing probability, which is not tied to a unit type
  unitType: UnitType | null;
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

const UNIT_MEASURE_GROUPS: MeasureGroup[] = UNITS.flatMap((unitType) =>
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

const PARSING_PROBABILITY_GROUPS: MeasureGroup[] = [
  { base: "pp", weighting: "Type" as const },
  { base: "pp_freq", weighting: "Token" as const },
].map(({ base, weighting }) => ({
  base,
  unitType: null,
  measureType: "Parsing Probability" as const,
  directionality: null,
  weighting,
  positional: true,
  label: `Parsing Probability (${weighting})`,
}));

export const MEASURE_GROUPS: MeasureGroup[] = [
  ...UNIT_MEASURE_GROUPS,
  ...PARSING_PROBABILITY_GROUPS,
];

const GROUPS_BY_BASE = new Map(MEASURE_GROUPS.map((group) => [group.base, group]));

// "PG_default_PG" + "mean" -> "PG_default_PG.mean"; "pp_freq" + "min" -> "pp_freq_min"
export function columnName(base: string, stat: Stat): string {
  const group = GROUPS_BY_BASE.get(base);
  return group?.measureType === "Parsing Probability" ? `${base}_${stat}` : `${base}.${stat}`;
}

const PARTS_BY_COLUMN = new Map(
  MEASURE_GROUPS.flatMap((group) =>
    STATS.map((stat) => [columnName(group.base, stat), { group, stat }] as const),
  ),
);

// "PG_default_PG.mean" -> { group: <PG_default_PG>, statLabel: "Mean" }
export function columnParts(
  column: string,
): { group: MeasureGroup; statLabel: string } | null {
  const parts = PARTS_BY_COLUMN.get(column);
  if (!parts) return null;
  return { group: parts.group, statLabel: parts.stat === "mean" ? "Mean" : "Min" };
}

// "PG_default_PG.mean" -> "PG Reading Consistency (Type, Positional) — Mean"
export function columnLabel(column: string): string {
  const parts = columnParts(column);
  if (!parts) return column;
  return `${parts.group.label} — ${parts.statLabel}`;
}
