import { describe, expect, it } from "vitest";
import { columnLabel, MEASURE_GROUPS } from "./measureColumns";
import { GENERAL_VARIABLES, onsetRime, phonemes, splitPhonemes } from "./phonologyMeasures";

describe("splitPhonemes", () => {
  it.each([
    ["k@t", ["k", "@", "t"]],
    ["skul", ["s", "k", "u", "l"]],
    ["kjut", ["k", "ju", "t"]],
    ["j3rn", ["j3r", "n"]],
    ["eb@nden1N", ["e", "b", "@", "n", "d", "e", "n", "1", "N"]],
  ])("splits %s", (pronunciation, phonemes) => {
    expect(splitPhonemes(pronunciation)).toEqual(phonemes);
  });
});

describe("phonemes", () => {
  it.each([
    ["k@t", 3],
    ["kjut", 4], // cute: k j u t
    ["1gz@m", 5], // exam: ɪ g z æ m
    ["@NkS^s", 6], // anxious-style kS: æ ŋ k ʃ ʌ s
    ["j3rn", 3], // yearn: j ɝ n
    ["baks", 4],
  ])("counts %s as %i phonemes", (pronunciation, count) => {
    expect(phonemes(pronunciation)).toHaveLength(count);
  });

  it("uses the full phoneme count for Phonological Length", () => {
    const length = GENERAL_VARIABLES.find((v) => v.id === "phonological-length")!;
    const entry = { spelling: "cute", pronunciation: "kjut" } as Parameters<typeof length.column.value>[0];
    expect(length.column.value(entry)).toBe(4);
  });
});

describe("onsetRime", () => {
  it.each([
    ["k@t", "k", "@t"],
    ["skul", "sk", "ul"],
    ["str8t", "str", "8t"],
    ["8", "", "8"],
  ])("splits %s into onset %s and rime %s", (pronunciation, onset, rime) => {
    expect(onsetRime(pronunciation)).toEqual({ onset, rime });
  });
});

describe("measure columns", () => {
  it("has 82 measures: 32 consistency, 48 frequency and 2 parsing probability", () => {
    expect(MEASURE_GROUPS).toHaveLength(82);
    expect(MEASURE_GROUPS.filter((g) => g.measureType === "Parsing Probability")).toHaveLength(2);
    expect(MEASURE_GROUPS.filter((g) => g.measureType === "Consistency")).toHaveLength(32);
    expect(MEASURE_GROUPS.filter((g) => g.measureType === "Frequency")).toHaveLength(48);
  });

  it("gives each frequency measure a modality and consistency none", () => {
    const modalities = (target: string) =>
      new Set(MEASURE_GROUPS.filter((g) => g.base.endsWith(`_${target}`)).map((g) => g.modality));
    expect(modalities("G_freq")).toEqual(new Set(["Orthographic"]));
    expect(modalities("P_freq")).toEqual(new Set(["Phonological"]));
    expect(modalities("PG_freq")).toEqual(new Set(["Ortho-Phono"]));
    const others = MEASURE_GROUPS.filter((g) => g.measureType !== "Frequency");
    expect(others.every((g) => g.modality === null)).toBe(true);
  });

  it("covers all four granularities, ONC included", () => {
    const granularities = new Set(MEASURE_GROUPS.map((g) => g.granularity).filter(Boolean));
    expect(granularities).toEqual(new Set(["PG", "ONC", "OC", "OR"]));
  });

  it("gives frequency measures no direction", () => {
    const frequency = MEASURE_GROUPS.filter((g) => g.measureType === "Frequency");
    expect(frequency.every((g) => g.directionality === null)).toBe(true);
  });

  it.each([
    ["PG_default_PG.mean", "PG Reading Consistency (Type, Positional) — Mean"],
    ["ONC_freq_noposition_GP.min", "ONC Spelling Consistency (Token, Non-positional) — Min"],
    ["OR_default_G_freq.mean", "OR Orthographic Frequency (Type, Positional) — Mean"],
    ["PG_freq_P_freq.mean", "PG Phonological Frequency (Token, Positional) — Mean"],
    ["ONC_noposition_PG_freq.min", "ONC Ortho-Phono Frequency (Type, Non-positional) — Min"],
    ["pp_mean", "Parsing Probability (Type) — Mean"],
    ["pp_freq_min", "Parsing Probability (Token) — Min"],
    ["not_a_column", "not_a_column"],
  ])("labels %s", (column, label) => {
    expect(columnLabel(column)).toBe(label);
  });
});
