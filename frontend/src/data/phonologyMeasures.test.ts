import { describe, expect, it } from "vitest";
import { columnLabel, MEASURE_GROUPS } from "./measureColumns";
import { onsetRime, splitPhonemes } from "./phonologyMeasures";

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
  it("has 80 measures: 32 consistency and 48 frequency", () => {
    expect(MEASURE_GROUPS).toHaveLength(80);
    expect(MEASURE_GROUPS.filter((g) => g.measureType === "Consistency")).toHaveLength(32);
    expect(MEASURE_GROUPS.filter((g) => g.measureType === "Frequency")).toHaveLength(48);
  });

  it("gives frequency measures no direction", () => {
    const frequency = MEASURE_GROUPS.filter((g) => g.measureType === "Frequency");
    expect(frequency.every((g) => g.directionality === null)).toBe(true);
  });

  it.each([
    ["PG_default_PG.mean", "PG Reading Consistency (Type, Positional) — Mean"],
    ["ONC_freq_noposition_GP.min", "ONC Spelling Consistency (Token, Non-positional) — Min"],
    ["OR_default_G_freq.mean", "OR Grapheme Frequency (Type, Positional) — Mean"],
    ["not_a_column", "not_a_column"],
  ])("labels %s", (column, label) => {
    expect(columnLabel(column)).toBe(label);
  });
});
