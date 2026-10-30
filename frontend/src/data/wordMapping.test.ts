import { describe, expect, it } from "vitest";
import { parseCsvRows } from "./csvParsing";
import {
  hasFinalE,
  parseMapping,
  splitOnsetRime,
  splitSyllables,
  whichMin,
} from "./wordMapping";

// Values as exported from scored_words_PG for "hello" and "capable"
const HELLO = {
  map_PG_phonemes: "h|E|l|o",
  map_PG_graphemes: "h|e|ll|o",
  map_PG_positions: "wi|sf|si|wf",
  map_PG_PG: "0.983|0.7654|0.2192|0.7409",
  map_PG_GP: "1|0.3914|0.998|0.9565",
  map_PG_PG_freq: "2.939|3.0678|2.6937|2.4579",
  map_PG_P_freq: "2.9465|3.1838|3.3522|2.5877",
  map_PG_G_freq: "2.939|3.4749|2.6946|2.4771",
};

const CAPABLE = {
  map_PG_phonemes: "k|8|p|e|b|e|l",
  map_PG_graphemes: "c|a|p|a|b|_e|l",
  map_PG_positions: "wi|sf|si|sf|si|sm|wf",
  map_PG_PG: "0.8495|0.8094|0.8126|0.2486|0.9309|0.1037|0.8829",
};

const phonemes = (units: { phoneme: string }[]) => units.map((unit) => unit.phoneme);

describe("parseMapping", () => {
  it("reads units and their values", () => {
    const units = parseMapping(HELLO, "PG")!;
    expect(units).toHaveLength(4);
    expect(units[2]).toMatchObject({ phoneme: "l", grapheme: "ll", position: "si", PG: 0.2192 });
  });

  it("returns null for words the toolkit could not map", () => {
    expect(parseMapping({ map_PG_phonemes: "" }, "PG")).toBeNull();
    expect(parseMapping({}, "OR")).toBeNull();
  });

  it("keeps a single numeric-looking phoneme as text when parsed from CSV", async () => {
    const csv = 'spelling,pronunciation,map_PG_phonemes,map_PG_PG\n"a","8",8,0.6667\n';
    const [row] = await parseCsvRows(csv);
    expect(parseMapping(row, "PG")![0]).toMatchObject({ phoneme: "8", PG: 0.6667 });
  });
});

describe("syllables", () => {
  it("starts a new syllable at each syllable-initial unit", () => {
    const syllables = splitSyllables(parseMapping(HELLO, "PG")!);
    expect(syllables.map(phonemes)).toEqual([["h", "E"], ["l", "o"]]);
  });

  it("splits a syllable into onset, nucleus and coda", () => {
    const [, , third] = splitSyllables(parseMapping(CAPABLE, "PG")!);
    const { onset, nucleus, coda } = splitOnsetRime(third);
    expect(phonemes(onset)).toEqual(["b"]);
    expect(nucleus?.phoneme).toBe("e");
    expect(phonemes(coda)).toEqual(["l"]);
  });

  it("marks a final E from silent-e graphemes", () => {
    const syllables = splitSyllables(parseMapping(CAPABLE, "PG")!);
    expect(syllables.map(hasFinalE)).toEqual([false, false, true]);
  });
});

describe("whichMin", () => {
  it("names the lowest unit as phoneme+grapheme", () => {
    const units = parseMapping(HELLO, "PG")!;
    expect(whichMin(units, "PG")).toBe("l+ll");
    expect(whichMin(units, "GP")).toBe("E+e");
  });

  it("is null when no unit has a value", () => {
    expect(whichMin(parseMapping(CAPABLE, "PG")!, "G_freq")).toBeNull();
  });
});
