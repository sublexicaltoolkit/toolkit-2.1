import { describe, expect, it } from "vitest";
import { ipaToToolkit } from "./ipa";

describe("ipaToToolkit", () => {
  it.each([
    ["kæt", "k@t"],
    ["laɪt", "l5t"],
    ["weɪt", "w8t"],
    ["θɪŋk", "T1Nk"],
    ["ˈsʌn.ʃaɪn", "s^n.S5n"],
    ["/ɡoʊst/", "gost"],
    ["fɑðɚ", "faD3r"],
    ["k@t", "k@t"],
  ])("converts %s", (ipa, code) => {
    expect(ipaToToolkit(ipa)).toBe(code);
  });
});
