// IPA <-> Toolkit pronunciation codes, from Appendix 8.1 of toolkit-guide.pdf.
// The palette shows IPA; searches still use the Toolkit's own codes (e.g. "cat" = k@t).

export type PaletteSymbol = { ipa: string; code: string; example: string };

export const IPA_VOWELS: PaletteSymbol[] = [
  { ipa: "i", code: "i", example: "neato" },
  { ipa: "ɪ", code: "1", example: "fit" },
  { ipa: "eɪ", code: "8", example: "weight" },
  { ipa: "ɛ", code: "E", example: "net" },
  { ipa: "æ", code: "@", example: "cat" },
  { ipa: "ɑ", code: "a", example: "father" },
  { ipa: "ɔ", code: "c", example: "force" },
  { ipa: "oʊ", code: "o", example: "ghost" },
  { ipa: "ʊ", code: "U", example: "look" },
  { ipa: "u", code: "u", example: "compute" },
  { ipa: "ʌ", code: "^", example: "mutt" },
  { ipa: "ə", code: "e", example: "about" },
  { ipa: "ɝ", code: "3r", example: "third" },
  { ipa: "aɪ", code: "5", example: "light" },
  { ipa: "aʊ", code: "O", example: "wow" },
  { ipa: "ɔɪ", code: "2", example: "poise" },
];

// Only the consonants you can't type on a keyboard; b, d, f, k, s, t... are typed as-is.
export const IPA_CONSONANTS: PaletteSymbol[] = [
  { ipa: "θ", code: "T", example: "think" },
  { ipa: "ð", code: "D", example: "brother" },
  { ipa: "ʃ", code: "S", example: "shell" },
  { ipa: "ʒ", code: "Z", example: "pleasure" },
  { ipa: "ŋ", code: "N", example: "wing" },
  { ipa: "tʃ", code: "C", example: "watch" },
  { ipa: "dʒ", code: "G", example: "juggle" },
  { ipa: "ɹ", code: "r", example: "reach" },
  { ipa: "j", code: "j", example: "you" },
];

export const SYLLABLE_BREAK = ".";

// Two-character IPA first, so "aɪ" becomes 5 rather than a + 1.
const IPA_TO_CODE: [string, string][] = [
  ...[...IPA_VOWELS, ...IPA_CONSONANTS]
    .filter((s) => s.ipa !== s.code)
    .map((s) => [s.ipa, s.code] as [string, string]),
  ...([
    ["ɚ", "3r"],
    ["ɜ", "3r"],
    ["ɒ", "a"],
    ["ɡ", "g"],
    ["ˈ", ""],
    ["ˌ", ""],
  ] as [string, string][]),
].sort((a, b) => b[0].length - a[0].length);

// "kæt" -> "k@t", "ˈsʌn.ʃaɪn" -> "s^n.S5n". Toolkit codes and plain letters pass through unchanged.
export function ipaToToolkit(text: string): string {
  let out = "";
  let rest = text.replace(/[/[\]]/g, "");
  while (rest.length > 0) {
    const hit = IPA_TO_CODE.find(([ipa]) => rest.startsWith(ipa));
    if (hit) {
      out += hit[1];
      rest = rest.slice(hit[0].length);
    } else {
      out += rest[0];
      rest = rest.slice(1);
    }
  }
  return out;
}
