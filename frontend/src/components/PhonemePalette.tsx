import { useRef, useState } from "react";
import {
  IPA_CONSONANTS,
  IPA_VOWELS,
  SYLLABLE_BREAK,
  ipaToToolkit,
  type PaletteSymbol,
} from "../data/ipa";

type Props = {
  value: string;
  onChange: (value: string) => void;
};

// Phoneme textarea with a clickable IPA palette. Symbols are inserted at the cursor.
export default function PhonemePalette({ value, onChange }: Props) {
  const [open, setOpen] = useState(true);
  const input = useRef<HTMLTextAreaElement>(null);

  // Latest text and cursor, so quick clicks before a re-render don't drop symbols.
  const latest = useRef({ value, cursor: value.length });
  if (latest.current.value !== value) latest.current = { value, cursor: value.length };

  const insert = (symbol: string) => {
    const el = input.current;
    const text = latest.current.value;
    const focused = el !== null && document.activeElement === el;
    const start = focused ? el.selectionStart : latest.current.cursor;
    const end = focused ? el.selectionEnd : latest.current.cursor;
    const next = text.slice(0, start) + symbol + text.slice(end);
    const cursor = start + symbol.length;
    latest.current = { value: next, cursor };
    onChange(next);
    requestAnimationFrame(() => el?.setSelectionRange(cursor, cursor));
  };

  const button = (s: PaletteSymbol) => (
    <button
      key={s.ipa}
      type="button"
      className="ipa-key"
      title={`/${s.ipa}/ as in "${s.example}" (Toolkit code ${s.code})`}
      aria-label={`${s.ipa} as in ${s.example}`}
      onClick={() => insert(s.ipa)}
    >
      {s.ipa}
    </button>
  );

  const lines = value.split(/[\n,]+/).map((line) => line.trim()).filter(Boolean);
  const converted = lines.map(ipaToToolkit);
  const showCodes = converted.some((code, i) => code !== lines[i]);

  return (
    <div className="input-section phoneme-input">
      <div className="phoneme-header">
        <label htmlFor="phoneme-input" className="section-title">
          Phoneme Input <span className="optional">(optional)</span>
        </label>
        <button
          type="button"
          className="ipa-toggle"
          aria-expanded={open}
          onClick={() => setOpen(!open)}
        >
          IPA Palette {open ? "▲" : "▼"}
        </button>
      </div>
      <textarea
        id="phoneme-input"
        ref={input}
        className="textarea-input"
        placeholder="Enter input here or click symbols below..."
        rows={3}
        value={value}
        onChange={(event) => onChange(event.target.value)}
        onSelect={(event) => {
          const el = event.currentTarget;
          latest.current = { value: el.value, cursor: el.selectionStart };
        }}
      />
      {showCodes && (
        <p className="ipa-codes">Toolkit codes: {converted.join(", ")}</p>
      )}
      {open && (
        <div className="ipa-palette">
          <p className="ipa-group-title">Vowels &amp; diphthongs</p>
          <div className="ipa-keys">{IPA_VOWELS.map(button)}</div>
          <p className="ipa-group-title">Consonants</p>
          <div className="ipa-keys">{IPA_CONSONANTS.map(button)}</div>
          <p className="ipa-group-title">Breaks</p>
          <div className="ipa-keys">
            <button
              type="button"
              className="ipa-key ipa-key-wide"
              title="Syllable break"
              onClick={() => insert(SYLLABLE_BREAK)}
            >
              . (syllable)
            </button>
          </div>
          <p className="ipa-hint">
            Type plain letters (b, d, k, s, t...) as usual. Separate words with commas or new lines.
          </p>
        </div>
      )}
    </div>
  );
}
