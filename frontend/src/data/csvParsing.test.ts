import { describe, expect, it } from "vitest";
import { parseCsvRows, parseWordFile } from "./csvParsing";

function textFile(name: string, content: string): File {
  return new File([content], name, { type: "text/plain" });
}

describe("parseCsvRows", () => {
  const byteOrderMark = String.fromCharCode(0xfeff);
  const csv = [
    `${byteOrderMark}"","spelling","pronunciation","freq","PG_default_PG.sd"`,
    '"1","a","8",1041179,NA',
    '"2","cat","k@t",3383,0.0668',
    "",
  ].join("\n");

  it("cleans the R export quirks", async () => {
    const rows = await parseCsvRows(csv);

    expect(rows).toHaveLength(2);
    expect(rows[0]).toEqual({
      rowId: 1,
      spelling: "a",
      pronunciation: "8",
      freq: 1041179,
      "PG_default_PG.sd": null,
    });
  });

  it("keeps spelling and pronunciation as text", async () => {
    const [row] = await parseCsvRows(csv);
    expect(typeof row.pronunciation).toBe("string");
  });

  it("keeps only rows that pass the filter", async () => {
    const rows = await parseCsvRows(csv, { keep: (row) => row.spelling === "cat" });
    expect(rows.map((row) => row.spelling)).toEqual(["cat"]);
  });
});

describe("parseWordFile", () => {
  it("reads the first column of a csv and skips a header", async () => {
    const file = textFile("words.csv", "Word,Notes\ncat,animal\nlight,\n\nschool,place\n");
    expect(await parseWordFile(file)).toEqual(["cat", "light", "school"]);
  });

  it("reads a txt file with one word per line", async () => {
    const file = textFile("words.txt", "dog\r\nboat\r\n  rain  \r\n");
    expect(await parseWordFile(file)).toEqual(["dog", "boat", "rain"]);
  });

  it("keeps the first row when it is not a header", async () => {
    const file = textFile("words.csv", "cat\ndog\n");
    expect(await parseWordFile(file)).toEqual(["cat", "dog"]);
  });

  it("returns no words for an empty file", async () => {
    expect(await parseWordFile(textFile("empty.csv", ""))).toEqual([]);
  });
});
