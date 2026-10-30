import { readFileSync } from "node:fs";
import { expect, test, type Page } from "@playwright/test";

const WORDS_CSV = {
  name: "words-with-header.csv",
  mimeType: "text/csv",
  buffer: Buffer.from("Word,Notes\ncat,animal\nlight,\nschool,place\nstreet,\nqwzx,not a word\n"),
};

const WORDS_TXT = "dog\nboat\nrain\nplay\nchair\nfish\n";

function resultRow(page: Page, word: string) {
  return page.locator(".results-table tbody tr", {
    has: page.locator("td:first-child", { hasText: new RegExp(`^${word}$`) }),
  });
}

test.describe("Frequency & Consistency", () => {
  test("shows real values for typed words", async ({ page }) => {
    await page.goto("/english/frequency-consistency/search");
    await page.locator(".textarea-input").first().fill("cat, light\nschool");
    await page.getByLabel("Phoneme-Grapheme (PG)").check();
    await page.getByRole("checkbox", { name: "Consistency", exact: true }).check();
    await page.getByRole("button", { name: "Apply" }).click();

    // 4 variants x 2 directions of PG consistency
    await expect(page.locator(".statistics-table tbody tr")).toHaveCount(8);

    await page.getByRole("button", { name: "View Results" }).click();
    await expect(page).toHaveURL(/frequency-consistency\/results/);

    const header = page.getByRole("columnheader", {
      name: "PG Reading Consistency (Type, Positional)",
    });
    await expect(header).toBeVisible();
    await expect(resultRow(page, "cat")).toContainText("k@t");
    // Consistency is shown as a percentage on screen
    await expect(resultRow(page, "cat")).toContainText("87.80%");
    await expect(resultRow(page, "school")).toContainText("56.83%");
    await expect(page.locator(".results-table tbody tr")).toHaveCount(3);
  });

  test("downloads the table as CSV", async ({ page }) => {
    await page.goto("/english/frequency-consistency/results?demo");
    await expect(resultRow(page, "cat")).toBeVisible();

    const downloadPromise = page.waitForEvent("download");
    await page.getByRole("button", { name: "Download results" }).click();
    const download = await downloadPromise;
    const csv = readFileSync(await download.path(), "utf8");

    const [header, ...rows] = csv.trim().split("\n");
    expect(header).toContain('"Word","Pronunciation"');
    expect(rows).toHaveLength(3);
    expect(rows[0]).toMatch(/^"cat","k@t","0\.8780"/);
  });
});

test("lists homographs in their own table", async ({ page }) => {
  await page.goto("/english/frequency-consistency/search");
  await page.locator(".textarea-input").first().fill("cat, read");
  await page.locator('input[name="multiplePronunciations"][value="Yes"]').check();
  await page.getByRole("button", { name: "View Results" }).click();

  const main = page.locator(".results-table:not(.homograph-table) tbody tr");
  await expect(main).toHaveCount(2);
  await expect(resultRow(page, "read").first()).toContainText("homograph");

  const homographs = page.locator(".homograph-table tbody tr");
  await expect(homographs).toHaveCount(2);
  await expect(homographs.nth(0).locator("td").nth(2)).toHaveText("rEd");
  await expect(homographs.nth(1).locator("td").nth(1)).toHaveText("2");
});

test.describe("Phonology & Orthography", () => {
  test("breaks a word into syllables like the Figma demo", async ({ page }) => {
    await page.goto("/english/phonology-orthography/search");
    await page.locator(".textarea-input").first().fill("hello");
    await page.getByRole("button", { name: "Submit" }).click();

    await expect(page.getByText("“Hello” Average Results")).toBeVisible();
    const phonology = page.locator(".breakdown-card", { hasText: "Phonology" }).locator("tbody tr");
    await expect(phonology.nth(0).locator("td")).toHaveText(["h", "E", "", "", "", ""]);
    await expect(phonology.nth(1).locator("td")).toHaveText(["l", "o", "", "", "", ""]);

    const orthography = page.locator(".breakdown-card", { hasText: "Orthography" });
    await expect(orthography.locator("tbody tr").nth(1).locator("td").first()).toHaveText("ll");

    const calculated = page.locator(".breakdown-calculated");
    await expect(calculated.getByRole("row", { name: /Which Min/ })).toContainText("l+ll");
  });

  test("sorts the breakdown by onset and rime", async ({ page }) => {
    await page.goto("/english/phonology-orthography/search");
    await page.locator(".textarea-input").first().fill("capable");
    await page.locator('input[name="phonologyFormat"][value="onset-rime"]').check();
    await page.getByRole("button", { name: "Submit" }).click();

    const orthography = page.locator(".breakdown-card", { hasText: "Orthography" });
    await expect(orthography.locator("tbody tr").nth(2).locator("td")).toHaveText(["b", "_el"]);
    await expect(page.locator(".breakdown-average thead th")).toHaveCount(3);
  });

  test("reads an uploaded CSV and shows its words", async ({ page }) => {
    await page.goto("/english/phonology-orthography/search");
    await page.locator(".file-upload-input").setInputFiles(WORDS_CSV);
    await expect(page.locator(".file-name")).toContainText("(5 words)");

    await page.getByLabel("Orthographic Length").check();
    await page.getByLabel("Phonological Length").check();
    await page.getByLabel("Polysemy").check();
    await page.getByRole("button", { name: "Submit" }).click();

    await expect(resultRow(page, "street")).toContainText("strit");
    await expect(page.locator(".results-table tbody tr")).toHaveCount(4);
    await expect(page.getByText("Not found: qwzx")).toBeVisible();
    await expect(page.getByText("Not available yet: Semantic — Polysemy")).toBeVisible();

    // street: 6 letters, 5 phonemes (s t r i t)
    const cells = resultRow(page, "street").locator("td");
    await expect(cells).toHaveText(["street", "strit", "6", "5"]);
  });

  test("accepts a dropped txt file and combines it with typed words", async ({ page }) => {
    await page.goto("/english/phonology-orthography/search");
    await page.locator(".textarea-input").first().fill("cat");

    const dataTransfer = await page.evaluateHandle((text) => {
      const transfer = new DataTransfer();
      transfer.items.add(new File([text], "words.txt", { type: "text/plain" }));
      return transfer;
    }, WORDS_TXT);
    await page.locator(".file-upload-label").dispatchEvent("drop", { dataTransfer });
    await expect(page.locator(".file-name")).toContainText("words.txt (6 words)");

    await page.getByRole("button", { name: "Submit" }).click();
    await expect(page.locator(".results-table tbody tr")).toHaveCount(7);
  });

  test("removing the uploaded file clears its words", async ({ page }) => {
    await page.goto("/english/phonology-orthography/search");
    await page.locator(".file-upload-input").setInputFiles(WORDS_CSV);
    await expect(page.locator(".file-name")).toContainText("(5 words)");
    await page.getByRole("button", { name: "Remove" }).click();
    await expect(page.locator(".file-name")).toHaveCount(0);
  });

  test("splits words into onset and rime", async ({ page }) => {
    await page.goto("/english/phonology-orthography/search");
    await page.locator(".textarea-input").first().fill("school\ncat");
    await page.locator('input[name="phonologyFormat"][value="onset-rime"]').check();
    await page.getByLabel("First Phoneme").check();
    await page.getByRole("button", { name: "Submit" }).click();

    await expect(page).toHaveURL(/results\/onset-rime/);
    await expect(resultRow(page, "school").locator("td")).toHaveText([
      "school",
      "skul",
      "sk",
      "ul",
      "s",
    ]);
  });

  test("shows a message when nothing is found", async ({ page }) => {
    await page.goto("/english/phonology-orthography/search");
    await page.locator(".textarea-input").first().fill("qwzx");
    await page.getByRole("button", { name: "Submit" }).click();
    await expect(page.getByText("Nothing was found for this query")).toBeVisible();
  });
});
