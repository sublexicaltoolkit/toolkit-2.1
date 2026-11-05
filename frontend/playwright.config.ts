import { defineConfig, devices } from "@playwright/test";

// Browser tests for the search -> results flow. Run with `npm run test:e2e`.
// Set E2E_PORT to use another port when 5173 is taken by a different app.
const port = process.env.E2E_PORT ?? "5173";
export default defineConfig({
  testDir: "./e2e",
  timeout: 30_000,
  use: {
    baseURL: `http://localhost:${port}`,
    trace: "retain-on-failure",
  },
  projects: [{ name: "chromium", use: { ...devices["Desktop Chrome"] } }],
  webServer: {
    command: `npm run dev -- --port ${port} --strictPort`,
    url: `http://localhost:${port}`,
    reuseExistingServer: true,
  },
});
