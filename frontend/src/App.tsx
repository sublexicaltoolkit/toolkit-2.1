import { BrowserRouter, Navigate, Routes, Route } from "react-router-dom";
import Home from "./pages/Home";
import About from "./pages/About";
import EnglishToolkit from "./pages/EnglishToolkit";
import ToolkitResults from "./pages/ToolkitResults";
import "./index.css";

// The old split search pages now live on the single toolkit page.
const OLD_PATHS = [
  "/english/frequency-consistency/search",
  "/english/phonology-orthography/search",
  "/english/frequency-consistency/results",
  "/english/phonology-orthography/results/standard",
  "/english/phonology-orthography/results/onset-rime",
];

export default function App() {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<Home />} />
        <Route path="/about" element={<About />} />
        <Route path="/english" element={<EnglishToolkit />} />
        <Route path="/english/results" element={<ToolkitResults />} />
        {OLD_PATHS.map((path) => (
          <Route key={path} path={path} element={<Navigate to="/english" replace />} />
        ))}
        <Route path="*" element={<Navigate to="/" replace />} />
      </Routes>
    </BrowserRouter>
  );
}
