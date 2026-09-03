import { BrowserRouter, Routes, Route } from "react-router-dom";
import Home from "./pages/Home";
import About from "./pages/About";
import EnglishToolkit from "./pages/EnglishToolkit";
import ResultsPlaceholder from "./pages/ResultsPlaceholder";
import ToolkitSelect from "./pages/ToolkitSelect";
import "./index.css";

export default function App() {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<Home />} />
        <Route path="/about" element={<About />} />
        <Route path="/english" element={<ToolkitSelect />} />
        <Route
          path="/english/frequency-consistency/search"
          element={<EnglishToolkit mode="frequency-consistency" />}
        />
        <Route
          path="/english/frequency-consistency/results"
          element={
            <ResultsPlaceholder
              title="Frequency & Consistency"
              searchPath="/english/frequency-consistency/search"
            />
          }
        />
        <Route
          path="/english/phonology-orthography/search"
          element={<EnglishToolkit mode="phonology-orthography" />}
        />
        <Route
          path="/english/phonology-orthography/results/standard"
          element={
            <ResultsPlaceholder
              title="Phonology & Orthography"
              format="Standard (no onset/rime sorting)"
              searchPath="/english/phonology-orthography/search"
            />
          }
        />
        <Route
          path="/english/phonology-orthography/results/onset-rime"
          element={
            <ResultsPlaceholder
              title="Phonology & Orthography"
              format="Onset/rime sorted"
              searchPath="/english/phonology-orthography/search"
            />
          }
        />
      </Routes>
    </BrowserRouter>
  );
}