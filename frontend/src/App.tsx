import { BrowserRouter, Routes, Route } from "react-router-dom";
import Home from "./pages/Home";
import About from "./pages/About";
import EnglishToolkit from "./pages/EnglishToolkit";
import ToolkitSelect from "./pages/ToolkitSelect";
import ToolkitResults from "./pages/ToolkitResults";
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
          element={<ToolkitResults variant="frequency-consistency" />}
        />
        <Route
          path="/english/phonology-orthography/search"
          element={<EnglishToolkit mode="phonology-orthography" />}
        />
        <Route
          path="/english/phonology-orthography/results/standard"
          element={<ToolkitResults variant="standard" />}
        />
        <Route
          path="/english/phonology-orthography/results/onset-rime"
          element={<ToolkitResults variant="onset-rime" />}
        />
      </Routes>
    </BrowserRouter>
  );
}