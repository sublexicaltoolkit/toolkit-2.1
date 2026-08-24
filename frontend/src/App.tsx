import { BrowserRouter, Routes, Route } from "react-router-dom";
import Home from "./pages/Home";
import About from "./pages/About";
import EnglishToolkit from "./pages/EnglishToolkit";
import "./index.css";

export default function App() {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<Home />} />
        <Route path="/about" element={<About />} />
        <Route path="/english" element={<EnglishToolkit />} />
      </Routes>
    </BrowserRouter>
  );
}