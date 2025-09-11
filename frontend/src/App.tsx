import { BrowserRouter, Routes, Route } from "react-router-dom";
import Home from "./pages/Home";
import EnglishToolkit from "./pages/EnglishToolkit";
import "./index.css";

export default function App() {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<Home />} />
        <Route path="/english" element={<EnglishToolkit />} />
      </Routes>
    </BrowserRouter>
  );
}