import { Link } from "react-router-dom";
import Layout from "../components/Layout";
import "./EnglishToolkit.css";

const searchPages = [
  {
    title: "Frequency & Consistency",
    description: "Choose frequency and consistency measures for your word list.",
    to: "/english/frequency-consistency/search",
  },
  {
    title: "Phonology & Orthography",
    description: "Choose phonology and orthography measures for your word list.",
    to: "/english/phonology-orthography/search",
  },
];

export default function ToolkitSelect() {
  return (
    <Layout variant="select" pageTitle="English Toolkit">
      <main className="toolkit-select">
        <header className="toolkit-select-header">
          <h1>English Toolkit</h1>
          <p>Select the type of analysis you want to run.</p>
        </header>

        <div className="search-option-grid">
          {searchPages.map((page) => (
            <article className="search-option-card" key={page.to}>
              <h2>{page.title}</h2>
              <p>{page.description}</p>
              <Link className="action-button search-option-link" to={page.to}>
                Open search
              </Link>
            </article>
          ))}
        </div>
      </main>
    </Layout>
  );
}
