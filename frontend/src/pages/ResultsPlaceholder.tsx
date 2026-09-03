import { Link } from "react-router-dom";
import Layout from "../components/Layout";
import "./EnglishToolkit.css";

type ResultsPlaceholderProps = {
  title: string;
  searchPath: string;
  format?: string;
};

export default function ResultsPlaceholder({
  title,
  searchPath,
  format,
}: ResultsPlaceholderProps) {
  return (
    <Layout variant="app">
      <main className="results-placeholder">
        <p className="route-label">Results page</p>
        <h1>{title}</h1>
        {format && <p className="results-format">{format}</p>}
        <p>The final Figma table and result controls will be added here.</p>
        <Link className="action-button search-option-link" to={searchPath}>
          Back to search
        </Link>
      </main>
    </Layout>
  );
}
