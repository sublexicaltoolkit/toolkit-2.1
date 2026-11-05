import { Link } from "react-router-dom";
import ForumIcon from "@mui/icons-material/Forum";
import QueryStatsIcon from "@mui/icons-material/QueryStats";
import Layout from "../components/Layout";

export default function Home() {
  return (
    <Layout>
      <main className="container">
        <section className="hero">
          <h1 className="hero-title">Sublexical Toolkit</h1>

          <div className="hero-graphics">
            <span className="hero-icon-tile">
              <ForumIcon fontSize="inherit" />
            </span>
            <span className="hero-arrow">→</span>
            <span className="hero-icon-tile">
              <QueryStatsIcon fontSize="inherit" />
            </span>
          </div>

          <p className="hero-subtitle">Language analysis, customized to your needs.</p>
        </section>

        <section className="toolkits">
          <h2 className="section-title">Available Toolkits</h2>

          <div className="card-grid">
            <article className="card">
              <h3 className="card-title">English Toolkit</h3>
              <p className="card-subtitle">
                Whole-word frequency, orthographic and phonological length, plus sublexical
                frequency, consistency and parsing probability for words in the Toolkit's
                English word list.
              </p>
              <Link to="/english" className="btn">
                Open toolkit
              </Link>
            </article>
          </div>
        </section>
      </main>
    </Layout>
  );
}
