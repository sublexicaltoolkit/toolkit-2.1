import { Link } from "react-router-dom";
import ForumIcon from '@mui/icons-material/Forum';
import TroubleshootIcon from '@mui/icons-material/Troubleshoot';

export default function Home() {
  return (
    <main className="container">
      <section className="hero">
        <h1 className="hero-title">Sublexical Toolkit</h1>

        <div className="hero-graphics">
          <span className="hero-icon-tile">
            <ForumIcon fontSize="inherit" />
          </span>
          <span className="hero-arrow">→</span>
          <span className="hero-icon-tile">
            <TroubleshootIcon fontSize="inherit" />
          </span>
        </div>

        <p className="hero-subtitle">
          Language analysis, customized to your needs.
        </p>
      </section>

      <section className="toolkits">
        <h2 className="section-title">Available Toolkits</h2>

        <div className="card-grid">
          <article className="card">
            <h3 className="card-title">English Toolkit</h3>
            <p className="card-subtitle">Includes:</p>
            <ul className="card-list">
              <li>Frequency and Consistency Log</li>
              <li>Phonology and Orthography</li>
            </ul>
            <Link to="/english" className="btn">Visit page</Link>
          </article>
        </div>
      </section>
    </main>
  );
}