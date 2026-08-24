import Layout from "../components/Layout";

const publications = [
  {
    title: "The English Sublexical Toolkit: Methods for indexing sound–spelling consistency",
    authors: "Wiley, R. W., Singh, S., Baig, Y., Key, K., & Purcell, J. J.",
    source: "Behavior Research Methods, 2024",
    href: "https://doi.org/10.3758/s13428-024-02395-3",
  },
  {
    title: "Sublexical Toolkit materials, wordlists, and analysis code",
    authors: "Wiley, R. W., Purcell, J. J., and collaborators",
    source: "Open Science Framework (OSF)",
    href: "https://osf.io/e95qw/",
  },
];

const authors = [
  { name: "Jeremy J. Purcell", role: "Principal investigator", initials: "JP" },
  { name: "Robert W. Wiley", role: "Co-author", initials: "RW" },
  { name: "Sartaj Singh", role: "Contributor", initials: "SS" },
  { name: "Yusuf Baig", role: "Contributor", initials: "YB" },
  { name: "Kristin Key", role: "Contributor", initials: "KK" },
];

const websiteTeam = [
  { name: "Snehit Shadangi", role: "Website development" },
  { name: "Undergraduate research team", role: "Design, data, and toolkit support" },
];

export default function About() {
  return (
    <Layout>
      <main className="container about">
        <section className="about-intro">
          <h1 className="about-title">About</h1>
          <p className="about-lead">
            The Sublexical Toolkit measures how regularly English spellings and pronunciations
            map to each other. It indexes frequency and consistency at several grain sizes —
            phonographemes, onset-rime, onset-nucleus-coda, and related units — so researchers
            can score real words and pseudowords in both the reading and spelling directions.
          </p>
          <p className="about-body">
            English is not a one-to-one writing system. The same letters can take different
            sounds, and the same sound can take different spellings. This site is a front end
            for those measures: search a word, choose unit and weighting options, and inspect
            frequency, consistency, phonology, and orthography in one place.
          </p>
        </section>

        <section className="about-section">
          <h2 className="about-heading">Publications</h2>
          <div className="pub-list">
            {publications.map((pub) => (
              <a key={pub.title} className="pub-bubble" href={pub.href} target="_blank" rel="noreferrer">
                <span className="pub-title">{pub.title}</span>
                <span className="pub-authors">{pub.authors}</span>
                <span className="pub-source">{pub.source}</span>
              </a>
            ))}
          </div>
        </section>

        <section className="about-section">
          <h2 className="about-heading">Authors &amp; contributors</h2>
          <div className="person-grid">
            {authors.map((person) => (
              <article key={person.name} className="person-card">
                <span className="person-avatar" aria-hidden="true">
                  {person.initials}
                </span>
                <h3 className="person-name">{person.name}</h3>
                <p className="person-role">{person.role}</p>
              </article>
            ))}
          </div>
        </section>

        <section className="about-section">
          <h2 className="about-heading">Website development</h2>
          <ul className="dev-list">
            {websiteTeam.map((person) => (
              <li key={person.name}>
                <strong>{person.name}</strong>
                <span>{person.role}</span>
              </li>
            ))}
          </ul>
        </section>
      </main>
    </Layout>
  );
}
