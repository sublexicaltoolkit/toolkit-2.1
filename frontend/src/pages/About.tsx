import Layout from "../components/Layout";

const publications = [
  {
    title:
      "Pseudoword spelling: insights into sublexical representations and lexical interactions",
    authors: "Robert W. Wiley, Kristin M. Key & Jeremy J. Purcell",
  },
  {
    title:
      "The English Sublexical Toolkit: Methods for indexing sound–spelling consistency",
    authors: "Robert W. Wiley, Sartaj Singh, Yusuf Baig, Kristin Key & Jeremy J. Purcell",
  },
];

const authors = [
  { name: "Name", initials: "A" },
  { name: "Name", initials: "B" },
  { name: "Name", initials: "C" },
  { name: "Name", initials: "D" },
];

const websiteTeam = ["Name", "Name"];

export default function About() {
  return (
    <Layout variant="about">
      <main className="container about">
        <section className="about-intro">
          <h1 className="about-title">English Sublexical Toolkit</h1>
          <p className="about-lead">
            The English Sublexical Toolkit is a suite of tools that models sublexical
            regularities in English using an experience-based learning framework.
            It computes frequency and probability indices for grapheme-phoneme
            mappings across multiple grain sizes, offering novel and more
            accurate measures to predict reading and spelling behavior for both
            real and pseudowords.
          </p>
        </section>

        <section className="about-section about-publications">
          <h2 className="about-heading">Publications</h2>
          <div className="pub-grid">
            {publications.map((pub) => (
              <article key={pub.title} className="pub-card">
                <h3 className="pub-title">{pub.title}</h3>
                <p className="pub-authors">{pub.authors}</p>
              </article>
            ))}
          </div>
        </section>

        <section className="about-section">
          <h2 className="about-heading">Authors &amp; contributors</h2>
          <div className="person-grid">
            {authors.map((person, index) => (
              <article key={`${person.name}-${index}`} className="person-card">
                <span className="person-avatar" aria-hidden="true">
                  {person.initials}
                </span>
                <h3 className="person-name">{person.name}</h3>
              </article>
            ))}
          </div>
        </section>

        <section className="about-section">
          <h2 className="about-heading">Website development</h2>
          <ul className="dev-list">
            {websiteTeam.map((name, index) => (
              <li key={`${name}-${index}`}>{name}</li>
            ))}
          </ul>
        </section>
      </main>
    </Layout>
  );
}
