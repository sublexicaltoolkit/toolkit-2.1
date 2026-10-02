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
  {
    name: "Robert W. Wiley",
    affiliation: "University of North Carolina Greensboro",
  },
  {
    name: "Jeremy J. Purcell",
    affiliation: "University of Maryland College Park",
  },
  {
    name: "Kristin M. Key",
    affiliation: "University of North Carolina Greensboro",
  },
];

const softwareColumns = [
  ["first name, last name", "first name, last name", "first name, last name"],
  ["first name, last name", "first name, last name", "first name, last name"],
  ["first name, last name", "first name, last name", "first name, last name"],
];

const websiteTeam = ["Abhiram Cheerla", "Tarun Kommuri"];

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

        <section className="about-section about-people">
          <h2 className="about-heading">Authors</h2>
          <div className="author-grid">
            {authors.map((person) => (
              <article key={person.name} className="author-card">
                <span className="author-photo" aria-hidden="true" />
                <h3 className="author-name">{person.name}</h3>
                <p className="author-affiliation">{person.affiliation}</p>
              </article>
            ))}
            <article className="author-card author-slot">
              <div className="image-placeholder">Image</div>
              <div className="name-placeholder">Name + Desc.</div>
            </article>
          </div>
        </section>

        <section className="about-section about-contributors">
          <h2 className="about-heading">Contributors</h2>
          <div className="contributor-layout">
            <div>
              <h3>Software/Documentation</h3>
              <div className="software-columns">
                {softwareColumns.map((column, columnIndex) => (
                  <ul key={columnIndex}>
                    {column.map((name, nameIndex) => (
                      <li key={`${columnIndex}-${nameIndex}`}>{name}</li>
                    ))}
                  </ul>
                ))}
              </div>
            </div>
            <div className="website-team">
              <h3>Website Development</h3>
              <ul>
                {websiteTeam.map((name) => (
                  <li key={name}>{name}</li>
                ))}
              </ul>
            </div>
          </div>
        </section>
      </main>
    </Layout>
  );
}
