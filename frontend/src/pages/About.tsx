import Layout from "../components/Layout";

const publications = [
  { title: "Publication title" },
  { title: "Publication title" },
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
          <h1 className="about-title">About</h1>
        </section>

        <section className="about-section">
          <h2 className="about-heading">Publications</h2>
          <div className="pub-grid">
            {publications.map((pub, index) => (
              <article key={`${pub.title}-${index}`} className="pub-card">
                <h3 className="pub-title">{pub.title}</h3>
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
