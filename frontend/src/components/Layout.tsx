import { NavLink } from "react-router-dom";

type LayoutProps = {
  children: React.ReactNode;
  variant?: "home" | "about" | "app";
};

export default function Layout({ children, variant = "home" }: LayoutProps) {
  return (
    <div className={`page page-${variant}`}>
      <nav className="nav" aria-label="Primary">
        <div className="nav-inner">
          <NavLink to="/" end className={({ isActive }) => `nav-tab${isActive ? " active" : ""}`}>
            Home
          </NavLink>
          <NavLink to="/about" className={({ isActive }) => `nav-tab${isActive ? " active" : ""}`}>
            About
          </NavLink>
          <NavLink to="/english" className={({ isActive }) => `nav-tab${isActive ? " active" : ""}`}>
            English Toolkit
          </NavLink>
        </div>
      </nav>

      {children}

      <footer className="footer">
        <div className="footer-inner">
          <div className="footer-brand">
            <p className="footer-title">Sublexical Toolkit</p>
            <p className="footer-copy">Copyright 2024</p>
            <p className="footer-credit">Icons by Icons8</p>
          </div>

          <div className="footer-col">
            <p className="footer-heading">Contact</p>
            <a href="mailto:sublexical@gmail.com">sublexical@gmail.com</a>
          </div>

          <div className="footer-col">
            <p className="footer-heading">Other</p>
            <a href="#privacy">Privacy Policy</a>
            <a href="#terms">Terms and Conditions</a>
            <NavLink to="/about">About</NavLink>
          </div>
        </div>
      </footer>
    </div>
  );
}
