import React from "react";
import Link from "@docusaurus/Link";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import Layout from "@theme/Layout";

const Features = () => {
  const features = [
    {
      title: "Focus on what matters",
      content:
        "Spend more time discussing important issues in code review and less " +
        "time on code style. Scalafmt formats code so that it looks consistent " +
        "between people on your team.",
    },
    {
      title: "Integrated with your toolchain",
      content:
        "Run scalafmt from your editor, build tool or terminal. Scalafmt has " +
        "integrations with IntelliJ, sbt, Maven, Gradle and Mill.",
    },
  ];
  return (
    <div className="hero text--center">
      <div className="container">
        <div className="row">
          {features.map((feature) => (
            <div className="col" key={feature.title}>
              <div className="padding--md">
                <h2 className="hero__subtitle">{feature.title}</h2>
                <p>{feature.content}</p>
              </div>
            </div>
          ))}
        </div>
      </div>
    </div>
  );
};

const Index = () => {
  const { siteConfig } = useDocusaurusContext();
  return (
    <Layout title={siteConfig.title} description={siteConfig.tagline}>
      <div className="hero text--center">
        <div className="container">
          <div className="padding-vert--md">
            <h1 className="hero__title">{siteConfig.title}</h1>
            <p className="hero__subtitle">{siteConfig.tagline}</p>
          </div>
          <div>
            <Link
              to="docs/installation"
              className="button button--lg button--outline button--primary margin--sm"
            >
              Get started
            </Link>
          </div>
          <Features />
        </div>
      </div>
    </Layout>
  );
};

export default Index;
