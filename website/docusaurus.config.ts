import type { Config } from "@docusaurus/types";

const repoUrl = "https://github.com/scalameta/scalafmt";

export default {
  title: "Scalafmt",
  tagline: "Code formatter for Scala",
  url: "https://scalameta.org",
  baseUrl: "/scalafmt/",
  organizationName: "scalameta",
  projectName: "scalafmt",
  deploymentBranch: "gh-pages",
  trailingSlash: false,
  favicon: "img/favicon.ico",
  customFields: { repoUrl },
  onBrokenLinks: "throw",
  // .md files (all our docs) parse as CommonMark, so the raw HTML the docs
  // and mdoc modifiers emit passes through untouched instead of hitting MDX's
  // stricter JSX rules. Reserve .mdx for anything that needs real MDX.
  markdown: { format: "detect", hooks: { onBrokenMarkdownLinks: "throw" } },
  presets: [
    [
      "@docusaurus/preset-classic",
      {
        docs: {
          // mdoc writes generated markdown here (see scalafmt-docs Main.scala).
          path: "../website/target/docs",
          sidebarPath: "../website/sidebars.json",
          editUrl: ({ docPath }: { docPath: string }) =>
            `${repoUrl}/edit/main/docs/${docPath}`,
        },
        theme: { customCss: "./src/css/customTheme.css" },
      },
    ],
  ],
  plugins: ["@easyops-cn/docusaurus-search-local"],
  clientModules: ["./src/js/scalafmt-compare.js"],
  themeConfig: {
    colorMode: { respectPrefersColorScheme: true },
    image: "img/scalameta-logo.png",
    navbar: {
      title: "Scalafmt",
      logo: { alt: "Scalafmt", src: "img/scalameta-logo.png" },
      items: [
        { to: "docs/installation", label: "Docs", position: "left" },
        {
          href: repoUrl,
          position: "right",
          className: "header-github-link",
          "aria-label": "GitHub repository",
        },
      ],
    },
    footer: {
      style: "dark",
      links: [
        {
          title: "Docs",
          items: [
            { label: "Installation", to: "docs/installation" },
            { label: "Configuration", to: "docs/configuration" },
            { label: "Contributing", to: "docs/contributing-scalafmt" },
          ],
        },
        {
          title: "Community",
          items: [{ label: "Join on Discord", href: "https://discord.gg/N43mbnH" }],
        },
        {
          title: "More",
          items: [{ label: "GitHub", href: repoUrl }],
        },
      ],
      logo: { alt: "Scalameta", src: "img/scalameta-logo.png" },
      copyright: `Copyright © ${new Date().getFullYear()} Scalafmt`,
    },
    prism: {
      // Order matters: Prism loads these sequentially without resolving
      // dependencies, and `scala` extends `java`. `properties` highlights the
      // key = value config blocks emitted by DefaultsModifier.
      additionalLanguages: ["java", "scala", "properties"],
    },
  },
} satisfies Config;
