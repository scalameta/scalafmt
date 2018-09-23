// See https://docusaurus.io/docs/site-config.html for all the possible
// site configuration options.

const repoUrl = "https://github.com/scalameta/scalafmt";
const baseUrl = "/scalafmt/";

const users = [
  {
    caption: "buildo",
    image: "https://www.buildo.io/static/media/logo.60e4e7fc.svg",
    infoLink: "https://www.buildo.io",
    pinned: true
  }
];

const siteConfig = {
  title: "Scalafmt",
  tagline: "Code formatter for Scala",
  url: "http://scalameta.org",
  baseUrl,

  // Used for publishing and more
  projectName: "Scalafmt",
  organizationName: "Scalameta",

  algolia: {
    apiKey: '3409ffc8eadf85381f1b8cca926b07c4',
    indexName: 'scalafmt'
  },

  // For no header links in the top nav bar -> headerLinks: [],
  headerLinks: [
    { doc: "installation", label: "Docs" },
    { href: repoUrl, label: "GitHub", external: true }
  ],

  // If you have users set above, you add it here:
  users,

  /* path to images for header/footer */
  // headerIcon: 'img/scalameta-logo.png',
  // footerIcon: 'img/scalameta-logo.png',
  favicon: "img/favicon.ico",

  /* colors for website */
  colors: {
    primaryColor: "#DB5859",
    secondaryColor: "#A23244"
  },

  customDocsPath: "website/target/docs",

  // This copyright info is used in /core/Footer.js and blog rss/atom feeds.
  copyright: `Copyright © ${new Date().getFullYear()} Scalafmt`,

  highlight: {
    // Highlight.js theme to use for syntax highlighting in code blocks
    theme: "github"
  },

  scripts: [
    "https://buttons.github.io/buttons.js",
    "https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.0/clipboard.min.js",
    `${baseUrl}js/code-blocks-buttons.js`,
    `${baseUrl}js/code-blocks-compare.js`
  ],

  stylesheets: [`${baseUrl}css/code-blocks-buttons.css`],

  /* On page navigation for the current documentation page */
  onPageNav: "separate",

  /* Open Graph and Twitter card images */
  ogImage: "img/scalameta-logo.png",
  twitterImage: "img/scalameta-logo.png",

  editUrl: `${repoUrl}/edit/master/docs/`,

  repoUrl
};

module.exports = siteConfig;
