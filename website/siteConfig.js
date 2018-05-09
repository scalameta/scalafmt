// See https://docusaurus.io/docs/site-config.html for all the possible
// site configuration options.

const repoUrl = 'https://github.com/scalameta/scalafmt';

const siteConfig = {
  title: 'Scalafmt',
  tagline: 'A code formatter for Scala',
  url: 'http://scalameta.org',
  baseUrl: '/scalafmt/',

  // Used for publishing and more
  projectName: 'Scalafmt',
  organizationName: 'Scalameta',

  // For no header links in the top nav bar -> headerLinks: [],
  headerLinks: [
    {doc: 'introduction', label: 'Docs'},
    {href: repoUrl, label: 'GitHub', external: true},
  ],

  // If you have users set above, you add it here:
  // users,

  /* path to images for header/footer */
  // headerIcon: 'img/scalameta-logo.png',
  // footerIcon: 'img/scalameta-logo.png',
  favicon: 'img/favicon.ico',

  /* colors for website */
  colors: {
    primaryColor: '#DB5859',
    secondaryColor: '#A23244',
  },

  customDocsPath: 'website/target/docs',

  // This copyright info is used in /core/Footer.js and blog rss/atom feeds.
  copyright: `Copyright © ${new Date().getFullYear()} Scalafmt`,

  highlight: {
    // Highlight.js theme to use for syntax highlighting in code blocks
    theme: 'vs2015',
  },

  /* On page navigation for the current documentation page */
  onPageNav: 'separate',

  /* Open Graph and Twitter card images */
  ogImage: 'img/scalameta-logo.png',
  twitterImage: 'img/scalameta-logo.png',

  editUrl: `${repoUrl}/edit/master/docs/`,

  repoUrl
};

module.exports = siteConfig;
