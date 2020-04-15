---
id: changelog
title: Changelog
---

## [v2.5.0-RC1](https://github.com/scalameta/scalafmt/tree/v2.5.0-RC1) (2020-04-15)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.4.2...v2.5.0-RC1)

**Merged pull requests:**

- AvoidInfix: simplify, fix incorrect rewrites [\#1875](https://github.com/scalameta/scalafmt/pull/1875) ([kitbellew](https://github.com/kitbellew))
- Router: remove unnecessary splits cache, used once [\#1874](https://github.com/scalameta/scalafmt/pull/1874) ([kitbellew](https://github.com/kitbellew))
- RedundantBraces: re-enable general expressions [\#1873](https://github.com/scalameta/scalafmt/pull/1873) ([kitbellew](https://github.com/kitbellew))
- CLI: add explicit flag --reportError to exit 1 on malformatted code [\#1872](https://github.com/scalameta/scalafmt/pull/1872) ([kitbellew](https://github.com/kitbellew))
- Router: fix strip margin in interpolated strings [\#1870](https://github.com/scalameta/scalafmt/pull/1870) ([kitbellew](https://github.com/kitbellew))
- Fixed alignment overriding [\#1869](https://github.com/scalameta/scalafmt/pull/1869) ([poslegm](https://github.com/poslegm))
- Dynamic configuration doc moved to "Disabling or customizing formatting" section [\#1868](https://github.com/scalameta/scalafmt/pull/1868) ([poslegm](https://github.com/poslegm))
- literals.\* docs [\#1867](https://github.com/scalameta/scalafmt/pull/1867) ([poslegm](https://github.com/poslegm))
- Documentation: explain `newlines.topLevelStatements` [\#1866](https://github.com/scalameta/scalafmt/pull/1866) ([kitbellew](https://github.com/kitbellew))
- ScalafmtConfig: allow overriding per file pattern [\#1865](https://github.com/scalameta/scalafmt/pull/1865) ([kitbellew](https://github.com/kitbellew))
- Newlines: allow break after top-level statements [\#1864](https://github.com/scalameta/scalafmt/pull/1864) ([kitbellew](https://github.com/kitbellew))
- Newlines: implicitParamListModifier to replace two [\#1863](https://github.com/scalameta/scalafmt/pull/1863) ([kitbellew](https://github.com/kitbellew))
- Documentation: add availability for recent params [\#1862](https://github.com/scalameta/scalafmt/pull/1862) ([kitbellew](https://github.com/kitbellew))
- Error.NoMatchingFiles: don't display a stack trace [\#1860](https://github.com/scalameta/scalafmt/pull/1860) ([kitbellew](https://github.com/kitbellew))
- TreeOps: use numParents for nestedApplies/Select [\#1859](https://github.com/scalameta/scalafmt/pull/1859) ([kitbellew](https://github.com/kitbellew))
- Flexible literal formatting [\#1857](https://github.com/scalameta/scalafmt/pull/1857) ([poslegm](https://github.com/poslegm))
- \#1627 \[19\]: Documentation: describe `newlines.afterInfix` [\#1856](https://github.com/scalameta/scalafmt/pull/1856) ([kitbellew](https://github.com/kitbellew))
- \#1627 \[18\]: FormatOps: ignore line breaks in infix expressions [\#1855](https://github.com/scalameta/scalafmt/pull/1855) ([kitbellew](https://github.com/kitbellew))
- Router bugfix: regression in statement/comments [\#1854](https://github.com/scalameta/scalafmt/pull/1854) ([kitbellew](https://github.com/kitbellew))
- FormatOps: implement recursive findToken method [\#1853](https://github.com/scalameta/scalafmt/pull/1853) ([kitbellew](https://github.com/kitbellew))
- Upgrade to the latest Metaconfig [\#1852](https://github.com/scalameta/scalafmt/pull/1852) ([olafurpg](https://github.com/olafurpg))
- Debug: move printing debug info from FormatWriter [\#1851](https://github.com/scalameta/scalafmt/pull/1851) ([kitbellew](https://github.com/kitbellew))
- State: clarify how priority is determined [\#1850](https://github.com/scalameta/scalafmt/pull/1850) ([kitbellew](https://github.com/kitbellew))
- \#1627 \[17\]: Several fixes for ApplyType, symbolic select, if in case clause [\#1849](https://github.com/scalameta/scalafmt/pull/1849) ([kitbellew](https://github.com/kitbellew))
- \#1627 \[16\]: ScalafmtConfig: refactor checking source errors [\#1848](https://github.com/scalameta/scalafmt/pull/1848) ([kitbellew](https://github.com/kitbellew))
- \#1627 \[15\]: Documentation: describe newlines.source [\#1847](https://github.com/scalameta/scalafmt/pull/1847) ([kitbellew](https://github.com/kitbellew))
- FormatOps: don't start a statement on a comment [\#1846](https://github.com/scalameta/scalafmt/pull/1846) ([kitbellew](https://github.com/kitbellew))
- Compiler warnings enabled [\#1843](https://github.com/scalameta/scalafmt/pull/1843) ([poslegm](https://github.com/poslegm))
- Enforce Prettier formatting on markdown sources. [\#1840](https://github.com/scalameta/scalafmt/pull/1840) ([olafurpg](https://github.com/olafurpg))
- Sunset editions [\#1839](https://github.com/scalameta/scalafmt/pull/1839) ([olafurpg](https://github.com/olafurpg))
- Align strip margin: allow disabling the alignment [\#1837](https://github.com/scalameta/scalafmt/pull/1837) ([kitbellew](https://github.com/kitbellew))
- Tests for already fixed SearchStateExploded error \(\#1527\) [\#1836](https://github.com/scalameta/scalafmt/pull/1836) ([poslegm](https://github.com/poslegm))
- TokenOps bugfix: logic for newline on docstring [\#1834](https://github.com/scalameta/scalafmt/pull/1834) ([kitbellew](https://github.com/kitbellew))
- Fixed newlines on NewAnonymous with empty arguments lambda [\#1833](https://github.com/scalameta/scalafmt/pull/1833) ([poslegm](https://github.com/poslegm))
- Hide lambda body squashing under newlines.source=fold [\#1832](https://github.com/scalameta/scalafmt/pull/1832) ([poslegm](https://github.com/poslegm))
- Update sbt-native-packager to 1.7.0 [\#1827](https://github.com/scalameta/scalafmt/pull/1827) ([scala-steward](https://github.com/scala-steward))
- Update scalatest to 3.1.1 [\#1824](https://github.com/scalameta/scalafmt/pull/1824) ([scala-steward](https://github.com/scala-steward))
- Update scalameta, testkit to 4.3.7 [\#1823](https://github.com/scalameta/scalafmt/pull/1823) ([scala-steward](https://github.com/scala-steward))
- Update scalafmt-core to 2.4.2 [\#1822](https://github.com/scalameta/scalafmt/pull/1822) ([scala-steward](https://github.com/scala-steward))
- Update sbt-scalafmt to 2.3.2 [\#1821](https://github.com/scalameta/scalafmt/pull/1821) ([scala-steward](https://github.com/scala-steward))
- Update sbt-mdoc to 2.1.5 [\#1820](https://github.com/scalameta/scalafmt/pull/1820) ([scala-steward](https://github.com/scala-steward))
- Update scala-xml to 1.3.0 [\#1819](https://github.com/scalameta/scalafmt/pull/1819) ([scala-steward](https://github.com/scala-steward))
- Update sbt-scalajs to 1.0.1 [\#1818](https://github.com/scalameta/scalafmt/pull/1818) ([scala-steward](https://github.com/scala-steward))
- Update sbt-native-packager to 1.6.2 [\#1816](https://github.com/scalameta/scalafmt/pull/1816) ([scala-steward](https://github.com/scala-steward))
- Update sbt-mima-plugin to 0.7.0 [\#1815](https://github.com/scalameta/scalafmt/pull/1815) ([scala-steward](https://github.com/scala-steward))
- Update scalatags to 0.8.6 [\#1814](https://github.com/scalameta/scalafmt/pull/1814) ([scala-steward](https://github.com/scala-steward))
- Update metaconfig-core, ... to 0.9.9 [\#1813](https://github.com/scalameta/scalafmt/pull/1813) ([scala-steward](https://github.com/scala-steward))
- Fixed unclear example for danglingParentheses in documentation [\#1812](https://github.com/scalameta/scalafmt/pull/1812) ([poslegm](https://github.com/poslegm))
- \#1627 \[14\]: Router: single-line Defn lambda for fold only [\#1810](https://github.com/scalameta/scalafmt/pull/1810) ([kitbellew](https://github.com/kitbellew))
- \#1627 \[13\]: Router: ignore line breaks in comma-separated args [\#1808](https://github.com/scalameta/scalafmt/pull/1808) ([kitbellew](https://github.com/kitbellew))
- All tests moved into the scalafmt-tests package [\#1807](https://github.com/scalameta/scalafmt/pull/1807) ([dkartashev](https://github.com/dkartashev))
- \#1627 \[12\]: Router: ignore line breaks in enumerators [\#1806](https://github.com/scalameta/scalafmt/pull/1806) ([kitbellew](https://github.com/kitbellew))
- \#1627 \[11\]: Router: relax restrictions on breaks within interpolate [\#1805](https://github.com/scalameta/scalafmt/pull/1805) ([kitbellew](https://github.com/kitbellew))
- \#1627 \[10\]: Router: ignore line breaks in select chains [\#1796](https://github.com/scalameta/scalafmt/pull/1796) ([kitbellew](https://github.com/kitbellew))
- Preserve newlines in for comprehension generators and values [\#1795](https://github.com/scalameta/scalafmt/pull/1795) ([poslegm](https://github.com/poslegm))
- \#1627 \[09\]: Router: ignore line breaks in case clauses [\#1793](https://github.com/scalameta/scalafmt/pull/1793) ([kitbellew](https://github.com/kitbellew))
- \#1627 \[08\]: Router: ignore line breaks in annotations [\#1792](https://github.com/scalameta/scalafmt/pull/1792) ([kitbellew](https://github.com/kitbellew))
- GraalVM Native Image fix [\#1791](https://github.com/scalameta/scalafmt/pull/1791) ([poslegm](https://github.com/poslegm))
- \#1627 \[07\]: FormatOps: ignore line breaks in rhsOptimalToken [\#1790](https://github.com/scalameta/scalafmt/pull/1790) ([kitbellew](https://github.com/kitbellew))
- Router bugfix: handle implicit consistently [\#1789](https://github.com/scalameta/scalafmt/pull/1789) ([kitbellew](https://github.com/kitbellew))
- \#1627 \[06\]: Router: ignore line breaks for config style apply [\#1786](https://github.com/scalameta/scalafmt/pull/1786) ([kitbellew](https://github.com/kitbellew))
- Align lines with different columns count [\#1785](https://github.com/scalameta/scalafmt/pull/1785) ([poslegm](https://github.com/poslegm))
- All tests enabled on CI \(and fixed\) [\#1784](https://github.com/scalameta/scalafmt/pull/1784) ([poslegm](https://github.com/poslegm))
- Add script to test that a release succeeded [\#1782](https://github.com/scalameta/scalafmt/pull/1782) ([olafurpg](https://github.com/olafurpg))
- Document config overriding functionality [\#1781](https://github.com/scalameta/scalafmt/pull/1781) ([poslegm](https://github.com/poslegm))
- Scalameta update [\#1780](https://github.com/scalameta/scalafmt/pull/1780) ([poslegm](https://github.com/poslegm))
- \#1627 \[05\]: Router: ignore line breaks in def/val assignments [\#1779](https://github.com/scalameta/scalafmt/pull/1779) ([kitbellew](https://github.com/kitbellew))
- \#1627 \[04\]: Router: ignore line breaks in if-else and bodies [\#1778](https://github.com/scalameta/scalafmt/pull/1778) ([kitbellew](https://github.com/kitbellew))
- Changelog updated [\#1777](https://github.com/scalameta/scalafmt/pull/1777) ([poslegm](https://github.com/poslegm))
- Router: modify indent of case clauses consistently [\#1776](https://github.com/scalameta/scalafmt/pull/1776) ([kitbellew](https://github.com/kitbellew))
- \#1627 \[03\]: Router: newlines.source=unfold for semicolons [\#1774](https://github.com/scalameta/scalafmt/pull/1774) ([kitbellew](https://github.com/kitbellew))
- "// format: off" enabled for comments, literals, etc. [\#1773](https://github.com/scalameta/scalafmt/pull/1773) ([poslegm](https://github.com/poslegm))
- \#1627 \[02\]: Router: ignore line breaks for blocks [\#1772](https://github.com/scalameta/scalafmt/pull/1772) ([kitbellew](https://github.com/kitbellew))
- BestFirstSearch: check optimal token not to overflow [\#1771](https://github.com/scalameta/scalafmt/pull/1771) ([kitbellew](https://github.com/kitbellew))
- Fetch tags on CI [\#1770](https://github.com/scalameta/scalafmt/pull/1770) ([poslegm](https://github.com/poslegm))
- Fixed contradiction between blankLineBeforeDocstring name and value [\#1768](https://github.com/scalameta/scalafmt/pull/1768) ([poslegm](https://github.com/poslegm))
- Documentation improvements [\#1767](https://github.com/scalameta/scalafmt/pull/1767) ([poslegm](https://github.com/poslegm))
- Router bugfix: extend line break past infix for any block [\#1766](https://github.com/scalameta/scalafmt/pull/1766) ([kitbellew](https://github.com/kitbellew))
- \#1627 \[01\]: ScalafmtConfig: flag to ignore input line breaks [\#1765](https://github.com/scalameta/scalafmt/pull/1765) ([kitbellew](https://github.com/kitbellew))
- Minor refactoring to prepare for \#1627 [\#1764](https://github.com/scalameta/scalafmt/pull/1764) ([kitbellew](https://github.com/kitbellew))
- Router: remove currently unused comma split [\#1763](https://github.com/scalameta/scalafmt/pull/1763) ([kitbellew](https://github.com/kitbellew))
- Fixed AST changes after formatting variant types with symbolic name [\#1762](https://github.com/scalameta/scalafmt/pull/1762) ([poslegm](https://github.com/poslegm))
- DanglingParentheses: move `exclude` list from VerticalMultiline [\#1761](https://github.com/scalameta/scalafmt/pull/1761) ([kitbellew](https://github.com/kitbellew))
- Router: fix \#1749, conflict with \#1755 [\#1760](https://github.com/scalameta/scalafmt/pull/1760) ([kitbellew](https://github.com/kitbellew))
- Alignment by right-side expression [\#1759](https://github.com/scalameta/scalafmt/pull/1759) ([poslegm](https://github.com/poslegm))
- Router: handle implicit in non-vertical-multiline [\#1749](https://github.com/scalameta/scalafmt/pull/1749) ([kitbellew](https://github.com/kitbellew))
- VerticalMultiline: check config style [\#1696](https://github.com/scalameta/scalafmt/pull/1696) ([kitbellew](https://github.com/kitbellew))

## [v2.4.2](https://github.com/scalameta/scalafmt/tree/v2.4.2) (2020-02-22)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.4.1...v2.4.2)

**Merged pull requests:**

- BestFirstSearch: if failed without a solution, retry without optimization [\#1731](https://github.com/scalameta/scalafmt/pull/1731) ([kitbellew](https://github.com/kitbellew))
- Release Drafter workflow [\#1730](https://github.com/scalameta/scalafmt/pull/1730) ([poslegm](https://github.com/poslegm))
- BestFirstSearch: don't give up too soon [\#1729](https://github.com/scalameta/scalafmt/pull/1729) ([kitbellew](https://github.com/kitbellew))
- Various cleanup and refactoring changes in BestFirstSearch [\#1728](https://github.com/scalameta/scalafmt/pull/1728) ([kitbellew](https://github.com/kitbellew))
- Dummy parameter against github caches [\#1727](https://github.com/scalameta/scalafmt/pull/1727) ([poslegm](https://github.com/poslegm))
- GraalVM to 20.0.0 [\#1726](https://github.com/scalameta/scalafmt/pull/1726) ([poslegm](https://github.com/poslegm))
- Documentation: describe a few important parameters [\#1725](https://github.com/scalameta/scalafmt/pull/1725) ([kitbellew](https://github.com/kitbellew))
- FormatOps: extract methods to get func/case arrow [\#1724](https://github.com/scalameta/scalafmt/pull/1724) ([kitbellew](https://github.com/kitbellew))
- RedundantBraces: add parensForOneLineApply flag [\#1723](https://github.com/scalameta/scalafmt/pull/1723) ([kitbellew](https://github.com/kitbellew))
- Router bugfix: find the correct lambda right arrow [\#1719](https://github.com/scalameta/scalafmt/pull/1719) ([kitbellew](https://github.com/kitbellew))
- FormatWriter: don't rewrite {} =\> \(\) in assignment [\#1715](https://github.com/scalameta/scalafmt/pull/1715) ([kitbellew](https://github.com/kitbellew))
- Update scalafmt-core to 2.4.1 [\#1713](https://github.com/scalameta/scalafmt/pull/1713) ([scala-steward](https://github.com/scala-steward))
- TreeOps: structure the details of defns or calls [\#1710](https://github.com/scalameta/scalafmt/pull/1710) ([kitbellew](https://github.com/kitbellew))

## [v2.4.1](https://github.com/scalameta/scalafmt/tree/v2.4.1) (2020-02-16)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.4.0...v2.4.1)

**Merged pull requests:**

- FormatWriter: {} =\> \(\): don't rewrite some lambdas [\#1709](https://github.com/scalameta/scalafmt/pull/1709) ([kitbellew](https://github.com/kitbellew))
- Update scalafmt-core to 2.4.0 [\#1706](https://github.com/scalameta/scalafmt/pull/1706) ([scala-steward](https://github.com/scala-steward))

## [v2.4.0](https://github.com/scalameta/scalafmt/tree/v2.4.0) (2020-02-15)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.4.0-RC2...v2.4.0)

**Merged pull requests:**

- Trailing commas bugfix: can't have trailing comma in `preserve` without a break [\#1705](https://github.com/scalameta/scalafmt/pull/1705) ([kitbellew](https://github.com/kitbellew))
- A couple of bugfixes in handling of first arg after "\(" [\#1701](https://github.com/scalameta/scalafmt/pull/1701) ([kitbellew](https://github.com/kitbellew))
- Various cleanup and refactor changes [\#1700](https://github.com/scalameta/scalafmt/pull/1700) ([kitbellew](https://github.com/kitbellew))
- Update metaconfig-core, ... to 0.9.8 [\#1699](https://github.com/scalameta/scalafmt/pull/1699) ([scala-steward](https://github.com/scala-steward))
- Tests: determine test's location, provide to Intellij [\#1698](https://github.com/scalameta/scalafmt/pull/1698) ([kitbellew](https://github.com/kitbellew))
- MUnit reverted to ScalaTest [\#1697](https://github.com/scalameta/scalafmt/pull/1697) ([poslegm](https://github.com/poslegm))
- Update sbt-scalajs-crossproject to 1.0.0 [\#1695](https://github.com/scalameta/scalafmt/pull/1695) ([scala-steward](https://github.com/scala-steward))
- Update metaconfig-core, ... to 0.9.7 [\#1694](https://github.com/scalameta/scalafmt/pull/1694) ([scala-steward](https://github.com/scala-steward))
- Update metaconfig-core, ... to 0.9.6 [\#1693](https://github.com/scalameta/scalafmt/pull/1693) ([scala-steward](https://github.com/scala-steward))
- Resource folder path resolution from classloader [\#1692](https://github.com/scalameta/scalafmt/pull/1692) ([poslegm](https://github.com/poslegm))
- Policy: add apply\(\) with partial func in 2nd group [\#1691](https://github.com/scalameta/scalafmt/pull/1691) ([kitbellew](https://github.com/kitbellew))
- Router: minor refactor for align vars in apply [\#1690](https://github.com/scalameta/scalafmt/pull/1690) ([kitbellew](https://github.com/kitbellew))
- Router: extract simple T.Dot patterns before chain [\#1689](https://github.com/scalameta/scalafmt/pull/1689) ([kitbellew](https://github.com/kitbellew))
- TreeOps: find a parent, avoid getting all parents [\#1688](https://github.com/scalameta/scalafmt/pull/1688) ([kitbellew](https://github.com/kitbellew))
- Update munit to 0.4.5 [\#1687](https://github.com/scalameta/scalafmt/pull/1687) ([scala-steward](https://github.com/scala-steward))
- Self formatting with 2.4.0-RC2 [\#1686](https://github.com/scalameta/scalafmt/pull/1686) ([poslegm](https://github.com/poslegm))
- ScalaTest -\> MUnit [\#1681](https://github.com/scalameta/scalafmt/pull/1681) ([poslegm](https://github.com/poslegm))

## [v2.4.0-RC2](https://github.com/scalameta/scalafmt/tree/v2.4.0-RC2) (2020-02-09)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.4.0-RC1...v2.4.0-RC2)

**Merged pull requests:**

- Update metaconfig-core, ... to 0.9.5 [\#1685](https://github.com/scalameta/scalafmt/pull/1685) ([scala-steward](https://github.com/scala-steward))
- Update sbt-ci-release to 1.5.2 [\#1684](https://github.com/scalameta/scalafmt/pull/1684) ([scala-steward](https://github.com/scala-steward))
- Update sbt-mima-plugin to 0.6.4 [\#1683](https://github.com/scalameta/scalafmt/pull/1683) ([scala-steward](https://github.com/scala-steward))
- Update scalatags to 0.8.5 [\#1680](https://github.com/scalameta/scalafmt/pull/1680) ([scala-steward](https://github.com/scala-steward))
- RewriteTrailingCommas: trivial, remove duplicate [\#1679](https://github.com/scalameta/scalafmt/pull/1679) ([kitbellew](https://github.com/kitbellew))
- Rewrites: traverse tree once, applying all rules [\#1677](https://github.com/scalameta/scalafmt/pull/1677) ([kitbellew](https://github.com/kitbellew))
- Update sbt-scalajs to 1.0.0 [\#1676](https://github.com/scalameta/scalafmt/pull/1676) ([scala-steward](https://github.com/scala-steward))
- Scalafmt is supported by format-all for emacs [\#1675](https://github.com/scalameta/scalafmt/pull/1675) ([sideshowcoder](https://github.com/sideshowcoder))
- ScalafmtConfig: clean up unused code, move some to tests [\#1674](https://github.com/scalameta/scalafmt/pull/1674) ([kitbellew](https://github.com/kitbellew))
- ScalafmtConfig bugfix: read align tokens as seq, not set [\#1673](https://github.com/scalameta/scalafmt/pull/1673) ([kitbellew](https://github.com/kitbellew))
- Update sbt to 1.3.8 [\#1672](https://github.com/scalameta/scalafmt/pull/1672) ([scala-steward](https://github.com/scala-steward))
- Trailing commas: add a rewrite rule, remove first [\#1669](https://github.com/scalameta/scalafmt/pull/1669) ([kitbellew](https://github.com/kitbellew))
- Router: treat single-arg lambda of init like apply [\#1666](https://github.com/scalameta/scalafmt/pull/1666) ([kitbellew](https://github.com/kitbellew))
- RedundantParens: rewrite single-arg apply of block [\#1665](https://github.com/scalameta/scalafmt/pull/1665) ([kitbellew](https://github.com/kitbellew))
- RedundantBraces: use \(\) for {} one-line lambdas [\#1663](https://github.com/scalameta/scalafmt/pull/1663) ([kitbellew](https://github.com/kitbellew))
- RedundantBraces: update method body rewrite rules [\#1662](https://github.com/scalameta/scalafmt/pull/1662) ([kitbellew](https://github.com/kitbellew))
- Minor improvements to split, decision and policy logic [\#1661](https://github.com/scalameta/scalafmt/pull/1661) ([kitbellew](https://github.com/kitbellew))
- Router: allow single-line curly lambdas [\#1660](https://github.com/scalameta/scalafmt/pull/1660) ([kitbellew](https://github.com/kitbellew))

## [v2.4.0-RC1](https://github.com/scalameta/scalafmt/tree/v2.4.0-RC1) (2020-01-30)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.3.3-RC2...v2.4.0-RC1)

**Merged pull requests:**

- Update sbt-native-packager to 1.6.1 [\#1658](https://github.com/scalameta/scalafmt/pull/1658) ([scala-steward](https://github.com/scala-steward))
- Update sbt-scalafmt to 2.3.1 [\#1657](https://github.com/scalameta/scalafmt/pull/1657) ([scala-steward](https://github.com/scala-steward))
- Enable to download and run snapshot version from dynamic [\#1656](https://github.com/scalameta/scalafmt/pull/1656) ([tanishiking](https://github.com/tanishiking))
- Scalafmt bugfix: restore .format signatures while passing filename [\#1654](https://github.com/scalameta/scalafmt/pull/1654) ([kitbellew](https://github.com/kitbellew))
- Router bugfix: break on else, indent for block within case [\#1652](https://github.com/scalameta/scalafmt/pull/1652) ([kitbellew](https://github.com/kitbellew))
- Policy: always pass sourcecode.Line implicitly [\#1651](https://github.com/scalameta/scalafmt/pull/1651) ([kitbellew](https://github.com/kitbellew))
- Update interface to 0.0.17 [\#1650](https://github.com/scalameta/scalafmt/pull/1650) ([scala-steward](https://github.com/scala-steward))
- SortModifiers: Use .syntax on tokens, not tree [\#1649](https://github.com/scalameta/scalafmt/pull/1649) ([kitbellew](https://github.com/kitbellew))
- State/FormatWriter: various optimizations and simplifications [\#1647](https://github.com/scalameta/scalafmt/pull/1647) ([kitbellew](https://github.com/kitbellew))
- Update sbt-scalajs to 0.6.32 [\#1642](https://github.com/scalameta/scalafmt/pull/1642) ([scala-steward](https://github.com/scala-steward))
- Update sbt-mdoc to 2.1.1 [\#1621](https://github.com/scalameta/scalafmt/pull/1621) ([scala-steward](https://github.com/scala-steward))
- Update paradise to 2.1.1 [\#1620](https://github.com/scalameta/scalafmt/pull/1620) ([scala-steward](https://github.com/scala-steward))

## [v2.3.3-RC2](https://github.com/scalameta/scalafmt/tree/v2.3.3-RC2) (2020-01-22)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.3.3-RC1...v2.3.3-RC2)

**Merged pull requests:**

- Update sbt-native-packager to 1.6.0 [\#1641](https://github.com/scalameta/scalafmt/pull/1641) ([scala-steward](https://github.com/scala-steward))
- Update scalatags to 0.8.4 [\#1636](https://github.com/scalameta/scalafmt/pull/1636) ([scala-steward](https://github.com/scala-steward))
- RedundantBraces: fix various bugs with general expressions [\#1635](https://github.com/scalameta/scalafmt/pull/1635) ([kitbellew](https://github.com/kitbellew))
- Update sbt to 1.3.7 [\#1634](https://github.com/scalameta/scalafmt/pull/1634) ([scala-steward](https://github.com/scala-steward))
- Add headers for each invidiual editor that Metals supports. [\#1629](https://github.com/scalameta/scalafmt/pull/1629) ([olafurpg](https://github.com/olafurpg))
- FormatOps: check "new anonymous" in infix indent [\#1626](https://github.com/scalameta/scalafmt/pull/1626) ([kitbellew](https://github.com/kitbellew))
- Rewrite: rewrite patchsets unless any is no-format [\#1625](https://github.com/scalameta/scalafmt/pull/1625) ([kitbellew](https://github.com/kitbellew))
- Router bugfix: improve optimal token for assign [\#1623](https://github.com/scalameta/scalafmt/pull/1623) ([kitbellew](https://github.com/kitbellew))
- Update scalatags to 0.8.3 [\#1619](https://github.com/scalameta/scalafmt/pull/1619) ([scala-steward](https://github.com/scala-steward))
- add in a bit of documentation about Metals [\#1617](https://github.com/scalameta/scalafmt/pull/1617) ([ckipp01](https://github.com/ckipp01))

## [v2.3.3-RC1](https://github.com/scalameta/scalafmt/tree/v2.3.3-RC1) (2020-01-08)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.3.2...v2.3.3-RC1)

**Merged pull requests:**

- Don't silence stdout when it is the only output [\#1622](https://github.com/scalameta/scalafmt/pull/1622) ([sideshowcoder](https://github.com/sideshowcoder))
- Some performance optimizations for FormatToken [\#1616](https://github.com/scalameta/scalafmt/pull/1616) ([kitbellew](https://github.com/kitbellew))
- Various refactoring and simplification commits [\#1615](https://github.com/scalameta/scalafmt/pull/1615) ([kitbellew](https://github.com/kitbellew))
- Router bugfix: keep single-line comment split [\#1614](https://github.com/scalameta/scalafmt/pull/1614) ([kitbellew](https://github.com/kitbellew))
- Update sbt-ci-release to 1.5.0 [\#1613](https://github.com/scalameta/scalafmt/pull/1613) ([scala-steward](https://github.com/scala-steward))
- Various bugfixes for single-arg apply of a lambda [\#1612](https://github.com/scalameta/scalafmt/pull/1612) ([kitbellew](https://github.com/kitbellew))
- Update sbt-mdoc to 2.1.0 [\#1611](https://github.com/scalameta/scalafmt/pull/1611) ([scala-steward](https://github.com/scala-steward))
- Update sbt to 1.3.6 [\#1610](https://github.com/scalameta/scalafmt/pull/1610) ([scala-steward](https://github.com/scala-steward))
- FormatOps: allow break after assign in infixSplit [\#1608](https://github.com/scalameta/scalafmt/pull/1608) ([kitbellew](https://github.com/kitbellew))
- FormatOps: consider comment for RHS optimal token [\#1604](https://github.com/scalameta/scalafmt/pull/1604) ([kitbellew](https://github.com/kitbellew))
- Update scalacheck to 1.14.3 [\#1602](https://github.com/scalameta/scalafmt/pull/1602) ([scala-steward](https://github.com/scala-steward))
- Update sbt to 1.3.5 [\#1601](https://github.com/scalameta/scalafmt/pull/1601) ([scala-steward](https://github.com/scala-steward))
- Router: dangle only if breaking in multi-line lambda [\#1600](https://github.com/scalameta/scalafmt/pull/1600) ([kitbellew](https://github.com/kitbellew))
- Update sbt-native-packager to 1.5.2 [\#1598](https://github.com/scalameta/scalafmt/pull/1598) ([scala-steward](https://github.com/scala-steward))
- Update scalafmt-core to 2.3.2 [\#1597](https://github.com/scalameta/scalafmt/pull/1597) ([scala-steward](https://github.com/scala-steward))
- Update sbt-scalafmt to 2.3.0 [\#1596](https://github.com/scalameta/scalafmt/pull/1596) ([scala-steward](https://github.com/scala-steward))
- Upgrade to Scalatest 3.1 [\#1595](https://github.com/scalameta/scalafmt/pull/1595) ([olafurpg](https://github.com/olafurpg))
- Router: preserve single-line formatting for if/try [\#1560](https://github.com/scalameta/scalafmt/pull/1560) ([kitbellew](https://github.com/kitbellew))

## [v2.3.2](https://github.com/scalameta/scalafmt/tree/v2.3.2) (2019-12-08)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.3.1...v2.3.2)

**Merged pull requests:**

- Do not add a new line in multilevel chained package clause [\#1594](https://github.com/scalameta/scalafmt/pull/1594) ([iRevive](https://github.com/iRevive))
- Update scalafmt-core to 2.3.1 [\#1590](https://github.com/scalameta/scalafmt/pull/1590) ([scala-steward](https://github.com/scala-steward))
- Update interface to 0.0.16 [\#1589](https://github.com/scalameta/scalafmt/pull/1589) ([scala-steward](https://github.com/scala-steward))
- adding possibility to specify custom repositories for coursier \(\#1521\) [\#1586](https://github.com/scalameta/scalafmt/pull/1586) ([slivkamiro](https://github.com/slivkamiro))

## [v2.3.1](https://github.com/scalameta/scalafmt/tree/v2.3.1) (2019-12-06)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.3.0...v2.3.1)

**Merged pull requests:**

- RedundantBraces: fix how we find {} in func body [\#1588](https://github.com/scalameta/scalafmt/pull/1588) ([kitbellew](https://github.com/kitbellew))

## [v2.3.0](https://github.com/scalameta/scalafmt/tree/v2.3.0) (2019-12-04)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.3.0-RC2...v2.3.0)

**Merged pull requests:**

- Update sbt-mdoc to 2.0.3 [\#1585](https://github.com/scalameta/scalafmt/pull/1585) ([scala-steward](https://github.com/scala-steward))
- Update typesafe:config to 1.4.0 [\#1584](https://github.com/scalameta/scalafmt/pull/1584) ([scala-steward](https://github.com/scala-steward))
- Update sbt-jmh to 0.3.7 [\#1581](https://github.com/scalameta/scalafmt/pull/1581) ([scala-steward](https://github.com/scala-steward))
- Update paiges-core to 0.3.0 [\#1580](https://github.com/scalameta/scalafmt/pull/1580) ([scala-steward](https://github.com/scala-steward))
- Update sbt-mdoc to 2.0.2 [\#1579](https://github.com/scalameta/scalafmt/pull/1579) ([scala-steward](https://github.com/scala-steward))
- Update sbt-scalajs to 0.6.31 [\#1578](https://github.com/scalameta/scalafmt/pull/1578) ([scala-steward](https://github.com/scala-steward))
- Update sbt-scalajs-crossproject to 0.6.1 [\#1577](https://github.com/scalameta/scalafmt/pull/1577) ([scala-steward](https://github.com/scala-steward))
- Update sbt-coursier to 1.0.3 [\#1576](https://github.com/scalameta/scalafmt/pull/1576) ([scala-steward](https://github.com/scala-steward))
- Update interface to 0.0.14 [\#1575](https://github.com/scalameta/scalafmt/pull/1575) ([scala-steward](https://github.com/scala-steward))
- Update sbt-mima-plugin to 0.6.1 [\#1574](https://github.com/scalameta/scalafmt/pull/1574) ([scala-steward](https://github.com/scala-steward))
- Update typesafe:config to 1.3.4 [\#1573](https://github.com/scalameta/scalafmt/pull/1573) ([scala-steward](https://github.com/scala-steward))
- Update sbt-buildinfo to 0.9.0 [\#1572](https://github.com/scalameta/scalafmt/pull/1572) ([scala-steward](https://github.com/scala-steward))
- Update sbt-assembly to 0.14.10 [\#1571](https://github.com/scalameta/scalafmt/pull/1571) ([scala-steward](https://github.com/scala-steward))
- Add installation instructions for scalafmt-native. [\#1570](https://github.com/scalameta/scalafmt/pull/1570) ([olafurpg](https://github.com/olafurpg))
- Router: break between curly and catch, like else [\#1567](https://github.com/scalameta/scalafmt/pull/1567) ([kitbellew](https://github.com/kitbellew))
- Formatter upgraded to 2.3.0-RC1 [\#1566](https://github.com/scalameta/scalafmt/pull/1566) ([poslegm](https://github.com/poslegm))

## [v2.3.0-RC2](https://github.com/scalameta/scalafmt/tree/v2.3.0-RC2) (2019-11-27)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.3.0-RC1...v2.3.0-RC2)

## [v2.3.0-RC1](https://github.com/scalameta/scalafmt/tree/v2.3.0-RC1) (2019-11-27)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.2.2...v2.3.0-RC1)

**Merged pull requests:**

- Upgrade to the latest Scalameta [\#1565](https://github.com/scalameta/scalafmt/pull/1565) ([olafurpg](https://github.com/olafurpg))
- Performance improvements [\#1564](https://github.com/scalameta/scalafmt/pull/1564) ([olafurpg](https://github.com/olafurpg))
- RedundantBraces: lambda braces to single-arg apply, remove arou… [\#1563](https://github.com/scalameta/scalafmt/pull/1563) ([kitbellew](https://github.com/kitbellew))
- Router: fix split rule after catch in try [\#1562](https://github.com/scalameta/scalafmt/pull/1562) ([kitbellew](https://github.com/kitbellew))
- Setup GitHub Actions to generate native images [\#1561](https://github.com/scalameta/scalafmt/pull/1561) ([olafurpg](https://github.com/olafurpg))
- Remove irregular files, including symbolic links, from `git ls-files`. [\#1559](https://github.com/scalameta/scalafmt/pull/1559) ([SamirTalwar](https://github.com/SamirTalwar))
- Router: fix def body indent with a comment \#1240 [\#1556](https://github.com/scalameta/scalafmt/pull/1556) ([kitbellew](https://github.com/kitbellew))
- FormatWriter: insert newline after packages \#1069 [\#1555](https://github.com/scalameta/scalafmt/pull/1555) ([kitbellew](https://github.com/kitbellew))
- Router: treat try/finally the same as if/else \#350 [\#1554](https://github.com/scalameta/scalafmt/pull/1554) ([kitbellew](https://github.com/kitbellew))
- Make it easier to test scalafmt local snapshot build [\#1552](https://github.com/scalameta/scalafmt/pull/1552) ([tanishiking](https://github.com/tanishiking))
- Router: format lambda in 1-arg call as with braces [\#1551](https://github.com/scalameta/scalafmt/pull/1551) ([kitbellew](https://github.com/kitbellew))
- RedundantBraces: remove nested braces in lambda [\#1549](https://github.com/scalameta/scalafmt/pull/1549) ([kitbellew](https://github.com/kitbellew))
- Revert avoidEmptyLinesAroundBlock feature [\#1548](https://github.com/scalameta/scalafmt/pull/1548) ([poslegm](https://github.com/poslegm))

## [v2.2.2](https://github.com/scalameta/scalafmt/tree/v2.2.2) (2019-10-29)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.2.1...v2.2.2)

**Merged pull requests:**

- Discord label in README [\#1545](https://github.com/scalameta/scalafmt/pull/1545) ([poslegm](https://github.com/poslegm))
- Edition docs fix [\#1544](https://github.com/scalameta/scalafmt/pull/1544) ([poslegm](https://github.com/poslegm))
- Remove `avoidEmptyLinesAroundBlock` option [\#1541](https://github.com/scalameta/scalafmt/pull/1541) ([olafurpg](https://github.com/olafurpg))
- sbt plugin version update [\#1537](https://github.com/scalameta/scalafmt/pull/1537) ([poslegm](https://github.com/poslegm))
- Fix inconsistent spaces with verticalMultiline and spaces.inParentheses [\#1529](https://github.com/scalameta/scalafmt/pull/1529) ([sirmax](https://github.com/sirmax))
- Avoid empty lines in the beginning and end of blocks [\#1431](https://github.com/scalameta/scalafmt/pull/1431) ([tdidriksen](https://github.com/tdidriksen))

## [v2.2.1](https://github.com/scalameta/scalafmt/tree/v2.2.1) (2019-10-21)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.2.0...v2.2.1)

**Merged pull requests:**

- Do not add space between constructor parameters [\#1535](https://github.com/scalameta/scalafmt/pull/1535) ([tanishiking](https://github.com/tanishiking))
- Welcome Mikhail Chugunkov to the team! [\#1533](https://github.com/scalameta/scalafmt/pull/1533) ([olafurpg](https://github.com/olafurpg))

## [v2.2.0](https://github.com/scalameta/scalafmt/tree/v2.2.0) (2019-10-17)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.1.1...v2.2.0)

**Merged pull requests:**

- sbt-mdoc update [\#1532](https://github.com/scalameta/scalafmt/pull/1532) ([poslegm](https://github.com/poslegm))
- Update Scala to 2.13 [\#1522](https://github.com/scalameta/scalafmt/pull/1522) ([poslegm](https://github.com/poslegm))

## [v2.1.1](https://github.com/scalameta/scalafmt/tree/v2.1.1) (2019-10-11)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.1.0...v2.1.1)

**Merged pull requests:**

- Use Coursier defaults, which also adds support for COURSIER_REPOSITORIES [\#1524](https://github.com/scalameta/scalafmt/pull/1524) ([henricook](https://github.com/henricook))
- Insert newline for else keyword, fixes \#1509. [\#1520](https://github.com/scalameta/scalafmt/pull/1520) ([olafurpg](https://github.com/olafurpg))
- Add a space between constructor annotations and their parameter lists [\#1516](https://github.com/scalameta/scalafmt/pull/1516) ([dominics](https://github.com/dominics))

## [v2.1.0](https://github.com/scalameta/scalafmt/tree/v2.1.0) (2019-09-30)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.1.0-RC2...v2.1.0)

**Merged pull requests:**

- Fixed bug with private\[x\] after class definitions \(\#1491\) [\#1513](https://github.com/scalameta/scalafmt/pull/1513) ([poslegm](https://github.com/poslegm))
- typo danglingParentheses [\#1508](https://github.com/scalameta/scalafmt/pull/1508) ([ilyakharlamov](https://github.com/ilyakharlamov))

## [v2.1.0-RC2](https://github.com/scalameta/scalafmt/tree/v2.1.0-RC2) (2019-09-26)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.1.0-RC1...v2.1.0-RC2)

**Merged pull requests:**

- Use coursier/interface instead of coursier-small [\#1511](https://github.com/scalameta/scalafmt/pull/1511) ([olafurpg](https://github.com/olafurpg))
- With chains alignment for "new" with mixins [\#1503](https://github.com/scalameta/scalafmt/pull/1503) ([poslegm](https://github.com/poslegm))
- Remove scalafmt-intellij [\#1500](https://github.com/scalameta/scalafmt/pull/1500) ([tanishiking](https://github.com/tanishiking))
- Update dependencies [\#1497](https://github.com/scalameta/scalafmt/pull/1497) ([tanishiking](https://github.com/tanishiking))
- Updated sbt plugin version on the website [\#1494](https://github.com/scalameta/scalafmt/pull/1494) ([poslegm](https://github.com/poslegm))
- Fixed alwaysBeforeTopLevelStatements for modifiers [\#1489](https://github.com/scalameta/scalafmt/pull/1489) ([poslegm](https://github.com/poslegm))
- Make comment blocks line up. [\#1488](https://github.com/scalameta/scalafmt/pull/1488) ([adampauls](https://github.com/adampauls))

## [v2.1.0-RC1](https://github.com/scalameta/scalafmt/tree/v2.1.0-RC1) (2019-08-25)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.0.1...v2.1.0-RC1)

**Merged pull requests:**

- Add release-drafter.yml [\#1483](https://github.com/scalameta/scalafmt/pull/1483) ([tanishiking](https://github.com/tanishiking))
- Tweak a test that makes sure multiple exclude flags works as expected [\#1481](https://github.com/scalameta/scalafmt/pull/1481) ([tanishiking](https://github.com/tanishiking))
- Don't treat commas in lambda bodies as "trailing commas" [\#1479](https://github.com/scalameta/scalafmt/pull/1479) ([adampauls](https://github.com/adampauls))
- Add --check cli option [\#1478](https://github.com/scalameta/scalafmt/pull/1478) ([droptheplot](https://github.com/droptheplot))
- Update ISSUE_TEMPLATE to current version support [\#1476](https://github.com/scalameta/scalafmt/pull/1476) ([anilkumarmyla](https://github.com/anilkumarmyla))
- Update CHANGELOG [\#1475](https://github.com/scalameta/scalafmt/pull/1475) ([tanishiking](https://github.com/tanishiking))
- fix running with the diff branch file filter [\#1472](https://github.com/scalameta/scalafmt/pull/1472) ([stephennancekivell](https://github.com/stephennancekivell))
- Keep indentation for comments within method chain [\#1470](https://github.com/scalameta/scalafmt/pull/1470) ([droptheplot](https://github.com/droptheplot))
- Allow for multiple --exclude flags [\#1469](https://github.com/scalameta/scalafmt/pull/1469) ([sortega](https://github.com/sortega))
- fix include resolution for cli and sbt plugin [\#1450](https://github.com/scalameta/scalafmt/pull/1450) ([nadavwr](https://github.com/nadavwr))
- Fixed unindentTopLevelOperator behavior without indentOperator=spray [\#1440](https://github.com/scalameta/scalafmt/pull/1440) ([poslegm](https://github.com/poslegm))

## [v2.0.1](https://github.com/scalameta/scalafmt/tree/v2.0.0) (2019-08-12)

This release contains some new features and fixes for regressions.
[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.0.0...HEAD)

### New features

**`--list` option**
When you want to check if your files are formatted, you can run scalafmt with the `--list` flag. This will output a list of unformatted files if any.

```
$ scalafmt --list
```

if some of the files require re-formatting, scalafmt print the list of those files and return exit code 1.

```
src/main/scala/file1.scala
src/main/scala/file1.scala
```

**Merged pull requests:**

- Return exit code 1 for `scalafmt --list` if there are any files that require re-formatting [\#1474](https://github.com/scalameta/scalafmt/pull/1474) ([tanishiking](https://github.com/tanishiking))
- dont uppercase long hex literal 0x prefix [\#1473](https://github.com/scalameta/scalafmt/pull/1473) ([stephennancekivell](https://github.com/stephennancekivell))
- Use openjdk8 instead of oraclejdk8 [\#1471](https://github.com/scalameta/scalafmt/pull/1471) ([tanishiking](https://github.com/tanishiking))
- do not cache for ever invalid configs [\#1467](https://github.com/scalameta/scalafmt/pull/1467) ([bjaglin](https://github.com/bjaglin))
- Add --list cli option \#1459 [\#1466](https://github.com/scalameta/scalafmt/pull/1466) ([droptheplot](https://github.com/droptheplot))
- Update the release process document, and upgrade docusaurus [\#1452](https://github.com/scalameta/scalafmt/pull/1452) ([tanishiking](https://github.com/tanishiking))
- Update .gitignore for metals and bloop [\#1451](https://github.com/scalameta/scalafmt/pull/1451) ([tanishiking](https://github.com/tanishiking))
- Suppress cli output in case option was set [\#1449](https://github.com/scalameta/scalafmt/pull/1449) ([stremlenye](https://github.com/stremlenye))
- exclude dangling parens in methods with enabled verticalMultiline [\#1435](https://github.com/scalameta/scalafmt/pull/1435) ([darl](https://github.com/darl))
- Prints out the paths of files subjected to formating before the beginning of formatting in debug mode [\#1422](https://github.com/scalameta/scalafmt/pull/1422) ([stremlenye](https://github.com/stremlenye))

## [v2.0.0-RC8](https://github.com/scalameta/scalafmt/tree/v2.0.0-RC8) (2019-06-06)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.0.0-RC7...v2.0.0-RC8)

**Merged pull requests:**

- Fixed error with includeNoParensInSelectChains and full qualified names [\#1429](https://github.com/scalameta/scalafmt/pull/1429) ([poslegm](https://github.com/poslegm))
- Fixed spaces.beforeContextBoundColon=IfMultipleBounds behavior for case with subtyping [\#1428](https://github.com/scalameta/scalafmt/pull/1428) ([poslegm](https://github.com/poslegm))
- Fixed bug with RedundantBraces for string interpolation [\#1425](https://github.com/scalameta/scalafmt/pull/1425) ([poslegm](https://github.com/poslegm))
- Remove space for variance annotations with underscore [\#1419](https://github.com/scalameta/scalafmt/pull/1419) ([vlovgr](https://github.com/vlovgr))
- Add the CLI option to fetch only recently changed files for formating. [\#1416](https://github.com/scalameta/scalafmt/pull/1416) ([stremlenye](https://github.com/stremlenye))
- Use a simple cache based on futures to avoid redownloading on concurrent usage [\#1384](https://github.com/scalameta/scalafmt/pull/1384) ([jrudolph](https://github.com/jrudolph))
- Upgrade IntelliJ plugin [\#1381](https://github.com/scalameta/scalafmt/pull/1381) ([olafurpg](https://github.com/olafurpg))
- Upgrade to latest scalafmt. [\#1380](https://github.com/scalameta/scalafmt/pull/1380) ([olafurpg](https://github.com/olafurpg))
- Update changelog and document release process [\#1379](https://github.com/scalameta/scalafmt/pull/1379) ([olafurpg](https://github.com/olafurpg))
- Avoid newlines after yield keyword. [\#1378](https://github.com/scalameta/scalafmt/pull/1378) ([olafurpg](https://github.com/olafurpg))
- Update website for sbt-scalafmt 2.0.0-RC5 [\#1377](https://github.com/scalameta/scalafmt/pull/1377) ([tanishiking](https://github.com/tanishiking))

## [v2.0.0-RC6](https://github.com/scalameta/scalafmt/tree/v2.0.0-RC6) (2019-04-04)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.0.0-RC5...v2.0.0-RC6)

**Merged pull requests:**

- Add flags to support apache/spark style of configuration [\#1395](https://github.com/scalameta/scalafmt/pull/1395) ([lihaoyi-databricks](https://github.com/lihaoyi-databricks))
- Update changelog and document release process [\#1379](https://github.com/scalameta/scalafmt/pull/1379) ([olafurpg](https://github.com/olafurpg))
- Avoid newlines after yield keyword. [\#1378](https://github.com/scalameta/scalafmt/pull/1378) ([olafurpg](https://github.com/olafurpg))

## [v2.0.0-RC5](https://github.com/scalameta/scalafmt/tree/v2.0.0-RC5) (2019-02-28)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.0.0-RC4...v2.0.0-RC5)

**Merged pull requests:**

- Indent Operator Fix [\#1372](https://github.com/scalameta/scalafmt/pull/1372) ([er1c](https://github.com/er1c))
- Indent Operator Tests [\#1371](https://github.com/scalameta/scalafmt/pull/1371) ([er1c](https://github.com/er1c))
- 1344 api changes to integrate with intellij [\#1368](https://github.com/scalameta/scalafmt/pull/1368) ([unkarjedy](https://github.com/unkarjedy))
- Make scalafmt-cli to use scalafmt-dynamic module instead of depending only on a specific version of scalafmt-core. [\#1366](https://github.com/scalameta/scalafmt/pull/1366) ([tanishiking](https://github.com/tanishiking))
- Minor tweak for java11 .lines not getting picked up via StringOps in scala-dynamic testfile DynamicSuite.scala [\#1365](https://github.com/scalameta/scalafmt/pull/1365) ([er1c](https://github.com/er1c))
- Welcome Rikito Taniguchi to the team! [\#1363](https://github.com/scalameta/scalafmt/pull/1363) ([olafurpg](https://github.com/olafurpg))
- Make ScalafmtReflect format any files if respectProjectFilters=false [\#1361](https://github.com/scalameta/scalafmt/pull/1361) ([tanishiking](https://github.com/tanishiking))
- sbt [\#1359](https://github.com/scalameta/scalafmt/pull/1359) ([dwijnand](https://github.com/dwijnand))
- GitHub [\#1358](https://github.com/scalameta/scalafmt/pull/1358) ([dwijnand](https://github.com/dwijnand))
- No more extempore2 [\#1357](https://github.com/scalameta/scalafmt/pull/1357) ([dwijnand](https://github.com/dwijnand))
- Fix a typo [\#1356](https://github.com/scalameta/scalafmt/pull/1356) ([dwijnand](https://github.com/dwijnand))
- Switch to non-legacy align = more [\#1355](https://github.com/scalameta/scalafmt/pull/1355) ([dwijnand](https://github.com/dwijnand))
- Switch from bintray to sonatype snapshots [\#1354](https://github.com/scalameta/scalafmt/pull/1354) ([dwijnand](https://github.com/dwijnand))
- Justify discouraging format on compile [\#1353](https://github.com/scalameta/scalafmt/pull/1353) ([dwijnand](https://github.com/dwijnand))
- Clarify scalafmtConfig's default vs None [\#1352](https://github.com/scalameta/scalafmt/pull/1352) ([dwijnand](https://github.com/dwijnand))
- Fix sbt plugin Maven Central badge [\#1350](https://github.com/scalameta/scalafmt/pull/1350) ([dwijnand](https://github.com/dwijnand))
- Update Coursier CLI link [\#1348](https://github.com/scalameta/scalafmt/pull/1348) ([er1c](https://github.com/er1c))
- Added Arch Linux installation instruction on website [\#1347](https://github.com/scalameta/scalafmt/pull/1347) ([poslegm](https://github.com/poslegm))

## [v2.0.0-RC4](https://github.com/scalameta/scalafmt/tree/v2.0.0-RC4) (2019-01-07)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.0.0-RC3...v2.0.0-RC4)

**Merged pull requests:**

- Report range positions for parse errors [\#1341](https://github.com/scalameta/scalafmt/pull/1341) ([olafurpg](https://github.com/olafurpg))

## [v2.0.0-RC3](https://github.com/scalameta/scalafmt/tree/v2.0.0-RC3) (2019-01-07)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.0.0-RC2...v2.0.0-RC3)

**Merged pull requests:**

- Add custom reporter endpoint for missing scalafmt version [\#1340](https://github.com/scalameta/scalafmt/pull/1340) ([olafurpg](https://github.com/olafurpg))

## [v2.0.0-RC2](https://github.com/scalameta/scalafmt/tree/v2.0.0-RC2) (2019-01-07)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v2.0.0-RC1...v2.0.0-RC2)

**Merged pull requests:**

- Change publish organization to org.scalameta from com.geirsson [\#1339](https://github.com/scalameta/scalafmt/pull/1339) ([olafurpg](https://github.com/olafurpg))
- Add release notes for v2.0 [\#1338](https://github.com/scalameta/scalafmt/pull/1338) ([olafurpg](https://github.com/olafurpg))

## [v2.0.0-RC1](https://github.com/scalameta/scalafmt/tree/v2.0.0-RC1) (2019-01-07)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v1.6.0-RC4...v2.0.0-RC1)

**Merged pull requests:**

- Add scalafmt-dynamic module for easier build tool integrations. [\#1337](https://github.com/scalameta/scalafmt/pull/1337) ([olafurpg](https://github.com/olafurpg))
- Type Annotation Site indention same as Definition Site [\#1336](https://github.com/scalameta/scalafmt/pull/1336) ([AesaKamar](https://github.com/AesaKamar))
- Fix the mysterious case of missing newline at end of file. [\#1333](https://github.com/scalameta/scalafmt/pull/1333) ([olafurpg](https://github.com/olafurpg))
- Expand on the align=none protip [\#1331](https://github.com/scalameta/scalafmt/pull/1331) ([dwijnand](https://github.com/dwijnand))
- Fix broken links README [\#1330](https://github.com/scalameta/scalafmt/pull/1330) ([glammers1](https://github.com/glammers1))
- Add Continuation Indentation for Type Annotation Site [\#1324](https://github.com/scalameta/scalafmt/pull/1324) ([AesaKamar](https://github.com/AesaKamar))
- avoid generating invalid scala code [\#1323](https://github.com/scalameta/scalafmt/pull/1323) ([woparry](https://github.com/woparry))
- Update sbt plugin docs [\#1322](https://github.com/scalameta/scalafmt/pull/1322) ([cb372](https://github.com/cb372))
- Preferences changed to Settings. [\#1319](https://github.com/scalameta/scalafmt/pull/1319) ([P3trur0](https://github.com/P3trur0))
- Clarify "use intellij formatter" [\#1315](https://github.com/scalameta/scalafmt/pull/1315) ([olafurpg](https://github.com/olafurpg))
- Update docs to clarify installation [\#1314](https://github.com/scalameta/scalafmt/pull/1314) ([olafurpg](https://github.com/olafurpg))
- Add includeNoParensInSelectChains [\#1310](https://github.com/scalameta/scalafmt/pull/1310) ([nkouevda](https://github.com/nkouevda))
- Fix configuration example for continuationIndent.callSite in document [\#1308](https://github.com/scalameta/scalafmt/pull/1308) ([tanishiking](https://github.com/tanishiking))
- add JDK 11 to Travis-CI matrix [\#1304](https://github.com/scalameta/scalafmt/pull/1304) ([SethTisue](https://github.com/SethTisue))
- Enable sbt-idea-plugin in intellij project [\#1301](https://github.com/scalameta/scalafmt/pull/1301) ([olafurpg](https://github.com/olafurpg))
- Add test for preserving location of comment when removing trailing comma [\#1296](https://github.com/scalameta/scalafmt/pull/1296) ([tanishiking](https://github.com/tanishiking))
- Upgrade mdoc and generate docs in PRs [\#1295](https://github.com/scalameta/scalafmt/pull/1295) ([olafurpg](https://github.com/olafurpg))
- Upgrade scala to 2.12.7 [\#1294](https://github.com/scalameta/scalafmt/pull/1294) ([aaabramov](https://github.com/aaabramov))
- Add test for https://github.com/scalameta/scalafmt/issues/1176 [\#1287](https://github.com/scalameta/scalafmt/pull/1287) ([tanishiking](https://github.com/tanishiking))
- Revert fix for unindentTopLevelOperators, acknowledge limitation instead [\#1286](https://github.com/scalameta/scalafmt/pull/1286) ([olafurpg](https://github.com/olafurpg))
- Add explicit dependency on org.scala-lang/scala-reflect [\#1284](https://github.com/scalameta/scalafmt/pull/1284) ([tanishiking](https://github.com/tanishiking))
- Scalameta 4.0.0 [\#1283](https://github.com/scalameta/scalafmt/pull/1283) ([eed3si9n](https://github.com/eed3si9n))
- Bring back the docs for sbt-scalafmt [\#1277](https://github.com/scalameta/scalafmt/pull/1277) ([tanishiking](https://github.com/tanishiking))
- Fix quote in homepage [\#1276](https://github.com/scalameta/scalafmt/pull/1276) ([gabro](https://github.com/gabro))
- Upgrade to sbt-docusaurus v0.2 [\#1275](https://github.com/scalameta/scalafmt/pull/1275) ([olafurpg](https://github.com/olafurpg))
- Enable search in website [\#1274](https://github.com/scalameta/scalafmt/pull/1274) ([gabro](https://github.com/gabro))
- Use sbt-docusaurus 0.1.2 [\#1273](https://github.com/scalameta/scalafmt/pull/1273) ([gabro](https://github.com/gabro))
- Publish website with CI [\#1271](https://github.com/scalameta/scalafmt/pull/1271) ([gabro](https://github.com/gabro))
- Add website landing page [\#1270](https://github.com/scalameta/scalafmt/pull/1270) ([gabro](https://github.com/gabro))
- Enable builds under GraalVM [\#1266](https://github.com/scalameta/scalafmt/pull/1266) ([ssaavedra](https://github.com/ssaavedra))
- Fix trailing commas always/never not to put/delete trailing comma on incorrect location [\#1262](https://github.com/scalameta/scalafmt/pull/1262) ([tanishiking](https://github.com/tanishiking))
- Fix jvm options [\#1261](https://github.com/scalameta/scalafmt/pull/1261) ([jiminhsieh](https://github.com/jiminhsieh))
- Vertical align adjacent single lines of comment [\#1260](https://github.com/scalameta/scalafmt/pull/1260) ([tanishiking](https://github.com/tanishiking))
- Cleanup empty parentheses check [\#1258](https://github.com/scalameta/scalafmt/pull/1258) ([gabro](https://github.com/gabro))
- Add ignore and allElementsOf to AvoidInfix [\#1257](https://github.com/scalameta/scalafmt/pull/1257) ([joan38](https://github.com/joan38))
- Update Adopters.scala [\#1253](https://github.com/scalameta/scalafmt/pull/1253) ([alodavi](https://github.com/alodavi))
- Fix the typos detected by github.com/client9/misspell [\#1251](https://github.com/scalameta/scalafmt/pull/1251) ([seratch](https://github.com/seratch))
- Simplify build [\#1250](https://github.com/scalameta/scalafmt/pull/1250) ([olafurpg](https://github.com/olafurpg))
- Add codacy to adopters list [\#1248](https://github.com/scalameta/scalafmt/pull/1248) ([bmbferreira](https://github.com/bmbferreira))
- Upgrade sbt to 1.2.0 [\#1247](https://github.com/scalameta/scalafmt/pull/1247) ([tanishiking](https://github.com/tanishiking))
- Fix combination of trailingCommas=always and verticalMultilineAtDefnSite=true [\#1241](https://github.com/scalameta/scalafmt/pull/1241) ([tanishiking](https://github.com/tanishiking))
- Fix invalid rewrite of trailingCommas=always [\#1239](https://github.com/scalameta/scalafmt/pull/1239) ([tanishiking](https://github.com/tanishiking))
- Add blurb about comment wrapping [\#1235](https://github.com/scalameta/scalafmt/pull/1235) ([calebharris](https://github.com/calebharris))
- Move website to markdown + docusaurus [\#1199](https://github.com/scalameta/scalafmt/pull/1199) ([gabro](https://github.com/gabro))

## [v1.6.0-RC4](https://github.com/scalameta/scalafmt/tree/v1.6.0-RC4) (2018-07-09)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v1.6.0-RC3...v1.6.0-RC4)

**Merged pull requests:**

- Fix trailing comma handling when followed by indenation-free comment [\#1230](https://github.com/scalameta/scalafmt/pull/1230) ([gabro](https://github.com/gabro))
- Add foursquare to adopters list [\#1229](https://github.com/scalameta/scalafmt/pull/1229) ([iantabolt](https://github.com/iantabolt))
- Upgrade metaconfig [\#1226](https://github.com/scalameta/scalafmt/pull/1226) ([olafurpg](https://github.com/olafurpg))

## [v1.6.0-RC3](https://github.com/scalameta/scalafmt/tree/v1.6.0-RC3) (2018-06-10)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v1.6.0-RC2...v1.6.0-RC3)

**Merged pull requests:**

- First steps to shading sbt-scalafmt [\#1218](https://github.com/scalameta/scalafmt/pull/1218) ([olafurpg](https://github.com/olafurpg))

## [v1.6.0-RC2](https://github.com/scalameta/scalafmt/tree/v1.6.0-RC2) (2018-06-05)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v1.6.0-RC1...v1.6.0-RC2)

**Merged pull requests:**

- Support `align.tokens={none,some,more,most}` [\#1215](https://github.com/scalameta/scalafmt/pull/1215) ([olafurpg](https://github.com/olafurpg))
- Change default to `trailingCommas = never` [\#1214](https://github.com/scalameta/scalafmt/pull/1214) ([olafurpg](https://github.com/olafurpg))
- Avoid lookup up match for non-Token.Left{Brace,Paren} [\#1213](https://github.com/scalameta/scalafmt/pull/1213) ([olafurpg](https://github.com/olafurpg))
- Re-add fromHoconString overload to please neo-scalafmt [\#1212](https://github.com/scalameta/scalafmt/pull/1212) ([olafurpg](https://github.com/olafurpg))
- Don't crash on isWindows check, fixes \#1207 [\#1211](https://github.com/scalameta/scalafmt/pull/1211) ([olafurpg](https://github.com/olafurpg))

## [v1.6.0-RC1](https://github.com/scalameta/scalafmt/tree/v1.6.0-RC1) (2018-05-27)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v1.5.1...v1.6.0-RC1)

**Merged pull requests:**

- Upgrade to the latest metaconfig [\#1145](https://github.com/scalameta/scalafmt/pull/1145) ([olafurpg](https://github.com/olafurpg))
- Enable dangling parentheses and disable align.openParen by default [\#1198](https://github.com/scalameta/scalafmt/pull/1198) ([olafurpg](https://github.com/olafurpg))
- Unskip trailing commas tests, fixes \#1174 [\#1197](https://github.com/scalameta/scalafmt/pull/1197) ([olafurpg](https://github.com/olafurpg))
- Indent commas in val patterns by continuationIndent.defnSite [\#1192](https://github.com/scalameta/scalafmt/pull/1192) ([olafurpg](https://github.com/olafurpg))
- Indent body of vals when unindentTopLevelOperators=true. [\#1191](https://github.com/scalameta/scalafmt/pull/1191) ([olafurpg](https://github.com/olafurpg))
- Clean up handling of exit codes in the cli [\#1189](https://github.com/scalameta/scalafmt/pull/1189) ([olafurpg](https://github.com/olafurpg))
- Add option spaces.afterSymbolicDefs [\#1188](https://github.com/scalameta/scalafmt/pull/1188) ([slavaschmidt](https://github.com/slavaschmidt))
- Force blocks in try / finally to format on multiple lines [\#1187](https://github.com/scalameta/scalafmt/pull/1187) ([slavaschmidt](https://github.com/slavaschmidt))
- Print out unified diff on --test failure. [\#1186](https://github.com/scalameta/scalafmt/pull/1186) ([olafurpg](https://github.com/olafurpg))
- Pretty-print parse error more nicely. [\#1185](https://github.com/scalameta/scalafmt/pull/1185) ([olafurpg](https://github.com/olafurpg))
- Add support to force upper/lower case literal suffixes [\#1183](https://github.com/scalameta/scalafmt/pull/1183) ([olafurpg](https://github.com/olafurpg))
- Fix indent for nested infix operators [\#1181](https://github.com/scalameta/scalafmt/pull/1181) ([olafurpg](https://github.com/olafurpg))
- Fix 3 bugs [\#1180](https://github.com/scalameta/scalafmt/pull/1180) ([olafurpg](https://github.com/olafurpg))
- Disable inline modifier by default. [\#1179](https://github.com/scalameta/scalafmt/pull/1179) ([olafurpg](https://github.com/olafurpg))
- Include more information on "error due to bug" failure [\#1178](https://github.com/scalameta/scalafmt/pull/1178) ([olafurpg](https://github.com/olafurpg))
- Add more examples for standalone library usage [\#1177](https://github.com/scalameta/scalafmt/pull/1177) ([olafurpg](https://github.com/olafurpg))
- Add repro for \#1033. [\#1175](https://github.com/scalameta/scalafmt/pull/1175) ([olafurpg](https://github.com/olafurpg))
- Add trailingCommas option [\#1174](https://github.com/scalameta/scalafmt/pull/1174) ([gabro](https://github.com/gabro))
- Upgrade to Scalameta v3.7 [\#1170](https://github.com/scalameta/scalafmt/pull/1170) ([olafurpg](https://github.com/olafurpg))
- Allow with-chain wrapping for all params \#1125 [\#1169](https://github.com/scalameta/scalafmt/pull/1169) ([iantabolt](https://github.com/iantabolt))
- updated maven settings [\#1165](https://github.com/scalameta/scalafmt/pull/1165) ([jozic](https://github.com/jozic))
- Add section about scalafmt support in mill [\#1164](https://github.com/scalameta/scalafmt/pull/1164) ([rockjam](https://github.com/rockjam))
- Render the used config values as part of formatting examples [\#1163](https://github.com/scalameta/scalafmt/pull/1163) ([mads-hartmann](https://github.com/mads-hartmann))
- Remove obsolete warning [\#1162](https://github.com/scalameta/scalafmt/pull/1162) ([jbgi](https://github.com/jbgi))
- Update Configuration.scalatex [\#1160](https://github.com/scalameta/scalafmt/pull/1160) ([jozic](https://github.com/jozic))
- Add size to AvoidInfix [\#1159](https://github.com/scalameta/scalafmt/pull/1159) ([joan38](https://github.com/joan38))
- Disable redundantBraces.generalExpressions by default. [\#1157](https://github.com/scalameta/scalafmt/pull/1157) ([olafurpg](https://github.com/olafurpg))
- Fix the full-width example styling [\#1156](https://github.com/scalameta/scalafmt/pull/1156) ([mads-hartmann](https://github.com/mads-hartmann))
- Reorganise the documentation for configuration options [\#1155](https://github.com/scalameta/scalafmt/pull/1155) ([mads-hartmann](https://github.com/mads-hartmann))
- Make it configurable when to exclude dangling parens for vertical [\#1154](https://github.com/scalameta/scalafmt/pull/1154) ([mads-hartmann](https://github.com/mads-hartmann))

## [v1.5.1](https://github.com/scalameta/scalafmt/tree/v1.5.1) (2018-04-26)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v1.5.0...v1.5.1)

**Merged pull requests:**

- SortModifiers rewrite fixes + doc prettyfying [\#1153](https://github.com/scalameta/scalafmt/pull/1153) ([lorandszakacs](https://github.com/lorandszakacs))

## [v1.5.0](https://github.com/scalameta/scalafmt/tree/v1.5.0) (2018-04-23)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v1.4.0...v1.5.0)

**Merged pull requests:**

- Style the page using the colors from the scalameta logo [\#1144](https://github.com/scalameta/scalafmt/pull/1144) ([mads-hartmann](https://github.com/mads-hartmann))
- Add verticalMultilineAtDefinitionSiteArityThreshold setting [\#1143](https://github.com/scalameta/scalafmt/pull/1143) ([mads-hartmann](https://github.com/mads-hartmann))
- Implement SortModifiers rewrite rule [\#1140](https://github.com/scalameta/scalafmt/pull/1140) ([lorandszakacs](https://github.com/lorandszakacs))
- Configuration doc fix [\#1139](https://github.com/scalameta/scalafmt/pull/1139) ([mattkohl](https://github.com/mattkohl))
- Make sbt-scalafmt incremental [\#1136](https://github.com/scalameta/scalafmt/pull/1136) ([vovapolu](https://github.com/vovapolu))
- Example for new sbt plugin [\#1135](https://github.com/scalameta/scalafmt/pull/1135) ([vovapolu](https://github.com/vovapolu))
- Add configuration to disable newlines between multiline defs [\#1134](https://github.com/scalameta/scalafmt/pull/1134) ([iantabolt](https://github.com/iantabolt))
- Generalize with-chain wrapping for defs and type definitions [\#1132](https://github.com/scalameta/scalafmt/pull/1132) ([iantabolt](https://github.com/iantabolt))
- Correct Homebrew installation terminology [\#1121](https://github.com/scalameta/scalafmt/pull/1121) ([srstevenson](https://github.com/srstevenson))
- Fix \#1116, insert space in type projection when selecting symbolic members [\#1117](https://github.com/scalameta/scalafmt/pull/1117) ([olafurpg](https://github.com/olafurpg))
- Fix documentation [\#1114](https://github.com/scalameta/scalafmt/pull/1114) ([jiminhsieh](https://github.com/jiminhsieh))
- Ensure RedundantParens only removes matching parens - fixes \#1111 [\#1112](https://github.com/scalameta/scalafmt/pull/1112) ([bcarl](https://github.com/bcarl))
- Fix openParenDefnSite docs [\#1108](https://github.com/scalameta/scalafmt/pull/1108) ([lorandszakacs](https://github.com/lorandszakacs))
- Adds support for formatting .sc files using sbt options. [\#1107](https://github.com/scalameta/scalafmt/pull/1107) ([lloydmeta](https://github.com/lloydmeta))
- Include ZSH tab-completion file to bin/\_scalafmt [\#1105](https://github.com/scalameta/scalafmt/pull/1105) ([propensive](https://github.com/propensive))
- Support trailing commas in sbt files [\#1103](https://github.com/scalameta/scalafmt/pull/1103) ([dwijnand](https://github.com/dwijnand))
- Add section on align.openParenDefnSite [\#1102](https://github.com/scalameta/scalafmt/pull/1102) ([lorandszakacs](https://github.com/lorandszakacs))
- Add optIn.blankLineBeforeDocstring setting [\#1101](https://github.com/scalameta/scalafmt/pull/1101) ([olafurpg](https://github.com/olafurpg))
- Remove redundant braces from if-else and case expressions [\#1122](https://github.com/scalameta/scalafmt/pull/1122) ([japgolly](https://github.com/japgolly))

## [v1.4.0](https://github.com/scalameta/scalafmt/tree/v1.4.0) (2017-12-18)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v1.3.0...v1.4.0)

**Merged pull requests:**

- Add an "original code" section the issue template [\#1092](https://github.com/scalameta/scalafmt/pull/1092) ([dwijnand](https://github.com/dwijnand))
- Fix version number in website and document pre-release. [\#1089](https://github.com/scalameta/scalafmt/pull/1089) ([olafurpg](https://github.com/olafurpg))
- Add -r bintray:scalameta/maven to Coursier install commands [\#1088](https://github.com/scalameta/scalafmt/pull/1088) ([lloydmeta](https://github.com/lloydmeta))
- Use - instead of + in snapshot versions [\#1082](https://github.com/scalameta/scalafmt/pull/1082) ([olafurpg](https://github.com/olafurpg))
- Point to a correct project on scaladex [\#1071](https://github.com/scalameta/scalafmt/pull/1071) ([alenkacz](https://github.com/alenkacz))
- Bump dependencies versions [\#1070](https://github.com/scalameta/scalafmt/pull/1070) ([loskutov](https://github.com/loskutov))
- Add Mendix to the list of adopters [\#1066](https://github.com/scalameta/scalafmt/pull/1066) ([nerush](https://github.com/nerush))
- Document that RedundantBraces may cause non-idempotent formatting. [\#1057](https://github.com/scalameta/scalafmt/pull/1057) ([olafurpg](https://github.com/olafurpg))
- Empty input with `--diff` and `--stdin` is okay [\#1093](https://github.com/scalameta/scalafmt/pull/1093) ([yln](https://github.com/yln))
- Introduce "default", "akka" and "akka-http" as IndentOperator config [\#1090](https://github.com/scalameta/scalafmt/pull/1090) ([daddykotex](https://github.com/daddykotex))
- Fix file not found errors when running from a subdirectory w/ project.git = true [\#1087](https://github.com/scalameta/scalafmt/pull/1087) ([lloydmeta](https://github.com/lloydmeta))
- New scalafmt sbt plugin [\#1085](https://github.com/scalameta/scalafmt/pull/1085) ([vovapolu](https://github.com/vovapolu))
- No space in by-name params [\#1079](https://github.com/scalameta/scalafmt/pull/1079) ([vovapolu](https://github.com/vovapolu))

## [v1.3.0](https://github.com/scalameta/scalafmt/tree/v1.3.0) (2017-09-24)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v1.2.0...v1.3.0)

**Merged pull requests:**

- add missing wonderbar to the optIn.breakChainOnFirstMethodDot code examples [\#1046](https://github.com/scalameta/scalafmt/pull/1046) ([esarbe](https://github.com/esarbe))
- Disabling alignment helps minimizing git diffs [\#1045](https://github.com/scalameta/scalafmt/pull/1045) ([nightscape](https://github.com/nightscape))
- Add http4s to adopters list [\#1036](https://github.com/scalameta/scalafmt/pull/1036) ([aeons](https://github.com/aeons))
- Repository transfer scmInfo: olafurpg =\> scalameta [\#1034](https://github.com/scalameta/scalafmt/pull/1034) ([MasseGuillaume](https://github.com/MasseGuillaume))
- Fix new GitOpsTest on OSX [\#1030](https://github.com/scalameta/scalafmt/pull/1030) ([olafurpg](https://github.com/olafurpg))
- Use the encoding option when writing files, too. [\#1053](https://github.com/scalameta/scalafmt/pull/1053) ([alexdupre](https://github.com/alexdupre))
- AsciiSortImports is now incompatible with ExpandImportSelectors. Fixes \#1024 [\#1048](https://github.com/scalameta/scalafmt/pull/1048) ([Lasering](https://github.com/Lasering))
- Add optIn.breaksInsideChains = true [\#1037](https://github.com/scalameta/scalafmt/pull/1037) ([olafurpg](https://github.com/olafurpg))
- Git testing framework and fix for some git-file-fetching issues [\#1028](https://github.com/scalameta/scalafmt/pull/1028) ([pjrt](https://github.com/pjrt))
- Return non-zero exit code in ./scalafmt if no files match filter [\#1025](https://github.com/scalameta/scalafmt/pull/1025) ([yln](https://github.com/yln))

## [v1.2.0](https://github.com/scalameta/scalafmt/tree/v1.2.0) (2017-08-11)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v1.1.0...v1.2.0)

**Merged pull requests:**

- Tweak Coursier installation instructions [\#1011](https://github.com/scalameta/scalafmt/pull/1011) ([dwijnand](https://github.com/dwijnand))
- \[typo\] bin.packParentConstructors =\> binPack.parentConstructors [\#1009](https://github.com/scalameta/scalafmt/pull/1009) ([ronanM](https://github.com/ronanM))
- Enable mima binary checks in CI. [\#1017](https://github.com/scalameta/scalafmt/pull/1017) ([olafurpg](https://github.com/olafurpg))
- Hacky hacks to fix off-by-two errors [\#1016](https://github.com/scalameta/scalafmt/pull/1016) ([olafurpg](https://github.com/olafurpg))
- Update to sbt 1.0.0 [\#1015](https://github.com/scalameta/scalafmt/pull/1015) ([olafurpg](https://github.com/olafurpg))
- Fail fast on invalid .scalafmt.conf when running with no args. [\#1014](https://github.com/scalameta/scalafmt/pull/1014) ([olafurpg](https://github.com/olafurpg))
- Use the VCS root as a potential config path in the IntelliJ plugin [\#1007](https://github.com/scalameta/scalafmt/pull/1007) ([stuhood](https://github.com/stuhood))
- Implement option for discarding whitespace between a keyword and left-paren [\#998](https://github.com/scalameta/scalafmt/pull/998) ([nrinaudo](https://github.com/nrinaudo))
- Add support for inserting a line break between a curly if and an else. [\#996](https://github.com/scalameta/scalafmt/pull/996) ([nrinaudo](https://github.com/nrinaudo))

## [v1.1.0](https://github.com/scalameta/scalafmt/tree/v1.1.0) (2017-07-09)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v1.0.0-RC4...v1.1.0)

**Merged pull requests:**

- Update Adopters.scala [\#992](https://github.com/scalameta/scalafmt/pull/992) ([cwbeck](https://github.com/cwbeck))
- fix for jvmopts and enabling no-stderr by default if possible [\#987](https://github.com/scalameta/scalafmt/pull/987) ([muxanick](https://github.com/muxanick))
- Re-enable appveyor [\#986](https://github.com/scalameta/scalafmt/pull/986) ([olafurpg](https://github.com/olafurpg))
- Adding of --no-stderr option [\#985](https://github.com/scalameta/scalafmt/pull/985) ([muxanick](https://github.com/muxanick))
- Add optIn.selfAnnotationNewline, fixes \#938 [\#983](https://github.com/scalameta/scalafmt/pull/983) ([olafurpg](https://github.com/olafurpg))
- Fix off-by-one error, \#976. [\#982](https://github.com/scalameta/scalafmt/pull/982) ([olafurpg](https://github.com/olafurpg))
- Document removal of bestEffortInDeeplyNestedCode. [\#980](https://github.com/scalameta/scalafmt/pull/980) ([olafurpg](https://github.com/olafurpg))
- Use lenient dialect by default [\#979](https://github.com/scalameta/scalafmt/pull/979) ([olafurpg](https://github.com/olafurpg))

## [v1.0.0-RC4](https://github.com/scalameta/scalafmt/tree/v1.0.0-RC4) (2017-06-17)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v1.0.0...v1.0.0-RC4)

## [v1.0.0](https://github.com/scalameta/scalafmt/tree/v1.0.0) (2017-06-17)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v1.0.0-RC3...v1.0.0)

## [v1.0.0-RC3](https://github.com/scalameta/scalafmt/tree/v1.0.0-RC3) (2017-06-14)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v1.0.0-RC2...v1.0.0-RC3)

**Merged pull requests:**

- Fix \#973. [\#974](https://github.com/scalameta/scalafmt/pull/974) ([olafurpg](https://github.com/olafurpg))
- Harden AvoidInfix. [\#971](https://github.com/scalameta/scalafmt/pull/971) ([olafurpg](https://github.com/olafurpg))

## [v1.0.0-RC2](https://github.com/scalameta/scalafmt/tree/v1.0.0-RC2) (2017-06-08)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v1.0.0-RC1...v1.0.0-RC2)

**Merged pull requests:**

- Re-enable auto-publish on merge into master. [\#968](https://github.com/scalameta/scalafmt/pull/968) ([olafurpg](https://github.com/olafurpg))
- Enable alignment on case arrows by default [\#964](https://github.com/scalameta/scalafmt/pull/964) ([olafurpg](https://github.com/olafurpg))
- Remove outdated contributing tutorial. [\#963](https://github.com/scalameta/scalafmt/pull/963) ([olafurpg](https://github.com/olafurpg))
- Fix \#960 and \#927: Oddness with git and non-root [\#961](https://github.com/scalameta/scalafmt/pull/961) ([pjrt](https://github.com/pjrt))
- Topic/\#933 [\#959](https://github.com/scalameta/scalafmt/pull/959) ([Daxten](https://github.com/Daxten))
- Break line if def equals is at columns + 1 [\#957](https://github.com/scalameta/scalafmt/pull/957) ([aeons](https://github.com/aeons))
- \#916 Add option to space context bounds only when multiple. ReaderUti… [\#956](https://github.com/scalameta/scalafmt/pull/956) ([hejfelix](https://github.com/hejfelix))
- Replace `./scalafmt` with `scalafmt` [\#954](https://github.com/scalameta/scalafmt/pull/954) ([amirkarimi](https://github.com/amirkarimi))
- Enable optIn.annotationNewlines = true by default, fixes \#942 [\#953](https://github.com/scalameta/scalafmt/pull/953) ([olafurpg](https://github.com/olafurpg))
- Bump build dependencies [\#952](https://github.com/scalameta/scalafmt/pull/952) ([olafurpg](https://github.com/olafurpg))
- Exclude scalapb from runtime, fixes \#944 [\#951](https://github.com/scalameta/scalafmt/pull/951) ([olafurpg](https://github.com/olafurpg))
- Enable Travis CI [\#947](https://github.com/scalameta/scalafmt/pull/947) ([olafurpg](https://github.com/olafurpg))

## [v1.0.0-RC1](https://github.com/scalameta/scalafmt/tree/v1.0.0-RC1) (2017-05-26)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.7.0-RC1...v1.0.0-RC1)

**Merged pull requests:**

- Upgrade to non-broken metaconfig release. [\#946](https://github.com/scalameta/scalafmt/pull/946) ([olafurpg](https://github.com/olafurpg))
- documentation fix [\#945](https://github.com/scalameta/scalafmt/pull/945) ([nadavwr](https://github.com/nadavwr))
- Remake the CLI interface [\#939](https://github.com/scalameta/scalafmt/pull/939) ([pjrt](https://github.com/pjrt))
- Invite Paul Draper to the team and document neo-sbt-scalafmt! [\#926](https://github.com/scalameta/scalafmt/pull/926) ([olafurpg](https://github.com/olafurpg))
- Cleanup before v1.0, fixes \#918 [\#921](https://github.com/scalameta/scalafmt/pull/921) ([olafurpg](https://github.com/olafurpg))
- Fix \#911 [\#920](https://github.com/scalameta/scalafmt/pull/920) ([olafurpg](https://github.com/olafurpg))
- Remove enclosing parens in avoidinfix , closes \#851 [\#919](https://github.com/scalameta/scalafmt/pull/919) ([otolabqu](https://github.com/otolabqu))
- Add support for auto-binpacking of argument lists with idents only [\#910](https://github.com/scalameta/scalafmt/pull/910) ([olafurpg](https://github.com/olafurpg))
- Fix coordinates for scalafmt-bootstrap [\#908](https://github.com/scalameta/scalafmt/pull/908) ([nightscape](https://github.com/nightscape))
- Update docs. [\#906](https://github.com/scalameta/scalafmt/pull/906) ([olafurpg](https://github.com/olafurpg))
- Remove homebrew. [\#905](https://github.com/scalameta/scalafmt/pull/905) ([olafurpg](https://github.com/olafurpg))

## [v0.7.0-RC1](https://github.com/scalameta/scalafmt/tree/v0.7.0-RC1) (2017-04-23)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.6.8...v0.7.0-RC1)

**Merged pull requests:**

- Multiline top level statement, addresses \#893 [\#904](https://github.com/scalameta/scalafmt/pull/904) ([olafurpg](https://github.com/olafurpg))
- Fix \#735. newlines before and after implicit keyword [\#903](https://github.com/scalameta/scalafmt/pull/903) ([sugakandrey](https://github.com/sugakandrey))
- Make build green again on appveyor and IntelliJ [\#902](https://github.com/scalameta/scalafmt/pull/902) ([olafurpg](https://github.com/olafurpg))
- Fix AvoidInfix for methods with empty parameters list [\#901](https://github.com/scalameta/scalafmt/pull/901) ([sugakandrey](https://github.com/sugakandrey))
- Port core to Scala.js [\#898](https://github.com/scalameta/scalafmt/pull/898) ([olafurpg](https://github.com/olafurpg))
- Migrate to sbt 1.0.0-M5 for sbt-scalafmt. [\#896](https://github.com/scalameta/scalafmt/pull/896) ([olafurpg](https://github.com/olafurpg))
- Fix \#883. [\#887](https://github.com/scalameta/scalafmt/pull/887) ([olafurpg](https://github.com/olafurpg))
- Upgrade dependencies [\#885](https://github.com/scalameta/scalafmt/pull/885) ([olafurpg](https://github.com/olafurpg))
- Fix \#880, filter diff files with canFormat [\#881](https://github.com/scalameta/scalafmt/pull/881) ([caoilte](https://github.com/caoilte))

## [v0.6.8](https://github.com/scalameta/scalafmt/tree/v0.6.8) (2017-04-06)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.6.7...v0.6.8)

**Merged pull requests:**

- Auto publish docs on merge to master, fixes \#764 [\#877](https://github.com/scalameta/scalafmt/pull/877) ([olafurpg](https://github.com/olafurpg))

## [v0.6.7](https://github.com/scalameta/scalafmt/tree/v0.6.7) (2017-04-05)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.6.6...v0.6.7)

**Merged pull requests:**

- Publish to bintray [\#870](https://github.com/scalameta/scalafmt/pull/870) ([olafurpg](https://github.com/olafurpg))
- Clean up repo. [\#869](https://github.com/scalameta/scalafmt/pull/869) ([olafurpg](https://github.com/olafurpg))
- Document random stuff in CONTRIBUTING.md [\#868](https://github.com/scalameta/scalafmt/pull/868) ([olafurpg](https://github.com/olafurpg))
- Implement \#863: vertical-multiline for type params [\#864](https://github.com/scalameta/scalafmt/pull/864) ([pjrt](https://github.com/pjrt))
- Fix \#799 Allow ASCII sorting of imports [\#860](https://github.com/scalameta/scalafmt/pull/860) ([pjrt](https://github.com/pjrt))
- Fix \#808: newlines after curly lambda [\#859](https://github.com/scalameta/scalafmt/pull/859) ([pjrt](https://github.com/pjrt))
- Fix appveyor again [\#858](https://github.com/scalameta/scalafmt/pull/858) ([olafurpg](https://github.com/olafurpg))
- Remove unused statement [\#857](https://github.com/scalameta/scalafmt/pull/857) ([ocadaruma](https://github.com/ocadaruma))
- Updated latest_version in scalafmt_auto [\#856](https://github.com/scalameta/scalafmt/pull/856) ([jupblb](https://github.com/jupblb))
- Fix appveyor [\#855](https://github.com/scalameta/scalafmt/pull/855) ([olafurpg](https://github.com/olafurpg))
- Fix \#736: Make verticalMultiline apply to classes too [\#854](https://github.com/scalameta/scalafmt/pull/854) ([pjrt](https://github.com/pjrt))
- fix readme.md [\#852](https://github.com/scalameta/scalafmt/pull/852) ([xuwei-k](https://github.com/xuwei-k))

## [v0.6.6](https://github.com/scalameta/scalafmt/tree/v0.6.6) (2017-03-20)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.6.5...v0.6.6)

**Merged pull requests:**

- Switch to coursier 1.0.0-M15-5 [\#846](https://github.com/scalameta/scalafmt/pull/846) ([alexarchambault](https://github.com/alexarchambault))

## [v0.6.5](https://github.com/scalameta/scalafmt/tree/v0.6.5) (2017-03-18)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.6.4...v0.6.5)

**Merged pull requests:**

- Bootstrap should resolve with lock retry [\#838](https://github.com/scalameta/scalafmt/pull/838) ([stephennancekivell](https://github.com/stephennancekivell))

## [v0.6.4](https://github.com/scalameta/scalafmt/tree/v0.6.4) (2017-03-14)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.6.3...v0.6.4)

**Merged pull requests:**

- Use custom cache dir, second attempt at \#807 [\#834](https://github.com/scalameta/scalafmt/pull/834) ([olafurpg](https://github.com/olafurpg))

## [v0.6.3](https://github.com/scalameta/scalafmt/tree/v0.6.3) (2017-03-13)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.6.2...v0.6.3)

**Merged pull requests:**

- Update appveyor [\#826](https://github.com/scalameta/scalafmt/pull/826) ([olafurpg](https://github.com/olafurpg))
- Fix spaces.inParens for brackets [\#825](https://github.com/scalameta/scalafmt/pull/825) ([olafurpg](https://github.com/olafurpg))
- Update badges [\#824](https://github.com/scalameta/scalafmt/pull/824) ([olafurpg](https://github.com/olafurpg))
- Clean up home dir and drone build steps. [\#823](https://github.com/scalameta/scalafmt/pull/823) ([olafurpg](https://github.com/olafurpg))
- Upgrade to drone v0.5 [\#821](https://github.com/scalameta/scalafmt/pull/821) ([olafurpg](https://github.com/olafurpg))
- fix mainClass not found error on windows machine \(that comment makes … [\#819](https://github.com/scalameta/scalafmt/pull/819) ([Daxten](https://github.com/Daxten))
- Fix issue \#816: Makes verticalMultilineDefnSite respect continuationIndent.defnSite [\#829](https://github.com/scalameta/scalafmt/pull/829) ([pjrt](https://github.com/pjrt))
- Topic/newlines_annotations \#806 [\#820](https://github.com/scalameta/scalafmt/pull/820) ([Daxten](https://github.com/Daxten))
- Fix \#780, add spaces in parens flag [\#818](https://github.com/scalameta/scalafmt/pull/818) ([olafurpg](https://github.com/olafurpg))
- Don't resolve from ivy2local in ScalafmtBotstrap. [\#817](https://github.com/scalameta/scalafmt/pull/817) ([olafurpg](https://github.com/olafurpg))
- Fix newlines.alwaysBeforeTopLevelStatements doc [\#804](https://github.com/scalameta/scalafmt/pull/804) ([dwijnand](https://github.com/dwijnand))
- Fix \#798. [\#803](https://github.com/scalameta/scalafmt/pull/803) ([olafurpg](https://github.com/olafurpg))
- Fix \#800. [\#801](https://github.com/scalameta/scalafmt/pull/801) ([olafurpg](https://github.com/olafurpg))
- \[WIP\] fix for \#774; no newline after comments [\#797](https://github.com/scalameta/scalafmt/pull/797) ([Daxten](https://github.com/Daxten))

## [v0.6.2](https://github.com/scalameta/scalafmt/tree/v0.6.2) (2017-03-07)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.6.1...v0.6.2)

## [v0.6.1](https://github.com/scalameta/scalafmt/tree/v0.6.1) (2017-03-06)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.6.0...v0.6.1)

## [v0.6.0](https://github.com/scalameta/scalafmt/tree/v0.6.0) (2017-03-06)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.5.8...v0.6.0)

**Merged pull requests:**

- s/precommit/pre-commit [\#795](https://github.com/scalameta/scalafmt/pull/795) ([xuwei-k](https://github.com/xuwei-k))
- Cross-build to 2.12 [\#794](https://github.com/scalameta/scalafmt/pull/794) ([olafurpg](https://github.com/olafurpg))
- Fix \#535 [\#793](https://github.com/scalameta/scalafmt/pull/793) ([olafurpg](https://github.com/olafurpg))
- Validate rewrite rule combination, fixes \#775 [\#792](https://github.com/scalameta/scalafmt/pull/792) ([olafurpg](https://github.com/olafurpg))
- Clean up docstrings and ScalafmtConfig, fixes \#586 [\#791](https://github.com/scalameta/scalafmt/pull/791) ([olafurpg](https://github.com/olafurpg))
- Fix \#728, infix indent for :: [\#790](https://github.com/scalameta/scalafmt/pull/790) ([olafurpg](https://github.com/olafurpg))
- Fix \#699, exclude excluded files in format-on-save in IntelliJ [\#789](https://github.com/scalameta/scalafmt/pull/789) ([olafurpg](https://github.com/olafurpg))
- Fix \#694, support encoding configuration. UTF-8 is default. [\#788](https://github.com/scalameta/scalafmt/pull/788) ([olafurpg](https://github.com/olafurpg))
- Fix \#741, unified spark colon for def/val [\#787](https://github.com/scalameta/scalafmt/pull/787) ([olafurpg](https://github.com/olafurpg))
- Fix \#590, indent by 2 spaces for = // [\#786](https://github.com/scalameta/scalafmt/pull/786) ([olafurpg](https://github.com/olafurpg))
- Fix \#747, respect align settings even for tuples [\#785](https://github.com/scalameta/scalafmt/pull/785) ([olafurpg](https://github.com/olafurpg))
- Fix \#748 [\#784](https://github.com/scalameta/scalafmt/pull/784) ([olafurpg](https://github.com/olafurpg))
- Fix \#753, add support for custom error message for --test via .scalafmt.conf [\#783](https://github.com/scalameta/scalafmt/pull/783) ([olafurpg](https://github.com/olafurpg))
- Fix \#745, handle config style + forced bin packing [\#782](https://github.com/scalameta/scalafmt/pull/782) ([olafurpg](https://github.com/olafurpg))
- Guard against tests that have ONLY in CI [\#781](https://github.com/scalameta/scalafmt/pull/781) ([olafurpg](https://github.com/olafurpg))
- Fix \#756 [\#779](https://github.com/scalameta/scalafmt/pull/779) ([olafurpg](https://github.com/olafurpg))
- Improve performance of State.next by reusing syntax instance [\#776](https://github.com/scalameta/scalafmt/pull/776) ([rorygraves](https://github.com/rorygraves))
- Handle comment breaking wrapping [\#773](https://github.com/scalameta/scalafmt/pull/773) ([dwijnand](https://github.com/dwijnand))
- Simplify build so `sbt test` "just works" [\#760](https://github.com/scalameta/scalafmt/pull/760) ([olafurpg](https://github.com/olafurpg))
- Polish infix indentation around assignment. [\#740](https://github.com/scalameta/scalafmt/pull/740) ([olafurpg](https://github.com/olafurpg))

## [v0.5.8](https://github.com/scalameta/scalafmt/tree/v0.5.8) (2017-03-03)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.5.7...v0.5.8)

**Merged pull requests:**

- \#763 Fix Varargs expansion shouldn't be split [\#772](https://github.com/scalameta/scalafmt/pull/772) ([tgodzik](https://github.com/tgodzik))
- Make sure to always resolve HOCON config [\#771](https://github.com/scalameta/scalafmt/pull/771) ([dwijnand](https://github.com/dwijnand))
- \#759 filter out deleted files on --diff [\#770](https://github.com/scalameta/scalafmt/pull/770) ([tgodzik](https://github.com/tgodzik))
- Fix vertical alignment on last block of file [\#769](https://github.com/scalameta/scalafmt/pull/769) ([dwijnand](https://github.com/dwijnand))
- Generalise and structure the issue template [\#766](https://github.com/scalameta/scalafmt/pull/766) ([dwijnand](https://github.com/dwijnand))
- Clarify config values & examples some [\#761](https://github.com/scalameta/scalafmt/pull/761) ([dwijnand](https://github.com/dwijnand))
- fix isTopLevelStatement for edge-cases [\#750](https://github.com/scalameta/scalafmt/pull/750) ([Daxten](https://github.com/Daxten))
- \#739: AvoidInfix Bug [\#749](https://github.com/scalameta/scalafmt/pull/749) ([ysusuk](https://github.com/ysusuk))
- Upgrade to meta 1.6 [\#746](https://github.com/scalameta/scalafmt/pull/746) ([olafurpg](https://github.com/olafurpg))
- Remove 'HEAD' from git diff, to include all uncommitted changes aswell [\#742](https://github.com/scalameta/scalafmt/pull/742) ([bjornj12](https://github.com/bjornj12))
- Fix \#731: empty-param group with verical style [\#734](https://github.com/scalameta/scalafmt/pull/734) ([pjrt](https://github.com/pjrt))
- Add flag for newline before toplevel statements [\#733](https://github.com/scalameta/scalafmt/pull/733) ([Daxten](https://github.com/Daxten))
- \#726: unimport is not respected [\#729](https://github.com/scalameta/scalafmt/pull/729) ([ysusuk](https://github.com/ysusuk))

## [v0.5.7](https://github.com/scalameta/scalafmt/tree/v0.5.7) (2017-02-24)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.5.6...v0.5.7)

**Merged pull requests:**

- skip git hooks during pages update [\#727](https://github.com/scalameta/scalafmt/pull/727) ([ysusuk](https://github.com/ysusuk))
- \#724: Update rewrite rules docs [\#725](https://github.com/scalameta/scalafmt/pull/725) ([ysusuk](https://github.com/ysusuk))

## [v0.5.6](https://github.com/scalameta/scalafmt/tree/v0.5.6) (2017-02-08)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.5.5...v0.5.6)

**Merged pull requests:**

- \#709: Feature request: AvoidInfix rewrite [\#715](https://github.com/scalameta/scalafmt/pull/715) ([ysusuk](https://github.com/ysusuk))
- Fix \#710: Extra space added for unary operations [\#714](https://github.com/scalameta/scalafmt/pull/714) ([pjrt](https://github.com/pjrt))
- Add Iurii Susuk to the team! [\#713](https://github.com/scalameta/scalafmt/pull/713) ([olafurpg](https://github.com/olafurpg))
- \#706: Migrate to scalafix patches [\#712](https://github.com/scalameta/scalafmt/pull/712) ([ysusuk](https://github.com/ysusuk))
- Another fix for `verticalMultilineAtDefinitionSite` [\#711](https://github.com/scalameta/scalafmt/pull/711) ([pjrt](https://github.com/pjrt))
- Fix \#672, edge case with inline comment [\#708](https://github.com/scalameta/scalafmt/pull/708) ([olafurpg](https://github.com/olafurpg))

## [v0.5.5](https://github.com/scalameta/scalafmt/tree/v0.5.5) (2017-02-01)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.5.4...v0.5.5)

**Merged pull requests:**

- \#690: Support check for "redundant enclosing brace" in string interpo... [\#703](https://github.com/scalameta/scalafmt/pull/703) ([ysusuk](https://github.com/ysusuk))
- Add new "Team" section in readme [\#702](https://github.com/scalameta/scalafmt/pull/702) ([olafurpg](https://github.com/olafurpg))
- Fix issue on verticalMultilineAtDefinitionSite [\#700](https://github.com/scalameta/scalafmt/pull/700) ([pjrt](https://github.com/pjrt))

## [v0.5.4](https://github.com/scalameta/scalafmt/tree/v0.5.4) (2017-01-26)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.5.3...v0.5.4)

**Merged pull requests:**

- Fix \#697, unary operator to symbolic ident [\#698](https://github.com/scalameta/scalafmt/pull/698) ([olafurpg](https://github.com/olafurpg))
- Adds a new style for function defs [\#682](https://github.com/scalameta/scalafmt/pull/682) ([pjrt](https://github.com/pjrt))

## [v0.5.3](https://github.com/scalameta/scalafmt/tree/v0.5.3) (2017-01-24)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.5.3-RC1...v0.5.3)

## [v0.5.3-RC1](https://github.com/scalameta/scalafmt/tree/v0.5.3-RC1) (2017-01-23)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.5.2...v0.5.3-RC1)

**Merged pull requests:**

- Initialize term display logger. [\#692](https://github.com/scalameta/scalafmt/pull/692) ([olafurpg](https://github.com/olafurpg))
- Fix "pr" helper method in docs gen [\#689](https://github.com/scalameta/scalafmt/pull/689) ([dwijnand](https://github.com/dwijnand))

## [v0.5.2](https://github.com/scalameta/scalafmt/tree/v0.5.2) (2017-01-22)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.5.2-RC1...v0.5.2)

**Merged pull requests:**

- Fix \#677 [\#688](https://github.com/scalameta/scalafmt/pull/688) ([olafurpg](https://github.com/olafurpg))
- Fix \#671 [\#687](https://github.com/scalameta/scalafmt/pull/687) ([olafurpg](https://github.com/olafurpg))
- 679 [\#686](https://github.com/scalameta/scalafmt/pull/686) ([olafurpg](https://github.com/olafurpg))
- Fix \#661 [\#685](https://github.com/scalameta/scalafmt/pull/685) ([olafurpg](https://github.com/olafurpg))
- Resolve config [\#681](https://github.com/scalameta/scalafmt/pull/681) ([avdv](https://github.com/avdv))
- New ContinuationIndent: extendSite [\#678](https://github.com/scalameta/scalafmt/pull/678) ([pjrt](https://github.com/pjrt))
- fix Patch insideRange [\#675](https://github.com/scalameta/scalafmt/pull/675) ([MasseGuillaume](https://github.com/MasseGuillaume))
- \#657: New rewrite: expand import selectors [\#674](https://github.com/scalameta/scalafmt/pull/674) ([ysusuk](https://github.com/ysusuk))
- \#660: Feature request: configurable spaces around infix types [\#669](https://github.com/scalameta/scalafmt/pull/669) ([ysusuk](https://github.com/ysusuk))
- Update ln numbers and output in Tutorial [\#666](https://github.com/scalameta/scalafmt/pull/666) ([ysusuk](https://github.com/ysusuk))
- Remove extra newline from stdout [\#665](https://github.com/scalameta/scalafmt/pull/665) ([dguo-coursera](https://github.com/dguo-coursera))
- Add --fallback-mode flag to cli [\#659](https://github.com/scalameta/scalafmt/pull/659) ([olafurpg](https://github.com/olafurpg))

## [v0.5.2-RC1](https://github.com/scalameta/scalafmt/tree/v0.5.2-RC1) (2016-12-28)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.5.1...v0.5.2-RC1)

**Merged pull requests:**

- Ditch synthetic project in favor of coursier. [\#658](https://github.com/scalameta/scalafmt/pull/658) ([olafurpg](https://github.com/olafurpg))
- Fixes \#399 - Automate IntelliJ Plugin Update [\#655](https://github.com/scalameta/scalafmt/pull/655) ([rbellamy](https://github.com/rbellamy))
- Fix \#607. [\#651](https://github.com/scalameta/scalafmt/pull/651) ([olafurpg](https://github.com/olafurpg))
- Make nested path exclusions working on Windows [\#648](https://github.com/scalameta/scalafmt/pull/648) ([mpociecha](https://github.com/mpociecha))
- Fix links to Tutorial.md [\#647](https://github.com/scalameta/scalafmt/pull/647) ([mpociecha](https://github.com/mpociecha))
- Setup AppVeyor [\#645](https://github.com/scalameta/scalafmt/pull/645) ([olafurpg](https://github.com/olafurpg))
- Christmas cleaning [\#643](https://github.com/scalameta/scalafmt/pull/643) ([olafurpg](https://github.com/olafurpg))
- Fix path separators and line endings to make most of tests passing on Windows [\#642](https://github.com/scalameta/scalafmt/pull/642) ([mpociecha](https://github.com/mpociecha))

## [v0.5.1](https://github.com/scalameta/scalafmt/tree/v0.5.1) (2016-12-23)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.5.0...v0.5.1)

**Merged pull requests:**

- Fix \#639 [\#640](https://github.com/scalameta/scalafmt/pull/640) ([olafurpg](https://github.com/olafurpg))
- Fix link URL in the sbt docs [\#637](https://github.com/scalameta/scalafmt/pull/637) ([cb372](https://github.com/cb372))

## [v0.5.0](https://github.com/scalameta/scalafmt/tree/v0.5.0) (2016-12-23)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.5.0-RC4...v0.5.0)

## [v0.5.0-RC4](https://github.com/scalameta/scalafmt/tree/v0.5.0-RC4) (2016-12-21)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.5.0-RC3...v0.5.0-RC4)

**Merged pull requests:**

- Fix idempotency regression from \#629 [\#631](https://github.com/scalameta/scalafmt/pull/631) ([olafurpg](https://github.com/olafurpg))
- Improve builds in IntelliJ and on Windows [\#630](https://github.com/scalameta/scalafmt/pull/630) ([mpociecha](https://github.com/mpociecha))

## [v0.5.0-RC3](https://github.com/scalameta/scalafmt/tree/v0.5.0-RC3) (2016-12-20)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.5.0-RC2...v0.5.0-RC3)

**Merged pull requests:**

- Automatically bin pack for argument lists of literals. [\#629](https://github.com/scalameta/scalafmt/pull/629) ([olafurpg](https://github.com/olafurpg))
- add maven version badge to readme [\#628](https://github.com/scalameta/scalafmt/pull/628) ([emanresusername](https://github.com/emanresusername))
- Fewer search state exploded. [\#626](https://github.com/scalameta/scalafmt/pull/626) ([olafurpg](https://github.com/olafurpg))
- Improve search state for reactjs style DSLs. Fixes \#458 [\#622](https://github.com/scalameta/scalafmt/pull/622) ([olafurpg](https://github.com/olafurpg))
- Add runner.optimizer.forceConfig... flag. [\#621](https://github.com/scalameta/scalafmt/pull/621) ([olafurpg](https://github.com/olafurpg))
- Add config-str flag, fixes \#617 [\#620](https://github.com/scalameta/scalafmt/pull/620) ([olafurpg](https://github.com/olafurpg))
- fixing regex for latest version [\#619](https://github.com/scalameta/scalafmt/pull/619) ([sudev](https://github.com/sudev))

## [v0.5.0-RC2](https://github.com/scalameta/scalafmt/tree/v0.5.0-RC2) (2016-12-18)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.5.0-RC1...v0.5.0-RC2)

**Merged pull requests:**

- Upgrade to scala.meta 1.3. [\#615](https://github.com/scalameta/scalafmt/pull/615) ([olafurpg](https://github.com/olafurpg))

## [v0.5.0-RC1](https://github.com/scalameta/scalafmt/tree/v0.5.0-RC1) (2016-12-18)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.4.10...v0.5.0-RC1)

**Merged pull requests:**

- Prepare for 0.5 release. [\#613](https://github.com/scalameta/scalafmt/pull/613) ([olafurpg](https://github.com/olafurpg))
- Support intellij style + config style arg, fixes \#593. [\#612](https://github.com/scalameta/scalafmt/pull/612) ([olafurpg](https://github.com/olafurpg))
- Bump penalty for select chains of length 1. [\#611](https://github.com/scalameta/scalafmt/pull/611) ([olafurpg](https://github.com/olafurpg))
- Revive the sbt plugin [\#610](https://github.com/scalameta/scalafmt/pull/610) ([olafurpg](https://github.com/olafurpg))
- Fix Linux installation command [\#602](https://github.com/scalameta/scalafmt/pull/602) ([felixmulder](https://github.com/felixmulder))
- Remove SBT plugin, see \#597. [\#598](https://github.com/scalameta/scalafmt/pull/598) ([olafurpg](https://github.com/olafurpg))
- Add newlines.neverInDanglingParenthesesSingleLineArgList, fixes \#593 [\#595](https://github.com/scalameta/scalafmt/pull/595) ([olafurpg](https://github.com/olafurpg))
- Add indentYieldKeyword setting, fixes \#592. [\#594](https://github.com/scalameta/scalafmt/pull/594) ([olafurpg](https://github.com/olafurpg))

## [v0.4.10](https://github.com/scalameta/scalafmt/tree/v0.4.10) (2016-11-10)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.4.9...v0.4.10)

**Merged pull requests:**

- Fix \#583. Avoid try to format dirs [\#584](https://github.com/scalameta/scalafmt/pull/584) ([andreaTP](https://github.com/andreaTP))
- Remove redundant braces in enumerator guard conditions [\#578](https://github.com/scalameta/scalafmt/pull/578) ([ShaneDelmore](https://github.com/ShaneDelmore))

## [v0.4.9](https://github.com/scalameta/scalafmt/tree/v0.4.9) (2016-11-04)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.4.8...v0.4.9)

**Merged pull requests:**

- Create bootstrap script, towards \#571. [\#577](https://github.com/scalameta/scalafmt/pull/577) ([olafurpg](https://github.com/olafurpg))
- /usr/bin/env [\#575](https://github.com/scalameta/scalafmt/pull/575) ([MasseGuillaume](https://github.com/MasseGuillaume))
- More configurable metaconfig [\#570](https://github.com/scalameta/scalafmt/pull/570) ([olafurpg](https://github.com/olafurpg))
- Issue 457 long imports [\#567](https://github.com/scalameta/scalafmt/pull/567) ([mtomko](https://github.com/mtomko))
- Add info about Arch Linux package [\#565](https://github.com/scalameta/scalafmt/pull/565) ([RatanRSur](https://github.com/RatanRSur))
- Update IDEA plugin [\#562](https://github.com/scalameta/scalafmt/pull/562) ([olafurpg](https://github.com/olafurpg))
- Fix scalafmt_auto script. [\#559](https://github.com/scalameta/scalafmt/pull/559) ([olafurpg](https://github.com/olafurpg))

## [v0.4.8](https://github.com/scalameta/scalafmt/tree/v0.4.8) (2016-10-25)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.4.7...v0.4.8)

**Merged pull requests:**

- Add flag: newlines.neverInResultType. Fixes \#496 [\#555](https://github.com/scalameta/scalafmt/pull/555) ([olafurpg](https://github.com/olafurpg))
- Replace java.io.File with custom AbsoluteFile. [\#554](https://github.com/scalameta/scalafmt/pull/554) ([olafurpg](https://github.com/olafurpg))
- Handle Pat.Interpolate, fixes \#512 [\#550](https://github.com/scalameta/scalafmt/pull/550) ([olafurpg](https://github.com/olafurpg))
- Infix applications to count towards valign depth, fixes \#531 [\#549](https://github.com/scalameta/scalafmt/pull/549) ([olafurpg](https://github.com/olafurpg))
- Use constant instead of strings and remove useless statement in SBT [\#548](https://github.com/scalameta/scalafmt/pull/548) ([joan38](https://github.com/joan38))
- Move Settings at project level [\#544](https://github.com/scalameta/scalafmt/pull/544) ([joan38](https://github.com/joan38))
- Fix broken test. [\#539](https://github.com/scalameta/scalafmt/pull/539) ([olafurpg](https://github.com/olafurpg))
- Format on save and update the notification system [\#534](https://github.com/scalameta/scalafmt/pull/534) ([joan38](https://github.com/joan38))
- WIP Break select chain on curly braces [\#532](https://github.com/scalameta/scalafmt/pull/532) ([olafurpg](https://github.com/olafurpg))
- Allow --config to be absolute path [\#528](https://github.com/scalameta/scalafmt/pull/528) ([olafurpg](https://github.com/olafurpg))

## [v0.4.7](https://github.com/scalameta/scalafmt/tree/v0.4.7) (2016-10-17)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.4.6...v0.4.7)

**Merged pull requests:**

- Skip RedundantBraces rewrite on term.function [\#526](https://github.com/scalameta/scalafmt/pull/526) ([olafurpg](https://github.com/olafurpg))
- Only align tokens with same distance to root node. [\#525](https://github.com/scalameta/scalafmt/pull/525) ([olafurpg](https://github.com/olafurpg))
- Expose --assume-filename flag in CLI [\#524](https://github.com/scalameta/scalafmt/pull/524) ([olafurpg](https://github.com/olafurpg))
- Swallow stderr when running gitops [\#523](https://github.com/scalameta/scalafmt/pull/523) ([olafurpg](https://github.com/olafurpg))

## [v0.4.6](https://github.com/scalameta/scalafmt/tree/v0.4.6) (2016-10-11)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.4.5...v0.4.6)

**Merged pull requests:**

- Add nailgun interrop to CLI. [\#514](https://github.com/scalameta/scalafmt/pull/514) ([olafurpg](https://github.com/olafurpg))

## [v0.4.5](https://github.com/scalameta/scalafmt/tree/v0.4.5) (2016-10-10)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.4.4...v0.4.5)

**Merged pull requests:**

- Fix --config flag [\#508](https://github.com/scalameta/scalafmt/pull/508) ([olafurpg](https://github.com/olafurpg))
- Add indent for Term.Assign, fixes \#506 [\#507](https://github.com/scalameta/scalafmt/pull/507) ([olafurpg](https://github.com/olafurpg))
- Add syntax align.tokens.add = \[foobar\] [\#499](https://github.com/scalameta/scalafmt/pull/499) ([olafurpg](https://github.com/olafurpg))
- 486 [\#498](https://github.com/scalameta/scalafmt/pull/498) ([olafurpg](https://github.com/olafurpg))

## [v0.4.4](https://github.com/scalameta/scalafmt/tree/v0.4.4) (2016-10-07)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.4.3...v0.4.4)

**Merged pull requests:**

- Add scala-reflect back, fixes \#492. [\#495](https://github.com/scalameta/scalafmt/pull/495) ([olafurpg](https://github.com/olafurpg))
- Distinction between Import braces and string interpolation braces \#424 [\#454](https://github.com/scalameta/scalafmt/pull/454) ([hasumedic](https://github.com/hasumedic))

## [v0.4.3](https://github.com/scalameta/scalafmt/tree/v0.4.3) (2016-10-05)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.4.2...v0.4.3)

**Merged pull requests:**

- Port macros to inline meta [\#491](https://github.com/scalameta/scalafmt/pull/491) ([cb372](https://github.com/cb372))
- Produce clearer error messages for config typos in tests. [\#489](https://github.com/scalameta/scalafmt/pull/489) ([olafurpg](https://github.com/olafurpg))
- Display error message on configuration error in IntelliJ plugin [\#484](https://github.com/scalameta/scalafmt/pull/484) ([phalodi](https://github.com/phalodi))
- New rewrite rule for replacing parens with braces in for expressions [\#483](https://github.com/scalameta/scalafmt/pull/483) ([caoilte](https://github.com/caoilte))
- Fix for https://github.com/olafurpg/scalafmt/issues/478 [\#482](https://github.com/scalameta/scalafmt/pull/482) ([caoilte](https://github.com/caoilte))
- Created a scalafmt shell script to bootstrap usage [\#359](https://github.com/scalameta/scalafmt/pull/359) ([hntd187](https://github.com/hntd187))

## [v0.4.2](https://github.com/scalameta/scalafmt/tree/v0.4.2) (2016-09-28)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.4.1...v0.4.2)

**Merged pull requests:**

- Allow indentOperator = akka [\#475](https://github.com/scalameta/scalafmt/pull/475) ([olafurpg](https://github.com/olafurpg))

## [v0.4.1](https://github.com/scalameta/scalafmt/tree/v0.4.1) (2016-09-27)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.4.0...v0.4.1)

## [v0.4.0](https://github.com/scalameta/scalafmt/tree/v0.4.0) (2016-09-27)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/0.4...v0.4.0)

**Merged pull requests:**

- Remove redundant braces and no other tokens! [\#471](https://github.com/scalameta/scalafmt/pull/471) ([olafurpg](https://github.com/olafurpg))
- Add note on how to build the docs. [\#469](https://github.com/scalameta/scalafmt/pull/469) ([johnynek](https://github.com/johnynek))
- The great refactoring. [\#468](https://github.com/scalameta/scalafmt/pull/468) ([olafurpg](https://github.com/olafurpg))
- Add support for dynamic configuration, fixes \#315 [\#464](https://github.com/scalameta/scalafmt/pull/464) ([olafurpg](https://github.com/olafurpg))
- Semantics preserving rewrites before formatting. [\#447](https://github.com/scalameta/scalafmt/pull/447) ([olafurpg](https://github.com/olafurpg))

## [0.4](https://github.com/scalameta/scalafmt/tree/0.4) (2016-09-23)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.3.2-RC4...0.4)

**Merged pull requests:**

- Move configuration to HOCON [\#459](https://github.com/scalameta/scalafmt/pull/459) ([olafurpg](https://github.com/olafurpg))
- Bump scala.meta version to 1.1.0 [\#452](https://github.com/scalameta/scalafmt/pull/452) ([olafurpg](https://github.com/olafurpg))
- \#356 Code with windows line endings after formatting still has windows endings [\#445](https://github.com/scalameta/scalafmt/pull/445) ([mmatloka](https://github.com/mmatloka))

## [v0.3.2-RC4](https://github.com/scalameta/scalafmt/tree/v0.3.2-RC4) (2016-09-14)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.3.2-RC3...v0.3.2-RC4)

## [v0.3.2-RC3](https://github.com/scalameta/scalafmt/tree/v0.3.2-RC3) (2016-09-14)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.3.2-RC1...v0.3.2-RC3)

## [v0.3.2-RC1](https://github.com/scalameta/scalafmt/tree/v0.3.2-RC1) (2016-09-14)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.3.2-RC2...v0.3.2-RC1)

## [v0.3.2-RC2](https://github.com/scalameta/scalafmt/tree/v0.3.2-RC2) (2016-09-14)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.3.1...v0.3.2-RC2)

**Merged pull requests:**

- Add support to align by tokens that have no space before them [\#441](https://github.com/scalameta/scalafmt/pull/441) ([olafurpg](https://github.com/olafurpg))
- Add style config to keep new lines on chained method calls [\#440](https://github.com/scalameta/scalafmt/pull/440) ([melrief](https://github.com/melrief))
- Option to put a newline to be put before the parameters of a lambda [\#439](https://github.com/scalameta/scalafmt/pull/439) ([stefanobaghino](https://github.com/stefanobaghino))
- Exclude files with a CLI option [\#437](https://github.com/scalameta/scalafmt/pull/437) ([stefanobaghino](https://github.com/stefanobaghino))
- align %%% in defaultWithAlign [\#436](https://github.com/scalameta/scalafmt/pull/436) ([melrief](https://github.com/melrief))
- Add adopter: Teralytics [\#433](https://github.com/scalameta/scalafmt/pull/433) ([stefanobaghino](https://github.com/stefanobaghino))
- Update Adopters.scala [\#430](https://github.com/scalameta/scalafmt/pull/430) ([easel](https://github.com/easel))
- Fix \#417 [\#429](https://github.com/scalameta/scalafmt/pull/429) ([olafurpg](https://github.com/olafurpg))
- Add letgo to adopters [\#428](https://github.com/scalameta/scalafmt/pull/428) ([GMadorell](https://github.com/GMadorell))
- Update Adopters.scala [\#426](https://github.com/scalameta/scalafmt/pull/426) ([RomanIakovlev](https://github.com/RomanIakovlev))

## [v0.3.1](https://github.com/scalameta/scalafmt/tree/v0.3.1) (2016-08-24)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.3.0...v0.3.1)

**Merged pull requests:**

- Add hidden poorMansTrailingCommasInConfigStyle flag for fun [\#412](https://github.com/scalameta/scalafmt/pull/412) ([olafurpg](https://github.com/olafurpg))
- Format sbt files [\#411](https://github.com/scalameta/scalafmt/pull/411) ([olafurpg](https://github.com/olafurpg))
- Treat Pat.ExtractInfix like Term.ApplyInfix. Fixes \#408 [\#409](https://github.com/scalameta/scalafmt/pull/409) ([olafurpg](https://github.com/olafurpg))
- Make SBT plugin pick up .sbt files [\#407](https://github.com/scalameta/scalafmt/pull/407) ([mdedetrich](https://github.com/mdedetrich))

## [v0.3.0](https://github.com/scalameta/scalafmt/tree/v0.3.0) (2016-08-17)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.3.0-RC1...v0.3.0)

**Merged pull requests:**

- Add drone file. [\#406](https://github.com/scalameta/scalafmt/pull/406) ([olafurpg](https://github.com/olafurpg))

## [v0.3.0-RC1](https://github.com/scalameta/scalafmt/tree/v0.3.0-RC1) (2016-08-13)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.2.12...v0.3.0-RC1)

**Merged pull requests:**

- Format sbt files, fixes \#378 [\#402](https://github.com/scalameta/scalafmt/pull/402) ([olafurpg](https://github.com/olafurpg))
- 0.3.0 rc1 [\#401](https://github.com/scalameta/scalafmt/pull/401) ([olafurpg](https://github.com/olafurpg))
- Scalameta 1.0 [\#395](https://github.com/scalameta/scalafmt/pull/395) ([olafurpg](https://github.com/olafurpg))
- Don't force break before self annotation, fixes \#376 [\#391](https://github.com/scalameta/scalafmt/pull/391) ([olafurpg](https://github.com/olafurpg))
- Consolidate type compounds and parent constructor handling, fixes \#370 [\#390](https://github.com/scalameta/scalafmt/pull/390) ([olafurpg](https://github.com/olafurpg))
- Allow limited mixed owner alignment, fixes \#184 [\#389](https://github.com/scalameta/scalafmt/pull/389) ([olafurpg](https://github.com/olafurpg))
- Allow \_ as identifier before apply, fixes \#375 [\#388](https://github.com/scalameta/scalafmt/pull/388) ([olafurpg](https://github.com/olafurpg))
- Depend on scala-library in sbt plugin, fixes \#190 [\#374](https://github.com/scalameta/scalafmt/pull/374) ([olafurpg](https://github.com/olafurpg))

## [v0.2.12](https://github.com/scalameta/scalafmt/tree/v0.2.12) (2016-08-05)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.2.11...v0.2.12)

**Merged pull requests:**

- Increase control over infix operator indentation, fixes \#367. [\#372](https://github.com/scalameta/scalafmt/pull/372) ([olafurpg](https://github.com/olafurpg))
- Add spaceBeforeContextBoundColon option [\#369](https://github.com/scalameta/scalafmt/pull/369) ([triggerNZ](https://github.com/triggerNZ))
- Updated sbt-assembly to 0.14.3 [\#361](https://github.com/scalameta/scalafmt/pull/361) ([VEINHORN](https://github.com/VEINHORN))
- Fix indent expire token for lambdas, towards \#357 [\#358](https://github.com/scalameta/scalafmt/pull/358) ([olafurpg](https://github.com/olafurpg))

## [v0.2.11](https://github.com/scalameta/scalafmt/tree/v0.2.11) (2016-07-08)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.2.10...v0.2.11)

## [v0.2.10](https://github.com/scalameta/scalafmt/tree/v0.2.10) (2016-06-30)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.2.9...v0.2.10)

## [v0.2.9](https://github.com/scalameta/scalafmt/tree/v0.2.9) (2016-06-30)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.2.8...v0.2.9)

**Merged pull requests:**

- 339 [\#352](https://github.com/scalameta/scalafmt/pull/352) ([olafurpg](https://github.com/olafurpg))

## [v0.2.8](https://github.com/scalameta/scalafmt/tree/v0.2.8) (2016-06-23)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.2.7...v0.2.8)

## [v0.2.7](https://github.com/scalameta/scalafmt/tree/v0.2.7) (2016-06-23)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.2.6...v0.2.7)

## [v0.2.6](https://github.com/scalameta/scalafmt/tree/v0.2.6) (2016-06-22)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.2.6-RC1...v0.2.6)

**Merged pull requests:**

- Allow NoSplit in config style in case of single line policy [\#335](https://github.com/scalameta/scalafmt/pull/335) ([olafurpg](https://github.com/olafurpg))
- Depenalize line breaks inside {}, fixes \#318 [\#333](https://github.com/scalameta/scalafmt/pull/333) ([olafurpg](https://github.com/olafurpg))
- Remove "bad files" from mega test, fixes \#280 [\#332](https://github.com/scalameta/scalafmt/pull/332) ([olafurpg](https://github.com/olafurpg))
- No newline between = {, fixes \#328. [\#329](https://github.com/scalameta/scalafmt/pull/329) ([olafurpg](https://github.com/olafurpg))
- No space before Decl.Type, fixes \#288 [\#328](https://github.com/scalameta/scalafmt/pull/328) ([olafurpg](https://github.com/olafurpg))
- Fail fast instead of best-effort formatting [\#326](https://github.com/scalameta/scalafmt/pull/326) ([olafurpg](https://github.com/olafurpg))
- Add macro benchmark. [\#320](https://github.com/scalameta/scalafmt/pull/320) ([olafurpg](https://github.com/olafurpg))
- Correct all other mentions of "paretheses" [\#314](https://github.com/scalameta/scalafmt/pull/314) ([i-am-the-slime](https://github.com/i-am-the-slime))
- Typo in parentheses [\#313](https://github.com/scalameta/scalafmt/pull/313) ([i-am-the-slime](https://github.com/i-am-the-slime))
- Fix typos [\#311](https://github.com/scalameta/scalafmt/pull/311) ([williamho](https://github.com/williamho))

## [v0.2.6-RC1](https://github.com/scalameta/scalafmt/tree/v0.2.6-RC1) (2016-06-01)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.2.5...v0.2.6-RC1)

**Merged pull requests:**

- gitignore and clean repos.tar.gz [\#302](https://github.com/scalameta/scalafmt/pull/302) ([annappropriate](https://github.com/annappropriate))
- Space after triple equals [\#298](https://github.com/scalameta/scalafmt/pull/298) ([annappropriate](https://github.com/annappropriate))
- Expose --alignByArrowEnumeratorGenerator in CLI [\#287](https://github.com/scalameta/scalafmt/pull/287) ([djspiewak](https://github.com/djspiewak))
- fallback to config file in home dir [\#246](https://github.com/scalameta/scalafmt/pull/246) ([maximn](https://github.com/maximn))
- Drop %% in addSbtPlugin [\#245](https://github.com/scalameta/scalafmt/pull/245) ([dwijnand](https://github.com/dwijnand))

## [v0.2.5](https://github.com/scalameta/scalafmt/tree/v0.2.5) (2016-05-17)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.2.4...v0.2.5)

## [v0.2.4](https://github.com/scalameta/scalafmt/tree/v0.2.4) (2016-05-16)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.2.3...v0.2.4)

**Merged pull requests:**

- 0.2.4 rc3 [\#240](https://github.com/scalameta/scalafmt/pull/240) ([olafurpg](https://github.com/olafurpg))
- 0.2.4 rc2 [\#234](https://github.com/scalameta/scalafmt/pull/234) ([olafurpg](https://github.com/olafurpg))
- 0.2.4 rc1 [\#231](https://github.com/scalameta/scalafmt/pull/231) ([olafurpg](https://github.com/olafurpg))
- Use unmanagedSourceDirectories for sourceDirectories [\#229](https://github.com/scalameta/scalafmt/pull/229) ([fthomas](https://github.com/fthomas))
- Optimizations: roughly 17% improvement [\#216](https://github.com/scalameta/scalafmt/pull/216) ([olafurpg](https://github.com/olafurpg))
- Parallel [\#215](https://github.com/scalameta/scalafmt/pull/215) ([olafurpg](https://github.com/olafurpg))

## [v0.2.3](https://github.com/scalameta/scalafmt/tree/v0.2.3) (2016-05-06)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.2.2...v0.2.3)

**Merged pull requests:**

- 0.2.3 [\#213](https://github.com/scalameta/scalafmt/pull/213) ([olafurpg](https://github.com/olafurpg))
- 0.2.3 [\#211](https://github.com/scalameta/scalafmt/pull/211) ([olafurpg](https://github.com/olafurpg))
- Fixes the check for URLs vs files [\#206](https://github.com/scalameta/scalafmt/pull/206) ([propensive](https://github.com/propensive))

## [v0.2.2](https://github.com/scalameta/scalafmt/tree/v0.2.2) (2016-04-19)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.2.1...v0.2.2)

**Merged pull requests:**

- 0.2.2 [\#189](https://github.com/scalameta/scalafmt/pull/189) ([olafurpg](https://github.com/olafurpg))
- 0.2.2 [\#173](https://github.com/scalameta/scalafmt/pull/173) ([olafurpg](https://github.com/olafurpg))

## [v0.2.1](https://github.com/scalameta/scalafmt/tree/v0.2.1) (2016-04-14)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.2.0...v0.2.1)

**Merged pull requests:**

- 0.2.1 [\#156](https://github.com/scalameta/scalafmt/pull/156) ([olafurpg](https://github.com/olafurpg))

## [v0.2.0](https://github.com/scalameta/scalafmt/tree/v0.2.0) (2016-04-12)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.1.6...v0.2.0)

**Merged pull requests:**

- 0.2.0 [\#146](https://github.com/scalameta/scalafmt/pull/146) ([olafurpg](https://github.com/olafurpg))

## [v0.1.6](https://github.com/scalameta/scalafmt/tree/v0.1.6) (2016-04-05)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.1.5...v0.1.6)

**Merged pull requests:**

- 0.1.6 - format all the files [\#142](https://github.com/scalameta/scalafmt/pull/142) ([olafurpg](https://github.com/olafurpg))
- 0.1.6-SNAPSHOT [\#137](https://github.com/scalameta/scalafmt/pull/137) ([olafurpg](https://github.com/olafurpg))
- 0.1.5 [\#136](https://github.com/scalameta/scalafmt/pull/136) ([olafurpg](https://github.com/olafurpg))

## [v0.1.5](https://github.com/scalameta/scalafmt/tree/v0.1.5) (2016-04-01)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.1.4...v0.1.5)

**Merged pull requests:**

- 0.1.5-SNAPSHOT [\#132](https://github.com/scalameta/scalafmt/pull/132) ([olafurpg](https://github.com/olafurpg))

## [v0.1.4](https://github.com/scalameta/scalafmt/tree/v0.1.4) (2016-03-18)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.1.3...v0.1.4)

**Merged pull requests:**

- Reformat project with 0.1.4 [\#130](https://github.com/scalameta/scalafmt/pull/130) ([olafurpg](https://github.com/olafurpg))
- 0.1.4 fixes [\#129](https://github.com/scalameta/scalafmt/pull/129) ([olafurpg](https://github.com/olafurpg))
- 0.1.4 [\#128](https://github.com/scalameta/scalafmt/pull/128) ([olafurpg](https://github.com/olafurpg))

## [v0.1.3](https://github.com/scalameta/scalafmt/tree/v0.1.3) (2016-03-11)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.1.2...v0.1.3)

**Merged pull requests:**

- 0.1.3 [\#124](https://github.com/scalameta/scalafmt/pull/124) ([olafurpg](https://github.com/olafurpg))
- Move intellij plugin to this repo. [\#119](https://github.com/scalameta/scalafmt/pull/119) ([olafurpg](https://github.com/olafurpg))

## [v0.1.2](https://github.com/scalameta/scalafmt/tree/v0.1.2) (2016-03-10)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.1.1...v0.1.2)

**Merged pull requests:**

- Fix most issues from @sjrd [\#118](https://github.com/scalameta/scalafmt/pull/118) ([olafurpg](https://github.com/olafurpg))
- Add Java 6 compatibility. [\#113](https://github.com/scalameta/scalafmt/pull/113) ([olafurpg](https://github.com/olafurpg))
- "Bootstrap": format scalafmt with scalafmt [\#93](https://github.com/scalameta/scalafmt/pull/93) ([olafurpg](https://github.com/olafurpg))

## [v0.1.1](https://github.com/scalameta/scalafmt/tree/v0.1.1) (2016-03-08)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.1.0...v0.1.1)

**Merged pull requests:**

- Format all files in directory with cli. [\#92](https://github.com/scalameta/scalafmt/pull/92) ([olafurpg](https://github.com/olafurpg))

## [v0.1.0](https://github.com/scalameta/scalafmt/tree/v0.1.0) (2016-03-07)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.1.0-RC3...v0.1.0)

**Merged pull requests:**

- Setup Scalatex doc site. [\#90](https://github.com/scalameta/scalafmt/pull/90) ([olafurpg](https://github.com/olafurpg))
- Sbt plugin [\#89](https://github.com/scalameta/scalafmt/pull/89) ([olafurpg](https://github.com/olafurpg))
- Remove logging dependency from core. [\#88](https://github.com/scalameta/scalafmt/pull/88) ([olafurpg](https://github.com/olafurpg))

## [v0.1.0-RC3](https://github.com/scalameta/scalafmt/tree/v0.1.0-RC3) (2016-03-04)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.1.0-RC2...v0.1.0-RC3)

**Merged pull requests:**

- Add support to call from Scala 2.10. [\#87](https://github.com/scalameta/scalafmt/pull/87) ([olafurpg](https://github.com/olafurpg))

## [v0.1.0-RC2](https://github.com/scalameta/scalafmt/tree/v0.1.0-RC2) (2016-03-03)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/v0.1.0-RC0...v0.1.0-RC2)

**Merged pull requests:**

- Publish to Maven Central. [\#86](https://github.com/scalameta/scalafmt/pull/86) ([olafurpg](https://github.com/olafurpg))
- Support binpack + non-binpack parameters. [\#85](https://github.com/scalameta/scalafmt/pull/85) ([olafurpg](https://github.com/olafurpg))
- Optimization: limit queue size even inside no opt zones. [\#84](https://github.com/scalameta/scalafmt/pull/84) ([olafurpg](https://github.com/olafurpg))
- Pretty [\#82](https://github.com/scalameta/scalafmt/pull/82) ([olafurpg](https://github.com/olafurpg))
- Various optimizations, 4 files to go Scala.js. [\#81](https://github.com/scalameta/scalafmt/pull/81) ([olafurpg](https://github.com/olafurpg))
- Up to ~30% performance improvements. [\#79](https://github.com/scalameta/scalafmt/pull/79) ([olafurpg](https://github.com/olafurpg))
- Fix all remaining output bugs. [\#78](https://github.com/scalameta/scalafmt/pull/78) ([olafurpg](https://github.com/olafurpg))
- Remove memoization, ~100x speed improvement. [\#77](https://github.com/scalameta/scalafmt/pull/77) ([olafurpg](https://github.com/olafurpg))
- Fix multiple output bugs from FormatExperiment. [\#76](https://github.com/scalameta/scalafmt/pull/76) ([olafurpg](https://github.com/olafurpg))
- Fix output bugs, scalafmt can format itself! [\#75](https://github.com/scalameta/scalafmt/pull/75) ([olafurpg](https://github.com/olafurpg))
- Minor optimizations. [\#74](https://github.com/scalameta/scalafmt/pull/74) ([olafurpg](https://github.com/olafurpg))
- Limit calls to token.position.end.line. [\#73](https://github.com/scalameta/scalafmt/pull/73) ([olafurpg](https://github.com/olafurpg))
- Fix output bugs, find new ones. [\#72](https://github.com/scalameta/scalafmt/pull/72) ([olafurpg](https://github.com/olafurpg))
- Find optimal tokens 3x faster, pay with memory. [\#71](https://github.com/scalameta/scalafmt/pull/71) ([olafurpg](https://github.com/olafurpg))
- Prepare for optimisations. [\#70](https://github.com/scalameta/scalafmt/pull/70) ([olafurpg](https://github.com/olafurpg))
- Refactor. [\#69](https://github.com/scalameta/scalafmt/pull/69) ([olafurpg](https://github.com/olafurpg))
- Move tests back to core, fixes \#66 [\#68](https://github.com/scalameta/scalafmt/pull/68) ([olafurpg](https://github.com/olafurpg))
- Add more benchmarks. [\#67](https://github.com/scalameta/scalafmt/pull/67) ([olafurpg](https://github.com/olafurpg))
- Benchmarks [\#65](https://github.com/scalameta/scalafmt/pull/65) ([olafurpg](https://github.com/olafurpg))
- Setup multi-project build for easier benchmarking. [\#64](https://github.com/scalameta/scalafmt/pull/64) ([olafurpg](https://github.com/olafurpg))
- Run formatter on scala-js repo. [\#63](https://github.com/scalameta/scalafmt/pull/63) ([olafurpg](https://github.com/olafurpg))
- Fix warts in output. [\#62](https://github.com/scalameta/scalafmt/pull/62) ([olafurpg](https://github.com/olafurpg))
- Format most files with scalafmt, yay :D [\#61](https://github.com/scalameta/scalafmt/pull/61) ([olafurpg](https://github.com/olafurpg))

## [v0.1.0-RC0](https://github.com/scalameta/scalafmt/tree/v0.1.0-RC0) (2016-02-09)

[Full Changelog](https://github.com/scalameta/scalafmt/compare/358ce783b5eb9a854006620ab9c058f3f7c85c0b...v0.1.0-RC0)

**Merged pull requests:**

- Add --range flag to cli. [\#60](https://github.com/scalameta/scalafmt/pull/60) ([olafurpg](https://github.com/olafurpg))
- OptimalAt keyed by state.column -\> format token. [\#59](https://github.com/scalameta/scalafmt/pull/59) ([olafurpg](https://github.com/olafurpg))
- More polish. [\#58](https://github.com/scalameta/scalafmt/pull/58) ([olafurpg](https://github.com/olafurpg))
- Update Split. [\#57](https://github.com/scalameta/scalafmt/pull/57) ([olafurpg](https://github.com/olafurpg))
- Add optimalAt optimization. [\#56](https://github.com/scalameta/scalafmt/pull/56) ([olafurpg](https://github.com/olafurpg))
- Fix formatting for lots of statements. [\#54](https://github.com/scalameta/scalafmt/pull/54) ([olafurpg](https://github.com/olafurpg))
- Bump test coverage. [\#53](https://github.com/scalameta/scalafmt/pull/53) ([olafurpg](https://github.com/olafurpg))
- Refactor: replace Push with Indent\[T \<: Length\] [\#52](https://github.com/scalameta/scalafmt/pull/52) ([olafurpg](https://github.com/olafurpg))
- Strip ONLY/SKIP from test names. [\#51](https://github.com/scalameta/scalafmt/pull/51) ([olafurpg](https://github.com/olafurpg))
- Refine Formatter cases. [\#50](https://github.com/scalameta/scalafmt/pull/50) ([olafurpg](https://github.com/olafurpg))
- No space on ApplyUnary. [\#49](https://github.com/scalameta/scalafmt/pull/49) ([olafurpg](https://github.com/olafurpg))
- Await in afterAll until results are submitted. [\#48](https://github.com/scalameta/scalafmt/pull/48) ([olafurpg](https://github.com/olafurpg))
- Add expire field to Indent. [\#47](https://github.com/scalameta/scalafmt/pull/47) ([olafurpg](https://github.com/olafurpg))
- Remove couchapp to separate project. [\#46](https://github.com/scalameta/scalafmt/pull/46) ([olafurpg](https://github.com/olafurpg))
- Simplify data submission. [\#45](https://github.com/scalameta/scalafmt/pull/45) ([olafurpg](https://github.com/olafurpg))
- Too big change :\( [\#44](https://github.com/scalameta/scalafmt/pull/44) ([olafurpg](https://github.com/olafurpg))
- Update Travis jdk to 8. [\#40](https://github.com/scalameta/scalafmt/pull/40) ([olafurpg](https://github.com/olafurpg))
- Add stopwatch class. [\#39](https://github.com/scalameta/scalafmt/pull/39) ([olafurpg](https://github.com/olafurpg))
- Regression testing with speed.scalafmt.org, fixes \#11. [\#38](https://github.com/scalameta/scalafmt/pull/38) ([olafurpg](https://github.com/olafurpg))
- Format with 2x newlines. [\#37](https://github.com/scalameta/scalafmt/pull/37) ([olafurpg](https://github.com/olafurpg))
- Add heatmap over state visits. [\#35](https://github.com/scalameta/scalafmt/pull/35) ([olafurpg](https://github.com/olafurpg))
- Shrink search space. [\#34](https://github.com/scalameta/scalafmt/pull/34) ([olafurpg](https://github.com/olafurpg))
- Format argument lists. [\#31](https://github.com/scalameta/scalafmt/pull/31) ([olafurpg](https://github.com/olafurpg))
- Link tests from readme. [\#27](https://github.com/scalameta/scalafmt/pull/27) ([olafurpg](https://github.com/olafurpg))
- Use policy to unindent when leaving block, fixes \#19. [\#26](https://github.com/scalameta/scalafmt/pull/26) ([olafurpg](https://github.com/olafurpg))
- Introduce policy, fixes \#15. [\#24](https://github.com/scalameta/scalafmt/pull/24) ([olafurpg](https://github.com/olafurpg))
- Add a Gitter chat badge to readme.md [\#22](https://github.com/scalameta/scalafmt/pull/22) ([gitter-badger](https://github.com/gitter-badger))
- Setup Coveralls. [\#21](https://github.com/scalameta/scalafmt/pull/21) ([olafurpg](https://github.com/olafurpg))
- Add column-limit with basic strategies. [\#20](https://github.com/scalameta/scalafmt/pull/20) ([olafurpg](https://github.com/olafurpg))
- Add pomExtra. [\#16](https://github.com/scalameta/scalafmt/pull/16) ([olafurpg](https://github.com/olafurpg))
- Add FormatToken, fixes \#9. [\#10](https://github.com/scalameta/scalafmt/pull/10) ([olafurpg](https://github.com/olafurpg))
- Downgrade Coveralls. [\#6](https://github.com/scalameta/scalafmt/pull/6) ([olafurpg](https://github.com/olafurpg))
- Add Travis badge to readme. [\#5](https://github.com/scalameta/scalafmt/pull/5) ([olafurpg](https://github.com/olafurpg))
- Add multi-project support for coveralls. [\#4](https://github.com/scalameta/scalafmt/pull/4) ([olafurpg](https://github.com/olafurpg))
- Coveralls [\#3](https://github.com/scalameta/scalafmt/pull/3) ([olafurpg](https://github.com/olafurpg))
- Coverage [\#2](https://github.com/scalameta/scalafmt/pull/2) ([olafurpg](https://github.com/olafurpg))
- Format using shortest path search. [\#1](https://github.com/scalameta/scalafmt/pull/1) ([olafurpg](https://github.com/olafurpg))

\* _This Changelog was automatically generated by [github_changelog_generator](https://github.com/github-changelog-generator/github-changelog-generator)_
