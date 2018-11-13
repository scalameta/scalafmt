---
id: installation
title: Installation
---

You can use Scalafmt from your editor, build tool or terminal.

## IntelliJ

To install the
[Scalafmt IntelliJ plugin](https://plugins.jetbrains.com/plugin/8236?pr=)

- open `Preferences > Plugins`
- open `Browse repositories`
- search for `scalafmt`
- click "Install"
- restart IntelliJ

![Scalafmt IntelliJ Plugin](assets/img/intellij-plugin.png)

### Format current file

- `Cmd + Shift + L` (macOS)
- `Ctrl + Shift + L` (other)

To re-configure the shortcut

- Open `Preferences > Keymap`
- Search for "Reformat with scalafmt"

### Format on save

- for the current project (recommended): `Preferences > Tools > Scalafmt`
- for all new project:
  `File > Other settings > Preferences for new projects > Tools > Scalafmt`

![Enable format on save in IntelliJ](assets/img/intellij-on-save.png)

### Install nightly plugin

To try out the latest pending releases for the Scalafmt plugin:

- Visit
  [Scalafmt plugin page](https://plugins.jetbrains.com/plugin/8236-scalafmt)
- Select "nightly"
- Click "Download"
- Open IntelliJ
- Uninstall existing Scalafmt plugin installation, if any
- Select `Preferences > Plugins > "install plugin from disk..."`
- Choose the downloaded `intellij-scalafmt.zip`
- Restart IntelliJ

### Continue using IntelliJ formatter

When prompted whether to "use scalafmt formatter" make sure to select "continue
using IntelliJ formatter"

![IntelliJ scalafmt formatter](assets/img/intellij-install.png)

As long as you have the Scalafmt plugin installed as instructed above you can
still [format current file](#format-current-file) and
[format on save](#format-on-save).

It is not recommended to select "use scalafmt formatter" since the built-in
support provided by IntelliJ has limitations

- it is hardcoded against a single Scalafmt version (v1.5.1 at this time),
  making it difficult to upgrade to new releases.
- it enables undesirable behavior such as formatting expanded snippets (example:
  "implement methods" inspection) with a low column width. Scalafmt is primarily
  designed to format entire text files instead of individual snippets of code.

To reset the formatter to IntelliJ for an existing project that uses the
Scalafmt formatter:

- Open `Preferences > Editor > Code Style > Scala`
- Switch "Formatter" value to "IntelliJ"

It is not possible to reset this setting for all existing projects.

## sbt

```scala
// In project/plugins.sbt. Note, does not support sbt 0.13, only sbt 1.0.
addSbtPlugin("com.geirsson" % "sbt-scalafmt" % "1.5.1")
```

[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.geirsson/sbt-scalafmt/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.geirsson/sbt-scalafmt)

### Task keys

- `myproject/scalafmt`: Format main sources of `myproject` project
- `myproject/test:scalafmt`: Format test sources of `myproject` project
- `scalafmtCli`: Run the scalafmt command line interface.
- `scalafmtCheck`: Check if the scala sources under the project has been
  formatted.
- `scalafmtSbt`: Format `*.sbt` and `project/*.scala` files.
- `scalafmtSbtCheck`: Check if the files has been formatted by `scalafmtSbt`.
- `scalafmtOnly`: Format a single given file.

### Customize configuration location

- `scalafmtConfig: Option[File]`: Optional location of `.scalafmt.conf` file. If
  `None` the default config is used. By default, `.scalafmt.conf` file on the
  project root will be used.

### Format on compile

> ⚠️ This option is **discouraged**, it is recommended to use "format on save"
> in the editor instead.

- `scalafmtOnCompile: Boolean`: Defines if the sbt-scalafmt should run scalafmt
  on compile. Default `false`.

### Enable IntegrationTest

The sbt plugin is enabled by default for the Test and Compile configurations.
Use `scalafmtConfigSettings` to enable the plugin for integration tests and then
use `it:scalafmt` to format.

```scala
inConfig(IntegrationTest)(scalafmtConfigSettings)
```

### Share configuration between builds

To share configuration across different sbt builds, create a custom sbt plugin
that generates `.scalafmt.conf` on build reload.

```scala
// project/MyScalafmtPlugin.scala
import sbt._
object MyScalafmtPlugin extends AutoPlugin {
  override def trigger = allRequirements
  override def requires = plugins.JvmPlugin
  override def buildSettings: Seq[Def.Setting[_]] = {
    SettingKey[Unit]("scalafmtGenerateConfig") :=
      IO.write(
        // writes to file once when build is loaded
        file(".scalafmt.conf"),
        "maxColumn = 100".stripMargin.getBytes("UTF-8")
      )
  }
}
```

## CLI

The recommended way to install the scalafmt command line tool is with
[Coursier](#coursier).

### Coursier

<div class="sidenote">
To install Coursier see <a href="https://github.com/coursier/coursier#command-line" target="_blank">here</a>
</div>

Create a standalone executable in `/usr/local/bin/scalafmt` with (sudo if
necessary):

```sh
coursier bootstrap com.geirsson:scalafmt-cli_2.12:@STABLE_VERSION@ \
  -r bintray:scalameta/maven \
  -o /usr/local/bin/scalafmt --standalone --main org.scalafmt.cli.Cli
scalafmt --version # should be @STABLE_VERSION@
```

Alternatively you can create a slim 15 KiB bootstrap script with:

```sh
coursier bootstrap com.geirsson:scalafmt-cli_2.12:@STABLE_VERSION@ \
  -r bintray:scalameta/maven \
  -o scalafmt --main org.scalafmt.cli.Cli
./scalafmt --version # should be @STABLE_VERSION@
```

It is **recommended** to put this bootstrap script in your code repository to
make sure everyone on your team, as well as CI, uses the same scalafmt version.
To configure which files to format, see [project](configuration.md#project).

To customize the JVM options, use the Coursier option `--java-opt`, more info
with

```sh
coursier bootstrap --help | grep -A 1 "\-\-java-opt"
```

### Pre-release

Our CI publishes a pre-release version of scalafmt to Bintray on every merge
into master. To use a pre-release, replace @STABLE_VERSION@ with the version
here:

<a href='https://bintray.com/scalameta/maven/scalafmt-cli/_latestVersion'>
    <img src='https://api.bintray.com/packages/scalameta/maven/scalafmt-cli/images/download.svg'>
</a>

If you use coursier to install a pre-release, be sure to include the flag -r
bintray:scalameta/maven so that the artifact can be resolved.

If you use sbt to install a pre-release, be sure to add the following setting

```scala
resolvers += Resolver.bintrayRepo("scalameta", "maven")
```

### Nailgun

Nailgun is recommended if you want to integrate scalafmt with a text editor like
vim/Emacs/Atom/Sublime/VS Code.

- Make sure you have a nailgun client installed. For example with
  `brew install nailgun`.
- Create a standalone executable in `/usr/local/bin/scalafmt_ng` with (sudo if
  necessary)

```sh
coursier bootstrap --standalone com.geirsson:scalafmt-cli_2.12:@STABLE_VERSION@ \
  -r bintray:scalameta/maven \
  -o /usr/local/bin/scalafmt_ng -f --main com.martiansoftware.nailgun.NGServer
scalafmt_ng & // start nailgun in background
ng ng-alias scalafmt org.scalafmt.cli.Cli
ng scalafmt --version # should be @STABLE_VERSION@
```

Nailgun keeps scalafmt running on a local server to avoid the JVM startup
penalty and also so scalafmt can benefit from JIT. This makes scalafmt up to 10x
faster when formatting a single file from the CLI. The downside to Nailgun is
that the setup is complicated and the long-running server needs to be restarted
once in awhile.

### Homebrew

You can install scalafmt via Homebrew using a custom formula

```sh
brew install --HEAD olafurpg/scalafmt/scalafmt
scalafmt --version // should be @STABLE_VERSION@

// to upgrade between releases
brew upgrade scalafmt
```

### --help

```scala mdoc:passthrough
println(website.plaintext(org.scalafmt.cli.CliArgParser.buildInfo))
```

```scala mdoc:passthrough
println(website.plaintext(org.scalafmt.cli.CliArgParser.scoptParser.usage))
```

## Gradle

It is possible to use scalafmt in gradle with the following externally
maintained plugins:

- [Spotless](https://github.com/diffplug/spotless/tree/master/plugin-gradle#applying-scalafmt-to-scala-files)
- [gradle-scalafmt](https://github.com/alenkacz/gradle-scalafmt)

## Maven

It is possible to use scalafmt in Maven with the following externally maintained
plugin:

- [mvn_scalafmt](https://github.com/SimonJPegg/mvn_scalafmt)

## Mill

Mill have scalafmt support built-in:

- [scalafmt module](http://www.lihaoyi.com/mill/page/configuring-mill.html#reformatting-your-code)

## Vim

- Make sure you have the [CLI](#cli) installed and working.
- install [vim-autoformat](https://github.com/Chiel92/vim-autoformat)
- add to your `.vimrc`

```
noremap <F5> :Autoformat<CR>
let g:formatdef_scalafmt = "'scalafmt --stdin'"
let g:formatters_scala = ['scalafmt']
```

<div class="sidenote">
You pay the JVM startup penalty on every format unless you're using
<a href="#nailgun">Nailgun</a>.
</div>

## Standalone library

Add to your dependencies

```scala
libraryDependencies += "com.geirsson" %% "scalafmt-core" % "@STABLE_VERSION@"
// Scala.js
libraryDependencies += "com.geirsson" %%% "scalafmt-core" % "@STABLE_VERSION@"
```

Use the API like this:

```scala mdoc
org.scalafmt.Scalafmt.format("""
      object FormatMe { List(Split(Space, 0).withPolicy(SingleLineBlock(close)), Split(Newline, 1).withPolicy{ case Decision(t@FormatToken(_, `close`, _), s) => Decision(t, List(Split(Newline, 0)))}.withIndent(2, close, Right)) }
""").get
```

Obtain a configuration object with `parseHoconConfig`

```scala mdoc:silent
val config = org.scalafmt.Scalafmt.parseHoconConfig("align=most").get
```

```scala mdoc
org.scalafmt.Scalafmt.format("""
    object Align {
        val x = 1
        val xx = 2
    }
""", config).get
```

To format code with top-level statements like `*.sbt` files

```scala mdoc:silent
val configForSbt = org.scalafmt.Scalafmt.configForSbt(config)
```

```scala mdoc
org.scalafmt.Scalafmt.format("""
    val x = 1
    val xx = 2
""", configForSbt).get
```

The Scalafmt public API consists only of methods in`org.scalafmt.Scalafmt`. In
particular, case classes in `org.scalafmt.config` are subject to binary and
source breaking changes on any release.

## Help Wanted

- Ensime
- Scala IDE ([help wanted!](https://github.com/scalameta/scalafmt/issues/125))
- Your favorite editor? Join the gitter channel.

[intellij-ticket]: https://youtrack.jetbrains.com/issue/SCL-13658
