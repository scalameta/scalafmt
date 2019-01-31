---
id: installation
title: Installation
---

You can use Scalafmt from your editor, build tool or terminal.

## IntelliJ

To install the
[Scalafmt IntelliJ plugin](https://plugins.jetbrains.com/plugin/8236?pr=)

- open `Preferences > Plugins` (for Windows/Linux it is called `Settings`
  instead of `Preferences`)
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
inConfig(IntegrationTest)(org.scalafmt.sbt.ScalafmtPlugin.scalafmtConfigSettings)
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
coursier bootstrap org.scalameta:scalafmt-cli_2.12:@STABLE_VERSION@ \
  -r bintray:scalameta/maven \
  -o /usr/local/bin/scalafmt --standalone --main org.scalafmt.cli.Cli
scalafmt --version # should be @STABLE_VERSION@
```

Alternatively you can create a slim 15 KiB bootstrap script with:

```sh
coursier bootstrap org.scalameta:scalafmt-cli_2.12:@STABLE_VERSION@ \
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
coursier bootstrap --standalone org.scalameta:scalafmt-cli_2.12:@STABLE_VERSION@ \
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

### Arch Linux

You can install scalafmt for Arch Linux from AUR. There is the [scalafmt-native](https://aur.archlinux.org/packages/scalafmt-native) package that installs scalafmt 
binary built with GraalVM. GraalVM native binary provides instant startup without Nailgun.
```sh
yaourt -S scalafmt-native
scalafmt --version // should be @STABLE_VERSION@
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

Use the `scalafmt-dynamic` module to integrate with Scalafmt.

```scala
libraryDependencies += "org.scalameta" %% "scalafmt-dynamic" % "@STABLE_VERSION@"
```

First, create an instance of `Scalafmt` and get paths for the file to format
along with it's configuration file.

```scala mdoc:silent
import java.nio.file._
import org.scalafmt.interfaces.Scalafmt
val scalafmt = Scalafmt.create(this.getClass.getClassLoader)
val config = Paths.get(".scalafmt.conf")
val file = Paths.get("Main.scala")
```

Use the `format` method to format a string with the given config and filename.

```scala mdoc
println(scalafmt.format(config, file, "object A  {  }"))
```

### Binary compatibility guarantees

Stable public APIs:

- `org.scalafmt.interfaces` (recommended): pure Java APIs with no external
  dependencies. Can be loaded via the `scalafmt-dynamic` module.
- `org.scalafmt.Scalafmt` (discouraged): old public API that is stable and will
  remain stable but has several limitations.
  - no support for `version` in `.scalafmt.conf`
  - does not respect `project.excludeFilters` in `.scalafmt.conf`
  - doesn't automatically handle `*.sbt` and `*.sc` files
  - no caching of `.scalafmt.conf`

Internal APIs that are subject to binary breaking changes in any release:

- `org.scalafmt.dynamic`: private implementation of `scalafmt-interfaces`. These
  classes can be used via the static method
  `org.scalafmt.interfaces.Scalafmt.create(ClassLoader)`.
- `org.scalafmt.config`: case classes for `.scalafmt.conf` configuration that
  that are only intended for internal usage.
- `org.scalafmt.cli`: private implementation of the command-line interface.

### `*.sbt` and `*.sc` files

It's possible to format `*.sbt` and `*.sc` files.

```scala mdoc
println(scalafmt.format(config, Paths.get("build.sbt"), "lazy    val   x =   project"))
println(scalafmt.format(config, Paths.get("build.sc"), "def  main(  ) = println()"))
```

The `scalafmt` instance automatically picks the correct parser depending on the
provided filename.

### Version handling

By default, the `scalafmt` instance automatically downloads the Scalafmt version
declared in `.scalafmt.conf`. If the `version` setting is not declared, the
original file contents are returned unchanged and an error is reported with
instructions how to fix the problem.

Use `withRespectVersion(false)` to fall back to a default Scalafmt version when
its not declared in `.scalafmt.conf`. Use `withDefaultVersion(version)` to
customize the fallback version.

```scala mdoc:silent
val scalafmtThatDoesntRequireVersionSetting = scalafmt
  .withRespectVersion(false)
  .withDefaultVersion("@STABLE_VERSION@")
```

### Error reporting

By default, Scalafmt errors are reported to `System.err`. Extend
`org.scalafmt.interfaces.ScalafmtReporter` to customize error reporting to
handle parse and config errors.

Here is an example how to extend `ScalafmtReporter`.

```scala mdoc:file:scalafmt-dynamic/src/main/scala/org/scalafmt/dynamic/ConsoleScalafmtReporter.scala

```

Use `withReporter(reporter)` to pass in your custom reporter.

```scala mdoc:silent
import java.io._
import org.scalafmt.dynamic._
val myOut = new ByteArrayOutputStream()
val myReporter = new ConsoleScalafmtReporter(new PrintStream(myOut))
val customReporterScalafmt = scalafmt.withReporter(myReporter)
```

### Project filters

By default, `scalafmt` only formats files that match the
`project.{excludeFilters,includeFilters}` settings in `.scalafmt.conf`. Use
`withRespectExcludeFilters(false)` to disable this behavior.

```scala mdoc:silent
val scalafmtThatIgnoresProjectSettings = scalafmt.withRespectProjectFilters(false)
```

### Clearing resources

Use the `clear()` method to clear up resources of the `scalafmt` instance.

```scala mdoc
scalafmt.clear()
```

### Calling from Java

It's possible to call Scalafmt from Java without depending directly on Scala
libraries.

First, depend on the `scalafmt-interfaces` module, which is a pure Java library
with no external dependencies.

```xml
<dependency>
    <groupId>org.scalameta</groupId>
    <artifactId>scalafmt-interfaces</artifactId>
    <version>@STABLE_VERSION@</version>
</dependency>
```

Next, obtain a classloader with the `scalafmt-dynamic_2.12` classpath.

```java
import java.net.URLClassLoader;
// this package contains only Java APIs.
import org.scalafmt.interfaces.*;

// ClassLoader that shares only org.scalafmt.interfaces from this classloader.
ClassLoader sharedParent = new ScalafmtClassLoader(this.getClass.getClassLoader)

// Jars to org.scalameta:scalafmt-dynamic_2.12:@STABLE_VERSION@ classpath. Obtain
// these from your build tool or programmatically with ivy/coursier.
URL[] jars = // ...
ClassLoader scalafmtDynamic = new URLClassLoader(jars, sharedParent)
```

Finally, create an instance of `Scalafmt` with the `scalafmt-dynamic`
classloader.

```java
Scalafmt scalafmt = Scalafmt.create(scalafmtDynamic)
String formatted = scalafmt.format(config, file, "object A   { }")
```
