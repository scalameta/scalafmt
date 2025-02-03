package org.scalafmt.interfaces

import java.nio.file.Path
import java.util
import java.util.NoSuchElementException
import java.util.ServiceLoader

/** A stable public interface to run Scalafmt.
  *
  * This interface is designed for integrations such as editor plugins and build
  * tools. An implementation of this interface is available in the module
  * 'scalafmt-dynamic'.
  *
  * It is recommended to use this interface over the org.scalafmt.Scalafmt
  * object for several reasons:
  *
  *   - this API is guaranteed to be binary compatible with all future versions
  *     of Scalafmt.
  *   - it downloads the Scalafmt version matching the 'version' setting in
  *     .scalafmt.conf. All versions down to v1.2.0 are supported.
  *   - it respects the 'project.{excludeFilters,includeFilters}' setting in
  *     .scalafmt.conf.
  *   - it uses the correct parser for `*.sbt` and `*.sc` files.
  *   - it automatically caches parsing of configuration files avoiding
  *     redundant work when possible.
  *   - it has two external library dependencies (com.geirsson:coursier-small
  *     and com.typesafe:config), which is a smaller dependency footprint
  *     compared to scalafmt-core.
  */
trait Scalafmt {

  /** Format a single file with the given .scalafmt.conf configuration.
    *
    * @param config
    *   the absolute path to the configuration file. This file must exist or an
    *   exception will be thrown.
    * @param file
    *   relative or absolute path to the file being formatted. Used only for the
    *   path name, the file does not have to exist on disk.
    * @param code
    *   the text contents to format.
    * @return
    *   the formatted contents if formatting was successful, otherwise the
    *   original text contents.
    */
  def format(config: Path, file: Path, code: String): String

  /** Builder method.
    *
    * @param respectExcludeFilters
    *   If true (default), returns the original file contents when formatting a
    *   file that does not matches the project settings in .scalafmt.conf. If
    *   false, formats every file that is passed to the
    *   {@link #format(Path, Path, String)} method regardless of .scalafmt.conf
    *   settings.
    * @return
    *   an updated interface instance controlling whether to respect the
    *   'project.{excludeFilters,includeFilters}' setting in .scalafmt.conf.
    */
  def withRespectProjectFilters(respectExcludeFilters: Boolean): Scalafmt

  /** Builder method.
    *
    * @param respectVersion
    *   If true (default), refuses to format files when the 'version' setting is
    *   missing in .scalafmt.conf and users must update .scalafmt.conf to format
    *   files. If false, falls back to the default version provided via
    *   {@link #withDefaultVersion(String)} .
    * @return
    *   an updated interface instance controlling whether to respect the
    *   'version' setting in .scalafmt.conf.
    */
  @Deprecated
  def withRespectVersion(respectVersion: Boolean): Scalafmt

  /** Builder method.
    *
    * @param defaultVersion
    *   the fallback Scalafmt version to use when there is no 'version' setting
    *   in `.scalafmt.conf`; N.B. ignored when
    *   {@link #withRespectVersion(boolean)} is true
    * @return
    *   an updated interface instance with the default version set
    */
  @Deprecated
  def withDefaultVersion(defaultVersion: String): Scalafmt

  /** Builder method.
    *
    * @param reporter
    *   Use this instance to report errors and information messages
    * @return
    *   an updated interface instance with the reporter instance set
    */
  def withReporter(reporter: ScalafmtReporter): Scalafmt

  /** Builder method.
    *
    * @param repositories
    *   maven repositories to use when resolving
    * @return
    *   an updated interface instance with the repositories to use to resolve
    *   dependencies.
    */
  def withMavenRepositories(repositories: String*): Scalafmt

  /** Builder method.
    *
    * @param credentials
    *   repository credentials to use when resolving
    * @return
    *   an updated interface instance.
    */
  def withRepositoryCredentials(credentials: RepositoryCredential*): Scalafmt

  /** Clear internal caches such as classloaded Scalafmt instances.
    */
  def clear(): Unit

  /** Create a ScalafmtSession to format a batch of files using fixed
    * configuration.
    * @param config
    *   location of the configuration file
    * @return
    *   a new session instance
    */
  def createSession(config: Path): ScalafmtSession
}

object Scalafmt {

  /** Classload a new instance of this Scalafmt instance.
    *
    * @param loader
    *   the classloader containing the 'scalafmt-dynamic' module.
    * @return
    *   a new instance of this interface
    * @throws NoSuchElementException
    *   if the classloader does not have the 'scalafmt-dynamic' module.
    */
  def create(loader: ClassLoader): Scalafmt = throw new ScalafmtException(
    "Can't use different version for native CLI",
    null,
  )
}
