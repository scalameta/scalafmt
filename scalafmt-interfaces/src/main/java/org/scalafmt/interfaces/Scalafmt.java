package org.scalafmt.interfaces;

import java.io.Writer;
import java.nio.file.Path;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.ServiceLoader;

/**
 * A stable public interface to run Scalafmt.
 *
 * This interface is designed for integrations such as editor plugins and build tools.
 * An implementation of this interface is available in the module 'scalafmt-dynamic'.
 *
 * It is recommended to use this interface over the org.scalafmt.Scalafmt object
 * for several reasons:
 *
 * - this API is guaranteed to be binary compatible with all future versions of Scalafmt.
 * - it downloads the Scalafmt version matching the 'version' setting in .scalafmt.conf.
 *   All versions down to v1.2.0 are supported.
 * - it respects the 'project.{excludeFilters,includeFilters}' setting in .scalafmt.conf.
 * - it uses the correct parser for `*.sbt` and `*.sc` files.
 * - it automatically caches parsing of configuration files avoiding redundant work when possible.
 * - it has two external library dependencies (com.geirsson:coursier-small and com.typesafe:config),
 *   which is a smaller dependency footprint compared to com.geirsson:scalafmt-core.
 */
public interface Scalafmt {

    /**
     * Format a single file with the given .scalafmt.conf configuration.
     *
     * @param config the absolute path to the configuration file. This file must exist or
     *               an exception will be thrown.
     * @param file relative or absolute path to the file being formatted. Used only for the path
     *             name, the file does not have to exist on disk.
     * @param code the text contents to format.
     * @return the formatted contents if formatting was successful, otherwise the original text
     * contents.
     */
    String format(Path config, Path file, String code);


    /**
     * Whether to respect the 'project.{excludeFilters,includeFilters}' setting in .scalafmt.conf.
     *
     * @param respectExcludeFilters If true (default), returns the original file contents when
     *                              formatting a file that does not matches the project settings
     *                              in .scalafmt.conf. If false, formats every file that is passed
     *                              to the {@link #format(Path, Path, String)} method regardless
     *                              of .scalafmt.conf settings.
     */
    Scalafmt withRespectProjectFilters(boolean respectExcludeFilters);

    /**
     * Whether to respect the 'version' setting in .scalafmt.conf.
     *
     * @param respectVersion If true (default), refuses to format files when the 'version' setting
     *                       is missing in .scalafmt.conf and users must update .scalafmt.conf to
     *                       format files. If false, falls back to the default version provided
     *                       via {@link #withDefaultVersion(String)}.
     *
     */
    Scalafmt withRespectVersion(boolean respectVersion);

    /**
     * The fallback Scalafmt version to use when there is no 'version' setting in .scalafmt.conf.
     *
     * The default version is ignored when {@link #withRespectVersion(boolean)} is true.
     */
    Scalafmt withDefaultVersion(String defaultVersion);

    /**
     * Use this reporter to report errors and information messages.
     */
    Scalafmt withReporter(ScalafmtReporter reporter);

    /**
     * Use this writer for printing download progress, defaults to System.err.
     */
    Scalafmt withDownloadWriter(Writer writer);

    /**
     * Clear internal caches such as classloaded Scalafmt instances.
     */
    void clear();

    static Scalafmt create() {
        return create(Thread.currentThread().getClass().getClassLoader());
    }

    /**
     * Classload a new instance of this Scalafmt instance.
     *
     * @param loader the classloader containing the 'scalafmt-dynamic' module.
     *
     * @throws NoSuchElementException if the classloader does not have the 'scalafmt-dynamic' module.
     */
    static Scalafmt create(ClassLoader loader) {
        Iterator<Scalafmt> builders = ServiceLoader.load(Scalafmt.class, loader).iterator();
        if (builders.hasNext()) {
            return builders.next();
        }
        throw new NoSuchElementException(
                Scalafmt.class.getName() +
                        ". To fix this problem, make sure that the provided classloader contains the 'scalafmt-dynamic' module."
        );
    }

}