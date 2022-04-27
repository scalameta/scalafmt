package org.scalafmt.interfaces;

import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.nio.file.Path;

/**
 * A reporter to handle error and information messages from Scalafmt.
 */
public interface ScalafmtReporter {

    /**
     * An error occurred while trying to process this file.
     *
     * @param file can be either a Scala source file or .scalafmt.conf.
     * @param message the error message.
     */
    void error(Path file, String message);

    /**
     * An exception occurred while trying to process this file.
     *
     * @param file can be either a Scala source file or .scalafmt.conf.
     * @param e the exception that occurred, has type {@link PositionException}
     *          when the error appeared as a position.
     */
    void error(Path file, Throwable e);

    /**
     * An exception occurred while trying to process this file.
     *
     * @param file can be either a Scala source file or .scalafmt.conf.
     * @param message additional error message
     * @param e the exception that occurred, has type {@link PositionException}
     *          when the error appeared as a position.
     */
    default void error(Path file, String message, Throwable e) {
        if (e == null) error(file, message);
        else error(file, new ScalafmtException(message, e));
    }

    /**
     * This file was not formatted because it's excluded by project settings from .scalafmt.conf.
     *
     * @param file the file path that was not formatted.
     */
    void excluded(Path file);

    /**
     * This .scalafmt.conf file is missing the 'version' setting.
     *
     * @param config the .scalafmt.conf file.
     * @param defaultVersion the configured default Scalafmt version.
     */
    default void missingVersion(Path config, String defaultVersion) {
        String message = String.format(
            "missing setting 'version'. To fix this problem, add the following line to .scalafmt.conf: 'version=%s'.",
            defaultVersion
        );
        error(config, message);
    }

    /**
     * Record the configuration file and version used
     *
     * @param config location of the configuration file parsed
     * @param scalafmtVersion the version of scalafmt used to parse
     */
    void parsedConfig(Path config, String scalafmtVersion);

    /**
     * Use {@link #downloadOutputStreamWriter} instead.
     *
     * @return an instance of progress writer
     */
    @Deprecated PrintWriter downloadWriter();

    /**
     * Use this writer for printing progress while downloading new Scalafmt versions.
     *
     * @return an instance of progress writer
     */
    OutputStreamWriter downloadOutputStreamWriter();

}