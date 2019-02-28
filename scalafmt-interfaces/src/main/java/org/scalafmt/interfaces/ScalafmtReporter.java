package org.scalafmt.interfaces;

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
        error(file, new RuntimeException(message, e));
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
     * The .scalafmt.conf file was parsed with the given Scalafmt version.
     */
    void parsedConfig(Path config, String scalafmtVersion);

    /**
     * Use this writer for printing progress while downloading new Scalafmt versions.
     */
    PrintWriter downloadWriter();

}