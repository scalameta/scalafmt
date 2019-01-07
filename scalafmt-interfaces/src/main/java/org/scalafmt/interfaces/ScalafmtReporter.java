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
     * An unexpected exception occurred while trying to process this file.
     *
     * @param file can be either a Scala source file or .scalafmt.conf.
     */
    void error(Path file, Throwable e);

    /**
     * This file was not formatted because it's excluded by project settings from .scalafmt.conf.
     *
     * @param file the file path that was not formatted.
     */
    void excluded(Path file);

    /**
     * The .scalafmt.conf file was parsed with the given Scalafmt version.
     */
    void parsedConfig(Path config, String scalafmtVersion);

    /**
     * Use this writer for printing progress while downloading new Scalafmt versions.
     */
    PrintWriter downloadWriter();

}