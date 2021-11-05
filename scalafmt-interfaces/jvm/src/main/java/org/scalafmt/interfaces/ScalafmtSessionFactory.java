package org.scalafmt.interfaces;

import java.nio.file.Path;

public interface ScalafmtSessionFactory {

    /**
     * Create a ScalafmtSession to format a batch of files using fixed configuration.
     * @param config location of the configuration file
     * @return a new session instance
     */
    ScalafmtSession createSession(Path config);

}
