package org.scalafmt.interfaces;

import java.nio.file.Path;

public interface ScalafmtSessionFactory {

    /**
     * Create a ScalafmtSession to format a batch of files using fixed configuration.
     */
    ScalafmtSession createSession(Path config);

}
