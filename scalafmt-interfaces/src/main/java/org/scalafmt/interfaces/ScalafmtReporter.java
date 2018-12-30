package org.scalafmt.interfaces;

import java.nio.file.Path;

public interface ScalafmtReporter {

    void excluded(String filename);

    void parsedConfig(Path config);

    void error(Throwable e);

    void error(String message);

    void error(Path path, String message);

}