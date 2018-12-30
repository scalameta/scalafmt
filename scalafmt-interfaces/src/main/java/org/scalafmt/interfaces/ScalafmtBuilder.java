package org.scalafmt.interfaces;

import java.nio.file.Path;

public interface ScalafmtBuilder {

    ScalafmtBuilder withRespectExcludeFilters(boolean respectExcludeFilters);

    ScalafmtBuilder withConfig(Path config);

    ScalafmtBuilder withReporter(ScalafmtReporter reporter);

    Scalafmt create();

}