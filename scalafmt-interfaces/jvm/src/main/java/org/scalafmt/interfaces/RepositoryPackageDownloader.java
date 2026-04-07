package org.scalafmt.interfaces;

import java.io.File;

public interface RepositoryPackageDownloader {

    /**
     * Download scalafmt dependencies from repository.
     *
     * @param scalaVersion    Scala version
     * @param scalafmtVersion Scalafmt version
     * @param reporter        to report errors
     * @param dependencies    the packages to download
     * @return locations of downloaded jars
     */
    Iterable<File> download(
            String scalaVersion,
            String scalafmtVersion,
            ScalafmtReporter reporter,
            Iterable<RepositoryPackage> dependencies);

}
