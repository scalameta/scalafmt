package org.scalafmt.interfaces;

public interface RepositoryPackageDownloaderFactory {

    /**
     * Construct a downloader instance
     */
    RepositoryPackageDownloader create(
            ScalafmtReporter reporter,
            RepositoryProperties properties);

    interface ScalafmtExtension {
        Scalafmt withRepositoryPackageDownloaderFactory(RepositoryPackageDownloaderFactory factory);
    }

}
