package org.scalafmt.interfaces;

import java.nio.file.Path;

/**
 * A session based on a fixed configuration.
 */
public interface ScalafmtSession {

    /**
     * Format a single file with the given configuration.
     *
     * @param file relative or absolute path to the file being formatted. Used only for the path
     *             name, the file does not have to exist on disk.
     * @param code the text contents to format.
     * @return the formatted contents if formatting was successful, otherwise the original text
     * contents.
     */
    String format(Path file, String code);


    /**
     * Format a single file with the given configuration.
     *
     * @param file relative or absolute path to the file being formatted. Used only for the path
     *             name, the file does not have to exist on disk.
     * @param code the text contents to format.
     * @return the formatted contents if formatting was successful, otherwise an error.
     */
    ScalafmtResult formatOrError(Path file, String code);

    /**
     * Whether the path matches the 'project.{excludeFilters,includeFilters}' setting.
     * @param file path to match
     * @return true if the path matched the filters
     */
    boolean matchesProjectFilters(Path file);

    /**
     * Whether this configuration intends to limit files to those managed by git.
     */
    boolean isGitOnly();

    /**
     * Discover the files this configuration would format, the way the command-line
     * interface does: git-tracked files when the configuration enables git, otherwise a
     * recursive filesystem walk. The returned files already match the
     * 'project.{excludeFilters,includeFilters}' settings.
     *
     * @param root the directory that anchors discovery: the git working directory when
     *             the configuration enables git, and the walk root otherwise.
     * @param paths specific files or directories to search under; if none are given,
     *              {@code root} itself is searched.
     * @return the matching files, as absolute paths.
     * @throws UnsupportedOperationException if this implementation does not support discovery.
     */
    default java.util.List<java.nio.file.Path> listFiles(java.nio.file.Path root, java.nio.file.Path... paths) {
        throw new UnsupportedOperationException("listFiles is not implemented by this ScalafmtSession");
    }

}
