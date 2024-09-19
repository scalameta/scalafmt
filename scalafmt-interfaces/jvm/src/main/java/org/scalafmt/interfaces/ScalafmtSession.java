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

}
