package org.scalafmt.interfaces;

import java.nio.file.Path;

/**
 * An exception that happened at a position in a source file such as a parse error.
 */
abstract public class PositionException extends Exception {
    public PositionException(String message, Throwable cause) {
        super(message, cause);
    }

    @Override
    public synchronized Throwable fillInStackTrace() {
        return this;
    }

    /**
     * @return The file where the error occurred.
     */
    public abstract Path file();

    /**
     * @return The text contents of the file being formatted.
     */
    public abstract String code();

    /**
     * @return The fully formatted error message including line content and caret.
     */
    public abstract String longMessage();

    /**
     * @return Only the error message itself without line content and caret.
     */
    public abstract String shortMessage();

    public abstract int startLine();
    public abstract int startCharacter();
    public abstract int endLine();
    public abstract int endCharacter();
}
