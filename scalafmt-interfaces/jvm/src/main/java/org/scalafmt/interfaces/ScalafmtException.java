package org.scalafmt.interfaces;

public class ScalafmtException extends RuntimeException {

    public ScalafmtException(String message, Throwable cause) {
        super(message, cause, true, false);
    }

}
