package org.scalafmt.interfaces;

public final class ScalafmtResult {
    public final String value;
    public final Throwable exception;

    public ScalafmtResult(String value) {
        this.value = value;
        this.exception = null;
    }

    public ScalafmtResult(Throwable exception) {
        this.exception = exception;
        this.value = null;
    }
}
