package org.scalafmt.interfaces;

public final class RepositoryCredential {
    public final String host;
    public final String username;
    public final String password;

    public RepositoryCredential(String host, String username, String password) {
        this.host = host;
        this.username = username;
        this.password = password;
    }

    public interface ScalafmtExtension {
        Scalafmt withRepositoryCredentials(RepositoryCredential... credentials);
    }

}
