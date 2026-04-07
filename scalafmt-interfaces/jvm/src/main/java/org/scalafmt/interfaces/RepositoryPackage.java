package org.scalafmt.interfaces;

public final class RepositoryPackage {
    public final String group;
    public final String artifact;
    public final String version;

    public RepositoryPackage(String group, String artifact, String version) {
        this.group = group;
        this.artifact = artifact;
        this.version = version;
    }
}
