package org.scalafmt.interfaces;

public interface RepositoryProperties {

    /**
     *
     * @return custom repositories, if any
     */
    Iterable<String> getRepositories();

    /**
     *
     * @return custom credentials, if any
     */
    Iterable<RepositoryCredential> getCredentials();

}
