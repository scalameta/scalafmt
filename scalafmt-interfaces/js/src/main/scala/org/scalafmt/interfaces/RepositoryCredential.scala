package org.scalafmt.interfaces

final class RepositoryCredential(
    val host: String,
    val username: String,
    val password: String,
)

object RepositoryCredential {
  trait ScalafmtExtension {
    def withRepositoryCredentials(credentials: RepositoryCredential*): Scalafmt
  }
}
