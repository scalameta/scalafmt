set -e
if [ $TRAVIS_SECURE_ENV_VARS = "true" ]; then
  echo "Publishing snapshot..."
  # Assert that nighly is set to snapshot
  grep "nightly.*SNAPSHOT" core/target/scala-2.11/src_managed/main/sbt-buildinfo/Versions.scala
  sbt publishSigned
else
  echo "Skipping publish"
fi
