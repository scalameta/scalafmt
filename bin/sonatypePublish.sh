set -e
if [ $TRAVIS_SECURE_ENV_VARS = "true" ]; then
  echo "Publishing snapshot..."
  # Assert that nighly is set to snapshot
  grep "nightly.*SNAPSHOT" core/src/main/scala/org/scalafmt/Versions.scala
  sbt publishSigned
else
  echo "Skipping publish"
fi
