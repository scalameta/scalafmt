set -eu
test -d /drone/.sbt && find /drone/.sbt -name "*.lock" -type f -delete
test -d /drone/.ivy/cache && find /drone/.ivy2/cache -name "ivydata-*.properties" -type f -delete
rm -rf /drone/.ivy2/local
