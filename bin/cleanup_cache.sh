set -eu
find /drone/.sbt -name "*.lock" -type f -delete
find /drone/.ivy2/cache -name "ivydata-*.properties" -type f -delete
rm -rf /drone/.ivy2/local
