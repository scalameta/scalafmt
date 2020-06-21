set -eux
# NOTE(olafur): for some reason `jabba use ...` doesn't seem to work on GH Actions
which gu
gu install native-image
sbt cli/graalvm-native-image:packageBin
cp scalafmt-cli/target/graalvm-native-image/cli scalafmt
