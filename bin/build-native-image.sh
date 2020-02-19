set -eux
# NOTE(olafur): for some reason `jabba use ...` doesn't seem to work on GH Actions
export JAVA_HOME=$(jabba which --home graal-custom@20.0)
export PATH=$JAVA_HOME/bin:$PATH
echo $JAVA_HOME
which gu
gu install native-image
sbt cli/graalvm-native-image:packageBin
cp scalafmt-cli/target/graalvm-native-image/cli scalafmt
