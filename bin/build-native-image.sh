set -eux
export JAVA_HOME=$(jabba which --home graal-custom@19.3)
export PATH=$JAVA_HOME/bin:$PATH
echo $JAVA_HOME
which gu
gu install native-image
sbt cli/graalvm-native-image:packageBin