# sbt cli/assembly
# I can't reproduce this error in sbt, see https://github.com/olafurpg/scalafmt/issues/492
set -e
echo "Issue 492"
in_file=bin/issue492.scala
out_file=target/issue492-formatted.scala
sbt cli/assembly
java -jar cli/target/scala-2.11/scalafmt.jar -f $in_file > $out_file
diff --ignore-blank-lines $in_file $out_file

