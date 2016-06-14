prefix=$1
iterations=$2
sbt "benchmarks/jmh:run -rf csv -rff target/jmh-results.csv -i $iterations -wi $iterations -f1 -t1 org.scalafmt.benchmarks.$prefix*"
# sbt "core/test:runMain  org.scalafmt.FormatExperiment"

