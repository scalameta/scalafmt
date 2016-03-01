sbt "benchmarks/jmh:run -i 10 -wi 10 -f1 -t1 org.scalafmt.*"
sbt "core/test:runMain  org.scalafmt.FormatExperiment"

