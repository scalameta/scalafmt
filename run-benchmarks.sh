if [[ ! -f benchmarks/repos.tar.gz ]]; then
  if [[ ! -f repos.tar.gz ]]; then 
    echo "Missing repos.tar.gz, run first sbt test"
    exit 1
  fi
  echo "Extracting repos.tar.gz..."
  cp repos.tar.gz benchmarks
  cd benchmarks
  tar xvf repos.tar.gz &> /dev/null
  cd ..
  echo "Done!"
fi
prefix=${1-Micro}
iterations=${2-3}
sbt "benchmarks/jmh:run -rf csv -rff target/jmh-results.csv -i $iterations -wi $iterations -f1 -t1 org.scalafmt.benchmarks.$prefix*"
# sbt "core/test:runMain  org.scalafmt.FormatExperiment"

