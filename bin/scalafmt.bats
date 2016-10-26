#!/usr/bin/env bats

current_version="v0.4.8" # $(curl -s https://api.github.com/repos/olafurpg/scalafmt/releases/latest | grep -Eo '"tag_name":(.*)' | grep -Eo 'v[0-9\.]+')
#current_version="v0.4.2"
test_version="v0.4.1"
test_dir="./jars"

@test "Runs with latest version" {
  run ./scalafmt_auto -v
  [[ $status -eq 0 ]]
  run ./scalafmt_auto -v
  [[ "$output" = "scalafmt ${current_version:1}" ]]
}

@test "Correct directory created" {
  [[ -d "$HOME/.scalafmt-bin/" ]]
}

@test "Correct jar for latest version" {
   [[ -e "$HOME/.scalafmt-bin/releases/$current_version/scalafmt-$current_version.jar" ]]
}

@test "Correct version file" {
  [[ -e "$HOME/.scalafmt-bin/version" ]]
  [[ $(cat "$HOME/.scalafmt-bin/version") = "$current_version" ]]
}

@test "Run with another version" {
  run ./scalafmt_auto --version ${test_version:1} -v
  [[ $status -eq 0 ]]
  result=$(./scalafmt_auto --version ${test_version:1} -v | tail -1)
  [[ $result = "scalafmt ${test_version:1}" ]]
}

@test "Correct jar for test version" {
   [[ -e "$HOME/.scalafmt-bin/releases/$test_version/scalafmt-$test_version.jar" ]]
}

@test "Can't use both upgrade and version" {
  run ./scalafmt_auto --version ${test_version:1} --upgrade -v
  [[ $status -eq 1 ]]
  [[ $output = "You can't specify a custom version with --upgrade" ]]
  run ./scalafmt_auto --upgrade --version ${test_version:1} -v
  [[ $status -eq 1 ]]
  [[ $output = "You can't specify a custom version with --upgrade" ]]
}

@test "Use --dir" {
    run ./scalafmt_auto --dir $test_dir -v
    [[ -d $test_dir ]]
    [[ $status -eq 0 ]]
    [[ -e "$test_dir/scalafmt-$current_version.jar" ]]
    result=$(./scalafmt_auto --dir $test_dir -v | tail -1)
    [[ "$result" = "scalafmt ${current_version:1}" ]]
}

@test "Use --dir and --upgrade" {
  run ./scalafmt_auto --upgrade --dir $test_dir -v
  [[ -d $test_dir ]]
  [[ $status -eq 0 ]]
  [[ -e "$test_dir/scalafmt-$current_version.jar" ]]
  result=$(./scalafmt_auto --dir $test_dir -v | tail -1)
  [[ "$result" = "scalafmt ${current_version:1}" ]]
}

@test "Use --dir and --version" {
  run ./scalafmt_auto --dir $test_dir --version ${test_version:1} -v
  [[ -d $test_dir ]]
  [[ $status -eq 0 ]]
  [[ -e "$test_dir/scalafmt-$test_version.jar" ]]
  result=$(./scalafmt_auto --dir $test_dir --version ${test_version:1} -v | tail -1)
  [[ "$result" = "scalafmt ${test_version:1}" ]]
}
