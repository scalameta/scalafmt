#!/usr/bin/env bash
set -eux

RAW_VERSION=${1:-2.6.4}
VERSION="v$RAW_VERSION"
INSTALL_LOCATION=${2:-/usr/local/bin/scalafmt-native}

CWD=$(pwd)

# from https://gist.github.com/Ariel-Rodriguez/9e3c2163f4644d7a389759b224bfe7f3
semver_compare() {
  local version_a version_b pr_a pr_b
  # strip word "v" and extract first subset version (x.y.z from x.y.z-foo.n)
  version_a=$(echo "${1//v/}" | awk -F'-' '{print $1}')
  version_b=$(echo "${2//v/}" | awk -F'-' '{print $1}')

  if [ "$version_a" \= "$version_b" ]
  then
    # check for pre-release
    # extract pre-release (-foo.n from x.y.z-foo.n)
    pr_a=$(echo "$1" | awk -F'-' '{print $2}')
    pr_b=$(echo "$2" | awk -F'-' '{print $2}')

    ####
    # Return 0 when A is equal to B
    [ "$pr_a" \= "$pr_b" ] && echo 0 && return 0

    ####
    # Return 1

    # Case when A is not pre-release
    if [ -z "$pr_a" ]
    then
      echo 1 && return 0
    fi

    ####
    # Case when pre-release A exists and is greater than B's pre-release

    # extract numbers -rc.x --> x
    local number_a number_b
    number_a=$(echo ${pr_a//[!0-9]/})
    number_b=$(echo ${pr_b//[!0-9]/})
    [ -z "${number_a}" ] && number_a=0
    [ -z "${number_b}" ] && number_b=0

    [ "$pr_a" \> "$pr_b" ] && [ -n "$pr_b" ] && [ "$number_a" -gt "$number_b" ] && echo 1 && return 0

    ####
    # Retrun -1 when A is lower than B
    echo -1 && return 0
  fi
  local arr_version_a arr_version_b cursor
  arr_version_a=(${version_a//./ })
  arr_version_b=(${version_b//./ })
  cursor=0
  # Iterate arrays from left to right and find the first difference
  while [ "$([ "${arr_version_a[$cursor]}" -eq "${arr_version_b[$cursor]}" ] && [ $cursor -lt ${#arr_version_a[@]} ] && echo true)" == true ]
  do
    cursor=$((cursor+1))
  done
  [ "${arr_version_a[$cursor]}" -gt "${arr_version_b[$cursor]}" ] && echo 1 || echo -1
}

NAME=scalafmt-linux
VERSION_COMPARE=$(semver_compare 2.7.0 "$RAW_VERSION")
if [ "$(uname)" == "Darwin" ]; then
  NAME=scalafmt-macos
elif [ "$VERSION_COMPARE" -le 0 ]; then
  NAME=scalafmt-linux-musl
fi

TMP=$(mktemp -d)
cd $TMP
if [ "$VERSION_COMPARE" -le 0 ]; then
curl --fail -Lo $NAME https://github.com/scalameta/scalafmt/releases/download/$VERSION/$NAME
cp $NAME $INSTALL_LOCATION
else
ZIP=$NAME.zip
curl --fail -Lo $ZIP https://github.com/scalameta/scalafmt/releases/download/$VERSION/$ZIP
unzip $ZIP
cp scalafmt $INSTALL_LOCATION
fi

chmod +x $INSTALL_LOCATION

cd $CWD
rm -rf $TMP

echo Installed $INSTALL_LOCATION

