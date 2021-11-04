choco install llvm
echo "${env:ProgramFiles}\LLVM\bin" | Out-File -FilePath $env:GITHUB_PATH -Encoding utf8 -Append
clang --version