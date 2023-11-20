param (
  [string]$Compiler = "gfortran",
  [string]$Build = ".\build",
  [string]$Util = ".\util",
  [string]$Executable = "update.exe"
)

New-Item -ItemType Directory -Force -Path "$Build" | out-null
Get-ChildItem .\data\ > "$Build\required_datasets.txt"
Get-ChildItem .\src\problems\ > "$Build\solved_problems.txt"
Invoke-Expression "$Compiler $Util\preprocess.f90 -o $Build\$Executable"
Invoke-Expression $Build\$Executable
