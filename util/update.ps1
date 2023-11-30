param (
  [string]$Compiler = "ifx",
  [string]$Build = ".\build",
  [string]$Util = ".\util",
  [string]$Executable = "update.exe"
)

New-Item -ItemType Directory -Force -Path "$Build" | out-null
Get-ChildItem .\data\ > "$Build\required_datasets.txt"
Get-ChildItem .\src\problems\ > "$Build\solved_problems.txt"
$flags = "/nologo /object:$Build/ /exe:$Build\$Executable"
Invoke-Expression "$Compiler $Util\preprocess.f90 $flags"
Invoke-Expression "$Build\$Executable"
