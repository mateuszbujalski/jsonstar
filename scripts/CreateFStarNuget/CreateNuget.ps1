[CmdletBinding()]
param(
    # Path to nuget.exe
    [Parameter(Mandatory=$true)]
    [ValidateScript({ Test-Path -PathType Leaf -Path $_})]
    $NugetPath,

    # Path to the root of FStar repo
    [Parameter(Mandatory=$true)]
    $FStarPath,

    # Path to z3
    [Parameter(Mandatory=$true)]
    $z3Path,

    # Path to mingw
    [Parameter(Mandatory=$true)]
    $mingwPath,

    # Directory in which nuget should be created
    [Parameter(Mandatory=$true)]
    $outputDir,

    # Version of the nuget package to create
    [Parameter(Mandatory=$true)]
    $version,

    # FStar commit (only used as nuget metadata)
    $commit
)

function UpdateVersionInNuspec{
    [CmdletBinding()] param([string] $nuspecPath, [string] $version, [string] $commit)

    $xml = New-Object XML
    $xml.Load($nuspecPath)
    
    $element = $xml.SelectSingleNode("//package/metadata/version")
    $element.InnerText = $version

    $desc = $xml.SelectSingleNode("//package/metadata/summary")
    $desc.InnerText = $desc.InnerText.replace('$commit$',$commit)

    $xml.Save($nuspecPath)
}

$nuget_name = "FStar.Windows.Ocaml.Unofficial"

# We assume that $FStarPath contains built F* executable (Ocaml version) and ulib extracted to F# and compiled in the bin subdirectory:
# Inside cygwin terminal run `make -C src -j6 ocaml-fstar-ocaml` from $FStarPath to build ocaml version of F* (requires being able to build F* from sources on windows - check everest script)
# From $FStarPath run `src\VS\nuget-restore.bat`
# Open $FStarPath/src/VS/FStar.sln
# Change project dependencies for ulib and uncheck dependency on fstar project.
# Build ulib project

# We also assume that next to this script are following files:
# - $nuget_name.props
# - $nuget_name.nuspec

# Requires Powershell 3+
$scriptDir = $PSScriptRoot

$fstarexe_path = Join-Path -Path $FStarPath -ChildPath "bin\fstar.exe"
$ulib_path = Join-Path -Path $FStarPath -ChildPath "ulib"
$ulibfs_path = Join-Path -Path $FStarPath -ChildPath "bin\ulibfs.*" # we need to copy dll, pdb and xml

New-Item $outputDir -ItemType Directory -Force | Out-Null
$nuget_dir = Join-Path -Path $outputDir -ChildPath "fstar"
try
{
    Write-Host "Creating nuget structure"
    $nuget_dir_tools = Join-Path -Path $nuget_dir -ChildPath "tools"
    $nuget_dir_tools_bin = Join-Path -Path $nuget_dir -ChildPath "tools\bin"
    $nuget_dir_lib = Join-Path -Path $nuget_dir -ChildPath "lib"
    $nuget_dir_build = Join-Path -Path $nuget_dir -ChildPath "build"

    New-Item $nuget_dir -ItemType Directory -Force | Out-Null
    New-Item $nuget_dir_tools -ItemType Directory -Force | Out-Null
    New-Item $nuget_dir_tools_bin -ItemType Directory -Force | Out-Null
    New-Item $nuget_dir_lib -ItemType Directory -Force | Out-Null
    New-Item $nuget_dir_build -ItemType Directory -Force | Out-Null

    Write-Host "Copying files..."
    # Copy <FStar_repo_root>/bin/fstar.exe to tools/bin
    Copy-Item -Path $fstarexe_path -Destination $nuget_dir_tools_bin -Force
    # Copy <FStar_repo_root>/ulib to tools/ulib
    Copy-Item -Path $ulib_path -Destination "$nuget_dir_tools\ulib" -Recurse -Force
    # Copy <FStar_repo_root>/bin/ulib.[dll|pdb|xml] to lib
    Copy-Item -Path $ulibfs_path -Destination $nuget_dir_lib -Force
    # Copy *.dll from mingw/bin (your installation) into tools/bin
    Copy-Item -Path "$mingwPath\*.dll" -Destination $nuget_dir_tools_bin -Force
    # Copy *.* from z3-4.8.5-x64-win/bin into tools/bin
    Copy-Item -Path "$z3Path\bin\*.*" -Destination $nuget_dir_tools_bin -Force
    # Copy <FStar_repo_root>/fsharp.extraction.targets to targets and rename it to $nuget_name.targets
    # NOTE: If you need to work with sdk projects change TaskFactory="RoslynCodeTaskFactory" and AssemblyFile="$(MSBuildToolsPath\Microsoft.Build.Tasks.Code.dll"
    Copy-Item -Path "$FStarPath\fsharp.extraction.targets" -Destination "$nuget_dir_build\$nuget_name.targets" -Force
    Copy-Item -Path "$scriptDir\$nuget_name.props" -Destination $nuget_dir_build -Force
    # copy nuspec file and set version
    Copy-Item -Path "$scriptDir\$nuget_name.nuspec" -Destination $outputDir -Force
    # Bump version in the copy of nuspec 
    UpdateVersionInNuspec "$outputDir\$nuget_name.nuspec" $version $commit

    Write-Host "Packaging..."
    & $NugetPath @("pack", "$outputDir\$nuget_name.nuspec", "-OutputDirectory", $outputDir)

    Write-Host "Finished"
}
finally {
    Write-Host "Removing $nuget_dir"
    Remove-Item -Force -LiteralPath $nuget_dir -Recurse
    Remove-Item -Force -LiteralPath "$outputDir\$nuget_name.nuspec"
}



