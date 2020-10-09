# This script will update build version in *.nuspec files.
# It's meant to run as part of the workflow when merging changes to master
# before publishing new nugets.

[String]$repoRoot = Join-Path -Path $PSScriptRoot -ChildPath "../" -Resolve
[String]$path = Join-Path -Path $repoRoot -ChildPath "jsonstar/jsonstar.nuspec"

[xml]$xmlDoc = New-Object xml
$xmlDoc.load($path)
$versionNode = $xmlDoc.SelectSingleNode("/package/metadata/version")
$version = New-Object System.Version($versionNode.InnerText)
$updatedVersion = New-Object System.Version($version.Major, $version.Minor, ($version.Build+1))
$versionNode.InnerText = $updatedVersion.ToString(3)
$xmlDoc.Save($path)

