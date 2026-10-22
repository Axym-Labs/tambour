# Packs a commit of Marey into the local feed that nuget.config names; see marey-feed.sh.
#
#   tools/marey-feed.ps1 [-Marey <checkout>] [-Ref <ref>]
param(
    [string]$Marey = $(if ($env:MAREY_DIR) { $env:MAREY_DIR } else { Join-Path $PSScriptRoot '../../marey' }),
    [string]$Ref = ''
)
$ErrorActionPreference = 'Stop'

$here = Resolve-Path (Join-Path $PSScriptRoot '..')
$feed = Join-Path $here 'artifacts/marey-feed'
$props = Get-Content (Join-Path $here 'Directory.Packages.props') -Raw
$pinnedCommit = [regex]::Match($props, '<MareyCommit>(.*)</MareyCommit>').Groups[1].Value
$pinnedVersion = [regex]::Match($props, '<MareyVersion>(.*)</MareyVersion>').Groups[1].Value

$target = if ($Ref) { $Ref } else { $pinnedCommit }
$sha = (git -C $Marey rev-parse --verify "$target^{commit}").Trim()
if ($LASTEXITCODE) { throw "no commit $target in $Marey" }
$version = "1.0.0-dev.$((git -C $Marey rev-list --count $sha).Trim())"

if (-not $Ref -and $version -ne $pinnedVersion) {
    throw "$sha packs as $version, but Directory.Packages.props pins $pinnedVersion (a shallow clone counts too few commits)"
}

if (Test-Path (Join-Path $feed "Marey.$version.nupkg")) {
    Write-Host "Marey $version is already in $feed"
} else {
    $work = Join-Path ([IO.Path]::GetTempPath()) ([IO.Path]::GetRandomFileName())
    git -C $Marey worktree add --quiet --detach $work $sha
    if ($LASTEXITCODE) { throw 'git worktree add failed' }
    try {
        dotnet build (Join-Path $work 'Marey.slnx') -c Release -p:Version=$version -nologo -v quiet
        if ($LASTEXITCODE) { throw 'build failed' }
        dotnet pack (Join-Path $work 'Marey.slnx') -c Release --no-build -p:Version=$version -o $feed -nologo -v quiet
        if ($LASTEXITCODE) { throw 'pack failed' }
    } finally {
        git -C $Marey worktree remove --force $work
    }
    Write-Host "packed Marey $version ($sha) into $feed"
}

if ($Ref) {
    Write-Host ''
    Write-Host 'Pin it in Directory.Packages.props:'
    Write-Host "    <MareyVersion>$version</MareyVersion>"
    Write-Host "    <MareyCommit>$sha</MareyCommit>"
}
