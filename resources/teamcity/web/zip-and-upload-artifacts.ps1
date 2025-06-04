# Keyman is copyright (C) SIL Global. MIT License.
#
# This script should be the identical for nightly/beta/stable for a given platform
#
$ErrorActionPreference = "Stop";

$tier = Get-Content ..\TIER.md
$build_number = Get-Content ..\VERSION.md
$build_counter = $build_number -replace "^\d+\.\d+\.(\d+)$", '$1'

$upload_path = "build\upload\$build_number"
$zip = "$upload_path\keymanweb-$build_number.zip"

$7Z_HOME = $env:7Z_HOME
$RSYNC_HOME = $env:RSYNC_HOME
$USERPROFILE = $env:USERPROFILE

# Since shell-scripting doesn't like number-initial variables, we convert it to a friendlier name.
$env:SEVEN_Z_HOME=$7Z_HOME

# PowerShell fun: `--` parameters will break a PowerShell command when not escaped in some form.
# We can build the command in string form first and then execute the string, fortunately.
$cmd = '"C:\Program Files\Git\bin\bash.exe" --init-file "c:\Program Files\Git\etc\profile" -l ./ci.sh prepare:downloads.keyman.com'
cmd /c $cmd

$hash = get-filehash $zip -Algorithm MD5

#
# Construct .build_info
#

$download_info = @"
{
  "name": "KeymanWeb",
  "version": "$build_number",
  "date": "$([DateTime]::Now.ToString("yyyy-MM-dd"))",
  "platform": "web",
  "stability": "$tier",
  "file": "keymanweb-$build_number.zip",
  "md5": "$($hash.Hash)",
  "type": "zip",
  "build": "$build_counter"
}
"@

# WriteAllLines is needed to avoid BOM
[System.IO.File]::WriteAllLines("$pwd\$upload_path\keymanweb-$build_number.zip.download_info", $download_info)
# $download_info | Out-File $upload_path\keymanweb-$build_number.zip.download_info -Encoding utf8

#
# Upload with rsync to downloads.keyman.com
# (rsync requires that we are in the upload folder to get folders in
# sync correctly; it is possible to resolve this but easier to just cd.)
#

$rsync_args = @(
  '-vrzltp',                                # verbose, recurse, zip, copy symlinks, preserve times, permissions
  '--chmod=Dug=rwx,Do=rx,Fug=rw,Fo=r',      # map Windows security to host security
  '--stats',                                # show statistics for log
  '--rsync-path="%downloads_rsync_path%"',    # path on remote server
  "--rsh=$RSYNC_HOME\ssh -i $USERPROFILE\.ssh\id_rsa -o UserKnownHostsFile=$USERPROFILE\.ssh\known_hosts",                  # use ssh
  "$build_number",                          # upload the whole build folder
  "%downloads_rsync_user%@%downloads_rsync_host%:%downloads_rsync_root%/web/$tier/" # target server + path
)

# Write-Output "rsync parameters:" $rsync_args

cd "$upload_path\.."
& $RSYNC_HOME\rsync.exe $rsync_args
if ($LASTEXITCODE -ne 0) { throw "Exit code is $LASTEXITCODE" }
cd ..

# EOF
