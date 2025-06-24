# Keyman is copyright (C) SIL Global. MIT License.

$tier = Get-Content ..\TIER.md
$build_number = Get-Content ..\VERSION.md
$build_counter = ${build_number} -replace "^\d+\.\d+\.(\d+)$", '$1'

$upload_path = "upload\${build_number}"

$keyman_engine_android_zip = "keyman-engine-android-${build_number}.zip"
$keyman_apk = "keyman-${build_number}.apk"
$firstvoices_apk = "firstvoices-${build_number}.apk"

$7Z_HOME = $env:7Z_HOME
$RSYNC_HOME = $env:RSYNC_HOME
$RSYNC_PATH = $env:RSYNC_PATH
$RSYNC_USER = $env:RSYNC_USER
$RSYNC_HOST = $env:RSYNC_HOST
$RSYNC_ROOT = $env:RSYNC_ROOT
$USERPROFILE = $env:USERPROFILE

#
# Preparation
#

if((Test-Path ${upload_path}) -ne 0) {
  Remove-Item -Path ${upload_path} -Recurse
}

mkdir ${upload_path}

#
# Build Keyman Engine Android archive
#

cd KMAPro\kMAPro\libs
& "$7Z_HOME\7z.exe" a -bd -bb0 ..\..\..\${upload_path}\${keyman_engine_android_zip} keyman-engine.aar ..\..\..\Samples '-xr!build.sh'
cd ..\..\..

#
# Copy APKs
#
copy KMAPro\kMAPro\build\outputs\apk\release\${keyman_apk} ${upload_path}\${keyman_apk}
if (Test-Path -Path ..\oem\firstvoices\android\app\build\outputs\apk\release) {
  copy ..\oem\firstvoices\android\app\build\outputs\apk\release\${firstvoices_apk} ${upload_path}\${firstvoices_apk}
}

#
# Construct keyman_engine_android.download_info
#

$hash = get-filehash ${upload_path}\${keyman_engine_android_zip} -Algorithm MD5
$size = (Get-Item ${upload_path}\${keyman_engine_android_zip}).length

$keyman_engine_android_download_info = @"
{
  "name": "Keyman Engine for Android",
  "version": "${build_number}",
  "date": "$([DateTime]::Now.ToString("yyyy-MM-dd"))",
  "platform": "android",
  "stability": "${tier}",
  "file": "keyman-engine-android-${build_number}.zip",
  "md5": "$($hash.Hash)",
  "type": "zip",
  "build": "${build_counter}",
  "size": "$size"
}
"@

[System.IO.File]::WriteAllLines("$pwd\${upload_path}\${keyman_engine_android_zip}.download_info", $keyman_engine_android_download_info)

#
# Construct {keyman_apk}.download_info
#

$hash = get-filehash ${upload_path}\${keyman_apk} -Algorithm MD5
$size = (Get-Item ${upload_path}\${keyman_apk}).length

$keyman_download_info = @"
{
  "name": "Keyman for Android",
  "version": "${build_number}",
  "date": "$([DateTime]::Now.ToString("yyyy-MM-dd"))",
  "platform": "android",
  "stability": "${tier}",
  "file": "keyman-${build_number}.apk",
  "md5": "$($hash.Hash)",
  "type": "apk",
  "build": "${build_counter}",
  "size": "$size"
}
"@

[System.IO.File]::WriteAllLines("$pwd\${upload_path}\${keyman_apk}.download_info", $keyman_download_info)

if (Test-Path -Path ..\oem\firstvoices\android\app\build\outputs\apk\release) {
  #
  # Construct {firstvoices_apk}.download_info
  #

  $hash = get-filehash ${upload_path}\${firstvoices_apk} -Algorithm MD5
  $size = (Get-Item ${upload_path}\${firstvoices_apk}).length

  $firstvoices_download_info = @"
{
  "name": "Firstvoices Keyboards",
  "version": "${build_number}",
  "date": "$([DateTime]::Now.ToString("yyyy-MM-dd"))",
  "platform": "android",
  "stability": "${tier}",
  "file": "firstvoices-${build_number}.apk",
  "md5": "$($hash.Hash)",
  "type": "apk",
  "build": "${build_counter}",
  "size": "$size"
}
"@

  [System.IO.File]::WriteAllLines("$pwd\${upload_path}\${firstvoices_apk}.download_info", $firstvoices_download_info)
}

#
# Upload with rsync to downloads.keyman.com
# (rsync requires that we are in the upload folder to get folders in
# sync correctly; it is possible to resolve this but easier to just cd.)
#

$rsync_args = @(
  '-vrzltp',                                # verbose, recurse, zip, copy symlinks, preserve times, permissions
  '--chmod=Dug=rwx,Do=rx,Fug=rw,Fo=r',      # map Windows security to host security
  '--stats',                                # show statistics for log
  "--rsync-path=""${RSYNC_PATH}""",         # path on remote server
  "--rsh=${RSYNC_HOME}\ssh -i ${USERPROFILE}\.ssh\id_rsa -o UserKnownHostsFile=${USERPROFILE}\.ssh\known_hosts",                  # use ssh
  "${build_number}",                          # upload the whole build folder
  "${RSYNC_USER}@${RSYNC_HOST}:${RSYNC_ROOT}/developer/${tier}/" # target server + path
)

# Write-Output "rsync parameters:" $rsync_args

cd upload
& ${RSYNC_HOME}\rsync.exe $rsync_args
if ($LASTEXITCODE -ne 0) { throw "Exit code is $LASTEXITCODE" }
cd ..

# EOF
