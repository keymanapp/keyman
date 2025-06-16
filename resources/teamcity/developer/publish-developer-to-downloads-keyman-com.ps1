# Keyman is copyright (C) SIL Global. MIT License.

$ErrorActionPreference = "Stop"

$tier = Get-Content ..\TIER.md
$build_number = Get-Content ..\VERSION.md
$build_counter = ${build_number} -replace "^\d+\.\d+\.(\d+)$", '$1'

if ( ${tier} -eq "alpha" -or ${tier} -eq "beta") {
  # $version_tag = ${tier}
  $version_with_tag = ${build_number}  # we may go with ${build_number} + "-" + ${version_tag} in the future one day
} else {
  $version_with_tag = ${build_number}
}

$upload_path = "upload\${build_number}"
$keyboards_path = "${upload_path}\keyboards"

# Keyman Developer installers
$kmcomp_zip = "kmcomp-${version_with_tag}.zip"
$keyman_developer_exe = "keymandeveloper-${version_with_tag}.exe"

# Debug files
$debug_zip = "debug-${build_number}.zip"

$7Z_HOME = $env:7Z_HOME
$RSYNC_HOME = $env:RSYNC_HOME
$RSYNC_PATH = $env:RSYNC_PATH
$RSYNC_USER = $env:RSYNC_USER
$RSYNC_HOST = $env:RSYNC_HOST
$RSYNC_ROOT = $env:RSYNC_ROOT
$USERPROFILE = $env:USERPROFILE

###########################################################################################
## Developer upload
###########################################################################################

#
# Preparation
#

if((Test-Path ${upload_path}) -ne 0) {
  Remove-Item -Path ${upload_path} -Recurse
}

mkdir ${upload_path}

#
# Build Keyman Compiler WINE archive
#

if((Test-Path ..\release\${kmcomp_zip}) -ne 0) {
  # REVIEW: I guess this is the path for Keyman 18+. In which case we
  # can remove the else block
  copy ..\release\${kmcomp_zip} ..\${upload_path}\${kmcomp_zip}
} else {
  cd bin
  # Keyman versions 17.0+; note, use npm install for most modules
  copy ..\..\common\schemas\keyboard_info\keyboard_info.schema.json .
  & "${7Z_HOME}\7z.exe" a -bd -bb0 ..\${upload_path}\${kmcomp_zip} kmconvert.exe keyboard_info.schema.json xml\layoutbuilder\*.keyman-touch-layout projects\ server\
  cd ..
}

#
# Copy source files
#

copy release\${build_number}\${keyman_developer_exe} ${upload_path}\${keyman_developer_exe}

#
# Construct keyman_developer.download_info
#

$hash = get-filehash ${upload_path}\${keyman_developer_exe} -Algorithm MD5

$keyman_developer_download_info = @"
{
  "name": "Keyman Developer",
  "version": "${version_with_tag}",
  "date": "$([DateTime]::Now.ToString("yyyy-MM-dd"))",
  "platform": "win",
  "stability": "${tier}",
  "file": "keymandeveloper-${version_with_tag}.exe",
  "md5": "$(${hash}.Hash)",
  "type": "exe",
  "build": "${build_counter}"
}
"@

[System.IO.File]::WriteAllLines("${pwd}\${upload_path}\${keyman_developer_exe}.download_info", ${keyman_developer_download_info})

#
# Construct kmcomp.download_info
#

$hash = get-filehash ${upload_path}\${kmcomp_zip} -Algorithm MD5

$kmcomp_download_info = @"
{
  "name": "Keyman Developer Command-Line Compiler",
  "version": "${version_with_tag}",
  "date": "$([DateTime]::Now.ToString("yyyy-MM-dd"))",
  "platform": "win",
  "stability": "${tier}",
  "file": "kmcomp-${version_with_tag}.zip",
  "md5": "$(${hash}.Hash)",
  "type": "zip",
  "build": "${build_counter}"
}
"@

[System.IO.File]::WriteAllLines("${pwd}\${upload_path}\${kmcomp_zip}.download_info", ${kmcomp_download_info})

#
# Copy common/test/keyboards/*/build/*.kmp to keyboards/
#
if((Test-Path ..\common\test\keyboards) -ne 0) {
  mkdir ${keyboards_path}
  Copy-Item ..\common\test\keyboards\*\build\*.kmp ${keyboards_path}\
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

# Write-Output "rsync parameters:" ${rsync_args}

cd upload
& ${RSYNC_HOME}\rsync.exe ${rsync_args}
if( ${LASTEXITCODE} -ne 0 ) {
  Write-Output "rsync failed."
  exit 99
}
cd ..

#################
# DEBUG.ZIP
#################

if((Test-Path release\${build_number}\${debug_zip}) -ne 0) {
  #
  # Copy source files
  #

  copy release\${build_number}\${debug_zip} ${upload_path}\${debug_zip}

  #
  # Construct debug-<version>.download_info
  #

  $hash = get-filehash ${upload_path}\${debug_zip} -Algorithm MD5

  $debug_zip_download_info = @"
{
  "name": "Keyman Developer debug files",
  "version": "${version_with_tag}",
  "date": "$([DateTime]::Now.ToString("yyyy-MM-dd"))",
  "platform": "win",
  "stability": "${tier}",
  "file": "debug-${build_number}.zip",
  "md5": "$(${hash}.Hash)",
  "type": "zip",
  "build": "${build_counter}"
}
"@

  [System.IO.File]::WriteAllLines("${pwd}\${upload_path}\${debug_zip}.download_info", ${debug_zip_download_info})
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

# Write-Output "rsync parameters:" ${rsync_args}

cd upload
& ${RSYNC_HOME}\rsync.exe ${rsync_args}
if( ${LASTEXITCODE} -ne 0 ) {
  Write-Output "rsync failed."
  exit 99
}
cd ..

# EOF
