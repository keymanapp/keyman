# Keyman is copyright (C) SIL Global. MIT License.

$ErrorActionPreference = "Stop"

$tier = Get-Content ..\TIER.md
$build_number = Get-Content ..\VERSION.md
$build_counter = ${build_number} -replace "^\d+\.\d+\.(\d+)$", '$1'
$msi_major_version = ${build_number} -replace "^(\d+)\.(\d+)\.(\d+)$", '$1$2'

if ( ${tier} -eq "alpha" -or ${tier} -eq "beta") {
  # $version_tag = ${tier}
  $version_with_tag = ${build_number}  # we may go with ${build_number} + "-" + $version_tag in the future one day
}
else {
  $version_with_tag = ${build_number}
}

$upload_path = "upload\${build_number}"

# Keyman Desktop installer
$keyman_desktop_exe = "keyman-${version_with_tag}.exe"

# Files used for building bundled keyboard + Keyman installers (placed in Desktop folder)
if (${msi_major_version} -eq "100") {
  $keyman_desktop_msi = "keymandesktop${msi_major_version}.msi"
}
else {
  $keyman_desktop_msi = "keymandesktop.msi"
}
$setup_exe = "setup.exe"
$setup_redist_zip = "setup-redist.zip"

$firstvoices_msi = "firstvoices.msi"
$firstvoices_exe = "firstvoices-${version_with_tag}.exe"

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
## Desktop upload
###########################################################################################

#
# Preparation
#

if ((Test-Path ${upload_path}) -ne 0) {
  Remove-Item -Path ${upload_path} -Recurse
}

mkdir ${upload_path}

#
# Copy source files
#

copy release\${build_number}\${keyman_desktop_exe} ${upload_path}\${keyman_desktop_exe}
copy release\${build_number}\${keyman_desktop_msi} ${upload_path}\${keyman_desktop_msi}
copy release\${build_number}\${setup_exe} ${upload_path}\${setup_exe}
if ((Test-Path release\${build_number}\${setup_redist_zip}) -ne 0) {
  # Keyman 16+ setup-redist.zip is an archive of the setup.exe installer for
  # Keyman for Windows, unsigned for use in bundling scenarios.
  copy release\${build_number}\${setup_redist_zip} ${upload_path}\${setup_redist_zip}
}
#
# Construct keyman_desktop.download_info
#

$hash = get-filehash ${upload_path}\${keyman_desktop_exe} -Algorithm MD5

$keyman_desktop_download_info = @"
{
  "name": "Keyman for Windows",
  "version": "${version_with_tag}",
  "date": "$([DateTime]::Now.ToString("yyyy-MM-dd"))",
  "platform": "win",
  "stability": "${tier}",
  "file": "keyman-${version_with_tag}.exe",
  "md5": "$(${hash}.Hash)",
  "type": "exe",
  "build": "${build_counter}"
}
"@

[System.IO.File]::WriteAllLines("$pwd\${upload_path}\${keyman_desktop_exe}.download_info", ${keyman_desktop_download_info})

#
# Construct keyman_desktop_msi.download_info
#

$hash = get-filehash ${upload_path}\${keyman_desktop_msi} -Algorithm MD5

$keyman_desktop_msi_download_info = @"
{
  "name": "Keyman for Windows MSI installer",
  "version": "${version_with_tag}",
  "date": "$([DateTime]::Now.ToString("yyyy-MM-dd"))",
  "platform": "win",
  "stability": "${tier}",
  "file": "${keyman_desktop_msi}",
  "md5": "$(${hash}.Hash)",
  "type": "msi",
  "build": "${build_counter}"
}
"@

[System.IO.File]::WriteAllLines("$pwd\${upload_path}\${keyman_desktop_msi}.download_info", ${keyman_desktop_msi_download_info})

#
# Construct setup.exe.download_info
#

$hash = get-filehash ${upload_path}\${setup_exe} -Algorithm MD5

$setup_exe_download_info = @"
{
  "name": "Keyman for Windows setup bootstrap",
  "version": "${version_with_tag}",
  "date": "$([DateTime]::Now.ToString("yyyy-MM-dd"))",
  "platform": "win",
  "stability": "${tier}",
  "file": "${setup_exe}",
  "md5": "$(${hash}.Hash)",
  "type": "exe",
  "build": "${build_counter}"
}
"@

[System.IO.File]::WriteAllLines("$pwd\${upload_path}\${setup_exe}.download_info", ${setup_exe_download_info})

if ((Test-Path release\${build_number}\${setup_redist_zip}) -ne 0) {

  #
  # Construct setup-redist.zip.download_info
  #

  $hash = get-filehash ${upload_path}\${setup_redist_zip} -Algorithm MD5

  $setup_redist_zip_download_info = @"
{
  "name": "Keyman for Windows setup bootstrap (unsigned for bundling)",
  "version": "${version_with_tag}",
  "date": "$([DateTime]::Now.ToString("yyyy-MM-dd"))",
  "platform": "win",
  "stability": "${tier}",
  "file": "${setup_redist_zip}",
  "md5": "$(${hash}.Hash)",
  "type": "zip",
  "build": "${build_counter}"
}
"@

  [System.IO.File]::WriteAllLines("$pwd\${upload_path}\${setup_redist_zip}.download_info", ${setup_redist_zip_download_info})
}

###########################################################################################
## FirstVoices upload
###########################################################################################

#
# Copy source files
#

copy release\${build_number}\${firstvoices_exe} ${upload_path}\${firstvoices_exe}
copy release\${build_number}\${firstvoices_msi} ${upload_path}\${firstvoices_msi}

#
# Construct firstvoices.download_info
#

$hash = get-filehash ${upload_path}\${firstvoices_exe} -Algorithm MD5

$firstvoices_download_info = @"
{
  "name": "FirstVoices Keyboards",
  "version": "${version_with_tag}",
  "date": "$([DateTime]::Now.ToString("yyyy-MM-dd"))",
  "platform": "win",
  "stability": "${tier}",
  "file": "${firstvoices_exe}",
  "md5": "$(${hash}.Hash)",
  "type": "exe",
  "build": "${build_counter}"
}
"@

[System.IO.File]::WriteAllLines("$pwd\${upload_path}\${firstvoices_exe}.download_info", ${firstvoices_download_info})

#
# Construct firstvoices_msi.download_info
#

$hash = get-filehash ${upload_path}\${firstvoices_msi} -Algorithm MD5

$firstvoices_msi_download_info = @"
{
  "name": "FirstVoices Keyboards MSI installer",
  "version": "${version_with_tag}",
  "date": "$([DateTime]::Now.ToString("yyyy-MM-dd"))",
  "platform": "win",
  "stability": "${tier}",
  "file": "${firstvoices_msi}",
  "md5": "$(${hash}.Hash)",
  "type": "msi",
  "build": "${build_counter}"
}
"@

[System.IO.File]::WriteAllLines("$pwd\${upload_path}\${firstvoices_msi}.download_info", ${firstvoices_msi_download_info})

#################
# DEBUG.ZIP
#################

if ((Test-Path release\${build_number}\${debug_zip}) -ne 0) {
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
  "name": "Keyman Desktop and Keyman Developer debug files",
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

  [System.IO.File]::WriteAllLines("$pwd\${upload_path}\${debug_zip}.download_info", ${debug_zip_download_info})
}

#
# Upload with rsync to downloads.keyman.com
# (rsync requires that we are in the upload folder to get folders in
# sync correctly; it is possible to resolve this but easier to just cd.)
#

$rsync_args = @(
  '-vrzltp', # verbose, recurse, zip, copy symlinks, preserve times, permissions
  '--chmod=Dug=rwx,Do=rx,Fug=rw,Fo=r', # map Windows security to host security
  '--stats', # show statistics for log
  "--rsync-path='${RSYNC_PATH}'", # path on remote server
  "--rsh=${RSYNC_HOME}\ssh -i ${USERPROFILE}\.ssh\id_rsa -o UserKnownHostsFile=${USERPROFILE}\.ssh\known_hosts", # use ssh
  "${build_number}", # upload the whole build folder
  "${RSYNC_USER}@${RSYNC_HOST}:${RSYNC_ROOT}/windows/${tier}/" # target server + path
)

# Write-Output "rsync parameters:" ${rsync_args}

cd upload
& ${RSYNC_HOME}\rsync.exe ${rsync_args}
if ( ${LASTEXITCODE} -ne 0 ) {
  Write-Output "rsync failed."
  exit 99
}
cd ..

# EOF
