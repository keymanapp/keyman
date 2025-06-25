# Keyman is copyright (C) SIL Global. MIT License.
#
# This script should be the identical for nightly/beta/stable for a given platform
#

$ErrorActionPreference = "Stop"

$RSYNC_HOME = $env:RSYNC_HOME
$RSYNC_PATH = $env:RSYNC_PATH
$RSYNC_USER = $env:RSYNC_USER
$RSYNC_HOST = $env:RSYNC_HOST
$RSYNC_ROOT = $env:RSYNC_ROOT
$USERPROFILE = $env:USERPROFILE

New-Item -Force -ItemType Directory symbols\000admin
cd symbols\000admin

#
# Download the symbol server index files with rsync from downloads.keyman.com
# (rsync requires that we are in the symbols folder to get folders in
# sync correctly; it is possible to resolve this but easier to just cd.)
#

$rsync_args = @(
  '-vrzltp',                                # verbose, recurse, zip, copy symlinks, preserve times, permissions
  '--chmod=Dug=rwx,Do=rx,Fug=rw,Fo=r',      # map Windows security to host security
  '--stats',                                # show statistics for log
  "--rsync-path=""${RSYNC_PATH}""",         # path on remote server
  "--rsh=${RSYNC_HOME}\ssh -i ${USERPROFILE}\.ssh\id_rsa -o UserKnownHostsFile=${USERPROFILE}\.ssh\known_hosts",                  # use ssh
  "${RSYNC_USER}@${RSYNC_HOST}:${RSYNC_ROOT}/windows/symbols/000admin/lastid.txt", # target server + path
  "."                                       # download the whole symbols 000Admin folder
)

& ${RSYNC_HOME}\rsync.exe ${rsync_args}
if (${LASTEXITCODE} -ne 0) { throw "Exit code is ${LASTEXITCODE}" }

$rsync_args = @(
  '-vrzltp',                                # verbose, recurse, zip, copy symlinks, preserve times, permissions
  '--chmod=Dug=rwx,Do=rx,Fug=rw,Fo=r',      # map Windows security to host security
  '--stats',                                # show statistics for log
  "--rsync-path=""${RSYNC_PATH}""",         # path on remote server
  "--rsh=${RSYNC_HOME}\ssh -i ${USERPROFILE}\.ssh\id_rsa -o UserKnownHostsFile=${USERPROFILE}\.ssh\known_hosts",                  # use ssh
  "${RSYNC_USER}@${RSYNC_HOST}:${RSYNC_ROOT}/windows/symbols/000admin/history.txt", # target server + path
  "."                                       # download the whole symbols 000Admin folder
)

& ${RSYNC_HOME}\rsync.exe ${rsync_args}
if (${LASTEXITCODE} -ne 0) { throw "Exit code is ${LASTEXITCODE}" }

$rsync_args = @(
  '-vrzltp',                                # verbose, recurse, zip, copy symlinks, preserve times, permissions
  '--chmod=Dug=rwx,Do=rx,Fug=rw,Fo=r',      # map Windows security to host security
  '--stats',                                # show statistics for log
  "--rsync-path=""${RSYNC_PATH}""",         # path on remote server
  "--rsh=${RSYNC_HOME}\ssh -i ${USERPROFILE}\.ssh\id_rsa -o UserKnownHostsFile=${USERPROFILE}\.ssh\known_hosts",                  # use ssh
  "${RSYNC_USER}@${RSYNC_HOST}:${RSYNC_ROOT}/windows/symbols/000admin/server.txt", # target server + path
  "."                                       # download the whole symbols 000Admin folder
)

& ${RSYNC_HOME}\rsync.exe ${rsync_args}
if (${LASTEXITCODE} -ne 0) { throw "Exit code is ${LASTEXITCODE}" }

# EOF
