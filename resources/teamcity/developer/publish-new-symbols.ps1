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

# Rename 000Admin to 000admin

ren 000Admin 000admin_
ren 000admin_ 000admin

#
# Upload with rsync to downloads.keyman.com
# (rsync requires that we are in the symbols folder to get folders in
# sync correctly; it is possible to resolve this but easier to just cd.)
#

$rsync_args = @(
  '-vrzltp',                                # verbose, recurse, zip, copy symlinks, preserve times, permissions
  '--chmod=Dug=rwx,Do=rx,Fug=rw,Fo=r',      # map Windows security to host security
  '--stats',                                # show statistics for log
  "--rsync-path='${RSYNC_PATH}'",             # path on remote server
  "--rsh=${RSYNC_HOME}\ssh -i ${USERPROFILE}\.ssh\id_rsa -o UserKnownHostsFile=${USERPROFILE}\.ssh\known_hosts",                  # use ssh
  ".",                                      # upload the whole symbols folder
  "${RSYNC_USER}@${RSYNC_HOST}:${RSYNC_ROOT}/windows/symbols/" # target server + path
)

& ${RSYNC_HOME}\rsync.exe ${rsync_args}
if (${LASTEXITCODE} -ne 0) { throw "Exit code is ${LASTEXITCODE}" }

# EOF
