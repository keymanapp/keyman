{******************************************************************************}
{                                                                              }
{ Directory Backup and Restore API interface Unit for Object Pascal            }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: ntdsbcli.h, released June 2000. The original Pascal    }
{ code is: NtDsbCli.pas, released December 2000. The initial developer of the  }
{ Pascal code is Marcel van Brakel (brakelm att chello dott nl).               }
{                                                                              }
{ Portions created by Marcel van Brakel are Copyright (C) 1999-2001            }
{ Marcel van Brakel. All Rights Reserved.                                      }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project JEDI         }
{ APILIB home page, located at http://jedi-apilib.sourceforge.net              }
{                                                                              }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{                                                                              }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{******************************************************************************}

// $Id: JwaNtDsbCli.pas,v 1.12 2007/09/05 11:58:51 dezipaitor Exp $
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaNtDsbCli;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include ":ntdsbcli.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaWinType;
{$ENDIF JWA_OMIT_SECTIONS}
{$IFNDEF JWA_IMPLEMENTATIONSECTION}

const
  g_wszBackupAnnotation = 'NTDS Backup Interface';
  {$EXTERNALSYM g_wszBackupAnnotation}
  g_aszBackupAnnotation = 'NTDS Backup Interface';
  {$EXTERNALSYM g_aszBackupAnnotation}

  g_wszRestoreAnnotation = 'NTDS Restore Interface';
  {$EXTERNALSYM g_wszRestoreAnnotation}
  g_aszRestoreAnnotation = 'NTDS Restore Interface';
  {$EXTERNALSYM g_aszRestoreAnnotation}

  {$IFDEF UNICODE}
  g_szBackupAnnotation  = g_wszBackupAnnotation;
  {$EXTERNALSYM g_szBackupAnnotation}
  g_szRestoreAnnotation = g_wszRestoreAnnotation;
  {$EXTERNALSYM g_szRestoreAnnotation}
  {$ELSE}
  g_szBackupAnnotation  = g_aszBackupAnnotation;
  {$EXTERNALSYM g_szBackupAnnotation}
  g_szRestoreAnnotation = g_aszRestoreAnnotation;
  {$EXTERNALSYM g_szRestoreAnnotation}
  {$ENDIF UNICODE}

// Type of Backup passed to DsBackupPrepare()
// BACKUP_TYPE_FULL: Requesting backup of the complete DS (DIT, Log files, and Patch files)
// BACKUP_TYPE_LOGS_ONLY: Requesting backup of only the log files
// BACKUP_TYPE_INCREMENTAL: Requesting incremental backup i.e. backing up only changes that happened since last backup

  BACKUP_TYPE_FULL        = $01;
  {$EXTERNALSYM BACKUP_TYPE_FULL}
  BACKUP_TYPE_LOGS_ONLY   = $02;
  {$EXTERNALSYM BACKUP_TYPE_LOGS_ONLY}
  BACKUP_TYPE_INCREMENTAL = $04; // not supported in product1
  {$EXTERNALSYM BACKUP_TYPE_INCREMENTAL}

// Type of Restore passed to DsRestorePrepare()
// RESTORE_TYPE_AUTHORATATIVE: The restored version wins throughout the enterprise
// RESTORE_TYPE_ONLINE: Restoration is done when NTDS is online.
// RESTORE_TYPE_CATCHUP: The restored version is reconciled through the standard reconciliation logic so that the
//                          restored DIT can catchup with the rest of the enterprise.

  RESTORE_TYPE_AUTHORATATIVE = $01;
  {$EXTERNALSYM RESTORE_TYPE_AUTHORATATIVE}
  RESTORE_TYPE_ONLINE        = $02; // not supported in product1
  {$EXTERNALSYM RESTORE_TYPE_ONLINE}
  RESTORE_TYPE_CATCHUP       = $04; // this is the default restore mode
  {$EXTERNALSYM RESTORE_TYPE_CATCHUP}

// Setting the current log # to this value would disable incremental/differential backup

  BACKUP_DISABLE_INCREMENTAL = DWORD($ffffffff);
  {$EXTERNALSYM BACKUP_DISABLE_INCREMENTAL}

// BFT is the bit flag used to represent file types (directory/dit/logfile/etc.)
// We keep them as a character so that we can append/prepend them to the actual file
// path. The code in the Backup API's rely on the fact that values 0-256 in 8 bit ascii
// map to the values 0-256 in unicode.

type
  {$IFDEF UNICODE}
  BFT = WCHAR;
  {$EXTERNALSYM BFT}
  {$ELSE}
  BFT = AnsiChar;
  {$EXTERNALSYM BFT}
  {$ENDIF UNICODE}

// Bit flags:
//  BFT_DIRECTORY               - indicates path specified is a directory
//  BFT_DATABASE_DIRECTORY      - indicates that file goes into database directory
//  BFT_LOG_DIRECTORY           - indicates that the file goes into log directory

const
  BFT_DIRECTORY          = $80;
  {$EXTERNALSYM BFT_DIRECTORY}
  BFT_DATABASE_DIRECTORY = $40;
  {$EXTERNALSYM BFT_DATABASE_DIRECTORY}
  BFT_LOG_DIRECTORY      = $20;
  {$EXTERNALSYM BFT_LOG_DIRECTORY}

// Following combinations are defined for easy use of the filetype and the directory into
// into which it goes

  BFT_LOG            = BFT($01 or BFT_LOG_DIRECTORY);
  BFT_LOG_DIR        = BFT($02 or BFT_DIRECTORY);
  BFT_CHECKPOINT_DIR = BFT($03 or BFT_DIRECTORY);
  BFT_NTDS_DATABASE  = BFT($04 or BFT_DATABASE_DIRECTORY);
  BFT_PATCH_FILE     = BFT($05 or BFT_LOG_DIRECTORY);
  BFT_UNKNOWN        = BFT($0F);

// #include <ntdsbmsg.h>

// Backup Context Handle

type
  HBC = Pointer;
  {$EXTERNALSYM HBC}

  PEDB_RSTMAPA = ^EDB_RSTMAPA;
  {$EXTERNALSYM PEDB_RSTMAPA}
  tagEDB_RSTMAPA = record
    szDatabaseName: PAnsiChar;
    szNewDtabaseName: PAnsiChar;
  end;
  {$EXTERNALSYM tagEDB_RSTMAPA}
  EDB_RSTMAPA = tagEDB_RSTMAPA;
  {$EXTERNALSYM EDB_RSTMAPA}
  TEdbRstMapA = EDB_RSTMAPA;
  PEdbRstMapA = PEDB_RSTMAPA;

// required for NTDS unicode support.
// UNDONE: NYI

  PEDB_RSTMAPW = ^EDB_RSTMAPW;
  {$EXTERNALSYM PEDB_RSTMAPW}
  tagEDB_RSTMAPW = record
    wszDatabaseName: PWCHAR;
    wszNewDatabaseName: PWCHAR;
  end;
  {$EXTERNALSYM tagEDB_RSTMAPW}
  EDB_RSTMAPW = tagEDB_RSTMAPW;
  {$EXTERNALSYM EDB_RSTMAPW}
  TEdbRstMapW = EDB_RSTMAPW;
  PEdbRstMapW = PEDB_RSTMAPW;

  {$IFDEF UNICODE}
  EDB_RSTMAP = EDB_RSTMAPW;
  {$EXTERNALSYM EDB_RSTMAP}
  PEDB_RSTMAP = PEDB_RSTMAPW;
  {$EXTERNALSYM PEDB_RSTMAP}
  TEdbRstMap = TEdbRstMapW;
  PEdbRstMap = PEdbRstMapW;
  {$ELSE}
  EDB_RSTMAP = EDB_RSTMAPA;
  {$EXTERNALSYM EDB_RSTMAP}
  PEDB_RSTMAP = PEDB_RSTMAPA;
  {$EXTERNALSYM PEDB_RSTMAP}
  TEdbRstMap = TEdbRstMapA;
  PEdbRstMap = PEdbRstMapA;
  {$ENDIF UNICODE}

{*************************************************************************************
Routine Description:

      DsIsNTDSOnline
        Checks to see if the NTDS is Online on the given server. This call is
        guaranteed to return quickly.

  Arguments:
    [in] szServerName - UNC name of the server to check
    [out] pfNTDSOnline - pointer to receive the bool result (TRUE if NTDS is
                            online; FALSE, otherwise)

Return Value:

    ERROR_SUCCESS if the call executed successfully;
    Failure code otherwise.
**************************************************************************************}

function DsIsNTDSOnlineA(szServerName: LPCSTR; var pfNTDSOnline: BOOL): HRESULT; stdcall;
{$EXTERNALSYM DsIsNTDSOnlineA}
function DsIsNTDSOnlineW(szServerName: LPCWSTR; var pfNTDSOnline: BOOL): HRESULT; stdcall;
{$EXTERNALSYM DsIsNTDSOnlineW}
function DsIsNTDSOnline(szServerName: LPCTSTR; var pfNTDSOnline: BOOL): HRESULT; stdcall;
{$EXTERNALSYM DsIsNTDSOnline}

{*************************************************************************************
Routine Description:

      DsBackupPrepare
        Prepares the DS for the online backup and returns a Backup Context Handle
        which should be used in the subsequent calls to other backup functions.

  Arguments:
    [in]    szBackupServer - UNC name of the server to be prepared for online backup
    [in]    grbit - flag to be passed to jet while backing up dbs
    [in]    btFlag - BACKUP_TYPE_FULL or BACKUP_TYPE_LOGS_ONLY
    [out]   ppvExpiryToken - pointer that will receive the pointer to the
                Expiry Token associated with this backup; Client should save
                this token and send it back through DsRestorePrepare() when
                attempting a restore; allocated memory should be freed using
                DsBackupFree() API by the caller when it is no longer needed.
    [out]   pcbExpiryTokenSize - pointer to receive the size of the expiry token
                returned.
    [out]   phbc - pointer that will receive the backup context handle

Return Value:

    One of the standard HRESULT success codes;
    Failure code otherwise.
**************************************************************************************}

function DsBackupPrepareA(szBackupServer: LPCSTR; grbit: ULONG; btFlag: ULONG;
  var ppvExpiryToken: PVOID; var pcbExpiryTokenSize: DWORD; var phbc: HBC): HRESULT; stdcall;
{$EXTERNALSYM DsBackupPrepareA}
function DsBackupPrepareW(szBackupServer: LPCWSTR; grbit: ULONG; btFlag: ULONG;
  var ppvExpiryToken: PVOID; var pcbExpiryTokenSize: DWORD; var phbc: HBC): HRESULT; stdcall;
{$EXTERNALSYM DsBackupPrepareW}
function DsBackupPrepare(szBackupServer: LPCTSTR; grbit: ULONG; btFlag: ULONG;
  var ppvExpiryToken: PVOID; var pcbExpiryTokenSize: DWORD; var phbc: HBC): HRESULT; stdcall;
{$EXTERNALSYM DsBackupPrepare}

{*************************************************************************************
Routine Description:

      DsBackupGetDatabaseNames
        Gives the list of data bases that need to be backed up for the given
        backup context

  Arguments:
    [in]    hbc - backup context handle
    [out]   pszAttachmentInfo - pointer that will receive the pointer to the attachment
                info; allocated memory should be freed using DsBackupFree() API by the
                caller when it is no longer needed; Attachment info is an array of
                null-terminated filenames and and the list is terminated by two-nulls.
    [out]   pcbSize - will receive the number of bytes returned
Return Value:

    One of the standard HRESULT success codes;
    Failure code otherwise.
**************************************************************************************}

function DsBackupGetDatabaseNamesA(hbc: HBC; var pszAttachmentInfo: LPSTR;
  var pcbSize: DWORD): HRESULT; stdcall;
{$EXTERNALSYM DsBackupGetDatabaseNamesA}
function DsBackupGetDatabaseNamesW(hbc: HBC; var pszAttachmentInfo: LPWSTR;
  var pcbSize: DWORD): HRESULT; stdcall;
{$EXTERNALSYM DsBackupGetDatabaseNamesW}
function DsBackupGetDatabaseNames(hbc: HBC; var pszAttachmentInfo: LPTSTR;
  var pcbSize: DWORD): HRESULT; stdcall;
{$EXTERNALSYM DsBackupGetDatabaseNames}

{*************************************************************************************
Routine Description:

      DsBackupOpenFile
        Opens the given attachment for read.

  Arguments:
    [in]    hbc - backup context handle
    [in]    szAttachmentName - name of the attachment to be opened for read
    [in]    cbReadHintSize - suggested size in bytes that might be used during the
                subsequent reads on this attachement
    [out]   pliFileSize - pointer to a large integer that would receive the size in
                bytes of the given attachment
Return Value:

    One of the standard HRESULT success codes;
    Failure code otherwise.
**************************************************************************************}

function DsBackupOpenFileA(hbc: HBC; szAttachmentName: LPCSTR; cbReadHintSize: DWORD;
  var pliFileSize: LARGE_INTEGER): HRESULT; stdcall;
{$EXTERNALSYM DsBackupOpenFileA}
function DsBackupOpenFileW(hbc: HBC; szAttachmentName: LPCWSTR; cbReadHintSize: DWORD;
  var pliFileSize: LARGE_INTEGER): HRESULT; stdcall;
{$EXTERNALSYM DsBackupOpenFileW}
function DsBackupOpenFile(hbc: HBC; szAttachmentName: LPCTSTR; cbReadHintSize: DWORD;
  var pliFileSize: LARGE_INTEGER): HRESULT; stdcall;
{$EXTERNALSYM DsBackupOpenFile}

{*************************************************************************************
Routine Description:

      DsBackupRead
        Reads the currently open attachment bytes into the given buffer. The client
        application is expected to call this function repeatedly until it gets the
        entire file (the application would have received the file size through the
        DsBackupOpenFile() call before.

  Arguments:
    [in]    hbc - backup context handle
    [in]    pvBuffer - pointer to the buffer that would receive the read data.
    [in]    cbBuffer - specifies the size of the above buffer
    [out]   pcbRead - pointer to receive the actual number of bytes read.
Return Value:

    One of the standard HRESULT success codes;
    Failure code otherwise.
**************************************************************************************}

function DsBackupRead(hbc: HBC; pvBuffer: PVOID; cbBuffer: DWORD; var pcbRead: DWORD): HRESULT; stdcall;
{$EXTERNALSYM DsBackupRead}

{*************************************************************************************
Routine Description:

      DsBackupClose
        To be called by the application after it completes reading all the data in
        the currently opened attachement.

  Arguments:
    [in]    hbc - backup context handle
Return Value:

    One of the standard HRESULT success codes;
    Failure code otherwise.
**************************************************************************************}

function DsBackupClose(hbc: HBC): HRESULT; stdcall;
{$EXTERNALSYM DsBackupClose}

{*************************************************************************************
Routine Description:

      DsBackupGetBackupLogs
        Gives the list of log files that need to be backed up for the given
        backup context

  Arguments:
    [in]    hbc - backup context handle
    [out]   pszBackupLogFiles - pointer that will receive the pointer to the list of
                log files; allocated memory should be freed using DsBackupFree() API by the
                caller when it is no longer needed; Log files are returned in an array of
                null-terminated filenames and and the list is terminated by two-nulls.
    [out]   pcbSize - will receive the number of bytes returned
Return Value:

    One of the standard HRESULT success codes;
    Failure code otherwise.
**************************************************************************************}

function DsBackupGetBackupLogsA(hbc: HBC; var pszBackupLogFiles: LPSTR;
  var pcbSize: DWORD): HRESULT; stdcall;
{$EXTERNALSYM DsBackupGetBackupLogsA}
function DsBackupGetBackupLogsW(hbc: HBC; var pszBackupLogFiles: LPWSTR;
  var pcbSize: DWORD): HRESULT; stdcall;
{$EXTERNALSYM DsBackupGetBackupLogsW}
function DsBackupGetBackupLogs(hbc: HBC; var pszBackupLogFiles: LPTSTR;
  var pcbSize: DWORD): HRESULT; stdcall;
{$EXTERNALSYM DsBackupGetBackupLogs}

{*************************************************************************************
Routine Description:

      DsBackupTruncateLogs
        Called to truncate the already read backup logs.

  Arguments:
    [in]    hbc - backup context handle
Return Value:

    One of the standard HRESULT success codes;
    Failure code otherwise.
**************************************************************************************}

function DsBackupTruncateLogs(hbc: HBC): HRESULT; stdcall;
{$EXTERNALSYM DsBackupTruncateLogs}

{*************************************************************************************
Routine Description:

      DsBackupEnd
        Called to end the current backup session.

  Arguments:
    [in]    hbc - backup context handle of the backup session
Return Value:

    One of the standard HRESULT success codes;
    Failure code otherwise.
**************************************************************************************}

function DsBackupEnd(hbc: HBC): HRESULT; stdcall;
{$EXTERNALSYM DsBackupEnd}

{*************************************************************************************
Routine Description:

      DsBackupFree
        Should be used by the application to free any buffer allocated by the
        NTDSBCLI dll.

  Arguments:
    [in]    pvBuffer - pointer to the buffer that is to be freed.

  Return Value:
    None.
**************************************************************************************}

procedure DsBackupFree(pvBuffer: PVOID); stdcall;
{$EXTERNALSYM DsBackupFree}

{*************************************************************************************
Routine Description:

      DsRestoreGetDatabaseLocations
        Called both at backup time as well at restoration time to get the data base
        locations for different types of files.

  Arguments:
    [in]    hbc - backup context handle which would have been obtained through
                    DsBackupPrepare() in the backup case and through DsRestorePrepare()
                    in the restore case.
    [out]   pszDatabaseLocationList - pointer that will receive the pointer to the list of
                database locations; allocated memory should be freed using DsBackupFree() API by the
                caller when it is no longer needed; locations are returned in an array of
                null-terminated names and and the list is terminated by two-nulls.
                The first character of each name is the BFT character that indicates the type
                of the file and the rest of the name tells gives the path into which that
                particular type of file should be restored.
    [out]   pcbSize - will receive the number of bytes returned
Return Value:

    One of the standard HRESULT success codes;
    Failure code otherwise.
**************************************************************************************}

function DsRestoreGetDatabaseLocationsA(hbc: HBC; var pszDatabaseLocationList: LPSTR;
  var pcbSize: DWORD): HRESULT; stdcall;
{$EXTERNALSYM DsRestoreGetDatabaseLocationsA}
function DsRestoreGetDatabaseLocationsW(hbc: HBC; var pszDatabaseLocationList: LPWSTR;
  var pcbSize: DWORD): HRESULT; stdcall;
{$EXTERNALSYM DsRestoreGetDatabaseLocationsW}
function DsRestoreGetDatabaseLocations(hbc: HBC; var pszDatabaseLocationList: LPTSTR;
  var pcbSize: DWORD): HRESULT; stdcall;
{$EXTERNALSYM DsRestoreGetDatabaseLocations}

{*************************************************************************************
Routine Description:

      DsRestorePrepare
        Called to indicate beginning of a restore session.

  Arguments:
    [in]    szServerName - UNC name of the server into which the restore operation is
                            going to be performed.
    [in]    rtFlag -  Or'ed combination of RESTORE_TYPE_* flags; 0 if no special flags
                            are to be specified
    [in]    pvExpiryToken - pointer to the expiry token associated with this
                            backup. The client would have received this when they backed up the DS.
    [in]    cbExpiryTokenSize - size of the expiry token.
    [out]   phbc - pointer to receive the backup context handle which is to be passed
                            to the subsequent restore APIs

Return Value:

    One of the standard HRESULT success codes;
    Failure code otherwise.
**************************************************************************************}

function DsRestorePrepareA(szServerName: LPCSTR; rtFlag: ULONG; pvExpiryToken: PVOID;
  cbExpiryTokenSize: DWORD; var phbc: HBC): HRESULT; stdcall;
{$EXTERNALSYM DsRestorePrepareA}
function DsRestorePrepareW(szServerName: LPCWSTR; rtFlag: ULONG; pvExpiryToken: PVOID;
  cbExpiryTokenSize: DWORD; var phbc: HBC): HRESULT; stdcall;
{$EXTERNALSYM DsRestorePrepareW}
function DsRestorePrepare(szServerName: LPCTSTR; rtFlag: ULONG; pvExpiryToken: PVOID;
  cbExpiryTokenSize: DWORD; var phbc: HBC): HRESULT; stdcall;
{$EXTERNALSYM DsRestorePrepare}

{*************************************************************************************
Routine Description:

      DsRestoreRegister
        This will register a restore operation. It will interlock all sbsequent restore
        operations, and will prevent the restore target from starting until the call
        to DsRestoreRegisterComplete() is made.

  Arguments:
    [in]    hbc - backup context handle for the restore session.
    [in]    szCheckPointFilePath - path where the check point files are restored
    [in]    szLogPath - path where the log files are restored
    [in]    rgrstmap - restore map
    [in]    crstmap - tells if ther is a new restore map
    [in]    szBackupLogPath - path where the backup logs are located
    [in]    genLow - Lowest log# that was restored in this restore session
    [in]    genHigh - Highest log# that was restored in this restore session

  Return Value:

    One of the standard HRESULT success codes;
    Failure code otherwise.
**************************************************************************************}

function DsRestoreRegisterA(hbc: HBC; szCheckPointFilePath, szLogPath: LPCSTR;
  rgrstmap: PEDB_RSTMAPA; crstmap: LONG; szBackupLogPath: LPCSTR; genLow, genHigh: ULONG): HRESULT; stdcall;
{$EXTERNALSYM DsRestoreRegisterA}
function DsRestoreRegisterW(hbc: HBC; szCheckPointFilePath, szLogPath: LPCWSTR;
  rgrstmap: PEDB_RSTMAPW; crstmap: LONG; szBackupLogPath: LPCWSTR; genLow, genHigh: ULONG): HRESULT; stdcall;
{$EXTERNALSYM DsRestoreRegisterW}
function DsRestoreRegister(hbc: HBC; szCheckPointFilePath, szLogPath: LPCTSTR;
  rgrstmap: PEDB_RSTMAP; crstmap: LONG; szBackupLogPath: LPCTSTR; genLow, genHigh: ULONG): HRESULT; stdcall;
{$EXTERNALSYM DsRestoreRegister}

{*************************************************************************************
Routine Description:

      DsRestoreRegisterComplete
        Called to indicate that a previously registered restore is complete.

  Arguments:
    [in]    hbc - backup context handle
    [in]    hrRestoreState - success code if the restore was successful
Return Value:

    One of the standard HRESULT success codes;
    Failure code otherwise.
**************************************************************************************}

function DsRestoreRegisterComplete(hbc: HBC; hrRestoreState: HRESULT): HRESULT; stdcall;
{$EXTERNALSYM DsRestoreRegisterComplete}

{*************************************************************************************
Routine Description:

      DsRestoreEnd
        Called to end a restore session

  Arguments:
    [in]    hbc - backup context handle
Return Value:

    One of the standard HRESULT success codes;
    Failure code otherwise.
**************************************************************************************}

function DsRestoreEnd(hbc: HBC): HRESULT; stdcall;
{$EXTERNALSYM DsRestoreEnd}

{*************************************************************************************
Routine Description:

      DsSetCurrentBackupLog
        Called to set the current backup log number after a successful restore

  Arguments:
    [in]    szServerName - UNC name of the server for which the current backup log has
                                to be set
    [in]    dwCurrentLog -  current log number
Return Value:

    One of the standard HRESULT success codes;
    Failure code otherwise.
**************************************************************************************}

function DsSetCurrentBackupLogA(szServerName: LPCSTR; dwCurrentLog: DWORD): HRESULT; stdcall;
{$EXTERNALSYM DsSetCurrentBackupLogA}
function DsSetCurrentBackupLogW(szServerName: LPCWSTR; dwCurrentLog: DWORD): HRESULT; stdcall;
{$EXTERNALSYM DsSetCurrentBackupLogW}
function DsSetCurrentBackupLog(szServerName: LPCTSTR; dwCurrentLog: DWORD): HRESULT; stdcall;
{$EXTERNALSYM DsSetCurrentBackupLog}

{*************************************************************************************
Routine Description:

      DsSetAuthIdentity
        Used to set the security context under which the client APIs are to be
        called. If this function is not called, security context of the current
        process is assumed.

  Arguments:
    [in]    szUserName - name of the user
    [in]    szDomainName -  name of the domain the user belongs to
    [in]    szPassword - password of the user in the specified domain

Return Value:

    One of the standard HRESULT success codes;
    Failure code otherwise.
**************************************************************************************}

function DsSetAuthIdentityA(szUserName, szDomainName, szPassword: LPCSTR): HRESULT; stdcall;
{$EXTERNALSYM DsSetAuthIdentityA}
function DsSetAuthIdentityW(szUserName, szDomainName, szPassword: LPCWSTR): HRESULT; stdcall;
{$EXTERNALSYM DsSetAuthIdentityW}
function DsSetAuthIdentity(szUserName, szDomainName, szPassword: LPCTSTR): HRESULT; stdcall;
{$EXTERNALSYM DsSetAuthIdentity}
{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}


{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  ntdsbclilib = 'ntdsbclilib.dll';
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}

var
  _DsIsNTDSOnlineA: Pointer;

function DsIsNTDSOnlineA;
begin
  GetProcedureAddress(_DsIsNTDSOnlineA, ntdsbclilib, 'DsIsNTDSOnlineA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsIsNTDSOnlineA]
  end;
end;

var
  _DsIsNTDSOnlineW: Pointer;

function DsIsNTDSOnlineW;
begin
  GetProcedureAddress(_DsIsNTDSOnlineW, ntdsbclilib, 'DsIsNTDSOnlineW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsIsNTDSOnlineW]
  end;
end;

var
  _DsIsNTDSOnline: Pointer;

function DsIsNTDSOnline;
begin
  GetProcedureAddress(_DsIsNTDSOnline, ntdsbclilib, 'DsIsNTDSOnline' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsIsNTDSOnline]
  end;
end;

var
  _DsBackupPrepareA: Pointer;

function DsBackupPrepareA;
begin
  GetProcedureAddress(_DsBackupPrepareA, ntdsbclilib, 'DsBackupPrepareA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsBackupPrepareA]
  end;
end;

var
  _DsBackupPrepareW: Pointer;

function DsBackupPrepareW;
begin
  GetProcedureAddress(_DsBackupPrepareW, ntdsbclilib, 'DsBackupPrepareW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsBackupPrepareW]
  end;
end;

var
  _DsBackupPrepare: Pointer;

function DsBackupPrepare;
begin
  GetProcedureAddress(_DsBackupPrepare, ntdsbclilib, 'DsBackupPrepare' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsBackupPrepare]
  end;
end;

var
  _DsBackupGetDatabaseNamesA: Pointer;

function DsBackupGetDatabaseNamesA;
begin
  GetProcedureAddress(_DsBackupGetDatabaseNamesA, ntdsbclilib, 'DsBackupGetDatabaseNamesA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsBackupGetDatabaseNamesA]
  end;
end;

var
  _DsBackupGetDatabaseNamesW: Pointer;

function DsBackupGetDatabaseNamesW;
begin
  GetProcedureAddress(_DsBackupGetDatabaseNamesW, ntdsbclilib, 'DsBackupGetDatabaseNamesW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsBackupGetDatabaseNamesW]
  end;
end;

var
  _DsBackupGetDatabaseNames: Pointer;

function DsBackupGetDatabaseNames;
begin
  GetProcedureAddress(_DsBackupGetDatabaseNames, ntdsbclilib, 'DsBackupGetDatabaseNames' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsBackupGetDatabaseNames]
  end;
end;

var
  _DsBackupOpenFileA: Pointer;

function DsBackupOpenFileA;
begin
  GetProcedureAddress(_DsBackupOpenFileA, ntdsbclilib, 'DsBackupOpenFileA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsBackupOpenFileA]
  end;
end;

var
  _DsBackupOpenFileW: Pointer;

function DsBackupOpenFileW;
begin
  GetProcedureAddress(_DsBackupOpenFileW, ntdsbclilib, 'DsBackupOpenFileW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsBackupOpenFileW]
  end;
end;

var
  _DsBackupOpenFile: Pointer;

function DsBackupOpenFile;
begin
  GetProcedureAddress(_DsBackupOpenFile, ntdsbclilib, 'DsBackupOpenFile' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsBackupOpenFile]
  end;
end;

var
  _DsBackupRead: Pointer;

function DsBackupRead;
begin
  GetProcedureAddress(_DsBackupRead, ntdsbclilib, 'DsBackupRead');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsBackupRead]
  end;
end;

var
  _DsBackupClose: Pointer;

function DsBackupClose;
begin
  GetProcedureAddress(_DsBackupClose, ntdsbclilib, 'DsBackupClose');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsBackupClose]
  end;
end;

var
  _DsBackupGetBackupLogsA: Pointer;

function DsBackupGetBackupLogsA;
begin
  GetProcedureAddress(_DsBackupGetBackupLogsA, ntdsbclilib, 'DsBackupGetBackupLogsA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsBackupGetBackupLogsA]
  end;
end;

var
  _DsBackupGetBackupLogsW: Pointer;

function DsBackupGetBackupLogsW;
begin
  GetProcedureAddress(_DsBackupGetBackupLogsW, ntdsbclilib, 'DsBackupGetBackupLogsW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsBackupGetBackupLogsW]
  end;
end;

var
  _DsBackupGetBackupLogs: Pointer;

function DsBackupGetBackupLogs;
begin
  GetProcedureAddress(_DsBackupGetBackupLogs, ntdsbclilib, 'DsBackupGetBackupLogs' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsBackupGetBackupLogs]
  end;
end;

var
  _DsBackupTruncateLogs: Pointer;

function DsBackupTruncateLogs;
begin
  GetProcedureAddress(_DsBackupTruncateLogs, ntdsbclilib, 'DsBackupTruncateLogs');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsBackupTruncateLogs]
  end;
end;

var
  _DsBackupEnd: Pointer;

function DsBackupEnd;
begin
  GetProcedureAddress(_DsBackupEnd, ntdsbclilib, 'DsBackupEnd');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsBackupEnd]
  end;
end;

var
  _DsBackupFree: Pointer;

procedure DsBackupFree;
begin
  GetProcedureAddress(_DsBackupFree, ntdsbclilib, 'DsBackupFree');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsBackupFree]
  end;
end;

var
  _DsRestoreGetDatabaseLocationsA: Pointer;

function DsRestoreGetDatabaseLocationsA;
begin
  GetProcedureAddress(_DsRestoreGetDatabaseLocationsA, ntdsbclilib, 'DsRestoreGetDatabaseLocationsA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsRestoreGetDatabaseLocationsA]
  end;
end;

var
  _DsRestoreGetDatabaseLocationsW: Pointer;

function DsRestoreGetDatabaseLocationsW;
begin
  GetProcedureAddress(_DsRestoreGetDatabaseLocationsW, ntdsbclilib, 'DsRestoreGetDatabaseLocationsW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsRestoreGetDatabaseLocationsW]
  end;
end;

var
  _DsRestoreGetDatabaseLocations: Pointer;

function DsRestoreGetDatabaseLocations;
begin
  GetProcedureAddress(_DsRestoreGetDatabaseLocations, ntdsbclilib, 'DsRestoreGetDatabaseLocations' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsRestoreGetDatabaseLocations]
  end;
end;

var
  _DsRestorePrepareA: Pointer;

function DsRestorePrepareA;
begin
  GetProcedureAddress(_DsRestorePrepareA, ntdsbclilib, 'DsRestorePrepareA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsRestorePrepareA]
  end;
end;

var
  _DsRestorePrepareW: Pointer;

function DsRestorePrepareW;
begin
  GetProcedureAddress(_DsRestorePrepareW, ntdsbclilib, 'DsRestorePrepareW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsRestorePrepareW]
  end;
end;

var
  _DsRestorePrepare: Pointer;

function DsRestorePrepare;
begin
  GetProcedureAddress(_DsRestorePrepare, ntdsbclilib, 'DsRestorePrepare' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsRestorePrepare]
  end;
end;

var
  _DsRestoreRegisterA: Pointer;

function DsRestoreRegisterA;
begin
  GetProcedureAddress(_DsRestoreRegisterA, ntdsbclilib, 'DsRestoreRegisterA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsRestoreRegisterA]
  end;
end;

var
  _DsRestoreRegisterW: Pointer;

function DsRestoreRegisterW;
begin
  GetProcedureAddress(_DsRestoreRegisterW, ntdsbclilib, 'DsRestoreRegisterW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsRestoreRegisterW]
  end;
end;

var
  _DsRestoreRegister: Pointer;

function DsRestoreRegister;
begin
  GetProcedureAddress(_DsRestoreRegister, ntdsbclilib, 'DsRestoreRegister' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsRestoreRegister]
  end;
end;

var
  _DsRestoreRegisterComplete: Pointer;

function DsRestoreRegisterComplete;
begin
  GetProcedureAddress(_DsRestoreRegisterComplete, ntdsbclilib, 'DsRestoreRegisterComplete');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsRestoreRegisterComplete]
  end;
end;

var
  _DsRestoreEnd: Pointer;

function DsRestoreEnd;
begin
  GetProcedureAddress(_DsRestoreEnd, ntdsbclilib, 'DsRestoreEnd');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsRestoreEnd]
  end;
end;

var
  _DsSetCurrentBackupLogA: Pointer;

function DsSetCurrentBackupLogA;
begin
  GetProcedureAddress(_DsSetCurrentBackupLogA, ntdsbclilib, 'DsSetCurrentBackupLogA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsSetCurrentBackupLogA]
  end;
end;

var
  _DsSetCurrentBackupLogW: Pointer;

function DsSetCurrentBackupLogW;
begin
  GetProcedureAddress(_DsSetCurrentBackupLogW, ntdsbclilib, 'DsSetCurrentBackupLogW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsSetCurrentBackupLogW]
  end;
end;

var
  _DsSetCurrentBackupLog: Pointer;

function DsSetCurrentBackupLog;
begin
  GetProcedureAddress(_DsSetCurrentBackupLog, ntdsbclilib, 'DsSetCurrentBackupLog' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsSetCurrentBackupLog]
  end;
end;

var
  _DsSetAuthIdentityA: Pointer;

function DsSetAuthIdentityA;
begin
  GetProcedureAddress(_DsSetAuthIdentityA, ntdsbclilib, 'DsSetAuthIdentityA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsSetAuthIdentityA]
  end;
end;

var
  _DsSetAuthIdentityW: Pointer;

function DsSetAuthIdentityW;
begin
  GetProcedureAddress(_DsSetAuthIdentityW, ntdsbclilib, 'DsSetAuthIdentityW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsSetAuthIdentityW]
  end;
end;

var
  _DsSetAuthIdentity: Pointer;

function DsSetAuthIdentity;
begin
  GetProcedureAddress(_DsSetAuthIdentity, ntdsbclilib, 'DsSetAuthIdentity' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsSetAuthIdentity]
  end;
end;

{$ELSE}

function DsIsNTDSOnlineA; external ntdsbclilib name 'DsIsNTDSOnlineA';
function DsIsNTDSOnlineW; external ntdsbclilib name 'DsIsNTDSOnlineW';
function DsIsNTDSOnline; external ntdsbclilib name 'DsIsNTDSOnline' + AWSuffix;
function DsBackupPrepareA; external ntdsbclilib name 'DsBackupPrepareA';
function DsBackupPrepareW; external ntdsbclilib name 'DsBackupPrepareW';
function DsBackupPrepare; external ntdsbclilib name 'DsBackupPrepare' + AWSuffix;
function DsBackupGetDatabaseNamesA; external ntdsbclilib name 'DsBackupGetDatabaseNamesA';
function DsBackupGetDatabaseNamesW; external ntdsbclilib name 'DsBackupGetDatabaseNamesW';
function DsBackupGetDatabaseNames; external ntdsbclilib name 'DsBackupGetDatabaseNames' + AWSuffix;
function DsBackupOpenFileA; external ntdsbclilib name 'DsBackupOpenFileA';
function DsBackupOpenFileW; external ntdsbclilib name 'DsBackupOpenFileW';
function DsBackupOpenFile; external ntdsbclilib name 'DsBackupOpenFile' + AWSuffix;
function DsBackupRead; external ntdsbclilib name 'DsBackupRead';
function DsBackupClose; external ntdsbclilib name 'DsBackupClose';
function DsBackupGetBackupLogsA; external ntdsbclilib name 'DsBackupGetBackupLogsA';
function DsBackupGetBackupLogsW; external ntdsbclilib name 'DsBackupGetBackupLogsW';
function DsBackupGetBackupLogs; external ntdsbclilib name 'DsBackupGetBackupLogs' + AWSuffix;
function DsBackupTruncateLogs; external ntdsbclilib name 'DsBackupTruncateLogs';
function DsBackupEnd; external ntdsbclilib name 'DsBackupEnd';
procedure DsBackupFree; external ntdsbclilib name 'DsBackupFree';
function DsRestoreGetDatabaseLocationsA; external ntdsbclilib name 'DsRestoreGetDatabaseLocationsA';
function DsRestoreGetDatabaseLocationsW; external ntdsbclilib name 'DsRestoreGetDatabaseLocationsW';
function DsRestoreGetDatabaseLocations; external ntdsbclilib name 'DsRestoreGetDatabaseLocations' + AWSuffix;
function DsRestorePrepareA; external ntdsbclilib name 'DsRestorePrepareA';
function DsRestorePrepareW; external ntdsbclilib name 'DsRestorePrepareW';
function DsRestorePrepare; external ntdsbclilib name 'DsRestorePrepare' + AWSuffix;
function DsRestoreRegisterA; external ntdsbclilib name 'DsRestoreRegisterA';
function DsRestoreRegisterW; external ntdsbclilib name 'DsRestoreRegisterW';
function DsRestoreRegister; external ntdsbclilib name 'DsRestoreRegister' + AWSuffix;
function DsRestoreRegisterComplete; external ntdsbclilib name 'DsRestoreRegisterComplete';
function DsRestoreEnd; external ntdsbclilib name 'DsRestoreEnd';
function DsSetCurrentBackupLogA; external ntdsbclilib name 'DsSetCurrentBackupLogA';
function DsSetCurrentBackupLogW; external ntdsbclilib name 'DsSetCurrentBackupLogW';
function DsSetCurrentBackupLog; external ntdsbclilib name 'DsSetCurrentBackupLog' + AWSuffix;
function DsSetAuthIdentityA; external ntdsbclilib name 'DsSetAuthIdentityA';
function DsSetAuthIdentityW; external ntdsbclilib name 'DsSetAuthIdentityW';
function DsSetAuthIdentity; external ntdsbclilib name 'DsSetAuthIdentity' + AWSuffix;

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}


{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
