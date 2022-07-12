{******************************************************************************}
{                                                                              }
{ Windows System Restore API interface Unit for Object Pascal                  }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: srrestoreptapi.h, released Match 2003. The original    }
{ Pascal code is: SrRestorePtApi.pas, released December 2000.                  }
{ The initial developer of the Pascal code is Marcel van Brakel                }
{ (brakelm att chello dott nl).                                                }
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

// $Id: JwaSrRestorePtApi.pas,v 1.9 2007/09/05 11:58:52 dezipaitor Exp $
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaSrRestorePtApi;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "SrRestorePtApi.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaWinNT, JwaWinType;
{$ENDIF JWA_OMIT_SECTIONS}
{$IFNDEF JWA_IMPLEMENTATIONSECTION}

//
// Type of Event
//

const
  MIN_EVENT                        = 100;
  {$EXTERNALSYM MIN_EVENT}
  BEGIN_SYSTEM_CHANGE              = 100;
  {$EXTERNALSYM BEGIN_SYSTEM_CHANGE}
  END_SYSTEM_CHANGE                = 101;
  {$EXTERNALSYM END_SYSTEM_CHANGE}
  BEGIN_NESTED_SYSTEM_CHANGE       = 102; // for Whistler only - use this to prevent nested restore pts
  {$EXTERNALSYM BEGIN_NESTED_SYSTEM_CHANGE}
  END_NESTED_SYSTEM_CHANGE         = 103; // for Whistler only - use this to prevent nested restore pts
  {$EXTERNALSYM END_NESTED_SYSTEM_CHANGE}
  MAX_EVENT                        = 103;
  {$EXTERNALSYM MAX_EVENT}

//
// Type of Restore Points
//

  MIN_RPT                = 0;
  {$EXTERNALSYM MIN_RPT}
  APPLICATION_INSTALL    = 0;
  {$EXTERNALSYM APPLICATION_INSTALL}
  APPLICATION_UNINSTALL  = 1;
  {$EXTERNALSYM APPLICATION_UNINSTALL}
  DESKTOP_SETTING        = 2;    // Not implemented
  {$EXTERNALSYM DESKTOP_SETTING}
  ACCESSIBILITY_SETTING  = 3;    // Not implemented
  {$EXTERNALSYM ACCESSIBILITY_SETTING}
  OE_SETTING             = 4;    // Not implemented
  {$EXTERNALSYM OE_SETTING}
  APPLICATION_RUN        = 5;    // Not implemented
  {$EXTERNALSYM APPLICATION_RUN}
  RESTORE                = 6;
  {$EXTERNALSYM RESTORE}
  CHECKPOINT             = 7;
  {$EXTERNALSYM CHECKPOINT}
  WINDOWS_SHUTDOWN       = 8;    // Not implemented
  {$EXTERNALSYM WINDOWS_SHUTDOWN}
  WINDOWS_BOOT           = 9;    // Not implemented
  {$EXTERNALSYM WINDOWS_BOOT}
  DEVICE_DRIVER_INSTALL  = 10;
  {$EXTERNALSYM DEVICE_DRIVER_INSTALL}
  FIRSTRUN               = 11;
  {$EXTERNALSYM FIRSTRUN}
  MODIFY_SETTINGS        = 12;
  {$EXTERNALSYM MODIFY_SETTINGS}
  CANCELLED_OPERATION    = 13;   // Only valid for END_SYSTEM_CHANGE
  {$EXTERNALSYM CANCELLED_OPERATION}
  BACKUP_RECOVERY        = 14;
  {$EXTERNALSYM BACKUP_RECOVERY}
  MAX_RPT                = 14;
  {$EXTERNALSYM MAX_RPT}

  MAX_DESC               = 64;
  {$EXTERNALSYM MAX_DESC}
  MAX_DESC_W             = 256;  // longer for Whistler
  {$EXTERNALSYM MAX_DESC_W}

//
// for Millennium compatibility
//

//#pragma pack(push, srrestoreptapi_include)
//#pragma pack(1)

//
// Restore point information
//

type
  _RESTOREPTINFOA = packed record
    dwEventType: DWORD;                // Type of Event - Begin or End
    dwRestorePtType: DWORD;            // Type of Restore Point - App install/uninstall
    llSequenceNumber: Int64;           // Sequence Number - 0 for begin
    szDescription: array [0..MAX_DESC - 1] of AnsiChar;    // Description - Name of Application / Operation
  end;
  {$EXTERNALSYM _RESTOREPTINFOA}
  RESTOREPOINTINFOA = _RESTOREPTINFOA;
  {$EXTERNALSYM RESTOREPOINTINFOA}
  PRESTOREPOINTINFOA = ^RESTOREPOINTINFOA;
  {$EXTERNALSYM PRESTOREPOINTINFOA}
  TRestorePointInfoA = RESTOREPOINTINFOA;

  _RESTOREPTINFOW = packed record
    dwEventType: DWORD;                // Type of Event - Begin or End
    dwRestorePtType: DWORD;            // Type of Restore Point - App install/uninstall
    llSequenceNumber: Int64;           // Sequence Number - 0 for begin
    szDescription: array [0..MAX_DESC_W - 1] of WideChar;    // Description - Name of Application / Operation
  end;
  {$EXTERNALSYM _RESTOREPTINFOW}
  RESTOREPOINTINFOW = _RESTOREPTINFOW;
  {$EXTERNALSYM RESTOREPOINTINFOW}
  PRESTOREPOINTINFOW = ^RESTOREPOINTINFOW;
  {$EXTERNALSYM PRESTOREPOINTINFOW}
  TRestorePointInfoW = RESTOREPOINTINFOW;
  {$IFDEF UNICODE}
  RESTOREPOINTINFO = RESTOREPOINTINFOW;
  {$EXTERNALSYM RESTOREPOINTINFO}
  PRESTOREPOINTINFO = PRESTOREPOINTINFOW;
  {$EXTERNALSYM PRESTOREPOINTINFO}
  TRestorePointInfo = TRestorePointInfoW;
  {$ELSE}
  RESTOREPOINTINFO = RESTOREPOINTINFOA;
  {$EXTERNALSYM RESTOREPOINTINFO}
  PRESTOREPOINTINFO = PRESTOREPOINTINFOA;
  {$EXTERNALSYM PRESTOREPOINTINFO}
  TRestorePointInfo = TRestorePointInfoA;
  {$ENDIF UNICODE}

//
// Status returned by System Restore
//

  _SMGRSTATUS =  packed record
    nStatus: DWORD;            // Status returned by State Manager Process
    llSequenceNumber: Int64;   // Sequence Number for the restore point
  end;
  {$EXTERNALSYM _SMGRSTATUS}
  STATEMGRSTATUS = _SMGRSTATUS;
  {$EXTERNALSYM STATEMGRSTATUS}
  PSTATEMGRSTATUS = ^STATEMGRSTATUS;
  {$EXTERNALSYM PSTATEMGRSTATUS}
  TSMgrStatus = STATEMGRSTATUS;

//#pragma pack(pop, srrestoreptapi_include)

//
// RPC call to set a restore point
//
// Return value  TRUE if the call was a success
//               FALSE if the call failed
//
// If pSmgrStatus nStatus field is set as follows
//
// ERROR_SUCCESS              If the call succeeded (return value will be TRUE)
//
// ERROR_TIMEOUT              If the call timed out due to a wait on a mutex for
//                            for setting restore points.
//
// ERROR_INVALID_DATA         If the cancel restore point is called with an invalid
//                            sequence number
//
// ERROR_INTERNAL_ERROR       If there are internal failures.
//
// ERROR_BAD_ENVIRONMENT      If the API is called in SafeMode
//
// ERROR_SERVICE_DISABLED     If SystemRestore is Disabled.
//
// ERROR_DISK_FULL            If System Restore is frozen (Windows Whistler only)
//
// ERROR_ALREADY_EXISTS       If this is a nested restore point

function SRSetRestorePointA(pRestorePtSpec: PRESTOREPOINTINFOA; pSMgrStatus: PSTATEMGRSTATUS): BOOL; stdcall;
{$EXTERNALSYM SRSetRestorePointA}
function SRSetRestorePointW(pRestorePtSpec: PRESTOREPOINTINFOW; pSMgrStatus: PSTATEMGRSTATUS): BOOL; stdcall;
{$EXTERNALSYM SRSetRestorePointW}
function SRRemoveRestorePoint(dwRPNum: DWORD): DWORD; stdcall;
{$EXTERNALSYM SRRemoveRestorePoint}
function SRSetRestorePoint(pRestorePtSpec: PRESTOREPOINTINFO; pSMgrStatus: PSTATEMGRSTATUS): BOOL; stdcall;
{$EXTERNALSYM SRSetRestorePoint}

{$ENDIF JWA_IMPLEMENTATIONSECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  srclient = 'srclient.dll';
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}

var
  _SRSetRestorePointA: Pointer;

function SRSetRestorePointA;
begin
  GetProcedureAddress(_SRSetRestorePointA, srclient, 'SRSetRestorePointA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SRSetRestorePointA]
  end;
end;

var
  _SRSetRestorePointW: Pointer;

function SRSetRestorePointW;
begin
  GetProcedureAddress(_SRSetRestorePointW, srclient, 'SRSetRestorePointW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SRSetRestorePointW]
  end;
end;

var
  _SRSetRestorePoint: Pointer;

function SRSetRestorePoint;
begin
  GetProcedureAddress(_SRSetRestorePoint, srclient, 'SRSetRestorePoint' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SRSetRestorePoint]
  end;
end;

var
  _SRRemoveRestorePoint: Pointer;

function SRRemoveRestorePoint;
begin
  GetProcedureAddress(_SRRemoveRestorePoint, srclient, 'SRRemoveRestorePoint');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SRRemoveRestorePoint]
  end;
end;

{$ELSE}

function SRSetRestorePointA; external srclient name 'SRSetRestorePointA';
function SRSetRestorePointW; external srclient name 'SRSetRestorePointW';
function SRSetRestorePoint; external srclient name 'SRSetRestorePointW' + AWSuffix;
function SRRemoveRestorePoint; external srclient name 'SRRemoveRestorePoint';

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}

