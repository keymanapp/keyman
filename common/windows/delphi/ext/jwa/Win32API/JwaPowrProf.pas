{******************************************************************************}
{                                                                              }
{ Power Policy Applicator interface Unit for Object Pascal                     }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: powrprof.h, released June 2000. The original Pascal    }
{ code is: PowrProf.pas, released August 2001. The initial developer of the    }
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

// $Id: JwaPowrProf.pas,v 1.12 2007/09/05 11:58:52 dezipaitor Exp $
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaPowrProf;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "powrprof.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaWinNT, JwaWinType;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}

// Registry storage structures for the GLOBAL_POWER_POLICY data. There are two
// structures, GLOBAL_MACHINE_POWER_POLICY and GLOBAL_USER_POWER_POLICY. the
// GLOBAL_MACHINE_POWER_POLICY stores per machine data for which there is no UI.
// GLOBAL_USER_POWER_POLICY stores the per user data.

type
  PGLOBAL_MACHINE_POWER_POLICY = ^GLOBAL_MACHINE_POWER_POLICY;
  {$EXTERNALSYM PGLOBAL_MACHINE_POWER_POLICY}
  _GLOBAL_MACHINE_POWER_POLICY = record
    Revision: ULONG;
    LidOpenWakeAc: SYSTEM_POWER_STATE;
    LidOpenWakeDc: SYSTEM_POWER_STATE;
    BroadcastCapacityResolution: ULONG;
  end;
  {$EXTERNALSYM _GLOBAL_MACHINE_POWER_POLICY}
  GLOBAL_MACHINE_POWER_POLICY = _GLOBAL_MACHINE_POWER_POLICY;
  {$EXTERNALSYM GLOBAL_MACHINE_POWER_POLICY}
  TGlobalMachinePowerPolicy = GLOBAL_MACHINE_POWER_POLICY;
  PGlobalMachinePowerPolicy = PGLOBAL_MACHINE_POWER_POLICY;

  PGLOBAL_USER_POWER_POLICY = ^GLOBAL_USER_POWER_POLICY;
  {$EXTERNALSYM PGLOBAL_USER_POWER_POLICY}
  _GLOBAL_USER_POWER_POLICY = record
    Revision: ULONG;
    PowerButtonAc: POWER_ACTION_POLICY;
    PowerButtonDc: POWER_ACTION_POLICY;
    SleepButtonAc: POWER_ACTION_POLICY;
    SleepButtonDc: POWER_ACTION_POLICY;
    LidCloseAc: POWER_ACTION_POLICY;
    LidCloseDc: POWER_ACTION_POLICY;
    DischargePolicy: array [0..NUM_DISCHARGE_POLICIES - 1] of SYSTEM_POWER_LEVEL;
    GlobalFlags: ULONG;
  end;
  {$EXTERNALSYM _GLOBAL_USER_POWER_POLICY}
  GLOBAL_USER_POWER_POLICY = _GLOBAL_USER_POWER_POLICY;
  {$EXTERNALSYM GLOBAL_USER_POWER_POLICY}
  TGlobalUserPowerPolicy = GLOBAL_USER_POWER_POLICY;
  PGlobalUserPowerPolicy = PGLOBAL_USER_POWER_POLICY;

// Structure to manage global power policies at the user level. This structure
// contains data which is common across all power policy profiles.

  PGLOBAL_POWER_POLICY = ^GLOBAL_POWER_POLICY;
  {$EXTERNALSYM PGLOBAL_POWER_POLICY}
  _GLOBAL_POWER_POLICY = record
    user: GLOBAL_USER_POWER_POLICY;
    mach: GLOBAL_MACHINE_POWER_POLICY;
  end;
  {$EXTERNALSYM _GLOBAL_POWER_POLICY}
  GLOBAL_POWER_POLICY = _GLOBAL_POWER_POLICY;
  {$EXTERNALSYM GLOBAL_POWER_POLICY}
  TGlobalPowerPolicy = GLOBAL_POWER_POLICY;
  PGlobalPowerPolicy = PGLOBAL_POWER_POLICY;

// Registry storage structures for the POWER_POLICY data. There are three
// structures, MACHINE_POWER_POLICY, MACHINE_PROCESSOR_POWER_POLICY and USER_POWER_POLICY. the
// MACHINE_POWER_POLICY stores per machine data for which there is no UI.
// USER_POWER_POLICY stores the per user data.

  PMACHINE_POWER_POLICY = ^MACHINE_POWER_POLICY;
  {$EXTERNALSYM PMACHINE_POWER_POLICY}
  _MACHINE_POWER_POLICY = record
    Revision: ULONG; // 1
    // meaning of power action "sleep"
    MinSleepAc: SYSTEM_POWER_STATE;
    MinSleepDc: SYSTEM_POWER_STATE;
    ReducedLatencySleepAc: SYSTEM_POWER_STATE;
    ReducedLatencySleepDc: SYSTEM_POWER_STATE;
    // parameters for dozing
    DozeTimeoutAc: ULONG;
    DozeTimeoutDc: ULONG;
    DozeS4TimeoutAc: ULONG;
    DozeS4TimeoutDc: ULONG;
    // processor policies
    MinThrottleAc: UCHAR;
    MinThrottleDc: UCHAR;
    pad1: array [0..1] of UCHAR;
    OverThrottledAc: POWER_ACTION_POLICY;
    OverThrottledDc: POWER_ACTION_POLICY;
  end;
  {$EXTERNALSYM _MACHINE_POWER_POLICY}
  MACHINE_POWER_POLICY = _MACHINE_POWER_POLICY;
  {$EXTERNALSYM MACHINE_POWER_POLICY}
  TMachinePowerPolicy = MACHINE_POWER_POLICY;
  PMachinePowerPolicy = PMACHINE_POWER_POLICY;

  PMACHINE_PROCESSOR_POWER_POLICY = ^MACHINE_PROCESSOR_POWER_POLICY;
  {$EXTERNALSYM PMACHINE_PROCESSOR_POWER_POLICY}
  _MACHINE_PROCESSOR_POWER_POLICY = record
    Revision: ULONG; // 1
    ProcessorPolicyAc: PROCESSOR_POWER_POLICY;
    ProcessorPolicyDc: PROCESSOR_POWER_POLICY;
  end;
  {$EXTERNALSYM _MACHINE_PROCESSOR_POWER_POLICY}
  MACHINE_PROCESSOR_POWER_POLICY = _MACHINE_PROCESSOR_POWER_POLICY;
  {$EXTERNALSYM MACHINE_PROCESSOR_POWER_POLICY}
  TMachineProcessorPowerPolicy = MACHINE_PROCESSOR_POWER_POLICY;
  PMachineProcessorPowerPolicy = PMACHINE_PROCESSOR_POWER_POLICY;

  PUSER_POWER_POLICY = ^USER_POWER_POLICY;
  {$EXTERNALSYM PUSER_POWER_POLICY}
  _USER_POWER_POLICY = record
    Revision: ULONG; // 1
    // "system idle" detection
    IdleAc: POWER_ACTION_POLICY;
    IdleDc: POWER_ACTION_POLICY;
    IdleTimeoutAc: ULONG;
    IdleTimeoutDc: ULONG;
    IdleSensitivityAc: UCHAR;
    IdleSensitivityDc: UCHAR;
    // Throttling Policy
    ThrottlePolicyAc: UCHAR;
    ThrottlePolicyDc: UCHAR;
    // meaning of power action "sleep"
    MaxSleepAc: SYSTEM_POWER_STATE;
    MaxSleepDc: SYSTEM_POWER_STATE;
    // For future use
    Reserved: array [0..1] of ULONG;
    // video policies
    VideoTimeoutAc: ULONG;
    VideoTimeoutDc: ULONG;
    // hard disk policies
    SpindownTimeoutAc: ULONG;
    SpindownTimeoutDc: ULONG;
    // processor policies
    OptimizeForPowerAc: ByteBool;
    OptimizeForPowerDc: ByteBool;
    FanThrottleToleranceAc: UCHAR;
    FanThrottleToleranceDc: UCHAR;
    ForcedThrottleAc: UCHAR;
    ForcedThrottleDc: UCHAR;
  end;
  {$EXTERNALSYM _USER_POWER_POLICY}
  USER_POWER_POLICY = _USER_POWER_POLICY;
  {$EXTERNALSYM USER_POWER_POLICY}
  TUserPowerPolicy = USER_POWER_POLICY;
  PUserPowerPolicy = PUSER_POWER_POLICY;

// Structure to manage power policies at the user level. This structure
// contains data which is unique across power policy profiles.

  PPOWER_POLICY = ^POWER_POLICY;
  {$EXTERNALSYM PPOWER_POLICY}
  _POWER_POLICY = record
    user: USER_POWER_POLICY;
    mach: MACHINE_POWER_POLICY;
  end;
  {$EXTERNALSYM _POWER_POLICY}
  POWER_POLICY = _POWER_POLICY;
  {$EXTERNALSYM POWER_POLICY}
  TPowerPolicy = POWER_POLICY;
  PPowerPolicy = PPOWER_POLICY;

// Constants for GlobalFlags

const
  EnableSysTrayBatteryMeter = $01;
  {$EXTERNALSYM EnableSysTrayBatteryMeter}
  EnableMultiBatteryDisplay = $02;
  {$EXTERNALSYM EnableMultiBatteryDisplay}
  EnablePasswordLogon       = $04;
  {$EXTERNALSYM EnablePasswordLogon}
  EnableWakeOnRing          = $08;
  {$EXTERNALSYM EnableWakeOnRing}
  EnableVideoDimDisplay     = $10;
  {$EXTERNALSYM EnableVideoDimDisplay}

// This constant is passed as a uiID to WritePwrScheme.

  NEWSCHEME = UINT(-1);
  {$EXTERNALSYM NEWSCHEME}

// Prototype for EnumPwrSchemes callback proceedures.

type
  PWRSCHEMESENUMPROC = function(uiIndex: UINT; dwName: DWORD; sName: LPWSTR; dwDesc: DWORD; sDesc: LPWSTR; pp: PPOWER_POLICY; lParam: LPARAM): ByteBool; stdcall;
  {$EXTERNALSYM PWRSCHEMESENUMPROC}
  PFNNTINITIATEPWRACTION = function(pPowerAction: POWER_ACTION; SystemPowerState: SYSTEM_POWER_STATE; u: ULONG; b: ByteBool): ByteBool; stdcall;
  {$EXTERNALSYM PFNNTINITIATEPWRACTION}

// Public function prototypes

function GetPwrDiskSpindownRange(var RangeMax, RangeMin: UINT): ByteBool; stdcall;
{$EXTERNALSYM GetPwrDiskSpindownRange}
function EnumPwrSchemes(lpfnPwrSchemesEnumProc: PWRSCHEMESENUMPROC; lParam: LPARAM): ByteBool; stdcall;
{$EXTERNALSYM EnumPwrSchemes}
function ReadGlobalPwrPolicy(var pGlobalPowerPolicy: GLOBAL_POWER_POLICY): ByteBool; stdcall;
{$EXTERNALSYM ReadGlobalPwrPolicy}
function ReadPwrScheme(uiID: UINT; var pPowerPolicy: POWER_POLICY): ByteBool; stdcall;
{$EXTERNALSYM ReadPwrScheme}
function WritePwrScheme(puiID: PUINT; lpszName, lpszDescription: LPWSTR; const pPowerPolicy: POWER_POLICY): ByteBool; stdcall;
{$EXTERNALSYM WritePwrScheme}
function WriteGlobalPwrPolicy(const pGlobalPowerPolicy: GLOBAL_POWER_POLICY): ByteBool; stdcall;
{$EXTERNALSYM WriteGlobalPwrPolicy}
function DeletePwrScheme(uiIndex: UINT): ByteBool; stdcall;
{$EXTERNALSYM DeletePwrScheme}
function GetActivePwrScheme(var puiID: UINT): ByteBool; stdcall;
{$EXTERNALSYM GetActivePwrScheme}
function SetActivePwrScheme(uiID: UINT; pGlobalPowerPolicy: PGLOBAL_POWER_POLICY; pPowerPolicy: PPOWER_POLICY): ByteBool; stdcall;
{$EXTERNALSYM SetActivePwrScheme}
function GetPwrCapabilities(var lpSystemPowerCapabilities: SYSTEM_POWER_CAPABILITIES): ByteBool; stdcall;
{$EXTERNALSYM GetPwrCapabilities}
function IsPwrSuspendAllowed: ByteBool; stdcall;
{$EXTERNALSYM IsPwrSuspendAllowed}
function IsPwrHibernateAllowed: ByteBool; stdcall;
{$EXTERNALSYM IsPwrHibernateAllowed}
function IsPwrShutdownAllowed: ByteBool; stdcall;
{$EXTERNALSYM IsPwrShutdownAllowed}
function IsAdminOverrideActive(pAdministratorPowerPolicy: PADMINISTRATOR_POWER_POLICY): ByteBool; stdcall;
{$EXTERNALSYM IsAdminOverrideActive}
function SetSuspendState(Hibernate, ForceCritical, DisableWakeEvent: ByteBool): ByteBool; stdcall;
{$EXTERNALSYM SetSuspendState}
function GetCurrentPowerPolicies(pGlobalPowerPolicy: PGLOBAL_POWER_POLICY; pPowerPolicy: PPOWER_POLICY): ByteBool; stdcall;
{$EXTERNALSYM GetCurrentPowerPolicies}
function CanUserWritePwrScheme: ByteBool; stdcall;
{$EXTERNALSYM CanUserWritePwrScheme}
function ReadProcessorPwrScheme(uiID: UINT; var pMachineProcessorPowerPolicy: MACHINE_PROCESSOR_POWER_POLICY): ByteBool; stdcall;
{$EXTERNALSYM ReadProcessorPwrScheme}
function WriteProcessorPwrScheme(uiID: UINT; const pMachineProcessorPowerPolicy: MACHINE_PROCESSOR_POWER_POLICY): ByteBool; stdcall;
{$EXTERNALSYM WriteProcessorPwrScheme}
function ValidatePowerPolicies(GlobalPolicy: PGLOBAL_POWER_POLICY; Policy: PPOWER_POLICY): ByteBool; stdcall;
{$EXTERNALSYM ValidatePowerPolicies}

function CallNtPowerInformation(InformationLeveL: POWER_INFORMATION_LEVEL; lpInputBuffer: PVOID; nInputBufferSize: ULONG; lpOutputBuffer: PVOID; nOutputBufferSize: ULONG): NTSTATUS; stdcall;
{$EXTERNALSYM CallNtPowerInformation}

{$ENDIF JWA_IMPLEMENTATIONSECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  powrproflib = 'powrprof.dll';
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}

var
  _GetPwrDiskSpindownRange: Pointer;

function GetPwrDiskSpindownRange;
begin
  GetProcedureAddress(_GetPwrDiskSpindownRange, powrproflib, 'GetPwrDiskSpindownRange');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetPwrDiskSpindownRange]
  end;
end;

var
  _EnumPwrSchemes: Pointer;

function EnumPwrSchemes;
begin
  GetProcedureAddress(_EnumPwrSchemes, powrproflib, 'EnumPwrSchemes');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EnumPwrSchemes]
  end;
end;

var
  _ReadGlobalPwrPolicy: Pointer;

function ReadGlobalPwrPolicy;
begin
  GetProcedureAddress(_ReadGlobalPwrPolicy, powrproflib, 'ReadGlobalPwrPolicy');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ReadGlobalPwrPolicy]
  end;
end;

var
  _ReadPwrScheme: Pointer;

function ReadPwrScheme;
begin
  GetProcedureAddress(_ReadPwrScheme, powrproflib, 'ReadPwrScheme');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ReadPwrScheme]
  end;
end;

var
  _WritePwrScheme: Pointer;

function WritePwrScheme;
begin
  GetProcedureAddress(_WritePwrScheme, powrproflib, 'WritePwrScheme');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WritePwrScheme]
  end;
end;

var
  _WriteGlobalPwrPolicy: Pointer;

function WriteGlobalPwrPolicy;
begin
  GetProcedureAddress(_WriteGlobalPwrPolicy, powrproflib, 'WriteGlobalPwrPolicy');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WriteGlobalPwrPolicy]
  end;
end;

var
  _DeletePwrScheme: Pointer;

function DeletePwrScheme;
begin
  GetProcedureAddress(_DeletePwrScheme, powrproflib, 'DeletePwrScheme');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DeletePwrScheme]
  end;
end;

var
  _GetActivePwrScheme: Pointer;

function GetActivePwrScheme;
begin
  GetProcedureAddress(_GetActivePwrScheme, powrproflib, 'GetActivePwrScheme');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetActivePwrScheme]
  end;
end;

var
  _SetActivePwrScheme: Pointer;

function SetActivePwrScheme;
begin
  GetProcedureAddress(_SetActivePwrScheme, powrproflib, 'SetActivePwrScheme');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetActivePwrScheme]
  end;
end;

var
  _GetPwrCapabilities: Pointer;

function GetPwrCapabilities;
begin
  GetProcedureAddress(_GetPwrCapabilities, powrproflib, 'GetPwrCapabilities');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetPwrCapabilities]
  end;
end;

var
  _IsPwrSuspendAllowed: Pointer;

function IsPwrSuspendAllowed;
begin
  GetProcedureAddress(_IsPwrSuspendAllowed, powrproflib, 'IsPwrSuspendAllowed');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_IsPwrSuspendAllowed]
  end;
end;

var
  _IsPwrHibernateAllowed: Pointer;

function IsPwrHibernateAllowed;
begin
  GetProcedureAddress(_IsPwrHibernateAllowed, powrproflib, 'IsPwrHibernateAllowed');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_IsPwrHibernateAllowed]
  end;
end;

var
  _IsPwrShutdownAllowed: Pointer;

function IsPwrShutdownAllowed;
begin
  GetProcedureAddress(_IsPwrShutdownAllowed, powrproflib, 'IsPwrShutdownAllowed');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_IsPwrShutdownAllowed]
  end;
end;

var
  _IsAdminOverrideActive: Pointer;

function IsAdminOverrideActive;
begin
  GetProcedureAddress(_IsAdminOverrideActive, powrproflib, 'IsAdminOverrideActive');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_IsAdminOverrideActive]
  end;
end;

var
  _SetSuspendState: Pointer;

function SetSuspendState;
begin
  GetProcedureAddress(_SetSuspendState, powrproflib, 'SetSuspendState');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetSuspendState]
  end;
end;

var
  _GetCurrentPowerPolicies: Pointer;

function GetCurrentPowerPolicies;
begin
  GetProcedureAddress(_GetCurrentPowerPolicies, powrproflib, 'GetCurrentPowerPolicies');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetCurrentPowerPolicies]
  end;
end;

var
  _CanUserWritePwrScheme: Pointer;

function CanUserWritePwrScheme;
begin
  GetProcedureAddress(_CanUserWritePwrScheme, powrproflib, 'CanUserWritePwrScheme');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CanUserWritePwrScheme]
  end;
end;

var
  _ReadProcessorPwrScheme: Pointer;

function ReadProcessorPwrScheme;
begin
  GetProcedureAddress(_ReadProcessorPwrScheme, powrproflib, 'ReadProcessorPwrScheme');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ReadProcessorPwrScheme]
  end;
end;

var
  _WriteProcessorPwrScheme: Pointer;

function WriteProcessorPwrScheme;
begin
  GetProcedureAddress(_WriteProcessorPwrScheme, powrproflib, 'WriteProcessorPwrScheme');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WriteProcessorPwrScheme]
  end;
end;

var
  _ValidatePowerPolicies: Pointer;

function ValidatePowerPolicies;
begin
  GetProcedureAddress(_ValidatePowerPolicies, powrproflib, 'ValidatePowerPolicies');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ValidatePowerPolicies]
  end;
end;

var
  _CallNtPowerInformation: Pointer;

function CallNtPowerInformation;
begin
  GetProcedureAddress(_CallNtPowerInformation, powrproflib, 'CallNtPowerInformation');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CallNtPowerInformation]
  end;
end;

{$ELSE}

function GetPwrDiskSpindownRange; external powrproflib name 'GetPwrDiskSpindownRange';
function EnumPwrSchemes; external powrproflib name 'EnumPwrSchemes';
function ReadGlobalPwrPolicy; external powrproflib name 'ReadGlobalPwrPolicy';
function ReadPwrScheme; external powrproflib name 'ReadPwrScheme';
function WritePwrScheme; external powrproflib name 'WritePwrScheme';
function WriteGlobalPwrPolicy; external powrproflib name 'WriteGlobalPwrPolicy';
function DeletePwrScheme; external powrproflib name 'DeletePwrScheme';
function GetActivePwrScheme; external powrproflib name 'GetActivePwrScheme';
function SetActivePwrScheme; external powrproflib name 'SetActivePwrScheme';
function GetPwrCapabilities; external powrproflib name 'GetPwrCapabilities';
function IsPwrSuspendAllowed; external powrproflib name 'IsPwrSuspendAllowed';
function IsPwrHibernateAllowed; external powrproflib name 'IsPwrHibernateAllowed';
function IsPwrShutdownAllowed; external powrproflib name 'IsPwrShutdownAllowed';
function IsAdminOverrideActive; external powrproflib name 'IsAdminOverrideActive';
function SetSuspendState; external powrproflib name 'SetSuspendState';
function GetCurrentPowerPolicies; external powrproflib name 'GetCurrentPowerPolicies';
function CanUserWritePwrScheme; external powrproflib name 'CanUserWritePwrScheme';
function ReadProcessorPwrScheme; external powrproflib name 'ReadProcessorPwrScheme';
function WriteProcessorPwrScheme; external powrproflib name 'WriteProcessorPwrScheme';
function ValidatePowerPolicies; external powrproflib name 'ValidatePowerPolicies';
function CallNtPowerInformation; external powrproflib name 'CallNtPowerInformation';

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
