{******************************************************************************}
{                                                                              }
{ Application Recovery and Restart API (ARR) interface Unit for Object Pascal  }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2008 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ Portions created by stOrM! are Copyright (C) 2009                            }
{ stOrM!. All Rights Reserved.                                                 }
{ Portions created by Christian Wimmer are Copyright (C) 2009                  }
{ Christian Wimmer. All Rights Reserved.                                       }
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
{ ARR:                                                                         }
{ http://msdn.microsoft.com/en-us/library/cc948909%28VS.85%29.aspx             }
{******************************************************************************}

{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaAppRecovery;


{.$HPPEMIT ''}
{.$HPPEMIT '#include "xxxx.h"'}
{.$HPPEMIT ''}

{$I ..\Includes\JediAPILib.inc}
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_OMIT_SECTIONS}
interface

{$IFDEF JWA_WINDOWS}
  uses JwaWindows;
{$ELSE}
uses
  JwaWinBase, JwaWinType;
{$ENDIF JWA_WINDOWS}


{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_IMPLEMENTATIONSECTION}
const
  RESTART_NO_FLAGS      = 0;
  RESTART_NO_CRASH      = 1;
  RESTART_NO_HANG       = 2;
  RESTART_NO_PATCH      = 4;
  RESTART_NO_REBOOT     = 8;
  RECOVERY_DEFAULT_PING_INTERVAL = 5000;
  RECOVERY_MAX_PING_INTERVAL = (5 * 60 * 1000);
  RESTART_MAX_CMD_LINE = 2048;



type
  APPLICATION_RECOVERY_CALLBACK = function (pvParameter : Pointer) : DWORD; stdcall;
  TApplicationRecoveryCallback = APPLICATION_RECOVERY_CALLBACK;

  function ApplicationRecoveryFinished({__in}bSuccess : BOOL) : HRESULT; stdcall;
  {$EXTERNALSYM ApplicationRecoveryFinished}

  function ApplicationRecoveryInProgress(out pbCanceled : BOOL) : HRESULT; stdcall;
  {$EXTERNALSYM ApplicationRecoveryInProgress}

  function GetApplicationRecoveryCallback({__in}hProcess: THandle;
    {__out}pRecoveryCallback : TApplicationRecoveryCallback;
    {__deref_opt_out_opt}ppvParameter : PPointer;
    {__out_opt}dwPingInterval : PDWORD;
    {__out_opt}dwFlags : PDWORD) : HRESULT; stdcall;
  {$EXTERNALSYM GetApplicationRecoveryCallback}

  function GetApplicationRestartSettings({__in}hProcess: THandle;
    {__out_opt}pwzCommandline : PWideChar;
    {__inout}var pcchSize : DWORD;
    {_out_opt}pdwFlags : PDWORD) : HRESULT; stdcall;
  {$EXTERNALSYM GetApplicationRestartSettings}

  function RegisterApplicationRecoveryCallback(
    {__in}pRecoveryCallback : TApplicationRecoveryCallback;
    {__in_opt}pvParameter : Pointer;
    {__in}dwPingInterval : DWORD;
    {__in}dwFlags : DWORD) : HRESULT; stdcall;
  {$EXTERNALSYM RegisterApplicationRecoveryCallback}

  function RegisterApplicationRestart({__in_opt}pwzCommandline: PWideChar;
    {__in}dwFlags: DWORD): HRESULT; stdcall;
  {$EXTERNALSYM RegisterApplicationRestart}

  function UnregisterApplicationRecoveryCallback : HRESULT; stdcall;
  {$EXTERNALSYM UnregisterApplicationRecoveryCallback}

  function UnregisterApplicationRestart : HRESULT; stdcall;
  {$EXTERNALSYM UnregisterApplicationRestart}

{$ENDIF JWA_IMPLEMENTATIONSECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
implementation
uses JwaWinDllNames;
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}
//add here implementation stuff


{$IFDEF DYNAMIC_LINK}

var
  _ApplicationRecoveryFinished: Pointer;

function ApplicationRecoveryFinished;
begin
  GetProcedureAddress(_ApplicationRecoveryFinished, kernel32, 'ApplicationRecoveryFinished');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ApplicationRecoveryFinished]
  end;
end;

var
  _ApplicationRecoveryInProgress: Pointer;

function ApplicationRecoveryInProgress;
begin
  GetProcedureAddress(_ApplicationRecoveryInProgress, kernel32, 'ApplicationRecoveryInProgress');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ApplicationRecoveryInProgress]
  end;
end;

var
  _GetApplicationRecoveryCallback: Pointer;

function GetApplicationRecoveryCallback;
begin
  GetProcedureAddress(_GetApplicationRecoveryCallback, kernel32, 'GetApplicationRecoveryCallback');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetApplicationRecoveryCallback]
  end;
end;

var
  _GetApplicationRestartSettings: Pointer;

function GetApplicationRestartSettings;
begin
  GetProcedureAddress(_GetApplicationRestartSettings, kernel32, 'GetApplicationRestartSettings');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetApplicationRestartSettings]
  end;
end;

var
  _RegisterApplicationRecoveryCallback: Pointer;

function RegisterApplicationRecoveryCallback;
begin
  GetProcedureAddress(_RegisterApplicationRecoveryCallback, kernel32, 'RegisterApplicationRecoveryCallback');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegisterApplicationRecoveryCallback]
  end;
end;

var
  _RegisterApplicationRestart: Pointer;

function RegisterApplicationRestart;
begin
  GetProcedureAddress(_RegisterApplicationRestart, kernel32, 'RegisterApplicationRestart');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegisterApplicationRestart]
  end;
end;

var
  _UnregisterApplicationRecoveryCallback: Pointer;

function UnregisterApplicationRecoveryCallback;
begin
  GetProcedureAddress(_UnregisterApplicationRecoveryCallback, kernel32, 'UnregisterApplicationRecoveryCallback');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UnregisterApplicationRecoveryCallback]
  end;
end;

var
  _UnregisterApplicationRestart: Pointer;

function UnregisterApplicationRestart;
begin
  GetProcedureAddress(_UnregisterApplicationRestart, kernel32, 'UnregisterApplicationRestart');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UnregisterApplicationRestart]
  end;
end;

{$ELSE}


function ApplicationRecoveryFinished;  external kernel32 Name 'ApplicationRecoveryFinished';
function ApplicationRecoveryInProgress; external kernel32 Name 'ApplicationRecoveryInProgress';
function GetApplicationRecoveryCallback; external kernel32 Name 'GetApplicationRecoveryCallback';
function GetApplicationRestartSettings; external kernel32 Name 'GetApplicationRestartSettings';
function RegisterApplicationRecoveryCallback; external kernel32 Name 'RegisterApplicationRecoveryCallback';
function RegisterApplicationRestart; external kernel32 Name 'RegisterApplicationRestart';
function UnregisterApplicationRecoveryCallback; external kernel32 Name 'UnregisterApplicationRecoveryCallback';
function UnregisterApplicationRestart;  external kernel32 Name 'UnregisterApplicationRestart';


{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
