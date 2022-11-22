{******************************************************************************}
{                                                                              }
{ Performance Monitoring Installer API interface Unit for Object Pascal        }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: loadperf.h, released June 2000. The original Pascal    }
{ code is: LoadPerf.pas, released December 2000. The initial developer of the  }
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

// $Id: JwaLoadPerf.pas,v 1.11 2007/09/05 11:58:51 dezipaitor Exp $
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaLoadPerf;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaWinType;
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_IMPLEMENTATIONSECTION}

// flags for dwFlags Argument

const
  LOADPERF_FLAGS_DELETE_MOF_ON_EXIT  = ULONG_PTR(1);
  {$EXTERNALSYM LOADPERF_FLAGS_DELETE_MOF_ON_EXIT}
  LOADPERF_FLAGS_LOAD_REGISTRY_ONLY  = ULONG_PTR(2);
  {$EXTERNALSYM LOADPERF_FLAGS_LOAD_REGISTRY_ONLY}
  LOADPERF_FLAGS_CREATE_MOF_ONLY     = ULONG_PTR(4);
  {$EXTERNALSYM LOADPERF_FLAGS_CREATE_MOF_ONLY}
  LOADPERF_FLAGS_DISPLAY_USER_MSGS   = ULONG_PTR(8);
  {$EXTERNALSYM LOADPERF_FLAGS_DISPLAY_USER_MSGS}

(* removed PSDK XP SP1
// note: LOADPERF_FLAGS_LOAD_REGISTRY_ONLY is not a valid flag for
// LoadMofFromInstalledServiceA/W as the service must already be installed

function LoadMofFromInstalledServiceA(szServiceName, szMofFilename: LPCSTR;
  dwFlags: ULONG_PTR): DWORD; stdcall;
{$EXTERNALSYM LoadMofFromInstalledServiceA}
function LoadMofFromInstalledServiceW(szServiceName, szMofFilename: LPCWSTR;
  dwFlags: ULONG_PTR): DWORD; stdcall;
{$EXTERNALSYM LoadMofFromInstalledServiceW}
function LoadMofFromInstalledService(szServiceName, szMofFilename: LPCTSTR;
  dwFlags: ULONG_PTR): DWORD; stdcall;
{$EXTERNALSYM LoadMofFromInstalledService}
*)

function InstallPerfDllA(szComputerName, lpIniFile: LPCSTR; dwFlags: ULONG_PTR): DWORD; stdcall;
{$EXTERNALSYM InstallPerfDllA}
function InstallPerfDllW(szComputerName, lpIniFile: LPCWSTR; dwFlags: ULONG_PTR): DWORD; stdcall;
{$EXTERNALSYM InstallPerfDllW}
function InstallPerfDll(szComputerName, lpIniFile: LPCTSTR; dwFlags: ULONG_PTR): DWORD; stdcall;
{$EXTERNALSYM InstallPerfDll}

function LoadPerfCounterTextStringsA(lpCommandLine: LPSTR; bQuietModeArg: BOOL): DWORD; stdcall;
{$EXTERNALSYM LoadPerfCounterTextStringsA}
function LoadPerfCounterTextStringsW(lpCommandLine: LPWSTR; bQuietModeArg: BOOL): DWORD; stdcall;
{$EXTERNALSYM LoadPerfCounterTextStringsW}
function LoadPerfCounterTextStrings(lpCommandLine: LPTSTR; bQuietModeArg: BOOL): DWORD; stdcall;
{$EXTERNALSYM LoadPerfCounterTextStrings}

function UnloadPerfCounterTextStringsA(lpCommandLine: LPSTR; bQuietModeArg: BOOL): DWORD; stdcall;
{$EXTERNALSYM UnloadPerfCounterTextStringsA}
function UnloadPerfCounterTextStringsW(lpCommandLine: LPWSTR; bQuietModeArg: BOOL): DWORD; stdcall;
{$EXTERNALSYM UnloadPerfCounterTextStringsW}
function UnloadPerfCounterTextStrings(lpCommandLine: LPTSTR; bQuietModeArg: BOOL): DWORD; stdcall;
{$EXTERNALSYM UnloadPerfCounterTextStrings}

function UpdatePerfNameFilesA(szNewCtrFilePath, szNewHlpFilePath: LPCSTR;
  szLanguageID: LPSTR; dwFlags: ULONG_PTR): DWORD; stdcall;
{$EXTERNALSYM UpdatePerfNameFilesA}
function UpdatePerfNameFilesW(szNewCtrFilePath, szNewHlpFilePath: LPCWSTR;
  szLanguageID: LPWSTR; dwFlags: ULONG_PTR): DWORD; stdcall;
{$EXTERNALSYM UpdatePerfNameFilesW}
function UpdatePerfNameFiles(szNewCtrFilePath, szNewHlpFilePath: LPCTSTR;
  szLanguageID: LPTSTR; dwFlags: ULONG_PTR): DWORD; stdcall;
{$EXTERNALSYM UpdatePerfNameFiles}

function SetServiceAsTrustedA(szReserved, szServiceName: LPCSTR): DWORD; stdcall;
{$EXTERNALSYM SetServiceAsTrustedA}
function SetServiceAsTrustedW(szReserved, szServiceName: LPCWSTR): DWORD; stdcall;
{$EXTERNALSYM SetServiceAsTrustedW}
function SetServiceAsTrusted(szReserved, szServiceName: LPCTSTR): DWORD; stdcall;
{$EXTERNALSYM SetServiceAsTrusted}

function BackupPerfRegistryToFileW(szFileName: LPCWSTR; szCommentString: LPCWSTR): DWORD; stdcall;
{$EXTERNALSYM BackupPerfRegistryToFileW}

function RestorePerfRegistryFromFileW(szFileName: LPCWSTR; szLangId: LPCWSTR): DWORD; stdcall;
{$EXTERNALSYM RestorePerfRegistryFromFileW}

{$ENDIF JWA_IMPLEMENTATIONSECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  loadperflib = 'loadperf.dll';
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}

var
  _InstallPerfDllA: Pointer;

function InstallPerfDllA;
begin
  GetProcedureAddress(_InstallPerfDllA, loadperflib, 'InstallPerfDllA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InstallPerfDllA]
  end;
end;

var
  _InstallPerfDllW: Pointer;

function InstallPerfDllW;
begin
  GetProcedureAddress(_InstallPerfDllW, loadperflib, 'InstallPerfDllW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InstallPerfDllW]
  end;
end;

var
  _InstallPerfDll: Pointer;

function InstallPerfDll;
begin
  GetProcedureAddress(_InstallPerfDll, loadperflib, 'InstallPerfDll' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InstallPerfDll]
  end;
end;

var
  _LoadPerfCounterTextStringsA: Pointer;

function LoadPerfCounterTextStringsA;
begin
  GetProcedureAddress(_LoadPerfCounterTextStringsA, loadperflib, 'LoadPerfCounterTextStringsA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_LoadPerfCounterTextStringsA]
  end;
end;

var
  _LoadPerfCounterTextStringsW: Pointer;

function LoadPerfCounterTextStringsW;
begin
  GetProcedureAddress(_LoadPerfCounterTextStringsW, loadperflib, 'LoadPerfCounterTextStringsW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_LoadPerfCounterTextStringsW]
  end;
end;

var
  _LoadPerfCounterTextStrings: Pointer;

function LoadPerfCounterTextStrings;
begin
  GetProcedureAddress(_LoadPerfCounterTextStrings, loadperflib, 'LoadPerfCounterTextStrings' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_LoadPerfCounterTextStrings]
  end;
end;

var
  _UnloadPerfCounterTextStringsA: Pointer;

function UnloadPerfCounterTextStringsA;
begin
  GetProcedureAddress(_UnloadPerfCounterTextStringsA, loadperflib, 'UnloadPerfCounterTextStringsA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UnloadPerfCounterTextStringsA]
  end;
end;

var
  _UnloadPerfCounterTextStringsW: Pointer;

function UnloadPerfCounterTextStringsW;
begin
  GetProcedureAddress(_UnloadPerfCounterTextStringsW, loadperflib, 'UnloadPerfCounterTextStringsW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UnloadPerfCounterTextStringsW]
  end;
end;

var
  _UnloadPerfCounterTextStrings: Pointer;

function UnloadPerfCounterTextStrings;
begin
  GetProcedureAddress(_UnloadPerfCounterTextStrings, loadperflib, 'UnloadPerfCounterTextStrings' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UnloadPerfCounterTextStrings]
  end;
end;

var
  _UpdatePerfNameFilesA: Pointer;

function UpdatePerfNameFilesA;
begin
  GetProcedureAddress(_UpdatePerfNameFilesA, loadperflib, 'UpdatePerfNameFilesA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UpdatePerfNameFilesA]
  end;
end;

var
  _UpdatePerfNameFilesW: Pointer;

function UpdatePerfNameFilesW;
begin
  GetProcedureAddress(_UpdatePerfNameFilesW, loadperflib, 'UpdatePerfNameFilesW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UpdatePerfNameFilesW]
  end;
end;

var
  _UpdatePerfNameFiles: Pointer;

function UpdatePerfNameFiles;
begin
  GetProcedureAddress(_UpdatePerfNameFiles, loadperflib, 'UpdatePerfNameFiles' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UpdatePerfNameFiles]
  end;
end;

var
  _SetServiceAsTrustedA: Pointer;

function SetServiceAsTrustedA;
begin
  GetProcedureAddress(_SetServiceAsTrustedA, loadperflib, 'SetServiceAsTrustedA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetServiceAsTrustedA]
  end;
end;

var
  _SetServiceAsTrustedW: Pointer;

function SetServiceAsTrustedW;
begin
  GetProcedureAddress(_SetServiceAsTrustedW, loadperflib, 'SetServiceAsTrustedW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetServiceAsTrustedW]
  end;
end;

var
  _SetServiceAsTrusted: Pointer;

function SetServiceAsTrusted;
begin
  GetProcedureAddress(_SetServiceAsTrusted, loadperflib, 'SetServiceAsTrusted' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetServiceAsTrusted]
  end;
end;

var
  _BackupPerfRegistryToFileW: Pointer;

function BackupPerfRegistryToFileW;
begin
  GetProcedureAddress(_BackupPerfRegistryToFileW, loadperflib, 'BackupPerfRegistryToFileW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BackupPerfRegistryToFileW]
  end;
end;

var
  _RestorePerfRegistryFromFileW: Pointer;

function RestorePerfRegistryFromFileW;
begin
  GetProcedureAddress(_RestorePerfRegistryFromFileW, loadperflib, 'RestorePerfRegistryFromFileW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RestorePerfRegistryFromFileW]
  end;
end;

{$ELSE}

function InstallPerfDllA; external loadperflib name 'InstallPerfDllA';
function InstallPerfDllW; external loadperflib name 'InstallPerfDllW';
function InstallPerfDll; external loadperflib name 'InstallPerfDll' + AWSuffix;
function LoadPerfCounterTextStringsA; external loadperflib name 'LoadPerfCounterTextStringsA';
function LoadPerfCounterTextStringsW; external loadperflib name 'LoadPerfCounterTextStringsW';
function LoadPerfCounterTextStrings; external loadperflib name 'LoadPerfCounterTextStrings' + AWSuffix;
function UnloadPerfCounterTextStringsA; external loadperflib name 'UnloadPerfCounterTextStringsA';
function UnloadPerfCounterTextStringsW; external loadperflib name 'UnloadPerfCounterTextStringsW';
function UnloadPerfCounterTextStrings; external loadperflib name 'UnloadPerfCounterTextStrings' + AWSuffix;
function UpdatePerfNameFilesA; external loadperflib name 'UpdatePerfNameFilesA';
function UpdatePerfNameFilesW; external loadperflib name 'UpdatePerfNameFilesW';
function UpdatePerfNameFiles; external loadperflib name 'UpdatePerfNameFiles' + AWSuffix;
function SetServiceAsTrustedA; external loadperflib name 'SetServiceAsTrustedA';
function SetServiceAsTrustedW; external loadperflib name 'SetServiceAsTrustedW';
function SetServiceAsTrusted; external loadperflib name 'SetServiceAsTrusted' + AWSuffix;
function BackupPerfRegistryToFileW; external loadperflib name 'BackupPerfRegistryToFileW';
function RestorePerfRegistryFromFileW; external loadperflib name 'RestorePerfRegistryFromFileW';

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}

