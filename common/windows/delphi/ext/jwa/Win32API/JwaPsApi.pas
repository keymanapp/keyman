{******************************************************************************}
{                                                                              }
{ Process Status API interface Unit for Object Pascal                          }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: psapi.h, released June 2000. The original Pascal       }
{ code is: PsApi.pas, released December 2000. The initial developer of the     }
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

// $Id: JwaPsApi.pas,v 1.12 2007/09/05 11:58:52 dezipaitor Exp $
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaPsApi;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include <psapi.h>'}
{$HPPEMIT ''}


{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaWinType;
{$ENDIF JWA_OMIT_SECTIONS}
{$IFNDEF JWA_IMPLEMENTATIONSECTION}

function EnumProcesses(lpidProcess: LPDWORD; cb: DWORD; var cbNeeded: DWORD): BOOL; stdcall;
{$EXTERNALSYM EnumProcesses}

function EnumProcessModules(hProcess: HANDLE; lphModule: PHMODULE; cb: DWORD;
  var lpcbNeeded: DWORD): BOOL; stdcall;
{$EXTERNALSYM EnumProcessModules}

function GetModuleBaseNameA(hProcess: HANDLE; hModule: HMODULE; lpBaseName: LPSTR;
  nSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetModuleBaseNameA}
function GetModuleBaseNameW(hProcess: HANDLE; hModule: HMODULE; lpBaseName: LPWSTR;
  nSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetModuleBaseNameW}
function GetModuleBaseName(hProcess: HANDLE; hModule: HMODULE; lpBaseName: LPTSTR;
  nSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetModuleBaseName}

function GetModuleFileNameExA(hProcess: HANDLE; hModule: HMODULE; lpFilename: LPSTR;
  nSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetModuleFileNameExA}
function GetModuleFileNameExW(hProcess: HANDLE; hModule: HMODULE; lpFilename: LPWSTR;
  nSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetModuleFileNameExW}
function GetModuleFileNameEx(hProcess: HANDLE; hModule: HMODULE; lpFilename: LPTSTR;
  nSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetModuleFileNameEx}

type
  LPMODULEINFO = ^MODULEINFO;
  {$EXTERNALSYM LPMODULEINFO}
  _MODULEINFO = packed record
    lpBaseOfDll: LPVOID;
    SizeOfImage: DWORD;
    EntryPoint: LPVOID;
  end;
  {$EXTERNALSYM _MODULEINFO}
  MODULEINFO = _MODULEINFO;
  {$EXTERNALSYM MODULEINFO}
  TModuleInfo = MODULEINFO;
  PModuleInfo = LPMODULEINFO;

function GetModuleInformation(hProcess: HANDLE; hModule: HMODULE;
  var lpmodinfo: MODULEINFO; cb: DWORD): BOOL; stdcall;
{$EXTERNALSYM GetModuleInformation}

function EmptyWorkingSet(hProcess: HANDLE): BOOL; stdcall;
{$EXTERNALSYM EmptyWorkingSet}

function QueryWorkingSet(hProcess: HANDLE; pv: PVOID; cb: DWORD): BOOL; stdcall;
{$EXTERNALSYM QueryWorkingSet}

function InitializeProcessForWsWatch(hProcess: HANDLE): BOOL; stdcall;
{$EXTERNALSYM InitializeProcessForWsWatch}

type
  PPSAPI_WS_WATCH_INFORMATION = ^PSAPI_WS_WATCH_INFORMATION;
  {$EXTERNALSYM PPSAPI_WS_WATCH_INFORMATION}
  _PSAPI_WS_WATCH_INFORMATION = packed record
    FaultingPc: LPVOID;
    FaultingVa: LPVOID;
  end;
  {$EXTERNALSYM _PSAPI_WS_WATCH_INFORMATION}
  PSAPI_WS_WATCH_INFORMATION = _PSAPI_WS_WATCH_INFORMATION;
  {$EXTERNALSYM PSAPI_WS_WATCH_INFORMATION}
  TPsApiWsWatchInformation = PSAPI_WS_WATCH_INFORMATION;
  PPsApiWsWatchInformation = PPSAPI_WS_WATCH_INFORMATION;

function GetWsChanges(hProcess: HANDLE; var lpWatchInfo: PSAPI_WS_WATCH_INFORMATION;
  cb: DWORD): BOOL; stdcall;
{$EXTERNALSYM GetWsChanges}

function GetMappedFileNameW(hProcess: HANDLE; lpv: LPVOID; lpFilename: LPWSTR;
  nSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetMappedFileNameW}
function GetMappedFileNameA(hProcess: HANDLE; lpv: LPVOID; lpFilename: LPSTR;
  nSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetMappedFileNameA}
function GetMappedFileName(hProcess: HANDLE; lpv: LPVOID; lpFilename: LPTSTR;
  nSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetMappedFileName}

function EnumDeviceDrivers(lpImageBase: LPLPVOID; cb: DWORD; var lpcbNeeded: DWORD): BOOL; stdcall;
{$EXTERNALSYM EnumDeviceDrivers}

function GetDeviceDriverBaseNameA(ImageBase: LPVOID; lpBaseName: LPSTR;
  nSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetDeviceDriverBaseNameA}
function GetDeviceDriverBaseNameW(ImageBase: LPVOID; lpBaseName: LPWSTR;
  nSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetDeviceDriverBaseNameW}
function GetDeviceDriverBaseName(ImageBase: LPVOID; lpBaseName: LPTSTR;
  nSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetDeviceDriverBaseName}

function GetDeviceDriverFileNameA(ImageBase: LPVOID; lpFilename: LPSTR;
  nSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetDeviceDriverFileNameA}
function GetDeviceDriverFileNameW(ImageBase: LPVOID; lpFilename: LPWSTR;
  nSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetDeviceDriverFileNameW}
function GetDeviceDriverFileName(ImageBase: LPVOID; lpFilename: LPTSTR;
  nSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetDeviceDriverFileName}

// Structure for GetProcessMemoryInfo()

type
  PPROCESS_MEMORY_COUNTERS = ^PROCESS_MEMORY_COUNTERS;
  {$EXTERNALSYM PPROCESS_MEMORY_COUNTERS}
  _PROCESS_MEMORY_COUNTERS = packed record
    cb: DWORD;
    PageFaultCount: DWORD;
    PeakWorkingSetSize: SIZE_T;
    WorkingSetSize: SIZE_T;
    QuotaPeakPagedPoolUsage: SIZE_T;
    QuotaPagedPoolUsage: SIZE_T;
    QuotaPeakNonPagedPoolUsage: SIZE_T;
    QuotaNonPagedPoolUsage: SIZE_T;
    PagefileUsage: SIZE_T;
    PeakPagefileUsage: SIZE_T;
  end;
  {$EXTERNALSYM _PROCESS_MEMORY_COUNTERS}
  PROCESS_MEMORY_COUNTERS = _PROCESS_MEMORY_COUNTERS;
  {$EXTERNALSYM PROCESS_MEMORY_COUNTERS}
  TProcessMemoryCounters = PROCESS_MEMORY_COUNTERS;
  PProcessMemoryCounters = PPROCESS_MEMORY_COUNTERS;

  _PROCESS_MEMORY_COUNTERS_EX = record
    cb: DWORD;
    PageFaultCount: DWORD;
    PeakWorkingSetSize: SIZE_T;
    WorkingSetSize: SIZE_T;
    QuotaPeakPagedPoolUsage: SIZE_T;
    QuotaPagedPoolUsage: SIZE_T;
    QuotaPeakNonPagedPoolUsage: SIZE_T;
    QuotaNonPagedPoolUsage: SIZE_T;
    PagefileUsage: SIZE_T;
    PeakPagefileUsage: SIZE_T;
    PrivateUsage: SIZE_T;
  end;
  {$EXTERNALSYM _PROCESS_MEMORY_COUNTERS_EX}
  PROCESS_MEMORY_COUNTERS_EX = _PROCESS_MEMORY_COUNTERS_EX;
  {$EXTERNALSYM PROCESS_MEMORY_COUNTERS_EX}
  PPROCESS_MEMORY_COUNTERS_EX = ^PROCESS_MEMORY_COUNTERS_EX;
  {$EXTERNALSYM PPROCESS_MEMORY_COUNTERS_EX}
  TProcessMemoryCountersEx = PROCESS_MEMORY_COUNTERS_EX;

function GetProcessMemoryInfo(Process: HANDLE;
  var ppsmemCounters: PROCESS_MEMORY_COUNTERS; cb: DWORD): BOOL; stdcall;
{$EXTERNALSYM GetProcessMemoryInfo}

type
  _PERFORMANCE_INFORMATION = record
    cb: DWORD;
    CommitTotal: SIZE_T;
    CommitLimit: SIZE_T;
    CommitPeak: SIZE_T;
    PhysicalTotal: SIZE_T;
    PhysicalAvailable: SIZE_T;
    SystemCache: SIZE_T;
    KernelTotal: SIZE_T;
    KernelPaged: SIZE_T;
    KernelNonpaged: SIZE_T;
    PageSize: SIZE_T;
    HandleCount: DWORD;
    ProcessCount: DWORD;
    ThreadCount: DWORD;
  end;
  {$EXTERNALSYM _PERFORMANCE_INFORMATION}
  PERFORMANCE_INFORMATION = _PERFORMANCE_INFORMATION;
  {$EXTERNALSYM PERFORMANCE_INFORMATION}
  PPERFORMANCE_INFORMATION = ^PERFORMANCE_INFORMATION;
  {$EXTERNALSYM PPERFORMANCE_INFORMATION}
  TPerformanceInformation = PERFORMANCE_INFORMATION;
  PPerformanceInformation = PPERFORMANCE_INFORMATION;

{ MVB:

  Please note that this function, unlike what the Platform SDK documents, is _not_ available for Windows 2000!!!
  It is available starting with Windows XP and Windows.NET Server.
  If needed, you can relatively easily clone the functionality of this function by using the performance monitor
  API (either through the HKEY_PERFORMANCE_DATA registry interface or using the Performance Data Helper API)
}

function GetPerformanceInfo(pPerformanceInformation: PPERFORMANCE_INFORMATION;
  cb: DWORD): BOOL; stdcall;
{$EXTERNALSYM GetPerformanceInfo}

type
  _ENUM_PAGE_FILE_INFORMATION = record
    cb: DWORD;
    Reserved: DWORD;
    TotalSize: SIZE_T;
    TotalInUse: SIZE_T;
    PeakUsage: SIZE_T;
  end;
  {$EXTERNALSYM _ENUM_PAGE_FILE_INFORMATION}
  ENUM_PAGE_FILE_INFORMATION = _ENUM_PAGE_FILE_INFORMATION;
  {$EXTERNALSYM ENUM_PAGE_FILE_INFORMATION}
  PENUM_PAGE_FILE_INFORMATION = ^ENUM_PAGE_FILE_INFORMATION;
  TEnumPageFileInformation = ENUM_PAGE_FILE_INFORMATION;
  PEnumPageFileInformation = PENUM_PAGE_FILE_INFORMATION;

type
  PENUM_PAGE_FILE_CALLBACKW = function(pContext: LPVOID;
    pPageFileInfo: PENUM_PAGE_FILE_INFORMATION; lpFilename: LPCWSTR): BOOL; stdcall;
  {$EXTERNALSYM PENUM_PAGE_FILE_CALLBACKW}
  PENUM_PAGE_FILE_CALLBACKA = function(pContext: LPVOID;
    pPageFileInfo: PENUM_PAGE_FILE_INFORMATION; lpFilename: LPCSTR): BOOL; stdcall;
  {$EXTERNALSYM PENUM_PAGE_FILE_CALLBACKA}
  {$IFDEF UNICODE}
  PENUM_PAGE_FILE_CALLBACK = PENUM_PAGE_FILE_CALLBACKW;
  {$EXTERNALSYM PENUM_PAGE_FILE_CALLBACK}
  {$ELSE}
  PENUM_PAGE_FILE_CALLBACK = PENUM_PAGE_FILE_CALLBACKA;
  {$EXTERNALSYM PENUM_PAGE_FILE_CALLBACK}
  {$ENDIF UNICODE}

function EnumPageFilesW(pCallBackRoutine: PENUM_PAGE_FILE_CALLBACKW; pContext: LPVOID): BOOL; stdcall;
{$EXTERNALSYM EnumPageFilesW}
function EnumPageFilesA(pCallBackRoutine: PENUM_PAGE_FILE_CALLBACKA; pContext: LPVOID): BOOL; stdcall;
{$EXTERNALSYM EnumPageFilesA}
function EnumPageFiles(pCallBackRoutine: PENUM_PAGE_FILE_CALLBACK; pContext: LPVOID): BOOL; stdcall;
{$EXTERNALSYM EnumPageFiles}

function GetProcessImageFileNameA(hProcess: HANDLE; lpImageFileName: LPSTR;
  nSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetProcessImageFileNameA}
function GetProcessImageFileNameW(hProcess: HANDLE; lpImageFileName: LPWSTR;
  nSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetProcessImageFileNameW}
function GetProcessImageFileName(hProcess: HANDLE; lpImageFileName: LPTSTR;
  nSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetProcessImageFileName}
{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}


{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  PsapiLib = 'psapi.dll';
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}

var
  _EnumProcesses: Pointer;

function EnumProcesses;
begin
  GetProcedureAddress(_EnumProcesses, PsapiLib, 'EnumProcesses');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EnumProcesses]
  end;
end;

var
  _EnumProcessModules: Pointer;

function EnumProcessModules;
begin
  GetProcedureAddress(_EnumProcessModules, PsapiLib, 'EnumProcessModules');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EnumProcessModules]
  end;
end;

var
  _GetModuleBaseNameA: Pointer;

function GetModuleBaseNameA;
begin
  GetProcedureAddress(_GetModuleBaseNameA, PsapiLib, 'GetModuleBaseNameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetModuleBaseNameA]
  end;
end;

var
  _GetModuleBaseNameW: Pointer;

function GetModuleBaseNameW;
begin
  GetProcedureAddress(_GetModuleBaseNameW, PsapiLib, 'GetModuleBaseNameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetModuleBaseNameW]
  end;
end;

var
  _GetModuleBaseName: Pointer;

function GetModuleBaseName;
begin
  GetProcedureAddress(_GetModuleBaseName, PsapiLib, 'GetModuleBaseName' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetModuleBaseName]
  end;
end;

var
  _GetModuleFileNameExA: Pointer;

function GetModuleFileNameExA;
begin
  GetProcedureAddress(_GetModuleFileNameExA, PsapiLib, 'GetModuleFileNameExA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetModuleFileNameExA]
  end;
end;

var
  _GetModuleFileNameExW: Pointer;

function GetModuleFileNameExW;
begin
  GetProcedureAddress(_GetModuleFileNameExW, PsapiLib, 'GetModuleFileNameExW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetModuleFileNameExW]
  end;
end;

var
  _GetModuleFileNameEx: Pointer;

function GetModuleFileNameEx;
begin
  GetProcedureAddress(_GetModuleFileNameEx, PsapiLib, 'GetModuleFileNameEx' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetModuleFileNameEx]
  end;
end;

var
  _GetModuleInformation: Pointer;

function GetModuleInformation;
begin
  GetProcedureAddress(_GetModuleInformation, PsapiLib, 'GetModuleInformation');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetModuleInformation]
  end;
end;

var
  _EmptyWorkingSet: Pointer;

function EmptyWorkingSet;
begin
  GetProcedureAddress(_EmptyWorkingSet, PsapiLib, 'EmptyWorkingSet');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EmptyWorkingSet]
  end;
end;

var
  _QueryWorkingSet: Pointer;

function QueryWorkingSet;
begin
  GetProcedureAddress(_QueryWorkingSet, PsapiLib, 'QueryWorkingSet');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_QueryWorkingSet]
  end;
end;

var
  _InitializeProcessForWsWatch: Pointer;

function InitializeProcessForWsWatch;
begin
  GetProcedureAddress(_InitializeProcessForWsWatch, PsapiLib, 'InitializeProcessForWsWatch');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InitializeProcessForWsWatch]
  end;
end;

var
  _GetWsChanges: Pointer;

function GetWsChanges;
begin
  GetProcedureAddress(_GetWsChanges, PsapiLib, 'GetWsChanges');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetWsChanges]
  end;
end;

var
  _GetMappedFileNameW: Pointer;

function GetMappedFileNameW;
begin
  GetProcedureAddress(_GetMappedFileNameW, PsapiLib, 'GetMappedFileNameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetMappedFileNameW]
  end;
end;

var
  _GetMappedFileNameA: Pointer;

function GetMappedFileNameA;
begin
  GetProcedureAddress(_GetMappedFileNameA, PsapiLib, 'GetMappedFileNameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetMappedFileNameA]
  end;
end;

var
  _GetMappedFileName: Pointer;

function GetMappedFileName;
begin
  GetProcedureAddress(_GetMappedFileName, PsapiLib, 'GetMappedFileName' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetMappedFileName]
  end;
end;

var
  _EnumDeviceDrivers: Pointer;

function EnumDeviceDrivers;
begin
  GetProcedureAddress(_EnumDeviceDrivers, PsapiLib, 'EnumDeviceDrivers');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EnumDeviceDrivers]
  end;
end;

var
  _GetDeviceDriverBaseNameA: Pointer;

function GetDeviceDriverBaseNameA;
begin
  GetProcedureAddress(_GetDeviceDriverBaseNameA, PsapiLib, 'GetDeviceDriverBaseNameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetDeviceDriverBaseNameA]
  end;
end;

var
  _GetDeviceDriverBaseNameW: Pointer;

function GetDeviceDriverBaseNameW;
begin
  GetProcedureAddress(_GetDeviceDriverBaseNameW, PsapiLib, 'GetDeviceDriverBaseNameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetDeviceDriverBaseNameW]
  end;
end;

var
  _GetDeviceDriverBaseName: Pointer;

function GetDeviceDriverBaseName;
begin
  GetProcedureAddress(_GetDeviceDriverBaseName, PsapiLib, 'GetDeviceDriverBaseName' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetDeviceDriverBaseName]
  end;
end;

var
  _GetDeviceDriverFileNameA: Pointer;

function GetDeviceDriverFileNameA;
begin
  GetProcedureAddress(_GetDeviceDriverFileNameA, PsapiLib, 'GetDeviceDriverFileNameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetDeviceDriverFileNameA]
  end;
end;

var
  _GetDeviceDriverFileNameW: Pointer;

function GetDeviceDriverFileNameW;
begin
  GetProcedureAddress(_GetDeviceDriverFileNameW, PsapiLib, 'GetDeviceDriverFileNameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetDeviceDriverFileNameW]
  end;
end;

var
  _GetDeviceDriverFileName: Pointer;

function GetDeviceDriverFileName;
begin
  GetProcedureAddress(_GetDeviceDriverFileName, PsapiLib, 'GetDeviceDriverFileName' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetDeviceDriverFileName]
  end;
end;

var
  _GetProcessMemoryInfo: Pointer;

function GetProcessMemoryInfo;
begin
  GetProcedureAddress(_GetProcessMemoryInfo, PsapiLib, 'GetProcessMemoryInfo');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetProcessMemoryInfo]
  end;
end;

var
  _GetPerformanceInfo: Pointer;

function GetPerformanceInfo;
begin
  GetProcedureAddress(_GetPerformanceInfo, PsapiLib, 'GetPerformanceInfo');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetPerformanceInfo]
  end;
end;

var
  _EnumPageFilesW: Pointer;

function EnumPageFilesW;
begin
  GetProcedureAddress(_EnumPageFilesW, PsapiLib, 'EnumPageFilesA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EnumPageFilesW]
  end;
end;

var
  _EnumPageFilesA: Pointer;

function EnumPageFilesA;
begin
  GetProcedureAddress(_EnumPageFilesA, PsapiLib, 'EnumPageFilesW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EnumPageFilesA]
  end;
end;

var
  _EnumPageFiles: Pointer;

function EnumPageFiles;
begin
  GetProcedureAddress(_EnumPageFiles, PsapiLib, 'EnumPageFiles' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EnumPageFiles]
  end;
end;

var
  _GetProcessImageFileNameA: Pointer;

function GetProcessImageFileNameA;
begin
  GetProcedureAddress(_GetProcessImageFileNameA, PsapiLib, 'GetProcessImageFileNameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetProcessImageFileNameA]
  end;
end;

var
  _GetProcessImageFileNameW: Pointer;

function GetProcessImageFileNameW;
begin
  GetProcedureAddress(_GetProcessImageFileNameW, PsapiLib, 'GetProcessImageFileNameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetProcessImageFileNameW]
  end;
end;

var
  _GetProcessImageFileName: Pointer;

function GetProcessImageFileName;
begin
  GetProcedureAddress(_GetProcessImageFileName, PsapiLib, 'GetProcessImageFileName' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetProcessImageFileName]
  end;
end;

{$ELSE}

function EnumProcesses; external PsapiLib name 'EnumProcesses';
function EnumProcessModules; external PsapiLib name 'EnumProcessModules';
function GetModuleBaseNameA; external PsapiLib name 'GetModuleBaseNameA';
function GetModuleBaseNameW; external PsapiLib name 'GetModuleBaseNameW';
function GetModuleBaseName; external PsapiLib name 'GetModuleBaseName' + AWSuffix;
function GetModuleFileNameExA; external PsapiLib name 'GetModuleFileNameExA';
function GetModuleFileNameExW; external PsapiLib name 'GetModuleFileNameExW';
function GetModuleFileNameEx; external PsapiLib name 'GetModuleFileNameEx' + AWSuffix;
function GetModuleInformation; external PsapiLib name 'GetModuleInformation';
function EmptyWorkingSet; external PsapiLib name 'EmptyWorkingSet';
function QueryWorkingSet; external PsapiLib name 'QueryWorkingSet';
function InitializeProcessForWsWatch; external PsapiLib name 'InitializeProcessForWsWatch';
function GetWsChanges; external PsapiLib name 'GetWsChanges';
function GetMappedFileNameW; external PsapiLib name 'GetMappedFileNameW';
function GetMappedFileNameA; external PsapiLib name 'GetMappedFileNameA';
function GetMappedFileName; external PsapiLib name 'GetMappedFileName' + AWSuffix;
function EnumDeviceDrivers; external PsapiLib name 'EnumDeviceDrivers';
function GetDeviceDriverBaseNameA; external PsapiLib name 'GetDeviceDriverBaseNameA';
function GetDeviceDriverBaseNameW; external PsapiLib name 'GetDeviceDriverBaseNameW';
function GetDeviceDriverBaseName; external PsapiLib name 'GetDeviceDriverBaseName' + AWSuffix;
function GetDeviceDriverFileNameA; external PsapiLib name 'GetDeviceDriverFileNameA';
function GetDeviceDriverFileNameW; external PsapiLib name 'GetDeviceDriverFileNameW';
function GetDeviceDriverFileName; external PsapiLib name 'GetDeviceDriverFileName' + AWSuffix;
function GetProcessMemoryInfo; external PsapiLib name 'GetProcessMemoryInfo';
function GetPerformanceInfo; external PsapiLib name 'GetPerformanceInfo';
function EnumPageFilesW; external PsapiLib name 'EnumPageFilesA';
function EnumPageFilesA; external PsapiLib name 'EnumPageFilesW';
function EnumPageFiles; external PsapiLib name 'EnumPageFiles' + AWSuffix;
function GetProcessImageFileNameA; external PsapiLib name 'GetProcessImageFileNameA';
function GetProcessImageFileNameW; external PsapiLib name 'GetProcessImageFileNameW';
function GetProcessImageFileName; external PsapiLib name 'GetProcessImageFileName' + AWSuffix;

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}


{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
