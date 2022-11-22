{******************************************************************************}
{                                                                              }
{ Windows Internal Services API interface Unit for Object Pascal               }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: winternl.h, released August 2001. The original Pascal  }
{ code is: Winternl.pas, released December 2000. The initial developer of the  }
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

// $Id: JwaWinternl.pas,v 1.6 2007/09/14 06:48:49 marquardt Exp $

(************************************************************************
*                                                                       *
*   winternl.h -- This module defines the internal NT APIs and data     *
*       structures that are intended for the use only by internal core  *
*       Windows components.  These APIs and data structures may change  *
*       at any time.                                                    *
*                                                                       *
*   These APIs and data structures are subject to changes from one      *
*       Windows release to another Windows release.  To maintain the    *
*       compatiblity of your application, avoid using these APIs and    *
*       data structures.                                                *
*                                                                       *
*   The appropriate mechanism for accessing the functions defined in    *
*       this header is to use LoadLibrary() for ntdll.dll and           *
*       GetProcAddress() for the particular function.  By using this    *
*       approach, your application will be more resilient to changes    *
*       for these functions between Windows releases.  If a function    *
*       prototype does change, then GetProcAddress() for that function  *
*       might detect the change and fail the function call, which your  *
*       application will be able to detect.  GetProcAddress() may not   *
*       be able to detect all signature changes, thus avoid using these *
*       internal functions.  Instead, your application should use the   *
*       appropriate Win32 function that provides equivalent or similiar *
*       functionality.                                                  *
*                                                                       *
*   Copyright (c) Microsoft Corp. All rights reserved.                  *
*                                                                       *
************************************************************************)
{$IFDEF JWA_INCLUDEMODE}
This unit must not be included in JwaWindows.pas because the members are
already declared.
{$ENDIF JWA_INCLUDEMODE}

{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaWinternl;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "Winternl.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

{$STACKFRAMES ON}

interface

uses
{$IFDEF JWA_WINDOWS}
  JwaWindows,
{$ELSE}
  JwaWinType, JwaWinNT
{$ENDIF}
  {$IFDEF USE_DELPHI_TYPES}
  ,Windows
  ,JwaWinDllNames
  {$ENDIF USE_DELPHI_TYPES}
  ;
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_IMPLEMENTATIONSECTION}

//
// The PEB and TEB structures are subject to changes between Windows
// releases, thus the fields offsets may change as well as the Reserved
// fields.  The Reserved fields are reserved for use only by the Windows
// operating systems.  Do not assume a maximum size for the structures.
//

//
// Instead of using the BeingDebugged field, use the Win32 APIs
//     IsDebuggerPresent, CheckRemoteDebuggerPresent
// Instead of using the SessionId field, use the Win32 APIs
//     GetCurrentProcessId and ProcessIdToSessionId
// Sample x86 assembly code that gets the SessionId (subject to change
//     between Windows releases, use the Win32 APIs to make your application
//     resilient to changes)
//     mov     eax,fs:[00000018]
//     mov     eax,[eax+0x30]
//     mov     eax,[eax+0x1d4]
//

{$IFNDEF JWA_INCLUDEMODE}
type
  _PEB = record
    Reserved1: array [0..1] of Byte;
    BeingDebugged: Byte;
    Reserved2: array [0..228] of Byte;
    Reserved3: array [0..58] of  PVOID;
    SessionId: ULONG;
  end;
  PEB = _PEB;
  PPEB = ^PEB;
  TPeb = PEB;
{$ENDIF JWA_INCLUDEMODE}

//
// Instead of using the Tls fields, use the Win32 TLS APIs
//     TlsAlloc, TlsGetValue, TlsSetValue, TlsFree
//
// Instead of using the ReservedForOle field, use the COM API
//     CoGetContextToken
//

type
  _TEB = record
    Reserved1: array [0..1951] of Byte;
    Reserved2: array [0..411] of PVOID;
    TlsSlots: array [0..63] of PVOID;
    Reserved3: array [0..78] of Byte;
    Reserved4: array [0..25] of PVOID;
    ReservedForOle: PVOID;  // Windows 2000 only
    Reserved5: array [0..3] of PVOID;
    TlsExpansionSlots: PVOID;
  end;
  TEB = _TEB;
  PTEB = ^TEB;
  TTeb = TEB;

//
// These data structures and type definitions are needed for compilation and
// use of the internal Windows APIs defined in this header.
//

{$IFNDEF JWA_INCLUDEMODE}
type
  NTSTATUS = ULONG;

  PCSZ = PAnsiChar;

  _STRING = record
    Length: USHORT;
    MaximumLength: USHORT;
    Buffer: PAnsiChar;
  end;
  TString = _STRING;
  PString = ^TString;

//typedef STRING *PSTRING;

  ANSI_STRING = _STRING;
  PANSI_STRING = ^ANSI_STRING;
  PCANSI_STRING = PSTRING;

  OEM_STRING = _STRING;
  POEM_STRING = ^OEM_STRING;
{$ENDIF JWA_INCLUDEMODE}

  PCOEM_STRING = POEM_STRING;
  {$IFNDEF JWA_INCLUDEMODE}
  _UNICODE_STRING = record
    Length: USHORT;
    MaximumLength: USHORT;
    Buffer: PWSTR;
  end;
  UNICODE_STRING = _UNICODE_STRING;
  PUNICODE_STRING = ^UNICODE_STRING;
  PCUNICODE_STRING = PUNICODE_STRING;
  TUnicodeString = UNICODE_STRING;
  PUnicodeString = ^TUnicodeString;

  _OBJECT_ATTRIBUTES = record
    Length: ULONG;
    RootDirectory: HANDLE;
    ObjectName: PUNICODE_STRING;
    Attributes: ULONG;
    SecurityDescriptor: PVOID;
    SecurityQualityOfService: PVOID;
  end;
  OBJECT_ATTRIBUTES = _OBJECT_ATTRIBUTES;
  POBJECT_ATTRIBUTES = ^OBJECT_ATTRIBUTES;
  TObjectAttributes = OBJECT_ATTRIBUTES;
  PObjectAttributes = ^TObjectAttributes;


  _IO_STATUS_BLOCK = record
    (*
    union {
        Status: NTSTATUS;
        Pointer: PVOID;
    }; *)
    Status: NTSTATUS;
    Information: ULONG_PTR;
  end;
  IO_STATUS_BLOCK = _IO_STATUS_BLOCK;
  PIO_STATUS_BLOCK = ^IO_STATUS_BLOCK;
  TIoStatusBlock = IO_STATUS_BLOCK;
  PIoStatusBlock = ^TIoStatusBlock;
  {$ENDIF JWA_INCLUDEMODE}

{$IFNDEF JWA_INCLUDEMODE}
type
  PIO_APC_ROUTINE = procedure (ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; Reserved: ULONG); stdcall;
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF _M_IA64}

typedef struct _FRAME_POINTERS {
    ULONGLONG MemoryStackFp;
    ULONGLONG BackingStoreFp;
} FRAME_POINTERS, *PFRAME_POINTERS;

#define UNWIND_HISTORY_TABLE_SIZE 12

typedef struct _RUNTIME_FUNCTION {
    ULONG BeginAddress;
    ULONG EndAddress;
    ULONG UnwindInfoAddress;
} RUNTIME_FUNCTION, *PRUNTIME_FUNCTION;

typedef struct _UNWIND_HISTORY_TABLE_ENTRY {
    ULONG64 ImageBase;
    ULONG64 Gp;
    PRUNTIME_FUNCTION FunctionEntry;
} UNWIND_HISTORY_TABLE_ENTRY, *PUNWIND_HISTORY_TABLE_ENTRY;

typedef struct _UNWIND_HISTORY_TABLE {
    ULONG Count;
    UCHAR Search;
    ULONG64 LowAddress;
    ULONG64 HighAddress;
    UNWIND_HISTORY_TABLE_ENTRY Entry[UNWIND_HISTORY_TABLE_SIZE];
} UNWIND_HISTORY_TABLE, *PUNWIND_HISTORY_TABLE;

{$ENDIF _M_IA64}

type
  _PROCESS_BASIC_INFORMATION = record
    Reserved1: PVOID;
    PebBaseAddress: PPEB;
    Reserved2: array [0..1] of PVOID;
    UniqueProcessId: ULONG_PTR;
    Reserved3: PVOID;
  end;
  PROCESS_BASIC_INFORMATION = _PROCESS_BASIC_INFORMATION;
  PPROCESS_BASIC_INFORMATION = ^PROCESS_BASIC_INFORMATION;
  TProcessBasicInformation = PROCESS_BASIC_INFORMATION;
  PProcessBasicInformation = ^TProcessBasicInformation;

  _SYSTEM_PROCESSOR_PERFORMANCE_INFORMATION = record
    IdleTime: LARGE_INTEGER;
    KernelTime: LARGE_INTEGER;
    UserTime: LARGE_INTEGER;
    Reserved1: array [0..1] of LARGE_INTEGER;
    Reserved2: ULONG;
  end;
  SYSTEM_PROCESSOR_PERFORMANCE_INFORMATION = _SYSTEM_PROCESSOR_PERFORMANCE_INFORMATION;
  PSYSTEM_PROCESSOR_PERFORMANCE_INFORMATION = ^SYSTEM_PROCESSOR_PERFORMANCE_INFORMATION;
  TSystemProcessorPerformanceInformation = SYSTEM_PROCESSOR_PERFORMANCE_INFORMATION;
  PSystemProcessorPerformanceInformation = ^TSystemProcessorPerformanceInformation;

  _SYSTEM_PROCESS_INFORMATION = record
    NextEntryOffset: ULONG;
    Reserved1: array [0..51] of BYTE;
    Reserved2: array [0..2] of PVOID;
    UniqueProcessId: HANDLE;
    Reserved3: PVOID;
    HandleCount: ULONG;
    Reserved4: array [0..3] of BYTE;
    Reserved5: array [0..11] of PVOID;
    PeakPagefileUsage: SIZE_T;
    PrivatePageCount: SIZE_T;
    Reserved6: array [0..5] of LARGE_INTEGER;
  end;
  SYSTEM_PROCESS_INFORMATION = _SYSTEM_PROCESS_INFORMATION;
  PSYSTEM_PROCESS_INFORMATION = ^SYSTEM_PROCESS_INFORMATION;
  TSystemProcessorInformation = SYSTEM_PROCESS_INFORMATION;
  PSystemProcessorInformation = ^TSystemProcessorInformation;

  _SYSTEM_REGISTRY_QUOTA_INFORMATION = record
    RegistryQuotaAllowed: ULONG;
    RegistryQuotaUsed: ULONG;
    Reserved1: PVOID;
  end;
  SYSTEM_REGISTRY_QUOTA_INFORMATION = _SYSTEM_REGISTRY_QUOTA_INFORMATION;
  PSYSTEM_REGISTRY_QUOTA_INFORMATION = ^SYSTEM_REGISTRY_QUOTA_INFORMATION;
  TSystemRegistryInformation = SYSTEM_REGISTRY_QUOTA_INFORMATION;
  PSystemRegistryInformation = ^TSystemRegistryInformation;

  _SYSTEM_BASIC_INFORMATION = record
    Reserved1: array [0..23] of BYTE;
    Reserved2: array [0..3] of PVOID;
    NumberOfProcessors: CCHAR;
  end;
  SYSTEM_BASIC_INFORMATION = _SYSTEM_BASIC_INFORMATION;
  PSYSTEM_BASIC_INFORMATION = ^SYSTEM_BASIC_INFORMATION;
  TSystemBasicInformation = SYSTEM_BASIC_INFORMATION;
  PSystemBasicInformation = ^TSystemBasicInformation;

  _SYSTEM_TIMEOFDAY_INFORMATION = record
    Reserved1: array [0..47] of BYTE;
  end;
  SYSTEM_TIMEOFDAY_INFORMATION = _SYSTEM_TIMEOFDAY_INFORMATION;
  PSYSTEM_TIMEOFDAY_INFORMATION = ^SYSTEM_TIMEOFDAY_INFORMATION;
  TSystemTimeOfDayInformation = SYSTEM_TIMEOFDAY_INFORMATION;
  PSystemTimeOfDayInformation = ^TSystemTimeOfDayInformation;

  _SYSTEM_PERFORMANCE_INFORMATION = record
    Reserved1: array [0..311] of BYTE;
  end;
  SYSTEM_PERFORMANCE_INFORMATION = _SYSTEM_PERFORMANCE_INFORMATION;
  PSYSTEM_PERFORMANCE_INFORMATION = ^SYSTEM_PERFORMANCE_INFORMATION;
  TSystemPerformanceInformation = SYSTEM_PERFORMANCE_INFORMATION;
  PSystemPerformanceInformation = ^TSystemPerformanceInformation;

  _SYSTEM_EXCEPTION_INFORMATION = record
    Reserved1: array [0..15] of BYTE;
  end;
  SYSTEM_EXCEPTION_INFORMATION = _SYSTEM_EXCEPTION_INFORMATION;
  PSYSTEM_EXCEPTION_INFORMATION = ^SYSTEM_EXCEPTION_INFORMATION;
  TSystemExceptionInformation = SYSTEM_EXCEPTION_INFORMATION;
  PSystemExceptionInformation = ^TSystemExceptionInformation;

  _SYSTEM_LOOKASIDE_INFORMATION = record
    Reserved1: array [0..31] of BYTE;
  end;
  SYSTEM_LOOKASIDE_INFORMATION = _SYSTEM_LOOKASIDE_INFORMATION;
  PSYSTEM_LOOKASIDE_INFORMATION = ^SYSTEM_LOOKASIDE_INFORMATION;
  TSystemLookASideInformation = SYSTEM_LOOKASIDE_INFORMATION;
  PSystemLookASideInformation = ^TSystemLookASideInformation;

  _SYSTEM_INTERRUPT_INFORMATION = record
    Reserved1: array [0..23] of BYTE;
  end;
  SYSTEM_INTERRUPT_INFORMATION = _SYSTEM_INTERRUPT_INFORMATION;
  PSYSTEM_INTERRUPT_INFORMATION = ^SYSTEM_INTERRUPT_INFORMATION;
  TSystemInterruptInformation = SYSTEM_INTERRUPT_INFORMATION;
  PSystemInterruptInformation = ^TSystemInterruptInformation;

const
  FileDirectoryInformation = 1;

{$IFNDEF JWA_INCLUDEMODE}
type
  _FILE_INFORMATION_CLASS = DWORD;
  FILE_INFORMATION_CLASS = _FILE_INFORMATION_CLASS;
  TFileInformationClass = FILE_INFORMATION_CLASS;
{$ENDIF JWA_INCLUDEMODE}  

{
const
  ProcessBasicInformation = 0;
  ProcessWow64Information = 26;
}

{$IFNDEF JWA_INCLUDEMODE}
type
{  _PROCESSINFOCLASS = DWORD;}
  _PROCESSINFOCLASS = (
    ProcessBasicInformation {= 0},
    ProcessPad1,
    ProcessPad2,
    ProcessPad3,
    ProcessPad4,
    ProcessPad5,
    ProcessPad6,
    ProcessPad7,
    ProcessPad8,
    ProcessPad9,
    ProcessPad10,
    ProcessPad11,
    ProcessPad12,
    ProcessPad13,
    ProcessPad14,
    ProcessPad15,
    ProcessPad16,
    ProcessPad17,
    ProcessPad18,
    ProcessPad19,
    ProcessPad20,
    ProcessPad21,
    ProcessPad22,
    ProcessPad23,
    ProcessPad24,
    ProcessPad25,
    ProcessWow64Information {= 26}
  );

  PROCESSINFOCLASS = _PROCESSINFOCLASS;
  TProcessInfoClass = PROCESSINFOCLASS;


{$ENDIF JWA_INCLUDEMODE}

{$IFNDEF JWA_INCLUDEMODE}
type
  _THREADINFOCLASS = DWORD;
  THREADINFOCLASS = _THREADINFOCLASS;
  TThreadInfoClass = THREADINFOCLASS;
{$ENDIF JWA_INCLUDEMODE}

const
  SystemBasicInformation = 0;
  SystemPerformanceInformation = 2;
  SystemTimeOfDayInformation = 3;
  SystemProcessInformation = 5;
  SystemProcessorPerformanceInformation = 8;
  SystemInterruptInformation = 23;
  SystemExceptionInformation = 33;
  SystemRegistryQuotaInformation = 37;
  SystemLookasideInformation = 45;

{$IFNDEF JWA_INCLUDEMODE}
type
  _SYSTEM_INFORMATION_CLASS = DWORD;
  SYSTEM_INFORMATION_CLASS = _SYSTEM_INFORMATION_CLASS;
{$ENDIF JWA_INCLUDEMODE}  

{$IFDEF WINXP}

//#if (_WIN32_WINNT >= 0x0501)

//
// use the WTS API instead
//     WTSGetActiveConsoleSessionId
// The active console id is cached as a volatile ULONG in a constant
// memory location.  This x86 memory location is subject to changes between
// Windows releases.  Use the WTS API to make your application resilient to
// changes.
//

function INTERNAL_TS_ACTIVE_CONSOLE_ID: ULONG;

{$ENDIF WINXP}

//
// These functions are intended for use by internal core Windows components
// since these functions may change between Windows releases.
//

//todo #define RtlFillMemory(Destination,Length,Fill) memset((Destination),(Fill),(Length))
//todo #define RtlZeroMemory(Destination,Length) memset((Destination),0,(Length))
//todo #define RtlMoveMemory(Destination,Source,Length) memmove((Destination),(Source),(Length))

//
// use the Win32 API instead
//     CloseHandle
//

function NtClose(_Handle: HANDLE): NTSTATUS; stdcall; {$IFNDEF DYNAMIC_LINK} external winternl_lib name 'NtClose'; {$ENDIF}

//
// use the Win32 API instead
//     CreateFile
//

function NtCreateFile(
  FileHandle: PHANDLE;
  DesiredAccess: ACCESS_MASK;
  ObjectAttributes: POBJECT_ATTRIBUTES;
  IoStatusBlock: PIO_STATUS_BLOCK;
  AllocationSize: PLARGE_INTEGER;
  FileAttributes: ULONG;
  ShareAccess: ULONG;
  CreateDisposition: ULONG;
  CreateOptions: ULONG;
  EaBuffer: PVOID;
  EaLength: ULONG): NTSTATUS; stdcall; {$IFNDEF DYNAMIC_LINK} external winternl_lib name 'NtCreateFile'; {$ENDIF}

//
// use the Win32 API instead
//     CreateFile
//

function NtOpenFile(
  FileHandle: PHANDLE;
  DesiredAccess: ACCESS_MASK;
  ObjectAttributes: POBJECT_ATTRIBUTES;
  IoStatusBlock: PIO_STATUS_BLOCK;
  ShareAccess: ULONG;
  OpenOptions: ULONG): NTSTATUS; stdcall; {$IFNDEF DYNAMIC_LINK} external winternl_lib name 'NtOpenFile'; {$ENDIF}

//
// use the Win32 API instead
//     DeviceIoControl
//

function NtDeviceIoControlFile(
  FileHandle: HANDLE;
  Event: HANDLE;
  ApcRoutine: PIO_APC_ROUTINE;
  ApcContext: PVOID;
  IoStatusBlock: PIO_STATUS_BLOCK;
  IoControlCode: ULONG;
  InputBuffer: PVOID;
  InputBufferLength: ULONG;
  OutputBuffer: PVOID;
  OutputBufferLength: ULONG): NTSTATUS; stdcall; {$IFNDEF DYNAMIC_LINK} external winternl_lib name 'NtDeviceIoControlFile'; {$ENDIF}

//
// use the Win32 API instead
//     WaitForSingleObjectEx
//

function NtWaitForSingleObject(
  Handle: HANDLE;
  Alertable: BOOLEAN;
  Timeout: PLARGE_INTEGER): NTSTATUS; stdcall; {$IFNDEF DYNAMIC_LINK} external winternl_lib name 'NtWaitForSingleObject'; {$ENDIF}

//
// use the Win32 API instead
//     CheckNameLegalDOS8Dot3
//

function RtlIsNameLegalDOS8Dot3(
  Name: PUNICODE_STRING;
  OemName: POEM_STRING;
  NameContainsSpaces: PBOOLEAN): BOOLEAN; stdcall; {$IFNDEF DYNAMIC_LINK} external winternl_lib name 'RtlIsNameLegalDOS8Dot3'; {$ENDIF}

//
// This function might be needed for some of the internal Windows functions,
// defined in this header file.
//

function RtlNtStatusToDosError(Status: NTSTATUS): ULONG; stdcall; {$IFNDEF DYNAMIC_LINK} external winternl_lib name 'RtlNtStatusToDosError'; {$ENDIF}

//
// use the Win32 APIs instead
//     GetProcessHandleCount
//     GetProcessId
//

function NtQueryInformationProcess(
  ProcessHandle: HANDLE;
  ProcessInformationClass: PROCESSINFOCLASS;
  ProcessInformation: PVOID;
  ProcessInformationLength: ULONG;
  ReturnLength: PULONG): NTSTATUS; stdcall; {$IFNDEF DYNAMIC_LINK} external winternl_lib name 'NtQueryInformationProcess'; {$ENDIF}

//
// use the Win32 API instead
//     GetThreadIOPendingFlag
//

function NtQueryInformationThread(
  ThreadHandle: HANDLE;
  ThreadInformationClass: THREADINFOCLASS;
  ThreadInformation: PVOID;
  ThreadInformationLength: ULONG;
  ReturnLength: PULONG): NTSTATUS; stdcall; {$IFNDEF DYNAMIC_LINK} external winternl_lib name 'NtQueryInformationThread'; {$ENDIF}

//
// use the Win32 APIs instead
//     GetSystemRegistryQuota
//     GetSystemTimes
// use the CryptoAPIs instead for generating random data
//     CryptGenRandom
//

function NtQuerySystemInformation(
  SystemInformationClass: SYSTEM_INFORMATION_CLASS;
  SystemInformation: PVOID;
  SystemInformationLength: ULONG;
  ReturnLength: PULONG): NTSTATUS; stdcall; {$IFNDEF DYNAMIC_LINK} external winternl_lib name 'NtQuerySystemInformation'; {$ENDIF}

//
// use the Win32 API instead
//     GetSystemTimeAsFileTime
//

function NtQuerySystemTime(SystemTime: PLARGE_INTEGER): NTSTATUS; stdcall; {$IFNDEF DYNAMIC_LINK} external winternl_lib name 'NtQuerySystemTime'; {$ENDIF}

//
// use the Win32 API instead
//     LocalFileTimeToFileTime
//

function RtlLocalTimeToSystemTime(
  LocalTime: PLARGE_INTEGER;
  SystemTime: PLARGE_INTEGER): NTSTATUS; stdcall; {$IFNDEF DYNAMIC_LINK} external winternl_lib name 'RtlLocalTimeToSystemTime'; {$ENDIF}

//
// use the Win32 API instead
//     SystemTimeToFileTime to convert to FILETIME structures
//     copy the resulting FILETIME structures to ULARGE_INTEGER structures
//     perform the calculation
//

function RtlTimeToSecondsSince1970(
  Time: PLARGE_INTEGER;
  ElapsedSeconds: PULONG): BOOLEAN; stdcall;{$IFNDEF DYNAMIC_LINK} external winternl_lib name 'RtlTimeToSecondsSince1970'; {$ENDIF}

//
// These APIs might be need for some of the internal Windows functions,
// defined in this header file.
//

{$IFNDEF JWA_INCLUDEMODE}
procedure RtlFreeAnsiString(AnsiString: PANSI_STRING); stdcall;
procedure RtlFreeUnicodeString(UnicodeString: PUNICODE_STRING); stdcall;
procedure RtlFreeOemString(OemString: POEM_STRING); stdcall;
procedure RtlInitString(DestinationString: PSTRING; SourceString: PCSZ); stdcall;
procedure RtlInitAnsiString(DestinationString: PANSI_STRING; SourceString: PCSZ); stdcall;
procedure RtlInitUnicodeString(DestinationString: PUNICODE_STRING; SourceString: PWSTR); stdcall;
{$ENDIF JWA_INCLUDEMODE}

function RtlAnsiStringToUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: PCANSI_STRING;
  AllocateDestinationString: BOOLEAN): NTSTATUS; stdcall; {$IFNDEF DYNAMIC_LINK} external winternl_lib name 'RtlAnsiStringToUnicodeString'; {$ENDIF}

function RtlUnicodeStringToAnsiString(
  DestinationString: PANSI_STRING;
  SourceString: PCUNICODE_STRING;
  AllocateDestinationString: BOOLEAN): NTSTATUS; stdcall; {$IFNDEF DYNAMIC_LINK} external winternl_lib name 'RtlUnicodeStringToAnsiString'; {$ENDIF}

function RtlUnicodeStringToOemString(
  DestinationString: POEM_STRING;
  SourceString: PCUNICODE_STRING;
  AllocateDestinationString: BOOLEAN): NTSTATUS; stdcall; {$IFNDEF DYNAMIC_LINK} external winternl_lib name 'RtlUnicodeStringToOemString'; {$ENDIF}

//
// Use the Win32 API instead
//     WideCharToMultiByte
//     set CodePage to CP_ACP
//     set cbMultiByte to 0
//

function RtlUnicodeToMultiByteSize(
  BytesInMultiByteString: PULONG;
  UnicodeString: PWSTR;
  BytesInUnicodeString: ULONG): NTSTATUS; stdcall; {$IFNDEF DYNAMIC_LINK} external winternl_lib name 'RtlUnicodeToMultiByteSize'; {$ENDIF}

//
// Use the C runtime function instead
//     strtol
//

function RtlCharToInteger(Str: PCSZ; Base: ULONG; Value: PULONG): NTSTATUS; stdcall; {$IFNDEF DYNAMIC_LINK} external winternl_lib name 'RtlCharToInteger'; {$ENDIF}

//
// use the Win32 API instead
//     ConvertSidToStringSid
//

function RtlConvertSidToUnicodeString(
  UnicodeString: PUNICODE_STRING;
  Sid: PSID;
  AllocateDestinationString: BOOLEAN): NTSTATUS; stdcall; {$IFNDEF DYNAMIC_LINK} external winternl_lib name 'RtlConvertSidToUnicodeString'; {$ENDIF}

//
// use the CryptoAPIs instead
//     CryptGenRandom
//

function RtlUniform(Seed: PULONG): ULONG; stdcall;

//
// Use the default built-in system exception handling instead of these
// functions
//

{$IFNDEF JWA_INCLUDEMODE}
procedure RtlUnwind(
    TargetFrame: PVOID;
    TargetIp: PVOID;
    ExceptionRecord: PEXCEPTION_RECORD;
    ReturnValue: PVOID); stdcall;
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF _M_IA64}

VOID
RtlUnwind2 (
    IN FRAME_POINTERS TargetFrame OPTIONAL,
    IN PVOID TargetIp OPTIONAL,
    IN PEXCEPTION_RECORD ExceptionRecord OPTIONAL,
    IN PVOID ReturnValue,
    IN PCONTEXT ContextRecord
    );

VOID
RtlUnwindEx (
    IN FRAME_POINTERS TargetFrame OPTIONAL,
    IN PVOID TargetIp OPTIONAL,
    IN PEXCEPTION_RECORD ExceptionRecord OPTIONAL,
    IN PVOID ReturnValue,
    IN PCONTEXT ContextRecord,
    IN PUNWIND_HISTORY_TABLE HistoryTable OPTIONAL
    );

{$ENDIF _M_IA64}

const
  LOGONID_CURRENT    = ULONG(-1);
  SERVERNAME_CURRENT = 0;

  WinStationInformation = 8;

type
  _WINSTATIONINFOCLASS = DWORD;
  WINSTATIONINFOCLASS = _WINSTATIONINFOCLASS;
  TWinStationInfoClass = WINSTATIONINFOCLASS;

  _WINSTATIONINFORMATIONW = record
    Reserved2: array [0..69] of Byte;
    LogonId: ULONG;
    Reserved3: array [0..1139] of Byte;
  end;
  WINSTATIONINFORMATIONW = _WINSTATIONINFORMATIONW;
  PWINSTATIONINFORMATIONW = ^WINSTATIONINFORMATIONW;
  TWinStationInformationW = WINSTATIONINFORMATIONW;

//
// this function is implemented in winsta.dll (you need to loadlibrary to call this function)
// this internal function retrives the LogonId (also called SessionId) for the current process
// You should avoid using this function as it can change. you can retrieve the same information 
// Using public api WTSQuerySessionInformation. Pass WTSSessionId as the WTSInfoClass parameter
//

type
  PWINSTATIONQUERYINFORMATIONW = function (p1: HANDLE; p2: ULONG; p3: WINSTATIONINFOCLASS; p4: PVOID; p5: ULONG; p6: PULONG): BOOLEAN; stdcall;

{$ENDIF JWA_IMPLEMENTATIONSECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
implementation

{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}


{$IFDEF WINXP}

function INTERNAL_TS_ACTIVE_CONSOLE_ID: ULONG;
begin
  Result := PULONG($7ffe02d8)^; // ( *((volatile ULONG*)(0x7ffe02d8)) )
end;

{$ENDIF WINXP}

{$IFDEF DYNAMIC_LINK}

var _NtClose: Pointer;

function NtClose;
begin
  GetProcedureAddress(_NtClose, winternl_lib, 'NtClose');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NtClose]
  end;
end;

var _NtCreateFile: Pointer;

function NtCreateFile;
begin
  GetProcedureAddress(_NtCreateFile, winternl_lib, 'NtCreateFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NtCreateFile]
  end;
end;

var _NtOpenFile: Pointer;


function NtOpenFile;
begin
  GetProcedureAddress(_NtOpenFile, winternl_lib, 'NtOpenFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NtOpenFile]
  end;
end;

var _NtDeviceIoControlFile: Pointer;


function NtDeviceIoControlFile;
begin
  GetProcedureAddress(_NtDeviceIoControlFile, winternl_lib, 'NtDeviceIoControlFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NtDeviceIoControlFile]
  end;
end;

var _NtWaitForSingleObject: Pointer;


function NtWaitForSingleObject;
begin
  GetProcedureAddress(_NtWaitForSingleObject, winternl_lib, 'NtWaitForSingleObject');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NtWaitForSingleObject]
  end;
end;
 
var _RtlIsNameLegalDOS8Dot3: Pointer;


function RtlIsNameLegalDOS8Dot3;
begin
  GetProcedureAddress(_RtlIsNameLegalDOS8Dot3, winternl_lib, 'RtlIsNameLegalDOS8Dot3');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RtlIsNameLegalDOS8Dot3]
  end;
end;
 
var _RtlNtStatusToDosError: Pointer;


function RtlNtStatusToDosError;
begin
  GetProcedureAddress(_RtlNtStatusToDosError, winternl_lib, 'RtlNtStatusToDosError');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RtlNtStatusToDosError]
  end;
end;
    
var _NtQueryInformationProcess: Pointer;


function NtQueryInformationProcess;
begin
  GetProcedureAddress(_NtQueryInformationProcess, winternl_lib, 'NtQueryInformationProcess');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NtQueryInformationProcess]
  end;
end;
 
var _NtQueryInformationThread: Pointer;


function NtQueryInformationThread;
begin
  GetProcedureAddress(_NtQueryInformationThread, winternl_lib, 'NtQueryInformationThread');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NtQueryInformationThread]
  end;
end;
 
var _NtQuerySystemInformation: Pointer;


function NtQuerySystemInformation;
begin
  GetProcedureAddress(_NtQuerySystemInformation, winternl_lib, 'NtQuerySystemInformation');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NtQuerySystemInformation]
  end;
end;

var _NtQuerySystemTime: Pointer;


function NtQuerySystemTime;
begin
  GetProcedureAddress(_NtQuerySystemTime, winternl_lib, 'NtQuerySystemTime');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NtQuerySystemTime]
  end;
end;

var _RtlLocalTimeToSystemTime: Pointer;


function RtlLocalTimeToSystemTime;
begin
  GetProcedureAddress(_RtlLocalTimeToSystemTime, winternl_lib, 'RtlLocalTimeToSystemTime');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RtlLocalTimeToSystemTime]
  end;
end;

var _RtlTimeToSecondsSince1970: Pointer;

function RtlTimeToSecondsSince1970;
begin
  GetProcedureAddress(_RtlTimeToSecondsSince1970, winternl_lib, 'RtlTimeToSecondsSince1970');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RtlTimeToSecondsSince1970]
  end;
end;

{$IFNDEF JWA_INCLUDEMODE}

var _RtlFreeAnsiString: Pointer;

procedure RtlFreeAnsiString;
begin
  GetProcedureAddress(_RtlFreeAnsiString, winternl_lib, 'RtlFreeAnsiString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RtlFreeAnsiString]
  end;
end;

var _RtlFreeUnicodeString: Pointer;


procedure RtlFreeUnicodeString;
begin
  GetProcedureAddress(_RtlFreeUnicodeString, winternl_lib, 'RtlFreeUnicodeString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RtlFreeUnicodeString]
  end;
end;

var _RtlFreeOemString: Pointer;


procedure RtlFreeOemString;
begin
  GetProcedureAddress(_RtlFreeOemString, winternl_lib, 'RtlFreeOemString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RtlFreeOemString]
  end;
end;

var _RtlInitString: Pointer;


procedure RtlInitString;
begin
  GetProcedureAddress(_RtlInitString, winternl_lib, 'RtlInitString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RtlInitString]
  end;
end;

var _RtlInitAnsiString: Pointer;


procedure RtlInitAnsiString;
begin
  GetProcedureAddress(_RtlInitAnsiString, winternl_lib, 'RtlInitAnsiString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RtlInitAnsiString]
  end;
end;

var _RtlInitUnicodeString: Pointer;


procedure RtlInitUnicodeString;
begin
  GetProcedureAddress(_RtlInitUnicodeString, winternl_lib, 'RtlInitUnicodeString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RtlInitUnicodeString]
  end;
end;

{$ENDIF JWA_INCLUDEMODE}

var _RtlAnsiStringToUnicodeString: Pointer;


function RtlAnsiStringToUnicodeString;
begin
  GetProcedureAddress(_RtlAnsiStringToUnicodeString, winternl_lib, 'RtlAnsiStringToUnicodeString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RtlAnsiStringToUnicodeString]
  end;
end;

var _RtlUnicodeStringToAnsiString: Pointer;


function RtlUnicodeStringToAnsiString;
begin
  GetProcedureAddress(_RtlUnicodeStringToAnsiString, winternl_lib, 'RtlUnicodeStringToAnsiString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RtlUnicodeStringToAnsiString]
  end;
end;

var _RtlUnicodeStringToOemString: Pointer;


function RtlUnicodeStringToOemString;
begin
  GetProcedureAddress(_RtlUnicodeStringToOemString, winternl_lib, 'RtlUnicodeStringToOemString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RtlUnicodeStringToOemString]
  end;
end;

var _RtlUnicodeToMultiByteSize: Pointer;


function RtlUnicodeToMultiByteSize;
begin
  GetProcedureAddress(_RtlUnicodeToMultiByteSize, winternl_lib, 'RtlUnicodeToMultiByteSize');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RtlUnicodeToMultiByteSize]
  end;
end;

var _RtlCharToInteger: Pointer;


function RtlCharToInteger;
begin
  GetProcedureAddress(_RtlCharToInteger, winternl_lib, 'RtlCharToInteger');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RtlCharToInteger]
  end;
end;

{$IFNDEF JWA_INCLUDEMODE}

var _RtlConvertSidToUnicodeString: Pointer;

function RtlConvertSidToUnicodeString;
begin
  GetProcedureAddress(_RtlConvertSidToUnicodeString, winternl_lib, 'RtlConvertSidToUnicodeString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RtlConvertSidToUnicodeString]
  end;
end;

var _RtlUniform: Pointer;

function RtlUniform;
begin
  GetProcedureAddress(_RtlUniform, winternl_lib, 'RtlUniform');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RtlUniform]
  end;
end;

var _RtlUnwind: Pointer;

procedure RtlUnwind;
begin
  GetProcedureAddress(_RtlUnwind, winternl_lib, 'RtlUnwind');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RtlUnwind]
  end;
end;

{$ENDIF JWA_INCLUDEMODE}

{$ELSE}
(*
function NtClose; external winternl_lib name 'NtClose';
function NtCreateFile; external winternl_lib name 'NtCreateFile';
function NtOpenFile; external winternl_lib name 'NtOpenFile';
function NtDeviceIoControlFile; external winternl_lib name 'NtDeviceIoControlFile';
function NtWaitForSingleObject; external winternl_lib name 'NtWaitForSingleObject';
function RtlIsNameLegalDOS8Dot3; external winternl_lib name 'RtlIsNameLegalDOS8Dot3';
function RtlNtStatusToDosError; external winternl_lib name 'RtlNtStatusToDosError';
function NtQueryInformationProcess; external winternl_lib name 'NtQueryInformationProcess';
function NtQueryInformationThread; external winternl_lib name 'NtQueryInformationThread';
function NtQuerySystemInformation; external winternl_lib name 'NtQuerySystemInformation';
function NtQuerySystemTime; external winternl_lib name 'NtQuerySystemTime';
function RtlLocalTimeToSystemTime; external winternl_lib name 'RtlLocalTimeToSystemTime';
function RtlTimeToSecondsSince1970; external winternl_lib name 'RtlTimeToSecondsSince1970';
*)

procedure RtlFreeAnsiString; external winternl_lib name 'RtlFreeAnsiString';
procedure RtlFreeUnicodeString; external winternl_lib name 'RtlFreeUnicodeString';
procedure RtlFreeOemString; external winternl_lib name 'RtlFreeOemString';
procedure RtlInitString; external winternl_lib name 'RtlInitString';
procedure RtlInitAnsiString; external winternl_lib name 'RtlInitAnsiString';
procedure RtlInitUnicodeString; external winternl_lib name 'RtlInitUnicodeString';

(*
{function RtlAnsiStringToUnicodeString; external winternl_lib name 'RtlAnsiStringToUnicodeString';
function RtlUnicodeStringToAnsiString; external winternl_lib name 'RtlUnicodeStringToAnsiString';
function RtlUnicodeStringToOemString; external winternl_lib name 'RtlUnicodeStringToOemString';
function RtlUnicodeToMultiByteSize; external winternl_lib name 'RtlUnicodeToMultiByteSize';
function RtlCharToInteger; external winternl_lib name 'RtlCharToInteger';}

function RtlConvertSidToUnicodeString; external winternl_lib name 'RtlConvertSidToUnicodeString';

*)
procedure RtlUnwind; external winternl_lib name 'RtlUnwind';

function RtlUniform; external winternl_lib name 'RtlUniform';

{$ENDIF DYNAMIC_LINK}

{$IFDEF _M_IA64}

RtlUnwind2
RtlUnwindEx

{$ENDIF _M_IA64}

{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
