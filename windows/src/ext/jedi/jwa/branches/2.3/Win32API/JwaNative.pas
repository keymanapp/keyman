{******************************************************************************}
{                                                                              }
{ Interface unit for the Windows NT Native API                                 }
{ Copyright (C) 1999, 2000, 2005 Marcel van Brakel (brakelm)                   }
{ Copyright (C) 2000-2001, 2005 Oliver Schneider (assarbad)                    }
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

// $Id: JwaNative.pas,v 1.24 2007/09/14 06:48:46 marquardt Exp $

{******************************************************************************}
{** WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING  **}
{******************************************************************************}
{**                                                                          **}
{** The prototypes, declarations and information in this file has been       **}
{** compiled from various sources as well as through reverse engineering     **}
{** techniques. We make no guarantee as to the correctness of the contents.  **}
{** Caution is recommended, USE AT YOUR OWN RISK.                            **}
{**                                                                          **}
{******************************************************************************}
{** About the Native API						     **                                          }
{******************************************************************************}
{**                                                                          **}
{** The functions herein are usually referred to as the NT Native API.       **}
{** The Native API is subdivided into several functional categories, which   **}
{** you can distinguish by the function name prefix:                         **}
{**                                                                          **}
{**   Cc   = Cache Controller                                                **}
{**   Cm   = Configuration Manager                                           **}
{**   Csr  = Client Server support functions (LPC; related: CSRSS.EXE)       **}
{**   Dbg  = Debugger support functions                                      **}
{**   Etw  = Event Tracing for Windows                                       **}
{**   Ex   = Executive                                                       **}
{**   Fs   = File system support functions                                   **}
{**   Hal  = Hardware abstraction layer functions                            **}
{**   Inbv = Something like: _In_itial _B_oot _V_ideo functions              **}
{**   Io   = I/O manager support functions                                   **}
{**   Kd   = Kernel debugger support functions                               **}
{**   Ke   = General Kernel                                                  **}
{**   Ki   = Kernel internal support functions (???)                         **}
{**   Ldr  = PE image loader support functions                               **}
{**   Lpc  = LPC support functions                                           **}
{**   Lsa  = Local security authority support functions                      **}
{**   Mm   = Memory manager support functions                                **}
{**   Nls  = National Language Support                                       **}
{**   Nt   = Generic Native APIs                                             **}
{**   Ob   = Object manager functions                                        **}
{**   Pfx  = Name prefix support functions (???)                             **}
{**   Po   = Power management support functions                              **}
{**   Ps   = Process management support functions                            **}
{**   Rtl  = Runtime library functions                                       **}
{**   Rtlp = Private runtime library functions 1)                            **}
{**   Se   = Security support functions                                      **}
{**   Wmi  = Windows management instrumentation support functions            **}
{**   Vf   = Driver Verifier                                                 **}
{**   Zw   = Nt* counterparts. Zw == "Zero Warranty"???                      **}
{**          1) "p" after the prefix means "private"                         **}
{**                                                                          **}
{** The Native API is split into a user mode component (mainly NTDLL.DLL)    **}
{** and a kernel mode component (mainly NTOSKRNL.EXE). While a large part of **}
{** the Native API is available both from usermode and kernelmode, some      **}
{** functions are exclusive to either mode. This unit only deals with 	     **}
{** functions that are available to usermode code.                           **}
{**                                                                          **}
{** Note that the functions prefixed with "Nt" and "Zw" usually appear in    **}
{** pairs, though not always! For details see http://assarbad.net    			**}
{**                                                                          **}
{** Most of the Native API is undocumented. However, Microsoft recently      **}
{** started to document a subset of the API in "winternl.h" in the Platform  **}
{** SDK. A small part of the Native API functions, specifically those useful **}
{** for kernel mode development (device drivers) are documented in the DDK.  **}
{**                                                                          **}
{******************************************************************************}
{** Special notes                                                            **}
{******************************************************************************}
{**                                                                          **}
{** Some functions herein have been implemented instead of being imported.   **}
{** That's due to the fact, that the FASTCALL calling convention is not      **}
{** available in Delphi. These functions include:                            **}
{** - RtlUshortByteSwap()                                                    **}
{** - RtlUlongByteSwap()                                                     **}
{** - RtlUlonglongByteSwap()                                                 **}
{**                                                                          **}
{** Other functions are implemented and also imported, but have been made    **}
{** available for OS versions not actually supporting them. These are:       **}
{** - RtlGetLastWin32Error()                                                 **}
{** - RtlSetLastWin32Error()                                                 **}
{**                                                                          **}
{** Starting with Windows XP, the Kernel32!GetLastError() and counterpart    **}
{** Kernel32!SetLastError() have been moved into NTDLL and are only function **}
{** forwarders to the NTDLL functions with the names above.                  **}
{** By importing them directly from Kernel32.dll via their old names we      **}
{** enable you to use these functions transparently without regard to the OS **}
{** version.                                                                 **}
{**                                                                          **}
{** RtlCopyMemory() had to be implemented via Delphi means because it's only **}
{** a macro (pointing to memcpy) in the C-world.                             **}
{**                                                                          **}
{** Last but not least we've implemented our own versions of some of the     **}
{** functions available through NTDLL [declared private Ntp*, Rtlp*]:        **}
{** - NtpGetProcessHeap()                                                    **}
{** - NtpCurrentTeb()                                                        **}
{** - RtlpGetCurrentPeb()                                                    **}
{**                                                                          **}
{** Plus a function that is available starting from Windows XP, but could be **}
{** useful on earlier versions as well [declared private Rtlp*]:             **}
{** - RtlpValidateUnicodeString()                                            **}
{**                                                                          **}
{** And our own flavor of it, omitting the first (currently unused)          **}
{** parameter:                                                               **}
{** - RtlpValidateUnicodeString2()                                           **}
{**                                                                          **}
{******************************************************************************}
{** References, Tools, Docs                                                  **}
{******************************************************************************}
{**                                                                          **}
{** - Windows NT/2000 Native API References (Gary Nebbett)                   **}
{**   ISBN 1-57870-199-6                                                     **}
{** - Undocumented Windows 2000 Secrets (Sven B. Schreiber)                  **}
{**   ISBN 0-201-72187-2                                                     **}
{** - Undocumented Windows NT (Prasad Dabak, Sandeep Phadke, Milind Borate)  **}
{**   ISBN 0-7645-4569-8                                                     **}
{** - Platform SDK for Windows 2003 Server (or later version)                **}
{**   http://www.microsoft.com/msdownload/platformsdk/sdkupdate/             **}
{** - Windows 2003 DDK (or similar DDK version                               **}
{**   http://www.microsoft.com/whdc/ddk/                                     **}
{** - WinDbg - a debugger that is usually badly underestimated!              **}
{**   http://www.microsoft.com/whdc/devtools/debugging                       **}
{** - IDA Pro Standard 4.7 (or later) - world's best disassembler            **}
{**   http://www.datarescue.com/idabase/                                     **}
{** - NTDEV, NTFSD, WINDBG mailing lists and more ...                        **}
{**   http://www.osronline.com/                                              **}
{** - Sysinternals tools and documentation of some "Windows secrets"         **}
{**   http://www.sysinternals.com/                                           **}
{** - A nicely done online compilation of NT Native APIs                     **}
{**   http://undocumented.ntinternals.net/                                   **}
{** - ReactOS (to cross-check own assumptions with those of other smart guys)**}
{**   http://www.reactos.com/                                                **}
{**                                                                          **}
{******************************************************************************}
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaNative;

interface
{$INCLUDE jediapilib.inc}

uses
  JwaWinType, JwaWinNT, JwaWinBase, JwaNtStatus, JwaBitFields;

{$WEAKPACKAGEUNIT}

// For native APIs we consider RTDL the better method of importing
{.$DEFINE RTDL}
{$IFDEF RTDL}{$DEFINE DYNAMIC_LINK}{$ENDIF}
const
  ntdll = 'ntdll.dll';

//------------------------------------------------------------------------------

{$ENDIF JWA_OMIT_SECTIONS}
{$IFNDEF JWA_IMPLEMENTATIONSECTION}


  
type
 
  _CLIENT_ID = record
    UniqueProcess: HANDLE;
    UniqueThread: HANDLE;
  end;
  CLIENT_ID = _CLIENT_ID;
  PCLIENT_ID = ^CLIENT_ID;
  TClientID = CLIENT_ID;
  PClientID = ^TClientID;

  KPRIORITY = LONG;

  _KWAIT_REASON = (
    Executive,
    FreePage,
    PageIn,
    PoolAllocation,
    DelayExecution,
    Suspended,
    UserRequest,
    WrExecutive,
    WrFreePage,
    WrPageIn,
    WrPoolAllocation,
    WrDelayExecution,
    WrSuspended,
    WrUserRequest,
    WrEventPair,
    WrQueue,
    WrLpcReceive,
    WrLpcReply,
    WrVirtualMemory,
    WrPageOut,
    WrRendezvous,
    Spare2,
    Spare3,
    Spare4,
    Spare5,
    Spare6,
    WrKernel,
    MaximumWaitReason);
  KWAIT_REASON = _KWAIT_REASON;
  TKWaitReason = KWAIT_REASON;

  _VM_COUNTERS = record
    PeakVirtualSize: SIZE_T;
    VirtualSize: SIZE_T;
    PageFaultCount: ULONG;
    PeakWorkingSetSize: SIZE_T;
    WorkingSetSize: SIZE_T;
    QuotaPeakPagedPoolUsage: SIZE_T;
    QuotaPagedPoolUsage: SIZE_T;
    QuotaPeakNonPagedPoolUsage: SIZE_T;
    QuotaNonPagedPoolUsage: SIZE_T;
    PagefileUsage: SIZE_T;
    PeakPagefileUsage: SIZE_T;
  end;
  VM_COUNTERS = _VM_COUNTERS;
  PVM_COUNTERS = ^VM_COUNTERS;
  TVmCounters = VM_COUNTERS;
  PVmCounters = ^TVmCounters;

const
  NonPagedPool = 0;
  PagedPool = 1;
  NonPagedPoolMustSucceed = 2;
  DontUseThisType = 3;
  NonPagedPoolCacheAligned = 4;
  PagedPoolCacheAligned = 5;
  NonPagedPoolCacheAlignedMustS = 6;
  MaxPoolType = 7;
  NonPagedPoolSession = 32;
  PagedPoolSession = NonPagedPoolSession + 1;
  NonPagedPoolMustSucceedSession = PagedPoolSession + 1;
  DontUseThisTypeSession = NonPagedPoolMustSucceedSession + 1;
  NonPagedPoolCacheAlignedSession = DontUseThisTypeSession + 1;
  PagedPoolCacheAlignedSession = NonPagedPoolCacheAlignedSession + 1;
  NonPagedPoolCacheAlignedMustSSession = PagedPoolCacheAlignedSession + 1;

type
  POOL_TYPE = NonPagedPool..NonPagedPoolCacheAlignedMustSSession;

  _IO_STATUS_BLOCK = record
    //union {
    Status: NTSTATUS;
    //    PVOID Pointer;
    //}
    Information: ULONG_PTR;
  end;
  IO_STATUS_BLOCK = _IO_STATUS_BLOCK;
  PIO_STATUS_BLOCK = ^IO_STATUS_BLOCK;
  TIoStatusBlock = IO_STATUS_BLOCK;
  PIoStatusBlock = ^TIoStatusBlock;


const
  ViewShare = 1;
  ViewUnmap = 2;

type
  SECTION_INHERIT = ViewShare..ViewUnmap;

  {.$IFNDEF JWA_INCLUDEMODE}
  _THREADINFOCLASS = (
    ThreadBasicInformation,
    ThreadTimes,
    ThreadPriority,
    ThreadBasePriority,
    ThreadAffinityMask,
    ThreadImpersonationToken,
    ThreadDescriptorTableEntry,
    ThreadEnableAlignmentFaultFixup,
    ThreadEventPair_Reusable,
    ThreadQuerySetWin32StartAddress,
    ThreadZeroTlsCell,
    ThreadPerformanceCount,
    ThreadAmILastThread,
    ThreadIdealProcessor,
    ThreadPriorityBoost,
    ThreadSetTlsArrayAddress,
    ThreadIsIoPending,
    ThreadHideFromDebugger,
    ThreadBreakOnTermination, // was added in XP - used by RtlSetThreadIsCritical()
    MaxThreadInfoClass);
  THREADINFOCLASS = _THREADINFOCLASS;
  {.$ENDIF JWA_INCLUDEMODE}
  THREAD_INFORMATION_CLASS = THREADINFOCLASS;


  TThreadInfoClass = THREADINFOCLASS;

{$IFNDEF JWA_INCLUDEMODE}
  KAFFINITY = ULONG;
  PKAFFINITY = ^KAFFINITY;
{$ENDIF JWA_INCLUDEMODE}

  PKNORMAL_ROUTINE = procedure(NormalContext, SystemArgument1, SystemArgument2: PVOID); stdcall;


  _PROCESSINFOCLASS = (
    ProcessBasicInformation,
    ProcessQuotaLimits,
    ProcessIoCounters,
    ProcessVmCounters,
    ProcessTimes,
    ProcessBasePriority,
    ProcessRaisePriority,
    ProcessDebugPort,
    ProcessExceptionPort,
    ProcessAccessToken,
    ProcessLdtInformation,
    ProcessLdtSize,
    ProcessDefaultHardErrorMode,
    ProcessIoPortHandlers, // Note: this is kernel mode only
    ProcessPooledUsageAndLimits,
    ProcessWorkingSetWatch,
    ProcessUserModeIOPL,
    ProcessEnableAlignmentFaultFixup,
    ProcessPriorityClass,
    ProcessWx86Information,
    ProcessHandleCount,
    ProcessAffinityMask,
    ProcessPriorityBoost,
    ProcessDeviceMap,
    ProcessSessionInformation,
    ProcessForegroundInformation,
    ProcessWow64Information, // = 26
    ProcessImageFileName, // added after W2K
    ProcessLUIDDeviceMapsEnabled,
    ProcessBreakOnTermination, // used by RtlSetProcessIsCritical()
    ProcessDebugObjectHandle,
    ProcessDebugFlags,
    ProcessHandleTracing,
    MaxProcessInfoClass);
  PROCESSINFOCLASS = _PROCESSINFOCLASS;
  PROCESS_INFORMATION_CLASS = PROCESSINFOCLASS;
  TProcessInfoClass = PROCESSINFOCLASS;

  _KPROFILE_SOURCE = (
    ProfileTime,
    ProfileAlignmentFixup,
    ProfileTotalIssues,
    ProfilePipelineDry,
    ProfileLoadInstructions,
    ProfilePipelineFrozen,
    ProfileBranchInstructions,
    ProfileTotalNonissues,
    ProfileDcacheMisses,
    ProfileIcacheMisses,
    ProfileCacheMisses,
    ProfileBranchMispredictions,
    ProfileStoreInstructions,
    ProfileFpInstructions,
    ProfileIntegerInstructions,
    Profile2Issue,
    Profile3Issue,
    Profile4Issue,
    ProfileSpecialInstructions,
    ProfileTotalCycles,
    ProfileIcacheIssues,
    ProfileDcacheAccesses,
    ProfileMemoryBarrierCycles,
    ProfileLoadLinkedIssues,
    ProfileMaximum);
  KPROFILE_SOURCE = _KPROFILE_SOURCE;
  TKProfileSource = KPROFILE_SOURCE;

  PIO_APC_ROUTINE = procedure(ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; Reserved: ULONG); stdcall;

  _FILE_FULL_EA_INFORMATION = record
    NextEntryOffset: ULONG;
    Flags: UCHAR;
    EaNameLength: UCHAR;
    EaValueLength: USHORT;
    EaName: array[0..0] of AnsiChar;
  end;
  FILE_FULL_EA_INFORMATION = _FILE_FULL_EA_INFORMATION;
  PFILE_FULL_EA_INFORMATION = ^FILE_FULL_EA_INFORMATION;
  TFileFullEaInformation = FILE_FULL_EA_INFORMATION;
  PFileFullEaInformation = ^TFileFullEaInformation;

  _FSINFOCLASS = (
    FileFsFiller0,
    FileFsVolumeInformation, // 1
    FileFsLabelInformation, // 2
    FileFsSizeInformation, // 3
    FileFsDeviceInformation, // 4
    FileFsAttributeInformation, // 5
    FileFsControlInformation, // 6
    FileFsFullSizeInformation, // 7
    FileFsObjectIdInformation, // 8
    FileFsMaximumInformation);
  FS_INFORMATION_CLASS = _FSINFOCLASS;
  PFS_INFORMATION_CLASS = ^FS_INFORMATION_CLASS;
  TFsInformationClass = FS_INFORMATION_CLASS;
  PFsInformationClass = ^TFsInformationClass;


{$IFNDEF JWA_INCLUDEMODE} //defined in jwaWindows.pas
  UUID = GUID;
{$ENDIF JWA_INCLUDEMODE}


  _FILE_BASIC_INFORMATION = record
    CreationTime: LARGE_INTEGER;
    LastAccessTime: LARGE_INTEGER;
    LastWriteTime: LARGE_INTEGER;
    ChangeTime: LARGE_INTEGER;
    FileAttributes: ULONG;
  end;
  FILE_BASIC_INFORMATION = _FILE_BASIC_INFORMATION;
  PFILE_BASIC_INFORMATION = ^FILE_BASIC_INFORMATION;
  TFileBasicInformation = FILE_BASIC_INFORMATION;
  PFileBasicInformation = ^TFileBasicInformation;

  _FILE_NETWORK_OPEN_INFORMATION = record
    CreationTime: LARGE_INTEGER;
    LastAccessTime: LARGE_INTEGER;
    LastWriteTime: LARGE_INTEGER;
    ChangeTime: LARGE_INTEGER;
    AllocationSize: LARGE_INTEGER;
    EndOfFile: LARGE_INTEGER;
    FileAttributes: ULONG;
  end;
  FILE_NETWORK_OPEN_INFORMATION = _FILE_NETWORK_OPEN_INFORMATION;
  PFILE_NETWORK_OPEN_INFORMATION = ^FILE_NETWORK_OPEN_INFORMATION;
  TFileNetworkOpenInformation = FILE_NETWORK_OPEN_INFORMATION;
  PFileNetworkOpenInformation = ^TFileNetworkOpenInformation;

  
  _FILE_INFORMATION_CLASS = (
    FileFiller0,
    FileDirectoryInformation, // 1
    FileFullDirectoryInformation, // 2
    FileBothDirectoryInformation, // 3
    FileBasicInformation, // 4  wdm
    FileStandardInformation, // 5  wdm
    FileInternalInformation, // 6
    FileEaInformation, // 7
    FileAccessInformation, // 8
    FileNameInformation, // 9
    FileRenameInformation, // 10
    FileLinkInformation, // 11
    FileNamesInformation, // 12
    FileDispositionInformation, // 13
    FilePositionInformation, // 14 wdm
    FileFullEaInformation, // 15
    FileModeInformation, // 16
    FileAlignmentInformation, // 17
    FileAllInformation, // 18
    FileAllocationInformation, // 19
    FileEndOfFileInformation, // 20 wdm
    FileAlternateNameInformation, // 21
    FileStreamInformation, // 22
    FilePipeInformation, // 23
    FilePipeLocalInformation, // 24
    FilePipeRemoteInformation, // 25
    FileMailslotQueryInformation, // 26
    FileMailslotSetInformation, // 27
    FileCompressionInformation, // 28
    FileObjectIdInformation, // 29
    FileCompletionInformation, // 30
    FileMoveClusterInformation, // 31
    FileQuotaInformation, // 32
    FileReparsePointInformation, // 33
    FileNetworkOpenInformation, // 34
    FileAttributeTagInformation, // 35
    FileTrackingInformation, // 36
    FileMaximumInformation);
  FILE_INFORMATION_CLASS = _FILE_INFORMATION_CLASS;
  PFILE_INFORMATION_CLASS = ^FILE_INFORMATION_CLASS;
  TFileInformationClass = FILE_INFORMATION_CLASS;
  PFileInformationClass = ^TFileInformationClass;

  _FILE_STANDARD_INFORMATION = record
    AllocationSize: LARGE_INTEGER;
    EndOfFile: LARGE_INTEGER;
    NumberOfLinks: ULONG;
    DeletePending: ByteBool;
    Directory: ByteBool;
  end;
  FILE_STANDARD_INFORMATION = _FILE_STANDARD_INFORMATION;
  PFILE_STANDARD_INFORMATION = ^FILE_STANDARD_INFORMATION;
  TFileStandardInformation = FILE_STANDARD_INFORMATION;
  PFileStandardInformation = ^TFileStandardInformation;

  _FILE_POSITION_INFORMATION = record
    CurrentByteOffset: LARGE_INTEGER;
  end;
  FILE_POSITION_INFORMATION = _FILE_POSITION_INFORMATION;
  PFILE_POSITION_INFORMATION = ^FILE_POSITION_INFORMATION;
  TFilePositionInformation = FILE_POSITION_INFORMATION;
  PFilePositionInformation = ^TFilePositionInformation;

  _FILE_ALIGNMENT_INFORMATION = record
    AlignmentRequirement: ULONG;
  end;
  FILE_ALIGNMENT_INFORMATION = _FILE_ALIGNMENT_INFORMATION;
  PFILE_ALIGNMENT_INFORMATION = ^FILE_ALIGNMENT_INFORMATION;
  TFileAlignmentInformation = FILE_ALIGNMENT_INFORMATION;
  PFileAlignmentInformation = ^TFileAlignmentInformation;

  _KEY_SET_INFORMATION_CLASS = (KeyWriteTimeInformation);
  KEY_SET_INFORMATION_CLASS = _KEY_SET_INFORMATION_CLASS;

  _KEY_INFORMATION_CLASS = (
    KeyBasicInformation,
    KeyNodeInformation,
    KeyFullInformation,
    KeyNameInformation);
  KEY_INFORMATION_CLASS = _KEY_INFORMATION_CLASS;
  TKeyInformationClass = KEY_INFORMATION_CLASS;

  _KEY_VALUE_INFORMATION_CLASS = (
    KeyValueBasicInformation,
    KeyValueFullInformation,
    KeyValuePartialInformation,
    KeyValueFullInformationAlign64,
    KeyValuePartialInformationAlign64);
  KEY_VALUE_INFORMATION_CLASS = _KEY_VALUE_INFORMATION_CLASS;
  TKeyValueInformationClass = KEY_VALUE_INFORMATION_CLASS;

  _KEY_VALUE_ENTRY = record
    ValueName: PUNICODE_STRING;
    DataLength: ULONG;
    DataOffset: ULONG;
    Type_: ULONG;
  end;
  KEY_VALUE_ENTRY = _KEY_VALUE_ENTRY;
  PKEY_VALUE_ENTRY = ^KEY_VALUE_ENTRY;
  TKeyValueEntry = KEY_VALUE_ENTRY;
  PKeyValueEntry = ^TKeyValueEntry;

  _KEY_FULL_INFORMATION = record
    LastWriteTime: LARGE_INTEGER;
    TitleIndex: ULONG;
    ClassOffset: ULONG;
    ClassLength: ULONG;
    SubKeys: ULONG;
    MaxNameLen: ULONG;
    MaxClassLen: ULONG;
    Values: ULONG;
    MaxValueNameLen: ULONG;
    MaxValueDataLen: ULONG;
    Class_: Array[0..0] of WCHAR;
  end;
  PKEY_FULL_INFORMATION = ^_KEY_FULL_INFORMATION;
  PKeyFullInformation = PKEY_FULL_INFORMATION;
  TKeyFullInformation = _KEY_FULL_INFORMATION;

  _KEY_VALUE_FULL_INFORMATION = record
    TitleIndex: ULONG;
    Type_: ULONG;
    DataOffset: ULONG;
    DataLength: ULONG;
    NameLength: ULONG;
    Name: Array[0..0] of WCHAR;
  end;
  PKEY_VALUE_FULL_INFORMATION = ^_KEY_VALUE_FULL_INFORMATION;
  TKeyValueFullInformation = _KEY_VALUE_FULL_INFORMATION;
  PKeyValueFullInformation = PKEY_VALUE_FULL_INFORMATION;


  {$IFNDEF JWA_INCLUDEMODE}
  _DEVICE_POWER_STATE = (
    PowerDeviceUnspecified,
    PowerDeviceD0,
    PowerDeviceD1,
    PowerDeviceD2,
    PowerDeviceD3,
    PowerDeviceMaximum);

  DEVICE_POWER_STATE = _DEVICE_POWER_STATE;
  PDEVICE_POWER_STATE = ^DEVICE_POWER_STATE;
  TDevicePowerState = DEVICE_POWER_STATE;

  POWER_ACTION = (
    PowerActionNone,
    PowerActionReserved,
    PowerActionSleep,
    PowerActionHibernate,
    PowerActionShutdown,
    PowerActionShutdownReset,
    PowerActionShutdownOff,
    PowerActionWarmEject);
  PPOWER_ACTION = ^POWER_ACTION;
  TPowerAction = POWER_ACTION;

  _SYSTEM_POWER_STATE = (
    PowerSystemUnspecified,
    PowerSystemWorking,
    PowerSystemSleeping1,
    PowerSystemSleeping2,
    PowerSystemSleeping3,
    PowerSystemHibernate,
    PowerSystemShutdown,
    PowerSystemMaximum);
  SYSTEM_POWER_STATE = _SYSTEM_POWER_STATE;
  PSYSTEM_POWER_STATE = ^SYSTEM_POWER_STATE;
  TSystemPowerState = SYSTEM_POWER_STATE;

  POWER_INFORMATION_LEVEL = (
    SystemPowerPolicyAc,
    SystemPowerPolicyDc,
    VerifySystemPolicyAc,
    VerifySystemPolicyDc,
    SystemPowerCapabilities,
    SystemBatteryState,
    SystemPowerStateHandler,
    ProcessorStateHandler,
    SystemPowerPolicyCurrent,
    AdministratorPowerPolicy,
    SystemReserveHiberFile,
    ProcessorInformation,
    SystemPowerInformation);
  TPowerInformationLevel = POWER_INFORMATION_LEVEL;
  {$ENDIF JWA_INCLUDEMODE}

  _RTL_RANGE = record
    // The start of the range
    Start: ULONGLONG; // Read only
    // The end of the range
    End_: ULONGLONG; // Read only
    // Data the user passed in when they created the range
    UserData: PVOID; // Read/Write
    // The owner of the range
    Owner: PVOID; // Read/Write
    // User defined flags the user specified when they created the range
    Attributes: UCHAR; // Read/Write
    // Flags (RTL_RANGE_*)
    Flags: UCHAR; // Read only
  end;
  RTL_RANGE = _RTL_RANGE;
  PRTL_RANGE = ^RTL_RANGE;
  TRtlRange = RTL_RANGE;
  PRtlRange = ^TRtlRange;

const
  RTL_RANGE_SHARED = $01;
  RTL_RANGE_CONFLICT = $02;

type
  _RTL_RANGE_LIST = record
    // The list of ranges
    ListHead: LIST_ENTRY;
    // These always come in useful
    Flags: ULONG; // use RANGE_LIST_FLAG_*
    // The number of entries in the list
    Count: ULONG;
    // Every time an add/delete operation is performed on the list this is
    // incremented.  It is checked during iteration to ensure that the list
    // hasn't changed between GetFirst/GetNext or GetNext/GetNext calls
    Stamp: ULONG;
  end;
  RTL_RANGE_LIST = _RTL_RANGE_LIST;
  PRTL_RANGE_LIST = ^RTL_RANGE_LIST;
  TRtlRangeList = RTL_RANGE_LIST;
  PRtlRangeList = ^TRtlRangeList;

  _RANGE_LIST_ITERATOR = record
    RangeListHead: PLIST_ENTRY;
    MergedHead: PLIST_ENTRY;
    Current: PVOID;
    Stamp: ULONG;
  end;
  RTL_RANGE_LIST_ITERATOR = _RANGE_LIST_ITERATOR;
  PRTL_RANGE_LIST_ITERATOR = ^RTL_RANGE_LIST_ITERATOR;
  TRtlRangeListIterator = RTL_RANGE_LIST_ITERATOR;
  PRtlRangeListIterator = ^TRtlRangeListIterator;

// End of NTDDK.H

//==============================================================================
// NT System Services
//==============================================================================

type
  _SYSTEM_INFORMATION_CLASS = (
    SystemBasicInformation,
    SystemProcessorInformation,
    SystemPerformanceInformation,
    SystemTimeOfDayInformation,
    SystemNotImplemented1,
    SystemProcessesAndThreadsInformation,
    SystemCallCounts,
    SystemConfigurationInformation,
    SystemProcessorTimes,
    SystemGlobalFlag,
    SystemNotImplemented2,
    SystemModuleInformation,
    SystemLockInformation,
    SystemNotImplemented3,
    SystemNotImplemented4,
    SystemNotImplemented5,
    SystemHandleInformation,
    SystemObjectInformation,
    SystemPagefileInformation,
    SystemInstructionEmulationCounts,
    SystemInvalidInfoClass1,
    SystemCacheInformation,
    SystemPoolTagInformation,
    SystemProcessorStatistics,
    SystemDpcInformation,
    SystemNotImplemented6,
    SystemLoadImage,
    SystemUnloadImage,
    SystemTimeAdjustment,
    SystemNotImplemented7,
    SystemNotImplemented8,
    SystemNotImplemented9,
    SystemCrashDumpInformation,
    SystemExceptionInformation,
    SystemCrashDumpStateInformation,
    SystemKernelDebuggerInformation,
    SystemContextSwitchInformation,
    SystemRegistryQuotaInformation,
    SystemLoadAndCallImage,
    SystemPrioritySeparation,
    SystemNotImplemented10,
    SystemNotImplemented11,
    SystemInvalidInfoClass2,
    SystemInvalidInfoClass3,
    SystemTimeZoneInformation,
    SystemLookasideInformation,
    SystemSetTimeSlipEvent,
    SystemCreateSession,
    SystemDeleteSession,
    SystemInvalidInfoClass4,
    SystemRangeStartInformation,
    SystemVerifierInformation,
    SystemAddVerifier,
    SystemSessionProcessesInformation);
  SYSTEM_INFORMATION_CLASS = _SYSTEM_INFORMATION_CLASS;
  TSystemInformationClass = SYSTEM_INFORMATION_CLASS;

type
  {.$IFNDEF JWA_INCLUDEMODE}
  _SYSTEM_BASIC_INFORMATION = record // Information Class 0
    Unknown: ULONG;
    MaximumIncrement: ULONG;
    PhysicalPageSize: ULONG;
    NumberOfPhysicalPages: ULONG;
    LowestPhysicalPage: ULONG;
    HighestPhysicalPage: ULONG;
    AllocationGranularity: ULONG;
    LowestUserAddress: ULONG;
    HighestUserAddress: ULONG;
    ActiveProcessors: ULONG;
    NumberProcessors: UCHAR;
  end;
  SYSTEM_BASIC_INFORMATION = _SYSTEM_BASIC_INFORMATION;
  PSYSTEM_BASIC_INFORMATION = ^SYSTEM_BASIC_INFORMATION;
  TSystemBasicInformation = SYSTEM_BASIC_INFORMATION;
  PSystemBasicInformation = ^TSystemBasicInformation;
  {.$ENDIF JWA_INCLUDEMODE}

  _SYSTEM_PROCESSOR_INFORMATION = record // Information Class 1
    ProcessorArchitecture: USHORT;
    ProcessorLevel: USHORT;
    ProcessorRevision: USHORT;
    Unknown: USHORT;
    FeatureBits: ULONG;
  end;
  SYSTEM_PROCESSOR_INFORMATION = _SYSTEM_PROCESSOR_INFORMATION;
  PSYSTEM_PROCESSOR_INFORMATION = ^SYSTEM_PROCESSOR_INFORMATION;

  {.$IFNDEF JWA_INCLUDEMODE}
  TSystemProcessorInformation = SYSTEM_PROCESSOR_INFORMATION;
  PSystemProcessorInformation = ^TSystemProcessorInformation;

  _SYSTEM_PERFORMANCE_INFORMATION = record // Information Class 2
    IdleTime: LARGE_INTEGER;
    ReadTransferCount: LARGE_INTEGER;
    WriteTransferCount: LARGE_INTEGER;
    OtherTransferCount: LARGE_INTEGER;
    ReadOperationCount: ULONG;
    WriteOperationCount: ULONG;
    OtherOperationCount: ULONG;
    AvailablePages: ULONG;
    TotalCommittedPages: ULONG;
    TotalCommitLimit: ULONG;
    PeakCommitment: ULONG;
    PageFaults: ULONG;
    WriteCopyFaults: ULONG;
    TransistionFaults: ULONG;
    Reserved1: ULONG;
    DemandZeroFaults: ULONG;
    PagesRead: ULONG;
    PageReadIos: ULONG;
    Reserved2: array[0..1] of ULONG;
    PagefilePagesWritten: ULONG;
    PagefilePageWriteIos: ULONG;
    MappedFilePagesWritten: ULONG;
    MappedFilePageWriteIos: ULONG;
    PagedPoolUsage: ULONG;
    NonPagedPoolUsage: ULONG;
    PagedPoolAllocs: ULONG;
    PagedPoolFrees: ULONG;
    NonPagedPoolAllocs: ULONG;
    NonPagedPoolFrees: ULONG;
    TotalFreeSystemPtes: ULONG;
    SystemCodePage: ULONG;
    TotalSystemDriverPages: ULONG;
    TotalSystemCodePages: ULONG;
    SmallNonPagedLookasideListAllocateHits: ULONG;
    SmallPagedLookasideListAllocateHits: ULONG;
    Reserved3: ULONG;
    MmSystemCachePage: ULONG;
    PagedPoolPage: ULONG;
    SystemDriverPage: ULONG;
    FastReadNoWait: ULONG;
    FastReadWait: ULONG;
    FastReadResourceMiss: ULONG;
    FastReadNotPossible: ULONG;
    FastMdlReadNoWait: ULONG;
    FastMdlReadWait: ULONG;
    FastMdlReadResourceMiss: ULONG;
    FastMdlReadNotPossible: ULONG;
    MapDataNoWait: ULONG;
    MapDataWait: ULONG;
    MapDataNoWaitMiss: ULONG;
    MapDataWaitMiss: ULONG;
    PinMappedDataCount: ULONG;
    PinReadNoWait: ULONG;
    PinReadWait: ULONG;
    PinReadNoWaitMiss: ULONG;
    PinReadWaitMiss: ULONG;
    CopyReadNoWait: ULONG;
    CopyReadWait: ULONG;
    CopyReadNoWaitMiss: ULONG;
    CopyReadWaitMiss: ULONG;
    MdlReadNoWait: ULONG;
    MdlReadWait: ULONG;
    MdlReadNoWaitMiss: ULONG;
    MdlReadWaitMiss: ULONG;
    ReadAheadIos: ULONG;
    LazyWriteIos: ULONG;
    LazyWritePages: ULONG;
    DataFlushes: ULONG;
    DataPages: ULONG;
    ContextSwitches: ULONG;
    FirstLevelTbFills: ULONG;
    SecondLevelTbFills: ULONG;
    SystemCalls: ULONG;
  end;
  SYSTEM_PERFORMANCE_INFORMATION = _SYSTEM_PERFORMANCE_INFORMATION;
  PSYSTEM_PERFORMANCE_INFORMATION = ^SYSTEM_PERFORMANCE_INFORMATION;
  TSystemPerformanceInformation = SYSTEM_PERFORMANCE_INFORMATION;
  PSystemPerformanceInformation = ^TSystemPerformanceInformation;
  {.$ENDIF JWA_INCLUDEMODE}

  _SYSTEM_TIME_OF_DAY_INFORMATION = record // Information Class 3
    BootTime: LARGE_INTEGER;
    CurrentTime: LARGE_INTEGER;
    TimeZoneBias: LARGE_INTEGER;
    CurrentTimeZoneId: ULONG;
  end;
  SYSTEM_TIME_OF_DAY_INFORMATION = _SYSTEM_TIME_OF_DAY_INFORMATION;
  PSYSTEM_TIME_OF_DAY_INFORMATION = ^SYSTEM_TIME_OF_DAY_INFORMATION;

  {.$IFNDEF JWA_INCLUDEMODE}
  TSystemTimeOfDayInformation = SYSTEM_TIME_OF_DAY_INFORMATION;
  PSystemTimeOfDayInformation = ^TSystemTimeOfDayInformation;
  {.$ENDIF JWA_INCLUDEMODE}

  _IO_COUNTERSEX = record
    ReadOperationCount: LARGE_INTEGER;
    WriteOperationCount: LARGE_INTEGER;
    OtherOperationCount: LARGE_INTEGER;
    ReadTransferCount: LARGE_INTEGER;
    WriteTransferCount: LARGE_INTEGER;
    OtherTransferCount: LARGE_INTEGER;
  end;
  IO_COUNTERSEX = _IO_COUNTERSEX;
  PIO_COUNTERSEX = ^IO_COUNTERSEX;
  TIoCountersEx = IO_COUNTERSEX;
  PIoCountersEx = ^TIoCountersEx;

  THREAD_STATE = (
    StateInitialized,
    StateReady,
    StateRunning,
    StateStandby,
    StateTerminated,
    StateWait,
    StateTransition,
    StateUnknown);
  TThreadState = THREAD_STATE;

  _SYSTEM_THREADS = record
    KernelTime: LARGE_INTEGER;
    UserTime: LARGE_INTEGER;
    CreateTime: LARGE_INTEGER;
    WaitTime: ULONG;
    StartAddress: PVOID;
    ClientId: CLIENT_ID;
    Priority: KPRIORITY;
    BasePriority: KPRIORITY;
    ContextSwitchCount: ULONG;
    State: THREAD_STATE;
    WaitReason: KWAIT_REASON;
  end;
  SYSTEM_THREADS = _SYSTEM_THREADS;
  PSYSTEM_THREADS = ^SYSTEM_THREADS;
  TSystemThreads = SYSTEM_THREADS;
  PSystemThreads = PSYSTEM_THREADS;

  _SYSTEM_PROCESSES = record // Information Class 5
    NextEntryDelta: ULONG;
    ThreadCount: ULONG;
    Reserved1: array[0..5] of ULONG;
    CreateTime: LARGE_INTEGER;
    UserTime: LARGE_INTEGER;
    KernelTime: LARGE_INTEGER;
    ProcessName: UNICODE_STRING;
    BasePriority: KPRIORITY;
    ProcessId: ULONG;
    InheritedFromProcessId: ULONG;
    HandleCount: ULONG;
    // next two were Reserved2: array [0..1] of ULONG; thanks to Nico Bendlin
    SessionId: ULONG;
    Reserved2: ULONG;
    VmCounters: VM_COUNTERS;
    PrivatePageCount: ULONG;
    IoCounters: IO_COUNTERSEX; // Windows 2000 only
    Threads: array[0..0] of SYSTEM_THREADS;
  end;
  SYSTEM_PROCESSES = _SYSTEM_PROCESSES;
  PSYSTEM_PROCESSES = ^SYSTEM_PROCESSES;
  TSystemProcesses = SYSTEM_PROCESSES;
  PSystemProcesses = PSYSTEM_PROCESSES;

  _SYSTEM_CALLS_INFORMATION = record // Information Class 6
    Size: ULONG;
    NumberOfDescriptorTables: ULONG;
    NumberOfRoutinesInTable: array[0..0] of ULONG;
    // ULONG CallCounts[];
  end;
  SYSTEM_CALLS_INFORMATION = _SYSTEM_CALLS_INFORMATION;
  PSYSTEM_CALLS_INFORMATION = ^SYSTEM_CALLS_INFORMATION;
  TSystemCallsInformation = SYSTEM_CALLS_INFORMATION;
  PSystemCallsInformation = ^TSystemCallsInformation;

  _SYSTEM_CONFIGURATION_INFORMATION = record // Information Class 7
    DiskCount: ULONG;
    FloppyCount: ULONG;
    CdRomCount: ULONG;
    TapeCount: ULONG;
    SerialCount: ULONG;
    ParallelCount: ULONG;
  end;
  SYSTEM_CONFIGURATION_INFORMATION = _SYSTEM_CONFIGURATION_INFORMATION;
  PSYSTEM_CONFIGURATION_INFORMATION = ^SYSTEM_CONFIGURATION_INFORMATION;
  TSystemConfigurationInformation = SYSTEM_CONFIGURATION_INFORMATION;
  PSystemConfigurationInformation = ^TSystemConfigurationInformation;

  _SYSTEM_PROCESSOR_TIMES = record // Information Class 8
    IdleTime: LARGE_INTEGER;
    KernelTime: LARGE_INTEGER;
    UserTime: LARGE_INTEGER;
    DpcTime: LARGE_INTEGER;
    InterruptTime: LARGE_INTEGER;
    InterruptCount: ULONG;
  end;
  SYSTEM_PROCESSOR_TIMES = _SYSTEM_PROCESSOR_TIMES;
  PSYSTEM_PROCESSOR_TIMES = ^SYSTEM_PROCESSOR_TIMES;
  TSystemProcessorTimes = SYSTEM_PROCESSOR_TIMES;
  PSystemProcessorTimes = ^TSystemProcessorTimes;

  _SYSTEM_GLOBAL_FLAG = record // Information Class 9
    GlobalFlag: ULONG;
  end;
  SYSTEM_GLOBAL_FLAG = _SYSTEM_GLOBAL_FLAG;
  PSYSTEM_GLOBAL_FLAG = ^SYSTEM_GLOBAL_FLAG;
  TSystemGlobalFlag = SYSTEM_GLOBAL_FLAG;
  PSystemGlobalFlag = ^TSystemGlobalFlag;

  _SYSTEM_MODULE_INFORMATION = record // Information Class 11
    Reserved: array[0..1] of ULONG;
    Base: PVOID;
    Size: ULONG;
    Flags: ULONG;
    Index: USHORT;
    Unknown: USHORT;
    LoadCount: USHORT;
    ModuleNameOffset: USHORT;
    ImageName: array[0..255] of AnsiChar;
  end;
  SYSTEM_MODULE_INFORMATION = _SYSTEM_MODULE_INFORMATION;
  PSYSTEM_MODULE_INFORMATION = ^SYSTEM_MODULE_INFORMATION;
  TSystemModuleInformation = SYSTEM_MODULE_INFORMATION;
  PSystemModuleInformation = PSYSTEM_MODULE_INFORMATION;

  _SYSTEM_LOCK_INFORMATION = record // Information Class 12
    Address: PVOID;
    Type_: USHORT;
    Reserved1: USHORT;
    ExclusiveOwnerThreadId: ULONG;
    ActiveCount: ULONG;
    ContentionCount: ULONG;
    Reserved2: array[0..1] of ULONG;
    NumberOfSharedWaiters: ULONG;
    NumberOfExclusiveWaiters: ULONG;
  end;
  SYSTEM_LOCK_INFORMATION = _SYSTEM_LOCK_INFORMATION;
  PSYSTEM_LOCK_INFORMATION = ^SYSTEM_LOCK_INFORMATION;
  TSystemLockInformation = SYSTEM_LOCK_INFORMATION;
  PSystemLockInformation = ^TSystemLockInformation;

  _SYSTEM_HANDLE_INFORMATION = record // Information Class 16
    ProcessId: ULONG;
    ObjectTypeNumber: UCHAR;
    Flags: UCHAR; // 0x01 = PROTECT_FROM_CLOSE, 0x02 = INHERIT
    Handle: USHORT;
    Object_: PVOID;
    GrantedAccess: ACCESS_MASK;
  end;
  SYSTEM_HANDLE_INFORMATION = _SYSTEM_HANDLE_INFORMATION;
  PSYSTEM_HANDLE_INFORMATION = ^SYSTEM_HANDLE_INFORMATION;
  TSystemHandleInformation = SYSTEM_HANDLE_INFORMATION;
  PSystemHandleInformation = ^TSystemHandleInformation;

  _SYSTEM_OBJECT_TYPE_INFORMATION = record // Information Class 17
    NextEntryOffset: ULONG;
    ObjectCount: ULONG;
    HandleCount: ULONG;
    TypeNumber: ULONG;
    InvalidAttributes: ULONG;
    GenericMapping: GENERIC_MAPPING;
    ValidAccessMask: ACCESS_MASK;
    PoolType: POOL_TYPE;
    Unknown: UCHAR;
    Name: UNICODE_STRING;
  end;
  SYSTEM_OBJECT_TYPE_INFORMATION = _SYSTEM_OBJECT_TYPE_INFORMATION;
  PSYSTEM_OBJECT_TYPE_INFORMATION = ^SYSTEM_OBJECT_TYPE_INFORMATION;
  TSystemObjectTypeInformation = SYSTEM_OBJECT_TYPE_INFORMATION;
  PSystemObjectTypeInformation = ^TSystemObjectTypeInformation;

  _SYSTEM_OBJECT_INFORMATION = record
    NextEntryOffset: ULONG;
    Object_: PVOID;
    CreatorProcessId: ULONG;
    Unknown: USHORT;
    Flags: USHORT;
    PointerCount: ULONG;
    HandleCount: ULONG;
    PagedPoolUsage: ULONG;
    NonPagedPoolUsage: ULONG;
    ExclusiveProcessId: ULONG;
    SecurityDescriptor: PSECURITY_DESCRIPTOR;
    Name: UNICODE_STRING;
  end;
  SYSTEM_OBJECT_INFORMATION = _SYSTEM_OBJECT_INFORMATION;
  PSYSTEM_OBJECT_INFORMATION = ^SYSTEM_OBJECT_INFORMATION;
  TSystemObjectInformation = SYSTEM_OBJECT_INFORMATION;
  PSystemObjectInformation = ^TSystemObjectInformation;

  _SYSTEM_PAGEFILE_INFORMATION = record // Information Class 18
    NextEntryOffset: ULONG;
    CurrentSize: ULONG;
    TotalUsed: ULONG;
    PeakUsed: ULONG;
    FileName: UNICODE_STRING;
  end;
  SYSTEM_PAGEFILE_INFORMATION = _SYSTEM_PAGEFILE_INFORMATION;
  PSYSTEM_PAGEFILE_INFORMATION = ^SYSTEM_PAGEFILE_INFORMATION;
  TSystemPageFileInformation = SYSTEM_PAGEFILE_INFORMATION;
  PSystemPageFileInformation = PSYSTEM_PAGEFILE_INFORMATION;

  _SYSTEM_INSTRUCTION_EMULATION_INFORMATION = record // Info Class 19
    GenericInvalidOpcode: ULONG;
    TwoByteOpcode: ULONG;
    ESprefix: ULONG;
    CSprefix: ULONG;
    SSprefix: ULONG;
    DSprefix: ULONG;
    FSPrefix: ULONG;
    GSprefix: ULONG;
    OPER32prefix: ULONG;
    ADDR32prefix: ULONG;
    INSB: ULONG;
    INSW: ULONG;
    OUTSB: ULONG;
    OUTSW: ULONG;
    PUSHFD: ULONG;
    POPFD: ULONG;
    INTnn: ULONG;
    INTO: ULONG;
    IRETD: ULONG;
    FloatingPointOpcode: ULONG;
    INBimm: ULONG;
    INWimm: ULONG;
    OUTBimm: ULONG;
    OUTWimm: ULONG;
    INB: ULONG;
    INW: ULONG;
    OUTB: ULONG;
    OUTW: ULONG;
    LOCKprefix: ULONG;
    REPNEprefix: ULONG;
    REPprefix: ULONG;
    CLI: ULONG;
    STI: ULONG;
    HLT: ULONG;
  end;
  SYSTEM_INSTRUCTION_EMULATION_INFORMATION = _SYSTEM_INSTRUCTION_EMULATION_INFORMATION;
  PSYSTEM_INSTRUCTION_EMULATION_INFORMATION = ^SYSTEM_INSTRUCTION_EMULATION_INFORMATION;
  TSystemInstructionEmulationInformation = SYSTEM_INSTRUCTION_EMULATION_INFORMATION;
  PSystemInstructionEmulationInformation = ^TSystemInstructionEmulationInformation;

  _SYSTEM_CACHE_INFORMATION = record // Information Class 21
    SystemCacheWsSize: ULONG;
    SystemCacheWsPeakSize: ULONG;
    SystemCacheWsFaults: ULONG;
    SystemCacheWsMinimum: ULONG;
    SystemCacheWsMaximum: ULONG;
    TransitionSharedPages: ULONG;
    TransitionSharedPagesPeak: ULONG;
    Reserved: array[0..1] of ULONG;
  end;
  SYSTEM_CACHE_INFORMATION = _SYSTEM_CACHE_INFORMATION;
  PSYSTEM_CACHE_INFORMATION = ^SYSTEM_CACHE_INFORMATION;
  TSystemCacheInformation = SYSTEM_CACHE_INFORMATION;
  PSystemCacheInformation = ^TSystemCacheInformation;

  _SYSTEM_POOL_TAG_INFORMATION = record // Information Class 22
    Tag: array[0..3] of AnsiChar;
    PagedPoolAllocs: ULONG;
    PagedPoolFrees: ULONG;
    PagedPoolUsage: ULONG;
    NonPagedPoolAllocs: ULONG;
    NonPagedPoolFrees: ULONG;
    NonPagedPoolUsage: ULONG;
  end;
  SYSTEM_POOL_TAG_INFORMATION = _SYSTEM_POOL_TAG_INFORMATION;
  PSYSTEM_POOL_TAG_INFORMATION = ^SYSTEM_POOL_TAG_INFORMATION;
  TSystemPoolTagInformation = SYSTEM_POOL_TAG_INFORMATION;
  PSystemPoolTagInformation = ^TSystemPoolTagInformation;

  _SYSTEM_PROCESSOR_STATISTICS = record // Information Class 23
    ContextSwitches: ULONG;
    DpcCount: ULONG;
    DpcRequestRate: ULONG;
    TimeIncrement: ULONG;
    DpcBypassCount: ULONG;
    ApcBypassCount: ULONG;
  end;
  SYSTEM_PROCESSOR_STATISTICS = _SYSTEM_PROCESSOR_STATISTICS;
  PSYSTEM_PROCESSOR_STATISTICS = ^SYSTEM_PROCESSOR_STATISTICS;
  TSystemProcessorStatistics = SYSTEM_PROCESSOR_STATISTICS;
  PSystemProcessorStatistics = ^TSystemProcessorStatistics;

  _SYSTEM_DPC_INFORMATION = record // Information Class 24
    Reserved: ULONG;
    MaximumDpcQueueDepth: ULONG;
    MinimumDpcRate: ULONG;
    AdjustDpcThreshold: ULONG;
    IdealDpcRate: ULONG;
  end;
  SYSTEM_DPC_INFORMATION = _SYSTEM_DPC_INFORMATION;
  PSYSTEM_DPC_INFORMATION = ^SYSTEM_DPC_INFORMATION;
  TSystemDpcInformation = SYSTEM_DPC_INFORMATION;
  PSystemDpcInformation = ^TSystemDpcInformation;

  _SYSTEM_LOAD_IMAGE = record // Information Class 26
    ModuleName: UNICODE_STRING;
    ModuleBase: PVOID;
    Unknown: PVOID;
    EntryPoint: PVOID;
    ExportDirectory: PVOID;
  end;
  SYSTEM_LOAD_IMAGE = _SYSTEM_LOAD_IMAGE;
  PSYSTEM_LOAD_IMAGE = ^SYSTEM_LOAD_IMAGE;
  TSystemLoadImage = SYSTEM_LOAD_IMAGE;
  PSystemLoadImage = ^TSystemLoadImage;

  _SYSTEM_UNLOAD_IMAGE = record // Information Class 27
    ModuleBase: PVOID;
  end;
  SYSTEM_UNLOAD_IMAGE = _SYSTEM_UNLOAD_IMAGE;
  PSYSTEM_UNLOAD_IMAGE = ^SYSTEM_UNLOAD_IMAGE;
  TSystemUnloadImage = SYSTEM_UNLOAD_IMAGE;
  PSystemUnloadImage = ^TSystemUnloadImage;

  _SYSTEM_QUERY_TIME_ADJUSTMENT = record // Information Class 28
    TimeAdjustment: ULONG;
    MaximumIncrement: ULONG;
    TimeSynchronization: ByteBool;
  end;
  SYSTEM_QUERY_TIME_ADJUSTMENT = _SYSTEM_QUERY_TIME_ADJUSTMENT;
  PSYSTEM_QUERY_TIME_ADJUSTMENT = ^SYSTEM_QUERY_TIME_ADJUSTMENT;
  TSystemQueryTimeAdjustment = SYSTEM_QUERY_TIME_ADJUSTMENT;
  PSystemQueryTimeAdjustment = ^TSystemQueryTimeAdjustment;

  _SYSTEM_SET_TIME_ADJUSTMENT = record // Information Class 28
    TimeAdjustment: ULONG;
    TimeSynchronization: ByteBool;
  end;
  SYSTEM_SET_TIME_ADJUSTMENT = _SYSTEM_SET_TIME_ADJUSTMENT;
  PSYSTEM_SET_TIME_ADJUSTMENT = ^SYSTEM_SET_TIME_ADJUSTMENT;
  TSystemSetTimeAdjustment = SYSTEM_SET_TIME_ADJUSTMENT;
  PSystemSetTimeAdjustment = ^TSystemSetTimeAdjustment;

  _SYSTEM_CRASH_DUMP_INFORMATION = record // Information Class 32
    CrashDumpSectionHandle: HANDLE;
    Unknown: HANDLE; // Windows 2000 only
  end;
  SYSTEM_CRASH_DUMP_INFORMATION = _SYSTEM_CRASH_DUMP_INFORMATION;
  PSYSTEM_CRASH_DUMP_INFORMATION = ^SYSTEM_CRASH_DUMP_INFORMATION;
  TSystemCrashDumpInformation = SYSTEM_CRASH_DUMP_INFORMATION;
  PSystemCrashDumpInformation = ^TSystemCrashDumpInformation;

  {.$IFNDEF JWA_INCLUDEMODE}
  _SYSTEM_EXCEPTION_INFORMATION = record // Information Class 33
    AlignmentFixupCount: ULONG;
    ExceptionDispatchCount: ULONG;
    FloatingEmulationCount: ULONG;
    Reserved: ULONG;
  end;
  SYSTEM_EXCEPTION_INFORMATION = _SYSTEM_EXCEPTION_INFORMATION;
  PSYSTEM_EXCEPTION_INFORMATION = ^SYSTEM_EXCEPTION_INFORMATION;
  TSystemExceptionInformation = SYSTEM_EXCEPTION_INFORMATION;
  PSystemExceptionInformation = ^TSystemExceptionInformation;
  {.$ENDIF JWA_INCLUDEMODE}

  _SYSTEM_CRASH_STATE_INFORMATION = record // Information Class 34
    ValidCrashDump: ULONG;
    Unknown: ULONG; // Windows 2000 only
  end;
  SYSTEM_CRASH_STATE_INFORMATION = _SYSTEM_CRASH_STATE_INFORMATION;
  PSYSTEM_CRASH_STATE_INFORMATION = ^SYSTEM_CRASH_STATE_INFORMATION;
  TSystemCrashStateInformation = SYSTEM_CRASH_STATE_INFORMATION;
  PSystemCrashStateInformation = ^TSystemCrashStateInformation;

  _SYSTEM_KERNEL_DEBUGGER_INFORMATION = record // Information Class 35
    DebuggerEnabled: ByteBool;
    DebuggerNotPresent: ByteBool;
  end;
  SYSTEM_KERNEL_DEBUGGER_INFORMATION = _SYSTEM_KERNEL_DEBUGGER_INFORMATION;
  PSYSTEM_KERNEL_DEBUGGER_INFORMATION = ^SYSTEM_KERNEL_DEBUGGER_INFORMATION;
  TSystemKernelDebuggerInformation = SYSTEM_KERNEL_DEBUGGER_INFORMATION;
  PSystemKernelDebuggerInformation = ^TSystemKernelDebuggerInformation;

  _SYSTEM_CONTEXT_SWITCH_INFORMATION = record // Information Class 36
    ContextSwitches: ULONG;
    ContextSwitchCounters: array[0..10] of ULONG;
  end;
  SYSTEM_CONTEXT_SWITCH_INFORMATION = _SYSTEM_CONTEXT_SWITCH_INFORMATION;
  PSYSTEM_CONTEXT_SWITCH_INFORMATION = ^SYSTEM_CONTEXT_SWITCH_INFORMATION;
  TSystemContextSwitchInformation = SYSTEM_CONTEXT_SWITCH_INFORMATION;
  PSystemContextSwitchInformation = ^TSystemContextSwitchInformation;

  {.$IFNDEF JWA_INCLUDEMODE}
  _SYSTEM_REGISTRY_QUOTA_INFORMATION = record // Information Class 37
    RegistryQuota: ULONG;
    RegistryQuotaInUse: ULONG;
    PagedPoolSize: ULONG;
  end;
  SYSTEM_REGISTRY_QUOTA_INFORMATION = _SYSTEM_REGISTRY_QUOTA_INFORMATION;
  PSYSTEM_REGISTRY_QUOTA_INFORMATION = ^SYSTEM_REGISTRY_QUOTA_INFORMATION;
  TSystemRegistryQuotaInformation = SYSTEM_REGISTRY_QUOTA_INFORMATION;
  PSystemRegistryQuotaInformation = ^TSystemRegistryQuotaInformation;
  {.$ENDIF JWA_INCLUDEMODE}

  _SYSTEM_LOAD_AND_CALL_IMAGE = record // Information Class 38
    ModuleName: UNICODE_STRING;
  end;
  SYSTEM_LOAD_AND_CALL_IMAGE = _SYSTEM_LOAD_AND_CALL_IMAGE;
  PSYSTEM_LOAD_AND_CALL_IMAGE = ^SYSTEM_LOAD_AND_CALL_IMAGE;
  TSystemLoadAndCallImage = SYSTEM_LOAD_AND_CALL_IMAGE;
  PSystemLoadAndCallImage = ^TSystemLoadAndCallImage;

  _SYSTEM_PRIORITY_SEPARATION = record // Information Class 39
    PrioritySeparation: ULONG;
  end;
  SYSTEM_PRIORITY_SEPARATION = _SYSTEM_PRIORITY_SEPARATION;
  PSYSTEM_PRIORITY_SEPARATION = ^SYSTEM_PRIORITY_SEPARATION;
  TSystemPrioritySeparation = SYSTEM_PRIORITY_SEPARATION;
  PSystemPrioritySeparation = ^TSystemPrioritySeparation;

  _SYSTEM_TIME_ZONE_INFORMATION = record // Information Class 44
    Bias: LONG;
    StandardName: array[0..31] of WCHAR;
    StandardDate: SYSTEMTIME;
    StandardBias: LONG;
    DaylightName: array[0..31] of WCHAR;
    DaylightDate: SYSTEMTIME;
    DaylightBias: LONG;
  end;
  SYSTEM_TIME_ZONE_INFORMATION = _SYSTEM_TIME_ZONE_INFORMATION;
  PSYSTEM_TIME_ZONE_INFORMATION = ^SYSTEM_TIME_ZONE_INFORMATION;
  TSystemTimeZoneInformation = SYSTEM_TIME_ZONE_INFORMATION;
  PSystemTimeZoneInformation = ^TSystemTimeZoneInformation;

  {.$IFNDEF JWA_INCLUDEMODE}
  _SYSTEM_LOOKASIDE_INFORMATION = record // Information Class 45
    Depth: USHORT;
    MaximumDepth: USHORT;
    TotalAllocates: ULONG;
    AllocateMisses: ULONG;
    TotalFrees: ULONG;
    FreeMisses: ULONG;
    Type_: POOL_TYPE;
    Tag: ULONG;
    Size: ULONG;
  end;
  SYSTEM_LOOKASIDE_INFORMATION = _SYSTEM_LOOKASIDE_INFORMATION;
  PSYSTEM_LOOKASIDE_INFORMATION = ^SYSTEM_LOOKASIDE_INFORMATION;
  TSystemLookAsideInformation = SYSTEM_LOOKASIDE_INFORMATION;
  PSystemLookAsideInformation = ^TSystemLookAsideInformation;
  {.$ENDIF JWA_INCLUDEMODE}

  _SYSTEM_SET_TIME_SLIP_EVENT = record // Information Class 46
    TimeSlipEvent: HANDLE;
  end;
  SYSTEM_SET_TIME_SLIP_EVENT = _SYSTEM_SET_TIME_SLIP_EVENT;
  PSYSTEM_SET_TIME_SLIP_EVENT = ^SYSTEM_SET_TIME_SLIP_EVENT;
  TSystemSetTimeSlipEvent = SYSTEM_SET_TIME_SLIP_EVENT;
  PSystemSetTimeSlipEvent = ^TSystemSetTimeSlipEvent;

  _SYSTEM_CREATE_SESSION = record // Information Class 47
    Session: ULONG;
  end;
  SYSTEM_CREATE_SESSION = _SYSTEM_CREATE_SESSION;
  PSYSTEM_CREATE_SESSION = ^SYSTEM_CREATE_SESSION;
  TSystemCreateSession = SYSTEM_CREATE_SESSION;
  PSystemCreateSession = ^TSystemCreateSession;

  _SYSTEM_DELETE_SESSION = record // Information Class 48
    Session: ULONG;
  end;
  SYSTEM_DELETE_SESSION = _SYSTEM_DELETE_SESSION;
  PSYSTEM_DELETE_SESSION = ^SYSTEM_DELETE_SESSION;
  TSystemDeleteSession = SYSTEM_DELETE_SESSION;
  PSystemDeleteSession = ^TSystemDeleteSession;

  _SYSTEM_RANGE_START_INFORMATION = record // Information Class 50
    SystemRangeStart: PVOID;
  end;
  SYSTEM_RANGE_START_INFORMATION = _SYSTEM_RANGE_START_INFORMATION;
  PSYSTEM_RANGE_START_INFORMATION = ^SYSTEM_RANGE_START_INFORMATION;
  TSystemRangeStartInformation = SYSTEM_RANGE_START_INFORMATION;
  PSystemRangeStartInformation = ^TSystemRangeStartInformation;

  _SYSTEM_POOL_BLOCK = record
    Allocated: ByteBool;
    Unknown: USHORT;
    Size: ULONG;
    Tag: array[0..3] of AnsiChar;
  end;
  SYSTEM_POOL_BLOCK = _SYSTEM_POOL_BLOCK;
  PSYSTEM_POOL_BLOCK = ^SYSTEM_POOL_BLOCK;
  TSystemPoolBlock = SYSTEM_POOL_BLOCK;
  PSystemPoolBlock = ^TSystemPoolBlock;

  _SYSTEM_POOL_BLOCKS_INFORMATION = record // Info Classes 14 and 15
    PoolSize: ULONG;
    PoolBase: PVOID;
    Unknown: USHORT;
    NumberOfBlocks: ULONG;
    PoolBlocks: array[0..0] of SYSTEM_POOL_BLOCK;
  end;
  SYSTEM_POOL_BLOCKS_INFORMATION = _SYSTEM_POOL_BLOCKS_INFORMATION;
  PSYSTEM_POOL_BLOCKS_INFORMATION = ^SYSTEM_POOL_BLOCKS_INFORMATION;
  TSystemPoolBlocksInformation = SYSTEM_POOL_BLOCKS_INFORMATION;
  PSystemPoolBlocksInformation = ^TSystemPoolBlocksInformation;

  _SYSTEM_MEMORY_USAGE = record
    Name: PVOID;
    Valid: USHORT;
    Standby: USHORT;
    Modified: USHORT;
    PageTables: USHORT;
  end;
  SYSTEM_MEMORY_USAGE = _SYSTEM_MEMORY_USAGE;
  PSYSTEM_MEMORY_USAGE = ^SYSTEM_MEMORY_USAGE;
  TSystemMemoryUsage = SYSTEM_MEMORY_USAGE;
  PSystemMemoryUsage = ^TSystemMemoryUsage;

  _SYSTEM_MEMORY_USAGE_INFORMATION = record // Info Classes 25 and 29
    Reserved: ULONG;
    EndOfData: PVOID;
    MemoryUsage: array[0..0] of SYSTEM_MEMORY_USAGE;
  end;
  SYSTEM_MEMORY_USAGE_INFORMATION = _SYSTEM_MEMORY_USAGE_INFORMATION;
  PSYSTEM_MEMORY_USAGE_INFORMATION = ^SYSTEM_MEMORY_USAGE_INFORMATION;
  TSystemMemoryUsageInformation = SYSTEM_MEMORY_USAGE_INFORMATION;
  PSystemMemoryUsageInformation = ^TSystemMemoryUsageInformation;

type
  _SHUTDOWN_ACTION = (
    ShutdownNoReboot,
    ShutdownReboot,
    ShutdownPowerOff);
  SHUTDOWN_ACTION = _SHUTDOWN_ACTION;
  TShutdownAction = SHUTDOWN_ACTION;

type
  _DEBUG_CONTROL_CODE = (
    DebugFiller0,
    DebugGetTraceInformation,
    DebugSetInternalBreakpoint,
    DebugSetSpecialCall,
    DebugClearSpecialCalls,
    DebugQuerySpecialCalls,
    DebugDbgBreakPoint);
  DEBUG_CONTROL_CODE = _DEBUG_CONTROL_CODE;
  TDebugControlCode = DEBUG_CONTROL_CODE;

type
  _OBJECT_INFORMATION_CLASS = (
    ObjectBasicInformation,
    ObjectNameInformation,
    ObjectTypeInformation,
    ObjectAllTypesInformation,
    ObjectHandleInformation);
  OBJECT_INFORMATION_CLASS = _OBJECT_INFORMATION_CLASS;
  TObjectInformationClass = OBJECT_INFORMATION_CLASS;

type
  _OBJECT_BASIC_INFORMATION = record // Information Class 0
    Attributes: ULONG;
    GrantedAccess: ACCESS_MASK;
    HandleCount: ULONG;
    PointerCount: ULONG;
    PagedPoolUsage: ULONG;
    NonPagedPoolUsage: ULONG;
    Reserved: array[0..2] of ULONG;
    NameInformationLength: ULONG;
    TypeInformationLength: ULONG;
    SecurityDescriptorLength: ULONG;
    CreateTime: LARGE_INTEGER;
  end;
  OBJECT_BASIC_INFORMATION = _OBJECT_BASIC_INFORMATION;
  POBJECT_BASIC_INFORMATION = ^OBJECT_BASIC_INFORMATION;
  TObjectBasicInformation = OBJECT_BASIC_INFORMATION;
  PObjectBasicInformation = ^TObjectBasicInformation;

  _OBJECT_TYPE_INFORMATION = record // Information Class 2
    Name: UNICODE_STRING;
    ObjectCount: ULONG;
    HandleCount: ULONG;
    Reserved1: array[0..3] of ULONG;
    PeakObjectCount: ULONG;
    PeakHandleCount: ULONG;
    Reserved2: array[0..3] of ULONG;
    InvalidAttributes: ULONG;
    GenericMapping: GENERIC_MAPPING;
    ValidAccess: ULONG;
    Unknown: UCHAR;
    MaintainHandleDatabase: ByteBool;
    Reserved3: array[0..1] of UCHAR;
    PoolType: POOL_TYPE;
    PagedPoolUsage: ULONG;
    NonPagedPoolUsage: ULONG;
  end;
  OBJECT_TYPE_INFORMATION = _OBJECT_TYPE_INFORMATION;
  POBJECT_TYPE_INFORMATION = ^OBJECT_TYPE_INFORMATION;
  TObjectTypeInformation = OBJECT_TYPE_INFORMATION;
  PObjectTypeInformation = ^TObjectTypeInformation;

  _OBJECT_ALL_TYPES_INFORMATION = record // Information Class 3
    NumberOfTypes: ULONG;
    TypeInformation: OBJECT_TYPE_INFORMATION;
  end;
  OBJECT_ALL_TYPES_INFORMATION = _OBJECT_ALL_TYPES_INFORMATION;
  POBJECT_ALL_TYPES_INFORMATION = ^OBJECT_ALL_TYPES_INFORMATION;
  TObjectAllTypesInformation = OBJECT_ALL_TYPES_INFORMATION;
  PObjectAllTypesInformation = ^TObjectAllTypesInformation;

  _OBJECT_HANDLE_ATTRIBUTE_INFORMATION = record // Information Class 4
    Inherit: ByteBool;
    ProtectFromClose: ByteBool;
  end;
  OBJECT_HANDLE_ATTRIBUTE_INFORMATION = _OBJECT_HANDLE_ATTRIBUTE_INFORMATION;
  POBJECT_HANDLE_ATTRIBUTE_INFORMATION = ^OBJECT_HANDLE_ATTRIBUTE_INFORMATION;
  TObjectHandleAttributeInformation = OBJECT_HANDLE_ATTRIBUTE_INFORMATION;
  PObjectHandleAttributeInformation = ^TObjectHandleAttributeInformation;

type
  _DIRECTORY_BASIC_INFORMATION = record
    ObjectName: UNICODE_STRING;
    ObjectTypeName: UNICODE_STRING;
  end;
  DIRECTORY_BASIC_INFORMATION = _DIRECTORY_BASIC_INFORMATION;
  PDIRECTORY_BASIC_INFORMATION = ^DIRECTORY_BASIC_INFORMATION;
  TDirectoryBasicInformation = DIRECTORY_BASIC_INFORMATION;
  PDirectoryBasicInformation = ^TDirectoryBasicInformation;

type
  _MEMORY_INFORMATION_CLASS = (
    MemoryBasicInformation,
    MemoryWorkingSetList,
    MemorySectionName,
    MemoryBasicVlmInformation);
  MEMORY_INFORMATION_CLASS = _MEMORY_INFORMATION_CLASS;
  TMemoryInformationClass = MEMORY_INFORMATION_CLASS;
  PMemoryInformationClass = ^TMemoryInformationClass;

type
  {$IFNDEF JWA_INCLUDEMODE}
  _MEMORY_BASIC_INFORMATION = record // Information Class 0
    BaseAddress: PVOID;
    AllocationBase: PVOID;
    AllocationProtect: ULONG;
    RegionSize: ULONG;
    State: ULONG;
    Protect: ULONG;
    Type_: ULONG;
  end;
  MEMORY_BASIC_INFORMATION = _MEMORY_BASIC_INFORMATION;
  PMEMORY_BASIC_INFORMATION = ^MEMORY_BASIC_INFORMATION;
  TMemoryBasicInformation = MEMORY_BASIC_INFORMATION;
  PMemoryBasicInformation = ^TMemoryBasicInformation;
  {$ENDIF JWA_INCLUDEMODE}

  _MEMORY_WORKING_SET_LIST = record // Information Class 1
    NumberOfPages: ULONG;
    WorkingSetList: array[0..0] of ULONG;
  end;
  MEMORY_WORKING_SET_LIST = _MEMORY_WORKING_SET_LIST;
  PMEMORY_WORKING_SET_LIST = ^MEMORY_WORKING_SET_LIST;
  TMemoryWorkingSetList = MEMORY_WORKING_SET_LIST;
  PMemoryWorkingSetList = ^TMemoryWorkingSetList;

  _MEMORY_SECTION_NAME = record // Information Class 2
    SectionFileName: UNICODE_STRING;
  end;
  MEMORY_SECTION_NAME = _MEMORY_SECTION_NAME;
  PMEMORY_SECTION_NAME = ^MEMORY_SECTION_NAME;
  TMemorySectionName = MEMORY_SECTION_NAME;
  PMemorySectionName = ^TMemorySectionName;

type
  _SECTION_INFORMATION_CLASS = (
    SectionBasicInformation,
    SectionImageInformation);
  SECTION_INFORMATION_CLASS = _SECTION_INFORMATION_CLASS;
  TSectionInformationClass = SECTION_INFORMATION_CLASS;

type
  _SECTION_BASIC_INFORMATION = record // Information Class 0
    BaseAddress: PVOID;
    Attributes: ULONG;
    Size: LARGE_INTEGER;
  end;
  SECTION_BASIC_INFORMATION = _SECTION_BASIC_INFORMATION;
  PSECTION_BASIC_INFORMATION = ^SECTION_BASIC_INFORMATION;
  TSectionBasicInformation = SECTION_BASIC_INFORMATION;
  PSectionBasicInformation = ^TSectionBasicInformation;

  _SECTION_IMAGE_INFORMATION = record // Information Class 1
    EntryPoint: PVOID;
    Unknown1: ULONG;
    StackReserve: ULONG;
    StackCommit: ULONG;
    Subsystem: ULONG;
    MinorSubsystemVersion: USHORT;
    MajorSubsystemVersion: USHORT;
    Unknown2: ULONG;
    Characteristics: ULONG;
    ImageNumber: USHORT;
    Executable: ByteBool;
    Unknown3: UCHAR;
    Unknown4: array[0..2] of ULONG;
  end;
  SECTION_IMAGE_INFORMATION = _SECTION_IMAGE_INFORMATION;
  PSECTION_IMAGE_INFORMATION = ^SECTION_IMAGE_INFORMATION;
  TSectionImageInformation = SECTION_IMAGE_INFORMATION;
  PSectionImageInformation = TSectionImageInformation;

type
  _USER_STACK = record
    FixedStackBase: PVOID;
    FixedStackLimit: PVOID;
    ExpandableStackBase: PVOID;
    ExpandableStackLimit: PVOID;
    ExpandableStackBottom: PVOID;
  end;
  USER_STACK = _USER_STACK;
  PUSER_STACK = ^USER_STACK;
  TUserStack = USER_STACK;
  PUserStack = ^TUserStack;

type
  _THREAD_BASIC_INFORMATION = record // Information Class 0
    ExitStatus: NTSTATUS;
    TebBaseAddress: PNT_TIB;
    ClientId: CLIENT_ID;
    AffinityMask: KAFFINITY;
    Priority: KPRIORITY;
    BasePriority: KPRIORITY;
  end;
  THREAD_BASIC_INFORMATION = _THREAD_BASIC_INFORMATION;
  PTHREAD_BASIC_INFORMATION = ^THREAD_BASIC_INFORMATION;
  TThreadBasicInformation = THREAD_BASIC_INFORMATION;
  PThreadBasicInformation = ^TThreadBasicInformation;

type
  _PROCESS_PRIORITY_CLASS = record // Information Class 18
    Foreground: ByteBool;
    PriorityClass: UCHAR;
  end;
  PROCESS_PRIORITY_CLASS = _PROCESS_PRIORITY_CLASS;
  PPROCESS_PRIORITY_CLASS = ^PROCESS_PRIORITY_CLASS;
  TProcessPriorityClass = PROCESS_PRIORITY_CLASS;
  PProcessPriorityClass = ^TProcessPriorityClass;

  _RTL_PROCESS_INFORMATION = record
    Size: ULONG;
    hProcess: HANDLE;
    hThread: HANDLE;
    ClientId: CLIENT_ID;
    ImageInfo: SECTION_IMAGE_INFORMATION;
  end;
  RTL_PROCESS_INFORMATION = _RTL_PROCESS_INFORMATION;
  PRTL_PROCESS_INFORMATION = ^RTL_PROCESS_INFORMATION;
  TRtlProcessInformation = RTL_PROCESS_INFORMATION;
  PRtlProcessInformation = ^RTL_PROCESS_INFORMATION;

type
  _DEBUG_BUFFER = record
    SectionHandle: HANDLE;
    SectionBase: PVOID;
    RemoteSectionBase: PVOID;
    SectionBaseDelta: ULONG;
    EventPairHandle: HANDLE;
    Unknown: array[0..1] of ULONG;
    RemoteThreadHandle: HANDLE;
    InfoClassMask: ULONG;
    SizeOfInfo: ULONG;
    AllocatedSize: ULONG;
    SectionSize: ULONG;
    ModuleInformation: PVOID;
    BackTraceInformation: PVOID;
    HeapInformation: PVOID;
    LockInformation: PVOID;
    Reserved: array[0..7] of PVOID;
  end;
  DEBUG_BUFFER = _DEBUG_BUFFER;
  PDEBUG_BUFFER = ^DEBUG_BUFFER;
  TDebugBuffer = DEBUG_BUFFER;
  PDebugBuffer = ^TDebugBuffer;

const
  PDI_MODULES = $01;
  PDI_BACKTRACE = $02;
  PDI_HEAPS = $04;
  PDI_HEAP_TAGS = $08;
  PDI_HEAP_BLOCKS = $10;
  PDI_LOCKS = $20;

type
  _DEBUG_MODULE_INFORMATION = record // c.f. SYSTEM_MODULE_INFORMATION
    Reserved: array[0..1] of ULONG;
    Base: ULONG;
    Size: ULONG;
    Flags: ULONG;
    Index: USHORT;
    Unknown: USHORT;
    LoadCount: USHORT;
    ModuleNameOffset: USHORT;
    ImageName: array[0..255] of AnsiChar;
  end;
  DEBUG_MODULE_INFORMATION = _DEBUG_MODULE_INFORMATION;
  PDEBUG_MODULE_INFORMATION = ^DEBUG_MODULE_INFORMATION;
  TDebugModuleInformation = DEBUG_MODULE_INFORMATION;
  PDebugModuleInformation = ^TDebugModuleInformation;

  _DEBUG_HEAP_INFORMATION = record
    Base: ULONG;
    Flags: ULONG;
    Granularity: USHORT;
    Unknown: USHORT;
    Allocated: ULONG;
    Committed: ULONG;
    TagCount: ULONG;
    BlockCount: ULONG;
    Reserved: array[0..6] of ULONG;
    Tags: PVOID;
    Blocks: PVOID;
  end;
  DEBUG_HEAP_INFORMATION = _DEBUG_HEAP_INFORMATION;
  PDEBUG_HEAP_INFORMATION = ^DEBUG_HEAP_INFORMATION;
  TDebugHeapInformation = DEBUG_HEAP_INFORMATION;
  PDebugHeapInformation = ^TDebugHeapInformation;

  _DEBUG_LOCK_INFORMATION = record // c.f. SYSTEM_LOCK_INFORMATION
    Address: PVOID;
    Type_: USHORT;
    CreatorBackTraceIndex: USHORT;
    OwnerThreadId: ULONG;
    ActiveCount: ULONG;
    ContentionCount: ULONG;
    EntryCount: ULONG;
    RecursionCount: ULONG;
    NumberOfSharedWaiters: ULONG;
    NumberOfExclusiveWaiters: ULONG;
  end;
  DEBUG_LOCK_INFORMATION = _DEBUG_LOCK_INFORMATION;
  PDEBUG_LOCK_INFORMATION = ^DEBUG_LOCK_INFORMATION;
  TDebugLockInformation = DEBUG_LOCK_INFORMATION;
  PDebugLockInformation = ^TDebugLockInformation;

type
  PTIMER_APC_ROUTINE = procedure(TimerContext: PVOID; TimerLowValue: ULONG; TimerHighValue: LONG); stdcall;

type
  _TIMER_INFORMATION_CLASS = (TimerBasicInformation);
  TIMER_INFORMATION_CLASS = _TIMER_INFORMATION_CLASS;
  TTimerInformationClass = TIMER_INFORMATION_CLASS;

type
  _TIMER_BASIC_INFORMATION = record
    TimeRemaining: LARGE_INTEGER;
    SignalState: ByteBool;
  end;
  TIMER_BASIC_INFORMATION = _TIMER_BASIC_INFORMATION;
  PTIMER_BASIC_INFORMATION = ^TIMER_BASIC_INFORMATION;
  TTimerBasicInformation = TIMER_BASIC_INFORMATION;
  PTimerBasicInformation = ^TTimerBasicInformation;

type
  _EVENT_INFORMATION_CLASS = (EventBasicInformation);
  EVENT_INFORMATION_CLASS = _EVENT_INFORMATION_CLASS;
  TEventInformationClass = EVENT_INFORMATION_CLASS;

type
  _EVENT_BASIC_INFORMATION = record
    EventType: EVENT_TYPE;
    SignalState: LONG;
  end;
  EVENT_BASIC_INFORMATION = _EVENT_BASIC_INFORMATION;
  PEVENT_BASIC_INFORMATION = ^EVENT_BASIC_INFORMATION;
  TEventBasicInformation = EVENT_BASIC_INFORMATION;
  PEventBasicInformation = ^TEventBasicInformation;

type
  _SEMAPHORE_INFORMATION_CLASS = (SemaphoreBasicInformation);
  SEMAPHORE_INFORMATION_CLASS = _SEMAPHORE_INFORMATION_CLASS;
  TSemaphoreInformationClass = SEMAPHORE_INFORMATION_CLASS;

type
  _SEMAPHORE_BASIC_INFORMATION = record
    CurrentCount: LONG;
    MaximumCount: LONG;
  end;
  SEMAPHORE_BASIC_INFORMATION = _SEMAPHORE_BASIC_INFORMATION;
  PSEMAPHORE_BASIC_INFORMATION = ^SEMAPHORE_BASIC_INFORMATION;
  TSemaphoreBasicInformation = SEMAPHORE_BASIC_INFORMATION;

type
  _MUTANT_INFORMATION_CLASS = (MutantBasicInformation);
  MUTANT_INFORMATION_CLASS = _MUTANT_INFORMATION_CLASS;
  TMutantInformationClass = MUTANT_INFORMATION_CLASS;

type
  _MUTANT_BASIC_INFORMATION = record
    SignalState: LONG;
    Owned: ByteBool;
    Abandoned: ByteBool;
  end;
  MUTANT_BASIC_INFORMATION = _MUTANT_BASIC_INFORMATION;
  PMUTANT_BASIC_INFORMATION = ^MUTANT_BASIC_INFORMATION;
  TMutantBasicInformation = MUTANT_BASIC_INFORMATION;
  PMutantBasicInformation = ^TMutantBasicInformation;

type
  _IO_COMPLETION_INFORMATION_CLASS = (IoCompletionBasicInformation);
  IO_COMPLETION_INFORMATION_CLASS = _IO_COMPLETION_INFORMATION_CLASS;
  TIoCompletionInformationClass = IO_COMPLETION_INFORMATION_CLASS;

type
  _IO_COMPLETION_BASIC_INFORMATION = record
    SignalState: LONG;
  end;
  IO_COMPLETION_BASIC_INFORMATION = _IO_COMPLETION_BASIC_INFORMATION;
  PIO_COMPLETION_BASIC_INFORMATION = ^IO_COMPLETION_BASIC_INFORMATION;
  TIoCompletionBasicInformation = IO_COMPLETION_BASIC_INFORMATION;
  PIoCompletionBasicInformation = ^TIoCompletionBasicInformation;

type
  _PORT_MESSAGE = record
    DataSize: USHORT;
    MessageSize: USHORT;
    MessageType: USHORT;
    VirtualRangesOffset: USHORT;
    ClientId: CLIENT_ID;
    MessageId: ULONG;
    SectionSize: ULONG;
    // UCHAR Data[];
  end;
  PORT_MESSAGE = _PORT_MESSAGE;
  PPORT_MESSAGE = ^PORT_MESSAGE;
  TPortMessage = PORT_MESSAGE;
  PPortMessage = ^TPortMessage;

  _LPC_TYPE = (
    LPC_NEW_MESSAGE, // A new message
    LPC_REQUEST, // A request message
    LPC_REPLY, // A reply to a request message
    LPC_DATAGRAM, //
    LPC_LOST_REPLY, //
    LPC_PORT_CLOSED, // Sent when port is deleted
    LPC_CLIENT_DIED, // Messages to thread termination ports
    LPC_EXCEPTION, // Messages to thread exception port
    LPC_DEBUG_EVENT, // Messages to thread debug port
    LPC_ERROR_EVENT, // Used by ZwRaiseHardError
    LPC_CONNECTION_REQUEST); // Used by ZwConnectPort
  LPC_TYPE = _LPC_TYPE;
  TLpcType = LPC_TYPE;

  _PORT_SECTION_WRITE = record
    Length: ULONG;
    SectionHandle: HANDLE;
    SectionOffset: ULONG;
    ViewSize: ULONG;
    ViewBase: PVOID;
    TargetViewBase: PVOID;
  end;
  PORT_SECTION_WRITE = _PORT_SECTION_WRITE;
  PPORT_SECTION_WRITE = ^PORT_SECTION_WRITE;
  TPortSectionWrite = PORT_SECTION_WRITE;
  PPortSectionWrite = ^TPortSectionWrite;

  _PORT_SECTION_READ = record
    Length: ULONG;
    ViewSize: ULONG;
    ViewBase: ULONG;
  end;
  PORT_SECTION_READ = _PORT_SECTION_READ;
  PPORT_SECTION_READ = ^PORT_SECTION_READ;
  TPortSectionRead = PORT_SECTION_READ;
  PPortSectionRead = ^TPortSectionRead;

type
  _PORT_INFORMATION_CLASS = (PortBasicInformation);
  PORT_INFORMATION_CLASS = _PORT_INFORMATION_CLASS;
  TPortInformationClass = PORT_INFORMATION_CLASS;


type
  _PORT_BASIC_INFORMATION = record
  end;
  PORT_BASIC_INFORMATION = _PORT_BASIC_INFORMATION;
  PPORT_BASIC_INFORMATION = ^PORT_BASIC_INFORMATION;
  TPortBasicInformation = PORT_BASIC_INFORMATION;
  PPortBasicInformation = ^TPortBasicInformation;

type
  _FILE_GET_EA_INFORMATION = record
    NextEntryOffset: ULONG;
    EaNameLength: UCHAR;
    EaName: array[0..0] of AnsiChar;
  end;
  FILE_GET_EA_INFORMATION = _FILE_GET_EA_INFORMATION;
  PFILE_GET_EA_INFORMATION = ^FILE_GET_EA_INFORMATION;
  TFileGetEaInformation = FILE_GET_EA_INFORMATION;
  PFileGetEaInformation = ^TFileGetEaInformation;

type
  _FILE_FS_VOLUME_INFORMATION = record
    VolumeCreationTime: LARGE_INTEGER;
    VolumeSerialNumber: ULONG;
    VolumeLabelLength: ULONG;
    Unknown: UCHAR;
    VolumeLabel: array[0..0] of WCHAR;
  end;
  FILE_FS_VOLUME_INFORMATION = _FILE_FS_VOLUME_INFORMATION;
  PFILE_FS_VOLUME_INFORMATION = ^FILE_FS_VOLUME_INFORMATION;
  TFileFsVolumeInformation = FILE_FS_VOLUME_INFORMATION;
  PFileFsVolumeInformation = ^TFileFsVolumeInformation;

  _FILE_FS_LABEL_INFORMATION = record
    VolumeLabelLength: ULONG;
    VolumeLabel: WCHAR;
  end;
  FILE_FS_LABEL_INFORMATION = _FILE_FS_LABEL_INFORMATION;
  PFILE_FS_LABEL_INFORMATION = ^FILE_FS_LABEL_INFORMATION;
  TFileFsLabelInformation = FILE_FS_LABEL_INFORMATION;
  PFileFsLabelInformation = ^TFileFsLabelInformation;

  _FILE_FS_SIZE_INFORMATION = record
    TotalAllocationUnits: LARGE_INTEGER;
    AvailableAllocationUnits: LARGE_INTEGER;
    SectorsPerAllocationUnit: ULONG;
    BytesPerSector: ULONG;
  end;
  FILE_FS_SIZE_INFORMATION = _FILE_FS_SIZE_INFORMATION;
  PFILE_FS_SIZE_INFORMATION = ^FILE_FS_SIZE_INFORMATION;
  TFileFsSizeInformation = FILE_FS_SIZE_INFORMATION;
  PFileFsSizeInformation = ^TFileFsSizeInformation;

  _FILE_FS_ATTRIBUTE_INFORMATION = record
    FileSystemFlags: ULONG;
    MaximumComponentNameLength: ULONG;
    FileSystemNameLength: ULONG;
    FileSystemName: array[0..0] of WCHAR;
  end;
  FILE_FS_ATTRIBUTE_INFORMATION = _FILE_FS_ATTRIBUTE_INFORMATION;
  PFILE_FS_ATTRIBUTE_INFORMATION = ^FILE_FS_ATTRIBUTE_INFORMATION;
  TFileFsAttributeInformation = FILE_FS_ATTRIBUTE_INFORMATION;
  PFileFsAttributeInformation = ^TFileFsAttributeInformation;

  _FILE_FS_CONTROL_INFORMATION = record
    Reserved: array[0..2] of LARGE_INTEGER;
    DefaultQuotaThreshold: LARGE_INTEGER;
    DefaultQuotaLimit: LARGE_INTEGER;
    QuotaFlags: ULONG;
  end;
  FILE_FS_CONTROL_INFORMATION = _FILE_FS_CONTROL_INFORMATION;
  PFILE_FS_CONTROL_INFORMATION = ^FILE_FS_CONTROL_INFORMATION;
  TFileFsControlInformation = FILE_FS_CONTROL_INFORMATION;
  PFileFsControlInformation = ^TFileFsControlInformation;

  _FILE_FS_FULL_SIZE_INFORMATION = record
    TotalQuotaAllocationUnits: LARGE_INTEGER;
    AvailableQuotaAllocationUnits: LARGE_INTEGER;
    AvailableAllocationUnits: LARGE_INTEGER;
    SectorsPerAllocationUnit: ULONG;
    BytesPerSector: ULONG;
  end;
  FILE_FS_FULL_SIZE_INFORMATION = _FILE_FS_FULL_SIZE_INFORMATION;
  PFILE_FS_FULL_SIZE_INFORMATION = ^FILE_FS_FULL_SIZE_INFORMATION;
  TFileFsFullSizeInformation = FILE_FS_FULL_SIZE_INFORMATION;
  PFileFsFullSizeInformation = ^TFileFsFullSizeInformation;

  _FILE_FS_OBJECT_ID_INFORMATION = record
    VolumeObjectId: UUID;
    VolumeObjectIdExtendedInfo: array[0..11] of ULONG;
  end;
  FILE_FS_OBJECT_ID_INFORMATION = _FILE_FS_OBJECT_ID_INFORMATION;
  PFILE_FS_OBJECT_ID_INFORMATION = ^FILE_FS_OBJECT_ID_INFORMATION;
  TFileFsObjectIdInformation = FILE_FS_OBJECT_ID_INFORMATION;
  PFileFsObjectIdInformation = ^TFileFsObjectIdInformation;

  _FILE_USER_QUOTA_INFORMATION = record
    NextEntryOffset: ULONG;
    SidLength: ULONG;
    ChangeTime: LARGE_INTEGER;
    QuotaUsed: LARGE_INTEGER;
    QuotaThreshold: LARGE_INTEGER;
    QuotaLimit: LARGE_INTEGER;
    Sid: array[0..0] of SID;
  end;
  FILE_USER_QUOTA_INFORMATION = _FILE_USER_QUOTA_INFORMATION;
  PFILE_USER_QUOTA_INFORMATION = ^FILE_USER_QUOTA_INFORMATION;
  TFileUserQuotaInformation = FILE_USER_QUOTA_INFORMATION;
  PFileUserQuotaInformation = ^TFileUserQuotaInformation;

  _FILE_QUOTA_LIST_INFORMATION = record
    NextEntryOffset: ULONG;
    SidLength: ULONG;
    Sid: array[0..0] of SID;
  end;
  FILE_QUOTA_LIST_INFORMATION = _FILE_QUOTA_LIST_INFORMATION;
  PFILE_QUOTA_LIST_INFORMATION = ^FILE_QUOTA_LIST_INFORMATION;
  TFileQuotaListInformation = FILE_QUOTA_LIST_INFORMATION;
  PFileQuotaListInformation = ^TFileQuotaListInformation;

type
  _FILE_DIRECTORY_INFORMATION = record // Information Class 1
    NextEntryOffset: ULONG;
    Unknown: ULONG;
    CreationTime: LARGE_INTEGER;
    LastAccessTime: LARGE_INTEGER;
    LastWriteTime: LARGE_INTEGER;
    ChangeTime: LARGE_INTEGER;
    EndOfFile: LARGE_INTEGER;
    AllocationSize: LARGE_INTEGER;
    FileAttributes: ULONG;
    FileNameLength: ULONG;
    FileName: array[0..0] of WCHAR;
  end;
  FILE_DIRECTORY_INFORMATION = _FILE_DIRECTORY_INFORMATION;
  PFILE_DIRECTORY_INFORMATION = ^FILE_DIRECTORY_INFORMATION;
  TFileDirectoryInformation = FILE_DIRECTORY_INFORMATION;
  PFileDirectoryInformation = ^TFileDirectoryInformation;

  _FILE_FULL_DIRECTORY_INFORMATION = record // Information Class 2
    NextEntryOffset: ULONG;
    Unknown: ULONG;
    CreationTime: LARGE_INTEGER;
    LastAccessTime: LARGE_INTEGER;
    LastWriteTime: LARGE_INTEGER;
    ChangeTime: LARGE_INTEGER;
    EndOfFile: LARGE_INTEGER;
    AllocationSize: LARGE_INTEGER;
    FileAttributes: ULONG;
    FileNameLength: ULONG;
    EaInformationLength: ULONG;
    FileName: array[0..0] of WCHAR;
  end;
  FILE_FULL_DIRECTORY_INFORMATION = _FILE_FULL_DIRECTORY_INFORMATION;
  PFILE_FULL_DIRECTORY_INFORMATION = ^FILE_FULL_DIRECTORY_INFORMATION;
  TFileFullDirectoryInformation = FILE_FULL_DIRECTORY_INFORMATION;
  PFileFullDirectoryInformation = ^TFileFullDirectoryInformation;

  _FILE_BOTH_DIRECTORY_INFORMATION = record // Information Class 3
    NextEntryOffset: ULONG;
    Unknown: ULONG;
    CreationTime: LARGE_INTEGER;
    LastAccessTime: LARGE_INTEGER;
    LastWriteTime: LARGE_INTEGER;
    ChangeTime: LARGE_INTEGER;
    EndOfFile: LARGE_INTEGER;
    AllocationSize: LARGE_INTEGER;
    FileAttributes: ULONG;
    FileNameLength: ULONG;
    EaInformationLength: ULONG;
    AlternateNameLength: UCHAR;
    AlternateName: array[0..11] of WCHAR;
    FileName: array[0..0] of WCHAR;
  end;
  FILE_BOTH_DIRECTORY_INFORMATION = _FILE_BOTH_DIRECTORY_INFORMATION;
  PFILE_BOTH_DIRECTORY_INFORMATION = ^FILE_BOTH_DIRECTORY_INFORMATION;
  TFileBothDirectoryInformation = FILE_BOTH_DIRECTORY_INFORMATION;
  PFileBothDirectoryInformation = ^TFileBothDirectoryInformation;

  _FILE_INTERNAL_INFORMATION = record // Information Class 6
    FileId: LARGE_INTEGER;
  end;
  FILE_INTERNAL_INFORMATION = _FILE_INTERNAL_INFORMATION;
  PFILE_INTERNAL_INFORMATION = ^FILE_INTERNAL_INFORMATION;
  TFileInternalInformation = FILE_INTERNAL_INFORMATION;
  PFileInternalInformation = ^TFileInternalInformation;

  _FILE_EA_INFORMATION = record // Information Class 7
    EaInformationLength: ULONG;
  end;
  FILE_EA_INFORMATION = _FILE_EA_INFORMATION;
  PFILE_EA_INFORMATION = ^FILE_EA_INFORMATION;
  TFileEaInformation = FILE_EA_INFORMATION;
  PFileEaInformation = ^TFileEaInformation;

  _FILE_ACCESS_INFORMATION = record // Information Class 8
    GrantedAccess: ACCESS_MASK;
  end;
  FILE_ACCESS_INFORMATION = _FILE_ACCESS_INFORMATION;
  PFILE_ACCESS_INFORMATION = ^FILE_ACCESS_INFORMATION;
  TFileAccessInformation = FILE_ACCESS_INFORMATION;
  PFileAccessInformation = ^TFileAccessInformation;

  _FILE_NAME_INFORMATION = record // Information Classes 9 and 21
    FileNameLength: ULONG;
    FileName: array[0..0] of WCHAR;
  end;
  FILE_NAME_INFORMATION = _FILE_NAME_INFORMATION;
  PFILE_NAME_INFORMATION = ^FILE_NAME_INFORMATION;
  FILE_ALTERNATE_NAME_INFORMATION = _FILE_NAME_INFORMATION;
  PFILE_ALTERNATE_NAME_INFORMATION = ^FILE_ALTERNATE_NAME_INFORMATION;
  TFileNameInformation = FILE_NAME_INFORMATION;
  PFileNameInformation = ^TFileNameInformation;

  _FILE_LINK_RENAME_INFORMATION = record // Info Classes 10 and 11
    ReplaceIfExists: ByteBool;
    RootDirectory: HANDLE;
    FileNameLength: ULONG;
    FileName: array[0..0] of WCHAR;
  end;
  FILE_LINK_INFORMATION = _FILE_LINK_RENAME_INFORMATION;
  PFILE_LINK_INFORMATION = ^FILE_LINK_INFORMATION;
  FILE_RENAME_INFORMATION = _FILE_LINK_RENAME_INFORMATION;
  PFILE_RENAME_INFORMATION = ^FILE_RENAME_INFORMATION;
  TFileLinkInformation = FILE_LINK_INFORMATION;
  PFileLinkInformation = ^TFileLinkInformation;

  _FILE_NAMES_INFORMATION = record // Information Class 12
    NextEntryOffset: ULONG;
    Unknown: ULONG;
    FileNameLength: ULONG;
    FileName: array[0..0] of WCHAR;
  end;
  FILE_NAMES_INFORMATION = _FILE_NAMES_INFORMATION;
  PFILE_NAMES_INFORMATION = ^FILE_NAMES_INFORMATION;
  TFileNamesInformation = FILE_NAMES_INFORMATION;
  PFileNamesInformation = ^TFileNamesInformation;

  _FILE_MODE_INFORMATION = record // Information Class 16
    Mode: ULONG;
  end;
  FILE_MODE_INFORMATION = _FILE_MODE_INFORMATION;
  PFILE_MODE_INFORMATION = ^FILE_MODE_INFORMATION;
  TFileModeInformation = FILE_MODE_INFORMATION;
  PFileModeInformation = ^TFileModeInformation;

  _FILE_ALL_INFORMATION = record // Information Class 18
    BasicInformation: FILE_BASIC_INFORMATION;
    StandardInformation: FILE_STANDARD_INFORMATION;
    InternalInformation: FILE_INTERNAL_INFORMATION;
    EaInformation: FILE_EA_INFORMATION;
    AccessInformation: FILE_ACCESS_INFORMATION;
    PositionInformation: FILE_POSITION_INFORMATION;
    ModeInformation: FILE_MODE_INFORMATION;
    AlignmentInformation: FILE_ALIGNMENT_INFORMATION;
    NameInformation: FILE_NAME_INFORMATION;
  end;
  FILE_ALL_INFORMATION = _FILE_ALL_INFORMATION;
  PFILE_ALL_INFORMATION = ^FILE_ALL_INFORMATION;
  TFileAllInformation = FILE_ALL_INFORMATION;
  PFileAllInformation = ^TFileAllInformation;

  _FILE_ALLOCATION_INFORMATION = record // Information Class 19
    AllocationSize: LARGE_INTEGER;
  end;
  FILE_ALLOCATION_INFORMATION = _FILE_ALLOCATION_INFORMATION;
  PFILE_ALLOCATION_INFORMATION = ^FILE_ALLOCATION_INFORMATION;
  TFileAllocationInformation = FILE_ALLOCATION_INFORMATION;
  PFileAllocationInformation = ^TFileAllocationInformation;

  _FILE_STREAM_INFORMATION = record // Information Class 22
    NextEntryOffset: ULONG;
    StreamNameLength: ULONG;
    EndOfStream: LARGE_INTEGER;
    AllocationSize: LARGE_INTEGER;
    StreamName: array[0..0] of WCHAR;
  end;
  FILE_STREAM_INFORMATION = _FILE_STREAM_INFORMATION;
  PFILE_STREAM_INFORMATION = ^FILE_STREAM_INFORMATION;
  TFileStreamInformation = FILE_STREAM_INFORMATION;
  PFileStreamInformation = ^TFileStreamInformation;

  _FILE_PIPE_INFORMATION = record // Information Class 23
    ReadModeMessage: ULONG;
    WaitModeBlocking: ULONG;
  end;
  FILE_PIPE_INFORMATION = _FILE_PIPE_INFORMATION;
  PFILE_PIPE_INFORMATION = ^FILE_PIPE_INFORMATION;
  TFilePipeInformation = FILE_PIPE_INFORMATION;
  PFilePipeInformation = ^TFilePipeInformation;

  _FILE_PIPE_LOCAL_INFORMATION = record // Information Class 24
    MessageType: ULONG;
    Unknown1: ULONG;
    MaxInstances: ULONG;
    CurInstances: ULONG;
    InBufferSize: ULONG;
    Unknown2: ULONG;
    OutBufferSize: ULONG;
    Unknown3: array[0..1] of ULONG;
    ServerEnd: ULONG;
  end;
  FILE_PIPE_LOCAL_INFORMATION = _FILE_PIPE_LOCAL_INFORMATION;
  PFILE_PIPE_LOCAL_INFORMATION = ^FILE_PIPE_LOCAL_INFORMATION;
  TFilePipeLocalInformation = FILE_PIPE_LOCAL_INFORMATION;
  PFilePipeLocalInformation = ^TFilePipeLocalInformation;

  _FILE_PIPE_REMOTE_INFORMATION = record // Information Class 25
    CollectDataTimeout: LARGE_INTEGER;
    MaxCollectionCount: ULONG;
  end;
  FILE_PIPE_REMOTE_INFORMATION = _FILE_PIPE_REMOTE_INFORMATION;
  PFILE_PIPE_REMOTE_INFORMATION = ^FILE_PIPE_REMOTE_INFORMATION;
  TFilePipeRemoteInformation = FILE_PIPE_REMOTE_INFORMATION;
  PFilePipeRemoteInformation = ^TFilePipeRemoteInformation;

  _FILE_MAILSLOT_QUERY_INFORMATION = record // Information Class 26
    MaxMessageSize: ULONG;
    Unknown: ULONG;
    NextSize: ULONG;
    MessageCount: ULONG;
    ReadTimeout: LARGE_INTEGER;
  end;
  FILE_MAILSLOT_QUERY_INFORMATION = _FILE_MAILSLOT_QUERY_INFORMATION;
  PFILE_MAILSLOT_QUERY_INFORMATION = ^FILE_MAILSLOT_QUERY_INFORMATION;
  TFileMailslotQueryInformation = FILE_MAILSLOT_QUERY_INFORMATION;
  PFileMailslotQueryInformation = ^TFileMailslotQueryInformation;

  _FILE_MAILSLOT_SET_INFORMATION = record // Information Class 27
    ReadTimeout: LARGE_INTEGER;
  end;
  FILE_MAILSLOT_SET_INFORMATION = _FILE_MAILSLOT_SET_INFORMATION;
  PFILE_MAILSLOT_SET_INFORMATION = ^FILE_MAILSLOT_SET_INFORMATION;
  TFileMailslotSetInformation = FILE_MAILSLOT_SET_INFORMATION;
  PFileMailslotSetInformation = ^TFileMailslotSetInformation;

  _FILE_COMPRESSION_INFORMATION = record // Information Class 28
    CompressedSize: LARGE_INTEGER;
    CompressionFormat: USHORT;
    CompressionUnitShift: UCHAR;
    Unknown: UCHAR;
    ClusterSizeShift: UCHAR;
  end;
  FILE_COMPRESSION_INFORMATION = _FILE_COMPRESSION_INFORMATION;
  PFILE_COMPRESSION_INFORMATION = ^FILE_COMPRESSION_INFORMATION;
  TFileCompressionInformation = FILE_COMPRESSION_INFORMATION;
  PFileCompressionInformation = ^TFileCompressionInformation;

  _FILE_COMPLETION_INFORMATION = record // Information Class 30
    IoCompletionHandle: HANDLE;
    CompletionKey: ULONG;
  end;
  FILE_COMPLETION_INFORMATION = _FILE_COMPLETION_INFORMATION;
  PFILE_COMPLETION_INFORMATION = ^FILE_COMPLETION_INFORMATION;
  TFileCompletionInformation = FILE_COMPLETION_INFORMATION;
  PFileCompletionInformation = ^TFileCompletionInformation;

type
  PEXECUTION_STATE = ^EXECUTION_STATE;
  PExecutionState = PEXECUTION_STATE;

{$IFNDEF JWA_INCLUDEMODE}
type
  PLANGID = ^LANGID;
{$ENDIF JWA_INCLUDEMODE}

type
  _ATOM_INFORMATION_CLASS = (AtomBasicInformation, AtomListInformation);
  ATOM_INFORMATION_CLASS = _ATOM_INFORMATION_CLASS;
  TAtomInformationClass = ATOM_INFORMATION_CLASS;

type
  _ATOM_BASIC_INFORMATION = record
    ReferenceCount: USHORT;
    Pinned: USHORT;
    NameLength: USHORT;
    Name: array[0..0] of WCHAR;
  end;
  ATOM_BASIC_INFORMATION = _ATOM_BASIC_INFORMATION;
  PATOM_BASIC_INFORMATION = ^ATOM_BASIC_INFORMATION;
  TAtomBasicInformation = ATOM_BASIC_INFORMATION;
  PAtomBasicInformation = ^TAtomBasicInformation;

  _ATOM_LIST_INFORMATION = record
    NumberOfAtoms: ULONG;
    Atoms: array[0..0] of ATOM;
  end;
  ATOM_LIST_INFORMATION = _ATOM_LIST_INFORMATION;
  PATOM_LIST_INFORMATION = ^ATOM_LIST_INFORMATION;
  TAtomListInformation = ATOM_LIST_INFORMATION;
  PAtomListInformation = ^TAtomListInformation;

//==============================================================================
// NTFS on disk structure structures
//==============================================================================

type
  _NTFS_RECORD_HEADER = record
    Type_: ULONG;
    UsaOffset: USHORT;
    UsaCount: USHORT;
    Usn: USN;
  end;
  NTFS_RECORD_HEADER = _NTFS_RECORD_HEADER;
  PNTFS_RECORD_HEADER = ^NTFS_RECORD_HEADER;
  TNtfsRecordHeader = NTFS_RECORD_HEADER;
  PNtfsRecordHeader = ^TNtfsRecordHeader;

  _FILE_RECORD_HEADER = record
    Ntfs: NTFS_RECORD_HEADER;
    SequenceNumber: USHORT;
    LinkCount: USHORT;
    AttributesOffset: USHORT;
    Flags: USHORT; // 0x0001 = InUse, 0x0002 = Directory
    BytesInUse: ULONG;
    BytesAllocated: ULONG;
    BaseFileRecord: ULONGLONG;
    NextAttributeNumber: USHORT;
  end;
  FILE_RECORD_HEADER = _FILE_RECORD_HEADER;
  PFILE_RECORD_HEADER = ^FILE_RECORD_HEADER;
  TFileRecordHeader = FILE_RECORD_HEADER;
  PFileRecordHeader = ^TFileRecordHeader;

const
  AttributeStandardInformation = $10;
  AttributeAttributeList = $20;
  AttributeFileName = $30;
  AttributeObjectId = $40;
  AttributeSecurityDescriptor = $50;
  AttributeVolumeName = $60;
  AttributeVolumeInformation = $70;
  AttributeData = $80;
  AttributeIndexRoot = $90;
  AttributeIndexAllocation = $A0;
  AttributeBitmap = $B0;
  AttributeReparsePoint = $C0;
  AttributeEAInformation = $D0;
  AttributeEA = $E0;
  AttributePropertySet = $F0;
  AttributeLoggedUtilityStream = $100;

type
  ATTRIBUTE_TYPE = AttributeStandardInformation..AttributeLoggedUtilityStream;
  PATTRIBUTE_TYPE = ^ATTRIBUTE_TYPE;
  TAttributeType = ATTRIBUTE_TYPE;

  _ATTRIBUTE = record
    AttributeType: ATTRIBUTE_TYPE;
    Length: ULONG;
    Nonresident: ByteBool;
    NameLength: UCHAR;
    NameOffset: USHORT;
    Flags: USHORT; // 0x0001 = Compressed
    AttributeNumber: USHORT;
  end;
  ATTRIBUTE = _ATTRIBUTE;
  PATTRIBUTE = ^ATTRIBUTE;
  TAttribute = ATTRIBUTE;

  _RESIDENT_ATTRIBUTE = record
    Attribute: ATTRIBUTE;
    ValueLength: ULONG;
    ValueOffset: USHORT;
    Flags: USHORT; // 0x0001 = Indexed
  end;
  RESIDENT_ATTRIBUTE = _RESIDENT_ATTRIBUTE;
  PRESIDENT_ATTRIBUTE = ^RESIDENT_ATTRIBUTE;
  TResidentAttribute = RESIDENT_ATTRIBUTE;
  PResidentAttribute = ^TResidentAttribute;

  _NONRESIDENT_ATTRIBUTE = record
    Attribute: ATTRIBUTE;
    LowVcn: ULONGLONG;
    HighVcn: ULONGLONG;
    RunArrayOffset: USHORT;
    CompressionUnit: UCHAR;
    AlignmentOrReserved: array[0..4] of UCHAR;
    AllocatedSize: ULONGLONG;
    DataSize: ULONGLONG;
    InitializedSize: ULONGLONG;
    CompressedSize: ULONGLONG; // Only when compressed
  end;
  NONRESIDENT_ATTRIBUTE = _NONRESIDENT_ATTRIBUTE;
  PNONRESIDENT_ATTRIBUTE = ^NONRESIDENT_ATTRIBUTE;
  TNonResidentAttribute = NONRESIDENT_ATTRIBUTE;
  PNonResidentAttribute = ^TNonResidentAttribute;

  _STANDARD_INFORMATION = record
    CreationTime: ULONGLONG;
    ChangeTime: ULONGLONG;
    LastWriteTime: ULONGLONG;
    LastAccessTime: ULONGLONG;
    FileAttributes: ULONG;
    AlignmentOrReservedOrUnknown: array[0..2] of ULONG;
    QuotaId: ULONG; // NTFS 3.0 only
    SecurityId: ULONG; // NTFS 3.0 only
    QuotaCharge: ULONGLONG; // NTFS 3.0 only
    Usn: USN; // NTFS 3.0 only
  end;
  STANDARD_INFORMATION = _STANDARD_INFORMATION;
  PSTANDARD_INFORMATION = ^STANDARD_INFORMATION;
  TStandardInformation = STANDARD_INFORMATION;
  PStandardInformation = ^TStandardInformation;

  _ATTRIBUTE_LIST = record
    AttributeType: ATTRIBUTE_TYPE;
    Length: USHORT;
    NameLength: UCHAR;
    NameOffset: UCHAR;
    LowVcn: ULONGLONG;
    FileReferenceNumber: ULONGLONG;
    AttributeNumber: USHORT;
    AlignmentOrReserved: array[0..2] of USHORT;
  end;
  ATTRIBUTE_LIST = _ATTRIBUTE_LIST;
  PATTRIBUTE_LIST = ^ATTRIBUTE_LIST;
  TAttributeList = ATTRIBUTE_LIST;
  PAttributeList = ^TAttributeList;

  _FILENAME_ATTRIBUTE = record
    DirectoryFileReferenceNumber: ULONGLONG;
    CreationTime: ULONGLONG; // Saved when filename last changed
    ChangeTime: ULONGLONG; // ditto
    LastWriteTime: ULONGLONG; // ditto
    LastAccessTime: ULONGLONG; // ditto
    AllocatedSize: ULONGLONG; // ditto
    DataSize: ULONGLONG; // ditto
    FileAttributes: ULONG; // ditto
    AlignmentOrReserved: ULONG;
    NameLength: UCHAR;
    NameType: UCHAR; // 0x01 = Long, 0x02 = Short
    Name: array[0..0] of UCHAR;
  end;
  FILENAME_ATTRIBUTE = _FILENAME_ATTRIBUTE;
  PFILENAME_ATTRIBUTE = ^FILENAME_ATTRIBUTE;
  TFilenameAttribute = FILENAME_ATTRIBUTE;
  PFilenameAttribute = ^TFilenameAttribute;

  _OBJECTID_ATTRIBUTE = record
    ObjectId: GUID;
    case Integer of
      0: (
        BirthVolumeId: GUID;
        BirthObjectId: GUID;
        DomainId: GUID);
      1: (
        ExtendedInfo: array[0..47] of UCHAR
        );
  end;
  OBJECTID_ATTRIBUTE = _OBJECTID_ATTRIBUTE;
  POBJECTID_ATTRIBUTE = ^OBJECTID_ATTRIBUTE;
  TObjectIdAttribute = OBJECTID_ATTRIBUTE;
  PObjectIdAttribute = ^TObjectIdAttribute;

  _VOLUME_INFORMATION = record
    Unknown: array[0..1] of ULONG;
    MajorVersion: UCHAR;
    MinorVersion: UCHAR;
    Flags: USHORT;
  end;
  VOLUME_INFORMATION = _VOLUME_INFORMATION;
  PVOLUME_INFORMATION = ^VOLUME_INFORMATION;
  TVolumeInformation = VOLUME_INFORMATION;
  PVolumeInformation = ^TVolumeInformation;

  _DIRECTORY_INDEX = record
    EntriesOffset: ULONG;
    IndexBlockLength: ULONG;
    AllocatedSize: ULONG;
    Flags: ULONG; // 0x00 = Small directory, 0x01 = Large directory
  end;
  DIRECTORY_INDEX = _DIRECTORY_INDEX;
  PDIRECTORY_INDEX = ^DIRECTORY_INDEX;
  TDirectoryIndex = DIRECTORY_INDEX;
  PDirectoryIndex = ^TDirectoryIndex;

  _DIRECTORY_ENTRY = record
    FileReferenceNumber: ULONGLONG;
    Length: USHORT;
    AttributeLength: USHORT;
    Flags: ULONG; // 0x01 = Has trailing VCN, 0x02 = Last entry
    // FILENAME_ATTRIBUTE Name;
    // ULONGLONG Vcn;       // VCN in IndexAllocation of earlier entries
  end;
  DIRECTORY_ENTRY = _DIRECTORY_ENTRY;
  PDIRECTORY_ENTRY = ^DIRECTORY_ENTRY;
  TDirectoryEntry = DIRECTORY_ENTRY;
  PDirectoryEntry = ^TDirectoryEntry;

  _INDEX_ROOT = record
    Type_: ATTRIBUTE_TYPE;
    CollationRule: ULONG;
    BytesPerIndexBlock: ULONG;
    ClustersPerIndexBlock: ULONG;
    DirectoryIndex: DIRECTORY_INDEX;
  end;
  INDEX_ROOT = _INDEX_ROOT;
  PINDEX_ROOT = ^INDEX_ROOT;
  TIndexRoot = INDEX_ROOT;
  PIndexRoot = ^TIndexRoot;

  _INDEX_BLOCK_HEADER = record
    Ntfs: NTFS_RECORD_HEADER;
    IndexBlockVcn: ULONGLONG;
    DirectoryIndex: DIRECTORY_INDEX;
  end;
  INDEX_BLOCK_HEADER = _INDEX_BLOCK_HEADER;
  PINDEX_BLOCK_HEADER = ^INDEX_BLOCK_HEADER;
  TIndexBlockHeader = _INDEX_BLOCK_HEADER;
  PIndexBlockHeader = ^TIndexBlockHeader;

  _REPARSE_POINT = record
    ReparseTag: ULONG;
    ReparseDataLength: USHORT;
    Reserved: USHORT;
    ReparseData: array[0..0] of UCHAR;
  end;
  REPARSE_POINT = _REPARSE_POINT;
  PREPARSE_POINT = ^REPARSE_POINT;
  TReparsePoint = REPARSE_POINT;
  PReparsePoint = ^TReparsePoint;

  _EA_INFORMATION = record
    EaLength: ULONG;
    EaQueryLength: ULONG;
  end;
  EA_INFORMATION = _EA_INFORMATION;
  PEA_INFORMATION = ^EA_INFORMATION;
  TEaInformation = EA_INFORMATION;
  PEaInformation = ^TEaInformation;

  _EA_ATTRIBUTE = record
    NextEntryOffset: ULONG;
    Flags: UCHAR;
    EaNameLength: UCHAR;
    EaValueLength: USHORT;
    EaName: array[0..0] of AnsiChar;
    // UCHAR EaData[];
  end;
  EA_ATTRIBUTE = _EA_ATTRIBUTE;
  PEA_ATTRIBUTE = ^EA_ATTRIBUTE;
  TEaAttribute = EA_ATTRIBUTE;
  PEaAttribute = ^TEaAttribute;

  _ATTRIBUTE_DEFINITION = record
    AttributeName: array[0..63] of WCHAR;
    AttributeNumber: ULONG;
    Unknown: array[0..1] of ULONG;
    Flags: ULONG;
    MinimumSize: ULONGLONG;
    MaximumSize: ULONGLONG;
  end;
  ATTRIBUTE_DEFINITION = _ATTRIBUTE_DEFINITION;
  PATTRIBUTE_DEFINITION = ^ATTRIBUTE_DEFINITION;
  TAttributeDefinition = ATTRIBUTE_DEFINITION;
  PAttributeDefinition = ^TAttributeDefinition;

  _BOOT_BLOCK = record
    Jump: array[0..2] of UCHAR;
    Format: array[0..7] of UCHAR;
    BytesPerSector: USHORT;
    SectorsPerCluster: UCHAR;
    BootSectors: USHORT;
    Mbz1: UCHAR;
    Mbz2: USHORT;
    Reserved1: USHORT;
    MediaType: UCHAR;
    Mbz3: USHORT;
    SectorsPerTrack: USHORT;
    NumberOfHeads: USHORT;
    PartitionOffset: ULONG;
    Reserved2: array[0..1] of ULONG;
    TotalSectors: ULONGLONG;
    MftStartLcn: ULONGLONG;
    Mft2StartLcn: ULONGLONG;
    ClustersPerFileRecord: ULONG;
    ClustersPerIndexBlock: ULONG;
    VolumeSerialNumber: ULONGLONG;
    Code: array[0..$1AD] of UCHAR;
    BootSignature: USHORT;
  end;
  BOOT_BLOCK = _BOOT_BLOCK;
  PBOOT_BLOCK = ^BOOT_BLOCK;
  TBootBlock = BOOT_BLOCK;
  PBootBlock = ^TBootBlock;

const
  DBG_STATUS_CONTROL_C = 1;
  DBG_STATUS_SYSRQ = 2;
  DBG_STATUS_BUGCHECK_FIRST = 3;
  DBG_STATUS_BUGCHECK_SECOND = 4;
  DBG_STATUS_FATAL = 5;
  DBG_STATUS_DEBUG_CONTROL = 6;

//function DbgPrint(Format: PCH; ...): ULONG; cdecl;
//function DbgPrintReturnControlC(Format: PCH; ...): ULONG; cdecl;

//==============================================================================
// Runtime Library
//==============================================================================

const
  RTL_RANGE_LIST_ADD_IF_CONFLICT = $00000001;
  RTL_RANGE_LIST_ADD_SHARED = $00000002;

const
  RTL_RANGE_LIST_SHARED_OK = $00000001;
  RTL_RANGE_LIST_NULL_CONFLICT_OK = $00000002;

type
  PRTL_CONFLICT_RANGE_CALLBACK = function(Context: PVOID; Range: PRTL_RANGE): ByteBool; stdcall;

{$IFNDEF JWA_INCLUDEMODE}
type
  _OSVERSIONINFOW = record
    dwOSVersionInfoSize: ULONG;
    dwMajorVersion: ULONG;
    dwMinorVersion: ULONG;
    dwBuildNumber: ULONG;
    dwPlatformId: ULONG;
    szCSDVersion: array[0..127] of WCHAR; // Maintenance string for PSS usage
  end;
  OSVERSIONINFOW = _OSVERSIONINFOW;
  POSVERSIONINFOW = ^OSVERSIONINFOW;
  LPOSVERSIONINFOW = ^OSVERSIONINFOW;
  RTL_OSVERSIONINFOW = OSVERSIONINFOW;
{$ENDIF JWA_INCLUDEMODE}
  PRTL_OSVERSIONINFOW = ^OSVERSIONINFOW;

  {$IFNDEF JWA_INCLUDEMODE}
  TOsVersionInfoW = OSVERSIONINFOW;
  //POsVersionInfoW = ^TOsVersionInfoW;

  OSVERSIONINFO = OSVERSIONINFOW;
  POSVERSIONINFO = POSVERSIONINFOW;
  LPOSVERSIONINFO = LPOSVERSIONINFOW;
  {$ENDIF JWA_INCLUDEMODE}

{$IFNDEF JWA_INCLUDEMODE}
const
  VER_PLATFORM_WIN32s = 0;
  VER_PLATFORM_WIN32_WINDOWS = 1;
  VER_PLATFORM_WIN32_NT = 2;
{$ENDIF JWA_INCLUDEMODE}

type
  _RTL_BITMAP = record
    SizeOfBitMap: ULONG; // Number of bits in bit map
    Buffer: PULONG; // Pointer to the bit map itself
  end;
  RTL_BITMAP = _RTL_BITMAP;
  PRTL_BITMAP = ^RTL_BITMAP;
  TRtlBitmap = RTL_BITMAP;
  PRtlBitmap = ^TRtlBitmap;

const
  RTL_REGISTRY_ABSOLUTE = 0; // Path is a full path
  RTL_REGISTRY_SERVICES = 1; // \Registry\Machine\System\CurrentControlSet\Services
  RTL_REGISTRY_CONTROL = 2; // \Registry\Machine\System\CurrentControlSet\Control
  RTL_REGISTRY_WINDOWS_NT = 3; // \Registry\Machine\Software\Microsoft\Windows NT\CurrentVersion
  RTL_REGISTRY_DEVICEMAP = 4; // \Registry\Machine\Hardware\DeviceMap
  RTL_REGISTRY_USER = 5; // \Registry\User\CurrentUser
  RTL_REGISTRY_MAXIMUM = 6;
  RTL_REGISTRY_HANDLE = $40000000; // Low order bits are registry handle
  RTL_REGISTRY_OPTIONAL = $80000000; // Indicates the key node is optional

type
  _TIME_FIELDS = record
    Year: CSHORT; // range [1601...]
    Month: CSHORT; // range [1..12]
    Day: CSHORT; // range [1..31]
    Hour: CSHORT; // range [0..23]
    Minute: CSHORT; // range [0..59]
    Second: CSHORT; // range [0..59]
    Milliseconds: CSHORT; // range [0..999]
    Weekday: CSHORT; // range [0..6] == [Sunday..Saturday]
  end;
  TIME_FIELDS = _TIME_FIELDS;
  PTIME_FIELDS = ^TIME_FIELDS;
  TTimeFields = TIME_FIELDS;
  PTimeFields = ^TTimeFields;

{$IFNDEF JWA_INCLUDEMODE}
type
  _OSVERSIONINFOEXW = record
    dwOSVersionInfoSize: ULONG;
    dwMajorVersion: ULONG;
    dwMinorVersion: ULONG;
    dwBuildNumber: ULONG;
    dwPlatformId: ULONG;
    szCSDVersion: array[0..127] of WCHAR; // Maintenance string for PSS usage
    wServicePackMajor: USHORT;
    wServicePackMinor: USHORT;
    wSuiteMask: USHORT;
    wProductType: UCHAR;
    wReserved: UCHAR;
  end;
  OSVERSIONINFOEXW = _OSVERSIONINFOEXW;
  POSVERSIONINFOEXW = ^OSVERSIONINFOEXW;
  LPOSVERSIONINFOEXW = ^OSVERSIONINFOEXW;
  RTL_OSVERSIONINFOEXW = OSVERSIONINFOEXW;
  PRTL_OSVERSIONINFOEXW = ^OSVERSIONINFOEXW;

  TOsVersionInfoExW = OSVERSIONINFOEXW;
  //POsVersionInfoExW = ^TOsVersionInfoExW;

  OSVERSIONINFOEX = OSVERSIONINFOEXW;
  POSVERSIONINFOEX = POSVERSIONINFOEXW;
  LPOSVERSIONINFOEX = LPOSVERSIONINFOEXW;
{$ENDIF JWA_INCLUDEMODE}

//
// RtlVerifyVersionInfo() conditions
//

const
  {$IFNDEF JWA_INCLUDEMODE}
  VER_EQUAL = 1;
  VER_GREATER = 2;
  VER_GREATER_EQUAL = 3;
  VER_LESS = 4;
  VER_LESS_EQUAL = 5;
  VER_AND = 6;
  VER_OR = 7;

  VER_CONDITION_MASK = 7;
  VER_NUM_BITS_PER_CONDITION_MASK = 3;

//
// RtlVerifyVersionInfo() type mask bits
//

  VER_MINORVERSION = $0000001;
  VER_MAJORVERSION = $0000002;
  VER_BUILDNUMBER = $0000004;
  VER_PLATFORMID = $0000008;
  VER_SERVICEPACKMINOR = $0000010;
  VER_SERVICEPACKMAJOR = $0000020;
  VER_SUITENAME = $0000040;
  VER_PRODUCT_TYPE = $0000080;

//
// RtlVerifyVersionInfo() os product type values
//

  VER_NT_WORKSTATION = $0000001;
  VER_NT_DOMAIN_CONTROLLER = $0000002;
  VER_NT_SERVER = $0000003;
  {$ENDIF JWA_INCLUDEMODE}

//
// Related constant(s) for RtlDetermineDosPathNameType_U()
//
  INVALID_PATH = 0;
  UNC_PATH = 1;
  ABSOLUTE_DRIVE_PATH = 2;
  RELATIVE_DRIVE_PATH = 3;
  ABSOLUTE_PATH = 4;
  RELATIVE_PATH = 5;
  DEVICE_PATH = 6;
  UNC_DOT_PATH = 7;

type
  PRTL_QUERY_REGISTRY_ROUTINE = function(ValueName: PWSTR; ValueType: ULONG;
    ValueData: PVOID; ValueLength: ULONG; Context, EntryContext: PVOID): NTSTATUS; stdcall;

  _RTL_QUERY_REGISTRY_TABLE = record
    QueryRoutine: PRTL_QUERY_REGISTRY_ROUTINE;
    Flags: ULONG;
    Name: PWSTR;
    EntryContext: PVOID;
    DefaultType: ULONG;
    DefaultData: PVOID;
    DefaultLength: ULONG;
  end;
  RTL_QUERY_REGISTRY_TABLE = _RTL_QUERY_REGISTRY_TABLE;
  PRTL_QUERY_REGISTRY_TABLE = ^RTL_QUERY_REGISTRY_TABLE;
  TRtlQueryRegistryTable = RTL_QUERY_REGISTRY_TABLE;
  PRtlQueryRegistryTable = ^TRtlQueryRegistryTable;

  REFGUID = ^GUID;
  TRefGuid = REFGUID;

{$IFNDEF JWA_INCLUDEMODE}
const
  // Should be defined, but isn't
  HEAP_ZERO_MEMORY = $00000008;
{$ENDIF JWA_INCLUDEMODE}

type
// =================================================================
// PROCESS ENVIRONMENT BLOCK (PEB)
// =================================================================

// Verified in XP using WinDbg
  _LDR_DATA_TABLE_ENTRY = record // not packed!
    case Integer of
  (*   *)0: (
  (*000*)InLoadOrderLinks: LIST_ENTRY
        );
  (*   *)1: (
  (*000*)InMemoryOrderLinks: LIST_ENTRY
        );
  (*   *)2: (
  (*000*)InInitializationOrderLinks: LIST_ENTRY;
  (*008*)DllBase: PVOID;
  (*00c*)EntryPoint: PVOID;
  (*010*)SizeOfImage: ULONG;
  (*014*)FullDllName: UNICODE_STRING;
  (*01c*)BaseDllName: UNICODE_STRING;
  (*024*)Flags: ULONG;
  (*028*)LoadCount: USHORT;
  (*02a*)TlsIndex: USHORT;
  (*02c*)HashLinks: LIST_ENTRY;
  (*034*)SectionPointer: PVOID;
  (*038*)CheckSum: ULONG;
  (*03C*)TimeDateStamp: ULONG;
  (*040*)LoadedImports: PVOID;
  (*044*)EntryPointActivationContext: PVOID; // PACTIVATION_CONTEXT
  (*048*)PatchInformation: PVOID;
        )
  end;
  LDR_DATA_TABLE_ENTRY = _LDR_DATA_TABLE_ENTRY;
  PLDR_DATA_TABLE_ENTRY = ^_LDR_DATA_TABLE_ENTRY;
  PPLDR_DATA_TABLE_ENTRY = ^PLDR_DATA_TABLE_ENTRY;
  TLdrDataTableEntry = _LDR_DATA_TABLE_ENTRY;
  PLdrDataTableEntry = ^_LDR_DATA_TABLE_ENTRY;

// Verified in XP using WinDbg
  _PEB_LDR_DATA = record // not packed!
  (*000*)Length: ULONG;
  (*004*)Initialized: BOOLEAN;
  (*008*)SsHandle: PVOID;
  (*00c*)InLoadOrderModuleList: LIST_ENTRY;
  (*014*)InMemoryOrderModuleList: LIST_ENTRY;
  (*01c*)InInitializationOrderModuleList: LIST_ENTRY;
  (*024*)EntryInProgress: PVOID;
  end;
  PEB_LDR_DATA = _PEB_LDR_DATA;
  PPEB_LDR_DATA = ^_PEB_LDR_DATA;
  PPPEB_LDR_DATA = ^PPEB_LDR_DATA;
  TPebLdrData = _PEB_LDR_DATA;
  PPebLdrData = ^_PEB_LDR_DATA;

// Verified in XP using WinDbg
  _RTL_DRIVE_LETTER_CURDIR = record // not packed!
  (*000*)Flags: USHORT;
  (*002*)Length: USHORT;
  (*004*)TimeStamp: ULONG;
  (*008*)DosPath: _STRING;
  end;
  RTL_DRIVE_LETTER_CURDIR = _RTL_DRIVE_LETTER_CURDIR;
  PRTL_DRIVE_LETTER_CURDIR = ^_RTL_DRIVE_LETTER_CURDIR;
  PPRTL_DRIVE_LETTER_CURDIR = ^PRTL_DRIVE_LETTER_CURDIR;
  TRtlDriveLetterCurdir = _RTL_DRIVE_LETTER_CURDIR;
  PRtlDriveLetterCurdir = ^_RTL_DRIVE_LETTER_CURDIR;

  _CURDIR = record // not packed!
  (*000*)DosPath: UNICODE_STRING;
  (*008*)Handle: HANDLE;
  end;
  CURDIR = _CURDIR;
  PCURDIR = ^_CURDIR;
  PPCURDIR = ^PCURDIR;
  TCurdir = _CURDIR;
// PCurdir = ^_CURDIR; // <--- Pascal is case-insensitive

// Verified in XP using WinDbg
  _RTL_USER_PROCESS_PARAMETERS = record // not packed!
  (*000*)MaximumLength: ULONG;
  (*004*)Length: ULONG;
  (*008*)Flags: ULONG; // Bit 0: all pointers normalized
  (*00c*)DebugFlags: ULONG;
  (*010*)ConsoleHandle: HANDLE;
  (*014*)ConsoleFlags: ULONG;
  (*018*)StandardInput: HANDLE;
  (*01c*)StandardOutput: HANDLE;
  (*020*)StandardError: HANDLE;
  (*024*)CurrentDirectory: CURDIR;
  (*030*)DllPath: UNICODE_STRING;
  (*038*)ImagePathName: UNICODE_STRING;
  (*040*)CommandLine: UNICODE_STRING;
  (*048*)Environment: PVOID;
  (*04c*)StartingX: ULONG;
  (*050*)StartingY: ULONG;
  (*054*)CountX: ULONG;
  (*058*)CountY: ULONG;
  (*05c*)CountCharsX: ULONG;
  (*060*)CountCharsY: ULONG;
  (*064*)FillAttribute: ULONG;
  (*068*)WindowFlags: ULONG;
  (*06c*)ShowWindowFlags: ULONG;
  (*070*)WindowTitle: UNICODE_STRING;
  (*078*)DesktopInfo: UNICODE_STRING;
  (*080*)ShellInfo: UNICODE_STRING;
  (*088*)RuntimeData: UNICODE_STRING;
  (*090*)CurrentDirectories: array[0..31] of RTL_DRIVE_LETTER_CURDIR;
  end;
  RTL_USER_PROCESS_PARAMETERS = _RTL_USER_PROCESS_PARAMETERS;
  PRTL_USER_PROCESS_PARAMETERS = ^_RTL_USER_PROCESS_PARAMETERS;
  PPRTL_USER_PROCESS_PARAMETERS = ^PRTL_USER_PROCESS_PARAMETERS;
  TRtlUserProcessParameters = _RTL_USER_PROCESS_PARAMETERS;
  PRtlUserProcessParameters = ^_RTL_USER_PROCESS_PARAMETERS;
  TProcessParameters = _RTL_USER_PROCESS_PARAMETERS;
  PProcessParameters = ^_RTL_USER_PROCESS_PARAMETERS;

  _SYSTEM_STRINGS = record // not packed!
  (*000*)SystemRoot: UNICODE_STRING; // %SystemRoot%
  (*008*)System32Root: UNICODE_STRING; // %SystemRoot%\System32
  (*010*)BaseNamedObjects: UNICODE_STRING; // \BaseNamedObjects
  end;
  SYSTEM_STRINGS = _SYSTEM_STRINGS;
  PSYSTEM_STRINGS = ^_SYSTEM_STRINGS;
  PPSYSTEM_STRINGS = ^PSYSTEM_STRINGS;
  TSystemStrings = _SYSTEM_STRINGS;
  PSystemStrings = ^_SYSTEM_STRINGS;

// Verified in XP using WinDbg
  _TEXT_INFO = record // not packed!
  (*000*)Reserved: PVOID;
  (*004*)SystemStrings: PSYSTEM_STRINGS;
  end;
  TEXT_INFO = _TEXT_INFO;
  PTEXT_INFO = ^_TEXT_INFO;
  PPTEXT_INFO = ^PTEXT_INFO;
  TTextInfo = _TEXT_INFO;
  PTextInfo = ^_TEXT_INFO;

// Verified in XP using WinDbg
  PPEB_FREE_BLOCK = ^_PEB_FREE_BLOCK;
  _PEB_FREE_BLOCK = record // not packed!
  (*000*)Next: PPEB_FREE_BLOCK;
  (*004*)Size: ULONG;
  end;
  PEB_FREE_BLOCK = _PEB_FREE_BLOCK;
  PPPEB_FREE_BLOCK = ^PPEB_FREE_BLOCK;
  TPebFreeBlock = _PEB_FREE_BLOCK;
  PPebFreeBlock = ^_PEB_FREE_BLOCK;

// Verified in W2K, WXP and W2K3 using WinDbg
  _PEB_W2K = packed record // packed!
  (*000*)InheritedAddressSpace: BOOLEAN;
  (*001*)ReadImageFileExecOptions: BOOLEAN;
  (*002*)BeingDebugged: BOOLEAN;
  (*003*)SpareBool: BOOLEAN;
  (*004*)Mutant: PVOID;
  (*008*)ImageBaseAddress: PVOID;
  (*00c*)Ldr: PPEB_LDR_DATA;
  (*010*)ProcessParameters: PRTL_USER_PROCESS_PARAMETERS;
  (*014*)SubSystemData: PVOID;
  (*018*)ProcessHeap: PVOID;
  (*01c*)FastPebLock: PRTL_CRITICAL_SECTION;
  (*020*)FastPebLockRoutine: PVOID; // RtlEnterCriticalSection
  (*024*)FastPebUnlockRoutine: PVOID; // RtlLeaveCriticalSection
  (*028*)EnvironmentUpdateCount: ULONG;
  (*02c*)KernelCallbackTable: PPVOID; // List of callback functions
  (*030*)SystemReserved: array[0..0] of ULONG;
  (*034*)d034: ULONG;
  (*038*)FreeList: PPEB_FREE_BLOCK;
  (*03c*)TlsExpansionCounter: ULONG;
  (*040*)TlsBitmap: PVOID; // ntdll!TlsBitMap of type PRTL_BITMAP
  (*044*)TlsBitmapBits: array[0..1] of ULONG; // 64 bits
  (*04c*)ReadOnlySharedMemoryBase: PVOID;
  (*050*)ReadOnlySharedMemoryHeap: PVOID;
  (*054*)ReadOnlyStaticServerData: PTEXT_INFO;
  (*058*)AnsiCodePageData: PVOID;
  (*05c*)OemCodePageData: PVOID;
  (*060*)UnicodeCaseTableData: PVOID;
  (*064*)NumberOfProcessors: ULONG;
  (*068*)NtGlobalFlag: ULONG;
  (*06C*)Unknown01: ULONG; // Padding or something
  (*070*)CriticalSectionTimeout: LARGE_INTEGER;
  (*078*)HeapSegmentReserve: ULONG;
  (*07c*)HeapSegmentCommit: ULONG;
  (*080*)HeapDeCommitTotalFreeThreshold: ULONG;
  (*084*)HeapDeCommitFreeBlockThreshold: ULONG;
  (*088*)NumberOfHeaps: ULONG;
  (*08c*)MaximumNumberOfHeaps: ULONG;
  (*090*)ProcessHeaps: PPVOID;
  (*094*)GdiSharedHandleTable: PPVOID;
  (*098*)ProcessStarterHelper: PVOID;
  (*09c*)GdiDCAttributeList: ULONG;
  (*0a0*)LoaderLock: PCRITICAL_SECTION;
  (*0a4*)OSMajorVersion: ULONG;
  (*0a8*)OSMinorVersion: ULONG;
  (*0ac*)OSBuildNumber: USHORT;
  (*0ae*)OSCSDVersion: USHORT;
  (*0b0*)OSPlatformId: ULONG;
  (*0b4*)ImageSubsystem: ULONG;
  (*0b8*)ImageSubsystemMajorVersion: ULONG;
  (*0bc*)ImageSubsystemMinorVersion: ULONG;
  (*0c0*)ImageProcessAffinityMask: ULONG;
  (*0c4*)GdiHandleBuffer: array[0..33] of HANDLE;
  (*14c*)PostProcessInitRoutine: PVOID;
  (*150*)TlsExpansionBitmap: PVOID;
  (*154*)TlsExpansionBitmapBits: array[0..31] of ULONG;
  (*1d4*)SessionId: ULONG;
  // Windows 2000
  (*1d8*)AppCompatInfo: PVOID;
  (*1dc*)CSDVersion: UNICODE_STRING;
  end;

// Verified in W2K, WXP and W2K3 using WinDbg
  _PEB_WXP = packed record // packed!
  (*000*)InheritedAddressSpace: BOOLEAN;
  (*001*)ReadImageFileExecOptions: BOOLEAN;
  (*002*)BeingDebugged: BOOLEAN;
  (*003*)SpareBool: BOOLEAN;
  (*004*)Mutant: PVOID;
  (*008*)ImageBaseAddress: PVOID;
  (*00c*)Ldr: PPEB_LDR_DATA;
  (*010*)ProcessParameters: PRTL_USER_PROCESS_PARAMETERS;
  (*014*)SubSystemData: PVOID;
  (*018*)ProcessHeap: PVOID;
  (*01c*)FastPebLock: PRTL_CRITICAL_SECTION;
  (*020*)FastPebLockRoutine: PVOID; // RtlEnterCriticalSection
  (*024*)FastPebUnlockRoutine: PVOID; // RtlLeaveCriticalSection
  (*028*)EnvironmentUpdateCount: ULONG;
  (*02c*)KernelCallbackTable: PPVOID; // List of callback functions
  (*030*)SystemReserved: array[0..0] of ULONG;
  (*034*)AtlThunkSListPtr32: PVOID; // (Windows XP)
  (*038*)FreeList: PPEB_FREE_BLOCK;
  (*03c*)TlsExpansionCounter: ULONG;
  (*040*)TlsBitmap: PVOID; // ntdll!TlsBitMap of type PRTL_BITMAP
  (*044*)TlsBitmapBits: array[0..1] of ULONG; // 64 bits
  (*04c*)ReadOnlySharedMemoryBase: PVOID;
  (*050*)ReadOnlySharedMemoryHeap: PVOID;
  (*054*)ReadOnlyStaticServerData: PTEXT_INFO;
  (*058*)AnsiCodePageData: PVOID;
  (*05c*)OemCodePageData: PVOID;
  (*060*)UnicodeCaseTableData: PVOID;
  (*064*)NumberOfProcessors: ULONG;
  (*068*)NtGlobalFlag: ULONG;
  (*06C*)Unknown01: ULONG; // Padding or something
  (*070*)CriticalSectionTimeout: LARGE_INTEGER;
  (*078*)HeapSegmentReserve: ULONG;
  (*07c*)HeapSegmentCommit: ULONG;
  (*080*)HeapDeCommitTotalFreeThreshold: ULONG;
  (*084*)HeapDeCommitFreeBlockThreshold: ULONG;
  (*088*)NumberOfHeaps: ULONG;
  (*08c*)MaximumNumberOfHeaps: ULONG;
  (*090*)ProcessHeaps: PPVOID;
  (*094*)GdiSharedHandleTable: PPVOID;
  (*098*)ProcessStarterHelper: PVOID;
  (*09c*)GdiDCAttributeList: ULONG;
  (*0a0*)LoaderLock: PCRITICAL_SECTION;
  (*0a4*)OSMajorVersion: ULONG;
  (*0a8*)OSMinorVersion: ULONG;
  (*0ac*)OSBuildNumber: USHORT;
  (*0ae*)OSCSDVersion: USHORT;
  (*0b0*)OSPlatformId: ULONG;
  (*0b4*)ImageSubsystem: ULONG;
  (*0b8*)ImageSubsystemMajorVersion: ULONG;
  (*0bc*)ImageSubsystemMinorVersion: ULONG;
  (*0c0*)ImageProcessAffinityMask: ULONG;
  (*0c4*)GdiHandleBuffer: array[0..33] of HANDLE;
  (*14c*)PostProcessInitRoutine: PVOID;
  (*150*)TlsExpansionBitmap: PVOID;
  (*154*)TlsExpansionBitmapBits: array[0..31] of ULONG;
  (*1d4*)SessionId: ULONG;
  // Windows XP
  (*1d8*)AppCompatFlags: ULARGE_INTEGER;
  (*1e0*)AppCompatFlagsUser: ULARGE_INTEGER;
  (*1e8*)pShimData: PVOID;
  (*1ec*)AppCompatInfo: PVOID;
  (*1f0*)CSDVersion: UNICODE_STRING;
  (*1f8*)ActivationContextData: PVOID; // PACTIVATION_CONTEXT_DATA
  (*1fc*)ProcessAssemblyStorageMap: PVOID; // PASSEMBLY_STORAGE_MAP
  (*200*)SystemDefaultActivationContextData: PVOID; // PACTIVATION_CONTEXT_DATA
  (*204*)SystemAssemblyStorageMap: PVOID; // PASSEMBLY_STORAGE_MAP
  (*208*)MinimumStackCommit: ULONG;
  end;

// Verified in W2K, WXP and W2K3 using WinDbg
  _PEB_2K3 = packed record // packed!
  (*000*)InheritedAddressSpace: BOOLEAN;
  (*001*)ReadImageFileExecOptions: BOOLEAN;
  (*002*)BeingDebugged: BOOLEAN;
  (*003*)SpareBool: BOOLEAN;
  (*004*)Mutant: PVOID;
  (*008*)ImageBaseAddress: PVOID;
  (*00c*)Ldr: PPEB_LDR_DATA;
  (*010*)ProcessParameters: PRTL_USER_PROCESS_PARAMETERS;
  (*014*)SubSystemData: PVOID;
  (*018*)ProcessHeap: PVOID;
  (*01c*)FastPebLock: PRTL_CRITICAL_SECTION;
  (*020*)FastPebLockRoutine: PVOID; // RtlEnterCriticalSection
  (*024*)FastPebUnlockRoutine: PVOID; // RtlLeaveCriticalSection
  (*028*)EnvironmentUpdateCount: ULONG;
  (*02c*)KernelCallbackTable: PPVOID; // List of callback functions
  (*030*)SystemReserved: array[0..0] of ULONG;
  (*034*)ExecuteOptions: ULONG; // 2 Bits used (Windows 2003)
  (*038*)FreeList: PPEB_FREE_BLOCK;
  (*03c*)TlsExpansionCounter: ULONG;
  (*040*)TlsBitmap: PVOID; // ntdll!TlsBitMap of type PRTL_BITMAP
  (*044*)TlsBitmapBits: array[0..1] of ULONG; // 64 bits
  (*04c*)ReadOnlySharedMemoryBase: PVOID;
  (*050*)ReadOnlySharedMemoryHeap: PVOID;
  (*054*)ReadOnlyStaticServerData: PTEXT_INFO;
  (*058*)AnsiCodePageData: PVOID;
  (*05c*)OemCodePageData: PVOID;
  (*060*)UnicodeCaseTableData: PVOID;
  (*064*)NumberOfProcessors: ULONG;
  (*068*)NtGlobalFlag: ULONG;
  (*06C*)Unknown01: ULONG; // Padding or something
  (*070*)CriticalSectionTimeout: LARGE_INTEGER;
  (*078*)HeapSegmentReserve: ULONG;
  (*07c*)HeapSegmentCommit: ULONG;
  (*080*)HeapDeCommitTotalFreeThreshold: ULONG;
  (*084*)HeapDeCommitFreeBlockThreshold: ULONG;
  (*088*)NumberOfHeaps: ULONG;
  (*08c*)MaximumNumberOfHeaps: ULONG;
  (*090*)ProcessHeaps: PPVOID;
  (*094*)GdiSharedHandleTable: PPVOID;
  (*098*)ProcessStarterHelper: PVOID;
  (*09c*)GdiDCAttributeList: ULONG;
  (*0a0*)LoaderLock: PCRITICAL_SECTION;
  (*0a4*)OSMajorVersion: ULONG;
  (*0a8*)OSMinorVersion: ULONG;
  (*0ac*)OSBuildNumber: USHORT;
  (*0ae*)OSCSDVersion: USHORT;
  (*0b0*)OSPlatformId: ULONG;
  (*0b4*)ImageSubsystem: ULONG;
  (*0b8*)ImageSubsystemMajorVersion: ULONG;
  (*0bc*)ImageSubsystemMinorVersion: ULONG;
  (*0c0*)ImageProcessAffinityMask: ULONG;
  (*0c4*)GdiHandleBuffer: array[0..33] of HANDLE;
  (*14c*)PostProcessInitRoutine: PVOID;
  (*150*)TlsExpansionBitmap: PVOID;
  (*154*)TlsExpansionBitmapBits: array[0..31] of ULONG;
  (*1d4*)SessionId: ULONG;
  // Windows XP
  (*1d8*)AppCompatFlags: ULARGE_INTEGER;
  (*1e0*)AppCompatFlagsUser: ULARGE_INTEGER;
  (*1e8*)pShimData: PVOID;
  (*1ec*)AppCompatInfo: PVOID;
  (*1f0*)CSDVersion: UNICODE_STRING;
  (*1f8*)ActivationContextData: PVOID; // PACTIVATION_CONTEXT_DATA
  (*1fc*)ProcessAssemblyStorageMap: PVOID; // PASSEMBLY_STORAGE_MAP
  (*200*)SystemDefaultActivationContextData: PVOID; // PACTIVATION_CONTEXT_DATA
  (*204*)SystemAssemblyStorageMap: PVOID; // PASSEMBLY_STORAGE_MAP
  (*208*)MinimumStackCommit: ULONG;
  // New members in Windows 2003
  (*20c*)FlsCallback: PPVOID;
  (*210*)FlsListHead: LIST_ENTRY;
  (*218*)FlsBitmap: PVOID;
  (*21c*)FlsBitmapBits: array[0..3] of ULONG;
  (*22c*)FlsHighIndex: ULONG;
  end;

  {
  PEB vor Windows VISTA - not confirmed!!

  }
  _PEB_VISTA = record
     InheritedAddressSpace,
     ReadImageFileExecOptions,
     BeingDebugged,
     BitField : UCHAR;

     Bitfields_1: Set of
     (
      ImageUsesLargePages,
      IsProtectedProcess,
      IsLegacyProcess,
      IsImageDynamicallyRelocated,
{$IFDEF DELPHI6_UP}
      SpareBits = al32Bit   //
{$ELSE}
      SpareBit1,
      SpareBit2,
      SpareBit3,
      SpareBit4,
      SpareBit5,
      SpareBit6,
      SpareBit7,
      SpareBit8,
      SpareBit9,
      SpareBit10,
      SpareBit11,
      SpareBit12,
      SpareBit13,
      SpareBit14,
      SpareBit15,
      SpareBit16,
      SpareBit17,
      SpareBit18,
      SpareBit19,
      SpareBit20,
      SpareBit21,
      SpareBit22,
      SpareBit23,
      SpareBit24,
      SpareBit25,
      SpareBit26,
      SpareBit27,
      SpareBit28,
      SpareBit29,
      SpareBit30,
      SpareBit31,
      SpareBit32
{$ENDIF}
     );

     Mutant : PVOID;
     ImageBaseAddress : PVOID;
     Ldr : PPEB_LDR_DATA;
     ProcessParameters : PRTL_USER_PROCESS_PARAMETERS;
     SubSystemData : PVOID;
     ProcessHeap : PVOID;
     FastPebLock : PRTL_CRITICAL_SECTION;
     AtlThunkSListPtr : PVOID;
     IFEOKey : PVOID;
     CrossProcessFlags : ULONG;

     ProcessBits : set of
     (
       ProcessInJob,
       ProcessInitializing
     );

     ReservedBits0 : set of
     (
{$IFDEF DELPHI6_UP}
       Bits_0  = al16bit,
       Bits_1  = al16bit
{$ELSE}
       Bits_0_1,
       Bits_0_2,
       Bits_0_3,
       Bits_0_4,
       Bits_0_5,
       Bits_0_6,
       Bits_0_7,
       Bits_0_8,
       Bits_0_9,
       Bits_0_10,
       Bits_0_11,
       Bits_0_12,
       Bits_0_13,
       Bits_0_14,
       Bits_0_15,
       Bits_0_16,

       Bits_1_1,
       Bits_1_2,
       Bits_1_3,
       Bits_1_4,
       Bits_1_5,
       Bits_1_6,
       Bits_1_7,
       Bits_1_8,
       Bits_1_9,
       Bits_1_10,
       Bits_1_11,
       Bits_1_12,
       Bits_1_13,
       Bits_1_14,
       Bits_1_15,
       Bits_1_16
{$ENDIF}
     );
     {ULONG ProcessInJob: 1;
     ULONG ProcessInitializing: 1;
     ULONG ReservedBits0: 30;
     }
     TableOrUserPtr : PVOID;
    {union
     PVOID KernelCallbackTable;
     PVOID UserSharedInfoPtr;
    }
     SystemReserved : array[0..0] of ULONG;
     SpareUlong : ULONG;
     FreeList : PPEB_FREE_BLOCK;
     TlsExpansionCounter : ULONG;
     TlsBitmap : PVOID;
     TlsBitmapBits : array[0..1] of ULONG;
     ReadOnlySharedMemoryBase : PVOID;
     HotpatchInformation : PVOID;
     ReadOnlyStaticServerData : PPVOID;
     AnsiCodePageData : PVOID;
     OemCodePageData : PVOID;
     UnicodeCaseTableData : PVOID;
     NumberOfProcessors : ULONG;
     NtGlobalFlag :  ULONG;
     CriticalSectionTimeout : LARGE_INTEGER;
     HeapSegmentReserve,
     HeapSegmentCommit,
     HeapDeCommitTotalFreeThreshold,
     HeapDeCommitFreeBlockThreshold,
     NumberOfHeaps,
     MaximumNumberOfHeaps : ULONG;
     ProcessHeaps : PPVOID;
     GdiSharedHandleTable : PVOID;
     ProcessStarterHelper : PVOID;
     GdiDCAttributeList : ULONG;
     LoaderLock : PRTL_CRITICAL_SECTION;
     OSMajorVersion,
     OSMinorVersion : ULONG;
     OSBuildNumber,
     OSCSDVersion : WORD;
     OSPlatformId,
     ImageSubsystem,
     ImageSubsystemMajorVersion,
     ImageSubsystemMinorVersion,
     ImageProcessAffinityMask : ULONG;
     GdiHandleBuffer : array[0..33] of ULONG;
     PostProcessInitRoutine,
     TlsExpansionBitmap : PVOID;
     TlsExpansionBitmapBits : array[0..31] of ULONG;
     SessionId : ULONG;
     AppCompatFlags,
     AppCompatFlagsUser : ULARGE_INTEGER;
     pShimData,
     AppCompatInfo : PVOID;
     CSDVersion : UNICODE_STRING;
     ActivationContextData : Pointer;//*_ACTIVATION_CONTEXT_DATA;
     ProcessAssemblyStorageMap : Pointer; //*_ASSEMBLY_STORAGE_MAP
     SystemDefaultActivationContextData : Pointer; //*_ACTIVATION_CONTEXT_DATA
     SystemAssemblyStorageMap : Pointer; //*_ASSEMBLY_STORAGE_MAP
     MinimumStackCommit : ULONG;
     FlsCallback : Pointer;//*_FLS_CALLBACK_INFO
     FlsListHead : LIST_ENTRY;
     FlsBitmap : PVOID;
     FlsBitmapBits : array[0..3] of ULONG;
     FlsHighIndex : ULONG;
     WerRegistrationData,
     WerShipAssertPtr : PVOID;
  end;
  (*
  ntdll!_PEB  VISTA no SP
   +0x000 InheritedAddressSpace : UChar
   +0x001 ReadImageFileExecOptions : UChar
   +0x002 BeingDebugged    : UChar
   +0x003 BitField         : UChar
   +0x003 ImageUsesLargePages : Pos 0, 1 Bit
   +0x003 IsProtectedProcess : Pos 1, 1 Bit
   +0x003 IsLegacyProcess  : Pos 2, 1 Bit
   +0x003 IsImageDynamicallyRelocated : Pos 3, 1 Bit
   +0x003 SpareBits        : Pos 4, 4 Bits
   +0x004 Mutant           : Ptr32 Void
   +0x008 ImageBaseAddress : Ptr32 Void
   +0x00c Ldr              : Ptr32 _PEB_LDR_DATA
   +0x010 ProcessParameters : Ptr32 _RTL_USER_PROCESS_PARAMETERS
   +0x014 SubSystemData    : Ptr32 Void
   +0x018 ProcessHeap      : Ptr32 Void
   +0x01c FastPebLock      : Ptr32 _RTL_CRITICAL_SECTION
   +0x020 AtlThunkSListPtr : Ptr32 Void
   +0x024 IFEOKey          : Ptr32 Void
   +0x028 CrossProcessFlags : Uint4B
   +0x028 ProcessInJob     : Pos 0, 1 Bit
   +0x028 ProcessInitializing : Pos 1, 1 Bit
   +0x028 ReservedBits0    : Pos 2, 30 Bits
   +0x02c KernelCallbackTable : Ptr32 Void
   +0x02c UserSharedInfoPtr : Ptr32 Void
   +0x030 SystemReserved   : [1] Uint4B
   +0x034 SpareUlong       : Uint4B
   +0x038 FreeList         : Ptr32 _PEB_FREE_BLOCK
   +0x03c TlsExpansionCounter : Uint4B
   +0x040 TlsBitmap        : Ptr32 Void
   +0x044 TlsBitmapBits    : [2] Uint4B
   +0x04c ReadOnlySharedMemoryBase : Ptr32 Void
   +0x050 HotpatchInformation : Ptr32 Void
   +0x054 ReadOnlyStaticServerData : Ptr32 Ptr32 Void
   +0x058 AnsiCodePageData : Ptr32 Void
   +0x05c OemCodePageData  : Ptr32 Void
   +0x060 UnicodeCaseTableData : Ptr32 Void
   +0x064 NumberOfProcessors : Uint4B
   +0x068 NtGlobalFlag     : Uint4B
   +0x070 CriticalSectionTimeout : _LARGE_INTEGER
   +0x078 HeapSegmentReserve : Uint4B
   +0x07c HeapSegmentCommit : Uint4B
   +0x080 HeapDeCommitTotalFreeThreshold : Uint4B
   +0x084 HeapDeCommitFreeBlockThreshold : Uint4B
   +0x088 NumberOfHeaps    : Uint4B
   +0x08c MaximumNumberOfHeaps : Uint4B
   +0x090 ProcessHeaps     : Ptr32 Ptr32 Void
   +0x094 GdiSharedHandleTable : Ptr32 Void
   +0x098 ProcessStarterHelper : Ptr32 Void
   +0x09c GdiDCAttributeList : Uint4B
   +0x0a0 LoaderLock       : Ptr32 _RTL_CRITICAL_SECTION
   +0x0a4 OSMajorVersion   : Uint4B
   +0x0a8 OSMinorVersion   : Uint4B
   +0x0ac OSBuildNumber    : Uint2B
   +0x0ae OSCSDVersion     : Uint2B
   +0x0b0 OSPlatformId     : Uint4B
   +0x0b4 ImageSubsystem   : Uint4B
   +0x0b8 ImageSubsystemMajorVersion : Uint4B
   +0x0bc ImageSubsystemMinorVersion : Uint4B
   +0x0c0 ImageProcessAffinityMask : Uint4B
   +0x0c4 GdiHandleBuffer  : [34] Uint4B
   +0x14c PostProcessInitRoutine : Ptr32     void 
   +0x150 TlsExpansionBitmap : Ptr32 Void
   +0x154 TlsExpansionBitmapBits : [32] Uint4B
   +0x1d4 SessionId        : Uint4B
   +0x1d8 AppCompatFlags   : _ULARGE_INTEGER
   +0x1e0 AppCompatFlagsUser : _ULARGE_INTEGER
   +0x1e8 pShimData        : Ptr32 Void
   +0x1ec AppCompatInfo    : Ptr32 Void
   +0x1f0 CSDVersion       : _UNICODE_STRING
   +0x1f8 ActivationContextData : Ptr32 _ACTIVATION_CONTEXT_DATA
   +0x1fc ProcessAssemblyStorageMap : Ptr32 _ASSEMBLY_STORAGE_MAP
   +0x200 SystemDefaultActivationContextData : Ptr32 _ACTIVATION_CONTEXT_DATA
   +0x204 SystemAssemblyStorageMap : Ptr32 _ASSEMBLY_STORAGE_MAP
   +0x208 MinimumStackCommit : Uint4B
   +0x20c FlsCallback      : Ptr32 _FLS_CALLBACK_INFO
   +0x210 FlsListHead      : _LIST_ENTRY
   +0x218 FlsBitmap        : Ptr32 Void
   +0x21c FlsBitmapBits    : [4] Uint4B
   +0x22c FlsHighIndex     : Uint4B
   +0x230 WerRegistrationData : Ptr32 Void
   +0x234 WerShipAssertPtr : Ptr32 Void
  *)
  
  (* Windows Vista SP1
  ntdll!_PEB
   +0x000 InheritedAddressSpace : UChar
   +0x001 ReadImageFileExecOptions : UChar
   +0x002 BeingDebugged    : UChar
   +0x003 BitField         : UChar
   +0x003 ImageUsesLargePages : Pos 0, 1 Bit
   +0x003 IsProtectedProcess : Pos 1, 1 Bit
   +0x003 IsLegacyProcess  : Pos 2, 1 Bit
   +0x003 IsImageDynamicallyRelocated : Pos 3, 1 Bit
   +0x003 SkipPatchingUser32Forwarders : Pos 4, 1 Bit
   +0x003 SpareBits        : Pos 5, 3 Bits
   +0x004 Mutant           : Ptr32 Void
   +0x008 ImageBaseAddress : Ptr32 Void
   +0x00c Ldr              : Ptr32 _PEB_LDR_DATA
      +0x000 Length           : Uint4B
      +0x004 Initialized      : UChar
      +0x008 SsHandle         : Ptr32 Void
      +0x00c InLoadOrderModuleList : _LIST_ENTRY
         +0x000 Flink            : Ptr32 _LIST_ENTRY
         +0x004 Blink            : Ptr32 _LIST_ENTRY
      +0x014 InMemoryOrderModuleList : _LIST_ENTRY
         +0x000 Flink            : Ptr32 _LIST_ENTRY
         +0x004 Blink            : Ptr32 _LIST_ENTRY
      +0x01c InInitializationOrderModuleList : _LIST_ENTRY
         +0x000 Flink            : Ptr32 _LIST_ENTRY
         +0x004 Blink            : Ptr32 _LIST_ENTRY
      +0x024 EntryInProgress  : Ptr32 Void
      +0x028 ShutdownInProgress : UChar
      +0x02c ShutdownThreadId : Ptr32 Void
   +0x010 ProcessParameters : Ptr32 _RTL_USER_PROCESS_PARAMETERS
      +0x000 MaximumLength    : Uint4B
      +0x004 Length           : Uint4B
      +0x008 Flags            : Uint4B
      +0x00c DebugFlags       : Uint4B
      +0x010 ConsoleHandle    : Ptr32 Void
      +0x014 ConsoleFlags     : Uint4B
      +0x018 StandardInput    : Ptr32 Void
      +0x01c StandardOutput   : Ptr32 Void
      +0x020 StandardError    : Ptr32 Void
      +0x024 CurrentDirectory : _CURDIR
         +0x000 DosPath          : _UNICODE_STRING
         +0x008 Handle           : Ptr32 Void
      +0x030 DllPath          : _UNICODE_STRING
         +0x000 Length           : Uint2B
         +0x002 MaximumLength    : Uint2B
         +0x004 Buffer           : Ptr32 Uint2B
      +0x038 ImagePathName    : _UNICODE_STRING
         +0x000 Length           : Uint2B
         +0x002 MaximumLength    : Uint2B
         +0x004 Buffer           : Ptr32 Uint2B
      +0x040 CommandLine      : _UNICODE_STRING
         +0x000 Length           : Uint2B
         +0x002 MaximumLength    : Uint2B
         +0x004 Buffer           : Ptr32 Uint2B
      +0x048 Environment      : Ptr32 Void
      +0x04c StartingX        : Uint4B
      +0x050 StartingY        : Uint4B
      +0x054 CountX           : Uint4B
      +0x058 CountY           : Uint4B
      +0x05c CountCharsX      : Uint4B
      +0x060 CountCharsY      : Uint4B
      +0x064 FillAttribute    : Uint4B
      +0x068 WindowFlags      : Uint4B
      +0x06c ShowWindowFlags  : Uint4B
      +0x070 WindowTitle      : _UNICODE_STRING
         +0x000 Length           : Uint2B
         +0x002 MaximumLength    : Uint2B
         +0x004 Buffer           : Ptr32 Uint2B
      +0x078 DesktopInfo      : _UNICODE_STRING
         +0x000 Length           : Uint2B
         +0x002 MaximumLength    : Uint2B
         +0x004 Buffer           : Ptr32 Uint2B
      +0x080 ShellInfo        : _UNICODE_STRING
         +0x000 Length           : Uint2B
         +0x002 MaximumLength    : Uint2B
         +0x004 Buffer           : Ptr32 Uint2B
      +0x088 RuntimeData      : _UNICODE_STRING
         +0x000 Length           : Uint2B
         +0x002 MaximumLength    : Uint2B
         +0x004 Buffer           : Ptr32 Uint2B
      +0x090 CurrentDirectores : [32] _RTL_DRIVE_LETTER_CURDIR
         +0x000 Flags            : Uint2B
         +0x002 Length           : Uint2B
         +0x004 TimeStamp        : Uint4B
         +0x008 DosPath          : _STRING
      +0x290 EnvironmentSize  : Uint4B
   +0x014 SubSystemData    : Ptr32 Void
   +0x018 ProcessHeap      : Ptr32 Void
   +0x01c FastPebLock      : Ptr32 _RTL_CRITICAL_SECTION
      +0x000 DebugInfo        : Ptr32 _RTL_CRITICAL_SECTION_DEBUG
         +0x000 Type             : Uint2B
         +0x002 CreatorBackTraceIndex : Uint2B
         +0x004 CriticalSection  : Ptr32 _RTL_CRITICAL_SECTION
         +0x008 ProcessLocksList : _LIST_ENTRY
         +0x010 EntryCount       : Uint4B
         +0x014 ContentionCount  : Uint4B
         +0x018 Flags            : Uint4B
         +0x01c CreatorBackTraceIndexHigh : Uint2B
         +0x01e SpareUSHORT      : Uint2B
      +0x004 LockCount        : Int4B
      +0x008 RecursionCount   : Int4B
      +0x00c OwningThread     : Ptr32 Void
      +0x010 LockSemaphore    : Ptr32 Void
      +0x014 SpinCount        : Uint4B
   +0x020 AtlThunkSListPtr : Ptr32 Void
   +0x024 IFEOKey          : Ptr32 Void
   +0x028 CrossProcessFlags : Uint4B
   +0x028 ProcessInJob     : Pos 0, 1 Bit
   +0x028 ProcessInitializing : Pos 1, 1 Bit
   +0x028 ProcessUsingVEH  : Pos 2, 1 Bit
   +0x028 ProcessUsingVCH  : Pos 3, 1 Bit
   +0x028 ReservedBits0    : Pos 4, 28 Bits
   +0x02c KernelCallbackTable : Ptr32 Void
   +0x02c UserSharedInfoPtr : Ptr32 Void
   +0x030 SystemReserved   : [1] Uint4B
   +0x034 SpareUlong       : Uint4B
   +0x038 SparePebPtr0     : Uint4B
   +0x03c TlsExpansionCounter : Uint4B
   +0x040 TlsBitmap        : Ptr32 Void
   +0x044 TlsBitmapBits    : [2] Uint4B
   +0x04c ReadOnlySharedMemoryBase : Ptr32 Void
   +0x050 HotpatchInformation : Ptr32 Void
   +0x054 ReadOnlyStaticServerData : Ptr32 Ptr32 Void
   +0x058 AnsiCodePageData : Ptr32 Void
   +0x05c OemCodePageData  : Ptr32 Void
   +0x060 UnicodeCaseTableData : Ptr32 Void
   +0x064 NumberOfProcessors : Uint4B
   +0x068 NtGlobalFlag     : Uint4B
   +0x070 CriticalSectionTimeout : _LARGE_INTEGER
      +0x000 LowPart          : Uint4B
      +0x004 HighPart         : Int4B
      +0x000 u                : <unnamed-tag>
         +0x000 LowPart          : Uint4B
         +0x004 HighPart         : Int4B
      +0x000 QuadPart         : Int8B
   +0x078 HeapSegmentReserve : Uint4B
   +0x07c HeapSegmentCommit : Uint4B
   +0x080 HeapDeCommitTotalFreeThreshold : Uint4B
   +0x084 HeapDeCommitFreeBlockThreshold : Uint4B
   +0x088 NumberOfHeaps    : Uint4B
   +0x08c MaximumNumberOfHeaps : Uint4B
   +0x090 ProcessHeaps     : Ptr32 Ptr32 Void
   +0x094 GdiSharedHandleTable : Ptr32 Void
   +0x098 ProcessStarterHelper : Ptr32 Void
   +0x09c GdiDCAttributeList : Uint4B
   +0x0a0 LoaderLock       : Ptr32 _RTL_CRITICAL_SECTION
      +0x000 DebugInfo        : Ptr32 _RTL_CRITICAL_SECTION_DEBUG
         +0x000 Type             : Uint2B
         +0x002 CreatorBackTraceIndex : Uint2B
         +0x004 CriticalSection  : Ptr32 _RTL_CRITICAL_SECTION
         +0x008 ProcessLocksList : _LIST_ENTRY
         +0x010 EntryCount       : Uint4B
         +0x014 ContentionCount  : Uint4B
         +0x018 Flags            : Uint4B
         +0x01c CreatorBackTraceIndexHigh : Uint2B
         +0x01e SpareUSHORT      : Uint2B
      +0x004 LockCount        : Int4B
      +0x008 RecursionCount   : Int4B
      +0x00c OwningThread     : Ptr32 Void
      +0x010 LockSemaphore    : Ptr32 Void
      +0x014 SpinCount        : Uint4B
   +0x0a4 OSMajorVersion   : Uint4B
   +0x0a8 OSMinorVersion   : Uint4B
   +0x0ac OSBuildNumber    : Uint2B
   +0x0ae OSCSDVersion     : Uint2B
   +0x0b0 OSPlatformId     : Uint4B
   +0x0b4 ImageSubsystem   : Uint4B
   +0x0b8 ImageSubsystemMajorVersion : Uint4B
   +0x0bc ImageSubsystemMinorVersion : Uint4B
   +0x0c0 ActiveProcessAffinityMask : Uint4B
   +0x0c4 GdiHandleBuffer  : [34] Uint4B
   +0x14c PostProcessInitRoutine : Ptr32     void 
   +0x150 TlsExpansionBitmap : Ptr32 Void
   +0x154 TlsExpansionBitmapBits : [32] Uint4B
   +0x1d4 SessionId        : Uint4B
   +0x1d8 AppCompatFlags   : _ULARGE_INTEGER
      +0x000 LowPart          : Uint4B
      +0x004 HighPart         : Uint4B
      +0x000 u                : <unnamed-tag>
         +0x000 LowPart          : Uint4B
         +0x004 HighPart         : Uint4B
      +0x000 QuadPart         : Uint8B
   +0x1e0 AppCompatFlagsUser : _ULARGE_INTEGER
      +0x000 LowPart          : Uint4B
      +0x004 HighPart         : Uint4B
      +0x000 u                : <unnamed-tag>
         +0x000 LowPart          : Uint4B
         +0x004 HighPart         : Uint4B
      +0x000 QuadPart         : Uint8B
   +0x1e8 pShimData        : Ptr32 Void
   +0x1ec AppCompatInfo    : Ptr32 Void
   +0x1f0 CSDVersion       : _UNICODE_STRING
      +0x000 Length           : Uint2B
      +0x002 MaximumLength    : Uint2B
      +0x004 Buffer           : Ptr32 Uint2B
   +0x1f8 ActivationContextData : Ptr32 _ACTIVATION_CONTEXT_DATA
   +0x1fc ProcessAssemblyStorageMap : Ptr32 _ASSEMBLY_STORAGE_MAP
   +0x200 SystemDefaultActivationContextData : Ptr32 _ACTIVATION_CONTEXT_DATA
   +0x204 SystemAssemblyStorageMap : Ptr32 _ASSEMBLY_STORAGE_MAP
   +0x208 MinimumStackCommit : Uint4B
   +0x20c FlsCallback      : Ptr32 _FLS_CALLBACK_INFO
   +0x210 FlsListHead      : _LIST_ENTRY
      +0x000 Flink            : Ptr32 _LIST_ENTRY
         +0x000 Flink            : Ptr32 _LIST_ENTRY
         +0x004 Blink            : Ptr32 _LIST_ENTRY
      +0x004 Blink            : Ptr32 _LIST_ENTRY
         +0x000 Flink            : Ptr32 _LIST_ENTRY
         +0x004 Blink            : Ptr32 _LIST_ENTRY
   +0x218 FlsBitmap        : Ptr32 Void
   +0x21c FlsBitmapBits    : [4] Uint4B
   +0x22c FlsHighIndex     : Uint4B
   +0x230 WerRegistrationData : Ptr32 Void
   +0x234 WerShipAssertPtr : Ptr32 Void
  *)
  

  {$IFDEF WINNT4}
  _PEB = _PEB_W2K; // Exact layout for NT4 unknown
  {$ENDIF WINNT4}

  {$IFDEF WIN2000}
  _PEB = _PEB_W2K;
  {$ENDIF WIN2000}

  {$IFDEF WINXP}
  _PEB = _PEB_WXP;
  {$ENDIF WINXP}

  {$IFDEF WIN2003}
  _PEB = _PEB_2K3;
  {$ENDIF WIN2003}

  {$IFDEF WINVISTA}
  _PEB = _PEB_VISTA;
  {$ENDIF WINVISTA}

  {$IFDEF WIN2008}
  _PEB = Pointer;
  {$ENDIF WIN2008}


  PEB = _PEB;
  PPEB = ^_PEB;
  PPPEB = ^PPEB;

{$IFNDEF JWA_INCLUDEMODE}
// =================================================================
// THREAD ENVIRONMENT BLOCK (TEB)
// =================================================================

  PNT_TIB = ^_NT_TIB;
  _NT_TIB = record
    ExceptionList: Pointer; // ^_EXCEPTION_REGISTRATION_RECORD
    StackBase,
      StackLimit,
      SubSystemTib: Pointer;
    case Integer of
      0: (
        FiberData: Pointer
        );
      1: (
        Version: ULONG;
        ArbitraryUserPointer: Pointer;
        Self: PNT_TIB;
        )
  end;
  NT_TIB = _NT_TIB;
{$ENDIF JWA_INCLUDEMODE}
  PPNT_TIB = ^PNT_TIB;

  tagACTCTX = record // not packed!
  (*000*)cbSize: ULONG;
  (*004*)dwFlags: DWORD;
  (*008*)lpSource: LPCWSTR;
  (*00C*)wProcessorArchitecture: USHORT;
  (*00E*)wLangId: LANGID;
  (*010*)lpAssemblyDirectory: LPCTSTR;
  (*014*)lpResourceName: LPCTSTR;
  (*018*)lpApplicationName: LPCTSTR;
  (*01C*)hModule: HMODULE;
  end;
  {$IFNDEF JWA_INCLUDEMODE}
  ACTCTX = tagACTCTX;
  PACTCTX = ^tagACTCTX;
  {$ENDIF JWA_INCLUDEMODE}
  ACTIVATION_CONTEXT = tagACTCTX;
  PACTIVATION_CONTEXT = ^tagACTCTX;
  PPACTIVATION_CONTEXT = ^PACTIVATION_CONTEXT;

  PRTL_ACTIVATION_CONTEXT_STACK_FRAME = ^_RTL_ACTIVATION_CONTEXT_STACK_FRAME;
  _RTL_ACTIVATION_CONTEXT_STACK_FRAME = record // not packed!
  (*000*)Previous: PRTL_ACTIVATION_CONTEXT_STACK_FRAME;
  (*004*)ActivationContext: PACTIVATION_CONTEXT;
  (*008*)Flags: ULONG;
  end;
  RTL_ACTIVATION_CONTEXT_STACK_FRAME = _RTL_ACTIVATION_CONTEXT_STACK_FRAME;
  PPRTL_ACTIVATION_CONTEXT_STACK_FRAME = ^PRTL_ACTIVATION_CONTEXT_STACK_FRAME;

// Verified in XP using WinDbg
  _ACTIVATION_CONTEXT_STACK = record // not packed!
  (*000*)Flags: ULONG;
  (*004*)NextCookieSequenceNumber: ULONG;
  (*008*)ActiveFrame: PRTL_ACTIVATION_CONTEXT_STACK_FRAME;
  (*00c*)FrameListCache: LIST_ENTRY;
  end;
  ACTIVATION_CONTEXT_STACK = _ACTIVATION_CONTEXT_STACK;
  PACTIVATION_CONTEXT_STACK = ^_ACTIVATION_CONTEXT_STACK;
  PPACTIVATION_CONTEXT_STACK = ^PACTIVATION_CONTEXT_STACK;

// Verified in XP using WinDbg
  _GDI_TEB_BATCH = record // not packed!
  (*000*)Offset: ULONG;
  (*004*)HDC: HANDLE;
  (*008*)Buffer: array[0..309] of ULONG;
  end;
  GDI_TEB_BATCH = _GDI_TEB_BATCH;
  PGDI_TEB_BATCH = ^_GDI_TEB_BATCH;
  PPGDI_TEB_BATCH = ^PGDI_TEB_BATCH;

// Verified in XP using WinDbg
  _Wx86ThreadState = packed record // packed!
  (*000*)CallBx86Eip: PULONG;
  (*004*)DeallocationCpu: PVOID;
  (*008*)UseKnownWx86Dll: BOOLEAN;
  (*009*)OleStubInvoked: AnsiChar;
  end;
  Wx86ThreadState = _Wx86ThreadState;
  PWx86ThreadState = ^_Wx86ThreadState;
  PPWx86ThreadState = ^PWx86ThreadState;

// Verified in XP using WinDbg
  _TEB_ACTIVE_FRAME_CONTEXT = record // not packed!
  (*000*)Flags: ULONG;
  (*004*)FrameName: PAnsiChar;
  end;
  TEB_ACTIVE_FRAME_CONTEXT = _TEB_ACTIVE_FRAME_CONTEXT;
  PTEB_ACTIVE_FRAME_CONTEXT = ^_TEB_ACTIVE_FRAME_CONTEXT;
  PPTEB_ACTIVE_FRAME_CONTEXT = ^PTEB_ACTIVE_FRAME_CONTEXT;

// Verified in XP using WinDbg
  PTEB_ACTIVE_FRAME = ^_TEB_ACTIVE_FRAME;
  _TEB_ACTIVE_FRAME = record // not packed!
  (*000*)Flags: ULONG;
  (*004*)Previous: PTEB_ACTIVE_FRAME;
  (*008*)Context: PTEB_ACTIVE_FRAME_CONTEXT;
  end;
  TEB_ACTIVE_FRAME = _TEB_ACTIVE_FRAME;
  PPTEB_ACTIVE_FRAME = ^PTEB_ACTIVE_FRAME;


// Verified in W2K, WXP and W2K3 using WinDbg
  _TEB = record // not packed!
  (*000*)NtTib: NT_TIB;
  (*01c*)EnvironmentPointer: PVOID;
  (*020*)ClientId: CLIENT_ID;
  (*028*)ActiveRpcHandle: PVOID;
  (*02c*)ThreadLocalStoragePointer: PVOID;
  (*030*)Peb: PPEB;
  (*034*)LastErrorValue: ULONG;
  (*038*)CountOfOwnedCriticalSections: ULONG;
  (*03c*)CsrClientThread: PVOID;
  (*040*)Win32ThreadInfo: PVOID;
  (*044*)User32Reserved: array[0..25] of ULONG;
  (*0ac*)UserReserved: array[0..4] of ULONG;
  (*0c0*)WOW32Reserved: PVOID;
  (*0c4*)CurrentLocale: LCID;
  (*0c8*)FpSoftwareStatusRegister: ULONG;
  (*0cc*)SystemReserved1: array[0..53] of PVOID;
  (*1a4*)ExceptionCode: LONG;
  (*1a8*)ActivationContextStack: ACTIVATION_CONTEXT_STACK;
  (*1bc*)SpareBytes1: array[0..23] of UCHAR;
  (*1d4*)GdiTebBatch: GDI_TEB_BATCH;
  (*6b4*)RealClientId: CLIENT_ID;
  (*6bc*)GdiCachedProcessHandle: PVOID;
  (*6c0*)GdiClientPID: ULONG;
  (*6c4*)GdiClientTID: ULONG;
  (*6c8*)GdiThreadLocalInfo: PVOID;
  (*6cc*)Win32ClientInfo: array[0..61] of ULONG;
  (*7c4*)glDispatchTable: array[0..232] of PVOID;
  (*b68*)glReserved1: array[0..28] of ULONG;
  (*bdc*)glReserved2: PVOID;
  (*be0*)glSectionInfo: PVOID;
  (*be4*)glSection: PVOID;
  (*be8*)glTable: PVOID;
  (*bec*)glCurrentRC: PVOID;
  (*bf0*)glContext: PVOID;
  (*bf4*)LastStatusValue: ULONG;
  (*bf8*)StaticUnicodeString: UNICODE_STRING;
  (*c00*)StaticUnicodeBuffer: array[0..MAX_PATH] of WCHAR;
  (*e0a*)Padding: USHORT;
  (*e0c*)DeallocationStack: PVOID;
  (*e10*)TlsSlots: array[0..63] of PVOID;
  (*f10*)TlsLinks: LIST_ENTRY;
  (*f18*)Vdm: PVOID;
  (*f1c*)ReservedForNtRpc: PVOID;
  (*f20*)DbgSsReserved: array[0..1] of PVOID;
    case Integer of
  (*   *)0: (
  (*f28*)HardErrorMode: ULONG // (Windows 2003)
        );
  (*   *)1: (
  (*f28*)HardErrorsAreDisabled: ULONG; // (Windows XP)
  (*f2c*)Instrumentation: array[0..15] of PVOID;
  (*f6c*)WinSockData: PVOID;
  (*f70*)GdiBatchCount: ULONG;
  (*f74*)InDbgPrint: BOOLEAN;
  (*f75*)FreeStackOnTermination: BOOLEAN;
  (*f76*)HasFiberData: BOOLEAN;
  (*f77*)IdealProcessor: BOOLEAN;
  (*f78*)Spare3: ULONG;
  (*f7c*)ReservedForPerf: PVOID;
  (*f80*)ReservedForOle: PVOID;
  (*f84*)WaitingOnLoaderLock: PVOID;
  (*f88*)Wx86Thread: Wx86ThreadState;
  (*f94*)TlsExpansionSlots: PPVOID;
  (*f98*)ImpersonationLocale: LCID;
  (*f9c*)IsImpersonating: ULONG;
  (*fa0*)NlsCache: PVOID;
  (*fa4*)pShimData: PVOID;
  (*fa8*)HeapVirtualAffinity: ULONG;
  (*fac*)CurrentTransactionHandle: PVOID;
  (*fb0*)ActiveFrame: PTEB_ACTIVE_FRAME;
        case Integer of
          0: (
  (*fb4*)SafeThunkCall: BOOLEAN; // Before Windows 2003
  (*fb5*)BooleanSpare: array[0..2] of BOOLEAN // Before Windows 2003
            );
          1: (
  (*fb4*)FlsData: PVOID; // Starting with Windows 2003
            )
          )
  end;
  TEB = _TEB;
  PTEB = ^_TEB;
  PPTEB = ^PTEB;

//from JwaWinternl.pas
type
  _OBJECT_NAME_INFORMATION = record
    Name: UNICODE_STRING;
  end;
  OBJECT_NAME_INFORMATION = _OBJECT_NAME_INFORMATION;
  POBJECT_NAME_INFORMATION = ^OBJECT_NAME_INFORMATION;
  TObjectNameInformation = OBJECT_NAME_INFORMATION;
  PObjectNameInformation = ^OBJECT_NAME_INFORMATION;




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










const
  NtCurrentProcess = HANDLE(-1);
  NtCurrentThread = HANDLE(-2);

// Object Manager specific stuff
  OBJ_NAME_PATH_SEPARATOR = '\';

// Object Manager Object Type Specific Access Rights.
  OBJECT_TYPE_CREATE = $0001;
  OBJECT_TYPE_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or $1;

// Object Manager Directory Specific Access Rights.
  DIRECTORY_QUERY = $0001;
  DIRECTORY_TRAVERSE = $0002;
  DIRECTORY_CREATE_OBJECT = $0004;
  DIRECTORY_CREATE_SUBDIRECTORY = $0008;
  DIRECTORY_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or $F;

// Object Manager Symbolic Link Specific Access Rights.
  SYMBOLIC_LINK_QUERY = $0001;
  SYMBOLIC_LINK_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or $1;

  {$IFNDEF JWA_INCLUDEMODE}
  DUPLICATE_CLOSE_SOURCE = $00000001;
  DUPLICATE_SAME_ACCESS = $00000002;
  DUPLICATE_SAME_ATTRIBUTES = $00000004;
  {$ENDIF JWA_INCLUDEMODE}

//
// Define the access check value for any access
//
//
// The FILE_READ_ACCESS and FILE_WRITE_ACCESS constants are also defined in
// ntioapi.h as FILE_READ_DATA and FILE_WRITE_DATA. The values for these
// constants *MUST* always be in sync.
//
//
// FILE_SPECIAL_ACCESS is checked by the NT I/O system the same as FILE_ANY_ACCESS.
// The file systems, however, may add additional access checks for I/O and FS controls
// that use this value.
//
  {$IFNDEF JWA_INCLUDEMODE}
  FILE_ANY_ACCESS = 0;
  FILE_SPECIAL_ACCESS = FILE_ANY_ACCESS;
  FILE_READ_ACCESS = $0001; // file & pipe
  FILE_WRITE_ACCESS = $0002; // file & pipe

//
// Define share access rights to files and directories
//
  FILE_SHARE_READ = $00000001; // JwaWindows.pas
  FILE_SHARE_WRITE = $00000002; // JwaWindows.pas
  FILE_SHARE_DELETE = $00000004; // JwaWindows.pas
  {$ENDIF JWA_INCLUDEMODE}
  FILE_SHARE_VALID_FLAGS = $00000007;

//
// Define the file attributes values
//
// Note:  = $00000008 is reserved for use for the old DOS VOLID (volume ID)
//        and is therefore not considered valid in NT.
//
// Note:  = $00000010 is reserved for use for the old DOS SUBDIRECTORY flag
//        and is therefore not considered valid in NT.  This flag has
//        been disassociated with file attributes since the other flags are
//        protected with READ_ and WRITE_ATTRIBUTES access to the file.
//
// Note:  Note also that the order of these flags is set to allow both the
//        FAT and the Pinball File Systems to directly set the attributes
//        flags in attributes words without having to pick each flag out
//        individually.  The order of these flags should not be changed!
//
  {$IFNDEF JWA_INCLUDEMODE}
  FILE_ATTRIBUTE_READONLY = $00000001; // JwaWindows.pas
  FILE_ATTRIBUTE_HIDDEN = $00000002; // JwaWindows.pas
  FILE_ATTRIBUTE_SYSTEM = $00000004; // JwaWindows.pas
//OLD DOS VOLID                               = $00000008

  FILE_ATTRIBUTE_DIRECTORY = $00000010; // JwaWindows.pas
  FILE_ATTRIBUTE_ARCHIVE = $00000020; // JwaWindows.pas
  FILE_ATTRIBUTE_DEVICE = $00000040; // JwaWindows.pas
  FILE_ATTRIBUTE_NORMAL = $00000080; // JwaWindows.pas

  FILE_ATTRIBUTE_TEMPORARY = $00000100; // JwaWindows.pas
  FILE_ATTRIBUTE_SPARSE_FILE = $00000200; // JwaWindows.pas
  FILE_ATTRIBUTE_REPARSE_POINT = $00000400; // JwaWindows.pas
  FILE_ATTRIBUTE_COMPRESSED = $00000800; // JwaWindows.pas

  FILE_ATTRIBUTE_OFFLINE = $00001000; // JwaWindows.pas
  FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = $00002000; // JwaWindows.pas
  FILE_ATTRIBUTE_ENCRYPTED = $00004000; // JwaWindows.pas

//
//  This definition is old and will disappear shortly
//

  FILE_ATTRIBUTE_VALID_FLAGS = $00007FB7;
  FILE_ATTRIBUTE_VALID_SET_FLAGS = $000031A7;
  {$ENDIF JWA_INCLUDEMODE}

//
// Define the create disposition values
//

  FILE_SUPERSEDE = $00000000;
  FILE_OPEN = $00000001;
  FILE_CREATE = $00000002;
  FILE_OPEN_IF = $00000003;
  FILE_OVERWRITE = $00000004;
  FILE_OVERWRITE_IF = $00000005;
  FILE_MAXIMUM_DISPOSITION = $00000005;

//
// Define the create/open option flags
//

  FILE_DIRECTORY_FILE = $00000001;
  FILE_WRITE_THROUGH = $00000002;
  FILE_SEQUENTIAL_ONLY = $00000004;
  FILE_NO_INTERMEDIATE_BUFFERING = $00000008;

  FILE_SYNCHRONOUS_IO_ALERT = $00000010;
  FILE_SYNCHRONOUS_IO_NONALERT = $00000020;
  FILE_NON_DIRECTORY_FILE = $00000040;
  FILE_CREATE_TREE_CONNECTION = $00000080;

  FILE_COMPLETE_IF_OPLOCKED = $00000100;
  FILE_NO_EA_KNOWLEDGE = $00000200;
  FILE_OPEN_FOR_RECOVERY = $00000400;
  FILE_RANDOM_ACCESS = $00000800;

  FILE_DELETE_ON_CLOSE = $00001000;
  FILE_OPEN_BY_FILE_ID = $00002000;
  FILE_OPEN_FOR_BACKUP_INTENT = $00004000;
  FILE_NO_COMPRESSION = $00008000;

  FILE_RESERVE_OPFILTER = $00100000;
  FILE_OPEN_REPARSE_POINT = $00200000;
  FILE_OPEN_NO_RECALL = $00400000;
  FILE_OPEN_FOR_FREE_SPACE_QUERY = $00800000;

  FILE_COPY_STRUCTURED_STORAGE = $00000041;
  FILE_STRUCTURED_STORAGE = $00000441;

  FILE_VALID_OPTION_FLAGS = $00FFFFFF;
  FILE_VALID_PIPE_OPTION_FLAGS = $00000032;
  FILE_VALID_MAILSLOT_OPTION_FLAGS = $00000032;
  FILE_VALID_SET_FLAGS = $00000036;

//
// Define the I/O status information return values for NtCreateFile/NtOpenFile
//

  FILE_SUPERSEDED = $00000000;
  FILE_OPENED = $00000001;
  FILE_CREATED = $00000002;
  FILE_OVERWRITTEN = $00000003;
  FILE_EXISTS = $00000004;
  FILE_DOES_NOT_EXIST = $00000005;

//
// Define special ByteOffset parameters for read and write operations
//

  FILE_WRITE_TO_END_OF_FILE = $FFFFFFFF;
  FILE_USE_FILE_POINTER_POSITION = $FFFFFFFE;

//
// Define alignment requirement values
//

  FILE_BYTE_ALIGNMENT = $00000000;
  FILE_WORD_ALIGNMENT = $00000001;
  FILE_LONG_ALIGNMENT = $00000003;
  FILE_QUAD_ALIGNMENT = $00000007;
  FILE_OCTA_ALIGNMENT = $0000000F;
  FILE_32_BYTE_ALIGNMENT = $0000001F;
  FILE_64_BYTE_ALIGNMENT = $0000003F;
  FILE_128_BYTE_ALIGNMENT = $0000007F;
  FILE_256_BYTE_ALIGNMENT = $000000FF;
  FILE_512_BYTE_ALIGNMENT = $000001FF;

//
// Define the maximum length of a filename string
//

  MAXIMUM_FILENAME_LENGTH = 256;

//
// Define the various device characteristics flags
//

  FILE_REMOVABLE_MEDIA = $00000001;
  FILE_READ_ONLY_DEVICE = $00000002;
  FILE_FLOPPY_DISKETTE = $00000004;
  FILE_WRITE_ONCE_MEDIA = $00000008;
  FILE_REMOTE_DEVICE = $00000010;
  FILE_DEVICE_IS_MOUNTED = $00000020;
  FILE_VIRTUAL_VOLUME = $00000040;
  FILE_AUTOGENERATED_DEVICE_NAME = $00000080;
  FILE_DEVICE_SECURE_OPEN = $00000100;

//
// Define kernel debugger print prototypes and macros.
//
// N.B. The following function cannot be directly imported because there are
//      a few places in the source tree where this function is redefined.
//
//procedure DbgBreakPoint(); stdcall;
//procedure DbgUserBreakPoint(); stdcall;
//procedure DbgBreakPointWithStatus(Status: ULONG); stdcall;

//// BEGIN: Reverse function forwarders and custom functions
//// Using Kernel32 function with same functionality for macros and "future version" functions
(* Compatibility: All *)
procedure RtlCopyMemory(
  Destination: PVOID;
  Source: PVOID;
  Length: SIZE_T
  ); stdcall; // Own replacement function

(* XREF: see GetLastError()! *)
(* Compatibility: All *)
// This functions was introduced with Windows XP. The Kernel32 version
// is a function forwarder for this function.
function RtlGetLastWin32Error(): DWORD; external 'kernel32.dll' name 'GetLastError'; // imported as kernel32!GetLastError

(* XREF: see SetLastError()! *)
(* Compatibility: All *)
// This functions was introduced with Windows XP. The Kernel32 version
// is a function forwarder for this function.
procedure RtlSetLastWin32Error(dwErrCode: DWORD); external 'kernel32.dll' name 'SetLastError'; // imported as kernel32!SetLastError

// Own function to retrieve the process's heap handle
(* XREF: see GetProcessHeap()! *)
(* Compatibility: All *)
function NtpGetProcessHeap(): HANDLE;

// Own function to retrieve the thread environment block (TEB) pointer
(* Compatibility: All *)
function NtpCurrentTeb(): PTEB;

// Own function to retrieve the process environment block (PEB) pointer
(* Compatibility: All *)
function RtlpGetCurrentPeb(): PPEB;

// No FASTCALL directive exists in Delphi so we write our own versions ...
// Own function to swap bytes in 16bit values
function RtlUshortByteSwap(Source: USHORT): USHORT;

// Own function to swap bytes in 32bit values
function RtlUlongByteSwap(Source: ULONG): ULONG;

// Own function to swap bytes in 64bit values
function RtlUlonglongByteSwap(Source: ULONGLONG): ULONGLONG;

// Resembles the RtlValidateUnicodeString() function available from Windows XP
// on exactly as it is on this OS version, except for the calling convention.
function RtlpValidateUnicodeString(dwMustBeNull: DWORD; UnicodeString: PUNICODE_STRING): NTSTATUS;

// Resembles the RtlValidateUnicodeString() function available from Windows XP
// on, but does not require the first parameter which anyway must be zero.
function RtlpValidateUnicodeString2(UnicodeString: PUNICODE_STRING): NTSTATUS;

//// END  : Reverse function forwarders and custom functions

//// BEGIN: Function prototypes
// Compatibility: WXP, 2K3
function  CsrGetProcessId(): DWORD; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: WXP, 2K3
function  DbgQueryDebugFilterState(
    ComponentId : ULONG;
    Level : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: WXP, 2K3
function  DbgSetDebugFilterState(
    ComponentId : ULONG;
    Level : ULONG;
    State : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Unknown return value, maybe NTSTATUS?
// Compatibility: NT4, W2K, WXP, 2K3
function  KiRaiseUserExceptionDispatcher(): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  LdrAccessResource(
    hModule : HANDLE;
    ResourceDataEntry : PIMAGE_RESOURCE_DATA_ENTRY;
    Address : PPVOID;
    dwSize : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  LdrAlternateResourcesEnabled(): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  LdrDisableThreadCalloutsForDll(
    hModule : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to GetModuleHandle() from Kernel32.dll.
// Refer to the PSDK for additional information. Usually the same flags
// apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  LdrGetDllHandle(
    pwPath : PWORD;
    pReserved : PVOID;
    pusPath : PUNICODE_STRING;
    var phModule : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to GetProcAddress() from Kernel32.dll.
// Refer to the PSDK for additional information. Usually the same flags
// apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  LdrGetProcedureAddress(
    hModule : HANDLE;
    dwOrdinal : ULONG;
    psName : PSTRING;
    var pProcedure : PVOID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to LoadLibrary() from Kernel32.dll. Refer
// to the PSDK for additional information. Usually the same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  LdrLoadDll(
    pwPath : PWORD;
    pdwFlags : PDWORD;
    pusPath : PUNICODE_STRING;
    var phModule : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  LdrQueryImageFileExecutionOptions(
    pusImagePath : PUNICODE_STRING;
    pwOptionName : PWORD;
    dwRequestedType : DWORD;
    pData : PVOID;
    dwSize : DWORD;
    pdwSize : PDWORD
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  LdrQueryProcessModuleInformation(
    psmi : PSYSTEM_MODULE_INFORMATION;
    dwSize : DWORD;
    pdwSize : PDWORD
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to TerminateProcess() from Kernel32.dll.
// Refer to the PSDK for additional information. Usually the same flags
// apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure LdrShutdownProcess(); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to TerminateThread() from Kernel32.dll.
// Refer to the PSDK for additional information. Usually the same flags
// apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure LdrShutdownThread(); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to FreeLibrary() from Kernel32.dll. Refer
// to the PSDK for additional information. Usually the same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  LdrUnloadDll(
    hModule : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtAcceptConnectPort(
    PortHandle : PHANDLE;
    PortIdentifier : ULONG;
    Message : PPORT_MESSAGE;
    Accept : BOOLEAN;
    WriteSection : PPORT_SECTION_WRITE;
    ReadSection : PPORT_SECTION_READ
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwAcceptConnectPort(PortHandle: PHANDLE; PortIdentifier: ULONG; Message: PPORT_MESSAGE; Accept: BOOLEAN; WriteSection: PPORT_SECTION_WRITE; ReadSection: PPORT_SECTION_READ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;
   {$ENDIF}

// This function is very similar to AccessCheck() from Advapi32.dll. Refer
// to the PSDK for additional information. Usually the same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtAccessCheck(
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    TokenHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    GenericMapping : PGENERIC_MAPPING;
    PrivilegeSet : PPRIVILEGE_SET;
    PrivilegeSetLength : PULONG;
    GrantedAccess : PACCESS_MASK;
    AccessStatus : PBOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwAccessCheck(SecurityDescriptor: PSECURITY_DESCRIPTOR; TokenHandle: HANDLE; DesiredAccess: ACCESS_MASK; GenericMapping: PGENERIC_MAPPING; PrivilegeSet: PPRIVILEGE_SET; PrivilegeSetLength: PULONG; GrantedAccess: PACCESS_MASK;
    AccessStatus: PBOOLEAN): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to AccessCheckAndAuditAlarm() from
// Advapi32.dll. Refer to the PSDK for additional information. Usually the
// same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtAccessCheckAndAuditAlarm(
    SubsystemName : PUNICODE_STRING;
    HandleId : PVOID;
    ObjectTypeName : PUNICODE_STRING;
    ObjectName : PUNICODE_STRING;
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    DesiredAccess : ACCESS_MASK;
    GenericMapping : PGENERIC_MAPPING;
    ObjectCreation : BOOLEAN;
    GrantedAccess : PACCESS_MASK;
    AccessStatus : PBOOLEAN;
    GenerateOnClose : PBOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwAccessCheckAndAuditAlarm(SubsystemName: PUNICODE_STRING; HandleId: PVOID; ObjectTypeName: PUNICODE_STRING; ObjectName: PUNICODE_STRING; SecurityDescriptor: PSECURITY_DESCRIPTOR; DesiredAccess: ACCESS_MASK;
    GenericMapping: PGENERIC_MAPPING; ObjectCreation: BOOLEAN; GrantedAccess: PACCESS_MASK; AccessStatus: PBOOLEAN; GenerateOnClose: PBOOLEAN): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to AccessCheckByType() from Advapi32.dll.
// Refer to the PSDK for additional information. Usually the same flags
// apply.
// Compatibility: W2K, WXP, 2K3
function  NtAccessCheckByType(
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    PrincipalSelfSid : PSID;
    TokenHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectTypeList : POBJECT_TYPE_LIST;
    ObjectTypeListLength : ULONG;
    GenericMapping : PGENERIC_MAPPING;
    PrivilegeSet : PPRIVILEGE_SET;
    PrivilegeSetLength : PULONG;
    GrantedAccess : PACCESS_MASK;
    AccessStatus : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwAccessCheckByType(SecurityDescriptor: PSECURITY_DESCRIPTOR; PrincipalSelfSid: PSID; TokenHandle: HANDLE; DesiredAccess: ACCESS_MASK; ObjectTypeList: POBJECT_TYPE_LIST; ObjectTypeListLength: ULONG;
    GenericMapping: PGENERIC_MAPPING; PrivilegeSet: PPRIVILEGE_SET; PrivilegeSetLength: PULONG; GrantedAccess: PACCESS_MASK; AccessStatus: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to AccessCheckByTypeAndAuditAlarm() from
// Advapi32.dll. Refer to the PSDK for additional information. Usually the
// same flags apply.
// Compatibility: W2K, WXP, 2K3
function  NtAccessCheckByTypeAndAuditAlarm(
    SubsystemName : PUNICODE_STRING;
    HandleId : PVOID;
    ObjectTypeName : PUNICODE_STRING;
    ObjectName : PUNICODE_STRING;
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    PrincipalSelfSid : PSID;
    DesiredAccess : ACCESS_MASK;
    AuditType : AUDIT_EVENT_TYPE;
    Flags : ULONG;
    ObjectTypeList : POBJECT_TYPE_LIST;
    ObjectTypeListLength : ULONG;
    GenericMapping : PGENERIC_MAPPING;
    ObjectCreation : BOOLEAN;
    GrantedAccess : PACCESS_MASK;
    AccessStatus : PULONG;
    GenerateOnClose : PBOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwAccessCheckByTypeAndAuditAlarm(SubsystemName: PUNICODE_STRING; HandleId: PVOID; ObjectTypeName: PUNICODE_STRING; ObjectName: PUNICODE_STRING; SecurityDescriptor: PSECURITY_DESCRIPTOR; PrincipalSelfSid: PSID;
    DesiredAccess: ACCESS_MASK; AuditType: AUDIT_EVENT_TYPE; Flags: ULONG; ObjectTypeList: POBJECT_TYPE_LIST; ObjectTypeListLength: ULONG; GenericMapping: PGENERIC_MAPPING; ObjectCreation: BOOLEAN; GrantedAccess: PACCESS_MASK;
    AccessStatus: PULONG; GenerateOnClose: PBOOLEAN): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to AccessCheckByTypeResultList() from
// Advapi32.dll. Refer to the PSDK for additional information. Usually the
// same flags apply.
// Compatibility: W2K, WXP, 2K3
function  NtAccessCheckByTypeResultList(
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    PrincipalSelfSid : PSID;
    TokenHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectTypeList : POBJECT_TYPE_LIST;
    ObjectTypeListLength : ULONG;
    GenericMapping : PGENERIC_MAPPING;
    PrivilegeSet : PPRIVILEGE_SET;
    PrivilegeSetLength : PULONG;
    GrantedAccessList : PACCESS_MASK;
    AccessStatusList : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwAccessCheckByTypeResultList(SecurityDescriptor: PSECURITY_DESCRIPTOR; PrincipalSelfSid: PSID; TokenHandle: HANDLE; DesiredAccess: ACCESS_MASK; ObjectTypeList: POBJECT_TYPE_LIST; ObjectTypeListLength: ULONG;
    GenericMapping: PGENERIC_MAPPING; PrivilegeSet: PPRIVILEGE_SET; PrivilegeSetLength: PULONG; GrantedAccessList: PACCESS_MASK; AccessStatusList: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to
// AccessCheckByTypeResultListAndAuditAlarm() from Advapi32.dll. Refer to
// the PSDK for additional information. Usually the same flags apply.
// Compatibility: W2K, WXP, 2K3
function  NtAccessCheckByTypeResultListAndAuditAlarm(
    SubsystemName : PUNICODE_STRING;
    HandleId : PVOID;
    ObjectTypeName : PUNICODE_STRING;
    ObjectName : PUNICODE_STRING;
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    PrincipalSelfSid : PSID;
    DesiredAccess : ACCESS_MASK;
    AuditType : AUDIT_EVENT_TYPE;
    Flags : ULONG;
    ObjectTypeList : POBJECT_TYPE_LIST;
    ObjectTypeListLength : ULONG;
    GenericMapping : PGENERIC_MAPPING;
    ObjectCreation : BOOLEAN;
    GrantedAccessList : PACCESS_MASK;
    AccessStatusList : PULONG;
    GenerateOnClose : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwAccessCheckByTypeResultListAndAuditAlarm(SubsystemName: PUNICODE_STRING; HandleId: PVOID; ObjectTypeName: PUNICODE_STRING; ObjectName: PUNICODE_STRING; SecurityDescriptor: PSECURITY_DESCRIPTOR; PrincipalSelfSid: PSID;
    DesiredAccess: ACCESS_MASK; AuditType: AUDIT_EVENT_TYPE; Flags: ULONG; ObjectTypeList: POBJECT_TYPE_LIST; ObjectTypeListLength: ULONG; GenericMapping: PGENERIC_MAPPING; ObjectCreation: BOOLEAN; GrantedAccessList: PACCESS_MASK;
    AccessStatusList: PULONG; GenerateOnClose: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to
// AccessCheckByTypeResultListAndAuditAlarmByHandle() from Advapi32.dll.
// Refer to the PSDK for additional information. Usually the same flags
// apply.
// Compatibility: W2K, WXP, 2K3
function  NtAccessCheckByTypeResultListAndAuditAlarmByHandle(
    SubsystemName : PUNICODE_STRING;
    HandleId : PVOID;
    TokenHandle : HANDLE;
    ObjectTypeName : PUNICODE_STRING;
    ObjectName : PUNICODE_STRING;
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    PrincipalSelfSid : PSID;
    DesiredAccess : ACCESS_MASK;
    AuditType : AUDIT_EVENT_TYPE;
    Flags : ULONG;
    ObjectTypeList : POBJECT_TYPE_LIST;
    ObjectTypeListLength : ULONG;
    GenericMapping : PGENERIC_MAPPING;
    ObjectCreation : BOOLEAN;
    GrantedAccessList : PACCESS_MASK;
    AccessStatusList : PULONG;
    GenerateOnClose : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwAccessCheckByTypeResultListAndAuditAlarmByHandle(SubsystemName: PUNICODE_STRING; HandleId: PVOID; TokenHandle: HANDLE; ObjectTypeName: PUNICODE_STRING; ObjectName: PUNICODE_STRING; SecurityDescriptor: PSECURITY_DESCRIPTOR;
    PrincipalSelfSid: PSID; DesiredAccess: ACCESS_MASK; AuditType: AUDIT_EVENT_TYPE; Flags: ULONG; ObjectTypeList: POBJECT_TYPE_LIST; ObjectTypeListLength: ULONG; GenericMapping: PGENERIC_MAPPING; ObjectCreation: BOOLEAN;
    GrantedAccessList: PACCESS_MASK; AccessStatusList: PULONG; GenerateOnClose: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT4, W2K, WXP, 2K3
function  NtAddAtom(
    Str : PWSTR;
    StringLength : ULONG;
    Atom : PUSHORT
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwAddAtom(Str: PWSTR; StringLength: ULONG; Atom: PUSHORT): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtAdjustGroupsToken(
    TokenHandle : HANDLE;
    ResetToDefault : BOOLEAN;
    NewState : PTOKEN_GROUPS;
    BufferLength : ULONG;
    PreviousState : PTOKEN_GROUPS;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwAdjustGroupsToken(TokenHandle: HANDLE; ResetToDefault: BOOLEAN; NewState: PTOKEN_GROUPS; BufferLength: ULONG; PreviousState: PTOKEN_GROUPS; ReturnLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtAdjustPrivilegesToken(
    TokenHandle : HANDLE;
    DisableAllPrivileges : BOOLEAN;
    NewState : PTOKEN_PRIVILEGES;
    BufferLength : ULONG;
    PreviousState : PTOKEN_PRIVILEGES;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwAdjustPrivilegesToken(TokenHandle: HANDLE; DisableAllPrivileges: BOOLEAN; NewState: PTOKEN_PRIVILEGES; BufferLength: ULONG; PreviousState: PTOKEN_PRIVILEGES; ReturnLength: PULONG): NTSTATUS; stdcall;
    {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtAlertResumeThread(
    ThreadHandle : HANDLE;
    PreviousSuspendCount : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwAlertResumeThread(ThreadHandle: HANDLE; PreviousSuspendCount: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtAlertThread(
    ThreadHandle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwAlertThread(ThreadHandle: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtAllocateLocallyUniqueId(
    Luid : PLUID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwAllocateLocallyUniqueId(Luid: PLUID): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtAllocateUserPhysicalPages(
    ProcessHandle : HANDLE;
    NumberOfPages : PULONG;
    PageFrameNumbers : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwAllocateUserPhysicalPages(ProcessHandle: HANDLE; NumberOfPages: PULONG; PageFrameNumbers: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtAllocateUuids(
    UuidLastTimeAllocated : PLARGE_INTEGER;
    UuidDeltaTime : PULONG;
    UuidSequenceNumber : PULONG;
    UuidSeed : PUCHAR
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwAllocateUuids(UuidLastTimeAllocated: PLARGE_INTEGER; UuidDeltaTime: PULONG; UuidSequenceNumber: PULONG; UuidSeed: PUCHAR): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtAllocateVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PPVOID;
    ZeroBits : ULONG;
    AllocationSize : PULONG;
    AllocationType : ULONG;
    Protect : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwAllocateVirtualMemory(ProcessHandle: HANDLE; BaseAddress: PPVOID; ZeroBits: ULONG; AllocationSize: PULONG; AllocationType: ULONG; Protect: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtAreMappedFilesTheSame(
    Address1 : PVOID;
    Address2 : PVOID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwAreMappedFilesTheSame(Address1: PVOID; Address2: PVOID): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtAssignProcessToJobObject(
    JobHandle : HANDLE;
    ProcessHandle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwAssignProcessToJobObject(JobHandle: HANDLE; ProcessHandle: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtCallbackReturn(
    Result_ : PVOID;
    ResultLength : ULONG;
    Status : NTSTATUS
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCallbackReturn(Result_: PVOID; ResultLength: ULONG; Status: NTSTATUS): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtCancelDeviceWakeupRequest(
    DeviceHandle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCancelDeviceWakeupRequest(DeviceHandle: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtCancelIoFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCancelIoFile(FileHandle: HANDLE; IoStatusBlock: PIO_STATUS_BLOCK): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtCancelTimer(
    TimerHandle : HANDLE;
    PreviousState : PBOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCancelTimer(TimerHandle: HANDLE; PreviousState: PBOOLEAN): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtClearEvent(
    EventHandle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwClearEvent(EventHandle: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to CloseHandle() from Kernel32.dll. Refer
// to the PSDK for additional information. Usually the same flags apply.
// Documented in the DDK as ZwClose().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
{.$IFNDEF JWA_INCLUDEMODE}
function  NtClose(
    Handle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL} external ntdll; {$ENDIF}
{.$ENDIF JWA_INCLUDEMODE}

function  ZwClose(Handle: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtCloseObjectAuditAlarm(
    SubsystemName : PUNICODE_STRING;
    HandleId : PVOID;
    GenerateOnClose : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCloseObjectAuditAlarm(SubsystemName: PUNICODE_STRING; HandleId: PVOID; GenerateOnClose: BOOLEAN): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtCompleteConnectPort(
    PortHandle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCompleteConnectPort(PortHandle: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtConnectPort(
    PortHandle : PHANDLE;
    PortName : PUNICODE_STRING;
    SecurityQos : PSECURITY_QUALITY_OF_SERVICE;
    WriteSection : PPORT_SECTION_WRITE;
    ReadSection : PPORT_SECTION_READ;
    MaxMessageSize : PULONG;
    ConnectData : PVOID;
    ConnectDataLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwConnectPort(PortHandle: PHANDLE; PortName: PUNICODE_STRING; SecurityQos: PSECURITY_QUALITY_OF_SERVICE; WriteSection: PPORT_SECTION_WRITE; ReadSection: PPORT_SECTION_READ; MaxMessageSize: PULONG; ConnectData: PVOID;
    ConnectDataLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtContinue(
    Context : PCONTEXT;
    TestAlert : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwContinue(Context: PCONTEXT; TestAlert: BOOLEAN): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Returns STATUS_NOT_IMPLEMENTED. Only MS knows the intention behind this.
// 
// !!!DO NOT USE!!!
// Compatibility: NT4, W2K
function  NtCreateChannel(
    ChannelHandle : PHANDLE;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCreateChannel(ChannelHandle: PHANDLE; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK as ZwCreateDirectoryObject().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtCreateDirectoryObject(
    DirectoryHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCreateDirectoryObject(DirectoryHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtCreateEvent(
    EventHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    EventType : EVENT_TYPE;
    InitialState : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCreateEvent(EventHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; EventType: EVENT_TYPE; InitialState: BOOLEAN): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtCreateEventPair(
    EventPairHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCreateEventPair(EventPairHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

{.$IFNDEF JWA_INCLUDEMODE}
// Documented in the DDK as ZwCreateFile().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtCreateFile(
    FileHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    IoStatusBlock : PIO_STATUS_BLOCK;
    AllocationSize : PLARGE_INTEGER;
    FileAttributes : ULONG;
    ShareAccess : ULONG;
    CreateDisposition : ULONG;
    CreateOptions : ULONG;
    EaBuffer : PVOID;
    EaLength : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
{.$ENDIF JWA_INCLUDEMODE}
function  ZwCreateFile(FileHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; IoStatusBlock: PIO_STATUS_BLOCK; AllocationSize: PLARGE_INTEGER; FileAttributes: ULONG; ShareAccess: ULONG;
    CreateDisposition: ULONG; CreateOptions: ULONG; EaBuffer: PVOID; EaLength: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtCreateIoCompletion(
    IoCompletionHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    NumberOfConcurrentThreads : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCreateIoCompletion(IoCompletionHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; NumberOfConcurrentThreads: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtCreateJobObject(
    JobHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCreateJobObject(JobHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK as ZwCreateKey().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtCreateKey(
    KeyHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    TitleIndex : ULONG;
    Class_ : PUNICODE_STRING;
    CreateOptions : ULONG;
    Disposition : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCreateKey(KeyHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; TitleIndex: ULONG; Class_: PUNICODE_STRING; CreateOptions: ULONG; Disposition: PULONG): NTSTATUS; stdcall;
    {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtCreateMailslotFile(
    FileHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    IoStatusBlock : PIO_STATUS_BLOCK;
    CreateOptions : ULONG;
    Unknown : ULONG;
    MaxMessageSize : ULONG;
    ReadTimeout : PLARGE_INTEGER
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCreateMailslotFile(FileHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; IoStatusBlock: PIO_STATUS_BLOCK; CreateOptions: ULONG; Unknown: ULONG; MaxMessageSize: ULONG;
    ReadTimeout: PLARGE_INTEGER): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtCreateMutant(
    MutantHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    InitialOwner : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCreateMutant(MutantHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; InitialOwner: BOOLEAN): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtCreateNamedPipeFile(
    FileHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    IoStatusBlock : PIO_STATUS_BLOCK;
    ShareAccess : ULONG;
    CreateDisposition : ULONG;
    CreateOptions : ULONG;
    TypeMessage : BOOLEAN;
    ReadmodeMessage : BOOLEAN;
    Nonblocking : BOOLEAN;
    MaxInstances : ULONG;
    InBufferSize : ULONG;
    OutBufferSize : ULONG;
    DefaultTimeout : PLARGE_INTEGER
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCreateNamedPipeFile(FileHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; IoStatusBlock: PIO_STATUS_BLOCK; ShareAccess: ULONG; CreateDisposition: ULONG; CreateOptions: ULONG;
    TypeMessage: BOOLEAN; ReadmodeMessage: BOOLEAN; Nonblocking: BOOLEAN; MaxInstances: ULONG; InBufferSize: ULONG; OutBufferSize: ULONG; DefaultTimeout: PLARGE_INTEGER): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtCreatePagingFile(
    FileName : PUNICODE_STRING;
    InitialSize : PULARGE_INTEGER;
    MaximumSize : PULARGE_INTEGER;
    Reserved : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCreatePagingFile(FileName: PUNICODE_STRING; InitialSize: PULARGE_INTEGER; MaximumSize: PULARGE_INTEGER; Reserved: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtCreatePort(
    PortHandle : PHANDLE;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    MaxDataSize : ULONG;
    MaxMessageSize : ULONG;
    Reserved : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCreatePort(PortHandle: PHANDLE; ObjectAttributes: POBJECT_ATTRIBUTES; MaxDataSize: ULONG; MaxMessageSize: ULONG; Reserved: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtCreateProcess(
    ProcessHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    InheritFromProcessHandle : HANDLE;
    InheritHandles : BOOLEAN;
    SectionHandle : HANDLE;
    DebugPort : HANDLE;
    ExceptionPort : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCreateProcess(ProcessHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; InheritFromProcessHandle: HANDLE; InheritHandles: BOOLEAN; SectionHandle: HANDLE; DebugPort: HANDLE;
    ExceptionPort: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtCreateProfile(
    ProfileHandle : PHANDLE;
    ProcessHandle : HANDLE;
    Base : PVOID;
    Size : ULONG;
    BucketShift : ULONG;
    Buffer : PULONG;
    BufferLength : ULONG;
    Source : KPROFILE_SOURCE;
    ProcessorMask : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCreateProfile(ProfileHandle: PHANDLE; ProcessHandle: HANDLE; Base: PVOID; Size: ULONG; BucketShift: ULONG; Buffer: PULONG; BufferLength: ULONG; Source: KPROFILE_SOURCE; ProcessorMask: ULONG): NTSTATUS; stdcall;
    {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK as ZwCreateSection().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtCreateSection(
    SectionHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    SectionSize : PLARGE_INTEGER;
    Protect : ULONG;
    Attributes : ULONG;
    FileHandle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCreateSection(SectionHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; SectionSize: PLARGE_INTEGER; Protect: ULONG; Attributes: ULONG; FileHandle: HANDLE): NTSTATUS; stdcall;
    {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtCreateSemaphore(
    SemaphoreHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    InitialCount : LONG;
    MaximumCount : LONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCreateSemaphore(SemaphoreHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; InitialCount: LONG; MaximumCount: LONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtCreateSymbolicLinkObject(
    SymbolicLinkHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    TargetName : PUNICODE_STRING
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCreateSymbolicLinkObject(SymbolicLinkHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; TargetName: PUNICODE_STRING): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtCreateThread(
    ThreadHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    ProcessHandle : HANDLE;
    ClientId : PCLIENT_ID;
    ThreadContext : PCONTEXT;
    UserStack : PUSER_STACK;
    CreateSuspended : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCreateThread(ThreadHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; ProcessHandle: HANDLE; ClientId: PCLIENT_ID; ThreadContext: PCONTEXT; UserStack: PUSER_STACK;
    CreateSuspended: BOOLEAN): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtCreateTimer(
    TimerHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    TimerType : TIMER_TYPE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCreateTimer(TimerHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; TimerType: TIMER_TYPE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtCreateToken(
    TokenHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    Type_ : TOKEN_TYPE;
    AuthenticationId : PLUID;
    ExpirationTime : PLARGE_INTEGER;
    User : PTOKEN_USER;
    Groups : PTOKEN_GROUPS;
    Privileges : PTOKEN_PRIVILEGES;
    Owner : PTOKEN_OWNER;
    PrimaryGroup : PTOKEN_PRIMARY_GROUP;
    DefaultDacl : PTOKEN_DEFAULT_DACL;
    Source : PTOKEN_SOURCE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCreateToken(TokenHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; Type_: TOKEN_TYPE; AuthenticationId: PLUID; ExpirationTime: PLARGE_INTEGER; User: PTOKEN_USER; Groups: PTOKEN_GROUPS;
    Privileges: PTOKEN_PRIVILEGES; Owner: PTOKEN_OWNER; PrimaryGroup: PTOKEN_PRIMARY_GROUP; DefaultDacl: PTOKEN_DEFAULT_DACL; Source: PTOKEN_SOURCE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtCreateWaitablePort(
    PortHandle : PHANDLE;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    MaxDataSize : ULONG;
    MaxMessageSize : ULONG;
    Reserved : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwCreateWaitablePort(PortHandle: PHANDLE; ObjectAttributes: POBJECT_ATTRIBUTES; MaxDataSize: ULONG; MaxMessageSize: ULONG; Reserved: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3


function  NtCurrentTeb(): PTEB; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

function  ZwCurrentTeb(): PTEB; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: WXP, 2K3
function  NtDebugActiveProcess(
    hProcess : HANDLE;
    hDebugObject : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwDebugActiveProcess(hProcess: HANDLE; hDebugObject: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtDelayExecution(
    Alertable : BOOLEAN;
    Interval : PLARGE_INTEGER
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwDelayExecution(Alertable: BOOLEAN; Interval: PLARGE_INTEGER): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT4, W2K, WXP, 2K3
function  NtDeleteAtom(
    Atom : USHORT
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwDeleteAtom(Atom: USHORT): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtDeleteFile(
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwDeleteFile(ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK as ZwDeleteKey().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtDeleteKey(
    KeyHandle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwDeleteKey(KeyHandle: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT4, W2K, WXP, 2K3
function  NtDeleteObjectAuditAlarm(
    SubsystemName : PUNICODE_STRING;
    HandleId : PVOID;
    GenerateOnClose : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwDeleteObjectAuditAlarm(SubsystemName: PUNICODE_STRING; HandleId: PVOID; GenerateOnClose: BOOLEAN): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtDeleteValueKey(
    KeyHandle : HANDLE;
    ValueName : PUNICODE_STRING
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwDeleteValueKey(KeyHandle: HANDLE; ValueName: PUNICODE_STRING): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

{.$IFNDEF JWA_INCLUDEMODE}
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtDeviceIoControlFile(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    IoControlCode : ULONG;
    InputBuffer : PVOID;
    InputBufferLength : ULONG;
    OutputBuffer : PVOID;
    OutputBufferLength : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
{.$ENDIF JWA_INCLUDEMODE}

function  ZwDeviceIoControlFile(FileHandle: HANDLE; Event: HANDLE; ApcRoutine: PIO_APC_ROUTINE; ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; IoControlCode: ULONG; InputBuffer: PVOID; InputBufferLength: ULONG; OutputBuffer: PVOID;
    OutputBufferLength: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtDisplayString(
    Str : PUNICODE_STRING
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwDisplayString(Str: PUNICODE_STRING): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtDuplicateObject(
    SourceProcessHandle : HANDLE;
    SourceHandle : HANDLE;
    TargetProcessHandle : HANDLE;
    TargetHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    Attributes : ULONG;
    Options : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwDuplicateObject(SourceProcessHandle: HANDLE; SourceHandle: HANDLE; TargetProcessHandle: HANDLE; TargetHandle: PHANDLE; DesiredAccess: ACCESS_MASK; Attributes: ULONG; Options: ULONG): NTSTATUS; stdcall;
    {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtDuplicateToken(
    ExistingTokenHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    EffectiveOnly : BOOLEAN;
    TokenType : TOKEN_TYPE;
    NewTokenHandle : PHANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwDuplicateToken(ExistingTokenHandle: HANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; EffectiveOnly: BOOLEAN; TokenType: TOKEN_TYPE; NewTokenHandle: PHANDLE): NTSTATUS; stdcall;
    {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK as ZwEnumerateKey().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtEnumerateKey(
    KeyHandle : HANDLE;
    Index : ULONG;
    KeyInformationClass : KEY_INFORMATION_CLASS;
    KeyInformation : PVOID;
    KeyInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwEnumerateKey(KeyHandle: HANDLE; Index: ULONG; KeyInformationClass: KEY_INFORMATION_CLASS; KeyInformation: PVOID; KeyInformationLength: ULONG; ResultLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK as ZwEnumerateValueKey().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtEnumerateValueKey(
    KeyHandle : HANDLE;
    Index : ULONG;
    KeyValueInformationClass : KEY_VALUE_INFORMATION_CLASS;
    KeyValueInformation : PVOID;
    KeyValueInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwEnumerateValueKey(KeyHandle: HANDLE; Index: ULONG; KeyValueInformationClass: KEY_VALUE_INFORMATION_CLASS; KeyValueInformation: PVOID; KeyValueInformationLength: ULONG; ResultLength: PULONG): NTSTATUS; stdcall;
    {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtExtendSection(
    SectionHandle : HANDLE;
    SectionSize : PLARGE_INTEGER
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwExtendSection(SectionHandle: HANDLE; SectionSize: PLARGE_INTEGER): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtFilterToken(
    ExistingTokenHandle : HANDLE;
    Flags : ULONG;
    SidsToDisable : PTOKEN_GROUPS;
    PrivilegesToDelete : PTOKEN_PRIVILEGES;
    SidsToRestricted : PTOKEN_GROUPS;
    NewTokenHandle : PHANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwFilterToken(ExistingTokenHandle: HANDLE; Flags: ULONG; SidsToDisable: PTOKEN_GROUPS; PrivilegesToDelete: PTOKEN_PRIVILEGES; SidsToRestricted: PTOKEN_GROUPS; NewTokenHandle: PHANDLE): NTSTATUS; stdcall;
    {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT4, W2K, WXP, 2K3
function  NtFindAtom(
    Str : PWSTR;
    StringLength : ULONG;
    Atom : PUSHORT
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwFindAtom(Str: PWSTR; StringLength: ULONG; Atom: PUSHORT): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtFlushBuffersFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwFlushBuffersFile(FileHandle: HANDLE; IoStatusBlock: PIO_STATUS_BLOCK): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtFlushInstructionCache(
    ProcessHandle : HANDLE;
    BaseAddress : PVOID;
    FlushSize : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwFlushInstructionCache(ProcessHandle: HANDLE; BaseAddress: PVOID; FlushSize: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK as ZwFlushKey().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtFlushKey(
    KeyHandle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwFlushKey(KeyHandle: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtFlushVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PPVOID;
    FlushSize : PULONG;
    IoStatusBlock : PIO_STATUS_BLOCK
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwFlushVirtualMemory(ProcessHandle: HANDLE; BaseAddress: PPVOID; FlushSize: PULONG; IoStatusBlock: PIO_STATUS_BLOCK): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtFlushWriteBuffer(): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwFlushWriteBuffer(): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtFreeUserPhysicalPages(
    ProcessHandle : HANDLE;
    NumberOfPages : PULONG;
    PageFrameNumbers : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwFreeUserPhysicalPages(ProcessHandle: HANDLE; NumberOfPages: PULONG; PageFrameNumbers: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtFreeVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PPVOID;
    FreeSize : PULONG;
    FreeType : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwFreeVirtualMemory(ProcessHandle: HANDLE; BaseAddress: PPVOID; FreeSize: PULONG; FreeType: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtFsControlFile(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    FsControlCode : ULONG;
    InputBuffer : PVOID;
    InputBufferLength : ULONG;
    OutputBuffer : PVOID;
    OutputBufferLength : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwFsControlFile(FileHandle: HANDLE; Event: HANDLE; ApcRoutine: PIO_APC_ROUTINE; ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; FsControlCode: ULONG; InputBuffer: PVOID; InputBufferLength: ULONG; OutputBuffer: PVOID;
    OutputBufferLength: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtGetContextThread(
    ThreadHandle : HANDLE;
    Context : PCONTEXT
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwGetContextThread(ThreadHandle: HANDLE; Context: PCONTEXT): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: 2K3
function  NtGetCurrentProcessorNumber(): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwGetCurrentProcessorNumber(): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtGetDevicePowerState(
    DeviceHandle : HANDLE;
    DevicePowerState : PDEVICE_POWER_STATE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwGetDevicePowerState(DeviceHandle: HANDLE; DevicePowerState: PDEVICE_POWER_STATE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtGetPlugPlayEvent(
    Reserved1 : ULONG;
    Reserved2 : ULONG;
    Buffer : PVOID;
    BufferLength : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwGetPlugPlayEvent(Reserved1: ULONG; Reserved2: ULONG; Buffer: PVOID; BufferLength: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, 2K3
function  NtGetTickCount(): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwGetTickCount(): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtGetWriteWatch(
    ProcessHandle : HANDLE;
    Flags : ULONG;
    BaseAddress : PVOID;
    RegionSize : ULONG;
    Buffer : PULONG;
    BufferEntries : PULONG;
    Granularity : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwGetWriteWatch(ProcessHandle: HANDLE; Flags: ULONG; BaseAddress: PVOID; RegionSize: ULONG; Buffer: PULONG; BufferEntries: PULONG; Granularity: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtImpersonateAnonymousToken(
    ThreadHandle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwImpersonateAnonymousToken(ThreadHandle: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtImpersonateClientOfPort(
    PortHandle : HANDLE;
    Message : PPORT_MESSAGE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwImpersonateClientOfPort(PortHandle: HANDLE; Message: PPORT_MESSAGE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtImpersonateThread(
    ThreadHandle : HANDLE;
    TargetThreadHandle : HANDLE;
    SecurityQos : PSECURITY_QUALITY_OF_SERVICE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwImpersonateThread(ThreadHandle: HANDLE; TargetThreadHandle: HANDLE; SecurityQos: PSECURITY_QUALITY_OF_SERVICE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtInitializeRegistry(
    Setup : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwInitializeRegistry(Setup: BOOLEAN): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtInitiatePowerAction(
    SystemAction : POWER_ACTION;
    MinSystemState : SYSTEM_POWER_STATE;
    Flags : ULONG;
    Asynchronous : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwInitiatePowerAction(SystemAction: POWER_ACTION; MinSystemState: SYSTEM_POWER_STATE; Flags: ULONG; Asynchronous: BOOLEAN): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtIsSystemResumeAutomatic(): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwIsSystemResumeAutomatic(): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Returns STATUS_NOT_IMPLEMENTED. Only MS knows the intention behind this.
// 
// !!!DO NOT USE!!!
// Compatibility: NT4, W2K
function  NtListenChannel(
    x : PVOID;
    y : PVOID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwListenChannel(x: PVOID; y: PVOID): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtListenPort(
    PortHandle : HANDLE;
    Message : PPORT_MESSAGE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwListenPort(PortHandle: HANDLE; Message: PPORT_MESSAGE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtLoadDriver(
    DriverServiceName : PUNICODE_STRING
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwLoadDriver(DriverServiceName: PUNICODE_STRING): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Relates to RegLoadKey().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtLoadKey(
    KeyObjectAttributes : POBJECT_ATTRIBUTES;
    FileObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwLoadKey(KeyObjectAttributes: POBJECT_ATTRIBUTES; FileObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Relates to RegLoadKey().
// Compatibility: NT4, W2K, WXP, 2K3
function  NtLoadKey2(
    KeyObjectAttributes : POBJECT_ATTRIBUTES;
    FileObjectAttributes : POBJECT_ATTRIBUTES;
    Flags : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwLoadKey2(KeyObjectAttributes: POBJECT_ATTRIBUTES; FileObjectAttributes: POBJECT_ATTRIBUTES; Flags: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtLockFile(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    LockOffset : PULARGE_INTEGER;
    LockLength : PULARGE_INTEGER;
    Key : ULONG;
    FailImmediately : BOOLEAN;
    ExclusiveLock : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwLockFile(FileHandle: HANDLE; Event: HANDLE; ApcRoutine: PIO_APC_ROUTINE; ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; LockOffset: PULARGE_INTEGER; LockLength: PULARGE_INTEGER; Key: ULONG; FailImmediately: BOOLEAN;
    ExclusiveLock: BOOLEAN): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtLockVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PPVOID;
    LockSize : PULONG;
    LockType : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwLockVirtualMemory(ProcessHandle: HANDLE; BaseAddress: PPVOID; LockSize: PULONG; LockType: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: WXP, 2K3
function  NtMakePermanentObject(
    Handle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwMakePermanentObject(Handle: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK as ZwMakeTemporaryObject().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtMakeTemporaryObject(
    Handle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwMakeTemporaryObject(Handle: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtMapUserPhysicalPages(
    BaseAddress : PVOID;
    NumberOfPages : PULONG;
    PageFrameNumbers : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwMapUserPhysicalPages(BaseAddress: PVOID; NumberOfPages: PULONG; PageFrameNumbers: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtMapUserPhysicalPagesScatter(
    BaseAddresses : PPVOID;
    NumberOfPages : PULONG;
    PageFrameNumbers : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwMapUserPhysicalPagesScatter(BaseAddresses: PPVOID; NumberOfPages: PULONG; PageFrameNumbers: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK as ZwMapViewOfSection().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtMapViewOfSection(
    SectionHandle : HANDLE;
    ProcessHandle : HANDLE;
    BaseAddress : PPVOID;
    ZeroBits : ULONG;
    CommitSize : ULONG;
    SectionOffset : PLARGE_INTEGER;
    ViewSize : PULONG;
    InheritDisposition : SECTION_INHERIT;
    AllocationType : ULONG;
    Protect : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwMapViewOfSection(SectionHandle: HANDLE; ProcessHandle: HANDLE; BaseAddress: PPVOID; ZeroBits: ULONG; CommitSize: ULONG; SectionOffset: PLARGE_INTEGER; ViewSize: PULONG; InheritDisposition: SECTION_INHERIT; AllocationType: ULONG;
    Protect: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtNotifyChangeDirectoryFile(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PFILE_NOTIFY_INFORMATION;
    BufferLength : ULONG;
    NotifyFilter : ULONG;
    WatchSubtree : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwNotifyChangeDirectoryFile(FileHandle: HANDLE; Event: HANDLE; ApcRoutine: PIO_APC_ROUTINE; ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; Buffer: PFILE_NOTIFY_INFORMATION; BufferLength: ULONG; NotifyFilter: ULONG;
    WatchSubtree: BOOLEAN): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtNotifyChangeKey(
    KeyHandle : HANDLE;
    EventHandle : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    NotifyFilter : ULONG;
    WatchSubtree : BOOLEAN;
    Buffer : PVOID;
    BufferLength : ULONG;
    Asynchronous : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwNotifyChangeKey(KeyHandle: HANDLE; EventHandle: HANDLE; ApcRoutine: PIO_APC_ROUTINE; ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; NotifyFilter: ULONG; WatchSubtree: BOOLEAN; Buffer: PVOID; BufferLength: ULONG;
    Asynchronous: BOOLEAN): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtNotifyChangeMultipleKeys(
    KeyHandle : HANDLE;
    Flags : ULONG;
    KeyObjectAttributes : POBJECT_ATTRIBUTES;
    EventHandle : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    NotifyFilter : ULONG;
    WatchSubtree : BOOLEAN;
    Buffer : PVOID;
    BufferLength : ULONG;
    Asynchronous : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwNotifyChangeMultipleKeys(KeyHandle: HANDLE; Flags: ULONG; KeyObjectAttributes: POBJECT_ATTRIBUTES; EventHandle: HANDLE; ApcRoutine: PIO_APC_ROUTINE; ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; NotifyFilter: ULONG;
    WatchSubtree: BOOLEAN; Buffer: PVOID; BufferLength: ULONG; Asynchronous: BOOLEAN): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Returns STATUS_NOT_IMPLEMENTED. Only MS knows the intention behind this.
// 
// !!!DO NOT USE!!!
// Compatibility: NT4, W2K
function  NtOpenChannel(
    ChannelHandle : PHANDLE;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwOpenChannel(ChannelHandle: PHANDLE; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtOpenDirectoryObject(
    DirectoryHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwOpenDirectoryObject(DirectoryHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtOpenEvent(
    EventHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwOpenEvent(EventHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtOpenEventPair(
    EventPairHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwOpenEventPair(EventPairHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

{.$IFNDEF JWA_INCLUDEMODE}
// Documented in the DDK as ZwOpenFile().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtOpenFile(
    FileHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    IoStatusBlock : PIO_STATUS_BLOCK;
    ShareAccess : ULONG;
    OpenOptions : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
{.$ENDIF JWA_INCLUDEMODE}

function  ZwOpenFile(FileHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; IoStatusBlock: PIO_STATUS_BLOCK; ShareAccess: ULONG; OpenOptions: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtOpenIoCompletion(
    IoCompletionHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwOpenIoCompletion(IoCompletionHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtOpenJobObject(
    JobHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwOpenJobObject(JobHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK as ZwOpenKey().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtOpenKey(
    KeyHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwOpenKey(KeyHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtOpenMutant(
    MutantHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwOpenMutant(MutantHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtOpenObjectAuditAlarm(
    SubsystemName : PUNICODE_STRING;
    HandleId : PPVOID;
    ObjectTypeName : PUNICODE_STRING;
    ObjectName : PUNICODE_STRING;
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    TokenHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    GrantedAccess : ACCESS_MASK;
    Privileges : PPRIVILEGE_SET;
    ObjectCreation : BOOLEAN;
    AccessGranted : BOOLEAN;
    GenerateOnClose : PBOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwOpenObjectAuditAlarm(SubsystemName: PUNICODE_STRING; HandleId: PPVOID; ObjectTypeName: PUNICODE_STRING; ObjectName: PUNICODE_STRING; SecurityDescriptor: PSECURITY_DESCRIPTOR; TokenHandle: HANDLE; DesiredAccess: ACCESS_MASK;
    GrantedAccess: ACCESS_MASK; Privileges: PPRIVILEGE_SET; ObjectCreation: BOOLEAN; AccessGranted: BOOLEAN; GenerateOnClose: PBOOLEAN): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtOpenProcess(
    ProcessHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    ClientId : PCLIENT_ID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwOpenProcess(ProcessHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; ClientId: PCLIENT_ID): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtOpenProcessToken(
    ProcessHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    TokenHandle : PHANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwOpenProcessToken(ProcessHandle: HANDLE; DesiredAccess: ACCESS_MASK; TokenHandle: PHANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK as ZwOpenSection().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtOpenSection(
    SectionHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwOpenSection(SectionHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtOpenSemaphore(
    SemaphoreHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwOpenSemaphore(SemaphoreHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK as ZwOpenSymbolicLinkObject().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtOpenSymbolicLinkObject(
    SymbolicLinkHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwOpenSymbolicLinkObject(SymbolicLinkHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtOpenThread(
    ThreadHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    ClientId : PCLIENT_ID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwOpenThread(ThreadHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; ClientId: PCLIENT_ID): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtOpenThreadToken(
    ThreadHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    OpenAsSelf : BOOLEAN;
    TokenHandle : PHANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwOpenThreadToken(ThreadHandle: HANDLE; DesiredAccess: ACCESS_MASK; OpenAsSelf: BOOLEAN; TokenHandle: PHANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtOpenTimer(
    TimerHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwOpenTimer(TimerHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtPlugPlayControl(
    ControlCode : ULONG;
    Buffer : PVOID;
    BufferLength : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwPlugPlayControl(ControlCode: ULONG; Buffer: PVOID; BufferLength: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtPowerInformation(
    PowerInformationLevel : POWER_INFORMATION_LEVEL;
    InputBuffer : PVOID;
    InputBufferLength : ULONG;
    OutputBuffer : PVOID;
    OutputBufferLength : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwPowerInformation(PowerInformationLevel: POWER_INFORMATION_LEVEL; InputBuffer: PVOID; InputBufferLength: ULONG; OutputBuffer: PVOID; OutputBufferLength: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to PrivilegeCheck() from Advapi32.dll.
// Refer to the PSDK for additional information. Usually the same flags
// apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtPrivilegeCheck(
    TokenHandle : HANDLE;
    RequiredPrivileges : PPRIVILEGE_SET;
    Result_ : PBOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwPrivilegeCheck(TokenHandle: HANDLE; RequiredPrivileges: PPRIVILEGE_SET; Result_: PBOOLEAN): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to PrivilegedServiceAuditAlarm() from
// Advapi32.dll. Refer to the PSDK for additional information. Usually the
// same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtPrivilegedServiceAuditAlarm(
    SubsystemName : PUNICODE_STRING;
    ServiceName : PUNICODE_STRING;
    TokenHandle : HANDLE;
    Privileges : PPRIVILEGE_SET;
    AccessGranted : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwPrivilegedServiceAuditAlarm(SubsystemName: PUNICODE_STRING; ServiceName: PUNICODE_STRING; TokenHandle: HANDLE; Privileges: PPRIVILEGE_SET; AccessGranted: BOOLEAN): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtPrivilegeObjectAuditAlarm(
    SubsystemName : PUNICODE_STRING;
    HandleId : PVOID;
    TokenHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    Privileges : PPRIVILEGE_SET;
    AccessGranted : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwPrivilegeObjectAuditAlarm(SubsystemName: PUNICODE_STRING; HandleId: PVOID; TokenHandle: HANDLE; DesiredAccess: ACCESS_MASK; Privileges: PPRIVILEGE_SET; AccessGranted: BOOLEAN): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;
   {$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtProtectVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PPVOID;
    ProtectSize : PULONG;
    NewProtect : ULONG;
    OldProtect : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwProtectVirtualMemory(ProcessHandle: HANDLE; BaseAddress: PPVOID; ProtectSize: PULONG; NewProtect: ULONG; OldProtect: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtPulseEvent(
    EventHandle : HANDLE;
    PreviousState : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwPulseEvent(EventHandle: HANDLE; PreviousState: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQueryAttributesFile(
    ObjectAttributes : POBJECT_ATTRIBUTES;
    FileInformation : PFILE_BASIC_INFORMATION
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryAttributesFile(ObjectAttributes: POBJECT_ATTRIBUTES; FileInformation: PFILE_BASIC_INFORMATION): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQueryDefaultLocale(
    ThreadOrSystem : BOOLEAN;
    Locale : PLCID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryDefaultLocale(ThreadOrSystem: BOOLEAN; Locale: PLCID): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtQueryDefaultUILanguage(
    LanguageId : PLANGID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryDefaultUILanguage(LanguageId: PLANGID): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQueryDirectoryFile(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    FileInformation : PVOID;
    FileInformationLength : ULONG;
    FileInformationClass : FILE_INFORMATION_CLASS;
    ReturnSingleEntry : BOOLEAN;
    FileName : PUNICODE_STRING;
    RestartScan : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryDirectoryFile(FileHandle: HANDLE; Event: HANDLE; ApcRoutine: PIO_APC_ROUTINE; ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; FileInformation: PVOID; FileInformationLength: ULONG;
    FileInformationClass: FILE_INFORMATION_CLASS; ReturnSingleEntry: BOOLEAN; FileName: PUNICODE_STRING; RestartScan: BOOLEAN): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQueryDirectoryObject(
    DirectoryHandle : HANDLE;
    Buffer : PVOID;
    BufferLength : ULONG;
    ReturnSingleEntry : BOOLEAN;
    RestartScan : BOOLEAN;
    Context : PULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryDirectoryObject(DirectoryHandle: HANDLE; Buffer: PVOID; BufferLength: ULONG; ReturnSingleEntry: BOOLEAN; RestartScan: BOOLEAN; Context: PULONG; ReturnLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;
   {$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQueryEaFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PFILE_FULL_EA_INFORMATION;
    BufferLength : ULONG;
    ReturnSingleEntry : BOOLEAN;
    EaList : PFILE_GET_EA_INFORMATION;
    EaListLength : ULONG;
    EaIndex : PULONG;
    RestartScan : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryEaFile(FileHandle: HANDLE; IoStatusBlock: PIO_STATUS_BLOCK; Buffer: PFILE_FULL_EA_INFORMATION; BufferLength: ULONG; ReturnSingleEntry: BOOLEAN; EaList: PFILE_GET_EA_INFORMATION; EaListLength: ULONG; EaIndex: PULONG;
    RestartScan: BOOLEAN): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQueryEvent(
    EventHandle : HANDLE;
    EventInformationClass : EVENT_INFORMATION_CLASS;
    EventInformation : PVOID;
    EventInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryEvent(EventHandle: HANDLE; EventInformationClass: EVENT_INFORMATION_CLASS; EventInformation: PVOID; EventInformationLength: ULONG; ResultLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT4, W2K, WXP, 2K3
function  NtQueryFullAttributesFile(
    ObjectAttributes : POBJECT_ATTRIBUTES;
    FileInformation : PFILE_NETWORK_OPEN_INFORMATION
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryFullAttributesFile(ObjectAttributes: POBJECT_ATTRIBUTES; FileInformation: PFILE_NETWORK_OPEN_INFORMATION): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT4, W2K, WXP, 2K3
function  NtQueryInformationAtom(
    Atom : USHORT;
    AtomInformationClass : ATOM_INFORMATION_CLASS;
    AtomInformation : PVOID;
    AtomInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryInformationAtom(Atom: USHORT; AtomInformationClass: ATOM_INFORMATION_CLASS; AtomInformation: PVOID; AtomInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK as ZwQueryInformationFile().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQueryInformationFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    FileInformation : PVOID;
    FileInformationLength : ULONG;
    FileInformationClass : FILE_INFORMATION_CLASS
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryInformationFile(FileHandle: HANDLE; IoStatusBlock: PIO_STATUS_BLOCK; FileInformation: PVOID; FileInformationLength: ULONG; FileInformationClass: FILE_INFORMATION_CLASS): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;
   {$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtQueryInformationJobObject(
    JobHandle : HANDLE;
    JobInformationClass : JOBOBJECTINFOCLASS;
    JobInformation : PVOID;
    JobInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryInformationJobObject(JobHandle: HANDLE; JobInformationClass: JOBOBJECTINFOCLASS; JobInformation: PVOID; JobInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQueryInformationPort(
    PortHandle : HANDLE;
    PortInformationClass : PORT_INFORMATION_CLASS;
    PortInformation : PVOID;
    PortInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryInformationPort(PortHandle: HANDLE; PortInformationClass: PORT_INFORMATION_CLASS; PortInformation: PVOID; PortInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

{.$IFNDEF JWA_INCLUDEMODE} // drop JwaWinternal
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQueryInformationProcess(
    ProcessHandle : HANDLE;
    ProcessInformationClass : PROCESSINFOCLASS;
    ProcessInformation : PVOID;
    ProcessInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
{.$ENDIF JWA_INCLUDEMODE}

function  ZwQueryInformationProcess(ProcessHandle: HANDLE; ProcessInformationClass: PROCESSINFOCLASS; ProcessInformation: PVOID; ProcessInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
  {$IFNDEF RTDL}external ntdll;{$ENDIF}

{.$IFNDEF JWA_INCLUDEMODE}
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQueryInformationThread(
    ThreadHandle : HANDLE;
    ThreadInformationClass : THREADINFOCLASS;
    ThreadInformation : PVOID;
    ThreadInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
{.$ENDIF JWA_INCLUDEMODE}

function  ZwQueryInformationThread(ThreadHandle: HANDLE; ThreadInformationClass: THREADINFOCLASS; ThreadInformation: PVOID; ThreadInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQueryInformationToken(
    TokenHandle : HANDLE;
    TokenInformationClass : TOKEN_INFORMATION_CLASS;
    TokenInformation : PVOID;
    TokenInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryInformationToken(TokenHandle: HANDLE; TokenInformationClass: TOKEN_INFORMATION_CLASS; TokenInformation: PVOID; TokenInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtQueryInstallUILanguage(
    LanguageId : PLANGID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryInstallUILanguage(LanguageId: PLANGID): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQueryIntervalProfile(
    Source : KPROFILE_SOURCE;
    Interval : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryIntervalProfile(Source: KPROFILE_SOURCE; Interval: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQueryIoCompletion(
    IoCompletionHandle : HANDLE;
    IoCompletionInformationClass : IO_COMPLETION_INFORMATION_CLASS;
    IoCompletionInformation : PVOID;
    IoCompletionInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryIoCompletion(IoCompletionHandle: HANDLE; IoCompletionInformationClass: IO_COMPLETION_INFORMATION_CLASS; IoCompletionInformation: PVOID; IoCompletionInformationLength: ULONG; ResultLength: PULONG): NTSTATUS; stdcall;
    {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK as ZwQueryKey().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQueryKey(
    KeyHandle : HANDLE;
    KeyInformationClass : KEY_INFORMATION_CLASS;
    KeyInformation : PVOID;
    KeyInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryKey(KeyHandle: HANDLE; KeyInformationClass: KEY_INFORMATION_CLASS; KeyInformation: PVOID; KeyInformationLength: ULONG; ResultLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT4, W2K, WXP, 2K3
function  NtQueryMultipleValueKey(
    KeyHandle : HANDLE;
    ValueList : PKEY_VALUE_ENTRY;
    NumberOfValues : ULONG;
    Buffer : PVOID;
    Length : PULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryMultipleValueKey(KeyHandle: HANDLE; ValueList: PKEY_VALUE_ENTRY; NumberOfValues: ULONG; Buffer: PVOID; Length: PULONG; ReturnLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQueryMutant(
    MutantHandle : HANDLE;
    MutantInformationClass : MUTANT_INFORMATION_CLASS;
    MutantInformation : PVOID;
    MutantInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryMutant(MutantHandle: HANDLE; MutantInformationClass: MUTANT_INFORMATION_CLASS; MutantInformation: PVOID; MutantInformationLength: ULONG; ResultLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQueryObject(
    ObjectHandle : HANDLE;
    ObjectInformationClass : OBJECT_INFORMATION_CLASS;
    ObjectInformation : PVOID;
    ObjectInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryObject(ObjectHandle: HANDLE; ObjectInformationClass: OBJECT_INFORMATION_CLASS; ObjectInformation: PVOID; ObjectInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtQueryOpenSubKeys(
    KeyObjectAttributes : POBJECT_ATTRIBUTES;
    NumberOfKey : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryOpenSubKeys(KeyObjectAttributes: POBJECT_ATTRIBUTES; NumberOfKey: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQueryPerformanceCounter(
    PerformanceCount : PLARGE_INTEGER;
    PerformanceFrequency : PLARGE_INTEGER
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryPerformanceCounter(PerformanceCount: PLARGE_INTEGER; PerformanceFrequency: PLARGE_INTEGER): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: WXP, 2K3
function  NtQueryPortInformationProcess(): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryPortInformationProcess(): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtQueryQuotaInformationFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PFILE_USER_QUOTA_INFORMATION;
    BufferLength : ULONG;
    ReturnSingleEntry : BOOLEAN;
    QuotaList : PFILE_QUOTA_LIST_INFORMATION;
    QuotaListLength : ULONG;
    ResumeSid : PSID;
    RestartScan : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryQuotaInformationFile(FileHandle: HANDLE; IoStatusBlock: PIO_STATUS_BLOCK; Buffer: PFILE_USER_QUOTA_INFORMATION; BufferLength: ULONG; ReturnSingleEntry: BOOLEAN; QuotaList: PFILE_QUOTA_LIST_INFORMATION;
    QuotaListLength: ULONG; ResumeSid: PSID; RestartScan: BOOLEAN): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQuerySection(
    SectionHandle : HANDLE;
    SectionInformationClass : SECTION_INFORMATION_CLASS;
    SectionInformation : PVOID;
    SectionInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQuerySection(SectionHandle: HANDLE; SectionInformationClass: SECTION_INFORMATION_CLASS; SectionInformation: PVOID; SectionInformationLength: ULONG; ResultLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQuerySecurityObject(
    Handle : HANDLE;
    RequestedInformation : SECURITY_INFORMATION;
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    SecurityDescriptorLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQuerySecurityObject(Handle: HANDLE; RequestedInformation: SECURITY_INFORMATION; SecurityDescriptor: PSECURITY_DESCRIPTOR; SecurityDescriptorLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;
   {$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQuerySemaphore(
    SemaphoreHandle : HANDLE;
    SemaphoreInformationClass : SEMAPHORE_INFORMATION_CLASS;
    SemaphoreInformation : PVOID;
    SemaphoreInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQuerySemaphore(SemaphoreHandle: HANDLE; SemaphoreInformationClass: SEMAPHORE_INFORMATION_CLASS; SemaphoreInformation: PVOID; SemaphoreInformationLength: ULONG; ResultLength: PULONG): NTSTATUS; stdcall;
    {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK as ZwQuerySymbolicLinkObject().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQuerySymbolicLinkObject(
    SymbolicLinkHandle : HANDLE;
    TargetName : PUNICODE_STRING;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQuerySymbolicLinkObject(SymbolicLinkHandle: HANDLE; TargetName: PUNICODE_STRING; ReturnLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQuerySystemEnvironmentValue(
    Name : PUNICODE_STRING;
    Value : PVOID;
    ValueLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQuerySystemEnvironmentValue(Name: PUNICODE_STRING; Value: PVOID; ValueLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

{.$IFNDEF JWA_INCLUDEMODE} //do include since we dropped JwaWinternl 
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQuerySystemInformation(
    SystemInformationClass : SYSTEM_INFORMATION_CLASS;
    SystemInformation : PVOID;
    SystemInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
{.$ENDIF JWA_INCLUDEMODE}

function  ZwQuerySystemInformation(SystemInformationClass: SYSTEM_INFORMATION_CLASS; SystemInformation: PVOID; SystemInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
{.$IFNDEF JWA_INCLUDEMODE}
function  NtQuerySystemTime(
    CurrentTime : PLARGE_INTEGER
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
{.$ENDIF JWA_INCLUDEMODE}

function  ZwQuerySystemTime(CurrentTime: PLARGE_INTEGER): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQueryTimer(
    TimerHandle : HANDLE;
    TimerInformationClass : TIMER_INFORMATION_CLASS;
    TimerInformation : PVOID;
    TimerInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryTimer(TimerHandle: HANDLE; TimerInformationClass: TIMER_INFORMATION_CLASS; TimerInformation: PVOID; TimerInformationLength: ULONG; ResultLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQueryTimerResolution(
    CoarsestResolution : PULONG;
    FinestResolution : PULONG;
    ActualResolution : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryTimerResolution(CoarsestResolution: PULONG; FinestResolution: PULONG; ActualResolution: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK as ZwQueryValueKey().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQueryValueKey(
    KeyHandle : HANDLE;
    ValueName : PUNICODE_STRING;
    KeyValueInformationClass : KEY_VALUE_INFORMATION_CLASS;
    KeyValueInformation : PVOID;
    KeyValueInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryValueKey(KeyHandle: HANDLE; ValueName: PUNICODE_STRING; KeyValueInformationClass: KEY_VALUE_INFORMATION_CLASS; KeyValueInformation: PVOID; KeyValueInformationLength: ULONG; ResultLength: PULONG): NTSTATUS; stdcall;
    {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQueryVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PVOID;
    MemoryInformationClass : MEMORY_INFORMATION_CLASS;
    MemoryInformation : PVOID;
    MemoryInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryVirtualMemory(ProcessHandle: HANDLE; BaseAddress: PVOID; MemoryInformationClass: MEMORY_INFORMATION_CLASS; MemoryInformation: PVOID; MemoryInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
    {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtQueryVolumeInformationFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    VolumeInformation : PVOID;
    VolumeInformationLength : ULONG;
    VolumeInformationClass : FS_INFORMATION_CLASS
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueryVolumeInformationFile(FileHandle: HANDLE; IoStatusBlock: PIO_STATUS_BLOCK; VolumeInformation: PVOID; VolumeInformationLength: ULONG; VolumeInformationClass: FS_INFORMATION_CLASS): NTSTATUS; stdcall;
    {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT4, W2K, WXP, 2K3
function  NtQueueApcThread(
    ThreadHandle : HANDLE;
    ApcRoutine : PKNORMAL_ROUTINE;
    ApcContext : PVOID;
    Argument1 : PVOID;
    Argument2 : PVOID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwQueueApcThread(ThreadHandle: HANDLE; ApcRoutine: PKNORMAL_ROUTINE; ApcContext: PVOID; Argument1: PVOID; Argument2: PVOID): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtRaiseException(
    ExceptionRecord : PEXCEPTION_RECORD;
    Context : PCONTEXT;
    SearchFrames : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwRaiseException(ExceptionRecord: PEXCEPTION_RECORD; Context: PCONTEXT; SearchFrames: BOOLEAN): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtRaiseHardError(
    Status : NTSTATUS;
    NumberOfArguments : ULONG;
    StringArgumentsMask : ULONG;
    Arguments : PULONG;
    MessageBoxType : ULONG;
    MessageBoxResult : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwRaiseHardError(Status: NTSTATUS; NumberOfArguments: ULONG; StringArgumentsMask: ULONG; Arguments: PULONG; MessageBoxType: ULONG; MessageBoxResult: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK as ZwReadFile().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtReadFile(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PVOID;
    Length : ULONG;
    ByteOffset : PLARGE_INTEGER;
    Key : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwReadFile(FileHandle: HANDLE; Event: HANDLE; ApcRoutine: PIO_APC_ROUTINE; ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; Buffer: PVOID; Length: ULONG; ByteOffset: PLARGE_INTEGER; Key: PULONG): NTSTATUS; stdcall;
    {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT4, W2K, WXP, 2K3
function  NtReadFileScatter(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PFILE_SEGMENT_ELEMENT;
    Length : ULONG;
    ByteOffset : PLARGE_INTEGER;
    Key : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwReadFileScatter(FileHandle: HANDLE; Event: HANDLE; ApcRoutine: PIO_APC_ROUTINE; ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; Buffer: PFILE_SEGMENT_ELEMENT; Length: ULONG; ByteOffset: PLARGE_INTEGER;
    Key: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtReadRequestData(
    PortHandle : HANDLE;
    Message : PPORT_MESSAGE;
    Index : ULONG;
    Buffer : PVOID;
    BufferLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwReadRequestData(PortHandle: HANDLE; Message: PPORT_MESSAGE; Index: ULONG; Buffer: PVOID; BufferLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtReadVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PVOID;
    Buffer : PVOID;
    BufferLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwReadVirtualMemory(ProcessHandle: HANDLE; BaseAddress: PVOID; Buffer: PVOID; BufferLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtRegisterThreadTerminatePort(
    PortHandle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwRegisterThreadTerminatePort(PortHandle: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtReleaseMutant(
    MutantHandle : HANDLE;
    PreviousState : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwReleaseMutant(MutantHandle: HANDLE; PreviousState: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtReleaseSemaphore(
    SemaphoreHandle : HANDLE;
    ReleaseCount : LONG;
    PreviousCount : PLONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwReleaseSemaphore(SemaphoreHandle: HANDLE; ReleaseCount: LONG; PreviousCount: PLONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtRemoveIoCompletion(
    IoCompletionHandle : HANDLE;
    CompletionKey : PULONG;
    CompletionValue : PULONG;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Timeout : PLARGE_INTEGER
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwRemoveIoCompletion(IoCompletionHandle: HANDLE; CompletionKey: PULONG; CompletionValue: PULONG; IoStatusBlock: PIO_STATUS_BLOCK; Timeout: PLARGE_INTEGER): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: WXP, 2K3
function  NtRemoveProcessDebug(
    hProcess : HANDLE;
    hDebugObject : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwRemoveProcessDebug(hProcess: HANDLE; hDebugObject: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtReplaceKey(
    NewFileObjectAttributes : POBJECT_ATTRIBUTES;
    KeyHandle : HANDLE;
    OldFileObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwReplaceKey(NewFileObjectAttributes: POBJECT_ATTRIBUTES; KeyHandle: HANDLE; OldFileObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtReplyPort(
    PortHandle : HANDLE;
    ReplyMessage : PPORT_MESSAGE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwReplyPort(PortHandle: HANDLE; ReplyMessage: PPORT_MESSAGE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtReplyWaitReceivePort(
    PortHandle : HANDLE;
    PortIdentifier : PULONG;
    ReplyMessage : PPORT_MESSAGE;
    Message : PPORT_MESSAGE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwReplyWaitReceivePort(PortHandle: HANDLE; PortIdentifier: PULONG; ReplyMessage: PPORT_MESSAGE; Message: PPORT_MESSAGE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtReplyWaitReceivePortEx(
    PortHandle : HANDLE;
    PortIdentifier : PULONG;
    ReplyMessage : PPORT_MESSAGE;
    Message : PPORT_MESSAGE;
    Timeout : PLARGE_INTEGER
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwReplyWaitReceivePortEx(PortHandle: HANDLE; PortIdentifier: PULONG; ReplyMessage: PPORT_MESSAGE; Message: PPORT_MESSAGE; Timeout: PLARGE_INTEGER): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtReplyWaitReplyPort(
    PortHandle : HANDLE;
    ReplyMessage : PPORT_MESSAGE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwReplyWaitReplyPort(PortHandle: HANDLE; ReplyMessage: PPORT_MESSAGE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Returns STATUS_NOT_IMPLEMENTED. Only MS knows the intention behind this.
// 
// !!!DO NOT USE!!!
// Compatibility: NT4, W2K
function  NtReplyWaitSendChannel(
    x : PVOID;
    y : PVOID;
    z : PVOID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwReplyWaitSendChannel(x: PVOID; y: PVOID; z: PVOID): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtRequestDeviceWakeup(
    DeviceHandle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwRequestDeviceWakeup(DeviceHandle: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtRequestPort(
    PortHandle : HANDLE;
    RequestMessage : PPORT_MESSAGE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwRequestPort(PortHandle: HANDLE; RequestMessage: PPORT_MESSAGE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtRequestWaitReplyPort(
    PortHandle : HANDLE;
    RequestMessage : PPORT_MESSAGE;
    ReplyMessage : PPORT_MESSAGE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwRequestWaitReplyPort(PortHandle: HANDLE; RequestMessage: PPORT_MESSAGE; ReplyMessage: PPORT_MESSAGE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtRequestWakeupLatency(
    Latency : LATENCY_TIME
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwRequestWakeupLatency(Latency: LATENCY_TIME): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtResetEvent(
    EventHandle : HANDLE;
    PreviousState : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwResetEvent(EventHandle: HANDLE; PreviousState: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtResetWriteWatch(
    ProcessHandle : HANDLE;
    BaseAddress : PVOID;
    RegionSize : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwResetWriteWatch(ProcessHandle: HANDLE; BaseAddress: PVOID; RegionSize: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtRestoreKey(
    KeyHandle : HANDLE;
    FileHandle : HANDLE;
    Flags : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwRestoreKey(KeyHandle: HANDLE; FileHandle: HANDLE; Flags: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: WXP, 2K3
function  NtResumeProcess(
    hProcess : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwResumeProcess(hProcess: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to ResumeThread() from Kernel32.dll. Refer
// to the PSDK for additional information. Usually the same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtResumeThread(
    hThread : HANDLE;
    dwResumeCount : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwResumeThread(hThread: HANDLE; dwResumeCount: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Relates to RegSaveKey().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSaveKey(
    KeyHandle : HANDLE;
    FileHandle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSaveKey(KeyHandle: HANDLE; FileHandle: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Relates to RegSaveKeyEx().
// Compatibility: WXP, 2K3
function  NtSaveKeyEx(
    KeyHandle : HANDLE;
    FileHandle : HANDLE;
    Flags : DWORD
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSaveKeyEx(KeyHandle: HANDLE; FileHandle: HANDLE; Flags: DWORD): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtSaveMergedKeys(
    KeyHandle1 : HANDLE;
    KeyHandle2 : HANDLE;
    FileHandle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSaveMergedKeys(KeyHandle1: HANDLE; KeyHandle2: HANDLE; FileHandle: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtSecureConnectPort(
    PortHandle : PHANDLE;
    PortName : PUNICODE_STRING;
    SecurityQos : PSECURITY_QUALITY_OF_SERVICE;
    WriteSection : PPORT_SECTION_WRITE;
    ServerSid : PSID;
    ReadSection : PPORT_SECTION_READ;
    MaxMessageSize : PULONG;
    ConnectData : PVOID;
    ConnectDataLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSecureConnectPort(PortHandle: PHANDLE; PortName: PUNICODE_STRING; SecurityQos: PSECURITY_QUALITY_OF_SERVICE; WriteSection: PPORT_SECTION_WRITE; ServerSid: PSID; ReadSection: PPORT_SECTION_READ; MaxMessageSize: PULONG;
    ConnectData: PVOID; ConnectDataLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Returns STATUS_NOT_IMPLEMENTED. Only MS knows the intention behind this.
// 
// !!!DO NOT USE!!!
// Compatibility: NT4, W2K
function  NtSendWaitReplyChannel(
    x : PVOID;
    y : PVOID;
    z : PVOID;
    z2 : PVOID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSendWaitReplyChannel(x: PVOID; y: PVOID; z: PVOID; z2: PVOID): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Returns STATUS_NOT_IMPLEMENTED. Only MS knows the intention behind this.
// 
// !!!DO NOT USE!!!
// Compatibility: NT4, W2K
function  NtSetContextChannel(
    x : PVOID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetContextChannel(x: PVOID): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetContextThread(
    ThreadHandle : HANDLE;
    Context : PCONTEXT
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetContextThread(ThreadHandle: HANDLE; Context: PCONTEXT): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetDefaultHardErrorPort(
    PortHandle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetDefaultHardErrorPort(PortHandle: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetDefaultLocale(
    ThreadOrSystem : BOOLEAN;
    Locale : LCID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetDefaultLocale(ThreadOrSystem: BOOLEAN; Locale: LCID): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtSetDefaultUILanguage(
    LanguageId : LANGID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetDefaultUILanguage(LanguageId: LANGID): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetEaFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PFILE_FULL_EA_INFORMATION;
    BufferLength : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetEaFile(FileHandle: HANDLE; IoStatusBlock: PIO_STATUS_BLOCK; Buffer: PFILE_FULL_EA_INFORMATION; BufferLength: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetEvent(
    EventHandle : HANDLE;
    PreviousState : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetEvent(EventHandle: HANDLE; PreviousState: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetHighEventPair(
    EventPairHandle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetHighEventPair(EventPairHandle: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetHighWaitLowEventPair(
    EventPairHandle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetHighWaitLowEventPair(EventPairHandle: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4
function  NtSetHighWaitLowThread(): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetHighWaitLowThread(): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK as ZwSetInformationFile().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetInformationFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    FileInformation : PVOID;
    FileInformationLength : ULONG;
    FileInformationClass : FILE_INFORMATION_CLASS
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetInformationFile(FileHandle: HANDLE; IoStatusBlock: PIO_STATUS_BLOCK; FileInformation: PVOID; FileInformationLength: ULONG; FileInformationClass: FILE_INFORMATION_CLASS): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;
   {$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtSetInformationJobObject(
    JobHandle : HANDLE;
    JobInformationClass : JOBOBJECTINFOCLASS;
    JobInformation : PVOID;
    JobInformationLength : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetInformationJobObject(JobHandle: HANDLE; JobInformationClass: JOBOBJECTINFOCLASS; JobInformation: PVOID; JobInformationLength: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetInformationKey(
    KeyHandle : HANDLE;
    KeyInformationClass : KEY_SET_INFORMATION_CLASS;
    KeyInformation : PVOID;
    KeyInformationLength : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetInformationKey(KeyHandle: HANDLE; KeyInformationClass: KEY_SET_INFORMATION_CLASS; KeyInformation: PVOID; KeyInformationLength: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetInformationObject(
    ObjectHandle : HANDLE;
    ObjectInformationClass : OBJECT_INFORMATION_CLASS;
    ObjectInformation : PVOID;
    ObjectInformationLength : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetInformationObject(ObjectHandle: HANDLE; ObjectInformationClass: OBJECT_INFORMATION_CLASS; ObjectInformation: PVOID; ObjectInformationLength: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetInformationProcess(
    ProcessHandle : HANDLE;
    ProcessInformationClass : PROCESSINFOCLASS;
    ProcessInformation : PVOID;
    ProcessInformationLength : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetInformationProcess(ProcessHandle: HANDLE; ProcessInformationClass: PROCESSINFOCLASS; ProcessInformation: PVOID; ProcessInformationLength: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK as ZwSetInformationThread().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetInformationThread(
    ThreadHandle : HANDLE;
    ThreadInformationClass : THREADINFOCLASS;
    ThreadInformation : PVOID;
    ThreadInformationLength : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetInformationThread(ThreadHandle: HANDLE; ThreadInformationClass: THREADINFOCLASS; ThreadInformation: PVOID; ThreadInformationLength: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetInformationToken(
    TokenHandle : HANDLE;
    TokenInformationClass : TOKEN_INFORMATION_CLASS;
    TokenInformation : PVOID;
    TokenInformationLength : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetInformationToken(TokenHandle: HANDLE; TokenInformationClass: TOKEN_INFORMATION_CLASS; TokenInformation: PVOID; TokenInformationLength: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetIntervalProfile(
    Interval : ULONG;
    Source : KPROFILE_SOURCE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetIntervalProfile(Interval: ULONG; Source: KPROFILE_SOURCE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetIoCompletion(
    IoCompletionHandle : HANDLE;
    CompletionKey : ULONG;
    CompletionValue : ULONG;
    Status : NTSTATUS;
    Information : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetIoCompletion(IoCompletionHandle: HANDLE; CompletionKey: ULONG; CompletionValue: ULONG; Status: NTSTATUS; Information: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetLdtEntries(
    Selector1 : ULONG;
    LdtEntry1 : LDT_ENTRY;
    Selector2 : ULONG;
    LdtEntry2 : LDT_ENTRY
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetLdtEntries(Selector1: ULONG; LdtEntry1: LDT_ENTRY; Selector2: ULONG; LdtEntry2: LDT_ENTRY): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetLowEventPair(
    EventPairHandle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetLowEventPair(EventPairHandle: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetLowWaitHighEventPair(
    EventPairHandle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetLowWaitHighEventPair(EventPairHandle: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4
function  NtSetLowWaitHighThread(): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetLowWaitHighThread(): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtSetQuotaInformationFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PFILE_USER_QUOTA_INFORMATION;
    BufferLength : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetQuotaInformationFile(FileHandle: HANDLE; IoStatusBlock: PIO_STATUS_BLOCK; Buffer: PFILE_USER_QUOTA_INFORMATION; BufferLength: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetSecurityObject(
    Handle : HANDLE;
    SecurityInformation : SECURITY_INFORMATION;
    SecurityDescriptor : PSECURITY_DESCRIPTOR
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetSecurityObject(Handle: HANDLE; SecurityInformation: SECURITY_INFORMATION; SecurityDescriptor: PSECURITY_DESCRIPTOR): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetSystemEnvironmentValue(
    Name : PUNICODE_STRING;
    Value : PUNICODE_STRING
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetSystemEnvironmentValue(Name: PUNICODE_STRING; Value: PUNICODE_STRING): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetSystemInformation(
    SystemInformationClass : SYSTEM_INFORMATION_CLASS;
    SystemInformation : PVOID;
    SystemInformationLength : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetSystemInformation(SystemInformationClass: SYSTEM_INFORMATION_CLASS; SystemInformation: PVOID; SystemInformationLength: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetSystemPowerState(
    SystemAction : POWER_ACTION;
    MinSystemState : SYSTEM_POWER_STATE;
    Flags : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetSystemPowerState(SystemAction: POWER_ACTION; MinSystemState: SYSTEM_POWER_STATE; Flags: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetSystemTime(
    NewTime : PLARGE_INTEGER;
    OldTime : PLARGE_INTEGER
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetSystemTime(NewTime: PLARGE_INTEGER; OldTime: PLARGE_INTEGER): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtSetThreadExecutionState(
    ExecutionState : EXECUTION_STATE;
    PreviousExecutionState : PEXECUTION_STATE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetThreadExecutionState(ExecutionState: EXECUTION_STATE; PreviousExecutionState: PEXECUTION_STATE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetTimer(
    TimerHandle : HANDLE;
    DueTime : PLARGE_INTEGER;
    TimerApcRoutine : PTIMER_APC_ROUTINE;
    TimerContext : PVOID;
    Resume : BOOLEAN;
    Period : LONG;
    PreviousState : PBOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetTimer(TimerHandle: HANDLE; DueTime: PLARGE_INTEGER; TimerApcRoutine: PTIMER_APC_ROUTINE; TimerContext: PVOID; Resume: BOOLEAN; Period: LONG; PreviousState: PBOOLEAN): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetTimerResolution(
    RequestedResolution : ULONG;
    Set_ : BOOLEAN;
    ActualResolution : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetTimerResolution(RequestedResolution: ULONG; Set_: BOOLEAN; ActualResolution: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtSetUuidSeed(
    UuidSeed : PUCHAR
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetUuidSeed(UuidSeed: PUCHAR): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK as ZwSetValueKey().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetValueKey(
    KeyHandle : HANDLE;
    ValueName : PUNICODE_STRING;
    TitleIndex : ULONG;
    Type_ : ULONG;
    Data : PVOID;
    DataSize : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetValueKey(KeyHandle: HANDLE; ValueName: PUNICODE_STRING; TitleIndex: ULONG; Type_: ULONG; Data: PVOID; DataSize: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSetVolumeInformationFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PVOID;
    BufferLength : ULONG;
    VolumeInformationClass : FS_INFORMATION_CLASS
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSetVolumeInformationFile(FileHandle: HANDLE; IoStatusBlock: PIO_STATUS_BLOCK; Buffer: PVOID; BufferLength: ULONG; VolumeInformationClass: FS_INFORMATION_CLASS): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtShutdownSystem(
    Action : SHUTDOWN_ACTION
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwShutdownSystem(Action: SHUTDOWN_ACTION): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT4, W2K, WXP, 2K3
function  NtSignalAndWaitForSingleObject(
    HandleToSignal : HANDLE;
    HandleToWait : HANDLE;
    Alertable : BOOLEAN;
    Timeout : PLARGE_INTEGER
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSignalAndWaitForSingleObject(HandleToSignal: HANDLE; HandleToWait: HANDLE; Alertable: BOOLEAN; Timeout: PLARGE_INTEGER): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtStartProfile(
    ProfileHandle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwStartProfile(ProfileHandle: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtStopProfile(
    ProfileHandle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwStopProfile(ProfileHandle: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: WXP, 2K3
function  NtSuspendProcess(
    hProcess : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSuspendProcess(hProcess: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to SuspendThread() from Kernel32.dll.
// Refer to the PSDK for additional information. Usually the same flags
// apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSuspendThread(
    hThread : HANDLE;
    dwLastResumeCount : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSuspendThread(hThread: HANDLE; dwLastResumeCount: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtSystemDebugControl(
    ControlCode : DEBUG_CONTROL_CODE;
    InputBuffer : PVOID;
    InputBufferLength : ULONG;
    OutputBuffer : PVOID;
    OutputBufferLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwSystemDebugControl(ControlCode: DEBUG_CONTROL_CODE; InputBuffer: PVOID; InputBufferLength: ULONG; OutputBuffer: PVOID; OutputBufferLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  NtTerminateJobObject(
    JobHandle : HANDLE;
    ExitStatus : NTSTATUS
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwTerminateJobObject(JobHandle: HANDLE; ExitStatus: NTSTATUS): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtTerminateProcess(
    ProcessHandle : HANDLE;
    ExitStatus : NTSTATUS
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwTerminateProcess(ProcessHandle: HANDLE; ExitStatus: NTSTATUS): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtTerminateThread(
    ThreadHandle : HANDLE;
    ExitStatus : NTSTATUS
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwTerminateThread(ThreadHandle: HANDLE; ExitStatus: NTSTATUS): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtTestAlert(): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwTestAlert(): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtUnloadDriver(
    DriverServiceName : PUNICODE_STRING
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwUnloadDriver(DriverServiceName: PUNICODE_STRING): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtUnloadKey(
    KeyObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwUnloadKey(KeyObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtUnlockFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    LockOffset : PULARGE_INTEGER;
    LockLength : PULARGE_INTEGER;
    Key : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwUnlockFile(FileHandle: HANDLE; IoStatusBlock: PIO_STATUS_BLOCK; LockOffset: PULARGE_INTEGER; LockLength: PULARGE_INTEGER; Key: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtUnlockVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PPVOID;
    LockSize : PULONG;
    LockType : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwUnlockVirtualMemory(ProcessHandle: HANDLE; BaseAddress: PPVOID; LockSize: PULONG; LockType: ULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK as ZwUnmapViewOfSection().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtUnmapViewOfSection(
    ProcessHandle : HANDLE;
    BaseAddress : PVOID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwUnmapViewOfSection(ProcessHandle: HANDLE; BaseAddress: PVOID): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtVdmControl(
    ControlCode : ULONG;
    ControlData : PVOID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwVdmControl(ControlCode: ULONG; ControlData: PVOID): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3
function  NtW32Call(
    RoutineIndex : ULONG;
    Argument : PVOID;
    ArgumentLength : ULONG;
    Result_ : PPVOID;
    ResultLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwW32Call(RoutineIndex: ULONG; Argument: PVOID; ArgumentLength: ULONG; Result_: PPVOID; ResultLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtWaitForMultipleObjects(
    HandleCount : ULONG;
    Handles : PHANDLE;
    WaitType : WAIT_TYPE;
    Alertable : BOOLEAN;
    Timeout : PLARGE_INTEGER
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwWaitForMultipleObjects(HandleCount: ULONG; Handles: PHANDLE; WaitType: WAIT_TYPE; Alertable: BOOLEAN; Timeout: PLARGE_INTEGER): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

{.$IFNDEF JWA_INCLUDEMODE}
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtWaitForSingleObject(
    Handle : HANDLE;
    Alertable : BOOLEAN;
    Timeout : PLARGE_INTEGER
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
{.$ENDIF JWA_INCLUDEMODE}

function  ZwWaitForSingleObject(Handle: HANDLE; Alertable: BOOLEAN; Timeout: PLARGE_INTEGER): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtWaitHighEventPair(
    EventPairHandle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwWaitHighEventPair(EventPairHandle: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtWaitLowEventPair(
    EventPairHandle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwWaitLowEventPair(EventPairHandle: HANDLE): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK as ZwWriteFile().
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtWriteFile(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PVOID;
    Length : ULONG;
    ByteOffset : PLARGE_INTEGER;
    Key : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwWriteFile(FileHandle: HANDLE; Event: HANDLE; ApcRoutine: PIO_APC_ROUTINE; ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; Buffer: PVOID; Length: ULONG; ByteOffset: PLARGE_INTEGER; Key: PULONG): NTSTATUS; stdcall;
    {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT4, W2K, WXP, 2K3
function  NtWriteFileGather(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PFILE_SEGMENT_ELEMENT;
    Length : ULONG;
    ByteOffset : PLARGE_INTEGER;
    Key : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwWriteFileGather(FileHandle: HANDLE; Event: HANDLE; ApcRoutine: PIO_APC_ROUTINE; ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; Buffer: PFILE_SEGMENT_ELEMENT; Length: ULONG; ByteOffset: PLARGE_INTEGER;
    Key: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtWriteRequestData(
    PortHandle : HANDLE;
    Message : PPORT_MESSAGE;
    Index : ULONG;
    Buffer : PVOID;
    BufferLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwWriteRequestData(PortHandle: HANDLE; Message: PPORT_MESSAGE; Index: ULONG; Buffer: PVOID; BufferLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  NtWriteVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PVOID;
    Buffer : PVOID;
    BufferLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwWriteVirtualMemory(ProcessHandle: HANDLE; BaseAddress: PVOID; Buffer: PVOID; BufferLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT4, W2K, WXP, 2K3
function  NtYieldExecution(): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
function  ZwYieldExecution(): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to MakeSelfRelativeSD() from Advapi32.dll.
// Refer to the PSDK for additional information. Usually the same flags
// apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlAbsoluteToSelfRelativeSD(
    pAbsoluteSD : PSECURITY_DESCRIPTOR;
    pSelfRelativeSD : PSECURITY_DESCRIPTOR;
    lpdwBufferLength : LPDWORD
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlAcquirePebLock(); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to AddAccessAllowedAce() from
// Advapi32.dll. Refer to the PSDK for additional information. Usually the
// same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlAddAccessAllowedAce(
    pAcl: PACL;
    dwAceRevision : DWORD;
    AccessMask : ACCESS_MASK;
    pSid : PSID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to AddAccessAllowedAceEx() from
// Advapi32.dll. Refer to the PSDK for additional information. Usually the
// same flags apply.
// Compatibility: W2K, WXP, 2K3
function  RtlAddAccessAllowedAceEx(
    pAcl : PACL;
    dwAceRevision : DWORD;
    AceFlags : DWORD;
    AccessMask : ACCESS_MASK;
    pSid : PSID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to AddAccessDeniedAce() from Advapi32.dll.
// Refer to the PSDK for additional information. Usually the same flags
// apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlAddAccessDeniedAce(
    pAcl : PACL;
    dwAceRevision : DWORD;
    AccessMask : ACCESS_MASK;
    pSid : PSID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to AddAccessDeniedAceEx() from
// Advapi32.dll. Refer to the PSDK for additional information. Usually the
// same flags apply.
// Compatibility: W2K, WXP, 2K3
function  RtlAddAccessDeniedAceEx(
    pAcl : PACL;
    dwAceRevision : DWORD;
    AceFlags : DWORD;
    AccessMask : ACCESS_MASK;
    pSid : PSID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to AddAce() from Advapi32.dll. Refer to
// the PSDK for additional information. Usually the same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlAddAce(
    pAcl : PACL;
    dwAceRevision : DWORD;
    dwStartingAceIndex : DWORD;
    pAceList : PVOID;
    nAceListLength : DWORD
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to AddAuditAccessAce() from Advapi32.dll.
// Refer to the PSDK for additional information. Usually the same flags
// apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlAddAuditAccessAce(
    pAcl : PACL;
    dwAceRevision : DWORD;
    AccessMask : ACCESS_MASK;
    pSid : PSID;
    bAuditSuccess : BOOLEAN;
    bAuditFailure : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to AddAuditAccessAceEx() from
// Advapi32.dll. Refer to the PSDK for additional information. Usually the
// same flags apply.
// Compatibility: W2K, WXP, 2K3
function  RtlAddAuditAccessAceEx(
    pAcl : PACL;
    dwAceRevision : DWORD;
    AceFlags : DWORD;
    AccessMask : ACCESS_MASK;
    pSid : PSID;
    bAuditSuccess : BOOLEAN;
    bAuditFailure : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP
function  RtlAddRange(
    RangeList : PRTL_RANGE_LIST;
    Start : ULONGLONG;
    End_ : ULONGLONG;
    Attributes : UCHAR;
    Flags : ULONG;
    UserData : PVOID;
    Owner : PVOID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: WXP, 2K3
function  RtlAddVectoredExceptionHandler(
    FirstHandler : ULONG;
    VectoredHandler : PVECTORED_EXCEPTION_HANDLER
  ): PVOID; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlAdjustPrivilege(
    Privilege : ULONG;
    Enable : BOOLEAN;
    CurrentThread : BOOLEAN;
    Enabled : PBOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to AllocateAndInitializeSid() from
// Advapi32.dll. Refer to the PSDK for additional information. Usually the
// same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlAllocateAndInitializeSid(
    pIdentifierAuthority : PSID_IDENTIFIER_AUTHORITY;
    SubAuthorityCount : BYTE;
    nSubAuthority0 : DWORD;
    nSubAuthority1 : DWORD;
    nSubAuthority2 : DWORD;
    nSubAuthority3 : DWORD;
    nSubAuthority4 : DWORD;
    nSubAuthority5 : DWORD;
    nSubAuthority6 : DWORD;
    nSubAuthority7 : DWORD;
    var pSid : PSID
  ): BOOL; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// The function HeapAlloc() from Kernel32.dll is an export forwarder to
// this function. This means you can refer to the documentation of
// HeapAlloc()!
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlAllocateHeap(
    hHeap : HANDLE;
    dwFlags : ULONG;
    Size : ULONG
  ): PVOID; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlAnsiCharToUnicodeChar(
    AnsiChar : AnsiChar
  ): WCHAR; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlAnsiStringToUnicodeSize(
    AnsiString : PANSI_STRING
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

{.$IFNDEF JWA_INCLUDEMODE}
// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlAnsiStringToUnicodeString(
    DestinationString : PUNICODE_STRING;
    SourceString : PANSI_STRING;
    AllocateDestinationString : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
{.$ENDIF JWA_INCLUDEMODE}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlAppendAsciizToString(
    DestinationString : PSTRING;
    AppendThisString : LPCSTR
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlAppendStringToString(
    DestinationString : PSTRING;
    AppendThisString : PSTRING
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlAppendUnicodeStringToString(
    DestinationString : PUNICODE_STRING;
    SourceString : PUNICODE_STRING
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlAppendUnicodeToString(
    Destination : PUNICODE_STRING;
    Source : LPCWSTR
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to AreAllAccessesGranted() from
// Advapi32.dll. Refer to the PSDK for additional information. Usually the
// same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlAreAllAccessesGranted(
    GrantedAccess : ACCESS_MASK;
    WantedAccess : ACCESS_MASK
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to AreAnyAccessesGranted() from
// Advapi32.dll. Refer to the PSDK for additional information. Usually the
// same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlAreAnyAccessesGranted(
    GrantedAccess : ACCESS_MASK;
    WantedAccess : ACCESS_MASK
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlAreBitsClear(
    BitMapHeader : PRTL_BITMAP;
    StartingIndex : ULONG;
    Length : ULONG
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlAreBitsSet(
    BitMapHeader : PRTL_BITMAP;
    StartingIndex : ULONG;
    Length : ULONG
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Mentioned in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlAssert(
    FailedAssertion : PVOID;
    FileName : PVOID;
    LineNumber : ULONG;
    Message : PAnsiChar
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
{$IFNDEF JWA_INCLUDEMODE}
// The function RtlCaptureContext() from Kernel32.dll is an export
// forwarder to this function. This means you can refer to the
// documentation of RtlCaptureContext()!
// Compatibility: WXP, 2K3
procedure RtlCaptureContext(
    ContextRecord : PCONTEXT
  ); stdcall;  {$IFNDEF RTDL}external ntdll;{$ENDIF}
{$ENDIF JWA_INCLUDEMODE}

{$IFNDEF JWA_INCLUDEMODE}
// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlCharToInteger(
    Str : PCSZ;
    Base : ULONG;
    Value : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
{$ENDIF JWA_INCLUDEMODE}


// Somehow internally used.
// Compatibility: W2K, WXP, 2K3
procedure RtlCheckForOrphanedCriticalSections(
    hThread : HANDLE
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlCheckRegistryKey(
    RelativeTo : ULONG;
    Path : PWSTR
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlClearAllBits(
    BitMapHeader : PRTL_BITMAP
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlClearBits(
    BitMapHeader : PRTL_BITMAP;
    StartingIndex : ULONG;
    NumberToClear : ULONG
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to HeapCompact() from Kernel32.dll. Refer
// to the PSDK for additional information. Usually the same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlCompactHeap(
    hHeap : HANDLE;
    dwFlags : ULONG
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

{$IFNDEF JWA_INCLUDEMODE}
// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlCompareMemory(
    Source1 : PVOID;
    Source2 : PVOID;
    Length : SIZE_T
  ): SIZE_T; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
{$ENDIF JWA_INCLUDEMODE}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlCompareMemoryUlong(
    Source : PVOID;
    Length : ULONG;
    Value : ULONG
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlCompareString(
    String1 : PSTRING;
    String2 : PSTRING;
    CaseInsensitive : BOOLEAN
  ): LONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlCompareUnicodeString(
    String1 : PUNICODE_STRING;
    String2 : PUNICODE_STRING;
    CaseInsensitive : BOOLEAN
  ): LONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlConvertLongToLargeInteger(
    SignedInteger : LONG
  ): LARGE_INTEGER; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

{.$IFNDEF JWA_INCLUDEMODE}
// This function is very similar to ConvertSidToStringSid() from
// Advapi32.dll. Refer to the PSDK for additional information. Usually the
// same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlConvertSidToUnicodeString(
    UnicodeString : PUNICODE_STRING;
    Sid : PSID;
    AllocateDestinationString : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
{.$ENDIF JWA_INCLUDEMODE}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlConvertUlongToLargeInteger(
    UnsignedInteger : ULONG
  ): LARGE_INTEGER; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlCopyLuid(
    Destination : PLUID;
    Source : PLUID
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP
function  RtlCopyRangeList(
    CopyRangeList : PRTL_RANGE_LIST;
    RangeList : PRTL_RANGE_LIST
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlCopySecurityDescriptor(
    Source : PSECURITY_DESCRIPTOR;
    var Destination : PSECURITY_DESCRIPTOR
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to CopySid() from Advapi32.dll. Refer to
// the PSDK for additional information. Usually the same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlCopySid(
    DestinationLength : ULONG;
    Destination : PSID;
    Source : PSID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlCopyString(
    DestinationString : PSTRING;
    SourceString : PSTRING
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlCopyUnicodeString(
    DestinationString : PUNICODE_STRING;
    SourceString : PUNICODE_STRING
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to InitializeAcl() from Advapi32.dll.
// Refer to the PSDK for additional information. Usually the same flags
// apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlCreateAcl(
    pAcl : PACL;
    nAclLength : DWORD;
    dwAclRevision : DWORD
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to HeapCreate() from Kernel32.dll. Refer
// to the PSDK for additional information. Usually the same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlCreateHeap(
    dwOptions : ULONG;
    Base : PVOID;
    dwMaximumSize : SIZE_T;
    dwInitialSize : SIZE_T;
    UnknownOptional1 : PVOID;
    UnknownOptional2 : PVOID
  ): HANDLE; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlCreateProcessParameters(
    ProcessParameters : PPRTL_USER_PROCESS_PARAMETERS;
    ImageFile : PUNICODE_STRING;
    DllPath : PUNICODE_STRING;
    CurrentDirectory : PUNICODE_STRING;
    CommandLine : PUNICODE_STRING;
    CreationFlags : ULONG;
    WindowTitle : PUNICODE_STRING;
    Desktop : PUNICODE_STRING;
    Reserved : PUNICODE_STRING;
    Reserved2 : PUNICODE_STRING
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlCreateQueryDebugBuffer(
    Size : ULONG;
    EventPair : BOOLEAN
  ): PDEBUG_BUFFER; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlCreateRegistryKey(
    RelativeTo : ULONG;
    Path : PWSTR
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlCreateSecurityDescriptor(
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    Revision : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlCreateUnicodeString(
    DestinationString : PUNICODE_STRING;
    SourceString : PWSTR
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlCreateUnicodeStringFromAsciiz(
    DestinationString : PUNICODE_STRING;
    SourceString : PAnsiChar
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlCreateUserProcess(
    ImageFileName : PUNICODE_STRING;
    Attributes : ULONG;
    ProcessParameters : PRTL_USER_PROCESS_PARAMETERS;
    ProcessSecurityDescriptor : PSECURITY_DESCRIPTOR;
    ThreadSecurityDescriptor : PSECURITY_DESCRIPTOR;
    ParentProcess : HANDLE;
    InheritHandles : BOOLEAN;
    DebugPort : HANDLE;
    ExceptionPort : HANDLE;
    ProcessInfo : PRTL_PROCESS_INFORMATION
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlCreateUserThread(
    hProcess : HANDLE;
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    CreateSuspended : BOOLEAN;
    StackZeroBits : ULONG;
    StackReserve : ULONG;
    StackCommit : ULONG;
    lpStartAddress : PTHREAD_START_ROUTINE;
    lpParameter : PVOID;
    phThread : PHANDLE;
    ClientId : PCLIENT_ID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// #->REVIEW LAST PARAMETER
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlCutoverTimeToSystemTime(
    TargetTimeFields : PTIME_FIELDS;
    Time : PLARGE_INTEGER;
    CurrentTime : PLARGE_INTEGER;
    bUnknown : BOOLEAN
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to DeleteAce() from Advapi32.dll. Refer to
// the PSDK for additional information. Usually the same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlDeleteAce(
    pAcl : PACL;
    dwAceIndex : DWORD
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// The function DeleteCriticalSection() from Kernel32.dll is an export
// forwarder to this function. This means you can refer to the
// documentation of DeleteCriticalSection()!
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlDeleteCriticalSection(
    lpCriticalSection : PRTL_CRITICAL_SECTION
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP
function  RtlDeleteOwnersRanges(
    RangeList : PRTL_RANGE_LIST;
    Owner : PVOID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP
function  RtlDeleteRange(
    RangeList : PRTL_RANGE_LIST;
    Start : ULONGLONG;
    End_ : ULONGLONG;
    Owner : PVOID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlDeleteRegistryValue(
    RelativeTo : ULONG;
    Path : LPCWSTR;
    ValueName : LPCWSTR
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlDeNormalizeProcessParams(
    ProcessParameters : PRTL_USER_PROCESS_PARAMETERS
  ): PRTL_USER_PROCESS_PARAMETERS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to HeapDestroy() from Kernel32.dll. Refer
// to the PSDK for additional information. Usually the same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlDestroyHeap(
    HeapHandle : HANDLE
  ): HANDLE; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlDestroyProcessParameters(
    ProcessParameters : PRTL_USER_PROCESS_PARAMETERS
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlDestroyQueryDebugBuffer(
    DebugBuffer : PDEBUG_BUFFER
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlDetermineDosPathNameType_U(
    wcsPathNameType : PWSTR
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  RtlDnsHostNameToComputerName(
    ComputerName : PUNICODE_STRING;
    DnsName : PUNICODE_STRING;
    AllocateComputerNameString : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlDoesFileExists_U(
    FileName : PWSTR
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlDosPathNameToNtPathName_U(
    DosName : PWSTR;
    var NtName : UNICODE_STRING;
    DosFilePath : PPWSTR;
    NtFilePath : PUNICODE_STRING
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlDosSearchPath_U(
    SearchPath : PWSTR;
    Name : PWSTR;
    Ext : PWSTR;
    cbBuf : ULONG;
    Buffer : PWSTR;
    var Shortname : PWSTR
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: WXP, 2K3
function  RtlDowncaseUnicodeChar(
    Source : WCHAR
  ): WCHAR; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT4, W2K, WXP, 2K3
function  RtlDowncaseUnicodeString(
    DestinationString : PUNICODE_STRING;
    SourceString : PUNICODE_STRING;
    AllocateDestinationString : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// #->REVIEW First parameter must be 0..3, but details have to be
// investigated!!!
// Compatibility: WXP, 2K3
function  RtlDuplicateUnicodeString(
    AddTerminatingZero : ULONG;
    Source : PUNICODE_STRING;
    Destination : PUNICODE_STRING
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
procedure RtlEnableEarlyCriticalSectionEventCreation(); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlEnlargedIntegerMultiply(
    Multiplicand : LONG;
    Multiplier : LONG
  ): LARGE_INTEGER; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlEnlargedUnsignedDivide(
    Dividend : ULARGE_INTEGER;
    Divisor : ULONG;
    Remainder : PULONG
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlEnlargedUnsignedMultiply(
    Multiplicand : ULONG;
    Multiplier : ULONG
  ): LARGE_INTEGER; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// The function EnterCriticalSection() from Kernel32.dll is an export
// forwarder to this function. This means you can refer to the
// documentation of EnterCriticalSection()!
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlEnterCriticalSection(
    lpCriticalSection : PRTL_CRITICAL_SECTION
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlEqualComputerName(
    String1 : PUNICODE_STRING;
    String2 : PUNICODE_STRING
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlEqualDomainName(
    String1 : PUNICODE_STRING;
    String2 : PUNICODE_STRING
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlEqualLuid(
    Luid1 : PLUID;
    Luid2 : PLUID
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to EqualPrefixSid() from Advapi32.dll.
// Refer to the PSDK for additional information. Usually the same flags
// apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlEqualPrefixSid(
    pSid1 : PSID;
    pSid2 : PSID
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to EqualSid() from Advapi32.dll. Refer to
// the PSDK for additional information. Usually the same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlEqualSid(
    pSid1 : PSID;
    pSid2 : PSID
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlEqualString(
    String1 : PSTRING;
    String2 : PSTRING;
    CaseInsensitive : BOOLEAN
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlEqualUnicodeString(
    String1 : PUNICODE_STRING;
    String2 : PUNICODE_STRING;
    CaseInsensitive : BOOLEAN
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlEraseUnicodeString(
    Str : PUNICODE_STRING
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlExpandEnvironmentStrings_U(
    Environment : PVOID;
    Source : PUNICODE_STRING;
    Destination : PUNICODE_STRING;
    ReturnedLength : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlExtendedIntegerMultiply(
    Multiplicand : LARGE_INTEGER;
    Multiplier : LONG
  ): LARGE_INTEGER; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlExtendedLargeIntegerDivide(
    Dividend : LARGE_INTEGER;
    Divisor : ULONG;
    Remainder : PULONG
  ): LARGE_INTEGER; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlExtendedMagicDivide(
    Dividend : LARGE_INTEGER;
    MagicDivisor : LARGE_INTEGER;
    ShiftCount : CCHAR
  ): LARGE_INTEGER; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// The function RtlFillMemory() from Kernel32.dll is an export forwarder to
// this function. This means you can refer to the documentation of
// RtlFillMemory()!
// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlFillMemory(
    Destination : PVOID;
    Length : SIZE_T;
    Fill : UCHAR
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlFillMemoryUlong(
    Destination : PVOID;
    Length : ULONG;
    Fill : ULONG
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Finds characters out of the set contained in CharactersToFind inside
// UnicodeString - description of flags will follow. Only the lower 3 bits
// are valid!!!
// Compatibility: WXP, 2K3
function  RtlFindCharInUnicodeString(
    dwFlags : ULONG;
    UnicodeString : PUNICODE_STRING;
    CharactersToFind : PUNICODE_STRING;
    Positions : PUSHORT
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlFindClearBits(
    BitMapHeader : PRTL_BITMAP;
    NumberToFind : ULONG;
    HintIndex : ULONG
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlFindClearBitsAndSet(
    BitMapHeader : PRTL_BITMAP;
    NumberToFind : ULONG;
    HintIndex : ULONG
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: W2K, WXP, 2K3
function  RtlFindLastBackwardRunClear(
    BitMapHeader : PRTL_BITMAP;
    FromIndex : ULONG;
    StartingRunIndex : PULONG
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: W2K, WXP, 2K3
function  RtlFindLeastSignificantBit(
    Set_ : ULONGLONG
  ): CCHAR; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlFindLongestRunClear(
    BitMapHeader : PRTL_BITMAP;
    StartingIndex : PULONG
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  RtlFindMostSignificantBit(
    Set_ : ULONGLONG
  ): CCHAR; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: W2K, WXP, 2K3
function  RtlFindNextForwardRunClear(
    BitMapHeader : PRTL_BITMAP;
    FromIndex : ULONG;
    StartingRunIndex : PULONG
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP
function  RtlFindRange(
    RangeList : PRTL_RANGE_LIST;
    Minimum : ULONGLONG;
    Maximum : ULONGLONG;
    Length : ULONG;
    Alignment : ULONG;
    Flags : ULONG;
    AttributeAvailableMask : UCHAR;
    Context : PVOID;
    Callback : PRTL_CONFLICT_RANGE_CALLBACK;
    Start : PULONGLONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlFindSetBits(
    BitMapHeader : PRTL_BITMAP;
    NumberToFind : ULONG;
    HintIndex : ULONG
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlFindSetBitsAndClear(
    BitMapHeader : PRTL_BITMAP;
    NumberToFind : ULONG;
    HintIndex : ULONG
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to FindFirstFreeAce() from Advapi32.dll.
// Refer to the PSDK for additional information. Usually the same flags
// apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlFirstFreeAce(
    pAcl : PACL;
    var pAce : PVOID
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlFormatCurrentUserKeyPath(
    CurrentUserKeyPath : PUNICODE_STRING
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlFreeAnsiString(
    AnsiString : PANSI_STRING
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// The function HeapFree() from Kernel32.dll is an export forwarder to this
// function. This means you can refer to the documentation of HeapFree()!
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlFreeHeap(
    hHeap : HANDLE;
    dwFlags : ULONG;
    MemoryPointer : PVOID
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlFreeOemString(
    OemString : POEM_STRING
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP
procedure RtlFreeRangeList(
    RangeList : PRTL_RANGE_LIST
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to FreeSid() from Advapi32.dll. Refer to
// the PSDK for additional information. Usually the same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlFreeSid(
    pSid : PSID
  ): PVOID; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlFreeUnicodeString(
    UnicodeString : PUNICODE_STRING
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to GetAce() from Advapi32.dll. Refer to
// the PSDK for additional information. Usually the same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlGetAce(
    pAcl : PACL;
    dwAceIndex : DWORD;
    var pAce : PVOID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Mentioned in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlGetCallersAddress(
    CallersAddress : PPVOID;
    CallersCaller : PPVOID
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to GetSecurityDescriptorControl() from
// Advapi32.dll. Refer to the PSDK for additional information. Usually the
// same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlGetControlSecurityDescriptor(
    pSecurityDescriptor : PSECURITY_DESCRIPTOR;
    var Control : SECURITY_DESCRIPTOR_CONTROL;
    var dwRevision : DWORD
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlGetCurrentDirectory_U(
    MaximumLength : ULONG;
    Buffer : PWSTR
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: WXP, 2K3
function  RtlGetCurrentPeb(): PPEB; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to GetSecurityDescriptorDacl() from
// Advapi32.dll. Refer to the PSDK for additional information. Usually the
// same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlGetDaclSecurityDescriptor(
    pSecurityDescriptor : PSECURITY_DESCRIPTOR;
    var bDaclPresent : BOOLEAN;
    var Dacl : PACL;
    var bDaclDefaulted : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP
function  RtlGetFirstRange(
    RangeList : PRTL_RANGE_LIST;
    Iterator : PRTL_RANGE_LIST_ITERATOR;
    var Range : PRTL_RANGE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlGetFullPathName_U(
    DosName : PWSTR;
    Size : ULONG;
    Buf : PWSTR;
    var Shortname : PWSTR
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to GetSecurityDescriptorGroup() from
// Advapi32.dll. Refer to the PSDK for additional information. Usually the
// same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlGetGroupSecurityDescriptor(
    pSecurityDescriptor : PSECURITY_DESCRIPTOR;
    var pGroup : PSID;
    var bGroupDefaulted : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: WXP, 2K3
function  RtlGetLastNtStatus(): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlGetLongestNtPathLength(): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP
function  RtlGetNextRange(
    Iterator : PRTL_RANGE_LIST_ITERATOR;
    var Range : PRTL_RANGE;
    MoveForwards : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlGetNtGlobalFlags(): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlGetNtProductType(
    var ProductType : ULONG
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// #->REVIEW LAST PARAMETER
// Compatibility: WXP, 2K3
procedure RtlGetNtVersionNumbers(
    var dwMajorVersion : ULONG;
    var dwMinorVersion : ULONG;
    UnknownCanBeNull : PDWORD
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to GetSecurityDescriptorOwner() from
// Advapi32.dll. Refer to the PSDK for additional information. Usually the
// same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlGetOwnerSecurityDescriptor(
    pSecurityDescriptor : PSECURITY_DESCRIPTOR;
    var pOwner : PSID;
    var OwnerDefaulted : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to GetProcessHeaps() from Kernel32.dll.
// Refer to the PSDK for additional information. Usually the same flags
// apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlGetProcessHeaps(
    ArraySize : ULONG;
    HeapArray : PHANDLE
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to GetSecurityDescriptorSacl() from
// Advapi32.dll. Refer to the PSDK for additional information. Usually the
// same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlGetSaclSecurityDescriptor(
    pSecurityDescriptor : PSECURITY_DESCRIPTOR;
    var bSaclPresent : BOOLEAN;
    var Sacl : PACL;
    var bSaclDefaulted : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to GetVersionEx() from Kernel32.dll. Refer
// to the PSDK for additional information. Usually the same flags apply.
// Documented in the DDK.
// Compatibility: W2K, WXP, 2K3
function  RtlGetVersion(
    lpVersionInformation : PRTL_OSVERSIONINFOW
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: W2K, WXP, 2K3
function  RtlGUIDFromString(
    GuidString : PUNICODE_STRING;
    Guid : LPGUID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to GetSidIdentifierAuthority() from
// Advapi32.dll. Refer to the PSDK for additional information. Usually the
// same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlIdentifierAuthoritySid(
    Sid : PSID
  ): PSID_IDENTIFIER_AUTHORITY; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to ImageDirectoryEntryToData() from
// Dbghelp.dll. Refer to the PSDK for additional information. Usually the
// same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlImageDirectoryEntryToData(
    ImageBase : HMODULE;
    MappedAsImage : BOOLEAN;
    DirectoryEntry : USHORT;
    Size : PULONG
  ): PVOID; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to ImageNtHeader() from Dbghelp.dll. Refer
// to the PSDK for additional information. Usually the same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlImageNtHeader(
    ImageBase : HMODULE
  ): PIMAGE_NT_HEADERS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to ImageNtHeader() from Dbghelp.dll. Refer
// to the PSDK for additional information. Usually the same flags apply.
// With dwFlags equal 1 it shows the same behavior as RtlImageNtHeader()
// Compatibility: 2K3
function  RtlImageNtHeaderEx(
    dwFlags : DWORD;
    ImageBase : HMODULE
  ): PIMAGE_NT_HEADERS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to ImageRvaToSection() from Dbghelp.dll.
// Refer to the PSDK for additional information. Usually the same flags
// apply.
// Compatibility: NT4, W2K, WXP, 2K3
function  RtlImageRvaToSection(
    NtHeaders : PIMAGE_NT_HEADERS;
    ImageBase : HMODULE;
    Rva : ULONG
  ): PIMAGE_SECTION_HEADER; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to ImageRvaToVa() from Dbghelp.dll. Refer
// to the PSDK for additional information. Usually the same flags apply.
// Compatibility: NT4, W2K, WXP, 2K3
function  RtlImageRvaToVa(
    NtHeaders : PIMAGE_NT_HEADERS;
    ImageBase : HMODULE;
    Rva : ULONG;
    var LastRvaSection : PIMAGE_SECTION_HEADER
  ): PVOID; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to ImpersonateSelf() from Advapi32.dll.
// Refer to the PSDK for additional information. Usually the same flags
// apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlImpersonateSelf(
    ImpersonationLevel : SECURITY_IMPERSONATION_LEVEL
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlInitAnsiString(
    DestinationString : PANSI_STRING;
    SourceString : PCSZ
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: 2K3
function  RtlInitAnsiStringEx(
    DestinationString : PANSI_STRING;
    SourceString : PCSZ
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlInitializeBitMap(
    BitMapHeader : PRTL_BITMAP;
    BitMapBuffer : PULONG;
    SizeOfBitMap : ULONG
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlInitializeCriticalSection(
    lpCriticalSection : PRTL_CRITICAL_SECTION
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT4, W2K, WXP, 2K3
function  RtlInitializeCriticalSectionAndSpinCount(
    lpCriticalSection : PRTL_CRITICAL_SECTION;
    dwSpinCount : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP
procedure RtlInitializeRangeList(
    RangeList : PRTL_RANGE_LIST
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to InitializeSid() from Advapi32.dll.
// Refer to the PSDK for additional information. Usually the same flags
// apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlInitializeSid(
    pSid : PSID;
    pIdentifierAuthority : PSID_IDENTIFIER_AUTHORITY;
    nSubAuthorityCount : UCHAR
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

{$IFNDEF JWA_INCLUDEMODE}
// Compatibility: WXP, 2K3
procedure RtlInitializeSListHead(
    ListHead : PSLIST_HEADER
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
{$ENDIF JWA_INCLUDEMODE}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlInitString(
    DestinationString : PSTRING;
    SourceString : PCSZ
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlInitUnicodeString(
    DestinationString : PUNICODE_STRING;
    SourceString : LPCWSTR
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: WXP, 2K3
function  RtlInitUnicodeStringEx(
    DestinationString : PUNICODE_STRING;
    SourceString : LPCWSTR
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: W2K, WXP, 2K3
function  RtlInt64ToUnicodeString(
    Value : ULONGLONG;
    Base : ULONG;
    Str : PUNICODE_STRING
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlIntegerToChar(
    Value : ULONG;
    Base : ULONG;
    Length : ULONG;
    Str : PAnsiChar
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlIntegerToUnicodeString(
    Value : ULONG;
    Base : ULONG;
    Str : PUNICODE_STRING
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

{$IFNDEF JWA_INCLUDEMODE}
// Compatibility: WXP, 2K3
function  RtlInterlockedFlushSList(
    ListHead : PSLIST_HEADER
  ): PSLIST_ENTRY; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}


// Compatibility: WXP, 2K3
function  RtlInterlockedPopEntrySList(
    ListHead : PSLIST_HEADER
  ): PSLIST_ENTRY; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
{$ENDIF JWA_INCLUDEMODE}
// Compatibility: WXP, 2K3
function  RtlInterlockedPushEntrySList(
    ListHead : PSLIST_HEADER;
    ListEntry : PSLIST_ENTRY
  ): PSLIST_ENTRY; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}


// Compatibility: W2K, WXP
function  RtlInvertRangeList(
    InvertedRangeList : PRTL_RANGE_LIST;
    RangeList : PRTL_RANGE_LIST
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: WXP, 2K3
function  RtlIpv4AddressToStringA(
    IP : PULONG;
    Buffer : LPSTR
  ): LPSTR; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: WXP, 2K3
function  RtlIpv4AddressToStringW(
    IP : PULONG;
    Buffer : LPWSTR
  ): LPWSTR; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlIsDosDeviceName_U(
    TestString : LPCWSTR
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

{.$IFNDEF JWA_INCLUDEMODE}
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlIsNameLegalDOS8Dot3(
    Name : PUNICODE_STRING;
    OemName : POEM_STRING;
    NameContainsSpaces : PBOOLEAN
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
{.$ENDIF JWA_INCLUDEMODE}

// Compatibility: W2K, WXP
function  RtlIsRangeAvailable(
    RangeList : PRTL_RANGE_LIST;
    Start : ULONGLONG;
    End_ : ULONGLONG;
    Flags : ULONG;
    AttributeAvailableMask : UCHAR;
    Context : PVOID;
    Callback : PRTL_CONFLICT_RANGE_CALLBACK;
    Available : PBOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to IsTextUnicode() from Advapi32.dll.
// Refer to the PSDK for additional information. Usually the same flags
// apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlIsTextUnicode(
    lpBuffer : PVOID;
    cb : Integer;
    lpi : LPINT
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlLargeIntegerAdd(
    Addend1 : LARGE_INTEGER;
    Addend2 : LARGE_INTEGER
  ): LARGE_INTEGER; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlLargeIntegerArithmeticShift(
    LargeInteger : LARGE_INTEGER;
    ShiftCount : CCHAR
  ): LARGE_INTEGER; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlLargeIntegerDivide(
    Dividend : LARGE_INTEGER;
    Divisor : LARGE_INTEGER;
    Remainder : PLARGE_INTEGER
  ): LARGE_INTEGER; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlLargeIntegerNegate(
    NegateThis : LARGE_INTEGER
  ): LARGE_INTEGER; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlLargeIntegerShiftLeft(
    LargeInteger : LARGE_INTEGER;
    ShiftCount : CCHAR
  ): LARGE_INTEGER; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlLargeIntegerShiftRight(
    LargeInteger : LARGE_INTEGER;
    ShiftCount : CCHAR
  ): LARGE_INTEGER; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlLargeIntegerSubtract(
    Number : LARGE_INTEGER;
    Subtrahend : LARGE_INTEGER
  ): LARGE_INTEGER; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlLargeIntegerToChar(
    Value : PLARGE_INTEGER;
    Base : ULONG;
    BufferLength : ULONG;
    Buffer : PAnsiChar
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// The function LeaveCriticalSection() from Kernel32.dll is an export
// forwarder to this function. This means you can refer to the
// documentation of LeaveCriticalSection()!
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlLeaveCriticalSection(
    lpCriticalSection : PRTL_CRITICAL_SECTION
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to GetSidLengthRequired() from
// Advapi32.dll. Refer to the PSDK for additional information. Usually the
// same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlLengthRequiredSid(
    nSubAuthorityCount : ULONG
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to GetSecurityDescriptorLength() from
// Advapi32.dll. Refer to the PSDK for additional information. Usually the
// same flags apply.
// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlLengthSecurityDescriptor(
    SecurityDescriptor : PSECURITY_DESCRIPTOR
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to GetLengthSid() from Advapi32.dll. Refer
// to the PSDK for additional information. Usually the same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlLengthSid(
    pSid : PSID
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

{.$IFNDEF JWA_INCLUDEMODE}
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlLocalTimeToSystemTime(
    LocalTime : PLARGE_INTEGER;
    SystemTime : PLARGE_INTEGER
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
{.$ENDIF JWA_INCLUDEMODE}

// This function is very similar to HeapLock() from Kernel32.dll. Refer to
// the PSDK for additional information. Usually the same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlLockHeap(
    hHeap : PVOID
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to MakeSelfRelativeSD() from Advapi32.dll.
// Refer to the PSDK for additional information. Usually the same flags
// apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlMakeSelfRelativeSD(
    pAbsoluteSD : PSECURITY_DESCRIPTOR;
    pSelfRelativeSD : PSECURITY_DESCRIPTOR;
    lpdwBufferLength : LPDWORD
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to MapGenericMask() from Advapi32.dll.
// Refer to the PSDK for additional information. Usually the same flags
// apply.
// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlMapGenericMask(
    AccessMask : PACCESS_MASK;
    GenericMapping : PGENERIC_MAPPING
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Maps an error from the security subsystem to a native error status.
// Compatibility: WXP, 2K3
function  RtlMapSecurityErrorToNtStatus(
    SecurityError : DWORD
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP
function  RtlMergeRangeLists(
    MergedRangeList : PRTL_RANGE_LIST;
    RangeList1 : PRTL_RANGE_LIST;
    RangeList2 : PRTL_RANGE_LIST;
    Flags : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlMoveMemory(
    Destination : PVOID;
    Source : PVOID;
    Length : SIZE_T
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlNormalizeProcessParams(
    ProcessParameters : PRTL_USER_PROCESS_PARAMETERS
  ): PRTL_USER_PROCESS_PARAMETERS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlNtStatusToDosError(
    Status : NTSTATUS
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}


// Compatibility: WXP, 2K3
function  RtlNtStatusToDosErrorNoTeb(
    Status : NTSTATUS
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlNumberOfClearBits(
    BitMapHeader : PRTL_BITMAP
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlNumberOfSetBits(
    BitMapHeader : PRTL_BITMAP
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlOemStringToUnicodeSize(
    AnsiString : POEM_STRING
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlOemStringToUnicodeString(
    DestinationString : PUNICODE_STRING;
    SourceString : POEM_STRING;
    AllocateDestinationString : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlOemToUnicodeN(
    UnicodeString : PWSTR;
    UnicodeSize : ULONG;
    var ResultSize : ULONG;
    OemString : PAnsiChar;
    OemSize : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlOpenCurrentUser(
    samDesired : ACCESS_MASK;
    phkResult : PHKEY
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Either raises an exception of type STATUS_RESOURCE_NOT_OWNED or returns
// a BOOLEAN value.
// Should perhaps not be called explicitly.
// Compatibility: WXP, 2K3
function  RtlpNotOwnerCriticalSection(
    lpCriticalSection : PRTL_CRITICAL_SECTION
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This is a private wrapper for NtCreateKey().
// However, 2 of the parameters are not being used!
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlpNtCreateKey(
    KeyHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    Unused1 : ULONG;
    Unused2 : ULONG;
    Disposition : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlpNtEnumerateSubKey(
    KeyHandle : HANDLE;
    SubKeyName : PUNICODE_STRING;
    Index : ULONG;
    Unused1 : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to NtCreateKey() from Ntdll.dll. Usually
// the same or similar flags apply.
// This is exactly the same as NtDeleteKey() by now!
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlpNtMakeTemporaryKey(
    KeyHandle : HANDLE
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlpNtOpenKey(
    KeyHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    Unused : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlpNtQueryValueKey(
    KeyHandle : HANDLE;
    Type_ : PULONG;
    Data : PVOID;
    DataSize : PULONG;
    Unused : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This is a private wrapper for NtSetValueKey().
// The parameters of TitleIndex and ValueName are not being passed, that is
// empty.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlpNtSetValueKey(
    KeyHandle : HANDLE;
    Type_ : ULONG;
    Data : PVOID;
    DataSize : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlPrefixString(
    String1 : PANSI_STRING;
    String2 : PANSI_STRING;
    CaseInsensitive : BOOLEAN
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlPrefixUnicodeString(
    String1 : PUNICODE_STRING;
    String2 : PUNICODE_STRING;
    CaseInsensitive : BOOLEAN
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

{$IFNDEF JWA_INCLUDEMODE}
// Compatibility: WXP, 2K3
function  RtlQueryDepthSList(
    ListHead : PSLIST_HEADER
  ): USHORT; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
{$ENDIF JWA_INCLUDEMODE}

// VarValue has to have a buffer assigned big enough to hold the value.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlQueryEnvironmentVariable_U(
    Environment : PVOID;
    VarName : PUNICODE_STRING;
    VarValue : PUNICODE_STRING
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to GetAclInformation() from Advapi32.dll.
// Refer to the PSDK for additional information. Usually the same flags
// apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlQueryInformationAcl(
    pAcl : PACL;
    pAclInformation : PVOID;
    nAclInformationLength : DWORD;
    dwAclInformationClass : ACL_INFORMATION_CLASS
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlQueryProcessDebugInformation(
    ProcessId : ULONG;
    DebugInfoClassMask : ULONG;
    DebugBuffer : PDEBUG_BUFFER
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlQueryRegistryValues(
    RelativeTo : ULONG;
    Path : LPCWSTR;
    QueryTable : PRTL_QUERY_REGISTRY_TABLE;
    Context : PVOID;
    Environment : PVOID
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlRaiseStatus(
    Status : NTSTATUS
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlRandom(
    Seed : PULONG
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: WXP, 2K3
function  RtlRandomEx(
    Seed : PULONG
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// The function HeapReAlloc() from Kernel32.dll is an export forwarder to
// this function. This means you can refer to the documentation of
// HeapReAlloc()!
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlReAllocateHeap(
    hHeap : HANDLE;
    dwFlags : ULONG;
    lpMem : PVOID;
    dwBytes : SIZE_T
  ): PVOID; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlReleasePebLock(); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: WXP, 2K3
function  RtlRemoveVectoredExceptionHandler(
    VectoredHandlerHandle : PVOID
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: WXP, 2K3
procedure RtlRestoreLastWin32Error(
    dwErrCode : DWORD
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlRunDecodeUnicodeString(
    CodeSeed : UCHAR;
    StringToDecode : PUNICODE_STRING
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// If CodeSeed == 0 it will be assigned a value by the function. Use this
// very value in a call to RtlRunDecodeUnicodeString()! To decode the
// string afterwards.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlRunEncodeUnicodeString(
    var CodeSeed : UCHAR;
    StringToEncode : PUNICODE_STRING
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlSecondsSince1970ToTime(
    SecondsSince1970 : ULONG;
    Time : PLARGE_INTEGER
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlSecondsSince1980ToTime(
    SecondsSince1980 : ULONG;
    Time : PLARGE_INTEGER
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to MakeAbsoluteSD() from Advapi32.dll.
// Refer to the PSDK for additional information. Usually the same flags
// apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlSelfRelativeToAbsoluteSD(
    pSelfRelativeSD : PSECURITY_DESCRIPTOR;
    pAbsoluteSD : PSECURITY_DESCRIPTOR;
    lpdwAbsoluteSDSize : LPDWORD;
    pDacl : PACL;
    lpdwDaclSize : LPDWORD;
    pSacl : PACL;
    lpdwSaclSize : LPDWORD;
    pOwner : PSID;
    lpdwOwnerSize : LPDWORD;
    pPrimaryGroup : PSID;
    lpdwPrimaryGroupSize : LPDWORD
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlSetAllBits(
    BitMapHeader : PRTL_BITMAP
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlSetBits(
    BitMapHeader : PRTL_BITMAP;
    StartingIndex : ULONG;
    NumberToSet : ULONG
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to SetSecurityDescriptorControl() from
// Advapi32.dll. Refer to the PSDK for additional information. Usually the
// same flags apply.
// Compatibility: W2K, WXP, 2K3
function  RtlSetControlSecurityDescriptor(
    pSecurityDescriptor : PSECURITY_DESCRIPTOR;
    ControlBitsOfInterest : SECURITY_DESCRIPTOR_CONTROL;
    ControlBitsToSet : SECURITY_DESCRIPTOR_CONTROL
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// The function SetCriticalSectionSpinCount() from Kernel32.dll is an
// export forwarder to this function. This means you can refer to the
// documentation of SetCriticalSectionSpinCount()!
// Compatibility: NT4, W2K, WXP, 2K3
function  RtlSetCriticalSectionSpinCount(
    lpCriticalSection : PRTL_CRITICAL_SECTION;
    dwSpinCount : ULONG
  ): DWORD; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlSetCurrentDirectory_U(
    NewCurrentDirectory : PUNICODE_STRING
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlSetDaclSecurityDescriptor(
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    DaclPresent : BOOLEAN;
    Dacl : PACL;
    DaclDefaulted : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlSetGroupSecurityDescriptor(
    pSecurityDescriptor : PSECURITY_DESCRIPTOR;
    pGroup : PSID;
    bGroupDefaulted : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to SetAclInformation() from Advapi32.dll.
// Refer to the PSDK for additional information. Usually the same flags
// apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlSetInformationAcl(
    pAcl : PACL;
    pAclInformation : PVOID;
    nInformationLength : DWORD;
    dwAclInformationClass : ACL_INFORMATION_CLASS
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: WXP, 2K3
function  RtlSetLastWin32ErrorAndNtStatusFromNtStatus(
    Status : NTSTATUS
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlSetOwnerSecurityDescriptor(
    pSecurityDescriptor : PSECURITY_DESCRIPTOR;
    pOwner : PSID;
    bOwnerDefaulted : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: WXP, 2K3
function  RtlSetProcessIsCritical(
    bIsCritical : BOOLEAN;
    pbOldIsCriticalValue : PBOOLEAN;
    bUnknownCanBeFalse : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to SetSecurityDescriptorSacl() from
// Advapi32.dll. Refer to the PSDK for additional information. Usually the
// same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlSetSaclSecurityDescriptor(
    pSecurityDescriptor : PSECURITY_DESCRIPTOR;
    bSaclPresent : BOOLEAN;
    pSacl : PACL;
    SaclDefaulted : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: WXP, 2K3
function  RtlSetThreadIsCritical(
    bIsCritical : BOOLEAN;
    pbOldIsCriticalValue : PBOOLEAN;
    bUnknownCanBeFalse : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// The function HeapSize() from Kernel32.dll is an export forwarder to this
// function. This means you can refer to the documentation of HeapSize()!
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlSizeHeap(
    hHeap : HANDLE;
    dwFlags : ULONG;
    lpMem : PVOID
  ): SIZE_T; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: W2K, WXP, 2K3
function  RtlStringFromGUID(
    Guid : REFGUID;
    GuidString : PUNICODE_STRING
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to GetSidSubAuthorityCount() from
// Advapi32.dll. Refer to the PSDK for additional information. Usually the
// same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlSubAuthorityCountSid(
    pSid : PSID
  ): PUCHAR; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to GetSidSubAuthority() from Advapi32.dll.
// Refer to the PSDK for additional information. Usually the same flags
// apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlSubAuthoritySid(
    pSid : PSID;
    nSubAuthority : DWORD
  ): PDWORD; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlSystemTimeToLocalTime(
    SystemTime : PLARGE_INTEGER;
    LocalTime : PLARGE_INTEGER
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlTimeFieldsToTime(
    TimeFields : PTIME_FIELDS;
    Time : PLARGE_INTEGER
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlTimeToElapsedTimeFields(
    Time : PLARGE_INTEGER;
    TimeFields : PTIME_FIELDS
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

{.$IFNDEF JWA_INCLUDEMODE}
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlTimeToSecondsSince1970(
    Time : PLARGE_INTEGER;
    ElapsedSeconds : PULONG
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
{.$ENDIF JWA_INCLUDEMODE}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlTimeToSecondsSince1980(
    Time : PLARGE_INTEGER;
    ElapsedSeconds : PULONG
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlTimeToTimeFields(
    Time : PLARGE_INTEGER;
    TimeFields : PTIME_FIELDS
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// The function TryEnterCriticalSection() from Kernel32.dll is an export
// forwarder to this function. This means you can refer to the
// documentation of TryEnterCriticalSection()!
// Compatibility: NT4, W2K, WXP, 2K3
function  RtlTryEnterCriticalSection(
    lpCriticalSection : PRTL_CRITICAL_SECTION
  ): BOOL; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlUnicodeStringToAnsiSize(
    UnicodeString : PUNICODE_STRING
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
{.$IFNDEF JWA_INCLUDEMODE}
function  RtlUnicodeStringToAnsiString(
    DestinationString : PANSI_STRING;
    SourceString : PUNICODE_STRING;
    AllocateDestinationString : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
{.$ENDIF JWA_INCLUDEMODE}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlUnicodeStringToCountedOemString(
    DestinationString : POEM_STRING;
    SourceString : PUNICODE_STRING;
    AllocateDestinationString : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlUnicodeStringToInteger(
    Str : PUNICODE_STRING;
    Base : ULONG;
    Value : PULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlUnicodeStringToOemSize(
    UnicodeString : PUNICODE_STRING
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

{.$IFNDEF JWA_INCLUDEMODE}
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlUnicodeStringToOemString(
    DestinationString : POEM_STRING;
    SourceString : PCUNICODE_STRING;
    AllocateDestinationString : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
{.$ENDIF JWA_INCLUDEMODE}

{.$IFNDEF JWA_INCLUDEMODE}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlUnicodeToMultiByteSize(
    BytesInMultiByteString : PULONG;
    UnicodeString : PWSTR;
    BytesInUnicodeString : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}


// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlUniform(
    Seed : PULONG
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

{.$ENDIF JWA_INCLUDEMODE}

// The function RtlUnwind() from Kernel32.dll is an export forwarder to
// this function. This means you can refer to the documentation of
// RtlUnwind()!
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlUnwind(
    TargetFrame : PVOID;
    TargetIp : PVOID;
    ExceptionRecord : PEXCEPTION_RECORD;
    ReturnValue : PVOID
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlUpcaseUnicodeChar(
    SourceCharacter : WCHAR
  ): WCHAR; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlUpcaseUnicodeString(
    DestinationString : PUNICODE_STRING;
    SourceString : PUNICODE_STRING;
    AllocateDestinationString : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlUpcaseUnicodeStringToAnsiString(
    DestinationString : PSTRING;
    SourceString : PUNICODE_STRING;
    AllocateDestinationString : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlUpcaseUnicodeStringToCountedOemString(
    DestinationString : PSTRING;
    SourceString : PUNICODE_STRING;
    AllocateDestinationString : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlUpcaseUnicodeStringToOemString(
    DestinationString : PSTRING;
    SourceString : PUNICODE_STRING;
    AllocateDestinationString : BOOLEAN
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlUpcaseUnicodeToMultiByteN(
    MbString : PAnsiChar;
    MbSize : ULONG;
    var ResultSize : ULONG;
    UnicodeString : PWSTR;
    UnicodeSize : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlUpcaseUnicodeToOemN(
    OemString : PAnsiChar;
    OemSize : ULONG;
    var ResultSize : ULONG;
    UnicodeString : PWSTR;
    UnicodeSize : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlUpperChar(
    Character : AnsiChar
  ): AnsiChar; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlUpperString(
    DestinationString : PSTRING;
    SourceString : PSTRING
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// #->REVIEW NUMBER OF PARAMETERS
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlValidAcl(
    Acl : PACL
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// This function is very similar to HeapValidate() from Kernel32.dll. Refer
// to the PSDK for additional information. Usually the same flags apply.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlValidateHeap(
    hHeap : HANDLE;
    dwFlags : ULONG;
    lpMem : LPCVOID
  ): BOOL; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: WXP, 2K3
function  RtlValidateUnicodeString(
    dwMustBeNull : ULONG;
    ValidateThis : PUNICODE_STRING
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: W2K, WXP, 2K3
function  RtlValidRelativeSecurityDescriptor(
    SecurityDescriptorInput : PSECURITY_DESCRIPTOR;
    SecurityDescriptorLength : ULONG;
    RequiredInformation : SECURITY_INFORMATION
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlValidSecurityDescriptor(
    SecurityDescriptor : PSECURITY_DESCRIPTOR
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// #->REVIEW NUMBER OF PARAMETERS; XREF: see IsValidSid()!
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlValidSid(
    pSid : PSID
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: W2K, WXP, 2K3
function  RtlVerifyVersionInfo(
    VersionInfo : PRTL_OSVERSIONINFOEXW;
    TypeMask : ULONG;
    ConditionMask : ULONGLONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
function  RtlVolumeDeviceToDosName(
    VolumeDeviceObject : PVOID;
    DosName : PUNICODE_STRING
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlWriteRegistryValue(
    RelativeTo : ULONG;
    Path : LPCWSTR;
    ValueName : LPCWSTR;
    ValueType : ULONG;
    ValueData : PVOID;
    ValueLength : ULONG
  ): NTSTATUS; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlxAnsiStringToUnicodeSize(
    AnsiString : PANSI_STRING
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlxOemStringToUnicodeSize(
    AnsiString : POEM_STRING
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlxUnicodeStringToAnsiSize(
    UnicodeString : PUNICODE_STRING
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlxUnicodeStringToOemSize(
    UnicodeString : PUNICODE_STRING
  ): ULONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Compatibility: NT3, NT4, W2K, WXP, 2K3
function  RtlZeroHeap(
    hHeap : HANDLE;
    dwFlags : ULONG
  ): BOOLEAN; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

// Documented in the DDK.
// Compatibility: NT3, NT4, W2K, WXP, 2K3
procedure RtlZeroMemory(
    Destination : PVOID;
    Length : SIZE_T
  ); stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}

{$IFNDEF JWA_INCLUDEMODE}
// This function is very similar to VerSetConditionMask() from
// Kernel32.dll. Refer to the PSDK for additional information. Usually the
// same flags apply.
// Compatibility: W2K, WXP, 2K3
function  VerSetConditionMask(
    ConditionMask : ULONGLONG;
    dwTypeMask : DWORD;
    Condition : BYTE
  ): ULONGLONG; stdcall; {$IFNDEF RTDL}external ntdll;{$ENDIF}
{$ENDIF JWA_INCLUDEMODE}

//// 810 automatically created prototype entries.
//// END  : Function prototypes

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INTERFACESECTION}

// This function is actually declared as a macro defined as memcpy()

procedure RtlCopyMemory(Destination: PVOID; Source: PVOID; Length: SIZE_T); stdcall;
begin
  Move(Source^, Destination^, Length);
end;

// Own function to retrieve the process's heap handle

function NtpGetProcessHeap(): HANDLE;
asm
  {$ifdef CPU386}
  mov   EAX, FS:[018h]            // EAX now holds the TEB address
  mov   EAX, [EAX+030h]           // TEB+$30 holds the PEB address
  mov   EAX, DWORD PTR [EAX+018h] // PEB+$30 holds the ProcessHeap's handle
  {$endif}
  {$ifdef cpux86_64}
    mov   RAX, GS:[48]              // EAX now holds the TEB address
    mov   RAX, [RAX+060h]           // TEB+$30 holds the PEB address
    mov   RAX, DWORD PTR [RAX+48]   // PEB+$30 holds the ProcessHeap's handle
  {$endif cpux86_64}
end;

// Own function to retrieve the thread environment block (TEB) pointer

function NtpCurrentTeb(): PTEB;
asm
  {$ifdef cpu386}
    mov   EAX, FS:[018h]
  {$endif cpu386}
  {$ifdef cpux86_64}
    mov   RAX, GS:[48]
  {$endif cpux86_64}
  
end;

// Own function to retrieve the process environment block (PEB) pointer

function RtlpGetCurrentPeb(): PPEB;
asm
  {$ifdef cpu386}
   mov   EAX, FS:[018h]
   mov   EAX, [EAX+030h]
  {$endif cpu386}
  {$ifdef cpux86_64}
   mov   RAX, GS:[24]
   mov   RAX, [RAX+060h]
  {$endif cpux86_64}
end;

(* Own function to swap bytes in 16bit values

   The RtlUshortByteSwap routine converts a USHORT from
   little-endian to big-endian, and vice versa. *)

function RtlUshortByteSwap(Source: USHORT): USHORT;
asm
  {$ifdef cpux86_64}
   mov   CX, AX
  {$endif cpux86_64}
  rol   AX, 08h
end;

(* Own function to swap bytes in 32bit values

   The RtlUlongByteSwap routine converts a ULONG from little-endian to
   big-endian, and vice versa. *)

function RtlUlongByteSwap(Source: ULONG): ULONG;
asm
  {$ifndef FPC}
  // This is not written as mnemonics to be compatible with D4!
  db    0Fh, 0C8h       // "bswap EAX" can only be executed on 486+!!!
  {$else}
    {$ifdef cpux86_64}
       mov   ECX, EAX
    {$endif cpux86_64}
       bswap EAX	// .. but bswap EAX is also 64-bit!!! 0F C8 isn't.
  {$endif}
   
(*
// Does the same but perhaps slower ...
                        // Source = $11223344
  rol   AX,  08h        // Source = $11224433
  rol   EAX, 0Fh        // Source = $44331122
  rol   AX,  08h        // Source = $44332211
*)
end;

(* Own function to swap bytes in 64bit values

   The RtlUlonglongByteSwap routine converts a ULONGLONG from
   little-endian to big-endian, and vice versa. *)

function RtlUlonglongByteSwap(Source: ULONGLONG): ULONGLONG;
asm
  {$ifdef cpu386}
    mov   EAX, [ESP+0Ch]  // Get the high part of the ULONGLONG into EAX
    mov   EDX, [ESP+08h]  // Get the low part of the ULONGLONG into EDX
  // This is not written as mnemonics to be compatible with D4!
    db    0Fh, 0C8h       // "bswap EAX" can only be executed on 486+!!!
    db    0Fh, 0CAh       // "bswap EDX" can only be executed on 486+!!!
  // High part returns in EDX, low part in EAX
  {$endif}
  {$ifdef cpux86_64}
    MOV   RCX,RAX
    BSWAP EAX
  {$endif cpux86_64}
end;

// Resembles the RtlValidateUnicodeString() function available from Windows XP
// on exactly as it is on this OS version, except for the calling convention.

function RtlpValidateUnicodeString(dwMustBeNull: DWORD; UnicodeString: PUNICODE_STRING): NTSTATUS;
begin
  result := STATUS_INVALID_PARAMETER;
  if (dwMustBeNull = 0) then
  begin
    result := STATUS_SUCCESS;
    if (Assigned(UnicodeString)) then
    begin
      result := STATUS_INVALID_PARAMETER;
      if ((UnicodeString^.Length mod 2 = 0) and (UnicodeString^.MaximumLength mod 2 = 0) and (UnicodeString^.Length <= UnicodeString^.MaximumLength)) then
        if ((UnicodeString^.Length > 0) and (UnicodeString^.MaximumLength > 0)) then
          if (Assigned(UnicodeString^.Buffer)) then
            result := STATUS_SUCCESS;
    end;
  end;
end;

// Resembles the RtlValidateUnicodeString() function available from Windows XP
// on, but does not require the first parameter which anyway must be zero.

function RtlpValidateUnicodeString2(UnicodeString: PUNICODE_STRING): NTSTATUS;
begin
  result := STATUS_SUCCESS;
  if (Assigned(UnicodeString)) then
  begin
    result := STATUS_INVALID_PARAMETER;
    if ((UnicodeString^.Length mod 2 = 0) and (UnicodeString^.MaximumLength mod 2 = 0) and (UnicodeString^.Length <= UnicodeString^.MaximumLength)) then
      if ((UnicodeString^.Length > 0) and (UnicodeString^.MaximumLength > 0)) then
        if (Assigned(UnicodeString^.Buffer)) then
          result := STATUS_SUCCESS;
  end;
end;


{$IFDEF RTDL}

/// Function types
type
  TFNCsrGetProcessId = function (): DWORD; stdcall;
  TFNDbgQueryDebugFilterState = function (ComponentId: ULONG; Level: ULONG): NTSTATUS; stdcall;
  TFNDbgSetDebugFilterState = function (ComponentId: ULONG; Level: ULONG; State: BOOLEAN): NTSTATUS; stdcall;
  TFNKiRaiseUserExceptionDispatcher = function (): ULONG; stdcall;
  TFNLdrAccessResource = function (hModule: HANDLE; ResourceDataEntry: PIMAGE_RESOURCE_DATA_ENTRY; Address: PPVOID; dwSize: PULONG): NTSTATUS; stdcall;
  TFNLdrAlternateResourcesEnabled = function (): BOOLEAN; stdcall;
  TFNLdrDisableThreadCalloutsForDll = function (hModule: HANDLE): NTSTATUS; stdcall;
  TFNLdrGetDllHandle = function (pwPath: PWORD; pReserved: PVOID; pusPath: PUNICODE_STRING; var phModule: HANDLE): NTSTATUS; stdcall;
  TFNLdrGetProcedureAddress = function (hModule: HANDLE; dwOrdinal: ULONG; psName: PSTRING; var pProcedure: PVOID): NTSTATUS; stdcall;
  TFNLdrLoadDll = function (pwPath: PWORD; pdwFlags: PDWORD; pusPath: PUNICODE_STRING; var phModule: HANDLE): NTSTATUS; stdcall;
  TFNLdrQueryImageFileExecutionOptions = function (pusImagePath: PUNICODE_STRING; pwOptionName: PWORD; dwRequestedType: DWORD; pData: PVOID; dwSize: DWORD; pdwSize: PDWORD): NTSTATUS; stdcall;
  TFNLdrQueryProcessModuleInformation = function (psmi: PSYSTEM_MODULE_INFORMATION; dwSize: DWORD; pdwSize: PDWORD): NTSTATUS; stdcall;
  TFNLdrShutdownProcess = procedure(); stdcall;
  TFNLdrShutdownThread = procedure(); stdcall;
  TFNLdrUnloadDll = function (hModule: HANDLE): NTSTATUS; stdcall;
  TFNNtAcceptConnectPort = function (PortHandle: PHANDLE; PortIdentifier: ULONG; Message: PPORT_MESSAGE; Accept: BOOLEAN; WriteSection: PPORT_SECTION_WRITE; ReadSection: PPORT_SECTION_READ): NTSTATUS; stdcall;
  TFNNtAccessCheck = function (SecurityDescriptor: PSECURITY_DESCRIPTOR; TokenHandle: HANDLE; DesiredAccess: ACCESS_MASK; GenericMapping: PGENERIC_MAPPING; PrivilegeSet: PPRIVILEGE_SET; PrivilegeSetLength: PULONG;
    GrantedAccess: PACCESS_MASK; AccessStatus: PBOOLEAN): NTSTATUS; stdcall;
  TFNNtAccessCheckAndAuditAlarm = function (SubsystemName: PUNICODE_STRING; HandleId: PVOID; ObjectTypeName: PUNICODE_STRING; ObjectName: PUNICODE_STRING; SecurityDescriptor: PSECURITY_DESCRIPTOR; DesiredAccess: ACCESS_MASK;
    GenericMapping: PGENERIC_MAPPING; ObjectCreation: BOOLEAN; GrantedAccess: PACCESS_MASK; AccessStatus: PBOOLEAN; GenerateOnClose: PBOOLEAN): NTSTATUS; stdcall;
  TFNNtAccessCheckByType = function (SecurityDescriptor: PSECURITY_DESCRIPTOR; PrincipalSelfSid: PSID; TokenHandle: HANDLE; DesiredAccess: ACCESS_MASK; ObjectTypeList: POBJECT_TYPE_LIST; ObjectTypeListLength: ULONG;
    GenericMapping: PGENERIC_MAPPING; PrivilegeSet: PPRIVILEGE_SET; PrivilegeSetLength: PULONG; GrantedAccess: PACCESS_MASK; AccessStatus: PULONG): NTSTATUS; stdcall;
  TFNNtAccessCheckByTypeAndAuditAlarm = function (SubsystemName: PUNICODE_STRING; HandleId: PVOID; ObjectTypeName: PUNICODE_STRING; ObjectName: PUNICODE_STRING; SecurityDescriptor: PSECURITY_DESCRIPTOR; PrincipalSelfSid: PSID;
    DesiredAccess: ACCESS_MASK; AuditType: AUDIT_EVENT_TYPE; Flags: ULONG; ObjectTypeList: POBJECT_TYPE_LIST; ObjectTypeListLength: ULONG; GenericMapping: PGENERIC_MAPPING; ObjectCreation: BOOLEAN; GrantedAccess: PACCESS_MASK;
    AccessStatus: PULONG; GenerateOnClose: PBOOLEAN): NTSTATUS; stdcall;
  TFNNtAccessCheckByTypeResultList = function (SecurityDescriptor: PSECURITY_DESCRIPTOR; PrincipalSelfSid: PSID; TokenHandle: HANDLE; DesiredAccess: ACCESS_MASK; ObjectTypeList: POBJECT_TYPE_LIST; ObjectTypeListLength: ULONG;
    GenericMapping: PGENERIC_MAPPING; PrivilegeSet: PPRIVILEGE_SET; PrivilegeSetLength: PULONG; GrantedAccessList: PACCESS_MASK; AccessStatusList: PULONG): NTSTATUS; stdcall;
  TFNNtAccessCheckByTypeResultListAndAuditAlarm = function (SubsystemName: PUNICODE_STRING; HandleId: PVOID; ObjectTypeName: PUNICODE_STRING; ObjectName: PUNICODE_STRING; SecurityDescriptor: PSECURITY_DESCRIPTOR; PrincipalSelfSid: PSID;
    DesiredAccess: ACCESS_MASK; AuditType: AUDIT_EVENT_TYPE; Flags: ULONG; ObjectTypeList: POBJECT_TYPE_LIST; ObjectTypeListLength: ULONG; GenericMapping: PGENERIC_MAPPING; ObjectCreation: BOOLEAN; GrantedAccessList: PACCESS_MASK;
    AccessStatusList: PULONG; GenerateOnClose: PULONG): NTSTATUS; stdcall;
  TFNNtAccessCheckByTypeResultListAndAuditAlarmByHandle = function (SubsystemName: PUNICODE_STRING; HandleId: PVOID; TokenHandle: HANDLE; ObjectTypeName: PUNICODE_STRING; ObjectName: PUNICODE_STRING;
    SecurityDescriptor: PSECURITY_DESCRIPTOR; PrincipalSelfSid: PSID; DesiredAccess: ACCESS_MASK; AuditType: AUDIT_EVENT_TYPE; Flags: ULONG; ObjectTypeList: POBJECT_TYPE_LIST; ObjectTypeListLength: ULONG; GenericMapping: PGENERIC_MAPPING;
    ObjectCreation: BOOLEAN; GrantedAccessList: PACCESS_MASK; AccessStatusList: PULONG; GenerateOnClose: PULONG): NTSTATUS; stdcall;
  TFNNtAddAtom = function (Str: PWSTR; StringLength: ULONG; Atom: PUSHORT): NTSTATUS; stdcall;
  TFNNtAdjustGroupsToken = function (TokenHandle: HANDLE; ResetToDefault: BOOLEAN; NewState: PTOKEN_GROUPS; BufferLength: ULONG; PreviousState: PTOKEN_GROUPS; ReturnLength: PULONG): NTSTATUS; stdcall;
  TFNNtAdjustPrivilegesToken = function (TokenHandle: HANDLE; DisableAllPrivileges: BOOLEAN; NewState: PTOKEN_PRIVILEGES; BufferLength: ULONG; PreviousState: PTOKEN_PRIVILEGES; ReturnLength: PULONG): NTSTATUS; stdcall;
  TFNNtAlertResumeThread = function (ThreadHandle: HANDLE; PreviousSuspendCount: PULONG): NTSTATUS; stdcall;
  TFNNtAlertThread = function (ThreadHandle: HANDLE): NTSTATUS; stdcall;
  TFNNtAllocateLocallyUniqueId = function (Luid: PLUID): NTSTATUS; stdcall;
  TFNNtAllocateUserPhysicalPages = function (ProcessHandle: HANDLE; NumberOfPages: PULONG; PageFrameNumbers: PULONG): NTSTATUS; stdcall;
  TFNNtAllocateUuids = function (UuidLastTimeAllocated: PLARGE_INTEGER; UuidDeltaTime: PULONG; UuidSequenceNumber: PULONG; UuidSeed: PUCHAR): NTSTATUS; stdcall;
  TFNNtAllocateVirtualMemory = function (ProcessHandle: HANDLE; BaseAddress: PPVOID; ZeroBits: ULONG; AllocationSize: PULONG; AllocationType: ULONG; Protect: ULONG): NTSTATUS; stdcall;
  TFNNtAreMappedFilesTheSame = function (Address1: PVOID; Address2: PVOID): NTSTATUS; stdcall;
  TFNNtAssignProcessToJobObject = function (JobHandle: HANDLE; ProcessHandle: HANDLE): NTSTATUS; stdcall;
  TFNNtCallbackReturn = function (Result_: PVOID; ResultLength: ULONG; Status: NTSTATUS): NTSTATUS; stdcall;
  TFNNtCancelDeviceWakeupRequest = function (DeviceHandle: HANDLE): NTSTATUS; stdcall;
  TFNNtCancelIoFile = function (FileHandle: HANDLE; IoStatusBlock: PIO_STATUS_BLOCK): NTSTATUS; stdcall;
  TFNNtCancelTimer = function (TimerHandle: HANDLE; PreviousState: PBOOLEAN): NTSTATUS; stdcall;
  TFNNtClearEvent = function (EventHandle: HANDLE): NTSTATUS; stdcall;
  TFNNtClose = function (Handle: HANDLE): NTSTATUS; stdcall;
  TFNNtCloseObjectAuditAlarm = function (SubsystemName: PUNICODE_STRING; HandleId: PVOID; GenerateOnClose: BOOLEAN): NTSTATUS; stdcall;
  TFNNtCompleteConnectPort = function (PortHandle: HANDLE): NTSTATUS; stdcall;
  TFNNtConnectPort = function (PortHandle: PHANDLE; PortName: PUNICODE_STRING; SecurityQos: PSECURITY_QUALITY_OF_SERVICE; WriteSection: PPORT_SECTION_WRITE; ReadSection: PPORT_SECTION_READ; MaxMessageSize: PULONG; ConnectData: PVOID;
    ConnectDataLength: PULONG): NTSTATUS; stdcall;
  TFNNtContinue = function (Context: PCONTEXT; TestAlert: BOOLEAN): NTSTATUS; stdcall;
  TFNNtCreateChannel = function (ChannelHandle: PHANDLE; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall;
  TFNNtCreateDirectoryObject = function (DirectoryHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall;
  TFNNtCreateEvent = function (EventHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; EventType: EVENT_TYPE; InitialState: BOOLEAN): NTSTATUS; stdcall;
  TFNNtCreateEventPair = function (EventPairHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall;
  TFNNtCreateFile = function (FileHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; IoStatusBlock: PIO_STATUS_BLOCK; AllocationSize: PLARGE_INTEGER; FileAttributes: ULONG; ShareAccess: ULONG;
    CreateDisposition: ULONG; CreateOptions: ULONG; EaBuffer: PVOID; EaLength: ULONG): NTSTATUS; stdcall;
  TFNNtCreateIoCompletion = function (IoCompletionHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; NumberOfConcurrentThreads: ULONG): NTSTATUS; stdcall;
  TFNNtCreateJobObject = function (JobHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall;
  TFNNtCreateKey = function (KeyHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; TitleIndex: ULONG; Class_: PUNICODE_STRING; CreateOptions: ULONG; Disposition: PULONG): NTSTATUS; stdcall;
  TFNNtCreateMailslotFile = function (FileHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; IoStatusBlock: PIO_STATUS_BLOCK; CreateOptions: ULONG; Unknown: ULONG; MaxMessageSize: ULONG;
    ReadTimeout: PLARGE_INTEGER): NTSTATUS; stdcall;
  TFNNtCreateMutant = function (MutantHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; InitialOwner: BOOLEAN): NTSTATUS; stdcall;
  TFNNtCreateNamedPipeFile = function (FileHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; IoStatusBlock: PIO_STATUS_BLOCK; ShareAccess: ULONG; CreateDisposition: ULONG; CreateOptions: ULONG;
    TypeMessage: BOOLEAN; ReadmodeMessage: BOOLEAN; Nonblocking: BOOLEAN; MaxInstances: ULONG; InBufferSize: ULONG; OutBufferSize: ULONG; DefaultTimeout: PLARGE_INTEGER): NTSTATUS; stdcall;
  TFNNtCreatePagingFile = function (FileName: PUNICODE_STRING; InitialSize: PULARGE_INTEGER; MaximumSize: PULARGE_INTEGER; Reserved: ULONG): NTSTATUS; stdcall;
  TFNNtCreatePort = function (PortHandle: PHANDLE; ObjectAttributes: POBJECT_ATTRIBUTES; MaxDataSize: ULONG; MaxMessageSize: ULONG; Reserved: ULONG): NTSTATUS; stdcall;
  TFNNtCreateProcess = function (ProcessHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; InheritFromProcessHandle: HANDLE; InheritHandles: BOOLEAN; SectionHandle: HANDLE; DebugPort: HANDLE;
    ExceptionPort: HANDLE): NTSTATUS; stdcall;
  TFNNtCreateProfile = function (ProfileHandle: PHANDLE; ProcessHandle: HANDLE; Base: PVOID; Size: ULONG; BucketShift: ULONG; Buffer: PULONG; BufferLength: ULONG; Source: KPROFILE_SOURCE; ProcessorMask: ULONG): NTSTATUS; stdcall;
  TFNNtCreateSection = function (SectionHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; SectionSize: PLARGE_INTEGER; Protect: ULONG; Attributes: ULONG; FileHandle: HANDLE): NTSTATUS; stdcall;
  TFNNtCreateSemaphore = function (SemaphoreHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; InitialCount: LONG; MaximumCount: LONG): NTSTATUS; stdcall;
  TFNNtCreateSymbolicLinkObject = function (SymbolicLinkHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; TargetName: PUNICODE_STRING): NTSTATUS; stdcall;
  TFNNtCreateThread = function (ThreadHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; ProcessHandle: HANDLE; ClientId: PCLIENT_ID; ThreadContext: PCONTEXT; UserStack: PUSER_STACK;
    CreateSuspended: BOOLEAN): NTSTATUS; stdcall;
  TFNNtCreateTimer = function (TimerHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; TimerType: TIMER_TYPE): NTSTATUS; stdcall;
  TFNNtCreateToken = function (TokenHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; Type_: TOKEN_TYPE; AuthenticationId: PLUID; ExpirationTime: PLARGE_INTEGER; User: PTOKEN_USER; Groups: PTOKEN_GROUPS;
    Privileges: PTOKEN_PRIVILEGES; Owner: PTOKEN_OWNER; PrimaryGroup: PTOKEN_PRIMARY_GROUP; DefaultDacl: PTOKEN_DEFAULT_DACL; Source: PTOKEN_SOURCE): NTSTATUS; stdcall;
  TFNNtCreateWaitablePort = function (PortHandle: PHANDLE; ObjectAttributes: POBJECT_ATTRIBUTES; MaxDataSize: ULONG; MaxMessageSize: ULONG; Reserved: ULONG): NTSTATUS; stdcall;
  TFNNtCurrentTeb = function (): PTEB; stdcall;
  TFNNtDebugActiveProcess = function (hProcess: HANDLE; hDebugObject: HANDLE): NTSTATUS; stdcall;
  TFNNtDelayExecution = function (Alertable: BOOLEAN; Interval: PLARGE_INTEGER): NTSTATUS; stdcall;
  TFNNtDeleteAtom = function (Atom: USHORT): NTSTATUS; stdcall;
  TFNNtDeleteFile = function (ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall;
  TFNNtDeleteKey = function (KeyHandle: HANDLE): NTSTATUS; stdcall;
  TFNNtDeleteObjectAuditAlarm = function (SubsystemName: PUNICODE_STRING; HandleId: PVOID; GenerateOnClose: BOOLEAN): NTSTATUS; stdcall;
  TFNNtDeleteValueKey = function (KeyHandle: HANDLE; ValueName: PUNICODE_STRING): NTSTATUS; stdcall;
  TFNNtDeviceIoControlFile = function (FileHandle: HANDLE; Event: HANDLE; ApcRoutine: PIO_APC_ROUTINE; ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; IoControlCode: ULONG; InputBuffer: PVOID; InputBufferLength: ULONG;
    OutputBuffer: PVOID; OutputBufferLength: ULONG): NTSTATUS; stdcall;
  TFNNtDisplayString = function (Str: PUNICODE_STRING): NTSTATUS; stdcall;
  TFNNtDuplicateObject = function (SourceProcessHandle: HANDLE; SourceHandle: HANDLE; TargetProcessHandle: HANDLE; TargetHandle: PHANDLE; DesiredAccess: ACCESS_MASK; Attributes: ULONG; Options: ULONG): NTSTATUS; stdcall;
  TFNNtDuplicateToken = function (ExistingTokenHandle: HANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; EffectiveOnly: BOOLEAN; TokenType: TOKEN_TYPE; NewTokenHandle: PHANDLE): NTSTATUS; stdcall;
  TFNNtEnumerateKey = function (KeyHandle: HANDLE; Index: ULONG; KeyInformationClass: KEY_INFORMATION_CLASS; KeyInformation: PVOID; KeyInformationLength: ULONG; ResultLength: PULONG): NTSTATUS; stdcall;
  TFNNtEnumerateValueKey = function (KeyHandle: HANDLE; Index: ULONG; KeyValueInformationClass: KEY_VALUE_INFORMATION_CLASS; KeyValueInformation: PVOID; KeyValueInformationLength: ULONG; ResultLength: PULONG): NTSTATUS; stdcall;
  TFNNtExtendSection = function (SectionHandle: HANDLE; SectionSize: PLARGE_INTEGER): NTSTATUS; stdcall;
  TFNNtFilterToken = function (ExistingTokenHandle: HANDLE; Flags: ULONG; SidsToDisable: PTOKEN_GROUPS; PrivilegesToDelete: PTOKEN_PRIVILEGES; SidsToRestricted: PTOKEN_GROUPS; NewTokenHandle: PHANDLE): NTSTATUS; stdcall;
  TFNNtFindAtom = function (Str: PWSTR; StringLength: ULONG; Atom: PUSHORT): NTSTATUS; stdcall;
  TFNNtFlushBuffersFile = function (FileHandle: HANDLE; IoStatusBlock: PIO_STATUS_BLOCK): NTSTATUS; stdcall;
  TFNNtFlushInstructionCache = function (ProcessHandle: HANDLE; BaseAddress: PVOID; FlushSize: ULONG): NTSTATUS; stdcall;
  TFNNtFlushKey = function (KeyHandle: HANDLE): NTSTATUS; stdcall;
  TFNNtFlushVirtualMemory = function (ProcessHandle: HANDLE; BaseAddress: PPVOID; FlushSize: PULONG; IoStatusBlock: PIO_STATUS_BLOCK): NTSTATUS; stdcall;
  TFNNtFlushWriteBuffer = function (): NTSTATUS; stdcall;
  TFNNtFreeUserPhysicalPages = function (ProcessHandle: HANDLE; NumberOfPages: PULONG; PageFrameNumbers: PULONG): NTSTATUS; stdcall;
  TFNNtFreeVirtualMemory = function (ProcessHandle: HANDLE; BaseAddress: PPVOID; FreeSize: PULONG; FreeType: ULONG): NTSTATUS; stdcall;
  TFNNtFsControlFile = function (FileHandle: HANDLE; Event: HANDLE; ApcRoutine: PIO_APC_ROUTINE; ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; FsControlCode: ULONG; InputBuffer: PVOID; InputBufferLength: ULONG; OutputBuffer: PVOID;
    OutputBufferLength: ULONG): NTSTATUS; stdcall;
  TFNNtGetContextThread = function (ThreadHandle: HANDLE; Context: PCONTEXT): NTSTATUS; stdcall;
  TFNNtGetCurrentProcessorNumber = function (): ULONG; stdcall;
  TFNNtGetDevicePowerState = function (DeviceHandle: HANDLE; DevicePowerState: PDEVICE_POWER_STATE): NTSTATUS; stdcall;
  TFNNtGetPlugPlayEvent = function (Reserved1: ULONG; Reserved2: ULONG; Buffer: PVOID; BufferLength: ULONG): NTSTATUS; stdcall;
  TFNNtGetTickCount = function (): ULONG; stdcall;
  TFNNtGetWriteWatch = function (ProcessHandle: HANDLE; Flags: ULONG; BaseAddress: PVOID; RegionSize: ULONG; Buffer: PULONG; BufferEntries: PULONG; Granularity: PULONG): NTSTATUS; stdcall;
  TFNNtImpersonateAnonymousToken = function (ThreadHandle: HANDLE): NTSTATUS; stdcall;
  TFNNtImpersonateClientOfPort = function (PortHandle: HANDLE; Message: PPORT_MESSAGE): NTSTATUS; stdcall;
  TFNNtImpersonateThread = function (ThreadHandle: HANDLE; TargetThreadHandle: HANDLE; SecurityQos: PSECURITY_QUALITY_OF_SERVICE): NTSTATUS; stdcall;
  TFNNtInitializeRegistry = function (Setup: BOOLEAN): NTSTATUS; stdcall;
  TFNNtInitiatePowerAction = function (SystemAction: POWER_ACTION; MinSystemState: SYSTEM_POWER_STATE; Flags: ULONG; Asynchronous: BOOLEAN): NTSTATUS; stdcall;
  TFNNtIsSystemResumeAutomatic = function (): BOOLEAN; stdcall;
  TFNNtListenChannel = function (x: PVOID; y: PVOID): NTSTATUS; stdcall;
  TFNNtListenPort = function (PortHandle: HANDLE; Message: PPORT_MESSAGE): NTSTATUS; stdcall;
  TFNNtLoadDriver = function (DriverServiceName: PUNICODE_STRING): NTSTATUS; stdcall;
  TFNNtLoadKey = function (KeyObjectAttributes: POBJECT_ATTRIBUTES; FileObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall;
  TFNNtLoadKey2 = function (KeyObjectAttributes: POBJECT_ATTRIBUTES; FileObjectAttributes: POBJECT_ATTRIBUTES; Flags: ULONG): NTSTATUS; stdcall;
  TFNNtLockFile = function (FileHandle: HANDLE; Event: HANDLE; ApcRoutine: PIO_APC_ROUTINE; ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; LockOffset: PULARGE_INTEGER; LockLength: PULARGE_INTEGER; Key: ULONG; FailImmediately: BOOLEAN;
    ExclusiveLock: BOOLEAN): NTSTATUS; stdcall;
  TFNNtLockVirtualMemory = function (ProcessHandle: HANDLE; BaseAddress: PPVOID; LockSize: PULONG; LockType: ULONG): NTSTATUS; stdcall;
  TFNNtMakePermanentObject = function (Handle: HANDLE): NTSTATUS; stdcall;
  TFNNtMakeTemporaryObject = function (Handle: HANDLE): NTSTATUS; stdcall;
  TFNNtMapUserPhysicalPages = function (BaseAddress: PVOID; NumberOfPages: PULONG; PageFrameNumbers: PULONG): NTSTATUS; stdcall;
  TFNNtMapUserPhysicalPagesScatter = function (BaseAddresses: PPVOID; NumberOfPages: PULONG; PageFrameNumbers: PULONG): NTSTATUS; stdcall;
  TFNNtMapViewOfSection = function (SectionHandle: HANDLE; ProcessHandle: HANDLE; BaseAddress: PPVOID; ZeroBits: ULONG; CommitSize: ULONG; SectionOffset: PLARGE_INTEGER; ViewSize: PULONG; InheritDisposition: SECTION_INHERIT;
    AllocationType: ULONG; Protect: ULONG): NTSTATUS; stdcall;
  TFNNtNotifyChangeDirectoryFile = function (FileHandle: HANDLE; Event: HANDLE; ApcRoutine: PIO_APC_ROUTINE; ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; Buffer: PFILE_NOTIFY_INFORMATION; BufferLength: ULONG; NotifyFilter: ULONG;
    WatchSubtree: BOOLEAN): NTSTATUS; stdcall;
  TFNNtNotifyChangeKey = function (KeyHandle: HANDLE; EventHandle: HANDLE; ApcRoutine: PIO_APC_ROUTINE; ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; NotifyFilter: ULONG; WatchSubtree: BOOLEAN; Buffer: PVOID; BufferLength: ULONG;
    Asynchronous: BOOLEAN): NTSTATUS; stdcall;
  TFNNtNotifyChangeMultipleKeys = function (KeyHandle: HANDLE; Flags: ULONG; KeyObjectAttributes: POBJECT_ATTRIBUTES; EventHandle: HANDLE; ApcRoutine: PIO_APC_ROUTINE; ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; NotifyFilter: ULONG;
    WatchSubtree: BOOLEAN; Buffer: PVOID; BufferLength: ULONG; Asynchronous: BOOLEAN): NTSTATUS; stdcall;
  TFNNtOpenChannel = function (ChannelHandle: PHANDLE; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall;
  TFNNtOpenDirectoryObject = function (DirectoryHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall;
  TFNNtOpenEvent = function (EventHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall;
  TFNNtOpenEventPair = function (EventPairHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall;
  TFNNtOpenFile = function (FileHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; IoStatusBlock: PIO_STATUS_BLOCK; ShareAccess: ULONG; OpenOptions: ULONG): NTSTATUS; stdcall;
  TFNNtOpenIoCompletion = function (IoCompletionHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall;
  TFNNtOpenJobObject = function (JobHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall;
  TFNNtOpenKey = function (KeyHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall;
  TFNNtOpenMutant = function (MutantHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall;
  TFNNtOpenObjectAuditAlarm = function (SubsystemName: PUNICODE_STRING; HandleId: PPVOID; ObjectTypeName: PUNICODE_STRING; ObjectName: PUNICODE_STRING; SecurityDescriptor: PSECURITY_DESCRIPTOR; TokenHandle: HANDLE;
    DesiredAccess: ACCESS_MASK; GrantedAccess: ACCESS_MASK; Privileges: PPRIVILEGE_SET; ObjectCreation: BOOLEAN; AccessGranted: BOOLEAN; GenerateOnClose: PBOOLEAN): NTSTATUS; stdcall;
  TFNNtOpenProcess = function (ProcessHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; ClientId: PCLIENT_ID): NTSTATUS; stdcall;
  TFNNtOpenProcessToken = function (ProcessHandle: HANDLE; DesiredAccess: ACCESS_MASK; TokenHandle: PHANDLE): NTSTATUS; stdcall;
  TFNNtOpenSection = function (SectionHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall;
  TFNNtOpenSemaphore = function (SemaphoreHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall;
  TFNNtOpenSymbolicLinkObject = function (SymbolicLinkHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall;
  TFNNtOpenThread = function (ThreadHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; ClientId: PCLIENT_ID): NTSTATUS; stdcall;
  TFNNtOpenThreadToken = function (ThreadHandle: HANDLE; DesiredAccess: ACCESS_MASK; OpenAsSelf: BOOLEAN; TokenHandle: PHANDLE): NTSTATUS; stdcall;
  TFNNtOpenTimer = function (TimerHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall;
  TFNNtPlugPlayControl = function (ControlCode: ULONG; Buffer: PVOID; BufferLength: ULONG): NTSTATUS; stdcall;
  TFNNtPowerInformation = function (PowerInformationLevel: POWER_INFORMATION_LEVEL; InputBuffer: PVOID; InputBufferLength: ULONG; OutputBuffer: PVOID; OutputBufferLength: ULONG): NTSTATUS; stdcall;
  TFNNtPrivilegeCheck = function (TokenHandle: HANDLE; RequiredPrivileges: PPRIVILEGE_SET; Result_: PBOOLEAN): NTSTATUS; stdcall;
  TFNNtPrivilegeObjectAuditAlarm = function (SubsystemName: PUNICODE_STRING; HandleId: PVOID; TokenHandle: HANDLE; DesiredAccess: ACCESS_MASK; Privileges: PPRIVILEGE_SET; AccessGranted: BOOLEAN): NTSTATUS; stdcall;
  TFNNtPrivilegedServiceAuditAlarm = function (SubsystemName: PUNICODE_STRING; ServiceName: PUNICODE_STRING; TokenHandle: HANDLE; Privileges: PPRIVILEGE_SET; AccessGranted: BOOLEAN): NTSTATUS; stdcall;
  TFNNtProtectVirtualMemory = function (ProcessHandle: HANDLE; BaseAddress: PPVOID; ProtectSize: PULONG; NewProtect: ULONG; OldProtect: PULONG): NTSTATUS; stdcall;
  TFNNtPulseEvent = function (EventHandle: HANDLE; PreviousState: PULONG): NTSTATUS; stdcall;
  TFNNtQueryAttributesFile = function (ObjectAttributes: POBJECT_ATTRIBUTES; FileInformation: PFILE_BASIC_INFORMATION): NTSTATUS; stdcall;
  TFNNtQueryDefaultLocale = function (ThreadOrSystem: BOOLEAN; Locale: PLCID): NTSTATUS; stdcall;
  TFNNtQueryDefaultUILanguage = function (LanguageId: PLANGID): NTSTATUS; stdcall;
  TFNNtQueryDirectoryFile = function (FileHandle: HANDLE; Event: HANDLE; ApcRoutine: PIO_APC_ROUTINE; ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; FileInformation: PVOID; FileInformationLength: ULONG;
    FileInformationClass: FILE_INFORMATION_CLASS; ReturnSingleEntry: BOOLEAN; FileName: PUNICODE_STRING; RestartScan: BOOLEAN): NTSTATUS; stdcall;
  TFNNtQueryDirectoryObject = function (DirectoryHandle: HANDLE; Buffer: PVOID; BufferLength: ULONG; ReturnSingleEntry: BOOLEAN; RestartScan: BOOLEAN; Context: PULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
  TFNNtQueryEaFile = function (FileHandle: HANDLE; IoStatusBlock: PIO_STATUS_BLOCK; Buffer: PFILE_FULL_EA_INFORMATION; BufferLength: ULONG; ReturnSingleEntry: BOOLEAN; EaList: PFILE_GET_EA_INFORMATION; EaListLength: ULONG; EaIndex: PULONG;
    RestartScan: BOOLEAN): NTSTATUS; stdcall;
  TFNNtQueryEvent = function (EventHandle: HANDLE; EventInformationClass: EVENT_INFORMATION_CLASS; EventInformation: PVOID; EventInformationLength: ULONG; ResultLength: PULONG): NTSTATUS; stdcall;
  TFNNtQueryFullAttributesFile = function (ObjectAttributes: POBJECT_ATTRIBUTES; FileInformation: PFILE_NETWORK_OPEN_INFORMATION): NTSTATUS; stdcall;
  TFNNtQueryInformationAtom = function (Atom: USHORT; AtomInformationClass: ATOM_INFORMATION_CLASS; AtomInformation: PVOID; AtomInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
  TFNNtQueryInformationFile = function (FileHandle: HANDLE; IoStatusBlock: PIO_STATUS_BLOCK; FileInformation: PVOID; FileInformationLength: ULONG; FileInformationClass: FILE_INFORMATION_CLASS): NTSTATUS; stdcall;
  TFNNtQueryInformationJobObject = function (JobHandle: HANDLE; JobInformationClass: JOBOBJECTINFOCLASS; JobInformation: PVOID; JobInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
  TFNNtQueryInformationPort = function (PortHandle: HANDLE; PortInformationClass: PORT_INFORMATION_CLASS; PortInformation: PVOID; PortInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
  TFNNtQueryInformationProcess = function (ProcessHandle: HANDLE; ProcessInformationClass: PROCESSINFOCLASS; ProcessInformation: PVOID; ProcessInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
  TFNNtQueryInformationThread = function (ThreadHandle: HANDLE; ThreadInformationClass: THREADINFOCLASS; ThreadInformation: PVOID; ThreadInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
  TFNNtQueryInformationToken = function (TokenHandle: HANDLE; TokenInformationClass: TOKEN_INFORMATION_CLASS; TokenInformation: PVOID; TokenInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
  TFNNtQueryInstallUILanguage = function (LanguageId: PLANGID): NTSTATUS; stdcall;
  TFNNtQueryIntervalProfile = function (Source: KPROFILE_SOURCE; Interval: PULONG): NTSTATUS; stdcall;
  TFNNtQueryIoCompletion = function (IoCompletionHandle: HANDLE; IoCompletionInformationClass: IO_COMPLETION_INFORMATION_CLASS; IoCompletionInformation: PVOID; IoCompletionInformationLength: ULONG; ResultLength: PULONG): NTSTATUS; stdcall;
  TFNNtQueryKey = function (KeyHandle: HANDLE; KeyInformationClass: KEY_INFORMATION_CLASS; KeyInformation: PVOID; KeyInformationLength: ULONG; ResultLength: PULONG): NTSTATUS; stdcall;
  TFNNtQueryMultipleValueKey = function (KeyHandle: HANDLE; ValueList: PKEY_VALUE_ENTRY; NumberOfValues: ULONG; Buffer: PVOID; Length: PULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
  TFNNtQueryMutant = function (MutantHandle: HANDLE; MutantInformationClass: MUTANT_INFORMATION_CLASS; MutantInformation: PVOID; MutantInformationLength: ULONG; ResultLength: PULONG): NTSTATUS; stdcall;
  TFNNtQueryObject = function (ObjectHandle: HANDLE; ObjectInformationClass: OBJECT_INFORMATION_CLASS; ObjectInformation: PVOID; ObjectInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
  TFNNtQueryOpenSubKeys = function (KeyObjectAttributes: POBJECT_ATTRIBUTES; NumberOfKey: PULONG): NTSTATUS; stdcall;
  TFNNtQueryPerformanceCounter = function (PerformanceCount: PLARGE_INTEGER; PerformanceFrequency: PLARGE_INTEGER): NTSTATUS; stdcall;
  TFNNtQueryPortInformationProcess = function (): ULONG; stdcall;
  TFNNtQueryQuotaInformationFile = function (FileHandle: HANDLE; IoStatusBlock: PIO_STATUS_BLOCK; Buffer: PFILE_USER_QUOTA_INFORMATION; BufferLength: ULONG; ReturnSingleEntry: BOOLEAN; QuotaList: PFILE_QUOTA_LIST_INFORMATION;
    QuotaListLength: ULONG; ResumeSid: PSID; RestartScan: BOOLEAN): NTSTATUS; stdcall;
  TFNNtQuerySection = function (SectionHandle: HANDLE; SectionInformationClass: SECTION_INFORMATION_CLASS; SectionInformation: PVOID; SectionInformationLength: ULONG; ResultLength: PULONG): NTSTATUS; stdcall;
  TFNNtQuerySecurityObject = function (Handle: HANDLE; RequestedInformation: SECURITY_INFORMATION; SecurityDescriptor: PSECURITY_DESCRIPTOR; SecurityDescriptorLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
  TFNNtQuerySemaphore = function (SemaphoreHandle: HANDLE; SemaphoreInformationClass: SEMAPHORE_INFORMATION_CLASS; SemaphoreInformation: PVOID; SemaphoreInformationLength: ULONG; ResultLength: PULONG): NTSTATUS; stdcall;
  TFNNtQuerySymbolicLinkObject = function (SymbolicLinkHandle: HANDLE; TargetName: PUNICODE_STRING; ReturnLength: PULONG): NTSTATUS; stdcall;
  TFNNtQuerySystemEnvironmentValue = function (Name: PUNICODE_STRING; Value: PVOID; ValueLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
  TFNNtQuerySystemInformation = function (SystemInformationClass: SYSTEM_INFORMATION_CLASS; SystemInformation: PVOID; SystemInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
  TFNNtQuerySystemTime = function (CurrentTime: PLARGE_INTEGER): NTSTATUS; stdcall;
  TFNNtQueryTimer = function (TimerHandle: HANDLE; TimerInformationClass: TIMER_INFORMATION_CLASS; TimerInformation: PVOID; TimerInformationLength: ULONG; ResultLength: PULONG): NTSTATUS; stdcall;
  TFNNtQueryTimerResolution = function (CoarsestResolution: PULONG; FinestResolution: PULONG; ActualResolution: PULONG): NTSTATUS; stdcall;
  TFNNtQueryValueKey = function (KeyHandle: HANDLE; ValueName: PUNICODE_STRING; KeyValueInformationClass: KEY_VALUE_INFORMATION_CLASS; KeyValueInformation: PVOID; KeyValueInformationLength: ULONG; ResultLength: PULONG): NTSTATUS; stdcall;
  TFNNtQueryVirtualMemory = function (ProcessHandle: HANDLE; BaseAddress: PVOID; MemoryInformationClass: MEMORY_INFORMATION_CLASS; MemoryInformation: PVOID; MemoryInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
  TFNNtQueryVolumeInformationFile = function (FileHandle: HANDLE; IoStatusBlock: PIO_STATUS_BLOCK; VolumeInformation: PVOID; VolumeInformationLength: ULONG; VolumeInformationClass: FS_INFORMATION_CLASS): NTSTATUS; stdcall;
  TFNNtQueueApcThread = function (ThreadHandle: HANDLE; ApcRoutine: PKNORMAL_ROUTINE; ApcContext: PVOID; Argument1: PVOID; Argument2: PVOID): NTSTATUS; stdcall;
  TFNNtRaiseException = function (ExceptionRecord: PEXCEPTION_RECORD; Context: PCONTEXT; SearchFrames: BOOLEAN): NTSTATUS; stdcall;
  TFNNtRaiseHardError = function (Status: NTSTATUS; NumberOfArguments: ULONG; StringArgumentsMask: ULONG; Arguments: PULONG; MessageBoxType: ULONG; MessageBoxResult: PULONG): NTSTATUS; stdcall;
  TFNNtReadFile = function (FileHandle: HANDLE; Event: HANDLE; ApcRoutine: PIO_APC_ROUTINE; ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; Buffer: PVOID; Length: ULONG; ByteOffset: PLARGE_INTEGER; Key: PULONG): NTSTATUS; stdcall;
  TFNNtReadFileScatter = function (FileHandle: HANDLE; Event: HANDLE; ApcRoutine: PIO_APC_ROUTINE; ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; Buffer: PFILE_SEGMENT_ELEMENT; Length: ULONG; ByteOffset: PLARGE_INTEGER;
    Key: PULONG): NTSTATUS; stdcall;
  TFNNtReadRequestData = function (PortHandle: HANDLE; Message: PPORT_MESSAGE; Index: ULONG; Buffer: PVOID; BufferLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
  TFNNtReadVirtualMemory = function (ProcessHandle: HANDLE; BaseAddress: PVOID; Buffer: PVOID; BufferLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
  TFNNtRegisterThreadTerminatePort = function (PortHandle: HANDLE): NTSTATUS; stdcall;
  TFNNtReleaseMutant = function (MutantHandle: HANDLE; PreviousState: PULONG): NTSTATUS; stdcall;
  TFNNtReleaseSemaphore = function (SemaphoreHandle: HANDLE; ReleaseCount: LONG; PreviousCount: PLONG): NTSTATUS; stdcall;
  TFNNtRemoveIoCompletion = function (IoCompletionHandle: HANDLE; CompletionKey: PULONG; CompletionValue: PULONG; IoStatusBlock: PIO_STATUS_BLOCK; Timeout: PLARGE_INTEGER): NTSTATUS; stdcall;
  TFNNtRemoveProcessDebug = function (hProcess: HANDLE; hDebugObject: HANDLE): NTSTATUS; stdcall;
  TFNNtReplaceKey = function (NewFileObjectAttributes: POBJECT_ATTRIBUTES; KeyHandle: HANDLE; OldFileObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall;
  TFNNtReplyPort = function (PortHandle: HANDLE; ReplyMessage: PPORT_MESSAGE): NTSTATUS; stdcall;
  TFNNtReplyWaitReceivePort = function (PortHandle: HANDLE; PortIdentifier: PULONG; ReplyMessage: PPORT_MESSAGE; Message: PPORT_MESSAGE): NTSTATUS; stdcall;
  TFNNtReplyWaitReceivePortEx = function (PortHandle: HANDLE; PortIdentifier: PULONG; ReplyMessage: PPORT_MESSAGE; Message: PPORT_MESSAGE; Timeout: PLARGE_INTEGER): NTSTATUS; stdcall;
  TFNNtReplyWaitReplyPort = function (PortHandle: HANDLE; ReplyMessage: PPORT_MESSAGE): NTSTATUS; stdcall;
  TFNNtReplyWaitSendChannel = function (x: PVOID; y: PVOID; z: PVOID): NTSTATUS; stdcall;
  TFNNtRequestDeviceWakeup = function (DeviceHandle: HANDLE): NTSTATUS; stdcall;
  TFNNtRequestPort = function (PortHandle: HANDLE; RequestMessage: PPORT_MESSAGE): NTSTATUS; stdcall;
  TFNNtRequestWaitReplyPort = function (PortHandle: HANDLE; RequestMessage: PPORT_MESSAGE; ReplyMessage: PPORT_MESSAGE): NTSTATUS; stdcall;
  TFNNtRequestWakeupLatency = function (Latency: LATENCY_TIME): NTSTATUS; stdcall;
  TFNNtResetEvent = function (EventHandle: HANDLE; PreviousState: PULONG): NTSTATUS; stdcall;
  TFNNtResetWriteWatch = function (ProcessHandle: HANDLE; BaseAddress: PVOID; RegionSize: ULONG): NTSTATUS; stdcall;
  TFNNtRestoreKey = function (KeyHandle: HANDLE; FileHandle: HANDLE; Flags: ULONG): NTSTATUS; stdcall;
  TFNNtResumeProcess = function (hProcess: HANDLE): NTSTATUS; stdcall;
  TFNNtResumeThread = function (hThread: HANDLE; dwResumeCount: PULONG): NTSTATUS; stdcall;
  TFNNtSaveKey = function (KeyHandle: HANDLE; FileHandle: HANDLE): NTSTATUS; stdcall;
  TFNNtSaveKeyEx = function (KeyHandle: HANDLE; FileHandle: HANDLE; Flags: DWORD): NTSTATUS; stdcall;
  TFNNtSaveMergedKeys = function (KeyHandle1: HANDLE; KeyHandle2: HANDLE; FileHandle: HANDLE): NTSTATUS; stdcall;
  TFNNtSecureConnectPort = function (PortHandle: PHANDLE; PortName: PUNICODE_STRING; SecurityQos: PSECURITY_QUALITY_OF_SERVICE; WriteSection: PPORT_SECTION_WRITE; ServerSid: PSID; ReadSection: PPORT_SECTION_READ; MaxMessageSize: PULONG;
    ConnectData: PVOID; ConnectDataLength: PULONG): NTSTATUS; stdcall;
  TFNNtSendWaitReplyChannel = function (x: PVOID; y: PVOID; z: PVOID; z2: PVOID): NTSTATUS; stdcall;
  TFNNtSetContextChannel = function (x: PVOID): NTSTATUS; stdcall;
  TFNNtSetContextThread = function (ThreadHandle: HANDLE; Context: PCONTEXT): NTSTATUS; stdcall;
  TFNNtSetDefaultHardErrorPort = function (PortHandle: HANDLE): NTSTATUS; stdcall;
  TFNNtSetDefaultLocale = function (ThreadOrSystem: BOOLEAN; Locale: LCID): NTSTATUS; stdcall;
  TFNNtSetDefaultUILanguage = function (LanguageId: LANGID): NTSTATUS; stdcall;
  TFNNtSetEaFile = function (FileHandle: HANDLE; IoStatusBlock: PIO_STATUS_BLOCK; Buffer: PFILE_FULL_EA_INFORMATION; BufferLength: ULONG): NTSTATUS; stdcall;
  TFNNtSetEvent = function (EventHandle: HANDLE; PreviousState: PULONG): NTSTATUS; stdcall;
  TFNNtSetHighEventPair = function (EventPairHandle: HANDLE): NTSTATUS; stdcall;
  TFNNtSetHighWaitLowEventPair = function (EventPairHandle: HANDLE): NTSTATUS; stdcall;
  TFNNtSetHighWaitLowThread = function (): NTSTATUS; stdcall;
  TFNNtSetInformationFile = function (FileHandle: HANDLE; IoStatusBlock: PIO_STATUS_BLOCK; FileInformation: PVOID; FileInformationLength: ULONG; FileInformationClass: FILE_INFORMATION_CLASS): NTSTATUS; stdcall;
  TFNNtSetInformationJobObject = function (JobHandle: HANDLE; JobInformationClass: JOBOBJECTINFOCLASS; JobInformation: PVOID; JobInformationLength: ULONG): NTSTATUS; stdcall;
  TFNNtSetInformationKey = function (KeyHandle: HANDLE; KeyInformationClass: KEY_SET_INFORMATION_CLASS; KeyInformation: PVOID; KeyInformationLength: ULONG): NTSTATUS; stdcall;
  TFNNtSetInformationObject = function (ObjectHandle: HANDLE; ObjectInformationClass: OBJECT_INFORMATION_CLASS; ObjectInformation: PVOID; ObjectInformationLength: ULONG): NTSTATUS; stdcall;
  TFNNtSetInformationProcess = function (ProcessHandle: HANDLE; ProcessInformationClass: PROCESSINFOCLASS; ProcessInformation: PVOID; ProcessInformationLength: ULONG): NTSTATUS; stdcall;
  TFNNtSetInformationThread = function (ThreadHandle: HANDLE; ThreadInformationClass: THREADINFOCLASS; ThreadInformation: PVOID; ThreadInformationLength: ULONG): NTSTATUS; stdcall;
  TFNNtSetInformationToken = function (TokenHandle: HANDLE; TokenInformationClass: TOKEN_INFORMATION_CLASS; TokenInformation: PVOID; TokenInformationLength: ULONG): NTSTATUS; stdcall;
  TFNNtSetIntervalProfile = function (Interval: ULONG; Source: KPROFILE_SOURCE): NTSTATUS; stdcall;
  TFNNtSetIoCompletion = function (IoCompletionHandle: HANDLE; CompletionKey: ULONG; CompletionValue: ULONG; Status: NTSTATUS; Information: ULONG): NTSTATUS; stdcall;
  TFNNtSetLdtEntries = function (Selector1: ULONG; LdtEntry1: LDT_ENTRY; Selector2: ULONG; LdtEntry2: LDT_ENTRY): NTSTATUS; stdcall;
  TFNNtSetLowEventPair = function (EventPairHandle: HANDLE): NTSTATUS; stdcall;
  TFNNtSetLowWaitHighEventPair = function (EventPairHandle: HANDLE): NTSTATUS; stdcall;
  TFNNtSetLowWaitHighThread = function (): NTSTATUS; stdcall;
  TFNNtSetQuotaInformationFile = function (FileHandle: HANDLE; IoStatusBlock: PIO_STATUS_BLOCK; Buffer: PFILE_USER_QUOTA_INFORMATION; BufferLength: ULONG): NTSTATUS; stdcall;
  TFNNtSetSecurityObject = function (Handle: HANDLE; SecurityInformation: SECURITY_INFORMATION; SecurityDescriptor: PSECURITY_DESCRIPTOR): NTSTATUS; stdcall;
  TFNNtSetSystemEnvironmentValue = function (Name: PUNICODE_STRING; Value: PUNICODE_STRING): NTSTATUS; stdcall;
  TFNNtSetSystemInformation = function (SystemInformationClass: SYSTEM_INFORMATION_CLASS; SystemInformation: PVOID; SystemInformationLength: ULONG): NTSTATUS; stdcall;
  TFNNtSetSystemPowerState = function (SystemAction: POWER_ACTION; MinSystemState: SYSTEM_POWER_STATE; Flags: ULONG): NTSTATUS; stdcall;
  TFNNtSetSystemTime = function (NewTime: PLARGE_INTEGER; OldTime: PLARGE_INTEGER): NTSTATUS; stdcall;
  TFNNtSetThreadExecutionState = function (ExecutionState: EXECUTION_STATE; PreviousExecutionState: PEXECUTION_STATE): NTSTATUS; stdcall;
  TFNNtSetTimer = function (TimerHandle: HANDLE; DueTime: PLARGE_INTEGER; TimerApcRoutine: PTIMER_APC_ROUTINE; TimerContext: PVOID; Resume: BOOLEAN; Period: LONG; PreviousState: PBOOLEAN): NTSTATUS; stdcall;
  TFNNtSetTimerResolution = function (RequestedResolution: ULONG; Set_: BOOLEAN; ActualResolution: PULONG): NTSTATUS; stdcall;
  TFNNtSetUuidSeed = function (UuidSeed: PUCHAR): NTSTATUS; stdcall;
  TFNNtSetValueKey = function (KeyHandle: HANDLE; ValueName: PUNICODE_STRING; TitleIndex: ULONG; Type_: ULONG; Data: PVOID; DataSize: ULONG): NTSTATUS; stdcall;
  TFNNtSetVolumeInformationFile = function (FileHandle: HANDLE; IoStatusBlock: PIO_STATUS_BLOCK; Buffer: PVOID; BufferLength: ULONG; VolumeInformationClass: FS_INFORMATION_CLASS): NTSTATUS; stdcall;
  TFNNtShutdownSystem = function (Action: SHUTDOWN_ACTION): NTSTATUS; stdcall;
  TFNNtSignalAndWaitForSingleObject = function (HandleToSignal: HANDLE; HandleToWait: HANDLE; Alertable: BOOLEAN; Timeout: PLARGE_INTEGER): NTSTATUS; stdcall;
  TFNNtStartProfile = function (ProfileHandle: HANDLE): NTSTATUS; stdcall;
  TFNNtStopProfile = function (ProfileHandle: HANDLE): NTSTATUS; stdcall;
  TFNNtSuspendProcess = function (hProcess: HANDLE): NTSTATUS; stdcall;
  TFNNtSuspendThread = function (hThread: HANDLE; dwLastResumeCount: PULONG): NTSTATUS; stdcall;
  TFNNtSystemDebugControl = function (ControlCode: DEBUG_CONTROL_CODE; InputBuffer: PVOID; InputBufferLength: ULONG; OutputBuffer: PVOID; OutputBufferLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
  TFNNtTerminateJobObject = function (JobHandle: HANDLE; ExitStatus: NTSTATUS): NTSTATUS; stdcall;
  TFNNtTerminateProcess = function (ProcessHandle: HANDLE; ExitStatus: NTSTATUS): NTSTATUS; stdcall;
  TFNNtTerminateThread = function (ThreadHandle: HANDLE; ExitStatus: NTSTATUS): NTSTATUS; stdcall;
  TFNNtTestAlert = function (): NTSTATUS; stdcall;
  TFNNtUnloadDriver = function (DriverServiceName: PUNICODE_STRING): NTSTATUS; stdcall;
  TFNNtUnloadKey = function (KeyObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall;
  TFNNtUnlockFile = function (FileHandle: HANDLE; IoStatusBlock: PIO_STATUS_BLOCK; LockOffset: PULARGE_INTEGER; LockLength: PULARGE_INTEGER; Key: ULONG): NTSTATUS; stdcall;
  TFNNtUnlockVirtualMemory = function (ProcessHandle: HANDLE; BaseAddress: PPVOID; LockSize: PULONG; LockType: ULONG): NTSTATUS; stdcall;
  TFNNtUnmapViewOfSection = function (ProcessHandle: HANDLE; BaseAddress: PVOID): NTSTATUS; stdcall;
  TFNNtVdmControl = function (ControlCode: ULONG; ControlData: PVOID): NTSTATUS; stdcall;
  TFNNtW32Call = function (RoutineIndex: ULONG; Argument: PVOID; ArgumentLength: ULONG; Result_: PPVOID; ResultLength: PULONG): NTSTATUS; stdcall;
  TFNNtWaitForMultipleObjects = function (HandleCount: ULONG; Handles: PHANDLE; WaitType: WAIT_TYPE; Alertable: BOOLEAN; Timeout: PLARGE_INTEGER): NTSTATUS; stdcall;
  TFNNtWaitForSingleObject = function (Handle: HANDLE; Alertable: BOOLEAN; Timeout: PLARGE_INTEGER): NTSTATUS; stdcall;
  TFNNtWaitHighEventPair = function (EventPairHandle: HANDLE): NTSTATUS; stdcall;
  TFNNtWaitLowEventPair = function (EventPairHandle: HANDLE): NTSTATUS; stdcall;
  TFNNtWriteFile = function (FileHandle: HANDLE; Event: HANDLE; ApcRoutine: PIO_APC_ROUTINE; ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; Buffer: PVOID; Length: ULONG; ByteOffset: PLARGE_INTEGER; Key: PULONG): NTSTATUS; stdcall;
  TFNNtWriteFileGather = function (FileHandle: HANDLE; Event: HANDLE; ApcRoutine: PIO_APC_ROUTINE; ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; Buffer: PFILE_SEGMENT_ELEMENT; Length: ULONG; ByteOffset: PLARGE_INTEGER;
    Key: PULONG): NTSTATUS; stdcall;
  TFNNtWriteRequestData = function (PortHandle: HANDLE; Message: PPORT_MESSAGE; Index: ULONG; Buffer: PVOID; BufferLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
  TFNNtWriteVirtualMemory = function (ProcessHandle: HANDLE; BaseAddress: PVOID; Buffer: PVOID; BufferLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
  TFNNtYieldExecution = function (): NTSTATUS; stdcall;
  TFNRtlAbsoluteToSelfRelativeSD = function (pAbsoluteSD: PSECURITY_DESCRIPTOR; pSelfRelativeSD: PSECURITY_DESCRIPTOR; lpdwBufferLength: LPDWORD): NTSTATUS; stdcall;
  TFNRtlAcquirePebLock = procedure(); stdcall;
  TFNRtlAddAccessAllowedAce = function (pAcl: PACL; dwAceRevision: DWORD; AccessMask: ACCESS_MASK; pSid: PSID): NTSTATUS; stdcall;
  TFNRtlAddAccessAllowedAceEx = function (pAcl: PACL; dwAceRevision: DWORD; AceFlags: DWORD; AccessMask: ACCESS_MASK; pSid: PSID): NTSTATUS; stdcall;
  TFNRtlAddAccessDeniedAce = function (pAcl: PACL; dwAceRevision: DWORD; AccessMask: ACCESS_MASK; pSid: PSID): NTSTATUS; stdcall;
  TFNRtlAddAccessDeniedAceEx = function (pAcl: PACL; dwAceRevision: DWORD; AceFlags: DWORD; AccessMask: ACCESS_MASK; pSid: PSID): NTSTATUS; stdcall;
  TFNRtlAddAce = function (pAcl: PACL; dwAceRevision: DWORD; dwStartingAceIndex: DWORD; pAceList: PVOID; nAceListLength: DWORD): NTSTATUS; stdcall;
  TFNRtlAddAuditAccessAce = function (pAcl: PACL; dwAceRevision: DWORD; AccessMask: ACCESS_MASK; pSid: PSID; bAuditSuccess: BOOLEAN; bAuditFailure: BOOLEAN): NTSTATUS; stdcall;
  TFNRtlAddAuditAccessAceEx = function (pAcl: PACL; dwAceRevision: DWORD; AceFlags: DWORD; AccessMask: ACCESS_MASK; pSid: PSID; bAuditSuccess: BOOLEAN; bAuditFailure: BOOLEAN): NTSTATUS; stdcall;
  TFNRtlAddRange = function (RangeList: PRTL_RANGE_LIST; Start: ULONGLONG; End_: ULONGLONG; Attributes: UCHAR; Flags: ULONG; UserData: PVOID; Owner: PVOID): NTSTATUS; stdcall;
  TFNRtlAddVectoredExceptionHandler = function (FirstHandler: ULONG; VectoredHandler: PVECTORED_EXCEPTION_HANDLER): PVOID; stdcall;
  TFNRtlAdjustPrivilege = function (Privilege: ULONG; Enable: BOOLEAN; CurrentThread: BOOLEAN; Enabled: PBOOLEAN): NTSTATUS; stdcall;
  TFNRtlAllocateAndInitializeSid = function (pIdentifierAuthority: PSID_IDENTIFIER_AUTHORITY; SubAuthorityCount: BYTE; nSubAuthority0: DWORD; nSubAuthority1: DWORD; nSubAuthority2: DWORD; nSubAuthority3: DWORD; nSubAuthority4: DWORD;
    nSubAuthority5: DWORD; nSubAuthority6: DWORD; nSubAuthority7: DWORD; var pSid: PSID): BOOL; stdcall;
  TFNRtlAllocateHeap = function (hHeap: HANDLE; dwFlags: ULONG; Size: ULONG): PVOID; stdcall;
  TFNRtlAnsiCharToUnicodeChar = function (AnsiChar: AnsiChar): WCHAR; stdcall;
  TFNRtlAnsiStringToUnicodeSize = function (AnsiString: PANSI_STRING): ULONG; stdcall;
  TFNRtlAnsiStringToUnicodeString = function (DestinationString: PUNICODE_STRING; SourceString: PANSI_STRING; AllocateDestinationString: BOOLEAN): NTSTATUS; stdcall;
  TFNRtlAppendAsciizToString = function (DestinationString: PSTRING; AppendThisString: LPCSTR): NTSTATUS; stdcall;
  TFNRtlAppendStringToString = function (DestinationString: PSTRING; AppendThisString: PSTRING): NTSTATUS; stdcall;
  TFNRtlAppendUnicodeStringToString = function (DestinationString: PUNICODE_STRING; SourceString: PUNICODE_STRING): NTSTATUS; stdcall;
  TFNRtlAppendUnicodeToString = function (Destination: PUNICODE_STRING; Source: LPCWSTR): NTSTATUS; stdcall;
  TFNRtlAreAllAccessesGranted = function (GrantedAccess: ACCESS_MASK; WantedAccess: ACCESS_MASK): BOOLEAN; stdcall;
  TFNRtlAreAnyAccessesGranted = function (GrantedAccess: ACCESS_MASK; WantedAccess: ACCESS_MASK): BOOLEAN; stdcall;
  TFNRtlAreBitsClear = function (BitMapHeader: PRTL_BITMAP; StartingIndex: ULONG; Length: ULONG): BOOLEAN; stdcall;
  TFNRtlAreBitsSet = function (BitMapHeader: PRTL_BITMAP; StartingIndex: ULONG; Length: ULONG): BOOLEAN; stdcall;
  TFNRtlAssert = procedure(FailedAssertion: PVOID; FileName: PVOID; LineNumber: ULONG; Message: PAnsiChar); stdcall;
  TFNRtlCaptureContext = procedure(ContextRecord: PCONTEXT); stdcall;
  TFNRtlCharToInteger = function (Str: PCSZ; Base: ULONG; Value: PULONG): NTSTATUS; stdcall;
  TFNRtlCheckForOrphanedCriticalSections = procedure(hThread: HANDLE); stdcall;
  TFNRtlCheckRegistryKey = function (RelativeTo: ULONG; Path: PWSTR): NTSTATUS; stdcall;
  TFNRtlClearAllBits = procedure(BitMapHeader: PRTL_BITMAP); stdcall;
  TFNRtlClearBits = procedure(BitMapHeader: PRTL_BITMAP; StartingIndex: ULONG; NumberToClear: ULONG); stdcall;
  TFNRtlCompactHeap = function (hHeap: HANDLE; dwFlags: ULONG): ULONG; stdcall;
  TFNRtlCompareMemory = function (Source1: PVOID; Source2: PVOID; Length: SIZE_T): SIZE_T; stdcall;
  TFNRtlCompareMemoryUlong = function (Source: PVOID; Length: ULONG; Value: ULONG): ULONG; stdcall;
  TFNRtlCompareString = function (String1: PSTRING; String2: PSTRING; CaseInsensitive: BOOLEAN): LONG; stdcall;
  TFNRtlCompareUnicodeString = function (String1: PUNICODE_STRING; String2: PUNICODE_STRING; CaseInsensitive: BOOLEAN): LONG; stdcall;
  TFNRtlConvertLongToLargeInteger = function (SignedInteger: LONG): LARGE_INTEGER; stdcall;
  TFNRtlConvertSidToUnicodeString = function (UnicodeString: PUNICODE_STRING; Sid: PSID; AllocateDestinationString: BOOLEAN): NTSTATUS; stdcall;
  TFNRtlConvertUlongToLargeInteger = function (UnsignedInteger: ULONG): LARGE_INTEGER; stdcall;
  TFNRtlCopyLuid = procedure(Destination: PLUID; Source: PLUID); stdcall;
  TFNRtlCopyRangeList = function (CopyRangeList: PRTL_RANGE_LIST; RangeList: PRTL_RANGE_LIST): NTSTATUS; stdcall;
  TFNRtlCopySecurityDescriptor = function (Source: PSECURITY_DESCRIPTOR; var Destination: PSECURITY_DESCRIPTOR): NTSTATUS; stdcall;
  TFNRtlCopySid = function (DestinationLength: ULONG; Destination: PSID; Source: PSID): NTSTATUS; stdcall;
  TFNRtlCopyString = procedure(DestinationString: PSTRING; SourceString: PSTRING); stdcall;
  TFNRtlCopyUnicodeString = procedure(DestinationString: PUNICODE_STRING; SourceString: PUNICODE_STRING); stdcall;
  TFNRtlCreateAcl = function (pAcl: PACL; nAclLength: DWORD; dwAclRevision: DWORD): NTSTATUS; stdcall;
  TFNRtlCreateHeap = function (dwOptions: ULONG; Base: PVOID; dwMaximumSize: SIZE_T; dwInitialSize: SIZE_T; UnknownOptional1: PVOID; UnknownOptional2: PVOID): HANDLE; stdcall;
  TFNRtlCreateProcessParameters = function (ProcessParameters: PPRTL_USER_PROCESS_PARAMETERS; ImageFile: PUNICODE_STRING; DllPath: PUNICODE_STRING; CurrentDirectory: PUNICODE_STRING; CommandLine: PUNICODE_STRING; CreationFlags: ULONG;
    WindowTitle: PUNICODE_STRING; Desktop: PUNICODE_STRING; Reserved: PUNICODE_STRING; Reserved2: PUNICODE_STRING): NTSTATUS; stdcall;
  TFNRtlCreateQueryDebugBuffer = function (Size: ULONG; EventPair: BOOLEAN): PDEBUG_BUFFER; stdcall;
  TFNRtlCreateRegistryKey = function (RelativeTo: ULONG; Path: PWSTR): NTSTATUS; stdcall;
  TFNRtlCreateSecurityDescriptor = function (SecurityDescriptor: PSECURITY_DESCRIPTOR; Revision: ULONG): NTSTATUS; stdcall;
  TFNRtlCreateUnicodeString = function (DestinationString: PUNICODE_STRING; SourceString: PWSTR): BOOLEAN; stdcall;
  TFNRtlCreateUnicodeStringFromAsciiz = function (DestinationString: PUNICODE_STRING; SourceString: PAnsiChar): BOOLEAN; stdcall;
  TFNRtlCreateUserProcess = function (ImageFileName: PUNICODE_STRING; Attributes: ULONG; ProcessParameters: PRTL_USER_PROCESS_PARAMETERS; ProcessSecurityDescriptor: PSECURITY_DESCRIPTOR; ThreadSecurityDescriptor: PSECURITY_DESCRIPTOR;
    ParentProcess: HANDLE; InheritHandles: BOOLEAN; DebugPort: HANDLE; ExceptionPort: HANDLE; ProcessInfo: PRTL_PROCESS_INFORMATION): NTSTATUS; stdcall;
  TFNRtlCreateUserThread = function (hProcess: HANDLE; SecurityDescriptor: PSECURITY_DESCRIPTOR; CreateSuspended: BOOLEAN; StackZeroBits: ULONG; StackReserve: ULONG; StackCommit: ULONG; lpStartAddress: PTHREAD_START_ROUTINE;
    lpParameter: PVOID; phThread: PHANDLE; ClientId: PCLIENT_ID): NTSTATUS; stdcall;
  TFNRtlCutoverTimeToSystemTime = function (TargetTimeFields: PTIME_FIELDS; Time: PLARGE_INTEGER; CurrentTime: PLARGE_INTEGER; bUnknown: BOOLEAN): BOOLEAN; stdcall;
  TFNRtlDeNormalizeProcessParams = function (ProcessParameters: PRTL_USER_PROCESS_PARAMETERS): PRTL_USER_PROCESS_PARAMETERS; stdcall;
  TFNRtlDeleteAce = function (pAcl: PACL; dwAceIndex: DWORD): NTSTATUS; stdcall;
  TFNRtlDeleteCriticalSection = procedure(lpCriticalSection: PRTL_CRITICAL_SECTION); stdcall;
  TFNRtlDeleteOwnersRanges = function (RangeList: PRTL_RANGE_LIST; Owner: PVOID): NTSTATUS; stdcall;
  TFNRtlDeleteRange = function (RangeList: PRTL_RANGE_LIST; Start: ULONGLONG; End_: ULONGLONG; Owner: PVOID): NTSTATUS; stdcall;
  TFNRtlDeleteRegistryValue = function (RelativeTo: ULONG; Path: LPCWSTR; ValueName: LPCWSTR): NTSTATUS; stdcall;
  TFNRtlDestroyHeap = function (HeapHandle: HANDLE): HANDLE; stdcall;
  TFNRtlDestroyProcessParameters = function (ProcessParameters: PRTL_USER_PROCESS_PARAMETERS): NTSTATUS; stdcall;
  TFNRtlDestroyQueryDebugBuffer = function (DebugBuffer: PDEBUG_BUFFER): NTSTATUS; stdcall;
  TFNRtlDetermineDosPathNameType_U = function (wcsPathNameType: PWSTR): ULONG; stdcall;
  TFNRtlDnsHostNameToComputerName = function (ComputerName: PUNICODE_STRING; DnsName: PUNICODE_STRING; AllocateComputerNameString: BOOLEAN): NTSTATUS; stdcall;
  TFNRtlDoesFileExists_U = function (FileName: PWSTR): BOOLEAN; stdcall;
  TFNRtlDosPathNameToNtPathName_U = function (DosName: PWSTR; var NtName: UNICODE_STRING; DosFilePath: PPWSTR; NtFilePath: PUNICODE_STRING): BOOLEAN; stdcall;
  TFNRtlDosSearchPath_U = function (SearchPath: PWSTR; Name: PWSTR; Ext: PWSTR; cbBuf: ULONG; Buffer: PWSTR; var Shortname: PWSTR): ULONG; stdcall;
  TFNRtlDowncaseUnicodeChar = function (Source: WCHAR): WCHAR; stdcall;
  TFNRtlDowncaseUnicodeString = function (DestinationString: PUNICODE_STRING; SourceString: PUNICODE_STRING; AllocateDestinationString: BOOLEAN): NTSTATUS; stdcall;
  TFNRtlDuplicateUnicodeString = function (AddTerminatingZero: ULONG; Source: PUNICODE_STRING; Destination: PUNICODE_STRING): NTSTATUS; stdcall;
  TFNRtlEnableEarlyCriticalSectionEventCreation = procedure(); stdcall;
  TFNRtlEnlargedIntegerMultiply = function (Multiplicand: LONG; Multiplier: LONG): LARGE_INTEGER; stdcall;
  TFNRtlEnlargedUnsignedDivide = function (Dividend: ULARGE_INTEGER; Divisor: ULONG; Remainder: PULONG): ULONG; stdcall;
  TFNRtlEnlargedUnsignedMultiply = function (Multiplicand: ULONG; Multiplier: ULONG): LARGE_INTEGER; stdcall;
  TFNRtlEnterCriticalSection = procedure(lpCriticalSection: PRTL_CRITICAL_SECTION); stdcall;
  TFNRtlEqualComputerName = function (String1: PUNICODE_STRING; String2: PUNICODE_STRING): BOOLEAN; stdcall;
  TFNRtlEqualDomainName = function (String1: PUNICODE_STRING; String2: PUNICODE_STRING): BOOLEAN; stdcall;
  TFNRtlEqualLuid = function (Luid1: PLUID; Luid2: PLUID): BOOLEAN; stdcall;
  TFNRtlEqualPrefixSid = function (pSid1: PSID; pSid2: PSID): BOOLEAN; stdcall;
  TFNRtlEqualSid = function (pSid1: PSID; pSid2: PSID): BOOLEAN; stdcall;
  TFNRtlEqualString = function (String1: PSTRING; String2: PSTRING; CaseInsensitive: BOOLEAN): BOOLEAN; stdcall;
  TFNRtlEqualUnicodeString = function (String1: PUNICODE_STRING; String2: PUNICODE_STRING; CaseInsensitive: BOOLEAN): BOOLEAN; stdcall;
  TFNRtlEraseUnicodeString = procedure(Str: PUNICODE_STRING); stdcall;
  TFNRtlExpandEnvironmentStrings_U = function (Environment: PVOID; Source: PUNICODE_STRING; Destination: PUNICODE_STRING; ReturnedLength: PULONG): NTSTATUS; stdcall;
  TFNRtlExtendedIntegerMultiply = function (Multiplicand: LARGE_INTEGER; Multiplier: LONG): LARGE_INTEGER; stdcall;
  TFNRtlExtendedLargeIntegerDivide = function (Dividend: LARGE_INTEGER; Divisor: ULONG; Remainder: PULONG): LARGE_INTEGER; stdcall;
  TFNRtlExtendedMagicDivide = function (Dividend: LARGE_INTEGER; MagicDivisor: LARGE_INTEGER; ShiftCount: CCHAR): LARGE_INTEGER; stdcall;
  TFNRtlFillMemory = procedure(Destination: PVOID; Length: SIZE_T; Fill: UCHAR); stdcall;
  TFNRtlFillMemoryUlong = procedure(Destination: PVOID; Length: ULONG; Fill: ULONG); stdcall;
  TFNRtlFindCharInUnicodeString = function (dwFlags: ULONG; UnicodeString: PUNICODE_STRING; CharactersToFind: PUNICODE_STRING; Positions: PUSHORT): NTSTATUS; stdcall;
  TFNRtlFindClearBits = function (BitMapHeader: PRTL_BITMAP; NumberToFind: ULONG; HintIndex: ULONG): ULONG; stdcall;
  TFNRtlFindClearBitsAndSet = function (BitMapHeader: PRTL_BITMAP; NumberToFind: ULONG; HintIndex: ULONG): ULONG; stdcall;
  TFNRtlFindLastBackwardRunClear = function (BitMapHeader: PRTL_BITMAP; FromIndex: ULONG; StartingRunIndex: PULONG): ULONG; stdcall;
  TFNRtlFindLeastSignificantBit = function (Set_: ULONGLONG): CCHAR; stdcall;
  TFNRtlFindLongestRunClear = function (BitMapHeader: PRTL_BITMAP; StartingIndex: PULONG): ULONG; stdcall;
  TFNRtlFindMostSignificantBit = function (Set_: ULONGLONG): CCHAR; stdcall;
  TFNRtlFindNextForwardRunClear = function (BitMapHeader: PRTL_BITMAP; FromIndex: ULONG; StartingRunIndex: PULONG): ULONG; stdcall;
  TFNRtlFindRange = function (RangeList: PRTL_RANGE_LIST; Minimum: ULONGLONG; Maximum: ULONGLONG; Length: ULONG; Alignment: ULONG; Flags: ULONG; AttributeAvailableMask: UCHAR; Context: PVOID; Callback: PRTL_CONFLICT_RANGE_CALLBACK;
    Start: PULONGLONG): NTSTATUS; stdcall;
  TFNRtlFindSetBits = function (BitMapHeader: PRTL_BITMAP; NumberToFind: ULONG; HintIndex: ULONG): ULONG; stdcall;
  TFNRtlFindSetBitsAndClear = function (BitMapHeader: PRTL_BITMAP; NumberToFind: ULONG; HintIndex: ULONG): ULONG; stdcall;
  TFNRtlFirstFreeAce = function (pAcl: PACL; var pAce: PVOID): BOOLEAN; stdcall;
  TFNRtlFormatCurrentUserKeyPath = function (CurrentUserKeyPath: PUNICODE_STRING): NTSTATUS; stdcall;
  TFNRtlFreeAnsiString = procedure(AnsiString: PANSI_STRING); stdcall;
  TFNRtlFreeHeap = function (hHeap: HANDLE; dwFlags: ULONG; MemoryPointer: PVOID): BOOLEAN; stdcall;
  TFNRtlFreeOemString = procedure(OemString: POEM_STRING); stdcall;
  TFNRtlFreeRangeList = procedure(RangeList: PRTL_RANGE_LIST); stdcall;
  TFNRtlFreeSid = function (pSid: PSID): PVOID; stdcall;
  TFNRtlFreeUnicodeString = procedure(UnicodeString: PUNICODE_STRING); stdcall;
  TFNRtlGUIDFromString = function (GuidString: PUNICODE_STRING; Guid: LPGUID): NTSTATUS; stdcall;
  TFNRtlGetAce = function (pAcl: PACL; dwAceIndex: DWORD; var pAce: PVOID): NTSTATUS; stdcall;
  TFNRtlGetCallersAddress = procedure(CallersAddress: PPVOID; CallersCaller: PPVOID); stdcall;
  TFNRtlGetControlSecurityDescriptor = function (pSecurityDescriptor: PSECURITY_DESCRIPTOR; var Control: SECURITY_DESCRIPTOR_CONTROL; var dwRevision: DWORD): NTSTATUS; stdcall;
  TFNRtlGetCurrentDirectory_U = function (MaximumLength: ULONG; Buffer: PWSTR): ULONG; stdcall;
  TFNRtlGetCurrentPeb = function (): PPEB; stdcall;
  TFNRtlGetDaclSecurityDescriptor = function (pSecurityDescriptor: PSECURITY_DESCRIPTOR; var bDaclPresent: BOOLEAN; var Dacl: PACL; var bDaclDefaulted: BOOLEAN): NTSTATUS; stdcall;
  TFNRtlGetFirstRange = function (RangeList: PRTL_RANGE_LIST; Iterator: PRTL_RANGE_LIST_ITERATOR; var Range: PRTL_RANGE): NTSTATUS; stdcall;
  TFNRtlGetFullPathName_U = function (DosName: PWSTR; Size: ULONG; Buf: PWSTR; var Shortname: PWSTR): ULONG; stdcall;
  TFNRtlGetGroupSecurityDescriptor = function (pSecurityDescriptor: PSECURITY_DESCRIPTOR; var pGroup: PSID; var bGroupDefaulted: BOOLEAN): NTSTATUS; stdcall;
  TFNRtlGetLastNtStatus = function (): NTSTATUS; stdcall;
  TFNRtlGetLongestNtPathLength = function (): ULONG; stdcall;
  TFNRtlGetNextRange = function (Iterator: PRTL_RANGE_LIST_ITERATOR; var Range: PRTL_RANGE; MoveForwards: BOOLEAN): NTSTATUS; stdcall;
  TFNRtlGetNtGlobalFlags = function (): ULONG; stdcall;
  TFNRtlGetNtProductType = function (var ProductType: ULONG): BOOLEAN; stdcall;
  TFNRtlGetNtVersionNumbers = procedure(var dwMajorVersion: ULONG; var dwMinorVersion: ULONG; UnknownCanBeNull: PDWORD); stdcall;
  TFNRtlGetOwnerSecurityDescriptor = function (pSecurityDescriptor: PSECURITY_DESCRIPTOR; var pOwner: PSID; var OwnerDefaulted: BOOLEAN): NTSTATUS; stdcall;
  TFNRtlGetProcessHeaps = function (ArraySize: ULONG; HeapArray: PHANDLE): ULONG; stdcall;
  TFNRtlGetSaclSecurityDescriptor = function (pSecurityDescriptor: PSECURITY_DESCRIPTOR; var bSaclPresent: BOOLEAN; var Sacl: PACL; var bSaclDefaulted: BOOLEAN): NTSTATUS; stdcall;
  TFNRtlGetVersion = function (lpVersionInformation: PRTL_OSVERSIONINFOW): NTSTATUS; stdcall;
  TFNRtlIdentifierAuthoritySid = function (Sid: PSID): PSID_IDENTIFIER_AUTHORITY; stdcall;
  TFNRtlImageDirectoryEntryToData = function (ImageBase: HMODULE; MappedAsImage: BOOLEAN; DirectoryEntry: USHORT; Size: PULONG): PVOID; stdcall;
  TFNRtlImageNtHeader = function (ImageBase: HMODULE): PIMAGE_NT_HEADERS; stdcall;
  TFNRtlImageNtHeaderEx = function (dwFlags: DWORD; ImageBase: HMODULE): PIMAGE_NT_HEADERS; stdcall;
  TFNRtlImageRvaToSection = function (NtHeaders: PIMAGE_NT_HEADERS; ImageBase: HMODULE; Rva: ULONG): PIMAGE_SECTION_HEADER; stdcall;
  TFNRtlImageRvaToVa = function (NtHeaders: PIMAGE_NT_HEADERS; ImageBase: HMODULE; Rva: ULONG; var LastRvaSection: PIMAGE_SECTION_HEADER): PVOID; stdcall;
  TFNRtlImpersonateSelf = function (ImpersonationLevel: SECURITY_IMPERSONATION_LEVEL): NTSTATUS; stdcall;
  TFNRtlInitAnsiString = procedure(DestinationString: PANSI_STRING; SourceString: PCSZ); stdcall;
  TFNRtlInitAnsiStringEx = function (DestinationString: PANSI_STRING; SourceString: PCSZ): NTSTATUS; stdcall;
  TFNRtlInitString = procedure(DestinationString: PSTRING; SourceString: PCSZ); stdcall;
  TFNRtlInitUnicodeString = procedure(DestinationString: PUNICODE_STRING; SourceString: LPCWSTR); stdcall;
  TFNRtlInitUnicodeStringEx = function (DestinationString: PUNICODE_STRING; SourceString: LPCWSTR): NTSTATUS; stdcall;
  TFNRtlInitializeBitMap = procedure(BitMapHeader: PRTL_BITMAP; BitMapBuffer: PULONG; SizeOfBitMap: ULONG); stdcall;
  TFNRtlInitializeCriticalSection = function (lpCriticalSection: PRTL_CRITICAL_SECTION): NTSTATUS; stdcall;
  TFNRtlInitializeCriticalSectionAndSpinCount = function (lpCriticalSection: PRTL_CRITICAL_SECTION; dwSpinCount: ULONG): NTSTATUS; stdcall;
  TFNRtlInitializeRangeList = procedure(RangeList: PRTL_RANGE_LIST); stdcall;
  TFNRtlInitializeSListHead = procedure(ListHead: PSLIST_HEADER); stdcall;
  TFNRtlInitializeSid = function (pSid: PSID; pIdentifierAuthority: PSID_IDENTIFIER_AUTHORITY; nSubAuthorityCount: UCHAR): NTSTATUS; stdcall;
  TFNRtlInt64ToUnicodeString = function (Value: ULONGLONG; Base: ULONG; Str: PUNICODE_STRING): NTSTATUS; stdcall;
  TFNRtlIntegerToChar = function (Value: ULONG; Base: ULONG; Length: ULONG; Str: PAnsiChar): NTSTATUS; stdcall;
  TFNRtlIntegerToUnicodeString = function (Value: ULONG; Base: ULONG; Str: PUNICODE_STRING): NTSTATUS; stdcall;
  TFNRtlInterlockedFlushSList = function (ListHead: PSLIST_HEADER): PSLIST_ENTRY; stdcall;
  TFNRtlInterlockedPopEntrySList = function (ListHead: PSLIST_HEADER): PSLIST_ENTRY; stdcall;
  TFNRtlInterlockedPushEntrySList = function (ListHead: PSLIST_HEADER; ListEntry: PSLIST_ENTRY): PSLIST_ENTRY; stdcall;
  TFNRtlInvertRangeList = function (InvertedRangeList: PRTL_RANGE_LIST; RangeList: PRTL_RANGE_LIST): NTSTATUS; stdcall;
  TFNRtlIpv4AddressToStringA = function (IP: PULONG; Buffer: LPSTR): LPSTR; stdcall;
  TFNRtlIpv4AddressToStringW = function (IP: PULONG; Buffer: LPWSTR): LPWSTR; stdcall;
  TFNRtlIsDosDeviceName_U = function (TestString: LPCWSTR): ULONG; stdcall;
  TFNRtlIsNameLegalDOS8Dot3 = function (Name: PUNICODE_STRING; OemName: POEM_STRING; NameContainsSpaces: PBOOLEAN): BOOLEAN; stdcall;
  TFNRtlIsRangeAvailable = function (RangeList: PRTL_RANGE_LIST; Start: ULONGLONG; End_: ULONGLONG; Flags: ULONG; AttributeAvailableMask: UCHAR; Context: PVOID; Callback: PRTL_CONFLICT_RANGE_CALLBACK; Available: PBOOLEAN): NTSTATUS;
    stdcall;
  TFNRtlIsTextUnicode = function (lpBuffer: PVOID; cb: Integer; lpi: LPINT): BOOLEAN; stdcall;
  TFNRtlLargeIntegerAdd = function (Addend1: LARGE_INTEGER; Addend2: LARGE_INTEGER): LARGE_INTEGER; stdcall;
  TFNRtlLargeIntegerArithmeticShift = function (LargeInteger: LARGE_INTEGER; ShiftCount: CCHAR): LARGE_INTEGER; stdcall;
  TFNRtlLargeIntegerDivide = function (Dividend: LARGE_INTEGER; Divisor: LARGE_INTEGER; Remainder: PLARGE_INTEGER): LARGE_INTEGER; stdcall;
  TFNRtlLargeIntegerNegate = function (NegateThis: LARGE_INTEGER): LARGE_INTEGER; stdcall;
  TFNRtlLargeIntegerShiftLeft = function (LargeInteger: LARGE_INTEGER; ShiftCount: CCHAR): LARGE_INTEGER; stdcall;
  TFNRtlLargeIntegerShiftRight = function (LargeInteger: LARGE_INTEGER; ShiftCount: CCHAR): LARGE_INTEGER; stdcall;
  TFNRtlLargeIntegerSubtract = function (Number: LARGE_INTEGER; Subtrahend: LARGE_INTEGER): LARGE_INTEGER; stdcall;
  TFNRtlLargeIntegerToChar = function (Value: PLARGE_INTEGER; Base: ULONG; BufferLength: ULONG; Buffer: PAnsiChar): NTSTATUS; stdcall;
  TFNRtlLeaveCriticalSection = procedure(lpCriticalSection: PRTL_CRITICAL_SECTION); stdcall;
  TFNRtlLengthRequiredSid = function (nSubAuthorityCount: ULONG): ULONG; stdcall;
  TFNRtlLengthSecurityDescriptor = function (SecurityDescriptor: PSECURITY_DESCRIPTOR): ULONG; stdcall;
  TFNRtlLengthSid = function (pSid: PSID): ULONG; stdcall;
  TFNRtlLocalTimeToSystemTime = function (LocalTime: PLARGE_INTEGER; SystemTime: PLARGE_INTEGER): NTSTATUS; stdcall;
  TFNRtlLockHeap = function (hHeap: PVOID): BOOLEAN; stdcall;
  TFNRtlMakeSelfRelativeSD = function (pAbsoluteSD: PSECURITY_DESCRIPTOR; pSelfRelativeSD: PSECURITY_DESCRIPTOR; lpdwBufferLength: LPDWORD): NTSTATUS; stdcall;
  TFNRtlMapGenericMask = procedure(AccessMask: PACCESS_MASK; GenericMapping: PGENERIC_MAPPING); stdcall;
  TFNRtlMapSecurityErrorToNtStatus = function (SecurityError: DWORD): NTSTATUS; stdcall;
  TFNRtlMergeRangeLists = function (MergedRangeList: PRTL_RANGE_LIST; RangeList1: PRTL_RANGE_LIST; RangeList2: PRTL_RANGE_LIST; Flags: ULONG): NTSTATUS; stdcall;
  TFNRtlMoveMemory = procedure(Destination: PVOID; Source: PVOID; Length: SIZE_T); stdcall;
  TFNRtlNormalizeProcessParams = function (ProcessParameters: PRTL_USER_PROCESS_PARAMETERS): PRTL_USER_PROCESS_PARAMETERS; stdcall;
  TFNRtlNtStatusToDosError = function (Status: NTSTATUS): ULONG; stdcall;
  TFNRtlNtStatusToDosErrorNoTeb = function (Status: NTSTATUS): ULONG; stdcall;
  TFNRtlNumberOfClearBits = function (BitMapHeader: PRTL_BITMAP): ULONG; stdcall;
  TFNRtlNumberOfSetBits = function (BitMapHeader: PRTL_BITMAP): ULONG; stdcall;
  TFNRtlOemStringToUnicodeSize = function (AnsiString: POEM_STRING): ULONG; stdcall;
  TFNRtlOemStringToUnicodeString = function (DestinationString: PUNICODE_STRING; SourceString: POEM_STRING; AllocateDestinationString: BOOLEAN): NTSTATUS; stdcall;
  TFNRtlOemToUnicodeN = function (UnicodeString: PWSTR; UnicodeSize: ULONG; var ResultSize: ULONG; OemString: PAnsiChar; OemSize: ULONG): NTSTATUS; stdcall;
  TFNRtlOpenCurrentUser = function (samDesired: ACCESS_MASK; phkResult: PHKEY): NTSTATUS; stdcall;
  TFNRtlPrefixString = function (String1: PANSI_STRING; String2: PANSI_STRING; CaseInsensitive: BOOLEAN): BOOLEAN; stdcall;
  TFNRtlPrefixUnicodeString = function (String1: PUNICODE_STRING; String2: PUNICODE_STRING; CaseInsensitive: BOOLEAN): BOOLEAN; stdcall;
  TFNRtlQueryDepthSList = function (ListHead: PSLIST_HEADER): USHORT; stdcall;
  TFNRtlQueryEnvironmentVariable_U = function (Environment: PVOID; VarName: PUNICODE_STRING; VarValue: PUNICODE_STRING): NTSTATUS; stdcall;
  TFNRtlQueryInformationAcl = function (pAcl: PACL; pAclInformation: PVOID; nAclInformationLength: DWORD; dwAclInformationClass: ACL_INFORMATION_CLASS): NTSTATUS; stdcall;
  TFNRtlQueryProcessDebugInformation = function (ProcessId: ULONG; DebugInfoClassMask: ULONG; DebugBuffer: PDEBUG_BUFFER): NTSTATUS; stdcall;
  TFNRtlQueryRegistryValues = function (RelativeTo: ULONG; Path: LPCWSTR; QueryTable: PRTL_QUERY_REGISTRY_TABLE; Context: PVOID; Environment: PVOID): NTSTATUS; stdcall;
  TFNRtlRaiseStatus = procedure(Status: NTSTATUS); stdcall;
  TFNRtlRandom = function (Seed: PULONG): ULONG; stdcall;
  TFNRtlRandomEx = function (Seed: PULONG): ULONG; stdcall;
  TFNRtlReAllocateHeap = function (hHeap: HANDLE; dwFlags: ULONG; lpMem: PVOID; dwBytes: SIZE_T): PVOID; stdcall;
  TFNRtlReleasePebLock = procedure(); stdcall;
  TFNRtlRemoveVectoredExceptionHandler = function (VectoredHandlerHandle: PVOID): ULONG; stdcall;
  TFNRtlRestoreLastWin32Error = procedure(dwErrCode: DWORD); stdcall;
  TFNRtlRunDecodeUnicodeString = procedure(CodeSeed: UCHAR; StringToDecode: PUNICODE_STRING); stdcall;
  TFNRtlRunEncodeUnicodeString = procedure(var CodeSeed: UCHAR; StringToEncode: PUNICODE_STRING); stdcall;
  TFNRtlSecondsSince1970ToTime = procedure(SecondsSince1970: ULONG; Time: PLARGE_INTEGER); stdcall;
  TFNRtlSecondsSince1980ToTime = procedure(SecondsSince1980: ULONG; Time: PLARGE_INTEGER); stdcall;
  TFNRtlSelfRelativeToAbsoluteSD = function (pSelfRelativeSD: PSECURITY_DESCRIPTOR; pAbsoluteSD: PSECURITY_DESCRIPTOR; lpdwAbsoluteSDSize: LPDWORD; pDacl: PACL; lpdwDaclSize: LPDWORD; pSacl: PACL; lpdwSaclSize: LPDWORD; pOwner: PSID;
    lpdwOwnerSize: LPDWORD; pPrimaryGroup: PSID; lpdwPrimaryGroupSize: LPDWORD): NTSTATUS; stdcall;
  TFNRtlSetAllBits = procedure(BitMapHeader: PRTL_BITMAP); stdcall;
  TFNRtlSetBits = procedure(BitMapHeader: PRTL_BITMAP; StartingIndex: ULONG; NumberToSet: ULONG); stdcall;
  TFNRtlSetControlSecurityDescriptor = function (pSecurityDescriptor: PSECURITY_DESCRIPTOR; ControlBitsOfInterest: SECURITY_DESCRIPTOR_CONTROL; ControlBitsToSet: SECURITY_DESCRIPTOR_CONTROL): NTSTATUS; stdcall;
  TFNRtlSetCriticalSectionSpinCount = function (lpCriticalSection: PRTL_CRITICAL_SECTION; dwSpinCount: ULONG): DWORD; stdcall;
  TFNRtlSetCurrentDirectory_U = function (NewCurrentDirectory: PUNICODE_STRING): NTSTATUS; stdcall;
  TFNRtlSetDaclSecurityDescriptor = function (SecurityDescriptor: PSECURITY_DESCRIPTOR; DaclPresent: BOOLEAN; Dacl: PACL; DaclDefaulted: BOOLEAN): NTSTATUS; stdcall;
  TFNRtlSetGroupSecurityDescriptor = function (pSecurityDescriptor: PSECURITY_DESCRIPTOR; pGroup: PSID; bGroupDefaulted: BOOLEAN): NTSTATUS; stdcall;
  TFNRtlSetInformationAcl = function (pAcl: PACL; pAclInformation: PVOID; nInformationLength: DWORD; dwAclInformationClass: ACL_INFORMATION_CLASS): NTSTATUS; stdcall;
  TFNRtlSetLastWin32ErrorAndNtStatusFromNtStatus = function (Status: NTSTATUS): ULONG; stdcall;
  TFNRtlSetOwnerSecurityDescriptor = function (pSecurityDescriptor: PSECURITY_DESCRIPTOR; pOwner: PSID; bOwnerDefaulted: BOOLEAN): NTSTATUS; stdcall;
  TFNRtlSetProcessIsCritical = function (bIsCritical: BOOLEAN; pbOldIsCriticalValue: PBOOLEAN; bUnknownCanBeFalse: BOOLEAN): NTSTATUS; stdcall;
  TFNRtlSetSaclSecurityDescriptor = function (pSecurityDescriptor: PSECURITY_DESCRIPTOR; bSaclPresent: BOOLEAN; pSacl: PACL; SaclDefaulted: BOOLEAN): NTSTATUS; stdcall;
  TFNRtlSetThreadIsCritical = function (bIsCritical: BOOLEAN; pbOldIsCriticalValue: PBOOLEAN; bUnknownCanBeFalse: BOOLEAN): NTSTATUS; stdcall;
  TFNRtlSizeHeap = function (hHeap: HANDLE; dwFlags: ULONG; lpMem: PVOID): SIZE_T; stdcall;
  TFNRtlStringFromGUID = function (Guid: REFGUID; GuidString: PUNICODE_STRING): NTSTATUS; stdcall;
  TFNRtlSubAuthorityCountSid = function (pSid: PSID): PUCHAR; stdcall;
  TFNRtlSubAuthoritySid = function (pSid: PSID; nSubAuthority: DWORD): PDWORD; stdcall;
  TFNRtlSystemTimeToLocalTime = function (SystemTime: PLARGE_INTEGER; LocalTime: PLARGE_INTEGER): NTSTATUS; stdcall;
  TFNRtlTimeFieldsToTime = function (TimeFields: PTIME_FIELDS; Time: PLARGE_INTEGER): BOOLEAN; stdcall;
  TFNRtlTimeToElapsedTimeFields = procedure(Time: PLARGE_INTEGER; TimeFields: PTIME_FIELDS); stdcall;
  TFNRtlTimeToSecondsSince1970 = function (Time: PLARGE_INTEGER; ElapsedSeconds: PULONG): BOOLEAN; stdcall;
  TFNRtlTimeToSecondsSince1980 = function (Time: PLARGE_INTEGER; ElapsedSeconds: PULONG): BOOLEAN; stdcall;
  TFNRtlTimeToTimeFields = procedure(Time: PLARGE_INTEGER; TimeFields: PTIME_FIELDS); stdcall;
  TFNRtlTryEnterCriticalSection = function (lpCriticalSection: PRTL_CRITICAL_SECTION): BOOL; stdcall;
  TFNRtlUnicodeStringToAnsiSize = function (UnicodeString: PUNICODE_STRING): ULONG; stdcall;
  TFNRtlUnicodeStringToAnsiString = function (DestinationString: PANSI_STRING; SourceString: PUNICODE_STRING; AllocateDestinationString: BOOLEAN): NTSTATUS; stdcall;
  TFNRtlUnicodeStringToCountedOemString = function (DestinationString: POEM_STRING; SourceString: PUNICODE_STRING; AllocateDestinationString: BOOLEAN): NTSTATUS; stdcall;
  TFNRtlUnicodeStringToInteger = function (Str: PUNICODE_STRING; Base: ULONG; Value: PULONG): NTSTATUS; stdcall;
  TFNRtlUnicodeStringToOemSize = function (UnicodeString: PUNICODE_STRING): ULONG; stdcall;
  TFNRtlUnicodeStringToOemString = function (DestinationString: POEM_STRING; SourceString: PCUNICODE_STRING; AllocateDestinationString: BOOLEAN): NTSTATUS; stdcall;
  TFNRtlUnicodeToMultiByteSize = function (BytesInMultiByteString: PULONG; UnicodeString: PWSTR; BytesInUnicodeString: ULONG): NTSTATUS; stdcall;
  TFNRtlUniform = function (Seed: PULONG): ULONG; stdcall;
  TFNRtlUnwind = procedure(TargetFrame: PVOID; TargetIp: PVOID; ExceptionRecord: PEXCEPTION_RECORD; ReturnValue: PVOID); stdcall;
  TFNRtlUpcaseUnicodeChar = function (SourceCharacter: WCHAR): WCHAR; stdcall;
  TFNRtlUpcaseUnicodeString = function (DestinationString: PUNICODE_STRING; SourceString: PUNICODE_STRING; AllocateDestinationString: BOOLEAN): NTSTATUS; stdcall;
  TFNRtlUpcaseUnicodeStringToAnsiString = function (DestinationString: PSTRING; SourceString: PUNICODE_STRING; AllocateDestinationString: BOOLEAN): NTSTATUS; stdcall;
  TFNRtlUpcaseUnicodeStringToCountedOemString = function (DestinationString: PSTRING; SourceString: PUNICODE_STRING; AllocateDestinationString: BOOLEAN): NTSTATUS; stdcall;
  TFNRtlUpcaseUnicodeStringToOemString = function (DestinationString: PSTRING; SourceString: PUNICODE_STRING; AllocateDestinationString: BOOLEAN): NTSTATUS; stdcall;
  TFNRtlUpcaseUnicodeToMultiByteN = function (MbString: PAnsiChar; MbSize: ULONG; var ResultSize: ULONG; UnicodeString: PWSTR; UnicodeSize: ULONG): NTSTATUS; stdcall;
  TFNRtlUpcaseUnicodeToOemN = function (OemString: PAnsiChar; OemSize: ULONG; var ResultSize: ULONG; UnicodeString: PWSTR; UnicodeSize: ULONG): NTSTATUS; stdcall;
  TFNRtlUpperChar = function (Character: AnsiChar): AnsiChar; stdcall;
  TFNRtlUpperString = procedure(DestinationString: PSTRING; SourceString: PSTRING); stdcall;
  TFNRtlValidAcl = function (Acl: PACL): BOOLEAN; stdcall;
  TFNRtlValidRelativeSecurityDescriptor = function (SecurityDescriptorInput: PSECURITY_DESCRIPTOR; SecurityDescriptorLength: ULONG; RequiredInformation: SECURITY_INFORMATION): BOOLEAN; stdcall;
  TFNRtlValidSecurityDescriptor = function (SecurityDescriptor: PSECURITY_DESCRIPTOR): BOOLEAN; stdcall;
  TFNRtlValidSid = function (pSid: PSID): BOOLEAN; stdcall;
  TFNRtlValidateHeap = function (hHeap: HANDLE; dwFlags: ULONG; lpMem: LPCVOID): BOOL; stdcall;
  TFNRtlValidateUnicodeString = function (dwMustBeNull: ULONG; ValidateThis: PUNICODE_STRING): NTSTATUS; stdcall;
  TFNRtlVerifyVersionInfo = function (VersionInfo: PRTL_OSVERSIONINFOEXW; TypeMask: ULONG; ConditionMask: ULONGLONG): NTSTATUS; stdcall;
  TFNRtlVolumeDeviceToDosName = function (VolumeDeviceObject: PVOID; DosName: PUNICODE_STRING): NTSTATUS; stdcall;
  TFNRtlWriteRegistryValue = function (RelativeTo: ULONG; Path: LPCWSTR; ValueName: LPCWSTR; ValueType: ULONG; ValueData: PVOID; ValueLength: ULONG): NTSTATUS; stdcall;
  TFNRtlZeroHeap = function (hHeap: HANDLE; dwFlags: ULONG): BOOLEAN; stdcall;
  TFNRtlZeroMemory = procedure(Destination: PVOID; Length: SIZE_T); stdcall;
  TFNRtlpNotOwnerCriticalSection = function (lpCriticalSection: PRTL_CRITICAL_SECTION): BOOLEAN; stdcall;
  TFNRtlpNtCreateKey = function (KeyHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; Unused1: ULONG; Unused2: ULONG; Disposition: PULONG): NTSTATUS; stdcall;
  TFNRtlpNtEnumerateSubKey = function (KeyHandle: HANDLE; SubKeyName: PUNICODE_STRING; Index: ULONG; Unused1: ULONG): NTSTATUS; stdcall;
  TFNRtlpNtMakeTemporaryKey = function (KeyHandle: HANDLE): NTSTATUS; stdcall;
  TFNRtlpNtOpenKey = function (KeyHandle: HANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; Unused: ULONG): NTSTATUS; stdcall;
  TFNRtlpNtQueryValueKey = function (KeyHandle: HANDLE; Type_: PULONG; Data: PVOID; DataSize: PULONG; Unused: ULONG): NTSTATUS; stdcall;
  TFNRtlpNtSetValueKey = function (KeyHandle: HANDLE; Type_: ULONG; Data: PVOID; DataSize: ULONG): NTSTATUS; stdcall;
  TFNRtlxAnsiStringToUnicodeSize = function (AnsiString: PANSI_STRING): ULONG; stdcall;
  TFNRtlxOemStringToUnicodeSize = function (AnsiString: POEM_STRING): ULONG; stdcall;
  TFNRtlxUnicodeStringToAnsiSize = function (UnicodeString: PUNICODE_STRING): ULONG; stdcall;
  TFNRtlxUnicodeStringToOemSize = function (UnicodeString: PUNICODE_STRING): ULONG; stdcall;
  TFNVerSetConditionMask = function (ConditionMask: ULONGLONG; dwTypeMask: DWORD; Condition: BYTE): ULONGLONG; stdcall;
  TFNZwAcceptConnectPort = TFNNtAcceptConnectPort;
  TFNZwAccessCheck = TFNNtAccessCheck;
  TFNZwAccessCheckAndAuditAlarm = TFNNtAccessCheckAndAuditAlarm;
  TFNZwAccessCheckByType = TFNNtAccessCheckByType;
  TFNZwAccessCheckByTypeAndAuditAlarm = TFNNtAccessCheckByTypeAndAuditAlarm;
  TFNZwAccessCheckByTypeResultList = TFNNtAccessCheckByTypeResultList;
  TFNZwAccessCheckByTypeResultListAndAuditAlarm = TFNNtAccessCheckByTypeResultListAndAuditAlarm;
  TFNZwAccessCheckByTypeResultListAndAuditAlarmByHandle = TFNNtAccessCheckByTypeResultListAndAuditAlarmByHandle;
  TFNZwAddAtom = TFNNtAddAtom;
  TFNZwAdjustGroupsToken = TFNNtAdjustGroupsToken;
  TFNZwAdjustPrivilegesToken = TFNNtAdjustPrivilegesToken;
  TFNZwAlertResumeThread = TFNNtAlertResumeThread;
  TFNZwAlertThread = TFNNtAlertThread;
  TFNZwAllocateLocallyUniqueId = TFNNtAllocateLocallyUniqueId;
  TFNZwAllocateUserPhysicalPages = TFNNtAllocateUserPhysicalPages;
  TFNZwAllocateUuids = TFNNtAllocateUuids;
  TFNZwAllocateVirtualMemory = TFNNtAllocateVirtualMemory;
  TFNZwAreMappedFilesTheSame = TFNNtAreMappedFilesTheSame;
  TFNZwAssignProcessToJobObject = TFNNtAssignProcessToJobObject;
  TFNZwCallbackReturn = TFNNtCallbackReturn;
  TFNZwCancelDeviceWakeupRequest = TFNNtCancelDeviceWakeupRequest;
  TFNZwCancelIoFile = TFNNtCancelIoFile;
  TFNZwCancelTimer = TFNNtCancelTimer;
  TFNZwClearEvent = TFNNtClearEvent;
  TFNZwClose = TFNNtClose;
  TFNZwCloseObjectAuditAlarm = TFNNtCloseObjectAuditAlarm;
  TFNZwCompleteConnectPort = TFNNtCompleteConnectPort;
  TFNZwConnectPort = TFNNtConnectPort;
  TFNZwContinue = TFNNtContinue;
  TFNZwCreateChannel = TFNNtCreateChannel;
  TFNZwCreateDirectoryObject = TFNNtCreateDirectoryObject;
  TFNZwCreateEvent = TFNNtCreateEvent;
  TFNZwCreateEventPair = TFNNtCreateEventPair;
  TFNZwCreateFile = TFNNtCreateFile;
  TFNZwCreateIoCompletion = TFNNtCreateIoCompletion;
  TFNZwCreateJobObject = TFNNtCreateJobObject;
  TFNZwCreateKey = TFNNtCreateKey;
  TFNZwCreateMailslotFile = TFNNtCreateMailslotFile;
  TFNZwCreateMutant = TFNNtCreateMutant;
  TFNZwCreateNamedPipeFile = TFNNtCreateNamedPipeFile;
  TFNZwCreatePagingFile = TFNNtCreatePagingFile;
  TFNZwCreatePort = TFNNtCreatePort;
  TFNZwCreateProcess = TFNNtCreateProcess;
  TFNZwCreateProfile = TFNNtCreateProfile;
  TFNZwCreateSection = TFNNtCreateSection;
  TFNZwCreateSemaphore = TFNNtCreateSemaphore;
  TFNZwCreateSymbolicLinkObject = TFNNtCreateSymbolicLinkObject;
  TFNZwCreateThread = TFNNtCreateThread;
  TFNZwCreateTimer = TFNNtCreateTimer;
  TFNZwCreateToken = TFNNtCreateToken;
  TFNZwCreateWaitablePort = TFNNtCreateWaitablePort;
  TFNZwCurrentTeb = TFNNtCurrentTeb;
  TFNZwDebugActiveProcess = TFNNtDebugActiveProcess;
  TFNZwDelayExecution = TFNNtDelayExecution;
  TFNZwDeleteAtom = TFNNtDeleteAtom;
  TFNZwDeleteFile = TFNNtDeleteFile;
  TFNZwDeleteKey = TFNNtDeleteKey;
  TFNZwDeleteObjectAuditAlarm = TFNNtDeleteObjectAuditAlarm;
  TFNZwDeleteValueKey = TFNNtDeleteValueKey;
  TFNZwDeviceIoControlFile = TFNNtDeviceIoControlFile;
  TFNZwDisplayString = TFNNtDisplayString;
  TFNZwDuplicateObject = TFNNtDuplicateObject;
  TFNZwDuplicateToken = TFNNtDuplicateToken;
  TFNZwEnumerateKey = TFNNtEnumerateKey;
  TFNZwEnumerateValueKey = TFNNtEnumerateValueKey;
  TFNZwExtendSection = TFNNtExtendSection;
  TFNZwFilterToken = TFNNtFilterToken;
  TFNZwFindAtom = TFNNtFindAtom;
  TFNZwFlushBuffersFile = TFNNtFlushBuffersFile;
  TFNZwFlushInstructionCache = TFNNtFlushInstructionCache;
  TFNZwFlushKey = TFNNtFlushKey;
  TFNZwFlushVirtualMemory = TFNNtFlushVirtualMemory;
  TFNZwFlushWriteBuffer = TFNNtFlushWriteBuffer;
  TFNZwFreeUserPhysicalPages = TFNNtFreeUserPhysicalPages;
  TFNZwFreeVirtualMemory = TFNNtFreeVirtualMemory;
  TFNZwFsControlFile = TFNNtFsControlFile;
  TFNZwGetContextThread = TFNNtGetContextThread;
  TFNZwGetCurrentProcessorNumber = TFNNtGetCurrentProcessorNumber;
  TFNZwGetDevicePowerState = TFNNtGetDevicePowerState;
  TFNZwGetPlugPlayEvent = TFNNtGetPlugPlayEvent;
  TFNZwGetTickCount = TFNNtGetTickCount;
  TFNZwGetWriteWatch = TFNNtGetWriteWatch;
  TFNZwImpersonateAnonymousToken = TFNNtImpersonateAnonymousToken;
  TFNZwImpersonateClientOfPort = TFNNtImpersonateClientOfPort;
  TFNZwImpersonateThread = TFNNtImpersonateThread;
  TFNZwInitializeRegistry = TFNNtInitializeRegistry;
  TFNZwInitiatePowerAction = TFNNtInitiatePowerAction;
  TFNZwIsSystemResumeAutomatic = TFNNtIsSystemResumeAutomatic;
  TFNZwListenChannel = TFNNtListenChannel;
  TFNZwListenPort = TFNNtListenPort;
  TFNZwLoadDriver = TFNNtLoadDriver;
  TFNZwLoadKey = TFNNtLoadKey;
  TFNZwLoadKey2 = TFNNtLoadKey2;
  TFNZwLockFile = TFNNtLockFile;
  TFNZwLockVirtualMemory = TFNNtLockVirtualMemory;
  TFNZwMakePermanentObject = TFNNtMakePermanentObject;
  TFNZwMakeTemporaryObject = TFNNtMakeTemporaryObject;
  TFNZwMapUserPhysicalPages = TFNNtMapUserPhysicalPages;
  TFNZwMapUserPhysicalPagesScatter = TFNNtMapUserPhysicalPagesScatter;
  TFNZwMapViewOfSection = TFNNtMapViewOfSection;
  TFNZwNotifyChangeDirectoryFile = TFNNtNotifyChangeDirectoryFile;
  TFNZwNotifyChangeKey = TFNNtNotifyChangeKey;
  TFNZwNotifyChangeMultipleKeys = TFNNtNotifyChangeMultipleKeys;
  TFNZwOpenChannel = TFNNtOpenChannel;
  TFNZwOpenDirectoryObject = TFNNtOpenDirectoryObject;
  TFNZwOpenEvent = TFNNtOpenEvent;
  TFNZwOpenEventPair = TFNNtOpenEventPair;
  TFNZwOpenFile = TFNNtOpenFile;
  TFNZwOpenIoCompletion = TFNNtOpenIoCompletion;
  TFNZwOpenJobObject = TFNNtOpenJobObject;
  TFNZwOpenKey = TFNNtOpenKey;
  TFNZwOpenMutant = TFNNtOpenMutant;
  TFNZwOpenObjectAuditAlarm = TFNNtOpenObjectAuditAlarm;
  TFNZwOpenProcess = TFNNtOpenProcess;
  TFNZwOpenProcessToken = TFNNtOpenProcessToken;
  TFNZwOpenSection = TFNNtOpenSection;
  TFNZwOpenSemaphore = TFNNtOpenSemaphore;
  TFNZwOpenSymbolicLinkObject = TFNNtOpenSymbolicLinkObject;
  TFNZwOpenThread = TFNNtOpenThread;
  TFNZwOpenThreadToken = TFNNtOpenThreadToken;
  TFNZwOpenTimer = TFNNtOpenTimer;
  TFNZwPlugPlayControl = TFNNtPlugPlayControl;
  TFNZwPowerInformation = TFNNtPowerInformation;
  TFNZwPrivilegeCheck = TFNNtPrivilegeCheck;
  TFNZwPrivilegeObjectAuditAlarm = TFNNtPrivilegeObjectAuditAlarm;
  TFNZwPrivilegedServiceAuditAlarm = TFNNtPrivilegedServiceAuditAlarm;
  TFNZwProtectVirtualMemory = TFNNtProtectVirtualMemory;
  TFNZwPulseEvent = TFNNtPulseEvent;
  TFNZwQueryAttributesFile = TFNNtQueryAttributesFile;
  TFNZwQueryDefaultLocale = TFNNtQueryDefaultLocale;
  TFNZwQueryDefaultUILanguage = TFNNtQueryDefaultUILanguage;
  TFNZwQueryDirectoryFile = TFNNtQueryDirectoryFile;
  TFNZwQueryDirectoryObject = TFNNtQueryDirectoryObject;
  TFNZwQueryEaFile = TFNNtQueryEaFile;
  TFNZwQueryEvent = TFNNtQueryEvent;
  TFNZwQueryFullAttributesFile = TFNNtQueryFullAttributesFile;
  TFNZwQueryInformationAtom = TFNNtQueryInformationAtom;
  TFNZwQueryInformationFile = TFNNtQueryInformationFile;
  TFNZwQueryInformationJobObject = TFNNtQueryInformationJobObject;
  TFNZwQueryInformationPort = TFNNtQueryInformationPort;
  TFNZwQueryInformationProcess = TFNNtQueryInformationProcess;
  TFNZwQueryInformationThread = TFNNtQueryInformationThread;
  TFNZwQueryInformationToken = TFNNtQueryInformationToken;
  TFNZwQueryInstallUILanguage = TFNNtQueryInstallUILanguage;
  TFNZwQueryIntervalProfile = TFNNtQueryIntervalProfile;
  TFNZwQueryIoCompletion = TFNNtQueryIoCompletion;
  TFNZwQueryKey = TFNNtQueryKey;
  TFNZwQueryMultipleValueKey = TFNNtQueryMultipleValueKey;
  TFNZwQueryMutant = TFNNtQueryMutant;
  TFNZwQueryObject = TFNNtQueryObject;
  TFNZwQueryOpenSubKeys = TFNNtQueryOpenSubKeys;
  TFNZwQueryPerformanceCounter = TFNNtQueryPerformanceCounter;
  TFNZwQueryPortInformationProcess = TFNNtQueryPortInformationProcess;
  TFNZwQueryQuotaInformationFile = TFNNtQueryQuotaInformationFile;
  TFNZwQuerySection = TFNNtQuerySection;
  TFNZwQuerySecurityObject = TFNNtQuerySecurityObject;
  TFNZwQuerySemaphore = TFNNtQuerySemaphore;
  TFNZwQuerySymbolicLinkObject = TFNNtQuerySymbolicLinkObject;
  TFNZwQuerySystemEnvironmentValue = TFNNtQuerySystemEnvironmentValue;
  TFNZwQuerySystemInformation = TFNNtQuerySystemInformation;
  TFNZwQuerySystemTime = TFNNtQuerySystemTime;
  TFNZwQueryTimer = TFNNtQueryTimer;
  TFNZwQueryTimerResolution = TFNNtQueryTimerResolution;
  TFNZwQueryValueKey = TFNNtQueryValueKey;
  TFNZwQueryVirtualMemory = TFNNtQueryVirtualMemory;
  TFNZwQueryVolumeInformationFile = TFNNtQueryVolumeInformationFile;
  TFNZwQueueApcThread = TFNNtQueueApcThread;
  TFNZwRaiseException = TFNNtRaiseException;
  TFNZwRaiseHardError = TFNNtRaiseHardError;
  TFNZwReadFile = TFNNtReadFile;
  TFNZwReadFileScatter = TFNNtReadFileScatter;
  TFNZwReadRequestData = TFNNtReadRequestData;
  TFNZwReadVirtualMemory = TFNNtReadVirtualMemory;
  TFNZwRegisterThreadTerminatePort = TFNNtRegisterThreadTerminatePort;
  TFNZwReleaseMutant = TFNNtReleaseMutant;
  TFNZwReleaseSemaphore = TFNNtReleaseSemaphore;
  TFNZwRemoveIoCompletion = TFNNtRemoveIoCompletion;
  TFNZwRemoveProcessDebug = TFNNtRemoveProcessDebug;
  TFNZwReplaceKey = TFNNtReplaceKey;
  TFNZwReplyPort = TFNNtReplyPort;
  TFNZwReplyWaitReceivePort = TFNNtReplyWaitReceivePort;
  TFNZwReplyWaitReceivePortEx = TFNNtReplyWaitReceivePortEx;
  TFNZwReplyWaitReplyPort = TFNNtReplyWaitReplyPort;
  TFNZwReplyWaitSendChannel = TFNNtReplyWaitSendChannel;
  TFNZwRequestDeviceWakeup = TFNNtRequestDeviceWakeup;
  TFNZwRequestPort = TFNNtRequestPort;
  TFNZwRequestWaitReplyPort = TFNNtRequestWaitReplyPort;
  TFNZwRequestWakeupLatency = TFNNtRequestWakeupLatency;
  TFNZwResetEvent = TFNNtResetEvent;
  TFNZwResetWriteWatch = TFNNtResetWriteWatch;
  TFNZwRestoreKey = TFNNtRestoreKey;
  TFNZwResumeProcess = TFNNtResumeProcess;
  TFNZwResumeThread = TFNNtResumeThread;
  TFNZwSaveKey = TFNNtSaveKey;
  TFNZwSaveKeyEx = TFNNtSaveKeyEx;
  TFNZwSaveMergedKeys = TFNNtSaveMergedKeys;
  TFNZwSecureConnectPort = TFNNtSecureConnectPort;
  TFNZwSendWaitReplyChannel = TFNNtSendWaitReplyChannel;
  TFNZwSetContextChannel = TFNNtSetContextChannel;
  TFNZwSetContextThread = TFNNtSetContextThread;
  TFNZwSetDefaultHardErrorPort = TFNNtSetDefaultHardErrorPort;
  TFNZwSetDefaultLocale = TFNNtSetDefaultLocale;
  TFNZwSetDefaultUILanguage = TFNNtSetDefaultUILanguage;
  TFNZwSetEaFile = TFNNtSetEaFile;
  TFNZwSetEvent = TFNNtSetEvent;
  TFNZwSetHighEventPair = TFNNtSetHighEventPair;
  TFNZwSetHighWaitLowEventPair = TFNNtSetHighWaitLowEventPair;
  TFNZwSetHighWaitLowThread = TFNNtSetHighWaitLowThread;
  TFNZwSetInformationFile = TFNNtSetInformationFile;
  TFNZwSetInformationJobObject = TFNNtSetInformationJobObject;
  TFNZwSetInformationKey = TFNNtSetInformationKey;
  TFNZwSetInformationObject = TFNNtSetInformationObject;
  TFNZwSetInformationProcess = TFNNtSetInformationProcess;
  TFNZwSetInformationThread = TFNNtSetInformationThread;
  TFNZwSetInformationToken = TFNNtSetInformationToken;
  TFNZwSetIntervalProfile = TFNNtSetIntervalProfile;
  TFNZwSetIoCompletion = TFNNtSetIoCompletion;
  TFNZwSetLdtEntries = TFNNtSetLdtEntries;
  TFNZwSetLowEventPair = TFNNtSetLowEventPair;
  TFNZwSetLowWaitHighEventPair = TFNNtSetLowWaitHighEventPair;
  TFNZwSetLowWaitHighThread = TFNNtSetLowWaitHighThread;
  TFNZwSetQuotaInformationFile = TFNNtSetQuotaInformationFile;
  TFNZwSetSecurityObject = TFNNtSetSecurityObject;
  TFNZwSetSystemEnvironmentValue = TFNNtSetSystemEnvironmentValue;
  TFNZwSetSystemInformation = TFNNtSetSystemInformation;
  TFNZwSetSystemPowerState = TFNNtSetSystemPowerState;
  TFNZwSetSystemTime = TFNNtSetSystemTime;
  TFNZwSetThreadExecutionState = TFNNtSetThreadExecutionState;
  TFNZwSetTimer = TFNNtSetTimer;
  TFNZwSetTimerResolution = TFNNtSetTimerResolution;
  TFNZwSetUuidSeed = TFNNtSetUuidSeed;
  TFNZwSetValueKey = TFNNtSetValueKey;
  TFNZwSetVolumeInformationFile = TFNNtSetVolumeInformationFile;
  TFNZwShutdownSystem = TFNNtShutdownSystem;
  TFNZwSignalAndWaitForSingleObject = TFNNtSignalAndWaitForSingleObject;
  TFNZwStartProfile = TFNNtStartProfile;
  TFNZwStopProfile = TFNNtStopProfile;
  TFNZwSuspendProcess = TFNNtSuspendProcess;
  TFNZwSuspendThread = TFNNtSuspendThread;
  TFNZwSystemDebugControl = TFNNtSystemDebugControl;
  TFNZwTerminateJobObject = TFNNtTerminateJobObject;
  TFNZwTerminateProcess = TFNNtTerminateProcess;
  TFNZwTerminateThread = TFNNtTerminateThread;
  TFNZwTestAlert = TFNNtTestAlert;
  TFNZwUnloadDriver = TFNNtUnloadDriver;
  TFNZwUnloadKey = TFNNtUnloadKey;
  TFNZwUnlockFile = TFNNtUnlockFile;
  TFNZwUnlockVirtualMemory = TFNNtUnlockVirtualMemory;
  TFNZwUnmapViewOfSection = TFNNtUnmapViewOfSection;
  TFNZwVdmControl = TFNNtVdmControl;
  TFNZwW32Call = TFNNtW32Call;
  TFNZwWaitForMultipleObjects = TFNNtWaitForMultipleObjects;
  TFNZwWaitForSingleObject = TFNNtWaitForSingleObject;
  TFNZwWaitHighEventPair = TFNNtWaitHighEventPair;
  TFNZwWaitLowEventPair = TFNNtWaitLowEventPair;
  TFNZwWriteFile = TFNNtWriteFile;
  TFNZwWriteFileGather = TFNNtWriteFileGather;
  TFNZwWriteRequestData = TFNNtWriteRequestData;
  TFNZwWriteVirtualMemory = TFNNtWriteVirtualMemory;
  TFNZwYieldExecution = TFNNtYieldExecution;

/// (Global) function pointers
var
  _CsrGetProcessId : Pointer = nil;
  _DbgQueryDebugFilterState : Pointer = nil;
  _DbgSetDebugFilterState : Pointer = nil;
  _KiRaiseUserExceptionDispatcher : Pointer = nil;
  _LdrAccessResource : Pointer = nil;
  _LdrAlternateResourcesEnabled : Pointer = nil;
  _LdrDisableThreadCalloutsForDll : Pointer = nil;
  _LdrGetDllHandle : Pointer = nil;
  _LdrGetProcedureAddress : Pointer = nil;
  _LdrLoadDll : Pointer = nil;
  _LdrQueryImageFileExecutionOptions : Pointer = nil;
  _LdrQueryProcessModuleInformation : Pointer = nil;
  _LdrShutdownProcess : Pointer = nil;
  _LdrShutdownThread : Pointer = nil;
  _LdrUnloadDll : Pointer = nil;
  _NtAcceptConnectPort : Pointer = nil;
  _NtAccessCheck : Pointer = nil;
  _NtAccessCheckAndAuditAlarm : Pointer = nil;
  _NtAccessCheckByType : Pointer = nil;
  _NtAccessCheckByTypeAndAuditAlarm : Pointer = nil;
  _NtAccessCheckByTypeResultList : Pointer = nil;
  _NtAccessCheckByTypeResultListAndAuditAlarm : Pointer = nil;
  _NtAccessCheckByTypeResultListAndAuditAlarmByHandle : Pointer = nil;
  _NtAddAtom : Pointer = nil;
  _NtAdjustGroupsToken : Pointer = nil;
  _NtAdjustPrivilegesToken : Pointer = nil;
  _NtAlertResumeThread : Pointer = nil;
  _NtAlertThread : Pointer = nil;
  _NtAllocateLocallyUniqueId : Pointer = nil;
  _NtAllocateUserPhysicalPages : Pointer = nil;
  _NtAllocateUuids : Pointer = nil;
  _NtAllocateVirtualMemory : Pointer = nil;
  _NtAreMappedFilesTheSame : Pointer = nil;
  _NtAssignProcessToJobObject : Pointer = nil;
  _NtCallbackReturn : Pointer = nil;
  _NtCancelDeviceWakeupRequest : Pointer = nil;
  _NtCancelIoFile : Pointer = nil;
  _NtCancelTimer : Pointer = nil;
  _NtClearEvent : Pointer = nil;

  _NtClose : Pointer = nil;

  _NtCloseObjectAuditAlarm : Pointer = nil;
  _NtCompleteConnectPort : Pointer = nil;
  _NtConnectPort : Pointer = nil;
  _NtContinue : Pointer = nil;
  _NtCreateChannel : Pointer = nil;
  _NtCreateDirectoryObject : Pointer = nil;
  _NtCreateEvent : Pointer = nil;
  _NtCreateEventPair : Pointer = nil;

  _NtCreateFile : Pointer = nil;

  _NtCreateIoCompletion : Pointer = nil;
  _NtCreateJobObject : Pointer = nil;
  _NtCreateKey : Pointer = nil;
  _NtCreateMailslotFile : Pointer = nil;
  _NtCreateMutant : Pointer = nil;
  _NtCreateNamedPipeFile : Pointer = nil;
  _NtCreatePagingFile : Pointer = nil;
  _NtCreatePort : Pointer = nil;
  _NtCreateProcess : Pointer = nil;
  _NtCreateProfile : Pointer = nil;
  _NtCreateSection : Pointer = nil;
  _NtCreateSemaphore : Pointer = nil;
  _NtCreateSymbolicLinkObject : Pointer = nil;
  _NtCreateThread : Pointer = nil;
  _NtCreateTimer : Pointer = nil;
  _NtCreateToken : Pointer = nil;
  _NtCreateWaitablePort : Pointer = nil;
  _NtCurrentTeb : Pointer = nil;
  _NtDebugActiveProcess : Pointer = nil;
  _NtDelayExecution : Pointer = nil;
  _NtDeleteAtom : Pointer = nil;
  _NtDeleteFile : Pointer = nil;
  _NtDeleteKey : Pointer = nil;
  _NtDeleteObjectAuditAlarm : Pointer = nil;
  _NtDeleteValueKey : Pointer = nil;

  _NtDeviceIoControlFile : Pointer = nil;

  _NtDisplayString : Pointer = nil;
  _NtDuplicateObject : Pointer = nil;
  _NtDuplicateToken : Pointer = nil;
  _NtEnumerateKey : Pointer = nil;
  _NtEnumerateValueKey : Pointer = nil;
  _NtExtendSection : Pointer = nil;
  _NtFilterToken : Pointer = nil;
  _NtFindAtom : Pointer = nil;
  _NtFlushBuffersFile : Pointer = nil;
  _NtFlushInstructionCache : Pointer = nil;
  _NtFlushKey : Pointer = nil;
  _NtFlushVirtualMemory : Pointer = nil;
  _NtFlushWriteBuffer : Pointer = nil;
  _NtFreeUserPhysicalPages : Pointer = nil;
  _NtFreeVirtualMemory : Pointer = nil;
  _NtFsControlFile : Pointer = nil;
  _NtGetContextThread : Pointer = nil;
  _NtGetCurrentProcessorNumber : Pointer = nil;
  _NtGetDevicePowerState : Pointer = nil;
  _NtGetPlugPlayEvent : Pointer = nil;
  _NtGetTickCount : Pointer = nil;
  _NtGetWriteWatch : Pointer = nil;
  _NtImpersonateAnonymousToken : Pointer = nil;
  _NtImpersonateClientOfPort : Pointer = nil;
  _NtImpersonateThread : Pointer = nil;
  _NtInitializeRegistry : Pointer = nil;
  _NtInitiatePowerAction : Pointer = nil;
  _NtIsSystemResumeAutomatic : Pointer = nil;
  _NtListenChannel : Pointer = nil;
  _NtListenPort : Pointer = nil;
  _NtLoadDriver : Pointer = nil;
  _NtLoadKey : Pointer = nil;
  _NtLoadKey2 : Pointer = nil;
  _NtLockFile : Pointer = nil;
  _NtLockVirtualMemory : Pointer = nil;
  _NtMakePermanentObject : Pointer = nil;
  _NtMakeTemporaryObject : Pointer = nil;
  _NtMapUserPhysicalPages : Pointer = nil;
  _NtMapUserPhysicalPagesScatter : Pointer = nil;
  _NtMapViewOfSection : Pointer = nil;
  _NtNotifyChangeDirectoryFile : Pointer = nil;
  _NtNotifyChangeKey : Pointer = nil;
  _NtNotifyChangeMultipleKeys : Pointer = nil;
  _NtOpenChannel : Pointer = nil;
  _NtOpenDirectoryObject : Pointer = nil;
  _NtOpenEvent : Pointer = nil;
  _NtOpenEventPair : Pointer = nil;

  _NtOpenFile : Pointer = nil;

  _NtOpenIoCompletion : Pointer = nil;
  _NtOpenJobObject : Pointer = nil;
  _NtOpenKey : Pointer = nil;
  _NtOpenMutant : Pointer = nil;
  _NtOpenObjectAuditAlarm : Pointer = nil;
  _NtOpenProcess : Pointer = nil;
  _NtOpenProcessToken : Pointer = nil;
  _NtOpenSection : Pointer = nil;
  _NtOpenSemaphore : Pointer = nil;
  _NtOpenSymbolicLinkObject : Pointer = nil;
  _NtOpenThread : Pointer = nil;
  _NtOpenThreadToken : Pointer = nil;
  _NtOpenTimer : Pointer = nil;
  _NtPlugPlayControl : Pointer = nil;
  _NtPowerInformation : Pointer = nil;
  _NtPrivilegeCheck : Pointer = nil;
  _NtPrivilegeObjectAuditAlarm : Pointer = nil;
  _NtPrivilegedServiceAuditAlarm : Pointer = nil;
  _NtProtectVirtualMemory : Pointer = nil;
  _NtPulseEvent : Pointer = nil;
  _NtQueryAttributesFile : Pointer = nil;
  _NtQueryDefaultLocale : Pointer = nil;
  _NtQueryDefaultUILanguage : Pointer = nil;
  _NtQueryDirectoryFile : Pointer = nil;
  _NtQueryDirectoryObject : Pointer = nil;
  _NtQueryEaFile : Pointer = nil;
  _NtQueryEvent : Pointer = nil;
  _NtQueryFullAttributesFile : Pointer = nil;
  _NtQueryInformationAtom : Pointer = nil;
  _NtQueryInformationFile : Pointer = nil;
  _NtQueryInformationJobObject : Pointer = nil;
  _NtQueryInformationPort : Pointer = nil;
  _NtQueryInformationProcess : Pointer = nil;
  _NtQueryInformationThread : Pointer = nil;
  _NtQueryInformationToken : Pointer = nil;
  _NtQueryInstallUILanguage : Pointer = nil;
  _NtQueryIntervalProfile : Pointer = nil;
  _NtQueryIoCompletion : Pointer = nil;
  _NtQueryKey : Pointer = nil;
  _NtQueryMultipleValueKey : Pointer = nil;
  _NtQueryMutant : Pointer = nil;
  _NtQueryObject : Pointer = nil;
  _NtQueryOpenSubKeys : Pointer = nil;
  _NtQueryPerformanceCounter : Pointer = nil;
  _NtQueryPortInformationProcess : Pointer = nil;
  _NtQueryQuotaInformationFile : Pointer = nil;
  _NtQuerySection : Pointer = nil;
  _NtQuerySecurityObject : Pointer = nil;
  _NtQuerySemaphore : Pointer = nil;
  _NtQuerySymbolicLinkObject : Pointer = nil;
  _NtQuerySystemEnvironmentValue : Pointer = nil;
  _NtQuerySystemInformation : Pointer = nil;
  _NtQuerySystemTime : Pointer = nil;
  _NtQueryTimer : Pointer = nil;
  _NtQueryTimerResolution : Pointer = nil;
  _NtQueryValueKey : Pointer = nil;
  _NtQueryVirtualMemory : Pointer = nil;
  _NtQueryVolumeInformationFile : Pointer = nil;
  _NtQueueApcThread : Pointer = nil;
  _NtRaiseException : Pointer = nil;
  _NtRaiseHardError : Pointer = nil;
  _NtReadFile : Pointer = nil;
  _NtReadFileScatter : Pointer = nil;
  _NtReadRequestData : Pointer = nil;
  _NtReadVirtualMemory : Pointer = nil;
  _NtRegisterThreadTerminatePort : Pointer = nil;
  _NtReleaseMutant : Pointer = nil;
  _NtReleaseSemaphore : Pointer = nil;
  _NtRemoveIoCompletion : Pointer = nil;
  _NtRemoveProcessDebug : Pointer = nil;
  _NtReplaceKey : Pointer = nil;
  _NtReplyPort : Pointer = nil;
  _NtReplyWaitReceivePort : Pointer = nil;
  _NtReplyWaitReceivePortEx : Pointer = nil;
  _NtReplyWaitReplyPort : Pointer = nil;
  _NtReplyWaitSendChannel : Pointer = nil;
  _NtRequestDeviceWakeup : Pointer = nil;
  _NtRequestPort : Pointer = nil;
  _NtRequestWaitReplyPort : Pointer = nil;
  _NtRequestWakeupLatency : Pointer = nil;
  _NtResetEvent : Pointer = nil;
  _NtResetWriteWatch : Pointer = nil;
  _NtRestoreKey : Pointer = nil;
  _NtResumeProcess : Pointer = nil;
  _NtResumeThread : Pointer = nil;
  _NtSaveKey : Pointer = nil;
  _NtSaveKeyEx : Pointer = nil;
  _NtSaveMergedKeys : Pointer = nil;
  _NtSecureConnectPort : Pointer = nil;
  _NtSendWaitReplyChannel : Pointer = nil;
  _NtSetContextChannel : Pointer = nil;
  _NtSetContextThread : Pointer = nil;
  _NtSetDefaultHardErrorPort : Pointer = nil;
  _NtSetDefaultLocale : Pointer = nil;
  _NtSetDefaultUILanguage : Pointer = nil;
  _NtSetEaFile : Pointer = nil;
  _NtSetEvent : Pointer = nil;
  _NtSetHighEventPair : Pointer = nil;
  _NtSetHighWaitLowEventPair : Pointer = nil;
  _NtSetHighWaitLowThread : Pointer = nil;
  _NtSetInformationFile : Pointer = nil;
  _NtSetInformationJobObject : Pointer = nil;
  _NtSetInformationKey : Pointer = nil;
  _NtSetInformationObject : Pointer = nil;
  _NtSetInformationProcess : Pointer = nil;
  _NtSetInformationThread : Pointer = nil;
  _NtSetInformationToken : Pointer = nil;
  _NtSetIntervalProfile : Pointer = nil;
  _NtSetIoCompletion : Pointer = nil;
  _NtSetLdtEntries : Pointer = nil;
  _NtSetLowEventPair : Pointer = nil;
  _NtSetLowWaitHighEventPair : Pointer = nil;
  _NtSetLowWaitHighThread : Pointer = nil;
  _NtSetQuotaInformationFile : Pointer = nil;
  _NtSetSecurityObject : Pointer = nil;
  _NtSetSystemEnvironmentValue : Pointer = nil;
  _NtSetSystemInformation : Pointer = nil;
  _NtSetSystemPowerState : Pointer = nil;
  _NtSetSystemTime : Pointer = nil;
  _NtSetThreadExecutionState : Pointer = nil;
  _NtSetTimer : Pointer = nil;
  _NtSetTimerResolution : Pointer = nil;
  _NtSetUuidSeed : Pointer = nil;
  _NtSetValueKey : Pointer = nil;
  _NtSetVolumeInformationFile : Pointer = nil;
  _NtShutdownSystem : Pointer = nil;
  _NtSignalAndWaitForSingleObject : Pointer = nil;
  _NtStartProfile : Pointer = nil;
  _NtStopProfile : Pointer = nil;
  _NtSuspendProcess : Pointer = nil;
  _NtSuspendThread : Pointer = nil;
  _NtSystemDebugControl : Pointer = nil;
  _NtTerminateJobObject : Pointer = nil;
  _NtTerminateProcess : Pointer = nil;
  _NtTerminateThread : Pointer = nil;
  _NtTestAlert : Pointer = nil;
  _NtUnloadDriver : Pointer = nil;
  _NtUnloadKey : Pointer = nil;
  _NtUnlockFile : Pointer = nil;
  _NtUnlockVirtualMemory : Pointer = nil;
  _NtUnmapViewOfSection : Pointer = nil;
  _NtVdmControl : Pointer = nil;
  _NtW32Call : Pointer = nil;
  _NtWaitForMultipleObjects : Pointer = nil;
  _NtWaitForSingleObject : Pointer = nil;
  _NtWaitHighEventPair : Pointer = nil;
  _NtWaitLowEventPair : Pointer = nil;
  _NtWriteFile : Pointer = nil;
  _NtWriteFileGather : Pointer = nil;
  _NtWriteRequestData : Pointer = nil;
  _NtWriteVirtualMemory : Pointer = nil;
  _NtYieldExecution : Pointer = nil;
  _RtlAbsoluteToSelfRelativeSD : Pointer = nil;
  _RtlAcquirePebLock : Pointer = nil;
  _RtlAddAccessAllowedAce : Pointer = nil;
  _RtlAddAccessAllowedAceEx : Pointer = nil;
  _RtlAddAccessDeniedAce : Pointer = nil;
  _RtlAddAccessDeniedAceEx : Pointer = nil;
  _RtlAddAce : Pointer = nil;
  _RtlAddAuditAccessAce : Pointer = nil;
  _RtlAddAuditAccessAceEx : Pointer = nil;
  _RtlAddRange : Pointer = nil;
  _RtlAddVectoredExceptionHandler : Pointer = nil;
  _RtlAdjustPrivilege : Pointer = nil;
  _RtlAllocateAndInitializeSid : Pointer = nil;
  _RtlAllocateHeap : Pointer = nil;
  _RtlAnsiCharToUnicodeChar : Pointer = nil;
  _RtlAnsiStringToUnicodeSize : Pointer = nil;
  _RtlAnsiStringToUnicodeString : Pointer = nil;
  _RtlAppendAsciizToString : Pointer = nil;
  _RtlAppendStringToString : Pointer = nil;
  _RtlAppendUnicodeStringToString : Pointer = nil;
  _RtlAppendUnicodeToString : Pointer = nil;
  _RtlAreAllAccessesGranted : Pointer = nil;
  _RtlAreAnyAccessesGranted : Pointer = nil;
  _RtlAreBitsClear : Pointer = nil;
  _RtlAreBitsSet : Pointer = nil;
  _RtlAssert : Pointer = nil;
  {$IFNDEF JWA_INCLUDEMODE}
  _RtlCaptureContext : Pointer = nil;
  _RtlCharToInteger : Pointer = nil;
  {$ENDIF JWA_INCLUDEMODE}
  _RtlCheckForOrphanedCriticalSections : Pointer = nil;
  _RtlCheckRegistryKey : Pointer = nil;
  _RtlClearAllBits : Pointer = nil;
  _RtlClearBits : Pointer = nil;
  _RtlCompactHeap : Pointer = nil;
  {$IFNDEF JWA_INCLUDEMODE}
  _RtlCompareMemory : Pointer = nil;
  {$ENDIF JWA_INCLUDEMODE}
  _RtlCompareMemoryUlong : Pointer = nil;
  _RtlCompareString : Pointer = nil;
  _RtlCompareUnicodeString : Pointer = nil;
  _RtlConvertLongToLargeInteger : Pointer = nil;
  _RtlConvertSidToUnicodeString : Pointer = nil;
  _RtlConvertUlongToLargeInteger : Pointer = nil;
  _RtlCopyLuid : Pointer = nil;
  _RtlCopyRangeList : Pointer = nil;
  _RtlCopySecurityDescriptor : Pointer = nil;
  _RtlCopySid : Pointer = nil;
  _RtlCopyString : Pointer = nil;
  _RtlCopyUnicodeString : Pointer = nil;
  _RtlCreateAcl : Pointer = nil;
  _RtlCreateHeap : Pointer = nil;
  _RtlCreateProcessParameters : Pointer = nil;
  _RtlCreateQueryDebugBuffer : Pointer = nil;
  _RtlCreateRegistryKey : Pointer = nil;
  _RtlCreateSecurityDescriptor : Pointer = nil;
  _RtlCreateUnicodeString : Pointer = nil;
  _RtlCreateUnicodeStringFromAsciiz : Pointer = nil;
  _RtlCreateUserProcess : Pointer = nil;
  _RtlCreateUserThread : Pointer = nil;
  _RtlCutoverTimeToSystemTime : Pointer = nil;
  _RtlDeNormalizeProcessParams : Pointer = nil;
  _RtlDeleteAce : Pointer = nil;
  _RtlDeleteCriticalSection : Pointer = nil;
  _RtlDeleteOwnersRanges : Pointer = nil;
  _RtlDeleteRange : Pointer = nil;
  _RtlDeleteRegistryValue : Pointer = nil;
  _RtlDestroyHeap : Pointer = nil;
  _RtlDestroyProcessParameters : Pointer = nil;
  _RtlDestroyQueryDebugBuffer : Pointer = nil;
  _RtlDetermineDosPathNameType_U : Pointer = nil;
  _RtlDnsHostNameToComputerName : Pointer = nil;
  _RtlDoesFileExists_U : Pointer = nil;
  _RtlDosPathNameToNtPathName_U : Pointer = nil;
  _RtlDosSearchPath_U : Pointer = nil;
  _RtlDowncaseUnicodeChar : Pointer = nil;
  _RtlDowncaseUnicodeString : Pointer = nil;
  _RtlDuplicateUnicodeString : Pointer = nil;
  _RtlEnableEarlyCriticalSectionEventCreation : Pointer = nil;
  _RtlEnlargedIntegerMultiply : Pointer = nil;
  _RtlEnlargedUnsignedDivide : Pointer = nil;
  _RtlEnlargedUnsignedMultiply : Pointer = nil;
  _RtlEnterCriticalSection : Pointer = nil;
  _RtlEqualComputerName : Pointer = nil;
  _RtlEqualDomainName : Pointer = nil;
  _RtlEqualLuid : Pointer = nil;
  _RtlEqualPrefixSid : Pointer = nil;
  _RtlEqualSid : Pointer = nil;
  _RtlEqualString : Pointer = nil;
  _RtlEqualUnicodeString : Pointer = nil;
  _RtlEraseUnicodeString : Pointer = nil;
  _RtlExpandEnvironmentStrings_U : Pointer = nil;
  _RtlExtendedIntegerMultiply : Pointer = nil;
  _RtlExtendedLargeIntegerDivide : Pointer = nil;
  _RtlExtendedMagicDivide : Pointer = nil;
  _RtlFillMemory : Pointer = nil;
  _RtlFillMemoryUlong : Pointer = nil;
  _RtlFindCharInUnicodeString : Pointer = nil;
  _RtlFindClearBits : Pointer = nil;
  _RtlFindClearBitsAndSet : Pointer = nil;
  _RtlFindLastBackwardRunClear : Pointer = nil;
  _RtlFindLeastSignificantBit : Pointer = nil;
  _RtlFindLongestRunClear : Pointer = nil;
  _RtlFindMostSignificantBit : Pointer = nil;
  _RtlFindNextForwardRunClear : Pointer = nil;
  _RtlFindRange : Pointer = nil;
  _RtlFindSetBits : Pointer = nil;
  _RtlFindSetBitsAndClear : Pointer = nil;
  _RtlFirstFreeAce : Pointer = nil;
  _RtlFormatCurrentUserKeyPath : Pointer = nil;
  _RtlFreeAnsiString : Pointer = nil;
  _RtlFreeHeap : Pointer = nil;
  _RtlFreeOemString : Pointer = nil;
  _RtlFreeRangeList : Pointer = nil;
  _RtlFreeSid : Pointer = nil;
  _RtlFreeUnicodeString : Pointer = nil;
  _RtlGUIDFromString : Pointer = nil;
  _RtlGetAce : Pointer = nil;
  _RtlGetCallersAddress : Pointer = nil;
  _RtlGetControlSecurityDescriptor : Pointer = nil;
  _RtlGetCurrentDirectory_U : Pointer = nil;
  _RtlGetCurrentPeb : Pointer = nil;
  _RtlGetDaclSecurityDescriptor : Pointer = nil;
  _RtlGetFirstRange : Pointer = nil;
  _RtlGetFullPathName_U : Pointer = nil;
  _RtlGetGroupSecurityDescriptor : Pointer = nil;
  _RtlGetLastNtStatus : Pointer = nil;
  _RtlGetLongestNtPathLength : Pointer = nil;
  _RtlGetNextRange : Pointer = nil;
  _RtlGetNtGlobalFlags : Pointer = nil;
  _RtlGetNtProductType : Pointer = nil;
  _RtlGetNtVersionNumbers : Pointer = nil;
  _RtlGetOwnerSecurityDescriptor : Pointer = nil;
  _RtlGetProcessHeaps : Pointer = nil;
  _RtlGetSaclSecurityDescriptor : Pointer = nil;
  _RtlGetVersion : Pointer = nil;
  _RtlIdentifierAuthoritySid : Pointer = nil;
  _RtlImageDirectoryEntryToData : Pointer = nil;
  _RtlImageNtHeader : Pointer = nil;
  _RtlImageNtHeaderEx : Pointer = nil;
  _RtlImageRvaToSection : Pointer = nil;
  _RtlImageRvaToVa : Pointer = nil;
  _RtlImpersonateSelf : Pointer = nil;
  _RtlInitAnsiString : Pointer = nil;
  _RtlInitAnsiStringEx : Pointer = nil;
  _RtlInitString : Pointer = nil;
  _RtlInitUnicodeString : Pointer = nil;
  _RtlInitUnicodeStringEx : Pointer = nil;
  _RtlInitializeBitMap : Pointer = nil;
  _RtlInitializeCriticalSection : Pointer = nil;
  _RtlInitializeCriticalSectionAndSpinCount : Pointer = nil;
  _RtlInitializeRangeList : Pointer = nil;
  {$IFNDEF JWA_INCLUDEMODE}
  _RtlInitializeSListHead : Pointer = nil;
  {$ENDIF JWA_INCLUDEMODE}
  _RtlInitializeSid : Pointer = nil;
  _RtlInt64ToUnicodeString : Pointer = nil;
  _RtlIntegerToChar : Pointer = nil;
  _RtlIntegerToUnicodeString : Pointer = nil;
  {$IFNDEF JWA_INCLUDEMODE}
  _RtlInterlockedFlushSList : Pointer = nil;
  _RtlInterlockedPopEntrySList : Pointer = nil;
  {$ENDIF JWA_INCLUDEMODE}
  _RtlInterlockedPushEntrySList : Pointer = nil;
  _RtlInvertRangeList : Pointer = nil;
  _RtlIpv4AddressToStringA : Pointer = nil;
  _RtlIpv4AddressToStringW : Pointer = nil;
  _RtlIsDosDeviceName_U : Pointer = nil;
  {.$IFNDEF JWA_INCLUDEMODE}
  _RtlIsNameLegalDOS8Dot3 : Pointer = nil;
  {.$ENDIF JWA_INCLUDEMODE}
  _RtlIsRangeAvailable : Pointer = nil;
  _RtlIsTextUnicode : Pointer = nil;
  _RtlLargeIntegerAdd : Pointer = nil;
  _RtlLargeIntegerArithmeticShift : Pointer = nil;
  _RtlLargeIntegerDivide : Pointer = nil;
  _RtlLargeIntegerNegate : Pointer = nil;
  _RtlLargeIntegerShiftLeft : Pointer = nil;
  _RtlLargeIntegerShiftRight : Pointer = nil;
  _RtlLargeIntegerSubtract : Pointer = nil;
  _RtlLargeIntegerToChar : Pointer = nil;
  _RtlLeaveCriticalSection : Pointer = nil;
  _RtlLengthRequiredSid : Pointer = nil;
  _RtlLengthSecurityDescriptor : Pointer = nil;
  _RtlLengthSid : Pointer = nil;
  {.$IFNDEF JWA_INCLUDEMODE}
  _RtlLocalTimeToSystemTime : Pointer = nil;
  {.$ENDIF JWA_INCLUDEMODE}
  _RtlLockHeap : Pointer = nil;
  _RtlMakeSelfRelativeSD : Pointer = nil;
  _RtlMapGenericMask : Pointer = nil;
  _RtlMapSecurityErrorToNtStatus : Pointer = nil;
  _RtlMergeRangeLists : Pointer = nil;
  _RtlMoveMemory : Pointer = nil;
  _RtlNormalizeProcessParams : Pointer = nil;
  _RtlNtStatusToDosError : Pointer = nil;
  _RtlNtStatusToDosErrorNoTeb : Pointer = nil;
  _RtlNumberOfClearBits : Pointer = nil;
  _RtlNumberOfSetBits : Pointer = nil;
  _RtlOemStringToUnicodeSize : Pointer = nil;
  _RtlOemStringToUnicodeString : Pointer = nil;
  _RtlOemToUnicodeN : Pointer = nil;
  _RtlOpenCurrentUser : Pointer = nil;
  _RtlPrefixString : Pointer = nil;
  _RtlPrefixUnicodeString : Pointer = nil;
  _RtlQueryDepthSList : Pointer = nil;
  _RtlQueryEnvironmentVariable_U : Pointer = nil;
  _RtlQueryInformationAcl : Pointer = nil;
  _RtlQueryProcessDebugInformation : Pointer = nil;
  _RtlQueryRegistryValues : Pointer = nil;
  _RtlRaiseStatus : Pointer = nil;
  _RtlRandom : Pointer = nil;
  _RtlRandomEx : Pointer = nil;
  _RtlReAllocateHeap : Pointer = nil;
  _RtlReleasePebLock : Pointer = nil;
  _RtlRemoveVectoredExceptionHandler : Pointer = nil;
  _RtlRestoreLastWin32Error : Pointer = nil;
  _RtlRunDecodeUnicodeString : Pointer = nil;
  _RtlRunEncodeUnicodeString : Pointer = nil;
  _RtlSecondsSince1970ToTime : Pointer = nil;
  _RtlSecondsSince1980ToTime : Pointer = nil;
  _RtlSelfRelativeToAbsoluteSD : Pointer = nil;
  _RtlSetAllBits : Pointer = nil;
  _RtlSetBits : Pointer = nil;
  _RtlSetControlSecurityDescriptor : Pointer = nil;
  _RtlSetCriticalSectionSpinCount : Pointer = nil;
  _RtlSetCurrentDirectory_U : Pointer = nil;
  _RtlSetDaclSecurityDescriptor : Pointer = nil;
  _RtlSetGroupSecurityDescriptor : Pointer = nil;
  _RtlSetInformationAcl : Pointer = nil;
  _RtlSetLastWin32ErrorAndNtStatusFromNtStatus : Pointer = nil;
  _RtlSetOwnerSecurityDescriptor : Pointer = nil;
  _RtlSetProcessIsCritical : Pointer = nil;
  _RtlSetSaclSecurityDescriptor : Pointer = nil;
  _RtlSetThreadIsCritical : Pointer = nil;
  _RtlSizeHeap : Pointer = nil;
  _RtlStringFromGUID : Pointer = nil;
  _RtlSubAuthorityCountSid : Pointer = nil;
  _RtlSubAuthoritySid : Pointer = nil;
  _RtlSystemTimeToLocalTime : Pointer = nil;
  _RtlTimeFieldsToTime : Pointer = nil;
  _RtlTimeToElapsedTimeFields : Pointer = nil;
  {.$IFNDEF JWA_INCLUDEMODE}
  _RtlTimeToSecondsSince1970 : Pointer = nil;
  {.$ENDIF JWA_INCLUDEMODE}
  _RtlTimeToSecondsSince1980 : Pointer = nil;
  _RtlTimeToTimeFields : Pointer = nil;
  _RtlTryEnterCriticalSection : Pointer = nil;
  _RtlUnicodeStringToAnsiSize : Pointer = nil;
  {.$IFNDEF JWA_INCLUDEMODE}
  _RtlUnicodeStringToAnsiString : Pointer = nil;
  {.$ENDIF JWA_INCLUDEMODE}
  _RtlUnicodeStringToCountedOemString : Pointer = nil;
  _RtlUnicodeStringToInteger : Pointer = nil;
  _RtlUnicodeStringToOemSize : Pointer = nil;
  {.$IFNDEF JWA_INCLUDEMODE}
  _RtlUnicodeStringToOemString : Pointer = nil;
  _RtlUnicodeToMultiByteSize : Pointer = nil;
  {,$ENDIF JWA_INCLUDEMODE}
  _RtlUniform : Pointer = nil;
  _RtlUnwind : Pointer = nil;
  _RtlUpcaseUnicodeChar : Pointer = nil;
  _RtlUpcaseUnicodeString : Pointer = nil;
  _RtlUpcaseUnicodeStringToAnsiString : Pointer = nil;
  _RtlUpcaseUnicodeStringToCountedOemString : Pointer = nil;
  _RtlUpcaseUnicodeStringToOemString : Pointer = nil;
  _RtlUpcaseUnicodeToMultiByteN : Pointer = nil;
  _RtlUpcaseUnicodeToOemN : Pointer = nil;
  _RtlUpperChar : Pointer = nil;
  _RtlUpperString : Pointer = nil;
  _RtlValidAcl : Pointer = nil;
  _RtlValidRelativeSecurityDescriptor : Pointer = nil;
  _RtlValidSecurityDescriptor : Pointer = nil;
  _RtlValidSid : Pointer = nil;
  _RtlValidateHeap : Pointer = nil;
  _RtlValidateUnicodeString : Pointer = nil;
  _RtlVerifyVersionInfo : Pointer = nil;
  _RtlVolumeDeviceToDosName : Pointer = nil;
  _RtlWriteRegistryValue : Pointer = nil;
  _RtlZeroHeap : Pointer = nil;
  _RtlZeroMemory : Pointer = nil;
  _RtlpNotOwnerCriticalSection : Pointer = nil;
  _RtlpNtCreateKey : Pointer = nil;
  _RtlpNtEnumerateSubKey : Pointer = nil;
  _RtlpNtMakeTemporaryKey : Pointer = nil;
  _RtlpNtOpenKey : Pointer = nil;
  _RtlpNtQueryValueKey : Pointer = nil;
  _RtlpNtSetValueKey : Pointer = nil;
  _RtlxAnsiStringToUnicodeSize : Pointer = nil;
  _RtlxOemStringToUnicodeSize : Pointer = nil;
  _RtlxUnicodeStringToAnsiSize : Pointer = nil;
  _RtlxUnicodeStringToOemSize : Pointer = nil;
  {$IFNDEF JWA_INCLUDEMODE}
  _VerSetConditionMask : Pointer = nil;
  {$ENDIF JWA_INCLUDEMODE}

// Dynamic version of CsrGetProcessId
function  CsrGetProcessId(): DWORD; stdcall;
begin
  GetProcedureAddress(_CsrGetProcessId, ntdll, 'CsrGetProcessId');
  Result := TFNCsrGetProcessId(_CsrGetProcessId)();
end;

// Dynamic version of DbgQueryDebugFilterState
function  DbgQueryDebugFilterState(
    ComponentId : ULONG;
    Level : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_DbgQueryDebugFilterState, ntdll, 'DbgQueryDebugFilterState');
  Result := TFNDbgQueryDebugFilterState(_DbgQueryDebugFilterState)(
    ComponentId, Level
  );
end;

// Dynamic version of DbgSetDebugFilterState
function  DbgSetDebugFilterState(
    ComponentId : ULONG;
    Level : ULONG;
    State : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_DbgSetDebugFilterState, ntdll, 'DbgSetDebugFilterState');
  Result := TFNDbgSetDebugFilterState(_DbgSetDebugFilterState)(
    ComponentId, Level, State
  );
end;

// Dynamic version of KiRaiseUserExceptionDispatcher
function  KiRaiseUserExceptionDispatcher(): ULONG; stdcall;
begin
  GetProcedureAddress(_KiRaiseUserExceptionDispatcher, ntdll, 'KiRaiseUserExceptionDispatcher');
  Result := TFNKiRaiseUserExceptionDispatcher(_KiRaiseUserExceptionDispatcher)();
end;

// Dynamic version of LdrAccessResource
function  LdrAccessResource(
    hModule : HANDLE;
    ResourceDataEntry : PIMAGE_RESOURCE_DATA_ENTRY;
    Address : PPVOID;
    dwSize : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_LdrAccessResource, ntdll, 'LdrAccessResource');
  Result := TFNLdrAccessResource(_LdrAccessResource)(
    hModule, ResourceDataEntry, Address, dwSize
  );
end;

// Dynamic version of LdrAlternateResourcesEnabled
function  LdrAlternateResourcesEnabled(): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_LdrAlternateResourcesEnabled, ntdll, 'LdrAlternateResourcesEnabled');
  Result := TFNLdrAlternateResourcesEnabled(_LdrAlternateResourcesEnabled)();
end;

// Dynamic version of LdrDisableThreadCalloutsForDll
function  LdrDisableThreadCalloutsForDll(
    hModule : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_LdrDisableThreadCalloutsForDll, ntdll, 'LdrDisableThreadCalloutsForDll');
  Result := TFNLdrDisableThreadCalloutsForDll(_LdrDisableThreadCalloutsForDll)(
    hModule
  );
end;

// Dynamic version of LdrGetDllHandle
function  LdrGetDllHandle(
    pwPath : PWORD;
    pReserved : PVOID;
    pusPath : PUNICODE_STRING;
    var phModule : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_LdrGetDllHandle, ntdll, 'LdrGetDllHandle');
  Result := TFNLdrGetDllHandle(_LdrGetDllHandle)(
    pwPath, pReserved, pusPath, phModule
  );
end;

// Dynamic version of LdrGetProcedureAddress
function  LdrGetProcedureAddress(
    hModule : HANDLE;
    dwOrdinal : ULONG;
    psName : PSTRING;
    var pProcedure : PVOID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_LdrGetProcedureAddress, ntdll, 'LdrGetProcedureAddress');
  Result := TFNLdrGetProcedureAddress(_LdrGetProcedureAddress)(
    hModule, dwOrdinal, psName, pProcedure
  );
end;

// Dynamic version of LdrLoadDll
function  LdrLoadDll(
    pwPath : PWORD;
    pdwFlags : PDWORD;
    pusPath : PUNICODE_STRING;
    var phModule : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_LdrLoadDll, ntdll, 'LdrLoadDll');
  Result := TFNLdrLoadDll(_LdrLoadDll)(
    pwPath, pdwFlags, pusPath, phModule
  );
end;

// Dynamic version of LdrQueryImageFileExecutionOptions
function  LdrQueryImageFileExecutionOptions(
    pusImagePath : PUNICODE_STRING;
    pwOptionName : PWORD;
    dwRequestedType : DWORD;
    pData : PVOID;
    dwSize : DWORD;
    pdwSize : PDWORD
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_LdrQueryImageFileExecutionOptions, ntdll, 'LdrQueryImageFileExecutionOptions');
  Result := TFNLdrQueryImageFileExecutionOptions(_LdrQueryImageFileExecutionOptions)(
    pusImagePath, pwOptionName, dwRequestedType, pData, dwSize, pdwSize
  );
end;

// Dynamic version of LdrQueryProcessModuleInformation
function  LdrQueryProcessModuleInformation(
    psmi : PSYSTEM_MODULE_INFORMATION;
    dwSize : DWORD;
    pdwSize : PDWORD
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_LdrQueryProcessModuleInformation, ntdll, 'LdrQueryProcessModuleInformation');
  Result := TFNLdrQueryProcessModuleInformation(_LdrQueryProcessModuleInformation)(
    psmi, dwSize, pdwSize
  );
end;

// Dynamic version of LdrShutdownProcess
procedure LdrShutdownProcess(); stdcall;
begin
  GetProcedureAddress(_LdrShutdownProcess, ntdll, 'LdrShutdownProcess');
  TFNLdrShutdownProcess(_LdrShutdownProcess)();
end;

// Dynamic version of LdrShutdownThread
procedure LdrShutdownThread(); stdcall;
begin
  GetProcedureAddress(_LdrShutdownThread, ntdll, 'LdrShutdownThread');
  TFNLdrShutdownThread(_LdrShutdownThread)();
end;

// Dynamic version of LdrUnloadDll
function  LdrUnloadDll(
    hModule : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_LdrUnloadDll, ntdll, 'LdrUnloadDll');
  Result := TFNLdrUnloadDll(_LdrUnloadDll)(
    hModule
  );
end;

// Dynamic version of NtAcceptConnectPort
function  NtAcceptConnectPort(
    PortHandle : PHANDLE;
    PortIdentifier : ULONG;
    Message : PPORT_MESSAGE;
    Accept : BOOLEAN;
    WriteSection : PPORT_SECTION_WRITE;
    ReadSection : PPORT_SECTION_READ
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAcceptConnectPort, ntdll, 'NtAcceptConnectPort');
  Result := TFNNtAcceptConnectPort(_NtAcceptConnectPort)(
    PortHandle, PortIdentifier, Message, Accept, WriteSection, ReadSection
  );
end;

// Dynamic version of NtAcceptConnectPort
function  ZwAcceptConnectPort(
    PortHandle : PHANDLE;
    PortIdentifier : ULONG;
    Message : PPORT_MESSAGE;
    Accept : BOOLEAN;
    WriteSection : PPORT_SECTION_WRITE;
    ReadSection : PPORT_SECTION_READ
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAcceptConnectPort, ntdll, 'NtAcceptConnectPort');
  Result := TFNNtAcceptConnectPort(_NtAcceptConnectPort)(
    PortHandle, PortIdentifier, Message, Accept, WriteSection, ReadSection
  );
end;

// Dynamic version of NtAccessCheck
function  NtAccessCheck(
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    TokenHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    GenericMapping : PGENERIC_MAPPING;
    PrivilegeSet : PPRIVILEGE_SET;
    PrivilegeSetLength : PULONG;
    GrantedAccess : PACCESS_MASK;
    AccessStatus : PBOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAccessCheck, ntdll, 'NtAccessCheck');
  Result := TFNNtAccessCheck(_NtAccessCheck)(
    SecurityDescriptor, TokenHandle, DesiredAccess, GenericMapping, PrivilegeSet, PrivilegeSetLength, GrantedAccess, AccessStatus
  );
end;

// Dynamic version of NtAccessCheck
function  ZwAccessCheck(
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    TokenHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    GenericMapping : PGENERIC_MAPPING;
    PrivilegeSet : PPRIVILEGE_SET;
    PrivilegeSetLength : PULONG;
    GrantedAccess : PACCESS_MASK;
    AccessStatus : PBOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAccessCheck, ntdll, 'NtAccessCheck');
  Result := TFNNtAccessCheck(_NtAccessCheck)(
    SecurityDescriptor, TokenHandle, DesiredAccess, GenericMapping, PrivilegeSet, PrivilegeSetLength, GrantedAccess, AccessStatus
  );
end;

// Dynamic version of NtAccessCheckAndAuditAlarm
function  NtAccessCheckAndAuditAlarm(
    SubsystemName : PUNICODE_STRING;
    HandleId : PVOID;
    ObjectTypeName : PUNICODE_STRING;
    ObjectName : PUNICODE_STRING;
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    DesiredAccess : ACCESS_MASK;
    GenericMapping : PGENERIC_MAPPING;
    ObjectCreation : BOOLEAN;
    GrantedAccess : PACCESS_MASK;
    AccessStatus : PBOOLEAN;
    GenerateOnClose : PBOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAccessCheckAndAuditAlarm, ntdll, 'NtAccessCheckAndAuditAlarm');
  Result := TFNNtAccessCheckAndAuditAlarm(_NtAccessCheckAndAuditAlarm)(
    SubsystemName, HandleId, ObjectTypeName, ObjectName, SecurityDescriptor, DesiredAccess, GenericMapping, ObjectCreation, GrantedAccess, AccessStatus, GenerateOnClose
  );
end;

// Dynamic version of NtAccessCheckAndAuditAlarm
function  ZwAccessCheckAndAuditAlarm(
    SubsystemName : PUNICODE_STRING;
    HandleId : PVOID;
    ObjectTypeName : PUNICODE_STRING;
    ObjectName : PUNICODE_STRING;
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    DesiredAccess : ACCESS_MASK;
    GenericMapping : PGENERIC_MAPPING;
    ObjectCreation : BOOLEAN;
    GrantedAccess : PACCESS_MASK;
    AccessStatus : PBOOLEAN;
    GenerateOnClose : PBOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAccessCheckAndAuditAlarm, ntdll, 'NtAccessCheckAndAuditAlarm');
  Result := TFNNtAccessCheckAndAuditAlarm(_NtAccessCheckAndAuditAlarm)(
    SubsystemName, HandleId, ObjectTypeName, ObjectName, SecurityDescriptor, DesiredAccess, GenericMapping, ObjectCreation, GrantedAccess, AccessStatus, GenerateOnClose
  );
end;

// Dynamic version of NtAccessCheckByType
function  NtAccessCheckByType(
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    PrincipalSelfSid : PSID;
    TokenHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectTypeList : POBJECT_TYPE_LIST;
    ObjectTypeListLength : ULONG;
    GenericMapping : PGENERIC_MAPPING;
    PrivilegeSet : PPRIVILEGE_SET;
    PrivilegeSetLength : PULONG;
    GrantedAccess : PACCESS_MASK;
    AccessStatus : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAccessCheckByType, ntdll, 'NtAccessCheckByType');
  Result := TFNNtAccessCheckByType(_NtAccessCheckByType)(
    SecurityDescriptor, PrincipalSelfSid, TokenHandle, DesiredAccess, ObjectTypeList, ObjectTypeListLength, GenericMapping, PrivilegeSet, PrivilegeSetLength, GrantedAccess, AccessStatus
  );
end;

// Dynamic version of NtAccessCheckByType
function  ZwAccessCheckByType(
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    PrincipalSelfSid : PSID;
    TokenHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectTypeList : POBJECT_TYPE_LIST;
    ObjectTypeListLength : ULONG;
    GenericMapping : PGENERIC_MAPPING;
    PrivilegeSet : PPRIVILEGE_SET;
    PrivilegeSetLength : PULONG;
    GrantedAccess : PACCESS_MASK;
    AccessStatus : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAccessCheckByType, ntdll, 'NtAccessCheckByType');
  Result := TFNNtAccessCheckByType(_NtAccessCheckByType)(
    SecurityDescriptor, PrincipalSelfSid, TokenHandle, DesiredAccess, ObjectTypeList, ObjectTypeListLength, GenericMapping, PrivilegeSet, PrivilegeSetLength, GrantedAccess, AccessStatus
  );
end;

// Dynamic version of NtAccessCheckByTypeAndAuditAlarm
function  NtAccessCheckByTypeAndAuditAlarm(
    SubsystemName : PUNICODE_STRING;
    HandleId : PVOID;
    ObjectTypeName : PUNICODE_STRING;
    ObjectName : PUNICODE_STRING;
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    PrincipalSelfSid : PSID;
    DesiredAccess : ACCESS_MASK;
    AuditType : AUDIT_EVENT_TYPE;
    Flags : ULONG;
    ObjectTypeList : POBJECT_TYPE_LIST;
    ObjectTypeListLength : ULONG;
    GenericMapping : PGENERIC_MAPPING;
    ObjectCreation : BOOLEAN;
    GrantedAccess : PACCESS_MASK;
    AccessStatus : PULONG;
    GenerateOnClose : PBOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAccessCheckByTypeAndAuditAlarm, ntdll, 'NtAccessCheckByTypeAndAuditAlarm');
  Result := TFNNtAccessCheckByTypeAndAuditAlarm(_NtAccessCheckByTypeAndAuditAlarm)(
    SubsystemName, HandleId, ObjectTypeName, ObjectName, SecurityDescriptor, PrincipalSelfSid, DesiredAccess, AuditType, Flags, ObjectTypeList, ObjectTypeListLength, GenericMapping, ObjectCreation, GrantedAccess, AccessStatus,
    GenerateOnClose
  );
end;

// Dynamic version of NtAccessCheckByTypeAndAuditAlarm
function  ZwAccessCheckByTypeAndAuditAlarm(
    SubsystemName : PUNICODE_STRING;
    HandleId : PVOID;
    ObjectTypeName : PUNICODE_STRING;
    ObjectName : PUNICODE_STRING;
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    PrincipalSelfSid : PSID;
    DesiredAccess : ACCESS_MASK;
    AuditType : AUDIT_EVENT_TYPE;
    Flags : ULONG;
    ObjectTypeList : POBJECT_TYPE_LIST;
    ObjectTypeListLength : ULONG;
    GenericMapping : PGENERIC_MAPPING;
    ObjectCreation : BOOLEAN;
    GrantedAccess : PACCESS_MASK;
    AccessStatus : PULONG;
    GenerateOnClose : PBOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAccessCheckByTypeAndAuditAlarm, ntdll, 'NtAccessCheckByTypeAndAuditAlarm');
  Result := TFNNtAccessCheckByTypeAndAuditAlarm(_NtAccessCheckByTypeAndAuditAlarm)(
    SubsystemName, HandleId, ObjectTypeName, ObjectName, SecurityDescriptor, PrincipalSelfSid, DesiredAccess, AuditType, Flags, ObjectTypeList, ObjectTypeListLength, GenericMapping, ObjectCreation, GrantedAccess, AccessStatus,
    GenerateOnClose
  );
end;

// Dynamic version of NtAccessCheckByTypeResultList
function  NtAccessCheckByTypeResultList(
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    PrincipalSelfSid : PSID;
    TokenHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectTypeList : POBJECT_TYPE_LIST;
    ObjectTypeListLength : ULONG;
    GenericMapping : PGENERIC_MAPPING;
    PrivilegeSet : PPRIVILEGE_SET;
    PrivilegeSetLength : PULONG;
    GrantedAccessList : PACCESS_MASK;
    AccessStatusList : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAccessCheckByTypeResultList, ntdll, 'NtAccessCheckByTypeResultList');
  Result := TFNNtAccessCheckByTypeResultList(_NtAccessCheckByTypeResultList)(
    SecurityDescriptor, PrincipalSelfSid, TokenHandle, DesiredAccess, ObjectTypeList, ObjectTypeListLength, GenericMapping, PrivilegeSet, PrivilegeSetLength, GrantedAccessList, AccessStatusList
  );
end;

// Dynamic version of NtAccessCheckByTypeResultList
function  ZwAccessCheckByTypeResultList(
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    PrincipalSelfSid : PSID;
    TokenHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectTypeList : POBJECT_TYPE_LIST;
    ObjectTypeListLength : ULONG;
    GenericMapping : PGENERIC_MAPPING;
    PrivilegeSet : PPRIVILEGE_SET;
    PrivilegeSetLength : PULONG;
    GrantedAccessList : PACCESS_MASK;
    AccessStatusList : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAccessCheckByTypeResultList, ntdll, 'NtAccessCheckByTypeResultList');
  Result := TFNNtAccessCheckByTypeResultList(_NtAccessCheckByTypeResultList)(
    SecurityDescriptor, PrincipalSelfSid, TokenHandle, DesiredAccess, ObjectTypeList, ObjectTypeListLength, GenericMapping, PrivilegeSet, PrivilegeSetLength, GrantedAccessList, AccessStatusList
  );
end;

// Dynamic version of NtAccessCheckByTypeResultListAndAuditAlarm
function  NtAccessCheckByTypeResultListAndAuditAlarm(
    SubsystemName : PUNICODE_STRING;
    HandleId : PVOID;
    ObjectTypeName : PUNICODE_STRING;
    ObjectName : PUNICODE_STRING;
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    PrincipalSelfSid : PSID;
    DesiredAccess : ACCESS_MASK;
    AuditType : AUDIT_EVENT_TYPE;
    Flags : ULONG;
    ObjectTypeList : POBJECT_TYPE_LIST;
    ObjectTypeListLength : ULONG;
    GenericMapping : PGENERIC_MAPPING;
    ObjectCreation : BOOLEAN;
    GrantedAccessList : PACCESS_MASK;
    AccessStatusList : PULONG;
    GenerateOnClose : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAccessCheckByTypeResultListAndAuditAlarm, ntdll, 'NtAccessCheckByTypeResultListAndAuditAlarm');
  Result := TFNNtAccessCheckByTypeResultListAndAuditAlarm(_NtAccessCheckByTypeResultListAndAuditAlarm)(
    SubsystemName, HandleId, ObjectTypeName, ObjectName, SecurityDescriptor, PrincipalSelfSid, DesiredAccess, AuditType, Flags, ObjectTypeList, ObjectTypeListLength, GenericMapping, ObjectCreation, GrantedAccessList, AccessStatusList,
    GenerateOnClose
  );
end;

// Dynamic version of NtAccessCheckByTypeResultListAndAuditAlarm
function  ZwAccessCheckByTypeResultListAndAuditAlarm(
    SubsystemName : PUNICODE_STRING;
    HandleId : PVOID;
    ObjectTypeName : PUNICODE_STRING;
    ObjectName : PUNICODE_STRING;
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    PrincipalSelfSid : PSID;
    DesiredAccess : ACCESS_MASK;
    AuditType : AUDIT_EVENT_TYPE;
    Flags : ULONG;
    ObjectTypeList : POBJECT_TYPE_LIST;
    ObjectTypeListLength : ULONG;
    GenericMapping : PGENERIC_MAPPING;
    ObjectCreation : BOOLEAN;
    GrantedAccessList : PACCESS_MASK;
    AccessStatusList : PULONG;
    GenerateOnClose : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAccessCheckByTypeResultListAndAuditAlarm, ntdll, 'NtAccessCheckByTypeResultListAndAuditAlarm');
  Result := TFNNtAccessCheckByTypeResultListAndAuditAlarm(_NtAccessCheckByTypeResultListAndAuditAlarm)(
    SubsystemName, HandleId, ObjectTypeName, ObjectName, SecurityDescriptor, PrincipalSelfSid, DesiredAccess, AuditType, Flags, ObjectTypeList, ObjectTypeListLength, GenericMapping, ObjectCreation, GrantedAccessList, AccessStatusList,
    GenerateOnClose
  );
end;

// Dynamic version of NtAccessCheckByTypeResultListAndAuditAlarmByHandle
function  NtAccessCheckByTypeResultListAndAuditAlarmByHandle(
    SubsystemName : PUNICODE_STRING;
    HandleId : PVOID;
    TokenHandle : HANDLE;
    ObjectTypeName : PUNICODE_STRING;
    ObjectName : PUNICODE_STRING;
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    PrincipalSelfSid : PSID;
    DesiredAccess : ACCESS_MASK;
    AuditType : AUDIT_EVENT_TYPE;
    Flags : ULONG;
    ObjectTypeList : POBJECT_TYPE_LIST;
    ObjectTypeListLength : ULONG;
    GenericMapping : PGENERIC_MAPPING;
    ObjectCreation : BOOLEAN;
    GrantedAccessList : PACCESS_MASK;
    AccessStatusList : PULONG;
    GenerateOnClose : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAccessCheckByTypeResultListAndAuditAlarmByHandle, ntdll, 'NtAccessCheckByTypeResultListAndAuditAlarmByHandle');
  Result := TFNNtAccessCheckByTypeResultListAndAuditAlarmByHandle(_NtAccessCheckByTypeResultListAndAuditAlarmByHandle)(
    SubsystemName, HandleId, TokenHandle, ObjectTypeName, ObjectName, SecurityDescriptor, PrincipalSelfSid, DesiredAccess, AuditType, Flags, ObjectTypeList, ObjectTypeListLength, GenericMapping, ObjectCreation, GrantedAccessList,
    AccessStatusList, GenerateOnClose
  );
end;

// Dynamic version of NtAccessCheckByTypeResultListAndAuditAlarmByHandle
function  ZwAccessCheckByTypeResultListAndAuditAlarmByHandle(
    SubsystemName : PUNICODE_STRING;
    HandleId : PVOID;
    TokenHandle : HANDLE;
    ObjectTypeName : PUNICODE_STRING;
    ObjectName : PUNICODE_STRING;
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    PrincipalSelfSid : PSID;
    DesiredAccess : ACCESS_MASK;
    AuditType : AUDIT_EVENT_TYPE;
    Flags : ULONG;
    ObjectTypeList : POBJECT_TYPE_LIST;
    ObjectTypeListLength : ULONG;
    GenericMapping : PGENERIC_MAPPING;
    ObjectCreation : BOOLEAN;
    GrantedAccessList : PACCESS_MASK;
    AccessStatusList : PULONG;
    GenerateOnClose : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAccessCheckByTypeResultListAndAuditAlarmByHandle, ntdll, 'NtAccessCheckByTypeResultListAndAuditAlarmByHandle');
  Result := TFNNtAccessCheckByTypeResultListAndAuditAlarmByHandle(_NtAccessCheckByTypeResultListAndAuditAlarmByHandle)(
    SubsystemName, HandleId, TokenHandle, ObjectTypeName, ObjectName, SecurityDescriptor, PrincipalSelfSid, DesiredAccess, AuditType, Flags, ObjectTypeList, ObjectTypeListLength, GenericMapping, ObjectCreation, GrantedAccessList,
    AccessStatusList, GenerateOnClose
  );
end;

// Dynamic version of NtAddAtom
function  NtAddAtom(
    Str : PWSTR;
    StringLength : ULONG;
    Atom : PUSHORT
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAddAtom, ntdll, 'NtAddAtom');
  Result := TFNNtAddAtom(_NtAddAtom)(
    Str, StringLength, Atom
  );
end;

// Dynamic version of NtAddAtom
function  ZwAddAtom(
    Str : PWSTR;
    StringLength : ULONG;
    Atom : PUSHORT
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAddAtom, ntdll, 'NtAddAtom');
  Result := TFNNtAddAtom(_NtAddAtom)(
    Str, StringLength, Atom
  );
end;

// Dynamic version of NtAdjustGroupsToken
function  NtAdjustGroupsToken(
    TokenHandle : HANDLE;
    ResetToDefault : BOOLEAN;
    NewState : PTOKEN_GROUPS;
    BufferLength : ULONG;
    PreviousState : PTOKEN_GROUPS;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAdjustGroupsToken, ntdll, 'NtAdjustGroupsToken');
  Result := TFNNtAdjustGroupsToken(_NtAdjustGroupsToken)(
    TokenHandle, ResetToDefault, NewState, BufferLength, PreviousState, ReturnLength
  );
end;

// Dynamic version of NtAdjustGroupsToken
function  ZwAdjustGroupsToken(
    TokenHandle : HANDLE;
    ResetToDefault : BOOLEAN;
    NewState : PTOKEN_GROUPS;
    BufferLength : ULONG;
    PreviousState : PTOKEN_GROUPS;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAdjustGroupsToken, ntdll, 'NtAdjustGroupsToken');
  Result := TFNNtAdjustGroupsToken(_NtAdjustGroupsToken)(
    TokenHandle, ResetToDefault, NewState, BufferLength, PreviousState, ReturnLength
  );
end;

// Dynamic version of NtAdjustPrivilegesToken
function  NtAdjustPrivilegesToken(
    TokenHandle : HANDLE;
    DisableAllPrivileges : BOOLEAN;
    NewState : PTOKEN_PRIVILEGES;
    BufferLength : ULONG;
    PreviousState : PTOKEN_PRIVILEGES;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAdjustPrivilegesToken, ntdll, 'NtAdjustPrivilegesToken');
  Result := TFNNtAdjustPrivilegesToken(_NtAdjustPrivilegesToken)(
    TokenHandle, DisableAllPrivileges, NewState, BufferLength, PreviousState, ReturnLength
  );
end;

// Dynamic version of NtAdjustPrivilegesToken
function  ZwAdjustPrivilegesToken(
    TokenHandle : HANDLE;
    DisableAllPrivileges : BOOLEAN;
    NewState : PTOKEN_PRIVILEGES;
    BufferLength : ULONG;
    PreviousState : PTOKEN_PRIVILEGES;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAdjustPrivilegesToken, ntdll, 'NtAdjustPrivilegesToken');
  Result := TFNNtAdjustPrivilegesToken(_NtAdjustPrivilegesToken)(
    TokenHandle, DisableAllPrivileges, NewState, BufferLength, PreviousState, ReturnLength
  );
end;

// Dynamic version of NtAlertResumeThread
function  NtAlertResumeThread(
    ThreadHandle : HANDLE;
    PreviousSuspendCount : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAlertResumeThread, ntdll, 'NtAlertResumeThread');
  Result := TFNNtAlertResumeThread(_NtAlertResumeThread)(
    ThreadHandle, PreviousSuspendCount
  );
end;

// Dynamic version of NtAlertResumeThread
function  ZwAlertResumeThread(
    ThreadHandle : HANDLE;
    PreviousSuspendCount : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAlertResumeThread, ntdll, 'NtAlertResumeThread');
  Result := TFNNtAlertResumeThread(_NtAlertResumeThread)(
    ThreadHandle, PreviousSuspendCount
  );
end;

// Dynamic version of NtAlertThread
function  NtAlertThread(
    ThreadHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAlertThread, ntdll, 'NtAlertThread');
  Result := TFNNtAlertThread(_NtAlertThread)(
    ThreadHandle
  );
end;

// Dynamic version of NtAlertThread
function  ZwAlertThread(
    ThreadHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAlertThread, ntdll, 'NtAlertThread');
  Result := TFNNtAlertThread(_NtAlertThread)(
    ThreadHandle
  );
end;

// Dynamic version of NtAllocateLocallyUniqueId
function  NtAllocateLocallyUniqueId(
    Luid : PLUID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAllocateLocallyUniqueId, ntdll, 'NtAllocateLocallyUniqueId');
  Result := TFNNtAllocateLocallyUniqueId(_NtAllocateLocallyUniqueId)(
    Luid
  );
end;

// Dynamic version of NtAllocateLocallyUniqueId
function  ZwAllocateLocallyUniqueId(
    Luid : PLUID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAllocateLocallyUniqueId, ntdll, 'NtAllocateLocallyUniqueId');
  Result := TFNNtAllocateLocallyUniqueId(_NtAllocateLocallyUniqueId)(
    Luid
  );
end;

// Dynamic version of NtAllocateUserPhysicalPages
function  NtAllocateUserPhysicalPages(
    ProcessHandle : HANDLE;
    NumberOfPages : PULONG;
    PageFrameNumbers : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAllocateUserPhysicalPages, ntdll, 'NtAllocateUserPhysicalPages');
  Result := TFNNtAllocateUserPhysicalPages(_NtAllocateUserPhysicalPages)(
    ProcessHandle, NumberOfPages, PageFrameNumbers
  );
end;

// Dynamic version of NtAllocateUserPhysicalPages
function  ZwAllocateUserPhysicalPages(
    ProcessHandle : HANDLE;
    NumberOfPages : PULONG;
    PageFrameNumbers : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAllocateUserPhysicalPages, ntdll, 'NtAllocateUserPhysicalPages');
  Result := TFNNtAllocateUserPhysicalPages(_NtAllocateUserPhysicalPages)(
    ProcessHandle, NumberOfPages, PageFrameNumbers
  );
end;

// Dynamic version of NtAllocateUuids
function  NtAllocateUuids(
    UuidLastTimeAllocated : PLARGE_INTEGER;
    UuidDeltaTime : PULONG;
    UuidSequenceNumber : PULONG;
    UuidSeed : PUCHAR
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAllocateUuids, ntdll, 'NtAllocateUuids');
  Result := TFNNtAllocateUuids(_NtAllocateUuids)(
    UuidLastTimeAllocated, UuidDeltaTime, UuidSequenceNumber, UuidSeed
  );
end;

// Dynamic version of NtAllocateUuids
function  ZwAllocateUuids(
    UuidLastTimeAllocated : PLARGE_INTEGER;
    UuidDeltaTime : PULONG;
    UuidSequenceNumber : PULONG;
    UuidSeed : PUCHAR
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAllocateUuids, ntdll, 'NtAllocateUuids');
  Result := TFNNtAllocateUuids(_NtAllocateUuids)(
    UuidLastTimeAllocated, UuidDeltaTime, UuidSequenceNumber, UuidSeed
  );
end;

// Dynamic version of NtAllocateVirtualMemory
function  NtAllocateVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PPVOID;
    ZeroBits : ULONG;
    AllocationSize : PULONG;
    AllocationType : ULONG;
    Protect : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAllocateVirtualMemory, ntdll, 'NtAllocateVirtualMemory');
  Result := TFNNtAllocateVirtualMemory(_NtAllocateVirtualMemory)(
    ProcessHandle, BaseAddress, ZeroBits, AllocationSize, AllocationType, Protect
  );
end;

// Dynamic version of NtAllocateVirtualMemory
function  ZwAllocateVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PPVOID;
    ZeroBits : ULONG;
    AllocationSize : PULONG;
    AllocationType : ULONG;
    Protect : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAllocateVirtualMemory, ntdll, 'NtAllocateVirtualMemory');
  Result := TFNNtAllocateVirtualMemory(_NtAllocateVirtualMemory)(
    ProcessHandle, BaseAddress, ZeroBits, AllocationSize, AllocationType, Protect
  );
end;

// Dynamic version of NtAreMappedFilesTheSame
function  NtAreMappedFilesTheSame(
    Address1 : PVOID;
    Address2 : PVOID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAreMappedFilesTheSame, ntdll, 'NtAreMappedFilesTheSame');
  Result := TFNNtAreMappedFilesTheSame(_NtAreMappedFilesTheSame)(
    Address1, Address2
  );
end;

// Dynamic version of NtAreMappedFilesTheSame
function  ZwAreMappedFilesTheSame(
    Address1 : PVOID;
    Address2 : PVOID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAreMappedFilesTheSame, ntdll, 'NtAreMappedFilesTheSame');
  Result := TFNNtAreMappedFilesTheSame(_NtAreMappedFilesTheSame)(
    Address1, Address2
  );
end;

// Dynamic version of NtAssignProcessToJobObject
function  NtAssignProcessToJobObject(
    JobHandle : HANDLE;
    ProcessHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAssignProcessToJobObject, ntdll, 'NtAssignProcessToJobObject');
  Result := TFNNtAssignProcessToJobObject(_NtAssignProcessToJobObject)(
    JobHandle, ProcessHandle
  );
end;

// Dynamic version of NtAssignProcessToJobObject
function  ZwAssignProcessToJobObject(
    JobHandle : HANDLE;
    ProcessHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtAssignProcessToJobObject, ntdll, 'NtAssignProcessToJobObject');
  Result := TFNNtAssignProcessToJobObject(_NtAssignProcessToJobObject)(
    JobHandle, ProcessHandle
  );
end;

// Dynamic version of NtCallbackReturn
function  NtCallbackReturn(
    Result_ : PVOID;
    ResultLength : ULONG;
    Status : NTSTATUS
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCallbackReturn, ntdll, 'NtCallbackReturn');
  Result := TFNNtCallbackReturn(_NtCallbackReturn)(
    Result_, ResultLength, Status
  );
end;

// Dynamic version of NtCallbackReturn
function  ZwCallbackReturn(
    Result_ : PVOID;
    ResultLength : ULONG;
    Status : NTSTATUS
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCallbackReturn, ntdll, 'NtCallbackReturn');
  Result := TFNNtCallbackReturn(_NtCallbackReturn)(
    Result_, ResultLength, Status
  );
end;

// Dynamic version of NtCancelDeviceWakeupRequest
function  NtCancelDeviceWakeupRequest(
    DeviceHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCancelDeviceWakeupRequest, ntdll, 'NtCancelDeviceWakeupRequest');
  Result := TFNNtCancelDeviceWakeupRequest(_NtCancelDeviceWakeupRequest)(
    DeviceHandle
  );
end;

// Dynamic version of NtCancelDeviceWakeupRequest
function  ZwCancelDeviceWakeupRequest(
    DeviceHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCancelDeviceWakeupRequest, ntdll, 'NtCancelDeviceWakeupRequest');
  Result := TFNNtCancelDeviceWakeupRequest(_NtCancelDeviceWakeupRequest)(
    DeviceHandle
  );
end;

// Dynamic version of NtCancelIoFile
function  NtCancelIoFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCancelIoFile, ntdll, 'NtCancelIoFile');
  Result := TFNNtCancelIoFile(_NtCancelIoFile)(
    FileHandle, IoStatusBlock
  );
end;

// Dynamic version of NtCancelIoFile
function  ZwCancelIoFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCancelIoFile, ntdll, 'NtCancelIoFile');
  Result := TFNNtCancelIoFile(_NtCancelIoFile)(
    FileHandle, IoStatusBlock
  );
end;

// Dynamic version of NtCancelTimer
function  NtCancelTimer(
    TimerHandle : HANDLE;
    PreviousState : PBOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCancelTimer, ntdll, 'NtCancelTimer');
  Result := TFNNtCancelTimer(_NtCancelTimer)(
    TimerHandle, PreviousState
  );
end;

// Dynamic version of NtCancelTimer
function  ZwCancelTimer(
    TimerHandle : HANDLE;
    PreviousState : PBOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCancelTimer, ntdll, 'NtCancelTimer');
  Result := TFNNtCancelTimer(_NtCancelTimer)(
    TimerHandle, PreviousState
  );
end;

// Dynamic version of NtClearEvent
function  NtClearEvent(
    EventHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtClearEvent, ntdll, 'NtClearEvent');
  Result := TFNNtClearEvent(_NtClearEvent)(
    EventHandle
  );
end;

// Dynamic version of NtClearEvent
function  ZwClearEvent(
    EventHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtClearEvent, ntdll, 'NtClearEvent');
  Result := TFNNtClearEvent(_NtClearEvent)(
    EventHandle
  );
end;

{.$IFNDEF JWA_INCLUDEMODE}
// Dynamic version of NtClose
function  NtClose(
    Handle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtClose, ntdll, 'NtClose');
  Result := TFNNtClose(_NtClose)(
    Handle
  );
end;
{.$ENDIF JWA_INCLUDEMODE}

// Dynamic version of NtClose
function  ZwClose(
    Handle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtClose, ntdll, 'NtClose');
  Result := TFNNtClose(_NtClose)(
    Handle
  );
end;

// Dynamic version of NtCloseObjectAuditAlarm
function  NtCloseObjectAuditAlarm(
    SubsystemName : PUNICODE_STRING;
    HandleId : PVOID;
    GenerateOnClose : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCloseObjectAuditAlarm, ntdll, 'NtCloseObjectAuditAlarm');
  Result := TFNNtCloseObjectAuditAlarm(_NtCloseObjectAuditAlarm)(
    SubsystemName, HandleId, GenerateOnClose
  );
end;

// Dynamic version of NtCloseObjectAuditAlarm
function  ZwCloseObjectAuditAlarm(
    SubsystemName : PUNICODE_STRING;
    HandleId : PVOID;
    GenerateOnClose : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCloseObjectAuditAlarm, ntdll, 'NtCloseObjectAuditAlarm');
  Result := TFNNtCloseObjectAuditAlarm(_NtCloseObjectAuditAlarm)(
    SubsystemName, HandleId, GenerateOnClose
  );
end;

// Dynamic version of NtCompleteConnectPort
function  NtCompleteConnectPort(
    PortHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCompleteConnectPort, ntdll, 'NtCompleteConnectPort');
  Result := TFNNtCompleteConnectPort(_NtCompleteConnectPort)(
    PortHandle
  );
end;

// Dynamic version of NtCompleteConnectPort
function  ZwCompleteConnectPort(
    PortHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCompleteConnectPort, ntdll, 'NtCompleteConnectPort');
  Result := TFNNtCompleteConnectPort(_NtCompleteConnectPort)(
    PortHandle
  );
end;

// Dynamic version of NtConnectPort
function  NtConnectPort(
    PortHandle : PHANDLE;
    PortName : PUNICODE_STRING;
    SecurityQos : PSECURITY_QUALITY_OF_SERVICE;
    WriteSection : PPORT_SECTION_WRITE;
    ReadSection : PPORT_SECTION_READ;
    MaxMessageSize : PULONG;
    ConnectData : PVOID;
    ConnectDataLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtConnectPort, ntdll, 'NtConnectPort');
  Result := TFNNtConnectPort(_NtConnectPort)(
    PortHandle, PortName, SecurityQos, WriteSection, ReadSection, MaxMessageSize, ConnectData, ConnectDataLength
  );
end;

// Dynamic version of NtConnectPort
function  ZwConnectPort(
    PortHandle : PHANDLE;
    PortName : PUNICODE_STRING;
    SecurityQos : PSECURITY_QUALITY_OF_SERVICE;
    WriteSection : PPORT_SECTION_WRITE;
    ReadSection : PPORT_SECTION_READ;
    MaxMessageSize : PULONG;
    ConnectData : PVOID;
    ConnectDataLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtConnectPort, ntdll, 'NtConnectPort');
  Result := TFNNtConnectPort(_NtConnectPort)(
    PortHandle, PortName, SecurityQos, WriteSection, ReadSection, MaxMessageSize, ConnectData, ConnectDataLength
  );
end;

// Dynamic version of NtContinue
function  NtContinue(
    Context : PCONTEXT;
    TestAlert : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtContinue, ntdll, 'NtContinue');
  Result := TFNNtContinue(_NtContinue)(
    Context, TestAlert
  );
end;

// Dynamic version of NtContinue
function  ZwContinue(
    Context : PCONTEXT;
    TestAlert : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtContinue, ntdll, 'NtContinue');
  Result := TFNNtContinue(_NtContinue)(
    Context, TestAlert
  );
end;

// Dynamic version of NtCreateChannel
function  NtCreateChannel(
    ChannelHandle : PHANDLE;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateChannel, ntdll, 'NtCreateChannel');
  Result := TFNNtCreateChannel(_NtCreateChannel)(
    ChannelHandle, ObjectAttributes
  );
end;

// Dynamic version of NtCreateChannel
function  ZwCreateChannel(
    ChannelHandle : PHANDLE;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateChannel, ntdll, 'NtCreateChannel');
  Result := TFNNtCreateChannel(_NtCreateChannel)(
    ChannelHandle, ObjectAttributes
  );
end;

// Dynamic version of NtCreateDirectoryObject
function  NtCreateDirectoryObject(
    DirectoryHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateDirectoryObject, ntdll, 'NtCreateDirectoryObject');
  Result := TFNNtCreateDirectoryObject(_NtCreateDirectoryObject)(
    DirectoryHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtCreateDirectoryObject
function  ZwCreateDirectoryObject(
    DirectoryHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateDirectoryObject, ntdll, 'NtCreateDirectoryObject');
  Result := TFNNtCreateDirectoryObject(_NtCreateDirectoryObject)(
    DirectoryHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtCreateEvent
function  NtCreateEvent(
    EventHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    EventType : EVENT_TYPE;
    InitialState : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateEvent, ntdll, 'NtCreateEvent');
  Result := TFNNtCreateEvent(_NtCreateEvent)(
    EventHandle, DesiredAccess, ObjectAttributes, EventType, InitialState
  );
end;

// Dynamic version of NtCreateEvent
function  ZwCreateEvent(
    EventHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    EventType : EVENT_TYPE;
    InitialState : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateEvent, ntdll, 'NtCreateEvent');
  Result := TFNNtCreateEvent(_NtCreateEvent)(
    EventHandle, DesiredAccess, ObjectAttributes, EventType, InitialState
  );
end;

// Dynamic version of NtCreateEventPair
function  NtCreateEventPair(
    EventPairHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateEventPair, ntdll, 'NtCreateEventPair');
  Result := TFNNtCreateEventPair(_NtCreateEventPair)(
    EventPairHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtCreateEventPair
function  ZwCreateEventPair(
    EventPairHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateEventPair, ntdll, 'NtCreateEventPair');
  Result := TFNNtCreateEventPair(_NtCreateEventPair)(
    EventPairHandle, DesiredAccess, ObjectAttributes
  );
end;

{.$IFNDEF JWA_INCLUDEMODE}
// Dynamic version of NtCreateFile
function  NtCreateFile(
    FileHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    IoStatusBlock : PIO_STATUS_BLOCK;
    AllocationSize : PLARGE_INTEGER;
    FileAttributes : ULONG;
    ShareAccess : ULONG;
    CreateDisposition : ULONG;
    CreateOptions : ULONG;
    EaBuffer : PVOID;
    EaLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateFile, ntdll, 'NtCreateFile');
  Result := TFNNtCreateFile(_NtCreateFile)(
    FileHandle, DesiredAccess, ObjectAttributes, IoStatusBlock, AllocationSize, FileAttributes, ShareAccess, CreateDisposition, CreateOptions, EaBuffer, EaLength
  );
end;
{.$ENDIF JWA_INCLUDEMODE}

// Dynamic version of NtCreateFile
function  ZwCreateFile(
    FileHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    IoStatusBlock : PIO_STATUS_BLOCK;
    AllocationSize : PLARGE_INTEGER;
    FileAttributes : ULONG;
    ShareAccess : ULONG;
    CreateDisposition : ULONG;
    CreateOptions : ULONG;
    EaBuffer : PVOID;
    EaLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateFile, ntdll, 'NtCreateFile');
  Result := TFNNtCreateFile(_NtCreateFile)(
    FileHandle, DesiredAccess, ObjectAttributes, IoStatusBlock, AllocationSize, FileAttributes, ShareAccess, CreateDisposition, CreateOptions, EaBuffer, EaLength
  );
end;

// Dynamic version of NtCreateIoCompletion
function  NtCreateIoCompletion(
    IoCompletionHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    NumberOfConcurrentThreads : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateIoCompletion, ntdll, 'NtCreateIoCompletion');
  Result := TFNNtCreateIoCompletion(_NtCreateIoCompletion)(
    IoCompletionHandle, DesiredAccess, ObjectAttributes, NumberOfConcurrentThreads
  );
end;

// Dynamic version of NtCreateIoCompletion
function  ZwCreateIoCompletion(
    IoCompletionHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    NumberOfConcurrentThreads : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateIoCompletion, ntdll, 'NtCreateIoCompletion');
  Result := TFNNtCreateIoCompletion(_NtCreateIoCompletion)(
    IoCompletionHandle, DesiredAccess, ObjectAttributes, NumberOfConcurrentThreads
  );
end;

// Dynamic version of NtCreateJobObject
function  NtCreateJobObject(
    JobHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateJobObject, ntdll, 'NtCreateJobObject');
  Result := TFNNtCreateJobObject(_NtCreateJobObject)(
    JobHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtCreateJobObject
function  ZwCreateJobObject(
    JobHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateJobObject, ntdll, 'NtCreateJobObject');
  Result := TFNNtCreateJobObject(_NtCreateJobObject)(
    JobHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtCreateKey
function  NtCreateKey(
    KeyHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    TitleIndex : ULONG;
    Class_ : PUNICODE_STRING;
    CreateOptions : ULONG;
    Disposition : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateKey, ntdll, 'NtCreateKey');
  Result := TFNNtCreateKey(_NtCreateKey)(
    KeyHandle, DesiredAccess, ObjectAttributes, TitleIndex, Class_, CreateOptions, Disposition
  );
end;

// Dynamic version of NtCreateKey
function  ZwCreateKey(
    KeyHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    TitleIndex : ULONG;
    Class_ : PUNICODE_STRING;
    CreateOptions : ULONG;
    Disposition : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateKey, ntdll, 'NtCreateKey');
  Result := TFNNtCreateKey(_NtCreateKey)(
    KeyHandle, DesiredAccess, ObjectAttributes, TitleIndex, Class_, CreateOptions, Disposition
  );
end;

// Dynamic version of NtCreateMailslotFile
function  NtCreateMailslotFile(
    FileHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    IoStatusBlock : PIO_STATUS_BLOCK;
    CreateOptions : ULONG;
    Unknown : ULONG;
    MaxMessageSize : ULONG;
    ReadTimeout : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateMailslotFile, ntdll, 'NtCreateMailslotFile');
  Result := TFNNtCreateMailslotFile(_NtCreateMailslotFile)(
    FileHandle, DesiredAccess, ObjectAttributes, IoStatusBlock, CreateOptions, Unknown, MaxMessageSize, ReadTimeout
  );
end;

// Dynamic version of NtCreateMailslotFile
function  ZwCreateMailslotFile(
    FileHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    IoStatusBlock : PIO_STATUS_BLOCK;
    CreateOptions : ULONG;
    Unknown : ULONG;
    MaxMessageSize : ULONG;
    ReadTimeout : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateMailslotFile, ntdll, 'NtCreateMailslotFile');
  Result := TFNNtCreateMailslotFile(_NtCreateMailslotFile)(
    FileHandle, DesiredAccess, ObjectAttributes, IoStatusBlock, CreateOptions, Unknown, MaxMessageSize, ReadTimeout
  );
end;

// Dynamic version of NtCreateMutant
function  NtCreateMutant(
    MutantHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    InitialOwner : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateMutant, ntdll, 'NtCreateMutant');
  Result := TFNNtCreateMutant(_NtCreateMutant)(
    MutantHandle, DesiredAccess, ObjectAttributes, InitialOwner
  );
end;

// Dynamic version of NtCreateMutant
function  ZwCreateMutant(
    MutantHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    InitialOwner : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateMutant, ntdll, 'NtCreateMutant');
  Result := TFNNtCreateMutant(_NtCreateMutant)(
    MutantHandle, DesiredAccess, ObjectAttributes, InitialOwner
  );
end;

// Dynamic version of NtCreateNamedPipeFile
function  NtCreateNamedPipeFile(
    FileHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    IoStatusBlock : PIO_STATUS_BLOCK;
    ShareAccess : ULONG;
    CreateDisposition : ULONG;
    CreateOptions : ULONG;
    TypeMessage : BOOLEAN;
    ReadmodeMessage : BOOLEAN;
    Nonblocking : BOOLEAN;
    MaxInstances : ULONG;
    InBufferSize : ULONG;
    OutBufferSize : ULONG;
    DefaultTimeout : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateNamedPipeFile, ntdll, 'NtCreateNamedPipeFile');
  Result := TFNNtCreateNamedPipeFile(_NtCreateNamedPipeFile)(
    FileHandle, DesiredAccess, ObjectAttributes, IoStatusBlock, ShareAccess, CreateDisposition, CreateOptions, TypeMessage, ReadmodeMessage, Nonblocking, MaxInstances, InBufferSize, OutBufferSize, DefaultTimeout
  );
end;

// Dynamic version of NtCreateNamedPipeFile
function  ZwCreateNamedPipeFile(
    FileHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    IoStatusBlock : PIO_STATUS_BLOCK;
    ShareAccess : ULONG;
    CreateDisposition : ULONG;
    CreateOptions : ULONG;
    TypeMessage : BOOLEAN;
    ReadmodeMessage : BOOLEAN;
    Nonblocking : BOOLEAN;
    MaxInstances : ULONG;
    InBufferSize : ULONG;
    OutBufferSize : ULONG;
    DefaultTimeout : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateNamedPipeFile, ntdll, 'NtCreateNamedPipeFile');
  Result := TFNNtCreateNamedPipeFile(_NtCreateNamedPipeFile)(
    FileHandle, DesiredAccess, ObjectAttributes, IoStatusBlock, ShareAccess, CreateDisposition, CreateOptions, TypeMessage, ReadmodeMessage, Nonblocking, MaxInstances, InBufferSize, OutBufferSize, DefaultTimeout
  );
end;

// Dynamic version of NtCreatePagingFile
function  NtCreatePagingFile(
    FileName : PUNICODE_STRING;
    InitialSize : PULARGE_INTEGER;
    MaximumSize : PULARGE_INTEGER;
    Reserved : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreatePagingFile, ntdll, 'NtCreatePagingFile');
  Result := TFNNtCreatePagingFile(_NtCreatePagingFile)(
    FileName, InitialSize, MaximumSize, Reserved
  );
end;

// Dynamic version of NtCreatePagingFile
function  ZwCreatePagingFile(
    FileName : PUNICODE_STRING;
    InitialSize : PULARGE_INTEGER;
    MaximumSize : PULARGE_INTEGER;
    Reserved : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreatePagingFile, ntdll, 'NtCreatePagingFile');
  Result := TFNNtCreatePagingFile(_NtCreatePagingFile)(
    FileName, InitialSize, MaximumSize, Reserved
  );
end;

// Dynamic version of NtCreatePort
function  NtCreatePort(
    PortHandle : PHANDLE;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    MaxDataSize : ULONG;
    MaxMessageSize : ULONG;
    Reserved : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreatePort, ntdll, 'NtCreatePort');
  Result := TFNNtCreatePort(_NtCreatePort)(
    PortHandle, ObjectAttributes, MaxDataSize, MaxMessageSize, Reserved
  );
end;

// Dynamic version of NtCreatePort
function  ZwCreatePort(
    PortHandle : PHANDLE;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    MaxDataSize : ULONG;
    MaxMessageSize : ULONG;
    Reserved : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreatePort, ntdll, 'NtCreatePort');
  Result := TFNNtCreatePort(_NtCreatePort)(
    PortHandle, ObjectAttributes, MaxDataSize, MaxMessageSize, Reserved
  );
end;

// Dynamic version of NtCreateProcess
function  NtCreateProcess(
    ProcessHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    InheritFromProcessHandle : HANDLE;
    InheritHandles : BOOLEAN;
    SectionHandle : HANDLE;
    DebugPort : HANDLE;
    ExceptionPort : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateProcess, ntdll, 'NtCreateProcess');
  Result := TFNNtCreateProcess(_NtCreateProcess)(
    ProcessHandle, DesiredAccess, ObjectAttributes, InheritFromProcessHandle, InheritHandles, SectionHandle, DebugPort, ExceptionPort
  );
end;

// Dynamic version of NtCreateProcess
function  ZwCreateProcess(
    ProcessHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    InheritFromProcessHandle : HANDLE;
    InheritHandles : BOOLEAN;
    SectionHandle : HANDLE;
    DebugPort : HANDLE;
    ExceptionPort : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateProcess, ntdll, 'NtCreateProcess');
  Result := TFNNtCreateProcess(_NtCreateProcess)(
    ProcessHandle, DesiredAccess, ObjectAttributes, InheritFromProcessHandle, InheritHandles, SectionHandle, DebugPort, ExceptionPort
  );
end;

// Dynamic version of NtCreateProfile
function  NtCreateProfile(
    ProfileHandle : PHANDLE;
    ProcessHandle : HANDLE;
    Base : PVOID;
    Size : ULONG;
    BucketShift : ULONG;
    Buffer : PULONG;
    BufferLength : ULONG;
    Source : KPROFILE_SOURCE;
    ProcessorMask : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateProfile, ntdll, 'NtCreateProfile');
  Result := TFNNtCreateProfile(_NtCreateProfile)(
    ProfileHandle, ProcessHandle, Base, Size, BucketShift, Buffer, BufferLength, Source, ProcessorMask
  );
end;

// Dynamic version of NtCreateProfile
function  ZwCreateProfile(
    ProfileHandle : PHANDLE;
    ProcessHandle : HANDLE;
    Base : PVOID;
    Size : ULONG;
    BucketShift : ULONG;
    Buffer : PULONG;
    BufferLength : ULONG;
    Source : KPROFILE_SOURCE;
    ProcessorMask : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateProfile, ntdll, 'NtCreateProfile');
  Result := TFNNtCreateProfile(_NtCreateProfile)(
    ProfileHandle, ProcessHandle, Base, Size, BucketShift, Buffer, BufferLength, Source, ProcessorMask
  );
end;

// Dynamic version of NtCreateSection
function  NtCreateSection(
    SectionHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    SectionSize : PLARGE_INTEGER;
    Protect : ULONG;
    Attributes : ULONG;
    FileHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateSection, ntdll, 'NtCreateSection');
  Result := TFNNtCreateSection(_NtCreateSection)(
    SectionHandle, DesiredAccess, ObjectAttributes, SectionSize, Protect, Attributes, FileHandle
  );
end;

// Dynamic version of NtCreateSection
function  ZwCreateSection(
    SectionHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    SectionSize : PLARGE_INTEGER;
    Protect : ULONG;
    Attributes : ULONG;
    FileHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateSection, ntdll, 'NtCreateSection');
  Result := TFNNtCreateSection(_NtCreateSection)(
    SectionHandle, DesiredAccess, ObjectAttributes, SectionSize, Protect, Attributes, FileHandle
  );
end;

// Dynamic version of NtCreateSemaphore
function  NtCreateSemaphore(
    SemaphoreHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    InitialCount : LONG;
    MaximumCount : LONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateSemaphore, ntdll, 'NtCreateSemaphore');
  Result := TFNNtCreateSemaphore(_NtCreateSemaphore)(
    SemaphoreHandle, DesiredAccess, ObjectAttributes, InitialCount, MaximumCount
  );
end;

// Dynamic version of NtCreateSemaphore
function  ZwCreateSemaphore(
    SemaphoreHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    InitialCount : LONG;
    MaximumCount : LONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateSemaphore, ntdll, 'NtCreateSemaphore');
  Result := TFNNtCreateSemaphore(_NtCreateSemaphore)(
    SemaphoreHandle, DesiredAccess, ObjectAttributes, InitialCount, MaximumCount
  );
end;

// Dynamic version of NtCreateSymbolicLinkObject
function  NtCreateSymbolicLinkObject(
    SymbolicLinkHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    TargetName : PUNICODE_STRING
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateSymbolicLinkObject, ntdll, 'NtCreateSymbolicLinkObject');
  Result := TFNNtCreateSymbolicLinkObject(_NtCreateSymbolicLinkObject)(
    SymbolicLinkHandle, DesiredAccess, ObjectAttributes, TargetName
  );
end;

// Dynamic version of NtCreateSymbolicLinkObject
function  ZwCreateSymbolicLinkObject(
    SymbolicLinkHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    TargetName : PUNICODE_STRING
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateSymbolicLinkObject, ntdll, 'NtCreateSymbolicLinkObject');
  Result := TFNNtCreateSymbolicLinkObject(_NtCreateSymbolicLinkObject)(
    SymbolicLinkHandle, DesiredAccess, ObjectAttributes, TargetName
  );
end;

// Dynamic version of NtCreateThread
function  NtCreateThread(
    ThreadHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    ProcessHandle : HANDLE;
    ClientId : PCLIENT_ID;
    ThreadContext : PCONTEXT;
    UserStack : PUSER_STACK;
    CreateSuspended : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateThread, ntdll, 'NtCreateThread');
  Result := TFNNtCreateThread(_NtCreateThread)(
    ThreadHandle, DesiredAccess, ObjectAttributes, ProcessHandle, ClientId, ThreadContext, UserStack, CreateSuspended
  );
end;

// Dynamic version of NtCreateThread
function  ZwCreateThread(
    ThreadHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    ProcessHandle : HANDLE;
    ClientId : PCLIENT_ID;
    ThreadContext : PCONTEXT;
    UserStack : PUSER_STACK;
    CreateSuspended : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateThread, ntdll, 'NtCreateThread');
  Result := TFNNtCreateThread(_NtCreateThread)(
    ThreadHandle, DesiredAccess, ObjectAttributes, ProcessHandle, ClientId, ThreadContext, UserStack, CreateSuspended
  );
end;

// Dynamic version of NtCreateTimer
function  NtCreateTimer(
    TimerHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    TimerType : TIMER_TYPE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateTimer, ntdll, 'NtCreateTimer');
  Result := TFNNtCreateTimer(_NtCreateTimer)(
    TimerHandle, DesiredAccess, ObjectAttributes, TimerType
  );
end;

// Dynamic version of NtCreateTimer
function  ZwCreateTimer(
    TimerHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    TimerType : TIMER_TYPE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateTimer, ntdll, 'NtCreateTimer');
  Result := TFNNtCreateTimer(_NtCreateTimer)(
    TimerHandle, DesiredAccess, ObjectAttributes, TimerType
  );
end;

// Dynamic version of NtCreateToken
function  NtCreateToken(
    TokenHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    Type_ : TOKEN_TYPE;
    AuthenticationId : PLUID;
    ExpirationTime : PLARGE_INTEGER;
    User : PTOKEN_USER;
    Groups : PTOKEN_GROUPS;
    Privileges : PTOKEN_PRIVILEGES;
    Owner : PTOKEN_OWNER;
    PrimaryGroup : PTOKEN_PRIMARY_GROUP;
    DefaultDacl : PTOKEN_DEFAULT_DACL;
    Source : PTOKEN_SOURCE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateToken, ntdll, 'NtCreateToken');
  Result := TFNNtCreateToken(_NtCreateToken)(
    TokenHandle, DesiredAccess, ObjectAttributes, Type_, AuthenticationId, ExpirationTime, User, Groups, Privileges, Owner, PrimaryGroup, DefaultDacl, Source
  );
end;

// Dynamic version of NtCreateToken
function  ZwCreateToken(
    TokenHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    Type_ : TOKEN_TYPE;
    AuthenticationId : PLUID;
    ExpirationTime : PLARGE_INTEGER;
    User : PTOKEN_USER;
    Groups : PTOKEN_GROUPS;
    Privileges : PTOKEN_PRIVILEGES;
    Owner : PTOKEN_OWNER;
    PrimaryGroup : PTOKEN_PRIMARY_GROUP;
    DefaultDacl : PTOKEN_DEFAULT_DACL;
    Source : PTOKEN_SOURCE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateToken, ntdll, 'NtCreateToken');
  Result := TFNNtCreateToken(_NtCreateToken)(
    TokenHandle, DesiredAccess, ObjectAttributes, Type_, AuthenticationId, ExpirationTime, User, Groups, Privileges, Owner, PrimaryGroup, DefaultDacl, Source
  );
end;

// Dynamic version of NtCreateWaitablePort
function  NtCreateWaitablePort(
    PortHandle : PHANDLE;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    MaxDataSize : ULONG;
    MaxMessageSize : ULONG;
    Reserved : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateWaitablePort, ntdll, 'NtCreateWaitablePort');
  Result := TFNNtCreateWaitablePort(_NtCreateWaitablePort)(
    PortHandle, ObjectAttributes, MaxDataSize, MaxMessageSize, Reserved
  );
end;

// Dynamic version of NtCreateWaitablePort
function  ZwCreateWaitablePort(
    PortHandle : PHANDLE;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    MaxDataSize : ULONG;
    MaxMessageSize : ULONG;
    Reserved : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtCreateWaitablePort, ntdll, 'NtCreateWaitablePort');
  Result := TFNNtCreateWaitablePort(_NtCreateWaitablePort)(
    PortHandle, ObjectAttributes, MaxDataSize, MaxMessageSize, Reserved
  );
end;

{.$IFNDEF JWA_INCLUDEMODE}

// Dynamic version of NtCurrentTeb
function  NtCurrentTeb(): PTEB; stdcall;
begin
  GetProcedureAddress(_NtCurrentTeb, ntdll, 'NtCurrentTeb');
  Result := TFNNtCurrentTeb(_NtCurrentTeb)();
end;
{.$ENDIF JWA_INCLUDEMODE}

// Dynamic version of NtCurrentTeb
function  ZwCurrentTeb(): PTEB; stdcall;
begin
  GetProcedureAddress(_NtCurrentTeb, ntdll, 'NtCurrentTeb');
  Result := TFNNtCurrentTeb(_NtCurrentTeb)();
end;

// Dynamic version of NtDebugActiveProcess
function  NtDebugActiveProcess(
    hProcess : HANDLE;
    hDebugObject : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtDebugActiveProcess, ntdll, 'NtDebugActiveProcess');
  Result := TFNNtDebugActiveProcess(_NtDebugActiveProcess)(
    hProcess, hDebugObject
  );
end;

// Dynamic version of NtDebugActiveProcess
function  ZwDebugActiveProcess(
    hProcess : HANDLE;
    hDebugObject : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtDebugActiveProcess, ntdll, 'NtDebugActiveProcess');
  Result := TFNNtDebugActiveProcess(_NtDebugActiveProcess)(
    hProcess, hDebugObject
  );
end;

// Dynamic version of NtDelayExecution
function  NtDelayExecution(
    Alertable : BOOLEAN;
    Interval : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtDelayExecution, ntdll, 'NtDelayExecution');
  Result := TFNNtDelayExecution(_NtDelayExecution)(
    Alertable, Interval
  );
end;

// Dynamic version of NtDelayExecution
function  ZwDelayExecution(
    Alertable : BOOLEAN;
    Interval : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtDelayExecution, ntdll, 'NtDelayExecution');
  Result := TFNNtDelayExecution(_NtDelayExecution)(
    Alertable, Interval
  );
end;

// Dynamic version of NtDeleteAtom
function  NtDeleteAtom(
    Atom : USHORT
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtDeleteAtom, ntdll, 'NtDeleteAtom');
  Result := TFNNtDeleteAtom(_NtDeleteAtom)(
    Atom
  );
end;

// Dynamic version of NtDeleteAtom
function  ZwDeleteAtom(
    Atom : USHORT
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtDeleteAtom, ntdll, 'NtDeleteAtom');
  Result := TFNNtDeleteAtom(_NtDeleteAtom)(
    Atom
  );
end;

// Dynamic version of NtDeleteFile
function  NtDeleteFile(
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtDeleteFile, ntdll, 'NtDeleteFile');
  Result := TFNNtDeleteFile(_NtDeleteFile)(
    ObjectAttributes
  );
end;

// Dynamic version of NtDeleteFile
function  ZwDeleteFile(
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtDeleteFile, ntdll, 'NtDeleteFile');
  Result := TFNNtDeleteFile(_NtDeleteFile)(
    ObjectAttributes
  );
end;

// Dynamic version of NtDeleteKey
function  NtDeleteKey(
    KeyHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtDeleteKey, ntdll, 'NtDeleteKey');
  Result := TFNNtDeleteKey(_NtDeleteKey)(
    KeyHandle
  );
end;

// Dynamic version of NtDeleteKey
function  ZwDeleteKey(
    KeyHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtDeleteKey, ntdll, 'NtDeleteKey');
  Result := TFNNtDeleteKey(_NtDeleteKey)(
    KeyHandle
  );
end;

// Dynamic version of NtDeleteObjectAuditAlarm
function  NtDeleteObjectAuditAlarm(
    SubsystemName : PUNICODE_STRING;
    HandleId : PVOID;
    GenerateOnClose : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtDeleteObjectAuditAlarm, ntdll, 'NtDeleteObjectAuditAlarm');
  Result := TFNNtDeleteObjectAuditAlarm(_NtDeleteObjectAuditAlarm)(
    SubsystemName, HandleId, GenerateOnClose
  );
end;

// Dynamic version of NtDeleteObjectAuditAlarm
function  ZwDeleteObjectAuditAlarm(
    SubsystemName : PUNICODE_STRING;
    HandleId : PVOID;
    GenerateOnClose : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtDeleteObjectAuditAlarm, ntdll, 'NtDeleteObjectAuditAlarm');
  Result := TFNNtDeleteObjectAuditAlarm(_NtDeleteObjectAuditAlarm)(
    SubsystemName, HandleId, GenerateOnClose
  );
end;

// Dynamic version of NtDeleteValueKey
function  NtDeleteValueKey(
    KeyHandle : HANDLE;
    ValueName : PUNICODE_STRING
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtDeleteValueKey, ntdll, 'NtDeleteValueKey');
  Result := TFNNtDeleteValueKey(_NtDeleteValueKey)(
    KeyHandle, ValueName
  );
end;

// Dynamic version of NtDeleteValueKey
function  ZwDeleteValueKey(
    KeyHandle : HANDLE;
    ValueName : PUNICODE_STRING
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtDeleteValueKey, ntdll, 'NtDeleteValueKey');
  Result := TFNNtDeleteValueKey(_NtDeleteValueKey)(
    KeyHandle, ValueName
  );
end;

{.$IFNDEF JWA_INCLUDEMODE}
// Dynamic version of NtDeviceIoControlFile
function  NtDeviceIoControlFile(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    IoControlCode : ULONG;
    InputBuffer : PVOID;
    InputBufferLength : ULONG;
    OutputBuffer : PVOID;
    OutputBufferLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtDeviceIoControlFile, ntdll, 'NtDeviceIoControlFile');
  Result := TFNNtDeviceIoControlFile(_NtDeviceIoControlFile)(
    FileHandle, Event, ApcRoutine, ApcContext, IoStatusBlock, IoControlCode, InputBuffer, InputBufferLength, OutputBuffer, OutputBufferLength
  );
end;
{.$ENDIF JWA_INCLUDEMODE}

// Dynamic version of NtDeviceIoControlFile
function  ZwDeviceIoControlFile(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    IoControlCode : ULONG;
    InputBuffer : PVOID;
    InputBufferLength : ULONG;
    OutputBuffer : PVOID;
    OutputBufferLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtDeviceIoControlFile, ntdll, 'NtDeviceIoControlFile');
  Result := TFNNtDeviceIoControlFile(_NtDeviceIoControlFile)(
    FileHandle, Event, ApcRoutine, ApcContext, IoStatusBlock, IoControlCode, InputBuffer, InputBufferLength, OutputBuffer, OutputBufferLength
  );
end;

// Dynamic version of NtDisplayString
function  NtDisplayString(
    Str : PUNICODE_STRING
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtDisplayString, ntdll, 'NtDisplayString');
  Result := TFNNtDisplayString(_NtDisplayString)(
    Str
  );
end;

// Dynamic version of NtDisplayString
function  ZwDisplayString(
    Str : PUNICODE_STRING
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtDisplayString, ntdll, 'NtDisplayString');
  Result := TFNNtDisplayString(_NtDisplayString)(
    Str
  );
end;

// Dynamic version of NtDuplicateObject
function  NtDuplicateObject(
    SourceProcessHandle : HANDLE;
    SourceHandle : HANDLE;
    TargetProcessHandle : HANDLE;
    TargetHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    Attributes : ULONG;
    Options : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtDuplicateObject, ntdll, 'NtDuplicateObject');
  Result := TFNNtDuplicateObject(_NtDuplicateObject)(
    SourceProcessHandle, SourceHandle, TargetProcessHandle, TargetHandle, DesiredAccess, Attributes, Options
  );
end;

// Dynamic version of NtDuplicateObject
function  ZwDuplicateObject(
    SourceProcessHandle : HANDLE;
    SourceHandle : HANDLE;
    TargetProcessHandle : HANDLE;
    TargetHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    Attributes : ULONG;
    Options : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtDuplicateObject, ntdll, 'NtDuplicateObject');
  Result := TFNNtDuplicateObject(_NtDuplicateObject)(
    SourceProcessHandle, SourceHandle, TargetProcessHandle, TargetHandle, DesiredAccess, Attributes, Options
  );
end;

// Dynamic version of NtDuplicateToken
function  NtDuplicateToken(
    ExistingTokenHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    EffectiveOnly : BOOLEAN;
    TokenType : TOKEN_TYPE;
    NewTokenHandle : PHANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtDuplicateToken, ntdll, 'NtDuplicateToken');
  Result := TFNNtDuplicateToken(_NtDuplicateToken)(
    ExistingTokenHandle, DesiredAccess, ObjectAttributes, EffectiveOnly, TokenType, NewTokenHandle
  );
end;

// Dynamic version of NtDuplicateToken
function  ZwDuplicateToken(
    ExistingTokenHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    EffectiveOnly : BOOLEAN;
    TokenType : TOKEN_TYPE;
    NewTokenHandle : PHANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtDuplicateToken, ntdll, 'NtDuplicateToken');
  Result := TFNNtDuplicateToken(_NtDuplicateToken)(
    ExistingTokenHandle, DesiredAccess, ObjectAttributes, EffectiveOnly, TokenType, NewTokenHandle
  );
end;

// Dynamic version of NtEnumerateKey
function  NtEnumerateKey(
    KeyHandle : HANDLE;
    Index : ULONG;
    KeyInformationClass : KEY_INFORMATION_CLASS;
    KeyInformation : PVOID;
    KeyInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtEnumerateKey, ntdll, 'NtEnumerateKey');
  Result := TFNNtEnumerateKey(_NtEnumerateKey)(
    KeyHandle, Index, KeyInformationClass, KeyInformation, KeyInformationLength, ResultLength
  );
end;

// Dynamic version of NtEnumerateKey
function  ZwEnumerateKey(
    KeyHandle : HANDLE;
    Index : ULONG;
    KeyInformationClass : KEY_INFORMATION_CLASS;
    KeyInformation : PVOID;
    KeyInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtEnumerateKey, ntdll, 'NtEnumerateKey');
  Result := TFNNtEnumerateKey(_NtEnumerateKey)(
    KeyHandle, Index, KeyInformationClass, KeyInformation, KeyInformationLength, ResultLength
  );
end;

// Dynamic version of NtEnumerateValueKey
function  NtEnumerateValueKey(
    KeyHandle : HANDLE;
    Index : ULONG;
    KeyValueInformationClass : KEY_VALUE_INFORMATION_CLASS;
    KeyValueInformation : PVOID;
    KeyValueInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtEnumerateValueKey, ntdll, 'NtEnumerateValueKey');
  Result := TFNNtEnumerateValueKey(_NtEnumerateValueKey)(
    KeyHandle, Index, KeyValueInformationClass, KeyValueInformation, KeyValueInformationLength, ResultLength
  );
end;

// Dynamic version of NtEnumerateValueKey
function  ZwEnumerateValueKey(
    KeyHandle : HANDLE;
    Index : ULONG;
    KeyValueInformationClass : KEY_VALUE_INFORMATION_CLASS;
    KeyValueInformation : PVOID;
    KeyValueInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtEnumerateValueKey, ntdll, 'NtEnumerateValueKey');
  Result := TFNNtEnumerateValueKey(_NtEnumerateValueKey)(
    KeyHandle, Index, KeyValueInformationClass, KeyValueInformation, KeyValueInformationLength, ResultLength
  );
end;

// Dynamic version of NtExtendSection
function  NtExtendSection(
    SectionHandle : HANDLE;
    SectionSize : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtExtendSection, ntdll, 'NtExtendSection');
  Result := TFNNtExtendSection(_NtExtendSection)(
    SectionHandle, SectionSize
  );
end;

// Dynamic version of NtExtendSection
function  ZwExtendSection(
    SectionHandle : HANDLE;
    SectionSize : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtExtendSection, ntdll, 'NtExtendSection');
  Result := TFNNtExtendSection(_NtExtendSection)(
    SectionHandle, SectionSize
  );
end;

// Dynamic version of NtFilterToken
function  NtFilterToken(
    ExistingTokenHandle : HANDLE;
    Flags : ULONG;
    SidsToDisable : PTOKEN_GROUPS;
    PrivilegesToDelete : PTOKEN_PRIVILEGES;
    SidsToRestricted : PTOKEN_GROUPS;
    NewTokenHandle : PHANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtFilterToken, ntdll, 'NtFilterToken');
  Result := TFNNtFilterToken(_NtFilterToken)(
    ExistingTokenHandle, Flags, SidsToDisable, PrivilegesToDelete, SidsToRestricted, NewTokenHandle
  );
end;

// Dynamic version of NtFilterToken
function  ZwFilterToken(
    ExistingTokenHandle : HANDLE;
    Flags : ULONG;
    SidsToDisable : PTOKEN_GROUPS;
    PrivilegesToDelete : PTOKEN_PRIVILEGES;
    SidsToRestricted : PTOKEN_GROUPS;
    NewTokenHandle : PHANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtFilterToken, ntdll, 'NtFilterToken');
  Result := TFNNtFilterToken(_NtFilterToken)(
    ExistingTokenHandle, Flags, SidsToDisable, PrivilegesToDelete, SidsToRestricted, NewTokenHandle
  );
end;

// Dynamic version of NtFindAtom
function  NtFindAtom(
    Str : PWSTR;
    StringLength : ULONG;
    Atom : PUSHORT
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtFindAtom, ntdll, 'NtFindAtom');
  Result := TFNNtFindAtom(_NtFindAtom)(
    Str, StringLength, Atom
  );
end;

// Dynamic version of NtFindAtom
function  ZwFindAtom(
    Str : PWSTR;
    StringLength : ULONG;
    Atom : PUSHORT
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtFindAtom, ntdll, 'NtFindAtom');
  Result := TFNNtFindAtom(_NtFindAtom)(
    Str, StringLength, Atom
  );
end;

// Dynamic version of NtFlushBuffersFile
function  NtFlushBuffersFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtFlushBuffersFile, ntdll, 'NtFlushBuffersFile');
  Result := TFNNtFlushBuffersFile(_NtFlushBuffersFile)(
    FileHandle, IoStatusBlock
  );
end;

// Dynamic version of NtFlushBuffersFile
function  ZwFlushBuffersFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtFlushBuffersFile, ntdll, 'NtFlushBuffersFile');
  Result := TFNNtFlushBuffersFile(_NtFlushBuffersFile)(
    FileHandle, IoStatusBlock
  );
end;

// Dynamic version of NtFlushInstructionCache
function  NtFlushInstructionCache(
    ProcessHandle : HANDLE;
    BaseAddress : PVOID;
    FlushSize : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtFlushInstructionCache, ntdll, 'NtFlushInstructionCache');
  Result := TFNNtFlushInstructionCache(_NtFlushInstructionCache)(
    ProcessHandle, BaseAddress, FlushSize
  );
end;

// Dynamic version of NtFlushInstructionCache
function  ZwFlushInstructionCache(
    ProcessHandle : HANDLE;
    BaseAddress : PVOID;
    FlushSize : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtFlushInstructionCache, ntdll, 'NtFlushInstructionCache');
  Result := TFNNtFlushInstructionCache(_NtFlushInstructionCache)(
    ProcessHandle, BaseAddress, FlushSize
  );
end;

// Dynamic version of NtFlushKey
function  NtFlushKey(
    KeyHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtFlushKey, ntdll, 'NtFlushKey');
  Result := TFNNtFlushKey(_NtFlushKey)(
    KeyHandle
  );
end;

// Dynamic version of NtFlushKey
function  ZwFlushKey(
    KeyHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtFlushKey, ntdll, 'NtFlushKey');
  Result := TFNNtFlushKey(_NtFlushKey)(
    KeyHandle
  );
end;

// Dynamic version of NtFlushVirtualMemory
function  NtFlushVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PPVOID;
    FlushSize : PULONG;
    IoStatusBlock : PIO_STATUS_BLOCK
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtFlushVirtualMemory, ntdll, 'NtFlushVirtualMemory');
  Result := TFNNtFlushVirtualMemory(_NtFlushVirtualMemory)(
    ProcessHandle, BaseAddress, FlushSize, IoStatusBlock
  );
end;

// Dynamic version of NtFlushVirtualMemory
function  ZwFlushVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PPVOID;
    FlushSize : PULONG;
    IoStatusBlock : PIO_STATUS_BLOCK
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtFlushVirtualMemory, ntdll, 'NtFlushVirtualMemory');
  Result := TFNNtFlushVirtualMemory(_NtFlushVirtualMemory)(
    ProcessHandle, BaseAddress, FlushSize, IoStatusBlock
  );
end;

// Dynamic version of NtFlushWriteBuffer
function  NtFlushWriteBuffer(): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtFlushWriteBuffer, ntdll, 'NtFlushWriteBuffer');
  Result := TFNNtFlushWriteBuffer(_NtFlushWriteBuffer)();
end;

// Dynamic version of NtFlushWriteBuffer
function  ZwFlushWriteBuffer(): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtFlushWriteBuffer, ntdll, 'NtFlushWriteBuffer');
  Result := TFNNtFlushWriteBuffer(_NtFlushWriteBuffer)();
end;

// Dynamic version of NtFreeUserPhysicalPages
function  NtFreeUserPhysicalPages(
    ProcessHandle : HANDLE;
    NumberOfPages : PULONG;
    PageFrameNumbers : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtFreeUserPhysicalPages, ntdll, 'NtFreeUserPhysicalPages');
  Result := TFNNtFreeUserPhysicalPages(_NtFreeUserPhysicalPages)(
    ProcessHandle, NumberOfPages, PageFrameNumbers
  );
end;

// Dynamic version of NtFreeUserPhysicalPages
function  ZwFreeUserPhysicalPages(
    ProcessHandle : HANDLE;
    NumberOfPages : PULONG;
    PageFrameNumbers : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtFreeUserPhysicalPages, ntdll, 'NtFreeUserPhysicalPages');
  Result := TFNNtFreeUserPhysicalPages(_NtFreeUserPhysicalPages)(
    ProcessHandle, NumberOfPages, PageFrameNumbers
  );
end;

// Dynamic version of NtFreeVirtualMemory
function  NtFreeVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PPVOID;
    FreeSize : PULONG;
    FreeType : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtFreeVirtualMemory, ntdll, 'NtFreeVirtualMemory');
  Result := TFNNtFreeVirtualMemory(_NtFreeVirtualMemory)(
    ProcessHandle, BaseAddress, FreeSize, FreeType
  );
end;

// Dynamic version of NtFreeVirtualMemory
function  ZwFreeVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PPVOID;
    FreeSize : PULONG;
    FreeType : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtFreeVirtualMemory, ntdll, 'NtFreeVirtualMemory');
  Result := TFNNtFreeVirtualMemory(_NtFreeVirtualMemory)(
    ProcessHandle, BaseAddress, FreeSize, FreeType
  );
end;

// Dynamic version of NtFsControlFile
function  NtFsControlFile(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    FsControlCode : ULONG;
    InputBuffer : PVOID;
    InputBufferLength : ULONG;
    OutputBuffer : PVOID;
    OutputBufferLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtFsControlFile, ntdll, 'NtFsControlFile');
  Result := TFNNtFsControlFile(_NtFsControlFile)(
    FileHandle, Event, ApcRoutine, ApcContext, IoStatusBlock, FsControlCode, InputBuffer, InputBufferLength, OutputBuffer, OutputBufferLength
  );
end;

// Dynamic version of NtFsControlFile
function  ZwFsControlFile(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    FsControlCode : ULONG;
    InputBuffer : PVOID;
    InputBufferLength : ULONG;
    OutputBuffer : PVOID;
    OutputBufferLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtFsControlFile, ntdll, 'NtFsControlFile');
  Result := TFNNtFsControlFile(_NtFsControlFile)(
    FileHandle, Event, ApcRoutine, ApcContext, IoStatusBlock, FsControlCode, InputBuffer, InputBufferLength, OutputBuffer, OutputBufferLength
  );
end;

// Dynamic version of NtGetContextThread
function  NtGetContextThread(
    ThreadHandle : HANDLE;
    Context : PCONTEXT
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtGetContextThread, ntdll, 'NtGetContextThread');
  Result := TFNNtGetContextThread(_NtGetContextThread)(
    ThreadHandle, Context
  );
end;

// Dynamic version of NtGetContextThread
function  ZwGetContextThread(
    ThreadHandle : HANDLE;
    Context : PCONTEXT
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtGetContextThread, ntdll, 'NtGetContextThread');
  Result := TFNNtGetContextThread(_NtGetContextThread)(
    ThreadHandle, Context
  );
end;

// Dynamic version of NtGetCurrentProcessorNumber
function  NtGetCurrentProcessorNumber(): ULONG; stdcall;
begin
  GetProcedureAddress(_NtGetCurrentProcessorNumber, ntdll, 'NtGetCurrentProcessorNumber');
  Result := TFNNtGetCurrentProcessorNumber(_NtGetCurrentProcessorNumber)();
end;

// Dynamic version of NtGetCurrentProcessorNumber
function  ZwGetCurrentProcessorNumber(): ULONG; stdcall;
begin
  GetProcedureAddress(_NtGetCurrentProcessorNumber, ntdll, 'NtGetCurrentProcessorNumber');
  Result := TFNNtGetCurrentProcessorNumber(_NtGetCurrentProcessorNumber)();
end;

// Dynamic version of NtGetDevicePowerState
function  NtGetDevicePowerState(
    DeviceHandle : HANDLE;
    DevicePowerState : PDEVICE_POWER_STATE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtGetDevicePowerState, ntdll, 'NtGetDevicePowerState');
  Result := TFNNtGetDevicePowerState(_NtGetDevicePowerState)(
    DeviceHandle, DevicePowerState
  );
end;

// Dynamic version of NtGetDevicePowerState
function  ZwGetDevicePowerState(
    DeviceHandle : HANDLE;
    DevicePowerState : PDEVICE_POWER_STATE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtGetDevicePowerState, ntdll, 'NtGetDevicePowerState');
  Result := TFNNtGetDevicePowerState(_NtGetDevicePowerState)(
    DeviceHandle, DevicePowerState
  );
end;

// Dynamic version of NtGetPlugPlayEvent
function  NtGetPlugPlayEvent(
    Reserved1 : ULONG;
    Reserved2 : ULONG;
    Buffer : PVOID;
    BufferLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtGetPlugPlayEvent, ntdll, 'NtGetPlugPlayEvent');
  Result := TFNNtGetPlugPlayEvent(_NtGetPlugPlayEvent)(
    Reserved1, Reserved2, Buffer, BufferLength
  );
end;

// Dynamic version of NtGetPlugPlayEvent
function  ZwGetPlugPlayEvent(
    Reserved1 : ULONG;
    Reserved2 : ULONG;
    Buffer : PVOID;
    BufferLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtGetPlugPlayEvent, ntdll, 'NtGetPlugPlayEvent');
  Result := TFNNtGetPlugPlayEvent(_NtGetPlugPlayEvent)(
    Reserved1, Reserved2, Buffer, BufferLength
  );
end;

// Dynamic version of NtGetTickCount
function  NtGetTickCount(): ULONG; stdcall;
begin
  GetProcedureAddress(_NtGetTickCount, ntdll, 'NtGetTickCount');
  Result := TFNNtGetTickCount(_NtGetTickCount)();
end;

// Dynamic version of NtGetTickCount
function  ZwGetTickCount(): ULONG; stdcall;
begin
  GetProcedureAddress(_NtGetTickCount, ntdll, 'NtGetTickCount');
  Result := TFNNtGetTickCount(_NtGetTickCount)();
end;

// Dynamic version of NtGetWriteWatch
function  NtGetWriteWatch(
    ProcessHandle : HANDLE;
    Flags : ULONG;
    BaseAddress : PVOID;
    RegionSize : ULONG;
    Buffer : PULONG;
    BufferEntries : PULONG;
    Granularity : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtGetWriteWatch, ntdll, 'NtGetWriteWatch');
  Result := TFNNtGetWriteWatch(_NtGetWriteWatch)(
    ProcessHandle, Flags, BaseAddress, RegionSize, Buffer, BufferEntries, Granularity
  );
end;

// Dynamic version of NtGetWriteWatch
function  ZwGetWriteWatch(
    ProcessHandle : HANDLE;
    Flags : ULONG;
    BaseAddress : PVOID;
    RegionSize : ULONG;
    Buffer : PULONG;
    BufferEntries : PULONG;
    Granularity : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtGetWriteWatch, ntdll, 'NtGetWriteWatch');
  Result := TFNNtGetWriteWatch(_NtGetWriteWatch)(
    ProcessHandle, Flags, BaseAddress, RegionSize, Buffer, BufferEntries, Granularity
  );
end;

// Dynamic version of NtImpersonateAnonymousToken
function  NtImpersonateAnonymousToken(
    ThreadHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtImpersonateAnonymousToken, ntdll, 'NtImpersonateAnonymousToken');
  Result := TFNNtImpersonateAnonymousToken(_NtImpersonateAnonymousToken)(
    ThreadHandle
  );
end;

// Dynamic version of NtImpersonateAnonymousToken
function  ZwImpersonateAnonymousToken(
    ThreadHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtImpersonateAnonymousToken, ntdll, 'NtImpersonateAnonymousToken');
  Result := TFNNtImpersonateAnonymousToken(_NtImpersonateAnonymousToken)(
    ThreadHandle
  );
end;

// Dynamic version of NtImpersonateClientOfPort
function  NtImpersonateClientOfPort(
    PortHandle : HANDLE;
    Message : PPORT_MESSAGE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtImpersonateClientOfPort, ntdll, 'NtImpersonateClientOfPort');
  Result := TFNNtImpersonateClientOfPort(_NtImpersonateClientOfPort)(
    PortHandle, Message
  );
end;

// Dynamic version of NtImpersonateClientOfPort
function  ZwImpersonateClientOfPort(
    PortHandle : HANDLE;
    Message : PPORT_MESSAGE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtImpersonateClientOfPort, ntdll, 'NtImpersonateClientOfPort');
  Result := TFNNtImpersonateClientOfPort(_NtImpersonateClientOfPort)(
    PortHandle, Message
  );
end;

// Dynamic version of NtImpersonateThread
function  NtImpersonateThread(
    ThreadHandle : HANDLE;
    TargetThreadHandle : HANDLE;
    SecurityQos : PSECURITY_QUALITY_OF_SERVICE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtImpersonateThread, ntdll, 'NtImpersonateThread');
  Result := TFNNtImpersonateThread(_NtImpersonateThread)(
    ThreadHandle, TargetThreadHandle, SecurityQos
  );
end;

// Dynamic version of NtImpersonateThread
function  ZwImpersonateThread(
    ThreadHandle : HANDLE;
    TargetThreadHandle : HANDLE;
    SecurityQos : PSECURITY_QUALITY_OF_SERVICE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtImpersonateThread, ntdll, 'NtImpersonateThread');
  Result := TFNNtImpersonateThread(_NtImpersonateThread)(
    ThreadHandle, TargetThreadHandle, SecurityQos
  );
end;

// Dynamic version of NtInitializeRegistry
function  NtInitializeRegistry(
    Setup : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtInitializeRegistry, ntdll, 'NtInitializeRegistry');
  Result := TFNNtInitializeRegistry(_NtInitializeRegistry)(
    Setup
  );
end;

// Dynamic version of NtInitializeRegistry
function  ZwInitializeRegistry(
    Setup : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtInitializeRegistry, ntdll, 'NtInitializeRegistry');
  Result := TFNNtInitializeRegistry(_NtInitializeRegistry)(
    Setup
  );
end;

// Dynamic version of NtInitiatePowerAction
function  NtInitiatePowerAction(
    SystemAction : POWER_ACTION;
    MinSystemState : SYSTEM_POWER_STATE;
    Flags : ULONG;
    Asynchronous : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtInitiatePowerAction, ntdll, 'NtInitiatePowerAction');
  Result := TFNNtInitiatePowerAction(_NtInitiatePowerAction)(
    SystemAction, MinSystemState, Flags, Asynchronous
  );
end;

// Dynamic version of NtInitiatePowerAction
function  ZwInitiatePowerAction(
    SystemAction : POWER_ACTION;
    MinSystemState : SYSTEM_POWER_STATE;
    Flags : ULONG;
    Asynchronous : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtInitiatePowerAction, ntdll, 'NtInitiatePowerAction');
  Result := TFNNtInitiatePowerAction(_NtInitiatePowerAction)(
    SystemAction, MinSystemState, Flags, Asynchronous
  );
end;

// Dynamic version of NtIsSystemResumeAutomatic
function  NtIsSystemResumeAutomatic(): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_NtIsSystemResumeAutomatic, ntdll, 'NtIsSystemResumeAutomatic');
  Result := TFNNtIsSystemResumeAutomatic(_NtIsSystemResumeAutomatic)();
end;

// Dynamic version of NtIsSystemResumeAutomatic
function  ZwIsSystemResumeAutomatic(): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_NtIsSystemResumeAutomatic, ntdll, 'NtIsSystemResumeAutomatic');
  Result := TFNNtIsSystemResumeAutomatic(_NtIsSystemResumeAutomatic)();
end;

// Dynamic version of NtListenChannel
function  NtListenChannel(
    x : PVOID;
    y : PVOID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtListenChannel, ntdll, 'NtListenChannel');
  Result := TFNNtListenChannel(_NtListenChannel)(
    x, y
  );
end;

// Dynamic version of NtListenChannel
function  ZwListenChannel(
    x : PVOID;
    y : PVOID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtListenChannel, ntdll, 'NtListenChannel');
  Result := TFNNtListenChannel(_NtListenChannel)(
    x, y
  );
end;

// Dynamic version of NtListenPort
function  NtListenPort(
    PortHandle : HANDLE;
    Message : PPORT_MESSAGE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtListenPort, ntdll, 'NtListenPort');
  Result := TFNNtListenPort(_NtListenPort)(
    PortHandle, Message
  );
end;

// Dynamic version of NtListenPort
function  ZwListenPort(
    PortHandle : HANDLE;
    Message : PPORT_MESSAGE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtListenPort, ntdll, 'NtListenPort');
  Result := TFNNtListenPort(_NtListenPort)(
    PortHandle, Message
  );
end;

// Dynamic version of NtLoadDriver
function  NtLoadDriver(
    DriverServiceName : PUNICODE_STRING
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtLoadDriver, ntdll, 'NtLoadDriver');
  Result := TFNNtLoadDriver(_NtLoadDriver)(
    DriverServiceName
  );
end;

// Dynamic version of NtLoadDriver
function  ZwLoadDriver(
    DriverServiceName : PUNICODE_STRING
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtLoadDriver, ntdll, 'NtLoadDriver');
  Result := TFNNtLoadDriver(_NtLoadDriver)(
    DriverServiceName
  );
end;

// Dynamic version of NtLoadKey
function  NtLoadKey(
    KeyObjectAttributes : POBJECT_ATTRIBUTES;
    FileObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtLoadKey, ntdll, 'NtLoadKey');
  Result := TFNNtLoadKey(_NtLoadKey)(
    KeyObjectAttributes, FileObjectAttributes
  );
end;

// Dynamic version of NtLoadKey
function  ZwLoadKey(
    KeyObjectAttributes : POBJECT_ATTRIBUTES;
    FileObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtLoadKey, ntdll, 'NtLoadKey');
  Result := TFNNtLoadKey(_NtLoadKey)(
    KeyObjectAttributes, FileObjectAttributes
  );
end;

// Dynamic version of NtLoadKey2
function  NtLoadKey2(
    KeyObjectAttributes : POBJECT_ATTRIBUTES;
    FileObjectAttributes : POBJECT_ATTRIBUTES;
    Flags : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtLoadKey2, ntdll, 'NtLoadKey2');
  Result := TFNNtLoadKey2(_NtLoadKey2)(
    KeyObjectAttributes, FileObjectAttributes, Flags
  );
end;

// Dynamic version of NtLoadKey2
function  ZwLoadKey2(
    KeyObjectAttributes : POBJECT_ATTRIBUTES;
    FileObjectAttributes : POBJECT_ATTRIBUTES;
    Flags : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtLoadKey2, ntdll, 'NtLoadKey2');
  Result := TFNNtLoadKey2(_NtLoadKey2)(
    KeyObjectAttributes, FileObjectAttributes, Flags
  );
end;

// Dynamic version of NtLockFile
function  NtLockFile(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    LockOffset : PULARGE_INTEGER;
    LockLength : PULARGE_INTEGER;
    Key : ULONG;
    FailImmediately : BOOLEAN;
    ExclusiveLock : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtLockFile, ntdll, 'NtLockFile');
  Result := TFNNtLockFile(_NtLockFile)(
    FileHandle, Event, ApcRoutine, ApcContext, IoStatusBlock, LockOffset, LockLength, Key, FailImmediately, ExclusiveLock
  );
end;

// Dynamic version of NtLockFile
function  ZwLockFile(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    LockOffset : PULARGE_INTEGER;
    LockLength : PULARGE_INTEGER;
    Key : ULONG;
    FailImmediately : BOOLEAN;
    ExclusiveLock : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtLockFile, ntdll, 'NtLockFile');
  Result := TFNNtLockFile(_NtLockFile)(
    FileHandle, Event, ApcRoutine, ApcContext, IoStatusBlock, LockOffset, LockLength, Key, FailImmediately, ExclusiveLock
  );
end;

// Dynamic version of NtLockVirtualMemory
function  NtLockVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PPVOID;
    LockSize : PULONG;
    LockType : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtLockVirtualMemory, ntdll, 'NtLockVirtualMemory');
  Result := TFNNtLockVirtualMemory(_NtLockVirtualMemory)(
    ProcessHandle, BaseAddress, LockSize, LockType
  );
end;

// Dynamic version of NtLockVirtualMemory
function  ZwLockVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PPVOID;
    LockSize : PULONG;
    LockType : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtLockVirtualMemory, ntdll, 'NtLockVirtualMemory');
  Result := TFNNtLockVirtualMemory(_NtLockVirtualMemory)(
    ProcessHandle, BaseAddress, LockSize, LockType
  );
end;

// Dynamic version of NtMakePermanentObject
function  NtMakePermanentObject(
    Handle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtMakePermanentObject, ntdll, 'NtMakePermanentObject');
  Result := TFNNtMakePermanentObject(_NtMakePermanentObject)(
    Handle
  );
end;

// Dynamic version of NtMakePermanentObject
function  ZwMakePermanentObject(
    Handle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtMakePermanentObject, ntdll, 'NtMakePermanentObject');
  Result := TFNNtMakePermanentObject(_NtMakePermanentObject)(
    Handle
  );
end;

// Dynamic version of NtMakeTemporaryObject
function  NtMakeTemporaryObject(
    Handle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtMakeTemporaryObject, ntdll, 'NtMakeTemporaryObject');
  Result := TFNNtMakeTemporaryObject(_NtMakeTemporaryObject)(
    Handle
  );
end;

// Dynamic version of NtMakeTemporaryObject
function  ZwMakeTemporaryObject(
    Handle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtMakeTemporaryObject, ntdll, 'NtMakeTemporaryObject');
  Result := TFNNtMakeTemporaryObject(_NtMakeTemporaryObject)(
    Handle
  );
end;

// Dynamic version of NtMapUserPhysicalPages
function  NtMapUserPhysicalPages(
    BaseAddress : PVOID;
    NumberOfPages : PULONG;
    PageFrameNumbers : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtMapUserPhysicalPages, ntdll, 'NtMapUserPhysicalPages');
  Result := TFNNtMapUserPhysicalPages(_NtMapUserPhysicalPages)(
    BaseAddress, NumberOfPages, PageFrameNumbers
  );
end;

// Dynamic version of NtMapUserPhysicalPages
function  ZwMapUserPhysicalPages(
    BaseAddress : PVOID;
    NumberOfPages : PULONG;
    PageFrameNumbers : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtMapUserPhysicalPages, ntdll, 'NtMapUserPhysicalPages');
  Result := TFNNtMapUserPhysicalPages(_NtMapUserPhysicalPages)(
    BaseAddress, NumberOfPages, PageFrameNumbers
  );
end;

// Dynamic version of NtMapUserPhysicalPagesScatter
function  NtMapUserPhysicalPagesScatter(
    BaseAddresses : PPVOID;
    NumberOfPages : PULONG;
    PageFrameNumbers : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtMapUserPhysicalPagesScatter, ntdll, 'NtMapUserPhysicalPagesScatter');
  Result := TFNNtMapUserPhysicalPagesScatter(_NtMapUserPhysicalPagesScatter)(
    BaseAddresses, NumberOfPages, PageFrameNumbers
  );
end;

// Dynamic version of NtMapUserPhysicalPagesScatter
function  ZwMapUserPhysicalPagesScatter(
    BaseAddresses : PPVOID;
    NumberOfPages : PULONG;
    PageFrameNumbers : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtMapUserPhysicalPagesScatter, ntdll, 'NtMapUserPhysicalPagesScatter');
  Result := TFNNtMapUserPhysicalPagesScatter(_NtMapUserPhysicalPagesScatter)(
    BaseAddresses, NumberOfPages, PageFrameNumbers
  );
end;

// Dynamic version of NtMapViewOfSection
function  NtMapViewOfSection(
    SectionHandle : HANDLE;
    ProcessHandle : HANDLE;
    BaseAddress : PPVOID;
    ZeroBits : ULONG;
    CommitSize : ULONG;
    SectionOffset : PLARGE_INTEGER;
    ViewSize : PULONG;
    InheritDisposition : SECTION_INHERIT;
    AllocationType : ULONG;
    Protect : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtMapViewOfSection, ntdll, 'NtMapViewOfSection');
  Result := TFNNtMapViewOfSection(_NtMapViewOfSection)(
    SectionHandle, ProcessHandle, BaseAddress, ZeroBits, CommitSize, SectionOffset, ViewSize, InheritDisposition, AllocationType, Protect
  );
end;

// Dynamic version of NtMapViewOfSection
function  ZwMapViewOfSection(
    SectionHandle : HANDLE;
    ProcessHandle : HANDLE;
    BaseAddress : PPVOID;
    ZeroBits : ULONG;
    CommitSize : ULONG;
    SectionOffset : PLARGE_INTEGER;
    ViewSize : PULONG;
    InheritDisposition : SECTION_INHERIT;
    AllocationType : ULONG;
    Protect : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtMapViewOfSection, ntdll, 'NtMapViewOfSection');
  Result := TFNNtMapViewOfSection(_NtMapViewOfSection)(
    SectionHandle, ProcessHandle, BaseAddress, ZeroBits, CommitSize, SectionOffset, ViewSize, InheritDisposition, AllocationType, Protect
  );
end;

// Dynamic version of NtNotifyChangeDirectoryFile
function  NtNotifyChangeDirectoryFile(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PFILE_NOTIFY_INFORMATION;
    BufferLength : ULONG;
    NotifyFilter : ULONG;
    WatchSubtree : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtNotifyChangeDirectoryFile, ntdll, 'NtNotifyChangeDirectoryFile');
  Result := TFNNtNotifyChangeDirectoryFile(_NtNotifyChangeDirectoryFile)(
    FileHandle, Event, ApcRoutine, ApcContext, IoStatusBlock, Buffer, BufferLength, NotifyFilter, WatchSubtree
  );
end;

// Dynamic version of NtNotifyChangeDirectoryFile
function  ZwNotifyChangeDirectoryFile(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PFILE_NOTIFY_INFORMATION;
    BufferLength : ULONG;
    NotifyFilter : ULONG;
    WatchSubtree : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtNotifyChangeDirectoryFile, ntdll, 'NtNotifyChangeDirectoryFile');
  Result := TFNNtNotifyChangeDirectoryFile(_NtNotifyChangeDirectoryFile)(
    FileHandle, Event, ApcRoutine, ApcContext, IoStatusBlock, Buffer, BufferLength, NotifyFilter, WatchSubtree
  );
end;

// Dynamic version of NtNotifyChangeKey
function  NtNotifyChangeKey(
    KeyHandle : HANDLE;
    EventHandle : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    NotifyFilter : ULONG;
    WatchSubtree : BOOLEAN;
    Buffer : PVOID;
    BufferLength : ULONG;
    Asynchronous : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtNotifyChangeKey, ntdll, 'NtNotifyChangeKey');
  Result := TFNNtNotifyChangeKey(_NtNotifyChangeKey)(
    KeyHandle, EventHandle, ApcRoutine, ApcContext, IoStatusBlock, NotifyFilter, WatchSubtree, Buffer, BufferLength, Asynchronous
  );
end;

// Dynamic version of NtNotifyChangeKey
function  ZwNotifyChangeKey(
    KeyHandle : HANDLE;
    EventHandle : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    NotifyFilter : ULONG;
    WatchSubtree : BOOLEAN;
    Buffer : PVOID;
    BufferLength : ULONG;
    Asynchronous : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtNotifyChangeKey, ntdll, 'NtNotifyChangeKey');
  Result := TFNNtNotifyChangeKey(_NtNotifyChangeKey)(
    KeyHandle, EventHandle, ApcRoutine, ApcContext, IoStatusBlock, NotifyFilter, WatchSubtree, Buffer, BufferLength, Asynchronous
  );
end;

// Dynamic version of NtNotifyChangeMultipleKeys
function  NtNotifyChangeMultipleKeys(
    KeyHandle : HANDLE;
    Flags : ULONG;
    KeyObjectAttributes : POBJECT_ATTRIBUTES;
    EventHandle : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    NotifyFilter : ULONG;
    WatchSubtree : BOOLEAN;
    Buffer : PVOID;
    BufferLength : ULONG;
    Asynchronous : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtNotifyChangeMultipleKeys, ntdll, 'NtNotifyChangeMultipleKeys');
  Result := TFNNtNotifyChangeMultipleKeys(_NtNotifyChangeMultipleKeys)(
    KeyHandle, Flags, KeyObjectAttributes, EventHandle, ApcRoutine, ApcContext, IoStatusBlock, NotifyFilter, WatchSubtree, Buffer, BufferLength, Asynchronous
  );
end;

// Dynamic version of NtNotifyChangeMultipleKeys
function  ZwNotifyChangeMultipleKeys(
    KeyHandle : HANDLE;
    Flags : ULONG;
    KeyObjectAttributes : POBJECT_ATTRIBUTES;
    EventHandle : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    NotifyFilter : ULONG;
    WatchSubtree : BOOLEAN;
    Buffer : PVOID;
    BufferLength : ULONG;
    Asynchronous : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtNotifyChangeMultipleKeys, ntdll, 'NtNotifyChangeMultipleKeys');
  Result := TFNNtNotifyChangeMultipleKeys(_NtNotifyChangeMultipleKeys)(
    KeyHandle, Flags, KeyObjectAttributes, EventHandle, ApcRoutine, ApcContext, IoStatusBlock, NotifyFilter, WatchSubtree, Buffer, BufferLength, Asynchronous
  );
end;

// Dynamic version of NtOpenChannel
function  NtOpenChannel(
    ChannelHandle : PHANDLE;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenChannel, ntdll, 'NtOpenChannel');
  Result := TFNNtOpenChannel(_NtOpenChannel)(
    ChannelHandle, ObjectAttributes
  );
end;

// Dynamic version of NtOpenChannel
function  ZwOpenChannel(
    ChannelHandle : PHANDLE;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenChannel, ntdll, 'NtOpenChannel');
  Result := TFNNtOpenChannel(_NtOpenChannel)(
    ChannelHandle, ObjectAttributes
  );
end;

// Dynamic version of NtOpenDirectoryObject
function  NtOpenDirectoryObject(
    DirectoryHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenDirectoryObject, ntdll, 'NtOpenDirectoryObject');
  Result := TFNNtOpenDirectoryObject(_NtOpenDirectoryObject)(
    DirectoryHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtOpenDirectoryObject
function  ZwOpenDirectoryObject(
    DirectoryHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenDirectoryObject, ntdll, 'NtOpenDirectoryObject');
  Result := TFNNtOpenDirectoryObject(_NtOpenDirectoryObject)(
    DirectoryHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtOpenEvent
function  NtOpenEvent(
    EventHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenEvent, ntdll, 'NtOpenEvent');
  Result := TFNNtOpenEvent(_NtOpenEvent)(
    EventHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtOpenEvent
function  ZwOpenEvent(
    EventHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenEvent, ntdll, 'NtOpenEvent');
  Result := TFNNtOpenEvent(_NtOpenEvent)(
    EventHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtOpenEventPair
function  NtOpenEventPair(
    EventPairHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenEventPair, ntdll, 'NtOpenEventPair');
  Result := TFNNtOpenEventPair(_NtOpenEventPair)(
    EventPairHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtOpenEventPair
function  ZwOpenEventPair(
    EventPairHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenEventPair, ntdll, 'NtOpenEventPair');
  Result := TFNNtOpenEventPair(_NtOpenEventPair)(
    EventPairHandle, DesiredAccess, ObjectAttributes
  );
end;

{.$IFNDEF JWA_INCLUDEMODE}
// Dynamic version of NtOpenFile
function  NtOpenFile(
    FileHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    IoStatusBlock : PIO_STATUS_BLOCK;
    ShareAccess : ULONG;
    OpenOptions : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenFile, ntdll, 'NtOpenFile');
  Result := TFNNtOpenFile(_NtOpenFile)(
    FileHandle, DesiredAccess, ObjectAttributes, IoStatusBlock, ShareAccess, OpenOptions
  );
end;
{.$ENDIF JWA_INCLUDEMODE}

// Dynamic version of NtOpenFile
function  ZwOpenFile(
    FileHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    IoStatusBlock : PIO_STATUS_BLOCK;
    ShareAccess : ULONG;
    OpenOptions : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenFile, ntdll, 'NtOpenFile');
  Result := TFNNtOpenFile(_NtOpenFile)(
    FileHandle, DesiredAccess, ObjectAttributes, IoStatusBlock, ShareAccess, OpenOptions
  );
end;

// Dynamic version of NtOpenIoCompletion
function  NtOpenIoCompletion(
    IoCompletionHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenIoCompletion, ntdll, 'NtOpenIoCompletion');
  Result := TFNNtOpenIoCompletion(_NtOpenIoCompletion)(
    IoCompletionHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtOpenIoCompletion
function  ZwOpenIoCompletion(
    IoCompletionHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenIoCompletion, ntdll, 'NtOpenIoCompletion');
  Result := TFNNtOpenIoCompletion(_NtOpenIoCompletion)(
    IoCompletionHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtOpenJobObject
function  NtOpenJobObject(
    JobHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenJobObject, ntdll, 'NtOpenJobObject');
  Result := TFNNtOpenJobObject(_NtOpenJobObject)(
    JobHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtOpenJobObject
function  ZwOpenJobObject(
    JobHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenJobObject, ntdll, 'NtOpenJobObject');
  Result := TFNNtOpenJobObject(_NtOpenJobObject)(
    JobHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtOpenKey
function  NtOpenKey(
    KeyHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenKey, ntdll, 'NtOpenKey');
  Result := TFNNtOpenKey(_NtOpenKey)(
    KeyHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtOpenKey
function  ZwOpenKey(
    KeyHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenKey, ntdll, 'NtOpenKey');
  Result := TFNNtOpenKey(_NtOpenKey)(
    KeyHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtOpenMutant
function  NtOpenMutant(
    MutantHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenMutant, ntdll, 'NtOpenMutant');
  Result := TFNNtOpenMutant(_NtOpenMutant)(
    MutantHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtOpenMutant
function  ZwOpenMutant(
    MutantHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenMutant, ntdll, 'NtOpenMutant');
  Result := TFNNtOpenMutant(_NtOpenMutant)(
    MutantHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtOpenObjectAuditAlarm
function  NtOpenObjectAuditAlarm(
    SubsystemName : PUNICODE_STRING;
    HandleId : PPVOID;
    ObjectTypeName : PUNICODE_STRING;
    ObjectName : PUNICODE_STRING;
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    TokenHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    GrantedAccess : ACCESS_MASK;
    Privileges : PPRIVILEGE_SET;
    ObjectCreation : BOOLEAN;
    AccessGranted : BOOLEAN;
    GenerateOnClose : PBOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenObjectAuditAlarm, ntdll, 'NtOpenObjectAuditAlarm');
  Result := TFNNtOpenObjectAuditAlarm(_NtOpenObjectAuditAlarm)(
    SubsystemName, HandleId, ObjectTypeName, ObjectName, SecurityDescriptor, TokenHandle, DesiredAccess, GrantedAccess, Privileges, ObjectCreation, AccessGranted, GenerateOnClose
  );
end;

// Dynamic version of NtOpenObjectAuditAlarm
function  ZwOpenObjectAuditAlarm(
    SubsystemName : PUNICODE_STRING;
    HandleId : PPVOID;
    ObjectTypeName : PUNICODE_STRING;
    ObjectName : PUNICODE_STRING;
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    TokenHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    GrantedAccess : ACCESS_MASK;
    Privileges : PPRIVILEGE_SET;
    ObjectCreation : BOOLEAN;
    AccessGranted : BOOLEAN;
    GenerateOnClose : PBOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenObjectAuditAlarm, ntdll, 'NtOpenObjectAuditAlarm');
  Result := TFNNtOpenObjectAuditAlarm(_NtOpenObjectAuditAlarm)(
    SubsystemName, HandleId, ObjectTypeName, ObjectName, SecurityDescriptor, TokenHandle, DesiredAccess, GrantedAccess, Privileges, ObjectCreation, AccessGranted, GenerateOnClose
  );
end;

// Dynamic version of NtOpenProcess
function  NtOpenProcess(
    ProcessHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    ClientId : PCLIENT_ID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenProcess, ntdll, 'NtOpenProcess');
  Result := TFNNtOpenProcess(_NtOpenProcess)(
    ProcessHandle, DesiredAccess, ObjectAttributes, ClientId
  );
end;

// Dynamic version of NtOpenProcess
function  ZwOpenProcess(
    ProcessHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    ClientId : PCLIENT_ID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenProcess, ntdll, 'NtOpenProcess');
  Result := TFNNtOpenProcess(_NtOpenProcess)(
    ProcessHandle, DesiredAccess, ObjectAttributes, ClientId
  );
end;

// Dynamic version of NtOpenProcessToken
function  NtOpenProcessToken(
    ProcessHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    TokenHandle : PHANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenProcessToken, ntdll, 'NtOpenProcessToken');
  Result := TFNNtOpenProcessToken(_NtOpenProcessToken)(
    ProcessHandle, DesiredAccess, TokenHandle
  );
end;

// Dynamic version of NtOpenProcessToken
function  ZwOpenProcessToken(
    ProcessHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    TokenHandle : PHANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenProcessToken, ntdll, 'NtOpenProcessToken');
  Result := TFNNtOpenProcessToken(_NtOpenProcessToken)(
    ProcessHandle, DesiredAccess, TokenHandle
  );
end;

// Dynamic version of NtOpenSection
function  NtOpenSection(
    SectionHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenSection, ntdll, 'NtOpenSection');
  Result := TFNNtOpenSection(_NtOpenSection)(
    SectionHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtOpenSection
function  ZwOpenSection(
    SectionHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenSection, ntdll, 'NtOpenSection');
  Result := TFNNtOpenSection(_NtOpenSection)(
    SectionHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtOpenSemaphore
function  NtOpenSemaphore(
    SemaphoreHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenSemaphore, ntdll, 'NtOpenSemaphore');
  Result := TFNNtOpenSemaphore(_NtOpenSemaphore)(
    SemaphoreHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtOpenSemaphore
function  ZwOpenSemaphore(
    SemaphoreHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenSemaphore, ntdll, 'NtOpenSemaphore');
  Result := TFNNtOpenSemaphore(_NtOpenSemaphore)(
    SemaphoreHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtOpenSymbolicLinkObject
function  NtOpenSymbolicLinkObject(
    SymbolicLinkHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenSymbolicLinkObject, ntdll, 'NtOpenSymbolicLinkObject');
  Result := TFNNtOpenSymbolicLinkObject(_NtOpenSymbolicLinkObject)(
    SymbolicLinkHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtOpenSymbolicLinkObject
function  ZwOpenSymbolicLinkObject(
    SymbolicLinkHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenSymbolicLinkObject, ntdll, 'NtOpenSymbolicLinkObject');
  Result := TFNNtOpenSymbolicLinkObject(_NtOpenSymbolicLinkObject)(
    SymbolicLinkHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtOpenThread
function  NtOpenThread(
    ThreadHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    ClientId : PCLIENT_ID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenThread, ntdll, 'NtOpenThread');
  Result := TFNNtOpenThread(_NtOpenThread)(
    ThreadHandle, DesiredAccess, ObjectAttributes, ClientId
  );
end;

// Dynamic version of NtOpenThread
function  ZwOpenThread(
    ThreadHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    ClientId : PCLIENT_ID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenThread, ntdll, 'NtOpenThread');
  Result := TFNNtOpenThread(_NtOpenThread)(
    ThreadHandle, DesiredAccess, ObjectAttributes, ClientId
  );
end;

// Dynamic version of NtOpenThreadToken
function  NtOpenThreadToken(
    ThreadHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    OpenAsSelf : BOOLEAN;
    TokenHandle : PHANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenThreadToken, ntdll, 'NtOpenThreadToken');
  Result := TFNNtOpenThreadToken(_NtOpenThreadToken)(
    ThreadHandle, DesiredAccess, OpenAsSelf, TokenHandle
  );
end;

// Dynamic version of NtOpenThreadToken
function  ZwOpenThreadToken(
    ThreadHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    OpenAsSelf : BOOLEAN;
    TokenHandle : PHANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenThreadToken, ntdll, 'NtOpenThreadToken');
  Result := TFNNtOpenThreadToken(_NtOpenThreadToken)(
    ThreadHandle, DesiredAccess, OpenAsSelf, TokenHandle
  );
end;

// Dynamic version of NtOpenTimer
function  NtOpenTimer(
    TimerHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenTimer, ntdll, 'NtOpenTimer');
  Result := TFNNtOpenTimer(_NtOpenTimer)(
    TimerHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtOpenTimer
function  ZwOpenTimer(
    TimerHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtOpenTimer, ntdll, 'NtOpenTimer');
  Result := TFNNtOpenTimer(_NtOpenTimer)(
    TimerHandle, DesiredAccess, ObjectAttributes
  );
end;

// Dynamic version of NtPlugPlayControl
function  NtPlugPlayControl(
    ControlCode : ULONG;
    Buffer : PVOID;
    BufferLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtPlugPlayControl, ntdll, 'NtPlugPlayControl');
  Result := TFNNtPlugPlayControl(_NtPlugPlayControl)(
    ControlCode, Buffer, BufferLength
  );
end;

// Dynamic version of NtPlugPlayControl
function  ZwPlugPlayControl(
    ControlCode : ULONG;
    Buffer : PVOID;
    BufferLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtPlugPlayControl, ntdll, 'NtPlugPlayControl');
  Result := TFNNtPlugPlayControl(_NtPlugPlayControl)(
    ControlCode, Buffer, BufferLength
  );
end;

// Dynamic version of NtPowerInformation
function  NtPowerInformation(
    PowerInformationLevel : POWER_INFORMATION_LEVEL;
    InputBuffer : PVOID;
    InputBufferLength : ULONG;
    OutputBuffer : PVOID;
    OutputBufferLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtPowerInformation, ntdll, 'NtPowerInformation');
  Result := TFNNtPowerInformation(_NtPowerInformation)(
    PowerInformationLevel, InputBuffer, InputBufferLength, OutputBuffer, OutputBufferLength
  );
end;

// Dynamic version of NtPowerInformation
function  ZwPowerInformation(
    PowerInformationLevel : POWER_INFORMATION_LEVEL;
    InputBuffer : PVOID;
    InputBufferLength : ULONG;
    OutputBuffer : PVOID;
    OutputBufferLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtPowerInformation, ntdll, 'NtPowerInformation');
  Result := TFNNtPowerInformation(_NtPowerInformation)(
    PowerInformationLevel, InputBuffer, InputBufferLength, OutputBuffer, OutputBufferLength
  );
end;

// Dynamic version of NtPrivilegeCheck
function  NtPrivilegeCheck(
    TokenHandle : HANDLE;
    RequiredPrivileges : PPRIVILEGE_SET;
    Result_ : PBOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtPrivilegeCheck, ntdll, 'NtPrivilegeCheck');
  Result := TFNNtPrivilegeCheck(_NtPrivilegeCheck)(
    TokenHandle, RequiredPrivileges, Result_
  );
end;

// Dynamic version of NtPrivilegeCheck
function  ZwPrivilegeCheck(
    TokenHandle : HANDLE;
    RequiredPrivileges : PPRIVILEGE_SET;
    Result_ : PBOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtPrivilegeCheck, ntdll, 'NtPrivilegeCheck');
  Result := TFNNtPrivilegeCheck(_NtPrivilegeCheck)(
    TokenHandle, RequiredPrivileges, Result_
  );
end;

// Dynamic version of NtPrivilegeObjectAuditAlarm
function  NtPrivilegeObjectAuditAlarm(
    SubsystemName : PUNICODE_STRING;
    HandleId : PVOID;
    TokenHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    Privileges : PPRIVILEGE_SET;
    AccessGranted : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtPrivilegeObjectAuditAlarm, ntdll, 'NtPrivilegeObjectAuditAlarm');
  Result := TFNNtPrivilegeObjectAuditAlarm(_NtPrivilegeObjectAuditAlarm)(
    SubsystemName, HandleId, TokenHandle, DesiredAccess, Privileges, AccessGranted
  );
end;

// Dynamic version of NtPrivilegeObjectAuditAlarm
function  ZwPrivilegeObjectAuditAlarm(
    SubsystemName : PUNICODE_STRING;
    HandleId : PVOID;
    TokenHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    Privileges : PPRIVILEGE_SET;
    AccessGranted : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtPrivilegeObjectAuditAlarm, ntdll, 'NtPrivilegeObjectAuditAlarm');
  Result := TFNNtPrivilegeObjectAuditAlarm(_NtPrivilegeObjectAuditAlarm)(
    SubsystemName, HandleId, TokenHandle, DesiredAccess, Privileges, AccessGranted
  );
end;

// Dynamic version of NtPrivilegedServiceAuditAlarm
function  NtPrivilegedServiceAuditAlarm(
    SubsystemName : PUNICODE_STRING;
    ServiceName : PUNICODE_STRING;
    TokenHandle : HANDLE;
    Privileges : PPRIVILEGE_SET;
    AccessGranted : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtPrivilegedServiceAuditAlarm, ntdll, 'NtPrivilegedServiceAuditAlarm');
  Result := TFNNtPrivilegedServiceAuditAlarm(_NtPrivilegedServiceAuditAlarm)(
    SubsystemName, ServiceName, TokenHandle, Privileges, AccessGranted
  );
end;

// Dynamic version of NtPrivilegedServiceAuditAlarm
function  ZwPrivilegedServiceAuditAlarm(
    SubsystemName : PUNICODE_STRING;
    ServiceName : PUNICODE_STRING;
    TokenHandle : HANDLE;
    Privileges : PPRIVILEGE_SET;
    AccessGranted : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtPrivilegedServiceAuditAlarm, ntdll, 'NtPrivilegedServiceAuditAlarm');
  Result := TFNNtPrivilegedServiceAuditAlarm(_NtPrivilegedServiceAuditAlarm)(
    SubsystemName, ServiceName, TokenHandle, Privileges, AccessGranted
  );
end;

// Dynamic version of NtProtectVirtualMemory
function  NtProtectVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PPVOID;
    ProtectSize : PULONG;
    NewProtect : ULONG;
    OldProtect : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtProtectVirtualMemory, ntdll, 'NtProtectVirtualMemory');
  Result := TFNNtProtectVirtualMemory(_NtProtectVirtualMemory)(
    ProcessHandle, BaseAddress, ProtectSize, NewProtect, OldProtect
  );
end;

// Dynamic version of NtProtectVirtualMemory
function  ZwProtectVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PPVOID;
    ProtectSize : PULONG;
    NewProtect : ULONG;
    OldProtect : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtProtectVirtualMemory, ntdll, 'NtProtectVirtualMemory');
  Result := TFNNtProtectVirtualMemory(_NtProtectVirtualMemory)(
    ProcessHandle, BaseAddress, ProtectSize, NewProtect, OldProtect
  );
end;

// Dynamic version of NtPulseEvent
function  NtPulseEvent(
    EventHandle : HANDLE;
    PreviousState : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtPulseEvent, ntdll, 'NtPulseEvent');
  Result := TFNNtPulseEvent(_NtPulseEvent)(
    EventHandle, PreviousState
  );
end;

// Dynamic version of NtPulseEvent
function  ZwPulseEvent(
    EventHandle : HANDLE;
    PreviousState : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtPulseEvent, ntdll, 'NtPulseEvent');
  Result := TFNNtPulseEvent(_NtPulseEvent)(
    EventHandle, PreviousState
  );
end;

// Dynamic version of NtQueryAttributesFile
function  NtQueryAttributesFile(
    ObjectAttributes : POBJECT_ATTRIBUTES;
    FileInformation : PFILE_BASIC_INFORMATION
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryAttributesFile, ntdll, 'NtQueryAttributesFile');
  Result := TFNNtQueryAttributesFile(_NtQueryAttributesFile)(
    ObjectAttributes, FileInformation
  );
end;

// Dynamic version of NtQueryAttributesFile
function  ZwQueryAttributesFile(
    ObjectAttributes : POBJECT_ATTRIBUTES;
    FileInformation : PFILE_BASIC_INFORMATION
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryAttributesFile, ntdll, 'NtQueryAttributesFile');
  Result := TFNNtQueryAttributesFile(_NtQueryAttributesFile)(
    ObjectAttributes, FileInformation
  );
end;

// Dynamic version of NtQueryDefaultLocale
function  NtQueryDefaultLocale(
    ThreadOrSystem : BOOLEAN;
    Locale : PLCID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryDefaultLocale, ntdll, 'NtQueryDefaultLocale');
  Result := TFNNtQueryDefaultLocale(_NtQueryDefaultLocale)(
    ThreadOrSystem, Locale
  );
end;

// Dynamic version of NtQueryDefaultLocale
function  ZwQueryDefaultLocale(
    ThreadOrSystem : BOOLEAN;
    Locale : PLCID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryDefaultLocale, ntdll, 'NtQueryDefaultLocale');
  Result := TFNNtQueryDefaultLocale(_NtQueryDefaultLocale)(
    ThreadOrSystem, Locale
  );
end;

// Dynamic version of NtQueryDefaultUILanguage
function  NtQueryDefaultUILanguage(
    LanguageId : PLANGID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryDefaultUILanguage, ntdll, 'NtQueryDefaultUILanguage');
  Result := TFNNtQueryDefaultUILanguage(_NtQueryDefaultUILanguage)(
    LanguageId
  );
end;

// Dynamic version of NtQueryDefaultUILanguage
function  ZwQueryDefaultUILanguage(
    LanguageId : PLANGID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryDefaultUILanguage, ntdll, 'NtQueryDefaultUILanguage');
  Result := TFNNtQueryDefaultUILanguage(_NtQueryDefaultUILanguage)(
    LanguageId
  );
end;

// Dynamic version of NtQueryDirectoryFile
function  NtQueryDirectoryFile(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    FileInformation : PVOID;
    FileInformationLength : ULONG;
    FileInformationClass : FILE_INFORMATION_CLASS;
    ReturnSingleEntry : BOOLEAN;
    FileName : PUNICODE_STRING;
    RestartScan : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryDirectoryFile, ntdll, 'NtQueryDirectoryFile');
  Result := TFNNtQueryDirectoryFile(_NtQueryDirectoryFile)(
    FileHandle, Event, ApcRoutine, ApcContext, IoStatusBlock, FileInformation, FileInformationLength, FileInformationClass, ReturnSingleEntry, FileName, RestartScan
  );
end;

// Dynamic version of NtQueryDirectoryFile
function  ZwQueryDirectoryFile(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    FileInformation : PVOID;
    FileInformationLength : ULONG;
    FileInformationClass : FILE_INFORMATION_CLASS;
    ReturnSingleEntry : BOOLEAN;
    FileName : PUNICODE_STRING;
    RestartScan : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryDirectoryFile, ntdll, 'NtQueryDirectoryFile');
  Result := TFNNtQueryDirectoryFile(_NtQueryDirectoryFile)(
    FileHandle, Event, ApcRoutine, ApcContext, IoStatusBlock, FileInformation, FileInformationLength, FileInformationClass, ReturnSingleEntry, FileName, RestartScan
  );
end;

// Dynamic version of NtQueryDirectoryObject
function  NtQueryDirectoryObject(
    DirectoryHandle : HANDLE;
    Buffer : PVOID;
    BufferLength : ULONG;
    ReturnSingleEntry : BOOLEAN;
    RestartScan : BOOLEAN;
    Context : PULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryDirectoryObject, ntdll, 'NtQueryDirectoryObject');
  Result := TFNNtQueryDirectoryObject(_NtQueryDirectoryObject)(
    DirectoryHandle, Buffer, BufferLength, ReturnSingleEntry, RestartScan, Context, ReturnLength
  );
end;

// Dynamic version of NtQueryDirectoryObject
function  ZwQueryDirectoryObject(
    DirectoryHandle : HANDLE;
    Buffer : PVOID;
    BufferLength : ULONG;
    ReturnSingleEntry : BOOLEAN;
    RestartScan : BOOLEAN;
    Context : PULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryDirectoryObject, ntdll, 'NtQueryDirectoryObject');
  Result := TFNNtQueryDirectoryObject(_NtQueryDirectoryObject)(
    DirectoryHandle, Buffer, BufferLength, ReturnSingleEntry, RestartScan, Context, ReturnLength
  );
end;

// Dynamic version of NtQueryEaFile
function  NtQueryEaFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PFILE_FULL_EA_INFORMATION;
    BufferLength : ULONG;
    ReturnSingleEntry : BOOLEAN;
    EaList : PFILE_GET_EA_INFORMATION;
    EaListLength : ULONG;
    EaIndex : PULONG;
    RestartScan : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryEaFile, ntdll, 'NtQueryEaFile');
  Result := TFNNtQueryEaFile(_NtQueryEaFile)(
    FileHandle, IoStatusBlock, Buffer, BufferLength, ReturnSingleEntry, EaList, EaListLength, EaIndex, RestartScan
  );
end;

// Dynamic version of NtQueryEaFile
function  ZwQueryEaFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PFILE_FULL_EA_INFORMATION;
    BufferLength : ULONG;
    ReturnSingleEntry : BOOLEAN;
    EaList : PFILE_GET_EA_INFORMATION;
    EaListLength : ULONG;
    EaIndex : PULONG;
    RestartScan : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryEaFile, ntdll, 'NtQueryEaFile');
  Result := TFNNtQueryEaFile(_NtQueryEaFile)(
    FileHandle, IoStatusBlock, Buffer, BufferLength, ReturnSingleEntry, EaList, EaListLength, EaIndex, RestartScan
  );
end;

// Dynamic version of NtQueryEvent
function  NtQueryEvent(
    EventHandle : HANDLE;
    EventInformationClass : EVENT_INFORMATION_CLASS;
    EventInformation : PVOID;
    EventInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryEvent, ntdll, 'NtQueryEvent');
  Result := TFNNtQueryEvent(_NtQueryEvent)(
    EventHandle, EventInformationClass, EventInformation, EventInformationLength, ResultLength
  );
end;

// Dynamic version of NtQueryEvent
function  ZwQueryEvent(
    EventHandle : HANDLE;
    EventInformationClass : EVENT_INFORMATION_CLASS;
    EventInformation : PVOID;
    EventInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryEvent, ntdll, 'NtQueryEvent');
  Result := TFNNtQueryEvent(_NtQueryEvent)(
    EventHandle, EventInformationClass, EventInformation, EventInformationLength, ResultLength
  );
end;

// Dynamic version of NtQueryFullAttributesFile
function  NtQueryFullAttributesFile(
    ObjectAttributes : POBJECT_ATTRIBUTES;
    FileInformation : PFILE_NETWORK_OPEN_INFORMATION
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryFullAttributesFile, ntdll, 'NtQueryFullAttributesFile');
  Result := TFNNtQueryFullAttributesFile(_NtQueryFullAttributesFile)(
    ObjectAttributes, FileInformation
  );
end;

// Dynamic version of NtQueryFullAttributesFile
function  ZwQueryFullAttributesFile(
    ObjectAttributes : POBJECT_ATTRIBUTES;
    FileInformation : PFILE_NETWORK_OPEN_INFORMATION
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryFullAttributesFile, ntdll, 'NtQueryFullAttributesFile');
  Result := TFNNtQueryFullAttributesFile(_NtQueryFullAttributesFile)(
    ObjectAttributes, FileInformation
  );
end;

// Dynamic version of NtQueryInformationAtom
function  NtQueryInformationAtom(
    Atom : USHORT;
    AtomInformationClass : ATOM_INFORMATION_CLASS;
    AtomInformation : PVOID;
    AtomInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryInformationAtom, ntdll, 'NtQueryInformationAtom');
  Result := TFNNtQueryInformationAtom(_NtQueryInformationAtom)(
    Atom, AtomInformationClass, AtomInformation, AtomInformationLength, ReturnLength
  );
end;

// Dynamic version of NtQueryInformationAtom
function  ZwQueryInformationAtom(
    Atom : USHORT;
    AtomInformationClass : ATOM_INFORMATION_CLASS;
    AtomInformation : PVOID;
    AtomInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryInformationAtom, ntdll, 'NtQueryInformationAtom');
  Result := TFNNtQueryInformationAtom(_NtQueryInformationAtom)(
    Atom, AtomInformationClass, AtomInformation, AtomInformationLength, ReturnLength
  );
end;

// Dynamic version of NtQueryInformationFile
function  NtQueryInformationFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    FileInformation : PVOID;
    FileInformationLength : ULONG;
    FileInformationClass : FILE_INFORMATION_CLASS
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryInformationFile, ntdll, 'NtQueryInformationFile');
  Result := TFNNtQueryInformationFile(_NtQueryInformationFile)(
    FileHandle, IoStatusBlock, FileInformation, FileInformationLength, FileInformationClass
  );
end;

// Dynamic version of NtQueryInformationFile
function  ZwQueryInformationFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    FileInformation : PVOID;
    FileInformationLength : ULONG;
    FileInformationClass : FILE_INFORMATION_CLASS
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryInformationFile, ntdll, 'NtQueryInformationFile');
  Result := TFNNtQueryInformationFile(_NtQueryInformationFile)(
    FileHandle, IoStatusBlock, FileInformation, FileInformationLength, FileInformationClass
  );
end;

// Dynamic version of NtQueryInformationJobObject
function  NtQueryInformationJobObject(
    JobHandle : HANDLE;
    JobInformationClass : JOBOBJECTINFOCLASS;
    JobInformation : PVOID;
    JobInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryInformationJobObject, ntdll, 'NtQueryInformationJobObject');
  Result := TFNNtQueryInformationJobObject(_NtQueryInformationJobObject)(
    JobHandle, JobInformationClass, JobInformation, JobInformationLength, ReturnLength
  );
end;

// Dynamic version of NtQueryInformationJobObject
function  ZwQueryInformationJobObject(
    JobHandle : HANDLE;
    JobInformationClass : JOBOBJECTINFOCLASS;
    JobInformation : PVOID;
    JobInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryInformationJobObject, ntdll, 'NtQueryInformationJobObject');
  Result := TFNNtQueryInformationJobObject(_NtQueryInformationJobObject)(
    JobHandle, JobInformationClass, JobInformation, JobInformationLength, ReturnLength
  );
end;

// Dynamic version of NtQueryInformationPort
function  NtQueryInformationPort(
    PortHandle : HANDLE;
    PortInformationClass : PORT_INFORMATION_CLASS;
    PortInformation : PVOID;
    PortInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryInformationPort, ntdll, 'NtQueryInformationPort');
  Result := TFNNtQueryInformationPort(_NtQueryInformationPort)(
    PortHandle, PortInformationClass, PortInformation, PortInformationLength, ReturnLength
  );
end;

// Dynamic version of NtQueryInformationPort
function  ZwQueryInformationPort(
    PortHandle : HANDLE;
    PortInformationClass : PORT_INFORMATION_CLASS;
    PortInformation : PVOID;
    PortInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryInformationPort, ntdll, 'NtQueryInformationPort');
  Result := TFNNtQueryInformationPort(_NtQueryInformationPort)(
    PortHandle, PortInformationClass, PortInformation, PortInformationLength, ReturnLength
  );
end;

{/$IFNDEF JWA_INCLUDEMODE} // drop JwaWinternal
// Dynamic version of NtQueryInformationProcess
function  NtQueryInformationProcess(
    ProcessHandle : HANDLE;
    ProcessInformationClass : PROCESSINFOCLASS;
    ProcessInformation : PVOID;
    ProcessInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryInformationProcess, ntdll, 'NtQueryInformationProcess');
  Result := TFNNtQueryInformationProcess(_NtQueryInformationProcess)(
    ProcessHandle, ProcessInformationClass, ProcessInformation, ProcessInformationLength, ReturnLength
  );
end;
{.$ENDIF JWA_INCLUDEMODE}

// Dynamic version of NtQueryInformationProcess
function  ZwQueryInformationProcess(
    ProcessHandle : HANDLE;
    ProcessInformationClass : PROCESSINFOCLASS;
    ProcessInformation : PVOID;
    ProcessInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryInformationProcess, ntdll, 'NtQueryInformationProcess');
  Result := TFNNtQueryInformationProcess(_NtQueryInformationProcess)(
    ProcessHandle, ProcessInformationClass, ProcessInformation, ProcessInformationLength, ReturnLength
  );
end;

{.$IFNDEF JWA_INCLUDEMODE} // drop JwaWinternal
// Dynamic version of NtQueryInformationThread
function  NtQueryInformationThread(
    ThreadHandle : HANDLE;
    ThreadInformationClass : THREADINFOCLASS;
    ThreadInformation : PVOID;
    ThreadInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryInformationThread, ntdll, 'NtQueryInformationThread');
  Result := TFNNtQueryInformationThread(_NtQueryInformationThread)(
    ThreadHandle, ThreadInformationClass, ThreadInformation, ThreadInformationLength, ReturnLength
  );
end;
{.$ENDIF JWA_INCLUDEMODE}

// Dynamic version of NtQueryInformationThread
function  ZwQueryInformationThread(
    ThreadHandle : HANDLE;
    ThreadInformationClass : THREADINFOCLASS;
    ThreadInformation : PVOID;
    ThreadInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryInformationThread, ntdll, 'NtQueryInformationThread');
  Result := TFNNtQueryInformationThread(_NtQueryInformationThread)(
    ThreadHandle, ThreadInformationClass, ThreadInformation, ThreadInformationLength, ReturnLength
  );
end;

// Dynamic version of NtQueryInformationToken
function  NtQueryInformationToken(
    TokenHandle : HANDLE;
    TokenInformationClass : TOKEN_INFORMATION_CLASS;
    TokenInformation : PVOID;
    TokenInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryInformationToken, ntdll, 'NtQueryInformationToken');
  Result := TFNNtQueryInformationToken(_NtQueryInformationToken)(
    TokenHandle, TokenInformationClass, TokenInformation, TokenInformationLength, ReturnLength
  );
end;

// Dynamic version of NtQueryInformationToken
function  ZwQueryInformationToken(
    TokenHandle : HANDLE;
    TokenInformationClass : TOKEN_INFORMATION_CLASS;
    TokenInformation : PVOID;
    TokenInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryInformationToken, ntdll, 'NtQueryInformationToken');
  Result := TFNNtQueryInformationToken(_NtQueryInformationToken)(
    TokenHandle, TokenInformationClass, TokenInformation, TokenInformationLength, ReturnLength
  );
end;

// Dynamic version of NtQueryInstallUILanguage
function  NtQueryInstallUILanguage(
    LanguageId : PLANGID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryInstallUILanguage, ntdll, 'NtQueryInstallUILanguage');
  Result := TFNNtQueryInstallUILanguage(_NtQueryInstallUILanguage)(
    LanguageId
  );
end;

// Dynamic version of NtQueryInstallUILanguage
function  ZwQueryInstallUILanguage(
    LanguageId : PLANGID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryInstallUILanguage, ntdll, 'NtQueryInstallUILanguage');
  Result := TFNNtQueryInstallUILanguage(_NtQueryInstallUILanguage)(
    LanguageId
  );
end;

// Dynamic version of NtQueryIntervalProfile
function  NtQueryIntervalProfile(
    Source : KPROFILE_SOURCE;
    Interval : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryIntervalProfile, ntdll, 'NtQueryIntervalProfile');
  Result := TFNNtQueryIntervalProfile(_NtQueryIntervalProfile)(
    Source, Interval
  );
end;

// Dynamic version of NtQueryIntervalProfile
function  ZwQueryIntervalProfile(
    Source : KPROFILE_SOURCE;
    Interval : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryIntervalProfile, ntdll, 'NtQueryIntervalProfile');
  Result := TFNNtQueryIntervalProfile(_NtQueryIntervalProfile)(
    Source, Interval
  );
end;

// Dynamic version of NtQueryIoCompletion
function  NtQueryIoCompletion(
    IoCompletionHandle : HANDLE;
    IoCompletionInformationClass : IO_COMPLETION_INFORMATION_CLASS;
    IoCompletionInformation : PVOID;
    IoCompletionInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryIoCompletion, ntdll, 'NtQueryIoCompletion');
  Result := TFNNtQueryIoCompletion(_NtQueryIoCompletion)(
    IoCompletionHandle, IoCompletionInformationClass, IoCompletionInformation, IoCompletionInformationLength, ResultLength
  );
end;

// Dynamic version of NtQueryIoCompletion
function  ZwQueryIoCompletion(
    IoCompletionHandle : HANDLE;
    IoCompletionInformationClass : IO_COMPLETION_INFORMATION_CLASS;
    IoCompletionInformation : PVOID;
    IoCompletionInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryIoCompletion, ntdll, 'NtQueryIoCompletion');
  Result := TFNNtQueryIoCompletion(_NtQueryIoCompletion)(
    IoCompletionHandle, IoCompletionInformationClass, IoCompletionInformation, IoCompletionInformationLength, ResultLength
  );
end;

// Dynamic version of NtQueryKey
function  NtQueryKey(
    KeyHandle : HANDLE;
    KeyInformationClass : KEY_INFORMATION_CLASS;
    KeyInformation : PVOID;
    KeyInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryKey, ntdll, 'NtQueryKey');
  Result := TFNNtQueryKey(_NtQueryKey)(
    KeyHandle, KeyInformationClass, KeyInformation, KeyInformationLength, ResultLength
  );
end;

// Dynamic version of NtQueryKey
function  ZwQueryKey(
    KeyHandle : HANDLE;
    KeyInformationClass : KEY_INFORMATION_CLASS;
    KeyInformation : PVOID;
    KeyInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryKey, ntdll, 'NtQueryKey');
  Result := TFNNtQueryKey(_NtQueryKey)(
    KeyHandle, KeyInformationClass, KeyInformation, KeyInformationLength, ResultLength
  );
end;

// Dynamic version of NtQueryMultipleValueKey
function  NtQueryMultipleValueKey(
    KeyHandle : HANDLE;
    ValueList : PKEY_VALUE_ENTRY;
    NumberOfValues : ULONG;
    Buffer : PVOID;
    Length : PULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryMultipleValueKey, ntdll, 'NtQueryMultipleValueKey');
  Result := TFNNtQueryMultipleValueKey(_NtQueryMultipleValueKey)(
    KeyHandle, ValueList, NumberOfValues, Buffer, Length, ReturnLength
  );
end;

// Dynamic version of NtQueryMultipleValueKey
function  ZwQueryMultipleValueKey(
    KeyHandle : HANDLE;
    ValueList : PKEY_VALUE_ENTRY;
    NumberOfValues : ULONG;
    Buffer : PVOID;
    Length : PULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryMultipleValueKey, ntdll, 'NtQueryMultipleValueKey');
  Result := TFNNtQueryMultipleValueKey(_NtQueryMultipleValueKey)(
    KeyHandle, ValueList, NumberOfValues, Buffer, Length, ReturnLength
  );
end;

// Dynamic version of NtQueryMutant
function  NtQueryMutant(
    MutantHandle : HANDLE;
    MutantInformationClass : MUTANT_INFORMATION_CLASS;
    MutantInformation : PVOID;
    MutantInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryMutant, ntdll, 'NtQueryMutant');
  Result := TFNNtQueryMutant(_NtQueryMutant)(
    MutantHandle, MutantInformationClass, MutantInformation, MutantInformationLength, ResultLength
  );
end;

// Dynamic version of NtQueryMutant
function  ZwQueryMutant(
    MutantHandle : HANDLE;
    MutantInformationClass : MUTANT_INFORMATION_CLASS;
    MutantInformation : PVOID;
    MutantInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryMutant, ntdll, 'NtQueryMutant');
  Result := TFNNtQueryMutant(_NtQueryMutant)(
    MutantHandle, MutantInformationClass, MutantInformation, MutantInformationLength, ResultLength
  );
end;

// Dynamic version of NtQueryObject
function  NtQueryObject(
    ObjectHandle : HANDLE;
    ObjectInformationClass : OBJECT_INFORMATION_CLASS;
    ObjectInformation : PVOID;
    ObjectInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryObject, ntdll, 'NtQueryObject');
  Result := TFNNtQueryObject(_NtQueryObject)(
    ObjectHandle, ObjectInformationClass, ObjectInformation, ObjectInformationLength, ReturnLength
  );
end;

// Dynamic version of NtQueryObject
function  ZwQueryObject(
    ObjectHandle : HANDLE;
    ObjectInformationClass : OBJECT_INFORMATION_CLASS;
    ObjectInformation : PVOID;
    ObjectInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryObject, ntdll, 'NtQueryObject');
  Result := TFNNtQueryObject(_NtQueryObject)(
    ObjectHandle, ObjectInformationClass, ObjectInformation, ObjectInformationLength, ReturnLength
  );
end;

// Dynamic version of NtQueryOpenSubKeys
function  NtQueryOpenSubKeys(
    KeyObjectAttributes : POBJECT_ATTRIBUTES;
    NumberOfKey : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryOpenSubKeys, ntdll, 'NtQueryOpenSubKeys');
  Result := TFNNtQueryOpenSubKeys(_NtQueryOpenSubKeys)(
    KeyObjectAttributes, NumberOfKey
  );
end;

// Dynamic version of NtQueryOpenSubKeys
function  ZwQueryOpenSubKeys(
    KeyObjectAttributes : POBJECT_ATTRIBUTES;
    NumberOfKey : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryOpenSubKeys, ntdll, 'NtQueryOpenSubKeys');
  Result := TFNNtQueryOpenSubKeys(_NtQueryOpenSubKeys)(
    KeyObjectAttributes, NumberOfKey
  );
end;

// Dynamic version of NtQueryPerformanceCounter
function  NtQueryPerformanceCounter(
    PerformanceCount : PLARGE_INTEGER;
    PerformanceFrequency : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryPerformanceCounter, ntdll, 'NtQueryPerformanceCounter');
  Result := TFNNtQueryPerformanceCounter(_NtQueryPerformanceCounter)(
    PerformanceCount, PerformanceFrequency
  );
end;

// Dynamic version of NtQueryPerformanceCounter
function  ZwQueryPerformanceCounter(
    PerformanceCount : PLARGE_INTEGER;
    PerformanceFrequency : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryPerformanceCounter, ntdll, 'NtQueryPerformanceCounter');
  Result := TFNNtQueryPerformanceCounter(_NtQueryPerformanceCounter)(
    PerformanceCount, PerformanceFrequency
  );
end;

// Dynamic version of NtQueryPortInformationProcess
function  NtQueryPortInformationProcess(): ULONG; stdcall;
begin
  GetProcedureAddress(_NtQueryPortInformationProcess, ntdll, 'NtQueryPortInformationProcess');
  Result := TFNNtQueryPortInformationProcess(_NtQueryPortInformationProcess)();
end;

// Dynamic version of NtQueryPortInformationProcess
function  ZwQueryPortInformationProcess(): ULONG; stdcall;
begin
  GetProcedureAddress(_NtQueryPortInformationProcess, ntdll, 'NtQueryPortInformationProcess');
  Result := TFNNtQueryPortInformationProcess(_NtQueryPortInformationProcess)();
end;

// Dynamic version of NtQueryQuotaInformationFile
function  NtQueryQuotaInformationFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PFILE_USER_QUOTA_INFORMATION;
    BufferLength : ULONG;
    ReturnSingleEntry : BOOLEAN;
    QuotaList : PFILE_QUOTA_LIST_INFORMATION;
    QuotaListLength : ULONG;
    ResumeSid : PSID;
    RestartScan : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryQuotaInformationFile, ntdll, 'NtQueryQuotaInformationFile');
  Result := TFNNtQueryQuotaInformationFile(_NtQueryQuotaInformationFile)(
    FileHandle, IoStatusBlock, Buffer, BufferLength, ReturnSingleEntry, QuotaList, QuotaListLength, ResumeSid, RestartScan
  );
end;

// Dynamic version of NtQueryQuotaInformationFile
function  ZwQueryQuotaInformationFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PFILE_USER_QUOTA_INFORMATION;
    BufferLength : ULONG;
    ReturnSingleEntry : BOOLEAN;
    QuotaList : PFILE_QUOTA_LIST_INFORMATION;
    QuotaListLength : ULONG;
    ResumeSid : PSID;
    RestartScan : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryQuotaInformationFile, ntdll, 'NtQueryQuotaInformationFile');
  Result := TFNNtQueryQuotaInformationFile(_NtQueryQuotaInformationFile)(
    FileHandle, IoStatusBlock, Buffer, BufferLength, ReturnSingleEntry, QuotaList, QuotaListLength, ResumeSid, RestartScan
  );
end;

// Dynamic version of NtQuerySection
function  NtQuerySection(
    SectionHandle : HANDLE;
    SectionInformationClass : SECTION_INFORMATION_CLASS;
    SectionInformation : PVOID;
    SectionInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQuerySection, ntdll, 'NtQuerySection');
  Result := TFNNtQuerySection(_NtQuerySection)(
    SectionHandle, SectionInformationClass, SectionInformation, SectionInformationLength, ResultLength
  );
end;

// Dynamic version of NtQuerySection
function  ZwQuerySection(
    SectionHandle : HANDLE;
    SectionInformationClass : SECTION_INFORMATION_CLASS;
    SectionInformation : PVOID;
    SectionInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQuerySection, ntdll, 'NtQuerySection');
  Result := TFNNtQuerySection(_NtQuerySection)(
    SectionHandle, SectionInformationClass, SectionInformation, SectionInformationLength, ResultLength
  );
end;

// Dynamic version of NtQuerySecurityObject
function  NtQuerySecurityObject(
    Handle : HANDLE;
    RequestedInformation : SECURITY_INFORMATION;
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    SecurityDescriptorLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQuerySecurityObject, ntdll, 'NtQuerySecurityObject');
  Result := TFNNtQuerySecurityObject(_NtQuerySecurityObject)(
    Handle, RequestedInformation, SecurityDescriptor, SecurityDescriptorLength, ReturnLength
  );
end;

// Dynamic version of NtQuerySecurityObject
function  ZwQuerySecurityObject(
    Handle : HANDLE;
    RequestedInformation : SECURITY_INFORMATION;
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    SecurityDescriptorLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQuerySecurityObject, ntdll, 'NtQuerySecurityObject');
  Result := TFNNtQuerySecurityObject(_NtQuerySecurityObject)(
    Handle, RequestedInformation, SecurityDescriptor, SecurityDescriptorLength, ReturnLength
  );
end;

// Dynamic version of NtQuerySemaphore
function  NtQuerySemaphore(
    SemaphoreHandle : HANDLE;
    SemaphoreInformationClass : SEMAPHORE_INFORMATION_CLASS;
    SemaphoreInformation : PVOID;
    SemaphoreInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQuerySemaphore, ntdll, 'NtQuerySemaphore');
  Result := TFNNtQuerySemaphore(_NtQuerySemaphore)(
    SemaphoreHandle, SemaphoreInformationClass, SemaphoreInformation, SemaphoreInformationLength, ResultLength
  );
end;

// Dynamic version of NtQuerySemaphore
function  ZwQuerySemaphore(
    SemaphoreHandle : HANDLE;
    SemaphoreInformationClass : SEMAPHORE_INFORMATION_CLASS;
    SemaphoreInformation : PVOID;
    SemaphoreInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQuerySemaphore, ntdll, 'NtQuerySemaphore');
  Result := TFNNtQuerySemaphore(_NtQuerySemaphore)(
    SemaphoreHandle, SemaphoreInformationClass, SemaphoreInformation, SemaphoreInformationLength, ResultLength
  );
end;

// Dynamic version of NtQuerySymbolicLinkObject
function  NtQuerySymbolicLinkObject(
    SymbolicLinkHandle : HANDLE;
    TargetName : PUNICODE_STRING;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQuerySymbolicLinkObject, ntdll, 'NtQuerySymbolicLinkObject');
  Result := TFNNtQuerySymbolicLinkObject(_NtQuerySymbolicLinkObject)(
    SymbolicLinkHandle, TargetName, ReturnLength
  );
end;

// Dynamic version of NtQuerySymbolicLinkObject
function  ZwQuerySymbolicLinkObject(
    SymbolicLinkHandle : HANDLE;
    TargetName : PUNICODE_STRING;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQuerySymbolicLinkObject, ntdll, 'NtQuerySymbolicLinkObject');
  Result := TFNNtQuerySymbolicLinkObject(_NtQuerySymbolicLinkObject)(
    SymbolicLinkHandle, TargetName, ReturnLength
  );
end;

// Dynamic version of NtQuerySystemEnvironmentValue
function  NtQuerySystemEnvironmentValue(
    Name : PUNICODE_STRING;
    Value : PVOID;
    ValueLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQuerySystemEnvironmentValue, ntdll, 'NtQuerySystemEnvironmentValue');
  Result := TFNNtQuerySystemEnvironmentValue(_NtQuerySystemEnvironmentValue)(
    Name, Value, ValueLength, ReturnLength
  );
end;

// Dynamic version of NtQuerySystemEnvironmentValue
function  ZwQuerySystemEnvironmentValue(
    Name : PUNICODE_STRING;
    Value : PVOID;
    ValueLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQuerySystemEnvironmentValue, ntdll, 'NtQuerySystemEnvironmentValue');
  Result := TFNNtQuerySystemEnvironmentValue(_NtQuerySystemEnvironmentValue)(
    Name, Value, ValueLength, ReturnLength
  );
end;

{.$IFNDEF JWA_INCLUDEMODE} // do include since we droppen JwaWinternl
// Dynamic version of NtQuerySystemInformation
function  NtQuerySystemInformation(
    SystemInformationClass : SYSTEM_INFORMATION_CLASS;
    SystemInformation : PVOID;
    SystemInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQuerySystemInformation, ntdll, 'NtQuerySystemInformation');
  Result := TFNNtQuerySystemInformation(_NtQuerySystemInformation)(
    SystemInformationClass, SystemInformation, SystemInformationLength, ReturnLength
  );
end;
{.$ENDIF JWA_INCLUDEMODE}

// Dynamic version of NtQuerySystemInformation
function  ZwQuerySystemInformation(
    SystemInformationClass : SYSTEM_INFORMATION_CLASS;
    SystemInformation : PVOID;
    SystemInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQuerySystemInformation, ntdll, 'NtQuerySystemInformation');
  Result := TFNNtQuerySystemInformation(_NtQuerySystemInformation)(
    SystemInformationClass, SystemInformation, SystemInformationLength, ReturnLength
  );
end;

{.$IFNDEF JWA_INCLUDEMODE}
// Dynamic version of NtQuerySystemTime
function  NtQuerySystemTime(
    CurrentTime : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQuerySystemTime, ntdll, 'NtQuerySystemTime');
  Result := TFNNtQuerySystemTime(_NtQuerySystemTime)(
    CurrentTime
  );
end;
{.$ENDIF JWA_INCLUDEMODE}

// Dynamic version of NtQuerySystemTime
function  ZwQuerySystemTime(
    CurrentTime : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQuerySystemTime, ntdll, 'NtQuerySystemTime');
  Result := TFNNtQuerySystemTime(_NtQuerySystemTime)(
    CurrentTime
  );
end;

// Dynamic version of NtQueryTimer
function  NtQueryTimer(
    TimerHandle : HANDLE;
    TimerInformationClass : TIMER_INFORMATION_CLASS;
    TimerInformation : PVOID;
    TimerInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryTimer, ntdll, 'NtQueryTimer');
  Result := TFNNtQueryTimer(_NtQueryTimer)(
    TimerHandle, TimerInformationClass, TimerInformation, TimerInformationLength, ResultLength
  );
end;

// Dynamic version of NtQueryTimer
function  ZwQueryTimer(
    TimerHandle : HANDLE;
    TimerInformationClass : TIMER_INFORMATION_CLASS;
    TimerInformation : PVOID;
    TimerInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryTimer, ntdll, 'NtQueryTimer');
  Result := TFNNtQueryTimer(_NtQueryTimer)(
    TimerHandle, TimerInformationClass, TimerInformation, TimerInformationLength, ResultLength
  );
end;

// Dynamic version of NtQueryTimerResolution
function  NtQueryTimerResolution(
    CoarsestResolution : PULONG;
    FinestResolution : PULONG;
    ActualResolution : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryTimerResolution, ntdll, 'NtQueryTimerResolution');
  Result := TFNNtQueryTimerResolution(_NtQueryTimerResolution)(
    CoarsestResolution, FinestResolution, ActualResolution
  );
end;

// Dynamic version of NtQueryTimerResolution
function  ZwQueryTimerResolution(
    CoarsestResolution : PULONG;
    FinestResolution : PULONG;
    ActualResolution : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryTimerResolution, ntdll, 'NtQueryTimerResolution');
  Result := TFNNtQueryTimerResolution(_NtQueryTimerResolution)(
    CoarsestResolution, FinestResolution, ActualResolution
  );
end;

// Dynamic version of NtQueryValueKey
function  NtQueryValueKey(
    KeyHandle : HANDLE;
    ValueName : PUNICODE_STRING;
    KeyValueInformationClass : KEY_VALUE_INFORMATION_CLASS;
    KeyValueInformation : PVOID;
    KeyValueInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryValueKey, ntdll, 'NtQueryValueKey');
  Result := TFNNtQueryValueKey(_NtQueryValueKey)(
    KeyHandle, ValueName, KeyValueInformationClass, KeyValueInformation, KeyValueInformationLength, ResultLength
  );
end;

// Dynamic version of NtQueryValueKey
function  ZwQueryValueKey(
    KeyHandle : HANDLE;
    ValueName : PUNICODE_STRING;
    KeyValueInformationClass : KEY_VALUE_INFORMATION_CLASS;
    KeyValueInformation : PVOID;
    KeyValueInformationLength : ULONG;
    ResultLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryValueKey, ntdll, 'NtQueryValueKey');
  Result := TFNNtQueryValueKey(_NtQueryValueKey)(
    KeyHandle, ValueName, KeyValueInformationClass, KeyValueInformation, KeyValueInformationLength, ResultLength
  );
end;

// Dynamic version of NtQueryVirtualMemory
function  NtQueryVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PVOID;
    MemoryInformationClass : MEMORY_INFORMATION_CLASS;
    MemoryInformation : PVOID;
    MemoryInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryVirtualMemory, ntdll, 'NtQueryVirtualMemory');
  Result := TFNNtQueryVirtualMemory(_NtQueryVirtualMemory)(
    ProcessHandle, BaseAddress, MemoryInformationClass, MemoryInformation, MemoryInformationLength, ReturnLength
  );
end;

// Dynamic version of NtQueryVirtualMemory
function  ZwQueryVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PVOID;
    MemoryInformationClass : MEMORY_INFORMATION_CLASS;
    MemoryInformation : PVOID;
    MemoryInformationLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryVirtualMemory, ntdll, 'NtQueryVirtualMemory');
  Result := TFNNtQueryVirtualMemory(_NtQueryVirtualMemory)(
    ProcessHandle, BaseAddress, MemoryInformationClass, MemoryInformation, MemoryInformationLength, ReturnLength
  );
end;

// Dynamic version of NtQueryVolumeInformationFile
function  NtQueryVolumeInformationFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    VolumeInformation : PVOID;
    VolumeInformationLength : ULONG;
    VolumeInformationClass : FS_INFORMATION_CLASS
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryVolumeInformationFile, ntdll, 'NtQueryVolumeInformationFile');
  Result := TFNNtQueryVolumeInformationFile(_NtQueryVolumeInformationFile)(
    FileHandle, IoStatusBlock, VolumeInformation, VolumeInformationLength, VolumeInformationClass
  );
end;

// Dynamic version of NtQueryVolumeInformationFile
function  ZwQueryVolumeInformationFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    VolumeInformation : PVOID;
    VolumeInformationLength : ULONG;
    VolumeInformationClass : FS_INFORMATION_CLASS
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueryVolumeInformationFile, ntdll, 'NtQueryVolumeInformationFile');
  Result := TFNNtQueryVolumeInformationFile(_NtQueryVolumeInformationFile)(
    FileHandle, IoStatusBlock, VolumeInformation, VolumeInformationLength, VolumeInformationClass
  );
end;

// Dynamic version of NtQueueApcThread
function  NtQueueApcThread(
    ThreadHandle : HANDLE;
    ApcRoutine : PKNORMAL_ROUTINE;
    ApcContext : PVOID;
    Argument1 : PVOID;
    Argument2 : PVOID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueueApcThread, ntdll, 'NtQueueApcThread');
  Result := TFNNtQueueApcThread(_NtQueueApcThread)(
    ThreadHandle, ApcRoutine, ApcContext, Argument1, Argument2
  );
end;

// Dynamic version of NtQueueApcThread
function  ZwQueueApcThread(
    ThreadHandle : HANDLE;
    ApcRoutine : PKNORMAL_ROUTINE;
    ApcContext : PVOID;
    Argument1 : PVOID;
    Argument2 : PVOID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtQueueApcThread, ntdll, 'NtQueueApcThread');
  Result := TFNNtQueueApcThread(_NtQueueApcThread)(
    ThreadHandle, ApcRoutine, ApcContext, Argument1, Argument2
  );
end;

// Dynamic version of NtRaiseException
function  NtRaiseException(
    ExceptionRecord : PEXCEPTION_RECORD;
    Context : PCONTEXT;
    SearchFrames : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtRaiseException, ntdll, 'NtRaiseException');
  Result := TFNNtRaiseException(_NtRaiseException)(
    ExceptionRecord, Context, SearchFrames
  );
end;

// Dynamic version of NtRaiseException
function  ZwRaiseException(
    ExceptionRecord : PEXCEPTION_RECORD;
    Context : PCONTEXT;
    SearchFrames : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtRaiseException, ntdll, 'NtRaiseException');
  Result := TFNNtRaiseException(_NtRaiseException)(
    ExceptionRecord, Context, SearchFrames
  );
end;

// Dynamic version of NtRaiseHardError
function  NtRaiseHardError(
    Status : NTSTATUS;
    NumberOfArguments : ULONG;
    StringArgumentsMask : ULONG;
    Arguments : PULONG;
    MessageBoxType : ULONG;
    MessageBoxResult : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtRaiseHardError, ntdll, 'NtRaiseHardError');
  Result := TFNNtRaiseHardError(_NtRaiseHardError)(
    Status, NumberOfArguments, StringArgumentsMask, Arguments, MessageBoxType, MessageBoxResult
  );
end;

// Dynamic version of NtRaiseHardError
function  ZwRaiseHardError(
    Status : NTSTATUS;
    NumberOfArguments : ULONG;
    StringArgumentsMask : ULONG;
    Arguments : PULONG;
    MessageBoxType : ULONG;
    MessageBoxResult : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtRaiseHardError, ntdll, 'NtRaiseHardError');
  Result := TFNNtRaiseHardError(_NtRaiseHardError)(
    Status, NumberOfArguments, StringArgumentsMask, Arguments, MessageBoxType, MessageBoxResult
  );
end;

// Dynamic version of NtReadFile
function  NtReadFile(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PVOID;
    Length : ULONG;
    ByteOffset : PLARGE_INTEGER;
    Key : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtReadFile, ntdll, 'NtReadFile');
  Result := TFNNtReadFile(_NtReadFile)(
    FileHandle, Event, ApcRoutine, ApcContext, IoStatusBlock, Buffer, Length, ByteOffset, Key
  );
end;

// Dynamic version of NtReadFile
function  ZwReadFile(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PVOID;
    Length : ULONG;
    ByteOffset : PLARGE_INTEGER;
    Key : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtReadFile, ntdll, 'NtReadFile');
  Result := TFNNtReadFile(_NtReadFile)(
    FileHandle, Event, ApcRoutine, ApcContext, IoStatusBlock, Buffer, Length, ByteOffset, Key
  );
end;

// Dynamic version of NtReadFileScatter
function  NtReadFileScatter(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PFILE_SEGMENT_ELEMENT;
    Length : ULONG;
    ByteOffset : PLARGE_INTEGER;
    Key : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtReadFileScatter, ntdll, 'NtReadFileScatter');
  Result := TFNNtReadFileScatter(_NtReadFileScatter)(
    FileHandle, Event, ApcRoutine, ApcContext, IoStatusBlock, Buffer, Length, ByteOffset, Key
  );
end;

// Dynamic version of NtReadFileScatter
function  ZwReadFileScatter(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PFILE_SEGMENT_ELEMENT;
    Length : ULONG;
    ByteOffset : PLARGE_INTEGER;
    Key : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtReadFileScatter, ntdll, 'NtReadFileScatter');
  Result := TFNNtReadFileScatter(_NtReadFileScatter)(
    FileHandle, Event, ApcRoutine, ApcContext, IoStatusBlock, Buffer, Length, ByteOffset, Key
  );
end;

// Dynamic version of NtReadRequestData
function  NtReadRequestData(
    PortHandle : HANDLE;
    Message : PPORT_MESSAGE;
    Index : ULONG;
    Buffer : PVOID;
    BufferLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtReadRequestData, ntdll, 'NtReadRequestData');
  Result := TFNNtReadRequestData(_NtReadRequestData)(
    PortHandle, Message, Index, Buffer, BufferLength, ReturnLength
  );
end;

// Dynamic version of NtReadRequestData
function  ZwReadRequestData(
    PortHandle : HANDLE;
    Message : PPORT_MESSAGE;
    Index : ULONG;
    Buffer : PVOID;
    BufferLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtReadRequestData, ntdll, 'NtReadRequestData');
  Result := TFNNtReadRequestData(_NtReadRequestData)(
    PortHandle, Message, Index, Buffer, BufferLength, ReturnLength
  );
end;

// Dynamic version of NtReadVirtualMemory
function  NtReadVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PVOID;
    Buffer : PVOID;
    BufferLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtReadVirtualMemory, ntdll, 'NtReadVirtualMemory');
  Result := TFNNtReadVirtualMemory(_NtReadVirtualMemory)(
    ProcessHandle, BaseAddress, Buffer, BufferLength, ReturnLength
  );
end;

// Dynamic version of NtReadVirtualMemory
function  ZwReadVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PVOID;
    Buffer : PVOID;
    BufferLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtReadVirtualMemory, ntdll, 'NtReadVirtualMemory');
  Result := TFNNtReadVirtualMemory(_NtReadVirtualMemory)(
    ProcessHandle, BaseAddress, Buffer, BufferLength, ReturnLength
  );
end;

// Dynamic version of NtRegisterThreadTerminatePort
function  NtRegisterThreadTerminatePort(
    PortHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtRegisterThreadTerminatePort, ntdll, 'NtRegisterThreadTerminatePort');
  Result := TFNNtRegisterThreadTerminatePort(_NtRegisterThreadTerminatePort)(
    PortHandle
  );
end;

// Dynamic version of NtRegisterThreadTerminatePort
function  ZwRegisterThreadTerminatePort(
    PortHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtRegisterThreadTerminatePort, ntdll, 'NtRegisterThreadTerminatePort');
  Result := TFNNtRegisterThreadTerminatePort(_NtRegisterThreadTerminatePort)(
    PortHandle
  );
end;

// Dynamic version of NtReleaseMutant
function  NtReleaseMutant(
    MutantHandle : HANDLE;
    PreviousState : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtReleaseMutant, ntdll, 'NtReleaseMutant');
  Result := TFNNtReleaseMutant(_NtReleaseMutant)(
    MutantHandle, PreviousState
  );
end;

// Dynamic version of NtReleaseMutant
function  ZwReleaseMutant(
    MutantHandle : HANDLE;
    PreviousState : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtReleaseMutant, ntdll, 'NtReleaseMutant');
  Result := TFNNtReleaseMutant(_NtReleaseMutant)(
    MutantHandle, PreviousState
  );
end;

// Dynamic version of NtReleaseSemaphore
function  NtReleaseSemaphore(
    SemaphoreHandle : HANDLE;
    ReleaseCount : LONG;
    PreviousCount : PLONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtReleaseSemaphore, ntdll, 'NtReleaseSemaphore');
  Result := TFNNtReleaseSemaphore(_NtReleaseSemaphore)(
    SemaphoreHandle, ReleaseCount, PreviousCount
  );
end;

// Dynamic version of NtReleaseSemaphore
function  ZwReleaseSemaphore(
    SemaphoreHandle : HANDLE;
    ReleaseCount : LONG;
    PreviousCount : PLONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtReleaseSemaphore, ntdll, 'NtReleaseSemaphore');
  Result := TFNNtReleaseSemaphore(_NtReleaseSemaphore)(
    SemaphoreHandle, ReleaseCount, PreviousCount
  );
end;

// Dynamic version of NtRemoveIoCompletion
function  NtRemoveIoCompletion(
    IoCompletionHandle : HANDLE;
    CompletionKey : PULONG;
    CompletionValue : PULONG;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Timeout : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtRemoveIoCompletion, ntdll, 'NtRemoveIoCompletion');
  Result := TFNNtRemoveIoCompletion(_NtRemoveIoCompletion)(
    IoCompletionHandle, CompletionKey, CompletionValue, IoStatusBlock, Timeout
  );
end;

// Dynamic version of NtRemoveIoCompletion
function  ZwRemoveIoCompletion(
    IoCompletionHandle : HANDLE;
    CompletionKey : PULONG;
    CompletionValue : PULONG;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Timeout : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtRemoveIoCompletion, ntdll, 'NtRemoveIoCompletion');
  Result := TFNNtRemoveIoCompletion(_NtRemoveIoCompletion)(
    IoCompletionHandle, CompletionKey, CompletionValue, IoStatusBlock, Timeout
  );
end;

// Dynamic version of NtRemoveProcessDebug
function  NtRemoveProcessDebug(
    hProcess : HANDLE;
    hDebugObject : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtRemoveProcessDebug, ntdll, 'NtRemoveProcessDebug');
  Result := TFNNtRemoveProcessDebug(_NtRemoveProcessDebug)(
    hProcess, hDebugObject
  );
end;

// Dynamic version of NtRemoveProcessDebug
function  ZwRemoveProcessDebug(
    hProcess : HANDLE;
    hDebugObject : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtRemoveProcessDebug, ntdll, 'NtRemoveProcessDebug');
  Result := TFNNtRemoveProcessDebug(_NtRemoveProcessDebug)(
    hProcess, hDebugObject
  );
end;

// Dynamic version of NtReplaceKey
function  NtReplaceKey(
    NewFileObjectAttributes : POBJECT_ATTRIBUTES;
    KeyHandle : HANDLE;
    OldFileObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtReplaceKey, ntdll, 'NtReplaceKey');
  Result := TFNNtReplaceKey(_NtReplaceKey)(
    NewFileObjectAttributes, KeyHandle, OldFileObjectAttributes
  );
end;

// Dynamic version of NtReplaceKey
function  ZwReplaceKey(
    NewFileObjectAttributes : POBJECT_ATTRIBUTES;
    KeyHandle : HANDLE;
    OldFileObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtReplaceKey, ntdll, 'NtReplaceKey');
  Result := TFNNtReplaceKey(_NtReplaceKey)(
    NewFileObjectAttributes, KeyHandle, OldFileObjectAttributes
  );
end;

// Dynamic version of NtReplyPort
function  NtReplyPort(
    PortHandle : HANDLE;
    ReplyMessage : PPORT_MESSAGE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtReplyPort, ntdll, 'NtReplyPort');
  Result := TFNNtReplyPort(_NtReplyPort)(
    PortHandle, ReplyMessage
  );
end;

// Dynamic version of NtReplyPort
function  ZwReplyPort(
    PortHandle : HANDLE;
    ReplyMessage : PPORT_MESSAGE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtReplyPort, ntdll, 'NtReplyPort');
  Result := TFNNtReplyPort(_NtReplyPort)(
    PortHandle, ReplyMessage
  );
end;

// Dynamic version of NtReplyWaitReceivePort
function  NtReplyWaitReceivePort(
    PortHandle : HANDLE;
    PortIdentifier : PULONG;
    ReplyMessage : PPORT_MESSAGE;
    Message : PPORT_MESSAGE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtReplyWaitReceivePort, ntdll, 'NtReplyWaitReceivePort');
  Result := TFNNtReplyWaitReceivePort(_NtReplyWaitReceivePort)(
    PortHandle, PortIdentifier, ReplyMessage, Message
  );
end;

// Dynamic version of NtReplyWaitReceivePort
function  ZwReplyWaitReceivePort(
    PortHandle : HANDLE;
    PortIdentifier : PULONG;
    ReplyMessage : PPORT_MESSAGE;
    Message : PPORT_MESSAGE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtReplyWaitReceivePort, ntdll, 'NtReplyWaitReceivePort');
  Result := TFNNtReplyWaitReceivePort(_NtReplyWaitReceivePort)(
    PortHandle, PortIdentifier, ReplyMessage, Message
  );
end;

// Dynamic version of NtReplyWaitReceivePortEx
function  NtReplyWaitReceivePortEx(
    PortHandle : HANDLE;
    PortIdentifier : PULONG;
    ReplyMessage : PPORT_MESSAGE;
    Message : PPORT_MESSAGE;
    Timeout : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtReplyWaitReceivePortEx, ntdll, 'NtReplyWaitReceivePortEx');
  Result := TFNNtReplyWaitReceivePortEx(_NtReplyWaitReceivePortEx)(
    PortHandle, PortIdentifier, ReplyMessage, Message, Timeout
  );
end;

// Dynamic version of NtReplyWaitReceivePortEx
function  ZwReplyWaitReceivePortEx(
    PortHandle : HANDLE;
    PortIdentifier : PULONG;
    ReplyMessage : PPORT_MESSAGE;
    Message : PPORT_MESSAGE;
    Timeout : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtReplyWaitReceivePortEx, ntdll, 'NtReplyWaitReceivePortEx');
  Result := TFNNtReplyWaitReceivePortEx(_NtReplyWaitReceivePortEx)(
    PortHandle, PortIdentifier, ReplyMessage, Message, Timeout
  );
end;

// Dynamic version of NtReplyWaitReplyPort
function  NtReplyWaitReplyPort(
    PortHandle : HANDLE;
    ReplyMessage : PPORT_MESSAGE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtReplyWaitReplyPort, ntdll, 'NtReplyWaitReplyPort');
  Result := TFNNtReplyWaitReplyPort(_NtReplyWaitReplyPort)(
    PortHandle, ReplyMessage
  );
end;

// Dynamic version of NtReplyWaitReplyPort
function  ZwReplyWaitReplyPort(
    PortHandle : HANDLE;
    ReplyMessage : PPORT_MESSAGE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtReplyWaitReplyPort, ntdll, 'NtReplyWaitReplyPort');
  Result := TFNNtReplyWaitReplyPort(_NtReplyWaitReplyPort)(
    PortHandle, ReplyMessage
  );
end;

// Dynamic version of NtReplyWaitSendChannel
function  NtReplyWaitSendChannel(
    x : PVOID;
    y : PVOID;
    z : PVOID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtReplyWaitSendChannel, ntdll, 'NtReplyWaitSendChannel');
  Result := TFNNtReplyWaitSendChannel(_NtReplyWaitSendChannel)(
    x, y, z
  );
end;

// Dynamic version of NtReplyWaitSendChannel
function  ZwReplyWaitSendChannel(
    x : PVOID;
    y : PVOID;
    z : PVOID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtReplyWaitSendChannel, ntdll, 'NtReplyWaitSendChannel');
  Result := TFNNtReplyWaitSendChannel(_NtReplyWaitSendChannel)(
    x, y, z
  );
end;

// Dynamic version of NtRequestDeviceWakeup
function  NtRequestDeviceWakeup(
    DeviceHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtRequestDeviceWakeup, ntdll, 'NtRequestDeviceWakeup');
  Result := TFNNtRequestDeviceWakeup(_NtRequestDeviceWakeup)(
    DeviceHandle
  );
end;

// Dynamic version of NtRequestDeviceWakeup
function  ZwRequestDeviceWakeup(
    DeviceHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtRequestDeviceWakeup, ntdll, 'NtRequestDeviceWakeup');
  Result := TFNNtRequestDeviceWakeup(_NtRequestDeviceWakeup)(
    DeviceHandle
  );
end;

// Dynamic version of NtRequestPort
function  NtRequestPort(
    PortHandle : HANDLE;
    RequestMessage : PPORT_MESSAGE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtRequestPort, ntdll, 'NtRequestPort');
  Result := TFNNtRequestPort(_NtRequestPort)(
    PortHandle, RequestMessage
  );
end;

// Dynamic version of NtRequestPort
function  ZwRequestPort(
    PortHandle : HANDLE;
    RequestMessage : PPORT_MESSAGE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtRequestPort, ntdll, 'NtRequestPort');
  Result := TFNNtRequestPort(_NtRequestPort)(
    PortHandle, RequestMessage
  );
end;

// Dynamic version of NtRequestWaitReplyPort
function  NtRequestWaitReplyPort(
    PortHandle : HANDLE;
    RequestMessage : PPORT_MESSAGE;
    ReplyMessage : PPORT_MESSAGE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtRequestWaitReplyPort, ntdll, 'NtRequestWaitReplyPort');
  Result := TFNNtRequestWaitReplyPort(_NtRequestWaitReplyPort)(
    PortHandle, RequestMessage, ReplyMessage
  );
end;

// Dynamic version of NtRequestWaitReplyPort
function  ZwRequestWaitReplyPort(
    PortHandle : HANDLE;
    RequestMessage : PPORT_MESSAGE;
    ReplyMessage : PPORT_MESSAGE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtRequestWaitReplyPort, ntdll, 'NtRequestWaitReplyPort');
  Result := TFNNtRequestWaitReplyPort(_NtRequestWaitReplyPort)(
    PortHandle, RequestMessage, ReplyMessage
  );
end;

// Dynamic version of NtRequestWakeupLatency
function  NtRequestWakeupLatency(
    Latency : LATENCY_TIME
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtRequestWakeupLatency, ntdll, 'NtRequestWakeupLatency');
  Result := TFNNtRequestWakeupLatency(_NtRequestWakeupLatency)(
    Latency
  );
end;

// Dynamic version of NtRequestWakeupLatency
function  ZwRequestWakeupLatency(
    Latency : LATENCY_TIME
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtRequestWakeupLatency, ntdll, 'NtRequestWakeupLatency');
  Result := TFNNtRequestWakeupLatency(_NtRequestWakeupLatency)(
    Latency
  );
end;

// Dynamic version of NtResetEvent
function  NtResetEvent(
    EventHandle : HANDLE;
    PreviousState : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtResetEvent, ntdll, 'NtResetEvent');
  Result := TFNNtResetEvent(_NtResetEvent)(
    EventHandle, PreviousState
  );
end;

// Dynamic version of NtResetEvent
function  ZwResetEvent(
    EventHandle : HANDLE;
    PreviousState : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtResetEvent, ntdll, 'NtResetEvent');
  Result := TFNNtResetEvent(_NtResetEvent)(
    EventHandle, PreviousState
  );
end;

// Dynamic version of NtResetWriteWatch
function  NtResetWriteWatch(
    ProcessHandle : HANDLE;
    BaseAddress : PVOID;
    RegionSize : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtResetWriteWatch, ntdll, 'NtResetWriteWatch');
  Result := TFNNtResetWriteWatch(_NtResetWriteWatch)(
    ProcessHandle, BaseAddress, RegionSize
  );
end;

// Dynamic version of NtResetWriteWatch
function  ZwResetWriteWatch(
    ProcessHandle : HANDLE;
    BaseAddress : PVOID;
    RegionSize : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtResetWriteWatch, ntdll, 'NtResetWriteWatch');
  Result := TFNNtResetWriteWatch(_NtResetWriteWatch)(
    ProcessHandle, BaseAddress, RegionSize
  );
end;

// Dynamic version of NtRestoreKey
function  NtRestoreKey(
    KeyHandle : HANDLE;
    FileHandle : HANDLE;
    Flags : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtRestoreKey, ntdll, 'NtRestoreKey');
  Result := TFNNtRestoreKey(_NtRestoreKey)(
    KeyHandle, FileHandle, Flags
  );
end;

// Dynamic version of NtRestoreKey
function  ZwRestoreKey(
    KeyHandle : HANDLE;
    FileHandle : HANDLE;
    Flags : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtRestoreKey, ntdll, 'NtRestoreKey');
  Result := TFNNtRestoreKey(_NtRestoreKey)(
    KeyHandle, FileHandle, Flags
  );
end;

// Dynamic version of NtResumeProcess
function  NtResumeProcess(
    hProcess : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtResumeProcess, ntdll, 'NtResumeProcess');
  Result := TFNNtResumeProcess(_NtResumeProcess)(
    hProcess
  );
end;

// Dynamic version of NtResumeProcess
function  ZwResumeProcess(
    hProcess : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtResumeProcess, ntdll, 'NtResumeProcess');
  Result := TFNNtResumeProcess(_NtResumeProcess)(
    hProcess
  );
end;

// Dynamic version of NtResumeThread
function  NtResumeThread(
    hThread : HANDLE;
    dwResumeCount : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtResumeThread, ntdll, 'NtResumeThread');
  Result := TFNNtResumeThread(_NtResumeThread)(
    hThread, dwResumeCount
  );
end;

// Dynamic version of NtResumeThread
function  ZwResumeThread(
    hThread : HANDLE;
    dwResumeCount : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtResumeThread, ntdll, 'NtResumeThread');
  Result := TFNNtResumeThread(_NtResumeThread)(
    hThread, dwResumeCount
  );
end;

// Dynamic version of NtSaveKey
function  NtSaveKey(
    KeyHandle : HANDLE;
    FileHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSaveKey, ntdll, 'NtSaveKey');
  Result := TFNNtSaveKey(_NtSaveKey)(
    KeyHandle, FileHandle
  );
end;

// Dynamic version of NtSaveKey
function  ZwSaveKey(
    KeyHandle : HANDLE;
    FileHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSaveKey, ntdll, 'NtSaveKey');
  Result := TFNNtSaveKey(_NtSaveKey)(
    KeyHandle, FileHandle
  );
end;

// Dynamic version of NtSaveKeyEx
function  NtSaveKeyEx(
    KeyHandle : HANDLE;
    FileHandle : HANDLE;
    Flags : DWORD
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSaveKeyEx, ntdll, 'NtSaveKeyEx');
  Result := TFNNtSaveKeyEx(_NtSaveKeyEx)(
    KeyHandle, FileHandle, Flags
  );
end;

// Dynamic version of NtSaveKeyEx
function  ZwSaveKeyEx(
    KeyHandle : HANDLE;
    FileHandle : HANDLE;
    Flags : DWORD
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSaveKeyEx, ntdll, 'NtSaveKeyEx');
  Result := TFNNtSaveKeyEx(_NtSaveKeyEx)(
    KeyHandle, FileHandle, Flags
  );
end;

// Dynamic version of NtSaveMergedKeys
function  NtSaveMergedKeys(
    KeyHandle1 : HANDLE;
    KeyHandle2 : HANDLE;
    FileHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSaveMergedKeys, ntdll, 'NtSaveMergedKeys');
  Result := TFNNtSaveMergedKeys(_NtSaveMergedKeys)(
    KeyHandle1, KeyHandle2, FileHandle
  );
end;

// Dynamic version of NtSaveMergedKeys
function  ZwSaveMergedKeys(
    KeyHandle1 : HANDLE;
    KeyHandle2 : HANDLE;
    FileHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSaveMergedKeys, ntdll, 'NtSaveMergedKeys');
  Result := TFNNtSaveMergedKeys(_NtSaveMergedKeys)(
    KeyHandle1, KeyHandle2, FileHandle
  );
end;

// Dynamic version of NtSecureConnectPort
function  NtSecureConnectPort(
    PortHandle : PHANDLE;
    PortName : PUNICODE_STRING;
    SecurityQos : PSECURITY_QUALITY_OF_SERVICE;
    WriteSection : PPORT_SECTION_WRITE;
    ServerSid : PSID;
    ReadSection : PPORT_SECTION_READ;
    MaxMessageSize : PULONG;
    ConnectData : PVOID;
    ConnectDataLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSecureConnectPort, ntdll, 'NtSecureConnectPort');
  Result := TFNNtSecureConnectPort(_NtSecureConnectPort)(
    PortHandle, PortName, SecurityQos, WriteSection, ServerSid, ReadSection, MaxMessageSize, ConnectData, ConnectDataLength
  );
end;

// Dynamic version of NtSecureConnectPort
function  ZwSecureConnectPort(
    PortHandle : PHANDLE;
    PortName : PUNICODE_STRING;
    SecurityQos : PSECURITY_QUALITY_OF_SERVICE;
    WriteSection : PPORT_SECTION_WRITE;
    ServerSid : PSID;
    ReadSection : PPORT_SECTION_READ;
    MaxMessageSize : PULONG;
    ConnectData : PVOID;
    ConnectDataLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSecureConnectPort, ntdll, 'NtSecureConnectPort');
  Result := TFNNtSecureConnectPort(_NtSecureConnectPort)(
    PortHandle, PortName, SecurityQos, WriteSection, ServerSid, ReadSection, MaxMessageSize, ConnectData, ConnectDataLength
  );
end;

// Dynamic version of NtSendWaitReplyChannel
function  NtSendWaitReplyChannel(
    x : PVOID;
    y : PVOID;
    z : PVOID;
    z2 : PVOID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSendWaitReplyChannel, ntdll, 'NtSendWaitReplyChannel');
  Result := TFNNtSendWaitReplyChannel(_NtSendWaitReplyChannel)(
    x, y, z, z2
  );
end;

// Dynamic version of NtSendWaitReplyChannel
function  ZwSendWaitReplyChannel(
    x : PVOID;
    y : PVOID;
    z : PVOID;
    z2 : PVOID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSendWaitReplyChannel, ntdll, 'NtSendWaitReplyChannel');
  Result := TFNNtSendWaitReplyChannel(_NtSendWaitReplyChannel)(
    x, y, z, z2
  );
end;

// Dynamic version of NtSetContextChannel
function  NtSetContextChannel(
    x : PVOID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetContextChannel, ntdll, 'NtSetContextChannel');
  Result := TFNNtSetContextChannel(_NtSetContextChannel)(
    x
  );
end;

// Dynamic version of NtSetContextChannel
function  ZwSetContextChannel(
    x : PVOID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetContextChannel, ntdll, 'NtSetContextChannel');
  Result := TFNNtSetContextChannel(_NtSetContextChannel)(
    x
  );
end;

// Dynamic version of NtSetContextThread
function  NtSetContextThread(
    ThreadHandle : HANDLE;
    Context : PCONTEXT
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetContextThread, ntdll, 'NtSetContextThread');
  Result := TFNNtSetContextThread(_NtSetContextThread)(
    ThreadHandle, Context
  );
end;

// Dynamic version of NtSetContextThread
function  ZwSetContextThread(
    ThreadHandle : HANDLE;
    Context : PCONTEXT
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetContextThread, ntdll, 'NtSetContextThread');
  Result := TFNNtSetContextThread(_NtSetContextThread)(
    ThreadHandle, Context
  );
end;

// Dynamic version of NtSetDefaultHardErrorPort
function  NtSetDefaultHardErrorPort(
    PortHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetDefaultHardErrorPort, ntdll, 'NtSetDefaultHardErrorPort');
  Result := TFNNtSetDefaultHardErrorPort(_NtSetDefaultHardErrorPort)(
    PortHandle
  );
end;

// Dynamic version of NtSetDefaultHardErrorPort
function  ZwSetDefaultHardErrorPort(
    PortHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetDefaultHardErrorPort, ntdll, 'NtSetDefaultHardErrorPort');
  Result := TFNNtSetDefaultHardErrorPort(_NtSetDefaultHardErrorPort)(
    PortHandle
  );
end;

// Dynamic version of NtSetDefaultLocale
function  NtSetDefaultLocale(
    ThreadOrSystem : BOOLEAN;
    Locale : LCID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetDefaultLocale, ntdll, 'NtSetDefaultLocale');
  Result := TFNNtSetDefaultLocale(_NtSetDefaultLocale)(
    ThreadOrSystem, Locale
  );
end;

// Dynamic version of NtSetDefaultLocale
function  ZwSetDefaultLocale(
    ThreadOrSystem : BOOLEAN;
    Locale : LCID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetDefaultLocale, ntdll, 'NtSetDefaultLocale');
  Result := TFNNtSetDefaultLocale(_NtSetDefaultLocale)(
    ThreadOrSystem, Locale
  );
end;

// Dynamic version of NtSetDefaultUILanguage
function  NtSetDefaultUILanguage(
    LanguageId : LANGID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetDefaultUILanguage, ntdll, 'NtSetDefaultUILanguage');
  Result := TFNNtSetDefaultUILanguage(_NtSetDefaultUILanguage)(
    LanguageId
  );
end;

// Dynamic version of NtSetDefaultUILanguage
function  ZwSetDefaultUILanguage(
    LanguageId : LANGID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetDefaultUILanguage, ntdll, 'NtSetDefaultUILanguage');
  Result := TFNNtSetDefaultUILanguage(_NtSetDefaultUILanguage)(
    LanguageId
  );
end;

// Dynamic version of NtSetEaFile
function  NtSetEaFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PFILE_FULL_EA_INFORMATION;
    BufferLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetEaFile, ntdll, 'NtSetEaFile');
  Result := TFNNtSetEaFile(_NtSetEaFile)(
    FileHandle, IoStatusBlock, Buffer, BufferLength
  );
end;

// Dynamic version of NtSetEaFile
function  ZwSetEaFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PFILE_FULL_EA_INFORMATION;
    BufferLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetEaFile, ntdll, 'NtSetEaFile');
  Result := TFNNtSetEaFile(_NtSetEaFile)(
    FileHandle, IoStatusBlock, Buffer, BufferLength
  );
end;

// Dynamic version of NtSetEvent
function  NtSetEvent(
    EventHandle : HANDLE;
    PreviousState : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetEvent, ntdll, 'NtSetEvent');
  Result := TFNNtSetEvent(_NtSetEvent)(
    EventHandle, PreviousState
  );
end;

// Dynamic version of NtSetEvent
function  ZwSetEvent(
    EventHandle : HANDLE;
    PreviousState : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetEvent, ntdll, 'NtSetEvent');
  Result := TFNNtSetEvent(_NtSetEvent)(
    EventHandle, PreviousState
  );
end;

// Dynamic version of NtSetHighEventPair
function  NtSetHighEventPair(
    EventPairHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetHighEventPair, ntdll, 'NtSetHighEventPair');
  Result := TFNNtSetHighEventPair(_NtSetHighEventPair)(
    EventPairHandle
  );
end;

// Dynamic version of NtSetHighEventPair
function  ZwSetHighEventPair(
    EventPairHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetHighEventPair, ntdll, 'NtSetHighEventPair');
  Result := TFNNtSetHighEventPair(_NtSetHighEventPair)(
    EventPairHandle
  );
end;

// Dynamic version of NtSetHighWaitLowEventPair
function  NtSetHighWaitLowEventPair(
    EventPairHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetHighWaitLowEventPair, ntdll, 'NtSetHighWaitLowEventPair');
  Result := TFNNtSetHighWaitLowEventPair(_NtSetHighWaitLowEventPair)(
    EventPairHandle
  );
end;

// Dynamic version of NtSetHighWaitLowEventPair
function  ZwSetHighWaitLowEventPair(
    EventPairHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetHighWaitLowEventPair, ntdll, 'NtSetHighWaitLowEventPair');
  Result := TFNNtSetHighWaitLowEventPair(_NtSetHighWaitLowEventPair)(
    EventPairHandle
  );
end;

// Dynamic version of NtSetHighWaitLowThread
function  NtSetHighWaitLowThread(): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetHighWaitLowThread, ntdll, 'NtSetHighWaitLowThread');
  Result := TFNNtSetHighWaitLowThread(_NtSetHighWaitLowThread)();
end;

// Dynamic version of NtSetHighWaitLowThread
function  ZwSetHighWaitLowThread(): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetHighWaitLowThread, ntdll, 'NtSetHighWaitLowThread');
  Result := TFNNtSetHighWaitLowThread(_NtSetHighWaitLowThread)();
end;

// Dynamic version of NtSetInformationFile
function  NtSetInformationFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    FileInformation : PVOID;
    FileInformationLength : ULONG;
    FileInformationClass : FILE_INFORMATION_CLASS
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetInformationFile, ntdll, 'NtSetInformationFile');
  Result := TFNNtSetInformationFile(_NtSetInformationFile)(
    FileHandle, IoStatusBlock, FileInformation, FileInformationLength, FileInformationClass
  );
end;

// Dynamic version of NtSetInformationFile
function  ZwSetInformationFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    FileInformation : PVOID;
    FileInformationLength : ULONG;
    FileInformationClass : FILE_INFORMATION_CLASS
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetInformationFile, ntdll, 'NtSetInformationFile');
  Result := TFNNtSetInformationFile(_NtSetInformationFile)(
    FileHandle, IoStatusBlock, FileInformation, FileInformationLength, FileInformationClass
  );
end;

// Dynamic version of NtSetInformationJobObject
function  NtSetInformationJobObject(
    JobHandle : HANDLE;
    JobInformationClass : JOBOBJECTINFOCLASS;
    JobInformation : PVOID;
    JobInformationLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetInformationJobObject, ntdll, 'NtSetInformationJobObject');
  Result := TFNNtSetInformationJobObject(_NtSetInformationJobObject)(
    JobHandle, JobInformationClass, JobInformation, JobInformationLength
  );
end;

// Dynamic version of NtSetInformationJobObject
function  ZwSetInformationJobObject(
    JobHandle : HANDLE;
    JobInformationClass : JOBOBJECTINFOCLASS;
    JobInformation : PVOID;
    JobInformationLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetInformationJobObject, ntdll, 'NtSetInformationJobObject');
  Result := TFNNtSetInformationJobObject(_NtSetInformationJobObject)(
    JobHandle, JobInformationClass, JobInformation, JobInformationLength
  );
end;

// Dynamic version of NtSetInformationKey
function  NtSetInformationKey(
    KeyHandle : HANDLE;
    KeyInformationClass : KEY_SET_INFORMATION_CLASS;
    KeyInformation : PVOID;
    KeyInformationLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetInformationKey, ntdll, 'NtSetInformationKey');
  Result := TFNNtSetInformationKey(_NtSetInformationKey)(
    KeyHandle, KeyInformationClass, KeyInformation, KeyInformationLength
  );
end;

// Dynamic version of NtSetInformationKey
function  ZwSetInformationKey(
    KeyHandle : HANDLE;
    KeyInformationClass : KEY_SET_INFORMATION_CLASS;
    KeyInformation : PVOID;
    KeyInformationLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetInformationKey, ntdll, 'NtSetInformationKey');
  Result := TFNNtSetInformationKey(_NtSetInformationKey)(
    KeyHandle, KeyInformationClass, KeyInformation, KeyInformationLength
  );
end;

// Dynamic version of NtSetInformationObject
function  NtSetInformationObject(
    ObjectHandle : HANDLE;
    ObjectInformationClass : OBJECT_INFORMATION_CLASS;
    ObjectInformation : PVOID;
    ObjectInformationLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetInformationObject, ntdll, 'NtSetInformationObject');
  Result := TFNNtSetInformationObject(_NtSetInformationObject)(
    ObjectHandle, ObjectInformationClass, ObjectInformation, ObjectInformationLength
  );
end;

// Dynamic version of NtSetInformationObject
function  ZwSetInformationObject(
    ObjectHandle : HANDLE;
    ObjectInformationClass : OBJECT_INFORMATION_CLASS;
    ObjectInformation : PVOID;
    ObjectInformationLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetInformationObject, ntdll, 'NtSetInformationObject');
  Result := TFNNtSetInformationObject(_NtSetInformationObject)(
    ObjectHandle, ObjectInformationClass, ObjectInformation, ObjectInformationLength
  );
end;

// Dynamic version of NtSetInformationProcess
function  NtSetInformationProcess(
    ProcessHandle : HANDLE;
    ProcessInformationClass : PROCESSINFOCLASS;
    ProcessInformation : PVOID;
    ProcessInformationLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetInformationProcess, ntdll, 'NtSetInformationProcess');
  Result := TFNNtSetInformationProcess(_NtSetInformationProcess)(
    ProcessHandle, ProcessInformationClass, ProcessInformation, ProcessInformationLength
  );
end;

// Dynamic version of NtSetInformationProcess
function  ZwSetInformationProcess(
    ProcessHandle : HANDLE;
    ProcessInformationClass : PROCESSINFOCLASS;
    ProcessInformation : PVOID;
    ProcessInformationLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetInformationProcess, ntdll, 'NtSetInformationProcess');
  Result := TFNNtSetInformationProcess(_NtSetInformationProcess)(
    ProcessHandle, ProcessInformationClass, ProcessInformation, ProcessInformationLength
  );
end;

// Dynamic version of NtSetInformationThread
function  NtSetInformationThread(
    ThreadHandle : HANDLE;
    ThreadInformationClass : THREADINFOCLASS;
    ThreadInformation : PVOID;
    ThreadInformationLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetInformationThread, ntdll, 'NtSetInformationThread');
  Result := TFNNtSetInformationThread(_NtSetInformationThread)(
    ThreadHandle, ThreadInformationClass, ThreadInformation, ThreadInformationLength
  );
end;

// Dynamic version of NtSetInformationThread
function  ZwSetInformationThread(
    ThreadHandle : HANDLE;
    ThreadInformationClass : THREADINFOCLASS;
    ThreadInformation : PVOID;
    ThreadInformationLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetInformationThread, ntdll, 'NtSetInformationThread');
  Result := TFNNtSetInformationThread(_NtSetInformationThread)(
    ThreadHandle, ThreadInformationClass, ThreadInformation, ThreadInformationLength
  );
end;

// Dynamic version of NtSetInformationToken
function  NtSetInformationToken(
    TokenHandle : HANDLE;
    TokenInformationClass : TOKEN_INFORMATION_CLASS;
    TokenInformation : PVOID;
    TokenInformationLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetInformationToken, ntdll, 'NtSetInformationToken');
  Result := TFNNtSetInformationToken(_NtSetInformationToken)(
    TokenHandle, TokenInformationClass, TokenInformation, TokenInformationLength
  );
end;

// Dynamic version of NtSetInformationToken
function  ZwSetInformationToken(
    TokenHandle : HANDLE;
    TokenInformationClass : TOKEN_INFORMATION_CLASS;
    TokenInformation : PVOID;
    TokenInformationLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetInformationToken, ntdll, 'NtSetInformationToken');
  Result := TFNNtSetInformationToken(_NtSetInformationToken)(
    TokenHandle, TokenInformationClass, TokenInformation, TokenInformationLength
  );
end;

// Dynamic version of NtSetIntervalProfile
function  NtSetIntervalProfile(
    Interval : ULONG;
    Source : KPROFILE_SOURCE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetIntervalProfile, ntdll, 'NtSetIntervalProfile');
  Result := TFNNtSetIntervalProfile(_NtSetIntervalProfile)(
    Interval, Source
  );
end;

// Dynamic version of NtSetIntervalProfile
function  ZwSetIntervalProfile(
    Interval : ULONG;
    Source : KPROFILE_SOURCE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetIntervalProfile, ntdll, 'NtSetIntervalProfile');
  Result := TFNNtSetIntervalProfile(_NtSetIntervalProfile)(
    Interval, Source
  );
end;

// Dynamic version of NtSetIoCompletion
function  NtSetIoCompletion(
    IoCompletionHandle : HANDLE;
    CompletionKey : ULONG;
    CompletionValue : ULONG;
    Status : NTSTATUS;
    Information : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetIoCompletion, ntdll, 'NtSetIoCompletion');
  Result := TFNNtSetIoCompletion(_NtSetIoCompletion)(
    IoCompletionHandle, CompletionKey, CompletionValue, Status, Information
  );
end;

// Dynamic version of NtSetIoCompletion
function  ZwSetIoCompletion(
    IoCompletionHandle : HANDLE;
    CompletionKey : ULONG;
    CompletionValue : ULONG;
    Status : NTSTATUS;
    Information : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetIoCompletion, ntdll, 'NtSetIoCompletion');
  Result := TFNNtSetIoCompletion(_NtSetIoCompletion)(
    IoCompletionHandle, CompletionKey, CompletionValue, Status, Information
  );
end;

// Dynamic version of NtSetLdtEntries
function  NtSetLdtEntries(
    Selector1 : ULONG;
    LdtEntry1 : LDT_ENTRY;
    Selector2 : ULONG;
    LdtEntry2 : LDT_ENTRY
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetLdtEntries, ntdll, 'NtSetLdtEntries');
  Result := TFNNtSetLdtEntries(_NtSetLdtEntries)(
    Selector1, LdtEntry1, Selector2, LdtEntry2
  );
end;

// Dynamic version of NtSetLdtEntries
function  ZwSetLdtEntries(
    Selector1 : ULONG;
    LdtEntry1 : LDT_ENTRY;
    Selector2 : ULONG;
    LdtEntry2 : LDT_ENTRY
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetLdtEntries, ntdll, 'NtSetLdtEntries');
  Result := TFNNtSetLdtEntries(_NtSetLdtEntries)(
    Selector1, LdtEntry1, Selector2, LdtEntry2
  );
end;

// Dynamic version of NtSetLowEventPair
function  NtSetLowEventPair(
    EventPairHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetLowEventPair, ntdll, 'NtSetLowEventPair');
  Result := TFNNtSetLowEventPair(_NtSetLowEventPair)(
    EventPairHandle
  );
end;

// Dynamic version of NtSetLowEventPair
function  ZwSetLowEventPair(
    EventPairHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetLowEventPair, ntdll, 'NtSetLowEventPair');
  Result := TFNNtSetLowEventPair(_NtSetLowEventPair)(
    EventPairHandle
  );
end;

// Dynamic version of NtSetLowWaitHighEventPair
function  NtSetLowWaitHighEventPair(
    EventPairHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetLowWaitHighEventPair, ntdll, 'NtSetLowWaitHighEventPair');
  Result := TFNNtSetLowWaitHighEventPair(_NtSetLowWaitHighEventPair)(
    EventPairHandle
  );
end;

// Dynamic version of NtSetLowWaitHighEventPair
function  ZwSetLowWaitHighEventPair(
    EventPairHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetLowWaitHighEventPair, ntdll, 'NtSetLowWaitHighEventPair');
  Result := TFNNtSetLowWaitHighEventPair(_NtSetLowWaitHighEventPair)(
    EventPairHandle
  );
end;

// Dynamic version of NtSetLowWaitHighThread
function  NtSetLowWaitHighThread(): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetLowWaitHighThread, ntdll, 'NtSetLowWaitHighThread');
  Result := TFNNtSetLowWaitHighThread(_NtSetLowWaitHighThread)();
end;

// Dynamic version of NtSetLowWaitHighThread
function  ZwSetLowWaitHighThread(): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetLowWaitHighThread, ntdll, 'NtSetLowWaitHighThread');
  Result := TFNNtSetLowWaitHighThread(_NtSetLowWaitHighThread)();
end;

// Dynamic version of NtSetQuotaInformationFile
function  NtSetQuotaInformationFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PFILE_USER_QUOTA_INFORMATION;
    BufferLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetQuotaInformationFile, ntdll, 'NtSetQuotaInformationFile');
  Result := TFNNtSetQuotaInformationFile(_NtSetQuotaInformationFile)(
    FileHandle, IoStatusBlock, Buffer, BufferLength
  );
end;

// Dynamic version of NtSetQuotaInformationFile
function  ZwSetQuotaInformationFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PFILE_USER_QUOTA_INFORMATION;
    BufferLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetQuotaInformationFile, ntdll, 'NtSetQuotaInformationFile');
  Result := TFNNtSetQuotaInformationFile(_NtSetQuotaInformationFile)(
    FileHandle, IoStatusBlock, Buffer, BufferLength
  );
end;

// Dynamic version of NtSetSecurityObject
function  NtSetSecurityObject(
    Handle : HANDLE;
    SecurityInformation : SECURITY_INFORMATION;
    SecurityDescriptor : PSECURITY_DESCRIPTOR
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetSecurityObject, ntdll, 'NtSetSecurityObject');
  Result := TFNNtSetSecurityObject(_NtSetSecurityObject)(
    Handle, SecurityInformation, SecurityDescriptor
  );
end;

// Dynamic version of NtSetSecurityObject
function  ZwSetSecurityObject(
    Handle : HANDLE;
    SecurityInformation : SECURITY_INFORMATION;
    SecurityDescriptor : PSECURITY_DESCRIPTOR
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetSecurityObject, ntdll, 'NtSetSecurityObject');
  Result := TFNNtSetSecurityObject(_NtSetSecurityObject)(
    Handle, SecurityInformation, SecurityDescriptor
  );
end;

// Dynamic version of NtSetSystemEnvironmentValue
function  NtSetSystemEnvironmentValue(
    Name : PUNICODE_STRING;
    Value : PUNICODE_STRING
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetSystemEnvironmentValue, ntdll, 'NtSetSystemEnvironmentValue');
  Result := TFNNtSetSystemEnvironmentValue(_NtSetSystemEnvironmentValue)(
    Name, Value
  );
end;

// Dynamic version of NtSetSystemEnvironmentValue
function  ZwSetSystemEnvironmentValue(
    Name : PUNICODE_STRING;
    Value : PUNICODE_STRING
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetSystemEnvironmentValue, ntdll, 'NtSetSystemEnvironmentValue');
  Result := TFNNtSetSystemEnvironmentValue(_NtSetSystemEnvironmentValue)(
    Name, Value
  );
end;

// Dynamic version of NtSetSystemInformation
function  NtSetSystemInformation(
    SystemInformationClass : SYSTEM_INFORMATION_CLASS;
    SystemInformation : PVOID;
    SystemInformationLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetSystemInformation, ntdll, 'NtSetSystemInformation');
  Result := TFNNtSetSystemInformation(_NtSetSystemInformation)(
    SystemInformationClass, SystemInformation, SystemInformationLength
  );
end;

// Dynamic version of NtSetSystemInformation
function  ZwSetSystemInformation(
    SystemInformationClass : SYSTEM_INFORMATION_CLASS;
    SystemInformation : PVOID;
    SystemInformationLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetSystemInformation, ntdll, 'NtSetSystemInformation');
  Result := TFNNtSetSystemInformation(_NtSetSystemInformation)(
    SystemInformationClass, SystemInformation, SystemInformationLength
  );
end;

// Dynamic version of NtSetSystemPowerState
function  NtSetSystemPowerState(
    SystemAction : POWER_ACTION;
    MinSystemState : SYSTEM_POWER_STATE;
    Flags : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetSystemPowerState, ntdll, 'NtSetSystemPowerState');
  Result := TFNNtSetSystemPowerState(_NtSetSystemPowerState)(
    SystemAction, MinSystemState, Flags
  );
end;

// Dynamic version of NtSetSystemPowerState
function  ZwSetSystemPowerState(
    SystemAction : POWER_ACTION;
    MinSystemState : SYSTEM_POWER_STATE;
    Flags : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetSystemPowerState, ntdll, 'NtSetSystemPowerState');
  Result := TFNNtSetSystemPowerState(_NtSetSystemPowerState)(
    SystemAction, MinSystemState, Flags
  );
end;

// Dynamic version of NtSetSystemTime
function  NtSetSystemTime(
    NewTime : PLARGE_INTEGER;
    OldTime : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetSystemTime, ntdll, 'NtSetSystemTime');
  Result := TFNNtSetSystemTime(_NtSetSystemTime)(
    NewTime, OldTime
  );
end;

// Dynamic version of NtSetSystemTime
function  ZwSetSystemTime(
    NewTime : PLARGE_INTEGER;
    OldTime : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetSystemTime, ntdll, 'NtSetSystemTime');
  Result := TFNNtSetSystemTime(_NtSetSystemTime)(
    NewTime, OldTime
  );
end;

// Dynamic version of NtSetThreadExecutionState
function  NtSetThreadExecutionState(
    ExecutionState : EXECUTION_STATE;
    PreviousExecutionState : PEXECUTION_STATE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetThreadExecutionState, ntdll, 'NtSetThreadExecutionState');
  Result := TFNNtSetThreadExecutionState(_NtSetThreadExecutionState)(
    ExecutionState, PreviousExecutionState
  );
end;

// Dynamic version of NtSetThreadExecutionState
function  ZwSetThreadExecutionState(
    ExecutionState : EXECUTION_STATE;
    PreviousExecutionState : PEXECUTION_STATE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetThreadExecutionState, ntdll, 'NtSetThreadExecutionState');
  Result := TFNNtSetThreadExecutionState(_NtSetThreadExecutionState)(
    ExecutionState, PreviousExecutionState
  );
end;

// Dynamic version of NtSetTimer
function  NtSetTimer(
    TimerHandle : HANDLE;
    DueTime : PLARGE_INTEGER;
    TimerApcRoutine : PTIMER_APC_ROUTINE;
    TimerContext : PVOID;
    Resume : BOOLEAN;
    Period : LONG;
    PreviousState : PBOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetTimer, ntdll, 'NtSetTimer');
  Result := TFNNtSetTimer(_NtSetTimer)(
    TimerHandle, DueTime, TimerApcRoutine, TimerContext, Resume, Period, PreviousState
  );
end;

// Dynamic version of NtSetTimer
function  ZwSetTimer(
    TimerHandle : HANDLE;
    DueTime : PLARGE_INTEGER;
    TimerApcRoutine : PTIMER_APC_ROUTINE;
    TimerContext : PVOID;
    Resume : BOOLEAN;
    Period : LONG;
    PreviousState : PBOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetTimer, ntdll, 'NtSetTimer');
  Result := TFNNtSetTimer(_NtSetTimer)(
    TimerHandle, DueTime, TimerApcRoutine, TimerContext, Resume, Period, PreviousState
  );
end;

// Dynamic version of NtSetTimerResolution
function  NtSetTimerResolution(
    RequestedResolution : ULONG;
    Set_ : BOOLEAN;
    ActualResolution : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetTimerResolution, ntdll, 'NtSetTimerResolution');
  Result := TFNNtSetTimerResolution(_NtSetTimerResolution)(
    RequestedResolution, Set_, ActualResolution
  );
end;

// Dynamic version of NtSetTimerResolution
function  ZwSetTimerResolution(
    RequestedResolution : ULONG;
    Set_ : BOOLEAN;
    ActualResolution : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetTimerResolution, ntdll, 'NtSetTimerResolution');
  Result := TFNNtSetTimerResolution(_NtSetTimerResolution)(
    RequestedResolution, Set_, ActualResolution
  );
end;

// Dynamic version of NtSetUuidSeed
function  NtSetUuidSeed(
    UuidSeed : PUCHAR
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetUuidSeed, ntdll, 'NtSetUuidSeed');
  Result := TFNNtSetUuidSeed(_NtSetUuidSeed)(
    UuidSeed
  );
end;

// Dynamic version of NtSetUuidSeed
function  ZwSetUuidSeed(
    UuidSeed : PUCHAR
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetUuidSeed, ntdll, 'NtSetUuidSeed');
  Result := TFNNtSetUuidSeed(_NtSetUuidSeed)(
    UuidSeed
  );
end;

// Dynamic version of NtSetValueKey
function  NtSetValueKey(
    KeyHandle : HANDLE;
    ValueName : PUNICODE_STRING;
    TitleIndex : ULONG;
    Type_ : ULONG;
    Data : PVOID;
    DataSize : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetValueKey, ntdll, 'NtSetValueKey');
  Result := TFNNtSetValueKey(_NtSetValueKey)(
    KeyHandle, ValueName, TitleIndex, Type_, Data, DataSize
  );
end;

// Dynamic version of NtSetValueKey
function  ZwSetValueKey(
    KeyHandle : HANDLE;
    ValueName : PUNICODE_STRING;
    TitleIndex : ULONG;
    Type_ : ULONG;
    Data : PVOID;
    DataSize : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetValueKey, ntdll, 'NtSetValueKey');
  Result := TFNNtSetValueKey(_NtSetValueKey)(
    KeyHandle, ValueName, TitleIndex, Type_, Data, DataSize
  );
end;

// Dynamic version of NtSetVolumeInformationFile
function  NtSetVolumeInformationFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PVOID;
    BufferLength : ULONG;
    VolumeInformationClass : FS_INFORMATION_CLASS
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetVolumeInformationFile, ntdll, 'NtSetVolumeInformationFile');
  Result := TFNNtSetVolumeInformationFile(_NtSetVolumeInformationFile)(
    FileHandle, IoStatusBlock, Buffer, BufferLength, VolumeInformationClass
  );
end;

// Dynamic version of NtSetVolumeInformationFile
function  ZwSetVolumeInformationFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PVOID;
    BufferLength : ULONG;
    VolumeInformationClass : FS_INFORMATION_CLASS
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSetVolumeInformationFile, ntdll, 'NtSetVolumeInformationFile');
  Result := TFNNtSetVolumeInformationFile(_NtSetVolumeInformationFile)(
    FileHandle, IoStatusBlock, Buffer, BufferLength, VolumeInformationClass
  );
end;

// Dynamic version of NtShutdownSystem
function  NtShutdownSystem(
    Action : SHUTDOWN_ACTION
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtShutdownSystem, ntdll, 'NtShutdownSystem');
  Result := TFNNtShutdownSystem(_NtShutdownSystem)(
    Action
  );
end;

// Dynamic version of NtShutdownSystem
function  ZwShutdownSystem(
    Action : SHUTDOWN_ACTION
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtShutdownSystem, ntdll, 'NtShutdownSystem');
  Result := TFNNtShutdownSystem(_NtShutdownSystem)(
    Action
  );
end;

// Dynamic version of NtSignalAndWaitForSingleObject
function  NtSignalAndWaitForSingleObject(
    HandleToSignal : HANDLE;
    HandleToWait : HANDLE;
    Alertable : BOOLEAN;
    Timeout : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSignalAndWaitForSingleObject, ntdll, 'NtSignalAndWaitForSingleObject');
  Result := TFNNtSignalAndWaitForSingleObject(_NtSignalAndWaitForSingleObject)(
    HandleToSignal, HandleToWait, Alertable, Timeout
  );
end;

// Dynamic version of NtSignalAndWaitForSingleObject
function  ZwSignalAndWaitForSingleObject(
    HandleToSignal : HANDLE;
    HandleToWait : HANDLE;
    Alertable : BOOLEAN;
    Timeout : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSignalAndWaitForSingleObject, ntdll, 'NtSignalAndWaitForSingleObject');
  Result := TFNNtSignalAndWaitForSingleObject(_NtSignalAndWaitForSingleObject)(
    HandleToSignal, HandleToWait, Alertable, Timeout
  );
end;

// Dynamic version of NtStartProfile
function  NtStartProfile(
    ProfileHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtStartProfile, ntdll, 'NtStartProfile');
  Result := TFNNtStartProfile(_NtStartProfile)(
    ProfileHandle
  );
end;

// Dynamic version of NtStartProfile
function  ZwStartProfile(
    ProfileHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtStartProfile, ntdll, 'NtStartProfile');
  Result := TFNNtStartProfile(_NtStartProfile)(
    ProfileHandle
  );
end;

// Dynamic version of NtStopProfile
function  NtStopProfile(
    ProfileHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtStopProfile, ntdll, 'NtStopProfile');
  Result := TFNNtStopProfile(_NtStopProfile)(
    ProfileHandle
  );
end;

// Dynamic version of NtStopProfile
function  ZwStopProfile(
    ProfileHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtStopProfile, ntdll, 'NtStopProfile');
  Result := TFNNtStopProfile(_NtStopProfile)(
    ProfileHandle
  );
end;

// Dynamic version of NtSuspendProcess
function  NtSuspendProcess(
    hProcess : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSuspendProcess, ntdll, 'NtSuspendProcess');
  Result := TFNNtSuspendProcess(_NtSuspendProcess)(
    hProcess
  );
end;

// Dynamic version of NtSuspendProcess
function  ZwSuspendProcess(
    hProcess : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSuspendProcess, ntdll, 'NtSuspendProcess');
  Result := TFNNtSuspendProcess(_NtSuspendProcess)(
    hProcess
  );
end;

// Dynamic version of NtSuspendThread
function  NtSuspendThread(
    hThread : HANDLE;
    dwLastResumeCount : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSuspendThread, ntdll, 'NtSuspendThread');
  Result := TFNNtSuspendThread(_NtSuspendThread)(
    hThread, dwLastResumeCount
  );
end;

// Dynamic version of NtSuspendThread
function  ZwSuspendThread(
    hThread : HANDLE;
    dwLastResumeCount : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSuspendThread, ntdll, 'NtSuspendThread');
  Result := TFNNtSuspendThread(_NtSuspendThread)(
    hThread, dwLastResumeCount
  );
end;

// Dynamic version of NtSystemDebugControl
function  NtSystemDebugControl(
    ControlCode : DEBUG_CONTROL_CODE;
    InputBuffer : PVOID;
    InputBufferLength : ULONG;
    OutputBuffer : PVOID;
    OutputBufferLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSystemDebugControl, ntdll, 'NtSystemDebugControl');
  Result := TFNNtSystemDebugControl(_NtSystemDebugControl)(
    ControlCode, InputBuffer, InputBufferLength, OutputBuffer, OutputBufferLength, ReturnLength
  );
end;

// Dynamic version of NtSystemDebugControl
function  ZwSystemDebugControl(
    ControlCode : DEBUG_CONTROL_CODE;
    InputBuffer : PVOID;
    InputBufferLength : ULONG;
    OutputBuffer : PVOID;
    OutputBufferLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtSystemDebugControl, ntdll, 'NtSystemDebugControl');
  Result := TFNNtSystemDebugControl(_NtSystemDebugControl)(
    ControlCode, InputBuffer, InputBufferLength, OutputBuffer, OutputBufferLength, ReturnLength
  );
end;

// Dynamic version of NtTerminateJobObject
function  NtTerminateJobObject(
    JobHandle : HANDLE;
    ExitStatus : NTSTATUS
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtTerminateJobObject, ntdll, 'NtTerminateJobObject');
  Result := TFNNtTerminateJobObject(_NtTerminateJobObject)(
    JobHandle, ExitStatus
  );
end;

// Dynamic version of NtTerminateJobObject
function  ZwTerminateJobObject(
    JobHandle : HANDLE;
    ExitStatus : NTSTATUS
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtTerminateJobObject, ntdll, 'NtTerminateJobObject');
  Result := TFNNtTerminateJobObject(_NtTerminateJobObject)(
    JobHandle, ExitStatus
  );
end;

// Dynamic version of NtTerminateProcess
function  NtTerminateProcess(
    ProcessHandle : HANDLE;
    ExitStatus : NTSTATUS
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtTerminateProcess, ntdll, 'NtTerminateProcess');
  Result := TFNNtTerminateProcess(_NtTerminateProcess)(
    ProcessHandle, ExitStatus
  );
end;

// Dynamic version of NtTerminateProcess
function  ZwTerminateProcess(
    ProcessHandle : HANDLE;
    ExitStatus : NTSTATUS
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtTerminateProcess, ntdll, 'NtTerminateProcess');
  Result := TFNNtTerminateProcess(_NtTerminateProcess)(
    ProcessHandle, ExitStatus
  );
end;

// Dynamic version of NtTerminateThread
function  NtTerminateThread(
    ThreadHandle : HANDLE;
    ExitStatus : NTSTATUS
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtTerminateThread, ntdll, 'NtTerminateThread');
  Result := TFNNtTerminateThread(_NtTerminateThread)(
    ThreadHandle, ExitStatus
  );
end;

// Dynamic version of NtTerminateThread
function  ZwTerminateThread(
    ThreadHandle : HANDLE;
    ExitStatus : NTSTATUS
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtTerminateThread, ntdll, 'NtTerminateThread');
  Result := TFNNtTerminateThread(_NtTerminateThread)(
    ThreadHandle, ExitStatus
  );
end;

// Dynamic version of NtTestAlert
function  NtTestAlert(): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtTestAlert, ntdll, 'NtTestAlert');
  Result := TFNNtTestAlert(_NtTestAlert)();
end;

// Dynamic version of NtTestAlert
function  ZwTestAlert(): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtTestAlert, ntdll, 'NtTestAlert');
  Result := TFNNtTestAlert(_NtTestAlert)();
end;

// Dynamic version of NtUnloadDriver
function  NtUnloadDriver(
    DriverServiceName : PUNICODE_STRING
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtUnloadDriver, ntdll, 'NtUnloadDriver');
  Result := TFNNtUnloadDriver(_NtUnloadDriver)(
    DriverServiceName
  );
end;

// Dynamic version of NtUnloadDriver
function  ZwUnloadDriver(
    DriverServiceName : PUNICODE_STRING
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtUnloadDriver, ntdll, 'NtUnloadDriver');
  Result := TFNNtUnloadDriver(_NtUnloadDriver)(
    DriverServiceName
  );
end;

// Dynamic version of NtUnloadKey
function  NtUnloadKey(
    KeyObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtUnloadKey, ntdll, 'NtUnloadKey');
  Result := TFNNtUnloadKey(_NtUnloadKey)(
    KeyObjectAttributes
  );
end;

// Dynamic version of NtUnloadKey
function  ZwUnloadKey(
    KeyObjectAttributes : POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtUnloadKey, ntdll, 'NtUnloadKey');
  Result := TFNNtUnloadKey(_NtUnloadKey)(
    KeyObjectAttributes
  );
end;

// Dynamic version of NtUnlockFile
function  NtUnlockFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    LockOffset : PULARGE_INTEGER;
    LockLength : PULARGE_INTEGER;
    Key : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtUnlockFile, ntdll, 'NtUnlockFile');
  Result := TFNNtUnlockFile(_NtUnlockFile)(
    FileHandle, IoStatusBlock, LockOffset, LockLength, Key
  );
end;

// Dynamic version of NtUnlockFile
function  ZwUnlockFile(
    FileHandle : HANDLE;
    IoStatusBlock : PIO_STATUS_BLOCK;
    LockOffset : PULARGE_INTEGER;
    LockLength : PULARGE_INTEGER;
    Key : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtUnlockFile, ntdll, 'NtUnlockFile');
  Result := TFNNtUnlockFile(_NtUnlockFile)(
    FileHandle, IoStatusBlock, LockOffset, LockLength, Key
  );
end;

// Dynamic version of NtUnlockVirtualMemory
function  NtUnlockVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PPVOID;
    LockSize : PULONG;
    LockType : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtUnlockVirtualMemory, ntdll, 'NtUnlockVirtualMemory');
  Result := TFNNtUnlockVirtualMemory(_NtUnlockVirtualMemory)(
    ProcessHandle, BaseAddress, LockSize, LockType
  );
end;

// Dynamic version of NtUnlockVirtualMemory
function  ZwUnlockVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PPVOID;
    LockSize : PULONG;
    LockType : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtUnlockVirtualMemory, ntdll, 'NtUnlockVirtualMemory');
  Result := TFNNtUnlockVirtualMemory(_NtUnlockVirtualMemory)(
    ProcessHandle, BaseAddress, LockSize, LockType
  );
end;

// Dynamic version of NtUnmapViewOfSection
function  NtUnmapViewOfSection(
    ProcessHandle : HANDLE;
    BaseAddress : PVOID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtUnmapViewOfSection, ntdll, 'NtUnmapViewOfSection');
  Result := TFNNtUnmapViewOfSection(_NtUnmapViewOfSection)(
    ProcessHandle, BaseAddress
  );
end;

// Dynamic version of NtUnmapViewOfSection
function  ZwUnmapViewOfSection(
    ProcessHandle : HANDLE;
    BaseAddress : PVOID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtUnmapViewOfSection, ntdll, 'NtUnmapViewOfSection');
  Result := TFNNtUnmapViewOfSection(_NtUnmapViewOfSection)(
    ProcessHandle, BaseAddress
  );
end;

// Dynamic version of NtVdmControl
function  NtVdmControl(
    ControlCode : ULONG;
    ControlData : PVOID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtVdmControl, ntdll, 'NtVdmControl');
  Result := TFNNtVdmControl(_NtVdmControl)(
    ControlCode, ControlData
  );
end;

// Dynamic version of NtVdmControl
function  ZwVdmControl(
    ControlCode : ULONG;
    ControlData : PVOID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtVdmControl, ntdll, 'NtVdmControl');
  Result := TFNNtVdmControl(_NtVdmControl)(
    ControlCode, ControlData
  );
end;

// Dynamic version of NtW32Call
function  NtW32Call(
    RoutineIndex : ULONG;
    Argument : PVOID;
    ArgumentLength : ULONG;
    Result_ : PPVOID;
    ResultLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtW32Call, ntdll, 'NtW32Call');
  Result := TFNNtW32Call(_NtW32Call)(
    RoutineIndex, Argument, ArgumentLength, Result_, ResultLength
  );
end;

// Dynamic version of NtW32Call
function  ZwW32Call(
    RoutineIndex : ULONG;
    Argument : PVOID;
    ArgumentLength : ULONG;
    Result_ : PPVOID;
    ResultLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtW32Call, ntdll, 'NtW32Call');
  Result := TFNNtW32Call(_NtW32Call)(
    RoutineIndex, Argument, ArgumentLength, Result_, ResultLength
  );
end;

// Dynamic version of NtWaitForMultipleObjects
function  NtWaitForMultipleObjects(
    HandleCount : ULONG;
    Handles : PHANDLE;
    WaitType : WAIT_TYPE;
    Alertable : BOOLEAN;
    Timeout : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtWaitForMultipleObjects, ntdll, 'NtWaitForMultipleObjects');
  Result := TFNNtWaitForMultipleObjects(_NtWaitForMultipleObjects)(
    HandleCount, Handles, WaitType, Alertable, Timeout
  );
end;

// Dynamic version of NtWaitForMultipleObjects
function  ZwWaitForMultipleObjects(
    HandleCount : ULONG;
    Handles : PHANDLE;
    WaitType : WAIT_TYPE;
    Alertable : BOOLEAN;
    Timeout : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtWaitForMultipleObjects, ntdll, 'NtWaitForMultipleObjects');
  Result := TFNNtWaitForMultipleObjects(_NtWaitForMultipleObjects)(
    HandleCount, Handles, WaitType, Alertable, Timeout
  );
end;

{.$IFNDEF JWA_INCLUDEMODE}
// Dynamic version of NtWaitForSingleObject
function  NtWaitForSingleObject(
    Handle : HANDLE;
    Alertable : BOOLEAN;
    Timeout : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtWaitForSingleObject, ntdll, 'NtWaitForSingleObject');
  Result := TFNNtWaitForSingleObject(_NtWaitForSingleObject)(
    Handle, Alertable, Timeout
  );
end;
{.$ENDIF JWA_INCLUDEMODE}

// Dynamic version of NtWaitForSingleObject
function  ZwWaitForSingleObject(
    Handle : HANDLE;
    Alertable : BOOLEAN;
    Timeout : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtWaitForSingleObject, ntdll, 'NtWaitForSingleObject');
  Result := TFNNtWaitForSingleObject(_NtWaitForSingleObject)(
    Handle, Alertable, Timeout
  );
end;

// Dynamic version of NtWaitHighEventPair
function  NtWaitHighEventPair(
    EventPairHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtWaitHighEventPair, ntdll, 'NtWaitHighEventPair');
  Result := TFNNtWaitHighEventPair(_NtWaitHighEventPair)(
    EventPairHandle
  );
end;

// Dynamic version of NtWaitHighEventPair
function  ZwWaitHighEventPair(
    EventPairHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtWaitHighEventPair, ntdll, 'NtWaitHighEventPair');
  Result := TFNNtWaitHighEventPair(_NtWaitHighEventPair)(
    EventPairHandle
  );
end;

// Dynamic version of NtWaitLowEventPair
function  NtWaitLowEventPair(
    EventPairHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtWaitLowEventPair, ntdll, 'NtWaitLowEventPair');
  Result := TFNNtWaitLowEventPair(_NtWaitLowEventPair)(
    EventPairHandle
  );
end;

// Dynamic version of NtWaitLowEventPair
function  ZwWaitLowEventPair(
    EventPairHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtWaitLowEventPair, ntdll, 'NtWaitLowEventPair');
  Result := TFNNtWaitLowEventPair(_NtWaitLowEventPair)(
    EventPairHandle
  );
end;

// Dynamic version of NtWriteFile
function  NtWriteFile(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PVOID;
    Length : ULONG;
    ByteOffset : PLARGE_INTEGER;
    Key : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtWriteFile, ntdll, 'NtWriteFile');
  Result := TFNNtWriteFile(_NtWriteFile)(
    FileHandle, Event, ApcRoutine, ApcContext, IoStatusBlock, Buffer, Length, ByteOffset, Key
  );
end;

// Dynamic version of NtWriteFile
function  ZwWriteFile(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PVOID;
    Length : ULONG;
    ByteOffset : PLARGE_INTEGER;
    Key : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtWriteFile, ntdll, 'NtWriteFile');
  Result := TFNNtWriteFile(_NtWriteFile)(
    FileHandle, Event, ApcRoutine, ApcContext, IoStatusBlock, Buffer, Length, ByteOffset, Key
  );
end;

// Dynamic version of NtWriteFileGather
function  NtWriteFileGather(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PFILE_SEGMENT_ELEMENT;
    Length : ULONG;
    ByteOffset : PLARGE_INTEGER;
    Key : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtWriteFileGather, ntdll, 'NtWriteFileGather');
  Result := TFNNtWriteFileGather(_NtWriteFileGather)(
    FileHandle, Event, ApcRoutine, ApcContext, IoStatusBlock, Buffer, Length, ByteOffset, Key
  );
end;

// Dynamic version of NtWriteFileGather
function  ZwWriteFileGather(
    FileHandle : HANDLE;
    Event : HANDLE;
    ApcRoutine : PIO_APC_ROUTINE;
    ApcContext : PVOID;
    IoStatusBlock : PIO_STATUS_BLOCK;
    Buffer : PFILE_SEGMENT_ELEMENT;
    Length : ULONG;
    ByteOffset : PLARGE_INTEGER;
    Key : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtWriteFileGather, ntdll, 'NtWriteFileGather');
  Result := TFNNtWriteFileGather(_NtWriteFileGather)(
    FileHandle, Event, ApcRoutine, ApcContext, IoStatusBlock, Buffer, Length, ByteOffset, Key
  );
end;

// Dynamic version of NtWriteRequestData
function  NtWriteRequestData(
    PortHandle : HANDLE;
    Message : PPORT_MESSAGE;
    Index : ULONG;
    Buffer : PVOID;
    BufferLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtWriteRequestData, ntdll, 'NtWriteRequestData');
  Result := TFNNtWriteRequestData(_NtWriteRequestData)(
    PortHandle, Message, Index, Buffer, BufferLength, ReturnLength
  );
end;

// Dynamic version of NtWriteRequestData
function  ZwWriteRequestData(
    PortHandle : HANDLE;
    Message : PPORT_MESSAGE;
    Index : ULONG;
    Buffer : PVOID;
    BufferLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtWriteRequestData, ntdll, 'NtWriteRequestData');
  Result := TFNNtWriteRequestData(_NtWriteRequestData)(
    PortHandle, Message, Index, Buffer, BufferLength, ReturnLength
  );
end;

// Dynamic version of NtWriteVirtualMemory
function  NtWriteVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PVOID;
    Buffer : PVOID;
    BufferLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtWriteVirtualMemory, ntdll, 'NtWriteVirtualMemory');
  Result := TFNNtWriteVirtualMemory(_NtWriteVirtualMemory)(
    ProcessHandle, BaseAddress, Buffer, BufferLength, ReturnLength
  );
end;

// Dynamic version of NtWriteVirtualMemory
function  ZwWriteVirtualMemory(
    ProcessHandle : HANDLE;
    BaseAddress : PVOID;
    Buffer : PVOID;
    BufferLength : ULONG;
    ReturnLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtWriteVirtualMemory, ntdll, 'NtWriteVirtualMemory');
  Result := TFNNtWriteVirtualMemory(_NtWriteVirtualMemory)(
    ProcessHandle, BaseAddress, Buffer, BufferLength, ReturnLength
  );
end;

// Dynamic version of NtYieldExecution
function  NtYieldExecution(): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtYieldExecution, ntdll, 'NtYieldExecution');
  Result := TFNNtYieldExecution(_NtYieldExecution)();
end;

// Dynamic version of NtYieldExecution
function  ZwYieldExecution(): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_NtYieldExecution, ntdll, 'NtYieldExecution');
  Result := TFNNtYieldExecution(_NtYieldExecution)();
end;

// Dynamic version of RtlAbsoluteToSelfRelativeSD
function  RtlAbsoluteToSelfRelativeSD(
    pAbsoluteSD : PSECURITY_DESCRIPTOR;
    pSelfRelativeSD : PSECURITY_DESCRIPTOR;
    lpdwBufferLength : LPDWORD
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlAbsoluteToSelfRelativeSD, ntdll, 'RtlAbsoluteToSelfRelativeSD');
  Result := TFNRtlAbsoluteToSelfRelativeSD(_RtlAbsoluteToSelfRelativeSD)(
    pAbsoluteSD, pSelfRelativeSD, lpdwBufferLength
  );
end;

// Dynamic version of RtlAcquirePebLock
procedure RtlAcquirePebLock(); stdcall;
begin
  GetProcedureAddress(_RtlAcquirePebLock, ntdll, 'RtlAcquirePebLock');
  TFNRtlAcquirePebLock(_RtlAcquirePebLock)();
end;

// Dynamic version of RtlAddAccessAllowedAce
function  RtlAddAccessAllowedAce(
    pAcl : PACL;
    dwAceRevision : DWORD;
    AccessMask : ACCESS_MASK;
    pSid : PSID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlAddAccessAllowedAce, ntdll, 'RtlAddAccessAllowedAce');
  Result := TFNRtlAddAccessAllowedAce(_RtlAddAccessAllowedAce)(
    pAcl, dwAceRevision, AccessMask, pSid
  );
end;

// Dynamic version of RtlAddAccessAllowedAceEx
function  RtlAddAccessAllowedAceEx(
    pAcl : PACL;
    dwAceRevision : DWORD;
    AceFlags : DWORD;
    AccessMask : ACCESS_MASK;
    pSid : PSID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlAddAccessAllowedAceEx, ntdll, 'RtlAddAccessAllowedAceEx');
  Result := TFNRtlAddAccessAllowedAceEx(_RtlAddAccessAllowedAceEx)(
    pAcl, dwAceRevision, AceFlags, AccessMask, pSid
  );
end;

// Dynamic version of RtlAddAccessDeniedAce
function  RtlAddAccessDeniedAce(
    pAcl : PACL;
    dwAceRevision : DWORD;
    AccessMask : ACCESS_MASK;
    pSid : PSID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlAddAccessDeniedAce, ntdll, 'RtlAddAccessDeniedAce');
  Result := TFNRtlAddAccessDeniedAce(_RtlAddAccessDeniedAce)(
    pAcl, dwAceRevision, AccessMask, pSid
  );
end;

// Dynamic version of RtlAddAccessDeniedAceEx
function  RtlAddAccessDeniedAceEx(
    pAcl : PACL;
    dwAceRevision : DWORD;
    AceFlags : DWORD;
    AccessMask : ACCESS_MASK;
    pSid : PSID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlAddAccessDeniedAceEx, ntdll, 'RtlAddAccessDeniedAceEx');
  Result := TFNRtlAddAccessDeniedAceEx(_RtlAddAccessDeniedAceEx)(
    pAcl, dwAceRevision, AceFlags, AccessMask, pSid
  );
end;

// Dynamic version of RtlAddAce
function  RtlAddAce(
    pAcl : PACL;
    dwAceRevision : DWORD;
    dwStartingAceIndex : DWORD;
    pAceList : PVOID;
    nAceListLength : DWORD
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlAddAce, ntdll, 'RtlAddAce');
  Result := TFNRtlAddAce(_RtlAddAce)(
    pAcl, dwAceRevision, dwStartingAceIndex, pAceList, nAceListLength
  );
end;

// Dynamic version of RtlAddAuditAccessAce
function  RtlAddAuditAccessAce(
    pAcl : PACL;
    dwAceRevision : DWORD;
    AccessMask : ACCESS_MASK;
    pSid : PSID;
    bAuditSuccess : BOOLEAN;
    bAuditFailure : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlAddAuditAccessAce, ntdll, 'RtlAddAuditAccessAce');
  Result := TFNRtlAddAuditAccessAce(_RtlAddAuditAccessAce)(
    pAcl, dwAceRevision, AccessMask, pSid, bAuditSuccess, bAuditFailure
  );
end;

// Dynamic version of RtlAddAuditAccessAceEx
function  RtlAddAuditAccessAceEx(
    pAcl : PACL;
    dwAceRevision : DWORD;
    AceFlags : DWORD;
    AccessMask : ACCESS_MASK;
    pSid : PSID;
    bAuditSuccess : BOOLEAN;
    bAuditFailure : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlAddAuditAccessAceEx, ntdll, 'RtlAddAuditAccessAceEx');
  Result := TFNRtlAddAuditAccessAceEx(_RtlAddAuditAccessAceEx)(
    pAcl, dwAceRevision, AceFlags, AccessMask, pSid, bAuditSuccess, bAuditFailure
  );
end;

// Dynamic version of RtlAddRange
function  RtlAddRange(
    RangeList : PRTL_RANGE_LIST;
    Start : ULONGLONG;
    End_ : ULONGLONG;
    Attributes : UCHAR;
    Flags : ULONG;
    UserData : PVOID;
    Owner : PVOID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlAddRange, ntdll, 'RtlAddRange');
  Result := TFNRtlAddRange(_RtlAddRange)(
    RangeList, Start, End_, Attributes, Flags, UserData, Owner
  );
end;

// Dynamic version of RtlAddVectoredExceptionHandler
function  RtlAddVectoredExceptionHandler(
    FirstHandler : ULONG;
    VectoredHandler : PVECTORED_EXCEPTION_HANDLER
  ): PVOID; stdcall;
begin
  GetProcedureAddress(_RtlAddVectoredExceptionHandler, ntdll, 'RtlAddVectoredExceptionHandler');
  Result := TFNRtlAddVectoredExceptionHandler(_RtlAddVectoredExceptionHandler)(
    FirstHandler, VectoredHandler
  );
end;

// Dynamic version of RtlAdjustPrivilege
function  RtlAdjustPrivilege(
    Privilege : ULONG;
    Enable : BOOLEAN;
    CurrentThread : BOOLEAN;
    Enabled : PBOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlAdjustPrivilege, ntdll, 'RtlAdjustPrivilege');
  Result := TFNRtlAdjustPrivilege(_RtlAdjustPrivilege)(
    Privilege, Enable, CurrentThread, Enabled
  );
end;

// Dynamic version of RtlAllocateAndInitializeSid
function  RtlAllocateAndInitializeSid(
    pIdentifierAuthority : PSID_IDENTIFIER_AUTHORITY;
    SubAuthorityCount : BYTE;
    nSubAuthority0 : DWORD;
    nSubAuthority1 : DWORD;
    nSubAuthority2 : DWORD;
    nSubAuthority3 : DWORD;
    nSubAuthority4 : DWORD;
    nSubAuthority5 : DWORD;
    nSubAuthority6 : DWORD;
    nSubAuthority7 : DWORD;
    var pSid : PSID
  ): BOOL; stdcall;
begin
  GetProcedureAddress(_RtlAllocateAndInitializeSid, ntdll, 'RtlAllocateAndInitializeSid');
  Result := TFNRtlAllocateAndInitializeSid(_RtlAllocateAndInitializeSid)(
    pIdentifierAuthority, SubAuthorityCount, nSubAuthority0, nSubAuthority1, nSubAuthority2, nSubAuthority3, nSubAuthority4, nSubAuthority5, nSubAuthority6, nSubAuthority7, pSid
  );
end;

// Dynamic version of RtlAllocateHeap
function  RtlAllocateHeap(
    hHeap : HANDLE;
    dwFlags : ULONG;
    Size : ULONG
  ): PVOID; stdcall;
begin
  GetProcedureAddress(_RtlAllocateHeap, ntdll, 'RtlAllocateHeap');
  Result := TFNRtlAllocateHeap(_RtlAllocateHeap)(
    hHeap, dwFlags, Size
  );
end;

// Dynamic version of RtlAnsiCharToUnicodeChar
function  RtlAnsiCharToUnicodeChar(
    AnsiChar : AnsiChar
  ): WCHAR; stdcall;
begin
  GetProcedureAddress(_RtlAnsiCharToUnicodeChar, ntdll, 'RtlAnsiCharToUnicodeChar');
  Result := TFNRtlAnsiCharToUnicodeChar(_RtlAnsiCharToUnicodeChar)(
    AnsiChar
  );
end;

// Dynamic version of RtlAnsiStringToUnicodeSize
function  RtlAnsiStringToUnicodeSize(
    AnsiString : PANSI_STRING
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlAnsiStringToUnicodeSize, ntdll, 'RtlAnsiStringToUnicodeSize');
  Result := TFNRtlAnsiStringToUnicodeSize(_RtlAnsiStringToUnicodeSize)(
    AnsiString
  );
end;

{.$IFNDEF JWA_INCLUDEMODE}
// Dynamic version of RtlAnsiStringToUnicodeString
function  RtlAnsiStringToUnicodeString(
    DestinationString : PUNICODE_STRING;
    SourceString : PANSI_STRING;
    AllocateDestinationString : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlAnsiStringToUnicodeString, ntdll, 'RtlAnsiStringToUnicodeString');
  Result := TFNRtlAnsiStringToUnicodeString(_RtlAnsiStringToUnicodeString)(
    DestinationString, SourceString, AllocateDestinationString
  );
end;
{.$ENDIF JWA_INCLUDEMODE}

// Dynamic version of RtlAppendAsciizToString
function  RtlAppendAsciizToString(
    DestinationString : PSTRING;
    AppendThisString : LPCSTR
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlAppendAsciizToString, ntdll, 'RtlAppendAsciizToString');
  Result := TFNRtlAppendAsciizToString(_RtlAppendAsciizToString)(
    DestinationString, AppendThisString
  );
end;

// Dynamic version of RtlAppendStringToString
function  RtlAppendStringToString(
    DestinationString : PSTRING;
    AppendThisString : PSTRING
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlAppendStringToString, ntdll, 'RtlAppendStringToString');
  Result := TFNRtlAppendStringToString(_RtlAppendStringToString)(
    DestinationString, AppendThisString
  );
end;

// Dynamic version of RtlAppendUnicodeStringToString
function  RtlAppendUnicodeStringToString(
    DestinationString : PUNICODE_STRING;
    SourceString : PUNICODE_STRING
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlAppendUnicodeStringToString, ntdll, 'RtlAppendUnicodeStringToString');
  Result := TFNRtlAppendUnicodeStringToString(_RtlAppendUnicodeStringToString)(
    DestinationString, SourceString
  );
end;

// Dynamic version of RtlAppendUnicodeToString
function  RtlAppendUnicodeToString(
    Destination : PUNICODE_STRING;
    Source : LPCWSTR
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlAppendUnicodeToString, ntdll, 'RtlAppendUnicodeToString');
  Result := TFNRtlAppendUnicodeToString(_RtlAppendUnicodeToString)(
    Destination, Source
  );
end;

// Dynamic version of RtlAreAllAccessesGranted
function  RtlAreAllAccessesGranted(
    GrantedAccess : ACCESS_MASK;
    WantedAccess : ACCESS_MASK
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlAreAllAccessesGranted, ntdll, 'RtlAreAllAccessesGranted');
  Result := TFNRtlAreAllAccessesGranted(_RtlAreAllAccessesGranted)(
    GrantedAccess, WantedAccess
  );
end;

// Dynamic version of RtlAreAnyAccessesGranted
function  RtlAreAnyAccessesGranted(
    GrantedAccess : ACCESS_MASK;
    WantedAccess : ACCESS_MASK
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlAreAnyAccessesGranted, ntdll, 'RtlAreAnyAccessesGranted');
  Result := TFNRtlAreAnyAccessesGranted(_RtlAreAnyAccessesGranted)(
    GrantedAccess, WantedAccess
  );
end;

// Dynamic version of RtlAreBitsClear
function  RtlAreBitsClear(
    BitMapHeader : PRTL_BITMAP;
    StartingIndex : ULONG;
    Length : ULONG
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlAreBitsClear, ntdll, 'RtlAreBitsClear');
  Result := TFNRtlAreBitsClear(_RtlAreBitsClear)(
    BitMapHeader, StartingIndex, Length
  );
end;

// Dynamic version of RtlAreBitsSet
function  RtlAreBitsSet(
    BitMapHeader : PRTL_BITMAP;
    StartingIndex : ULONG;
    Length : ULONG
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlAreBitsSet, ntdll, 'RtlAreBitsSet');
  Result := TFNRtlAreBitsSet(_RtlAreBitsSet)(
    BitMapHeader, StartingIndex, Length
  );
end;

// Dynamic version of RtlAssert
procedure RtlAssert(
    FailedAssertion : PVOID;
    FileName : PVOID;
    LineNumber : ULONG;
    Message : PAnsiChar
  ); stdcall;
begin
  GetProcedureAddress(_RtlAssert, ntdll, 'RtlAssert');
  TFNRtlAssert(_RtlAssert)(
    FailedAssertion, FileName, LineNumber, Message
  );
end;

{$IFNDEF JWA_INCLUDEMODE}
// Dynamic version of RtlCaptureContext
procedure RtlCaptureContext(
    ContextRecord : PCONTEXT
  ); stdcall;
begin
  GetProcedureAddress(_RtlCaptureContext, ntdll, 'RtlCaptureContext');
  TFNRtlCaptureContext(_RtlCaptureContext)(
    ContextRecord
  );
end;


// Dynamic version of RtlCharToInteger
function  RtlCharToInteger(
    Str : PCSZ;
    Base : ULONG;
    Value : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlCharToInteger, ntdll, 'RtlCharToInteger');
  Result := TFNRtlCharToInteger(_RtlCharToInteger)(
    Str, Base, Value
  );
end;
{$ENDIF JWA_INCLUDEMODE}

// Dynamic version of RtlCheckForOrphanedCriticalSections
procedure RtlCheckForOrphanedCriticalSections(
    hThread : HANDLE
  ); stdcall;
begin
  GetProcedureAddress(_RtlCheckForOrphanedCriticalSections, ntdll, 'RtlCheckForOrphanedCriticalSections');
  TFNRtlCheckForOrphanedCriticalSections(_RtlCheckForOrphanedCriticalSections)(
    hThread
  );
end;

// Dynamic version of RtlCheckRegistryKey
function  RtlCheckRegistryKey(
    RelativeTo : ULONG;
    Path : PWSTR
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlCheckRegistryKey, ntdll, 'RtlCheckRegistryKey');
  Result := TFNRtlCheckRegistryKey(_RtlCheckRegistryKey)(
    RelativeTo, Path
  );
end;

// Dynamic version of RtlClearAllBits
procedure RtlClearAllBits(
    BitMapHeader : PRTL_BITMAP
  ); stdcall;
begin
  GetProcedureAddress(_RtlClearAllBits, ntdll, 'RtlClearAllBits');
  TFNRtlClearAllBits(_RtlClearAllBits)(
    BitMapHeader
  );
end;

// Dynamic version of RtlClearBits
procedure RtlClearBits(
    BitMapHeader : PRTL_BITMAP;
    StartingIndex : ULONG;
    NumberToClear : ULONG
  ); stdcall;
begin
  GetProcedureAddress(_RtlClearBits, ntdll, 'RtlClearBits');
  TFNRtlClearBits(_RtlClearBits)(
    BitMapHeader, StartingIndex, NumberToClear
  );
end;

// Dynamic version of RtlCompactHeap
function  RtlCompactHeap(
    hHeap : HANDLE;
    dwFlags : ULONG
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlCompactHeap, ntdll, 'RtlCompactHeap');
  Result := TFNRtlCompactHeap(_RtlCompactHeap)(
    hHeap, dwFlags
  );
end;

{$IFNDEF JWA_INCLUDEMODE}
// Dynamic version of RtlCompareMemory
function  RtlCompareMemory(
    Source1 : PVOID;
    Source2 : PVOID;
    Length : SIZE_T
  ): SIZE_T; stdcall;
begin
  GetProcedureAddress(_RtlCompareMemory, ntdll, 'RtlCompareMemory');
  Result := TFNRtlCompareMemory(_RtlCompareMemory)(
    Source1, Source2, Length
  );
end;
{$ENDIF JWA_INCLUDEMODE}

// Dynamic version of RtlCompareMemoryUlong
function  RtlCompareMemoryUlong(
    Source : PVOID;
    Length : ULONG;
    Value : ULONG
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlCompareMemoryUlong, ntdll, 'RtlCompareMemoryUlong');
  Result := TFNRtlCompareMemoryUlong(_RtlCompareMemoryUlong)(
    Source, Length, Value
  );
end;

// Dynamic version of RtlCompareString
function  RtlCompareString(
    String1 : PSTRING;
    String2 : PSTRING;
    CaseInsensitive : BOOLEAN
  ): LONG; stdcall;
begin
  GetProcedureAddress(_RtlCompareString, ntdll, 'RtlCompareString');
  Result := TFNRtlCompareString(_RtlCompareString)(
    String1, String2, CaseInsensitive
  );
end;

// Dynamic version of RtlCompareUnicodeString
function  RtlCompareUnicodeString(
    String1 : PUNICODE_STRING;
    String2 : PUNICODE_STRING;
    CaseInsensitive : BOOLEAN
  ): LONG; stdcall;
begin
  GetProcedureAddress(_RtlCompareUnicodeString, ntdll, 'RtlCompareUnicodeString');
  Result := TFNRtlCompareUnicodeString(_RtlCompareUnicodeString)(
    String1, String2, CaseInsensitive
  );
end;

// Dynamic version of RtlConvertLongToLargeInteger
function  RtlConvertLongToLargeInteger(
    SignedInteger : LONG
  ): LARGE_INTEGER; stdcall;
begin
  GetProcedureAddress(_RtlConvertLongToLargeInteger, ntdll, 'RtlConvertLongToLargeInteger');
  Result := TFNRtlConvertLongToLargeInteger(_RtlConvertLongToLargeInteger)(
    SignedInteger
  );
end;

// Dynamic version of RtlConvertSidToUnicodeString
function  RtlConvertSidToUnicodeString(
    UnicodeString : PUNICODE_STRING;
    Sid : PSID;
    AllocateDestinationString : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlConvertSidToUnicodeString, ntdll, 'RtlConvertSidToUnicodeString');
  Result := TFNRtlConvertSidToUnicodeString(_RtlConvertSidToUnicodeString)(
    UnicodeString, Sid, AllocateDestinationString
  );
end;

// Dynamic version of RtlConvertUlongToLargeInteger
function  RtlConvertUlongToLargeInteger(
    UnsignedInteger : ULONG
  ): LARGE_INTEGER; stdcall;
begin
  GetProcedureAddress(_RtlConvertUlongToLargeInteger, ntdll, 'RtlConvertUlongToLargeInteger');
  Result := TFNRtlConvertUlongToLargeInteger(_RtlConvertUlongToLargeInteger)(
    UnsignedInteger
  );
end;

// Dynamic version of RtlCopyLuid
procedure RtlCopyLuid(
    Destination : PLUID;
    Source : PLUID
  ); stdcall;
begin
  GetProcedureAddress(_RtlCopyLuid, ntdll, 'RtlCopyLuid');
  TFNRtlCopyLuid(_RtlCopyLuid)(
    Destination, Source
  );
end;

// Dynamic version of RtlCopyRangeList
function  RtlCopyRangeList(
    CopyRangeList : PRTL_RANGE_LIST;
    RangeList : PRTL_RANGE_LIST
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlCopyRangeList, ntdll, 'RtlCopyRangeList');
  Result := TFNRtlCopyRangeList(_RtlCopyRangeList)(
    CopyRangeList, RangeList
  );
end;

// Dynamic version of RtlCopySecurityDescriptor
function  RtlCopySecurityDescriptor(
    Source : PSECURITY_DESCRIPTOR;
    var Destination : PSECURITY_DESCRIPTOR
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlCopySecurityDescriptor, ntdll, 'RtlCopySecurityDescriptor');
  Result := TFNRtlCopySecurityDescriptor(_RtlCopySecurityDescriptor)(
    Source, Destination
  );
end;

// Dynamic version of RtlCopySid
function  RtlCopySid(
    DestinationLength : ULONG;
    Destination : PSID;
    Source : PSID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlCopySid, ntdll, 'RtlCopySid');
  Result := TFNRtlCopySid(_RtlCopySid)(
    DestinationLength, Destination, Source
  );
end;

// Dynamic version of RtlCopyString
procedure RtlCopyString(
    DestinationString : PSTRING;
    SourceString : PSTRING
  ); stdcall;
begin
  GetProcedureAddress(_RtlCopyString, ntdll, 'RtlCopyString');
  TFNRtlCopyString(_RtlCopyString)(
    DestinationString, SourceString
  );
end;

// Dynamic version of RtlCopyUnicodeString
procedure RtlCopyUnicodeString(
    DestinationString : PUNICODE_STRING;
    SourceString : PUNICODE_STRING
  ); stdcall;
begin
  GetProcedureAddress(_RtlCopyUnicodeString, ntdll, 'RtlCopyUnicodeString');
  TFNRtlCopyUnicodeString(_RtlCopyUnicodeString)(
    DestinationString, SourceString
  );
end;

// Dynamic version of RtlCreateAcl
function  RtlCreateAcl(
    pAcl : PACL;
    nAclLength : DWORD;
    dwAclRevision : DWORD
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlCreateAcl, ntdll, 'RtlCreateAcl');
  Result := TFNRtlCreateAcl(_RtlCreateAcl)(
    pAcl, nAclLength, dwAclRevision
  );
end;

// Dynamic version of RtlCreateHeap
function  RtlCreateHeap(
    dwOptions : ULONG;
    Base : PVOID;
    dwMaximumSize : SIZE_T;
    dwInitialSize : SIZE_T;
    UnknownOptional1 : PVOID;
    UnknownOptional2 : PVOID
  ): HANDLE; stdcall;
begin
  GetProcedureAddress(_RtlCreateHeap, ntdll, 'RtlCreateHeap');
  Result := TFNRtlCreateHeap(_RtlCreateHeap)(
    dwOptions, Base, dwMaximumSize, dwInitialSize, UnknownOptional1, UnknownOptional2
  );
end;

// Dynamic version of RtlCreateProcessParameters
function  RtlCreateProcessParameters(
    ProcessParameters : PPRTL_USER_PROCESS_PARAMETERS;
    ImageFile : PUNICODE_STRING;
    DllPath : PUNICODE_STRING;
    CurrentDirectory : PUNICODE_STRING;
    CommandLine : PUNICODE_STRING;
    CreationFlags : ULONG;
    WindowTitle : PUNICODE_STRING;
    Desktop : PUNICODE_STRING;
    Reserved : PUNICODE_STRING;
    Reserved2 : PUNICODE_STRING
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlCreateProcessParameters, ntdll, 'RtlCreateProcessParameters');
  Result := TFNRtlCreateProcessParameters(_RtlCreateProcessParameters)(
    ProcessParameters, ImageFile, DllPath, CurrentDirectory, CommandLine, CreationFlags, WindowTitle, Desktop, Reserved, Reserved2
  );
end;

// Dynamic version of RtlCreateQueryDebugBuffer
function  RtlCreateQueryDebugBuffer(
    Size : ULONG;
    EventPair : BOOLEAN
  ): PDEBUG_BUFFER; stdcall;
begin
  GetProcedureAddress(_RtlCreateQueryDebugBuffer, ntdll, 'RtlCreateQueryDebugBuffer');
  Result := TFNRtlCreateQueryDebugBuffer(_RtlCreateQueryDebugBuffer)(
    Size, EventPair
  );
end;

// Dynamic version of RtlCreateRegistryKey
function  RtlCreateRegistryKey(
    RelativeTo : ULONG;
    Path : PWSTR
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlCreateRegistryKey, ntdll, 'RtlCreateRegistryKey');
  Result := TFNRtlCreateRegistryKey(_RtlCreateRegistryKey)(
    RelativeTo, Path
  );
end;

// Dynamic version of RtlCreateSecurityDescriptor
function  RtlCreateSecurityDescriptor(
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    Revision : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlCreateSecurityDescriptor, ntdll, 'RtlCreateSecurityDescriptor');
  Result := TFNRtlCreateSecurityDescriptor(_RtlCreateSecurityDescriptor)(
    SecurityDescriptor, Revision
  );
end;

// Dynamic version of RtlCreateUnicodeString
function  RtlCreateUnicodeString(
    DestinationString : PUNICODE_STRING;
    SourceString : PWSTR
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlCreateUnicodeString, ntdll, 'RtlCreateUnicodeString');
  Result := TFNRtlCreateUnicodeString(_RtlCreateUnicodeString)(
    DestinationString, SourceString
  );
end;

// Dynamic version of RtlCreateUnicodeStringFromAsciiz
function  RtlCreateUnicodeStringFromAsciiz(
    DestinationString : PUNICODE_STRING;
    SourceString : PAnsiChar
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlCreateUnicodeStringFromAsciiz, ntdll, 'RtlCreateUnicodeStringFromAsciiz');
  Result := TFNRtlCreateUnicodeStringFromAsciiz(_RtlCreateUnicodeStringFromAsciiz)(
    DestinationString, SourceString
  );
end;

// Dynamic version of RtlCreateUserProcess
function  RtlCreateUserProcess(
    ImageFileName : PUNICODE_STRING;
    Attributes : ULONG;
    ProcessParameters : PRTL_USER_PROCESS_PARAMETERS;
    ProcessSecurityDescriptor : PSECURITY_DESCRIPTOR;
    ThreadSecurityDescriptor : PSECURITY_DESCRIPTOR;
    ParentProcess : HANDLE;
    InheritHandles : BOOLEAN;
    DebugPort : HANDLE;
    ExceptionPort : HANDLE;
    ProcessInfo : PRTL_PROCESS_INFORMATION
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlCreateUserProcess, ntdll, 'RtlCreateUserProcess');
  Result := TFNRtlCreateUserProcess(_RtlCreateUserProcess)(
    ImageFileName, Attributes, ProcessParameters, ProcessSecurityDescriptor, ThreadSecurityDescriptor, ParentProcess, InheritHandles, DebugPort, ExceptionPort, ProcessInfo
  );
end;

// Dynamic version of RtlCreateUserThread
function  RtlCreateUserThread(
    hProcess : HANDLE;
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    CreateSuspended : BOOLEAN;
    StackZeroBits : ULONG;
    StackReserve : ULONG;
    StackCommit : ULONG;
    lpStartAddress : PTHREAD_START_ROUTINE;
    lpParameter : PVOID;
    phThread : PHANDLE;
    ClientId : PCLIENT_ID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlCreateUserThread, ntdll, 'RtlCreateUserThread');
  Result := TFNRtlCreateUserThread(_RtlCreateUserThread)(
    hProcess, SecurityDescriptor, CreateSuspended, StackZeroBits, StackReserve, StackCommit, lpStartAddress, lpParameter, phThread, ClientId
  );
end;

// Dynamic version of RtlCutoverTimeToSystemTime
function  RtlCutoverTimeToSystemTime(
    TargetTimeFields : PTIME_FIELDS;
    Time : PLARGE_INTEGER;
    CurrentTime : PLARGE_INTEGER;
    bUnknown : BOOLEAN
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlCutoverTimeToSystemTime, ntdll, 'RtlCutoverTimeToSystemTime');
  Result := TFNRtlCutoverTimeToSystemTime(_RtlCutoverTimeToSystemTime)(
    TargetTimeFields, Time, CurrentTime, bUnknown
  );
end;

// Dynamic version of RtlDeNormalizeProcessParams
function  RtlDeNormalizeProcessParams(
    ProcessParameters : PRTL_USER_PROCESS_PARAMETERS
  ): PRTL_USER_PROCESS_PARAMETERS; stdcall;
begin
  GetProcedureAddress(_RtlDeNormalizeProcessParams, ntdll, 'RtlDeNormalizeProcessParams');
  Result := TFNRtlDeNormalizeProcessParams(_RtlDeNormalizeProcessParams)(
    ProcessParameters
  );
end;

// Dynamic version of RtlDeleteAce
function  RtlDeleteAce(
    pAcl : PACL;
    dwAceIndex : DWORD
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlDeleteAce, ntdll, 'RtlDeleteAce');
  Result := TFNRtlDeleteAce(_RtlDeleteAce)(
    pAcl, dwAceIndex
  );
end;

// Dynamic version of RtlDeleteCriticalSection
procedure RtlDeleteCriticalSection(
    lpCriticalSection : PRTL_CRITICAL_SECTION
  ); stdcall;
begin
  GetProcedureAddress(_RtlDeleteCriticalSection, ntdll, 'RtlDeleteCriticalSection');
  TFNRtlDeleteCriticalSection(_RtlDeleteCriticalSection)(
    lpCriticalSection
  );
end;

// Dynamic version of RtlDeleteOwnersRanges
function  RtlDeleteOwnersRanges(
    RangeList : PRTL_RANGE_LIST;
    Owner : PVOID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlDeleteOwnersRanges, ntdll, 'RtlDeleteOwnersRanges');
  Result := TFNRtlDeleteOwnersRanges(_RtlDeleteOwnersRanges)(
    RangeList, Owner
  );
end;

// Dynamic version of RtlDeleteRange
function  RtlDeleteRange(
    RangeList : PRTL_RANGE_LIST;
    Start : ULONGLONG;
    End_ : ULONGLONG;
    Owner : PVOID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlDeleteRange, ntdll, 'RtlDeleteRange');
  Result := TFNRtlDeleteRange(_RtlDeleteRange)(
    RangeList, Start, End_, Owner
  );
end;

// Dynamic version of RtlDeleteRegistryValue
function  RtlDeleteRegistryValue(
    RelativeTo : ULONG;
    Path : LPCWSTR;
    ValueName : LPCWSTR
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlDeleteRegistryValue, ntdll, 'RtlDeleteRegistryValue');
  Result := TFNRtlDeleteRegistryValue(_RtlDeleteRegistryValue)(
    RelativeTo, Path, ValueName
  );
end;

// Dynamic version of RtlDestroyHeap
function  RtlDestroyHeap(
    HeapHandle : HANDLE
  ): HANDLE; stdcall;
begin
  GetProcedureAddress(_RtlDestroyHeap, ntdll, 'RtlDestroyHeap');
  Result := TFNRtlDestroyHeap(_RtlDestroyHeap)(
    HeapHandle
  );
end;

// Dynamic version of RtlDestroyProcessParameters
function  RtlDestroyProcessParameters(
    ProcessParameters : PRTL_USER_PROCESS_PARAMETERS
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlDestroyProcessParameters, ntdll, 'RtlDestroyProcessParameters');
  Result := TFNRtlDestroyProcessParameters(_RtlDestroyProcessParameters)(
    ProcessParameters
  );
end;

// Dynamic version of RtlDestroyQueryDebugBuffer
function  RtlDestroyQueryDebugBuffer(
    DebugBuffer : PDEBUG_BUFFER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlDestroyQueryDebugBuffer, ntdll, 'RtlDestroyQueryDebugBuffer');
  Result := TFNRtlDestroyQueryDebugBuffer(_RtlDestroyQueryDebugBuffer)(
    DebugBuffer
  );
end;

// Dynamic version of RtlDetermineDosPathNameType_U
function  RtlDetermineDosPathNameType_U(
    wcsPathNameType : PWSTR
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlDetermineDosPathNameType_U, ntdll, 'RtlDetermineDosPathNameType_U');
  Result := TFNRtlDetermineDosPathNameType_U(_RtlDetermineDosPathNameType_U)(
    wcsPathNameType
  );
end;

// Dynamic version of RtlDnsHostNameToComputerName
function  RtlDnsHostNameToComputerName(
    ComputerName : PUNICODE_STRING;
    DnsName : PUNICODE_STRING;
    AllocateComputerNameString : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlDnsHostNameToComputerName, ntdll, 'RtlDnsHostNameToComputerName');
  Result := TFNRtlDnsHostNameToComputerName(_RtlDnsHostNameToComputerName)(
    ComputerName, DnsName, AllocateComputerNameString
  );
end;

// Dynamic version of RtlDoesFileExists_U
function  RtlDoesFileExists_U(
    FileName : PWSTR
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlDoesFileExists_U, ntdll, 'RtlDoesFileExists_U');
  Result := TFNRtlDoesFileExists_U(_RtlDoesFileExists_U)(
    FileName
  );
end;

// Dynamic version of RtlDosPathNameToNtPathName_U
function  RtlDosPathNameToNtPathName_U(
    DosName : PWSTR;
    var NtName : UNICODE_STRING;
    DosFilePath : PPWSTR;
    NtFilePath : PUNICODE_STRING
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlDosPathNameToNtPathName_U, ntdll, 'RtlDosPathNameToNtPathName_U');
  Result := TFNRtlDosPathNameToNtPathName_U(_RtlDosPathNameToNtPathName_U)(
    DosName, NtName, DosFilePath, NtFilePath
  );
end;

// Dynamic version of RtlDosSearchPath_U
function  RtlDosSearchPath_U(
    SearchPath : PWSTR;
    Name : PWSTR;
    Ext : PWSTR;
    cbBuf : ULONG;
    Buffer : PWSTR;
    var Shortname : PWSTR
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlDosSearchPath_U, ntdll, 'RtlDosSearchPath_U');
  Result := TFNRtlDosSearchPath_U(_RtlDosSearchPath_U)(
    SearchPath, Name, Ext, cbBuf, Buffer, Shortname
  );
end;

// Dynamic version of RtlDowncaseUnicodeChar
function  RtlDowncaseUnicodeChar(
    Source : WCHAR
  ): WCHAR; stdcall;
begin
  GetProcedureAddress(_RtlDowncaseUnicodeChar, ntdll, 'RtlDowncaseUnicodeChar');
  Result := TFNRtlDowncaseUnicodeChar(_RtlDowncaseUnicodeChar)(
    Source
  );
end;

// Dynamic version of RtlDowncaseUnicodeString
function  RtlDowncaseUnicodeString(
    DestinationString : PUNICODE_STRING;
    SourceString : PUNICODE_STRING;
    AllocateDestinationString : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlDowncaseUnicodeString, ntdll, 'RtlDowncaseUnicodeString');
  Result := TFNRtlDowncaseUnicodeString(_RtlDowncaseUnicodeString)(
    DestinationString, SourceString, AllocateDestinationString
  );
end;

// Dynamic version of RtlDuplicateUnicodeString
function  RtlDuplicateUnicodeString(
    AddTerminatingZero : ULONG;
    Source : PUNICODE_STRING;
    Destination : PUNICODE_STRING
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlDuplicateUnicodeString, ntdll, 'RtlDuplicateUnicodeString');
  Result := TFNRtlDuplicateUnicodeString(_RtlDuplicateUnicodeString)(
    AddTerminatingZero, Source, Destination
  );
end;

// Dynamic version of RtlEnableEarlyCriticalSectionEventCreation
procedure RtlEnableEarlyCriticalSectionEventCreation(); stdcall;
begin
  GetProcedureAddress(_RtlEnableEarlyCriticalSectionEventCreation, ntdll, 'RtlEnableEarlyCriticalSectionEventCreation');
  TFNRtlEnableEarlyCriticalSectionEventCreation(_RtlEnableEarlyCriticalSectionEventCreation)();
end;

// Dynamic version of RtlEnlargedIntegerMultiply
function  RtlEnlargedIntegerMultiply(
    Multiplicand : LONG;
    Multiplier : LONG
  ): LARGE_INTEGER; stdcall;
begin
  GetProcedureAddress(_RtlEnlargedIntegerMultiply, ntdll, 'RtlEnlargedIntegerMultiply');
  Result := TFNRtlEnlargedIntegerMultiply(_RtlEnlargedIntegerMultiply)(
    Multiplicand, Multiplier
  );
end;

// Dynamic version of RtlEnlargedUnsignedDivide
function  RtlEnlargedUnsignedDivide(
    Dividend : ULARGE_INTEGER;
    Divisor : ULONG;
    Remainder : PULONG
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlEnlargedUnsignedDivide, ntdll, 'RtlEnlargedUnsignedDivide');
  Result := TFNRtlEnlargedUnsignedDivide(_RtlEnlargedUnsignedDivide)(
    Dividend, Divisor, Remainder
  );
end;

// Dynamic version of RtlEnlargedUnsignedMultiply
function  RtlEnlargedUnsignedMultiply(
    Multiplicand : ULONG;
    Multiplier : ULONG
  ): LARGE_INTEGER; stdcall;
begin
  GetProcedureAddress(_RtlEnlargedUnsignedMultiply, ntdll, 'RtlEnlargedUnsignedMultiply');
  Result := TFNRtlEnlargedUnsignedMultiply(_RtlEnlargedUnsignedMultiply)(
    Multiplicand, Multiplier
  );
end;

// Dynamic version of RtlEnterCriticalSection
procedure RtlEnterCriticalSection(
    lpCriticalSection : PRTL_CRITICAL_SECTION
  ); stdcall;
begin
  GetProcedureAddress(_RtlEnterCriticalSection, ntdll, 'RtlEnterCriticalSection');
  TFNRtlEnterCriticalSection(_RtlEnterCriticalSection)(
    lpCriticalSection
  );
end;

// Dynamic version of RtlEqualComputerName
function  RtlEqualComputerName(
    String1 : PUNICODE_STRING;
    String2 : PUNICODE_STRING
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlEqualComputerName, ntdll, 'RtlEqualComputerName');
  Result := TFNRtlEqualComputerName(_RtlEqualComputerName)(
    String1, String2
  );
end;

// Dynamic version of RtlEqualDomainName
function  RtlEqualDomainName(
    String1 : PUNICODE_STRING;
    String2 : PUNICODE_STRING
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlEqualDomainName, ntdll, 'RtlEqualDomainName');
  Result := TFNRtlEqualDomainName(_RtlEqualDomainName)(
    String1, String2
  );
end;

// Dynamic version of RtlEqualLuid
function  RtlEqualLuid(
    Luid1 : PLUID;
    Luid2 : PLUID
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlEqualLuid, ntdll, 'RtlEqualLuid');
  Result := TFNRtlEqualLuid(_RtlEqualLuid)(
    Luid1, Luid2
  );
end;

// Dynamic version of RtlEqualPrefixSid
function  RtlEqualPrefixSid(
    pSid1 : PSID;
    pSid2 : PSID
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlEqualPrefixSid, ntdll, 'RtlEqualPrefixSid');
  Result := TFNRtlEqualPrefixSid(_RtlEqualPrefixSid)(
    pSid1, pSid2
  );
end;

// Dynamic version of RtlEqualSid
function  RtlEqualSid(
    pSid1 : PSID;
    pSid2 : PSID
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlEqualSid, ntdll, 'RtlEqualSid');
  Result := TFNRtlEqualSid(_RtlEqualSid)(
    pSid1, pSid2
  );
end;

// Dynamic version of RtlEqualString
function  RtlEqualString(
    String1 : PSTRING;
    String2 : PSTRING;
    CaseInsensitive : BOOLEAN
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlEqualString, ntdll, 'RtlEqualString');
  Result := TFNRtlEqualString(_RtlEqualString)(
    String1, String2, CaseInsensitive
  );
end;

// Dynamic version of RtlEqualUnicodeString
function  RtlEqualUnicodeString(
    String1 : PUNICODE_STRING;
    String2 : PUNICODE_STRING;
    CaseInsensitive : BOOLEAN
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlEqualUnicodeString, ntdll, 'RtlEqualUnicodeString');
  Result := TFNRtlEqualUnicodeString(_RtlEqualUnicodeString)(
    String1, String2, CaseInsensitive
  );
end;

// Dynamic version of RtlEraseUnicodeString
procedure RtlEraseUnicodeString(
    Str : PUNICODE_STRING
  ); stdcall;
begin
  GetProcedureAddress(_RtlEraseUnicodeString, ntdll, 'RtlEraseUnicodeString');
  TFNRtlEraseUnicodeString(_RtlEraseUnicodeString)(
    Str
  );
end;

// Dynamic version of RtlExpandEnvironmentStrings_U
function  RtlExpandEnvironmentStrings_U(
    Environment : PVOID;
    Source : PUNICODE_STRING;
    Destination : PUNICODE_STRING;
    ReturnedLength : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlExpandEnvironmentStrings_U, ntdll, 'RtlExpandEnvironmentStrings_U');
  Result := TFNRtlExpandEnvironmentStrings_U(_RtlExpandEnvironmentStrings_U)(
    Environment, Source, Destination, ReturnedLength
  );
end;

// Dynamic version of RtlExtendedIntegerMultiply
function  RtlExtendedIntegerMultiply(
    Multiplicand : LARGE_INTEGER;
    Multiplier : LONG
  ): LARGE_INTEGER; stdcall;
begin
  GetProcedureAddress(_RtlExtendedIntegerMultiply, ntdll, 'RtlExtendedIntegerMultiply');
  Result := TFNRtlExtendedIntegerMultiply(_RtlExtendedIntegerMultiply)(
    Multiplicand, Multiplier
  );
end;

// Dynamic version of RtlExtendedLargeIntegerDivide
function  RtlExtendedLargeIntegerDivide(
    Dividend : LARGE_INTEGER;
    Divisor : ULONG;
    Remainder : PULONG
  ): LARGE_INTEGER; stdcall;
begin
  GetProcedureAddress(_RtlExtendedLargeIntegerDivide, ntdll, 'RtlExtendedLargeIntegerDivide');
  Result := TFNRtlExtendedLargeIntegerDivide(_RtlExtendedLargeIntegerDivide)(
    Dividend, Divisor, Remainder
  );
end;

// Dynamic version of RtlExtendedMagicDivide
function  RtlExtendedMagicDivide(
    Dividend : LARGE_INTEGER;
    MagicDivisor : LARGE_INTEGER;
    ShiftCount : CCHAR
  ): LARGE_INTEGER; stdcall;
begin
  GetProcedureAddress(_RtlExtendedMagicDivide, ntdll, 'RtlExtendedMagicDivide');
  Result := TFNRtlExtendedMagicDivide(_RtlExtendedMagicDivide)(
    Dividend, MagicDivisor, ShiftCount
  );
end;

// Dynamic version of RtlFillMemory
procedure RtlFillMemory(
    Destination : PVOID;
    Length : SIZE_T;
    Fill : UCHAR
  ); stdcall;
begin
  GetProcedureAddress(_RtlFillMemory, ntdll, 'RtlFillMemory');
  TFNRtlFillMemory(_RtlFillMemory)(
    Destination, Length, Fill
  );
end;

// Dynamic version of RtlFillMemoryUlong
procedure RtlFillMemoryUlong(
    Destination : PVOID;
    Length : ULONG;
    Fill : ULONG
  ); stdcall;
begin
  GetProcedureAddress(_RtlFillMemoryUlong, ntdll, 'RtlFillMemoryUlong');
  TFNRtlFillMemoryUlong(_RtlFillMemoryUlong)(
    Destination, Length, Fill
  );
end;

// Dynamic version of RtlFindCharInUnicodeString
function  RtlFindCharInUnicodeString(
    dwFlags : ULONG;
    UnicodeString : PUNICODE_STRING;
    CharactersToFind : PUNICODE_STRING;
    Positions : PUSHORT
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlFindCharInUnicodeString, ntdll, 'RtlFindCharInUnicodeString');
  Result := TFNRtlFindCharInUnicodeString(_RtlFindCharInUnicodeString)(
    dwFlags, UnicodeString, CharactersToFind, Positions
  );
end;

// Dynamic version of RtlFindClearBits
function  RtlFindClearBits(
    BitMapHeader : PRTL_BITMAP;
    NumberToFind : ULONG;
    HintIndex : ULONG
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlFindClearBits, ntdll, 'RtlFindClearBits');
  Result := TFNRtlFindClearBits(_RtlFindClearBits)(
    BitMapHeader, NumberToFind, HintIndex
  );
end;

// Dynamic version of RtlFindClearBitsAndSet
function  RtlFindClearBitsAndSet(
    BitMapHeader : PRTL_BITMAP;
    NumberToFind : ULONG;
    HintIndex : ULONG
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlFindClearBitsAndSet, ntdll, 'RtlFindClearBitsAndSet');
  Result := TFNRtlFindClearBitsAndSet(_RtlFindClearBitsAndSet)(
    BitMapHeader, NumberToFind, HintIndex
  );
end;

// Dynamic version of RtlFindLastBackwardRunClear
function  RtlFindLastBackwardRunClear(
    BitMapHeader : PRTL_BITMAP;
    FromIndex : ULONG;
    StartingRunIndex : PULONG
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlFindLastBackwardRunClear, ntdll, 'RtlFindLastBackwardRunClear');
  Result := TFNRtlFindLastBackwardRunClear(_RtlFindLastBackwardRunClear)(
    BitMapHeader, FromIndex, StartingRunIndex
  );
end;

// Dynamic version of RtlFindLeastSignificantBit
function  RtlFindLeastSignificantBit(
    Set_ : ULONGLONG
  ): CCHAR; stdcall;
begin
  GetProcedureAddress(_RtlFindLeastSignificantBit, ntdll, 'RtlFindLeastSignificantBit');
  Result := TFNRtlFindLeastSignificantBit(_RtlFindLeastSignificantBit)(
    Set_
  );
end;

// Dynamic version of RtlFindLongestRunClear
function  RtlFindLongestRunClear(
    BitMapHeader : PRTL_BITMAP;
    StartingIndex : PULONG
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlFindLongestRunClear, ntdll, 'RtlFindLongestRunClear');
  Result := TFNRtlFindLongestRunClear(_RtlFindLongestRunClear)(
    BitMapHeader, StartingIndex
  );
end;

// Dynamic version of RtlFindMostSignificantBit
function  RtlFindMostSignificantBit(
    Set_ : ULONGLONG
  ): CCHAR; stdcall;
begin
  GetProcedureAddress(_RtlFindMostSignificantBit, ntdll, 'RtlFindMostSignificantBit');
  Result := TFNRtlFindMostSignificantBit(_RtlFindMostSignificantBit)(
    Set_
  );
end;

// Dynamic version of RtlFindNextForwardRunClear
function  RtlFindNextForwardRunClear(
    BitMapHeader : PRTL_BITMAP;
    FromIndex : ULONG;
    StartingRunIndex : PULONG
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlFindNextForwardRunClear, ntdll, 'RtlFindNextForwardRunClear');
  Result := TFNRtlFindNextForwardRunClear(_RtlFindNextForwardRunClear)(
    BitMapHeader, FromIndex, StartingRunIndex
  );
end;

// Dynamic version of RtlFindRange
function  RtlFindRange(
    RangeList : PRTL_RANGE_LIST;
    Minimum : ULONGLONG;
    Maximum : ULONGLONG;
    Length : ULONG;
    Alignment : ULONG;
    Flags : ULONG;
    AttributeAvailableMask : UCHAR;
    Context : PVOID;
    Callback : PRTL_CONFLICT_RANGE_CALLBACK;
    Start : PULONGLONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlFindRange, ntdll, 'RtlFindRange');
  Result := TFNRtlFindRange(_RtlFindRange)(
    RangeList, Minimum, Maximum, Length, Alignment, Flags, AttributeAvailableMask, Context, Callback, Start
  );
end;

// Dynamic version of RtlFindSetBits
function  RtlFindSetBits(
    BitMapHeader : PRTL_BITMAP;
    NumberToFind : ULONG;
    HintIndex : ULONG
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlFindSetBits, ntdll, 'RtlFindSetBits');
  Result := TFNRtlFindSetBits(_RtlFindSetBits)(
    BitMapHeader, NumberToFind, HintIndex
  );
end;

// Dynamic version of RtlFindSetBitsAndClear
function  RtlFindSetBitsAndClear(
    BitMapHeader : PRTL_BITMAP;
    NumberToFind : ULONG;
    HintIndex : ULONG
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlFindSetBitsAndClear, ntdll, 'RtlFindSetBitsAndClear');
  Result := TFNRtlFindSetBitsAndClear(_RtlFindSetBitsAndClear)(
    BitMapHeader, NumberToFind, HintIndex
  );
end;

// Dynamic version of RtlFirstFreeAce
function  RtlFirstFreeAce(
    pAcl : PACL;
    var pAce : PVOID
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlFirstFreeAce, ntdll, 'RtlFirstFreeAce');
  Result := TFNRtlFirstFreeAce(_RtlFirstFreeAce)(
    pAcl, pAce
  );
end;

// Dynamic version of RtlFormatCurrentUserKeyPath
function  RtlFormatCurrentUserKeyPath(
    CurrentUserKeyPath : PUNICODE_STRING
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlFormatCurrentUserKeyPath, ntdll, 'RtlFormatCurrentUserKeyPath');
  Result := TFNRtlFormatCurrentUserKeyPath(_RtlFormatCurrentUserKeyPath)(
    CurrentUserKeyPath
  );
end;

// Dynamic version of RtlFreeAnsiString
procedure RtlFreeAnsiString(
    AnsiString : PANSI_STRING
  ); stdcall;
begin
  GetProcedureAddress(_RtlFreeAnsiString, ntdll, 'RtlFreeAnsiString');
  TFNRtlFreeAnsiString(_RtlFreeAnsiString)(
    AnsiString
  );
end;

// Dynamic version of RtlFreeHeap
function  RtlFreeHeap(
    hHeap : HANDLE;
    dwFlags : ULONG;
    MemoryPointer : PVOID
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlFreeHeap, ntdll, 'RtlFreeHeap');
  Result := TFNRtlFreeHeap(_RtlFreeHeap)(
    hHeap, dwFlags, MemoryPointer
  );
end;

// Dynamic version of RtlFreeOemString
procedure RtlFreeOemString(
    OemString : POEM_STRING
  ); stdcall;
begin
  GetProcedureAddress(_RtlFreeOemString, ntdll, 'RtlFreeOemString');
  TFNRtlFreeOemString(_RtlFreeOemString)(
    OemString
  );
end;

// Dynamic version of RtlFreeRangeList
procedure RtlFreeRangeList(
    RangeList : PRTL_RANGE_LIST
  ); stdcall;
begin
  GetProcedureAddress(_RtlFreeRangeList, ntdll, 'RtlFreeRangeList');
  TFNRtlFreeRangeList(_RtlFreeRangeList)(
    RangeList
  );
end;

// Dynamic version of RtlFreeSid
function  RtlFreeSid(
    pSid : PSID
  ): PVOID; stdcall;
begin
  GetProcedureAddress(_RtlFreeSid, ntdll, 'RtlFreeSid');
  Result := TFNRtlFreeSid(_RtlFreeSid)(
    pSid
  );
end;

// Dynamic version of RtlFreeUnicodeString
procedure RtlFreeUnicodeString(
    UnicodeString : PUNICODE_STRING
  ); stdcall;
begin
  GetProcedureAddress(_RtlFreeUnicodeString, ntdll, 'RtlFreeUnicodeString');
  TFNRtlFreeUnicodeString(_RtlFreeUnicodeString)(
    UnicodeString
  );
end;

// Dynamic version of RtlGUIDFromString
function  RtlGUIDFromString(
    GuidString : PUNICODE_STRING;
    Guid : LPGUID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlGUIDFromString, ntdll, 'RtlGUIDFromString');
  Result := TFNRtlGUIDFromString(_RtlGUIDFromString)(
    GuidString, Guid
  );
end;

// Dynamic version of RtlGetAce
function  RtlGetAce(
    pAcl : PACL;
    dwAceIndex : DWORD;
    var pAce : PVOID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlGetAce, ntdll, 'RtlGetAce');
  Result := TFNRtlGetAce(_RtlGetAce)(
    pAcl, dwAceIndex, pAce
  );
end;

// Dynamic version of RtlGetCallersAddress
procedure RtlGetCallersAddress(
    CallersAddress : PPVOID;
    CallersCaller : PPVOID
  ); stdcall;
begin
  GetProcedureAddress(_RtlGetCallersAddress, ntdll, 'RtlGetCallersAddress');
  TFNRtlGetCallersAddress(_RtlGetCallersAddress)(
    CallersAddress, CallersCaller
  );
end;

// Dynamic version of RtlGetControlSecurityDescriptor
function  RtlGetControlSecurityDescriptor(
    pSecurityDescriptor : PSECURITY_DESCRIPTOR;
    var Control : SECURITY_DESCRIPTOR_CONTROL;
    var dwRevision : DWORD
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlGetControlSecurityDescriptor, ntdll, 'RtlGetControlSecurityDescriptor');
  Result := TFNRtlGetControlSecurityDescriptor(_RtlGetControlSecurityDescriptor)(
    pSecurityDescriptor, Control, dwRevision
  );
end;

// Dynamic version of RtlGetCurrentDirectory_U
function  RtlGetCurrentDirectory_U(
    MaximumLength : ULONG;
    Buffer : PWSTR
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlGetCurrentDirectory_U, ntdll, 'RtlGetCurrentDirectory_U');
  Result := TFNRtlGetCurrentDirectory_U(_RtlGetCurrentDirectory_U)(
    MaximumLength, Buffer
  );
end;

// Dynamic version of RtlGetCurrentPeb
function  RtlGetCurrentPeb(): PPEB; stdcall;
begin
  GetProcedureAddress(_RtlGetCurrentPeb, ntdll, 'RtlGetCurrentPeb');
  Result := TFNRtlGetCurrentPeb(_RtlGetCurrentPeb)();
end;

// Dynamic version of RtlGetDaclSecurityDescriptor
function  RtlGetDaclSecurityDescriptor(
    pSecurityDescriptor : PSECURITY_DESCRIPTOR;
    var bDaclPresent : BOOLEAN;
    var Dacl : PACL;
    var bDaclDefaulted : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlGetDaclSecurityDescriptor, ntdll, 'RtlGetDaclSecurityDescriptor');
  Result := TFNRtlGetDaclSecurityDescriptor(_RtlGetDaclSecurityDescriptor)(
    pSecurityDescriptor, bDaclPresent, Dacl, bDaclDefaulted
  );
end;

// Dynamic version of RtlGetFirstRange
function  RtlGetFirstRange(
    RangeList : PRTL_RANGE_LIST;
    Iterator : PRTL_RANGE_LIST_ITERATOR;
    var Range : PRTL_RANGE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlGetFirstRange, ntdll, 'RtlGetFirstRange');
  Result := TFNRtlGetFirstRange(_RtlGetFirstRange)(
    RangeList, Iterator, Range
  );
end;

// Dynamic version of RtlGetFullPathName_U
function  RtlGetFullPathName_U(
    DosName : PWSTR;
    Size : ULONG;
    Buf : PWSTR;
    var Shortname : PWSTR
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlGetFullPathName_U, ntdll, 'RtlGetFullPathName_U');
  Result := TFNRtlGetFullPathName_U(_RtlGetFullPathName_U)(
    DosName, Size, Buf, Shortname
  );
end;

// Dynamic version of RtlGetGroupSecurityDescriptor
function  RtlGetGroupSecurityDescriptor(
    pSecurityDescriptor : PSECURITY_DESCRIPTOR;
    var pGroup : PSID;
    var bGroupDefaulted : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlGetGroupSecurityDescriptor, ntdll, 'RtlGetGroupSecurityDescriptor');
  Result := TFNRtlGetGroupSecurityDescriptor(_RtlGetGroupSecurityDescriptor)(
    pSecurityDescriptor, pGroup, bGroupDefaulted
  );
end;

// Dynamic version of RtlGetLastNtStatus
function  RtlGetLastNtStatus(): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlGetLastNtStatus, ntdll, 'RtlGetLastNtStatus');
  Result := TFNRtlGetLastNtStatus(_RtlGetLastNtStatus)();
end;

// Dynamic version of RtlGetLongestNtPathLength
function  RtlGetLongestNtPathLength(): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlGetLongestNtPathLength, ntdll, 'RtlGetLongestNtPathLength');
  Result := TFNRtlGetLongestNtPathLength(_RtlGetLongestNtPathLength)();
end;

// Dynamic version of RtlGetNextRange
function  RtlGetNextRange(
    Iterator : PRTL_RANGE_LIST_ITERATOR;
    var Range : PRTL_RANGE;
    MoveForwards : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlGetNextRange, ntdll, 'RtlGetNextRange');
  Result := TFNRtlGetNextRange(_RtlGetNextRange)(
    Iterator, Range, MoveForwards
  );
end;

// Dynamic version of RtlGetNtGlobalFlags
function  RtlGetNtGlobalFlags(): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlGetNtGlobalFlags, ntdll, 'RtlGetNtGlobalFlags');
  Result := TFNRtlGetNtGlobalFlags(_RtlGetNtGlobalFlags)();
end;

// Dynamic version of RtlGetNtProductType
function  RtlGetNtProductType(
    var ProductType : ULONG
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlGetNtProductType, ntdll, 'RtlGetNtProductType');
  Result := TFNRtlGetNtProductType(_RtlGetNtProductType)(
    ProductType
  );
end;

// Dynamic version of RtlGetNtVersionNumbers
procedure RtlGetNtVersionNumbers(
    var dwMajorVersion : ULONG;
    var dwMinorVersion : ULONG;
    UnknownCanBeNull : PDWORD
  ); stdcall;
begin
  GetProcedureAddress(_RtlGetNtVersionNumbers, ntdll, 'RtlGetNtVersionNumbers');
  TFNRtlGetNtVersionNumbers(_RtlGetNtVersionNumbers)(
    dwMajorVersion, dwMinorVersion, UnknownCanBeNull
  );
end;

// Dynamic version of RtlGetOwnerSecurityDescriptor
function  RtlGetOwnerSecurityDescriptor(
    pSecurityDescriptor : PSECURITY_DESCRIPTOR;
    var pOwner : PSID;
    var OwnerDefaulted : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlGetOwnerSecurityDescriptor, ntdll, 'RtlGetOwnerSecurityDescriptor');
  Result := TFNRtlGetOwnerSecurityDescriptor(_RtlGetOwnerSecurityDescriptor)(
    pSecurityDescriptor, pOwner, OwnerDefaulted
  );
end;

// Dynamic version of RtlGetProcessHeaps
function  RtlGetProcessHeaps(
    ArraySize : ULONG;
    HeapArray : PHANDLE
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlGetProcessHeaps, ntdll, 'RtlGetProcessHeaps');
  Result := TFNRtlGetProcessHeaps(_RtlGetProcessHeaps)(
    ArraySize, HeapArray
  );
end;

// Dynamic version of RtlGetSaclSecurityDescriptor
function  RtlGetSaclSecurityDescriptor(
    pSecurityDescriptor : PSECURITY_DESCRIPTOR;
    var bSaclPresent : BOOLEAN;
    var Sacl : PACL;
    var bSaclDefaulted : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlGetSaclSecurityDescriptor, ntdll, 'RtlGetSaclSecurityDescriptor');
  Result := TFNRtlGetSaclSecurityDescriptor(_RtlGetSaclSecurityDescriptor)(
    pSecurityDescriptor, bSaclPresent, Sacl, bSaclDefaulted
  );
end;

// Dynamic version of RtlGetVersion
function  RtlGetVersion(
    lpVersionInformation : PRTL_OSVERSIONINFOW
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlGetVersion, ntdll, 'RtlGetVersion');
  Result := TFNRtlGetVersion(_RtlGetVersion)(
    lpVersionInformation
  );
end;

// Dynamic version of RtlIdentifierAuthoritySid
function  RtlIdentifierAuthoritySid(
    Sid : PSID
  ): PSID_IDENTIFIER_AUTHORITY; stdcall;
begin
  GetProcedureAddress(_RtlIdentifierAuthoritySid, ntdll, 'RtlIdentifierAuthoritySid');
  Result := TFNRtlIdentifierAuthoritySid(_RtlIdentifierAuthoritySid)(
    Sid
  );
end;

// Dynamic version of RtlImageDirectoryEntryToData
function  RtlImageDirectoryEntryToData(
    ImageBase : HMODULE;
    MappedAsImage : BOOLEAN;
    DirectoryEntry : USHORT;
    Size : PULONG
  ): PVOID; stdcall;
begin
  GetProcedureAddress(_RtlImageDirectoryEntryToData, ntdll, 'RtlImageDirectoryEntryToData');
  Result := TFNRtlImageDirectoryEntryToData(_RtlImageDirectoryEntryToData)(
    ImageBase, MappedAsImage, DirectoryEntry, Size
  );
end;

// Dynamic version of RtlImageNtHeader
function  RtlImageNtHeader(
    ImageBase : HMODULE
  ): PIMAGE_NT_HEADERS; stdcall;
begin
  GetProcedureAddress(_RtlImageNtHeader, ntdll, 'RtlImageNtHeader');
  Result := TFNRtlImageNtHeader(_RtlImageNtHeader)(
    ImageBase
  );
end;

// Dynamic version of RtlImageNtHeaderEx
function  RtlImageNtHeaderEx(
    dwFlags : DWORD;
    ImageBase : HMODULE
  ): PIMAGE_NT_HEADERS; stdcall;
begin
  GetProcedureAddress(_RtlImageNtHeaderEx, ntdll, 'RtlImageNtHeaderEx');
  Result := TFNRtlImageNtHeaderEx(_RtlImageNtHeaderEx)(
    dwFlags, ImageBase
  );
end;

// Dynamic version of RtlImageRvaToSection
function  RtlImageRvaToSection(
    NtHeaders : PIMAGE_NT_HEADERS;
    ImageBase : HMODULE;
    Rva : ULONG
  ): PIMAGE_SECTION_HEADER; stdcall;
begin
  GetProcedureAddress(_RtlImageRvaToSection, ntdll, 'RtlImageRvaToSection');
  Result := TFNRtlImageRvaToSection(_RtlImageRvaToSection)(
    NtHeaders, ImageBase, Rva
  );
end;

// Dynamic version of RtlImageRvaToVa
function  RtlImageRvaToVa(
    NtHeaders : PIMAGE_NT_HEADERS;
    ImageBase : HMODULE;
    Rva : ULONG;
    var LastRvaSection : PIMAGE_SECTION_HEADER
  ): PVOID; stdcall;
begin
  GetProcedureAddress(_RtlImageRvaToVa, ntdll, 'RtlImageRvaToVa');
  Result := TFNRtlImageRvaToVa(_RtlImageRvaToVa)(
    NtHeaders, ImageBase, Rva, LastRvaSection
  );
end;

// Dynamic version of RtlImpersonateSelf
function  RtlImpersonateSelf(
    ImpersonationLevel : SECURITY_IMPERSONATION_LEVEL
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlImpersonateSelf, ntdll, 'RtlImpersonateSelf');
  Result := TFNRtlImpersonateSelf(_RtlImpersonateSelf)(
    ImpersonationLevel
  );
end;

// Dynamic version of RtlInitAnsiString
procedure RtlInitAnsiString(
    DestinationString : PANSI_STRING;
    SourceString : PCSZ
  ); stdcall;
begin
  GetProcedureAddress(_RtlInitAnsiString, ntdll, 'RtlInitAnsiString');
  TFNRtlInitAnsiString(_RtlInitAnsiString)(
    DestinationString, SourceString
  );
end;

// Dynamic version of RtlInitAnsiStringEx
function  RtlInitAnsiStringEx(
    DestinationString : PANSI_STRING;
    SourceString : PCSZ
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlInitAnsiStringEx, ntdll, 'RtlInitAnsiStringEx');
  Result := TFNRtlInitAnsiStringEx(_RtlInitAnsiStringEx)(
    DestinationString, SourceString
  );
end;

// Dynamic version of RtlInitString
procedure RtlInitString(
    DestinationString : PSTRING;
    SourceString : PCSZ
  ); stdcall;
begin
  GetProcedureAddress(_RtlInitString, ntdll, 'RtlInitString');
  TFNRtlInitString(_RtlInitString)(
    DestinationString, SourceString
  );
end;

// Dynamic version of RtlInitUnicodeString
procedure RtlInitUnicodeString(
    DestinationString : PUNICODE_STRING;
    SourceString : LPCWSTR
  ); stdcall;
begin
  GetProcedureAddress(_RtlInitUnicodeString, ntdll, 'RtlInitUnicodeString');
  TFNRtlInitUnicodeString(_RtlInitUnicodeString)(
    DestinationString, SourceString
  );
end;

// Dynamic version of RtlInitUnicodeStringEx
function  RtlInitUnicodeStringEx(
    DestinationString : PUNICODE_STRING;
    SourceString : LPCWSTR
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlInitUnicodeStringEx, ntdll, 'RtlInitUnicodeStringEx');
  Result := TFNRtlInitUnicodeStringEx(_RtlInitUnicodeStringEx)(
    DestinationString, SourceString
  );
end;

// Dynamic version of RtlInitializeBitMap
procedure RtlInitializeBitMap(
    BitMapHeader : PRTL_BITMAP;
    BitMapBuffer : PULONG;
    SizeOfBitMap : ULONG
  ); stdcall;
begin
  GetProcedureAddress(_RtlInitializeBitMap, ntdll, 'RtlInitializeBitMap');
  TFNRtlInitializeBitMap(_RtlInitializeBitMap)(
    BitMapHeader, BitMapBuffer, SizeOfBitMap
  );
end;

// Dynamic version of RtlInitializeCriticalSection
function  RtlInitializeCriticalSection(
    lpCriticalSection : PRTL_CRITICAL_SECTION
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlInitializeCriticalSection, ntdll, 'RtlInitializeCriticalSection');
  Result := TFNRtlInitializeCriticalSection(_RtlInitializeCriticalSection)(
    lpCriticalSection
  );
end;

// Dynamic version of RtlInitializeCriticalSectionAndSpinCount
function  RtlInitializeCriticalSectionAndSpinCount(
    lpCriticalSection : PRTL_CRITICAL_SECTION;
    dwSpinCount : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlInitializeCriticalSectionAndSpinCount, ntdll, 'RtlInitializeCriticalSectionAndSpinCount');
  Result := TFNRtlInitializeCriticalSectionAndSpinCount(_RtlInitializeCriticalSectionAndSpinCount)(
    lpCriticalSection, dwSpinCount
  );
end;

// Dynamic version of RtlInitializeRangeList
procedure RtlInitializeRangeList(
    RangeList : PRTL_RANGE_LIST
  ); stdcall;
begin
  GetProcedureAddress(_RtlInitializeRangeList, ntdll, 'RtlInitializeRangeList');
  TFNRtlInitializeRangeList(_RtlInitializeRangeList)(
    RangeList
  );
end;

{$IFNDEF JWA_INCLUDEMODE}
// Dynamic version of RtlInitializeSListHead
procedure RtlInitializeSListHead(
    ListHead : PSLIST_HEADER
  ); stdcall;
begin
  GetProcedureAddress(_RtlInitializeSListHead, ntdll, 'RtlInitializeSListHead');
  TFNRtlInitializeSListHead(_RtlInitializeSListHead)(
    ListHead
  );
end;
{$ENDIF JWA_INCLUDEMODE}

// Dynamic version of RtlInitializeSid
function  RtlInitializeSid(
    pSid : PSID;
    pIdentifierAuthority : PSID_IDENTIFIER_AUTHORITY;
    nSubAuthorityCount : UCHAR
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlInitializeSid, ntdll, 'RtlInitializeSid');
  Result := TFNRtlInitializeSid(_RtlInitializeSid)(
    pSid, pIdentifierAuthority, nSubAuthorityCount
  );
end;

// Dynamic version of RtlInt64ToUnicodeString
function  RtlInt64ToUnicodeString(
    Value : ULONGLONG;
    Base : ULONG;
    Str : PUNICODE_STRING
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlInt64ToUnicodeString, ntdll, 'RtlInt64ToUnicodeString');
  Result := TFNRtlInt64ToUnicodeString(_RtlInt64ToUnicodeString)(
    Value, Base, Str
  );
end;

// Dynamic version of RtlIntegerToChar
function  RtlIntegerToChar(
    Value : ULONG;
    Base : ULONG;
    Length : ULONG;
    Str : PAnsiChar
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlIntegerToChar, ntdll, 'RtlIntegerToChar');
  Result := TFNRtlIntegerToChar(_RtlIntegerToChar)(
    Value, Base, Length, Str
  );
end;

// Dynamic version of RtlIntegerToUnicodeString
function  RtlIntegerToUnicodeString(
    Value : ULONG;
    Base : ULONG;
    Str : PUNICODE_STRING
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlIntegerToUnicodeString, ntdll, 'RtlIntegerToUnicodeString');
  Result := TFNRtlIntegerToUnicodeString(_RtlIntegerToUnicodeString)(
    Value, Base, Str
  );
end;

{$IFNDEf JWA_INCLUDEMODE}
// Dynamic version of RtlInterlockedFlushSList
function  RtlInterlockedFlushSList(
    ListHead : PSLIST_HEADER
  ): PSLIST_ENTRY; stdcall;
begin
  GetProcedureAddress(_RtlInterlockedFlushSList, ntdll, 'RtlInterlockedFlushSList');
  Result := TFNRtlInterlockedFlushSList(_RtlInterlockedFlushSList)(
    ListHead
  );
end;


// Dynamic version of RtlInterlockedPopEntrySList
function  RtlInterlockedPopEntrySList(
    ListHead : PSLIST_HEADER
  ): PSLIST_ENTRY; stdcall;
begin
  GetProcedureAddress(_RtlInterlockedPopEntrySList, ntdll, 'RtlInterlockedPopEntrySList');
  Result := TFNRtlInterlockedPopEntrySList(_RtlInterlockedPopEntrySList)(
    ListHead
  );
end;
{$ENDIF JWA_INCLUDEMODE}

// Dynamic version of RtlInterlockedPushEntrySList
function  RtlInterlockedPushEntrySList(
    ListHead : PSLIST_HEADER;
    ListEntry : PSLIST_ENTRY
  ): PSLIST_ENTRY; stdcall;
begin
  GetProcedureAddress(_RtlInterlockedPushEntrySList, ntdll, 'RtlInterlockedPushEntrySList');
  Result := TFNRtlInterlockedPushEntrySList(_RtlInterlockedPushEntrySList)(
    ListHead, ListEntry
  );
end;


// Dynamic version of RtlInvertRangeList
function  RtlInvertRangeList(
    InvertedRangeList : PRTL_RANGE_LIST;
    RangeList : PRTL_RANGE_LIST
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlInvertRangeList, ntdll, 'RtlInvertRangeList');
  Result := TFNRtlInvertRangeList(_RtlInvertRangeList)(
    InvertedRangeList, RangeList
  );
end;

// Dynamic version of RtlIpv4AddressToStringA
function  RtlIpv4AddressToStringA(
    IP : PULONG;
    Buffer : LPSTR
  ): LPSTR; stdcall;
begin
  GetProcedureAddress(_RtlIpv4AddressToStringA, ntdll, 'RtlIpv4AddressToStringA');
  Result := TFNRtlIpv4AddressToStringA(_RtlIpv4AddressToStringA)(
    IP, Buffer
  );
end;

// Dynamic version of RtlIpv4AddressToStringW
function  RtlIpv4AddressToStringW(
    IP : PULONG;
    Buffer : LPWSTR
  ): LPWSTR; stdcall;
begin
  GetProcedureAddress(_RtlIpv4AddressToStringW, ntdll, 'RtlIpv4AddressToStringW');
  Result := TFNRtlIpv4AddressToStringW(_RtlIpv4AddressToStringW)(
    IP, Buffer
  );
end;

// Dynamic version of RtlIsDosDeviceName_U
function  RtlIsDosDeviceName_U(
    TestString : LPCWSTR
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlIsDosDeviceName_U, ntdll, 'RtlIsDosDeviceName_U');
  Result := TFNRtlIsDosDeviceName_U(_RtlIsDosDeviceName_U)(
    TestString
  );
end;

{.$IFNDEF JWA_INCLUDEMODE}
// Dynamic version of RtlIsNameLegalDOS8Dot3
function  RtlIsNameLegalDOS8Dot3(
    Name : PUNICODE_STRING;
    OemName : POEM_STRING;
    NameContainsSpaces : PBOOLEAN
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlIsNameLegalDOS8Dot3, ntdll, 'RtlIsNameLegalDOS8Dot3');
  Result := TFNRtlIsNameLegalDOS8Dot3(_RtlIsNameLegalDOS8Dot3)(
    Name, OemName, NameContainsSpaces
  );
end;
{.$ENDIF JWA_INCLUDEMODE}

// Dynamic version of RtlIsRangeAvailable
function  RtlIsRangeAvailable(
    RangeList : PRTL_RANGE_LIST;
    Start : ULONGLONG;
    End_ : ULONGLONG;
    Flags : ULONG;
    AttributeAvailableMask : UCHAR;
    Context : PVOID;
    Callback : PRTL_CONFLICT_RANGE_CALLBACK;
    Available : PBOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlIsRangeAvailable, ntdll, 'RtlIsRangeAvailable');
  Result := TFNRtlIsRangeAvailable(_RtlIsRangeAvailable)(
    RangeList, Start, End_, Flags, AttributeAvailableMask, Context, Callback, Available
  );
end;

// Dynamic version of RtlIsTextUnicode
function  RtlIsTextUnicode(
    lpBuffer : PVOID;
    cb : Integer;
    lpi : LPINT
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlIsTextUnicode, ntdll, 'RtlIsTextUnicode');
  Result := TFNRtlIsTextUnicode(_RtlIsTextUnicode)(
    lpBuffer, cb, lpi
  );
end;

// Dynamic version of RtlLargeIntegerAdd
function  RtlLargeIntegerAdd(
    Addend1 : LARGE_INTEGER;
    Addend2 : LARGE_INTEGER
  ): LARGE_INTEGER; stdcall;
begin
  GetProcedureAddress(_RtlLargeIntegerAdd, ntdll, 'RtlLargeIntegerAdd');
  Result := TFNRtlLargeIntegerAdd(_RtlLargeIntegerAdd)(
    Addend1, Addend2
  );
end;

// Dynamic version of RtlLargeIntegerArithmeticShift
function  RtlLargeIntegerArithmeticShift(
    LargeInteger : LARGE_INTEGER;
    ShiftCount : CCHAR
  ): LARGE_INTEGER; stdcall;
begin
  GetProcedureAddress(_RtlLargeIntegerArithmeticShift, ntdll, 'RtlLargeIntegerArithmeticShift');
  Result := TFNRtlLargeIntegerArithmeticShift(_RtlLargeIntegerArithmeticShift)(
    LargeInteger, ShiftCount
  );
end;

// Dynamic version of RtlLargeIntegerDivide
function  RtlLargeIntegerDivide(
    Dividend : LARGE_INTEGER;
    Divisor : LARGE_INTEGER;
    Remainder : PLARGE_INTEGER
  ): LARGE_INTEGER; stdcall;
begin
  GetProcedureAddress(_RtlLargeIntegerDivide, ntdll, 'RtlLargeIntegerDivide');
  Result := TFNRtlLargeIntegerDivide(_RtlLargeIntegerDivide)(
    Dividend, Divisor, Remainder
  );
end;

// Dynamic version of RtlLargeIntegerNegate
function  RtlLargeIntegerNegate(
    NegateThis : LARGE_INTEGER
  ): LARGE_INTEGER; stdcall;
begin
  GetProcedureAddress(_RtlLargeIntegerNegate, ntdll, 'RtlLargeIntegerNegate');
  Result := TFNRtlLargeIntegerNegate(_RtlLargeIntegerNegate)(
    NegateThis
  );
end;

// Dynamic version of RtlLargeIntegerShiftLeft
function  RtlLargeIntegerShiftLeft(
    LargeInteger : LARGE_INTEGER;
    ShiftCount : CCHAR
  ): LARGE_INTEGER; stdcall;
begin
  GetProcedureAddress(_RtlLargeIntegerShiftLeft, ntdll, 'RtlLargeIntegerShiftLeft');
  Result := TFNRtlLargeIntegerShiftLeft(_RtlLargeIntegerShiftLeft)(
    LargeInteger, ShiftCount
  );
end;

// Dynamic version of RtlLargeIntegerShiftRight
function  RtlLargeIntegerShiftRight(
    LargeInteger : LARGE_INTEGER;
    ShiftCount : CCHAR
  ): LARGE_INTEGER; stdcall;
begin
  GetProcedureAddress(_RtlLargeIntegerShiftRight, ntdll, 'RtlLargeIntegerShiftRight');
  Result := TFNRtlLargeIntegerShiftRight(_RtlLargeIntegerShiftRight)(
    LargeInteger, ShiftCount
  );
end;

// Dynamic version of RtlLargeIntegerSubtract
function  RtlLargeIntegerSubtract(
    Number : LARGE_INTEGER;
    Subtrahend : LARGE_INTEGER
  ): LARGE_INTEGER; stdcall;
begin
  GetProcedureAddress(_RtlLargeIntegerSubtract, ntdll, 'RtlLargeIntegerSubtract');
  Result := TFNRtlLargeIntegerSubtract(_RtlLargeIntegerSubtract)(
    Number, Subtrahend
  );
end;

// Dynamic version of RtlLargeIntegerToChar
function  RtlLargeIntegerToChar(
    Value : PLARGE_INTEGER;
    Base : ULONG;
    BufferLength : ULONG;
    Buffer : PAnsiChar
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlLargeIntegerToChar, ntdll, 'RtlLargeIntegerToChar');
  Result := TFNRtlLargeIntegerToChar(_RtlLargeIntegerToChar)(
    Value, Base, BufferLength, Buffer
  );
end;

// Dynamic version of RtlLeaveCriticalSection
procedure RtlLeaveCriticalSection(
    lpCriticalSection : PRTL_CRITICAL_SECTION
  ); stdcall;
begin
  GetProcedureAddress(_RtlLeaveCriticalSection, ntdll, 'RtlLeaveCriticalSection');
  TFNRtlLeaveCriticalSection(_RtlLeaveCriticalSection)(
    lpCriticalSection
  );
end;

// Dynamic version of RtlLengthRequiredSid
function  RtlLengthRequiredSid(
    nSubAuthorityCount : ULONG
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlLengthRequiredSid, ntdll, 'RtlLengthRequiredSid');
  Result := TFNRtlLengthRequiredSid(_RtlLengthRequiredSid)(
    nSubAuthorityCount
  );
end;

// Dynamic version of RtlLengthSecurityDescriptor
function  RtlLengthSecurityDescriptor(
    SecurityDescriptor : PSECURITY_DESCRIPTOR
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlLengthSecurityDescriptor, ntdll, 'RtlLengthSecurityDescriptor');
  Result := TFNRtlLengthSecurityDescriptor(_RtlLengthSecurityDescriptor)(
    SecurityDescriptor
  );
end;

// Dynamic version of RtlLengthSid
function  RtlLengthSid(
    pSid : PSID
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlLengthSid, ntdll, 'RtlLengthSid');
  Result := TFNRtlLengthSid(_RtlLengthSid)(
    pSid
  );
end;

{.$IFNDEF JWA_INCLUDEMODE}
// Dynamic version of RtlLocalTimeToSystemTime
function  RtlLocalTimeToSystemTime(
    LocalTime : PLARGE_INTEGER;
    SystemTime : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlLocalTimeToSystemTime, ntdll, 'RtlLocalTimeToSystemTime');
  Result := TFNRtlLocalTimeToSystemTime(_RtlLocalTimeToSystemTime)(
    LocalTime, SystemTime
  );
end;
{.$ENDIF JWA_INCLUDEMODE}

// Dynamic version of RtlLockHeap
function  RtlLockHeap(
    hHeap : PVOID
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlLockHeap, ntdll, 'RtlLockHeap');
  Result := TFNRtlLockHeap(_RtlLockHeap)(
    hHeap
  );
end;

// Dynamic version of RtlMakeSelfRelativeSD
function  RtlMakeSelfRelativeSD(
    pAbsoluteSD : PSECURITY_DESCRIPTOR;
    pSelfRelativeSD : PSECURITY_DESCRIPTOR;
    lpdwBufferLength : LPDWORD
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlMakeSelfRelativeSD, ntdll, 'RtlMakeSelfRelativeSD');
  Result := TFNRtlMakeSelfRelativeSD(_RtlMakeSelfRelativeSD)(
    pAbsoluteSD, pSelfRelativeSD, lpdwBufferLength
  );
end;

// Dynamic version of RtlMapGenericMask
procedure RtlMapGenericMask(
    AccessMask : PACCESS_MASK;
    GenericMapping : PGENERIC_MAPPING
  ); stdcall;
begin
  GetProcedureAddress(_RtlMapGenericMask, ntdll, 'RtlMapGenericMask');
  TFNRtlMapGenericMask(_RtlMapGenericMask)(
    AccessMask, GenericMapping
  );
end;

// Dynamic version of RtlMapSecurityErrorToNtStatus
function  RtlMapSecurityErrorToNtStatus(
    SecurityError : DWORD
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlMapSecurityErrorToNtStatus, ntdll, 'RtlMapSecurityErrorToNtStatus');
  Result := TFNRtlMapSecurityErrorToNtStatus(_RtlMapSecurityErrorToNtStatus)(
    SecurityError
  );
end;

// Dynamic version of RtlMergeRangeLists
function  RtlMergeRangeLists(
    MergedRangeList : PRTL_RANGE_LIST;
    RangeList1 : PRTL_RANGE_LIST;
    RangeList2 : PRTL_RANGE_LIST;
    Flags : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlMergeRangeLists, ntdll, 'RtlMergeRangeLists');
  Result := TFNRtlMergeRangeLists(_RtlMergeRangeLists)(
    MergedRangeList, RangeList1, RangeList2, Flags
  );
end;

// Dynamic version of RtlMoveMemory
procedure RtlMoveMemory(
    Destination : PVOID;
    Source : PVOID;
    Length : SIZE_T
  ); stdcall;
begin
  GetProcedureAddress(_RtlMoveMemory, ntdll, 'RtlMoveMemory');
  TFNRtlMoveMemory(_RtlMoveMemory)(
    Destination, Source, Length
  );
end;

// Dynamic version of RtlNormalizeProcessParams
function  RtlNormalizeProcessParams(
    ProcessParameters : PRTL_USER_PROCESS_PARAMETERS
  ): PRTL_USER_PROCESS_PARAMETERS; stdcall;
begin
  GetProcedureAddress(_RtlNormalizeProcessParams, ntdll, 'RtlNormalizeProcessParams');
  Result := TFNRtlNormalizeProcessParams(_RtlNormalizeProcessParams)(
    ProcessParameters
  );
end;


// Dynamic version of RtlNtStatusToDosError
function  RtlNtStatusToDosError(
    Status : NTSTATUS
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlNtStatusToDosError, ntdll, 'RtlNtStatusToDosError');
  Result := TFNRtlNtStatusToDosError(_RtlNtStatusToDosError)(
    Status
  );
end;


// Dynamic version of RtlNtStatusToDosErrorNoTeb
function  RtlNtStatusToDosErrorNoTeb(
    Status : NTSTATUS
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlNtStatusToDosErrorNoTeb, ntdll, 'RtlNtStatusToDosErrorNoTeb');
  Result := TFNRtlNtStatusToDosErrorNoTeb(_RtlNtStatusToDosErrorNoTeb)(
    Status
  );
end;

// Dynamic version of RtlNumberOfClearBits
function  RtlNumberOfClearBits(
    BitMapHeader : PRTL_BITMAP
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlNumberOfClearBits, ntdll, 'RtlNumberOfClearBits');
  Result := TFNRtlNumberOfClearBits(_RtlNumberOfClearBits)(
    BitMapHeader
  );
end;

// Dynamic version of RtlNumberOfSetBits
function  RtlNumberOfSetBits(
    BitMapHeader : PRTL_BITMAP
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlNumberOfSetBits, ntdll, 'RtlNumberOfSetBits');
  Result := TFNRtlNumberOfSetBits(_RtlNumberOfSetBits)(
    BitMapHeader
  );
end;

// Dynamic version of RtlOemStringToUnicodeSize
function  RtlOemStringToUnicodeSize(
    AnsiString : POEM_STRING
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlOemStringToUnicodeSize, ntdll, 'RtlOemStringToUnicodeSize');
  Result := TFNRtlOemStringToUnicodeSize(_RtlOemStringToUnicodeSize)(
    AnsiString
  );
end;

// Dynamic version of RtlOemStringToUnicodeString
function  RtlOemStringToUnicodeString(
    DestinationString : PUNICODE_STRING;
    SourceString : POEM_STRING;
    AllocateDestinationString : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlOemStringToUnicodeString, ntdll, 'RtlOemStringToUnicodeString');
  Result := TFNRtlOemStringToUnicodeString(_RtlOemStringToUnicodeString)(
    DestinationString, SourceString, AllocateDestinationString
  );
end;

// Dynamic version of RtlOemToUnicodeN
function  RtlOemToUnicodeN(
    UnicodeString : PWSTR;
    UnicodeSize : ULONG;
    var ResultSize : ULONG;
    OemString : PAnsiChar;
    OemSize : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlOemToUnicodeN, ntdll, 'RtlOemToUnicodeN');
  Result := TFNRtlOemToUnicodeN(_RtlOemToUnicodeN)(
    UnicodeString, UnicodeSize, ResultSize, OemString, OemSize
  );
end;

// Dynamic version of RtlOpenCurrentUser
function  RtlOpenCurrentUser(
    samDesired : ACCESS_MASK;
    phkResult : PHKEY
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlOpenCurrentUser, ntdll, 'RtlOpenCurrentUser');
  Result := TFNRtlOpenCurrentUser(_RtlOpenCurrentUser)(
    samDesired, phkResult
  );
end;

// Dynamic version of RtlPrefixString
function  RtlPrefixString(
    String1 : PANSI_STRING;
    String2 : PANSI_STRING;
    CaseInsensitive : BOOLEAN
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlPrefixString, ntdll, 'RtlPrefixString');
  Result := TFNRtlPrefixString(_RtlPrefixString)(
    String1, String2, CaseInsensitive
  );
end;

// Dynamic version of RtlPrefixUnicodeString
function  RtlPrefixUnicodeString(
    String1 : PUNICODE_STRING;
    String2 : PUNICODE_STRING;
    CaseInsensitive : BOOLEAN
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlPrefixUnicodeString, ntdll, 'RtlPrefixUnicodeString');
  Result := TFNRtlPrefixUnicodeString(_RtlPrefixUnicodeString)(
    String1, String2, CaseInsensitive
  );
end;

// Dynamic version of RtlQueryDepthSList
function  RtlQueryDepthSList(
    ListHead : PSLIST_HEADER
  ): USHORT; stdcall;
begin
  GetProcedureAddress(_RtlQueryDepthSList, ntdll, 'RtlQueryDepthSList');
  Result := TFNRtlQueryDepthSList(_RtlQueryDepthSList)(
    ListHead
  );
end;

// Dynamic version of RtlQueryEnvironmentVariable_U
function  RtlQueryEnvironmentVariable_U(
    Environment : PVOID;
    VarName : PUNICODE_STRING;
    VarValue : PUNICODE_STRING
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlQueryEnvironmentVariable_U, ntdll, 'RtlQueryEnvironmentVariable_U');
  Result := TFNRtlQueryEnvironmentVariable_U(_RtlQueryEnvironmentVariable_U)(
    Environment, VarName, VarValue
  );
end;

// Dynamic version of RtlQueryInformationAcl
function  RtlQueryInformationAcl(
    pAcl : PACL;
    pAclInformation : PVOID;
    nAclInformationLength : DWORD;
    dwAclInformationClass : ACL_INFORMATION_CLASS
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlQueryInformationAcl, ntdll, 'RtlQueryInformationAcl');
  Result := TFNRtlQueryInformationAcl(_RtlQueryInformationAcl)(
    pAcl, pAclInformation, nAclInformationLength, dwAclInformationClass
  );
end;

// Dynamic version of RtlQueryProcessDebugInformation
function  RtlQueryProcessDebugInformation(
    ProcessId : ULONG;
    DebugInfoClassMask : ULONG;
    DebugBuffer : PDEBUG_BUFFER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlQueryProcessDebugInformation, ntdll, 'RtlQueryProcessDebugInformation');
  Result := TFNRtlQueryProcessDebugInformation(_RtlQueryProcessDebugInformation)(
    ProcessId, DebugInfoClassMask, DebugBuffer
  );
end;

// Dynamic version of RtlQueryRegistryValues
function  RtlQueryRegistryValues(
    RelativeTo : ULONG;
    Path : LPCWSTR;
    QueryTable : PRTL_QUERY_REGISTRY_TABLE;
    Context : PVOID;
    Environment : PVOID
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlQueryRegistryValues, ntdll, 'RtlQueryRegistryValues');
  Result := TFNRtlQueryRegistryValues(_RtlQueryRegistryValues)(
    RelativeTo, Path, QueryTable, Context, Environment
  );
end;

// Dynamic version of RtlRaiseStatus
procedure RtlRaiseStatus(
    Status : NTSTATUS
  ); stdcall;
begin
  GetProcedureAddress(_RtlRaiseStatus, ntdll, 'RtlRaiseStatus');
  TFNRtlRaiseStatus(_RtlRaiseStatus)(
    Status
  );
end;

// Dynamic version of RtlRandom
function  RtlRandom(
    Seed : PULONG
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlRandom, ntdll, 'RtlRandom');
  Result := TFNRtlRandom(_RtlRandom)(
    Seed
  );
end;

// Dynamic version of RtlRandomEx
function  RtlRandomEx(
    Seed : PULONG
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlRandomEx, ntdll, 'RtlRandomEx');
  Result := TFNRtlRandomEx(_RtlRandomEx)(
    Seed
  );
end;

// Dynamic version of RtlReAllocateHeap
function  RtlReAllocateHeap(
    hHeap : HANDLE;
    dwFlags : ULONG;
    lpMem : PVOID;
    dwBytes : SIZE_T
  ): PVOID; stdcall;
begin
  GetProcedureAddress(_RtlReAllocateHeap, ntdll, 'RtlReAllocateHeap');
  Result := TFNRtlReAllocateHeap(_RtlReAllocateHeap)(
    hHeap, dwFlags, lpMem, dwBytes
  );
end;

// Dynamic version of RtlReleasePebLock
procedure RtlReleasePebLock(); stdcall;
begin
  GetProcedureAddress(_RtlReleasePebLock, ntdll, 'RtlReleasePebLock');
  TFNRtlReleasePebLock(_RtlReleasePebLock)();
end;

// Dynamic version of RtlRemoveVectoredExceptionHandler
function  RtlRemoveVectoredExceptionHandler(
    VectoredHandlerHandle : PVOID
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlRemoveVectoredExceptionHandler, ntdll, 'RtlRemoveVectoredExceptionHandler');
  Result := TFNRtlRemoveVectoredExceptionHandler(_RtlRemoveVectoredExceptionHandler)(
    VectoredHandlerHandle
  );
end;

// Dynamic version of RtlRestoreLastWin32Error
procedure RtlRestoreLastWin32Error(
    dwErrCode : DWORD
  ); stdcall;
begin
  GetProcedureAddress(_RtlRestoreLastWin32Error, ntdll, 'RtlRestoreLastWin32Error');
  TFNRtlRestoreLastWin32Error(_RtlRestoreLastWin32Error)(
    dwErrCode
  );
end;

// Dynamic version of RtlRunDecodeUnicodeString
procedure RtlRunDecodeUnicodeString(
    CodeSeed : UCHAR;
    StringToDecode : PUNICODE_STRING
  ); stdcall;
begin
  GetProcedureAddress(_RtlRunDecodeUnicodeString, ntdll, 'RtlRunDecodeUnicodeString');
  TFNRtlRunDecodeUnicodeString(_RtlRunDecodeUnicodeString)(
    CodeSeed, StringToDecode
  );
end;

// Dynamic version of RtlRunEncodeUnicodeString
procedure RtlRunEncodeUnicodeString(
    var CodeSeed : UCHAR;
    StringToEncode : PUNICODE_STRING
  ); stdcall;
begin
  GetProcedureAddress(_RtlRunEncodeUnicodeString, ntdll, 'RtlRunEncodeUnicodeString');
  TFNRtlRunEncodeUnicodeString(_RtlRunEncodeUnicodeString)(
    CodeSeed, StringToEncode
  );
end;

// Dynamic version of RtlSecondsSince1970ToTime
procedure RtlSecondsSince1970ToTime(
    SecondsSince1970 : ULONG;
    Time : PLARGE_INTEGER
  ); stdcall;
begin
  GetProcedureAddress(_RtlSecondsSince1970ToTime, ntdll, 'RtlSecondsSince1970ToTime');
  TFNRtlSecondsSince1970ToTime(_RtlSecondsSince1970ToTime)(
    SecondsSince1970, Time
  );
end;

// Dynamic version of RtlSecondsSince1980ToTime
procedure RtlSecondsSince1980ToTime(
    SecondsSince1980 : ULONG;
    Time : PLARGE_INTEGER
  ); stdcall;
begin
  GetProcedureAddress(_RtlSecondsSince1980ToTime, ntdll, 'RtlSecondsSince1980ToTime');
  TFNRtlSecondsSince1980ToTime(_RtlSecondsSince1980ToTime)(
    SecondsSince1980, Time
  );
end;

// Dynamic version of RtlSelfRelativeToAbsoluteSD
function  RtlSelfRelativeToAbsoluteSD(
    pSelfRelativeSD : PSECURITY_DESCRIPTOR;
    pAbsoluteSD : PSECURITY_DESCRIPTOR;
    lpdwAbsoluteSDSize : LPDWORD;
    pDacl : PACL;
    lpdwDaclSize : LPDWORD;
    pSacl : PACL;
    lpdwSaclSize : LPDWORD;
    pOwner : PSID;
    lpdwOwnerSize : LPDWORD;
    pPrimaryGroup : PSID;
    lpdwPrimaryGroupSize : LPDWORD
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlSelfRelativeToAbsoluteSD, ntdll, 'RtlSelfRelativeToAbsoluteSD');
  Result := TFNRtlSelfRelativeToAbsoluteSD(_RtlSelfRelativeToAbsoluteSD)(
    pSelfRelativeSD, pAbsoluteSD, lpdwAbsoluteSDSize, pDacl, lpdwDaclSize, pSacl, lpdwSaclSize, pOwner, lpdwOwnerSize, pPrimaryGroup, lpdwPrimaryGroupSize
  );
end;

// Dynamic version of RtlSetAllBits
procedure RtlSetAllBits(
    BitMapHeader : PRTL_BITMAP
  ); stdcall;
begin
  GetProcedureAddress(_RtlSetAllBits, ntdll, 'RtlSetAllBits');
  TFNRtlSetAllBits(_RtlSetAllBits)(
    BitMapHeader
  );
end;

// Dynamic version of RtlSetBits
procedure RtlSetBits(
    BitMapHeader : PRTL_BITMAP;
    StartingIndex : ULONG;
    NumberToSet : ULONG
  ); stdcall;
begin
  GetProcedureAddress(_RtlSetBits, ntdll, 'RtlSetBits');
  TFNRtlSetBits(_RtlSetBits)(
    BitMapHeader, StartingIndex, NumberToSet
  );
end;

// Dynamic version of RtlSetControlSecurityDescriptor
function  RtlSetControlSecurityDescriptor(
    pSecurityDescriptor : PSECURITY_DESCRIPTOR;
    ControlBitsOfInterest : SECURITY_DESCRIPTOR_CONTROL;
    ControlBitsToSet : SECURITY_DESCRIPTOR_CONTROL
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlSetControlSecurityDescriptor, ntdll, 'RtlSetControlSecurityDescriptor');
  Result := TFNRtlSetControlSecurityDescriptor(_RtlSetControlSecurityDescriptor)(
    pSecurityDescriptor, ControlBitsOfInterest, ControlBitsToSet
  );
end;

// Dynamic version of RtlSetCriticalSectionSpinCount
function  RtlSetCriticalSectionSpinCount(
    lpCriticalSection : PRTL_CRITICAL_SECTION;
    dwSpinCount : ULONG
  ): DWORD; stdcall;
begin
  GetProcedureAddress(_RtlSetCriticalSectionSpinCount, ntdll, 'RtlSetCriticalSectionSpinCount');
  Result := TFNRtlSetCriticalSectionSpinCount(_RtlSetCriticalSectionSpinCount)(
    lpCriticalSection, dwSpinCount
  );
end;

// Dynamic version of RtlSetCurrentDirectory_U
function  RtlSetCurrentDirectory_U(
    NewCurrentDirectory : PUNICODE_STRING
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlSetCurrentDirectory_U, ntdll, 'RtlSetCurrentDirectory_U');
  Result := TFNRtlSetCurrentDirectory_U(_RtlSetCurrentDirectory_U)(
    NewCurrentDirectory
  );
end;

// Dynamic version of RtlSetDaclSecurityDescriptor
function  RtlSetDaclSecurityDescriptor(
    SecurityDescriptor : PSECURITY_DESCRIPTOR;
    DaclPresent : BOOLEAN;
    Dacl : PACL;
    DaclDefaulted : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlSetDaclSecurityDescriptor, ntdll, 'RtlSetDaclSecurityDescriptor');
  Result := TFNRtlSetDaclSecurityDescriptor(_RtlSetDaclSecurityDescriptor)(
    SecurityDescriptor, DaclPresent, Dacl, DaclDefaulted
  );
end;

// Dynamic version of RtlSetGroupSecurityDescriptor
function  RtlSetGroupSecurityDescriptor(
    pSecurityDescriptor : PSECURITY_DESCRIPTOR;
    pGroup : PSID;
    bGroupDefaulted : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlSetGroupSecurityDescriptor, ntdll, 'RtlSetGroupSecurityDescriptor');
  Result := TFNRtlSetGroupSecurityDescriptor(_RtlSetGroupSecurityDescriptor)(
    pSecurityDescriptor, pGroup, bGroupDefaulted
  );
end;

// Dynamic version of RtlSetInformationAcl
function  RtlSetInformationAcl(
    pAcl : PACL;
    pAclInformation : PVOID;
    nInformationLength : DWORD;
    dwAclInformationClass : ACL_INFORMATION_CLASS
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlSetInformationAcl, ntdll, 'RtlSetInformationAcl');
  Result := TFNRtlSetInformationAcl(_RtlSetInformationAcl)(
    pAcl, pAclInformation, nInformationLength, dwAclInformationClass
  );
end;

// Dynamic version of RtlSetLastWin32ErrorAndNtStatusFromNtStatus
function  RtlSetLastWin32ErrorAndNtStatusFromNtStatus(
    Status : NTSTATUS
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlSetLastWin32ErrorAndNtStatusFromNtStatus, ntdll, 'RtlSetLastWin32ErrorAndNtStatusFromNtStatus');
  Result := TFNRtlSetLastWin32ErrorAndNtStatusFromNtStatus(_RtlSetLastWin32ErrorAndNtStatusFromNtStatus)(
    Status
  );
end;

// Dynamic version of RtlSetOwnerSecurityDescriptor
function  RtlSetOwnerSecurityDescriptor(
    pSecurityDescriptor : PSECURITY_DESCRIPTOR;
    pOwner : PSID;
    bOwnerDefaulted : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlSetOwnerSecurityDescriptor, ntdll, 'RtlSetOwnerSecurityDescriptor');
  Result := TFNRtlSetOwnerSecurityDescriptor(_RtlSetOwnerSecurityDescriptor)(
    pSecurityDescriptor, pOwner, bOwnerDefaulted
  );
end;

// Dynamic version of RtlSetProcessIsCritical
function  RtlSetProcessIsCritical(
    bIsCritical : BOOLEAN;
    pbOldIsCriticalValue : PBOOLEAN;
    bUnknownCanBeFalse : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlSetProcessIsCritical, ntdll, 'RtlSetProcessIsCritical');
  Result := TFNRtlSetProcessIsCritical(_RtlSetProcessIsCritical)(
    bIsCritical, pbOldIsCriticalValue, bUnknownCanBeFalse
  );
end;

// Dynamic version of RtlSetSaclSecurityDescriptor
function  RtlSetSaclSecurityDescriptor(
    pSecurityDescriptor : PSECURITY_DESCRIPTOR;
    bSaclPresent : BOOLEAN;
    pSacl : PACL;
    SaclDefaulted : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlSetSaclSecurityDescriptor, ntdll, 'RtlSetSaclSecurityDescriptor');
  Result := TFNRtlSetSaclSecurityDescriptor(_RtlSetSaclSecurityDescriptor)(
    pSecurityDescriptor, bSaclPresent, pSacl, SaclDefaulted
  );
end;

// Dynamic version of RtlSetThreadIsCritical
function  RtlSetThreadIsCritical(
    bIsCritical : BOOLEAN;
    pbOldIsCriticalValue : PBOOLEAN;
    bUnknownCanBeFalse : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlSetThreadIsCritical, ntdll, 'RtlSetThreadIsCritical');
  Result := TFNRtlSetThreadIsCritical(_RtlSetThreadIsCritical)(
    bIsCritical, pbOldIsCriticalValue, bUnknownCanBeFalse
  );
end;

// Dynamic version of RtlSizeHeap
function  RtlSizeHeap(
    hHeap : HANDLE;
    dwFlags : ULONG;
    lpMem : PVOID
  ): SIZE_T; stdcall;
begin
  GetProcedureAddress(_RtlSizeHeap, ntdll, 'RtlSizeHeap');
  Result := TFNRtlSizeHeap(_RtlSizeHeap)(
    hHeap, dwFlags, lpMem
  );
end;

// Dynamic version of RtlStringFromGUID
function  RtlStringFromGUID(
    Guid : REFGUID;
    GuidString : PUNICODE_STRING
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlStringFromGUID, ntdll, 'RtlStringFromGUID');
  Result := TFNRtlStringFromGUID(_RtlStringFromGUID)(
    Guid, GuidString
  );
end;

// Dynamic version of RtlSubAuthorityCountSid
function  RtlSubAuthorityCountSid(
    pSid : PSID
  ): PUCHAR; stdcall;
begin
  GetProcedureAddress(_RtlSubAuthorityCountSid, ntdll, 'RtlSubAuthorityCountSid');
  Result := TFNRtlSubAuthorityCountSid(_RtlSubAuthorityCountSid)(
    pSid
  );
end;

// Dynamic version of RtlSubAuthoritySid
function  RtlSubAuthoritySid(
    pSid : PSID;
    nSubAuthority : DWORD
  ): PDWORD; stdcall;
begin
  GetProcedureAddress(_RtlSubAuthoritySid, ntdll, 'RtlSubAuthoritySid');
  Result := TFNRtlSubAuthoritySid(_RtlSubAuthoritySid)(
    pSid, nSubAuthority
  );
end;

// Dynamic version of RtlSystemTimeToLocalTime
function  RtlSystemTimeToLocalTime(
    SystemTime : PLARGE_INTEGER;
    LocalTime : PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlSystemTimeToLocalTime, ntdll, 'RtlSystemTimeToLocalTime');
  Result := TFNRtlSystemTimeToLocalTime(_RtlSystemTimeToLocalTime)(
    SystemTime, LocalTime
  );
end;

// Dynamic version of RtlTimeFieldsToTime
function  RtlTimeFieldsToTime(
    TimeFields : PTIME_FIELDS;
    Time : PLARGE_INTEGER
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlTimeFieldsToTime, ntdll, 'RtlTimeFieldsToTime');
  Result := TFNRtlTimeFieldsToTime(_RtlTimeFieldsToTime)(
    TimeFields, Time
  );
end;

// Dynamic version of RtlTimeToElapsedTimeFields
procedure RtlTimeToElapsedTimeFields(
    Time : PLARGE_INTEGER;
    TimeFields : PTIME_FIELDS
  ); stdcall;
begin
  GetProcedureAddress(_RtlTimeToElapsedTimeFields, ntdll, 'RtlTimeToElapsedTimeFields');
  TFNRtlTimeToElapsedTimeFields(_RtlTimeToElapsedTimeFields)(
    Time, TimeFields
  );
end;

{.$IFNDEF JWA_INCLUDEMODE}
// Dynamic version of RtlTimeToSecondsSince1970
function  RtlTimeToSecondsSince1970(
    Time : PLARGE_INTEGER;
    ElapsedSeconds : PULONG
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlTimeToSecondsSince1970, ntdll, 'RtlTimeToSecondsSince1970');
  Result := TFNRtlTimeToSecondsSince1970(_RtlTimeToSecondsSince1970)(
    Time, ElapsedSeconds
  );
end;
{.$ENDIF JWA_INCLUDEMODE}

// Dynamic version of RtlTimeToSecondsSince1980
function  RtlTimeToSecondsSince1980(
    Time : PLARGE_INTEGER;
    ElapsedSeconds : PULONG
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlTimeToSecondsSince1980, ntdll, 'RtlTimeToSecondsSince1980');
  Result := TFNRtlTimeToSecondsSince1980(_RtlTimeToSecondsSince1980)(
    Time, ElapsedSeconds
  );
end;

// Dynamic version of RtlTimeToTimeFields
procedure RtlTimeToTimeFields(
    Time : PLARGE_INTEGER;
    TimeFields : PTIME_FIELDS
  ); stdcall;
begin
  GetProcedureAddress(_RtlTimeToTimeFields, ntdll, 'RtlTimeToTimeFields');
  TFNRtlTimeToTimeFields(_RtlTimeToTimeFields)(
    Time, TimeFields
  );
end;

// Dynamic version of RtlTryEnterCriticalSection
function  RtlTryEnterCriticalSection(
    lpCriticalSection : PRTL_CRITICAL_SECTION
  ): BOOL; stdcall;
begin
  GetProcedureAddress(_RtlTryEnterCriticalSection, ntdll, 'RtlTryEnterCriticalSection');
  Result := TFNRtlTryEnterCriticalSection(_RtlTryEnterCriticalSection)(
    lpCriticalSection
  );
end;

// Dynamic version of RtlUnicodeStringToAnsiSize
function  RtlUnicodeStringToAnsiSize(
    UnicodeString : PUNICODE_STRING
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlUnicodeStringToAnsiSize, ntdll, 'RtlUnicodeStringToAnsiSize');
  Result := TFNRtlUnicodeStringToAnsiSize(_RtlUnicodeStringToAnsiSize)(
    UnicodeString
  );
end;

{.$IFNDEF JWA_INCLUDEMODE}
// Dynamic version of RtlUnicodeStringToAnsiString
function  RtlUnicodeStringToAnsiString(
    DestinationString : PANSI_STRING;
    SourceString : PUNICODE_STRING;
    AllocateDestinationString : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlUnicodeStringToAnsiString, ntdll, 'RtlUnicodeStringToAnsiString');
  Result := TFNRtlUnicodeStringToAnsiString(_RtlUnicodeStringToAnsiString)(
    DestinationString, SourceString, AllocateDestinationString
  );
end;
{.$ENDIF JWA_INCLUDEMODE}

// Dynamic version of RtlUnicodeStringToCountedOemString
function  RtlUnicodeStringToCountedOemString(
    DestinationString : POEM_STRING;
    SourceString : PUNICODE_STRING;
    AllocateDestinationString : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlUnicodeStringToCountedOemString, ntdll, 'RtlUnicodeStringToCountedOemString');
  Result := TFNRtlUnicodeStringToCountedOemString(_RtlUnicodeStringToCountedOemString)(
    DestinationString, SourceString, AllocateDestinationString
  );
end;

// Dynamic version of RtlUnicodeStringToInteger
function  RtlUnicodeStringToInteger(
    Str : PUNICODE_STRING;
    Base : ULONG;
    Value : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlUnicodeStringToInteger, ntdll, 'RtlUnicodeStringToInteger');
  Result := TFNRtlUnicodeStringToInteger(_RtlUnicodeStringToInteger)(
    Str, Base, Value
  );
end;

// Dynamic version of RtlUnicodeStringToOemSize
function  RtlUnicodeStringToOemSize(
    UnicodeString : PUNICODE_STRING
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlUnicodeStringToOemSize, ntdll, 'RtlUnicodeStringToOemSize');
  Result := TFNRtlUnicodeStringToOemSize(_RtlUnicodeStringToOemSize)(
    UnicodeString
  );
end;

{.$IFNDEF JWA_INCLUDEMODE}
// Dynamic version of RtlUnicodeStringToOemString
function  RtlUnicodeStringToOemString(
    DestinationString : POEM_STRING;
    SourceString : PCUNICODE_STRING;
    AllocateDestinationString : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlUnicodeStringToOemString, ntdll, 'RtlUnicodeStringToOemString');
  Result := TFNRtlUnicodeStringToOemString(_RtlUnicodeStringToOemString)(
    DestinationString, SourceString, AllocateDestinationString
  );
end;


// Dynamic version of RtlUnicodeToMultiByteSize
function  RtlUnicodeToMultiByteSize(
    BytesInMultiByteString : PULONG;
    UnicodeString : PWSTR;
    BytesInUnicodeString : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlUnicodeToMultiByteSize, ntdll, 'RtlUnicodeToMultiByteSize');
  Result := TFNRtlUnicodeToMultiByteSize(_RtlUnicodeToMultiByteSize)(
    BytesInMultiByteString, UnicodeString, BytesInUnicodeString
  );
end;
{.$ENDIF JWA_INCLUDEMODE}

// Dynamic version of RtlUniform
function  RtlUniform(
    Seed : PULONG
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlUniform, ntdll, 'RtlUniform');
  Result := TFNRtlUniform(_RtlUniform)(
    Seed
  );
end;

// Dynamic version of RtlUnwind
procedure RtlUnwind(
    TargetFrame : PVOID;
    TargetIp : PVOID;
    ExceptionRecord : PEXCEPTION_RECORD;
    ReturnValue : PVOID
  ); stdcall;
begin
  GetProcedureAddress(_RtlUnwind, ntdll, 'RtlUnwind');
  TFNRtlUnwind(_RtlUnwind)(
    TargetFrame, TargetIp, ExceptionRecord, ReturnValue
  );
end;

// Dynamic version of RtlUpcaseUnicodeChar
function  RtlUpcaseUnicodeChar(
    SourceCharacter : WCHAR
  ): WCHAR; stdcall;
begin
  GetProcedureAddress(_RtlUpcaseUnicodeChar, ntdll, 'RtlUpcaseUnicodeChar');
  Result := TFNRtlUpcaseUnicodeChar(_RtlUpcaseUnicodeChar)(
    SourceCharacter
  );
end;

// Dynamic version of RtlUpcaseUnicodeString
function  RtlUpcaseUnicodeString(
    DestinationString : PUNICODE_STRING;
    SourceString : PUNICODE_STRING;
    AllocateDestinationString : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlUpcaseUnicodeString, ntdll, 'RtlUpcaseUnicodeString');
  Result := TFNRtlUpcaseUnicodeString(_RtlUpcaseUnicodeString)(
    DestinationString, SourceString, AllocateDestinationString
  );
end;

// Dynamic version of RtlUpcaseUnicodeStringToAnsiString
function  RtlUpcaseUnicodeStringToAnsiString(
    DestinationString : PSTRING;
    SourceString : PUNICODE_STRING;
    AllocateDestinationString : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlUpcaseUnicodeStringToAnsiString, ntdll, 'RtlUpcaseUnicodeStringToAnsiString');
  Result := TFNRtlUpcaseUnicodeStringToAnsiString(_RtlUpcaseUnicodeStringToAnsiString)(
    DestinationString, SourceString, AllocateDestinationString
  );
end;

// Dynamic version of RtlUpcaseUnicodeStringToCountedOemString
function  RtlUpcaseUnicodeStringToCountedOemString(
    DestinationString : PSTRING;
    SourceString : PUNICODE_STRING;
    AllocateDestinationString : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlUpcaseUnicodeStringToCountedOemString, ntdll, 'RtlUpcaseUnicodeStringToCountedOemString');
  Result := TFNRtlUpcaseUnicodeStringToCountedOemString(_RtlUpcaseUnicodeStringToCountedOemString)(
    DestinationString, SourceString, AllocateDestinationString
  );
end;

// Dynamic version of RtlUpcaseUnicodeStringToOemString
function  RtlUpcaseUnicodeStringToOemString(
    DestinationString : PSTRING;
    SourceString : PUNICODE_STRING;
    AllocateDestinationString : BOOLEAN
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlUpcaseUnicodeStringToOemString, ntdll, 'RtlUpcaseUnicodeStringToOemString');
  Result := TFNRtlUpcaseUnicodeStringToOemString(_RtlUpcaseUnicodeStringToOemString)(
    DestinationString, SourceString, AllocateDestinationString
  );
end;

// Dynamic version of RtlUpcaseUnicodeToMultiByteN
function  RtlUpcaseUnicodeToMultiByteN(
    MbString : PAnsiChar;
    MbSize : ULONG;
    var ResultSize : ULONG;
    UnicodeString : PWSTR;
    UnicodeSize : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlUpcaseUnicodeToMultiByteN, ntdll, 'RtlUpcaseUnicodeToMultiByteN');
  Result := TFNRtlUpcaseUnicodeToMultiByteN(_RtlUpcaseUnicodeToMultiByteN)(
    MbString, MbSize, ResultSize, UnicodeString, UnicodeSize
  );
end;

// Dynamic version of RtlUpcaseUnicodeToOemN
function  RtlUpcaseUnicodeToOemN(
    OemString : PAnsiChar;
    OemSize : ULONG;
    var ResultSize : ULONG;
    UnicodeString : PWSTR;
    UnicodeSize : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlUpcaseUnicodeToOemN, ntdll, 'RtlUpcaseUnicodeToOemN');
  Result := TFNRtlUpcaseUnicodeToOemN(_RtlUpcaseUnicodeToOemN)(
    OemString, OemSize, ResultSize, UnicodeString, UnicodeSize
  );
end;

// Dynamic version of RtlUpperChar
function  RtlUpperChar(
    Character : AnsiChar
  ): AnsiChar; stdcall;
begin
  GetProcedureAddress(_RtlUpperChar, ntdll, 'RtlUpperChar');
  Result := TFNRtlUpperChar(_RtlUpperChar)(
    Character
  );
end;

// Dynamic version of RtlUpperString
procedure RtlUpperString(
    DestinationString : PSTRING;
    SourceString : PSTRING
  ); stdcall;
begin
  GetProcedureAddress(_RtlUpperString, ntdll, 'RtlUpperString');
  TFNRtlUpperString(_RtlUpperString)(
    DestinationString, SourceString
  );
end;

// Dynamic version of RtlValidAcl
function  RtlValidAcl(
    Acl : PACL
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlValidAcl, ntdll, 'RtlValidAcl');
  Result := TFNRtlValidAcl(_RtlValidAcl)(
    Acl
  );
end;

// Dynamic version of RtlValidRelativeSecurityDescriptor
function  RtlValidRelativeSecurityDescriptor(
    SecurityDescriptorInput : PSECURITY_DESCRIPTOR;
    SecurityDescriptorLength : ULONG;
    RequiredInformation : SECURITY_INFORMATION
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlValidRelativeSecurityDescriptor, ntdll, 'RtlValidRelativeSecurityDescriptor');
  Result := TFNRtlValidRelativeSecurityDescriptor(_RtlValidRelativeSecurityDescriptor)(
    SecurityDescriptorInput, SecurityDescriptorLength, RequiredInformation
  );
end;

// Dynamic version of RtlValidSecurityDescriptor
function  RtlValidSecurityDescriptor(
    SecurityDescriptor : PSECURITY_DESCRIPTOR
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlValidSecurityDescriptor, ntdll, 'RtlValidSecurityDescriptor');
  Result := TFNRtlValidSecurityDescriptor(_RtlValidSecurityDescriptor)(
    SecurityDescriptor
  );
end;

// Dynamic version of RtlValidSid
function  RtlValidSid(
    pSid : PSID
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlValidSid, ntdll, 'RtlValidSid');
  Result := TFNRtlValidSid(_RtlValidSid)(
    pSid
  );
end;

// Dynamic version of RtlValidateHeap
function  RtlValidateHeap(
    hHeap : HANDLE;
    dwFlags : ULONG;
    lpMem : LPCVOID
  ): BOOL; stdcall;
begin
  GetProcedureAddress(_RtlValidateHeap, ntdll, 'RtlValidateHeap');
  Result := TFNRtlValidateHeap(_RtlValidateHeap)(
    hHeap, dwFlags, lpMem
  );
end;

// Dynamic version of RtlValidateUnicodeString
function  RtlValidateUnicodeString(
    dwMustBeNull : ULONG;
    ValidateThis : PUNICODE_STRING
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlValidateUnicodeString, ntdll, 'RtlValidateUnicodeString');
  Result := TFNRtlValidateUnicodeString(_RtlValidateUnicodeString)(
    dwMustBeNull, ValidateThis
  );
end;

// Dynamic version of RtlVerifyVersionInfo
function  RtlVerifyVersionInfo(
    VersionInfo : PRTL_OSVERSIONINFOEXW;
    TypeMask : ULONG;
    ConditionMask : ULONGLONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlVerifyVersionInfo, ntdll, 'RtlVerifyVersionInfo');
  Result := TFNRtlVerifyVersionInfo(_RtlVerifyVersionInfo)(
    VersionInfo, TypeMask, ConditionMask
  );
end;

// Dynamic version of RtlVolumeDeviceToDosName
function  RtlVolumeDeviceToDosName(
    VolumeDeviceObject : PVOID;
    DosName : PUNICODE_STRING
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlVolumeDeviceToDosName, ntdll, 'RtlVolumeDeviceToDosName');
  Result := TFNRtlVolumeDeviceToDosName(_RtlVolumeDeviceToDosName)(
    VolumeDeviceObject, DosName
  );
end;

// Dynamic version of RtlWriteRegistryValue
function  RtlWriteRegistryValue(
    RelativeTo : ULONG;
    Path : LPCWSTR;
    ValueName : LPCWSTR;
    ValueType : ULONG;
    ValueData : PVOID;
    ValueLength : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlWriteRegistryValue, ntdll, 'RtlWriteRegistryValue');
  Result := TFNRtlWriteRegistryValue(_RtlWriteRegistryValue)(
    RelativeTo, Path, ValueName, ValueType, ValueData, ValueLength
  );
end;

// Dynamic version of RtlZeroHeap
function  RtlZeroHeap(
    hHeap : HANDLE;
    dwFlags : ULONG
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlZeroHeap, ntdll, 'RtlZeroHeap');
  Result := TFNRtlZeroHeap(_RtlZeroHeap)(
    hHeap, dwFlags
  );
end;

// Dynamic version of RtlZeroMemory
procedure RtlZeroMemory(
    Destination : PVOID;
    Length : SIZE_T
  ); stdcall;
begin
  GetProcedureAddress(_RtlZeroMemory, ntdll, 'RtlZeroMemory');
  TFNRtlZeroMemory(_RtlZeroMemory)(
    Destination, Length
  );
end;

// Dynamic version of RtlpNotOwnerCriticalSection
function  RtlpNotOwnerCriticalSection(
    lpCriticalSection : PRTL_CRITICAL_SECTION
  ): BOOLEAN; stdcall;
begin
  GetProcedureAddress(_RtlpNotOwnerCriticalSection, ntdll, 'RtlpNotOwnerCriticalSection');
  Result := TFNRtlpNotOwnerCriticalSection(_RtlpNotOwnerCriticalSection)(
    lpCriticalSection
  );
end;

// Dynamic version of RtlpNtCreateKey
function  RtlpNtCreateKey(
    KeyHandle : PHANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    Unused1 : ULONG;
    Unused2 : ULONG;
    Disposition : PULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlpNtCreateKey, ntdll, 'RtlpNtCreateKey');
  Result := TFNRtlpNtCreateKey(_RtlpNtCreateKey)(
    KeyHandle, DesiredAccess, ObjectAttributes, Unused1, Unused2, Disposition
  );
end;

// Dynamic version of RtlpNtEnumerateSubKey
function  RtlpNtEnumerateSubKey(
    KeyHandle : HANDLE;
    SubKeyName : PUNICODE_STRING;
    Index : ULONG;
    Unused1 : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlpNtEnumerateSubKey, ntdll, 'RtlpNtEnumerateSubKey');
  Result := TFNRtlpNtEnumerateSubKey(_RtlpNtEnumerateSubKey)(
    KeyHandle, SubKeyName, Index, Unused1
  );
end;

// Dynamic version of RtlpNtMakeTemporaryKey
function  RtlpNtMakeTemporaryKey(
    KeyHandle : HANDLE
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlpNtMakeTemporaryKey, ntdll, 'RtlpNtMakeTemporaryKey');
  Result := TFNRtlpNtMakeTemporaryKey(_RtlpNtMakeTemporaryKey)(
    KeyHandle
  );
end;

// Dynamic version of RtlpNtOpenKey
function  RtlpNtOpenKey(
    KeyHandle : HANDLE;
    DesiredAccess : ACCESS_MASK;
    ObjectAttributes : POBJECT_ATTRIBUTES;
    Unused : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlpNtOpenKey, ntdll, 'RtlpNtOpenKey');
  Result := TFNRtlpNtOpenKey(_RtlpNtOpenKey)(
    KeyHandle, DesiredAccess, ObjectAttributes, Unused
  );
end;

// Dynamic version of RtlpNtQueryValueKey
function  RtlpNtQueryValueKey(
    KeyHandle : HANDLE;
    Type_ : PULONG;
    Data : PVOID;
    DataSize : PULONG;
    Unused : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlpNtQueryValueKey, ntdll, 'RtlpNtQueryValueKey');
  Result := TFNRtlpNtQueryValueKey(_RtlpNtQueryValueKey)(
    KeyHandle, Type_, Data, DataSize, Unused
  );
end;

// Dynamic version of RtlpNtSetValueKey
function  RtlpNtSetValueKey(
    KeyHandle : HANDLE;
    Type_ : ULONG;
    Data : PVOID;
    DataSize : ULONG
  ): NTSTATUS; stdcall;
begin
  GetProcedureAddress(_RtlpNtSetValueKey, ntdll, 'RtlpNtSetValueKey');
  Result := TFNRtlpNtSetValueKey(_RtlpNtSetValueKey)(
    KeyHandle, Type_, Data, DataSize
  );
end;

// Dynamic version of RtlxAnsiStringToUnicodeSize
function  RtlxAnsiStringToUnicodeSize(
    AnsiString : PANSI_STRING
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlxAnsiStringToUnicodeSize, ntdll, 'RtlxAnsiStringToUnicodeSize');
  Result := TFNRtlxAnsiStringToUnicodeSize(_RtlxAnsiStringToUnicodeSize)(
    AnsiString
  );
end;

// Dynamic version of RtlxOemStringToUnicodeSize
function  RtlxOemStringToUnicodeSize(
    AnsiString : POEM_STRING
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlxOemStringToUnicodeSize, ntdll, 'RtlxOemStringToUnicodeSize');
  Result := TFNRtlxOemStringToUnicodeSize(_RtlxOemStringToUnicodeSize)(
    AnsiString
  );
end;

// Dynamic version of RtlxUnicodeStringToAnsiSize
function  RtlxUnicodeStringToAnsiSize(
    UnicodeString : PUNICODE_STRING
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlxUnicodeStringToAnsiSize, ntdll, 'RtlxUnicodeStringToAnsiSize');
  Result := TFNRtlxUnicodeStringToAnsiSize(_RtlxUnicodeStringToAnsiSize)(
    UnicodeString
  );
end;

// Dynamic version of RtlxUnicodeStringToOemSize
function  RtlxUnicodeStringToOemSize(
    UnicodeString : PUNICODE_STRING
  ): ULONG; stdcall;
begin
  GetProcedureAddress(_RtlxUnicodeStringToOemSize, ntdll, 'RtlxUnicodeStringToOemSize');
  Result := TFNRtlxUnicodeStringToOemSize(_RtlxUnicodeStringToOemSize)(
    UnicodeString
  );
end;

{$IFNDEF JWA_INCLUDEMODE}
// Dynamic version of VerSetConditionMask
function  VerSetConditionMask(
    ConditionMask : ULONGLONG;
    dwTypeMask : DWORD;
    Condition : BYTE
  ): ULONGLONG; stdcall;
begin
  GetProcedureAddress(_VerSetConditionMask, ntdll, 'VerSetConditionMask');
  Result := TFNVerSetConditionMask(_VerSetConditionMask)(
    ConditionMask, dwTypeMask, Condition
  );
end;
{$ENDIF JWA_INCLUDEMODE}

{$ENDIF RTDL}

(*
Function forwarders which are not implemented by this unit
because they are available only on the 64bit editions of
Windows XP and Windows 2003 Server.

[KERNEL32.dll]RtlCaptureContext -> NTDLL.RtlCaptureContext
[KERNEL32.dll]RtlCaptureStackBackTrace -> NTDLL.RtlCaptureStackBackTrace

Usually the Kernel32 functions are documented in the Platform SDK, so knowing
of these function forwarders gives you the chance to find out the prototype of
the respective Native API to which the call is forwarded.

The following usermode Native APIs are not included in this unit:
-----------------------------------------------------------------
CsrAllocateCaptureBuffer [NT3, NT4, W2K, WXP, 2K3]
CsrAllocateMessagePointer [NT3, NT4, W2K, WXP, 2K3]
CsrCaptureMessageBuffer [NT3, NT4, W2K, WXP, 2K3]
CsrCaptureMessageMultiUnicodeStringsInPlace [WXP, 2K3]
CsrCaptureMessageString [NT3, NT4, W2K, WXP, 2K3]
CsrCaptureTimeout [NT3, NT4, W2K, WXP, 2K3]
CsrClientCallServer [NT3, NT4, W2K, WXP, 2K3]
CsrClientConnectToServer [NT3, NT4, W2K, WXP, 2K3]
CsrFreeCaptureBuffer [NT3, NT4, W2K, WXP, 2K3]
CsrIdentifyAlertableThread [NT3, NT4, W2K, WXP, 2K3]
CsrNewThread [NT3, NT4, W2K, WXP, 2K3]
CsrProbeForRead [NT3, NT4, W2K, WXP, 2K3]
CsrProbeForWrite [NT3, NT4, W2K, WXP, 2K3]
CsrSetPriorityClass [NT3, NT4, W2K, WXP, 2K3]
DbgPrintEx [WXP, 2K3]
DbgPrintReturnControlC [W2K, WXP, 2K3]
DbgPrompt [NT3, NT4, W2K, WXP, 2K3]
DbgSsHandleKmApiMsg [NT3, NT4, W2K]
DbgSsInitialize [NT3, NT4, W2K]
DbgUiConnectToDbg [NT3, NT4, W2K, WXP, 2K3]
DbgUiContinue [NT3, NT4, W2K, WXP, 2K3]
DbgUiConvertStateChangeStructure [WXP, 2K3]
DbgUiDebugActiveProcess [WXP, 2K3]
DbgUiGetThreadDebugObject [WXP, 2K3]
DbgUiIssueRemoteBreakin [WXP, 2K3]
DbgUiRemoteBreakin [WXP, 2K3]
DbgUiSetThreadDebugObject [WXP, 2K3]
DbgUiStopDebugging [WXP, 2K3]
DbgUiWaitStateChange [NT3, NT4, W2K, WXP, 2K3]
DbgUserBreakPoint [NT3, NT4, W2K, WXP, 2K3]
EtwControlTraceA [2K3]
EtwControlTraceW [2K3]
EtwCreateTraceInstanceId [2K3]
EtwEnableTrace [2K3]
EtwEnumerateTraceGuids [2K3]
EtwFlushTraceA [2K3]
EtwFlushTraceW [2K3]
EtwGetTraceEnableFlags [2K3]
EtwGetTraceEnableLevel [2K3]
EtwGetTraceLoggerHandle [2K3]
EtwNotificationRegistrationA [2K3]
EtwNotificationRegistrationW [2K3]
EtwQueryAllTracesA [2K3]
EtwQueryAllTracesW [2K3]
EtwQueryTraceA [2K3]
EtwQueryTraceW [2K3]
EtwReceiveNotificationsA [2K3]
EtwReceiveNotificationsW [2K3]
EtwRegisterTraceGuidsA [2K3]
EtwRegisterTraceGuidsW [2K3]
EtwStartTraceA [2K3]
EtwStartTraceW [2K3]
EtwStopTraceA [2K3]
EtwStopTraceW [2K3]
EtwTraceEvent [2K3]
EtwTraceEventInstance [2K3]
EtwTraceMessage [2K3]
EtwTraceMessageVa [2K3]
EtwUnregisterTraceGuids [2K3]
EtwUpdateTraceA [2K3]
EtwUpdateTraceW [2K3]
EtwpGetTraceBuffer [2K3]
EtwpSetHWConfigFunction [2K3]
KiUserApcDispatcher [NT3, NT4, W2K, WXP, 2K3]
KiUserCallbackDispatcher [NT3, NT4, W2K, WXP, 2K3]
KiUserExceptionDispatcher [NT3, NT4, W2K, WXP, 2K3]
LdrAccessOutOfProcessResource [WXP, 2K3]
LdrAddRefDll [WXP, 2K3]
LdrCreateOutOfProcessImage [WXP, 2K3]
LdrDestroyOutOfProcessImage [WXP, 2K3]
LdrEnumResources [NT3, NT4, W2K, WXP, 2K3]
LdrEnumerateLoadedModules [WXP, 2K3]
LdrFindCreateProcessManifest [WXP, 2K3]
LdrFindEntryForAddress [NT3, NT4, W2K, WXP, 2K3]
LdrFindResourceDirectory_U [NT3, NT4, W2K, WXP, 2K3]
LdrFindResourceEx_U [WXP, 2K3]
LdrFindResource_U [NT3, NT4, W2K, WXP, 2K3]
LdrFlushAlternateResourceModules [W2K, WXP, 2K3]
LdrGetDllHandleEx [WXP, 2K3]
LdrHotPatchRoutine [2K3]
LdrInitShimEngineDynamic [WXP, 2K3]
LdrInitializeThunk [NT3, NT4, W2K, WXP, 2K3]
LdrLoadAlternateResourceModule [W2K, WXP, 2K3]
LdrLockLoaderLock [WXP, 2K3]
LdrProcessRelocationBlock [NT3, NT4, W2K, WXP, 2K3]
LdrQueryImageFileExecutionOptionsEx [2K3]
LdrSetAppCompatDllRedirectionCallback [WXP, 2K3]
LdrSetDllManifestProber [WXP, 2K3]
LdrUnloadAlternateResourceModule [W2K, WXP, 2K3]
LdrUnlockLoaderLock [WXP, 2K3]
LdrVerifyImageMatchesChecksum [NT3, NT4, W2K, WXP, 2K3]
NPXEMULATORTABLE [NT3, NT4, W2K]
NlsAnsiCodePage [NT4, W2K, WXP, 2K3]
NlsMbCodePageTag [NT3, NT4, W2K, WXP, 2K3]
NlsMbOemCodePageTag [NT3, NT4, W2K, WXP, 2K3]
NtAddBootEntry [WXP, 2K3]
NtAddDriverEntry [2K3]
NtApphelpCacheControl [2K3]
NtCompactKeys [WXP, 2K3]
NtCompareTokens [WXP, 2K3]
NtCompressKey [WXP, 2K3]
NtCreateDebugObject [WXP, 2K3]
NtCreateJobSet [WXP, 2K3]
NtCreateKeyedEvent [WXP, 2K3]
NtCreateProcessEx [WXP, 2K3]
NtDebugContinue [WXP, 2K3]
NtDeleteBootEntry [WXP, 2K3]
NtDeleteDriverEntry [2K3]
NtEnumerateBootEntries [WXP, 2K3]
NtEnumerateDriverEntries [2K3]
NtEnumerateSystemEnvironmentValuesEx [WXP, 2K3]
NtIsProcessInJob [WXP, 2K3]
NtLoadKeyEx [2K3]
NtLockProductActivationKeys [WXP, 2K3]
NtLockRegistryKey [WXP, 2K3]
NtModifyBootEntry [WXP, 2K3]
NtModifyDriverEntry [2K3]
NtOpenKeyedEvent [WXP, 2K3]
NtOpenProcessTokenEx [WXP, 2K3]
NtOpenThreadTokenEx [WXP, 2K3]
NtQueryBootEntryOrder [WXP, 2K3]
NtQueryBootOptions [WXP, 2K3]
NtQueryDebugFilterState [WXP, 2K3]
NtQueryDriverEntryOrder [2K3]
NtQueryOpenSubKeysEx [2K3]
NtQuerySystemEnvironmentValueEx [WXP, 2K3]
NtReleaseKeyedEvent [WXP, 2K3]
NtRenameKey [WXP, 2K3]
NtSetBootEntryOrder [WXP, 2K3]
NtSetBootOptions [WXP, 2K3]
NtSetDebugFilterState [WXP, 2K3]
NtSetDriverEntryOrder [2K3]
NtSetEventBoostPriority [WXP, 2K3]
NtSetInformationDebugObject [WXP, 2K3]
NtSetSystemEnvironmentValueEx [WXP, 2K3]
NtTraceEvent [WXP, 2K3]
NtTranslateFilePath [WXP, 2K3]
NtUnloadKey2 [2K3]
NtUnloadKeyEx [WXP, 2K3]
NtWaitForDebugEvent [WXP, 2K3]
NtWaitForKeyedEvent [WXP, 2K3]
PfxFindPrefix [NT3, NT4, W2K, WXP, 2K3]
PfxInitialize [NT3, NT4, W2K, WXP, 2K3]
PfxInsertPrefix [NT3, NT4, W2K, WXP, 2K3]
PfxRemovePrefix [NT3, NT4, W2K, WXP, 2K3]
PropertyLengthAsVariant [NT4, W2K, WXP, 2K3]
RestoreEm87Context [NT3, NT4, W2K, WXP, 2K3]
RtlAbortRXact [NT3, NT4, W2K, WXP, 2K3]
RtlAcquireResourceExclusive [NT3, NT4, W2K, WXP, 2K3]
RtlAcquireResourceShared [NT3, NT4, W2K, WXP, 2K3]
RtlActivateActivationContext [WXP, 2K3]
RtlActivateActivationContextEx [WXP, 2K3]
RtlActivateActivationContextUnsafeFast [WXP, 2K3]
RtlAddAccessAllowedObjectAce [W2K, WXP, 2K3]
RtlAddAccessDeniedObjectAce [W2K, WXP, 2K3]
RtlAddActionToRXact [NT3, NT4, W2K, WXP, 2K3]
RtlAddAtomToAtomTable [NT4, W2K, WXP, 2K3]
RtlAddAttributeActionToRXact [NT3, NT4, W2K, WXP, 2K3]
RtlAddAuditAccessObjectAce [W2K, WXP, 2K3]
RtlAddCompoundAce [NT4, W2K, WXP, 2K3]
RtlAddRefActivationContext [WXP, 2K3]
RtlAddRefMemoryStream [WXP, 2K3]
RtlAddressInSectionTable [WXP, 2K3]
RtlAllocateHandle [NT4, W2K, WXP, 2K3]
RtlAppendPathElement [WXP, 2K3]
RtlApplicationVerifierStop [WXP, 2K3]
RtlApplyRXact [NT3, NT4, W2K, WXP, 2K3]
RtlApplyRXactNoFlush [NT3, NT4, W2K, WXP, 2K3]
RtlAssert2 [WXP]
RtlCallbackLpcClient [W2K]
RtlCancelTimer [W2K, WXP, 2K3]
RtlCaptureStackBackTrace [NT3, NT4, W2K, WXP, 2K3]
RtlCaptureStackContext [WXP, 2K3]
RtlCheckProcessParameters [WXP, 2K3]
RtlCloneMemoryStream [WXP, 2K3]
RtlCommitMemoryStream [WXP, 2K3]
RtlCompressBuffer [NT3, NT4, W2K, WXP, 2K3]
RtlComputeCrc32 [WXP, 2K3]
RtlComputeImportTableHash [WXP, 2K3]
RtlComputePrivatizedDllName_U [WXP, 2K3]
RtlConsoleMultiByteToUnicodeN [NT3, NT4, W2K, WXP, 2K3]
RtlConvertExclusiveToShared [NT3, NT4, W2K, WXP, 2K3]
RtlConvertPropertyToVariant [NT4, W2K, WXP, 2K3]
RtlConvertSharedToExclusive [NT3, NT4, W2K, WXP, 2K3]
RtlConvertToAutoInheritSecurityObject [W2K, WXP, 2K3]
RtlConvertUiListToApiList [NT3, NT4, W2K, WXP, 2K3]
RtlConvertVariantToProperty [NT4, W2K, WXP, 2K3]
RtlCopyLuidAndAttributesArray [NT3, NT4, W2K, WXP, 2K3]
RtlCopyMappedMemory [2K3]
RtlCopyMemoryStreamTo [WXP, 2K3]
RtlCopyOutOfProcessMemoryStreamTo [WXP, 2K3]
RtlCopySidAndAttributesArray [NT3, NT4, W2K, WXP, 2K3]
RtlCreateActivationContext [WXP, 2K3]
RtlCreateAndSetSD [NT3, NT4, W2K, WXP, 2K3]
RtlCreateAtomTable [NT4, W2K, WXP, 2K3]
RtlCreateBootStatusDataFile [WXP, 2K3]
RtlCreateEnvironment [NT3, NT4, W2K, WXP, 2K3]
RtlCreateLpcServer [W2K]
RtlCreateSystemVolumeInformationFolder [WXP, 2K3]
RtlCreateTagHeap [NT3, NT4, W2K, WXP, 2K3]
RtlCreateTimer [W2K, WXP, 2K3]
RtlCreateTimerQueue [W2K, WXP, 2K3]
RtlCreateUserSecurityObject [NT3, NT4, W2K, WXP, 2K3]
RtlCustomCPToUnicodeN [NT3, NT4, W2K, WXP, 2K3]
RtlDeactivateActivationContext [WXP, 2K3]
RtlDeactivateActivationContextUnsafeFast [WXP, 2K3]
RtlDebugPrintTimes [W2K, WXP, 2K3]
RtlDecompressBuffer [NT3, NT4, W2K, WXP, 2K3]
RtlDecompressFragment [NT3, NT4, W2K, WXP, 2K3]
RtlDefaultNpAcl [W2K, WXP, 2K3]
RtlDeleteAtomFromAtomTable [NT4, W2K, WXP, 2K3]
RtlDeleteElementGenericTable [NT3, NT4, W2K, WXP, 2K3]
RtlDeleteElementGenericTableAvl [WXP, 2K3]
RtlDeleteNoSplay [NT4, W2K, WXP, 2K3]
RtlDeleteResource [NT3, NT4, W2K, WXP, 2K3]
RtlDeleteSecurityObject [NT3, NT4, W2K, WXP, 2K3]
RtlDeleteTimer [W2K, WXP, 2K3]
RtlDeleteTimerQueue [W2K, WXP, 2K3]
RtlDeleteTimerQueueEx [W2K, WXP, 2K3]
RtlDeregisterWait [W2K, WXP, 2K3]
RtlDeregisterWaitEx [W2K, WXP, 2K3]
RtlDestroyAtomTable [NT4, W2K, WXP, 2K3]
RtlDestroyEnvironment [NT3, NT4, W2K, WXP, 2K3]
RtlDestroyHandleTable [NT4, W2K, WXP, 2K3]
RtlDllShutdownInProgress [WXP, 2K3]
RtlDosApplyFileIsolationRedirection_Ustr [WXP, 2K3]
RtlDosPathNameToRelativeNtPathName_U [2K3]
RtlDosSearchPath_Ustr [WXP, 2K3]
RtlDumpResource [NT3, NT4, W2K, WXP, 2K3]
RtlEmptyAtomTable [NT4, W2K, WXP, 2K3]
RtlEnumProcessHeaps [NT3, NT4, W2K, WXP, 2K3]
RtlEnumerateGenericTable [NT3, NT4, W2K, WXP, 2K3]
RtlEnumerateGenericTableAvl [WXP, 2K3]
RtlEnumerateGenericTableLikeADirectory [WXP, 2K3]
RtlEnumerateGenericTableWithoutSplaying [NT3, NT4, W2K, WXP, 2K3]
RtlEnumerateGenericTableWithoutSplayingAvl [WXP, 2K3]
RtlExitUserThread [WXP, 2K3]
RtlExtendHeap [NT3, NT4, W2K, WXP, 2K3]
RtlFinalReleaseOutOfProcessMemoryStream [WXP, 2K3]
RtlFindActivationContextSectionGuid [WXP, 2K3]
RtlFindActivationContextSectionString [WXP, 2K3]
RtlFindClearRuns [WXP, 2K3]
RtlFindMessage [NT3, NT4, W2K, WXP, 2K3]
RtlFirstEntrySList [WXP, 2K3]
RtlFlushSecureMemoryCache [WXP, 2K3]
RtlFormatMessage [NT3, NT4, W2K, WXP, 2K3]
RtlFreeHandle [NT4, W2K, WXP, 2K3]
RtlFreeThreadActivationContextStack [WXP, 2K3]
RtlFreeUserThreadStack [NT4, W2K, WXP, 2K3]
RtlGenerate8dot3Name [NT3, NT4, W2K, WXP, 2K3]
RtlGetActiveActivationContext [WXP, 2K3]
RtlGetCompressionWorkSpaceSize [NT3, NT4, W2K, WXP, 2K3]
RtlGetElementGenericTable [NT3, NT4, W2K, WXP, 2K3]
RtlGetElementGenericTableAvl [WXP, 2K3]
RtlGetFrame [WXP, 2K3]
RtlGetFullPathName_UstrEx [2K3]
RtlGetLengthWithoutLastFullDosOrNtPathElement [WXP, 2K3]
RtlGetLengthWithoutTrailingPathSeperators [WXP, 2K3]
RtlGetNativeSystemInformation [WXP, 2K3]
RtlGetSecurityDescriptorRMControl [W2K, WXP, 2K3]
RtlGetSetBootStatusData [WXP, 2K3]
RtlGetThreadErrorMode [2K3]
RtlGetUnloadEventTrace [2K3]
RtlGetUserInfoHeap [NT3, NT4, W2K, WXP, 2K3]
RtlHashUnicodeString [WXP, 2K3]
RtlImpersonateLpcClient [W2K]
RtlInitCodePageTable [NT3, NT4, W2K, WXP, 2K3]
RtlInitMemoryStream [WXP, 2K3]
RtlInitNlsTables [NT3, NT4, W2K, WXP, 2K3]
RtlInitOutOfProcessMemoryStream [WXP, 2K3]
RtlInitializeAtomPackage [NT4, W2K, WXP, 2K3]
RtlInitializeContext [NT3, NT4, W2K, WXP, 2K3]
RtlInitializeGenericTable [NT3, NT4, W2K, WXP, 2K3]
RtlInitializeGenericTableAvl [WXP, 2K3]
RtlInitializeHandleTable [NT4, W2K, WXP, 2K3]
RtlInitializeRXact [NT3, NT4, W2K, WXP, 2K3]
RtlInitializeResource [NT3, NT4, W2K, WXP, 2K3]
RtlInsertElementGenericTable [NT3, NT4, W2K, WXP, 2K3]
RtlInsertElementGenericTableAvl [WXP, 2K3]
RtlInsertElementGenericTableFull [2K3]
RtlInsertElementGenericTableFullAvl [2K3]
RtlInterlockedCompareExchange64 [2K3]
RtlInterlockedPushListSList [WXP, 2K3]
RtlIpv4AddressToStringExA [2K3]
RtlIpv4AddressToStringExW [2K3]
RtlIpv4StringToAddressA [WXP, 2K3]
RtlIpv4StringToAddressExA [2K3]
RtlIpv4StringToAddressExW [2K3]
RtlIpv4StringToAddressW [WXP, 2K3]
RtlIpv6AddressToStringA [WXP, 2K3]
RtlIpv6AddressToStringExA [2K3]
RtlIpv6AddressToStringExW [2K3]
RtlIpv6AddressToStringW [WXP, 2K3]
RtlIpv6StringToAddressA [WXP, 2K3]
RtlIpv6StringToAddressExA [2K3]
RtlIpv6StringToAddressExW [2K3]
RtlIpv6StringToAddressW [WXP, 2K3]
RtlIsActivationContextActive [WXP, 2K3]
RtlIsGenericTableEmpty [NT3, NT4, W2K, WXP, 2K3]
RtlIsGenericTableEmptyAvl [WXP, 2K3]
RtlIsThreadWithinLoaderCallout [WXP, 2K3]
RtlIsValidHandle [NT4, W2K, WXP, 2K3]
RtlIsValidIndexHandle [NT4, W2K, WXP, 2K3]
RtlLockBootStatusData [WXP, 2K3]
RtlLockMemoryStreamRegion [WXP, 2K3]
RtlLogStackBackTrace [WXP, 2K3]
RtlLookupAtomInAtomTable [NT4, W2K, WXP, 2K3]
RtlLookupElementGenericTable [NT3, NT4, W2K, WXP, 2K3]
RtlLookupElementGenericTableAvl [WXP, 2K3]
RtlLookupElementGenericTableFull [2K3]
RtlLookupElementGenericTableFullAvl [2K3]
RtlMultiAppendUnicodeStringBuffer [WXP, 2K3]
RtlMultiByteToUnicodeN [NT3, NT4, W2K, WXP, 2K3]
RtlMultiByteToUnicodeSize [NT3, NT4, W2K, WXP, 2K3]
RtlMultipleAllocateHeap [2K3]
RtlMultipleFreeHeap [2K3]
RtlNewInstanceSecurityObject [NT3, NT4, W2K, WXP, 2K3]
RtlNewSecurityGrantedAccess [NT3, NT4, W2K, WXP, 2K3]
RtlNewSecurityObject [NT3, NT4, W2K, WXP, 2K3]
RtlNewSecurityObjectEx [W2K, WXP, 2K3]
RtlNewSecurityObjectWithMultipleInheritance [WXP, 2K3]
RtlNtPathNameToDosPathName [WXP, 2K3]
RtlNumberGenericTableElements [NT3, NT4, W2K, WXP, 2K3]
RtlNumberGenericTableElementsAvl [WXP, 2K3]
RtlPcToFileHeader [NT3, NT4, W2K, WXP, 2K3]
RtlPinAtomInAtomTable [NT4, W2K, WXP, 2K3]
RtlPopFrame [WXP, 2K3]
RtlProtectHeap [NT3, NT4, W2K, WXP, 2K3]
RtlPushFrame [WXP, 2K3]
RtlQueryAtomInAtomTable [NT4, W2K, WXP, 2K3]
RtlQueryHeapInformation [W2K, WXP, 2K3]
RtlQueryInformationActivationContext [WXP, 2K3]
RtlQueryInformationActiveActivationContext [WXP, 2K3]
RtlQueryInterfaceMemoryStream [WXP, 2K3]
RtlQueryProcessBackTraceInformation [NT3, NT4, W2K, WXP, 2K3]
RtlQueryProcessHeapInformation [NT3, NT4, W2K, WXP, 2K3]
RtlQueryProcessLockInformation [NT3, NT4, W2K, WXP, 2K3]
RtlQuerySecurityObject [NT3, NT4, W2K, WXP, 2K3]
RtlQueryTagHeap [NT3, NT4, W2K, WXP, 2K3]
RtlQueryTimeZoneInformation [NT3, NT4, W2K, WXP, 2K3]
RtlQueueApcWow64Thread [WXP, 2K3]
RtlQueueWorkItem [W2K, WXP, 2K3]
RtlRaiseException [NT3, NT4, W2K, WXP, 2K3]
RtlReadMemoryStream [WXP, 2K3]
RtlReadOutOfProcessMemoryStream [WXP, 2K3]
RtlRealPredecessor [NT3, NT4, W2K, WXP, 2K3]
RtlRealSuccessor [NT3, NT4, W2K, WXP, 2K3]
RtlRegisterSecureMemoryCacheCallback [WXP, 2K3]
RtlRegisterWait [W2K, WXP, 2K3]
RtlReleaseActivationContext [WXP, 2K3]
RtlReleaseMemoryStream [WXP, 2K3]
RtlReleaseRelativeName [2K3]
RtlReleaseResource [NT3, NT4, W2K, WXP, 2K3]
RtlRemoteCall [NT3, NT4, W2K, WXP, 2K3]
RtlResetRtlTranslations [NT3, NT4, W2K, WXP, 2K3]
RtlRevertMemoryStream [WXP, 2K3]
RtlSeekMemoryStream [WXP, 2K3]
RtlSelfRelativeToAbsoluteSD2 [W2K, WXP, 2K3]
RtlSetAttributesSecurityDescriptor [NT4, W2K, WXP, 2K3]
RtlSetCurrentEnvironment [NT3, NT4, W2K, WXP, 2K3]
RtlSetEnvironmentStrings [2K3]
RtlSetEnvironmentVariable [NT3, NT4, W2K, WXP, 2K3]
RtlSetHeapInformation [W2K, WXP, 2K3]
RtlSetIoCompletionCallback [W2K, WXP, 2K3]
RtlSetMemoryStreamSize [WXP, 2K3]
RtlSetSecurityDescriptorRMControl [W2K, WXP, 2K3]
RtlSetSecurityObject [NT3, NT4, W2K, WXP, 2K3]
RtlSetSecurityObjectEx [W2K, WXP, 2K3]
RtlSetThreadErrorMode [2K3]
RtlSetThreadPoolStartFunc [W2K, WXP, 2K3]
RtlSetTimeZoneInformation [NT3, NT4, W2K, WXP, 2K3]
RtlSetTimer [W2K, WXP, 2K3]
RtlSetUnicodeCallouts [NT4, W2K, WXP, 2K3]
RtlSetUserFlagsHeap [NT3, NT4, W2K, WXP, 2K3]
RtlSetUserValueHeap [NT3, NT4, W2K, WXP, 2K3]
RtlShutdownLpcServer [W2K]
RtlSplay [NT3, NT4, W2K, WXP, 2K3]
RtlStartRXact [NT3, NT4, W2K, WXP, 2K3]
RtlStatMemoryStream [WXP, 2K3]
RtlSubtreePredecessor [NT3, NT4, W2K, WXP, 2K3]
RtlSubtreeSuccessor [NT3, NT4, W2K, WXP, 2K3]
RtlTraceDatabaseAdd [W2K, WXP, 2K3]
RtlTraceDatabaseCreate [W2K, WXP, 2K3]
RtlTraceDatabaseDestroy [W2K, WXP, 2K3]
RtlTraceDatabaseEnumerate [W2K, WXP, 2K3]
RtlTraceDatabaseFind [W2K, WXP, 2K3]
RtlTraceDatabaseLock [W2K, WXP, 2K3]
RtlTraceDatabaseUnlock [W2K, WXP, 2K3]
RtlTraceDatabaseValidate [W2K, WXP, 2K3]
RtlUnhandledExceptionFilter [WXP, 2K3]
RtlUnhandledExceptionFilter2 [WXP, 2K3]
RtlUnicodeToCustomCPN [NT3, NT4, W2K, WXP, 2K3]
RtlUnicodeToMultiByteN [NT3, NT4, W2K, WXP, 2K3]
RtlUnicodeToOemN [NT3, NT4, W2K, WXP, 2K3]
RtlUnlockBootStatusData [WXP, 2K3]
RtlUnlockHeap [NT3, NT4, W2K, WXP, 2K3]
RtlUnlockMemoryStreamRegion [WXP, 2K3]
RtlUpcaseUnicodeToCustomCPN [NT3, NT4, W2K, WXP, 2K3]
RtlUpdateTimer [W2K, WXP, 2K3]
RtlUsageHeap [NT3, NT4, W2K, WXP, 2K3]
RtlValidateProcessHeaps [NT3, NT4, W2K, WXP, 2K3]
RtlWalkFrameChain [W2K, WXP, 2K3]
RtlWalkHeap [NT3, NT4, W2K, WXP, 2K3]
RtlWow64EnableFsRedirection [2K3]
RtlWriteMemoryStream [WXP, 2K3]
RtlZombifyActivationContext [WXP, 2K3]
RtlpApplyLengthFunction [WXP, 2K3]
RtlpEnsureBufferSize [WXP, 2K3]
RtlpUnWaitCriticalSection [NT3, NT4, W2K, WXP, 2K3]
RtlpWaitForCriticalSection [NT3, NT4, W2K, WXP, 2K3]
SaveEm87Context [NT3, NT4, W2K, WXP, 2K3]
ZwAddBootEntry [WXP, 2K3]
ZwAddDriverEntry [2K3]
ZwApphelpCacheControl [2K3]
ZwCompactKeys [WXP, 2K3]
ZwCompareTokens [WXP, 2K3]
ZwCompressKey [WXP, 2K3]
ZwCreateDebugObject [WXP, 2K3]
ZwCreateJobSet [WXP, 2K3]
ZwCreateKeyedEvent [WXP, 2K3]
ZwCreateProcessEx [WXP, 2K3]
ZwDebugContinue [WXP, 2K3]
ZwDeleteBootEntry [WXP, 2K3]
ZwDeleteDriverEntry [2K3]
ZwEnumerateBootEntries [WXP, 2K3]
ZwEnumerateDriverEntries [2K3]
ZwEnumerateSystemEnvironmentValuesEx [WXP, 2K3]
ZwIsProcessInJob [WXP, 2K3]
ZwLoadKeyEx [2K3]
ZwLockProductActivationKeys [WXP, 2K3]
ZwLockRegistryKey [WXP, 2K3]
ZwModifyBootEntry [WXP, 2K3]
ZwModifyDriverEntry [2K3]
ZwOpenKeyedEvent [WXP, 2K3]
ZwOpenProcessTokenEx [WXP, 2K3]
ZwOpenThreadTokenEx [WXP, 2K3]
ZwQueryBootEntryOrder [WXP, 2K3]
ZwQueryBootOptions [WXP, 2K3]
ZwQueryDebugFilterState [WXP, 2K3]
ZwQueryDriverEntryOrder [2K3]
ZwQueryOpenSubKeysEx [2K3]
ZwQuerySystemEnvironmentValueEx [WXP, 2K3]
ZwReleaseKeyedEvent [WXP, 2K3]
ZwRenameKey [WXP, 2K3]
ZwSetBootEntryOrder [WXP, 2K3]
ZwSetBootOptions [WXP, 2K3]
ZwSetDebugFilterState [WXP, 2K3]
ZwSetDriverEntryOrder [2K3]
ZwSetEventBoostPriority [WXP, 2K3]
ZwSetInformationDebugObject [WXP, 2K3]
ZwSetSystemEnvironmentValueEx [WXP, 2K3]
ZwTraceEvent [WXP, 2K3]
ZwTranslateFilePath [WXP, 2K3]
ZwUnloadKey2 [2K3]
ZwUnloadKeyEx [WXP, 2K3]
ZwWaitForDebugEvent [WXP, 2K3]
ZwWaitForKeyedEvent [WXP, 2K3]

 +  457 (35.90%) not yet declared
 +  816 (64.10%) declared already
 = 1273 (100.00%) relevant functions overall


The following usermode Native APIs are considered deprecated
since they are only available in NT3 or NT4 only or in NT3/NT4
only. Hence they are considered irrelevant. These are:
-----------------------------------------------------------------
CsrAllocateCapturePointer [NT3, NT4]
CsrClientMaxMessage [NT3]
CsrClientSendMessage [NT3]
CsrClientThreadConnect [NT3]
CsrpProcessCallbackRequest [NT3]
NtEnumerateBus [NT3]
NtQueryOleDirectoryFile [NT4]
NtRegisterNewDevice [NT3]
NtReleaseProcessMutant [NT3]
NtWaitForProcessMutant [NT3]
RtlClosePropertySet [NT4]
RtlCompareVariants [NT4]
RtlCreatePropertySet [NT4]
RtlEnumerateProperties [NT4]
RtlFindLongestRunSet [NT3, NT4]
RtlFlushPropertySet [NT4]
RtlGuidToPropertySetName [NT4]
RtlOnMappedStreamEvent [NT4]
RtlPropertySetNameToGuid [NT4]
RtlQueryProperties [NT4]
RtlQueryPropertyNames [NT4]
RtlQueryPropertySet [NT4]
RtlSetProperties [NT4]
RtlSetPropertyNames [NT4]
RtlSetPropertySetClassId [NT4]
RtlpInitializeRtl [NT3]
ZwEnumerateBus [NT3]
ZwQueryOleDirectoryFile [NT4]
ZwRegisterNewDevice [NT3]
ZwReleaseProcessMutant [NT3]
ZwWaitForProcessMutant [NT3]

 = 31 deprecated functions
*)
{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}

