{******************************************************************************}
{                                                                              }
{ Event Tracing API interface Unit for Object Pascal                           }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2008 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ Portions created by Christian Wimmer are Copyright (C) 20008                 }
{ Christian Wimmer. All Rights Reserved.                                       }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ The original file is: Evntrace.h, released <unknown>.                        }
{ The initial developer of the Pascal code is Christian Wimmer.                }
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


{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaEventTracing;
{$IFDEF BCB1_UP}
This unit does not support Borland C++ yet!
{$ENDIF BCB1_UP}
{$ENDIF JWA_OMIT_SECTIONS}


{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

{$IFDEF JWA_WINDOWS}
  uses JwaWindows;
{$ELSE}
  uses JwaWinBase, JwaWinType, JwaWmiStr, JwaEventDefs;
{$ENDIF JWA_WINDOWS}

{$IFDEF DELPHI6_UP}
{$ALIGN 8}
{$ELSE}
{$A+}
//Warning: Record alignment 4
{$ENDIF DELPHI6_UP}

{$ENDIF JWA_OMIT_SECTIONS}

{Some routines are only available in
WINXP_UP
WINVISTA_UP
}


{$IFNDEF JWA_IMPLEMENTATIONSECTION}
const
  EventTraceGuid 			: TGUID = '{68fdd900-4a3e-11d1-84f4-0000f80464e3}';
  SystemTraceControlGuid 	: TGUID = '{9e814aad-3204-11d2-9a82-006008a86939}';
  EventTraceConfigGuid		: TGUID = '{01853a65-418f-4f36-aefc-dc0f1d2fd235}';
  DefaultTraceSecurityGuid  : TGUID = '{0811c1af-7a07-4a06-82ed-869455cdf713}';
  
const
	KERNEL_LOGGER_NAMEW = WideString('NT Kernel Logger');
	GLOBAL_LOGGER_NAMEW = WideString('GlobalLogger');
	EVENT_LOGGER_NAMEW  = WideString('EventLog');
	DIAG_LOGGER_NAMEW   = WideString('DiagLog');

	KERNEL_LOGGER_NAMEA = AnsiString('NT Kernel Logger');
	GLOBAL_LOGGER_NAMEA = AnsiString('GlobalLogger');
	EVENT_LOGGER_NAMEA  = AnsiString('EventLog');
	DIAG_LOGGER_NAMEA   = AnsiString('DiagLog');

	MAX_MOF_FIELDS = 16;  // Limit of USE_MOF_PTR fields

  
type
  TRACEHANDLE  = UINT64;
  PTRACEHANDLE = ^TRACEHANDLE;


const
	
	//types for event data going to System Event Logger
	SYSTEM_EVENT_TYPE = 1;
	
	//
	// predefined generic event types ($00 to $09 reserved).
	//

	 EVENT_TRACE_TYPE_INFO               = $00;  // Info or point event
	 EVENT_TRACE_TYPE_START              = $01;  // Start event
	 EVENT_TRACE_TYPE_END                = $02;  // End event
	 EVENT_TRACE_TYPE_STOP               = $02;  // Stop event (WinEvent compatible)
	 EVENT_TRACE_TYPE_DC_START           = $03;  // Collection start marker
	 EVENT_TRACE_TYPE_DC_END             = $04;  // Collection end marker
	 EVENT_TRACE_TYPE_EXTENSION          = $05;  // Extension/continuation
	 EVENT_TRACE_TYPE_REPLY              = $06;  // Reply event
	 EVENT_TRACE_TYPE_DEQUEUE            = $07;  // De-queue event
	 EVENT_TRACE_TYPE_RESUME             = $07;  // Resume event (WinEvent compatible)
	 EVENT_TRACE_TYPE_CHECKPOINT         = $08;  // Generic checkpoint event
	 EVENT_TRACE_TYPE_SUSPEND            = $08;  // Suspend event (WinEvent compatible)
	 EVENT_TRACE_TYPE_WINEVT_SEND        = $09;  // Send Event (WinEvent compatible)
	 EVENT_TRACE_TYPE_WINEVT_RECEIVE     = $F0;  // Receive Event (WinEvent compatible)
		
		
	//
	// Predefined Event Tracing Levels for Software/Debug Tracing
	//
	//
	// Trace Level is UCHAR and passed in through the EnableLevel parameter
	// in EnableTrace API. It is retrieved by the provider using the
	// GetTraceEnableLevel macro.It should be interpreted as an integer value
	// to mean everything at or below that level will be traced.
	//
	// Here are the possible Levels.
	//

	 TRACE_LEVEL_NONE        = 0;   // Tracing is not on
	 TRACE_LEVEL_CRITICAL    = 1;   // Abnormal exit or termination
	 TRACE_LEVEL_FATAL       = 1;   // Deprecated name for Abnormal exit or termination
	 TRACE_LEVEL_ERROR       = 2;   // Severe errors that need logging
	 TRACE_LEVEL_WARNING     = 3;   // Warnings such as allocation failure
	 TRACE_LEVEL_INFORMATION = 4;   // Includes non-error cases(e.g.,Entry-Exit)
	 TRACE_LEVEL_VERBOSE     = 5;   // Detailed traces from intermediate steps
	 TRACE_LEVEL_RESERVED6   = 6;
	 TRACE_LEVEL_RESERVED7   = 7;
	 TRACE_LEVEL_RESERVED8   = 8;
	 TRACE_LEVEL_RESERVED9   = 9;	
		

	//
	// Event types for Process & Threads
	//

	 EVENT_TRACE_TYPE_LOAD                  = $0A;      // Load image

	//
	// Event types for IO subsystem
	//

	 EVENT_TRACE_TYPE_IO_READ               = $0A;
	 EVENT_TRACE_TYPE_IO_WRITE              = $0B;
	 EVENT_TRACE_TYPE_IO_READ_INIT          = $0C;
	 EVENT_TRACE_TYPE_IO_WRITE_INIT         = $0D;
	 EVENT_TRACE_TYPE_IO_FLUSH              = $0E;
	 EVENT_TRACE_TYPE_IO_FLUSH_INIT         = $0F;


	//
	// Event types for Memory subsystem
	//

	 EVENT_TRACE_TYPE_MM_TF                 = $0A;      // Transition fault
	 EVENT_TRACE_TYPE_MM_DZF                = $0B;      // Demand Zero fault
	 EVENT_TRACE_TYPE_MM_COW                = $0C;      // Copy on Write
	 EVENT_TRACE_TYPE_MM_GPF                = $0D;      // Guard Page fault
	 EVENT_TRACE_TYPE_MM_HPF                = $0E;      // Hard page fault
	 EVENT_TRACE_TYPE_MM_AV                 = $0F;      // Access violation

	//
	// Event types for Network subsystem, all protocols
	//

	 EVENT_TRACE_TYPE_SEND                  = $0A;     // Send
	 EVENT_TRACE_TYPE_RECEIVE               = $0B;     // Receive
	 EVENT_TRACE_TYPE_CONNECT               = $0C;     // Connect
	 EVENT_TRACE_TYPE_DISCONNECT            = $0D;     // Disconnect
	 EVENT_TRACE_TYPE_RETRANSMIT            = $0E;     // ReTransmit
	 EVENT_TRACE_TYPE_ACCEPT                = $0F;     // Accept
	 EVENT_TRACE_TYPE_RECONNECT             = $10;     // ReConnect
	 EVENT_TRACE_TYPE_CONNFAIL              = $11;     // Fail
	 EVENT_TRACE_TYPE_COPY_TCP              = $12;     // Copy in PendData
	 EVENT_TRACE_TYPE_COPY_ARP              = $13;     // NDIS_STATUS_RESOURCES Copy
	 EVENT_TRACE_TYPE_ACKFULL               = $14;     // A full data ACK
	 EVENT_TRACE_TYPE_ACKPART               = $15;     // A Partial data ACK
	 EVENT_TRACE_TYPE_ACKDUP                = $16;     // A Duplicate data ACK


	//
	// Event Types for the Header (to handle internal event headers)
	//

	 EVENT_TRACE_TYPE_GUIDMAP                = $0A;
	 EVENT_TRACE_TYPE_CONFIG                 = $0B;
	 EVENT_TRACE_TYPE_SIDINFO                = $0C;
	 EVENT_TRACE_TYPE_SECURITY               = $0D;

	//
	// Event Types for Registry subsystem
	//

	 EVENT_TRACE_TYPE_REGCREATE                  = $0A;     // NtCreateKey
	 EVENT_TRACE_TYPE_REGOPEN                    = $0B;     // NtOpenKey
	 EVENT_TRACE_TYPE_REGDELETE                  = $0C;     // NtDeleteKey
	 EVENT_TRACE_TYPE_REGQUERY                   = $0D;     // NtQueryKey
	 EVENT_TRACE_TYPE_REGSETVALUE                = $0E;     // NtSetValueKey
	 EVENT_TRACE_TYPE_REGDELETEVALUE             = $0F;     // NtDeleteValueKey
	 EVENT_TRACE_TYPE_REGQUERYVALUE              = $10;     // NtQueryValueKey
	 EVENT_TRACE_TYPE_REGENUMERATEKEY            = $11;     // NtEnumerateKey
	 EVENT_TRACE_TYPE_REGENUMERATEVALUEKEY       = $12;     // NtEnumerateValueKey
	 EVENT_TRACE_TYPE_REGQUERYMULTIPLEVALUE      = $13;     // NtQueryMultipleValueKey
	 EVENT_TRACE_TYPE_REGSETINFORMATION          = $14;     // NtSetInformationKey
	 EVENT_TRACE_TYPE_REGFLUSH                   = $15;     // NtFlushKey
	 EVENT_TRACE_TYPE_REGKCBCREATE               = $16;     // KcbCreate
	 EVENT_TRACE_TYPE_REGKCBDELETE               = $17;     // KcbDelete
	 EVENT_TRACE_TYPE_REGKCBRUNDOWNBEGIN         = $18;     // KcbRundownBegin
	 EVENT_TRACE_TYPE_REGKCBRUNDOWNEND           = $19;     // KcbRundownEnd
	 EVENT_TRACE_TYPE_REGVIRTUALIZE              = $1A;     // VirtualizeKey
	 EVENT_TRACE_TYPE_REGCLOSE                   = $1B;     // NtClose (KeyObject)

	//
	// Event types for system configuration records
	//
	 EVENT_TRACE_TYPE_CONFIG_CPU             = $0A;     // CPU Configuration
	 EVENT_TRACE_TYPE_CONFIG_PHYSICALDISK    = $0B;     // Physical Disk Configuration
	 EVENT_TRACE_TYPE_CONFIG_LOGICALDISK     = $0C;     // Logical Disk Configuration
	 EVENT_TRACE_TYPE_CONFIG_NIC             = $0D;     // NIC Configuration
	 EVENT_TRACE_TYPE_CONFIG_VIDEO           = $0E;     // Video Adapter Configuration
	 EVENT_TRACE_TYPE_CONFIG_SERVICES        = $0F;     // Active Services
	 EVENT_TRACE_TYPE_CONFIG_POWER           = $10;     // ACPI Configuration
	 EVENT_TRACE_TYPE_CONFIG_NETINFO         = $11;     // Networking Configuration

	 EVENT_TRACE_TYPE_CONFIG_IRQ             = $15;     // IRQ assigned to devices
	 EVENT_TRACE_TYPE_CONFIG_PNP             = $16;     // PnP device info
	 EVENT_TRACE_TYPE_CONFIG_IDECHANNEL      = $17;     // Primary/Secondary IDE channel Configuration


	//
	// Enable flags for Kernel Events
	//
	 EVENT_TRACE_FLAG_PROCESS            = $00000001;  // process start & end
	 EVENT_TRACE_FLAG_THREAD             = $00000002;  // thread start & end
	 EVENT_TRACE_FLAG_IMAGE_LOAD         = $00000004;  // image load

	 EVENT_TRACE_FLAG_DISK_IO            = $00000100;  // physical disk IO
	 EVENT_TRACE_FLAG_DISK_FILE_IO       = $00000200;  // requires disk IO

	 EVENT_TRACE_FLAG_MEMORY_PAGE_FAULTS = $00001000;  // all page faults
	 EVENT_TRACE_FLAG_MEMORY_HARD_FAULTS = $00002000;  // hard faults only

	 EVENT_TRACE_FLAG_NETWORK_TCPIP      = $00010000;  // tcpip send & receive

	 EVENT_TRACE_FLAG_REGISTRY           = $00020000;  // registry calls
	 EVENT_TRACE_FLAG_DBGPRINT           = $00040000;  // DbgPrint(ex) Calls

	//
	// Enable flags for Kernel Events on Vista and above 
	//
	 EVENT_TRACE_FLAG_PROCESS_COUNTERS   = $00000008;  // process perf counters
	 EVENT_TRACE_FLAG_CSWITCH            = $00000010;  // context switches 
	 EVENT_TRACE_FLAG_DPC                = $00000020;  // deffered procedure calls 
	 EVENT_TRACE_FLAG_INTERRUPT          = $00000040;  // interrupts
	 EVENT_TRACE_FLAG_SYSTEMCALL         = $00000080;  // system calls

	 EVENT_TRACE_FLAG_DISK_IO_INIT       = $00000400;  // physical disk IO initiation

	 EVENT_TRACE_FLAG_ALPC               = $00100000;  // ALPC traces
	 EVENT_TRACE_FLAG_SPLIT_IO           = $00200000;  // split io traces (VolumeManager)

	 EVENT_TRACE_FLAG_DRIVER             = $00800000;  // driver delays
	 EVENT_TRACE_FLAG_PROFILE            = $01000000;  // sample based profiling
	 EVENT_TRACE_FLAG_FILE_IO            = $02000000;  // file IO
	 EVENT_TRACE_FLAG_FILE_IO_INIT       = $04000000;  // file IO initiation

	//
	// Pre-defined Enable flags for everybody else
	//
	 EVENT_TRACE_FLAG_EXTENSION          = $80000000;  // Indicates more flags
	 EVENT_TRACE_FLAG_FORWARD_WMI        = $40000000;  // Can forward to WMI
	 EVENT_TRACE_FLAG_ENABLE_RESERVE     = $20000000;  // Reserved

	//
	// Logger Mode flags
	//
	 EVENT_TRACE_FILE_MODE_NONE          = $00000000; // Logfile is off
	 EVENT_TRACE_FILE_MODE_SEQUENTIAL    = $00000001;  // Log sequentially
	 EVENT_TRACE_FILE_MODE_CIRCULAR      = $00000002;  // Log in circular manner
	 EVENT_TRACE_FILE_MODE_APPEND        = $00000004;  // Append sequential log

	 EVENT_TRACE_REAL_TIME_MODE          = $00000100;  // Real time mode on
	 EVENT_TRACE_DELAY_OPEN_FILE_MODE    = $00000200;  // Delay opening file
	 EVENT_TRACE_BUFFERING_MODE          = $00000400;  // Buffering mode only
	 EVENT_TRACE_PRIVATE_LOGGER_MODE     = $00000800;  // Process Private Logger
	 EVENT_TRACE_ADD_HEADER_MODE         = $00001000;  // Add a logfile header

	 EVENT_TRACE_USE_GLOBAL_SEQUENCE     = $00004000;  // Use global sequence no.
	 EVENT_TRACE_USE_LOCAL_SEQUENCE      = $00008000;  // Use local sequence no.

	 EVENT_TRACE_RELOG_MODE              = $00010000;  // Relogger

	 EVENT_TRACE_USE_PAGED_MEMORY        = $01000000;  // Use pageable buffers

	//
	// Logger Mode flags on XP and above
	//
	 EVENT_TRACE_FILE_MODE_NEWFILE       = $00000008;  // Auto-switch log file
	 EVENT_TRACE_FILE_MODE_PREALLOCATE   = $00000020;  // Pre-allocate mode

	//
	// Logger Mode flags on Vista and above
	//
	 EVENT_TRACE_NONSTOPPABLE_MODE       = $00000040;  // Session cannot be stopped (Autologger only)
	 EVENT_TRACE_SECURE_MODE             = $00000080;  // Secure session
	 EVENT_TRACE_USE_KBYTES_FOR_SIZE     = $00002000;  // Use KBytes as file size unit
	 EVENT_TRACE_PRIVATE_IN_PROC         = $00020000;  // In process private logger
	 EVENT_TRACE_MODE_RESERVED           = $00100000;  // Reserved bit, used to signal Heap/Critsec tracing


		

	//
	// ControlTrace Codes
	//
	 EVENT_TRACE_CONTROL_QUERY         = 0;
	 EVENT_TRACE_CONTROL_STOP          = 1;
	 EVENT_TRACE_CONTROL_UPDATE        = 2;

	//
	// Flush ControlTrace Codes for XP and above
	//
	 EVENT_TRACE_CONTROL_FLUSH         = 3;       // Flushes all the buffers

	//
	// Flags used by WMI Trace Message
	// Note that the order or value of these flags should NOT be changed as they are processed
	// in this order.
	//
	 TRACE_MESSAGE_SEQUENCE              = 1;  // Message should include a sequence number
	 TRACE_MESSAGE_GUID                  = 2;  // Message includes a GUID
	 TRACE_MESSAGE_COMPONENTID           = 4;  // Message has no GUID, Component ID instead
	 TRACE_MESSAGE_TIMESTAMP             = 8;  // Message includes a timestamp
	 TRACE_MESSAGE_PERFORMANCE_TIMESTAMP = 16; // *Obsolete* Clock type is controlled by the logger
	 TRACE_MESSAGE_SYSTEMINFO            = 32; // Message includes system information TID,PID

	//
	// Vista flags set by system to indicate provider pointer size.
	//

	 TRACE_MESSAGE_POINTER32             = $0040;  // Message logged by 32 bit provider
	 TRACE_MESSAGE_POINTER64             = $0080;  // Message logged by 64 bit provider

	 TRACE_MESSAGE_FLAG_MASK         = $FFFF;  // Only the lower 16 bits of flags are placed in the message
													// those above 16 bits are reserved for local processing
	 TRACE_MESSAGE_MAXIMUM_SIZE  = 8*1024;      // the maximum size allowed for a single trace message
													// longer messages will return ERROR_BUFFER_OVERFLOW
	//
	// Flags to indicate to consumer which fields
	// in the EVENT_TRACE_HEADER are valid
	//

	 EVENT_TRACE_USE_PROCTIME   = $0001;    // ProcessorTime field is valid
	 EVENT_TRACE_USE_NOCPUTIME  = $0002;    // No Kernel/User/Processor Times

	//
	// TRACE_HEADER_FLAG values are used in the Flags field of EVENT_TRACE_HEADER
	// structure while calling into TraceEvent API
	//

	 TRACE_HEADER_FLAG_USE_TIMESTAMP     = $00000200;
	 TRACE_HEADER_FLAG_TRACED_GUID       = $00020000; // denotes a trace
	 TRACE_HEADER_FLAG_LOG_WNODE         = $00040000; // request to log Wnode
	 TRACE_HEADER_FLAG_USE_GUID_PTR      = $00080000; // Guid is actually a pointer
	 TRACE_HEADER_FLAG_USE_MOF_PTR       = $00100000; // MOF data are dereferenced

type
//
// Trace header for all legacy events. 
//

  _EVENT_TRACE_HEADER = record        // overlays WNODE_HEADER
    Size : USHORT;                   // Size of entire record
    _FieldTypeFlags : record
    case boolean of
      True: (FieldTypeFlags : USHORT);         // Indicates valid fields
      False: (
              HeaderType  : UCHAR;             // Header type - internal use only
              MarkerFlags : UCHAR);            // Marker - internal use only
    end;
    _Version : record
      case Boolean of
          true  : (Version1 : ULONG);
          false : (
              Type_ : UCHAR;                   // event type
              Level : UCHAR;                  // trace instrumentation level
              Version2: USHORT);                // version of trace record
    end;
    ThreadId  : ULONG;               // Thread Id
    ProcessId : ULONG;              // Process Id

    TimeStamp : LARGE_INTEGER;              // time when event happens

    _GuidValue : record
      case Boolean of
        true  : (GuidValue : GUID);                   // Guid that identifies event
        false : (GuidPtr   : ULONGLONG);                // use with WNODE_FLAG_USE_GUID_PTR
    end;
    _KernelTime : Record
      case Short of
        0  : (
              KernelTime : ULONG;             // Kernel Mode CPU ticks
              UserTime   : ULONG);               // User mode CPU ticks
        1  : (ProcessorTime : ULONG64);          // Processor Clock
        2  : (
              ClientContext : ULONG;          // Reserved
              Flags         : ULONG)                  // Event Flags
    end;
  end;

  EVENT_TRACE_HEADER = _EVENT_TRACE_HEADER;
  PEVENT_TRACE_HEADER = ^EVENT_TRACE_HEADER;

  TEventTraceHeader = EVENT_TRACE_HEADER;
  PEventTraceHeader = ^TEventTraceHeader;


//
// This header is used to trace and track transaction co-relations
//
  _EVENT_INSTANCE_HEADER = record
    Size : USHORT;                   // Size of entire record
    _FieldTypeFlags : record
    case boolean of
      True: (FieldTypeFlags : USHORT);         // Indicates valid fields
      False: (
              HeaderType  : UCHAR;             // Header type - internal use only
              MarkerFlags : UCHAR);            // Marker - internal use only
    end;
    _Version : record
      case Boolean of
          true  : (Version1 : ULONG);
          false : (
              Type_ : UCHAR;                   // event type
              Level : UCHAR;                  // trace instrumentation level
              Version2: USHORT);                // version of trace record
    end;
    ThreadId  : ULONG;               // Thread Id
    ProcessId : ULONG;              // Process Id

    TimeStamp : LARGE_INTEGER;              // time when event happens

    RegHandle : ULONGLONG;
    InstanceId : ULONG;
    ParentInstanceId : ULONG;
    _KernelTime : Record
      case Short of
        0  : (
              KernelTime : ULONG;             // Kernel Mode CPU ticks
              UserTime   : ULONG);               // User mode CPU ticks
        1  : (ProcessorTime : ULONG64);          // Processor Clock
        2  : (
              ClientContext : ULONG;          // Reserved
              Flags         : ULONG)                  // Event Flags
    end;
    ParentRegHandle : ULONGLONG;
  end;
  EVENT_INSTANCE_HEADER  = _EVENT_INSTANCE_HEADER;
  PEVENT_INSTANCE_HEADER = ^EVENT_INSTANCE_HEADER;

  TEventInstanceHeader = EVENT_INSTANCE_HEADER;
  PEVentInstanceHeader = ^TEventInstanceHeader;



//
// Following are structures and macros for use with USE_MOF_PTR
//
const
  // Trace data types
  ETW_NULL_TYPE_VALUE                        = 0;
  ETW_OBJECT_TYPE_VALUE                      = 1;
  ETW_STRING_TYPE_VALUE                      = 2;
  ETW_SBYTE_TYPE_VALUE                       = 3;
  ETW_BYTE_TYPE_VALUE                        = 4;
  ETW_INT16_TYPE_VALUE                       = 5;
  ETW_UINT16_TYPE_VALUE                      = 6;
  ETW_INT32_TYPE_VALUE                       = 7;
  ETW_UINT32_TYPE_VALUE                      = 8;
  ETW_INT64_TYPE_VALUE                       = 9;
  ETW_UINT64_TYPE_VALUE                      = 10;
  ETW_CHAR_TYPE_VALUE                        = 11;
  ETW_SINGLE_TYPE_VALUE                      = 12;
  ETW_DOUBLE_TYPE_VALUE                      = 13;
  ETW_BOOLEAN_TYPE_VALUE                     = 14;
  ETW_DECIMAL_TYPE_VALUE                     = 15;
  // Extended types
  ETW_GUID_TYPE_VALUE                        = 101;
  ETW_ASCIICHAR_TYPE_VALUE                   = 102;
  ETW_ASCIISTRING_TYPE_VALUE                 = 103;
  ETW_COUNTED_STRING_TYPE_VALUE              = 104;
  ETW_POINTER_TYPE_VALUE                     = 105;
  ETW_SIZET_TYPE_VALUE                       = 106;
  ETW_HIDDEN_TYPE_VALUE                      = 107;
  ETW_BOOL_TYPE_VALUE                        = 108;
  ETW_COUNTED_ANSISTRING_TYPE_VALUE          = 109;
  ETW_REVERSED_COUNTED_STRING_TYPE_VALUE     = 110;
  ETW_REVERSED_COUNTED_ANSISTRING_TYPE_VALUE = 111;
  ETW_NON_NULL_TERMINATED_STRING_TYPE_VALUE  = 112;
  ETW_REDUCED_ANSISTRING_TYPE_VALUE          = 113;
  ETW_REDUCED_STRING_TYPE_VALUE              = 114;
  ETW_SID_TYPE_VALUE                         = 115;
  ETW_VARIANT_TYPE_VALUE                     = 116;
  ETW_PTVECTOR_TYPE_VALUE                    = 117;
  ETW_WMITIME_TYPE_VALUE                     = 118;
  ETW_DATETIME_TYPE_VALUE                    = 119;
  ETW_REFRENCE_TYPE_VALUE                    = 120;


type
  _MOF_FIELD = record
    DataPtr  : ULONG64;    // Pointer to the field. Up to 64-bits only
    Length   : ULONG;     // Length of the MOF field
    DataType : ULONG;   // Type of data
  end;
  MOF_FIELD  = _MOF_FIELD;
  PMOF_FIELD = ^MOF_FIELD;

  TMOFField = MOF_FIELD;
  PMOFField = ^TMOFField;


  procedure DEFINE_TRACE_MOF_FIELD(var MOF : PMOFField;
    const ptr : ULONG64; const length : ULONG; const Typ : ULONG);


type


//
// This is the header for every logfile. The memory for LoggerName
// and LogFileName must be contiguous adjacent to this structure
// Allows both user-mode and kernel-mode to understand the header
//
  _TRACE_LOGFILE_HEADER = record
    BufferSize : ULONG;         // Logger buffer size in Kbytes
    VersionDetail : record
      case Boolean of
        true : (Version : ULONG);            // Logger version
        false : (
                 MajorVersion : UCHAR;
                 MinorVersion : UCHAR;
                 SubVersion   : UCHAR;
                 SubMinorVersion : UCHAR);
    end;
    ProviderVersion : ULONG;    // defaults to NT version
    NumberOfProcessors : ULONG; // Number of Processors
    EndTime : LARGE_INTEGER;            // Time when logger stops
    TimerResolution : ULONG;    // assumes timer is constant!!!
    MaximumFileSize : ULONG;    // Maximum in Mbytes
    LogFileMode : ULONG;        // specify logfile mode
    BuffersWritten : ULONG;     // used to file start of Circular File

    LogInstanceGuid : record
      case Boolean of
        true : (LogInstanceGuid : GUID);           // For RealTime Buffer Delivery
        false : (
                 StartBuffers : ULONG;       // Count of buffers written at start.
                 PointerSize  : ULONG;        // Size of pointer type in bits
                 EventsLost   : ULONG;         // Events losts during log session
                 CpuSpeedInMHz  : ULONG);      // Cpu Speed in MHz
    end;

    LoggerName  : LPWSTR;
    LogFileName : LPWSTR;
    TimeZone    : TIME_ZONE_INFORMATION;

    BootTime  : LARGE_INTEGER;
    PerfFreq  : LARGE_INTEGER;           // Reserved
    StartTime : LARGE_INTEGER;          // Reserved
    ReservedFlags : ULONG;      // Reserved
    BuffersLost   : ULONG;
  end;

  TRACE_LOGFILE_HEADER = _TRACE_LOGFILE_HEADER;
  PTRACE_LOGFILE_HEADER = ^TRACE_LOGFILE_HEADER;

  TTraceLogFileHeader = TRACE_LOGFILE_HEADER;
  PTraceLogFileHeader = ^TTraceLogFileHeader;


//
// Instance Information to track parent child relationship of Instances.
//
  _EVENT_INSTANCE_INFO = record
    RegHandle  : HANDLE;
    InstanceId : ULONG;
  end;
  EVENT_INSTANCE_INFO = _EVENT_INSTANCE_INFO;
  PEVENT_INSTANCE_INFO = ^EVENT_INSTANCE_INFO;

  TEventInstanceInfo = EVENT_INSTANCE_INFO;
  PEventInstanceInfo = ^TEventInstanceInfo;


//
// Structures that have UNICODE and ANSI versions are defined here
//

//
// Logger configuration and running statistics. This structure is used
// by user-mode callers, such as PDH library
//

  _EVENT_TRACE_PROPERTIES = record
    Wnode : WNODE_HEADER;
//
// data provided by caller
    BufferSize : ULONG;                   // buffer size for logging (kbytes)
    MinimumBuffers : ULONG;               // minimum to preallocate
    MaximumBuffers : ULONG;               // maximum buffers allowed
    MaximumFileSize : ULONG;              // maximum logfile size (in MBytes)
    LogFileMode : ULONG;                  // sequential, circular
    FlushTimer : ULONG;                   // buffer flush timer, in seconds
    EnableFlags : ULONG;                  // trace enable flags
    AgeLimit : LONG;                     // age decay time, in minutes

// data returned to caller
    NumberOfBuffers : ULONG;              // no of buffers in use
    FreeBuffers : ULONG;                  // no of buffers free
    EventsLost : ULONG;                   // event records lost
    BuffersWritten : ULONG;               // no of buffers written to file
    LogBuffersLost : ULONG;               // no of logfile write failures
    RealTimeBuffersLost : ULONG;          // no of rt delivery failures
    LoggerThreadId : HANDLE;              // thread id of Logger
    LogFileNameOffset : ULONG;            // Offset to LogFileName
    LoggerNameOffset : ULONG;             // Offset to LoggerName
  end;

  EVENT_TRACE_PROPERTIES = _EVENT_TRACE_PROPERTIES;
  PEVENT_TRACE_PROPERTIES = ^EVENT_TRACE_PROPERTIES;

  TEventTraceProperties = EVENT_TRACE_PROPERTIES;
  PEventTraceProperties = ^TEventTraceProperties; 


// NOTE:
// If AgeLimit is 0, default is used
// If AgeLimit is < 0, buffer aging is turned off


//
// Data Provider structures
//
// Used by RegisterTraceGuids()
//
  _TRACE_GUID_REGISTRATION = record
    Guid : PGUID;            // Guid of data block being registered or updated.
    RegHandle : HANDLE;        // Guid Registration Handle is returned.
  end;

  TRACE_GUID_REGISTRATION = _TRACE_GUID_REGISTRATION;
  PTRACE_GUID_REGISTRATION = ^TRACE_GUID_REGISTRATION;

  TTraceGUIDRegistration = TRACE_GUID_REGISTRATION;
  PTraceGUIDRegistration = ^TTraceGUIDRegistration;


//
// Data consumer structures
//
  _TRACE_GUID_PROPERTIES = record
    Guid : GUID;
    GuidType : ULONG;
    LoggerId : ULONG;
    EnableLevel : ULONG;
    EnableFlags : ULONG;
    IsEnable : Boolean;
  end;

  TRACE_GUID_PROPERTIES = _TRACE_GUID_PROPERTIES;
  PTRACE_GUID_PROPERTIES = ^TRACE_GUID_PROPERTIES;

  TTraceGUIDProperties = TRACE_GUID_PROPERTIES;
  PTraceGUIDProperties = ^TTraceGUIDProperties;






//
// Provider Information Flags used on Vista and above
//
const
  TRACE_PROVIDER_FLAG_LEGACY     = $00000001;
  TRACE_PROVIDER_FLAG_PRE_ENABLE = $00000002;

//
// Enable Information for Provider Instance
// Used on Vista and above
//
type
  _TRACE_ENABLE_INFO = record
    IsEnabled : ULONG;
    Level : UCHAR;
    Reserved1 : UCHAR;
    LoggerId : USHORT;
    EnableProperty : ULONG;
    Reserved2 : ULONG;
    MatchAnyKeyword : ULONGLONG;
    MatchAllKeyword : ULONGLONG;
  end;

  TRACE_ENABLE_INFO = _TRACE_ENABLE_INFO;
  PTRACE_ENABLE_INFO = ^TRACE_ENABLE_INFO;

  TTraceEnableInfo = TRACE_ENABLE_INFO;
  PTraceEnableInfo = ^TTraceEnableInfo;

//
// Instance Information for Provider 
// Used on Vista and above
//
  _TRACE_PROVIDER_INSTANCE_INFO = record
    NextOffset : ULONG;
    EnableCount : ULONG;
    Pid : ULONG;
    Flags : ULONG;
  end;

  TRACE_PROVIDER_INSTANCE_INFO  = _TRACE_PROVIDER_INSTANCE_INFO;
  PTRACE_PROVIDER_INSTANCE_INFO = ^TRACE_PROVIDER_INSTANCE_INFO;

  TTraceProviderInstanceInfo = TRACE_PROVIDER_INSTANCE_INFO;
  PTraceProviderInstanceInfo = ^TTraceProviderInstanceInfo;

//
// GUID Information Used on Vista and above
//
  _TRACE_GUID_INFO = record
    InstanceCount : ULONG;
    Reserved : ULONG ;
  end;

  TRACE_GUID_INFO = _TRACE_GUID_INFO;
  PTRACE_GUID_INFO = ^TRACE_GUID_INFO;

  TTraceGUIDInfo = TRACE_GUID_INFO;
  PTraceGUIDInfo = ^TTraceGUIDInfo;

//
// An EVENT_TRACE consists of a fixed header (EVENT_TRACE_HEADER) and
// optionally a variable portion pointed to by MofData. The datablock
// layout of the variable portion is unknown to the Logger and must
// be obtained from WBEM CIMOM database.
//

  _EVENT_TRACE = record
    Header : EVENT_TRACE_HEADER;             // Event trace header
    InstanceId : ULONG;         // Instance Id of this event
    ParentInstanceId : ULONG;   // Parent Instance Id.
    ParentGuid : GUID;         // Parent Guid;
    MofData : PVOID;            // Pointer to Variable Data
    MofLength : ULONG;          // Variable Datablock Length
    case Boolean of
        true  : (ClientContext : ULONG);
        false : (BufferContext : ETW_BUFFER_CONTEXT);
  end;

  EVENT_TRACE = _EVENT_TRACE;
  PEVENT_TRACE = EVENT_TRACE;

  TEventTrace = EVENT_TRACE;
  PEventTrace = ^TEventTrace;



  //defined in JwaEventDefs.pas
  {_EVENT_RECORD = record}

  //Predefinitions - defined later here
  PEVENT_TRACE_LOGFILEW = ^EVENT_TRACE_LOGFILEW;
  PEVENT_TRACE_LOGFILEA = ^EVENT_TRACE_LOGFILEA;


  EVENT_TRACE_BUFFER_CALLBACKW = function (Logfile : PEVENT_TRACE_LOGFILEW) : ULONG; stdcall;
  PEVENT_TRACE_BUFFER_CALLBACKW = EVENT_TRACE_BUFFER_CALLBACKW;

  EVENT_TRACE_BUFFER_CALLBACKA = function (Logfile : PEVENT_TRACE_LOGFILEA) : ULONG; stdcall;
  PEVENT_TRACE_BUFFER_CALLBACKA = EVENT_TRACE_BUFFER_CALLBACKA;

  EVENT_CALLBACK = procedure (pEvent : PEVENT_TRACE); stdcall;
  PEVENT_CALLBACK = EVENT_CALLBACK;

  EVENT_RECORD_CALLBACK = procedure (EventRecord : PEVENT_RECORD); stdcall;
  PEVENT_RECORD_CALLBACK = EVENT_RECORD_CALLBACK;

//
// Prototype for service request callback. Data providers register with WMI
// by passing a service request callback function that is called for all
// wmi requests.

  WMIDPREQUEST = function (
    {IN} RequestCode : WMIDPREQUESTCODE;
    {IN} RequestContext : PVOID ;
    {IN OUT} BufferSize : PULONG;
    {IN OUT} Buffer : PVOID
    ) : ULONG; stdcall;


  _EVENT_TRACE_LOGFILEW = record
    LogFileName : LPWSTR;      // Logfile Name
    LoggerName  : LPWSTR;       // LoggerName
    CurrentTime : LONGLONG;      // timestamp of last event
    BuffersRead : ULONG;      // buffers read to date
    LogFileMode : record
      case Boolean of
        // Mode of the logfile
        true : (LogFileMode : ULONG);
        // Processing flags used on Vista and above
        false : (ProcessTraceMode : ULONG);
    end;

    CurrentEvent  : EVENT_TRACE;     // Current Event from this stream.
    LogfileHeader : TRACE_LOGFILE_HEADER;    // logfile header structure
    BufferCallback : PEVENT_TRACE_BUFFER_CALLBACKW;             // callback before each buffer is read
    //
    // following variables are filled for BufferCallback.
    //
    BufferSize : ULONG;
    Filled : ULONG;
    EventsLost : ULONG;
    //
    // following needs to be propaged to each buffer
    //
    EventCallback : record
      case Boolean of
        // Callback with EVENT_TRACE
        true : (EventCallback : PEVENT_CALLBACK);
        // Callback with EVENT_RECORD on Vista and above
        false : (EventRecordCallback : PEVENT_RECORD_CALLBACK);
    end;

    IsKernelTrace : ULONG;    // TRUE for kernel logfile

    Context : PVOID;          // reserved for internal use
  end;
  EVENT_TRACE_LOGFILEW = _EVENT_TRACE_LOGFILEW;

  _EVENT_TRACE_LOGFILEA = record
    LogFileName : LPSTR;      // Logfile Name
    LoggerName  : LPSTR;       // LoggerName
    CurrentTime : LONGLONG;      // timestamp of last event
    BuffersRead : ULONG;      // buffers read to date
    LogFileMode : record
      case Boolean of
        // Mode of the logfile
        true : (LogFileMode : ULONG);
        // Processing flags used on Vista and above
        false : (ProcessTraceMode : ULONG);
    end;

    CurrentEvent  : EVENT_TRACE;     // Current Event from this stream.
    LogfileHeader : TRACE_LOGFILE_HEADER;    // logfile header structure
    BufferCallback : PEVENT_TRACE_BUFFER_CALLBACKW;             // callback before each buffer is read
    //
    // following variables are filled for BufferCallback.
    //
    BufferSize : ULONG;
    Filled : ULONG;
    EventsLost : ULONG;
    //
    // following needs to be propaged to each buffer
    //
    EventCallback : record
      case Boolean of
        // Callback with EVENT_TRACE
        true : (EventCallback : PEVENT_CALLBACK);
        // Callback with EVENT_RECORD on Vista and above
        false : (EventRecordCallback : PEVENT_RECORD_CALLBACK);
    end;

    IsKernelTrace : ULONG;    // TRUE for kernel logfile

    Context : PVOID;          // reserved for internal use
  end;

  EVENT_TRACE_LOGFILEA = _EVENT_TRACE_LOGFILEA;

//
// Define generic structures
//

{$IFDEF UNICODE}
type
  PEVENT_TRACE_BUFFER_CALLBACK    = PEVENT_TRACE_BUFFER_CALLBACKW;
  EVENT_TRACE_LOGFILE             = EVENT_TRACE_LOGFILEW;
  PEVENT_TRACE_LOGFILE            = PEVENT_TRACE_LOGFILEW;
const
  KERNEL_LOGGER_NAME              = KERNEL_LOGGER_NAMEW;
  GLOBAL_LOGGER_NAME              = GLOBAL_LOGGER_NAMEW;
  EVENT_LOGGER_NAME               = EVENT_LOGGER_NAMEW;

{$ELSE}
type
  PEVENT_TRACE_BUFFER_CALLBACK    = PEVENT_TRACE_BUFFER_CALLBACKA;
  EVENT_TRACE_LOGFILE             = EVENT_TRACE_LOGFILEA;
  PEVENT_TRACE_LOGFILE            = PEVENT_TRACE_LOGFILEA;
const
  KERNEL_LOGGER_NAME              = KERNEL_LOGGER_NAMEA;
  GLOBAL_LOGGER_NAME              = GLOBAL_LOGGER_NAMEA;
  EVENT_LOGGER_NAME               = EVENT_LOGGER_NAMEA;

{$ENDIF UNICODE}




//
// Logger control APIs
//

//
// Use the routine below to start an event trace session
//

// ULONG
// StartTrace(
//      OUT PTRACEHANDLE TraceHandle,
//      IN LPTSTR InstanceName,
//      IN OUT PEVENT_TRACE_PROPERTIES Properties
//      );


function StartTraceW(
    {__out} TraceHandle : PTRACEHANDLE;
    {__in} InstanceName : LPCWSTR;
    {__inout}var Properties : EVENT_TRACE_PROPERTIES) : ULONG; stdcall;

function StartTraceA(
    {__out} TraceHandle : PTRACEHANDLE;
    {__in} InstanceName : LPCSTR;
    {__inout} var Properties : EVENT_TRACE_PROPERTIES
    ) : ULONG; stdcall;

function StartTrace(
    {__out} TraceHandle : PTRACEHANDLE;
    {__in} InstanceName : LPTSTR;
    {__inout} var Properties : EVENT_TRACE_PROPERTIES
    ) : ULONG; stdcall;

//
// Use the routine below to stop an event trace session
//

//
// ULONG
// StopTrace(
//      IN TRACEHANDLE TraceHandle,
//      IN LPTSTR InstanceName,
//      IN OUT PEVENT_TRACE_PROPERTIES Properties
//      );

function StopTraceW(
    {__in} TraceHandle : TRACEHANDLE;
    {__in_opt} InstanceName : LPCWSTR;
    {__inout}var Properties : EVENT_TRACE_PROPERTIES
    ) : ULONG; stdcall;

function StopTraceA(
    {__in} TraceHandle : TRACEHANDLE;
    {__in_opt} InstanceName : LPCSTR;
    {__inout} var Properties : EVENT_TRACE_PROPERTIES
    ) : ULONG; stdcall;

function StopTrace(
    {__in} TraceHandle : TRACEHANDLE;
    {__in_opt} InstanceName : LPTSTR;
    {__inout} var Properties : EVENT_TRACE_PROPERTIES
    ) : ULONG; stdcall;


//
// Use the routine below to query the properties of an event trace session
//

// ULONG
// QueryTrace(
//      IN TRACEHANDLE TraceHandle,
//      IN LPTSTR InstanceName,
//      IN OUT PEVENT_TRACE_PROPERTIES Properties
//      );

function QueryTraceW(
    {__in} TraceHandle : TRACEHANDLE;
    {__in_opt} InstanceName : LPCWSTR;
    {__inout} var Properties : EVENT_TRACE_PROPERTIES
    ) : ULONG; stdcall;

function QueryTraceA(
    {__in} TraceHandle : TRACEHANDLE;
    {__in_opt} InstanceName : LPCSTR;
    {__inout} var Properties : EVENT_TRACE_PROPERTIES
    ) : ULONG; stdcall;

function QueryTrace(
    {__in} TraceHandle : TRACEHANDLE;
    {__in_opt} InstanceName : LPTSTR;
    {__inout} var Properties : EVENT_TRACE_PROPERTIES
    ) : ULONG; stdcall;
//
// Use the routine below to update certain properties of an event trace session
//

// ULONG
// UpdateTrace(
//      IN (PTRACEHANDLE TraceHandle,
//      IN LPTSTR InstanceName,
//      IN OUT PEVENT_TRACE_PROPERTIES Properties
//      );


function UpdateTraceW(
    {__in} TraceHandle : TRACEHANDLE;
    {__in_opt} InstanceName : LPCWSTR;
    {__inout} var Properties : EVENT_TRACE_PROPERTIES
    ) : ULONG; stdcall;

function UpdateTraceA(
    {__in} TraceHandle : TRACEHANDLE;
    {__in_opt} InstanceName : LPCSTR;
    {__inout} var Properties : EVENT_TRACE_PROPERTIES
    ) : ULONG; stdcall;

function UpdateTrace(
    {__in} TraceHandle : TRACEHANDLE;
    {__in_opt} InstanceName : LPTSTR;
    {__inout} var Properties : EVENT_TRACE_PROPERTIES
    ) : ULONG; stdcall;
//
// Use the routine below to request that all active buffers an event trace
// session be "flushed", or written out.
//

// ULONG
// FlushTrace(
//      IN TRACEHANDLE TraceHandle,
//      IN LPTSTR InstanceName,
//      IN OUT PEVENT_TRACE_PROPERTIES Properties
//      );

{$IFDEF WINXP_UP}

function FlushTraceW(
    {__in} TraceHandle : TRACEHANDLE;
    {__in_opt} InstanceName : LPCWSTR;
    {__inout} var Properties : EVENT_TRACE_PROPERTIES
    ) : ULONG; stdcall;

function FlushTraceA(
    {__in} TraceHandle : TRACEHANDLE;
    {__in_opt} InstanceName : LPCSTR;
    {__inout} var Properties : EVENT_TRACE_PROPERTIES
    ) : ULONG; stdcall;

function FlushTrace(
    {__in} TraceHandle : TRACEHANDLE;
    {__in_opt} InstanceName : LPTSTR;
    {__inout} var Properties : EVENT_TRACE_PROPERTIES
    ) : ULONG; stdcall;

{$ENDIF WINXP_UP}

//
// Generic trace control routine
//
function ControlTraceW(
    {__in} TraceHandle : TRACEHANDLE;
    {__in_opt} InstanceName : LPCWSTR;
    {__inout} var Properties : EVENT_TRACE_PROPERTIES;
    {__in} ControlCode : ULONG
    ) : ULONG; stdcall;

function ControlTraceA(
    {__in} TraceHandle : TRACEHANDLE;
    {__in_opt} InstanceName : LPCSTR;
    {__inout} var Properties : EVENT_TRACE_PROPERTIES;
    {__in} ControlCode : ULONG
    ) : ULONG; stdcall;

function ControlTrace(
    {__in} TraceHandle : TRACEHANDLE;
    {__in_opt} InstanceName : LPTSTR;
    {__inout} var Properties : EVENT_TRACE_PROPERTIES;
    {__in} ControlCode : ULONG
    ) : ULONG; stdcall;

//
// ULONG
// QueryAllTraces(
//  OUT PEVENT_TRACE_PROPERTIES *PropertyArray,
//  IN ULONG PropertyArrayCount,
//  OUT PULONG LoggerCount
//  );
//

function QueryAllTracesW(
    {__out_ecount(PropertyArrayCount)} var PropertyArray : PEVENT_TRACE_PROPERTIES;
    {__in}  PropertyArrayCount : ULONG;
    {__out} out LoggerCount : ULONG) : ULONG; stdcall;

function QueryAllTracesA(
    {__out_ecount(PropertyArrayCount)}var PropertyArray : PEVENT_TRACE_PROPERTIES;
    {__in}  PropertyArrayCount : ULONG;
    {__out} LoggerCount : ULONG) : ULONG; stdcall;

function QueryAllTraces(
    {__out_ecount(PropertyArrayCount)}var PropertyArray : PEVENT_TRACE_PROPERTIES;
    {__in}  PropertyArrayCount : ULONG;
    {__out} LoggerCount : ULONG) : ULONG; stdcall;


//
// Data Provider Enable APIs
//

function EnableTrace(
    {__in} Enable : ULONG;
    {__in} EnableFlag : ULONG;
    {__in} EnableLevel : ULONG;
    {__in} ControlGuid : PGUID;
    {__in} TraceHandle : TRACEHANDLE) : ULONG; stdcall;



{$IFDEF WINVISTA_UP}
function EnableTraceEx(
    {__in} ProviderId : PGUID;
    {__in_opt} SourceId : PGUID;
    {__in} TraceHandle : TRACEHANDLE;
    {__in} IsEnabled : ULONG;
    {__in} Level : UCHAR;
    {__in} MatchAnyKeyword : ULONGLONG;
    {__in} MatchAllKeyword : ULONGLONG;
    {__in} EnableProperty : ULONG;
    {__in_opt} EnableFilterDesc : PEVENT_FILTER_DESCRIPTOR) : ULONG; stdcall;
{$ENDIF}


type
  _TRACE_QUERY_INFO_CLASS =
    (TraceGuidQueryList,
    TraceGuidQueryInfo,
    TraceGuidQueryProcess);
  TRACE_QUERY_INFO_CLASS = _TRACE_QUERY_INFO_CLASS;

{$IFDEF WINVISTA_UP}
function EnumerateTraceGuidsEx(
    {__in} TraceQueryInfoClass : TRACE_QUERY_INFO_CLASS;
    {__in_bcount_opt(InBufferSize)} InBuffer : PVOID;
    {__in} InBufferSize: ULONG;
    {__out_bcount_opt(OutBufferSize)} OutBuffer : PVOID;
    {__in} OutBufferSize : ULONG;
    {__out} out ReturnLength : ULONG) : ULONG; stdcall;
{$ENDIF}

//
// Data Provider APIs
//

function CreateTraceInstanceId(
    {__in} RegHandle : HANDLE;
    {__inout} var pInstInfo : EVENT_INSTANCE_INFO) : ULONG; stdcall;

//
// Use the routine below to generate and record an event trace
//

function TraceEvent(
    {__in} TraceHandle : TRACEHANDLE;
    {__inout} var EventTrace : EVENT_TRACE_HEADER) : ULONG; stdcall;

function TraceEventInstance(
    {__in} TraceHandle : TRACEHANDLE;
    {__in} EventTrace : PEVENT_INSTANCE_HEADER;
    {__in} pInstInfo : PEVENT_INSTANCE_INFO;
    {__in_opt} pParentInstInfo : PEVENT_INSTANCE_INFO) : ULONG; stdcall;

//
// Use the routine below to register a guid for tracing
//

//
// ULONG
// RegisterTraceGuids(
//  IN WMIDPREQUEST  RequestAddress,
//  IN PVOID         RequestContext,
//  IN LPCGUID       ControlGuid,
//  IN ULONG         GuidCount,
//  IN PTRACE_GUID_REGISTRATION TraceGuidReg,
//  IN LPCTSTR       MofImagePath,
//  IN LPCTSTR       MofResourceName,
//  OUT PTRACEHANDLE RegistrationHandle
//  ) : ULONG; stdcall;
//

function RegisterTraceGuidsW(
    {__in} RequestAddress : WMIDPREQUEST;
    {__in_opt} RequestContext : PVOID;
    {__in} ControlGuid : PGUID;
    {__in} GuidCount : ULONG;
    {__in_ecount_opt(GuidCount)} TraceGuidReg : PTRACE_GUID_REGISTRATION;
    {__in_opt} MofImagePath : LPCWSTR;
    {__in_opt} MofResourceName : LPCWSTR;
    {__out} var RegistrationHandle : TRACEHANDLE) : ULONG; stdcall;



function RegisterTraceGuidsA(
    {__in} RequestAddress : WMIDPREQUEST;
    {__in_opt} RequestContext : PVOID;
    {__in} ControlGuid : PGUID;
    {__in} GuidCount : ULONG;
    {__in_ecount_opt(GuidCount)} TraceGuidReg : PTRACE_GUID_REGISTRATION;
    {__in_opt} MofImagePath : LPCSTR;
    {__in_opt} MofResourceName : LPCSTR;
    {__out} var RegistrationHandle : TRACEHANDLE) : ULONG; stdcall;

function RegisterTraceGuids(
    {__in} RequestAddress : WMIDPREQUEST;
    {__in_opt} RequestContext : PVOID;
    {__in} ControlGuid : PGUID;
    {__in} GuidCount : ULONG;
    {__in_ecount_opt(GuidCount)} TraceGuidReg : PTRACE_GUID_REGISTRATION;
    {__in_opt} MofImagePath : LPCSTR;
    {__in_opt} MofResourceName : LPTSTR;
    {__out} var RegistrationHandle : TRACEHANDLE) : ULONG; stdcall;


{$IFDEF WINXP_UP}
function EnumerateTraceGuids(
    {__inout_ecount(PropertyArrayCount)} var GuidPropertiesArray : PTRACE_GUID_PROPERTIES;
    {__in} PropertyArrayCount : ULONG;
    {__out} var GuidCount : ULONG) : ULONG; stdcall;
{$ENDIF WINXP_UP}


function UnregisterTraceGuids(
    {__in}RegistrationHandle : TRACEHANDLE) : ULONG; stdcall;

function GetTraceLoggerHandle(
    {__in}Buffer : PVOID) : ULONG; stdcall;

function GetTraceEnableLevel(
    {__in} _TraceHandle : TRACEHANDLE) : ULONG; stdcall;

function GetTraceEnableFlags(
    {__in} _TraceHandle : TRACEHANDLE) : ULONG; stdcall;

//
// Data Consumer APIs and structures start here
//

//
// TRACEHANDLE
// OpenTrace(
//  IN OUT PEVENT_TRACE_LOGFILE Logfile
//  ) : ULONG; stdcall;
//

function OpenTraceA(
    {__inout} var Logfile : EVENT_TRACE_LOGFILEA) : ULONG; stdcall;

function OpenTraceW(
    {__inout} var Logfile : EVENT_TRACE_LOGFILEW) : ULONG; stdcall;

function OpenTrace(
    {__inout} var Logfile : EVENT_TRACE_LOGFILE) : ULONG; stdcall;


function ProcessTrace(
    {__in_ecount(HandleCount)} HandleArray : PTRACEHANDLE;
    {__in} HandleCount : ULONG;
    {__in_opt} StartTime : LPFILETIME;
    {__in_opt} EndTime : LPFILETIME) : ULONG; stdcall;

function CloseTrace(
    {__in} _TraceHandle : TRACEHANDLE) : ULONG; stdcall;

function SetTraceCallback(
    {__in} pGuid : PGUID;
    {__in} EventCallback : PEVENT_CALLBACK) : ULONG; stdcall;

function RemoveTraceCallback (
    {__in} const pGuid : PGUID
    ) : ULONG; stdcall;

//
// The routines for tracing Messages follow
//
//function TraceMessage(
//    {__in} LoggerHandle : TRACEHANDLE;
//    {__in} MessageFlags : ULONG;
//    {__in} MessageGuid : LPGUID;
//    {__in} MessageNumber : USHORT
//      ...
//) : ULONG; stdcall;

function TraceMessageVa(
    {__in} LoggerHandle : TRACEHANDLE;
    {__in} MessageFlags : ULONG;
    {__in} MessageGuid : LPGUID;
    {__in} MessageNumber : USHORT;
    {__in} MessageArgList : Pointer) : ULONG; cdecl;

//replaced routine defined in Delphi - not exported!
function TraceMessage(
    {__in} LoggerHandle : TRACEHANDLE;
    {__in} MessageFlags : ULONG;
    {__in} MessageGuid : LPGUID;
    {__in} MessageNumber : USHORT;
    {__in} params: array of const) : ULONG;







{$ENDIF JWA_IMPLEMENTATIONSECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
implementation
uses JwaWinDllNames;
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}
//add here implementation stuff

{$IFNDEF JWA_INCLUDEMODE}
const
  advapi32 = 'Advapi32.dll';
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

procedure DEFINE_TRACE_MOF_FIELD(var MOF : PMOFField;
  const ptr : ULONG64; const length : ULONG; const Typ : ULONG);
begin
  MOF^.DataPtr := ptr;
  MOF^.Length := length;
  MOF^.DataType := Typ;
end;

function TraceMessage(
    {__in} LoggerHandle : TRACEHANDLE;
    {__in} MessageFlags : ULONG;
    {__in} MessageGuid : LPGUID;
    {__in} MessageNumber : USHORT;
    {__in} params: array of const
  ) : ULONG;
var
  pdw1, pdw2: PDWORD;
  i: integer;
begin
  pdw1 := nil;

  if High(params) >= 0 then
    GetMem(pdw1, (High(params) + 1) * sizeof(Pointer));
  pdw2 := pdw1;

  for i := 0 to High(params) do begin
    pdw2^ := PDWORD(@params[i])^;
    inc(pdw2);
  end;

  try
    result := TraceMessageVa(LoggerHandle, MessageFlags, MessageGuid, MessageNumber, pdw1);
  finally
    if (pdw1 <> nil) then
      FreeMem(pdw1);
  end;
end;


{$IFDEF DYNAMIC_LINK}
var
  _StartTraceW: Pointer;

function StartTraceW;
begin
  GetProcedureAddress(_StartTraceW, advapi32, 'StartTraceW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StartTraceW]
  end;
end;

var
  _StartTraceA: Pointer;

function StartTraceA;
begin
  GetProcedureAddress(_StartTraceA, advapi32, 'StartTraceA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StartTraceA]
  end;
end;

var
  _StartTrace: Pointer;

function StartTrace;
begin
  GetProcedureAddress(_StartTrace, advapi32, 'StartTrace'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StartTrace]
  end;
end;

var
  _StopTraceW: Pointer;

function StopTraceW;
begin
  GetProcedureAddress(_StopTraceW, advapi32, 'StopTraceW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StopTraceW]
  end;
end;

var
  _StopTraceA: Pointer;

function StopTraceA;
begin
  GetProcedureAddress(_StopTraceA, advapi32, 'StopTraceA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StopTraceA]
  end;
end;

var
  _StopTrace: Pointer;

function StopTrace;
begin
  GetProcedureAddress(_StopTrace, advapi32, 'StopTrace'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StopTrace]
  end;
end;

var
  _QueryTraceW: Pointer;

function QueryTraceW;
begin
  GetProcedureAddress(_QueryTraceW, advapi32, 'QueryTraceW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_QueryTraceW]
  end;
end;

var
  _QueryTraceA: Pointer;

function QueryTraceA;
begin
  GetProcedureAddress(_QueryTraceA, advapi32, 'QueryTraceA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_QueryTraceA]
  end;
end;

var
  _QueryTrace: Pointer;

function QueryTrace;
begin
  GetProcedureAddress(_QueryTrace, advapi32, 'QueryTrace'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_QueryTrace]
  end;
end;

var
  _UpdateTraceW: Pointer;

function UpdateTraceW;
begin
  GetProcedureAddress(_UpdateTraceW, advapi32, 'UpdateTraceW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UpdateTraceW]
  end;
end;

var
  _UpdateTraceA: Pointer;

function UpdateTraceA;
begin
  GetProcedureAddress(_UpdateTraceA, advapi32, 'UpdateTraceA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UpdateTraceA]
  end;
end;

var
  _UpdateTrace: Pointer;

function UpdateTrace;
begin
  GetProcedureAddress(_UpdateTrace, advapi32, 'UpdateTrace'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UpdateTrace]
  end;
end;


{$IFDEF WINXP_UP}
var
  _FlushTraceW: Pointer;

function FlushTraceW;
begin
  GetProcedureAddress(_FlushTraceW, advapi32, 'FlushTraceW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FlushTraceW]
  end;
end;

var
  _FlushTraceA: Pointer;

function FlushTraceA;
begin
  GetProcedureAddress(_FlushTraceA, advapi32, 'FlushTraceA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FlushTraceA]
  end;
end;
var
  _FlushTrace: Pointer;

function FlushTrace;
begin
  GetProcedureAddress(_FlushTrace, advapi32, 'FlushTrace'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FlushTrace]
  end;
end;

{$ENDIF WINXP_UP}


var
  _ControlTraceW: Pointer;

function ControlTraceW;
begin
  GetProcedureAddress(_ControlTraceW, advapi32, 'ControlTraceW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ControlTraceW]
  end;
end;

var
  _ControlTraceA: Pointer;

function ControlTraceA;
begin
  GetProcedureAddress(_ControlTraceA, advapi32, 'ControlTraceA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ControlTraceA]
  end;
end;

var
  _ControlTrace: Pointer;

function ControlTrace;
begin
  GetProcedureAddress(_ControlTrace, advapi32, 'ControlTrace'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ControlTrace]
  end;
end;

var
  _QueryAllTracesW: Pointer;

function QueryAllTracesW;
begin
  GetProcedureAddress(_QueryAllTracesW, advapi32, 'QueryAllTracesW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_QueryAllTracesW]
  end;
end;

var
  _QueryAllTracesA: Pointer;

function QueryAllTracesA;
begin
  GetProcedureAddress(_QueryAllTracesA, advapi32, 'QueryAllTracesA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_QueryAllTracesA]
  end;
end;

var
  _QueryAllTraces: Pointer;

function QueryAllTraces;
begin
  GetProcedureAddress(_QueryAllTraces, advapi32, 'QueryAllTraces'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_QueryAllTraces]
  end;
end;

var
  _EnableTrace: Pointer;

function EnableTrace;
begin
  GetProcedureAddress(_EnableTrace, advapi32, 'EnableTrace');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EnableTrace]
  end;
end;

{$IFDEF WINVISTA_UP}
var
  _EnableTraceEx: Pointer;

function EnableTraceEx;
begin
  GetProcedureAddress(_EnableTraceEx, advapi32, 'EnableTraceEx');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EnableTraceEx]
  end;
end;

var
  _EnumerateTraceGuidsEx: Pointer;

function EnumerateTraceGuidsEx;
begin
  GetProcedureAddress(_EnumerateTraceGuidsEx, advapi32, 'EnumerateTraceGuidsEx');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EnumerateTraceGuidsEx]
  end;
end;
{$ENDIF WINVISTA_UP}


var
  _CreateTraceInstanceId: Pointer;

function CreateTraceInstanceId;
begin
  GetProcedureAddress(_CreateTraceInstanceId, advapi32, 'CreateTraceInstanceId');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateTraceInstanceId]
  end;
end;

var
  _TraceEvent: Pointer;

function TraceEvent;
begin
  GetProcedureAddress(_TraceEvent, advapi32, 'TraceEvent');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TraceEvent]
  end;
end;

var
  _TraceEventInstance: Pointer;

function TraceEventInstance;
begin
  GetProcedureAddress(_TraceEventInstance, advapi32, 'TraceEventInstance');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TraceEventInstance]
  end;
end;

var
  _RegisterTraceGuidsW: Pointer;

function RegisterTraceGuidsW;
begin
  GetProcedureAddress(_RegisterTraceGuidsW, advapi32, 'RegisterTraceGuidsW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegisterTraceGuidsW]
  end;
end;

var
  _RegisterTraceGuidsA: Pointer;

function RegisterTraceGuidsA;
begin
  GetProcedureAddress(_RegisterTraceGuidsA, advapi32, 'RegisterTraceGuidsA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegisterTraceGuidsA]
  end;
end;

var
  _RegisterTraceGuids: Pointer;

function RegisterTraceGuids;
begin
  GetProcedureAddress(_RegisterTraceGuids, advapi32, 'RegisterTraceGuids'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegisterTraceGuids]
  end;
end;


{$IFDEF WINXP_UP}

var
  _EnumerateTraceGuids: Pointer;

function EnumerateTraceGuids;
begin
  GetProcedureAddress(_EnumerateTraceGuids, advapi32, 'EnumerateTraceGuids');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EnumerateTraceGuids]
  end;
end;
{$ENDIF WINXP_UP}

var
  _UnregisterTraceGuids: Pointer;

function UnregisterTraceGuids;
begin
  GetProcedureAddress(_UnregisterTraceGuids, advapi32, 'UnregisterTraceGuids');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UnregisterTraceGuids]
  end;
end;

var
  _GetTraceLoggerHandle: Pointer;

function GetTraceLoggerHandle;
begin
  GetProcedureAddress(_GetTraceLoggerHandle, advapi32, 'GetTraceLoggerHandle');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetTraceLoggerHandle]
  end;
end;

var
  _GetTraceEnableLevel: Pointer;

function GetTraceEnableLevel;
begin
  GetProcedureAddress(_GetTraceEnableLevel, advapi32, 'GetTraceEnableLevel');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetTraceEnableLevel]
  end;
end;

var
  _GetTraceEnableFlags: Pointer;

function GetTraceEnableFlags;
begin
  GetProcedureAddress(_GetTraceEnableFlags, advapi32, 'GetTraceEnableFlags');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetTraceEnableFlags]
  end;
end;

var
  _OpenTraceA: Pointer;

function OpenTraceA;
begin
  GetProcedureAddress(_OpenTraceA, advapi32, 'OpenTraceA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_OpenTraceA]
  end;
end;

var
  _OpenTraceW: Pointer;

function OpenTraceW;
begin
  GetProcedureAddress(_OpenTraceW, advapi32, 'OpenTraceW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_OpenTraceW]
  end;
end;

var
  _OpenTrace: Pointer;

function OpenTrace;
begin
  GetProcedureAddress(_OpenTrace, advapi32, 'OpenTrace'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_OpenTrace]
  end;
end;

var
  _ProcessTrace: Pointer;

function ProcessTrace;
begin
  GetProcedureAddress(_ProcessTrace, advapi32, 'ProcessTrace');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ProcessTrace]
  end;
end;

var
  _CloseTrace: Pointer;

function CloseTrace;
begin
  GetProcedureAddress(_CloseTrace, advapi32, 'CloseTrace');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CloseTrace]
  end;
end;

var
  _SetTraceCallback: Pointer;

function SetTraceCallback;
begin
  GetProcedureAddress(_SetTraceCallback, advapi32, 'SetTraceCallback');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetTraceCallback]
  end;
end;

var
  _RemoveTraceCallback : Pointer;

function RemoveTraceCallback ;
begin
  GetProcedureAddress(_RemoveTraceCallback , advapi32, 'RemoveTraceCallback ');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RemoveTraceCallback ]
  end;
end;

var                                                   
  _TraceMessageVa: Pointer;

function TraceMessageVa;
begin
  GetProcedureAddress(_TraceMessageVa, advapi32, 'TraceMessageVa');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TraceMessageVa]
  end;
end;



{$ELSE}

function StartTraceW; external advapi32 name 'StartTraceW';
function StartTraceA; external advapi32 name 'StartTraceA';
function StartTrace; external advapi32 name 'StartTrace'+AWSuffix;

function StopTraceW; external advapi32 name 'StopTraceW';
function StopTraceA; external advapi32 name 'StopTraceA';
function StopTrace; external advapi32 name 'StopTraceA'+AWSuffix;


function QueryTraceW; external advapi32 name 'QueryTraceW';
function QueryTraceA; external advapi32 name 'QueryTraceA';
function QueryTrace; external advapi32 name 'QueryTrace'+AWSuffix;

function UpdateTraceW; external advapi32 name 'UpdateTraceW';
function UpdateTraceA; external advapi32 name 'UpdateTraceA';
function UpdateTrace; external advapi32 name 'UpdateTrace'+AWSuffix;

{$IFDEF WINXP_UP}
function FlushTraceW; external advapi32 name 'FlushTraceW';
function FlushTraceA; external advapi32 name 'FlushTraceA';
function FlushTrace; external advapi32 name 'FlushTrace'+AWSuffix;

{$ENDIF WINXP_UP}
function ControlTraceW; external advapi32 name 'ControlTraceW';
function ControlTraceA; external advapi32 name 'ControlTraceA';
function ControlTrace; external advapi32 name 'ControlTraceA'+AWSuffix;

function QueryAllTracesW; external advapi32 name 'QueryAllTracesW';
function QueryAllTracesA; external advapi32 name 'QueryAllTracesA';
function QueryAllTraces; external advapi32 name 'QueryAllTraces'+AWSuffix;

function EnableTrace; external advapi32 name 'EnableTrace';

{$IFDEF WINVISTA_UP}
function EnableTraceEx; external advapi32 name 'EnableTraceEx';
function EnumerateTraceGuidsEx; external advapi32 name 'EnumerateTraceGuidsEx';
{$ENDIF}
function CreateTraceInstanceId; external advapi32 name 'CreateTraceInstanceId';
function TraceEvent; external advapi32 name 'TraceEvent';
function TraceEventInstance; external advapi32 name 'TraceEventInstance';

function RegisterTraceGuidsW; external advapi32 name 'RegisterTraceGuidsW';
function RegisterTraceGuidsA; external advapi32 name 'RegisterTraceGuidsA';
function RegisterTraceGuids; external advapi32 name 'RegisterTraceGuids'+AWSuffix;
{$IFDEF WINXP_UP}
function EnumerateTraceGuids; external advapi32 name 'EnumerateTraceGuids';
{$ENDIF WINXP_UP}
function UnregisterTraceGuids; external advapi32 name 'UnregisterTraceGuids';
function GetTraceLoggerHandle; external advapi32 name 'GetTraceLoggerHandle';
function GetTraceEnableLevel; external advapi32 name 'GetTraceEnableLevel';
function GetTraceEnableFlags; external advapi32 name 'GetTraceEnableFlags';

function OpenTraceA; external advapi32 name 'OpenTraceA';
function OpenTraceW; external advapi32 name 'OpenTraceW';
function OpenTrace; external advapi32 name 'OpenTrace'+AWSuffix;

function ProcessTrace; external advapi32 name 'ProcessTrace';
function CloseTrace; external advapi32 name 'CloseTrace';
function SetTraceCallback; external advapi32 name 'SetTraceCallback';
function RemoveTraceCallback ; external advapi32 name 'RemoveTraceCallback';
function TraceMessageVa; external advapi32 name 'TraceMessageVa';
{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}




{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
