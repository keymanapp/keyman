{******************************************************************}
{ This Unit provides Delphi translations of some functions from    }
{ WinSta.dll and Utildll.                                          }
{ Most functions are undocumented and somehow related to           }
{ Terminal Server                                                  }
{                                                                  }
{ Author: Remko Weijnen (r dot weijnen at gmail dot com)           }
{ Documentation can be found at www.remkoweijnen.nl                }
{                                                                  }
{ The contents of this file are subject to                         }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/MPL/MPL-1.1.html                          }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{******************************************************************}

{$IFNDEF JWA_OMIT_SECTIONS}

unit JwaWinSta;


interface

{$I ..\Includes\JediAPILib.inc}

uses
  SysUtils, JwaWinType, // JwaWinType must be declared before JwaWinBase because of duplicate declaration of FILETIME
  JwaWinBase, JwaWinError, JwaNTStatus, JwaWinNT, JwaWinsock2,
  JwaWinSvc, JwaWtsApi32, JwaWinNLS, JwaNative, JwaBitFields;
{$ENDIF JWA_OMIT_SECTIONS}


{$IFNDEF JWA_IMPLEMENTATIONSECTION}
//==============================================================================
// Defines
//==============================================================================
const
  SERVERNAME_CURRENT = 0;

  // old (reversed) constants used for WinStationGetTermSrvCounters
{  TOTAL_SESSIONS_CREATED_COUNTER = 1;
  TOTAL_SESSIONS_DISCONNECTED_COUNTER = 2;
  TOTAL_SESSIONS_RECONNECTED_COUNTER = 3;
  TOTAL_SESSIONS_TOTAL_CONNECTED_NOW_COUNTER = 4;
  TOTAL_SESSIONS_TOTAL_DISCONNECTED_NOW_COUNTER = 5;
  TOTAL_SESSIONS_TOTAL_CONNECTED_NOW_COUNTER_2 = 6; //TermSrvSuccLocalLogons;
  TOTAL_SESSIONS_TOTAL_DISCONNECTED_NOW_COUNTER_2 = 7;}

  // Max lenght for ElapsedTimeString (server 2008 version of utildll
  // fixes size at 15, so that's assumed to be safe
  ELAPSED_TIME_STRING_LENGTH = 15;

  // WdFlag = WinStation Driver Flag, it is returned in class 3 (WdConfig)
  // of WinStationQueryInformation and has a different value which
  // depends on the protocol. WdFlag is also returned by QueryCurrentWinStation
  WD_FLAG_CONSOLE_XP = $24; // XP
  WD_FLAG_CONSOLE = $34; // 2003/2008
  WD_FLAG_RDP = $36; // XP/2003/2008
  WD_FLAG_ICA = $6E; // Citrix Presentation Server
//  (value from Citrix PS4, other versions could be different!)

const
// WdFlag: Driver flags. SHOULD be any bitwise OR combination of the following
// values.
  WDF_UNUSED = $1;             // Not used.
  WDF_SHADOW_SOURCE = $2;      // Valid shadow source.
  WDF_SHADOW_TARGET = $4;      // Valid shadow target.
  WDF_OTHER = $8;              // Other protocol.
  WDF_TSHARE = $10;            // Remote Protocol used by Terminal Services.
  WDF_DYNAMIC_RECONNECT = $20; // Session can resize display at reconnect.
  WDF_USER_VCIOCTL = $40;      // User mode applications can send virtual channel IOCTL
  WDF_SUBDESKTOP = $8000;      // Sub-desktop session.

  WDPREFIX_LENGTH = 12;
  STACK_ADDRESS_LENGTH = 128;
  MAX_BR_NAME = 65;
  DIRECTORY_LENGTH = 256;
  INITIALPROGRAM_LENGTH = 256;
{$IFNDEF JWA_INCLUDEMODE}
  AF_INET      = 2; // internetwork: UDP, TCP, etc.
  {$EXTERNALSYM AF_INET}
  AF_INET6     = 23; // Internetwork Version 6
  {$EXTERNALSYM AF_INET6}

  USERNAME_LENGTH = 20;
  DOMAIN_LENGTH = 17;
{$ENDIF JWA_INCLUDEMODE}
  PASSWORD_LENGTH = 14;
  NASISPECIFICNAME_LENGTH = 14;
  NASIUSERNAME_LENGTH = 47;
  NASIPASSWORD_LENGTH = 24;
  NASISESSIONNAME_LENGTH = 16;
  NASIFILESERVER_LENGTH = 47;

  CLIENTDATANAME_LENGTH = 7;
{$IFNDEF JWA_INCLUDEMODE}
  CLIENTNAME_LENGTH = 20;
  CLIENTADDRESS_LENGTH = 30;
{$ENDIF JWA_INCLUDEMODE}

  IMEFILENAME_LENGTH = 32;
  CLIENTLICENSE_LENGTH = 32;
  CLIENTMODEM_LENGTH = 40;
  CLIENT_PRODUCT_ID_LENGTH = 32;

  MAX_COUNTER_EXTENSIONS = 2; {/* actual value not known*/}


{$IFNDEF JWA_INCLUDEMODE}
  WINSTATIONNAME_LENGTH = 32;
{$ENDIF JWA_INCLUDEMODE}


type
  _WINSTATIONINFOCLASS = (
    WinStationCreateData,
    WinStationConfiguration,
    WinStationPdParams,
    WinStationWd,
    WinStationPd,
    WinStationPrinter,
    WinStationClient,
    WinStationModules,
    WinStationInformation,
    WinStationTrace,
    WinStationBeep,
    WinStationEncryptionOff,
    WinStationEncryptionPerm,
    WinStationNtSecurity, // vista returns Incorrect function
    WinStationUserToken,
    WinStationUnused1,
    WinStationVideoData, // vista returns Incorrect function
    WinStationInitialProgram,
    WinStationCd,
    WinStationSystemTrace,
    WinStationVirtualData,
    WinStationClientData,
    WinStationSecureDesktopEnter, // not supported on RDP (ica?)
    WinStationSecureDesktopExit, // not supported on RDP (ica?)
    WinStationLoadBalanceSessionTarget,
    WinStationLoadIndicator,
    WinStationShadowInfo,
    WinStationDigProductId, // vista returns Incorrect function
    WinStationLockedState,
    WinStationRemoteAddress,
    WinStationIdleTime,
    WinStationLastReconnectType,
    WinStationDisallowAutoReconnect,
    WinStationMprNotifyInfo,
    WinStationExecSrvSystemPipe,
    WinStationSmartCardAutoLogon,
    WinStationIsAdminLoggedOn,
    WinStationReconnectedFromId,
    WinStationEffectsPolicy
  );

  WINSTATIONINFOCLASS = _WINSTATIONINFOCLASS;
  TWinStationInfoClass = WINSTATIONINFOCLASS;

{ old declarations of infoclasses which were reversed:
  WinStationCreate = 0;
  WinStationClient = 1;
  WdConfig = 3;
  WinStationConfig = 6;
  WinStationInformation = 8;
  WinStationBeep = 10;  // Calls MessageBeep
  WinStationToken = 14;
  WinStationResolution = 16;
  WinStationShadowInformation = 26;
  WinStationProductId = 27;
  WinStationLock = 28; // Locks or Unlocks the WinStation
  WinStationRemoteAddress = 29;
  WinStationPipeInformation = 33; }

const
  SECONDS_PER_DAY = 86400;
  SECONDS_PER_HOUR = 3600;
  SECONDS_PER_MINUTE = 60;

  // shadow state constants (class 26)
  SHADOW_STATE_NONE = 0;
  SHADOW_STATE_SHADOWING = 1;
  SHADOW_STATE_BEING_SHADOWED = 2;

  // shadow mode constants (class 26)
  SHADOW_MODE_NONE_ALLOWED = 0;
  SHADOW_MODE_FULL_CONTROL_WITH_PERMISSION = 1;
  SHADOW_MODE_FULL_CONTROL_WITHOUT_PERMISSION = 2;
  SHADOW_MODE_VIEW_ONLY_WITH_PERMISSION = 3;
  SHADOW_MODE_VIEW_ONLY_WITHOUT_PERMISSION = 4;

type
  // This type is used for ElapsedTimeString
  TDiffTime = record
    wDays: Word;
    wHours: Word;
    wMinutes: Word;
    wSeconds: Word;
    wMilliseconds: Word;
  end;
  PDiffTime = ^TDiffTime;

  // This type is used for WinStationQueryLogonCredentialsW
  // dwType can be one of the types defined in JwaWinWlx
  // WLX_CREDENTIAL_TYPE_V1_0 or  WLX_CREDENTIAL_TYPE_V2_0 = 2
  _LOGON_CREDENTIALSW = record
    dwType: DWORD;
    pUsername: PWideChar;
    pDomain: PWideChar;
    pPassword: PWideChar;
    Unknown2 : DWORD;
    Unknown3 : DWORD;
    Unknown4: DWORD;
  end;
  PLOGON_CREDENTIALSW = ^_LOGON_CREDENTIALSW;
  TLogonCredentialsW = _LOGON_CREDENTIALSW;
  PLogonCredentialsW = PLOGON_CREDENTIALSW;

  // WinStationToken (14)
  // You must set ProcessId and ThreadId to the valid values
  // Function actually duplicates a token handle to the
  // process, which id (ProcessId and ThreadId) are set here.
  _WINSTATIONUSERTOKEN = record
    ProcessId: DWORD;
    ThreadId: DWORD;
    UserToken: HANDLE;
  end;
  TWinStationUserToken = _WINSTATIONUSERTOKEN;  {WinStationUserToken is alreay an enum}
  PWINSTATIONUSERTOKEN = ^_WINSTATIONUSERTOKEN;

  _WINSTA_USER_TOKEN = record
    ProcessId : DWORD;
    ThreadId : DWORD;
    TokenHandle : THandle;
  end;
  PWINSTA_USER_TOKEN = ^_WINSTA_USER_TOKEN;
  TWinstaUserToken = _WINSTA_USER_TOKEN;
  PWinstaUserToken = ^TWinstaUserToken;

  _LOADFACTORTYPE = (
    ErrorConstraint,
    PagedPoolConstraint,
    NonPagedPoolConstraint,
    AvailablePagesConstraint,
    SystemPtesConstraint,
    CPUConstraint  );
  LOADFACTORTYPE = _LOADFACTORTYPE;
  TLoadFactorType = _LOADFACTORTYPE;

  _WINSTATIONLOADINDICATORDATA = record
    RemainingSessionCapacity: ULONG;
    LoadFactor: LOADFACTORTYPE;
    TotalSessions: ULONG;
    DisconnectedSessions: ULONG;
    IdleCPU: LARGE_INTEGER;
    TotalCPU: LARGE_INTEGER;
    RawSessionCapacity: ULONG;
    reserved1: ULONG;
    SessionAvgPaged: ULONG;   { undocumented }
    reserved3: ULONG;
    SessionAvgCommit: ULONG;  { undocumented }
    SessionAvgPte: ULONG;     { undocumented }
    SessionAvgPaged2: ULONG;  { undocumented }
    reserved: array[0..2] of ULONG
  end {_WINSTATIONLOADINDICATORDATA};
  WINSTATIONLOADINDICATORDATA = _WINSTATIONLOADINDICATORDATA;
  TWinStationLoadIndicatorData = WINSTATIONLOADINDICATORDATA;
  PWINSTATIONLOADINDICATORDATA = ^_WINSTATIONLOADINDICATORDATA;

  _TS_UNICODE_STRING = record
    Length: USHORT;
    MaximumLength: USHORT;
    Buffer: PWSTR;
  end;
  TS_UNICODE_STRING = _TS_UNICODE_STRING;
  TTSUnicodeString = TS_UNICODE_STRING;

  _TS_SYS_PROCESS_INFORMATION = record
    NextEntryOffset: ULONG;
    NumberOfThreads: ULONG;
    SpareLi1: LARGE_INTEGER;
    SpareLi2: LARGE_INTEGER;
    SpareLi3: LARGE_INTEGER;
    CreateTime: LARGE_INTEGER;
    UserTime: LARGE_INTEGER;
    KernelTime: LARGE_INTEGER;
    ImageName: TS_UNICODE_STRING;
    BasePriority: LONG;
    UniqueProcessId: DWORD;
    InheritedFromUniqueProcessId: DWORD;
    HandleCount: ULONG;
    SessionId: ULONG;
    SpareUl3: ULONG;
    PeakVirtualSize: SIZE_T;
    VirtualSize: SIZE_T;
    PageFaultCount: ULONG;
    PeakWorkingSetSize: ULONG;
    WorkingSetSize: ULONG;
    QuotaPeakPagedPoolUsage: SIZE_T;
    QuotaPagedPoolUsage: SIZE_T;
    QuotaPeakNonPagedPoolUsage: SIZE_T;
    QuotaNonPagedPoolUsage: SIZE_T;
    PagefileUsage: SIZE_T;
    PeakPagefileUsage: SIZE_T;
    PrivatePageCount: SIZE_T;
  end;
  TS_SYS_PROCESS_INFORMATION = _TS_SYS_PROCESS_INFORMATION;
  PTS_SYS_PROCESS_INFORMATION = ^TS_SYS_PROCESS_INFORMATION;
  TTSSysProcessInformation = TS_SYS_PROCESS_INFORMATION;
  PTSSysProcessInformation = PTS_SYS_PROCESS_INFORMATION;

  _TS_ALL_PROCESSES_INFO = record
    pTsProcessInfo: PTS_SYS_PROCESS_INFORMATION;
    SizeOfSid: DWORD;
    UserSid: PSID;
  end;
  TS_ALL_PROCESSES_INFO =  _TS_ALL_PROCESSES_INFO;
  PTS_ALL_PROCESSES_INFO = ^TS_ALL_PROCESSES_INFO;
  TTSAllProcessesInfo = TS_ALL_PROCESSES_INFO;
  PTSAllProcessesInfo = PTS_ALL_PROCESSES_INFO;

  { helper array to ease enumeration }
  TS_ALL_PROCESSES_INFO_ARRAY = array [0..ANYSIZE_ARRAY-1] of TS_ALL_PROCESSES_INFO;
  PTS_ALL_PROCESSES_INFO_ARRAY = ^TS_ALL_PROCESSES_INFO_ARRAY;
  TTSAllProcessesInfoArray = TS_ALL_PROCESSES_INFO_ARRAY;
  PTSAllProcessesInfoArray = PTS_ALL_PROCESSES_INFO_ARRAY;

  // The following types are used for WinStationGetAllProcesses
  // _WINSTA_PROCESS_INFO
  _WINSTA_PROCESS_INFO = record
    ExtendedInfo: PSYSTEM_PROCESSES;
    dwSidLength: DWORD;
    pUserSid: PSID;
  end;
  PWINSTA_PROCESS_INFO = ^_WINSTA_PROCESS_INFO;
  TWinstaProcessInfo = _WINSTA_PROCESS_INFO;
  PWinstaProcessInfo = PWINSTA_PROCESS_INFO;

  // Array of _WINSTA_PROCESS_INFO
  _WINSTA_PROCESS_INFO_ARRAY = array [0..ANYSIZE_ARRAY-1] of _WINSTA_PROCESS_INFO;
  PWINSTA_PROCESS_INFO_ARRAY= ^_WINSTA_PROCESS_INFO_ARRAY;
  TWinstaProcessInfoArray = _WINSTA_PROCESS_INFO_ARRAY;
  PWinstaProcessInfoArray = PWINSTA_PROCESS_INFO_ARRAY;

  // The following types are used for WinStationQueryInformationW

  // WinStationCreate (0)
  // Both functions (A and W) shares the single definition

  _WINSTATION_CREATE = record
    EnableWinStation : BOOL;
    MaxInstanceCount : DWORD;
   end;
  PWINSTATION_CREATE = ^_WINSTATION_CREATE;
  TWinStationCreate = _WINSTATION_CREATE;
  PWinStationCreate = PWINSTATION_CREATE;

  // WinStationClient (1)
  // returns information as provided by the
  // Terminal Server client (mstsc).
{  _WINSTATION_CLIENTW = record
    Comment: array[0..59] of WCHAR;
    Reserved1: array[0..2] of DWORD;
    ClientUsername: array[0..20] of WCHAR;
    ClientDomain: array[0..17] of WCHAR;
    ClientPassword: array[0..255] of WCHAR; // this was fixec win2000 SP4
    Reserved2: array[0..1635] of BYTE;
    Reserved3: array[0..6] of DWORD;
    Reserved4: array[0..275] of BYTE;
  end;
  PWINSTATION_CLIENTW = ^_WINSTATION_CLIENTW;
  TWinStationClientW = _WINSTATION_CLIENTW;
  PWinStationClientW = PWINSTATION_CLIENTW;}

  TWinStationClientFlags = Set Of (
	  fTextOnly,                                         //: 1
 	  fDisableCtrlAltDel,                                //: 1
 	  fMouse,                                            //: 1
 	  fDoubleClickDetect,                                //: 1
 	  fINetClient,                                       //: 1
 	  fWinStationClientPromptForPassword,                //: 1
 	  fMaximizeShell,                                    //: 1
 	  fEnableWindowsKey,                                 //: 1
 	  fRemoteConsoleAudio,                               //: 1
 	  fWinStationClientPasswordIsScPin,                  //: 1
 	  fNoAudioPlayback,                                  //: 1
 	  fUsingSavedCreds,                                  //: 1
{$IFDEF DELPHI6_UP}
    _TWinStationClientFlagsAlign = al32Bit
{$ELSE}
    _TWinStationClientFlagsAlign1,
    _TWinStationClientFlagsAlign2,
    _TWinStationClientFlagsAlign3,
    _TWinStationClientFlagsAlign4,
    _TWinStationClientFlagsAlign5,
    _TWinStationClientFlagsAlign6,
    _TWinStationClientFlagsAlign7,
    _TWinStationClientFlagsAlign8,
    _TWinStationClientFlagsAlign9,
    _TWinStationClientFlagsAlign10,
    _TWinStationClientFlagsAlign11,
    _TWinStationClientFlagsAlign12,
    _TWinStationClientFlagsAlign13,
    _TWinStationClientFlagsAlign14,
    _TWinStationClientFlagsAlign15,
    _TWinStationClientFlagsAlign16,
    _TWinStationClientFlagsAlign17,
    _TWinStationClientFlagsAlign18,
    _TWinStationClientFlagsAlign19,
    _TWinStationClientFlagsAlign20,
    _TWinStationClientFlagsAlign21,
    _TWinStationClientFlagsAlign22,
    _TWinStationClientFlagsAlign23,
    _TWinStationClientFlagsAlign24,
    _TWinStationClientFlagsAlign25,
    _TWinStationClientFlagsAlign26,
    _TWinStationClientFlagsAlign27,
    _TWinStationClientFlagsAlign28,
    _TWinStationClientFlagsAlign29,
    _TWinStationClientFlagsAlign30,
    _TWinStationClientFlagsAlign31,
    _TWinStationClientFlagsAlign32
{$ENDIF}
  );

  _TS_SYSTEMTIME = record
    wYear: USHORT;
    wMonth: USHORT;
    wDayOfWeek: USHORT;
    wDay: USHORT;
    wHour: USHORT;
    wMinute: USHORT;
    wSecond: USHORT;
    wMilliseconds: USHORT;
  end {_TS_SYSTEMTIME};
  TS_SYSTEMTIME = _TS_SYSTEMTIME;

  _TS_TIME_ZONE_INFORMATION = record
    Bias: LongInt;
    StandardName: Array[0..31] of WCHAR;
    StandardDate: TS_SYSTEMTIME;
    StandardBias: LongInt;
    DaylightName: Array[0..31] of WCHAR;
    DaylightDate: TS_SYSTEMTIME;
    DaylightBias: LongInt;
  end {_TS_TIME_ZONE_INFORMATION};
  TS_TIME_ZONE_INFORMATION = _TS_TIME_ZONE_INFORMATION;


const
  TS_PERF_DISABLE_NOTHING = $0;
  TS_PERF_DISABLE_WALLPAPER = $1;
  TS_PERF_DISABLE_FULLWINDOWDRAG = $2;
  TS_PERF_DISABLE_MENUANIMATIONS = $4;
  TS_PERF_DISABLE_THEMING = $8;
  TS_PERF_ENABLE_ENHANCED_GRAPHICS = $10;
  TS_PERF_DISABLE_CURSOR_SHADOW = $20;
  TS_PERF_DISABLE_CURSORSETTINGS = $40;
  TS_PERF_ENABLE_FONT_SMOOTHING= $80;
  TS_PERF_ENABLE_DESKTOP_COMPOSITION = $100;
  TS_PERF_DEFAULT_NONPERFCLIENT_SETTING = $40000000;
  TS_PERF_RESERVED1 = $80000000;

type
  _WINSTATIONCLIENTW = record
    WinStationClientFlags: TWinStationClientFlags;
    ClientName: Array[0..CLIENTNAME_LENGTH] of WCHAR;
    Domain: Array[0..DOMAIN_LENGTH] of WCHAR;
    UserName: Array[0..USERNAME_LENGTH] of WCHAR;
    Password: Array[0..PASSWORD_LENGTH] of WCHAR;
    WorkDirectory: Array[0..DIRECTORY_LENGTH] of WCHAR;
    InitialProgram: Array[0..INITIALPROGRAM_LENGTH] of WCHAR;
    SerialNumber: ULONG;
    EncryptionLevel: BYTE;
    ClientAddressFamily: ULONG;
    ClientAddress: Array[0..CLIENTADDRESS_LENGTH] of WCHAR;
    HRes: USHORT;
    VRes: USHORT;
    ColorDepth: USHORT;
    ProtocolType: USHORT;
    KeyboardLayout: ULONG;
    KeyboardType: ULONG;
    KeyboardSubType: ULONG;
    KeyboardFunctionKey: ULONG;
    imeFileName: Array[0..IMEFILENAME_LENGTH] of WCHAR;
    ClientDirectory: Array[0..DIRECTORY_LENGTH] of WCHAR;
    ClientLicense: Array[0..CLIENTLICENSE_LENGTH] of WCHAR;
    ClientModem: Array[0..CLIENTMODEM_LENGTH] of WCHAR;
    ClientBuildNumber: ULONG;
    ClientHardwareId: ULONG;
    ClientProductId: USHORT;
    OutBufCountHost: USHORT;
    OutBufCountClient: USHORT;
    OutBufLength: USHORT;
    AudioDriverName: Array[0..8] of WCHAR;
    ClientTimeZone: TS_TIME_ZONE_INFORMATION;
    ClientSessionId: ULONG;
    clientDigProductId: Array[0..CLIENT_PRODUCT_ID_LENGTH-1] of WCHAR;
    PerformanceFlags: ULONG;
    ActiveInputLocale: ULONG;
  end {_WINSTATIONCLIENTW};
  WINSTATIONCLIENTW = _WINSTATIONCLIENTW;
  PWINSTATIONCLIENTW = ^_WINSTATIONCLIENTW;


  // WdConfig class (3)
  // returns information about the WinStationDriver
  _WD_CONFIGW = record
    WdName: array[0..32] of WCHAR;
    WdDLL: array[0..32] of WCHAR;
    WsxDLL: array[0..33] of WCHAR;
    WdFlag: DWORD;
    InputBufferLength: DWORD;
    CfgDLL: array[0..32] of WCHAR;
    WdPrefix: array[0..12] of WCHAR;
  end;
  PWD_CONFIGW = ^_WD_CONFIGW;
  TWdConfigW = _WD_CONFIGW;
  PWdConfigW = PWD_CONFIGW;

  // WinStationConfig (6)
  // class, returns information about the client's
  // configuration such as network, time(zone) settings and such
{  _WINSTATION_CONFIGW = record
    Reserved1: DWORD;
    ClientName: array[0..20] of WCHAR;
    Domain: array[0..17] of WCHAR;
    Username: array[0..35] of WCHAR;
    CurrentDirectory: array[0..256] of WCHAR;
    ApplicationName:array[0..259] of WCHAR;
    Reserved2: DWORD;
    AddressFamily: DWORD;  // AF_INET, AF_IPX, AF_NETBIOS, AF_UNSPEC
    ClientAddress: array[0..27] of WCHAR;
    Reserved3: array[0..7] of BYTE;
    Reserved4: array[0..4] of DWORD;
    Reserved5: array[0..69] of BYTE;
    ClientDLLName: array[0..330] of WCHAR;
    Reserved6: array[0..1] of FILETIME;
    AudioDriver: array[0..9] of WCHAR;
    TZBias: DWORD;
    TZStandardName: array[0..31] of WCHAR;
    Reserved7: DWORD; // Standard Bias??
    TZDaylightName: array[0..31] of WCHAR;
    TZDayLightStart: array[0..15] of BYTE;
    TZDayLightBias: DWORD;
    Reserved8: DWORD; // Daylight offset?
    TSInstanceID: array[0..33] of WCHAR; // sometimes windows license key(s)
    Reserved9: DWORD;      // related to license key or instanceid?
  end;
  PWINSTATION_CONFIGW = ^_WINSTATION_CONFIGW;
  TWinStationConfigW = _WINSTATION_CONFIGW;
  PWinStationConfigW = PWINSTATION_CONFIGW;}

  // class WinStationInformation (8)
  // provides information about the current state of the client such as
  // idletime, sessionstatus and transferred/received bytes
{ The structure below was reverse engineered but is now documented. I replaced
  it with the offical structure _WINSTATIONINFORMATIONW (in this unit)
 _WINSTATION_INFORMATIONW = record
    State: DWORD;
    WinStationName: array[0..10] of WideChar;
    Unknown1: array[0..10] of byte;
    Unknown3: array[0..10] of WideChar;
    Unknown2: array[0..8] of byte;
    SessionId: DWORD;
    Reserved2: array[0..3] of byte;
    ConnectTime: FILETIME;
    DisconnectTime: FILETIME;
    LastInputTime: FILETIME;
    LogonTime: FILETIME;
    Unknown4: array[0..11] of byte;
    OutgoingFrames: DWORD;
    OutgoingBytes: DWORD;
    OutgoingCompressBytes: DWORD;
    Unknown5: array[0..435] of byte;
    IncomingCompressedBytes: DWORD;
    Unknown6: array[0..7] of byte;
    IncomingFrames: DWORD;
    IncomingBytes: DWORD;
    Unknown7: array[0..3] of byte;
    Reserved3: array[0..528] of byte;
    Domain: array[0..17] of WideChar;
    Username: array[0..22] of WideChar;
    CurrentTime: FILETIME;
  end;
  PWINSTATION_INFORMATIONW = ^_WINSTATION_INFORMATIONW;
  TWinStationInformationExW = _WINSTATION_INFORMATIONW;
  PWinStationInformationExW = PWINSTATION_INFORMATIONW;}

{ The WINSTATIONSTATECLASS enumeration represents the current state of a session
  RW: corresponds to WTS_CONNECTSTATE_CLASS from WtsApi32
  State_Active: A user is logged on to a session and the client is connected.
  State_Connected: A client is connected to a session but the user has not yet logged on.
  State_ConnectQuery: A session is in the process of connecting to a client.
  State_Shadow: A session is shadowing another session.
  State_Disconnected: A user is logged on to the session but the client is currently disconnected from the server.
  State_Idle: A session is waiting for a client to connect to the server.
  State_Listen: A listener is waiting for connections from the Terminal Services client.
  State_Reset: A session is being reset. As a result, the user is logged off, the session is terminated, and the client is disconnected.<34>
  State_Down: A session is currently tearing down or is in the down state, indicating an error.
  State_Init: A session is in the process of being initialized.
}
  _WINSTATIONSTATECLASS = (
    State_Active {= 0},
    State_Connected {= 1},
    State_ConnectQuery {= 2},
    State_Shadow {= 3},
    State_Disconnected {= 4},
    State_Idle {= 5},
    State_Listen {= 6},
    State_Reset {= 7},
    State_Down {= 8},
    State_Init {= 9 } );
  WINSTATIONSTATECLASS = _WINSTATIONSTATECLASS;
  TWinStationStateClass = _WINSTATIONSTATECLASS;

  WINSTATIONNAMEW = Array[0..WINSTATIONNAME_LENGTH] of WCHAR;
  WINSTATIONNAMEA = Array[0..WINSTATIONNAME_LENGTH] of AnsiChar;

  _TSHARE_COUNTERS = packed record
    Reserved: ULONG;
  end {_TSHARE_COUNTERS};
  TSHARE_COUNTERS = _TSHARE_COUNTERS;
  PTSHARE_COUNTERS = ^_TSHARE_COUNTERS;

  _PROTOCOLCOUNTERS = packed record
    WdBytes: ULONG;
    WdFrames: ULONG;
    WaitForOutBuf: ULONG;
    Frames: ULONG;
    Bytes: ULONG;
    CompressedBytes: ULONG;
    CompressFlushes: ULONG;
    Errors: ULONG;
    Timeouts: ULONG;
    AsyncFramingError: ULONG;
    AsyncOverrunError: ULONG;
    AsyncOverflowError: ULONG;
    AsyncParityError: ULONG;
    TdErrors: ULONG;
    ProtocolType: USHORT;
    case Length: USHORT of
      1: (TShareCounters: TSHARE_COUNTERS);
      2: (Reserved: Array[0..99] of ULONG);
  end {_PROTOCOLCOUNTERS};
  PROTOCOLCOUNTERS = _PROTOCOLCOUNTERS;
  PPROTOCOLCOUNTERS = ^_PROTOCOLCOUNTERS;
  TProtocolCounters = _PROTOCOLCOUNTERS;

  _THINWIRECACHE = packed record
    CacheReads: ULONG;
    CacheHits: ULONG;
  end {_THINWIRECACHE};
  THINWIRECACHE = _THINWIRECACHE;
  PTHINWIRECACHE = ^_THINWIRECACHE;

const
  MAX_THINWIRECACHE = 4;

type

 _RESERVED_CACHE = packed record
    ThinWireCache: Array[0..MAX_THINWIRECACHE-1] of THINWIRECACHE;
  end {_RESERVED_CACHE};
  RESERVED_CACHE = _RESERVED_CACHE;
  PRESERVED_CACHE = ^_RESERVED_CACHE;

  _TSHARE_CACHE = packed record
    Reserved: ULONG;
  end {_TSHARE_CACHE};
  TSHARE_CACHE = _TSHARE_CACHE;
  PTSHARE_CACHE = ^_TSHARE_CACHE;
  
  CACHE_STATISTICS = packed record
    ProtocolType: USHORT;
    case Length: USHORT of
      1: (ReservedCacheStats: RESERVED_CACHE);
      2: (TShareCacheStats: TSHARE_CACHE);
      3: (Reserved: Array[0..19] of ULONG);
  end {CACHE_STATISTICS};
  
  _PROTOCOLSTATUS = packed record
    Output: PROTOCOLCOUNTERS;
    Input: PROTOCOLCOUNTERS;
    Cache: CACHE_STATISTICS;
    AsyncSignal: ULONG;
    AsyncSignalMask: ULONG;
  end {_PROTOCOLSTATUS};
  PROTOCOLSTATUS = _PROTOCOLSTATUS;
  PPROTOCOLSTATUS = ^_PROTOCOLSTATUS;

  _WINSTATIONINFORMATIONW = record
    ConnectState: WINSTATIONSTATECLASS;
    WinStationName: WINSTATIONNAMEW;
    LogonId: ULONG;
    Align: array[0..3] of byte;
    ConnectTime: LARGE_INTEGER;
    DisconnectTime: LARGE_INTEGER;
    LastInputTime: LARGE_INTEGER;
    LogonTime: LARGE_INTEGER;
    Status: PROTOCOLSTATUS;
    Domain: Array[0..DOMAIN_LENGTH] of WCHAR;
    UserName: Array[0..USERNAME_LENGTH] of WCHAR;
    CurrentTime: LARGE_INTEGER;
  end {_WINSTATIONINFORMATIONW};
  WINSTATIONINFORMATIONW = _WINSTATIONINFORMATIONW;
  PWINSTATIONINFORMATIONW = ^_WINSTATIONINFORMATIONW;

  // Class WinStationResolution (16)
  _WINSTATION_RESOLUTION =  record
     HorizontalResolution : WORD; // width
     VerticalResolution : WORD;  // height
     ColorDepth : WORD; // bits per pixel, see JwaWtsApi._WTS_CLIENT_DISPLAY.ColorDepth for format
  end;
  PWINSTATION_RESOLUTION = ^_WINSTATION_RESOLUTION;
  TWinStationResolution = _WINSTATION_RESOLUTION;
  PWinStationResolution = PWINSTATION_RESOLUTION;


  // WinStationRemoteAddress (class 29)
  // Definition is preliminary
  // AddressFamily can be AF_INET, AF_IPX, AF_NETBIOS, AF_UNSPEC
  // Port is the remote port number (local port number is 3389 by default)
  // Address (for type AF_INET it start's at a 2 byte offset)
  // You can format IP Address to string like this:
  // Format('%d.%d.%d.%d', [WinStationAddress.Address[2],
  //  WinStationRemoteAddress.[3], WinStationRemoteAddress.Address[4],
  //  WinStationRemoteAddress.Address[5]]);
  //
  // Be sure to fill the structure with zeroes before query!
  _WINSTATION_REMOTE_ADDRESS = record
    AddressFamily: DWORD;
    Port: WORD;
    Address: array [0..19] of BYTE;
    Reserved: array[0..5] of BYTE;
  end;
  PWINSTATION_REMOTE_ADDRESS = ^_WINSTATION_REMOTE_ADDRESS;
  TWinStationRemoteAddress = _WINSTATION_REMOTE_ADDRESS;
  PWinStationRemoteAddress = PWINSTATION_REMOTE_ADDRESS;

  // WinStationShadowInformation (26)
  // Setting it requires the caller to be a local system, only ShadowMode field is used
  _WINSTATION_SHADOW_INFORMATION = record
    CurrentShadowState : DWORD;  //one of the SHADOW_STATE_XXX constants
    ShadowMode : DWORD;          // one of the SHADOW_MODE_XXX constants
    CurrentSessionId : DWORD;
    Unknown1 : DWORD; // unknown; contains 2 or normal sessions, 0 on console and idle sessions
  end;
  PWINSTATION_SHADOW_INFORMATION = ^_WINSTATION_SHADOW_INFORMATION;
  TWinStationShadowInformation = _WINSTATION_SHADOW_INFORMATION;
  PWinStationShadowInformation = PWINSTATION_SHADOW_INFORMATION;

  // WinStationPipeInformation (33)
  // returns name of the pipe (e.g. \\.\pipe\TerminalServer\g0djjEInbXQFxJ9JLPl\2)
  // which is used to create processes in the target session
  // by default, system can write and read to this pipe
  _WINSTATION_PIPE_INFORMATIONW = record
    PipeName : array [0..47] of WideChar;
  end;
  PWINSTATION_PIPE_INFORMATIONW = ^_WINSTATION_PIPE_INFORMATIONW;
  TWinStationPipeInformationW = _WINSTATION_PIPE_INFORMATIONW;
  PWinStationPipeInformationW = PWINSTATION_PIPE_INFORMATIONW;

 { The header of the Terminal Services performance counter structure providing
   general information on the counter.
   dwCounterID: The identifier of the counter. Set by the caller of
   (Rpc)WinStationGetTermSrvCountersValue to indicate the counter on which to
   retrieve data. This will be set to zero by
   (Rpc)WinStationGetTermSrvCountersValue if the dwCounterId isn't recognized.
   bResult: Set to TRUE if counter information is returned. Set to FALSE if
   counter data isn't being returned because the counter ID being requested
   was unrecognized.

   The following counters are supported:
   Total number of sessions: dwCounterId SHOULD be TERMSRV_TOTAL_SESSIONS.
   Value will indicate the total number of reconnections to the server since
   startup.
   Number of disconnected sessions: dwCounterId SHOULD be TERMSRV_DISC_SESSIONS.
   Value will indicate the total number of disconnections from the server since
   startup.
   Number of reconnected sessions: dwCounterId SHOULD be TERMSRV_RECON_SESSIONS.
   Value will indicate the total number of all reconnected sessions that have
   existed on the server since startup.
   Current number of active sessions: dwCounterId SHOULD be
   TERMSRV_CURRENT_ACTIVE_SESSIONS.
   Value will indicate the current number of active sessions on the server.
   Current number of disconnected sessions: dwCounterId SHOULD be
   TERMSRV_CURRENT_DISC_SESSIONS. Value will indicate the current number of
   disconnected sessions on the server. Windows XP only
   }
  const TERMSRV_TOTAL_SESSIONS = 1;
  const TERMSRV_DISC_SESSIONS = 2;
  const TERMSRV_RECON_SESSIONS = 3;
  const TERMSRV_CURRENT_ACTIVE_SESSIONS = 4;
  const TERMSRV_CURRENT_DISC_SESSIONS = 5;
  const TERMSRV_PENDING_SESSIONS = 6;
  const TERMSRV_SUCC_TOTAL_LOGONS = 7;
  const TERMSRV_SUCC_LOCAL_LOGONS = 8;
  const TERMSRV_SUCC_REMOTE_LOGONS = 9;
  const TERMSRV_SUCC_SESSION0_LOGONS = 10;
  const TERMSRV_CURRENT_TERMINATING_SESSIONS = 11;
  const TERMSRV_CURRENT_LOGGEDON_SESSIONS = 12;

type
  _TS_COUNTER_HEADER = record
    dwCounterID: DWORD;
    bResult: Boolean;
  end;
  TS_COUNTER_HEADER = _TS_COUNTER_HEADER;
  TTSCounterHeader = TS_COUNTER_HEADER;
  PTS_COUNTER_HEADER = ^TS_COUNTER_HEADER;
  PTSCounterHeader = PTS_COUNTER_HEADER;

  _TS_COUNTER = record
    CounterHead: TS_COUNTER_HEADER;
    dwValue: DWORD;
    StartTime: LARGE_INTEGER; //Currently, always set to zero because time stamps are not supported.
  end;
  TS_COUNTER = _TS_COUNTER;
  TTSCounter = TS_COUNTER;
  PTS_COUNTER = ^TS_COUNTER;
  PTSCounter = PTS_COUNTER;

function AreWeRunningTerminalServices: Boolean;

procedure CachedGetUserFromSid(pSid: PSID; pUserName: LPWSTR;
  var cbUserName: DWORD); stdcall;

function CalculateDiffTime(TimeLow: INT64; TimeHigh: INT64): INT64;
  stdcall;

// Calculate Elapsed time from a Filetime (UTC time) to DiffTime structure
function CalculateElapsedTime(lpFileTime: PFILETIME; var DiffTime: TDiffTime):
  Boolean; stdcall;

function CpuTime2Str(ACPUTime: LARGE_INTEGER): String;

function CurrentDateTimeString(out lpBuffer: PWideChar): Boolean; stdcall;

// This is the version for NT Terminal Server, 2000, XP/2003 and Server 2008
function DateTimeString(DateTime: PFILETIME; lpBuffer: PWideChar): PWideChar;
  stdcall;

// This is a wrapper for all OS versions, you are strongly recommended to use
// only this version.
function DateTimeStringSafe(DateTime: PFILETIME; lpBuffer: PWideChar;
  cchDest: SIZE_T): PWideChar; stdcall;

// This is the Vista RTM version which takes an additional parameter with
// maximum buffer size (you have to set it). This bug was fixed in SP1.
function DateTimeStringVistaRTM(DateTime: PFILETIME; lpBuffer: PWideChar;
  cchDest: SIZE_T): PWideChar; stdcall;

function DiffTimeString(FTLow: FILETIME; FTHigh: FILETIME;
  out pwElapsedTime: PWideChar): Integer;

// This is the version for NT Terminal Server, 2000, XP/2003 and Server 2008
function ElapsedTimeString(DiffTime: PDiffTime; bShowSeconds: Boolean;
  lpElapsedTime: PWideChar): Integer; stdcall;

// This is a wrapper for all OS versions, you are strongly recommended to use
// only this version.
function ElapsedTimeStringSafe(DiffTime: PDiffTime; bShowSeconds: Boolean;
  lpElapsedTime: PWideChar; cchDest: SIZE_T): Integer;

// This is the Vista RTM version of ElapsedTimeString which takes an additional
// parameter with the count of characters for lpElapsedTime (you have to set it)
// This bug was fixed in SP1.
function ElapsedTimeStringVistaRTM(DiffTime: PDiffTime; bShowSeconds: Boolean;
  lpElapsedTime: PWideChar; cchDest: SIZE_T): HRESULT; stdcall;



//returns -109205 on error
function FileTime2DateTime(FileTime: TFileTime): TDateTime;

function GetUnknownString: PWideChar; stdcall;

function IsTerminalServiceRunning: boolean;

// Tested and working on Windows XP but doesn't seem to work on
// Windows Vista/2008. Better use W version to be sure!
function LogonIdFromWinStationNameA(hServer: HANDLE; pWinStationName: LPSTR;
  var SessionId: DWORD): Boolean; stdcall;

// Tested and working on XP, 2003 and 2008
function LogonIdFromWinStationNameW(hServer: HANDLE; pWinStationName: LPWSTR;
  var SessionId: DWORD): Boolean; stdcall;

// This is the version for NT Terminal Server, 2000, XP/2003 and Server 2008
// Reserve 66 bytes for pWinStationName and 21 for pUserName
function QueryCurrentWinStation(pWinStationName: LPWSTR;
  pUserName: LPWSTR; var SessionId: DWORD; var WdFlag: DWORD): Boolean;
  stdcall;

// This is the Vista RTM version of QueryCurrentWinStation which takes an
// additional parameter with the count of characters for pUserName
// note that pWinStationname is Fixed Size!
// This bug was fixed in SP1.
function QueryCurrentWinStationVistaRTM(pWinStationName: LPWSTR;
  pUserName: PWideChar; cchDest: DWORD; var SessionId: DWORD;
  var WdFlag: DWORD): Boolean; stdcall;

// This is a wrapper for all OS versions, you are strongly recommended to use
// only this version.
function QueryCurrentWinStationSafe(pWinStationName: LPWSTR;
  pUserName: PWideChar; cchDest: DWORD; var SessionId: DWORD;
  var WdFlag: DWORD): Boolean; stdcall;

function StrConnectState(ConnectState: WTS_CONNECTSTATE_CLASS;
  bShortString: BOOL): PWideChar; stdcall;

function WinStationBroadcastSystemMessage(hServer: HANDLE;
  SendToAllWinstations: BOOL; SessionId: DWORD; TimeOut: DWORD;
  dwFlags: DWORD; lpdwRecipients: DWORD; uiMessage: ULONG; _wParam: WPARAM;
  _lParam: LPARAM; pResponse: LONGINT): LONGINT; stdcall;

function WinStationCallBack(hServer:HANDLE; SessionId: DWORD;
	pPhoneNumber: LPWSTR): BOOL; stdcall;

procedure WinStationCloseServer(hServer: HANDLE); stdcall;

function WinStationConnectW(hServer: Handle; SessionId: DWORD;
  TargetSessionId: DWORD; pPassword: LPWSTR;
  bWait: BOOL): Boolean; stdcall;

function WinStationDisconnect(hServer: THandle; SessionId: DWORD;
  bWait: BOOL): Boolean; stdcall;

function WinStationEnumerateA(hServer: HANDLE;
  var ppSessionInfo: PWTS_SESSION_INFOA; var pCount: DWORD): Boolean; stdcall;

function WinStationEnumerateW(hServer: HANDLE;
  var ppSessionInfo: PWTS_SESSION_INFOW; var pCount: DWORD): Boolean; stdcall;

// Used to release memory allocated by WinStationGetAllProcesses
function WinStationFreeGAPMemory(Level: DWORD;
  ppTsAllProcessesInfo: PTS_ALL_PROCESSES_INFO_ARRAY;
  pNumberOfProcesses: Integer): Boolean; stdcall;

// Important! pProcessInfo must be nil before calling this function
// by using Out parameter Delphi takes care of this for us
function WinStationGetAllProcesses(hServer: HANDLE; Level: DWORD;
  var pNumberOfProcesses: Integer;
  out ppTsAllProcessesInfo: PTS_ALL_PROCESSES_INFO_ARRAY): Boolean stdcall;

function WinStationGetLanAdapterNameW(hServer: HANDLE; LanaId: DWORD;
  ProtocolTypeLength: DWORD; ProtocolType: PWideChar;
  var ResultLength: DWORD; var LanAdapterName: PWideChar): DWORD; stdcall;

function WinStationGetProcessSid(hServer: Handle; dwPID: DWORD;
  ProcessStartTime: FILETIME; pProcessUserSid: PSID; var dwSidSize: DWORD):
  Boolean; stdcall;

function WinStationGetRemoteIPAddress(hServer: HANDLE; SessionId: DWORD;
  var RemoteIPAddress: WideString; var Port: WORD): Boolean;

function WinStationGetTermSrvCountersValue(hServer: Handle;
  dwEntries: DWORD; pCounter: PTS_COUNTER): Boolean; stdcall;

function WinStationNameFromLogonIdA(hServer: HANDLE; SessionId: ULONG;
  pWinStationName: LPSTR): Boolean; stdcall;

function WinStationNameFromLogonIdW(hServer: HANDLE; SessionId: ULONG;
  pWinStationName: LPWSTR): Boolean; stdcall;

function WinStationOpenServerA(pServerName: LPSTR): HANDLE; stdcall;

function WinStationOpenServerW(pServerName: LPWSTR): HANDLE; stdcall;

function WinStationQueryInformationW(hServer: HANDLE; SessionId: DWORD;
  WinStationInformationClass: WINSTATIONINFOCLASS; pWinStationInformation: PVOID;
  WinStationInformationLength: DWORD; var pReturnLength: DWORD):
  Boolean; stdcall;

function WinStationQueryLogonCredentialsW(
  var LogonCredentials: _LOGON_CREDENTIALSW): HRESULT; stdcall;

function WinstationQueryUserToken(hServer: HANDLE; SessionId: DWORD;
  var hToken: HANDLE): Boolean;

function WinStationRegisterConsoleNotification(hServer: HANDLE; _hwnd: HWND;
  dwFlags: Cardinal): Boolean; stdcall;

// WinStationRename needs Admin rights and always returns true
// need to check GetLastError
// Duplicate names are not allowed
// Renaming a WinStation gives errors on Remote Connections:
// the windowstation is busy processing connect, disconnect, reset
// or login request

// A version untested
function WinStationRenameA(hServer: HANDLE; pOldWinStationName: LPSTR;
  pNewWinStationName: LPSTR): Boolean; stdcall;

// W version was tested
function WinStationRenameW(hServer: HANDLE; pOldWinStationName: LPWSTR;
  pNewWinStationName: LPWSTR): Boolean; stdcall;

function WinStationSendMessageA(hServer: HANDLE; SessionId: DWORD;
  pTitle: LPSTR; TitleLength: DWORD; pMessage: LPSTR; MessageLength: DWORD;
  Style: DWORD; Timeout: DWORD; var pResponse: DWORD;
  bWait: BOOL): Boolean; stdcall;

function WinStationSendMessageW(hServer: HANDLE; SessionId: DWORD;
  pTitle: LPWSTR; TitleLength: DWORD; pMessage: LPWSTR; MessageLength: DWORD;
  Style: DWORD; Timeout: DWORD; var pResponse: DWORD;
  bWait: BOOL): Boolean; stdcall;

function WinStationServerPing(hServer: HANDLE): BOOLEAN; stdcall;

function WinStationSetInformationA(hServer: HANDLE; SessionID: DWORD;
  InformationClass: WINSTATIONINFOCLASS; pWinStationInformation: PVOID;
  WinStationInformationLength: DWORD): Boolean; stdcall;

function WinStationSetInformationW(hServer: HANDLE; SessionID: DWORD;
  InformationClass: WINSTATIONINFOCLASS; pWinStationInformation: PVOID;
  DataSWinStationInformationLength: DWORD): Boolean; stdcall;

function WinStationShadow(hServer: Handle; pServerName: LPWSTR;
  SessionId: DWORD; HotKey: DWORD; HKModifier: DWORD): Boolean; stdcall;

// Admin can stop a shadowed session. SessionId is the targetsession
// so the "victim" and not the one who is shadowing
function WinStationShadowStop(hServer: Handle; SessionId: DWORD;
  bWait: BOOL): Boolean; stdcall;

function WinStationShutDownSystem(hSERVER: HANDLE;
  ShutdownFlags: DWORD): Boolean; stdcall;

function WinStationTerminateProcess(hServer: Handle; dwPID: DWORD;
  dwExitCode: DWORD): Boolean; stdcall;

function WinStationUnRegisterConsoleNotification(hServer: HANDLE;
  hwnd: THANDLE): Boolean; stdcall;

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation

uses
  JwaWinDLLNames;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INCLUDEMODE}
const
  winstadll = 'winsta.dll';
  utildll = 'utildll.dll';
{$ENDIF JWA_INCLUDEMODE}

{$IFNDEF JWA_INTERFACESECTION}


{$IFNDEF DYNAMIC_LINK}
procedure CachedGetUserFromSid; external utildll name 'CachedGetUserFromSid';
function CalculateDiffTime; external utildll name 'CalculateDiffTime';
function CalculateElapsedTime; external utildll name 'CalculateElapsedTime';
function CurrentDateTimeString; external utildll name 'CurrentDateTimeString';
function DateTimeString; external utildll name 'DateTimeString';
function DateTimeStringVistaRTM; external utildll name 'DateTimeString';
function ElapsedTimeString; external utildll name 'ElapsedTimeString';
// Vista version of ElapsedTimeString, exported name is ElapsedTimeString

function ElapsedTimeStringVistaRTM; external utildll name 'ElapsedTimeString';
function GetUnknownString; external utildll name 'GetUnknownString';
function LogonIdFromWinStationNameA; external winstadll name 'LogonIdFromWinStationNameA';
function LogonIdFromWinStationNameW; external winstadll name 'LogonIdFromWinStationNameW';
function QueryCurrentWinStation; external utildll name 'QueryCurrentWinStation';
function QueryCurrentWinStationVistaRTM; external utildll name 'QueryCurrentWinStation';
function StrConnectState; external utildll name 'StrConnectState';
function WinStationBroadcastSystemMessage; external winstadll name 'WinStationBroadcastSystemMessage';
function WinStationCallBack; external winstadll name 'WinStationCallBack';
procedure WinStationCloseServer; external winstadll name 'WinStationCloseServer';
function WinStationConnectW; external winstadll name 'WinStationConnectW';
function WinStationDisconnect; external winstadll name 'WinStationDisconnect';
function WinStationEnumerateA; external winstadll name 'WinStationEnumerateA';
function WinStationEnumerateW; external winstadll name 'WinStationEnumerateW';
function WinStationFreeGAPMemory; external winstadll name 'WinStationFreeGAPMemory';
function WinStationGetAllProcesses; external winstadll name 'WinStationGetAllProcesses';
function WinStationGetLanAdapterNameW; external winstadll name 'WinStationGetLanAdapterNameW';
function WinStationGetProcessSid; external winstadll name 'WinStationGetProcessSid';
function WinStationGetTermSrvCountersValue; external winstadll name 'WinStationGetTermSrvCountersValue';
function WinStationNameFromLogonIdA; external winstadll name 'WinStationNameFromLogonIdA';
function WinStationNameFromLogonIdW; external winstadll name 'WinStationNameFromLogonIdW';
function WinStationOpenServerA; external winstadll name 'WinStationOpenServerA';
function WinStationOpenServerW; external winstadll name 'WinStationOpenServerW';
function WinStationQueryLogonCredentialsW; external winstadll name 'WinStationQueryLogonCredentialsW';
function WinStationRegisterConsoleNotification; external winstadll name 'WinStationRegisterConsoleNotification';
function WinStationRenameA; external winstadll name 'WinStationRenameA';
function WinStationRenameW; external winstadll name 'WinStationRenameW';
function WinStationSendMessageA; external winstadll name 'WinStationSendMessageA';
function WinStationSendMessageW; external winstadll name 'WinStationSendMessageW';
function WinStationServerPing; external winstadll name 'WinStationServerPing';
function WinStationSetInformationA; external winstadll name 'WinStationSetInformationA';
function WinStationSetInformationW; external winstadll name 'WinStationSetInformationW';
function WinStationShadow; external winstadll name 'WinStationShadow';
function WinStationShadowStop; external winstadll name 'WinStationShadowStop';
function WinStationShutDownSystem; external winstadll name 'WinStationShutDownSystem';
function WinStationQueryInformationW; external winstadll name 'WinStationQueryInformationW';
function WinStationTerminateProcess; external winstadll name 'WinStationTerminateProcess';
function WinStationUnRegisterConsoleNotification; external winstadll name 'WinStationUnRegisterConsoleNotification';
{$ELSE}

var
  __CachedGetUserFromSid: Pointer;

procedure CachedGetUserFromSid;
begin
  GetProcedureAddress(__CachedGetUserFromSid, utildll, 'CachedGetUserFromSid');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__CachedGetUserFromSid]
  end;
end;

var
  __CalculateDiffTime: Pointer;

function CalculateDiffTime;
begin
  GetProcedureAddress(__CalculateDiffTime, utildll, 'CalculateDiffTime');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__CalculateDiffTime]
  end;
end;

var
  __CalculateElapsedTime: Pointer;

function CalculateElapsedTime;
begin
  GetProcedureAddress(__CalculateElapsedTime, utildll, 'CalculateElapsedTime');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__CalculateElapsedTime]
  end;
end;

var
  __CurrentDateTimeString: Pointer;

function CurrentDateTimeString;
begin
  GetProcedureAddress(__CurrentDateTimeString, utildll, 'CurrentDateTimeString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__CurrentDateTimeString]
  end;
end;

var
  __DateTimeString: Pointer;

function DateTimeString;
begin
  GetProcedureAddress(__DateTimeString, utildll, 'DateTimeString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__DateTimeString]
  end;
end;

var
  __DateTimeStringVistaRTM: Pointer;

function DateTimeStringVistaRTM;
begin
  GetProcedureAddress(__DateTimeStringVistaRTM, utildll, 'DateTimeString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__DateTimeStringVistaRTM]
  end;
end;

var
  __ElapsedTimeString: Pointer;

function ElapsedTimeString;
begin
  GetProcedureAddress(__ElapsedTimeString, utildll, 'ElapsedTimeString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__ElapsedTimeString]
  end;
end;

var
  __ElapsedTimeStringVistaRTM: Pointer;

function ElapsedTimeStringVistaRTM;
begin
  GetProcedureAddress(__ElapsedTimeStringVistaRTM, utildll, 'ElapsedTimeString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__ElapsedTimeStringVistaRTM]
  end;
end;


var
  __GetUnknownString: Pointer;

function GetUnknownString;
begin
  GetProcedureAddress(__GetUnknownString, utildll, 'GetUnknownString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__GetUnknownString]
  end;
end;

var
  __LogonIdFromWinStationNameA: Pointer;

function LogonIdFromWinStationNameA;
begin
  GetProcedureAddress(__LogonIdFromWinStationNameA, winstadll, 'LogonIdFromWinStationNameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__LogonIdFromWinStationNameA]
  end;
end;

var
  __LogonIdFromWinStationNameW: Pointer;

function LogonIdFromWinStationNameW;
begin
  GetProcedureAddress(__LogonIdFromWinStationNameW, winstadll, 'LogonIdFromWinStationNameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__LogonIdFromWinStationNameW]
  end;
end;

var
  __QueryCurrentWinStation: Pointer;

function QueryCurrentWinStation;
begin
  GetProcedureAddress(__QueryCurrentWinStation, utildll, 'QueryCurrentWinStation');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__QueryCurrentWinStation]
  end;
end;

var
  __QueryCurrentWinStationVistaRTM: Pointer;

function QueryCurrentWinStationVistaRTM;
begin
  GetProcedureAddress(__QueryCurrentWinStationVistaRTM, utildll, 'QueryCurrentWinStation');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__QueryCurrentWinStationVistaRTM]
  end;
end;

var
  __StrConnectState: Pointer;

function StrConnectState;
begin
  GetProcedureAddress(__StrConnectState, utildll, 'StrConnectState');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__StrConnectState]
  end;
end;

var
  //__WinStationBroadcastSystemMessage: Pointer;
  __WinStationBroadcastSM: Pointer;

function WinStationBroadcastSystemMessage;
begin
  GetProcedureAddress(__WinStationBroadcastSM, winstadll, 'WinStationBroadcastSystemMessage');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationBroadcastSM]
  end;
end;

var
  __WinStationCallBack: Pointer;

function WinStationCallBack;
begin
  GetProcedureAddress(__WinStationCallBack, winstadll, 'WinStationCallBack');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationCallBack]
  end;
end;

var
  __WinStationCloseServer: Pointer;

procedure WinStationCloseServer;
begin
  GetProcedureAddress(__WinStationCloseServer, winstadll, 'WinStationCloseServer');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationCloseServer]
  end;
end;

var
  __WinStationConnectW: Pointer;

function WinStationConnectW;
begin
  GetProcedureAddress(__WinStationConnectW, winstadll, 'WinStationConnectW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationConnectW]
  end;
end;


var
  __WinStationDisconnect: Pointer;

function WinStationDisconnect;
begin
  GetProcedureAddress(__WinStationDisconnect, winstadll, 'WinStationDisconnect');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationDisconnect]
  end;
end;

var
  __WinStationEnumerateA: Pointer;

function WinStationEnumerateA;
begin
  GetProcedureAddress(__WinStationEnumerateA, winstadll, 'WinStationEnumerateA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationEnumerateA]
  end;
end;

var
  __WinStationEnumerateW: Pointer;

function WinStationEnumerateW;
begin
  GetProcedureAddress(__WinStationEnumerateW, winstadll, 'WinStationEnumerateW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationEnumerateW]
  end;
end;

var
  __WinStationFreeGAPMemory: Pointer;

function WinStationFreeGAPMemory;
begin
  GetProcedureAddress(__WinStationFreeGAPMemory, winstadll, 'WinStationFreeGAPMemory');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationFreeGAPMemory]
  end;
end;

var
  __WinStationGetAllProcesses: Pointer;

function WinStationGetAllProcesses;
begin
  GetProcedureAddress(__WinStationGetAllProcesses, winstadll, 'WinStationGetAllProcesses');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationGetAllProcesses]
  end;
end;

var
  __WinStationGetLanAdapterNameW: Pointer;

function WinStationGetLanAdapterNameW;
begin
  GetProcedureAddress(__WinStationGetLanAdapterNameW, winstadll, 'WinStationGetLanAdapterNameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationGetLanAdapterNameW]
  end;
end;

var
  __WinStationGetProcessSid: Pointer;

function WinStationGetProcessSid;
begin
  GetProcedureAddress(__WinStationGetProcessSid, winstadll, 'WinStationGetProcessSid');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationGetProcessSid]
  end;
end;

var
  __WinStationGetTermSrvCountersV: Pointer;

function WinStationGetTermSrvCountersValue;
begin
  GetProcedureAddress(__WinStationGetTermSrvCountersV, winstadll, 'WinStationGetTermSrvCountersValue');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationGetTermSrvCountersV]
  end;
end;

var
  __WinStationNameFromLogonIdA: Pointer;

function WinStationNameFromLogonIdA;
begin
  GetProcedureAddress(__WinStationNameFromLogonIdA, winstadll, 'WinStationNameFromLogonIdA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationNameFromLogonIdA]
  end;
end;


var
  __WinStationNameFromLogonIdW: Pointer;

function WinStationNameFromLogonIdW;
begin
  GetProcedureAddress(__WinStationNameFromLogonIdW, winstadll, 'WinStationNameFromLogonIdW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationNameFromLogonIdW]
  end;
end;

var
  __WinStationOpenServerA: Pointer;

function WinStationOpenServerA;
begin
  GetProcedureAddress(__WinStationOpenServerA, winstadll, 'WinStationOpenServerA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationOpenServerA]
  end;
end;

var
  __WinStationOpenServerW: Pointer;

function WinStationOpenServerW;
begin
  GetProcedureAddress(__WinStationOpenServerW, winstadll, 'WinStationOpenServerW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationOpenServerW]
  end;
end;

var
  __WinStationQueryLogonCW: Pointer;

function WinStationQueryLogonCredentialsW;
begin
  GetProcedureAddress(__WinStationQueryLogonCW, winstadll, 'WinStationQueryLogonCredentialsW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationQueryLogonCW]
  end;
end;

var
  __WinStationRenameA: Pointer;

function WinStationRenameA;
begin
  GetProcedureAddress(__WinStationRenameA, winstadll, 'WinStationRenameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationRenameA]
  end;
end;

var
  __WinStationRenameW: Pointer;

function WinStationRenameW;
begin
  GetProcedureAddress(__WinStationRenameW, winstadll, 'WinStationRenameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationRenameW]
  end;
end;

var
  __WinStationQueryInformationW: Pointer;

function WinStationQueryInformationW;
begin
  GetProcedureAddress(__WinStationQueryInformationW, winstadll, 'WinStationQueryInformationW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationQueryInformationW]
  end;
end;

var
  __WinStationRegisterCN: Pointer;

function WinStationRegisterConsoleNotification;
begin
  GetProcedureAddress(__WinStationRegisterCN, winstadll, 'WinStationRegisterConsoleNotification');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationRegisterCN]
  end;
end;

var
  __WinStationSendMessageA: Pointer;

function WinStationSendMessageA;
begin
  GetProcedureAddress(__WinStationSendMessageA, winstadll, 'WinStationSendMessageA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationSendMessageA]
  end;
end;

var
  __WinStationSendMessageW: Pointer;

function WinStationSendMessageW;
begin
  GetProcedureAddress(__WinStationSendMessageW, winstadll, 'WinStationSendMessageW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationSendMessageW]
  end;
end;

var
  __WinStationServerPing: Pointer;

function WinStationServerPing;
begin
  GetProcedureAddress(__WinStationServerPing, winstadll, 'WinStationServerPing');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationServerPing]
  end;
end;

var
  __WinStationSetInformationA: Pointer;

function WinStationSetInformationA;
begin
  GetProcedureAddress(__WinStationSetInformationA, winstadll, 'WinStationSetInformationA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationSetInformationA]
  end;
end;

var
  __WinStationSetInformationW: Pointer;

function WinStationSetInformationW;
begin
  GetProcedureAddress(__WinStationSetInformationW, winstadll, 'WinStationSetInformationW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationSetInformationW]
  end;
end;

var
  __WinStationShadow: Pointer;

function WinStationShadow;
begin
  GetProcedureAddress(__WinStationShadow, winstadll, 'WinStationShadow');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationShadow]
  end;
end;


var
  __WinStationShadowStop : Pointer;

function WinStationShadowStop;
begin
  GetProcedureAddress(__WinStationShadowStop, winstadll, 'WinStationShadowStop');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationShadowStop]
  end;
end;


var
  __WinStationShutDownSystem : Pointer;

function WinStationShutDownSystem;
begin
  GetProcedureAddress(__WinStationShutDownSystem, winstadll, 'WinStationShutDownSystem');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationShutDownSystem]
  end;
end;

var
  __WinStationTerminateProcess: Pointer;

function WinStationTerminateProcess;
begin
  GetProcedureAddress(__WinStationTerminateProcess, winstadll, 'WinStationTerminateProcess');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationTerminateProcess]
  end;
end;

var
  __WinStationUnRegisterCN: Pointer;

function WinStationUnRegisterConsoleNotification;
begin
  GetProcedureAddress(__WinStationUnRegisterCN, winstadll, 'WinStationUnRegisterConsoleNotification');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [__WinStationUnRegisterCN]
  end;
end;
{$ENDIF DYNAMIC_LINK}

function IsWindows7Beta: boolean;
var VersionInfo: TOSVersionInfoEx;
begin
  // Zero Memory and set structure size
  ZeroMemory(@VersionInfo, SizeOf(VersionInfo));
  VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
  GetVersionEx(@VersionInfo);
  // Are we running Vista RTM?
  Result := (VersionInfo.dwMajorVersion = 6) and
    (VersionInfo.dwMinorVersion = 1) and
    (VersionInfo.wProductType = VER_NT_WORKSTATION) and
    (VersionInfo.dwBuildNumber = 7000);
end;

// This function is not exported
function IsVistaRTM: boolean;
var VersionInfo: TOSVersionInfoEx;
begin
  // Zero Memory and set structure size
  ZeroMemory(@VersionInfo, SizeOf(VersionInfo));
  VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
  GetVersionEx(@VersionInfo);

  // Are we running Vista RTM?
  Result := (VersionInfo.dwMajorVersion = 6) and
    (VersionInfo.dwMinorVersion = 0) and
    (VersionInfo.wProductType = VER_NT_WORKSTATION) and
    (VersionInfo.wServicePackMajor = 0); //If no Service Pack has been installed, the value is zero
end;

function IsVista: boolean;
var VersionInfo: TOSVersionInfoEx;
begin
  // Zero Memory and set structure size
  ZeroMemory(@VersionInfo, SizeOf(VersionInfo));
  VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
  GetVersionEx(@VersionInfo);

  // Are we running Vista?
  Result := (VersionInfo.dwMajorVersion = 6) and
    (VersionInfo.dwMinorVersion = 0) and
    (VersionInfo.wProductType = VER_NT_WORKSTATION);
end;

// This the way QWinsta checks if Terminal Services is active:
function AreWeRunningTerminalServices: Boolean;
var VersionInfo: TOSVersionInfoEx;
  dwlConditionMask: Int64;
begin
  // Zero Memory and set structure size
  ZeroMemory(@VersionInfo, SizeOf(VersionInfo));
  VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);

  // We are either Terminal Server or Personal Terminal Server
  VersionInfo.wSuiteMask := VER_SUITE_TERMINAL or VER_SUITE_SINGLEUSERTS;
  dwlConditionMask := VerSetConditionMask(0, VER_SUITENAME, VER_OR);

  // Test it
  Result := VerifyVersionInfo(VersionInfo, VER_SUITENAME, dwlConditionMask);
end;

// This functions converts CPU times as returned by
// TSystemProcesses structure to a string
function CpuTime2Str(ACPUTime: LARGE_INTEGER): String;
var
  SystemTime: TSystemTime;
  TimeSeparator: Char;
begin
  TimeSeparator := GetLocaleChar(LOCALE_USER_DEFAULT, LOCALE_STIME, ':');
  FileTimeToSystemTime(FILETIME(ACPUTime), SystemTime);

  Result := Format('%0:.2d%1:s%2:.2d%3:s%4:.2d', [SystemTime.wHour, TimeSeparator,
    SystemTime.wMinute, TimeSeparator, SystemTime.wSecond]);
end;

function DateTimeStringSafe(DateTime: PFILETIME; lpBuffer: PWideChar;
  cchDest: SIZE_T): PWideChar; stdcall;
begin
  // Zero Memory
  ZeroMemory(lpBuffer, cchDest * SizeOf(WCHAR));

 // Are we running Vista?
  if IsVistaRTM or IsWindows7Beta then
  begin
    // Vista version
    Result := DateTimeStringVistaRTM(DateTime, lpBuffer, cchDest);
  end
  else begin
    // Other OS's (including server 2008!)
    Result := DateTimeString(DateTime, lpBuffer);
  end;
end;

// DiffTimeString is a helper function that returns a formatted
// Elapsed time string (the way Idle Time is displayed in TSAdmin)
// Return value is the string length
function DiffTimeString(FTLow: FILETIME; FTHigh: FILETIME;
  out pwElapsedTime: PWideChar): Integer;
var
  DiffSecs: INT64;
  DiffTime: TDiffTime;
  NumChars: DWORD;
begin
  // Get the Difftime where Time1 is the "oldest" time
  // Return value is the difference in seconds
  DiffSecs := CalculateDiffTime(Int64(FTLow), Int64(FTHigh));
  // Recalc DiffTime to TDiffTime
  ZeroMemory(@DiffTime, SizeOf(DiffTime));
  // Calculate no of whole days
  DiffTime.wDays := DiffSecs DIV SECONDS_PER_DAY;
  // Calculate no of whole hours
  DiffTime.wHours :=  DiffSecs MOD SECONDS_PER_DAY DIV SECONDS_PER_HOUR;
  // Calculate no of whole minutes
  DiffTime.wMinutes := DiffSecs MOD SECONDS_PER_DAY MOD SECONDS_PER_HOUR
    DIV SECONDS_PER_MINUTE; // Result = No of whole minutes
  // Calculate no of whole seconds
  DiffTime.wSeconds := DiffSecs MOD SECONDS_PER_DAY MOD SECONDS_PER_HOUR
    MOD SECONDS_PER_MINUTE; // Result = No of seconds
  // Note that Milliseconds are not used and therefore not calculated

  // Reserve Memory
  GetMem(pwElapsedTime, ELAPSED_TIME_STRING_LENGTH * SizeOf(WCHAR));
  // Format Elapsed TimeString in minutes (bShowSeconds = False)
  NumChars := ElapsedTimeStringSafe(@DiffTime, False, pwElapsedTime,
    ELAPSED_TIME_STRING_LENGTH);
  Result := NumChars;
  // Caller has to free memory when done
end;

function ElapsedTimeStringSafe(DiffTime: PDiffTime; bShowSeconds: Boolean;
  lpElapsedTime: PWideChar; cchDest: SIZE_T): Integer;
var
  hr: HRESULT;
begin
  // Zero Memory
  ZeroMemory(lpElapsedTime, cchDest * SizeOf(WCHAR));

 // Are we running Vista?
  if IsVistaRTM or IsWindows7Beta then
  begin
    hr := ElapsedTimeStringVistaRTM(DiffTime, bShowSeconds, lpElapsedTime,
      cchDest);
    if Succeeded(hr) then
    begin
      Result := cchDest;
    end
    else begin
      Result := 0;
    end;
   
  end
  else begin
    Result := ElapsedTimeString(DiffTime, bShowSeconds, lpElapsedTime);
  end;
  // Caller has to free memory when done
end;

function FileTime2DateTime(FileTime: TFileTime): TDateTime;
var
  LocalFileTime: TFileTime;
  SystemTime: TSystemTime;
const
   EncodeDateTimeMinValue = -109205;
begin
  { TerminalServer works with FILETIMES which represent the number of
    100-nanosecond intervals since January 1, 1601, so we set the default to
    that date }

  Result := EncodeDateTimeMinValue; //EncodeDateTime(1601, 1, 1, 0, 0, 0, 0); = -109205

  if FileTimeToLocalFileTime(FileTime, LocalFileTime) then
  begin
    if FileTimeToSystemTime(LocalFileTime, SystemTime) then
    begin
      try
        // SystemTimeToDateTime can raise EConvertError!
        Result := SystemTimeToDateTime(SystemTime);
      except
        on E: Exception do
        begin
          // Ignore the exception and thus return default
        end;
      end;
    end;
  end;
end;

{procedure InitTermSrvCounterArray(var ATermSrvCounterArray: TTermSrvCounterArray);
begin
  ATermSrvCounterArray[1].dwIndex := TOTAL_SESSIONS_CREATED_COUNTER;
  ATermSrvCounterArray[2].dwIndex := TOTAL_SESSIONS_DISCONNECTED_COUNTER;
  ATermSrvCounterArray[3].dwIndex := TOTAL_SESSIONS_RECONNECTED_COUNTER;
  ATermSrvCounterArray[4].dwIndex := TOTAL_SESSIONS_TOTAL_CONNECTED_NOW_COUNTER;
  ATermSrvCounterArray[5].dwIndex := TOTAL_SESSIONS_TOTAL_DISCONNECTED_NOW_COUNTER;
  ATermSrvCounterArray[6].dwIndex := TOTAL_SESSIONS_TOTAL_CONNECTED_NOW_COUNTER_2;
  ATermSrvCounterArray[7].dwIndex := TOTAL_SESSIONS_TOTAL_DISCONNECTED_NOW_COUNTER_2;
end;}

// This is the way WTSApi32.dll checks if Terminal Service is running
function IsTerminalServiceRunning: boolean;
var hSCM: HANDLE;
  hService: HANDLE;
  ServiceStatus: SERVICE_STATUS;
begin
  Result := False;
  // Open handle to Service Control Manager
  hSCM := OpenSCManager(nil, SERVICES_ACTIVE_DATABASE, GENERIC_READ);
  if hSCM > 0 then
  begin
    // Open handle to Terminal Server Service
    hService := OpenService(hSCM, 'TermService', GENERIC_READ);
    if hService > 0 then
    begin
      // Check if the service is running
      QueryServiceStatus(hService, ServiceStatus);
      Result := ServiceStatus.dwCurrentState = SERVICE_RUNNING;
      // Close the handle
      CloseServiceHandle(hService);
    end;
    // Close the handle
    CloseServiceHandle(hSCM);
  end;
end;

function QueryCurrentWinStationSafe(pWinStationName: LPWSTR;
  pUserName: PWideChar; cchDest: DWORD; var SessionId: DWORD;
  var WdFlag: DWORD): Boolean;
begin
  // Zero Memory
  ZeroMemory(pWinStationName, (WINSTATIONNAME_LENGTH+1) * SizeOf(WChar));
  ZeroMemory(pUserName, cchDest * SizeOf(WCHAR));

  // Are we running Vista?
  if IsVistaRTM then
  begin
    Result := QueryCurrentWinStationVistaRTM(pWinStationName, pUserName, cchDest,
      SessionId, WdFlag);
  end
  else begin
    Result := QueryCurrentWinStation(pWinStationName, pUserName, SessionId,
      WdFlag);
  end;

end;

function WinStationGetRemoteIPAddress(hServer: HANDLE; SessionId: DWORD;
  var RemoteIPAddress: WideString; var Port: WORD): Boolean;
var WinStationRemoteIPAddress: TWinStationRemoteAddress;
  pReturnLength: DWORD;
begin
  // Zero Memory
  ZeroMemory(@WinStationRemoteIPAddress, SizeOf(WinStationRemoteIPAddress));
  // Query Remote Address
  Result := WinStationQueryInformationW(hServer, SessionId,
    WinStationRemoteAddress, @WinStationRemoteIPAddress,
    SizeOf(WinStationRemoteIPAddress), pReturnLength);
  if Result then
  begin
    // If the AddressFamily is IPv4
    if WinStationRemoteIPAddress.AddressFamily = AF_INET then
    begin
      // The ntohs function converts a u_short from TCP/IP network byte order
      // to host byte order (which is little-endian on Intel processors).
      Port := ntohs(WinStationRemoteIPAddress.Port);
      with WinStationRemoteIPAddress do
      begin
        // format the IP Address as string
        RemoteIPAddress := Format('%d.%d.%d.%d', [Address[2], Address[3],
          Address[4], Address[5]]);
        // If you want to convert the to a sockaddr structure you could
        // user WSAStringToAddress
      end;
    end
    else begin
      Result := False;
      Port := 0;
      RemoteIPAddress := '';
      // SetLastError to give the user a clue as to why we failed..
      //  An address incompatible with the requested protocol was used.
      // (An address incompatible with the requested protocol was used.)
      SetLastError(WSAEAFNOSUPPORT);
    end;
  end;
end;

function WinStationQueryUserToken(hServer: HANDLE; SessionId: DWORD;
  var hToken: HANDLE): Boolean;
var WinstaUserToken: _WINSTA_USER_TOKEN;
  dwReturnLength: DWORD;
  LUID: _LUID;
  bWasPrivEnabled: Boolean;
  Res: NTSTATUS;
begin
  // Enable SeTcbPrivilege (system account has this enabled by default)
  LookupPrivilegeValue(nil, SE_TCB_NAME, LUID);
  Res := RtlAdjustPrivilege(LUID.LowPart, True, False, @bWasPrivEnabled);

  // Initialize structure
  WinstaUserToken.ProcessId :=  GetCurrentProcessId;  // Current Process Id
  WinstaUserToken.ThreadId := GetCurrentThreadId; // Current Thread Id
  WinstaUserToken.TokenHandle := 0;

  if Res = STATUS_SUCCESS then
  begin
    // Query for the token, we are only allowed to do this if we are the
    // System account (else ACCESS_DENIED is returned)
    Result := WinStationQueryInformationW(hServer, SessionId, WinStationUserToken,
      @WinstaUserToken, SizeOf(WinstaUserToken), dwReturnLength);
    hToken := WinStaUserToken.TokenHandle;

    // Restore state of SeTcbPrivilege
    RtlAdjustPrivilege(LUID.LowPart, bWasPrivEnabled, False, @bWasPrivEnabled);
  end
  else begin
    Result := False;
    // Convert NTStatus to WinError and SetLastError
    SetLastError(RtlNtStatusToDosError(Res));
  end;

end;

{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}

