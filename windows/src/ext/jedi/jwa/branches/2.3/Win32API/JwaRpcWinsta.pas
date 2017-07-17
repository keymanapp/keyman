{******************************************************************************}
{ Windows API interface Unit for Object Pascal                                 }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) Microsoft Corporation.       }
{  All Rights Reserved.                                                        }
{                                                                              }
{ The original Pascal code is: JwaRpcWinsta.pas, released April 2008.          }
{                                                                              }
{ Portions created by Remko Weijnen are Copyright (C) 2008                     }
{ Remko Weijnen. All Rights Reserved.                                          }
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
{ This unit is currently not part of JwaWindows.pas and MUST be included by    }
{ uses clause. To use this unit you must compile JwaWindows.pas with include   }
{ mode.                                                                        }

{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaRpcWinsta;

interface

uses
// Using JwaWindows for now for easier testing (because of the new Include model)
// Should be JwaWinType
{$IFDEF JWA_WINDOWS}
  JwaWindows;
{$ELSE}
  JwaWinType, JwaBitFields, JwaWinNT;
{$ENDIF JWA_WINDOWS}

{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}

{/// [MS-TSTS] specific defines }
const
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
  _SDCLASS = (
    SdNone {= 0},
    SdConsole,
    SdNetwork  );
  SDCLASS = _SDCLASS;
  TSDClass = _SDCLASS;

type
  _FLOWCONTROLCLASS = (
    FlowControl_None, 
    FlowControl_Hardware, 
    FlowControl_Software  );
  FLOWCONTROLCLASS = _FLOWCONTROLCLASS;
  TFlowControlClass = _FLOWCONTROLCLASS;

  type
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

type
  NASISPECIFICNAMEW = Array[0..NASISPECIFICNAME_LENGTH] of WCHAR;
  NASISPECIFICNAMEA = Array[0..NASISPECIFICNAME_LENGTH] of AnsiChar;

  NASIUSERNAMEW = Array[0..NASIUSERNAME_LENGTH] of WCHAR;
  NASIUSERNAMEA = Array[0..NASIUSERNAME_LENGTH] of AnsiChar;

  NASIPASSWORDW = Array[0..NASIPASSWORD_LENGTH] of WCHAR;
  NASIPASSWORDA = Array[0..NASIPASSWORD_LENGTH] of AnsiChar;

  NASISESSIONNAMEW = Array[0..NASISESSIONNAME_LENGTH] of WCHAR;
  NASISESSIONNAMEA = Array[0..NASISESSIONNAME_LENGTH] of AnsiChar;

  NASIFILESERVERW = Array[0..NASIFILESERVER_LENGTH] of WCHAR;
  NASIFILESERVERA = Array[0..NASIFILESERVER_LENGTH] of AnsiChar;

  CLIENTDATANAME = Array[0..CLIENTDATANAME_LENGTH] of AnsiChar;
  PCLIENTDATANAME = PAnsiChar;

  WINSTATIONNAMEW = Array[0..WINSTATIONNAME_LENGTH] of WCHAR;
  WINSTATIONNAMEA = Array[0..WINSTATIONNAME_LENGTH] of AnsiChar;

type
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

type
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

  SESSION_FILTER = (
    SF_SERVICES_SESSION_POPUP
  );


const
  PROTOCOL_CONSOLE = 0;
  PROTOCOL_ICA = 1;
  PROTOCOL_TSHARE = 2;
  PROTOCOL_RDP = 2;
  PDNAME_LENGTH = 32;
  WDNAME_LENGTH = 32;
  CDNAME_LENGTH = 32;
  DEVICENAME_LENGTH = 128;
  MODEMNAME_LENGTH = DEVICENAME_LENGTH;
  CALLBACK_LENGTH = 50;
  DLLNAME_LENGTH = 32;
  WINSTATIONCOMMENT_LENGTH = 60;
  MAX_LICENSE_SERVER_LENGTH = 1024;
  LOGONID_CURRENT = DWORD(-1);
  MAX_PDCONFIG = 10;

  TERMSRV_TOTAL_SESSIONS = 1;
  TERMSRV_DISC_SESSIONS = 2;
  TERMSRV_RECON_SESSIONS = 3;
  TERMSRV_CURRENT_ACTIVE_SESSIONS = 4;
  TERMSRV_CURRENT_DISC_SESSIONS = 5;
  TERMSRV_PENDING_SESSIONS = 6;
  TERMSRV_SUCC_TOTAL_LOGONS = 7;
  TERMSRV_SUCC_LOCAL_LOGONS = 8;
  TERMSRV_SUCC_REMOTE_LOGONS = 9;
  TERMSRV_SUCC_SESSION0_LOGONS = 10;
  TERMSRV_CURRENT_TERMINATING_SESSIONS = 11;
  TERMSRV_CURRENT_LOGGEDON_SESSIONS = 12;

  NO_FALLBACK_DRIVERS = $0;
  FALLBACK_BESTGUESS = $1;
  FALLBACK_PCL = $2;
  FALLBACK_PS = $3;
  FALLBACK_PCLANDPS = $4;

{******************************** }
{ WinStationOpen access values    }
{******************************** }

const
  WINSTATION_QUERY = $00000001; {/* WinStationQueryInformation()*/}
  WINSTATION_SET = $00000002; {/* WinStationSetInformation()*/}
  WINSTATION_RESET = $00000004; {/* WinStationReset()*/}
  WINSTATION_VIRTUAL = $00000008; {/* read/write direct data*/}
  WINSTATION_SHADOW = $00000010; {/* WinStationShadow()*/}
  WINSTATION_LOGON = $00000020; {/* logon to WinStation*/}
  WINSTATION_LOGOFF = $00000040; {/* WinStationLogoff()*/}
  WINSTATION_MSG = $00000080; {/* WinStationMsg()*/}
  WINSTATION_CONNECT = $00000100; {/* WinStationConnect()*/}
  WINSTATION_DISCONNECT = $00000200; {/* WinStationDisconnect()*/}
  WINSTATION_GUEST_ACCESS = (WINSTATION_LOGON);
  WINSTATION_CURRENT_GUEST_ACCESS = WINSTATION_VIRTUAL or WINSTATION_LOGOFF;
  WINSTATION_USER_ACCESS = WINSTATION_GUEST_ACCESS or WINSTATION_QUERY or
    WINSTATION_CONNECT;
  WINSTATION_CURRENT_USER_ACCESS = WINSTATION_SET or WINSTATION_RESET or
    WINSTATION_VIRTUAL or WINSTATION_LOGOFF or WINSTATION_DISCONNECT;
  WINSTATION_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or
    WINSTATION_QUERY or
    WINSTATION_SET or
    WINSTATION_RESET or
    WINSTATION_VIRTUAL or
    WINSTATION_SHADOW or
    WINSTATION_LOGON or
    WINSTATION_MSG or
    WINSTATION_CONNECT or
    WINSTATION_DISCONNECT;



type
  PDNAMEW = Array[0..PDNAME_LENGTH] of WCHAR;
  PPDNAMEW = PWCHAR;
  PDNAMEA = Array[0..PDNAME_LENGTH] of AnsiChar;
  PPDNAMEA = PAnsiChar;

{$IFDEF UNICODE}
  PDNAME = PDNAMEW;
  PPDNAME = PPDNAMEW;
{$ELSE}
  PDNAME = PDNAMEA;
  PPDNAME = PPDNAMEA;
{$ENDIF UNICODE}

{+//------------------------------------------------*/ }

type
  WDNAMEW = Array[0..WDNAME_LENGTH] of WCHAR;
  PWDNAMEW = PWCHAR;
  WDNAMEA = Array[0..WDNAME_LENGTH] of AnsiChar;
  PWDNAMEA = PAnsiChar;
{$IFDEF UNICODE}
  WDNAME = WDNAMEW;
  PWDNAME = PWDNAMEW;
{$ELSE}
  WDNAME = WDNAMEA;
  PWDNAME = PWDNAMEA;
{$ENDIF /* UNICODE*/}

{+//------------------------------------------------*/ }

type
  CDNAMEW = Array[0..CDNAME_LENGTH] of WCHAR;
  PCDNAMEW = PWCHAR;
  CDNAMEA = Array[0..CDNAME_LENGTH] of AnsiChar;
  PCDNAMEA = PAnsiChar;
{$IFDEF UNICODE}
  CDNAME = CDNAMEW;
  PCDNAME = PCDNAMEW;
{$ELSE}
  CDNAME = CDNAMEA;
  PCDNAME = PCDNAMEA;
{$ENDIF /* UNICODE*/}

{+//------------------------------------------------*/ }

type
  DEVICENAMEW = Array[0..DEVICENAME_LENGTH] of WCHAR;
  PDEVICENAMEW = PWCHAR;
  DEVICENAMEA = Array[0..DEVICENAME_LENGTH] of AnsiChar;
  PDEVICENAMEA = PAnsiChar;
{$IFDEF UNICODE}
  DEVICENAME = DEVICENAMEW;
  PDEVICENAME = PDEVICENAMEW;
{$ELSE}
  DEVICENAME = DEVICENAMEA;
  PDEVICENAME = PDEVICENAMEA;
{$ENDIF /* UNICODE*/}

{+//------------------------------------------------*/ }

type
  MODEMNAMEW = Array[0..MODEMNAME_LENGTH] of WCHAR;
  PMODEMNAMEW = PWCHAR;
  MODEMNAMEA = Array[0..MODEMNAME_LENGTH] of AnsiChar;
  PMODEMNAMEA = PAnsiChar;
{$IFDEF UNICODE}
type
  MODEMNAME = MODEMNAMEW;
  PMODEMNAME = PMODEMNAMEW;
{$ELSE}
  MODEMNAME = MODEMNAMEA;
  PMODEMNAME = PMODEMNAMEA;
{$ENDIF /* UNICODE*/}

{+//------------------------------------------------*/ }

type
  DLLNAMEW = Array[0..DLLNAME_LENGTH] of WCHAR;
  PDLLNAMEW = PWCHAR;
  DLLNAMEA = Array[0..DLLNAME_LENGTH] of AnsiChar;
  PDLLNAMEA = PAnsiChar;
{$IFDEF UNICODE}
  DLLNAME = DLLNAMEW;
  PDLLNAME = PDLLNAMEW;
{$ELSE}
  DLLNAME = DLLNAMEA;
  PDLLNAME = PDLLNAMEA;
{$ENDIF /* UNICODE*/}

{+//------------------------------------------------*/ }

type
  WDPREFIXW = Array[0..WDPREFIX_LENGTH] of WCHAR;
  PWDPREFIXW = PWCHAR;
  WDPREFIXA = Array[0..WDPREFIX_LENGTH] of AnsiChar;
  PWDPREFIXA = PAnsiChar;

{$IFDEF UNICODE}
  WDPREFIX = WDPREFIXW;
  PWDPREFIX = PWDPREFIXW;
{$ELSE}
  WDPREFIX = WDPREFIXA;
  PWDPREFIX = PWDPREFIXA;
{$ENDIF /* UNICODE*/}

{+// }
{-* Stack address structure }
{= }

type
  _CLIENT_STACK_ADDRESS = record
    Address: Array[0..STACK_ADDRESS_LENGTH-1] of BYTE;
  end {_CLIENT_STACK_ADDRESS};
  CLIENT_STACK_ADDRESS = _CLIENT_STACK_ADDRESS;
  PCLIENT_STACK_ADDRESS = ^_CLIENT_STACK_ADDRESS;

type
  _TS_TRACE = record
    TraceFile: Array[0..256-1] of WCHAR;
    fDebugger: BOOLEAN;
    fTimestamp: BOOLEAN;
    TraceClass: ULONG;
    TraceEnable: ULONG;
    TraceOption: Array[0..64-1] of WCHAR;
  end {_TS_TRACE};
  TS_TRACE = _TS_TRACE;
  PTS_TRACE = ^_TS_TRACE;

const
  EXTENDED_USERNAME_LEN = 255;
  EXTENDED_PASSWORD_LEN = 255;
  EXTENDED_DOMAIN_LEN = 255;

type
  _ExtendedClientCredentials = record
    UserName: Array[0..EXTENDED_USERNAME_LEN] of WCHAR;
    Password: Array[0..EXTENDED_PASSWORD_LEN] of WCHAR;
    DOMAIN: Array[0..EXTENDED_DOMAIN_LEN] of WCHAR;
  end {_ExtendedClientCredentials};
  ExtendedClientCredentials = _ExtendedClientCredentials;
  pExtendedClientCredentials = ^_ExtendedClientCredentials;

{+//******************************** }
{-* User Configuration structures }
{=******************************** }

type
  APPLICATIONNAMEW = Array[0..MAX_BR_NAME] of WCHAR;
  PAPPLICATIONNAMEW = PWCHAR;
  APPLICATIONNAMEA = Array[0..MAX_BR_NAME] of AnsiChar;
  PAPPLICATIONNAMEA = PAnsiChar;
{$IFDEF UNICODE}
  APPLICATIONNAME = APPLICATIONNAMEW;
  PAPPLICATIONNAME = PAPPLICATIONNAMEW;
{$ELSE}
  APPLICATIONNAME = APPLICATIONNAMEA;
  PAPPLICATIONNAME = PAPPLICATIONNAMEA;
{$ENDIF /* UNICODE*/}

{+// }
{-* Shadow options }
{= }

type
  _SHADOWCLASS = (
    Shadow_Disable, 
    Shadow_EnableInputNotify, 
    Shadow_EnableInputNoNotify, 
    Shadow_EnableNoInputNotify, 
    Shadow_EnableNoInputNoNotify );
  SHADOWCLASS = _SHADOWCLASS;
  TShadowClass = _SHADOWCLASS;
  
{+// }
{-* Callback options }
{= }

type
  _CALLBACKCLASS = (
    Callback_Disable,
    Callback_Roving,
    Callback_Fixed );
  CALLBACKCLASS = _CALLBACKCLASS;
  TCallBackClass = _CALLBACKCLASS;

type
  _POLICY_TS_MACHINE = record
    PolicyTsMachineFlags1: Set Of (
  	  fPolicyDisableClip,
  	  fPolicyDisableCam,
  	  fPolicyDisableCcm,
  	  fPolicyDisableLPT,
  	  fPolicyDisableCpm,
  	  fPolicyPromptForPassword,
  	  fPolicyMaxInstanceCount,
  	  fPolicyMinEncryptionLevel,
  	  fPolicyFipsEnabled,
  	  fPolicyDisableAutoReconnect,
  	  fPolicyWFProfilePat,
  	  fPolicyWFHomeDir,
  	  fPolicyWFHomeDirDriv,
  	  fPolicyDenyTSConnections,
  	  fPolicyTempFoldersPerSession,
  	  fPolicyDeleteTempFoldersOnExit,
  	  fPolicyColorDepth,
  	  fPolicySessionDirectoryActive,
  	  fPolicySessionDirectoryLocation,
  	  fPolicySessionDirectoryClusterName,
  	  fPolicySessionDirectoryAdditionalParams,
  	  fPolicySessionDirectoryExposeServerIP,
  	  fPolicyPreventLicenseUpgrade,
  	  fPolicySecureLicensing,
  	  fPolicyWritableTSCCPermissionsTAB,
  	  fPolicyDisableCdm,
  	  fPolicyForceClientLptDef,
  	  fPolicyShadow,
  	  fPolicyResetBroken,
  	  fPolicyReconnectSame,
  	  fPolicyMaxSessionTime,
  	  fPolicyMaxDisconnectionTime,
  	  fPolicyMaxIdleTime,
	    fPolicyInitialProgram,
  	  fPolicySingleSessionPerUser,
  	  fPolicyDisableWallpaper,
  	  fPolicyKeepAlive  ,
  	  fPolicyEnableTimeZoneRedirection,
  	  fPolicyDisableForcibleLogoff,
  	  fPolicyLicensingMode,
  	  fPolicyExplicitLSDiscover,
  	  fPolicyDisableTerminalServerTooltip,
  	  fPolicyTsMachineDisableClip,
  	  fPolicyTsMachineDisableCam,
  	  fPolicyTsMachineDisableCcm,
  	  fPolicyTsMachineDisableLPT,
  	  fPolicyTsMachineDisableCpm,
  	  fPolicyTsMachinePromptForPassword,
      PolicyTSMachineColorDepth0,    //	ULONG ColorDepth: 3;
      PolicyTSMachineColorDepth1,
      PolicyTSMachineColorDepth2,
  	  fDenyTSConnections,
  	  fTempFoldersPerSession,
  	  fDeleteTempFoldersOnExit,
  	  fWritableTSCCPermissionsTAB,
  	  fDisableCdm,
  	  fPolicyTSMachineForceClientLptDef,
  	  fPolicyTSMachineResetBroken,
  	  fPolicyTSMachineReconnectSame,
  	  fSingleSessionPerUser,
  	  fDisableWallpaper,
  	  fKeepAliveEnable,
  	  fPreventLicenseUpgrade,
  	  fSecureLicensing,
  	  fEnableTimeZoneRedirection,
  	  fTSPolicyMachineDisableAutoReconnect,
  	  fDisableForcibleLogoff,
  	  fPolicyEncryptRPCTraffic,
  	  fEncryptRPCTraffic,
  	  fPolicyTSMachineErrorInvalidProfile,
      fFallbackPrintDriverType0,  // ULONG   FallbackPrintDriverType: 3;
      fFallbackPrintDriverType1,
      fFallbackPrintDriverType2,	
  	  fDisableTerminalServerTooltip,
{$IFDEF DELPHI6_UP}
      _PolicyTsMachineFlags1Align = al96bit // align the bitset on 3 ULONG's
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
      Bits_0_17,
      Bits_0_18,
      Bits_0_19,
      Bits_0_20,
      Bits_0_21,
      Bits_0_22,
      Bits_0_23,
      Bits_0_24,
      Bits_0_25,
      Bits_0_26,
      Bits_0_27,
      Bits_0_28,
      Bits_0_29,
      Bits_0_30,
      Bits_0_31,
      Bits_0_32,
      Bits_0_33,
      Bits_0_34,
      Bits_0_35,
      Bits_0_36,
      Bits_0_37,
      Bits_0_38,
      Bits_0_39,
      Bits_0_40,
      Bits_0_41,
      Bits_0_42,
      Bits_0_43,
      Bits_0_44,
      Bits_0_45,
      Bits_0_46,
      Bits_0_47,
      Bits_0_48,
      Bits_0_49,
      Bits_0_50,
      Bits_0_51,
      Bits_0_52,
      Bits_0_53,
      Bits_0_54,
      Bits_0_55,
      Bits_0_56,
      Bits_0_57,
      Bits_0_58,
      Bits_0_59,
      Bits_0_60,
      Bits_0_61,
      Bits_0_62,
      Bits_0_63,
      Bits_0_64,
      Bits_0_65,
      Bits_0_66,
      Bits_0_67,
      Bits_0_68,
      Bits_0_69,
      Bits_0_70,
      Bits_0_71,
      Bits_0_72,
      Bits_0_73,
      Bits_0_74,
      Bits_0_75,
      Bits_0_76,
      Bits_0_77,
      Bits_0_78,
      Bits_0_79,
      Bits_0_80,
      Bits_0_81,
      Bits_0_82,
      Bits_0_83,
      Bits_0_84,
      Bits_0_85,
      Bits_0_86,
      Bits_0_87,
      Bits_0_88,
      Bits_0_89,
      Bits_0_90,
      Bits_0_91,
      Bits_0_92,
      Bits_0_93,
      Bits_0_94,
      Bits_0_95,
      Bits_0_96



{$ENDIF}
 	  );
	  bSecurityLayer: Boolean;
	  PolicyTsMachineFlag2: Set Of (
		  fPolicySecurityLayer,
{$IFDEF DELPHI6_UP}
      _PolicyTsMachineFlags2Align = al32bit // align the bitset on ULONG
{$ELSE}
      _PolicyTsMachineFlags2Align1,
      _PolicyTsMachineFlags2Align2,
      _PolicyTsMachineFlags2Align3,
      _PolicyTsMachineFlags2Align4,
      _PolicyTsMachineFlags2Align5,
      _PolicyTsMachineFlags2Align6,
      _PolicyTsMachineFlags2Align7,
      _PolicyTsMachineFlags2Align8,
      _PolicyTsMachineFlags2Align9,
      _PolicyTsMachineFlags2Align10,
      _PolicyTsMachineFlags2Align11,
      _PolicyTsMachineFlags2Align12,
      _PolicyTsMachineFlags2Align13,
      _PolicyTsMachineFlags2Align14,
      _PolicyTsMachineFlags2Align15,
      _PolicyTsMachineFlags2Align16,
      _PolicyTsMachineFlags2Align17,
      _PolicyTsMachineFlags2Align18,
      _PolicyTsMachineFlags2Align19,
      _PolicyTsMachineFlags2Align20,
      _PolicyTsMachineFlags2Align21,
      _PolicyTsMachineFlags2Align22,
      _PolicyTsMachineFlags2Align23,
      _PolicyTsMachineFlags2Align24,
      _PolicyTsMachineFlags2Align25,
      _PolicyTsMachineFlags2Align26,
      _PolicyTsMachineFlags2Align27,
      _PolicyTsMachineFlags2Align28,
      _PolicyTsMachineFlags2Align29,
      _PolicyTsMachineFlags2Align30,
      _PolicyTsMachineFlags2Align31,
      _PolicyTsMachineFlags2Align32

{$ENDIF}
	  );
	  bUserAuthentication: Boolean;
	  PolicyTsMachineFlags3: Set Of (
		  fPolicyUserAuthentication,
  		fPolicyTurnOffSingleAppMode,
  		fTurnOffSingleAppMode,
  		fDisablePNPPolicyIsEnforced,
  		fDisablePNPPolicyValue,
{$IFDEF DELPHI6_UP}
      _PolicyTsMachineFlags3Align = al32bit // align the bitset on ULONG
{$ELSE}
    _PolicyTsMachineFlags3Align1,
    _PolicyTsMachineFlags3Align2,
    _PolicyTsMachineFlags3Align3,
    _PolicyTsMachineFlags3Align4,
    _PolicyTsMachineFlags3Align5,
    _PolicyTsMachineFlags3Align6,
    _PolicyTsMachineFlags3Align7,
    _PolicyTsMachineFlags3Align8,
    _PolicyTsMachineFlags3Align9,
    _PolicyTsMachineFlags3Align10,
    _PolicyTsMachineFlags3Align11,
    _PolicyTsMachineFlags3Align12,
    _PolicyTsMachineFlags3Align13,
    _PolicyTsMachineFlags3Align14,
    _PolicyTsMachineFlags3Align15,
    _PolicyTsMachineFlags3Align16,
    _PolicyTsMachineFlags3Align17,
    _PolicyTsMachineFlags3Align18,
    _PolicyTsMachineFlags3Align19,
    _PolicyTsMachineFlags3Align20,
    _PolicyTsMachineFlags3Align21,
    _PolicyTsMachineFlags3Align22,
    _PolicyTsMachineFlags3Align23,
    _PolicyTsMachineFlags3Align24,
    _PolicyTsMachineFlags3Align25,
    _PolicyTsMachineFlags3Align26,
    _PolicyTsMachineFlags3Align27,
    _PolicyTsMachineFlags3Align28,
    _PolicyTsMachineFlags3Align29,
    _PolicyTsMachineFlags3Align30,
    _PolicyTsMachineFlags3Align31,
    _PolicyTsMachineFlags3Align32
{$ENDIF}
    );
    MaxInstanceCount: ULONG;
    LicensingMode: ULONG;
    MinEncryptionLevel: BYTE;
    WFProfilePath: Array[0..DIRECTORY_LENGTH] of WCHAR;
    WFHomeDir: Array[0..DIRECTORY_LENGTH] of WCHAR;
    WFHomeDirDrive: Array[0..3] of WCHAR;
    SessionDirectoryActive: ULONG;
    SessionDirectoryLocation: Array[0..DIRECTORY_LENGTH] of WCHAR;
    SessionDirectoryClusterName: Array[0..DIRECTORY_LENGTH] of WCHAR;
    SessionDirectoryAdditionalParams: Array[0..DIRECTORY_LENGTH] of WCHAR;
    SessionDirectoryExposeServerIP: ULONG;
    KeepAliveInterval: ULONG;
    Shadow: _SHADOWCLASS;
    MaxConnectionTime: ULONG;
    MaxDisconnectionTime: ULONG;
    MaxIdleTime: ULONG;
    WorkDirectory: Array[0..DIRECTORY_LENGTH] of WCHAR;
    InitialProgram: Array[0..INITIALPROGRAM_LENGTH] of WCHAR;
    LicenseServers: Array[0..MAX_LICENSE_SERVER_LENGTH] of WCHAR;
    __Alignment4: ULONG;      // Need to align at 8 byte boundary
  end {_POLICY_TS_MACHINE};
  POLICY_TS_MACHINE = _POLICY_TS_MACHINE;
  PPOLICY_TS_MACHINE = ^_POLICY_TS_MACHINE;
  TPolicyTSMachine = _POLICY_TS_MACHINE;
  PPolicyTSMachine = PPOLICY_TS_MACHINE;

{+// }
{-* User Configuration data }
{= }

type
  TUserConfigFlags = Set Of(
    fInheritAutoLogon,                                //: 1
  	fInheritResetBroken,                              //: 1
  	fInheritReconnectSame,                            //: 1
  	fInheritInitialProgram,                           //: 1
  	fInheritCallback,                                 //: 1
  	fInheritCallbackNumber,                           //: 1
  	fInheritShadow,                                   //: 1
  	fInheritMaxSessionTime,                           //: 1
  	fInheritMaxDisconnectionTime,                     //: 1
  	fInheritMaxIdleTime,                              //: 1
  	fInheritAutoClient,                               //: 1
  	fInheritSecurity,                                 //: 1
  	fUserConfigPromptForPassword,                     //: 1
  	fUserConfigResetBroken,                           //: 1
  	fUserConfigReconnectSame,                         //: 1
  	fLogonDisabled,                                   //: 1
  	fWallPaperDisabled,                               //: 1
  	fAutoClientDrives,                                //: 1
  	fAutoClientLpts,                                  //: 1
  	fUserConfigForceClientLptDef,                     //: 1
  	fRequireEncryption,                               //: 1
  	fDisableEncryption,                               //: 1
  	fUnused1,                                         //: 1
  	fHomeDirectoryMapRoot,                            //: 1
  	fUseDefaultGina,                                  //: 1
  	fCursorBlinkDisabled,                             //: 1
  	fPublishedApp,                                    //: 1
  	fHideTitleBar,                                    //: 1
  	fMaximize,                                        //: 1
  	fUserConfigDisableCpm,                            //: 1
  	fUserConfigDisableCdm,                            //: 1
  	fUserConfigDisableCcm,                            //: 1
  	fUserConfigDisableLPT,                            //: 1
  	fUserConfigDisableClip,                           //: 1
  	fDisableExe,                                      //: 1
  	fUserConfigfDisableCam,                           //: 1
  	fUserConfigDisableAutoReconnect,                  //: 1
  	UserConfigColorDepth0,                            //: 3
  	UserConfigColorDepth1,
  	UserConfigColorDepth2,
  	fInheritColorDepth,                               //: 1
  	fUserConfigErrorInvalidProfile,                   //: 1
  	fPasswordIsScPin,                                 //: 1
  	fDisablePNPRedir,                                 //: 1
{$IFDEF DELPHI6_UP}
    _TUserConfigFlagsAlign = al64Bit
{$ELSE}
    _TUserConfigFlagsAlign1,
    _TUserConfigFlagsAlign2,
    _TUserConfigFlagsAlign3,
    _TUserConfigFlagsAlign4,
    _TUserConfigFlagsAlign5,
    _TUserConfigFlagsAlign6,
    _TUserConfigFlagsAlign7,
    _TUserConfigFlagsAlign8,
    _TUserConfigFlagsAlign9,
    _TUserConfigFlagsAlign10,
    _TUserConfigFlagsAlign11,
    _TUserConfigFlagsAlign12,
    _TUserConfigFlagsAlign13,
    _TUserConfigFlagsAlign14,
    _TUserConfigFlagsAlign15,
    _TUserConfigFlagsAlign16,
    _TUserConfigFlagsAlign17,
    _TUserConfigFlagsAlign18,
    _TUserConfigFlagsAlign19,
    _TUserConfigFlagsAlign20,
    _TUserConfigFlagsAlign21,
    _TUserConfigFlagsAlign22,
    _TUserConfigFlagsAlign23,
    _TUserConfigFlagsAlign24,
    _TUserConfigFlagsAlign25,
    _TUserConfigFlagsAlign26,
    _TUserConfigFlagsAlign27,
    _TUserConfigFlagsAlign28,
    _TUserConfigFlagsAlign29,
    _TUserConfigFlagsAlign30,
    _TUserConfigFlagsAlign31,
    _TUserConfigFlagsAlign32,
    _TUserConfigFlagsAlign33,
    _TUserConfigFlagsAlign34,
    _TUserConfigFlagsAlign35,
    _TUserConfigFlagsAlign36,
    _TUserConfigFlagsAlign37,
    _TUserConfigFlagsAlign38,
    _TUserConfigFlagsAlign39,
    _TUserConfigFlagsAlign40,
    _TUserConfigFlagsAlign41,
    _TUserConfigFlagsAlign42,
    _TUserConfigFlagsAlign43,
    _TUserConfigFlagsAlign44,
    _TUserConfigFlagsAlign45,
    _TUserConfigFlagsAlign46,
    _TUserConfigFlagsAlign47,
    _TUserConfigFlagsAlign48,
    _TUserConfigFlagsAlign49,
    _TUserConfigFlagsAlign50,
    _TUserConfigFlagsAlign51,
    _TUserConfigFlagsAlign52,
    _TUserConfigFlagsAlign53,
    _TUserConfigFlagsAlign54,
    _TUserConfigFlagsAlign55,
    _TUserConfigFlagsAlign56,
    _TUserConfigFlagsAlign57,
    _TUserConfigFlagsAlign58,
    _TUserConfigFlagsAlign59,
    _TUserConfigFlagsAlign60,
    _TUserConfigFlagsAlign61,
    _TUserConfigFlagsAlign62,
    _TUserConfigFlagsAlign63,
    _TUserConfigFlagsAlign64
{$ENDIF}
  );

  _USERCONFIGW = record
    Flags: TUserConfigFlags;
    UserName: Array[0..USERNAME_LENGTH] of WCHAR;
    Domain: Array[0..DOMAIN_LENGTH] of WCHAR;
    Password: Array[0..PASSWORD_LENGTH] of WCHAR;
    WorkDirectory: Array[0..DIRECTORY_LENGTH] of WCHAR;
    InitialProgram: Array[0..INITIALPROGRAM_LENGTH] of WCHAR;
    CallbackNumber: Array[0..CALLBACK_LENGTH] of WCHAR;
    Callback: CALLBACKCLASS;
    Shadow: SHADOWCLASS;
    MaxConnectionTime: ULONG;
    MaxDisconnectionTime: ULONG;
    MaxIdleTime: ULONG;
    KeyboardLayout: ULONG;
    MinEncryptionLevel: BYTE;
    NWLogonServer: Array[0..NASIFILESERVER_LENGTH] of WCHAR;
    PublishedName: APPLICATIONNAMEW;
    WFProfilePath: Array[0..DIRECTORY_LENGTH] of WCHAR;
    WFHomeDir: Array[0..DIRECTORY_LENGTH] of WCHAR;
    WFHomeDirDrive: Array[0..3] of WCHAR;
    _Align: DWORD;
  end {_USERCONFIGW};
  USERCONFIGW = _USERCONFIGW;
  PUSERCONFIGW = ^_USERCONFIGW;
  TUserConfigW = _USERCONFIGW;

type
  _USERCONFIGA = record
    Flags: TUserConfigFlags;
    UserName: Array[0..USERNAME_LENGTH] of AnsiChar;
    Domain: Array[0..DOMAIN_LENGTH] of AnsiChar;
    Password: Array[0..PASSWORD_LENGTH] of AnsiChar;
    WorkDirectory: Array[0..DIRECTORY_LENGTH] of AnsiChar;
    InitialProgram: Array[0..INITIALPROGRAM_LENGTH] of AnsiChar;
    CallbackNumber: Array[0..CALLBACK_LENGTH] of AnsiChar;
    Callback: CALLBACKCLASS;
    Shadow: SHADOWCLASS;
    MaxConnectionTime: ULONG;
    MaxDisconnectionTime: ULONG;
    MaxIdleTime: ULONG;
    KeyboardLayout: ULONG;
    MinEncryptionLevel: BYTE;
    NWLogonServer: Array[0..NASIFILESERVER_LENGTH] of AnsiChar;
    PublishedName: APPLICATIONNAMEA;
    WFProfilePath: Array[0..DIRECTORY_LENGTH] of AnsiChar;
    WFHomeDir: Array[0..DIRECTORY_LENGTH] of AnsiChar;
    WFHomeDirDrive: Array[0..3] of AnsiChar;
  end {_USERCONFIGA};
  USERCONFIGA = _USERCONFIGA;
  PUSERCONFIGA = ^_USERCONFIGA;
  TUserConfigA = _USERCONFIGA;

{$IFDEF UNICODE}
type
  USERCONFIG = USERCONFIGW;
  PUSERCONFIG = PUSERCONFIGW;
{$ELSE}
type
  USERCONFIG = USERCONFIGA;
  PUSERCONFIG = PUSERCONFIGA;
{$ENDIF /* UNICODE*/}

{+//***************** }
{-* PD structures }
{=***************** }

type
  _PDCONFIG2W = record
    PdName: PDNAMEW;
    SdClass: SDCLASS;
    PdDLL: DLLNAMEW;
    PdFlag: ULONG;
    OutBufLength: ULONG;
    OutBufCount: ULONG;
    OutBufDelay: ULONG;
    InteractiveDelay: ULONG;
    PortNumber: ULONG;
    KeepAliveTimeout: ULONG;
  end {_PDCONFIG2W};
  PDCONFIG2W = _PDCONFIG2W;
  PPDCONFIG2W = ^_PDCONFIG2W;

type
  _PDCONFIG2A = record
    PdName: PDNAMEA;
    SdClass: SDCLASS;
    PdDLL: DLLNAMEA;
    PdFlag: ULONG;
    OutBufLength: ULONG;
    OutBufCount: ULONG;
    OutBufDelay: ULONG;
    InteractiveDelay: ULONG;
    PortNumber: ULONG;
    KeepAliveTimeout: ULONG;
  end {_PDCONFIG2A};
  PDCONFIG2A = _PDCONFIG2A;
  PPDCONFIG2A = ^_PDCONFIG2A;

{+// }
{-* PdFlag defines }
{= }

const
  PD_UNUSED = $00000001;
  PD_RELIABLE = $00000002;
  PD_FRAME = $00000004;
  PD_CONNECTION = $00000008;
  PD_CONSOLE = $00000010;
  PD_LANA = $00000020;
  PD_TRANSPORT = $00000040;
  PD_SINGLE_INST = $00000080;
  PD_NOLOW_WATERMARK = $00000100;
{$IFDEF UNICODE}
type
  PDCONFIG2 = PDCONFIG2W;
  PPDCONFIG2 = PPDCONFIG2W;
{$ELSE}
type
  PDCONFIG2 = PDCONFIG2A;
  PPDCONFIG2 = PPDCONFIG2A;
{$ENDIF /* UNICODE*/}

{+//------------------------------------------------*/ }
type
  _RECEIVEFLOWCONTROLCLASS = (
    ReceiveFlowControl_None,
    ReceiveFlowControl_RTS,
    ReceiveFlowControl_DTR );
  RECEIVEFLOWCONTROLCLASS = _RECEIVEFLOWCONTROLCLASS;
  TReceiveFlowControlClass = _RECEIVEFLOWCONTROLCLASS;

type
  _TRANSMITFLOWCONTROLCLASS = (
    TransmitFlowControl_None,
    TransmitFlowControl_CTS,
    TransmitFlowControl_DSR );
  TRANSMITFLOWCONTROLCLASS = _TRANSMITFLOWCONTROLCLASS;
  TTransmitFlowControlClass = _TRANSMITFLOWCONTROLCLASS;

type
  _FLOWCONTROLCONFIG = record
    FlowControlFlags: Set Of (
  	  fEnableSoftwareTx,                                 //: 1
  	  fEnableSoftwareRx,                                 //: 1
  	  fEnableDTR,                                        //: 1
  	  fEnableRTS,                                        //: 1
{$IFDEF DELPHI6_UP}
      _FlowControlFlagsAlign = al32Bit
{$ELSE}
      _FlowControlFlagsAlign1,
      _FlowControlFlagsAlign2,
      _FlowControlFlagsAlign3,
      _FlowControlFlagsAlign4,
      _FlowControlFlagsAlign5,
      _FlowControlFlagsAlign6,
      _FlowControlFlagsAlign7,
      _FlowControlFlagsAlign8,
      _FlowControlFlagsAlign9,
      _FlowControlFlagsAlign10,
      _FlowControlFlagsAlign11,
      _FlowControlFlagsAlign12,
      _FlowControlFlagsAlign13,
      _FlowControlFlagsAlign14,
      _FlowControlFlagsAlign15,
      _FlowControlFlagsAlign16,
      _FlowControlFlagsAlign17,
      _FlowControlFlagsAlign18,
      _FlowControlFlagsAlign19,
      _FlowControlFlagsAlign20,
      _FlowControlFlagsAlign21,
      _FlowControlFlagsAlign22,
      _FlowControlFlagsAlign23,
      _FlowControlFlagsAlign24,
      _FlowControlFlagsAlign25,
      _FlowControlFlagsAlign26,
      _FlowControlFlagsAlign27,
      _FlowControlFlagsAlign28,
      _FlowControlFlagsAlign29,
      _FlowControlFlagsAlign30,
      _FlowControlFlagsAlign31,
      _FlowControlFlagsAlign32
{$ENDIF}
    );
    XonChar: AnsiChar;
    XoffChar: AnsiChar;
    Type_: FLOWCONTROLCLASS;
    HardwareReceive: RECEIVEFLOWCONTROLCLASS;
    HardwareTransmit: TRANSMITFLOWCONTROLCLASS;
  end {_FLOWCONTROLCONFIG};
  FLOWCONTROLCONFIG = _FLOWCONTROLCONFIG;
  PFLOWCONTROLCONFIG = ^_FLOWCONTROLCONFIG;
  TFlowControlConfig = _FLOWCONTROLCONFIG;

type
  _ASYNCCONNECTCLASS = (
    Connect_CTS,
    Connect_DSR,
    Connect_RI,
    Connect_DCD,
    Connect_FirstChar,
    Connect_Perm );
  ASYNCCONNECTCLASS = _ASYNCCONNECTCLASS;
  TAsyncConnectClass = _ASYNCCONNECTCLASS;

type
  _CONNECTCONFIG = record
    Type_: ASYNCCONNECTCLASS;
    ConnectConfigFlags: Set Of (
      fEnableBreakDisconnect,
{$IFDEF DELPHI6}      
      _ConnectConfigFlagsAlign = al32Bit
{$ELSE}
    _ConnectConfigFlagsAlign1,
    _ConnectConfigFlagsAlign2,
    _ConnectConfigFlagsAlign3,
    _ConnectConfigFlagsAlign4,
    _ConnectConfigFlagsAlign5,
    _ConnectConfigFlagsAlign6,
    _ConnectConfigFlagsAlign7,
    _ConnectConfigFlagsAlign8,
    _ConnectConfigFlagsAlign9,
    _ConnectConfigFlagsAlign10,
    _ConnectConfigFlagsAlign11,
    _ConnectConfigFlagsAlign12,
    _ConnectConfigFlagsAlign13,
    _ConnectConfigFlagsAlign14,
    _ConnectConfigFlagsAlign15,
    _ConnectConfigFlagsAlign16,
    _ConnectConfigFlagsAlign17,
    _ConnectConfigFlagsAlign18,
    _ConnectConfigFlagsAlign19,
    _ConnectConfigFlagsAlign20,
    _ConnectConfigFlagsAlign21,
    _ConnectConfigFlagsAlign22,
    _ConnectConfigFlagsAlign23,
    _ConnectConfigFlagsAlign24,
    _ConnectConfigFlagsAlign25,
    _ConnectConfigFlagsAlign26,
    _ConnectConfigFlagsAlign27,
    _ConnectConfigFlagsAlign28,
    _ConnectConfigFlagsAlign29,
    _ConnectConfigFlagsAlign30,
    _ConnectConfigFlagsAlign31,
    _ConnectConfigFlagsAlign32
{$ENDIF}
    );
  end {_CONNECTCONFIG};
  CONNECTCONFIG = _CONNECTCONFIG;
  PCONNECTCONFIG = ^_CONNECTCONFIG;
  TConnectConfig = _CONNECTCONFIG;
{+//------------------------------------------------*/ }

type
    TASyncConfigFlags = Set of (
      fEnableDsrSensitivity,
      fConnectionDriver,
{$IFDEF DELPHI6_UP}
      _TASyncConfigFlagsAlign = al32Bit
{$ELSE}
      _TASyncConfigFlagsAlign1,
      _TASyncConfigFlagsAlign2,
      _TASyncConfigFlagsAlign3,
      _TASyncConfigFlagsAlign4,
      _TASyncConfigFlagsAlign5,
      _TASyncConfigFlagsAlign6,
      _TASyncConfigFlagsAlign7,
      _TASyncConfigFlagsAlign8,
      _TASyncConfigFlagsAlign9,
      _TASyncConfigFlagsAlign10,
      _TASyncConfigFlagsAlign11,
      _TASyncConfigFlagsAlign12,
      _TASyncConfigFlagsAlign13,
      _TASyncConfigFlagsAlign14,
      _TASyncConfigFlagsAlign15,
      _TASyncConfigFlagsAlign16,
      _TASyncConfigFlagsAlign17,
      _TASyncConfigFlagsAlign18,
      _TASyncConfigFlagsAlign19,
      _TASyncConfigFlagsAlign20,
      _TASyncConfigFlagsAlign21,
      _TASyncConfigFlagsAlign22,
      _TASyncConfigFlagsAlign23,
      _TASyncConfigFlagsAlign24,
      _TASyncConfigFlagsAlign25,
      _TASyncConfigFlagsAlign26,
      _TASyncConfigFlagsAlign27,
      _TASyncConfigFlagsAlign28,
      _TASyncConfigFlagsAlign29,
      _TASyncConfigFlagsAlign30,
      _TASyncConfigFlagsAlign31,
      _TASyncConfigFlagsAlign32,
      _TASyncConfigFlagsAlign33,
      _TASyncConfigFlagsAlign34,
      _TASyncConfigFlagsAlign35,
      _TASyncConfigFlagsAlign36,
      _TASyncConfigFlagsAlign37,
      _TASyncConfigFlagsAlign38,
      _TASyncConfigFlagsAlign39,
      _TASyncConfigFlagsAlign40,
      _TASyncConfigFlagsAlign41,
      _TASyncConfigFlagsAlign42,
      _TASyncConfigFlagsAlign43,
      _TASyncConfigFlagsAlign44,
      _TASyncConfigFlagsAlign45,
      _TASyncConfigFlagsAlign46,
      _TASyncConfigFlagsAlign47,
      _TASyncConfigFlagsAlign48,
      _TASyncConfigFlagsAlign49,
      _TASyncConfigFlagsAlign50,
      _TASyncConfigFlagsAlign51,
      _TASyncConfigFlagsAlign52,
      _TASyncConfigFlagsAlign53,
      _TASyncConfigFlagsAlign54,
      _TASyncConfigFlagsAlign55,
      _TASyncConfigFlagsAlign56,
      _TASyncConfigFlagsAlign57,
      _TASyncConfigFlagsAlign58,
      _TASyncConfigFlagsAlign59,
      _TASyncConfigFlagsAlign60,
      _TASyncConfigFlagsAlign61,
      _TASyncConfigFlagsAlign62,
      _TASyncConfigFlagsAlign63,
      _TASyncConfigFlagsAlign64
{$ENDIF}
    );

  _ASYNCCONFIGW = record
    DeviceName: DEVICENAMEW;
    ModemName: MODEMNAMEW;
    BaudRate: ULONG;
    Parity: ULONG;
    StopBits: ULONG;
    ByteSize: ULONG;
    ASyncConfigFlags: TASyncConfigFlags;
    FlowControl: FLOWCONTROLCONFIG;
    Connect: CONNECTCONFIG;
  end {_ASYNCCONFIGW};
  ASYNCCONFIGW = _ASYNCCONFIGW;
  PASYNCCONFIGW = ^_ASYNCCONFIGW;
  TAsyncConfigW = _ASYNCCONFIGW;

type
  _ASYNCCONFIGA = record
    DeviceName: DEVICENAMEA;
    ModemName: MODEMNAMEA;
    BaudRate: ULONG;
    Parity: ULONG;
    StopBits: ULONG;
    ByteSize: ULONG;
    ASyncConfigFlags: TASyncConfigFlags;
    FlowControl: FLOWCONTROLCONFIG;
    Connect: CONNECTCONFIG;
  end {_ASYNCCONFIGA};
  ASYNCCONFIGA = _ASYNCCONFIGA;
  PASYNCCONFIGA = ^_ASYNCCONFIGA;
  TAsyncConfigA = _ASYNCCONFIGA;

{$IFDEF UNICODE}
type
  ASYNCCONFIG = ASYNCCONFIGW;
  PASYNCCONFIG = PASYNCCONFIGW;
{$ELSE}
type
  ASYNCCONFIG = ASYNCCONFIGA;
  PASYNCCONFIG = PASYNCCONFIGA;
{$ENDIF /* UNICODE*/}

{+//------------------------------------------------*/ }

type
  _NETWORKCONFIGW = record
    LanAdapter: LongInt;
    NetworkName: DEVICENAMEW;
    Flags: ULONG;
  end {_NETWORKCONFIGW};
  NETWORKCONFIGW = _NETWORKCONFIGW;
  PNETWORKCONFIGW = ^_NETWORKCONFIGW;
  TNetworkConfigW = _NETWORKCONFIGW;

type
  _NETWORKCONFIGA = record
    LanAdapter: LongInt;
    NetworkName: DEVICENAMEA;
    Flags: ULONG;
  end {_NETWORKCONFIGA};
  NETWORKCONFIGA = _NETWORKCONFIGA;
  PNETWORKCONFIGA = ^_NETWORKCONFIGA;
  TNetworkConfigA = _NETWORKCONFIGA;

const
  NETWORK_CLIENT = $00000001;
{$IFDEF UNICODE}
type
  NETWORKCONFIG = NETWORKCONFIGW;
  PNETWORKCONFIG = PNETWORKCONFIGW;
{$ELSE}
type
  NETWORKCONFIG = NETWORKCONFIGA;
  PNETWORKCONFIG = PNETWORKCONFIGA;
{$ENDIF /* UNICODE*/}
{+//------------------------------------------------*/ }
type
  _NASICONFIGW = record
    SpecificName: NASISPECIFICNAMEW;
    UserName: NASIUSERNAMEW;
    PassWord: NASIPASSWORDW;
    SessionName: NASISESSIONNAMEW;   // Error in winsta.h NASISESIONNAMEW 
    FileServer: NASIFILESERVERW;
    GlobalSession: BOOLEAN;
  end {_NASICONFIGW};
  NASICONFIGW = _NASICONFIGW;
  PNASICONFIGW = ^_NASICONFIGW;

type
  _NASICONFIGA = record
    SpecificName: NASISPECIFICNAMEA;
    UserName: NASIUSERNAMEA;
    PassWord: NASIPASSWORDA;
    SessionName: NASISESSIONNAMEA;  // Error in winsta.h NASISESIONNAMEW
    FileServer: NASIFILESERVERA;
    GlobalSession: BOOLEAN;
  end {_NASICONFIGA};
  NASICONFIGA = _NASICONFIGA;
  PNASICONFIGA = ^_NASICONFIGA;

{$IFDEF UNICODE}
type
  NASICONFIG = NASICONFIGW;
  PNASICONFIG = PNASICONFIGW;
{$ELSE}
type
  NASICONFIG = NASICONFIGA;
  PNASICONFIG = PNASICONFIGA;
{$ENDIF /* UNICODE*/}

{+//------------------------------------------------*/ }

type
  _OEMTDCONFIGW = record
    Adapter: LongInt;
    DeviceName: DEVICENAMEW;
    Flags: ULONG;
  end {_OEMTDCONFIGW};
  OEMTDCONFIGW = _OEMTDCONFIGW;
  POEMTDCONFIGW = ^_OEMTDCONFIGW;

type
  _OEMTDCONFIGA = record
    Adapter: LongInt;
    DeviceName: DEVICENAMEA;
    Flags: ULONG;
  end {_OEMTDCONFIGA};
  OEMTDCONFIGA = _OEMTDCONFIGA;
  POEMTDCONFIGA = ^_OEMTDCONFIGA;

{$IFDEF UNICODE}
type
  OEMTDCONFIG = OEMTDCONFIGW;
  POEMTDCONFIG = POEMTDCONFIGW;
{$ELSE}
type
  OEMTDCONFIG = OEMTDCONFIGA;
  POEMTDCONFIG = POEMTDCONFIGA;
{$ENDIF /* UNICODE*/}

{+//------------------------------------------------*/ }

type
  _PDPARAMSW = record
    SdClass: SDCLASS;
    case integer of
      1: (Network: NETWORKCONFIGW);
      2: (Async: ASYNCCONFIGW);
      3: (Nasi: NASICONFIGW);
      4: (OemTd: OEMTDCONFIGW);
  end {_PDPARAMSW};
  PDPARAMSW = _PDPARAMSW;
  PPDPARAMSW = ^_PDPARAMSW;

type
  _PDPARAMSA = record
    SdClass: SDCLASS;
    case integer of
      1: (Network: NETWORKCONFIGA);
      2: (Async: ASYNCCONFIGA);
      3: (Nasi: NASICONFIGA);
      4: (OemTd: OEMTDCONFIGA);
  end {_PDPARAMSA};
  PDPARAMSA = _PDPARAMSA;
  PPDPARAMSA = ^_PDPARAMSA;

{$IFDEF UNICODE}
type
  PDPARAMS = PDPARAMSW;
  PPDPARAMS = PPDPARAMSW;
{$ELSE}
type
  PDPARAMS = PDPARAMSA;
  PPDPARAMS = PPDPARAMSA;
{$ENDIF /* UNICODE*/}

{+//------------------------------------------------*/ }

type
  _PDCONFIGW = record
    Create_: PDCONFIG2W;
    Params: PDPARAMSW;
  end {_PDCONFIGW};
  PDCONFIGW = _PDCONFIGW;
  PPDCONFIGW = ^_PDCONFIGW;

type
  _PDCONFIGA = record
    Create_: PDCONFIG2A;
    Params: PDPARAMSA;
  end {_PDCONFIGA};
  PDCONFIGA = _PDCONFIGA;
  PPDCONFIGA = ^_PDCONFIGA;

{$IFDEF UNICODE}
type
  PDCONFIG = PDCONFIGW;
  PPDCONFIG = PPDCONFIGW;
{$ELSE}
type
  PDCONFIG = PDCONFIGA;
  PPDCONFIG = PPDCONFIGA;
{$ENDIF /* UNICODE*/}

{+//********************** }
{-* Wd structures }
{=********************** }

type
  _WDCONFIGW = record
    WdName: WDNAMEW;
    WdDLL: DLLNAMEW;
    WsxDLL: DLLNAMEW;
    WdFlag: ULONG;
    WdInputBufferLength: ULONG;
    CfgDLL: DLLNAMEW;
    WdPrefix: WDPREFIXW;
  end {_WDCONFIGW};
  WDCONFIGW = _WDCONFIGW;
  PWDCONFIGW = ^_WDCONFIGW;

type
  _WDCONFIGA = record
    WdName: WDNAMEA;
    WdDLL: DLLNAMEA;
    WsxDLL: DLLNAMEA;
    WdFlag: ULONG;
    WdInputBufferLength: ULONG;
    CfgDLL: DLLNAMEA;
    WdPrefix: WDPREFIXA;
  end {_WDCONFIGA};
  WDCONFIGA = _WDCONFIGA;
  PWDCONFIGA = ^_WDCONFIGA;

{+// }
{-* WdFlag defines }
{= }

const
  WDF_UNUSED = $00000001;
  WDF_SHADOW_SOURCE = $00000002;
  WDF_SHADOW_TARGET = $00000004;
  WDF_OTHER = $00000008;
  WDF_TSHARE = $00000010;
  WDF_DYNAMIC_RECONNECT = $00000020;
  WDF_USER_VCIOCTL = $00000040;
  WDF_SUBDESKTOP = $00008000;
{$IFDEF UNICODE}
type
  WDCONFIG = WDCONFIGW;
  PWDCONFIG = PWDCONFIGW;
{$ELSE}
type
  WDCONFIG = WDCONFIGA;
  PWDCONFIG = PWDCONFIGA;
{$ENDIF /* UNICODE*/}

{+//************************************* }
{-* Connection Driver structures (CD) }
{=************************************* }

{+// }
{-* connection driver classes }
{= }

type
  _CDCLASS = (
    CdNone,
    CdModem,
    CdClass_Maximum );
  CDCLASS = _CDCLASS;
  TCDClass = _CDCLASS;

{+//------------------------------------------------*/ }

type
  _CDCONFIGW = record
    CdClass: CDCLASS;
    CdName: CDNAMEW;
    CdDLL: DLLNAMEW;
    CdFlag: ULONG;
  end {_CDCONFIGW};
  CDCONFIGW = _CDCONFIGW;
  PCDCONFIGW = ^_CDCONFIGW;

type
  _CDCONFIGA = record
    CdClass: CDCLASS;
    CdName: CDNAMEA;
    CdDLL: DLLNAMEA;
    CdFlag: ULONG;
  end {_CDCONFIGA};
  CDCONFIGA = _CDCONFIGA;
  PCDCONFIGA = ^_CDCONFIGA;

{$IFDEF UNICODE}
type
  CDCONFIG = CDCONFIGW;
  PCDCONFIG = PCDCONFIGW;
{$ELSE}
type
  CDCONFIG = CDCONFIGA;
  PCDCONFIG = PCDCONFIGA;
{$ENDIF /* UNICODE*/}

{+//**************************** }
{-* Window Station structures }
{=**************************** }

type
  TWinStationCreateFlags = Set Of (
    fEnableWinStation,
{$IFDEF DELPHI6_UP}
    _TWinStationCreateFlagsAlign = al32Bit
{$ELSE}
    _TWinStationCreateFlagsAlign1,
    _TWinStationCreateFlagsAlign2,
    _TWinStationCreateFlagsAlign3,
    _TWinStationCreateFlagsAlign4,
    _TWinStationCreateFlagsAlign5,
    _TWinStationCreateFlagsAlign6,
    _TWinStationCreateFlagsAlign7,
    _TWinStationCreateFlagsAlign8,
    _TWinStationCreateFlagsAlign9,
    _TWinStationCreateFlagsAlign10,
    _TWinStationCreateFlagsAlign11,
    _TWinStationCreateFlagsAlign12,
    _TWinStationCreateFlagsAlign13,
    _TWinStationCreateFlagsAlign14,
    _TWinStationCreateFlagsAlign15,
    _TWinStationCreateFlagsAlign16,
    _TWinStationCreateFlagsAlign17,
    _TWinStationCreateFlagsAlign18,
    _TWinStationCreateFlagsAlign19,
    _TWinStationCreateFlagsAlign20,
    _TWinStationCreateFlagsAlign21,
    _TWinStationCreateFlagsAlign22,
    _TWinStationCreateFlagsAlign23,
    _TWinStationCreateFlagsAlign24,
    _TWinStationCreateFlagsAlign25,
    _TWinStationCreateFlagsAlign26,
    _TWinStationCreateFlagsAlign27,
    _TWinStationCreateFlagsAlign28,
    _TWinStationCreateFlagsAlign29,
    _TWinStationCreateFlagsAlign30,
    _TWinStationCreateFlagsAlign31,
    _TWinStationCreateFlagsAlign32

{$ENDIF}
  );

  _WINSTATIONCREATEW = record
    WinStationCreateFlags: TWinStationCreateFlags;
    MaxInstanceCount: ULONG;
  end {_WINSTATIONCREATEW};
  WINSTATIONCREATEW = _WINSTATIONCREATEW;
  PWINSTATIONCREATEW = ^_WINSTATIONCREATEW;

type
  _WINSTATIONCREATEA = record
    WinStationCreateFlags: TWinStationCreateFlags;
    MaxInstanceCount: ULONG;
  end {_WINSTATIONCREATEA};
  WINSTATIONCREATEA = _WINSTATIONCREATEA;
  PWINSTATIONCREATEA = ^_WINSTATIONCREATEA;

{$IFDEF UNICODE}
type
  WINSTATIONCREATE = WINSTATIONCREATEW;
  PWINSTATIONCREATE = PWINSTATIONCREATEW;
{$ELSE}
type
  WINSTATIONCREATE = WINSTATIONCREATEA;
  PWINSTATIONCREATE = PWINSTATIONCREATEA;
{$ENDIF /* UNICODE*/}

{+//------------------------------------------------*/ }

type
  _WINSTATIONCONFIGW = record
    Comment: Array[0..WINSTATIONCOMMENT_LENGTH] of WCHAR;
    User: USERCONFIGW;
    OEMId: Array[0..3] of AnsiChar;
  end {_WINSTATIONCONFIGW};
  WINSTATIONCONFIGW = _WINSTATIONCONFIGW;
  PWINSTATIONCONFIGW = ^_WINSTATIONCONFIGW;

type
  _WINSTATIONCONFIGA = record
    Comment: Array[0..WINSTATIONCOMMENT_LENGTH] of AnsiChar;
    User: USERCONFIGA;
    OEMId: Array[0..3] of AnsiChar;
  end {_WINSTATIONCONFIGA};
  WINSTATIONCONFIGA = _WINSTATIONCONFIGA;
  PWINSTATIONCONFIGA = ^_WINSTATIONCONFIGA;

{$IFDEF UNICODE}
type
  WINSTATIONCONFIG = WINSTATIONCONFIGW;
  PWINSTATIONCONFIG = PWINSTATIONCONFIGW;
{$ELSE}
type
  WINSTATIONCONFIG = WINSTATIONCONFIGA;
  PWINSTATIONCONFIG = PWINSTATIONCONFIGA;
{$ENDIF /* UNICODE*/}

{+//------------------------------------------------*/ }

const
  EXECSRVPIPENAMELEN = 48;

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
{+//------------------------------------------------*/ }

type
  TData = record
  end;

type
  _WINSTATIONCLIENTDATA = record
    DataName: CLIENTDATANAME;
    fUnicodeData: BOOLEAN;
    Data: TData;   {+// BYTE Data[1]; Variable length data follows*/ }
  end {_WINSTATIONCLIENTDATA};
  TWinStationClientData = _WINSTATIONCLIENTDATA;
  PWINSTATIONCLIENTDATA = ^_WINSTATIONCLIENTDATA;

{+//------------------------------------------------*/ }

type
  _WINSTATIONUSERTOKEN = record
    ProcessId: THandle;
    ThreadId: THandle;
    UserToken: THandle;
  end {_WINSTATIONUSERTOKEN};
  TWinStationUserToken = _WINSTATIONUSERTOKEN;
  PWINSTATIONUSERTOKEN = ^_WINSTATIONUSERTOKEN;

{+//------------------------------------------------*/ }

type
  _WINSTATIONVIDEODATA = record
    HResolution: USHORT;
    VResolution: USHORT;
    fColorDepth: USHORT;
  end {_WINSTATIONVIDEODATA};
  TWinStationVideoData = _WINSTATIONVIDEODATA;
  PWINSTATIONVIDEODATA = ^_WINSTATIONVIDEODATA;

{+//----------------------------------------------*/ }

type
  _WINSTATIONCONFIG2W = record
    Create: WINSTATIONCREATEW;
    Pd: Array[0..MAX_PDCONFIG-1] of PDCONFIGW;
    Wd: WDCONFIGW;
    Cd: CDCONFIGW;
    Config: WINSTATIONCONFIGW;
  end {_WINSTATIONCONFIG2W};
  WINSTATIONCONFIG2W = _WINSTATIONCONFIG2W;
  PWINSTATIONCONFIG2W = ^_WINSTATIONCONFIG2W;

type
  _WINSTATIONCONFIG2A = record
    Create: WINSTATIONCREATEA;
    Pd: Array[0..MAX_PDCONFIG-1] of PDCONFIGA;
    Wd: WDCONFIGA;
    Cd: CDCONFIGA;
    Config: WINSTATIONCONFIGA;
  end {_WINSTATIONCONFIG2A};
  WINSTATIONCONFIG2A = _WINSTATIONCONFIG2A;
  PWINSTATIONCONFIG2A = ^_WINSTATIONCONFIG2A;

{$IFDEF UNICODE}
type
  WINSTATIONCONFIG2 = WINSTATIONCONFIG2W;
  PWINSTATIONCONFIG2 = PWINSTATIONCONFIG2W;
{$ELSE}
type
  WINSTATIONCONFIG2 = WINSTATIONCONFIG2A;
  PWINSTATIONCONFIG2 = PWINSTATIONCONFIG2A;
{$ENDIF /* UNICODE*/}

{+// }
{-* WinStation client data structure }
{= }
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

{+// }
{-* WinStation client data structure }
{= }

type
  _WINSTATIONCLIENTA = record
    WinStationClientFlags: TWinStationClientFlags;
    ClientName: Array[0..CLIENTNAME_LENGTH] of AnsiChar;
    Domain: Array[0..DOMAIN_LENGTH] of AnsiChar;
    UserName: Array[0..USERNAME_LENGTH] of AnsiChar;
    Password: Array[0..PASSWORD_LENGTH] of AnsiChar;
    WorkDirectory: Array[0..DIRECTORY_LENGTH] of AnsiChar;
    InitialProgram: Array[0..INITIALPROGRAM_LENGTH] of AnsiChar;
    SerialNumber: ULONG;
    EncryptionLevel: BYTE;
    ClientAddressFamily: ULONG;
    ClientAddress: Array[0..CLIENTADDRESS_LENGTH] of AnsiChar;
    HRes: USHORT;
    VRes: USHORT;
    ColorDepth: USHORT;
    ProtocolType: USHORT;
    KeyboardLayout: ULONG;
    KeyboardType: ULONG;
    KeyboardSubType: ULONG;
    KeyboardFunctionKey: ULONG;
    imeFileName: Array[0..IMEFILENAME_LENGTH] of AnsiChar;
    ClientDirectory: Array[0..DIRECTORY_LENGTH] of AnsiChar;
    ClientLicense: Array[0..CLIENTLICENSE_LENGTH] of AnsiChar;
    ClientModem: Array[0..CLIENTMODEM_LENGTH] of AnsiChar;
    ClientBuildNumber: ULONG;
    ClientHardwareId: ULONG;
    ClientProductId: USHORT;
    OutBufCountHost: USHORT;
    OutBufCountClient: USHORT;
    OutBufLength: USHORT;
    AudioDriverName: Array[0..8] of AnsiChar;
    ClientTimeZone: TS_TIME_ZONE_INFORMATION;
    ClientSessionId: ULONG;
    clientDigProductId: Array[0..CLIENT_PRODUCT_ID_LENGTH-1] of AnsiChar;
    PerformanceFlags: ULONG;
    ActiveInputLocale: ULONG;
  end {_WINSTATIONCLIENTA};
  WINSTATIONCLIENTA = _WINSTATIONCLIENTA;
  PWINSTATIONCLIENTA = ^_WINSTATIONCLIENTA;

{$IFDEF UNICODE}
type
  WINSTATIONCLIENT_ = WINSTATIONCLIENTW;
  PWINSTATIONCLIENT = PWINSTATIONCLIENTW;
{$ELSE}
type
  WINSTATIONCLIENT_ = WINSTATIONCLIENTA;
  PWINSTATIONCLIENT = PWINSTATIONCLIENTA;
{$ENDIF /* UNICODE*/}

{+// }
{-* T.Share specific protocol performance counters }
{= }

type
  _TSHARE_COUNTERS = record
    Reserved: ULONG;
  end {_TSHARE_COUNTERS};
  TSHARE_COUNTERS = _TSHARE_COUNTERS;
  PTSHARE_COUNTERS = ^_TSHARE_COUNTERS;

{+// }
{-* WinStation protocol performance counters }
{= }

type
  _PROTOCOLCOUNTERS = record
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
    case Specific: Integer of
      1: (TShareCounters: TSHARE_COUNTERS);
      2: (Reserved: Array[0..100-1] of ULONG);
  end {_PROTOCOLCOUNTERS};
  PROTOCOLCOUNTERS = _PROTOCOLCOUNTERS;
  PPROTOCOLCOUNTERS = ^_PROTOCOLCOUNTERS;
  TProtocolCounters = _PROTOCOLCOUNTERS;

{+// }
{-* ThinWire cache statistics }
{= }

type
  _THINWIRECACHE = record
    CacheReads: ULONG;
    CacheHits: ULONG;
  end {_THINWIRECACHE};
  THINWIRECACHE = _THINWIRECACHE;
  PTHINWIRECACHE = ^_THINWIRECACHE;
const
  MAX_THINWIRECACHE = 4;


type
  _RESERVED_CACHE = record
    ThinWireCache: Array[0..MAX_THINWIRECACHE-1] of THINWIRECACHE;
  end {_RESERVED_CACHE};
  RESERVED_CACHE = _RESERVED_CACHE;
  PRESERVED_CACHE = ^_RESERVED_CACHE;

{+// }
{-* T.Share specific cache statistics }
{= }

type
  _TSHARE_CACHE = record
    Reserved: ULONG;
  end {_TSHARE_CACHE};
  TSHARE_CACHE = _TSHARE_CACHE;
  PTSHARE_CACHE = ^_TSHARE_CACHE;

{+// }
{-* WinStation cache statistics }
{= }

type
  CACHE_STATISTICS = record
    ProtocolType: USHORT;
    Length: USHORT;
        ReservedCacheStats: RESERVED_CACHE;
    TShareCacheStats: TSHARE_CACHE;
    Reserved: Array[0..20-1] of ULONG;
  end {CACHE_STATISTICS};

{+// }
{-* WinStation protocol status }
{= }

type
  _PROTOCOLSTATUS = record
    Output: PROTOCOLCOUNTERS;
    Input: PROTOCOLCOUNTERS;
    Cache: CACHE_STATISTICS;
    AsyncSignal: ULONG;
    AsyncSignalMask: ULONG;
  end {_PROTOCOLSTATUS};
  PROTOCOLSTATUS = _PROTOCOLSTATUS;
  PPROTOCOLSTATUS = ^_PROTOCOLSTATUS;

type
  _PROTOCOLSTATUSEX = record
    PROTOCOLSTATUS: _PROTOCOLSTATUS;
    Counters: Array[0..MAX_COUNTER_EXTENSIONS-1] of LARGE_INTEGER;
  end;
  PROTOCOLSTATUSEX = _PROTOCOLSTATUSEX;
  PPROTOCOLSTATUSEX = ^_PROTOCOLSTATUSEX;
  TProtocolStatusEx = _PROTOCOLSTATUSEX;

{+// }
{-* WinStation query information }
{= }

type
  _WINSTATIONINFORMATIONW = record
    ConnectState: WINSTATIONSTATECLASS;
    WinStationName: WINSTATIONNAMEW;
    LogonId: ULONG;
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

type
  _WINSTATIONINFORMATIONA = record
    ConnectState: WINSTATIONSTATECLASS;
    WinStationName: WINSTATIONNAMEA;
    LogonId: ULONG;
    ConnectTime: LARGE_INTEGER;
    DisconnectTime: LARGE_INTEGER;
    LastInputTime: LARGE_INTEGER;
    LogonTime: LARGE_INTEGER;
    Status: PROTOCOLSTATUS;
    Domain: Array[0..DOMAIN_LENGTH] of AnsiChar;
    UserName: Array[0..USERNAME_LENGTH] of AnsiChar;
    CurrentTime: LARGE_INTEGER;
  end {_WINSTATIONINFORMATIONA};
  WINSTATIONINFORMATIONA = _WINSTATIONINFORMATIONA;
  PWINSTATIONINFORMATIONA = ^_WINSTATIONINFORMATIONA;

{$IFDEF UNICODE}
type
  WINSTATIONINFORMATION_ = WINSTATIONINFORMATIONW;
  PWINSTATIONINFORMATION = PWINSTATIONINFORMATIONW;
{$ELSE}
type
  WINSTATIONINFORMATION_ = WINSTATIONINFORMATIONA;
  PWINSTATIONINFORMATION = PWINSTATIONINFORMATIONA;
{$ENDIF /* UNICODE*/}

{+// }
{-* Load balancing data types }
{= }

type
  _LOADFACTORTYPE = (
    ErrorConstraint,
    PagedPoolConstraint,
    NonPagedPoolConstraint,
    AvailablePagesConstraint,
    SystemPtesConstraint,
    CPUConstraint  );
  LOADFACTORTYPE = _LOADFACTORTYPE;
  TLoadFactorType = _LOADFACTORTYPE;

type
  _WINSTATIONLOADINDICATORDATA = record
    RemainingSessionCapacity: ULONG;
    LoadFactor: LOADFACTORTYPE;
    TotalSessions: ULONG;
    DisconnectedSessions: ULONG;
    IdleCPU: LARGE_INTEGER;
    TotalCPU: LARGE_INTEGER;
    RawSessionCapacity: ULONG;
    reserved: Array[0..9-1] of ULONG;
  end {_WINSTATIONLOADINDICATORDATA};
  WINSTATIONLOADINDICATORDATA = _WINSTATIONLOADINDICATORDATA;
  PWINSTATIONLOADINDICATORDATA = ^_WINSTATIONLOADINDICATORDATA;

{+// }
{-* WinStation shadow states }
{= }

type
  _SHADOWSTATECLASS = (
    State_NoShadow, 
    State_Shadowing,
    State_Shadowed );
  SHADOWSTATECLASS = _SHADOWSTATECLASS;
  TShadowStateClass = _SHADOWSTATECLASS;

{+// }
{-* Shadow query/set information }
{= }

type
  _WINSTATIONSHADOW = record
    ShadowState: SHADOWSTATECLASS;
    ShadowClass: SHADOWCLASS;
    SessionId: ULONG;
    ProtocolType: ULONG;
  end {_WINSTATIONSHADOW};
  WINSTATIONSHADOW = _WINSTATIONSHADOW;
  PWINSTATIONSHADOW = ^_WINSTATIONSHADOW;

type
  _WINSTATIONPRODIDW = record
    DigProductId: Array[0..CLIENT_PRODUCT_ID_LENGTH-1] of WCHAR;
    ClientDigProductId: Array[0..CLIENT_PRODUCT_ID_LENGTH-1] of WCHAR;
    OuterMostDigProductId: Array[0..CLIENT_PRODUCT_ID_LENGTH-1] of WCHAR;
    curentSessionId: ULONG;
    ClientSessionId: ULONG;
    OuterMostSessionId: ULONG;
  end {_WINSTATIONPRODIDW};
  WINSTATIONPRODIDW = _WINSTATIONPRODIDW;
  PWINSTATIONPRODIDW = ^_WINSTATIONPRODIDW;

type
  _WINSTATIONPRODIDA = record
    DigProductId: Array[0..CLIENT_PRODUCT_ID_LENGTH-1] of AnsiChar;
    ClientDigProductId: Array[0..CLIENT_PRODUCT_ID_LENGTH-1] of AnsiChar;
    OuterMostDigProductId: Array[0..CLIENT_PRODUCT_ID_LENGTH-1] of AnsiChar;
    curentSessionId: ULONG;
    ClientSessionId: ULONG;
    OuterMostSessionId: ULONG;
  end {_WINSTATIONPRODIDA};
  WINSTATIONPRODIDA = _WINSTATIONPRODIDA;
  PWINSTATIONPRODIDA = ^_WINSTATIONPRODIDA;

{$IFDEF UNICODE}
  WINSTATIONPRODID = WINSTATIONPRODIDW;
  PWINSTATIONPRODID = PWINSTATIONPRODIDW;
{$ELSE}
  WINSTATIONPRODID = WINSTATIONPRODIDA;
  PWINSTATIONPRODID = PWINSTATIONPRODIDA;
{$ENDIF /* UNICODE*/}
  _WINSTATION_REMOTE_ADDRESS = record
    AddressFamily: DWORD;
    Port: WORD;
    Address: array [0..19] of BYTE;
    Reserved: array[0..5] of BYTE;
  end;

  Tipv4 = record
    sin_family: Word;
    sin_port: USHORT;
    in_addr: ULONG;
    sin_zero: Array[0..7] of UCHAR;
  end {ipv4};

  Tipv6 = record
    sin6_port: USHORT;
    sin6_flowinfo: ULONG;
    sin6_addr: Array[0..7] of USHORT;
    sin6_scope_id: ULONG;
  end {ipv6};

  type _WINSTATIONREMOTEADDRESS = record
    case sin_family: USHORT of
      AF_INET:  (ipv4: TIPv4);
      AF_INET6: (ipv6: TIPv6);
  end;


{+//------------------------------------------------*/ }
{+// }
{-* Licensing Policy information struct }
{= }

const
  LCPOLICYINFOTYPE_V1 = (1);
  LCPOLICYINFOTYPE_CURRENT = LCPOLICYINFOTYPE_V1;

type
  LCPOLICYINFO_V1W = record
    ulVersion: ULONG;
    lpPolicyName: PWCHAR;
    lpPolicyDescription: PWCHAR;
  end {LCPOLICYINFO_V1W};
  LPLCPOLICYINFO_V1W = ^LCPOLICYINFO_V1W;

type
  LCPOLICYINFO_V1A = record
    ulVersion: ULONG;
    lpPolicyName: PAnsiChar;
    lpPolicyDescription: PAnsiChar;
  end {LCPOLICYINFO_V1A};
  LPLCPOLICYINFO_V1A = ^LCPOLICYINFO_V1A;

{$IFDEF UNICODE}
type
  LCPOLICYINFO_V1 = LCPOLICYINFO_V1W;
  LPLCPOLICYINFO_V1 = LPLCPOLICYINFO_V1W;
{$ELSE}
type
  LCPOLICYINFO_V1 = LCPOLICYINFO_V1A;
  LPLCPOLICYINFO_V1 = LPLCPOLICYINFO_V1A;
{$ENDIF}
const
  DEFAULT_POLICY_ID = 1;
  PERSEAT_POLICY_ID = 2;
  INTCONN_POLICY_ID = 3;
  PERUSER_POLICY_ID = 4;
  POLICY_NOT_CONFIGURED = 5;
  MAXIMUM_POLICY_ID = 6;

{+//------------------------------------------------*/ }

type
  _BEEPINPUT = record
    uType: ULONG;
  end {_BEEPINPUT};
  BEEPINPUT = _BEEPINPUT;
  PBEEPINPUT = ^_BEEPINPUT;

{+//********************* }
{-* NWLogon Structure }
{=********************* }

const
{$IFNDEF JWA_INCLUDEMODE}
  IDTIMEOUT = 32000;
  IDASYNC = 32001;
{$ENDIF JWA_INCLUDEMODE}  
  WSD_LOGOFF = $00000001;
  WSD_SHUTDOWN = $00000002;
  WSD_REBOOT = $00000004;
  WSD_POWEROFF = $00000008;
  WSD_FASTREBOOT = $00000010;

{$IFNDEF JWA_INCLUDEMODE}
const
  WTS_CONSOLE_CONNECT = $1;
  WTS_CONSOLE_DISCONNECT = $2;
  WTS_REMOTE_CONNECT = $3;
  WTS_REMOTE_DISCONNECT = $4;
  WTS_SESSION_LOGON = $5;
  WTS_SESSION_LOGOFF = $6;
  WTS_SESSION_LOCK = $7;
  WTS_SESSION_UNLOCK = $8;
  WTS_SESSION_REMOTE_CONTROL = $9;
{$ENDIF JWA_INCLUDEMODE}  


// We cannot convert the CREATE_MASK macro below to Delphi
// CREATE_MASK(__bit) = (1 << (__bit -1) );
// The Macro is used to calculate constants:
// #define WTS_CONSOLE_CONNECT_MASK         CREATE_MASK( WTS_CONSOLE_CONNECT )
// The output for the masks is:
// WTS_CONSOLE_CONNECT_MASK=1
// WTS_CONSOLE_DISCONNECT_MASK=2
// WTS_REMOTE_CONNECT_MASK=4
// WTS_REMOTE_DISCONNECT_MASK=8
// WTS_SESSION_LOGON_MASK=16
// WTS_SESSION_LOGOFF_MASK=32
// WTS_SESSION_LOCK_MASK=64
// WTS_SESSION_UNLOCK_MASK=128
// WTS_SESSION_REMOTE_CONTROL_MASK=256
// WTS_ALL_NOTIFICATION_MASK=-1

const
  WTS_CONSOLE_CONNECT_MASK = 1;
  WTS_CONSOLE_DISCONNECT_MASK = 2;
  WTS_REMOTE_CONNECT_MASK = 4;
  WTS_REMOTE_DISCONNECT_MASK = 8;
  WTS_SESSION_LOGON_MASK = 16;
  WTS_SESSION_LOGOFF_MASK = 32;
  WTS_SESSION_LOCK_MASK = 64;
  WTS_SESSION_UNLOCK_MASK = 128;
  WTS_SESSION_REMOTE_CONTROL_MASK = 256;
  WTS_ALL_NOTIFICATION_MASK = -1;

type
  _SESSIONDATAW = record
    SessionId: ULONG;
    State: WINSTATIONSTATECLASS;
    Source: ULONG;
    bFullDesktop: BOOLEAN;
    SessionType: GUID;
    WinStationName: WINSTATIONNAMEW;
    ProtocolStatus: PROTOCOLSTATUS;
  end {_SESSIONDATAW};
  SESSIONDATAW = _SESSIONDATAW;
  PSESSIONDATAW = ^_SESSIONDATAW;

type
  SESSIONDATA = SESSIONDATAW;
  PSESSIONDATA = PSESSIONDATAW;

{$IFNDEF _TS_CONNECTIONTYPE_GUID_}
{$DEFINE _TS_CONNECTIONTYPE_GUID_}

{///=========================================================================== }
{/// }
{/// Definitions of various terminal types. }
{/// }
{///=========================================================================== }

{/// GUID for Service Terminal }
const
  TERMINAL_TYPE_SERVICE: TGUID = '{88f5767d-d13f-404d-a348-8b8e030294a9}';

{/// GUID for Regular desktop Remote Terminal }
const
  TERMINAL_TYPE_REGULAR_DESKTOP: TGUID = '{0f0a4bf8-8362-435d-938c-222a518a8b78}';

{/// GUID for Remote Applications Terminal }
const
  TERMINAL_TYPE_RDP_REMOTEAPP: TGUID = '{eddcc3ce-6e7e-4f4b-8439-3d9ad4c9440f}';

{/// GUID for MCE Terminal - This is same as MCE license type }
const
  TERMINAL_TYPE_MCE: TGUID = '{8dc86f1d-9969-4379-91c1-06fe1dc60575}';

{/// GUID for the license pool that includes regular desktop, single app sessions, }
{/// RAIL etc. }
const
  LICENSE_TYPE_DEFAULT: TGUID = '{00000000-0000-0000-0000-000000000000}';

const
  LICENSE_TYPE_BUILTIN: TGUID = '{45344fe7-00e6-4ac6-9f01-d01fd4ffadfb}';

{/// GUID for the app server license type. }
const
  LICENSE_TYPE_APPSERVER: TGUID = '{36e64007-aef1-4085-89e3-66c26abade84}';

{/// GUID for the license pool that includes MCE sessions. }
const
  LICENSE_TYPE_MCE: TGUID = '{8dc86f1d-9969-4379-91c1-06fe1dc60575}';

{$ENDIF _TS_CONNECTIONTYPE_GUID_}

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation

{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF DYNAMIC_LINK}
//static link
{$ELSE}
//dynamic link
{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
