{******************************************************************************}
{                                                                              }
{ BlueTooth API interface Unit for Object Pascal                               }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ Portions created by Marcel van Brakel are Copyright (C) 1999-2001            }
{ Marcel van Brakel. All Rights Reserved.                                      }
{                                                                              }
{ Contributors: John Penman                                                    }
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

// $Id: JwaWs2Bth.pas,v 1.12 2007/09/14 06:48:49 marquardt Exp $
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaWs2Bth;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "ws2bth.h"'}
{$HPPEMIT ''}


{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaWinType, JwaBthSdpDef, JwaBlueToothApis, JwaBthDef;
{$ENDIF JWA_OMIT_SECTIONS}
{$IFNDEF JWA_IMPLEMENTATIONSECTION}

const
  BT_PORT_ANY = -1;
  {$EXTERNALSYM BT_PORT_ANY}
  BT_PORT_MIN = $1;
  {$EXTERNALSYM BT_PORT_MIN}
  BT_PORT_MAX = $ffff;
  {$EXTERNALSYM BT_PORT_MAX}
  BT_PORT_DYN_FIRST = $1001;
  {$EXTERNALSYM BT_PORT_DYN_FIRST}

//
// These three definitions are duplicated in winsock2.h to reserve ordinals
//

const
  AF_BTH = 32;
  {$EXTERNALSYM AF_BTH}
  PF_BTH = AF_BTH;
  {$EXTERNALSYM PF_BTH}
  NS_BTH = 16;
  {$EXTERNALSYM NS_BTH}

type
  _SOCKADDR_BTH = packed record
    addressFamily: Word; // Always AF_BTH
    btAddr: BTH_ADDR;      // Bluetooth device address
    serviceClassId: TGUID; // [OPTIONAL] system will query SDP for port
    port: ULONG;           // RFCOMM channel or L2CAP PSM
  end;
  {$EXTERNALSYM _SOCKADDR_BTH}
  SOCKADDR_BTH = _SOCKADDR_BTH;
  {$EXTERNALSYM SOCKADDR_BTH}
  PSOCKADDR_BTH = ^_SOCKADDR_BTH;
  {$EXTERNALSYM PSOCKADDR_BTH}
  TSockAddrBth = SOCKADDR_BTH;
  PSockAddrBth = PSOCKADDR_BTH;

const
  SVCID_BTH_PROVIDER: TGUID = '{06AA63E0-7D60-41FF-AFB2-3EE6D2D9392D}';
  {$EXTERNALSYM SVCID_BTH_PROVIDER}
  BTH_ADDR_STRING_SIZE = 12; // max size from WSAAddressToString
  {$EXTERNALSYM BTH_ADDR_STRING_SIZE}

//
// Bluetooth protocol #s are assigned according to the Bluetooth
// Assigned Numbers portion of the Bluetooth Specification
//

const
  BTHPROTO_RFCOMM = $0003;
  {$EXTERNALSYM BTHPROTO_RFCOMM}
  BTHPROTO_L2CAP = $0100;
  {$EXTERNALSYM BTHPROTO_L2CAP}

  SOL_RFCOMM = BTHPROTO_RFCOMM;
  {$EXTERNALSYM SOL_RFCOMM}
  SOL_L2CAP = BTHPROTO_L2CAP;
  {$EXTERNALSYM SOL_L2CAP}
  SOL_SDP = $0101;
  {$EXTERNALSYM SOL_SDP}

//
// SOCKET OPTIONS
//

  SO_BTH_AUTHENTICATE = $80000001; // optlen=sizeof(ULONG), optval = &(ULONG)TRUE/FALSE
  {$EXTERNALSYM SO_BTH_AUTHENTICATE}
  SO_BTH_ENCRYPT = $00000002; // optlen=sizeof(ULONG), optval = &(ULONG)TRUE/FALSE
  {$EXTERNALSYM SO_BTH_ENCRYPT}
  SO_BTH_MTU = $80000007; // optlen=sizeof(ULONG), optval = &mtu
  {$EXTERNALSYM SO_BTH_MTU}
  SO_BTH_MTU_MAX = $80000008; // optlen=sizeof(ULONG), optval = &max. mtu
  {$EXTERNALSYM SO_BTH_MTU_MAX}
  SO_BTH_MTU_MIN = $8000000a; // optlen=sizeof(ULONG), optval = &min. mtu
  {$EXTERNALSYM SO_BTH_MTU_MIN}

//
// Socket option parameters
//

  RFCOMM_MAX_MTU = $0000029a; // L2CAP MTU (672) - RFCOMM header size (6)
  {$EXTERNALSYM RFCOMM_MAX_MTU}
  RFCOMM_MIN_MTU = $00000017; // RFCOMM spec sec 5.3 table 5.1
  {$EXTERNALSYM RFCOMM_MIN_MTU}

//
// NAME SERVICE PROVIDER DEFINITIONS
// For calling WSASetService
// and WSALookupServiceBegin, WSALookupServiceNext, WSALookupServiceEnd
// with Bluetooth-specific extensions
//

  BTH_SDP_VERSION = 1;
  {$EXTERNALSYM BTH_SDP_VERSION}

//
// [OPTIONAL] passed in BLOB member of WSAQUERYSET
// QUERYSET and its lpBlob member are copied & converted
// to unicode in the system for non-unicode applications.
// However, nothing is copied back upon return.  In
// order for the system to return data such as pRecordHandle,
// it much have an extra level of indirection from lpBlob
//

type
  _BTH_SET_SERVICE = packed record

    //
    // This version number will change when/if the binary format of
    // SDP records change, affecting the format of pRecord.
    // Set to BTH_SDP_VERSION by client, and returned by system
    //
    pSdpVersion: PULONG;

    //
    // Handle to SDP record.  When BTH_SET_SERVICE structure is later
    // passed to WSASetService RNRSERVICE_DELETE, this handle identifies the
    // record to delete.
    //
    pRecordHandle: PHandle;

    //
    // COD_SERVICE_* bit(s) associated with this SDP record, which will be
    // advertised when the local radio is found during device inquiry.
    // When the last SDP record associated with a bit is deleted, that
    // service bit is no longer reported in repsonse to inquiries
    //
    fCodService: ULONG;    // COD_SERVICE_* bits

    Reserved: array [0..4] of ULONG;    // Reserved by system.  Must be zero.
    ulRecordLength: ULONG; // length of pRecord which follows
    pRecord: array [0..0] of ULONG;     // SDP record as defined by bluetooth spec
  end;
  {$EXTERNALSYM _BTH_SET_SERVICE}
  BTH_SET_SERVICE = _BTH_SET_SERVICE;
  {$EXTERNALSYM BTH_SET_SERVICE}
  PBTH_SET_SERVICE = ^_BTH_SET_SERVICE;
  {$EXTERNALSYM PBTH_SET_SERVICE}
  TBthSetService = BTH_SET_SERVICE;
  PBthSetService = PBTH_SET_SERVICE;

//
// Default device inquiry duration in seconds
//
// The application thread will be blocked in WSALookupServiceBegin
// for the duration of the device inquiry, so this value needs to
// be balanced against the chance that a device that is actually
// present might not being found by Bluetooth in this time
//
// Paging improvements post-1.1 will cause devices to be
// found generally uniformly in the 0-6 sec timeperiod
//

const
  SDP_DEFAULT_INQUIRY_SECONDS = 6;
  {$EXTERNALSYM SDP_DEFAULT_INQUIRY_SECONDS}
  SDP_MAX_INQUIRY_SECONDS = 60;
  {$EXTERNALSYM SDP_MAX_INQUIRY_SECONDS}

//
// Default maximum number of devices to search for
//

  SDP_DEFAULT_INQUIRY_MAX_RESPONSES = 255;
  {$EXTERNALSYM SDP_DEFAULT_INQUIRY_MAX_RESPONSES}

  SDP_SERVICE_SEARCH_REQUEST = 1;
  {$EXTERNALSYM SDP_SERVICE_SEARCH_REQUEST}
  SDP_SERVICE_ATTRIBUTE_REQUEST = 2;
  {$EXTERNALSYM SDP_SERVICE_ATTRIBUTE_REQUEST}
  SDP_SERVICE_SEARCH_ATTRIBUTE_REQUEST = 3;
  {$EXTERNALSYM SDP_SERVICE_SEARCH_ATTRIBUTE_REQUEST}

//
// [OPTIONAL] input restrictions on device inquiry
// Passed in BLOB of LUP_CONTAINERS (device) search
//

type
  _BTH_QUERY_DEVICE = packed record
    LAP: ULONG;
    length: UCHAR;
  end;
  {$EXTERNALSYM _BTH_QUERY_DEVICE}
  BTH_QUERY_DEVICE = _BTH_QUERY_DEVICE;
  {$EXTERNALSYM BTH_QUERY_DEVICE}
  PBTH_QUERY_DEVICE = ^_BTH_QUERY_DEVICE;
  {$EXTERNALSYM PBTH_QUERY_DEVICE}
  TBthQueryDevice = BTH_QUERY_DEVICE;
  PBthQueryDevice = PBTH_QUERY_DEVICE;

//
// [OPTIONAL] Restrictions on searching for a particular service
// Passed in BLOB of !LUP_CONTAINERS (service) search
//

  _BTH_QUERY_SERVICE = packed record
    _type: ULONG;
    serviceHandle: ULONG;
    uuids: array[0..MAX_UUIDS_IN_QUERY - 1] of SdpQueryUuid;
    numRange: ULONG;
    pRange: array[0..0] of SdpAttributeRange;
  end;
  {$EXTERNALSYM _BTH_QUERY_SERVICE}
  BTH_QUERY_SERVICE = _BTH_QUERY_SERVICE;
  {$EXTERNALSYM BTH_QUERY_SERVICE}
  PBTH_QUERY_SERVICE = ^_BTH_QUERY_SERVICE;
  {$EXTERNALSYM PBTH_QUERY_SERVICE}
  TBthQueryService = BTH_QUERY_SERVICE;
  PBthQueryService = PBTH_QUERY_SERVICE;

//
// BTHNS_RESULT_*
//
// Bluetooth specific flags returned from WSALookupServiceNext
// in WSAQUERYSET.dwOutputFlags in response to device inquiry
//

//
// Local device is paired with remote device
//

const
  BTHNS_RESULT_DEVICE_CONNECTED = $00010000;
  {$EXTERNALSYM BTHNS_RESULT_DEVICE_CONNECTED}
  BTHNS_RESULT_DEVICE_REMEMBERED = $00020000;
  {$EXTERNALSYM BTHNS_RESULT_DEVICE_REMEMBERED}
  BTHNS_RESULT_DEVICE_AUTHENTICATED = $00040000;
  {$EXTERNALSYM BTHNS_RESULT_DEVICE_AUTHENTICATED}

//
// SOCKET IOCTLs
//
  {$IFNDEF JWA_INCLUDEMODE}
  IOC_OUT      = $40000000;        // copy out parameters
  {$EXTERNALSYM IOC_OUT}
  IOC_IN       = DWORD($80000000); // copy in parameters
  {$EXTERNALSYM IOC_IN}
  IOC_INOUT    = DWORD(IOC_IN or IOC_OUT);
  {$EXTERNALSYM IOC_INOUT}
  {$ENDIF JWA_INCLUDEMODE}

  {$IFNDEF JWA_WINSOCK_2}
  IOC_VENDOR   = $18000000;
  {$EXTERNALSYM IOC_VENDOR}
  {$ENDIF JWA_WINSOCK_2}

const
  SIO_RFCOMM_SEND_COMMAND = IOC_INOUT or IOC_VENDOR or 101;
  {$EXTERNALSYM SIO_RFCOMM_SEND_COMMAND}
  SIO_RFCOMM_WAIT_COMMAND = IOC_INOUT or IOC_VENDOR or 102;
  {$EXTERNALSYM SIO_RFCOMM_WAIT_COMMAND}

//
// These IOCTLs are for test/validation/conformance and may only be
// present in debug/checked builds of the system
//

  SIO_BTH_PING                = IOC_INOUT or IOC_VENDOR or 8;
  {$EXTERNALSYM SIO_BTH_PING}
  SIO_BTH_INFO                = IOC_INOUT or IOC_VENDOR or 9;
  {$EXTERNALSYM SIO_BTH_INFO}
  SIO_RFCOMM_SESSION_FLOW_OFF = IOC_INOUT or IOC_VENDOR or 103;
  {$EXTERNALSYM SIO_RFCOMM_SESSION_FLOW_OFF}
  SIO_RFCOMM_TEST             = IOC_INOUT or IOC_VENDOR or 104;
  {$EXTERNALSYM SIO_RFCOMM_TEST}
  SIO_RFCOMM_USECFC           = IOC_INOUT or IOC_VENDOR or 105;
  {$EXTERNALSYM SIO_RFCOMM_USECFC}

//
// SOCKET IOCTL DEFINITIONS
//

function BIT(b: Integer): DWORD;

//
// Structure definition from Bluetooth RFCOMM spec, TS 07.10 5.4.6.3.7
//

const
// todo  MSC_EA_BIT      = EA_BIT;
  MSC_FC_BIT      = 1 shl 1;      // Flow control, clear if we can receive
  {$EXTERNALSYM MSC_FC_BIT}
  MSC_RTC_BIT     = 1 shl 2;      // Ready to communicate, set when ready
  {$EXTERNALSYM MSC_RTC_BIT}
  MSC_RTR_BIT     = 1 shl 3;      // Ready to receive, set when ready
  {$EXTERNALSYM MSC_RTR_BIT}
  MSC_RESERVED    = (1 shl 4) or (1 shl 5); // Reserved by spec, must be 0
  {$EXTERNALSYM MSC_RESERVED}
  MSC_IC_BIT      = 1 shl 6;      // Incoming call
  {$EXTERNALSYM MSC_IC_BIT}
  MSC_DV_BIT      = 1 shl 7;      // Data valid
  {$EXTERNALSYM MSC_DV_BIT}

  MSC_BREAK_BIT   = 1 shl 1;      // Set if sending break
  {$EXTERNALSYM MSC_BREAK_BIT}

//MSC_SET_BREAK_LENGTH(b, l) ((b) = ((b)&0x3) | (((l)&0xf) << 4))

type
  _RFCOMM_MSC_DATA = packed record
    Signals: UCHAR;
    Break: UCHAR;
  end;
  {$EXTERNALSYM _RFCOMM_MSC_DATA}
  RFCOMM_MSC_DATA = _RFCOMM_MSC_DATA;
  {$EXTERNALSYM RFCOMM_MSC_DATA}
  PRFCOMM_MSC_DATA = ^RFCOMM_MSC_DATA;
  {$EXTERNALSYM PRFCOMM_MSC_DATA}
  TRfCommMscData = RFCOMM_MSC_DATA;
  PRfCommMscData = PRFCOMM_MSC_DATA;

//
// Structure definition from Bluetooth RFCOMM spec, TS 07.10 5.4.6.3.10
//

const
  RLS_ERROR          = $01;
  {$EXTERNALSYM RLS_ERROR}
  RLS_OVERRUN        = $02;
  {$EXTERNALSYM RLS_OVERRUN}
  RLS_PARITY         = $04;
  {$EXTERNALSYM RLS_PARITY}
  RLS_FRAMING        = $08;
  {$EXTERNALSYM RLS_FRAMING}

type
  _RFCOMM_RLS_DATA = packed record
    LineStatus: UCHAR;
  end;
  {$EXTERNALSYM _RFCOMM_RLS_DATA}
  RFCOMM_RLS_DATA = _RFCOMM_RLS_DATA;
  {$EXTERNALSYM RFCOMM_RLS_DATA}
  PRFCOMM_RLS_DATA = ^RFCOMM_RLS_DATA;
  {$EXTERNALSYM PRFCOMM_RLS_DATA}
  TRfCommRlsData = RFCOMM_RLS_DATA;
  PRfCommRlsData = PRFCOMM_RLS_DATA;

//
// Structure definition from Bluetooth RFCOMM spec, TS 07.10 5.4.6.3.9
//

const
  RPN_BAUD_2400      = 0;
  {$EXTERNALSYM RPN_BAUD_2400}
  RPN_BAUD_4800      = 1;
  {$EXTERNALSYM RPN_BAUD_4800}
  RPN_BAUD_7200      = 2;
  {$EXTERNALSYM RPN_BAUD_7200}
  RPN_BAUD_9600      = 3;
  {$EXTERNALSYM RPN_BAUD_9600}
  RPN_BAUD_19200     = 4;
  {$EXTERNALSYM RPN_BAUD_19200}
  RPN_BAUD_38400     = 5;
  {$EXTERNALSYM RPN_BAUD_38400}
  RPN_BAUD_57600     = 6;
  {$EXTERNALSYM RPN_BAUD_57600}
  RPN_BAUD_115200    = 7;
  {$EXTERNALSYM RPN_BAUD_115200}
  RPN_BAUD_230400    = 8;
  {$EXTERNALSYM RPN_BAUD_230400}

  RPN_DATA_5          = $0;
  {$EXTERNALSYM RPN_DATA_5}
  RPN_DATA_6          = $1;
  {$EXTERNALSYM RPN_DATA_6}
  RPN_DATA_7          = $2;
  {$EXTERNALSYM RPN_DATA_7}
  RPN_DATA_8          = $3;
  {$EXTERNALSYM RPN_DATA_8}

  RPN_STOP_1          = $0;
  {$EXTERNALSYM RPN_STOP_1}
  RPN_STOP_1_5        = $4;
  {$EXTERNALSYM RPN_STOP_1_5}

  RPN_PARITY_NONE     = $00;
  {$EXTERNALSYM RPN_PARITY_NONE}
  RPN_PARITY_ODD      = $08;
  {$EXTERNALSYM RPN_PARITY_ODD}
  RPN_PARITY_EVEN     = $18;
  {$EXTERNALSYM RPN_PARITY_EVEN}
  RPN_PARITY_MARK     = $28;
  {$EXTERNALSYM RPN_PARITY_MARK}
  RPN_PARITY_SPACE    = $38;
  {$EXTERNALSYM RPN_PARITY_SPACE}

  RPN_FLOW_X_IN       = $01;
  {$EXTERNALSYM RPN_FLOW_X_IN}
  RPN_FLOW_X_OUT      = $02;
  {$EXTERNALSYM RPN_FLOW_X_OUT}
  RPN_FLOW_RTR_IN     = $04;
  {$EXTERNALSYM RPN_FLOW_RTR_IN}
  RPN_FLOW_RTR_OUT    = $08;
  {$EXTERNALSYM RPN_FLOW_RTR_OUT}
  RPN_FLOW_RTC_IN     = $10;
  {$EXTERNALSYM RPN_FLOW_RTC_IN}
  RPN_FLOW_RTC_OUT    = $20;
  {$EXTERNALSYM RPN_FLOW_RTC_OUT}

  RPN_PARAM_BAUD      = $01;
  {$EXTERNALSYM RPN_PARAM_BAUD}
  RPN_PARAM_DATA      = $02;
  {$EXTERNALSYM RPN_PARAM_DATA}
  RPN_PARAM_STOP      = $04;
  {$EXTERNALSYM RPN_PARAM_STOP}
  RPN_PARAM_PARITY    = $08;
  {$EXTERNALSYM RPN_PARAM_PARITY}
  RPN_PARAM_P_TYPE    = $10;
  {$EXTERNALSYM RPN_PARAM_P_TYPE}
  RPN_PARAM_XON       = $20;
  {$EXTERNALSYM RPN_PARAM_XON}
  RPN_PARAM_XOFF      = $40;
  {$EXTERNALSYM RPN_PARAM_XOFF}

  RPN_PARAM_X_IN      = $01;
  {$EXTERNALSYM RPN_PARAM_X_IN}
  RPN_PARAM_X_OUT     = $02;
  {$EXTERNALSYM RPN_PARAM_X_OUT}
  RPN_PARAM_RTR_IN    = $04;
  {$EXTERNALSYM RPN_PARAM_RTR_IN}
  RPN_PARAM_RTR_OUT   = $08;
  {$EXTERNALSYM RPN_PARAM_RTR_OUT}
  RPN_PARAM_RTC_IN    = $10;
  {$EXTERNALSYM RPN_PARAM_RTC_IN}
  RPN_PARAM_RTC_OUT   = $20;
  {$EXTERNALSYM RPN_PARAM_RTC_OUT}

type
  _RFCOMM_RPN_DATA = packed record
    Baud: UCHAR;
    Data: UCHAR;
    FlowControl: UCHAR;
    XonChar: UCHAR;
    XoffChar: UCHAR;
    ParameterMask1: UCHAR;
    ParameterMask2: UCHAR;
  end;
  {$EXTERNALSYM _RFCOMM_RPN_DATA}
  RFCOMM_RPN_DATA = _RFCOMM_RPN_DATA;
  {$EXTERNALSYM RFCOMM_RPN_DATA}
  PRFCOMM_RPN_DATA = ^_RFCOMM_RPN_DATA;
  {$EXTERNALSYM PRFCOMM_RPN_DATA}
  TRfCommRpnData = RFCOMM_RPN_DATA;
  PRfCommRpnData = PRFCOMM_RPN_DATA;

const
  RFCOMM_CMD_NONE            = 0;
  {$EXTERNALSYM RFCOMM_CMD_NONE}
  RFCOMM_CMD_MSC             = 1;
  {$EXTERNALSYM RFCOMM_CMD_MSC}
  RFCOMM_CMD_RLS             = 2;
  {$EXTERNALSYM RFCOMM_CMD_RLS}
  RFCOMM_CMD_RPN             = 3;
  {$EXTERNALSYM RFCOMM_CMD_RPN}
  RFCOMM_CMD_RPN_REQUEST     = 4;
  {$EXTERNALSYM RFCOMM_CMD_RPN_REQUEST}
  RFCOMM_CMD_RPN_RESPONSE    = 5;
  {$EXTERNALSYM RFCOMM_CMD_RPN_RESPONSE}

type
  _RFCOMM_COMMAND = packed record
    CmdType: ULONG;          // one of RFCOMM_CMD_*
    case Integer of
      0: (MSC: RFCOMM_MSC_DATA);
      1: (RLS: RFCOMM_RLS_DATA);
      2: (RPN: RFCOMM_RPN_DATA);
  end;
  {$EXTERNALSYM _RFCOMM_COMMAND}
  RFCOMM_COMMAND = _RFCOMM_COMMAND;
  {$EXTERNALSYM RFCOMM_COMMAND}
  PRFCOMM_COMMAND = ^RFCOMM_COMMAND;
  {$EXTERNALSYM PRFCOMM_COMMAND}
  TRfCommCommand = RFCOMM_COMMAND;
  PRfCommCommand = PRFCOMM_COMMAND;

//
// These structures are for test/validation/conformance and may only be
// present in debug/checked builds of the system
//

type
  _BTH_PING_REQ = packed record
    btAddr: BTH_ADDR;
    dataLen: UCHAR;
    data: array [0..MAX_L2CAP_PING_DATA_LENGTH - 1] of UCHAR;
  end;
  {$EXTERNALSYM _BTH_PING_REQ}
  BTH_PING_REQ = _BTH_PING_REQ;
  {$EXTERNALSYM BTH_PING_REQ}
  PBTH_PING_REQ = ^BTH_PING_REQ;
  {$EXTERNALSYM PBTH_PING_REQ}
  TBthPingReq = BTH_PING_REQ;
  PBthPingReq = PBTH_PING_REQ;

  _BTH_PING_RSP = packed record
    dataLen: UCHAR;
    data: array [0..MAX_L2CAP_PING_DATA_LENGTH - 1] of UCHAR;
  end;
  {$EXTERNALSYM _BTH_PING_RSP}
  BTH_PING_RSP = _BTH_PING_RSP;
  {$EXTERNALSYM BTH_PING_RSP}
  PBTH_PING_RSP = ^BTH_PING_RSP;
  {$EXTERNALSYM PBTH_PING_RSP}
  TBthPingRsp = BTH_PING_RSP;
  PBthPingRsp = PBTH_PING_RSP;

  _BTH_INFO_REQ = packed record
    btAddr: BTH_ADDR;
    infoType: Word;
  end;
  {$EXTERNALSYM _BTH_INFO_REQ}
  BTH_INFO_REQ = _BTH_INFO_REQ;
  {$EXTERNALSYM BTH_INFO_REQ}
  PBTH_INFO_REQ = ^BTH_INFO_REQ;
  {$EXTERNALSYM PBTH_INFO_REQ}
  TBthInfoReq = BTH_INFO_REQ;
  PBthInfoReq = PBTH_INFO_REQ;

  _BTH_INFO_RSP = packed record
    result: Word;
    dataLen: UCHAR;
    case Integer of
      0: (connectionlessMTU: Word);
      1: (data: array [0..MAX_L2CAP_INFO_DATA_LENGTH - 1] of UCHAR);
  end;
  {$EXTERNALSYM _BTH_INFO_RSP}
  BTH_INFO_RSP = _BTH_INFO_RSP;
  {$EXTERNALSYM BTH_INFO_RSP}
  PBTH_INFO_RSP = ^BTH_INFO_RSP;
  {$EXTERNALSYM PBTH_INFO_RSP}
  TBthInfoRsp = BTH_INFO_RSP;
  PBthInfoRsp = PBTH_INFO_RSP;

//
// WinCE compatible struct names
//

{ todo
typedef struct _BTH_SET_SERVICE BTHNS_SETBLOB, *PBTHNS_SETBLOB;
typedef struct _BTH_QUERY_DEVICE BTHNS_INQUIRYBLOB, *PBTHNS_INQUIRYBLOB;
typedef struct _BTH_QUERY_SERVICE BTHNS_RESTRICTIONBLOB, *PBTHNS_RESTRICTIONBLOB;
}
{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}


{$IFNDEF JWA_INTERFACESECTION}

function BIT(b: Integer): DWORD;
begin
  Result := DWORD(1 shl b);
end;

{$ENDIF JWA_INTERFACESECTION}


{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
