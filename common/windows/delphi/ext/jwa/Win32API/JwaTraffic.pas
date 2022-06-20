{******************************************************************************}
{                                                                              }
{ Traffic Control API interface Unit for Object Pascal                         }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: traffic.h, released June 2000. The original Pascal     }
{ code is: Traffic.pas, released December 2000. The initial developer of the   }
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

// $Id: JwaTraffic.pas,v 1.11 2007/09/05 11:58:52 dezipaitor Exp $
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaTraffic;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "traffic.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaWinType, JwaQos;

{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}

//---------------------------------------------------------------------------
//
// Define's
//

const
  CURRENT_TCI_VERSION = $0002;
  {$EXTERNALSYM CURRENT_TCI_VERSION}

//
// Definitions of notification events. These may be passed
// to the client's notification handler, to identify the
// notification type
//

//
// A TC interface has come up
//

  TC_NOTIFY_IFC_UP = 1;
  {$EXTERNALSYM TC_NOTIFY_IFC_UP}

//
// A TC interface has come down
//

  TC_NOTIFY_IFC_CLOSE = 2;
  {$EXTERNALSYM TC_NOTIFY_IFC_CLOSE}

//
// A change on a TC interface, typically a change in the
// list of supported network addresses
//

  TC_NOTIFY_IFC_CHANGE = 3;
  {$EXTERNALSYM TC_NOTIFY_IFC_CHANGE}

//
// A TC parameter has changed
//

  TC_NOTIFY_PARAM_CHANGED = 4;
  {$EXTERNALSYM TC_NOTIFY_PARAM_CHANGED}

//
// A flow has been closed by the TC interface
// for example: after a remote call close, or the whole interface
// is going down
//

  TC_NOTIFY_FLOW_CLOSE = 5;
  {$EXTERNALSYM TC_NOTIFY_FLOW_CLOSE}

  TC_INVALID_HANDLE = HANDLE(0);
  {$EXTERNALSYM TC_INVALID_HANDLE}

  MAX_STRING_LENGTH = 256;
  {$EXTERNALSYM MAX_STRING_LENGTH}

//---------------------------------------------------------------------------
//
// Typedef's and structures
//

//
// Handlers registered by the TCI client
//

type
  TCI_NOTIFY_HANDLER = procedure(ClRegCtx, ClIfcCtx: HANDLE; Event: ULONG;
    SubCode: HANDLE; BufSize: ULONG; Buffer: PVOID); stdcall;
  {$EXTERNALSYM TCI_NOTIFY_HANDLER}
  TTciNotifyHandler = TCI_NOTIFY_HANDLER;

  TCI_ADD_FLOW_COMPLETE_HANDLER = procedure(ClFlowCtx: HANDLE; Status: ULONG); stdcall;
  {$EXTERNALSYM TCI_ADD_FLOW_COMPLETE_HANDLER}
  TTciAddFlowCompleteHandler = TCI_ADD_FLOW_COMPLETE_HANDLER;

  TCI_MOD_FLOW_COMPLETE_HANDLER = procedure(ClFlowCtx: HANDLE; Status: ULONG); stdcall;
  {$EXTERNALSYM TCI_MOD_FLOW_COMPLETE_HANDLER}
  TTciModFlowCompleteHandler = TCI_MOD_FLOW_COMPLETE_HANDLER;

  TCI_DEL_FLOW_COMPLETE_HANDLER = procedure(ClFlowCtx: HANDLE; Status: ULONG); stdcall;
  {$EXTERNALSYM TCI_DEL_FLOW_COMPLETE_HANDLER}
  TTciDelFlowComlpeteHandler = TCI_DEL_FLOW_COMPLETE_HANDLER;

type
  PTCI_CLIENT_FUNC_LIST = ^TCI_CLIENT_FUNC_LIST;
  {$EXTERNALSYM PTCI_CLIENT_FUNC_LIST}
  _TCI_CLIENT_FUNC_LIST = record
    ClNotifyHandler: TCI_NOTIFY_HANDLER;
    ClAddFlowCompleteHandler: TCI_ADD_FLOW_COMPLETE_HANDLER;
    ClModifyFlowCompleteHandler: TCI_MOD_FLOW_COMPLETE_HANDLER;
    ClDeleteFlowCompleteHandler: TCI_DEL_FLOW_COMPLETE_HANDLER;
  end;
  {$EXTERNALSYM _TCI_CLIENT_FUNC_LIST}
  TCI_CLIENT_FUNC_LIST = _TCI_CLIENT_FUNC_LIST;
  {$EXTERNALSYM TCI_CLIENT_FUNC_LIST}
  TTciClientFuncList = TCI_CLIENT_FUNC_LIST;
  PTciClientFuncList = PTCI_CLIENT_FUNC_LIST;

  // TODO NETWORD_ADDRESS and NETWORK_ADDRESS_LIST are from NtDDNdis.h

  _NETWORK_ADDRESS = record
    AddressLength: USHORT;              // length in bytes of Address[] in this
    AddressType: USHORT;                // type of this address (NDIS_PROTOCOL_ID_XXX above)
    Address: array [0..0] of UCHAR;     // actually AddressLength bytes long
  end;
  NETWORK_ADDRESS = _NETWORK_ADDRESS;
  PNETWORK_ADDRESS = ^NETWORK_ADDRESS;

  _NETWORK_ADDRESS_LIST = record
    AddressCount: LONG;                 // number of addresses following
    AddressType: USHORT;                // type of this address (NDIS_PROTOCOL_ID_XXX above)
    Address: array [0..0] of NETWORK_ADDRESS; // actually AddressCount elements long
  end;
  NETWORK_ADDRESS_LIST = _NETWORK_ADDRESS_LIST;
  PNETWORK_ADDRESS_LIST = ^NETWORK_ADDRESS_LIST;

//
// Network address descriptor
//

  PADDRESS_LIST_DESCRIPTOR = ^ADDRESS_LIST_DESCRIPTOR;
  {$EXTERNALSYM PADDRESS_LIST_DESCRIPTOR}
  _ADDRESS_LIST_DESCRIPTOR = record
    MediaType: ULONG;
    AddressList: NETWORK_ADDRESS_LIST;
  end;
  {$EXTERNALSYM _ADDRESS_LIST_DESCRIPTOR}
  ADDRESS_LIST_DESCRIPTOR = _ADDRESS_LIST_DESCRIPTOR;
  {$EXTERNALSYM ADDRESS_LIST_DESCRIPTOR}
  TAddressListDescriptor = ADDRESS_LIST_DESCRIPTOR;
  PAddressListDescriptor = PADDRESS_LIST_DESCRIPTOR;

//
// An interface ID that is returned by the enumerator
//

  PTC_IFC_DESCRIPTOR = ^TC_IFC_DESCRIPTOR;
  {$EXTERNALSYM PTC_IFC_DESCRIPTOR}
  _TC_IFC_DESCRIPTOR = record
    Length: ULONG;
    pInterfaceName: LPWSTR;
    pInterfaceID: LPWSTR;
    AddressListDesc: ADDRESS_LIST_DESCRIPTOR;
  end;
  {$EXTERNALSYM _TC_IFC_DESCRIPTOR}
  TC_IFC_DESCRIPTOR = _TC_IFC_DESCRIPTOR;
  {$EXTERNALSYM TC_IFC_DESCRIPTOR}
  TTcIfcDescriptor = TC_IFC_DESCRIPTOR;
  PTcIfcDescriptor = PTC_IFC_DESCRIPTOR;

//
// This structure is returned by a QoS data provider in reply to
// GUID_QOS_SUPPORTED query or with an interface UP notification
//

  PTC_SUPPORTED_INFO_BUFFER = ^TC_SUPPORTED_INFO_BUFFER;
  {$EXTERNALSYM PTC_SUPPORTED_INFO_BUFFER}
  _TC_SUPPORTED_INFO_BUFFER = record
    InstanceIDLength: USHORT;
    // device or interface ID
    InstanceID: array [0..MAX_STRING_LENGTH - 1] of WCHAR;
    // address list
    AddrListDesc: ADDRESS_LIST_DESCRIPTOR;
  end;
  {$EXTERNALSYM _TC_SUPPORTED_INFO_BUFFER}
  TC_SUPPORTED_INFO_BUFFER = _TC_SUPPORTED_INFO_BUFFER;
  {$EXTERNALSYM TC_SUPPORTED_INFO_BUFFER}
  TTcSupportedInfoBuffer = TC_SUPPORTED_INFO_BUFFER;
  PTcSupportedInfoBuffer = PTC_SUPPORTED_INFO_BUFFER;

//
// Filters are used to match packets. The Pattern field
// indicates the values to which bits in corresponding
// positions in candidate packets should be compared. The
// Mask field indicates which bits are to be compared and
// which bits are don't cares.
//
// Different filters can be submitted on the TCI interface.
// The generic filter structure is defined to include an
// AddressType, which indicates the specific type of filter to
// follow.
//

  PTC_GEN_FILTER = ^TC_GEN_FILTER;
  {$EXTERNALSYM PTC_GEN_FILTER}
  _TC_GEN_FILTER = record
    AddressType: USHORT; // IP, IPX, etc.
    PatternSize: ULONG; // byte count of the pattern
    Pattern: PVOID; // specific format, e.g. IP_PATTERN
    Mask: PVOID; // same type as Pattern
  end;
  {$EXTERNALSYM _TC_GEN_FILTER}
  TC_GEN_FILTER = _TC_GEN_FILTER;
  {$EXTERNALSYM TC_GEN_FILTER}
  TTcGenFilter = TC_GEN_FILTER;
  PTcGenFilter = PTC_GEN_FILTER;

//
// A generic flow includes two flowspecs and a freeform
// buffer which contains flow specific TC objects.
//

  PTC_GEN_FLOW = ^TC_GEN_FLOW;
  {$EXTERNALSYM PTC_GEN_FLOW}
  _TC_GEN_FLOW = record
    SendingFlowspec: FLOWSPEC;
    ReceivingFlowspec: FLOWSPEC;
    TcObjectsLength: ULONG; // number of optional bytes
    TcObjects: array [0..0] of QOS_OBJECT_HDR;
  end;
  {$EXTERNALSYM _TC_GEN_FLOW}
  TC_GEN_FLOW = _TC_GEN_FLOW;
  {$EXTERNALSYM TC_GEN_FLOW}
  TTcGenFlow = TC_GEN_FLOW;
  PTcGenFlow = PTC_GEN_FLOW;

//
// Format of specific pattern or mask used by GPC for the IP protocol
//

  PIP_PATTERN = ^IP_PATTERN;
  {$EXTERNALSYM PIP_PATTERN}
  _IP_PATTERN = record
    Reserved1: ULONG;
    Reserved2: ULONG;
    SrcAddr: ULONG;
    DstAddr: ULONG;
    S_un: record
    case Integer of
      0: (
        s_srcport: USHORT;
        s_dstport: USHORT);
      1: (
        s_type: UCHAR;
        s_code: UCHAR;
        filler: USHORT);
      2: (
        S_Spi: ULONG);
    end;
    ProtocolId: UCHAR;
    Reserved3: array [0..3 - 1] of UCHAR;
  end;
  {$EXTERNALSYM _IP_PATTERN}
  IP_PATTERN = _IP_PATTERN;
  {$EXTERNALSYM IP_PATTERN}
  TIpPattern = IP_PATTERN;
  PIpPattern = PIP_PATTERN;

//
// Format of specific pattern or mask used by GPC for the IPX protocol
//

  TIpxPatternAddress = record
    NetworkAddress: ULONG;
    NodeAddress: array [0..5] of UCHAR;
    Socket: USHORT;
  end;

  PIPX_PATTERN = ^IPX_PATTERN;
  {$EXTERNALSYM PIPX_PATTERN}
  _IPX_PATTERN = record
    Src: TIpxPatternAddress;
    Dest: TIpxPatternAddress;
  end;
  {$EXTERNALSYM _IPX_PATTERN}
  IPX_PATTERN = _IPX_PATTERN;
  {$EXTERNALSYM IPX_PATTERN}
  TIpxPattern = IPX_PATTERN;
  PIpxPattern = PIPX_PATTERN;

//
// The enumeration buffer is the flow parameters + a list of filters
//

  PENUMERATION_BUFFER = ^ENUMERATION_BUFFER;
  {$EXTERNALSYM PENUMERATION_BUFFER}
  _ENUMERATION_BUFFER = record
    Length: ULONG;
    OwnerProcessId: ULONG;
    FlowNameLength: USHORT;
    FlowName: array [0..MAX_STRING_LENGTH - 1] of WCHAR;
    pFlow: PTC_GEN_FLOW;
    NumberOfFilters: ULONG;
    GenericFilter: array [0..0] of TC_GEN_FILTER; // one for each filter
  end;
  {$EXTERNALSYM _ENUMERATION_BUFFER}
  ENUMERATION_BUFFER = _ENUMERATION_BUFFER;
  {$EXTERNALSYM ENUMERATION_BUFFER}
  TEnumerationBuffer = ENUMERATION_BUFFER;
  PEnumerationBuffer = PENUMERATION_BUFFER;

//
// QoS objects supported by traffic
//

const
  QOS_TRAFFIC_GENERAL_ID_BASE = 4000;
  {$EXTERNALSYM QOS_TRAFFIC_GENERAL_ID_BASE}

  QOS_OBJECT_DS_CLASS      = $00000001 + QOS_TRAFFIC_GENERAL_ID_BASE;
  {$EXTERNALSYM QOS_OBJECT_DS_CLASS}
  QOS_OBJECT_TRAFFIC_CLASS = $00000002 + QOS_TRAFFIC_GENERAL_ID_BASE;
  {$EXTERNALSYM QOS_OBJECT_TRAFFIC_CLASS}
  QOS_OBJECT_DIFFSERV      = $00000003 + QOS_TRAFFIC_GENERAL_ID_BASE;
  {$EXTERNALSYM QOS_OBJECT_DIFFSERV}
  QOS_OBJECT_TCP_TRAFFIC   = $00000004 + QOS_TRAFFIC_GENERAL_ID_BASE;
  {$EXTERNALSYM QOS_OBJECT_TCP_TRAFFIC}
  QOS_OBJECT_FRIENDLY_NAME = $00000005 + QOS_TRAFFIC_GENERAL_ID_BASE;
  {$EXTERNALSYM QOS_OBJECT_FRIENDLY_NAME}

//
// This structure is used to associate a friendly name with the flow
// 

type
  LPQOS_FRIENDLY_NAME = ^QOS_FRIENDLY_NAME;
  {$EXTERNALSYM LPQOS_FRIENDLY_NAME}
  _QOS_FRIENDLY_NAME = record
    ObjectHdr: QOS_OBJECT_HDR;
    FriendlyName: array [0..MAX_STRING_LENGTH - 1] of WCHAR;
  end;
  {$EXTERNALSYM _QOS_FRIENDLY_NAME}
  QOS_FRIENDLY_NAME = _QOS_FRIENDLY_NAME;
  {$EXTERNALSYM QOS_FRIENDLY_NAME}
  TQosFriendlyName = QOS_FRIENDLY_NAME;
  PQosFriendlyName = LPQOS_FRIENDLY_NAME;

//
// This structure may carry an 802.1 TrafficClass parameter which 
// has been provided to the host by a layer 2 network, for example, 
// in an 802.1 extended RSVP RESV message. If this object is obtained
// from the network, hosts will stamp the MAC headers of corresponding
// transmitted packets, with the value in the object. Otherwise, hosts
// may select a value based on the standard Intserv mapping of 
// ServiceType to 802.1 TrafficClass.
//
//

  LPQOS_TRAFFIC_CLASS = ^QOS_TRAFFIC_CLASS;
  {$EXTERNALSYM LPQOS_TRAFFIC_CLASS}
  _QOS_TRAFFIC_CLASS = record
    ObjectHdr: QOS_OBJECT_HDR;
    TrafficClass: ULONG;
  end;
  {$EXTERNALSYM _QOS_TRAFFIC_CLASS}
  QOS_TRAFFIC_CLASS = _QOS_TRAFFIC_CLASS;
  {$EXTERNALSYM QOS_TRAFFIC_CLASS}
  TQosTrafficClass = QOS_TRAFFIC_CLASS;
  PQosTrafficClass = LPQOS_TRAFFIC_CLASS;

//
// This structure may carry an DSField parameter which  has been provided to 
// the host by a layer 3 network, for example, in an extended RSVP RESV message. 
// If this object is obtained from the network, hosts will stamp the DS Field on the
// IP header of transmitted packets, with the value in the object. Otherwise, hosts
// may select a value based on the standard Intserv mapping of ServiceType to DS Field 
//

  LPQOS_DS_CLASS = ^QOS_DS_CLASS;
  {$EXTERNALSYM LPQOS_DS_CLASS}
  _QOS_DS_CLASS = record
    ObjectHdr: QOS_OBJECT_HDR;
    DSField: ULONG;
  end;
  {$EXTERNALSYM _QOS_DS_CLASS}
  QOS_DS_CLASS = _QOS_DS_CLASS;
  {$EXTERNALSYM QOS_DS_CLASS}
  TQosDsClass = QOS_DS_CLASS;
  PQosDsClass = LPQOS_DS_CLASS;

//
// This structure is used to create DiffServ Flows. This creates flows in the packet scheduler
// and allows it to classify to packets based on a particular DS field. This structure takes
// a variable length array of QOS_DIFFSERV_RULE, where each DS field is specified by a 
// QOS_DIFFSERV_RULE
//
  LPQOS_DIFFSERV = ^QOS_DIFFSERV;
  {$EXTERNALSYM LPQOS_DIFFSERV}
  _QOS_DIFFSERV = record
    ObjectHdr: QOS_OBJECT_HDR;
    DSFieldCount: ULONG;
    DiffservRule: array [0..0] of UCHAR;
  end;
  {$EXTERNALSYM _QOS_DIFFSERV}
  QOS_DIFFSERV = _QOS_DIFFSERV;
  {$EXTERNALSYM QOS_DIFFSERV}
  TQosDiffserv = QOS_DIFFSERV;
  PQosDiffserv = LPQOS_DIFFSERV;

//
// The rule for a Diffserv DS codepoint. 
//

  LPQOS_DIFFSERV_RULE = ^QOS_DIFFSERV_RULE;
  {$EXTERNALSYM LPQOS_DIFFSERV_RULE}
  _QOS_DIFFSERV_RULE = record
    InboundDSField: UCHAR;
    ConformingOutboundDSField: UCHAR;
    NonConformingOutboundDSField: UCHAR;
    ConformingUserPriority: UCHAR;
    NonConformingUserPriority: UCHAR;
  end;
  {$EXTERNALSYM _QOS_DIFFSERV_RULE}
  QOS_DIFFSERV_RULE = _QOS_DIFFSERV_RULE;
  {$EXTERNALSYM QOS_DIFFSERV_RULE}
  TQosDiffservRule = QOS_DIFFSERV_RULE;
  PQosDiffservRule = LPQOS_DIFFSERV_RULE;

// 
// This structure is passed to indicate that the IP Precedence and UserPriority mappings for the flow
// have to be set to the system defaults for TCP traffic. If this object is passed, 
// the ServiceType ==> DSField mapping, ServiceType ==> UserPriorityMapping, QOS_OBJECT_DS_CLASS
// and QOS_OBJECT_TRAFFIC_CLASS will be ignored.
//

  LPQOS_TCP_TRAFFIC = ^QOS_TCP_TRAFFIC;
  {$EXTERNALSYM LPQOS_TCP_TRAFFIC}
  _QOS_TCP_TRAFFIC = record
    ObjectHdr: QOS_OBJECT_HDR;
  end;
  {$EXTERNALSYM _QOS_TCP_TRAFFIC}
  QOS_TCP_TRAFFIC = _QOS_TCP_TRAFFIC;
  {$EXTERNALSYM QOS_TCP_TRAFFIC}
  TQosTcpTraffic = QOS_TCP_TRAFFIC;
  PQosTcpTraffic = LPQOS_TCP_TRAFFIC;

//---------------------------------------------------------------------------
//
// Interface Function Definitions
//

function TcRegisterClient(TciVersion: ULONG; ClRegCtx: HANDLE; const ClientHandlerList: TCI_CLIENT_FUNC_LIST; var pClientHandle: HANDLE): ULONG; stdcall;
{$EXTERNALSYM TcRegisterClient}
function TcEnumerateInterfaces(ClientHandle: HANDLE; var pBufferSize: ULONG; var InterfaceBuffer: TC_IFC_DESCRIPTOR): ULONG; stdcall;
{$EXTERNALSYM TcEnumerateInterfaces}
function TcOpenInterfaceA(pInterfaceName: LPSTR; ClientHandle, ClIfcCtx: HANDLE; var pIfcHandle: HANDLE): ULONG; stdcall;
{$EXTERNALSYM TcOpenInterfaceA}
function TcOpenInterfaceW(pInterfaceName: LPWSTR; ClientHandle, ClIfcCtx: HANDLE; var pIfcHandle: HANDLE): ULONG; stdcall;
{$EXTERNALSYM TcOpenInterfaceW}
function TcCloseInterface(IfcHandle: HANDLE): ULONG; stdcall;
{$EXTERNALSYM TcCloseInterface}
function TcQueryInterface(IfcHandle: HANDLE; const pGuidParam: GUID; NotifyChange: Longbool; var pBufferSize: ULONG; Buffer: PVOID): ULONG; stdcall;
{$EXTERNALSYM TcQueryInterface}
function TcSetInterface(IfcHandle: HANDLE; const pGuidParam: GUID; BufferSize: ULONG; Buffer: PVOID): ULONG; stdcall;
{$EXTERNALSYM TcSetInterface}
function TcQueryFlowA(pFlowName: LPSTR; const pGuidParam: GUID; var pBufferSize: ULONG; Buffer: PVOID): ULONG; stdcall;
{$EXTERNALSYM TcQueryFlowA}
function TcQueryFlowW(pFlowName: LPWSTR; const pGuidParam: GUID; var pBufferSize: ULONG; Buffer: PVOID): ULONG; stdcall;
{$EXTERNALSYM TcQueryFlowW}
function TcSetFlowA(pFlowName: LPSTR; const pGuidParam: GUID; BufferSize: GUID; Buffer: PVOID): ULONG; stdcall;
{$EXTERNALSYM TcSetFlowA}
function TcSetFlowW(pFlowName: LPWSTR; const pGuidParam: GUID; BufferSize: GUID; Buffer: PVOID): ULONG; stdcall;
{$EXTERNALSYM TcSetFlowW}
function TcAddFlow(IfcHandle, ClFlowCtx: HANDLE; Flags: ULONG; const pGenericFlow: TC_GEN_FLOW; var pFlowHandle: HANDLE): ULONG; stdcall;
{$EXTERNALSYM TcAddFlow}
function TcGetFlowNameA(FlowHandle: HANDLE; StrSize: ULONG; pFlowName: LPSTR): ULONG; stdcall;
{$EXTERNALSYM TcGetFlowNameA}
function TcGetFlowNameW(FlowHandle: HANDLE; StrSize: ULONG; pFlowName: LPWSTR): ULONG; stdcall;
{$EXTERNALSYM TcGetFlowNameW}
function TcModifyFlow(FlowHandle: HANDLE; const pGenericFlow: TC_GEN_FLOW): ULONG; stdcall;
{$EXTERNALSYM TcModifyFlow}
function TcAddFilter(FlowHandle: HANDLE; const pGenericFilter: TC_GEN_FILTER; var pFilterHandle: HANDLE): ULONG; stdcall;
{$EXTERNALSYM TcAddFilter}
function TcDeregisterClient(ClientHandle: HANDLE): ULONG; stdcall;
{$EXTERNALSYM TcDeregisterClient}
function TcDeleteFlow(FlowHandle: HANDLE): ULONG; stdcall;
{$EXTERNALSYM TcDeleteFlow}
function TcDeleteFilter(FilterHandle: HANDLE): ULONG; stdcall;
{$EXTERNALSYM TcDeleteFilter}
function TcEnumerateFlows(IfcHandle: HANDLE; var pEnumHandle: HANDLE; var pFlowCount, pBufSize: ULONG; var Buffer: ENUMERATION_BUFFER): ULONG; stdcall;
{$EXTERNALSYM TcEnumerateFlows}

function TcOpenInterface(pInterfaceName: LPTSTR; ClientHandle, ClIfcCtx: HANDLE; var pIfcHandle: HANDLE): ULONG; stdcall;
{$EXTERNALSYM TcOpenInterface}
function TcQueryFlow(pFlowName: LPTSTR; const pGuidParam: GUID; var pBufferSize: ULONG; Buffer: PVOID): ULONG; stdcall;
{$EXTERNALSYM TcQueryFlow}
function TcSetFlow(pFlowName: LPTSTR; const pGuidParam: GUID; BufferSize: GUID; Buffer: PVOID): ULONG; stdcall;
{$EXTERNALSYM TcSetFlow}
function TcGetFlowName(FlowHandle: HANDLE; StrSize: ULONG; pFlowName: LPTSTR): ULONG; stdcall;
{$EXTERNALSYM TcGetFlowName}

{$ENDIF JWA_IMPLEMENTATIONSECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  trafficlib = 'traffic.dll';
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}

var
  _TcRegisterClient: Pointer;

function TcRegisterClient;
begin
  GetProcedureAddress(_TcRegisterClient, trafficlib, 'TcRegisterClient');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TcRegisterClient]
  end;
end;

var
  _TcEnumerateInterfaces: Pointer;

function TcEnumerateInterfaces;
begin
  GetProcedureAddress(_TcEnumerateInterfaces, trafficlib, 'TcEnumerateInterfaces');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TcEnumerateInterfaces]
  end;
end;

var
  _TcOpenInterfaceA: Pointer;

function TcOpenInterfaceA;
begin
  GetProcedureAddress(_TcOpenInterfaceA, trafficlib, 'TcOpenInterfaceA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TcOpenInterfaceA]
  end;
end;

var
  _TcOpenInterfaceW: Pointer;

function TcOpenInterfaceW;
begin
  GetProcedureAddress(_TcOpenInterfaceW, trafficlib, 'TcOpenInterfaceW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TcOpenInterfaceW]
  end;
end;

var
  _TcCloseInterface: Pointer;

function TcCloseInterface;
begin
  GetProcedureAddress(_TcCloseInterface, trafficlib, 'TcCloseInterface');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TcCloseInterface]
  end;
end;

var
  _TcQueryInterface: Pointer;

function TcQueryInterface;
begin
  GetProcedureAddress(_TcQueryInterface, trafficlib, 'TcQueryInterface');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TcQueryInterface]
  end;
end;

var
  _TcSetInterface: Pointer;

function TcSetInterface;
begin
  GetProcedureAddress(_TcSetInterface, trafficlib, 'TcSetInterface');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TcSetInterface]
  end;
end;

var
  _TcQueryFlowA: Pointer;

function TcQueryFlowA;
begin
  GetProcedureAddress(_TcQueryFlowA, trafficlib, 'TcQueryFlowA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TcQueryFlowA]
  end;
end;

var
  _TcQueryFlowW: Pointer;

function TcQueryFlowW;
begin
  GetProcedureAddress(_TcQueryFlowW, trafficlib, 'TcQueryFlowW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TcQueryFlowW]
  end;
end;

var
  _TcSetFlowA: Pointer;

function TcSetFlowA;
begin
  GetProcedureAddress(_TcSetFlowA, trafficlib, 'TcSetFlowA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TcSetFlowA]
  end;
end;

var
  _TcSetFlowW: Pointer;

function TcSetFlowW;
begin
  GetProcedureAddress(_TcSetFlowW, trafficlib, 'TcSetFlowW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TcSetFlowW]
  end;
end;

var
  _TcAddFlow: Pointer;

function TcAddFlow;
begin
  GetProcedureAddress(_TcAddFlow, trafficlib, 'TcAddFlow');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TcAddFlow]
  end;
end;

var
  _TcGetFlowNameA: Pointer;

function TcGetFlowNameA;
begin
  GetProcedureAddress(_TcGetFlowNameA, trafficlib, 'TcGetFlowNameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TcGetFlowNameA]
  end;
end;

var
  _TcGetFlowNameW: Pointer;

function TcGetFlowNameW;
begin
  GetProcedureAddress(_TcGetFlowNameW, trafficlib, 'TcGetFlowNameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TcGetFlowNameW]
  end;
end;

var
  _TcModifyFlow: Pointer;

function TcModifyFlow;
begin
  GetProcedureAddress(_TcModifyFlow, trafficlib, 'TcModifyFlow');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TcModifyFlow]
  end;
end;

var
  _TcAddFilter: Pointer;

function TcAddFilter;
begin
  GetProcedureAddress(_TcAddFilter, trafficlib, 'TcAddFilter');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TcAddFilter]
  end;
end;

var
  _TcDeregisterClient: Pointer;

function TcDeregisterClient;
begin
  GetProcedureAddress(_TcDeregisterClient, trafficlib, 'TcDeregisterClient');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TcDeregisterClient]
  end;
end;

var
  _TcDeleteFlow: Pointer;

function TcDeleteFlow;
begin
  GetProcedureAddress(_TcDeleteFlow, trafficlib, 'TcDeleteFlow');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TcDeleteFlow]
  end;
end;

var
  _TcDeleteFilter: Pointer;

function TcDeleteFilter;
begin
  GetProcedureAddress(_TcDeleteFilter, trafficlib, 'TcDeleteFilter');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TcDeleteFilter]
  end;
end;

var
  _TcEnumerateFlows: Pointer;

function TcEnumerateFlows;
begin
  GetProcedureAddress(_TcEnumerateFlows, trafficlib, 'TcEnumerateFlows');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TcEnumerateFlows]
  end;
end;

var
  _TcOpenInterface: Pointer;

function TcOpenInterface;
begin
  GetProcedureAddress(_TcOpenInterface, trafficlib, 'TcOpenInterface' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TcOpenInterface]
  end;
end;

var
  _TcQueryFlow: Pointer;

function TcQueryFlow;
begin
  GetProcedureAddress(_TcQueryFlow, trafficlib, 'TcQueryFlow' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TcQueryFlow]
  end;
end;

var
  _TcSetFlow: Pointer;

function TcSetFlow;
begin
  GetProcedureAddress(_TcSetFlow, trafficlib, 'TcSetFlow' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TcSetFlow]
  end;
end;

var
  _TcGetFlowName: Pointer;

function TcGetFlowName;
begin
  GetProcedureAddress(_TcGetFlowName, trafficlib, 'TcGetFlowName' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TcGetFlowName]
  end;
end;

{$ELSE}

function TcRegisterClient; external trafficlib name 'TcRegisterClient';
function TcEnumerateInterfaces; external trafficlib name 'TcEnumerateInterfaces';
function TcOpenInterfaceA; external trafficlib name 'TcOpenInterfaceA';
function TcOpenInterfaceW; external trafficlib name 'TcOpenInterfaceW';
function TcCloseInterface; external trafficlib name 'TcCloseInterface';
function TcQueryInterface; external trafficlib name 'TcQueryInterface';
function TcSetInterface; external trafficlib name 'TcSetInterface';
function TcQueryFlowA; external trafficlib name 'TcQueryFlowA';
function TcQueryFlowW; external trafficlib name 'TcQueryFlowW';
function TcSetFlowA; external trafficlib name 'TcSetFlowA';
function TcSetFlowW; external trafficlib name 'TcSetFlowW';
function TcAddFlow; external trafficlib name 'TcAddFlow';
function TcGetFlowNameA; external trafficlib name 'TcGetFlowNameA';
function TcGetFlowNameW; external trafficlib name 'TcGetFlowNameW';
function TcModifyFlow; external trafficlib name 'TcModifyFlow';
function TcAddFilter; external trafficlib name 'TcAddFilter';
function TcDeregisterClient; external trafficlib name 'TcDeregisterClient';
function TcDeleteFlow; external trafficlib name 'TcDeleteFlow';
function TcDeleteFilter; external trafficlib name 'TcDeleteFilter';
function TcEnumerateFlows; external trafficlib name 'TcEnumerateFlows';
function TcOpenInterface; external trafficlib name 'TcOpenInterface' + AWSuffix;
function TcQueryFlow; external trafficlib name 'TcQueryFlow' + AWSuffix;
function TcSetFlow; external trafficlib name 'TcSetFlow' + AWSuffix;
function TcGetFlowName; external trafficlib name 'TcGetFlowName' + AWSuffix;

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
