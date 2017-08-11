unit rdpencomapi_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// $Rev: 8291 $
// File generated on 02.04.2008 17:07:18 from Type Library described below.

// ************************************************************************  //
// Type Lib: \Microsoft SDKs\Windows\v6.0\Lib\rdpencomapi.tlb (1)
// LIBID: {CC802D05-AE07-4C15-B496-DB9D22AA0A84}
// LCID: 0
// Helpfile: 
// HelpString: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\system32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  rdpencomapiMajorVersion = 1;
  rdpencomapiMinorVersion = 0;

  LIBID_rdpencomapi: TGUID = '{CC802D05-AE07-4C15-B496-DB9D22AA0A84}';

  DIID__IRDPSessionEvents: TGUID = '{98A97042-6698-40E9-8EFD-B3200990004B}';
  IID_IRDPSRAPIApplication: TGUID = '{41E7A09D-EB7A-436E-935D-780CA2628324}';
  IID_IRDPSRAPIWindowList: TGUID = '{8A05CE44-715A-4116-A189-A118F30A07BD}';
  IID_IRDPSRAPIWindow: TGUID = '{BEAFE0F9-C77B-4933-BA9F-A24CDDCC27CF}';
  IID_IRDPSRAPIApplicationList: TGUID = '{D4B4AEB3-22DC-4837-B3B6-42EA2517849A}';
  IID_IRDPSRAPIApplicationFilter: TGUID = '{D20F10CA-6637-4F06-B1D5-277EA7E5160D}';
  IID_IRDPSRAPISessionProperties: TGUID = '{339B24F2-9BC0-4F16-9AAC-F165433D13D4}';
  IID_IRDPSRAPIInvitation: TGUID = '{4FAC1D43-FC51-45BB-B1B4-2B53AA562FA3}';
  IID_IRDPSRAPIInvitationManager: TGUID = '{4722B049-92C3-4C2D-8A65-F7348F644DCF}';
  IID_IRDPSRAPITcpConnectionInfo: TGUID = '{F74049A4-3D06-4028-8193-0A8C29BC2452}';
  IID_IRDPSRAPIAttendee: TGUID = '{EC0671B3-1B78-4B80-A464-9132247543E3}';
  IID_IRDPSRAPIAttendeeManager: TGUID = '{BA3A37E8-33DA-4749-8DA0-07FA34DA7944}';
  IID_IRDPSRAPIAttendeeDisconnectInfo: TGUID = '{C187689F-447C-44A1-9C14-FFFBB3B7EC17}';
  IID_IRDPSRAPIVirtualChannel: TGUID = '{05E12F95-28B3-4C9A-8780-D0248574A1E0}';
  IID_IRDPSRAPIVirtualChannelManager: TGUID = '{0D11C661-5D0D-4EE4-89DF-2166AE1FDFED}';
  IID_IRDPSRAPISharingSession: TGUID = '{EEB20886-E470-4CF6-842B-2739C0EC5CFB}';
  IID_IRDPSRAPIViewer: TGUID = '{C6BFCD38-8CE9-404D-8AE8-F31D00C65CB5}';
  CLASS_RDPViewer: TGUID = '{32BE5ED2-5C86-480F-A914-0FF8885A1B3F}';
  CLASS_RDPSession: TGUID = '{9B78F0E6-3E05-4A5B-B2E8-E743A8956B65}';
  CLASS_RDPSRAPISessionProperties: TGUID = '{DD7594FF-EA2A-4C06-8FDF-132DE48B6510}';
  CLASS_RDPSRAPIInvitationManager: TGUID = '{53D9C9DB-75AB-4271-948A-4C4EB36A8F2B}';
  CLASS_RDPSRAPIInvitation: TGUID = '{49174DC6-0731-4B5E-8EE1-83A63D3868FA}';
  CLASS_RDPSRAPIAttendeeManager: TGUID = '{D7B13A01-F7D4-42A6-8595-12FC8C24E851}';
  CLASS_RDPSRAPIAttendee: TGUID = '{74F93BB5-755F-488E-8A29-2390108AEF55}';
  CLASS_RDPSRAPIAttendeeDisconnectInfo: TGUID = '{B47D7250-5BDB-405D-B487-CAAD9C56F4F8}';
  CLASS_RDPSRAPIApplicationFilter: TGUID = '{E35ACE89-C7E8-427E-A4F9-B9DA072826BD}';
  CLASS_RDPSRAPIApplicationList: TGUID = '{9E31C815-7433-4876-97FB-ED59FE2BAA22}';
  CLASS_RDPSRAPIApplication: TGUID = '{C116A484-4B25-4B9F-8A54-B934B06E57FA}';
  CLASS_RDPSRAPIWindowList: TGUID = '{9C21E2B8-5DD4-42CC-81BA-1C099852E6FA}';
  CLASS_RDPSRAPIWindow: TGUID = '{03CF46DB-CE45-4D36-86ED-ED28B74398BF}';
  CLASS_RDPSRAPITcpConnectionInfo: TGUID = '{BE49DB3F-EBB6-4278-8CE0-D5455833EAEE}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum __MIDL___MIDL_itf_rdpencomapi_0000_0001_0001
type
  __MIDL___MIDL_itf_rdpencomapi_0000_0001_0001 = TOleEnum;
const
  CTRL_LEVEL_MIN = $00000000;
  CTRL_LEVEL_INVALID = $00000000;
  CTRL_LEVEL_NONE = $00000001;
  CTRL_LEVEL_VIEW = $00000002;
  CTRL_LEVEL_INTERACTIVE = $00000003;
  CTRL_LEVEL_MAX = $00000003;

// Constants for enum __MIDL___MIDL_itf_rdpencomapi_0000_0001_0002
type
  __MIDL___MIDL_itf_rdpencomapi_0000_0001_0002 = TOleEnum;
const
  ATTENDEE_DISCONNECT_REASON_MIN = $00000000;
  ATTENDEE_DISCONNECT_REASON_APP = $00000000;
  ATTENDEE_DISCONNECT_REASON_ERR = $00000001;
  ATTENDEE_DISCONNECT_REASON_CLI = $00000002;
  ATTENDEE_DISCONNECT_REASON_MAX = $00000002;

// Constants for enum __MIDL___MIDL_itf_rdpencomapi_0000_0001_0003
type
  __MIDL___MIDL_itf_rdpencomapi_0000_0001_0003 = TOleEnum;
const
  CHANNEL_PRIORITY_LO = $00000000;
  CHANNEL_PRIORITY_MED = $00000001;
  CHANNEL_PRIORITY_HI = $00000002;

// Constants for enum __MIDL___MIDL_itf_rdpencomapi_0000_0001_0004
type
  __MIDL___MIDL_itf_rdpencomapi_0000_0001_0004 = TOleEnum;
const
  CHANNEL_FLAGS_LEGACY = $00000001;
  CHANNEL_FLAGS_UNCOMPRESSED = $00000002;

// Constants for enum __MIDL___MIDL_itf_rdpencomapi_0000_0001_0005
type
  __MIDL___MIDL_itf_rdpencomapi_0000_0001_0005 = TOleEnum;
const
  CHANNEL_ACCESS_ENUM_NONE = $00000000;
  CHANNEL_ACCESS_ENUM_SENDRECEIVE = $00000001;

// Constants for enum __MIDL___MIDL_itf_rdpencomapi_0000_0001_0006
type
  __MIDL___MIDL_itf_rdpencomapi_0000_0001_0006 = TOleEnum;
const
  CONST_MAX_CHANNEL_MESSAGE_SIZE = $00000400;
  CONST_MAX_CHANNEL_NAME_LEN = $00000008;
  CONST_MAX_LEGACY_CHANNEL_MESSAGE_SIZE = $00064000;
  CONST_ATTENDEE_ID_EVERYONE = $FFFFFFFF;
  CONST_ATTENDEE_ID_HOST = $00000000;
  CONST_CONN_INTERVAL = $00000032;

// Constants for enum __MIDL___MIDL_itf_rdpencomapi_0000_0001_0007
type
  __MIDL___MIDL_itf_rdpencomapi_0000_0001_0007 = TOleEnum;
const
  ATTENDEE_FLAGS_LOCAL = $00000001;

// Constants for enum __MIDL___MIDL_itf_rdpencomapi_0000_0001_0008
type
  __MIDL___MIDL_itf_rdpencomapi_0000_0001_0008 = TOleEnum;
const
  WND_FLAG_PRIVILEGED = $00000001;

// Constants for enum __MIDL___MIDL_itf_rdpencomapi_0000_0001_0009
type
  __MIDL___MIDL_itf_rdpencomapi_0000_0001_0009 = TOleEnum;
const
  APP_FLAG_PRIVILEGED = $00000001;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  _IRDPSessionEvents = dispinterface;
  IRDPSRAPIApplication = interface;
  IRDPSRAPIApplicationDisp = dispinterface;
  IRDPSRAPIWindowList = interface;
  IRDPSRAPIWindowListDisp = dispinterface;
  IRDPSRAPIWindow = interface;
  IRDPSRAPIWindowDisp = dispinterface;
  IRDPSRAPIApplicationList = interface;
  IRDPSRAPIApplicationListDisp = dispinterface;
  IRDPSRAPIApplicationFilter = interface;
  IRDPSRAPIApplicationFilterDisp = dispinterface;
  IRDPSRAPISessionProperties = interface;
  IRDPSRAPISessionPropertiesDisp = dispinterface;
  IRDPSRAPIInvitation = interface;
  IRDPSRAPIInvitationDisp = dispinterface;
  IRDPSRAPIInvitationManager = interface;
  IRDPSRAPIInvitationManagerDisp = dispinterface;
  IRDPSRAPITcpConnectionInfo = interface;
  IRDPSRAPITcpConnectionInfoDisp = dispinterface;
  IRDPSRAPIAttendee = interface;
  IRDPSRAPIAttendeeDisp = dispinterface;
  IRDPSRAPIAttendeeManager = interface;
  IRDPSRAPIAttendeeManagerDisp = dispinterface;
  IRDPSRAPIAttendeeDisconnectInfo = interface;
  IRDPSRAPIAttendeeDisconnectInfoDisp = dispinterface;
  IRDPSRAPIVirtualChannel = interface;
  IRDPSRAPIVirtualChannelDisp = dispinterface;
  IRDPSRAPIVirtualChannelManager = interface;
  IRDPSRAPIVirtualChannelManagerDisp = dispinterface;
  IRDPSRAPISharingSession = interface;
  IRDPSRAPISharingSessionDisp = dispinterface;
  IRDPSRAPIViewer = interface;
  IRDPSRAPIViewerDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  RDPViewer = IRDPSRAPIViewer;
  RDPSession = IRDPSRAPISharingSession;
  RDPSRAPISessionProperties = IRDPSRAPISessionProperties;
  RDPSRAPIInvitationManager = IRDPSRAPIInvitationManager;
  RDPSRAPIInvitation = IRDPSRAPIInvitation;
  RDPSRAPIAttendeeManager = IRDPSRAPIAttendeeManager;
  RDPSRAPIAttendee = IRDPSRAPIAttendee;
  RDPSRAPIAttendeeDisconnectInfo = IRDPSRAPIAttendeeDisconnectInfo;
  RDPSRAPIApplicationFilter = IRDPSRAPIApplicationFilter;
  RDPSRAPIApplicationList = IRDPSRAPIApplicationList;
  RDPSRAPIApplication = IRDPSRAPIApplication;
  RDPSRAPIWindowList = IRDPSRAPIWindowList;
  RDPSRAPIWindow = IRDPSRAPIWindow;
  RDPSRAPITcpConnectionInfo = IRDPSRAPITcpConnectionInfo;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//

  CTRL_LEVEL = __MIDL___MIDL_itf_rdpencomapi_0000_0001_0001; 
  ATTENDEE_DISCONNECT_REASON = __MIDL___MIDL_itf_rdpencomapi_0000_0001_0002; 
  CHANNEL_PRIORITY = __MIDL___MIDL_itf_rdpencomapi_0000_0001_0003; 
  CHANNEL_FLAGS = __MIDL___MIDL_itf_rdpencomapi_0000_0001_0004; 
  CHANNEL_ACCESS_ENUM = __MIDL___MIDL_itf_rdpencomapi_0000_0001_0005; 
  RDPENCOMAPI_CONSTANTS = __MIDL___MIDL_itf_rdpencomapi_0000_0001_0006; 
  RDPENCOMAPI_ATTENDEE_FLAGS = __MIDL___MIDL_itf_rdpencomapi_0000_0001_0007; 
  RDPSRAPI_WND_FLAGS = __MIDL___MIDL_itf_rdpencomapi_0000_0001_0008; 
  RDPSRAPI_APP_FLAGS = __MIDL___MIDL_itf_rdpencomapi_0000_0001_0009; 

// *********************************************************************//
// DispIntf:  _IRDPSessionEvents
// Flags:     (4096) Dispatchable
// GUID:      {98A97042-6698-40E9-8EFD-B3200990004B}
// *********************************************************************//
  _IRDPSessionEvents = dispinterface
    ['{98A97042-6698-40E9-8EFD-B3200990004B}']
    procedure OnAttendeeConnected(const pAttendee: IDispatch); dispid 301;
    procedure OnAttendeeDisconnected(const pDisconnectInfo: IDispatch); dispid 302;
    procedure OnAttendeeUpdate(const pAttendee: IDispatch); dispid 303;
    procedure OnConnectionEstablished; dispid 305;
    procedure OnConnectionFailed; dispid 308;
    procedure OnConnectionTerminated(discReason: Integer; ExtendedInfo: Integer); dispid 306;
    procedure OnConnectionAuthenticated; dispid 307;
    procedure OnError(ErrorInfo: OleVariant); dispid 304;
    procedure OnApplicationOpen(const pApplication: IDispatch); dispid 316;
    procedure OnApplicationClose(const pApplication: IDispatch); dispid 317;
    procedure OnApplicationUpdate(const pApplication: IDispatch); dispid 318;
    procedure OnWindowOpen(const pWindow: IDispatch); dispid 319;
    procedure OnWindowClose(const pWindow: IDispatch); dispid 320;
    procedure OnWindowUpdate(const pWindow: IDispatch); dispid 321;
    procedure OnControlLevelChangeRequest(const pAttendee: IDispatch; RequestedLevel: CTRL_LEVEL); dispid 309;
    procedure OnGraphicsStreamPaused; dispid 310;
    procedure OnGraphicsStreamResumed; dispid 311;
    procedure OnChannelDataReceived(const pChannel: IUnknown; lAttendeeId: Integer; 
                                    const bstrData: WideString); dispid 314;
    procedure OnChannelDataSent(const pChannel: IUnknown; lAttendeeId: Integer; BytesSent: Integer); dispid 315;
    procedure OnSharedRectChanged(left: Integer; top: Integer; right: Integer; bottom: Integer); dispid 323;
    procedure OnFocusReleased(iDirection: SYSINT); dispid 324;
    procedure OnSharedDesktopSettingsChanged(width: Integer; height: Integer; colordepth: Integer); dispid 325;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIApplication
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {41E7A09D-EB7A-436E-935D-780CA2628324}
// *********************************************************************//
  IRDPSRAPIApplication = interface(IDispatch)
    ['{41E7A09D-EB7A-436E-935D-780CA2628324}']
    function Get_Windows(out pWindowList: IRDPSRAPIWindowList): HResult; stdcall;
    function Get_Id(out pRetVal: Integer): HResult; stdcall;
    function Get_Shared(out pRetVal: WordBool): HResult; stdcall;
    function Set_Shared(pRetVal: WordBool): HResult; stdcall;
    function Get_Name(out pRetVal: WideString): HResult; stdcall;
    function Get_Flags(out pdwFlags: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIApplicationDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {41E7A09D-EB7A-436E-935D-780CA2628324}
// *********************************************************************//
  IRDPSRAPIApplicationDisp = dispinterface
    ['{41E7A09D-EB7A-436E-935D-780CA2628324}']
    property Windows: IRDPSRAPIWindowList readonly dispid 0;
    property Id: Integer readonly dispid 201;
    function Shared: WordBool; dispid 220;
    property Name: WideString readonly dispid 214;
    property Flags: LongWord readonly dispid 223;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIWindowList
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8A05CE44-715A-4116-A189-A118F30A07BD}
// *********************************************************************//
  IRDPSRAPIWindowList = interface(IDispatch)
    ['{8A05CE44-715A-4116-A189-A118F30A07BD}']
    function Get__NewEnum(out retval: IUnknown): HResult; stdcall;
    function Get_Item(Item: Integer; out pWindow: IRDPSRAPIWindow): HResult; stdcall;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIWindowListDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8A05CE44-715A-4116-A189-A118F30A07BD}
// *********************************************************************//
  IRDPSRAPIWindowListDisp = dispinterface
    ['{8A05CE44-715A-4116-A189-A118F30A07BD}']
    property _NewEnum: IUnknown readonly dispid -4;
    property Item[Item: Integer]: IRDPSRAPIWindow readonly dispid 0; default;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIWindow
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BEAFE0F9-C77B-4933-BA9F-A24CDDCC27CF}
// *********************************************************************//
  IRDPSRAPIWindow = interface(IDispatch)
    ['{BEAFE0F9-C77B-4933-BA9F-A24CDDCC27CF}']
    function Get_Id(out pRetVal: Integer): HResult; stdcall;
    function Get_Application(out pApplication: IRDPSRAPIApplication): HResult; stdcall;
    function Get_Shared(out pRetVal: WordBool): HResult; stdcall;
    function Set_Shared(pRetVal: WordBool): HResult; stdcall;
    function Get_Name(out pRetVal: WideString): HResult; stdcall;
    function Show: HResult; stdcall;
    function Get_Flags(out pdwFlags: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIWindowDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BEAFE0F9-C77B-4933-BA9F-A24CDDCC27CF}
// *********************************************************************//
  IRDPSRAPIWindowDisp = dispinterface
    ['{BEAFE0F9-C77B-4933-BA9F-A24CDDCC27CF}']
    property Id: Integer readonly dispid 201;
    property Application: IRDPSRAPIApplication readonly dispid 211;
    function Shared: WordBool; dispid 220;
    property Name: WideString readonly dispid 213;
    procedure Show; dispid 114;
    property Flags: LongWord readonly dispid 224;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIApplicationList
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D4B4AEB3-22DC-4837-B3B6-42EA2517849A}
// *********************************************************************//
  IRDPSRAPIApplicationList = interface(IDispatch)
    ['{D4B4AEB3-22DC-4837-B3B6-42EA2517849A}']
    function Get__NewEnum(out retval: IUnknown): HResult; stdcall;
    function Get_Item(Item: Integer; out pApplication: IRDPSRAPIApplication): HResult; stdcall;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIApplicationListDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D4B4AEB3-22DC-4837-B3B6-42EA2517849A}
// *********************************************************************//
  IRDPSRAPIApplicationListDisp = dispinterface
    ['{D4B4AEB3-22DC-4837-B3B6-42EA2517849A}']
    property _NewEnum: IUnknown readonly dispid -4;
    property Item[Item: Integer]: IRDPSRAPIApplication readonly dispid 0; default;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIApplicationFilter
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D20F10CA-6637-4F06-B1D5-277EA7E5160D}
// *********************************************************************//
  IRDPSRAPIApplicationFilter = interface(IDispatch)
    ['{D20F10CA-6637-4F06-B1D5-277EA7E5160D}']
    function Get_Applications(out pApplications: IRDPSRAPIApplicationList): HResult; stdcall;
    function Get_Windows(out pWindows: IRDPSRAPIWindowList): HResult; stdcall;
    function Get_Enabled(out pRetVal: WordBool): HResult; stdcall;
    function Set_Enabled(pRetVal: WordBool): HResult; stdcall;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIApplicationFilterDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D20F10CA-6637-4F06-B1D5-277EA7E5160D}
// *********************************************************************//
  IRDPSRAPIApplicationFilterDisp = dispinterface
    ['{D20F10CA-6637-4F06-B1D5-277EA7E5160D}']
    property Applications: IRDPSRAPIApplicationList readonly dispid 217;
    property Windows: IRDPSRAPIWindowList readonly dispid 216;
    function Enabled: WordBool; dispid 219;
  end;

// *********************************************************************//
// Interface: IRDPSRAPISessionProperties
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {339B24F2-9BC0-4F16-9AAC-F165433D13D4}
// *********************************************************************//
  IRDPSRAPISessionProperties = interface(IDispatch)
    ['{339B24F2-9BC0-4F16-9AAC-F165433D13D4}']
    function Get_Property_(const PropertyName: WideString; out pVal: OleVariant): HResult; stdcall;
    function Set_Property_(const PropertyName: WideString; pVal: OleVariant): HResult; stdcall;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPISessionPropertiesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {339B24F2-9BC0-4F16-9AAC-F165433D13D4}
// *********************************************************************//
  IRDPSRAPISessionPropertiesDisp = dispinterface
    ['{339B24F2-9BC0-4F16-9AAC-F165433D13D4}']
    function Property_(const PropertyName: WideString): OleVariant; dispid 0;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIInvitation
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4FAC1D43-FC51-45BB-B1B4-2B53AA562FA3}
// *********************************************************************//
  IRDPSRAPIInvitation = interface(IDispatch)
    ['{4FAC1D43-FC51-45BB-B1B4-2B53AA562FA3}']
    function Get_ConnectionString(out pbstrVal: WideString): HResult; stdcall;
    function Get_GroupName(out pbstrVal: WideString): HResult; stdcall;
    function Get_Password(out pbstrVal: WideString): HResult; stdcall;
    function Get_AttendeeLimit(out pRetVal: Integer): HResult; stdcall;
    function Set_AttendeeLimit(pRetVal: Integer): HResult; stdcall;
    function Get_Revoked(out pRetVal: WordBool): HResult; stdcall;
    function Set_Revoked(pRetVal: WordBool): HResult; stdcall;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIInvitationDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4FAC1D43-FC51-45BB-B1B4-2B53AA562FA3}
// *********************************************************************//
  IRDPSRAPIInvitationDisp = dispinterface
    ['{4FAC1D43-FC51-45BB-B1B4-2B53AA562FA3}']
    property ConnectionString: WideString readonly dispid 232;
    property GroupName: WideString readonly dispid 233;
    property Password: WideString readonly dispid 234;
    function AttendeeLimit: Integer; dispid 235;
    function Revoked: WordBool; dispid 236;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIInvitationManager
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4722B049-92C3-4C2D-8A65-F7348F644DCF}
// *********************************************************************//
  IRDPSRAPIInvitationManager = interface(IDispatch)
    ['{4722B049-92C3-4C2D-8A65-F7348F644DCF}']
    function Get__NewEnum(out retval: IUnknown): HResult; stdcall;
    function Get_Item(Item: OleVariant; out ppInvitation: IRDPSRAPIInvitation): HResult; stdcall;
    function Get_Count(out pRetVal: Integer): HResult; stdcall;
    function CreateInvitation(const bstrAuthString: WideString; const bstrGroupName: WideString; 
                              const bstrPassword: WideString; AttendeeLimit: Integer; 
                              out ppInvitation: IRDPSRAPIInvitation): HResult; stdcall;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIInvitationManagerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4722B049-92C3-4C2D-8A65-F7348F644DCF}
// *********************************************************************//
  IRDPSRAPIInvitationManagerDisp = dispinterface
    ['{4722B049-92C3-4C2D-8A65-F7348F644DCF}']
    property _NewEnum: IUnknown readonly dispid -4;
    property Item[Item: OleVariant]: IRDPSRAPIInvitation readonly dispid 0; default;
    property Count: Integer readonly dispid 244;
    function CreateInvitation(const bstrAuthString: WideString; const bstrGroupName: WideString; 
                              const bstrPassword: WideString; AttendeeLimit: Integer): IRDPSRAPIInvitation; dispid 107;
  end;

// *********************************************************************//
// Interface: IRDPSRAPITcpConnectionInfo
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F74049A4-3D06-4028-8193-0A8C29BC2452}
// *********************************************************************//
  IRDPSRAPITcpConnectionInfo = interface(IDispatch)
    ['{F74049A4-3D06-4028-8193-0A8C29BC2452}']
    function Get_Protocol(out plProtocol: Integer): HResult; stdcall;
    function Get_LocalPort(out plPort: Integer): HResult; stdcall;
    function Get_LocalIP(out pbsrLocalIP: WideString): HResult; stdcall;
    function Get_PeerPort(out plPort: Integer): HResult; stdcall;
    function Get_PeerIP(out pbstrIP: WideString): HResult; stdcall;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPITcpConnectionInfoDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F74049A4-3D06-4028-8193-0A8C29BC2452}
// *********************************************************************//
  IRDPSRAPITcpConnectionInfoDisp = dispinterface
    ['{F74049A4-3D06-4028-8193-0A8C29BC2452}']
    property Protocol: Integer readonly dispid 225;
    property LocalPort: Integer readonly dispid 226;
    property LocalIP: WideString readonly dispid 227;
    property PeerPort: Integer readonly dispid 228;
    property PeerIP: WideString readonly dispid 229;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIAttendee
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EC0671B3-1B78-4B80-A464-9132247543E3}
// *********************************************************************//
  IRDPSRAPIAttendee = interface(IDispatch)
    ['{EC0671B3-1B78-4B80-A464-9132247543E3}']
    function Get_Id(out pId: Integer): HResult; stdcall;
    function Get_RemoteName(out pVal: WideString): HResult; stdcall;
    function Get_ControlLevel(out pVal: CTRL_LEVEL): HResult; stdcall;
    function Set_ControlLevel(pVal: CTRL_LEVEL): HResult; stdcall;
    function Get_Invitation(out ppVal: IRDPSRAPIInvitation): HResult; stdcall;
    function TerminateConnection: HResult; stdcall;
    function Get_Flags(out plFlags: Integer): HResult; stdcall;
    function Get_ConnectivityInfo(out ppVal: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIAttendeeDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EC0671B3-1B78-4B80-A464-9132247543E3}
// *********************************************************************//
  IRDPSRAPIAttendeeDisp = dispinterface
    ['{EC0671B3-1B78-4B80-A464-9132247543E3}']
    property Id: Integer readonly dispid 201;
    property RemoteName: WideString readonly dispid 243;
    function ControlLevel: CTRL_LEVEL; dispid 242;
    property Invitation: IRDPSRAPIInvitation readonly dispid 205;
    procedure TerminateConnection; dispid 106;
    property Flags: Integer readonly dispid 230;
    property ConnectivityInfo: IUnknown readonly dispid 231;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIAttendeeManager
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BA3A37E8-33DA-4749-8DA0-07FA34DA7944}
// *********************************************************************//
  IRDPSRAPIAttendeeManager = interface(IDispatch)
    ['{BA3A37E8-33DA-4749-8DA0-07FA34DA7944}']
    function Get__NewEnum(out retval: IUnknown): HResult; stdcall;
    function Get_Item(Id: Integer; out ppItem: IRDPSRAPIAttendee): HResult; stdcall;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIAttendeeManagerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BA3A37E8-33DA-4749-8DA0-07FA34DA7944}
// *********************************************************************//
  IRDPSRAPIAttendeeManagerDisp = dispinterface
    ['{BA3A37E8-33DA-4749-8DA0-07FA34DA7944}']
    property _NewEnum: IUnknown readonly dispid -4;
    property Item[Id: Integer]: IRDPSRAPIAttendee readonly dispid 0; default;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIAttendeeDisconnectInfo
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C187689F-447C-44A1-9C14-FFFBB3B7EC17}
// *********************************************************************//
  IRDPSRAPIAttendeeDisconnectInfo = interface(IDispatch)
    ['{C187689F-447C-44A1-9C14-FFFBB3B7EC17}']
    function Get_Attendee(out retval: IRDPSRAPIAttendee): HResult; stdcall;
    function Get_Reason(out pReason: ATTENDEE_DISCONNECT_REASON): HResult; stdcall;
    function Get_Code(out pVal: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIAttendeeDisconnectInfoDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C187689F-447C-44A1-9C14-FFFBB3B7EC17}
// *********************************************************************//
  IRDPSRAPIAttendeeDisconnectInfoDisp = dispinterface
    ['{C187689F-447C-44A1-9C14-FFFBB3B7EC17}']
    property Attendee: IRDPSRAPIAttendee readonly dispid 0;
    property Reason: ATTENDEE_DISCONNECT_REASON readonly dispid 240;
    property Code: Integer readonly dispid 241;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIVirtualChannel
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {05E12F95-28B3-4C9A-8780-D0248574A1E0}
// *********************************************************************//
  IRDPSRAPIVirtualChannel = interface(IDispatch)
    ['{05E12F95-28B3-4C9A-8780-D0248574A1E0}']
    function SendData(const bstrData: WideString; lAttendeeId: Integer; ChannelSendFlags: LongWord): HResult; stdcall;
    function SetAccess(lAttendeeId: Integer; AccessType: CHANNEL_ACCESS_ENUM): HResult; stdcall;
    function Get_Name(out pbstrName: WideString): HResult; stdcall;
    function Get_Flags(out plFlags: Integer): HResult; stdcall;
    function Get_Priority(out pPriority: CHANNEL_PRIORITY): HResult; stdcall;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIVirtualChannelDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {05E12F95-28B3-4C9A-8780-D0248574A1E0}
// *********************************************************************//
  IRDPSRAPIVirtualChannelDisp = dispinterface
    ['{05E12F95-28B3-4C9A-8780-D0248574A1E0}']
    procedure SendData(const bstrData: WideString; lAttendeeId: Integer; ChannelSendFlags: LongWord); dispid 110;
    procedure SetAccess(lAttendeeId: Integer; AccessType: CHANNEL_ACCESS_ENUM); dispid 111;
    property Name: WideString readonly dispid 207;
    property Flags: Integer readonly dispid 208;
    property Priority: CHANNEL_PRIORITY readonly dispid 209;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIVirtualChannelManager
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {0D11C661-5D0D-4EE4-89DF-2166AE1FDFED}
// *********************************************************************//
  IRDPSRAPIVirtualChannelManager = interface(IDispatch)
    ['{0D11C661-5D0D-4EE4-89DF-2166AE1FDFED}']
    function Get__NewEnum(out retval: IUnknown): HResult; stdcall;
    function Get_Item(Item: OleVariant; out pChannel: IRDPSRAPIVirtualChannel): HResult; stdcall;
    function CreateVirtualChannel(const bstrChannelName: WideString; Priority: CHANNEL_PRIORITY; 
                                  ChannelFlags: LongWord; out ppChannel: IRDPSRAPIVirtualChannel): HResult; stdcall;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIVirtualChannelManagerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {0D11C661-5D0D-4EE4-89DF-2166AE1FDFED}
// *********************************************************************//
  IRDPSRAPIVirtualChannelManagerDisp = dispinterface
    ['{0D11C661-5D0D-4EE4-89DF-2166AE1FDFED}']
    property _NewEnum: IUnknown readonly dispid -4;
    property Item[Item: OleVariant]: IRDPSRAPIVirtualChannel readonly dispid 0; default;
    function CreateVirtualChannel(const bstrChannelName: WideString; Priority: CHANNEL_PRIORITY; 
                                  ChannelFlags: LongWord): IRDPSRAPIVirtualChannel; dispid 109;
  end;

// *********************************************************************//
// Interface: IRDPSRAPISharingSession
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EEB20886-E470-4CF6-842B-2739C0EC5CFB}
// *********************************************************************//
  IRDPSRAPISharingSession = interface(IDispatch)
    ['{EEB20886-E470-4CF6-842B-2739C0EC5CFB}']
    function Open: HResult; stdcall;
    function Close: HResult; stdcall;
    function Set_colordepth(pColorDepth: Integer): HResult; stdcall;
    function Get_colordepth(out pColorDepth: Integer): HResult; stdcall;
    function Get_Properties(out ppVal: IRDPSRAPISessionProperties): HResult; stdcall;
    function Get_Attendees(out ppVal: IRDPSRAPIAttendeeManager): HResult; stdcall;
    function Get_Invitations(out ppVal: IRDPSRAPIInvitationManager): HResult; stdcall;
    function Get_ApplicationFilter(out ppVal: IRDPSRAPIApplicationFilter): HResult; stdcall;
    function Get_VirtualChannelManager(out ppVal: IRDPSRAPIVirtualChannelManager): HResult; stdcall;
    function Pause: HResult; stdcall;
    function Resume: HResult; stdcall;
    function ConnectToClient(const bstrConnectionString: WideString): HResult; stdcall;
    function SetDesktopSharedRect(left: Integer; top: Integer; right: Integer; bottom: Integer): HResult; stdcall;
    function GetDesktopSharedRect(out pleft: Integer; out ptop: Integer; out pright: Integer; 
                                  out pbottom: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPISharingSessionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EEB20886-E470-4CF6-842B-2739C0EC5CFB}
// *********************************************************************//
  IRDPSRAPISharingSessionDisp = dispinterface
    ['{EEB20886-E470-4CF6-842B-2739C0EC5CFB}']
    procedure Open; dispid 100;
    procedure Close; dispid 101;
    property colordepth: Integer dispid 239;
    property Properties: IRDPSRAPISessionProperties readonly dispid 202;
    property Attendees: IRDPSRAPIAttendeeManager readonly dispid 203;
    property Invitations: IRDPSRAPIInvitationManager readonly dispid 204;
    property ApplicationFilter: IRDPSRAPIApplicationFilter readonly dispid 215;
    property VirtualChannelManager: IRDPSRAPIVirtualChannelManager readonly dispid 206;
    procedure Pause; dispid 112;
    procedure Resume; dispid 113;
    procedure ConnectToClient(const bstrConnectionString: WideString); dispid 117;
    procedure SetDesktopSharedRect(left: Integer; top: Integer; right: Integer; bottom: Integer); dispid 102;
    procedure GetDesktopSharedRect(out pleft: Integer; out ptop: Integer; out pright: Integer; 
                                   out pbottom: Integer); dispid 103;
  end;

// *********************************************************************//
// Interface: IRDPSRAPIViewer
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C6BFCD38-8CE9-404D-8AE8-F31D00C65CB5}
// *********************************************************************//
  IRDPSRAPIViewer = interface(IDispatch)
    ['{C6BFCD38-8CE9-404D-8AE8-F31D00C65CB5}']
    function Connect(const bstrConnectionString: WideString; const bstrName: WideString; 
                     const bstrPassword: WideString): HResult; stdcall;
    function Disconnect: HResult; stdcall;
    function Get_Attendees(out ppVal: IRDPSRAPIAttendeeManager): HResult; stdcall;
    function Get_Invitations(out ppVal: IRDPSRAPIInvitationManager): HResult; stdcall;
    function Get_ApplicationFilter(out ppVal: IRDPSRAPIApplicationFilter): HResult; stdcall;
    function Get_VirtualChannelManager(out ppVal: IRDPSRAPIVirtualChannelManager): HResult; stdcall;
    function Set_SmartSizing(pvbSmartSizing: WordBool): HResult; stdcall;
    function Get_SmartSizing(out pvbSmartSizing: WordBool): HResult; stdcall;
    function RequestControl(CtrlLevel: CTRL_LEVEL): HResult; stdcall;
    function Set_DisconnectedText(const pbstrDisconnectedText: WideString): HResult; stdcall;
    function Get_DisconnectedText(out pbstrDisconnectedText: WideString): HResult; stdcall;
    function RequestColorDepthChange(Bpp: Integer): HResult; stdcall;
    function Get_Properties(out ppVal: IRDPSRAPISessionProperties): HResult; stdcall;
    function StartReverseConnectListener(const bstrConnectionString: WideString; 
                                         const bstrUserName: WideString; 
                                         const bstrPassword: WideString; 
                                         out pbstrReverseConnectString: WideString): HResult; stdcall;
  end;

// *********************************************************************//
// DispIntf:  IRDPSRAPIViewerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C6BFCD38-8CE9-404D-8AE8-F31D00C65CB5}
// *********************************************************************//
  IRDPSRAPIViewerDisp = dispinterface
    ['{C6BFCD38-8CE9-404D-8AE8-F31D00C65CB5}']
    procedure Connect(const bstrConnectionString: WideString; const bstrName: WideString; 
                      const bstrPassword: WideString); dispid 104;
    procedure Disconnect; dispid 105;
    property Attendees: IRDPSRAPIAttendeeManager readonly dispid 203;
    property Invitations: IRDPSRAPIInvitationManager readonly dispid 204;
    property ApplicationFilter: IRDPSRAPIApplicationFilter readonly dispid 215;
    property VirtualChannelManager: IRDPSRAPIVirtualChannelManager readonly dispid 206;
    property SmartSizing: WordBool dispid 238;
    procedure RequestControl(CtrlLevel: CTRL_LEVEL); dispid 108;
    property DisconnectedText: WideString dispid 237;
    procedure RequestColorDepthChange(Bpp: Integer); dispid 115;
    property Properties: IRDPSRAPISessionProperties readonly dispid 202;
    function StartReverseConnectListener(const bstrConnectionString: WideString; 
                                         const bstrUserName: WideString; 
                                         const bstrPassword: WideString): WideString; dispid 116;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TRDPViewer
// Help String      : 
// Default Interface: IRDPSRAPIViewer
// Def. Intf. DISP? : No
// Event   Interface: _IRDPSessionEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TRDPViewerOnAttendeeConnected = procedure(ASender: TObject; const pAttendee: IDispatch) of object;
  TRDPViewerOnAttendeeDisconnected = procedure(ASender: TObject; const pDisconnectInfo: IDispatch) of object;
  TRDPViewerOnAttendeeUpdate = procedure(ASender: TObject; const pAttendee: IDispatch) of object;
  TRDPViewerOnConnectionTerminated = procedure(ASender: TObject; discReason: Integer; 
                                                                 ExtendedInfo: Integer) of object;
  TRDPViewerOnError = procedure(ASender: TObject; ErrorInfo: OleVariant) of object;
  TRDPViewerOnApplicationOpen = procedure(ASender: TObject; const pApplication: IDispatch) of object;
  TRDPViewerOnApplicationClose = procedure(ASender: TObject; const pApplication: IDispatch) of object;
  TRDPViewerOnApplicationUpdate = procedure(ASender: TObject; const pApplication: IDispatch) of object;
  TRDPViewerOnWindowOpen = procedure(ASender: TObject; const pWindow: IDispatch) of object;
  TRDPViewerOnWindowClose = procedure(ASender: TObject; const pWindow: IDispatch) of object;
  TRDPViewerOnWindowUpdate = procedure(ASender: TObject; const pWindow: IDispatch) of object;
  TRDPViewerOnControlLevelChangeRequest = procedure(ASender: TObject; const pAttendee: IDispatch; 
                                                                      RequestedLevel: CTRL_LEVEL) of object;
  TRDPViewerOnChannelDataReceived = procedure(ASender: TObject; const pChannel: IUnknown; 
                                                                lAttendeeId: Integer; 
                                                                const bstrData: WideString) of object;
  TRDPViewerOnChannelDataSent = procedure(ASender: TObject; const pChannel: IUnknown; 
                                                            lAttendeeId: Integer; BytesSent: Integer) of object;
  TRDPViewerOnSharedRectChanged = procedure(ASender: TObject; left: Integer; top: Integer; 
                                                              right: Integer; bottom: Integer) of object;
  TRDPViewerOnFocusReleased = procedure(ASender: TObject; iDirection: SYSINT) of object;
  TRDPViewerOnSharedDesktopSettingsChanged = procedure(ASender: TObject; width: Integer; 
                                                                         height: Integer; 
                                                                         colordepth: Integer) of object;

  TRDPViewer = class(TOleControl)
  private
    FOnAttendeeConnected: TRDPViewerOnAttendeeConnected;
    FOnAttendeeDisconnected: TRDPViewerOnAttendeeDisconnected;
    FOnAttendeeUpdate: TRDPViewerOnAttendeeUpdate;
    FOnConnectionEstablished: TNotifyEvent;
    FOnConnectionFailed: TNotifyEvent;
    FOnConnectionTerminated: TRDPViewerOnConnectionTerminated;
    FOnConnectionAuthenticated: TNotifyEvent;
    FOnError: TRDPViewerOnError;
    FOnApplicationOpen: TRDPViewerOnApplicationOpen;
    FOnApplicationClose: TRDPViewerOnApplicationClose;
    FOnApplicationUpdate: TRDPViewerOnApplicationUpdate;
    FOnWindowOpen: TRDPViewerOnWindowOpen;
    FOnWindowClose: TRDPViewerOnWindowClose;
    FOnWindowUpdate: TRDPViewerOnWindowUpdate;
    FOnControlLevelChangeRequest: TRDPViewerOnControlLevelChangeRequest;
    FOnGraphicsStreamPaused: TNotifyEvent;
    FOnGraphicsStreamResumed: TNotifyEvent;
    FOnChannelDataReceived: TRDPViewerOnChannelDataReceived;
    FOnChannelDataSent: TRDPViewerOnChannelDataSent;
    FOnSharedRectChanged: TRDPViewerOnSharedRectChanged;
    FOnFocusReleased: TRDPViewerOnFocusReleased;
    FOnSharedDesktopSettingsChanged: TRDPViewerOnSharedDesktopSettingsChanged;
    FIntf: IRDPSRAPIViewer;
    function  GetControlInterface: IRDPSRAPIViewer;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function Get_Attendees(out ppVal: IRDPSRAPIAttendeeManager): HResult;
    function Get_Invitations(out ppVal: IRDPSRAPIInvitationManager): HResult;
    function Get_ApplicationFilter(out ppVal: IRDPSRAPIApplicationFilter): HResult;
    function Get_VirtualChannelManager(out ppVal: IRDPSRAPIVirtualChannelManager): HResult;
    function Get_SmartSizing(out pvbSmartSizing: WordBool): HResult;
    function Get_DisconnectedText(out pbstrDisconnectedText: WideString): HResult;
    function Get_Properties(out ppVal: IRDPSRAPISessionProperties): HResult;
  public
    function Connect(const bstrConnectionString: WideString; const bstrName: WideString; 
                     const bstrPassword: WideString): HResult;
    function Disconnect: HResult;
    function RequestControl(CtrlLevel: CTRL_LEVEL): HResult;
    function RequestColorDepthChange(Bpp: Integer): HResult;
    function StartReverseConnectListener(const bstrConnectionString: WideString; 
                                         const bstrUserName: WideString; 
                                         const bstrPassword: WideString; 
                                         out pbstrReverseConnectString: WideString): HResult;
    property  ControlInterface: IRDPSRAPIViewer read GetControlInterface;
    property  DefaultInterface: IRDPSRAPIViewer read GetControlInterface;
  published
    property Anchors;
    property  TabStop;
    property  Align;
    property  DragCursor;
    property  DragMode;
    property  ParentShowHint;
    property  PopupMenu;
    property  ShowHint;
    property  TabOrder;
    property  Visible;
    property  OnDragDrop;
    property  OnDragOver;
    property  OnEndDrag;
    property  OnEnter;
    property  OnExit;
    property  OnStartDrag;
    property OnAttendeeConnected: TRDPViewerOnAttendeeConnected read FOnAttendeeConnected write FOnAttendeeConnected;
    property OnAttendeeDisconnected: TRDPViewerOnAttendeeDisconnected read FOnAttendeeDisconnected write FOnAttendeeDisconnected;
    property OnAttendeeUpdate: TRDPViewerOnAttendeeUpdate read FOnAttendeeUpdate write FOnAttendeeUpdate;
    property OnConnectionEstablished: TNotifyEvent read FOnConnectionEstablished write FOnConnectionEstablished;
    property OnConnectionFailed: TNotifyEvent read FOnConnectionFailed write FOnConnectionFailed;
    property OnConnectionTerminated: TRDPViewerOnConnectionTerminated read FOnConnectionTerminated write FOnConnectionTerminated;
    property OnConnectionAuthenticated: TNotifyEvent read FOnConnectionAuthenticated write FOnConnectionAuthenticated;
    property OnError: TRDPViewerOnError read FOnError write FOnError;
    property OnApplicationOpen: TRDPViewerOnApplicationOpen read FOnApplicationOpen write FOnApplicationOpen;
    property OnApplicationClose: TRDPViewerOnApplicationClose read FOnApplicationClose write FOnApplicationClose;
    property OnApplicationUpdate: TRDPViewerOnApplicationUpdate read FOnApplicationUpdate write FOnApplicationUpdate;
    property OnWindowOpen: TRDPViewerOnWindowOpen read FOnWindowOpen write FOnWindowOpen;
    property OnWindowClose: TRDPViewerOnWindowClose read FOnWindowClose write FOnWindowClose;
    property OnWindowUpdate: TRDPViewerOnWindowUpdate read FOnWindowUpdate write FOnWindowUpdate;
    property OnControlLevelChangeRequest: TRDPViewerOnControlLevelChangeRequest read FOnControlLevelChangeRequest write FOnControlLevelChangeRequest;
    property OnGraphicsStreamPaused: TNotifyEvent read FOnGraphicsStreamPaused write FOnGraphicsStreamPaused;
    property OnGraphicsStreamResumed: TNotifyEvent read FOnGraphicsStreamResumed write FOnGraphicsStreamResumed;
    property OnChannelDataReceived: TRDPViewerOnChannelDataReceived read FOnChannelDataReceived write FOnChannelDataReceived;
    property OnChannelDataSent: TRDPViewerOnChannelDataSent read FOnChannelDataSent write FOnChannelDataSent;
    property OnSharedRectChanged: TRDPViewerOnSharedRectChanged read FOnSharedRectChanged write FOnSharedRectChanged;
    property OnFocusReleased: TRDPViewerOnFocusReleased read FOnFocusReleased write FOnFocusReleased;
    property OnSharedDesktopSettingsChanged: TRDPViewerOnSharedDesktopSettingsChanged read FOnSharedDesktopSettingsChanged write FOnSharedDesktopSettingsChanged;
  end;

// *********************************************************************//
// The Class CoRDPSession provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPISharingSession exposed by              
// the CoClass RDPSession. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSession = class
    class function Create: IRDPSRAPISharingSession;
    class function CreateRemote(const MachineName: string): IRDPSRAPISharingSession;
  end;

// *********************************************************************//
// The Class CoRDPSRAPISessionProperties provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPISessionProperties exposed by              
// the CoClass RDPSRAPISessionProperties. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSRAPISessionProperties = class
    class function Create: IRDPSRAPISessionProperties;
    class function CreateRemote(const MachineName: string): IRDPSRAPISessionProperties;
  end;

// *********************************************************************//
// The Class CoRDPSRAPIInvitationManager provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPIInvitationManager exposed by              
// the CoClass RDPSRAPIInvitationManager. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSRAPIInvitationManager = class
    class function Create: IRDPSRAPIInvitationManager;
    class function CreateRemote(const MachineName: string): IRDPSRAPIInvitationManager;
  end;

// *********************************************************************//
// The Class CoRDPSRAPIInvitation provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPIInvitation exposed by              
// the CoClass RDPSRAPIInvitation. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSRAPIInvitation = class
    class function Create: IRDPSRAPIInvitation;
    class function CreateRemote(const MachineName: string): IRDPSRAPIInvitation;
  end;

// *********************************************************************//
// The Class CoRDPSRAPIAttendeeManager provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPIAttendeeManager exposed by              
// the CoClass RDPSRAPIAttendeeManager. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSRAPIAttendeeManager = class
    class function Create: IRDPSRAPIAttendeeManager;
    class function CreateRemote(const MachineName: string): IRDPSRAPIAttendeeManager;
  end;

// *********************************************************************//
// The Class CoRDPSRAPIAttendee provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPIAttendee exposed by              
// the CoClass RDPSRAPIAttendee. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSRAPIAttendee = class
    class function Create: IRDPSRAPIAttendee;
    class function CreateRemote(const MachineName: string): IRDPSRAPIAttendee;
  end;

// *********************************************************************//
// The Class CoRDPSRAPIAttendeeDisconnectInfo provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPIAttendeeDisconnectInfo exposed by              
// the CoClass RDPSRAPIAttendeeDisconnectInfo. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSRAPIAttendeeDisconnectInfo = class
    class function Create: IRDPSRAPIAttendeeDisconnectInfo;
    class function CreateRemote(const MachineName: string): IRDPSRAPIAttendeeDisconnectInfo;
  end;

// *********************************************************************//
// The Class CoRDPSRAPIApplicationFilter provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPIApplicationFilter exposed by              
// the CoClass RDPSRAPIApplicationFilter. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSRAPIApplicationFilter = class
    class function Create: IRDPSRAPIApplicationFilter;
    class function CreateRemote(const MachineName: string): IRDPSRAPIApplicationFilter;
  end;

// *********************************************************************//
// The Class CoRDPSRAPIApplicationList provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPIApplicationList exposed by              
// the CoClass RDPSRAPIApplicationList. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSRAPIApplicationList = class
    class function Create: IRDPSRAPIApplicationList;
    class function CreateRemote(const MachineName: string): IRDPSRAPIApplicationList;
  end;

// *********************************************************************//
// The Class CoRDPSRAPIApplication provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPIApplication exposed by              
// the CoClass RDPSRAPIApplication. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSRAPIApplication = class
    class function Create: IRDPSRAPIApplication;
    class function CreateRemote(const MachineName: string): IRDPSRAPIApplication;
  end;

// *********************************************************************//
// The Class CoRDPSRAPIWindowList provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPIWindowList exposed by              
// the CoClass RDPSRAPIWindowList. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSRAPIWindowList = class
    class function Create: IRDPSRAPIWindowList;
    class function CreateRemote(const MachineName: string): IRDPSRAPIWindowList;
  end;

// *********************************************************************//
// The Class CoRDPSRAPIWindow provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPIWindow exposed by              
// the CoClass RDPSRAPIWindow. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSRAPIWindow = class
    class function Create: IRDPSRAPIWindow;
    class function CreateRemote(const MachineName: string): IRDPSRAPIWindow;
  end;

// *********************************************************************//
// The Class CoRDPSRAPITcpConnectionInfo provides a Create and CreateRemote method to          
// create instances of the default interface IRDPSRAPITcpConnectionInfo exposed by              
// the CoClass RDPSRAPITcpConnectionInfo. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRDPSRAPITcpConnectionInfo = class
    class function Create: IRDPSRAPITcpConnectionInfo;
    class function CreateRemote(const MachineName: string): IRDPSRAPITcpConnectionInfo;
  end;

procedure Register;

resourcestring
  dtlServerPage = 'Servers';

  dtlOcxPage = 'ActiveX';

implementation

uses ComObj;

procedure TRDPViewer.InitControlData;
const
  CEventDispIDs: array [0..21] of DWORD = (
    $0000012D, $0000012E, $0000012F, $00000131, $00000134, $00000132,
    $00000133, $00000130, $0000013C, $0000013D, $0000013E, $0000013F,
    $00000140, $00000141, $00000135, $00000136, $00000137, $0000013A,
    $0000013B, $00000143, $00000144, $00000145);
  CControlData: TControlData2 = (
    ClassID: '{32BE5ED2-5C86-480F-A914-0FF8885A1B3F}';
    EventIID: '{98A97042-6698-40E9-8EFD-B3200990004B}';
    EventCount: 22;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil (*HR:$80004002*);
    Flags: $00000000;
    Version: 401);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := Cardinal(@@FOnAttendeeConnected) - Cardinal(Self);
end;

procedure TRDPViewer.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IRDPSRAPIViewer;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TRDPViewer.GetControlInterface: IRDPSRAPIViewer;
begin
  CreateControl;
  Result := FIntf;
end;

function TRDPViewer.Get_Attendees(out ppVal: IRDPSRAPIAttendeeManager): HResult;
begin
    Result := DefaultInterface.Get_Attendees(ppVal);
end;

function TRDPViewer.Get_Invitations(out ppVal: IRDPSRAPIInvitationManager): HResult;
begin
    Result := DefaultInterface.Get_Invitations(ppVal);
end;

function TRDPViewer.Get_ApplicationFilter(out ppVal: IRDPSRAPIApplicationFilter): HResult;
begin
    Result := DefaultInterface.Get_ApplicationFilter(ppVal);
end;

function TRDPViewer.Get_VirtualChannelManager(out ppVal: IRDPSRAPIVirtualChannelManager): HResult;
begin
    Result := DefaultInterface.Get_VirtualChannelManager(ppVal);
end;

function TRDPViewer.Get_SmartSizing(out pvbSmartSizing: WordBool): HResult;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.SmartSizing;
end;

function TRDPViewer.Get_DisconnectedText(out pbstrDisconnectedText: WideString): HResult;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.DisconnectedText;
end;

function TRDPViewer.Get_Properties(out ppVal: IRDPSRAPISessionProperties): HResult;
begin
    Result := DefaultInterface.Get_Properties(ppVal);
end;

function TRDPViewer.Connect(const bstrConnectionString: WideString; const bstrName: WideString; 
                            const bstrPassword: WideString): HResult;
begin
  Result := DefaultInterface.Connect(bstrConnectionString, bstrName, bstrPassword);
end;

function TRDPViewer.Disconnect: HResult;
begin
  Result := DefaultInterface.Disconnect;
end;

function TRDPViewer.RequestControl(CtrlLevel: CTRL_LEVEL): HResult;
begin
  Result := DefaultInterface.RequestControl(CtrlLevel);
end;

function TRDPViewer.RequestColorDepthChange(Bpp: Integer): HResult;
begin
  Result := DefaultInterface.RequestColorDepthChange(Bpp);
end;

function TRDPViewer.StartReverseConnectListener(const bstrConnectionString: WideString; 
                                                const bstrUserName: WideString; 
                                                const bstrPassword: WideString; 
                                                out pbstrReverseConnectString: WideString): HResult;
begin
  Result := DefaultInterface.StartReverseConnectListener(bstrConnectionString, bstrUserName, 
                                                         bstrPassword, pbstrReverseConnectString);
end;

class function CoRDPSession.Create: IRDPSRAPISharingSession;
begin
  Result := CreateComObject(CLASS_RDPSession) as IRDPSRAPISharingSession;
end;

class function CoRDPSession.CreateRemote(const MachineName: string): IRDPSRAPISharingSession;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSession) as IRDPSRAPISharingSession;
end;

class function CoRDPSRAPISessionProperties.Create: IRDPSRAPISessionProperties;
begin
  Result := CreateComObject(CLASS_RDPSRAPISessionProperties) as IRDPSRAPISessionProperties;
end;

class function CoRDPSRAPISessionProperties.CreateRemote(const MachineName: string): IRDPSRAPISessionProperties;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSRAPISessionProperties) as IRDPSRAPISessionProperties;
end;

class function CoRDPSRAPIInvitationManager.Create: IRDPSRAPIInvitationManager;
begin
  Result := CreateComObject(CLASS_RDPSRAPIInvitationManager) as IRDPSRAPIInvitationManager;
end;

class function CoRDPSRAPIInvitationManager.CreateRemote(const MachineName: string): IRDPSRAPIInvitationManager;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSRAPIInvitationManager) as IRDPSRAPIInvitationManager;
end;

class function CoRDPSRAPIInvitation.Create: IRDPSRAPIInvitation;
begin
  Result := CreateComObject(CLASS_RDPSRAPIInvitation) as IRDPSRAPIInvitation;
end;

class function CoRDPSRAPIInvitation.CreateRemote(const MachineName: string): IRDPSRAPIInvitation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSRAPIInvitation) as IRDPSRAPIInvitation;
end;

class function CoRDPSRAPIAttendeeManager.Create: IRDPSRAPIAttendeeManager;
begin
  Result := CreateComObject(CLASS_RDPSRAPIAttendeeManager) as IRDPSRAPIAttendeeManager;
end;

class function CoRDPSRAPIAttendeeManager.CreateRemote(const MachineName: string): IRDPSRAPIAttendeeManager;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSRAPIAttendeeManager) as IRDPSRAPIAttendeeManager;
end;

class function CoRDPSRAPIAttendee.Create: IRDPSRAPIAttendee;
begin
  Result := CreateComObject(CLASS_RDPSRAPIAttendee) as IRDPSRAPIAttendee;
end;

class function CoRDPSRAPIAttendee.CreateRemote(const MachineName: string): IRDPSRAPIAttendee;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSRAPIAttendee) as IRDPSRAPIAttendee;
end;

class function CoRDPSRAPIAttendeeDisconnectInfo.Create: IRDPSRAPIAttendeeDisconnectInfo;
begin
  Result := CreateComObject(CLASS_RDPSRAPIAttendeeDisconnectInfo) as IRDPSRAPIAttendeeDisconnectInfo;
end;

class function CoRDPSRAPIAttendeeDisconnectInfo.CreateRemote(const MachineName: string): IRDPSRAPIAttendeeDisconnectInfo;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSRAPIAttendeeDisconnectInfo) as IRDPSRAPIAttendeeDisconnectInfo;
end;

class function CoRDPSRAPIApplicationFilter.Create: IRDPSRAPIApplicationFilter;
begin
  Result := CreateComObject(CLASS_RDPSRAPIApplicationFilter) as IRDPSRAPIApplicationFilter;
end;

class function CoRDPSRAPIApplicationFilter.CreateRemote(const MachineName: string): IRDPSRAPIApplicationFilter;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSRAPIApplicationFilter) as IRDPSRAPIApplicationFilter;
end;

class function CoRDPSRAPIApplicationList.Create: IRDPSRAPIApplicationList;
begin
  Result := CreateComObject(CLASS_RDPSRAPIApplicationList) as IRDPSRAPIApplicationList;
end;

class function CoRDPSRAPIApplicationList.CreateRemote(const MachineName: string): IRDPSRAPIApplicationList;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSRAPIApplicationList) as IRDPSRAPIApplicationList;
end;

class function CoRDPSRAPIApplication.Create: IRDPSRAPIApplication;
begin
  Result := CreateComObject(CLASS_RDPSRAPIApplication) as IRDPSRAPIApplication;
end;

class function CoRDPSRAPIApplication.CreateRemote(const MachineName: string): IRDPSRAPIApplication;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSRAPIApplication) as IRDPSRAPIApplication;
end;

class function CoRDPSRAPIWindowList.Create: IRDPSRAPIWindowList;
begin
  Result := CreateComObject(CLASS_RDPSRAPIWindowList) as IRDPSRAPIWindowList;
end;

class function CoRDPSRAPIWindowList.CreateRemote(const MachineName: string): IRDPSRAPIWindowList;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSRAPIWindowList) as IRDPSRAPIWindowList;
end;

class function CoRDPSRAPIWindow.Create: IRDPSRAPIWindow;
begin
  Result := CreateComObject(CLASS_RDPSRAPIWindow) as IRDPSRAPIWindow;
end;

class function CoRDPSRAPIWindow.CreateRemote(const MachineName: string): IRDPSRAPIWindow;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSRAPIWindow) as IRDPSRAPIWindow;
end;

class function CoRDPSRAPITcpConnectionInfo.Create: IRDPSRAPITcpConnectionInfo;
begin
  Result := CreateComObject(CLASS_RDPSRAPITcpConnectionInfo) as IRDPSRAPITcpConnectionInfo;
end;

class function CoRDPSRAPITcpConnectionInfo.CreateRemote(const MachineName: string): IRDPSRAPITcpConnectionInfo;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RDPSRAPITcpConnectionInfo) as IRDPSRAPITcpConnectionInfo;
end;

procedure Register;
begin
  RegisterComponents(dtlOcxPage, [TRDPViewer]);
end;

end.
