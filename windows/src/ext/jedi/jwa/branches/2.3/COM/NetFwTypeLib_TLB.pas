unit NetFwTypeLib_TLB;

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
// File generated on 04.07.2008 19:32:40 from Type Library described below.

// ************************************************************************  //
// Type Lib: netfw.tlb (1)
// LIBID: {DB4F3345-3EF8-45ED-B976-25A6D3B81B71}
// LCID: 0
// Helpfile: 
// HelpString: 
// DepndLst:
//   (1) v2.0 stdole, (C:\Windows\system32\stdole2.tlb)
// Errors:
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Parameter 'Type' of INetFwService.Type changed to 'Type_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Parameter 'Type' of INetFwProfile.Type changed to 'Type_'
//   Hint: Parameter 'Type' of INetFwMgr.IsIcmpTypeAllowed changed to 'Type_'
//   Error creating palette bitmap of (TNetFwRule) : Server C:\Windows\system32\FirewallAPI.dll contains no icons
//   Error creating palette bitmap of (TNetFwOpenPort) : Server C:\Windows\system32\FirewallAPI.dll contains no icons
//   Error creating palette bitmap of (TNetFwAuthorizedApplication) : Server C:\Windows\system32\FirewallAPI.dll contains no icons
//   Error creating palette bitmap of (TNetFwPolicy2) : Server C:\Windows\system32\FirewallAPI.dll contains no icons
//   Error creating palette bitmap of (TNetFwMgr) : Server C:\Windows\system32\FirewallAPI.dll contains no icons
// ************************************************************************ //
// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$IFDEF DELPHI6_UP}
{$WARN SYMBOL_PLATFORM OFF}
{$VARPROPSETTER ON}
{$ENDIF}
{$WRITEABLECONST ON}
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, StdVCL{, Variants};
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  NetFwPublicTypeLibMajorVersion = 1;
  NetFwPublicTypeLibMinorVersion = 0;

  LIBID_NetFwPublicTypeLib: TGUID = '{DB4F3345-3EF8-45ED-B976-25A6D3B81B71}';

  IID_INetFwRemoteAdminSettings: TGUID = '{D4BECDDF-6F73-4A83-B832-9C66874CD20E}';
  IID_INetFwIcmpSettings: TGUID = '{A6207B2E-7CDD-426A-951E-5E1CBC5AFEAD}';
  IID_INetFwOpenPort: TGUID = '{E0483BA0-47FF-4D9C-A6D6-7741D0B195F7}';
  IID_INetFwOpenPorts: TGUID = '{C0E9D7FA-E07E-430A-B19A-090CE82D92E2}';
  IID_INetFwService: TGUID = '{79FD57C8-908E-4A36-9888-D5B3F0A444CF}';
  IID_INetFwServices: TGUID = '{79649BB4-903E-421B-94C9-79848E79F6EE}';
  IID_INetFwAuthorizedApplication: TGUID = '{B5E64FFA-C2C5-444E-A301-FB5E00018050}';
  IID_INetFwAuthorizedApplications: TGUID = '{644EFD52-CCF9-486C-97A2-39F352570B30}';
  IID_INetFwServiceRestriction: TGUID = '{8267BBE3-F890-491C-B7B6-2DB1EF0E5D2B}';
  IID_INetFwRules: TGUID = '{9C4C6277-5027-441E-AFAE-CA1F542DA009}';
  IID_INetFwRule: TGUID = '{AF230D27-BABA-4E42-ACED-F524F22CFCE2}';
  IID_INetFwProfile: TGUID = '{174A0DDA-E9F9-449D-993B-21AB667CA456}';
  IID_INetFwPolicy: TGUID = '{D46D2478-9AC9-4008-9DC7-5563CE5536CC}';
  IID_INetFwPolicy2: TGUID = '{98325047-C671-4174-8D81-DEFCD3F03186}';
  IID_INetFwMgr: TGUID = '{F7898AF5-CAC4-4632-A2EC-DA06E5111AF2}';
  CLASS_NetFwRule: TGUID = '{2C5BC43E-3369-4C33-AB0C-BE9469677AF4}';
  CLASS_NetFwOpenPort: TGUID = '{0CA545C6-37AD-4A6C-BF92-9F7610067EF5}';
  CLASS_NetFwAuthorizedApplication: TGUID = '{EC9846B3-2762-4A6B-A214-6ACB603462D2}';
  CLASS_NetFwPolicy2: TGUID = '{E2B3C97F-6AE1-41AC-817A-F6F92166D7DD}';
  CLASS_NetFwMgr: TGUID = '{304CE942-6E39-40D8-943A-B913C40C9CD4}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum NET_FW_IP_VERSION_
type
  NET_FW_IP_VERSION_ = TOleEnum;
const
  NET_FW_IP_VERSION_V4 = $00000000;
  NET_FW_IP_VERSION_V6 = $00000001;
  NET_FW_IP_VERSION_ANY = $00000002;
  NET_FW_IP_VERSION_MAX = $00000003;

// Constants for enum NET_FW_SCOPE_
type
  NET_FW_SCOPE_ = TOleEnum;
const
  NET_FW_SCOPE_ALL = $00000000;
  NET_FW_SCOPE_LOCAL_SUBNET = $00000001;
  NET_FW_SCOPE_CUSTOM = $00000002;
  NET_FW_SCOPE_MAX = $00000003;

// Constants for enum NET_FW_IP_PROTOCOL_
type
  NET_FW_IP_PROTOCOL_ = TOleEnum;
const
  NET_FW_IP_PROTOCOL_TCP = $00000006;
  NET_FW_IP_PROTOCOL_UDP = $00000011;
  NET_FW_IP_PROTOCOL_ANY = $00000100;

// Constants for enum NET_FW_SERVICE_TYPE_
type
  NET_FW_SERVICE_TYPE_ = TOleEnum;
const
  NET_FW_SERVICE_FILE_AND_PRINT = $00000000;
  NET_FW_SERVICE_UPNP = $00000001;
  NET_FW_SERVICE_REMOTE_DESKTOP = $00000002;
  NET_FW_SERVICE_NONE = $00000003;
  NET_FW_SERVICE_TYPE_MAX = $00000004;

// Constants for enum NET_FW_RULE_DIRECTION_
type
  NET_FW_RULE_DIRECTION_ = TOleEnum;
const
  NET_FW_RULE_DIR_IN = $00000001;
  NET_FW_RULE_DIR_OUT = $00000002;
  NET_FW_RULE_DIR_MAX = $00000003;

// Constants for enum NET_FW_ACTION_
type
  NET_FW_ACTION_ = TOleEnum;
const
  NET_FW_ACTION_BLOCK = $00000000;
  NET_FW_ACTION_ALLOW = $00000001;
  NET_FW_ACTION_MAX = $00000002;

// Constants for enum NET_FW_PROFILE_TYPE_
type
  NET_FW_PROFILE_TYPE_ = TOleEnum;
const
  NET_FW_PROFILE_DOMAIN = $00000000;
  NET_FW_PROFILE_STANDARD = $00000001;
  NET_FW_PROFILE_CURRENT = $00000002;
  NET_FW_PROFILE_TYPE_MAX = $00000003;

// Constants for enum NET_FW_PROFILE_TYPE2_
type
  NET_FW_PROFILE_TYPE2_ = TOleEnum;
const
  NET_FW_PROFILE2_DOMAIN = $00000001;
  NET_FW_PROFILE2_PRIVATE = $00000002;
  NET_FW_PROFILE2_PUBLIC = $00000004;
  NET_FW_PROFILE2_ALL = $7FFFFFFF;

// Constants for enum NET_FW_MODIFY_STATE_
type
  NET_FW_MODIFY_STATE_ = TOleEnum;
const
  NET_FW_MODIFY_STATE_OK = $00000000;
  NET_FW_MODIFY_STATE_GP_OVERRIDE = $00000001;
  NET_FW_MODIFY_STATE_INBOUND_BLOCKED = $00000002;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  INetFwRemoteAdminSettings = interface;
  INetFwRemoteAdminSettingsDisp = dispinterface;
  INetFwIcmpSettings = interface;
  INetFwIcmpSettingsDisp = dispinterface;
  INetFwOpenPort = interface;
  INetFwOpenPortDisp = dispinterface;
  INetFwOpenPorts = interface;
  INetFwOpenPortsDisp = dispinterface;
  INetFwService = interface;
  INetFwServiceDisp = dispinterface;
  INetFwServices = interface;
  INetFwServicesDisp = dispinterface;
  INetFwAuthorizedApplication = interface;
  INetFwAuthorizedApplicationDisp = dispinterface;
  INetFwAuthorizedApplications = interface;
  INetFwAuthorizedApplicationsDisp = dispinterface;
  INetFwServiceRestriction = interface;
  INetFwServiceRestrictionDisp = dispinterface;
  INetFwRules = interface;
  INetFwRulesDisp = dispinterface;
  INetFwRule = interface;
  INetFwRuleDisp = dispinterface;
  INetFwProfile = interface;
  INetFwProfileDisp = dispinterface;
  INetFwPolicy = interface;
  INetFwPolicyDisp = dispinterface;
  INetFwPolicy2 = interface;
  INetFwPolicy2Disp = dispinterface;
  INetFwMgr = interface;
  INetFwMgrDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  NetFwRule = INetFwRule;
  NetFwOpenPort = INetFwOpenPort;
  NetFwAuthorizedApplication = INetFwAuthorizedApplication;
  NetFwPolicy2 = INetFwPolicy2;
  NetFwMgr = INetFwMgr;


// *********************************************************************//
// Interface: INetFwRemoteAdminSettings
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D4BECDDF-6F73-4A83-B832-9C66874CD20E}
// *********************************************************************//
  INetFwRemoteAdminSettings = interface(IDispatch)
    ['{D4BECDDF-6F73-4A83-B832-9C66874CD20E}']
    function Get_IpVersion: NET_FW_IP_VERSION_; safecall;
    procedure Set_IpVersion(IpVersion: NET_FW_IP_VERSION_); safecall;
    function Get_Scope: NET_FW_SCOPE_; safecall;
    procedure Set_Scope(Scope: NET_FW_SCOPE_); safecall;
    function Get_RemoteAddresses: WideString; safecall;
    procedure Set_RemoteAddresses(const remoteAddrs: WideString); safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Enabled: WordBool); safecall;
    property IpVersion: NET_FW_IP_VERSION_ read Get_IpVersion write Set_IpVersion;
    property Scope: NET_FW_SCOPE_ read Get_Scope write Set_Scope;
    property RemoteAddresses: WideString read Get_RemoteAddresses write Set_RemoteAddresses;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
  end;

// *********************************************************************//
// DispIntf:  INetFwRemoteAdminSettingsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D4BECDDF-6F73-4A83-B832-9C66874CD20E}
// *********************************************************************//
  INetFwRemoteAdminSettingsDisp = dispinterface
    ['{D4BECDDF-6F73-4A83-B832-9C66874CD20E}']
    property IpVersion: NET_FW_IP_VERSION_ dispid 1;
    property Scope: NET_FW_SCOPE_ dispid 2;
    property RemoteAddresses: WideString dispid 3;
    property Enabled: WordBool dispid 4;
  end;

// *********************************************************************//
// Interface: INetFwIcmpSettings
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A6207B2E-7CDD-426A-951E-5E1CBC5AFEAD}
// *********************************************************************//
  INetFwIcmpSettings = interface(IDispatch)
    ['{A6207B2E-7CDD-426A-951E-5E1CBC5AFEAD}']
    function Get_AllowOutboundDestinationUnreachable: WordBool; safecall;
    procedure Set_AllowOutboundDestinationUnreachable(allow: WordBool); safecall;
    function Get_AllowRedirect: WordBool; safecall;
    procedure Set_AllowRedirect(allow: WordBool); safecall;
    function Get_AllowInboundEchoRequest: WordBool; safecall;
    procedure Set_AllowInboundEchoRequest(allow: WordBool); safecall;
    function Get_AllowOutboundTimeExceeded: WordBool; safecall;
    procedure Set_AllowOutboundTimeExceeded(allow: WordBool); safecall;
    function Get_AllowOutboundParameterProblem: WordBool; safecall;
    procedure Set_AllowOutboundParameterProblem(allow: WordBool); safecall;
    function Get_AllowOutboundSourceQuench: WordBool; safecall;
    procedure Set_AllowOutboundSourceQuench(allow: WordBool); safecall;
    function Get_AllowInboundRouterRequest: WordBool; safecall;
    procedure Set_AllowInboundRouterRequest(allow: WordBool); safecall;
    function Get_AllowInboundTimestampRequest: WordBool; safecall;
    procedure Set_AllowInboundTimestampRequest(allow: WordBool); safecall;
    function Get_AllowInboundMaskRequest: WordBool; safecall;
    procedure Set_AllowInboundMaskRequest(allow: WordBool); safecall;
    function Get_AllowOutboundPacketTooBig: WordBool; safecall;
    procedure Set_AllowOutboundPacketTooBig(allow: WordBool); safecall;
    property AllowOutboundDestinationUnreachable: WordBool read Get_AllowOutboundDestinationUnreachable write Set_AllowOutboundDestinationUnreachable;
    property AllowRedirect: WordBool read Get_AllowRedirect write Set_AllowRedirect;
    property AllowInboundEchoRequest: WordBool read Get_AllowInboundEchoRequest write Set_AllowInboundEchoRequest;
    property AllowOutboundTimeExceeded: WordBool read Get_AllowOutboundTimeExceeded write Set_AllowOutboundTimeExceeded;
    property AllowOutboundParameterProblem: WordBool read Get_AllowOutboundParameterProblem write Set_AllowOutboundParameterProblem;
    property AllowOutboundSourceQuench: WordBool read Get_AllowOutboundSourceQuench write Set_AllowOutboundSourceQuench;
    property AllowInboundRouterRequest: WordBool read Get_AllowInboundRouterRequest write Set_AllowInboundRouterRequest;
    property AllowInboundTimestampRequest: WordBool read Get_AllowInboundTimestampRequest write Set_AllowInboundTimestampRequest;
    property AllowInboundMaskRequest: WordBool read Get_AllowInboundMaskRequest write Set_AllowInboundMaskRequest;
    property AllowOutboundPacketTooBig: WordBool read Get_AllowOutboundPacketTooBig write Set_AllowOutboundPacketTooBig;
  end;

// *********************************************************************//
// DispIntf:  INetFwIcmpSettingsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A6207B2E-7CDD-426A-951E-5E1CBC5AFEAD}
// *********************************************************************//
  INetFwIcmpSettingsDisp = dispinterface
    ['{A6207B2E-7CDD-426A-951E-5E1CBC5AFEAD}']
    property AllowOutboundDestinationUnreachable: WordBool dispid 1;
    property AllowRedirect: WordBool dispid 2;
    property AllowInboundEchoRequest: WordBool dispid 3;
    property AllowOutboundTimeExceeded: WordBool dispid 4;
    property AllowOutboundParameterProblem: WordBool dispid 5;
    property AllowOutboundSourceQuench: WordBool dispid 6;
    property AllowInboundRouterRequest: WordBool dispid 7;
    property AllowInboundTimestampRequest: WordBool dispid 8;
    property AllowInboundMaskRequest: WordBool dispid 9;
    property AllowOutboundPacketTooBig: WordBool dispid 10;
  end;

// *********************************************************************//
// Interface: INetFwOpenPort
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E0483BA0-47FF-4D9C-A6D6-7741D0B195F7}
// *********************************************************************//
  INetFwOpenPort = interface(IDispatch)
    ['{E0483BA0-47FF-4D9C-A6D6-7741D0B195F7}']
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Name: WideString); safecall;
    function Get_IpVersion: NET_FW_IP_VERSION_; safecall;
    procedure Set_IpVersion(IpVersion: NET_FW_IP_VERSION_); safecall;
    function Get_Protocol: NET_FW_IP_PROTOCOL_; safecall;
    procedure Set_Protocol(ipProtocol: NET_FW_IP_PROTOCOL_); safecall;
    function Get_Port: Integer; safecall;
    procedure Set_Port(portNumber: Integer); safecall;
    function Get_Scope: NET_FW_SCOPE_; safecall;
    procedure Set_Scope(Scope: NET_FW_SCOPE_); safecall;
    function Get_RemoteAddresses: WideString; safecall;
    procedure Set_RemoteAddresses(const remoteAddrs: WideString); safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Enabled: WordBool); safecall;
    function Get_BuiltIn: WordBool; safecall;
    property Name: WideString read Get_Name write Set_Name;
    property IpVersion: NET_FW_IP_VERSION_ read Get_IpVersion write Set_IpVersion;
    property Protocol: NET_FW_IP_PROTOCOL_ read Get_Protocol write Set_Protocol;
    property Port: Integer read Get_Port write Set_Port;
    property Scope: NET_FW_SCOPE_ read Get_Scope write Set_Scope;
    property RemoteAddresses: WideString read Get_RemoteAddresses write Set_RemoteAddresses;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property BuiltIn: WordBool read Get_BuiltIn;
  end;

// *********************************************************************//
// DispIntf:  INetFwOpenPortDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E0483BA0-47FF-4D9C-A6D6-7741D0B195F7}
// *********************************************************************//
  INetFwOpenPortDisp = dispinterface
    ['{E0483BA0-47FF-4D9C-A6D6-7741D0B195F7}']
    property Name: WideString dispid 1;
    property IpVersion: NET_FW_IP_VERSION_ dispid 2;
    property Protocol: NET_FW_IP_PROTOCOL_ dispid 3;
    property Port: Integer dispid 4;
    property Scope: NET_FW_SCOPE_ dispid 5;
    property RemoteAddresses: WideString dispid 6;
    property Enabled: WordBool dispid 7;
    property BuiltIn: WordBool readonly dispid 8;
  end;

// *********************************************************************//
// Interface: INetFwOpenPorts
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C0E9D7FA-E07E-430A-B19A-090CE82D92E2}
// *********************************************************************//
  INetFwOpenPorts = interface(IDispatch)
    ['{C0E9D7FA-E07E-430A-B19A-090CE82D92E2}']
    function Get_Count: Integer; safecall;
    procedure Add(const Port: INetFwOpenPort); safecall;
    procedure Remove(portNumber: Integer; ipProtocol: NET_FW_IP_PROTOCOL_); safecall;
    function Item(portNumber: Integer; ipProtocol: NET_FW_IP_PROTOCOL_): INetFwOpenPort; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  INetFwOpenPortsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C0E9D7FA-E07E-430A-B19A-090CE82D92E2}
// *********************************************************************//
  INetFwOpenPortsDisp = dispinterface
    ['{C0E9D7FA-E07E-430A-B19A-090CE82D92E2}']
    property Count: Integer readonly dispid 1;
    procedure Add(const Port: INetFwOpenPort); dispid 2;
    procedure Remove(portNumber: Integer; ipProtocol: NET_FW_IP_PROTOCOL_); dispid 3;
    function Item(portNumber: Integer; ipProtocol: NET_FW_IP_PROTOCOL_): INetFwOpenPort; dispid 4;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: INetFwService
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {79FD57C8-908E-4A36-9888-D5B3F0A444CF}
// *********************************************************************//
  INetFwService = interface(IDispatch)
    ['{79FD57C8-908E-4A36-9888-D5B3F0A444CF}']
    function Get_Name: WideString; safecall;
    function Get_type_: NET_FW_SERVICE_TYPE_; safecall;
    function Get_Customized: WordBool; safecall;
    function Get_IpVersion: NET_FW_IP_VERSION_; safecall;
    procedure Set_IpVersion(IpVersion: NET_FW_IP_VERSION_); safecall;
    function Get_Scope: NET_FW_SCOPE_; safecall;
    procedure Set_Scope(Scope: NET_FW_SCOPE_); safecall;
    function Get_RemoteAddresses: WideString; safecall;
    procedure Set_RemoteAddresses(const remoteAddrs: WideString); safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Enabled: WordBool); safecall;
    function Get_GloballyOpenPorts: INetFwOpenPorts; safecall;
    property Name: WideString read Get_Name;
    property type_: NET_FW_SERVICE_TYPE_ read Get_type_;
    property Customized: WordBool read Get_Customized;
    property IpVersion: NET_FW_IP_VERSION_ read Get_IpVersion write Set_IpVersion;
    property Scope: NET_FW_SCOPE_ read Get_Scope write Set_Scope;
    property RemoteAddresses: WideString read Get_RemoteAddresses write Set_RemoteAddresses;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property GloballyOpenPorts: INetFwOpenPorts read Get_GloballyOpenPorts;
  end;

// *********************************************************************//
// DispIntf:  INetFwServiceDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {79FD57C8-908E-4A36-9888-D5B3F0A444CF}
// *********************************************************************//
  INetFwServiceDisp = dispinterface
    ['{79FD57C8-908E-4A36-9888-D5B3F0A444CF}']
    property Name: WideString readonly dispid 1;
    property type_: NET_FW_SERVICE_TYPE_ readonly dispid 2;
    property Customized: WordBool readonly dispid 3;
    property IpVersion: NET_FW_IP_VERSION_ dispid 4;
    property Scope: NET_FW_SCOPE_ dispid 5;
    property RemoteAddresses: WideString dispid 6;
    property Enabled: WordBool dispid 7;
    property GloballyOpenPorts: INetFwOpenPorts readonly dispid 8;
  end;

// *********************************************************************//
// Interface: INetFwServices
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {79649BB4-903E-421B-94C9-79848E79F6EE}
// *********************************************************************//
  INetFwServices = interface(IDispatch)
    ['{79649BB4-903E-421B-94C9-79848E79F6EE}']
    function Get_Count: Integer; safecall;
    function Item(svcType: NET_FW_SERVICE_TYPE_): INetFwService; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  INetFwServicesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {79649BB4-903E-421B-94C9-79848E79F6EE}
// *********************************************************************//
  INetFwServicesDisp = dispinterface
    ['{79649BB4-903E-421B-94C9-79848E79F6EE}']
    property Count: Integer readonly dispid 1;
    function Item(svcType: NET_FW_SERVICE_TYPE_): INetFwService; dispid 2;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: INetFwAuthorizedApplication
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B5E64FFA-C2C5-444E-A301-FB5E00018050}
// *********************************************************************//
  INetFwAuthorizedApplication = interface(IDispatch)
    ['{B5E64FFA-C2C5-444E-A301-FB5E00018050}']
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Name: WideString); safecall;
    function Get_ProcessImageFileName: WideString; safecall;
    procedure Set_ProcessImageFileName(const imageFileName: WideString); safecall;
    function Get_IpVersion: NET_FW_IP_VERSION_; safecall;
    procedure Set_IpVersion(IpVersion: NET_FW_IP_VERSION_); safecall;
    function Get_Scope: NET_FW_SCOPE_; safecall;
    procedure Set_Scope(Scope: NET_FW_SCOPE_); safecall;
    function Get_RemoteAddresses: WideString; safecall;
    procedure Set_RemoteAddresses(const remoteAddrs: WideString); safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Enabled: WordBool); safecall;
    property Name: WideString read Get_Name write Set_Name;
    property ProcessImageFileName: WideString read Get_ProcessImageFileName write Set_ProcessImageFileName;
    property IpVersion: NET_FW_IP_VERSION_ read Get_IpVersion write Set_IpVersion;
    property Scope: NET_FW_SCOPE_ read Get_Scope write Set_Scope;
    property RemoteAddresses: WideString read Get_RemoteAddresses write Set_RemoteAddresses;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
  end;

// *********************************************************************//
// DispIntf:  INetFwAuthorizedApplicationDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B5E64FFA-C2C5-444E-A301-FB5E00018050}
// *********************************************************************//
  INetFwAuthorizedApplicationDisp = dispinterface
    ['{B5E64FFA-C2C5-444E-A301-FB5E00018050}']
    property Name: WideString dispid 1;
    property ProcessImageFileName: WideString dispid 2;
    property IpVersion: NET_FW_IP_VERSION_ dispid 3;
    property Scope: NET_FW_SCOPE_ dispid 4;
    property RemoteAddresses: WideString dispid 5;
    property Enabled: WordBool dispid 6;
  end;

// *********************************************************************//
// Interface: INetFwAuthorizedApplications
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {644EFD52-CCF9-486C-97A2-39F352570B30}
// *********************************************************************//
  INetFwAuthorizedApplications = interface(IDispatch)
    ['{644EFD52-CCF9-486C-97A2-39F352570B30}']
    function Get_Count: Integer; safecall;
    procedure Add(const app: INetFwAuthorizedApplication); safecall;
    procedure Remove(const imageFileName: WideString); safecall;
    function Item(const imageFileName: WideString): INetFwAuthorizedApplication; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  INetFwAuthorizedApplicationsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {644EFD52-CCF9-486C-97A2-39F352570B30}
// *********************************************************************//
  INetFwAuthorizedApplicationsDisp = dispinterface
    ['{644EFD52-CCF9-486C-97A2-39F352570B30}']
    property Count: Integer readonly dispid 1;
    procedure Add(const app: INetFwAuthorizedApplication); dispid 2;
    procedure Remove(const imageFileName: WideString); dispid 3;
    function Item(const imageFileName: WideString): INetFwAuthorizedApplication; dispid 4;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: INetFwServiceRestriction
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8267BBE3-F890-491C-B7B6-2DB1EF0E5D2B}
// *********************************************************************//
  INetFwServiceRestriction = interface(IDispatch)
    ['{8267BBE3-F890-491C-B7B6-2DB1EF0E5D2B}']
    procedure RestrictService(const serviceName: WideString; const appName: WideString; 
                              RestrictService: WordBool; serviceSidRestricted: WordBool); safecall;
    function ServiceRestricted(const serviceName: WideString; const appName: WideString): WordBool; safecall;
    function Get_Rules: INetFwRules; safecall;
    property Rules: INetFwRules read Get_Rules;
  end;

// *********************************************************************//
// DispIntf:  INetFwServiceRestrictionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8267BBE3-F890-491C-B7B6-2DB1EF0E5D2B}
// *********************************************************************//
  INetFwServiceRestrictionDisp = dispinterface
    ['{8267BBE3-F890-491C-B7B6-2DB1EF0E5D2B}']
    procedure RestrictService(const serviceName: WideString; const appName: WideString; 
                              RestrictService: WordBool; serviceSidRestricted: WordBool); dispid 1;
    function ServiceRestricted(const serviceName: WideString; const appName: WideString): WordBool; dispid 2;
    property Rules: INetFwRules readonly dispid 3;
  end;

// *********************************************************************//
// Interface: INetFwRules
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9C4C6277-5027-441E-AFAE-CA1F542DA009}
// *********************************************************************//
  INetFwRules = interface(IDispatch)
    ['{9C4C6277-5027-441E-AFAE-CA1F542DA009}']
    function Get_Count: Integer; safecall;
    procedure Add(const rule: INetFwRule); safecall;
    procedure Remove(const Name: WideString); safecall;
    function Item(const Name: WideString): INetFwRule; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  INetFwRulesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9C4C6277-5027-441E-AFAE-CA1F542DA009}
// *********************************************************************//
  INetFwRulesDisp = dispinterface
    ['{9C4C6277-5027-441E-AFAE-CA1F542DA009}']
    property Count: Integer readonly dispid 1;
    procedure Add(const rule: INetFwRule); dispid 2;
    procedure Remove(const Name: WideString); dispid 3;
    function Item(const Name: WideString): INetFwRule; dispid 4;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: INetFwRule
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {AF230D27-BABA-4E42-ACED-F524F22CFCE2}
// *********************************************************************//
  INetFwRule = interface(IDispatch)
    ['{AF230D27-BABA-4E42-ACED-F524F22CFCE2}']
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Name: WideString); safecall;
    function Get_Description: WideString; safecall;
    procedure Set_Description(const desc: WideString); safecall;
    function Get_ApplicationName: WideString; safecall;
    procedure Set_ApplicationName(const imageFileName: WideString); safecall;
    function Get_serviceName: WideString; safecall;
    procedure Set_serviceName(const serviceName: WideString); safecall;
    function Get_Protocol: Integer; safecall;
    procedure Set_Protocol(Protocol: Integer); safecall;
    function Get_LocalPorts: WideString; safecall;
    procedure Set_LocalPorts(const portNumbers: WideString); safecall;
    function Get_RemotePorts: WideString; safecall;
    procedure Set_RemotePorts(const portNumbers: WideString); safecall;
    function Get_LocalAddresses: WideString; safecall;
    procedure Set_LocalAddresses(const localAddrs: WideString); safecall;
    function Get_RemoteAddresses: WideString; safecall;
    procedure Set_RemoteAddresses(const remoteAddrs: WideString); safecall;
    function Get_IcmpTypesAndCodes: WideString; safecall;
    procedure Set_IcmpTypesAndCodes(const IcmpTypesAndCodes: WideString); safecall;
    function Get_Direction: NET_FW_RULE_DIRECTION_; safecall;
    procedure Set_Direction(dir: NET_FW_RULE_DIRECTION_); safecall;
    function Get_Interfaces: OleVariant; safecall;
    procedure Set_Interfaces(Interfaces: OleVariant); safecall;
    function Get_InterfaceTypes: WideString; safecall;
    procedure Set_InterfaceTypes(const InterfaceTypes: WideString); safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Enabled: WordBool); safecall;
    function Get_Grouping: WideString; safecall;
    procedure Set_Grouping(const context: WideString); safecall;
    function Get_Profiles: Integer; safecall;
    procedure Set_Profiles(profileTypesBitmask: Integer); safecall;
    function Get_EdgeTraversal: WordBool; safecall;
    procedure Set_EdgeTraversal(Enabled: WordBool); safecall;
    function Get_Action: NET_FW_ACTION_; safecall;
    procedure Set_Action(Action: NET_FW_ACTION_); safecall;
    property Name: WideString read Get_Name write Set_Name;
    property Description: WideString read Get_Description write Set_Description;
    property ApplicationName: WideString read Get_ApplicationName write Set_ApplicationName;
    property serviceName: WideString read Get_serviceName write Set_serviceName;
    property Protocol: Integer read Get_Protocol write Set_Protocol;
    property LocalPorts: WideString read Get_LocalPorts write Set_LocalPorts;
    property RemotePorts: WideString read Get_RemotePorts write Set_RemotePorts;
    property LocalAddresses: WideString read Get_LocalAddresses write Set_LocalAddresses;
    property RemoteAddresses: WideString read Get_RemoteAddresses write Set_RemoteAddresses;
    property IcmpTypesAndCodes: WideString read Get_IcmpTypesAndCodes write Set_IcmpTypesAndCodes;
    property Direction: NET_FW_RULE_DIRECTION_ read Get_Direction write Set_Direction;
    property Interfaces: OleVariant read Get_Interfaces write Set_Interfaces;
    property InterfaceTypes: WideString read Get_InterfaceTypes write Set_InterfaceTypes;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property Grouping: WideString read Get_Grouping write Set_Grouping;
    property Profiles: Integer read Get_Profiles write Set_Profiles;
    property EdgeTraversal: WordBool read Get_EdgeTraversal write Set_EdgeTraversal;
    property Action: NET_FW_ACTION_ read Get_Action write Set_Action;
  end;

// *********************************************************************//
// DispIntf:  INetFwRuleDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {AF230D27-BABA-4E42-ACED-F524F22CFCE2}
// *********************************************************************//
  INetFwRuleDisp = dispinterface
    ['{AF230D27-BABA-4E42-ACED-F524F22CFCE2}']
    property Name: WideString dispid 1;
    property Description: WideString dispid 2;
    property ApplicationName: WideString dispid 3;
    property serviceName: WideString dispid 4;
    property Protocol: Integer dispid 5;
    property LocalPorts: WideString dispid 6;
    property RemotePorts: WideString dispid 7;
    property LocalAddresses: WideString dispid 8;
    property RemoteAddresses: WideString dispid 9;
    property IcmpTypesAndCodes: WideString dispid 10;
    property Direction: NET_FW_RULE_DIRECTION_ dispid 11;
    property Interfaces: OleVariant dispid 12;
    property InterfaceTypes: WideString dispid 13;
    property Enabled: WordBool dispid 14;
    property Grouping: WideString dispid 15;
    property Profiles: Integer dispid 16;
    property EdgeTraversal: WordBool dispid 17;
    property Action: NET_FW_ACTION_ dispid 18;
  end;

// *********************************************************************//
// Interface: INetFwProfile
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {174A0DDA-E9F9-449D-993B-21AB667CA456}
// *********************************************************************//
  INetFwProfile = interface(IDispatch)
    ['{174A0DDA-E9F9-449D-993B-21AB667CA456}']
    function Get_type_: NET_FW_PROFILE_TYPE_; safecall;
    function Get_FirewallEnabled: WordBool; safecall;
    procedure Set_FirewallEnabled(Enabled: WordBool); safecall;
    function Get_ExceptionsNotAllowed: WordBool; safecall;
    procedure Set_ExceptionsNotAllowed(notAllowed: WordBool); safecall;
    function Get_NotificationsDisabled: WordBool; safecall;
    procedure Set_NotificationsDisabled(disabled: WordBool); safecall;
    function Get_UnicastResponsesToMulticastBroadcastDisabled: WordBool; safecall;
    procedure Set_UnicastResponsesToMulticastBroadcastDisabled(disabled: WordBool); safecall;
    function Get_RemoteAdminSettings: INetFwRemoteAdminSettings; safecall;
    function Get_IcmpSettings: INetFwIcmpSettings; safecall;
    function Get_GloballyOpenPorts: INetFwOpenPorts; safecall;
    function Get_Services: INetFwServices; safecall;
    function Get_AuthorizedApplications: INetFwAuthorizedApplications; safecall;
    property type_: NET_FW_PROFILE_TYPE_ read Get_type_;
    property FirewallEnabled: WordBool read Get_FirewallEnabled write Set_FirewallEnabled;
    property ExceptionsNotAllowed: WordBool read Get_ExceptionsNotAllowed write Set_ExceptionsNotAllowed;
    property NotificationsDisabled: WordBool read Get_NotificationsDisabled write Set_NotificationsDisabled;
    property UnicastResponsesToMulticastBroadcastDisabled: WordBool read Get_UnicastResponsesToMulticastBroadcastDisabled write Set_UnicastResponsesToMulticastBroadcastDisabled;
    property RemoteAdminSettings: INetFwRemoteAdminSettings read Get_RemoteAdminSettings;
    property IcmpSettings: INetFwIcmpSettings read Get_IcmpSettings;
    property GloballyOpenPorts: INetFwOpenPorts read Get_GloballyOpenPorts;
    property Services: INetFwServices read Get_Services;
    property AuthorizedApplications: INetFwAuthorizedApplications read Get_AuthorizedApplications;
  end;

// *********************************************************************//
// DispIntf:  INetFwProfileDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {174A0DDA-E9F9-449D-993B-21AB667CA456}
// *********************************************************************//
  INetFwProfileDisp = dispinterface
    ['{174A0DDA-E9F9-449D-993B-21AB667CA456}']
    property type_: NET_FW_PROFILE_TYPE_ readonly dispid 1;
    property FirewallEnabled: WordBool dispid 2;
    property ExceptionsNotAllowed: WordBool dispid 3;
    property NotificationsDisabled: WordBool dispid 4;
    property UnicastResponsesToMulticastBroadcastDisabled: WordBool dispid 5;
    property RemoteAdminSettings: INetFwRemoteAdminSettings readonly dispid 6;
    property IcmpSettings: INetFwIcmpSettings readonly dispid 7;
    property GloballyOpenPorts: INetFwOpenPorts readonly dispid 8;
    property Services: INetFwServices readonly dispid 9;
    property AuthorizedApplications: INetFwAuthorizedApplications readonly dispid 10;
  end;

// *********************************************************************//
// Interface: INetFwPolicy
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D46D2478-9AC9-4008-9DC7-5563CE5536CC}
// *********************************************************************//
  INetFwPolicy = interface(IDispatch)
    ['{D46D2478-9AC9-4008-9DC7-5563CE5536CC}']
    function Get_CurrentProfile: INetFwProfile; safecall;
    function GetProfileByType(profileType: NET_FW_PROFILE_TYPE_): INetFwProfile; safecall;
    property CurrentProfile: INetFwProfile read Get_CurrentProfile;
  end;

// *********************************************************************//
// DispIntf:  INetFwPolicyDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D46D2478-9AC9-4008-9DC7-5563CE5536CC}
// *********************************************************************//
  INetFwPolicyDisp = dispinterface
    ['{D46D2478-9AC9-4008-9DC7-5563CE5536CC}']
    property CurrentProfile: INetFwProfile readonly dispid 1;
    function GetProfileByType(profileType: NET_FW_PROFILE_TYPE_): INetFwProfile; dispid 2;
  end;

// *********************************************************************//
// Interface: INetFwPolicy2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {98325047-C671-4174-8D81-DEFCD3F03186}
// *********************************************************************//
  INetFwPolicy2 = interface(IDispatch)
    ['{98325047-C671-4174-8D81-DEFCD3F03186}']
    function Get_CurrentProfileTypes: Integer; safecall;
    function Get_FirewallEnabled(profileType: NET_FW_PROFILE_TYPE2_): WordBool; safecall;
    procedure Set_FirewallEnabled(profileType: NET_FW_PROFILE_TYPE2_; Enabled: WordBool); safecall;
    function Get_ExcludedInterfaces(profileType: NET_FW_PROFILE_TYPE2_): OleVariant; safecall;
    procedure Set_ExcludedInterfaces(profileType: NET_FW_PROFILE_TYPE2_; Interfaces: OleVariant); safecall;
    function Get_BlockAllInboundTraffic(profileType: NET_FW_PROFILE_TYPE2_): WordBool; safecall;
    procedure Set_BlockAllInboundTraffic(profileType: NET_FW_PROFILE_TYPE2_; Block: WordBool); safecall;
    function Get_NotificationsDisabled(profileType: NET_FW_PROFILE_TYPE2_): WordBool; safecall;
    procedure Set_NotificationsDisabled(profileType: NET_FW_PROFILE_TYPE2_; disabled: WordBool); safecall;
    function Get_UnicastResponsesToMulticastBroadcastDisabled(profileType: NET_FW_PROFILE_TYPE2_): WordBool; safecall;
    procedure Set_UnicastResponsesToMulticastBroadcastDisabled(profileType: NET_FW_PROFILE_TYPE2_; 
                                                               disabled: WordBool); safecall;
    function Get_Rules: INetFwRules; safecall;
    function Get_ServiceRestriction: INetFwServiceRestriction; safecall;
    procedure EnableRuleGroup(profileTypesBitmask: Integer; const group: WideString; 
                              enable: WordBool); safecall;
    function IsRuleGroupEnabled(profileTypesBitmask: Integer; const group: WideString): WordBool; safecall;
    procedure RestoreLocalFirewallDefaults; safecall;
    function Get_DefaultInboundAction(profileType: NET_FW_PROFILE_TYPE2_): NET_FW_ACTION_; safecall;
    procedure Set_DefaultInboundAction(profileType: NET_FW_PROFILE_TYPE2_; Action: NET_FW_ACTION_); safecall;
    function Get_DefaultOutboundAction(profileType: NET_FW_PROFILE_TYPE2_): NET_FW_ACTION_; safecall;
    procedure Set_DefaultOutboundAction(profileType: NET_FW_PROFILE_TYPE2_; Action: NET_FW_ACTION_); safecall;
    function Get_IsRuleGroupCurrentlyEnabled(const group: WideString): WordBool; safecall;
    function Get_LocalPolicyModifyState: NET_FW_MODIFY_STATE_; safecall;
    property CurrentProfileTypes: Integer read Get_CurrentProfileTypes;
    property FirewallEnabled[profileType: NET_FW_PROFILE_TYPE2_]: WordBool read Get_FirewallEnabled write Set_FirewallEnabled;
    property ExcludedInterfaces[profileType: NET_FW_PROFILE_TYPE2_]: OleVariant read Get_ExcludedInterfaces write Set_ExcludedInterfaces;
    property BlockAllInboundTraffic[profileType: NET_FW_PROFILE_TYPE2_]: WordBool read Get_BlockAllInboundTraffic write Set_BlockAllInboundTraffic;
    property NotificationsDisabled[profileType: NET_FW_PROFILE_TYPE2_]: WordBool read Get_NotificationsDisabled write Set_NotificationsDisabled;
    property UnicastResponsesToMulticastBroadcastDisabled[profileType: NET_FW_PROFILE_TYPE2_]: WordBool read Get_UnicastResponsesToMulticastBroadcastDisabled write Set_UnicastResponsesToMulticastBroadcastDisabled;
    property Rules: INetFwRules read Get_Rules;
    property ServiceRestriction: INetFwServiceRestriction read Get_ServiceRestriction;
    property DefaultInboundAction[profileType: NET_FW_PROFILE_TYPE2_]: NET_FW_ACTION_ read Get_DefaultInboundAction write Set_DefaultInboundAction;
    property DefaultOutboundAction[profileType: NET_FW_PROFILE_TYPE2_]: NET_FW_ACTION_ read Get_DefaultOutboundAction write Set_DefaultOutboundAction;
    property IsRuleGroupCurrentlyEnabled[const group: WideString]: WordBool read Get_IsRuleGroupCurrentlyEnabled;
    property LocalPolicyModifyState: NET_FW_MODIFY_STATE_ read Get_LocalPolicyModifyState;
  end;

// *********************************************************************//
// DispIntf:  INetFwPolicy2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {98325047-C671-4174-8D81-DEFCD3F03186}
// *********************************************************************//
  INetFwPolicy2Disp = dispinterface
    ['{98325047-C671-4174-8D81-DEFCD3F03186}']
    property CurrentProfileTypes: Integer readonly dispid 1;
    property FirewallEnabled[profileType: NET_FW_PROFILE_TYPE2_]: WordBool dispid 2;
    property ExcludedInterfaces[profileType: NET_FW_PROFILE_TYPE2_]: OleVariant dispid 3;
    property BlockAllInboundTraffic[profileType: NET_FW_PROFILE_TYPE2_]: WordBool dispid 4;
    property NotificationsDisabled[profileType: NET_FW_PROFILE_TYPE2_]: WordBool dispid 5;
    property UnicastResponsesToMulticastBroadcastDisabled[profileType: NET_FW_PROFILE_TYPE2_]: WordBool dispid 6;
    property Rules: INetFwRules readonly dispid 7;
    property ServiceRestriction: INetFwServiceRestriction readonly dispid 8;
    procedure EnableRuleGroup(profileTypesBitmask: Integer; const group: WideString; 
                              enable: WordBool); dispid 9;
    function IsRuleGroupEnabled(profileTypesBitmask: Integer; const group: WideString): WordBool; dispid 10;
    procedure RestoreLocalFirewallDefaults; dispid 11;
    property DefaultInboundAction[profileType: NET_FW_PROFILE_TYPE2_]: NET_FW_ACTION_ dispid 12;
    property DefaultOutboundAction[profileType: NET_FW_PROFILE_TYPE2_]: NET_FW_ACTION_ dispid 13;
    property IsRuleGroupCurrentlyEnabled[const group: WideString]: WordBool readonly dispid 14;
    property LocalPolicyModifyState: NET_FW_MODIFY_STATE_ readonly dispid 15;
  end;

// *********************************************************************//
// Interface: INetFwMgr
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F7898AF5-CAC4-4632-A2EC-DA06E5111AF2}
// *********************************************************************//
  INetFwMgr = interface(IDispatch)
    ['{F7898AF5-CAC4-4632-A2EC-DA06E5111AF2}']
    function Get_LocalPolicy: INetFwPolicy; safecall;
    function Get_CurrentProfileType: NET_FW_PROFILE_TYPE_; safecall;
    procedure RestoreDefaults; safecall;
    procedure IsPortAllowed(const imageFileName: WideString; IpVersion: NET_FW_IP_VERSION_; 
                            portNumber: Integer; const localAddress: WideString; 
                            ipProtocol: NET_FW_IP_PROTOCOL_; out allowed: OleVariant; 
                            out restricted: OleVariant); safecall;
    procedure IsIcmpTypeAllowed(IpVersion: NET_FW_IP_VERSION_; const localAddress: WideString; 
                                Type_: Byte; out allowed: OleVariant; out restricted: OleVariant); safecall;
    property LocalPolicy: INetFwPolicy read Get_LocalPolicy;
    property CurrentProfileType: NET_FW_PROFILE_TYPE_ read Get_CurrentProfileType;
  end;

// *********************************************************************//
// DispIntf:  INetFwMgrDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F7898AF5-CAC4-4632-A2EC-DA06E5111AF2}
// *********************************************************************//
  INetFwMgrDisp = dispinterface
    ['{F7898AF5-CAC4-4632-A2EC-DA06E5111AF2}']
    property LocalPolicy: INetFwPolicy readonly dispid 1;
    property CurrentProfileType: NET_FW_PROFILE_TYPE_ readonly dispid 2;
    procedure RestoreDefaults; dispid 3;
    procedure IsPortAllowed(const imageFileName: WideString; IpVersion: NET_FW_IP_VERSION_; 
                            portNumber: Integer; const localAddress: WideString; 
                            ipProtocol: NET_FW_IP_PROTOCOL_; out allowed: OleVariant; 
                            out restricted: OleVariant); dispid 4;
    procedure IsIcmpTypeAllowed(IpVersion: NET_FW_IP_VERSION_; const localAddress: WideString; 
                                Type_: Byte; out allowed: OleVariant; out restricted: OleVariant); dispid 5;
  end;

// *********************************************************************//
// The Class CoNetFwRule provides a Create and CreateRemote method to          
// create instances of the default interface INetFwRule exposed by              
// the CoClass NetFwRule. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoNetFwRule = class
    class function Create: INetFwRule;
    class function CreateRemote(const MachineName: string): INetFwRule;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TNetFwRule
// Help String      : 
// Default Interface: INetFwRule
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TNetFwRuleProperties= class;
{$ENDIF}
  TNetFwRule = class(TOleServer)
  private
    FIntf: INetFwRule;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TNetFwRuleProperties;
    function GetServerProperties: TNetFwRuleProperties;
{$ENDIF}
    function GetDefaultInterface: INetFwRule;
  protected
    procedure InitServerData; override;
    function Get_Name: WideString;
    procedure Set_Name(const Name: WideString);
    function Get_Description: WideString;
    procedure Set_Description(const desc: WideString);
    function Get_ApplicationName: WideString;
    procedure Set_ApplicationName(const imageFileName: WideString);
    function Get_serviceName: WideString;
    procedure Set_serviceName(const serviceName: WideString);
    function Get_Protocol: Integer;
    procedure Set_Protocol(Protocol: Integer);
    function Get_LocalPorts: WideString;
    procedure Set_LocalPorts(const portNumbers: WideString);
    function Get_RemotePorts: WideString;
    procedure Set_RemotePorts(const portNumbers: WideString);
    function Get_LocalAddresses: WideString;
    procedure Set_LocalAddresses(const localAddrs: WideString);
    function Get_RemoteAddresses: WideString;
    procedure Set_RemoteAddresses(const remoteAddrs: WideString);
    function Get_IcmpTypesAndCodes: WideString;
    procedure Set_IcmpTypesAndCodes(const IcmpTypesAndCodes: WideString);
    function Get_Direction: NET_FW_RULE_DIRECTION_;
    procedure Set_Direction(dir: NET_FW_RULE_DIRECTION_);
    function Get_Interfaces: OleVariant;
    procedure Set_Interfaces(Interfaces: OleVariant);
    function Get_InterfaceTypes: WideString;
    procedure Set_InterfaceTypes(const InterfaceTypes: WideString);
    function Get_Enabled: WordBool;
    procedure Set_Enabled(Enabled: WordBool);
    function Get_Grouping: WideString;
    procedure Set_Grouping(const context: WideString);
    function Get_Profiles: Integer;
    procedure Set_Profiles(profileTypesBitmask: Integer);
    function Get_EdgeTraversal: WordBool;
    procedure Set_EdgeTraversal(Enabled: WordBool);
    function Get_Action: NET_FW_ACTION_;
    procedure Set_Action(Action: NET_FW_ACTION_);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: INetFwRule);
    procedure Disconnect; override;
    property DefaultInterface: INetFwRule read GetDefaultInterface;
    property Interfaces: OleVariant read Get_Interfaces write Set_Interfaces;
    property Name: WideString read Get_Name write Set_Name;
    property Description: WideString read Get_Description write Set_Description;
    property ApplicationName: WideString read Get_ApplicationName write Set_ApplicationName;
    property serviceName: WideString read Get_serviceName write Set_serviceName;
    property Protocol: Integer read Get_Protocol write Set_Protocol;
    property LocalPorts: WideString read Get_LocalPorts write Set_LocalPorts;
    property RemotePorts: WideString read Get_RemotePorts write Set_RemotePorts;
    property LocalAddresses: WideString read Get_LocalAddresses write Set_LocalAddresses;
    property RemoteAddresses: WideString read Get_RemoteAddresses write Set_RemoteAddresses;
    property IcmpTypesAndCodes: WideString read Get_IcmpTypesAndCodes write Set_IcmpTypesAndCodes;
    property Direction: NET_FW_RULE_DIRECTION_ read Get_Direction write Set_Direction;
    property InterfaceTypes: WideString read Get_InterfaceTypes write Set_InterfaceTypes;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property Grouping: WideString read Get_Grouping write Set_Grouping;
    property Profiles: Integer read Get_Profiles write Set_Profiles;
    property EdgeTraversal: WordBool read Get_EdgeTraversal write Set_EdgeTraversal;
    property Action: NET_FW_ACTION_ read Get_Action write Set_Action;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TNetFwRuleProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TNetFwRule
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TNetFwRuleProperties = class(TPersistent)
  private
    FServer:    TNetFwRule;
    function    GetDefaultInterface: INetFwRule;
    constructor Create(AServer: TNetFwRule);
  protected
    function Get_Name: WideString;
    procedure Set_Name(const Name: WideString);
    function Get_Description: WideString;
    procedure Set_Description(const desc: WideString);
    function Get_ApplicationName: WideString;
    procedure Set_ApplicationName(const imageFileName: WideString);
    function Get_serviceName: WideString;
    procedure Set_serviceName(const serviceName: WideString);
    function Get_Protocol: Integer;
    procedure Set_Protocol(Protocol: Integer);
    function Get_LocalPorts: WideString;
    procedure Set_LocalPorts(const portNumbers: WideString);
    function Get_RemotePorts: WideString;
    procedure Set_RemotePorts(const portNumbers: WideString);
    function Get_LocalAddresses: WideString;
    procedure Set_LocalAddresses(const localAddrs: WideString);
    function Get_RemoteAddresses: WideString;
    procedure Set_RemoteAddresses(const remoteAddrs: WideString);
    function Get_IcmpTypesAndCodes: WideString;
    procedure Set_IcmpTypesAndCodes(const IcmpTypesAndCodes: WideString);
    function Get_Direction: NET_FW_RULE_DIRECTION_;
    procedure Set_Direction(dir: NET_FW_RULE_DIRECTION_);
    function Get_Interfaces: OleVariant;
    procedure Set_Interfaces(Interfaces: OleVariant);
    function Get_InterfaceTypes: WideString;
    procedure Set_InterfaceTypes(const InterfaceTypes: WideString);
    function Get_Enabled: WordBool;
    procedure Set_Enabled(Enabled: WordBool);
    function Get_Grouping: WideString;
    procedure Set_Grouping(const context: WideString);
    function Get_Profiles: Integer;
    procedure Set_Profiles(profileTypesBitmask: Integer);
    function Get_EdgeTraversal: WordBool;
    procedure Set_EdgeTraversal(Enabled: WordBool);
    function Get_Action: NET_FW_ACTION_;
    procedure Set_Action(Action: NET_FW_ACTION_);
  public
    property DefaultInterface: INetFwRule read GetDefaultInterface;
  published
    property Name: WideString read Get_Name write Set_Name;
    property Description: WideString read Get_Description write Set_Description;
    property ApplicationName: WideString read Get_ApplicationName write Set_ApplicationName;
    property serviceName: WideString read Get_serviceName write Set_serviceName;
    property Protocol: Integer read Get_Protocol write Set_Protocol;
    property LocalPorts: WideString read Get_LocalPorts write Set_LocalPorts;
    property RemotePorts: WideString read Get_RemotePorts write Set_RemotePorts;
    property LocalAddresses: WideString read Get_LocalAddresses write Set_LocalAddresses;
    property RemoteAddresses: WideString read Get_RemoteAddresses write Set_RemoteAddresses;
    property IcmpTypesAndCodes: WideString read Get_IcmpTypesAndCodes write Set_IcmpTypesAndCodes;
    property Direction: NET_FW_RULE_DIRECTION_ read Get_Direction write Set_Direction;
    property InterfaceTypes: WideString read Get_InterfaceTypes write Set_InterfaceTypes;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property Grouping: WideString read Get_Grouping write Set_Grouping;
    property Profiles: Integer read Get_Profiles write Set_Profiles;
    property EdgeTraversal: WordBool read Get_EdgeTraversal write Set_EdgeTraversal;
    property Action: NET_FW_ACTION_ read Get_Action write Set_Action;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoNetFwOpenPort provides a Create and CreateRemote method to          
// create instances of the default interface INetFwOpenPort exposed by              
// the CoClass NetFwOpenPort. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoNetFwOpenPort = class
    class function Create: INetFwOpenPort;
    class function CreateRemote(const MachineName: string): INetFwOpenPort;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TNetFwOpenPort
// Help String      : 
// Default Interface: INetFwOpenPort
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TNetFwOpenPortProperties= class;
{$ENDIF}
  TNetFwOpenPort = class(TOleServer)
  private
    FIntf: INetFwOpenPort;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TNetFwOpenPortProperties;
    function GetServerProperties: TNetFwOpenPortProperties;
{$ENDIF}
    function GetDefaultInterface: INetFwOpenPort;
  protected
    procedure InitServerData; override;
    function Get_Name: WideString;
    procedure Set_Name(const Name: WideString);
    function Get_IpVersion: NET_FW_IP_VERSION_;
    procedure Set_IpVersion(IpVersion: NET_FW_IP_VERSION_);
    function Get_Protocol: NET_FW_IP_PROTOCOL_;
    procedure Set_Protocol(ipProtocol: NET_FW_IP_PROTOCOL_);
    function Get_Port: Integer;
    procedure Set_Port(portNumber: Integer);
    function Get_Scope: NET_FW_SCOPE_;
    procedure Set_Scope(Scope: NET_FW_SCOPE_);
    function Get_RemoteAddresses: WideString;
    procedure Set_RemoteAddresses(const remoteAddrs: WideString);
    function Get_Enabled: WordBool;
    procedure Set_Enabled(Enabled: WordBool);
    function Get_BuiltIn: WordBool;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: INetFwOpenPort);
    procedure Disconnect; override;
    property DefaultInterface: INetFwOpenPort read GetDefaultInterface;
    property BuiltIn: WordBool read Get_BuiltIn;
    property Name: WideString read Get_Name write Set_Name;
    property IpVersion: NET_FW_IP_VERSION_ read Get_IpVersion write Set_IpVersion;
    property Protocol: NET_FW_IP_PROTOCOL_ read Get_Protocol write Set_Protocol;
    property Port: Integer read Get_Port write Set_Port;
    property Scope: NET_FW_SCOPE_ read Get_Scope write Set_Scope;
    property RemoteAddresses: WideString read Get_RemoteAddresses write Set_RemoteAddresses;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TNetFwOpenPortProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TNetFwOpenPort
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TNetFwOpenPortProperties = class(TPersistent)
  private
    FServer:    TNetFwOpenPort;
    function    GetDefaultInterface: INetFwOpenPort;
    constructor Create(AServer: TNetFwOpenPort);
  protected
    function Get_Name: WideString;
    procedure Set_Name(const Name: WideString);
    function Get_IpVersion: NET_FW_IP_VERSION_;
    procedure Set_IpVersion(IpVersion: NET_FW_IP_VERSION_);
    function Get_Protocol: NET_FW_IP_PROTOCOL_;
    procedure Set_Protocol(ipProtocol: NET_FW_IP_PROTOCOL_);
    function Get_Port: Integer;
    procedure Set_Port(portNumber: Integer);
    function Get_Scope: NET_FW_SCOPE_;
    procedure Set_Scope(Scope: NET_FW_SCOPE_);
    function Get_RemoteAddresses: WideString;
    procedure Set_RemoteAddresses(const remoteAddrs: WideString);
    function Get_Enabled: WordBool;
    procedure Set_Enabled(Enabled: WordBool);
    function Get_BuiltIn: WordBool;
  public
    property DefaultInterface: INetFwOpenPort read GetDefaultInterface;
  published
    property Name: WideString read Get_Name write Set_Name;
    property IpVersion: NET_FW_IP_VERSION_ read Get_IpVersion write Set_IpVersion;
    property Protocol: NET_FW_IP_PROTOCOL_ read Get_Protocol write Set_Protocol;
    property Port: Integer read Get_Port write Set_Port;
    property Scope: NET_FW_SCOPE_ read Get_Scope write Set_Scope;
    property RemoteAddresses: WideString read Get_RemoteAddresses write Set_RemoteAddresses;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoNetFwAuthorizedApplication provides a Create and CreateRemote method to          
// create instances of the default interface INetFwAuthorizedApplication exposed by              
// the CoClass NetFwAuthorizedApplication. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoNetFwAuthorizedApplication = class
    class function Create: INetFwAuthorizedApplication;
    class function CreateRemote(const MachineName: string): INetFwAuthorizedApplication;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TNetFwAuthorizedApplication
// Help String      : 
// Default Interface: INetFwAuthorizedApplication
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TNetFwAuthorizedApplicationProperties= class;
{$ENDIF}
  TNetFwAuthorizedApplication = class(TOleServer)
  private
    FIntf: INetFwAuthorizedApplication;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TNetFwAuthorizedApplicationProperties;
    function GetServerProperties: TNetFwAuthorizedApplicationProperties;
{$ENDIF}
    function GetDefaultInterface: INetFwAuthorizedApplication;
  protected
    procedure InitServerData; override;
    function Get_Name: WideString;
    procedure Set_Name(const Name: WideString);
    function Get_ProcessImageFileName: WideString;
    procedure Set_ProcessImageFileName(const imageFileName: WideString);
    function Get_IpVersion: NET_FW_IP_VERSION_;
    procedure Set_IpVersion(IpVersion: NET_FW_IP_VERSION_);
    function Get_Scope: NET_FW_SCOPE_;
    procedure Set_Scope(Scope: NET_FW_SCOPE_);
    function Get_RemoteAddresses: WideString;
    procedure Set_RemoteAddresses(const remoteAddrs: WideString);
    function Get_Enabled: WordBool;
    procedure Set_Enabled(Enabled: WordBool);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: INetFwAuthorizedApplication);
    procedure Disconnect; override;
    property DefaultInterface: INetFwAuthorizedApplication read GetDefaultInterface;
    property Name: WideString read Get_Name write Set_Name;
    property ProcessImageFileName: WideString read Get_ProcessImageFileName write Set_ProcessImageFileName;
    property IpVersion: NET_FW_IP_VERSION_ read Get_IpVersion write Set_IpVersion;
    property Scope: NET_FW_SCOPE_ read Get_Scope write Set_Scope;
    property RemoteAddresses: WideString read Get_RemoteAddresses write Set_RemoteAddresses;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TNetFwAuthorizedApplicationProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TNetFwAuthorizedApplication
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TNetFwAuthorizedApplicationProperties = class(TPersistent)
  private
    FServer:    TNetFwAuthorizedApplication;
    function    GetDefaultInterface: INetFwAuthorizedApplication;
    constructor Create(AServer: TNetFwAuthorizedApplication);
  protected
    function Get_Name: WideString;
    procedure Set_Name(const Name: WideString);
    function Get_ProcessImageFileName: WideString;
    procedure Set_ProcessImageFileName(const imageFileName: WideString);
    function Get_IpVersion: NET_FW_IP_VERSION_;
    procedure Set_IpVersion(IpVersion: NET_FW_IP_VERSION_);
    function Get_Scope: NET_FW_SCOPE_;
    procedure Set_Scope(Scope: NET_FW_SCOPE_);
    function Get_RemoteAddresses: WideString;
    procedure Set_RemoteAddresses(const remoteAddrs: WideString);
    function Get_Enabled: WordBool;
    procedure Set_Enabled(Enabled: WordBool);
  public
    property DefaultInterface: INetFwAuthorizedApplication read GetDefaultInterface;
  published
    property Name: WideString read Get_Name write Set_Name;
    property ProcessImageFileName: WideString read Get_ProcessImageFileName write Set_ProcessImageFileName;
    property IpVersion: NET_FW_IP_VERSION_ read Get_IpVersion write Set_IpVersion;
    property Scope: NET_FW_SCOPE_ read Get_Scope write Set_Scope;
    property RemoteAddresses: WideString read Get_RemoteAddresses write Set_RemoteAddresses;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoNetFwPolicy2 provides a Create and CreateRemote method to          
// create instances of the default interface INetFwPolicy2 exposed by              
// the CoClass NetFwPolicy2. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoNetFwPolicy2 = class
    class function Create: INetFwPolicy2;
    class function CreateRemote(const MachineName: string): INetFwPolicy2;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TNetFwPolicy2
// Help String      : 
// Default Interface: INetFwPolicy2
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TNetFwPolicy2Properties= class;
{$ENDIF}
  TNetFwPolicy2 = class(TOleServer)
  private
    FIntf: INetFwPolicy2;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TNetFwPolicy2Properties;
    function GetServerProperties: TNetFwPolicy2Properties;
{$ENDIF}
    function GetDefaultInterface: INetFwPolicy2;
  protected
    procedure InitServerData; override;
    function Get_CurrentProfileTypes: Integer;
    function Get_FirewallEnabled(profileType: NET_FW_PROFILE_TYPE2_): WordBool;
    procedure Set_FirewallEnabled(profileType: NET_FW_PROFILE_TYPE2_; Enabled: WordBool);
    function Get_ExcludedInterfaces(profileType: NET_FW_PROFILE_TYPE2_): OleVariant;
    procedure Set_ExcludedInterfaces(profileType: NET_FW_PROFILE_TYPE2_; Interfaces: OleVariant);
    function Get_BlockAllInboundTraffic(profileType: NET_FW_PROFILE_TYPE2_): WordBool;
    procedure Set_BlockAllInboundTraffic(profileType: NET_FW_PROFILE_TYPE2_; Block: WordBool);
    function Get_NotificationsDisabled(profileType: NET_FW_PROFILE_TYPE2_): WordBool;
    procedure Set_NotificationsDisabled(profileType: NET_FW_PROFILE_TYPE2_; disabled: WordBool);
    function Get_UnicastResponsesToMulticastBroadcastDisabled(profileType: NET_FW_PROFILE_TYPE2_): WordBool;
    procedure Set_UnicastResponsesToMulticastBroadcastDisabled(profileType: NET_FW_PROFILE_TYPE2_; 
                                                               disabled: WordBool);
    function Get_Rules: INetFwRules;
    function Get_ServiceRestriction: INetFwServiceRestriction;
    function Get_DefaultInboundAction(profileType: NET_FW_PROFILE_TYPE2_): NET_FW_ACTION_;
    procedure Set_DefaultInboundAction(profileType: NET_FW_PROFILE_TYPE2_; Action: NET_FW_ACTION_);
    function Get_DefaultOutboundAction(profileType: NET_FW_PROFILE_TYPE2_): NET_FW_ACTION_;
    procedure Set_DefaultOutboundAction(profileType: NET_FW_PROFILE_TYPE2_; Action: NET_FW_ACTION_);
    function Get_IsRuleGroupCurrentlyEnabled(const group: WideString): WordBool;
    function Get_LocalPolicyModifyState: NET_FW_MODIFY_STATE_;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: INetFwPolicy2);
    procedure Disconnect; override;
    procedure EnableRuleGroup(profileTypesBitmask: Integer; const group: WideString; 
                              enable: WordBool);
    function IsRuleGroupEnabled(profileTypesBitmask: Integer; const group: WideString): WordBool;
    procedure RestoreLocalFirewallDefaults;
    property DefaultInterface: INetFwPolicy2 read GetDefaultInterface;
    property CurrentProfileTypes: Integer read Get_CurrentProfileTypes;
    property FirewallEnabled[profileType: NET_FW_PROFILE_TYPE2_]: WordBool read Get_FirewallEnabled write Set_FirewallEnabled;
    property ExcludedInterfaces[profileType: NET_FW_PROFILE_TYPE2_]: OleVariant read Get_ExcludedInterfaces write Set_ExcludedInterfaces;
    property BlockAllInboundTraffic[profileType: NET_FW_PROFILE_TYPE2_]: WordBool read Get_BlockAllInboundTraffic write Set_BlockAllInboundTraffic;
    property NotificationsDisabled[profileType: NET_FW_PROFILE_TYPE2_]: WordBool read Get_NotificationsDisabled write Set_NotificationsDisabled;
    property UnicastResponsesToMulticastBroadcastDisabled[profileType: NET_FW_PROFILE_TYPE2_]: WordBool read Get_UnicastResponsesToMulticastBroadcastDisabled write Set_UnicastResponsesToMulticastBroadcastDisabled;
    property Rules: INetFwRules read Get_Rules;
    property ServiceRestriction: INetFwServiceRestriction read Get_ServiceRestriction;
    property DefaultInboundAction[profileType: NET_FW_PROFILE_TYPE2_]: NET_FW_ACTION_ read Get_DefaultInboundAction write Set_DefaultInboundAction;
    property DefaultOutboundAction[profileType: NET_FW_PROFILE_TYPE2_]: NET_FW_ACTION_ read Get_DefaultOutboundAction write Set_DefaultOutboundAction;
    property IsRuleGroupCurrentlyEnabled[const group: WideString]: WordBool read Get_IsRuleGroupCurrentlyEnabled;
    property LocalPolicyModifyState: NET_FW_MODIFY_STATE_ read Get_LocalPolicyModifyState;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TNetFwPolicy2Properties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TNetFwPolicy2
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TNetFwPolicy2Properties = class(TPersistent)
  private
    FServer:    TNetFwPolicy2;
    function    GetDefaultInterface: INetFwPolicy2;
    constructor Create(AServer: TNetFwPolicy2);
  protected
    function Get_CurrentProfileTypes: Integer;
    function Get_FirewallEnabled(profileType: NET_FW_PROFILE_TYPE2_): WordBool;
    procedure Set_FirewallEnabled(profileType: NET_FW_PROFILE_TYPE2_; Enabled: WordBool);
    function Get_ExcludedInterfaces(profileType: NET_FW_PROFILE_TYPE2_): OleVariant;
    procedure Set_ExcludedInterfaces(profileType: NET_FW_PROFILE_TYPE2_; Interfaces: OleVariant);
    function Get_BlockAllInboundTraffic(profileType: NET_FW_PROFILE_TYPE2_): WordBool;
    procedure Set_BlockAllInboundTraffic(profileType: NET_FW_PROFILE_TYPE2_; Block: WordBool);
    function Get_NotificationsDisabled(profileType: NET_FW_PROFILE_TYPE2_): WordBool;
    procedure Set_NotificationsDisabled(profileType: NET_FW_PROFILE_TYPE2_; disabled: WordBool);
    function Get_UnicastResponsesToMulticastBroadcastDisabled(profileType: NET_FW_PROFILE_TYPE2_): WordBool;
    procedure Set_UnicastResponsesToMulticastBroadcastDisabled(profileType: NET_FW_PROFILE_TYPE2_; 
                                                               disabled: WordBool);
    function Get_Rules: INetFwRules;
    function Get_ServiceRestriction: INetFwServiceRestriction;
    function Get_DefaultInboundAction(profileType: NET_FW_PROFILE_TYPE2_): NET_FW_ACTION_;
    procedure Set_DefaultInboundAction(profileType: NET_FW_PROFILE_TYPE2_; Action: NET_FW_ACTION_);
    function Get_DefaultOutboundAction(profileType: NET_FW_PROFILE_TYPE2_): NET_FW_ACTION_;
    procedure Set_DefaultOutboundAction(profileType: NET_FW_PROFILE_TYPE2_; Action: NET_FW_ACTION_);
    function Get_IsRuleGroupCurrentlyEnabled(const group: WideString): WordBool;
    function Get_LocalPolicyModifyState: NET_FW_MODIFY_STATE_;
  public
    property DefaultInterface: INetFwPolicy2 read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoNetFwMgr provides a Create and CreateRemote method to          
// create instances of the default interface INetFwMgr exposed by              
// the CoClass NetFwMgr. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoNetFwMgr = class
    class function Create: INetFwMgr;
    class function CreateRemote(const MachineName: string): INetFwMgr;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TNetFwMgr
// Help String      : 
// Default Interface: INetFwMgr
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TNetFwMgrProperties= class;
{$ENDIF}
  TNetFwMgr = class(TOleServer)
  private
    FIntf: INetFwMgr;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TNetFwMgrProperties;
    function GetServerProperties: TNetFwMgrProperties;
{$ENDIF}
    function GetDefaultInterface: INetFwMgr;
  protected
    procedure InitServerData; override;
    function Get_LocalPolicy: INetFwPolicy;
    function Get_CurrentProfileType: NET_FW_PROFILE_TYPE_;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: INetFwMgr);
    procedure Disconnect; override;
    procedure RestoreDefaults;
    procedure IsPortAllowed(const imageFileName: WideString; IpVersion: NET_FW_IP_VERSION_; 
                            portNumber: Integer; const localAddress: WideString; 
                            ipProtocol: NET_FW_IP_PROTOCOL_; out allowed: OleVariant; 
                            out restricted: OleVariant);
    procedure IsIcmpTypeAllowed(IpVersion: NET_FW_IP_VERSION_; const localAddress: WideString; 
                                Type_: Byte; out allowed: OleVariant; out restricted: OleVariant);
    property DefaultInterface: INetFwMgr read GetDefaultInterface;
    property LocalPolicy: INetFwPolicy read Get_LocalPolicy;
    property CurrentProfileType: NET_FW_PROFILE_TYPE_ read Get_CurrentProfileType;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TNetFwMgrProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TNetFwMgr
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TNetFwMgrProperties = class(TPersistent)
  private
    FServer:    TNetFwMgr;
    function    GetDefaultInterface: INetFwMgr;
    constructor Create(AServer: TNetFwMgr);
  protected
    function Get_LocalPolicy: INetFwPolicy;
    function Get_CurrentProfileType: NET_FW_PROFILE_TYPE_;
  public
    property DefaultInterface: INetFwMgr read GetDefaultInterface;
  published
  end;
{$ENDIF}


procedure Register;

resourcestring
  dtlServerPage = '(none)';

  dtlOcxPage = '(none)';

implementation

uses ComObj;

class function CoNetFwRule.Create: INetFwRule;
begin
  Result := CreateComObject(CLASS_NetFwRule) as INetFwRule;
end;

class function CoNetFwRule.CreateRemote(const MachineName: string): INetFwRule;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_NetFwRule) as INetFwRule;
end;

procedure TNetFwRule.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{2C5BC43E-3369-4C33-AB0C-BE9469677AF4}';
    IntfIID:   '{AF230D27-BABA-4E42-ACED-F524F22CFCE2}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TNetFwRule.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as INetFwRule;
  end;
end;

procedure TNetFwRule.ConnectTo(svrIntf: INetFwRule);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TNetFwRule.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TNetFwRule.GetDefaultInterface: INetFwRule;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TNetFwRule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TNetFwRuleProperties.Create(Self);
{$ENDIF}
end;

destructor TNetFwRule.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TNetFwRule.GetServerProperties: TNetFwRuleProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TNetFwRule.Get_Name: WideString;
begin
    Result := DefaultInterface.Name;
end;

procedure TNetFwRule.Set_Name(const Name: WideString);
  { Warning: The property Name has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Name := Name;
end;

function TNetFwRule.Get_Description: WideString;
begin
    Result := DefaultInterface.Description;
end;

procedure TNetFwRule.Set_Description(const desc: WideString);
  { Warning: The property Description has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Description := desc;
end;

function TNetFwRule.Get_ApplicationName: WideString;
begin
    Result := DefaultInterface.ApplicationName;
end;

procedure TNetFwRule.Set_ApplicationName(const imageFileName: WideString);
  { Warning: The property ApplicationName has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.ApplicationName := imageFileName;
end;

function TNetFwRule.Get_serviceName: WideString;
begin
    Result := DefaultInterface.serviceName;
end;

procedure TNetFwRule.Set_serviceName(const serviceName: WideString);
  { Warning: The property serviceName has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.serviceName := serviceName;
end;

function TNetFwRule.Get_Protocol: Integer;
begin
    Result := DefaultInterface.Protocol;
end;

procedure TNetFwRule.Set_Protocol(Protocol: Integer);
begin
  DefaultInterface.Set_Protocol(Protocol);
end;

function TNetFwRule.Get_LocalPorts: WideString;
begin
    Result := DefaultInterface.LocalPorts;
end;

procedure TNetFwRule.Set_LocalPorts(const portNumbers: WideString);
  { Warning: The property LocalPorts has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.LocalPorts := portNumbers;
end;

function TNetFwRule.Get_RemotePorts: WideString;
begin
    Result := DefaultInterface.RemotePorts;
end;

procedure TNetFwRule.Set_RemotePorts(const portNumbers: WideString);
  { Warning: The property RemotePorts has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.RemotePorts := portNumbers;
end;

function TNetFwRule.Get_LocalAddresses: WideString;
begin
    Result := DefaultInterface.LocalAddresses;
end;

procedure TNetFwRule.Set_LocalAddresses(const localAddrs: WideString);
  { Warning: The property LocalAddresses has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.LocalAddresses := localAddrs;
end;

function TNetFwRule.Get_RemoteAddresses: WideString;
begin
    Result := DefaultInterface.RemoteAddresses;
end;

procedure TNetFwRule.Set_RemoteAddresses(const remoteAddrs: WideString);
  { Warning: The property RemoteAddresses has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.RemoteAddresses := remoteAddrs;
end;

function TNetFwRule.Get_IcmpTypesAndCodes: WideString;
begin
    Result := DefaultInterface.IcmpTypesAndCodes;
end;

procedure TNetFwRule.Set_IcmpTypesAndCodes(const IcmpTypesAndCodes: WideString);
  { Warning: The property IcmpTypesAndCodes has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.IcmpTypesAndCodes := IcmpTypesAndCodes;
end;

function TNetFwRule.Get_Direction: NET_FW_RULE_DIRECTION_;
begin
    Result := DefaultInterface.Direction;
end;

procedure TNetFwRule.Set_Direction(dir: NET_FW_RULE_DIRECTION_);
begin
  DefaultInterface.Set_Direction(dir);
end;

function TNetFwRule.Get_Interfaces: OleVariant;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.Interfaces;
end;

procedure TNetFwRule.Set_Interfaces(Interfaces: OleVariant);
begin
  DefaultInterface.Set_Interfaces(Interfaces);
end;

function TNetFwRule.Get_InterfaceTypes: WideString;
begin
    Result := DefaultInterface.InterfaceTypes;
end;

procedure TNetFwRule.Set_InterfaceTypes(const InterfaceTypes: WideString);
  { Warning: The property InterfaceTypes has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.InterfaceTypes := InterfaceTypes;
end;

function TNetFwRule.Get_Enabled: WordBool;
begin
    Result := DefaultInterface.Enabled;
end;

procedure TNetFwRule.Set_Enabled(Enabled: WordBool);
begin
  DefaultInterface.Set_Enabled(Enabled);
end;

function TNetFwRule.Get_Grouping: WideString;
begin
    Result := DefaultInterface.Grouping;
end;

procedure TNetFwRule.Set_Grouping(const context: WideString);
  { Warning: The property Grouping has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Grouping := context;
end;

function TNetFwRule.Get_Profiles: Integer;
begin
    Result := DefaultInterface.Profiles;
end;

procedure TNetFwRule.Set_Profiles(profileTypesBitmask: Integer);
begin
  DefaultInterface.Set_Profiles(profileTypesBitmask);
end;

function TNetFwRule.Get_EdgeTraversal: WordBool;
begin
    Result := DefaultInterface.EdgeTraversal;
end;

procedure TNetFwRule.Set_EdgeTraversal(Enabled: WordBool);
begin
  DefaultInterface.Set_EdgeTraversal(Enabled);
end;

function TNetFwRule.Get_Action: NET_FW_ACTION_;
begin
    Result := DefaultInterface.Action;
end;

procedure TNetFwRule.Set_Action(Action: NET_FW_ACTION_);
begin
  DefaultInterface.Set_Action(Action);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TNetFwRuleProperties.Create(AServer: TNetFwRule);
begin
  inherited Create;
  FServer := AServer;
end;

function TNetFwRuleProperties.GetDefaultInterface: INetFwRule;
begin
  Result := FServer.DefaultInterface;
end;

function TNetFwRuleProperties.Get_Name: WideString;
begin
    Result := DefaultInterface.Name;
end;

procedure TNetFwRuleProperties.Set_Name(const Name: WideString);
  { Warning: The property Name has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Name := Name;
end;

function TNetFwRuleProperties.Get_Description: WideString;
begin
    Result := DefaultInterface.Description;
end;

procedure TNetFwRuleProperties.Set_Description(const desc: WideString);
  { Warning: The property Description has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Description := desc;
end;

function TNetFwRuleProperties.Get_ApplicationName: WideString;
begin
    Result := DefaultInterface.ApplicationName;
end;

procedure TNetFwRuleProperties.Set_ApplicationName(const imageFileName: WideString);
  { Warning: The property ApplicationName has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.ApplicationName := imageFileName;
end;

function TNetFwRuleProperties.Get_serviceName: WideString;
begin
    Result := DefaultInterface.serviceName;
end;

procedure TNetFwRuleProperties.Set_serviceName(const serviceName: WideString);
  { Warning: The property serviceName has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.serviceName := serviceName;
end;

function TNetFwRuleProperties.Get_Protocol: Integer;
begin
    Result := DefaultInterface.Protocol;
end;

procedure TNetFwRuleProperties.Set_Protocol(Protocol: Integer);
begin
  DefaultInterface.Set_Protocol(Protocol);
end;

function TNetFwRuleProperties.Get_LocalPorts: WideString;
begin
    Result := DefaultInterface.LocalPorts;
end;

procedure TNetFwRuleProperties.Set_LocalPorts(const portNumbers: WideString);
  { Warning: The property LocalPorts has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.LocalPorts := portNumbers;
end;

function TNetFwRuleProperties.Get_RemotePorts: WideString;
begin
    Result := DefaultInterface.RemotePorts;
end;

procedure TNetFwRuleProperties.Set_RemotePorts(const portNumbers: WideString);
  { Warning: The property RemotePorts has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.RemotePorts := portNumbers;
end;

function TNetFwRuleProperties.Get_LocalAddresses: WideString;
begin
    Result := DefaultInterface.LocalAddresses;
end;

procedure TNetFwRuleProperties.Set_LocalAddresses(const localAddrs: WideString);
  { Warning: The property LocalAddresses has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.LocalAddresses := localAddrs;
end;

function TNetFwRuleProperties.Get_RemoteAddresses: WideString;
begin
    Result := DefaultInterface.RemoteAddresses;
end;

procedure TNetFwRuleProperties.Set_RemoteAddresses(const remoteAddrs: WideString);
  { Warning: The property RemoteAddresses has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.RemoteAddresses := remoteAddrs;
end;

function TNetFwRuleProperties.Get_IcmpTypesAndCodes: WideString;
begin
    Result := DefaultInterface.IcmpTypesAndCodes;
end;

procedure TNetFwRuleProperties.Set_IcmpTypesAndCodes(const IcmpTypesAndCodes: WideString);
  { Warning: The property IcmpTypesAndCodes has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.IcmpTypesAndCodes := IcmpTypesAndCodes;
end;

function TNetFwRuleProperties.Get_Direction: NET_FW_RULE_DIRECTION_;
begin
    Result := DefaultInterface.Direction;
end;

procedure TNetFwRuleProperties.Set_Direction(dir: NET_FW_RULE_DIRECTION_);
begin
  DefaultInterface.Set_Direction(dir);
end;

function TNetFwRuleProperties.Get_Interfaces: OleVariant;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.Interfaces;
end;

procedure TNetFwRuleProperties.Set_Interfaces(Interfaces: OleVariant);
begin
  DefaultInterface.Set_Interfaces(Interfaces);
end;

function TNetFwRuleProperties.Get_InterfaceTypes: WideString;
begin
    Result := DefaultInterface.InterfaceTypes;
end;

procedure TNetFwRuleProperties.Set_InterfaceTypes(const InterfaceTypes: WideString);
  { Warning: The property InterfaceTypes has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.InterfaceTypes := InterfaceTypes;
end;

function TNetFwRuleProperties.Get_Enabled: WordBool;
begin
    Result := DefaultInterface.Enabled;
end;

procedure TNetFwRuleProperties.Set_Enabled(Enabled: WordBool);
begin
  DefaultInterface.Set_Enabled(Enabled);
end;

function TNetFwRuleProperties.Get_Grouping: WideString;
begin
    Result := DefaultInterface.Grouping;
end;

procedure TNetFwRuleProperties.Set_Grouping(const context: WideString);
  { Warning: The property Grouping has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Grouping := context;
end;

function TNetFwRuleProperties.Get_Profiles: Integer;
begin
    Result := DefaultInterface.Profiles;
end;

procedure TNetFwRuleProperties.Set_Profiles(profileTypesBitmask: Integer);
begin
  DefaultInterface.Set_Profiles(profileTypesBitmask);
end;

function TNetFwRuleProperties.Get_EdgeTraversal: WordBool;
begin
    Result := DefaultInterface.EdgeTraversal;
end;

procedure TNetFwRuleProperties.Set_EdgeTraversal(Enabled: WordBool);
begin
  DefaultInterface.Set_EdgeTraversal(Enabled);
end;

function TNetFwRuleProperties.Get_Action: NET_FW_ACTION_;
begin
    Result := DefaultInterface.Action;
end;

procedure TNetFwRuleProperties.Set_Action(Action: NET_FW_ACTION_);
begin
  DefaultInterface.Set_Action(Action);
end;

{$ENDIF}

class function CoNetFwOpenPort.Create: INetFwOpenPort;
begin
  Result := CreateComObject(CLASS_NetFwOpenPort) as INetFwOpenPort;
end;

class function CoNetFwOpenPort.CreateRemote(const MachineName: string): INetFwOpenPort;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_NetFwOpenPort) as INetFwOpenPort;
end;

procedure TNetFwOpenPort.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{0CA545C6-37AD-4A6C-BF92-9F7610067EF5}';
    IntfIID:   '{E0483BA0-47FF-4D9C-A6D6-7741D0B195F7}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TNetFwOpenPort.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as INetFwOpenPort;
  end;
end;

procedure TNetFwOpenPort.ConnectTo(svrIntf: INetFwOpenPort);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TNetFwOpenPort.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TNetFwOpenPort.GetDefaultInterface: INetFwOpenPort;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TNetFwOpenPort.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TNetFwOpenPortProperties.Create(Self);
{$ENDIF}
end;

destructor TNetFwOpenPort.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TNetFwOpenPort.GetServerProperties: TNetFwOpenPortProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TNetFwOpenPort.Get_Name: WideString;
begin
    Result := DefaultInterface.Name;
end;

procedure TNetFwOpenPort.Set_Name(const Name: WideString);
  { Warning: The property Name has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Name := Name;
end;

function TNetFwOpenPort.Get_IpVersion: NET_FW_IP_VERSION_;
begin
    Result := DefaultInterface.IpVersion;
end;

procedure TNetFwOpenPort.Set_IpVersion(IpVersion: NET_FW_IP_VERSION_);
begin
  DefaultInterface.Set_IpVersion(IpVersion);
end;

function TNetFwOpenPort.Get_Protocol: NET_FW_IP_PROTOCOL_;
begin
    Result := DefaultInterface.Protocol;
end;

procedure TNetFwOpenPort.Set_Protocol(ipProtocol: NET_FW_IP_PROTOCOL_);
begin
  DefaultInterface.Set_Protocol(ipProtocol);
end;

function TNetFwOpenPort.Get_Port: Integer;
begin
    Result := DefaultInterface.Port;
end;

procedure TNetFwOpenPort.Set_Port(portNumber: Integer);
begin
  DefaultInterface.Set_Port(portNumber);
end;

function TNetFwOpenPort.Get_Scope: NET_FW_SCOPE_;
begin
    Result := DefaultInterface.Scope;
end;

procedure TNetFwOpenPort.Set_Scope(Scope: NET_FW_SCOPE_);
begin
  DefaultInterface.Set_Scope(Scope);
end;

function TNetFwOpenPort.Get_RemoteAddresses: WideString;
begin
    Result := DefaultInterface.RemoteAddresses;
end;

procedure TNetFwOpenPort.Set_RemoteAddresses(const remoteAddrs: WideString);
  { Warning: The property RemoteAddresses has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.RemoteAddresses := remoteAddrs;
end;

function TNetFwOpenPort.Get_Enabled: WordBool;
begin
    Result := DefaultInterface.Enabled;
end;

procedure TNetFwOpenPort.Set_Enabled(Enabled: WordBool);
begin
  DefaultInterface.Set_Enabled(Enabled);
end;

function TNetFwOpenPort.Get_BuiltIn: WordBool;
begin
    Result := DefaultInterface.BuiltIn;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TNetFwOpenPortProperties.Create(AServer: TNetFwOpenPort);
begin
  inherited Create;
  FServer := AServer;
end;

function TNetFwOpenPortProperties.GetDefaultInterface: INetFwOpenPort;
begin
  Result := FServer.DefaultInterface;
end;

function TNetFwOpenPortProperties.Get_Name: WideString;
begin
    Result := DefaultInterface.Name;
end;

procedure TNetFwOpenPortProperties.Set_Name(const Name: WideString);
  { Warning: The property Name has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Name := Name;
end;

function TNetFwOpenPortProperties.Get_IpVersion: NET_FW_IP_VERSION_;
begin
    Result := DefaultInterface.IpVersion;
end;

procedure TNetFwOpenPortProperties.Set_IpVersion(IpVersion: NET_FW_IP_VERSION_);
begin
  DefaultInterface.Set_IpVersion(IpVersion);
end;

function TNetFwOpenPortProperties.Get_Protocol: NET_FW_IP_PROTOCOL_;
begin
    Result := DefaultInterface.Protocol;
end;

procedure TNetFwOpenPortProperties.Set_Protocol(ipProtocol: NET_FW_IP_PROTOCOL_);
begin
  DefaultInterface.Set_Protocol(ipProtocol);
end;

function TNetFwOpenPortProperties.Get_Port: Integer;
begin
    Result := DefaultInterface.Port;
end;

procedure TNetFwOpenPortProperties.Set_Port(portNumber: Integer);
begin
  DefaultInterface.Set_Port(portNumber);
end;

function TNetFwOpenPortProperties.Get_Scope: NET_FW_SCOPE_;
begin
    Result := DefaultInterface.Scope;
end;

procedure TNetFwOpenPortProperties.Set_Scope(Scope: NET_FW_SCOPE_);
begin
  DefaultInterface.Set_Scope(Scope);
end;

function TNetFwOpenPortProperties.Get_RemoteAddresses: WideString;
begin
    Result := DefaultInterface.RemoteAddresses;
end;

procedure TNetFwOpenPortProperties.Set_RemoteAddresses(const remoteAddrs: WideString);
  { Warning: The property RemoteAddresses has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.RemoteAddresses := remoteAddrs;
end;

function TNetFwOpenPortProperties.Get_Enabled: WordBool;
begin
    Result := DefaultInterface.Enabled;
end;

procedure TNetFwOpenPortProperties.Set_Enabled(Enabled: WordBool);
begin
  DefaultInterface.Set_Enabled(Enabled);
end;

function TNetFwOpenPortProperties.Get_BuiltIn: WordBool;
begin
    Result := DefaultInterface.BuiltIn;
end;

{$ENDIF}

class function CoNetFwAuthorizedApplication.Create: INetFwAuthorizedApplication;
begin
  Result := CreateComObject(CLASS_NetFwAuthorizedApplication) as INetFwAuthorizedApplication;
end;

class function CoNetFwAuthorizedApplication.CreateRemote(const MachineName: string): INetFwAuthorizedApplication;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_NetFwAuthorizedApplication) as INetFwAuthorizedApplication;
end;

procedure TNetFwAuthorizedApplication.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{EC9846B3-2762-4A6B-A214-6ACB603462D2}';
    IntfIID:   '{B5E64FFA-C2C5-444E-A301-FB5E00018050}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TNetFwAuthorizedApplication.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as INetFwAuthorizedApplication;
  end;
end;

procedure TNetFwAuthorizedApplication.ConnectTo(svrIntf: INetFwAuthorizedApplication);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TNetFwAuthorizedApplication.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TNetFwAuthorizedApplication.GetDefaultInterface: INetFwAuthorizedApplication;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TNetFwAuthorizedApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TNetFwAuthorizedApplicationProperties.Create(Self);
{$ENDIF}
end;

destructor TNetFwAuthorizedApplication.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TNetFwAuthorizedApplication.GetServerProperties: TNetFwAuthorizedApplicationProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TNetFwAuthorizedApplication.Get_Name: WideString;
begin
    Result := DefaultInterface.Name;
end;

procedure TNetFwAuthorizedApplication.Set_Name(const Name: WideString);
  { Warning: The property Name has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Name := Name;
end;

function TNetFwAuthorizedApplication.Get_ProcessImageFileName: WideString;
begin
    Result := DefaultInterface.ProcessImageFileName;
end;

procedure TNetFwAuthorizedApplication.Set_ProcessImageFileName(const imageFileName: WideString);
  { Warning: The property ProcessImageFileName has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.ProcessImageFileName := imageFileName;
end;

function TNetFwAuthorizedApplication.Get_IpVersion: NET_FW_IP_VERSION_;
begin
    Result := DefaultInterface.IpVersion;
end;

procedure TNetFwAuthorizedApplication.Set_IpVersion(IpVersion: NET_FW_IP_VERSION_);
begin
  DefaultInterface.Set_IpVersion(IpVersion);
end;

function TNetFwAuthorizedApplication.Get_Scope: NET_FW_SCOPE_;
begin
    Result := DefaultInterface.Scope;
end;

procedure TNetFwAuthorizedApplication.Set_Scope(Scope: NET_FW_SCOPE_);
begin
  DefaultInterface.Set_Scope(Scope);
end;

function TNetFwAuthorizedApplication.Get_RemoteAddresses: WideString;
begin
    Result := DefaultInterface.RemoteAddresses;
end;

procedure TNetFwAuthorizedApplication.Set_RemoteAddresses(const remoteAddrs: WideString);
  { Warning: The property RemoteAddresses has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.RemoteAddresses := remoteAddrs;
end;

function TNetFwAuthorizedApplication.Get_Enabled: WordBool;
begin
    Result := DefaultInterface.Enabled;
end;

procedure TNetFwAuthorizedApplication.Set_Enabled(Enabled: WordBool);
begin
  DefaultInterface.Set_Enabled(Enabled);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TNetFwAuthorizedApplicationProperties.Create(AServer: TNetFwAuthorizedApplication);
begin
  inherited Create;
  FServer := AServer;
end;

function TNetFwAuthorizedApplicationProperties.GetDefaultInterface: INetFwAuthorizedApplication;
begin
  Result := FServer.DefaultInterface;
end;

function TNetFwAuthorizedApplicationProperties.Get_Name: WideString;
begin
    Result := DefaultInterface.Name;
end;

procedure TNetFwAuthorizedApplicationProperties.Set_Name(const Name: WideString);
  { Warning: The property Name has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Name := Name;
end;

function TNetFwAuthorizedApplicationProperties.Get_ProcessImageFileName: WideString;
begin
    Result := DefaultInterface.ProcessImageFileName;
end;

procedure TNetFwAuthorizedApplicationProperties.Set_ProcessImageFileName(const imageFileName: WideString);
  { Warning: The property ProcessImageFileName has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.ProcessImageFileName := imageFileName;
end;

function TNetFwAuthorizedApplicationProperties.Get_IpVersion: NET_FW_IP_VERSION_;
begin
    Result := DefaultInterface.IpVersion;
end;

procedure TNetFwAuthorizedApplicationProperties.Set_IpVersion(IpVersion: NET_FW_IP_VERSION_);
begin
  DefaultInterface.Set_IpVersion(IpVersion);
end;

function TNetFwAuthorizedApplicationProperties.Get_Scope: NET_FW_SCOPE_;
begin
    Result := DefaultInterface.Scope;
end;

procedure TNetFwAuthorizedApplicationProperties.Set_Scope(Scope: NET_FW_SCOPE_);
begin
  DefaultInterface.Set_Scope(Scope);
end;

function TNetFwAuthorizedApplicationProperties.Get_RemoteAddresses: WideString;
begin
    Result := DefaultInterface.RemoteAddresses;
end;

procedure TNetFwAuthorizedApplicationProperties.Set_RemoteAddresses(const remoteAddrs: WideString);
  { Warning: The property RemoteAddresses has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.RemoteAddresses := remoteAddrs;
end;

function TNetFwAuthorizedApplicationProperties.Get_Enabled: WordBool;
begin
    Result := DefaultInterface.Enabled;
end;

procedure TNetFwAuthorizedApplicationProperties.Set_Enabled(Enabled: WordBool);
begin
  DefaultInterface.Set_Enabled(Enabled);
end;

{$ENDIF}

class function CoNetFwPolicy2.Create: INetFwPolicy2;
begin
  Result := CreateComObject(CLASS_NetFwPolicy2) as INetFwPolicy2;
end;

class function CoNetFwPolicy2.CreateRemote(const MachineName: string): INetFwPolicy2;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_NetFwPolicy2) as INetFwPolicy2;
end;

procedure TNetFwPolicy2.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{E2B3C97F-6AE1-41AC-817A-F6F92166D7DD}';
    IntfIID:   '{98325047-C671-4174-8D81-DEFCD3F03186}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TNetFwPolicy2.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as INetFwPolicy2;
  end;
end;

procedure TNetFwPolicy2.ConnectTo(svrIntf: INetFwPolicy2);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TNetFwPolicy2.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TNetFwPolicy2.GetDefaultInterface: INetFwPolicy2;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TNetFwPolicy2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TNetFwPolicy2Properties.Create(Self);
{$ENDIF}
end;

destructor TNetFwPolicy2.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TNetFwPolicy2.GetServerProperties: TNetFwPolicy2Properties;
begin
  Result := FProps;
end;
{$ENDIF}

function TNetFwPolicy2.Get_CurrentProfileTypes: Integer;
begin
    Result := DefaultInterface.CurrentProfileTypes;
end;

function TNetFwPolicy2.Get_FirewallEnabled(profileType: NET_FW_PROFILE_TYPE2_): WordBool;
begin
    Result := DefaultInterface.FirewallEnabled[profileType];
end;

procedure TNetFwPolicy2.Set_FirewallEnabled(profileType: NET_FW_PROFILE_TYPE2_; Enabled: WordBool);
begin
  DefaultInterface.FirewallEnabled[profileType] := Enabled;
end;

function TNetFwPolicy2.Get_ExcludedInterfaces(profileType: NET_FW_PROFILE_TYPE2_): OleVariant;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.ExcludedInterfaces[profileType];
end;

procedure TNetFwPolicy2.Set_ExcludedInterfaces(profileType: NET_FW_PROFILE_TYPE2_; 
                                               Interfaces: OleVariant);
begin
  DefaultInterface.ExcludedInterfaces[profileType] := Interfaces;
end;

function TNetFwPolicy2.Get_BlockAllInboundTraffic(profileType: NET_FW_PROFILE_TYPE2_): WordBool;
begin
    Result := DefaultInterface.BlockAllInboundTraffic[profileType];
end;

procedure TNetFwPolicy2.Set_BlockAllInboundTraffic(profileType: NET_FW_PROFILE_TYPE2_; 
                                                   Block: WordBool);
begin
  DefaultInterface.BlockAllInboundTraffic[profileType] := Block;
end;

function TNetFwPolicy2.Get_NotificationsDisabled(profileType: NET_FW_PROFILE_TYPE2_): WordBool;
begin
    Result := DefaultInterface.NotificationsDisabled[profileType];
end;

procedure TNetFwPolicy2.Set_NotificationsDisabled(profileType: NET_FW_PROFILE_TYPE2_; 
                                                  disabled: WordBool);
begin
  DefaultInterface.NotificationsDisabled[profileType] := disabled;
end;

function TNetFwPolicy2.Get_UnicastResponsesToMulticastBroadcastDisabled(profileType: NET_FW_PROFILE_TYPE2_): WordBool;
begin
    Result := DefaultInterface.UnicastResponsesToMulticastBroadcastDisabled[profileType];
end;

procedure TNetFwPolicy2.Set_UnicastResponsesToMulticastBroadcastDisabled(profileType: NET_FW_PROFILE_TYPE2_; 
                                                                         disabled: WordBool);
begin
  DefaultInterface.UnicastResponsesToMulticastBroadcastDisabled[profileType] := disabled;
end;

function TNetFwPolicy2.Get_Rules: INetFwRules;
begin
    Result := DefaultInterface.Rules;
end;

function TNetFwPolicy2.Get_ServiceRestriction: INetFwServiceRestriction;
begin
    Result := DefaultInterface.ServiceRestriction;
end;

function TNetFwPolicy2.Get_DefaultInboundAction(profileType: NET_FW_PROFILE_TYPE2_): NET_FW_ACTION_;
begin
    Result := DefaultInterface.DefaultInboundAction[profileType];
end;

procedure TNetFwPolicy2.Set_DefaultInboundAction(profileType: NET_FW_PROFILE_TYPE2_; 
                                                 Action: NET_FW_ACTION_);
begin
  DefaultInterface.DefaultInboundAction[profileType] := Action;
end;

function TNetFwPolicy2.Get_DefaultOutboundAction(profileType: NET_FW_PROFILE_TYPE2_): NET_FW_ACTION_;
begin
    Result := DefaultInterface.DefaultOutboundAction[profileType];
end;

procedure TNetFwPolicy2.Set_DefaultOutboundAction(profileType: NET_FW_PROFILE_TYPE2_; 
                                                  Action: NET_FW_ACTION_);
begin
  DefaultInterface.DefaultOutboundAction[profileType] := Action;
end;

function TNetFwPolicy2.Get_IsRuleGroupCurrentlyEnabled(const group: WideString): WordBool;
begin
    Result := DefaultInterface.IsRuleGroupCurrentlyEnabled[group];
end;

function TNetFwPolicy2.Get_LocalPolicyModifyState: NET_FW_MODIFY_STATE_;
begin
    Result := DefaultInterface.LocalPolicyModifyState;
end;

procedure TNetFwPolicy2.EnableRuleGroup(profileTypesBitmask: Integer; const group: WideString; 
                                        enable: WordBool);
begin
  DefaultInterface.EnableRuleGroup(profileTypesBitmask, group, enable);
end;

function TNetFwPolicy2.IsRuleGroupEnabled(profileTypesBitmask: Integer; const group: WideString): WordBool;
begin
  Result := DefaultInterface.IsRuleGroupEnabled(profileTypesBitmask, group);
end;

procedure TNetFwPolicy2.RestoreLocalFirewallDefaults;
begin
  DefaultInterface.RestoreLocalFirewallDefaults;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TNetFwPolicy2Properties.Create(AServer: TNetFwPolicy2);
begin
  inherited Create;
  FServer := AServer;
end;

function TNetFwPolicy2Properties.GetDefaultInterface: INetFwPolicy2;
begin
  Result := FServer.DefaultInterface;
end;

function TNetFwPolicy2Properties.Get_CurrentProfileTypes: Integer;
begin
    Result := DefaultInterface.CurrentProfileTypes;
end;

function TNetFwPolicy2Properties.Get_FirewallEnabled(profileType: NET_FW_PROFILE_TYPE2_): WordBool;
begin
    Result := DefaultInterface.FirewallEnabled[profileType];
end;

procedure TNetFwPolicy2Properties.Set_FirewallEnabled(profileType: NET_FW_PROFILE_TYPE2_; 
                                                      Enabled: WordBool);
begin
  DefaultInterface.FirewallEnabled[profileType] := Enabled;
end;

function TNetFwPolicy2Properties.Get_ExcludedInterfaces(profileType: NET_FW_PROFILE_TYPE2_): OleVariant;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.ExcludedInterfaces[profileType];
end;

procedure TNetFwPolicy2Properties.Set_ExcludedInterfaces(profileType: NET_FW_PROFILE_TYPE2_; 
                                                         Interfaces: OleVariant);
begin
  DefaultInterface.ExcludedInterfaces[profileType] := Interfaces;
end;

function TNetFwPolicy2Properties.Get_BlockAllInboundTraffic(profileType: NET_FW_PROFILE_TYPE2_): WordBool;
begin
    Result := DefaultInterface.BlockAllInboundTraffic[profileType];
end;

procedure TNetFwPolicy2Properties.Set_BlockAllInboundTraffic(profileType: NET_FW_PROFILE_TYPE2_; 
                                                             Block: WordBool);
begin
  DefaultInterface.BlockAllInboundTraffic[profileType] := Block;
end;

function TNetFwPolicy2Properties.Get_NotificationsDisabled(profileType: NET_FW_PROFILE_TYPE2_): WordBool;
begin
    Result := DefaultInterface.NotificationsDisabled[profileType];
end;

procedure TNetFwPolicy2Properties.Set_NotificationsDisabled(profileType: NET_FW_PROFILE_TYPE2_; 
                                                            disabled: WordBool);
begin
  DefaultInterface.NotificationsDisabled[profileType] := disabled;
end;

function TNetFwPolicy2Properties.Get_UnicastResponsesToMulticastBroadcastDisabled(profileType: NET_FW_PROFILE_TYPE2_): WordBool;
begin
    Result := DefaultInterface.UnicastResponsesToMulticastBroadcastDisabled[profileType];
end;

procedure TNetFwPolicy2Properties.Set_UnicastResponsesToMulticastBroadcastDisabled(profileType: NET_FW_PROFILE_TYPE2_; 
                                                                                   disabled: WordBool);
begin
  DefaultInterface.UnicastResponsesToMulticastBroadcastDisabled[profileType] := disabled;
end;

function TNetFwPolicy2Properties.Get_Rules: INetFwRules;
begin
    Result := DefaultInterface.Rules;
end;

function TNetFwPolicy2Properties.Get_ServiceRestriction: INetFwServiceRestriction;
begin
    Result := DefaultInterface.ServiceRestriction;
end;

function TNetFwPolicy2Properties.Get_DefaultInboundAction(profileType: NET_FW_PROFILE_TYPE2_): NET_FW_ACTION_;
begin
    Result := DefaultInterface.DefaultInboundAction[profileType];
end;

procedure TNetFwPolicy2Properties.Set_DefaultInboundAction(profileType: NET_FW_PROFILE_TYPE2_; 
                                                           Action: NET_FW_ACTION_);
begin
  DefaultInterface.DefaultInboundAction[profileType] := Action;
end;

function TNetFwPolicy2Properties.Get_DefaultOutboundAction(profileType: NET_FW_PROFILE_TYPE2_): NET_FW_ACTION_;
begin
    Result := DefaultInterface.DefaultOutboundAction[profileType];
end;

procedure TNetFwPolicy2Properties.Set_DefaultOutboundAction(profileType: NET_FW_PROFILE_TYPE2_; 
                                                            Action: NET_FW_ACTION_);
begin
  DefaultInterface.DefaultOutboundAction[profileType] := Action;
end;

function TNetFwPolicy2Properties.Get_IsRuleGroupCurrentlyEnabled(const group: WideString): WordBool;
begin
    Result := DefaultInterface.IsRuleGroupCurrentlyEnabled[group];
end;

function TNetFwPolicy2Properties.Get_LocalPolicyModifyState: NET_FW_MODIFY_STATE_;
begin
    Result := DefaultInterface.LocalPolicyModifyState;
end;

{$ENDIF}

class function CoNetFwMgr.Create: INetFwMgr;
begin
  Result := CreateComObject(CLASS_NetFwMgr) as INetFwMgr;
end;

class function CoNetFwMgr.CreateRemote(const MachineName: string): INetFwMgr;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_NetFwMgr) as INetFwMgr;
end;

procedure TNetFwMgr.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{304CE942-6E39-40D8-943A-B913C40C9CD4}';
    IntfIID:   '{F7898AF5-CAC4-4632-A2EC-DA06E5111AF2}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TNetFwMgr.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as INetFwMgr;
  end;
end;

procedure TNetFwMgr.ConnectTo(svrIntf: INetFwMgr);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TNetFwMgr.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TNetFwMgr.GetDefaultInterface: INetFwMgr;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TNetFwMgr.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TNetFwMgrProperties.Create(Self);
{$ENDIF}
end;

destructor TNetFwMgr.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TNetFwMgr.GetServerProperties: TNetFwMgrProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TNetFwMgr.Get_LocalPolicy: INetFwPolicy;
begin
    Result := DefaultInterface.LocalPolicy;
end;

function TNetFwMgr.Get_CurrentProfileType: NET_FW_PROFILE_TYPE_;
begin
    Result := DefaultInterface.CurrentProfileType;
end;

procedure TNetFwMgr.RestoreDefaults;
begin
  DefaultInterface.RestoreDefaults;
end;

procedure TNetFwMgr.IsPortAllowed(const imageFileName: WideString; IpVersion: NET_FW_IP_VERSION_; 
                                  portNumber: Integer; const localAddress: WideString; 
                                  ipProtocol: NET_FW_IP_PROTOCOL_; out allowed: OleVariant; 
                                  out restricted: OleVariant);
begin
  DefaultInterface.IsPortAllowed(imageFileName, IpVersion, portNumber, localAddress, ipProtocol, 
                                 allowed, restricted);
end;

procedure TNetFwMgr.IsIcmpTypeAllowed(IpVersion: NET_FW_IP_VERSION_; 
                                      const localAddress: WideString; Type_: Byte; 
                                      out allowed: OleVariant; out restricted: OleVariant);
begin
  DefaultInterface.IsIcmpTypeAllowed(IpVersion, localAddress, Type_, allowed, restricted);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TNetFwMgrProperties.Create(AServer: TNetFwMgr);
begin
  inherited Create;
  FServer := AServer;
end;

function TNetFwMgrProperties.GetDefaultInterface: INetFwMgr;
begin
  Result := FServer.DefaultInterface;
end;

function TNetFwMgrProperties.Get_LocalPolicy: INetFwPolicy;
begin
    Result := DefaultInterface.LocalPolicy;
end;

function TNetFwMgrProperties.Get_CurrentProfileType: NET_FW_PROFILE_TYPE_;
begin
    Result := DefaultInterface.CurrentProfileType;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents(dtlServerPage, [TNetFwRule, TNetFwOpenPort, TNetFwAuthorizedApplication, TNetFwPolicy2, 
    TNetFwMgr]);
end;

end.
