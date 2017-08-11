unit EventSystemLib_TLB;

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
// File generated on 03.06.2008 20:50:27 from Type Library described below.

// ************************************************************************  //
// Type Lib: jwapi\trunk\COM\EventSystemLib.tlb (1)
// LIBID: {E81221DC-C4D8-11D1-B653-00805FC79216}
// LCID: 0
// Helpfile:
// HelpString: EventSystem 1.0 Type Library
// DepndLst:
//   (1) v2.0 stdole, (C:\Windows\system32\stdole2.tlb)
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
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  DummyEventSystemLibMajorVersion = 1;
  DummyEventSystemLibMinorVersion = 0;

  LIBID_DummyEventSystemLib: TGUID = '{E81221DC-C4D8-11D1-B653-00805FC79216}';

  IID_IEventSystem: TGUID = '{4E14FB9F-2E22-11D1-9964-00C04FBBB345}';
  CLASS_CEventSystem: TGUID = '{4E14FBA2-2E22-11D1-9964-00C04FBBB345}';
  IID_IEventPublisher: TGUID = '{E341516B-2E32-11D1-9964-00C04FBBB345}';
  CLASS_CEventPublisher: TGUID = '{AB944620-79C6-11D1-88F9-0080C7D771BF}';
  IID_IEventObjectCollection: TGUID = '{F89AC270-D4EB-11D1-B682-00805FC79216}';
  IID_IEnumEventObject: TGUID = '{F4A07D63-2E25-11D1-9964-00C04FBBB345}';
  IID_IEventClass: TGUID = '{FB2B72A0-7A68-11D1-88F9-0080C7D771BF}';
  CLASS_CEventClass: TGUID = '{CDBEC9C0-7A68-11D1-88F9-0080C7D771BF}';
  IID_IEventSubscription: TGUID = '{4A6B0E15-2E38-11D1-9965-00C04FBBB345}';
  CLASS_CEventSubscription: TGUID = '{7542E960-79C7-11D1-88F9-0080C7D771BF}';
  IID_IEventObjectChange: TGUID = '{F4A07D70-2E25-11D1-9964-00C04FBBB345}';
  CLASS_EventObjectChange: TGUID = '{D0565000-9DF4-11D1-A281-00C04FCA0AA7}';
  IID_IEventObjectChange2: TGUID = '{7701A9C3-BD68-438F-83E0-67BF4F53A422}';
  CLASS_EventObjectChange2: TGUID = '{BB07BACD-CD56-4E63-A8FF-CBF0355FB9F4}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum __MIDL_IEventObjectChange_0001
type
  __MIDL_IEventObjectChange_0001 = TOleEnum;
const
  EOC_NewObject = $00000000;
  EOC_ModifiedObject = $00000001;
  EOC_DeletedObject = $00000002;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IEventSystem = interface;
  IEventSystemDisp = dispinterface;
  IEventPublisher = interface;
  IEventPublisherDisp = dispinterface;
  IEventObjectCollection = interface;
  IEventObjectCollectionDisp = dispinterface;
  IEnumEventObject = interface;
  IEventClass = interface;
  IEventClassDisp = dispinterface;
  IEventSubscription = interface;
  IEventSubscriptionDisp = dispinterface;
  IEventObjectChange = interface;
  IEventObjectChange2 = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  CEventSystem = IEventSystem;
  CEventPublisher = IEventPublisher;
  CEventClass = IEventClass;
  CEventSubscription = IEventSubscription;
  EventObjectChange = IEventObjectChange;
  EventObjectChange2 = IEventObjectChange2;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  POleVariant1 = ^OleVariant; {*}
  PUserType1 = ^COMEVENTSYSCHANGEINFO; {*}

  EOC_ChangeType = __MIDL_IEventObjectChange_0001; 

  __MIDL___MIDL_itf_EventSys_0000_0009_0001 = packed record
    cbSize: LongWord;
    changeType: __MIDL_IEventObjectChange_0001;
    objectID: WideString;
    partitionId: WideString;
    applicationId: WideString;
    reserved: array[0..9] of TGUID;
  end;

  COMEVENTSYSCHANGEINFO = __MIDL___MIDL_itf_EventSys_0000_0009_0001; 

// *********************************************************************//
// Interface: IEventSystem
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4E14FB9F-2E22-11D1-9964-00C04FBBB345}
// *********************************************************************//
  IEventSystem = interface(IDispatch)
    ['{4E14FB9F-2E22-11D1-9964-00C04FBBB345}']
    function Query(const progID: WideString; const queryCriteria: WideString; out errorIndex: SYSINT): IUnknown; safecall;
    procedure Store(const progID: WideString; const pInterface: IUnknown); safecall;
    procedure Remove(const progID: WideString; const queryCriteria: WideString; 
                     out errorIndex: SYSINT); safecall;
    function Get_EventObjectChangeEventClassID: WideString; safecall;
    function QueryS(const progID: WideString; const queryCriteria: WideString): IUnknown; safecall;
    procedure RemoveS(const progID: WideString; const queryCriteria: WideString); safecall;
    property EventObjectChangeEventClassID: WideString read Get_EventObjectChangeEventClassID;
  end;

// *********************************************************************//
// DispIntf:  IEventSystemDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4E14FB9F-2E22-11D1-9964-00C04FBBB345}
// *********************************************************************//
  IEventSystemDisp = dispinterface
    ['{4E14FB9F-2E22-11D1-9964-00C04FBBB345}']
    function Query(const progID: WideString; const queryCriteria: WideString; out errorIndex: SYSINT): IUnknown; dispid 1;
    procedure Store(const progID: WideString; const pInterface: IUnknown); dispid 2;
    procedure Remove(const progID: WideString; const queryCriteria: WideString; 
                     out errorIndex: SYSINT); dispid 3;
    property EventObjectChangeEventClassID: WideString readonly dispid 4;
    function QueryS(const progID: WideString; const queryCriteria: WideString): IUnknown; dispid 5;
    procedure RemoveS(const progID: WideString; const queryCriteria: WideString); dispid 6;
  end;

// *********************************************************************//
// Interface: IEventPublisher
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E341516B-2E32-11D1-9964-00C04FBBB345}
// *********************************************************************//
  IEventPublisher = interface(IDispatch)
    ['{E341516B-2E32-11D1-9964-00C04FBBB345}']
    function Get_PublisherID: WideString; safecall;
    procedure Set_PublisherID(const pbstrPublisherID: WideString); safecall;
    function Get_PublisherName: WideString; safecall;
    procedure Set_PublisherName(const pbstrPublisherName: WideString); safecall;
    function Get_PublisherType: WideString; safecall;
    procedure Set_PublisherType(const pbstrPublisherType: WideString); safecall;
    function Get_OwnerSID: WideString; safecall;
    procedure Set_OwnerSID(const pbstrOwnerSID: WideString); safecall;
    function Get_Description: WideString; safecall;
    procedure Set_Description(const pbstrDescription: WideString); safecall;
    function GetDefaultProperty(const bstrPropertyName: WideString): OleVariant; safecall;
    procedure PutDefaultProperty(const bstrPropertyName: WideString; var propertyValue: OleVariant); safecall;
    procedure RemoveDefaultProperty(const bstrPropertyName: WideString); safecall;
    function GetDefaultPropertyCollection: IEventObjectCollection; safecall;
    property PublisherID: WideString read Get_PublisherID write Set_PublisherID;
    property PublisherName: WideString read Get_PublisherName write Set_PublisherName;
    property PublisherType: WideString read Get_PublisherType write Set_PublisherType;
    property OwnerSID: WideString read Get_OwnerSID write Set_OwnerSID;
    property Description: WideString read Get_Description write Set_Description;
  end;

// *********************************************************************//
// DispIntf:  IEventPublisherDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E341516B-2E32-11D1-9964-00C04FBBB345}
// *********************************************************************//
  IEventPublisherDisp = dispinterface
    ['{E341516B-2E32-11D1-9964-00C04FBBB345}']
    property PublisherID: WideString dispid 1;
    property PublisherName: WideString dispid 2;
    property PublisherType: WideString dispid 3;
    property OwnerSID: WideString dispid 4;
    property Description: WideString dispid 5;
    function GetDefaultProperty(const bstrPropertyName: WideString): OleVariant; dispid 6;
    procedure PutDefaultProperty(const bstrPropertyName: WideString; var propertyValue: OleVariant); dispid 7;
    procedure RemoveDefaultProperty(const bstrPropertyName: WideString); dispid 8;
    function GetDefaultPropertyCollection: IEventObjectCollection; dispid 9;
  end;

// *********************************************************************//
// Interface: IEventObjectCollection
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F89AC270-D4EB-11D1-B682-00805FC79216}
// *********************************************************************//
  IEventObjectCollection = interface(IDispatch)
    ['{F89AC270-D4EB-11D1-B682-00805FC79216}']
    function Get__NewEnum: IUnknown; safecall;
    function Get_Item(const objectID: WideString): OleVariant; safecall;
    function Get_NewEnum: IEnumEventObject; safecall;
    function Get_Count: Integer; safecall;
    procedure Add(var Item: OleVariant; const objectID: WideString); safecall;
    procedure Remove(const objectID: WideString); safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Item[const objectID: WideString]: OleVariant read Get_Item; default;
    property NewEnum: IEnumEventObject read Get_NewEnum;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  IEventObjectCollectionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F89AC270-D4EB-11D1-B682-00805FC79216}
// *********************************************************************//
  IEventObjectCollectionDisp = dispinterface
    ['{F89AC270-D4EB-11D1-B682-00805FC79216}']
    property _NewEnum: IUnknown readonly dispid -4;
    property Item[const objectID: WideString]: OleVariant readonly dispid 0; default;
    property NewEnum: IEnumEventObject readonly dispid 1;
    property Count: Integer readonly dispid 2;
    procedure Add(var Item: OleVariant; const objectID: WideString); dispid 3;
    procedure Remove(const objectID: WideString); dispid 4;
  end;

// *********************************************************************//
// Interface: IEnumEventObject
// Flags:     (0)
// GUID:      {F4A07D63-2E25-11D1-9964-00C04FBBB345}
// *********************************************************************//
  IEnumEventObject = interface(IUnknown)
    ['{F4A07D63-2E25-11D1-9964-00C04FBBB345}']
    function Clone(out ppInterface: IEnumEventObject): HResult; stdcall;
    function Next(cReqElem: LongWord; out ppInterface: IUnknown; out cRetElem: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Skip(cSkipElem: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEventClass
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {FB2B72A0-7A68-11D1-88F9-0080C7D771BF}
// *********************************************************************//
  IEventClass = interface(IDispatch)
    ['{FB2B72A0-7A68-11D1-88F9-0080C7D771BF}']
    function Get_EventClassID: WideString; safecall;
    procedure Set_EventClassID(const pbstrEventClassID: WideString); safecall;
    function Get_EventClassName: WideString; safecall;
    procedure Set_EventClassName(const pbstrEventClassName: WideString); safecall;
    function Get_OwnerSID: WideString; safecall;
    procedure Set_OwnerSID(const pbstrOwnerSID: WideString); safecall;
    function Get_FiringInterfaceID: WideString; safecall;
    procedure Set_FiringInterfaceID(const pbstrFiringInterfaceID: WideString); safecall;
    function Get_Description: WideString; safecall;
    procedure Set_Description(const pbstrDescription: WideString); safecall;
    function Get_CustomConfigCLSID: WideString; safecall;
    procedure Set_CustomConfigCLSID(const pbstrCustomConfigCLSID: WideString); safecall;
    function Get_TypeLib: WideString; safecall;
    procedure Set_TypeLib(const pbstrTypeLib: WideString); safecall;
    property EventClassID: WideString read Get_EventClassID write Set_EventClassID;
    property EventClassName: WideString read Get_EventClassName write Set_EventClassName;
    property OwnerSID: WideString read Get_OwnerSID write Set_OwnerSID;
    property FiringInterfaceID: WideString read Get_FiringInterfaceID write Set_FiringInterfaceID;
    property Description: WideString read Get_Description write Set_Description;
    property CustomConfigCLSID: WideString read Get_CustomConfigCLSID write Set_CustomConfigCLSID;
    property TypeLib: WideString read Get_TypeLib write Set_TypeLib;
  end;

// *********************************************************************//
// DispIntf:  IEventClassDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {FB2B72A0-7A68-11D1-88F9-0080C7D771BF}
// *********************************************************************//
  IEventClassDisp = dispinterface
    ['{FB2B72A0-7A68-11D1-88F9-0080C7D771BF}']
    property EventClassID: WideString dispid 1;
    property EventClassName: WideString dispid 2;
    property OwnerSID: WideString dispid 3;
    property FiringInterfaceID: WideString dispid 4;
    property Description: WideString dispid 5;
    property CustomConfigCLSID: WideString dispid 6;
    property TypeLib: WideString dispid 7;
  end;

// *********************************************************************//
// Interface: IEventSubscription
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4A6B0E15-2E38-11D1-9965-00C04FBBB345}
// *********************************************************************//
  IEventSubscription = interface(IDispatch)
    ['{4A6B0E15-2E38-11D1-9965-00C04FBBB345}']
    function Get_SubscriptionID: WideString; safecall;
    procedure Set_SubscriptionID(const pbstrSubscriptionID: WideString); safecall;
    function Get_SubscriptionName: WideString; safecall;
    procedure Set_SubscriptionName(const pbstrSubscriptionName: WideString); safecall;
    function Get_PublisherID: WideString; safecall;
    procedure Set_PublisherID(const pbstrPublisherID: WideString); safecall;
    function Get_EventClassID: WideString; safecall;
    procedure Set_EventClassID(const pbstrEventClassID: WideString); safecall;
    function Get_MethodName: WideString; safecall;
    procedure Set_MethodName(const pbstrMethodName: WideString); safecall;
    function Get_SubscriberCLSID: WideString; safecall;
    procedure Set_SubscriberCLSID(const pbstrSubscriberCLSID: WideString); safecall;
    function Get_SubscriberInterface: IUnknown; safecall;
    procedure Set_SubscriberInterface(const ppSubscriberInterface: IUnknown); safecall;
    function Get_PerUser: Integer; safecall;
    procedure Set_PerUser(pfPerUser: Integer); safecall;
    function Get_OwnerSID: WideString; safecall;
    procedure Set_OwnerSID(const pbstrOwnerSID: WideString); safecall;
    function Get_Enabled: Integer; safecall;
    procedure Set_Enabled(pfEnabled: Integer); safecall;
    function Get_Description: WideString; safecall;
    procedure Set_Description(const pbstrDescription: WideString); safecall;
    function Get_MachineName: WideString; safecall;
    procedure Set_MachineName(const pbstrMachineName: WideString); safecall;
    function GetPublisherProperty(const bstrPropertyName: WideString): OleVariant; safecall;
    procedure PutPublisherProperty(const bstrPropertyName: WideString; var propertyValue: OleVariant); safecall;
    procedure RemovePublisherProperty(const bstrPropertyName: WideString); safecall;
    function GetPublisherPropertyCollection: IEventObjectCollection; safecall;
    function GetSubscriberProperty(const bstrPropertyName: WideString): OleVariant; safecall;
    procedure PutSubscriberProperty(const bstrPropertyName: WideString; 
                                    var propertyValue: OleVariant); safecall;
    procedure RemoveSubscriberProperty(const bstrPropertyName: WideString); safecall;
    function GetSubscriberPropertyCollection: IEventObjectCollection; safecall;
    function Get_InterfaceID: WideString; safecall;
    procedure Set_InterfaceID(const pbstrInterfaceID: WideString); safecall;
    property SubscriptionID: WideString read Get_SubscriptionID write Set_SubscriptionID;
    property SubscriptionName: WideString read Get_SubscriptionName write Set_SubscriptionName;
    property PublisherID: WideString read Get_PublisherID write Set_PublisherID;
    property EventClassID: WideString read Get_EventClassID write Set_EventClassID;
    property MethodName: WideString read Get_MethodName write Set_MethodName;
    property SubscriberCLSID: WideString read Get_SubscriberCLSID write Set_SubscriberCLSID;
    property SubscriberInterface: IUnknown read Get_SubscriberInterface write Set_SubscriberInterface;
    property PerUser: Integer read Get_PerUser write Set_PerUser;
    property OwnerSID: WideString read Get_OwnerSID write Set_OwnerSID;
    property Enabled: Integer read Get_Enabled write Set_Enabled;
    property Description: WideString read Get_Description write Set_Description;
    property MachineName: WideString read Get_MachineName write Set_MachineName;
    property InterfaceID: WideString read Get_InterfaceID write Set_InterfaceID;
  end;

// *********************************************************************//
// DispIntf:  IEventSubscriptionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4A6B0E15-2E38-11D1-9965-00C04FBBB345}
// *********************************************************************//
  IEventSubscriptionDisp = dispinterface
    ['{4A6B0E15-2E38-11D1-9965-00C04FBBB345}']
    property SubscriptionID: WideString dispid 1;
    property SubscriptionName: WideString dispid 2;
    property PublisherID: WideString dispid 3;
    property EventClassID: WideString dispid 4;
    property MethodName: WideString dispid 5;
    property SubscriberCLSID: WideString dispid 6;
    property SubscriberInterface: IUnknown dispid 7;
    property PerUser: Integer dispid 8;
    property OwnerSID: WideString dispid 9;
    property Enabled: Integer dispid 10;
    property Description: WideString dispid 11;
    property MachineName: WideString dispid 12;
    function GetPublisherProperty(const bstrPropertyName: WideString): OleVariant; dispid 13;
    procedure PutPublisherProperty(const bstrPropertyName: WideString; var propertyValue: OleVariant); dispid 14;
    procedure RemovePublisherProperty(const bstrPropertyName: WideString); dispid 15;
    function GetPublisherPropertyCollection: IEventObjectCollection; dispid 16;
    function GetSubscriberProperty(const bstrPropertyName: WideString): OleVariant; dispid 17;
    procedure PutSubscriberProperty(const bstrPropertyName: WideString; 
                                    var propertyValue: OleVariant); dispid 18;
    procedure RemoveSubscriberProperty(const bstrPropertyName: WideString); dispid 19;
    function GetSubscriberPropertyCollection: IEventObjectCollection; dispid 20;
    property InterfaceID: WideString dispid 21;
  end;

// *********************************************************************//
// Interface: IEventObjectChange
// Flags:     (0)
// GUID:      {F4A07D70-2E25-11D1-9964-00C04FBBB345}
// *********************************************************************//
  IEventObjectChange = interface(IUnknown)
    ['{F4A07D70-2E25-11D1-9964-00C04FBBB345}']
    function ChangedSubscription(changeType: EOC_ChangeType; const bstrSubscriptionID: WideString): HResult; stdcall;
    function ChangedEventClass(changeType: EOC_ChangeType; const bstrEventClassID: WideString): HResult; stdcall;
    function ChangedPublisher(changeType: EOC_ChangeType; const bstrPublisherID: WideString): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEventObjectChange2
// Flags:     (0)
// GUID:      {7701A9C3-BD68-438F-83E0-67BF4F53A422}
// *********************************************************************//
  IEventObjectChange2 = interface(IUnknown)
    ['{7701A9C3-BD68-438F-83E0-67BF4F53A422}']
    function ChangedSubscription(var pInfo: COMEVENTSYSCHANGEINFO): HResult; stdcall;
    function ChangedEventClass(var pInfo: COMEVENTSYSCHANGEINFO): HResult; stdcall;
  end;

// *********************************************************************//
// The Class CoCEventSystem provides a Create and CreateRemote method to          
// create instances of the default interface IEventSystem exposed by              
// the CoClass CEventSystem. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCEventSystem = class
    class function Create: IEventSystem;
    class function CreateRemote(const MachineName: string): IEventSystem;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCEventSystem
// Help String      : CEventSystem Class
// Default Interface: IEventSystem
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TCEventSystemProperties= class;
{$ENDIF}
  TCEventSystem = class(TOleServer)
  private
    FIntf: IEventSystem;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TCEventSystemProperties;
    function GetServerProperties: TCEventSystemProperties;
{$ENDIF}
    function GetDefaultInterface: IEventSystem;
  protected
    procedure InitServerData; override;
    function Get_EventObjectChangeEventClassID: WideString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IEventSystem);
    procedure Disconnect; override;
    function Query(const progID: WideString; const queryCriteria: WideString; out errorIndex: SYSINT): IUnknown;
    procedure Store(const progID: WideString; const pInterface: IUnknown);
    procedure Remove(const progID: WideString; const queryCriteria: WideString; 
                     out errorIndex: SYSINT);
    function QueryS(const progID: WideString; const queryCriteria: WideString): IUnknown;
    procedure RemoveS(const progID: WideString; const queryCriteria: WideString);
    property DefaultInterface: IEventSystem read GetDefaultInterface;
    property EventObjectChangeEventClassID: WideString read Get_EventObjectChangeEventClassID;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TCEventSystemProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TCEventSystem
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TCEventSystemProperties = class(TPersistent)
  private
    FServer:    TCEventSystem;
    function    GetDefaultInterface: IEventSystem;
    constructor Create(AServer: TCEventSystem);
  protected
    function Get_EventObjectChangeEventClassID: WideString;
  public
    property DefaultInterface: IEventSystem read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoCEventPublisher provides a Create and CreateRemote method to          
// create instances of the default interface IEventPublisher exposed by              
// the CoClass CEventPublisher. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCEventPublisher = class
    class function Create: IEventPublisher;
    class function CreateRemote(const MachineName: string): IEventPublisher;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCEventPublisher
// Help String      : 
// Default Interface: IEventPublisher
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TCEventPublisherProperties= class;
{$ENDIF}
  TCEventPublisher = class(TOleServer)
  private
    FIntf: IEventPublisher;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TCEventPublisherProperties;
    function GetServerProperties: TCEventPublisherProperties;
{$ENDIF}
    function GetDefaultInterface: IEventPublisher;
  protected
    procedure InitServerData; override;
    function Get_PublisherID: WideString;
    procedure Set_PublisherID(const pbstrPublisherID: WideString);
    function Get_PublisherName: WideString;
    procedure Set_PublisherName(const pbstrPublisherName: WideString);
    function Get_PublisherType: WideString;
    procedure Set_PublisherType(const pbstrPublisherType: WideString);
    function Get_OwnerSID: WideString;
    procedure Set_OwnerSID(const pbstrOwnerSID: WideString);
    function Get_Description: WideString;
    procedure Set_Description(const pbstrDescription: WideString);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IEventPublisher);
    procedure Disconnect; override;
    function GetDefaultProperty(const bstrPropertyName: WideString): OleVariant;
    procedure PutDefaultProperty(const bstrPropertyName: WideString; var propertyValue: OleVariant);
    procedure RemoveDefaultProperty(const bstrPropertyName: WideString);
    function GetDefaultPropertyCollection: IEventObjectCollection;
    property DefaultInterface: IEventPublisher read GetDefaultInterface;
    property PublisherID: WideString read Get_PublisherID write Set_PublisherID;
    property PublisherName: WideString read Get_PublisherName write Set_PublisherName;
    property PublisherType: WideString read Get_PublisherType write Set_PublisherType;
    property OwnerSID: WideString read Get_OwnerSID write Set_OwnerSID;
    property Description: WideString read Get_Description write Set_Description;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TCEventPublisherProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TCEventPublisher
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TCEventPublisherProperties = class(TPersistent)
  private
    FServer:    TCEventPublisher;
    function    GetDefaultInterface: IEventPublisher;
    constructor Create(AServer: TCEventPublisher);
  protected
    function Get_PublisherID: WideString;
    procedure Set_PublisherID(const pbstrPublisherID: WideString);
    function Get_PublisherName: WideString;
    procedure Set_PublisherName(const pbstrPublisherName: WideString);
    function Get_PublisherType: WideString;
    procedure Set_PublisherType(const pbstrPublisherType: WideString);
    function Get_OwnerSID: WideString;
    procedure Set_OwnerSID(const pbstrOwnerSID: WideString);
    function Get_Description: WideString;
    procedure Set_Description(const pbstrDescription: WideString);
  public
    property DefaultInterface: IEventPublisher read GetDefaultInterface;
  published
    property PublisherID: WideString read Get_PublisherID write Set_PublisherID;
    property PublisherName: WideString read Get_PublisherName write Set_PublisherName;
    property PublisherType: WideString read Get_PublisherType write Set_PublisherType;
    property OwnerSID: WideString read Get_OwnerSID write Set_OwnerSID;
    property Description: WideString read Get_Description write Set_Description;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoCEventClass provides a Create and CreateRemote method to          
// create instances of the default interface IEventClass exposed by              
// the CoClass CEventClass. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCEventClass = class
    class function Create: IEventClass;
    class function CreateRemote(const MachineName: string): IEventClass;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCEventClass
// Help String      : 
// Default Interface: IEventClass
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TCEventClassProperties= class;
{$ENDIF}
  TCEventClass = class(TOleServer)
  private
    FIntf: IEventClass;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TCEventClassProperties;
    function GetServerProperties: TCEventClassProperties;
{$ENDIF}
    function GetDefaultInterface: IEventClass;
  protected
    procedure InitServerData; override;
    function Get_EventClassID: WideString;
    procedure Set_EventClassID(const pbstrEventClassID: WideString);
    function Get_EventClassName: WideString;
    procedure Set_EventClassName(const pbstrEventClassName: WideString);
    function Get_OwnerSID: WideString;
    procedure Set_OwnerSID(const pbstrOwnerSID: WideString);
    function Get_FiringInterfaceID: WideString;
    procedure Set_FiringInterfaceID(const pbstrFiringInterfaceID: WideString);
    function Get_Description: WideString;
    procedure Set_Description(const pbstrDescription: WideString);
    function Get_CustomConfigCLSID: WideString;
    procedure Set_CustomConfigCLSID(const pbstrCustomConfigCLSID: WideString);
    function Get_TypeLib: WideString;
    procedure Set_TypeLib(const pbstrTypeLib: WideString);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IEventClass);
    procedure Disconnect; override;
    property DefaultInterface: IEventClass read GetDefaultInterface;
    property EventClassID: WideString read Get_EventClassID write Set_EventClassID;
    property EventClassName: WideString read Get_EventClassName write Set_EventClassName;
    property OwnerSID: WideString read Get_OwnerSID write Set_OwnerSID;
    property FiringInterfaceID: WideString read Get_FiringInterfaceID write Set_FiringInterfaceID;
    property Description: WideString read Get_Description write Set_Description;
    property CustomConfigCLSID: WideString read Get_CustomConfigCLSID write Set_CustomConfigCLSID;
    property TypeLib: WideString read Get_TypeLib write Set_TypeLib;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TCEventClassProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TCEventClass
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TCEventClassProperties = class(TPersistent)
  private
    FServer:    TCEventClass;
    function    GetDefaultInterface: IEventClass;
    constructor Create(AServer: TCEventClass);
  protected
    function Get_EventClassID: WideString;
    procedure Set_EventClassID(const pbstrEventClassID: WideString);
    function Get_EventClassName: WideString;
    procedure Set_EventClassName(const pbstrEventClassName: WideString);
    function Get_OwnerSID: WideString;
    procedure Set_OwnerSID(const pbstrOwnerSID: WideString);
    function Get_FiringInterfaceID: WideString;
    procedure Set_FiringInterfaceID(const pbstrFiringInterfaceID: WideString);
    function Get_Description: WideString;
    procedure Set_Description(const pbstrDescription: WideString);
    function Get_CustomConfigCLSID: WideString;
    procedure Set_CustomConfigCLSID(const pbstrCustomConfigCLSID: WideString);
    function Get_TypeLib: WideString;
    procedure Set_TypeLib(const pbstrTypeLib: WideString);
  public
    property DefaultInterface: IEventClass read GetDefaultInterface;
  published
    property EventClassID: WideString read Get_EventClassID write Set_EventClassID;
    property EventClassName: WideString read Get_EventClassName write Set_EventClassName;
    property OwnerSID: WideString read Get_OwnerSID write Set_OwnerSID;
    property FiringInterfaceID: WideString read Get_FiringInterfaceID write Set_FiringInterfaceID;
    property Description: WideString read Get_Description write Set_Description;
    property CustomConfigCLSID: WideString read Get_CustomConfigCLSID write Set_CustomConfigCLSID;
    property TypeLib: WideString read Get_TypeLib write Set_TypeLib;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoCEventSubscription provides a Create and CreateRemote method to          
// create instances of the default interface IEventSubscription exposed by              
// the CoClass CEventSubscription. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCEventSubscription = class
    class function Create: IEventSubscription;
    class function CreateRemote(const MachineName: string): IEventSubscription;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCEventSubscription
// Help String      : 
// Default Interface: IEventSubscription
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TCEventSubscriptionProperties= class;
{$ENDIF}
  TCEventSubscription = class(TOleServer)
  private
    FIntf: IEventSubscription;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TCEventSubscriptionProperties;
    function GetServerProperties: TCEventSubscriptionProperties;
{$ENDIF}
    function GetDefaultInterface: IEventSubscription;
  protected
    procedure InitServerData; override;
    function Get_SubscriptionID: WideString;
    procedure Set_SubscriptionID(const pbstrSubscriptionID: WideString);
    function Get_SubscriptionName: WideString;
    procedure Set_SubscriptionName(const pbstrSubscriptionName: WideString);
    function Get_PublisherID: WideString;
    procedure Set_PublisherID(const pbstrPublisherID: WideString);
    function Get_EventClassID: WideString;
    procedure Set_EventClassID(const pbstrEventClassID: WideString);
    function Get_MethodName: WideString;
    procedure Set_MethodName(const pbstrMethodName: WideString);
    function Get_SubscriberCLSID: WideString;
    procedure Set_SubscriberCLSID(const pbstrSubscriberCLSID: WideString);
    function Get_SubscriberInterface: IUnknown;
    procedure Set_SubscriberInterface(const ppSubscriberInterface: IUnknown);
    function Get_PerUser: Integer;
    procedure Set_PerUser(pfPerUser: Integer);
    function Get_OwnerSID: WideString;
    procedure Set_OwnerSID(const pbstrOwnerSID: WideString);
    function Get_Enabled: Integer;
    procedure Set_Enabled(pfEnabled: Integer);
    function Get_Description: WideString;
    procedure Set_Description(const pbstrDescription: WideString);
    function Get_MachineName: WideString;
    procedure Set_MachineName(const pbstrMachineName: WideString);
    function Get_InterfaceID: WideString;
    procedure Set_InterfaceID(const pbstrInterfaceID: WideString);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IEventSubscription);
    procedure Disconnect; override;
    function GetPublisherProperty(const bstrPropertyName: WideString): OleVariant;
    procedure PutPublisherProperty(const bstrPropertyName: WideString; var propertyValue: OleVariant);
    procedure RemovePublisherProperty(const bstrPropertyName: WideString);
    function GetPublisherPropertyCollection: IEventObjectCollection;
    function GetSubscriberProperty(const bstrPropertyName: WideString): OleVariant;
    procedure PutSubscriberProperty(const bstrPropertyName: WideString; 
                                    var propertyValue: OleVariant);
    procedure RemoveSubscriberProperty(const bstrPropertyName: WideString);
    function GetSubscriberPropertyCollection: IEventObjectCollection;
    property DefaultInterface: IEventSubscription read GetDefaultInterface;
    property SubscriberInterface: IUnknown read Get_SubscriberInterface write Set_SubscriberInterface;
    property SubscriptionID: WideString read Get_SubscriptionID write Set_SubscriptionID;
    property SubscriptionName: WideString read Get_SubscriptionName write Set_SubscriptionName;
    property PublisherID: WideString read Get_PublisherID write Set_PublisherID;
    property EventClassID: WideString read Get_EventClassID write Set_EventClassID;
    property MethodName: WideString read Get_MethodName write Set_MethodName;
    property SubscriberCLSID: WideString read Get_SubscriberCLSID write Set_SubscriberCLSID;
    property PerUser: Integer read Get_PerUser write Set_PerUser;
    property OwnerSID: WideString read Get_OwnerSID write Set_OwnerSID;
    property Enabled: Integer read Get_Enabled write Set_Enabled;
    property Description: WideString read Get_Description write Set_Description;
    property MachineName: WideString read Get_MachineName write Set_MachineName;
    property InterfaceID: WideString read Get_InterfaceID write Set_InterfaceID;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TCEventSubscriptionProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TCEventSubscription
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TCEventSubscriptionProperties = class(TPersistent)
  private
    FServer:    TCEventSubscription;
    function    GetDefaultInterface: IEventSubscription;
    constructor Create(AServer: TCEventSubscription);
  protected
    function Get_SubscriptionID: WideString;
    procedure Set_SubscriptionID(const pbstrSubscriptionID: WideString);
    function Get_SubscriptionName: WideString;
    procedure Set_SubscriptionName(const pbstrSubscriptionName: WideString);
    function Get_PublisherID: WideString;
    procedure Set_PublisherID(const pbstrPublisherID: WideString);
    function Get_EventClassID: WideString;
    procedure Set_EventClassID(const pbstrEventClassID: WideString);
    function Get_MethodName: WideString;
    procedure Set_MethodName(const pbstrMethodName: WideString);
    function Get_SubscriberCLSID: WideString;
    procedure Set_SubscriberCLSID(const pbstrSubscriberCLSID: WideString);
    function Get_SubscriberInterface: IUnknown;
    procedure Set_SubscriberInterface(const ppSubscriberInterface: IUnknown);
    function Get_PerUser: Integer;
    procedure Set_PerUser(pfPerUser: Integer);
    function Get_OwnerSID: WideString;
    procedure Set_OwnerSID(const pbstrOwnerSID: WideString);
    function Get_Enabled: Integer;
    procedure Set_Enabled(pfEnabled: Integer);
    function Get_Description: WideString;
    procedure Set_Description(const pbstrDescription: WideString);
    function Get_MachineName: WideString;
    procedure Set_MachineName(const pbstrMachineName: WideString);
    function Get_InterfaceID: WideString;
    procedure Set_InterfaceID(const pbstrInterfaceID: WideString);
  public
    property DefaultInterface: IEventSubscription read GetDefaultInterface;
  published
    property SubscriptionID: WideString read Get_SubscriptionID write Set_SubscriptionID;
    property SubscriptionName: WideString read Get_SubscriptionName write Set_SubscriptionName;
    property PublisherID: WideString read Get_PublisherID write Set_PublisherID;
    property EventClassID: WideString read Get_EventClassID write Set_EventClassID;
    property MethodName: WideString read Get_MethodName write Set_MethodName;
    property SubscriberCLSID: WideString read Get_SubscriberCLSID write Set_SubscriberCLSID;
    property PerUser: Integer read Get_PerUser write Set_PerUser;
    property OwnerSID: WideString read Get_OwnerSID write Set_OwnerSID;
    property Enabled: Integer read Get_Enabled write Set_Enabled;
    property Description: WideString read Get_Description write Set_Description;
    property MachineName: WideString read Get_MachineName write Set_MachineName;
    property InterfaceID: WideString read Get_InterfaceID write Set_InterfaceID;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoEventObjectChange provides a Create and CreateRemote method to          
// create instances of the default interface IEventObjectChange exposed by              
// the CoClass EventObjectChange. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoEventObjectChange = class
    class function Create: IEventObjectChange;
    class function CreateRemote(const MachineName: string): IEventObjectChange;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TEventObjectChange
// Help String      : 
// Default Interface: IEventObjectChange
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TEventObjectChangeProperties= class;
{$ENDIF}
  TEventObjectChange = class(TOleServer)
  private
    FIntf: IEventObjectChange;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TEventObjectChangeProperties;
    function GetServerProperties: TEventObjectChangeProperties;
{$ENDIF}
    function GetDefaultInterface: IEventObjectChange;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IEventObjectChange);
    procedure Disconnect; override;
    function ChangedSubscription(changeType: EOC_ChangeType; const bstrSubscriptionID: WideString): HResult;
    function ChangedEventClass(changeType: EOC_ChangeType; const bstrEventClassID: WideString): HResult;
    function ChangedPublisher(changeType: EOC_ChangeType; const bstrPublisherID: WideString): HResult;
    property DefaultInterface: IEventObjectChange read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TEventObjectChangeProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TEventObjectChange
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TEventObjectChangeProperties = class(TPersistent)
  private
    FServer:    TEventObjectChange;
    function    GetDefaultInterface: IEventObjectChange;
    constructor Create(AServer: TEventObjectChange);
  protected
  public
    property DefaultInterface: IEventObjectChange read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoEventObjectChange2 provides a Create and CreateRemote method to          
// create instances of the default interface IEventObjectChange2 exposed by              
// the CoClass EventObjectChange2. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoEventObjectChange2 = class
    class function Create: IEventObjectChange2;
    class function CreateRemote(const MachineName: string): IEventObjectChange2;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TEventObjectChange2
// Help String      : 
// Default Interface: IEventObjectChange2
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TEventObjectChange2Properties= class;
{$ENDIF}
  TEventObjectChange2 = class(TOleServer)
  private
    FIntf: IEventObjectChange2;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TEventObjectChange2Properties;
    function GetServerProperties: TEventObjectChange2Properties;
{$ENDIF}
    function GetDefaultInterface: IEventObjectChange2;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IEventObjectChange2);
    procedure Disconnect; override;
    function ChangedSubscription(var pInfo: COMEVENTSYSCHANGEINFO): HResult;
    function ChangedEventClass(var pInfo: COMEVENTSYSCHANGEINFO): HResult;
    property DefaultInterface: IEventObjectChange2 read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TEventObjectChange2Properties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TEventObjectChange2
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TEventObjectChange2Properties = class(TPersistent)
  private
    FServer:    TEventObjectChange2;
    function    GetDefaultInterface: IEventObjectChange2;
    constructor Create(AServer: TEventObjectChange2);
  protected
  public
    property DefaultInterface: IEventObjectChange2 read GetDefaultInterface;
  published
  end;
{$ENDIF}


procedure Register;

resourcestring
  dtlServerPage = '(none)';

  dtlOcxPage = '(none)';

implementation

uses ComObj;

class function CoCEventSystem.Create: IEventSystem;
begin
  Result := CreateComObject(CLASS_CEventSystem) as IEventSystem;
end;

class function CoCEventSystem.CreateRemote(const MachineName: string): IEventSystem;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CEventSystem) as IEventSystem;
end;

procedure TCEventSystem.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{4E14FBA2-2E22-11D1-9964-00C04FBBB345}';
    IntfIID:   '{4E14FB9F-2E22-11D1-9964-00C04FBBB345}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCEventSystem.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IEventSystem;
  end;
end;

procedure TCEventSystem.ConnectTo(svrIntf: IEventSystem);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCEventSystem.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCEventSystem.GetDefaultInterface: IEventSystem;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TCEventSystem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TCEventSystemProperties.Create(Self);
{$ENDIF}
end;

destructor TCEventSystem.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TCEventSystem.GetServerProperties: TCEventSystemProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TCEventSystem.Get_EventObjectChangeEventClassID: WideString;
begin
    Result := DefaultInterface.EventObjectChangeEventClassID;
end;

function TCEventSystem.Query(const progID: WideString; const queryCriteria: WideString; 
                             out errorIndex: SYSINT): IUnknown;
begin
  Result := DefaultInterface.Query(progID, queryCriteria, errorIndex);
end;

procedure TCEventSystem.Store(const progID: WideString; const pInterface: IUnknown);
begin
  DefaultInterface.Store(progID, pInterface);
end;

procedure TCEventSystem.Remove(const progID: WideString; const queryCriteria: WideString; 
                               out errorIndex: SYSINT);
begin
  DefaultInterface.Remove(progID, queryCriteria, errorIndex);
end;

function TCEventSystem.QueryS(const progID: WideString; const queryCriteria: WideString): IUnknown;
begin
  Result := DefaultInterface.QueryS(progID, queryCriteria);
end;

procedure TCEventSystem.RemoveS(const progID: WideString; const queryCriteria: WideString);
begin
  DefaultInterface.RemoveS(progID, queryCriteria);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TCEventSystemProperties.Create(AServer: TCEventSystem);
begin
  inherited Create;
  FServer := AServer;
end;

function TCEventSystemProperties.GetDefaultInterface: IEventSystem;
begin
  Result := FServer.DefaultInterface;
end;

function TCEventSystemProperties.Get_EventObjectChangeEventClassID: WideString;
begin
    Result := DefaultInterface.EventObjectChangeEventClassID;
end;

{$ENDIF}

class function CoCEventPublisher.Create: IEventPublisher;
begin
  Result := CreateComObject(CLASS_CEventPublisher) as IEventPublisher;
end;

class function CoCEventPublisher.CreateRemote(const MachineName: string): IEventPublisher;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CEventPublisher) as IEventPublisher;
end;

procedure TCEventPublisher.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{AB944620-79C6-11D1-88F9-0080C7D771BF}';
    IntfIID:   '{E341516B-2E32-11D1-9964-00C04FBBB345}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCEventPublisher.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IEventPublisher;
  end;
end;

procedure TCEventPublisher.ConnectTo(svrIntf: IEventPublisher);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCEventPublisher.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCEventPublisher.GetDefaultInterface: IEventPublisher;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TCEventPublisher.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TCEventPublisherProperties.Create(Self);
{$ENDIF}
end;

destructor TCEventPublisher.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TCEventPublisher.GetServerProperties: TCEventPublisherProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TCEventPublisher.Get_PublisherID: WideString;
begin
    Result := DefaultInterface.PublisherID;
end;

procedure TCEventPublisher.Set_PublisherID(const pbstrPublisherID: WideString);
  { Warning: The property PublisherID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.PublisherID := pbstrPublisherID;
end;

function TCEventPublisher.Get_PublisherName: WideString;
begin
    Result := DefaultInterface.PublisherName;
end;

procedure TCEventPublisher.Set_PublisherName(const pbstrPublisherName: WideString);
  { Warning: The property PublisherName has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.PublisherName := pbstrPublisherName;
end;

function TCEventPublisher.Get_PublisherType: WideString;
begin
    Result := DefaultInterface.PublisherType;
end;

procedure TCEventPublisher.Set_PublisherType(const pbstrPublisherType: WideString);
  { Warning: The property PublisherType has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.PublisherType := pbstrPublisherType;
end;

function TCEventPublisher.Get_OwnerSID: WideString;
begin
    Result := DefaultInterface.OwnerSID;
end;

procedure TCEventPublisher.Set_OwnerSID(const pbstrOwnerSID: WideString);
  { Warning: The property OwnerSID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.OwnerSID := pbstrOwnerSID;
end;

function TCEventPublisher.Get_Description: WideString;
begin
    Result := DefaultInterface.Description;
end;

procedure TCEventPublisher.Set_Description(const pbstrDescription: WideString);
  { Warning: The property Description has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Description := pbstrDescription;
end;

function TCEventPublisher.GetDefaultProperty(const bstrPropertyName: WideString): OleVariant;
begin
  Result := DefaultInterface.GetDefaultProperty(bstrPropertyName);
end;

procedure TCEventPublisher.PutDefaultProperty(const bstrPropertyName: WideString; 
                                              var propertyValue: OleVariant);
begin
  DefaultInterface.PutDefaultProperty(bstrPropertyName, propertyValue);
end;

procedure TCEventPublisher.RemoveDefaultProperty(const bstrPropertyName: WideString);
begin
  DefaultInterface.RemoveDefaultProperty(bstrPropertyName);
end;

function TCEventPublisher.GetDefaultPropertyCollection: IEventObjectCollection;
begin
  Result := DefaultInterface.GetDefaultPropertyCollection;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TCEventPublisherProperties.Create(AServer: TCEventPublisher);
begin
  inherited Create;
  FServer := AServer;
end;

function TCEventPublisherProperties.GetDefaultInterface: IEventPublisher;
begin
  Result := FServer.DefaultInterface;
end;

function TCEventPublisherProperties.Get_PublisherID: WideString;
begin
    Result := DefaultInterface.PublisherID;
end;

procedure TCEventPublisherProperties.Set_PublisherID(const pbstrPublisherID: WideString);
  { Warning: The property PublisherID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.PublisherID := pbstrPublisherID;
end;

function TCEventPublisherProperties.Get_PublisherName: WideString;
begin
    Result := DefaultInterface.PublisherName;
end;

procedure TCEventPublisherProperties.Set_PublisherName(const pbstrPublisherName: WideString);
  { Warning: The property PublisherName has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.PublisherName := pbstrPublisherName;
end;

function TCEventPublisherProperties.Get_PublisherType: WideString;
begin
    Result := DefaultInterface.PublisherType;
end;

procedure TCEventPublisherProperties.Set_PublisherType(const pbstrPublisherType: WideString);
  { Warning: The property PublisherType has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.PublisherType := pbstrPublisherType;
end;

function TCEventPublisherProperties.Get_OwnerSID: WideString;
begin
    Result := DefaultInterface.OwnerSID;
end;

procedure TCEventPublisherProperties.Set_OwnerSID(const pbstrOwnerSID: WideString);
  { Warning: The property OwnerSID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.OwnerSID := pbstrOwnerSID;
end;

function TCEventPublisherProperties.Get_Description: WideString;
begin
    Result := DefaultInterface.Description;
end;

procedure TCEventPublisherProperties.Set_Description(const pbstrDescription: WideString);
  { Warning: The property Description has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Description := pbstrDescription;
end;

{$ENDIF}

class function CoCEventClass.Create: IEventClass;
begin
  Result := CreateComObject(CLASS_CEventClass) as IEventClass;
end;

class function CoCEventClass.CreateRemote(const MachineName: string): IEventClass;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CEventClass) as IEventClass;
end;

procedure TCEventClass.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{CDBEC9C0-7A68-11D1-88F9-0080C7D771BF}';
    IntfIID:   '{FB2B72A0-7A68-11D1-88F9-0080C7D771BF}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCEventClass.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IEventClass;
  end;
end;

procedure TCEventClass.ConnectTo(svrIntf: IEventClass);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCEventClass.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCEventClass.GetDefaultInterface: IEventClass;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TCEventClass.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TCEventClassProperties.Create(Self);
{$ENDIF}
end;

destructor TCEventClass.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TCEventClass.GetServerProperties: TCEventClassProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TCEventClass.Get_EventClassID: WideString;
begin
    Result := DefaultInterface.EventClassID;
end;

procedure TCEventClass.Set_EventClassID(const pbstrEventClassID: WideString);
  { Warning: The property EventClassID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.EventClassID := pbstrEventClassID;
end;

function TCEventClass.Get_EventClassName: WideString;
begin
    Result := DefaultInterface.EventClassName;
end;

procedure TCEventClass.Set_EventClassName(const pbstrEventClassName: WideString);
  { Warning: The property EventClassName has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.EventClassName := pbstrEventClassName;
end;

function TCEventClass.Get_OwnerSID: WideString;
begin
    Result := DefaultInterface.OwnerSID;
end;

procedure TCEventClass.Set_OwnerSID(const pbstrOwnerSID: WideString);
  { Warning: The property OwnerSID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.OwnerSID := pbstrOwnerSID;
end;

function TCEventClass.Get_FiringInterfaceID: WideString;
begin
    Result := DefaultInterface.FiringInterfaceID;
end;

procedure TCEventClass.Set_FiringInterfaceID(const pbstrFiringInterfaceID: WideString);
  { Warning: The property FiringInterfaceID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.FiringInterfaceID := pbstrFiringInterfaceID;
end;

function TCEventClass.Get_Description: WideString;
begin
    Result := DefaultInterface.Description;
end;

procedure TCEventClass.Set_Description(const pbstrDescription: WideString);
  { Warning: The property Description has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Description := pbstrDescription;
end;

function TCEventClass.Get_CustomConfigCLSID: WideString;
begin
    Result := DefaultInterface.CustomConfigCLSID;
end;

procedure TCEventClass.Set_CustomConfigCLSID(const pbstrCustomConfigCLSID: WideString);
  { Warning: The property CustomConfigCLSID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.CustomConfigCLSID := pbstrCustomConfigCLSID;
end;

function TCEventClass.Get_TypeLib: WideString;
begin
    Result := DefaultInterface.TypeLib;
end;

procedure TCEventClass.Set_TypeLib(const pbstrTypeLib: WideString);
  { Warning: The property TypeLib has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.TypeLib := pbstrTypeLib;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TCEventClassProperties.Create(AServer: TCEventClass);
begin
  inherited Create;
  FServer := AServer;
end;

function TCEventClassProperties.GetDefaultInterface: IEventClass;
begin
  Result := FServer.DefaultInterface;
end;

function TCEventClassProperties.Get_EventClassID: WideString;
begin
    Result := DefaultInterface.EventClassID;
end;

procedure TCEventClassProperties.Set_EventClassID(const pbstrEventClassID: WideString);
  { Warning: The property EventClassID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.EventClassID := pbstrEventClassID;
end;

function TCEventClassProperties.Get_EventClassName: WideString;
begin
    Result := DefaultInterface.EventClassName;
end;

procedure TCEventClassProperties.Set_EventClassName(const pbstrEventClassName: WideString);
  { Warning: The property EventClassName has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.EventClassName := pbstrEventClassName;
end;

function TCEventClassProperties.Get_OwnerSID: WideString;
begin
    Result := DefaultInterface.OwnerSID;
end;

procedure TCEventClassProperties.Set_OwnerSID(const pbstrOwnerSID: WideString);
  { Warning: The property OwnerSID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.OwnerSID := pbstrOwnerSID;
end;

function TCEventClassProperties.Get_FiringInterfaceID: WideString;
begin
    Result := DefaultInterface.FiringInterfaceID;
end;

procedure TCEventClassProperties.Set_FiringInterfaceID(const pbstrFiringInterfaceID: WideString);
  { Warning: The property FiringInterfaceID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.FiringInterfaceID := pbstrFiringInterfaceID;
end;

function TCEventClassProperties.Get_Description: WideString;
begin
    Result := DefaultInterface.Description;
end;

procedure TCEventClassProperties.Set_Description(const pbstrDescription: WideString);
  { Warning: The property Description has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Description := pbstrDescription;
end;

function TCEventClassProperties.Get_CustomConfigCLSID: WideString;
begin
    Result := DefaultInterface.CustomConfigCLSID;
end;

procedure TCEventClassProperties.Set_CustomConfigCLSID(const pbstrCustomConfigCLSID: WideString);
  { Warning: The property CustomConfigCLSID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.CustomConfigCLSID := pbstrCustomConfigCLSID;
end;

function TCEventClassProperties.Get_TypeLib: WideString;
begin
    Result := DefaultInterface.TypeLib;
end;

procedure TCEventClassProperties.Set_TypeLib(const pbstrTypeLib: WideString);
  { Warning: The property TypeLib has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.TypeLib := pbstrTypeLib;
end;

{$ENDIF}

class function CoCEventSubscription.Create: IEventSubscription;
begin
  Result := CreateComObject(CLASS_CEventSubscription) as IEventSubscription;
end;

class function CoCEventSubscription.CreateRemote(const MachineName: string): IEventSubscription;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CEventSubscription) as IEventSubscription;
end;

procedure TCEventSubscription.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{7542E960-79C7-11D1-88F9-0080C7D771BF}';
    IntfIID:   '{4A6B0E15-2E38-11D1-9965-00C04FBBB345}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCEventSubscription.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IEventSubscription;
  end;
end;

procedure TCEventSubscription.ConnectTo(svrIntf: IEventSubscription);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCEventSubscription.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCEventSubscription.GetDefaultInterface: IEventSubscription;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TCEventSubscription.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TCEventSubscriptionProperties.Create(Self);
{$ENDIF}
end;

destructor TCEventSubscription.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TCEventSubscription.GetServerProperties: TCEventSubscriptionProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TCEventSubscription.Get_SubscriptionID: WideString;
begin
    Result := DefaultInterface.SubscriptionID;
end;

procedure TCEventSubscription.Set_SubscriptionID(const pbstrSubscriptionID: WideString);
  { Warning: The property SubscriptionID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.SubscriptionID := pbstrSubscriptionID;
end;

function TCEventSubscription.Get_SubscriptionName: WideString;
begin
    Result := DefaultInterface.SubscriptionName;
end;

procedure TCEventSubscription.Set_SubscriptionName(const pbstrSubscriptionName: WideString);
  { Warning: The property SubscriptionName has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.SubscriptionName := pbstrSubscriptionName;
end;

function TCEventSubscription.Get_PublisherID: WideString;
begin
    Result := DefaultInterface.PublisherID;
end;

procedure TCEventSubscription.Set_PublisherID(const pbstrPublisherID: WideString);
  { Warning: The property PublisherID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.PublisherID := pbstrPublisherID;
end;

function TCEventSubscription.Get_EventClassID: WideString;
begin
    Result := DefaultInterface.EventClassID;
end;

procedure TCEventSubscription.Set_EventClassID(const pbstrEventClassID: WideString);
  { Warning: The property EventClassID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.EventClassID := pbstrEventClassID;
end;

function TCEventSubscription.Get_MethodName: WideString;
begin
    Result := DefaultInterface.MethodName;
end;

procedure TCEventSubscription.Set_MethodName(const pbstrMethodName: WideString);
  { Warning: The property MethodName has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.MethodName := pbstrMethodName;
end;

function TCEventSubscription.Get_SubscriberCLSID: WideString;
begin
    Result := DefaultInterface.SubscriberCLSID;
end;

procedure TCEventSubscription.Set_SubscriberCLSID(const pbstrSubscriberCLSID: WideString);
  { Warning: The property SubscriberCLSID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.SubscriberCLSID := pbstrSubscriberCLSID;
end;

function TCEventSubscription.Get_SubscriberInterface: IUnknown;
begin
    Result := DefaultInterface.SubscriberInterface;
end;

procedure TCEventSubscription.Set_SubscriberInterface(const ppSubscriberInterface: IUnknown);
begin
  DefaultInterface.Set_SubscriberInterface(ppSubscriberInterface);
end;

function TCEventSubscription.Get_PerUser: Integer;
begin
    Result := DefaultInterface.PerUser;
end;

procedure TCEventSubscription.Set_PerUser(pfPerUser: Integer);
begin
  DefaultInterface.Set_PerUser(pfPerUser);
end;

function TCEventSubscription.Get_OwnerSID: WideString;
begin
    Result := DefaultInterface.OwnerSID;
end;

procedure TCEventSubscription.Set_OwnerSID(const pbstrOwnerSID: WideString);
  { Warning: The property OwnerSID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.OwnerSID := pbstrOwnerSID;
end;

function TCEventSubscription.Get_Enabled: Integer;
begin
    Result := DefaultInterface.Enabled;
end;

procedure TCEventSubscription.Set_Enabled(pfEnabled: Integer);
begin
  DefaultInterface.Set_Enabled(pfEnabled);
end;

function TCEventSubscription.Get_Description: WideString;
begin
    Result := DefaultInterface.Description;
end;

procedure TCEventSubscription.Set_Description(const pbstrDescription: WideString);
  { Warning: The property Description has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Description := pbstrDescription;
end;

function TCEventSubscription.Get_MachineName: WideString;
begin
    Result := DefaultInterface.MachineName;
end;

procedure TCEventSubscription.Set_MachineName(const pbstrMachineName: WideString);
  { Warning: The property MachineName has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.MachineName := pbstrMachineName;
end;

function TCEventSubscription.Get_InterfaceID: WideString;
begin
    Result := DefaultInterface.InterfaceID;
end;

procedure TCEventSubscription.Set_InterfaceID(const pbstrInterfaceID: WideString);
  { Warning: The property InterfaceID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.InterfaceID := pbstrInterfaceID;
end;

function TCEventSubscription.GetPublisherProperty(const bstrPropertyName: WideString): OleVariant;
begin
  Result := DefaultInterface.GetPublisherProperty(bstrPropertyName);
end;

procedure TCEventSubscription.PutPublisherProperty(const bstrPropertyName: WideString; 
                                                   var propertyValue: OleVariant);
begin
  DefaultInterface.PutPublisherProperty(bstrPropertyName, propertyValue);
end;

procedure TCEventSubscription.RemovePublisherProperty(const bstrPropertyName: WideString);
begin
  DefaultInterface.RemovePublisherProperty(bstrPropertyName);
end;

function TCEventSubscription.GetPublisherPropertyCollection: IEventObjectCollection;
begin
  Result := DefaultInterface.GetPublisherPropertyCollection;
end;

function TCEventSubscription.GetSubscriberProperty(const bstrPropertyName: WideString): OleVariant;
begin
  Result := DefaultInterface.GetSubscriberProperty(bstrPropertyName);
end;

procedure TCEventSubscription.PutSubscriberProperty(const bstrPropertyName: WideString; 
                                                    var propertyValue: OleVariant);
begin
  DefaultInterface.PutSubscriberProperty(bstrPropertyName, propertyValue);
end;

procedure TCEventSubscription.RemoveSubscriberProperty(const bstrPropertyName: WideString);
begin
  DefaultInterface.RemoveSubscriberProperty(bstrPropertyName);
end;

function TCEventSubscription.GetSubscriberPropertyCollection: IEventObjectCollection;
begin
  Result := DefaultInterface.GetSubscriberPropertyCollection;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TCEventSubscriptionProperties.Create(AServer: TCEventSubscription);
begin
  inherited Create;
  FServer := AServer;
end;

function TCEventSubscriptionProperties.GetDefaultInterface: IEventSubscription;
begin
  Result := FServer.DefaultInterface;
end;

function TCEventSubscriptionProperties.Get_SubscriptionID: WideString;
begin
    Result := DefaultInterface.SubscriptionID;
end;

procedure TCEventSubscriptionProperties.Set_SubscriptionID(const pbstrSubscriptionID: WideString);
  { Warning: The property SubscriptionID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.SubscriptionID := pbstrSubscriptionID;
end;

function TCEventSubscriptionProperties.Get_SubscriptionName: WideString;
begin
    Result := DefaultInterface.SubscriptionName;
end;

procedure TCEventSubscriptionProperties.Set_SubscriptionName(const pbstrSubscriptionName: WideString);
  { Warning: The property SubscriptionName has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.SubscriptionName := pbstrSubscriptionName;
end;

function TCEventSubscriptionProperties.Get_PublisherID: WideString;
begin
    Result := DefaultInterface.PublisherID;
end;

procedure TCEventSubscriptionProperties.Set_PublisherID(const pbstrPublisherID: WideString);
  { Warning: The property PublisherID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.PublisherID := pbstrPublisherID;
end;

function TCEventSubscriptionProperties.Get_EventClassID: WideString;
begin
    Result := DefaultInterface.EventClassID;
end;

procedure TCEventSubscriptionProperties.Set_EventClassID(const pbstrEventClassID: WideString);
  { Warning: The property EventClassID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.EventClassID := pbstrEventClassID;
end;

function TCEventSubscriptionProperties.Get_MethodName: WideString;
begin
    Result := DefaultInterface.MethodName;
end;

procedure TCEventSubscriptionProperties.Set_MethodName(const pbstrMethodName: WideString);
  { Warning: The property MethodName has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.MethodName := pbstrMethodName;
end;

function TCEventSubscriptionProperties.Get_SubscriberCLSID: WideString;
begin
    Result := DefaultInterface.SubscriberCLSID;
end;

procedure TCEventSubscriptionProperties.Set_SubscriberCLSID(const pbstrSubscriberCLSID: WideString);
  { Warning: The property SubscriberCLSID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.SubscriberCLSID := pbstrSubscriberCLSID;
end;

function TCEventSubscriptionProperties.Get_SubscriberInterface: IUnknown;
begin
    Result := DefaultInterface.SubscriberInterface;
end;

procedure TCEventSubscriptionProperties.Set_SubscriberInterface(const ppSubscriberInterface: IUnknown);
begin
  DefaultInterface.Set_SubscriberInterface(ppSubscriberInterface);
end;

function TCEventSubscriptionProperties.Get_PerUser: Integer;
begin
    Result := DefaultInterface.PerUser;
end;

procedure TCEventSubscriptionProperties.Set_PerUser(pfPerUser: Integer);
begin
  DefaultInterface.Set_PerUser(pfPerUser);
end;

function TCEventSubscriptionProperties.Get_OwnerSID: WideString;
begin
    Result := DefaultInterface.OwnerSID;
end;

procedure TCEventSubscriptionProperties.Set_OwnerSID(const pbstrOwnerSID: WideString);
  { Warning: The property OwnerSID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.OwnerSID := pbstrOwnerSID;
end;

function TCEventSubscriptionProperties.Get_Enabled: Integer;
begin
    Result := DefaultInterface.Enabled;
end;

procedure TCEventSubscriptionProperties.Set_Enabled(pfEnabled: Integer);
begin
  DefaultInterface.Set_Enabled(pfEnabled);
end;

function TCEventSubscriptionProperties.Get_Description: WideString;
begin
    Result := DefaultInterface.Description;
end;

procedure TCEventSubscriptionProperties.Set_Description(const pbstrDescription: WideString);
  { Warning: The property Description has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Description := pbstrDescription;
end;

function TCEventSubscriptionProperties.Get_MachineName: WideString;
begin
    Result := DefaultInterface.MachineName;
end;

procedure TCEventSubscriptionProperties.Set_MachineName(const pbstrMachineName: WideString);
  { Warning: The property MachineName has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.MachineName := pbstrMachineName;
end;

function TCEventSubscriptionProperties.Get_InterfaceID: WideString;
begin
    Result := DefaultInterface.InterfaceID;
end;

procedure TCEventSubscriptionProperties.Set_InterfaceID(const pbstrInterfaceID: WideString);
  { Warning: The property InterfaceID has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.InterfaceID := pbstrInterfaceID;
end;

{$ENDIF}

class function CoEventObjectChange.Create: IEventObjectChange;
begin
  Result := CreateComObject(CLASS_EventObjectChange) as IEventObjectChange;
end;

class function CoEventObjectChange.CreateRemote(const MachineName: string): IEventObjectChange;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_EventObjectChange) as IEventObjectChange;
end;

procedure TEventObjectChange.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{D0565000-9DF4-11D1-A281-00C04FCA0AA7}';
    IntfIID:   '{F4A07D70-2E25-11D1-9964-00C04FBBB345}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TEventObjectChange.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IEventObjectChange;
  end;
end;

procedure TEventObjectChange.ConnectTo(svrIntf: IEventObjectChange);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TEventObjectChange.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TEventObjectChange.GetDefaultInterface: IEventObjectChange;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TEventObjectChange.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TEventObjectChangeProperties.Create(Self);
{$ENDIF}
end;

destructor TEventObjectChange.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TEventObjectChange.GetServerProperties: TEventObjectChangeProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TEventObjectChange.ChangedSubscription(changeType: EOC_ChangeType; 
                                                const bstrSubscriptionID: WideString): HResult;
begin
  Result := DefaultInterface.ChangedSubscription(changeType, bstrSubscriptionID);
end;

function TEventObjectChange.ChangedEventClass(changeType: EOC_ChangeType; 
                                              const bstrEventClassID: WideString): HResult;
begin
  Result := DefaultInterface.ChangedEventClass(changeType, bstrEventClassID);
end;

function TEventObjectChange.ChangedPublisher(changeType: EOC_ChangeType; 
                                             const bstrPublisherID: WideString): HResult;
begin
  Result := DefaultInterface.ChangedPublisher(changeType, bstrPublisherID);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TEventObjectChangeProperties.Create(AServer: TEventObjectChange);
begin
  inherited Create;
  FServer := AServer;
end;

function TEventObjectChangeProperties.GetDefaultInterface: IEventObjectChange;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

class function CoEventObjectChange2.Create: IEventObjectChange2;
begin
  Result := CreateComObject(CLASS_EventObjectChange2) as IEventObjectChange2;
end;

class function CoEventObjectChange2.CreateRemote(const MachineName: string): IEventObjectChange2;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_EventObjectChange2) as IEventObjectChange2;
end;

procedure TEventObjectChange2.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{BB07BACD-CD56-4E63-A8FF-CBF0355FB9F4}';
    IntfIID:   '{7701A9C3-BD68-438F-83E0-67BF4F53A422}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TEventObjectChange2.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IEventObjectChange2;
  end;
end;

procedure TEventObjectChange2.ConnectTo(svrIntf: IEventObjectChange2);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TEventObjectChange2.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TEventObjectChange2.GetDefaultInterface: IEventObjectChange2;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TEventObjectChange2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TEventObjectChange2Properties.Create(Self);
{$ENDIF}
end;

destructor TEventObjectChange2.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TEventObjectChange2.GetServerProperties: TEventObjectChange2Properties;
begin
  Result := FProps;
end;
{$ENDIF}

function TEventObjectChange2.ChangedSubscription(var pInfo: COMEVENTSYSCHANGEINFO): HResult;
begin
  Result := DefaultInterface.ChangedSubscription(pInfo);
end;

function TEventObjectChange2.ChangedEventClass(var pInfo: COMEVENTSYSCHANGEINFO): HResult;
begin
  Result := DefaultInterface.ChangedEventClass(pInfo);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TEventObjectChange2Properties.Create(AServer: TEventObjectChange2);
begin
  inherited Create;
  FServer := AServer;
end;

function TEventObjectChange2Properties.GetDefaultInterface: IEventObjectChange2;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents(dtlServerPage, [TCEventSystem, TCEventPublisher, TCEventClass, TCEventSubscription, 
    TEventObjectChange, TEventObjectChange2]);
end;

end.
