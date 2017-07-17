unit CONTACT_TLB;

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
// File generated on 27.03.2008 20:20:13 from Type Library described below.

// ************************************************************************  //
// Type Lib: \Microsoft SDKs\Windows\v6.0\Include\icontact.tlb (1)
// LIBID: {FFB3DF4D-F600-473E-92C1-CF9A1F4CCCC5}
// LCID: 0
// Helpfile: 
// HelpString: Microsoft Contact Objects
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\system32\stdole2.tlb)
// Errors:
//   Hint: TypeInfo 'CONTACT' changed to 'CONTACT_'
//   Hint: Symbol 'type' renamed to 'type_'
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
  CONTACTMajorVersion = 1;
  CONTACTMinorVersion = 0;

  LIBID_CONTACT: TGUID = '{FFB3DF4D-F600-473E-92C1-CF9A1F4CCCC5}';

  IID_IContact: TGUID = '{F941B671-BDA7-4F77-884A-F46462F226A7}';
  CLASS_CONTACT_: TGUID = '{61B68808-8EEE-4FD1-ACB8-3D804C8DB056}';
  IID_IContactProperties: TGUID = '{70DD27DD-5CBD-46E8-BEF0-23B6B346288F}';
  IID_ISequentialStream: TGUID = '{0C733A30-2A1C-11CE-ADE5-00AA0044773D}';
  IID_IStream: TGUID = '{0000000C-0000-0000-C000-000000000046}';
  IID_IContactPropertyCollection: TGUID = '{FFD3ADF8-FA64-4328-B1B6-2E0DB509CB3C}';
  IID_IContactManager: TGUID = '{AD553D98-DEB1-474A-8E17-FC0C2075B738}';
  CLASS_ContactManager: TGUID = '{7165C8AB-AF88-42BD-86FD-5310B4285A02}';
  IID_IContactCollection: TGUID = '{B6AFA338-D779-11D9-8BDE-F66BAD1E3F3A}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IContact = interface;
  IContactProperties = interface;
  ISequentialStream = interface;
  IStream = interface;
  IContactPropertyCollection = interface;
  IContactManager = interface;
  IContactCollection = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  CONTACT_ = IContact;
  ContactManager = IContactManager;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  PPWideChar1 = ^PWideChar; {*}
  PByte1 = ^Byte; {*}

  _FILETIME = packed record
    dwLowDateTime: LongWord;
    dwHighDateTime: LongWord;
  end;

  _LARGE_INTEGER = packed record
    QuadPart: Int64;
  end;

  _ULARGE_INTEGER = packed record
    QuadPart: Largeuint;
  end;

  tagSTATSTG = packed record
    pwcsName: PWideChar;
    type_: LongWord;
    cbSize: _ULARGE_INTEGER;
    mtime: _FILETIME;
    ctime: _FILETIME;
    atime: _FILETIME;
    grfMode: LongWord;
    grfLocksSupported: LongWord;
    clsid: TGUID;
    grfStateBits: LongWord;
    reserved: LongWord;
  end;


// *********************************************************************//
// Interface: IContact
// Flags:     (0)
// GUID:      {F941B671-BDA7-4F77-884A-F46462F226A7}
// *********************************************************************//
  IContact = interface(IUnknown)
    ['{F941B671-BDA7-4F77-884A-F46462F226A7}']
    procedure GetContactID(pszContactID: PWideChar; cchContactID: LongWord; 
                           var pdwcchContactIDRequired: LongWord); safecall;
    procedure GetPath(pszPath: PWideChar; cchPath: LongWord; var pdwcchPathRequired: LongWord); safecall;
    procedure CommitChanges(dwCommitFlags: LongWord); safecall;
    procedure _DO_NOT_USE1(const pC: IContactProperties); safecall;
    procedure _DO_NOT_USE2(const pC: IContactPropertyCollection); safecall;
  end;

// *********************************************************************//
// Interface: IContactProperties
// Flags:     (0)
// GUID:      {70DD27DD-5CBD-46E8-BEF0-23B6B346288F}
// *********************************************************************//
  IContactProperties = interface(IUnknown)
    ['{70DD27DD-5CBD-46E8-BEF0-23B6B346288F}']
    procedure GetString(pszPropertyName: PWideChar; dwFlags: LongWord; pszValue: PWideChar; 
                        cchValue: LongWord; var pdwcchPropertyValueRequired: LongWord); safecall;
    procedure GetDate(pszPropertyName: PWideChar; dwFlags: LongWord; var pftDateTime: _FILETIME); safecall;
    procedure GetBinary(pszPropertyName: PWideChar; dwFlags: LongWord; pszContentType: PWideChar; 
                        cchContentType: LongWord; var pdwcchContentTypeRequired: LongWord; 
                        out ppStream: IStream); safecall;
    procedure GetLabels(pszArrayElementName: PWideChar; dwFlags: LongWord; pszLabels: PWideChar; 
                        cchLabels: LongWord; var pdwcchLabelsRequired: LongWord); safecall;
    procedure SetString(pszPropertyName: PWideChar; dwFlags: LongWord; pszValue: PWideChar); safecall;
    procedure SetDate(pszPropertyName: PWideChar; dwFlags: LongWord; ftDateTime: _FILETIME); safecall;
    procedure SetBinary(pszPropertyName: PWideChar; dwFlags: LongWord; pszContentType: PWideChar; 
                        const pStream: IStream); safecall;
    procedure SetLabels(pszArrayElementName: PWideChar; dwFlags: LongWord; dwLabelCount: LongWord; 
                        var ppszLabels: PWideChar); safecall;
    procedure CreateArrayNode(pszArrayName: PWideChar; dwFlags: LongWord; fAppend: Integer; 
                              pszNewArrayElementName: PWideChar; cchNewArrayElementName: LongWord; 
                              var pdwcchNewArrayElementNameRequired: LongWord); safecall;
    procedure DeleteProperty(pszPropertyName: PWideChar; dwFlags: LongWord); safecall;
    procedure DeleteArrayNode(pszArrayElementName: PWideChar; dwFlags: LongWord); safecall;
    procedure DeleteLabels(pszArrayElementName: PWideChar; dwFlags: LongWord); safecall;
    procedure GetPropertyCollection(out ppPropertyCollection: IContactPropertyCollection; 
                                    dwFlags: LongWord; pszMultiValueName: PWideChar; 
                                    dwLabelCount: LongWord; var ppszLabels: PWideChar; 
                                    fAnyLabelMatches: Integer); safecall;
  end;

// *********************************************************************//
// Interface: ISequentialStream
// Flags:     (0)
// GUID:      {0C733A30-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  ISequentialStream = interface(IUnknown)
    ['{0C733A30-2A1C-11CE-ADE5-00AA0044773D}']
    procedure RemoteRead(out pv: Byte; cb: LongWord; out pcbRead: LongWord); safecall;
    procedure RemoteWrite(var pv: Byte; cb: LongWord; out pcbWritten: LongWord); safecall;
  end;

// *********************************************************************//
// Interface: IStream
// Flags:     (0)
// GUID:      {0000000C-0000-0000-C000-000000000046}
// *********************************************************************//
  IStream = interface(ISequentialStream)
    ['{0000000C-0000-0000-C000-000000000046}']
    procedure RemoteSeek(dlibMove: _LARGE_INTEGER; dwOrigin: LongWord; 
                         out plibNewPosition: _ULARGE_INTEGER); safecall;
    procedure SetSize(libNewSize: _ULARGE_INTEGER); safecall;
    procedure RemoteCopyTo(const pstm: IStream; cb: _ULARGE_INTEGER; out pcbRead: _ULARGE_INTEGER; 
                           out pcbWritten: _ULARGE_INTEGER); safecall;
    procedure Commit(grfCommitFlags: LongWord); safecall;
    procedure Revert; safecall;
    procedure LockRegion(libOffset: _ULARGE_INTEGER; cb: _ULARGE_INTEGER; dwLockType: LongWord); safecall;
    procedure UnlockRegion(libOffset: _ULARGE_INTEGER; cb: _ULARGE_INTEGER; dwLockType: LongWord); safecall;
    procedure Stat(out pstatstg: tagSTATSTG; grfStatFlag: LongWord); safecall;
    procedure Clone(out ppstm: IStream); safecall;
  end;

// *********************************************************************//
// Interface: IContactPropertyCollection
// Flags:     (0)
// GUID:      {FFD3ADF8-FA64-4328-B1B6-2E0DB509CB3C}
// *********************************************************************//
  IContactPropertyCollection = interface(IUnknown)
    ['{FFD3ADF8-FA64-4328-B1B6-2E0DB509CB3C}']
    procedure Reset; safecall;
    function Next : HRESULT; stdcall;
    procedure GetPropertyName(pszPropertyName: PWideChar; cchPropertyName: LongWord; 
                              var pdwcchPropertyNameRequired: LongWord); safecall;
    procedure GetPropertyType(var pdwType: LongWord); safecall;
    procedure GetPropertyVersion(var pdwVersion: LongWord); safecall;
    procedure GetPropertyModificationDate(var pftModificationDate: _FILETIME); safecall;
    procedure GetPropertyArrayElementID(pszArrayElementID: PWideChar; cchArrayElementID: LongWord; 
                                        var pdwcchArrayElementIDRequired: LongWord); safecall;
  end;

// *********************************************************************//
// Interface: IContactManager
// Flags:     (0)
// GUID:      {AD553D98-DEB1-474A-8E17-FC0C2075B738}
// *********************************************************************//
  IContactManager = interface(IUnknown)
    ['{AD553D98-DEB1-474A-8E17-FC0C2075B738}']
    procedure Initialize(pszAppName: PWideChar; pszAppVersion: PWideChar); safecall;
    procedure Load(pszContactID: PWideChar; out ppContact: IContact); safecall;
    procedure MergeContactIDs(pszNewContactID: PWideChar; pszOldContactID: PWideChar); safecall;
    procedure GetMeContact(out ppMeContact: IContact); safecall;
    procedure SetMeContact(const pMeContact: IContact); safecall;
    procedure GetContactCollection(out ppContactCollection: IContactCollection); safecall;
  end;

// *********************************************************************//
// Interface: IContactCollection
// Flags:     (0)
// GUID:      {B6AFA338-D779-11D9-8BDE-F66BAD1E3F3A}
// *********************************************************************//
  IContactCollection = interface(IUnknown)
    ['{B6AFA338-D779-11D9-8BDE-F66BAD1E3F3A}']
    procedure Reset; safecall;
    function Next : HRESULT; stdcall;
    procedure GetCurrent(out ppContact: IContact); safecall;
  end;

// *********************************************************************//
// The Class CoCONTACT_ provides a Create and CreateRemote method to          
// create instances of the default interface IContact exposed by              
// the CoClass CONTACT_. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCONTACT_ = class
    class function Create: IContact;
    class function CreateRemote(const MachineName: string): IContact;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCONTACT_
// Help String      : Contact
// Default Interface: IContact
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TCONTACT_Properties= class;
{$ENDIF}
  TCONTACT_ = class(TOleServer)
  private
    FIntf: IContact;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TCONTACT_Properties;
    function GetServerProperties: TCONTACT_Properties;
{$ENDIF}
    function GetDefaultInterface: IContact;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IContact);
    procedure Disconnect; override;
    procedure GetContactID(pszContactID: PWideChar; cchContactID: LongWord; 
                           var pdwcchContactIDRequired: LongWord);
    procedure GetPath(pszPath: PWideChar; cchPath: LongWord; var pdwcchPathRequired: LongWord);
    procedure CommitChanges(dwCommitFlags: LongWord);
    procedure _DO_NOT_USE1(const pC: IContactProperties);
    procedure _DO_NOT_USE2(const pC: IContactPropertyCollection);
    property DefaultInterface: IContact read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TCONTACT_Properties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TCONTACT_
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TCONTACT_Properties = class(TPersistent)
  private
    FServer:    TCONTACT_;
    function    GetDefaultInterface: IContact;
    constructor Create(AServer: TCONTACT_);
  protected
  public
    property DefaultInterface: IContact read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoContactManager provides a Create and CreateRemote method to          
// create instances of the default interface IContactManager exposed by              
// the CoClass ContactManager. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoContactManager = class
    class function Create: IContactManager;
    class function CreateRemote(const MachineName: string): IContactManager;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TContactManager
// Help String      : ContactManager
// Default Interface: IContactManager
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TContactManagerProperties= class;
{$ENDIF}
  TContactManager = class(TOleServer)
  private
    FIntf: IContactManager;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TContactManagerProperties;
    function GetServerProperties: TContactManagerProperties;
{$ENDIF}
    function GetDefaultInterface: IContactManager;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IContactManager);
    procedure Disconnect; override;
    procedure Initialize(pszAppName: PWideChar; pszAppVersion: PWideChar);
    procedure Load(pszContactID: PWideChar; out ppContact: IContact);
    procedure MergeContactIDs(pszNewContactID: PWideChar; pszOldContactID: PWideChar);
    procedure GetMeContact(out ppMeContact: IContact);
    procedure SetMeContact(const pMeContact: IContact);
    procedure GetContactCollection(out ppContactCollection: IContactCollection);
    property DefaultInterface: IContactManager read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TContactManagerProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TContactManager
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TContactManagerProperties = class(TPersistent)
  private
    FServer:    TContactManager;
    function    GetDefaultInterface: IContactManager;
    constructor Create(AServer: TContactManager);
  protected
  public
    property DefaultInterface: IContactManager read GetDefaultInterface;
  published
  end;
{$ENDIF}


procedure Register;

resourcestring
  dtlServerPage = '(none)';

  dtlOcxPage = '(none)';

implementation

uses ComObj;

class function CoCONTACT_.Create: IContact;
begin
  Result := CreateComObject(CLASS_CONTACT_) as IContact;
end;

class function CoCONTACT_.CreateRemote(const MachineName: string): IContact;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CONTACT_) as IContact;
end;

procedure TCONTACT_.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{61B68808-8EEE-4FD1-ACB8-3D804C8DB056}';
    IntfIID:   '{F941B671-BDA7-4F77-884A-F46462F226A7}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCONTACT_.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IContact;
  end;
end;

procedure TCONTACT_.ConnectTo(svrIntf: IContact);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCONTACT_.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCONTACT_.GetDefaultInterface: IContact;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TCONTACT_.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TCONTACT_Properties.Create(Self);
{$ENDIF}
end;

destructor TCONTACT_.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TCONTACT_.GetServerProperties: TCONTACT_Properties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TCONTACT_.GetContactID(pszContactID: PWideChar; cchContactID: LongWord; 
                                 var pdwcchContactIDRequired: LongWord);
begin
  DefaultInterface.GetContactID(pszContactID, cchContactID, pdwcchContactIDRequired);
end;

procedure TCONTACT_.GetPath(pszPath: PWideChar; cchPath: LongWord; var pdwcchPathRequired: LongWord);
begin
  DefaultInterface.GetPath(pszPath, cchPath, pdwcchPathRequired);
end;

procedure TCONTACT_.CommitChanges(dwCommitFlags: LongWord);
begin
  DefaultInterface.CommitChanges(dwCommitFlags);
end;

procedure TCONTACT_._DO_NOT_USE1(const pC: IContactProperties);
begin
  DefaultInterface._DO_NOT_USE1(pC);
end;

procedure TCONTACT_._DO_NOT_USE2(const pC: IContactPropertyCollection);
begin
  DefaultInterface._DO_NOT_USE2(pC);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TCONTACT_Properties.Create(AServer: TCONTACT_);
begin
  inherited Create;
  FServer := AServer;
end;

function TCONTACT_Properties.GetDefaultInterface: IContact;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

class function CoContactManager.Create: IContactManager;
begin
  Result := CreateComObject(CLASS_ContactManager) as IContactManager;
end;

class function CoContactManager.CreateRemote(const MachineName: string): IContactManager;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ContactManager) as IContactManager;
end;

procedure TContactManager.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{7165C8AB-AF88-42BD-86FD-5310B4285A02}';
    IntfIID:   '{AD553D98-DEB1-474A-8E17-FC0C2075B738}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TContactManager.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IContactManager;
  end;
end;

procedure TContactManager.ConnectTo(svrIntf: IContactManager);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TContactManager.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TContactManager.GetDefaultInterface: IContactManager;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TContactManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TContactManagerProperties.Create(Self);
{$ENDIF}
end;

destructor TContactManager.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TContactManager.GetServerProperties: TContactManagerProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TContactManager.Initialize(pszAppName: PWideChar; pszAppVersion: PWideChar);
begin
  DefaultInterface.Initialize(pszAppName, pszAppVersion);
end;

procedure TContactManager.Load(pszContactID: PWideChar; out ppContact: IContact);
begin
  DefaultInterface.Load(pszContactID, ppContact);
end;

procedure TContactManager.MergeContactIDs(pszNewContactID: PWideChar; pszOldContactID: PWideChar);
begin
  DefaultInterface.MergeContactIDs(pszNewContactID, pszOldContactID);
end;

procedure TContactManager.GetMeContact(out ppMeContact: IContact);
begin
  DefaultInterface.GetMeContact(ppMeContact);
end;

procedure TContactManager.SetMeContact(const pMeContact: IContact);
begin
  DefaultInterface.SetMeContact(pMeContact);
end;

procedure TContactManager.GetContactCollection(out ppContactCollection: IContactCollection);
begin
  DefaultInterface.GetContactCollection(ppContactCollection);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TContactManagerProperties.Create(AServer: TContactManager);
begin
  inherited Create;
  FServer := AServer;
end;

function TContactManagerProperties.GetDefaultInterface: IContactManager;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents(dtlServerPage, [TCONTACT_, TContactManager]);
end;

end.
