(*
  Name:             keymancustomisation
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    25 Oct 2016
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Add MouseButton for menu items
                    14 Sep 2006 - mcdurdin - Add Welcome size, splash file name, supports character map properties
                    04 Dec 2006 - mcdurdin - Localization support
                    12 Dec 2006 - mcdurdin - Add DialogParameters and LanguageCode support
                    19 Mar 2007 - mcdurdin - I701 - Fix leaked com objects
                    23 Aug 2007 - mcdurdin - I933 - support locale in package
                    12 Oct 2007 - mcdurdin - I1093, I1039 - Handle errors around registry keys
                    27 Mar 2008 - mcdurdin - I1248 - Add TutorialCommandLine
                    26 Mar 2009 - mcdurdin - I1907 - Work around Developer installs
                    12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
                    26 Jul 2010 - mcdurdin - I2467 - 8.0 renumber
                    26 Jul 2010 - mcdurdin - I2468 - Eliminate KeymanWeb Pack
                    22 Oct 2010 - mcdurdin - I2521 - Activation Client support for Developer 8.0
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    26 Jun 2012 - mcdurdin - I3383 - KM9 - ErrorControlledRegistry is obsolete
                    17 Aug 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    19 Oct 2012 - mcdurdin - I3477 - V9.0 - Remove generic locale path function and use GetLocalPathForLocale
                    03 Jul 2014 - mcdurdin - I4314 - V9.0 - Icon size in tool tray is wrong when using large fonts
                    12 Aug 2014 - mcdurdin - I4314 - V9.0 - Icon size in tool tray is wrong when using large fonts
                    25 Oct 2016 - mcdurdin - I5122 - CrashID:kmshell.exe_10.0.751.0_2C684B25_EOleException
*)
unit keymancustomisation;  // I3306

interface

uses
  Windows, Classes, SysUtils, ActiveX, ComObj,
  Vcl.Graphics,
  keymanapi_TLB,
  KeymanAutoObject, KeymanContext,
  internalinterfaces,
  CustomisationStorage, CustomisationMessages,
  CustomisationMenu,
  custinterfaces;

type
  ECustomisation = class(Exception)
  end;

  TCustomisationAutoObject = class;

  ICustomisationInterface = interface
    ['{45B66C8D-A2AF-445A-96FD-34EDA8302D13}']
    procedure DoDetach;
  end;

  TCustomisationInterfacedObject = class(TInterfacedObject, ICustomisationInterface)
  protected
    FCust: TCustomisationAutoObject;

    function DoSerialize(Flags: TOleEnum; const ImagePath: WideString; References: TStrings): WideString;
    function Serialize(Flags: TOleEnum; const ImagePath: WideString; References: TStrings): WideString;
    function XMLClassName: WideString;

    procedure DoDetach; virtual;
  public
    constructor Create(ACust: TCustomisationAutoObject); //; intf: TGUID);
    destructor Destroy; override;
    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
  end;

  TCustomisationMessages = class(TCustomisationInterfacedObject, IKeymanCustomisationMessages)
  protected
    function MessageFromID(const ID: WideString): WideString; overload; safecall;
    function MessageFromID(const ID, LanguageCode: WideString): WideString; overload; safecall;
    function GetAvailableLanguages: WideString; safecall;
    function GetLanguageCode: WideString; safecall;
    procedure SetLanguageCode(const Value: WideString); safecall;
    function GetLocalePath: WideString; safecall;
    procedure GetDialogParameters(const DialogName: WideString;
      var FWidth: Integer; var FHeight: Integer); stdcall;
    function GetLocalePathForLocale(const Locale: WideString): WideString; safecall;
    procedure Load; safecall;
    procedure Refresh; safecall;
  end;

  TCustomisationMenuItem = class(TCustomisationInterfacedObject, IIntKeymanInterface, IKeymanCustomisationMenuItem)
  private
    FIndex: Integer;
    FMenuItem: CustomisationMenu.TCustomisationMenuItem;
  protected
    function Get_Location: TOleEnum; safecall;
    function Get_ItemType: TCustomisationMenuItemType; safecall;
    function Get_Caption: WideString; safecall;
    function Get_Action: TCustomisationMenuItemAction; safecall;
    function Get_CmdLine: WideString; safecall;
    function Get_Icon: IPictureDisp; safecall;
    function Get_PictureDefault: IPictureDisp; safecall;
    function Get_PictureSelected: IPictureDisp; safecall;
    function Get_Hotkey: Word; safecall;
    function Get_Font: IFontDisp; safecall;

    function Get_Index: Integer; safecall;

    procedure DoDetach; override;
  public
    constructor Create(AIndex: Integer; ACust: TCustomisationAutoObject; AMenuItem: CustomisationMenu.TCustomisationMenuItem);
  end;

  TCustomisationMenuItems = class(TCustomisationInterfacedObject, IKeymanCustomisationMenuItems)
  private
    FItems: TAutoObjectList;
  protected
    function Get_Item(const Index: Integer): IKeymanCustomisationMenuItem; safecall;
    function Get_Count: Integer; safecall;
    procedure DoDetach; override;
  public
    constructor Create(ACust: TCustomisationAutoObject);
    destructor Destroy; override;
  end;

  TCustomisationAutoObject = class(TKeymanAutoObject, IKeymanCustomisation, IKeymanUserInterface)
  private
    FFileName: string;
    FStorage: TCustomisationStorage;
    FMessages: TCustomisationMessageManager;
    FMenu: TCustomisationMenu;

    FCustMessages: IKeymanCustomisationMessages;
    FCustMenuItems: IKeymanCustomisationMenuItems;

    procedure LoadStorage;
  protected
    function Get_CustMenuItems: IKeymanCustomisationMenuItems; safecall;
    function Get_CustMessages: IKeymanCustomisationMessages; safecall;
    function Get_CustFile(const FileName: WideString): IStream; safecall;
    procedure Refresh; safecall;

    property CustMessages: IKeymanCustomisationMessages read Get_CustMessages;
    property CustMenuItems: IKeymanCustomisationMenuItems read Get_CustMenuItems;
    property CustFile[const FileName: WideString]: IStream read Get_CustFile;
  public
    constructor Create(AContext: TKeymanContext; const dispintf: TGUID; const AFileName: string);
    destructor Destroy; override;
  end;

implementation

uses
  onlineconstants,
  utilhandleexception,
  utilicon,
  utilolepicture,
  Vcl.Imaging.jpeg,
  ErrorControlledRegistry,
  RegistryKeys;

{ TCustomisationAutoObject }

constructor TCustomisationAutoObject.Create(AContext: TKeymanContext; const dispintf: TGUID; const AFileName: string);
begin
  inherited Create(AContext, dispintf);
  FFileName := AFileName;
  FCustMessages := TCustomisationMessages.Create(Self);
  FCustMenuItems := TCustomisationMenuItems.Create(Self);
  LoadStorage;
  FCustMessages.Load;
end;

destructor TCustomisationAutoObject.Destroy;
begin
  (FCustMessages as ICustomisationInterface).DoDetach;
  (FCustMenuItems as ICustomisationInterface).DoDetach;

  FCustMessages := nil;
  FCustMenuItems := nil;

  FreeAndNil(FMessages);
  FreeAndNil(FStorage);
  FreeAndNil(FMenu);
  inherited Destroy;
end;

function TCustomisationAutoObject.Get_CustMessages: IKeymanCustomisationMessages;
begin
  LoadStorage;
  Result := FCustMessages;
end;

procedure TCustomisationAutoObject.LoadStorage;
begin
  if Assigned(FStorage) then Exit;
  // I5122

  FStorage := TCustomisationStorage.Create(FFileName, nil, True);
  FMessages := TCustomisationMessageManager.Create(FStorage.FileName);
  FMenu := TCustomisationMenu.Create(FStorage);
end;

procedure TCustomisationAutoObject.Refresh;
begin
  if Assigned(FCustMessages) then
    FCustMessages.Refresh;
end;

function TCustomisationAutoObject.Get_CustFile(const FileName: WideString): IStream;
var
  n: Integer;
begin
  Result := nil;
  with FStorage.CustFiles do
  begin
    n := IndexOfFileName(FileName);
    if n < 0 then Exit;
    Result := TStreamAdapter.Create(Items[n].Stream, soReference) as IStream;
  end;
end;

function TCustomisationAutoObject.Get_CustMenuItems: IKeymanCustomisationMenuItems;
begin
  LoadStorage;
  Result := FCustMenuItems;
end;

{ TCustomisationMessages }

function TCustomisationMessages.GetAvailableLanguages: WideString;
begin
  if Assigned(FCust) and Assigned(FCust.FMessages)
    then Result := FCust.FMessages.AvailableLanguages
    else Result := '';
end;

procedure TCustomisationMessages.GetDialogParameters(
  const DialogName: WideString; var FWidth, FHeight: Integer);
begin
  if Assigned(FCust) then
    FCust.FMessages.GetDialogParameters(DialogName, FWidth, FHeight);
end;

function TCustomisationMessages.GetLanguageCode: WideString;
begin
  if Assigned(FCust) and Assigned(FCust.FMessages)
    then Result := FCust.FMessages.LanguageCode
    else Result := '';
end;

function TCustomisationMessages.GetLocalePath: WideString;
begin
  if Assigned(FCust) and Assigned(FCust.FMessages)
    then Result := FCust.FMessages.GetLocalePathForLocale('')  // I3477
    else Result := '';
end;

function TCustomisationMessages.GetLocalePathForLocale(
  const Locale: WideString): WideString;
begin
  if Assigned(FCust) and Assigned(FCust.FMessages)
    then Result := FCust.FMessages.GetLocalePathForLocale(Locale)
    else Result := '';
end;

procedure TCustomisationMessages.Load;
begin
  if Assigned(FCust) and Assigned(FCust.FMessages) then
    with TRegistryErrorControlled.Create do
    try
      if OpenKeyReadOnly(SRegKey_KeymanEngine_CU) and ValueExists(SRegValue_CurrentLanguage)
        then FCust.FMessages.LanguageCode := ReadString(SRegValue_CurrentLanguage)
        else FCust.FMessages.LanguageCode := '';
    finally
      Free;
    end;
end;

function TCustomisationMessages.MessageFromID(const ID,
  LanguageCode: WideString): WideString;
begin
  if Assigned(FCust) and Assigned(FCust.FMessages)
    then Result := FCust.FMessages.MessageFromID(ID, LanguageCode)
    else Result := '';
end;

procedure TCustomisationMessages.Refresh;
begin
  Load;
end;

function TCustomisationMessages.MessageFromID(const ID: WideString): WideString;
begin
  if Assigned(FCust) and Assigned(FCust.FMessages)
    then Result := FCust.FMessages.MessageFromID(ID)
    else Result := '';
end;

procedure TCustomisationMessages.SetLanguageCode(const Value: WideString);
begin
  if Assigned(FCust) then
  begin
    if Assigned(FCust.FMessages) then
      FCust.FMessages.LanguageCode := Value;

    with TRegistryErrorControlled.Create do    // I1093, I1039 - 'Failed to set data for current language'
    try
      if not OpenKey(SRegKey_KeymanEngine_CU, True) then
        RaiseLastOSError(Integer(LastError));    // I1093, I1039 - 'Failed to set data for current language'
      WriteString(SRegValue_CurrentLanguage, string(Value));
    finally
      Free;
    end;
  end;
end;

{ TCustomisationInterfacedObject }

constructor TCustomisationInterfacedObject.Create(ACust: TCustomisationAutoObject); //; intf: TGUID);
begin
  inherited Create;
  FCust := ACust;
end;

destructor TCustomisationInterfacedObject.Destroy;
begin
  DoDetach;
  inherited Destroy;
end;

procedure TCustomisationInterfacedObject.DoDetach;
begin
  FCust := nil;
end;

function TCustomisationInterfacedObject.DoSerialize(Flags: TOleEnum;
  const ImagePath: WideString; References: TStrings): WideString;
begin
  Result := '';
end;

function TCustomisationInterfacedObject.SafeCallException(
  ExceptObject: TObject; ExceptAddr: Pointer): HResult;
begin
  if (ExceptObject is Exception) and not (ExceptObject is EOleException) { for some reason, we lose the class name EKeyman? } then
    LogException(ClassName, ExceptObject as Exception, ExceptAddr);
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, IKeyman, '', '');

end;

function TCustomisationInterfacedObject.Serialize(Flags: TOleEnum;
  const ImagePath: WideString; References: TStrings): WideString;
begin
  Result := '';
end;

function TCustomisationInterfacedObject.XMLClassName: WideString;
begin
  Result := '';
end;

{ TCustomisationMenuItem }

constructor TCustomisationMenuItem.Create(AIndex: Integer; ACust: TCustomisationAutoObject; AMenuItem: CustomisationMenu.TCustomisationMenuItem);
begin
  inherited Create(ACust); // (ACust, IKeymanCustomisationMenuItem);
  FIndex := AIndex;
  FMenuItem := AMenuItem;
end;

procedure TCustomisationMenuItem.DoDetach;
begin
  inherited;
  FMenuItem := nil;
end;

function TCustomisationMenuItem.Get_Action: TCustomisationMenuItemAction;
begin
  if Assigned(FCust) and Assigned(FMenuItem)
    then Result := FMenuItem.Action
    else Result := miaNull;
end;

function TCustomisationMenuItem.Get_Caption: WideString;
begin
  if Assigned(FCust) and Assigned(FMenuItem)
    then Result := FMenuItem.Caption
    else Result := '';
end;

function TCustomisationMenuItem.Get_CmdLine: WideString;
begin
  if Assigned(FCust) and Assigned(FMenuItem)
    then Result := FMenuItem.CmdLine
    else Result := '';
end;

function TCustomisationMenuItem.Get_Font: IFontDisp;
begin
  if Assigned(FCust) and Assigned(FMenuItem)
    then GetOleFont(FMenuItem.Font, Result)
    else Result := nil;
end;

function TCustomisationMenuItem.Get_Hotkey: Word;
begin
  if Assigned(FCust) and Assigned(FMenuItem)
    then Result := FMenuItem.Hotkey
    else Result := 0;
end;

function TCustomisationMenuItem.Get_Icon: IPictureDisp;
begin
  if not Assigned(FCust) or not Assigned(FMenuItem) or not Assigned(FMenuItem.Icon.Graphic) then
    Result := nil
  else
  begin
    GetOlePictureEx(FMenuItem.Icon, Result);
  end;
end;

function TCustomisationMenuItem.Get_Index: Integer;
begin
  Result := FIndex;
end;

function TCustomisationMenuItem.Get_ItemType: TCustomisationMenuItemType;
begin
  if Assigned(FCust) and Assigned(FMenuItem)
    then Result := FMenuItem.ItemType
    else Result := mitText;
end;

function TCustomisationMenuItem.Get_Location: TOleEnum;
begin
  if Assigned(FCust) and Assigned(FMenuItem)
    then Result := FMenuItem.Location
    else Result := milLeft;
end;

function TCustomisationMenuItem.Get_PictureDefault: IPictureDisp;
begin
  if not Assigned(FCust) or not Assigned(FMenuItem) or not Assigned(FMenuItem.PictureDefault.Graphic) then
    Result := nil
  else
  begin
    GetOlePictureEx(FMenuItem.PictureDefault, Result);
  end;
end;

function TCustomisationMenuItem.Get_PictureSelected: IPictureDisp;
begin
  if not Assigned(FCust) or not Assigned(FMenuItem) or not Assigned(FMenuItem.PictureSelected.Graphic) then
    Result := nil
  else
  begin
    GetOlePictureEx(FMenuItem.PictureSelected, Result);
  end;
end;

{ TCustomisationMenuItems }

constructor TCustomisationMenuItems.Create(ACust: TCustomisationAutoObject);
begin
  inherited Create(ACust);
  FItems := TAutoObjectList.Create;
end;

destructor TCustomisationMenuItems.Destroy;
begin
  inherited;
  FreeAndNil(FItems);
end;

procedure TCustomisationMenuItems.DoDetach;
var
  i: Integer;
begin
  inherited;
  for i := 0 to FItems.Count - 1 do
    (FItems[i] as ICustomisationInterface).DoDetach;
end;

function TCustomisationMenuItems.Get_Count: Integer;
begin
  if Assigned(FCust) and Assigned(FCust.FMenu)
    then Result := FCust.FMenu.Items.Count
    else Result := 0;
end;

function TCustomisationMenuItems.Get_Item(
  const Index: Integer): IKeymanCustomisationMenuItem;
var
  i: Integer;
begin
  if Assigned(FCust) and Assigned(FCust.FMenu) then
  begin
    for i := 0 to FItems.Count - 1 do
      if (FItems[i] as IKeymanCustomisationMenuItem).Index = Index then
      begin
        Result := FItems[i] as IKeymanCustomisationMenuItem;
        Exit;
      end;
    Result := TCustomisationMenuItem.Create(Index, FCust, FCust.FMenu.Items[Index-1]);
    FItems.Add(Result as IIntKeymanInterface);
  end
  else Result := nil;
end;

end.

