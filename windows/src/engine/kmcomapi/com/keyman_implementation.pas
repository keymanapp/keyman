(*
  Name:             keyman_implementation
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      1 Aug 2006

  Modified Date:    22 Feb 2011
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          01 Aug 2006 - mcdurdin - Add UniqueIndex, SerializeXML and ObjectByIndex
                    04 Dec 2006 - mcdurdin - Rework COM objects to be internally owned by TKeyman so that they can't be destroyed by an external process
                    19 Mar 2007 - mcdurdin - Remove forms.pas dependency
                    13 Jul 2007 - mcdurdin - I960 - Support COM scripted access to activation functions
                    27 Mar 2008 - mcdurdin - Add Languages property
                    14 Jun 2008 - mcdurdin - I1400 - Refactor language check into kmcomapi
                    19 Sep 2008 - mcdurdin - I1642 - Move GetErrLogPath out of DLLMain
                    16 Jan 2009 - mcdurdin - I1799 - Add CoClass for Ta-vultesoftKeymanLangConfig
                    12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
                    22 Feb 2011 - mcdurdin - I2750 - Setup gives an access violation when destroying kmcomapi (masking another error?)
*)
unit keyman_implementation;

interface

uses
  System.Win.ComObj,
  System.Win.StdVcl,
  Winapi.ActiveX,
  Winapi.Windows,

  internalinterfaces,
  keymanapi_TLB,
  keymanautoobject,
  keymancontext,
  keymancontrol,
  keymanerrors,
  keymanhotkeys,
  keymankeyboardsinstalled,
  keymanlanguages,
  keymanoptions,
  keymanpackagesinstalled,
  keymansysteminfo;

type
  TKeyman = class(TAutoObject, IKeyman, IIntKeyman, IKeymanBCP47Canonicalization)
  private
    FInitialized: Boolean;
    FContext: TKeymanContext;
    FErrors: IIntKeymanErrors;
    FLanguages: IIntKeymanLanguages;
    FKeyboards: IIntKeymanKeyboardsInstalled;
    FPackages: IIntKeymanPackagesInstalled;
    FSystemInfo: IIntKeymanSystemInfo;
    FOptions: IIntKeymanOptions;
    FHotkeys: IIntKeymanHotkeys;
    FControl: IIntKeymanControl;
  protected
    { IKeyman }
    function Get_AutoApply: WordBool; safecall;
    procedure Set_AutoApply(Value: WordBool); safecall;
    function Get_Control: IKeymanControl; safecall;
    function Get_Errors: IKeymanErrors; safecall;
    function Get_Hotkeys: IKeymanHotkeys; safecall;
    function Get_Keyboards: IKeymanKeyboardsInstalled; safecall;
    function Get_Languages: IKeymanLanguages; safecall;
    function Get_Options: IKeymanOptions; safecall;
    function Get_Packages: IKeymanPackagesInstalled; safecall;
    function Get_SystemInfo: IKeymanSystemInfo; safecall;

    procedure Apply; safecall;
    procedure Refresh; safecall;

    { IKeymanBCP47Canonicalization }
    function GetCanonicalTag(const Tag: WideString): WideString; safecall;
    function GetFullTagList(const Tag: WideString): OleVariant; safecall;

    { IKeymanObject }
    // Reimplement as a special case for this interface
    function SerializeXML(Flags: TOleEnum; const ImagePath: WideString; out References: OleVariant): WideString; safecall;

  public
    procedure Initialize; override;
    destructor Destroy; override;

    function ObjAddRef: Integer; override; stdcall;
    function ObjRelease: Integer; override; stdcall;

    { IIntKeyman - in theory }
    property Keyboards: IIntKeymanKeyboardsInstalled read FKeyboards;
    property Languages: IIntKeymanLanguages read FLanguages;
    property Packages: IIntKeymanPackagesInstalled read FPackages;
    property SystemInfo: IIntKeymanSystemInfo read FSystemInfo;
    property Errors: IIntKeymanErrors read FErrors;
    property Options: IIntKeymanOptions read FOptions;
    property Hotkeys: IIntKeymanHotkeys read FHotkeys;
    property Control: IIntKeymanControl read FControl;
  end;

implementation

uses
  System.Classes,
  System.Variants,
  System.Win.ComServ,

  sysutils,
  klog,
  utilhandleexception,

  Keyman.System.CanonicalLanguageCodeUtils;

const
  SErrorUninitialised = 'Keyman COM API did not initialize successfully';

procedure TKeyman.Initialize;
begin
  KL.MethodEnter(Self, 'Initialize', []);
  try
    inherited;

    try
      FContext := TKeymanContext.Create(Self);
      FControl := TKeymanControl.Create(FContext);
      FErrors := TKeymanErrors.Create(FContext);
      FKeyboards := TKeymanKeyboardsInstalled.Create(FContext);
      FPackages := TKeymanPackagesInstalled.Create(FContext);
      FSystemInfo := TKeymanSystemInfo.Create(FContext);
      FOptions := TKeymanOptions.Create(FContext);
      FLanguages := TKeymanLanguages.Create(FContext);
      FHotkeys := TKeymanHotkeys.Create(FContext);
      FInitialized := True;
    except
      on E:Exception do
      begin
        LogException('TKeyman', E, ExceptAddr);
        FInitialized := False;
      end;
    end;
  finally
    KL.MethodExit(Self, 'Initialize');
  end;
end;

destructor TKeyman.Destroy;
begin
  KL.MethodEnter(Self, 'Destroy', []);
  try
    FInitialized := False;
    FControl := nil; // Destruction of FControl likes to use FKeyboards
    FKeyboards := nil;  // I2750
    FPackages := nil;
    FSystemInfo := nil;
    FErrors := nil;
    FOptions := nil;
    FLanguages := nil;
    FHotkeys := nil;

    FContext.Free; FContext := nil;
    inherited Destroy;
  finally
    KL.MethodExit(Self, 'Destroy');
  end;
end;

procedure TKeyman.Refresh;
begin
  KL.MethodEnter(Self, 'Refresh', []);
  try
    if not FInitialized then raise Exception.Create(SErrorUninitialised);
    try
      (FErrors as IKeymanErrors).Clear;
      (FOptions as IKeymanOptions).Refresh;
      (FPackages as IKeymanPackagesInstalled).Refresh;
      (FKeyboards as IKeymanKeyboardsInstalled).Refresh;
      (FLanguages as IKeymanLanguages).Refresh;
      (FHotkeys as IKeymanHotkeys).Refresh;
      FControl.Refresh;
    except
      on E:Exception do
      begin
        KL.LogError('Error in TKeyman.Refresh: '+E.Message);
        LogException('TKeyman', E, ExceptAddr);
        raise;
      end;
    end;
  finally
    KL.MethodExit(Self, 'Refresh');
  end;
end;

function TKeyman.Get_Keyboards: IKeymanKeyboardsInstalled;
begin
  if not FInitialized then raise Exception.Create(SErrorUninitialised);
  Result := FKeyboards as IKeymanKeyboardsInstalled;
end;

function TKeyman.Get_Packages: IKeymanPackagesInstalled;
begin
  if not FInitialized then raise Exception.Create(SErrorUninitialised);
  Result := FPackages as IKeymanPackagesInstalled;
end;

function TKeyman.Get_SystemInfo: IKeymanSystemInfo;
begin
  if not FInitialized then raise Exception.Create(SErrorUninitialised);
  Result := FSystemInfo as IKeymanSystemInfo;
end;

function TKeyman.Get_Errors: IKeymanErrors;
begin
  if not FInitialized then raise Exception.Create(SErrorUninitialised);
  Result := FErrors as IKeymanErrors;
end;

function TKeyman.Get_Hotkeys: IKeymanHotkeys;
begin
  if not FInitialized then raise Exception.Create(SErrorUninitialised);
  Result := FHotkeys as IKeymanHotkeys;
end;

function TKeyman.Get_Options: IKeymanOptions;
begin
  if not FInitialized then raise Exception.Create(SErrorUninitialised);
  Result := FOptions as IKeymanOptions;
end;

function TKeyman.Get_Languages: IKeymanLanguages;
begin
  if not FInitialized then raise Exception.Create(SErrorUninitialised);
  Result := FLanguages as IKeymanLanguages;
end;

function TKeyman.SerializeXML(Flags: TOleEnum;
  const ImagePath: WideString; out References: OleVariant): WideString;
begin
  // Reimplement as a E_NOTIMPL for this interface only
  raise EOleException.Create('', E_NOTIMPL, '', '', 0);
end;

function TKeyman.Get_Control: IKeymanControl;
begin
  if not FInitialized then raise Exception.Create(SErrorUninitialised);
  Result := FControl as IKeymanControl;
end;

function TKeyman.ObjAddRef: Integer;
begin
  Result := inherited ObjAddRef;
end;

function TKeyman.ObjRelease: Integer;
begin
  Result := inherited ObjRelease;
end;

function TKeyman.GetCanonicalTag(const Tag: WideString): WideString;
begin
  // We implement this here to avoid sharing standards datasets across
  // multiple executables
  Result := TCanonicalLanguageCodeUtils.FindBestTag(Tag, True, True);
end;

function TKeyman.Get_AutoApply: WordBool;
begin
  if not FInitialized then raise Exception.Create(SErrorUninitialised);
  Result := FControl.AutoApply;
end;

procedure TKeyman.Apply;
var
  AutoApply: Boolean;
begin
  if not FInitialized then raise Exception.Create(SErrorUninitialised);
  AutoApply := FControl.AutoApply;
  FControl.AutoApply := False;
  try
    (FKeyboards as IKeymanKeyboardsInstalled).Apply;
    (FLanguages as IKeymanLanguages).Apply;
    (FOptions as IKeymanOptions).Apply;
    (FHotkeys as IKeymanHotkeys).Apply;
    FControl.ApplyKeyman;
  finally
    FControl.AutoApply := AutoApply;
  end;
end;

procedure TKeyman.Set_AutoApply(Value: WordBool);
begin
  if not FInitialized then raise Exception.Create(SErrorUninitialised);
  FControl.AutoApply := Value;
end;

function TKeyman.GetFullTagList(const Tag: WideString): OleVariant;
begin
  // Converts array of strings as a Variant array
  Result := TCanonicalLanguageCodeUtils.GetFullTagList(Tag);
end;

initialization
  TAutoObjectFactory.Create(ComServer, TKeyman, Class_Keyman, ciMultiInstance, tmApartment);
end.




