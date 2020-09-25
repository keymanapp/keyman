(*
  Name:             keymankeyboardinstalled
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    13 Mar 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Add Serialize
                    14 Sep 2006 - mcdurdin - Draw keyboard icons as bitmaps for xml serializing
                    14 Sep 2006 - mcdurdin - Support icons instead of bitmaps for keyboards
                    04 Dec 2006 - mcdurdin - Add Get_LayoutType;
                    22 Jan 2007 - mcdurdin - Remove Get_Licence unused function
                    19 Mar 2007 - mcdurdin - I701 - fix leaked COM objects
                    05 Nov 2007 - mcdurdin - I930 - Report an error on an invalid keyboard file
                    27 Mar 2008 - mcdurdin - I1374 - Font helper code - chars and scripts used
                    27 Mar 2008 - mcdurdin - I1220 - Fixup ms office languages - read languages required from keyboard
                    14 Jun 2008 - mcdurdin - Add PrimaryLanguage property
                    12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
                    25 May 2010 - mcdurdin - I1632 - Keyboard Options
                    10 Dec 2010 - mcdurdin - I2558 - Improve LazyWrite performance
                    25 Jan 2011 - mcdurdin - I2569 - Keyboard welcome should always shown from kmshell
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    20 Nov 2012 - mcdurdin - I3581 - V9.0 - KMTip needs to pass activated profile guid through to Keyman32 to switch keyboards
                    01 Jan 2013 - mcdurdin - I3624 - V9.0 - Install keyboard language profiles from Keyman COM API
                    19 Mar 2014 - mcdurdin - I4136 - V9.0 - Add keyboard version information to Keyman Configuration
                    16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard
                    17 Aug 2014 - mcdurdin - I4376 - V9.0 - Unticked keyboards in configuration should be removed from language profile
                    13 Mar 2015 - mcdurdin - I4615 - Keyman crashes when trying to recompile a missing mnemonic layout
*)
unit keymankeyboardinstalled;  // I3306

interface

uses
  System.Classes,
  System.Win.ComObj,
  System.Win.StdVcl,
  Vcl.Graphics,
  Winapi.ActiveX,
  Winapi.MsCTF,
  Winapi.Windows,

  keymanapi_TLB,
  keymanautoobject,
  RegKeyboards,
  keymanvisualkeyboard,
  keymankeyboard,
  keymancontext,
  internalinterfaces;

type
  TKeymanKeyboardInstalled = class;

  TKeymanKeyboardInstalled = class(   // I3581
    TKeymanKeyboard,
    IIntKeymanKeyboardInstalled,
    IKeymanKeyboardInstalled)
  private
    FRegKeyboard: TRegKeyboard;
    FVisualKeyboard: IKeymanVisualKeyboard;
    FLanguages: IKeymanKeyboardLanguagesInstalled;   // I3624
    FKeyboardOptions: IKeymanKeyboardOptions;
    FDefaultHotkey: IKeymanHotkey;

    function Get_OwnerPackageName: WideString;

  protected
    function Serialize(Flags: TOleEnum; const ImagePath: WideString; References: TStrings): WideString;
      override;

    { IKeymanKeyboard }
    function Get_Copyright: WideString; override; safecall;
    function Get_Encodings: KeymanKeyboardEncodings; override; safecall;
    function Get_Filename: WideString; override; safecall;
    function Get_Message: WideString; override; safecall;
    function Get_ID: WideString; override; safecall;
    function Get_Name: WideString; override; safecall;
    function Get_Bitmap: IPicture; override; safecall;
    function Get_LayoutType: TOleEnum; override; safecall;
    function GetCharsUsed: WideString; override; safecall;
    function Get_DefaultBCP47Languages: WideString; override; safecall;
    function Get_DefaultWindowsLanguages: WideString; override; safecall;
    function Get_DefaultPrimaryLanguage: Integer; override; safecall;
    function Get_DefaultHotkey: IKeymanHotkey; override; safecall;
    function Get_Version: WideString; override; safecall;   // I4136

    { IKeymanKeyboardInstalled }
    function Get_IconFilename: WideString; safecall;   // I3581
    function Get_KeymanID: Integer; safecall;
    function Get_Languages: IKeymanKeyboardLanguagesInstalled; safecall;   // I3581
    function Get_Loaded: WordBool; safecall;
    procedure Set_Loaded(Value: WordBool); safecall;
    function Get_Options: IKeymanKeyboardOptions; safecall;
    function Get_OwnerPackage: IKeymanPackageInstalled; safecall;
    function Get_VisualKeyboard: IKeymanVisualKeyboard; safecall;

    procedure InstallVisualKeyboard(const FileName: WideString); safecall;
    procedure Uninstall; safecall;

    { IIntKeymanKeyboardInstalled }
    function RegKeyboard: TRegKeyboard;
    procedure ClearVisualKeyboard;
    procedure UpdateBaseLayout;   // I4169
    procedure RefreshInstallation;

  public
    constructor Create(AContext: TKeymanContext; const Name: string);
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  System.Variants,

  Keyman.System.LanguageCodeUtils,

  KPUninstallKeyboard,
  isadmin,
  OnlineConstants,
  kmxfile,
  ErrorControlledRegistry,
  RegistryKeys,
  custinterfaces,
  KPInstallKeyboard,
  KPInstallVisualKeyboard,
  KPRecompileMnemonicKeyboard,
  keymanhotkey,
  keymankeyboardlanguagesinstalled,
  keymankeyboardoptions,
  keymanerrorcodes,
  PackageInfo,
  utilxml;

procedure TKeymanKeyboardInstalled.Uninstall;
begin
  with TKPUninstallKeyboard.Create(Context) do
  try
    Execute(Get_ID, []);
  finally
    Free;
  end;
end;

procedure TKeymanKeyboardInstalled.UpdateBaseLayout;   // I4169
begin
  if FRegKeyboard.MnemonicLayout and FileExists(FRegKeyboard.KeymanFile) then   // I4615
    with TKPRecompileMnemonicKeyboard.Create(Context) do
    try
      Execute(FRegKeyboard.KeymanFile, FRegKeyboard.PackageName);
    finally
      Free;
    end;
end;

function TKeymanKeyboardInstalled.Get_Copyright: WideString;
begin
  Result := FRegKeyboard.Copyright;
end;

function TKeymanKeyboardInstalled.Get_DefaultBCP47Languages: WideString;
begin
  Result := TLanguageCodeUtils.TranslateMultipleISO6393ToBCP47(FRegKeyboard.ISO6393Languages);
end;

function TKeymanKeyboardInstalled.Get_DefaultHotkey: IKeymanHotkey;
begin
  Result := FDefaultHotkey;
end;

function TKeymanKeyboardInstalled.Get_DefaultPrimaryLanguage: Integer;
begin
  Result := FRegKeyboard.KeyboardLanguageID;
end;

function TKeymanKeyboardInstalled.Get_DefaultWindowsLanguages: WideString;
begin
  Result := FRegKeyboard.WindowsLanguages;
end;

function TKeymanKeyboardInstalled.Get_Filename: WideString;
begin
  Result := FRegKeyboard.KeymanFile;
end;

function TKeymanKeyboardInstalled.Get_KeymanID: Integer;
begin
  Result := FRegKeyboard.KeymanID;
end;

function TKeymanKeyboardInstalled.Get_Message: WideString;
begin
  Result := FRegKeyboard.Message;
end;

function TKeymanKeyboardInstalled.Get_ID: WideString;
begin
  Result := FRegKeyboard.Name;
end;

function TKeymanKeyboardInstalled.Serialize(Flags: TOleEnum; const ImagePath: WideString; References: TStrings): WideString;
var
  FHasVisualKeyboard: Boolean;
  FBitmap, FEncodings: WideString;
begin
  FEncodings := '';
  if (Get_Encodings and keymanapi_TLB.keUnicode) = keymanapi_TLB.keUnicode then
    FEncodings := FEncodings + '<encoding>Unicode</encoding>';
  if (Get_Encodings and keymanapi_TLB.keANSI) = keymanapi_TLB.keANSI then
    FEncodings := FEncodings + '<encoding>ANSI</encoding>';
  if Get_VisualKeyboard <> nil then
    FHasVisualKeyboard := True
  else
    FHasVisualKeyboard := False;

  Result := XMLFormat([
    'id', Get_ID,
    'name', Get_Name,
    'filename', Get_Filename,
    'message', Get_Message,
    'copyright', Get_Copyright,
    'x:encodings', FEncodings, ////
    'loaded', Get_Loaded,
    'ownerpackagename', Get_OwnerPackageName,
    'keymanid', Get_KeymanID,
    'visualkeyboard', FHasVisualKeyboard,
    'layoutpositional', Get_LayoutType = kltPositional,
    'version', Get_Version   // I4136
  ]);

  if ((Flags and ksfExportImages) = ksfExportImages) and Assigned(FRegKeyboard.Icon) then
  begin
    FBitmap := XMLImageTempName(ImagePath, References);
    with Vcl.Graphics.TBitmap.Create do
    try
      Width := 16;
      Height := 16;
      Canvas.Draw(0,0,FRegKeyboard.Icon);
      SaveToFile(FBitmap);
    finally
      Free;
    end;
    Result := Result + '<bitmap>'+ExtractFileName(FBitmap)+'</bitmap>';
  end;

  Result := Result + (Get_Languages as IIntKeymanInterface).DoSerialize(Flags, ImagePath, References);   // I3624
end;

function TKeymanKeyboardInstalled.GetCharsUsed: WideString;
begin
  Result := FRegKeyboard.GetUsedChars;
end;

function TKeymanKeyboardInstalled.Get_Bitmap: IPicture;
var
  pd: TPictDesc;
begin
  if Assigned(FRegKeyboard.Icon) then
  begin
    pd.cbSizeofstruct := SizeOf(TPictDesc);
    pd.picType := PICTYPE_ICON;
    pd.hIcon := FRegKeyboard.Icon.Handle;
    //pd.hpal := 0;
    OleCreatePictureIndirect(pd, IPicture, False, Result);
  end
  else
    Result := nil;
end;

function TKeymanKeyboardInstalled.Get_Loaded: WordBool;
begin
  Result := FRegKeyboard.Enabled;
end;

procedure TKeymanKeyboardInstalled.Set_Loaded(Value: WordBool);
var
  i: Integer;
  lang: IKeymanKeyboardLanguageInstalled2;
  LangID: Integer;
  TemporaryKeyboardID: WideString;
  RegistrationRequired: WordBool;
begin
  if Value then
  begin
    FRegKeyboard.Enabled := True;
    for i := 0 to Get_Languages.Count - 1 do
    begin
      lang := Get_Languages[i] as IKeymanKeyboardLanguageInstalled2;
      if lang.IsInstalled then
      begin
        if lang.FindInstallationLangID(LangID, TemporaryKeyboardID, RegistrationRequired, kifInstallTransientLanguage) and
            not RegistrationRequired then
          lang.InstallTip(LangID, TemporaryKeyboardID)
        else
        begin
          // The registration for the keyboard TIP has somehow disappeared; this is unexpected.
          // We don't want to crash, so we'll just try and clean it up. The user will have to
          // reinstall it, which is not the end of the world.
          lang.Uninstall;
        end;
      end;
    end;
  end
  else
  begin
    FRegKeyboard.Enabled := False;
    for i := 0 to Get_Languages.Count - 1 do
    begin
      lang := Get_Languages[i] as IKeymanKeyboardLanguageInstalled2;
      if lang.IsInstalled then
      begin
        (lang as IIntKeymanKeyboardLanguage).Disable;
      end;
    end;
  end;

  (Context.Keyboards as IKeymanKeyboardsInstalled).Apply;
end;

function TKeymanKeyboardInstalled.Get_IconFilename: WideString;   // I3581
begin
  Result := FRegKeyboard.IconFileName;
end;

function TKeymanKeyboardInstalled.Get_Options: IKeymanKeyboardOptions;
begin
  if FKeyboardOptions = nil then
    FKeyboardOptions := TKeymanKeyboardOptions.Create(Context, Get_ID);
  Result := FKeyboardOptions;
end;

function TKeymanKeyboardInstalled.Get_OwnerPackage: IKeymanPackageInstalled;
var
  i: Integer;
begin
  if FRegKeyboard.PackageName <> '' then
    with Context do
      for i := 0 to (Packages as IKeymanPackagesInstalled).Count - 1 do
        if AnsiCompareText((Packages.Items[i] as IKeymanPackage).ID, FRegKeyboard.PackageName) = 0 then
        begin
          Result := Packages.Items[i] as IKeymanPackageInstalled;
          Exit;
        end;
  Result := nil;
end;

function TKeymanKeyboardInstalled.Get_Languages: IKeymanKeyboardLanguagesInstalled;   // I3581
begin
  if not Assigned(FLanguages) then   // I3624
  begin
    FLanguages := TKeymanKeyboardLanguagesInstalled.Create(Context, Self);
  end;
  Result := FLanguages;
end;

function TKeymanKeyboardInstalled.Get_LayoutType: TOleEnum;
begin
  if FRegKeyboard.MnemonicLayout
    then Result := kltMnemonic
    else Result := kltPositional;
end;

procedure TKeymanKeyboardInstalled.ClearVisualKeyboard;
begin
  FVisualKeyboard := nil;
end;

constructor TKeymanKeyboardInstalled.Create(AContext: TKeymanContext; const Name: string);
begin
  inherited Create(AContext, IKeymanKeyboardInstalled);
  if Name <> '' then
  begin
    FRegKeyboard := TRegKeyboard.Create(Name);
    FRegKeyboard.Load(True);

    if FRegKeyboard.VisualKeyboardInstalled then
      FVisualKeyboard := TKeymanVisualKeyboard.Create(Context, FRegKeyboard.VisualKeyboardFileName, Get_ID);

    FDefaultHotkey := TKeymanHotkey.Create(AContext, FRegKeyboard.DefaultHotkey, khKeyboard);
  end;
end;

destructor TKeymanKeyboardInstalled.Destroy;
begin
  FreeAndNil(FRegKeyboard);
  FVisualKeyboard := nil;
  inherited Destroy;
end;

function TKeymanKeyboardInstalled.Get_OwnerPackageName: WideString;
begin
  Result := FRegKeyboard.PackageName;
end;

function TKeymanKeyboardInstalled.Get_Name: WideString;
begin
  Result := FRegKeyboard.KeyboardName;
end;

function TKeymanKeyboardInstalled.Get_Encodings: KeymanKeyboardEncodings;
begin
  Result := 0;
  if kmxfile.keANSI in FRegKeyboard.Encodings then Result := keymanapi_TLB.keANSI;
  if kmxfile.keUnicode in FRegKeyboard.Encodings then Result := Result or keymanapi_TLB.keUnicode;
end;

function TKeymanKeyboardInstalled.Get_Version: WideString;
begin
  Result := FRegKeyboard.KeyboardVersion;
end;

function TKeymanKeyboardInstalled.Get_VisualKeyboard: IKeymanVisualKeyboard;
begin
  Result := FVisualKeyboard;
end;

procedure TKeymanKeyboardInstalled.InstallVisualKeyboard(const FileName: WideString);
begin
  if FVisualKeyboard <> nil then
    ErrorFmt(KMN_E_VisualKeyboard_Install_AlreadyInstalled, VarArrayOf([Get_ID]));

  with TKPInstallVisualKeyboard.Create(Context) do
  try
    Execute(FileName, Get_ID);
    FRegKeyboard.Load(True);
    if FRegKeyboard.VisualKeyboardInstalled then
      FVisualKeyboard := TKeymanVisualKeyboard.Create(Context, FRegKeyboard.VisualKeyboardFileName, Get_ID);
  finally
    Free;
  end;
end;

/// <summary>Updates installed keyboard to Keyman 14+ registration pattern</summary>
/// <remarks>This refreshes all the registered profiles for the keyboard and registers
/// transient profiles. This function is idempotent. This function requires elevation '
/// to succeed.</remarks>
procedure TKeymanKeyboardInstalled.RefreshInstallation;
var
  kpik: TKPInstallKeyboard;
  o: IKeymanPackageInstalled;
  ll: TPackageKeyboardLanguageList;
begin
  kpik := TKPInstallKeyboard.Create(Context);
  try
    o := Get_OwnerPackage;
    if Assigned(o) then
    begin
      ll := (o as IIntKeymanPackageInstalled).GetKeyboardLanguageList(Get_ID) as TPackageKeyboardLanguageList;
      kpik.RegisterProfiles(FRegKeyboard.KeymanFile, o.ID, [ikPartOfPackage], ll);
    end
    else
      kpik.RegisterProfiles(FRegKeyboard.KeymanFile, '', [], nil);
  finally
    kpik.Free;
  end;
end;

function TKeymanKeyboardInstalled.RegKeyboard: TRegKeyboard;
begin
  Result := FRegKeyboard;
end;

end.

