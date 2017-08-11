(*
  Name:             UfrmUnloadKeyboards
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:
  Description:
  Create Date:      17 Aug 2014

  Modified Date:    17 Aug 2014
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          17 Aug 2014 - mcdurdin - I4376 - V9.0 - Unticked keyboards in configuration should be removed from language profile

*)
unit UfrmUnloadKeyboards;   // I4376

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls, msctf,

  System.Generics.Collections;

type
  TKeyboardItemLegacyRegistry = record
    Exists: Boolean;
    Preload: DWORD;
    Substitute: DWORD;
  end;

  TKeyboardItemLegacyLoaded = record
    Exists: Boolean;
    Handle: HKL;
  end;

  TKeyboardItemTSFRegistry = record
    Exists: Boolean;
    // TODO
  end;

  TKeyboardItemTSFLoaded = record
    Exists: Boolean;
    Enabled: Boolean;
    Valid: Boolean;
    Profile: TF_INPUTPROCESSORPROFILE;
  end;

  TKeyboardItem = class
  private
    FKeyboardName: string;

    // Legacy Registry Settings
    FLegacyRegistry: TKeyboardItemLegacyRegistry;

    // Legacy Loaded Settings
    FLegacyLoaded: TKeyboardItemLegacyLoaded;

    // Registry TSF
    FTSFRegistry: TKeyboardItemTSFRegistry;

    // Loaded TSF
    FTSFLoaded: TKeyboardItemTSFLoaded;
  public
    constructor Create(const AKeyboardName: string);
//    destructor Destroy; override;
    property KeyboardName: string read FKeyboardName;
    property LegacyRegistry: TKeyboardItemLegacyRegistry read FLegacyRegistry;
    property LegacyLoaded: TKeyboardItemLegacyLoaded read FLegacyLoaded;
    property TSFRegistry: TKeyboardItemTSFRegistry read FTSFRegistry;
    property TSFLoaded: TKeyboardItemTSFLoaded read FTSFLoaded;
  end;

  TKeyboardItems = class(TObjectList<TKeyboardItem>);

  TLanguageItem = class
  private
    FLanguageName: string;
    FLanguageID: Word;
    FKeyboards: TKeyboardItems;
  public
    constructor Create(ALanguageID: Word);
    destructor Destroy; override;
    property LanguageID: Word read FLanguageID;
    property LanguageName: string read FLanguageName;
    property Keyboards: TKeyboardItems read FKeyboards;
  end;

  TLanguageItems = class(TObjectList<TLanguageItem>);

type
  TForm1 = class(TForm)
    grid: TStringGrid;
    cmdUnload: TButton;
    cmdExit: TButton;
    cmdRefresh: TButton;
    cmdLoadKeyboardLayout: TButton;
    Button1: TButton;
    Button2: TButton;
    memoDetail: TMemo;
    Button3: TButton;
    Button4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure cmdUnloadClick(Sender: TObject);
    procedure cmdExitClick(Sender: TObject);
    procedure cmdRefreshClick(Sender: TObject);
    procedure cmdLoadKeyboardLayoutClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure gridClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
//    FKeyboards: TKeyboardItems;
//    rk: array of TRegKeyboard;
    FLanguages: TLanguageItems;
    procedure FillGrid;
    procedure FillMemo;
    function GetKeyboardName(h: HKL): string;

    procedure EnumLegacyRegistry;
    procedure EnumLegacyLoaded;
    procedure EnumTSFRegistry;
    procedure EnumTSFLoaded;

//    procedure LoadRegKeyboardList;
//    function IsRegInstalledKeyboard(h: HKL): Boolean;
    function GetLanguageItem(LangID: Word): TLanguageItem;
    procedure RefreshLanguages;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  System.Math,
  System.StrUtils,
  System.Win.ComObj,
  System.Win.Registry,
  Winapi.ActiveX,

  input_installlayoutortip,
  glossary,
  LangManager,
  RegistryKeys;

type
  PHKL = ^HKL;


function GetKeyboardLayoutListX(nBuff: Integer; List: PHKL): UINT; stdcall; external 'user32.dll' name 'GetKeyboardLayoutList';

procedure TForm1.cmdLoadKeyboardLayoutClick(Sender: TObject);
begin
  if LoadKeyboardLayout('00000448', 0) = 0 then RaiseLastOSError;
  RefreshLanguages;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  pInputProcessorProfiles: ITfInputProcessorProfiles;
  pInputProcessorProfileMgr: ItfInputProcessorProfileMgr;
  langid: Word;
  clsid: GUID;
  guidProfile: GUID;

  function GetDefaultHKL: HKL;   // I3581   // I3619   // I3619
  begin
    if not SystemParametersInfo(SPI_GETDEFAULTINPUTLANG, 0, @Result, 0) then
      Result := 0;
  end;

begin
  OleCheck(CoCreateInstance(CLASS_TF_InputProcessorProfiles, nil, CLSCTX_INPROC_SERVER,
                          IID_ITfInputProcessorProfiles, pInputProcessorProfiles));

  if not Supports(pInputProcessorProfiles, IID_ITfInputProcessorProfileMgr, pInputProcessorProfileMgr) then   // I3743
    raise Exception.Create('Missing interface IID_ITfInputProcessorProfileMgr');

  // Get default language
  langid := HKLToLanguageID(GetDefaultHKL);
  if pInputProcessorProfiles.GetDefaultLanguageProfile(langid, GUID_TFCAT_TIP_KEYBOARD, clsid, guidProfile) <> S_OK then
  begin
    ShowMessage('oops');
  end;

  ShowMessage('Default Lang: '+IntToHex(langid, 4)+', Default CLSID: '+GUIDToString(clsid)+', Default GUID:'+GUIDToString(guidProfile));
//    pInputProcessorProfiles.ActivateLanguageProfile(clsid, langid, guidProfile);
  //  // If that is a Keyman keyboard, then find first non-Keyman keyboard
//  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  kbd: TKeyboardItem;
  s: string;
var
  pInputProcessorProfiles: ITfInputProcessorProfiles;
  pInputProcessorProfileMgr: ItfInputProcessorProfileMgr;
  langid: Word;
  clsid: GUID;
  guidProfile: GUID;

begin
  kbd := grid.Objects[0, grid.Row] as TKeyboardItem;
  if kbd.FTSFLoaded.Exists then
  begin

    OleCheck(CoCreateInstance(CLASS_TF_InputProcessorProfiles, nil, CLSCTX_INPROC_SERVER,
                            IID_ITfInputProcessorProfiles, pInputProcessorProfiles));

    if not Supports(pInputProcessorProfiles, IID_ITfInputProcessorProfileMgr, pInputProcessorProfileMgr) then   // I3743
      raise Exception.Create('Missing interface IID_ITfInputProcessorProfileMgr');

    OleCheck(pInputProcessorProfiles.EnableLanguageProfile(kbd.FTSFLoaded.Profile.clsid,
      kbd.TSFLoaded.Profile.langid, kbd.FTSFLoaded.Profile.guidProfile, 0));

//    s := Format('0x%0.04x:%s%s', [kbd.TSFLoaded.Profile.langid,
//      GuidToString(kbd.TSFLoaded.Profile.clsid),
//      GuidToString(kbd.TSFLoaded.Profile.guidProfile)]);
//    if not InstallLayoutOrTip(PChar(s), ILOT_UNINSTALL) then
//      RaiseLastOSError;
  end;

//  if kbd.FLegacyLoaded.Exists then
//    if not UnloadKeyboardLayout(kbd.FLegacyLoaded.Handle) then
//      RaiseLastOSError;
  RefreshLanguages;
  FillGrid;
//  InstallLayoutOrTip(
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  Fprofiles: ITfInputProcessorProfiles;
  Fprofilemgr: ITfInputProcessorProfileMgr;
  ppEnumIPP: IEnumGUID;
  ppE: IEnumTfLanguageProfiles;
  langprofile: TF_LANGUAGEPROFILE;
var
  i: Integer;
  pcFetch: Cardinal;
  profile: TF_INPUTPROCESSORPROFILE;
  ppEnum: IEnumTfInputProcessorProfiles;
var
  pLangID: PWord1;
  ulCount: Cardinal;
  FLanguage: TLangSwitchLanguage;
  FLanguages: TLangSwitchLanguages;
  hr: HResult;
begin
  FProfiles := CreateComObject(CLASS_TF_InputProcessorProfiles) as ITfInputProcessorProfiles;
  if not Supports(FProfiles, ITfInputProcessorProfileMgr, FProfileMgr) then  // I2864
    FProfileMgr := nil;


  FLanguages := TLangSwitchLanguages.Create(nil);



  FProfiles.GetLanguageList(pLangID, ulCount);
  for I := 0 to ulCount - 1 do
  begin
    FLanguage := TLangSwitchLanguage.Create(nil, pLangID^);
    FLanguages.Add(FLanguage);
    Inc(pLangID);
  end;
  Dec(pLangID, ulCount);
  CoTaskMemFree(pLangID);

  Assert(Assigned(FProfileMgr));  // I2864

  for I := 0 to FLanguages.Count - 1 do
  begin
    FProfileMgr.EnumProfiles(FLanguages[i].LangID, ppEnum);
    while ppEnum.Next(1, profile, pcFetch) = S_OK do
    begin
      if profile.dwProfileType = TF_PROFILETYPE_KEYBOARDLAYOUT then
      begin
        memoDetail.Lines.Add(format('Language %x KeyboardLayout %x', [FLanguages[i].LangID, profile.HKL]));
      end
      else if (profile.catid = GUID_TFCAT_TIP_KEYBOARD) then  // I2826   // I3933   // I4005
      begin
        memoDetail.Lines.Add(format('Language %x TIP %s', [FLanguages[i].LangID, GuidToString(profile.guidProfile)]));
      end;
    end;
  end;

  for I := 0 to FLanguages.Count - 1 do
  begin
    FProfiles.EnumLanguageProfiles(FLanguages[i].LangID, ppE);
    hr := ppE.Next(1, langprofile, pcFetch);
    while hr = S_OK do
    begin
      memoDetail.Lines.Add(format('Language %x TIP %s', [FLanguages[i].LangID,
        GuidToString(langprofile.guidProfile)]));
      hr := ppE.Next(1, langprofile, pcFetch);
    end;
  end;
end;

procedure TForm1.cmdExitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.cmdRefreshClick(Sender: TObject);
begin
  RefreshLanguages;
end;

procedure TForm1.cmdUnloadClick(Sender: TObject);
var
  kbd: TKeyboardItem;
begin
  kbd := grid.Objects[0, grid.Row] as TKeyboardItem;
  if kbd.FLegacyLoaded.Exists then
    if not UnloadKeyboardLayout(kbd.FLegacyLoaded.Handle) then
      RaiseLastOSError;
  RefreshLanguages;
  FillGrid;
end;

procedure TForm1.EnumLegacyLoaded;
var
  n: Integer;
  hp, hkls: PHKL;
  i: Integer;
  ki: TKeyboardItem;
  lang: TLanguageItem;
begin
  hkls := nil;
  n := GetKeyboardLayoutListX(0, hkls);
  if n = 0 then n := 1;
  hkls := AllocMem(SizeOf(HKL) * n);
  try
    GetKeyboardLayoutListX(n, hkls);
    hp := hkls;
    for i := 0 to n - 1 do
    begin
      lang := GetLanguageItem(HKLToLanguageID(hp^));
      ki := TKeyboardItem.Create(GetKeyboardName(hp^));
      ki.FLegacyLoaded.Exists := True;
      ki.FLegacyLoaded.Handle := hp^;
      lang.Keyboards.Add(ki);
      Inc(hp);
    end;
  finally
    FreeMem(hkls);
  end;
end;

function TForm1.GetLanguageItem(LangID: Word): TLanguageItem;
var
  lang: TLanguageItem;
begin
  for lang in FLanguages do
    if lang.LanguageID = LangID then
      Exit(lang);

  Result := TLanguageItem.Create(LangID);
  FLanguages.Add(Result);
end;


procedure TForm1.gridClick(Sender: TObject);
begin
  FillMemo;
end;

procedure TForm1.EnumLegacyRegistry;
type
  TRegKeyboard = record
    Index: string;
    BaseValue: string;
    SubstValue: string;
    PresumedHKL: HKL;
  end;
  TRegKeyboards = array of TRegKeyboard;

var
  lang: TLanguageItem;
  kbd: TKeyboardItem;
  i: Integer;
  s: TStringList;
  rk: TRegKeyboards;
  Found: Boolean;
begin
  s := TStringList.Create;
  with TRegistry.Create do
  try
    if not OpenKeyReadOnly(SRegKey_KeyboardLayoutPreload) then Exit;

    GetValueNames(s);
    SetLength(rk, s.Count);
    for i := 0 to s.Count - 1 do
    begin
      rk[i].Index := s[i];
      rk[i].BaseValue := ReadString(s[i]);
    end;
    CloseKey;

    if not OpenKeyReadOnly(SRegKey_KeyboardLayoutSubstitutes) then Exit;

    for i := 0 to s.Count - 1 do
    begin
      if ValueExists(rk[i].BaseValue)
        then rk[i].SubstValue := ReadString(rk[i].BaseValue)
        else rk[i].SubstValue := '';
      rk[i].PresumedHKL := MakeLong(
        LoWord(StrToIntDef('$'+rk[i].BaseValue, 0)),
        LoWord(StrToIntDef('$'+IfThen(rk[i].SubstValue='', rk[i].BaseValue, rk[i].SubstValue), 0)));

      lang := GetLanguageItem(LoWord(rk[i].PresumedHKL));

      Found := False;

      for kbd in lang.Keyboards do
      begin
        if kbd.FLegacyLoaded.Handle = rk[i].PresumedHKL then
        begin
          kbd.FLegacyRegistry.Exists := True;
          kbd.FLegacyRegistry.Preload := StrToIntDef('$'+rk[i].BaseValue, 0);
          kbd.FLegacyRegistry.Substitute := StrToIntDef('$'+rk[i].SubstValue, 0);
          Found := True;
          Break;
        end;
      end;

      if not Found then
      begin
        kbd := TKeyboardItem.Create(GetKeyboardName(rk[i].PresumedHKL));
        kbd.FLegacyRegistry.Exists := True;
        kbd.FLegacyRegistry.Preload := StrToIntDef('$'+rk[i].BaseValue, 0);
        kbd.FLegacyRegistry.Substitute := StrToIntDef('$'+rk[i].SubstValue, 0);
        lang.Keyboards.Add(kbd);
      end;
    end;
  finally
    Free;
    s.Free;
  end;
end;

  // For now, only comparing registry to loaded list, not adding missing items
//  for lang in FLanguages do
//  begin
//    for kbd in lang.Keyboards do
//      kbd.FLegacyRegistry.Exists := IsRegInstalledKeyboard(kbd.FLegacyLoaded.Handle);
//  end;

  // TODO: for each preload item, find the matching loaded keyboard, and if missing, then
//end;

//      True;
//          then grid{ TLanguageItem }

(*.Cells[4, i+1] := 'Installed'
          else grid.Cells[4, i+1] := 'Transient';

        grid.Cells[5, i+1] := '';
        grid.Cells[6, i+1] := '';

        Inc(hp);
      end;
    finally
      FreeMem(hkls);
    end;
  finally
    m.Free;
  end;

end;


end;
*)

procedure TForm1.EnumTSFLoaded;
var
  m: TLangSwitchManager;
  mk, ml: Integer;
  lang: TLanguageItem;
  k: TLangSwitchKeyboard_WinKeyboard;
  kt: TLangSwitchKeyboard_TIP;
  kbd: TKeyboardItem;
  Found: Boolean;
begin
  m := TLangSwitchManager.Create;
  try
    m.Refresh;

    for ml := 0 to m.LanguageCount-1 do
    begin
      lang := GetLanguageItem(m.Languages[ml].LangID);

      for mk := 0 to m.Languages[ml].KeyboardCount - 1 do
      begin
        if m.Languages[ml].Keyboards[mk].ItemType = lsitWinKeyboard then
        begin
          k := m.Languages[ml].Keyboards[mk] as TLangSwitchKeyboard_WinKeyboard;

          Found := False;
          for kbd in lang.Keyboards do
          begin
            if k.HKL = kbd.FLegacyLoaded.Handle then
            begin
              kbd.FTSFLoaded.Exists := True;
              kbd.FTSFLoaded.Enabled := k.Enabled;
              kbd.FTSFLoaded.Valid := k.Valid;
              Found := True;
              Break;
            end;
          end;
          if not Found then
          begin
            kbd := TKeyboardItem.Create(k.Caption);
            kbd.FTSFLoaded.Exists := True;
            kbd.FTSFLoaded.Enabled := k.Enabled;
            kbd.FTSFLoaded.Valid := k.Valid;
            lang.Keyboards.Add(kbd);
          end;
        end
        else
        begin
          kt := m.Languages[ml].Keyboards[mk] as TLangSwitchKeyboard_TIP;
          kbd := TKeyboardItem.Create(kt.Caption);
//          kbd.FTSFRegistry.Exists := True;
          kbd.FTSFLoaded.Exists := true;
          kbd.FTSFLoaded.Enabled := kt.Enabled;
          kbd.FTSFLoaded.Valid := kt.Valid;
          kbd.FTSFLoaded.Profile := kt.Profile;
          lang.Keyboards.Add(kbd);
        end;
      end;
    end;
  finally
    m.Free;
  end;
end;

procedure TForm1.EnumTSFRegistry;
begin
  // TODO
end;

function TForm1.GetKeyboardName(h: HKL): string;
var
  v: Cardinal;
begin
  v := HKLToKeyboardID(h);
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(SRegKey_KeyboardLayouts + '\' + IntToHex(v,8)) and
        ValueExists(SRegValue_KeyboardLayoutText) then
      Exit(ReadString(SRegValue_KeyboardLayoutText));
  finally
    Free;
  end;
  Result := '???';
end;

//function TForm1.IsRegInstalledKeyboard(h: HKL): Boolean;
//var
//  i: Integer;
//begin
  //TODO: this is a mess to get right! - substitutes, IMEs, custom keyboards, etc.
//  for i := 0 to High(rk) do
//    if rk[i].PresumedHKL = h then
//      Exit(True);
//  Result := False;
//end;

procedure TForm1.FillGrid;
  function TrueStr(v: Boolean): string;
  begin
    if v then Result := 'True' else Result := '';
  end;
var
  n: Integer;
  lang: TLanguageItem;
  kbd: TKeyboardItem;
  x, w, y: Integer;
  gw: Integer;
begin
//  LoadRegKeyboardList;

  grid.Cells[0, 0] := 'Language Name';
  grid.Cells[1, 0] := 'Language ID';

  grid.Cells[2, 0] := 'Keyboard Name';
  grid.Cells[3, 0] := 'LegacyLoaded?';
  grid.Cells[4, 0] := 'LegacyRegistry?';
  grid.Cells[5, 0] := 'TSFLoaded?';
  grid.Cells[6, 0] := 'TSFEnabled?';
  grid.Cells[7, 0] := 'TSFValid?';

  n := 1;

  for lang in FLanguages do
  begin
    for kbd in lang.Keyboards do
    begin
      grid.RowCount := n+1;

      grid.Objects[0, n] := kbd;
      grid.Objects[1, n] := lang;

      grid.Cells[0, n] := lang.LanguageName;
      grid.Cells[1, n] := IntToHex(lang.LanguageID, 4);

      grid.Cells[2, n] := kbd.KeyboardName;
      grid.Cells[3, n] := TrueStr(kbd.FLegacyLoaded.Exists);
      grid.Cells[4, n] := TrueStr(kbd.FLegacyRegistry.Exists);
      grid.Cells[5, n] := TrueStr(kbd.FTSFLoaded.Exists);
      grid.Cells[6, n] := TrueStr(kbd.FTSFLoaded.Enabled);
      grid.Cells[7, n] := TrueStr(kbd.FTSFLoaded.Valid);
      Inc(n);
    end;
  end;

  gw := 0;
  for x := 0 to grid.ColCount - 1 do
  begin
    w := 0;
    for y := 0 to grid.RowCount - 1 do
      w := Max(w,grid.Canvas.TextWidth(grid.Cells[x,y]));
    grid.ColWidths[x] := w + 8;
    Inc(gw, w + 9);
  end;
  ClientWidth := (ClientWidth-grid.ClientWidth) + gw - 1;
  ClientHeight := (ClientHeight-grid.ClientHeight) + (grid.DefaultRowHeight+1) * grid.RowCount - 1;
end;

procedure TForm1.FillMemo;
var
  lang: TLanguageItem;
  kbd: TKeyboardItem;
  progid: PWideChar;
begin
  lang := grid.Objects[1, grid.Row] as TLanguageItem;
  kbd := grid.Objects[0, grid.Row] as TKeyboardItem;
  memoDetail.Text := '';

  memoDetail.Lines.Add(Format('Language %s', [lang.LanguageName]));
  memoDetail.Lines.Add(Format('  Name = %s', [lang.LanguageName]));
  memoDetail.Lines.Add(Format('  LangId = %x', [lang.LanguageID]));
  memoDetail.Lines.Add(Format('  Keyboards.Count = %d', [lang.Keyboards.Count]));
  memoDetail.Lines.Add(Format('  SelectedKeyboardIndex = %d', [lang.Keyboards.IndexOf(kbd)]));

  memoDetail.Lines.Add('');

  memoDetail.Lines.Add(Format('Keyboard %s', [kbd.KeyboardName]));
  memoDetail.Lines.Add(Format('  Name = %s', [kbd.KeyboardName]));
  memoDetail.Lines.Add('');

  if kbd.LegacyRegistry.Exists then
  begin
    memoDetail.Lines.Add('  Has legacy registration');
    memoDetail.Lines.Add(Format('  Registry Preload = %x', [kbd.LegacyRegistry.Preload]));
    memoDetail.Lines.Add(Format('  Registry Substitute = %x', [kbd.LegacyRegistry.Substitute]));
    memoDetail.Lines.Add('');
  end;

  if kbd.LegacyLoaded.Exists then
  begin
    memoDetail.Lines.Add('  Has legacy HKL in memory');
    memoDetail.Lines.Add(Format('  HKL = %x', [kbd.LegacyLoaded.Handle]));
    memoDetail.Lines.Add('');
  end;

  if kbd.TSFRegistry.Exists then
  begin
    memoDetail.Lines.Add('  Has TSF registration');
    memoDetail.Lines.Add('');
  end;

  if kbd.TSFLoaded.Exists then
  begin
    ProgIDFromCLSID(kbd.TSFLoaded.Profile.clsid, progid);

    memoDetail.Lines.Add('  Has TSF entry in memory');
    memoDetail.Lines.Add(Format('  Enabled = %s', [BoolToStr(kbd.TSFLoaded.Enabled,True)]));
    memoDetail.Lines.Add(Format('  Valid = %s', [BoolToStr(kbd.TSFLoaded.Valid,True)]));
    memoDetail.Lines.Add(Format('  Profile.dwProfileType = %x', [kbd.TSFLoaded.Profile.dwProfileType]));
    memoDetail.Lines.Add(Format('  Profile.langid = %x', [kbd.TSFLoaded.Profile.langid]));
    memoDetail.Lines.Add(Format('  Profile.clsid = %s [%s]', [GUIDToString(kbd.TSFLoaded.Profile.clsid),progid]));
    memoDetail.Lines.Add(Format('  Profile.guidProfile = %s', [GUIDToString(kbd.TSFLoaded.Profile.guidProfile)]));
    memoDetail.Lines.Add(Format('  Profile.catid = %s', [GUIDToString(kbd.TSFLoaded.Profile.catid)]));
    memoDetail.Lines.Add(Format('  Profile.hklSubstitute = %x', [kbd.TSFLoaded.Profile.hklSubstitute]));
    memoDetail.Lines.Add(Format('  Profile.dwCaps = %x', [kbd.TSFLoaded.Profile.dwCaps]));
    memoDetail.Lines.Add(Format('  Profile.HKL = %x', [kbd.TSFLoaded.Profile.HKL]));
    memoDetail.Lines.Add(Format('  Profile.dwFlags = %x [%s]', [kbd.TSFLoaded.Profile.dwFlags, '']));

    CoTaskMemFree(progid);
  end;
//    property KeyboardName: string read FKeyboardName;
//    property LegacyRegistry: TKeyboardItemLegacyRegistry read FLegacyRegistry;
//    property LegacyLoaded: TKeyboardItemLegacyLoaded read FLegacyLoaded;
//    property TSFRegistry: TKeyboardItemTSFRegistry read FTSFRegistry;
//    property TSFLoaded: TKeyboardItemTSFLoaded read FTSFLoaded;
//
//    property LanguageID: Word read FLanguageID;
//    property LanguageName: string read FLanguageName;
//
//  memoDetail.Lines.Add('Keyboard Name: '+kbd.KeyboardName);
//    '
end;

function IsAdministrator: Boolean;
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if not OpenKey(SRegKey_KeyboardLayouts, False) then Exit(False);
  finally
    Free;
  end;

  Result := True;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := 'Keyboard Info - '+IfThen(IsAdministrator, 'Administrator', 'User')+' - '+{$IFDEF WIN64}'x64'{$ELSE}'x86'{$ENDIF};
  RefreshLanguages;
end;

procedure TForm1.RefreshLanguages;
begin
  FreeAndNil(FLanguages);
  FLanguages := TLanguageItems.Create;
  // Order is important
  EnumLegacyLoaded;
  EnumLegacyRegistry;
  EnumTSFRegistry;
  EnumTSFLoaded;
  FillGrid;
end;

{ TKeyboardItem }

constructor TKeyboardItem.Create(const AKeyboardName: string);
begin
  inherited Create;
  FKeyboardName := AKeyboardName;
end;

{ TLanguageItem }

constructor TLanguageItem.Create(ALanguageID: Word);
begin
  inherited Create;
  FKeyboards := TKeyboardItems.Create;
  FLanguageID := ALanguageID;

  if not GetLanguageName(FLanguageID, FLanguageName, True) then
    FLanguageName := '';
end;

destructor TLanguageItem.Destroy;
begin
  FreeAndNil(FKeyboards);
  inherited Destroy;
end;

end.
