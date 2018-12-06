(*
  Name:             keymanlanguage
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    14 May 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Add Serialize
                    02 Aug 2006 - mcdurdin - Use HKLIsIME instead of ImmIsIME
                    04 Dec 2006 - mcdurdin - Disable range checks due to HKL/LONGWORD out of range issues
                    25 Jan 2007 - mcdurdin - Fix layout name for languages in Vista
                    21 Mar 2007 - mcdurdin - Use TKeymanKeyboardInstalled internally instead of IKeymanKeyboard
                    05 Nov 2007 - mcdurdin - I1087 - Language switching hotkeys for Pro
                    20 Jul 2008 - mcdurdin - I1498 - Fix Toolbox keyboard switching
                    05 Aug 2008 - mcdurdin - I1576 - Crash after Keyboards.refresh due to dud pointer
                    12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    17 Jan 2013 - mcdurdin - I3763 - V9.0 - GetLanguageName refactor
                    23 Jan 2013 - mcdurdin - I3774 - V9.0 - Regression KMCOMAPI IKeymanLanguage returns region-free language name, incorrectly
                    28 May 2014 - mcdurdin - I4220 - V9.0 - Remove references to LoadKeyboardLayout, Preload, Substitutes, etc. and use only TSF
                    14 May 2015 - mcdurdin - I4712 - V9.0 - MSKLC keyboards do not get correct name in Configuration Hotkeys tab
*)
unit keymanlanguage;

interface

{$R-}

uses
  System.Classes,
  System.Win.ComObj,
  Winapi.ActiveX,
  Winapi.msctf,
  keyman_msctf,
  keymanapi_TLB,
  KeymanContext,
  keymanautoobject,
  keymankeyboardinstalled,
  Windows8LanguageList;

const
  EmptyGuid: TGuid = '{00000000-0000-0000-0000-000000000000}';

type
  TKeymanLanguage = class(TKeymanAutoObject, IKeymanLanguage)
  private
    FProfile: keyman_msctf.TF_INPUTPROCESSORPROFILE;
    FBCP47Code: string;
    FLayoutName: WideString;
    FLocaleName: string;
    FHotkey: IKeymanHotkey;
    function LoadLayoutName: string;

  protected
    function Serialize(Flags: TOleEnum; const ImagePath: WideString;
      References: TStrings): WideString; override;

    { IKeymanLanguage }
    function Get_BCP47Code: WideString; safecall;
    function Get_HKL: LongWord; safecall;
    function Get_Hotkey: IKeymanHotkey; safecall;
    function Get_KeymanKeyboardLanguage: IKeymanKeyboardLanguageInstalled; safecall;
    function Get_LangID: Integer; safecall;
    function Get_LayoutName: WideString; safecall;
    function Get_LocaleName: WideString; safecall;
    function Get_ProfileGUID: TGUID; safecall;
    function Get_ClassID: TGUID; safecall;
  public
    constructor Create(AContext: TKeymanContext; AWin8Language: TWindows8Language; AProfile: keyman_msctf.TF_INPUTPROCESSORPROFILE; ALayoutName: string = '');
  end;


implementation

uses
  Winapi.Windows,
  Imm,
  kmxfile,
  ComServ,
  Glossary,
  SysUtils,
  ErrorControlledRegistry,
  RegistryKeys,
  keymanerrorcodes,
  keymanhotkey,
  utilsystem,
  utilxml,
  GetOSVersion;

function TKeymanLanguage.LoadLayoutName: string;
var
  v: LongWord;
begin
  { Find the name of the keyboard layout }
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    v := HKLToKeyboardID(FProfile.HKL);   // I4712
    if OpenKeyReadOnly('\'+SRegKey_KeyboardLayouts_LM+'\'+IntToHex(v, 8)) then
    begin
      if ValueExists(SRegValue_LayoutDisplayName) then
      begin
        Result := LoadIndirectString(ReadString(SRegValue_LayoutDisplayName));
      end
      else if ValueExists(SRegValue_KeyboardLayoutText) then
        Result := ReadString(SRegValue_KeyboardLayoutText)
      else Result := IntToHex(v, 8);
    end
    else
      Result := IntToHex(v, 8);
  finally
    Free;
  end;
end;

constructor TKeymanLanguage.Create(AContext: TKeymanContext; AWin8Language: TWindows8Language; AProfile: keyman_msctf.TF_INPUTPROCESSORPROFILE; ALayoutName: string = '');
var
  FHotkeyValue: Integer;
  ValueName: string;
  szLangName: array[0..MAX_PATH] of char;
begin
  inherited Create(AContext, IKeymanLanguage);

  FProfile := AProfile;
  if ALayoutName = ''
    then FLayoutName := LoadLayoutName
    else FLayoutName := ALayoutName;

  { Find the locale name }

//  if AWinUserLanguage.Autonym <> '' then
//    FLocaleName := AWinUserLanguage.Autonym
//  else

  if Assigned(AWin8Language) and (Trim(AWin8Language.LocaleName) <> '')
    then FLocaleName := AWin8Language.LocaleName
    else GetLanguageName(FProfile.langid, FLocaleName, True);   // I3763  // I3774

  { Load the status of the hotkey }

  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_CURRENT_USER;

    if IsEqualGuid(Get_ProfileGUID, EmptyGuid)
      then ValueName := IntToHex(Get_HKL, 8)
      else ValueName := GUIDToString(Get_ProfileGUID);

    if OpenKey(SRegKey_LanguageHotkeys_CU, True) and ValueExists(ValueName)
      then FHotkeyValue := StrToIntDef(ReadString(ValueName), 0)
      else FHotkeyValue := 0;
  finally
    Free;
  end;

  FHotkey := TKeymanHotkey.Create(Context, FHotkeyValue, khLanguageSwitch);

  if Assigned(AWin8Language) then
    FBCP47Code := AWin8Language.BCP47Tag
  else if LCIDToLocaleName(MAKELCID(FProfile.langid, SORT_DEFAULT), szLangName, MAX_PATH, 0) <> 0
    then FBCP47Code := szLangName
    else FBCP47Code := '';
end;

function TKeymanLanguage.Get_HKL: LongWord;
begin
  Result := FProfile.HKL;
end;

function TKeymanLanguage.Get_Hotkey: IKeymanHotkey;
begin
  Result := FHotkey;
end;

function TKeymanLanguage.Get_KeymanKeyboardLanguage: IKeymanKeyboardLanguageInstalled;
var
  i, j: Integer;
  kbd: IKeymanKeyboardInstalled;
begin
  with Context.Keyboards do
    for i := 0 to Count - 1 do
    begin
      kbd := Items[i] as IKeymanKeyboardInstalled;
      for j := 0 to kbd.Languages.Count - 1 do
      begin
        if kbd.Languages[j].IsInstalled and (kbd.Languages[j].ProfileGUID = Get_ProfileGUID) then
          Exit(kbd.Languages[j]);
      end;
    end;

  Result := nil;
end;

function TKeymanLanguage.Serialize(Flags: TOleEnum; const ImagePath: WideString;
  References: TStrings): WideString;
var
  FKeymanKeyboardID: WideString;
begin
  if Get_KeymanKeyboardLanguage <> nil
    then FKeymanKeyboardID := Get_KeymanKeyboardLanguage.OwnerKeyboard.ID
    else FKeymanKeyboardID := '';

  Result := XMLFormat([
    'bcp47code', Get_BCP47Code,
    'hkl', Get_HKL,
    'keymankeyboardid', FKeymanKeyboardID,
    'langid', Get_LangID,
    'layoutname', Get_LayoutName,
    'localename', Get_LocaleName,
    'hotkey', HotkeyAsString(Get_Hotkey.RawValue)]);
end;

function TKeymanLanguage.Get_BCP47Code: WideString;
begin
  Result := FBCP47Code;
end;

function TKeymanLanguage.Get_ClassID: TGUID;
begin
  Result := FProfile.clsid;
end;

function TKeymanLanguage.Get_LangID: Integer;
begin
  Result := FProfile.langid;
end;

function TKeymanLanguage.Get_LayoutName: WideString;
begin
  Result := FLayoutName;
end;

function TKeymanLanguage.Get_LocaleName: WideString;
begin
  Result := FLocaleName;
end;

function TKeymanLanguage.Get_ProfileGUID: TGUID;
begin
  Result := FProfile.guidProfile;
end;

{$R+}

end.
