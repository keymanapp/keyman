(*
  Name:             BaseKeyboards
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:
  Description:
  Create Date:      16 Apr 2014

  Modified Date:    16 Apr 2014
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard

*)
unit BaseKeyboards;   // I4169

interface

uses
  System.Classes;

type
  TBaseKeyboards = class
  private
    class function IsLatinKeyboard(const KeyboardID: string; LatinKeyboardIDs: TStrings): Boolean;
    class function TestAndCacheIsLatinKeyboard(
      const KeyboardID: string): Boolean; static;
  public
    class function EnumerateXML(BaseKeyboardID: Cardinal): string;
    class function GetName(BaseKeyboardID: Cardinal): string;
  end;

implementation

uses
  Winapi.Windows,
  System.Character,
  System.SysUtils,
  System.Win.Registry,

  Glossary,
  LoadIndirectStringUnit,
  RegistryKeys,
  UnicodeBlocks,
  utilxml;

{ TBaseKeyboards }

class function TBaseKeyboards.EnumerateXML(BaseKeyboardID: Cardinal): string;
var
  FKeyNames: TStrings;
  I: Integer;
  FCaption: string;
  FKeyboardID: Integer;
  FLanguageName: string;
  FLangNames: TStringList;
  FLatinKeyboardIDs: TStringList;
begin
  FLangNames := TStringList.Create;
  FKeyNames := TStringList.Create;
  FLatinKeyboardIDs := TStringList.Create;
  try
    with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly(SRegKey_LatinKeyboardCache_LM) then
      begin
        GetValueNames(FLatinKeyboardIDs);
        for i := 0 to FLatinKeyboardIDs.Count - 1 do
          FLatinKeyboardIDs.ValueFromIndex[i] := ReadString(FLatinKeyboardIDs[i]);
      end;
    finally
      Free;
    end;

    with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly(SRegKey_KeyboardLayouts_LM) then
      begin
        GetKeyNames(FKeyNames);
        for I := 0 to FKeyNames.Count - 1 do
        begin
          if TryStrToInt('$'+FKeyNames[i], FKeyboardID) and
             OpenKeyReadOnly('\' + SRegKey_KeyboardLayouts_LM + '\' + FKeyNames[i]) then
          begin
            if not IsLatinKeyboard(FKeyNames[i], FLatinKeyboardIDs) then
              Continue;

            if ValueExists(SRegValue_LayoutDisplayName) then
            begin
              FCaption := LoadIndirectString(ReadString(SRegValue_LayoutDisplayName));
            end
            else if ValueExists(SRegValue_KeyboardLayoutText) then
              FCaption := ReadString(SRegValue_KeyboardLayoutText)
            else
              FCaption := FKeyNames[i];

            GetLanguageName(HKLToLanguageID(FKeyboardID), FLanguageName);

            if not SameText(Copy(FCaption, 1, Length(FLanguageName)), FLanguageName) then
              FCaption := FLanguageName + ' - ' + FCaption;

            FLangNames.AddObject(FCaption, Pointer(FKeyboardID));
          end;
        end;
      end;

      Result := '<BaseKeyboards>';
      FLangNames.Sort;
      for I := 0 to FLangNames.Count-1 do
      begin
        Result := Result + '<BaseKeyboard>'+
          '<id>'+IntToHex(Integer(FLangNames.Objects[i]),8)+'</id>'+
          '<name>'+XMLEncode(FLangNames[i])+'</name>';
        if Cardinal(FLangNames.Objects[i]) = BaseKeyboardID then
          Result := Result + '<selected />';
        Result := Result + '</BaseKeyboard>';
      end;
      Result := Result + '</BaseKeyboards>';

    finally
      Free;
    end;
  finally
    FKeyNames.Free;
    FLangNames.Free;
  end;
end;


class function TBaseKeyboards.GetName(BaseKeyboardID: Cardinal): string;
var
  FCaption: string;
  FLanguageName: string;
  FKeyboardIDString: string;
begin
  FKeyboardIDString := IntToHex(BaseKeyboardID, 8);
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(SRegKey_KeyboardLayouts_LM + '\' + FKeyboardIDString) then
    begin
      if ValueExists(SRegValue_LayoutDisplayName) then
      begin
        FCaption := LoadIndirectString(ReadString(SRegValue_LayoutDisplayName));
      end
      else if ValueExists(SRegValue_KeyboardLayoutText) then
      begin
        FCaption := ReadString(SRegValue_KeyboardLayoutText)
      end
      else
      begin
        FCaption := FKeyboardIDString;
      end;

      GetLanguageName(HKLToLanguageID(BaseKeyboardID), FLanguageName);

      if not SameText(Copy(FCaption, 1, Length(FLanguageName)), FLanguageName) then
        FCaption := FLanguageName + ' - ' + FCaption;

      Result := FCaption;
    end;
  finally
    Free;
  end;
end;

class function TBaseKeyboards.IsLatinKeyboard(const KeyboardID: string; LatinKeyboardIDs: TStrings): Boolean;
var
  v: string;
begin
  v := LatinKeyboardIDs.Values[KeyboardID];
  if v = ''
    then Result := TestAndCacheIsLatinKeyboard(KeyboardID)
    else Result := v = '1';
end;

function IsLatin(ch: string): Boolean;
var
  i: Integer;
begin
  if Char.IsLetter(ch, 0) then
    for i := 0 to High(SUnicodeBlocks) do
      if (Ord(ch[1]) >= SUnicodeBlocks[i].Ch1) and (Ord(ch[1]) <= SUnicodeBlocks[i].Ch2) then
        Exit(SameText(SUnicodeBlocks[i].CleanName, 'Latin'));

  Result := False;
end;

class function TBaseKeyboards.TestAndCacheIsLatinKeyboard(const KeyboardID: string): Boolean;
var
  i, v, count: Integer;
  h: HKL;
  hp, hkls: PHKL;
  Found: Boolean;
  FIsLatin: Integer;
  buf: array[0..8] of char;
  keystate: TKeyboardState;
begin
  FIsLatin := 0;

  if not TryStrToInt('$'+KeyboardID, v) then
  begin
    // Ignore the keyboard ID if it is not hexadecimal
  end
  else if PRIMARYLANGID(v) in [LANG_ARABIC, LANG_CHINESE, LANG_JAPANESE, LANG_KOREAN] then
  begin
    // Special case: ignore Arabic, CJK "English" alternate keyboards
  end
  else if Copy(KeyboardID, 1, 1) = '0' then
  begin
    // Only support basic system-supplied DLLs -- not custom "A" or IME/TSF "E" DLLs
    GetKeyboardState(keystate);
    hkls := GetKeyboardLayouts(count);
    if Assigned(hkls) then
    begin
      // Test the keyboard layout A-Z characters for Latin letters.
      h := LoadKeyboardLayout(KeyboardID, KLF_NOTELLSHELL);

      if h <> 0 then
      begin
        for i := Ord('A') to Ord('Z') do
        begin
          case ToUnicodeEx(i, 0, keystate, buf, 8, 0, h) of
            -1: Continue;
            0: Continue;
            1: ;
            else Break;
          end;

//          ch := buf[0];
          if not Char.IsLetter(buf, 0) then
            Continue;
          if IsLatin(buf)
            then FIsLatin := 1;
          Break;
        end;

        // Unload the keyboard layout if not already loaded
        hp := hkls;
        Found := False;
        for i := 0 to count - 1 do
        begin
          if hp^ = h then
          begin
            Found := True;
            Break;
          end;
          Inc(hp);
        end;

        if not Found then
        begin
          UnloadKeyboardLayout(h);
        end;
      end;

      FreeMem(hkls);
    end;
  end;

  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKey(SRegKey_LatinKeyboardCache_LM, True) then
    begin
      WriteString(KeyboardID, IntToStr(FIsLatin));
    end;
  finally
    Free;
  end;

  Result := FIsLatin = 1;
end;

end.
