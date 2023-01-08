(*
  Name:             Glossary
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    28 May 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    02 Aug 2006 - mcdurdin - Add HKLIsIME function (tests if IMM is installed before ImmIsIME)
                    23 Aug 2007 - mcdurdin - I981 - Fix ImmIsIME to be always ignored on Vista
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    01 Jan 2013 - mcdurdin - I3724 - V9.0 - GetLanguageName function needs to be general
                    17 Jan 2013 - mcdurdin - I3763 - V9.0 - GetLanguageName refactor
                    23 Jan 2013 - mcdurdin - I3774 - V9.0 - Regression KMCOMAPI IKeymanLanguage returns region-free language name, incorrectly
                    16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard
                    28 May 2014 - mcdurdin - I4220 - V9.0 - Remove references to LoadKeyboardLayout, Preload, Substitutes, etc. and use only TSF
*)
unit Glossary;  // I3306

interface

uses
  System.Classes,
  System.SysUtils,
  Winapi.Imm,
  Winapi.Windows,

  ErrorControlledRegistry,
  RegistryKeys;

type
  PHKL = ^HKL;

function KeyboardIDToHKL(KeyboardID: DWord): HKL; deprecated;   // I4220
function HKLToKeyboardID(ahkl: HKL): DWord;
function HKLToLanguageID(ahkl: HKL): Word;
function HKLToLayoutNumber(ahkl: HKL): Word;
function HKLToLayoutID(ahkl: HKL): Word;
function MAKELCID(lgid, srtid: Word): DWord;
function MAKELANGID(prilangid, sublangid: Word): Word;
function IsKeymanLayout(layout: DWord): Boolean;
function IsKeymanHKL(ahkl: HKL): Boolean;

function GetLanguageName(langid: Word; var LanguageName: string; IncludeRegion: Boolean = False): Boolean;   // I3724   // I3763   // I3774

function GetKeyboardLayouts(var count: Integer): PHKL; // I4169

implementation

uses
  GetOsVersion;

function GetKeyboardLayouts(var count: Integer): PHKL;   // I4169
begin
  Result := nil;
	count := GetKeyboardLayoutList(0, Result^);
	Result := AllocMem(count * SizeOf(HKL));
  if Assigned(Result) then
  	GetKeyboardLayoutList(count, Result^);
end;

function KeyboardIDToHKL(KeyboardID: DWord): HKL;
var
  i, n: Integer;
  hp, hkls: PHKL;
begin
  Result := 0;

  hkls := nil;
	n := GetKeyboardLayoutList(0, hkls^);
	hkls := AllocMem(n * SizeOf(HKL));
  if not Assigned(hkls) then Exit;

  hp := hkls;
	GetKeyboardLayoutList(n, hkls^);
	for i := 0 to n-1 do
		if HKLToKeyboardID(hp^) = KeyboardID then
    begin
      Result := hp^;
      Break;
    end;

	FreeMem(hkls);
end;

function HKLToKeyboardID(ahkl: HKL): DWord;
var
	i, LayoutID: Integer;
  str: TStringList;
begin
	// Test for IME, and if IME return LanguageID

  //if((HIWORD(hkl) & 0xF000) == 0xE000) return (DWORD) hkl;

	// Test for standard layouts: 0409041D for instance
	// specialised layout is: F0020409 fr instance [find layout id:0002]

  if LOWORD(ahkl) = 0 then
  begin
    // Look up the HKL in the registry to get substitutes
    with TRegistryErrorControlled.Create do  // I2890
    try
      if OpenKeyReadOnly('\'+SRegKey_KeyboardLayoutSubstitutes_CU) and
        ValueExists(IntToHex(HIWORD(ahkl), 8)) then
      begin
        Result := DWord(StrToIntDef('$'+ReadString(IntToHex(HIWORD(ahkl), 8)), 0));
        Exit;
      end;
    finally
      Free;
    end;
  end;

	LayoutID := HKLToLayoutID(ahkl);
	if LayoutID = 0 then
  begin
    Result := HIWORD(ahkl);
    Exit;
  end;

	// Find the KeyboardID associated with the LayoutID

  str := TStringList.Create;
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if not OpenKeyReadOnly(SRegKey_KeyboardLayouts_LM) then
    begin
      Result := LOWORD(ahkl);
      Exit;
    end;

    GetKeyNames(str);
    for i := 0 to str.Count - 1 do
      if OpenKeyReadOnly('\'+SRegKey_KeyboardLayouts_LM+'\'+str[i]) and
        ValueExists(SRegValue_KeyboardLayoutID) then
      begin
        if StrToIntDef('$'+ReadString(SRegValue_KeyboardLayoutID), 0) = LayoutID then
        begin
          Result := DWord(StrToIntDef('$'+str[i], 0));
          if Result = 0 then Result := LOWORD(ahkl);
          Exit;
        end;
      end;
  finally
    Free;
    str.Free;
  end;

  Result := LOWORD(ahkl);		// should never happen
end;

function HKLToLanguageID(ahkl: HKL): Word;
begin
  Result := LOWORD(ahkl);
end;

function HKLToLayoutNumber(ahkl: HKL): Word;
var
  LayoutID: Word;
  i: Integer;
  str: TStringList;
begin
  Result := 0;

  LayoutID := HKLToLayoutID(ahkl);
  if LayoutID = 0 then Exit;

  str := TStringList.Create;
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if not OpenKeyReadOnly(SRegKey_KeyboardLayouts_LM) then Exit;
    GetKeyNames(str);
    for i := 0 to str.Count - 1 do
      if OpenKeyReadOnly('\'+SRegKey_KeyboardLayouts_LM+'\'+str[i]) and
        ValueExists(SRegValue_KeyboardLayoutID) then
          if StrToIntDef('$' + ReadString(SRegValue_KeyboardLayoutID), 0) = LayoutID then
        begin
          Result := HIWORD(DWord(StrToIntDef('$'+str[i], 0)));
          Exit;
        end;
  finally
    Free;
    str.Free;
  end;
end;

function HKLToLayoutID(ahkl: HKL): Word;
begin
  if (HIWORD(ahkl) and $F000) <> $F000
    then Result := 0
    else Result := HIWORD(ahkl) and $0FFF;
end;

function MAKELCID(lgid, srtid: Word): DWord;
begin
  Result := (DWord(srtid) shl 16) or lgid;
end;

function IsKeymanHKL(ahkl: HKL): Boolean;
begin
  Result := IsKeymanLayout(HKLToKeyboardID(ahkl));
end;

function IsKeymanLayout(layout: DWord): Boolean;
begin
  Result := LOWORD(layout) = $05FE;
end;

function MAKELANGID(prilangid, sublangid: Word): Word;
begin
  Result := prilangid or (sublangid shl 10);
end;


const
  LOCALE_SLANGDISPLAYNAME = $6f;

function GetLanguageName(langid: Word; var LanguageName: string; IncludeRegion: Boolean = False): Boolean;   // I3724   // I3763  // I3774
var
  szLangName: array[0..MAX_PATH] of char;
  LCType: Cardinal;
begin
  if IncludeRegion
    then LCType := LOCALE_SLANGUAGE  // I3774
    else LCType := LOCALE_SLANGDISPLAYNAME;

  Result := GetLocaleInfo(langid, LCType, szLangName, MAX_PATH) <> 0;
  if Result
    then LanguageName := Copy(szLangName, 0, MAX_PATH - 1)
    else LanguageName := 'Unknown '+IntToHex(langid, 8);
end;

end.
