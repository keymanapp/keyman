(*
  Name:             findfonts
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      27 Mar 2008

  Modified Date:    3 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          27 Mar 2008 - mcdurdin - Initial version
                    14 Jun 2008 - mcdurdin - I1484 - Fix fonts not correctly locating characters
                    29 Mar 2010 - mcdurdin - I2261 - Font list shows bold or italic instead of plain version
                    29 Mar 2010 - mcdurdin - I2267 - Font helper shows incorrect names for some fonts
                    24 Jun 2010 - mcdurdin - I2421 - Font helper lists matching characters
                    29 Jun 2010 - mcdurdin - I2421 - Test against blank and invalid glyphs as well as default
                    18 Feb 2011 - mcdurdin - I2715 - Percentage showed as wrong for some fonts with missing characters
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    03 Aug 2015 - mcdurdin - I4828 - Font identifier miscalculates percentages when supplementary plane characters in string
*)
unit findfonts;  // I3306

interface

uses
  Classes, Contnrs;

type
  TFindFontResult = class
    FontName: WideString;
    Coverage: Integer; { 0 - 100, only fonts with 90% + }
    IncludedChars: WideString;
    ExcludedChars: WideString;
  end;

  TFindFontList = class(TObjectList)
  private
    function GetItem(Index: Integer): TFindFontResult;
    procedure SetItem(Index: Integer; const Value: TFindFontResult);
  public
    function Add(FontName: WideString; Coverage: Integer; IncludedChars, ExcludedChars: WideString): Integer;
    function IndexOfName(const Name: WideString): Integer;
    property Items[Index: Integer]: TFindFontResult read GetItem write SetItem; default;
  end;

procedure FindFontsForChars(const Chars: WideString; MinCoverage: Integer; Fonts: TFindFontList);

implementation

uses
  unicode,
  SysUtils,
  StrUtils,
  Windows;

const
  USP_E_SCRIPT_NOT_IN_FONT: HRESULT = HRESULT($80040200);

type
  PSCRIPT_CACHE = ^SCRIPT_CACHE;
  SCRIPT_CACHE = Pointer;

  SCRIPT_STATE = packed record
    uBidiLevel_Etc_: WORD;
  end;

  SCRIPT_ANALYSIS = packed record
    eScript_Etc: WORD;
    s: SCRIPT_STATE;
  end;

  SCRIPT_ITEM = packed record
    iCharPos: Integer;
    a: SCRIPT_ANALYSIS;
  end;
  PSCRIPT_ITEM = ^SCRIPT_ITEM;

  SCRIPT_VISATTR = packed record
    uJustification_Etc_: WORD;
  end;
  PSCRIPT_VISATTR = ^SCRIPT_VISATTR;


  SCRIPT_CONTROL = packed record
    uDefaultLanguage_Etc_: DWORD;
  end;

  TFindFontEnumData = record
    hdc: HDC;
    Chars: WideString;
    MinCoverage: Integer;
    ExcludedFonts: TStringList;
    Fonts: TFindFontList;
    itemcount: Integer;
    items: PSCRIPT_ITEM;
    outglyphs: PWord;
  end;

  SCRIPT_FONTPROPERTIES = record
    cBytes: integer;
    wgBlank: WORD;
    wgDefault: WORD;
    wgInvalid: WORD;
    wgKashida: WORD;
    iKashidaWidth: Integer;
  end;

  PFindFontEnumData = ^TFindFontEnumData;


function ScriptItemize(text: PWCHAR; text_length: Integer; maxItems: Integer;
  var control: SCRIPT_CONTROL; var state: SCRIPT_STATE; items: PSCRIPT_ITEM; var item_count: Integer): HRESULT; stdcall; external 'usp10.dll';
function ScriptShape(hdc: HDC; var psc: SCRIPT_CACHE; pwcChars: PWCHAR; cChars: Integer; cMaxGlyphs: Integer;
  var psa: SCRIPT_ANALYSIS; OutGlyphs: PWORD; LogClust: PWORD; psva: PSCRIPT_VISATTR; var pcGlyphs: Integer): HRESULT; stdcall; external 'usp10.dll';


function ScriptGetCMap(hdc: HDC; var psc: SCRIPT_CACHE; wsz: PWideChar; cChars: Integer; dwFlags: DWORD; pwOutGlyphs: PWORD): HRESULT; stdcall; external 'usp10.dll';
function ScriptGetFontProperties(hdc: HDC; var psc: SCRIPT_CACHE; var sfp: SCRIPT_FONTPROPERTIES): HRESULT; stdcall; external 'usp10.dll';
function ScriptFreeCache(var psc: SCRIPT_CACHE): HRESULT; stdcall; external 'usp10.dll';

function DoScriptItemize(const text: WideString; var items: PSCRIPT_ITEM; var itemcount: Integer): Boolean;
var
  control: SCRIPT_CONTROL;
  state: SCRIPT_STATE;
  max_items: Integer;
  hr: HRESULT;
begin

  FillChar(control, sizeof(control), 0);
  FillChar(state, sizeof(state), 0);

  items := nil;
  max_items := 8;
  repeat
    max_items := max_items * 2;
    ReallocMem(items, max_items * sizeof(SCRIPT_ITEM));

    itemcount := 0;
    hr := ScriptItemize(PWideChar(text), Length(text), max_items - 1, control, state, items, itemcount);
    if SUCCEEDED(hr) then
    begin
      Inc(itemcount);
      ReallocMem(items, itemcount * sizeof(SCRIPT_ITEM));
      Result := True;
      Exit;
    end;

  until hr <> E_OUTOFMEMORY;

  FreeMem(items);
  Result := False;
end;

function DoScriptShape(input: PWideChar; input_length: Integer;
  hfont: THandle; var uscript_cache: SCRIPT_CACHE; temp_dc: HDC;
  var analysis: SCRIPT_ANALYSIS;
  var logs: PWORD;
  var glyphs: PWORD;
  var visattr: PSCRIPT_VISATTR;
  var glyphs_used: Integer): HRESULT;
var
  glyphs_size: Integer;
begin
  logs := AllocMem(input_length * sizeof(WORD));
  glyphs_size := input_length * 3 div 2 + 16;
  glyphs := AllocMem(glyphs_size * sizeof(WORD));
  visattr := AllocMem(glyphs_size * sizeof(WORD));

//  Result := E_UNEXPECTED;

  while True do
  begin
    Result := ScriptShape(temp_dc, uscript_cache, input, input_length, glyphs_size, analysis,
      glyphs, logs, visattr, glyphs_used);

    if SUCCEEDED(Result) then
    begin
      // It worked, resize the output list to the exact number it returned.
      ReallocMem(glyphs, glyphs_used * sizeof(WORD));
      Break;
    end;

    // Different types of failure...
    if Result = E_OUTOFMEMORY then
    begin
      // The glyph buffer needs to be larger. Just double it every time.
      glyphs_size := glyphs_size * 2;
      ReallocMem(glyphs, glyphs_size * sizeof(WORD));
      ReallocMem(visattr, glyphs_size * sizeof(WORD));
      // Loop again...
    end
    else if Result = USP_E_SCRIPT_NOT_IN_FONT then
    begin
      // The font you selected doesn't have enough information to display
      // what you want. You'll have to pick another one somehow...
      // For our cases, we'll just return failure.
      Break;
    end
    else
    begin
      // Some other failure.
      Break;
    end;
  end;

  if not Succeeded(Result) then
  begin
    FreeMem(visattr); visattr := nil;
    FreeMem(logs);   logs := nil;
    FreeMem(glyphs); glyphs := nil;
  end;
end;

function FindFontEnum(lpelfe: PENUMLOGFONTEX; lpntme: PNewTextMetricEx; FontType: DWORD; lParam: LPARAM): integer; stdcall;
var
  data: PFindFontEnumData;
  hfont: THandle;
  analysis: SCRIPT_ANALYSIS;
  props: SCRIPT_FONTPROPERTIES;
  nTotal: Integer;
  nDef: Integer;
  I: Integer;
  logs: PWord;
  glyphs: PWord;
  visattr: PSCRIPT_VISATTR;
  glyphs_used: Integer;
    temp_dc: THandle;
    old_font: THandle;
  item, item2: PSCRIPT_ITEM;
  j: Integer;
  hr: HRESULT;
  psc: SCRIPT_CACHE;
  nGlyph: Word;
  IncludedChars: WideString;
  ExcludedChars: WideString;
begin
  Result := 1;

  if FontType <> TRUETYPE_FONTTYPE then Exit;

  data := PFindFontEnumData(lParam);
  if data.Fonts.IndexOfName(lpelfe.elfLogFont.lfFaceName) >= 0 then Exit;
  if data.ExcludedFonts.IndexOf(lpelfe.elfLogFont.lfFaceName) >= 0 then Exit;

  if lpelfe.elfLogFont.lfFaceName[0] = '@' then
  begin
    data.ExcludedFonts.Add(lpelfe.elfLogFont.lfFaceName);
    Exit; // Ignore Far-East font names
  end;

  SetLastError(0);

  ////if Copy(lpelfe.elfLogFont.lfFaceName, 1, Length('Palatino Linotype')) <> 'Palatino Linotype' then Exit;

  hfont := CreateFontIndirect(lpelfe.elfLogFont);
  if GetLastError <> 0 then RaiseLastOSError;
  try
    temp_dc := GetDC(0);
    old_font := SelectObject(temp_dc, hfont);
    try

      nDef := 0;
      nTotal := 0;

      item := data.items;
      item2 := item; Inc(item2);

      FillChar(props, sizeof(props), 0);

      psc := nil;

      ExcludedChars := '';
      IncludedChars := '';

      for i := 0 to data.itemcount - 2 do
      begin
        analysis.eScript_Etc := item.a.eScript_Etc;
        analysis.s.uBidiLevel_Etc_ := item.a.s.uBidiLevel_Etc_;

        hr := DoScriptShape(PWideChar(@data.Chars[item.iCharPos+1]), item2.iCharPos - item.iCharPos,
          hfont, psc, temp_dc, analysis, logs, glyphs, visattr, glyphs_used);
        if SUCCEEDED(hr) then
        begin
          if props.cBytes = 0 then
          begin
            props.cBytes := sizeof(props);
            hr := ScriptGetFontProperties(0, psc, props);
            if not SUCCEEDED(hr) then
            begin
              data.ExcludedFonts.Add(lpelfe.elfLogFont.lfFaceName);
              Exit; // Ignore fonts that give us errors
            end;
          end;

          // ... check for default glyph

          for j := item.iCharPos to item2.iCharPos - 1 do
          begin
            nGlyph := PWordArray(logs)[j - item.iCharPos];
            if (PWordArray(glyphs)[nGlyph] = props.wgDefault) or (PWordArray(glyphs)[nGlyph] = props.wgInvalid) or (PWordArray(glyphs)[nGlyph] = props.wgBlank) then
              // I2421 - Test against blank and invalid glyphs as well as default
            begin
              if not Uni_IsSurrogate2(data.Chars[j+1]) then Inc(nDef);   // I4828
              ExcludedChars := ExcludedChars + data.Chars[j+1];
            end
            else
              IncludedChars := IncludedChars + data.Chars[j+1];
            if not Uni_IsSurrogate2(data.Chars[j+1]) then Inc(nTotal);   // I4828
          end;

          FreeMem(visattr);
          FreeMem(glyphs);
          FreeMem(logs);
        end
        else
        begin
          { I1484 - fix problem where single missing character could stop font from being matched }
          ExcludedChars := ExcludedChars + Copy(data.Chars, item.iCharPos + 1, item2.iCharPos - item.iCharPos);  // I2715
          for j := item.iCharPos to item2.iCharPos - 1 do   // I4828
          begin
            if not Uni_IsSurrogate2(data.Chars[j+1]) then
            begin
              Inc(nTotal);
              Inc(nDef);
            end;
          end;
        end;
        Inc(item); Inc(item2);
      end;

      if (nTotal > 0) and ((nTotal-nDef) * 100 div nTotal > data.MinCoverage)
        then data.Fonts.Add(lpelfe.elfLogFont.lfFaceName, (nTotal-nDef) * 100 div nTotal,IncludedChars,ExcludedChars)
        else data.ExcludedFonts.Add(lpelfe.elfLogFont.lfFaceName);
    finally
      SelectObject(temp_dc, old_font);
      ReleaseDC(0, temp_dc);
    end;
  finally
    if psc <> nil then
      ScriptFreeCache(psc);
    DeleteObject(hfont);
  end;
end;

function SortFonts(Item1, Item2: Pointer): Integer;
begin
  Result := TFindFontResult(Item2).Coverage - TFindFontResult(Item1).Coverage; // best match first
  if Result = 0 then
    Result := WideCompareStr(TFindFontResult(Item1).FontName, TFindFontResult(Item2).FontName);
end;

procedure FindFontsForChars(const Chars: WideString; MinCoverage: Integer; Fonts: TFindFontList);
var
  data: TFindFontEnumData;
  lf: TLogFont;
  hdc: THandle;
  i: Integer;
begin                                  
  if Chars = '' then Exit;

  try
    hdc := GetDC(0);
    try
      data.Chars := Chars;
      data.MinCoverage := MinCoverage;
      data.Fonts := Fonts;
      data.ExcludedFonts := TStringList.Create;
      data.hdc := hdc;
      if not DoScriptItemize(PWideChar(data.Chars), data.items, data.itemcount) then RaiseLastOSError;
      //if not DoScriptLayout(  ScriptLayout(data.itemcount - 1, directions,
      try
        data.outglyphs := AllocMem(SizeOf(WORD) * (Length(Chars) * 5));
        try
          FillChar(lf, sizeof(lf), 0);
          lf.lfCharSet := DEFAULT_CHARSET;
          EnumFontFamiliesEx(hdc, lf, @FindFontEnum, Integer(@data), 0);
        finally
          FreeMem(data.outglyphs);
        end;
        for i := 0 to data.ExcludedFonts.Count - 1 do
          data.Fonts.Add(data.ExcludedFonts[i], -1, '', '');
      finally
        if data.items <> nil then
          FreeMem(data.items);
        data.ExcludedFonts.Free;
      end;
    finally
      ReleaseDC(0, hdc);
    end;
  except
    on E:Exception do
    begin
      //KL.Log(E.Message);
      raise;
    end;
  end;

  data.Fonts.Sort(SortFonts);
end;

{ TFindFontList }

function TFindFontList.Add(FontName: WideString; Coverage: Integer; IncludedChars, ExcludedChars: WideString): Integer;
var
  f: TFindFontResult;
begin
  f := TFindFontResult.Create;
  f.FontName := FontName;
  f.Coverage := Coverage;
  f.IncludedChars := IncludedChars;
  f.ExcludedChars := ExcludedChars;
  Result := inherited Add(f);
end;

function TFindFontList.GetItem(Index: Integer): TFindFontResult;
begin
  Result := inherited GetItem(Index) as TFindFontResult;
end;

function TFindFontList.IndexOfName(const Name: WideString): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if WideSameText(Items[i].FontName, Name) then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

procedure TFindFontList.SetItem(Index: Integer; const Value: TFindFontResult);
begin
  inherited SetItem(Index, Value);
end;

end.
