(*
  Name:             CleartypeDrawCharacter
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Aug 2006

  Modified Date:    24 Feb 2019
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2006 - mcdurdin - Initial version
                    14 Sep 2006 - mcdurdin - Add style, showprefix, beginupdate, endupdate
                    14 Sep 2006 - mcdurdin - Look for alternate fallback fonts when Code200 not installed
                    06 Oct 2006 - mcdurdin - Only do font linking if GetGlyphIndicesW fails
                    07 Feb 2007 - mcdurdin - Fix plane 1 character display font linking
                    30 May 2007 - mcdurdin - Add support for resizing text to fit into a bounding box
                    31 Aug 2008 - mcdurdin - I720 - Support Uniscribe display of text on OSK
                    19 Apr 2010 - mcdurdin - I2311 - Fix font linking with Character Map in plane 1
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    28 Feb 2014 - mcdurdin - I4098 - V9.0 - OSK is still 8.0 style
                    04 Nov 2014 - mcdurdin - I4488 - V9.0 - The character map is not falling back to system fonts well when Code2000 missing
                    24 Feb 2019 - mayura   - V11.0 - Removed font fallback and coloring code, since it is not required in windows 7 and above
*)
unit CleartypeDrawCharacter;

interface

uses
  Windows, Classes, Graphics, Types, ComCtrls, usp10,
  System.Character;

(*
type
  PSCRIPT_STRING_ANALYSIS = Pointer;
  SCRIPT_CACHE = Pointer;

  SCRIPT_FONTPROPERTIES = record
    cBytes: Integer;
    wgBlank: WORD;
    wgDefault: WORD;
    wgInvalid: WORD;
    wgKashida: WORD;
    iKashidaWidth: Integer;
  end;

  PSCRIPT_FONTPROPERTIES = ^SCRIPT_FONTPROPERTIES;

  SCRIPT_ITEM = record
  end;

  PSCRIPT_ITEM = ^SCRIPT_ITEM;

  SCRIPT_CONTROL = record
  end;

  PSCRIPT_CONTROL = ^SCRIPT_CONTROL;

  SCRIPT_STATE = record
  {WORD uBidiLevel :5;
  WORD fOverrideDirection :1;
  WORD fInhibitSymSwap :1;
  WORD fCharShape :1;
  WORD fDigitSubstitute :1;
  WORD fInhibitLigate :1;
  WORD fDisplayZWG :1;
  WORD fArabicNumContext :1;
  WORD fGcpClusters :1;
  WORD fReserved :1;
  WORD fEngineReserved :2;}
    FFlags: WORD;
  end;

  PSCRIPT_STATE = ^SCRIPT_STATE;

  SCRIPT_VISATTR = record

  end;

  PSCRIPT_VISATTR = ^SCRIPT_VISATTR;

  SCRIPT_ANALYSIS = record
    eScriptAndFlags: WORD;
    s: SCRIPT_STATE;
  end;

  PSCRIPT_ANALYSIS = ^SCRIPT_ANALYSIS;

function ScriptStringAnalyse(hdc: HDC; psz: PWideChar; cch, cGlyphs, iCharset: Integer; dwFlags: DWORD; iReqWidth: Integer; psControl, psState, piDx, pTabdef, pbInClass: Pointer; var pssa: PSCRIPT_STRING_ANALYSIS): HRESULT; stdcall; external 'usp10.dll';
function ScriptStringOut(ssa: PSCRIPT_STRING_ANALYSIS; iX, iY: Integer; uOptions: UINT; prc: PRect; iMinSel, iMaxSel: Integer; fDisabled: Boolean): HRESULT; stdcall; external 'usp10.dll';
function ScriptStringFree(var ssa: PSCRIPT_STRING_ANALYSIS): HRESULT; stdcall; external 'usp10.dll';

const
  SSA_LINK = $1000;
  SSA_GLYPHS = $80;
  SSA_FALLBACK = $20;
  
function ScriptFreeCache(var cache: SCRIPT_CACHE): HRESULT; stdcall; external 'usp10.dll';
function ScriptGetCMap(hdc: HDC; var cache: SCRIPT_CACHE; pwcInChars: PWideChar; cChars: Integer; dwFlags: DWORD; outGlyphs: PWord): HRESULT; stdcall; external 'usp10.dll';
function ScriptGetFontProperties(hdc: HDC; var cache: SCRIPT_CACHE; var properties: SCRIPT_FONTPROPERTIES): HRESULT; stdcall; external 'usp10.dll';

function ScriptItemize(pwcInChars: PWideChar; cInChars: Integer; cMaxItems: Integer; const psControl: SCRIPT_CONTROL; const psState: SCRIPT_STATE; psItems: PSCRIPT_ITEMS; var pcItems: Integer): HRESULT; stdcall; external 'usp10.dll';
function ScriptShape(hdc: HDC; var cache: SCRIPT_CACHE; pwcInChars: PWideChar; cChars: Integer; cMaxGlyphs: Integer; var psa: SCRIPT_ANALYSIS; pwOutGlyphs: PWORD; pwLogClust: PWORD; psva: PSCRIPT_VISATTR; var pcGlyphs: Integer): HRESULT; stdcall; external 'usp10.dll';
*)

type
  TClearTypeDisplayQuality = (ctPlain, ctAntialias, ctCleartype);

  { Does a font-linked character draw }

  TClearTypeFont = (ctfSelected, ctfCode2000, ctfCode2001, ctfCode2002);

  TClearTypeDrawCharacter = class
  private
    FDisplayQuality: TClearTypeDisplayQuality;
    FFontStyle: TFontStyles;
    FFontName: WideString;
    FColor: TColor;

    hFonts: array[TClearTypeFont] of THandle;
    FCaches: array[TClearTypeFont] of SCRIPT_CACHE;
    FFontProperties: array[TClearTypeFont] of SCRIPT_FONTPROPERTIES;
    FSelectedFont: TClearTypeFont;
    //hCode2000Font, hCode2001Font, hCode2002Font, hFont: THandle;

    FFontHeight: Integer;
    FUpdating: Integer;
    FCreateFonts: Boolean;
    FShowPrefix: Boolean;

    procedure SetDisplayQuality(const Value: TClearTypeDisplayQuality);
    function FLTextOut(hdc: THandle; x, y: Integer; Rect: PRect; psz: PWideChar;
      cch: Integer; FitRect, Uniscribe: Boolean): HRESULT;
    procedure CreateFonts;
    procedure SetShowPrefix(const Value: Boolean);
    function GetGlyphIndex(Handle: THandle; const Text: WideString;
      var Index: Integer): Boolean;
    function SelectFont(Handle: THandle; FFont: TClearTypeFont): THandle;
    procedure FreeFonts;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function TextExtent(Handle: THandle; const Text: WideString): TPoint;
    procedure DrawText(Handle: THandle; TextAlign, X, Y: Integer; ARect: TRect; const Text: WideString; Fit: Boolean = False; Uniscribe: Boolean = False);
    function CalcTextSize(Handle: THandle; ARect: TRect; const Text: WideString): Integer;
    procedure SetFontDetails(const Name: WideString; Height: Integer; Style: TFontStyles = []);
    property DisplayQuality: TClearTypeDisplayQuality read FDisplayQuality write SetDisplayQuality;
    property Color: TColor read FColor write FColor;
    property ShowPrefix: Boolean read FShowPrefix write SetShowPrefix;
  end;

implementation

uses
  ActiveX,
  GetOSVersion,
  Math,
  SysUtils,
  Unicode;

procedure SetPlane0FallbackFont; forward;

var
  FPlane0FontName: string = '';

{ TClearTypeDrawCharacter }

procedure TClearTypeDrawCharacter.BeginUpdate;
begin
  if FUpdating = 0 then
    FCreateFonts := False;
  Inc(FUpdating);
end;

function TClearTypeDrawCharacter.CalcTextSize(Handle: THandle; ARect: TRect;
  const Text: WideString): Integer;

      function DoCalcTextSize: Integer;
      var
        sz: TSize;
        hOldFont: THandle;
        lf: LOGFONT;
      begin
        GetTextExtentPoint32W(Handle, PWideChar(Text), Length(Text), sz);
        if (sz.cx > ARect.Right - ARect.Left) then // or (sz.cy > Rect.Bottom - Rect.Top) then
        begin
          hOldFont := GetCurrentObject(Handle, OBJ_FONT);
          GetObject(hOldFont, sizeof(LOGFONT), @lf);
          Result := lf.lfHeight * (ARect.Right - ARect.Left) div sz.cx;
          Result := -Result * 72 div GetDeviceCaps(Handle, LOGPIXELSY);
        end
        else   // I4098
        begin
          hOldFont := GetCurrentObject(Handle, OBJ_FONT);
          GetObject(hOldFont, sizeof(LOGFONT), @lf);
          Result := -lf.lfHeight * 72 div GetDeviceCaps(Handle, LOGPIXELSY);
        end;
      end;
begin
  SelectFont(Handle, ctfSelected);
  Result := DoCalcTextSize;
end;

constructor TClearTypeDrawCharacter.Create;
begin
  inherited Create;
  if FPlane0FontName = '' then
    SetPlane0FallbackFont;
end;

destructor TClearTypeDrawCharacter.Destroy;
begin
  FreeFonts;
  inherited Destroy;
end;

procedure TClearTypeDrawCharacter.FreeFonts;
var
  I: TClearTypeFont;
begin
  for I := Low(TClearTypeFont) to High(TClearTypeFont) do
  begin
    if hFonts[I] <> 0 then DeleteObject(hFonts[I]);
    hFonts[I] := 0;
    if FCaches[I] <> nil then
      ScriptFreeCache(@FCaches[I]);
    FCaches[I] := nil;
    FFontProperties[I].cBytes := 0;
  end;

  inherited Destroy;
end;

(*
var
  _GetGlyphIndicesW: function (hdc: HDC; lpstr: PWIDECHAR; c: Integer; pgi: PWORD; fl: DWORD): DWORD; stdcall; // = nil;
  _GetGlyphIndicesWLoaded: Boolean = False;

function GetGlyphIndicesW_(hdc: HDC; lpstr: PWIDECHAR; c: Integer; pgi: PWORD; fl: DWORD): DWORD;
var
  hGDI: THandle;
begin
  if not _GetGlyphIndicesWLoaded then
  begin
    hGDI := GetModuleHandle('gdi32.dll');
    _GetGlyphIndicesW := GetProcAddress(hGDI, 'GetGlyphIndicesW');
    _GetGlyphIndicesWLoaded := True;
  end;
  if not Assigned(_GetGlyphIndicesW) then Result := 0
  else Result := _GetGlyphIndicesW(hdc, lpstr, c, pgi, fl);
end;
*)

function TClearTypeDrawCharacter.SelectFont(Handle: THandle; FFont: TClearTypeFont): THandle;
begin
  FSelectedFont := FFont;
  Result := SelectObject(Handle, hFonts[FSelectedFont]);
end;

function TClearTypeDrawCharacter.GetGlyphIndex(Handle: THandle; const Text: WideString; var Index: Integer): Boolean;
var
  len: Integer;

  items: array[0..9] of TScriptItem;
  control: TScriptControl;
  state: TScriptState;
  nitems: Integer;
  glyphs: array[0..9] of WORD;
  clusters: array[0..9] of WORD;
  attr: array[0..9] of TScriptVisAttr;
  nglyphs, maxItems: Integer;
  res: HRESULT;
begin
  Result := False;

  if (Length(Text) > 1) and Uni_IsSurrogate1(Text[1]) and Uni_IsSurrogate2(Text[2]) then len := 2 else len := 1;

  FillChar(control, sizeof(SCRIPT_CONTROL), 0);
  FillChar(state, sizeof(SCRIPT_STATE), 0);

  state.uBidiLevel := 0;
  Include(state.fFlags, fOverrideDirection);
  maxItems := 5;

  //items := PScriptItem(AllocMem(10 * sizeof(SCRIPT_ITEM)));

  res := ScriptItemize(PWideChar(text), len, maxItems, @control, @state, @items, @nitems);
  if res <> S_OK then
    Exit;

  Include(items[0].a.s.fFlags, fDisplayZWG);

  res := ScriptShape(Handle, @FCaches[FSelectedFont], PWideChar(text), len, 10, @items[0].a, @glyphs[0], @clusters[0], @attr[0], @nglyphs);
  if res = S_OK then
  begin
    if FFontProperties[FSelectedFont].cBytes = 0 then
    begin
      FFontProperties[FSelectedFont].cBytes := sizeof(SCRIPT_FONTPROPERTIES);
      ScriptGetFontProperties(Handle, @FCaches[FSelectedFont], @FFontProperties[FSelectedFont]);
    end;

    if (glyphs[0] = FFontProperties[FSelectedFont].wgDefault) or
        (glyphs[0] = FFontProperties[FSelectedFont].wgInvalid) then
      Index := $FFFF
    else
      Index := glyphs[0];
    Result := True;
  end
  else if res = HRESULT(USP_E_SCRIPT_NOT_IN_FONT) then
  begin
    Index := $FFFF;
    Result := True;
  end;
end;


procedure TClearTypeDrawCharacter.DrawText(Handle: THandle; TextAlign, X, Y: Integer; ARect: TRect; const Text: WideString; Fit: Boolean = False; Uniscribe: Boolean = False);
var
  hOrigFont: THandle;
  FOldColor: Cardinal;
  FOldAlign: Cardinal;
begin
  //FSelectedFont := ctfSelected;

  if (Text = '') or (hFonts[ctfSelected] = 0) then Exit;

  FOldColor := SetTextColor(Handle, ColorToRGB(FColor));
  hOrigFont := SelectFont(Handle, ctfSelected); // SelectObject(Handle, hFonts[FSelectedFont]);

  { Draw character[s] }

  FOldAlign := SetTextAlign(Handle, TextAlign); // TA_CENTER or TA_TOP);

  FLTextOut(Handle, X, Y, @ARect, PWideChar(Text), Length(Text), Fit, Uniscribe); // RectBmp.Right div 2, 0, @RectBmp, PWideChar(ch), Length(ch));

  SelectObject(Handle, hOrigFont);
  SetTextColor(Handle, FOldColor);
  SetTextAlign(Handle, FOldAlign);
end;

procedure TClearTypeDrawCharacter.EndUpdate;
begin
  if FUpdating > 0 then
  begin
    Dec(FUpdating);
    if FUpdating = 0 then if FCreateFonts then CreateFonts;
  end;
end;

procedure TClearTypeDrawCharacter.SetDisplayQuality(const Value: TClearTypeDisplayQuality);
begin
  if FDisplayQuality <> Value then
  begin
    FDisplayQuality := Value;
    CreateFonts;
  end;
end;

procedure TClearTypeDrawCharacter.SetFontDetails(const Name: WideString; Height: Integer; Style: TFontStyles = []);
begin
  if (Name = FFontName) and (Height = FFontHeight) then Exit;

  FFontName := Name;
  FFontHeight := Height;
  FFontStyle := Style;

  CreateFonts;
end;

procedure TClearTypeDrawCharacter.SetShowPrefix(const Value: Boolean);
begin
  FShowPrefix := Value;
end;

function TClearTypeDrawCharacter.TextExtent(Handle: THandle; const Text: WideString): TPoint;
var
  hOrigFont: THandle;
  sz: TSize;
begin
  if Text = '' then
  begin
    Result := Point(0,0);
    Exit;
  end;

  hOrigFont := SelectFont(Handle, ctfSelected);

  { Draw character[s] }

  GetTextExtentPoint32W(Handle, PWideChar(Text), Length(Text), sz);

  Result := Point(sz.cx, sz.cy);

  SelectObject(Handle, hOrigFont);
end;

procedure TClearTypeDrawCharacter.CreateFonts;
var
  lf: TLogFont;
begin
  if FUpdating > 0 then
  begin
    FCreateFonts := True;
    Exit;
  end;

  FreeFonts;

  { Create font for character }

  lf.lfHeight := FFontHeight;
  lf.lfWidth := 0; { have font mapper choose }
  lf.lfEscapement := 0;
  lf.lfOrientation := 0;
  if fsBold in FFontStyle
    then lf.lfWeight := FW_BOLD
    else lf.lfWeight := FW_NORMAL;
  if fsItalic in FFontStyle
    then lf.lfItalic := 1
    else lf.lfItalic := 0;
  if fsUnderline in FFontStyle
    then lf.lfUnderline := 1
    else lf.lfUnderline := 0;
  if fsStrikeOut in FFontStyle
    then lf.lfStrikeOut := 1
    else lf.lfStrikeOut := 0;
  lf.lfCharSet := DEFAULT_CHARSET;
  case FDisplayQuality of
    ctPlain: lf.lfQuality := NONANTIALIASED_QUALITY;
    ctAntialias: lf.lfQuality := ANTIALIASED_QUALITY;
    ctCleartype: lf.lfQuality := 5; //CLEARTYPE_QUALITY;
  end;

  lf.lfOutPrecision := OUT_DEFAULT_PRECIS;
  lf.lfClipPrecision := CLIP_DEFAULT_PRECIS;
  lf.lfPitchAndFamily := DEFAULT_PITCH;
  StrPLCopy(lf.lfFaceName, FFontName, 31);

  hFonts[ctfSelected] := CreateFontIndirect(lf);

  StrPCopy(lf.lfFaceName, FPlane0FontName);
  hFonts[ctfCode2000] := CreateFontIndirect(lf);

  StrPCopy(lf.lfFaceName, 'Code2001');
  hFonts[ctfCode2001] := CreateFontIndirect(lf);

  StrPCopy(lf.lfFaceName, 'Code2002');
  hFonts[ctfCode2002] := CreateFontIndirect(lf);
end;


function TClearTypeDrawCharacter.FLTextOut(hdc: THandle; x, y: Integer; Rect: PRect; psz: PWideChar; cch: Integer; FitRect, Uniscribe: Boolean): HRESULT;

  procedure UniscribeTextOut(hdc: THandle; x, y, flags: Integer; Rect: PRect; psz: PWideChar; cch: Integer; FitRect: Boolean);
  var
    ssa: PScriptStringAnalysis;
    hr: HRESULT;
  begin
    ssa := nil;
    hr := ScriptStringAnalyse(hdc, psz, cch, 0, -1, SSA_LINK or SSA_GLYPHS or SSA_FALLBACK, Rect.Right-Rect.Left, nil, nil, nil, nil, nil, ssa);
    if hr = ERROR_SUCCESS then
    begin
      hr := ScriptStringOut(ssa, x, y, 0, nil, 1, 0, False);
      if hr = ERROR_SUCCESS then
        ScriptStringFree(ssa);
    end;
  end;

  procedure DoExtTextOut(hdc: THandle; x, y, flags: Integer; Rect: PRect; psz: PWideChar; cch: Integer; FitRect: Boolean);
  var
    lf: LOGFONT;
//    c: DWord;

    hOldFont, hTempFont: THandle;

    pi, pj: PWideChar;
    q: WideString;
    i: Integer;
    j: Integer;
    sz: TSize;
    pt: TPoint;
    ch: Integer;
  begin
    if (cch in [1,2]) and GetGlyphIndex(hdc, psz, ch) and (ch = $FFFF) then
    begin
      { Use a default glyph bitmap here }
      q := psz; //'?';   // I4488
      //c := SetTextColor(hdc, $e0e0e0);
      ExtTextOutW(hdc, x, y, flags, Rect, psz, cch, nil); //PWideChar(q), 1, nil);
//      SetTextColor(hdc, c);
    end
    else
    begin
      if FShowPrefix then
      begin
        i := 0; j := 0; pi := psz; pj := psz;
        while i < cch do
        begin
          if pi^ = '&' then
          begin
            ExtTextOutW(hdc, x, y, flags, Rect, pj, i-j, nil);
            Inc(pi); Inc(i);
            if (pi^ <> '&') and (pi^ <> #0) then
            begin
              GetTextExtentPoint32W(hdc, pi, 1, sz);
              GetCurrentPositionEx(hdc, @pt);
              ExtTextOutW(hdc, 0, 0, flags, Rect, '_', 1, nil);
              MoveToEx(hdc, pt.X, pt.y, nil);
            end;
            j := i;
            pj := psz; Inc(pj, j);
          end;
          Inc(pi); Inc(i);
        end;
        if j < cch then
          ExtTextOutW(hdc, x, y, flags, Rect, pj, cch-j, nil);
      end
      else
      begin
        hOldFont := 0;
        hTempFont := 0;

        //if Uniscribe then UniscribeTextOut(hdc, x, y, flags, Rect, psz, cch, FitRect)
        //else
        begin
          if FitRect then
          begin
            GetTextExtentPoint32W(hdc, psz, cch, sz);
            if (sz.cx > Rect.Right - Rect.Left) then // or (sz.cy > Rect.Bottom - Rect.Top) then
            begin
              hOldFont := GetCurrentObject(hDC, OBJ_FONT);
              GetObject(hOldFont, sizeof(LOGFONT), @lf);
              lf.lfHeight := lf.lfHeight * (Rect.Right - Rect.Left) div sz.cx;
              hTempFont := CreateFontIndirect(lf);
              SelectObject(hdc, hTempFont);
            end;
          end;

          ExtTextOutW(hdc, x, y, flags, Rect, psz, cch, nil);
          if hOldFont <> 0 then
          begin
            SelectObject(hdc, hOldFont);
            DeleteObject(hTempFont);
          end;
        end;
      end;
    end;
  end;
var
  ptOrig: TPOINT;
  dwAlignOrig: DWORD;
  FOldMode: Integer;
begin
  FOldMode := SetBkMode(hdc, TRANSPARENT);
  dwAlignOrig := GetTextAlign(hdc);
  if (dwAlignOrig and TA_UPDATECP) = 0 then
    SetTextAlign(hdc, dwAlignOrig or TA_UPDATECP);

  MoveToEx(hdc, x, y, @ptOrig);

  DoExtTextOut(hdc, 0, 0,  ETO_CLIPPED, rect, psz, cch, FitRect);

  if (dwAlignOrig and TA_UPDATECP) = 0 then
  begin
    SetTextAlign(hdc, dwAlignOrig);
    MoveToEx(hdc, ptOrig.x, ptOrig.y, nil);
  end;

  SetBkMode(hdc, FOldMode);
  Result := S_OK;
end;

function EnumFallbackFonts(lpelfe: PEnumLogFontEx; lpntme: PNewTextMetricEx; FontType: DWord; lParam: LParam): integer; StdCall;
begin
  Result := 1;
end;

function TestFont(FFontName: string): Boolean;
var
  lf: TLogFont;
  hdc: THandle;
begin
  FillChar(lf, SizeOf(TLogFont), 0);
  lf.lfCharSet := DEFAULT_CHARSET;
  StrPCopy(lf.lfFaceName, FFontName); //'Code2000');
  hdc := GetDC(0);
  //FPlane0FontName := 'Code2000';
{$IFDEF VER340}
  if EnumFontFamiliesEx(hdc, lf, @EnumFallbackFonts, 0, 0) <> 0 then
{$ELSE}
  if EnumFontFamiliesEx(hdc, lf, @EnumFallbackFonts, 0, 0) then
{$ENDIF}
  begin
    FPlane0FontName := FFontName;
    Result := True;
  end
  else
    Result := False;
  ReleaseDC(0, hdc);
end;

procedure SetPlane0FallbackFont;
begin
  if not TestFont('Code2000') and not TestFont('Arial Unicode MS') and not TestFont('Lucida Sans Unicode') then
    FPlane0FontName := 'Arial';
end;

end.
