(*
  Name:             MLang
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    1 Aug 2006
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Initial version
*)
unit MLang;

interface

uses
  Windows;

{ interface IMLangFontLink2 }

type
  SCRIPT_ID = type byte;
  PSCRIPT_ID = ^SCRIPT_ID;
  SCRIPT_IDS = type int64;

  TScriptFontInfo = record
    scripts: SCRIPT_IDS;
    wszFont: array[0..31] of WCHAR;
  end;
  PScriptFontInfo = ^TScriptFontInfo;

  TUnicodeRange = record
    wcFrom, wcTo: WCHAR;
  end;

  PUnicodeRange = ^TUnicodeRange;

  PHFONT = ^HFONT;

const
  CLSID_CMultiLanguage: TGUID = '{275c23e2-3747-11d0-9fea-00aa003f8646}';

type
  IMLangCodePages  = interface(IUnknown)
    ['{359F3443-BD4A-11D0-B188-00AA0038C969}']
    function GetCharCodePages(
            { [in] } chSrc: WCHAR;
            { [out] } pdwCodePages: PDWORD): HRESULT; stdcall;
    function GetStrCodePages(
            { [size_is][in] } const pszSrc: PWCHAR;
            { [in] } cchSrc: longint;
            { [in] } dwPriorityCodePages: DWORD;
            { [out] } pdwCodePages: PDWORD;
            { [out] } pcchCodePages: PInteger): HRESULT; stdcall;

    function CodePageToCodePages(
            { [in] } uCodePage: UINT;
            { [out] } pdwCodePages: PDWORD): HRESULT; stdcall;

    function CodePagesToCodePage(
            { [in] } dwCodePages: DWORD;
            { [in] } uDefaultCodePage: UINT;
            { [out] } puCodePage: PUINT): HRESULT; stdcall;
  end;

  IMLangFontLink2 = interface(IMLangCodePages)
    ['{DCCFC162-2B38-11d2-B7EC-00C04F8F5D9A}']
    function GetFontCodePages(
            { [in] } hDC: HDC;
            { [in] } hFont: HFONT;
            { [out] } pdwCodePages: PDWORD): HRESULT; stdcall;

    function ReleaseFont(
            { [in] } hFont: HFONT): HRESULT; stdcall;

    function ResetFontMapping: HRESULT; stdcall;

    function MapFont(
            { [in] } hDC: HDC;
            { [in] } dwCodePages: DWORD;
            { [in] } chSrc: WCHAR;
            { [out] } pFont: PHFONT): HRESULT; stdcall;

    function GetFontUnicodeRanges(
            { [in] } hDC: HDC;
            { [out][in] } puiRanges: PUINT;
            { [out] } pUranges: PUNICODERANGE): HRESULT; stdcall;

    function GetScriptFontInfo(
            { [in] } sid: SCRIPT_ID;
            { [in] } dwFlags: DWORD;
            { [out][in] } puiFonts: PUINT;
            { [out] } pScriptFont: PSCRIPTFONTINFO): HRESULT; stdcall;

    function CodePageToScriptID(
            { [in] } uiCodePage: UINT;
            { [out] } pSid: PSCRIPT_ID): HRESULT; stdcall;

    end;

implementation

(*
function TfrmCharacterMapNew.FLTextOut(hdc: THandle; x, y: Integer; psz: PWideChar; cch: Integer): HRESULT;
var
  pfl: IMLangFontLink2;
  hr: HRESULT;
  hfLinked, hfOrig: HFONT;
  ptOrig: TPOINT;
  clOrig, dwActualCodePages, dwFontCodePages, dwAlignOrig: DWORD;
  cchActual: LongInt;
begin

  clOrig := GetTextColor(hdc);

  hr := CoCreateInstance(CLSID_CMultiLanguage, nil,
                      CLSCTX_ALL, IMLangFontLink2, pfl);
  if Succeeded(hr) then
  begin
    hfOrig := GetCurrentObject(hdc, OBJ_FONT);
    dwAlignOrig := GetTextAlign(hdc);
    if (dwAlignOrig and TA_UPDATECP) = 0 then
      SetTextAlign(hdc, dwAlignOrig or TA_UPDATECP);

    MoveToEx(hdc, x, y, @ptOrig);
    dwFontCodePages := 0;
    hr := pfl.GetFontCodePages(hdc, hfOrig, @dwFontCodePages);
    if Succeeded(hr) then
    begin
      while cch > 0 do
      begin
        hr := pfl.GetStrCodePages(psz, cch, dwFontCodePages, @dwActualCodePages, @cchActual);
        if Failed(hr) then
          Break;

        if (dwActualCodePages and dwFontCodePages) <> 0 then
        begin
          SetTextColor(hdc, ColorToRGB(clOrig));
          TextOutW(hdc, 0, 0, psz, cchActual)
        end
        else
        begin
          SetTextColor(hdc, ColorToRGB(clGreen));
          hr := pfl.MapFont(hdc, dwActualCodePages, psz^, @hfLinked);
          if Failed(hr) then
            Break;
          SelectObject(hdc, hfLinked);
          TextOutW(hdc, 0, 0, psz, cchActual);
          SelectObject(hdc, hfOrig);
          pfl.ReleaseFont(hfLinked);
        end;
        Inc(psz, cchActual);
        Dec(cch, cchActual);
      end;
      if Failed(hr) then
      begin
        //  We started outputting characters so we have to finish.
        //  Do the rest without font linking since we have no choice.
        SetTextColor(hdc, ColorToRGB(clRed));
        TextOutW(hdc, 0, 0, psz, cch);
        hr := S_FALSE;
      end;
    end;

    pfl := nil;

    if (dwAlignOrig and TA_UPDATECP) = 0 then
    begin
      SetTextAlign(hdc, dwAlignOrig);
      MoveToEx(hdc, ptOrig.x, ptOrig.y, nil);
    end;
  end;

  SetTextColor(hdc, clOrig);
  Result := hr;
end;
*)

        (*FillChar(mat2, Sizeof(mat2), 0);
        mat2.eM11.value := 1;
        mat2.eM22.value := 1;
        FillChar(gm, Sizeof(gm), 0);
        sz := GetGlyphOutlineW(grid.Canvas.Handle, gi^, GGO_GRAY8_BITMAP or GGO_GLYPH_INDEX, gm, 0, nil, mat2);
        if sz <> GDI_ERROR then
        begin
          bi := AllocMem(sizeof(TBitmapInfoHeader) + 64 * sizeof(RGBQUAD));
          bi.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
          bi.bmiHeader.biWidth := gm.gmBlackBoxX;
          bi.bmiHeader.biHeight := -gm.gmBlackBoxY;
          bi.bmiHeader.biPlanes := 1;
          bi.bmiHeader.biBitCount := 8;
          bi.bmiHeader.biCompression := BI_RGB;
          bi.bmiHeader.biSizeImage := 0;
          bi.bmiHeader.biXPelsPerMeter := 72;
          bi.bmiHeader.biYPelsPerMeter := 72;
          bi.bmiHeader.biClrUsed := 65;
          bi.bmiHeader.biClrImportant := 0;
          for I := 0 to 64 do
          begin
            if I = 64 then n := 255 else n := I*4;
            {$R-}
            bi.bmiColors[64-I].rgbBlue := n;
            bi.bmiColors[64-I].rgbGreen := n;
            bi.bmiColors[64-I].rgbRed := n;
            bi.bmiColors[64-I].rgbReserved := 0;
            {$R+}
          end;
          buf := AllocMem(sz);
          GetGlyphOutlineW(grid.Canvas.Handle, gi^, GGO_GRAY8_BITMAP or GGO_GLYPH_INDEX, gm, sz, buf, mat2);
          SetDiBitsToDevice(grid.Canvas.Handle, (Rect.Right+Rect.Left - gm.gmBlackBoxX) div 2, (Rect.Bottom-12+Rect.Top - gm.gmBlackBoxY) div 2, gm.gmBlackBoxX, gm.gmBlackBoxY, 0, 0, 0, gm.gmBlackBoxY, buf, bi^,
            DIB_RGB_COLORS);
          FreeMem(buf);
          FreeMem(bi);
        end;*)

end.
