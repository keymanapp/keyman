(*
  Name:             UFixFontDialogBold
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Sep 2006

  Modified Date:    14 Sep 2006
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          14 Sep 2006 - mcdurdin - Initial version
*)
unit UFixFontDialogBold;

interface

uses
  System.UITypes,
  Windows,
  Graphics;

function FixFontDialogBold(Font: TFont): TFont;

implementation

function FontProc(lplfe: PEnumLogFontExW; lpntme: PNewTextMetricExW; FontType: DWORD; lParam: LPARAM): Integer; stdcall;
begin
  if lpntme.ntmTm.tmWeight >= FW_BOLD then
  begin
    Result := 0;
    Exit;
  end;
  Result := 1;
end;

function FixFontDialogBold(Font: TFont): TFont;
var
  Bitmap: TBitmap;
  LogFont: TLogFontW;
begin
  Result := Font;
  if fsBold in Font.Style then
  begin
    if GetObjectW(Font.Handle, SizeOf(LogFont), @LogFont) <> 0 then
    begin
      logFont.lfPitchAndFamily := 0;
      Bitmap := TBitmap.Create;
      try
        Bitmap.Canvas.Font := Font;
        if Integer(EnumFontFamiliesExW(Bitmap.Canvas.Handle, logFont, @FontProc, 0, 0)) = 1 then
          Result.Style := Result.Style - [fsBold];
      finally
        Bitmap.Free;
      end;
    end;
  end;
end;

end.
