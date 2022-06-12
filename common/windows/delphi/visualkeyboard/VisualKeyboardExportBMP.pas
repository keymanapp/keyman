(*
  Name:             VisualKeyboardExportBMP
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Aug 2006

  Modified Date:    18 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2006 - mcdurdin - Initial refactor for new visual keyboard
                    22 Jan 2007 - mcdurdin - Rework to support pixel width and new osk
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    16 Jan 2009 - mcdurdin - Widestring filenames
                    04 Jun 2009 - mcdurdin - I2004 - Export OSK to graphic (BMP,PNG,etc) misses some shift states
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit VisualKeyboardExportBMP;  // I3306

interface

uses Graphics, Controls, VisualKeyboard, SysUtils, VisualKeyboardParameters;

type
  TVisualKeyboardExportBMP = class(TVisualKeyboardExport)
  private
    FPixelWidth: Integer;
  protected
    FMulti, FANSI, FUnicode: Boolean;
    function GenerateBMP(var bmp: TBitmap; FUnicode: Boolean; FShift: Integer): Boolean;
    procedure SaveToFile(bmp: TBitmap; FileName: WideString); virtual;
  public
    procedure ExportToFile(FileName: WideString); override;
    constructor Create(AKbd: TVisualKeyboard; AMulti, AANSI, AUnicode: Boolean; APixelWidth: Integer); reintroduce;
  end;

implementation

uses Classes, OnScreenKeyboard, KeyBtn, Types;

{ TVisualKeyboardExportBMP }

constructor TVisualKeyboardExportBMP.Create(AKbd: TVisualKeyboard; AMulti,
  AANSI, AUnicode: Boolean; APixelWidth: Integer);
begin
  inherited Create(AKbd);
  FMulti := AMulti;
  FANSI := AANSI;
  FUnicode := AUnicode;
  FPixelWidth := APixelWidth;
end;

procedure TVisualKeyboardExportBMP.ExportToFile(FileName: WideString);
var
  bigbmp, bmp: TBitmap;
  i, FShift: Integer;
  FShiftText: string;
begin
  bigbmp := TBitmap.Create;
  bigbmp.Height := 0;
  bigbmp.Width := 0;
  bmp := TBitmap.Create;
  try
    for i := 1 to 2 do
      if ((i = 1) and FUnicode) or ((i = 2) and FANSI) then
        for FShift := 0 to High(VKLegalShiftStates) do
          if GenerateBMP(bmp, i = 1, VKLegalShiftStates[FShift].Shift) then
          begin
            if not FMulti then
            begin
              if i = 1 then FShiftText := 'Unicode: ' else FShiftText := 'Code Page: ';
              FShiftText := FShiftText + VKLegalShiftStates[FShift].Desc;
              bigbmp.Height := bigbmp.Height + bmp.Height + bigbmp.Canvas.TextHeight('A');
              if bigbmp.Width < bmp.Width then bigbmp.Width := bmp.Width;
              bigbmp.Canvas.TextOut(0, bigbmp.Height - bmp.Height - bigbmp.Canvas.TextHeight('A'), FShiftText);
              bigbmp.Canvas.Draw(0, bigbmp.Height - bmp.Height, bmp);
            end
            else
            begin
              if i = 1 then FShiftText := 'U_' else FShiftText := 'A_';
              FShiftText := FShiftText + VKLegalShiftStates[FShift].Name;
              SaveToFile(bmp, ChangeFileExt(FileName,'')+FShiftText+ExtractFileExt(FileName));
            end;
          end;
    if bigbmp.Height > 0 then
      SaveToFile(bigbmp, FileName);
  finally
    bmp.Free;
    bigbmp.Free;
  end;
end;

procedure TVisualKeyboardExportBMP.SaveToFile(bmp: TBitmap; FileName: WideString);
var
  stream: TStream;
begin
  stream := TFileStream.Create(FileName, fmCreate);
  try
    bmp.SaveToStream(stream);
  finally
    stream.Free;
  end;
end;

function TVisualKeyboardExportBMP.GenerateBMP(var bmp: TBitmap; FUnicode: Boolean; FShift: Integer): Boolean;
var
  hwnd: THandle;
  osk: TOnScreenKeyboard;
  i: Integer;
  k: TOnScreenKeyboardKey;
  r: TRect;
begin
  Result := False;
  hwnd := AllocateHWND(nil);
  osk := TOnScreenKeyboard.Create(nil);
  try
    osk.BeginUpdate;
    try
      osk.ParentWindow := hwnd;

      osk.Transparent := False;
      osk.DisplayUnderlyingChar := kvkhDisplayUnderlying in FKbd.Header.Flags;
      osk.Display102Key := kvkh102 in FKbd.Header.Flags;
      osk.Width := FPixelWidth;
      r := Rect(0, 0, osk.Width, 0);
      osk.AdjustBoundsRect(r, True);
      osk.Height := r.Bottom;
      osk.LRShift := kvkhAltGr in FKbd.Header.Flags;

  {    if Assigned(FKeyBitmap.Bitmap) then
      begin
        osk. KeyBitmap := FKeyBitmap.Bitmap;
        //kbd.Parameters := FKeyBitmap.Params;
      end;
  }
      if FUnicode
        then osk.DataFont := FKbd.Header.UnicodeFont
        else osk.DataFont := FKbd.Header.ANSIFont;

      osk.Keys.ClearValues;

      for i := 0 to FKbd.Keys.Count - 1 do
      begin
        if (FKbd.Keys[i].Shift = FShift) and
          (((kvkkUnicode in FKbd.Keys[i].Flags) and (FUnicode)) or
          (not (kvkkUnicode in FKbd.Keys[i].Flags) and (not FUnicode))) then
        begin
          k := osk.Keys.ItemsByVK[FKbd.Keys[i].VKey];
          if Assigned(k) and ((FKbd.Keys[i].Text <> '') or Assigned(FKbd.Keys[i].Bitmap)) then
          begin
            k.KeyValue := FKbd.Keys[i].Text;
            k.KeyGlyph := FKbd.Keys[i].Bitmap;
            Result := True;
          end;
        end;
      end;

      if not Result then Exit;

      bmp.SetSize(osk.Width, osk.Height);
      bmp.Canvas.Brush.Color := clWhite;
      bmp.Canvas.FillRect(Rect(0, 0, osk.Width, osk.Height));
    finally
      osk.EndUpdate;
    end;

    osk.PaintTo(bmp.Canvas.Handle, 0, 0);
    osk.ParentWindow := 0;
  finally
    osk.Free;
    DeallocateHWND(hwnd);
  end;
end;

end.
