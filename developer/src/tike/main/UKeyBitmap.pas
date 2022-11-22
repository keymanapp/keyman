unit UKeyBitmap;

interface

uses Windows, SysUtils, Classes, Graphics, KeyNames;

procedure CreateKeyBitmap(imgKeyCap: TBitmap; PanelColor: TColor; ShiftFlags, VirtualKey: Integer; Character: WideChar; var KeyBitmap: TBitmap);//, Virtuakey:

implementation

uses
  kmxfileconsts;

procedure CreateKeyBitmap(imgKeyCap: TBitmap; PanelColor: TColor; ShiftFlags, VirtualKey: Integer; Character: WideChar; var KeyBitmap: TBitmap);//, Virtuakey:
  function CreateKey(txt: string): TBitmap;
  var
    bmp: TBitmap;
    x, y: Integer;
  begin
    bmp := TBitmap.Create;
    with bmp.Canvas do
    begin
      Font.Name := 'Tahoma';
      Font.Size := 8;
      Font.Style := [fsBold];

      x := TextWidth(txt);
      y := TextHeight(txt);

      if x + 14 > 30 then bmp.Width := x + 14 else bmp.Width := 30;
      bmp.Height := 30;

      Brush.Color := PanelColor;
      FillRect(Rect(0, 0, bmp.Width, bmp.Height));

      SetBkMode(Handle, TRANSPARENT);

      BitBlt(Handle, 0, 0, 6, 30, imgKeyCap.Canvas.Handle, 0, 0, SRCCOPY);
      StretchBlt(Handle, 6, 0, bmp.Width-12, bmp.Height, imgKeyCap.Canvas.Handle, 6, 0, imgKeyCap.Width-12, imgKeyCap.Height, SRCCOPY);
      BitBlt(Handle, bmp.Width-6, 0, 6, 30, imgKeyCap.Canvas.Handle, 24, 0, SRCCOPY);

      TextOut(7, 15 - y div 2 - 3, txt);
    end;

    Result := bmp;
  end;

  function AddKeyPicture(srcbmp: TBitmap; txt: string): TBitmap;
  var
    bmp: TBitmap;
  begin
    bmp := CreateKey(txt);
    if not Assigned(srcbmp) then
    begin
      Result := bmp;
      Exit;
    end;
    srcbmp.Width := srcbmp.Width + bmp.Width + 20;
    with srcbmp.Canvas do
    begin
      Draw(srcbmp.Width - bmp.Width, 0, bmp);
      MoveTo(srcbmp.Width - bmp.Width - 15, 15);
      LineTo(srcbmp.Width - bmp.Width - 4, 15);
      MoveTo(srcbmp.Width - bmp.Width - 10, 10);
      LineTo(srcbmp.Width - bmp.Width - 10, 21);
    end;
    bmp.Free;
    Result := srcbmp;
  end;

var
  s: WideString;
  bmp: TBitmap;
begin
  FreeAndNil(KeyBitmap);

  bmp := nil;
  if (ShiftFlags and (KMX_CTRLFLAG or KMX_LCTRLFLAG)) <> 0 then bmp := AddKeyPicture(bmp, 'Ctrl');
  if (ShiftFlags and (KMX_RCTRLFLAG)) <> 0               then bmp := AddKeyPicture(bmp, 'R Ctrl');
  if (ShiftFlags and (KMX_SHIFTFLAG)) <> 0               then bmp := AddKeyPicture(bmp, 'Shift');
  if (ShiftFlags and (KMX_ALTFLAG or KMX_LALTFLAG)) <> 0   then bmp := AddKeyPicture(bmp, 'Alt');
  if (ShiftFlags and (KMX_RALTFLAG)) <> 0                then bmp := AddKeyPicture(bmp, 'AltGr');
  if (ShiftFlags and (KMX_CAPITALFLAG)) <> 0             then bmp := AddKeyPicture(bmp, 'Caps Lock');
  if Character <> #0 then
    if Character = ' '
      then s := StringOfChar(' ', 48) // Special handling for spacebar
      else s := Character
  else
    if VirtualKey = VK_SPACE
      then s := StringOfChar(' ', 48) // Special handling for spacebar
      else s := SKeyNames[VirtualKey];
  KeyBitmap := AddKeyPicture(bmp, s);
end;

end.
