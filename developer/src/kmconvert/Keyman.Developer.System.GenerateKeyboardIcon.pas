unit Keyman.Developer.System.GenerateKeyboardIcon;

interface

type
  TKeyboardIconGenerator = class sealed
    class function GenerateIcon(const BCP47Tags, IconFilename: string; size: Integer): Boolean;
  end;

implementation

uses
  System.Math,
  System.SysUtils,
  System.Types,
  Vcl.Graphics,
  Winapi.Windows,

  Keyman.System.Util.RenderLanguageIcon;

class function TKeyboardIconGenerator.GenerateIcon(
  const BCP47Tags, IconFilename: string; size: Integer): Boolean;
var
  FTag: string;
  n: Integer;
  ico: TIcon;
  b: array[0..1] of Vcl.Graphics.TBitmap;
  iconInfo: TIconInfo;
begin
  // We need to use the BCP47 tag that we have received and render that, for now

  n := Min(Pos(' ', BCP47Tags), Pos('-', BCP47Tags));
  if n > 0
    then FTag := Copy(BCP47Tags, 1, n-1)
    else FTag := BCP47Tags;

  b[0] := Vcl.Graphics.TBitmap.Create;
  b[1] := Vcl.Graphics.TBitmap.Create;
  try
    b[0].SetSize(16, 16);
    b[0].PixelFormat := pf32bit;

    b[1].SetSize(16, 16);
    b[1].PixelFormat := pf1bit;
    b[1].Canvas.Brush.Color := clBlack;
    b[1].Canvas.FillRect(Rect(0,0,16,16));

    DrawLanguageIcon(b[0].Canvas, 0, 0, UpperCase(FTag), size);
    ico := TIcon.Create;
    try
      FillChar(iconInfo, sizeof(iconInfo), 0);
      iconInfo.fIcon := True;
      iconInfo.hbmMask := b[1].Handle;
      iconInfo.hbmColor := b[0].Handle;
      ico.Handle := CreateIconIndirect(iconInfo);
      ico.SaveToFile(IconFilename);
    finally
      ico.Free;
    end;
    Result := True;
  finally
    b[0].Free;
    b[1].Free;
  end;
end;

end.
