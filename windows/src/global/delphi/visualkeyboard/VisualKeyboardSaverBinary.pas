unit VisualKeyboardSaverBinary;

interface

uses
  System.Classes,
  Vcl.Graphics,
  Winapi.Windows,

  VisualKeyboard;

type
  TVisualKeyboardSaverBinary = class(TVisualKeyboardSaver)
  private
    procedure SaveHeader(Header: TVisualKeyboardHeader; Stream: TStream);
    procedure SaveKey(Key: TVisualKeyboardKey; Stream: TStream);
    procedure SaveKeys(Keys: TVisualKeyboardKeyList; Stream: TStream);
    procedure WriteBitmap(FFile: TStream; FBitmap: Vcl.Graphics.TBitmap);
    procedure WriteFont(FFile: TStream; FFont: TFont);
    procedure WriteString(FFile: TStream; str: WideString);
  public
    procedure SaveToStream(Stream: TStream); override;
  end;

implementation

procedure TVisualKeyboardSaverBinary.WriteString(FFile: TStream; str: WideString);
var
  w: Word;
begin
  w := Length(str)+1;
  FFile.Write(w, sizeof(w));
  FFile.Write(PWideChar(str)^, w*2);
end;


procedure TVisualKeyboardSaverBinary.WriteFont(FFile: TStream; FFont: TFont);
var
  n: Integer;
begin
  WriteString(FFile, FFont.Name);
  n := FFont.Size;
  FFile.Write(n, sizeof(n));
  n := Integer(FFont.Color);
  FFile.Write(n, sizeof(n));
end;


procedure TVisualKeyboardSaverBinary.WriteBitmap(FFile: TStream; FBitmap: Vcl.Graphics.TBitmap);
var
  n: DWord;
  mem: TMemoryStream;
begin
  if not Assigned(FBitmap) then
  begin
    n := 0;
    FFile.Write(n, Sizeof(n));
  end
  else
  begin
    mem := TMemoryStream.Create;
    try
      FBitmap.SaveToStream(mem);
      n := mem.Size;
      FFile.Write(n, Sizeof(n));
      FFile.CopyFrom(mem, 0);
    finally
      mem.Free;
    end;
  end;
end;

procedure TVisualKeyboardSaverBinary.SaveToStream(Stream: TStream);
begin
  SaveHeader(FKbd.Header, Stream);
  SaveKeys(FKbd.Keys, Stream);
end;

procedure TVisualKeyboardSaverBinary.SaveHeader(Header: TVisualKeyboardHeader; Stream: TStream);
var
  ch: array[0..4] of ansichar;  // I3310
  version: Integer;
begin
  ch := 'KVKF';
  Stream.Write(ch, 4);
  version := $0600;
  Stream.Write(version, 4);
  Stream.Write(Header.Flags, sizeof(Header.Flags));
  WriteString(Stream, Header.AssociatedKeyboard);
  WriteFont(Stream, Header.ANSIFont);
  WriteFont(Stream, Header.UnicodeFont);
end;

procedure TVisualKeyboardSaverBinary.SaveKeys(Keys: TVisualKeyboardKeyList; Stream: TStream);
var
  i: Integer;
begin
  Stream.Write(Keys.Count, Sizeof(Keys.Count));
  for i := 0 to Keys.Count - 1 do
    SaveKey(Keys[i], Stream);
end;

procedure TVisualKeyboardSaverBinary.SaveKey(Key: TVisualKeyboardKey; Stream: TStream);
begin
  Stream.Write(Key.Flags, sizeof(Key.Flags));
  Stream.Write(Key.Shift, sizeof(Key.Shift));
  Stream.Write(Key.VKey, sizeof(Key.VKey));
  WriteString(Stream, Key.Text);
  WriteBitmap(Stream, Key.Bitmap);
end;


end.
