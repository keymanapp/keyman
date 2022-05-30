unit VisualKeyboardLoaderBinary;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Winapi.Windows,

  VisualKeyboard;

//
// This originated with the TVisualKeyboardImportXML code but has now diverged for
// UI-clean single-file round-trippable consistency.
//

type
  EVisualKeyboardLoaderBinary = class(EVisualKeyboardLoader);

  TVisualKeyboardLoaderBinary = class(TVisualKeyboardLoader)
  private
    procedure LoadKey(Key: TVisualKeyboardKey; Stream: TStream);
    procedure LoadHeader(Header: TVisualKeyboardHeader; Stream: TStream);
    function ReadBitmap(Stream: TStream): Vcl.Graphics.TBitmap;
    procedure ReadFont(Stream: TStream; FFont: TFont);
    procedure LoadKeys(Keys: TVisualKeyboardKeyList; Stream: TStream);
    function ReadString(Stream: TStream): string;
  public
    procedure LoadFromStream(Stream: TStream); override;
  end;

implementation

{ TVisualKeyboardLoaderBinary }

function TVisualKeyboardLoaderBinary.ReadString(Stream: TStream): string;
var
  w: Word;
  str: string;
begin
  Stream.Read(w, sizeof(w));
  SetLength(str, w);
  Stream.Read(PChar(str)^, w*2);
  // The string read is a C-style null terminated string. We need
  // to remove the null character because Pascal strings don't
  // use null termination in the same way. Best way to do this is
  // to cast it as a C-style string, which reliably terminates at
  // first null byte.
  Result := PWideChar(str);
end;

procedure TVisualKeyboardLoaderBinary.ReadFont(Stream: TStream; FFont: TFont);
var
  n: Integer;
begin
  FFont.Name := ReadString(Stream);
  Stream.Read(n, sizeof(n));
  FFont.Size := n;
  Stream.Read(n, sizeof(n));
  FFont.Color := TColor(n);
end;

function TVisualKeyboardLoaderBinary.ReadBitmap(Stream: TStream): Vcl.Graphics.TBitmap;
var
  n: DWord;
  mem: TMemoryStream;
begin
  Stream.Read(n, Sizeof(n));
  if n > 0 then
  begin
    mem := TMemoryStream.Create;
    try
      mem.CopyFrom(Stream, n);
      mem.Position := 0;
      Result := Vcl.Graphics.TBitmap.Create;
      Result.LoadFromStream(mem);
    finally
      mem.Free;
    end;
  end
  else
    Result := nil;
end;

procedure TVisualKeyboardLoaderBinary.LoadHeader(Header: TVisualKeyboardHeader; Stream: TStream);
var
  ch: array[0..4] of ansichar;  // I3310
  version: Integer;
  f: TVisualKeyboardHeaderFlags;
begin
  Stream.Read(ch, 4); ch[4] := #0;
  if ch <> 'KVKF' then raise EVisualKeyboardLoader.Create('Not a valid Keyman Visual Keyboard File');
  Stream.Read(version, 4);
  if version <> $0600 then raise EVisualKeyboardLoader.Create('Version number not recognised');

  Stream.Read(f, sizeof(f)); Header.Flags := f;
  Header.AssociatedKeyboard := ReadString(Stream);
  ReadFont(Stream, Header.ANSIFont);
  ReadFont(Stream, Header.UnicodeFont);
end;

procedure TVisualKeyboardLoaderBinary.LoadKeys(Keys: TVisualKeyboardKeyList; Stream: TStream);
var
  i, n: Integer;
  item: TVisualKeyboardKey;
begin
  Stream.Read(n, Sizeof(n));
  for i := 0 to n - 1 do
  begin
    item := TVisualKeyboardKey.Create;
    LoadKey(item, Stream);
    Keys.Add(item);
  end;
end;

procedure TVisualKeyboardLoaderBinary.LoadKey(Key: TVisualKeyboardKey; Stream: TStream);
var
  Flags: TVisualKeyboardKeyFlags;
  Shift, VKey: Word;
begin
  Stream.Read(Flags, sizeof(Flags)); Key.Flags := Flags;
  Stream.Read(Shift, sizeof(Shift)); Key.Shift := Shift;
  Stream.Read(VKey, sizeof(VKey)); Key.VKey := VKey;
  Key.Text := ReadString(Stream);
  Key.Bitmap := ReadBitmap(Stream);
end;

procedure TVisualKeyboardLoaderBinary.LoadFromStream(Stream: TStream);
begin
  LoadHeader(FKbd.Header, Stream);
  LoadKeys(FKbd.Keys, Stream);
end;

end.
