unit SFX;

interface

uses
  Winapi.Windows;

function ProcessArchive(const ExtPath: string): Boolean;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Zip;

var
  StartOfFile: Int64 = -1;

function FindFirstHeader(s: TStream): BOOLEAN;
var
  DOSHeader: TImageDosHeader;
  PEHeader: TImageFileHeader;
  OptHeader: TImageOptionalHeader;
  magic: DWord;
  n: Integer;
  HEADER_SIGNATURE: DWord;
begin
  // We store the local header signature in the file one greater than
  // the real value, so we don't falsely detect a zip file entry inside
  // the SFX code of the .EXE file

  Result := False;

  HEADER_SIGNATURE:=$04034B51; // intentionally 1 higher than the real sig
  dec(HEADER_SIGNATURE);       // fix the sig in RAM only

  s.Seek(0, TSeekOrigin.soBeginning);
  s.Read(DOSHeader, sizeof(DOSHeader));
  s.Seek(DOSHeader._lfanew, TSeekOrigin.soBeginning);

  s.Read(magic, 4);
  if magic <> $4550 then Exit;

  s.Read(PEHeader, sizeof(PEHeader));
  s.Read(OptHeader, sizeof(OptHeader));

  n := OptHeader.FileAlignment;

  StartOfFile := -1;

  // Scan file by alignment chunks for PK header
  while n < s.Size do
  begin
    s.Seek(n, TSeekOrigin.soBeginning);
    s.Read(magic, 4);
    if magic = HEADER_SIGNATURE then
    begin
      StartOfFile := n;
      Exit(True);
    end;
    Inc(n, OptHeader.FileAlignment);
  end;

  Result := StartOfFile >= 0;
end;

//
// This is not good for very large archives. We are assuming the archive
// is small enough to load into memory which should be fine on all modern PCs
// for Keyman Desktop and Keyman Developer. If files get bigger than a couple
// of hundred megs, then we should consider saving to temp disk file instead.
//
// All this is due to RSP-17889. If this gets fixed, we can eliminate the
// copy to TMemoryStream - and probably even the FindFirstHeader call
//
// 2020-07-07: RSP-17889 checked against Delphi 10.4 Sydney and not yet fixed.
// https://quality.embarcadero.com/browse/RSP-17889

function ProcessArchive(const ExtPath: string): Boolean;
var
  fs: TFileStream;
  ms: TMemoryStream;
begin
  fs := TFileStream.Create(ParamStr(0), fmOpenRead or fmShareDenyWrite);
  ms := TMemoryStream.Create;
  try
    if not FindFirstHeader(fs) then
      Exit(False);

    fs.Seek(StartOfFile, TSeekOrigin.soBeginning);

    ms.CopyFrom(fs, fs.Size - StartOfFile);
    ms.Position := 0;

    with TZipFile.Create do
    try
      Open(ms, zmRead);
      ExtractAll(ExtPath);
    finally
      Free;
    end;
  finally
    fs.Free;
    ms.Free;
  end;
  Result := True;
end;

end.
