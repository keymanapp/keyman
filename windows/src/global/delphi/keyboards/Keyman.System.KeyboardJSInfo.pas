unit Keyman.System.KeyboardJSInfo;

interface

uses
  System.Classes,
  System.SysUtils;

type
  TKeyboardJSInfo = class
  private
    FVersion: string;
    FName: string;
    FMnemonic: Boolean;
    FRTL: Boolean;

    procedure Parse(data: string);
  public
    constructor Create(AFilename: string); overload;
    constructor Create(AStream: TStream); overload;

    property Name: string read FName;
    property Version: string read FVersion;
    property RTL: Boolean read FRTL;
    property Mnemonic: Boolean read FMnemonic;
  end;

implementation

uses
  System.RegularExpressions;

{ TKeyboardJSInfo }

constructor TKeyboardJSInfo.Create(AFilename: string);
var
  s: TStringStream;
begin
  s := TStringStream.Create('', TEncoding.UTF8);
  try
    s.LoadFromFile(AFilename);
    Create(s);
  finally
    s.Free;
  end;
end;

constructor TKeyboardJSInfo.Create(AStream: TStream);
var
  s: TStringStream;
begin
  if AStream is TStringStream then
  begin
    Parse((AStream as TStringStream).DataString);
  end
  else
  begin
    s := TStringStream.Create('', TEncoding.UTF8);
    try
      s.CopyFrom(AStream, 0);
      Parse(s.DataString);
    finally
      s.Free;
    end;
  end;
end;

(**
  This function parses the JavaScript to do a best-effort retrieval of
  keyboard data. It assumes a format similar to what the compiler produces.
  It may be possible for it to miss something if the code is hand written and
  diverges enough from the compiler code (e.g. if there is a comment between
  a member variable and its value or something crazy like that).
*)
procedure TKeyboardJSInfo.Parse(data: string);
var
  m: TMatch;
begin
  // Retrieve keyboard name from .js
  m := TRegEx.Match(data, 'this.KN\s*=\s*([''"])(.*?)\1');
  if m.Success
    then FName := m.Groups[2].Value
    else FName := '';

  // Extract keyboard version
  m := TRegEx.Match(data, 'this.KBVER\s*=\s*([''"])(.*?)\1');
  if m.Success
    then FVersion := m.Groups[2].Value
    else FVersion := '';

  // Extract RTL status
  m := TRegEx.Match(data, 'this.KRTL\s*=\s*(.*?)\s*;');
  if m.Success
    then FRTL := StrToBoolDef(m.Groups[1].Value, False)
    else FRTL := False;

  // Extract mnemonic layout status
  m := TRegEx.Match(data, 'this.KM\s*=\s*(.*?)\s*;');
  if m.Success
    then FMnemonic := StrToBoolDef(m.Groups[1].Value, False)
    else FMnemonic := False;
end;

end.
