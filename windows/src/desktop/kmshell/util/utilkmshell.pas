(*
  Name:             utilkmshell
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jun 2006

  Modified Date:    1 May 2014
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Remove old util functions
                    30 May 2007 - mcdurdin - Refactor OS checks for newer versions
                    05 Nov 2007 - mcdurdin - I1109 - Elevated call and wait; for installing keyboard elevated
                    16 Jan 2009 - mcdurdin - I1730 - CanElevate refactor into utiluac
                    16 Jan 2009 - mcdurdin - I1730 - Add FWait param to WaitForElevatedConfiguration
                    09 Mar 2009 - mcdurdin - I1889 - Keyman Configuration takes high CPU when elevating
                    01 Jun 2009 - mcdurdin - I2001 - use current user not local machine when testing root keyboard path
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    04 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    26 Jun 2012 - mcdurdin - I3379 - KM9 - Remove old Winapi references now in Delphi libraries
                    16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard
                    01 May 2014 - mcdurdin - I4181 - V9.0 - Stop using DeleteFileAlways, MOVEFILE_DELAY_UNTIL_REBOOT
*)
unit utilkmshell;  // I3306   // I4181

interface

uses
  System.UITypes, System.IOUtils, System.Types,
  Dialogs, Windows, ComObj, shlobj, controls, sysutils, classes;

const
  CRLF = #13#10;

type
  TString = class
    FString: string;
    constructor Create(const AString: string); overload;
    constructor Create; overload;
  end;

{ TWideStringStream }

type
  TWideStringStream = class(TStream)
  private
    PDataString: PChar;
    FDataString: WideString;
    FPosition: Integer;
  protected
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(const AString: WideString);
    function Read(var Buffer; Count: Longint): Longint; override;
    function ReadString(Count: Longint): WideString;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure WriteString(const AString: WideString);
    property DataString: WideString read FDataString;
  end deprecated;

procedure WinMsg(const msg: string);
function IsKeyboardPackage(const FileName: string): Boolean;
function IsKeyboardFile(const FileName: string): Boolean;
function IsKeyboardSourceFile(const FileName: string): Boolean;
function IsKeyboardPackageSourceFile(const FileName: string): Boolean;
function IsKeymanAddinFile(const FileName: string): Boolean;
function IsKeymanVisualKeyboard(const FileName: string): Boolean;
function MAKELCID(wLanguageID, wSortID: WORD): DWORD;


function wchr(n: Integer): WideChar;
function StrToken(var s: string; const tokens: string): string;
function WStrToken(var s: WideString; const tokens: WideString): WideString;
function CommaToken(var s: string): string;

function GetTokenFromCaret(line: string; var selx, sellen: Integer): string;

function ExtNumToInt(s: string): Integer;
function IntToExtNum(n, base: Integer): string;
function ExtStringToString(s: WideString; out FError: Boolean): WideString;

function TrimQuotes(s: string): string;
function FormatUnicode(s: WideString): string;
function FormatANSI(s: string): string;
function AppendSlash(s: string): string;

procedure SplitString(const instr: string; var outstr1, outstr2: string; const split: string);
function ValidDirectory(const dir: string): string;

function GetLongFile(APath:String):String;
procedure GetFileNamesInDirectory(const directoryPath: string; var fileNames: TStringDynArray);

function TSFInstalled: Boolean;

function GetLongFileName(const fname: string): string;

function WaitForElevatedConfiguration(WindowHandle: THandle; const Parameters: WideString; FWait: Boolean = True): Cardinal;
function RunConfiguration(WindowHandle: THandle; const Parameters: WideString): Boolean;
function DefaultServersXMLTags: string;
function DefaultVersionXMLTags: string;

implementation

uses
  ShellApi,
  KLog,
  Forms,
  ActiveX,
  getosversion,
  ErrorControlledRegistry,
  KeymanVersion,
  utilexecute,
  Upload_Settings,
  utilxml;

function IsKeyboardPackage(const FileName: string): Boolean;
begin
    if Length(FileName) < 5 then Result := False
    else Result := LowerCase(Copy(FileName, Length(FileName)-3, 4)) = '.kmp';
end;

function IsKeyboardFile(const FileName: string): Boolean;
begin
    if Length(FileName) < 5 then Result := False
    else Result := LowerCase(Copy(FileName, Length(FileName)-3, 4)) = '.kmx';
end;

function IsKeymanVisualKeyboard(const FileName: string): Boolean;
begin
    if Length(FileName) < 5 then Result := False
    else Result := LowerCase(Copy(FileName, Length(FileName)-3, 4)) = '.kvk';
end;

function IsKeymanAddinFile(const FileName: string): Boolean;
begin
    if Length(FileName) < 5 then Result := False
    else Result := LowerCase(Copy(FileName, Length(FileName)-3, 4)) = '.kma';
end;

function IsKeyboardSourceFile(const FileName: string): Boolean;
begin
    if Length(FileName) < 5 then Result := False
    else Result := LowerCase(ExtractFileExt(FileName)) = '.kmn';
end;

function IsKeyboardPackageSourceFile(const FileName: string): Boolean;
begin
    if Length(FileName) < 5 then Result := False
    else Result := LowerCase(ExtractFileExt(FileName)) = '.kps';
end;

procedure WinMsg(const msg: string);
begin
    Windows.MessageBox(0, PChar(msg), nil, MB_OK or MB_ICONEXCLAMATION);
end;

function MAKELCID(wLanguageID, wSortID: WORD): DWORD;
begin
    Result := (DWORD(wSortID) shl 16) or DWORD(wLanguageID);
end;

function ExtNumToInt(s: string): Integer;
begin
  Result := 0;
  s := LowerCase(Trim(s));
  if (s <> '') then
  begin
    if s[1] = 'x' then
      Result := StrToIntDef('$' + Copy(s,2,32), 0)
    else if s[1] = 'd' then
      Result := StrToIntDef(Copy(s,2,32), 0)
    else if Copy(s,1,2) = 'u+' then
    begin
      if Length(s) < 6 then Exit;
      Result := StrToIntDef('$' + Copy(s,3,32), 0);
    end
    else if CharInSet(s[1], ['0'..'9']) then  // I3310
      Result := StrToIntDef(s, 0);
  end;
end;

function IntToExtNum(n, base: Integer): string;
begin
  if base = 16 then
    Result := 'x' + IntToHex(n, 1)
  else
    Result := 'd' + IntToStr(n);
end;

function TrimQuotes(s: string): string;
begin
  s := Trim(s);

  if Length(s) < 2 then
  begin
    Result := s;
    Exit;
  end;

  if CharInSet(s[1], ['"', '''']) and (s[Length(s)] = s[1]) then  // I3310
  begin
    Delete(s, 1, 1);
    Delete(s, Length(s), 1);
  end;

  Result := s;
end;

function FormatANSI(s: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
    Result := Result + Format('d%d ', [Ord(s[i])]);
  Result := Trim(Result);
end;

function FormatUnicode(s: WideString): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
    Result := Result + Format('U+%4.4X ', [Ord(s[i])]);
  Result := Trim(Result);
end;

function AppendSlash(s: string): string;
begin
  if s = '' then Result := ''
  else if s[Length(s)] <> '\' then Result := s + '\'
  else Result := s;
end;

function isspace(s: Char): Boolean;
begin
  Result := CharInSet(s, [' ', #9, #13, #10]);  // I3310
end;

function GetTokenFromCaret(line: string; var selx, sellen: Integer): string;
begin
  Result := '';
  if selx > Length(line)+1 then Exit;
  Dec(selx);
  while selx >= 1 do
  begin
    if isspace(line[selx]) then Break;
    Dec(selx);
  end;
  if selx > 0 then Delete(line, 1, selx);
  sellen := 0;
  while (sellen < Length(line)) and not isspace(line[sellen+1]) do
    Inc(sellen);
  Delete(line, sellen+1, Length(line));
  Result := line;
end;

function StrToken(var s: string; const tokens: string): string;
var
  n: Integer;
begin
  Result := '';

  if s = '' then Exit;

  while (s <> '') and (Pos(s[1], tokens) > 0) do
  begin
    Delete(s, 1, 1);
  end;

  if s = '' then Exit;

  n := 1;
  while (n <= Length(s)) and (Pos(s[n], tokens) = 0) do
  begin
    Inc(n);
  end;

  Result := Copy(s, 1, n-1);
  Delete(s, 1, n);
end;

function WStrToken(var s: WideString; const tokens: WideString): WideString;
var
  n: Integer;
begin
  Result := '';

  if s = '' then Exit;

  while (s <> '') and (Pos(s[1], tokens) > 0) do
  begin
    Delete(s, 1, 1);
  end;

  if s = '' then Exit;

  n := 1;
  while (n <= Length(s)) and (Pos(s[n], tokens) = 0) do
  begin
    Inc(n);
  end;

  Result := Copy(s, 1, n-1);
  s := Copy(s, n+1, Length(s));
end;

function wchr(n: Integer): WideChar;
begin
  Result := WideChar(n);
end;


{ TWideStringStream }

constructor TWideStringStream.Create(const AString: WideString);
begin
  inherited Create;
  FDataString := AString;
end;

function TWideStringStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := Length(FDataString)*2 - FPosition;
  if Result > Count then Result := Count;
  PDataString := @FDataString[1]; Inc(PDataString, FPosition);
  Move(PDataString^, Buffer, Result);
  Inc(FPosition, Result);
end;

function TWideStringStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := Count;
  SetLength(FDataString, (FPosition + Result) div 2);
  PDataString := @FDataString[1]; Inc(PDataString, FPosition);
  Move(Buffer, PDataString^, Result);
  Inc(FPosition, Result);
end;

function TWideStringStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: FPosition := FPosition + Offset;
    soFromEnd: FPosition := Length(FDataString)*2 - Offset;
  end;
  if FPosition > Length(FDataString)*2 then
    FPosition := Length(FDataString)*2
  else if FPosition < 0 then FPosition := 0;
  Result := FPosition;
end;

function TWideStringStream.ReadString(Count: Longint): WideString;
var
  Len: Integer;
begin
  Len := Length(FDataString)*2 - FPosition;
  if Len > Count then Len := Count;
  PDataString := @FDataString[1]; Inc(PDataString, FPosition);
  SetString(Result, PDataString, Len);
  Inc(FPosition, Len);
end;

procedure TWideStringStream.WriteString(const AString: WideString);
begin
  Write(PChar(PWideChar(AString))^, Length(AString)*2);
end;

procedure TWideStringStream.SetSize(NewSize: Longint);
begin
  SetLength(FDataString, NewSize div 2);
  if FPosition > NewSize then FPosition := NewSize;
end;

procedure SplitString(const instr: string; var outstr1, outstr2: string; const split: string);
var
  n: Integer;
begin
  n := Pos(split, instr);
  if n > 0 then
  begin
    outstr1 := Copy(instr,1,n-1);
    outstr2 := Copy(instr,n+1,Length(instr));
  end
  else
  begin
    outstr1 := instr;
    outstr2 := instr;
  end;
end;

function ValidDirectory(const dir: string): string;
begin
  Result := '';
  if Length(dir) = 0 then Exit;
  //Result := ExpandFileName(dir);
  if (dir[Length(dir)] = '\') then Result := Copy(dir,1,Length(dir)-1)
  else Result := dir;
  //if ExpandFileName(Result) <> Result then Result := '';
end;

function ExtStringToString(s: WideString; out FError: Boolean): WideString;
var
  t: WideString;
  opch: WideString;
  n: Integer;
begin
  Result := '';
  t := '';
  FError := True;
  while s <> '' do
  begin
    case s[1] of
      ' ', #9, #13, #10: Delete(s,1,1);
      'U', 'u':
        begin
          if Length(s) < 2 then Exit;
          if s[2] <> '+' then Exit;
          Delete(s,1,2);
          opch := '$' + WStrToken(s, ' '#9#13#10);
          if not (Length(opch) in [5, 6, 7]) then Exit;
          n := StrToIntDef(opch, 0);
          if (n < 32) or (n > $10FFFF) then Exit;
          t := t + WideChar(n);
        end;
      'd':
        begin
          Delete(s,1,1);
          n := StrToIntDef(WStrToken(s, ' '#9#13#10), 0);
          if (n < 32) or (n > $10FFFF) then Exit;
          t := t + WideChar(n);
        end;
      'x':
        begin
          Delete(s,1,1);
          n := StrToIntDef('$' + WStrToken(s, ' '#9#13#10), 0);
          if (n < 32) or (n > $10FFFF) then Exit;
          t := t + WideChar(n);
        end;
      '0'..'7':
        begin
          n := StrToIntDef(WStrToken(s, ' '#9#13#10), 0);
          if (n < 40) or (n > 4177777) then Exit;
          t := t + WideChar(n);
        end;
      '''', '"':
        begin
          opch := s[1];
          Delete(s,1,1);
          while (s <> '') and (s[1] <> opch) do
          begin
            t := t + s[1]; Delete(s, 1, 1);
          end;
          if s = '' then Exit;
          Delete(s, 1, 1);
        end;
      else
        Exit;
    end;
  end;

  FError := False;
  Result := t;
end;

function CommaToken(var s: string): string;
var
  n: Integer;
begin
  while (s <> '') and CharInSet(s[1], [' ', #9, #13, #10]) do Delete(s,1,1);  // I3310

  if s = '' then
  begin
    Result := '';
    Exit;
  end;

  if s[1] = '"' then
  begin
    Delete(s,1,1);
    n := Pos('"', s);
    if n = 0 then raise Exception.Create('CommaToken: Unmatched opening quote in input text');
    Result := Trim(Copy(s, 1, n-1));
    Delete(s, 1, n);
    if s <> '' then
      if s[1] <> ','
        then raise Exception.Create('CommaToken: Expected but did not find comma or EOL after closing quote')
        else Delete(s,1,1);
  end
  else
  begin
    n := Pos(',', s);
    if n = 0 then n := Length(s)+1;
    Result := Trim(Copy(s, 1, n-1));
    Delete(s, 1, n);
  end;

  while (s <> '') and CharInSet(s[1], [' ', #9, #13, #10]) do Delete(s,1,1);  // I3310
end;

function GetLongFile(APath:String):String;
var
  i : Integer;
  h : THandle;
  Data : TWin32FindData;
  IsBackSlash : Boolean;
begin
  APath:=ExpandFileName(APath);
  i:=Pos('\',APath);
  Result:=Copy(APath,1,i);
  Delete(APath,1,i);
  repeat
    i:=Pos('\',APath);
    IsBackSlash:=i>0;
    if Not IsBackSlash then
      i:=Length(APath)+1;
    h:=FindFirstFile(PChar(Result+Copy(APath,1,i-1)),Data);
    if h<>INVALID_HANDLE_VALUE then begin
      try
        Result:=Result+Data.cFileName;
        if IsBackSlash then
          Result:=Result+'\';
      finally
        Windows.FindClose(h);
      end;
    end
    else begin
      Result:=Result+APath;
      Exit;
    end;
    Delete(APath,1,i);
  until Length(APath)=0;
end; {Peter Haas}

procedure GetFileNamesInDirectory(const directoryPath: string; var fileNames: TStringDynArray);

begin
  // Check if the directory exists
  if TDirectory.Exists(directoryPath) then
  begin
    // Retrieve file names within the directory
    fileNames := TDirectory.GetFiles(directoryPath);
  end
  else
    KL.Log('Directory does not exist.');
end;

{ TString }

constructor TString.Create(const AString: string);
begin
  inherited Create;
  FString := AString;
end;

constructor TString.Create;
begin
  inherited Create;
end;


function TSFInstalled: Boolean;
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_CLASSES_ROOT;
    Result := OpenKeyReadOnly('CLSID\{529A9E6B-6587-4F23-AB9E-9C7D683E3C50}');
  finally
    Free;
  end;
end;


function GetLongFileName(const fname: string): string;
var
  buf: array[0..260] of char;
  p: PChar;
begin
  buf[0] := #0;
  if GetFullPathName(PChar(fname), 260, buf, p) = 0
    then Result := fname
    else Result := buf;
end;

function RunConfiguration(WindowHandle: THandle; const Parameters: WideString): Boolean;
begin
  Result := TUtilExecute.Shell(WindowHandle, ParamStr(0), ExtractFileDir(ParamStr(0)), Parameters);
end;

function WaitForElevatedConfiguration(WindowHandle: THandle; const Parameters: WideString; FWait: Boolean): Cardinal;
var
  execinfo: TShellExecuteInfoW;
begin
  Result := $FFFFFFFF;
  FillChar(execinfo, sizeof(execinfo), 0);
  execinfo.cbSize := SizeOf(execinfo);
  execinfo.fMask := SEE_MASK_NOCLOSEPROCESS;
  execinfo.Wnd := WindowHandle;
  execinfo.lpVerb := 'runas';
  execinfo.lpFile := PWideChar(ParamStr(0));
  execinfo.lpParameters := PWideChar(Parameters);
  execinfo.lpDirectory := PWideChar(ExtractFileDir(ParamStr(0)));
  execinfo.nShow := SW_SHOWNORMAL;
  if ShellExecuteExW(@execinfo) then
  begin
    if FWait then
    begin
      EnableWindow(WindowHandle, False);   // I4169
      repeat
        if not GetExitCodeProcess(execinfo.hProcess, Result) then
        begin
          Result := $FFFFFFFF;
          Break;
        end;
        Sleep(10);
        Application.ProcessMessages;
      until Result <> STILL_ACTIVE;
      EnableWindow(WindowHandle, True);   // I4169
    end
    else
      Result := 0;
    CloseHandle(execinfo.hProcess);
  end
  else
    ShowMessage(SysErrorMessage(GetLastError));
end;

function DefaultServersXMLTags: string;
begin
  Result := Format('<keyman-com>%s</keyman-com><api-keyman-com>%s</api-keyman-com>', [
    XMLEncode(KeymanCom_Protocol_Server),
    XMLEncode(MakeAPIURL(''))
  ]);
end;

function DefaultVersionXMLTags: string;
begin
  with CKeymanVersionInfo do
    Result := Format(
      '<version-info version="%s" versionWin="%s" versionRelease="%s" versionMajor="%d" versionMinor="%d" '+
        'versionPatch="%d" tier="%s" tag="%s" versionWithTag="%s" environment="%s" />',
    [Version, VersionWin, VersionRelease, VersionMajor, VersionMinor,
    VersionPatch, Tier, Tag, VersionWithTag, Environment]);
end;

end.

