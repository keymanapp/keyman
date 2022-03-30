unit Keyman.Developer.System.Project.ProjectLogConsole;

interface

uses
  Keyman.Developer.System.Project.ProjectLog;

type
  TProjectLogConsole = class
  strict private
    class var FInstance: TProjectLogConsole;
  private
    hConsole: THandle;
    hOutFile: THandle;
    FFullySilent: Boolean;
    FSilent: Boolean;
    FFilename: string;
    FHasWarning: Boolean;
    FMessageCount: Integer;
    FColor: Boolean;
    procedure DetectColorMode;
  public
    type TColorMode = (cmDefault, cmForceColor, cmForceNoColor);
  public
    constructor Create(ASilent, AFullySilent: Boolean; AhOutFile: THandle; AColorMode: TColorMode);
    procedure Log(AState: TProjectLogState; Filename: string; Msg: string; MsgCode, Line: Integer); overload;
    procedure Log(AState: TProjectLogState; Msg: string; MsgCode, Line: Integer); overload;
    class property Instance: TProjectLogConsole read FInstance;
    property Filename: string read FFilename write FFilename;
    property HasWarning: Boolean read FHasWarning;
  end;

function CompilerMessage(line: Integer; msgcode: LongWord; text: PAnsiChar): Integer; stdcall;   // I3310
function CompilerMessageW( line: Integer; msgcode: LongWord; const text: string): Integer;
implementation

uses
  System.SysUtils,
  Winapi.Windows,

  compile;

const
  MAX_MESSAGES = 100;

{ TProjectLogConsole }

procedure TProjectLogConsole.Log(AState: TProjectLogState; Filename,
  Msg: string; MsgCode, Line: Integer);
var
	dw: DWord;
  str: string;
  astr: RawByteString;
const
	nlstr: array[0..2] of ansichar = (#$D, #$A, #$0);   // I3310

const
  ESC=#$1b;
  ESC_BRIGHT_YELLOW=ESC+'[38;2;255;255;0m';
  ESC_RED=ESC+'[38;2;255;0;0m';
  ESC_GREEN=ESC+'[38;2;0;255;0m';
  ESC_DEFAULT=ESC+'[0m';
begin
  // TODO: Colour
  if (AState = plsInfo) and FSilent then Exit;

  if AState = plsWarning then
    FHasWarning := True;

  if (AState in [plsWarning, plsError, plsSuccess, plsFailure]) and FFullySilent then Exit;

  str := '';

  if AState in [plsWarning, plsError] then
  begin
    Inc(FMessageCount);
    if FMessageCount > MAX_MESSAGES then
      Exit;

    if FMessageCount = MAX_MESSAGES then
      str := Format('More than %d warnings or errors received; suppressing further messages', [MAX_MESSAGES]);
  end;

  if str = '' then

  str := TProjectLog.FormatMessage(AState, Filename, msg, msgcode, line);

	if hOutfile <> 0 then
  begin
    astr := UTF8Encode(str);
		WriteFile(hOutfile, astr, Length(astr), dw, nil);
		WriteFile(hOutfile, nlstr, 2, dw, nil);
	end
	else
  begin
    if FColor then
    begin
      case AState of
        plsInfo: write(ESC_DEFAULT);
        plsWarning: write(ESC_BRIGHT_YELLOW);
        plsSuccess: write(ESC_GREEN);
        plsFailure,
        plsFatal,
        plsError: write(ESC_RED);
        else write(ESC_DEFAULT);
      end;
    end;
    writeln(str);
    if FColor then
      write(ESC_DEFAULT);
  end;
end;

procedure TProjectLogConsole.Log(AState: TProjectLogState; Msg: string; MsgCode, Line: Integer);
begin
  Log(AState, FFilename, Msg, MsgCode, Line);
end;

{ TLogger }

constructor TProjectLogConsole.Create(ASilent, AFullySilent: Boolean; AhOutFile: THandle; AColorMode: TColorMode);
begin
  Assert(FInstance = nil);
  FInstance := Self;
  inherited Create;
  FSilent := ASilent;
  FFullySilent := AFullySilent;
  hOutFile := AhOutFile;

  case AColorMode of
    cmDefault: DetectColorMode;
    cmForceColor: FColor := True;
    cmForceNoColor: FColor := False;
  end;
end;

procedure TProjectLogConsole.DetectColorMode;
var
  mode: DWORD;
const
  ENABLE_VIRTUAL_TERMINAL_PROCESSING = 4;
begin
  mode := 0;
  hConsole := GetStdHandle(STD_OUTPUT_HANDLE);
  if hConsole = INVALID_HANDLE_VALUE then
  begin
    writeln(Format('GetStdHandle failed with %d %s', [GetLastError, SysErrorMessage(GetLastError)]));
    Exit;
  end;

  if GetEnvironmentVariable('MSYSTEM') = 'MINGW64' then
  begin
    // MinGW64 test
    // Use colour mode only with a non-redirected console. This test fails with pipes.
    // For pipe use, explicitly use -no-color parameter
    FColor := GetFileType(hConsole) = 3;
  end
  else
  begin
    // Win32 console color mode test
    if not GetConsoleMode(hConsole, mode) then
      Exit;

    mode := mode or ENABLE_VIRTUAL_TERMINAL_PROCESSING;
    if not SetConsoleMode(hConsole, mode) then
      Exit;

    FColor := True;
  end;
end;

function CompilerMessageW( line: Integer; msgcode: LongWord; const text: string): Integer;
var
  state: TProjectLogState;
begin
  if (msgcode = CWARN_Info) then state := plsInfo
  else if (msgcode and CERR_ERROR) <> 0   then state := plsError
  else if (msgcode and CERR_WARNING) <> 0 then begin state := plsWarning; end   // I4706
  else if (msgcode and CERR_FATAL) <> 0   then state := plsFatal
  else if (msgcode and CERR_HINT) <> 0    then state := plsHint
  else state := plsFatal;

  TProjectLogConsole.Instance.Log(state, text, msgcode, line);

  Result := 1;
end;

function CompilerMessage(line: Integer; msgcode: LongWord; text: PAnsiChar): Integer; stdcall;   // I3310
begin
  Result := CompilerMessageW(line, msgcode, string(AnsiString(text)));
end;

end.
