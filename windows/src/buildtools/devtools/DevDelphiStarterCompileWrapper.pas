//
// This unit exists to allow us to do a semi-automated build of the Keyman
// projects with Delphi Starter Edition. Without it, Delphi Starter users
// will be hamstrung and probably will give up. This unit exists because
// Delphi Starter Edition deliberately disabled the command line compiler.
//
unit DevDelphiStarterCompileWrapper;

interface

type
  TDelphiStarterCompileWrapper = class
  private
    function RunViaIDE(AProjectFilename, ADcpFilename, ABplFilename: string): Boolean;
    procedure WaitCallback(hProcess: THandle; var Waiting, Cancelled: Boolean);
    function CancelSaveDialog(dwProcessID: Cardinal): Boolean;
  public
    class function Run(Quiet: Boolean = False; Silent: Boolean = False): Boolean;  // I3378
  end;

{$IFDEF VER320}
const
  SBDSExe = 'C:\Program Files (x86)\Embarcadero\Studio\19.0\bin\BDS.EXE';
{$ELSE}
{$IFDEF VER330}
const
  SBDSExe = 'C:\Program Files (x86)\Embarcadero\Studio\20.0\bin\BDS.EXE';
{$ELSE}
ERROR: SBDSExe is not defined
{$ENDIF}
{$ENDIF}

implementation

uses
  Winapi.Messages,
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  DevUtils,
  utilexecute,
  utilsystem;

class function TDelphiStarterCompileWrapper.Run(Quiet, Silent: Boolean): Boolean;  // I3378
var
  DprFilename: string;
  DcpFilename: string;
  BplFilename: string;
begin
  DprFilename := ParamStr(2);
  DcpFilename := ParamStr(3);
  BplFilename := ParamStr(4);

  DevLog('BDS '+DprFilename,True);

  with TDelphiStarterCompileWrapper.Create do
  try
    Result := RunViaIDE(DprFilename, DcpFilename, BplFilename);
  finally
    Free;
  end;
end;

//
// Presses a No button in a confirmation dialog. Yikes.
// (EnumChildWindowProc)
//
function PressNoButtonProc(hwnd: THandle; lParam: LPARAM): BOOL; stdcall;
var
  buf: array[0..128] of char;
begin
  if GetClassName(hwnd, buf, 128) = 0 then
    Exit(True);
  buf[128] := #0;
  if not SameText(buf, 'Button') then
    Exit(True);

  if GetWindowText(hwnd, buf, 128) = 0 then
    Exit(True);
  if not SameText(buf, '&No') then
    Exit(True);

  PostMessage(hwnd, WM_LBUTTONDOWN, 0, 0);
  PostMessage(hwnd, WM_LBUTTONUP, 0, 0);
  PDWord(lParam)^ := 1;
  Exit(False);
end;

//
// Many times, a build will cause Delphi to make silent changes to the project.
// This blocks the automated build unnecessarily, so we go ahead and click the
// <No> button in the Save confirmation dialog when it pops up. We are careful
// to do this only in the process that we started with the command line build
// parameter.
//
function TDelphiStarterCompileWrapper.CancelSaveDialog(dwProcessID: Cardinal): Boolean;
var
  dwWindowProcessID: DWord;
  handle: THandle;
  ecwresult: DWord;
begin
  // Does a Save Changes window exist? (yeah, this is a little iffy)
  handle := FindWindow( '#32770', 'Confirm');
  if handle = 0 then
    Exit(False);

  // Does the window belong to the same process?
  GetWindowThreadProcessId(handle, dwWindowProcessID);
  if dwProcessID <> dwWindowProcessID then
    Exit(False);

  // Find the <No> button and click it, and then let the parent process
  // know that we have done so.
  ecwresult := 0;
  EnumChildWindows(handle, @PressNoButtonProc, NativeInt(@ecwresult));
  Result := ecwresult = 1;
end;

//
// Wait for Delphi IDE to finish doing the automated build
//
procedure TDelphiStarterCompileWrapper.WaitCallback(hProcess: THandle; var Waiting, Cancelled: Boolean);
var
  dwProcessId: Cardinal;
  SaveDialogCancelled: Boolean;
begin
  // Watch for a popup from the process that says 'Confirm' and "click" the NO button.
  // This won't work in non-English Delphi. Scripting UI is BAD.

  SaveDialogCancelled := False;
  dwProcessId := GetProcessId(hProcess);
  repeat
    if not SaveDialogCancelled and CancelSaveDialog(dwProcessId) then SaveDialogCancelled := True;
  until WaitForSingleObject(hProcess, 100) <> WAIT_TIMEOUT;
  Cancelled := False;
  Waiting := False;
end;

//
// Fire up the IDE and build the project automatically with the hacky -b command line parameter
// Delphi IDE ignores SW_SHOWMINIMIZED but we will pretend anyway like no one notices.
//
function TDelphiStarterCompileWrapper.RunViaIDE(AProjectFilename, ADcpFilename, ABplFilename: string): Boolean;
var
  ec: DWord;
  i: Integer;
  FCmdline: string;
  FCurDir: string;
begin
  FCmdline := '"'+SBDSExe+'" -ns -b "'+AProjectFileName+'"';

  FCurDir := ExtractFileDir(AProjectFileName);
  if FCurDir = '' then
    FCurDir := GetCurrentDir;

  // Run the Delphi IDE to build the project
  if not TUtilExecute.WaitForProcess(FCmdline, FCurDir, ec, SW_SHOWMINIMIZED, WaitCallback) then
  begin
    writeln('Failed to start BDS.EXE with error '+SysErrorMessage(GetLastError));
    Exit(False);
  end;

  // Write the output of the compiler to console
  if FileExists(ChangeFileExt(AProjectFileName, '.err')) then
  begin
    with TStringList.Create do
    try
      LoadFromFile(ChangeFileExt(AProjectFileName, '.err'));
      for i := 0 to Count - 1 do
        writeln(Strings[i]);
    finally
      Free;
    end;
  end;

  if ec > 0 then
  begin
    writeln('BDS.EXE failed with error '+ec.ToString);
    Exit(False);
  end;

  Result := True;
end;

end.
