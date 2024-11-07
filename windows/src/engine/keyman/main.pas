(*
  Name:             main
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      1 Aug 2006

  Modified Date:    25 Oct 2016
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          01 Aug 2006 - mcdurdin - Initial version
                    02 Aug 2006 - mcdurdin - Timeout when Beta expires
                    04 Dec 2006 - mcdurdin - Block Keyman loading if KM5/6 running
                    23 Aug 2007 - mcdurdin - I910 - Fix crash when Keyman 6.0 is running and Keyman starts
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    01 Jan 2013 - mcdurdin - I3677 - V9.0 - OSK and Keyman menu show an entry in taskbar
                    17 Jan 2013 - mcdurdin - I3758 - V9.0 - keymanx64 can find incorrect window handle for keyman.exe
                    25 Oct 2016 - mcdurdin - I5136 - Remove additional product references from Keyman Engine
*)
unit main;  // I3306   // I5136

interface

procedure Run;

implementation

uses
  Vcl.Dialogs,
  Vcl.Forms,
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.Win.Registry,

  GetOsVersion,
  Keyman.System.ExecutionHistory,
  Keyman.System.Security,
  Keyman.Winapi.VersionHelpers,
  KeymanVersion,
  Klog,
  RegistryKeys,
  UfrmKeyman7Main,
  UserMessages;

function ValidateParameters(var FCommand: Integer): Boolean; forward;
function PassParametersToRunningInstance(FCommand: Integer): Boolean; forward;
procedure InitialiseRegistrySecurity; forward;

function CaptureMutex: Boolean;
begin
  case WaitForSingleObject(hProgramMutex, 0) of
    WAIT_ABANDONED, WAIT_OBJECT_0: Result := True;
    WAIT_FAILED, WAIT_TIMEOUT: Result := False;
    else Result := False;
  end;
end;

procedure RunProgram; forward;

procedure Run;
begin
  RunProgram;
  frmKeyman7Main.Free;
end;

procedure RunProgram;
var
  FCommand: Integer;
  Finished: Boolean;
  Count: Integer;
  hMutex: Cardinal;
begin

  if not ValidateParameters(FCommand) then Exit;

  RecordKeymanStarted;

  hProgramMutex := CreateMutex(nil, False, 'KeymanEXE70');
  if hProgramMutex = 0 then
  begin
    ShowMessage('Keyman Engine '+SKeymanVersion+' is already running');
    Exit; { TODO: return an error message }
  end;

  try
    Count := 0;

    repeat
      Inc(Count);
      if CaptureMutex then
      begin

        // Stop Keyman 5 or 6 from starting while Keyman is running
        hMutex := CreateMutex(nil, True, 'Tavultesoft Keyman '+SKeymanVersion50); // DO NOT CHANGE!
        try
          if (GetLastError = ERROR_ALREADY_EXISTS) or (hMutex = 0) then
          begin
            ShowMessage('Keyman Engine '+SKeymanVersion+' can not be started while Keyman 5.x or 6.x is running.  Please shut down Keyman 5.x/6.x first.');
            Exit;
          end;

          if FindWindow('Keyman50', nil) <> 0 then
          begin
            ShowMessage('Keyman Engine '+SKeymanVersion+' can not be started while Keyman 5.x or 6.x is running.  Please shut down Keyman 5.x/6.x first.');
            Exit;
          end;

          InitialiseRegistrySecurity;

          { we now own the mutex }
          Application.Initialize;
          Application.ShowMainForm := False;
          Application.Title := 'keyman';   // I3758

          // Stop activation of main window
          EnableWindow(Application.Handle, False);

          // Hide app window from task bar
          //SetWindowLong(Application.Handle, GWL_EXSTYLE, GetWindowLong(Application.Handle, GWL_EXSTYLE) or WS_EX_TOOLWINDOW);   // I3677
          Application.MainFormOnTaskBar := True;   // I3677

          Application.CreateForm(TfrmKeyman7Main, frmKeyman7Main);
          if not Application.Terminated then
            Application.Run;

          FreeAndNil(frmKeyman7Main);

          Finished := True;
        finally
          if hMutex <> 0 then CloseHandle(hMutex);
        end;
      end
      else
      begin
        Finished := PassParametersToRunningInstance(FCommand);
      end;
    until Finished or (Count > 3);

  finally
    CloseHandle(hProgramMutex);
  end;
end;

function PassParametersToRunningInstance(FCommand: Integer): Boolean;
var
  hwnd: THandle;
begin
  hwnd := FindWindow('TfrmKeyman7Main', nil);
  Result := hwnd <> 0;
  if not Result
    then Sleep(500) { wait 0.5 seconds, may be starting up or shutting down; try again to get the mutex }
    else PostMessage(hwnd, WM_USER_ParameterPass, FCommand, 0);
end;

function ValidateParameters(var FCommand: Integer): Boolean;
var
  sCommand: string;
begin
  Result := False;

  if LowerCase(ParamStr(1)) <> '-kmc' then Exit;

  sCommand := LowerCase(ParamStr(2));

  if sCommand = 'start' then
    FCommand := KMC_StartProduct
  else if sCommand = 'stop' then
    FCommand := KMC_StopProduct
  else if sCommand = 'showvisualkeyboard' then
    FCommand := KMC_ShowVisualKeyboard
  else if sCommand = 'hidevisualkeyboard' then
    FCommand := KMC_HideVisualKeyboard
  else Exit;

  FEnableCrashTest := ParamStr(3) = '-sentry-client-test-exception';


  Result := True;
end;

{
  Enables metro-style apps to read the Keyman registry key in order
  to load the list of keyboards, and other settings, on Windows 8 and
  later versions
}
procedure InitialiseRegistrySecurity;
var
  r: TRegistry;

  procedure ProcessKey(const root: string);
  var
    s: string;
    str: TStringList;
  begin
    if r.OpenKey('\' + root, True) then
    begin
      str := TStringList.Create;
      try
        r.GetKeyNames(str);
        GrantPermissionToAllApplicationPackages(r.CurrentKey, KEY_READ);
        for s in str do
          ProcessKey(root + '\' + s);
      finally
        str.Free;
      end;
    end;
  end;

begin
  r := TRegistry.Create;
  try
    ProcessKey(SRegKey_KeymanRoot_CU);
  finally
    r.Free;
  end;
end;

end.

