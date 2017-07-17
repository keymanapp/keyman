{
The contents of this file is released under the terms of the
GNU General Public License 3.0 (the  "GPL License").

For more information about the GPL: http://www.gnu.org/licenses/gpl-3.0.txt

Original author is: Christian Wimmer
This application is part of the JEDI API Project.
Visit at http://blog.delphi-jedi.net/

}
program RunAsSys;

uses
  SvcMgr,
  SysUtils,
  Dialogs,
  Controls,
  uLogging,
  JwaWindows,
  JwsclLogging,
  JwsclToken,
  JwsclKnownSid,
  JwsclElevation,
  JwsclVersion,
  JwsclExceptions,
  jwscltypes,
  JwsclConstants,
  RunAsSysService in 'RunAsSysService.pas' {RunAsSysSvc9: TService};

{$R *.RES}


procedure StartTheService(Service : TService);
var
  hSCM: SC_HANDLE;
  hSvc: SC_HANDLE;
  args: array of PChar;
  i : Integer;
  Log : IJwLogClient;
begin
  Log := uLogging.LogServer.Connect(etFunction, '','StartTheService','RunAsSys.dpr','Starting service...');

  Log.Log('Open service manager...');
  hSCM := OpenSCManager(nil, SERVICES_ACTIVE_DATABASEW, SC_MANAGER_CONNECT);
  if hSCM = 0 then
    RaiseLastOSError;

  try
    Log.Log('Open service...');
    hSvc := OpenService(hSCM, PChar(Service.Name), SERVICE_START);
    if hSvc = 0 then
      RaiseLastOSError;

    try
      SetLength(args, ParamCount+1);

      try
        args[0] := PChar(IntToStr(JwGetProcessLogonSession));
      except
        args[0] := '0'#0; 
      end;
      for i := 1 to ParamCount do
      begin
        Log.Log('Adding Parameter: '+ParamStr(i));
        GetMem(args[i], SizeOf(Char) * (Length(ParamStr(i))+2));
        StringCchCopy(args[i], Length(ParamStr(i))+2, PChar(ParamStr(i)));
      end;
      try
        Log.Log('Start service...');
        if not StartService(hSvc, Length(args), @args[0]) then
          RaiseLastOSError;
        Log.Log('Cleanup...');
      finally
        for i := 1 to ParamCount do
        begin
          FreeMem(args[i]);
        end;
      end;
    finally
      CloseServiceHandle(hSvc);
    end;
  finally
   CloseServiceHandle(hScm);
  end;
end;

function CheckParameters(Log : IJwLogClient) : AnsiString;
var
  SysPath : Array[0..MAX_PATH] of AnsiChar;
  hres : HRESULT;
  Temp1 : AnsiString;
begin
  result := '';

  begin
    if Assigned(Log) then
      Log.Log('No parameters found. Starting command line as SYSTEM...');
    ZeroMemory(@SysPath, sizeof(SysPath));

    hres := SHGetFolderPathA(0,CSIDL_SYSTEMX86,0,0, SysPath);
    if SUCCEEDED(hres) then
    begin
      Temp1 := StringReplace(SysPath,'\\','\',[rfReplaceAll]);
      Temp1 := IncludeTrailingBackslash(Temp1);
      result := '"' +Temp1+'cmd.exe"'
    end
    else
    begin
      if Assigned(Log) then
        Log.Log(Format('SHGetFolderPath returned error %d',[hres]),lsError);
      result := 'C:\Windows\System32\cmd.exe';
    end;
  end
end;

function MainWorkerProcedure : Integer;
var
  Log : IJwLogClient;

  procedure InitLog;
  begin
    uLogging.InitLog;

    Log := uLogging.LogServer.Connect(etThread, '','WinMain','RunAsSys.dpr','Entering main thread');
  end;


var
  SuRunCode : Cardinal;
  Retries,
  i : Integer;

  SuRun,
  Parameters : String;
  hRes : HRESULT;


  Continue : Boolean;
  Shell : TShellExecuteInfo;
begin
  result := 0;
  Log := nil;
  try
    JwInitWellKnownSIDs;


    if JwIsPrivilegeSet(SE_TCB_NAME, pqt_Available) and
     {  JwIsPrivilegeSet(SE_CREATE_TOKEN_NAME, pqt_Available) and}
       (JwSecurityProcessUserSID.EqualSid(JwLocalSystemSID)) then
    begin
      InitLog;
     
      Log.Log('Found service privileges. Starting service...');
      SvcMgr.Application.Initialize;
      SvcMgr.Application.CreateForm(TRunAsSysSvc9, RunAsSysSvc9);
  {$IFDEF CMD}
      RunAsSysSvc3.DoExecute();
  {$ELSE}
      SvcMgr.Application.Run;
  {$ENDIF}
      Log.Log('Ending service.');

      exit;
    end
    else
    if (JwCheckAdministratorAccess) then
    begin
      if ParamCount = 0 then
      begin
        JwShellExecute(0, //const hWnd: HWND;
          ParamStr(0), //FileName,
          CheckParameters(Log), //Parameters,
          '', //Directory: TJwString;
          0, //ShowCmd: Integer;
          [sefIgnoreElevationIfNotAvailable]//Flags : TJwShellExecuteFlags = [sefNoClosehProcess]): HANDLE;
        );
        exit;
      end;

      InitLog;

      Log.Log('Found Administrator privileges. Creating service...');


      SvcMgr.Application.Free;
      SvcMgr.Application := TServiceApplicationEx.Create(nil);
      SvcMgr.Application.Initialize;
      SvcMgr.Application.CreateForm(TRunAsSysSvc9, RunAsSysSvc9);


      Randomize;
      Retries := 5;

     {
     Todo:
       The service itself started above must be changed as well to the name given here.
       Otherwise it wont start.
     }
    {  while Retries > 0 do}
      begin
{        RunAsSysSvc8.Name := 'RunAsSysSvc'+IntToStr(Random(100000));
        RunAsSysSvc8.DisplayName := 'This is a temporary RunAsSys service. ' + DateTimeToStr(Now) ;

 }
        RunAsSysSvc9.DisplayName := 'This is a temporary RunAsSys service. ' + DateTimeToStr(Now) ;

        try
          Log.Log('Registering service...');
          TServiceApplicationEx(SvcMgr.Application).RegisterServices(true, true);
          Retries := 0;
        except
          on E : EOSError do
          begin
            Log.Exception(E);
            if (E.ErrorCode <> 1073) {and (Retries <= 0)} then
              raise;
            Dec(Retries);
          end;
        end;

      end;


      try
        Log.Log('Starting service...');
        StartTheService(RunAsSysSvc9);
      finally
       Log.Log('Remove service.');
       TServiceApplicationEx(SvcMgr.Application).RegisterServices(false, true);
      end;
      SvcMgr.Application.Run;

      exit;
    end
    else
    begin
      InitLog;
      try
        if ParamCount = 0 then
          Parameters := CheckParameters(log)
        else
        begin
          Parameters := '';
          for i := 1 to ParamCount do
          begin
            Parameters := Parameters + ' "' +ParamStr(i)+'"';
          end;
          System.Delete(Parameters, 1,1);
        end;

        Continue := true;

        {This section uses the surun verb to execute the process
        as an administrator.
        Get SuRun from http://kay-bruns.de/wp/software/surun/
        }

        //hold key to ignore surun !
        if GetAsyncKeyState(VK_SHIFT) = 0 then
        begin
          Log.Log(Format('Try to surun with these parameters: %s',[Parameters]));

          ZeroMemory(@Shell, sizeof(TShellExecuteInfo));
          Shell.cbSize := sizeof(TShellExecuteInfo);
          Shell.lpVerb := 'SuRun';
          Shell.lpFile := PChar(ParamStr(0));
          Shell.lpParameters := PChar(Parameters);
          Shell.fMask := SEE_MASK_NOCLOSEPROCESS or SEE_MASK_FLAG_NO_UI;

          if ShellExecuteEx(Shell) then
          if Shell.hProcess <> 0 then
          begin
            //60sec timeout period
            case WaitForSingleObject(Shell.hProcess, 60*1000) of
              WAIT_FAILED :
                begin
                  Continue := true;
                  Log.Log(lsWarning,Format('Waiting for SuRun failed: %d',[GetLastError]));
                  //not sure what to do here so we just continue
                end;
              WAIT_OBJECT_0 :
                begin
                  if GetExitCodeProcess(Shell.hProcess,SuRunCode) then
                  begin
{
#define RETVAL_OK           0
#define RETVAL_ACCESSDENIED 1
#define RETVAL_RESTRICT     3
#define RETVAL_CANCELLED    4
}
                    Continue := (SuRunCode <> 0) and (SuRunCode <> 4);
                    if Continue then
                    begin
                      Log.Log(lsError,Format('SuRun returned error code: %d',[SuRunCode]));
                    end;
                  end;
                end;
            end;

            CloseHandle(Shell.hProcess);

            if Continue then
              Log.Log('SuRun did not work! Falling through to compatibility mode.');
          end;  // if Shell.hProcess <> 0 then

        end;

        //compatibility mode
        if Continue then
        begin
          if not (TJwWindowsVersion.IsWindowsVista(true) or
            TJwWindowsVersion.IsWindows2008(true)) then
          begin
            if MessageDlg('You have to logon as administrator. Please enter your admin credentials. ',
                mtInformation, [mbOK,mbCancel],0) = mrCancel then
            begin
              result := 2;
              exit;
            end;
          end;


          Log.Log(Format('Try to elevate with these parameters: %s',[Parameters]));
          JwShellExecute(0, //const hWnd: HWND;
            ParamStr(0), //FileName,
            Parameters, //Parameters,
            '', //Directory: TJwString;
            0, //ShowCmd: Integer;
            [sefIgnoreElevationIfNotAvailable]//Flags : TJwShellExecuteFlags = [sefNoClosehProcess]): HANDLE;
          );
        end;
      except
        on E : EJwsclWinCallFailedException do
        begin
          Log.Log(lsError,'Catching EJwsclWinCallFailedException from JwShellExecute');
          Log.Exception(E);
          if E.LastError <> E_USER_CANCELED_OPERATION then
            ShowMessage(e.Message);

          result := E.LastError;
          exit;
        end;
        on E1 : Exception do
        begin
          Log.Log(lsError,'Catching Exception from JwShellExecute');
          ShowMessage(e1.Message);

          Result := 1;
          exit;
        end;
      end;
      exit;
    end;
  except
    on E1 : EOSError do
    begin
      result := E1.ErrorCode;
      Log.Exception(E1);
    end;
    on E2 : EJwsclSecurityException do
    begin
      result := E2.LastError;
      Log.Exception(E2);
    end;
    on E3 : Exception do
    begin
      result := 1;
      Log.Exception(E3);
    end;
  end;
end;

procedure DoHalt(const Value : Integer);

procedure EndLog;
var
  Log : IJwLogClient;
begin
  if Assigned(LogServer) then
  begin
    Log := uLogging.LogServer.Connect(etNone, '','','','');
    Log.Log(lsStop,Format('Halt(%d)',[Value]));

    uLogging.LogServer.Disconnect(Log);
  end;
end;

begin
  EndLog;
  DoneLog;
  LogServer := nil;
  halt(Value);
end;

begin
  DoHalt(MainWorkerProcedure);
end.
