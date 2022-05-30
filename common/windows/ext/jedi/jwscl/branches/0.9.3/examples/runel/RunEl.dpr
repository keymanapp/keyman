program RunEl;

{$APPTYPE CONSOLE}
{$I JwsclUAC.inc}

{$R *.res}
uses
  JwaWindows,
  Dialogs,
  Math,
  ShellAPI,
  RunElCom_TLB,
  FileCtrl,
  Activex,
  ComObj,
  SysUtils,
  JwsclToken,
  JwsclElevation,
  JwsclVersion,
  JwsclTypes,
  JwsclConstants,
  JwsclExceptions;



type EUACCanceled = class(EJwsclSecurityException);
     EElevationFailed = class(EJwsclSecurityException);
     EProcessFailed = class(EJwsclSecurityException);

     TOption = (oWait, oCOMDll, oUseForeGroundWindow);
     TOptions = set of TOption;

procedure Elevate(Options: TOptions; const Name, Parameter, Directory : WideString);

procedure WaitFor(const ProcessHandle, ThreadHandle : HANDLE);
begin
  if ProcessHandle <> 0 then
    WaitForSingleObject(ProcessHandle,INFINITE);
end;

var
    ElevatedObject: IJwRunElevated;
    Result : HRESULT;
    LastError,
    ResultValue,
    NewThreadHandle,
    NewProcessHandle: LongWord;

    ForeGroundWindow : HWND;
begin
  if oUseForeGroundWindow in Options then
    ForeGroundWindow := GetForegroundWindow
  else
    ForeGroundWindow := 0;

  //use Shellexecute to start
  if not (oCOMDll in Options) then
  begin
    NewProcessHandle := 0;
    try
      NewProcessHandle := JwShellExecute(ForeGroundWindow, Name, Parameter, Directory, SW_NORMAL,
        [sefNoUi, sefFixDirWithRunAs, sefIgnoreElevationIfNotAvailable, sefNoClosehProcess]);
    except
      On E : EJwsclWinCallFailedException do
      begin
        LastError := E.LastError;
        NewProcessHandle := 0;

      end;
    end;

    try
      if (oWait in Options) and (NewProcessHandle <> 0) then
      begin
        WaitFor(NewProcessHandle, NewThreadHandle);

        //tell our caller the result of the process that we called
        ResultValue := 0;
        if (NewProcessHandle <> 0) and GetExitCodeProcess(NewProcessHandle, ResultValue) and (ResultValue <> 0)
         then
          raise EProcessFailed.CreateFmtEx('','','','',0,ResultValue,[]);
      end;
    finally
      if NewProcessHandle <> 0 then
        CloseHandle(NewProcessHandle);
    end;

    if (NewProcessHandle = 0) then
    begin
      if (LastError = 0) then
        LastError := DWORD(E_USER_CANCELED_OPERATION);
      raise EUACCanceled.CreateFmtEx('','','','',0,LastError,[]);
    end;
    exit;
  end
  else
  begin
    //use own implementation
    try
      Result := JwCoCreateInstanceAsAdmin(
        ForeGroundWindow,
        CLASS_JwRunElevated,
        IID_IJwRunElevated,
        ElevatedObject);

      if FAILED(Result) or not Assigned(ElevatedObject) then
      begin
        if GetLastError() <> 0 then
          Result := GetLastError();

        raise EUACCanceled.CreateFmtEx('','','','',0,Result,[]);
      end;

      if ElevatedObject.RunAppElevated
        (PWideChar(Name), PWideChar(Parameter), PWideChar(Directory),
          GetProcessId(GetCurrentProcess),//ClientProcessHandle: LongWord;
          NewThreadHandle, //out NewThreadHandle: LongWord;
          NewProcessHandle,//out NewProcessHandle: LongWord;
          ResultValue//out ResultValue: LongWord
         ) <> S_OK then
      begin
        raise EElevationFailed.CreateFmtEx('','','','',0,ResultValue,[]);
      end
      else
      begin
        ElevatedObject := nil; //disconnect from and terminate surrogate COM process

        try
          if (oWait in Options) and (NewProcessHandle <> 0) then
          begin
            WaitFor(NewProcessHandle, NewThreadHandle);

            //tell our caller the result of the process that we called
            ResultValue := 0;
            if GetExitCodeProcess(NewProcessHandle, ResultValue) and (ResultValue <> 0) then
              raise EProcessFailed.CreateFmtEx('','','','',0,ResultValue,[]);
          end;
        finally
          if NewProcessHandle <> 0 then
            CloseHandle(NewProcessHandle);
          if NewThreadHandle <> 0 then
            CloseHandle(NewThreadHandle);
        end;
      end;
    finally
      ElevatedObject := nil; //disconnect from and terminate surrogate COM process
    end;
  end;
end;

const COMDLLName = 'RunElCOM.dll';

procedure SetupCOMDLL(const DoUninstall : Boolean = false);
var Path : String;
begin
  Path := '"'+GetCurrentDir+'\'+COMDLLName+'"';
  try
    if DoUninstall then
      Elevate([], 'regsvr32.exe', '/s /u '+Path,'')
    else
      Elevate([], 'regsvr32.exe', '/s '+Path,'');
  except
    on E : EJwsclSecurityException do
    begin
      if E.LastError <> ERROR_NO_MORE_ITEMS then
      begin
        Writeln('Could not register ',Path,'. (',E.LastError,') ',#13#10+E.GetErrorMessage(E.LastError));

        CoUninitialize;
        Halt(E.LastError);
      end;
    end;
  end;
  if DoUninstall then
  begin
    Writeln('The COM DLL was successfully uninstalled.');
  end
  else
  begin
    Writeln('The COM DLL was successfully installed.');
  end;
end;


var iP,i2  : Integer;
    Configs : String;
    P : PChar;
    Name,
    Parameter : WideString;
    Options: TOptions;
    OptionStr : String;

    HaltValue : Integer;
begin
  if not (TJwWindowsVersion.IsWindowsVista(true) or
    TJwWindowsVersion.IsWindows2008(true)) then
  begin
    Writeln('The operating system is not supported. Only Windows Vista, Server 2008 and newer are supported');
    Halt($DB8); //3512 : The software requires a newer version of the operating system.
  end;

  if ParamCount = 0 then
  begin
    Writeln('RunEl V1.0 - Run application elevated ');
    Writeln('            by Christian Wimmer @ 2008');
    Writeln('Visit us at http://blog.delphi-jedi.net');
    Writeln('');
    Writeln('RunEl [/INSTALL][/UNINSTALL][/W[D][G]] [AppName] [Parameters]');
    Writeln('');
    Writeln('Parameters: ');
    Writeln('/INSTALL - installs the RunElCOM.dll. Needed for parameter /D ');
    Writeln('   RunElCOM.dll must be in the same folder as RunEl.');
    Writeln('/UNINSTALL - uninstalls the RunElCOM.dll.');
    Writeln('W - Wait for called process to be finished.');
    Writeln('D - uses own elevation COM Class. Before using must be setup with /INSTALL .');
    Writeln('G - uses foreground window to display UAC Prompt on.');
    Writeln('Parameters W, D and G must be mixed up with only one "/"');
    Writeln('');
    halt(0);
  end;


  HaltValue := 0;
  CoInitialize(nil);
  try
    iP := 1;

    //get configs
    Configs := ParamStr(iP);
    if (Length(Configs) > 0) and (Configs[1] = '/') then
    begin
      OptionStr := UpperCase(Copy(Configs,2,Length(Configs)));

      if OptionStr = 'INSTALL' then
      begin
        SetupCOMDLL;

        CoUninitialize;
        Halt(0);
      end
      else
      if OptionStr = 'UNINSTALL' then
      begin
        SetupCOMDLL(true);

        CoUninitialize;
        Halt(0);
      end
      else
      begin
        if pos('W', OptionStr) > 0 then
          Include(Options,oWait);
        if pos('D', OptionStr) > 0 then
          Include(Options,oCOMDll);
        if pos('G', OptionStr) > 0 then
          Include(Options,oUseForeGroundWindow);
      end;

      Inc(iP);
    end;

    //get main application name
    Name := ParamStr(iP);
    Inc(iP);

    //get rest of parameters
    Parameter := ParamStr(iP);

    i2 := Pos(Parameter, WideString(CmdLine));
    if i2 > 0 then
    begin
      P := CmdLine;
      Inc(P, i2-1);
      Parameter := WideString(P);
    end
    else
      Parameter := '';

    try
      Elevate(Options, Name, Parameter, GetCurrentDir);
    except
      on e : EProcessFailed do
      begin
        Writeln('The process returned an error: (',E.LastError,') '+E.GetErrorMessage(E.LastError));
        HaltValue := E.LastError;
      end;
      on E : EJwsclUnsupportedWindowsVersionException do
      begin
        Writeln('Elevation is not supported on this system.');
        HaltValue := 740;
      end;
      on E : EJwsclSecurityException do
      begin
        Writeln('Elevation failed. (',E.LastError,') '+E.GetErrorMessage(E.LastError));
        HaltValue := E.LastError;
      end;
    end;
  except
    on E : Exception do
    begin
      Writeln('Abnormal program termination');
      Writeln(E.Message);
      HaltValue := -2147286790;
    end;
  end;
  CoUninitialize;
  Halt(HaltValue);
end.
