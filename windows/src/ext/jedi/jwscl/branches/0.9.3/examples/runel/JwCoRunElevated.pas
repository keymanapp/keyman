unit JwCoRunElevated;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  JwaWindows, ActiveX, Dialogs, Classes, ComObj, RunElCOM_TLB, StdVcl;

type
  TJwRunElevated = class(TTypedComObject, IJwRunElevated)
  protected
    function RunAppElevated(AppName: PWideChar; Parameter: PWideChar; Dir: PWideChar; 
                            ClientProcessID: LongWord; out NewThreadHandle: LongWord;
                            out NewProcessHandle: LongWord; out ResultValue: LongWord): HResult;  stdcall;
    {IJwRunElevated-Methoden hier deklarieren}
  end;

resourcestring
  ElevationDescription = 'Run Application Elevated';

implementation



uses ComServ, JwsclElevation, SysUtils;

function TJwRunElevated.RunAppElevated(AppName: PWideChar; Parameter: PWideChar; Dir: PWideChar;
                            ClientProcessID: LongWord; out NewThreadHandle: LongWord;
                            out NewProcessHandle: LongWord; out ResultValue: LongWord): HResult;
var ProcHandle : HANDLE;
    StartupInfo : TStartupInfoW;
    ProcessInfo : TProcessInformation;

    pDir : PWideChar;
    Para : WideString;
    ClientProc,
    ThisProc : HANDLE;
begin


  ZeroMemory(@StartupInfo, sizeof(StartupInfo));
  StartupInfo.cb := SizeOf(StartupInfo);


  if Length(Dir) <> 0 then
    pDir := PWideChar(Dir)
  else
    pDir := nil;

  Para := '"'+WideString(AppName)+'" '+WideString(Parameter);

  SetLastError(0);
  if CreateProcessW(
        nil,//__in_opt     LPCTSTR lpApplicationName,
        PWideChar(Para), //__inout_opt  LPTSTR lpCommandLine,
        nil,//__in_opt     LPSECURITY_ATTRIBUTES lpProcessAttributes,
        nil,//LPSECURITY_ATTRIBUTES(InVars.Parameters.lpThreadAttributes),//__in_opt     LPSECURITY_ATTRIBUTES lpThreadAttributes,
        true,//__in         BOOL bInheritHandles,
        0,//__in         DWORD dwCreationFlags,
        nil,//__in_opt     LPVOID lpEnvironment,
        pDir,//'',//TJwPChar(InVars.Parameters.lpCurrentDirectory),//__in_opt     LPCTSTR lpCurrentDirectory,
        StartupInfo,//__in         LPSTARTUPINFO lpStartupInfo,
        ProcessInfo //__out        LPPROCESS_INFORMATION lpProcessInformation
    ) then
  begin
    ResultValue := GetLastError;
    result := S_OK;


    if GetCurrentProcessId <> ClientProcessID then
    begin
      ClientProc := OpenProcess(MAXIMUM_ALLOWED, true, ClientProcessID);

      DuplicateHandle(GetCurrentProcess, ProcessInfo.hProcess, ClientProc, @NewProcessHandle,
         SYNCHRONIZE or READ_CONTROL or PROCESS_VM_READ, true, DUPLICATE_CLOSE_SOURCE);

      DuplicateHandle(GetCurrentProcess, ProcessInfo.hThread, ClientProc, @NewThreadHandle,
         SYNCHRONIZE or READ_CONTROL or THREAD_QUERY_INFORMATION, true, DUPLICATE_CLOSE_SOURCE);

      CloseHandle(ClientProc);
    end
    else
    begin
      NewProcessHandle := ProcessInfo.hProcess;
      NewThreadHandle  := ProcessInfo.hThread;
    end;

  end
  else
  begin
    ResultValue := GetLastError;
    if ResultValue = 0 then;
    result := E_FAIL;
  end;

end;

initialization
  try
   TJwElevationClassFactory.Create(
    @ElevationDescription, true,
    ComServer, TJwRunElevated,
    CLASS_JwRunElevated,
    ciMultiInstance);
  except
  end;
end.
