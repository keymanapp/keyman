unit uLogging;

interface
uses Classes, SysUtils, JwsclLogging, FileCtrl, JwsclStrings,
  JwsclToken;

//inits LogServer
procedure InitLog;
{@Name shuts down logging and write log file to LogFileNameLocation.
Make sure all threads are finished.
}
procedure DoneLog;

{@Name turns on/off logging. Events that were logged
before turn on logging are lost.}
procedure SwitchLog(const Enabled : Boolean);


{@Name calls RaiseLastOsError and logs it.}
procedure LogAndRaiseLastOsError(Log : IJwLogClient;
    const ClassName, MethodName, FileName : TJwString);

procedure InitFileLocation;

var //log filename + path
    LogFileNameLocation : WideString = '';
    LogFilePath : String = '';

    ApplicationFileName : String = '';


    {
    Type of events that are logged.
    If this array is empty all events are logged.
    }
    LogEventTypes : TJwEventTypes = nil;

    {
    Main Log Server.
    Use LogServer.Connect to get a log interface to start logging events
    }
    LogServer : IJwLogServer = nil;

const LogFileKey = 'Log';

implementation
uses Registry, JwaWindows;

var Strings : TStringList;

procedure LogAndRaiseLastOsError(Log : IJwLogClient;
    const ClassName, MethodName, FileName : TJwString);
begin
  try
    RaiseLastOSError;
  except
    on E : Exception do
    begin
      Log.Exception(E);
      raise;
    end;
  end;
end;

procedure InitLog;
begin
  Strings := TStringList.Create;
  Strings.Add('<?xml version="1.0"?>');
  Strings.Add('<logfile version="1.0" name="XPElevation">');
  LogServer := CreateLogServer(Strings, LogEventTypes);
end;

procedure DoneLog;
begin
  LogServer.Done;
  Strings.Add('</logfile>');
  try
    Strings.SaveToFile(LogFileNameLocation);
  finally
    Strings.Free;
  end;
end;

procedure SwitchLog(const Enabled : Boolean);
begin
  if Assigned(LogServer) then
    SetEventTypesEnabled(LogServer, Enabled);
end;


function RegGetFullPath(PathKey: string): string;
var Reg: TRegistry; Unresolved: string;
begin
  Reg:=TRegistry.Create(KEY_QUERY_VALUE);
  try
    Reg.RootKey:=HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('Software\XPElevation\Paths\', false) then
    try
      Unresolved:=Reg.ReadString(PathKey);
      SetLength(Result, MAX_PATH+1);
      ExpandEnvironmentStrings(PChar(Unresolved), @Result[1], MAX_PATH+1);
      SetLength(Result, StrLen(PChar(Result)));
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;



procedure InitFileLocation;
var S, FileName: String;
    F : TextFile;
    Token : TJwSecurityToken;
begin
  Token := TJwSecurityToken.CreateTokenEffective(TOKEN_READ or TOKEN_QUERY);
  try
    FileName := Token.GetTokenUserName + IntToStr(JwGetProcessLogonSession)
  except
    FileName := IntToStr(JwGetProcessLogonSession);
  end;
  Token.Free;
  if JwIsSystem then
    FileName := 'SYSTEM'
  else
  if JwCheckAdministratorAccess then
    FileName := FileName + '_Admin';


  if (Length(LogFilePath) > 0) and DirectoryExists(LogFilePath) then
    LogFileNameLocation := LogFilePath  
  else
    LogFileNameLocation := ExtractFilePath(ParamStr(0));
  LogFileNameLocation := IncludeTrailingBackslash(LogFileNameLocation);

  LogFileNameLocation := Format('%s_%s%s_%s.log',
      [ApplicationFileName, LogFileNameLocation, FileName, FormatDateTime('dd_mm_yyyy__hh_nn_ss', now)]);
end;


end.
