unit uLogging;

interface
uses Classes, SysUtils, JwsclLogging, FileCtrl, JwsclStrings;

//inits LogServer
procedure InitLog;
{@Name shuts down logging and write log file to LogFileNameLocation.
Make sure all threads are finished.
}
procedure DoneLog;

{@Name turns on/off logging. Events that were logged
before turn on logging are lost.}
procedure SwitchLog(const Enabled : Boolean);




procedure InitFileLocation;

{@Name calls RaiseLastOsError and logs it.}
procedure LogAndRaiseLastOsError(Log : IJwLogClient;
    const ClassName, MethodName, FileName : TJwString);



var //log filename + path
    LogFileNameLocation : WideString = '';
    LogFilePath : AnsiString = '';
    FileName : AnsiString = 'RunAsSys';

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
uses Registry, JwaWindows, JwsclToken, JwsclSecureObjects;

var
  Strings : TStringList;
  LogEnabled : Boolean = true;


function IsLogEnabled(out LogPath : AnsiString) : Boolean;
var Reg : TRegistry;
begin
  result := false;
  try
    Reg := TRegistry.Create(KEY_READ or KEY_ENUMERATE_SUB_KEYS or KEY_QUERY_VALUE);
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      if Reg.OpenKey('Software\JEDI\WSCL\RunAsSys\1.0', false) then
      begin
        try
          result := Reg.ReadBool('LogEnabled');
        except
          result := false;
        end;
        try
          LogPath := Reg.ReadString('LogPath');
        except
          LogPath := '';
        end;
      end;
    finally
      Reg.Free;
    end;
  except
  end;
end;

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
  SwitchLog(false);

  Strings := TStringList.Create;
  Strings.Add('<?xml version="1.0"?>');
  Strings.Add('<logfile version="1.0" name="RunAsSys">');
  LogServer := CreateLogServer(Strings, LogEventTypes);

  SwitchLog(IsLogEnabled(LogFilePath));
  uLogging.InitFileLocation;;
end;

procedure DoneLog;
begin
  if LogServer <> nil then
  begin
    LogServer.Done;
    Strings.Add('</logfile>');
    try
      if LogEnabled then
      try
        Strings.SaveToFile(LogFileNameLocation);
      except
      end;
    finally
      Strings.Free;
    end;
  end;
end;


procedure SwitchLog(const Enabled : Boolean);
begin
  if Assigned(LogServer) then
    SetEventTypesEnabled(LogServer, Enabled);
  LogEnabled := Enabled;
end;




procedure InitFileLocation;
var S : String;
    F : TextFile;
    Token : TJwSecurityToken;
begin
  Token := TJwSecurityToken.CreateTokenEffective(TOKEN_READ or TOKEN_QUERY);
  try
    FileName := Token.GetTokenUserName + IntToStr(JwGetProcessLogonSession)
  except
    FileName := 'RunAsUser';
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

  LogFileNameLocation := Format('%s%s_%s.log',
      [LogFileNameLocation, FileName, FormatDateTime('dd_mm_yyyy__hh_nn_ss', now)]);
end;

initialization



end.
