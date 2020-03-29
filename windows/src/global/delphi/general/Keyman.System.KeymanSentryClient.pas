unit Keyman.System.KeymanSentryClient;

interface

uses
  Sentry.Client;

type
  TKeymanSentryClientProject = (kscpDesktop, kscpDeveloper);
  TKeymanSentryClientFlags = set of (kscfCaptureExceptions, kscfShowUI, kscfTerminate);
  TKeymanSentryClient = class
  private
    class var FInstance: TKeymanSentryClient;
    class var FClient: TSentryClient;
  private
    FFlags: TKeymanSentryClientFlags;
    FProject: TKeymanSentryClientProject;
    procedure ClientAfterEvent(Sender: TObject; EventType: TSentryClientEventType;
      const EventID, EventClassName, Message: string;
      var EventAction: TSentryClientEventAction);
    constructor Create(SentryClientClass: TSentryClientClass; AProject: TKeymanSentryClientProject; AFlags: TKeymanSentryClientFlags);
    procedure ReportRemoteErrors(const childEventID: string);
  public
    destructor Destroy; override;
    class procedure Start(SentryClientClass: TSentryClientClass; AProject: TKeymanSentryClientProject; AFlags: TKeymanSentryClientFlags = [kscfCaptureExceptions, kscfShowUI, kscfTerminate]);
    class procedure Stop;
    class property Client: TSentryClient read FClient;
    class property Instance: TKeymanSentryClient read FInstance;
  public
    const LOGGER_DEVELOPER_IDE = 'KeymanDeveloper.IDE';
  end;


implementation

uses
  System.Classes,
  System.Generics.Collections,
  System.JSON,
  System.SysUtils,
  System.Win.Registry,
{$IF NOT DEFINED(CONSOLE)}
  System.UITypes,
  Vcl.Dialogs,
  Vcl.Forms,
{$ENDIF}

  sentry,

  KeymanPaths,
  KeymanVersion,
  RegistryKeys,
  utilexecute;

const
  SENTRY_DSN_DESKTOP = 'https://92eb58e6005d47daa33c9c9e39458eb7@sentry.keyman.com/5';
  SENTRY_DSN_DEVELOPER = 'https://39b25a09410349a58fe12aaf721565af@sentry.keyman.com/6';

  SENTRY_PROJECT_NAME_DESKTOP = 'keyman-desktop';
  SENTRY_PROJECT_NAME_DEVELOPER = 'keyman-developer';

  //SENTRY_DSN = 'https://7b1ff1dae2c8495b84f90dadcf512b84@sentry.io/4853461'; <-- this is a testing-only host

  // Settings valid on development machines only:
  KEYMAN_ROOT = 'KEYMAN_ROOT'; // environment variable

{$IFDEF WIN64}
  DEV_SENTRY_PATH = 'windows\src\ext\sentry\sentry.x64.dll';
{$ELSE WIN64}
  DEV_SENTRY_PATH = 'windows\src\ext\sentry\sentry.dll';
{$ENDIF}

{ TKeymanSentryClient }

function FindSentryDLL: string;
begin
  // It might be in same folder as executable (probably true for Developer)
  Result := ExtractFilePath(ParamStr(0)) + sentry_dll;
  if FileExists(Result) then Exit;

  // Keyman Engine install dir or registry override next (for Engine, Desktop)
  Result := TKeymanPaths.KeymanEngineInstallPath(sentry_dll);
  if (Result <> '') and FileExists(Result) then Exit;

  // Last gasp, if we are in a development situation
  Result := GetEnvironmentVariable(KEYMAN_ROOT);
  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(Result) + DEV_SENTRY_PATH;
end;

procedure TKeymanSentryClient.ClientAfterEvent(Sender: TObject;
  EventType: TSentryClientEventType; const EventID, EventClassName, Message: string;
  var EventAction: TSentryClientEventAction);
{$IF NOT DEFINED(CONSOLE)}
var
  AppID, ApplicationTitle, CommandLine, ProjectName: string;
{$ENDIF}
begin
  if EventType = scetException then
  begin
    // We need to look for a kmcomapi errlog and report that
    if FClient.ReportExceptions then
      ReportRemoteErrors(EventID);

    if kscfShowUI in FFlags then
    begin
{$IF DEFINED(CONSOLE)}
      // Write to console
      writeln(ErrOutput, 'Fatal error '+EventClassName+': '+Message);
      if FClient.ReportExceptions then
        writeln(ErrOutput, 'This error has been automatically reported to the Keyman team.');
      writeln(ErrOutput);
{$ELSE}
      // Launch external gui exception dialog app.
      // Usage: tsysinfo -c <crashid> <appname> <appid> [sentryprojectname [classname [message]]]
      AppID := LowerCase(ChangeFileExt(ExtractFileName(ParamStr(0)), ''))+'-'+CKeymanVersionInfo.VersionWithTag;
      if Assigned(Application)
        then ApplicationTitle := Application.Title
        else ApplicationTitle := AppID;

      if FProject = kscpDesktop
        then ProjectName := SENTRY_PROJECT_NAME_DESKTOP
        else ProjectName := SENTRY_PROJECT_NAME_DEVELOPER;


      CommandLine := Format('-c "%s" "%s" "%s" "%s" "%s" "%s"', [
        EventID,
        ApplicationTitle,
        AppID,
        ProjectName,
        EventClassName,
        StringReplace(Message, '"', '""', [rfReplaceAll])
      ]);

      if not TUtilExecute.Shell(0, TKeymanPaths.KeymanEngineInstallPath('tsysinfo.exe'),  // I3349
          TKeymanPaths.KeymanEngineInstallPath(''), CommandLine) then
      begin
        MessageDlg(Application.Title+' has had a fatal error.  An additional error was encountered '+
          'starting the exception manager ('+SysErrorMessage(GetLastError)+'). '+
          'This error has been automatically reported to the Keyman team.', mtError, [mbOK], 0);
      end;
{$ENDIF}
    end;
    // Abort process: we'll get TSentryClient to clean up and crash
    if kscfTerminate in FFlags then
      EventAction := sceaTerminate;
  end;
end;

procedure TKeymanSentryClient.ReportRemoteErrors(const childEventID: string);
var
  errlogfile: string;
  o: TJSONObject;
  event: sentry_value_t;
  stack: TJSONArray;
  i: Integer;
  frames: sentry_value_t;
  stacktrace: sentry_value_t;
  threads: sentry_value_t;
  thread: sentry_value_t;
  frame: sentry_value_t;
begin
  errlogfile := TKeymanPaths.ErrorLogPath('kmcomapi');  // I2824
  if FileExists(errlogfile) then
  begin
    // We'll use the Sentry API directly here to construct a crash report from
    // kmcomapi.
    with TStringStream.Create('', TEncoding.UTF8) do
    try
      LoadFromFile(errlogfile);
      o := TJSONObject.ParseJSONValue(DataString) as TJSONObject;
    finally
      Free;
    end;

    DeleteFile(errlogfile);

(*
  When we set exception information, the report is corrupted. Not sure why. So
  for now we won't create as an exception event. We still get all the information
  we want from this.

  Investigating this further at https://forum.sentry.io/t/corrupted-display-when-exception-data-is-set-using-native-sdk/9167/2

  exc := sentry_value_new_object;
  sentry_value_set_by_key(exc, 'type', sentry_value_new_string(PAnsiChar(UTF8Encode(ExceptionClassName))));
  sentry_value_set_by_key(exc, 'value', sentry_value_new_string(PAnsiChar(UTF8Encode(Message))));
  sentry_value_set_by_key(event, 'exception', exc);
*)

    event := sentry_value_new_event;
    sentry_value_set_by_key(event, 'message', sentry_value_new_string(PAnsiChar(UTF8Encode(o.Values['message'].Value))));
    // Construct the stack trace

    stack := o.Values['stack'] as TJSONArray;

    frames := sentry_value_new_list;

    for i := 0 to stack.Count - 1 do
    begin
      frame := sentry_value_new_object;
      sentry_value_set_by_key(frame, 'instruction_addr', sentry_value_new_string(PAnsiChar(AnsiString(stack.Items[i].Value))));
      sentry_value_append(frames, frame);
    end;

    stacktrace := sentry_value_new_object;
    sentry_value_set_by_key(stacktrace, 'frames', frames);

    threads := sentry_value_new_list;
    thread := sentry_value_new_object;
    sentry_value_set_by_key(thread, 'stacktrace', stacktrace);
    sentry_value_append(threads, thread);

    sentry_value_set_by_key(event, 'threads', threads);

    sentry_set_extra('child_event', sentry_value_new_string(PAnsiChar(AnsiString(childEventID))));

    sentry_capture_event(event);
  end;
end;

constructor TKeymanSentryClient.Create(SentryClientClass: TSentryClientClass; AProject: TKeymanSentryClientProject; AFlags: TKeymanSentryClientFlags);
var
  reg: TRegistry;
  o: TSentryClientOptions;
  f: TSentryClientFlags;
  RegKey: string;
begin
  Assert(not Assigned(FInstance));

  FInstance := Self;
  FProject := AProject;
  FFlags := AFlags;

  sentry_set_library_path(FindSentryDLL);

  o.Debug := False;

  if AProject = kscpDesktop
    then o.DSN := SENTRY_DSN_DESKTOP
    else o.DSN := SENTRY_DSN_DEVELOPER;

  // Note: system proxy is used automatically if no proxy is defined

  o.Release := 'release-'+CKeymanVersionInfo.VersionWithTag;

  if kscfCaptureExceptions in FFlags
    then f := [scfCaptureExceptions]
    else f := [];

  // Load the registry settings for privacy settings

  if AProject = kscpDesktop
    then RegKey := SRegKey_KeymanEngine_CU
    else RegKey := SRegKey_IDEOptions_CU;

  reg := TRegistry.Create;
  try
    if reg.OpenKeyReadOnly(RegKey) then
    begin
      if not reg.ValueExists(SRegValue_AutomaticallyReportErrors) or
          reg.ReadBool(SRegValue_AutomaticallyReportErrors) then
        Include(f, scfReportExceptions);
      if not reg.ValueExists(SRegValue_AutomaticallyReportUsage) or
          reg.ReadBool(SRegValue_AutomaticallyReportUsage) then
        Include(f, scfReportMessages);
    end
    else
    begin
      Include(f, scfReportExceptions);
      Include(f, scfReportMessages);
    end;
  finally
    reg.Free;
  end;

  FClient := SentryClientClass.Create(o, f);
  FClient.OnAfterEvent := ClientAfterEvent;
end;

destructor TKeymanSentryClient.Destroy;
begin
  if FInstance = Self then
    FInstance := nil;
  FreeAndNil(FClient);
  inherited Destroy;
end;

class procedure TKeymanSentryClient.Start(SentryClientClass: TSentryClientClass; AProject: TKeymanSentryClientProject; AFlags: TKeymanSentryClientFlags);
begin
  TKeymanSentryClient.Create(SentryClientClass, AProject, AFlags);
end;

class procedure TKeymanSentryClient.Stop;
begin
  FreeAndNil(FInstance);
end;

end.
