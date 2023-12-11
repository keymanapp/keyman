unit Keyman.System.KeymanSentryClient;

interface

uses
  System.Classes,
  System.SysUtils,
  Sentry.Client;

type
  TKeymanSentryClientProject = (kscpDesktop, kscpDeveloper);
  TKeymanSentryClientFlags = set of (kscfCaptureExceptions, kscfShowUI, kscfTerminate,
    kscfReportExceptions, kscfReportMessages);
  TKeymanSentryClient = class
  private
    class var FInstance: TKeymanSentryClient;
    class var FClient: TSentryClient;
    class var FOnBeforeShutdown: TNotifyEvent;
  private
    FFlags: TKeymanSentryClientFlags;
    FProject: TKeymanSentryClientProject;
    procedure ClientAfterEvent(Sender: TObject; EventType: TSentryClientEventType;
      const EventID, EventClassName, Message: string;
      var EventAction: TSentryClientEventAction);
    constructor Create(SentryClientClass: TSentryClientClass; AProject: TKeymanSentryClientProject; const ALogger: string; AFlags: TKeymanSentryClientFlags);
    procedure ReportRemoteErrors(const childEventID: string);
    procedure NestedValidate(Force: Boolean);
    procedure NestedValidateAccessViolation;
    procedure NestedValidateDelphiException;
    procedure NestedValidateFloatingPointException;
    class function GetEnabled: Boolean; static;
  public
    destructor Destroy; override;

    class procedure Validate(Force: Boolean = False);

    class procedure ReportHandledException(E: Exception; const Message: string = ''; IncludeStack: Boolean = True);
    class procedure ReportMessage(const Message: string; IncludeStack: Boolean = True);

    class procedure Breadcrumb(const BreadcrumbType, Message: string; const Category: string = ''; const Level: string = 'info');

    class procedure Start(SentryClientClass: TSentryClientClass; AProject: TKeymanSentryClientProject; const ALogger: string; AFlags: TKeymanSentryClientFlags);
    class procedure Stop;
    class property Client: TSentryClient read FClient;
    class property Enabled: Boolean read GetEnabled;
    class property Instance: TKeymanSentryClient read FInstance;
    class property OnBeforeShutdown: TNotifyEvent read FOnBeforeShutdown write FOnBeforeShutdown;
  public
    const LOGGER_DEVELOPER_IDE = 'KeymanDeveloper.IDE';
    const LOGGER_DEVELOPER_TOOLS = 'KeymanDeveloper.Tools';
    const LOGGER_DESKTOP = 'KeymanWindows';
    const LOGGER_DESKTOP_ENGINE = 'KeymanWindows.Engine';
    const S_Sentry_ViewEvent_URL = 'https://sentry.io/organizations/keyman/projects/%0:s/events/%1:s/'; // Do not localize
  end;

function LoadKeymanDesktopSentryFlags(Default: TKeymanSEntryClientFlags = [kscfCaptureExceptions, kscfShowUI, kscfTerminate]): TKeymanSentryClientFlags;

implementation

uses
  System.Generics.Collections,
  System.JSON,
  System.StrUtils,
  System.Win.ComObj,
  System.Win.Registry,
{$IF NOT DEFINED(CONSOLE)}
{$IF NOT DEFINED(SENTRY_NOVCL)}
  System.UITypes,
  Vcl.Dialogs,
  Vcl.Forms,
{$ENDIF}
{$ENDIF}

  sentry,

  KeymanPaths,
  KeymanVersion,
  RegistryKeys,
  utilexecute;

const
  SENTRY_DSN_DESKTOP = 'https://92eb58e6005d47daa33c9c9e39458eb7@o1005580.ingest.sentry.io/5983518';
  SENTRY_DSN_DEVELOPER = 'https://39b25a09410349a58fe12aaf721565af@o1005580.ingest.sentry.io/5983519';

  SENTRY_PROJECT_NAME_DESKTOP = 'keyman-windows';
  SENTRY_PROJECT_NAME_DEVELOPER = 'keyman-developer';

  //SENTRY_DSN = 'https://7b1ff1dae2c8495b84f90dadcf512b84@sentry.io/4853461'; <-- this is a testing-only host

  // Settings valid on development machines only:
  KEYMAN_ROOT = 'KEYMAN_ROOT'; // environment variable

{$IFDEF WIN64}
  DEV_SENTRY_PATH = 'common\windows\delphi\ext\sentry\sentry.x64.dll';
{$ELSE WIN64}
  DEV_SENTRY_PATH = 'common\windows\delphi\ext\sentry\sentry.dll';
{$ENDIF}

{ TKeymanSentryClient }

function FindSentryDLL: string;
var
  keyman_root: string;
begin
  // If we are in a development situation, use the dev version
  // of the DLL. Beware mixing and matching which can happen
  // when testing DLLs that are registered and loaded from
  // Program Files if Keyman is installed.
  if TKeymanPaths.RunningFromSource(keyman_root) then
  begin
    Result := keyman_root + DEV_SENTRY_PATH;
    if FileExists(Result) then
      Exit;
  end;

  // When installed, it should be in the sentry-x.y.z folder for the executable
  Result := ExtractFilePath(ParamStr(0)) + '\sentry-'+SENTRY_SDK_VERSION+'\' + sentry_dll;
  if FileExists(Result) then Exit;

  Result := '';
end;

class procedure TKeymanSentryClient.Breadcrumb(const BreadcrumbType, Message,
  Category, Level: string);
begin
  if Enabled then
    Client.Breadcrumb(BreadcrumbType, Message, Category, Level);
end;

procedure TKeymanSentryClient.ClientAfterEvent(Sender: TObject;
  EventType: TSentryClientEventType; const EventID, EventClassName, Message: string;
  var EventAction: TSentryClientEventAction);
var
  AppID, ProjectName: string;
{$IF NOT DEFINED(CONSOLE)}
  ApplicationTitle, CommandLine: string;
  tsysinfopath, enginepath: string;
  keyman_root: string;
{$ENDIF}
begin
  if EventType = scetException then
  begin
    // We need to look for a kmcomapi errlog and report that
    if FClient.ReportExceptions then
      ReportRemoteErrors(EventID);

    if Assigned(FOnBeforeShutdown) then
      FOnBeforeShutdown(Self);

    if kscfShowUI in FFlags then
    begin
      AppID := LowerCase(ChangeFileExt(ExtractFileName(ParamStr(0)), ''))+'-'+CKeymanVersionInfo.VersionWithTag;

      if FProject = kscpDesktop
        then ProjectName := SENTRY_PROJECT_NAME_DESKTOP
        else ProjectName := SENTRY_PROJECT_NAME_DEVELOPER;

{$IF DEFINED(CONSOLE)}
      // Write to console
      writeln(ErrOutput, 'Fatal error '+EventClassName+': '+Message);
      if FClient.ReportExceptions then
      begin
        writeln(ErrOutput, 'This error has been automatically reported to the Keyman team.');
        writeln(ErrOutput, '  Identifier:  '+EventID);
        writeln(ErrOutput, '  Application: '+AppID);
        writeln(ErrOutput, '  Reported at: '+Format(S_Sentry_ViewEvent_URL, [ProjectName, EventID]));
      end;
      writeln(ErrOutput);
{$ELSE}
      // Launch external gui exception dialog app.
      // Usage: tsysinfo -c <crashid> <appname> <appid> [sentryprojectname [classname [message]]]
{$IF NOT DEFINED(SENTRY_NOVCL)}
      if Assigned(Application)
        then ApplicationTitle := Application.Title
        else ApplicationTitle := AppID;
{$ELSE}
      ApplicationTitle := AppID;
{$ENDIF}

      CommandLine := Format('-c "%s" "%s" "%s" "%s" "%s" "%s" %s', [
        IfThen(EventID = '', '_', EventID),
        IfThen(ApplicationTitle = '', ChangeFileExt(ExtractFileName(ParamStr(0)),''), ApplicationTitle),
        AppID,
        ProjectName,
        EventClassName,
        StringReplace(Message, '"', '""', [rfReplaceAll]),
        IfThen(FClient.ReportExceptions, 'report', 'no-report')
      ]);

      if TKeymanPaths.RunningFromSource(keyman_root) then
      begin
        enginepath := keyman_root + 'windows\bin\engine';
        tsysinfopath := enginepath + '\tsysinfo.exe';
      end
      else if FileExists(ExtractFilePath(ParamStr(0)) + 'tsysinfo\tsysinfo.exe') then
      begin
        enginepath := ExtractFilePath(ParamStr(0)) + 'tsysinfo';
        tsysinfopath := enginepath + '\tsysinfo.exe';
      end
      else
      begin
        try
          tsysinfopath := TKeymanPaths.KeymanEngineInstallPath('tsysinfo.exe');
          enginepath := TKeymanPaths.KeymanEngineInstallPath('');
        except
          on E:EKeymanPath do
          begin
            tsysinfopath := '';
            enginepath := '';
          end;
        end;
      end;
      if (tsysinfopath = '') or not TUtilExecute.Shell(0, tsysinfopath, enginepath, CommandLine) then
      begin
{$IF NOT DEFINED(SENTRY_NOVCL)}
        MessageDlg(Application.Title+' has had a fatal error '+EventID+'. '+
          'This error has been automatically reported to the Keyman team.', mtError, [mbOK], 0);
{$ENDIF}
      end;
{$ENDIF}
    end;
    // Abort process: we'll get TSentryClient to clean up and crash
    if kscfTerminate in FFlags then
      EventAction := sceaTerminate;
  end;
end;

class procedure TKeymanSentryClient.ReportHandledException(E: Exception;
  const Message: string = ''; IncludeStack: Boolean = True);
var
  text: string;
begin
  if not Enabled then
    Exit;

  if Message <> ''
    then text := Message + ': '
    else text := '';

  if E is EOleException then
    text := Format('%s(%x): %s%s', [E.ClassName, (E as EOleException).ErrorCode, text, E.Message])
  else if E is EOSError then
    text := Format('%s(%d): %s%s', [E.ClassName, (E as EOSError).ErrorCode, text, E.Message])
  else
    text := Format('%s: %s%s', [E.ClassName, text, E.Message]);

  Client.MessageEvent(TSentryLevel.SENTRY_LEVEL_WARNING, text, IncludeStack);
end;

class procedure TKeymanSentryClient.ReportMessage(const Message: string; IncludeStack: Boolean);
begin
  if not Enabled then
    Exit;

  Client.MessageEvent(TSentryLevel.SENTRY_LEVEL_WARNING, Message, IncludeStack);
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

constructor TKeymanSentryClient.Create(SentryClientClass: TSentryClientClass; AProject: TKeymanSentryClientProject; const ALogger: string; AFlags: TKeymanSentryClientFlags);
var
  o: TSentryClientOptions;
  f: TSentryClientFlags;
  path: string;
begin
  Assert(not Assigned(FInstance));

  FInstance := Self;
  FProject := AProject;
  FFlags := AFlags;

  if not Enabled then
    Exit;

  path := FindSentryDLL;
  if path = '' then
    // Cannot find sentry.dll
    Exit;

  sentry_set_library_path(path);

  o.Debug := False;

  if AProject = kscpDesktop
    then o.DSN := SENTRY_DSN_DESKTOP
    else o.DSN := SENTRY_DSN_DEVELOPER;

  // Note: system proxy is used automatically if no proxy is defined

  o.Release := CKeymanVersionInfo.VersionGitTag; // matches git tag
  o.Environment := CKeymanVersionInfo.Environment; // stable, beta, alpha, test, local

  if kscfCaptureExceptions in FFlags
    then f := [scfCaptureExceptions]
    else f := [];

  // Load the registry settings for privacy settings

  if kscfReportExceptions in FFlags
    then Include(f, scfReportExceptions);

  if kscfReportMessages in FFlags
    then Include(f, scfReportMessages);

  o.HandlerPath := ExtractFilePath(path) + 'crashpad_handler.exe';
  o.DatabasePath := TKeymanPaths.ErrorLogPath + 'sentry-'+SENTRY_SDK_VERSION+'-db';

  FClient := SentryClientClass.Create(o, ALogger, f);
  FClient.OnAfterEvent := ClientAfterEvent;
  // We used the 'Started' event when testing Sentry integration in 14.0 alpha
  // but we don't want or need it for stable.
  // FClient.MessageEvent(Sentry.Client.SENTRY_LEVEL_INFO, 'Started '+ALogger);
end;

destructor TKeymanSentryClient.Destroy;
begin
  if FInstance = Self then
    FInstance := nil;
  FreeAndNil(FClient);
  inherited Destroy;
end;

class function TKeymanSentryClient.GetEnabled: Boolean;
begin
  Result := TSentryClient.Enabled;
end;

class procedure TKeymanSentryClient.Start(SentryClientClass: TSentryClientClass; AProject: TKeymanSentryClientProject; const ALogger: string; AFlags: TKeymanSentryClientFlags);
begin
  TKeymanSentryClient.Create(SentryClientClass, AProject, ALogger, AFlags);
end;

class procedure TKeymanSentryClient.Stop;
begin
  FreeAndNil(FInstance);
end;

//
// With this function, we throw a test crash event to make sure that:
// a) exception hooking is in place
// b) events are correctly sent through
// c) symbolication is working
// d) privacy options are correctly checked
//
// We nest some function calls for a more expressive stack
//
class procedure TKeymanSentryClient.Validate(Force: Boolean);
begin
  if not Enabled then
    Exit;

  TKeymanSentryClient.Instance.NestedValidate(Force);
end;

procedure TKeymanSentryClient.NestedValidate(Force: Boolean);
begin
  if Force then
    NestedValidateDelphiException
  else if ParamStr(1) = '-sentry-client-test-exception' then
  begin
    // Undocumented test parameter
    if ParamStr(2) = '' then
      NestedValidateDelphiException
    else if ParamStr(2) = 'av' then
      NestedValidateAccessViolation
    else if ParamStr(2) = 'fp' then
      NestedValidateFloatingPointException;
  end;
end;

procedure TKeymanSentryClient.NestedValidateDelphiException;
begin
  raise ESentryTest.Create('Just testing Sentry');
end;

procedure TKeymanSentryClient.NestedValidateAccessViolation;
var
  p: PByte;
begin
  // Force an Access Violation
  p := nil;
  p^ := 0;
end;

procedure TKeymanSentryClient.NestedValidateFloatingPointException;
var
  n: Double;
begin
  // Force a floating point exception
{$IF DEFINED(CONSOLE)}
  writeln('Attempting FP Exception');
{$ENDIF}
  n := 0;
  n := 1 / n;
  writeln(n); // We'll never get here, but this stops a compiler warning
end;

// These functions may be moved elsewhere in the future, but in order to remove
// the registry load from TKeymanSentryClient, we'll start by extracting them
// from the constructor

function LoadKeymanDesktopSentryFlags(Default: TKeymanSentryClientFlags): TKeymanSentryClientFlags;
var
  reg: TRegistry;
begin
  Result := Default;

  reg := TRegistry.Create;
  try
    if reg.OpenKeyReadOnly(SRegKey_KeymanEngine_CU) then
    begin
      if not reg.ValueExists(SRegValue_AutomaticallyReportErrors) or
          reg.ReadBool(SRegValue_AutomaticallyReportErrors) then
        Include(Result, kscfReportExceptions);
      if not reg.ValueExists(SRegValue_AutomaticallyReportUsage) or
          reg.ReadBool(SRegValue_AutomaticallyReportUsage) then
        Include(Result, kscfReportMessages);
    end
    else
    begin
      Include(Result, kscfReportExceptions);
      Include(Result, kscfReportMessages);
    end;
  finally
    reg.Free;
  end;
end;


end.
