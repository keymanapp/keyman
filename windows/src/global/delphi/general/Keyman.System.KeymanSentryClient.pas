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
    procedure ClientAfterEvent(Sender: TObject; EventType: TSentryClientEventType;
      const EventClassName, Message: string;
      var EventAction: TSentryClientEventAction);
    constructor Create(SentryClientClass: TSentryClientClass; AProject: TKeymanSentryClientProject; AFlags: TKeymanSentryClientFlags);
  public
    destructor Destroy; override;
    class procedure Start(SentryClientClass: TSentryClientClass; AProject: TKeymanSentryClientProject; AFlags: TKeymanSentryClientFlags = [kscfCaptureExceptions, kscfShowUI, kscfTerminate]);
    class procedure Stop;
    class property Client: TSentryClient read FClient;
    class property Instance: TKeymanSentryClient read FInstance;
  end;


implementation

uses
  sentry,

  System.SysUtils,

  KeymanPaths,
  KeymanVersion,
  utilexecute;

const
  SENTRY_DSN_DESKTOP = 'https://92eb58e6005d47daa33c9c9e39458eb7@sentry.keyman.com/5';
  SENTRY_DSN_DEVELOPER = 'https://92eb58e6005d47daa33c9c9e39458eb7@sentry.keyman.com/6';
  //SENTRY_DSN = 'https://7b1ff1dae2c8495b84f90dadcf512b84@sentry.io/4853461'; <-- this is a testing-only host

  // Settings valid on development machines only:
  KEYMAN_ROOT = 'KEYMAN_ROOT'; // environment variable
  DEV_SENTRY_PATH = 'windows\src\ext\sentry\sentry.dll';

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
  EventType: TSentryClientEventType; const EventClassName, Message: string;
  var EventAction: TSentryClientEventAction);
begin
  if EventType = scetException then
  begin
    if kscfShowUI in FFlags then
    begin
{$IF DEFINED(CONSOLE)}
      // Write to console
      writeln('Fatal error '+EventClassName+': '+Message);
      writeln;
{$ELSE}
      // Launch external gui exception handler
      if not TUtilExecute.Shell(0, TKeymanPaths.KeymanEngineInstallPath('tsysinfo.exe'),  // I3349
          TKeymanPaths.KeymanEngineInstallPath(''), '-c') then
      begin
        {$MESSAGE HINT 'Show a message before aborting here?'}
        {MessageDlg(Application.Title+' has had a fatal error.  An additional error was encountered starting the exception manager ('+SysErrorMessage(GetLastError)+').  '+
          #13#10'Error log is stored in '#13#10#13#10+'  '+FLogFile+#13#10#13#10+
          message+#13#10+
          detail+#13#10+
          'Please send this information to Keyman Support',
          mtError, [mbOK], 0);}
      end;
{$ENDIF}
    end;
    // Abort process: we'll get TSentryClient to clean up and crash
    if kscfTerminate in FFlags then
      EventAction := sceaTerminate;
  end;
end;

constructor TKeymanSentryClient.Create(SentryClientClass: TSentryClientClass; AProject: TKeymanSentryClientProject; AFlags: TKeymanSentryClientFlags);
var
  o: TSentryClientOptions;
begin
  Assert(not Assigned(FInstance));

  FInstance := Self;
  FFlags := AFlags;

  sentry_set_library_path(FindSentryDLL);

  o.Debug := False;

  if AProject = kscpDesktop
    then o.DSN := SENTRY_DSN_DESKTOP
    else o.DSN := SENTRY_DSN_DEVELOPER;

  {$MESSAGE HINT 'Support proxies'}

  o.Release := 'keyman-14.0.22-alpha-local';
  //o.Release := 'release-'+SKeymanVersionWithTag;

  FClient := SentryClientClass.Create(o, kscfCaptureExceptions in FFlags);
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
