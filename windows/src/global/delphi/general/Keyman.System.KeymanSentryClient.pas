unit Keyman.System.KeymanSentryClient;

interface

uses
  Sentry.Client;

type
  TKeymanSentryClient = class
  private
    class var FInstance: TKeymanSentryClient;
    class var FClient: TSentryClient;
  private
    constructor Create(SentryClientClass: TSentryClientClass; ACaptureExceptions: Boolean);
  public
    destructor Destroy; override;
    class procedure Start(SentryClientClass: TSentryClientClass; ACaptureExceptions: Boolean = True);
    class procedure Stop;
    class property Client: TSentryClient read FClient;
    class property Instance: TKeymanSentryClient read FInstance;
  end;


implementation

uses
  sentry,

  System.SysUtils,

  KeymanPaths,
  KeymanVersion;

{ TKeymanSentryClient }

function FindSentryDLL: string;
const
  KEYMAN_ROOT = 'KEYMAN_ROOT';
  DEV_SENTRY_PATH = 'windows\src\ext\sentry\sentry.dll';
begin
  // We'll check a few places -- Keyman Engine install dir or registry override first
  Result := TKeymanPaths.KeymanEngineInstallPath(sentry_dll);
  if (Result <> '') and FileExists(Result) then Exit;

  // It might be in same folder as executable
  Result := ExtractFilePath(ParamStr(0)) + sentry_dll;
  if FileExists(Result) then Exit;

  // Last gasp, if we are in a development situation
  Result := IncludeTrailingPathDelimiter(GetEnvironmentVariable(KEYMAN_ROOT)) + DEV_SENTRY_PATH;

end;

constructor TKeymanSentryClient.Create(SentryClientClass: TSentryClientClass; ACaptureExceptions: Boolean);
var
  o: TSentryClientOptions;
begin
  Assert(not Assigned(FInstance));

  FInstance := Self;

  sentry_set_library_path(FindSentryDLL);

  o.Debug := False;
  o.DSN := 'https://92eb58e6005d47daa33c9c9e39458eb7@sentry.keyman.com/5';
  //o.DSN := 'https://7b1ff1dae2c8495b84f90dadcf512b84@sentry.io/4853461';

  o.Release := 'keyman-14.0.22-alpha-local';
  {$MESSAGE HINT 'Update o.Release to correct value'}
//  o.Release := 'release-'+SKeymanVersionWithTag;

  // We don't currently use crashpad:
  //o.HandlerPath := // 'c:\Projects\keyman\app\windows\src\ext\sentry\test\Win32\Release\crashpad_handler.exe';
  //o.DatabasePath := '.\sentry-db';

  FClient := SentryClientClass.Create(o, ACaptureExceptions);
end;

destructor TKeymanSentryClient.Destroy;
begin
  if FInstance = Self then
    FInstance := nil;
  inherited Destroy;
end;

class procedure TKeymanSentryClient.Start(SentryClientClass: TSentryClientClass; ACaptureExceptions: Boolean = True);
begin
  TKeymanSentryClient.Create(SentryClientClass, ACaptureExceptions);
end;

class procedure TKeymanSentryClient.Stop;
begin
  FreeAndNil(FInstance);
end;

end.
