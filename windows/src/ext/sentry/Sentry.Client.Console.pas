unit Sentry.Client.Console;

interface

uses
  Sentry.Client;

type
  TSentryClientConsole = class(TSentryClient)
  public
    constructor Create(AOptions: TSentryClientOptions; ACaptureExceptions: Boolean = True); override;
    destructor Destroy; override;
  end;

implementation

{ TSentryClientConsole }

constructor TSentryClientConsole.Create(AOptions: TSentryClientOptions;
  ACaptureExceptions: Boolean);
begin
  inherited;

end;

destructor TSentryClientConsole.Destroy;
begin

  inherited;
end;

end.
