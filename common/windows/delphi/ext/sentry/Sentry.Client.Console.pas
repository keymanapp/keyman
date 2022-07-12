unit Sentry.Client.Console;

interface

uses
  Sentry.Client;

type
  TSentryClientConsole = class(TSentryClient)
  public
    constructor Create(AOptions: TSentryClientOptions; const ALogger: string; AFlags: TSentryClientFlags); override;
    destructor Destroy; override;
  end;

implementation

{ TSentryClientConsole }

constructor TSentryClientConsole.Create(AOptions: TSentryClientOptions;
  const ALogger: string; AFlags: TSentryClientFlags);
begin
  inherited;

end;

destructor TSentryClientConsole.Destroy;
begin

  inherited;
end;

end.
