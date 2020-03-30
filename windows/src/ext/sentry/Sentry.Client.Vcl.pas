unit Sentry.Client.Vcl;

interface

uses
  System.SysUtils,
  Sentry.Client;

type
  TSentryClientVcl = class(TSentryClient)
  private
    procedure HandleApplicationException(Sender: TObject; E: Exception);
  public
    constructor Create(AOptions: TSentryClientOptions; const ALogger: string; AFlags: TSentryClientFlags); override;
    destructor Destroy; override;
  end;

implementation

uses
  Vcl.Forms;

{ TSentryClientVcl }

constructor TSentryClientVcl.Create(AOptions: TSentryClientOptions;
  const ALogger: string; AFlags: TSentryClientFlags);
begin
  inherited Create(AOptions, ALogger, AFlags);

  if scfCaptureExceptions in AFlags then
  begin
    Application.OnException := HandleApplicationException;
  end;
end;

destructor TSentryClientVcl.Destroy;
begin
  inherited Destroy;
  if (TMethod(Application.OnException).Code = @TSentryClientVcl.HandleApplicationException)
      and (TMethod(Application.OnException).Data = Self) then
    Application.OnException := nil;
end;

procedure TSentryClientVcl.HandleApplicationException(Sender: TObject; E: Exception);
begin
  SentryHandleException(E);
end;

end.
