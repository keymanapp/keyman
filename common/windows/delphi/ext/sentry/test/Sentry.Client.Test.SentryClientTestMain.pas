unit Sentry.Client.Test.SentryClientTestMain;

interface

procedure main;

implementation

uses
  sentry,
  Sentry.Client,
  System.SysUtils;

procedure DoCrash; forward;
procedure DoEvent; forward;

procedure main;
begin
  if ParamStr(1) = 'crash'
    then DoCrash
    else DoEvent;
end;

function Init: TSentryClient;
var
  o: TSentryClientOptions;
begin
  o.Debug := False;
  o.DSN := 'https://92eb58e6005d47daa33c9c9e39458eb7@o1005580.ingest.sentry.io/5983518';
  //o.DSN := 'https://7b1ff1dae2c8495b84f90dadcf512b84@sentry.io/4853461';
  o.Release := 'keyman-14.0.22-alpha-local';
  o.HandlerPath := 'c:\Projects\keyman\app\windows\src\ext\sentry\test\Win32\Release\crashpad_handler.exe';
  o.DatabasePath := '.\sentry-'+SENTRY_SDK_VERSION+'-db';
  Result := TSentryClient.Create(o);
end;

type
  EFoo = class(Exception);

procedure DoCrash;
var
  c: TSentryClient;
begin
  c := Init;
  try
    try
      raise EFoo.Create('This should be captured by TSentryClient');
    except
      on E:Exception do
        SentryHandleException(E);
    end;
  finally
    c.Free;
  end;
end;

procedure DoEvent;
var
  c: TSentryClient;
begin
  c := Init;
  try
//    c.ExceptionEvent('fpp');
    c.MessageEvent(SENTRY_LEVEL_INFO, 'customer', 'Now testing with TSentryClient');
  finally
    c.Free;
  end;
end;

end.
