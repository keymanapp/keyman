program SentryClientTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Sentry.Client.Test.SentryClientTestMain in 'Sentry.Client.Test.SentryClientTestMain.pas',
  Sentry.Client in '..\Sentry.Client.pas',
  sentry in '..\sentry.pas',
  Sentry.Client.Console in '..\Sentry.Client.Console.pas';

begin
  try
    main;
  except
    on E: Exception do
    begin
//       Note: in this project we should never get here
//       exceptions should always be captured by Sentry
      Writeln('UNEXPECTED '+E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
