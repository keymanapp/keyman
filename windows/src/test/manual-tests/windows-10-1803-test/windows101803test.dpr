program windows101803test;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Winapi.Windows,
  Keyman.System.IsWin10_1830_OrGreater in 'Keyman.System.IsWin10_1830_OrGreater.pas';

begin
  try
    WriteOSVersionInfo;
    if IsWindows10_1803_OrGreater
      then writeln('Windows version is 10.0 1803 or later')
      else writeln('Windows version is earlier than 10.0 1803');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
