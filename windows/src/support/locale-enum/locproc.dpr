program locproc;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  elp in 'elp.pas',
  Unicode in 'C:\Projects\keyman\open\windows\src\global\delphi\general\Unicode.pas',
  utilstr in 'C:\Projects\keyman\open\windows\src\global\delphi\general\utilstr.pas';

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
