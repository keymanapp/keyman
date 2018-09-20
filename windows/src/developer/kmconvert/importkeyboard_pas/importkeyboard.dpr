program importkeyboard;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Keyman.Developer.System.ImportKeyboardDLL in 'Keyman.Developer.System.ImportKeyboardDLL.pas',
  VKeys in '..\..\..\global\delphi\general\VKeys.pas',
  ScanCodeMap in '..\..\..\global\delphi\general\ScanCodeMap.pas',
  Keyman.Developer.System.ImportKeyboardMain in 'Keyman.Developer.System.ImportKeyboardMain.pas';

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
