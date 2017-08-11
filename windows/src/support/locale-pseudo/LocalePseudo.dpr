program LocalePseudo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.Win.ComObj,
  Winapi.ActiveX,
  System.SysUtils,
  MakePseudoLocaleXml in 'MakePseudoLocaleXml.pas',
  MSXMLDomCreate in '..\..\global\delphi\general\MSXMLDomCreate.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas';

begin
  try
    CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
    Run;
    CoUninitialize;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
