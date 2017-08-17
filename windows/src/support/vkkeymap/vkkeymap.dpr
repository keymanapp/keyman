program vkkeymap;

uses
  Vcl.Forms,
  UfrmVKKeyMap in 'UfrmVKKeyMap.pas' {Form1},
  OnScreenKeyboard in '..\..\global\delphi\comp\OnScreenKeyboard.pas',
  OnScreenKeyboardData in '..\..\global\delphi\visualkeyboard\OnScreenKeyboardData.pas',
  VisualKeyboardParameters in '..\..\global\delphi\visualkeyboard\VisualKeyboardParameters.pas',
  ExtShiftState in '..\..\global\delphi\comp\ExtShiftState.pas',
  CleartypeDrawCharacter in '..\..\global\delphi\general\CleartypeDrawCharacter.pas',
  usp10 in '..\..\global\delphi\general\usp10.pas',
  GetOsVersion in '..\..\global\delphi\general\GetOsVersion.pas',
  Unicode in '..\..\global\delphi\general\Unicode.pas',
  Glossary in '..\..\global\delphi\general\Glossary.pas',
  ScanCodeMap in '..\..\global\delphi\general\ScanCodeMap.pas',
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas',
  RegistryKeys in '..\..\global\delphi\general\RegistryKeys.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  VKeys in '..\..\global\delphi\general\VKeys.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
