program vkkeymap;

uses
  Vcl.Forms,
  UfrmVKKeyMap in 'UfrmVKKeyMap.pas' {Form1},
  OnScreenKeyboard in '..\..\..\..\common\windows\delphi\components\OnScreenKeyboard.pas',
  OnScreenKeyboardData in '..\..\..\..\common\windows\delphi\visualkeyboard\OnScreenKeyboardData.pas',
  VisualKeyboardParameters in '..\..\..\..\common\windows\delphi\visualkeyboard\VisualKeyboardParameters.pas',
  ExtShiftState in '..\..\..\..\common\windows\delphi\visualkeyboard\ExtShiftState.pas',
  CleartypeDrawCharacter in '..\..\..\..\common\windows\delphi\general\CleartypeDrawCharacter.pas',
  usp10 in '..\..\..\..\common\windows\delphi\general\usp10.pas',
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
