program oskbulkrenderer;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Keyman.System.OSKBulkRenderer in 'Keyman.System.OSKBulkRenderer.pas',
  OnScreenKeyboard in '..\..\..\..\common\windows\delphi\components\OnScreenKeyboard.pas',
  OnScreenKeyboardData in '..\..\..\..\common\windows\delphi\visualkeyboard\OnScreenKeyboardData.pas',
  VisualKeyboard in '..\..\..\..\common\windows\delphi\visualkeyboard\VisualKeyboard.pas',
  VisualKeyboardParameters in '..\..\..\..\common\windows\delphi\visualkeyboard\VisualKeyboardParameters.pas',
  ExtShiftState in '..\..\..\..\common\windows\delphi\visualkeyboard\ExtShiftState.pas',
  CleartypeDrawCharacter in '..\..\..\..\common\windows\delphi\general\CleartypeDrawCharacter.pas',
  usp10 in '..\..\..\..\common\windows\delphi\general\usp10.pas',
  GetOsVersion in '..\..\..\..\common\windows\delphi\general\GetOsVersion.pas',
  Unicode in '..\..\..\..\common\windows\delphi\general\Unicode.pas',
  Glossary in '..\..\global\delphi\general\Glossary.pas',
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas',
  RegistryKeys in '..\..\global\delphi\general\RegistryKeys.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  ScanCodeMap in '..\..\global\delphi\general\ScanCodeMap.pas',
  kmxfileconsts in '..\..\global\delphi\general\kmxfileconsts.pas',
  VKeyChars in '..\..\global\delphi\general\VKeyChars.pas',
  VisualKeyboardLoaderBinary in '..\..\..\..\common\windows\delphi\visualkeyboard\VisualKeyboardLoaderBinary.pas',
  VisualKeyboardLoaderXML in '..\..\..\..\common\windows\delphi\visualkeyboard\VisualKeyboardLoaderXML.pas',
  VisualKeyboardSaverBinary in '..\..\..\..\common\windows\delphi\visualkeyboard\VisualKeyboardSaverBinary.pas',
  VisualKeyboardSaverXML in '..\..\..\..\common\windows\delphi\visualkeyboard\VisualKeyboardSaverXML.pas',
  utilstr in '..\..\global\delphi\general\utilstr.pas',
  utilsystem in '..\..\global\delphi\general\utilsystem.pas',
  utilexecute in '..\..\global\delphi\general\utilexecute.pas',
  VKeys in '..\..\global\delphi\general\VKeys.pas',
  VisualKeyboardExportPNG in '..\..\..\..\common\windows\delphi\visualkeyboard\VisualKeyboardExportPNG.pas',
  VisualKeyboardExportBMP in '..\..\..\..\common\windows\delphi\visualkeyboard\VisualKeyboardExportBMP.pas',
  keybtn in '..\..\..\..\common\windows\delphi\components\keybtn.pas',
  VersionInfo in '..\..\global\delphi\general\VersionInfo.pas';

begin

{$R version.res}

  try
    if not TOSKBulkRenderer.Run
      then ExitCode := 1
      else ExitCode := 0;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
