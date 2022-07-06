program passtructsize;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  compile in '..\..\..\..\common\delphi\compiler\compile.pas',
  kmxfileconsts in '..\..\..\..\..\..\common\windows\delphi\keyboards\kmxfileconsts.pas',
  RegistryKeys in '..\..\..\..\..\..\common\windows\delphi\general\RegistryKeys.pas',
  KeymanVersion in '..\..\..\..\..\..\common\windows\delphi\general\KeymanVersion.pas',
  RedistFiles in '..\..\..\..\tike\main\RedistFiles.pas',
  ErrorControlledRegistry in '..\..\..\..\..\..\common\windows\delphi\vcl\ErrorControlledRegistry.pas',
  Upload_Settings in '..\..\..\..\..\..\common\windows\delphi\general\Upload_Settings.pas',
  DebugPaths in '..\..\..\..\..\..\common\windows\delphi\general\DebugPaths.pas',
  KeymanPaths in '..\..\..\..\..\..\common\windows\delphi\general\KeymanPaths.pas',
  VersionInfo in '..\..\..\..\..\..\common\windows\delphi\general\VersionInfo.pas';

{
  These size values are used in unit tests to ensure
  that the structure sizes correspond precisely across
  compilers (pas and c++).
}
const sz_FILE_STORE = sizeof(FILE_STORE);
const sz_FILE_KEY = sizeof(FILE_KEY);
const sz_FILE_GROUP = sizeof(FILE_GROUP);
const sz_FILE_DEADKEY = sizeof(FILE_DEADKEY);
const sz_FILE_VKDICTIONARY = sizeof(FILE_VKDICTIONARY);
const sz_FILE_KEYBOARD = sizeof(FILE_KEYBOARD);

begin
  writeln('sz_FILE_STORE = '+IntToStr(sz_FILE_STORE));
  writeln('sz_FILE_KEY = '+IntToStr(sz_FILE_KEY));
  writeln('sz_FILE_GROUP = '+IntToStr(sz_FILE_GROUP));
  writeln('sz_FILE_DEADKEY = '+IntToStr(sz_FILE_DEADKEY));
  writeln('sz_FILE_VKDICTIONARY = '+IntToStr(sz_FILE_VKDICTIONARY));
  writeln('sz_FILE_KEYBOARD = '+IntToStr(sz_FILE_KEYBOARD));
end.
