program InstallCustomLanguage;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.Win.ComObj,
  Winapi.ActiveX,
  System.SysUtils,
  InstallCustomLanguageMain in 'InstallCustomLanguageMain.pas',
  keymanapi_TLB in '..\..\engine\kmcomapi\keymanapi_TLB.pas',
  keyman_msctf in '..\..\global\delphi\winapi\keyman_msctf.pas',
  TempFileManager in '..\..\..\..\common\windows\delphi\general\TempFileManager.pas',
  utildir in '..\..\..\..\common\windows\delphi\general\utildir.pas',
  utilexecute in '..\..\..\..\common\windows\delphi\general\utilexecute.pas',
  klog in '..\..\..\..\common\windows\delphi\general\klog.pas',
  Unicode in '..\..\..\..\common\windows\delphi\general\Unicode.pas',
  BCP47SuppressScriptRegistry in '..\..\global\delphi\general\BCP47SuppressScriptRegistry.pas';

begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  try
    ExitCode := Run;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 2;
    end;
  end;
  CoUninitialize;
end.
