program tsysinfo;

uses
  Forms,
  winver in 'winver.pas',
  fileversioninfo in 'fileversioninfo.pas',
  sysinfo_main in 'sysinfo_main.pas' {frmDiagnostics},
  sysinfo_util in 'sysinfo_util.pas',
  udumpfile in 'udumpfile.pas',
  UfrmEmail in 'UfrmEmail.pas' {frmEmail},
  UfrmProgress in 'UfrmProgress.pas' {frmProgress},
  httpuploader in '..\..\global\delphi\general\httpuploader.pas',
  httpuploader_messageprocessor_forms in '..\..\global\delphi\general\httpuploader_messageprocessor_forms.pas',
  VersionInfo in '..\..\global\delphi\general\VersionInfo.pas',
  utilhttp in '..\..\global\delphi\general\utilhttp.pas',
  wininet5 in '..\..\global\delphi\general\wininet5.pas',
  RegistryKeys in '..\..\global\delphi\general\RegistryKeys.pas',
  kmxfile in '..\..\global\delphi\general\kmxfile.pas',
  utilkeyboard in '..\..\global\delphi\general\utilkeyboard.pas',
  utilfiletypes in '..\..\global\delphi\general\utilfiletypes.pas',
  CRC32 in '..\..\global\delphi\general\CRC32.pas',
  KeyNames in '..\..\global\delphi\general\KeyNames.pas',
  OnlineConstants in '..\..\global\delphi\productactivation\OnlineConstants.pas',
  ttinfo in '..\..\global\delphi\general\ttinfo.pas',
  Upload_Settings in '..\..\global\delphi\general\Upload_Settings.pas',
  DebugPaths in '..\..\global\delphi\general\DebugPaths.pas',
  GetOsVersion in '..\..\global\delphi\general\GetOsVersion.pas',
  GlobalProxySettings in '..\..\global\delphi\general\GlobalProxySettings.pas',
  MSHTML_TLB in '..\..\global\delphi\tlb\MSHTML_TLB.pas',
  ErrLogPath in '..\..\global\delphi\general\ErrLogPath.pas',
  klog in '..\..\global\delphi\general\klog.pas',
  utildir in '..\..\global\delphi\general\utildir.pas',
  utilsystem in '..\..\global\delphi\general\utilsystem.pas',
  SystemDebugPath in '..\..\global\delphi\general\SystemDebugPath.pas',
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas',
  Unicode in '..\..\global\delphi\general\Unicode.pas',
  utilexecute in '..\..\global\delphi\general\utilexecute.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  si_base in 'si_base.pas',
  si_browsers in 'si_browsers.pas',
  si_fonts in 'si_fonts.pas',
  si_hookdlls in 'si_hookdlls.pas',
  si_keyman in 'si_keyman.pas',
  si_language in 'si_language.pas',
  si_office in 'si_office.pas',
  si_overview in 'si_overview.pas',
  si_processes in 'si_processes.pas',
  si_processes_x64 in 'si_processes_x64.pas',
  si_startup in 'si_startup.pas',
  UfrmRemoteExceptionHandler in 'UfrmRemoteExceptionHandler.pas' {frmExceptionHandler},
  UframeAttachedFiles in 'UframeAttachedFiles.pas',
  keyman_msctf in '..\..\global\delphi\winapi\keyman_msctf.pas',
  Glossary in '..\..\global\delphi\general\Glossary.pas',
  TempFileManager in '..\..\global\delphi\general\TempFileManager.pas',
  SHDocVw in '..\..\global\delphi\vcl\SHDocVw.pas',
  StockFileNames in '..\..\global\delphi\cust\StockFileNames.pas';

{$R *.res}
{$R manifest.res}
{$R version.res}

begin
  Application.Title := 'Keyman Desktop System Information';
  Application.Initialize;
  Application.CreateForm(TfrmDiagnostics, frmDiagnostics);
  Application.Run;
end.
