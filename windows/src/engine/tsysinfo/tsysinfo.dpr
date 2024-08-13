program tsysinfo;

uses
  Winapi.Windows,
  Forms,
  winver in 'winver.pas',
  fileversioninfo in 'fileversioninfo.pas',
  sysinfo_main in 'sysinfo_main.pas' {frmDiagnostics},
  sysinfo_util in 'sysinfo_util.pas',
  udumpfile in 'udumpfile.pas',
  UfrmEmail in 'UfrmEmail.pas' {frmEmail},
  UfrmProgress in 'UfrmProgress.pas' {frmProgress},
  httpuploader in '..\..\..\..\common\windows\delphi\general\httpuploader.pas',
  httpuploader_messageprocessor_forms in '..\..\..\..\common\windows\delphi\general\httpuploader_messageprocessor_forms.pas',
  VersionInfo in '..\..\..\..\common\windows\delphi\general\VersionInfo.pas',
  utilhttp in '..\..\..\..\common\windows\delphi\general\utilhttp.pas',
  wininet5 in '..\..\..\..\common\windows\delphi\general\wininet5.pas',
  RegistryKeys in '..\..\..\..\common\windows\delphi\general\RegistryKeys.pas',
  kmxfile in '..\..\..\..\common\windows\delphi\keyboards\kmxfile.pas',
  utilkeyboard in '..\..\..\..\common\windows\delphi\keyboards\utilkeyboard.pas',
  utilfiletypes in '..\..\..\..\common\windows\delphi\general\utilfiletypes.pas',
  KeyNames in '..\..\..\..\common\windows\delphi\general\KeyNames.pas',
  ttinfo in '..\..\..\..\common\windows\delphi\general\ttinfo.pas',
  Upload_Settings in '..\..\..\..\common\windows\delphi\general\Upload_Settings.pas',
  DebugPaths in '..\..\..\..\common\windows\delphi\general\DebugPaths.pas',
  GetOsVersion in '..\..\..\..\common\windows\delphi\general\GetOsVersion.pas',
  GlobalProxySettings in '..\..\..\..\common\windows\delphi\general\GlobalProxySettings.pas',
  klog in '..\..\..\..\common\windows\delphi\general\klog.pas',
  utildir in '..\..\..\..\common\windows\delphi\general\utildir.pas',
  utilsystem in '..\..\..\..\common\windows\delphi\general\utilsystem.pas',
  ErrorControlledRegistry in '..\..\..\..\common\windows\delphi\vcl\ErrorControlledRegistry.pas',
  Unicode in '..\..\..\..\common\windows\delphi\general\Unicode.pas',
  utilexecute in '..\..\..\..\common\windows\delphi\general\utilexecute.pas',
  KeymanVersion in '..\..\..\..\common\windows\delphi\general\KeymanVersion.pas',
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
  Glossary in '..\..\..\..\common\windows\delphi\general\Glossary.pas',
  TempFileManager in '..\..\..\..\common\windows\delphi\general\TempFileManager.pas',
  StockFileNames in '..\..\..\..\common\windows\delphi\general\StockFileNames.pas',
  kmxfileconsts in '..\..\..\..\common\windows\delphi\keyboards\kmxfileconsts.pas',
  Keyman.System.CEFManager in '..\..\..\..\common\windows\delphi\chromium\Keyman.System.CEFManager.pas',
  Keyman.UI.UframeCEFHost in '..\..\..\..\common\windows\delphi\chromium\Keyman.UI.UframeCEFHost.pas' {frameCEFHost},
  UserMessages in '..\..\..\..\common\windows\delphi\general\UserMessages.pas',
  KeymanPaths in '..\..\..\..\common\windows\delphi\general\KeymanPaths.pas',
  Sentry.Client in '..\..\..\..\common\windows\delphi\ext\sentry\Sentry.Client.pas',
  Sentry.Client.Vcl in '..\..\..\..\common\windows\delphi\ext\sentry\Sentry.Client.Vcl.pas',
  sentry in '..\..\..\..\common\windows\delphi\ext\sentry\sentry.pas',
  Keyman.System.KeymanSentryClient in '..\..\..\..\common\windows\delphi\general\Keyman.System.KeymanSentryClient.pas';

{$R *.res}
{$R manifest.res}
{$R version.res}

// CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
// If you don't add this flag the rederer process will crash when you try to load large images.
{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

const
  LOGGER_DESKTOP_ENGINE_TSYSINFO = TKeymanSentryClient.LOGGER_DESKTOP_ENGINE + '.tsysinfo';
begin
  TKeymanSentryClient.Start(TSentryClientVcl, kscpDesktop, LOGGER_DESKTOP_ENGINE_TSYSINFO,
    LoadKeymanDesktopSentryFlags([kscfCaptureExceptions])); // no ui for exceptions, no termination
  try
    if RunCrashReportHandler then
      Exit;

    FInitializeCEF := TCEFManager.Create(False);
    try
      try
        if FInitializeCEF.Start then
        begin
          Application.Title := 'Keyman System Information';
          Application.Initialize;
          Application.CreateForm(TfrmDiagnostics, frmDiagnostics);
          Application.Run;
        end;
      finally
        frmDiagnostics.Free;
      end;
    finally
      FInitializeCEF.Free;
    end;
  finally
    TKeymanSentryClient.Stop;
  end;
end.
