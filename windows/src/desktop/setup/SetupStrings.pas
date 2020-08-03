(*
  Name:             SetupStrings
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      19 Jun 2007

  Modified Date:    23 Oct 2014
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          19 Jun 2007 - mcdurdin - I817 - Translate to Unicode
                    14 Sep 2007 - mcdurdin - I1066 - -s option does not do a fully silent install in setup.exe (strings for logging)
                    28 Aug 2008 - mcdurdin - I1616 - Upgrade keyboards from 6.x
                    30 Nov 2010 - mcdurdin - I2548 - Support for upgrading Desktop 7 to Desktop 8
                    23 Oct 2014 - mcdurdin - I4470 - Internet Explorer 9.0 of later required [CrashID:kmshell.exe_9.0.472.0_script_TfrmMain_0]
*)
unit SetupStrings;

interface

uses
  System.Generics.Collections;

type
  TInstallInfoText = (
    ssLanguageName,
    ssApplicationTitle,
    ssTitle,
    ssInstallSuccess,
    ssCancelQuery,

    ssActionInstallKeyman,
    ssActionInstallPackage,
    ssActionInstallPackageLanguage,
    ssActionNothingToInstall,
    ssActionDownloadAndInstall,
    ssActionInstall,

    ssFreeCaption,
    ssLicenseLink,
    ssInstallOptionsLink,

    ssMessageBoxTitle,
    ssOkButton,
    ssInstallButton,
    ssCancelButton,
    ssExitButton,

    ssStatusInstalling,
    ssStatusComplete,

    ssQueryRestart,
    ssErrorUnableToAutomaticallyRestart,
    ssMustRestart,

    ssOldOsVersionInstallKeyboards,

    ssOldOsVersionDownload,

    { Options dialog }

    ssOptionsTitle,

    ssOptionsTitleInstallOptions,
    ssOptionsTitleDefaultKeymanSettings,
    ssOptionsTitleSelectModulesToInstall,
    ssOptionsTitleAssociatedKeyboardLanguage,

    ssOptionsStartWithWindows,
    ssOptionsStartAfterInstall,
    ssOptionsCheckForUpdates,
    ssOptionsUpgradeKeyboards,
    ssOptionsAutomaticallyReportUsage,

    ssOptionsInstallKeyman,
    ssOptionsDownloadInstallKeyman,
    ssOptionsUpgradeKeyman,
    ssOptionsDownloadUpgradeKeyman,
    ssOptionsKeymanAlreadyInstalled,

    ssOptionsInstallPackage,
    ssOptionsDownloadInstallPackage,
    ssOptionsPackageLanguageAssociation,
    ssOptionsDefaultLanguage,

    { Download dialog }

    ssDownloadingTitle,
    ssDownloadingText,

    ssOffline
  );

  (*
const
  FDefaultStrings: array[TInstallInfoText] of string = (
  {ssApplicationTitle}                        '$APPNAME $VERSION Setup',
  {ssTitle}                                   'Install $APPNAME $VERSION',
  {ssInstallSuccess}                          '$APPNAME $VERSION has been installed successfully.',
  {ssCancelQuery}                             'Are you sure you want to cancel the installation of $APPNAME?',

  {ssActionInstallKeyman}                     Char($2022)+' $APPNAME %0:s %1:s', // %0:s: version, %1:s: (size)
  {ssActionInstallPackage}                    Char($2022)+' %0:s %1:s %2:s',     // %0:s: package name %1:s: version %2:s: (size)
  {ssActionInstallPackageLanguage}            Char($2022)+' %0:s %1:s for %2:s %3:s', // %0:s: package name %1:s: version %2:s: language %3:s: (size)
  {ssActionNothingToInstall}                  'There is nothing to install.',
  {ssActionDownloadAndInstall}                'Setup will download and install:',
  {ssActionInstall}                           'Setup will install:',

  {ssFreeCaption}                             '$APPNAME $VERSION is free and open source',
  {ssLicenseLink}                             '&Read the license',
  {ssInstallOptionsLink}                      'Install &options',

  {ssMessageBoxTitle}                         '$APPNAME Setup',
  {ssOkButton}                                'OK',
  {ssInstallButton}                           '&Install',
  {ssCancelButton}                            'Cancel',
  {ssExitButton}                              'E&xit',

  {ssStatusInstalling}                        'Installing $APPNAME',
  {ssStatusComplete}                          'Installation Complete',

  {ssQueryRestart}                            'You must restart Windows before Setup can complete.  When you restart Windows, Setup will continue.'+
                                              #13#10#13#10'Restart now?',
  {ssErrorUnableToAutomaticallyRestart}       'Windows was not able to be automatically restarted.  You should restart Windows before you try and start Keyman.',
  {ssMustRestart}                             'You must restart Windows to complete Setup.  When you restart Windows, Setup will finish.',

  {ssOldOsVersionInstallKeyboards}            '$APPNAME $VERSION requires Windows 7 or later to install.  '+
                                              'However, Keyman Desktop 7, 8 or 9 has been detected.  '+
                                              'Do you want to install the keyboards included in this installer '+
                                              'into the installed Keyman Desktop version?',   // I4460

  {ssOldOsVersionDownload}                    'This product requires Windows 7 or later to install.  '+
                                              'Do you want to download Keyman Desktop 8?',

  { Options dialog }

  {ssOptionsTitle}                            'Install Options',

  {ssOptionsTitleInstallOptions}              'Installation options',
  {ssOptionsTitleDefaultKeymanSettings}       'Default Keyman settings',
  {ssOptionsTitleSelectModulesToInstall}      'Select modules to install or upgrade',
  {ssOptionsTitleAssociatedKeyboardLanguage}  'Associated Keyboard Language',

  {ssOptionsStartWithWindows}                 'Start $APPNAME when Windows starts',
  {ssOptionsStartAfterInstall}                'Start $APPNAME when installation completes',
  {ssOptionsCheckForUpdates}                  'Check for updates online periodically',
  {ssOptionsUpgradeKeyboards}                 'Upgrade keyboards installed with older versions to version $VERSION',
  {ssOptionsAutomaticallyReportUsage}         'Share anonymous usage statistics with keyman.com',

  {ssOptionsInstallKeyman}                    'Install $APPNAME %0:s', // %0:s: version
  {ssOptionsDownloadInstallKeyman}            'Download and install $APPNAME %0:s (%1:s)', // %0:s: version, %1:s: size
  {ssOptionsUpgradeKeyman}                    'Upgrade $APPNAME to %0:s', // %0:s: version
  {ssOptionsDownloadUpgradeKeyman}            'Download and upgrade $APPNAME to %0:s (%1:s)',  // %0:s: version, %1:s: size
  {ssOptionsKeymanAlreadyInstalled}           '$APPNAME %0:s is already installed.', // %0:s: installed version

  {ssOptionsInstallPackage}                   'Install %0:s %1:s',  // %0:s: package name %1:s: package version
  {ssOptionsDownloadInstallPackage}           'Download and install %0:s %1:s (%2:s)', // %0:s: package name %1:s: package version %2:s package size
  {ssOptionsPackageLanguageAssociation}       'Select the language that you wish to associate with %0:s keyboard', // %0:s package name
  {ssOptionsDefaultLanguage}                  'Default language',

  {ssDownloadingTitle}                        'Downloading %0:s',    // %0:s: filename
  {ssDownloadingText}                         'Downloading %0:s',    // %0:s: filename

  {ssOffline}                                 'Keyman Setup could not connect to keyman.com to download additional resources.'#13#10#13#10+
                                              'Please check that you are online, and give Keyman Setup permission to access the Internet in your firewall settings.'#13#10#13#10+
                                              'Click Abort to exit Setup, Retry to try and download resources again, or Ignore to continue offline.'

);
*)

type
  TLocaleArray = array[TInstallInfoText] of string;

  TSetupLocales = TDictionary<string,string>;

  TLocaleManager = class
  private
    class var
    FStrings: TDictionary<string, TLocaleArray>;
    FActiveLocale: string;
    FLocales: TSetupLocales;
    class procedure CreateStatic;
  public
    class procedure RegisterSetupStrings(const tag: string; const locale: TLocaleArray);
    class function Get(id: TInstallInfoText): string;
    class function Locales: TSetupLocales; static;
    class property ActiveLocale: string read FActiveLocale write FActiveLocale;
  end;

implementation

{ TLocaleManager }

class procedure TLocaleManager.CreateStatic;
begin
  if not Assigned(FStrings) then
  begin
    FStrings := TDictionary<string,TLocaleArray>.Create;
    FLocales := TSetupLocales.Create;
  end;
end;

class function TLocaleManager.Locales: TSetupLocales;
begin
  Result := FLocales;
end;

class function TLocaleManager.Get(id: TInstallInfoText): string;
begin
  CreateStatic;
  Result := FStrings[FActiveLocale][id];
end;

class procedure TLocaleManager.RegisterSetupStrings(const tag: string;
  const locale: TLocaleArray);
begin
  CreateStatic;
  if FActiveLocale = '' then
    FActiveLocale := tag;
  FStrings.Add(tag, locale);
  FLocales.Add(tag, locale[ssLanguageName]);
end;

end.
