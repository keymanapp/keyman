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

type
  TInstallInfoText = (
    ssFontSize_Dialog,
    ssFontSize_Title,
    ssFontSize_InstallButton,
    ssFontName_Dialog,
    ssFontName_Title,
    ssFontName_InstallButton,

    ssApplicationTitle,
    ssTitle,
    ssWelcome_Keyboards,
    ssInstallSuccess,
    ssCancelQuery,

    ssFreeCaption,
    ssLicenseLink,
    ssInstallOptionsLink,

    ssOkButton,
    ssInstallButton,
    ssCancelButton,
    ssExitButton,

    ssPackageMissing,

    ssErrorDownloadingUpdate,
    ssErrorUnableToContactServer,
    ssErrorUnableToContactServerDetailed,

    ssStatusCheckingInternetExplorer,
    ssStatusCheckingForUpdates,
    ssStatusInstalling,
    ssStatusInstallingPackage,
    ssStatusComplete,

    ssQueryUpdateInternetExplorer,

    ssQueryUpdateVersion,
    ssQueryUpdatePackage,

    ssQueryRestart,
    ssErrorUnableToAutomaticallyRestart,

    ssRedistIEUpdateRequired,
    ssMustRestart,
    ssRedistRequired,

    ssOldOsVersionInstallKeyboards,
    ssOldKeymanVersionInstallKeyboards,

    ssOldOsVersionDownload,

    { Options dialog }

    ssOptionsTitle,

    ssOptionsStartWithWindows,
    ssOptionsStartAfterInstall,
    ssOptionsCheckForUpdates,
    ssOptionsUpgradeKeyboards,
    ssOptionsAutomaticallyReportUsage
  );

const
  FDefaultStrings: array[TInstallInfoText] of WideString = (
    '16', '24', '18',
    '', '', '',

  {ssApplicationTitle}                        '$APPNAME $VERSION Setup',
  {ssTitle}                                   'Install $APPNAME $VERSION',
  {ssWelcome_Keyboards}                       'This install includes:'#13#10+'• $APPNAME $VERSION',
  {ssInstallSuccess}                          '$APPNAME $VERSION has been installed successfully.',
  {ssCancelQuery}                             'Are you sure you want to cancel the installation of $APPNAME?',

  {ssFreeCaption}                             '$APPNAME $VERSION is free and open source',
  {ssLicenseLink}                             '&Read the license',
  {ssInstallOptionsLink}                      'Install &options',

  {ssOkButton}                                'OK',
  {ssInstallButton}                           '&Install $APPNAME',
  {ssCancelButton}                            'Cancel',
  {ssExitButton}                              'E&xit',

  {ssPackageMissing}                          'Package %0:s (%1:s) is missing.  Setup can continue but will not install this package.',

  {ssErrorDownloadingUpdate}                  'Error %0:d downloading update from server',
  {ssErrorUnableToContactServer}              'Unable to contact server',
  {ssErrorUnableToContactServerDetailed}      'Unable to contact server, error was: %0:s',

  {ssStatusCheckingInternetExplorer}          'Checking Internet Explorer version',
  {ssStatusCheckingForUpdates}                'Checking for updates online',
  {ssStatusInstalling}                        'Installing $APPNAME',
  {ssStatusInstallingPackage}                 'Installing package %0:s',
  {ssStatusComplete}                          'Installation Complete',

  {ssQueryUpdateInternetExplorer}             'Internet Explorer 9.0 or later is required to install $APPNAME.  '+
                                              'Please use Windows Update to update your system before installing $APPNAME.',   // I4470

  {ssQueryUpdateVersion}                      'Version %1:s of $APPNAME has been released and is available for download.  This update is %0:dKB.  '+
                                              'Do you want to download and install the updated version (recommended)?',
  {ssQueryUpdatePackage}                      'The package %0:s is already installed.  Do you wish to update it now?',

  {ssQueryRestart}                            'You must restart Windows before Setup can complete.  When you restart Windows, Setup will continue.'+
                                              #13#10#13#10'Restart now?',
  {ssErrorUnableToAutomaticallyRestart}       'Windows was not able to be automatically restarted.  You should restart Windows before you try and start Keyman.',
  {ssRedistIEUpdateRequired}                  'Internet Explorer 9.0 or later is required to install $APPNAME.',   // I4470
  {ssMustRestart}                             'You must restart Windows to complete Setup.  When you restart Windows, Setup will finish.',
  {ssRedistRequired}                          'A redistributable %0:s is required but is not available in the install path.  '+
                                              'This redistributable can be downloaded from the Keyman website.',

  {ssOldOsVersionInstallKeyboards}            '$APPNAME $VERSION requires Windows 7 or later to install.  '+
                                              'However, Keyman Desktop 7, 8 or 9 has been detected.  '+
                                              'Do you want to install the keyboards included in this installer '+
                                              'into the installed Keyman Desktop version?',   // I4460
  {ssOldKeymanVersionInstallKeyboards}        'This installer includes $APPNAME $VERSION.  '+
                                              'However, an earlier version of Keyman Desktop has been detected.  '+
                                              'Do you want to upgrade to $APPNAME $VERSION as well as installing '+
                                              'the keyboards in this package?  '+
                                              '(Selecting No will install the keyboards without upgrading $APPNAME)',

  {ssOldOsVersionDownload}                    'This product requires Windows 7 or later to install.  '+
                                              'Do you want to download Keyman Desktop 8?',

  { Options dialog }

  {ssOptionsTitle}                            'Install Options',

  {ssOptionsStartWithWindows}                 'Start $APPNAME when Windows starts',
  {ssOptionsStartAfterInstall}                'Start $APPNAME when installation completes',
  {ssOptionsCheckForUpdates}                  'Check for updates online periodically',
  {ssOptionsUpgradeKeyboards}                 'Upgrade keyboards installed with older versions to version $VERSION',
  {ssOptionsAutomaticallyReportUsage}         'Share anonymous usage statistics with keyman.com'
);

implementation

end.
