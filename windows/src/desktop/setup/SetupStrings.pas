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
    ssWelcome_Plain,
    ssCheckForUpdates,
    ssInstallButton,
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

    ssQueryInstallLightMoreThan2Keyboards,
    ssQueryUpgradeVersion6,
    ssQueryUpgradeVersion7
    );

const
  FDefaultStrings: array[TInstallInfoText] of WideString = (
    '16', '24', '18',
    '', '', '',

    'Keyman Desktop Setup',
    '%0:s Setup',
    'Setup will install %0:s and the following keyboards:',
    'Setup will install %0:s',
    'Check for &updates online before installing',
    '&Install Keyman Desktop',
    'E&xit',

    'Package %0:s (%1:s) is missing.  Setup can continue but will not install this package.',

    'Error %0:d downloading update from server',
    'Unable to contact server',
    'Unable to contact server, error was: %0:s',

    'Checking Internet Explorer version',
    'Checking for updates online',
    'Installing Keyman Desktop',
    'Installing package %0:s',
    'Installation Complete',

    'Internet Explorer 9.0 or later is required to install Keyman Desktop.  Please use Windows Update to update your system before installing Keyman Destop.',   // I4470

    'Version %1:s of Keyman Desktop has been released and is available for download.  This update is %0:dKB.  '+
      'Do you want to download and install the updated version (recommended)?',

    'The package %0:s is already installed.  Do you wish to update it now?',

    'You must restart Windows before Setup can complete.  When you restart Windows, Setup will continue.'#13#10#13#10'Restart now?',
    'Windows was not able to be automatically restarted.  You should restart Windows before you try and start Keyman.',

    'Internet Explorer 9.0 or later is required to install Keyman Desktop.',   // I4470

    'You must restart Windows to complete Setup.  When you restart Windows, Setup will finish.',

    'A redistributable %0:s is required but is not available in the install path.  This redistributable can be downloaded from the Keyman website.',

    'Keyman Desktop Light Setup has detected that you have Keyman 6 installed.  However, you have more than 2 keyboards installed, '+
    'so Setup will not be able to upgrade your keyboards.  We recommend that you install Keyman Desktop Professional '+
    'to use more than 2 keyboards.  Do you want to continue Setup?',

    'Setup has detected that Keyman 6 is installed.  Do you want to copy your Keyman 6 keyboard configuration into Keyman Desktop 8?',
    'Setup has detected that Keyman Desktop 7.x is installed.  Do you want to copy your Keyman Desktop 7.x keyboard configuration into Keyman Desktop 8?'
    );

implementation

end.
