(*
  Name:             SetupStrings
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      19 Jun 2007

  Modified Date:    17 Sep 2007
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          19 Jun 2007 - mcdurdin - I817 - Translate to Unicode
                    23 Aug 2007 - mcdurdin - Initial version from Desktop
                    17 Sep 2007 - mcdurdin - I1072 - Add log messages for silent install
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
    ssWelcome_Plain,
    ssSIL,
    ssCheckForUpdates,
    ssInstallButton,
    ssExitButton,

    ssErrorDownloadingUpdate,
    ssErrorUnableToContactServer,
    ssErrorUnableToContactServerDetailed,

    ssStatusCheckingWindowsInstaller,
    ssStatusCheckingInternetExplorer,
    ssStatusCheckingMSXML,
    ssStatusCheckingForUpdates,
    ssStatusInstalling,
    ssStatusComplete,

    ssQueryUpdateWindowsInstaller,
    ssQueryUpdateInternetExplorer,
    ssQueryUpdateMSXML,

    ssQueryUpdateVersion,

    ssQueryRestart,
    ssErrorUnableToAutomaticallyRestart,

    ssRedistIEUpdateRequired,
    ssMustRestart,
    ssRedistRequired,
    ssCheckForUpdatesError
    );

const
  FDefaultStrings: array[TInstallInfoText] of WideString = (
    '16', '24', '18',
    '', '', '',

    'Keyman Developer Setup',
    '%0:s Setup',
    'Setup will install %0:s',
    'Created by SIL International',
    'Check for &updates online before installing',
    '&Install Keyman Developer',
    'E&xit',

    'Error %0:d downloading update from server',
    'Unable to contact server',
    'Unable to contact server, error was: %0:s',

    'Checking Windows Installer version',
    'Checking Internet Explorer version',
    'Checking MSXML version',
    'Checking for updates online',
    'Installing Keyman Developer',
    'Installation Complete',

    'An updated version of Windows Installer is required to install Keyman Developer.  Do you want to download and install it now?',
    'Internet Explorer 5.5 or later is required to install Keyman Developer.  Do you want to download it now?',
    'MSXML 3.0 is required to install Keyman Developer.  Do you want to download it now?',

    'Version %1:s of Keyman Developer has been released and is available for download.  This update is %0:dKB.  '+
      'Do you want to download and install the updated version (recommended)?',

    'You must restart Windows before Setup can complete.  When you restart Windows, Setup will continue.  Restart now?',
    'Windows was not able to be automatically restarted.  You should restart Windows before you try and start Keyman Developer.',

    'Internet Explorer 5.5 or later is required to install Keyman Developer.',

    'You must restart Windows to complete Setup.  When you restart Windows, Setup will finish.',

    'A redistributable %0:s is required but is not available in the install path.  This redistributable can be downloaded from the Keyman website.',

    'The online update check failed with an error: %0:s'
    );

implementation

end.
