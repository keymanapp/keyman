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

    ssStatusCheckingForUpdates,
    ssStatusInstalling,
    ssStatusComplete,

    ssQueryUpdateVersion,

    ssQueryRestart,
    ssErrorUnableToAutomaticallyRestart,

    ssMustRestart,
    ssCheckForUpdatesError
    );

const
  FDefaultStrings: array[TInstallInfoText] of WideString = (
    '16', '24', '18',
    '', '', '',

    'Keyman Developer Setup',
    'Keyman Developer %0:s Setup',
    'Setup will install %0:s',
    'Created by SIL Global',
    'Check for &updates online before installing',
    '&Install Keyman Developer',
    'E&xit',

    'Error %0:d downloading update from server',
    'Unable to contact server',
    'Unable to contact server, error was: %0:s',

    'Checking for updates online',
    'Installing Keyman Developer',
    'Installation Complete',

    'Version %1:s of Keyman Developer has been released and is available for download.  This update is %0:dKB.  '+
      'Do you want to download and install the updated version (recommended)?',

    'You must restart Windows before Setup can complete.  When you restart Windows, Setup will continue.  Restart now?',
    'Windows was not able to be automatically restarted.  You should restart Windows before you try and start Keyman Developer.',

    'You must restart Windows to complete Setup.  When you restart Windows, Setup will finish.',

    'The online update check failed with an error: %0:s'
    );

implementation

end.
