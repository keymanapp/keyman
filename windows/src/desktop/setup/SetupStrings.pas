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
    ssStatusRemovingOlderVersions,
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

    ssInstallerVersion,

    { Download dialog }

    ssDownloadingTitle,
    ssDownloadingText,

    ssOffline
  );

implementation

end.
