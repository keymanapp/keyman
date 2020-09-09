// This file is no longer auto-generated
unit keymanerrorcodes;

interface

{$I ..\..\..\global\delphi\general\kmcomapi_errors.inc}

type
  TKeymanErrorInfo = record
    Message: string;
    Source: string;
    HelpContext: Integer;
    Args: string;
  end;

const
  KeymanErrors: array[KMN_E_BASE and $FFFF..KMN_E_LAST and $FFFF] of TKeymanErrorInfo = (
{ Start Message Info }
    {KMN_E_Install_InvalidFile} (Message: 'The file %0:s could not be installed because of the following error: %1:s'; Source: 'InstallKeyboard'; HelpContext: 0; Args: 'Keyboard filename,Error Message'),  // The file %0:s could not be installed because of the following error: %1:s
    {KMN_E_Install_AlreadyInstalled} (Message: 'A keyboard with the name %0:s is already installed.'; Source: 'InstallKeyboard'; HelpContext: 0; Args: 'Keyboard filename'),  // A keyboard with the name %0:s is already installed.
    {KMN_E_Install_FailureToCreateDirectories} (Message: 'Failure to create directories for keyboard %0:s.'; Source: 'InstallKeyboard'; HelpContext: 0; Args: 'Keyboard name'),  // Failure to create directories for keyboard %0:s.
    {KMN_E_Uninstall_InvalidKeyboard} (Message: 'The keyboard %0:s could not be uninstalled because it was invalid.'; Source: 'UninstallKeyboard'; HelpContext: 0; Args: 'Keyboard name'),  // The keyboard %0:s could not be uninstalled because it was invalid.
    {KMN_E_Uninstall_KeyboardPartOfPackage} (Message: 'The keyboard %0:s could not be uninstalled because it was part of package %1:s.'; Source: 'UninstallKeyboard'; HelpContext: 0; Args: 'Keyboard name, Package name'),  // The keyboard %0:s could not be uninstalled because it was part of package %1:s.
    {KMN_E_Uninstall_AdminKeyboardInstalled} (Message: 'You do not have sufficient privileges to uninstall keyboard %0:s.'; Source: 'UninstallKeyboard'; HelpContext: 0; Args: 'Keyboard name'),  // You do not have sufficient privileges to uninstall keyboard %0:s.
    {KMN_W_UninstallFileNotFound} (Message: 'The file %0:s was not found during the uninstall.'; Source: 'UninstallKeyboard'; HelpContext: 0; Args: 'Filename'),  // The file %0:s was not found during the uninstall.
    {KMN_W_UninstallFileInUse} (Message: 'The file %0:s is currently in use and will be removed after the next reboot.'; Source: 'UninstallKeyboard'; HelpContext: 0; Args: 'Filename'),  // The file %0:s is currently in use and will be removed after the next reboot.
    {KMN_W_UninstallError_UnableToDeleteKeyboardRegistrySetting} (Message: 'Unable to delete keyboard %0:s registry key %1:s'; Source: 'UninstallKeyboard'; HelpContext: 0; Args: 'Keyboard name,Registry Key'),  // Unable to delete keyboard %0:s registry key %1:s
    {KMN_W_UninstallError_UnableToRemoveDirectory} (Message: 'Unable to remove directory for keyboard %0:s (error %1:s)'; Source: 'UninstallKeyboard'; HelpContext: 0; Args: 'Keyboard name,Error String'),  // Unable to remove directory for keyboard %0:s (error %1:s)
    {KMN_E_PackageInstall_UnableToGetTempPath} (Message: 'Unable to find a temp path to install package.'; Source: 'InstallPackage'; HelpContext: 0; Args: ''),  // Unable to find a temp path to install package.
    {KMN_E_PackageInstall_UnableToGetTempFileName} (Message: 'Unable to get a temp file name to install package.'; Source: 'InstallPackage'; HelpContext: 0; Args: ''),  // Unable to get a temp file name to install package.
    {KMN_E_PackageInstall_UnableToCreateTemporaryDirectory} (Message: 'Unable to create temporary folder to install package.'; Source: 'InstallPackage'; HelpContext: 0; Args: ''),  // Unable to create temporary folder to install package.
    {KMN_E_PackageInstall_UnableToFindInfFile} (Message: 'Unable to locate inf file from package - it may be damaged.  Please download the package again and try again.'; Source: 'InstallPackage'; HelpContext: 0; Args: ''),  // Unable to locate inf file from package - it may be damaged.  Please download the package again and try again.
    {KMN_E_PackageInstall_PackageAlreadyInstalled} (Message: 'The package is already installed.'; Source: 'InstallPackage'; HelpContext: 0; Args: ''),  // The package is already installed.
    {KMN_E_PackageInstall_UnableToCopyFile} (Message: 'Unable to copy file %0:s for the package to %1:s - you may have insufficient privileges, or you may have run out of disk space.'; Source: 'InstallPackage'; HelpContext: 0; Args: 'File to copy,Destination Path'),  // Unable to copy file %0:s for the package to %1:s - you may have insufficient privileges, or you may have run out of disk space.
    {KMN_W_InstallPackage_UnableToFindProgramsFolder} (Message: 'Unable to find Start Menu folder'; Source: 'InstallPackage'; HelpContext: 0; Args: ''),  // Unable to find Start Menu folder
    {KMN_W_InstallPackage_UnableToCreateStartMenuEntry} (Message: 'Unable to create Start Menu entry %0:s'; Source: 'InstallPackage'; HelpContext: 0; Args: 'Start Menu entry name'),  // Unable to create Start Menu entry %0:s
    {KMN_W_InstallPackage_CannotRunExternalProgram} (Message: 'Cannot run external program %0:s (error %1:s)'; Source: 'InstallPackage'; HelpContext: 0; Args: 'Program Command Line,Error String'),  // Cannot run external program %0:s (error %1:s)
    {KMN_W_InstallFont_CannotInstallFont} (Message: 'Cannot install font %0:s (error %2:d: %1:s)'; Source: 'InstallFont'; HelpContext: 0; Args: 'Font Name,Error String,Error Code'),  // Cannot install font %0:s (error %2:d: %1:s)
    {KMN_W_InstallFont_CannotInstallFontAdmin} (Message: 'Cannot install font registry settings: %0:s'; Source: 'InstallFont'; HelpContext: 0; Args: 'Error String'),  // Cannot install font registry settings: %0:s
    {KMN_E_Collection_InvalidIndex} (Message: 'Index %0:s out of bounds'; Source: 'Collection'; HelpContext: 0; Args: 'Collection Item Index'),  // Index %0:s out of bounds
    {KMN_E_PackageUninstall_NotFound} (Message: 'Cannot find the package to uninstall'; Source: 'UninstallPackage'; HelpContext: 0; Args: ''),  // Cannot find the package to uninstall
    {KMN_E_PackageUninstall_AdminRequired} (Message: 'This package requires administrator access to uninstall'; Source: 'UninstallPackage'; HelpContext: 0; Args: ''),  // This package requires administrator access to uninstall
    {KMN_W_PackageUninstall_FileInUse} (Message: 'The file %0:s is in use and will be removed on next reboot'; Source: 'UninstallPackage'; HelpContext: 0; Args: 'Filename'),  // The file %0:s is in use and will be removed on next reboot
    {KMN_W_UninstallFont_FontInUse} (Message: 'The font %0:s is in use and will be removed on next reboot.  Please reboot before attempting to install any additional fonts'; Source: 'UninstallFont'; HelpContext: 0; Args: 'Font name'),  // The font %0:s is in use and will be removed on next reboot.  Please reboot before attempting to install any additional fonts
    {KMN_E_VisualKeyboard_Install_AlreadyInstalled} (Message: 'A visual keyboard is already installed for keyboard %0:s'; Source: 'InstallVisualKeyboard'; HelpContext: 0; Args: 'Keyboard full name'),  // A visual keyboard is already installed for keyboard %0:s
    {KMN_E_VisualKeyboard_Install_CouldNotInstall} (Message: 'The visual keyboard %0:s could not be installed'; Source: 'InstallVisualKeyboard'; HelpContext: 0; Args: 'Visual keyboard file name'),  // The visual keyboard %0:s could not be installed
    {KMN_E_VisualKeyboard_Install_KeyboardNotInstalled} (Message: 'The visual keyboard %0:s could not be installed because the keyboard %1:s was not found'; Source: 'InstallVisualKeyboard'; HelpContext: 0; Args: 'Visual keyboard file name, keyboard name'),  // The visual keyboard %0:s could not be installed because the keyboard %1:s was not found
    {KMN_E_KeymanControl_CannotLoadKeyman32} (Message: 'Could not load keyman32.dll.  Windows returned the error code %0:8x (%1:s)'; Source: 'Control'; HelpContext: 0; Args: 'Error code,error string'),  // Could not load keyman32.dll.  Windows returned the error code %0:8x (%1:s)
    {KMN_E_KeymanControl_CannotStartProduct} (Message: 'Could not find %0:s to start Keyman Product %1:s'; Source: 'Control'; HelpContext: 0; Args: 'Keyman.exe name,Product name'),  // Could not find %0:s to start Keyman Product %1:s
    {KMN_E_KeymanControl_CannotRegisterControllerWindow} (Message: 'Could not register controller window'; Source: 'Control'; HelpContext: 0; Args: ''),  // Could not register controller window
    {KMN_E_KeymanControl_CannotUnregisterControllerWindow} (Message: 'Could not unregister controller window'; Source: 'Control'; HelpContext: 0; Args: ''),  // Could not unregister controller window
    {KMN_E_KeyboardInstall_UnableToCopyFile} (Message: 'Unable to copy file %0:s to %1:s - you may have insufficient privileges, or you may have run out of disk space.'; Source: 'InstallKeyboard'; HelpContext: 0; Args: 'File to copy,Destination path'),  // Unable to copy file %0:s to %1:s - you may have insufficient privileges, or you may have run out of disk space.
    {KMN_E_Install_KeyboardMustBeInstalledByAdmin} (Message: 'Error installing keyboard %0:s: Keyman requires keyboards to be installed by administrator'; Source: 'InstallKeyboard'; HelpContext: 0; Args: 'Keyboard filename'),  // Error installing keyboard %0:s: Keyman requires keyboards to be installed by administrator
    {KMN_W_KeyboardUninstall_ProfileNotFound} (Message: 'A language profile for %0:s could not be uninstalled for %1:s (it may be missing)'; Source: 'UninstallKeyboardLanguageProfile'; HelpContext: 0; Args: 'Language,keyboard filename'),  // A language profile for %0:s could not be uninstalled for %1:s (it may be missing)
    {KMN_E_ProfileInstall_MustBeAllUsers} (Message: 'You do not have sufficient privileges to install the language profile for %0:s'; Source: 'InstallKeyboardLanguageProfile'; HelpContext: 0; Args: 'Keyboard name'),  // You do not have sufficient privileges to install the language profile for %0:s
    {KMN_E_ProfileUninstall_MustBeAllUsers} (Message: 'You do not have sufficient privileges to uninstall the language profile for %0:s'; Source: 'UninstallKeyboardLanguageProfile'; HelpContext: 0; Args: 'Keyboard name'),  // You do not have sufficient privileges to uninstall the language profile for %0:s
    {KMN_E_ProfileInstall_KeyboardNotFound} (Message: 'The keyboard %0:s is not installed'; Source: 'InstallKeyboardLanguageProfile'; HelpContext: 0; Args: 'Keyboard name'),  // The keyboard %0:s is not installed
    {KMN_E_RecompileMnemonicLayout_mcompileFailed} (Message: 'The keyboard %2:s was not generated because mcompile failed to start with error %0:d (%1:s)'; Source: 'RecompileMnemonicKeyboard'; HelpContext: 0; Args: 'Error Code,Error String,Keyboard filename'),  // The keyboard %2:s was not generated because mcompile failed to start with error %0:d (%1:s)
    {KMN_E_RecompileMnemonicLayout_mcompileError} (Message: 'The keyboard %2:s was not generated because mcompile failed with error %0:d: %1:s'; Source: 'RecompileMnemonicKeyboard'; HelpContext: 0; Args: 'Error Code,Log Text,Keyboard filename'),  // The keyboard %2:s was not generated because mcompile failed with error %0:d: %1:s
    {KMN_E_RecompileMnemonicLayout_mcompileUnexpected} (Message: 'The keyboard %0:s was not generated due to an unknown error.'; Source: 'RecompileMnemonicLayout'; HelpContext: 0; Args: 'Keyboard filename'),  // The keyboard %0:s was not generated due to an unknown error.
    {KMN_W_KeyboardInstall_InvalidIcon} (Message: 'The icon for keyboard %0:s was not converted due to an error: %1:s'; Source: 'InstallKeyboard'; HelpContext: 0; Args: 'Keyboard name,Error String'),  // The icon for keyboard %0:s was not converted due to an error: %1:s
    {KMN_W_TSF_COMError} (Message: 'Error %0:s updating TSF'; Source: 'TSF'; HelpContext: 0; Args: 'Error message'),  // Error %0:s updating TSF
    {KMN_W_InstallPackage_KVK_Error} (Message: 'The visual keyboard %1:s for %0:s could not be installed because of an error: %2:s'; Source: 'InstallPackage'; HelpContext: 0; Args: 'Keyboard filename,Visual Keyboard filename, Error message'),  // The visual keyboard %1:s for %0:s could not be installed because of an error: %2:s
    {KMN_W_ProfileInstall_Win10_1803_MitigationApplied} (Message: 'Your edition of Windows 10 does not currently support installing Keyman keyboard layouts for %0:s. Keyman has installed this keyboard layout for %1:s instead.'; Source: 'InstallKeyboardLanguageProfile'; HelpContext: 0; Args: 'Original language,Replacement language'),  // Your edition of Windows 10 does not currently support installing Keyman keyboard layouts for %0:s. Keyman has installed this keyboard layout for %1:s instead.
    {KMN_W_Options_UnableToSaveValue} (Message: 'The option value %0:s could not be saved'; Source: 'Options'; HelpContext: 0; Args: 'Option value name'),  // The option value %0:s could not be saved

    {KMN_E_ProfileUninstall_KeyboardNotFound} (Message: 'Keyboard %0:s could not be found for profile uninstall'; Source: 'UninstallProfile'; HelpContext: 0; Args: 'KeyboardID'),
    {KMN_E_ProfileUninstall_NotATransientLanguageCode} (Message: 'Transient language code required but code %1:x found for keyboard %0:s'; Source: 'UninstallProfile'; HelpContext: 0; Args: 'KeyboardID, LangID'),
    {KMN_W_ProfileUninstall_RegistryCorrupt} (Message: 'Registry entries are missing, cannot complete profile uninstall for keyboard %0:s'; Source: 'UninstallProfile'; HelpContext: 0; Args: 'KeyboardID'),
    {KMN_W_ProfileUninstall_InstallLayoutOrTipFailed} (Message: 'InstallLayoutOrTip failed with an unspecified error for profile uninstall for keyboard %0:s'; Source: 'UninstallProfile'; HelpContext: 0; Args: 'KeyboardID'),
    {KMN_E_ProfileUninstall_ProfileNotFound} (Message: 'Profile was not found for uninstall for keyboard %0:s'; Source: 'UninstallProfile'; HelpContext: 0; Args: 'KeyboardID'),
    {KMN_E_ProfileUninstall_FailedToDeleteProfile} (Message: 'Failed to delete profile with error %1:x for keyboard %0:s'; Source: 'UninstallProfile'; HelpContext: 0; Args: 'KeyboardID,LastError'),

    {KMN_W_ProfileInstall_CustomLocalesNotSupported} (Message: 'Custom locales are not supported on Windows 7'; Source: 'InstallProfile'; HelpContext: 0),
    {KMN_W_ProfileInstall_FailedToInstallLanguage} (Message: 'Could not install language for BCP47 tag %0:s'; Source: 'InstallProfile'; HelpContext: 0; Args: 'BCP47Tag'),
    {KMN_E_ProfileInstall_InvalidBCP47Tag} (Message: 'Invalid BCP47 tag %0:s'; Source: 'InstallProfile'; HelpContext: 0; Args: 'BCP47Tag'),
    {KMN_W_ProfileInstall_LanguageInstalledButNotFound} (Message: 'BCP47 tag %0:s was installed but could not be located'; Source: 'InstallProfile'; HelpContext: 0; Args: 'BCP47Tag'),
    {KMN_W_ProfileInstall_MoreThanOneInputMethodInstalled} (Message: 'Expecting exactly one input method; instead got %0:s'; Source: 'InstallProfile'; HelpContext: 0; Args: 'list of language tags'),
    {KMN_E_ProfileInstall_IsATransientLanguageCode} (Message: 'Attempt to register profile guid for keyboard %0:s on transient lang code %1:x; instead use RegisterTransientTips'; Source: 'InstallProfile'; HelpContext: 0; Args: 'KeyboardID,LangID'),
    {KMN_E_ProfileInstall_ProfileNotFound} (Message: 'Profile for language code %1:s was not found when attempting to install TIP for %0:s'; Source: 'InstallProfile'; HelpContext: 0; Args: 'KeyboardID,BCP47Tag'),
    {KMN_E_ProfileInstall_RegistryCorrupt} (Message: 'Registry entries are missing, cannot complete profile install for keyboard %0:s'; Source: 'InstallProfile'; HelpContext: 0; Args: 'KeyboardID'),
    {KMN_E_ProfileInstall_InstallLayoutOrTipFailed} (Message: 'InstallLayoutOrTip failed when installing profile for %0:s'; Source: 'InstallProfile'; HelpContext: 0; Args: 'KeyboardID'),
    {KMN_E_ProfileInstall_UninstallLayoutOrTipFailed} (Message: 'Failed to uninstall temporary keyboard with code %0:s'; Source: 'InstallProfile'; HelpContext: 0; Args: 'Layout string'),
    {KMN_E_ProfileInstall_RegisterProfileFailed} (Message: 'RegisterProfile failed with error %1:x: %0:s'; Source: 'InstallProfile'; HelpContext: 0; Args: 'Message, Code')

  { End Message Info }
  );

implementation

end.
