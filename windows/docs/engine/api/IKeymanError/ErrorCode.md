---
title: IKeymanError::ErrorCode Property
---

## Introduction

The `IKeymanError::ErrorCode` property returns an integer error code for
the error.

## Specification

``` clike
readonly long ErrorCode
```

## Defined Error Codes

| Code | Identifier | Message | Source |
|----|----|----|----|
| 0xA0000200 | KMN_E_Install_InvalidFile | The file %0:s could not be installed because of the following error: %1:s | InstallKeyboard |
| 0xA0000201 | KMN_E_Install_AlreadyInstalled | A keyboard with the name %0:s is already installed. | InstallKeyboard |
| 0xA0000202 | KMN_E_Install_FailureToCreateDirectories | Failure to create directories for keyboard %0:s. | InstallKeyboard |
| 0xA0000203 | KMN_E_Uninstall_InvalidKeyboard | The keyboard %0:s could not be uninstalled because it was invalid. | UninstallKeyboard |
| 0xA0000204 | KMN_E_Uninstall_KeyboardPartOfPackage | The keyboard %0:s could not be uninstalled because it was part of package %1:s. | UninstallKeyboard |
| 0xA0000205 | KMN_E_Uninstall_AdminKeyboardInstalled | You do not have sufficient privileges to uninstall keyboard %0:s. | UninstallKeyboard |
| 0x20000206 | KMN_W_UninstallFileNotFound | The file %0:s was not found during the uninstall. | UninstallKeyboard |
| 0x20000207 | KMN_W_UninstallFileInUse | The file %0:s is currently in use and will be removed after the next reboot. | UninstallKeyboard |
| 0x20000208 | KMN_W_UninstallError_UnableToDeleteKeyboardRegistrySetting | Unable to delete keyboard %0:s registry key %1:s | UninstallKeyboard |
| 0x20000209 | KMN_W_UninstallError_UnableToRemoveDirectory | Unable to remove directory for keyboard %0:s (error %1:s) | UninstallKeyboard |
| 0xA000020A | KMN_E_PackageInstall_UnableToGetTempPath | Unable to find a temp path to install package. | InstallPackage |
| 0xA000020B | KMN_E_PackageInstall_UnableToGetTempFileName | Unable to get a temp file name to install package. | InstallPackage |
| 0xA000020C | KMN_E_PackageInstall_UnableToCreateTemporaryDirectory | Unable to create temporary folder to install package. | InstallPackage |
| 0xA000020D | KMN_E_PackageInstall_UnableToFindInfFile | Unable to locate inf file from package - it may be damaged. Please download the package again and try again. | InstallPackage |
| 0xA000020E | KMN_E_PackageInstall_PackageAlreadyInstalled | The package is already installed. | InstallPackage |
| 0xA000020F | KMN_E_PackageInstall_UnableToCopyFile | Unable to copy file %0:s for the package to %1:s - you may have insufficient privileges, or you may have run out of disk space. | InstallPackage |
| 0x20000210 | KMN_W_InstallPackage_UnableToFindProgramsFolder | Unable to find Start Menu folder | InstallPackage |
| 0x20000211 | KMN_W_InstallPackage_UnableToCreateStartMenuEntry | Unable to create Start Menu entry %0:s | InstallPackage |
| 0x20000212 | KMN_W_InstallPackage_CannotRunExternalProgram | Cannot run external program %0:s (error %1:s) | InstallPackage |
| 0x20000213 | KMN_W_InstallFont_CannotInstallFont | Cannot install font %0:s (error %2:d: %1:s) | InstallFont |
| 0x20000214 | KMN_W_InstallFont_CannotInstallFontAdmin | Cannot install font registry settings: %0:s | InstallFont |
| 0xA0000215 | KMN_E_Collection_InvalidIndex | Index %0:s out of bounds | Collection |
| 0xA0000216 | KMN_E_PackageUninstall_NotFound | Cannot find the package to uninstall | UninstallPackage |
| 0xA0000217 | KMN_E_PackageUninstall_AdminRequired | This package requires administrator access to uninstall | UninstallPackage |
| 0x20000218 | KMN_W_PackageUninstall_FileInUse | The file %0:s is in use and will be removed on next reboot | UninstallPackage |
| 0x20000219 | KMN_W_UninstallFont_FontInUse | The font %0:s is in use and will be removed on next reboot. Please reboot before attempting to install any additional fonts | UninstallFont |
| 0xA000021A | KMN_E_VisualKeyboard_Install_AlreadyInstalled | A visual keyboard is already installed for keyboard %0:s | InstallVisualKeyboard |
| 0xA000021B | KMN_E_VisualKeyboard_Install_CouldNotInstall | The visual keyboard %0:s could not be installed | InstallVisualKeyboard |
| 0xA000021C | KMN_E_VisualKeyboard_Install_KeyboardNotInstalled | The visual keyboard %0:s could not be installed because the keyboard %1:s was not found | InstallVisualKeyboard |
| 0xA000021D | KMN_E_KeymanControl_CannotLoadKeyman32 | Could not load keyman32.dll. Windows returned the error code %0:8x (%1:s) | Control |
| 0xA000021E | KMN_E_KeymanControl_CannotStartProduct | Could not find %0:s to start Keyman Product %1:s | Control |
| 0xA000021F | KMN_E_KeymanControl_CannotRegisterControllerWindow | Could not register controller window | Control |
| 0xA0000220 | KMN_E_KeymanControl_CannotUnregisterControllerWindow | Could not unregister controller window | Control |
| 0xA0000221 | KMN_E_KeyboardInstall_UnableToCopyFile | Unable to copy file %0:s to %1:s - you may have insufficient privileges, or you may have run out of disk space. | InstallKeyboard |
| 0xA0000222 | KMN_E_Install_KeyboardMustBeInstalledByAdmin | Error installing keyboard %0:s: Keyman requires keyboards to be installed by administrator | InstallKeyboard |
| 0x20000223 | KMN_W_KeyboardUninstall_ProfileNotFound | A language profile for %0:s could not be uninstalled for %1:s (it may be missing) | UninstallKeyboardLanguageProfile |
| 0xA0000224 | KMN_E_ProfileInstall_MustBeAllUsers | You do not have sufficient privileges to install the language profile for %0:s | InstallKeyboardLanguageProfile |
| 0xA0000225 | KMN_E_ProfileUninstall_MustBeAllUsers | You do not have sufficient privileges to uninstall the language profile for %0:s | UninstallKeyboardLanguageProfile |
| 0xA0000226 | KMN_E_ProfileInstall_KeyboardNotFound | The keyboard %0:s is not installed | InstallKeyboardLanguageProfile |
| 0xA0000227 | KMN_E_RecompileMnemonicLayout_mcompileFailed | The keyboard %2:s was not generated because mcompile failed to start with error %0:d (%1:s) | RecompileMnemonicKeyboard |
| 0xA0000228 | KMN_E_RecompileMnemonicLayout_mcompileError | The keyboard %2:s was not generated because mcompile failed with error %0:d: %1:s | RecompileMnemonicKeyboard |
| 0xA0000229 | KMN_E_RecompileMnemonicLayout_mcompileUnexpected | The keyboard %0:s was not generated due to an unknown error. | RecompileMnemonicLayout |
| 0x2000022A | KMN_W_KeyboardInstall_InvalidIcon | The icon for keyboard %0:s was not converted due to an error: %1:s | InstallKeyboard |
| 0x2000022B | KMN_W_TSF_COMError | Error %0:s updating TSF | TSF |
