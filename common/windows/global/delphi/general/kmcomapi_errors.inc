const
  KMN_E_BASE = $A0000200;
  KMN_W_BASE = $20000200;

const
  KMN_E_Install_InvalidFile = KMN_E_BASE + $0;
  KMN_E_Install_AlreadyInstalled = KMN_E_BASE + $1;
  KMN_E_Install_FailureToCreateDirectories = KMN_E_BASE + $2;
  KMN_E_Uninstall_InvalidKeyboard = KMN_E_BASE + $3;
  KMN_E_Uninstall_KeyboardPartOfPackage = KMN_E_BASE + $4;
  KMN_E_Uninstall_AdminKeyboardInstalled = KMN_E_BASE + $5;
  KMN_W_UninstallFileNotFound = KMN_W_BASE + $6;
  KMN_W_UninstallFileInUse = KMN_W_BASE + $7;
  KMN_W_UninstallError_UnableToDeleteKeyboardRegistrySetting = KMN_W_BASE + $8;
  KMN_W_UninstallError_UnableToRemoveDirectory = KMN_W_BASE + $9;
  KMN_E_PackageInstall_UnableToGetTempPath = KMN_E_BASE + $A;
  KMN_E_PackageInstall_UnableToGetTempFileName = KMN_E_BASE + $B;
  KMN_E_PackageInstall_UnableToCreateTemporaryDirectory = KMN_E_BASE + $C;
  KMN_E_PackageInstall_UnableToFindInfFile = KMN_E_BASE + $D;
  KMN_E_PackageInstall_PackageAlreadyInstalled = KMN_E_BASE + $E;
  KMN_E_PackageInstall_UnableToCopyFile = KMN_E_BASE + $F;
  KMN_W_InstallPackage_UnableToFindProgramsFolder = KMN_W_BASE + $10;
  KMN_W_InstallPackage_UnableToCreateStartMenuEntry = KMN_W_BASE + $11;
  KMN_W_InstallPackage_CannotRunExternalProgram = KMN_W_BASE + $12;
  KMN_W_InstallFont_CannotInstallFont = KMN_W_BASE + $13;
  KMN_W_InstallFont_CannotInstallFontAdmin = KMN_W_BASE + $14;
  KMN_E_Collection_InvalidIndex = KMN_E_BASE + $15;
  KMN_E_PackageUninstall_NotFound = KMN_E_BASE + $16;
  KMN_E_PackageUninstall_AdminRequired = KMN_E_BASE + $17;
  KMN_W_PackageUninstall_FileInUse = KMN_W_BASE + $18;
  KMN_W_UninstallFont_FontInUse = KMN_W_BASE + $19;
  KMN_E_VisualKeyboard_Install_AlreadyInstalled = KMN_E_BASE + $1A;
  KMN_E_VisualKeyboard_Install_CouldNotInstall = KMN_E_BASE + $1B;
  KMN_E_VisualKeyboard_Install_KeyboardNotInstalled = KMN_E_BASE + $1C;
  KMN_E_KeymanControl_CannotLoadKeyman32 = KMN_E_BASE + $1D;
  KMN_E_KeymanControl_CannotStartProduct = KMN_E_BASE + $1E;
  KMN_E_KeymanControl_CannotRegisterControllerWindow = KMN_E_BASE + $1F;
  KMN_E_KeymanControl_CannotUnregisterControllerWindow = KMN_E_BASE + $20;
  KMN_E_KeyboardInstall_UnableToCopyFile = KMN_E_BASE + $21;
  KMN_E_Install_KeyboardMustBeInstalledByAdmin = KMN_E_BASE + $22;
  KMN_W_KeyboardUninstall_ProfileNotFound = KMN_W_BASE + $23;
  KMN_E_ProfileInstall_MustBeAllUsers = KMN_E_BASE + $24;
  KMN_E_ProfileUninstall_MustBeAllUsers = KMN_E_BASE + $25;
  KMN_E_ProfileInstall_KeyboardNotFound = KMN_E_BASE + $26;
  KMN_E_RecompileMnemonicLayout_mcompileFailed = KMN_E_BASE + $27;
  KMN_E_RecompileMnemonicLayout_mcompileError = KMN_E_BASE + $28;
  KMN_E_RecompileMnemonicLayout_mcompileUnexpected = KMN_E_BASE + $29;
  KMN_W_KeyboardInstall_InvalidIcon = KMN_W_BASE + $2A;
  KMN_W_TSF_COMError = KMN_W_BASE + $2B;
  KMN_W_InstallPackage_KVK_Error = KMN_W_BASE + $2C;
  KMN_W_ProfileInstall_Win10_1803_MitigationApplied = KMN_W_BASE + $2D;
  KMN_W_Options_UnableToSaveValue = KMN_W_BASE + $2E;

  KMN_E_ProfileUninstall_KeyboardNotFound = KMN_E_BASE + $2F; // keyboardid
  KMN_E_ProfileUninstall_NotATransientLanguageCode = KMN_E_BASE + $30; // keyboardid, langid
  KMN_W_ProfileUninstall_RegistryCorrupt = KMN_W_BASE + $31; //keyboardid
  KMN_W_ProfileUninstall_InstallLayoutOrTipFailed = KMN_W_BASE + $32; // keyboardid
  KMN_E_ProfileUninstall_ProfileNotFound = KMN_E_BASE + $33; // keyboardid
  KMN_E_ProfileUninstall_FailedToDeleteProfile = KMN_E_BASE + $34; // keyboardid, lasterror

  KMN_W_ProfileInstall_CustomLocalesNotSupported = KMN_W_BASE + $35; // Custom locales are not supported on Windows 7
  KMN_W_ProfileInstall_FailedToInstallLanguage = KMN_W_BASE + $36; //'Could not install language for BCP47 tag %0:s'
  KMN_E_ProfileInstall_InvalidBCP47Tag = KMN_E_BASE + $37; // bcp47tag
  KMN_W_ProfileInstall_LanguageInstalledButNotFound = KMN_W_BASE + $38; //'BCP47 language %0:s was installed but could not be located'
  KMN_W_ProfileInstall_MoreThanOneInputMethodInstalled = KMN_W_BASE + $39; //'Expecting exactly one input method; instead got %0:s'
  KMN_E_ProfileInstall_IsATransientLanguageCode = KMN_E_BASE + $3A; // keyboardid, langid 'Attempt to register profile guid for keyboard %0:s on transient lang code %0:x; instead use RegisterTransientTips'
  KMN_E_ProfileInstall_ProfileNotFound = KMN_E_BASE + $3B; // keyboardid,bcp47tag
  KMN_E_ProfileInstall_RegistryCorrupt = KMN_E_BASE + $3C; // keyboardid
  KMN_E_ProfileInstall_InstallLayoutOrTipFailed = KMN_E_BASE + $3D; // keyboardid
  KMN_E_ProfileInstall_UninstallLayoutOrTipFailed = KMN_E_BASE + $3E; // layoutstring
  KMN_E_ProfileInstall_RegisterProfileFailed = KMN_E_BASE + $3F; // message,code

  KMN_E_LAST = $A000023F;
  KMN_W_LAST = $2000023F;

