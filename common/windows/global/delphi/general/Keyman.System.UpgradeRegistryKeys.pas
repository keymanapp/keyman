unit Keyman.System.UpgradeRegistryKeys;

interface

{-------------------------------------------------------------------------------
 - Upgrade-related keys
 -------------------------------------------------------------------------------}

uses
  KeymanVersion,
  RegistryKeys;

const

// DO NOT LOCALISE ANYTHING IN THIS FILE

{-------------------------------------------------------------------------------
 - Upgrade related registry keys
 -------------------------------------------------------------------------------}

  SRegKey__Tavultesoft_LM        = SRegKey_Software_LM         + '\Tavultesoft';       // LM
  SRegKey__Tavultesoft_CU        = SRegKey_Software_CU         + '\Tavultesoft';       // CU

  SRegKey_TavultesoftKeyman_CU = SRegKey__Tavultesoft_CU + '\Keyman';
  SRegKey_Keyman60_CU = SRegKey_TavultesoftKeyman_CU + '\'+SKeymanVersion60;
  SRegKey_Keyman70_CU = SRegKey_TavultesoftKeyman_CU + '\'+SKeymanVersion70;
  SRegKey_Keyman80_CU = SRegKey_TavultesoftKeyman_CU + '\'+SKeymanVersion80;

  SRegKey_TavultesoftKeyman_LM = SRegKey__Tavultesoft_LM + '\Keyman';
  SRegKey_Keyman60_LM = SRegKey_TavultesoftKeyman_LM + '\'+SKeymanVersion60;
  SRegKey_Keyman70_LM = SRegKey_TavultesoftKeyman_LM + '\'+SKeymanVersion70;
  SRegKey_Keyman80_LM = SRegKey_TavultesoftKeyman_LM + '\'+SKeymanVersion80;

  SRegKey_TavultesoftKeymanEngine_CU = SRegKey__Tavultesoft_CU + '\Keyman Engine';
  SRegKey_KeymanEngine70_CU = SRegKey_TavultesoftKeymanEngine_CU + '\'+SKeymanVersion70;
  SRegKey_KeymanEngine80_CU = SRegKey_TavultesoftKeymanEngine_CU + '\'+SKeymanVersion80;
  SRegKey_KeymanEngine90_CU = SRegKey_TavultesoftKeymanEngine_CU + '\'+SKeymanVersion90;
  SRegKey_KeymanEngine100_CU = SRegKey_KeymanEngine_CU + '\'+SKeymanVersion100;

  SRegKey_TavultesoftKeymanEngine_LM = SRegKey__Tavultesoft_LM + '\Keyman Engine';
  SRegKey_KeymanEngine70_LM = SRegKey_TavultesoftKeymanEngine_LM + '\'+SKeymanVersion70;
  SRegKey_KeymanEngine80_LM = SRegKey_TavultesoftKeymanEngine_LM + '\'+SKeymanVersion80;
  SRegKey_KeymanEngine90_LM = SRegKey_TavultesoftKeymanEngine_LM + '\'+SKeymanVersion90;
  SRegKey_KeymanEngine100_LM = SRegKey_KeymanEngine_LM + '\'+SKeymanVersion100;

  SRegKey_Keyman60_InstalledPackages_CU = SRegKey_Keyman60_CU + '\Installed Packages';
  SRegKey_Keyman60_InstalledKeyboards_CU = SRegKey_Keyman60_CU + '\Installed Keyboards';
  SRegKey_Keyman60_ActiveKeyboards_CU = SRegKey_Keyman60_CU + '\Active Keyboards';

  SRegKey_Keyman60_InstalledPackages_LM = SRegKey_Keyman60_LM + '\Installed Packages';
  SRegKey_Keyman60_InstalledKeyboards_LM = SRegKey_Keyman60_LM + '\Installed Keyboards';

  SRegKey_KeymanEngine70_InstalledPackages_CU = SRegKey_KeymanEngine70_CU+'\Installed Packages';
  SRegKey_KeymanEngine70_InstalledKeyboards_CU = SRegKey_KeymanEngine70_CU+'\Installed Keyboards';
  SRegKey_KeymanEngine70_ActiveKeyboards_CU = SRegKey_KeymanEngine70_CU+'\Active Keyboards';

  SRegKey_KeymanEngine70_InstalledPackages_LM = SRegKey_KeymanEngine70_LM+'\Installed Packages';
  SRegKey_KeymanEngine70_InstalledKeyboards_LM = SRegKey_KeymanEngine70_LM+'\Installed Keyboards';

  SRegKey_KeymanEngine80_ActiveLanguages_CU = SRegKey_KeymanEngine80_CU+'\Active Languages';
  SRegKey_KeymanEngine80_InstalledPackages_CU = SRegKey_KeymanEngine80_CU+'\Installed Packages';
  SRegKey_KeymanEngine80_InstalledKeyboards_CU = SRegKey_KeymanEngine80_CU+'\Installed Keyboards';
  SRegKey_KeymanEngine80_ActiveKeyboards_CU = SRegKey_KeymanEngine80_CU+'\Active Keyboards';

  SRegKey_KeymanEngine80_InstalledPackages_LM = SRegKey_KeymanEngine80_LM+'\Installed Packages';
  SRegKey_KeymanEngine80_InstalledKeyboards_LM = SRegKey_KeymanEngine80_LM+'\Installed Keyboards';

  SRegKey_KeymanEngine90_ActiveLanguages_CU = SRegKey_KeymanEngine90_CU+'\Active Languages';
  SRegKey_KeymanEngine90_InstalledPackages_CU = SRegKey_KeymanEngine90_CU+'\Installed Packages';
  SRegKey_KeymanEngine90_InstalledKeyboards_CU = SRegKey_KeymanEngine90_CU+'\Installed Keyboards';
  SRegKey_KeymanEngine90_ActiveKeyboards_CU = SRegKey_KeymanEngine90_CU+'\Active Keyboards';

  SRegKey_KeymanEngine90_InstalledPackages_LM = SRegKey_KeymanEngine90_LM+'\Installed Packages';
  SRegKey_KeymanEngine90_InstalledKeyboards_LM = SRegKey_KeymanEngine90_LM+'\Installed Keyboards';

  SRegKey_KeymanEngine100_ProductOptions_Desktop_CU = SRegKey_KeymanEngine100_CU + '\Product Options\desktop_pro';
  SRegKey_KeymanEngine90_ProductOptions_Desktop_Pro_CU = SRegKey_KeymanEngine90_CU + '\Product Options\desktop_pro';
  SRegKey_KeymanEngine80_ProductOptions_Desktop_Pro_CU = SRegKey_KeymanEngine80_CU + '\Product Options\desktop_pro';
  SRegKey_KeymanEngine70_ProductOptions_Desktop_Pro_CU = SRegKey_KeymanEngine70_CU + '\Product Options\desktop_pro';
  SRegKey_KeymanEngine70_ProductOptions_Desktop_Light_CU = SRegKey_KeymanEngine70_CU + '\Product Options\desktop_light';

  SRegKey_KeymanEngine100_ActiveLanguages_CU = SRegKey_KeymanEngine100_CU+'\Active Languages';
  SRegKey_KeymanEngine100_InstalledPackages_CU = SRegKey_KeymanEngine100_CU+'\Installed Packages';
  SRegKey_KeymanEngine100_InstalledKeyboards_CU = SRegKey_KeymanEngine100_CU+'\Installed Keyboards';
  SRegKey_KeymanEngine100_ActiveKeyboards_CU = SRegKey_KeymanEngine100_CU+'\Active Keyboards';

  SRegKey_KeymanEngine100_InstalledPackages_LM = SRegKey_KeymanEngine100_LM+'\Installed Packages';
  SRegKey_KeymanEngine100_InstalledKeyboards_LM = SRegKey_KeymanEngine100_LM+'\Installed Keyboards';

  SRegValue_Legacy_KeymanActiveHotkey = 'keyman active hotkey';

  { Upgrade temporary keys }

  SRegKey_UpgradeBackupPath_LM = '\'+SRegKey_KeymanEngine_LM+'\Upgrade Backup\';  // I2642
  SRegKey_UpgradeBackupPath_CU = '\'+SRegKey_KeymanEngine_CU+'\Upgrade Backup\';  // I2642

  SRegValue_UpgradeRunKeyman = 'Upgrade - Start Keyman with Windows';

const
  SFolderTavultesoft_                = 'Tavultesoft';                              // PF, PFC, AD, CAD

implementation

end.
