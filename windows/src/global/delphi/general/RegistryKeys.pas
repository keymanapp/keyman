(*
  Name:             RegistryKeys
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    24 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Add KeymanHotkeys, ProductInstallDir, Tike->IDE
                    14 Sep 2006 - mcdurdin - Add SRegKey_KeymanOSK, SRegKey_KeymanOSK_CharMap, SRegValue_OSK_*, SRegKey_CRM_*
                    06 Oct 2006 - mcdurdin - Add SRegValue_ConfigurationState
                    04 Jan 2007 - mcdurdin - Add proxy support
                    22 Jan 2007 - mcdurdin - Add saved licence number and export bmp pixel width entries
                    25 Jan 2007 - mcdurdin - Add layout display name
                    16 May 2007 - mcdurdin - I819 - Add test keyman functioning
                    30 May 2007 - mcdurdin - I765 - Add support for releasing modifiers after key press
                    30 May 2007 - mcdurdin - I817 - Restart installer if windows restarted during installation
                    23 Aug 2007 - mcdurdin - I927 - Add external editor
                    05 Nov 2007 - mcdurdin - I1087 - Language hotkeys for Pro
                    27 Mar 2008 - mcdurdin - I1288 - Add SRegValue_AutoOpenOSK
                    27 Mar 2008 - mcdurdin - I1375 - Add SRegValue_AutoSwtichOSKPages
                    27 Mar 2008 - mcdurdin - I1287 - Add SRegValue_SwitchLanguageWithKeyboard
                    27 Mar 2008 - mcdurdin - I1248 - Add SRegValue_EnableHints, SRegKey_Hints
                    27 Mar 2008 - mcdurdin - I1220 - Add SRegKey_LanguageGroups
                    27 Jan 2009 - mcdurdin - I1817 - Keyman Uniscribe Manager integration
                    09 Mar 2009 - mcdurdin - I1878 - Reset evaluations when installing a new release
                    23 Apr 2009 - mcdurdin - I1940 - Add MiniDump Diag key
                    25 May 2010 - mcdurdin - I1632 - Keyboard Options
                    25 May 2010 - mcdurdin - I2392 - Activation Client integration
                    22 Oct 2010 - mcdurdin - I2522 - Language Switch Hotkey window
                    10 Dec 2010 - mcdurdin - I2555 - transparency bugs in Remote Desktop
                    28 Feb 2011 - mcdurdin - I2720 - Prevent Keyman Desktop splash from showing multiple copies
                    28 Feb 2011 - mcdurdin - I1729 - Keyboards in uninstall feedback
                    18 Mar 2011 - mcdurdin - I2807 - Support for disabling addins
                    26 Jun 2012 - mcdurdin - I3379 - KM9 - Remove old Winapi references now in Delphi libraries
                    26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    20 Nov 2012 - mcdurdin - I3581 - V9.0 - KMTip needs to pass activated profile guid through to Keyman32 to switch keyboards
                    01 Dec 2012 - mcdurdin - I3613 - V9.0 - System shadow keyboards obsolete, strip out remaining code
                    10 Jan 2014 - mcdurdin - I4021 - V9.0 - Redesign Keyboard Wizard to integrate V9 features
                    16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard
                    28 May 2014 - mcdurdin - I4220 - V9.0 - Remove references to LoadKeyboardLayout, Preload, Substitutes, etc. and use only TSF
                    03 Jun 2014 - mcdurdin - I4248 - V9.0 - Refactor of kmtip
                    10 Oct 2014 - mcdurdin - I4436 - V9.0 - browser emulation control for kmshell breaks downlevel versions of Keyman
                    04 Nov 2014 - mcdurdin - I4506 - V9.0 - Add command to send email with targets
                    31 Dec 2014 - mcdurdin - I4553 - V9.0 - Upgrade to 476 or later requires recompile of all mnemonic layouts
                    06 Feb 2015 - mcdurdin - I4552 - V9.0 - Add mnemonic recompile option to ignore deadkeys
                    15 Apr 2015 - mcdurdin - I4658 - V9.0 - Add Keep in Touch screen
                    22 Jun 2015 - mcdurdin - I4751 - Add "open in code view" default option for keyboards
                    24 Jul 2015 - mcdurdin - I4796 - Refresh Keyman Developer look and feel for release
                    24 Aug 2015 - mcdurdin - I4873 - Branding editor needs smoother interactions with test window
*)
unit RegistryKeys;   // I4248

interface

uses
  KeymanVersion;

const

// DO NOT LOCALISE ANYTHING IN THIS FILE

{-------------------------------------------------------------------------------
 - Keyman Desktop keys and values                                              -
 ------------------------------------------------------------------------------}

  { Keyman keys }

  SRegKey_Software            = 'Software';                                         // LM CU
  SRegKey__Tavultesoft        = SRegKey_Software            + '\Tavultesoft';       // LM CU
  SRegKey_KeymanRoot          = SRegKey_Software            + '\Keyman';
  SRegKey_KeymanEngineRoot    = SRegKey_KeymanRoot          + '\Keyman Engine';     // LM CU

  SRegKey_SoftwareKeyman = SRegKey_Software + '\Keyman';

  { Keyman base key and values }

  SRegKey_KeymanEngine        = SRegKey_KeymanEngineRoot    + '\'+SKeymanVersion;               // LM CU
  SRegKey_KeymanEngineDiag    = SRegKey_KeymanEngine + '\Diag';                     // CU

  SregKey_KeymanHotkeys       = SRegKey_KeymanEngine + '\hotkeys';

  SRegValue_RootKeyboardAdminPath   = 'root keyboard admin path';                   // LM
  SRegValue_RootKeyboardUserPath    = 'root keyboard user path';                    // CU

  SRegValue_MnemonicLayoutVersion = 'mnemonic layout version';                      // LM DWord   // I4553

  SRegValue_Transparency = 'transparency';                                          // CU, 0 or missing = heuristic, 1 = force enable, 2 = force disable, I2555

//  SRegValue_UnknownLayoutID         = 'unknown layout id';                          // LM
    SRegValue_Legacy_Default_UnknownLayoutID = '000005FE';   // I4220

  SRegValue_KeymanDebug             = 'debug';                                      // CU

  SRegValue_DefaultCustomisation    = 'default customisation';                      // LM

  SRegValue_ShowStartup             = 'show startup';                        // CU
  SRegValue_ShowWelcome             = 'show welcome';                        // CU
  //SRegValue_NoCheckAssociations     = 'no check associations';                      // CU
  SRegValue_UseAdvancedInstall      = 'use advanced install';                       // CU

//TOUCH    SRegValue_UseTouchLayout          = 'use touch layout';                           // CU, default false

  SRegValue_AltGrCtrlAlt                = 'simulate altgr';                         // CU
  SRegValue_KeyboardHotKeysAreToggle    = 'hotkeys are toggles';                    // CU
  SRegValue_ReleaseShiftKeysAfterKeyPress = 'release shift keys after key press';   // CU
  SRegValue_TestKeymanFunctioning       = 'test keyman functioning';                // CU, default true

  SRegValue_CreateStartMenuAsSubfolders = 'create start menu as subfolders';        // CU
  SRegValue_CreateUninstallEntries      = 'create uninstall entries';               // CU

  SRegValue_ProxyServer                 = 'proxy server';
  SRegValue_ProxyPort                   = 'proxy port';
  SRegValue_ProxyLogin                  = 'proxy login';

  SRegValue_AutoOpenOSK                       = 'auto open osk'; // CU, default true
  SRegValue_AutoSwitchOSKPages                = 'auto switch osk pages'; // CU, default true
  SRegValue_SwitchLanguageWithKeyboard        = 'switch language with keyboard'; // CU, default true
  SRegValue_SwitchLanguageForAllApplications  = 'switch language for all applications'; // CU, default true

  { On Screen Keyboard Settings }

  SRegKey_KeymanOSK = SRegKey_KeymanEngine + '\On Screen Keyboard';
  SRegKey_KeymanOSK_CharMap = SRegKey_KeymanOSK + '\Character Map';

  SRegValue_OSK_ShowVisualKeyboard          = 'show visual keyboard';                // CU
  SRegValue_OSK_FadeVisualKeyboard          = 'fade visual keyboard';                // CU
  SRegValue_OSK_ActivePage                  = 'active page';                         // CU
  SRegValue_OSK_Position                    = 'position';                            // CU
  SRegValue_OSK_ShowToolBar                 = 'show toolbar';                        // CU

  //SRegValue_VisualKeyboardKeyCap        = 'visual keyboard key cap';               // CU

  SRegValue_ConfigurationState              = 'configuration state';                 // CU

  SRegKey_KeymanRegisteredWindows           = SRegKey_KeymanRoot+'\Window References';       // CU  // I2720

  { Hint Settings - CU }

  SRegValue_EnableHints = 'enable hints';
  SRegKey_Hints = 'hints';

  { Keyman Desktop }

  SRegKey_KeymanDesktopRoot = SRegKey_KeymanRoot + '\Keyman Desktop';
  SRegKey_KeymanDesktop = SRegKey_KeymanDesktopRoot + '\' + SKeymanVersion;

  { Other Keyman Settings }

  SRegValue_DeadkeyConversionMode       = 'deadkey conversion mode';                // CU   // I4552
  SRegValue_UnderlyingLayout            = 'underlying layout';                      // CU

  SRegKey_AppInitDLLs                   = 'Software\Microsoft\Windows NT\CurrentVersion\Windows';  // LM
  SRegValue_AppInitDLLs                 = 'AppInit_DLLs';                                          // LM

  SRegKey_KeyboardLayoutToggle          = 'keyboard layout\toggle';                 // CU  // I2522
  SRegValue_Toggle_Hotkey               = 'Hotkey';
  SRegValue_Toggle_LanguageHotkey       = 'Language Hotkey';
  SRegValue_Toggle_LayoutHotkey         = 'Layout Hotkey';

  SRegValue_CharMapSourceData = 'charmap source data';              // LM

  SRegValue_AvailableLanguages = 'available languages'; //CU
  SRegValue_CurrentLanguage    = 'current language';    //CU

  { ActiveKeyboards }

  SRegKey_ActiveKeyboards = SRegKey_KeymanEngine + '\Active Keyboards';                   // CU

  SRegKeyNode_KeyboardOptions = 'Options';  // CU\ActiveKeyboards\<id>\Options

  SRegValue_KeymanID            = 'keyman id';                                      // CU
  SRegValue_Legacy_KeymanInstalledLanguage = 'keyman installed language';                  // CU   // I4220

  SRegKey_SharedOptionsSubKey = 'shared';                                             // CU

  { ActiveLanguages }

  SRegKey_ActiveLanguages = SRegKey_KeymanEngine + '\Active Languages';                   // CU
  SRegKey_LanguageHotkeys = SRegKey_KeymanEngine + '\Language Hotkeys';                   // CU

  { InstalledKeyboards }

  SRegKey_InstalledKeyboards = SRegKey_KeymanEngine + '\Installed Keyboards';             // LM CU

  SRegValue_KeymanFile_MnemonicOverride = 'keyman file mnemonic override';          // LM   // I4169
  SRegValue_KeymanFile_MnemonicOverride_Deadkey = 'keyman file mnemonic override deadkey';          // LM   // I4552
  SRegValue_KeymanDefaultHotkey = 'keyman default hotkey';                          // LM CU
  SRegValue_KeymanFile          = 'keyman file';                                    // LM CU
  SRegValue_Legacy_KeymanKeyboardID    = 'keyman keyboard id';                             // LM CU   // I3613
  SRegValue_PackageName         = 'package name';                                   // LM CU
  SRegValue_Legacy_DefaultLanguageID   = 'default language id';                            // LM CU   // I4220
  SRegValue_VisualKeyboard      = 'visual keyboard';                                // LM CU

  SRegValue_KeymanProfileGUID   = 'profile guid';                                   // LM CU   // I3581

  { Language Profiles }

  {$MESSAGE HINT 'Refactor this to use SRegKeyNode naming (no prefix \); fixup references'}
  SRegKey_LanguageProfiles      = { SRegKey_InstalledKeyboards + '\<keyboard>' } '\Language Profiles';  // LM CU
  SRegValue_LanguageProfileLangID = 'LangID';
  SRegValue_LanguageProfileLocale = 'Locale';

  { InstalledPackages }

  SRegKey_InstalledPackages = SRegKey_KeymanEngine + '\Installed Packages';               // LM CU

  SRegValue_PackageDescription  = 'package description';                            // LM CU
  SRegValue_PackageFile  = 'package inf file';                                      // LM CU

  { Latin keyboard cache }

  SRegKey_LatinKeyboardCache = SRegKey_KeymanEngine + '\Latin Keyboard Cache';      // LM   // I4169

  { Internet Explorer feature control }

  SRegKey_InternetExplorerFeatureBrowserEmulation = 'Software\Microsoft\Internet Explorer\Main\FeatureControl\FEATURE_BROWSER_EMULATION';   // I4436

  { System KeyboardLayouts }

  SRegKey_KeyboardLayouts = 'System\CurrentControlSet\Control\keyboard layouts';    // LM

  SRegValue_LayoutDisplayName                = 'layout display name';               // LM
  SRegValue_KeyboardLayoutText               = 'layout text';                       // LM
  SRegValue_KeyboardLayoutFile               = 'layout file';                       // LM
  SRegValue_KeyboardLayoutID                 = 'layout id';                         // LM
  SRegValue_Legacy_KeyboardKeymanInstall            = 'keyman install';                    // LM   // I3613
  SRegValue_Legacy_KeyboardKeymanOriginalLayoutText = 'keyman original layout text';       // LM   // I3613
  SRegValue_Legacy_KeyboardKeymanOriginalLayoutFile = 'keyman original layout file';       // LM   // I3613
  SRegValue_Legacy_KeyboardKeymanOriginalLayoutID   = 'keyman original layout id';         // LM   // I3613
  SRegValue_Legacy_KeyboardKeymanName               = 'keyman name';                       // LM   // I3613

  { User-installed keyboards/languages }

  SRegKey_KeyboardLayout = 'keyboard layout';                                       // CU
  SRegKey_KeyboardLayoutPreload = 'keyboard layout\preload';                        // CU
  SRegKey_KeyboardLayoutSubstitutes = 'keyboard layout\substitutes';                // CU

  { User profile keys }

  //SRegKey_NTProfileList = 'Software\Microsoft\Windows NT\CurrentVersion\ProfileList';

  { Font keys }

  SRegKey_FontList   = 'Software\Microsoft\Windows\CurrentVersion\Fonts';           // LM
  SRegKey_NTFontList = 'Software\Microsoft\Windows NT\CurrentVersion\Fonts';        // LM

  { Run with Windows keys }

  SRegKey_WindowsRun                   = 'Software\Microsoft\Windows\CurrentVersion\Run';  // CU, LM
  SRegKey_WindowsRunOnce               = 'Software\Microsoft\Windows\CurrentVersion\RunOnce';  // CU, LM
  SRegValue_WindowsRunOnce_Setup       = 'keyman-setup.exe';                                // CU

  SRegValue_WindowsRun_Keyman          = 'Keyman';

  SRegValue_LanguageCheckDisabledItems = 'language check disabled items'; // CU

  { Upgrade temporary keys }

  SRegKey_UpgradeBackupPath = '\'+SRegKey_KeymanEngine+'\Upgrade Backup\';  // I2642

{-------------------------------------------------------------------------------
 - Keyman Developer keys and values                                            -
 ------------------------------------------------------------------------------}

  { Keyman Developer Keys }

  SRegKey_KeymanDeveloperRoot = SRegKey_KeymanRoot          + '\Keyman Developer';  // LM CU
  SRegKey_KeymanDeveloper     = SRegKey_KeymanDeveloperRoot + '\'+SKeymanVersion;               // LM CU
  SRegKey_IDE                 = SRegKey_KeymanDeveloper     + '\IDE';                // CU
  SRegKey_IDEDock             = SRegKey_IDE                 + '\Dock';              // CU
  SRegKey_IDEFiles            = SRegKey_IDE                 + '\Files';               // CU
  SRegKey_IDEOptions          = SRegKey_IDE                 + '\Options';             // CU
  SRegKey_IDECharacterMap     = SRegKey_IDE                 + '\Character Map';             // CU
  SRegKey_IDEColours          = SRegKey_IDE                 + '\Colours';             // CU
  SRegKey_IDEEditFonts        = SRegKey_IDE                 + '\EditFonts';           // CU
  SRegKey_IDETestFonts        = SRegKey_IDE                 + '\TestFonts';           // CU
  SRegKey_IDEVisualKeyboard   = SRegKey_IDE                 + '\VisualKeyboard';      // CU
  SRegKey_IDEToolbars         = SRegKey_IDE                 + '\Toolbars';            // CU
  SRegKey_KCT                 = SRegKey_KeymanDeveloper     + '\KCT';               // LM CU
  SRegKey_KCTFiles            = SRegKey_KCT                 + '\Files';             // CU

  SRegKey_IDEOnline          = SRegKey_IDE                + '\Online';              // CU
  SRegKey_IDE_BrandingPackTest = SRegKey_IDE              + '\Branding Pack\Test';  // CU   // I4873

  SRegKey_CRM                = SRegKey_KeymanDeveloper    + '\CRM';                 // CU

  SRegValue_CheckForUpdates   = 'check for updates'; // CU
  SRegValue_LastUpdateCheckTime = 'last update check time'; // CU

  SRegValue_UpdateCheck_UseProxy = 'update check use proxy'; // CU
  SRegValue_UpdateCheck_ProxyHost = 'update check proxy host'; // CU
  SRegValue_UpdateCheck_ProxyPort = 'update check proxy port'; // CU

  SRegValue_KeepInTouchShown = 'keep in touch shown'; // CU. bool   // I4658

  //SRegValue_OnlineUsername    = 'online username';
  //SRegValue_OnlinePassword    = 'online password';
  SRegValue_OnlineLogin = 'online login';

  { SRegKey_CRM values }

  SRegValue_CRM_FileName            = 'filename';                                   // CU
  SRegValue_CRM_ActivePage          = 'active page';                                // CU
  SRegValue_CRM_ProductColumns      = 'product grid';                               // CU
  SRegValue_CRM_CustomerColumns      = 'customer grid';                             // CU
  SRegValue_CRM_PurchaseColumns      = 'purchase grid';                             // CU

  { SRegKey_KeymanDeveloper values }

  //SRegValue_ShowStartup             = 'show startup';                           // CU -- see Keyman Desktop option of same name
  SRegValue_Evaluation              = 'evaluation';                                 // CU
  SRegValue_ActiveProject           = 'active project';                             // CU

  { SRegKey_IDE values }

  SRegValue_IDECharacterMap_CellSize   = 'cell size';     // CU [int]
  SRegValue_IDECharacterMap_Filter     = 'filter';        // CU [str]
  SRegValue_IDECharacterMap_Font       = 'font';          // CU [str]
  SRegValue_IDECharacterMap_Quality    = 'quality';       // CU [int]
  SRegValue_IDECharacterMap_InsertMode = 'insert mode';   // CU [int]
  SRegValue_IDECharacterMap_Character  = 'character';     // CU [int]

  SRegValue_IDEMRU                 = 'MRU';                                        // CU
  SRegValue_CharMapSize             = 'char map size';                              // CU
//SRegValue_IDERegressionTestPath  = 'regression test path';                       // CU

  { SRegKey_IDEVisualKeyboard values }

  SRegValue_IDEVKbd_ExportBMPMulti   = 'export bmp multi';                         // CU
  SRegValue_IDEVKbd_ExportBMPANSI    = 'export bmp ansi';                          // CU
  SRegValue_IDEVKbd_ExportBMPUnicode = 'export bmp unicode';                       // CU
  SRegValue_IDEVKbd_ExportBMPPixelWidth = 'export bmp pixel width';                // CU

  { SRegKey_IDEOptions values }

  SRegValue_IDEOptLinkFontSizes    = 'link font sizes';                            // CU
  SRegValue_IDEOptUseTabCharacter  = 'use tab char';                               // CU
  SRegValue_IDEOptIndentSize       = 'indent size';                                // CU
  SRegValue_IDEOptDocVirusCheck    = 'warn if packaging doc files';                // CU
  SRegValue_IDEOptUseSyntaxHighlighting = 'use syntax highlighting';               // CU
  SRegValue_IDEOptToolbarVisible   = 'toolbar visible';                            // CU
  SRegValue_IDEOptUseOldDebugger   = 'use old debugger';                           // CU

  SRegValue_IDEOptShowStartupDialog       = 'show startup dialog';                 // CU
  //SRegValue_IDEOptShowStartupHelperDialog = 'show startup helper dialog';          // CU

  SRegValue_IDEOptDebuggerBreakWhenExitingLine = 'debugger break when exiting line';    // CU
  SRegValue_IDEOptDebuggerSingleStepAfterBreak = 'debugger single step after break';    // CU
  SRegValue_IDEOptDebuggerShowStoreOffset      = 'debugger show store offset';          // CU
  SRegValue_IDEOptDebuggerAutoRecompileWithDebugInfo = 'debugger recompile with debug info'; // CU

  SRegValue_IDEOptWebHostPort = 'web host port';   // I4021

  SRegValue_IDEOptCharMapDisableDatabaseLookups = 'char map disable database lookups';  // CU
  SRegValue_IDEOptCharMapAutoLookup             = 'char map auto lookup';               // CU

  SRegValue_IDEOptMultipleInstances = 'multiple instances';                         // CU

  SRegValue_IDEOptOpenKeyboardFilesInSourceView = 'open keyboard files in source view';  // CU   // I4751

  SRegValue_IDEDisplayTheme = 'display theme';   // I4796

  SRegValue_IDEOptExternalEditorPath = 'external editor path';                      // CU

  SRegValue_IDEOptSMTPServer = 'smtp server';                                       // CU   // I4506
  SRegValue_IDEOptTestEmailAddresses = 'test email addresses';                      // CU   // I4506

  { SRegKey_KCT values }

  SRegValue_KCTTemplatePath = 'template path';                                      // LM

{-------------------------------------------------------------------------------
 - Shared keys and values                                                      -
 ------------------------------------------------------------------------------}

  { SRegKey_Keyman and SRegKey_KeymanDeveloper values -- SHARED }

  SRegValue_RootPath                = 'root path';                                  // LM
  SRegValue_RegistrationKey         = 'registration key';                           // LM

  { Uninstall feedback keyboard list }

  SRegKey_UninstallBackupKeyboards = 'Software\KeymanDesktop_UninstallKeyboards';        // CU  // I1729

{-------------------------------------------------------------------------------
 -
 -------------------------------------------------------------------------------}

  { Debugging keys and values }

  SRegKey_KeymanDebug = SRegKey_KeymanRoot + '\Debug';
  SRegValue_Debug_TikeDebugMode = 'TikeDebugMode';

  SRegKey_DelphiProjectManager = SRegKey_SoftwareKeyman + '\DelphiProjectManager';
  SRegValue_CallbackWindow = 'CallbackWindow';

  SRegKey_Keyman_Exception = SRegKey_KeymanRoot + '\Exception';
  SRegValue_SymbolPath = 'SymbolPath';

{-------------------------------------------------------------------------------
 - Upgrade-related keys
 -------------------------------------------------------------------------------}

const
  SRegKey_TavultesoftKeyman = SRegKey__Tavultesoft + '\Keyman';
  SRegKey_Keyman60 = SRegKey_TavultesoftKeyman + '\'+SKeymanVersion60;
  SRegKey_Keyman70 = SRegKey_TavultesoftKeyman + '\'+SKeymanVersion70;
  SRegKey_Keyman80 = SRegKey_TavultesoftKeyman + '\'+SKeymanVersion80;

  SRegKey_TavultesoftKeymanEngine = SRegKey__Tavultesoft + '\Keyman Engine';
  SRegKey_KeymanEngine70 = SRegKey_TavultesoftKeymanEngine + '\'+SKeymanVersion70;
  SRegKey_KeymanEngine80 = SRegKey_TavultesoftKeymanEngine + '\'+SKeymanVersion80;
  SRegKey_KeymanEngine90 = SRegKey_TavultesoftKeymanEngine + '\'+SKeymanVersion90;

  SRegKey_Keyman60_InstalledPackages = SRegKey_Keyman60 + '\Installed Packages';
  SRegKey_Keyman60_InstalledKeyboards = SRegKey_Keyman60 + '\Installed Keyboards';
  SRegKey_Keyman60_ActiveKeyboards = SRegKey_Keyman60 + '\Active Keyboards';

  SRegKey_KeymanEngine70_InstalledPackages = SRegKey_KeymanEngine70+'\Installed Packages';
  SRegKey_KeymanEngine70_InstalledKeyboards = SRegKey_KeymanEngine70+'\Installed Keyboards';
  SRegKey_KeymanEngine70_ActiveKeyboards = SRegKey_KeymanEngine70+'\Active Keyboards';

  SRegKey_KeymanEngine80_ActiveLanguages = SRegKey_KeymanEngine80+'\Active Languages';
  SRegKey_KeymanEngine80_InstalledPackages = SRegKey_KeymanEngine80+'\Installed Packages';
  SRegKey_KeymanEngine80_InstalledKeyboards = SRegKey_KeymanEngine80+'\Installed Keyboards';
  SRegKey_KeymanEngine80_ActiveKeyboards = SRegKey_KeymanEngine80+'\Active Keyboards';

  SRegKey_KeymanEngine90_ActiveLanguages = SRegKey_KeymanEngine90+'\Active Languages';
  SRegKey_KeymanEngine90_InstalledPackages = SRegKey_KeymanEngine90+'\Installed Packages';
  SRegKey_KeymanEngine90_InstalledKeyboards = SRegKey_KeymanEngine90+'\Installed Keyboards';
  SRegKey_KeymanEngine90_ActiveKeyboards = SRegKey_KeymanEngine90+'\Active Keyboards';

  SRegKey_KeymanEngine90_ProductOptions_Desktop_Pro = SRegKey_KeymanEngine90 + '\Product Options\desktop_pro';
  SRegKey_KeymanEngine80_ProductOptions_Desktop_Pro = SRegKey_KeymanEngine80 + '\Product Options\desktop_pro';
  SRegKey_KeymanEngine70_ProductOptions_Desktop_Pro = SRegKey_KeymanEngine70 + '\Product Options\desktop_pro';
  SRegKey_KeymanEngine70_ProductOptions_Desktop_Light = SRegKey_KeymanEngine70 + '\Product Options\desktop_light';

  SRegValue_Legacy_KeymanActiveHotkey = 'keyman active hotkey';

// Fixed path names
const
  // PF = CSIDL_PROGRAM_FILES
  // PFC = CSIDL_PROGRAM_FILES_COMMON
  // AD = CSIDL_APPDATA
  // CAD = CSIDL_COMMON_APPDATA

  SFolderTavultesoft_                = 'Tavultesoft';                              // PF, PFC, AD, CAD
  SFolderKeymanRoot                 = 'Keyman';
  SFolderKeymanEngine               = SFolderKeymanRoot + '\Keyman Engine '+SKeymanVersion;
  SFolderKeymanKeyboard             = SFolderKeymanEngine + '\Keyboard';
  SFolderShared                     = SFolderKeymanRoot + '\Shared Data '+SKeymanVersion;
  SFolderSharedDatabases            = SFolderShared + '\Databases';
  SFolderKeymanDeveloper            = SFolderKeymanRoot + '\Keyman Developer '+SKeymanVersion;
  SFolderKeymanDeveloperTemplates   = SFolderKeymanDeveloper + '\Templates';
  SFolderRegressionTests            = SFolderKeymanDeveloper + '\Regression Tests';

  SFolder_CachedInstallerFiles      = SFolderKeymanRoot + '\Cached Installer Files';

  SFolderKeymanEngineDiag           = SFolderKeymanRoot + '\Diag';

function BuildKeyboardOptionKey(const KeyboardID: string): string;
function BuildKeyboardLanguageProfilesKey(const KeyboardID: string): string;

implementation

function BuildKeyboardOptionKey(const KeyboardID: string): string;
begin
  Result := SRegKey_ActiveKeyboards + '\' + KeyboardID + '\' + SRegKeyNode_KeyboardOptions;
end;

function BuildKeyboardLanguageProfilesKey(const KeyboardID: string): string;
begin
  Result := SRegKey_InstalledKeyboards + '\' + KeyboardID + SRegKey_LanguageProfiles;
end;

end.

