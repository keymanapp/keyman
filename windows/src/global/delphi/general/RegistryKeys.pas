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

{$IFDEF WIN64}
  SRegKey_Software_LM            = 'Software\WOW6432Node';                             // LM
{$ELSE}
  SRegKey_Software_LM            = 'Software';                                         // LM
{$ENDIF}
  SRegKey_Software_CU            = 'Software';                                         // CU
  SRegKey_KeymanRoot_LM          = SRegKey_Software_LM         + '\Keyman';            // LM
  SRegKey_KeymanRoot_CU          = SRegKey_Software_CU         + '\Keyman';            // CU
  SRegKey_KeymanEngineRoot_LM    = SRegKey_KeymanRoot_LM       + '\Keyman Engine';     // LM
  SRegKey_KeymanEngineRoot_CU    = SRegKey_KeymanRoot_CU       + '\Keyman Engine';     // CU

  SRegKey_SoftwareKeyman_LM      = SRegKey_KeymanRoot_LM;
  SRegKey_SoftwareKeyman_CU      = SRegKey_KeymanRoot_CU;

  { Keyman base key and values }

  SRegKey_KeymanEngine_LM     = SRegKey_KeymanEngineRoot_LM;     // LM
  SRegKey_KeymanEngine_CU     = SRegKey_KeymanEngineRoot_CU;     // CU
  SRegKey_KeymanEngineDiag_CU = SRegKey_KeymanEngine_CU + '\Diag';                     // CU

  SregKey_KeymanHotkeys_CU    = SRegKey_KeymanEngine_CU + '\hotkeys';

  SRegValue_RootKeyboardAdminPath   = 'root keyboard admin path';                   // LM
  SRegValue_RootKeyboardUserPath    = 'root keyboard user path';                    // CU

  SRegValue_MnemonicLayoutVersion = 'mnemonic layout version';                      // LM DWord   // I4553

  SRegValue_Engine_OEMProductPath = 'oem product path';

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

  SRegKey_KeymanOSK_CU         = SRegKey_KeymanEngine_CU + '\On Screen Keyboard';
  SRegKey_KeymanOSK_CharMap_CU = SRegKey_KeymanOSK_CU + '\Character Map';

  SRegValue_OSK_ShowVisualKeyboard          = 'show visual keyboard';                // CU
  SRegValue_OSK_FadeVisualKeyboard          = 'fade visual keyboard';                // CU
  SRegValue_OSK_ActivePage                  = 'active page';                         // CU
  SRegValue_OSK_Position                    = 'position';                            // CU
  SRegValue_OSK_ShowToolBar                 = 'show toolbar';                        // CU

  //SRegValue_VisualKeyboardKeyCap        = 'visual keyboard key cap';               // CU

  SRegValue_ConfigurationState              = 'configuration state';                 // CU

  SRegKey_KeymanRegisteredWindows_CU        = SRegKey_KeymanRoot_CU+'\Window References';       // CU  // I2720

  { Hint Settings - CU }

  SRegValue_EnableHints = 'enable hints';
  SRegSubKey_Hints = 'hints';

  { Keyman Desktop }

  SRegKey_KeymanDesktopRoot_CU = SRegKey_KeymanRoot_CU + '\Keyman Desktop';
  SRegKey_KeymanDesktopRoot_LM = SRegKey_KeymanRoot_LM + '\Keyman Desktop';
  SRegKey_KeymanDesktop_CU     = SRegKey_KeymanDesktopRoot_CU;
  SRegKey_KeymanDesktop_LM     = SRegKey_KeymanDesktopRoot_LM;

  { Other Keyman Settings }

  SRegValue_DeadkeyConversionMode       = 'deadkey conversion mode';                // CU   // I4552
  SRegValue_UnderlyingLayout            = 'underlying layout';                      // CU

//  SRegKey_AppInitDLLs                   = 'Software\Microsoft\Windows NT\CurrentVersion\Windows';  // LM
//  SRegValue_AppInitDLLs                 = 'AppInit_DLLs';                                          // LM

  SRegKey_KeyboardLayoutToggle          = 'keyboard layout\toggle';                 // CU  // I2522
  SRegValue_Toggle_Hotkey               = 'Hotkey';
  SRegValue_Toggle_LanguageHotkey       = 'Language Hotkey';
  SRegValue_Toggle_LayoutHotkey         = 'Layout Hotkey';

  SRegValue_CharMapSourceData = 'charmap source data';              // LM

  SRegValue_AvailableLanguages = 'available languages'; //CU
  SRegValue_CurrentLanguage    = 'current language';    //CU

  { ActiveKeyboards }

  SRegKey_ActiveKeyboards_CU = SRegKey_KeymanEngine_CU + '\Active Keyboards';                   // CU

  SRegSubKey_KeyboardOptions = 'Options';  // CU\ActiveKeyboards\<id>\Options

  SRegValue_KeymanID            = 'keyman id';                                      // CU
  SRegValue_Legacy_KeymanInstalledLanguage = 'keyman installed language';                  // CU   // I4220

  SRegSubKey_SharedOptions = 'shared';                                             // CU

  { ActiveLanguages }

  SRegKey_ActiveLanguages_CU = SRegKey_KeymanEngine_CU + '\Active Languages';                   // CU
  SRegKey_LanguageHotkeys_CU = SRegKey_KeymanEngine_CU + '\Language Hotkeys';                   // CU

  { InstalledKeyboards }

  SRegKey_InstalledKeyboards_LM = SRegKey_KeymanEngine_LM + '\Installed Keyboards';             // LM
  SRegKey_InstalledKeyboards_CU = SRegKey_KeymanEngine_CU + '\Installed Keyboards';             // CU

  SRegValue_KeymanFile_MnemonicOverride = 'keyman file mnemonic override';          // LM   // I4169
  SRegValue_KeymanFile_MnemonicOverride_Deadkey = 'keyman file mnemonic override deadkey';          // LM   // I4552
  SRegValue_KeymanFile          = 'keyman file';                                    // LM CU
  SRegValue_Legacy_KeymanKeyboardID    = 'keyman keyboard id';                             // LM CU   // I3613
  SRegValue_PackageName         = 'package name';                                   // LM CU
  SRegValue_Legacy_DefaultLanguageID   = 'default language id';                            // LM CU   // I4220
  SRegValue_VisualKeyboard      = 'visual keyboard';                                // LM CU

  SRegValue_KeymanProfileGUID   = 'profile guid';                                   // LM CU   // I3581

  SRegSubKey_SuggestedLanguages = 'Suggested Languages';                            // LM

  { Language Profiles }

  {$MESSAGE HINT 'Refactor this to use SRegKeyNode naming (no prefix \); fixup references'}
  SRegSubKey_LanguageProfiles      = { SRegKey_InstalledKeyboards + '\<keyboard>' } '\Language Profiles';  // LM CU
  SRegValue_LanguageProfileLangID = 'LangID';
  SRegValue_LanguageProfileLocale = 'Locale';
  SRegValue_LanguageProfileName = 'Name';

  { InstalledPackages }

  SRegKey_InstalledPackages_LM = SRegKey_KeymanEngine_LM + '\Installed Packages';               // LM
  SRegKey_InstalledPackages_CU = SRegKey_KeymanEngine_CU + '\Installed Packages';               // CU

  SRegValue_PackageDescription  = 'package description';                            // LM CU
  SRegValue_PackageFile  = 'package inf file';                                      // LM CU

  { Latin keyboard cache }

  SRegKey_LatinKeyboardCache_LM = SRegKey_KeymanEngine_LM + '\Latin Keyboard Cache';      // LM   // I4169

  { Internet Explorer feature control }

  SRegKey_InternetExplorerFeatureBrowserEmulation_CU = 'Software\Microsoft\Internet Explorer\Main\FeatureControl\FEATURE_BROWSER_EMULATION';   // I4436

  { System KeyboardLayouts }

  SRegKey_KeyboardLayouts_LM = 'System\CurrentControlSet\Control\keyboard layouts';    // LM

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

  SRegKey_KeyboardLayout_CU = 'keyboard layout';                                       // CU
  SRegKey_KeyboardLayoutPreload_CU = 'keyboard layout\preload';                        // CU
  SRegKey_KeyboardLayoutSubstitutes_CU = 'keyboard layout\substitutes';                // CU

  { User profile keys }

  //SRegKey_NTProfileList = 'Software\Microsoft\Windows NT\CurrentVersion\ProfileList';

  { Font keys }

  SRegKey_FontList_LM   = 'Software\Microsoft\Windows\CurrentVersion\Fonts';           // LM
  SRegKey_NTFontList_LM = 'Software\Microsoft\Windows NT\CurrentVersion\Fonts';        // LM

  { Run with Windows keys }

  SRegKey_WindowsRun_CU                   = 'Software\Microsoft\Windows\CurrentVersion\Run';  // CU, LM
  SRegKey_WindowsRun_LM                   = 'Software\Microsoft\Windows\CurrentVersion\Run';  // CU, LM
  SRegKey_WindowsRun_Wow64_LM             = 'Software\Wow6432Node\Microsoft\Windows\CurrentVersion\Run';  // CU, LM
  SRegKey_WindowsRunOnce_CU               = 'Software\Microsoft\Windows\CurrentVersion\RunOnce';  // CU, LM
  SRegKey_WindowsRunOnce_LM               = 'Software\Microsoft\Windows\CurrentVersion\RunOnce';  // CU, LM
  SRegKey_WindowsRunOnce_Wow64_LM         = 'Software\Wow6432Node\Microsoft\Windows\CurrentVersion\RunOnce';  // CU, LM

  SRegValue_WindowsRunOnce_Setup       = 'keyman-setup.exe';                                // CU

  SRegValue_WindowsRun_Keyman          = 'Keyman';

  SRegValue_LanguageCheckDisabledItems = 'language check disabled items'; // CU

{-------------------------------------------------------------------------------
 - Keyman Developer keys and values                                            -
 ------------------------------------------------------------------------------}

  { Keyman Developer Keys }

  SRegKey_KeymanDeveloperRoot_CU = SRegKey_KeymanRoot_CU          + '\Keyman Developer';  // LM CU
  SRegKey_KeymanDeveloper_CU     = SRegKey_KeymanDeveloperRoot_CU;               // LM CU

  SRegKey_KeymanDeveloperRoot_LM = SRegKey_KeymanRoot_LM          + '\Keyman Developer';  // LM CU
  SRegKey_KeymanDeveloper_LM     = SRegKey_KeymanDeveloperRoot_LM;               // LM CU

  SRegKey_IDE_CU                 = SRegKey_KeymanDeveloper_CU     + '\IDE';                // CU
  SRegKey_IDEDock_CU             = SRegKey_IDE_CU                 + '\Dock';              // CU
  SRegKey_IDEFiles_CU            = SRegKey_IDE_CU                 + '\Files';               // CU
  SRegKey_IDEOptions_CU          = SRegKey_IDE_CU                 + '\Options';             // CU
  SRegKey_IDECharacterMap_CU     = SRegKey_IDE_CU                 + '\Character Map';             // CU
  SRegKey_IDEColours_CU          = SRegKey_IDE_CU                 + '\Colours';             // CU
  SRegKey_IDEEditFonts_CU        = SRegKey_IDE_CU                 + '\EditFonts';           // CU
  SRegKey_IDETestFonts_CU        = SRegKey_IDE_CU                 + '\TestFonts';           // CU
  SRegKey_IDEVisualKeyboard_CU   = SRegKey_IDE_CU                 + '\VisualKeyboard';      // CU
  SRegKey_IDEToolbars_CU         = SRegKey_IDE_CU                 + '\Toolbars';            // CU
//  SRegKey_KCT                 = SRegKey_KeymanDeveloper_CU     + '\KCT';               // LM CU
//  SRegKey_KCTFiles            = SRegKey_KCT_CU                 + '\Files';             // CU

//  SRegKey_IDEOnline          = SRegKey_IDE_CU                + '\Online';              // CU
//  SRegKey_IDE_BrandingPackTest = SRegKey_IDE_CU              + '\Branding Pack\Test';  // CU   // I4873

//  SRegKey_CRM                = SRegKey_KeymanDeveloper    + '\CRM';                 // CU

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
  SRegValue_IDEOptEditorTheme      = 'editor theme';                               // CU

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

  SRegValue_IDEOpt_WebLadderLength = 'web ladder length';                           // CU
  CRegValue_IDEOpt_WebLadderLength_Default = 100;

  { SRegKey_KCT values }

//  SRegValue_KCTTemplatePath = 'template path';                                      // LM

{-------------------------------------------------------------------------------
 - Shared keys and values                                                      -
 ------------------------------------------------------------------------------}

  { SRegKey_Keyman and SRegKey_KeymanDeveloper values -- SHARED }

  SRegValue_RootPath                = 'root path';                                  // LM
  SRegValue_RegistrationKey         = 'registration key';                           // LM

  { Uninstall feedback keyboard list }

  SRegKey_UninstallBackupKeyboards_CU = 'Software\KeymanDesktop_UninstallKeyboards';        // CU  // I1729

  { Debugging keys and values }

  SRegKey_KeymanDebug_CU = SRegKey_KeymanRoot_CU + '\Debug';
  SRegValue_Debug_TikeDebugMode = 'TikeDebugMode';

  SRegKey_DelphiProjectManager_CU = SRegKey_SoftwareKeyman_CU + '\DelphiProjectManager';
  SRegValue_CallbackWindow = 'CallbackWindow';

  SRegKey_Keyman_Exception_CU = SRegKey_KeymanRoot_CU + '\Exception';
  SRegValue_SymbolPath = 'SymbolPath';


// Fixed path names
const
  // PF = CSIDL_PROGRAM_FILES
  // PFC = CSIDL_PROGRAM_FILES_COMMON
  // AD = CSIDL_APPDATA
  // CAD = CSIDL_COMMON_APPDATA

  SFolderKeymanRoot                 = 'Keyman';
  SFolderKeymanEngine               = SFolderKeymanRoot + '\Keyman Engine';
  SFolderKeymanKeyboard             = SFolderKeymanEngine + '\Keyboard';
  SFolderShared                     = SFolderKeymanRoot + '\Shared Data';
  SFolderSharedDatabases            = SFolderShared + '\Databases';
  SFolderKeymanDeveloper            = SFolderKeymanRoot + '\Keyman Developer';
  SFolderKeymanDeveloperTemplates   = SFolderKeymanDeveloper + '\Templates';
  SFolderRegressionTests            = SFolderKeymanDeveloper + '\Regression Tests';

  SFolder_CachedInstallerFiles      = SFolderKeymanRoot + '\Cached Installer Files';

  SFolderKeymanEngineDiag           = SFolderKeymanRoot + '\Diag';

function BuildKeyboardOptionKey_CU(const KeyboardID: string): string;
function BuildKeyboardLanguageProfilesKey_LM(const KeyboardID: string): string;

implementation

function BuildKeyboardOptionKey_CU(const KeyboardID: string): string;
begin
  Result := SRegKey_ActiveKeyboards_CU + '\' + KeyboardID + '\' + SRegSubKey_KeyboardOptions;
end;

function BuildKeyboardLanguageProfilesKey_LM(const KeyboardID: string): string;
begin
  Result := SRegKey_InstalledKeyboards_LM + '\' + KeyboardID + SRegSubKey_LanguageProfiles;
end;

end.

