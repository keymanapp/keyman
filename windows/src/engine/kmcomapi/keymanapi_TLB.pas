unit keymanapi_TLB;

// ************************************************************************ //
// WARNING
// -------
// The types declared in this file were generated from data read from a
// Type Library. If this type library is explicitly or indirectly (via
// another type library referring to this type library) re-imported, or the
// 'Refresh' command of the Type Library Editor activated while editing the
// Type Library, the contents of this file will be regenerated and all
// manual modifications will be lost.
// ************************************************************************ //

// $Rev: 52393 $
// File generated on 6/07/2020 3:15:46 PM from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Projects\keyman\app\windows\src\engine\kmcomapi\kmcomapi (1)
// LIBID: {F16E2A9A-DA46-4EA3-BFF3-BA46B480C961}
// LCID: 0
// Helpfile:
// HelpString: Keyman Engine for Windows API Library
// DepndLst:
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// SYS_KIND: SYS_WIN32
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers.
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}

interface

uses Winapi.Windows, System.Classes, System.Variants, System.Win.StdVCL, Vcl.Graphics, Vcl.OleServer, Winapi.ActiveX;


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:
//   Type Libraries     : LIBID_xxxx
//   CoClasses          : CLASS_xxxx
//   DISPInterfaces     : DIID_xxxx
//   Non-DISP interfaces: IID_xxxx
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  keymanapiMajorVersion = 10;
  keymanapiMinorVersion = 0;

  LIBID_keymanapi: TGUID = '{F16E2A9A-DA46-4EA3-BFF3-BA46B480C961}';

  IID_IKeymanObject: TGUID = '{4DE644EB-F254-4522-AF44-0966CD4AE9F4}';
  IID_IKeyman: TGUID = '{6B363F95-6459-404A-BB5E-F06AFB986BAA}';
  CLASS_Keyman: TGUID = '{CF46549D-4D2D-4679-A2E1-23A815F172F8}';
  IID_IKeymanCollection: TGUID = '{A2FB4733-30DE-4C7C-8ED5-F6CAFA09BB2F}';
  IID_IKeymanErrors: TGUID = '{6A05CFDD-2B7E-4CA5-BAE6-3D5DE193E9CB}';
  IID_IKeymanHotkeys: TGUID = '{5CB5DFC0-05EF-48F0-8249-7BC01AF404FF}';
  IID_IKeymanKeyboardLanguages: TGUID = '{F5CFFF42-4294-44C7-8F97-67EB2E83BB1F}';
  IID_IKeymanKeyboardOptions: TGUID = '{29D0C83A-DBCE-4C54-80F0-B45ABF9FAB49}';
  IID_IKeymanKeyboards: TGUID = '{4EF1F073-61C2-4579-AE34-058F17E56414}';
  IID_IKeymanKeyboardsInstalled: TGUID = '{61406197-399A-41C3-9A1A-158327523FB0}';
  IID_IKeymanPackageContentKeyboards: TGUID = '{3BCF539D-3E94-4271-A7F9-ACAF4D05C0B1}';
  IID_IKeymanLanguages: TGUID = '{53B3076E-86BE-4538-B3AB-5E36DDBD3A6E}';
  IID_IKeymanOptions: TGUID = '{F3EEB9F6-947E-4DD7-A6BD-86DA65B02143}';
  IID_IKeymanPackageContentFiles: TGUID = '{522DC05C-2315-4E99-B66C-468B7264A40B}';
  IID_IKeymanPackageContentFonts: TGUID = '{C27A1117-C075-4B1E-975F-4A44A757B0F1}';
  IID_IKeymanPackagesInstalled: TGUID = '{8A38B1C3-CB4D-4CCF-87B7-0F2A16076D99}';
  IID_IKeymanControl: TGUID = '{4B00DCE2-9DEF-41ED-8254-409DE8746CFF}';
  IID_IKeymanError: TGUID = '{1B9B7E43-189F-4852-A84F-E57D4A8C7243}';
  IID_IKeymanHotkey: TGUID = '{F470447C-2A4D-492C-9A94-91388E648544}';
  IID_IKeymanKeyboard: TGUID = '{8275AA21-9345-451D-8F73-17B6CB10874C}';
  IID_IKeymanKeyboardFile: TGUID = '{80959B80-C7A9-4EB1-AB46-3762F8E5315B}';
  IID_IKeymanKeyboardInstalled: TGUID = '{DF01AA44-06FC-46AC-A6E9-31102C30F5CA}';
  IID_IKeymanKeyboardLanguage: TGUID = '{8D0B3363-E537-437D-AD89-A0B5103F5F64}';
  IID_IKeymanKeyboardOption: TGUID = '{E0E3821D-08D7-4699-B453-EA4FDADE9B9B}';
  IID_IKeymanLanguage: TGUID = '{5C6551E6-FCC8-4D7F-96E4-DAB27251186C}';
  IID_IKeymanOption: TGUID = '{59FB7DCC-BB22-4BDF-B6A8-90BE67D629C8}';
  IID_IKeymanPackage: TGUID = '{FE0DFF5D-56E2-4B8F-988C-8EDCF8F8E6F1}';
  IID_IKeymanPackageFile: TGUID = '{9B67EB6C-5288-4E28-943C-F2981208D64A}';
  IID_IKeymanPackageInstalled: TGUID = '{7062A9DB-CC9C-49CF-AC03-D384C7C1527D}';
  IID_IKeymanPackageContentFile: TGUID = '{FCB845CD-BAD7-4C9F-8A7F-5F6389268A1E}';
  IID_IKeymanPackageContentFont: TGUID = '{79A41E0F-6177-434C-A20B-F1F61B7E87F5}';
  IID_IKeymanSystemInfo: TGUID = '{60AC9C46-167A-41A4-AD27-F2980C82C204}';
  IID_IKeymanUserInterface: TGUID = '{774793A4-259E-47F1-9D71-F83DC6C0A159}';
  IID_IKeymanVisualKeyboard: TGUID = '{CCA8112B-D72E-49FE-AC2C-CB18DC0D3EC7}';
  IID_IKeymanKeyboardLanguageInstalled: TGUID = '{2162FBDF-A165-4460-B99A-BF1FA3C96B76}';
  IID_IKeymanKeyboardLanguagesInstalled: TGUID = '{7DC22BC0-85BB-45C0-8EDB-A2F4BD1D500B}';
  IID_IKeymanKeyboardLanguagesFile: TGUID = '{5F90BCDA-F1C1-433A-8FD0-B498299D3C30}';
  IID_IKeymanKeyboardsInstalled2: TGUID = '{EA57C94F-C140-485E-941A-3F1D5A229024}';
  IID_IKeymanPackagesInstalled2: TGUID = '{F23B9848-2AEF-4A2B-BC3A-292E3A00D691}';
  IID_IKeymanKeyboardFile2: TGUID = '{EDE4326B-51F4-42D5-8251-B20B71993EC8}';
  IID_IKeymanPackageFile2: TGUID = '{9B43B6BC-C622-47EF-915E-6780CF53BAAA}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library
// *********************************************************************//
// Constants for enum tagKeymanKeyboardEncodings
type
  tagKeymanKeyboardEncodings = TOleEnum;
const
  keANSI = $00000001;
  keUnicode = $00000002;

// Constants for enum tagKeymanHotkeyModifiers
type
  tagKeymanHotkeyModifiers = TOleEnum;
const
  HK_ALT = $00010000;
  HK_CTRL = $00020000;
  HK_SHIFT = $00040000;

// Constants for enum tagKeymanErrorSeverity
type
  tagKeymanErrorSeverity = TOleEnum;
const
  kesFatal = $00000000;
  kesError = $00000001;
  kesWarning = $00000002;
  kesHint = $00000003;

// Constants for enum tagKeymanSerializeFlags
type
  tagKeymanSerializeFlags = TOleEnum;
const
  ksfExportImages = $00000001;

// Constants for enum tagKeymanKeyboardLayoutType
type
  tagKeymanKeyboardLayoutType = TOleEnum;
const
  kltPositional = $00000000;
  kltMnemonic = $00000001;

// Constants for enum tagKeymanHotkeyTarget
type
  tagKeymanHotkeyTarget = TOleEnum;
const
  kh__Low = $00000000;
  khKeymanOff = $00000000;
  khKeyboardMenu = $00000001;
  khVisualKeyboard = $00000002;
  khKeymanConfiguration = $00000003;
  khKeyboardUsage = $00000004;
  khFontHelper = $00000005;
  khCharacterMap = $00000006;
  khTextEditor = $00000007;
  khLanguageSwitch = $00000008;
  khKeyboard = $00000009;
  kh__High = $00000009;

// Constants for enum tagKeymanOptionType
type
  tagKeymanOptionType = TOleEnum;
const
  kotUnknown = $00000000;
  kotBool = $00000001;
  kotLong = $00000002;
  kotString = $00000003;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary
// *********************************************************************//
  IKeymanObject = interface;
  IKeymanObjectDisp = dispinterface;
  IKeyman = interface;
  IKeymanDisp = dispinterface;
  IKeymanCollection = interface;
  IKeymanCollectionDisp = dispinterface;
  IKeymanErrors = interface;
  IKeymanErrorsDisp = dispinterface;
  IKeymanHotkeys = interface;
  IKeymanHotkeysDisp = dispinterface;
  IKeymanKeyboardLanguages = interface;
  IKeymanKeyboardLanguagesDisp = dispinterface;
  IKeymanKeyboardOptions = interface;
  IKeymanKeyboardOptionsDisp = dispinterface;
  IKeymanKeyboards = interface;
  IKeymanKeyboardsDisp = dispinterface;
  IKeymanKeyboardsInstalled = interface;
  IKeymanKeyboardsInstalledDisp = dispinterface;
  IKeymanPackageContentKeyboards = interface;
  IKeymanPackageContentKeyboardsDisp = dispinterface;
  IKeymanLanguages = interface;
  IKeymanLanguagesDisp = dispinterface;
  IKeymanOptions = interface;
  IKeymanOptionsDisp = dispinterface;
  IKeymanPackageContentFiles = interface;
  IKeymanPackageContentFilesDisp = dispinterface;
  IKeymanPackageContentFonts = interface;
  IKeymanPackageContentFontsDisp = dispinterface;
  IKeymanPackagesInstalled = interface;
  IKeymanPackagesInstalledDisp = dispinterface;
  IKeymanControl = interface;
  IKeymanControlDisp = dispinterface;
  IKeymanError = interface;
  IKeymanErrorDisp = dispinterface;
  IKeymanHotkey = interface;
  IKeymanHotkeyDisp = dispinterface;
  IKeymanKeyboard = interface;
  IKeymanKeyboardDisp = dispinterface;
  IKeymanKeyboardFile = interface;
  IKeymanKeyboardFileDisp = dispinterface;
  IKeymanKeyboardInstalled = interface;
  IKeymanKeyboardInstalledDisp = dispinterface;
  IKeymanKeyboardLanguage = interface;
  IKeymanKeyboardLanguageDisp = dispinterface;
  IKeymanKeyboardOption = interface;
  IKeymanKeyboardOptionDisp = dispinterface;
  IKeymanLanguage = interface;
  IKeymanLanguageDisp = dispinterface;
  IKeymanOption = interface;
  IKeymanOptionDisp = dispinterface;
  IKeymanPackage = interface;
  IKeymanPackageDisp = dispinterface;
  IKeymanPackageFile = interface;
  IKeymanPackageFileDisp = dispinterface;
  IKeymanPackageInstalled = interface;
  IKeymanPackageInstalledDisp = dispinterface;
  IKeymanPackageContentFile = interface;
  IKeymanPackageContentFileDisp = dispinterface;
  IKeymanPackageContentFont = interface;
  IKeymanPackageContentFontDisp = dispinterface;
  IKeymanSystemInfo = interface;
  IKeymanSystemInfoDisp = dispinterface;
  IKeymanUserInterface = interface;
  IKeymanUserInterfaceDisp = dispinterface;
  IKeymanVisualKeyboard = interface;
  IKeymanVisualKeyboardDisp = dispinterface;
  IKeymanKeyboardLanguageInstalled = interface;
  IKeymanKeyboardLanguageInstalledDisp = dispinterface;
  IKeymanKeyboardLanguagesInstalled = interface;
  IKeymanKeyboardLanguagesInstalledDisp = dispinterface;
  IKeymanKeyboardLanguagesFile = interface;
  IKeymanKeyboardLanguagesFileDisp = dispinterface;
  IKeymanKeyboardsInstalled2 = interface;
  IKeymanKeyboardsInstalled2Disp = dispinterface;
  IKeymanPackagesInstalled2 = interface;
  IKeymanPackagesInstalled2Disp = dispinterface;
  IKeymanKeyboardFile2 = interface;
  IKeymanKeyboardFile2Disp = dispinterface;
  IKeymanPackageFile2 = interface;
  IKeymanPackageFile2Disp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library
// (NOTE: Here we map each CoClass to its Default Interface)
// *********************************************************************//
  Keyman = IKeyman;


// *********************************************************************//
// Declaration of structures, unions and aliases.
// *********************************************************************//

  KeymanKeyboardEncodings = tagKeymanKeyboardEncodings;
  KeymanHotkeyModifiers = tagKeymanHotkeyModifiers;
  KeymanErrorSeverity = tagKeymanErrorSeverity;
  KeymanKeyboardLayoutType = tagKeymanKeyboardLayoutType;
  KeymanHotkeyTarget = tagKeymanHotkeyTarget;
  KeymanOptionType = tagKeymanOptionType;

// *********************************************************************//
// Interface: IKeymanObject
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4DE644EB-F254-4522-AF44-0966CD4AE9F4}
// *********************************************************************//
  IKeymanObject = interface(IDispatch)
    ['{4DE644EB-F254-4522-AF44-0966CD4AE9F4}']
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; safecall;
  end;

// *********************************************************************//
// DispIntf:  IKeymanObjectDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4DE644EB-F254-4522-AF44-0966CD4AE9F4}
// *********************************************************************//
  IKeymanObjectDisp = dispinterface
    ['{4DE644EB-F254-4522-AF44-0966CD4AE9F4}']
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeyman
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6B363F95-6459-404A-BB5E-F06AFB986BAA}
// *********************************************************************//
  IKeyman = interface(IKeymanObject)
    ['{6B363F95-6459-404A-BB5E-F06AFB986BAA}']
    procedure Apply; safecall;
    function Get_AutoApply: WordBool; safecall;
    procedure Set_AutoApply(Value: WordBool); safecall;
    function Get_Control: IKeymanControl; safecall;
    function Get_Errors: IKeymanErrors; safecall;
    function Get_Hotkeys: IKeymanHotkeys; safecall;
    function Get_Keyboards: IKeymanKeyboardsInstalled; safecall;
    function Get_Languages: IKeymanLanguages; safecall;
    function Get_Options: IKeymanOptions; safecall;
    function Get_Packages: IKeymanPackagesInstalled; safecall;
    procedure Refresh; safecall;
    function Get_SystemInfo: IKeymanSystemInfo; safecall;
    property AutoApply: WordBool read Get_AutoApply write Set_AutoApply;
    property Control: IKeymanControl read Get_Control;
    property Errors: IKeymanErrors read Get_Errors;
    property Hotkeys: IKeymanHotkeys read Get_Hotkeys;
    property Keyboards: IKeymanKeyboardsInstalled read Get_Keyboards;
    property Languages: IKeymanLanguages read Get_Languages;
    property Options: IKeymanOptions read Get_Options;
    property Packages: IKeymanPackagesInstalled read Get_Packages;
    property SystemInfo: IKeymanSystemInfo read Get_SystemInfo;
  end;

// *********************************************************************//
// DispIntf:  IKeymanDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6B363F95-6459-404A-BB5E-F06AFB986BAA}
// *********************************************************************//
  IKeymanDisp = dispinterface
    ['{6B363F95-6459-404A-BB5E-F06AFB986BAA}']
    procedure Apply; dispid 1;
    property AutoApply: WordBool dispid 2;
    property Control: IKeymanControl readonly dispid 3;
    property Errors: IKeymanErrors readonly dispid 4;
    property Hotkeys: IKeymanHotkeys readonly dispid 5;
    property Keyboards: IKeymanKeyboardsInstalled readonly dispid 6;
    property Languages: IKeymanLanguages readonly dispid 7;
    property Options: IKeymanOptions readonly dispid 8;
    property Packages: IKeymanPackagesInstalled readonly dispid 9;
    procedure Refresh; dispid 10;
    property SystemInfo: IKeymanSystemInfo readonly dispid 11;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanCollection
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A2FB4733-30DE-4C7C-8ED5-F6CAFA09BB2F}
// *********************************************************************//
  IKeymanCollection = interface(IKeymanObject)
    ['{A2FB4733-30DE-4C7C-8ED5-F6CAFA09BB2F}']
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    procedure Refresh; safecall;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IKeymanCollectionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A2FB4733-30DE-4C7C-8ED5-F6CAFA09BB2F}
// *********************************************************************//
  IKeymanCollectionDisp = dispinterface
    ['{A2FB4733-30DE-4C7C-8ED5-F6CAFA09BB2F}']
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    procedure Refresh; dispid 2;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanErrors
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6A05CFDD-2B7E-4CA5-BAE6-3D5DE193E9CB}
// *********************************************************************//
  IKeymanErrors = interface(IKeymanCollection)
    ['{6A05CFDD-2B7E-4CA5-BAE6-3D5DE193E9CB}']
    function Get_Items(Index: Integer): IKeymanError; safecall;
    procedure Clear; safecall;
    property Items[Index: Integer]: IKeymanError read Get_Items; default;
  end;

// *********************************************************************//
// DispIntf:  IKeymanErrorsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6A05CFDD-2B7E-4CA5-BAE6-3D5DE193E9CB}
// *********************************************************************//
  IKeymanErrorsDisp = dispinterface
    ['{6A05CFDD-2B7E-4CA5-BAE6-3D5DE193E9CB}']
    property Items[Index: Integer]: IKeymanError readonly dispid 0; default;
    procedure Clear; dispid 16;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    procedure Refresh; dispid 2;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanHotkeys
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5CB5DFC0-05EF-48F0-8249-7BC01AF404FF}
// *********************************************************************//
  IKeymanHotkeys = interface(IKeymanCollection)
    ['{5CB5DFC0-05EF-48F0-8249-7BC01AF404FF}']
    function Get_Items(Index: Integer): IKeymanHotkey; safecall;
    procedure Apply; safecall;
    procedure Reset; safecall;
    property Items[Index: Integer]: IKeymanHotkey read Get_Items; default;
  end;

// *********************************************************************//
// DispIntf:  IKeymanHotkeysDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5CB5DFC0-05EF-48F0-8249-7BC01AF404FF}
// *********************************************************************//
  IKeymanHotkeysDisp = dispinterface
    ['{5CB5DFC0-05EF-48F0-8249-7BC01AF404FF}']
    property Items[Index: Integer]: IKeymanHotkey readonly dispid 0; default;
    procedure Apply; dispid 17;
    procedure Reset; dispid 18;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    procedure Refresh; dispid 2;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanKeyboardLanguages
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F5CFFF42-4294-44C7-8F97-67EB2E83BB1F}
// *********************************************************************//
  IKeymanKeyboardLanguages = interface(IKeymanCollection)
    ['{F5CFFF42-4294-44C7-8F97-67EB2E83BB1F}']
  end;

// *********************************************************************//
// DispIntf:  IKeymanKeyboardLanguagesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F5CFFF42-4294-44C7-8F97-67EB2E83BB1F}
// *********************************************************************//
  IKeymanKeyboardLanguagesDisp = dispinterface
    ['{F5CFFF42-4294-44C7-8F97-67EB2E83BB1F}']
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    procedure Refresh; dispid 2;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanKeyboardOptions
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {29D0C83A-DBCE-4C54-80F0-B45ABF9FAB49}
// *********************************************************************//
  IKeymanKeyboardOptions = interface(IKeymanCollection)
    ['{29D0C83A-DBCE-4C54-80F0-B45ABF9FAB49}']
    function Get_Items(Index: OleVariant): IKeymanKeyboardOption; safecall;
    function IndexOf(const ID: WideString): Integer; safecall;
    property Items[Index: OleVariant]: IKeymanKeyboardOption read Get_Items; default;
  end;

// *********************************************************************//
// DispIntf:  IKeymanKeyboardOptionsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {29D0C83A-DBCE-4C54-80F0-B45ABF9FAB49}
// *********************************************************************//
  IKeymanKeyboardOptionsDisp = dispinterface
    ['{29D0C83A-DBCE-4C54-80F0-B45ABF9FAB49}']
    property Items[Index: OleVariant]: IKeymanKeyboardOption readonly dispid 0; default;
    function IndexOf(const ID: WideString): Integer; dispid 16;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    procedure Refresh; dispid 2;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanKeyboards
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4EF1F073-61C2-4579-AE34-058F17E56414}
// *********************************************************************//
  IKeymanKeyboards = interface(IKeymanCollection)
    ['{4EF1F073-61C2-4579-AE34-058F17E56414}']
    function IndexOf(const ID: WideString): Integer; safecall;
  end;

// *********************************************************************//
// DispIntf:  IKeymanKeyboardsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4EF1F073-61C2-4579-AE34-058F17E56414}
// *********************************************************************//
  IKeymanKeyboardsDisp = dispinterface
    ['{4EF1F073-61C2-4579-AE34-058F17E56414}']
    function IndexOf(const ID: WideString): Integer; dispid 5;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    procedure Refresh; dispid 2;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanKeyboardsInstalled
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {61406197-399A-41C3-9A1A-158327523FB0}
// *********************************************************************//
  IKeymanKeyboardsInstalled = interface(IKeymanKeyboards)
    ['{61406197-399A-41C3-9A1A-158327523FB0}']
    function Get_Items(Index: OleVariant): IKeymanKeyboardInstalled; safecall;
    function GetKeyboardFromFile(const Filename: WideString): IKeymanKeyboardFile; safecall;
    procedure Install(const Filename: WideString; Force: WordBool); safecall;
    procedure Apply; safecall;
    property Items[Index: OleVariant]: IKeymanKeyboardInstalled read Get_Items; default;
  end;

// *********************************************************************//
// DispIntf:  IKeymanKeyboardsInstalledDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {61406197-399A-41C3-9A1A-158327523FB0}
// *********************************************************************//
  IKeymanKeyboardsInstalledDisp = dispinterface
    ['{61406197-399A-41C3-9A1A-158327523FB0}']
    property Items[Index: OleVariant]: IKeymanKeyboardInstalled readonly dispid 0; default;
    function GetKeyboardFromFile(const Filename: WideString): IKeymanKeyboardFile; dispid 16;
    procedure Install(const Filename: WideString; Force: WordBool); dispid 17;
    procedure Apply; dispid 18;
    function IndexOf(const ID: WideString): Integer; dispid 5;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    procedure Refresh; dispid 2;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanPackageContentKeyboards
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3BCF539D-3E94-4271-A7F9-ACAF4D05C0B1}
// *********************************************************************//
  IKeymanPackageContentKeyboards = interface(IKeymanKeyboards)
    ['{3BCF539D-3E94-4271-A7F9-ACAF4D05C0B1}']
    function Get_Items(Index: OleVariant): IKeymanKeyboard; safecall;
    property Items[Index: OleVariant]: IKeymanKeyboard read Get_Items; default;
  end;

// *********************************************************************//
// DispIntf:  IKeymanPackageContentKeyboardsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3BCF539D-3E94-4271-A7F9-ACAF4D05C0B1}
// *********************************************************************//
  IKeymanPackageContentKeyboardsDisp = dispinterface
    ['{3BCF539D-3E94-4271-A7F9-ACAF4D05C0B1}']
    property Items[Index: OleVariant]: IKeymanKeyboard readonly dispid 0; default;
    function IndexOf(const ID: WideString): Integer; dispid 5;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    procedure Refresh; dispid 2;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanLanguages
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {53B3076E-86BE-4538-B3AB-5E36DDBD3A6E}
// *********************************************************************//
  IKeymanLanguages = interface(IKeymanCollection)
    ['{53B3076E-86BE-4538-B3AB-5E36DDBD3A6E}']
    function Get_Items(Index: Integer): IKeymanLanguage; safecall;
    procedure Apply; safecall;
    property Items[Index: Integer]: IKeymanLanguage read Get_Items; default;
  end;

// *********************************************************************//
// DispIntf:  IKeymanLanguagesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {53B3076E-86BE-4538-B3AB-5E36DDBD3A6E}
// *********************************************************************//
  IKeymanLanguagesDisp = dispinterface
    ['{53B3076E-86BE-4538-B3AB-5E36DDBD3A6E}']
    property Items[Index: Integer]: IKeymanLanguage readonly dispid 0; default;
    procedure Apply; dispid 5;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    procedure Refresh; dispid 2;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanOptions
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F3EEB9F6-947E-4DD7-A6BD-86DA65B02143}
// *********************************************************************//
  IKeymanOptions = interface(IKeymanCollection)
    ['{F3EEB9F6-947E-4DD7-A6BD-86DA65B02143}']
    function Get_Items(Index: OleVariant): IKeymanOption; safecall;
    procedure Apply; safecall;
    function IndexOf(const ID: WideString): Integer; safecall;
    property Items[Index: OleVariant]: IKeymanOption read Get_Items; default;
  end;

// *********************************************************************//
// DispIntf:  IKeymanOptionsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F3EEB9F6-947E-4DD7-A6BD-86DA65B02143}
// *********************************************************************//
  IKeymanOptionsDisp = dispinterface
    ['{F3EEB9F6-947E-4DD7-A6BD-86DA65B02143}']
    property Items[Index: OleVariant]: IKeymanOption readonly dispid 0; default;
    procedure Apply; dispid 16;
    function IndexOf(const ID: WideString): Integer; dispid 17;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    procedure Refresh; dispid 2;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanPackageContentFiles
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {522DC05C-2315-4E99-B66C-468B7264A40B}
// *********************************************************************//
  IKeymanPackageContentFiles = interface(IKeymanCollection)
    ['{522DC05C-2315-4E99-B66C-468B7264A40B}']
    function Get_Items(Index: OleVariant): IKeymanPackageContentFile; safecall;
    function IndexOf(const Filename: WideString): Integer; safecall;
    property Items[Index: OleVariant]: IKeymanPackageContentFile read Get_Items; default;
  end;

// *********************************************************************//
// DispIntf:  IKeymanPackageContentFilesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {522DC05C-2315-4E99-B66C-468B7264A40B}
// *********************************************************************//
  IKeymanPackageContentFilesDisp = dispinterface
    ['{522DC05C-2315-4E99-B66C-468B7264A40B}']
    property Items[Index: OleVariant]: IKeymanPackageContentFile readonly dispid 0; default;
    function IndexOf(const Filename: WideString): Integer; dispid 16;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    procedure Refresh; dispid 2;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanPackageContentFonts
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C27A1117-C075-4B1E-975F-4A44A757B0F1}
// *********************************************************************//
  IKeymanPackageContentFonts = interface(IKeymanCollection)
    ['{C27A1117-C075-4B1E-975F-4A44A757B0F1}']
    function Get_Items(Index: OleVariant): IKeymanPackageContentFont; safecall;
    function IndexOf(const Filename: WideString): Integer; safecall;
    property Items[Index: OleVariant]: IKeymanPackageContentFont read Get_Items; default;
  end;

// *********************************************************************//
// DispIntf:  IKeymanPackageContentFontsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C27A1117-C075-4B1E-975F-4A44A757B0F1}
// *********************************************************************//
  IKeymanPackageContentFontsDisp = dispinterface
    ['{C27A1117-C075-4B1E-975F-4A44A757B0F1}']
    property Items[Index: OleVariant]: IKeymanPackageContentFont readonly dispid 0; default;
    function IndexOf(const Filename: WideString): Integer; dispid 16;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    procedure Refresh; dispid 2;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanPackagesInstalled
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8A38B1C3-CB4D-4CCF-87B7-0F2A16076D99}
// *********************************************************************//
  IKeymanPackagesInstalled = interface(IKeymanCollection)
    ['{8A38B1C3-CB4D-4CCF-87B7-0F2A16076D99}']
    function Get_Items(Index: OleVariant): IKeymanPackageInstalled; safecall;
    function GetPackageFromFile(const Filename: WideString): IKeymanPackageFile; safecall;
    procedure Install(const Filename: WideString; Force: WordBool); safecall;
    function IndexOf(const ID: WideString): Integer; safecall;
    property Items[Index: OleVariant]: IKeymanPackageInstalled read Get_Items; default;
  end;

// *********************************************************************//
// DispIntf:  IKeymanPackagesInstalledDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8A38B1C3-CB4D-4CCF-87B7-0F2A16076D99}
// *********************************************************************//
  IKeymanPackagesInstalledDisp = dispinterface
    ['{8A38B1C3-CB4D-4CCF-87B7-0F2A16076D99}']
    property Items[Index: OleVariant]: IKeymanPackageInstalled readonly dispid 0; default;
    function GetPackageFromFile(const Filename: WideString): IKeymanPackageFile; dispid 16;
    procedure Install(const Filename: WideString; Force: WordBool); dispid 17;
    function IndexOf(const ID: WideString): Integer; dispid 18;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    procedure Refresh; dispid 2;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanControl
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4B00DCE2-9DEF-41ED-8254-409DE8746CFF}
// *********************************************************************//
  IKeymanControl = interface(IKeymanObject)
    ['{4B00DCE2-9DEF-41ED-8254-409DE8746CFF}']
    function Get_ActiveLanguage: IKeymanLanguage; safecall;
    procedure Set_ActiveLanguage(const Value: IKeymanLanguage); safecall;
    function IsConfigurationOpen: WordBool; safecall;
    function IsKeymanRunning: WordBool; safecall;
    function IsOnlineUpdateCheckOpen: WordBool; safecall;
    function IsTextEditorOpen: WordBool; safecall;
    function IsVisualKeyboardOpen: WordBool; safecall;
    function Get_LastActiveWindow: LongWord; safecall;
    function Get_LastFocusWindow: LongWord; safecall;
    procedure OpenConfiguration; safecall;
    procedure OpenDiagnostics; safecall;
    procedure OpenHelp(const Topic: WideString); safecall;
    procedure OpenTextEditor; safecall;
    procedure OpenUpdateCheck; safecall;
    procedure ShowKeyboardWelcome(const Keyboard: IKeymanKeyboardInstalled); safecall;
    procedure StartKeyman; safecall;
    procedure StartVisualKeyboard; safecall;
    procedure StopKeyman; safecall;
    procedure StopVisualKeyboard; safecall;
    property ActiveLanguage: IKeymanLanguage read Get_ActiveLanguage write Set_ActiveLanguage;
    property LastActiveWindow: LongWord read Get_LastActiveWindow;
    property LastFocusWindow: LongWord read Get_LastFocusWindow;
  end;

// *********************************************************************//
// DispIntf:  IKeymanControlDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4B00DCE2-9DEF-41ED-8254-409DE8746CFF}
// *********************************************************************//
  IKeymanControlDisp = dispinterface
    ['{4B00DCE2-9DEF-41ED-8254-409DE8746CFF}']
    property ActiveLanguage: IKeymanLanguage dispid 1;
    function IsConfigurationOpen: WordBool; dispid 2;
    function IsKeymanRunning: WordBool; dispid 3;
    function IsOnlineUpdateCheckOpen: WordBool; dispid 4;
    function IsTextEditorOpen: WordBool; dispid 5;
    function IsVisualKeyboardOpen: WordBool; dispid 6;
    property LastActiveWindow: LongWord readonly dispid 7;
    property LastFocusWindow: LongWord readonly dispid 8;
    procedure OpenConfiguration; dispid 9;
    procedure OpenDiagnostics; dispid 10;
    procedure OpenHelp(const Topic: WideString); dispid 11;
    procedure OpenTextEditor; dispid 12;
    procedure OpenUpdateCheck; dispid 13;
    procedure ShowKeyboardWelcome(const Keyboard: IKeymanKeyboardInstalled); dispid 14;
    procedure StartKeyman; dispid 15;
    procedure StartVisualKeyboard; dispid 16;
    procedure StopKeyman; dispid 17;
    procedure StopVisualKeyboard; dispid 18;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanError
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {1B9B7E43-189F-4852-A84F-E57D4A8C7243}
// *********************************************************************//
  IKeymanError = interface(IKeymanObject)
    ['{1B9B7E43-189F-4852-A84F-E57D4A8C7243}']
    function Get_ErrorCode: Integer; safecall;
    function Get_Description: WideString; safecall;
    function Get_Severity: Integer; safecall;
    property ErrorCode: Integer read Get_ErrorCode;
    property Description: WideString read Get_Description;
    property Severity: Integer read Get_Severity;
  end;

// *********************************************************************//
// DispIntf:  IKeymanErrorDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {1B9B7E43-189F-4852-A84F-E57D4A8C7243}
// *********************************************************************//
  IKeymanErrorDisp = dispinterface
    ['{1B9B7E43-189F-4852-A84F-E57D4A8C7243}']
    property ErrorCode: Integer readonly dispid 1;
    property Description: WideString readonly dispid 2;
    property Severity: Integer readonly dispid 3;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanHotkey
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F470447C-2A4D-492C-9A94-91388E648544}
// *********************************************************************//
  IKeymanHotkey = interface(IKeymanObject)
    ['{F470447C-2A4D-492C-9A94-91388E648544}']
    procedure Clear; safecall;
    function IsEmpty: WordBool; safecall;
    function Get_Modifiers: KeymanHotkeyModifiers; safecall;
    procedure Set_Modifiers(Value: KeymanHotkeyModifiers); safecall;
    function Get_VirtualKey: Integer; safecall;
    procedure Set_VirtualKey(Value: Integer); safecall;
    function Get_RawValue: Integer; safecall;
    procedure Set_RawValue(Value: Integer); safecall;
    function Get_Target: KeymanHotkeyTarget; safecall;
    procedure Set_Target(Value: KeymanHotkeyTarget); safecall;
    property Modifiers: KeymanHotkeyModifiers read Get_Modifiers write Set_Modifiers;
    property VirtualKey: Integer read Get_VirtualKey write Set_VirtualKey;
    property RawValue: Integer read Get_RawValue write Set_RawValue;
    property Target: KeymanHotkeyTarget read Get_Target write Set_Target;
  end;

// *********************************************************************//
// DispIntf:  IKeymanHotkeyDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F470447C-2A4D-492C-9A94-91388E648544}
// *********************************************************************//
  IKeymanHotkeyDisp = dispinterface
    ['{F470447C-2A4D-492C-9A94-91388E648544}']
    procedure Clear; dispid 1;
    function IsEmpty: WordBool; dispid 2;
    property Modifiers: KeymanHotkeyModifiers dispid 3;
    property VirtualKey: Integer dispid 4;
    property RawValue: Integer dispid 5;
    property Target: KeymanHotkeyTarget dispid 6;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanKeyboard
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8275AA21-9345-451D-8F73-17B6CB10874C}
// *********************************************************************//
  IKeymanKeyboard = interface(IKeymanObject)
    ['{8275AA21-9345-451D-8F73-17B6CB10874C}']
    function Get_Bitmap: IPicture; safecall;
    function Get_Copyright: WideString; safecall;
    function Get_DefaultBCP47Languages: WideString; safecall;
    function Get_DefaultPrimaryLanguage: Integer; safecall;
    function Get_DefaultWindowsLanguages: WideString; safecall;
    function Get_DefaultHotkey: IKeymanHotkey; safecall;
    function Get_Encodings: KeymanKeyboardEncodings; safecall;
    function Get_Filename: WideString; safecall;
    function GetCharsUsed: WideString; safecall;
    function Get_ID: WideString; safecall;
    function Get_LayoutType: KeymanKeyboardLayoutType; safecall;
    function Get_Message: WideString; safecall;
    function Get_Name: WideString; safecall;
    function Get_Version: WideString; safecall;
    property Bitmap: IPicture read Get_Bitmap;
    property Copyright: WideString read Get_Copyright;
    property DefaultBCP47Languages: WideString read Get_DefaultBCP47Languages;
    property DefaultPrimaryLanguage: Integer read Get_DefaultPrimaryLanguage;
    property DefaultWindowsLanguages: WideString read Get_DefaultWindowsLanguages;
    property DefaultHotkey: IKeymanHotkey read Get_DefaultHotkey;
    property Encodings: KeymanKeyboardEncodings read Get_Encodings;
    property Filename: WideString read Get_Filename;
    property ID: WideString read Get_ID;
    property LayoutType: KeymanKeyboardLayoutType read Get_LayoutType;
    property Message: WideString read Get_Message;
    property Name: WideString read Get_Name;
    property Version: WideString read Get_Version;
  end;

// *********************************************************************//
// DispIntf:  IKeymanKeyboardDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8275AA21-9345-451D-8F73-17B6CB10874C}
// *********************************************************************//
  IKeymanKeyboardDisp = dispinterface
    ['{8275AA21-9345-451D-8F73-17B6CB10874C}']
    property Bitmap: IPicture readonly dispid 1;
    property Copyright: WideString readonly dispid 2;
    property DefaultBCP47Languages: WideString readonly dispid 3;
    property DefaultPrimaryLanguage: Integer readonly dispid 4;
    property DefaultWindowsLanguages: WideString readonly dispid 5;
    property DefaultHotkey: IKeymanHotkey readonly dispid 6;
    property Encodings: KeymanKeyboardEncodings readonly dispid 7;
    property Filename: WideString readonly dispid 8;
    function GetCharsUsed: WideString; dispid 9;
    property ID: WideString readonly dispid 10;
    property LayoutType: KeymanKeyboardLayoutType readonly dispid 11;
    property Message: WideString readonly dispid 12;
    property Name: WideString readonly dispid 13;
    property Version: WideString readonly dispid 14;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanKeyboardFile
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {80959B80-C7A9-4EB1-AB46-3762F8E5315B}
// *********************************************************************//
  IKeymanKeyboardFile = interface(IKeymanKeyboard)
    ['{80959B80-C7A9-4EB1-AB46-3762F8E5315B}']
    procedure Install(Force: WordBool); safecall;
    function Get_Languages: IKeymanKeyboardLanguagesFile; safecall;
    property Languages: IKeymanKeyboardLanguagesFile read Get_Languages;
  end;

// *********************************************************************//
// DispIntf:  IKeymanKeyboardFileDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {80959B80-C7A9-4EB1-AB46-3762F8E5315B}
// *********************************************************************//
  IKeymanKeyboardFileDisp = dispinterface
    ['{80959B80-C7A9-4EB1-AB46-3762F8E5315B}']
    procedure Install(Force: WordBool); dispid 256;
    property Languages: IKeymanKeyboardLanguagesFile readonly dispid 402;
    property Bitmap: IPicture readonly dispid 1;
    property Copyright: WideString readonly dispid 2;
    property DefaultBCP47Languages: WideString readonly dispid 3;
    property DefaultPrimaryLanguage: Integer readonly dispid 4;
    property DefaultWindowsLanguages: WideString readonly dispid 5;
    property DefaultHotkey: IKeymanHotkey readonly dispid 6;
    property Encodings: KeymanKeyboardEncodings readonly dispid 7;
    property Filename: WideString readonly dispid 8;
    function GetCharsUsed: WideString; dispid 9;
    property ID: WideString readonly dispid 10;
    property LayoutType: KeymanKeyboardLayoutType readonly dispid 11;
    property Message: WideString readonly dispid 12;
    property Name: WideString readonly dispid 13;
    property Version: WideString readonly dispid 14;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanKeyboardInstalled
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DF01AA44-06FC-46AC-A6E9-31102C30F5CA}
// *********************************************************************//
  IKeymanKeyboardInstalled = interface(IKeymanKeyboard)
    ['{DF01AA44-06FC-46AC-A6E9-31102C30F5CA}']
    function Get_IconFilename: WideString; safecall;
    procedure InstallVisualKeyboard(const Filename: WideString); safecall;
    function Get_KeymanID: Integer; safecall;
    function Get_Languages: IKeymanKeyboardLanguagesInstalled; safecall;
    function Get_Loaded: WordBool; safecall;
    procedure Set_Loaded(Value: WordBool); safecall;
    function Get_Options: IKeymanKeyboardOptions; safecall;
    function Get_OwnerPackage: IKeymanPackageInstalled; safecall;
    function Get_VisualKeyboard: IKeymanVisualKeyboard; safecall;
    procedure Uninstall; safecall;
    property IconFilename: WideString read Get_IconFilename;
    property KeymanID: Integer read Get_KeymanID;
    property Languages: IKeymanKeyboardLanguagesInstalled read Get_Languages;
    property Loaded: WordBool read Get_Loaded write Set_Loaded;
    property Options: IKeymanKeyboardOptions read Get_Options;
    property OwnerPackage: IKeymanPackageInstalled read Get_OwnerPackage;
    property VisualKeyboard: IKeymanVisualKeyboard read Get_VisualKeyboard;
  end;

// *********************************************************************//
// DispIntf:  IKeymanKeyboardInstalledDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DF01AA44-06FC-46AC-A6E9-31102C30F5CA}
// *********************************************************************//
  IKeymanKeyboardInstalledDisp = dispinterface
    ['{DF01AA44-06FC-46AC-A6E9-31102C30F5CA}']
    property IconFilename: WideString readonly dispid 257;
    procedure InstallVisualKeyboard(const Filename: WideString); dispid 258;
    property KeymanID: Integer readonly dispid 259;
    property Languages: IKeymanKeyboardLanguagesInstalled readonly dispid 260;
    property Loaded: WordBool dispid 261;
    property Options: IKeymanKeyboardOptions readonly dispid 262;
    property OwnerPackage: IKeymanPackageInstalled readonly dispid 263;
    property VisualKeyboard: IKeymanVisualKeyboard readonly dispid 264;
    procedure Uninstall; dispid 265;
    property Bitmap: IPicture readonly dispid 1;
    property Copyright: WideString readonly dispid 2;
    property DefaultBCP47Languages: WideString readonly dispid 3;
    property DefaultPrimaryLanguage: Integer readonly dispid 4;
    property DefaultWindowsLanguages: WideString readonly dispid 5;
    property DefaultHotkey: IKeymanHotkey readonly dispid 6;
    property Encodings: KeymanKeyboardEncodings readonly dispid 7;
    property Filename: WideString readonly dispid 8;
    function GetCharsUsed: WideString; dispid 9;
    property ID: WideString readonly dispid 10;
    property LayoutType: KeymanKeyboardLayoutType readonly dispid 11;
    property Message: WideString readonly dispid 12;
    property Name: WideString readonly dispid 13;
    property Version: WideString readonly dispid 14;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanKeyboardLanguage
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8D0B3363-E537-437D-AD89-A0B5103F5F64}
// *********************************************************************//
  IKeymanKeyboardLanguage = interface(IKeymanObject)
    ['{8D0B3363-E537-437D-AD89-A0B5103F5F64}']
    function Get_BCP47Code: WideString; safecall;
    function Get_OwnerKeyboard: IKeymanKeyboard; safecall;
    function Get_LangID: Integer; safecall;
    function Get_Name: WideString; safecall;
    property BCP47Code: WideString read Get_BCP47Code;
    property OwnerKeyboard: IKeymanKeyboard read Get_OwnerKeyboard;
    property LangID: Integer read Get_LangID;
    property Name: WideString read Get_Name;
  end;

// *********************************************************************//
// DispIntf:  IKeymanKeyboardLanguageDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8D0B3363-E537-437D-AD89-A0B5103F5F64}
// *********************************************************************//
  IKeymanKeyboardLanguageDisp = dispinterface
    ['{8D0B3363-E537-437D-AD89-A0B5103F5F64}']
    property BCP47Code: WideString readonly dispid 201;
    property OwnerKeyboard: IKeymanKeyboard readonly dispid 202;
    property LangID: Integer readonly dispid 203;
    property Name: WideString readonly dispid 301;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanKeyboardOption
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E0E3821D-08D7-4699-B453-EA4FDADE9B9B}
// *********************************************************************//
  IKeymanKeyboardOption = interface(IKeymanObject)
    ['{E0E3821D-08D7-4699-B453-EA4FDADE9B9B}']
    function Get_Name: WideString; safecall;
    function Get_Value: WideString; safecall;
    procedure Set_Value(const Value: WideString); safecall;
    property Name: WideString read Get_Name;
    property Value: WideString read Get_Value write Set_Value;
  end;

// *********************************************************************//
// DispIntf:  IKeymanKeyboardOptionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E0E3821D-08D7-4699-B453-EA4FDADE9B9B}
// *********************************************************************//
  IKeymanKeyboardOptionDisp = dispinterface
    ['{E0E3821D-08D7-4699-B453-EA4FDADE9B9B}']
    property Name: WideString readonly dispid 1;
    property Value: WideString dispid 2;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanLanguage
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5C6551E6-FCC8-4D7F-96E4-DAB27251186C}
// *********************************************************************//
  IKeymanLanguage = interface(IKeymanObject)
    ['{5C6551E6-FCC8-4D7F-96E4-DAB27251186C}']
    function Get_BCP47Code: WideString; safecall;
    function Get_HKL: LongWord; safecall;
    function Get_Hotkey: IKeymanHotkey; safecall;
    function Get_KeymanKeyboardLanguage: IKeymanKeyboardLanguageInstalled; safecall;
    function Get_LangID: Integer; safecall;
    function Get_LayoutName: WideString; safecall;
    function Get_LocaleName: WideString; safecall;
    function Get_ProfileGUID: TGUID; safecall;
    function Get_ClassID: TGUID; safecall;
    property BCP47Code: WideString read Get_BCP47Code;
    property HKL: LongWord read Get_HKL;
    property Hotkey: IKeymanHotkey read Get_Hotkey;
    property KeymanKeyboardLanguage: IKeymanKeyboardLanguageInstalled read Get_KeymanKeyboardLanguage;
    property LangID: Integer read Get_LangID;
    property LayoutName: WideString read Get_LayoutName;
    property LocaleName: WideString read Get_LocaleName;
    property ProfileGUID: TGUID read Get_ProfileGUID;
    property ClassID: TGUID read Get_ClassID;
  end;

// *********************************************************************//
// DispIntf:  IKeymanLanguageDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5C6551E6-FCC8-4D7F-96E4-DAB27251186C}
// *********************************************************************//
  IKeymanLanguageDisp = dispinterface
    ['{5C6551E6-FCC8-4D7F-96E4-DAB27251186C}']
    property BCP47Code: WideString readonly dispid 1;
    property HKL: LongWord readonly dispid 2;
    property Hotkey: IKeymanHotkey readonly dispid 3;
    property KeymanKeyboardLanguage: IKeymanKeyboardLanguageInstalled readonly dispid 4;
    property LangID: Integer readonly dispid 5;
    property LayoutName: WideString readonly dispid 6;
    property LocaleName: WideString readonly dispid 7;
    property ProfileGUID: {NOT_OLEAUTO(TGUID)}OleVariant readonly dispid 8;
    property ClassID: {NOT_OLEAUTO(TGUID)}OleVariant readonly dispid 9;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanOption
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {59FB7DCC-BB22-4BDF-B6A8-90BE67D629C8}
// *********************************************************************//
  IKeymanOption = interface(IKeymanObject)
    ['{59FB7DCC-BB22-4BDF-B6A8-90BE67D629C8}']
    function Get_DefaultValue: OleVariant; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_Group: WideString; safecall;
    function Get_ID: WideString; safecall;
    function Get_OptionType: KeymanOptionType; safecall;
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(Value: OleVariant); safecall;
    property DefaultValue: OleVariant read Get_DefaultValue;
    property Enabled: WordBool read Get_Enabled;
    property Group: WideString read Get_Group;
    property ID: WideString read Get_ID;
    property OptionType: KeymanOptionType read Get_OptionType;
    property Value: OleVariant read Get_Value write Set_Value;
  end;

// *********************************************************************//
// DispIntf:  IKeymanOptionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {59FB7DCC-BB22-4BDF-B6A8-90BE67D629C8}
// *********************************************************************//
  IKeymanOptionDisp = dispinterface
    ['{59FB7DCC-BB22-4BDF-B6A8-90BE67D629C8}']
    property DefaultValue: OleVariant readonly dispid 1;
    property Enabled: WordBool readonly dispid 2;
    property Group: WideString readonly dispid 3;
    property ID: WideString readonly dispid 4;
    property OptionType: KeymanOptionType readonly dispid 5;
    property Value: OleVariant dispid 6;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanPackage
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {FE0DFF5D-56E2-4B8F-988C-8EDCF8F8E6F1}
// *********************************************************************//
  IKeymanPackage = interface(IKeymanObject)
    ['{FE0DFF5D-56E2-4B8F-988C-8EDCF8F8E6F1}']
    function Get_Author: WideString; safecall;
    function Get_AuthorEmail: WideString; safecall;
    function Get_Copyright: WideString; safecall;
    function Get_Filename: WideString; safecall;
    function Get_Files: IKeymanPackageContentFiles; safecall;
    function Get_Fonts: IKeymanPackageContentFonts; safecall;
    function Get_Graphic: IPicture; safecall;
    function Get_GraphicFile: IKeymanPackageContentFile; safecall;
    function Get_ID: WideString; safecall;
    function Get_KeyboardOptionsFile: IKeymanPackageContentFile; safecall;
    function Get_Keyboards: IKeymanPackageContentKeyboards; safecall;
    function Get_Name: WideString; safecall;
    function Get_ReadmeFile: IKeymanPackageContentFile; safecall;
    function Get_UsageFile: IKeymanPackageContentFile; safecall;
    function Get_Version: WideString; safecall;
    function Get_WelcomeFile: IKeymanPackageContentFile; safecall;
    function Get_Website: WideString; safecall;
    property Author: WideString read Get_Author;
    property AuthorEmail: WideString read Get_AuthorEmail;
    property Copyright: WideString read Get_Copyright;
    property Filename: WideString read Get_Filename;
    property Files: IKeymanPackageContentFiles read Get_Files;
    property Fonts: IKeymanPackageContentFonts read Get_Fonts;
    property Graphic: IPicture read Get_Graphic;
    property GraphicFile: IKeymanPackageContentFile read Get_GraphicFile;
    property ID: WideString read Get_ID;
    property KeyboardOptionsFile: IKeymanPackageContentFile read Get_KeyboardOptionsFile;
    property Keyboards: IKeymanPackageContentKeyboards read Get_Keyboards;
    property Name: WideString read Get_Name;
    property ReadmeFile: IKeymanPackageContentFile read Get_ReadmeFile;
    property UsageFile: IKeymanPackageContentFile read Get_UsageFile;
    property Version: WideString read Get_Version;
    property WelcomeFile: IKeymanPackageContentFile read Get_WelcomeFile;
    property Website: WideString read Get_Website;
  end;

// *********************************************************************//
// DispIntf:  IKeymanPackageDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {FE0DFF5D-56E2-4B8F-988C-8EDCF8F8E6F1}
// *********************************************************************//
  IKeymanPackageDisp = dispinterface
    ['{FE0DFF5D-56E2-4B8F-988C-8EDCF8F8E6F1}']
    property Author: WideString readonly dispid 1;
    property AuthorEmail: WideString readonly dispid 2;
    property Copyright: WideString readonly dispid 3;
    property Filename: WideString readonly dispid 4;
    property Files: IKeymanPackageContentFiles readonly dispid 5;
    property Fonts: IKeymanPackageContentFonts readonly dispid 6;
    property Graphic: IPicture readonly dispid 7;
    property GraphicFile: IKeymanPackageContentFile readonly dispid 8;
    property ID: WideString readonly dispid 9;
    property KeyboardOptionsFile: IKeymanPackageContentFile readonly dispid 10;
    property Keyboards: IKeymanPackageContentKeyboards readonly dispid 11;
    property Name: WideString readonly dispid 12;
    property ReadmeFile: IKeymanPackageContentFile readonly dispid 13;
    property UsageFile: IKeymanPackageContentFile readonly dispid 14;
    property Version: WideString readonly dispid 15;
    property WelcomeFile: IKeymanPackageContentFile readonly dispid 16;
    property Website: WideString readonly dispid 17;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanPackageFile
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9B67EB6C-5288-4E28-943C-F2981208D64A}
// *********************************************************************//
  IKeymanPackageFile = interface(IKeymanPackage)
    ['{9B67EB6C-5288-4E28-943C-F2981208D64A}']
    procedure Install(Force: WordBool); safecall;
  end;

// *********************************************************************//
// DispIntf:  IKeymanPackageFileDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9B67EB6C-5288-4E28-943C-F2981208D64A}
// *********************************************************************//
  IKeymanPackageFileDisp = dispinterface
    ['{9B67EB6C-5288-4E28-943C-F2981208D64A}']
    procedure Install(Force: WordBool); dispid 256;
    property Author: WideString readonly dispid 1;
    property AuthorEmail: WideString readonly dispid 2;
    property Copyright: WideString readonly dispid 3;
    property Filename: WideString readonly dispid 4;
    property Files: IKeymanPackageContentFiles readonly dispid 5;
    property Fonts: IKeymanPackageContentFonts readonly dispid 6;
    property Graphic: IPicture readonly dispid 7;
    property GraphicFile: IKeymanPackageContentFile readonly dispid 8;
    property ID: WideString readonly dispid 9;
    property KeyboardOptionsFile: IKeymanPackageContentFile readonly dispid 10;
    property Keyboards: IKeymanPackageContentKeyboards readonly dispid 11;
    property Name: WideString readonly dispid 12;
    property ReadmeFile: IKeymanPackageContentFile readonly dispid 13;
    property UsageFile: IKeymanPackageContentFile readonly dispid 14;
    property Version: WideString readonly dispid 15;
    property WelcomeFile: IKeymanPackageContentFile readonly dispid 16;
    property Website: WideString readonly dispid 17;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanPackageInstalled
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7062A9DB-CC9C-49CF-AC03-D384C7C1527D}
// *********************************************************************//
  IKeymanPackageInstalled = interface(IKeymanPackage)
    ['{7062A9DB-CC9C-49CF-AC03-D384C7C1527D}']
    procedure Uninstall(RemoveFonts: WordBool); safecall;
  end;

// *********************************************************************//
// DispIntf:  IKeymanPackageInstalledDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7062A9DB-CC9C-49CF-AC03-D384C7C1527D}
// *********************************************************************//
  IKeymanPackageInstalledDisp = dispinterface
    ['{7062A9DB-CC9C-49CF-AC03-D384C7C1527D}']
    procedure Uninstall(RemoveFonts: WordBool); dispid 256;
    property Author: WideString readonly dispid 1;
    property AuthorEmail: WideString readonly dispid 2;
    property Copyright: WideString readonly dispid 3;
    property Filename: WideString readonly dispid 4;
    property Files: IKeymanPackageContentFiles readonly dispid 5;
    property Fonts: IKeymanPackageContentFonts readonly dispid 6;
    property Graphic: IPicture readonly dispid 7;
    property GraphicFile: IKeymanPackageContentFile readonly dispid 8;
    property ID: WideString readonly dispid 9;
    property KeyboardOptionsFile: IKeymanPackageContentFile readonly dispid 10;
    property Keyboards: IKeymanPackageContentKeyboards readonly dispid 11;
    property Name: WideString readonly dispid 12;
    property ReadmeFile: IKeymanPackageContentFile readonly dispid 13;
    property UsageFile: IKeymanPackageContentFile readonly dispid 14;
    property Version: WideString readonly dispid 15;
    property WelcomeFile: IKeymanPackageContentFile readonly dispid 16;
    property Website: WideString readonly dispid 17;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanPackageContentFile
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {FCB845CD-BAD7-4C9F-8A7F-5F6389268A1E}
// *********************************************************************//
  IKeymanPackageContentFile = interface(IKeymanObject)
    ['{FCB845CD-BAD7-4C9F-8A7F-5F6389268A1E}']
    function Get_Filename: WideString; safecall;
    function Get_FullFilename: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_Stream: IUnknown; safecall;
    property Filename: WideString read Get_Filename;
    property FullFilename: WideString read Get_FullFilename;
    property Description: WideString read Get_Description;
    property Stream: IUnknown read Get_Stream;
  end;

// *********************************************************************//
// DispIntf:  IKeymanPackageContentFileDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {FCB845CD-BAD7-4C9F-8A7F-5F6389268A1E}
// *********************************************************************//
  IKeymanPackageContentFileDisp = dispinterface
    ['{FCB845CD-BAD7-4C9F-8A7F-5F6389268A1E}']
    property Filename: WideString readonly dispid 1;
    property FullFilename: WideString readonly dispid 2;
    property Description: WideString readonly dispid 3;
    property Stream: IUnknown readonly dispid 4;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanPackageContentFont
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {79A41E0F-6177-434C-A20B-F1F61B7E87F5}
// *********************************************************************//
  IKeymanPackageContentFont = interface(IKeymanObject)
    ['{79A41E0F-6177-434C-A20B-F1F61B7E87F5}']
    function Get_Filename: WideString; safecall;
    function Get_Name: WideString; safecall;
    property Filename: WideString read Get_Filename;
    property Name: WideString read Get_Name;
  end;

// *********************************************************************//
// DispIntf:  IKeymanPackageContentFontDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {79A41E0F-6177-434C-A20B-F1F61B7E87F5}
// *********************************************************************//
  IKeymanPackageContentFontDisp = dispinterface
    ['{79A41E0F-6177-434C-A20B-F1F61B7E87F5}']
    property Filename: WideString readonly dispid 1;
    property Name: WideString readonly dispid 2;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanSystemInfo
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {60AC9C46-167A-41A4-AD27-F2980C82C204}
// *********************************************************************//
  IKeymanSystemInfo = interface(IKeymanObject)
    ['{60AC9C46-167A-41A4-AD27-F2980C82C204}']
    function Get_EngineInstallPath: WideString; safecall;
    function Get_EngineVersion: WideString; safecall;
    function Get_IsAdministrator: WordBool; safecall;
    function Get_RebootRequired: WordBool; safecall;
    procedure SetReboot; safecall;
    property EngineInstallPath: WideString read Get_EngineInstallPath;
    property EngineVersion: WideString read Get_EngineVersion;
    property IsAdministrator: WordBool read Get_IsAdministrator;
    property RebootRequired: WordBool read Get_RebootRequired;
  end;

// *********************************************************************//
// DispIntf:  IKeymanSystemInfoDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {60AC9C46-167A-41A4-AD27-F2980C82C204}
// *********************************************************************//
  IKeymanSystemInfoDisp = dispinterface
    ['{60AC9C46-167A-41A4-AD27-F2980C82C204}']
    property EngineInstallPath: WideString readonly dispid 1;
    property EngineVersion: WideString readonly dispid 2;
    property IsAdministrator: WordBool readonly dispid 3;
    property RebootRequired: WordBool readonly dispid 4;
    procedure SetReboot; dispid 5;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanUserInterface
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {774793A4-259E-47F1-9D71-F83DC6C0A159}
// *********************************************************************//
  IKeymanUserInterface = interface(IKeymanObject)
    ['{774793A4-259E-47F1-9D71-F83DC6C0A159}']
  end;

// *********************************************************************//
// DispIntf:  IKeymanUserInterfaceDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {774793A4-259E-47F1-9D71-F83DC6C0A159}
// *********************************************************************//
  IKeymanUserInterfaceDisp = dispinterface
    ['{774793A4-259E-47F1-9D71-F83DC6C0A159}']
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanVisualKeyboard
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CCA8112B-D72E-49FE-AC2C-CB18DC0D3EC7}
// *********************************************************************//
  IKeymanVisualKeyboard = interface(IKeymanObject)
    ['{CCA8112B-D72E-49FE-AC2C-CB18DC0D3EC7}']
    function Get_Filename: WideString; safecall;
    procedure Uninstall; safecall;
    property Filename: WideString read Get_Filename;
  end;

// *********************************************************************//
// DispIntf:  IKeymanVisualKeyboardDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CCA8112B-D72E-49FE-AC2C-CB18DC0D3EC7}
// *********************************************************************//
  IKeymanVisualKeyboardDisp = dispinterface
    ['{CCA8112B-D72E-49FE-AC2C-CB18DC0D3EC7}']
    property Filename: WideString readonly dispid 1;
    procedure Uninstall; dispid 2;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanKeyboardLanguageInstalled
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2162FBDF-A165-4460-B99A-BF1FA3C96B76}
// *********************************************************************//
  IKeymanKeyboardLanguageInstalled = interface(IKeymanKeyboardLanguage)
    ['{2162FBDF-A165-4460-B99A-BF1FA3C96B76}']
    function Get_OwnerKeyboard: IKeymanKeyboardInstalled; safecall;
    function Get_ProfileGUID: TGUID; safecall;
    procedure Uninstall; safecall;
    function Get_IsInstalled: WordBool; safecall;
    procedure Install; safecall;
    property OwnerKeyboard: IKeymanKeyboardInstalled read Get_OwnerKeyboard;
    property ProfileGUID: TGUID read Get_ProfileGUID;
    property IsInstalled: WordBool read Get_IsInstalled;
  end;

// *********************************************************************//
// DispIntf:  IKeymanKeyboardLanguageInstalledDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2162FBDF-A165-4460-B99A-BF1FA3C96B76}
// *********************************************************************//
  IKeymanKeyboardLanguageInstalledDisp = dispinterface
    ['{2162FBDF-A165-4460-B99A-BF1FA3C96B76}']
    property OwnerKeyboard: IKeymanKeyboardInstalled readonly dispid 4;
    property ProfileGUID: {NOT_OLEAUTO(TGUID)}OleVariant readonly dispid 5;
    procedure Uninstall; dispid 6;
    property IsInstalled: WordBool readonly dispid 402;
    procedure Install; dispid 403;
    property BCP47Code: WideString readonly dispid 201;
    property OwnerKeyboard: IKeymanKeyboard readonly dispid 202;
    property LangID: Integer readonly dispid 203;
    property Name: WideString readonly dispid 301;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanKeyboardLanguagesInstalled
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7DC22BC0-85BB-45C0-8EDB-A2F4BD1D500B}
// *********************************************************************//
  IKeymanKeyboardLanguagesInstalled = interface(IKeymanKeyboardLanguages)
    ['{7DC22BC0-85BB-45C0-8EDB-A2F4BD1D500B}']
    procedure Install(const BCP47Code: WideString); safecall;
    procedure InstallByLangID(LangID: Integer); safecall;
    function Get_Items(Index: SYSINT): IKeymanKeyboardLanguageInstalled; safecall;
    property Items[Index: SYSINT]: IKeymanKeyboardLanguageInstalled read Get_Items; default;
  end;

// *********************************************************************//
// DispIntf:  IKeymanKeyboardLanguagesInstalledDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7DC22BC0-85BB-45C0-8EDB-A2F4BD1D500B}
// *********************************************************************//
  IKeymanKeyboardLanguagesInstalledDisp = dispinterface
    ['{7DC22BC0-85BB-45C0-8EDB-A2F4BD1D500B}']
    procedure Install(const BCP47Code: WideString); dispid 16;
    procedure InstallByLangID(LangID: Integer); dispid 17;
    property Items[Index: SYSINT]: IKeymanKeyboardLanguageInstalled readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    procedure Refresh; dispid 2;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanKeyboardLanguagesFile
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5F90BCDA-F1C1-433A-8FD0-B498299D3C30}
// *********************************************************************//
  IKeymanKeyboardLanguagesFile = interface(IKeymanKeyboardLanguages)
    ['{5F90BCDA-F1C1-433A-8FD0-B498299D3C30}']
    function Get_Items(Index: SYSINT): IKeymanKeyboardLanguage; safecall;
    property Items[Index: SYSINT]: IKeymanKeyboardLanguage read Get_Items; default;
  end;

// *********************************************************************//
// DispIntf:  IKeymanKeyboardLanguagesFileDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5F90BCDA-F1C1-433A-8FD0-B498299D3C30}
// *********************************************************************//
  IKeymanKeyboardLanguagesFileDisp = dispinterface
    ['{5F90BCDA-F1C1-433A-8FD0-B498299D3C30}']
    property Items[Index: SYSINT]: IKeymanKeyboardLanguage readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    procedure Refresh; dispid 2;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanKeyboardsInstalled2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EA57C94F-C140-485E-941A-3F1D5A229024}
// *********************************************************************//
  IKeymanKeyboardsInstalled2 = interface(IKeymanKeyboardsInstalled)
    ['{EA57C94F-C140-485E-941A-3F1D5A229024}']
    function Install2(const Filename: WideString; Force: WordBool; InstallDefaultLanguage: WordBool): IKeymanKeyboardInstalled; safecall;
  end;

// *********************************************************************//
// DispIntf:  IKeymanKeyboardsInstalled2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EA57C94F-C140-485E-941A-3F1D5A229024}
// *********************************************************************//
  IKeymanKeyboardsInstalled2Disp = dispinterface
    ['{EA57C94F-C140-485E-941A-3F1D5A229024}']
    function Install2(const Filename: WideString; Force: WordBool; InstallDefaultLanguage: WordBool): IKeymanKeyboardInstalled; dispid 19;
    property Items[Index: OleVariant]: IKeymanKeyboardInstalled readonly dispid 0; default;
    function GetKeyboardFromFile(const Filename: WideString): IKeymanKeyboardFile; dispid 16;
    procedure Install(const Filename: WideString; Force: WordBool); dispid 17;
    procedure Apply; dispid 18;
    function IndexOf(const ID: WideString): Integer; dispid 5;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    procedure Refresh; dispid 2;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanPackagesInstalled2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F23B9848-2AEF-4A2B-BC3A-292E3A00D691}
// *********************************************************************//
  IKeymanPackagesInstalled2 = interface(IKeymanPackagesInstalled)
    ['{F23B9848-2AEF-4A2B-BC3A-292E3A00D691}']
    function Install2(const Filename: WideString; Force: WordBool; InstallDefaultLanguage: WordBool): IKeymanPackageInstalled; safecall;
  end;

// *********************************************************************//
// DispIntf:  IKeymanPackagesInstalled2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F23B9848-2AEF-4A2B-BC3A-292E3A00D691}
// *********************************************************************//
  IKeymanPackagesInstalled2Disp = dispinterface
    ['{F23B9848-2AEF-4A2B-BC3A-292E3A00D691}']
    function Install2(const Filename: WideString; Force: WordBool; InstallDefaultLanguage: WordBool): IKeymanPackageInstalled; dispid 19;
    property Items[Index: OleVariant]: IKeymanPackageInstalled readonly dispid 0; default;
    function GetPackageFromFile(const Filename: WideString): IKeymanPackageFile; dispid 16;
    procedure Install(const Filename: WideString; Force: WordBool); dispid 17;
    function IndexOf(const ID: WideString): Integer; dispid 18;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    procedure Refresh; dispid 2;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanKeyboardFile2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EDE4326B-51F4-42D5-8251-B20B71993EC8}
// *********************************************************************//
  IKeymanKeyboardFile2 = interface(IKeymanKeyboardFile)
    ['{EDE4326B-51F4-42D5-8251-B20B71993EC8}']
    function Install2(Force: WordBool; InstallDefaultLanguage: WordBool): IKeymanKeyboardInstalled; safecall;
  end;

// *********************************************************************//
// DispIntf:  IKeymanKeyboardFile2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EDE4326B-51F4-42D5-8251-B20B71993EC8}
// *********************************************************************//
  IKeymanKeyboardFile2Disp = dispinterface
    ['{EDE4326B-51F4-42D5-8251-B20B71993EC8}']
    function Install2(Force: WordBool; InstallDefaultLanguage: WordBool): IKeymanKeyboardInstalled; dispid 403;
    procedure Install(Force: WordBool); dispid 256;
    property Languages: IKeymanKeyboardLanguagesFile readonly dispid 402;
    property Bitmap: IPicture readonly dispid 1;
    property Copyright: WideString readonly dispid 2;
    property DefaultBCP47Languages: WideString readonly dispid 3;
    property DefaultPrimaryLanguage: Integer readonly dispid 4;
    property DefaultWindowsLanguages: WideString readonly dispid 5;
    property DefaultHotkey: IKeymanHotkey readonly dispid 6;
    property Encodings: KeymanKeyboardEncodings readonly dispid 7;
    property Filename: WideString readonly dispid 8;
    function GetCharsUsed: WideString; dispid 9;
    property ID: WideString readonly dispid 10;
    property LayoutType: KeymanKeyboardLayoutType readonly dispid 11;
    property Message: WideString readonly dispid 12;
    property Name: WideString readonly dispid 13;
    property Version: WideString readonly dispid 14;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// Interface: IKeymanPackageFile2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9B43B6BC-C622-47EF-915E-6780CF53BAAA}
// *********************************************************************//
  IKeymanPackageFile2 = interface(IKeymanPackageFile)
    ['{9B43B6BC-C622-47EF-915E-6780CF53BAAA}']
    function Install2(Force: WordBool; InstallDefaultLanguage: WordBool): IKeymanPackageInstalled; safecall;
  end;

// *********************************************************************//
// DispIntf:  IKeymanPackageFile2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9B43B6BC-C622-47EF-915E-6780CF53BAAA}
// *********************************************************************//
  IKeymanPackageFile2Disp = dispinterface
    ['{9B43B6BC-C622-47EF-915E-6780CF53BAAA}']
    function Install2(Force: WordBool; InstallDefaultLanguage: WordBool): IKeymanPackageInstalled; dispid 257;
    procedure Install(Force: WordBool); dispid 256;
    property Author: WideString readonly dispid 1;
    property AuthorEmail: WideString readonly dispid 2;
    property Copyright: WideString readonly dispid 3;
    property Filename: WideString readonly dispid 4;
    property Files: IKeymanPackageContentFiles readonly dispid 5;
    property Fonts: IKeymanPackageContentFonts readonly dispid 6;
    property Graphic: IPicture readonly dispid 7;
    property GraphicFile: IKeymanPackageContentFile readonly dispid 8;
    property ID: WideString readonly dispid 9;
    property KeyboardOptionsFile: IKeymanPackageContentFile readonly dispid 10;
    property Keyboards: IKeymanPackageContentKeyboards readonly dispid 11;
    property Name: WideString readonly dispid 12;
    property ReadmeFile: IKeymanPackageContentFile readonly dispid 13;
    property UsageFile: IKeymanPackageContentFile readonly dispid 14;
    property Version: WideString readonly dispid 15;
    property WelcomeFile: IKeymanPackageContentFile readonly dispid 16;
    property Website: WideString readonly dispid 17;
    function SerializeXML(Flags: tagKeymanSerializeFlags; const ImagePath: WideString;
                          out References: OleVariant): WideString; dispid 401;
  end;

// *********************************************************************//
// The Class CoKeyman provides a Create and CreateRemote method to
// create instances of the default interface IKeyman exposed by
// the CoClass Keyman. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoKeyman = class
    class function Create: IKeyman;
    class function CreateRemote(const MachineName: string): IKeyman;
  end;

implementation

uses System.Win.ComObj;

class function CoKeyman.Create: IKeyman;
begin
  Result := CreateComObject(CLASS_Keyman) as IKeyman;
end;

class function CoKeyman.CreateRemote(const MachineName: string): IKeyman;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Keyman) as IKeyman;
end;

end.

