(*
  Name:             PackageInfo
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      1 Aug 2006

  Modified Date:    25 Oct 2016
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          01 Aug 2006 - mcdurdin - Rework to load/save to/from XML
                    02 Aug 2006 - mcdurdin - Don't load from XML if file is 0 bytes or does not exist
                    23 Aug 2006 - mcdurdin - Add SaveXMLToText and LoadXMLFromText functions
                    06 Oct 2006 - mcdurdin - Add WasIni flag
                    04 Dec 2006 - mcdurdin - Add icon support
                    12 Dec 2006 - mcdurdin - Add StartMenuEntryLocation support
                    04 Jan 2007 - mcdurdin - Add pfclLocaleFolder copy location
                    19 Mar 2007 - mcdurdin - I689 - Assign location when copying shortcut information
                    16 May 2007 - mcdurdin - I233 - Convert all controls to Unicode
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    30 Jan 2009 - mcdurdin - I1826 - Improve config performance
                    22 Oct 2010 - mcdurdin - I2002 - Registry settings in products
                    18 Mar 2011 - mcdurdin - I2574 - Adding KMX to KPS will add KVK again, incorrectly
                    04 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls
                    04 May 2012 - mcdurdin - I3311 - V9.0 - Change 'published' to 'public' on classes that don't need RTTI
                    26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    03 Feb 2015 - mcdurdin - I4574 - V9.0 - If any files are read-only, they need the read-only flag removed on install
                    25 Oct 2016 - mcdurdin - I5126 - Version 10.0 is showing as < version 7.0 in version checks
*)
unit PackageInfo;  // I3306

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.IniFiles,
  System.JSON,
  System.StrUtils,
  System.Sysutils,
  Winapi.Windows,
  Xml.XMLDoc,
  Xml.XMLIntf,

  utilfiletypes,
  utilstr;

{ Package Information Classes }

function GetJsonValueString(o: TJSONObject; const n: string): string;
function GetJsonValueBool(o: TJSONObject; const n: string): Boolean;

type
  TPackageInfoEntryType = (
    pietName,
    pietVersion,
    pietCopyright,
    pietAuthor,
    pietWebsite,
    pietDescription,
    pietOther
  );

const
  PackageInfo_Name      = 'Name';
  PackageInfo_Version   = 'Version';
  PackageInfo_Copyright = 'Copyright';
  PackageInfo_Author    = 'Author';
  PackageInfo_Website   = 'Website';
  PackageInfo_Description = 'Description';

  PackageInfoEntryTypeNames: array[TPackageInfoEntryType] of string = (
    PackageInfo_Name,
    PackageInfo_Version,
    PackageInfo_Copyright,
    PackageInfo_Author,
    PackageInfo_Website,
    PackageInfo_Description,
    ''
  );

type
  EPackageInfo = class(Exception);

  { Package Base Object -- notifications }

  TPackageNotifyEventType = (netDeleteFromList, netDestroy);
  TPackageNotifyEvent = procedure(Sender: TObject; EventType: TPackageNotifyEventType; var FAllow: Boolean) of object;

  TPackageNotifyEventWrapper = class
    FEvent: TPackageNotifyEvent;
  end;

  TPackageFileCopyLocation = (pfclPackage,      // Copy into the package directory, default
                              pfclKeymanDir,    // Copy into the Keyman directory
                              pfclInstallTemp,  // Used only for installation, temporary
                              pfclLocaleFolder); // For locale files


  TPackage = class;

  { Base Classes }

  TPackageBaseObject = class
  strict private
    FPackage: TPackage;
  public
    constructor Create(APackage: TPackage); virtual;
    property Package: TPackage read FPackage;
  end;

  TPackageBaseNotifyObject = class(TPackageBaseObject)
  private
    FNotifyObjects: TObjectList<TPackageNotifyEventWrapper>;
    FTag: Integer;
    function Notify(EventType: TPackageNotifyEventType): Boolean;
  public
    constructor Create(APackage: TPackage); override;
    destructor Destroy; override;
    procedure AddNotifyObject(FEventHandler: TPackageNotifyEvent);
    procedure RemoveNotifyObject(FEventHandler: TPackageNotifyEvent);
    property Tag: Integer read FTag write FTag;
  end;

  TPackageObjectList<T: TPackageBaseObject> = class(TObjectList<T>)
  strict private
    FPackage: TPackage;
  protected
    property Package: TPackage read FPackage;
  public
    constructor Create(APackage: TPackage);
  end;


  TPackageContentFile = class;
  TPackageContentFileList = class;
  TPackageContentFileReferenceList = class;

  { Package Options }

  TPackageOptions = class(TPackageBaseObject)
  private
    FFileVersion: WideString;
    FExecuteProgram: WideString;
    FReadmeFile: TPackageContentFile;
    FGraphicFile: TPackageContentFile;
    FLicenseFile: TPackageContentFile;
    FWelcomeFile: TPackageContentFile;
    FLoadLegacy: Boolean;
    procedure SetReadmeFile(const Value: TPackageContentFile);
    procedure SetExecuteProgram(Value: WideString);
    procedure SetFileVersion(Value: WideString);
    procedure SetGraphicFile(const Value: TPackageContentFile);
    procedure GraphicRemoved(Sender: TObject;
      EventType: TPackageNotifyEventType; var FAllow: Boolean);
    procedure ReadmeRemoved(Sender: TObject;
      EventType: TPackageNotifyEventType; var FAllow: Boolean);
    procedure SetLicenseFile(const Value: TPackageContentFile);
    procedure LicenseRemoved(Sender: TObject;
      EventType: TPackageNotifyEventType; var FAllow: Boolean);
    procedure SetWelcomeFile(const Value: TPackageContentFile);
    procedure WelcomeRemoved(Sender: TObject;
      EventType: TPackageNotifyEventType; var FAllow: Boolean);
  public
    constructor Create(APackage: TPackage); override;
    destructor Destroy; override;
    procedure Assign(Source: TPackageOptions); virtual;
    procedure LoadIni(AIni: TIniFile); virtual;
    procedure SaveIni(AIni: TIniFile); virtual;
    procedure LoadXML(ARoot: IXMLNode); virtual;
    procedure SaveXML(ARoot: IXMLNode); virtual;
    procedure LoadJSON(ARoot: TJSONObject); virtual;
    procedure SaveJSON(ARoot: TJSONObject); virtual;
    property LoadLegacy: Boolean read FLoadLegacy write FLoadLegacy;
    property FileVersion: WideString read FFileVersion write SetFileVersion;
    property ExecuteProgram: WideString read FExecuteProgram write SetExecuteProgram;
    property ReadmeFile: TPackageContentFile read FReadmeFile write SetReadmeFile;
    property GraphicFile: TPackageContentFile read FGraphicFile write SetGraphicFile;
    property LicenseFile: TPackageContentFile read FLicenseFile write SetLicenseFile;
    property WelcomeFile: TPackageContentFile read FWelcomeFile write SetWelcomeFile;
  end;

  { Package Information }

  TPackageInfoEntry = class(TPackageBaseObject)
  private
    FURL: WideString;
    FDescription: WideString;
    FName: WideString;
    FInfoType: TPackageInfoEntryType;
    procedure SetDescription(Value: WideString);
    procedure SetName(Value: WideString);
    procedure SetURL(Value: WideString);
  public
    procedure Assign(Source: TPackageInfoEntry); virtual;
    property InfoType: TPackageInfoEntryType read FInfoType;
    property Name: WideString read FName write SetName;
    property Description: WideString read FDescription write SetDescription;
    property URL: WideString read FURL write SetURL;
  end;

  TPackageInfoEntryList = class(TPackageObjectList<TPackageInfoEntry>)
  protected
    procedure SetDesc(Name, Desc: WideString);
    procedure SetURL(Name, URL: WideString);
    function DescIndexOf(Name: WideString): WideString;
    function UrlIndexOf(Name: WideString): WideString;
  public
    procedure Assign(Source: TPackageInfoEntryList); virtual;
    procedure LoadIni(AIni: TIniFile); virtual;
    procedure SaveIni(AIni: TIniFile); virtual;
    procedure LoadXML(ARoot: IXMLNode); virtual;
    procedure SaveXML(ARoot: IXMLNode); virtual;
    procedure LoadJSON(ARoot: TJSONObject); virtual;
    procedure SaveJSON(ARoot: TJSONObject); virtual;
    function IndexOf(Name: WideString): Integer; overload;


    property Desc[Name: WideString]: WideString read DescIndexOf write SetDesc;
    property URL[Name: WideString]: WideString read URLIndexOf write SetURL;
  end;

  { Package Start Menu Classes }

  TPackageStartMenuEntryLocation = (psmelStartMenu, psmelDesktop); //, psmelQuickLaunch);

  TPackageStartMenuEntry = class(TPackageBaseObject)
  public
    Name: WideString;
    Prog: WideString;
    Params: WideString;
    Icon: WideString;
    Location: TPackageStartMenuEntryLocation;
    procedure Assign(Source: TPackageStartMenuEntry); virtual;
  end;

  TPackageStartMenuEntryList = class(TPackageObjectList<TPackageStartMenuEntry>)
  public
    procedure Assign(Source: TPackageStartMenuEntryList); virtual;
    procedure LoadIni(AIni: TIniFile); virtual;
    procedure SaveIni(AIni: TIniFile); virtual;
    procedure LoadXML(ARoot: IXMLNode); virtual;
    procedure SaveXML(ARoot: IXMLNode); virtual;
    procedure LoadJSON(ARoot: TJSONObject); virtual;
    procedure SaveJSON(ARoot: TJSONObject); virtual;
  end;

  TPackageStartMenu = class(TPackageBaseObject)
  public
    Path: WideString;
    DoCreate: Boolean;
    AddUninstallEntry: Boolean;
    Entries: TPackageStartMenuEntryList;
    constructor Create(APackage: TPackage); override;
    destructor Destroy; override;
    procedure Assign(Source: TPackageStartMenu); virtual;
    procedure LoadIni(AIni: TIniFile); virtual;
    procedure SaveIni(AIni: TIniFile); virtual;
    procedure LoadXML(ARoot: IXMLNode); virtual;
    procedure SaveXML(ARoot: IXMLNode); virtual;
    procedure LoadJSON(ARoot: TJSONObject); virtual;
    procedure SaveJSON(ARoot: TJSONObject); virtual;
  end;

  { Package contained files }

  TPackageContentFile = class(TPackageBaseNotifyObject)
  private
    FCopyLocation: TPackageFileCopyLocation;
    FDescription: WideString;
    FFileName: WideString;
    FFileType: TKMFileType;
    procedure SetDescription(Value: WideString);
    procedure SetFileName(Value: WideString);
    procedure SetFileType(const Value: TKMFileType);
    procedure SetCopyLocation(const Value: TPackageFileCopyLocation);
  public
    procedure Assign(Source: TPackageContentFile); virtual;
    function RelativeFileName: WideString;
    property FileName: WideString read FFileName write SetFileName;  // relative to .kps, or no path if inside .kmp or .exe
    property FileType: TKMFileType read FFileType write SetFileType;
    property Description: WideString read FDescription write SetDescription;
    property CopyLocation: TPackageFileCopyLocation read FCopyLocation write SetCopyLocation;
  end;

  TPackageContentFileList = class(TPackageObjectList<TPackageContentFile>)
  public
    procedure Assign(Source: TPackageContentFileList); virtual;
    procedure LoadIni(AIni: TIniFile); virtual;
    procedure SaveIni(AIni: TIniFile); virtual;
    procedure LoadXML(ARoot: IXMLNode); virtual;
    procedure SaveXML(ARoot: IXMLNode); virtual;
    procedure LoadJSON(ARoot: TJSONObject); virtual;
    procedure SaveJSON(ARoot: TJSONObject); virtual;

    function IndexOfFileType(FFileType: TKMFileType): Integer;
    function FromFileName(Filename: WideString): TPackageContentFile;
    function FromFileNameEx(Filename: WideString): TPackageContentFile;
    procedure Delete(Index: Integer);
  end;

  { TPackageKeyboard }

  TPackageKeyboard = class;
  TPackageKeyboardList = class;
  TPackageKeyboardLanguage = class;
  TPackageKeyboardLanguageList = class;
  TPackageKeyboardExample = class;
  TPackageKeyboardExampleList = class;

  TPackageKeyboard = class(TPackageBaseObject)
  private
    FName: string;
    FOSKFont: TPackageContentFile;
    FID: string;
    FRTL: Boolean;
    FDisplayFont: TPackageContentFile;
    FLanguages: TPackageKeyboardLanguageList;
    FExamples: TPackageKeyboardExampleList;
    FVersion: string;
    FMinKeymanVersion: string;
    FWebOSKFonts: TPackageContentFileReferenceList;
    FWebDisplayFonts: TPackageContentFileReferenceList;
    procedure SetDisplayFont(const Value: TPackageContentFile);
    procedure SetOSKFont(const Value: TPackageContentFile);
    procedure FontRemoved(Sender: TObject;
      EventType: TPackageNotifyEventType; var FAllow: Boolean);
    procedure FontListNotify(Sender: TObject; const Item: TPackageContentFile;
      Action: TCollectionNotification);
  public
    constructor Create(APackage: TPackage); override;
    destructor Destroy; override;
    procedure Assign(Source: TPackageKeyboard); virtual;
    property Name: string read FName write FName;
    property ID: string read FID write FID;
    property RTL: Boolean read FRTL write FRTL;
    property Version: string read FVersion write FVersion;
    property Languages: TPackageKeyboardLanguageList read FLanguages;
    property Examples: TPackageKeyboardExampleList read FExamples;
    property OSKFont: TPackageContentFile read FOSKFont write SetOSKFont;
    property DisplayFont: TPackageContentFile read FDisplayFont write SetDisplayFont;
    property WebOSKFonts: TPackageContentFileReferenceList read FWebOSKFonts;
    property WebDisplayFonts: TPackageContentFileReferenceList read FWebDisplayFonts;
    // The following properties are used only in memory and never streamed in or out
    property MinKeymanVersion: string read FMinKeymanVersion write FMinKeymanVersion;
  end;

  TPackageContentFileReferenceList = class(TPackageObjectList<TPackageContentFile>)
  public
    constructor Create(APackage: TPackage);
    procedure Assign(Source: TPackageContentFileReferenceList); virtual;
    procedure LoadXML(ARoot: IXMLNode); virtual;
    procedure SaveXML(ARoot: IXMLNode); virtual;
    procedure LoadJSON(ARoot: TJSONArray); virtual;
    procedure SaveJSON(ARoot: TJSONArray); virtual;
    function GetAsString: string;
  end;

  TPackageKeyboardList = class(TPackageObjectList<TPackageKeyboard>)
  public
    procedure Assign(Source: TPackageKeyboardList); virtual;
    procedure LoadIni(AIni: TIniFile); virtual;
    procedure SaveIni(AIni: TIniFile); virtual;
    procedure LoadXML(ARoot: IXMLNode); virtual;
    procedure SaveXML(ARoot: IXMLNode); virtual;
    procedure LoadJSON(ARoot: TJSONObject); virtual;
    procedure SaveJSON(ARoot: TJSONObject); virtual;
    function ItemByID(id: string): TPackageKeyboard;
  end;

  TPackageKeyboardLanguage = class(TPackageBaseObject)
    ID: string;
    Name: string;
  end;

  TPackageKeyboardLanguageList = class(TPackageObjectList<TPackageKeyboardLanguage>)
  public
    procedure LoadJSON(ARoot: TJSONObject); virtual;
    procedure SaveJSON(ARoot: TJSONObject); virtual;
    procedure LoadXML(ARoot: IXMLNode); virtual;
    procedure SaveXML(ARoot: IXMLNode); virtual;
    function ContainsID(const id: string): Boolean;
    function IndexOfID(const id: string): Integer;
  end;

  TPackageKeyboardExample = class(TPackageBaseObject)
    ID: string;
    Keys: string;
    Text: string;
    Note: string;
  end;

  TPackageKeyboardExampleList = class(TPackageObjectList<TPackageKeyboardExample>)
  public
    procedure LoadJSON(ARoot: TJSONObject); virtual;
    procedure SaveJSON(ARoot: TJSONObject); virtual;
    procedure LoadXML(ARoot: IXMLNode); virtual;
    procedure SaveXML(ARoot: IXMLNode); virtual;
    function ContainsID(const id: string): Boolean;
    function IndexOfID(const id: string; from: Integer = 0): Integer;
  end;

  TPackageLexicalModel = class(TPackageBaseObject)
  private
    FName: string;
    FID: string;
    FLanguages: TPackageKeyboardLanguageList;
    FRTL: Boolean;
  public
    constructor Create(APackage: TPackage); override;
    destructor Destroy; override;
    procedure Assign(Source: TPackageLexicalModel); virtual;
    property Name: string read FName write FName;
    property ID: string read FID write FID;
    property RTL: Boolean read FRTL write FRTL;
    property Languages: TPackageKeyboardLanguageList read FLanguages;
  end;

  TPackageLexicalModelList = class(TPackageObjectList<TPackageLexicalModel>)
    procedure Assign(Source: TPackageLexicalModelList); virtual;
    procedure LoadXML(ARoot: IXMLNode); virtual;
    procedure SaveXML(ARoot: IXMLNode); virtual;
    procedure LoadJSON(ARoot: TJSONObject); virtual;
    procedure SaveJSON(ARoot: TJSONObject); virtual;
    function ItemByID(id: string): TPackageLexicalModel;
  end;

  TPackageRelatedPackage = class(TPackageBaseObject)
  private
    FID: string;
    FRelationship: string;
  public
    procedure Assign(Source: TPackageRelatedPackage); virtual;
    property ID: string read FID write FID;
    property Relationship: string read FRelationship write FRelationship;
  end;

  TPackageRelatedPackageList = class(TPackageObjectList<TPackageRelatedPackage>)
    procedure Assign(Source: TPackageRelatedPackageList); virtual;
    procedure LoadXML(ARoot: IXMLNode); virtual;
    procedure SaveXML(ARoot: IXMLNode); virtual;
    procedure LoadJSON(ARoot: TJSONObject); virtual;
    procedure SaveJSON(ARoot: TJSONObject); virtual;
  end;

  { TPackage }

  TPackage = class
  private
    FFileName: WideString;
    FWasIni: Boolean;
    FLoadLegacy: Boolean;
    procedure FixupFileVersion;
  protected
    procedure Import(AIni: TIniFile); virtual; abstract;
    function XMLRootNode: WideString; virtual;
    procedure DoLoadXML(ARoot: IXMLNode); virtual;
    procedure DoSaveXML(ARoot: IXMLNode); virtual;
    procedure DoLoadJSON(ARoot: TJSONObject); virtual;
    procedure DoSaveJSON(ARoot: TJSONObject); virtual;
    procedure DoLoadIni(ini: TIniFile); virtual;
    procedure DoSaveIni(ini: TIniFile); virtual;
  public
    Options: TPackageOptions;
    StartMenu: TPackageStartMenu;
    Files: TPackageContentFileList;
    Info: TPackageInfoEntryList;
    Keyboards: TPackageKeyboardList;
    LexicalModels: TPackageLexicalModelList;
    RelatedPackages: TPackageRelatedPackageList;

    property FileName: WideString read FFileName write FFileName;
    procedure Assign(Source: TPackage); virtual;
    constructor Create;
    destructor Destroy; override;

    procedure LoadIni;
    procedure SaveIni;

    procedure LoadXML;
    procedure SaveXML;

    procedure LoadJSON;
    procedure LoadJSONFromStream(Stream: TStream);
    procedure SaveJSON;

    function SaveXMLToText: WideString;
    procedure LoadXMLFromText(Text: WideString);

    property LoadLegacy: Boolean read FLoadLegacy write FLoadLegacy;

    property WasIni: Boolean read FWasIni;
  end;

const
  PackageStartMenuEntryLocationName: array[TPackageStartMenuEntryLocation] of WideString = ('Start Menu', 'Desktop'); //, 'Quick Launch Toolbar');

const
  S_RelatedPackage_Deprecates = 'deprecates';

implementation

uses
  System.TypInfo,
  System.Variants,

  JsonUtil,
  KeymanVersion,
  utildir,
  utilsystem,
  VersionInfo;

const
  SPackageInfoTooNew = 'The package file is version %s.  This version can only read version '+SKeymanVersion+' and older files.';
  SReadmeNotOwnedCorrectly = 'The readme file ''%s'' referred to is not part of the package.';
  SGraphicNotOwnedCorrectly = 'The graphic file ''%s'' referred to is not part of the package.';
  SLicenseNotOwnedCorrectly = 'The license file ''%s'' referred to is not part of the package.';
  SWelcomeNotOwnedCorrectly = 'The welcome file ''%s'' referred to is not part of the package.';
  SFileNotOwnedCorrectly = 'The file ''%s'' referred to is not part of the package.';
  SDisplayFontNotOwnedCorrectly = 'The display font file ''%s'' referred to is not part of the package.';
  SOSKFontNotOwnedCorrectly = 'The OSK font file ''%s'' referred to is not part of the package.';

const
  SXML_PackageKeyboards = 'Keyboards';
  SXML_PackageKeyboard = 'Keyboard';
  SXML_PackageKeyboard_Name = 'Name';
  SXML_PackageKeyboard_ID = 'ID';
  SXML_PackageKeyboard_Version = 'Version';
  SXML_PackageKeyboard_RTL = 'RTL';
  SXML_PackageKeyboard_OSKFont = 'OSKFont';
  SXML_PackageKeyboard_DisplayFont = 'DisplayFont';
  SXML_PackageKeyboard_Languages = 'Languages';
  SXML_PackageKeyboard_Examples = 'Examples';

  SXML_PackageKeyboard_Language = 'Language';
  SXML_PackageKeyboard_Language_ID = 'ID';
  SXML_PackageKeyboard_Language_Name = 'Name';

  SXML_PackageKeyboard_Example = 'Example';
  SXML_PackageKeyboard_Example_ID = 'ID';
  SXML_PackageKeyboard_Example_Keys = 'Keys';
  SXML_PackageKeyboard_Example_Text = 'Text';
  SXML_PackageKeyboard_Example_Note = 'Note';

  SXML_PackageKeyboard_WebOskFonts = 'WebOSKFonts';
  SXML_PackageKeyboard_WebDisplayFonts = 'WebDisplayFonts';
  SXML_PackageKeyboardFont = 'Font';
  SXML_PackageKeyboardFont_Filename = 'Filename';

  SXML_PackageLexicalModels = 'LexicalModels';
  SXML_PackageLexicalModel = 'LexicalModel';
  SXML_PackageLexicalModel_Name = 'Name';
  SXML_PackageLexicalModel_ID = 'ID';
  SXML_PackageLexicalModel_RTL = 'RTL';
  SXML_PackageLexicalModel_Languages = 'Languages';

  SXML_PackageRelatedPackages = 'RelatedPackages';
  SXML_PackageRelatedPackage = 'RelatedPackage';
  SXML_PackageRelatedPackage_ID = 'ID';
  SXML_PackageRelatedPackage_Relationship = 'Relationship';

const
  SJSON_System = 'system';
  SJSON_System_KeymanDeveloperVersion = 'keymanDeveloperVersion';
  SJSON_System_FileVersion = 'fileVersion';

  SJSON_Options = 'options';
  SJSON_Options_ExecuteProgram = 'executeProgram';
  SJSON_Options_ReadMeFile = 'readmeFile';
  SJSON_Options_GraphicFile = 'graphicFile';
  SJSON_Options_LicenseFile = 'licenseFile';
  SJSON_Options_WelcomeFile = 'welcomeFile';

  SJSON_Registry = 'registry';
  SJSON_Registry_Root = 'root';
  SJSON_Registry_Key = 'key';
  SJSON_Registry_Name = 'name';
  SJSON_Registry_ValueType = 'valueType';
  SJSON_Registry_Value = 'value';

  SJSON_StartMenu = 'startMenu';
  SJSON_StartMenu_Folder = 'folder';
  SJSON_StartMenu_AddUninstallEntry = 'addUninstallEntry';
  SJSON_StartMenu_Items = 'items';
  SJSON_StartMenu_Items_Name = 'name';
  SJSON_StartMenu_Items_Filename = 'filename';
  SJSON_StartMenu_Items_Arguments = 'arguments';
  SJSON_StartMenu_Items_Icon = 'icon';
  SJSON_StartMenu_Items_Location = 'location';

  SJSON_Info = 'info';
  SJSON_Info__Description = 'description';
  SJSON_Info__URL = 'url';

  SJSON_Info_Name = 'name';
  SJSON_Info_Version = 'version';
  SJSON_Info_Copyright = 'copyright';
  SJSON_Info_Author = 'author';
  SJSON_Info_Website = 'website';
  SJSON_Info_Description = 'description';

  SJSON_PackageInfoEntryTypeNames: array[TPackageInfoEntryType] of string = (
    SJSON_Info_Name,
    SJSON_Info_Version,
    SJSON_Info_Copyright,
    SJSON_Info_Author,
    SJSON_Info_Website,
    SJSON_Info_Description,
    ''
  );

  SJSON_Files = 'files';
  SJSON_Files_Name = 'name';
  SJSON_Files_Description = 'description';
  SJSON_Files_CopyLocation = 'copyLocation';

  SJSON_Keyboards = 'keyboards';
  SJSON_Keyboard_Name = 'name';
  SJSON_Keyboard_ID = 'id';
  SJSON_Keyboard_RTL = 'rtl';
  SJSON_Keyboard_Version = 'version';
  SJSON_Keyboard_OSKFont = 'oskFont';
  SJSON_Keyboard_DisplayFont = 'displayFont';

  SJSON_Keyboard_Languages = 'languages';
  SJSON_Keyboard_Language_ID = 'id';
  SJSON_Keyboard_Language_Name = 'name';

  SJSON_Keyboard_Examples = 'examples';
  SJSON_Keyboard_Example_ID = 'id';
  SJSON_Keyboard_Example_Keys = 'keys';
  SJSON_Keyboard_Example_Text = 'text';
  SJSON_Keyboard_Example_Note = 'note';

  SJSON_Keyboard_WebOSKFonts = 'webOskFonts';
  SJSON_Keyboard_WebDisplayFonts = 'webDisplayFonts';

  SJSON_LexicalModels = 'lexicalModels';
  SJSON_LexicalModel_Name = 'name';
  SJSON_LexicalModel_ID = 'id';
  SJSON_LexicalModel_RTL = 'rtl';
  SJSON_LexicalModel_Languages = 'languages';

  SJSON_RelatedPackages = 'relatedPackages';
  SJSON_RelatedPackage_ID = 'id';
  SJSON_RelatedPackage_Relationship = 'relationship';

function XmlVarToStr(v: OleVariant): string;
begin
  Result := ReplaceStr(ReplaceStr(Trim(VarToStr(v)), #$D#$A, #$A), #$A, #$D#$A);
end;

{-------------------------------------------------------------------------------
 - TPackageOptions                                                             -
 ------------------------------------------------------------------------------}

procedure TPackageOptions.Assign(Source: TPackageOptions);
begin
  FFileVersion := Source.FileVersion;
  FExecuteProgram := Source.ExecuteProgram;
  if Assigned(Source.ReadmeFile)
    then ReadmeFile := Package.Files.FromFileName(Source.ReadmeFile.FileName)
    else ReadmeFile := nil;
  if Assigned(Source.GraphicFile)
    then GraphicFile := Package.Files.FromFileName(Source.GraphicFile.FileName)
    else GraphicFile := nil;
  if Assigned(Source.LicenseFile)
    then LicenseFile := Package.Files.FromFileName(Source.LicenseFile.FileName)
    else LicenseFile := nil;
  if Assigned(Source.WelcomeFile)
    then WelcomeFile := Package.Files.FromFileName(Source.WelcomeFile.FileName)
    else WelcomeFile := nil;
end;

constructor TPackageOptions.Create(APackage: TPackage);
begin
  inherited Create(APackage);
  FLoadLegacy := True;
  FFileVersion := SKeymanVersion70;
end;

destructor TPackageOptions.Destroy;
begin
  ReadmeFile := nil;
  GraphicFile := nil;
  LicenseFile := nil;
  WelcomeFile := nil;
  inherited Destroy;
end;

procedure TPackageOptions.ReadmeRemoved(Sender: TObject; EventType: TPackageNotifyEventType; var FAllow: Boolean);
begin
  FReadmeFile := nil;
end;

procedure TPackageOptions.GraphicRemoved(Sender: TObject; EventType: TPackageNotifyEventType; var FAllow: Boolean);
begin
  FGraphicFile := nil;
end;

procedure TPackageOptions.LicenseRemoved(Sender: TObject; EventType: TPackageNotifyEventType; var FAllow: Boolean);
begin
  FLicenseFile := nil;
end;

procedure TPackageOptions.WelcomeRemoved(Sender: TObject; EventType: TPackageNotifyEventType; var FAllow: Boolean);
begin
  FWelcomeFile := nil;
end;

procedure TPackageOptions.LoadXML(ARoot: IXMLNode);
begin
  FileVersion :=                XmlVarToStr(ARoot.ChildNodes['System'].ChildNodes['FileVersion'].NodeValue);
  ExecuteProgram :=             XmlVarToStr(ARoot.ChildNodes['Options'].ChildNodes['ExecuteProgram'].NodeValue);
  ReadmeFile :=                 Package.Files.FromFileName(XmlVarToStr(ARoot.ChildNodes['Options'].ChildNodes['ReadMeFile'].NodeValue));
  GraphicFile :=                Package.Files.FromFileName(XmlVarToStr(ARoot.ChildNodes['Options'].ChildNodes['GraphicFile'].NodeValue));
  LicenseFile :=                Package.Files.FromFileName(XmlVarToStr(ARoot.ChildNodes['Options'].ChildNodes['LicenseFile'].NodeValue));
  WelcomeFile :=                Package.Files.FromFileName(XmlVarToStr(ARoot.ChildNodes['Options'].ChildNodes['WelcomeFile'].NodeValue));
  if Assigned(ReadmeFile) then ReadmeFile.AddNotifyObject(ReadmeRemoved);
  if Assigned(GraphicFile) then GraphicFile.AddNotifyObject(GraphicRemoved);
  if Assigned(LicenseFile) then LicenseFile.AddNotifyObject(LicenseRemoved);
  if Assigned(WelcomeFile) then WelcomeFile.AddNotifyObject(WelcomeRemoved);
end;

procedure TPackageOptions.SaveXML(ARoot: IXMLNode);
begin
  ARoot.ChildNodes['System'].ChildNodes['FileVersion'].NodeValue := FileVersion;
  ARoot.ChildNodes['Options'].ChildNodes['ExecuteProgram'].NodeValue := ExecuteProgram;
  if Assigned(ReadmeFile) then
    ARoot.ChildNodes['Options'].ChildNodes['ReadMeFile'].NodeValue := ReadmeFile.RelativeFileName;
  if Assigned(GraphicFile) then
    ARoot.ChildNodes['Options'].ChildNodes['GraphicFile'].NodeValue := GraphicFile.RelativeFileName;
  if Assigned(LicenseFile) then
    ARoot.ChildNodes['Options'].ChildNodes['LicenseFile'].NodeValue := LicenseFile.RelativeFileName;
  if Assigned(WelcomeFile) then
    ARoot.ChildNodes['Options'].ChildNodes['WelcomeFile'].NodeValue := WelcomeFile.RelativeFileName;
end;

procedure TPackageOptions.LoadIni(AIni: TIniFile);
begin
  FileVersion :=                AIni.ReadString('Package', 'Version',                  '');
  ExecuteProgram :=             AIni.ReadString('Package', 'ExecuteProgram',           '');
  ReadmeFile :=                 Package.Files.FromFileName(AIni.ReadString('Package', 'ReadMeFile', ''));
  GraphicFile :=                Package.Files.FromFileName(AIni.ReadString('Package', 'GraphicFile', ''));
  if Assigned(ReadmeFile) then ReadmeFile.AddNotifyObject(ReadmeRemoved);
  if Assigned(GraphicFile) then GraphicFile.AddNotifyObject(GraphicRemoved);
  // LicenseFile not supported in ini
  // WelcomeFile not supported in ini
end;

procedure TPackageOptions.LoadJSON(ARoot: TJSONObject);
var
  FSystem, FOptions: TJSONObject;
begin
  FSystem := ARoot.Values[SJSON_System] as TJSONObject;
  FOptions := ARoot.Values[SJSON_Options] as TJSONObject;

  FileVersion :=                GetJsonValueString(FSystem, SJSON_System_FileVersion);
  ExecuteProgram :=             GetJsonValueString(FOptions, SJSON_Options_ExecuteProgram);
  ReadmeFile :=                 Package.Files.FromFileName(GetJsonValueString(FOptions, SJSON_Options_ReadMeFile));
  GraphicFile :=                Package.Files.FromFileName(GetJsonValueString(FOptions, SJSON_Options_GraphicFile));
  LicenseFile :=                Package.Files.FromFileName(GetJsonValueString(FOptions, SJSON_Options_LicenseFile));
  WelcomeFile :=                Package.Files.FromFileName(GetJsonValueString(FOptions, SJSON_Options_WelcomeFile));
  if Assigned(ReadmeFile) then ReadmeFile.AddNotifyObject(ReadmeRemoved);
  if Assigned(GraphicFile) then GraphicFile.AddNotifyObject(GraphicRemoved);
  if Assigned(LicenseFile) then LicenseFile.AddNotifyObject(LicenseRemoved);
  if Assigned(WelcomeFile) then WelcomeFile.AddNotifyObject(WelcomeRemoved);
end;

procedure TPackageOptions.SaveIni(AIni: TIniFile);
begin
  AIni.WriteString('Package', 'Version',                  FileVersion);
  AIni.WriteString('Package', 'ExecuteProgram',           ExecuteProgram);
  if Assigned(ReadmeFile) then
    AIni.WriteString('Package', 'ReadMeFile', ReadmeFile.RelativeFileName);
  if Assigned(GraphicFile) then
    AIni.WriteString('Package', 'GraphicFile', GraphicFile.RelativeFileName);
  // licenseFile not supported in ini
  // welcomeFile not supported in ini
end;

procedure TPackageOptions.SaveJSON(ARoot: TJSONObject);
var
  FOptions, FSystem: TJSONObject;
begin
  FSystem := TJSONObject.Create;
  ARoot.AddPair(SJSON_System, FSystem);
  FSystem.AddPair(SJSON_System_KeymanDeveloperVersion, GetVersionString);
  FSystem.AddPair(SJSON_System_FileVersion, FileVersion);

  FOptions := TJSONObject.Create;
  ARoot.AddPair(SJSON_Options, FOptions);
  if ExecuteProgram <> '' then
    FOptions.AddPair(SJSON_Options_ExecuteProgram, ExecuteProgram);
  if Assigned(ReadmeFile) then
    FOptions.AddPair(SJSON_Options_ReadMeFile, ReadmeFile.RelativeFileName);
  if Assigned(GraphicFile) then
    FOptions.AddPair(SJSON_Options_GraphicFile, GraphicFile.RelativeFileName);
  if Assigned(LicenseFile) then
    FOptions.AddPair(SJSON_Options_LicenseFile, LicenseFile.RelativeFileName);
  if Assigned(WelcomeFile) then
    FOptions.AddPair(SJSON_Options_WelcomeFile, WelcomeFile.RelativeFileName);
end;

procedure TPackageOptions.SetExecuteProgram(Value: WideString);
begin
  FExecuteProgram := Value;
end;

procedure TPackageOptions.SetFileVersion(Value: WideString);
begin
  if CompareVersions(Value, SKeymanVersion) < 0 then   // I5126
    raise EPackageInfo.CreateFmt(SPackageInfoTooNew, [Value]);
  FFileVersion := Value;
end;

procedure TPackageOptions.SetGraphicFile(const Value: TPackageContentFile);
begin
  if Assigned(FGraphicFile) then FGraphicFile.RemoveNotifyObject(GraphicRemoved);
  if not Assigned(Value) then
    FGraphicFile := nil
  else
  begin
    if Value.Package <> Package then raise EPackageInfo.CreateFmt(SGraphicNotOwnedCorrectly, [Value]);
    FGraphicFile := Value;
    FGraphicFile.AddNotifyObject(GraphicRemoved);
  end;
end;

procedure TPackageOptions.SetLicenseFile(const Value: TPackageContentFile);
begin
  if Assigned(FLicenseFile) then FLicenseFile.RemoveNotifyObject(LicenseRemoved);
  if not Assigned(Value) then
    FLicenseFile := nil
  else
  begin
    if Value.Package <> Package then raise EPackageInfo.CreateFmt(SLicenseNotOwnedCorrectly, [Value]);
    FLicenseFile := Value;
    FLicenseFile.AddNotifyObject(LicenseRemoved);
  end;
end;

procedure TPackageOptions.SetWelcomeFile(const Value: TPackageContentFile);
begin
  if Assigned(FWelcomeFile) then FWelcomeFile.RemoveNotifyObject(WelcomeRemoved);
  if not Assigned(Value) then
    FWelcomeFile := nil
  else
  begin
    if Value.Package <> Package then raise EPackageInfo.CreateFmt(SWelcomeNotOwnedCorrectly, [Value]);
    FWelcomeFile := Value;
    FWelcomeFile.AddNotifyObject(WelcomeRemoved);
  end;
end;

procedure TPackageOptions.SetReadmeFile(const Value: TPackageContentFile);
begin
  if Assigned(FReadmeFile) then FReadmeFile.RemoveNotifyObject(ReadmeRemoved);
  if not Assigned(Value) then
    FReadmeFile := nil
  else
  begin
    if Value.Package <> Package then raise EPackageInfo.CreateFmt(SReadmeNotOwnedCorrectly, [Value]);
    FReadmeFile := Value;
    FReadmeFile.AddNotifyObject(ReadmeRemoved);
  end;
end;

{-------------------------------------------------------------------------------
 - TPackageInfoEntry                                                           -
 ------------------------------------------------------------------------------}

procedure TPackageInfoEntry.Assign(Source: TPackageInfoEntry);
begin
  FDescription := Source.Description;
  FName := Source.Name;
  FURL := Source.URL;
  FInfoType := Source.InfoType;
end;

procedure TPackageInfoEntry.SetDescription(Value: WideString);
begin
  FDescription := Value;
end;

procedure TPackageInfoEntry.SetName(Value: WideString);
var
  piet: TPackageInfoEntryType;
begin
  FName := Value;
  FInfoType := pietOther;

  for piet := Low(PackageInfoEntryTypeNames) to High(PackageInfoEntryTypeNames) do
    if SameText(PackageInfoEntryTypeNames[piet], FName) then
      FInfoType := piet;
end;

procedure TPackageInfoEntry.SetURL(Value: WideString);
begin
  FURL := Value;
end;

{-------------------------------------------------------------------------------
 - TPackageInfoEntryList                                                       -
 ------------------------------------------------------------------------------}

procedure TPackageInfoEntryList.LoadXML(ARoot: IXMLNode);
var
  i: Integer;
  inf: TPackageInfoEntry;
  ANode: IXMLNode;
begin
  Clear;
  ARoot := ARoot.ChildNodes['Info'];
  for i := 0 to ARoot.ChildNodes.Count - 1 do
  begin
    ANode := ARoot.ChildNodes[i];
    inf := TPackageInfoEntry.Create(Package);
    inf.Name := ANode.NodeName;
    inf.Description := XmlVarToStr(ANode.NodeValue);
    inf.URL := XmlVarToStr(ANode.Attributes['URL']);
    Add(inf);
  end;
end;

procedure TPackageInfoEntryList.SaveXML(ARoot: IXMLNode);
var
  i: Integer;
  ANode: IXMLNode;
begin
  ARoot := ARoot.ChildNodes['Info'];
  for i := 0 to Count - 1 do
  begin
    ANode := ARoot.AddChild(Items[i].Name);
    ANode.NodeValue := Items[i].Description;
    ANode.Attributes['URL'] := Items[i].URL;
  end;
end;

procedure TPackageInfoEntryList.LoadIni(AIni: TIniFile);
var
  s: TStringList;
  i: Integer;
  description, url, t: WideString;
  inf: TPackageInfoEntry;
begin
  s := TStringList.Create;
  try
    AIni.ReadSection('Info', s);
    for i := 0 to s.Count - 1 do
    begin
      t := '"'+AIni.ReadString('Info', s[i], '')+'"';
      description := CommaToken(t);
      url := CommaToken(t);

      inf := TPackageInfoEntry.Create(Package);
      inf.Name := s[i];
      inf.Description := description;
      inf.URL := url;
      Add(inf);
    end;
  finally
    s.Free;
  end;
end;

procedure TPackageInfoEntryList.LoadJSON(ARoot: TJSONObject);
var
  name: string;
  i: Integer;
  inf: TPackageInfoEntry;
  FItems: TJSONObject;
  FNode: TJSONObject;
  v: TPackageInfoEntryType;
begin
  Clear;
  FItems := ARoot.Values[SJSON_Info] as TJSONObject;
  for i := 0 to FItems.Count - 1 do
  begin
    name := FItems.Pairs[i].JsonString.Value;
    for v := Low(TPackageInfoEntryType) to High(TPackageInfoEntryType) do
      if SJSON_PackageInfoEntryTypeNames[v] = name then
      begin
        FNode := FItems.Pairs[i].JsonValue as TJSONObject;
        inf := TPackageInfoEntry.Create(Package);
        inf.Name := name;
        inf.Description := GetJsonValueString(FNode, SJSON_Info__Description);
        inf.URL := GetJsonValueString(FNode, SJSON_Info__URL);
        Add(inf);
        Break;
      end;
  end;
end;

procedure TPackageInfoEntryList.SaveIni(AIni: TIniFile);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    AIni.WriteString('Info', Items[i].Name, Format('"%s","%s"', [Items[i].Description, Items[i].URL]));
end;

procedure TPackageInfoEntryList.SaveJSON(ARoot: TJSONObject);
var
  i: Integer;
  AInfoRoot, ANode: TJSONObject;
begin
  AInfoRoot := TJSONObject.Create;
  ARoot.AddPair(SJSON_Info, AInfoRoot);
  for i := 0 to Count - 1 do
  begin
    if SJSON_PackageInfoEntryTypeNames[Items[i].InfoType] <> '' then
    begin
      ANode := TJSONObject.Create;
      AInfoRoot.AddPair(SJSON_PackageInfoEntryTypeNames[Items[i].InfoType], ANode);
      ANode.AddPair(SJSON_Info__Description, Items[i].Description);
      if Items[i].URL <> '' then
        ANode.AddPair(SJSON_Info__URL, Items[i].URL);
    end;
  end;
end;

function TPackageInfoEntryList.DescIndexOf(Name: WideString): WideString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
    if WideLowerCase(Items[i].Name) = WideLowerCase(Name) then Result := Items[i].Description;
end;

function TPackageInfoEntryList.UrlIndexOf(Name: WideString): WideString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
    if WideLowerCase(Items[i].Name) = WideLowerCase(Name) then Result := Items[i].URL;
end;

procedure TPackageInfoEntryList.SetDesc(Name, Desc: WideString);
var
  i: Integer;
  inf: TPackageInfoEntry;
begin
  i := IndexOf(Name);
  if i = -1 then
  begin
    inf := TPackageInfoEntry.Create(Package);
    inf.Name := Name;
    Add(inf);
  end
  else
    inf := Items[i];
  inf.Description := Desc;
end;

procedure TPackageInfoEntryList.SetURL(Name, URL: WideString);
var
  i: Integer;
  inf: TPackageInfoEntry;
begin
  i := IndexOf(Name);
  if i = -1 then
  begin
    inf := TPackageInfoEntry.Create(Package);
    inf.Name := Name;
    Add(inf);
  end
  else
    inf := Items[i];
  inf.URL := URL;
end;

function TPackageInfoEntryList.IndexOf(Name: WideString): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if WideLowerCase(Items[i].Name) = WideLowerCase(Name) then Result := i;
end;

procedure TPackageInfoEntryList.Assign(Source: TPackageInfoEntryList);
var
  i: Integer;
  pie: TPackageInfoEntry;
begin
  Clear;
  for i := 0 to Source.Count - 1 do
  begin
    pie := TPackageInfoEntry.Create(Package);
    pie.Assign(Source[i]);
    Add(pie);
  end;
end;

{-------------------------------------------------------------------------------
 - TPackageStartMenuEntry                                                      -
 ------------------------------------------------------------------------------}

procedure TPackageStartMenuEntry.Assign(Source: TPackageStartMenuEntry);
begin
  Name := Source.Name;
  Prog := Source.Prog;
  Params := Source.Params;
  Icon := Source.Icon;
  Location := Source.Location;
end;

{-------------------------------------------------------------------------------
 - TPackageStartMenuEntryList                                                  -
 ------------------------------------------------------------------------------}

function StrToStartMenuEntryLocation(s: WideString): TPackageStartMenuEntryLocation;
var
  v: Integer;
begin
  v := System.TypInfo.GetEnumValue(TypeInfo(TPackageStartMenuEntryLocation), s);
  if v = -1 then Result := psmelStartMenu
  else Result := TPackageStartMenuEntryLocation(v);
end;

procedure TPackageStartMenuEntryList.LoadXML(ARoot: IXMLNode);
var
  sme: TPackageStartMenuEntry;
  i: Integer;
  ANode: IXMLNode;
begin
  Clear;
  ANode := ARoot.ChildNodes['StartMenu'].ChildNodes['Items'];
  for i := 0 to ANode.ChildNodes.Count - 1 do
  begin
    with ANode.ChildNodes[i] do
    begin
      sme := TPackageStartMenuEntry.Create(Package);
      sme.Name := XmlVarToStr(ChildNodes['Name'].NodeValue);
      sme.Prog := XmlVarToStr(ChildNodes['FileName'].NodeValue);
      sme.Params := XmlVarToStr(ChildNodes['Arguments'].NodeValue);
      sme.Icon := XmlVarToStr(ChildNodes['Icon'].NodeValue);
      sme.Location := StrToStartMenuEntryLocation(XmlVarToStr(ChildNodes['Location'].NodeValue));
      Self.Add(sme);
    end;
  end;
end;

procedure TPackageStartMenuEntryList.SaveXML(ARoot: IXMLNode);
var
  i: Integer;
  ANode: IXMLNode;
begin
  ANode := ARoot.ChildNodes['StartMenu'].ChildNodes['Items'];
  for i := 0 to Count - 1 do
    with ANode.AddChild('Item') do
    begin
      ChildNodes['Name'].NodeValue := Items[i].Name;
      ChildNodes['FileName'].NodeValue := Items[i].Prog;
      ChildNodes['Arguments'].NodeValue := Items[i].Params;
      ChildNodes['Icon'].NodeValue := Items[i].Icon;
      ChildNodes['Location'].NodeValue := System.TypInfo.GetEnumName(TypeInfo(TPackageStartMenuEntryLocation), Ord(Items[i].Location));
    end;
end;

procedure TPackageStartMenuEntryList.LoadIni(AIni: TIniFile);
var
  s: TStringList;
  t, params, prog: WideString;
  sme: TPackageStartMenuEntry;
  i: Integer;
begin
  s := TStringList.Create;
  try
    AIni.ReadSection('StartMenuEntries', s);
    for i := 0 to s.Count - 1 do
    begin
      t := '"'+AIni.ReadString('StartMenuEntries', s[i], '')+'"';
      prog := CommaToken(t);
      params := CommaToken(t);

      sme := TPackageStartMenuEntry.Create(Package);
      sme.Name := s[i];
      sme.Prog := prog;
      sme.Params := params;

      // For backward compatibility, we need these to go in another section.
      // Earlier versions of Keyman ignored these values
      sme.Icon := AIni.ReadString('StartMenuEntries_Icon', s[i], '');
      sme.Location := TPackageStartMenuEntryLocation(StrToIntDef(AIni.ReadString('StartMenuEntries_Location', s[i], ''), 0));

      Add(sme);
    end;
  finally
    s.Free;
  end;
end;

procedure TPackageStartMenuEntryList.LoadJSON(ARoot: TJSONObject);
var
  i: Integer;
  AItems: TJSONArray;
  AItem: TJSONObject;
  sme: TPackageStartMenuEntry;
begin
  AItems := ARoot.Values[SJSON_StartMenu_Items] as TJSONArray;
  if not Assigned(AItems) then
    Exit;

  for i := 0 to AItems.Count - 1 do
  begin
    AItem := AItems.Items[i] as TJSONObject;

    sme := TPackageStartMenuEntry.Create(Package);
    sme.Name := GetJsonValueString(AItem, SJSON_StartMenu_Items_Name);
    sme.Prog := GetJsonValueString(AItem, SJSON_StartMenu_Items_Filename);
    sme.Params := GetJsonValueString(AItem, SJSON_StartMenu_Items_Arguments);
    sme.Icon := GetJsonValueString(AItem, SJSON_StartMenu_Items_Icon);
    sme.Location := StrToStartMenuEntryLocation(GetJsonValueString(AItem, SJSON_StartMenu_Items_Location));
    Self.Add(sme);
  end;
end;

procedure TPackageStartMenuEntryList.SaveIni(AIni: TIniFile);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    AIni.WriteString('StartMenuEntries', Items[i].Name, Format('"%s","%s"', [Items[i].Prog, Items[i].Params]));

    // For backward compatibility, we need these to go in another section.
    // Earlier versions of Keyman ignored these values
    if Items[i].Icon <> '' then
      AIni.WriteString('StartMenuEntries_Icon', Items[i].Name, Items[i].Icon);

    if Items[i].Location <> psmelStartMenu then
      AIni.WriteString('StartMenuEntries_Location', Items[i].Name, IntToStr(Ord(Items[i].Location)));
  end;
end;

procedure TPackageStartMenuEntryList.SaveJSON(ARoot: TJSONObject);
var
  i: Integer;
  AItems: TJSONArray;
  AItem: TJSONObject;
begin
  AItems := TJSONArray.Create;
  ARoot.AddPair(SJSON_StartMenu_Items, AItems);
  for i := 0 to Count - 1 do
  begin
    AItem := TJSONObject.Create;
    AItems.Add(AItem);
    AItem.AddPair(SJSON_StartMenu_Items_Name, Items[i].Name);
    AItem.AddPair(SJSON_StartMenu_Items_Filename, Items[i].Prog);
    if Items[i].Params <> '' then
      AItem.AddPair(SJSON_StartMenu_Items_Arguments, Items[i].Params);
    if Items[i].Icon <> '' then
      AItem.AddPair(SJSON_StartMenu_Items_Icon, Items[i].Icon);
    if Items[i].Location <> psmelStartMenu then
      AItem.AddPair(SJSON_StartMenu_Items_Location,
        System.TypInfo.GetEnumName(TypeInfo(TPackageStartMenuEntryLocation), Ord(Items[i].Location)));
  end;
end;

procedure TPackageStartMenuEntryList.Assign(Source: TPackageStartMenuEntryList);
var
  i: Integer;
  psme: TPackageStartMenuEntry;
begin
  Clear;
  for i := 0 to Source.Count - 1 do
  begin
    psme := TPackageStartMenuEntry.Create(Package);
    psme.Assign(Source[i]);
    Add(psme);
  end;
end;

{-------------------------------------------------------------------------------
 - TPackageStartMenu                                                           -
 ------------------------------------------------------------------------------}

procedure TPackageStartMenu.Assign(Source: TPackageStartMenu);
begin
  Path := Source.Path;
  DoCreate := Source.DoCreate;
  AddUninstallEntry := Source.AddUninstallEntry;
  Entries.Assign(Source.Entries);
end;

constructor TPackageStartMenu.Create(APackage: TPackage);
begin
  inherited Create(APackage);
  Entries := TPackageStartMenuEntryList.Create(Package);
end;

destructor TPackageStartMenu.Destroy;
begin
  Entries.Free;
  inherited;
end;

procedure TPackageStartMenu.LoadXML(ARoot: IXMLNode);
var
  ANode: IXMLNode;
begin
  ANode := ARoot.ChildNodes['StartMenu'];
  Path := XmlVarToStr(ANode.ChildNodes['Folder'].NodeValue);
  DoCreate := Path <> '';
  AddUninstallEntry := ANode.ChildNodes.IndexOf('AddUninstallEntry') >= 0;
  Entries.LoadXML(ARoot);
end;

procedure TPackageStartMenu.SaveXML(ARoot: IXMLNode);
var
  ANode: IXMLNode;
begin
  ANode := ARoot.ChildNodes['StartMenu'];
  ANode.ChildNodes['Folder'].NodeValue := Path;
  if AddUninstallEntry then ANode.AddChild('AddUninstallEntry');
  Entries.SaveXML(ARoot);
end;

procedure TPackageStartMenu.LoadIni(AIni: TIniFile);
begin
  Path :=              AIni.ReadString('StartMenu', 'Path',              '');
  DoCreate :=          AIni.ReadBool(  'StartMenu', 'Create',            False);
  AddUninstallEntry := AIni.ReadBool(  'StartMenu', 'AddUninstallEntry', False);
  Entries.LoadIni(AIni);
end;

procedure TPackageStartMenu.LoadJSON(ARoot: TJSONObject);
var
  ANode: TJSONObject;
begin
  ANode := ARoot.Values[SJSON_StartMenu] as TJSONObject;
  if not Assigned(ANode) then
    Exit;

  Path := GetJsonValueString(ANode, SJSON_StartMenu_Folder);
  DoCreate := Path <> '';
  AddUninstallEntry := GetJsonValueBool(ANode, SJSON_StartMenu_AddUninstallEntry);
  Entries.LoadJSON(ANode);
end;

procedure TPackageStartMenu.SaveIni(AIni: TIniFile);
begin
  AIni.WriteString('StartMenu', 'Path',              Path);
  AIni.WriteBool(  'StartMenu', 'Create',            DoCreate);
  AIni.WriteBool(  'StartMenu', 'AddUninstallEntry', AddUninstallEntry);
  Entries.SaveIni(AIni);
end;

procedure TPackageStartMenu.SaveJSON(ARoot: TJSONObject);
var
  ANode: TJSONObject;
begin
  if (Path = '') and (Entries.Count = 0) then
    Exit;

  ANode := TJSONObject.Create;
  ARoot.AddPair(SJSON_StartMenu, ANode);
  if Path <> '' then
    ANode.AddPair(SJSON_StartMenu_Folder, Path);
  if AddUninstallEntry then
    ANode.AddPair(SJSON_StartMenu_AddUninstallEntry, TJSONBool.Create(True));
  Entries.SaveJSON(ANode);
end;

{-------------------------------------------------------------------------------
 - TPackageSubFile                                                             -
 ------------------------------------------------------------------------------}

procedure TPackageContentFile.Assign(Source: TPackageContentFile);
begin
  FCopyLocation := Source.CopyLocation;
  FDescription := Source.Description;
  FFileName := Source.FileName;
  FFileType := Source.FileType;
end;

function TPackageContentFile.RelativeFileName: WideString;
begin
  Result := ExtractRelativePath(Package.FileName, FFileName);
end;

procedure TPackageContentFile.SetCopyLocation(const Value: TPackageFileCopyLocation);
begin
  FCopyLocation := Value;
end;

procedure TPackageContentFile.SetDescription(Value: WideString);
begin
  FDescription := Value;
end;

procedure TPackageContentFile.SetFileName(Value: WideString);
begin
  FFileName := Value;
  FileType := GetFileTypeFromFileName(FFileName);
end;

procedure TPackageContentFile.SetFileType(const Value: TKMFileType);
begin
  FFileType := Value;
end;

{-------------------------------------------------------------------------------
 - TPackageSubFileList                                                         -
 ------------------------------------------------------------------------------}

function TPackageContentFileList.IndexOfFileType(FFileType: TKMFileType): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].FileType = FFileType then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

procedure TPackageContentFileList.Delete(Index: Integer);
begin
  Items[Index].Notify(netDeleteFromList);
  inherited Delete(Index);
end;

procedure TPackageContentFileList.LoadXML(ARoot: IXMLNode);
var
  subfile: TPackageContentFile;
  i: Integer;
  ANode: IXMLNode;
begin
  Clear;
  ANode := ARoot.ChildNodes['Files'];
  for i := 0 to ANode.ChildNodes.Count - 1 do
  begin
    with ANode.ChildNodes[i] do
    begin
      subfile := TPackageContentFile.Create(Package);
      subfile.FileName := XmlVarToStr(ChildNodes['Name'].NodeValue);
      subfile.Description := XmlVarToStr(ChildNodes['Description'].NodeValue);
      subfile.FCopyLocation := TPackageFileCopyLocation(StrToIntDef(XmlVarToStr(ChildNodes['Location'].NodeValue), 0));
      Add(subfile);
    end;
  end;
end;

procedure TPackageContentFileList.SaveXML(ARoot: IXMLNode);
var
  i: Integer;
  ANode: IXMLNode;
begin
  ANode := ARoot.ChildNodes['Files'];
  for i := 0 to Count - 1 do
  begin
    with ANode.AddChild('File') do
    begin
      ChildNodes['Name'].NodeValue := Items[i].FileName;
      ChildNodes['Description'].NodeValue := Items[i].Description;
      ChildNodes['CopyLocation'].NodeValue := Ord(Items[i].CopyLocation);
      ChildNodes['FileType'].NodeValue := ExtractFileExt(Items[i].FFileName);
    end;
  end;
end;

procedure TPackageContentFileList.LoadIni(AIni: TIniFile);
var
  s: TStringList;
  t, description, filename: WideString;
  copylocation: TPackageFileCopyLocation;
  subfile: TPackageContentFile;
  i: Integer;
begin
  Clear;
  s := TStringList.Create;
  try
    AIni.ReadSection('Files', s);
    for i := 0 to s.Count - 1 do
    begin
      t := {'"'+}AIni.ReadString('Files', s[i], ''){+'"'};
      description := CommaToken(t);
      filename := CommaToken(t);
      copylocation := TPackageFileCopyLocation(StrToIntDef(CommaToken(t), 0));// CommaToken(t) = '1';
      subfile := TPackageContentFile.Create(Package);
      subfile.FileName := filename;
      subfile.Description := description;
      subfile.FCopyLocation := copylocation;
      Add(subfile);
    end;
  finally
    s.Free;
  end;
end;

procedure TPackageContentFileList.LoadJSON(ARoot: TJSONObject);
var
  subfile: TPackageContentFile;
  i: Integer;
  FItems: TJSONArray;
  FItem: TJSONObject;
begin
  Clear;
  FItems := ARoot.Values[SJSON_Files] as TJSONArray;
  for i := 0 to FItems.Count - 1 do
  begin
    FItem := FItems.Items[i] as TJSONObject;

    subfile := TPackageContentFile.Create(Package);
    subfile.FileName := GetJsonValueString(FItem, SJSON_Files_Name);
    subfile.Description := GetJsonValueString(FItem, SJSON_Files_Description);
    if Assigned(FItem.Values[SJSON_Files_CopyLocation]) then
      subfile.FCopyLocation := TPackageFileCopyLocation((FItem.Values[SJSON_Files_CopyLocation] as TJSONNumber).AsInt);
    Add(subfile);
  end;
end;

procedure TPackageContentFileList.SaveIni(AIni: TIniFile);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    AIni.WriteString('Files', IntToStr(i), Format('"%s","%s",%d', [Items[i].Description,
      Items[i].FileName, Ord(Items[i].CopyLocation)]));
end;

procedure TPackageContentFileList.SaveJSON(ARoot: TJSONObject);
var
  i: Integer;
  AItems: TJSONArray;
  AItem: TJSONObject;
begin
  if Count = 0 then
    Exit;

  AItems := TJSONArray.Create;
  ARoot.AddPair(SJSON_Files, AItems);
  for i := 0 to Count - 1 do
  begin
    AItem := TJSONObject.Create;
    AItems.Add(AItem);
    AItem.AddPair(SJSON_Files_Name, Items[i].FileName);
    AItem.AddPair(SJSON_Files_Description, Items[i].Description);
    if Items[i].CopyLocation <> pfclPackage then
      AItem.AddPair(SJSON_Files_CopyLocation, TJSONNumber.Create(Ord(Items[i].CopyLocation)));
  end;
end;

procedure TPackageContentFileList.Assign(Source: TPackageContentFileList);
var
  i: Integer;
  psf: TPackageContentFile;
begin
  Clear;
  for i := 0 to Source.Count - 1 do
  begin
    psf := TPackageContentFile.Create(Package);
    psf.Assign(Source[i]);
    Add(psf);
  end;
end;

function TPackageContentFileList.FromFileName(Filename: WideString): TPackageContentFile;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if WideSameText(Items[i].FileName, Filename) then
    begin
      Result := Items[i];
      Exit;
    end;
  Result := nil;
end;

function TPackageContentFileList.FromFileNameEx(
  Filename: WideString): TPackageContentFile;
var
  f: WideString;
  i: Integer;
begin
  f := ExtractFileName(FileName);
  for i := 0 to Count - 1 do
    if WideSameText(ExtractFileName(Items[i].FileName), f) then  // I2574
    begin
      Result := Items[i];
      Exit;
    end;
  Result := nil;
end;

{-------------------------------------------------------------------------------
 - TPackage                                                                    -
 ------------------------------------------------------------------------------}

procedure TPackage.Assign(Source: TPackage);
begin
  FFileName := Source.FileName;
  Files.Assign(Source.Files);
  Options.Assign(Source.Options);
  StartMenu.Assign(Source.StartMenu);
  Info.Assign(Source.Info);
  Keyboards.Assign(Source.Keyboards);
  LexicalModels.Assign(Source.LexicalModels);
  RelatedPackages.Assign(Source.RelatedPackages);
end;

constructor TPackage.Create;
begin
  inherited Create;
  // A subclass of this may have already created (subclasses of) these, so only create if not already done
  if not Assigned(Files)     then Files     := TPackageContentFileList.Create(Self);
  if not Assigned(Options)   then Options   := TPackageOptions.Create(Self);
  if not Assigned(StartMenu) then StartMenu := TPackageStartMenu.Create(Self);
  if not Assigned(Info)      then Info      := TPackageInfoEntryList.Create(Self);
  if not Assigned(Keyboards) then Keyboards := TPackageKeyboardList.Create(Self);
  if not Assigned(LexicalModels) then LexicalModels := TPackageLexicalModelList.Create(Self);
  if not Assigned(RelatedPackages) then RelatedPackages := TPackageRelatedPackageList.Create(Self);
end;

destructor TPackage.Destroy;
begin
  Files.Free;
  Options.Free;
  StartMenu.Free;
  Info.Free;
  Keyboards.Free;
  LexicalModels.Free;
  RelatedPackages.Free;
  inherited Destroy;
end;

procedure TPackage.LoadIni;
var
  ini: TIniFile;
begin
  FWasIni := True;
  ini := TIniFile.Create(FileName);

  try
    if not ini.ValueExists('Package', 'Version') then
    begin
      { File is version 5.0 kps file }
      Import(ini);
      Exit;
    end;

    DoLoadIni(ini);
  finally
    ini.Free;
  end;
end;

procedure TPackage.DoLoadIni(ini: TIniFile);
begin
  StartMenu.LoadIni(ini);
  Info.LoadIni(ini);
  Files.LoadIni(ini);
  Options.LoadLegacy := FLoadLegacy;
  Options.LoadIni(ini);
  Keyboards.LoadIni(ini);
  //LexicalModels not supported in ini
  //RelatedPackages not supported in ini
end;

procedure TPackage.LoadJSON;
var
  FJSON: TJSONObject;
begin
  with TStringStream.Create('', TEncoding.UTF8) do
  try
    LoadFromFile(FileName);
    FJSON := TJSONObject.ParseJSONValue(DataString) as TJSONObject;
  finally
    Free;
  end;

  DoLoadJSON(FJSON);
end;

procedure TPackage.LoadJSONFromStream(Stream: TStream);
var
  FJSON: TJSONObject;
begin
  with TStringStream.Create('', TEncoding.UTF8) do
  try
    LoadFromStream(Stream);
    FJSON := TJSONObject.ParseJSONValue(DataString) as TJSONObject;
  finally
    Free;
  end;

  DoLoadJSON(FJSON);
end;

procedure PackageLoadError(Message: WideString);
begin
  raise EPackageInfo.Create(Message);
end;

procedure TPackage.LoadXML;
    function IsIniFile: Boolean;
    var
      i: Integer;
    begin
      if FileExists(FileName) then
        with TStringList.Create do
        try
          LoadFromFile(FileName);  // Prolog will inform encoding
          for i := 0 to Count - 1 do
            if Trim(Strings[i]) <> '' then
            begin
              Result := Copy(Strings[i], 1, 1) = '[';
              Exit;
            end;
        finally
          Free;
        end;
      Result := False;
    end;
var
  doc: IXMLDocument;
  root: IXMLNode;
  f: TSearchRec;
begin
  FWasIni := False;
  if IsIniFile then
    LoadIni { effectively imports from ini format }
  else
  begin
    if FindFirst(FileName, 0, f) = 0 then
    begin
      System.Sysutils.FindClose(f);
      if f.Size = 0 then Exit;
    end
    else
      Exit;

    doc := LoadXMLDocument(FileName);
    try
      doc.Options := [doNodeAutoCreate];
      root := doc.DocumentElement;
      if root.NodeName <> XMLRootNode then
        PackageLoadError('Not a valid '+XMLRootNode+' file');

      DoLoadXML(root);

    finally
      doc := nil;
      root := nil;
    end;
  end;
end;

procedure TPackage.LoadXMLFromText(Text: WideString);
var
  doc: IXMLDocument;
  root: IXMLNode;
begin
  doc := LoadXMLData(Text);
  try
    doc.Options := [doNodeAutoCreate];
    root := doc.DocumentElement;
    if root.NodeName <> XMLRootNode then
      PackageLoadError('Not a valid '+XMLRootNode+' file');

    DoLoadXML(root);

  finally
    doc := nil;
    root := nil;
  end;
end;

procedure TPackage.DoLoadJSON(ARoot: TJSONObject);
begin
  StartMenu.LoadJSON(ARoot);
  Info.LoadJSON(ARoot);
  Files.LoadJSON(ARoot);
  Options.LoadJSON(ARoot);
  Keyboards.LoadJSON(ARoot);
  if CompareVersions(Options.FileVersion, SKeymanVersion120) >= 0 then
    LexicalModels.LoadJSON(ARoot);
  RelatedPackages.LoadJSON(ARoot);
end;

procedure TPackage.DoLoadXML(ARoot: IXMLNode);
var
  FVersion: WideString;
begin
  // Keyman for Windows 14 and earlier only accepted version 7.0 and 12.0.
  // But Keyman for Windows 15 and onward accept any version number up to and
  // including their version.
  FVersion := XmlVarToStr(ARoot.ChildNodes['System'].ChildNodes['FileVersion'].NodeValue);
  if CompareVersions(SKeymanVersion, FVersion) > 0 then
    PackageLoadError('Package file version '+FVersion+' can only be loaded by a newer version of this software.');

  StartMenu.LoadXML(ARoot);
  Info.LoadXML(ARoot);
  Files.LoadXML(ARoot);
  Options.LoadXML(ARoot);
  Keyboards.LoadXML(ARoot);
  if CompareVersions(FVersion, SKeymanVersion120) >= 0 then
    LexicalModels.LoadXML(ARoot);
  RelatedPackages.LoadXML(ARoot);
end;

procedure TPackage.DoSaveIni(ini: TIniFile);
begin
  FixupFileVersion;
  Options.SaveIni(ini);
  StartMenu.SaveIni(ini);
  Info.SaveIni(ini);
  Files.SaveIni(ini);
  Keyboards.SaveIni(ini);
  // Lexical models not supported in ini
  // RelatedPackages not supported in ini
end;

procedure TPackage.FixupFileVersion;
begin
  // Note: see also CompilePackage, MergeKeyboardInfo
  if LexicalModels.Count > 0 then
    Options.FileVersion := SKeymanVersion120
  else if Options.FileVersion = '' then
    Options.FileVersion := SKeymanVersion70;
end;

procedure TPackage.DoSaveJSON(ARoot: TJSONObject);
begin
  FixupFileVersion;
  Options.SaveJSON(ARoot);
  StartMenu.SaveJSON(ARoot);
  Info.SaveJSON(ARoot);
  Files.SaveJSON(ARoot);
  Keyboards.SaveJSON(ARoot);
  if LexicalModels.Count > 0 then
    LexicalModels.SaveJSON(ARoot);
  RelatedPackages.SaveJSON(ARoot);
end;

procedure TPackage.DoSaveXML(ARoot: IXMLNode);
begin
  FixupFileVersion;
  ARoot.ChildNodes['System'].ChildNodes['KeymanDeveloperVersion'].NodeValue := GetVersionString;
  Options.SaveXML(ARoot);
  StartMenu.SaveXML(ARoot);
  Info.SaveXML(ARoot);
  Files.SaveXML(ARoot);
  Keyboards.SaveXML(ARoot);
  if LexicalModels.Count > 0 then
    LexicalModels.SaveXML(ARoot);
  if RelatedPackages.Count > 0 then
    RelatedPackages.SaveXML(ARoot);
end;

procedure TPackage.SaveIni;
var
  ini: TIniFile;
begin
  if FileExists(FileName) then DeleteFileCleanAttr(FileName);   // I4574

  ini := TIniFile.Create(FileName);
  try
    DoSaveIni(ini);
    ini.UpdateFile;
  finally
    ini.Free;
  end;
end;

procedure TPackage.SaveJSON;
var
  FJSON: TJSONObject;
  s: TStringList;
begin
  if FileExists(FileName) then DeleteFileCleanAttr(FileName);   // I4574

  FJSON := TJSONObject.Create;
  try
    DoSaveJSON(FJSON);
    s := TStringList.Create;
    try
      PrettyPrintJSON(FJSON, s);
      with TStringStream.Create(s.Text, TEncoding.UTF8) do
      try
        // Use TStringStream to avoid BOM from TStringList
        SaveToFile(FileName);
      finally
        Free;
      end;
    finally
      s.Free;
    end;
  finally
    FJSON.Free;
  end;
end;

procedure TPackage.SaveXML;
var
  doc: IXMLDocument;
  root: IXMLNode;
begin
  doc := NewXMLDocument;
  doc.Encoding := 'utf-8';

  root := doc.CreateElement(XMLRootNode, '');
  doc.DocumentElement := root;
  doc.Options := [doNodeAutoCreate, doNodeAutoIndent];
  try
    DoSaveXML(root);
    doc.SaveToFile(FileName);
  finally
    root := nil;
    doc := nil;
  end;
end;

function TPackage.SaveXMLToText: WideString;
var
  doc: IXMLDocument;
  root: IXMLNode;
begin
  doc := NewXMLDocument;
  doc.Encoding := 'utf-8';

  root := doc.CreateElement(XMLRootNode, '');
  doc.DocumentElement := root;
  doc.Options := [doNodeAutoCreate, doNodeAutoIndent];
  try
    DoSaveXML(root);
    doc.SaveToXML(Result);
  finally
    root := nil;
    doc := nil;
  end;
end;

function TPackage.XMLRootNode: WideString;
begin
  Result := 'Package';
end;

{ TPackageBaseObject }

constructor TPackageBaseNotifyObject.Create(APackage: TPackage);
begin
  inherited Create(APackage);
  FNotifyObjects := TObjectList<TPackageNotifyEventWrapper>.Create;
end;

destructor TPackageBaseNotifyObject.Destroy;
begin
  Notify(netDestroy);
  FNotifyObjects.Free;
  inherited Destroy;
end;

function TPackageBaseNotifyObject.Notify(EventType: TPackageNotifyEventType): Boolean;
var
  i: Integer;
  fAllow: Boolean;
begin
  for i := 0 to FNotifyObjects.Count - 1 do
  begin
    fAllow := True;
    FNotifyObjects[i].FEvent(Self, EventType, fAllow);
  end;
  Result := True;
end;

procedure TPackageBaseNotifyObject.AddNotifyObject(FEventHandler: TPackageNotifyEvent);
var
  ev: TPackageNotifyEventWrapper;
begin
  RemoveNotifyObject(FEventHandler);
  ev := TPackageNotifyEventWrapper.Create;
  ev.FEvent := FEventHandler;
  FNotifyObjects.Add(ev);
end;

procedure TPackageBaseNotifyObject.RemoveNotifyObject(FEventHandler: TPackageNotifyEvent);
var
  i: Integer;
begin
  for i := 0 to FNotifyObjects.Count - 1 do
    if @FNotifyObjects[i].FEvent = @FEventHandler then
    begin
      FNotifyObjects.Delete(i);
      Exit;
    end;
end;

{ TPackageBaseObject }

constructor TPackageBaseObject.Create(APackage: TPackage);
begin
  inherited Create;
  FPackage := APackage;
end;

{ TPackageObjectList<T> }

constructor TPackageObjectList<T>.Create(APackage: TPackage);
begin
  inherited Create;
  FPackage := APackage;
end;

{ TPackageKeyboard }

procedure TPackageKeyboard.Assign(Source: TPackageKeyboard);
var
  i: Integer;
  FLanguage: TPackageKeyboardLanguage;
  FExample: TPackageKeyboardExample;
begin
  FName := Source.Name;
  FID := Source.ID;
  FVersion := Source.Version;
  FMinKeymanVersion := Source.FMinKeymanVersion;
  FRTL := Source.RTL;
  if Assigned(Source.OSKFont)
    then FOSKFont := Package.Files.FromFileNameEx(Source.OSKFont.FileName)
    else FOSKFont := nil;
  if Assigned(Source.DisplayFont)
    then FDisplayFont := Package.Files.FromFileNameEx(Source.DisplayFont.FileName)
    else FDisplayFont := nil;
  FLanguages.Clear;
  for i := 0 to Source.Languages.Count - 1 do
  begin
    FLanguage := TPackageKeyboardLanguage.Create(Package);
    FLanguage.ID := Source.Languages[i].ID;
    FLanguage.Name := Source.Languages[i].Name;
    FLanguages.Add(FLanguage);
  end;
  FExamples.Clear;
  for i := 0 to Source.Examples.Count - 1 do
  begin
    FExample := TPackageKeyboardExample.Create(Package);
    FExample.ID := Source.Examples[i].ID;
    FExample.Keys := Source.Examples[i].Keys;
    FExample.Text := Source.Examples[i].Text;
    FExample.Note := Source.Examples[i].Note;
    FExamples.Add(FExample);
  end;

  FWebOSKFonts.Clear;
  FWebOSKFonts.Assign(Source.WebOSKFonts);

  FWebDisplayFonts.Clear;
  FWebDisplayFonts.Assign(Source.WebDisplayFonts);
end;

procedure TPackageKeyboard.SetDisplayFont(const Value: TPackageContentFile);
begin
  if Assigned(FDisplayFont) then
    FDisplayFont.RemoveNotifyObject(FontRemoved);
  if not Assigned(Value) then
    FDisplayFont := nil
  else
  begin
    if Value.Package <> Package then raise EPackageInfo.CreateFmt(SDisplayFontNotOwnedCorrectly, [Value]);
    FDisplayFont := Value;
    FDisplayFont.AddNotifyObject(FontRemoved);
  end;
end;

constructor TPackageKeyboard.Create(APackage: TPackage);
begin
  inherited Create(APackage);
  FLanguages := TPackageKeyboardLanguageList.Create(APackage);
  FExamples := TPackageKeyboardExampleList.Create(APackage);
  FWebOSKFonts := TPackageContentFileReferenceList.Create(APackage);
  FWebOSKFonts.OnNotify := FontListNotify;
  FWebDisplayFonts := TPackageContentFileReferenceList.Create(APackage);
  FWebDisplayFonts.OnNotify := FontListNotify;
end;

destructor TPackageKeyboard.Destroy;
begin
  if Assigned(FDisplayFont) then
    FDisplayFont.RemoveNotifyObject(FontRemoved);
  FDisplayFont := nil;

  if Assigned(FOSKFont) then
    FOSKFont.RemoveNotifyObject(FontRemoved);
  FOSKFont := nil;

  FreeAndNil(FLanguages);
  FreeAndNil(FExamples);
  FreeAndNil(FWebOSKFonts);
  FreeAndNil(FWebDisplayFonts);
  inherited Destroy;
end;

procedure TPackageKeyboard.FontListNotify(Sender: TObject;
  const Item: TPackageContentFile; Action: TCollectionNotification);
begin
  if Action = cnRemoved then
    Item.RemoveNotifyObject(FontRemoved)
  else if Action = cnAdded then
  begin
    Assert(Item.Package = Self.Package);
    Item.AddNotifyObject(FontRemoved);
  end;
end;

procedure TPackageKeyboard.FontRemoved(Sender: TObject;
  EventType: TPackageNotifyEventType; var FAllow: Boolean);
begin
  // For all EventTypes
  if Sender = FDisplayFont then
    FDisplayFont := nil;
  if Sender = FOSKFont then
    FOSKFont := nil;
  FWebDisplayFonts.Remove(Sender as TPackageContentFile);
  FWebOSKFonts.Remove(Sender as TPackageContentFile);
end;

procedure TPackageKeyboard.SetOSKFont(const Value: TPackageContentFile);
begin
  if Assigned(FOSKFont) then
    FOSKFont.RemoveNotifyObject(FontRemoved);
  if not Assigned(Value) then
    FOSKFont := nil
  else
  begin
    if Value.Package <> Package then raise EPackageInfo.CreateFmt(SOSKFontNotOwnedCorrectly, [Value]);
    FOSKFont := Value;
    FOSKFont.AddNotifyObject(FontRemoved);
  end;
end;

{ TPackageKeyboardList }

procedure TPackageKeyboardList.Assign(Source: TPackageKeyboardList);
var
  i: Integer;
  pk: TPackageKeyboard;
begin
  Clear;
  for i := 0 to Source.Count - 1 do
  begin
    pk := TPackageKeyboard.Create(Package);
    pk.Assign(Source[i]);
    Add(pk);
  end;
end;

function TPackageKeyboardList.ItemByID(id: string): TPackageKeyboard;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if SameText(id, Items[i].ID) then
      Exit(Items[i]);
  Result := nil;
end;

procedure TPackageKeyboardList.LoadIni(AIni: TIniFile);
var
  s: TStringList;
  ln: WideString;
  i, j: Integer;
  FSectionName: string;
  keyboard: TPackageKeyboard;
  FLanguage: TPackageKeyboardLanguage;
begin
  Clear;
  s := TStringList.Create;
  try
    AIni.ReadSections(s);
    for i := 0 to MaxInt do
    begin
      FSectionName := 'Keyboard'+IntToStr(i);
      if s.IndexOf(FSectionName) < 0 then
        Break;

      keyboard := TPackageKeyboard.Create(Package);
      keyboard.Name := AIni.ReadString(FSectionName, SXML_PackageKeyboard_Name, '');
      keyboard.ID := AIni.ReadString(FSectionName, SXML_PackageKeyboard_ID, '');
      keyboard.Version := AIni.ReadString(FSectionName, SXML_PackageKeyboard_Version, '1.0');
      keyboard.RTL := AIni.ReadBool(FSectionName, SXML_PackageKeyboard_RTL, False);
      keyboard.OSKFont := Package.Files.FromFileNameEx(AIni.ReadString(FSectionName, SXML_PackageKeyboard_OSKFont, ''));
      keyboard.DisplayFont := Package.Files.FromFileNameEx(AIni.ReadString(FSectionName, SXML_PackageKeyboard_DisplayFont, ''));

      for j := 0 to MaxInt do
      begin
        ln := AIni.ReadString(FSectionName, SXML_PackageKeyboard_Language+IntToStr(j), '');
        if ln = '' then
          Break;
        FLanguage := TPackageKeyboardLanguage.Create(Package);
        FLanguage.ID := CommaToken(ln);
        FLanguage.Name := CommaToken(ln);
        keyboard.Languages.Add(FLanguage);
      end;

      // web fonts not supported in ini

      Add(keyboard);
    end;
  finally
    s.Free;
  end;
end;

procedure TPackageKeyboardList.LoadJSON(ARoot: TJSONObject);
var
  keyboard: TPackageKeyboard;
  i: Integer;
  ASubNode, ANode: TJSONArray;
  AKeyboard: TJSONObject;
begin
  Clear;

  ANode := ARoot.Values[SJSON_Keyboards] as TJSONArray;
  if not Assigned(ANode) then
    Exit;

  for i := 0 to ANode.Count - 1 do
  begin
    AKeyboard := ANode.Items[i] as TJSONObject;

    keyboard := TPackageKeyboard.Create(Package);
    keyboard.Name := GetJsonValueString(AKeyboard, SJSON_Keyboard_Name);
    keyboard.ID := GetJsonValueString(AKeyboard,SJSON_Keyboard_ID);
    keyboard.Version := GetJsonValueString(AKeyboard, SJSON_Keyboard_Version);
    keyboard.RTL := GetJsonValueBool(AKeyboard, SJSON_Keyboard_RTL);
    keyboard.OSKFont := Package.Files.FromFileNameEx(GetJsonValueString(AKeyboard, SJSON_Keyboard_OSKFont));
    keyboard.DisplayFont := Package.Files.FromFileNameEx(GetJsonValueString(AKeyboard, SJSON_Keyboard_DisplayFont));
    keyboard.Languages.LoadJSON(AKeyboard);
    keyboard.Examples.LoadJSON(AKeyboard);

    ASubNode := AKeyboard.GetValue(SJSON_Keyboard_WebOSKFonts) as TJSONArray;
    if Assigned(ASubNode) then
      keyboard.WebOSKFonts.LoadJSON(ASubNode);

    ASubNode := AKeyboard.GetValue(SJSON_Keyboard_WebDisplayFonts) as TJSONArray;
    if Assigned(ASubNode) then
      keyboard.WebDisplayFonts.LoadJSON(ASubNode);

    Add(keyboard);
  end;
end;

procedure TPackageKeyboardList.LoadXML(ARoot: IXMLNode);
var
  keyboard: TPackageKeyboard;
  i: Integer;
  AKeyboard, ANode, ASubNode: IXMLNode;
begin
  Clear;

  ANode := ARoot.ChildNodes['Keyboards'];
  for i := 0 to ANode.ChildNodes.Count - 1 do
  begin
    AKeyboard := ANode.ChildNodes[i];

    keyboard := TPackageKeyboard.Create(Package);
    keyboard.Name := XmlVarToStr(AKeyboard.ChildValues[SXML_PackageKeyboard_Name]);
    keyboard.ID := XmlVarToStr(AKeyboard.ChildValues[SXML_PackageKeyboard_ID]);
    keyboard.Version := XmlVarToStr(AKeyboard.ChildValues[SXML_PackageKeyboard_Version]);
    keyboard.RTL := ANode.ChildNodes.IndexOf(SXML_PackageKeyboard_RTL) >= 0;
    keyboard.OSKFont := Package.Files.FromFileNameEx(XmlVarToStr(AKeyboard.ChildValues[SXML_PackageKeyboard_OSKFont]));
    keyboard.DisplayFont := Package.Files.FromFileNameEx(XmlVarToStr(AKeyboard.ChildValues[SXML_PackageKeyboard_DisplayFont]));

    keyboard.Languages.LoadXML(AKeyboard);
    keyboard.Examples.LoadXML(AKeyboard);

    ASubNode := AKeyboard.ChildNodes[SXML_PackageKeyboard_WebOSKFonts];
    if Assigned(ASubNode) then
    begin
      keyboard.WebOSKFonts.LoadXML(ASubNode);
    end;

    ASubNode := AKeyboard.ChildNodes[SXML_PackageKeyboard_WebDisplayFonts];
    if Assigned(ASubNode) then
    begin
      keyboard.WebDisplayFonts.LoadXML(ASubNode);
    end;

    Add(keyboard);
  end;
end;

procedure TPackageKeyboardList.SaveIni(AIni: TIniFile);
var
  i, j: Integer;
  FSectionName: string;
begin
  for i := 0 to Count-1 do
  begin
    FSectionName := 'Keyboard'+IntToStr(i);
    AIni.WriteString(FSectionName, SXML_PackageKeyboard_Name, Items[i].Name);
    AIni.WriteString(FSectionName, SXML_PackageKeyboard_ID, Items[i].ID);
    AIni.WriteString(FSectionName, SXML_PackageKeyboard_Version, Items[i].Version);
    if Items[i].RTL then AIni.WriteBool(FSectionName, SXML_PackageKeyboard_RTL, True);
    if Assigned(Items[i].OSKFont) then
      AIni.WriteString(FSectionName, SXML_PackageKeyboard_OSKFont, Items[i].OSKFont.RelativeFileName);
    if Assigned(Items[i].DisplayFont) then
      AIni.WriteString(FSectionName, SXML_PackageKeyboard_DisplayFont, Items[i].DisplayFont.RelativeFileName);

    for j := 0 to Items[i].Languages.Count-1 do
    begin
      AIni.WriteString(FSectionName, SXML_PackageKeyboard_Language+IntToStr(j), Items[i].Languages[j].ID+','+Items[i].Languages[j].Name);
    end;

    // web fonts not supported in ini
  end;
end;

procedure TPackageKeyboardList.SaveJSON(ARoot: TJSONObject);
var
  i: Integer;
  AKeyboard: TJSONObject;
  AFonts, AKeyboards: TJSONArray;
begin
  if Count = 0 then
    Exit;

  AKeyboards := TJSONArray.Create;
  ARoot.AddPair(SJSON_Keyboards, AKeyboards);

  for i := 0 to Count - 1 do
  begin
    AKeyboard := TJSONObject.Create;
    AKeyboards.Add(AKeyboard);

    AKeyboard.AddPair(SJSON_Keyboard_Name, Items[i].Name);
    AKeyboard.AddPair(SJSON_Keyboard_ID, Items[i].ID);
    AKeyboard.AddPair(SJSON_Keyboard_Version, Items[i].Version);
    if Items[i].RTL then AKeyboard.AddPair(SJSON_Keyboard_RTL, TJSONTrue.Create);
    if Assigned(Items[i].OSKFont) then
      AKeyboard.AddPair(SJSON_Keyboard_OSKFont, Items[i].OSKFont.RelativeFileName);
    if Assigned(Items[i].DisplayFont) then
      AKeyboard.AddPair(SJSON_Keyboard_DisplayFont, Items[i].DisplayFont.RelativeFileName);

    Items[i].Languages.SaveJSON(AKeyboard);
    Items[i].Examples.SaveJSON(AKeyboard);
    if Items[i].WebOSKFonts.Count > 0 then
    begin
      AFonts := TJSONArray.Create;
      Items[i].WebOSKFonts.SaveJSON(AFonts);
      AKeyboard.AddPair(SJSON_Keyboard_WebOSKFonts, AFonts);
    end;

    if Items[i].WebDisplayFonts.Count > 0 then
    begin
      AFonts := TJSONArray.Create;
      Items[i].WebDisplayFonts.SaveJSON(AFonts);
      AKeyboard.AddPair(SJSON_Keyboard_WebDisplayFonts, AFonts);
    end;
  end;
end;

procedure TPackageKeyboardList.SaveXML(ARoot: IXMLNode);
var
  i: Integer;
  AFonts, AKeyboard, ANode: IXMLNode;
begin
  ANode := ARoot.AddChild(SXML_PackageKeyboards);
  for i := 0 to Count - 1 do
  begin
    AKeyboard := ANode.AddChild(SXML_PackageKeyboard);

    AKeyboard.ChildNodes[SXML_PackageKeyboard_Name].NodeValue := Items[i].Name;
    AKeyboard.ChildNodes[SXML_PackageKeyboard_ID].NodeValue := Items[i].ID;
    AKeyboard.ChildNodes[SXML_PackageKeyboard_Version].NodeValue := Items[i].Version;
    if Items[i].RTL then
      AKeyboard.ChildNodes[SXML_PackageKeyboard_RTL].NodeValue := True;
    if Assigned(Items[i].OSKFont) then
      AKeyboard.ChildNodes[SXML_PackageKeyboard_OSKFont].NodeValue := Items[i].OSKFont.RelativeFileName;
    if Assigned(Items[i].DisplayFont) then
      AKeyboard.ChildNodes[SXML_PackageKeyboard_DisplayFont].NodeValue := Items[i].DisplayFont.RelativeFileName;

    Items[i].Languages.SaveXML(AKeyboard);
    Items[i].Examples.SaveXML(AKeyboard);
    if Items[i].WebOSKFonts.Count > 0 then
    begin
      AFonts := AKeyboard.AddChild(SXML_PackageKeyboard_WebOskFonts);
      Items[i].WebOSKFonts.SaveXML(AFonts);
    end;

    if Items[i].WebDisplayFonts.Count > 0 then
    begin
      AFonts := AKeyboard.AddChild(SXML_PackageKeyboard_WebDisplayFonts);
      Items[i].WebDisplayFonts.SaveXML(AFonts);
    end;
  end;
end;

function GetJsonValueString(o: TJSONObject; const n: string): string;
var
  v: TJSONValue;
begin
  v := o.Values[n];
  if not Assigned(v)
    then Result := ''
    else Result := Trim(v.Value);
end;

function GetJsonValueBool(o: TJSONObject; const n: string): Boolean;
begin
  Result := Assigned(o.Values[n]) and (o.Values[n] is TJSONTrue);
end;

{ TPackageLexicalModel }

procedure TPackageLexicalModel.Assign(Source: TPackageLexicalModel);
var
  i: Integer;
  FLanguage: TPackageKeyboardLanguage;
begin
  FName := Source.Name;
  FID := Source.ID;
  FRTL := Source.RTL;
  FLanguages.Clear;
  for i := 0 to Source.Languages.Count - 1 do
  begin
    FLanguage := TPackageKeyboardLanguage.Create(Package);
    FLanguage.ID := Source.Languages[i].ID;
    FLanguage.Name := Source.Languages[i].Name;
    FLanguages.Add(FLanguage);
  end;
end;

constructor TPackageLexicalModel.Create(APackage: TPackage);
begin
  inherited Create(APackage);
  FLanguages := TPackageKeyboardLanguageList.Create(APackage);
end;

destructor TPackageLexicalModel.Destroy;
begin
  FreeAndNil(FLanguages);
  inherited Destroy;
end;

{ TPackageLexicalModelList }

procedure TPackageLexicalModelList.Assign(Source: TPackageLexicalModelList);
var
  i: Integer;
  plm: TPackageLexicalModel;
begin
  Clear;
  for i := 0 to Source.Count - 1 do
  begin
    plm := TPackageLexicalModel.Create(Package);
    plm.Assign(Source[i]);
    Add(plm);
  end;
end;

function TPackageLexicalModelList.ItemByID(id: string): TPackageLexicalModel;
begin
  for Result in Self do
    if SameText(id, Result.ID) then
      Exit;
  Result := nil;
end;

procedure TPackageLexicalModelList.LoadJSON(ARoot: TJSONObject);
var
  lexicalModel: TPackageLexicalModel;
  i: Integer;
  ANode: TJSONArray;
  ALexicalModel: TJSONObject;
begin
  Clear;

  ANode := ARoot.Values[SJSON_LexicalModels] as TJSONArray;
  if not Assigned(ANode) then
    Exit;

  for i := 0 to ANode.Count - 1 do
  begin
    ALexicalModel := ANode.Items[i] as TJSONObject;

    lexicalModel := TPackageLexicalModel.Create(Package);
    lexicalModel.Name := GetJsonValueString(ALexicalModel, SJSON_LexicalModel_Name);
    lexicalModel.ID := GetJsonValueString(ALexicalModel,SJSON_LexicalModel_ID);
    lexicalModel.RTL := GetJsonValueBool(ALexicalModel, SJSON_LexicalModel_RTL);
    lexicalModel.Languages.LoadJSON(ALexicalModel);

    Add(lexicalModel);
  end;
end;

procedure TPackageLexicalModelList.LoadXML(ARoot: IXMLNode);
var
  lexicalModel: TPackageLexicalModel;
  i: Integer;
  ALexicalModel, ANode: IXMLNode;
begin
  Clear;

  ANode := ARoot.ChildNodes[SXML_PackageLexicalModels];
  for i := 0 to ANode.ChildNodes.Count - 1 do
  begin
    ALexicalModel := ANode.ChildNodes[i];

    lexicalModel := TPackageLexicalModel.Create(Package);
    lexicalModel.Name := XmlVarToStr(ALexicalModel.ChildValues[SXML_PackageLexicalModel_Name]);
    lexicalModel.ID := XmlVarToStr(ALexicalModel.ChildValues[SXML_PackageLexicalModel_ID]);
    lexicalModel.RTL := ALexicalModel.ChildNodes.IndexOf(SXML_PackageLexicalModel_RTL) >= 0;
    lexicalModel.Languages.LoadXML(ALexicalModel);
    Add(lexicalModel);
  end;
end;

procedure TPackageLexicalModelList.SaveJSON(ARoot: TJSONObject);
var
  i: Integer;
  ALexicalModel: TJSONObject;
  ALexicalModels: TJSONArray;
begin
  if Count = 0 then
    Exit;

  ALexicalModels := TJSONArray.Create;
  ARoot.AddPair(SJSON_LexicalModels, ALexicalModels);

  for i := 0 to Count - 1 do
  begin
    ALexicalModel := TJSONObject.Create;
    ALexicalModels.Add(ALexicalModel);

    ALexicalModel.AddPair(SJSON_LexicalModel_Name, Items[i].Name);
    ALexicalModel.AddPair(SJSON_LexicalModel_ID, Items[i].ID);
    if Items[i].RTL then ALexicalModel.AddPair(SJSON_LexicalModel_RTL, TJSONTrue.Create);
    Items[i].Languages.SaveJSON(ALexicalModel);
  end;
end;

procedure TPackageLexicalModelList.SaveXML(ARoot: IXMLNode);
var
  i: Integer;
  ALexicalModel, ANode: IXMLNode;
begin
  ANode := ARoot.AddChild(SXML_PackageLexicalModels);
  for i := 0 to Count - 1 do
  begin
    ALexicalModel := ANode.AddChild(SXML_PackageLexicalModel);

    ALexicalModel.ChildNodes[SXML_PackageLexicalModel_Name].NodeValue := Items[i].Name;
    ALexicalModel.ChildNodes[SXML_PackageLexicalModel_ID].NodeValue := Items[i].ID;
    if Items[i].RTL then
      ALexicalModel.ChildNodes[SXML_PackageLexicalModel_RTL].NodeValue := True;

    Items[i].Languages.SaveXML(ALexicalModel);
  end;
end;

{ TPackageKeyboardLanguageList }

function TPackageKeyboardLanguageList.ContainsID(const id: string): Boolean;
begin
  Result := IndexOfID(id) >= 0;
end;

function TPackageKeyboardLanguageList.IndexOfID(const id: string): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if SameText(Items[i].ID, id) then
      Exit(i);
  Result := -1;
end;

procedure TPackageKeyboardLanguageList.LoadJSON(ARoot: TJSONObject);
var
  j: Integer;
  ALanguage: TJSONObject;
  FLanguage: TPackageKeyboardLanguage;
  ALanguages: TJSONArray;
begin
  ALanguages := ARoot.Values[SJSON_Keyboard_Languages] as TJSONArray;
  if not Assigned(ALanguages) then
    Exit;

  for j := 0 to ALanguages.Count - 1 do
  begin
    ALanguage := ALanguages.Items[j] as TJSONObject;

    FLanguage := TPackageKeyboardLanguage.Create(Package);
    FLanguage.ID := GetJsonValueString(ALanguage, SJSON_Keyboard_Language_ID);
    FLanguage.Name := GetJsonValueString(ALanguage, SJSON_Keyboard_Language_Name);
    Self.Add(FLanguage);
  end;
end;

procedure TPackageKeyboardLanguageList.LoadXML(ARoot: IXMLNode);
var
  j: Integer;
  ALanguages, ALanguage: IXMLNode;
  FLanguage: TPackageKeyboardLanguage;
begin
  ALanguages := ARoot.ChildNodes[SXML_PackageKeyboard_Languages];
  if not Assigned(ALanguages) then
    Exit;

  for j := 0 to ALanguages.ChildNodes.Count - 1 do
  begin
    ALanguage := ALanguages.ChildNodes[j];

    FLanguage := TPackageKeyboardLanguage.Create(Package);
    FLanguage.ID := VarToStr(ALanguage.Attributes[SXML_PackageKeyboard_Language_ID]);
    FLanguage.Name := VarToStr(ALanguage.NodeValue);
    Self.Add(FLanguage);
  end;
end;

procedure TPackageKeyboardLanguageList.SaveJSON(ARoot: TJSONObject);
var
  ALanguages: TJSONArray;
  j: Integer;
  ALanguage: TJSONObject;
begin
  ALanguages := TJSONArray.Create;
  ARoot.AddPair(SJSON_Keyboard_Languages, ALanguages);
  for j := 0 to Count - 1 do
  begin
    ALanguage := TJSONObject.Create;
    ALanguages.Add(ALanguage);
    ALanguage.AddPair(SJSON_Keyboard_Language_Name, Items[j].Name);
    ALanguage.AddPair(SJSON_Keyboard_Language_ID, Items[j].ID);
  end;
end;

procedure TPackageKeyboardLanguageList.SaveXML(ARoot: IXMLNode);
var
  ALanguages: IXMLNode;
  j: Integer;
  ALanguage: IXMLNode;
begin
  ALanguages := ARoot.AddChild(SXML_PackageKeyboard_Languages);
  for j := 0 to Count - 1 do
  begin
    ALanguage := ALanguages.AddChild(SXML_PackageKeyboard_Language);
    ALanguage.NodeValue := Items[j].Name;
    ALanguage.Attributes[SXML_PackageKeyboard_Language_ID] := Items[j].ID;
  end;
end;

{ TPackageKeyboardExampleList }

function TPackageKeyboardExampleList.ContainsID(const id: string): Boolean;
begin
  Result := IndexOfID(id) >= 0;
end;

function TPackageKeyboardExampleList.IndexOfID(const id: string;
  from: Integer): Integer;
var
  i: Integer;
begin
  for i := from to Count - 1 do
    if SameText(Items[i].ID, id) then
      Exit(i);
  Result := -1;
end;

procedure TPackageKeyboardExampleList.LoadJSON(ARoot: TJSONObject);
var
  j: Integer;
  AExample: TJSONObject;
  FExample: TPackageKeyboardExample;
  AExamples: TJSONArray;
begin
  AExamples := ARoot.Values[SJSON_Keyboard_Examples] as TJSONArray;
  if not Assigned(AExamples) then
    Exit;

  for j := 0 to AExamples.Count - 1 do
  begin
    AExample := AExamples.Items[j] as TJSONObject;

    FExample := TPackageKeyboardExample.Create(Package);
    FExample.ID := GetJsonValueString(AExample, SJSON_Keyboard_Example_ID);
    FExample.Keys := GetJsonValueString(AExample, SJSON_Keyboard_Example_Keys);
    FExample.Text := GetJsonValueString(AExample, SJSON_Keyboard_Example_Text);
    FExample.Note := GetJsonValueString(AExample, SJSON_Keyboard_Example_Note);
    Self.Add(FExample);
  end;
end;

procedure TPackageKeyboardExampleList.LoadXML(ARoot: IXMLNode);
var
  j: Integer;
  AExamples, AExample: IXMLNode;
  FExample: TPackageKeyboardExample;
begin
  AExamples := ARoot.ChildNodes[SXML_PackageKeyboard_Examples];
  if not Assigned(AExamples) then
    Exit;

  for j := 0 to AExamples.ChildNodes.Count - 1 do
  begin
    AExample := AExamples.ChildNodes[j];

    FExample := TPackageKeyboardExample.Create(Package);
    FExample.ID := VarToStr(AExample.Attributes[SXML_PackageKeyboard_Example_ID]);
    FExample.Keys := VarToStr(AExample.Attributes[SXML_PackageKeyboard_Example_Keys]);
    FExample.Text := VarToStr(AExample.Attributes[SXML_PackageKeyboard_Example_Text]);
    FExample.Note := VarToStr(AExample.Attributes[SXML_PackageKeyboard_Example_Note]);
    Self.Add(FExample);
  end;
end;

procedure TPackageKeyboardExampleList.SaveJSON(ARoot: TJSONObject);
var
  AExamples: TJSONArray;
  j: Integer;
  AExample: TJSONObject;
begin
  AExamples := TJSONArray.Create;
  ARoot.AddPair(SJSON_Keyboard_Examples, AExamples);
  for j := 0 to Count - 1 do
  begin
    AExample := TJSONObject.Create;
    AExamples.Add(AExample);
    AExample.AddPair(SJSON_Keyboard_Example_ID, Items[j].ID);
    AExample.AddPair(SJSON_Keyboard_Example_Keys, Items[j].Keys);
    AExample.AddPair(SJSON_Keyboard_Example_Text, Items[j].Text);
    AExample.AddPair(SJSON_Keyboard_Example_Note, Items[j].Note);
  end;
end;

procedure TPackageKeyboardExampleList.SaveXML(ARoot: IXMLNode);
var
  AExamples: IXMLNode;
  j: Integer;
  AExample: IXMLNode;
begin
  if Count = 0 then
    Exit;

  AExamples := ARoot.AddChild(SXML_PackageKeyboard_Examples);
  for j := 0 to Count - 1 do
  begin
    AExample := AExamples.AddChild(SXML_PackageKeyboard_Example);
    AExample.Attributes[SXML_PackageKeyboard_Example_ID] := Items[j].ID;
    AExample.Attributes[SXML_PackageKeyboard_Example_Keys] := Items[j].Keys;
    AExample.Attributes[SXML_PackageKeyboard_Example_Text] := Items[j].Text;
    AExample.Attributes[SXML_PackageKeyboard_Example_Note] := Items[j].Note;
  end;
end;

{ TPackageRelatedPackage }

procedure TPackageRelatedPackage.Assign(Source: TPackageRelatedPackage);
begin
  FID := Source.ID;
  FRelationship := Source.Relationship;
end;

{ TPackageRelatedPackageList }

procedure TPackageRelatedPackageList.Assign(Source: TPackageRelatedPackageList);
var
  i: Integer;
  rp: TPackageRelatedPackage;
begin
  Clear;
  for i := 0 to Source.Count - 1 do
  begin
    rp := TPackageRelatedPackage.Create(Package);
    rp.Assign(Source[i]);
    Add(rp);
  end;
end;

procedure TPackageRelatedPackageList.LoadJSON(ARoot: TJSONObject);
var
  rp: TPackageRelatedPackage;
  i: Integer;
  ANode: TJSONArray;
  ARelatedPackage: TJSONObject;
begin
  Clear;

  ANode := ARoot.Values[SJSON_RelatedPackages] as TJSONArray;
  if not Assigned(ANode) then
    Exit;

  for i := 0 to ANode.Count - 1 do
  begin
    ARelatedPackage := ANode.Items[i] as TJSONObject;

    rp := TPackageRelatedPackage.Create(Package);
    rp.ID := GetJsonValueString(ARelatedPackage,SJSON_RelatedPackage_ID);
    rp.Relationship := GetJsonValueString(ARelatedPackage, SJSON_RelatedPackage_Relationship);
    if rp.Relationship <> 'deprecates' then
      rp.Relationship := '';

    Add(rp);
  end;
end;

procedure TPackageRelatedPackageList.LoadXML(ARoot: IXMLNode);
var
  rp: TPackageRelatedPackage;
  i: Integer;
  ARelatedPackage, ANode: IXMLNode;
begin
  Clear;

  ANode := ARoot.ChildNodes[SXML_PackageRelatedPackages];
  for i := 0 to ANode.ChildNodes.Count - 1 do
  begin
    ARelatedPackage := ANode.ChildNodes[i];

    rp := TPackageRelatedPackage.Create(Package);
    rp.ID := XmlVarToStr(ARelatedPackage.Attributes[SXML_PackageRelatedPackage_ID]);
    rp.Relationship := XmlVarToStr(ARelatedPackage.Attributes[SXML_PackageRelatedPackage_Relationship]);
    if rp.Relationship <> 'deprecates' then
      rp.Relationship := '';

    Add(rp);
  end;
end;

procedure TPackageRelatedPackageList.SaveJSON(ARoot: TJSONObject);
var
  i: Integer;
  ARelatedPackage: TJSONObject;
  ARelatedPackages: TJSONArray;
begin
  if Count = 0 then
    Exit;

  ARelatedPackages := TJSONArray.Create;
  ARoot.AddPair(SJSON_RelatedPackages, ARelatedPackages);

  for i := 0 to Count - 1 do
  begin
    ARelatedPackage := TJSONObject.Create;
    ARelatedPackages.Add(ARelatedPackage);

    ARelatedPackage.AddPair(SJSON_RelatedPackage_ID, Items[i].ID);
    // Relationship field required for kmp.json
    if Items[i].Relationship = ''
      then ARelatedPackage.AddPair(SJSON_RelatedPackage_Relationship, 'related')
      else ARelatedPackage.AddPair(SJSON_RelatedPackage_Relationship, Items[i].Relationship);
  end;
end;

procedure TPackageRelatedPackageList.SaveXML(ARoot: IXMLNode);
var
  i: Integer;
  ARelatedPackage, ANode: IXMLNode;
begin
  ANode := ARoot.AddChild(SXML_PackageRelatedPackages);
  for i := 0 to Count - 1 do
  begin
    ARelatedPackage := ANode.AddChild(SXML_PackageRelatedPackage);

    ARelatedPackage.Attributes[SXML_PackageRelatedPackage_ID] := Items[i].ID;
    // Relationship field optional for .kps
    if Items[i].Relationship <> '' then
      ARelatedPackage.Attributes[SXML_PackageRelatedPackage_Relationship] := Items[i].Relationship;
  end;
end;

{ TPackageContentFileReferenceList }

procedure TPackageContentFileReferenceList.Assign(Source: TPackageContentFileReferenceList);
var
  i: Integer;
  f: TPackageContentFile;
begin
  Clear;
  for i := 0 to Source.Count - 1 do
  begin
    f := Package.Files.FromFileNameEx(Source[i].FileName);
    if Assigned(f) then
      Add(f);
  end;
end;

constructor TPackageContentFileReferenceList.Create(APackage: TPackage);
begin
  inherited Create(APackage);
  OwnsObjects := False;
end;

function TPackageContentFileReferenceList.GetAsString: string;
var
  f: TPackageContentFile;
begin
  Result := '';
  for f in Self do
  begin
    Result := Result + ', ' + ExtractFileName(f.FileName);
  end;
  System.Delete(Result, 1, 2);
end;

procedure TPackageContentFileReferenceList.LoadJSON(ARoot: TJSONArray);
var
  i: Integer;
  f: TPackageContentFile;
begin
  for i := 0 to ARoot.Count - 1 do
  begin
    f := Package.Files.FromFileNameEx(ARoot.Items[i].Value);
    if Assigned(f) then
      Add(f);
  end;
end;

procedure TPackageContentFileReferenceList.LoadXML(ARoot: IXMLNode);
var
  i: Integer;
  ANode: IXMLNode;
  f: TPackageContentFile;
begin
  Clear;
  for i := 0 to ARoot.ChildNodes.Count - 1 do
  begin
    ANode := ARoot.ChildNodes[i];
    f := Package.Files.FromFileNameEx(ANode.Attributes[SXML_PackageKeyboardFont_Filename]);
    if Assigned(f) then
      Add(f);
  end;
end;

procedure TPackageContentFileReferenceList.SaveJSON(ARoot: TJSONArray);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    ARoot.Add(Items[i].FileName);
  end;
end;

procedure TPackageContentFileReferenceList.SaveXML(ARoot: IXMLNode);
var
  i: Integer;
  ANode: IXMLNode;
begin
  for i := 0 to Count - 1 do
  begin
    ANode := ARoot.AddChild(SXML_PackageKeyboardFont);
    ANode.Attributes[SXML_PackageKeyboardFont_Filename] := Items[i].FileName;
  end;
end;

end.

