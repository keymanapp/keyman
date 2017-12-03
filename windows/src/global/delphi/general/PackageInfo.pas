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
  System.Sysutils,
  Winapi.Windows,
  Xml.XMLDoc,
  Xml.XMLIntf,

  utilfiletypes,
  utilstr;

{ Package Information Classes }

function GetJsonValueString(o: TJSONObject; const n: string): string;

type
  TPackageInfoEntryType = (pietName, pietVersion, pietCopyright, pietAuthor, pietWebsite, pietOther);

const
  PackageInfo_Name      = 'Name';
  PackageInfo_Version   = 'Version';
  PackageInfo_Copyright = 'Copyright';
  PackageInfo_Author    = 'Author';
  PackageInfo_Website   = 'Website';

  PackageInfoEntryTypeNames: array[TPackageInfoEntryType] of WideString =
    (PackageInfo_Name, PackageInfo_Version, PackageInfo_Copyright, PackageInfo_Author, PackageInfo_Website, '');

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
  protected
    property Package: TPackage read FPackage;
  public
    constructor Create(APackage: TPackage); virtual;
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
  {TODO:   if Items[Index] = FPackage.Options.ReadmeFile then FPackage.Options.ReadmeFile := nil;}
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

  { I2002 - Package registry keys }

  TPackageRegistryKeyValueType = (rkvtString, rkvtDWord);

  TPackageRegistryKey = class(TPackageBaseObject)
  private
    FKey: WideString;
    FValueType: TPackageRegistryKeyValueType;
    FValue: WideString;
    FRoot: HKEY;
    FName: WideString;
  public  // I3311
    constructor Create(APackage: TPackage); override;
    destructor Destroy; override;
    procedure Assign(Source: TPackageRegistryKey);
    property Key: WideString read FKey write FKey;
    property Name: WideString read FName write FName;
    property Root: HKEY read FRoot write FRoot; { HKCU, HKLM, HKCR }
    property Value: WideString read FValue write FValue;
    property ValueType: TPackageRegistryKeyValueType read FValueType write FValueType;
  end;

  TPackageRegistryKeyList = class(TPackageObjectList<TPackageRegistryKey>)
  public
    procedure Assign(Source: TPackageRegistryKeyList); virtual;
    procedure LoadIni(AIni: TIniFile); virtual;
    procedure SaveIni(AIni: TIniFile); virtual;
    procedure LoadXML(ARoot: IXMLNode); virtual;
    procedure SaveXML(ARoot: IXMLNode); virtual;
    procedure LoadJSON(ARoot: TJSONObject); virtual;
    procedure SaveJSON(ARoot: TJSONObject); virtual;
  end;

  { Package Options }

  TPackageOptions = class(TPackageBaseObject)
  private
    FFileVersion: WideString;
    FExecuteProgram: WideString;
    FReadmeFile: TPackageContentFile;
    FGraphicFile: TPackageContentFile;
    FLoadLegacy: Boolean;
    FRegistryKeys: TPackageRegistryKeyList;
    procedure SetReadmeFile(const Value: TPackageContentFile);
    procedure SetExecuteProgram(Value: WideString);
    procedure SetFileVersion(Value: WideString);
    procedure SetGraphicFile(const Value: TPackageContentFile);
    procedure GraphicRemoved(Sender: TObject;
      EventType: TPackageNotifyEventType; var FAllow: Boolean);
    procedure ReadmeRemoved(Sender: TObject;
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
    property RegistryKeys: TPackageRegistryKeyList read FRegistryKeys;

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

  TPackageKeyboard = class(TPackageBaseObject)
  private
    FName: string;
    FOSKFont: TPackageContentFile;
    FID: string;
    FDisplayFont: TPackageContentFile;
    FLanguages: TPackageKeyboardLanguageList;
    FVersion: string;
    procedure SetDisplayFont(const Value: TPackageContentFile);
    procedure SetOSKFont(const Value: TPackageContentFile);
    procedure DisplayFontRemoved(Sender: TObject;
      EventType: TPackageNotifyEventType; var FAllow: Boolean);
    procedure OSKFontRemoved(Sender: TObject;
      EventType: TPackageNotifyEventType; var FAllow: Boolean);
  public
    constructor Create(APackage: TPackage); override;
    destructor Destroy; override;
    procedure Assign(Source: TPackageKeyboard); virtual;
    property Name: string read FName write FName;
    property ID: string read FID write FID;
    property Version: string read FVersion write FVersion;
    property Languages: TPackageKeyboardLanguageList read FLanguages;
    property OSKFont: TPackageContentFile read FOSKFont write SetOSKFont;
    property DisplayFont: TPackageContentFile read FDisplayFont write SetDisplayFont;
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

  TPackageKeyboardLanguageList = class(TPackageObjectList<TPackageKeyboardLanguage>);

  { TPackage }

  TPackage = class
  private
    FFileName: WideString;
    FWasIni: Boolean;
    FLoadLegacy: Boolean;
  protected
    procedure Import(AIni: TIniFile); virtual; abstract;
    function XMLRootNode: WideString; virtual;
    procedure DoLoadXML(ARoot: IXMLNode); virtual;
    procedure DoSaveXML(ARoot: IXMLNode); virtual;
    procedure DoLoadJSON(ARoot: TJSONObject); virtual;
    procedure DoSaveJSON(ARoot: TJSONObject); virtual;
  public
    Options: TPackageOptions;
    StartMenu: TPackageStartMenu;
    Files: TPackageContentFileList;
    Info: TPackageInfoEntryList;
    Keyboards: TPackageKeyboardList;

    property FileName: WideString read FFileName write FFileName;
    procedure Assign(Source: TPackage); virtual;
    constructor Create;
    destructor Destroy; override;
    procedure LoadIni; virtual;
    procedure SaveIni; virtual;

    procedure LoadXML; //virtual;
    procedure SaveXML; //virtual;

    procedure LoadJSON;
    procedure SaveJSON;

    function SaveXMLToText: WideString;
    procedure LoadXMLFromText(Text: WideString);

    property LoadLegacy: Boolean read FLoadLegacy write FLoadLegacy;

    property WasIni: Boolean read FWasIni;
  end;

const
  PackageStartMenuEntryLocationName: array[TPackageStartMenuEntryLocation] of WideString = ('Start Menu', 'Desktop'); //, 'Quick Launch Toolbar');

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
  SFileNotOwnedCorrectly = 'The file ''%s'' referred to is not part of the package.';
  SDisplayFontNotOwnedCorrectly = 'The display font file ''%s'' referred to is not part of the package.';
  SOSKFontNotOwnedCorrectly = 'The OSK font file ''%s'' referred to is not part of the package.';




const
  SXML_PackageKeyboards = 'Keyboards';
  SXML_PackageKeyboard_Name = 'Name';
  SXML_PackageKeyboard_ID = 'ID';
  SXML_PackageKeyboard_Version = 'Version';
  SXML_PackageKeyboard_OSKFont = 'OSKFont';
  SXML_PackageKeyboard_DisplayFont = 'DisplayFont';
  SXML_PackageKeyboard_Languages = 'Languages';

  SXML_PackageKeyboard_Language = 'Language';
  SXML_PackageKeyboard_Language_ID = 'ID';
  SXML_PackageKeyboard_Language_Name = 'Name';

const
  SJSON_System = 'system';
  SJSON_System_KeymanDeveloperVersion = 'keymanDeveloperVersion';
  SJSON_System_FileVersion = 'fileVersion';

  SJSON_Options = 'options';
  SJSON_Options_ExecuteProgram = 'executeProgram';
  SJSON_Options_ReadMeFile = 'readmeFile';
  SJSON_Options_GraphicFile = 'graphicFile';

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

  SJSON_Info_Website = 'website';
  SJSON_Info_Version = 'version';
  SJSON_Info_Name = 'name';
  SJSON_Info_Copyright = 'copyright';
  SJSON_Info_Author = 'author';

  SJSON_PackageInfoEntryTypeNames: array[TPackageInfoEntryType] of string =
    (SJSON_Info_Name, SJSON_Info_Version, SJSON_Info_Copyright, SJSON_Info_Author, SJSON_Info_Website, '');

  SJSON_Files = 'files';
  SJSON_Files_Name = 'name';
  SJSON_Files_Description = 'description';
  SJSON_Files_CopyLocation = 'copyLocation';

  SJSON_Keyboards = 'keyboards';
  SJSON_Keyboard_Name = 'name';
  SJSON_Keyboard_ID = 'id';
  SJSON_Keyboard_Version = 'version';
  SJSON_Keyboard_OSKFont = 'oskFont';
  SJSON_Keyboard_DisplayFont = 'displayFont';

  SJSON_Keyboard_Languages = 'languages';
  SJSON_Keyboard_Language_ID = 'id';
  SJSON_Keyboard_Language_Name = 'name';

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
  FRegistryKeys.Assign(Source.RegistryKeys);
end;

constructor TPackageOptions.Create(APackage: TPackage);
begin
  inherited Create(APackage);
  FLoadLegacy := True;
  FRegistryKeys := TPackageRegistryKeyList.Create(Package);
  FFileVersion := SKeymanVersion70;
end;

destructor TPackageOptions.Destroy;
begin
  ReadmeFile := nil;
  GraphicFile := nil;
  FRegistryKeys.Free;
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

procedure TPackageOptions.LoadXML(ARoot: IXMLNode);
begin
  FileVersion :=                VarToWideStr(ARoot.ChildNodes['System'].ChildNodes['FileVersion'].NodeValue);
  ExecuteProgram :=             VarToWideStr(ARoot.ChildNodes['Options'].ChildNodes['ExecuteProgram'].NodeValue);
  ReadmeFile :=                 Package.Files.FromFileName(VarToWideStr(ARoot.ChildNodes['Options'].ChildNodes['ReadMeFile'].NodeValue));
  GraphicFile :=                Package.Files.FromFileName(VarToWideStr(ARoot.ChildNodes['Options'].ChildNodes['GraphicFile'].NodeValue));
  if Assigned(ReadmeFile) then ReadmeFile.AddNotifyObject(ReadmeRemoved);
  if Assigned(GraphicFile) then GraphicFile.AddNotifyObject(GraphicRemoved);
  FRegistryKeys.LoadXML(ARoot);
end;

procedure TPackageOptions.SaveXML(ARoot: IXMLNode);
begin
  ARoot.ChildNodes['System'].ChildNodes['FileVersion'].NodeValue := FileVersion;
  ARoot.ChildNodes['Options'].ChildNodes['ExecuteProgram'].NodeValue := ExecuteProgram;
  if Assigned(ReadmeFile) then
    ARoot.ChildNodes['Options'].ChildNodes['ReadMeFile'].NodeValue := ReadmeFile.RelativeFileName;
  if Assigned(GraphicFile) then
    ARoot.ChildNodes['Options'].ChildNodes['GraphicFile'].NodeValue := GraphicFile.RelativeFileName;
  FRegistryKeys.SaveXML(ARoot);
end;

procedure TPackageOptions.LoadIni(AIni: TIniFile);
begin
  FileVersion :=                AIni.ReadString('Package', 'Version',                  '');
  ExecuteProgram :=             AIni.ReadString('Package', 'ExecuteProgram',           '');
  ReadmeFile :=                 Package.Files.FromFileName(AIni.ReadString('Package', 'ReadMeFile', ''));
  GraphicFile :=                Package.Files.FromFileName(AIni.ReadString('Package', 'GraphicFile', ''));
  if Assigned(ReadmeFile) then ReadmeFile.AddNotifyObject(ReadmeRemoved);
  if Assigned(GraphicFile) then GraphicFile.AddNotifyObject(GraphicRemoved);
  FRegistryKeys.LoadIni(AIni);
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
  if Assigned(ReadmeFile) then ReadmeFile.AddNotifyObject(ReadmeRemoved);
  if Assigned(GraphicFile) then GraphicFile.AddNotifyObject(GraphicRemoved);
  FRegistryKeys.LoadJSON(ARoot);
end;

procedure TPackageOptions.SaveIni(AIni: TIniFile);
begin
  AIni.WriteString('Package', 'Version',                  FileVersion);
  AIni.WriteString('Package', 'ExecuteProgram',           ExecuteProgram);
  if Assigned(ReadmeFile) then
    AIni.WriteString('Package', 'ReadMeFile', ReadmeFile.RelativeFileName);
  if Assigned(GraphicFile) then
    AIni.WriteString('Package', 'GraphicFile', GraphicFile.RelativeFileName);
  FRegistryKeys.SaveIni(AIni);
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
  FRegistryKeys.SaveJSON(ARoot);
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
    inf.Description := VarToWideStr(ANode.NodeValue);
    inf.URL := VarToWideStr(ANode.Attributes['URL']);
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
      sme.Name := VarToWideStr(ChildNodes['Name'].NodeValue);
      sme.Prog := VarToWideStr(ChildNodes['FileName'].NodeValue);
      sme.Params := VarToWideStr(ChildNodes['Arguments'].NodeValue);
      sme.Icon := VarToWideStr(ChildNodes['Icon'].NodeValue);
      sme.Location := StrToStartMenuEntryLocation(VarToWideStr(ChildNodes['Location'].NodeValue));
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
    AIni.WriteString('StartMenuEntries', Items[i].Name, Format('"%s","%s"', [Items[i].Prog, Items[i].Params]));
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
  Path := VarToWideStr(ANode.ChildNodes['Folder'].NodeValue);
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
  AddUninstallEntry := Assigned(ANode.Values[SJSON_StartMenu_AddUninstallEntry]) and
    (ANode.Values[SJSON_StartMenu_AddUninstallEntry] is TJSONTrue);
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
      subfile.FileName := VarToWideStr(ChildNodes['Name'].NodeValue);
      subfile.Description := VarToWideStr(ChildNodes['Description'].NodeValue);
      subfile.FCopyLocation := TPackageFileCopyLocation(StrToIntDef(VarToWideStr(ChildNodes['Location'].NodeValue), 0));
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
end;

constructor TPackage.Create;
begin
  inherited Create;
  if not Assigned(Files)     then Files     := TPackageContentFileList.Create(Self);
  if not Assigned(Options)   then Options   := TPackageOptions.Create(Self);
  if not Assigned(StartMenu) then StartMenu := TPackageStartMenu.Create(Self);
  if not Assigned(Info)      then Info      := TPackageInfoEntryList.Create(Self);
  if not Assigned(Keyboards) then Keyboards := TPackageKeyboardList.Create(Self);

end;

destructor TPackage.Destroy;
begin
  Files.Free;
  Options.Free;
  StartMenu.Free;
  Info.Free;
  Keyboards.Free;
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

    StartMenu.LoadIni(ini);
    Info.LoadIni(ini);
    Files.LoadIni(ini);
    Options.LoadLegacy := FLoadLegacy;
    Options.LoadIni(ini);
    Keyboards.LoadIni(ini);
  finally
    ini.Free;
  end;
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
end;

procedure TPackage.DoLoadXML(ARoot: IXMLNode);
var
  FVersion: WideString;
begin
  FVersion := VarToWideStr(ARoot.ChildNodes['System'].ChildNodes['FileVersion'].NodeValue);
  if FVersion <> SKeymanVersion70 then
    PackageLoadError('Package file version '+FVersion+' is not recognised.');

  StartMenu.LoadXML(ARoot);
  Info.LoadXML(ARoot);
  Files.LoadXML(ARoot);
  Options.LoadXML(ARoot);
  Keyboards.LoadXML(ARoot);
end;

procedure TPackage.DoSaveJSON(ARoot: TJSONObject);
begin
  Options.SaveJSON(ARoot);
  StartMenu.SaveJSON(ARoot);
  Info.SaveJSON(ARoot);
  Files.SaveJSON(ARoot);
  Keyboards.SaveJSON(ARoot);
end;

procedure TPackage.DoSaveXML(ARoot: IXMLNode);
begin
  ARoot.ChildNodes['System'].ChildNodes['KeymanDeveloperVersion'].NodeValue := GetVersionString;
  Options.SaveXML(ARoot);
  StartMenu.SaveXML(ARoot);
  Info.SaveXML(ARoot);
  Files.SaveXML(ARoot);
  Keyboards.SaveXML(ARoot);
end;

procedure TPackage.SaveIni;
var
  ini: TIniFile;
begin
  if FileExists(FileName) then DeleteFileCleanAttr(FileName);   // I4574

  ini := TIniFile.Create(FileName);
  try
    Options.SaveIni(ini);
    StartMenu.SaveIni(ini);
    Info.SaveIni(ini);
    Files.SaveIni(ini);
    Keyboards.SaveIni(ini);
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

{ TPackageRegistryKeyList }

procedure TPackageRegistryKeyList.Assign(Source: TPackageRegistryKeyList);
var
  i: Integer;
  prk: TPackageRegistryKey;
begin
  Clear;
  for i := 0 to Source.Count - 1 do
  begin
    prk := TPackageRegistryKey.Create(Package);
    prk.Assign(Source[i]);
    Add(prk);
  end;
end;

procedure TPackageRegistryKeyList.LoadIni(AIni: TIniFile);
var
  i: Integer;
  pkr: TPackageRegistryKey;
  FValues: TStringList;
  name, value: string;
  root: string;
  key: string;
  valuetype: string;
begin
  Clear;

  FValues := TStringList.Create;
  try
    AIni.ReadSectionValues('Registry', FValues);
    for i := 0 to FValues.Count - 1 do
    begin
      pkr := TPackageRegistryKey.Create(Package);

      name := FValues.Names[i];
      value := FValues.ValueFromIndex[i];
      root := UpperCase(StrToken(name, ';'));
      key := StrToken(name, ';');
      if root = 'HKCU' then pkr.Root := HKEY_CURRENT_USER
      else if root = 'HKLM' then pkr.Root := HKEY_LOCAL_MACHINE
      else if root = 'HKCR' then pkr.Root := HKEY_CLASSES_ROOT
      else pkr.Root := HKEY_CURRENT_USER;

      pkr.Name := name;
      pkr.Key := key;

      valuetype := UpperCase(StrToken(value, ';'));
      if valuetype = 'D'
        then pkr.ValueType := rkvtDWord
        else pkr.ValueType := rkvtString;

      pkr.Value := value;

      Add(pkr);
    end;
  finally
    FValues.Free;
  end;
end;

procedure TPackageRegistryKeyList.LoadJSON(ARoot: TJSONObject);
var
  i: Integer;
  ANode: TJSONArray;
  AKeyNode: TJSONObject;
  pkr: TPackageRegistryKey;
  name, value, root, key, valuetype: WideString;
begin
  Clear;

  ANode := ARoot.Values[SJSON_Registry] as TJSONArray;
  if not Assigned(ANode) then Exit;

  for i := 0 to ANode.Count - 1 do
  begin
    AKeyNode := ANode.Items[i] as TJSONObject;
    pkr := TPackageRegistryKey.Create(Package);

    name := GetJsonValueString(AKeyNode, SJSON_Registry_Name);
    value := GetJsonValueString(AKeyNode, SJSON_Registry_Value);
    root := GetJsonValueString(AKeyNode, SJSON_Registry_Root);
    key := GetJsonValueString(AKeyNode, SJSON_Registry_Key);

    if root = 'HKCU' then pkr.Root := HKEY_CURRENT_USER
    else if root = 'HKLM' then pkr.Root := HKEY_LOCAL_MACHINE
    else if root = 'HKCR' then pkr.Root := HKEY_CLASSES_ROOT
    else pkr.Root := HKEY_CURRENT_USER;

    pkr.Name := name;
    pkr.Key := key;

    valuetype := GetJsonValueString(AKeyNode, SJSON_Registry_ValueType);
    if valuetype = 'D'
      then pkr.ValueType := rkvtDWord
      else pkr.ValueType := rkvtString;

    pkr.Value := value;

    Add(pkr);
  end;
end;

procedure TPackageRegistryKeyList.SaveIni(AIni: TIniFile);
var
  pkr: TPackageRegistryKey;
  i: Integer;
  valuetype, root: string;
begin
  for i := 0 to Count - 1 do
  begin
    pkr := Items[i];
    if pkr.Root = HKEY_CURRENT_USER then root := 'HKCU'
    else if pkr.Root = HKEY_LOCAL_MACHINE then root := 'HKLM'
    else if pkr.Root = HKEY_CLASSES_ROOT then root := 'HKCR'
    else root := 'HKCU';

    if pkr.ValueType = rkvtString then valuetype := 'S' else valuetype := 'D';


    AIni.WriteString('Registry', root+';'+pkr.Key+';'+pkr.Name, valuetype+';'+pkr.Value);
  end;
end;

procedure TPackageRegistryKeyList.SaveJSON(ARoot: TJSONObject);
var
  i: Integer;
  AKeyNode: TJSONObject;
  ANode: TJSONArray;
  pkr: TPackageRegistryKey;
  root, valuetype: WideString;
begin
  if Count = 0 then
    Exit;

  ANode := TJSONArray.Create;
  ARoot.AddPair(SJSON_Registry, ANode);
  for i := 0 to Count - 1 do
  begin
    pkr := Items[i];
    if pkr.Root = HKEY_CURRENT_USER then root := 'HKCU'
    else if pkr.Root = HKEY_LOCAL_MACHINE then root := 'HKLM'
    else if pkr.Root = HKEY_CLASSES_ROOT then root := 'HKCR'
    else root := 'HKCU';

    if pkr.ValueType = rkvtString then valuetype := 'S' else valuetype := 'D';
    AKeyNode := TJSONObject.Create;
    ANode.Add(AKeyNode);
    AKeyNode.AddPair(SJSON_Registry_Root, root);
    AKeyNode.AddPair(SJSON_Registry_Key, pkr.Key);
    AKeyNode.AddPair(SJSON_Registry_Name, pkr.Name);
    AKeyNode.AddPair(SJSON_Registry_ValueType, valuetype);
    AKeyNode.AddPair(SJSON_Registry_Value, pkr.Value);
  end;
end;

procedure TPackageRegistryKeyList.LoadXML(ARoot: IXMLNode);
var
  i: Integer;
  ANode: IXMLNode;
  AKeyNode: IXMLNode;
  pkr: TPackageRegistryKey;
  name, value, root, key, valuetype: WideString;
begin
  Clear;

  ANode := ARoot.ChildNodes.FindNode('Registry');
  if not Assigned(ANode) then Exit;

  for i := 0 to ANode.ChildNodes.Count - 1 do
  begin
    AKeyNode := ANode.ChildNodes[i];
    pkr := TPackageRegistryKey.Create(Package);

    name := VarToWideStr(AKeyNode.Attributes['Name']);
    value := VarToWideStr(AKeyNode.Attributes['Value']);
    root := VarToWideStr(AKeyNode.Attributes['Root']);
    key := VarToWideStr(AKeyNode.Attributes['Key']);
    if root = 'HKCU' then pkr.Root := HKEY_CURRENT_USER
    else if root = 'HKLM' then pkr.Root := HKEY_LOCAL_MACHINE
    else if root = 'HKCR' then pkr.Root := HKEY_CLASSES_ROOT
    else pkr.Root := HKEY_CURRENT_USER;

    pkr.Name := name;
    pkr.Key := key;

    valuetype := VarToWideStr(AKeyNode.Attributes['ValueType']);
    if valuetype = 'D'
      then pkr.ValueType := rkvtDWord
      else pkr.ValueType := rkvtString;

    pkr.Value := value;

    Add(pkr);
  end;
end;


procedure TPackageRegistryKeyList.SaveXML(ARoot: IXMLNode);
var
  i: Integer;
  AKeyNode, ANode: IXMLNode;
  pkr: TPackageRegistryKey;
  root, valuetype: WideString;
begin
  ANode := ARoot.ChildNodes['Registry'];
  for i := 0 to Count - 1 do
  begin
    pkr := Items[i];
    if pkr.Root = HKEY_CURRENT_USER then root := 'HKCU'
    else if pkr.Root = HKEY_LOCAL_MACHINE then root := 'HKLM'
    else if pkr.Root = HKEY_CLASSES_ROOT then root := 'HKCR'
    else root := 'HKCU';

    if pkr.ValueType = rkvtString then valuetype := 'S' else valuetype := 'D';
    AKeyNode := ANode.AddChild('RegistryKey');
    AKeyNode.Attributes['Root'] := root;
    AKeyNode.Attributes['Key'] := pkr.Key;
    AKeyNode.Attributes['Name'] := pkr.Name;
    AKeyNode.Attributes['ValueType'] := valuetype;
    AKeyNode.Attributes['Value'] := pkr.Value;
  end;
end;

{ TPackageRegistryKey }

procedure TPackageRegistryKey.Assign(Source: TPackageRegistryKey);
begin
  FKey := Source.FKey;
  FValueType := Source.FValueType;
  FValue := Source.FValue;
  FRoot := Source.FRoot;
  FName := Source.FName;
end;

constructor TPackageRegistryKey.Create(APackage: TPackage);
begin
  inherited Create(APackage);
  FRoot := HKEY_CURRENT_USER;
  FValueType := rkvtString;
end;

destructor TPackageRegistryKey.Destroy;
begin
  inherited Destroy;
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
begin
  FName := Source.Name;
  FID := Source.ID;
  FVersion := Source.Version;
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
end;

procedure TPackageKeyboard.SetDisplayFont(const Value: TPackageContentFile);
begin
  if Assigned(FDisplayFont) then FDisplayFont.RemoveNotifyObject(DisplayFontRemoved);
  if not Assigned(Value) then
    FDisplayFont := nil
  else
  begin
    if Value.Package <> Package then raise EPackageInfo.CreateFmt(SDisplayFontNotOwnedCorrectly, [Value]);
    FDisplayFont := Value;
    FDisplayFont.AddNotifyObject(DisplayFontRemoved);
  end;
end;

constructor TPackageKeyboard.Create(APackage: TPackage);
begin
  inherited Create(APackage);
  FLanguages := TPackageKeyboardLanguageList.Create(APackage);
end;

destructor TPackageKeyboard.Destroy;
begin
  FreeAndNil(FLanguages);
  inherited Destroy;
end;

procedure TPackageKeyboard.DisplayFontRemoved(Sender: TObject;
  EventType: TPackageNotifyEventType; var FAllow: Boolean);
begin
  FDisplayFont := nil;
end;

procedure TPackageKeyboard.SetOSKFont(const Value: TPackageContentFile);
begin
  if Assigned(FOSKFont) then FOSKFont.RemoveNotifyObject(OSKFontRemoved);
  if not Assigned(Value) then
    FOSKFont := nil
  else
  begin
    if Value.Package <> Package then raise EPackageInfo.CreateFmt(SOSKFontNotOwnedCorrectly, [Value]);
    FOSKFont := Value;
    FOSKFont.AddNotifyObject(OSKFontRemoved);
  end;
end;

procedure TPackageKeyboard.OSKFontRemoved(Sender: TObject;
  EventType: TPackageNotifyEventType; var FAllow: Boolean);
begin
  FOSKFont := nil;
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

      Add(keyboard);
    end;
  finally
    s.Free;
  end;
end;

procedure TPackageKeyboardList.LoadJSON(ARoot: TJSONObject);
var
  keyboard: TPackageKeyboard;
  i, j: Integer;
  ALanguages, ANode: TJSONArray;
  AKeyboard, ALanguage: TJSONObject;
  FLanguage: TPackageKeyboardLanguage;
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
    keyboard.OSKFont := Package.Files.FromFileNameEx(GetJsonValueString(AKeyboard, SJSON_Keyboard_OSKFont));
    keyboard.DisplayFont := Package.Files.FromFileNameEx(GetJsonValueString(AKeyboard, SJSON_Keyboard_DisplayFont));

    ALanguages := AKeyboard.Values[SJSON_Keyboard_Languages] as TJSONArray;
    if Assigned(ALanguages) then
    begin
      for j := 0 to ALanguages.Count - 1 do
      begin
        ALanguage := ALanguages.Items[j] as TJSONObject;

        FLanguage := TPackageKeyboardLanguage.Create(Package);
        FLanguage.ID := GetJsonValueString(ALanguage, SJSON_Keyboard_Language_ID);
        FLanguage.Name := GetJsonValueString(ALanguage, SJSON_Keyboard_Language_Name);
        keyboard.Languages.Add(FLanguage);
      end;
    end;

    Add(keyboard);
  end;
end;

procedure TPackageKeyboardList.LoadXML(ARoot: IXMLNode);
var
  keyboard: TPackageKeyboard;
  i, j: Integer;
  AKeyboard, ALanguage, ALanguages, ANode: IXMLNode;
  FLanguage: TPackageKeyboardLanguage;
begin
  Clear;

  ANode := ARoot.ChildNodes['Keyboards'];
  for i := 0 to ANode.ChildNodes.Count - 1 do
  begin
    AKeyboard := ANode.ChildNodes[i];

    keyboard := TPackageKeyboard.Create(Package);
    keyboard.Name := VarToStr(AKeyboard.ChildValues[SXML_PackageKeyboard_Name]);
    keyboard.ID := VarToStr(AKeyboard.ChildValues[SXML_PackageKeyboard_ID]);
    keyboard.Version := VarToStr(AKeyboard.ChildValues[SXML_PackageKeyboard_Version]);
    keyboard.OSKFont := Package.Files.FromFileNameEx(VarToStr(AKeyboard.ChildValues[SXML_PackageKeyboard_OSKFont]));
    keyboard.DisplayFont := Package.Files.FromFileNameEx(VarToStr(AKeyboard.ChildValues[SXML_PackageKeyboard_DisplayFont]));

    ALanguages := AKeyboard.ChildNodes['Languages'];
    if Assigned(ALanguages) then
    begin
      for j := 0 to ALanguages.ChildNodes.Count - 1 do
      begin
        ALanguage := ALanguages.ChildNodes[j];

        FLanguage := TPackageKeyboardLanguage.Create(Package);
        FLanguage.ID := ALanguage.Attributes[SXML_PackageKeyboard_Language_ID];
        FLanguage.Name := ALanguage.NodeValue;
        keyboard.Languages.Add(FLanguage);
      end;
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
    if Assigned(Items[i].OSKFont) then
      AIni.WriteString(FSectionName, SXML_PackageKeyboard_OSKFont, Items[i].OSKFont.RelativeFileName);
    if Assigned(Items[i].DisplayFont) then
      AIni.WriteString(FSectionName, SXML_PackageKeyboard_DisplayFont, Items[i].DisplayFont.RelativeFileName);

    for j := 0 to Items[i].Languages.Count-1 do
    begin
      AIni.WriteString(FSectionName, SXML_PackageKeyboard_Language+IntToStr(j), Items[i].Languages[j].ID+','+Items[i].Languages[j].Name);
    end;
  end;
end;

procedure TPackageKeyboardList.SaveJSON(ARoot: TJSONObject);
var
  i, j: Integer;
  AKeyboard, ALanguage: TJSONObject;
  AKeyboards, ALanguages: TJSONArray;
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
    if Assigned(Items[i].OSKFont) then
      AKeyboard.AddPair(SJSON_Keyboard_OSKFont, Items[i].OSKFont.RelativeFileName);
    if Assigned(Items[i].DisplayFont) then
      AKeyboard.AddPair(SJSON_Keyboard_DisplayFont, Items[i].DisplayFont.RelativeFileName);

    ALanguages := TJSONArray.Create;
    AKeyboard.AddPair(SJSON_Keyboard_Languages, ALanguages);
    for j := 0 to Items[i].Languages.Count - 1 do
    begin
      ALanguage := TJSONObject.Create;
      ALanguages.Add(ALanguage);
      ALanguage.AddPair(SJSON_Keyboard_Language_Name, Items[i].Languages[j].Name);
      ALanguage.AddPair(SJSON_Keyboard_Language_ID, Items[i].Languages[j].ID);
    end;
  end;
end;

procedure TPackageKeyboardList.SaveXML(ARoot: IXMLNode);
var
  i, j: Integer;
  AKeyboard, ANode: IXMLNode;
  ALanguage, ALanguages: IXMLNode;
begin
  ANode := ARoot.AddChild('Keyboards');
  for i := 0 to Count - 1 do
  begin
    AKeyboard := ANode.AddChild('Keyboard');

    AKeyboard.ChildNodes[SXML_PackageKeyboard_Name].NodeValue := Items[i].Name;
    AKeyboard.ChildNodes[SXML_PackageKeyboard_ID].NodeValue := Items[i].ID;
    AKeyboard.ChildNodes[SXML_PackageKeyboard_Version].NodeValue := Items[i].Version;
    if Assigned(Items[i].OSKFont) then
      AKeyboard.ChildNodes[SXML_PackageKeyboard_OSKFont].NodeValue := Items[i].OSKFont.RelativeFileName;
    if Assigned(Items[i].DisplayFont) then
      AKeyboard.ChildNodes[SXML_PackageKeyboard_DisplayFont].NodeValue := Items[i].DisplayFont.RelativeFileName;
    ALanguages := AKeyboard.AddChild('Languages');
    for j := 0 to Items[i].Languages.Count - 1 do
    begin
      ALanguage := ALanguages.AddChild('Language');
      ALanguage.NodeValue := Items[i].Languages[j].Name;
      ALanguage.Attributes[SXML_PackageKeyboard_Language_ID] := Items[i].Languages[j].ID;
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
    else Result := v.Value;
end;

end.

