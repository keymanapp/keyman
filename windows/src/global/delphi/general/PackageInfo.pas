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
  System.Sysutils,
  Vcl.Graphics,
  Winapi.Windows,
  Xml.XMLDoc,
  Xml.XMLIntf,

  utilfiletypes,
  utilstr;

{ Package Information Classes }

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

  TPackageBaseObject = class
  private
    FNotifyObjects: TObjectList<TPackageNotifyEventWrapper>;
    FTag: Integer;
    function Notify(EventType: TPackageNotifyEventType): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNotifyObject(FEventHandler: TPackageNotifyEvent);
    procedure RemoveNotifyObject(FEventHandler: TPackageNotifyEvent);
  {TODO:   if Items[Index] = FPackage.Options.ReadmeFile then FPackage.Options.ReadmeFile := nil;}
    property Tag: Integer read FTag write FTag;
  end;

  TPackage = class;
  TPackageContentFile = class;
  TPackageContentFileList = class;

  { I2002 - Package registry keys }

  TPackageRegistryKeyValueType = (rkvtString, rkvtDWord);

  TPackageRegistryKey = class
  private
    FPackage: TPackage;
    FKey: WideString;
    FValueType: TPackageRegistryKeyValueType;
    FValue: WideString;
    FRoot: HKEY;
    FName: WideString;
  public  // I3311
    constructor Create(APackage: TPackage);
    destructor Destroy; override;
    procedure Assign(Source: TPackageRegistryKey);
    property Key: WideString read FKey write FKey;
    property Name: WideString read FName write FName;
    property Root: HKEY read FRoot write FRoot; { HKCU, HKLM, HKCR }
    property Value: WideString read FValue write FValue;
    property ValueType: TPackageRegistryKeyValueType read FValueType write FValueType;
  end;

  TPackageRegistryKeyList = class(TObjectList<TPackageRegistryKey>)
  private
    FPackage: TPackage;
  public
    constructor Create(APackage: TPackage);
    procedure Assign(Source: TPackageRegistryKeyList); virtual;
    procedure LoadIni(AIni: TIniFile); virtual;
    procedure SaveIni(AIni: TIniFile); virtual;
    procedure LoadXML(ARoot: IXMLNode); virtual;
    procedure SaveXML(ARoot: IXMLNode); virtual;
  end;

  { Package Options }

  TPackageOptions = class
  private
    FPackage: TPackage;
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
  protected
    property Package: TPackage read FPackage;
  public
    procedure Assign(Source: TPackageOptions); virtual;
    constructor Create(APackage: TPackage); virtual;
    destructor Destroy; override;
    procedure LoadIni(AIni: TIniFile); virtual;
    procedure SaveIni(AIni: TIniFile); virtual;
    procedure LoadXML(ARoot: IXMLNode); virtual;
    procedure SaveXML(ARoot: IXMLNode); virtual;
    property LoadLegacy: Boolean read FLoadLegacy write FLoadLegacy;
    property FileVersion: WideString read FFileVersion write SetFileVersion;
    property ExecuteProgram: WideString read FExecuteProgram write SetExecuteProgram;
    property ReadmeFile: TPackageContentFile read FReadmeFile write SetReadmeFile;
    property GraphicFile: TPackageContentFile read FGraphicFile write SetGraphicFile;
    property RegistryKeys: TPackageRegistryKeyList read FRegistryKeys;

  end;

  { Package Information }

  TPackageInfoEntry = class
  private
    FPackage: TPackage;
    FURL: WideString;
    FDescription: WideString;
    FName: WideString;
    FInfoType: TPackageInfoEntryType;
    procedure SetDescription(Value: WideString);
    procedure SetName(Value: WideString);
    procedure SetURL(Value: WideString);
  public
    constructor Create(APackage: TPackage);
    procedure Assign(Source: TPackageInfoEntry); virtual;
    property InfoType: TPackageInfoEntryType read FInfoType;
    property Name: WideString read FName write SetName;
    property Description: WideString read FDescription write SetDescription;
    property URL: WideString read FURL write SetURL;
  end;

  TPackageInfoEntryList = class(TObjectList<TPackageInfoEntry>)
  private
    FPackage: TPackage;
  protected
    procedure SetDesc(Name, Desc: WideString);
    procedure SetURL(Name, URL: WideString);
    function DescIndexOf(Name: WideString): WideString;
    function UrlIndexOf(Name: WideString): WideString;
  public
    constructor Create(APackage: TPackage);
    procedure Assign(Source: TPackageInfoEntryList); virtual;
    procedure LoadIni(AIni: TIniFile); virtual;
    procedure SaveIni(AIni: TIniFile); virtual;
    procedure LoadXML(ARoot: IXMLNode); virtual;
    procedure SaveXML(ARoot: IXMLNode); virtual;
    function IndexOf(Name: WideString): Integer; overload;


    property Desc[Name: WideString]: WideString read DescIndexOf write SetDesc;
    property URL[Name: WideString]: WideString read URLIndexOf write SetURL;

    function Add(Item: TPackageInfoEntry): Integer;
  end;

  { Package Start Menu Classes }

  TPackageStartMenuEntryLocation = (psmelStartMenu, psmelDesktop); //, psmelQuickLaunch);

  TPackageStartMenuEntry = class
  private
    FPackage: TPackage;
  public
    Name: WideString;
    Prog: WideString;
    Params: WideString;
    Icon: WideString;
    Location: TPackageStartMenuEntryLocation;
    constructor Create(APackage: TPackage);
    procedure Assign(Source: TPackageStartMenuEntry); virtual;
  end;

  TPackageStartMenuEntryList = class(TObjectList<TPackageStartMenuEntry>)
  private
    FPackage: TPackage;
  public
    constructor Create(APackage: TPackage);
    procedure Assign(Source: TPackageStartMenuEntryList); virtual;
    procedure LoadIni(AIni: TIniFile); virtual;
    procedure SaveIni(AIni: TIniFile); virtual;
    procedure LoadXML(ARoot: IXMLNode); virtual;
    procedure SaveXML(ARoot: IXMLNode); virtual;
  end;

  TPackageStartMenu = class
  private
    FPackage: TPackage;
  public
    Path: WideString;
    DoCreate: Boolean;
    AddUninstallEntry: Boolean;
    Entries: TPackageStartMenuEntryList;
    constructor Create(APackage: TPackage);
    destructor Destroy; override;
    procedure Assign(Source: TPackageStartMenu); virtual;
    procedure LoadIni(AIni: TIniFile); virtual;
    procedure SaveIni(AIni: TIniFile); virtual;
    procedure LoadXML(ARoot: IXMLNode); virtual;
    procedure SaveXML(ARoot: IXMLNode); virtual;
  end;

  { Package contained files }

  TPackageContentFile = class(TPackageBaseObject)
  private
    FPackage: TPackage;
    FCopyLocation: TPackageFileCopyLocation;
    FDescription: WideString;
    FFileName: WideString;
    FFileType: TKMFileType;
    procedure SetDescription(Value: WideString);
    procedure SetFileName(Value: WideString);
    procedure SetFileType(const Value: TKMFileType);
    procedure SetCopyLocation(const Value: TPackageFileCopyLocation);
  public
    constructor Create(APackage: TPackage);
    procedure Assign(Source: TPackageContentFile); virtual;
    function RelativeFileName: WideString;
    property FileName: WideString read FFileName write SetFileName;  // relative to .kps, or no path if inside .kmp or .exe
    property FileType: TKMFileType read FFileType write SetFileType;
    property Description: WideString read FDescription write SetDescription;
    property CopyLocation: TPackageFileCopyLocation read FCopyLocation write SetCopyLocation;
    property Package: TPackage read FPackage;
  end;

  TPackageContentFileList = class(TObjectList<TPackageContentFile>)
  private
    FPackage: TPackage;

  public
    constructor Create(APackage: TPackage);
    procedure Assign(Source: TPackageContentFileList); virtual;
    procedure LoadIni(AIni: TIniFile); virtual;
    procedure SaveIni(AIni: TIniFile); virtual;
    procedure LoadXML(ARoot: IXMLNode); virtual;
    procedure SaveXML(ARoot: IXMLNode); virtual;

    function IndexOfFileType(FFileType: TKMFileType): Integer;
    function FromFileName(Filename: WideString): TPackageContentFile;
    function FromFileNameEx(Filename: WideString): TPackageContentFile;
    procedure Delete(Index: Integer);
  end;

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
  public
    Options: TPackageOptions;
    StartMenu: TPackageStartMenu;
    Files: TPackageContentFileList;
    Info: TPackageInfoEntryList;

    property FileName: WideString read FFileName write FFileName;
    procedure Assign(Source: TPackage); virtual;
    constructor Create;
    destructor Destroy; override;
    procedure LoadIni; virtual;
    procedure SaveIni; virtual;

    procedure LoadXML; //virtual;
    procedure SaveXML; //virtual;

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

  KeymanVersion,
  utildir,
  utilsystem,
  VersionInfo;

const
  SPackageInfoTooNew = 'The package file is version %s.  This version can only read version '+SKeymanVersion+' and older files.';
  SReadmeNotOwnedCorrectly = 'The readme file ''%s'' referred to is not part of the package.';
  SGraphicNotOwnedCorrectly = 'The graphic file ''%s'' referred to is not part of the package.';
  SFileNotOwnedCorrectly = 'The file ''%s'' referred to is not part of the package.';

{-------------------------------------------------------------------------------
 - TPackageOptions                                                             -
 ------------------------------------------------------------------------------}

procedure TPackageOptions.Assign(Source: TPackageOptions);
begin
  FFileVersion := Source.FileVersion;
  FExecuteProgram := Source.ExecuteProgram;
  if Assigned(Source.ReadmeFile)
    then ReadmeFile := FPackage.Files.FromFileName(Source.ReadmeFile.FileName)
    else ReadmeFile := nil;
  if Assigned(Source.GraphicFile)
    then GraphicFile := FPackage.Files.FromFileName(Source.GraphicFile.FileName)
    else GraphicFile := nil;
  FRegistryKeys.Assign(Source.RegistryKeys);
end;

constructor TPackageOptions.Create(APackage: TPackage);
begin
  inherited Create;
  FLoadLegacy := True;
  FPackage := APackage;
  FRegistryKeys := TPackageRegistryKeyList.Create(FPackage);
  FFileVersion := SKeymanVersion;
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
  ReadmeFile :=                 FPackage.Files.FromFileName(VarToWideStr(ARoot.ChildNodes['Options'].ChildNodes['ReadMeFile'].NodeValue));
  GraphicFile :=                FPackage.Files.FromFileName(VarToWideStr(ARoot.ChildNodes['Options'].ChildNodes['GraphicFile'].NodeValue));
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
  ReadmeFile :=                 FPackage.Files.FromFileName(AIni.ReadString('Package', 'ReadMeFile', ''));
  GraphicFile :=                FPackage.Files.FromFileName(AIni.ReadString('Package', 'GraphicFile', ''));
  if Assigned(ReadmeFile) then ReadmeFile.AddNotifyObject(ReadmeRemoved);
  if Assigned(GraphicFile) then GraphicFile.AddNotifyObject(GraphicRemoved);
  FRegistryKeys.LoadIni(AIni);
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
    if Value.FPackage <> FPackage then raise EPackageInfo.CreateFmt(SGraphicNotOwnedCorrectly, [Value]);
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
    if Value.FPackage <> FPackage then raise EPackageInfo.CreateFmt(SReadmeNotOwnedCorrectly, [Value]);
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

constructor TPackageInfoEntry.Create(APackage: TPackage);
begin
  inherited Create;
  FPackage := APackage;
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
    if PackageInfoEntryTypeNames[piet] = LowerCase(FName) then
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
    inf := TPackageInfoEntry.Create(FPackage);
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

      inf := TPackageInfoEntry.Create(FPackage);
      inf.Name := s[i];
      inf.Description := description;
      inf.URL := url;
      Add(inf);
    end;
  finally
    s.Free;
  end;
end;

procedure TPackageInfoEntryList.SaveIni(AIni: TIniFile);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    AIni.WriteString('Info', Items[i].Name, Format('"%s","%s"', [Items[i].Description, Items[i].URL]));
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
    inf := TPackageInfoEntry.Create(FPackage);
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
    inf := TPackageInfoEntry.Create(FPackage);
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

constructor TPackageInfoEntryList.Create(APackage: TPackage);
begin
  inherited Create;
  FPackage := APackage;
end;

procedure TPackageInfoEntryList.Assign(Source: TPackageInfoEntryList);
var
  i: Integer;
  pie: TPackageInfoEntry;
begin
  Clear;
  for i := 0 to Source.Count - 1 do
  begin
    pie := TPackageInfoEntry.Create(FPackage);
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

constructor TPackageStartMenuEntry.Create(APackage: TPackage);
begin
  inherited Create;
  FPackage := APackage;
end;

{-------------------------------------------------------------------------------
 - TPackageStartMenuEntryList                                                  -
 ------------------------------------------------------------------------------}

procedure TPackageStartMenuEntryList.LoadXML(ARoot: IXMLNode);

    function StrToStartMenuEntryLocation(s: WideString): TPackageStartMenuEntryLocation;
    var
      v: Integer;
    begin
      v := System.TypInfo.GetEnumValue(TypeInfo(TPackageStartMenuEntryLocation), s);
      if v = -1 then Result := psmelStartMenu
      else Result := TPackageStartMenuEntryLocation(v);
    end;
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
      sme := TPackageStartMenuEntry.Create(FPackage);
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

      sme := TPackageStartMenuEntry.Create(FPackage);
      sme.Name := s[i];
      sme.Prog := prog;
      sme.Params := params;
      Add(sme);
    end;
  finally
    s.Free;
  end;
end;

procedure TPackageStartMenuEntryList.SaveIni(AIni: TIniFile);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    AIni.WriteString('StartMenuEntries', Items[i].Name, Format('"%s","%s"', [Items[i].Prog, Items[i].Params]));
end;

constructor TPackageStartMenuEntryList.Create(APackage: TPackage);
begin
  inherited Create;
  FPackage := APackage;
end;

procedure TPackageStartMenuEntryList.Assign(Source: TPackageStartMenuEntryList);
var
  i: Integer;
  psme: TPackageStartMenuEntry;
begin
  Clear;
  for i := 0 to Source.Count - 1 do
  begin
    psme := TPackageStartMenuEntry.Create(FPackage);
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
  inherited Create;
  FPackage := APackage;
  Entries := TPackageStartMenuEntryList.Create(FPackage);
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

procedure TPackageStartMenu.SaveIni(AIni: TIniFile);
begin
  AIni.WriteString('StartMenu', 'Path',              Path);
  AIni.WriteBool(  'StartMenu', 'Create',            DoCreate);
  AIni.WriteBool(  'StartMenu', 'AddUninstallEntry', AddUninstallEntry);
  Entries.SaveIni(AIni);
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

constructor TPackageContentFile.Create(APackage: TPackage);
begin
  inherited Create;
  FPackage := APackage;
end;

function TPackageContentFile.RelativeFileName: WideString;
begin
  Result := ExtractRelativePath(FPackage.FileName, FFileName);
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
      subfile := TPackageContentFile.Create(FPackage);
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
      subfile := TPackageContentFile.Create(FPackage);
      subfile.FileName := filename;
      subfile.Description := description;
      subfile.FCopyLocation := copylocation;
      Add(subfile);
    end;
  finally
    s.Free;
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

constructor TPackageContentFileList.Create(APackage: TPackage);
begin
  inherited Create;
  FPackage := APackage;
end;

procedure TPackageContentFileList.Assign(Source: TPackageContentFileList);
var
  i: Integer;
  psf: TPackageContentFile;
begin
  Clear;
  for i := 0 to Source.Count - 1 do
  begin
    psf := TPackageContentFile.Create(FPackage);
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
end;

constructor TPackage.Create;
begin
  inherited Create;
  if not Assigned(Files)     then Files     := TPackageContentFileList.Create(Self);
  if not Assigned(Options)   then Options   := TPackageOptions.Create(Self);
  if not Assigned(StartMenu) then StartMenu := TPackageStartMenu.Create(Self);
  if not Assigned(Info)      then Info      := TPackageInfoEntryList.Create(Self);
end;

destructor TPackage.Destroy;
begin
  Files.Free;
  Options.Free;
  StartMenu.Free;
  Info.Free;
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
  finally
    ini.Free;
  end;
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

procedure TPackage.DoLoadXML(ARoot: IXMLNode);
var
  FVersion: WideString;
begin

  FVersion := VarToWideStr(ARoot.ChildNodes['System'].ChildNodes['FileVersion'].NodeValue);
  if (FVersion <> SKeymanVersion70) and (FVersion <> SKeymanVersion80) and (FVersion <> SKeymanVersion) then
    PackageLoadError('Package file version '+FVersion+' is not recognised.');

  StartMenu.LoadXML(ARoot);
  Info.LoadXML(ARoot);
  Files.LoadXML(ARoot);
  Options.LoadXML(ARoot);
end;

procedure TPackage.DoSaveXML(ARoot: IXMLNode);
begin
  ARoot.ChildNodes['System'].ChildNodes['KeymanDeveloperVersion'].NodeValue := GetVersionString;
  Options.SaveXML(ARoot);
  StartMenu.SaveXML(ARoot);
  Info.SaveXML(ARoot);
  Files.SaveXML(ARoot);
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
    ini.UpdateFile;
  finally
    ini.Free;
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

constructor TPackageBaseObject.Create;
begin
  inherited Create;
  FNotifyObjects := TObjectList<TPackageNotifyEventWrapper>.Create;
end;

destructor TPackageBaseObject.Destroy;
begin
  Notify(netDestroy);
  FNotifyObjects.Free;
  inherited Destroy;
end;

function TPackageBaseObject.Notify(EventType: TPackageNotifyEventType): Boolean;
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

procedure TPackageBaseObject.AddNotifyObject(FEventHandler: TPackageNotifyEvent);
var
  ev: TPackageNotifyEventWrapper;
begin
  RemoveNotifyObject(FEventHandler);
  ev := TPackageNotifyEventWrapper.Create;
  ev.FEvent := FEventHandler;
  FNotifyObjects.Add(ev);
end;

procedure TPackageBaseObject.RemoveNotifyObject(FEventHandler: TPackageNotifyEvent);
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

{ TPackageRegistryKeyList }

procedure TPackageRegistryKeyList.Assign(Source: TPackageRegistryKeyList);
var
  i: Integer;
  prk: TPackageRegistryKey;
begin
  Clear;
  for i := 0 to Source.Count - 1 do
  begin
    prk := TPackageRegistryKey.Create(FPackage);
    prk.Assign(Source[i]);
    Add(prk);
  end;
end;

constructor TPackageRegistryKeyList.Create(APackage: TPackage);
begin
  inherited Create;
  FPackage := APackage;
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
      pkr := TPackageRegistryKey.Create(FPackage);

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
    pkr := TPackageRegistryKey.Create(FPackage);

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
  inherited Create;
  FPackage := APackage;
  FRoot := HKEY_CURRENT_USER;
  FValueType := rkvtString;
end;

destructor TPackageRegistryKey.Destroy;
begin
  inherited Destroy;
end;

end.

