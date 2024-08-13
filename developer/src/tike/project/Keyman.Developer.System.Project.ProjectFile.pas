(*
  Name:             Keyman.Developer.System.Project.ProjectFile
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
                    01 Aug 2006 - mcdurdin - Rework for Keyman Developer 7, XML based project
                    23 Aug 2006 - mcdurdin - Add IDEState property for automatic saving/loading of IDE state
                    06 Oct 2006 - mcdurdin - Add pfaTestKeymanWeb
                    04 Dec 2006 - mcdurdin - Localize project templates
                    19 Mar 2007 - mcdurdin - I708 - Add IndexOfFileNameAndParent
                    16 May 2007 - mcdurdin - I791 - Unicode characters not rendering correctly in project
                    30 May 2007 - mcdurdin - I817 - Added pfaCompileInstaller
                    30 May 2007 - mcdurdin - I671 - Fixed crash when changing project
                    30 May 2007 - mcdurdin - I730 - Fixed details not updating in project
                    30 May 2007 - mcdurdin - I742 - Fixed crash when project folder is deleted
                    30 May 2007 - mcdurdin - I805 - Fixed crash when project folder is deleted
                    04 Jun 2007 - mcdurdin - Fix missing Result
                    19 Jun 2007 - mcdurdin - Fix crash when files are updated when loading a project
                    13 Jul 2007 - mcdurdin - I939 - Report script errors to website
                    23 Aug 2007 - mcdurdin - I1010 - persist untitled project
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    27 Mar 2008 - mcdurdin - I1221 - Fixup performance (only save at end of GetFileParamters)
                    14 Jun 2008 - mcdurdin - I1420 - Improve project performance
                    16 Jan 2009 - mcdurdin - I1685 - Fix crash when closing a file that has subfiles added to the project
                    09 Mar 2009 - mcdurdin - I1890 - Images missing in project the first time Keyman Developer ever starts
                    17 Dec 2010 - mcdurdin - I2595 - Remove GnuGetText from project as we are not using it and it crashes
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    26 Jun 2012 - mcdurdin - I3379 - KM9 - Remove old Winapi references now in Delphi libraries
                    19 Oct 2012 - mcdurdin - I3473 - V9.0 - Tidy up project loader encoding check to use DXE2 methods
                    24 Oct 2012 - mcdurdin - I3310 - buf reference was missing dereference in TFileStream.Read call
                    21 Feb 2014 - mcdurdin - I4057 - V9.0 - Keyman Developer Keyboard Font dialog helpful to reduce font confusion
                    01 May 2014 - mcdurdin - I4181 - V9.0 - Stop using DeleteFileAlways, MOVEFILE_DELAY_UNTIL_REBOOT
                    04 May 2015 - mcdurdin - I4687 - V9.0 - Split project UI actions into separate classes
                    04 May 2015 - mcdurdin - I4688 - V9.0 - Add build path to project settings
                    04 May 2015 - mcdurdin - I4692 - V9.0 - Add Clean as project action
                    04 May 2015 - mcdurdin - I4694 - V9.0 - Split UI actions from non-UI actions in projects
                    05 May 2015 - mcdurdin - I4698 - V9.0 - Split project and user preferences files
                    06 May 2015 - mcdurdin - I4702 - Package actions crash if package not part of a project [CrashID:tike.exe_9.0.496.0_008033BD_EAccessViolation]
                    06 May 2015 - mcdurdin - I4701 - V9.0 - If version is not specified in keyboard file, it gets blank instead of 1.0 in kmcomp .kpj
                    11 May 2015 - mcdurdin - I4706 - V9.0 - Update compile logging for silent and warning-as-error cleanness
                    11 May 2015 - mcdurdin - I4709 - V9.0 - Use static hashing for id for project files to avoid unnecessary changes
                    12 May 2015 - mcdurdin - I4710 - V9.0 - Project can crash if file parameters change during build
                    27 May 2015 - mcdurdin - I4720 - Compiling a standalone keyboard crashes Developer [CrashID:tike.exe_9.0.503.0_00A4316C_EAccessViolation]
                    27 May 2015 - mcdurdin - I4703 - Loading a non-project file as a project crashes Developer [CrashID:tike.exe_9.0.497.0_00813E09_EProjectLoader]
                    30 May 2015 - mcdurdin - I4729 - If you attempt to load a non-XML file as a project, it silently fails and overwrites the file
                    24 Aug 2015 - mcdurdin - I4866 - Add warn on deprecated features to project and compile
                    24 Aug 2015 - mcdurdin - I4865 - Add treat hints and warnings as errors into project

*)
unit Keyman.Developer.System.Project.ProjectFile;  // I3306   // I4687

interface

uses
  System.Classes,
  System.Contnrs,
  System.SysUtils,
  System.Types,
  System.WideStrings,
  Winapi.Messages,
  Winapi.msxml,
  Winapi.Windows,
  Xml.Win.msxmldom,
  Xml.xmldoc,
  Xml.xmlintf,

  mrulist,
  Keyman.Developer.System.Project.ProjectLog,
  TempFileManager,
  utilfiletypes;

type
  TProjectState = (psCreating, psReady, psLoading, psSaving, psDestroying);
  TProjectType = (ptUnknown, ptKeyboard, ptLexicalModel); // distinct from utilfiletypes.TKeymanProjectType

  TProjectVersion = (pvUnknown, pv10, pv20);

  TProjectOptionsRecord = record
    BuildPath: string;
    SourcePath: string;
    CompilerWarningsAsErrors: Boolean;
    WarnDeprecatedCode: Boolean;
    CheckFilenameConventions: Boolean;
    SkipMetadataFiles: Boolean;
    ProjectType: TProjectType;
    Version: TProjectVersion;
  end;

  TProjectOptions = class
    BuildPath: string;
    SourcePath: string;
    CompilerWarningsAsErrors: Boolean;
    WarnDeprecatedCode: Boolean;
    CheckFilenameConventions: Boolean;
    SkipMetadataFiles: Boolean;
    ProjectType: TProjectType;
    Version: TProjectVersion;
  public
    procedure Assign(source: TProjectOptions); overload;
    procedure Assign(source: TProjectOptionsRecord); overload;
    function EqualsRecord(source: TProjectOptionsRecord): Boolean;
  end;

const DefaultProjectOptions: array[TProjectVersion] of TProjectOptionsRecord = (
( // unknown version, this is unused
  BuildPath: '';
  SourcePath: '';
  CompilerWarningsAsErrors: False;
  WarnDeprecatedCode: False;
  CheckFilenameConventions: False;
  SkipMetadatafiles: False;
  ProjectType: ptKeyboard;
  Version: pvUnknown
), ( // 1.0
  BuildPath: '';
  SourcePath: '';
  CompilerWarningsAsErrors: False;
  WarnDeprecatedCode: True;
  CheckFilenameConventions: False;
  SkipMetadatafiles: True;
  ProjectType: ptKeyboard;
  Version: pv10
), ( // 2.0
  BuildPath: '$PROJECTPATH\build';
  SourcePath: '$PROJECTPATH\source';
  CompilerWarningsAsErrors: False;
  WarnDeprecatedCode: True;
  CheckFilenameConventions: False;
  SkipMetadatafiles: False;
  ProjectType: ptKeyboard;
  Version: pv20
));

{ TODO: this will be enabled in 18.0; see #10113
const
  C_ProjectStandardFilename = 'keyman.kpj';
}

type
  { Forward declarations }

  IProjectFileFreeNotification = interface;

  TProjectFileList = class;
  TProjectFile = class;
  TProjectFileStates = class;

  { TProject }

  IProjectUI = interface
    ['{FA8AF90B-7C98-4306-AAF1-A15C6E20CD0C}']
    procedure Refresh;
  end;

  TProject = class
  private
    FBusy, FMustSave: Boolean;
    FState: TProjectState;
    FFileName: string;
    FFiles: TProjectFileList;
    FDisplayState: WideString;
    FMRU: TMRUList;
    FOptions: TProjectOptions;
    FUpgradeMessages: TStrings;

    function ImportFromIni(FileName: string): Boolean;
    function LoadFromXML(FileName: string): Boolean;
    function ExpandMemberFileName(Root, FileName: string): string;
    //procedure ChildRefresh(Sender: TObject);
    procedure ListNotify(Item: TProjectFile; Action: TListNotification);
    procedure ChildRefresh(Sender: TObject);
    procedure MRUChange(Sender: TObject);
    procedure UpdateFileParameters;
    function GetUserFileName: string;
    procedure PopulateFolder(const path: string);
    function GetTargetFilename10(ATargetFile, ASourceFile,
      AVersion: string): string;
    function GetTargetFilename20(ATargetFile, ASourceFile,
      AVersion: string): string;
  protected
    procedure DoRefresh; virtual;
    procedure DoRefreshCaption; virtual;

    property State: TProjectState read FState;

  public
    procedure Log(AState: TProjectLogState; Filename, Msg: string; MsgCode, line: Integer); virtual;

    constructor Create(AProjectType: TProjectType; AFileName: string; ALoad: Boolean); virtual;
    destructor Destroy; override;

    procedure Refresh;

    function Render: WideString;

    function IsDefaultProject(Version: TProjectVersion): Boolean;

    function ResolveProjectPath(APath: string): string;
    function ResolveSourcePath: string;

    function Load: Boolean; virtual;   // I4694
    function Save: Boolean; virtual;   // I4694
    function SaveUser: Boolean; virtual;   // I4694
    function PopulateFiles: Boolean;

    class function StandardTemplatePath: string;
    class function StringsTemplatePath: string;

    function GetTargetFilename(ATargetFile, ASourceFile, AVersion: string): string;   // I4688

    function CanUpgrade: Boolean;
    function Upgrade: Boolean;
    property UpgradeMessages: TStrings read FUpgradeMessages;

    //procedure AddMRU(const FFileName: string);
    property MRU: TMRUList read FMRU;

    property Options: TProjectOptions read FOptions;   // I4688

    function FindFile(AFileName: string): TProjectFile;

    property FileName: string read FFileName;
    property UserFileName: string read GetUserFileName;

    property Files: TProjectFileList read FFiles;
    property DisplayState: WideString read FDisplayState write FDisplayState;

    property Busy: Boolean read FBusy write FBusy;
    property MustSave: Boolean read FMustSave write FMustSave;
  end;

  { TProjectFile and associated classes }

  TProjectFile = class
  private
    FNotifiers: TInterfaceList;
    FProject: TProject;
    FFileName: string;
    FModified: Boolean;
    FTag: Integer;
    FParent: TProjectFile;
    FID: string;
    FParentFileID: string;
    FIDEState: TProjectFileStates;
    FFileVersion: string;   // I4688
    FUI: TObject;   // I4687
    FHasWarning: Boolean;   // I4706
    FCheckedParameters: Boolean;   // I4710
    procedure SetFileName(Value: string);
    procedure SetModified(const Value: Boolean);
    procedure SetParent(const Value: TProjectFile);
    function GetName: string;
    function CheckGetFileParameters: Boolean;
    function CleanID(s: string): string;
    function GetOwnerProject: TProject;
    procedure UpdateID;
  protected
    procedure Log(AState: TProjectLogState; Msg: string; MsgCode, Line: Integer);   // I4694   // I4706

    procedure SetFileVersion(const AVersion: string);   // I4688
    //FFiles: TProjectFileList;
    function GetRelativeOrder: Integer; virtual; abstract;
    procedure GetFileParameters; virtual; abstract;

    procedure CleanFile(ATargetFilename: string; ADontCleanIfInSource: Boolean = False);   // I4692

    property CheckedParameters: Boolean read FCheckedParameters write FCheckedParameters;   // I4710
  public
    constructor Create(AProject: TProject; AFileName: string; AParent: TProjectFile); virtual;
    destructor Destroy; override;

    function IsCompilable: Boolean; virtual;
    function IsSourceFile: Boolean; virtual;
    class function IsFileTypeSupported(const Filename: string): Boolean; virtual;

    procedure Load(node: IXMLNode); virtual;   // I4698
    procedure LoadState(node: IXMLNode); virtual;   // I4698
    procedure Save(node: IXMLNode); virtual;   // I4698
    procedure SaveState(node: IXMLNode); virtual;   // I4698

    procedure AddFreeNotification(AClient: IProjectFileFreeNotification);
    procedure RemoveFreeNotification(AClient: IProjectFileFreeNotification);

    procedure SetUI(AUI: TObject);   // I4687

    property OwnerProject: TProject read GetOwnerProject;
    property Project: TProject read FProject;
    property FileName: string read FFileName write SetFileName;
    property FileVersion: string read FFileVersion;   // I4688
    //property Files: TProjectFileList read FFiles;
    property Modified: Boolean read FModified write SetModified;
    property RelativeOrder: Integer read GetRelativeOrder;
    property HasCompileWarning: Boolean read FHasWarning write FHasWarning;   // I4706

    property Parent: TProjectFile read FParent write SetParent;
    property ParentFileID: string read FParentFileID write FParentFileID; // Used only in load

    property Name: string read GetName;
    property ID: string read FID;

    property IDEState: TProjectFileStates read FIDEState;

    property Tag: Integer read FTag write FTag;

    property UI: TObject read FUI;   // I4687
  end;

  TProjectFileClass = class of TProjectFile;

  TProjectFileListNotificationEvent = procedure(Item: TProjectFile; Action: TListNotification) of object;

  TProjectFileList = class(TObjectList)
  private
    FOnChange: TProjectFileListNotificationEvent;
    //procedure SortByType;
  protected
    function Get(Index: Integer): TProjectFile;
    procedure Put(Index: Integer; Item: TProjectFile);
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    property Items[Index: Integer]: TProjectFile read Get write Put; default;
    function IndexOf(Item: TProjectFile): Integer;
    function IndexOfID(ID: string): Integer;
    function IndexOfFileName(Name: string): Integer;
    function IndexOfFileNameAndParent(Name: string; Parent: TProjectFile): Integer;
    function Add(Item: TProjectFile): Integer;
    property OnChange: TProjectFileListNotificationEvent read FOnChange write FOnChange;
  end;

  TProjectFileState = class
    Name: WideString;
    Value: WideString;
  end;

  TProjectFileStates = class(TObjectList)
  private
    function GetItem(Index: WideString): WideString;
    procedure SetItem(Index, Value: WideString);
    function IndexOfName(Name: WideString): Integer;
  public
    procedure Delete(Index: WideString);
    property Items[Index: WideString]: WideString read GetItem write SetItem; default;
  end;

  IProjectFileFreeNotification = interface
    ['{C0B8261C-2F59-4C62-A59C-BAE3E09EEDDB}']
    procedure ProjectFileDestroying(ProjectFile: TProjectFile);
  end;


function ProjectTypeFromString(s: string): TProjectType;
function ProjectTypeToString(pt: TProjectType): string;
function ProjectVersionFromString(s: string): TProjectVersion;
function ProjectVersionToString(pv: TProjectVersion): string;

implementation

uses
  Winapi.ShlObj,
  System.AnsiStrings,
  System.IniFiles,
  System.DateUtils,
  System.Hash,
  System.StrUtils,
  System.Variants,

  Keyman.Developer.System.Project.Project,
  Keyman.Developer.System.Project.ProjectFileType,
  Keyman.Developer.System.Project.ProjectLoader,
  Keyman.Developer.System.Project.ProjectSaver,
  Keyman.Developer.System.Project.UrlRenderer,
  RedistFiles,
  RegistryKeys,
  Unicode,
  utildir,
  utilsystem;

{ TProjectFileList }

function TProjectFileList.Add(Item: TProjectFile): Integer;
begin
  Result := inherited Add(Item);
  //SortByType;
end;

function TProjectFileList.Get(Index: Integer): TProjectFile;
begin
  Result := TProjectFile(inherited Get(Index));
end;

function TProjectFileList.IndexOf(Item: TProjectFile): Integer;
begin
  Result := inherited IndexOf(Item);
end;

function TProjectFileList.IndexOfFileName(Name: string): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if (Items[i].FParentFileID = '') and (AnsiCompareText(Items[i].FileName, Name) = 0) then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

function TProjectFileList.IndexOfFileNameAndParent(Name: string; Parent: TProjectFile): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if (Items[i].Parent = Parent) and (AnsiCompareText(Items[i].FileName, Name) = 0) then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

function TProjectFileList.IndexOfID(ID: string): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].ID = ID then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

procedure TProjectFileList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Assigned(FOnChange) then FOnChange(Ptr, Action);
end;

procedure TProjectFileList.Put(Index: Integer; Item: TProjectFile);
begin
  inherited Put(Index, Item);
end;

{function ProjectFileListSortByType(Item1, Item2: Pointer): Integer;
begin
  Result := TProjectFile(Item1).RelativeOrder - TProjectFile(Item2).RelativeOrder;
end;}

{procedure TProjectFileList.SortByType;
begin
  // function ensures that files will be compiled in the correct order
  //Sort(ProjectFileListSortByType);
end;}

{ TProjectFile }

procedure TProjectFile.AddFreeNotification(
  AClient: IProjectFileFreeNotification);
begin
  if FNotifiers.IndexOf(AClient) < 0 then
    FNotifiers.Add(AClient);
end;

function TProjectFile.CheckGetFileParameters: Boolean;
var
  FileDateTime, ProjectDateTime: TDateTime;
begin
  Result := False;
  if not Assigned(FProject) then Result := True
  else if FileAge(FFileName, FileDateTime) then
  begin
    if not FileAge(FProject.FFileName, ProjectDateTime) then Result := True
    else if (FileDateTime >= ProjectDateTime) and (SecondsBetween(FileDateTime, ProjectDateTime) >= 0) then Result := True;
  end;
  if Result then GetFileParameters;
end;

procedure TProjectFile.CleanFile(ATargetFilename: string; ADontCleanIfInSource: Boolean);   // I4692
var
  FTargetPath: string;
  FTargetFilename: string;
begin
  // Sanity check.
  if ATargetFilename = '' then
    Exit;

  // Go through the BuildPath and sub folders, removing any files that match AFileMask?
  FTargetFilename := OwnerProject.GetTargetFilename(ATargetFilename, FileName, FileVersion);
  if FTargetFilename = '' then
    Exit;

  // If the target is the source file, that means we are building to the source folder
  // so we don't want to delete the output file. e.g. for .kvk files
  if SameFileName(ATargetFilename, FTargetFilename) and ADontCleanIfInSource then
    Exit;

  // Delete file and remove the folder if empty
  FTargetPath := ExtractFileDir(FTargetFilename);
  if FileExists(FTargetFilename) then
    System.SysUtils.DeleteFile(FTargetFilename);
  if DirectoryExists(FTargetPath) and DirectoryEmpty(FTargetPath) then
    RemoveDirectory(PChar(FTargetPath));
end;

function TProjectFile.CleanID(s: string): string;
begin
  Result := AnsiReplaceText(s, '{', '');
  Result := AnsiReplaceText(Result, '}', '');
  Result := AnsiReplaceText(Result, '-', '');
  if Copy(Result, 1, 3) <> 'id_' then Result := 'id_'+Result;
end;

constructor TProjectFile.Create(AProject: TProject; AFileName: string; AParent: TProjectFile);
begin
  inherited Create;

  FNotifiers := TInterfaceList.Create;
  FIDEState := TProjectFileStates.Create;
  FFileName := AFileName;
  FProject := AProject;
  FParent := AParent;

  if not Assigned(AProject) then   // I4720
    CheckGetFileParameters;

  if (GetCurrentThreadId = MainThreadID) and Assigned(FDoCreateProjectFileUI) then   // I4702
    FDoCreateProjectFileUI(Self);   // I4687

  UpdateID;
end;

destructor TProjectFile.Destroy;
var
  i: Integer;
  p: TProject;
begin
  for i := 0 to FNotifiers.Count - 1 do
    (FNotifiers.Items[i] as IProjectFileFreeNotification).ProjectFileDestroying(Self);

  p := OwnerProject;

  if Assigned(p) then
  begin
    p.Files.Extract(Self);

    for i := p.Files.Count - 1 downto 0 do
      if p.Files[i].Parent = Self then
        p.Files.Delete(i);
  end;

  FreeAndNil(FIDEState);

  FreeAndNil(FUI);

  FNotifiers.Free;
  inherited Destroy;
end;

function TProjectFile.GetName: string;
begin
  Result := ExtractFileName(FFileName);
end;

function TProjectFile.GetOwnerProject: TProject;
begin
  Result := FProject;
  if Result = nil then
  begin
    if Assigned(FParent) then
      Result := FParent.OwnerProject;
    if Result = nil then
    begin
      // TODO: RAISE ERROR
      Result := FGlobalProject;
    end;
  end;
end;

function TProjectFile.IsCompilable: Boolean;
begin
  Result := False;
end;

class function TProjectFile.IsFileTypeSupported(
  const Filename: string): Boolean;
begin
  // assumes that if we are registered for the file type extension, then we can
  // handle the file. For example, .xml LDML keyboards
  Result := True;
end;

function TProjectFile.IsSourceFile: Boolean;
var
  FilePath, SourcePath: string;
begin
  // An file that is a sub-file of another one is never a source file
  if Assigned(FParent) then
    Exit(False);

  // If no project is assigned, this is an unsupported mode (loading without
  // project is no longer supported in 17.0+) but we don't want to crash so
  // we'll just treat this as a source file
  if not Assigned(FProject) then
    Exit(True);

  // We don't restrict builds to sourcepath files for v1.0 projects
  if FProject.Options.Version = pv10 then
    Exit(True);

  // If no sourcepath is defined, then we'll build any files
  if FProject.Options.SourcePath = '' then
    Exit(True);

  // Only return true if the file is directly in the ProjectOptions.SourcePath folder
  SourcePath := DosSlashes(FProject.ResolveProjectPath(FProject.Options.SourcePath));
  FilePath := DosSlashes(ExtractFilePath(FFileName));
  Result := SameFileName(SourcePath, FilePath);
end;

procedure TProjectFile.Load(node: IXMLNode);   // I4698
begin
  if node.ChildNodes.IndexOf('ID') >= 0 then
    FID := CleanID(VarToWideStr(node.ChildValues['ID']));
  if node.ChildNodes.IndexOf('ParentFileID') >= 0 then
    FParentFileID := CleanID(VarToWideStr(node.ChildValues['ParentFileID']));
end;

procedure TProjectFile.LoadState(node: IXMLNode);   // I4698
var
  i: Integer;
begin
  if node.ChildNodes.IndexOf('IDEState') >= 0 then
  begin
    FIDEState.Clear;
    node := node.ChildNodes.Nodes['IDEState'];
    for i := 0 to node.ChildNodes.Count - 1 do
      FIDEState[node.ChildNodes[i].NodeName] := node.ChildNodes[i].NodeValue;
  end;
end;

procedure TProjectFile.Log(AState: TProjectLogState; Msg: string; MsgCode, Line: Integer);   // I4694   // I4706
begin
  OwnerProject.Log(AState, Filename, Msg, MsgCode, Line);   // I4702   // I4706
end;

procedure TProjectFile.RemoveFreeNotification(
  AClient: IProjectFileFreeNotification);
begin
  FNotifiers.Remove(AClient);
end;

procedure TProjectFile.Save(node: IXMLNode);   // I4698
begin
  node.AddChild('ID').NodeValue := FID;
  node.AddChild('Filename').NodeValue := ExtractFileName(FFileName);
  node.AddChild('Filepath').NodeValue := ExtractRelativePath(FProject.FileName, DosSlashes(FFileName));
  node.AddChild('FileVersion').NodeValue := FFileVersion;   // I4701

  // Note: FileType is only ever written in Delphi code; it is used by xsl
  // transforms for rendering
  node.AddChild('FileType').NodeValue := ExtractFileExt(FFileName);
  if Assigned(FParent) then
    node.AddChild('ParentFileID').NodeValue := FParent.ID;
end;

procedure TProjectFile.SaveState(node: IXMLNode);   // I4698
var
  I: Integer;
begin
  node.AddChild('ID').NodeValue := FID;
  node.AddChild('FullPath').NodeValue := FFileName;

  if FIDEState.Count > 0 then
  begin
    node := node.AddChild('IDEState');
    for I := 0 to FIDEState.Count - 1 do
    begin
      with node.AddChild(TProjectFileState(FIDEState.Get(I)).Name) do
        NodeValue := TProjectFileState(FIDEState.Get(I)).Value;
    end;
  end;
end;

procedure TProjectFile.UpdateID;
begin
  if Assigned(FProject)
    then FID := CleanID(THashMD5.GetHashString(LowerCase( ExtractRelativePath(FProject.FileName, FFileName) )))   // I4709
    else FID := CleanID(THashMD5.GetHashString(LowerCase(FFileName)));
end;

procedure TProjectFile.SetFileName(Value: string);
begin
  FFileName := Value;
  UpdateID;
  CheckGetFileParameters;
  //if Assigned(FProject) then FProject.ChildRefresh(Self);
end;

procedure TProjectFile.SetFileVersion(const AVersion: string);   // I4688
begin
  FFileVersion := AVersion;
end;

procedure TProjectFile.SetModified(const Value: Boolean);
begin
  if FModified <> Value then
  begin
    FModified := Value;
    if FModified = False then
    begin
      CheckGetFileParameters;
      if Assigned(FProject) then FProject.ChildRefresh(Self);
    end;
  end;
end;

procedure TProjectFile.SetParent(const Value: TProjectFile);
begin
  FParent := Value;
  if Assigned(FProject) then FProject.ChildRefresh(Self);
end;

procedure TProjectFile.SetUI(AUI: TObject);   // I4687
begin
  FUI := AUI;
end;

{ TProject }

procedure TProject.ChildRefresh(Sender: TObject);
begin
  if FState = psReady then
  begin
    Save;
    Refresh;
  end;
end;

constructor TProject.Create(AProjectType: TProjectType; AFileName: string; ALoad: Boolean);
var
  i: Integer;
begin
  Assert(AFileName <> '');

  FUpgradeMessages := TStringList.Create;

  // Assumes v1.0 by default
  FOptions := TProjectOptions.Create;
  FOptions.Assign(DefaultProjectOptions[pv10]);

  if AProjectType = ptUnknown
    then FOptions.ProjectType := ptKeyboard
    else FOptions.ProjectType := AProjectType;

  FState := psCreating;
  inherited Create;
  FMRU := TMRUList.Create('');
  FMRU.OnChange := MRUChange;
  FFileName := AFileName;
  FFiles := TProjectFileList.Create;
  FFiles.OnChange := ListNotify;

  FState := psReady;

  FMustSave := False;

  if ALoad and not Load then   // I4703
  begin
    raise EProjectLoader.Create('Unable to load project '+FFileName);
  end;

  FBusy := True;
  i := 0;
  while i < FFiles.Count do
  begin
    if FFiles[i].CheckGetFileParameters then FMustSave := True;
    Inc(i);
  end;

  FBusy := False;
  if FMustSave then Save;
end;

destructor TProject.Destroy;
begin
  FState := psDestroying;
  FFiles.Free;
  FMRU.Free;
  FreeandNil(FUpgradeMessages);
  FreeAndNil(FOptions);   // I4688
  inherited Destroy;
end;

procedure TProject.DoRefresh;   // I4687
begin
end;

procedure TProject.DoRefreshCaption;   // I4687
begin
end;

procedure TProject.ListNotify(Item: TProjectFile; Action: TListNotification);
begin
  if FState = psReady then
  begin
    if Action = lnAdded then
      Item.GetFileParameters;
    if FBusy then
      FMustSave := True
    else
    begin
      Save;
      Refresh;
    end;
  end;
end;

function TProject.Load: Boolean;
var
  buf: TBytes;  // I3310
  encoding: TEncoding;
begin
  encoding := nil;
  FState := psLoading;
  try
    SetLength(buf, 32);
    if FileExists(FileName) then
    begin
      with TFileStream.Create(FileName, fmOpenRead) do
      try
        Read(buf[0], 32);  // I3310
      finally
        Free;
      end;

      if (buf[0] = Ord('<')) or ((TEncoding.GetBufferEncoding(buf, encoding) > 0) and (encoding = TEncoding.UTF8))  // I3310, I3473
        then Result := LoadFromXML(FileName)
        else Result := ImportFromIni(FileName);
    end
    else
    begin
      Result := False;
      FFileName := '';
    end;

    UpdateFileParameters;   // I4688
  finally
    FState := psReady;
  end;
end;

procedure TProject.Log(AState: TProjectLogState; Filename, Msg: string; MsgCode, line: Integer);
begin
  // Do nothing
end;

procedure TProject.Refresh;   // I4687
begin
  DoRefresh;
end;

function TProject.Render: WideString;
var
  doc, userdoc, xsl: IXMLDomDocument;
  FLastDir: string;
  i: Integer;
  saver: TProjectSaver;
  xml, xmluser: string;
begin
  FState := psSaving;
  try
    saver := TProjectSaver.Create(Self, '');
    try
      saver.Execute;
      xml := saver.XML;
      xmluser := saver.XMLUser;
    finally
      saver.Free;
    end;
  finally
    FState := psReady;
  end;

  Result := '';
  FLastDir := GetCurrentDir;
  SetCurrentDir(StringsTemplatePath);
  try
    doc := MSXMLDOMDocumentFactory.CreateDOMDocument;
    try
      doc.async := False;
      doc.loadXML(xml);

      //
      // Inject the user settings to the loaded file
      //

      userdoc := MSXMLDOMDocumentFactory.CreateDOMDocument;
      try
        userdoc.async := False;
        userdoc.loadXML(xmluser);

        for i := 0 to userdoc.documentElement.childNodes.length - 1 do
          doc.documentElement.appendChild(userdoc.documentElement.childNodes.item[i].cloneNode(true));
      finally
        userdoc := nil;
      end;

      //
      // Inject state URLs into the loaded file
      //

      TProjectUrlRenderer.AddUrls(doc.documentElement);
      TProjectUrlRenderer.AddProcessState(doc.documentElement);

      xsl := MSXMLDOMDocumentFactory.CreateDOMDocument;
      try
        xsl.async := False;
        xsl.resolveExternals := True;
        xsl.validateOnParse := False;
        xsl.load(StringsTemplatePath + 'project.xsl');
        Result := doc.transformNode(xsl);
      finally
        xsl := nil;
      end;
    finally
      doc := nil;
    end;
  finally
    SetCurrentDir(FLastDir);
  end;
end;

function ReadUtf8FileText(const filename: string): string;
var
  ss: TStringStream;
begin
  ss := TStringStream.Create('', TEncoding.UTF8);
  try
    ss.LoadFromFile(filename);
    Result := ss.DataString;
  finally
    ss.Free;
  end;
end;

function IsKeymanFile(filename: string): Boolean;
begin
  filename := filename.ToLower;
  Result :=
    filename.EndsWith('.model.ts') or
//  filename.EndsWith('.kpj') or
    filename.EndsWith('.kmn') or
    filename.EndsWith('.xml') or
    filename.EndsWith('.kps') or
    filename.EndsWith('.kvks') or
    filename.EndsWith('.keyman-touch-layout');
end;

function TProject.IsDefaultProject(Version: TProjectVersion): Boolean;
begin
  Result := FOptions.EqualsRecord(DefaultProjectOptions[Version]);
end;

///
/// Adds all files in project folder to the in-memory project data
/// @param projectFilename Full path to project.kpj (even if the file doesn't exist)
///
///
function TProject.PopulateFiles: Boolean;
var
  SourcePath, ProjectPath: string;
begin
  if FOptions.Version <> pv20 then
    raise EProjectLoader.Create('PopulateFiles can only be called on a v2.0 project');

  FFiles.Clear;

  ProjectPath := ExpandFileName(ExtractFilePath(FileName));
  if not DirectoryExists(ProjectPath) then
    Exit(False);

  PopulateFolder(ProjectPath);
  SourcePath := ResolveProjectPath(FOptions.SourcePath);
  if not SameFileName(ProjectPath, SourcePath) and DirectoryExists(SourcePath) then
    PopulateFolder(SourcePath);

  Result := True;
end;

procedure TProject.PopulateFolder(const path: string);
var
  ff: string;
  f: TSearchRec;
begin
  if FindFirst(path + '*', 0, f) = 0 then
  begin
    repeat
      ff := path + f.Name;

      if (f.Name = '.') or (f.Name = '..') then
      begin
        Continue;
      end;

      CreateProjectFile(Self, ff, nil);
    until FindNext(f) <> 0;
    System.SysUtils.FindClose(f);
  end;
end;

function TProject.CanUpgrade: Boolean;
var
  i: Integer;
  Path: string;
  SourcePath, SourceFile: string;
begin
  if FOptions.Version = pv20 then
  begin
    Exit(False);
  end;

  FUpgradeMessages.Clear;
  Result := True;

  // Things that block upgrade:
  // 1. invalid paths in Options
  // 2. contained file paths outside the project folder (primary files only)
  // 3. primary source files in different folders

  if Options.BuildPath.Contains('$SOURCEPATH') then
  begin
    Result := False;
    FUpgradeMessages.Add('The BuildPath project setting contains the "$SOURCEPATH" tag, which is no longer supported');
  end;
  if Options.BuildPath.Contains('$VERSION') then
  begin
    Result := False;
    FUpgradeMessages.Add('The BuildPath project setting contains the "$VERSION" tag, which is no longer supported');
  end;

  SourcePath := '?';
  SourceFile := '';

  for i := 0 to Files.Count - 1 do
  begin
    if Assigned(Files[i].Parent) then
    begin
      Continue;
    end;

    Path := ExtractRelativePath(FileName, Files[i].FileName);

    // Ensure all compileable files
    if Files[i].IsCompilable then
    begin
      if SourcePath = '?' then
      begin
        SourcePath := ExtractFileDir(Path);
        SourceFile := Path;
      end
      else if not SameFileName(SourcePath, ExtractFileDir(Path)) then
      begin
        FUpgradeMessages.Add('File '+Files[i].FileName+' is not in the same folder as '+SourceFile+
          '. All primary source files must be in the same folder.');
        Result := False;
      end;
    end;

    if IsRelativePath(Path) and not Path.StartsWith('..') then
    begin
      // Path is in same folder or a subfolder of the project
      Continue;
    end;

    FUpgradeMessages.Add('File '+Files[i].FileName+' is outside the project folder ('+ExtractFileDir(Filename)+
      '). All primary source files must be in the same folder as the project file, or in the same subfolder.');
    Result := False;
  end;
end;

function TProject.Upgrade: Boolean;
var
  i: Integer;
begin
  if Options.Version = pv20 then
    raise Exception.Create('Unexpected: Upgrade was called when already version 2.0');

  if not CanUpgrade then
    raise Exception.Create('Unexpected: Upgrade was called when CanUpgrade=False');

  Options.Version := pv20;

  // Set location of all source files, default to 'source' if no source files
  // are present in the project
  Options.SourcePath := '$PROJECTPATH\source';
  for i := 0 to Files.Count - 1 do
  begin
    if Files[i].IsCompilable then
    begin
      Options.SourcePath := '$PROJECTPATH\' + ExtractFileDir(ExtractRelativePath(FFileName, Files[i].FileName));
      Break;
    end;
  end;

  for i := Files.Count - 1 downto 0 do
  begin
    if Assigned(Files[i].Parent) then
    begin
      Files.Delete(i);
    end;
  end;

  Save;

  PopulateFiles;

  Result := True;
end;

function TProject.LoadFromXML(FileName: string): Boolean;
begin
  with TProjectLoader.Create(Self, FileName) do
  try
    Execute;
  finally
    Free;
  end;
  Result := True;
end;

procedure TProject.MRUChange(Sender: TObject);
begin
  if FState = psReady then
  begin
    Save;
    Refresh;
  end;
end;

function TProject.ExpandMemberFileName(Root, FileName: string): string;
var
  buf: array[0..260] of char;
  p: PChar;
begin
  Result := ExpandFileNameEx(ExtractFilePath(Root), FileName);
  if GetFullPathName(PChar(Result), 260, buf, p) > 0 then Result := buf;
end;

function TProject.ImportFromIni(FileName: string): Boolean;
var
  s: TStringList;
  i: Integer;
  FRootPath: string;
begin
  with TStringStream.Create do   // I4729
  try
    LoadFromFile(FileName);
    if Copy(DataString, 1, 1) <> '[' then
      raise EProjectLoader.Create('Not a valid project file');
  finally
    Free;
  end;

  FRootPath := ExtractFilePath(FileName);
  s := TStringList.Create;
  with TIniFile.Create(FileName) do
  try
    if not SectionExists('Files') then   // I4729
      raise EProjectLoader.Create('Not a valid project file');

    ReadSection('Files', s);
    for i := 0 to s.Count - 1 do
      CreateProjectFile(Self, ExpandMemberFileName(FileName, s[i]), nil);

    ReadSection('MRU', s);
    for i := 0 to s.Count - 1 do
      MRU.Append(s[i]);
  finally
    Free;
    s.Free;
  end;
  Result := True;
end;

function TProject.Save: Boolean;
begin
  FState := psSaving;
  try
    with TProjectSaver.Create(Self, FileName) do
    try
      Execute;
    finally
      Free;
    end;
  finally
    FState := psReady;
  end;

  Result := True;
end;

function TProject.SaveUser: Boolean;
begin
  FState := psSaving;
  try
    with TProjectSaver.Create(Self, FileName) do
    try
      SaveUser;
    finally
      Free;
    end;
  finally
    FState := psReady;
  end;

  Result := True;
end;

function TProject.GetUserFileName: string;
begin
  Result := ChangeFileExt(FileName, Ext_ProjectSourceUser);
end;

function TProject.ResolveProjectPath(APath: string): string;
begin
  Result := IncludeTrailingPathDelimiter(ReplaceText(APath, '$PROJECTPATH', ExtractFileDir(ExpandFileName(FFileName))));
end;

function TProject.ResolveSourcePath: string;
begin
  if FOptions.Version = pv10
    then Result := ExtractFilePath(FFileName)
    else Result := ResolveProjectPath(FOptions.SourcePath);
end;

function TProject.GetTargetFilename10(ATargetFile, ASourceFile, AVersion: string): string;   // I4688
begin
  Result := Trim(Options.BuildPath);
  if Result = '' then Result := '$SOURCEPATH';
  Result := IncludeTrailingPathDelimiter(Result);

  // Replace placeholders in the target path
  Result := ReplaceText(Result, '$SOURCEPATH', ExtractFileDir(ExpandFileName(ASourceFile)));
  Result := ReplaceText(Result, '$PROJECTPATH', ExtractFileDir(ExpandFileName(FFileName)));
  Result := ReplaceText(Result, '$VERSION', AVersion);

  Result := Result + ExtractFileName(ATargetFile);
end;

function TProject.GetTargetFilename20(ATargetFile, ASourceFile, AVersion: string): string;   // I4688
begin
  Result := Trim(Options.BuildPath);
  if Result = '' then
  begin
    Exit(ExtractFilePath(ExpandFileName(ASourceFile)) + ExtractFileName(ATargetFile));
  end;

  Result := ResolveProjectPath(Result);
  Result := Result + ExtractFileName(ATargetFile);
end;

function TProject.GetTargetFilename(ATargetFile, ASourceFile, AVersion: string): string;   // I4688
begin
  if Options.Version = pv10
    then Result := GetTargetFilename10(ATargetFile, ASourceFile, AVersion)
    else Result := GetTargetFilename20(ATargetFile, ASourceFile, AVersion);
end;

class function TProject.StandardTemplatePath: string; //(const FileName: string): string;
begin
  Result := StringsTemplatePath; // GetXMLTemplatePath + 'project\';
end;

class function TProject.StringsTemplatePath: string;
begin
  Result := GetXMLTemplatePath + 'project\';
end;

procedure TProject.UpdateFileParameters;   // I4688   // I4710
var
  i: Integer;
  f: TProjectFile;
begin
  for i := 0 to Files.Count - 1 do
    Files[i].CheckedParameters := False;

  i := 0;
  while i < Files.Count do
  begin
    if not Files[i].CheckedParameters then
    begin
      f := Files[i];
      f.CheckedParameters := True;
      f.GetFileParameters;
      i := 0; // The file list can change
    end
    else
      Inc(i);
  end;
end;

function TProject.FindFile(AFileName: string): TProjectFile;
var
  i: Integer;
begin
  for i := 0 to Files.Count - 1 do
    if SameText(Files[i].FileName, AFileName) then
    begin
      Result := Files[i];
      Exit;
    end;
  Result := nil;
end;

{ TProjectFileStates }

procedure TProjectFileStates.Delete(Index: WideString);
var
  n: Integer;
begin
  n := IndexOfName(Index);
  if n >= 0 then
    inherited Delete(n);
end;

function TProjectFileStates.GetItem(Index: WideString): WideString;
var
  n: Integer;
begin
  n := IndexOfName(Index);
  if n >= 0
    then Result := (inherited GetItem(n) as TProjectFileState).Value
    else Result := '';
end;

function TProjectFileStates.IndexOfName(Name: WideString): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if (inherited GetItem(i) as TProjectFileState).Name = Name then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

procedure TProjectFileStates.SetItem(Index, Value: WideString);
var
  n: Integer;
  pfs: TProjectFileState;
begin
  n := IndexOfName(Index);
  if n >= 0 then (inherited GetItem(n) as TProjectFileState).Value := Value
  else
  begin
    pfs := TProjectFileState.Create;
    pfs.Name := Index;
    pfs.Value := Value;
    Add(pfs);
  end;
end;

{ TProjectOptions }

procedure TProjectOptions.Assign(source: TProjectOptions);
begin
  Self.BuildPath := source.BuildPath;
  Self.SourcePath := source.SourcePath;
  Self.CompilerWarningsAsErrors := source.CompilerWarningsAsErrors;
  Self.WarnDeprecatedCode := source.WarnDeprecatedCode;
  Self.CheckFilenameConventions := source.CheckFilenameConventions;
  Self.SkipMetadataFiles := source.SkipMetadataFiles;
  Self.ProjectType := source.ProjectType;
  Self.Version := Source.Version;
end;

procedure TProjectOptions.Assign(source: TProjectOptionsRecord);
begin
  Self.BuildPath := source.BuildPath;
  Self.SourcePath := source.SourcePath;
  Self.CompilerWarningsAsErrors := source.CompilerWarningsAsErrors;
  Self.WarnDeprecatedCode := source.WarnDeprecatedCode;
  Self.CheckFilenameConventions := source.CheckFilenameConventions;
  Self.SkipMetadataFiles := source.SkipMetadataFiles;
  Self.ProjectType := source.ProjectType;
  Self.Version := Source.Version;
end;

function TProjectOptions.EqualsRecord(source: TProjectOptionsRecord): Boolean;
begin
  Result :=
    (Self.BuildPath = source.BuildPath) and
    (Self.SourcePath = source.SourcePath) and
    (Self.CompilerWarningsAsErrors = source.CompilerWarningsAsErrors) and
    (Self.WarnDeprecatedCode = source.WarnDeprecatedCode) and
    (Self.CheckFilenameConventions = source.CheckFilenameConventions) and
    (Self.SkipMetadataFiles = source.SkipMetadataFiles) and
    (Self.ProjectType = source.ProjectType) and
    (Self.Version = Source.Version);
end;

function ProjectTypeFromString(s: string): TProjectType;
begin
  if SameText(s, 'keyboard') then Result := ptKeyboard
  else if SameText(s, 'lexicalmodel') then Result := ptLexicalModel
  else Result := ptUnknown;
end;

function ProjectTypeToString(pt: TProjectType): string;
begin
  case pt of
    ptUnknown: Result := '';
    ptKeyboard: Result := 'keyboard';
    ptLexicalModel: Result := 'lexicalmodel';
  end;
end;

function ProjectVersionFromString(s: string): TProjectVersion;
begin
  if SameText(s, '1.0') then Result := pv10
  else if SameText(s, '2.0') then Result := pv20
  else Result := pvUnknown;
end;

function ProjectVersionToString(pv: TProjectVersion): string;
begin
  case pv of
    pvUnknown: Result := '';
    pv10: Result := '1.0';
    pv20: Result := '2.0';
  end;
end;

end.
