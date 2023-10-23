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
  { Forward declarations }

  IProjectFileFreeNotification = interface;

  TProjectFileList = class;
  TProjectFile = class;
  TProjectFileStates = class;
  TProjectOptions = class;

  TProjectState = (psCreating, psReady, psLoading, psSaving, psDestroying);
  TProjectType = (ptUnknown, ptKeyboard, ptLexicalModel); // distinct from utilfiletypes.TKeymanProjectType

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

    procedure SetFileName(Value: string);
    function ImportFromIni(FileName: string): Boolean;
    function LoadFromXML(FileName: string): Boolean;
    function ExpandMemberFileName(Root, FileName: string): string;
    function GetSavedFileName: string;
    function GetUntitled: Boolean;
    //procedure ChildRefresh(Sender: TObject);
    procedure ListNotify(Item: TProjectFile; Action: TListNotification);
    procedure ChildRefresh(Sender: TObject);
    procedure MRUChange(Sender: TObject);
    procedure UpdateFileParameters;
    procedure LoadPersistedUntitledProject;
    function GetSavedUserFileName: string;
  protected
    procedure DoRefresh; virtual;
    procedure DoRefreshCaption; virtual;

    property State: TProjectState read FState;
    property SavedFileName: string read GetSavedFileName;
    property SavedUserFileName: string read GetSavedUserFileName;

  public
    procedure Log(AState: TProjectLogState; Filename, Msg: string; MsgCode, line: Integer); virtual;

    constructor Create(AProjectType: TProjectType; AFileName: string; ALoadPersistedUntitledProject: Boolean = False); virtual;
    destructor Destroy; override;

    procedure Refresh;

    procedure PersistUntitledProject;

    function Render: WideString;

    function Load: Boolean; virtual;   // I4694
    function Save: Boolean; virtual;   // I4694
    function SaveUser: Boolean; virtual;   // I4694

    class function StandardTemplatePath: string;
    class function StringsTemplatePath: string;

    class function GetUntitledProjectFilename(CurrentProcess: Boolean): string;

    function GetTargetFilename(ATargetFile, ASourceFile, AVersion: string): string;   // I4688

    //procedure AddMRU(const FFileName: string);
    property MRU: TMRUList read FMRU;

    property Options: TProjectOptions read FOptions;   // I4688

    function FindFile(AFileName: string): TProjectFile;

    //property StandardTemplatePath: string read GetStandardTemplatePath;
    property FileName: string read FFileName write SetFileName;
    property Untitled: Boolean read GetUntitled;

    property Files: TProjectFileList read FFiles;
    property DisplayState: WideString read FDisplayState write FDisplayState;

    property Busy: Boolean read FBusy write FBusy;
    property MustSave: Boolean read FMustSave write FMustSave;

  public
    class var CompilerMessageFile: TProjectFile;   // I4694
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

    procedure Load(node: IXMLNode; LoadState: Boolean); virtual;   // I4698
    procedure LoadState(node: IXMLNode); virtual;   // I4698
    procedure Save(node: IXMLNode; SaveState: Boolean); virtual;   // I4698
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

  TProjectOptions = class   // I4688
  private
    FBuildPath: string;
    FWarnDeprecatedCode: Boolean;   // I4866
    FCompilerWarningsAsErrors: Boolean;   // I4865
    FCheckFilenameConventions: Boolean;
    FProjectType: TProjectType;
  public
    constructor Create;
    property BuildPath: string read FBuildPath write FBuildPath;
    property WarnDeprecatedCode: Boolean read FWarnDeprecatedCode write FWarnDeprecatedCode;   // I4866
    property CompilerWarningsAsErrors: Boolean read FCompilerWarningsAsErrors write FCompilerWarningsAsErrors;   // I4865
    property CheckFilenameConventions: Boolean read FCheckFilenameConventions write FCheckFilenameConventions;
    property ProjectType: TProjectType read FProjectType write FProjectType;
  end;

const
  WM_USER_ProjectUpdateDisplayState = WM_USER;

function GlobalProjectStateWndHandle: THandle;
function ProjectCompilerMessage(line: Integer; msgcode: LongWord; text: PAnsiChar): Integer; stdcall;  // I3310   // I4694
function ProjectCompilerMessageW(line: Integer; msgcode: LongWord; const text: string): Integer;
procedure ProjectCompilerMessageClear;

function ProjectTypeFromString(s: string): TProjectType;
function ProjectTypeToString(pt: TProjectType): string;

implementation

uses
  Winapi.ShlObj,
  System.AnsiStrings,
  System.IniFiles,
  System.DateUtils,
  System.Hash,
  System.StrUtils,
  System.Variants,

  CompileErrorCodes,
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
      Result := FGlobalProject;
  end;
end;

procedure TProjectFile.Load(node: IXMLNode; LoadState: Boolean);   // I4698
var
  i: Integer;
begin
  if node.ChildNodes.IndexOf('ID') >= 0 then
    FID := CleanID(VarToWideStr(node.ChildValues['ID']));
  if node.ChildNodes.IndexOf('ParentFileID') >= 0 then
    FParentFileID := CleanID(VarToWideStr(node.ChildValues['ParentFileID']));

  if LoadState then
  begin
    FIDEState.Clear;
    if node.ChildNodes.IndexOf('IDEState') >= 0 then
    begin
      node := node.ChildNodes.Nodes['IDEState'];
      for i := 0 to node.ChildNodes.Count - 1 do
        FIDEState[node.ChildNodes[i].NodeName] := node.ChildNodes[i].NodeValue;
    end;
  end;
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

procedure TProjectFile.Save(node: IXMLNode; SaveState: Boolean);   // I4698
var
  I: Integer;
begin
  node.AddChild('ID').NodeValue := FID;
  node.AddChild('Filename').NodeValue := ExtractFileName(FFileName);
  node.AddChild('Filepath').NodeValue := ExtractRelativePath(FProject.FileName, FFileName);
  node.AddChild('FileVersion').NodeValue := FFileVersion;   // I4701
  node.AddChild('FileType').NodeValue := ExtractFileExt(FFileName);;
  if Assigned(FParent) then
    node.AddChild('ParentFileID').NodeValue := FParent.ID;

  if SaveState then
  begin
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

constructor TProject.Create(AProjectType: TProjectType; AFileName: string; ALoadPersistedUntitledProject: Boolean = False);
var
  i: Integer;
begin
  FOptions := TProjectOptions.Create;   // I4688

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

  if (FFileName = '') and (ALoadPersistedUntitledProject) then
  begin
    FMustSave := True;
    LoadPersistedUntitledProject; // I1010: Persist untitled project
  end
  else if not Load then   // I4703
  begin
    FFileName := '';
    if not Load then
    begin
      FMustSave := True;
      LoadPersistedUntitledProject;   // I4703
    end;
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
  FreeAndNil(FOptions);   // I4688
  inherited Destroy;
end;

procedure TProject.DoRefresh;   // I4687
begin
end;

procedure TProject.DoRefreshCaption;   // I4687
begin
end;

procedure TProject.SetFileName(Value: string);
begin
  FFileName := Value;
  DoRefreshCaption;   // I4687
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
  FState := psLoading;
  try
    SetLength(buf, 32);
    if FileExists(SavedFileName) then
    begin
      with TFileStream.Create(SavedFileName, fmOpenRead) do
      try
        Read(buf[0], 32);  // I3310
      finally
        Free;
      end;

      if (buf[0] = Ord('<')) or ((TEncoding.GetBufferEncoding(buf, encoding) > 0) and (encoding = TEncoding.UTF8))  // I3310, I3473
        then Result := LoadFromXML(SavedFileName)
        else Result := ImportFromIni(SavedFileName);
    end
    else if DirectoryExists(ExtractFilePath(SavedFileName)) then
      Result := Save { Create a temporary project file }
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

// I1010: Persist untitled project - begin

procedure TProject.LoadPersistedUntitledProject;
begin
  FFileName := TProject.GetUntitledProjectFilename(False);
  try
    Load;
  finally
    FFileName := '';
  end;
end;

procedure TProject.Log(AState: TProjectLogState; Filename, Msg: string; MsgCode, line: Integer);
begin
  // Do nothing
end;

procedure TProject.PersistUntitledProject;
var
  path: string;
begin
  path := TProject.GetUntitledProjectFilename(False);

  FState := psSaving;
  with TProjectSaver.Create(Self, path) do
  try
    Execute;
  finally
    Free;
  end;

  FState := psReady;
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
begin
  if not FileExists(SavedFileName) then Save;

  Result := '';
  FLastDir := GetCurrentDir;
  SetCurrentDir(StringsTemplatePath);
  try
    doc := MSXMLDOMDocumentFactory.CreateDOMDocument;
    try
      doc.async := False;
      doc.load(SavedFileName);

      //
      // Inject the user settings to the loaded file
      //

      if FileExists(SavedUserFileName) then   // I4698
      begin
        userdoc := MSXMLDOMDocumentFactory.CreateDOMDocument;
        try
          userdoc.async := False;
          userdoc.load(SavedUserFileName);

          for i := 0 to userdoc.documentElement.childNodes.length - 1 do
            doc.documentElement.appendChild(userdoc.documentElement.childNodes.item[i].cloneNode(true));
        finally
          userdoc := nil;
        end;
      end;

      //
      // Inject state URLs into the loaded file
      //

      TProjectUrlRenderer.AddUrls(doc.documentElement);

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

// I1010: Persist untitled project - end

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
    with TProjectSaver.Create(Self, SavedFileName) do
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
    with TProjectSaver.Create(Self, SavedFileName) do
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

function TProject.GetSavedFileName: string;
begin
  if FFileName = ''
    then Result := TProject.GetUntitledProjectFilename(True)   // I4181
    else Result := FFileName;
end;

function TProject.GetSavedUserFileName: string;
begin
  Result := ChangeFileExt(SavedFileName, Ext_ProjectSourceUser);
end;

function TProject.GetTargetFilename(ATargetFile, ASourceFile, AVersion: string): string;   // I4688
begin
  Result := Trim(Options.BuildPath);
  if Result = '' then Result := '$SOURCEPATH';
  Result := IncludeTrailingPathDelimiter(Result);

  // Replace placeholders in the target path
  Result := ReplaceText(Result, '$SOURCEPATH', ExtractFileDir(ExpandFileName(ASourceFile)));
  if FFileName = '' // if we have an unsaved project, use the source path for project path
    then Result := ReplaceText(Result, '$PROJECTPATH', ExtractFileDir(ExpandFileName(ASourceFile)))
    else Result := ReplaceText(Result, '$PROJECTPATH', ExtractFileDir(ExpandFileName(FFileName)));
  Result := ReplaceText(Result, '$VERSION', AVersion);

  Result := Result + ExtractFileName(ATargetFile);
end;

class function TProject.StandardTemplatePath: string; //(const FileName: string): string;
begin
  Result := StringsTemplatePath; // GetXMLTemplatePath + 'project\';
end;

class function TProject.StringsTemplatePath: string;
begin
  Result := GetXMLTemplatePath + 'project\';
end;

class function TProject.GetUntitledProjectFilename(CurrentProcess: Boolean): string;
begin
  if CurrentProcess
    then Result := GetFolderPath(CSIDL_APPDATA) + SFolderKeymanDeveloper + '\Untitled.' + IntToStr(GetCurrentProcessId) + Ext_ProjectSource
    else Result := GetFolderPath(CSIDL_APPDATA) + SFolderKeymanDeveloper + '\Untitled' + Ext_ProjectSource;
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

function TProject.GetUntitled: Boolean;
begin
  Result := FFileName = '';
end;

{procedure TProject.ChildRefresh(Sender: TObject);
begin
  if FState = psReady then
  begin
    Save;
    Refresh;
  end;
end;}

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

const
  MAX_MESSAGES = 100;

var
  MessageCount: Integer = 0;

procedure ProjectCompilerMessageClear;
begin
  MessageCount := 0;
end;

function ProjectCompilerMessage(line: Integer; msgcode: LongWord; text: PAnsiChar): Integer; stdcall;  // I3310   // I4694
begin
  Result := ProjectCompilerMessageW(line, msgcode, String_AtoU(text));
end;

function ProjectCompilerMessageW(line: Integer; msgcode: LongWord; const text: string): Integer;  // I3310   // I4694
const // from compile.pas
  CERR_FATAL   = $00008000;
  CERR_ERROR   = $00004000;
  CERR_WARNING = $00002000;
  CERR_HINT    = $00001000;
  CWARN_Info =   $0000208A;
var
  FLogState: TProjectLogState;
begin
  FLogState := plsInfo;

  if msgcode <> CWARN_Info then
    case msgcode and $F000 of
      CERR_HINT:    FLogState := plsHint;
      CERR_WARNING: FLogState := plsWarning;
      CERR_ERROR:   FLogState := plsError;
      CERR_FATAL:   FLogState := plsFatal;
    end;

  if FLogState = plsWarning then   // I4706
    TProject.CompilerMessageFile.FHasWarning := True;

  if FLogState in [plsWarning, plsError, plsFatal] then
  begin
    Inc(MessageCount);
    if MessageCount > MAX_MESSAGES then
      Exit(1);
  end;

  TProject.CompilerMessageFile.Log(FLogState, text, msgcode, line);   // I4706

  if (FLogState <> plsInfo) and (MessageCount = MAX_MESSAGES) then
    TProject.CompilerMessageFile.Log(
      plsWarning,
      Format('More than %d warnings or errors received; suppressing further messages', [MAX_MESSAGES]),
      CWARN_TooManyErrorsOrWarnings,
      line);

  Result := 1;
end;

{ TProjectOptions }

constructor TProjectOptions.Create;
begin
  WarnDeprecatedCode := True;   // I4866
  CompilerWarningsAsErrors := False;   // I4865
  CheckFilenameConventions := True; // default to TRUE for new projects
  ProjectType := ptKeyboard;
end;

type
  TGlobalProjectStateWnd = class
  private
    procedure WndProc(var Message: TMessage);
    constructor Create;
    destructor Destroy; override;
  end;

var
  FGlobalProjectStateWnd: TGlobalProjectStateWnd = nil;

  // Make this a global to prevent potential race
  // condition causing an access violation. If it
  // is an invalid window handle or 0 at destruction time,
  // it's no big deal...
  FGlobalProjectStateWndHandle: THandle = 0;

{ TGlobalProjectStateWnd }

constructor TGlobalProjectStateWnd.Create;
begin
  inherited Create;
  FGlobalProjectStateWndHandle := AllocateHWnd(WndProc);
end;

destructor TGlobalProjectStateWnd.Destroy;
var
  h: THandle;
begin
  h := FGlobalProjectStateWndHandle;
  FGlobalProjectStateWndHandle := 0;
  DeallocateHWnd(h);
  inherited Destroy;
end;

procedure TGlobalProjectStateWnd.WndProc(var Message: TMessage);
var
  PPath, PDisplayState: PChar;
begin
  if Message.Msg = WM_USER_ProjectUpdateDisplayState then
  begin
    PPath := PChar(Message.WParam);
    PDisplayState := PChar(Message.LParam);
    if Assigned(FGlobalProject) and (FGlobalProject.FileName = PPath) then
    begin
      FGlobalProject.DisplayState := PDisplayState;
      FGlobalProject.SaveUser;
    end;
    StrDispose(PDisplayState);
    StrDispose(PPath);
  end;
  DefWindowProc(FGlobalProjectStateWndHandle, Message.Msg, Message.WParam, Message.LParam);
end;

function GlobalProjectStateWndHandle: THandle;
begin
  Result := FGlobalProjectStateWndHandle;
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

initialization
  FGlobalProjectStateWnd := TGlobalProjectStateWnd.Create;
finalization
  // Deletes temporary session-local project
  if FileExists(TProject.GetUntitledProjectFilename(True)) then
    System.SysUtils.DeleteFile(TProject.GetUntitledProjectFilename(True));
  if FileExists(ChangeFileExt(TProject.GetUntitledProjectFilename(True),Ext_ProjectSourceUser)) then
    System.SysUtils.DeleteFile(ChangeFileExt(TProject.GetUntitledProjectFilename(True),Ext_ProjectSourceUser));

  FGlobalProjectStateWnd.Free;
end.
