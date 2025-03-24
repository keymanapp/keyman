(*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Parameters dialog for cloning a project from a GitHub repo
 *)
unit Keyman.Developer.UI.Project.UfrmCloneGitHubProjectParameters;

interface

uses
  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.Variants,
  Winapi.Messages,
  Winapi.Windows,
  Vcl.CheckLst,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Grids,
  Vcl.StdCtrls,

  kpsfile,
  PackageInfo,
  UfrmTike,
  UKeymanTargets,
  UfrmDownloadProgress,
  utilfiletypes,
  Browse4Folder;

type
  TfrmCloneGitHubProjectParameters = class(TTikeForm)
    cmdOK: TButton;
    cmdCancel: TButton;
    gbNewProjectDetails: TGroupBox;
    lblFileName: TLabel;
    lblProjectFilename: TLabel;
    editProjectID: TEdit;
    editProjectFilename: TEdit;
    lblPath: TLabel;
    cmdBrowse: TButton;
    editPath: TEdit;
    chkRelocateExternal: TCheckBox;
    gbSourceProjectDetails: TGroupBox;
    Label1: TLabel;
    editGitHubURL: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    lblSourceProjectLexicalModel: TLabel;
    lblSourceProjectKeyboard: TLabel;
    procedure cmdOKClick(Sender: TObject);
    procedure editSourceProjectFilenameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure editVersionChange(Sender: TObject);
    procedure editGitHubURLChange(Sender: TObject);
    procedure editProjectIDChange(Sender: TObject);
    procedure cmdBrowseClick(Sender: TObject);
  private
    dlgBrowse: TBrowse4Folder;
    frmDownloadProgress: TfrmDownloadProgress;
    function GetBasePath: string;
    function GetProjectID: string;
    function Validate: Boolean;
    procedure EnableControls;
    procedure UpdateProjectFilename;
    function GetProjectFilename: string;
    function GetSourceProjectFilename: string;
    function GetRelocateExternal: Boolean;
    procedure DownloadCallback(Owner: TfrmDownloadProgress; var Result: Boolean);
    procedure DownloadWrapperCallback(var Cancelled: Boolean);
    function IsSourceProjectALexicalModelProject: Boolean;
    procedure UpdateSourceProjectNote;
  protected
    function GetHelpTopic: string; override;
  public
    property BasePath: string read GetBasePath;
    property ProjectID: string read GetProjectID;
    property ProjectFilename: string read GetProjectFilename;
    property SourceProjectFilename: string read GetSourceProjectFilename;
    property RelocateExternal: Boolean read GetRelocateExternal;
  end;

function ShowCloneGitHubProjectParameters(Owner: TComponent): Boolean;

implementation

uses
  System.Net.UrlClient,
  System.RegularExpressions,
  Vcl.ComCtrls,

  KeymanDeveloperOptions,
  Keyman.Developer.System.KmcWrapper,
  Keyman.Developer.System.HelpTopics,
  Keyman.Developer.System.Project.Project,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.System.KeyboardUtils,
  Keyman.System.LexicalModelUtils,
  UfrmMain;

{$R *.dfm}

function ShowCloneGitHubProjectParameters(Owner: TComponent): Boolean;
var
  f: TfrmCloneGitHubProjectParameters;
  path: string;
  p: TfrmDownloadProgress;
begin
  f := TfrmCloneGitHubProjectParameters.Create(Owner);
  try
    Result := f.ShowModal = mrOk;
    if not Result then
      Exit;

    path := ExtractFileDir(f.BasePath);
    if not ForceDirectories(path) then
    begin
      ShowMessage('Unable to create path "'+path+'": '+SysErrorMessage(GetLastError));
      Exit;
    end;

    p := TfrmDownloadProgress.Create(Owner);
    try
      p.Callback := f.DownloadCallback;
      p.progress.Style := pbstMarquee;
      Result := p.ShowModal = mrOk;
    finally
      p.Free;
    end;
  finally
    f.Free;
  end;
end;

{ TfrmCloneGitHubProjectParameters }

procedure TfrmCloneGitHubProjectParameters.DownloadWrapperCallback(var Cancelled: Boolean);
begin
  Application.ProcessMessages;
  Cancelled := frmDownloadProgress.Cancel;
end;

procedure TfrmCloneGitHubProjectParameters.DownloadCallback(
  Owner: TfrmDownloadProgress; var Result: Boolean);
var
  wrapper: TKmcWrapper;
  path: string;
begin
  Result := False;
  path := ExtractFileDir(Self.BasePath);

  Update;
  frmDownloadProgress := Owner;
  wrapper := TKmcWrapper.Create;
  try
    if wrapper.Copy(Self.SourceProjectFilename, Self.ProjectFilename, path, Self.RelocateExternal, DownloadWrapperCallback) then
    begin
      frmKeymanDeveloper.OpenProject(Self.ProjectFilename);
      Result := True;
    end;
  finally
    wrapper.Free;
  end;
  frmDownloadProgress := nil;
end;

procedure TfrmCloneGitHubProjectParameters.FormCreate(Sender: TObject);
begin
  inherited;
  editPath.Text := FKeymanDeveloperOptions.DefaultProjectPath;

  dlgBrowse := TBrowse4Folder.Create(Self);
  dlgBrowse.InitialDir := editPath.Text;
  dlgBrowse.Options := [OnlySelectFileSysDir, ShowEditBox, UseNewDialogStyle];
  dlgBrowse.Root := Desktop;
  dlgBrowse.Title := 'Select folder to save project to';

  UpdateSourceProjectNote;
  EnableControls;
end;

procedure TfrmCloneGitHubProjectParameters.cmdBrowseClick(Sender: TObject);
begin
  dlgBrowse.InitialDir := editPath.Text;

  if dlgBrowse.Execute and (dlgBrowse.FileName <> '') then
    editPath.Text := ExcludeTrailingPathDelimiter(dlgBrowse.FileName);
end;

procedure TfrmCloneGitHubProjectParameters.cmdOKClick(Sender: TObject);
begin
  if Validate then
    ModalResult := mrOk;
end;

procedure TfrmCloneGitHubProjectParameters.UpdateSourceProjectNote;
begin
  lblSourceProjectLexicalModel.Visible := IsSourceProjectALexicalModelProject;
  lblSourceProjectKeyboard.Visible := (editGitHubURL.Text <> '') and not IsSourceProjectALexicalModelProject;
end;

procedure TfrmCloneGitHubProjectParameters.editProjectIDChange(Sender: TObject);
begin
  UpdateProjectFilename;
  EnableControls;
end;

procedure TfrmCloneGitHubProjectParameters.editSourceProjectFilenameChange(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmCloneGitHubProjectParameters.editGitHubURLChange(Sender: TObject);
begin
  UpdateProjectFilename;
  UpdateSourceProjectNote;
  EnableControls;
end;

procedure TfrmCloneGitHubProjectParameters.editVersionChange(Sender: TObject);
begin
  EnableControls;
end;

function IsValidGitHubProjectURL(const uri: string): Boolean;
const
  // From github-urls.ts (minor RE tweaks to match required format for PCRE

  {**
   * Matches only a GitHub permanent raw URI with a commit hash, without any other
   * components; note hash is called branch to match other URI formats
   *}
  GITHUB_STABLE_SOURCE = '^https:\/\/github\.com\/(?<owner>[a-zA-Z0-9-]+)\/(?<repo>[\w\.-]+)\/raw\/(?<branch>[a-f0-9]{40})\/(?<path>.+)$';

  {**
   * Matches any GitHub git resource raw 'user content' URI which can be
   * translated to a permanent URI with a commit hash
   *}
  GITHUB_RAW_URI = '^https:\/\/raw\.githubusercontent\.com\/(?<owner>[a-zA-Z0-9-]+)\/(?<repo>[\w\.-]+)\/(?:refs\/(?:heads|tags)\/)?(?<branch>[^\/]+)\/(?<path>.+)$';

  {**
   * Matches any GitHub git resource raw URI which can be translated to a
   * permanent URI with a commit hash
   *}
  GITHUB_URI = '^https:\/\/github\.com\/(?<owner>[a-zA-Z0-9-]+)\/(?<repo>[\w\.-]+)\/(?:raw|blob|tree)\/(?:refs\/(?:heads|tags)\/)?(?<branch>[^\/]+)\/(?<path>.+)$';

  {**
   * Matches any GitHub git resource raw URI which can be translated to a
   * permanent URI with a commit hash, with the http[s] protocol optional, for
   * matching user-supplied URLs. groups are: `owner`, `repo`, `branch`, and
   * `path`.
   *}
  GITHUB_URI_OPTIONAL_PROTOCOL = '^(?:http(?:s)?:\/\/)?github\.com\/(?<owner>[a-zA-Z0-9-]+)\/(?<repo>[\w\.-]+)(?:\/(?:(?:raw|blob|tree)\/(?:refs\/(?:heads|tags)\/)?(?<branch>[^\/]+)\/(?<path>.*))?)?$';

var
  opt: TRegEx;
  raw: TRegEx;
begin
  opt := TRegEx.Create(GITHUB_URI_OPTIONAL_PROTOCOL);
  raw := TRegEx.Create(GITHUB_RAW_URI);

  Result := opt.IsMatch(uri) or raw.IsMatch(uri);
end;

procedure TfrmCloneGitHubProjectParameters.EnableControls;
var
  e: Boolean;
  id: string;
begin
  e :=
    IsValidGitHubProjectURL(editGitHubURL.Text) and
    (Trim(editPath.Text) <> '') and
    (Trim(editProjectID.Text) <> '');

  if e then
  begin
    id := Trim(editProjectID.Text);
    if IsSourceProjectALexicalModelProject
      then e := TLexicalModelUtils.IsValidLexicalModelID(id, True)
      else e := TKeyboardUtils.IsValidKeyboardID(id, True);
  end;

  cmdOK.Enabled := e;
end;

function TfrmCloneGitHubProjectParameters.GetBasePath: string;
begin
  Result := Trim(editPath.Text);
end;

function TfrmCloneGitHubProjectParameters.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_CloneGitHubProjectParameters;
end;

function TfrmCloneGitHubProjectParameters.GetProjectID: string;
begin
  Result := Trim(LowerCase(editProjectID.Text));
end;

function TfrmCloneGitHubProjectParameters.GetProjectFilename: string;
begin
  Result := editProjectFilename.Text;
end;

function TfrmCloneGitHubProjectParameters.GetRelocateExternal: Boolean;
begin
  Result := chkRelocateExternal.Checked;
end;

function TfrmCloneGitHubProjectParameters.GetSourceProjectFilename: string;
begin
  Result := editGitHubURL.Text;
end;

function TfrmCloneGitHubProjectParameters.IsSourceProjectALexicalModelProject: Boolean;
var
  components: TArray<string>;
  sourceId: string;
begin
  sourceId := editGitHubURL.Text;
  components := sourceId.Split(['\','/']);
  if Length(components) = 0 then
    Exit(False);
  sourceId := components[Length(components) - 1];
  sourceId := ChangeFileExt(sourceId, '').ToLower;
  Result := TLexicalModelUtils.IsValidLexicalModelID(sourceId, False);
end;

function TfrmCloneGitHubProjectParameters.Validate: Boolean;
var
  ProjectFolder: string;
begin
  if IsSourceProjectALexicalModelProject
    then Result := TLexicalModelUtils.IsValidLexicalModelID(Trim(editProjectID.Text), True)
    else Result := TKeyboardUtils.IsValidKeyboardID(Trim(editProjectID.Text), True);

  if Result then
  begin
    if not DirectoryExists(editPath.Text) then
    begin
      if MessageDlg('The target folder '+editPath.Text+' does not exist. Create it now?', mtConfirmation, mbOkCancel, 0) = mrCancel then
        Exit(False);
    end;

    ProjectFolder := IncludeTrailingPathDelimiter(editPath.Text) + editProjectID.Text;
    if DirectoryExists(ProjectFolder) then
    begin
      if MessageDlg('The project folder '+ProjectFolder+' already exists. Are you sure you want to overwrite it?', mtWarning,
          mbOkCancel, 0) = mrCancel then
        Exit(False);
    end;
  end;
end;

procedure TfrmCloneGitHubProjectParameters.UpdateProjectFilename;
begin
  editProjectFilename.Text :=
    IncludeTrailingPathDelimiter(BasePath) +
    ProjectID + PathDelim +
    ProjectID + Ext_ProjectSource;
  // Scroll to the end of the control to show the filename
  editProjectFilename.Perform(EM_SETSEL, Length(editProjectFilename.Text), Length(editProjectFilename.Text));
  editProjectFilename.Perform(EM_SCROLLCARET, 0, 0);
end;

end.
