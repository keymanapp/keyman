(*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Parameters dialog for cloning a project from local filesystem
 *)
unit Keyman.Developer.UI.Project.UfrmCloneLocalProjectParameters;

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
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Grids,
  Vcl.StdCtrls,

  kpsfile,
  PackageInfo,
  UfrmTike,
  UKeymanTargets,
  utilfiletypes,
  Browse4Folder;

type
  TfrmCloneLocalProjectParameters = class(TTikeForm)
    cmdOK: TButton;
    cmdCancel: TButton;
    dlgBrowseSourceProject: TFileOpenDialog;
    gbNewProjectDetails: TGroupBox;
    lblFileName: TLabel;
    lblProjectFilename: TLabel;
    lblPath: TLabel;
    editProjectID: TEdit;
    editProjectFilename: TEdit;
    cmdBrowse: TButton;
    editPath: TEdit;
    chkRelocateExternal: TCheckBox;
    gbSourceProjectDetails: TGroupBox;
    lblKeyboardName: TLabel;
    editSourceProjectFilename: TEdit;
    cmdBrowseSourceProject: TButton;
    lblSourceProjectLexicalModel: TLabel;
    lblSourceProjectKeyboard: TLabel;
    procedure cmdOKClick(Sender: TObject);
    procedure editSourceProjectFilenameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure editVersionChange(Sender: TObject);
    procedure editPathChange(Sender: TObject);
    procedure editProjectIDChange(Sender: TObject);
    procedure cmdBrowseClick(Sender: TObject);
    procedure cmdBrowseSourceProjectClick(Sender: TObject);
  private
    dlgBrowse: TBrowse4Folder;
    function GetBasePath: string;
    function GetProjectID: string;
    function Validate: Boolean;
    procedure EnableControls;
    procedure UpdateProjectFilename;
    function GetProjectFilename: string;
    function GetSourceProjectFilename: string;
    function GetRelocateExternal: Boolean;
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

function ShowCloneLocalProjectParameters(Owner: TComponent): Boolean;

implementation

uses
  KeymanDeveloperOptions,
  Keyman.Developer.System.KmcWrapper,
  Keyman.Developer.System.HelpTopics,
  Keyman.Developer.System.Project.Project,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.System.KeyboardUtils,
  Keyman.System.LexicalModelUtils,
  UfrmMain;

{$R *.dfm}

function ShowCloneLocalProjectParameters(Owner: TComponent): Boolean;
var
  f: TfrmCloneLocalProjectParameters;
  wrapper: TKmcWrapper;
begin
  f := TfrmCloneLocalProjectParameters.Create(Owner);
  try
    Result := f.ShowModal = mrOk;
    if not Result then
      Exit;

    wrapper := TKmcWrapper.Create;
    if wrapper.Copy(f.SourceProjectFilename, f.ProjectFilename, ExtractFileDir(f.SourceProjectFilename), f.RelocateExternal) then
    begin
      frmKeymanDeveloper.OpenProject(f.ProjectFilename);
      Result := True;
    end;
  finally
    f.Free;
  end;
end;

{ TfrmCloneLocalProjectParameters }

procedure TfrmCloneLocalProjectParameters.FormCreate(Sender: TObject);
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

procedure TfrmCloneLocalProjectParameters.cmdBrowseClick(Sender: TObject);
begin
  dlgBrowse.InitialDir := editPath.Text;

  if dlgBrowse.Execute and (dlgBrowse.FileName <> '') then
    editPath.Text := ExcludeTrailingPathDelimiter(dlgBrowse.FileName);
end;

procedure TfrmCloneLocalProjectParameters.cmdBrowseSourceProjectClick(
  Sender: TObject);
begin
  if dlgBrowseSourceProject.Execute then
  begin
    editSourceProjectFilename.Text := dlgBrowseSourceProject.Filename;
    UpdateSourceProjectNote;
    EnableControls;
  end;
end;

procedure TfrmCloneLocalProjectParameters.UpdateSourceProjectNote;
begin
  lblSourceProjectLexicalModel.Visible := IsSourceProjectALexicalModelProject;
  lblSourceProjectKeyboard.Visible := (editSourceProjectFilename.Text <> '') and not IsSourceProjectALexicalModelProject;
end;

procedure TfrmCloneLocalProjectParameters.cmdOKClick(Sender: TObject);
begin
  if Validate then
    ModalResult := mrOk;
end;

procedure TfrmCloneLocalProjectParameters.editProjectIDChange(Sender: TObject);
begin
  UpdateProjectFilename;
  EnableControls;
end;

procedure TfrmCloneLocalProjectParameters.editSourceProjectFilenameChange(Sender: TObject);
begin
  EnableControls;
  UpdateSourceProjectNote;
end;

procedure TfrmCloneLocalProjectParameters.editPathChange(Sender: TObject);
begin
  UpdateProjectFilename;
  EnableControls;
end;

procedure TfrmCloneLocalProjectParameters.editVersionChange(Sender: TObject);
begin
  EnableControls;
end;

function TfrmCloneLocalProjectParameters.IsSourceProjectALexicalModelProject: Boolean;
var
  sourceId: string;
begin
  sourceId := ChangeFileExt(ExtractFileName(editSourceProjectFilename.Text), '').ToLower;
  Result := TLexicalModelUtils.IsValidLexicalModelID(sourceId, False);
end;

procedure TfrmCloneLocalProjectParameters.EnableControls;
var
  e: Boolean;
  id: string;
begin
  e := (Trim(editSourceProjectFilename.Text) <> '') and
    FileExists(Trim(editSourceProjectFilename.Text)) and
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

function TfrmCloneLocalProjectParameters.GetBasePath: string;
begin
  Result := Trim(editPath.Text);
end;

function TfrmCloneLocalProjectParameters.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_CloneLocalProjectParameters;
end;

function TfrmCloneLocalProjectParameters.GetProjectID: string;
begin
  Result := Trim(LowerCase(editProjectID.Text));
end;

function TfrmCloneLocalProjectParameters.GetProjectFilename: string;
begin
  Result := editProjectFilename.Text;
end;

function TfrmCloneLocalProjectParameters.GetRelocateExternal: Boolean;
begin
  Result := chkRelocateExternal.Checked;
end;

function TfrmCloneLocalProjectParameters.GetSourceProjectFilename: string;
begin
  Result := editSourceProjectFilename.Text;
end;

function TfrmCloneLocalProjectParameters.Validate: Boolean;
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

procedure TfrmCloneLocalProjectParameters.UpdateProjectFilename;
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
