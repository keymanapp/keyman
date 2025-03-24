(*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Parameters dialog for cloning a project from Keyman Cloud
 *)
unit Keyman.Developer.UI.Project.UfrmCloneKeymanCloudProjectParameters;

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
  Browse4Folder,
  Keyman.UI.UframeCEFHost;

type
  TfrmCloneKeymanCloudProjectParameters = class(TTikeForm)
    cmdOK: TButton;
    cmdCancel: TButton;
    panWebHost: TPanel;
    gbNewProjectDetails: TGroupBox;
    lblFileName: TLabel;
    lblProjectFilename: TLabel;
    editKeyboardID: TEdit;
    editProjectFilename: TEdit;
    lblPath: TLabel;
    cmdBrowse: TButton;
    editPath: TEdit;
    chkRelocateExternal: TCheckBox;
    procedure cmdOKClick(Sender: TObject);
    procedure editSourceProjectFilenameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure editVersionChange(Sender: TObject);
    procedure editPathChange(Sender: TObject);
    procedure editKeyboardIDChange(Sender: TObject);
    procedure cmdBrowseClick(Sender: TObject);
  private
    cef: TframeCEFHost;
    dlgBrowse: TBrowse4Folder;
    FKeymanID: string;
    frmDownloadProgress: TfrmDownloadProgress;
    function GetBasePath: string;
    function GetKeyboardID: string;
    function Validate: Boolean;
    procedure EnableControls;
    procedure SetKeyboardID(const Value: string);
    procedure UpdateProjectFilename;
    function GetProjectFilename: string;
    function GetSourceProjectFilename: string;
    function GetRelocateExternal: Boolean;
    procedure RefreshHTML;
    procedure cefLoadEnd(Sender: TObject);
    procedure DownloadCallback(Owner: TfrmDownloadProgress; var Result: Boolean);
    procedure DownloadWrapperCallback(var Cancelled: Boolean);
  protected
    function GetHelpTopic: string; override;
  public
    property BasePath: string read GetBasePath;
    property KeyboardID: string read GetKeyboardID write SetKeyboardID;
    property ProjectFilename: string read GetProjectFilename;
    property SourceProjectFilename: string read GetSourceProjectFilename;
    property RelocateExternal: Boolean read GetRelocateExternal;
  end;

function ShowCloneKeymanCloudProjectParameters(Owner: TComponent): Boolean;

implementation

uses
  System.Net.UrlClient,
  Vcl.ComCtrls,

  KeymanDeveloperOptions,
  Keyman.Developer.System.KmcWrapper,
  Keyman.Developer.System.HelpTopics,
  Keyman.Developer.System.Project.Project,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.System.KeyboardUtils,
  UfrmMain,
  Upload_Settings;

{$R *.dfm}

function ShowCloneKeymanCloudProjectParameters(Owner: TComponent): Boolean;
var
  f: TfrmCloneKeymanCloudProjectParameters;
  path: string;
  p: TfrmDownloadProgress;
begin
  f := TfrmCloneKeymanCloudProjectParameters.Create(Owner);
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

{ TfrmCloneKeymanCloudProjectParameters }

procedure TfrmCloneKeymanCloudProjectParameters.DownloadWrapperCallback(var Cancelled: Boolean);
begin
  Application.ProcessMessages;
  Cancelled := frmDownloadProgress.Cancel;
end;

procedure TfrmCloneKeymanCloudProjectParameters.DownloadCallback(
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

procedure TfrmCloneKeymanCloudProjectParameters.FormCreate(Sender: TObject);
begin
  inherited;
  cef := TframeCEFHost.Create(Self);
  cef.Parent := panWebHost;
  cef.Visible := True;
  cef.OnLoadEnd := cefLoadEnd;
  RefreshHTML;

  editPath.Text := FKeymanDeveloperOptions.DefaultProjectPath;

  dlgBrowse := TBrowse4Folder.Create(Self);
  dlgBrowse.InitialDir := editPath.Text;
  dlgBrowse.Options := [OnlySelectFileSysDir, ShowEditBox, UseNewDialogStyle];
  dlgBrowse.Root := Desktop;
  dlgBrowse.Title := 'Select folder to save project to';

  EnableControls;
end;

procedure TfrmCloneKeymanCloudProjectParameters.RefreshHTML;
begin
  cef.Navigate(MakeKeymanURL(URLPath_KeymanDeveloper_KeyboardSearchForCloneKeymanCloud));
end;

procedure TfrmCloneKeymanCloudProjectParameters.cefLoadEnd(Sender: TObject);
var
  u: TUri;
begin
  u := TUri.Create(cef.cef.DocumentURL);
  // We want to match on keyman.com/keyboards/<id>
  // but we need to avoid keyman.com/keyboards/h/<custom-keyboard-home-page>
  if u.Path.StartsWith(URLSubPath_KeymanDeveloper_Clone_Keyboards) and
      not u.Path.StartsWith(URLSubPath_KeymanDeveloper_Clone_Keyboards_Custom)
    then FKeymanID := u.Path.Substring(URLSubPath_KeymanDeveloper_Clone_Keyboards.Length)
    else FKeymanID := '';
  EnableControls;
end;

procedure TfrmCloneKeymanCloudProjectParameters.cmdBrowseClick(Sender: TObject);
begin
  dlgBrowse.InitialDir := editPath.Text;

  if dlgBrowse.Execute and (dlgBrowse.FileName <> '') then
    editPath.Text := ExcludeTrailingPathDelimiter(dlgBrowse.FileName);
end;

procedure TfrmCloneKeymanCloudProjectParameters.cmdOKClick(Sender: TObject);
begin
  if Validate then
    ModalResult := mrOk;
end;

procedure TfrmCloneKeymanCloudProjectParameters.editKeyboardIDChange(Sender: TObject);
begin
  UpdateProjectFilename;
  EnableControls;
end;

procedure TfrmCloneKeymanCloudProjectParameters.editSourceProjectFilenameChange(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmCloneKeymanCloudProjectParameters.editPathChange(Sender: TObject);
begin
  UpdateProjectFilename;
  EnableControls;
end;

procedure TfrmCloneKeymanCloudProjectParameters.editVersionChange(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmCloneKeymanCloudProjectParameters.EnableControls;
var
  e: Boolean;
begin
  e :=
    (FKeymanID <> '') and
    (Trim(editPath.Text) <> '') and
    (Trim(editKeyboardID.Text) <> '') and
    TKeyboardUtils.IsValidKeyboardID(Trim(editKeyboardID.Text), True);

  cmdOK.Enabled := e;
end;

function TfrmCloneKeymanCloudProjectParameters.GetBasePath: string;
begin
  Result := Trim(editPath.Text);
end;

function TfrmCloneKeymanCloudProjectParameters.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_CloneKeymanCloudProjectParameters;
end;

function TfrmCloneKeymanCloudProjectParameters.GetKeyboardID: string;
begin
  Result := Trim(LowerCase(editKeyboardID.Text));
end;

function TfrmCloneKeymanCloudProjectParameters.GetProjectFilename: string;
begin
  Result := editProjectFilename.Text;
end;

function TfrmCloneKeymanCloudProjectParameters.GetRelocateExternal: Boolean;
begin
  Result := chkRelocateExternal.Checked;
end;

function TfrmCloneKeymanCloudProjectParameters.GetSourceProjectFilename: string;
begin
  Result := 'cloud:'+FKeymanID;
end;

function TfrmCloneKeymanCloudProjectParameters.Validate: Boolean;
var
  ProjectFolder: string;
begin
  Result := TKeyboardUtils.IsValidKeyboardID(Trim(editKeyboardID.Text), True);

  if Result then
  begin
    if not DirectoryExists(editPath.Text) then
    begin
      if MessageDlg('The target folder '+editPath.Text+' does not exist. Create it now?', mtConfirmation, mbOkCancel, 0) = mrCancel then
        Exit(False);
    end;

    ProjectFolder := IncludeTrailingPathDelimiter(editPath.Text) + editKeyboardID.Text;
    if DirectoryExists(ProjectFolder) then
    begin
      if MessageDlg('The project folder '+ProjectFolder+' already exists. Are you sure you want to overwrite it?', mtWarning,
          mbOkCancel, 0) = mrCancel then
        Exit(False);
    end;
  end;
end;

procedure TfrmCloneKeymanCloudProjectParameters.SetKeyboardID(const Value: string);
begin
  editKeyboardID.Text := Value;
  EnableControls;
end;

procedure TfrmCloneKeymanCloudProjectParameters.UpdateProjectFilename;
begin
  editProjectFilename.Text :=
    IncludeTrailingPathDelimiter(BasePath) +
    KeyboardID + PathDelim +
    KeyboardID + Ext_ProjectSource;
  // Scroll to the end of the control to show the filename
  editProjectFilename.Perform(EM_SETSEL, Length(editProjectFilename.Text), Length(editProjectFilename.Text));
  editProjectFilename.Perform(EM_SCROLLCARET, 0, 0);
end;

end.
