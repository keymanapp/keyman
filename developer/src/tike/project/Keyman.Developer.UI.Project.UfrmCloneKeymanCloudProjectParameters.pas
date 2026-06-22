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
    lblMessage: TLabel;
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
    FSourceAvailable: Boolean;
    frmDownloadProgress: TfrmDownloadProgress;
    function GetBasePath: string;
    function GetKeyboardID: string;
    function Validate: Boolean;
    procedure EnableControls;
    procedure SetKeyboardID(const Value: string);
    procedure UpdateProjectFilename;
    procedure UpdateMessage;
    function GetProjectFilename: string;
    function GetSourceProjectFilename: string;
    function GetRelocateExternal: Boolean;
    procedure RefreshHTML;
    procedure cefLoadEnd(Sender: TObject);
    procedure DownloadCallback(Owner: TfrmDownloadProgress; var Result: Boolean);
    procedure DownloadWrapperCallback(var Cancelled: Boolean);
    function IsKeyboardSourceAvailable(const id: string): Boolean;
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
  System.JSON,
  System.Net.UrlClient,
  Vcl.ComCtrls,

  HttpUploader,
  KeymanDeveloperOptions,
  Keyman.Developer.System.KmcWrapper,
  Keyman.Developer.System.HelpTopics,
  Keyman.Developer.System.Project.Project,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.UI.Project.ProjectUI,
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

  UpdateMessage;
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

  FSourceAvailable := IsKeyboardSourceAvailable(FKeymanID);
  UpdateMessage;
  EnableControls;
end;

function TfrmCloneKeymanCloudProjectParameters.IsKeyboardSourceAvailable(const id: string): Boolean;

  function GetKeyboardDataFromApiServer(const id: string): string;
  var
    http: THTTPUploader;
  begin
    http := THTTPUploader.Create(nil);
    try
      http.Request.HostName := API_Server;
      http.Request.Protocol := API_Protocol;
      http.Request.UrlPath := API_Path_Keyboard(id);
      try
        http.Upload;
      except
        // Silently swallow network errors
        on E:Exception do Exit('');
      end;

      if (http.Response.StatusCode < 200) or (http.Response.StatusCode > 299) then
      begin
        // Keyboard not found or invalid response
        Exit('');
      end;

      Result := UTF8ToString(PAnsiChar(http.Response.MessageBodyAsString));
    finally
      FreeAndNil(http);
    end;
  end;

  function GetSourcePathFromBody(const body: string): string;
  var
    val: TJSONValue;
    obj: TJSONObject;
  begin
    try
      val := TJSONObject.ParseJSONValue(body);
    except
      // Not a valid response
      Exit('');
    end;

    if not (val is TJSONObject) then
    begin
      // Not a valid response
      Exit('');
    end;

    obj := val as TJSONObject;
    val := obj.Values['sourcePath'];
    if not Assigned(val) or not (val is TJSONString) then
    begin
      // no sourcePath property
      Exit('');
    end;

    Result := (val as TJSONString).Value;
  end;

var
  body, sourcePath: string;
begin
  if id = '' then
  begin
    Exit(False);
  end;

  body := GetKeyboardDataFromApiServer(id);
  if body = '' then
  begin
    Exit(False);
  end;

  sourcePath := GetSourcePathFromBody(body);
  if sourcePath = '' then
  begin
    Exit(False);
  end;

  // Keyboards in legacy/ do not have source available. Keyboards in
  // release/ and experimental/ have source, and other new categories will
  // also have source in future.
  Result := not sourcePath.startsWith('legacy');
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
  UpdateMessage;
  EnableControls;
end;

procedure TfrmCloneKeymanCloudProjectParameters.editSourceProjectFilenameChange(Sender: TObject);
begin
  UpdateMessage;
  EnableControls;
end;

procedure TfrmCloneKeymanCloudProjectParameters.editPathChange(Sender: TObject);
begin
  UpdateProjectFilename;
  UpdateMessage;
  EnableControls;
end;

procedure TfrmCloneKeymanCloudProjectParameters.editVersionChange(Sender: TObject);
begin
  UpdateMessage;
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
    TKeyboardUtils.IsValidKeyboardID(Trim(editKeyboardID.Text), True) and
    FSourceAvailable;

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
begin
  Result := TKeyboardUtils.IsValidKeyboardID(Trim(editKeyboardID.Text), True);
  Result := Result and VerifyNewProjectPathWithUser(BasePath, KeyboardID);
end;

procedure TfrmCloneKeymanCloudProjectParameters.SetKeyboardID(const Value: string);
begin
  editKeyboardID.Text := Value;
  UpdateMessage;
  EnableControls;
end;

procedure TfrmCloneKeymanCloudProjectParameters.UpdateMessage;
var
  msg: string;
begin
  if FKeymanID = '' then
  begin
    msg := 'Please choose a keyboard from the search form above.';
  end
  else if not FSourceAvailable then
  begin
    msg := Format('The keyboard %0:s has no source available. It cannot be cloned.', [FKeymanID]);
  end
  else if Trim(editPath.Text) = '' then
  begin
    msg := 'A valid destination path must be selected.';
  end
  else if Trim(editKeyboardID.Text) = '' then
  begin
    msg := 'Please enter a valid new project identifier.';
  end
  else if not TKeyboardUtils.IsValidKeyboardID(Trim(editKeyboardID.Text), True) then
  begin
    msg := 'Please enter a valid new project identifier.';
  end
  else
  begin
    msg := '';
  end;
  lblMessage.Caption := msg;
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
