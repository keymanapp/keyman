unit Keyman.Developer.UI.Project.UfrmNewProjectParameters;

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

  Browse4Folder,
  uCEFTypes,

  Keyman.UI.UframeCEFHost,
  PackageInfo,
  UfrmTike,
  UKeymanTargets,
  utilfiletypes;

type
  TfrmNewProjectParameters = class(TTikeForm)
    procedure FormCreate(Sender: TObject);
  private
    cef: TframeCEFHost;
    FToken: string;
    dlgBrowse: TBrowse4Folder;

    FAuthor: string;
    FVersion: string;
    FKeyboardID: string;
    FBCP47Tags: string;
    FTargets: TKeymanTargets;
    FFullCopyright: string;
    FDescription: string;
    FBasePath: string;
    FKeyboardName: string;
    FCopyright: string;

    procedure cefLoadEnd(Sender: TObject);
    procedure cefBeforeBrowse(Sender: TObject; const Url: string; isPopup, wasHandled: Boolean);
    procedure cefBeforeBrowseSync(Sender: TObject; const Url: string; isPopup: Boolean; out Handled: Boolean);
    procedure cefAfterCreated(Sender: TObject);
    function FinalizeForm: Boolean;

  protected
    function GetHelpTopic: string; override;
  public
    property Copyright: string read FCopyright;
    property FullCopyright: string read FFullCopyright;
    property Version: string read FVersion;
    property Author: string read FAuthor;
    property Targets: TKeymanTargets read FTargets;
    property BCP47Tags: string read FBCP47Tags write FBCP47Tags;
    property BasePath: string read FBasePath;

    property Description: string read FDescription;

    property KeyboardName: string read FKeyboardName write FKeyboardName;
    property KeyboardID: string read FKeyboardID write FKeyboardID;
  end;

function ShowNewProjectParameters(Owner: TComponent): Boolean;

implementation

uses
  System.JSON,
  System.NetEncoding,
  System.Net.HttpClient,
  System.Net.URLClient,
  Winapi.ActiveX,
  Keyman.System.CanonicalLanguageCodeUtils,
  Keyman.System.LanguageCodeUtils,
  BCP47Tag,
  utilstr,
  KeymanDeveloperOptions,
  Keyman.Developer.System.HelpTopics,
  Keyman.Developer.System.Project.Project,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.KeyboardProjectTemplate,
  Keyman.Developer.System.ProjectTemplate,
  Keyman.Developer.UI.UfrmSelectBCP47Language,
  Keyman.System.KeyboardUtils,
  UfrmMain;

// 1. project filename
// 2. location
// 3. keyboard name
// 4. copyright
// 5. targets
// 6. author
// 7. version
// 8. bcp47 tags
// 9. description

{$R *.dfm}

function ShowNewProjectParameters(Owner: TComponent): Boolean;
var
  f: TfrmNewProjectParameters;
  pt: TKeyboardProjectTemplate;
begin
  f := TfrmNewProjectParameters.Create(Owner);
  try
    Result := f.ShowModal = mrOk;
    if not Result then
      Exit;

    pt := TKeyboardProjectTemplate.Create(f.BasePath, f.KeyboardID, f.Targets);
    try
      pt.Name := f.KeyboardName;
      pt.Copyright := f.Copyright;
      pt.FullCopyright := f.FullCopyright;
      pt.Author := f.Author;
      pt.Version := f.Version;
      pt.Description := f.Description;
      pt.BCP47Tags := f.BCP47Tags;
      pt.IncludeIcon := True;

      try
        pt.Generate;
      except
        on E:EKeyboardProjectTemplate do
        begin
          ShowMessage('Unable to create project: '+E.Message);
          Exit(False);
        end;
        on E:EFOpenError do
        begin
          ShowMessage('Unable to create project; template files may be missing: '+E.Message);
          Exit(False);
        end;
      end;

      frmKeymanDeveloper.OpenProject(pt.ProjectFilename);
      Result := True;

    finally
      pt.Free;
    end;
  finally
    f.Free;
  end;
end;

{ TfrmNewBasicProjectParameters }

procedure TfrmNewProjectParameters.FormCreate(Sender: TObject);
var
  g: TGUID;
begin
  inherited;

  CoCreateGuid(g);
  FToken := GUIDToString(g);

  cef := TframeCEFHost.Create(Self);
  cef.Parent := Self;
  cef.Visible := True;
  cef.OnAfterCreated := cefAfterCreated;
  cef.OnBeforeBrowse := cefBeforeBrowse;
  cef.OnBeforeBrowseSync := cefBeforeBrowseSync;
  cef.OnLoadEnd := cefLoadEnd;

  // TODO: initial path from FKeymanDeveloperOptions.DefaultProjectPath
  cef.Navigate('http://localhost:5173/new-project/new-basic-keyboard-project');

  dlgBrowse := TBrowse4Folder.Create(Self);
  dlgBrowse.InitialDir := FKeymanDeveloperOptions.DefaultProjectPath;
  dlgBrowse.Options := [OnlySelectFileSysDir, ShowEditBox, UseNewDialogStyle];
  dlgBrowse.Root := Desktop;
  dlgBrowse.Title := 'Select folder to save project to';
end;

procedure TfrmNewProjectParameters.cefAfterCreated(Sender: TObject);
begin
  if not cef.cef.SetCookie( 'http://localhost:5173', 'token', FToken, '', '/', False, False, False,
    Now, Now, Now + 365, CEF_COOKIE_SAME_SITE_UNSPECIFIED, 0) then
    raise Exception.Create('Unable to set token cookie');
  if not cef.cef.SetCookie( 'http://localhost:5173', 'tike', 'true', '', '/', False, False, False,
    Now, Now, Now + 365, CEF_COOKIE_SAME_SITE_UNSPECIFIED, 0) then
    raise Exception.Create('Unable to set tike cookie');
end;

procedure TfrmNewProjectParameters.cefBeforeBrowse(Sender: TObject;
  const Url: string; isPopup, wasHandled: Boolean);
var
  newPath: string;
  u: TUri;
  s: TJSONString;
begin
  if Url = 'http://keyman/ok' then
  begin
    if FinalizeForm then
      ModalResult := mrOk;
  end
  else if Url = 'http://keyman/cancel' then
  begin
    ModalResult := mrCancel;
  end
  else if Url.StartsWith('http://keyman/browse') then
  begin
    u := TUri.Create(Url);

    dlgBrowse.InitialDir := TNetEncoding.URL.Decode(u.ParameterByName['basePath']);
    if dlgBrowse.Execute and (dlgBrowse.FileName <> '') then
    begin
      newPath := ExcludeTrailingPathDelimiter(dlgBrowse.FileName);
      s := TJSONString.Create(newPath);
      cef.cef.ExecuteJavaScript('window._keymanInterface.setPath(' + s.ToJSON + ');', '');
      s.Free;
    end;
  end;
end;

procedure TfrmNewProjectParameters.cefBeforeBrowseSync(Sender: TObject;
  const Url: string; isPopup: Boolean; out Handled: Boolean);
begin
  if Url = 'http://keyman/ok' then
    Handled := True
  else if Url = 'http://keyman/cancel' then
    Handled := True
  else if Url.StartsWith('http://keyman/browse') then
    Handled := True;
end;

procedure TfrmNewProjectParameters.cefLoadEnd(Sender: TObject);
begin
  cef.SetFocus;
end;

function TfrmNewProjectParameters.FinalizeForm: Boolean;
var
  doc: TJSONObject;
  http: THttpClient;
  res: IHTTPResponse;
  h: TNetHeader;
begin
  http := THttpClient.Create;
  try
    try
      // TODO: url encoding
      h := TNetHeader.Create('Cookie', 'token='+FToken);
      res := http.Get('http://localhost:5173/new-project/new-basic-keyboard-project/result', nil, [h]);
      Result := res.StatusCode = 200;
      if Result then
      begin
        doc := TJSONObject.ParseJSONValue(res.ContentAsString) as TJSONObject;
        if not Assigned(doc) then
          Exit(False);

        FCopyright := doc.GetValue<string>('copyright');
        FFullCopyright := doc.GetValue<string>('fullCopyright');
        FVersion := doc.GetValue<string>('version');
        FAuthor := doc.GetValue<string>('author');
        FBCP47Tags := doc.GetValue<string>('languages');
        FBasePath := doc.GetValue<string>('basePath');
        FDescription := doc.GetValue<string>('description');
        FKeyboardName := doc.GetValue<string>('keyboardName');
        FKeyboardID := doc.GetValue<string>('keyboardID');
      end;
    except
      Result := False;
    end;
  finally
    http.Free;
  end;
end;

function TfrmNewProjectParameters.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_NewProjectParameters;
end;

end.

