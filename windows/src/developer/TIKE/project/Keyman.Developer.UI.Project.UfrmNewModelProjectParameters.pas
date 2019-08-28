unit Keyman.Developer.UI.Project.UfrmNewModelProjectParameters;

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
  utilfiletypes;

type
  TfrmNewModelProjectParameters = class(TTikeForm)
    lblFileName: TLabel;
    lblPath: TLabel;
    lblAuthorID: TLabel;
    lblCoypright: TLabel;
    lblVersion: TLabel;
    lblAuthor: TLabel;
    lblKeyboardLanguages: TLabel;
    editModelID: TEdit;
    cmdBrowse: TButton;
    editPath: TEdit;
    editAuthorID: TEdit;
    editCopyright: TEdit;
    editVersion: TEdit;
    editAuthor: TEdit;
    cmdOK: TButton;
    cmdCancel: TButton;
    gridKeyboardLanguages: TStringGrid;
    cmdKeyboardAddLanguage: TButton;
    cmdKeyboardEditLanguage: TButton;
    cmdKeyboardRemoveLanguage: TButton;
    dlgSave: TSaveDialog;
    lblBCP47: TLabel;
    lblUniq: TLabel;
    editUniq: TEdit;
    cbBCP47: TComboBox;
    procedure cmdOKClick(Sender: TObject);
    procedure editModelIDComponentChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure gridKeyboardLanguagesClick(Sender: TObject);
    procedure gridKeyboardLanguagesDblClick(Sender: TObject);
    procedure cmdKeyboardAddLanguageClick(Sender: TObject);
    procedure cmdKeyboardEditLanguageClick(Sender: TObject);
    procedure cmdKeyboardRemoveLanguageClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure editCopyrightChange(Sender: TObject);
    procedure editVersionChange(Sender: TObject);
    procedure editAuthorChange(Sender: TObject);
    procedure editPathChange(Sender: TObject);
    procedure editModelIDChange(Sender: TObject);
    procedure cmdBrowseClick(Sender: TObject);
    procedure dlgSaveCanClose(Sender: TObject; var CanClose: Boolean);
  private
    pack: TKPSFile;
    FSetup: Integer;
    function GetAuthor: string;
    function GetBasePath: string;
    function GetCopyright: string;
    function GetBCP47Tags: string;
    function GetVersion: string;
    function Validate: Boolean;
    procedure EnableControls;
    function SelectedKeyboardLanguage: TPackageKeyboardLanguage;
    procedure LanguageGrid_Fill;
    procedure BCP47_Fill;
    function SelectedLexicalModel: TPackageLexicalModel;
    function GetModelID: string;
  protected
    function GetHelpTopic: string; override;
  public
    property Copyright: string read GetCopyright;
    property Version: string read GetVersion;
    property Author: string read GetAuthor;
    property BCP47Tags: string read GetBCP47Tags;
    property BasePath: string read GetBasePath;

    property ModelID: string read GetModelID;
  end;

function ShowNewModelProjectParameters(Owner: TComponent): Boolean;

implementation

uses
  Winapi.ShlObj,

  Keyman.System.LanguageCodeUtils,
  Keyman.System.LexicalModelUtils,
  BCP47Tag,
  utilstr,
  utilsystem,
  dmActionsMain,
  Keyman.Developer.System.HelpTopics,
  Keyman.Developer.System.Project.Project,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.KeyboardProjectTemplate,
  Keyman.Developer.System.ModelProjectTemplate,
  Keyman.Developer.System.ProjectTemplate,
  Keyman.Developer.UI.UfrmSelectBCP47Language;

// 1. project filename
// 2. location
// 3. keyboard name
// 4. copyright
// 5. targets
// 6. author
// 7. version
// 8. bcp47 tags

{$R *.dfm}

function ShowNewModelProjectParameters(Owner: TComponent): Boolean;
var
  f: TfrmNewModelProjectParameters;
  pt: TProjectTemplate;
begin
  f := TfrmNewModelProjectParameters.Create(Owner);
  try
    Result := f.ShowModal = mrOk;
    if not Result then
      Exit;

    pt := TModelProjectTemplate.Create(f.BasePath, f.ModelID);
    try
      pt.Name := f.ModelID;
      pt.Copyright := f.Copyright;
      pt.Author := f.Author;
      pt.Version := f.Version;
      pt.BCP47Tags := f.BCP47Tags;

      try
        pt.Generate;
      except
        on E:EFOpenError do
        begin
          ShowMessage('Unable to create project: '+E.Message);
          Exit(False);
        end;
      end;

      modActionsMain.OpenProject(pt.ProjectFilename);
      Result := True;

    finally
      pt.Free;
    end;
  finally
    f.Free;
  end;
end;

{ TfrmNewModelProjectParameters }

procedure TfrmNewModelProjectParameters.FormCreate(Sender: TObject);
begin
  inherited;
  editPath.Text := GetFolderPath(CSIDL_PERSONAL);

  pack := TKPSFile.Create;
  pack.LexicalModels.Add(TPackageLexicalModel.Create(pack));

  LanguageGrid_Fill;
  EnableControls;
end;

procedure TfrmNewModelProjectParameters.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(pack);
end;

procedure TfrmNewModelProjectParameters.BCP47_Fill;
var
  lm: TPackageLexicalModel;
  FLastBCP47: string;
  lang: TPackageKeyboardLanguage;
begin
  FLastBCP47 := cbBCP47.Text;
  cbBCP47.Clear;
  lm := SelectedLexicalModel;
  if not Assigned(lm) then
    Exit;
  for lang in lm.Languages do
    cbBCP47.Items.Add(lang.ID);

  cbBCP47.ItemIndex := cbBCP47.Items.IndexOf(FLastBCP47);
  if (cbBCP47.ItemIndex < 0) and (cbBCP47.Items.Count > 0) then
    cbBCP47.ItemIndex := 0;

  editModelIDComponentChange(cbBCP47);
end;

procedure TfrmNewModelProjectParameters.cmdBrowseClick(Sender: TObject);
var
  FPathName, FFolderName, FProjectName: string;
begin
  inherited;
  if dlgSave.Execute then
  begin
    FPathName := ExtractFilePath(ExtractFileDir(dlgSave.FileName));
    FFolderName := ExtractFileName(ExtractFileDir(dlgSave.FileName));
    FProjectName := ChangeFileExt(ExtractFileName(dlgSave.FileName), '');

    if not SameText(FFolderName, FProjectName) then
    begin
      if MessageDlg('The project will be saved at "'+FPathName+FFolderName+'\'+FProjectName+'\'+FProjectName+'.kpj". Continue?',
          mtConfirmation, mbOkCancel, 0) = mrCancel then
        Exit;
      FPathName := FPathName+FFolderName;
    end;
    editPath.Text := ExcludeTrailingPathDelimiter(FPathName);
    editModelID.Text := FProjectName;
  end;
end;

procedure TfrmNewModelProjectParameters.cmdKeyboardAddLanguageClick(
  Sender: TObject);
var
  lm: TPackageLexicalModel;
  lang: TPackageKeyboardLanguage;
  frm: TfrmSelectBCP47Language;
begin
  lm := SelectedLexicalModel;
  Assert(Assigned(lm));

  frm := TfrmSelectBCP47Language.Create(Self);
  try
    if frm.ShowModal = mrOk then
    begin
      lang := TPackageKeyboardLanguage.Create(pack);
      lang.ID := frm.LanguageID;
      lang.Name := frm.LanguageName;
      lm.Languages.Add(lang);

      BCP47_Fill;
      LanguageGrid_Fill;
      gridKeyboardLanguages.Row := gridKeyboardLanguages.RowCount - 1;
      gridKeyboardLanguagesClick(gridKeyboardLanguages);
      EnableControls;
    end;
  finally
    frm.Free;
  end;
end;

procedure TfrmNewModelProjectParameters.cmdKeyboardEditLanguageClick(
  Sender: TObject);
var
  lm: TPackageLexicalModel;
  lang: TPackageKeyboardLanguage;
  frm: TfrmSelectBCP47Language;
begin
  lm := SelectedLexicalModel;
  Assert(Assigned(lm));

  lang := SelectedKeyboardLanguage;
  Assert(Assigned(lang));

  frm := TfrmSelectBCP47Language.Create(Self);
  try
    frm.LanguageID := lang.ID;
    frm.LanguageName := lang.Name;
    if frm.ShowModal = mrOk then
    begin
      lang.ID := frm.LanguageID;
      lang.Name := frm.LanguageName;
      BCP47_Fill;
      LanguageGrid_Fill;
      EnableControls;
    end;
  finally
    frm.Free;
  end;
end;

procedure TfrmNewModelProjectParameters.cmdKeyboardRemoveLanguageClick(
  Sender: TObject);
var
  lm: TPackageLexicalModel;
  lang: TPackageKeyboardLanguage;
begin
  lm := SelectedLexicalModel;
  Assert(Assigned(lm));
  lang := SelectedKeyboardLanguage;
  Assert(Assigned(lang));

  lm.Languages.Remove(lang);
  LanguageGrid_Fill;
  BCP47_Fill;
  EnableControls;
end;

procedure TfrmNewModelProjectParameters.cmdOKClick(Sender: TObject);
begin
  if Validate then
    ModalResult := mrOk;
end;

procedure TfrmNewModelProjectParameters.dlgSaveCanClose(Sender: TObject; var CanClose: Boolean);
begin
  if not TLexicalModelUtils.DoesProjectFilenameFollowLexicalModelConventions(dlgSave.FileName) then
  begin
    CanClose := False;
    ShowMessage(Format(TLexicalModelUtils.SProjectFileNameDoesNotFollowLexicalModelConventions_Message, [ExtractFileName(dlgSave.FileName)]));
  end
  else
    CanClose := True;
end;

procedure TfrmNewModelProjectParameters.editAuthorChange(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmNewModelProjectParameters.editCopyrightChange(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmNewModelProjectParameters.editModelIDChange(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmNewModelProjectParameters.editModelIDComponentChange(Sender: TObject);
begin
  editModelID.Text := Format('%s.%s.%s', [editAuthorID.Text, cbBCP47.Text, editUniq.Text]);
  EnableControls;
end;

procedure TfrmNewModelProjectParameters.editPathChange(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmNewModelProjectParameters.editVersionChange(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmNewModelProjectParameters.EnableControls;
var
  e: Boolean;
begin
  e :=
    (Trim(editAuthorID.Text) <> '') and
    (Trim(cbBCP47.Text) <> '') and
    (Trim(editUniq.Text) <> '') and
    (Trim(editPath.Text) <> '') and
    TLexicalModelUtils.DoesProjectFilenameFollowLexicalModelConventions(editModelID.Text + Ext_LexicalModelProject) and
    (pack.LexicalModels[0].Languages.Count > 0);

  cmdOK.Enabled := e;

  e := gridKeyboardLanguages.RowCount > 1;
  gridKeyboardLanguages.Enabled := e;
  cmdKeyboardRemoveLanguage.Enabled := e;
  cmdKeyboardEditLanguage.Enabled := e;
  if e then
    gridKeyboardLanguages.FixedRows := 1;
end;

function TfrmNewModelProjectParameters.GetAuthor: string;
begin
  Result := Trim(editAuthor.Text);
end;

function TfrmNewModelProjectParameters.GetBasePath: string;
begin
  Result := Trim(editPath.Text);
end;

function TfrmNewModelProjectParameters.GetCopyright: string;
begin
  Result := Trim(editCopyright.Text);
end;

function TfrmNewModelProjectParameters.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_NewModelProjectParameters;
end;

function TfrmNewModelProjectParameters.GetModelID: string;
begin
  Result := editModelID.Text;
end;

function TfrmNewModelProjectParameters.GetBCP47Tags: string;
var
  lang: TPackageKeyboardLanguage;
begin
  Result := '';

  for lang in pack.LexicalModels[0].Languages do
    Result := Result + lang.ID + ' ';

  Result := Result.Trim;
end;

function TfrmNewModelProjectParameters.GetVersion: string;
begin
  Result := Trim(editVersion.Text);
end;

procedure TfrmNewModelProjectParameters.gridKeyboardLanguagesClick(
  Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmNewModelProjectParameters.gridKeyboardLanguagesDblClick(
  Sender: TObject);
begin
  if SelectedKeyboardLanguage <> nil then
    cmdKeyboardEditLanguage.Click;
end;

function TfrmNewModelProjectParameters.Validate: Boolean;
var
  ProjectFolder: string;
begin
  Result := TLexicalModelUtils.DoesProjectFilenameFollowLexicalModelConventions(editModelID.Text + Ext_LexicalModelProject);

  if Result then
  begin
    if not DirectoryExists(editPath.Text) then
    begin
      if MessageDlg('The target folder '+editPath.Text+' does not exist. Create it now?', mtConfirmation, mbOkCancel, 0) = mrCancel then
        Exit(False);
    end;

    ProjectFolder := IncludeTrailingPathDelimiter(editPath.Text) + editModelID.Text;
    if DirectoryExists(ProjectFolder) then
    begin
      if MessageDlg('The project folder '+ProjectFolder+' already exists. Are you sure you want to overwrite it?', mtWarning,
          mbOkCancel, 0) = mrCancel then
        Exit(False);
    end;
  end;
end;

{ Languages Grid }

function TfrmNewModelProjectParameters.SelectedLexicalModel: TPackageLexicalModel;
begin
  Result := pack.LexicalModels[0];
end;

function TfrmNewModelProjectParameters.SelectedKeyboardLanguage: TPackageKeyboardLanguage;
var
  lm: TPackageLexicalModel;
begin
  lm := SelectedLexicalModel;
  if not Assigned(lm) then
    Exit(nil);

  if gridKeyboardLanguages.Row = 0 then
    Exit(nil);

  Result := gridKeyboardLanguages.Objects[0, gridKeyboardLanguages.Row] as TPackageKeyboardLanguage;
end;

procedure TfrmNewModelProjectParameters.LanguageGrid_Fill;
var
  lm: TPackageLexicalModel;
  i: Integer;
begin
  Inc(FSetup);
  try
    gridKeyboardLanguages.Cells[0, 0] := 'BCP 47 tag';
    gridKeyboardLanguages.Cells[1, 0] := 'Language name';
    gridKeyboardLanguages.ColWidths[0] := 120;
    gridKeyboardLanguages.ColWidths[1] := 10;

    lm := SelectedLexicalModel;
    if not Assigned(lm) then
    begin
      gridKeyboardLanguages.RowCount := 1;
      EnableControls;
      Exit;
    end;

    gridKeyboardLanguages.RowCount := lm.Languages.Count + 1;
    gridKeyboardLanguages.ColWidths[1] := gridKeyboardLanguages.ClientWidth - 120 - 1;

    for i := 0 to lm.Languages.Count - 1 do
    begin
      gridKeyboardLanguages.Objects[0, i+1] := lm.Languages[i];
      gridKeyboardLanguages.Cells[0, i+1] := lm.Languages[i].ID;
      gridKeyboardLanguages.Cells[1, i+1] := lm.Languages[i].Name;
    end;

    EnableControls;
  finally
    Dec(FSetup);
  end;
end;

end.
