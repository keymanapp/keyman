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
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Grids,
  Vcl.StdCtrls,

  Browse4Folder,

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
    lblLanguages: TLabel;
    editModelID: TEdit;
    cmdBrowse: TButton;
    editPath: TEdit;
    editAuthorID: TEdit;
    editCopyright: TEdit;
    editVersion: TEdit;
    editAuthor: TEdit;
    cmdOK: TButton;
    cmdCancel: TButton;
    gridLanguages: TStringGrid;
    cmdAddLanguage: TButton;
    cmdEditLanguage: TButton;
    cmdRemoveLanguage: TButton;
    lblBCP47: TLabel;
    lblUniq: TLabel;
    editUniq: TEdit;
    cbBCP47: TComboBox;
    lblModelName: TLabel;
    editModelName: TEdit;
    Bevel1: TBevel;
    lblProjectFilename: TLabel;
    editProjectFilename: TEdit;
    Label1: TLabel;
    editFullCopyright: TEdit;
    Label2: TLabel;
    memoDescription: TMemo;
    procedure cmdOKClick(Sender: TObject);
    procedure editModelIDComponentChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure gridLanguagesClick(Sender: TObject);
    procedure gridLanguagesDblClick(Sender: TObject);
    procedure cmdAddLanguageClick(Sender: TObject);
    procedure cmdEditLanguageClick(Sender: TObject);
    procedure cmdRemoveLanguageClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure editCopyrightChange(Sender: TObject);
    procedure editVersionChange(Sender: TObject);
    procedure editAuthorChange(Sender: TObject);
    procedure editPathChange(Sender: TObject);
    procedure editModelIDChange(Sender: TObject);
    procedure cmdBrowseClick(Sender: TObject);
    procedure editModelNameChange(Sender: TObject);
    procedure editFullCopyrightChange(Sender: TObject);
    procedure memoDescriptionChange(Sender: TObject);
  private
    pack: TKPSFile;
    FSetup: Integer;
    dlgBrowse: TBrowse4Folder;
    function GetAuthor: string;
    function GetBasePath: string;
    function GetCopyright: string;
    function GetBCP47Tags: string;
    function GetVersion: string;
    function Validate: Boolean;
    procedure EnableControls;
    function SelectedLanguage: TPackageKeyboardLanguage;
    procedure LanguageGrid_Fill;
    procedure BCP47_Fill;
    function SelectedLexicalModel: TPackageLexicalModel;
    function GetModelID: string;
    procedure UpdateAuthorIDFromAuthor;
    procedure UpdateUniqFromModelName;
    function GetModelName: string;
    function GetAuthorID: string;
    function GetPrimaryBCP47: string;
    function GetUniq: string;
    procedure UpdateModelIDFromComponents;
    procedure UpdateProjectFilename;
    function GetFullCopyright: string;
    function GetDescription: string;
  protected
    function GetHelpTopic: string; override;
    property AuthorID: string read GetAuthorID;
    property PrimaryBCP47: string read GetPrimaryBCP47;
    property Uniq: string read GetUniq;
  public
    property Copyright: string read GetCopyright;
    property FullCopyright: string read GetFullCopyright;
    property Version: string read GetVersion;
    property Author: string read GetAuthor;
    property Description: string read GetDescription;
    property ModelName: string read GetModelName;
    property BCP47Tags: string read GetBCP47Tags;
    property BasePath: string read GetBasePath;

    property ModelID: string read GetModelID;
  end;

function ShowNewModelProjectParameters(Owner: TComponent): Boolean;

implementation

uses
  Keyman.System.LanguageCodeUtils,
  Keyman.System.LexicalModelUtils,
  BCP47Tag,
  utilstr,
  KeymanDeveloperOptions,
  Keyman.Developer.System.HelpTopics,
  Keyman.Developer.System.Project.Project,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.ModelProjectTemplate,
  Keyman.Developer.System.ProjectTemplate,
  Keyman.Developer.UI.UfrmSelectBCP47Language,
  UfrmMain;

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
      pt.Name := f.ModelName;
      pt.Copyright := f.Copyright;
      pt.FullCopyright := f.FullCopyright;
      pt.Description := f.Description;
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

      frmKeymanDeveloper.OpenProject(pt.ProjectFilename);
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
  editPath.Text := FKeymanDeveloperOptions.DefaultProjectPath;

  dlgBrowse := TBrowse4Folder.Create(Self);
  dlgBrowse.InitialDir := editPath.Text;
  dlgBrowse.Options := [OnlySelectFileSysAncestors, ShowEditBox, UseNewDialogStyle];
  dlgBrowse.Root := Desktop;
  dlgBrowse.Title := 'Select folder to save project to';

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

function TfrmNewModelProjectParameters.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_NewModelProjectParameters;
end;

procedure TfrmNewModelProjectParameters.cmdBrowseClick(Sender: TObject);
begin
  dlgBrowse.InitialDir := editPath.Text;

  if dlgBrowse.Execute and (dlgBrowse.FileName <> '') then
    editPath.Text := ExcludeTrailingPathDelimiter(dlgBrowse.FileName);
end;

procedure TfrmNewModelProjectParameters.cmdAddLanguageClick(
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
      gridLanguages.Row := gridLanguages.RowCount - 1;
      gridLanguagesClick(gridLanguages);
      EnableControls;
    end;
  finally
    frm.Free;
  end;
end;

procedure TfrmNewModelProjectParameters.cmdEditLanguageClick(
  Sender: TObject);
var
  lm: TPackageLexicalModel;
  lang: TPackageKeyboardLanguage;
  frm: TfrmSelectBCP47Language;
begin
  lm := SelectedLexicalModel;
  Assert(Assigned(lm));

  lang := SelectedLanguage;
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

procedure TfrmNewModelProjectParameters.cmdRemoveLanguageClick(
  Sender: TObject);
var
  lm: TPackageLexicalModel;
  lang: TPackageKeyboardLanguage;
begin
  lm := SelectedLexicalModel;
  Assert(Assigned(lm));
  lang := SelectedLanguage;
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

procedure TfrmNewModelProjectParameters.editAuthorChange(Sender: TObject);
begin
  UpdateAuthorIDFromAuthor;
  EnableControls;
end;

procedure TfrmNewModelProjectParameters.editModelNameChange(Sender: TObject);
begin
  UpdateUniqFromModelName;
  EnableControls;
end;

procedure TfrmNewModelProjectParameters.editCopyrightChange(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmNewModelProjectParameters.editFullCopyrightChange(
  Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmNewModelProjectParameters.editModelIDChange(Sender: TObject);
begin
  UpdateProjectFilename;
  EnableControls;
end;

procedure TfrmNewModelProjectParameters.editModelIDComponentChange(Sender: TObject);
begin
  UpdateModelIDFromComponents;
  EnableControls;
end;

procedure TfrmNewModelProjectParameters.editPathChange(Sender: TObject);
begin
  UpdateProjectFilename;
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
    not Author.IsEmpty and
    not Description.IsEmpty and
    not ModelName.IsEmpty and
    not AuthorID.IsEmpty and
    not PrimaryBCP47.IsEmpty and
    not Uniq.IsEmpty and
    not Trim(editPath.Text).IsEmpty and
    TLexicalModelUtils.DoesProjectFilenameFollowLexicalModelConventions(editModelID.Text + Ext_LexicalModelProject) and
    (pack.LexicalModels[0].Languages.Count > 0);

  cmdOK.Enabled := e;

  e := gridLanguages.RowCount > 1;
  gridLanguages.Enabled := e;
  cmdRemoveLanguage.Enabled := e;
  cmdEditLanguage.Enabled := e;
  if e then
    gridLanguages.FixedRows := 1;
end;

function TfrmNewModelProjectParameters.GetAuthor: string;
begin
  Result := Trim(editAuthor.Text);
end;

function TfrmNewModelProjectParameters.GetAuthorID: string;
begin
  Result := Trim(editAuthorID.Text);
end;

function TfrmNewModelProjectParameters.GetBasePath: string;
begin
  Result := Trim(editPath.Text);
end;

function TfrmNewModelProjectParameters.GetCopyright: string;
begin
  Result := Trim(editCopyright.Text);
end;

function TfrmNewModelProjectParameters.GetDescription: string;
begin
  Result := memoDescription.Text;
end;

function TfrmNewModelProjectParameters.GetFullCopyright: string;
begin
  Result := editFullCopyright.Text;
end;

function TfrmNewModelProjectParameters.GetModelID: string;
begin
  Result := editModelID.Text;
end;

function TfrmNewModelProjectParameters.GetModelName: string;
begin
  Result := Trim(editModelName.Text);
end;

function TfrmNewModelProjectParameters.GetPrimaryBCP47: string;
begin
  Result := Trim(cbBCP47.Text);
end;

function TfrmNewModelProjectParameters.GetUniq: string;
begin
  Result := Trim(editUniq.Text);
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

procedure TfrmNewModelProjectParameters.gridLanguagesClick(
  Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmNewModelProjectParameters.gridLanguagesDblClick(
  Sender: TObject);
begin
  if SelectedLanguage <> nil then
    cmdEditLanguage.Click;
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

{ Dynamically update other fields based on filled details }

procedure TfrmNewModelProjectParameters.UpdateAuthorIDFromAuthor;
begin
  editAuthorID.Text := TLexicalModelUtils.CleanLexicalModelIDComponent(Author);
  editCopyright.Text := 'Copyright ' + Char($00A9 {copyright})+' '+Author;
  editFullCopyright.Text := 'Copyright ' + Char($00A9 {copyright})+' '+FormatDateTime('yyyy', Now)+' '+Author;
end;

procedure TfrmNewModelProjectParameters.UpdateUniqFromModelName;
begin
  editUniq.Text := TLexicalModelUtils.CleanLexicalModelIDComponent(ModelName);
end;

procedure TfrmNewModelProjectParameters.UpdateModelIDFromComponents;
begin
  editModelID.Text := Format('%s.%s.%s', [AuthorID, PrimaryBCP47.ToLowerInvariant, Uniq]);
end;

procedure TfrmNewModelProjectParameters.UpdateProjectFilename;
begin
  editProjectFilename.Text :=
    IncludeTrailingPathDelimiter(BasePath) +
    ModelID + PathDelim +
    ModelID + Ext_LexicalModelProject;
  // Scroll to the end of the control to show the filename
  editProjectFilename.Perform(EM_SETSEL, Length(editProjectFilename.Text), Length(editProjectFilename.Text));
  editProjectFilename.Perform(EM_SCROLLCARET, 0, 0);
end;

{ Languages Grid }

function TfrmNewModelProjectParameters.SelectedLexicalModel: TPackageLexicalModel;
begin
  Result := pack.LexicalModels[0];
end;

function TfrmNewModelProjectParameters.SelectedLanguage: TPackageKeyboardLanguage;
var
  lm: TPackageLexicalModel;
begin
  lm := SelectedLexicalModel;
  if not Assigned(lm) then
    Exit(nil);

  if gridLanguages.Row = 0 then
    Exit(nil);

  Result := gridLanguages.Objects[0, gridLanguages.Row] as TPackageKeyboardLanguage;
end;

procedure TfrmNewModelProjectParameters.LanguageGrid_Fill;
var
  lm: TPackageLexicalModel;
  i: Integer;
begin
  Inc(FSetup);
  try
    gridLanguages.Cells[0, 0] := 'BCP 47 tag';
    gridLanguages.Cells[1, 0] := 'Language name';
    gridLanguages.ColWidths[0] := 120;
    gridLanguages.ColWidths[1] := 10;

    lm := SelectedLexicalModel;
    if not Assigned(lm) then
    begin
      gridLanguages.RowCount := 1;
      EnableControls;
      Exit;
    end;

    gridLanguages.RowCount := lm.Languages.Count + 1;
    gridLanguages.ColWidths[1] := gridLanguages.ClientWidth - 120 - 1;

    for i := 0 to lm.Languages.Count - 1 do
    begin
      gridLanguages.Objects[0, i+1] := lm.Languages[i];
      gridLanguages.Cells[0, i+1] := lm.Languages[i].ID;
      gridLanguages.Cells[1, i+1] := lm.Languages[i].Name;
    end;

    EnableControls;
  finally
    Dec(FSetup);
  end;
end;

procedure TfrmNewModelProjectParameters.memoDescriptionChange(Sender: TObject);
begin
  EnableControls;
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

end.
