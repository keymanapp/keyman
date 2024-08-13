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

  kpsfile,
  PackageInfo,
  UfrmTike,
  UKeymanTargets,
  utilfiletypes, Browse4Folder;

type
  TfrmNewProjectParameters = class(TTikeForm)
    lblFileName: TLabel;
    lblPath: TLabel;
    editKeyboardID: TEdit;
    cmdBrowse: TButton;
    editPath: TEdit;
    lblKeyboardName: TLabel;
    editKeyboardName: TEdit;
    lblCoypright: TLabel;
    editCopyright: TEdit;
    lblVersion: TLabel;
    editVersion: TEdit;
    lblAuthor: TLabel;
    editAuthor: TEdit;
    cmdOK: TButton;
    cmdCancel: TButton;
    lblTargets: TLabel;
    clbTargets: TCheckListBox;
    gridKeyboardLanguages: TStringGrid;
    cmdKeyboardAddLanguage: TButton;
    cmdKeyboardEditLanguage: TButton;
    cmdKeyboardRemoveLanguage: TButton;
    lblKeyboardLanguages: TLabel;
    lblProjectFilename: TLabel;
    editProjectFilename: TEdit;
    Label1: TLabel;
    editFullCopyright: TEdit;
    Label2: TLabel;
    memoDescription: TMemo;
    procedure cmdOKClick(Sender: TObject);
    procedure editKeyboardNameChange(Sender: TObject);
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
    procedure clbTargetsClickCheck(Sender: TObject);
    procedure editPathChange(Sender: TObject);
    procedure editKeyboardIDChange(Sender: TObject);
    procedure cmdBrowseClick(Sender: TObject);
    procedure editFullCopyrightChange(Sender: TObject);
    procedure memoDescriptionChange(Sender: TObject);
  private
    dlgBrowse: TBrowse4Folder;
    pack: TKPSFile; // Used temporarily for storing language list
    FSetup: Integer;
    function GetAuthor: string;
    function GetBasePath: string;
    function GetCopyright: string;
    function GetKeyboardID: string;
    function GetKeyboardName: string;
    function GetBCP47Tags: string;
    function GetTargets: TKeymanTargets;
    function GetVersion: string;
    function Validate: Boolean;
    procedure EnableControls;
    function SelectedKeyboardLanguage: TPackageKeyboardLanguage;
    procedure LanguageGrid_Fill;
    function SelectedKeyboard: TPackageKeyboard;
    procedure SetBCP47Tags(const Value: string);
    procedure SetKeyboardID(const Value: string);
    procedure SetKeyboardName(const Value: string);
    procedure UpdateProjectFilename;
    function GetFullCopyright: string;
    function GetDescription: string;
  protected
    function GetHelpTopic: string; override;
  public
    property Copyright: string read GetCopyright;
    property FullCopyright: string read GetFullCopyright;
    property Version: string read GetVersion;
    property Author: string read GetAuthor;
    property Targets: TKeymanTargets read GetTargets;
    property BCP47Tags: string read GetBCP47Tags write SetBCP47Tags;
    property BasePath: string read GetBasePath;

    property Description: string read GetDescription;

    property KeyboardName: string read GetKeyboardName write SetKeyboardName;
    property KeyboardID: string read GetKeyboardID write SetKeyboardID;
  end;

function ShowNewProjectParameters(Owner: TComponent): Boolean;

implementation

uses
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

{ TfrmNewBasicProjectParameters }

procedure TfrmNewProjectParameters.FormCreate(Sender: TObject);
var
  i: TKeymanTarget;
begin
  inherited;
  editPath.Text := FKeymanDeveloperOptions.DefaultProjectPath;
  editFullCopyright.Text := 'Copyright '+Char($00A9 {copyright})+' '+FormatDateTime('yyyy', Now)+' ';

  dlgBrowse := TBrowse4Folder.Create(Self);
  dlgBrowse.InitialDir := editPath.Text;
  dlgBrowse.Options := [OnlySelectFileSysDir, ShowEditBox, UseNewDialogStyle];
  dlgBrowse.Root := Desktop;
  dlgBrowse.Title := 'Select folder to save project to';

  pack := TKPSFile.Create;
  pack.Keyboards.Add(TPackageKeyboard.Create(pack));

  for i := Low(TKeymanTarget) to High(TKeymanTarget) do   // I4504
    clbTargets.Items.Add(SKeymanTargets[i]);

  clbTargets.Checked[0] := True;

  LanguageGrid_Fill;
  EnableControls;
end;

procedure TfrmNewProjectParameters.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(pack);
end;

procedure TfrmNewProjectParameters.clbTargetsClickCheck(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmNewProjectParameters.cmdBrowseClick(Sender: TObject);
begin
  dlgBrowse.InitialDir := editPath.Text;

  if dlgBrowse.Execute and (dlgBrowse.FileName <> '') then
    editPath.Text := ExcludeTrailingPathDelimiter(dlgBrowse.FileName);
end;

procedure TfrmNewProjectParameters.cmdKeyboardAddLanguageClick(
  Sender: TObject);
var
  k: TPackageKeyboard;
  lang: TPackageKeyboardLanguage;
  frm: TfrmSelectBCP47Language;
begin
  k := SelectedKeyboard;
  Assert(Assigned(k));

  frm := TfrmSelectBCP47Language.Create(Self);
  try
    if frm.ShowModal = mrOk then
    begin
      lang := TPackageKeyboardLanguage.Create(pack);
      lang.ID := frm.LanguageID;
      lang.Name := frm.LanguageName;
      k.Languages.Add(lang);
      LanguageGrid_Fill;
      gridKeyboardLanguages.Row := gridKeyboardLanguages.RowCount - 1;
      gridKeyboardLanguagesClick(gridKeyboardLanguages);
      EnableControls;
    end;
  finally
    frm.Free;
  end;
end;

procedure TfrmNewProjectParameters.cmdKeyboardEditLanguageClick(
  Sender: TObject);
var
  k: TPackageKeyboard;
  lang: TPackageKeyboardLanguage;
  frm: TfrmSelectBCP47Language;
begin
  k := SelectedKeyboard;
  Assert(Assigned(k));

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
      LanguageGrid_Fill;
      EnableControls;
    end;
  finally
    frm.Free;
  end;
end;

procedure TfrmNewProjectParameters.cmdKeyboardRemoveLanguageClick(
  Sender: TObject);
var
  k: TPackageKeyboard;
  lang: TPackageKeyboardLanguage;
begin
  k := SelectedKeyboard;
  Assert(Assigned(k));
  lang := SelectedKeyboardLanguage;
  Assert(Assigned(lang));

  k.Languages.Remove(lang);
  LanguageGrid_Fill;
  EnableControls;
end;

procedure TfrmNewProjectParameters.cmdOKClick(Sender: TObject);
begin
  if Validate then
    ModalResult := mrOk;
end;

procedure TfrmNewProjectParameters.editAuthorChange(Sender: TObject);
begin
  EnableControls;
  if not editCopyright.Modified then
    editCopyright.Text := 'Copyright '+Char($00A9 {copyright})+' '+Author;
  if not editFullCopyright.Modified then
    editFullCopyright.Text := 'Copyright '+Char($00A9 {copyright})+' '+FormatDateTime('yyyy', Now)+' '+Author;
end;

procedure TfrmNewProjectParameters.editCopyrightChange(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmNewProjectParameters.editFullCopyrightChange(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmNewProjectParameters.editKeyboardIDChange(Sender: TObject);
begin
  UpdateProjectFilename;
  EnableControls;
end;

procedure TfrmNewProjectParameters.editKeyboardNameChange(Sender: TObject);
begin
  if not editKeyboardID.Modified then
    editKeyboardID.Text := TKeyboardUtils.CleanKeyboardID(Trim(editKeyboardName.Text));
  EnableControls;
end;

procedure TfrmNewProjectParameters.editPathChange(Sender: TObject);
begin
  UpdateProjectFilename;
  EnableControls;
end;

procedure TfrmNewProjectParameters.editVersionChange(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmNewProjectParameters.EnableControls;
var
  e: Boolean;
begin
  e := (Trim(editKeyboardName.Text) <> '') and
    (Trim(editPath.Text) <> '') and
    TKeyboardUtils.IsValidKeyboardID(Trim(editKeyboardID.Text), True) and
    (GetTargets <> []) and
    (Trim(memoDescription.Text) <> '');

  cmdOK.Enabled := e;

  e := gridKeyboardLanguages.RowCount > 1;
  gridKeyboardLanguages.Enabled := e;
  cmdKeyboardRemoveLanguage.Enabled := e;
  cmdKeyboardEditLanguage.Enabled := e;
  if e then
    gridKeyboardLanguages.FixedRows := 1;
end;

function TfrmNewProjectParameters.GetAuthor: string;
begin
  Result := Trim(editAuthor.Text);
end;

function TfrmNewProjectParameters.GetBasePath: string;
begin
  Result := Trim(editPath.Text);
end;

function TfrmNewProjectParameters.GetCopyright: string;
begin
  Result := Trim(editCopyright.Text);
end;

function TfrmNewProjectParameters.GetDescription: string;
begin
  Result := Trim(memoDescription.Text);
end;

function TfrmNewProjectParameters.GetFullCopyright: string;
begin
  Result := Trim(editFullCopyright.Text);
end;

function TfrmNewProjectParameters.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_NewProjectParameters;
end;

function TfrmNewProjectParameters.GetKeyboardID: string;
begin
  Result := Trim(LowerCase(editKeyboardID.Text));
end;

function TfrmNewProjectParameters.GetKeyboardName: string;
begin
  Result := Trim(editKeyboardName.Text);
end;

function TfrmNewProjectParameters.GetBCP47Tags: string;
var
  lang: TPackageKeyboardLanguage;
begin
  Result := '';

  for lang in pack.Keyboards[0].Languages do
    Result := Result + lang.ID + ' ';

  Result := Result.Trim;
end;

function TfrmNewProjectParameters.GetTargets: TKeymanTargets;
var
  i: Integer;
begin
  Result := [];
  for i := 0 to clbTargets.Items.Count - 1 do
    if clbTargets.Checked[i] then
      Include(Result, TKeymanTarget(i));
end;

function TfrmNewProjectParameters.GetVersion: string;
begin
  Result := Trim(editVersion.Text);
end;

procedure TfrmNewProjectParameters.gridKeyboardLanguagesClick(
  Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmNewProjectParameters.gridKeyboardLanguagesDblClick(
  Sender: TObject);
begin
  if SelectedKeyboardLanguage <> nil then
    cmdKeyboardEditLanguage.Click;
end;

function TfrmNewProjectParameters.Validate: Boolean;
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

{ Languages Grid }

function TfrmNewProjectParameters.SelectedKeyboard: TPackageKeyboard;
begin
  Result := pack.Keyboards[0];
end;

function TfrmNewProjectParameters.SelectedKeyboardLanguage: TPackageKeyboardLanguage;
var
  k: TPackageKeyboard;
begin
  k := SelectedKeyboard;
  if not Assigned(k) then
    Exit(nil);

  if gridKeyboardLanguages.Row = 0 then
    Exit(nil);

  Result := gridKeyboardLanguages.Objects[0, gridKeyboardLanguages.Row] as TPackageKeyboardLanguage;
end;

procedure TfrmNewProjectParameters.SetBCP47Tags(const Value: string);
var
  Tag, Tags: string;
  BCP47Tag: TBCP47Tag;
  Language: TPackageKeyboardLanguage;
begin
  pack.Keyboards[0].Languages.Clear;
  Tags := Trim(Value);
  while Trim(Tags) <> '' do
  begin
    Tag := StrToken(Tags, ' ');
    BCP47Tag := TBCP47Tag.Create(Tag);
    try
      BCP47Tag.Tag := TCanonicalLanguageCodeUtils.FindBestTag(BCP47Tag.Tag, False, False);
      if BCP47Tag.IsValid(False) then
      begin
        Language := TPackageKeyboardLanguage.Create(pack);
        Language.ID := BCP47Tag.Tag;
        if not TLanguageCodeUtils.BCP47Languages.TryGetValue(BCP47Tag.Language, Language.Name) then
          Language.Name := Language.ID;
        pack.Keyboards[0].Languages.Add(Language);
      end;
    finally
      BCP47Tag.Free;
    end;
  end;
  LanguageGrid_Fill;
  EnableControls;
end;

procedure TfrmNewProjectParameters.SetKeyboardID(const Value: string);
begin
  editKeyboardID.Text := Value;
  EnableControls;
end;

procedure TfrmNewProjectParameters.SetKeyboardName(const Value: string);
begin
  editKeyboardName.Text := Value;
  EnableControls;
end;

procedure TfrmNewProjectParameters.LanguageGrid_Fill;
var
  k: TPackageKeyboard;
  i: Integer;
begin
  Inc(FSetup);
  try
    gridKeyboardLanguages.Cells[0, 0] := 'BCP 47 tag';
    gridKeyboardLanguages.Cells[1, 0] := 'Language name';
    gridKeyboardLanguages.ColWidths[0] := 120;
    gridKeyboardLanguages.ColWidths[1] := 10;

    k := SelectedKeyboard;
    if not Assigned(k) then
    begin
      gridKeyboardLanguages.RowCount := 1;
      EnableControls;
      Exit;
    end;

    gridKeyboardLanguages.RowCount := k.Languages.Count + 1;
    gridKeyboardLanguages.ColWidths[1] := gridKeyboardLanguages.ClientWidth - 120 - 1;

    for i := 0 to k.Languages.Count - 1 do
    begin
      gridKeyboardLanguages.Objects[0, i+1] := k.Languages[i];
      gridKeyboardLanguages.Cells[0, i+1] := k.Languages[i].ID;
      gridKeyboardLanguages.Cells[1, i+1] := k.Languages[i].Name;
    end;

    EnableControls;
  finally
    Dec(FSetup);
  end;
end;

procedure TfrmNewProjectParameters.memoDescriptionChange(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmNewProjectParameters.UpdateProjectFilename;
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
