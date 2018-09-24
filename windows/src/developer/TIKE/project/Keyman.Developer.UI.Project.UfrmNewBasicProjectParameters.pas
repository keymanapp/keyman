unit Keyman.Developer.UI.Project.UfrmNewBasicProjectParameters;

interface

uses
  System.Classes,
  System.SysUtils,
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
  UKeymanTargets;

type
  TfrmNewBasicProjectParameters = class(TTikeForm)
    lblFileName: TLabel;
    lblPath: TLabel;
    editFileName: TEdit;
    cmdBrowse: TButton;
    editPath: TEdit;
    dlgSave: TSaveDialog;
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
    procedure cmdOKClick(Sender: TObject);
    procedure editKeyboardNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure gridKeyboardLanguagesClick(Sender: TObject);
    procedure gridKeyboardLanguagesDblClick(Sender: TObject);
    procedure cmdKeyboardAddLanguageClick(Sender: TObject);
    procedure cmdKeyboardEditLanguageClick(Sender: TObject);
    procedure cmdKeyboardRemoveLanguageClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    pack: TKPSFile;
    FSetup: Integer; // Used temporarily for storing language list
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
  public
    property KeyboardName: string read GetKeyboardName;
    property Copyright: string read GetCopyright;
    property Version: string read GetVersion;
    property Author: string read GetAuthor;
    property Targets: TKeymanTargets read GetTargets;
    property BCP47Tags: string read GetBCP47Tags;
    property BasePath: string read GetBasePath;
    property KeyboardID: string read GetKeyboardID;
  end;

function ShowNewBasicProject(Owner: TComponent): Boolean;

implementation

uses
  Winapi.ShlObj,

  UfrmMain,
  utilsystem,
  Keyman.Developer.System.Project.Project,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.KeyboardProjectTemplate,
  Keyman.Developer.UI.Project.ProjectUI,
  Keyman.Developer.UI.UfrmSelectBCP47Language,
  Keyman.System.KeyboardUtils;

// 1. project filename
// 2. location
// 3. keyboard name
// 4. copyright
// 5. targets
// 6. author
// 7. version
// 8. bcp47 tags

{$R *.dfm}

function ShowNewBasicProject(Owner: TComponent): Boolean;
var
  f: TfrmNewBasicProjectParameters;
  kpt: TKeyboardProjectTemplate;
begin
  f := TfrmNewBasicProjectParameters.Create(Owner);
  try
    Result := f.ShowModal = mrOk;
    if not Result then
      Exit;

    kpt := TKeyboardProjectTemplate.Create(f.BasePath, f.KeyboardID, f.Targets);
    try
      kpt.Name := f.KeyboardName;
      kpt.Copyright := f.Copyright;
      kpt.Author := f.Author;
      kpt.Version := f.Version;
      kpt.BCP47Tags := f.BCP47Tags;
      kpt.Generate;

      FGlobalProject.Save;
      frmKeymanDeveloper.ProjectForm.Free;
      FreeGlobalProjectUI;
      LoadGlobalProjectUI(kpt.ProjectFilename);
      frmKeymanDeveloper.ShowProject;
      Result := True;

    finally
      kpt.Free;
    end;
  finally
    f.Free;
  end;
end;

{ TfrmNewBasicProjectParameters }

procedure TfrmNewBasicProjectParameters.FormCreate(Sender: TObject);
var
  i: TKeymanTarget;
begin
  inherited;
  editPath.Text := GetFolderPath(CSIDL_PERSONAL);

  pack := TKPSFile.Create;
  pack.Keyboards.Add(TPackageKeyboard.Create(pack));

  for i := Low(TKeymanTarget) to High(TKeymanTarget) do   // I4504
    clbTargets.Items.Add(SKeymanTargets[i]);

  clbTargets.Checked[0] := True;

  LanguageGrid_Fill;
  EnableControls;
end;

procedure TfrmNewBasicProjectParameters.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(pack);
end;

procedure TfrmNewBasicProjectParameters.cmdKeyboardAddLanguageClick(
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

procedure TfrmNewBasicProjectParameters.cmdKeyboardEditLanguageClick(
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

procedure TfrmNewBasicProjectParameters.cmdKeyboardRemoveLanguageClick(
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

procedure TfrmNewBasicProjectParameters.cmdOKClick(Sender: TObject);
begin
  if Validate then
    ModalResult := mrOk;
end;

procedure TfrmNewBasicProjectParameters.editKeyboardNameChange(Sender: TObject);
begin
  inherited;
  if not editFileName.Modified then
    editFileName.Text := TKeyboardUtils.CleanKeyboardID(Trim(editKeyboardName.Text));
end;

procedure TfrmNewBasicProjectParameters.EnableControls;
var
  e: Boolean;
begin
  e := (Trim(editKeyboardName.Text) <> '') and
    (Trim(editPath.Text) <> '') and
    (Trim(editFileName.Text) <> '');
  cmdOK.Enabled := e;

  e := gridKeyboardLanguages.Row > 0;
  gridKeyboardLanguages.Enabled := e;
  cmdKeyboardRemoveLanguage.Enabled := e;
  cmdKeyboardEditLanguage.Enabled := e;
  if e then
    gridKeyboardLanguages.FixedRows := 1;
end;

function TfrmNewBasicProjectParameters.GetAuthor: string;
begin
  Result := Trim(editAuthor.Text);
end;

function TfrmNewBasicProjectParameters.GetBasePath: string;
begin
  Result := Trim(editPath.Text);
end;

function TfrmNewBasicProjectParameters.GetCopyright: string;
begin
  Result := Trim(editCopyright.Text);
end;

function TfrmNewBasicProjectParameters.GetKeyboardID: string;
begin
  Result := Trim(LowerCase(editFileName.Text));
end;

function TfrmNewBasicProjectParameters.GetKeyboardName: string;
begin
  Result := Trim(editKeyboardName.Text);
end;

function TfrmNewBasicProjectParameters.GetBCP47Tags: string;
var
  lang: TPackageKeyboardLanguage;
begin
  Result := '';

  for lang in pack.Keyboards[0].Languages do
    Result := Result + lang.ID + ' ';

  Result := Result.Trim;
end;

function TfrmNewBasicProjectParameters.GetTargets: TKeymanTargets;
var
  i: Integer;
begin
  Result := [];
  for i := 0 to clbTargets.Items.Count - 1 do
    if clbTargets.Checked[i] then
      Include(Result, TKeymanTarget(i));
end;

function TfrmNewBasicProjectParameters.GetVersion: string;
begin
  Result := Trim(editVersion.Text);
end;

procedure TfrmNewBasicProjectParameters.gridKeyboardLanguagesClick(
  Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmNewBasicProjectParameters.gridKeyboardLanguagesDblClick(
  Sender: TObject);
begin
  if SelectedKeyboardLanguage <> nil then
    cmdKeyboardEditLanguage.Click;
end;

function TfrmNewBasicProjectParameters.Validate: Boolean;
begin
  Result := TKeyboardUtils.IsValidKeyboardID(Trim(editFileName.Text));
end;

{ Languages Grid }

function TfrmNewBasicProjectParameters.SelectedKeyboard: TPackageKeyboard;
begin
  Result := pack.Keyboards[0];
end;

function TfrmNewBasicProjectParameters.SelectedKeyboardLanguage: TPackageKeyboardLanguage;
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

procedure TfrmNewBasicProjectParameters.LanguageGrid_Fill;
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


end.
