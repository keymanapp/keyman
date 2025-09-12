unit Keyman.Developer.UI.Project.UfrmNewLDMLKeyboardProjectParameters;

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

  Keyman.Developer.UI.FormValidation,
  UfrmTike,
  utilfiletypes;

type
  TfrmNewLDMLKeyboardProjectParameters = class(TTikeForm)
    lblKeyboardID: TLabel;
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
    lblKeyboardLanguages: TLabel;
    lblProjectFilename: TLabel;
    editProjectFilename: TEdit;
    lblFullCopyright: TLabel;
    editFullCopyright: TEdit;
    lblDescription: TLabel;
    memoDescription: TMemo;
    memoLanguages: TMemo;
    lblLanguages: TLabel;
    Label4: TLabel;
    lblKeyboardNameValidation: TLabel;
    lblDescriptionValidation: TLabel;
    lblCopyrightValidation: TLabel;
    lblVersionValidation: TLabel;
    lblAuthorValidation: TLabel;
    lblFullCopyrightValidation: TLabel;
    lblLanguagesValidation: TLabel;
    lblPathValidation: TLabel;
    lblKeyboardIDValidation: TLabel;
    lblProjectFilenameValidation: TLabel;
    lblDescriptionMarkdown: TLabel;
    procedure cmdOKClick(Sender: TObject);
    procedure editKeyboardNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure editCopyrightChange(Sender: TObject);
    procedure editVersionChange(Sender: TObject);
    procedure editAuthorChange(Sender: TObject);
    procedure editPathChange(Sender: TObject);
    procedure editKeyboardIDChange(Sender: TObject);
    procedure cmdBrowseClick(Sender: TObject);
    procedure editFullCopyrightChange(Sender: TObject);
    procedure memoDescriptionChange(Sender: TObject);
    procedure memoLanguagesChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ControlExit(Sender: TObject);
  private
    FormValidation: TFormValidation;
    dlgBrowse: TBrowse4Folder;
    function GetAuthor: string;
    function GetBasePath: string;
    function GetCopyright: string;
    function GetKeyboardID: string;
    function GetKeyboardName: string;
    function GetBCP47Tags: string;
    function GetVersion: string;
    function Validate: Boolean;
    procedure EnableControls(ChangingControl: TControl);
    procedure SetBCP47Tags(const Value: string);
    procedure SetKeyboardID(const Value: string);
    procedure SetKeyboardName(const Value: string);
    procedure UpdateProjectFilename;
    function GetFullCopyright: string;
    function GetDescription: string;
    procedure SetupValidation;
  protected
    function GetHelpTopic: string; override;
  public
    property Copyright: string read GetCopyright;
    property FullCopyright: string read GetFullCopyright;
    property Version: string read GetVersion;
    property Author: string read GetAuthor;
    property BCP47Tags: string read GetBCP47Tags write SetBCP47Tags;
    property BasePath: string read GetBasePath;

    property Description: string read GetDescription;

    property KeyboardName: string read GetKeyboardName write SetKeyboardName;
    property KeyboardID: string read GetKeyboardID write SetKeyboardID;
  end;

function ShowNewLDMLKeyboardProjectParameters(Owner: TComponent): Boolean;

implementation

uses
  System.RegularExpressions,

  Keyman.System.CanonicalLanguageCodeUtils,
  Keyman.System.LanguageCodeUtils,
  BCP47Tag,
  utilstr,
  KeymanDeveloperOptions,
  Keyman.Developer.System.HelpTopics,
  Keyman.Developer.System.Project.Project,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.LDMLKeyboardProjectTemplate,
  Keyman.Developer.System.ProjectTemplate,
  Keyman.System.KeyboardUtils,
  UfrmMain;

// 1. project filename
// 2. location

// 3. keyboard3.locale
// 4. locales additional/alternate locales
// 5. info.name = keyboard name
// 6. info.author
// skip: 7. info.layout
// skip: 8. info.indicator

// Package metadata
// 1. copyright
// 2. description

{$R *.dfm}

function ShowNewLDMLKeyboardProjectParameters(Owner: TComponent): Boolean;
var
  f: TfrmNewLDMLKeyboardProjectParameters;
  pt: TLDMLKeyboardProjectTemplate;
begin
  f := TfrmNewLDMLKeyboardProjectParameters.Create(Owner);
  try
    Result := f.ShowModal = mrOk;
    if not Result then
      Exit;

    pt := TLDMLKeyboardProjectTemplate.Create(f.BasePath, f.KeyboardID);
    try
      pt.Name := f.KeyboardName;
      pt.Copyright := f.Copyright;
      pt.FullCopyright := f.FullCopyright;
      pt.Author := f.Author;
      pt.Version := f.Version;
      pt.Description := f.Description;
      pt.BCP47Tags := f.BCP47Tags;

      try
        pt.Generate;
      except
        on E:ELDMLKeyboardProjectTemplate do
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

{ TfrmNewLDMLKeyboardProjectParameters }

procedure TfrmNewLDMLKeyboardProjectParameters.FormCreate(Sender: TObject);
begin
  inherited;

  SetupValidation;

  editPath.Text := FKeymanDeveloperOptions.DefaultProjectPath;
  editFullCopyright.Text := 'Copyright '+Char($00A9 {copyright})+' '+FormatDateTime('yyyy', Now)+' ';

  dlgBrowse := TBrowse4Folder.Create(Self);
  dlgBrowse.InitialDir := editPath.Text;
  dlgBrowse.Options := [OnlySelectFileSysDir, ShowEditBox, UseNewDialogStyle];
  dlgBrowse.Root := Desktop;
  dlgBrowse.Title := 'Select folder to save project to';

  EnableControls(nil);
end;

procedure TfrmNewLDMLKeyboardProjectParameters.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(FormValidation);
end;

procedure TfrmNewLDMLKeyboardProjectParameters.cmdBrowseClick(Sender: TObject);
begin
  dlgBrowse.InitialDir := editPath.Text;

  if dlgBrowse.Execute and (dlgBrowse.FileName <> '') then
    editPath.Text := ExcludeTrailingPathDelimiter(dlgBrowse.FileName);
end;

procedure TfrmNewLDMLKeyboardProjectParameters.cmdOKClick(Sender: TObject);
begin
  FormValidation.TouchAllFields;
  if Validate then
    ModalResult := mrOk;
end;

procedure TfrmNewLDMLKeyboardProjectParameters.editAuthorChange(Sender: TObject);
begin
  if not editCopyright.Modified then
  begin
    editCopyright.Text := 'Copyright '+Char($00A9 {copyright})+' '+Author;
    FormValidation.TouchField(editCopyright);
  end;
  if not editFullCopyright.Modified then
  begin
    editFullCopyright.Text := 'Copyright '+Char($00A9 {copyright})+' '+FormatDateTime('yyyy', Now)+' '+Author;
    FormValidation.TouchField(editFullCopyright);
  end;
  EnableControls(editAuthor);
end;

procedure TfrmNewLDMLKeyboardProjectParameters.editCopyrightChange(Sender: TObject);
begin
  EnableControls(editCopyright);
end;

procedure TfrmNewLDMLKeyboardProjectParameters.editFullCopyrightChange(Sender: TObject);
begin
  EnableControls(editFullCopyright);
end;

procedure TfrmNewLDMLKeyboardProjectParameters.editKeyboardIDChange(Sender: TObject);
begin
  UpdateProjectFilename;
  EnableControls(editKeyboardID);
end;

procedure TfrmNewLDMLKeyboardProjectParameters.editKeyboardNameChange(Sender: TObject);
begin
  if not editKeyboardID.Modified then
  begin
    editKeyboardID.Text := TKeyboardUtils.CleanKeyboardID(Trim(editKeyboardName.Text));
    FormValidation.TouchField(editKeyboardID);
  end;
  EnableControls(editKeyboardName);
end;

procedure TfrmNewLDMLKeyboardProjectParameters.ControlExit(Sender: TObject);
begin
  EnableControls(Sender as TControl);
end;

procedure TfrmNewLDMLKeyboardProjectParameters.editPathChange(Sender: TObject);
begin
  UpdateProjectFilename;
  EnableControls(editPath);
end;

procedure TfrmNewLDMLKeyboardProjectParameters.editVersionChange(Sender: TObject);
begin
  EnableControls(editVersion);
end;

function IsValidSemanticVersion(const s: string): Boolean;
begin
  // For now, only supports triplet version due to limits in .kps
  Result := TRegEx.Create('^\d+\.\d+\.\d+$').IsMatch(s);
end;

function TfrmNewLDMLKeyboardProjectParameters.GetAuthor: string;
begin
  Result := Trim(editAuthor.Text);
end;

function TfrmNewLDMLKeyboardProjectParameters.GetBasePath: string;
begin
  Result := Trim(editPath.Text);
end;

function TfrmNewLDMLKeyboardProjectParameters.GetCopyright: string;
begin
  Result := Trim(editCopyright.Text);
end;

function TfrmNewLDMLKeyboardProjectParameters.GetDescription: string;
begin
  Result := Trim(memoDescription.Text);
end;

function TfrmNewLDMLKeyboardProjectParameters.GetFullCopyright: string;
begin
  Result := Trim(editFullCopyright.Text);
end;

function TfrmNewLDMLKeyboardProjectParameters.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_NewLdmlProjectParameters;
end;

function TfrmNewLDMLKeyboardProjectParameters.GetKeyboardID: string;
begin
  Result := Trim(LowerCase(editKeyboardID.Text));
end;

function TfrmNewLDMLKeyboardProjectParameters.GetKeyboardName: string;
begin
  Result := Trim(editKeyboardName.Text);
end;

function TfrmNewLDMLKeyboardProjectParameters.GetBCP47Tags: string;
begin
  Result := memoLanguages.Text;
  Result := Result.Replace(#13#10, ' ');
  Result := Result.Trim;
end;

function TfrmNewLDMLKeyboardProjectParameters.GetVersion: string;
begin
  Result := Trim(editVersion.Text);
end;

function TfrmNewLDMLKeyboardProjectParameters.Validate: Boolean;
var
  ProjectFolder: string;
begin
  if not FormValidation.Update then
  begin
    ShowMessage('One or more fields are missing or invalid. Please correct these fields and try again.');
    Exit(False);
  end;

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

procedure TfrmNewLDMLKeyboardProjectParameters.SetBCP47Tags(const Value: string);
begin
  memoLanguages.Text := Value.Replace(' ', #13#10);
  EnableControls(nil);
end;

procedure TfrmNewLDMLKeyboardProjectParameters.SetKeyboardID(const Value: string);
begin
  editKeyboardID.Text := Value;
  EnableControls(nil);
end;

procedure TfrmNewLDMLKeyboardProjectParameters.SetKeyboardName(const Value: string);
begin
  editKeyboardName.Text := Value;
  EnableControls(nil);
end;

procedure TfrmNewLDMLKeyboardProjectParameters.memoDescriptionChange(Sender: TObject);
begin
  EnableControls(memoDescription);
end;

procedure TfrmNewLDMLKeyboardProjectParameters.memoLanguagesChange(
  Sender: TObject);
begin
  EnableControls(memoLanguages);
end;

procedure TfrmNewLDMLKeyboardProjectParameters.UpdateProjectFilename;
begin
  editProjectFilename.Text :=
    IncludeTrailingPathDelimiter(BasePath) +
    KeyboardID + PathDelim +
    KeyboardID + Ext_ProjectSource;
  // Scroll to the end of the control to show the filename
  editProjectFilename.Perform(EM_SETSEL, Length(editProjectFilename.Text), Length(editProjectFilename.Text));
  editProjectFilename.Perform(EM_SCROLLCARET, 0, 0);
end;

////
//// Validation
////

procedure TfrmNewLDMLKeyboardProjectParameters.SetupValidation;
begin
  FormValidation := TFormValidation.Create;

  FormValidation.Add(lblKeyboardName,      lblKeyboardNameValidation,    editKeyboardName);
  FormValidation.Add(lblDescription,       lblDescriptionValidation,     memoDescription);
  FormValidation.Add(lblAuthor,            lblAuthorValidation,          editAuthor);
  FormValidation.Add(lblCoypright,         lblCopyrightValidation,       editCopyright);
  FormValidation.Add(lblFullCopyright,     lblFullCopyrightValidation,   editFullCopyright);
  FormValidation.Add(lblVersion,           lblVersionValidation,         editVersion);
  FormValidation.Add(lblKeyboardLanguages, lblLanguagesValidation,       memoLanguages);
  FormValidation.Add(lblPath,              lblPathValidation,            editPath);
  FormValidation.Add(lblKeyboardID,        lblKeyboardIDValidation,      editKeyboardID);
  FormValidation.Add(lblProjectFilename,   lblProjectFilenameValidation, editProjectFilename);

  FormValidation.AddMethod(
    editKeyboardName,
    function: Boolean begin Result := Trim(editKeyboardName.Text) <> '' end,
    'Keyboard name cannot be empty'
  );

  FormValidation.AddMethod(
    editPath,
    function: Boolean begin Result := (Trim(editPath.Text) <> '') end,
    'Keyboard path cannot be empty'
  );

  FormValidation.AddMethod(
    editKeyboardID,
    function: Boolean begin Result := Trim(editKeyboardID.Text) <> '' end,
    'Keyboard ID cannot be empty'
  );

  FormValidation.AddMethod(
    editKeyboardID,
    function: Boolean begin Result := TKeyboardUtils.IsValidKeyboardID(Trim(editKeyboardID.Text), True) end,
    'Keyboard ID can include only a-z, 0-9, and _, and must not start with a digit'
  );

  FormValidation.AddMethod(
    memoDescription,
    function: Boolean begin Result := Trim(memoDescription.Text) <> '' end,
    'Description cannot be empty'
  );

  FormValidation.AddMethod(
    editVersion,
    function: Boolean begin Result := IsValidSemanticVersion(Trim(editVersion.Text)) end,
    'Version must follow a "1.2.3" semantic version format'
  );

  FormValidation.AddMethod(
    memoLanguages,
    function: Boolean begin Result := Trim(memoLanguages.Text) <> '' end,
    'Languages list cannot be empty'
  );
end;

procedure TfrmNewLDMLKeyboardProjectParameters.EnableControls(ChangingControl: TControl);
begin
  if Assigned(ChangingControl) then
  begin
    FormValidation.TouchField(ChangingControl);
  end;
  FormValidation.Update;
end;

end.
