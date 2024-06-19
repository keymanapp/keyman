(*
  Name:             UfrmOptions
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jun 2006

  Modified Date:    24 Jul 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    14 Sep 2006 - mcdurdin - Tweak character map calls
                    04 Dec 2006 - mcdurdin - Add Allow Multiple Instances
                    30 May 2007 - mcdurdin - Add Proxy settings dialog
                    30 May 2007 - mcdurdin - I825 - Added proxy username and password
                    23 Aug 2007 - mcdurdin - I927 - Add external editor
                    23 Aug 2007 - mcdurdin - I1014 - Fix rebuild character map
                    23 Aug 2007 - mcdurdin - Remove unused options 'check file associations, startup helper dialog'
                    14 Sep 2007 - mcdurdin - I1014 - Cancel if cancel is pressed when rebuilding charmap database
                    25 May 2010 - mcdurdin - I2392 - Activation Client integration
                    26 Jul 2010 - mcdurdin - I2467 - 8.0 renumber and remove Developer edition restrictions
                    18 Mar 2011 - mcdurdin - I2299 - Keyman Developer tries to write to UnicodeData.mdb which is stored in Program Files
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    18 May 2012 - mcdurdin - I3323 - V9.0 - Change from Plus-MemoU to Plus-Memo
                    10 Jan 2014 - mcdurdin - I4021 - V9.0 - Redesign Keyboard Wizard to integrate V9 features
                    04 Nov 2014 - mcdurdin - I4506 - V9.0 - Add command to send email with targets
                    22 Jun 2015 - mcdurdin - I4751 - Add "open in code view" default option for keyboards
                    24 Jul 2015 - mcdurdin - I4796 - Refresh Keyman Developer look and feel for release
*)
unit UfrmOptions;  // I3306  // I3323   // I4796

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
//  Themes,
  StdCtrls, ComCtrls, ExtCtrls,
  UfrmTike, UserMessages, PaintPanel, Browse4Folder;

type
  TfrmOptions = class(TTikeForm)
    pages: TPageControl;
    tabEditor: TTabSheet;
    cmdCancel: TButton;
    cmdOK: TButton;
    dlgFonts: TFontDialog;
    tabGeneral: TTabSheet;
    tabDebugger: TTabSheet;
    tabCharMap: TTabSheet;
    gbCharMapCharacterLookups: TGroupBox;
    chkCharMapAutoLookup: TCheckBox;
    chkCharMapDisableDatabaseLookups: TCheckBox;
    gbCharMapCharacterDatabase: TGroupBox;
    cmdCharMapRebuildDatabase: TButton;
    gbDebuggerSettings: TGroupBox;
    chkUseOldDebugger: TCheckBox;
    chkDebuggerBreakWhenExitingLine: TCheckBox;
    chkDebuggerSingleStepAfterBreak: TCheckBox;
    chkDebuggerShowStoreOffset: TCheckBox;
    chkDebuggerAutoRecompile: TCheckBox;
    cmdProxySettings: TButton;
    gbExternalEditor: TGroupBox;
    editExternalEditorPath: TEdit;
    cmdBrowseExternalEditor: TButton;
    dlgBrowse: TOpenDialog;
    TntLabel1: TLabel;
    editDatabasePath: TEdit;
    dlgBrowseUnicodeData: TOpenDialog;
    cmdSMTPSettings: TButton;
    chkOpenKeyboardFilesInSourceView: TCheckBox;
    cmdResetToolWindows: TButton;
    dlgBrowseEditorTheme: TOpenDialog;
    gbEditorSpaces: TGroupBox;
    gbEditorFonts: TGroupBox;
    gbEditorTheme: TGroupBox;
    chkUseTab: TCheckBox;
    lblIndentSize: TLabel;
    editIndentSize: TEdit;
    chkLinkFontSizes: TCheckBox;
    cmdQuotedFont: TButton;
    panQuotedFontSample: TPanel;
    panFontSample: TPanel;
    cmdDefaultFont: TButton;
    cbEditorTheme: TComboBox;
    lblEditorCustomTheme: TLabel;
    lblEditorTheme: TLabel;
    gbDefaultProjectPath: TGroupBox;
    editDefaultProjectPath: TEdit;
    cmdBrowseDefaultProjectPath: TButton;
    dlgBrowseDefaultProjectPath: TBrowse4Folder;
    chkAutoSaveBeforeCompiling: TCheckBox;
    chkOSKAutoSaveBeforeImporting: TCheckBox;
    chkDebuggerAutoResetBeforeCompiling: TCheckBox;
    gbPrivacy: TGroupBox;
    chkReportUsage: TCheckBox;
    chkReportErrors: TCheckBox;
    tabServer: TTabSheet;
    gbServer: TGroupBox;
    chkListLocalURLs: TCheckBox;
    cmdConfigureServer: TButton;
    chkPromptToUpgradeProjects: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure cmdOKClick(Sender: TObject);
    procedure cmdDefaultFontClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cmdQuotedFontClick(Sender: TObject);
    procedure cmdCharMapRebuildDatabaseClick(Sender: TObject);
    procedure cmdProxySettingsClick(Sender: TObject);
    procedure cmdSMTPSettingsClick(Sender: TObject);
    procedure cmdResetToolWindowsClick(Sender: TObject);
    procedure cbEditorThemeClick(Sender: TObject);
    procedure cmdBrowseDefaultProjectPathClick(Sender: TObject);
    procedure cmdConfigureServerClick(Sender: TObject);
  private
    FDefaultFont, FQuotedFont: TFont;
  protected
    function GetHelpTopic: string; override;
  public
    { Public declarations }
    procedure FocusServerTab;
  end;

implementation

{$R *.DFM}

uses
  System.JSON,
  System.UITypes,

  Keyman.Developer.System.HelpTopics,

  JsonUtil,
  JvDockControlForm,
  RedistFiles,
  ErrorControlledRegistry,
  RegistryKeys,
  KeymanDeveloperOptions,
  KeymanDeveloperUtils,
  UfrmCharacterMapNew,
  UfrmMain,
  Keyman.Developer.UI.UfrmServerOptions,
  Keyman.Developer.UI.UfrmTikeOnlineUpdateSetup,
  UfrmSMTPSetup,
  UnicodeData,
  utilstr;

{ General functions }


procedure TfrmOptions.FocusServerTab;
begin
  pages.ActivePage := tabServer;
end;

procedure TfrmOptions.FormCreate(Sender: TObject);
var
  deffont, altfont: string;
  i: Integer;
begin
  inherited;

  pages.ActivePage := tabGeneral;

  { Initialise Editor tab }

  FDefaultFont := TFont.Create;
  FQuotedFont := TFont.Create;
  SetFontFromString(FDefaultFont, '');
  SetFontFromString(FQuotedFont, '');

  for i := 0 to High(SDefaultEditorThemes) do
    cbEditorTheme.Items.Add(SDefaultEditorThemes[i].Desc);
  cbEditorTheme.Items.Add('Custom...');

  with FKeymanDeveloperOptions do
  begin
    chkUseOldDebugger.Checked := UseOldDebugger;
    chkUseTab.Checked := UseTabChar;
    chkLinkFontSizes.Checked := LinkFontSizes;

    chkDebuggerBreakWhenExitingLine.Checked := DebuggerBreakWhenExitingLine;
    chkDebuggerSingleStepAfterBreak.Checked := DebuggerSingleStepAfterBreak;
    chkDebuggerShowStoreOffset.Checked :=      DebuggerShowStoreOffset;
    chkDebuggerAutoRecompile.Checked :=        DebuggerAutoRecompileWithDebugInfo;

    chkDebuggerAutoResetBeforeCompiling.Checked := DebuggerAutoResetBeforeCompiling;
    chkAutoSaveBeforeCompiling.Checked :=          AutoSaveBeforeCompiling;
    chkOSKAutoSaveBeforeImporting.Checked :=       OSKAutoSaveBeforeImporting;
    chkPromptToUpgradeProjects.Checked :=          PromptToUpgradeProjects;

    chkCharMapAutoLookup.Checked := CharMapAutoLookup;
    chkCharMapDisableDatabaseLookups.Checked := CharMapDisableDatabaseLookups;

    chkOpenKeyboardFilesInSourceView.Checked := OpenKeyboardFilesInSourceView;   // I4751

    editExternalEditorPath.Text := ExternalEditorPath;
    editDefaultProjectPath.Text := DefaultProjectPath;

    editIndentSize.Text := IntToStr(IndentSize);

    chkListLocalURLs.Checked := ServerUseLocalAddresses;

    chkReportUsage.Checked := ReportUsage;
    chkReportErrors.Checked := ReportErrors;

    lblEditorCustomTheme.Caption := '';
    if EditorTheme = '' then
      cbEditorTheme.ItemIndex := 0
    else if IsDefaultEditorTheme(EditorTheme) then
      cbEditorTheme.ItemIndex := DefaultEditorThemeItemIndex(EditorTheme)
    else
    begin
      cbEditorTheme.ItemIndex := cbEditorTheme.Items.Count - 1; // custom
      lblEditorCustomTheme.Caption := EditorTheme;
    end;
  end;

  editDatabasePath.Text := FUnicodeData.DBPath;

  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKeyReadOnly(SRegKey_IDEEditFonts_CU) then  // I2890
    try
      SplitString(ReadString(''), deffont, altfont, #13);
      SetFontFromString(FDefaultFont, deffont);
      SetFontFromString(FQuotedFont, altfont);
    except
    end;
  finally
    Free;
  end;

  panFontSample.Caption := FDefaultFont.Name;
  panFontSample.Font := FDefaultFont;
  panQuotedFontSample.Caption := FQuotedFont.Name;
  panQuotedFontSample.Font := FQuotedFont;

  chkCharMapDisableDatabaseLookups.Checked := True;
  chkCharMapDisableDatabaseLookups.Enabled := False;
end;

procedure TfrmOptions.FormDestroy(Sender: TObject);
begin
  FDefaultFont.Free;
  FQuotedFont.Free;
end;

function TfrmOptions.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_Options;
end;

procedure TfrmOptions.cmdOKClick(Sender: TObject);
var
  i: Integer;
begin
  if not DirectoryExists(editDefaultProjectPath.Text) then
  begin
    if MessageDlg('The default project folder '+editDefaultProjectPath.Text+' does not exist. Do you want to create it now?',
      mtConfirmation, mbYesNoCancel, 0) <> mrYes then Exit;

    if not ForceDirectories(editDefaultProjectPath.Text) then
    begin
      ShowMessage('Keyman Developer was unable to create the default project folder '+editDefaultProjectPath.Text+
        '. The error was: '+SysErrorMessage(GetLastError));
      Exit;
    end;
  end;

  with FKeymanDeveloperOptions do
  begin
    UseOldDebugger := chkUseOldDebugger.Checked;
    UseTabChar := chkUseTab.Checked;
    IndentSize := StrToIntDef(editIndentSize.Text, 4);
    LinkFontSizes := chkLinkFontSizes.Checked;
    if cbEditorTheme.ItemIndex = cbEditorTheme.Items.Count - 1
      then EditorTheme := lblEditorCustomTheme.Caption
      else EditorTheme := SDefaultEditorThemes[cbEditorTheme.ItemIndex].Name;

    DebuggerBreakWhenExitingLine       := chkDebuggerBreakWhenExitingLine.Checked;
    DebuggerSingleStepAfterBreak       := chkDebuggerSingleStepAfterBreak.Checked;
    DebuggerShowStoreOffset            := chkDebuggerShowStoreOffset.Checked;
    DebuggerAutoRecompileWithDebugInfo := chkDebuggerAutoRecompile.Checked;

    DebuggerAutoResetBeforeCompiling := chkDebuggerAutoResetBeforeCompiling.Checked;
    AutoSaveBeforeCompiling          := chkAutoSaveBeforeCompiling.Checked;
    OSKAutoSaveBeforeImporting       := chkOSKAutoSaveBeforeImporting.Checked;
    PromptToUpgradeProjects          := chkPromptToUpgradeProjects.Checked;

    ServerUseLocalAddresses := chkListLocalURLs.Checked;

    CharMapAutoLookup := chkCharMapAutoLookup.Checked;
    CharMapDisableDatabaseLookups := chkCharMapDisableDatabaseLookups.Checked;

    OpenKeyboardFilesInSourceView := chkOpenKeyboardFilesInSourceView.Checked;   // I4751

    ExternalEditorPath := editExternalEditorPath.Text;
    DefaultProjectPath := IncludeTrailingPathDelimiter(editDefaultProjectPath.Text);

    ReportUsage := chkReportUsage.Checked;
    ReportErrors := chkReportErrors.Checked;

    Write;
  end;

  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_CURRENT_USER;
    if not OpenKey(SRegKey_IDEEditFonts_CU, True) then  // I2890
      RaiseLastRegistryError;
    WriteString('', FontAsString(FDefaultFont)+#13+FontAsString(FQuotedFont));
  finally
    Free;
  end;

  for i := 0 to Screen.FormCount - 1 do
    PostMessage(Screen.Forms[i].Handle, WM_USER_SYNTAXCOLOURCHANGE, 0, 0);

  ModalResult := mrOk;
end;

procedure TfrmOptions.cmdProxySettingsClick(Sender: TObject);
begin
  with TfrmTikeOnlineUpdateSetup.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

{ Options page }

{ Colours page }

procedure TfrmOptions.cmdDefaultFontClick(Sender: TObject);
begin
  dlgFonts.Font := FDefaultFont;
  if dlgFonts.Execute then
  begin
    FDefaultFont.Assign(dlgFonts.Font);
    panFontSample.Caption := FDefaultFont.Name;
    panFontSample.Font := FDefaultFont;
  end;
end;

procedure TfrmOptions.cmdQuotedFontClick(Sender: TObject);
begin
  dlgFonts.Font := FQuotedFont;
  if dlgFonts.Execute then
  begin
    FQuotedFont.Assign(dlgFonts.Font);
    panQuotedFontSample.Caption := FQuotedFont.Name;
    panQuotedFontSample.Font := FQuotedFont;
  end;
end;

procedure TfrmOptions.cmdResetToolWindowsClick(Sender: TObject);
begin
  frmKeymanDeveloper.DefaultDockLayout;
  Self.BringToFront;
end;

procedure TfrmOptions.cmdSMTPSettingsClick(Sender: TObject);   // I4506
begin
  with TfrmSMTPSetup.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfrmOptions.cbEditorThemeClick(Sender: TObject);
var
  j: TJSONObject;
  offset: Integer;
begin
  inherited;
  if cbEditorTheme.ItemIndex = cbEditorTheme.Items.Count - 1 then
  begin
    dlgBrowseEditorTheme.FileName := lblEditorCustomTheme.Caption;
    if not dlgBrowseEditorTheme.Execute then
    begin
      cbEditorTheme.ItemIndex := 0;
      lblEditorCustomTheme.Caption := '';
      Exit;
    end;

    // Validate the theme file, somewhat anyway

    try
      j := LoadJSONFromFile(dlgBrowseEditorTheme.FileName, offset);
      if j = nil then
        raise Exception.CreateFmt('An error was encountered at offset %d', [offset]);
      j.Free;
    except
      on E:Exception do
      begin
        ShowMessage('The file '+dlgBrowseEditorTheme.FileName+' is not a valid JSON file: '+E.Message);
        cbEditorTheme.ItemIndex := 0;
        lblEditorCustomTheme.Caption := '';
        Exit;
      end;
    end;

    lblEditorCustomTheme.Caption := dlgBrowseEditorTheme.FileName;
  end
  else
    lblEditorCustomTheme.Caption := '';
end;

procedure TfrmOptions.cmdBrowseDefaultProjectPathClick(Sender: TObject);
begin
  dlgBrowseDefaultProjectPath.InitialDir := editDefaultProjectPath.Text;
  if dlgBrowseDefaultProjectPath.Execute then
    editDefaultProjectPath.Text := dlgBrowseDefaultProjectPath.FileName;
end;

procedure TfrmOptions.cmdCharMapRebuildDatabaseClick(Sender: TObject);
var
  FUnicodeSourcePath: WideString;
begin
  if dlgBrowseUnicodeData.Execute then
  begin
    FUnicodeSourcePath := ExtractFilePath(dlgBrowseUnicodeData.FileName);

    if not FileExists(FUnicodeSourcePath + 'unicodedata.txt') or
      not FileExists(FUnicodeSourcePath + 'blocks.txt') then
    begin
      ShowMessage('The files unicodedata.txt and blocks.txt could not be found at the path '+FUnicodeSourcePath+'.  These files can be downloaded from http://www.unicode.org/');
      Exit;
    end;

    FUnicodeData.BuildDatabase(FUnicodeSourcePath);
    if Assigned(frmCharacterMapNew) then
      frmCharacterMapNew.Reload;

    editDatabasePath.Text := FUnicodeData.DBPath; // I2299
  end;
end;

procedure TfrmOptions.cmdConfigureServerClick(Sender: TObject);
var
  serverOptionsForm: TfrmServerOptions;
begin
  serverOptionsForm := TfrmServerOptions.Create(Self);
  try
    serverOptionsForm.ShowModal;
  finally
    serverOptionsForm.Free;
  end;
end;

end.

