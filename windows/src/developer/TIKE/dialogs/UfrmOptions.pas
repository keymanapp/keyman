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
  UfrmTike, UserMessages, PaintPanel;

type
  TfrmOptions = class(TTikeForm)
    pages: TPageControl;
    tabEditor: TTabSheet;
    tabColours: TTabSheet;
    cmdCancel: TButton;
    cmdOK: TButton;
    chkUseTab: TCheckBox;
    editIndentSize: TEdit;
    lblIndentSize: TLabel;
    panCol1: TPaintPanel;
    lbColours: TListBox;
    panCol2: TPaintPanel;
    panCol3: TPaintPanel;
    panCol4: TPaintPanel;
    panCol5: TPaintPanel;
    panCol6: TPaintPanel;
    panCol7: TPaintPanel;
    panCol8: TPaintPanel;
    panCol9: TPaintPanel;
    panCol10: TPaintPanel;
    panCol11: TPaintPanel;
    panCol12: TPaintPanel;
    panCol13: TPaintPanel;
    panCol14: TPaintPanel;
    panCol15: TPaintPanel;
    panCol16: TPaintPanel;
    cmdResetSelected: TButton;
    cmdResetAll: TButton;
    gbColourStyle: TGroupBox;
    chkColoursBold: TCheckBox;
    chkColoursItalic: TCheckBox;
    chkColoursUnderline: TCheckBox;
    chkUseSyntaxHighlighting: TCheckBox;
    cmdDefaultFont: TButton;
    dlgFonts: TFontDialog;
    panFontSample: TPanel;
    chkPlainBG: TCheckBox;
    chkPlainFG: TCheckBox;
    cmdQuotedFont: TButton;
    panQuotedFontSample: TPanel;
    chkLinkFontSizes: TCheckBox;
    tabGeneral: TTabSheet;
    pmSyntaxExample: TMemo;
    gbStartup: TGroupBox;
    chkShowStartupDialog: TCheckBox;
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
    chkAllowMultipleInstances: TCheckBox;
    cmdProxySettings: TButton;
    gbExternalEditor: TGroupBox;
    editExternalEditorPath: TEdit;
    cmdBrowseExternalEditor: TButton;
    dlgBrowse: TOpenDialog;
    TntLabel1: TLabel;
    editDatabasePath: TEdit;
    dlgBrowseUnicodeData: TOpenDialog;
    gbWebHost: TGroupBox;
    editWebHostDefaultPort: TEdit;
    lblWebHostDefaultPort: TLabel;
    cmdSMTPSettings: TButton;
    chkOpenKeyboardFilesInSourceView: TCheckBox;
    cmdResetToolWindows: TButton;
    procedure FormCreate(Sender: TObject);
    procedure cmdOKClick(Sender: TObject);
    procedure cmdDefaultFontClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cmdQuotedFontClick(Sender: TObject);
    procedure cmdCharMapRebuildDatabaseClick(Sender: TObject);
    procedure cmdProxySettingsClick(Sender: TObject);
    procedure editWebHostDefaultPortKeyPress(Sender: TObject; var Key: Char);
    procedure cmdSMTPSettingsClick(Sender: TObject);
    procedure cmdResetToolWindowsClick(Sender: TObject);
  private
    FDefaultFont, FQuotedFont: TFont;
    panCol: array[1..16] of TPaintPanel;
    { Private declarations }
  protected
    function GetHelpTopic: string; override;
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

uses
  Keyman.Developer.System.HelpTopics,

  JvDockControlForm,
  OnlineConstants,
  RedistFiles,
  ErrorControlledRegistry, 
  RegistryKeys,
  KeymanDeveloperOptions,
  KeymanDeveloperUtils,
  UfrmCharacterMapNew,
  UfrmMain,
  UfrmOnlineUpdateSetup,
  UfrmSMTPSetup,
  UnicodeData,
  utilstr;

{ General functions }

procedure TfrmOptions.FormCreate(Sender: TObject);
var
  deffont, altfont: string;
begin
  inherited;

  pages.ActivePage := tabGeneral;

  { Initialise Colours tab }
  panCol[1] := panCol1;
  panCol[2] := panCol2;
  panCol[3] := panCol3;
  panCol[4] := panCol4;
  panCol[5] := panCol5;
  panCol[6] := panCol6;
  panCol[7] := panCol7;
  panCol[8] := panCol8;
  panCol[9] := panCol9;
  panCol[10] := panCol10;
  panCol[11] := panCol11;
  panCol[12] := panCol12;
  panCol[13] := panCol13;
  panCol[14] := panCol14;
  panCol[15] := panCol15;
  panCol[16] := panCol16;

  lbColours.ItemIndex := 0;

  chkUseSyntaxHighlighting.Checked := False;
  chkUseSyntaxHighlighting.Enabled := False;

  { Initialise Editor tab }

  FDefaultFont := TFont.Create;
  FQuotedFont := TFont.Create;
  SetFontFromString(FDefaultFont, '');
  SetFontFromString(FQuotedFont, '');

  with FKeymanDeveloperOptions do
  begin
    chkUseOldDebugger.Checked := UseOldDebugger; // or not chkUseOldDebugger.Enabled;
    chkUseTab.Checked := UseTabChar;
    chkLinkFontSizes.Checked := LinkFontSizes;

    chkDebuggerBreakWhenExitingLine.Checked := DebuggerBreakWhenExitingLine;
    chkDebuggerSingleStepAfterBreak.Checked := DebuggerSingleStepAfterBreak;
    chkDebuggerShowStoreOffset.Checked :=      DebuggerShowStoreOffset;
    chkDebuggerAutoRecompile.Checked :=        DebuggerAutoRecompileWithDebugInfo;

    chkShowStartupDialog.Checked := ShowStartupDialog;// or not chkShowStartupDialog.Enabled;

    //chkShowStartupHelperDialog.Checked := ShowStartupHelperDialog;

    chkCharMapAutoLookup.Checked := CharMapAutoLookup;
    chkCharMapDisableDatabaseLookups.Checked := CharMapDisableDatabaseLookups;

    chkAllowMultipleInstances.Checked := AllowMultipleInstances;

    chkOpenKeyboardFilesInSourceView.Checked := OpenKeyboardFilesInSourceView;   // I4751

    editExternalEditorPath.Text := ExternalEditorPath;

    editIndentSize.Text := IntToStr(IndentSize);

    editWebHostDefaultPort.Text := IntToStr(WebHostDefaultPort);   // I4021

//    cbTheme.ItemIndex := cbTheme.Items.IndexOf(DisplayTheme);   // I4796
  end;

  editDatabasePath.Text := FUnicodeData.DBPath;

{  with TRegistryErrorControlled.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKeyReadOnly(SRegKey_KeymanDeveloper) then
      chkFileAssociations.Checked :=
        not ValueExists(SRegValue_NoCheckAssociations) or
        (ReadString(SRegValue_NoCheckAssociations) = '0')
    else
      chkFileAssociations.Checked := True;
  finally
    Free;
  end;}

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
  //cmdCharMapRebuildDatabase.Enabled := False;
  //gbCharMapCharacterDatabase.Enabled := False;
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
  with FKeymanDeveloperOptions do
  begin
    UseOldDebugger := chkUseOldDebugger.Checked;
    UseTabChar := chkUseTab.Checked;
    IndentSize := StrToIntDef(editIndentSize.Text, 4);
    LinkFontSizes := chkLinkFontSizes.Checked;

    DebuggerBreakWhenExitingLine       := chkDebuggerBreakWhenExitingLine.Checked;
    DebuggerSingleStepAfterBreak       := chkDebuggerSingleStepAfterBreak.Checked;
    DebuggerShowStoreOffset            := chkDebuggerShowStoreOffset.Checked;
    DebuggerAutoRecompileWithDebugInfo := chkDebuggerAutoRecompile.Checked;

    WebHostDefaultPort := StrToIntDef(editWebHostDefaultPort.Text, 8008);   // I4021

    ShowStartupDialog := chkShowStartupDialog.Checked;

    CharMapAutoLookup := chkCharMapAutoLookup.Checked;
    CharMapDisableDatabaseLookups := chkCharMapDisableDatabaseLookups.Checked;

    AllowMultipleInstances := chkAllowMultipleInstances.Checked;

    OpenKeyboardFilesInSourceView := chkOpenKeyboardFilesInSourceView.Checked;   // I4751

    ExternalEditorPath := editExternalEditorPath.Text;

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

  for i := 0 to frmKeymanDeveloper.MDIChildCount - 1 do
    PostMessage(frmKeymanDeveloper.MDIChildren[i].Handle, WM_USER_SYNTAXCOLOURCHANGE, 0, 0);

  ModalResult := mrOk;
end;

procedure TfrmOptions.cmdProxySettingsClick(Sender: TObject);
begin
  with TfrmOnlineUpdateSetup.Create(Self) do
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

procedure TfrmOptions.editWebHostDefaultPortKeyPress(Sender: TObject;
  var Key: Char);   // I4021
begin
  if not CharInSet(Key, ['0' .. '9']) then
  begin
    Key := #0;
  end;
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

end.

