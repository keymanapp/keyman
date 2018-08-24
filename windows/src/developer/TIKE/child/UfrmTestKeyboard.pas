(*
  Name:             UfrmTestKeyboard
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    24 Jul 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Rework as tab child
                    02 Aug 2006 - mcdurdin - Rework menus as sp-TBX
                    23 Aug 2006 - mcdurdin - Refactor menus
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3323 - V9.0 - Change from Plus-MemoU to Plus-Memo(Debugger)
                    08 Jun 2012 - mcdurdin - I3337 - V9.0 - Review of input/output for Unicode
                    24 Jul 2015 - mcdurdin - I4796 - Refresh Keyman Developer look and feel for release
*)
unit UfrmTestKeyboard;  // I3323   // I4796

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, int_kmdebug, Menus, ErrorControlledRegistry, 
  RegistryKeys, UfrmMDIChild, KeymanDeveloperDebuggerMemo;

type
  TfrmTestKeyboard = class(TfrmTikeChild)
    panFontInfo: TPanel;
    dlgFont: TFontDialog;
    HSplitter: TSplitter;
    lbDebug: TListBox;
    chkDebug: TCheckBox;
    mnuDebugPopup: TPopupMenu;
    mnuDebugClear: TMenuItem;
    mnuDebugSaveToFile: TMenuItem;
    dlgSave: TSaveDialog;
    chkTestKeyboard: TCheckBox;
    rbANSI: TRadioButton;
    rbUnicode: TRadioButton;
    memo: TKeymanDeveloperDebuggerMemo;
    procedure chkDebugClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mnuDebugClearClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mnuDebugSaveToFileClick(Sender: TObject);
    procedure memoGotFocus(Sender: TObject);
    procedure memoLostFocus(Sender: TObject);
    procedure chkTestKeyboardClick(Sender: TObject);
    procedure mnuEditCutClick(Sender: TObject);
    procedure mnuEditCopyClick(Sender: TObject);
    procedure mnuEditPasteClick(Sender: TObject);
    procedure mnuViewFontClick(Sender: TObject);
    procedure mnuViewDefaultFontClick(Sender: TObject);
    procedure rbANSIClick(Sender: TObject);
    procedure rbUnicodeClick(Sender: TObject);
    procedure mnuViewClick(Sender: TObject);
    procedure memoSelMove(Sender: TObject);
    procedure mnuDebugClick(Sender: TObject);
    procedure mnuKeyboardDebugClick(Sender: TObject);
    procedure mnuProgramDebugClick(Sender: TObject);
    procedure mnuMessageDebugClick(Sender: TObject);
    procedure mnuInternatDebugClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure mnuDebugShowDebugPaneClick(Sender: TObject);
    procedure mnuDebugTestKeyboardClick(Sender: TObject);
    procedure mnuDebugANSIClick(Sender: TObject);
    procedure mnuDebugUnicodeClick(Sender: TObject);
    procedure mnuViewCharacterMapClick(Sender: TObject);
  private
    //editTest, editHide: TCustomControl;
    FAnsiFont, FUnicodeFont: string;
    FCannotTestKeyboard: Boolean;
    FInGotFocus: Boolean;
    FDebugObject: TDebugKeyboard;
    FDebugFlags: Integer;
    FTesting: Boolean;
    FKeyboardName: string;
    procedure SetTesting(const Value: Boolean);
    procedure SetKeyboardName(Value: string);
    procedure DebugMessage(Sender: TObject; hwnd, State, LineNo: Integer; const msg: string);
    procedure SetEditKeyboardState;
    procedure LoadFont;
    procedure SaveFont;
    procedure FocusTestWindow;
    procedure ShowEditControl;
  protected
    function GetHelpTopic: string; override;
  public
    procedure Reload;
    property KeyboardName: string read FKeyboardName write SetKeyboardName;
    property Testing: Boolean read FTesting write SetTesting;
  end;

implementation

uses
  Keyman.Developer.System.HelpTopics,

  KeymanDeveloperUtils,
  kmxfile,
  UfrmMessages,
  keyman32_int,
  UfrmMain,
  utilstr;

{$R *.DFM}

procedure TfrmTestKeyboard.SetKeyboardName(Value: string);
var
  ki: TKeyboardInfo;
begin
  FKeyboardName := Value;
  try
    GetKeyboardInfo(FKeyboardName, False, ki);
  except
    Exit;
  end;

  rbANSI.Enabled := keANSI in ki.Encodings;
  rbUnicode.Enabled := (keUnicode in ki.Encodings);
  //(GetOS in [osWinNT35, osWinNT4, osWin2000, osOther]);

  if rbANSI.Enabled then rbANSI.Checked := True else rbUnicode.Checked := True;

  Caption := 'Test - ' + ExtractFileName(FKeyboardName);
  LoadFont;
end;

procedure TfrmTestKeyboard.SetEditKeyboardState;
begin
  if FCannotTestKeyboard then Exit;
  if not FTesting then
    Keyman_StopForcingKeyboard
  else
  begin
    try
      if not Keyman_ForceKeyboard(FKeyboardName) then
      begin
        FCannotTestKeyboard := True;
        ShowMessage('Could not test keyboard.');
        Close;
      end;
    except
(*      on E:EKeymanNotInstalled do
      begin
        FCannotTestKeyboard := True;
        ShowMessage(E.Message);
        Close;
      end;*)
    end;
  end;
end;

procedure TfrmTestKeyboard.chkDebugClick(Sender: TObject);
begin
  if Assigned(FDebugObject) then FDebugObject.Free;
  FDebugObject := nil;
  if chkDebug.Checked then
  begin
    FDebugObject := TDebugKeyboard.Create(Self);
    FDebugObject.Parent := Self;
    FDebugObject.DebugFlags := FDebugFlags;
    FDebugObject.DebugWindow := memo.Handle;
    FDebugObject.OnDebugMessage := DebugMessage;
    lbDebug.Visible := True;
    HSplitter.Visible := True;
    FDebugObject.StartDebugging;
  end
  else
  begin
    lbDebug.Visible := False;
    HSplitter.Visible := False;
  end;
  FocusTestWindow;
end;

procedure TfrmTestKeyboard.FormCreate(Sender: TObject);
begin
  inherited;
//  AddMenuItem(mnuEdit);
//  AddMenuItem(mnuView);
//  AddMenuItem(mnuDebug);
  FTesting := False;
  FDebugFlags := 1;
  chkDebug.Checked := False;
  //chkDebugClick(chkDebug);
end;

procedure TfrmTestKeyboard.DebugMessage(Sender: TObject; hwnd, State,
  LineNo: Integer; const msg: string);
var
  n: Integer;
begin
  n := lbDebug.Items.Add(IntToStr(State) + ' - ' + IntToStr(LineNo) + ' - ' + msg);
  if not lbDebug.Focused then lbDebug.ItemIndex := n;
end;

procedure TfrmTestKeyboard.mnuDebugClearClick(Sender: TObject);
begin
  lbDebug.Clear;
end;

procedure TfrmTestKeyboard.mnuDebugSaveToFileClick(Sender: TObject);
begin
  dlgSave.Title := 'Save to file';
  if dlgSave.Execute then
    lbDebug.Items.SaveToFile(dlgSave.FileName, TEncoding.UTF8);  // I3337
end;

procedure TfrmTestKeyboard.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  FDebugObject.Free;
  FDebugObject := nil;
  CanClose := True;
end;

procedure TfrmTestKeyboard.memoGotFocus(Sender: TObject);
begin
  if FInGotFocus then Exit;
  FInGotFocus := True;
  SetEditKeyboardState;
  FInGotFocus := False;
end;

procedure TfrmTestKeyboard.memoLostFocus(Sender: TObject);
begin
  Keyman_StopForcingKeyboard;
end;

procedure TfrmTestKeyboard.LoadFont;
var
  n: Integer;
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_CURRENT_USER;

    FAnsiFont := '';

    if OpenKeyReadOnly('\'+SRegKey_IDEEditFonts_CU) then
    try
      FAnsiFont := ReadString('');
    except
    end;

    FUnicodeFont := FAnsiFont;

    if OpenKeyReadOnly('\'+SRegKey_IDETestFonts_CU) then
    try
      if ValueExists(FKeyboardName+',A') then
      begin
        FAnsiFont := ReadString(FKeyboardName+',A');
        n := Pos('|', FAnsiFont);
        if n > 0 then Delete(FAnsiFont, 1, n);
      end;
      if ValueExists(FKeyboardName+',U') then
      begin
        FUnicodeFont := ReadString(FKeyboardName+',U');
        n := Pos('|', FUnicodeFont);
        if n > 0 then Delete(FUnicodeFont, 1, n);
      end;
    except
    end;

    if rbUnicode.Checked
      then SetFontFromString(memo.Font, FUnicodeFont)
      else SetFontFromString(memo.Font, FAnsiFont);
  finally
    Free;
  end;
end;

procedure TfrmTestKeyboard.SaveFont;
var
  s: string;
begin
  RemoveOldestTikeTestFonts(True);
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_CURRENT_USER;
    if not OpenKey(SRegKey_IDETestFonts_CU, True) then  // I2890
      RaiseLastRegistryError;
    s := ReadString('');

    if FAnsiFont <> s then WriteString(FKeyboardName+',A', GetCurrentDateTime+'|'+FAnsiFont)
    else if ValueExists(FKeyboardName+',A') then DeleteValue(FKeyboardName+',A');

    if FUnicodeFont <> s then WriteString(FKeyboardName+',U', GetCurrentDateTime+'|'+FUnicodeFont)
    else if ValueExists(FKeyboardName+',U') then DeleteValue(FKeyboardName+',U');
  finally
    Free;
  end;
end;

procedure TfrmTestKeyboard.SetTesting(const Value: Boolean);
begin
  chkTestKeyboard.Checked := Value;
end;

procedure TfrmTestKeyboard.chkTestKeyboardClick(Sender: TObject);
begin
  FTesting := chkTestKeyboard.Checked;
  if memo.Focused then SetEditKeyboardState;
  FocusTestWindow;
end;

procedure TfrmTestKeyboard.mnuEditCutClick(Sender: TObject);
begin
  memo.CutToClipboard;
end;

procedure TfrmTestKeyboard.mnuEditCopyClick(Sender: TObject);
begin
  memo.CopyToClipboard;
end;

procedure TfrmTestKeyboard.mnuEditPasteClick(Sender: TObject);
begin
  memo.PasteFromClipboard;
end;

procedure TfrmTestKeyboard.mnuViewFontClick(Sender: TObject);
begin
  dlgFont.Font := memo.Font;
  if dlgFont.Execute then
  begin
    memo.Font := dlgFont.Font;
    if rbUnicode.Checked
      then FUnicodeFont := FontAsString(memo.Font)
      else FAnsiFont := FontAsString(memo.Font);
    SaveFont;
  end;
end;

procedure TfrmTestKeyboard.mnuViewDefaultFontClick(Sender: TObject);
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_CURRENT_USER;
    if not OpenKey(SRegKey_IDETestFonts_CU, True) then  // I2890
      RaiseLastRegistryError;
    try
      if ValueExists(FKeyboardName+',A') then DeleteValue(FKeyboardName+',A');
      if ValueExists(FKeyboardName+',U') then DeleteValue(FKeyboardName+',U');
    except
    end;
  finally
    Free;
  end;
  LoadFont;
end;

procedure TfrmTestKeyboard.rbANSIClick(Sender: TObject);
begin
  ShowEditControl;
end;

procedure TfrmTestKeyboard.rbUnicodeClick(Sender: TObject);
begin
  ShowEditControl;
end;

procedure TfrmTestKeyboard.FocusTestWindow;
begin
  if memo.CanFocus and Visible and not (fsCreating in FormState) then
    memo.SetFocus;
end;

procedure TfrmTestKeyboard.ShowEditControl;
begin
  memo.AllowUnicode := rbUnicode.Checked;

  if rbUnicode.Checked
    then SetFontFromString(memo.Font, FUnicodeFont)
    else SetFontFromString(memo.Font, FAnsiFont);

  chkDebugClick(chkDebug);
end;

procedure TfrmTestKeyboard.Reload;
begin
  if Testing then
  begin
//    Testing := False;
    memo.SetFocus;
    Testing := True;
  end;
end;

procedure TfrmTestKeyboard.mnuViewClick(Sender: TObject);
begin
//  mnuViewMessages.Checked := frmMessages.Visible;
//  mnuViewToolbar.Checked := frmKeymanDeveloper.barTools.Visible;
end;

procedure TfrmTestKeyboard.memoSelMove(Sender: TObject);
var
  ch: WideString;
begin
  if memo.SelText = '' then
    ch := Copy(memo.Text, memo.SelStart-1, 1) //TODO: Supplementary pairs
  else
    ch := Copy(memo.SelText, 1, 16);

  if ch = ''
    then frmKeymanDeveloper.barStatus.Panels[2].Text := ''
    else if rbUnicode.Checked
      then frmKeymanDeveloper.barStatus.Panels[2].Text := FormatUnicode(ch)
      else frmKeymanDeveloper.barStatus.Panels[2].Text := FormatANSI(ch);
end;

procedure TfrmTestKeyboard.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = Ord('D')) and (Shift = [ssCtrl, ssAlt]) then
  begin
//    N2.Visible := not N2.Visible;
//    mnuKeyboardDebug.Visible := N2.Visible;
//    mnuProgramDebug.Visible := N2.Visible;
//    mnuMessageDebug.Visible := N2.Visible;
//    mnuInternatDebug.Visible := N2.Visible;
    Key := 0;
  end;
end;


function TfrmTestKeyboard.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_TestKeyboard;
end;

{ Debug menu }


procedure TfrmTestKeyboard.mnuDebugClick(Sender: TObject);
begin
//  mnuDebugShowDebugPane.Checked := chkDebug.Checked;
//  mnuDebugTestKeyboard.Checked := chkTestKeyboard.Checked;

//  mnuDebugANSI.Checked := rbANSI.Checked;
//  mnuDebugANSI.Enabled := rbANSI.Enabled;
//  mnuDebugUnicode.Checked := rbUnicode.Checked;
//  mnuDebugUnicode.Enabled := rbUnicode.Enabled;

//  mnuKeyboardDebug.Checked := (FDebugFlags and KDS_KEYBOARD) <> 0;
//  mnuProgramDebug.Checked := (FDebugFlags and KDS_PROGRAM) <> 0;
//  mnuMessageDebug.Checked := (FDebugFlags and KDS_MESSAGE) <> 0;
//  mnuInternatDebug.Checked := (FDebugFlags and KDS_INTERNAT) <> 0;
end;

procedure TfrmTestKeyboard.mnuDebugShowDebugPaneClick(Sender: TObject);
begin
  chkDebug.Checked := not chkDebug.Checked;
end;

procedure TfrmTestKeyboard.mnuDebugTestKeyboardClick(Sender: TObject);
begin
  chkTestKeyboard.Checked := not chkTestKeyboard.Checked;
end;

procedure TfrmTestKeyboard.mnuDebugANSIClick(Sender: TObject);
begin
  rbANSI.Checked := True;
end;

procedure TfrmTestKeyboard.mnuDebugUnicodeClick(Sender: TObject);
begin
  rbUnicode.Checked := True;
end;

procedure TfrmTestKeyboard.mnuKeyboardDebugClick(Sender: TObject);
begin
  if (FDebugFlags and KDS_KEYBOARD) <> 0 then FDebugFlags := FDebugFlags - KDS_KEYBOARD
  else FDebugFlags := FDebugFlags + KDS_KEYBOARD;
  chkDebugClick(chkDebug);
end;

procedure TfrmTestKeyboard.mnuProgramDebugClick(Sender: TObject);
begin
  if (FDebugFlags and KDS_PROGRAM) <> 0 then FDebugFlags := FDebugFlags - KDS_PROGRAM
  else FDebugFlags := FDebugFlags + KDS_PROGRAM;
  chkDebugClick(chkDebug);
end;

procedure TfrmTestKeyboard.mnuMessageDebugClick(Sender: TObject);
begin
  if (FDebugFlags and KDS_MESSAGE) <> 0 then FDebugFlags := FDebugFlags - KDS_MESSAGE
  else FDebugFlags := FDebugFlags + KDS_MESSAGE;
  chkDebugClick(chkDebug);
end;

procedure TfrmTestKeyboard.mnuInternatDebugClick(Sender: TObject);
begin
  if (FDebugFlags and KDS_INTERNAT) <> 0 then FDebugFlags := FDebugFlags - KDS_INTERNAT
  else FDebugFlags := FDebugFlags + KDS_INTERNAT;
  chkDebugClick(chkDebug);
end;

procedure TfrmTestKeyboard.mnuViewCharacterMapClick(Sender: TObject);
begin
(*
  CharMapSettings.FontName := memo.Font.Name;
  CharMapSettings.IsUnicode := memo.AllowUnicodeInput;
  ShowCharacterMap(CharMapSettings); //memo.Font.Name, memo.AllowUnicodeInput);
*)
end;

end.

