(*
 * Keyman is copyright (C) SIL Global. MIT License.
 *)
unit UfrmMain;  // I3306

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, ExtCtrls, StdCtrls, ComCtrls, Grids, Menus,
  KeymanTrayIcon, System.ImageList, Vcl.XPMan;

const
  SStatusSiteURL = 'https://status.keyman.com';
  SSearchURL = 'https://github.com/keymanapp/%s/issues/%s';
  SAddIssueURL = 'https://github.com/keymanapp/keyman/issues/new';

type
  TfrmMain = class(TForm)
    ilTrayIcons: TImageList;
    Label3: TLabel;
    pages: TPageControl;
    tabSetup: TTabSheet;
    cmdHide: TButton;
    cmdExit: TButton;
    mnuPopup: TPopupMenu;
    N1: TMenuItem;
    mnuExit: TMenuItem;
    cmdOpenCRM: TButton;
    mnuOpenStatusSite: TMenuItem;
    chkStartWithWindows: TCheckBox;
    cmdSave: TButton;
    mnuOpenIssueOrPR: TMenuItem;
    Label1: TLabel;
    hkOpenCRM: THotKey;
    Label2: TLabel;
    hkOpenCustomerRecord: THotKey;
    Label4: TLabel;
    hkOpenWindow: THotKey;
    mnuAddIssue: TMenuItem;
    N4: TMenuItem;
    Label7: TLabel;
    hkAddIssue: THotKey;
    XPManifest1: TXPManifest;
    Label9: TLabel;
    hkOpenCharIdent: THotKey;
    mnuOpenCharIdent: TMenuItem;
    N6: TMenuItem;
    Button1: TButton;
    mnuSettings: TMenuItem;
    procedure cmdExitClick(Sender: TObject);
    procedure cmdHideClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tiCRMClick(Sender: TObject);
    procedure cmdOpenCRMClick(Sender: TObject);
    procedure cmdSaveClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cmdOpenIssueOrPRClick(Sender: TObject);
    procedure hkFieldEnter(Sender: TObject);
    procedure hkFieldExit(Sender: TObject);
    procedure mnuAddIssueClick(Sender: TObject);
    procedure mnuOpenCharIdentClick(Sender: TObject);
  private
    CustomerText: string;
    tiCRM: TKeymanTrayIcon;
    ForceClose: Boolean;
    FIcon: TIcon;
    procedure WMQueryEndSession(var Message: TWMQueryEndSession); message WM_QUERYENDSESSION;
    procedure WMHotkey(var Message: TWMHotKey); message WM_HOTKEY;
    procedure AppMinimize(Sender: TObject);
    { Private declarations }
    procedure DisableHotkeys;
    procedure UpdateHotkeys;
    procedure GrabFocus;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  ErrorControlledRegistry,
  ShellApi,
  UfrmCharacterIdentifier,
  UfrmOpenCRMRecord,
  utilexecute,
  DCPcrypt2;

{$R *.DFM}
{$R grayicon.res}

procedure TfrmMain.cmdExitClick(Sender: TObject);
begin
  ForceClose := True;
  Close;
end;

procedure TfrmMain.cmdHideClick(Sender: TObject);
begin
  Hide;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Application.OnMinimize := AppMinimize;
  FIcon := TIcon.Create;
  FIcon.Handle := LoadIcon(HInstance, 'grayicon');
  tiCRM := TKeymanTrayIcon.Create(Self);
  tiCRM.OnClick := tiCRMClick;
  tiCRM.Icons := ilTrayIcons;
  tiCRM.PopupMenu := mnuPopup;
  tiCRM.Icon := FIcon; //.Assign(FIcon);
  tiCRM.AnimateInterval := 250;
  tiCRM.Visible := True;

  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKeyReadOnly('Software\Keyman\KMDevLink') then
    begin
      if ValueExists('Hotkey_OpenCRM') then hkOpenCRM.HotKey := TextToShortCut(ReadString('Hotkey_OpenCRM'));
      if ValueExists('Hotkey_OpenCustomerRecord') then hkOpenCustomerRecord.HotKey := TextToShortCut(ReadString('Hotkey_OpenCustomerRecord'));
      if ValueExists('Hotkey_OpenWindow') then hkOpenWindow.HotKey := TextToShortCut(ReadString('Hotkey_OpenWindow'));
      if ValueExists('Hotkey_AddIssue') then hkAddIssue.HotKey := TextToShortCut(ReadString('Hotkey_AddIssue'));


      if ValueExists('Hotkey_OpenCharIdent') then hkOpenCharIdent.HotKey := TextToShortCut(ReadString('Hotkey_OpenCharIdent'));

      UpdateHotkeys;

      Application.ShowMainForm := False;
    end;

    chkStartWithWindows.Checked := OpenKeyReadOnly('\Software\Microsoft\Windows\CurrentVersion\Run') and ValueExists('kmdevlink');
  finally
    Free;
  end;
end;

type
  TExTrayIcon = class(TTrayIcon);

procedure TfrmMain.tiCRMClick(Sender: TObject);
var
  msg: TMessage;
begin
  msg.Msg := WM_USER+1;
  msg.lParam := NIN_BALLOONHIDE;
  TExTrayIcon(tiCRM).WindowProc(msg);
  tiCRM.Refresh;
  Show;
  Application.BringToFront;
  Application.Restore;
  BringToFront;
end;

procedure TfrmMain.cmdOpenCRMClick(Sender: TObject);
begin
  if not TUtilExecute.URL(SStatusSiteURL) then  // I3349
    ShowMessage(SysErrorMessage(GetLastError));
end;

procedure TfrmMain.cmdSaveClick(Sender: TObject);
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    if not OpenKey('Software\Keyman\KMDevLink', True) then  // I2890
      RaiseLastRegistryError;

    WriteString('Hotkey_OpenCRM', ShortCutToText(hkOpenCRM.HotKey));
    WriteString('Hotkey_OpenCustomerRecord', ShortcutToText(hkOpenCustomerRecord.HotKey));
    WriteString('Hotkey_OpenWindow', ShortcutToText(hkOpenWindow.HotKey));
    WriteString('Hotkey_AddIssue', ShortcutToText(hkAddIssue.HotKey));

    WriteString('Hotkey_OpenCharIdent', ShortcutToText(hkOpenCharIdent.HotKey));

    if not OpenKey('\Software\Microsoft\Windows\CurrentVersion\Run', True) then  // I2890
      RaiseLastRegistryError;
    if not chkStartWithWindows.Checked and ValueExists('kmdevlink') then DeleteValue('kmdevlink')
    else if chkStartWithWindows.Checked then WriteString('kmdevlink', ParamStr(0));

  finally
    Free;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FIcon);
  FreeAndNil(tiCRM);
  DisableHotkeys;
end;

procedure TfrmMain.GrabFocus;
var
  h: THandle;
begin
  h := GetForegroundWindow;
  if h = 0 then Exit;

  AttachThreadInput(GetWindowThreadProcessId(h, nil), GetCurrentThreadId, True);
  Windows.SetFocus(Application.Handle);
  AttachThreadInput(GetWindowThreadProcessId(h, nil), GetCurrentThreadId, False);
end;

procedure TfrmMain.AppMinimize(Sender: TObject);
begin
  Hide;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if not ForceClose then
  begin
    CanClose := False;
    Hide;
  end
  else
    CanClose := True;
end;

procedure TfrmMain.WMQueryEndSession(var Message: TWMQueryEndSession);
begin
  Message.Result := 1;
  ForceClose := True;
end;

procedure TfrmMain.cmdOpenIssueOrPRClick(Sender: TObject);
var
  parts: TArray<string>;
  repo, number: string;
begin
  with TfrmOpenCRMRecord.Create(Self) do
  try
    SearchText := Self.CustomerText;
    if ShowModal <> mrOk then Exit;
    Self.CustomerText := SearchText;
  finally
    Free;
  end;

  parts := CustomerText.Split(['#']);
  if Length(parts) = 1 then
  begin
    repo := 'keyman';
    number := parts[0];
  end
  else
  begin
    repo := parts[0];
    if repo = '' then repo := 'keyman' else repo := RepoShortNameToFullName(repo);

    number := parts[1];
  end;
  if not TUtilExecute.URL(Format(SSearchURL, [repo, number])) then  // I3349
    ShowMessage(SysErrorMessage(GetLastError));
end;

procedure TfrmMain.mnuAddIssueClick(Sender: TObject);
begin
  if not TUtilExecute.URL(SAddIssueURL) then  // I3349
    ShowMessage(SysErrorMessage(GetLastError));
end;

procedure TfrmMain.mnuOpenCharIdentClick(Sender: TObject);
begin
  if not Assigned(frmCharacterIdentifier) then
    frmCharacterIdentifier := TfrmCharacterIdentifier.Create(nil);
  frmCharacterIdentifier.Show;
end;

procedure TfrmMain.hkFieldEnter(Sender: TObject);
begin
  DisableHotkeys;
end;

procedure TfrmMain.hkFieldExit(Sender: TObject);
begin
  UpdateHotkeys;
end;

procedure TfrmMain.DisableHotkeys;
var
  I: Integer;
begin
  for I := 1 to 10 do
    UnregisterHotkey(Handle, i);
end;

procedure TfrmMain.UpdateHotkeys;
  procedure DoRegHotkey(id: Integer; hotkey: TShortcut);
  var
    shift: TShiftState;
    mods: UINT;
    vk: Word;
  begin
    ShortCutToKey(hotkey, vk, shift);
    if vk = 0 then Exit;
    mods := 0;
    if ssShift in shift then mods := mods or MOD_SHIFT;
    if ssCtrl in shift then mods := mods or MOD_CONTROL;
    if ssAlt in shift then mods := mods or MOD_ALT;
    RegisterHotkey(Handle, id, mods, vk);
  end;

begin
  DoRegHotkey(1, hkOpenCRM.Hotkey); mnuOpenStatusSite.ShortCut := hkOpenCRM.Hotkey;
  DoRegHotkey(2, hkOpenCustomerRecord.Hotkey); mnuOpenIssueOrPR.ShortCut := hkOpenCustomerRecord.Hotkey;
  DoRegHotkey(4, hkOpenWindow.Hotkey); mnuSettings.ShortCut := hkOpenWindow.Hotkey;
  DoRegHotkey(6, hkAddIssue.HotKey); mnuAddIssue.ShortCut := hkAddIssue.Hotkey;
  DoRegHotkey(8, hkOpenCharIdent.HotKey); mnuOpenCharIdent.ShortCut := hkOpenCharIdent.Hotkey;
end;

procedure TfrmMain.WMHotkey(var Message: TWMHotKey);
begin
  case Message.HotKey of
    1: cmdOpenCRMClick(cmdOpenCRM);
    2: cmdOpenIssueOrPRClick(mnuOpenIssueOrPR);
    4: tiCRMClick(tiCRM);
    6: mnuAddIssueClick(mnuAddIssue);
    8: mnuOpenCharIdentClick(mnuOpenCharIdent);
    9: begin GrabFocus; BringToFront; mnuPopup.Popup(Screen.Width div 2, Screen.Height div 3); end;
    else inherited;
  end;
end;

end.

