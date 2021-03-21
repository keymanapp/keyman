(*
  Name:             UfrmOnlineUpdateIcon
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      18 May 2012

  Modified Date:    18 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit UfrmOnlineUpdateIcon;  // I3306

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, KeymanTrayIcon, Menus, ExtCtrls;

type
  TfrmOnlineUpdateIcon = class(TForm)
    trayMenu: TPopupMenu;
    cmdView: TMenuItem;
    cmdExit: TMenuItem;
    tmrShowBalloon: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure trayClick(Sender: TObject);
    procedure cmdViewClick(Sender: TObject);
    procedure cmdExitClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tmrShowBalloonTimer(Sender: TObject);
  private
    tray: TKeymanTrayIcon;
  end;

procedure ShowUpdateIcon;

implementation

{$R *.dfm}

uses
  DebugPaths,
  initprog,
  KeymanPaths,
  OnlineUpdateCheck,
  OnlineUpdateCheckMessages,
  ShellApi;

procedure ShowUpdateIcon;
var
  frmOnlineUpdateIcon: TfrmOnlineUpdateIcon;
begin
  if ApplicationRunning then
  begin
    TfrmOnlineUpdateIcon.Create(nil);
  end
  else
  begin
    Application.CreateForm(TfrmOnlineUpdateIcon, frmOnlineUpdateIcon);
    if Application.MainForm = frmOnlineUpdateIcon then
      Application.ShowMainForm := False;
  end;
end;

procedure TfrmOnlineUpdateIcon.cmdExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmOnlineUpdateIcon.cmdViewClick(Sender: TObject);
begin
  trayClick(Sender);
end;

procedure TfrmOnlineUpdateIcon.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmOnlineUpdateIcon.FormCreate(Sender: TObject);
var
  FIcon: WideString;
begin
  tray := TKeymanTrayIcon.Create(Self);
  tray.OnClick := trayClick;
  tray.OnBalloonClick := trayClick;
  tray.PopupMenu := trayMenu;
  tray.Icon := TIcon.Create;

  FIcon := TKeymanPaths.KeymanDesktopInstallPath('cfgicon.ico'); // GetDebugPath(FIcon, ExtractFilePath(ParamStr(0)) + FIcon, False);
  if FileExists(FIcon) then
    tray.Icon.LoadFromFile(FIcon);

  tray.Hint := S_OnlineUpdate_IconTitle + #13#10 + S_OnlineUpdate_IconText;
  tray.Visible := True;

  cmdView.Caption := S_OnlineUpdate_IconMenuText;
  cmdExit.Caption := S_OnlineUpdate_IconMenuExit;

  tmrShowBalloon.Enabled := True;
end;

procedure TfrmOnlineUpdateIcon.tmrShowBalloonTimer(Sender: TObject);
begin
  tmrShowBalloon.Enabled := False;
  tray.BalloonHint := S_OnlineUpdate_IconText;
  tray.BalloonTitle := S_OnlineUpdate_IconTitle;
  tray.BalloonTimeout := 15000;
  tray.BalloonIcon := tray.Icon.Handle;
  tray.BalloonFlags := bfUser;
  tray.ShowBalloonHint;
end;

procedure TfrmOnlineUpdateIcon.trayClick(Sender: TObject);
begin
  tmrShowBalloon.Enabled := False;
  tray.Visible := False;
  with TOnlineUpdateCheck.Create(nil, True, False) do
  try
    if Run = oucUnknown {Cancel clicked} then
    begin
      tray.Visible := True;
      Exit;
    end;
  finally
    Free;
  end;

  Close;
end;

end.
