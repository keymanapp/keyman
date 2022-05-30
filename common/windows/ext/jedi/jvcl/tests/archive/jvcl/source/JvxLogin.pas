{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvxLogin.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvxLogin;

interface

uses
  Windows,
  SysUtils, Messages, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls,
  JvComponent, JvBaseDlg;

type
  TUpdateCaption = (ucNoChange, ucAppTitle, ucFormCaption);
  TJvLoginEvent = procedure(Sender: TObject; const UserName, Password: string;
    var AllowLogin: Boolean) of object;
  TCheckUnlockEvent = function(const Password: string): Boolean of object;
  TUnlockAppEvent = procedure(Sender: TObject; const UserName,
    Password: string; var AllowUnlock: Boolean) of object;

  TJvLoginForm = class;

  TJvCustomLogin = class(TJvCommonDialogF)
  private
    FActive: Boolean;
    FAttemptNumber: Integer;
    FLoggedUser: string;
    FMaxPasswordLen: Integer;
    FAllowEmptyPassword: Boolean;
    FUpdateCaption: TUpdateCaption;
    FIniFileName: string;
    FUseRegistry: Boolean;
    FLocked: Boolean;
    FUnlockDlgShowing: Boolean;
    FSaveOnRestore: TNotifyEvent;
    FAfterLogin: TNotifyEvent;
    FBeforeLogin: TNotifyEvent;
    FOnUnlock: TCheckUnlockEvent;
    FOnUnlockApp: TUnlockAppEvent;
    FOnIconDblClick: TNotifyEvent;
    FPasswordChar: char;
    function GetLoggedUser: string;
    function GetIniFileName: string;
    procedure SetIniFileName(const Value: string);
    function UnlockHook(var Msg: TMessage): Boolean;
  protected
    function CheckUnlock(const UserName, Password: string): Boolean; dynamic;
    function CreateLoginForm(UnlockMode: Boolean): TJvLoginForm; virtual;
    procedure DoAfterLogin; dynamic;
    procedure DoBeforeLogin; dynamic;
    procedure DoIconDblCLick(Sender: TObject); dynamic;
    function DoLogin(var UserName: string): Boolean; virtual; abstract;
    function DoUnlockDialog: Boolean; virtual;
    procedure SetLoggedUser(const Value: string);
    procedure DoUpdateCaption;
    procedure UnlockOkClick(Sender: TObject);
    property Active: Boolean read FActive write FActive default True;
    property AllowEmptyPassword: Boolean read FAllowEmptyPassword write FAllowEmptyPassword default True;
    property AttemptNumber: Integer read FAttemptNumber write FAttemptNumber default 3;
    property IniFileName: string read GetIniFileName write SetIniFileName;
    property MaxPasswordLen: Integer read FMaxPasswordLen write FMaxPasswordLen default 0;
    property UpdateCaption: TUpdateCaption read FUpdateCaption write FUpdateCaption default ucNoChange;
    property UseRegistry: Boolean read FUseRegistry write FUseRegistry default False;
    property PasswordChar:char read FPasswordChar write FPasswordChar default '*'; 
    property AfterLogin: TNotifyEvent read FAfterLogin write FAfterLogin;
    property BeforeLogin: TNotifyEvent read FBeforeLogin write FBeforeLogin;
    property OnUnlock: TCheckUnlockEvent read FOnUnlock write FOnUnlock; { obsolete }
    property OnUnlockApp: TUnlockAppEvent read FOnUnlockApp write FOnUnlockApp;
    property OnIconDblClick: TNotifyEvent read FOnIconDblClick write FOnIconDblClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Login: Boolean; virtual;
    function Execute: Boolean; override;
    procedure TerminateApplication;
    procedure Lock;
    property LoggedUser: string read GetLoggedUser;
  end;

  TJvLoginDialog = class(TJvCustomLogin)
  private
    FOnCheckUser: TJvLoginEvent;
    procedure OkButtonClick(Sender: TObject);
    procedure WriteUserName(const UserName: string);
    function ReadUserName(const UserName: string): string;
  protected
    function DoCheckUser(const UserName, Password: string): Boolean; dynamic;
    function DoLogin(var UserName: string): Boolean; override;
    procedure Loaded; override;
  published
    property Active;
    property AttemptNumber;
    property IniFileName;
    property MaxPasswordLen;
    property UpdateCaption;
    property UseRegistry;
    property PasswordChar;
    property OnCheckUser: TJvLoginEvent read FOnCheckUser write FOnCheckUser;
    property AfterLogin;
    property BeforeLogin;
    property OnUnlockApp;
    property OnIconDblClick;
  end;

  TJvLoginForm = class(TJvForm)
    AppIcon: TImage;
    KeyImage: TImage;
    HintLabel: TLabel;
    UserNameLabel: TLabel;
    PasswordLabel: TLabel;
    UserNameEdit: TEdit;
    PasswordEdit: TEdit;
    AppTitleLabel: TLabel;
    OkBtn: TButton;
    CancelBtn: TButton;
    CustomLabel: TLabel;
    CustomCombo: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FSelectDatabase: Boolean;
    FUnlockMode: Boolean;
    FAttempt: Integer;
    FOnFormShow: TNotifyEvent;
    FOnOkClick: TNotifyEvent;
  public
    AttemptNumber: Integer;
    property Attempt: Integer read FAttempt;
    property SelectDatabase: Boolean read FSelectDatabase write FSelectDatabase;
    property OnFormShow: TNotifyEvent read FOnFormShow write FOnFormShow;
    property OnOkClick: TNotifyEvent read FOnOkClick write FOnOkClick;
  end;

function CreateLoginDialog(UnlockMode, ASelectDatabase: Boolean;
  FormShowEvent, OkClickEvent: TNotifyEvent): TJvLoginForm;

implementation

uses
  Registry, Consts, IniFiles,
  JvAppUtils, JvxRConst, JvVCLUtils, JvConst;

{$R *.DFM}

const
  keyLoginSection = 'Login Dialog';
  keyLastLoginUserName = 'Last Logged User';

function CreateLoginDialog(UnlockMode, ASelectDatabase: Boolean;
  FormShowEvent, OkClickEvent: TNotifyEvent): TJvLoginForm;
begin
  Result := TJvLoginForm.Create(Application);
  with Result do
  begin
    FSelectDatabase := ASelectDatabase;
    FUnlockMode := UnlockMode;
    if FUnlockMode then
    begin
      FormStyle := fsNormal;
      FSelectDatabase := False;
    end
    else
      FormStyle := fsStayOnTop;
    OnFormShow := FormShowEvent;
    OnOkClick := OkClickEvent;
  end;
end;

//=== TJvCustomLogin =========================================================

constructor TJvCustomLogin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIniFileName := EmptyStr;
  FLoggedUser := EmptyStr;
  FActive := True;
  FAttemptNumber := 3;
  FPasswordChar := '*';
  FAllowEmptyPassword := True;
  FUseRegistry := False;
end;

destructor TJvCustomLogin.Destroy;
begin
  if FLocked then
  begin
    Application.UnhookMainWindow(UnlockHook);
    FLocked := False;
  end;
  //DisposeStr(FLoggedUser);
  //DisposeStr(FIniFileName);
  inherited Destroy;
end;

function TJvCustomLogin.GetIniFileName: string;
begin
  Result := FIniFileName;
  if (Result = '') and not (csDesigning in ComponentState) then
  begin
    {$IFDEF WIN32}
    if UseRegistry then
      Result := GetDefaultIniRegKey
    else
      Result := GetDefaultIniName;
    {$ELSE}
    Result := GetDefaultIniName;
    {$ENDIF}
  end;
end;

procedure TJvCustomLogin.SetIniFileName(const Value: string);
begin
  FIniFileName := Value;
end;

function TJvCustomLogin.GetLoggedUser: string;
begin
  Result := FLoggedUser;
end;

procedure TJvCustomLogin.SetLoggedUser(const Value: string);
begin
  FLoggedUser := Value;
end;

procedure TJvCustomLogin.DoAfterLogin;
begin
  if Assigned(FAfterLogin) then
    FAfterLogin(Self);
end;

procedure TJvCustomLogin.DoBeforeLogin;
begin
  if Assigned(FBeforeLogin) then
    FBeforeLogin(Self);
end;

procedure TJvCustomLogin.DoIconDblCLick(Sender: TObject);
begin
  if Assigned(FOnIconDblClick) then
    FOnIconDblClick(Self);
end;

procedure TJvCustomLogin.DoUpdateCaption;
var
  F: TForm;
begin
  F := Application.MainForm;
  if (F = nil) and (Owner is TForm) then
    F := Owner as TForm;
  if (F <> nil) and (LoggedUser <> '') then
    case UpdateCaption of
      ucAppTitle:
        F.Caption := Format('%s (%s)', [Application.Title, LoggedUser]);
      ucFormCaption:
        begin
          F.Caption := Format('%s (%s)', [F.Caption, LoggedUser]);
          UpdateCaption := ucNoChange;
        end;
    end;
end;

function TJvCustomLogin.Login: Boolean;
var
  LoginName: string;
begin
  LoginName := EmptyStr;
  DoBeforeLogin;
  Result := DoLogin(LoginName);
  if Result then
  begin
    SetLoggedUser(LoginName);
    DoUpdateCaption;
    DoAfterLogin;
  end;
end;

procedure TJvCustomLogin.Lock;
begin
  FSaveOnRestore := Application.OnRestore;
  Application.Minimize;
  Application.HookMainWindow(UnlockHook);
  FLocked := True;
end;

procedure TJvCustomLogin.TerminateApplication;
begin
  with Application do
  begin
    {$IFDEF WIN32}
    ShowMainForm := False;
    {$ENDIF}
    if Handle <> 0 then
      ShowOwnedPopups(Handle, False);
    Terminate;
  end;
  {$IFDEF COMPILER3_UP}
  CallTerminateProcs;
  {$ENDIF}
  {$IFNDEF COMPILER3_UP}
  Halt(10);
  {$ENDIF}
end;

procedure TJvCustomLogin.UnlockOkClick(Sender: TObject);
var
  Ok: Boolean;
begin
  with TJvLoginForm(Sender) do
  begin
    Ok := False;
    try
      Ok := CheckUnlock(UserNameEdit.Text, PasswordEdit.Text);
    except
      Application.HandleException(Self);
    end;
    if Ok then
      ModalResult := mrOk
    else
      ModalResult := mrCancel;
  end;
end;

function TJvCustomLogin.CheckUnlock(const UserName, Password: string): Boolean;
begin
  Result := True;
  if Assigned(FOnUnlockApp) then
    FOnUnlockApp(Self, UserName, Password, Result)
  else
  if Assigned(FOnUnlock) then
    Result := FOnUnlock(Password);
end;

function TJvCustomLogin.CreateLoginForm(UnlockMode: Boolean): TJvLoginForm;
begin
  Result := TJvLoginForm.Create(Application);
  with Result do
  begin
    FUnlockMode := UnlockMode;
    if FUnlockMode then
    begin
      FormStyle := fsNormal;
      FSelectDatabase := False;
    end
    else
      FormStyle := fsStayOnTop;
    if Assigned(Self.FOnIconDblClick) then
    begin
      with AppIcon do
      begin
        OnDblClick := DoIconDblClick;
        Cursor := crHand;
      end;
      with KeyImage do
      begin
        OnDblClick := DoIconDblClick;
        Cursor := crHand;
      end;
    end;
    PasswordEdit.MaxLength := FMaxPasswordLen;
    PasswordEdit.PasswordChar := PassWordChar;
    AttemptNumber := Self.AttemptNumber;
  end;
end;

function TJvCustomLogin.DoUnlockDialog: Boolean;
begin
  with CreateLoginForm(True) do
  try
    OnFormShow := nil;
    OnOkClick := UnlockOkClick;
    with UserNameEdit do
    begin
      Text := LoggedUser;
      ReadOnly := True;
      Font.Color := clGrayText;
    end;
    Result := ShowModal = mrOk;
  finally
    Free;
  end;
end;

function TJvCustomLogin.UnlockHook(var Msg: TMessage): Boolean;

  function DoUnlock: Boolean;
  var
    Popup: HWND;
  begin
    with Application do
      if IsWindowVisible(Handle) and IsWindowEnabled(Handle) then
        {$IFDEF WIN32}
        SetForegroundWindow(Handle);
        {$ELSE}
        BringWindowToTop(Handle);
        {$ENDIF}
    if FUnlockDlgShowing then
    begin
      Popup := GetLastActivePopup(Application.Handle);
      if (Popup <> 0) and IsWindowVisible(Popup) and
        (WindowClassName(Popup) = TJvLoginForm.ClassName) then
      begin
        {$IFDEF WIN32}
        SetForegroundWindow(Popup);
        {$ELSE}
        BringWindowToTop(Popup);
        {$ENDIF}
      end;
      Result := False;
      Exit;
    end;
    FUnlockDlgShowing := True;
    try
      Result := DoUnlockDialog;
    finally
      FUnlockDlgShowing := False;
    end;
    if Result then
    begin
      Application.UnhookMainWindow(UnlockHook);
      FLocked := False;
    end;
  end;

begin
  Result := False;
  if not FLocked then
    Exit;
  with Msg do
  begin
    case Msg of
      WM_QUERYOPEN:
        begin
          UnlockHook := not DoUnlock;
        end;
      WM_SHOWWINDOW:
        if Bool(WParam) then
        begin
          UnlockHook := not DoUnlock;
        end;
      WM_SYSCOMMAND:
        if (WParam and $FFF0 = SC_RESTORE) or
          (WParam and $FFF0 = SC_ZOOM) then
        begin
          UnlockHook := not DoUnlock;
        end;
    end;
  end;
end;

//=== TJvLoginDialog =========================================================

procedure TJvLoginDialog.Loaded;
var
  Loading: Boolean;
begin
  Loading := csLoading in ComponentState;
  inherited Loaded;
  if not (csDesigning in ComponentState) and Loading then
  begin
    if Active and not Login then
      TerminateApplication;
  end;
end;

procedure TJvLoginDialog.OkButtonClick(Sender: TObject);
var
  SetCursor: Boolean;
begin
  with TJvLoginForm(Sender) do
  begin
    {$IFDEF WIN32}
    SetCursor := GetCurrentThreadID = MainThreadID;
    {$ELSE}
    SetCursor := True;
    {$ENDIF}
    try
      if SetCursor then
        Screen.Cursor := crHourGlass;
      try
        if DoCheckUser(UserNameEdit.Text, PasswordEdit.Text) then
          ModalResult := mrOk
        else
          ModalResult := mrNone;
      finally
        if SetCursor then
          Screen.Cursor := crDefault;
      end;
    except
      Application.HandleException(Self);
    end;
  end;
end;

function TJvLoginDialog.DoCheckUser(const UserName, Password: string): Boolean;
begin
  Result := True;
  if Assigned(FOnCheckUser) then
    FOnCheckUser(Self, UserName, Password, Result);
end;

procedure TJvLoginDialog.WriteUserName(const UserName: string);
var
  Ini: TObject;
begin
  try
    {$IFDEF WIN32}
    if UseRegistry then
      Ini := TRegIniFile.Create(IniFileName)
    else
      Ini := TIniFile.Create(IniFileName);
    {$ELSE}
    Ini := TIniFile.Create(IniFileName);
    {$ENDIF}
    try
      IniWriteString(Ini, keyLoginSection, keyLastLoginUserName, UserName);
    finally
      Ini.Free;
    end;
  except
  end;
end;

function TJvLoginDialog.ReadUserName(const UserName: string): string;
var
  Ini: TObject;
begin
  try
    {$IFDEF WIN32}
    if UseRegistry then
    begin
      Ini := TRegIniFile.Create(IniFileName);
      {$IFDEF COMPILER5_UP}
      TRegIniFile(Ini).Access := KEY_READ;
      {$ENDIF}
    end
    else
      Ini := TIniFile.Create(IniFileName);
    {$ELSE}
    Ini := TIniFile.Create(IniFileName);
    {$ENDIF}
    try
      Result := IniReadString(Ini, keyLoginSection, keyLastLoginUserName,
        UserName);
    finally
      Ini.Free;
    end;
  except
    Result := UserName;
  end;
end;

function TJvLoginDialog.DoLogin(var UserName: string): Boolean;
begin
  try
    with CreateLoginForm(False) do
    try
      OnOkClick := Self.OkButtonClick;
      UserName := ReadUserName(UserName);
      UserNameEdit.Text := UserName;
      Result := (ShowModal = mrOk);
      if Result then
      begin
        UserName := UserNameEdit.Text;
        WriteUserName(UserName);
      end;
    finally
      Free;
    end;
  except
    Application.HandleException(Self);
    Result := False;
  end;
end;

//=== TJvLoginForm ===========================================================

procedure TJvLoginForm.FormCreate(Sender: TObject);
begin
  Icon := Application.Icon;
  if Icon.Empty then
    Icon.Handle := LoadIcon(0, IDI_APPLICATION);
  AppIcon.Picture.Assign(Icon);
  AppTitleLabel.Caption := Format(SAppTitleLabel, [Application.Title]);
  PasswordLabel.Caption := SPasswordLabel;
  UserNameLabel.Caption := SUserNameLabel;
  OkBtn.Caption := SOKButton;
  CancelBtn.Caption := SCancelButton;
end;

procedure TJvLoginForm.OkBtnClick(Sender: TObject);
begin
  Inc(FAttempt);
  if Assigned(FOnOkClick) then
    FOnOkClick(Self)
  else
    ModalResult := mrOk;
  if (ModalResult <> mrOk) and (FAttempt >= AttemptNumber) then
    ModalResult := mrCancel;
end;

procedure TJvLoginForm.FormShow(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  if FSelectDatabase then
  begin
    ClientHeight := CustomCombo.Top + PasswordEdit.Top - UserNameEdit.Top;
    S := SDatabaseName;
    I := Pos(':', S);
    if I = 0 then
      I := Length(S);
    CustomLabel.Caption := '&' + Copy(S, 1, I);
  end
  else
  begin
    ClientHeight := PasswordEdit.Top + PasswordEdit.Top - UserNameEdit.Top;
    CustomLabel.Visible := False;
    CustomCombo.Visible := False;
  end;
  if not FUnlockMode then
  begin
    HintLabel.Caption := SHintLabel;
    Caption := SRegistration;
  end
  else
  begin
    HintLabel.Caption := SUnlockHint;
    Caption := SUnlockCaption;
  end;
  if (UserNameEdit.Text = EmptyStr) and not FUnlockMode then
    ActiveControl := UserNameEdit
  else
    ActiveControl := PasswordEdit;
  if Assigned(FOnFormShow) then
    FOnFormShow(Self);
  FAttempt := 0;
end;

function TJvCustomLogin.Execute: Boolean;
begin
  Result := Login;
end;

end.

