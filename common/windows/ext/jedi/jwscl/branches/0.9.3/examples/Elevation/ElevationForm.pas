{
This demo shows how JwElevateProcess from unit JwsclElevation works.

Note:
 Using Vista or Win7 radiobox on a pre Vista OS does not work
 and will fail with invalid parameter exception.

Original author is: Christian Wimmer
This application is part of the JEDI API Project.
Visit at http://blog.delphi-jedi.net/

Version 1.0
Creation date: 17. January 2009

}
unit ElevationForm;

interface

uses
  JwaWindows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvExMask, JvToolEdit,
  JwsclToken, JwsclElevation, JwsclConstants, JwsclTypes, JwsclExceptions,
  JwsclVersion, JwsclEncryption, JwsclUtils, JvBaseDlg, JvLoginForm,
  JwsclKnownSid,JwsclStrings
  ;

type
  TForm1 = class(TForm)
    JvFilenameEdit1: TJvFilenameEdit;
    Label1: TLabel;
    lblCurrentUser: TLabel;
    RadioButton2000: TRadioButton;
    RadioButtonXP: TRadioButton;
    RadioButtonVista: TRadioButton;
    RadioButtonWin7: TRadioButton;
    chkSuRun: TCheckBox;
    lblSurun: TLabel;
    btnRun: TButton;
    chkAllowUI: TCheckBox;
    dlgLogin: TJvLoginDialog;
    lblPID: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure dlgLoginGetPassword(Sender: TObject; const UserName: String;
      var Password: String);
    procedure RadioButton2000Click(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure dlgLoginCheckUser(Sender: TObject; const UserName,
      Password: String; var AllowLogin: Boolean);
  private
    { Private declarations }
    fWinver : Integer;
    fUserName,
    fPassword : TJwString;
    procedure OnElevationGetCredentials(var Abort : Boolean; var UserName, Password : TJwString;
                var EncryptedPassword : Boolean; var Entropy : PDataBlob; var EncryptionPrompt : Boolean;
                var Environment : Pointer; var lpStartupInfo: TStartupInfoW);

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  StatusData : TJwSuRunStatus;
begin
  //set tags to windows version constants
  //will be used to set win ver
  RadioButton2000.Tag := cOsWin2000;
  RadioButtonXP.Tag := cOsXP;
  RadioButtonVista.Tag := cOsVista;
  RadioButtonWin7.Tag := cOsWin7;

  RadioButtonWin7.checked := TJwWindowsVersion.Iswindows7(false);
  RadioButtonVista.checked := TJwWindowsVersion.IsWindowsVista(false);
  RadioButtonXP.checked := TJwWindowsVersion.IsWindowsXP(false);
  RadioButton2000.checked := TJwWindowsVersion.IsWindows2000(false);

  //current radio selection
  fWinver := TJwWindowsVersion.GetWindowsType;

  JwInitWellKnownSIDs;

  JvFilenameEdit1.FileName := ParamStr(0);

  //find surun + ver
  if JwCheckSuRunStatus(StatusData) then
    lblSurun.Caption := Format('Surun %d.%d.%d.%d is available.',
      [StatusData.Version[0],StatusData.Version[1],StatusData.Version[2],StatusData.Version[3]])
  else
    lblSurun.Caption := 'Surun is not available.';

  //get username + admin privs
  lblCurrentUser.Caption := Format('Running as "%s"',[JwSecurityProcessUserSID.GetCachedUserFromSid]);

  if JwCheckAdministratorAccess then
    lblCurrentUser.Caption := lblCurrentUser.Caption + ' - Running with admin privileges.';
end;

//called when noUI flag is used
procedure TForm1.OnElevationGetCredentials(var Abort: Boolean;
  var UserName, Password: TJwString; var EncryptedPassword: Boolean;
  var Entropy: PDataBlob; var EncryptionPrompt: Boolean;
  var Environment: Pointer; var lpStartupInfo: TStartupInfoW);
begin
  //set default username
  dlgLogin.LoggedUser := UserName;
  //run login dialog
  Abort := not dlgLogin.Execute;
  if abort then exit;

  ///fXX is set in dlgLoginGetPassword
  Password := fPassword;
  UserName := fUserName; //encrypted username is not supported!

  //the password is encrypted
  EncryptedPassword := True;

  //remove them from memory securely
  JwZeroPassword(fPassword);
  JwZeroPassword(fUserName);
end;

procedure TForm1.dlgLoginGetPassword(Sender: TObject;
  const UserName: String; var Password: String);
begin
  //remove them from memory securely
  JwZeroPassword(fPassword);
  JwZeroPassword(fUserName);

  //secure password in memory -
  //no prompt and on local machine only
  fPassword := JwEncryptString(Password, '', false,true);
  fUserName := UserName; //don't encrypt username !
end;

procedure TForm1.RadioButton2000Click(Sender: TObject);
begin
  //tag contains win type of radio button
  fWinver := (Sender as TRadioButton).Tag;
end;

procedure TForm1.btnRunClick(Sender: TObject);
var ElevationProcessFlags : TJwElevationProcessFlags;
begin
  ElevationProcessFlags := [];

  //use surun?
  if chkSuRun.Checked then
    ElevationProcessFlags := ElevationProcessFlags + [epfAllowSuRun];

  //use windows logon dialog?
  if not chkAllowUI.Checked then
    ElevationProcessFlags := ElevationProcessFlags + [epfNoUi];

  lblPID.caption := '';

  //simulate other winver for JwElevateProcess
  TJwWindowsVersion.SetCachedWindowsType(fWinver);
  try
    lblPID.caption := Format('Process started with PID: %d',[
    JwElevateProcess(
         JvFilenameEdit1.FileName,//const FileName : TJwString;
         '',//Parameters : TJwString;
         '',//Directory : TJwString;
         handle,//hWindow : HWND;
         ElevationProcessFlags,//ElevationProcessFlags : TJwElevationProcessFlags;
         OnElevationGetCredentials//const OnElevationGetCredentials : TJwOnElevationGetCredentials
         )]);
  finally
    //get back to original winver
    TJwWindowsVersion.ResetCachedWindowsType;
  end;
end;

//checks logon data
procedure TForm1.dlgLoginCheckUser(Sender: TObject; const UserName,
  Password: String; var AllowLogin: Boolean);
var T : TJwSecurityToken;
begin
  T := nil;
  if TJwWindowsVersion.IsWindowsXP(true) then //win2000 needs tcb privilege, so ignore
  try
    //verify user logon data
    T := TJwSecurityToken.CreateLogonUser(UserName, '', password, LOGON32_LOGON_INTERACTIVE,
        LOGON32_PROVIDER_DEFAULT);
  except
    {This logon dialog has top most flag and therefore
    all message boxes will be shown behind it.

    You should use a better suited dialog!
    I also hope that Password string in dialog is also freed correctly. Well
    it is just a demo nonetheless! 
    }
    AllowLogin := False;
    Exit;
  end;
  T.Free;
  AllowLogin := True;
end;

end.
