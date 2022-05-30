unit CredentialsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, JwsclStrings, JvExControls,
  JvLookOut, JvExExtCtrls, JvBevel, JvButton, JvTransparentButton,
  JvComponent, JvExStdCtrls, JvHtControls, JvLinkLabel, JvExtComponent,
  JvLinkLabelTools, SessionPipe,
  JvPanel, jpeg, JvLabel, JvComponentBase, JvComputerInfoEx, JvImage, JvGradient,
  JvGradientHeaderPanel, JvEdit, JvWaitingGradient, JvWaitingProgress,
  JvStaticText, ComCtrls, JvExComCtrls, JvProgressBar;

type
  TFormCredentials = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn_Ok: TBitBtn;
    Image1: TImage;
    Label_DefaultUser: TLabel;
    Image2: TImage;
    UsersComboBox: TComboBox;
    CheckBoxSaveLogon: TCheckBox;
    ButtonDefaultUser: TJvTransparentButton;
    JvBevel1: TJvBevel;
    JvBevel2: TJvBevel;
    ButtonUser: TJvTransparentButton;
    JvPanel1: TJvPanel;
    JvLinkLabel1: TJvLinkLabel;
    Image3: TImage;
    JvLinkLabel2: TJvLinkLabel;
    JvComputerInfoEx1: TJvComputerInfoEx;
    JvImage1: TJvImage;
    JvGradient_SignedApp: TJvGradient;
    Image_Windows: TImage;
    JvGradient_UnsignedApp: TJvGradient;
    JvLabel_UnsignedApp: TJvLabel;
    JvLabel_SignedApp: TJvLabel;
    Image_Application: TImage;
    JvStaticText_AppPublisher: TJvStaticText;
    JvEdit_CmdLine: TJvEdit;
    JvProgressBar: TJvProgressBar;
    Timer1: TTimer;
    JvEdit_AppName: TJvEdit;
    EditPassword1: TJvEdit;
    EditPassword2: TJvEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure EditPassword1Change(Sender: TObject);
    procedure EditPassword2Change(Sender: TObject);
    procedure ButtonDefaultUserClick(Sender: TObject);
    procedure ButtonUserClick(Sender: TObject);
    procedure UsersComboBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxSaveLogonClick(Sender: TObject);
    procedure BitBtn_OkClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure JvBevel1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure JvLinkLabel1LinkClick(Sender: TObject; LinkNumber: Integer;
      LinkText, LinkParam: String);
    procedure Timer1Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EditPassword1Exit(Sender: TObject);
    procedure EditPassword1Enter(Sender: TObject);
    procedure UsersComboBoxMouseEnter(Sender: TObject);
    procedure JvProgressBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private-Deklarationen }
    fSaveLogon : Boolean;
    fAppName,
    fAppCmdLine,
    fUserName,
    fPassword : TJwString;
    internalMsg : Boolean;
    fTimeout,
    fFlags : Cardinal;

    function GetUserPicture(): TBitmap;
    procedure OnGetImage;
    function IsPasswordCacheAvailable : Boolean; 
  public
    destructor Destroy; override;

    procedure CenterInMonitor(const i : Integer);
    { Public-Deklarationen }
    property AppName : TJwString read fAppName write fAppName;
    property AppCmdLine : TJwString read fAppCmdLine write fAppCmdLine;

    property SaveLogon : Boolean read fSaveLogon write fSaveLogon;
    property UserName : TJwString read fUserName write fUserName;
    property Password : TJwString read fPassword write fPassword;
    property Flags : Cardinal read fFlags write fFlags;
    property TimeOut : Cardinal read fTimeout write fTimeout;

  end;

var
  FormCredentials: TFormCredentials;

function GetFillPasswd : String;

implementation
uses Math;

{$R *.dfm}

procedure TFormCredentials.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  EditPassword1.Text := GetFillPasswd;
  EditPassword2.Text := GetFillPasswd;

  Application.Terminate;
end;

function GetUserName(): string;
var
  Buffer: array [0..MAX_COMPUTERNAME_LENGTH + 1] of Char;
  Size: DWord;
begin
  Size := Pred(SizeOf(Buffer));
  Windows.GetUserName(Buffer, Size);
  Result := StrPas(Buffer);
end;

function TFormCredentials.GetUserPicture(): TBitmap;
var
  CommonDataPath, UserName: string;
begin
 { Result := TBitmap.Create;

  UserName := GetUserName;
  CommonDataPath := JvComputerInfoEx1.Folders.CommonAppData + '\Microsoft\User Account Pictures\' + UserName + '.bmp';

  if FileExists(CommonDataPath) then
  begin
    Result.Handle := LoadImage(0, PChar(CommonDataPath), IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE);
  end
  else ShowMessage(Format('%s'#13#10'"%s"', ['File not found:', CommonDataPath])); }
end;


function TFormCredentials.IsPasswordCacheAvailable: Boolean;
begin
  result := Flags and SERVER_CACHEAVAILABLE = SERVER_CACHEAVAILABLE;
end;

procedure TFormCredentials.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var bButtons,
    bDefaultUserPassword,
    bUser : Boolean;
begin
  CanClose := true;

  if ModalResult = mrOk then
  begin


    try
      bButtons := (ButtonDefaultUser.Down xor ButtonUser.Down);
      bDefaultUserPassword :=
        ButtonDefaultUser.Down and (Length(Trim(EditPassword1.Text)) > 0);
      bUser := ButtonUser.Down and (Length(Trim(UsersComboBox.Text)) > 0) and
              (Length(Trim(EditPassword2.Text)) > 0);

      CanClose := bButtons and (bDefaultUserPassword or bUser);
    finally
      //if not CanClose then
      //Do info here
    end;

    if CanClose then
    begin
      Flags := 0;

      fSaveLogon := CheckBoxSaveLogon.Checked;
      fUserName := '';
      if ButtonUser.Down then
      begin
        fUserName  := UsersComboBox.Text;
        fPassword  := EditPassword2.Text;
        if (Length(EditPassword2.Text) = 0) and
          (EditPassword2.Tag = 1) then //use cached password
           Flags := Flags or CLIENT_USECACHECREDS;
      end
      else
      begin
        fPassword  := EditPassword1.Text;
        if (Length(EditPassword2.Text) = 0) and
          (EditPassword2.Tag = 1) then //use cached password
          Flags := Flags or CLIENT_USECACHECREDS;
      end;

      if SaveLogon then
        Flags := Flags or CLIENT_CACHECREDS; //save pass to cache if correct

      EditPassword2.Text := GetFillPasswd;
      EditPassword1.Text := GetFillPasswd;
    end;
  end;
end;

procedure TFormCredentials.EditPassword1Change(Sender: TObject);
begin
  if internalMsg then
    exit;
  internalMsg := true;
  begin
    ButtonDefaultUser.Click;
  end;

  if (Length(TJvEdit(Sender).Text) > 0) then
  begin
    TJvEdit(Sender).PasswordChar := #7;
    //EditPassword1.ThemedPassword := true;
    TJvEdit(Sender).ProtectPassword := true;
  end;

  internalMsg := false;

end;

procedure TFormCredentials.EditPassword1Enter(Sender: TObject);
begin
  if (Length(TJvEdit(Sender).Text) = 0) then
  begin
    TJvEdit(Sender).EmptyValue := '';
    TJvEdit(Sender).Text := '';

    TJvEdit(Sender).PasswordChar := #7;
    //EditPassword1.ThemedPassword := false;
    TJvEdit(Sender).ProtectPassword := true;
  end;
end;

procedure TFormCredentials.EditPassword1Exit(Sender: TObject);
begin
  if (Length(TJvEdit(Sender).Text) = 0) and
     IsPasswordCacheAvailable then
  begin
    TJvEdit(Sender).PasswordChar := #0;
    TJvEdit(Sender).EmptyValue := 'Password is chached';
    //EditPassword1.ThemedPassword := false;
    TJvEdit(Sender).ProtectPassword := false;
    TJvEdit(Sender).Text := '';
  end;
end;

procedure TFormCredentials.EditPassword2Change(Sender: TObject);
begin
  if internalMsg then
    exit;
  internalMsg := true;
  begin
    ButtonUser.Click;
  end;
  internalMsg := false;
end;

procedure TFormCredentials.ButtonDefaultUserClick(Sender: TObject);
begin
  if internalMsg then
    exit;
  internalMsg := true;
  begin
    if EditPassword1.Tag = 1 then //password cached
      BitBtn_Ok.SetFocus
    else
      EditPassword1.SetFocus;
  end;
  internalMsg := false;
end;

procedure TFormCredentials.ButtonUserClick(Sender: TObject);
begin
  if internalMsg then
    exit;

  internalMsg := true;
  begin
    if Length(Trim(UsersComboBox.Text)) = 0 then
      UsersComboBox.SetFocus
    else
      if EditPassword2.Tag = 1 then //password cached
        BitBtn_Ok.SetFocus
      else
        EditPassword2.SetFocus;

  end;
  internalMsg := false;
end;

procedure TFormCredentials.UsersComboBoxChange(Sender: TObject);
begin
  if internalMsg then
    exit;
  internalMsg := true;
  begin
    ButtonUser.Click;
  end;
  internalMsg := false;
end;

procedure TFormCredentials.FormCreate(Sender: TObject);
begin
  internalMsg := false;
//  Image1.Picture.Bitmap := GetUserPicture;

  //RetrieveProfileImage(OnGetImage);
end;

procedure TFormCredentials.FormShow(Sender: TObject);

function IsSigned(const App : String; out Publisher : String) : Boolean;
begin
  result := true;
  Publisher := 'Cert Publisher';
end;

function GetBinaryPublisher : String;
begin
  result := 'Binary Publisher';
end;

var
  Publisher : String;
  Signed : Boolean;
begin
  JvEdit_AppName.Text := AppName;
  JvEdit_CmdLine.Text := '"'+AppName + '" ' +AppCmdLine;
  Label_DefaultUser.Caption := 'Elevate me as Administrator';

  if TimeOut = INFINITE then
  begin
    JvProgressBar.Max := 0;
    Timer1.Enabled := false;
  end
  else
  begin
    Timer1.Enabled := true;
    JvProgressBar.Max := Max(TimeOut div 100, 60);
    if JvProgressBar.Max < 10 then
      JvProgressBar.Max := 60;
  end;




  if not IsPasswordCacheAvailable then
  begin
    EditPassword1.Tag := 0;
    EditPassword1.Text := '';
    EditPassword1.EmptyValue := '';
    //EditPassword1.ThemedPassword := true;
    EditPassword1.ProtectPassword := true;

    EditPassword2.Tag := 0;
    EditPassword2.Text := '';
    EditPassword2.EmptyValue := '';
    //EditPassword1.ThemedPassword := true;
    EditPassword2.ProtectPassword := true;
  end
  else
  begin
    EditPassword1.Tag := 1;
    EditPassword1.EmptyValue := 'Password is cached';
    //EditPassword1.ThemedPassword := false;
    EditPassword1.ProtectPassword := false;
    EditPassword1.PasswordChar := #0;
    EditPassword1.Text := '';

    EditPassword2.Tag := 1;
    EditPassword2.EmptyValue := 'Password is cached';
    //EditPassword1.ThemedPassword := false;
    EditPassword2.ProtectPassword := false;
    EditPassword2.PasswordChar := #0;
    EditPassword2.Text := '';
  end;

  UsersComboBox.Text := UserName;
  if Length(UserName) > 0 then
  begin
    ButtonUser.Click;
  end
  else
  begin
    ButtonDefaultUser.Click;
  end;

  Signed := IsSigned(AppName ,Publisher);
  if Signed then
    JvStaticText_AppPublisher.Caption := Publisher
  else
    JvStaticText_AppPublisher.Caption := GetBinaryPublisher;

  JvGradient_SignedApp.Visible := Signed;
  JvGradient_UnsignedApp.Visible := not Signed;
  JvLabel_SignedApp.Visible := Signed;
  JvLabel_UnsignedApp.Visible := not Signed;
  
end;

procedure TFormCredentials.CenterInMonitor(const i : Integer);
var SX, SY : Integer;
begin
  SX := abs(abs(Screen.Monitors[I].BoundsRect.Right) - abs(Screen.Monitors[I].BoundsRect.Left));
  Left := Screen.Monitors[I].Left + ((SX div 2) - (Width div 2));

  SY := abs(abs(Screen.Monitors[I].BoundsRect.Bottom) - abs(Screen.Monitors[I].BoundsRect.Top));
  Top := Screen.Monitors[I].Top + ((SY div 2) - (Height div 2));

  exit;
  SY := Screen.Monitors[I].BoundsRect.Top - Screen.Monitors[I].BoundsRect.Bottom;
  SetBounds(Screen.Monitors[I].Left + SX, //+ ((SX - Width) div 2),
                    Top,
                    //(((Screen.Monitors[I].BoundsRect.Bottom - Screen.Monitors[I].BoundsRect.Top)- Height) div 2),
                     Width, Height);
//  Left := 0;
end;

procedure TFormCredentials.CheckBoxSaveLogonClick(Sender: TObject);
begin
  {if CheckBoxSaveLogon.Checked then
    MakeFullyVisible(Screen.Monitors[1])
  else
    MakeFullyVisible(Screen.Monitors[0]);
  DefaultMonitor := dmPrimary;
  Position := poScreenCenter;   }

  if CheckBoxSaveLogon.Checked then
    Flags := Flags or CLIENT_CACHECREDS
  else
    Flags := Flags and not CLIENT_CACHECREDS;
  
end;

destructor TFormCredentials.Destroy;
begin
  fAppName := GetFillPasswd;
  fAppCmdLine := GetFillPasswd;
  fUserName := GetFillPasswd;
  fPassword := GetFillPasswd;

  inherited;
end;

procedure TFormCredentials.BitBtn_OkClick(Sender: TObject);
begin
  ModalResult := mrOk;
  Close;
end;

procedure TFormCredentials.BitBtn1Click(Sender: TObject);
begin
  ModalResult := mrCancel;
  Close;
end;

procedure TFormCredentials.JvBevel1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var S : String;
begin
  try
    S := TControl(Sender).Name + #13#10+
        TControl(Sender).Hint;
  except
    S := '';
  end;
  if Sender = JvProgressBar then
  begin
    S := Format('%d seconds left until this dialog will be canceled and the eleveation is aborted. Click on it to restart the timer.',[JvProgressBar.Max - JvProgressBar.Position]);
  end;

  try
    JvLinkLabel2.Caption := S;
  except
    JvLinkLabel2.Caption := '';
  end;
end;

procedure TFormCredentials.UsersComboBoxMouseEnter(Sender: TObject);
begin
  try
     JvLinkLabel2.Caption := TControl(Sender).Name + #13#10+
       TControl(Sender).Hint;
  except
  end;
end;

procedure TFormCredentials.JvLinkLabel1LinkClick(Sender: TObject;
  LinkNumber: Integer; LinkText, LinkParam: String);
begin
  //
  TWebTools.OpenWebPage('http://blog.delphi-jedi.net');
end;

procedure TFormCredentials.JvProgressBarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  JvProgressBar.Position := 0;
end;

procedure TFormCredentials.OnGetImage;
begin
  //
 // ImageStream.SaveToFile('E:\Temp\_test.jpg');
  //JvImage1.LoadFromStream(ImageStream);
  //FreeAndNil(ImageStream);
  //Image1.Picture.LoadFromFile('E:\temp\bild-16.jpg');
end;

procedure TFormCredentials.Timer1Timer(Sender: TObject);
Var Element : TObject;
  P : TPoint;
begin
  P := Self.ScreenToClient(Mouse.CursorPos);

  Element := ControlAtPos(P,false,true, true);

  if Assigned(Element) and (Element = JvProgressBar) then
     JvBevel1MouseMove(Element,[],P.X, P.Y);

  JvProgressBar.Position := JvProgressBar.Position + 1;
  {JvStaticText_TimeOut.Caption :=
    Format('%d seconds left',
      [JvProgressBar.Max - JvProgressBar.Position]);}
end;

function GetFillPasswd : String;
var i,c : Integer;
begin

  repeat
    c := random(200);
  until c > 40;

  SetLength(result, c);
  for i := 1 to c  do
    result[i] := Char(random(255));
end;


begin
  randomize;
end.
