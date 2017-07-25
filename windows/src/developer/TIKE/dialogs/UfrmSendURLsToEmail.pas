unit UfrmSendURLsToEmail;

interface

uses
  Winapi.Windows, Winapi.Messages,

  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmSendURLsToEmail = class(TForm)
    editEmail: TEdit;
    lblEmail: TLabel;
    cmdOK: TButton;
    cmdCancel: TButton;
    lblEmailNote: TLabel;
    cmdSMTPSettings: TButton;
    procedure cmdSMTPSettingsClick(Sender: TObject);
    procedure cmdOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure editEmailChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FHosts: TStrings;
    FKeyboardName: string;
    procedure EnableControls;
    { Private declarations }
  public
    { Public declarations }
    property KeyboardName: string read FKeyboardName write FKeyboardName;
    property Hosts: TStrings read FHosts;
  end;

implementation

{$R *.dfm}

uses
  IdEmailAddress,
  IdMessage,
  IdSMTP,
  KeymanDeveloperOptions,
  UfrmSMTPSetup,
  utilstr;

procedure TfrmSendURLsToEmail.cmdOKClick(Sender: TObject);
var
  msg: TIdMessage;
  i: Integer;
begin
  if FKeymanDeveloperOptions.SMTPServer = '' then
  begin
    if MessageDlg('You need to configure yout SMTP server before sending email.'#13#10#13#10+
      'Do you want to do this now?', mtConfirmation, mbOkCancel, 0) <> mrOk then Exit;
    with TfrmSMTPSetup.Create(Application.MainForm) do
    try
      if ShowModal <> mrOk then Exit;
    finally
      Free;
    end;
  end;

  FKeymanDeveloperOptions.TestEmailAddresses := editEmail.Text;
  FKeymanDeveloperOptions.Write;

  msg := TIdMessage.Create(Self);
  try
    msg.From.Address := 'no-reply@keyman.com';
    msg.Recipients.EMailAddresses := editEmail.Text;
    msg.Subject := 'Debug URLs for Keyman Keyboard '+FKeyboardName;
    msg.Body.Text :=
      'The following URLs are available for testing your keyboard '+FKeyboardName+':'#13#10#13#10;
    for i := 0 to FHosts.Count-1 do
      if (Pos('127.0.0.1',FHosts[i]) = 0) and (Pos('localhost',FHosts[i]) = 0) then
        msg.Body.Add('  '+FHosts[i]);
    msg.Body.Add(#13#10'You must be on the same network in order to access these URLs. '+
      'Some of the host names may not resolve if your DNS configuration does not include '+
      'them. If none of the URLs work, please check your firewall and network settings '+
      'on both your Keyman Developer computer and on this device, and try again.');
    with TIdSMTP.Create(Self) do
    try
      try
        Connect(FKeymanDeveloperOptions.SMTPServer);
        Send(msg);
        Disconnect;
      except
        on E:Exception do
        begin
          ShowMessage(
            'The message failed to send with the following error: '+E.Message+
            '.  Please check your SMTP settings and try again.');
          Exit;
        end;
      end;
    finally
      Free;
    end;
  finally
    msg.Free;
  end;

  ShowMessage('Message successfully sent to '+editEmail.Text+'.');
  ModalResult := mrOk;
end;

procedure TfrmSendURLsToEmail.cmdSMTPSettingsClick(Sender: TObject);
begin
  with TfrmSMTPSetup.Create(Self) do
  try
    if ShowModal = mrOk then
      FKeymanDeveloperOptions.Write;
  finally
    Free;
  end;
end;

procedure TfrmSendURLsToEmail.editEmailChange(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmSendURLsToEmail.EnableControls;
begin
  with TIdEMailAddressList.Create(Self) do
  try
    EMailAddresses := editEmail.Text;
    cmdOK.Enabled := Count > 0;
  finally
    Free;
  end;
end;

procedure TfrmSendURLsToEmail.FormCreate(Sender: TObject);
begin
  FHosts := TStringList.Create;
  with FKeymanDeveloperOptions do
  begin
    editEmail.Text := TestEmailAddresses;
  end;
  EnableControls;
end;

procedure TfrmSendURLsToEmail.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FHosts);
end;

end.
