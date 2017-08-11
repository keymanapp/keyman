unit UfrmSMTPSetup;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmSMTPSetup = class(TForm)
    lblSMTPServer: TLabel;
    editSMTPServer: TEdit;
    cmdOK: TButton;
    cmdCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure cmdOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  KeymanDeveloperOptions;

procedure TfrmSMTPSetup.cmdOKClick(Sender: TObject);
begin
  FKeymanDeveloperOptions.SMTPServer := editSMTPServer.Text;
  ModalResult := mrOk;
end;

procedure TfrmSMTPSetup.FormCreate(Sender: TObject);
begin
  with FKeymanDeveloperOptions do
  begin
    editSMTPServer.Text := SMTPServer;
  end;
end;

end.
