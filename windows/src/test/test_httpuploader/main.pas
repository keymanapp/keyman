(*
  Name:             main
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      17 Sep 2007

  Modified Date:    14 Jun 2009
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          17 Sep 2007 - mcdurdin - Initial version
                    05 Aug 2008 - mcdurdin - Add more diagnostic reporting information to test http proxy code
                    25 May 2009 - mcdurdin - Testing for httpuploader
                    14 Jun 2009 - mcdurdin - I1704 - More testing for httpuploader
*)
unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, httpuploader;

type
  TForm2 = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    memoLog: TMemo;
    Button2: TButton;
    editServer: TEdit;
    editProtocol: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    TestNumber: Integer;
    { Private declarations }
    procedure LogStatus(Sender: THTTPUploader; const Message: string; Position, Total: Integer);
    procedure Log(const msg: string);
    procedure LogConnectingToServer(Sender: THTTPUploader; const Data: string);
    procedure LogFileBegin(Sender: THTTPUploader; const Data: string);
    procedure LogNameResolved(Sender: THTTPUploader; const Data: string);
    procedure LogResolvingName(Sender: THTTPUploader; const Data: string);
    procedure LogClosingConnection(Sender: THTTPUploader);
    procedure LogConnectedToServer(Sender: THTTPUploader; const Data: string);
    procedure LogConnectionClosed(Sender: THTTPUploader);
    procedure LogReceivingResponse(Sender: THTTPUploader);
    procedure LogRequestSent(Sender: THTTPUploader; const Data: DWord);
    procedure LogResponseReceived(Sender: THTTPUploader; const Data: DWord);
    procedure LogSendingRequest(Sender: THTTPUploader);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  GlobalProxySettings,
  UfrmOnlineUpdateSetup,
  Upload_Settings;

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  Log(#13#10'--- Starting Test ---');
  Log(FormatDateTime('dd mmm yyyy hh:nn', Now));

  GetProxySettings.Refresh;

  with THTTPUploader.Create(nil) do
  try
    OnStatus := LogStatus;
    OnConnectingToServer := LogConnectingToServer;
    OnFileBegin := LogFileBegin;
    OnNameResolved := LogNameResolved;
    OnResolvingName := LogResolvingName;
    OnClosingConnection := LogClosingConnection;
    OnConnectedToServer := LogConnectedToServer;
    OnConnectionClosed := LogConnectionClosed;
    OnReceivingResponse := LogReceivingResponse;
    OnRequestSent := LogRequestSent;
    OnResponseReceived := LogResponseReceived;
    OnSendingRequest := LogSendingRequest;

    Fields.Add('MachineName', GetEnvironmentVariable('COMPUTERNAME'));
    Fields.Add('TestNumber', IntToStr(TestNumber)); Inc(TestNumber);
    Fields.Add('Proxy.Server', GetProxySettings.Server);
    Fields.Add('Proxy.Port', IntToStr(GetProxySettings.Port));
    Fields.Add('Proxy.Username', GetProxySettings.Username);
    if GetProxySettings.Password <> ''
      then Fields.Add('Proxy.Password', '*********')
      else Fields.Add('Proxy.Password', '');

    Request.HostName := editServer.Text;
    Request.Protocol := editProtocol.Text;
    Request.UrlPath := '/prog/70/test_upload.php';

    Proxy.Server := GetProxySettings.Server;
    Log('ProxyServer is '+Proxy.Server);
    Proxy.Port := GetProxySettings.Port;
    Log('ProxyPort is '+IntToStr(Proxy.Port));
    Proxy.Username := GetProxySettings.Username;
    Log('ProxyUsername is '+Proxy.Username);
    Proxy.Password := GetProxySettings.Password;
    if PRoxy.Password <> ''
      then Log('ProxyPassword is *********')
      else Log('ProxyPassword is <blank>');

    if CheckBox1.Checked then
      Files.Add('ThisExecutable', ParamStr(0));

    Upload;
                           
    Log('Response Status = '+IntToStr(Response.StatusCode));
    Log('Response MessageBodyLength = '+IntToStr(Response.MessageBodyLength));
    Log('Response MessageBody = '#13#10+Response.MessageBodyAsString);

    if Response.StatusCode = 200 then
    begin
    end
    else
      raise Exception.Create('Error '+IntToStr(Response.StatusCode));
  finally
    Free;
  end;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  with TfrmOnlineUpdateSetup.Create(self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  editServer.Text := Upload_Server;
  editProtocol.Text := Upload_Protocol;
end;

procedure TForm2.Log(const msg: string);
begin
  memoLog.Lines.Add(msg);
  Application.ProcessMessages;
end;

procedure TForm2.LogStatus(Sender: THTTPUploader; const Message: string;
  Position, Total: Integer);
begin
  Log('Status: '+Message + ' -- ' +IntToStr(Position)+'/'+IntToStr(Total));
end;

procedure TForm2.LogFileBegin(Sender: THTTPUploader; const Data: string);
begin
  Log('FileBegin: '+Data);
end;

procedure TForm2.LogResolvingName(Sender: THTTPUploader; const Data: string);
begin
  Log('ResolvingName: '+Data);
end;

procedure TForm2.LogNameResolved(Sender: THTTPUploader; const Data: string);
begin
  Log('NameResolved: '+Data);
end;

procedure TForm2.LogConnectingToServer(Sender: THTTPUploader; const Data: string);
begin                                                           
  Log('ConnectingToServer: '+Data);
end;

procedure TForm2.LogConnectedToServer(Sender: THTTPUploader; const Data: string);
begin
  Log('ConnectedToServer: '+Data);
end;

procedure TForm2.LogSendingRequest(Sender: THTTPUploader);
begin
  Log('SendingRequest');
end;

procedure TForm2.LogRequestSent(Sender: THTTPUploader; const Data: DWord);
begin
  Log('RequestSent: '+IntToStr(Data));
end;

procedure TForm2.LogReceivingResponse(Sender: THTTPUploader);
begin
  Log('ReceivingResponse');
end;

procedure TForm2.LogResponseReceived(Sender: THTTPUploader; const Data: DWord);
begin
  Log('ResponseReceived: '+IntToStr(Data));
end;

procedure TForm2.LogClosingConnection(Sender: THTTPUploader);
begin
  Log('ClosingConnection');
end;

procedure TForm2.LogConnectionClosed(Sender: THTTPUploader);
begin
  Log('ConnectionClosed');
end;


end.
