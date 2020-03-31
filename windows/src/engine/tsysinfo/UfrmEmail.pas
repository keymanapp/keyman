(*
  Name:             UfrmEmail
  Copyright:        Copyright (C) SIL International.
  Documentation:    km4.1
  Description:      Send a sysinfo packet to secure website (despite misnomer)
  Create Date:      13 May 2005

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          13 May 2005 - mcdurdin - Integrated into kmshell from tsysinfo
                    16 Aug 2005 - mcdurdin - Add privacy statement
                    20 Jul 2008 - mcdurdin - I1554 - Don't require password for tsysinfo
                    20 Jul 2008 - mcdurdin - I5555 - Support proxy in tsysinfo
                    28 Jul 2008 - mcdurdin - I1574 - Report exceptions in all TS apps
                    28 Aug 2008 - mcdurdin - I1593 - Delete error logs after successful upload only
                    23 Apr 2009 - mcdurdin - I1941 - Collect minidump files, diagnostic log files and zip them with the diagnostic report (don't send them separately)
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
*)
unit UfrmEmail;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, httpuploader, ComCtrls;

type
  TfrmEmail = class(TForm)
    lblEmail: TLabel;
    editEmail: TEdit;
    lblIntro: TLabel;
    cmdSend: TButton;
    cmdCancel: TButton;
    memoBody: TMemo;
    Label1: TLabel;
    lblCase: TLabel;
    editTitle: TEdit;
    lblSecure: TLabel;
    Image1: TImage;
    lblPrivacy1: TLabel;
    lblPrivacy2: TLabel;
    lblPrivacy3: TLabel;
    lblPrivacyStatement: TLabel;
    lblPrivacy4: TLabel;
    lblName: TLabel;
    editName: TEdit;
    procedure cmdSendClick(Sender: TObject);
    procedure lblPrivacyStatementClick(Sender: TObject);
  private
    FStatusProgress: TProgressBar;
    FStatusCaption: TLabel;
    FAttachFile: string;
    procedure SendEmail(Progress: TProgressBar; Caption: TLabel);
    procedure SendEmailStatusEvent(Sender: THTTPUploader;
      const Message: string; Position, Total: Int64);
  public
    property AttachFile: string read FAttachFile write FAttachFile;
  end;

implementation

uses
  GlobalProxySettings,
  Math,
  UfrmProgress,
  Upload_Settings,
  utilexecute,
  utildir,
  VersionInfo,
  WinInet;

{$R *.dfm}

procedure TfrmEmail.cmdSendClick(Sender: TObject);
begin
  if (editEmail.Text = '') or (editTitle.Text = '') or (memoBody.Text = '') then
  begin
    ShowMessage('Your email address and the details of the problem, cannot be blank.');
    Exit;
  end;

  with TfrmProgress.Create(Self) do
  try
    Caption := 'Sending Message';
    OnExecute := SendEmail;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfrmEmail.SendEmailStatusEvent(Sender: THTTPUploader; const Message: string; Position, Total: Int64);
begin
  FStatusCaption.Caption := Message;
  FStatusCaption.Update;
  if Total > 0 then
  begin
    FStatusProgress.Max := Total;
    FStatusProgress.Position := Position;
    FStatusProgress.Update;
  end;
  Application.ProcessMessages;
end;

procedure TfrmEmail.SendEmail(Progress: TProgressBar; Caption: TLabel);
var
  s: string;
begin
  FStatusProgress := Progress;
  FStatusCaption := Caption;
  try
    with THTTPUploader.Create(Self) do
    try
      OnStatus := SendEmailStatusEvent;

      Proxy.Server := GetProxySettings.Server;
      Proxy.Port := GetProxySettings.Port;
      Proxy.Username := GetProxySettings.Username;
      Proxy.Password := GetProxySettings.Password;

      Files.Add('File', FAttachFile);
      Fields.Add('Username', AnsiString(editEmail.Text));
      Fields.Add('Title', AnsiString(editTitle.Text));
      Fields.Add('Body', AnsiString(memoBody.Text));
      Request.Agent := API_UserAgent_Diagnostics;

      Request.HostName := API_Server;
      Request.Protocol := API_Protocol;
      Request.UrlPath := API_Path_SubmitDiag;

      Upload;
      s := Trim(string(Response.MessageBodyAsString));
      if Copy(s,1,7) = '<error>' then
      begin
        Delete(s,1,7);
        if Pos('</error>', s) > 0 then Delete(s, Pos('</error>', s), Length(s));
        ShowMessage('The report was not successfully sent: '+Trim(s));
        Exit;
      end
      else if Copy(s,1,8) = '<result>' then
      begin
        Delete(s,1,8);
        if Pos('</result>', s) > 0 then Delete(s, Pos('</result>', s), Length(s));
        ShowMessage('The report was successfully sent to Keyman Support and has been assigned case #'+Trim(s)+'.  You should receive a reply from Support within 4 business days.');
        ModalResult := mrOk;
      end
      else
        ShowMessage('An error occurred during the upload process: '+s+'.  Please send this message to Keyman Support.');
    finally
      Free;
    end;
  except
    on E:Exception do
    begin
      ShowMessage('An error occurred during the upload process: '+E.Message+'.  You must be online.  If you have a firewall, you may need to configure your Proxy Server Settings in Control Panel/Internet Options.');
    end;
  end;
  FStatusProgress := nil;
  FStatusCaption := nil;
end;

procedure TfrmEmail.lblPrivacyStatementClick(Sender: TObject);
begin
  TUtilExecute.URL(lblPrivacyStatement.Hint);  // I3349
end;

end.
