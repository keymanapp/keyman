(*
  Name:             UfrmDownloadProgress
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Dec 2006

  Modified Date:    18 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Dec 2006 - mcdurdin - Initial version
                    31 Mar 2011 - mcdurdin - I2855 - Keyman Developer online update crashes with Integer Overflow
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit UfrmDownloadProgress;  // I3306

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, httpuploader, UfrmTike, UserMessages;

type
  TfrmDownloadProgress = class;

  TDownloadProgressCallback = procedure(Owner: TfrmDownloadProgress; var Result: Boolean) of object;

  TfrmDownloadProgress = class(TTIKEForm)
    progress: TProgressBar;
    cmdCancel: TButton;
    lblStatus: TLabel;
    procedure TntFormShow(Sender: TObject);
    procedure cmdCancelClick(Sender: TObject);
  private
    FCallback: TDownloadProgressCallback;
    FCancel: Boolean;
    procedure WMUserFormShown(var Message: TMessage); message WM_USER_FormShown;
  public

    procedure HTTPCheckCancel(Sender: THTTPUploader; var Cancel: Boolean);
    //procedure HTTPFileProgress(Sender: THTTPUploader; const FileName: string; dwFileBytesSent, dwLocalFileSize, dwSecondsToFileCompletion, dwOverallBytesSent, dwOverallBytesTotal, dwSecondsToOverallCompletion, dwBytesPerSecond: DWord);
    procedure HTTPStatus(Sender: THTTPUploader; const Message: string; Position, Total: Int64); // I2855
    property Callback: TDownloadProgressCallback read FCallback write FCallback;
    property Cancel: Boolean read FCancel;
  end;

implementation

{$R *.dfm}

procedure TfrmDownloadProgress.cmdCancelClick(Sender: TObject);
begin
  inherited;
  FCancel := True;
end;

procedure TfrmDownloadProgress.HTTPCheckCancel(Sender: THTTPUploader; var Cancel: Boolean);
begin
  Cancel := FCancel;
end;

{procedure TfrmDownloadProgress.HTTPFileProgress(
  Sender: THTTPUploader; const FileName: string; dwFileBytesSent,
  dwLocalFileSize, dwSecondsToFileCompletion, dwOverallBytesSent,
  dwOverallBytesTotal, dwSecondsToOverallCompletion, dwBytesPerSecond: DWord);
begin
  progress.Max := dwOverallBytesTotal;
  progress.Position := dwOverallBytesSent;
  progress.Update;
  Application.ProcessMessages;
end;}

procedure TfrmDownloadProgress.HTTPStatus(Sender: THTTPUploader;
  const Message: string; Position, Total: Int64); // I2855
begin
  if Total = 0
    then progress.Max := 100
    else progress.Max := Total;
  progress.Position := Position;
  progress.Update;
  lblStatus.Caption := Message;
  lblStatus.Update;
  Application.ProcessMessages;
end;

procedure TfrmDownloadProgress.TntFormShow(Sender: TObject);
begin
  inherited;
  PostMessage(Handle, WM_USER_FormShown, 0, 0);
end;

procedure TfrmDownloadProgress.WMUserFormShown(var Message: TMessage);
var
  Result: Boolean;
begin
  Result := False;
  FCancel := False;
  try
    if Assigned(FCallback) then
      FCallback(Self, Result);
    if Result
      then ModalResult := mrOk
      else ModalResult := mrCancel;
  except
    on EHTTPUploaderCancel do ModalResult := mrCancel;
    on E:EHTTPUploader do
    begin
      ShowMessage(E.Message);
      ModalResult := mrCancel;
    end;
  end;
end;

end.

