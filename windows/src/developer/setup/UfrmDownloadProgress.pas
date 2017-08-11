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
                    05 Dec 2006 - mcdurdin - Localize caption
                    15 Jan 2007 - mcdurdin - Use font from locale.xml
                    04 Jun 2007 - mcdurdin - Initial version - for setup
                    19 Jun 2007 - mcdurdin - I817 - Translate to Unicode and remove Forms dependence
                    23 Aug 2007 - mcdurdin - Initial version from Desktop
                    31 Mar 2011 - mcdurdin - I2855 - Keyman Developer online update crashes with Integer Overflow
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit UfrmDownloadProgress;  // I3306

interface

uses
  CommCtrl, resource,
  SetupForm, TntDialogHelp, Windows, Messages, SysUtils, Variants, Classes, httpuploader;

const
  WM_USER_FormShown = WM_USER+100;
type
  TfrmDownloadProgress = class;

  TDownloadProgressCallback = procedure(Owner: TfrmDownloadProgress; var Result: Boolean) of object;

  TfrmDownloadProgress = class(TSetupForm)
    procedure FormShow(Sender: TObject); override;
    procedure cmdCancelClick(Sender: TObject);
  private
    FCallback: TDownloadProgressCallback;
    FCancel: Boolean;
    Max: Int64;  // I2855
    procedure WMUserFormShown(var Message: TMessage); message WM_USER_FormShown;
  public
    function DialogID: Integer; override;
    procedure DlgMain(var Message: TMessage); override;
    function ProcessCommand(ID, NotificationCode: Integer; hControl: HWND): Boolean; override;

    procedure HTTPCheckCancel(Sender: THTTPUploader; var Cancel: Boolean);
    //procedure HTTPFileProgress(Sender: THTTPUploader; const FileName: string; dwFileBytesSent, dwLocalFileSize, dwSecondsToFileCompletion, dwOverallBytesSent, dwOverallBytesTotal, dwSecondsToOverallCompletion, dwBytesPerSecond: DWord);
    procedure HTTPStatus(Sender: THTTPUploader; const Message: string; Position, Total: Int64);  // I2855
    property Callback: TDownloadProgressCallback read FCallback write FCallback;
    property Cancel: Boolean read FCancel;
  end;

implementation

{R *.dfm}

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
  const Message: string; Position, Total: Int64);  // I2855
var
  msg: TMsg;
begin
  Max := Total;
  if Max = 0 then
  begin
    Max := 100;
    SendDlgItemMessage(Handle, IDC_PROGRESS1, PBM_SETRANGE, 0, MAKELONG(0, 100));
  end;

  SendDlgItemMessage(Handle, IDC_PROGRESS1, PBM_SETPOS, Position * 100 div Max, 0);
  UpdateWindow(GetDlgItem(Handle, IDC_STATUS));
  UpdateWindow(GetDlgItem(Handle, IDC_PROGRESS1));
  SetWindowText(GetDlgItem(Handle, IDC_STATUS), PWideChar(Message));

  while PeekMessage(msg, 0, 0, 0, PM_REMOVE) do
  begin
    if not IsDialogMessage(Handle, msg) then
    begin
      TranslateMessage(msg);
      DispatchMessage(msg);
    end;
  end;
end;

function TfrmDownloadProgress.ProcessCommand(ID, NotificationCode: Integer;
  hControl: HWND): Boolean;
begin
  Result := True;
  case ID of
    IDCANCEL: cmdCancelClick(Self);
    else Result := False;
  end;
end;

function TfrmDownloadProgress.DialogID: Integer;
begin
  Result := 101;
end;

procedure TfrmDownloadProgress.DlgMain(var Message: TMessage);
begin
  case Message.Msg of
    WM_USER_FormShown:
      WMUserFormShown(Message);
  else inherited;
  end;
end;

procedure TfrmDownloadProgress.FormShow(Sender: TObject);
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
  UpdateWindow(Handle);
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
      ShowMessageW(E.Message);
      ModalResult := mrCancel;
    end;
  end;
end;

end.

