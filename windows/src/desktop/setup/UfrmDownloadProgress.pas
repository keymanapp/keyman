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
                    27 Mar 2008 - mcdurdin - Use TfrmKeymanBase
                    31 Mar 2011 - mcdurdin - I2855 - Keyman Developer online update crashes with Integer Overflow
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit UfrmDownloadProgress;  // I3306

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Vcl.ComCtrls,

  httpuploader;

type
  TfrmDownloadProgress = class;

  TDownloadProgressCallback = procedure(Owner: TfrmDownloadProgress; var Result: Boolean) of object;

  TfrmDownloadProgress = class(TForm)
    progress: TProgressBar;
    cmdCancel: TButton;
    lblStatus: TLabel;
    procedure cmdCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FCallback: TDownloadProgressCallback;
    FCancel: Boolean;
    procedure WMUserFormShown(var Message: TMessage); message WM_USER;
    procedure SetProgressPosition(Position: Integer);
  public
    function Status(const Message: string; Position: Integer; Total: Integer = -1): Boolean;
    procedure HTTPCheckCancel(Sender: THTTPUploader; var Cancel: Boolean);
    procedure HTTPStatus(Sender: THTTPUploader; const Message: string; Position, Total: Int64);  // I2855
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

procedure TfrmDownloadProgress.FormShow(Sender: TObject);
begin
  PostMessage(Handle, WM_USER, 0, 0); // Starts process after form displays
end;

procedure TfrmDownloadProgress.HTTPCheckCancel(Sender: THTTPUploader; var Cancel: Boolean);
begin
  Cancel := FCancel;
end;

procedure TfrmDownloadProgress.HTTPStatus(Sender: THTTPUploader;
  const Message: string; Position, Total: Int64);  // I2855
begin
  if Total = 0
    then progress.Max := 100
    else progress.Max := Integer(Total);
  SetProgressPosition(Integer(Position));
  lblStatus.Caption := Message;
  lblStatus.Update;
  Application.ProcessMessages;
end;

procedure TfrmDownloadProgress.SetProgressPosition(Position: Integer);
begin
  progress.Position := Position;
  if (Position < progress.Max) then
  begin
    // This 'hack' encourages the progress bar to move immediately to the
    // requested position, because otherwise it animates into position, which
    // won't happen because we're doing all the work on the main thread. It is
    // not worth refactoring into a threaded model for the sake of the progress
    // bar!
    progress.StepBy(1);
    progress.StepBy(-1);
  end;
  progress.Update;
end;

function TfrmDownloadProgress.Status(const Message: string; Position,
  Total: Integer): Boolean;
begin
  if Total > 0 then progress.Max := Total;
  SetProgressPosition(Position);
  lblStatus.Caption := Message;
  lblStatus.Update;
  Application.ProcessMessages;
  Result := not FCancel;
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
    on E:EHTTPUploader do
    begin
      ShowMessage(E.Message);
      ModalResult := mrCancel;
    end;
    on E:EHTTPUploaderCancel do
    begin
      ModalResult := mrCancel;
    end;
  end;
end;

end.

