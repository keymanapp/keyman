(*
  Name:             UfrmUnicodeDataStatus
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Sep 2006

  Modified Date:    19 Nov 2007
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          14 Sep 2006 - mcdurdin - Moved to new location
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
*)
unit UfrmUnicodeDataStatus;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TfrmUnicodeDataStatus = class(TForm)
    lblPleaseWait: TLabel;
    lblStatus: TLabel;
    pbar: TProgressBar;
    procedure FormShow(Sender: TObject);
  private
    FCallback: TNotifyEvent;
    procedure WMUser100(var msg: TMessage); message WM_USER+100;
    { Private declarations }
  public
    { Public declarations }
    procedure UpdateStatus(msg: string; pos, max: Integer);
    property Callback: TNotifyEvent read FCallback write FCallback;
  end;

implementation

{$R *.DFM}

{ TfrmUnicodeDataStatus }

procedure TfrmUnicodeDataStatus.UpdateStatus(msg: string; pos,
  max: Integer);
begin
  if lblStatus.Caption <> msg then
  begin
    lblStatus.Caption := msg;
    lblStatus.Update;
  end;

  if max = 0
    then pbar.Position := 0
    else pbar.Position := pos * pbar.max div max;
  pbar.Update;

  Application.ProcessMessages;
end;

procedure TfrmUnicodeDataStatus.FormShow(Sender: TObject);
begin
  PostMessage(Handle, WM_USER+100, 0, 0);
end;

procedure TfrmUnicodeDataStatus.WMUser100(var msg: TMessage);
begin
  if Assigned(FCallback) then FCallback(Self);
  ModalResult := mrOk;
end;

end.
