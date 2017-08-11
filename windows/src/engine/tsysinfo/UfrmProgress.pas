(*
  Name:             UfrmProgress
  Copyright:        Copyright (C) SIL International.
  Documentation:    km4.1
  Description:      Show progress messages while retrieving system info, uploading sysinfo packet
  Create Date:      13 May 2005

  Modified Date:    13 May 2005
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          13 May 2005 - mcdurdin - Integrated into kmshell from tsysinfo
*)
unit UfrmProgress;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TProgressExecuteEvent = procedure(Progress: TProgressBar; Caption: TLabel) of object;
  
  TfrmProgress = class(TForm)
    prog: TProgressBar;
    lblStatus: TLabel;
    procedure FormShow(Sender: TObject);
  private
    FOnExecute: TProgressExecuteEvent;
    { Private declarations }
    procedure WMUser(var Message: TMessage); message WM_USER;
  public
    { Public declarations }
    property OnExecute: TProgressExecuteEvent read FOnExecute write FOnExecute;
  end;

implementation

{$R *.dfm}

procedure TfrmProgress.FormShow(Sender: TObject);
begin
  PostMessage(Handle, WM_USER, 0, 0);
end;

procedure TfrmProgress.WMUser(var Message: TMessage);
begin
  if Assigned(FOnExecute) then FOnExecute(prog, lblStatus);
  ModalResult := mrOk;
end;

end.
