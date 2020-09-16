(*
  Copyright (C) SIL International.
*)
unit Keyman.UI.UfrmProgress;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, httpuploader,
  UfrmKeymanBase, UserMessages;

type
  IProgressManager = interface
    ['{12CFB751-740A-4DBE-958A-74119E87DC50}']
    function GetCanCancel: Boolean;
    procedure SetCanCancel(const Value: Boolean);
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function IsCancelled: Boolean;
    procedure UpdateProgress(const status: string; position, total: Integer);
    property CanCancel: Boolean read GetCanCancel write SetCanCancel;
    property Title: string read GetTitle write SetTitle;
  end;

  TProgressCallbackProcedure = reference to function(Manager: IProgressManager): Boolean;

  TfrmProgress = class(TfrmKeymanBase, IProgressManager)
    progress: TProgressBar;
    cmdCancel: TButton;
    lblStatus: TLabel;
    procedure cmdCancelClick(Sender: TObject);
    procedure TntFormCreate(Sender: TObject);
  private
    FIsCancelled: Boolean;
    FCallbackProc: TProgressCallbackProcedure;

    procedure WMUserFormShown(var Message: TMessage); message WM_USER_FormShown;
  protected
    procedure UpdateProgress(const status: string; position, total: Integer);


    function GetCanCancel: Boolean;
    procedure SetCanCancel(const Value: Boolean);
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function IsCancelled: Boolean;

  public
    class function Execute(AOwner: TComponent; proc: TProgressCallbackProcedure): Boolean;
  end;

implementation

uses
  MessageIdentifiers, MessageIdentifierConsts;

{$R *.dfm}

procedure TfrmProgress.cmdCancelClick(Sender: TObject);
begin
  FIsCancelled := True;
end;

class function TfrmProgress.Execute(AOwner: TComponent; proc: TProgressCallbackProcedure): Boolean;
var
  frm: TfrmProgress;
begin
  frm := TfrmProgress.Create(AOwner);
  try
    frm.FCallbackProc := proc;
    Result := frm.ShowModal = mrOk;
  finally
    frm.Free;
  end;
end;

function TfrmProgress.GetCanCancel: Boolean;
begin
  Result := cmdCancel.Enabled;
end;

function TfrmProgress.GetTitle: string;
begin
  Result := Caption;
end;

function TfrmProgress.IsCancelled: Boolean;
begin
  Result := FIsCancelled;
end;

procedure TfrmProgress.SetCanCancel(const Value: Boolean);
begin
  cmdCancel.Enabled := Value;
  cmdCancel.Update;
end;

procedure TfrmProgress.SetTitle(const Value: string);
begin
  Caption := Value;
  Update;
end;

procedure TfrmProgress.TntFormCreate(Sender: TObject);
var
  s: string;
begin
  inherited;
  s := MsgFromId(SK_UIFontName);
  if s <> '' then Font.Name := s;
  cmdCancel.Caption := MsgFromId(SKButtonCancel);
  //Caption := MsgFromId(SKProgress_Title);
end;

procedure TfrmProgress.UpdateProgress(const status: string; position,
  total: Integer);
begin
  if status <> lblStatus.Caption then
  begin
    lblStatus.Caption := status;
    lblStatus.Update;
  end;

  if total = 0 then
  begin
    progress.Style := pbstMarquee;
  end
  else
  begin
    progress.Style := pbstNormal;
    progress.Max := total;
    progress.Position := position;
    progress.Update;
  end;

  Application.ProcessMessages;
end;

procedure TfrmProgress.WMUserFormShown(var Message: TMessage);
var
  Result: Boolean;
begin
  Result := False;
  FIsCancelled := False;

  if Assigned(FCallbackProc) then
    Result := FCallbackProc(Self);

  if Result
    then ModalResult := mrOk
    else ModalResult := mrCancel;
end;

end.

