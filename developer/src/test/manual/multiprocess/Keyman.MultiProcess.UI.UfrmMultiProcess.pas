unit Keyman.MultiProcess.UI.UfrmMultiProcess;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Keyman.Developer.System.MultiProcess,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfrmMultiProcess = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    tmrEnumerate: TTimer;
    Button1: TButton;
    lbProcess: TListBox;
    cmdFocus: TButton;
    procedure FormCreate(Sender: TObject);
    procedure tmrEnumerateTimer(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure lbProcessDblClick(Sender: TObject);
    procedure cmdFocusClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    function SelectedProcess: TMultiProcessInstance;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMultiProcess: TfrmMultiProcess;

implementation

uses
  utilexecute;

{$R *.dfm}

procedure TfrmMultiProcess.Button1Click(Sender: TObject);
begin
  TUtilExecute.Execute('"'+ParamStr(0)+'"', ExtractFileDir(ParamStr(0)), SW_SHOWNORMAL);
end;

procedure TfrmMultiProcess.cmdFocusClick(Sender: TObject);
var
  p: TMultiProcessInstance;
begin
  p := SelectedProcess;
  if Assigned(p) then
    p.BringToFront;
end;

function TfrmMultiProcess.SelectedProcess: TMultiProcessInstance;
begin
  if lbProcess.ItemIndex < 0 then
    Result := nil
  else
    Result := TMultiProcessInstance(lbProcess.Items.Objects[lbProcess.ItemIndex]);
end;

procedure TfrmMultiProcess.Edit1Change(Sender: TObject);
begin
  MultiProcessCoordinator.SetProcessIdentifier(Edit1.Text);
end;

procedure TfrmMultiProcess.FormCreate(Sender: TObject);
begin
  Caption := 'MultiProcess Test Form - '+IntToStr(Handle)+'/'+IntToStr(GetCurrentThreadId);
end;

procedure TfrmMultiProcess.lbProcessDblClick(Sender: TObject);
begin
  cmdFocusClick(cmdFocus);
end;

procedure TfrmMultiProcess.tmrEnumerateTimer(Sender: TObject);
var
  p: TMultiProcessInstance;
  LastTID: Cardinal;
begin
  p := SelectedProcess;
  if Assigned(p) then
  begin
    LastTID := p.ThreadId;
  end
  else
    LastTID := 0;

  lbProcess.Clear;
  MultiProcessCoordinator.Enumerate;
  for p in MultiProcessCoordinator.Processes do
  begin
    lbProcess.Items.AddObject(
      p.Handle.ToString + '/' + p.ThreadId.ToString + ': '+p.Identifier,
      p);
    if p.ThreadId = LastTID then
      lbProcess.ItemIndex := lbProcess.Items.Count - 1;
  end;
end;

end.
