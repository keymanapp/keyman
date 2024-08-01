unit UfrmKeymanDebugLogMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Grids;

type
  TfrmDebugLog = class(TForm)
    panCommand: TPanel;
    cmdClearLog: TButton;
    gridLog: TStringGrid;
    editFilterProcess: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cmdClearLogClick(Sender: TObject);
  private
    Initialized: Boolean;
    procedure WMUser(var Message: TMessage); message WM_USER;
    procedure DebugMessage(Sender: TObject; const Message: string);
    procedure Report(Message: string; TopRow: Boolean = False);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDebugLog: TfrmDebugLog;

implementation

uses
  DebugManager,
  utilstr;

{$R *.dfm}

procedure TfrmDebugLog.cmdClearLogClick(Sender: TObject);
begin
  gridLog.RowCount := 1;
end;

procedure TfrmDebugLog.DebugMessage(Sender: TObject; const Message: string);
var
  s: PChar;
begin
  s := StrNew(PChar(Message));
  PostMessage(Handle, WM_USER, 0, NativeInt(s));
end;

procedure TfrmDebugLog.FormCreate(Sender: TObject);
begin
  try
    GetDebugManager(Handle).OnMessage := DebugMessage;
  except
    on E:Exception do
    begin
      ShowMessage(E.Message);
      Application.Terminate;
      Application.ShowMainForm := False;
      Exit;
    end;

  end;
  Initialized := True;
  gridLog.ColCount := 11;
  gridLog.RowCount := 2;
  gridLog.FixedRows := 1;
  Report('Platform	Process	PID	TID	ShiftState	ActualShiftState	TickCount	FocusHWND	ActiveHKL	SourceFile	Function	Message', True);
end;

procedure TfrmDebugLog.Report(Message: string; TopRow: Boolean = False);
var
  n: Integer;
  i: Integer;
  s: array of string;
begin
  SetLength(s, gridLog.ColCount);
  for i := 0 to High(s) do
    s[i] := StrToken(Message, #9);

  // Filtering
  if (editFilterProcess.Text <> '') and not SameText(editFilterProcess.Text, s[1]) then
    Exit;

  LockWindowUpdate(Handle);
  if TopRow then
    n := 0
  else
  begin
    gridLog.RowCount := gridLog.RowCount + 1;
    gridLog.FixedRows := 1;
    n := gridLog.RowCount - 1;
  end;

  for i := 0 to High(s) do
    gridLog.Cells[i, n] := s[i];

  if not TopRow then
  begin
    if gridLog.RowCount - gridLog.VisibleRowCount > 2 then
      gridLog.TopRow := gridLog.RowCount - gridLog.VisibleRowCount;
  end;
  LockWindowUpdate(0);
end;

procedure TfrmDebugLog.WMUser(var Message: TMessage);
begin
  Report(PChar(Message.LParam));
  StrDispose(PChar(Message.LParam));
end;

procedure TfrmDebugLog.FormDestroy(Sender: TObject);
begin
  if Initialized then
    GetDebugManager(Handle).OnMessage := nil;
end;

end.
