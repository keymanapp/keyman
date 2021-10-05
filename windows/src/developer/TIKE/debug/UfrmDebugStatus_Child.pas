(*
  Name:             UfrmDebugStatus_Child
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      14 Sep 2006

  Modified Date:    3 Aug 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          14 Sep 2006 - mcdurdin - Initial version
                    18 May 2012 - mcdurdin - I3323 - V9.0 - Change from Plus-MemoU to Plus-Memo
                    03 Aug 2015 - mcdurdin - I4809 - Track keystrokes in debug status form
*)
unit UfrmDebugStatus_Child;  // I3323

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,

  KeymanDeveloperDebuggerMemo,
  Keyman.System.Debug.DebugCore,
  Keyman.System.Debug.DebugEvent,
  debugkeyboard,
  UfrmTike,
  UframeTextEditor;

type
  TfrmDebugStatus_Child = class(TTikeForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FDebugKeyboard: TDebugKeyboard;
    FDisplayFont: TFont;
    FEditorMemo: TframeTextEditor;
    FDebugMemo: TKeymanDeveloperDebuggerMemo;
    FCurrentEvent: TDebugEvent;
    FDebugCore: TDebugCore;
  protected
    property EditorMemo: TframeTextEditor read FEditorMemo;
    property debugkeyboard: TDebugKeyboard read FDebugKeyboard;
    property DisplayFont: TFont read FDisplayFont;
    property CurrentEvent: TDebugEvent read FCurrentEvent;
    property DebugCore: TDebugCore read FDebugCore;

    function memoDebug: TKeymanDeveloperDebuggerMemo;

    procedure DebugEventChanged; virtual;
    procedure DebugKeyboardChanged; virtual;
    procedure DisplayFontChanged; virtual;
  public
    procedure StartBatch; virtual;
    procedure FinishBatch; virtual;

    procedure SetDisplayFont(Value: TFont);
    procedure SetDebugKeyboard(const Value: TDebugKeyboard);
    procedure SetEditorMemo(Value: TframeTextEditor);
    procedure SetDebugMemo(Value: TKeymanDeveloperDebuggerMemo);
    procedure SetCurrentEvent(const Value: TDebugEvent);
    procedure SetDebugCore(const Value: TDebugCore);
  end;

implementation

{$R *.dfm}

{ TfrmDebugStatus_Child }

procedure TfrmDebugStatus_Child.DebugEventChanged;
begin

end;

procedure TfrmDebugStatus_Child.DebugKeyboardChanged;
begin

end;

procedure TfrmDebugStatus_Child.DisplayFontChanged;
begin

end;

procedure TfrmDebugStatus_Child.FormCreate(Sender: TObject);
begin
  inherited;
  FDisplayFont := TFont.Create;
  DisplayFontChanged;
end;

procedure TfrmDebugStatus_Child.FormDestroy(Sender: TObject);
begin
  FDisplayFont.Free;
end;

function TfrmDebugStatus_Child.memoDebug: TKeymanDeveloperDebuggerMemo;
begin
  Result := FDebugMemo;
end;

procedure TfrmDebugStatus_Child.SetCurrentEvent(const Value: TDebugEvent);
begin
  FCurrentEvent := Value;
  DebugEventChanged;
end;

procedure TfrmDebugStatus_Child.SetDebugCore(const Value: TDebugCore);
begin
  FDebugCore := Value;
end;

procedure TfrmDebugStatus_Child.SetDebugKeyboard(const Value: TDebugKeyboard);
begin
  FDebugKeyboard := Value;
  DebugKeyboardChanged;
end;

procedure TfrmDebugStatus_Child.SetDebugMemo(Value: TKeymanDeveloperDebuggerMemo);
begin
  FDebugMemo := Value;
end;

procedure TfrmDebugStatus_Child.SetDisplayFont(Value: TFont);
begin
  FDisplayFont.Assign(Value);
  DisplayFontChanged;
end;

procedure TfrmDebugStatus_Child.SetEditorMemo(Value: TframeTextEditor);
begin
  FEditorMemo := Value;
end;

procedure TfrmDebugStatus_Child.StartBatch;
begin

end;

procedure TfrmDebugStatus_Child.FinishBatch;
begin

end;

end.
