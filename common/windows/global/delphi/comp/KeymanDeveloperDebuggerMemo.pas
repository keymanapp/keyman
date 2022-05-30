(*
  Name:             KeymanDeveloperDebuggerMemo
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      8 Jun 2012

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          08 Jun 2012 - mcdurdin - I3323 - V9.0 - Extract debug-related code TPlus-Memo into subclass
*)
unit KeymanDeveloperDebuggerMemo;  // I3323

interface

uses
  System.Classes,
  Winapi.Messages,
  Winapi.Windows,
  Vcl.Controls,
  Vcl.StdCtrls;

type
  TKeymanDeveloperDebuggerMessageEvent = procedure(Sender: TObject; var Message: TMessage; var Handled: Boolean) of object;

  TMemoSelection = record
    Start, Finish: Integer;
    Anchor: Integer;
  end;

  TKeymanDeveloperDebuggerMemo = class(TMemo)
  private
    FOnMessage: TKeymanDeveloperDebuggerMessageEvent;
    FAllowUnicodeInput: Boolean;
    FIsDebugging: Boolean;
    procedure SetAllowUnicode(const Value: Boolean);
    function GetSelection: TMemoSelection;
    procedure SetSelection(const Value: TMemoSelection);
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
  protected
    procedure CreateHandle; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AllowUnicode: Boolean read FAllowUnicodeInput write SetAllowUnicode default True;
    property OnMessage: TKeymanDeveloperDebuggerMessageEvent read FOnMessage write FOnMessage;
    property Selection: TMemoSelection read GetSelection write SetSelection;
    property IsDebugging: Boolean read FIsDebugging write FIsDebugging;
  end;

procedure Register;

implementation

{ TKeymanDeveloperDebuggerMemo }

procedure TKeymanDeveloperDebuggerMemo.CNKeyDown(var Message: TWMKeyDown);
begin
  // Shortcuts for actions are handled with CN_KEYDOWN before we get WM_KEYDOWN,
  // so if we are debugging we want to filter those out so that the debugger
  // gets access to them instead of us. Note that this means that editor
  // shortcuts such as Ctrl+A (select all) will not be processed in the normal
  // way, so the host of this control needs to look after that.
  if IsDebugging
    then Exit
    else inherited;
end;

constructor TKeymanDeveloperDebuggerMemo.Create(AOwner: TComponent);
begin
  FAllowUnicodeInput := True;
  inherited Create(AOwner);
end;

procedure TKeymanDeveloperDebuggerMemo.CreateHandle;
begin
  inherited;
  if FAllowUnicodeInput
    then SetWindowLongW(Handle, GWL_WNDPROC, GetWindowLong(Handle, GWL_WNDPROC))
    else SetWindowLongA(Handle, GWL_WNDPROC, GetWindowLong(Handle, GWL_WNDPROC));
end;

function TKeymanDeveloperDebuggerMemo.GetSelection: TMemoSelection;
begin
  // EM_GETSEL doesn't tell us the anchor position, but we can figure
  // it out with this kludge. I am not aware of side effects from this
  // at this time.
  SendMessage(Handle, EM_GETSEL, NativeUInt(@Result.Start), NativeUInt(@Result.Finish));
  SendMessage(Handle, EM_SETSEL, -1, 0);
  SendMessage(Handle, EM_GETSEL, NativeUInt(@Result.Anchor), 0);
  SetSelection(Result);
end;

procedure TKeymanDeveloperDebuggerMemo.SetAllowUnicode(const Value: Boolean);
begin
  if Value <> fAllowUnicodeInput then
  begin
    fAllowUnicodeInput := Value;
    if HandleAllocated then RecreateWnd;
  end;
end;

procedure TKeymanDeveloperDebuggerMemo.SetSelection(
  const Value: TMemoSelection);
begin
  if Value.Anchor = Value.Start
    then SendMessage(Handle, EM_SETSEL, Value.Finish, Value.Start)
    else SendMessage(Handle, EM_SETSEL, Value.Start, Value.Finish);
end;

procedure TKeymanDeveloperDebuggerMemo.WndProc(var Message: TMessage);
var
  b: Boolean;
begin
  b := False;
  if Assigned(FOnMessage) then
    FOnMessage(Self, Message, b);
  if not b then inherited WndProc(Message);
end;

procedure Register;
begin
  RegisterComponents('Keyman', [TKeymanDeveloperDebuggerMemo]);
end;

end.
