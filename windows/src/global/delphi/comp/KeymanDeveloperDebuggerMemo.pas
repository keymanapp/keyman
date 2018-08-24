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
  Vcl.StdCtrls;

type
  TKeymanDeveloperDebuggerMessageEvent = procedure(Sender: TObject; var Message: TMessage; var Handled: Boolean) of object;

  TKeymanDeveloperDebuggerMemo = class(TMemo)
  private
    FOnMessage: TKeymanDeveloperDebuggerMessageEvent;
    FAllowUnicodeInput: Boolean;
    procedure SetAllowUnicode(const Value: Boolean);
  protected
    procedure CreateHandle; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AllowUnicode: Boolean read FAllowUnicodeInput write SetAllowUnicode default True;
    property OnMessage: TKeymanDeveloperDebuggerMessageEvent read FOnMessage write FOnMessage;
  end;

procedure Register;

implementation

{ TKeymanDeveloperDebuggerMemo }

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

procedure TKeymanDeveloperDebuggerMemo.SetAllowUnicode(const Value: Boolean);
begin
  if Value <> fAllowUnicodeInput then
  begin
    fAllowUnicodeInput := Value;
    if HandleAllocated then RecreateWnd;
  end;
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
