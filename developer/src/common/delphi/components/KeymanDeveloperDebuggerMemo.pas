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
  Winapi.ActiveX,
  Winapi.Messages,
  Winapi.RichEdit,
  Winapi.Windows,
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.StdCtrls,

  Keyman.Developer.UI.RichEdit41;

type
  TKeymanDeveloperDebuggerMessageEvent = procedure(Sender: TObject; var Message: TMessage; var Handled: Boolean) of object;

  TMemoSelection = record
    Start, Finish: Integer;
    Caret: Integer; // In a non-empty selection, Anchor is at the other end of the selection
    function ToString: string;
  end;

  TKeymanDeveloperDebuggerMemo = class(TRichEdit41)
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
    function GetTextCR: string;
    function GetSelTextCR: string;
  published
    property AllowUnicode: Boolean read FAllowUnicodeInput write SetAllowUnicode default True;
    property OnMessage: TKeymanDeveloperDebuggerMessageEvent read FOnMessage write FOnMessage;
    property Selection: TMemoSelection read GetSelection write SetSelection;
    property IsDebugging: Boolean read FIsDebugging write FIsDebugging;
  end;

procedure Register;

implementation

uses
  tom_TLB,
  System.SysUtils;

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
  PlainText := True;
end;

procedure TKeymanDeveloperDebuggerMemo.CreateHandle;
const
  TO_ADVANCEDTYPOGRAPHY = 1;
begin
  inherited;
  if FAllowUnicodeInput
    then SetWindowLongW(Handle, GWL_WNDPROC, GetWindowLong(Handle, GWL_WNDPROC))
    else SetWindowLongA(Handle, GWL_WNDPROC, GetWindowLong(Handle, GWL_WNDPROC));
  SendMessage(Handle, WM_SETTEXT, 0, NativeUInt(PChar('')));
  SendMessage(Handle, EM_SETTEXTMODE, TM_PLAINTEXT or TM_MULTICODEPAGE, 0);
  SendMessage(Handle, EM_SETTYPOGRAPHYOPTIONS, TO_ADVANCEDTYPOGRAPHY, TO_ADVANCEDTYPOGRAPHY);
  SendMessage(Handle, EM_SETLANGOPTIONS, 0, $0040 {IMF_NOIMPLICITLANG} or $0200 {IMF_NOKBDLIDFIXUP});
end;

function TKeymanDeveloperDebuggerMemo.GetSelection: TMemoSelection;
var
  Unk: IUnknown;
  TextDoc: ITextDocument;
  TextSel: ITextSelection;
begin
  SendMessage(Handle, EM_GETSEL, NativeUInt(@Result.Start), NativeUInt(@Result.Finish));
  Result.Caret := Result.Finish;

  if Result.Finish <> Result.Start then
  begin
    if SendMessage(Handle, EM_GETOLEINTERFACE, 0, LPARAM(@Unk)) <> 0 then
    begin
      if Unk.QueryInterface(IID_ITextDocument, TextDoc) = S_OK then
      begin
        TextSel := TextDoc.Selection;
        if TextSel <> nil then
        begin
          if (TextSel.Flags and tomSelStartActive) <> 0 then
          begin
            Result.Caret := Result.Start;
          end;
        end;
      end;
    end;
  end;
end;

function TKeymanDeveloperDebuggerMemo.GetSelTextCR: string;
begin
  Result := SelText.Replace(#$0D#$0A, #$0D, [rfReplaceAll]);
end;

function TKeymanDeveloperDebuggerMemo.GetTextCR: string;
begin
  Result := string(Text).Replace(#$0D#$0A, #$0D, [rfReplaceAll]);
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
  if Value.Caret = Value.Start
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

{ TMemoSelection }

function TMemoSelection.ToString: string;
begin
  Result := Format('TMemoSelection(start=%d finish=%d caret=%d)', [Start, Finish, Caret]);
end;

end.
