unit GlobalKeyboardChangeManager;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.Classes,
  System.SysUtils,

  LangSwitchManager;

type
  TGlobalKeyboardChangeManager = class(TThread)
  private
    FWindow: HWND;
    FLastKeyboard: string;
    procedure WndProc(var Message: TMessage);
    procedure ProcessChange(Mode: Integer; Param: NativeUInt);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure PostChange(Keyboard: TLangSwitchKeyboard);
  end;

implementation

uses
  Keyman.System.DebugLogClient,
  UfrmKeyman7Main;

const
  WM_USER_DESTROY = WM_USER + 1;
  WM_USER_CHANGE  = WM_USER + 2;

{ TGlobalKeyboardChangeManager }

constructor TGlobalKeyboardChangeManager.Create;
begin
  inherited Create(False);
end;

destructor TGlobalKeyboardChangeManager.Destroy;
begin
  if FWindow <> 0 then
    PostMessage(FWindow, WM_USER_DESTROY, 0, 0);
  inherited Destroy;
end;

procedure TGlobalKeyboardChangeManager.Execute;
var
  msg: TMsg;
begin
  FWindow := AllocateHwnd(WndProc); // TODO: Not thread safe!
  while GetMessage(msg, 0, 0, 0) do
  begin
    DispatchMessage(msg);
  end;
  DeallocateHwnd(FWindow);
end;

procedure TGlobalKeyboardChangeManager.PostChange(
  Keyboard: TLangSwitchKeyboard);
var
  TIP: TLangSwitchKeyboard_TIP;
  WinKB: TLangSwitchKeyboard_WinKeyboard;
  s: string;
  FAtom: Word;
begin
  if Keyboard is TLangSwitchKeyboard_TIP then
  begin
    TIP := Keyboard as TLangSwitchKeyboard_TIP;

    // ATOM construction copied from TLangSwitchKeyboard
    s := IntToStr(TIP.Profile.langid) + '|' + GUIDToString(TGUID(TIP.Profile.clsid)) + '|' + GUIDToString(TGUID(TIP.Profile.guidProfile));

    TDebugLogClient.Instance.WriteMessage('TGlobalKeyboardChangeManager.PostChange: '+s, []);

    if s <> FLastKeyboard then
    begin
      FAtom := GlobalAddAtom(PChar(s));
      FLastKeyboard := s;
      PostMessage(FWindow, WM_USER_CHANGE, KMCI_SELECTKEYBOARD_BACKGROUND_TSF, FAtom);
    end;
  end
  else if Keyboard is TLangSwitchKeyboard_WinKeyboard then
  begin
    WinKB := Keyboard as TLangSwitchKeyboard_WinKeyboard;
    s := IntToStr(WinKB.HKL);

    TDebugLogClient.Instance.WriteMessage('TGlobalKeyboardChangeManager.PostChange: '+s, []);

    if s <> FLastKeyboard then
    begin
      FLastKeyboard := s;
      PostMessage(FWindow, WM_USER_CHANGE, KMCI_SELECTKEYBOARD_BACKGROUND, WinKB.HKL);
    end;
  end
  else
  begin
    Exit;
  end;

end;

procedure TGlobalKeyboardChangeManager.ProcessChange(Mode: Integer; Param: NativeUInt);
begin
  SendMessageTimeout(HWND_BROADCAST, wm_keyman_control_internal, Mode, Param, SMTO_ABORTIFHUNG, 250, nil);
  if Mode = KMCI_SELECTKEYBOARD_BACKGROUND_TSF then
    GlobalDeleteAtom(Param);
end;

procedure TGlobalKeyboardChangeManager.WndProc(var Message: TMessage);
begin
  with Message do
  begin
    case Message.Msg of
      WM_USER_DESTROY:
        DestroyWindow(FWindow);
      WM_DESTROY:
        begin
          PostQuitMessage(0);
          Result := 0;
        end;
      WM_USER_CHANGE:
        ProcessChange(WParam, LParam);
    else
      Message.Result := DefWindowProc(FWindow, Msg, WParam, LParam);
    end;
  end;
end;

end.
