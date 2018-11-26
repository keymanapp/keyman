unit GlobalKeyboardChangeManager;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  System.SysUtils,

  Keyman.System.SharedBuffers,
  LangSwitchManager;

type
  TGlobalKeyboardChangeManager = class(TThread)
  private type
    TLastKeyboard = record
      TIP: TSelectKeyboardBuffer;
      Keyboard: HKL;
    end;
  private var
    FWindow: HWND;
    FLastKeyboard: TLastKeyboard;
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
  lk: TLastKeyboard;
  FIdentity: DWORD;
begin
  lk := FLastKeyboard;

  if Keyboard is TLangSwitchKeyboard_TIP then
  begin
    TIP := Keyboard as TLangSwitchKeyboard_TIP;

    lk.TIP.LangID := TIP.Profile.langid;
    lk.TIP.CLSID := TIP.Profile.clsid;
    lk.TIP.GUIDProfile := TIP.Profile.guidProfile;
    TDebugLogClient.Instance.WriteMessage('TGlobalKeyboardChangeManager.PostChange TIP', []);

    if not CompareMem(@lk, @FLastKeyboard, SizeOf(TLastKeyboard)) then
    begin
      FLastKeyboard := lk;
      FIdentity := TSharedBufferManager.Identity.WriteSelectKeyboardBuffer(lk.TIP);
      PostMessage(FWindow, WM_USER_CHANGE, KMCI_SELECTKEYBOARD_BACKGROUND_TSF, FIdentity);
    end;
  end
  else if Keyboard is TLangSwitchKeyboard_WinKeyboard then
  begin
    WinKB := Keyboard as TLangSwitchKeyboard_WinKeyboard;
    lk.Keyboard := WinKB.HKL;

    TDebugLogClient.Instance.WriteMessage('TGlobalKeyboardChangeManager.PostChange HKL', []);

    if not CompareMem(@lk, @FLastKeyboard, SizeOf(TLastKeyboard)) then
    begin
      FLastKeyboard := lk;
      PostMessage(FWindow, WM_USER_CHANGE, KMCI_SELECTKEYBOARD_BACKGROUND, lk.Keyboard);
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
