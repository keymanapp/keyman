(*
  Name:             KeymanTrayIcon
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    18 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Initial version
                    12 Oct 2007 - mcdurdin - I984, I1077, I1078 - Handle crash when starting Keyman with Windows and the tray is unresponsive
                    19 Nov 2007 - mcdurdin - I1148 - Fix crash starting with windows
                    27 Mar 2008 - mcdurdin - I1372 - Tooltip in tray icon was not Unicode
                    27 Mar 2008 - mcdurdin - I1248 - Welcome process - balloon support
                    19 Sep 2008 - mcdurdin - I1649 - Tooltray icon robustness
                    29 Sep 2008 - mcdurdin - I1649 - Additional work around tray icon robustness - try to modify an existing icon first before attempting re-adding
                    16 Jan 2009 - mcdurdin - I1649 - Additional work around tray icon robustness
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit KeymanTrayIcon;  // I3306

interface

uses
  Windows, Messages, Classes, ShellApi, StdCtrls, Graphics, Menus,
  Controls, ExtCtrls, Consts, UserMessages;

{ TTrayIcon }

const
  NIIF_USER = 4;
  NIIF_NOSOUND = $10;
  NIIF_LARGE_ICON = $20;
  NIIF_RESPECT_QUIET_TIME = $80;
  
type
  TBalloonFlags = (bfNone = NIIF_NONE, bfInfo = NIIF_INFO,
    bfWarning = NIIF_WARNING, bfError = NIIF_ERROR, bfUser = NIIF_USER);

  TTrayIconUnresponsiveEvent = procedure(Sender: TObject; RetryCount: Integer; var ShouldCancel: Boolean) of object;
    // I984, I1077, I1078

  TNotifyIconData_XP = record
    cbSize: DWORD;
    Wnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array [0..127] of WideChar;
    dwState: DWORD;
    dwStateMask: DWORD;
    szInfo: array [0..255] of WideChar;
    uTimeoutVersion: UINT;
    szInfoTitle: array [0..63] of WideChar;
    dwInfoFlags: DWORD;
    guidItem: TGUID;
    //hBalloonIcon: HICON;
  end;

  TTrayIconMessageEvent = procedure(Sender: TObject; msg: Integer) of object;

  TCustomKeymanTrayIcon = class(TComponent)
  private
    FAnimate: Boolean;
    FData: TNotifyIconData_XP;
    FIsClicked: Boolean;
    FCurrentIcon: TIcon;
    FIcon: TIcon;
    FIconList: TImageList;
    FPopupMenu: TPopupMenu;
    FTimer: TTimer;
    FHint: WideString;
    FIconIndex: Integer;
    FVisible: Boolean;
    FOnMouseMove: TMouseMoveEvent;
    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    FOnAnimate: TNotifyEvent;
    FBalloonHint: WideString;
    FBalloonTitle: WideString;
    FBalloonFlags: TBalloonFlags;
    FOnUnresponsive: TTrayIconUnresponsiveEvent;
    FLastError: DWORD; // I1148
    RetryCount: Integer;
    FOnBalloonClick: TNotifyEvent;
    FOnMessage: TTrayIconMessageEvent;
    FOnBalloonShow: TNotifyEvent;
    FOnBalloonHide: TNotifyEvent;
    FOnBalloonTimeout: TNotifyEvent;
    FBalloonIcon: THandle;
    //class var
    //  RM_TaskbarCreated: DWORD;
    procedure IconChange(Sender: TObject);
    procedure RefreshIconTimeout;
    procedure StartRefreshIconTimeout;
    procedure SetBalloonIcon(const Value: THandle);
    function GetNotificationWindow: THandle;
  protected
    procedure SetHint(const Value: WideString);
    function GetAnimateInterval: Cardinal;
    procedure SetAnimateInterval(Value: Cardinal);
    procedure SetAnimate(Value: Boolean);
    procedure SetBalloonHint(const Value: WideString);
    function GetBalloonTimeout: Integer;
    procedure SetBalloonTimeout(Value: Integer);
    procedure SetBalloonTitle(const Value: WideString);
    procedure SetVisible(Value: Boolean); virtual;
    procedure SetIconIndex(Value: Integer); virtual;
    procedure SetIcon(Value: TIcon);
    procedure SetIconList(Value: TImageList);
    procedure WindowProc(var Message: TMessage); virtual;
    procedure DoOnAnimate(Sender: TObject); virtual;
    property Data: TNotifyIconData_XP read FData;
    function RefreshIcon(Message: Integer): Boolean;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Refresh;
    procedure SetDefaultIcon;
    procedure ShowBalloonHint; virtual;
    procedure HideBalloonHint; virtual;
    property Animate: Boolean read FAnimate write SetAnimate default False;
    property AnimateInterval: Cardinal read GetAnimateInterval write SetAnimateInterval default 1000;
    property Hint: WideString read FHint write SetHint;
    property BalloonHint: WideString read FBalloonHint write SetBalloonHint;
    property BalloonTitle: WideString read FBalloonTitle write SetBalloonTitle;
    property BalloonTimeout: Integer read GetBalloonTimeout write SetBalloonTimeout default 3000;
    property BalloonFlags: TBalloonFlags read FBalloonFlags write FBalloonFlags default bfNone;
    property BalloonIcon: THandle read FBalloonIcon write SetBalloonIcon;
    property NotificationWindow: THandle read GetNotificationWindow;
    property Icon: TIcon read FIcon write SetIcon;
    property Icons: TImageList read FIconList write SetIconList;
    property IconIndex: Integer read FIconIndex write SetIconIndex default 0;
    property LastError: DWORD read FLastError; // I1148
    property PopupMenu: TPopupMenu read FPopupMenu write FPopupMenu;
    property Visible: Boolean read FVisible write SetVisible default False;
    property OnMessage: TTrayIconMessageEvent read FOnMessage write FOnMessage;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnBalloonClick: TNotifyEvent read FOnBalloonClick write FOnBalloonClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnAnimate: TNotifyEvent read FOnAnimate write FOnAnimate;

    property OnBalloonShow: TNotifyEvent read FOnBalloonShow write FOnBalloonShow;
    property OnBalloonHide: TNotifyEvent read FOnBalloonHide write FOnBalloonHide;
    property OnBalloonTimeout: TNotifyEvent read FOnBalloonTimeout write FOnBalloonTimeout;

    property OnUnresponsive: TTrayIconUnresponsiveEvent read FOnUnresponsive write FOnUnresponsive;
  end;

  TKeymanTrayIcon = class(TCustomKeymanTrayIcon)
  published
    property Animate;
    property AnimateInterval;
    property Hint;
    property BalloonHint;
    property BalloonTitle;
    property BalloonTimeout;
    property BalloonFlags;
    property Icon;
    property Icons;
    property IconIndex;
    property PopupMenu;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseDown;
    property OnAnimate;
    property OnUnresponsive;
  end;

implementation

uses
  Forms,
  GetOsVersion,
  KLog,
  SysUtils,
  WideStrUtils;

const
  NOTIFYICON_VERSION = 3;

{ TTrayIcon}

constructor TCustomKeymanTrayIcon.Create(Owner: TComponent);
begin
  inherited;
  FAnimate := False;
  FBalloonFlags := bfNone;
  BalloonTimeout := 3000;
  FIcon := TIcon.Create;
  FIcon.OnChange := IconChange;
  FCurrentIcon := TIcon.Create;
  FTimer := TTimer.Create(Nil);
  FIconIndex := 0;
  FVisible := False;
  FIsClicked := False;
  FTimer.Enabled := False;
  FTimer.OnTimer := DoOnAnimate;
  FTimer.Interval := 1000;

  if not (csDesigning in ComponentState) then
  begin
    FillChar(FData, SizeOf(FData), 0);
    {if GetOs in [osWin2000, osWinXP, osWin2003Server]
      then FData.cbSize := SizeOf(FData) - SizeOf(HICON)
      else}
      FData.cbSize := SizeOf(FData);
    FData.Wnd := Classes.AllocateHwnd(WindowProc);
    FData.uID := 1; //FData.Wnd;
    FData.uTimeoutVersion := 3000;
    FData.hIcon := FCurrentIcon.Handle;
    FData.uFlags := NIF_ICON or NIF_MESSAGE;
    FData.uCallbackMessage := WM_SYSTEM_TRAY_MESSAGE;
    WStrPLCopy(FData.szTip, Application.Title, SizeOf(FData.szTip) - 1);

    if Length(Application.Title) > 0 then
       FData.uFlags := FData.uFlags or NIF_TIP;

    Refresh;
  end;
end;

destructor TCustomKeymanTrayIcon.Destroy;
begin
  if not (csDesigning in ComponentState) then
    RefreshIcon(NIM_DELETE);

  FCurrentIcon.Free;
  FIcon.Free;
  FTimer.Free;
  Classes.DeallocateHWnd(FData.Wnd);
  inherited;
end;


procedure TCustomKeymanTrayIcon.StartRefreshIconTimeout;
begin
  // I1649 - Increase robustness of icon creation
  RetryCount := 0;
  RefreshIconTimeout;
end;

procedure TCustomKeymanTrayIcon.RefreshIconTimeout;
var
  timerid: Integer;
begin
  KL.Log('TrayIcon: RefreshIconTimeout');
  if not RefreshIcon(NIM_MODIFY) then
  begin
    // Ixxxx - Try modifying an existing icon first - and if that fails, then try adding it again
    if not RefreshIcon(NIM_ADD) then
    begin
      // I1148 - Wait a second and always retry.  Keep the last GetLastError message returned so that the user can have the knowledge
      FLastError := GetLastError;
      timerid := SetTimer(FData.Wnd, 1, 1000, nil);
      KL.Log('TrayIcon: RefreshIconTimeout, failed, setting Timer - %d [FLastError=%d]', [timerid, FLastError]);
    end
    else
    begin
      RetryCount := 0;
      KL.Log('TrayIcon: RefreshIconTimeout, succeeded in NIM_ADD');
    end
{
    case GetLastError of
      ERROR_SUCCESS,        // Unknown error?
      ERROR_FILE_NOT_FOUND: // taskbar window does not exist or is unresponsive, so retry after a second
        SetTimer(FData.Wnd, Message, 500, nil);
      else
        RaiseLastOSError;
    end;
}
  end
  else
  begin
    RetryCount := 0;
    KL.Log('TrayIcon: RefreshIconTimeout, succeeded in NIM_MODIFY');
  end;
end;
procedure TCustomKeymanTrayIcon.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    if (not FAnimate) or (FAnimate and FCurrentIcon.Empty) and not FVisible then
      SetDefaultIcon;

    FVisible := Value;    // I1649 - move setvisible after setdefaulticon so that setdefaulticon doesn't do an NIM_MODIFY directly before NIM_ADD

    if not (csDesigning in ComponentState) then
    begin
      if FVisible then
      begin
        StartRefreshIconTimeout;
        //raise EOutOfResources.Create(STrayIconCreateError);
      end
      else if not (csLoading in ComponentState) then
      begin
        RefreshIcon(NIM_DELETE);
        //raise EOutOfResources.Create(STrayIconRemoveError);
      end;
      if FAnimate then
        FTimer.Enabled := Value;
    end;
  end;
end;

procedure TCustomKeymanTrayIcon.SetIconList(Value: TImageList);
begin
  if FIconList <> Value then
  begin
    FIconList := Value;
    if not (csDesigning in ComponentState) then
    begin
      if Assigned(FIconList) then
        FIconList.GetIcon(FIconIndex, FCurrentIcon)
      else
        SetDefaultIcon;
      Refresh;
    end;
  end;
end;

procedure TCustomKeymanTrayIcon.SetHint(const Value: WideString);
begin
  if CompareStr(FHint, Value) <> 0 then
  begin
    FHint := Value;
    WStrPLCopy(FData.szTip, FHint, SizeOf(FData.szTip) - 1);

    if Length(Hint) > 0 then
      FData.uFlags := FData.uFlags or NIF_TIP
    else
      FData.uFlags := FData.uFlags and not NIF_TIP;

    Refresh;
  end;
end;

function TCustomKeymanTrayIcon.GetAnimateInterval: Cardinal;
begin
  Result := FTimer.Interval;
end;

procedure TCustomKeymanTrayIcon.SetAnimateInterval(Value: Cardinal);
begin
  FTimer.Interval := Value;
end;

procedure TCustomKeymanTrayIcon.SetAnimate(Value: Boolean);
begin
  if FAnimate <> Value then
  begin
    FAnimate := Value;
    if not (csDesigning in ComponentState) then
    begin
      if (FIconList <> nil) and (FIconList.Count > 0) and Visible then
        FTimer.Enabled := Value;
      if (not FAnimate) and (not FCurrentIcon.Empty) then
        FIcon.Assign(FCurrentIcon);
    end;
  end;
end;

{ Message handler for the hidden shell notification window. Most messages
  use WM_SYSTEM_TRAY_MESSAGE as the Message ID, with WParam as the ID of the
  shell notify icon data. LParam is a message ID for the actual message, e.g.,
  WM_MOUSEMOVE. Another important message is WM_ENDSESSION, telling the shell
  notify icon to delete itself, so Windows can shut down.

  Send the usual events for the mouse messages. Also interpolate the OnClick
  event when the user clicks the left button, and popup the menu, if there is
  one, for right click events. }
procedure TCustomKeymanTrayIcon.WindowProc(var Message: TMessage);

  { Return the state of the shift keys. }
  function ShiftState: TShiftState;
  begin
    Result := [];

    if GetKeyState(VK_SHIFT) < 0 then
      Include(Result, ssShift);
    if GetKeyState(VK_CONTROL) < 0 then
      Include(Result, ssCtrl);
    if GetKeyState(VK_MENU) < 0 then
      Include(Result, ssAlt);
  end;

var
  Point: TPoint;
  Shift: TShiftState;
  ShouldCancel: Boolean;
begin
  case Message.Msg of
    WM_QUERYENDSESSION:
      Message.Result := 1;

    WM_ENDSESSION:
    begin
      if TWmEndSession(Message).EndSession then
        RefreshIcon(NIM_DELETE);
    end;

    WM_TIMER:
    begin
      Inc(RetryCount);
      KL.Log('TrayIcon: Killing Timer - %s [retrycount=%d]', [BoolToStr(KillTimer(FData.Wnd, Message.WParam)), RetryCount]);
      ShouldCancel := False;
      if Assigned(FOnUnresponsive) then
        FOnUnresponsive(Self, RetryCount, ShouldCancel);
      if not ShouldCancel then RefreshIconTimeout; // I1141: It should retry again!
    end;

    WM_SYSTEM_TRAY_MESSAGE:
    begin
      Message.Result := 0;

      //if Assigned(FOnMessage) then
       // FOnMessage(Self, Message.LParam);

      case Message.lParam of
        WM_MOUSEMOVE:
        begin
          if Assigned(FOnMouseMove) then
          begin
            Shift := ShiftState;
            GetCursorPos(Point);
            FOnMouseMove(Self, Shift, Point.X, Point.Y);
          end;
        end;

        WM_LBUTTONDOWN:
        begin
          if Assigned(FOnMouseDown) then
          begin
            Shift := ShiftState + [ssLeft];
            GetCursorPos(Point);
            FOnMouseDown(Self, mbLeft, Shift, Point.X, Point.Y);
          end;

          FIsClicked := True;
        end;

        WM_LBUTTONUP:
        begin
          Shift := ShiftState + [ssLeft];
          GetCursorPos(Point);
          if FIsClicked and Assigned(FOnClick) then
          begin
            FOnClick(Self);
            FIsClicked := False;
          end;
          if Assigned(FOnMouseUp) then
            FOnMouseUp(Self, mbLeft, Shift, Point.X, Point.Y);
        end;

        WM_RBUTTONDOWN:
        begin
          if Assigned(FOnMouseDown) then
          begin
            Shift := ShiftState + [ssRight];
            GetCursorPos(Point);
            FOnMouseDown(Self, mbRight, Shift, Point.X, Point.Y);
          end;
        end;

        WM_RBUTTONUP:
        begin
          Shift := ShiftState + [ssRight];
          GetCursorPos(Point);
          if Assigned(FOnMouseUp) then
            FOnMouseUp(Self, mbRight, Shift, Point.X, Point.Y);
          if Assigned(FPopupMenu) then
          begin
            SetForegroundWindow(Application.Handle);
            Application.ProcessMessages;
            FPopupMenu.AutoPopup := False;
            FPopupMenu.PopupComponent := Owner;
            FPopupMenu.Popup(Point.x, Point.y);
          end;
        end;

        WM_LBUTTONDBLCLK, WM_MBUTTONDBLCLK, WM_RBUTTONDBLCLK:
          if Assigned(FOnDblClick) then
            FOnDblClick(Self);

        WM_MBUTTONDOWN:
        begin
          if Assigned(FOnMouseDown) then
          begin
            Shift := ShiftState + [ssMiddle];
            GetCursorPos(Point);
            FOnMouseDown(Self, mbMiddle, Shift, Point.X, Point.Y);
          end;
        end;

        WM_MBUTTONUP:
        begin
          if Assigned(FOnMouseUp) then
          begin
            Shift := ShiftState + [ssMiddle];
            GetCursorPos(Point);
            FOnMouseUp(Self, mbMiddle, Shift, Point.X, Point.Y);
          end;
        end;

        NIN_BALLOONUSERCLICK:
        begin
          if Assigned(FOnBalloonClick) then
            FOnBalloonClick(Self);
        end;

        NIN_BALLOONSHOW:
        begin
          if Assigned(FOnBalloonShow) then
            FOnBalloonShow(Self);
        end;

        NIN_BALLOONHIDE:
        begin
          if Assigned(FOnBalloonHide) then
            FOnBalloonHide(Self);
        end;

        NIN_BALLOONTIMEOUT:
        begin
          if Assigned(FOnBalloonTimeout) then
            FOnBalloonTimeout(Self);
        end;
      end;
    end;

    //else if (Message.Msg = RM_TaskBarCreated) and Visible then
    //  RefreshIcon(NIM_ADD);
  end;
end;

procedure TCustomKeymanTrayIcon.Refresh;
begin
  if not (csDesigning in ComponentState) then
  begin
    FData.hIcon := FCurrentIcon.Handle;

    if Visible then
      if not RefreshIcon(NIM_MODIFY) then
        StartRefreshIconTimeout;
  end;
end;

function TCustomKeymanTrayIcon.RefreshIcon(Message: Integer): Boolean;
var
  uVersion: UINT;
  f: Boolean;
begin
  Result := Shell_NotifyIconW(Message, @FData);

  KL.Log('TrayIcon: RefreshIcon(%d; FData={cbSize:%d, Wnd:%d, uID:%d, uFlags: %d, uCallbackMessage: %d, hIcon: %d, szTip: %s, dwState: %d, dwStateMask: %d, szInfo: %s, uTimeoutVersion: %d, szInfoTitle: %s, dwInfoFlags: %d) = %s [GLE:%d]',
    [
    Message,
    FData.cbSize,
    FData.Wnd,
    FData.uID,
    FData.uFlags,
    FData.uCallbackMessage,
    FData.hIcon,
    FData.szTip,
    FData.dwState,
    FData.dwStateMask,
    FData.szInfo,
    FData.uTimeoutVersion,
    FData.szInfoTitle,
    FData.dwInfoFlags,
    BoolToStr(Result),
    GetLastError
    ]);

  if Result and (Message = NIM_ADD) then
  begin
    uVersion := FData.uTimeoutVersion;
    FData.uTimeoutVersion := NOTIFYICON_VERSION;
    f := Shell_NotifyIconW(NIM_SETVERSION, @FData);
    KL.Log('TrayIcon: RefreshIcon(NIM_SETVERSION) = %s [%d]', [BoolToStr(f), GetLastError]);
    FData.uTimeoutVersion := uVersion;
  end;
end;

procedure TCustomKeymanTrayIcon.SetIconIndex(Value: Integer);
begin
  if FIconIndex <> Value then
  begin
    FIconIndex := Value;
    if not (csDesigning in ComponentState) then
    begin
      if Assigned(FIconList) then
        FIconList.GetIcon(FIconIndex, FCurrentIcon);
      Refresh;
    end;
  end;
end;

procedure TCustomKeymanTrayIcon.DoOnAnimate(Sender: TObject);
begin
  if Assigned(FOnAnimate) then
    FOnAnimate(Self);
  if Assigned(FIconList) and (FIconIndex < FIconList.Count - 1) then
    IconIndex := FIconIndex + 1
  else
    IconIndex := 0;
  Refresh;
end;

procedure TCustomKeymanTrayIcon.SetIcon(Value: TIcon);
begin
  FIcon.Assign(Value);
  FCurrentIcon.Assign(Value);
  Refresh;
end;

procedure TCustomKeymanTrayIcon.SetBalloonHint(const Value: WideString);
begin
  if CompareStr(FBalloonHint, Value) <> 0 then
  begin
    FBalloonHint := Value;
    WStrPLCopy(FData.szInfo, FBalloonHint, SizeOf(FData.szInfo) - 1);
    //RefreshIcon(NIM_MODIFY);
  end;
end;

procedure TCustomKeymanTrayIcon.SetBalloonIcon(const Value: THandle);
begin
  {if FBalloonIcon <> Value then
  begin
    FBalloonIcon := Value;
    FData.hBalloonIcon := FBalloonIcon;
  end;}
end;

procedure TCustomKeymanTrayIcon.SetDefaultIcon;
begin
(* Changing the tray icon to Application Icon always. https://github.com/keymanapp/keyman/issues/859
  if not FIcon.Empty then
    FCurrentIcon.Assign(FIcon)
  else
    FCurrentIcon.Assign(Application.Icon);
  *)
  FCurrentIcon.Assign(Application.Icon);
  Refresh;
end;

procedure TCustomKeymanTrayIcon.SetBalloonTimeout(Value: Integer);
begin
  FData.uTimeoutVersion := Value;
end;

function TCustomKeymanTrayIcon.GetBalloonTimeout: Integer;
begin
  Result := FData.uTimeoutVersion;
end;

function TCustomKeymanTrayIcon.GetNotificationWindow: THandle;
begin
  Result := FData.Wnd;
end;

procedure TCustomKeymanTrayIcon.HideBalloonHint;
begin
  RefreshIcon(NIM_MODIFY);
end;

procedure TCustomKeymanTrayIcon.IconChange(Sender: TObject);
begin
(* Changing the tray icon to Application Icon always. https://github.com/keymanapp/keyman/issues/859
  FCurrentIcon.Assign(FIcon);
*)
  FCurrentIcon.Assign(Application.Icon);
  Refresh;
end;

procedure TCustomKeymanTrayIcon.ShowBalloonHint; //(const Title, Text: WideString; Timeout: Integer);
var
  uFlags: UINT;
begin
  uFlags := FData.uFlags;
  FData.uFlags := NIF_INFO;
  FData.dwInfoFlags := Ord(FBalloonFlags);

  RefreshIcon(NIM_MODIFY);

  FData.uFlags := uFlags;
end;

procedure TCustomKeymanTrayIcon.SetBalloonTitle(const Value: WideString);
begin
  if CompareStr(FBalloonTitle, Value) <> 0 then
  begin
    FBalloonTitle := Value;
    WStrPLCopy(FData.szInfoTitle, FBalloonTitle, SizeOf(FData.szInfoTitle) - 1);
    //RefreshIcon(NIM_MODIFY);
  end;
end;

initialization
end.
