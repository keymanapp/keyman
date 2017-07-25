(*
  Name:             int_kmdebug
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Aug 2006

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2006 - mcdurdin - Use TApplicationEvents instead of AppMessageHandler
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
*)
unit int_kmdebug;

interface

uses Windows, Messages, SysUtils, Classes, Controls, Forms, AppEvnts;

const
  MAXDEBUGMSG = 2048;

  KDS_KEYBOARD = $0001;
  KDS_PROGRAM  = $0002;
  KDS_MESSAGE  = $0004;
  KDS_INTERNAT = $0008;

  KDM_ADDDEBUGWINDOW = 1;
  KDM_REMOVEDEBUGWINDOW = 2;
  KDM_IDENTIFYYOURSELF = 3;
  KDM_DEBUG = 4;

  DM_ADDDEBUGWINDOW    = 1;
  DM_REMOVEDEBUGWINDOW = 2;

type
  PDebugInfo = ^TDebugInfo;
  TDebugInfo = record
    version: DWord;
    handle: HWND;
    state, kmn_lineno: Integer;
    msg: array[0..MAXDEBUGMSG-1] of ansichar;  // I3310
  end;

  TDebugMessageEvent = procedure(Sender: TObject; hwnd, State, LineNo: Integer; const msg: string) of object;

type
  TDebugKeyboard = class(TWinControl)
  private
    wm_keymandebug: DWord;
    FDebugging: Boolean;
    hDebugFile: THandle;
    DebugInfo: PDebugInfo;
    FDebugWindow: HWND;
    FOnDebugMessage: TDebugMessageEvent;
    FDebugFlags: Integer;
    FAppEvents: TApplicationEvents;
    procedure AppOnMessage(var Msg: TMsg; var Handled: Boolean);
    procedure AddDebugInfo;
    procedure SetDebugFlags(const Value: Integer);
  public
    procedure WndProc(var Message: TMessage); override;
    function StartDebugging: Boolean;
    function StopDebugging: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DebugFlags: Integer read FDebugFlags write SetDebugFlags;
    property DebugWindow: HWND read FDebugWindow write FDebugWindow;
    property OnDebugMessage: TDebugMessageEvent read FOnDebugMessage write FOnDebugMessage;
  end;

implementation

uses
  Unicode;

function TDebugKeyboard.StartDebugging: Boolean;
begin
  hDebugFile := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0,
    SizeOf(TDebugInfo), 'Keyman.Debug');
  DebugInfo := PDebugInfo(MapViewOfFile(hDebugFile, FILE_MAP_WRITE, 0, 0, 0));

  wm_keymandebug := RegisterWindowMessage('WM_KEYMANDEBUG');
  PostMessage(HWND_BROADCAST, wm_keymandebug, MAKELONG(KDM_ADDDEBUGWINDOW, FDebugFlags), Handle);

  FDebugging := True;
  FAppEvents.OnMessage := AppOnMessage;
  Result := True;
end;

function TDebugKeyboard.StopDebugging: Boolean;
begin
  PostMessage(HWND_BROADCAST, wm_keymandebug, MAKELONG(KDM_REMOVEDEBUGWINDOW, 0), Handle);
  FAppEvents.OnMessage := nil;
  FDebugging := False;
  Result := True;
end;

procedure TDebugKeyboard.AppOnMessage(var Msg: TMsg; var Handled: Boolean);
begin
    if (Msg.message = wm_keymandebug) and (Msg.wParam = KDM_IDENTIFYYOURSELF) then
    begin
      Handled := True;
      PostMessage(HWND_BROADCAST, wm_keymandebug, MAKELONG(KDM_ADDDEBUGWINDOW, FDebugFlags), Handle);
      FAppEvents.CancelDispatch;
    end
    else
      Handled := False;
end;

constructor TDebugKeyboard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAppEvents := TApplicationEvents.Create(Self);
  FDebugFlags := KDS_KEYBOARD;
end;

destructor TDebugKeyboard.Destroy;
begin
  if FDebugging then StopDebugging;
  inherited Destroy;
end;

procedure TDebugKeyboard.WndProc(var Message: TMessage);
begin
  if (Message.Msg = wm_keymandebug) and (wm_keymandebug <> 0) and (Message.wParam = KDM_DEBUG) then
    AddDebugInfo
  else
    inherited WndProc(Message);
end;

procedure TDebugKeyboard.AddDebugInfo;
begin
  //if (DebugInfo.Handle <> 0) and (DebugInfo.Handle <> FDebugWindow) and (FDebugWindow <> 0) then Exit;

  if Assigned(FOnDebugMessage) then
    FOnDebugMessage(Self, DebugInfo.handle, DebugInfo.State, DebugInfo.kmn_lineno, String_AtoU(DebugInfo.msg));  // I3310
end;

procedure TDebugKeyboard.SetDebugFlags(const Value: Integer);
begin
  FDebugFlags := Value;
  if FDebugging then
  begin
    StopDebugging;
    StartDebugging;
  end;
end;

end.
