unit Keyman.Developer.System.Project.Project;

interface

uses
  System.Classes,
  Winapi.Messages,

  Keyman.Developer.System.Project.ProjectFile;

var
  FGlobalProject: TProject = nil;
  FGlobalProjectRefresh: TNotifyEvent = nil;
  FGlobalProjectRefreshCaption: TNotifyEvent = nil;

const
  WM_USER_ProjectUpdateDisplayState = WM_USER;

function GlobalProjectStateWndHandle: THandle;

implementation

uses
  System.SysUtils,
  Winapi.Windows;

type
  TGlobalProjectStateWnd = class
  private
    procedure WndProc(var Message: TMessage);
    constructor Create;
    destructor Destroy; override;
  end;

var
  FGlobalProjectStateWnd: TGlobalProjectStateWnd = nil;

  // Make this a global to prevent potential race
  // condition causing an access violation. If it
  // is an invalid window handle or 0 at destruction time,
  // it's no big deal...
  FGlobalProjectStateWndHandle: THandle = 0;

{ TGlobalProjectStateWnd }

constructor TGlobalProjectStateWnd.Create;
begin
  inherited Create;
  FGlobalProjectStateWndHandle := AllocateHWnd(WndProc);
end;

destructor TGlobalProjectStateWnd.Destroy;
var
  h: THandle;
begin
  h := FGlobalProjectStateWndHandle;
  FGlobalProjectStateWndHandle := 0;
  DeallocateHWnd(h);
  inherited Destroy;
end;

procedure TGlobalProjectStateWnd.WndProc(var Message: TMessage);
var
  PPath, PDisplayState: PChar;
begin
  if Message.Msg = WM_USER_ProjectUpdateDisplayState then
  begin
    PPath := PChar(Message.WParam);
    PDisplayState := PChar(Message.LParam);
    if Assigned(FGlobalProject) and (FGlobalProject.FileName = PPath) then
    begin
      FGlobalProject.DisplayState := PDisplayState;
      FGlobalProject.SaveUser;
    end;
    StrDispose(PDisplayState);
    StrDispose(PPath);
  end;
  DefWindowProc(FGlobalProjectStateWndHandle, Message.Msg, Message.WParam, Message.LParam);
end;

function GlobalProjectStateWndHandle: THandle;
begin
  Result := FGlobalProjectStateWndHandle;
end;

initialization
  FGlobalProjectStateWnd := TGlobalProjectStateWnd.Create;
finalization
  // Deletes temporary session-local project
  FGlobalProjectStateWnd.Free;
end.
