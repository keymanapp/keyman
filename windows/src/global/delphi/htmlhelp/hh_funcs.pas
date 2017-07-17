{*******************************************************}
{                                                       }
{       HTML Help helper functin                        }
{                                                       }
{       Copyright (c) 1999  The Helpware Group          }
{                                                       }
{*******************************************************}

{
  Result examples
    HHFormat('windows.chm', 'about_magnify.htm', 'windefault', ptIE3);
    => 'mk:@MSITStore:windows.chm::/about_magnify.htm>windefault'

    chmFile.chm
    chmFile.chm>WinDef
    Helpfile.chm::/Topic.htm>WinDef
    ms-its:chmFile.chm>WinDef
    mk:@MSITStore:Helpfile.chm::/Topic.htm>WinDef

}

unit hh_funcs;

interface

uses Windows, SysUtils, Classes, Forms, Dialogs;

type
  THelpProcCallback1 = procedure (Data: Longint);
  THelpProcCallback2 = procedure (Data: Longint; X, Y: Integer);

type
  THookHelpSystem = class(TObject)
  private
    FInitHelp: Boolean;
    FhhInstalled: Boolean;

    FOldHelpEvent: THelpEvent;
    FChmFile: String;
    FPopupXY: TPoint;

    function HelpHook(Command : Word; Data : Longint; Var CallHelp : Boolean) : Boolean;

    procedure InitHelp;

  public
    HelpCallback1: THelpProcCallback1;
    HelpCallback2: THelpProcCallback2;

    constructor Create(aDefChmFile: String);
    destructor Destroy; override;

    function HelpContext(aContextId: DWord): Boolean;
    function HelpTopic(aTopic: String): Boolean;

    property ChmFile: String read FChmFile write FChmFile;
  end;

implementation

uses
  Types,
  hh;  //HH API

constructor THookHelpSystem.Create(aDefChmFile: string);
begin
  inherited Create;

  FhhInstalled := hh.GetPathToHHCtrlOCX <> '';
  
  FChmFile := aDefChmFile;

  FOldHelpEvent := Application.OnHelp;
  Application.OnHelp := HelpHook;
end;

destructor THookHelpSystem.Destroy;
begin
  if FInitHelp then
  begin
    HH.HtmlHelp(0, nil, HH_CLOSE_ALL, 0);
    UnloadHTMLHelp;
  end;

  Application.OnHelp := FOldHelpEvent;
  inherited destroy;
end;

function THookHelpSystem.HelpHook(Command: Word; Data: Longint; Var CallHelp: Boolean) : Boolean;
begin
   CallHelp := false;
   case Command of
    Help_Context:      //help button
      begin
        if Assigned(HelpCallback1)
          then HelpCallback1(Data)           //Call back
          else Self.HelpContext( Data );     //Call help
      end;
    HELP_SETPOPUP_POS: //call #1 of F1 Popup (Whats This) help
      FPopupXY := SmallPointToPoint(TSmallPoint(Data));           //data = x,y pos for popup
    Help_ContextPopup: //call #2 of F1 Popup (Whats This) help
      begin
        if Assigned(HelpCallback2)
          then HelpCallback2(Data, FPopupXY.X, FPopupXY.Y)   //Call back
          else Self.HelpContext(Data);                       //Call help
      end
    else
      CallHelp := TRUE; //Default handling - WinHelp
  end;
  result := TRUE;
end;

function THookHelpSystem.HelpContext(aContextId: DWord): Boolean;
var
  h: HWND;
begin
  Result := False;
  InitHelp;
  if FhhInstalled then
  begin
    h := HH.HtmlHelp(GetDesktopWindow, PChar(FChmFile), HH_HELP_CONTEXT, aContextID);
    if h > 0 then SetForegroundWindow(h);
    Result := True;
  end;
end;

function THookHelpSystem.HelpTopic(aTopic: String): Boolean;
var
  h: HWND;
  i: Integer;
  s: string;
begin
  Result := False;

  if FhhInstalled then
  begin
    InitHelp;

    if aTopic <> '' then
    begin
      for i := 1 to Length(aTopic) do if aTopic[i] = '\' then aTopic[i] := '/';

      if aTopic[1] <> '/' then aTopic := '/' + aTopic;
      if FChmFile <> '' then s := FChmFile + '::' + aTopic
      else s := aTopic;
    end
    else
      s := FChmFile;

    h := HH.HtmlHelp(GetDesktopWindow, PChar(s), HH_DISPLAY_TOPIC, 0);
    if h > 0 then SetForegroundWindow(h);
    Result := True;
  end;
end;

procedure THookHelpSystem.InitHelp;
begin
  if FInitHelp then Exit;
  LoadHtmlHelp;
end;

end.

