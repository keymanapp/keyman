{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSIMDView.pas, released on 2004-10-11.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvSIMDView;

{$I jedi.inc}
                     
interface

uses
  Windows, Classes, Menus, ActnList, ToolsAPI, SysUtils, Graphics, Dialogs,
  Forms, ComCtrls, JclSysInfo, JvSIMDViewForm;

type
  TProcessReference = record
    Process:IOTAProcess;
    ID:Integer;
  end;
  PProcessReference = ^TProcessReference;

  TThreadReference = record
    Thread:IOTAThread;
    ID:Integer;
  end;
  PThreadReference = ^TThreadReference;

  TDebuggerNotifier = class;

  TIDESSEWizard = class (TNotifierObject,IOTAWizard)
  private
    FServices:IOTAServices;
    FDebuggerServices:IOTADebuggerServices;
    FNTAServices:INTAServices;
    FIndex:Integer;
    FDebuggerNotifier:TDebuggerNotifier;
    FSSEAction:TAction;
    FIcon:TIcon;
    FSSEMenuItem:TMenuItem;
    FViewDebugMenu:TMenuItem;
    FForm:TJvSIMDViewFrm;
    FCpuInfo: TCpuInfo;
    FCpuInfoValid: Boolean;
    procedure CheckToolBarButton(AToolBar:TToolBar);
  protected
    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
  public
    constructor Create;
    destructor Destroy; override;
    function CpuInfo: TCpuInfo;
    function GetSSEString: string;
    procedure ActionExecute(Sender:TObject);
    procedure ActionUpdate(Sender:TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure Refresh;
    procedure ThreadEvaluate(const ExprStr, ResultStr: string;
      ReturnCode: Integer);
    procedure Close;
  end;

  TDebuggerNotifier = class (TNotifierObject,IOTADebuggerNotifier,
                             IOTAProcessNotifier, IOTAThreadNotifier)
  private
    FOwner:TIDESSEWizard;
    FProcessList:TList;
    FThreadList:TList;
    function FindProcessReference (AProcess:IOTAProcess):PProcessReference;
    function FindThreadReference (AThread:IOTAThread):PThreadReference;
  public
    // IOTADebuggerNotifier
    procedure ProcessCreated(Process: IOTAProcess);
    procedure ProcessDestroyed(Process: IOTAProcess);
    procedure BreakpointAdded(Breakpoint: IOTABreakpoint);
    procedure BreakpointDeleted(Breakpoint: IOTABreakpoint);
    // IOTAProcessNotifier
    procedure ThreadCreated(Thread: IOTAThread);
    procedure ThreadDestroyed(Thread: IOTAThread);
    procedure ProcessModuleCreated(ProcessModule: IOTAProcessModule);
    procedure ProcessModuleDestroyed(ProcessModule: IOTAProcessModule);
    // IOTAThreadNotifier
    procedure ThreadNotify(Reason: TOTANotifyReason);
    procedure EvaluteComplete(const ExprStr, ResultStr: string;
      CanModify: Boolean; ResultAddress, ResultSize: LongWord;
      ReturnCode: Integer);
    procedure ModifyComplete(const ExprStr, ResultStr: string;
      ReturnCode: Integer);
    constructor Create(AOwner:TIDESSEWizard); reintroduce;
    destructor Destroy; override;
    property Owner:TIDESSEWizard read FOwner;
  end;

procedure Register;

implementation

uses
{$IFDEF UNITVERSIONING}
  JclUnitVersioning,
{$ENDIF UNITVERSIONING}
  JvSIMDUtils;

procedure Register;
begin
  RegisterPackageWizard(TIDESSEWizard.Create);
end;

{ TIDESSEWizard }

procedure TIDESSEWizard.ActionExecute(Sender: TObject);
begin
  if CpuInfo.SSE = 0 then
  begin
    MessageDlg(RsNoSSE,mtError,[mbAbort],0);
    Exit;
  end;
  if not Assigned(FForm) then
  begin
    FForm:=TJvSIMDViewFrm.Create(Application,FDebuggerServices);
    FForm.Icon:=FIcon;
    FForm.OnDestroy:=FormDestroy;
    FForm.SSECaption := GetSSEString;
  end;
  FForm.Show;
end;

procedure TIDESSEWizard.ActionUpdate(Sender: TObject);
var
  AProcess:IOTAProcess;
  AThread:IOTAThread;
begin
  if CpuInfo.SSE = 0 then
      FSSEAction.Enabled:=False
  else begin
    AThread := nil;
    AProcess := nil;
    if (FDebuggerServices.ProcessCount > 0) then
      AProcess:=FDebuggerServices.CurrentProcess;
    if (AProcess<>nil) and (AProcess.ThreadCount > 0) then
      AThread:=AProcess.CurrentThread;
    if (AThread<>nil) then
      FSSEAction.Enabled:=AThread.State in [tsStopped, tsBlocked]
    else FSSEAction.Enabled:=False;
  end;
end;

type
  THackToolButton = class (TToolButton);

procedure TIDESSEWizard.CheckToolBarButton(AToolBar: TToolBar);
var
  Index:Integer;
  AButton:THackToolButton;
begin
  if Assigned(AToolBar) then
  begin
    for Index:=AToolBar.ButtonCount-1 downto 0 do
    begin
      AButton:=THackToolButton(AToolBar.Buttons[Index]);
      if (AButton.Action=FSSEAction) then
      begin
        AButton.SetToolBar(nil);
        AButton.Free;
      end;
    end;
  end;
end;

procedure TIDESSEWizard.Close;
begin
  if Assigned(FForm)
    then FForm.Close;
end;

function TIDESSEWizard.CpuInfo: TCpuInfo;
begin
  if not FCpuInfoValid then
  begin
    GetCpuInfo(FCpuInfo);
    FCpuInfoValid := True;
  end;
  Result := FCpuInfo;
end;

constructor TIDESSEWizard.Create;
var
  I:Integer;
  IDEMenu:TMenu;
  ViewMenu:TMenuItem;
  Category:string;
begin
  inherited Create;

  FCpuInfoValid := False;

  FForm:=nil;
  Assert(Supports(BorlandIDEServices,IOTAServices,FServices),
    'Unable to get Borland IDE Services');
  Assert(Supports(FServices,INTAServices,FNTAServices),
    'Unable to get Borland NTA Services');
  Assert(Supports(FServices,IOTADebuggerServices,FDebuggerServices),
    'Unable to get Borland Debugger Services');

  Category:='';
  with FNTAServices do
    for I:=0 to ActionList.ActionCount-1 do
      if (CompareText(ActionList.Actions[I].Name,'DebugCPUCommand')=0)
        then Category:=ActionList.Actions[I].Category;

  FIcon := TIcon.Create;
  FIcon.Handle:=LoadIcon(FindResourceHInstance(HInstance),'SIMDICON');

  FSSEAction:=TAction.Create(nil);
  FSSEAction.Caption := RsSSE;
  FSSEAction.Visible:=True;
  FSSEAction.OnExecute:=ActionExecute;
  FSSEAction.OnUpdate:=ActionUpdate;
  FSSEAction.ActionList:=FNTAServices.ActionList;
  FSSEAction.Category:=Category;
  FSSEAction.Name:='DebugSSECommand';
  FSSEAction.ImageIndex := FNTAServices.ImageList.AddIcon(FIcon);

  FSSEMenuItem := TMenuItem.Create(nil);
  FSSEMenuItem.Action := FSSEAction;
  FSSEMenuItem.ShortCut := Shortcut(Ord('D'),[ssCtrl,ssAlt]);

  IDEMenu:=FNTAServices.MainMenu;

  ViewMenu:=nil;
  for I:=0 to IDEMenu.Items.Count-1 do
    if (CompareText(IDEMenu.Items[I].Name,'ViewsMenu')=0)
      then ViewMenu:=IDEMenu.Items[I];
  Assert(ViewMenu<>nil,'Unable to localize View menu');

  FViewDebugMenu:=nil;
  for I:=0 to ViewMenu.Count-1 do
    if (CompareText(ViewMenu.Items[I].Name,'ViewDebugItem')=0)
      then FViewDebugMenu:=ViewMenu.Items[I];
  Assert(FViewDebugMenu<>nil,'Unable to localize View Debug menu');

  FViewDebugMenu.Add(FSSEMenuItem);

  FDebuggerNotifier:=TDebuggerNotifier.Create(Self);
  FIndex:=FDebuggerServices.AddNotifier(FDebuggerNotifier);
end;

destructor TIDESSEWizard.Destroy;
begin
  FDebuggerServices.RemoveNotifier(FIndex);

  CheckToolBarButton(FNTAServices.ToolBar[sCustomToolBar]);
  CheckToolBarButton(FNTAServices.ToolBar[sStandardToolBar]);
  CheckToolBarButton(FNTAServices.ToolBar[sDebugToolBar]);
  CheckToolBarButton(FNTAServices.ToolBar[sViewToolBar]);
  CheckToolBarButton(FNTAServices.ToolBar[sDesktopToolBar]);
{$IFDEF COMPILER7_UP}
  CheckToolBarButton(FNTAServices.ToolBar[sInternetToolBar]);
  CheckToolBarButton(FNTAServices.ToolBar[sCORBAToolBar]);
{$ENDIF}
  FViewDebugMenu.Remove(FSSEMenuItem);
  FSSEAction.ActionList:=nil;

  FreeAndNil(FIcon);
  FreeAndNil(FSSEMenuItem);
  FreeAndNil(FSSEAction);
  //FreeAndNil(FDebuggerNotifier);   // Buggy !!!!
  FreeAndNil(FForm);

  FServices:=nil;
  FDebuggerServices:=nil;
  FNTAServices:=nil;

  inherited Destroy;
end;

procedure TIDESSEWizard.Execute;
begin

end;

procedure TIDESSEWizard.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caFree;
end;

procedure TIDESSEWizard.FormDestroy(Sender: TObject);
begin
  FForm:=nil;
end;

function TIDESSEWizard.GetIDString: string;
begin
  Result:='jvcl.'+ClassName;
end;

function TIDESSEWizard.GetName: string;
begin
  Result:=ClassName;
end;

function TIDESSEWizard.GetSSEString: string;
begin
  Result := '';
  with CpuInfo do
  begin
    Result := '';
    case SSE of
      0  : Result := RsNoSSE;
      1  : Result := RsSSE1;
      2  : Result := RsSSE1 + ',' + RsSSE2;
      3  : Result := RsSSE1 + ',' + RsSSE2 + ',' + RsSSE3;
      else Result := RsSSE+IntToStr(SSE);
    end;
    if Is64Bits then
      Result := Result + ',' + RsLong;
  end;
end;

function TIDESSEWizard.GetState: TWizardState;
begin
  Result:=[];
end;

procedure TIDESSEWizard.Refresh;
begin
  if Assigned(FForm)
    then FForm.GetThreadValues;
end;

procedure TIDESSEWizard.ThreadEvaluate(const ExprStr,
  ResultStr: string; ReturnCode: Integer);
begin
  if Assigned(FForm)
    then FForm.ThreadEvaluate(ExprStr,ResultStr,ReturnCode);
end;

{ TDebuggerNotifier }

procedure TDebuggerNotifier.BreakpointAdded(Breakpoint: IOTABreakpoint);
begin

end;

procedure TDebuggerNotifier.BreakpointDeleted(Breakpoint: IOTABreakpoint);
begin

end;

constructor TDebuggerNotifier.Create(AOwner: TIDESSEWizard);
begin
  inherited Create;
  FOwner:=AOwner;
  FProcessList:=TList.Create;
  FThreadList:=TList.Create;
end;

destructor TDebuggerNotifier.Destroy;
var
  AThreadReference:PThreadReference;
  AProcessReference:PProcessReference;
begin
  while (FThreadList.Count>0) do
  begin
    AThreadReference:=PThreadReference(FThreadList.Items[0]);
    AThreadReference.Thread.RemoveNotifier(AThreadReference.ID);
    FThreadList.Remove(AThreadReference);
    Dispose(AThreadReference);
  end;
  while (FProcessList.Count>0) do
  begin
    AProcessReference:=PProcessReference(FProcessList.Items[0]);
    AProcessReference.Process.RemoveNotifier(AProcessReference.ID);
    FProcessList.Remove(AProcessReference);
    Dispose(AProcessReference);
  end;
  FThreadList.Free;
  FProcessList.Free;
  inherited Destroy;
end;

procedure TDebuggerNotifier.EvaluteComplete(const ExprStr,
  ResultStr: string; CanModify: Boolean; ResultAddress,
  ResultSize: LongWord; ReturnCode: Integer);
begin
  Owner.ThreadEvaluate(ExprStr,ResultStr,ReturnCode);
end;

function TDebuggerNotifier.FindProcessReference(
  AProcess: IOTAProcess): PProcessReference;
var
  Index:Integer;
begin
  for Index:=0 to FProcessList.Count-1 do
  begin
    Result:=PProcessReference(FProcessList.Items[Index]);
    if (Result.Process=AProcess)
      then Exit;
  end;
  Result:=nil;
end;

function TDebuggerNotifier.FindThreadReference(
  AThread: IOTAThread): PThreadReference;
var
  Index:Integer;
begin
  for Index:=0 to FThreadList.Count-1 do
  begin
    Result:=PThreadReference(FThreadList.Items[Index]);
    if (Result.Thread=AThread)
      then Exit;
  end;
  Result:=nil;
end;

procedure TDebuggerNotifier.ModifyComplete(const ExprStr,
  ResultStr: string; ReturnCode: Integer);
begin

end;

procedure TDebuggerNotifier.ProcessCreated(Process: IOTAProcess);
var
  AProcessReference:PProcessReference;
begin
  AProcessReference:=FindProcessReference(Process);
  if (AProcessReference=nil) then
  begin
    New(AProcessReference);
    AProcessReference.Process:=Process;
    AProcessReference.ID:=Process.AddNotifier(Self);
    FProcessList.Add(AProcessReference);
  end;
end;

procedure TDebuggerNotifier.ProcessDestroyed(Process: IOTAProcess);
var
  AProcessReference:PProcessReference;
  AThreadReference:PThreadReference;
  Index:Integer;
begin
  for Index:=0 to Process.ThreadCount-1 do
  begin
    AThreadReference:=FindThreadReference(Process.Threads[Index]);
    if (AThreadReference<>nil) then
    begin
      AThreadReference.Thread.RemoveNotifier(AThreadReference.ID);
      FThreadList.Remove(AThreadReference);
      Dispose(AThreadReference);
    end;
  end;

  AProcessReference:=FindProcessReference(Process);
  if (AProcessReference<>nil) then
  begin
    AProcessReference.Process.RemoveNotifier(AProcessReference.ID);
    FProcessList.Remove(AProcessReference);
    Dispose(AProcessReference);
  end;

  if (Owner.FDebuggerServices.ProcessCount=1)
    then Owner.Close;
end;

procedure TDebuggerNotifier.ProcessModuleCreated(
  ProcessModule: IOTAProcessModule);
begin

end;

procedure TDebuggerNotifier.ProcessModuleDestroyed(
  ProcessModule: IOTAProcessModule);
begin

end;

procedure TDebuggerNotifier.ThreadCreated(Thread: IOTAThread);
var
  AThreadReference:PThreadReference;
begin
  AThreadReference:=FindThreadReference(Thread);
  if (AThreadReference=nil) then
  begin
    New(AThreadReference);
    AThreadReference.Thread:=Thread;
    AThreadReference.ID:=Thread.AddNotifier(Self);
    FThreadList.Add(AThreadReference);
  end;
end;

procedure TDebuggerNotifier.ThreadDestroyed(Thread: IOTAThread);
var
  AThreadReference:PThreadReference;
begin
  AThreadReference:=FindThreadReference(Thread);
  if (AThreadReference<>nil) then
  begin
    AThreadReference.Thread.RemoveNotifier(AThreadReference.ID);
    FThreadList.Remove(AThreadReference);
    Dispose(AThreadReference);
  end;
end;

procedure TDebuggerNotifier.ThreadNotify(Reason: TOTANotifyReason);
begin
  if (Reason <> nrRunning) then
    Owner.Refresh;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
