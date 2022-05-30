(*
  Name:             DelphiProjectManagerWizard
  Copyright:        Copyright SIL International.
  Documentation:    
  Description:      
  Create Date:      25 May 2012

  Modified Date:    25 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          25 May 2012 - mcdurdin - I3339 - V9.0 - Add GUI compiler wrapper for quicker review of hints and warnings
*)
unit DelphiProjectManagerWizard;  // I3339

interface

uses
  Classes, SysUtils, ToolsApi, Controls,
  Windows,
  Messages;

type
  TDelphiProjectManager = class(TNotifierObject, IOTANotifier, IOTAWizard)
  private
    hProjectCallbackWindow: HWND;
    FOpenFile: string;
    procedure DelphiProjectCallback(var Message: TMessage);
    procedure ProcessCopyData(var Message: TWMCopyData);
    procedure ProcessUserMessage(var Message: TMessage);
  public
    constructor Create;
    destructor Destroy; override;
  protected
    { IOTANotifier }
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    { IOTAWizard }
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
  end;

procedure Register;

implementation

uses
  System.Win.Registry;

//
// Extracted from RegistryKeys.pas
// Don't include in this project because
// it conflicts with KeymanComponents
// and this is not worth sorting out at this
// time.
//
const
  SRegKey_Software            = 'Software';                                         // LM CU
  SRegKey_SoftwareKeyman = SRegKey_Software + '\Keyman';
  SRegKey_DelphiProjectManager = SRegKey_SoftwareKeyman + '\DelphiProjectManager';
  SRegValue_CallbackWindow = 'CallbackWindow';

{ TDelphiProjectManager }

constructor TDelphiProjectManager.Create;
begin
  inherited;
  hProjectCallbackWindow := AllocateHwnd(DelphiProjectCallback);
  with TRegistry.Create do
  try
    OpenKey(SRegKey_DelphiProjectManager, True);
    WriteInteger(SRegValue_CallbackWindow, hProjectCallbackWindow);
  finally
    Free;
  end;
end;

destructor TDelphiProjectManager.Destroy;
begin
  with TRegistry.Create do
  try
    OpenKey(SRegKey_DelphiProjectManager, True);
    if ValueExists(SRegValue_CallbackWindow) then DeleteValue(SRegValue_CallbackWindow);
  finally
    Free;
  end;
  DeallocateHwnd(hProjectCallbackWindow);
  inherited Destroy;
end;

procedure TDelphiProjectManager.DelphiProjectCallback(var Message: TMessage);
begin
  with Message do
    case Msg of
      WM_COPYDATA:
        ProcessCopyData(TWMCopyData(Message));
      WM_USER:
        ProcessUserMessage(Message);
      else Result := DefWindowProc(hProjectCallbackWindow, Msg, wParam, lParam);
    end;
end;

procedure TDelphiProjectManager.ProcessCopyData(var Message: TWMCopyData);
var
  PFileNum: Pchar;
begin
  if Message.CopyDataStruct^.dwData = $1234 then
  begin
    Message.Result := 0;
    PFileNum := PChar(Message.CopyDataStruct^.lpData);
    FOpenFile := Copy(PFileNum, 1, Length(PFileNum));
    Message.Result := 1;
    AttachThreadInput(GetCurrentThreadId, GetWindowThreadProcessId(Message.From), TRUE);
    //BringToFront;
    Windows.SetFocus((BorlandIDEServices as IOTAServices).GetParentHandle);
    AttachThreadInput(GetCurrentThreadId, GetWindowThreadProcessId(Message.From), FALSE);
    PostMessage(hProjectCallbackWindow, WM_USER, 0, 0);
  end
  else
    inherited;
end;

procedure TDelphiProjectManager.ProcessUserMessage(var Message: TMessage);
var
  v, linenum: Integer;
  s: string;
  m: IOTAModule;
  ms: IOTAModuleServices;
  e: IOTAEditor;
  se: IOTASourceEditor;
  i: Integer;
  FProjectName: string;
  FFileName: string;
  n: Integer;
  w: INTAEditWindow;
  j: Integer;
begin
  s := FOpenFile;
  v := Pos(';', s);
  if v > 0 then
  begin
    linenum := StrToIntDef(Copy(s, 1, v-1), 0);
    Delete(s,1,v);
    v := Pos(';', s);
    if v = 0 then Exit;

    FFileName := Copy(s, 1, v - 1);
    FProjectName := Copy(s, v + 1, MAXINT);

    SetCurrentDir(ExtractFileDir(FProjectName));
    FFileName := ExpandFileName(FFileName);

    if (BorlandIDEServices as IOTAActionServices).OpenProject(FProjectName, True) then
    begin
      ms := BorlandIDEServices as IOTAModuleServices;
      for n := 0 to ms.ModuleCount - 1 do
      begin
        m := ms.Modules[n];
        if SameFileName(ExtractFileName(m.FileName), ExtractFileName(FFileName)) then
        begin
          for i := 0 to m.ModuleFileCount - 1 do
          begin
            e := m.ModuleFileEditors[i];
            if Supports(e, IOTASourceEditor, se) then
            begin
              se.Show;
              se.EditViews[0].Position.GotoLine(linenum);
              se.EditViews[0].Center(linenum, 0);
              se.EditViews[0].Buffer.BufferOptions.HighlightCurrentLine := True;
              se.EditViews[0].Paint;
              w := se.EditViews[0].GetEditWindow;
              for j := 0 to w.Form.ControlCount - 1 do
              begin
                if w.Form.Controls[j].ClassNameIs('TEditControl') then
                begin
                  (w.Form.Controls[j] as TWinControl).SetFocus;
                  Break;
                end;
              end;
              Exit;
            end;
          end;
        end;
      end;
      if (BorlandIDEServices as IOTAActionServices).OpenFile(FFileName) then
      begin
        m := (BorlandIDEServices as IOTAModuleServices).FindModule(FFileName);
        if Assigned(m) then
        begin
          for i := 0 to m.ModuleFileCount - 1 do
          begin
            e := m.ModuleFileEditors[i];
            if Supports(e, IOTASourceEditor, se) then
            begin
              se.Show;
              se.EditViews[0].Position.GotoLine(linenum);
              se.EditViews[0].Center(linenum, 0);
              se.EditViews[0].Paint;
              w := se.EditViews[0].GetEditWindow;
              for j := 0 to w.Form.ControlCount - 1 do
              begin
                if w.Form.Controls[j].ClassNameIs('TEditControl') then
                begin
                  (w.Form.Controls[j] as TWinControl).SetFocus;
                  Break;
                end;
              end;
              Exit;
            end;
          end;
        end;
      end;
    end;
  end;
end;

{ Interface methods }

procedure TDelphiProjectManager.Execute;
begin
  { Doesn't ever get called }
end;

function TDelphiProjectManager.GetIDString: string;
begin
  Result := 'Keyman.DelphiProjectManager.1';
end;

function TDelphiProjectManager.GetName: string;
begin
  Result := 'Keyman.DelphiProjectManager';
end;

function TDelphiProjectManager.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;


procedure TDelphiProjectManager.AfterSave;
begin
end;

procedure TDelphiProjectManager.BeforeSave;
begin
end;

procedure TDelphiProjectManager.Destroyed;
begin
end;

procedure TDelphiProjectManager.Modified;
begin
end;

procedure Register;
begin
  RegisterPackageWizard(TDelphiProjectManager.Create);
end;

end.

