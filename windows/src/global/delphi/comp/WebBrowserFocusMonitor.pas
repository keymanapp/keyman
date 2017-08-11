(*
  Name:             WebBrowserFocusMonitor
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Aug 2006

  Modified Date:    17 Dec 2010
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2006 - mcdurdin - Initial version
                    30 Aug 2006 - mcdurdin - Only set focus if the web browser is visible
                    17 Dec 2010 - mcdurdin - I2570 - Use new EmbeddedWB
*)
unit WebBrowserFocusMonitor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  MSHTML_TLB, shdocvw, ActiveX;

type
  TWebBrowserFocusMonitor = class(TComponent)
  private
    FWB: TWebBrowser;
    FDefInetExplorerServerProc: Pointer;
    FDefShellObjViewProc: Pointer;
    FShellDocObjViewHandle: THandle;
    FInetExplorerServerHandle: THandle;
    FShellDocObjInstance: Pointer;
    FInetExplorerServerInstance: Pointer;
    FHooksSet: Boolean;
  protected
    procedure InetExplorerServerWndProc(var Message: TMessage);
    procedure ShellDocObjWndProc(var Message: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    procedure UnHookChildWindows;
    procedure HookChildWindows;
    function IsHooked: Boolean;
  published
    property WebBrowser: TWebBrowser read FWB write FWB;
  end;

procedure Register;
procedure LoadDocFromString(WB: TWebBrowser; const HTMLString: string);

implementation

uses
  Variants;

procedure LoadDocFromString(WB: TWebBrowser; const HTMLString: string);
var
  v: OleVariant;
  HTMLDocument: IHTMLDocument2;
begin
  HTMLDocument := WB.Document as IHTMLDocument2;
  v := VarArrayCreate([0, 0], varVariant);
  v[0] := HTMLString;
  HTMLDocument.Write(PSafeArray(TVarData(v).VArray));
  HTMLDocument.Close;
end;

procedure Register;
begin
  RegisterComponents('Eze', [TWebBrowserFocusMonitor]);
end;

constructor TWebBrowserFocusMonitor.Create(AOwner: TComponent);
begin
  inherited;
  FWB := nil;
  FDefInetExplorerServerProc := nil;
  FDefShellObjViewProc := nil;
  FShellDocObjViewHandle := 0;
  FInetExplorerServerHandle := 0;
  FShellDocObjInstance := nil;
  FInetExplorerServerInstance := nil;
  FHooksSet := False;
end;

procedure TWebBrowserFocusMonitor.HookChildWindows;
begin
  if csDesigning in ComponentState then exit;

  if FWB = nil then Exit;

  //hHookProc := SetWindowsHookEx(WH_CALLWNDPROCRET, HookProc, 0, GetCurrentThreadId);

  // Hook child windows to catch destroywnd messages when docking the editor window
  if (FShellDocObjViewHandle <> 0) or (FInetExplorerServerHandle <> 0) then
    raise Exception.Create('Child already hooked');
  FShellDocObjViewHandle := Windows.GetWindow(FWB.Handle, GW_CHILD);
  if (FShellDocObjViewHandle <> 0) and not FHooksSet then
  begin
    FInetExplorerServerInstance := Classes.MakeObjectInstance(InetExplorerServerWndProc);
    FShellDocObjInstance := Classes.MakeObjectInstance(ShellDocObjWndProc);
    FHooksSet := True;
    FInetExplorerServerHandle := Windows.GetWindow(FShellDocObjViewHandle, GW_CHILD);
    FDefShellObjViewProc := Pointer(GetWindowLong(FShellDocObjViewHandle, GWL_WNDPROC));
    SetWindowLong(FShellDocObjViewHandle, GWL_WNDPROC, Longint(FShellDocObjInstance));
    FDefInetExplorerServerProc := Pointer(GetWindowLong(FInetExplorerServerHandle, GWL_WNDPROC));
    SetWindowLong(FInetExplorerServerHandle, GWL_WNDPROC, Longint(FInetExplorerServerInstance));
  end;
end;

procedure TWebBrowserFocusMonitor.InetExplorerServerWndProc(var Message: TMessage);
begin
  Message.Result := CallWindowProc(FDefInetExplorerServerProc, FInetExplorerServerHandle, Message.Msg, Message.WParam, Message.LParam);
  case Message.Msg of
    WM_SETFOCUS:
      begin
        // Catching this message allows us to set the Active control to the
        // WebBrowser itself which keeps VCL in sync with the real active control
        // which makes things like tabbing work correctly.
        if FWB.Showing and FWB.CanFocus then
          GetParentForm(FWB).ActiveControl := FWB
        else if Assigned(GetParentForm(FWB).ActiveControl) then
          // ignore the wm_setfocus because it came at an inappropriate time
          GetParentForm(FWB).ActiveControl.SetFocus;
      end;
    WM_DESTROY: UnHookChildWindows;
  end;
end;

function TWebBrowserFocusMonitor.IsHooked: Boolean;
begin
  Result := (FShellDocObjViewHandle <> 0) or (FInetExplorerServerHandle <> 0);
end;

procedure TWebBrowserFocusMonitor.ShellDocObjWndProc(var Message: TMessage);
begin
  Message.Result := CallWindowProc(FDefShellObjViewProc, FShellDocObjViewHandle,
    Message.Msg, Message.WParam, Message.LParam);
  case Message.Msg of
    WM_DESTROY: UnHookChildWindows;
  end;
end;

procedure TWebBrowserFocusMonitor.UnHookChildWindows;
begin
  if FShellDocObjViewHandle <> 0 then
  begin
    if GetWindowLong(FShellDocObjViewHandle, GWL_WNDPROC) = Longint(FShellDocObjInstance) then  // I2570
      SetWindowLong(FShellDocObjViewHandle, GWL_WNDPROC, Integer(FDefShellObjViewProc));
    Classes.FreeObjectInstance(FShellDocObjInstance);
    FShellDocObjViewHandle := 0;
    FShellDocObjInstance := nil;
  end;

  if FInetExplorerServerHandle <> 0 then
  begin
    if GetWindowLong(FShellDocObjViewHandle, GWL_WNDPROC) = Integer(FInetExplorerServerInstance) then  // I2570
      SetWindowLong(FInetExplorerServerHandle, GWL_WNDPROC, Integer(FDefInetExplorerServerProc));
    Classes.FreeObjectInstance(FInetExplorerServerInstance);
    FInetExplorerServerHandle := 0;
    FInetExplorerServerInstance := nil;
  end;
end;

end.
