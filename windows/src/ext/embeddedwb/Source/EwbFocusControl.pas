//***********************************************************
//                       TEWBFocusControl unit              *
//                                                          *
//                     For Delphi 5 to XE                   *
//                     Freeware Component                   *
//                            by                            *
//                          (smot)                          *
//                                                          *
//  Documentation and updated versions:                     *
//                                                          *
//               http://www.bsalsa.com                      *
//***********************************************************
{*******************************************************************************}
{LICENSE:
THIS SOFTWARE IS PROVIDED TO YOU "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED INCLUDING BUT NOT LIMITED TO THE APPLIED
WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
YOU ASSUME THE ENTIRE RISK AS TO THE ACCURACY AND THE USE OF THE SOFTWARE
AND ALL OTHER RISK ARISING OUT OF THE USE OR PERFORMANCE OF THIS SOFTWARE
AND DOCUMENTATION. BSALSA PRODUCTIONS DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SYSTEMS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SYSTEMS. BSALSA PRODUCTIONS SPECIFICALLY
DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use, change or modify the component under 4 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@gmail.com) any code change in the unit
   for the benefit of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}

unit EwbFocusControl;

{$I EWB.inc}

interface

uses
  Windows, Messages, Classes, Forms, Controls;

// -- TEWBFocusControl ---------------------------------------------------------

type
  TEWBFocusControl = class { Singleton }
  private
    constructor Create;
  public
    class procedure Activate(Value: Boolean);
  end;

  // Enable TEWBApplicationHook, if accessible at runtime only
  // Enable_HookMainWindow must be defined in EWB.inc
  // EWBHookMainWindow is activated by default.

var
  EWBEnableFocusControl: Boolean = True; // DO NOT CHANGE HERE

implementation

uses
  EwbCoreTools;

// -- FreeAndNil (SysUtils) ----------------------------------------------------

procedure FreeAndNil(var Obj);
var
  Temp: TObject;
begin
  try
    Temp := TObject(Obj);
  finally
    Pointer(Obj) := nil;
  end;
  Temp.Free;
end;

// -- TAppHookWindow ----------------------------------------------------------

type
  TAppHookWindow = class(TWinControl)
  private
    FHookSet: Boolean;
    function MessageHook(var Msg: TMessage): Boolean;
  public
    destructor Destroy; override;
    procedure Activate;
    procedure Deactivate;
  end;

var
  EWBAppHookInstance: TAppHookWindow;

  // -- TEWBFocusControl ---------------------------------------------------------

constructor TEWBFocusControl.Create;
begin
  inherited;
  if EWBEnableFocusControl then
  begin
    if EWBAppHookInstance = nil then
    begin
      EWBAppHookInstance := TAppHookWindow.Create(nil);
      EWBAppHookInstance.Activate;
    end;
  end;
end;

class procedure TEWBFocusControl.Activate(Value: Boolean)
  {: TEWBApplicationHook};
const
{$J+}
  Instance: TEWBFocusControl = nil;
{$J-}
begin
  if EWBEnableFocusControl then
    case Value of
      True:
        begin
          if not Assigned(Instance) then
            Instance := Create;
        end;
      False:
        begin
          if Assigned(EWBAppHookInstance) then
            FreeAndNil(EWBAppHookInstance);
          if Assigned(Instance) then
            FreeAndNil(Instance);
        end;
    end;
end;

// -- TAppHookWindow -----------------------------------------------------------

function TAppHookWindow.MessageHook(var Msg: TMessage): Boolean;
var
  ActiveControl: TWinControl;
  ActiveForm: TCustomForm;
  bContinue: Boolean;
//  s: string;
begin
  Result := False;
  if (Msg.Msg = WM_WINDOWPOSCHANGING) or (Msg.Msg = CM_ACTIVATE) then
  begin
    ActiveForm := Screen.ActiveForm;
    if Assigned(ActiveForm) then
    begin
      if Screen.ActiveForm.FormStyle = fsMDIChild then // Check if MDI
        bContinue := IsChild(GetActiveWindow, ActiveForm.Handle)
      else
        bContinue :=  not Forms.Application.Terminated and ActiveForm.HandleAllocated and
          (ActiveForm.Handle = GetActiveWindow);

      if bContinue and (ActiveForm.CanFocus) then
      begin
        ActiveControl := ActiveForm.ActiveControl;
       // s := '** MessageHook ' + ActiveControl.ClassName + ' ' + Inttostr(ActiveForm.Handle) + ' ' +  Inttostr(GetFocus);
      //  OutputDebugString(PChar(s));
        if Assigned(ActiveControl) and ((ActiveControl.ClassName = 'TEmbeddedWB') or
          (ActiveControl.ClassName = 'TEWBCore')) then
          if GetFocus <> ActiveControl.Handle then
          begin
            PostMessage(ActiveControl.Handle, WM_SETWBFOCUS, Integer(ActiveControl), 0);
             //  OutputDebugString(PChar('Focus set'));
             //  ActiveControl.SetFocus doesn't work when switching between forms.
          end;
      end;
    end;
  end;
end;

destructor TAppHookWindow.Destroy;
begin
  inherited;
  Deactivate;
end;

procedure TAppHookWindow.Activate;
begin
  if (not FHookSet) and EWBEnableFocusControl then
    if Assigned(Application) then
    begin
      Application.HookMainWindow(MessageHook);
      FHookSet := True;
    end;
end;

procedure TAppHookWindow.Deactivate;
begin
  if Assigned(Application) then
  begin
    Application.UnHookMainWindow(MessageHook);
    FHookSet := False;
  end;
end;

end.
