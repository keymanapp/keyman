{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHotKey.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Th�rnqvist [peter3@peter3.com]
Portions created by Peter Th�rnqvist are Copyright (C) 2002 Peter Th�rnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{ A component that allows the user to register an application wide hotkey combination.
    Set the HotKey property to a *unique* combination of Ctrl,Alt,Shift and a character.
    Set active to True to receive notifications when the hotkey is pressed. The OnHotKey
    event is called when the user presses the hotkey combination.}

unit JvAppHotKey;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  JvComponent;

type
  TJvHotKeyRegisterFailed = procedure(Sender: TObject; var HotKey: TShortCut) of object;

  TJvApplicationHotKey = class(TJvComponent)
  private
    FActive: Boolean;
    FShortCut: TShortCut;
    FOnHotKey: TNotifyEvent;
    FHandle: THandle;
    FID: Integer;
    FHasRegistered: Boolean;
    FOnHotKeyRegisterFailed: TJvHotKeyRegisterFailed;
    procedure SetActive(Value: Boolean);
    procedure SetShortCut(Value: TShortCut);
    function WndProc(var Msg: TMessage): Boolean;
    procedure GetWndProc;
    procedure ResetWndProc;
  protected
    procedure DoHotKey; virtual;
    function DoRegisterHotKey: Boolean; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read FActive write SetActive default False;
    property HotKey: TShortCut read FShortCut write SetShortCut;
    property OnHotKey: TNotifyEvent read FOnHotKey write FOnHotKey;
    property OnHotKeyRegisterFailed:TJvHotKeyRegisterFailed read FOnHotKeyRegisterFailed write FOnHotKeyRegisterFailed;
  end;

implementation

uses
  JvWndProcHook;

{ $R HotKey.dcr }

var
  HotKeyInstances: Integer = 0;

procedure GetHotKey(AShortCut: TShortCut; var VirtKey, Modifiers: Word);
var
  Shift: TShiftState;
begin
  ShortCutToKey(AShortCut, VirtKey, Shift);
  Modifiers := 0;
  if ssCtrl in Shift then
    Modifiers := Modifiers or MOD_CONTROL;
  if ssShift in Shift then
    Modifiers := Modifiers or MOD_SHIFT;
  if ssAlt in Shift then
    Modifiers := Modifiers or MOD_ALT;
end;


//=== TJvApplicationHotKey ===================================================

constructor TJvApplicationHotKey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TJvApplicationHotKey.Destroy;
begin
  ResetWndProc;
  inherited Destroy;
end;

procedure TJvApplicationHotKey.SetShortCut(Value: TShortCut);
var
  B: Boolean;
begin
  if FShortCut <> Value then
  begin
    B := FActive;
    SetActive(False);
    FShortCut := Value;
    SetActive(B);
  end;
end;

function TJvApplicationHotKey.DoRegisterHotKey: Boolean;
var
  AShortCut: TShortCut;
  VirtKey, Mods: Word;
begin
  Result := False;
  if FHandle = 0 then
  begin
    FHandle := TWinControl(Owner).Handle;
    GetHotKey(FShortCut, VirtKey, Mods);
    while not RegisterHotKey(FHandle, FID, Mods, VirtKey) do
    begin
      if Assigned(FOnHotKeyRegisterFailed) then
      begin
        AShortCut := FShortCut;
        FOnHotKeyRegisterFailed(Self, FShortCut);
        // make sure we don't get stuck in a loop here:
        if AShortCut = FShortCut then
          Exit;
        GetHotKey(FShortCut, VirtKey, Mods);
      end
      else
        Exit;
    end;
    Result := True;
  end;
end;

procedure TJvApplicationHotKey.SetActive(Value: Boolean);
begin
  if FActive <> Value then
  begin
    if csDesigning in ComponentState then
    begin
      FActive := Value;
      Exit;
    end;
    if Value and not FHasRegistered then
    begin
      if IsLibrary then
        FID := GlobalAddAtom(PChar(Application.Exename))
      else
      begin
        FID := HotKeyInstances;
        Inc(HotKeyInstances);
      end;
      if not DoRegisterHotKey then
        Exit;
      GetWndProc;
    end
    else
    if FHasRegistered then
    begin
      UnRegisterHotKey(FHandle, FID);
      ResetWndProc;
      if IsLibrary then
        GlobalDeleteAtom(FID);
    end;
    FActive := Value;
  end;
end;

procedure TJvApplicationHotKey.DoHotKey;
begin
  if Assigned(FOnHotKey) then
    FOnHotKey(Self);
end;

procedure TJvApplicationHotKey.GetWndProc;
begin
  if not FHasRegistered and (Owner is TWinControl) then
  begin
    RegisterWndProcHook(TWinControl(Owner), WndProc, hoAfterMsg);
    FHasRegistered := True;
  end
  else
    SetActive(False);
end;

procedure TJvApplicationHotKey.ResetWndProc;
begin
  if FHasRegistered and (Owner is TWinControl) then
  begin
    UnregisterWndProcHook(TWinControl(Owner), WndProc, hoAfterMsg);
    FHasRegistered := False;
  end;
  FHandle := 0;
end;

function TJvApplicationHotKey.WndProc(var Msg: TMessage): Boolean;
begin
  if (Msg.Msg = WM_HOTKEY) and (FID = Msg.WParam) then
    DoHotKey;
  Result := False;
end;

end.

