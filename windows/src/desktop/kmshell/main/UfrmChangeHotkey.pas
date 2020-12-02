(*
  Name:             UfrmChangeHotkey
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
                    04 Dec 2006 - mcdurdin - Tweak UI for more straightforward operation
                    05 Dec 2006 - mcdurdin - Localize
                    12 Dec 2006 - mcdurdin - Test for conflicting hotkeys
                    04 Jan 2007 - mcdurdin - Don't test for conflicting hotkeys when clearing hotkey!
                    15 Jan 2007 - mcdurdin - Use font from locale.xml
                    27 Mar 2008 - mcdurdin - Use TfrmKeymanBase instead of TfrmKMShell
                    16 Jan 2009 - mcdurdin - I1630 - Hotkey not showing in menu
                    17 Dec 2010 - mcdurdin - I1188 - Hotkey control usability
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit UfrmChangeHotkey;  // I3306

interface

uses
  System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, UfrmKeymanBase,
  keymanapi_TLB;

type
  TfrmChangeHotkey = class(TfrmKeymanBase)
    lblHotkey: TLabel;
    hkHotkey: THotKey;
    cmdOK: TButton;
    cmdCancel: TButton;
    rbAltLeftShift: TRadioButton;
    rbNone: TRadioButton;
    rbCtrlShift: TRadioButton;
    rbCustom: TRadioButton;
    procedure cmdOKClick(Sender: TObject);
    procedure chkUseHotkeyClick(Sender: TObject);
    procedure TntFormShow(Sender: TObject);
    procedure hkHotkeyChange(Sender: TObject);
    procedure TntFormCreate(Sender: TObject);
    procedure TntFormDestroy(Sender: TObject);
    procedure rbNoneClick(Sender: TObject);
  private
    Checked: Boolean;
    FHotkey: IKeymanHotkey;
    procedure EnableControls;
    function GetNewHotkey: Integer;
  public
    property Hotkey: IKeymanHotkey read FHotkey write FHotkey;
  end;

function ChangeHotkey(AOwner: TComponent; const AMessage: WideString; AHotkey: IKeymanHotkey): Boolean;

implementation

uses
  HotkeyUtils,
  kmint,
  MessageIdentifiers,
  MessageIdentifierConsts,
  Menus,
  utilhotkey;

{$R *.dfm}

function ChangeHotkey(AOwner: TComponent; const AMessage: WideString; AHotkey: IKeymanHotkey): Boolean;
begin
  //TODO: kmcom.Control.DisableHotkeys;
  try
    with TfrmChangeHotkey.Create(AOwner) do
    try
      lblHotkey.Caption := AMessage;
      rbNone.Checked := AHotkey.VirtualKey = 0;
      rbAltLeftShift.Checked := (AHotkey.Modifiers = HK_ALT or HK_SHIFT) and (AHotkey.VirtualKey = 0);
      rbCtrlShift.Checked := (AHotkey.Modifiers = HK_CTRL or HK_SHIFT) and (AHotkey.VirtualKey = 0);
      rbCustom.Checked :=
        not rbNone.Checked and
        not rbAltLeftShift.Checked and
        not rbCtrlShift.Checked;
      if rbCustom.Checked
        then hkHotkey.HotKey := HotkeyToShortcut(AHotkey)
        else hkHotkey.HotKey := 0;
      Hotkey := AHotkey;
      Result := ShowModal = mrOk;
    finally
      Free;
    end;
  finally
    //TODO: kmcom.Control.EnableHotkeys;
  end;
end;

procedure TfrmChangeHotkey.chkUseHotkeyClick(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmChangeHotkey.cmdOKClick(Sender: TObject);
var
  i: Integer;
  OriginalHotkey, NewHotkey: Integer;
  HotkeyText: WideString;
  j: Integer;
begin
  OriginalHotkey := FHotkey.RawValue;
  NewHotkey := GetNewHotkey;
  if OriginalHotkey = NewHotkey then
  begin
    ModalResult := mrOk;
    Exit;
  end;

  if rbCustom.Checked then
  begin
    HotkeyText := ShortcutToTextEx(hkHotkey.Hotkey);

    if (IsHotkeySafe(NewHotkey) = hksNeedsShifting) and not Checked then
      if MessageDlg(MsgFromIdFormat(SKUnsafeHotkey, [HotkeyText]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        Exit;
  end;

  if NewHotkey <> 0 then
  begin
    for i := 0 to kmcom.Languages.Count - 1 do
    begin
      if kmcom.Languages[i].Hotkey.RawValue = NewHotkey then
      begin
        case MessageDlg(MsgFromIdFormat(SKHotkeyConflicts_Keyboard, [HotkeyText, kmcom.Languages[i].LayoutName]), mtConfirmation, mbYesNoCancel, 0) of
          mrYes:    kmcom.Languages[i].Hotkey.Clear;
          mrNo:     Exit;
          mrCancel: Exit;
        end;
      end;
    end;

    for j := 0 to kmcom.Hotkeys.Count - 1 do
    begin
      if kmcom.Hotkeys[j].RawValue = NewHotkey then
      begin
        case MessageDlg(MsgFromIdFormat(SKHotkeyConflicts_Interface, [HotkeyText]), mtConfirmation, mbYesNoCancel, 0) of
          mrYes:    kmcom.Hotkeys[j].Clear;
          mrNo:     Exit;
          mrCancel: Exit;
        end;
      end;
    end;
  end;

  // Note, this forces a requirement to Apply at top level

  FHotkey.RawValue := NewHotkey;

  ModalResult := mrOk;
end;

procedure TfrmChangeHotkey.EnableControls;
begin
  hkHotkey.Enabled := rbCustom.Checked;
end;

function TfrmChangeHotkey.GetNewHotkey: Integer;
begin
  if rbNone.Checked then Result := 0
  else if rbAltLeftShift.Checked then Result := HK_ALT or HK_SHIFT
  else if rbCtrlShift.Checked then Result := HK_CTRL or HK_SHIFT
  else Result := ShortcutToHotkey(hkHotkey.HotKey);
end;

procedure TfrmChangeHotkey.TntFormCreate(Sender: TObject);
var
  s: string;
begin
  inherited;
  s := MsgFromId(SK_UIFontName);
  if s <> '' then Font.Name := s;
  HelpTopic := 'context_changehotkey';
  Caption := MsgFromId(SKChangeHotkeyTitle);
  cmdOK.Caption := MsgFromId(SKButtonOK);
  cmdCancel.Caption := MsgFromId(SKButtonCancel);
end;

procedure TfrmChangeHotkey.TntFormDestroy(Sender: TObject);
begin
  inherited;
  kmint.KeymanEngineControl.EnableUserInterface;
end;

procedure TfrmChangeHotkey.TntFormShow(Sender: TObject);
begin
  kmint.KeymanEngineControl.DisableUserInterface;
  EnableControls;
end;

procedure TfrmChangeHotkey.hkHotkeyChange(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmChangeHotkey.rbNoneClick(Sender: TObject);
begin
  EnableControls;
  if hkHotkey.Enabled then  // I1188
    if Showing
      then hkHotkey.SetFocus
      else ActiveControl := hkHotkey;
end;

end.
