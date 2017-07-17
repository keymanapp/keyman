(*
  Name:             Menu_KeyboardItems
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Sep 2006

  Modified Date:    23 Oct 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          14 Sep 2006 - mcdurdin - Initial version
                    12 Dec 2006 - mcdurdin - Localize menu
                    04 Jan 2007 - mcdurdin - Add OpenKeyboardHelp action
                    15 Jan 2007 - mcdurdin - Get menu font and size from locale.xml
                    14 Sep 2007 - mcdurdin - I1027 - Do nothing if "No Action" assigned to menu item (instead of show error)
                    27 Mar 2008 - mcdurdin - I1352 - Show hint when not keyboards are in the menu
                    27 Mar 2008 - mcdurdin - I1373 - Link to keyboard usage page
                    14 Jun 2008 - mcdurdin - I1439 - Skip keyboards without an owner product
                    12 Mar 2010 - mcdurdin - I2226 - Show Text Editor, Char Map, Keyboard Usage and Font Helper entries in menu
                    12 Mar 2010 - mcdurdin - I2228 - Support column break in menu
                    09 Apr 2010 - mcdurdin - I2296 - New menu items not showing hotkeys
                    09 Apr 2010 - mcdurdin - I2287 - Menu links not opening correct page in OSK
                    11 Jan 2011 - mcdurdin - I1867 - Sort keyboards alphabetically
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    01 Dec 2012 - mcdurdin - I3614 - V9.0 - Start removal of keyboard selection list from UI
                    24 Oct 2013 - mcdurdin - I3933 - V9.0 - Keyman tray icon menu is not showing installed keyboards
                    11 Nov 2013 - mcdurdin - I3960 - V9.0 - Reskin the Keyman menu with 9.0 style
                    17 Dec 2013 - mcdurdin - I4004 - V9.0 - Remove old Keyman keyboard code from LangSwitchManager
                    01 May 2014 - mcdurdin - I4207 - V9.0 - After installing + uninstalling keyboards, found extra blank entries in the Keyman keyboard list
                    28 Aug 2014 - mcdurdin - I4390 - V9.0 - Free vs Pro
                    28 Aug 2014 - mcdurdin - I4390 - V9.0 - Free vs Pro
                    01 Sep 2014 - mcdurdin - I4398 - V9.0 - Hotkeys don't show on keyboard list
                    01 Sep 2014 - mcdurdin - I4393 - V9.0 - Keyman Desktop Free Edition polish
                    23 Oct 2014 - mcdurdin - I4458 - Crash showing keyboard menu when product missing [CrashID:keyman.exe_9.0.472.0_2C5933D2_EAccessViolation]
*)
unit Menu_KeyboardItems;  // I3306   // I3614   // I4004

interface

uses
  Windows,
  Classes,
  Controls,
  custinterfaces,
  Graphics,
  KeymanMenuItem,
  keymanapi_TLB,
  Menus,
  SysUtils,
  utilhotkey;

function CreateKeymanMenuItem(FKeyman: IKeyman; Owner: TPopupMenu; cmi: IKeymanCustomisationMenuItem): TKeymanMenuItem;

procedure BuildCustMenu(FKeyman: IKeyman; FMenu: TPopupMenu; Location: TCustomisationMenuItemLocation);   // I3933

implementation

uses
  ActiveX,
  AxCtrls,
  BitmapIPicture,
  KeymanDesktopShell,
  kmint,
  LangSwitchManager,
  MessageIdentifierConsts,
  MessageIdentifiers,
  InterfaceHotkeys,
  UfrmKeyman7Main,
  UfrmOSKCharacterMap,
  UfrmVisualKeyboard,
  utilxml;

procedure AddKeyboardItems(FKeyman: IKeyman; mnu: TPopupMenu; MenuClick: TNotifyEvent;   // I3960
  cmi: IKeymanCustomisationMenuItem);   // I3933   // I4390

  function CreateLanguageMenuItem(Language: TLangSwitchLanguage): TKeymanMenuItem;   // I3933
  var
    s: WideString;
  begin
    Result := TKeymanMenuItem.Create(mnu);

    Result.FontName := MsgFromId(SK_UIFontName);
    if Result.FontName = '' then Result.FontName := 'Tahoma';

    Result.FontSize := StrToIntDef(MsgFromId(SK_UIFontSize), 8);

    s := MsgFromStr(':String[@Caption='''+xmlencode(StripHotkey(Language.Caption))+''']');
    if s = ''
      then Result.WideCaption := StringReplace(Language.Caption, '&', '&&', [rfReplaceAll])
      else Result.WideCaption := s;

    Result.OnClick := MenuClick;
    if Assigned(cmi)
      then Result.CMIItemType := cmi.ItemType   // I3960
      else Result.CMIItemType := _mitKeyboardsList;   // I3960
    Result.CustomisationMenuItem := cmi;
    Result.ItemType := kmitLanguage;
    Result.LanguageCode := Language.IconText;
    Result.Keyboard := Language.Keyboards[0];
  end;

  function CreateKeyboardMenuItem(Language: TLangSwitchLanguage; Keyboard: TLangSwitchKeyboard): TKeymanMenuItem;   // I3933
  var
    kmkbd: IKeymanKeyboardInstalled;
    kbd: IKeymanKeyboardInstalled;
    FCaption: WideString;
    i, j: Integer;
    FHKL: HKL;
    FTargetLanguage: IKeymanLanguage;
    FProfileGUID: TGUID;
  begin
    Result := TKeymanMenuItem.Create(mnu);

    Result.CMIItemType := _mitKeyboardsList;   // I3960

    FTargetLanguage := nil;
    kbd := nil;
    if Keyboard is TLangSwitchKeyboard_TIP then   // I4398
    begin
      FProfileGUID := (Keyboard as TLangSwitchKeyboard_TIP).Profile.guidProfile;

      for i := 0 to FKeyman.Keyboards.Count - 1 do
      begin
        kbd := FKeyman.Keyboards[i];
        for j := 0 to kbd.Languages.Count - 1 do
          if kbd.Languages[j].ProfileGUID = FProfileGUID then
          begin
            kmkbd := kbd as IKeymanKeyboardInstalled;
            Break;
          end;
        if Assigned(kmkbd) then
          Break;
      end;

      for i := 0 to FKeyman.Languages.Count - 1 do
        if FKeyman.Languages[i].ProfileGUID = FProfileGUID then
        begin
          FTargetLanguage := FKeyman.Languages[i];
          Break;
        end;
    end
    else if Keyboard is TLangSwitchKeyboard_WinKeyboard then
    begin
      FHKL := (Keyboard as TLangSwitchKeyboard_WinKeyboard).HKL;
      for i := 0 to FKeyman.Languages.Count - 1 do
        if FKeyman.Languages[i].HKL = FHKL then
        begin
          FTargetLanguage := FKeyman.Languages[i];
          Break;
        end;
    end;

    Result.FontName := MsgFromId(SK_UIFontName);
    if Result.FontName = '' then Result.FontName := 'Tahoma';

    Result.FontSize := StrToIntDef(MsgFromId(SK_UIFontSize), 8);

    if Language.KeyboardCount = 1 then
    begin
      FCaption := Language.Caption + ' - ' + Keyboard.Caption;
      Result.ItemType := kmitLanguageKeyboard;
      Result.LanguageCode := Language.IconText;
    end
    else
    begin
      FCaption := Keyboard.Caption;
      Result.ItemType := kmitKeyboard;
    end;

    Result.WideCaption := MsgFromStr(':String[@Caption='''+xmlencode(StripHotkey(FCaption))+''']');
    if Result.WideCaption = '' then
      Result.WideCaption := StringReplace(FCaption, '&', '&&', [rfReplaceAll]);

    Result.OnClick := MenuClick;
    if Assigned(FTargetLanguage) then
      Result.ShortCut := HotkeyToShortcut(FTargetLanguage.Hotkey);   // I4398
    Result.CustomisationMenuItem := cmi;

    Result.Checked := Keyboard.Active;

    // TODO: Support icon for TSF TIPs

    if Assigned(Keyboard.Bitmap) then
    begin
      // Special case for windows keyboards and language single item - we
      // want to show the language icon in that situation
      if (Language.KeyboardCount > 1) or not (Keyboard is TLangSwitchKeyboard_WinKeyboard) then
        Result.Icon.Assign(Keyboard.Bitmap);
    end;

    Result.Keyboard := Keyboard;
  end;


var
  i: Integer;
  FLanguage: TLangSwitchLanguage;
  j: Integer;
begin
  { Use the lang switch manager to get the list of installed languages }

  frmKeyman7Main.LangSwitchManager.Refresh;   // I4207 ?? is this really needed

  for i := 0 to frmKeyman7Main.LangSwitchManager.LanguageCount - 1 do   // I3933
  begin
    FLanguage := frmKeyman7Main.LangSwitchManager.Languages[i];
    if FLanguage.KeyboardCount = 1 then
    begin
      mnu.Items.Add(CreateKeyboardMenuItem(FLanguage, FLanguage.Keyboards[0]));
    end
    else if FLanguage.KeyboardCount > 0 then
    begin
      mnu.Items.Add(CreateLanguageMenuItem(FLanguage));
      for j := 0 to FLanguage.KeyboardCount - 1 do
      begin
        mnu.Items.Add(CreateKeyboardMenuItem(FLanguage, FLanguage.Keyboards[j]));
      end;
    end;
  end;
end;

function CreateKeymanMenuItem(FKeyman: IKeyman; Owner: TPopupMenu; cmi: IKeymanCustomisationMenuItem): TKeymanMenuItem;
var
  FBitmap: TBitmapIPicture;
  Hotkeys: IKeymanHotkeys;
  i: Integer;
  DesiredTarget: KeymanHotkeyTarget;
  s: WideString;
begin
  Result := TKeymanMenuItem.Create(Owner);
  Result.FontName := MsgFromId(SK_UIFontName);
  Result.FontSize := StrToIntDef(MsgFromId(SK_UIFontSize), 0);
  Result.CMIItemType := cmi.ItemType;   // I3960
  Result.CustomisationMenuItem := cmi;

  case cmi.ItemType of
    mitText:
      begin
        s := MsgFromStr(':String[@Caption='''+xmlencode(StripHotkey(cmi.Caption))+''']');
        if s = ''
          then Result.WideCaption := cmi.Caption
          else Result.WideCaption := s;
        FBitmap := TBitmapIPicture.Create;
        try
          FBitmap.LoadFromIPicture(cmi.Icon as IPicture);
          if not FBitmap.Empty then
          begin
            Result.Icon := FBitmap;
            Result.Icon.Transparent := True;
          end;
        finally
          FBitmap.Free;
        end;
      end;
    mitSeparator: Result.WideCaption := '-';
    mitGraphic:
      begin
        Result.DefaultBitmap.LoadFromIPicture(cmi.PictureDefault as IPicture);
        Result.SelectedBitmap.LoadFromIPicture(cmi.PictureSelected as IPicture);
      end;
  end;

  case cmi.Action of
    miaOpenKeymanConfiguration: DesiredTarget := khKeymanConfiguration;
    miaToggleVisualKeyboard: DesiredTarget := khVisualKeyboard;
    miaOSK_ViewKeyboard: DesiredTarget := khVisualKeyboard;
    miaOSK_ViewCharMap: DesiredTarget := khCharacterMap;
    miaOSK_ViewKeyboardUsage: DesiredTarget := khKeyboardUsage;
    miaOSK_ViewFontHelper: DesiredTarget := khFontHelper;
    miaOpenTextEditor: DesiredTarget := khTextEditor;
    else Exit;
  end;

  Hotkeys := FKeyman.Hotkeys;
  for i := 0 to Hotkeys.Count - 1 do
    if Hotkeys[i].Target = DesiredTarget then
    begin
      Result.ShortCut := HotkeyToShortcut(Hotkeys[i]);
      Break;
    end;
end;

procedure BuildCustMenu(FKeyman: IKeyman; FMenu: TPopupMenu; Location: TCustomisationMenuItemLocation);   // I3933
var
  i: Integer;
  cmi: IKeymanCustomisationMenuItem;
  mi: TKeymanMenuItem;
//  xmi: TMenuItem;
  FNextMenuItemIsBreak: Boolean;
begin
  FMenu.Items.Clear;
  FNextMenuItemIsBreak := False;

  with kmint.KeymanCustomisation do
    for i := 1 to CustMenuItems.Count do {$MESSAGE HINT 'This indexing needs to be sorted out'}
    begin
      cmi := CustMenuItems.Items[i];
      if cmi.Location <> Location then Continue;

      if (CustMenuItems.Items[i].Action = miaOSK_ViewCharMap) and (not TfrmOSKCharacterMap.IsCharMapDataInstalled or not TfrmOSKCharacterMap.IsCharMapAvailable) then Continue;
      if (CustMenuItems.Items[i].Action = miaOSK_ViewEntryHelper) and not TfrmOSKCharacterMap.IsCharMapAvailable then Continue;

      if cmi.ItemType = mitBreak then
      begin
        FNextMenuItemIsBreak := True;
        Continue;
      end;

      //if FNextMenuItemIsBreak and (cmi.ItemType = mit) then Continue;

      if cmi.ItemType = _mitKeyboardsList then   // I3933
      begin
        AddKeyboardItems(FKeyman, FMenu, frmKeyman7Main.MnuKeyboardClick, cmi);   // I3960   // I4390
        FNextMenuItemIsBreak := True;
        Continue;
        //FNextMenuItemIsBreak := True;
        { xmi := TMenuItem.Create(FMenu);
        xmi.Caption := 'z-';
        xmi.Break := mbBreak;
        FMenu.Items.Add(xmi);   }
      end
      else if cmi.ItemType = mitBreak then
      begin
        FNextMenuItemIsBreak := True;
        Continue;
      end;


      if (cmi.ItemType = _mitKeyboardsList) or (cmi.Action = _miaSwitchKeymanOff) then Continue;


      mi := CreateKeymanMenuItem(FKeyman, FMenu, cmi);

      if FNextMenuItemIsBreak then mi.Break := mbBreak;
      FNextMenuItemIsBreak := False;

      case cmi.Action of
        miaOpenKeymanConfiguration: mi.OnClick := frmKeyman7Main.MnuOpenKeymanConfiguration;
        miaOpenProductHelp: mi.OnClick := frmKeyman7Main.MnuOpenProductHelp;
        miaOpenKeyboardHelp:
          begin
            mi.Enabled := frmKeyman7Main.ActiveKeymanID >= 0;
            mi.OnClick := frmKeyman7Main.MnuOpenKeyboardHelp;
          end;
        miaOpenProductAbout: mi.OnClick := frmKeyman7Main.MnuOpenProductAbout;
        miaOSK_ViewKeyboard, miaToggleVisualKeyboard:
          begin
            mi.Checked := frmKeyman7Main.VisualKeyboardVisible(apKeyboard);
            mi.OnClick := frmKeyman7Main.MnuVisualKeyboard;
          end;
        miaOSK_ViewKeyboardUsage:
          begin
            mi.Checked := frmKeyman7Main.VisualKeyboardVisible(apKeyboardUsage);
            mi.OnClick := frmKeyman7Main.MnuKeyboardUsage;
          end;
        miaOSK_ViewFontHelper:
          begin
            mi.Checked := frmKeyman7Main.VisualKeyboardVisible(apFontHelper);
            mi.OnClick := frmKeyman7Main.MnuFontHelper;
          end;
        miaOSK_ViewCharMap:
          begin
            mi.Checked := frmKeyman7Main.VisualKeyboardVisible(apCharacterMap);
            mi.OnClick := frmKeyman7Main.MnuCharacterMap;
          end;
        miaOpenTextEditor:
          begin
            mi.Checked := TKeymanDesktopShell.IsTextEditorVisible;
            mi.OnClick := frmKeyman7Main.MnuOpenTextEditor;
          end;
        miaExitProduct: mi.OnClick := frmKeyman7Main.MnuExitKeyman;
        miaRunProgram: mi.OnClick := frmKeyman7Main.MnuRunProgram;
      end;

      if Assigned(frmKeyman7Main.frmVisualKeyboard) then
        case cmi.Action of
          miaOSK_Fade:
            begin
              mi.Enabled := (GetVersion and $80000000) = 0;
              mi.OnClick := frmKeyman7Main.frmVisualKeyboard.MnuOSK_Fade;
              mi.Checked := frmKeyman7Main.frmVisualKeyboard.IsFading;
            end;
          miaOSK_ToggleToolbar:
            begin
              mi.OnClick := frmKeyman7Main.frmVisualKeyboard.MnuOSK_ToggleToolbar;
              mi.Checked := frmKeyman7Main.frmVisualKeyboard.IsToolbarVisible;
            end;
          miaOSK_SaveAsWebPage:
            begin
              mi.OnClick := frmKeyman7Main.frmVisualKeyboard.MnuOSK_SaveAsWebPage;
              mi.Enabled := frmKeyman7Main.frmVisualKeyboard.IsSaveEnabled;
            end;
          miaOSK_Print:
            begin
              mi.OnClick := frmKeyman7Main.frmVisualKeyboard.MnuOSK_Print;
              mi.Enabled := frmKeyman7Main.frmVisualKeyboard.IsPrintEnabled;
            end;
          miaOSK_Close: mi.OnClick := frmKeyman7Main.frmVisualKeyboard.MnuOSK_Close;
        end;

      if not Assigned(mi.OnClick) then mi.OnClick := nil; //frmKeyman7Main.MnuError;  I1027

      FMenu.Items.Add(mi);
    end;
end;

end.
