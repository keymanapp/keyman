(*
  Name:             UfrmVisualKeyboard
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    10 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Clean up visual keyboard display
                    23 Aug 2006 - mcdurdin - Initial refactor for new visual keyboard
                    14 Sep 2006 - mcdurdin - Complete toolbar, character map integration, menus, polish
                    04 Dec 2006 - mcdurdin - Resize keyboard appropriately when first shown
                    04 Dec 2006 - mcdurdin - Add Vista support
                    12 Dec 2006 - mcdurdin - Fix fade in on-screen keyboard
                    12 Dec 2006 - mcdurdin - Translate right-click menu and toolbar tooltips
                    04 Jan 2007 - mcdurdin - Make tooltips for toolbar more visible
                    04 Jan 2007 - mcdurdin - Add help support
                    04 Jan 2007 - mcdurdin - Add keyboard help support
                    04 Jan 2007 - mcdurdin - Fix buttons deleting when switching products
                    15 Jan 2007 - mcdurdin - Use font from locale.xml
                    25 Jan 2007 - mcdurdin - Disable text entry helper for release
                    23 Aug 2007 - mcdurdin - I933 - Fix text display in tooltips
                    12 Oct 2007 - mcdurdin - I958 - Remove unnecessary crossreference
                    27 Mar 2008 - mcdurdin - I1256 - hint system
                    27 Mar 2008 - mcdurdin - I1373 - keyboard usage
                    27 Mar 2008 - mcdurdin - I1374 - font helper
                    27 Mar 2008 - mcdurdin - I1375 - switch to correct page automatically
                    27 Mar 2008 - mcdurdin - I1378 - allow drag anywhere on title
                    27 Mar 2008 - mcdurdin - I1311 - Fix selecting incorrect keyboard with popup menu
                    27 Mar 2008 - mcdurdin - I1251 - combine keyboard help with Keyman help
                    27 Mar 2008 - mcdurdin - I1314 - don't allow all buttons to be up for keyboards
                    27 Mar 2008 - mcdurdin - I1232 - Show hint on how to get OSK back when closing
                    14 Jun 2008 - mcdurdin - I1434 - Fix range check error with Greek Polytonic keyboard
                    14 Jun 2008 - mcdurdin - Refactor web hint manager
                    28 Jul 2008 - mcdurdin - I1510 - context help
                    16 Jan 2009 - mcdurdin - I1144 - Reset shift state when OSK is closed or goes to another tab
                    16 Jan 2009 - mcdurdin - I1630 - Hotkey not displayed in menu
                    09 Mar 2009 - mcdurdin - I1763 - OSK can open on "usage" page even if option is not available
                    12 Mar 2010 - mcdurdin - I2226 - Add text editor, char map, keyboard usage and font helper to keyman menu
                    29 Mar 2010 - mcdurdin - I2072 - Cannot move OSK to secondary monitory
                    29 Mar 2010 - mcdurdin - I1720 - OSK does not limit to workarea correctly if taskbar at top of screen
                    09 Apr 2010 - mcdurdin - I2287 - OSK menu items not opening correct page
                    28 Jun 2010 - mcdurdin - I2444 - Keyman tries to re-init because OSK was not unregistering until after Keyman_Exit
                    11 Jan 2011 - mcdurdin - I2633 - Wrong icons in toolbar
                    11 Jan 2011 - mcdurdin - I2287 - Keyman menu links select incorrect page when opening OSK
                    11 Jan 2011 - mcdurdin - I1867 - Sort keyboard icons alphabetically
                    31 Jan 2011 - mcdurdin - I2688 - Resizing did not scale correctly when form was partially off-screen
                    31 Jan 2011 - mcdurdin - I2683 - Extra keyboards menu does not return focus correctly
                    31 Jan 2011 - mcdurdin - I2692 - Ensure OSK closes before main form is destroyed to prevent errors
                    18 Feb 2011 - mcdurdin - I2721 - Override Javascript-disabled security for web controls
                    28 Feb 2011 - mcdurdin - I2398 - After reloading configuration, OSK does not show active keyboard in toolbar
                    18 Mar 2011 - mcdurdin - I2820 - avoid crash with multiple monitors
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3309 - V9.0 - Migrate to Delphi XE2, VS2010, svn 1.7
                    24 Jan 2012 - mcdurdin - I3213 - Crash when moving OSK and Ctrl+Alt+Del or Win+L pressed
                    04 Nov 2012 - mcdurdin - I3517 - V9.0 - Merge of I3213 - Crash when Keyman menu is visible and Ctrl+Alt+Del or Win+L pressed
                    01 Dec 2012 - mcdurdin - I3614 - V9.0 - Start removal of keyboard selection list from UI
                    01 Jan 2013 - mcdurdin - I3677 - V9.0 - OSK and Keyman menu show an entry in taskbar
                    24 Oct 2013 - mcdurdin - I3933 - V9.0 - Keyman tray icon menu is not showing installed keyboards
                    07 Nov 2013 - mcdurdin - I3949 - V9.0 - Keyboard selection and notification needs consolidation
                    11 Nov 2013 - mcdurdin - I3962 - V9.0 - OSK Toolbar needs cleanup for v9.0
                    29 Nov 2013 - mcdurdin - I3990 - V9.0 - Keyman keyboards menu should be used on OSK toolbar
                    28 Feb 2014 - mcdurdin - I4098 - V9.0 - OSK is still 8.0 style
                    01 May 2014 - mcdurdin - I4208 - V9.0 - Font helper and Keyboard usage appear outside frame in Win 8
                    02 May 2014 - mcdurdin - I4225 - V9.0- Opening font helper or keyboard usage from Keyman menu on Win 8 still shows HTML outside window
                    28 May 2014 - mcdurdin - I4242 - Crash when OSK closed/reopened without dismissing hint window [CrashID:keyman.exe_9.0.449.0_2C405C5D_EInvalidPointer]
                    03 Aug 2014 - mcdurdin - I4359 - V9.0 - OSK shows wrong base keyboard and doesn't refresh
                    12 Aug 2014 - mcdurdin - I4360 - V9.0 - When OSK opens, if Keyman is off then icon shows wrongly
                    28 Aug 2014 - mcdurdin - I4390 - V9.0 - Free vs Pro
                    01 Sep 2014 - mcdurdin - I4393 - V9.0 - Keyman Desktop Free Edition polish
                    11 Feb 2015 - mcdurdin - I4593 - V9.0 - The keyboard usage page can appear outside the OSK in some situations
                    04 Mar 2015 - mcdurdin - I4606 - V9.0 - Support single keyboard buttons on OSK toolbar
                    05 Mar 2015 - mcdurdin - I4606 - V9.0 - Support single keyboard buttons on OSK toolbar
                    10 Aug 2015 - mcdurdin - I4849 - Keyman Desktop title in OSK has wrong grey background
*)
unit UfrmVisualKeyboard;  // I3306   // I3614

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, ActiveX, keymanapi_TLB, utilstr, custinterfaces,
  OleCtrls, SHDocVw, ImgList, UfrmOSKPlugInBase,
  UfrmOSKOnScreenKeyboard, UfrmOSKCharacterMap, UfrmOSKKeyboardUsage,
  UfrmOSKEntryHelper,
  Buttons, ToolWin, PaintPanel, exImageList,
  Menu_KeyboardItems, ComCtrls, StdCtrls, EmbeddedWB,
  UfrmOSKFontHelper, LangSwitchManager,
  WebBrowserManager, UserMessages, SHDocVw_EWB, EwbCore,
  KeymanEmbeddedWB, System.ImageList;

type
  TOSKActivePage = (apKeyboard, apCharacterMap, apEntryHelper, apKeyboardUsage, apFontHelper, apUndefined);

  TKeymanToolButton = class(TToolButton)   // I4606
  private
    FKeyboardName, FCmdLine: string;
  public
    property KeyboardName: string read FKeyboardName write FKeyboardName;
    property CmdLine: string read FCmdLine write FCmdLine;
  end;

  TKeymanCustomisationMenuItem_Clone = class(TInterfacedObject, IKeymanCustomisationMenuItem)
  public
    function Get_Action: TOleEnum; safecall;
    function Get_Caption: WideString; safecall;
    function Get_CmdLine: WideString; safecall;
    function Get_Font: IFontDisp; safecall;
    function Get_Hotkey: Word; safecall;
    function Get_Index: Integer; safecall;
    function Get_Icon: IPictureDisp; safecall;
    function Get_ItemType: TOleEnum; safecall;
    function Get_Location: TOleEnum; safecall;
    function Get_PictureDefault: IPictureDisp; safecall;
    function Get_PictureSelected: IPictureDisp; safecall;
  end;

  TfrmVisualKeyboard = class(TForm)
    tmrFade: TTimer;
    panCharMap: TPanel;
    panKeyboard: TPanel;
    panEntryHelper: TPanel;
    ilToolbar: TexImageList;
    status: TStatusBar;
    tmrStatus: TTimer;
    panContent: TPanel;
    panTop: TPaintPanel;
    panTitle: TPaintPanel;
    tbLeft: TToolBar;
    tbRight: TToolBar;
    mnuPopup: TPopupMenu;
    ilKeyboards: TexImageList;
    panKeyboardUsage: TPanel;
    panHint: TPanel;
    webHint: TKeymanEmbeddedWB;  // I2721
    panFontHelper: TPanel;
    tbKeyboards: TToolBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure tmrFadeTimer(Sender: TObject);
    procedure tmrStatusTimer(Sender: TObject);
    procedure panTopPaint(Sender: TObject);
    procedure tbRightAdvancedCustomDraw(Sender: TToolBar; const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure mnuPopupPopup(Sender: TObject);
    procedure panTopResize(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure BtnShowHint(Sender: TObject);
    procedure BtnHideHint(Sender: TObject);
    function FormHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
    procedure TitleMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TitleMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TitleMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure panTitlePaint(Sender: TObject);   // I4849
  private
    webHintManager: TWebBrowserManager;
    btnKeyboardHelp: TToolButton;
    cmi: IKeymanCustomisationMenuItem;
    FFadeVisualKeyboard: Boolean;

    FUnicode: Boolean;

    FOnScreenKeyboard: TfrmOSKOnScreenKeyboard;
    FCharacterMap: TfrmOSKCharacterMap;
    FEntryHelper: TfrmOSKEntryHelper;
    FKeyboardUsage: TfrmOSKKeyboardUsage;
    FFontHelper: TfrmOSKFontHelper;
    SizeMoveStartPos: TPoint;
    SizeMoveStartRect: TRect;
    FNCButtonIsDown: Boolean;

    FToolbarHintWindow: THintWindow;
    FUpdatingToolbar: Boolean;
    FActivePageSet: Boolean;
    FRegistered: Boolean;
    FKeyboardMenuButton: TToolButton;   // I3962
    FLastTotalKeyboards: Integer;   // I4606

    procedure ResetShiftStates;  // I1144

    procedure LoadSettings;
    procedure SaveSettings;

    { Moving and sizing }
    procedure WMEnterSizeMove(var Message: TMessage); message WM_ENTERSIZEMOVE;
    procedure WMMoving(var Message: TMessage); message WM_MOVING;
    procedure WMSizing(var Message: TMessage); message WM_SIZING;
    procedure WMExitSizeMove(var Message: TMessage); message WM_EXITSIZEMOVE;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure UpdateConstraints(Resize: Boolean = False);


    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;

    procedure WMNCLButtonDown(var Message: TMessage); message WM_NCLBUTTONDOWN;

    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;

    procedure WMUserFormShown(var Message: TMessage); message WM_USER_FormShown;   // I4225

    procedure UpdatePanels;
    procedure SetActivePage(Page: TOSKActivePage);
    function GetActivePage: TOSKActivePage;
    function ActivePageForm: TfrmOSKPlugInBase;
    procedure LoadCharacterMap;
    procedure LoadEntryHelper;
    procedure LoadKeyboard;
    procedure LoadFontHelper;

    procedure RefreshToolbar;
    procedure LoadKeyboardUsage;
    procedure UpdateActivePage;
    procedure LoadHintSettings;
    procedure ToggleHintPanel;
    procedure webHintFireCommand(Sender: TObject; const command: WideString;
      params: TStringList);
    procedure webHintResize(Sender: TObject; X, Y: Integer);
    procedure CreateWebHintManager;
    procedure AdjustToClosestMonitor(var x, y, cx, cy: Integer);

    procedure KeyboardMenuClick(Sender: TObject);   // I3962
    function IsHintBarEnabled: Boolean;   // I4390
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SetTopMost(ATopMost: Boolean);   // I4208   // I4593

    procedure RefreshKeyman;
    procedure PreRefreshKeyman;
    procedure UpdateToolbar;
    procedure RefreshSelectedKeyboard;  // I2398

    procedure Unregister;
    procedure Register;

    function GetToolbarPositionXML: WideString;

    function IsFading: Boolean;
    function IsToolbarVisible: Boolean;
    function IsSaveEnabled: Boolean;
    function IsPrintEnabled: Boolean;
    procedure MnuOSK_Fade(Sender: TObject);
    procedure MnuOSK_ToggleToolbar(Sender: TObject);
    procedure MnuOSK_SaveAsWebPage(Sender: TObject);
    procedure MnuOSK_Print(Sender: TObject);

    procedure MnuOSK_ViewKeyboard(Sender: TObject);
    procedure MnuOSK_ViewCharMap(Sender: TObject);
    procedure MnuOSK_ViewEntryHelper(Sender: TObject);
    procedure MnuOSK_ViewKeyboardUsage(Sender: TObject);
    procedure MnuOSK_ViewFontHelper(Sender: TObject);

    procedure MnuOSK_Close(Sender: TObject);

    procedure BtnHelpClick(Sender: TObject);

    procedure UpdateKeyboardIcon;   // I3962

    property ActivePage: TOSKActivePage read GetActivePage write SetActivePage;
  end;

implementation

uses
  CommCtrl,
  GraphUtil,
  KeymanHints,
  Hints,
  HintConsts,
  InterfaceHotkeys,
  klog,
  kmint,
  Math,
  psapi,
  ErrorControlledRegistry,
  KeymanDesktopShell,
  RegistryKeys,
  LangSwitchUtils,
  UfrmHelp,
  UfrmKeyman7Main,
  UnicodeData,
  utildir,
  utilhotkey,
  utilxml,
  VistaMessages,
  MessageIdentifierConsts,
  messageidentifiers,
  BitmapIPicture, Types;

{$R *.DFM}

const
  WS_EX_NOACTIVATE: DWord = $08000000;
  SnapThreshold = 32;

procedure TfrmVisualKeyboard.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  ResetShiftStates; // I1144
  Action := caFree;
end;

type
  TOSKHintWindow = class(THintWindow)
  end;

procedure TfrmVisualKeyboard.webHintResize(Sender: TObject; X, Y: Integer);
begin
  panHint.Height := Y + 2;
end;

procedure TfrmVisualKeyboard.CreateWebHintManager;
begin
  if Assigned(webHintManager) then Exit;

  webHintManager := TWebBrowserManager.Create(Self);  // I1256 - hint system
  webHintManager.WebBrowser := webHint;
  webHintManager.OnFireCommand := webHintFireCommand;
  webHintManager.OnResize := webHintResize;
  webHintManager.RenderTemplate := 'VKHints.xsl';
end;

procedure TfrmVisualKeyboard.FormCreate(Sender: TObject);
begin
  inherited;

  CreateWebHintManager;

  FToolbarHintWindow := TOSKHintWindow.Create(Self);
  FToolbarHintWindow.Color := clInfoBk;
  FToolbarHintWindow.Font.Color := clInfoText;

  // frmKeyman7Main.frmVisualKeyboard := Self; // I958 - unnecessary crossreference

  ilToolbar.InitializeImageList;
  ilKeyboards.InitializeImageList;
  ScreenSnap := True;
  SnapBuffer := 8;

  cmi := TKeymanCustomisationMenuItem_Clone.Create;
  Register;  // I2444 - Keyman tries to re-init because OSK was not unregistering until after Keyman_Exit

  FUnicode := True;

  RefreshToolbar;
  UpdateKeyboardIcon;   // I4360
end;

procedure TfrmVisualKeyboard.RefreshToolbar;
var
  i: Integer;
  btn: TKeymanToolButton;
  FBitmap: TBitmapIPicture;
  FMaskBitmap: TBitmap;
  DesiredTarget: KeymanHotkeyTarget;
  FShortCut: WideString;
  j: Integer;
  tb: TToolbar;
  x: Integer;
  s: WideString;
  WidthSet: Boolean;
begin
  if FLastTotalKeyboards = frmKeyman7Main.LangSwitchManager.TotalKeyboardCount then   // I4606
    Exit;

  FLastTotalKeyboards := frmKeyman7Main.LangSwitchManager.TotalKeyboardCount;   // I4606

  SendMessage(panTop.Handle, WM_SETREDRAW, 0, 0);
  try
    for i := tbLeft.ButtonCount - 1 downto 0 do
      tbLeft.Buttons[i].Free;
    for i := tbRight.ButtonCount - 1 downto 0 do
      tbRight.Buttons[i].Free;
    for i := tbKeyboards.ButtonCount - 1 downto 0 do   // I4606
      tbKeyboards.Buttons[i].Free;

    btnKeyboardHelp := nil;

    WidthSet := False;

    x := 0;
    tb := tbLeft;
    with kmint.KeymanCustomisation do
    begin
      for i := 1 to CustMenuItems.Count do {$MESSAGE HINT 'This indexing needs to be sorted out'}
      begin
        if CustMenuItems.Items[i].Location <> milOSKToolbar then Continue;

        { Character Map Test }

        if (CustMenuItems.Items[i].Action = miaOSK_ViewCharMap) and
          (not TfrmOSKCharacterMap.IsCharMapDataInstalled or
          not TfrmOSKCharacterMap.IsCharMapAvailable) then Continue;

        { Text Entry Helper Test }

        if (CustMenuItems.Items[i].Action = miaOSK_ViewEntryHelper) then Continue; 

        if (CustMenuItems.Items[i].Action = miaSelectKeyboard) then  // I4606
        begin
          if (CustMenuItems.Items[i].CmdLine <> '') and (CustMenuItems.Items[i].CmdLine <> '-') and
              (frmKeyman7Main.GetLangSwitchKeyboard(CustMenuItems.Items[i].CmdLine, False) = nil) then
            Continue;
        end;


        case CustMenuItems.Items[i].ItemType of
          _mitKeyboardsList:   // I3962
            begin
              tbLeft.Perform(TB_AUTOSIZE, 0, 0);

              FKeyboardMenuButton := TToolButton.Create(Self);
              FKeyboardMenuButton.Left := x;
              FKeyboardMenuButton.Width := 23;
              tbKeyboards.InsertControl(FKeyboardMenuButton);
              //btn.Tag := CustMenuItems.Items[i].Action;
              //btn.ShowHint := True;
              //FKeyboardMenuButton.OnMouseEnter := BtnShowKeyboardHint;
              //FKeyboardMenuButton.OnMouseLeave := BtnHideKeyboardHint;
              FKeyboardMenuButton.OnClick := KeyboardMenuClick;
              x := 0;
              tb := tbRight;
            end;
          mitText:
            begin
              btn := TKeymanToolButton.Create(Self);   // I4606
              btn.Left := x;
              tb.InsertControl(btn);
              Inc(x, btn.Width);
              btn.Width := 23;
              btn.Tag := CustMenuItems.Items[i].Action;
              //btn.ShowHint := True;
              btn.OnMouseEnter := BtnShowHint;
              btn.OnMouseLeave := BtnHideHint;

              DesiredTarget := $FFFFFFFF;
              case CustMenuItems.Items[i].Action of
                miaOSK_ViewKeyboard, miaOSK_ViewCharMap,
                miaOSK_ViewEntryHelper, miaOSK_ViewKeyboardUsage,  // I1373,I1374 - keyboard usage and font helper
                miaOSK_ViewFontHelper:
                  begin
                    btn.Style := tbsCheck;
                    btn.Grouped := True;
                  end;
                miaOSK_Fade:
                  btn.Style := tbsCheck;
                miaOpenKeymanConfiguration: DesiredTarget := khKeymanConfiguration;
                miaToggleVisualKeyboard: DesiredTarget := khVisualKeyboard;
              end;

              case CustMenuItems.Items[i].Action of
                miaOpenKeymanConfiguration: btn.OnClick := frmKeyman7Main.MnuOpenKeymanConfiguration;
                miaOpenProductHelp: btn.OnClick := BtnHelpClick;
                miaOpenKeyboardHelp:
                  begin
                    btn.Enabled := frmKeyman7Main.ActiveKeymanID >= 0;
                    btn.OnClick := frmKeyman7Main.MnuOpenKeyboardHelp;
                    btnKeyboardHelp := btn;
                  end;
                miaOpenProductAbout: btn.OnClick := frmKeyman7Main.MnuOpenProductAbout;
                miaToggleVisualKeyboard:
                  begin
                    btn.OnClick := frmKeyman7Main.MnuVisualKeyboard;
                  end;
                miaExitProduct: btn.OnClick := frmKeyman7Main.MnuExitKeyman;
                miaRunProgram:
                  begin
                    btn.OnClick := frmKeyman7Main.MnuRunProgram;
                    btn.CmdLine := CustMenuItems.Items[i].CmdLine;   // I4606
                  end;
              end;

              case CustMenuItems.Items[i].Action of
                miaOSK_Fade:
                  begin
                    btn.Enabled := (GetVersion and $80000000) = 0;
                    btn.OnClick := MnuOSK_Fade;
                    btn.Down := IsFading;
                  end;
                miaOSK_ToggleToolbar:
                  begin
                    btn.OnClick := MnuOSK_ToggleToolbar;
                    btn.Down := IsToolbarVisible;
                  end;
                miaOSK_SaveAsWebPage:
                  begin
                    btn.OnClick := MnuOSK_SaveAsWebPage;
                    btn.Enabled := IsSaveEnabled;
                  end;
                miaOSK_Print:
                  begin
                    btn.OnClick := MnuOSK_Print;
                    btn.Enabled := IsPrintEnabled;
                  end;
                miaOSK_ViewKeyboard:
                  begin
                    btn.OnClick := MnuOSK_ViewKeyboard;
                    btn.Down := ActivePage = apKeyboard;
                  end;
                miaOSK_ViewCharMap:
                  begin
                    btn.OnClick := MnuOSK_ViewCharMap;
                    btn.Enabled := TfrmOSKCharacterMap.IsCharMapAvailable;
                    btn.Down := ActivePage = apCharacterMap;
                  end;
                miaOSK_ViewEntryHelper:
                  begin
                    btn.OnClick := MnuOSK_ViewEntryHelper;
                    btn.Down := ActivePage = apEntryHelper;
                  end;
                miaOSK_ViewKeyboardUsage:
                  begin
                    btn.OnClick := MnuOSK_ViewKeyboardUsage;
                    btn.Down := ActivePage = apKeyboardUsage;
                  end;
                miaOSK_ViewFontHelper:
                  begin
                    btn.OnClick := MnuOSK_ViewFontHelper;
                    btn.Down := ActivePage = apFontHelper;
                  end;
                miaOSK_Close: btn.OnClick := MnuOSK_Close;
                miaSelectKeyboard:   // I4606
                  begin
                    btn.OnClick := frmKeyman7Main.MnuSelectKeyboard;
                    btn.KeyboardName := CustMenuItems.Items[i].CmdLine;
                  end;
              end;

              FShortCut := '';
              for j := 0 to kmcom.Hotkeys.Count - 1 do
                if (kmcom.Hotkeys[j].Target = DesiredTarget) and (kmcom.Hotkeys[j].VirtualKey <> 0) then
                begin
                  FShortCut := ' ('+ShortCutToTextEx(HotkeyToShortcut(kmcom.Hotkeys[j]))+')';
                  Break;
                end;

              s := MsgFromStr(':String[@Caption='''+xmlencode(StripHotkey(CustMenuItems.Items[i].Caption))+''']');
              if s = ''
                then btn.Hint := StripHotkey(CustMenuItems.Items[i].Caption) + FShortCut
                else btn.Hint := s + FShortCut;

              //btn.Hint := WideStripHotkey(CustMenuItems.Items[i].Caption) + FShortCut;

              FBitmap := TBitmapIPicture.Create;
              try
                FBitmap.LoadFromIPicture(CustMenuItems.Items[i].Icon as IPicture);
                if not FBitmap.Empty then
                begin
                  FMaskBitmap := TBitmap.Create;
                  try
                    if not WidthSet then  // I2633
                    begin
                      WidthSet := True;
                      ilToolbar.Width := FBitmap.Width;
                      ilToolbar.Height := FBitmap.Height;
                      tb.ButtonWidth := Max(tb.ButtonWidth, FBitmap.Width + 7);
                      tb.ButtonHeight := Max(tb.ButtonHeight, FBitmap.Height + 6);
                      if FBitmap.Height > 16 then
                      begin
                        tb.Margins.Top := 1;
                        tb.Margins.Bottom := 0;
                      end;
                    end;

                    FMaskBitmap.Assign(FBitmap);
                    FMaskBitmap.Mask(FBitmap.TransparentColor);

                    if (FBitmap.Width = ilToolbar.Width) and
                       (FBitmap.Height = ilToolbar.Height) then
                      btn.ImageIndex := ilToolbar.Add(FBitmap, FMaskBitmap);
                  finally
                    FMaskBitmap.Free;
                  end;
                end;
              finally
                FBitmap.Free;
              end;
            end;
          mitSeparator:
            begin
              btn := TKeymanToolButton.Create(Self);   // I4606
              btn.Style := tbsSeparator;
              btn.Left := x;
              btn.Width := 8;
              btn.OnMouseEnter := BtnShowHint;
              btn.OnMouseLeave := BtnHideHint;
              tb.InsertControl(btn);
              Inc(x, 8); //btn.Width);
            end;
        end;
      end;
    end;
  finally
    SendMessage(panTop.Handle, WM_SETREDRAW, 1, 0);
    InvalidateRect(panTop.Handle, nil, FALSE);   // I4606
    InvalidateRect(tbLeft.Handle, nil, FALSE);   // I4606
    InvalidateRect(tbRight.Handle, nil, FALSE);   // I4606
    InvalidateRect(tbKeyboards.Handle, nil, FALSE);   // I4606
    panTitle.Invalidate;   // I4606
  end;

  tbRight.Perform(TB_AUTOSIZE, 0, 0);

  panTitle.Caption := kmint.KeymanCustomisation.CustMessages.MessageFromID('SKShortApplicationTitle');
  s := kmint.KeymanCustomisation.CustMessages.MessageFromID('SK_UIFontName');
  if s <> '' then
  begin
    panTitle.Font.Name := s;
    FToolbarHintWindow.Font.Name := s;
    FToolbarHintWindow.Canvas.Font.Name := s;
  end;
  Canvas.Font := panTitle.Font;
  panTitle.Width := Canvas.TextWidth(panTitle.Caption);

  UpdateConstraints;

  UpdateToolbar;
end;

procedure TfrmVisualKeyboard.ResetShiftStates;
begin
  if Assigned(FOnScreenKeyboard) then
    FOnScreenKeyboard.ResetShiftStates;  // I1144
end;

procedure TfrmVisualKeyboard.FormDeactivate(Sender: TObject);
begin
  if ActivePageForm <> nil then
    ActivePageForm.Deactivating;
end;

procedure TfrmVisualKeyboard.FormDestroy(Sender: TObject);
begin
  Unregister; // I2444 - Keyman tries to re-init because OSK was not unregistering until after Keyman_Exit

  SaveSettings;

  FreeAndNil(FOnScreenKeyboard);
  FreeAndNil(FCharacterMap);
  FreeAndNil(FEntryHelper);
  FreeAndNil(FKeyboardUsage);
  FreeAndNil(FFontHelper);

  cmi := nil;

  Application.OnMessage := nil;
end;

procedure TfrmVisualKeyboard.Register; // I2444 - Keyman tries to re-init because OSK was not unregistering until after Keyman_Exit
begin
  if not FRegistered then // I2692
    kmint.KeymanEngineControl.RegisterControllerWindow(Handle);
  FRegistered := True;
end;

procedure TfrmVisualKeyboard.Unregister; // I2444 - Keyman tries to re-init because OSK was not unregistering until after Keyman_Exit
begin
  if FRegistered then // I2692
    kmint.KeymanEngineControl.UnregisterControllerWindow(Handle);
  FRegistered := False;
end;

function TfrmVisualKeyboard.FormHelp(Command: Word; Data: NativeInt; var CallHelp: Boolean): Boolean;
begin
  CallHelp := False;
  Result := True;

  if Command in [HELP_CONTEXT, HELP_CONTEXTPOPUP, HELP_INDEX, HELP_CONTENTS] then
  begin
    case ActivePage of
      apKeyboard: TKeymanDesktopShell.OpenHelp('context_onscreenkeyboard');
      apCharacterMap: TKeymanDesktopShell.OpenHelp('context_charactermap');
      apEntryHelper: TKeymanDesktopShell.OpenHelp('context_entryhelper');
    end;
  end;
end;

procedure TfrmVisualKeyboard.FormResize(Sender: TObject);
begin
  Update;
end;

procedure TfrmVisualKeyboard.SetTopMost(ATopMost: Boolean);   // I4208
begin
  if ATopMost
    then SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_NOREDRAW or SWP_NOACTIVATE or SWP_NOSENDCHANGING or SWP_NOREPOSITION)
    else SetWindowPos(Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_NOREDRAW or SWP_NOACTIVATE or SWP_NOSENDCHANGING or SWP_NOREPOSITION)
end;

procedure TfrmVisualKeyboard.FormShow(Sender: TObject);
begin
  LoadSettings;
  //if Assigned(FOnScreenKeyboard) then FOnScreenKeyboard.ConstrainSizing(WMSZ_LEFT, );
  UpdateConstraints;
  if not FActivePageSet then  // I2287
    UpdateActivePage;  // I1375 - Switch to correct page automatically

  PostMessage(Handle, WM_USER_FormShown, 0, 0);
end;


function TfrmVisualKeyboard.IsFading: Boolean;
begin
  Result := FFadeVisualKeyboard;
end;

function TfrmVisualKeyboard.IsPrintEnabled: Boolean;
begin
  Result := (ActivePage = apKeyboard) and FOnScreenKeyboard.HasVisualKeyboard;
end;

function TfrmVisualKeyboard.IsSaveEnabled: Boolean;
begin
  Result := (ActivePage = apKeyboard) and FOnScreenKeyboard.HasVisualKeyboard;
end;

function TfrmVisualKeyboard.IsToolbarVisible: Boolean;
begin
  Result := panTop.Visible;
end;

procedure TfrmVisualKeyboard.KeyboardMenuClick(Sender: TObject);   // I3962   // I3990
var
  FRect: TRect;
begin
  FRect.TopLeft := FKeyboardMenuButton.ClientToScreen(Point(0, 0));
  FRect.Top := Top;
  FRect.BottomRight := FKeyboardMenuButton.ClientToScreen(Point(FKeyboardMenuButton.ClientWidth,
    FKeyboardMenuButton.ClientHeight));
  frmKeyman7Main.ShowMenu(nil, milLeft, False, FRect);
end;

procedure TfrmVisualKeyboard.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or (WS_EX_NOACTIVATE or WS_EX_TOOLWINDOW);   // I3677   // I4208
end;

function TfrmVisualKeyboard.IsHintBarEnabled: Boolean;   // I4390
begin
  Result := IsHintEnabled(KH_OSKHINTBAR);
end;

procedure TfrmVisualKeyboard.LoadHintSettings;  // I1256 - Hint System
begin
  if not webHintManager.TemplateExists then
  begin
    if panHint.Visible then ToggleHintPanel;
  end
  else if (IsHintBarEnabled <> panHint.Visible) then
  begin
    ToggleHintPanel;
  end;
end;

procedure TfrmVisualKeyboard.ToggleHintPanel;   // I1256 - Hint System
begin
  if panHint.Visible then
  begin
    panHint.Visible := False;
    UpdateConstraints;
    SetBounds(Left, Top+panHint.Height, Width, Height-panHint.Height);
  end
  else
  begin
    panHint.Visible := True;
    SetBounds(Left, Top-panHint.Height, Width, Height+panHint.Height);
    UpdateConstraints;
    webHintManager.Content_Render;
  end;
end;

procedure TfrmVisualKeyboard.TitleMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
  Messages: array[TMouseButton] of UINT = (WM_NCLBUTTONDOWN, WM_NCRBUTTONDOWN, WM_NCMBUTTONDOWN);
begin
  // I1378 - allow dragging of OSK anywhere on title (i.e. toolbar)
  if Button = mbLeft then
  begin
    ReleaseCapture;
    SendMessage(Handle, WM_SYSCOMMAND, SC_MOVE + 2, 0);
  end;
end;

procedure TfrmVisualKeyboard.TitleMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  // I1378 - allow dragging of OSK anywhere on title (i.e. toolbar)
  with (Sender as TWinControl).ClientToScreen(Point(X,Y)) do
    Perform(WM_NCMOUSEMOVE, HTCAPTION, MAKELONG(X, Y));
end;

procedure TfrmVisualKeyboard.TitleMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
  Messages: array[TMouseButton] of UINT = (WM_NCLBUTTONUP, WM_NCRBUTTONUP, WM_NCMBUTTONUP);
begin
  // I1378 - allow dragging of OSK anywhere on title (i.e. toolbar)
  with (Sender as TWinControl).ClientToScreen(Point(X,Y)) do
    Perform(Messages[Button], HTCAPTION, MAKELONG(X, Y));
end;

procedure TfrmVisualKeyboard.tbRightAdvancedCustomDraw(Sender: TToolBar;
  const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  if Stage = cdPrePaint then
    GradientFillCanvas(Sender.Canvas, $DAC379, $A8975E, {C1D3D9, $ABBFC6,} Sender.ClientRect, gdVertical);  // I2633   // I4098
end;

{ Fading and unfading }

procedure TfrmVisualKeyboard.tmrFadeTimer(Sender: TObject);
begin
  if AlphaBlendValue <= 150 then
  begin
    AlphaBlendValue := 150;
    tmrFade.Enabled := False;
  end;
  AlphaBlendValue := AlphaBlendValue - 70;
end;

procedure TfrmVisualKeyboard.tmrStatusTimer(Sender: TObject);
var
  rect: TRect;
  pt: TPoint;
begin
  if not IsFading then Exit;

  if FNCButtonIsDown then
  begin
    // Sizing or moving
    AlphaBlendValue := 255;
    Exit;
  end;

  GetCursorPos(pt);
  GetWindowRect(Handle, rect);
  if not PtInRect(rect, pt) then
  begin
    if (AlphaBlendValue = 255) and not tmrFade.Enabled then
      tmrFade.Enabled := True;
  end
  else
  begin
    AlphaBlendValue := 255;
  end;

  
{var
  buf: array[0..64] of char;
  pid: DWord;
begin
  status.Panels[0].Text := IntToHex(kmcom.Control.LastActiveWindow, 8);
  status.Panels[2].Text := IntToHex(kmcom.Control.LastFocusWindow, 8);

  buf[0] := #0;
  if (SendMessage(kmcom.Control.LastActiveWindow, WM_GETTEXT, 64, Integer(PChar(@buf[0]))) > 0) or (GetLastError = ERROR_SUCCESS)
    then status.Panels[1].Text := buf
    else status.Panels[1].Text := SysErrorMessage(GetLastError);

  buf[0] := #0;
  if (SendMessage(kmcom.Control.LastFocusWindow, WM_GETTEXT, 64, Integer(PChar(@buf[0]))) > 0) or (GetLastError = ERROR_SUCCESS)
    then status.Panels[3].Text := buf
    else status.Panels[3].Text := SysErrorMessage(GetLastError);

  GetWindowThreadProcessId(kmcom.Control.LastFocusWindow, @pid);
  status.Panels[4].Text := IntToStr(FActiveKeymanID); //IntToHex(GetCurrentProcessId, 8) + ' <> '+ IntToHex(pid, 8);
}
end;

procedure TfrmVisualKeyboard.WMMouseActivate(var Message: TWMMouseActivate);
begin
  Message.Result := MA_NOACTIVATE;
end;

{ Sizing and moving }

procedure TfrmVisualKeyboard.WMEnterSizeMove(var Message: TMessage);
begin
  try
    SizeMoveStartPos := Mouse.CursorPos;
  except
    SizeMoveStartPos := Point(0,0);
  end;
  SizeMoveStartRect := BoundsRect;
  inherited;
end;

procedure TfrmVisualKeyboard.WMMoving(var Message: TMessage);
var
  p: PRect;
  w: Integer;
  h: Integer;
  pt: TPoint;
begin
  FNCButtonIsDown := True;

  p := PRect(Message.LParam);
  p^ := SizeMoveStartRect;
  if not GetCursorPos(pt) then Exit;  // I3213   // I3517
  OffsetRect(p^, pt.X - SizeMoveStartPos.X, pt.Y - SizeMoveStartPos.Y);
  with p^ do
  begin
    w := Right-Left; h := Bottom-Top;

    AdjustToClosestMonitor(Left, Top, w, h);

    Right := Left + w;
    Bottom := Top + h;

    SetBounds(Left, Top, w, h);
  end;
  Message.Result := 1;
end;

procedure TfrmVisualKeyboard.WMSizing(var Message: TMessage);
var
  p: PRect;
  frm: TfrmOSKPlugInBase;
begin
  FNCButtonIsDown := True;

  { Keep proportions correct for keyboard sizing? }
  p := PRect(Message.LParam);

  frm := ActivePageForm;
  if Assigned(frm) then
    frm.ConstrainSizing(Message.WParam, p^);

  with p^ do
  begin
    { Min constraints and keep on screen }
    case Message.wParam of
      WMSZ_TOP,
      WMSZ_LEFT,
      WMSZ_TOPLEFT,
      WMSZ_BOTTOMLEFT:
        begin
          if Right-Left < Constraints.MinWidth then
            Left := Right - Constraints.MinWidth;
          {if Right > Screen.DesktopRect.Right then  // I2688
            Left := Screen.DesktopRect.Right - (Right-Left);}
        end;
      WMSZ_BOTTOM,
      WMSZ_RIGHT,
      WMSZ_TOPRIGHT,
      WMSZ_BOTTOMRIGHT:
        if Right-Left < Constraints.MinWidth then
          Right := Left + Constraints.MinWidth;
    end;

    case Message.wParam of
      WMSZ_LEFT,
      WMSZ_TOP,
      WMSZ_TOPLEFT,
      WMSZ_TOPRIGHT:
        if Bottom-Top < Constraints.MinHeight then
          Top := Bottom - Constraints.MinHeight;
      WMSZ_RIGHT,
      WMSZ_BOTTOM,
      WMSZ_BOTTOMLEFT,
      WMSZ_BOTTOMRIGHT:
        if Bottom-Top < Constraints.MinHeight then
          Bottom := Top + Constraints.MinHeight;
    end;

    { Snapping and keep on screen }

{    if Right > Screen.DesktopRect.Right then
    begin

    end;}

    SetBounds(Left, Top, Right-Left, Bottom-Top);
  end;
  Message.Result := 1;
end;

procedure TfrmVisualKeyboard.WMUserFormShown(var Message: TMessage);   // I4225
begin
  SetTopMost(TRUE);   // I4208
end;

procedure TfrmVisualKeyboard.WMExitSizeMove(var Message: TMessage);
begin
  FNCButtonIsDown := False;
  inherited;
end;

procedure TfrmVisualKeyboard.AdjustToClosestMonitor(var x, y, cx, cy: Integer);
var
  m: TMonitor;
  r: TRect;
begin
  m := Screen.MonitorFromRect(Rect(x, y, x+cx, y+cy), mdNearest);
  if not Assigned(m) then m := Screen.PrimaryMonitor;
  if Assigned(m) // I2820 preventative work - didn't actually crash here but similar scenario
    then r := m.WorkareaRect
    else r := Screen.WorkAreaRect;

  { Snap to borders - right }
  if Abs(x + cx - r.Right) < SnapThreshold then
  begin
    x := r.Right - cx;
  end;
  { Snap to borders - bottom }
  if Abs(y + cy - r.Bottom) < SnapThreshold then
  begin
    y := r.Bottom - cy;
  end;
  { Snap to borders - left }
  if Abs(x - r.Left) < SnapThreshold then
  begin
    x := r.Left;
  end;
  { Snap to borders - bottom }
  if Abs(y - r.Top) < SnapThreshold then
  begin
    y := r.Top;
  end;
end;

procedure TfrmVisualKeyboard.WMWindowPosChanging(
  var Message: TWMWindowPosChanging);
begin
  with Message.WindowPos^ do
  begin
    // I1720 : Fix taskbar positioning and just stick to edges, don't prevent moving off screen.
    if (flags and (SWP_NOSIZE or SWP_NOMOVE)) <> 0 then
      AdjustToClosestMonitor(x, y, cx, cy);
  end;
end;

procedure TfrmVisualKeyboard.UpdateConstraints(Resize: Boolean);
  function GetCWidth(Control: TControl): Integer;
  begin
    if Control.AlignWithMargins and (Control.Align <> alNone)
      then Result := Control.Width + Control.Margins.Left + Control.Margins.Right
      else Result := Control.Width;
  end;
var
  FNewWidth: Integer;
  frm: TfrmOSKPlugInBase;
  R: TRect;
begin
  if panTop.Visible then
  begin
    FNewWidth := 4 + GetCWidth(panTitle) + GetCWidth(tbLeft) + GetCWidth(tbRight);
    if Width < FNewWidth then
      SetBounds(Left+Width - FNewWidth, Top, FNewWidth, Height);

    R := Rect(Left, Top, Left+FNewWidth, Top + 4 + 2 + 64 + panTitle.Height);
    frm := ActivePageForm;
    if Assigned(frm) then
      frm.ConstrainSizing(WMSZ_TOPLEFT, R);
    Constraints.MinWidth := R.Right - R.Left;
    Constraints.MinHeight := R.Bottom - R.Top;
  end
  else
  begin
    FNewWidth := 120;

    R := Rect(Left, Top, Left + FNewWidth, Top + 4 + 2 + 64);
    frm := ActivePageForm;
    if Assigned(frm) then
      frm.ConstrainSizing(WMSZ_TOPLEFT, R);
    Constraints.MinWidth := R.Right - R.Left;
    Constraints.MinHeight := R.Bottom - R.Top;
  end;

  if Resize then
  begin
    R := BoundsRect;
    if Assigned(frm) then
      frm.ConstrainSizing(WMSZ_TOPLEFT, R);
    SetBounds(R.Left, R.Top, R.Right-R.Left, R.Bottom-R.Top);
  end;
end;

procedure TfrmVisualKeyboard.UpdateKeyboardIcon;   // I3962
  procedure UpdateButtons(kbd: TLangSwitchKeyboard; Toolbar: TToolbar);   // I4606
  var
    i: Integer;
    kb: TKeymanToolButton;
  begin
    for i := 0 to Toolbar.ButtonCount - 1 do
    begin
      kb := Toolbar.Buttons[i] as TKeymanToolButton;
      if kb.Tag <> miaSelectKeyboard then Continue;
      if not Assigned(kbd)
        then kb.Down := (kb.KeyboardName = '-') or (kb.KeyboardName = '')
        else kb.Down := SameText(kb.KeyboardName, kbd.Caption);
    end;
  end;
var
  kbd: TLangSwitchKeyboard;
  FBitmap: TBitmap;
  FIconText: WideString;
begin
  ilKeyboards.Clear;
  FIconText := '';
  kbd := frmKeyman7Main.LangSwitchManager.ActiveKeyboard;
  if Assigned(kbd) then
  begin
    if Assigned(kbd.Bitmap) then
    begin
      if Assigned(FKeyboardMenuButton) then
        FKeyboardMenuButton.ImageIndex := ilKeyboards.Add(kbd.Bitmap, nil);
    end
    else
      FIconText := kbd.Language.IconText;
  end
  else
    FIconText := '??';

  if FIconText <> '' then
  begin
    // Use the language's bitmap
    FBitmap := TBitmap.Create;
    try
      FBitmap.SetSize(16, 16);
      DrawLanguageIcon(FBitmap.Canvas, 0, 0, FIconText);
      if Assigned(FKeyboardMenuButton) then
        FKeyboardMenuButton.ImageIndex := ilKeyboards.Add(FBitmap, nil);
    finally
      FBitmap.Free;
    end;
  end;

  UpdateButtons(kbd, tbLeft);   // I4606
  UpdateButtons(kbd, tbRight);   // I4606
end;

procedure TfrmVisualKeyboard.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin

  if AWidth < Constraints.MinWidth then AWidth := Constraints.MinWidth;
  if AHeight < Constraints.MinHeight then AHeight := Constraints.MinHeight;

  AdjustToClosestMonitor(ALeft, ATop, AWidth, AHeight);

  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

{ End of sizing and moving }

procedure TfrmVisualKeyboard.WMNCHitTest(var Message: TWMNCHitTest);
const
  ht: array[0..4, 0..4] of Integer = (
    (HTTOPLEFT, HTTOPLEFT, HTTOP, HTTOPRIGHT, HTTOPRIGHT),
    (HTTOPLEFT, HTCLIENT, HTCLIENT, HTCLIENT, HTTOPRIGHT),
    (HTLEFT, HTCLIENT, HTCLIENT, HTCLIENT, HTRIGHT),
    (HTBOTTOMLEFT, HTCLIENT, HTCLIENT, HTCLIENT, HTBOTTOMRIGHT),
    (HTBOTTOMLEFT, HTBOTTOMLEFT, HTBOTTOM, HTBOTTOMRIGHT, HTBOTTOMRIGHT));
var
  x, y: Integer;
  pt: TPoint;
begin
  pt := ScreenToClient(Point(Message.XPos, Message.YPos));
  if (pt.X < 0) or (pt.X >= Width) or
    (pt.Y < 0) or (pt.Y >= Height) then
  begin
    Message.Result := Windows.HTNOWHERE;
    Exit;
  end;

  if pt.x < 2 then x := 0
  else if pt.x < 10 then x := 1
  else if pt.x >= Width - 2 then x := 4
  else if pt.x >= Width - 10 then x := 3
  else x := 2;

  if pt.y < 2 then y := 0
  else if pt.y < 10 then y := 1
  else if pt.y >= Height - 2 then y := 4
  else if pt.y >= Height - 10 then y := 3
  else y := 2;

  Message.Result := ht[y, x];
end;

procedure TfrmVisualKeyboard.WMNCLButtonDown(var Message: TMessage);
begin
  FNCButtonIsDown := True;
  inherited;
end;


procedure TfrmVisualKeyboard.UpdateToolbar;
begin
  if FUpdatingToolbar then Exit;
  FUpdatingToolbar := True;

  //KL.Log('TfrmVisualKeyboard.UpdateToolbar: '+IntToStr(FActiveKeymanID));

  if Assigned(btnKeyboardHelp) then btnKeyboardHelp.Enabled := False;

  FUpdatingToolbar := False;
end;

procedure TfrmVisualKeyboard.webHintFireCommand(Sender: TObject; const command: WideString;
  params: TStringList);
begin
  // I1256 - hint system

  if command = 'closehintbar' then
  begin
    DisableHint(KH_OSKHINTBAR);
    if panHint.Visible then ToggleHintPanel;
  end;
end;

procedure TfrmVisualKeyboard.PreRefreshKeyman;
begin
  FreeAndNil(webHintManager);
end;

procedure TfrmVisualKeyboard.RefreshKeyman;
begin
  CreateWebHintManager;

  // Reload table of visual keyboard references
  if Assigned(FOnScreenKeyboard) then
    FOnScreenKeyboard.RefreshKeyboards;

  RefreshToolbar;   // I4606

  LoadHintSettings;
end;

procedure TfrmVisualKeyboard.SaveSettings;
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKey(SRegKey_KeymanOSK, True) then
    begin
      WriteBool(SRegValue_OSK_FadeVisualKeyboard, FFadeVisualKeyboard);
      WriteInteger(SRegValue_OSK_ActivePage, Ord(ActivePage));
      WriteString(SRegValue_OSK_Position, RectToString(BoundsRect));
      WriteBool(SRegValue_OSK_ShowToolBar, panTop.Visible);
    end;
  finally
    Free;
  end;
end;

procedure TfrmVisualKeyboard.SetActivePage(Page: TOSKActivePage);

  procedure UpdateButtons(Toolbar: TToolbar);
  var
    i: Integer;
  begin
    for i := 0 to Toolbar.ButtonCount - 1 do
    begin
      case Toolbar.Buttons[i].Tag of
        miaOSK_ViewKeyboard: Toolbar.Buttons[i].Down := Page = apKeyboard;
        miaOSK_ViewCharMap: Toolbar.Buttons[i].Down := Page = apCharacterMap;
        miaOSK_ViewEntryHelper: Toolbar.Buttons[i].Down := Page = apEntryHelper;
        miaOSK_ViewKeyboardUsage: Toolbar.Buttons[i].Down := Page = apKeyboardUsage;
        miaOSK_ViewFontHelper: Toolbar.Buttons[i].Down := Page = apFontHelper;
      end;
    end;
  end;
begin
  FActivePageSet := Page <> apUndefined; // I2447 - OSK shows blank pane on display
  if not FActivePageSet then Exit;

  // Disable character map if it is not available in current product, or if the charmap data is missing
  if (Page in [apCharacterMap, apEntryHelper]) and not TfrmOSKCharacterMap.IsCharMapAvailable then
    Page := apKeyboard;

  if (Page = apCharacterMap) and not TfrmOSKCharacterMap.IsCharMapDataInstalled then Page := apKeyboard;

  panKeyboard.Visible := Page = apKeyboard;
  panCharMap.Visible := Page = apCharacterMap;
  panEntryHelper.Visible := Page = apEntryHelper;
  panKeyboardUsage.Visible := Page = apKeyboardUsage;
  panFontHelper.Visible := Page = apFontHelper;

  UpdateButtons(tbLeft);
  UpdateButtons(tbRight);

  UpdatePanels;
end;

function TfrmVisualKeyboard.GetActivePage: TOSKActivePage;
begin
  if panKeyboard.Visible then Result := apKeyboard
  else if panCharMap.Visible then Result := apCharacterMap
  else if panEntryHelper.Visible then Result := apEntryHelper
  else if panKeyboardUsage.Visible then Result := apKeyboardUsage
  else { if panFontHelper.Visible then } Result := apFontHelper;
end;

procedure TfrmVisualKeyboard.LoadKeyboard;
begin
  if not Assigned(FOnScreenKeyboard) then
  begin
    FOnScreenKeyboard := TfrmOSKOnScreenKeyboard.Create(Self);
    FOnScreenKeyboard.Parent := panKeyboard;
    FOnScreenKeyboard.Align := alClient;
    FOnScreenKeyboard.Visible := True;
  end;

  FOnScreenKeyboard.SelectKeyboard(frmKeyman7Main.ActiveKeymanID, FUnicode);   // I3949   // I4359
end;

procedure TfrmVisualKeyboard.LoadSettings;
  procedure MoveBounds(R: TRect);
  begin
    if (R.Left < R.Right) and (R.Top < R.Bottom) and (R.Left >= 0) and (R.Top >= 0) and
        (R.Bottom <= Screen.Height) and (R.Right <= Screen.Width) then
      BoundsRect := R;
  end;
  procedure MoveDefault;
  begin
    SetBounds(Screen.WorkAreaRect.Right - Width, Screen.WorkAreaRect.Bottom - Height, Width, Height);
  end;
var
  xml: string;
begin
  FFadeVisualKeyboard := False;

  LoadHintSettings;

  xml := '';

  if panHint.Visible then
    webHintManager.Content_Render(False, xml);


  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKeyReadOnly(SRegKey_KeymanOSK) then
    begin
      if ValueExists(SRegValue_OSK_FadeVisualKeyboard) then FFadeVisualKeyboard := ReadBool(SRegValue_OSK_FadeVisualKeyboard);

      if not FActivePageSet then
      begin
        if ValueExists(SRegValue_OSK_ActivePage)
          then ActivePage := TOSKActivePage(ReadInteger(SRegValue_OSK_ActivePage))
          else UpdatePanels;
      end;

      if ValueExists(SRegValue_OSK_ShowToolbar) then panTop.Visible := ReadBool(SRegValue_OSK_ShowToolbar);

      if ValueExists(SRegValue_OSK_Position) then MoveBounds(StringToRect(ReadString(SRegValue_OSK_Position)))
      else MoveDefault;
    end
    else
    begin
      MoveDefault;
      if not FActivePageSet then
        UpdatePanels;
    end;
  finally
    Free;
  end;

  UpdateConstraints(True);

  tmrStatus.Enabled := FFadeVisualKeyboard;
  AlphaBlend := FFadeVisualKeyboard;
end;

procedure TfrmVisualKeyboard.MnuOSK_Close(Sender: TObject);
begin
  PostMessage(frmKeyman7Main.Handle, WM_USER_VisualKeyboardClosed, 0, 0);   // I4242
  Close;
end;

procedure TfrmVisualKeyboard.MnuOSK_Fade(Sender: TObject);
begin
  FFadeVisualKeyboard := not FFadeVisualKeyboard;
  tmrStatus.Enabled := FFadeVisualKeyboard;
  AlphaBlend := FFadeVisualKeyboard;
end;

procedure TfrmVisualKeyboard.MnuOSK_Print(Sender: TObject);
begin
  FOnScreenKeyboard.PrintKeyboard;
end;

procedure TfrmVisualKeyboard.MnuOSK_SaveAsWebPage(Sender: TObject);
begin
  FOnScreenKeyboard.SaveAsWebPage;
end;

procedure TfrmVisualKeyboard.MnuOSK_ToggleToolbar(Sender: TObject);
begin
  panTop.Visible := not panTop.Visible;
  if not panTop.Visible then
  begin
    UpdateConstraints;
    SetBounds(Left, Top+panTop.Height, Width, Height-panTop.Height);
  end
  else
  begin
    SetBounds(Left, Top-panTop.Height, Width, Height+panTop.Height);
    UpdateConstraints;
  end;
end;

procedure TfrmVisualKeyboard.MnuOSK_ViewCharMap(Sender: TObject);
begin
  ActivePage := apCharacterMap;
end;

procedure TfrmVisualKeyboard.MnuOSK_ViewEntryHelper(Sender: TObject);
begin
  ActivePage := apEntryHelper;
end;

procedure TfrmVisualKeyboard.MnuOSK_ViewKeyboard(Sender: TObject);
begin
  ActivePage := apKeyboard;
end;

procedure TfrmVisualKeyboard.MnuOSK_ViewKeyboardUsage(Sender: TObject);
begin
  ActivePage := apKeyboardUsage;
end;

procedure TfrmVisualKeyboard.MnuOSK_ViewFontHelper(Sender: TObject);
begin
  ActivePage := apFontHelper;
end;

procedure TfrmVisualKeyboard.mnuPopupPopup(Sender: TObject);
begin
  BuildCustMenu(kmcom, mnuPopup, milOSKContextMenu);   // I3933
end;

procedure TfrmVisualKeyboard.panTitlePaint(Sender: TObject);   // I4849
begin
  GradientFillCanvas(panTitle.Canvas, $DAC379, $A8975E {C2D4DA, $A6BAC2}, panTitle.ClientRect, gdVertical); // I2633   // I4098

end;

procedure TfrmVisualKeyboard.panTopPaint(Sender: TObject);
begin
  GradientFillCanvas(panTop.Canvas, $DAC379, $A8975E {C2D4DA, $A6BAC2}, panTop.ClientRect, gdVertical); // I2633   // I4098
end;

procedure TfrmVisualKeyboard.panTopResize(Sender: TObject);
begin
  panTop.Update;
  tbLeft.Update;
  tbRight.Update;
end;

procedure TfrmVisualKeyboard.LoadCharacterMap;
begin
  if not Assigned(FCharacterMap) and TfrmOSKCharacterMap.IsCharMapAvailable then
  begin
    FCharacterMap := TfrmOSKCharacterMap.Create(Self);
    FCharacterMap.Parent := panCharMap;
    FCharacterMap.Align := alClient;
    FCharacterMap.Visible := True;
  end;
end;

procedure TfrmVisualKeyboard.LoadEntryHelper;
begin
  if not Assigned(FEntryHelper) and TfrmOSKCharacterMap.IsCharMapAvailable then
  begin
    SetTopMost(False);   // I4208   // I4593
    FEntryHelper := TfrmOSKEntryHelper.Create(Self);
    FEntryHelper.Parent := panEntryHelper;
    FEntryHelper.Align := alClient;
    FEntryHelper.Visible := True;
    SetTopMost(True);   // I4208   // I4593
  end;
end;

procedure TfrmVisualKeyboard.LoadFontHelper;
begin
  if not Assigned(FFontHelper) then
  begin
    SetTopMost(False);   // I4208
    FFontHelper := TfrmOSKFontHelper.Create(Self);
    FFontHelper.Parent := panFontHelper;
    FFontHelper.Align := alClient;
    FFontHelper.Visible := True;
    SetTopMost(True);   // I4208
  end;
  FFontHelper.SelectKeyboard(frmKeyman7Main.ActiveKeymanID);   // I3949
end;

procedure TfrmVisualKeyboard.LoadKeyboardUsage;
begin
  if not Assigned(FKeyboardUsage) then
  begin
    SetTopMost(False);   // I4208
    FKeyboardUsage := TfrmOSKKeyboardUsage.Create(Self);
    FKeyboardUsage.Parent := panKeyboardUsage;
    FKeyboardUsage.Align := alClient;
    FKeyboardUsage.Visible := True;
  end;
  FKeyboardUsage.SelectKeyboard(frmKeyman7Main.ActiveKeymanID);   // I3949
end;

procedure TfrmVisualKeyboard.UpdatePanels;
begin
  case ActivePage of
    apKeyboard: LoadKeyboard;
    apCharacterMap: LoadCharacterMap;
    apEntryHelper: LoadEntryHelper;
    apKeyboardUsage: LoadKeyboardUsage;
    apFontHelper: LoadFontHelper;
  end;

  if ActivePage <> apKeyboard then    // I1144
    ResetShiftStates;
end;

function TfrmVisualKeyboard.ActivePageForm: TfrmOSKPlugInBase;
begin
  case ActivePage of
    apKeyboard: Result := FOnScreenKeyboard;
    apCharacterMap: Result := FCharacterMap;
    apEntryHelper: Result := FEntryHelper;
    apKeyboardUsage: Result := FKeyboardUsage;
    apFontHelper: Result := FFontHelper;
    else Result := nil;
  end;
end;

procedure TfrmVisualKeyboard.BtnShowHint(Sender: TObject);
var
  R: TRect;
begin
  with Sender as TToolButton do
  begin
    R := FToolbarHintWindow.CalcHintRect(Screen.Width div 3, Hint, nil);
    with ClientToScreen(Point(0, 0)) do
      OffsetRect(R, X + Width div 2 - R.Right div 2, Y + Height + 2);
    FToolbarHintWindow.ActivateHint(R, Hint);
  end;
end;

procedure TfrmVisualKeyboard.BtnHelpClick(Sender: TObject);
begin
  with TfrmHelp.Create(Self) do   // I1251 - Combine help with keyboard help
  try
    HelpJump := 'context_onscreenkeyboard';
    ActiveKeyboard := frmKeyman7Main.ActiveKeyboard;// KeymanInterface.Control.ActiveKeyboard;
    ShowModal;
    case HelpTarget of
      htNone: ;
      htProduct: OpenProductHelp;
      htKeyboard: OpenKeyboardHelp;
    end;
  finally
    Free;
  end;

  {case ActivePage of
    apKeyboard: prod.OpenHelp('context_onscreenkeyboard');
    apCharacterMap: prod.OpenHelp('context_charactermap');
    apEntryHelper: prod.OpenHelp('context_entryhelper');
    apKeyboardUsage: prod.OpenHelp('context_keyboardusage');
    apFontHelper:
  end;}
end;

procedure TfrmVisualKeyboard.BtnHideHint(Sender: TObject);
begin
  ShowWindow(FToolbarHintWindow.Handle, SW_HIDE);
  { Keep visible in sync with the actual visible-ness of the property }
  FToolbarHintWindow.Visible := False;
end;

procedure TfrmVisualKeyboard.RefreshSelectedKeyboard;  // I2398
begin
  if Assigned(FOnScreenKeyboard) and (ActivePage = apKeyboard) then
  begin
    FOnScreenKeyboard.SelectKeyboard(frmKeyman7Main.ActiveKeymanID, FUnicode);   // I3949   // I4359
  end;

  if Assigned(FKeyboardUsage) and (ActivePage = apKeyboardUsage) then
    FKeyboardUsage.SelectKeyboard(frmKeyman7Main.ActiveKeymanID);   // I3949

  if Assigned(FFontHelper) and (ActivePage = apFontHelper) then
    FFontHelper.SelectKeyboard(frmKeyman7Main.ActiveKeymanID);   // I3949

  UpdateKeyboardIcon;   // I3962

  UpdateActivePage;
  RefreshToolbar;
  UpdateToolbar;
end;

procedure TfrmVisualKeyboard.UpdateActivePage;
begin
  if ActivePage in [apUndefined, apKeyboard, apKeyboardUsage] then  // I2287
  begin
    if frmKeyman7Main.Option_AutoSwitchOSKPages then
    begin
      { Select either Keyboard Usage or VK, depending on whether or not the VK exists }
      LoadKeyboard;
      if (frmKeyman7Main.ActiveKeymanID = KEYMANID_NONKEYMAN) or FOnScreenKeyboard.HasVisualKeyboard(frmKeyman7Main.ActiveKeymanID) then   // I3949
        ActivePage := apKeyboard
      else
      begin
        LoadKeyboardUsage;
        ActivePage := apKeyboardUsage;
      end;
    end;
  end;
end;

function TfrmVisualKeyboard.GetToolbarPositionXML: WideString;
var
  FXML: WideString;
begin
  if panTop.Visible then
  begin
    FXML := '<Toolbar Visible="1" Keyboards="0" />'
  end
  else
    FXML := '<Toolbar Visible="0" />';

  Result := FXML;
end;

{ TKeymanCustomisationMenuItem_Clone }

function TKeymanCustomisationMenuItem_Clone.Get_Action: TOleEnum;
begin
  Result := miaNull;
end;

function TKeymanCustomisationMenuItem_Clone.Get_Caption: WideString;
begin
  Result := '';
end;

function TKeymanCustomisationMenuItem_Clone.Get_CmdLine: WideString;
begin
  Result := '';
end;

function TKeymanCustomisationMenuItem_Clone.Get_Font: IFontDisp;
begin
  Result := nil;
end;

function TKeymanCustomisationMenuItem_Clone.Get_Hotkey: Word;
begin
  Result := 0;
end;

function TKeymanCustomisationMenuItem_Clone.Get_Icon: IPictureDisp;
begin
  Result := nil;
end;

function TKeymanCustomisationMenuItem_Clone.Get_Index: Integer;
begin
  Result := 0;
end;

function TKeymanCustomisationMenuItem_Clone.Get_ItemType: TOleEnum;
begin
  Result := mitText;
end;

function TKeymanCustomisationMenuItem_Clone.Get_Location: TOleEnum;
begin
  Result := milLeft;
end;

function TKeymanCustomisationMenuItem_Clone.Get_PictureDefault: IPictureDisp;
begin
  Result := nil;
end;

function TKeymanCustomisationMenuItem_Clone.Get_PictureSelected: IPictureDisp;
begin
  Result := nil;
end;

end.

