(*
  Name:             UfrmLanguageSwitch
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      22 Oct 2010

  Modified Date:    10 Jun 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          22 Oct 2010 - mcdurdin - I2522 - Initial version of language switch window
                    30 Nov 2010 - mcdurdin - I2544 - Restyle language switch window as per Paul's design
                    10 Dec 2010 - mcdurdin - I2555 - Transparency issues
                    10 Dec 2010 - mcdurdin - I2554 - Potential for crash when window closes
                    17 Dec 2010 - mcdurdin - I2593 - Transparent Keyman keyboard icons
                    17 Dec 2010 - mcdurdin - I2592 - Stop window disappearing as soon as it opens
                    31 Jan 2011 - mcdurdin - I2639 - Fix memory leak in language switch window
                    31 Jan 2011 - mcdurdin - I2677 - Lang Switch window shows old selection when popping up, disconcerting
                    31 Jan 2011 - mcdurdin - I2674 - Show the Selection with a little triangle on the left
                    28 Feb 2011 - mcdurdin - I2669 - If too many keyboards are associated with one language, the language switch window can become unusable
                    03 May 2011 - mcdurdin - I2867 - When no keyboards listed for a language, language switch window can crash
                    19 Aug 2011 - mcdurdin - I3023 - Boxes in the language switch window can be wrong size
                    03 Oct 2011 - mcdurdin - I3087 - Crash opening language switch window when only 1 language detected
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    24 Jan 2012 - mcdurdin - I3213 - Handle access denied error when lang switch window visible and Ctrl+Alt+Del pressed
                    03 Nov 2012 - mcdurdin - I3517 - V9.0 - Merge of I3213 - Crash when Keyman menu is visible and Ctrl+Alt+Del or Win+L pressed
                    24 Oct 2013 - mcdurdin - I3933 - V9.0 - Keyman tray icon menu is not showing installed keyboards
                    06 Mar 2014 - mcdurdin - I4124 - V9.0 - Language switch dialog is not working in v9
                    24 Apr 2014 - mcdurdin - I4191 - V9.0 - Lang switch window shows wrong selection with Alt+LeftShift when TIP is active
                    24 Apr 2014 - mcdurdin - I4190 - V9.0 - Lang switch window shifts on first view
                    10 Jun 2014 - mcdurdin - I4204 - V9.0 - Icons do not show background correctly in lang switch window and Win 8 languages controls
*)
unit UfrmLanguageSwitch;  // I3306

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  ActiveX,
  ComObj,
  Glossary,
  LangSwitchManager,
  msctf,
  Types,
  ErrorControlledRegistry, 
  RegistryKeys, ExtCtrls;

type
  TMetrics = record
    CellWidth,
    ColCount,
    RowCount,
    TotalCellHeight,
    TitleTextHeight,
    KeyboardTextHeight,
    KeyboardRowWidth,
    KeyboardRowHeight: Integer;
    CellHeight: array of Integer;
  end;

  TObjectType = (ptBorder, ptBorder2, ptBackground, ptLanguageIcon, ptHighlight, ptSelection, ptCellBackground,
    csNormal, csHover, csSelected, csHoverSelected,
    bsNormal, bsHover, bsSelected, bsHoverSelected);
  TCellStatus = csNormal .. csHoverSelected;

  TfrmLanguageSwitch = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pbMouseEnter(Sender: TObject);
    procedure pbMouseLeave(Sender: TObject);
    procedure pbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure pbClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormHide(Sender: TObject);
  private
    FMetrics: TMetrics;
    FHoverLanguage: Integer;
    FHoverKeyboard: Integer;
    FSelectedLanguage: Integer;
    FSelectedKeyboard: Integer;
    FOnHidden: TNotifyEvent;

    hBuffer: THandle;
    hLanguageFont: THandle;
    hKeyboardFont: THandle;
    hLanguageIconFont: THandle;
    hPens: array[TObjectType] of THandle;
    hBrushes: array[TObjectType] of THandle;

    ptPos, ptSrc: TPoint;
    szWnd: TSize;
    blend: TBlendFunction;
    bmi: TBitmapInfo;
    pvBufferBits: Pointer;
    dcLayered: Cardinal;
    hLayeredBitmap: Cardinal;
    pvLayeredBits: Pointer;  // I2554
    FTransparency: DWord;  // I2555
    FCurrentlySelectedLanguage: Integer;   // I4191
    FCurrentlySelectedKeyboard: Integer;   // I4191

    procedure GetColors(y: Integer; var cc: array of Cardinal);

    procedure CancelDialog;

    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMNCLButtonDown(var Message: TMessage); message WM_NCLBUTTONDOWN;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;

    procedure WMUser(var Message: TMessage); message WM_USER;
    procedure DrawLanguageIcons(DrawSelection: Boolean);
    procedure MeasureFormSize;
    procedure UpdateHover;
    procedure SelectKeyboard(FromSelection: Boolean);
    procedure DrawLayer;
    procedure EndCanvas;
    procedure StartCanvas;
    function GetCellY(y: Integer): Integer;  // I2669
    function GetRowY(y: Integer): Integer;  // I2669

    function Manager: TLangSwitchManager;
    function KeyboardTextWidth(Canvas: TCanvas;
      Keyboard: TLangSwitchKeyboard): Integer;   // I4191
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc(var Message: TMessage); override;

  public
    procedure SelectItem(Item: TLangSwitchKeyboard);   // I4191
    function GetSelection: TLangSwitchKeyboard;
    property OnHidden: TNotifyEvent read FOnHidden write FOnHidden;
  end;

implementation

{$R *.dfm}

uses
  System.Math,

  Keyman.System.DebugLogClient,
  KeymanControlMessages,
  keymanapi_TLB,
  UfrmKeyman7Main,
  utilhotkey;

const
  MLanguageFontHeight = -14;
  MLanguageIconFontHeight = -10;
  MKeyboardFontHeight = -12;

  MWindowMargin: TRect = (Left: 16; Top: 16; Right: 16; Bottom: 16);
  MCellSpacing: TPoint = (X: 8; Y: 8);
  MCellMargin: TRect =   (Left: 4; Top: 4; Right: 4; Bottom: 4);

  MLanguageIconPosition: TPoint = (X: 2; Y: 2);  // relative to MCellMargin
  MLanguageTextPosition: TPoint = (X: 20; Y: 0); // relative to MCellMargin

  MKeyboardsMargin: TRect = (Left: 10; Top: 2; Right: 0; Bottom: 0);  // keyboard list margins
  MKeyboardMargin: TRect = (Left: 2; Top: 2; Right: 2; Bottom: 2); // margin inside a single keyboard box
  MKeyboardTextPosition: TPoint = (X: 18; Y: 0); // relative to MKeyboardsMargin + MKeyboardPadding
  MKeyboardIconPosition: TPoint = (X: 0; Y: 0); // relative to MKeyboardsMargin + MKeyboardPadding
  MHotkeyMargin = 8; // pixels between keyboard caption and hotkey text
  MKeyboardSpacing = 2; // pixels between each keyboard entry

  CTransparencyMask: Cardinal = $F0000000;  // I2555
  CNoTransparencyMask: Cardinal = $FF000000;  // I2555

  CWindowBackground: TColor = $D3BBA0;
  CWindowBorder: array[0..5] of Cardinal = ($00000000,$3F4C58,$BCD1E3,$BCD1E3,$A5A2A5,$677B8E);
  //CWindowBorder:array[0..3] of TColor = ($00FF00,$584C3F,$E3D0BB,$D3BBA0);
  CCellBorder:array[TCellStatus,0..4] of TColor =
    (($D3BBA0,$A3907C,$EADBC9,$C6B096,$D8C8B5),
     ($D3BBA0,$CDA77B,$F7E5CD,$E5CAA7,$E7D1B5),
     ($D3BBA0,$284AAD,$FFFFFF,$A793A2,$C8D1E9),
     ($D3BBA0,$284AAD,$FFFFFF,$A793A2,$C8D1E9));
  CSelection: TColor = $8F7C67;
  CSelectionText: TColor = $FFFFFF;
  CNormalText: TColor = $000000;
  CHighlight: TColor = $BD976B;
  CHighlightText: TColor = $FFFFFF;
  CLanguageIcon: TColor = $C0C2C6;
  CLanguageIconText: TCOlor = $404143;

const
  CColors: array[TObjectType] of TColor =
    ($584C3F,$E3D0BB,$D3BBA0,$C0C2C6,$BD976B,$8F7C67,$EADBC9,
    $A3907C,$CDA77B,$284AAD,$284AAD,$EADBC9,$F7E5CD,$FFFFFF,$FFFFFF);

{ TForm1 }

procedure TfrmLanguageSwitch.FormCreate(Sender: TObject);
var
  i: TObjectType;
begin
  SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);

  hLanguageFont := CreateFont(MLanguageFontHeight,0,0,0,FW_NORMAL,0,0,0,ANSI_CHARSET,OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS,ANTIALIASED_QUALITY,DEFAULT_PITCH or FF_DONTCARE,'Tahoma');
  hKeyboardFont := CreateFont(MKeyboardFontHeight,0,0,0,FW_NORMAL,0,0,0,ANSI_CHARSET,OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS,ANTIALIASED_QUALITY,DEFAULT_PITCH or FF_DONTCARE,'Tahoma');
  hLanguageIconFont := CreateFont(MLanguageIconFontHeight,0,0,0,FW_NORMAL,0,0,0,ANSI_CHARSET,OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS,ANTIALIASED_QUALITY,DEFAULT_PITCH or FF_DONTCARE,'Tahoma');

  for i := Low(TObjectType) to High(TObjectType) do
  begin
    hPens[i] := CreatePen(PS_SOLID, 1, CColors[i]);
    hBrushes[i] := CreateSolidBrush(CColors[i]);
  end;
end;

procedure TfrmLanguageSwitch.FormDestroy(Sender: TObject);
var
  i: TObjectType;
begin
  if hLanguageFont <> 0 then DeleteObject(hLanguageFont); // I2639
  hLanguageFont := 0;

  if hKeyboardFont <> 0 then DeleteObject(hKeyboardFont); // I2639
  hKeyboardFont := 0;

  if hLanguageIconFont <> 0 then DeleteObject(hLanguageIconFont); // I2639
  hLanguageIconFont := 0;

  if hBuffer <> 0 then DeleteObject(hBuffer); // I2639
  hBuffer := 0;

  if hLayeredBitmap <> 0 then DeleteObject(hLayeredBitmap); // I2639
  hLayeredBitmap := 0;

  for i := Low(TObjectType) to High(TObjectType) do
  begin
    if hPens[i] <> 0 then DeleteObject(hPens[i]); // I2639
    hPens[i] := 0;
    if hBrushes[i] <> 0 then DeleteObject(hBrushes[i]); // I2639
    hBrushes[i] := 0;
  end;
end;

procedure TfrmLanguageSwitch.FormHide(Sender: TObject);
begin
  if Assigned(FOnHidden) then FOnHidden(Self);
  if hLayeredBitmap <> 0 then DeleteObject(hLayeredBitmap); // I2639
  hLayeredBitmap := 0;
  UpdateLayeredWindow(Handle, 0, nil, nil, 0, nil, 0, @blend, ULW_ALPHA); // I2677
end;

procedure TfrmLanguageSwitch.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

  procedure AdjustSelectedKeyboard;
  begin
    if FSelectedKeyboard >= Manager.Languages[FSelectedLanguage].KeyboardCount then   // I4191
      FSelectedKeyboard := Manager.Languages[FSelectedLanguage].KeyboardCount - 1;
  end;

begin
  TDebugLogClient.Instance.WriteMessage('TfrmLanguageSwitch.FormKeyDown ENTER: %d [%d %d]', [Key, FSelectedLanguage, FSelectedKeyboard]);
  try
    if FSelectedLanguage = -1 then FSelectedLanguage := 0;
    if FSelectedKeyboard = -1 then FSelectedKeyboard := 0;

    case Key of
      VK_SHIFT, VK_LSHIFT, VK_RSHIFT:   // I4124
        begin
          Inc(FSelectedKeyboard);
          if FSelectedKeyboard >= Manager.Languages[FSelectedLanguage].KeyboardCount then   // I4191
          begin
            FSelectedKeyboard := 0;
            Inc(FSelectedLanguage);
            if FSelectedLanguage >= Manager.LanguageCount then   // I4191
              FSelectedLanguage := 0;
          end;
        end;
      VK_LEFT:
        begin
          Dec(FSelectedLanguage);
          if FSelectedLanguage < 0 then FSelectedLanguage := Manager.LanguageCount - 1;   // I4191
          AdjustSelectedKeyboard;
        end;

      VK_RIGHT:
        begin
          Inc(FSelectedLanguage);
          if FSelectedLanguage >= Manager.LanguageCount then FSelectedLanguage := 0;   // I4191
          AdjustSelectedKeyboard;
        end;
      VK_UP:
        begin
          Dec(FSelectedKeyboard);
          if FSelectedKeyboard < 0 then
          begin
            if FSelectedLanguage < FMetrics.ColCount then
            begin
              if (FMetrics.RowCount-1) * FMetrics.ColCount + FSelectedLanguage >= Manager.LanguageCount   // I4191
                then FSelectedLanguage := (FMetrics.RowCount-2) * FMetrics.ColCount + FSelectedLanguage
                else FSelectedLanguage := (FMetrics.RowCount-1) * FMetrics.ColCount + FSelectedLanguage;
            end
            else
              Dec(FSelectedLanguage, FMetrics.ColCount);

            FSelectedKeyboard := Manager.Languages[FSelectedLanguage].KeyboardCount - 1;   // I4191
          end;
        end;

      VK_DOWN:
        begin
          Inc(FSelectedKeyboard);
          if FSelectedKeyboard >= Manager.Languages[FSelectedLanguage].KeyboardCount then   // I4191
          begin
            Inc(FSelectedLanguage, FMetrics.ColCount);
            if FSelectedLanguage >= Manager.LanguageCount then   // I4191
              FSelectedLanguage := FSelectedLanguage mod FMetrics.ColCount;
            FSelectedKeyboard := 0;
          end;
        end;
      VK_HOME:
        begin
          FSelectedLanguage := 0;
          FSelectedKeyboard := 0;
        end;
      VK_END:
        begin
          FSelectedLanguage := Manager.LanguageCount - 1;   // I4191
          FSelectedKeyboard := Manager.Languages[FSelectedLanguage].KeyboardCount - 1;   // I4191
        end;
      VK_PRIOR:
        begin
          if FSelectedLanguage < FMetrics.ColCount then
          begin
            if (FMetrics.RowCount-1) * FMetrics.ColCount + FSelectedLanguage >= Manager.LanguageCount   // I4191
              then FSelectedLanguage := (FMetrics.RowCount-2) * FMetrics.ColCount + FSelectedLanguage
              else FSelectedLanguage := (FMetrics.RowCount-1) * FMetrics.ColCount + FSelectedLanguage;
          end
          else
            Dec(FSelectedLanguage, FMetrics.ColCount);
          FSelectedKeyboard := 0;
        end;
      VK_NEXT:
        begin
          Inc(FSelectedLanguage, FMetrics.ColCount);
          if FSelectedLanguage >= Manager.LanguageCount then   // I4191
            FSelectedLanguage := FSelectedLanguage mod FMetrics.ColCount;
          FSelectedKeyboard := 0;
        end;
      32, VK_RETURN:
        SelectKeyboard(True);
      VK_ESCAPE:
        CancelDialog;
      else Exit;
    end;
    StartCanvas;
    try
      DrawLanguageIcons(True);
    finally
      EndCanvas;
    end;
  finally
    TDebugLogClient.Instance.WriteMessage('TfrmLanguageSwitch.FormKeyDown EXIT: %d [%d %d]', [Key, FSelectedLanguage, FSelectedKeyboard]);
  end;
  Key := 0;
end;

procedure TfrmLanguageSwitch.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key in [VK_MENU, VK_LMENU, VK_RMENU] then   // I4124
  begin
    Key := 0;
    SelectKeyboard(True);
  end;
end;

procedure TfrmLanguageSwitch.FormShow(Sender: TObject);
begin
  FTransparency := CNoTransparencyMask;

  MeasureFormSize;

  FHoverKeyboard := -1;
  FHoverLanguage := -1;

  DrawLayer;  // I2677

  TDebugLogClient.Instance.WriteMessage('TfrmLanguageSwitch.FormShow EXIT: [%d %d]', [FSelectedLanguage, FSelectedKeyboard]);
end;

procedure TfrmLanguageSwitch.GetColors(y: Integer; var cc: array of Cardinal);
const
  cbounds: array[0..1,0..3] of Cardinal = (
    ($9C8B7A, $B7A48F, $D2BBA3, $E3D1BC),
    ($8E7B67, $AD9780, $CCB296, $E0CBB3));
var
  range, r1, g1, b1, r2, g2, b2, r, g, b, c: Cardinal;
  i, v: Integer;
begin
  v := 0;
  range := ClientHeight;
  cc[3] := $3F4C58 or FTransparency;  // I2555

  if y = 0 then y := 1;

  for i := 0 to 1 do
  begin
    r1 := (cbounds[i,v+0] and $FF0000) shr 16;
    g1 := (cbounds[i,v+0] and $00FF00) shr 8;
    b1 := (cbounds[i,v+0] and $0000FF);

    r2 := (cbounds[i,v+1] and $FF0000) shr 16;
    g2 := (cbounds[i,v+1] and $00FF00) shr 8;
    b2 := (cbounds[i,v+1] and $0000FF);

    r := (r2 - r1) * Cardinal(y) div range + r1;
    g := (g2 - g1) * Cardinal(y) div range + g1;
    b := (b2 - b1) * Cardinal(y) div range + b1;

    c := FTransparency or (r or (g shl 8) or (b shl 16));  // I2555
    cc[i+1] := c;
  end;
end;

function TfrmLanguageSwitch.GetSelection: TLangSwitchKeyboard;
begin
  if (FSelectedLanguage < 0) or (FSelectedKeyboard < 0) // I2867
    then Result := nil
    else Result := Manager.Languages[FSelectedLanguage].Keyboards[FSelectedKeyboard];   // I4191
end;

function TfrmLanguageSwitch.Manager: TLangSwitchManager;   // I4191
begin
  Result := frmKeyman7Main.LangSwitchManager;
end;

function TfrmLanguageSwitch.KeyboardTextWidth(Canvas: TCanvas; Keyboard: TLangSwitchKeyboard): Integer;
var
  FLanguage: IKeymanLanguage;
begin
  Result := Canvas.TextWidth(Keyboard.Caption);
  FLanguage := Keyboard.KeymanLanguage;
  if Assigned(FLanguage) and (FLanguage.Hotkey.RawValue <> 0) then
    Result := Result + MHotkeyMargin + Canvas.TextWidth(ShortcutToTextEx(HotkeyToShortcut(FLanguage.Hotkey)));
end;

procedure TfrmLanguageSwitch.MeasureFormSize;
var
  x, y, i: Integer;
  j: Integer;
  FBuffer: TBitmap;
  n: Integer;
begin
  FBuffer := TBitmap.Create;
  try
    FBuffer.Canvas.Font.Name := 'Tahoma';
    FBuffer.Canvas.Font.Size := MLanguageFontHeight;
    FMetrics.TitleTextHeight := FBuffer.Canvas.TextHeight('A');

    SetLength(FMetrics.CellHeight, Manager.LanguageCount);  // I2669   // I4191

    x := 1;  // I3087
    y := 0;
    for i := 0 to Manager.LanguageCount - 1 do   // I4191
    begin
      x := Max(x, FBuffer.Canvas.TextWidth(Manager.Languages[i].Caption) + MLanguageTextPosition.X);   // I4191
      y := Max(y, Manager.Languages[i].KeyboardCount);   // I4191
    end;

    FBuffer.Canvas.Font.Size := MKeyboardFontHeight;
    FMetrics.KeyboardTextHeight := FBuffer.Canvas.TextHeight('A');
    FMetrics.KeyboardRowHeight := Max(FMetrics.KeyboardTextHeight, 16) + MKeyboardMargin.Top + MKeyboardMargin.Bottom;

    for i := 0 to Manager.LanguageCount - 1 do   // I4191
    begin
      for j := 0 to Manager.Languages[i].KeyboardCount - 1 do   // I4191
      begin
        x := Max(x,
          KeyboardTextWidth(FBuffer.Canvas, Manager.Languages[i].Keyboards[j]) +
          MKeyboardsMargin.Left + MKeyboardsMargin.Right + MKeyboardMargin.Left + MKeyboardMargin.Right + MKeyboardTextPosition.X);   // I4191
      end;
    end;

    FMetrics.KeyboardRowWidth := x - MKeyboardsMargin.Left - MKeyboardsMargin.Right;

    FMetrics.CellWidth := x + MCellMargin.Left + MCellMargin.Right;

    FMetrics.TotalCellHeight := 0;  // I2669

    FMetrics.ColCount := Min(Manager.LanguageCount, (Screen.Width * 2 div 3) div x);   // I4191
    if FMetrics.ColCount = 0 then FMetrics.ColCount := 1;  // I3087

    FMetrics.RowCount := (Manager.LanguageCount-1) div FMetrics.ColCount + 1;   // I4191

    for i := 0 to Manager.LanguageCount do  // I2669   // I4191
    begin
      if ((i mod FMetrics.ColCount) = 0) or (i = Manager.LanguageCount) then   // I4191
      begin
        if i > 0 then
        begin
          y := (y * FMetrics.KeyboardRowHeight) + MKeyboardsMargin.Top + MKeyboardsMargin.Bottom + FMetrics.TitleTextHeight + MCellMargin.Top + MCellMargin.Bottom;
          if (i mod FMetrics.ColCount) > 0  // I3023
            then n := i - (i mod FMetrics.ColCount)
            else n := i - FMetrics.ColCount;

          for j := n to i - 1 do
            FMetrics.CellHeight[j] := y;
          FMetrics.TotalCellHeight := FMetrics.TotalCellHeight + y;
        end;
        y := 0;
      end;
      if i < Manager.LanguageCount then   // I4191
        y := Max(y, Manager.Languages[i].KeyboardCount);   // I4191
    end;

    //FMetrics.CellHeight := (y * FMetrics.KeyboardRowHeight) + MKeyboardsMargin.Top + MKeyboardsMargin.Bottom + FMetrics.TitleTextHeight + MCellMargin.Top + MCellMargin.Bottom;

    with FMetrics do
    begin
      szWnd.cx := CellWidth * ColCount + MCellSpacing.X * (ColCount - 1) + MWindowMargin.Left + MWindowMargin.Right;
      szWnd.cy := TotalCellHeight + MCellSpacing.Y * (RowCount - 1) + MWindowMargin.Top + MWindowMargin.Bottom;  // I2669
    end;
    ptPos.X := (Screen.Width div 2) - szWnd.cx div 2;
    ptPos.Y := (Screen.Height div 2) - szWnd.cy div 2;

    //TDebugManager.WriteMessage('TfrmLanguageSwitch.MeasureFormSize(%d, %d, %d, %d)', [ptPos.X, ptPos.Y, szWnd.cx, szWnd.cy]);   // I4190

    SetBounds(ptPos.X, ptPos.Y, szWnd.cx, szWnd.cy);
  finally
    FBuffer.Free;
  end;
end;

procedure TfrmLanguageSwitch.pbClick(Sender: TObject);
begin
  SelectKeyboard(False);
end;

procedure TfrmLanguageSwitch.pbMouseEnter(Sender: TObject);
begin
  UpdateHover;
end;

procedure TfrmLanguageSwitch.pbMouseLeave(Sender: TObject);
begin
  UpdateHover;
end;

procedure TfrmLanguageSwitch.pbMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  UpdateHover;
end;

function TfrmLanguageSwitch.GetCellY(y: Integer): Integer;  // Get index of row from pixel Y  // I2669
var
  i: Integer;
begin
  i := 0;
  while i < FMetrics.RowCount do
  begin
    if y < FMetrics.CellHeight[i * FMetrics.ColCount] + MCellSpacing.Y then
    begin
      Result := i;
      Exit;
    end;
    Dec(y, FMetrics.CellHeight[i * FMetrics.ColCount] + MCellSpacing.Y);
    Inc(i);
  end;
  Result := i;
end;

function TfrmLanguageSwitch.GetRowY(y: Integer): Integer; // Get pixel Y from index of row  // I2669
var
  i: Integer;
begin
  i := 0;
  Result := 0;
  while i < y do
  begin
    Inc(Result, FMetrics.CellHeight[i * FMetrics.ColCount] + MCellSpacing.Y);
    Inc(i);
  end;
end;

procedure TfrmLanguageSwitch.UpdateHover;
var
  pt: TPoint;
  r: TRect;
  FNewHoverLanguage: Integer;
  lang: TLangSwitchLanguage;
  FNewHoverKeyboard: Integer;
begin
  with FMetrics do
  begin
    if not GetCursorPos(pt) then pt := Point(-1,-1)  // I3213   // I3517
    else pt := ScreenToClient(pt);
    r := ClientRect;
    Inc(r.Left, MWindowMargin.Left);
    Inc(r.Top, MWindowMargin.Top);
    Dec(r.Right, MWindowMargin.Right);
    Dec(r.Bottom, MWindowMargin.Bottom);

    FNewHoverKeyboard := -1;
    FNewHoverLanguage := -1;

    if PtInRect(r, pt) then
    begin
      pt.X := (pt.X - r.Left) div (CellWidth + MCellSpacing.X);
      pt.Y := GetCellY(pt.Y - r.Top);  // I2669
      
      FNewHoverLanguage := pt.X + pt.Y * ColCount;
      if FNewHoverLanguage >= Manager.LanguageCount then   // I4191
        FNewHoverLanguage := -1
      else
      begin
        lang := Manager.Languages[FNewHoverLanguage];   // I4191
        if not GetCursorPos(pt) then pt := Point(-1,-1)  // I3213   // I3517
        else pt := ScreenToClient(pt);
        Dec(pt.X, MWindowMargin.Left);
        Dec(pt.Y, MWindowMargin.Top);
        pt.X := pt.X mod (CellWidth + MCellSpacing.X);
        pt.Y := pt.Y - GetRowY(FNewHoverLanguage div ColCount);  // I2669
        if pt.Y < MCellMargin.Top + TitleTextHeight + MKeyboardsMargin.Top then
          FNewHoverKeyboard := 0  // on or around the title
        else if pt.Y > MCellMargin.Top + TitleTextHeight + MKeyboardsMargin.Top + lang.KeyboardCount * KeyboardRowHeight then
          FNewHoverKeyboard := 0  // below list of keyboards
        else if (pt.X < MCellMargin.Left) or (pt.X > CellWidth - MCellMargin.Right) then
          FNewHoverKeyboard := 0  // in the left or right margin of the cell
        else
          FNewHoverKeyboard := (pt.Y - (MCellMargin.Top + TitleTextHeight + MKeyboardsMargin.Top)) div KeyboardRowHeight;
      end;
    end;

    if (FNewHoverLanguage <> FHoverLanguage) or
      (FNewHoverKeyboard <> FHoverKeyboard) then
    begin
      FHoverLanguage := FNewHoverLanguage;
      FHoverKeyboard := FNewHoverKeyboard;
      StartCanvas;
      try
        DrawLanguageIcons(True);
      finally
        EndCanvas;
      end;
    end;
  end;
end;

procedure TfrmLanguageSwitch.WMMouseActivate(var Message: TWMMouseActivate);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TfrmLanguageSwitch.WMNCHitTest(var Message: TWMNCHitTest);
begin
  Message.Result := htClient;
end;

procedure TfrmLanguageSwitch.WMNCLButtonDown(var Message: TMessage);
begin
  inherited;
end;

procedure TfrmLanguageSwitch.WMUser(var Message: TMessage);
begin
  DrawLayer;
end;

procedure TfrmLanguageSwitch.WndProc(var Message: TMessage);
var
  wParam: WORD;
begin
  if Message.Msg = wm_keyman_control then
  begin
    wParam := Message.lParam; 
    case Message.WParam of
      KMC_KEYDOWN:
        KeyDown(wParam, []);
      KMC_KEYUP:
        KeyUp(WParam, []);
    end;
  end
  else
    inherited;
end;

type
  CardinalArray  = array[0..$effffff] of Cardinal;
  PCardinalArray = ^CardinalArray;

procedure TfrmLanguageSwitch.DrawLayer;
var
  dc, dcScreen: THandle;
  hBuffer: THandle;
  hOldBitmap: THandle;
  p: PCardinalArray;

      procedure DrawBackgroundAndBorders;
      var
        x, y: Integer;
        cc: array[0..3] of Cardinal;
      begin
        { Draw bottom border }
        GetColors(1, cc);
        y := 0;
        for x := 5 to ClientWidth - 6 do p^[x + y] := cc[3];
        y := ClientWidth * 1;
        for x := 5 to ClientWidth - 6 do p^[x + y] := cc[1];

        { Draw top border }
        GetColors(ClientHeight-1, cc);
        y := ClientWidth * (ClientHeight - 1);
        for x := 5 to ClientWidth - 6 do p^[x + y] := cc[3]; //CWindowBorder[5] or CTransparency;
        y := ClientWidth * (ClientHeight - 2);
        for x := 5 to ClientWidth - 6 do p^[x + y] := cc[1]; //CWindowBorder[2] or CTransparency;

        { Draw side borders }
        for y := 2 to ClientHeight - 3 do
        begin
          GetColors(y, cc);
          p^[ClientWidth * y] := cc[3];
          p^[ClientWidth * y + 1] := cc[1];
          p^[ClientWidth * y + ClientWidth - 2] := cc[1];
          p^[ClientWidth * y + ClientWidth - 1] := cc[3];
          for x := 2 to ClientWidth - 3 do p^[ClientWidth * y + x] := cc[2];
        end;
      end;

begin
  dcScreen := GetDC(Handle);
  dc := CreateCompatibleDC(dcScreen);

  // zero the memory for the bitmap info
  ZeroMemory(@bmi, sizeof(BITMAPINFO));

  // setup bitmap info
  bmi.bmiHeader.biSize := sizeof(BITMAPINFOHEADER);
  bmi.bmiHeader.biWidth := ClientWidth;
  bmi.bmiHeader.biHeight := ClientHeight;
  bmi.bmiHeader.biPlanes := 1;
  bmi.bmiHeader.biBitCount := 32;         // four 8-bit components
  bmi.bmiHeader.biCompression := BI_RGB;
  bmi.bmiHeader.biSizeImage := ClientWidth * ClientHeight * 4;

  if hLayeredBitmap <> 0 then DeleteObject(hLayeredBitmap); // I2639

  // create our DIB section and select the bitmap into the dc
  hBuffer := CreateDIBSection(dc, bmi, DIB_RGB_COLORS, pvBufferBits, 0, 0);
  hLayeredBitmap := CreateDIBSection(dc, bmi, DIB_RGB_COLORS, pvLayeredBits, 0, 0);  // I2554

  hOldBitmap := SelectObject(dc, hBuffer);

  ptSrc := Point(0, 0);

  blend.BlendOp := AC_SRC_OVER;
  blend.BlendFlags := 0;
  blend.SourceConstantAlpha := 255;
  blend.AlphaFormat := AC_SRC_ALPHA;

  p := PCardinalArray(pvBufferBits);

  DrawBackgroundAndBorders;

  dcLayered := dc;
  DrawLanguageIcons(False);
  dcLayered := 0;

  SelectObject(dc, hOldBitmap);

  DeleteDC(dc);
  ReleaseDC(Handle, dcScreen);

  StartCanvas;
  DrawLanguageIcons(True);
  EndCanvas;
end;

procedure TfrmLanguageSwitch.StartCanvas;
var
  dcScreen: THandle;
begin
  if Assigned(pvLayeredBits) and Assigned(pvBufferBits) and (hLayeredBitmap <> 0) then  // I2554
  begin
    dcScreen := GetDC(Handle);
    dcLayered := CreateCompatibleDC(dcScreen);
    ReleaseDC(Handle, dcScreen);

    SelectObject(dcLayered, hLayeredBitmap);

    CopyMemory(pvLayeredBits, pvBufferBits, ClientWidth * ClientHeight * 4);
  end;
end;

procedure TfrmLanguageSwitch.EndCanvas;
var
  dcScreen: THandle;

      procedure DrawCornersAndMakeTransparent;
      const
        pels: array[0..4,0..4] of Byte =
          ((0,0,0,0,0),
           (0,0,0,1,1),
           (0,0,1,2,2),
           (0,1,2,3,3),
           (0,1,2,3,3));
      var
        v, x, y: Integer;
        X1, X2: Integer;
        c: Cardinal;
        cc: array[0..3] of Cardinal;
        p: PCardinalArray;
        //FSelectionRect: TRect;
      begin
        { Draw corners }
        p := PCardinalArray(pvLayeredBits);  // I2554

        for y := 0 to 4 do
        begin
          v := (ClientHeight - y - 1) * ClientWidth;
          GetColors(ClientHeight - y, cc); //1, c2, c3);
          for x := 0 to 4 do
          begin
            case pels[x, y] of
              0: c := 0;
              2: c := cc[1];
              3: c := cc[2];
              else c := CWindowBorder[pels[x, y]] or FTransparency;  // I2555
            end;

            p^[v + x] := c;
            p^[v + ClientWidth - x - 1] := c;
          end;

          v := y * ClientWidth;
          GetColors(y, cc);
          for x := 0 to 4 do
          begin
            case pels[x, y] of
              0: c := 0;
              2: c := cc[1];
              3: c := cc[2];
              else c := CWindowBorder[pels[x, y]] or FTransparency;  // I2555
            end;

            p^[v + x] := c;
            p^[v + ClientWidth - x - 1] := c;
          end;

        end;

        { Reset transparency after GDI operations }

        for y := 0 to ClientHeight - 1 do
        begin
          v := y * Clientwidth;
          if (y < ClientHeight - 5) and (y > 4) then
          begin
            X1 := 0;
            X2 := ClientWidth - 1;
          end
          else
          begin
            X1 := 5;
            X2 := ClientWidth - 6;
          end;

          for x := X1 to X2 do
            p^[v + x] := (p^[v + x] and $FFFFFF) or FTransparency;  // I2555
        end;

        {if FSelectedItemIndex >= 0 then
        begin
          FSelectionRect := FItemRects[FSelectedItemIndex];
          for y := FSelectionRect.Top to FSelectionRect.Bottom - 1 do
          begin
            v := (ClientHeight - y) * ClientWidth;
            for x := FSelectionRect.Left to FSelectionRect.Right - 1 do
              p^[v + x] := (p^[v + x] and $FFFFFF) or $FF000000;
          end;
        end;}
      end;

begin
  if dcLayered = 0 then Exit;  // I2554

  if Assigned(pvLayeredBits) and Assigned(pvBufferBits) and (hLayeredBitmap <> 0) then  // I2554
  begin
    SelectObject(dcLayered, GetStockObject(NULL_BRUSH));
    SelectObject(dcLayered, GetStockObject(NULL_PEN));
    SelectObject(dcLayered, GetStockObject(SYSTEM_FONT));

    GdiFlush;
    DrawCornersAndMakeTransparent;

    dcScreen := GetDC(Handle);
    if not UpdateLayeredWindow(Handle, dcScreen, @ptPos, @szWnd, dcLayered, @ptSrc, 0, @blend, ULW_ALPHA) then
      RaiseLastOSError;
    ReleaseDC(Handle, dcScreen);
  end;

  DeleteDC(dcLayered);
  dcLayered := 0;  // I2554
end;

procedure TfrmLanguageSwitch.SelectKeyboard(FromSelection: Boolean);
begin
  if not FromSelection then
  begin
    FSelectedKeyboard := FHoverKeyboard;
    FSelectedLanguage := FHoverLanguage;
  end;

  Hide;
end;

procedure TfrmLanguageSwitch.CancelDialog;
begin
  Hide;
end;

procedure TfrmLanguageSwitch.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or (WS_EX_NOACTIVATE or WS_EX_TOPMOST or WS_EX_LAYERED);
end;

procedure TfrmLanguageSwitch.DrawLanguageIcons(DrawSelection: Boolean);
const
  pels: array[0..5,0..5] of Byte =
    ((0,0,0,0,0,1),
     (0,0,0,1,1,4),
     (0,0,1,4,2,2),
     (0,1,4,2,2,2),
     (0,1,2,2,2,2),
     (1,4,2,2,2,2));

     function BrushState(CellStatus: TObjectType): TObjectType;
     begin
       Result := TCellStatus(Ord(CellStatus)+4);
     end;

var
  bx, by, x, y, I: Integer;
  j: Integer;
  FTextRect: TRect;
  FRowRect: TRect;
  FCellRect: TRect;
  CellStatus: TCellStatus;
  FIconRect: TRect;
  szHotkey, sz: TSize;
  cc0,cc1: array[0..3] of Cardinal;
  c0, c1: Cardinal;
  Save: THandle;
  MaskDC: HDC;
  k, m, l: Integer;
  FKeyboard_Tip: TLangSwitchKeyboard_TIP;
  FHotkeyText: string;
  FKeymanLanguage: IKeymanLanguage;
begin
  with FMetrics do
  begin
    SetBkMode(dcLayered, TRANSPARENT);

    for I := 0 to Manager.LanguageCount - 1 do   // I4191
    begin
      if DrawSelection and (FHoverLanguage <> I) and (FSelectedLanguage <> I) then Continue;

      x := MWindowMargin.Left + (I mod ColCount) * (CellWidth + MCellSpacing.X);
      y := MWindowMargin.Top + GetRowY(I div ColCount);  // I2669

      FCellRect := Types.Rect(x, y, x + CellWidth, y + CellHeight[I]);  // I2669

      if not DrawSelection then
        CellStatus := csNormal
      else if FHoverLanguage = I then
        if FSelectedLanguage = I
          then CellStatus := csHoverSelected
          else CellStatus := csHover
      else if FSelectedLanguage = I
        then CellStatus := csSelected
        else CellStatus := csNormal;

      { Draw border }
      SelectObject(dcLayered, hPens[CellStatus]);
      SelectObject(dcLayered, hBrushes[BrushState(CellStatus)]);
      Rectangle(dcLayered, FCellRect.Left, FCellRect.Top, FCellRect.Right, FCellRect.Bottom);

      for by := 0 to 5 do
      begin
        GetColors(ClientHeight-FCellRect.Top-by, cc0);
        GetColors(ClientHeight-FCellRect.Bottom+by-1, cc1);
        for bx := 0 to 5 do
        begin
          if pels[bx,by] = 0 then
          begin
            c0 := cc0[2]; c0 := ((c0 and $FF) shl 16) or ((c0 and $FF00)) or ((c0 and $FF0000) shr 16);
            c1 := cc1[2]; c1 := ((c1 and $FF) shl 16) or ((c1 and $FF00)) or ((c1 and $FF0000) shr 16);
          end
          else
          begin
            c0 := CCellBorder[CellStatus,pels[bx,by]]; c1 := c0;
          end;
          SetPixelV(dcLayered, FCellRect.Left+bx, FCellRect.Top+by, c0);
          SetPixelV(dcLayered, FCellRect.Right-bx-1, FCellRect.Top+by, c0);

          SetPixelV(dcLayered, FCellRect.Right-bx-1, FCellRect.Bottom-by-1, c1);
          SetPixelV(dcLayered, FCellRect.Left+bx, FCellRect.Bottom-by-1, c1);
        end;
      end;
      SelectObject(dcLayered, hLanguageFont);

      { Draw language title }

      Inc(x, MCellMargin.Left);
      Inc(y, MCellMargin.Top);

      ExtTextOutW(dcLayered, x + MLanguageTextPosition.X, y + MLanguageTextPosition.Y, 0, nil, PWideChar(Manager.Languages[i].Caption),
        Length(Manager.Languages[i].Caption), nil);   // I4191

      { Draw language icon }

      SelectObject(dcLayered, GetStockObject(NULL_PEN));
      SelectObject(dcLayered, hBrushes[ptLanguageIcon]);
      SelectObject(dcLayered, hLanguageIconFont);
      SetTextColor(dcLayered, CLanguageIconText);

      FIconRect := Types.Rect(x + MLanguageIconPosition.X, y + MLanguageIconPosition.Y, x + MLanguageIconPosition.X + 16, y + MLanguageIconPosition.Y + 16);

      Rectangle(dcLayered, FIconRect.Left, FIconRect.Top, FIconRect.Right + 1, FIconRect.Bottom + 1);

      GetTextExtentPoint32W(dcLayered, PWideChar(Manager.Languages[i].IconText), Length(Manager.Languages[i].IconText), sz);   // I4191

      ExtTextOutW(dcLayered, FIconRect.Left + 8 - sz.cx div 2, FIconRect.Top + 8 - sz.cy div 2 - 1,
        ETO_CLIPPED, @FIconRect, PWideChar(Manager.Languages[i].IconText),   
        Length(Manager.Languages[i].IconText), nil);   // I4191

      SetPixelV(dcLayered, FIconRect.Left, FIconRect.Top, CCellBorder[CellStatus,2]);
      SetPixelV(dcLayered, FIconRect.Right - 1, FIconRect.Top, CCellBorder[CellStatus,2]);
      SetPixelV(dcLayered, FIconRect.Right - 1, FIconRect.Bottom - 1, CCellBorder[CellStatus,2]);
      SetPixelV(dcLayered, FIconRect.Left, FIconRect.Bottom - 1, CCellBorder[CellStatus,2]);

      { Draw keyboard list }

      Inc(x, MKeyboardsMargin.Left);
      Inc(y, TitleTextHeight + MKeyboardsMargin.Top);

      SelectObject(dcLayered, hKeyboardFont);
      for j := 0 to Manager.Languages[i].KeyboardCount - 1 do   // I4191
      begin
        FRowRect := Rect(x, y, x + KeyboardRowWidth, y + KeyboardRowHeight);
        FTextRect := Rect(
          FRowRect.Left + MKeyboardMargin.Left + MKeyboardTextPosition.X,
          FRowRect.Top + MKeyboardMargin.Top + MKeyboardTextPosition.Y,
          FRowRect.Right - MKeyboardMargin.Right,
          FRowRect.Top + MKeyboardMargin.Top + MKeyboardTextPosition.Y + KeyboardTextHeight);

        if (FHoverLanguage = i) and (FHoverKeyboard = j) and DrawSelection then
        begin
          SelectObject(dcLayered, hBrushes[ptHighlight]);
          SetTextColor(dcLayered, CHighlightText);
        end
        else if (FSelectedKeyboard = j) and (FSelectedLanguage = i) and DrawSelection then
        begin
          SelectObject(dcLayered, hBrushes[ptSelection]);
          SetTextColor(dcLayered, CSelectionText);
        end
        else
        begin
          SelectObject(dcLayered, hBrushes[BrushState(CellStatus)]);
          SetTextColor(dcLayered, CNormalText);
        end;

        SelectObject(dcLayered, GetStockObject(NULL_PEN));

        Rectangle(dcLayered, FRowRect.Left, FRowRect.Top, FRowRect.Right + 1, FRowRect.Bottom + 1);
        SetPixelV(dcLayered, FRowRect.Left, FRowRect.Top, CCellBorder[CellStatus,2]);
        SetPixelV(dcLayered, FRowRect.Right-1, FRowRect.Top, CCellBorder[CellStatus,2]);
        SetPixelV(dcLayered, FRowRect.Right-1, FRowRect.Bottom-1, CCellBorder[CellStatus,2]);
        SetPixelV(dcLayered, FRowRect.Left, FRowRect.Bottom-1, CCellBorder[CellStatus,2]);

        if (I = FCurrentlySelectedLanguage) and (J = FCurrentlySelectedKeyboard) then   // I4191
        begin
          // I2674 - Show the Selection with a little triangle on the left
          for k := 0 to 8 do
          begin
            if k > 4 then m := 8 - k else m := k;
            for l := 0 to m do
              SetPixelV(dcLayered, FRowRect.Left - MKeyboardsMargin.Left + l + 2, FRowRect.Top + 5 + k, CNormalText);
          end;
        end;

        ExtTextOutW(dcLayered, FTextRect.Left, FTextRect.Top, ETO_CLIPPED, @FTextRect,
          PWideChar(Manager.Languages[i].Keyboards[j].Caption),
          Length(Manager.Languages[i].Keyboards[j].Caption), nil);   // I4191

        FKeymanLanguage := Manager.Languages[i].Keyboards[j].KeymanLanguage;
        if Assigned(FKeymanLanguage) and (FKeymanLanguage.Hotkey.RawValue <> 0) then
        begin
          FHotkeyText := ShortcutToTextEx(HotkeyToShortcut(FKeymanLanguage.Hotkey));
          GetTextExtentPoint32W(dcLayered, PWideChar(FHotkeyText), Length(FHotkeyText), szHotkey);   // I4191
          ExtTextOutW(dcLayered, FTextRect.Right - szHotkey.cx, FTextRect.Top, ETO_CLIPPED, @FTextRect,
            PWideChar(FHotkeyText), Length(FHotkeyText), nil);   // I4191
        end;

        if Assigned(Manager.Languages[i].Keyboards[j].Bitmap) then   // I4191
        begin
          Save := 0;  // I2593
          MaskDC := 0;
          try
            if Manager.Languages[i].Keyboards[j] is TLangSwitchKeyboard_TIP then   // I4204
            begin
              FKeyboard_Tip := Manager.Languages[i].Keyboards[j] as TLangSwitchKeyboard_TIP;
              if FKeyboard_Tip.IconHandle <> 0 then
                DrawIconEx(dcLayered,
                  FRowRect.Left + MKeyboardMargin.Left + MKeyboardIconPosition.X,
                  FRowRect.Top + MKeyboardMargin.Top + MKeyboardIconPosition.Y,
                  FKeyboard_Tip.IconHandle,
                  16, 16, 0, 0, DI_NORMAL)
              else
                FKeyboard_Tip := nil;
            end
            else
              FKeyboard_Tip := nil;
            if FKeyboard_Tip = nil then
            begin
              MaskDC := CreateCompatibleDC(0);
              Save := SelectObject(MaskDC, Manager.Languages[i].Keyboards[j].Bitmap.MaskHandle);   // I4191
              TransparentStretchBlt(dcLayered,
                FRowRect.Left + MKeyboardMargin.Left + MKeyboardIconPosition.X,
                FRowRect.Top + MKeyboardMargin.Top + MKeyboardIconPosition.Y,
                16, 16, Manager.Languages[i].Keyboards[j].Bitmap.Canvas.Handle, 0, 0, 16, 16, MaskDC, 0, 0);   // I4191
            end;
          finally
            if Save <> 0 then SelectObject(MaskDC, Save);
            if MaskDC <> 0 then DeleteDC(MaskDC);
          end;

                       {
          BitBlt(dcLayered,
            FRowRect.Left + MKeyboardMargin.Left + MKeyboardIconPosition.X,
            FRowRect.Top + MKeyboardMargin.Top + MKeyboardIconPosition.Y,
            16, 16, FManager.Languages[i].Keyboards[j].Bitmap.Canvas.Handle,
            0, 0, SRCCOPY);
            }
        end;

        Inc(y, KeyboardRowHeight);

        SetTextColor(dcLayered, $000000);
      end;
    end;
  end;
end;

procedure TfrmLanguageSwitch.SelectItem(Item: TLangSwitchKeyboard);   // I4191
var
  I: Integer;
  J: Integer;
begin
  FSelectedKeyboard := 0;
  FSelectedLanguage := 0;
  for I := 0 to Manager.LanguageCount - 1 do
    for J := 0 to Manager.Languages[i].KeyboardCount - 1 do
      if Manager.Languages[I].Keyboards[J] = Item then
      begin
        FSelectedKeyboard := J;
        FSelectedLanguage := I;
        Break;
      end;
  FCurrentlySelectedKeyboard := FSelectedKeyboard;
  FCurrentlySelectedLanguage := FSelectedLanguage;
end;

end.
