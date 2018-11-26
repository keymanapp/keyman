(*
  Name:             KeymanMenuItem
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    6 Nov 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Initial version
                    14 Sep 2006 - mcdurdin - Add icon (to avoid painful stupidities with TMenuItem.Bitmap transparency) and use cleartype render for menu items
                    04 Dec 2006 - mcdurdin - Disabled options in gray
                    15 Jan 2007 - mcdurdin - Read font name and size properties
                    19 Mar 2007 - mcdurdin - Clean up font color and caption trimming
                    23 Aug 2007 - mcdurdin - I933 - Fix width of text in menu not calculated correctly
                    27 Mar 2008 - mcdurdin - I1352 - Show hint in keyboard menu when no keyboards are installed
                    27 Mar 2008 - mcdurdin - I1371 - Improve colour of menu left hand bar
                    27 Mar 2008 - mcdurdin - I1371 - Polish display of menu
                    16 Jan 2009 - mcdurdin - I1630 - Hotkey not displayed in menu
                    14 May 2010 - mcdurdin - I2226 - Improve Keyman menu
                    31 Jan 2011 - mcdurdin - I2613 - "No keyboards" message is chopped on the right
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    01 Dec 2012 - mcdurdin - I3614 - V9.0 - Start removal of keyboard selection list from UI
                    24 Oct 2013 - mcdurdin - I3933 - V9.0 - Keyman tray icon menu is not showing installed keyboards
                    11 Nov 2013 - mcdurdin - I3960 - V9.0 - Reskin the Keyman menu with 9.0 style
                    06 Nov 2015 - mcdurdin - I4920 - Keyman menu for OEM products does not show indented menu items
*)
unit KeymanMenuItem;  // I3306

interface

uses
  System.UITypes,
  Windows, BitmapIPicture, Classes, Menus, custinterfaces, Graphics, keymanapi_TLB, SysUtils, Types, Controls,
  LangSwitchManager;

type
  TKeymanMenuItemType = (kmitNormal, kmitLanguage, kmitKeyboard, kmitLanguageKeyboard);   // I3933

  TKeymanMenuItem = class(TMenuItem)
  private
    FCMIItemType: TCustomisationMenuItemType;   // I3960
    FDefaultBitmap: TBitmapIPicture;
    FSelectedBitmap: TBitmapIPicture;
    FWideCaption: WideString;
    FIcon: TBitmap;
    FFontSize: Integer;
    FFontName: WideString;
    FItemType: TKeymanMenuItemType;   // I3933
    FLanguageCode: string;   // I3933
    FKeyboard: TLangSwitchKeyboard;   // I3960
    FCustomisationMenuItem: IKeymanCustomisationMenuItem;   // I3933   // I3960
    function GetDefaultBitmap: TBitmapIPicture;
    function GetSelectedBitmap: TBitmapIPicture;
    procedure SetWideCaption(const Value: WideString);
    procedure SetIcon(const Value: TBitmap);
    function GetIcon: TBitmap;
    procedure DrawItemShared(ACanvas: TCanvas; ARect: TRect;   // I4920
      State: TOwnerDrawState; BarColor, SelColor, SelBorderColor: TColor);
  protected
    procedure AdvancedDrawItem(ACanvas: TCanvas; ARect: TRect;
      State: TOwnerDrawState; TopLevel: Boolean); override;
    procedure DoMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawItem80(ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);

    property Keyboard: TLangSwitchKeyboard read FKeyboard write FKeyboard;   // I3933
    property ItemType: TKeymanMenuItemType read FItemType write FItemType;   // I3933

    property Icon: TBitmap read GetIcon write SetIcon;
    property CMIItemType: TCustomisationMenuItemType read FCMIItemType write FCMIItemType;   // I3960
    property CustomisationMenuItem: IKeymanCustomisationMenuItem read FCustomisationMenuItem write FCustomisationMenuItem;
    property SelectedBitmap: TBitmapIPicture read GetSelectedBitmap;
    property DefaultBitmap: TBitmapIPicture read GetDefaultBitmap;
    property WideCaption: WideString read FWideCaption write SetWideCaption;
    property LanguageCode: string read FLanguageCode write FLanguageCode;   // I3933
    property FontName: WideString read FFontName write FFontName;
    property FontSize: Integer read FFontSize write FFontSize;
  end;

implementation

uses
  CleartypeDrawCharacter,
  ImgList,
  Keyman.System.Util.RenderLanguageIcon,
  Math,
  utilhotkey,
  WideStrings;

{$R keymanmenuitem.res}

var
  FCheckedBitmap: TBitmap = nil;
  FInfoBitmap: TBitmap = nil;
  FCleartypeDrawCharacter: TClearTypeDrawCharacter = nil;

constructor TKeymanMenuItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnMeasureItem := Self.DoMeasureItem;
end;

destructor TKeymanMenuItem.Destroy;
begin
  FreeAndNil(FSelectedBitmap);
  FreeAndNil(FDefaultBitmap);
  FreeAndNil(FIcon);
  inherited Destroy;
end;

function TKeymanMenuItem.GetDefaultBitmap: TBitmapIPicture;
begin
  if not Assigned(FDefaultBitmap) then
    FDefaultBitmap := TBitmapIPicture.Create;
  Result := FDefaultBitmap;
end;

function TKeymanMenuItem.GetIcon: TBitmap;
begin
  if not Assigned(FIcon) then
    FIcon := TBitmap.Create;
  Result := FIcon;
end;

function TKeymanMenuItem.GetSelectedBitmap: TBitmapIPicture;
begin
  if not Assigned(FSelectedBitmap) then
    FSelectedBitmap := TBitmapIPicture.Create;
  Result := FSelectedBitmap;
end;

procedure TKeymanMenuItem.SetIcon(const Value: TBitmap);
begin
  if not Assigned(FIcon) then FIcon := TBitmap.Create;
  FIcon.Assign(Value);
end;

procedure TKeymanMenuItem.SetWideCaption(const Value: WideString);
begin
  FWideCaption := Trim(Value);
  Caption := Trim(Value); { for separators, etc } 
end;

procedure TKeymanMenuItem.DoMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
var
  sz: TPoint;
  s: WideString;
begin
  if FFontName <> '' then ACanvas.Font.Name := FFontName;
  if FFontSize <> 0 then ACanvas.Font.Size := FFontSize;

  case FCMIItemType of   // I3960
    mitGraphic:
      begin
        Width := FDefaultBitmap.Width - 12;
        Height := FDefaultBitmap.Height;
      end;
    mitText, _mitKeyboardsList:   // I3614   // I3933
      begin
        FCleartypeDrawCharacter.SetFontDetails(ACanvas.Font.Name, ACanvas.Font.Height, []);
        ACanvas.Font.Style := [];
        if Default then
          ACanvas.Font.Style := ACanvas.Font.Style + [fsBold];

        s := StripHotkey(FWideCaption);
        sz := FCleartypeDrawCharacter.TextExtent(ACanvas.Handle, s);
        //GetTextExtentPoint32W(ACanvas.Handle, PWideChar(s), Length(s), sz);
        Width := sz.x + 23 + 7;

        if FItemType = kmitKeyboard then Inc(Width, 32);   // I3933

        if ShortCut <> 0 then
          Width := Width + FCleartypeDrawCharacter.TextExtent(ACanvas.Handle, ShortCutToTextEx(ShortCut)).X + 7 + 7;
        Height := Max(sz.y, FCheckedBitmap.Height); // 22;
        if Assigned(FIcon) then
          Height := Max(Height, FIcon.Height + 6);
      end;
    mitSeparator:
      begin
        Width := 30;
        Height := 3;
      end;
  end;
end;

procedure TKeymanMenuItem.DrawItem80(ACanvas: TCanvas; ARect: TRect;
  State: TOwnerDrawState);
const
  BarColor: TColor = $D3B86A; //$B5D7DB; //..$DEEDEF;   // I3960
  SelColor: TColor = $D3B86A; //$748BE0; // $DEE5F3; // $DBE3FF;
  SelBorderColor: TColor = $D3B86A; //$284AAD; //4F70DB;
begin
  DrawItemShared(ACanvas, ARect, State, BarColor, SelColor, SelBorderColor);   // I4920
end;

procedure TKeymanMenuItem.DrawItemShared(ACanvas: TCanvas; ARect: TRect;
  State: TOwnerDrawState; BarColor, SelColor, SelBorderColor: TColor);   // I4920
const
  Alignments: array[TPopupAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  EdgeStyle: array[Boolean] of Longint = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
var
  Selected: Boolean;

  procedure GraphicDraw;
  begin
    if Selected and Assigned(FSelectedBitmap) and not FSelectedBitmap.Empty
      then ACanvas.Draw(ARect.Left, ARect.Top, FSelectedBitmap)
      else ACanvas.Draw(ARect.Left, ARect.Top, FDefaultBitmap);
  end;

  procedure NiceDraw;
  var
    x, y: Integer;
    r: TRect;
    S: string;
    h: Integer;
  begin
    with ACanvas do
    begin
      { Draw bitmap }
        //Draw(ARect.Left, ARect.Top, FCheckedBitmap);

      { Draw background }
      if Selected then
      begin
        Pen.Style := psSolid;
        Pen.Color := SelBorderColor;
        Brush.Color := SelColor;
        Brush.Style := bsSolid;
        Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom - 1);   // I3960
      end;

      { Indent multiple keyboards under a language }

      if FItemType = kmitKeyboard then   // I3933
        Inc(ARect.Left, 32);

      if Checked then
      begin
        Pen.Color := SelBorderColor;
        if Assigned(FIcon) then
        begin
          x := FIcon.Width;
          y := FIcon.Height;
        end
        else
        begin
          x := 16;
          y := 16;
        end;
        if not Selected then   // I3960
        begin
          Brush.Style := bsClear;
          Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Top + 6 + y);
        end;
        Brush.Color := clWhite;
        Brush.Style := bsSolid;
        Rectangle(ARect.Left, ARect.Top, ARect.Left + 6 + x, ARect.Top + 6 + y);
      end;

      Brush.Style := bsClear;

      if Assigned(FIcon) then
      begin
        { do a transparent blt for the bitmap }
        Draw(ARect.Left + 3, ARect.Top + 3, FIcon);
      end
      else if FItemType in [kmitLanguage, kmitLanguageKeyboard] then   // I3933
      begin
        DrawLanguageIcon(ACanvas, ARect.Left + 3, ARect.Top + 3, FLanguageCode);   // I3960
      end;

      //if not Selected then
      Brush.Style := bsClear;

      r := Rect(ARect.Left + 30, ARect.Top, ARect.Right, ARect.Bottom);

      if Enabled
        then Font.Color := clBlack
        else Font.Color := clGray;

      if FWideCaption = cLineCaption then
      begin
        Pen.Color := clBlack;
        MoveTo(ARect.Left, ARect.Top + 1);
        LineTo(ARect.Right, ARect.Top + 1);
      end
      else
      begin
        FCleartypeDrawCharacter.Color := clWindowText; //Font.Color;
        FCleartypeDrawCharacter.ShowPrefix := True;
        FCleartypeDrawCharacter.DisplayQuality := ctCleartype;
        FCleartypeDrawCharacter.SetFontDetails(Font.Name, Font.Height, []);
        h := FCleartypeDrawCharacter.TextExtent(Handle, 'A').Y;

        //SetBkMode(ACanvas.Handle, TRANSPARENT);

        FCleartypeDrawCharacter.DrawText(ACanvas.Handle, TA_TOP or TA_LEFT, r.Left, (r.Bottom + r.Top - h) div 2, r, FWideCaption);

        if ShortCut <> 0 then
        begin
          r := Rect(ARect.Left, ARect.Top, ARect.Right - 7, ARect.Bottom);
          S := ShortCutToTextEx(ShortCut);
          FCleartypeDrawCharacter.DrawText(ACanvas.Handle, TA_TOP or TA_RIGHT, r.Right, (r.Bottom + r.Top - h) div 2, r, s);
        end;
      end;
    end;
  end;

  procedure NoKeyboardsDraw;
  var
    I, h, y: Integer;
    r: TRect;
  begin
    with ACanvas do
    begin
      // I1352 - Split into multiple lines

      Brush.Color := $BEF6F5;
      Pen.Color := $a0a0a0;
      Rectangle(ARect.Left + 2, ARect.Top + 2, ARect.Right - 2, ARect.Bottom - 2);

      Draw(ARect.Left + 8, ARect.Top + 8, FInfoBitmap);

      Font.Color := clBlack;
      FCleartypeDrawCharacter.Color := clWindowText; //Font.Color;
      FCleartypeDrawCharacter.ShowPrefix := True;
      FCleartypeDrawCharacter.DisplayQuality := ctCleartype;
      FCleartypeDrawCharacter.SetFontDetails(Font.Name, Font.Height, []);

      r := Rect(ARect.Left + 55, ARect.Top + 4, ARect.Right - 8, ARect.Bottom - 8);

      h := FCleartypeDrawCharacter.TextExtent(Handle, 'A').Y;
      
      with TStringList.Create do
      try
        Text := Trim(FWideCaption);
        y := 0;
        for I := 0 to Count - 1 do
        begin
          FCleartypeDrawCharacter.DrawText(ACanvas.Handle, TA_TOP or TA_LEFT, r.Left, r.Top + y + 2, r, Strings[I]);
          Inc(y, h);
        end;
      finally
        Free;
      end;

      Brush.Color := clNone;
    end;
  end;

begin
  if FFontName <> '' then ACanvas.Font.Name := FFontName;
  if FFontSize <> 0 then ACanvas.Font.Size := FFontSize;

  Selected := odSelected in State;

  case FCMIItemType of   // I3960
    _mitKeyboardsList, mitText, mitSeparator: NiceDraw;   // I3614   // I3933
    mitGraphic: GraphicDraw;
  end;
end;

procedure TKeymanMenuItem.AdvancedDrawItem(ACanvas: TCanvas; ARect: TRect;
  State: TOwnerDrawState; TopLevel: Boolean);
begin
  ACanvas.Brush.Color := clMenu;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.FillRect(ARect);
  DrawItemShared(ACanvas, ARect, State,
    clMenuHighlight, clMenuHighlight, clMenuHighlight);   // I4920
end;

initialization
  FCheckedBitmap := TBitmap.Create;
  FCheckedBitmap.LoadFromResourceName(HInstance, 'keymanmenuitem_selected');
  FInfoBitmap := TBitmap.Create;
  FInfoBitmap.LoadFromResourceName(HInstance, 'keymanmenuitem_info');
  FCleartypeDrawCharacter := TClearTypeDrawCharacter.Create;
finalization
  FreeAndNil(FCheckedBitmap);
  FreeAndNil(FInfoBitmap);
  FreeAndNil(FCleartypeDrawCharacter);
end.
