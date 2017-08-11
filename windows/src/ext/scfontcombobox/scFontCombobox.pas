{--------------------------------------------------------------------------------
* Description : TscFontComboBox
  Font combobox with
    - small images for truetype ansi/symbol and printer fonts
    - fontnames displayed in font of item
    - small preview window 
    - last used fonts at top above line and in given color
* Dates : March 2002 - December 2004
* Version : 1.1

* Author : Stefan Cruysberghs
* Email : stefancr@scip.be
* Website : http://www.scip.be
  Visit SCIP.be for other components, tools, articles, ...
--------------------------------------------------------------------------------
* $Archive: /Component Library/SC_Public/scFontCombobox.pas $
* $Author: Stefancr $
* $Date: 19/06/03 15:59 $
* $Modtime: 3/09/02 21:53 $
* $Revision: 1 $
--------------------------------------------------------------------------------
* This component is free of charge.
* The author doesn't give a warranty for error free running
  of this component and he doesn't give any support.
* Suggestions and bugs can be send by email.
--------------------------------------------------------------------------------
Installation
* Install the TscFontComboBox component by adding the unit scFontComboBox.pas
  to a package. Remove the DCR file in the package. Compile and Install the package.
* The TscFontComboBox component can be found in the tabsheet 'SC'
  of the component palette.
--------------------------------------------------------------------------------
* History
Version 1.1 (3/12/2004)
- Added ShowPreviewInEdit property
Version 1.0 (1/3/2002)
- First version of component
--------------------------------------------------------------------------------}

//
// mcd 2017-04-19 tweaked display in list to fix colour and clipping
//
unit scFontCombobox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TFormExampleFont = class(TForm)
    PanelPreview: TPanel;
    PanelFontName: TPanel;
  private
    CurrentUser: TCustomComboBox;
  public
  end;

  TscFontType = (ftTrueTypeAnsi, ftTrueTypeSymbol, ftRaster);
  TscFontTypes = set of TscFontType;

  TscFontComboBox = class(TCustomComboBox)
  private
    BitmapTrueTypeAnsi : TBitmap;
    BitmapTrueTypeSymbol : TBitmap;
    BitmapRaster : TBitmap;
    IntCountUsed : Integer;
    BlnDown : Boolean;

    FIntPreviewWidth : Integer;
    FIntPreviewHeight : Integer;
    FStrFontName : String;
    FStrPreviewText : String;
    FBlnMoveUsedToTop : Boolean;
    FIntMaxUsed : Integer;
    FColorUsed : TColor;
    FFontTypes : TscFontTypes;
    FBlnShowPreviewInList: Boolean;
    FBlnShowPreview: Boolean;
    FBlnShowImagesFontType: Boolean;
    FBlnShowPreviewFontName: Boolean;
    FBlnShowPreviewInEdit: Boolean;

    procedure SetPopupHeight(const Value: Integer);
    procedure SetPopupWidth(const Value: Integer);
    procedure SetFontName(const Value: String);
    procedure SetPreviewText(const Value: String);
    procedure SetShowPreviewFontName(const Value: Boolean);
    function UsingFormExample: Boolean;
    function GetFontName: String;
  protected
    procedure OnCloseup(var Message: TWMCommand); message CN_COMMAND;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);override;
    procedure ChooseFont;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;

    procedure DropDown; override;
    procedure Click; override;

    // Refill items of listbox depening the fonttypes property
    procedure GetFontNames;
  published
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;

    property Anchors;
    property Color;
    property Ctl3D;
    property Cursor;
    property DragCursor;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property Height;
    property HelpContext;
    property Hint;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property Left;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopUpMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    // Add used fonts to top of listbox
    property MoveUsedToTop : Boolean read FBlnMoveUsedToTop write FBlnMoveUsedToTop default True;
    property MaxUsed : Integer read FIntMaxUsed write FIntMaxUsed default 5;
    property ColorUsed : TColor read FColorUsed write FColorUsed default clNavy;

    // Preview popup window
    // When previewtext is not specified, AaBbYyZz will be shown
    property PreviewText : String read FStrPreviewText write SetPreviewText;
    property PreviewWidth : Integer read FIntPreviewWidth write SetPopupWidth default 250;
    property PreviewHeight : Integer read FIntPreviewHeight write SetPopupHeight default 45;

    // Get or set font
    property FontName : String read GetFontName write SetFontName;

    // Fonttypes which will be visible (ftTrueTypeAnsi, ftTrueTypeSymbol, ftRaster)
    property FontTypes : TscFontTypes read FFontTypes write FFontTypes;

    // Show preview popup window
    property ShowPreview : Boolean read FBlnShowPreview write FBlnShowPreview default True;
    // Show small panel with fontname in preview popup window
    property ShowPreviewFontName : Boolean read FBlnShowPreviewFontName write SetShowPreviewFontName default True;
    // Show preview of item in listbox
    property ShowPreviewInList : Boolean read FBlnShowPreviewInList write FBlnShowPreviewInList default True;
    // Show preview of item in edit of combobox (choosen font)
    property ShowPreviewInEdit : Boolean read FBlnShowPreviewInEdit write FBlnShowPreviewInEdit default False;
    // Show small images depending the fonttype
    property ShowImagesFontType : Boolean read FBlnShowImagesFontType write FBlnShowImagesFontType default True;
  end;

procedure Register;

implementation

{$R scFontCombobox.dfm}
{$R scFontCombobox.res}
{$R scFontCombobox.dcr}

var
  FormExample : TFormExampleFont = nil;


procedure Register;
begin
  RegisterComponents('SC', [TscFontComboBox]);
end;

//------------------------------------------------------------------------------
constructor TscFontComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then
  begin
    if not Assigned(FormExample) then
    begin
      FormExample := TFormExampleFont.Create(nil);
      FormExample.Height := 0;
      FormExample.Width := 0;
      FormExample.Color := clWhite;
      FormExample.Visible := True;
      ShowWindow(FormExample.Handle,SW_HIDE);
    end;

    BitmapTrueTypeAnsi := TBitmap.Create;
    BitmapTrueTypeAnsi.LoadFromResourceName(HInstance,'FONTTRUETYPEANSI');
    BitmapTrueTypeAnsi.Transparent:=True;
    BitmapTrueTypeSymbol := TBitmap.Create;
    BitmapTrueTypeSymbol.LoadFromResourceName(HInstance,'FONTTRUETYPESYMBOL');
    BitmapTrueTypeSymbol.Transparent:=True;
    BitmapRaster := TBitmap.Create;
    BitmapRaster.LoadFromResourceName(HInstance,'FONTRASTER');
    BitmapRaster.Transparent:=True;

    FormExample.Width := 240;
    FormExample.Height := 60;
  end;

  FIntPreviewWidth := 240;
  FIntPreviewHeight := 60;
  FFontTypes := [ftTrueTypeAnsi,ftTrueTypeSymbol];
  FBlnMoveUsedToTop := True;
  FIntMaxUsed := 5;
  FColorUsed := clNavy;
  FBlnShowPreviewInList := True;
  FBlnShowPreviewInEdit := False;
  FBlnShowPreview := True;
  FBlnShowPreviewFontName := True;
  FBlnShowImagesFontType := True;

  IntCountUsed := 0;
  BlnDown := False;

  Style := csOwnerDrawFixed;
  ItemHeight := 18;
  DropDownCount := 12;
  Width := 200;
  Font.Size := 10;
end;

//------------------------------------------------------------------------------
destructor TscFontComboBox.Destroy;
begin
  BitmapTrueTypeAnsi.Free;
  BitmapTrueTypeSymbol.Free;
  BitmapRaster.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
procedure TscFontComboBox.Loaded;
begin
  inherited;
  GetFontNames;
end;

//------------------------------------------------------------------------------
procedure TscFontComboBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  StrFont : String;
  StrFontType : String;
  v: Integer;
begin
  if csDesigning in ComponentState then
    Exit;

  with Canvas do
  begin
    StrFont := Items.Names[Index];
    StrFontType := Items.Values[StrFont];

    FillRect(Rect);

    // Color for used itmes can be different
    if not (odFocused in State) then
      if (BlnDown) and (Index < IntCountUsed) then
        Font.Color := FColorUsed
      else
        Font.Color := Self.Font.Color;

    // If property ShowPreviewInList is true the current
    // font will be the fontname of the item
    if (FBlnShowPreviewInList and (not (odComboBoxEdit in State)))
      or (FBlnShowPreviewInEdit and (odComboBoxEdit in State)) then
    begin
      if Items.Values[StrFont] <> 'TS' then
        Font.Name := StrFont
      else
        Font.Name := Self.Font.Name;
    end;

    v := TextHeight(StrFont);
    if FBlnShowImagesFontType then
      TextRect(Rect, 20, (Rect.Top + Rect.Bottom - v) div 2, StrFont)
    else
      TextRect(Rect, 4, (Rect.Top + Rect.Bottom - v) div 2, StrFont);

    // Show small image depending the fonttype
    if FBlnShowImagesFontType then
    begin
      if (StrFontType = 'TA') then
        Draw(2,Rect.Top+1,BitmapTrueTypeAnsi)
      else
        if (StrFontType = 'TS') then
          Draw(2,Rect.Top+1,BitmapTrueTypeSymbol)
        else
          Draw(2,Rect.Top+1,BitmapRaster);
    end;

    // Show preview popupwindow for focused item
    if (FBlnShowPreview) and (odFocused in State) then
    begin
      if UsingFormExample then
      begin
        FormExample.PanelPreview.Font.Name := StrFont;
        if Trim(FStrPreviewText) = '' then
          FormExample.PanelPreview.Caption := 'AaBbYyZz';

        FormExample.PanelFontName.Caption := StrFont;
      end;
    end;

    // Draw line after used fonts
    if (BlnDown) and (FBlnMoveUsedToTop) and (Index = (IntCountUsed - 1)) then
    begin
      MoveTo(0,Rect.Bottom-1);
      LineTo(Width,Rect.Bottom-1);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TscFontComboBox.DropDown;
var
  Point : TPoint;
begin
  BlnDown := True;

  inherited Dropdown;

  // Set position of preview popup window
  if FBlnShowPreview then
  begin
    FormExample.CurrentUser := Self;

    FormExample.Width := FIntPreviewWidth;
    FormExample.Height := FIntPreviewHeight;
    FormExample.PanelPreview.Caption := FStrPreviewText;
    FormExample.PanelFontName.Visible := FBlnShowPreviewFontName;

    Point.x := (Self.Left)+ Self.width;
    Point.y := (Self.Top)+ Self.height ;
    Point := Parent.ClientToScreen(Point);
    FormExample.Top := Point.y;
    FormExample.Left := Point.x;

    if FormExample.Left + FormExample.Width > Screen.Width then
    begin
      Point.x := (Self.Left);
      Point := Parent.ClientToScreen(Point);
      FormExample.Left := Point.x - FormExample.Width;
    end;

    if FormExample.Top + FormExample.Height > Screen.Height then
    begin
      Point.y := (Self.Top);
      Point := Parent.ClientToScreen(Point);
      FormExample.Top := Point.y - FormExample.Height;
    end;

    ShowWindow(FormExample.Handle, SW_SHOWNA);
  end;
end;

//------------------------------------------------------------------------------
procedure TscFontComboBox.Click;
begin
  if BlnDown = False then
  begin
    FStrFontName := Items.Names[ItemIndex];
    inherited Click;
  end;
end;

//------------------------------------------------------------------------------
procedure TscFontComboBox.OnCloseup(var Message: TWMCommand);
begin
  if Message.NotifyCode = CBN_CLOSEUP then
  begin
    Self.SetFocus;
    ShowWindow(FormExample.Handle,SW_HIDE);
    FormExample.CurrentUser := nil;

    ChooseFont;

    inherited Click;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------
procedure TscFontComboBox.ChooseFont;
var
  IntIndex : Integer;
  BlnAlreadyUsed : Boolean;
begin
  if ItemIndex = -1 then
    Exit;

  FStrFontName := Items[ItemIndex];
  Text := FStrFontName;

  BlnDown := False;
  if (FBlnMoveUsedToTop = True) and (ItemIndex <> 0) then
  begin

    // Test if font has already been used
    BlnAlreadyUsed := False;
    IntIndex:=0;
    while (not BlnAlreadyUsed) and (IntIndex < IntCountUsed) do
    begin
      BlnAlreadyUsed := (Items[IntIndex] = FStrFontName);
      Inc(IntIndex);
    end;

    // Insert item at top when font is not used yet
    // Otherwise move item from used list to top
    if not BlnAlreadyUsed then
    begin
      Items.Insert(0,FStrFontName);
      Inc(IntCountUsed);
    end
    else
    begin
      Items.Move(IntIndex-1,0);
    end;

    // When maximum used items is reached, delete last item
    if (FIntMaxUsed <> 0) and (IntCountUsed > FIntMaxUsed) then
    begin
      Items.Delete(FIntMaxUsed);
      Dec(IntCountUsed);
    end;

    ItemIndex := 0;
  end;
end;

//------------------------------------------------------------------------------
function TscFontComboBox.GetFontName: String;
begin
  Result := FStrFontName;
  if Pos('=', Result) > 0 then Delete(Result, Pos('=', Result), MaxInt);
end;

procedure TscFontComboBox.GetFontNames;
type
  TFontRec = record 
    FontTypes : TscFontTypes;
    FontItems : TStrings;
  end;
var
  DC: HDC;
  FontRec : TFontRec;

  function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
    FontType: Integer; Data: Pointer): Integer; stdcall;
  var
    StrType : String;
  begin
    StrType :='';
    if FontType = TRUETYPE_FONTTYPE then
      case LogFont.lfCharset of
        ANSI_CHARSET : StrType := 'TA';
        SYMBOL_CHARSET : StrType := 'TS';
      end
    else
      if FontType = RASTER_FONTTYPE then
        StrType := 'R';

    // Check which fonts have to be added to listbox
    if (LogFont.lfFaceName[0] <> '@')
      and
        (((StrType = 'TA') and (ftTrueTypeAnsi in TFontRec(Data^).FontTypes)) or
         ((StrType = 'TS') and (ftTrueTypeSymbol in TFontRec(Data^).FontTypes)) or
         ((StrType = 'R') and (ftRaster in TFontRec(Data^).FontTypes))) then
    begin
      TStrings(TFontRec(Data^).FontItems).Add(string(LogFont.lfFaceName)+'='+StrType);
    end;

    Result := 1;
  end;
begin
  IntCountUsed := 0;
  Items.Clear;

  // Make record to provide a pointer to the items
  // of the listbox and the property fonttypes
  FontRec.FontItems := Pointer(Items);
  FontRec.FontTypes := FFontTypes;

  DC := GetDC(0);
  try
    EnumFonts(DC, nil, @EnumFontsProc, @FontRec);
  finally
    ReleaseDC(0, DC);
  end;
  Sorted := True;
end;

//------------------------------------------------------------------------------
procedure TscFontComboBox.SetPopupHeight(const Value: Integer);
begin
  FIntPreviewHeight := Value;
  if Assigned(FormExample) then
    FormExample.Height := Value;
end;

//------------------------------------------------------------------------------
procedure TscFontComboBox.SetPopupWidth(const Value: Integer);
begin
  FIntPreviewWidth := Value;
  if Assigned(FormExample) then
    FormExample.Width := Value;
end;

//------------------------------------------------------------------------------
procedure TscFontComboBox.SetFontName(const Value: String);
begin
  // Run through items to see if font is available in list
  ItemIndex := Items.IndexOfName(Value);
  ChooseFont;
end;

function TscFontComboBox.UsingFormExample: Boolean;
begin
  Result := Assigned(FormExample) and (FormExample.CurrentUser = Self);
end;
//------------------------------------------------------------------------------
procedure TscFontComboBox.SetPreviewText(const Value: String);
begin
  FStrPreviewText := Value;
  if UsingFormExample then
    FormExample.PanelPreview.Caption := FStrPreviewText;
end;

//------------------------------------------------------------------------------
procedure TscFontComboBox.SetShowPreviewFontName(const Value: Boolean);
begin
  FBlnShowPreviewFontName := Value;
  if UsingFormExample then
    FormExample.PanelFontName.Visible := FBlnShowPreviewFontName;
end;

initialization
finalization
  FreeAndNil(FormExample);
end.

