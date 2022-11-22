(*
  Name:             UfrmDebugStatus_Elements
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      14 Sep 2006

  Modified Date:    30 May 2007
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          14 Sep 2006 - mcdurdin - Initial version
                    30 May 2007 - mcdurdin - I800 - Fix crash when resizing store elements
*)
unit UfrmDebugStatus_Elements;

interface

uses
  System.Contnrs,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, DebugListView, XString, Menus,
  UfrmDebugStatus_Child, Keyman.System.Debug.DebugEvent;

type
  TfrmDebugStatus_Elements = class(TfrmDebugStatus_Child)
    lvElements: TDebugListView;
    mnuPopupStores: TPopupMenu;
    mnuPopupStoresHexadecimalView: TMenuItem;
    procedure mnuPopupStoresHexadecimalViewClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure lvElementsCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure lvElementsCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure lvElementsCustomDraw(Sender: TCustomListView; const ARect: TRect;
      var DefaultDraw: Boolean);
    procedure lvElementsEnter(Sender: TObject);
    procedure lvElementsExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Element and Store functions }
    elements: TXStringElementList;
    FShowHexadecimal: TXStringFormatOptions;

    function FormatStoreDisplay(p: WideString; start, len: Integer): WideString;
    function GetStoreDisplayWidth(Element: TXStringElement): Integer;
    procedure ResizeStoreGrid;

  protected
    function GetHelpTopic: string; override;
    procedure DisplayFontChanged; override;
  public
    procedure UpdateStores(Event: TDebugEvent);
    procedure ClearStores;
  end;

implementation

uses
  Keyman.Developer.System.HelpTopics,

  KeymanDeveloperOptions,
  UfrmDebugStatus;

{$R *.dfm}

const
  LVM_FIRST = $1000;
  LVM_GETSUBITEMRECT = LVM_FIRST+56;
  LVIR_BOUNDS = 0;

procedure TfrmDebugStatus_Elements.ClearStores;
begin
  lvElements.Items.Clear;
  elements.Clear;
end;

procedure TfrmDebugStatus_Elements.lvElementsCustomDraw(Sender: TCustomListView;
  const ARect: TRect; var DefaultDraw: Boolean);
begin
  if (lvElements.Items.Count = 0) and lvElements.Focused then
    with lvElements.Canvas do
    begin
//      DefaultDraw := False;
      DrawFocusRect(Rect(0, 0, lvElements.Columns[0].Width, TextHeight('A')+1));
    end;
end;

procedure TfrmDebugStatus_Elements.lvElementsCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
var
  p1, p2, p3, p: WideString;
  sz1, sz2: TSize;
  r: TRect;
  Element: TXStringElement;
  hOldFont: HFONT;
begin
  DefaultDraw := False;
  with Sender.Canvas do
  begin
    Element := TXStringElement(Item.Data);
    Font := lvElements.Font;
    //Font.Name := 'Tahoma';

    r := Item.DisplayRect(drSelectBounds);

    p := Element.Name;
    if FKeymanDeveloperOptions.DebuggerShowStoreOffset and (Element.Tag > 0) then
      p := p + ' ('+IntToStr(Element.Tag)+')';

    GetTextExtentPoint32W(Handle, PWideChar(p), Length(p), sz1);
    if sz1.cx+2 < lvElements.Column[0].Width
      then r.Right := r.Left + sz1.cx+2
      else r.Right := r.Left + lvElements.Column[0].Width;

    SetBkMode(Handle, TRANSPARENT);
    if cdsSelected in State then
    begin
      SetTextColor(Handle, GetSysColor(COLOR_HIGHLIGHTTEXT));
      Windows.FillRect(Handle, r, HBRUSH(COLOR_HIGHLIGHT+1));
    end
    else
    begin
      SetTextColor(Handle, GetSysColor(COLOR_WINDOWTEXT));
      Windows.FillRect(Handle, r, HBRUSH(COLOR_WINDOW+1));
    end;
    if cdsFocused in State then
      DrawFocusRect(r);

    TextOutW(Handle, r.Left + 2, r.Top + 1, PWideChar(p), Length(p));
  end;

  with Sender.Canvas do
  begin
    Element := TXStringElement(Item.Data);
    Font := lvElements.Font;
    hOldFont := SelectObject(Handle, Font.Handle); // Selecting font not working?

    if Element.Tag = 0 then
      with TXString.Create(Element.Store, debugkeyboard) do
      try
        p1 := FormatString(FShowHexadecimal);
        p2 := '';
        p3 := '';
      finally
        Free;
      end
    else
    begin
      p1 := FormatStoreDisplay(Element.Store, 1, Element.Tag-1);
      p2 := ' '+FormatStoreDisplay(Element.Store, Element.Tag, 1)+' ';
      p3 := FormatStoreDisplay(Element.Store, Element.Tag+1, $FFFF);
      if p1 = '' then Delete(p2,1,1);
      if p3 = '' then Delete(p2,Length(p2),1);
    end;

    r.Top := 1;
    r.Left := LVIR_BOUNDS;
    SendMessage(Sender.Handle, LVM_GETSUBITEMRECT, Item.Index, DWord(@r));

    if xfoHexadecimal in FShowHexadecimal then
    begin
      SelectObject(Handle, hOldFont); // Selecting font not working?
      Font.Name := 'Courier New';
      hOldFont := SelectObject(Handle, Font.Handle); // Selecting font not working?
    end;

    Windows.FillRect(Handle, r, HBRUSH(COLOR_WINDOW+1));

    GetTextExtentPoint32W(Handle, PWideChar(p1), Length(p1), sz1);
    GetTextExtentPoint32W(Handle, PWideChar(p2), Length(p2), sz2);
    SetTextColor(Handle, GetSysColor(COLOR_WINDOWTEXT));
    TextOutW(Handle, r.Left + 2, r.Top + 1, {Rect.Left + 2, Rect.Top + 1,} PWideChar(p1), Length(p1));
    SetTextColor(Handle, RGB(255,0,0));
    TextOutW(Handle, r.Left + sz1.cx+2, r.Top + 1, {Rect.Left + sz1.cx + 2, Rect.Top + 1,} PWideChar(p2), Length(p2));
    SetTextColor(Handle, GetSysColor(COLOR_WINDOWTEXT));
    TextOutW(Handle, r.Left + sz1.cx+sz2.cx+2, r.Top + 1, {Rect.Left + sz1.cx + sz2.cx + 2, Rect.Top + 1,} PWideChar(p3), Length(p3));
    SelectObject(Handle, hOldFont); // Selecting font not working?
  end;
end;

procedure TfrmDebugStatus_Elements.lvElementsCustomDrawSubItem(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  DefaultDraw := False;
end;

procedure TfrmDebugStatus_Elements.lvElementsEnter(Sender: TObject);
begin
  lvElements.Repaint;
end;

procedure TfrmDebugStatus_Elements.lvElementsExit(Sender: TObject);
begin
  lvElements.Repaint;
end;

procedure TfrmDebugStatus_Elements.mnuPopupStoresHexadecimalViewClick(Sender: TObject);
begin
  if xfoHexadecimal in FShowHexadecimal
    then FShowHexadecimal := []
    else FShowHexadecimal := [xfoHexadecimal];
  UpdateStores(CurrentEvent);
  lvElements.Repaint;
end;

procedure TfrmDebugStatus_Elements.DisplayFontChanged;
begin
  inherited;
  lvElements.Font := DisplayFont;
  lvElements.Invalidate;
  ResizeStoreGrid;
end;

procedure TfrmDebugStatus_Elements.UpdateStores(Event: Keyman.System.Debug.DebugEvent.TDebugEvent);
var
  ncontext, i: Integer;
  e: TXStringElement;
begin
  SendMessage(lvElements.Handle, WM_SETREDRAW, 0, 0);
  try
    lvElements.DoubleBuffered := True;
    lvElements.Items.Clear;
    elements.Clear;             

    if not Assigned(Event) then Exit;

    with TXString.Create(Event.Rule.Rule.dpContext, debugkeyboard) do
    try
      FormatElements(elements, FShowHexadecimal);
    finally
      Free;
    end;

    ncontext := elements.Count;

    with TXString.Create(Event.Rule.Rule.dpOutput, debugkeyboard) do
    try
      FormatElements(elements, FShowHexadecimal);
    finally
      Free;
    end;

    { Set store offsets for all stores }
    for i := 0 to Event.Rule.nStores - 1 do
      elements[i].Tag := Event.Rule.StoreOffsets[i]+1;

    { Add + > }

    e := TXStringElement.Create;
    e.StoreIndex := -1;
    e.Pos := 0;
    elements.Insert(ncontext, e);

    if Event.Rule.Group.fUsingKeys then
      with TXString.Create(Event.Rule.Rule.Key, debugkeyboard) do
      try
        e.Name := '+ '+FormatString(FShowHexadecimal)+' >';
     finally
        Free;
      end
    else
      e.Name := '>';

    for i := 0 to elements.Count - 1 do
      with lvElements.Items.Add do
      begin
        Data := elements[i];
        Caption := '';
        SubItems.Add('');
      end;

    ResizeStoreGrid;
  finally
    SendMessage(lvElements.Handle, WM_SETREDRAW, 1, 0);
    lvElements.Repaint;
  end;
end;

function TfrmDebugStatus_Elements.FormatStoreDisplay(p: WideString; start, len: Integer): WideString;
var
  s: WideString;
begin
  with TXString.Create(p, debugkeyboard) do
  try
    s := SubString(start, len);
  finally
    Free;
  end;

  with TXString.Create(s, debugkeyboard) do
  try
    Result := FormatString(FShowHexadecimal);
  finally
    Free;
  end;
end;

procedure TfrmDebugStatus_Elements.FormCreate(Sender: TObject);
begin
  inherited;
  elements := TXStringElementList.Create;
end;

procedure TfrmDebugStatus_Elements.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(elements);
end;

procedure TfrmDebugStatus_Elements.FormResize(Sender: TObject);
begin
  ResizeStoreGrid;
  lvElements.Repaint;
end;

function TfrmDebugStatus_Elements.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_DebugStatus_Elements;
end;

function TfrmDebugStatus_Elements.GetStoreDisplayWidth(Element: TXStringElement): Integer;
var
  p1, p2, p3: WideString;
  sz1, sz2, sz3: TSize;
  hOldFont: HFONT;
begin
  if Element.Tag = 0 then
    with TXString.Create(Element.Store, debugkeyboard) do
    try
      p1 := FormatString(FShowHexadecimal);
      p2 := '';
      p3 := '';
    finally
      Free;
    end
  else
  begin
    p1 := FormatStoreDisplay(Element.Store, 1, Element.Tag-1);
    p2 := ' '+FormatStoreDisplay(Element.Store, Element.Tag, 1)+' ';
    p3 := FormatStoreDisplay(Element.Store, Element.Tag+1, $FFFF);
    if p1 = '' then Delete(p2,1,1);
    if p3 = '' then Delete(p2,Length(p2),1);
  end;

  with lvElements.Canvas do
  begin
    Font := lvElements.Font;
    if xfoHexadecimal in FShowHexadecimal then Font.Name := 'Courier New';
    hOldFont := SelectObject(Handle, Font.Handle);
    GetTextExtentPoint32W(Handle, PWideChar(p1), Length(p1), sz1);
    GetTextExtentPoint32W(Handle, PWideChar(p2), Length(p2), sz2);
    GetTextExtentPoint32W(Handle, PWideChar(p3), Length(p3), sz3);
    Result := sz1.cx + sz2.cx + sz3.cx;
    SelectObject(Handle, hOldFont);
  end;
end;

procedure TfrmDebugStatus_Elements.ResizeStoreGrid;
var
  w, maxw, i: Integer;
  p: WideString;
  sz: TSize;
  hOldFont: HFONT;
begin
  lvElements.Columns[1].Width := 1; // Don't interfere with scroll bars

  maxw := 100;
  with lvElements.Canvas do
  begin
    Font := lvElements.Font;
    //Font.Name := 'Tahoma';
    hOldFont := SelectObject(Handle, Font.Handle);

    for i := 0 to lvElements.Items.Count-1 do
    begin
      with TXStringElement(lvElements.Items[i].Data) do
      begin
        p := Name;
        if FKeymanDeveloperOptions.DebuggerShowStoreOffset and (Tag > 0) then
          p := p + ' ('+IntToStr(Tag)+')';
      end;
      GetTextExtentPoint32W(Handle, PWideChar(p), Length(p), sz);
      if sz.cx + 4 > maxw then maxw := sz.cx + 4;
    end;

    SelectObject(Handle, hOldFont);

    lvElements.Columns[0].Width := maxw;
  end;

  maxw := lvElements.ClientWidth - lvElements.Columns[0].Width - 4;
  for i := 0 to lvElements.Items.Count-1 do
  begin
    w := GetStoreDisplayWidth(TXStringElement(lvElements.Items[i].Data));
    if w > maxw then maxw := w;
  end;

  if maxw <= 0 then maxw := 1;
  lvElements.Columns[1].Width := maxw;
end;

{procedure TfrmDebugStatus_Elements.RefreshOptions;
begin
  lvElements.Repaint;
end;}

end.


