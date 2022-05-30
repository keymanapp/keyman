{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSbEdit.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}


unit JvSbEdit;

interface

uses
  Windows,

{$IFDEF Delphi6_Up}
  DesignIntf, DesignEditors, DesignWindows,
{$ELSE}
  LibIntf, DsgnIntf, DsgnWnds,
{$ENDIF}
  SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Grids, JvSpeedbar, Menus, JvPlacemnt, JvConst, JvxCtrls, JvVCLUtils;

type

{ TJvSpeedbarEditor }

  TSelectData = record
    bRowCount: Integer;
    bRow: Integer;
    sRowCount: Integer;
    sRow: Integer;
  end;

  TJvSpeedbarEditor = class(TDesignWindow)
    SectionsBox: TGroupBox;
    NewSection: TButton;
    DelSection: TButton;
    ButtonsBox: TGroupBox;
    UpBtn: TSpeedButton;
    DownBtn: TSpeedButton;
    AddButton: TButton;
    RemoveButton: TButton;
    CloseBtn: TButton;
    SectionName: TEdit;
    SectionNameLabel: TLabel;
    SectionList: TDrawGrid;
    ButtonsList: TDrawGrid;
    LabelHint: TLabel;
    PopupMenu: TPopupMenu;
    CopyMenu: TMenuItem;
    PasteMenu: TMenuItem;
    CutMenu: TMenuItem;
    FormPlacement1: TJvFormPlacement;
    procedure DelSectionClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SectionNameExit(Sender: TObject);
    procedure SectionListSelectCell(Sender: TObject; Col, Row: Longint;
      var CanSelect: Boolean);
    procedure SectionListDrawCell(Sender: TObject; Col, Row: Longint;
      Rect: TRect; State: TGridDrawState);
    procedure ButtonsListDblClick(Sender: TObject);
    procedure ButtonsListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ButtonsListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonsListMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ButtonsListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonsListSelectCell(Sender: TObject; Col, Row: Longint;
      var CanSelect: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NewSectionClick(Sender: TObject);
    procedure SectionNameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ButtonsListDrawCell(Sender: TObject; Col, Row: Longint;
      Rect: TRect; State: TGridDrawState);
    procedure SectionListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SectionListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SectionListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SectionListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CopyMenuClick(Sender: TObject);
    procedure PasteMenuClick(Sender: TObject);
    procedure CutMenuClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FButton: TJvBtnControl;
    FImage: TJvButtonImage;
    FBar: TJvSpeedBar;
    FDrag: Boolean;
    FDragItem: TJvSpeedItem;
    FLocked: Integer;
    FSelectData: TSelectData;
    procedure Copy;
    procedure Cut;
    procedure Paste;
    procedure OnPasteItem(Item: TObject);
    procedure SaveSelection;
    procedure RestoreSelection;
    procedure SelectButton(Section: Integer; Item: TJvSpeedItem; SelectBar: Boolean);
    procedure UpdateEnabled(BtnRow, Section: Integer);
    function CheckSpeedBar: Boolean;
    function ConfirmDelete: Boolean;
    function CurrentSection: Integer;
    function GetForm: TCustomForm;
    procedure SetSection(Section: Integer);
    procedure UpdateData;
    procedure UpdateListHeight;
    procedure SeTJvSpeedBar(Value: TJvSpeedBar);
    function ItemByRow(Row: Integer): TJvSpeedItem;
    function SectionByRow(Row: Integer): TJvSpeedbarSection;
    function ItemBySectionRow(Section, Row: Integer): TJvSpeedItem;
    procedure CMSpeedBarChanged(var Message: TMessage); message CM_SPEEDBARCHANGED;
  protected
    procedure Activated; override;
    function UniqueName(Component: TComponent): string; override;
  public
    { Public declarations }

{$IFDEF Delphi6_Up}
    procedure ItemsModified(const Designer: IDesigner); override;
    procedure DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean); override;
{$ELSE}
    procedure FormModified; override;
{$IFDEF Delphi3_Up}
    procedure FormClosed(Form: TCustomForm); override;
{$ELSE}
    procedure FormClosed(Form: TForm); override;
{$ENDIF}
{$ENDIF}
    function GetEditState: TEditState; override;
{$IFDEF Delphi6_Up}
    function EditAction(Action: TEditAction): Boolean; override;
{$ELSE}
    procedure EditAction(Action: TEditAction); override;
{$ENDIF}

    property JvSpeedbar: TJvSpeedBar read FBar write SeTJvSpeedBar;
    property OwnerForm: TCustomForm read GetForm;
  end;

{ TJvSpeedbarCompEditor }

  TJvSpeedbarCompEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses TypInfo, JvMaxMin, JvLConst, JvProps, JvDsgn;

{$R *.DFM}

{$IFDEF WIN32}
{$D-}
{$ENDIF}

{$IFDEF Delphi6_Up}
type
  TDesigner = DesignIntf.IDesigner;
  TFormDesigner = DesignIntf.IDesigner;
{$ELSE}
{$IFDEF Delphi4_Up}
type
  TDesigner = IDesigner;
  TFormDesigner = IFormDesigner;
{$ENDIF}
{$ENDIF}

{ Utility routines }

function FindEditor(JvSpeedbar: TJvSpeedBar): TJvSpeedbarEditor;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do begin
    if Screen.Forms[I] is TJvSpeedbarEditor then begin
      if TJvSpeedbarEditor(Screen.Forms[I]).JvSpeedbar = JvSpeedbar then
      begin
        Result := TJvSpeedbarEditor(Screen.Forms[I]);
        Break;
      end;
    end;
  end;
end;

procedure ShowSpeedbarDesigner(Designer: TDesigner; JvSpeedbar: TJvSpeedBar);
var
  Editor: TJvSpeedbarEditor;
begin
  if JvSpeedbar = nil then Exit;
  Editor := FindEditor(JvSpeedbar);
  if Editor <> nil then begin
    Editor.Show;
    if Editor.WindowState = wsMinimized then Editor.WindowState := wsNormal;
  end
  else begin
    Editor := TJvSpeedbarEditor.Create(Application);
    try
      Editor.Designer := TFormDesigner(Designer);
      Editor.JvSpeedbar := JvSpeedbar;
      Editor.Show;
    except
      Editor.Free;
      raise;
    end;
  end;
end;

{ TJvSpeedbarCompEditor }

procedure TJvSpeedbarCompEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ShowSpeedbarDesigner(Designer, TJvSpeedBar(Component));
  end;
end;

function TJvSpeedbarCompEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := LoadStr(srSpeedbarDesigner);
  end;
end;

function TJvSpeedbarCompEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TJvSpeedbarEditor }

const
  MaxBtnListHeight = 158;

function TJvSpeedbarEditor.UniqueName(Component: TComponent): string;
var
  Temp: string;
begin
  Result := '';
  if (Component <> nil) then Temp := Component.ClassName
  else Temp := TJvSpeedItem.ClassName;
  if (UpCase(Temp[1]) = 'T') and (Length(Temp) > 1) then
    System.Delete(Temp, 1, 1);
  Result := Designer.UniqueName(Temp);
end;

function TJvSpeedbarEditor.GetEditState: TEditState;
begin
  Result := [];
  if RemoveButton.Enabled then begin
    Result := [esCanDelete, esCanCut, esCanCopy];
  end;
  if AddButton.Enabled and ClipboardComponents then
    Include(Result, esCanPaste);
end;

{$IFDEF Delphi6_Up}

function TJvSpeedbarEditor.EditAction(Action: TEditAction): Boolean;
begin
  Result := True;
{$ELSE}

procedure TJvSpeedbarEditor.EditAction(Action: TEditAction);
begin
{$ENDIF}

  case Action of
    eaCut: Cut;
    eaCopy: Copy;
    eaPaste: Paste;
    eaDelete: RemoveButtonClick(Self);
  end;
end;

{$IFDEF Delphi6_Up}
type
  TDesignerSelectionList = IDesignerSelections;
{$ENDIF}

procedure TJvSpeedbarEditor.SelectButton(Section: Integer; Item: TJvSpeedItem;
  SelectBar: Boolean);
var
  FCompList: TDesignerSelectionList;
  Sect: TJvSpeedbarSection;
begin
  if CheckSpeedBar and Active then begin
    //Designer.GetSelections(FCompList);
    FCompList := {$IFDEF Delphi6_Up}TDesignerSelections{$ELSE}TDesignerSelectionList{$ENDIF}.Create;
    if not SelectBar then begin
      if (ActiveControl = SectionList) or (ActiveControl = SectionName) then
      begin
        Sect := SectionByRow(Section);
        if Sect <> nil then FCompList.Add(Sect);
      end;
      if (FCompList.Count = 0) and (Item <> nil) then FCompList.Add(Item);
    end;
    if (FBar <> nil) and (FCompList.Count = 0) then FCompList.Add(FBar);
    SetSelection(FCompList);
  end;
end;


{$IFDEF Delphi6_Up}
procedure TJvSpeedbarEditor.DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
{$ELSE}
{$IFDEF Delphi3_Up}
procedure TJvSpeedbarEditor.FormClosed(Form: TCustomForm);
{$ELSE}
procedure TJvSpeedbarEditor.FormClosed(Form: TForm);
{$ENDIF}
{$ENDIF}
begin
  if {$IFDEF Delphi6_Up}ADesigner.Root{$ELSE}Form{$ENDIF} = OwnerForm then Free;
end;

{$IFDEF Delphi6_Up}
procedure TJvSpeedbarEditor.ItemsModified(const Designer: IDesigner);
{$ELSE}
procedure TJvSpeedbarEditor.FormModified;
{$ENDIF}
begin
  if not (csDestroying in ComponentState) then UpdateData;
end;

procedure TJvSpeedbarEditor.Activated;
begin
  SelectButton(CurrentSection, ItemByRow(ButtonsList.Row), False);
  PasteMenu.Enabled := CheckSpeedBar and (FBar.SectionCount > 0) and
    ClipboardComponents;
end;

function TJvSpeedbarEditor.ConfirmDelete: Boolean;
begin
  Result := MessageDlg(LoadStr(srConfirmSBDelete), mtWarning, mbYesNoCancel, 0) = mrYes;
end;

procedure TJvSpeedbarEditor.SaveSelection;
begin
  with FSelectData do begin
    bRowCount := ButtonsList.RowCount;
    bRow := ButtonsList.Row;
    sRowCount := SectionList.RowCount;
    sRow := SectionList.Row;
  end;
end;

procedure TJvSpeedbarEditor.RestoreSelection;
var
  NewSRow, NewBRow: Integer;
begin
  NewSRow := FSelectData.sRow;
  if (SectionList.RowCount > FSelectData.sRowCount) or
    (NewSRow > SectionList.RowCount - 1) then
    NewSRow := SectionList.RowCount - 1;
  if NewSRow < 0 then NewSRow := 0;
  SectionList.Row := NewSRow;
  SetSection(SectionList.Row); { set ButtonsList to current section }
  NewBRow := FSelectData.bRow;
  if (ButtonsList.RowCount > FSelectData.bRowCount) or
    (NewBRow > ButtonsList.RowCount - 1) then
    NewBRow := ButtonsList.RowCount - 1;
  if NewBRow < 0 then NewBRow := 0;
  ButtonsList.Row := NewBRow;
end;

procedure TJvSpeedbarEditor.UpdateEnabled(BtnRow, Section: Integer);
var
  EnableSect, EnableBtn: Boolean;
begin
  EnableSect := CheckSpeedBar and (FBar.SectionCount > 0);
  EnableBtn := EnableSect and (BtnRow >= 0) and (ItemBySectionRow(Section,
    BtnRow) <> nil);
  DelSection.Enabled := EnableSect;
  SectionName.Enabled := EnableSect;
  AddButton.Enabled := EnableSect;
  RemoveButton.Enabled := EnableBtn;
  CopyMenu.Enabled := EnableBtn;
  CutMenu.Enabled := EnableBtn;
  PasteMenu.Enabled := EnableSect and ClipboardComponents;
  UpBtn.Enabled := EnableBtn and (BtnRow > 0);
  DownBtn.Enabled := EnableBtn and (BtnRow < ButtonsList.RowCount - 1);
end;

function TJvSpeedbarEditor.CheckSpeedBar: Boolean;
begin
  Result := (FBar <> nil) and (FBar.Owner <> nil) and (FBar.Parent <> nil)
    and ({$IFDEF Delphi6_Up}Designer.Root{$ELSE}Designer.Form{$ENDIF} <> nil);
end;

function TJvSpeedbarEditor.CurrentSection: Integer;
begin
  if CheckSpeedBar and (FBar.SectionCount > 0) then
    Result := SectionList.Row
  else Result := -1;
end;

procedure TJvSpeedbarEditor.SetSection(Section: Integer);
var
  I: Integer;
begin
  if CheckSpeedBar then begin
    I := Section;
    if (I >= 0) and (I < FBar.SectionCount) then begin
      SectionName.Text := TJvSpeedbarSection(FBar.Sections[I]).Caption;
      ButtonsList.RowCount := FBar.ItemsCount(I);
    end
    else begin
      SectionName.Text := '';
      ButtonsList.RowCount := 0;
    end;
    SectionList.DefaultColWidth := SectionList.ClientWidth;
    ButtonsList.DefaultColWidth := ButtonsList.ClientWidth;
  end;
end;

procedure TJvSpeedbarEditor.UpdateData;
begin
  Inc(FLocked);
  try
    SaveSelection;
    if CheckSpeedBar then SectionList.RowCount := FBar.SectionCount
    else SectionList.RowCount := 0;
    RestoreSelection; { set section }
  finally
    Dec(FLocked);
  end;
  UpdateEnabled(ButtonsList.Row, SectionList.Row);
  SelectButton(CurrentSection, ItemByRow(ButtonsList.Row), False);
end;

function TJvSpeedbarEditor.GetForm: TCustomForm;
begin
  Result := {$IFDEF Delphi6_Up}TCustomForm(Designer.Root){$ELSE}Designer.Form{$ENDIF}; { GetParentForm(FBar) }
end;

procedure TJvSpeedbarEditor.UpdateListHeight;
var
  Cnt: Integer;
  MaxHeight: Integer;
begin
  Canvas.Font := Font;
  MaxHeight := MulDiv(MaxBtnListHeight, Screen.PixelsPerInch, 96);
  ButtonsList.DefaultRowHeight := FBar.BtnHeight + 2;
  Cnt := Max(1, Max(ButtonsList.ClientHeight, MaxHeight) div
    (FBar.BtnHeight + 2));
  ButtonsList.ClientHeight := Min(ButtonsList.DefaultRowHeight * Cnt,
    MaxHeight);
  SectionList.DefaultRowHeight := Canvas.TextHeight('Wg') + 2;
end;

procedure TJvSpeedbarEditor.SeTJvSpeedBar(Value: TJvSpeedBar);
var
  I: Integer;
begin
  if FBar <> Value then begin
    if FBar <> nil then FBar.SetEditing(0);
    FBar := Value;
    if FBar <> nil then FBar.SetEditing(Handle);
    Inc(FLocked);
    try
      if FBar <> nil then UpdateListHeight;
      if FBar.SectionCount = 0 then NewSectionClick(Self)
      else
        for I := 0 to FBar.SectionCount - 1 do begin
          if FBar.Sections[I].Name = '' then begin
            FBar.Sections[I].Name := UniqueName(FBar.Sections[I]);
            Designer.Modified;
          end;
        end;
      if ButtonsList.RowCount > 0 then ActiveControl := ButtonsList
      else ActiveControl := SectionList;
      UpdateData;
      ButtonsList.Row := 0;
    finally
      Dec(FLocked);
    end;
    SectionList.Row := 0;
  end;
end;

procedure TJvSpeedbarEditor.CMSpeedBarChanged(var Message: TMessage);
begin
  if Pointer(Message.LParam) = FBar then begin
    case Message.WParam of
      SBR_CHANGED: Designer.Modified;
      SBR_DESTROYED: Close;
      SBR_BTNSIZECHANGED: if FBar <> nil then UpdateListHeight;
    end;
  end
  else if (Message.WParam = SBR_BTNSELECT) and CheckSpeedBar then begin
    SelectButton(-1, nil, True);
    Designer.Modified;
  end;
end;

function TJvSpeedbarEditor.ItemBySectionRow(Section, Row: Integer): TJvSpeedItem;
begin
  if CheckSpeedBar then Result := FBar.Items(Section, Row)
  else Result := nil;
end;

function TJvSpeedbarEditor.SectionByRow(Row: Integer): TJvSpeedbarSection;
begin
  if CheckSpeedBar and (Row >= 0) and (Row < FBar.SectionCount) then
    Result := FBar.Sections[Row]
  else Result := nil;
end;

function TJvSpeedbarEditor.ItemByRow(Row: Integer): TJvSpeedItem;
begin
  Result := ItemBySectionRow(CurrentSection, Row);
end;

procedure TJvSpeedbarEditor.NewSectionClick(Sender: TObject);
var
  S: string;
  I: Integer;
begin
  if CheckSpeedBar then begin
    I := 0;
    repeat
      S := Format(LoadStr(srNewSectionName), [I]);
      Inc(I);
    until FBar.SearchSection(S) < 0;
    I := NewSpeedSection(FBar, S);
    if I >= 0 then FBar.Sections[I].Name := UniqueName(FBar.Sections[I]);
    ActiveControl := SectionName;
    Designer.Modified;
  end;
end;

procedure TJvSpeedbarEditor.DelSectionClick(Sender: TObject);
var
  Sect: Integer;
  Item: TJvSpeedItem;
begin
  if CheckSpeedBar and ConfirmDelete then begin
    Sect := SectionList.Row;
    if (Sect >= 0) and (Sect < FBar.SectionCount) then begin
      {$IFDEF Delphi6_Up}TCustomForm(Designer.Root).Designer{$ELSE}Designer{$ENDIF}.ValidateRename(FBar.Sections[Sect],
//      Self.ValidateRename(FBar.Sections[Sect],
        FBar.Sections[Sect].Name, '');
      try
        while FBar.ItemsCount(Sect) > 0 do begin
          Item := FBar.Items(Sect, 0);
          if Item <> nil then begin
            OwnerForm.RemoveComponent(Item);
            Item.Free;
          end;
        end;
        FBar.RemoveSection(Sect);
      finally
        Designer.Modified;
      end;
    end;
  end;
end;

procedure TJvSpeedbarEditor.Copy;
var
  CompList: TDesignerSelectionList;
  Item: TJvSpeedItem;
begin
//----------------
  CompList := {$IFDEF Delphi6_Up}TDesignerSelections{$ELSE}TDesignerSelectionList{$ENDIF}.Create;
{$IFNDEF Delphi6_Up}
  try
{$ENDIF}
    Item := ItemByRow(ButtonsList.Row);
    if Item <> nil then begin
      Item.InvalidateItem;
      CompList.Add(Item);
      CopyComponents(OwnerForm, CompList);
      Item.UpdateSection;
    end;
{$IFNDEF Delphi6_Up}
  finally
    CompList.Free;
  end;
{$ENDIF}



end;

procedure TJvSpeedbarEditor.Paste;
var
  CompList: TDesignerSelectionList;
begin
  if CheckSpeedBar then begin
    CompList := {$IFDEF Delphi6_Up}TDesignerSelections{$ELSE}TDesignerSelectionList{$ENDIF}.Create;
{$IFNDEF Delphi6_Up}
    try
{$ENDIF}
      FBar.OnAddItem := OnPasteItem;
      try
        PasteComponents(OwnerForm, FBar, CompList);
      finally
        FBar.OnAddItem := nil;
      end;
      UpdateData;
{$IFNDEF Delphi6_Up}
    finally
      CompList.Free;
    end;
{$ENDIF}
  end;
end;

procedure TJvSpeedbarEditor.Cut;
begin
  Copy;
  RemoveButtonClick(Self);
end;

procedure TJvSpeedbarEditor.OnPasteItem(Item: TObject);
begin
  if (Item <> nil) then begin
    if CheckSpeedBar and (Item is TJvSpeedItem) then begin
      TJvSpeedItem(Item).ASection := CurrentSection;
      TJvSpeedItem(Item).Visible := False;
    end
  end;
end;

procedure TJvSpeedbarEditor.AddButtonClick(Sender: TObject);
var
  I: Integer;
  Item: TJvSpeedItem;
begin
  I := CurrentSection;
  if I < 0 then Exit;
  Item := TJvSpeedItem.Create(OwnerForm);
  if Item <> nil then
  try
    FBar.AddItem(I, Item);
    Item.Name := UniqueName(Item);
    Designer.Modified;
    if (Sender <> nil) then ActivateInspector(#0);
  except
    Item.Free;
    raise;
  end
  else raise EJvSpeedbarError.CreateRes(srSBItemNotCreate);
end;

procedure TJvSpeedbarEditor.RemoveButtonClick(Sender: TObject);
var
  Item: TJvSpeedItem;
begin
  Item := ItemByRow(ButtonsList.Row);
  if Item <> nil then begin
    Self.ValidateRename(Item, Item.Name, '');
    OwnerForm.RemoveComponent(Item);
    Item.Free;
    Designer.Modified;
  end;
end;

procedure TJvSpeedbarEditor.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TJvSpeedbarEditor.UpBtnClick(Sender: TObject);
var
  I, Sect: Integer;
begin
  if CheckSpeedBar and FBar.FindItem(ItemByRow(ButtonsList.Row), Sect, I) then
  begin
    if I > 0 then begin
      FBar.Sections[Sect].List.Move(I, I - 1);
      Designer.Modified;
      ButtonsList.Invalidate;
      ButtonsList.Row := ButtonsList.Row - 1;
    end;
  end;
end;

procedure TJvSpeedbarEditor.DownBtnClick(Sender: TObject);
var
  I, Sect: Integer;
begin
  if CheckSpeedBar and FBar.FindItem(ItemByRow(ButtonsList.Row), Sect, I) then
  begin
    if I < FBar.ItemsCount(Sect) - 1 then begin
      FBar.Sections[Sect].List.Move(I, I + 1);
      Designer.Modified;
      ButtonsList.Invalidate;
      ButtonsList.Row := ButtonsList.Row + 1;
    end;
  end;
end;

procedure TJvSpeedbarEditor.CopyMenuClick(Sender: TObject);
begin
  Copy;
end;

procedure TJvSpeedbarEditor.PasteMenuClick(Sender: TObject);
begin
  Paste;
end;

procedure TJvSpeedbarEditor.CutMenuClick(Sender: TObject);
begin
  Cut;
end;

procedure TJvSpeedbarEditor.SectionNameExit(Sender: TObject);
var
  I: Integer;
begin
  if CheckSpeedBar and (FBar.SectionCount > 0) then begin
    I := CurrentSection;
    if I >= 0 then begin
      FBar.Sections[I].Caption := SectionName.Text;
      Designer.Modified;
    end;
  end;
end;

procedure TJvSpeedbarEditor.SectionListSelectCell(Sender: TObject; Col,
  Row: Longint; var CanSelect: Boolean);
begin
  CanSelect := False;
  if CheckSpeedBar and (Row < FBar.SectionCount) and (Row >= 0) then begin
    if FLocked = 0 then begin
      SetSection(Row);
      UpdateEnabled(ButtonsList.Row, Row);
      ButtonsList.Invalidate;
      SelectButton(Row, ItemBySectionRow(Row, ButtonsList.Row), False);
    end;
    CanSelect := True;
  end;
end;

procedure TJvSpeedbarEditor.SectionListDrawCell(Sender: TObject; Col,
  Row: Longint; Rect: TRect; State: TGridDrawState);
begin
  if CheckSpeedBar then begin
    if (Row < FBar.SectionCount) and (Row >= 0) then begin
      DrawCellText(Sender as TDrawGrid, Col, Row,
        FBar.Sections[Row].Caption, Rect, taLeftJustify, vaCenter
{$IFDEF Delphi4_Up}, TDrawGrid(Sender).IsRightToLeft{$ENDIF});
    end;
  end;
end;

procedure TJvSpeedbarEditor.SectionListKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN: if SectionByRow(SectionList.Row) <> nil then ActivateInspector(#0);
    VK_DELETE: DelSectionClick(Self);
    VK_INSERT, VK_ADD: NewSectionClick(Self);
  else Exit;
  end;
  Key := 0;
end;

procedure TJvSpeedbarEditor.ButtonsListKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN: if ItemByRow(ButtonsList.Row) <> nil then ActivateInspector(#0);
    VK_DELETE: RemoveButtonClick(Self);
    VK_INSERT, VK_ADD: AddButtonClick(Self);
  else Exit;
  end;
  Key := 0;
end;

procedure TJvSpeedbarEditor.ButtonsListDblClick(Sender: TObject);
type
  PParamData = ^TParamData;
  TParamData = record
    Flags: TParamFlags;
    ParamNameAndType: array[0..100] of Char;
  end;
const
{$IFDEF CBUILDER}
  sSender: string[7] = '*Sender';
{$ELSE}
  sSender: string[6] = 'Sender';
{$ENDIF}
  sObject: string[7] = 'TObject';
var
  Btn: TJvSpeedItem;
  I, Num: Integer;
  MethodName: string;
  Method: TMethod;
  TypeData: PTypeData;
  ParamData: PParamData;
  PropInfo: PPropInfo;
  Candidates: TJvPropInfoList;
begin
  Btn := ItemByRow(ButtonsList.Row);
  if Btn = nil then Exit;
  Candidates := TJvPropInfoList.Create(Btn, [tkMethod]);
  try
    for I := Candidates.Count - 1 downto 0 do begin
      PropInfo := Candidates[I];
      if CompareText(PropInfo^.Name, 'OnClick') = 0 then begin
        Method := GetMethodProp(Btn, PropInfo);
        MethodName := TFormDesigner(Designer).GetMethodName(Method);
        if MethodName = '' then begin
          MethodName := Btn.Name + 'Click';
          Num := 0;
          while TFormDesigner(Designer).MethodExists(MethodName) do begin
            MethodName := Btn.Name + 'Click' + IntToStr(Num);
            Inc(Num);
          end;
          TypeData := AllocMem(SizeOf(TTypeData));
          try
            TypeData^.MethodKind := mkProcedure;
            TypeData^.ParamCount := 1;
            ParamData := PParamData(@TypeData^.ParamList);
            with ParamData^ do begin
              Flags := [];
              ParamNameAndType[0] := Char(Length(sSender));
              Move(sSender[1], ParamNameAndType[1], Length(sSender));
              ParamNameAndType[Length(sSender) + 1] := char(Length(sObject));
              Move(sObject[1], ParamNameAndType[Length(sSender) + 2],
                Length(sObject));
            end;
            Method := TFormDesigner(Designer).CreateMethod(MethodName, TypeData);
            Method.Data := OwnerForm;
          finally
            FreeMem(TypeData, SizeOf(TTypeData));
          end;
          Btn.OnClick := TNotifyEvent(Method);
          Designer.Modified;
        end;
        if (MethodName <> '') and TFormDesigner(Designer).MethodExists(MethodName) then
          TFormDesigner(Designer).ShowMethod(MethodName);
        Break;
      end;
    end;
  finally
    Candidates.Free;
  end;
end;

procedure TJvSpeedbarEditor.ButtonsListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Item: TJvSpeedItem;
begin
  if (X < FBar.BtnWidth + 2) and (Button = mbLeft) then
  begin
    Item := ItemByRow(ButtonsList.Row);
    if Item <> nil then begin
      FDrag := True;
      if Item.Visible then FDragItem := nil
      else begin
        FDragItem := Item;
        if FButton = nil then begin
          FButton := TJvBtnControl.Create(Self);
          TJvBtnControl(FButton).AssignSpeedItem(Item);
        end;
      end;
    end;
  end;
end;

procedure TJvSpeedbarEditor.ButtonsListMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if FDrag and (FButton <> nil) and (FDragItem <> nil) then begin
    P := (Sender as TControl).ClientToScreen(Point(X, Y));
    X := P.X - (FButton.Width {div 2});
    Y := P.Y - (FButton.Height {div 2});
    FButton.Activate(Bounds(X, Y, FBar.BtnWidth, FBar.BtnHeight));
  end
  else if FDrag then SetCursor(Screen.Cursors[crNoDrop]);
end;

procedure TJvSpeedbarEditor.ButtonsListMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if FDrag and (Button = mbLeft) then
  try
    if (FDragItem <> nil) and (FButton <> nil) then begin
      Dec(X, FButton.Width {div 2});
      Dec(Y, FButton.Height {div 2});
      P := (Sender as TControl).ClientToScreen(Point(X, Y));
      FButton.Free;
      FButton := nil;
      if CheckSpeedBar and (FBar = FindSpeedBar(P)) then begin
        P := FBar.ScreenToClient(P);
        if FBar.AcceptDropItem(FDragItem, P.X, P.Y) then begin
          Designer.Modified;
        end;
      end;
    end
    else SetCursor(Screen.Cursors[ButtonsList.Cursor]);
  finally
    FDrag := False;
    FDragItem := nil;
  end;
end;

procedure TJvSpeedbarEditor.ButtonsListSelectCell(Sender: TObject; Col,
  Row: Longint; var CanSelect: Boolean);
var
  Item: TJvSpeedItem;
begin
  Item := ItemByRow(Row);
  CanSelect := not FDrag and (Item <> nil);
  if FLocked = 0 then begin
    if CanSelect then begin
      UpdateEnabled(Row, SectionList.Row);
      SelectButton(CurrentSection, Item, False);
    end
    else if not FDrag then begin
      UpdateEnabled(-1, SectionList.Row);
      SelectButton(-1, nil, True);
    end;
  end;
end;

procedure TJvSpeedbarEditor.FormCreate(Sender: TObject);
begin
  FImage := TJvButtonImage.Create;
  FButton := nil;
  FBar := nil;
  FDrag := False;
  if NewStyleControls then Font.Style := [];
  with FormPlacement1 do begin
    UseRegistry := True;
    IniFileName := SDelphiKey;
  end;
end;

procedure TJvSpeedbarEditor.FormDestroy(Sender: TObject);
begin
  FImage.Free;
end;

procedure TJvSpeedbarEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  FButton.Free;
  FButton := nil;
  if FBar <> nil then begin
    FBar.SetEditing(0);
    SelectButton(-1, nil, True);
    FBar.Invalidate;
  end;
  FBar := nil;
end;

procedure TJvSpeedbarEditor.SectionNameKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = (VK_RETURN) then begin
    SectionNameExit(SectionName);
    Key := 0;
    ActiveControl := SectionList;
  end;
end;

procedure TJvSpeedbarEditor.ButtonsListDrawCell(Sender: TObject; Col,
  Row: Longint; Rect: TRect; State: TGridDrawState);
var
  I: Integer;
begin
  I := CurrentSection;
  if (I >= 0) and (Row < FBar.ItemsCount(I)) then
    DrawCellButton(Sender as TDrawGrid, Rect, ItemByRow(Row), FImage
{$IFDEF Delphi4_Up}, TDrawGrid(Sender).IsRightToLeft{$ENDIF});
end;

procedure TJvSpeedbarEditor.SectionListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Longint;
begin
  if (Button = mbLeft) then
    with (Sender as TDrawGrid) do begin
      MouseToCell(X, Y, ACol, ARow);
      Tag := Row;
      BeginDrag(False);
    end;
end;

procedure TJvSpeedbarEditor.SectionListDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  Col, Row: Longint;
begin
  try
    (Sender as TDrawGrid).MouseToCell(X, Y, Col, Row);
    FBar.Sections[(Sender as TDrawGrid).Tag].Index := Row;
    Designer.Modified;
    UpdateData;
    SectionList.Row := Row;
  finally
    (Sender as TDrawGrid).Tag := 0;
  end;
end;

procedure TJvSpeedbarEditor.SectionListDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  Col, Row: Longint;
begin
  (Sender as TDrawGrid).MouseToCell(X, Y, Col, Row);
  Accept := (Row >= 0) and (Row <> (Sender as TDrawGrid).Tag);
end;

procedure TJvSpeedbarEditor.FormShow(Sender: TObject);
begin
  if FBar <> nil then UpdateListHeight;
  SectionList.DefaultColWidth := SectionList.ClientWidth;
  ButtonsList.DefaultColWidth := ButtonsList.ClientWidth;
end;

end.

