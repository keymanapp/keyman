{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBLookupTreeView.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

components  : TJvDBLookupTreeView,
              TJvDBLookupTreeViewCombo
description : db-aware lookup TreeView

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{ history
 (JVCL Library versions) :
  1.20:
    - first release;
  1.61:
    - support for non-bde components;
  2.01:
    - support for BiDi mode
     (thanks to Oussama Al-Rifai);
}

unit JvDBLookupTreeView;

interface

uses
  {$IFDEF COMPILER6_UP}
  Variants, VDBConsts,
  {$ENDIF}
  Windows, Messages, SysUtils, Classes, Controls, Forms, Graphics,
  CommCtrl, ComCtrls, Db,
  JvDBTreeView;

// (rom) obviously not used
//const
  //WM_INCLOSE = WM_USER + 1;

{********************** Borland **********************}

type
  TJvDBLookupControl = class;

  TJvDataSourceLink = class(TDataLink)
  private
    FDBLookupControl: TJvDBLookupControl;
  protected
    procedure FocusControl(Field: TFieldRef); override;
    procedure ActiveChanged; override;
    procedure RecordChanged(Field: TField); override;
  end;

  TJvListSourceLink = class(TDataLink)
  private
    FDBLookupControl: TJvDBLookupControl;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
  end;

  TJvDBLookupControl = class(TCustomControl)
  private
    FLookupSource: TDataSource;
    FDataLink: TJvDataSourceLink;
    FListLink: TJvListSourceLink;
    FDataFieldName: string;
    FKeyFieldName: string;
    FListFieldName: string;
    FListFieldIndex: Integer;
    FDataField: TField;
    FMasterField: TField;
    FKeyField: TField;
    FListField: TField;
    FListFields: TList;
    FKeyValue: Variant;
    FUseFilter: Boolean;
    FSearchText: string;
    FLookupMode: Boolean;
    FListActive: Boolean;
    FFocused: Boolean;
    function CanModify: Boolean;
    procedure CheckNotCircular;
    procedure CheckNotLookup;
    procedure DataLinkActiveChanged;
    procedure DataLinkRecordChanged(Field: TField);
    function GetBorderSize: Integer;
    function GetDataSource: TDataSource;
    function GetKeyFieldName: string;
    function GetListSource: TDataSource;
    function GetReadOnly: Boolean;
    function GetTextHeight: Integer;
    procedure KeyValueChanged; virtual;
    procedure ListLinkActiveChanged; virtual;
    procedure ListLinkDataChanged; virtual;
    function LocateKey: Boolean;
    procedure ProcessSearchKey(Key: Char);
    procedure SelectKeyValue(const Value: Variant);
    procedure SetDataFieldName(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetKeyFieldName(const Value: string);
    procedure SetKeyValue(const Value: Variant);
    procedure SetListFieldName(const Value: string);
    procedure SetListSource(Value: TDataSource);
    procedure SetLookupMode(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure WMGetDlgCode(var Msg: TMessage); message WM_GETDLGCODE;
    procedure WMKillFocus(var Msg: TMessage); message WM_KILLFOCUS;
    procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
    procedure CMGetDataLink(var Msg: TMessage); message CM_GETDATALINK;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    property DataField: string read FDataFieldName write SetDataFieldName;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property KeyField: string read GetKeyFieldName write SetKeyFieldName;
    property KeyValue: Variant read FKeyValue write SetKeyValue;
    property ListField: string read FListFieldName write SetListFieldName;
    property ListFieldIndex: Integer read FListFieldIndex write FListFieldIndex default 0;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property UseFilter: Boolean read FUseFilter write FUseFilter;
    property ParentColor default False;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property TabStop default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read FDataField;
  end;

  TJvPopupDataList = class;

  TDropDownAlign = (daLeft, daRight, daCenter);

  TJvDBLookupTreeViewCombo = class(TJvDBLookupControl)
  private
    FDataList: TJvPopupDataList;
    FButtonWidth: Integer;
    FText: string;
//    FDropDownRows: Integer;
    FTracking: Boolean;
    FDropDownWidth: Integer;
    FDropDownHeight: Integer;
    FDropDownAlign: TDropDownAlign;
    FListVisible: Boolean;
    FPressed: Boolean;
    FAlignment: TAlignment;
    FLookupMode: Boolean;
    FOnDropDown: TNotifyEvent;
    FOnCloseUp: TNotifyEvent;
    FMasterField: string;      {new}
    FDetailField: string;      {new}
    FIconField: string;        {new}
    FStartMasterValue: string; {new}
    procedure KeyValueChanged; override;
    procedure ListLinkActiveChanged; override;
{    procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);}
    procedure StopTracking;
    procedure TrackButton(X, Y: Integer);
    procedure CMCancelMode(var Msg: TCMCancelMode); message CM_CANCELMODE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMGetDataLink(var Msg: TMessage); message CM_GETDATALINK;
    procedure WMCancelMode(var Msg: TMessage); message WM_CANCELMODE;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
  private
    {$IFDEF COMPILER4_UP}
    FAutoExpand: Boolean;
    FChangeDelay: Integer;
    FHotTrack: Boolean;
    FRowSelect: Boolean;
    FToolTips: Boolean;
    FOnCustomDraw: TTVCustomDrawEvent;
    FOnCustomDrawItem: TTVCustomDrawItemEvent;
    FOnGetImageIndex: TTVExpandedEvent;
    {$ENDIF COMPILER4_UP}
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CloseUp(Accept: Boolean);
    procedure DropDown;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property KeyValue;
    property ListVisible: Boolean read FListVisible;
    property Text: string read FText;
  published
    property Color;
    property Ctl3D;
    property DataField;
    property DataSource;
    property DragCursor;
    property DragMode;
    property DropDownAlign: TDropDownAlign read FDropDownAlign write FDropDownAlign default daLeft;
//    property DropDownRows: Integer read FDropDownRows write FDropDownRows default 7;
    property DropDownWidth: Integer read FDropDownWidth write FDropDownWidth default 0;
    {new}
    property DropDownHeight: Integer read FDropDownHeight write FDropDownHeight default 100;
    
    property Enabled;
    property Font;
    property KeyField;
    property ListField;
    property UseFilter;
    {new}
    property MasterField: string read FMasterField write FMasterField;
    property DetailField: string read FDetailField write FDetailField;
    property IconField: string read FIconField write FIconField;
    property StartMasterValue: string read FStartMasterValue write FStartMasterValue;

    property ListFieldIndex;
    property ListSource;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    {$IFDEF COMPILER3_UP}
    property ImeMode;
    property ImeName;
    {$ENDIF COMPILER3_UP}
    {$IFDEF COMPILER4_UP}
    property Anchors;
    property BiDiMode;
    property BorderWidth;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    property AutoExpand: Boolean read FAutoExpand write FAutoExpand;
    property ChangeDelay: Integer read FChangeDelay write FChangeDelay;
    property HotTrack: Boolean read FHotTrack write FHotTrack;
    property RowSelect: Boolean read FRowSelect write FRowSelect;
    property ToolTips: Boolean read FToolTips write FToolTips;
    property OnCustomDraw: TTVCustomDrawEvent read FOnCustomDraw write FOnCustomDraw;
    property OnCustomDrawItem: TTVCustomDrawItemEvent read FOnCustomDrawItem write FOnCustomDrawItem;
    property OnGetImageIndex: TTVExpandedEvent read FOnGetImageIndex write FOnGetImageIndex;
    {$ENDIF COMPILER4_UP}
  end;

{###################### Borland ######################}

  TJvPopupTree = class(TJvDBTreeView)
  private
    procedure CNNotify(var Msg: TWMNotify); message CN_NOTIFY;
    procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
  protected
    procedure DblClick; override;
  end;

  TJvPopupDataList = class(TWinControl)
  private
    FTree: TJvPopupTree;
    function GetKeyValue: Variant;
    property KeyValue: Variant read GetKeyValue;
   // procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TJvDBLookupTreeView = class(TJvDBLookupControl)
  private
    FTree: TJvDBTreeView;
    FBorderStyle: TBorderStyle;
    InKeyValueChanged: Boolean;
    procedure SetBorderStyle(Value: TBorderStyle);
    function GetMasterField: string;
    procedure SetMasterField(Value: string);
    function GetDetailField: string;
    procedure SetDetailField(Value: string);
    function GetStartMasterValue: string;
    procedure SetStartMasterValue(Value: string);
    function GetIconField: string;
    procedure SetIconField(const Value: string);
    procedure KeyValueChanged; override;
    procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
   {Tree}
    function GetShowButtons: Boolean;
    function GetShowLines: Boolean;
    function GetShowRoot: Boolean;
    function GetReadOnly: Boolean;
    function GetHideSelection: Boolean;
    function GetIndent: Integer;
    procedure SetShowButtons(Value: Boolean);
    procedure SetShowLines(Value: Boolean);
    procedure SetShowRoot(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure SetHideSelection(Value: Boolean);
    procedure SetIndent(Value: Integer);
    {$IFDEF COMPILER3_UP}
    function GetRightClickSelect: Boolean;
    procedure SetRightClickSelect(Value: Boolean);
    {$ENDIF COMPILER3_UP}
    {$IFDEF COMPILER4_UP}
    function GetAutoExpand: Boolean;
    function GetChangeDelay: Integer;
    function GetHotTrack: Boolean;
    function GetOnGetImageIndex: TTVExpandedEvent;
    function GetRowSelect: Boolean;
    function GetToolTips: Boolean;
    procedure SetAutoExpand(const Value: Boolean);
    procedure SetChangeDelay(const Value: Integer);
    procedure SetHotTrack(const Value: Boolean);
    procedure SetOnGetImageIndex(const Value: TTVExpandedEvent);
    procedure SetRowSelect(const Value: Boolean);
    procedure SetToolTips(const Value: Boolean);
    function GetOnCustomDraw: TTVCustomDrawEvent;
    function GetOnCustomDrawItem: TTVCustomDrawItemEvent;
    procedure SetOnCustomDraw(const Value: TTVCustomDrawEvent);
    procedure SetOnCustomDrawItem(const Value: TTVCustomDrawItemEvent);
    {$ENDIF COMPILER4_UP}
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure ListLinkActiveChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Color;
    property Ctl3D;
    property DataField;
    property DataSource;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property KeyField;
    property ListField;
    property ListFieldIndex;
    property ListSource;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    {$IFDEF COMPILER3_UP}
    property ImeMode;
    property ImeName;
    {$ENDIF COMPILER3_UP}
    {$IFDEF COMPILER4_UP}
    property Anchors;
    property BiDiMode;
    property BorderWidth;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;

    property AutoExpand: Boolean read GetAutoExpand write SetAutoExpand;
    property ChangeDelay: Integer read GetChangeDelay write SetChangeDelay;
    property HotTrack: Boolean read GetHotTrack write SetHotTrack;
    property RowSelect: Boolean read GetRowSelect write SetRowSelect;
    property ToolTips: Boolean read GetToolTips write SetToolTips;
    property OnCustomDraw: TTVCustomDrawEvent read GetOnCustomDraw write SetOnCustomDraw;
    property OnCustomDrawItem: TTVCustomDrawItemEvent read GetOnCustomDrawItem write SetOnCustomDrawItem;
    property OnGetImageIndex: TTVExpandedEvent read GetOnGetImageIndex write SetOnGetImageIndex;
    {$ENDIF COMPILER4_UP}
    {Tree}
    property MasterField: string read GetMasterField write SetMasterField;
    property DetailField: string read GetDetailField write SetDetailField;
    property IconField: string read GetIconField write SetIconField;
    property StartMasterValue: string read GetStartMasterValue write SetStartMasterValue;
    property ShowButtons: Boolean read GetShowButtons write SetShowButtons;
    property ShowLines: Boolean read GetShowLines write SetShowLines;
    property ShowRoot: Boolean read GetShowRoot write SetShowRoot;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    {$IFDEF COMPILER3_UP}
    property RightClickSelect: Boolean read GetRightClickSelect write SetRightClickSelect;
    {$ENDIF COMPILER3_UP}
    property HideSelection: Boolean read GetHideSelection write SetHideSelection;
    property Indent: Integer read GetIndent write SetIndent;
  end;

implementation

uses DBConsts;

//=== TJvDataSourceLink ======================================================

{$IFDEF COMPILER2}
function GetFieldProperty(DataSet: TDataSet; Control: TComponent;
  const FieldName: string): TField;
begin
  Result := DataSet.FindField(FieldName);
  if Result = nil then
    raise EDatabaseError.CreateFmt(LoadStr(SFieldNotFound), [Control.Name, FieldName]);
end;
{$ENDIF COMPILER2}

procedure TJvDataSourceLink.ActiveChanged;
begin
  if FDBLookupControl <> nil then
    FDBLookupControl.DataLinkActiveChanged;
end;

procedure TJvDataSourceLink.RecordChanged(Field: TField);
begin
  if FDBLookupControl <> nil then
    FDBLookupControl.DataLinkRecordChanged(Field);
end;

procedure TJvDataSourceLink.FocusControl(Field: TFieldRef);
begin
  if (Field^ <> nil) and (Field^ = FDBLookupControl.Field) and
    (FDBLookupControl <> nil) and FDBLookupControl.CanFocus then
  begin
    Field^ := nil;
    FDBLookupControl.SetFocus;
  end;
end;

procedure TJvListSourceLink.ActiveChanged;
begin
  if FDBLookupControl <> nil then
    FDBLookupControl.ListLinkActiveChanged;
end;

procedure TJvListSourceLink.DataSetChanged;
begin
  if FDBLookupControl <> nil then
    FDBLookupControl.ListLinkDataChanged;
end;

//=== TJvDBLookupControl =====================================================

function VarEquals(const V1, V2: Variant): Boolean;
begin
  Result := False;
  try
    Result := V1 = V2;
  except
  end;
end;

// (rom) definitely bad implementation two components may concurrently use
// (rom) the variable
var
  SearchTickCount: Integer = 0;

constructor TJvDBLookupControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if NewStyleControls then
    ControlStyle := [csOpaque]
  else
    ControlStyle := [csOpaque, csFramed];
  ParentColor := False;
  TabStop := True;
  FLookupSource := TDataSource.Create(Self);
  FDataLink := TJvDataSourceLink.Create;
  FDataLink.FDBLookupControl := Self;
  FListLink := TJvListSourceLink.Create;
  FListLink.FDBLookupControl := Self;
  FListFields := TList.Create;
  FKeyValue := Null;
end;

destructor TJvDBLookupControl.Destroy;
begin
  FListFields.Free;
  FListLink.FDBLookupControl := nil;
  FListLink.Free;
  FDataLink.FDBLookupControl := nil;
  FDataLink.Free;
  inherited Destroy;
end;

function TJvDBLookupControl.CanModify: Boolean;
begin
  Result := FListActive and not ReadOnly and ((FDataLink.DataSource = nil) or
    (FMasterField <> nil) and FMasterField.CanModify);
end;

procedure TJvDBLookupControl.CheckNotCircular;
begin
  if (FDataLink.Active and FDataLink.DataSet.IsLinkedTo(ListSource)) or
    (FListLink.Active and FListLink.DataSet.IsLinkedTo(DataSource)) then
    {$IFDEF COMPILER3_UP}
    DatabaseError(SCircularDataLink);
    {$ELSE}
    DatabaseError(LoadStr(SCircularDataLink));
    {$ENDIF COMPILER3_UP}
end;

procedure TJvDBLookupControl.CheckNotLookup;
begin
  if FLookupMode then
    {$IFDEF COMPILER3_UP}
    DatabaseError(SPropDefByLookup);
    {$ELSE}
    DatabaseError(LoadStr(SPropDefByLookup));
    {$ENDIF COMPILER3_UP}
  if FDataLink.DataSourceFixed then
    {$IFDEF COMPILER3_UP}
    DatabaseError(SDataSourceFixed);
    {$ELSE}
    DatabaseError(LoadStr(SDataSourceFixed));
    {$ENDIF COMPILER3_UP}
end;

procedure TJvDBLookupControl.DataLinkActiveChanged;
begin
  FDataField := nil;
  FMasterField := nil;
  if FDataLink.Active and (FDataFieldName <> '') then
  begin
    CheckNotCircular;
    FDataField := GetFieldProperty(FDataLink.DataSet, Self, FDataFieldName);
    FMasterField := FDataField;
  end;
  SetLookupMode((FDataField <> nil) and (FDataField.FieldKind = fkLookup));
  DataLinkRecordChanged(nil);
end;

procedure TJvDBLookupControl.DataLinkRecordChanged(Field: TField);
begin
  if (Field = nil) or (Field = FMasterField) then
    if FMasterField <> nil then
      SetKeyValue(FMasterField.Value)
    else
      SetKeyValue(Null);
end;

function TJvDBLookupControl.GetBorderSize: Integer;
var
  Params: TCreateParams;
  R: TRect;
begin
  CreateParams(Params);
  SetRect(R, 0, 0, 0, 0);
  AdjustWindowRectEx(R, Params.Style, False, Params.ExStyle);
  Result := R.Bottom - R.Top;
end;

function TJvDBLookupControl.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TJvDBLookupControl.GetKeyFieldName: string;
begin
  if FLookupMode then
    Result := ''
  else
    Result := FKeyFieldName;
end;

function TJvDBLookupControl.GetListSource: TDataSource;
begin
  if FLookupMode then
    Result := nil
  else
    Result := FListLink.DataSource;
end;

function TJvDBLookupControl.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

function TJvDBLookupControl.GetTextHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  Result := Metrics.tmHeight;
end;

procedure TJvDBLookupControl.KeyValueChanged;
begin
end;

procedure TJvDBLookupControl.ListLinkActiveChanged;
var
  DataSet: TDataSet;
  ResultField: TField;
begin
  FListActive := False;
  FKeyField := nil;
  FListField := nil;
  FListFields.Clear;
  if FListLink.Active and (FKeyFieldName <> '') then
  begin
    CheckNotCircular;
    DataSet := FListLink.DataSet;
    FKeyField := GetFieldProperty(DataSet, Self, FKeyFieldName);
    try
      DataSet.GetFieldList(FListFields, FListFieldName);
    except
      {$IFDEF COMPILER3_UP}
      DatabaseErrorFmt(SFieldNotFound, [Self.Name, FListFieldName]);
      {$ELSE}
      raise EDatabaseError.CreateFmt(LoadStr(SFieldNotFound), [Self.Name, FListFieldName]);
      {$ENDIF COMPILER3_UP}
    end;
    if FLookupMode then
    begin
      ResultField := GetFieldProperty(DataSet, Self, FDataField.LookupResultField);
      if FListFields.IndexOf(ResultField) < 0 then
        FListFields.Insert(0, ResultField);
      FListField := ResultField;
    end
    else
    begin
      if FListFields.Count = 0 then
        FListFields.Add(FKeyField);
      if (FListFieldIndex >= 0) and (FListFieldIndex < FListFields.Count) then
        FListField := FListFields[FListFieldIndex]
      else
        FListField := FListFields[0];
    end;
    FListActive := True;
  end;
end;

procedure TJvDBLookupControl.ListLinkDataChanged;
begin
end;

function TJvDBLookupControl.LocateKey: Boolean;
begin
  Result := False;
  try
    if not VarIsNull(FKeyValue) and
      FListLink.DataSet.Locate(FKeyFieldName, FKeyValue, []) then
      Result := True;
  except
  end;
end;

procedure TJvDBLookupControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then
      DataSource := nil;
    if (FListLink <> nil) and (AComponent = ListSource) then
      ListSource := nil;
  end;
end;

procedure TJvDBLookupControl.ProcessSearchKey(Key: Char);
var
  TickCount: Integer;
  S: string;
begin
  if (FListField <> nil) and (FListField.FieldKind = fkData) and
    (FListField.DataType = ftString) then
    case Word(Key) of
      VK_BACK, VK_ESCAPE:
        FSearchText := '';
      VK_SPACE..255:
        if CanModify then
        begin
          TickCount := GetTickCount;
          if TickCount - SearchTickCount > 2000 then
            FSearchText := '';
          SearchTickCount := TickCount;
          if Length(FSearchText) < 32 then
          begin
            S := FSearchText + Key;
            if FListLink.DataSet.Locate(FListField.FieldName, S,
              [loCaseInsensitive, loPartialKey]) then
            begin
              SelectKeyValue(FKeyField.Value);
              FSearchText := S;
            end;
          end;
        end;
    end;
end;

procedure TJvDBLookupControl.SelectKeyValue(const Value: Variant);
begin
  if FMasterField <> nil then
  begin
    if FDataLink.Edit then
      FMasterField.Value := Value;
  end
  else
    SetKeyValue(Value);
  Repaint;
  Click;
end;

procedure TJvDBLookupControl.SetDataFieldName(const Value: string);
begin
  if FDataFieldName <> Value then
  begin
    FDataFieldName := Value;
    DataLinkActiveChanged;
  end;
end;

procedure TJvDBLookupControl.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TJvDBLookupControl.SetKeyFieldName(const Value: string);
begin
  CheckNotLookup;
  if FKeyFieldName <> Value then
  begin
    FKeyFieldName := Value;
    ListLinkActiveChanged;
  end;
end;

procedure TJvDBLookupControl.SetKeyValue(const Value: Variant);
begin
  if not VarEquals(FKeyValue, Value) then
  begin
    FKeyValue := Value;
    KeyValueChanged;
  end;
end;

procedure TJvDBLookupControl.SetListFieldName(const Value: string);
begin
  if FListFieldName <> Value then
  begin
    FListFieldName := Value;
    ListLinkActiveChanged;
  end;
end;

procedure TJvDBLookupControl.SetListSource(Value: TDataSource);
begin
  CheckNotLookup;
  FListLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TJvDBLookupControl.SetLookupMode(Value: Boolean);
begin
  if FLookupMode <> Value then
    if Value then
    begin
      FMasterField := GetFieldProperty(FDataField.DataSet, Self, FDataField.KeyFields);
      FLookupSource.DataSet := FDataField.LookupDataSet;
      FKeyFieldName := FDataField.LookupKeyFields;
      FLookupMode := True;
      FListLink.DataSource := FLookupSource;
    end
    else
    begin
      FListLink.DataSource := nil;
      FLookupMode := False;
      FKeyFieldName := '';
      FLookupSource.DataSet := nil;
      FMasterField := FDataField;
    end;
end;

procedure TJvDBLookupControl.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

procedure TJvDBLookupControl.WMGetDlgCode(var Msg: TMessage);
begin
  Msg.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

procedure TJvDBLookupControl.WMKillFocus(var Msg: TMessage);
begin
  FFocused := False;
  inherited;
  Invalidate;
end;

procedure TJvDBLookupControl.WMSetFocus(var Msg: TMessage);
begin
  FFocused := True;
  inherited;
  Invalidate;
end;

procedure TJvDBLookupControl.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := Integer(FDataLink);
end;

//=== TJvDBLookupTreeViewCombo ===============================================

constructor TJvDBLookupTreeViewCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  Width := 145;
  Height := 0;
  FDataList := TJvPopupDataList.Create(Self);
  FDataList.Visible := False;
  FDataList.Parent := Self;
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
  FDropDownHeight := 100;
end;

procedure TJvDBLookupTreeViewCombo.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    if NewStyleControls and Ctl3D then
      ExStyle := ExStyle or WS_EX_CLIENTEDGE
    else
      Style := Style or WS_BORDER;
end;

procedure TJvDBLookupTreeViewCombo.Paint;
var
  W, X, Flags: Integer;
  Text: string;
  Alignment: TAlignment;
  Selected: Boolean;
  R: TRect;
begin
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
  Selected := FFocused and not FListVisible and
    not (csPaintCopy in ControlState);
  if Selected then
  begin
    Canvas.Font.Color := clHighlightText;
    Canvas.Brush.Color := clHighlight;
  end;
  if (csPaintCopy in ControlState) and (FDataField <> nil) then
  begin
    Text := FDataField.DisplayText;
    Alignment := FDataField.Alignment;
  end
  else
  begin
    Text := FText;
    Alignment := FAlignment;
  end;
  W := ClientWidth - FButtonWidth;
  X := 2;
  case Alignment of
    taRightJustify: X := W - Canvas.TextWidth(Text) - 3;
    taCenter: X := (W - Canvas.TextWidth(Text)) div 2;
  end;
  SetRect(R, 1, 1, W - 1, ClientHeight - 1);
  Canvas.TextRect(R, X, 2, Text);
  if Selected then
    Canvas.DrawFocusRect(R);
  SetRect(R, W, 0, ClientWidth, ClientHeight);
  if not FListActive then
    Flags := DFCS_SCROLLCOMBOBOX or DFCS_INACTIVE
  else
    if FPressed then
    Flags := DFCS_SCROLLCOMBOBOX or DFCS_FLAT or DFCS_PUSHED
  else
    Flags := DFCS_SCROLLCOMBOBOX;
  DrawFrameControl(Canvas.Handle, R, DFC_SCROLL, Flags);
end;

procedure TJvDBLookupTreeViewCombo.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, GetTextHeight + GetBorderSize + 4);
end;

procedure TJvDBLookupTreeViewCombo.KeyValueChanged;
begin
  if FLookupMode then
  begin
    FText := FDataField.DisplayText;
    FAlignment := FDataField.Alignment;
  end
  else
  if FListActive and LocateKey then
  begin
    FText := FListField.DisplayText;
    FAlignment := FListField.Alignment;
  end
  else
  begin
    FText := '';
    FAlignment := taLeftJustify;
  end;
  Invalidate;
end;

procedure TJvDBLookupTreeViewCombo.ListLinkActiveChanged;
begin
  inherited ListLinkActiveChanged;
  KeyValueChanged;
end;

procedure TJvDBLookupTreeViewCombo.CloseUp(Accept: Boolean);
var
  ListValue: Variant;
begin
  if FListVisible then
  begin
    if GetCapture <> 0 then
      SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    ListValue := FDataList.KeyValue;
    SetFocus;
    SetWindowPos(FDataList.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    FListVisible := False;
   // FDataList.ListSource := nil;
    FDataList.FTree.DataSource := nil;
    Invalidate;
    FSearchText := '';
    if Accept and CanModify then
      SelectKeyValue(ListValue);
    if Assigned(FOnCloseUp) then
      FOnCloseUp(Self);
    FPressed := False;
    Repaint;
  end;
end;

procedure TJvDBLookupTreeViewCombo.DropDown;
var
  P: TPoint;
  {I,}Y: Integer;
  {S: string;}
  {$IFDEF COMPILER4_UP}
  OldLong: Longword;
  {$ENDIF COMPILER4_UP}
begin
  if not FListVisible and FListActive then
  begin
    if Assigned(FOnDropDown) then
      FOnDropDown(Self);
    FDataList.Color := Color;
    FDataList.Font := Font;
    if FDropDownWidth > 0 then
      FDataList.Width := FDropDownWidth
    else
      FDataList.Width := Width;
    FDataList.Height := FDropDownHeight;
   // FDataList.RowCount := FDropDownRows;
   // FDataList.KeyField := FKeyFieldName;
    FDataList.FTree.MasterField := FKeyFieldName;
    FDataList.FTree.DetailField := FDetailField;
    FDataList.FTree.IconField := FIconField;
    FDataList.FTree.MasterField := FMasterField;
    FDataList.FTree.StartMasterValue := FStartMasterValue;
    FDataList.FTree.UseFilter := FUseFilter;

    {$IFDEF COMPILER4_UP}
   {Source added by Oussama Al-Rifai}
    OldLong := GetWindowLong(FDataList.FTree.Handle, GWL_EXSTYLE);
    if BiDiMode <> bdLeftToRight then
    begin
      FDataList.FTree.BiDiMode := bdLeftToRight;
      SetWindowLong(FDataList.FTree.Handle, GWL_EXSTYLE, OldLong or $00400000);
    end
    else
      SetWindowLong(FDataList.FTree.Handle, GWL_EXSTYLE, OldLong and not $00400000);
   {End of source added by Oussama Al-Rifai}

    FDataList.FTree.AutoExpand := FAutoExpand;
    FDataList.FTree.ChangeDelay := FChangeDelay;
    FDataList.FTree.HotTrack := FHotTrack;
    FDataList.FTree.RowSelect := FRowSelect;
    FDataList.FTree.ToolTips := FToolTips;
    FDataList.FTree.OnCustomDraw := FOnCustomDraw;
    FDataList.FTree.OnCustomDrawItem := FOnCustomDrawItem;
    FDataList.FTree.OnGetImageIndex := FOnGetImageIndex;
    FDataList.FTree.ReadOnly := not FDataLink.ReadOnly;
    {$ENDIF COMPILER4_UP}

   { for I := 0 to FListFields.Count - 1 do
      S := S + TField(FListFields[I]).FieldName + ';';
    FDataList.ListField := S;}
    FDataList.FTree.ItemField := ListField;

   // FDataList.ListFieldIndex := FListFields.IndexOf(FListField);
   // FDataList.ListSource := FListLink.DataSource;
    FDataList.FTree.DataSource := FListLink.DataSource;
   { FDataList.FTree.FullExpand;
    FDataList.FTree.FullCollapse;
    FDataList.FTree.DataChanged; }
    FDataList.FTree.SelectNode(
      FListLink.DataSet.Lookup(FKeyFieldName, FKeyValue, FMasterField));

   // FDataList.KeyValue := KeyValue;

    P := Parent.ClientToScreen(Point(Left, Top));
    Y := P.Y + Height;
    if Y + FDataList.Height > Screen.Height then
      Y := P.Y - FDataList.Height;
    case FDropDownAlign of
      daRight: Dec(P.X, FDataList.Width - Width);
      daCenter: Dec(P.X, (FDataList.Width - Width) div 2);
    end;
    FDataList.Left := P.X;
    FDataList.Top := P.Y;
    FDataList.Visible := True;
    SetWindowPos(FDataList.Handle, HWND_TOP, P.X, Y, 0, 0,
      SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
    FListVisible := True;
    Repaint;
  end;
end;

procedure TJvDBLookupTreeViewCombo.KeyDown(var Key: Word; Shift: TShiftState);
var
  Delta: Integer;
begin
  inherited KeyDown(Key, Shift);
  if FListActive and ((Key = VK_UP) or (Key = VK_DOWN)) then
    if ssAlt in Shift then
    begin
      if FListVisible then
        CloseUp(True)
      else
        DropDown;
      Key := 0;
    end
    else
    if not FListVisible then
    begin
      if not LocateKey then
        FListLink.DataSet.First
      else
      begin
        if Key = VK_UP then
          Delta := -1
        else
          Delta := 1;
        FListLink.DataSet.MoveBy(Delta);
      end;
      SelectKeyValue(FKeyField.Value);
      Key := 0;
    end;
  if (Key <> 0) and FListVisible then
    // FDataList.KeyDown(Key, Shift);
    SendMessage(FDataList.FTree.Handle, WM_KEYDOWN, Key, 0);
end;

procedure TJvDBLookupTreeViewCombo.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if FListVisible then
    if Word(Key) in [VK_RETURN, VK_ESCAPE] then
      CloseUp(Word(Key) = VK_RETURN)
    else
      FDataList.KeyPress(Key)
  else
    ProcessSearchKey(Key);
end;

procedure TJvDBLookupTreeViewCombo.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    SetFocus;
    if not FFocused then
      Exit;
    if FListVisible then
      CloseUp(False)
    else
    if FListActive then
    begin
      MouseCapture := True;
      FTracking := True;
      TrackButton(X, Y);
      DropDown;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvDBLookupTreeViewCombo.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ListPos: TPoint;
  MousePos: TSmallPoint;
begin
  if FTracking then
  begin
    TrackButton(X, Y);
    if FListVisible then
    begin
      ListPos := FDataList.ScreenToClient(ClientToScreen(Point(X, Y)));
      if PtInRect(FDataList.ClientRect, ListPos) then
      begin
        StopTracking;
        MousePos := PointToSmallPoint(ListPos);
        SendMessage(FDataList.FTree.Handle, WM_LBUTTONDOWN, 0, Integer(MousePos));
        Exit;
      end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TJvDBLookupTreeViewCombo.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  StopTracking;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvDBLookupTreeViewCombo.StopTracking;
begin
  if FTracking then
  begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TJvDBLookupTreeViewCombo.TrackButton(X, Y: Integer);
var
  NewState: Boolean;
begin
  Repaint;
  NewState := PtInRect(Rect(ClientWidth - FButtonWidth, 0, ClientWidth,
    ClientHeight), Point(X, Y));
  if FPressed <> NewState then
  begin
    FPressed := NewState;
    Repaint;
  end;
end;

procedure TJvDBLookupTreeViewCombo.CMCancelMode(var Msg: TCMCancelMode);
begin
  if (Msg.Sender <> Self) and (Msg.Sender <> FDataList) then
    CloseUp(False);
end;

procedure TJvDBLookupTreeViewCombo.CMCtl3DChanged(var Msg: TMessage);
begin
  if NewStyleControls then
  begin
    RecreateWnd;
    Height := 0;
  end;
  inherited;
end;

procedure TJvDBLookupTreeViewCombo.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  Height := 0;
end;

procedure TJvDBLookupTreeViewCombo.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := Integer(FDataLink);
end;

procedure TJvDBLookupTreeViewCombo.WMCancelMode(var Msg: TMessage);
begin
  StopTracking;
  inherited;
end;

procedure TJvDBLookupTreeViewCombo.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
//  if Msg.FocusedWnd = FDataList.Handle then SetFocus {else CloseUp(False);}
end;

//=== TJvPopupDataList =======================================================

constructor TJvPopupDataList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable];
  TabStop := False;
  FTree := TJvPopupTree.Create(Self);
  FTree.Parent := Self;
  FTree.Align := alClient;
  FTree.ReadOnly := True;
  FTree.BorderStyle := bsNone;
  FTree.HideSelection := False;
  FTree.TabStop := False;
//  FTree.OnDblClick := OnDblClick2;
end;

destructor TJvPopupDataList.Destroy;
begin
  FTree.Free;
  inherited Destroy;
end;

procedure TJvPopupDataList.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW;
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

function TJvPopupDataList.GetKeyValue: Variant;
begin
  if FTree.Selected <> nil then
//    Result := (FTree.Selected as TJvDBTreeNode).MasterValue
    Result := FTree.DataSource.DataSet.Lookup(FTree.MasterField,
      (FTree.Selected as TJvDBTreeNode).MasterValue, (Owner as TJvDBLookupControl).KeyField)
  else
    Result := Null;
end;

//=== TJvPopupTree ===========================================================

{******* from ComCtl98 unit}
  ////////////////////////////
  // Jean-Luc Mattei        //
  // jlucm@club-internet.fr //
  ////////////////////////////
const
  NM_CUSTOMDRAW = (NM_FIRST - 12);
  CDDS_PREPAINT = $000000001;
  CDRF_NOTIFYITEMDRAW = $00000020;
  CDDS_ITEM = $000010000;
  CDDS_ITEMPREPAINT = (CDDS_ITEM or CDDS_PREPAINT);
  CDIS_SELECTED = $0001;

type
  PNMCustomDrawInfo = ^TNMCustomDrawInfo;
  TNMCustomDrawInfo = packed record
    hdr: TNMHDR;
    dwDrawStage: LONGINT;
    hdc: HDC;
    rc: TRect;
    dwItemSpec: LONGINT; // this is control specific, but it's how to specify an item.  valid only with CDDS_ITEM bit set
    uItemState: Cardinal;
    lItemlParam: Longint;
  end;
{####### from ComCtl98 unit}

procedure TJvPopupTree.CNNotify(var Msg: TWMNotify);
begin
  with Msg.NMHdr^ do
    case code of
      NM_CUSTOMDRAW:
        begin
          with PNMCustomDrawInfo(Pointer(Msg.NMHdr))^ do
          begin
            if (dwDrawStage and CDDS_PREPAINT) = CDDS_PREPAINT then
              Msg.Result := CDRF_NOTIFYITEMDRAW;
            if (dwDrawStage and CDDS_ITEMPREPAINT) = CDDS_ITEMPREPAINT then
            begin
              if (uItemstate and CDIS_SELECTED) <> 0 then
              begin
                SetTextColor(hdc, ColorToRGB(clHighLightText));
                SetBkColor(hdc, ColorToRGB(clHighLight));
              end;
              Msg.Result := CDRF_NOTIFYITEMDRAW;
            end;
          end;
        end;
    else
      inherited;
    end;
end;

procedure TJvPopupTree.WMSetFocus(var Msg: TMessage);
begin
  inherited;
  Msg.Result := 1;
  (Owner.Owner as TJvDBLookupTreeViewCombo).SetFocus;
end;

procedure TJvPopupTree.DblClick;
begin
  (Owner.Owner as TJvDBLookupTreeViewCombo).CloseUp(True);
end;

//=== TJvDBLookupTreeView ====================================================

type
  TJvDBLookupTreeViewTree = class(TJvDBTreeView)
  private
    procedure DataScrolled; override;
    procedure DataChanged; override;
    procedure Change2(Node: TTreeNode); override;
    procedure DefaultHandler(var Msg); override;
  end;

constructor TJvDBLookupTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBorderStyle := bsSingle;
  FTree := TJvDBLookupTreeViewTree.Create(Self);
  FTree.Parent := Self;
  Width := FTree.Width;
  Height := FTree.Height;
  FTree.Align := alClient;
  FTree.ReadOnly := True;
  FTree.BorderStyle := bsNone;
  FTree.HideSelection := False;
//  FTree.TabStop := False;
end;

destructor TJvDBLookupTreeView.Destroy;
begin
  FTree.Free;
  inherited Destroy;
end;

procedure TJvDBLookupTreeView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    if FBorderStyle = bsSingle then
      if NewStyleControls and Ctl3D then
        ExStyle := ExStyle or WS_EX_CLIENTEDGE
      else
        Style := Style or WS_BORDER;
end;

procedure TJvDBLookupTreeView.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

function TJvDBLookupTreeView.GetMasterField: string;
begin
  Result := FTree.MasterField;
end;

procedure TJvDBLookupTreeView.SetMasterField(Value: string);
begin
  FTree.MasterField := Value;
end;

function TJvDBLookupTreeView.GetDetailField: string;
begin
  Result := FTree.DetailField;
end;

procedure TJvDBLookupTreeView.SetDetailField(Value: string);
begin
  FTree.DetailField := Value;
end;

function TJvDBLookupTreeView.GetIconField: string;
begin
  Result := FTree.IconField;
end;

procedure TJvDBLookupTreeView.SetIconField(const Value: string);
begin
  FTree.IconField := Value;
end;

function TJvDBLookupTreeView.GetStartMasterValue: string;
begin
  Result := FTree.StartMasterValue;
end;

procedure TJvDBLookupTreeView.SetStartMasterValue(Value: string);
begin
  FTree.StartMasterValue := Value;
end;

procedure TJvDBLookupTreeView.ListLinkActiveChanged;
begin
  inherited ListLinkActiveChanged;
  FTree.DataSource := ListSource;
  FTree.ItemField := ListField;
end;

procedure TJvDBLookupTreeView.KeyValueChanged;
begin
  InKeyValueChanged := True;
  try
    TJvDBLookupTreeViewTree(FTree).SelectNode(FKeyValue);
  finally
    InKeyValueChanged := False;
  end;
end;

procedure TJvDBLookupTreeView.WMSetFocus(var Msg: TMessage);
begin
  FTree.SetFocus;
end;

{** Tree}

function TJvDBLookupTreeView.GetShowButtons: Boolean;
begin
  Result := FTree.ShowButtons;
end;

function TJvDBLookupTreeView.GetShowLines: Boolean;
begin
  Result := FTree.ShowLines;
end;

function TJvDBLookupTreeView.GetShowRoot: Boolean;
begin
  Result := FTree.ShowRoot;
end;

function TJvDBLookupTreeView.GetReadOnly: Boolean;
begin
  Result := FTree.ReadOnly;
end;

{$IFDEF COMPILER3_UP}
function TJvDBLookupTreeView.GetRightClickSelect: Boolean;
begin
  Result := FTree.RightClickSelect;
end;
{$ENDIF COMPILER3_UP}

function TJvDBLookupTreeView.GetHideSelection: Boolean;
begin
  Result := FTree.HideSelection;
end;

function TJvDBLookupTreeView.GetIndent: Integer;
begin
  Result := FTree.Indent;
end;

procedure TJvDBLookupTreeView.SetShowButtons(Value: Boolean);
begin
  FTree.ShowButtons := Value;
end;

procedure TJvDBLookupTreeView.SetShowLines(Value: Boolean);
begin
  FTree.ShowLines := Value;
end;

procedure TJvDBLookupTreeView.SetShowRoot(Value: Boolean);
begin
  FTree.ShowRoot := Value;
end;

procedure TJvDBLookupTreeView.SetReadOnly(Value: Boolean);
begin
  FTree.ReadOnly := Value;
end;

{$IFDEF COMPILER3_UP}
procedure TJvDBLookupTreeView.SetRightClickSelect(Value: Boolean);
begin
  FTree.RightClickSelect := Value;
end;
{$ENDIF COMPILER3_UP}

procedure TJvDBLookupTreeView.SetHideSelection(Value: Boolean);
begin
  FTree.HideSelection := Value;
end;

procedure TJvDBLookupTreeView.SetIndent(Value: Integer);
begin
  FTree.Indent := Value;
end;

{ Translate properties }

{$IFDEF COMPILER4_UP}

function TJvDBLookupTreeView.GetAutoExpand: Boolean;
begin
  Result := FTree.AutoExpand;
end;

function TJvDBLookupTreeView.GetChangeDelay: Integer;
begin
  Result := FTree.ChangeDelay;
end;

function TJvDBLookupTreeView.GetHotTrack: Boolean;
begin
  Result := FTree.HotTrack;
end;

function TJvDBLookupTreeView.GetOnCustomDraw: TTVCustomDrawEvent;
begin
  Result := FTree.OnCustomDraw;
end;

function TJvDBLookupTreeView.GetOnCustomDrawItem: TTVCustomDrawItemEvent;
begin
  Result := FTree.OnCustomDrawItem;
end;

function TJvDBLookupTreeView.GetOnGetImageIndex: TTVExpandedEvent;
begin
  Result := FTree.OnGetImageIndex;
end;

function TJvDBLookupTreeView.GetRowSelect: Boolean;
begin
  Result := FTree.RowSelect;
end;

function TJvDBLookupTreeView.GetToolTips: Boolean;
begin
  Result := FTree.ToolTips;
end;

procedure TJvDBLookupTreeView.SetAutoExpand(const Value: Boolean);
begin
  FTree.AutoExpand := Value;
end;

procedure TJvDBLookupTreeView.SetChangeDelay(const Value: Integer);
begin
  FTree.ChangeDelay := Value;
end;

procedure TJvDBLookupTreeView.SetHotTrack(const Value: Boolean);
begin
  FTree.HotTrack := Value;
end;

procedure TJvDBLookupTreeView.SetOnCustomDraw(const Value: TTVCustomDrawEvent);
begin
  FTree.OnCustomDraw := Value;
end;

procedure TJvDBLookupTreeView.SetOnCustomDrawItem(const Value: TTVCustomDrawItemEvent);
begin
  FTree.OnCustomDrawItem := Value;
end;

procedure TJvDBLookupTreeView.SetOnGetImageIndex(const Value: TTVExpandedEvent);
begin
  FTree.OnGetImageIndex := Value;
end;

procedure TJvDBLookupTreeView.SetRowSelect(const Value: Boolean);
begin
  FTree.RowSelect := Value;
end;

procedure TJvDBLookupTreeView.SetToolTips(const Value: Boolean);
begin
  FTree.ToolTips := Value;
end;

{$ENDIF COMPILER4_UP}
{# Translate properties }

//=== TJvDBLookupTreeViewTree ================================================

procedure TJvDBLookupTreeViewTree.DataScrolled;
begin
end;

procedure TJvDBLookupTreeViewTree.DataChanged;
begin
  inherited DataChanged;
end;

procedure TJvDBLookupTreeViewTree.Change2(Node: TTreeNode);
begin
  with Owner as TJvDBLookupTreeView do
    if not InKeyValueChanged then
    begin
      FListLink.DataSet.Locate(MasterField, (Node as TJvDBTreeNode).MasterValue, []);
      SelectKeyValue(FKeyField.Value);
      KeyValueChanged;
    end;
end;

procedure TJvDBLookupTreeViewTree.DefaultHandler(var Msg);
begin
  inherited DefaultHandler(Msg);
  with TMessage(Msg) do
    case Msg of
      WM_KEYDOWN, WM_KEYUP, WM_CHAR, WM_LBUTTONDOWN, WM_LBUTTONUP,
      WM_RBUTTONDOWN, WM_RBUTTONUP, WM_MBUTTONDOWN, WM_MBUTTONUP,
      WM_MOUSEMOVE:
        PostMessage((Owner as TWinControl).Handle, Msg, WParam, LParam);
    end;
end;

end.

