unit Keyman.Developer.UI.RichEdit41;

interface

uses
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.Themes,
  Winapi.Windows;

type
  TRichEdit41 = class(TCustomRichEdit)
  strict private
    class constructor Create;
    class destructor Destroy;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  published
    property Align;
    property Alignment;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property HideScrollBars;
    property ImeMode;
    property ImeName;
    property Constraints;
    property Lines;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PlainText;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Touch;
    property Visible;
    property WantTabs;
    property WantReturns;
    property WordWrap;
    property StyleElements;
    property Zoom;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnProtectChange;
    property OnResizeRequest;
    property OnSaveClipboard;
    property OnSelectionChange;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

uses
  System.Classes,
  Winapi.RichEdit;

{ TRichEdit41 }

class constructor TRichEdit41.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TRichEdit41, TRichEditStyleHook);
end;

class destructor TRichEdit41.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TRichEdit41, TRichEditStyleHook);
end;

var
  FRichEditModule: THandle = 0;

procedure TRichEdit41.CreateParams(var Params: TCreateParams);
const
  HideScrollBars: array[Boolean] of DWORD = (ES_DISABLENOSCROLL, 0);
  HideSelections: array[Boolean] of DWORD = (ES_NOHIDESEL, 0);
  RichEditClassName = 'RICHEDIT50W';
  RichEditModuleName = 'MSFTEDIT.DLL';
begin
  if FRichEditModule = 0 then
  begin
    FRichEditModule := LoadLibrary(RichEditModuleName);
    if FRichEditModule <= HINSTANCE_ERROR then FRichEditModule := 0;
  end;

  inherited CreateParams(Params);

  CreateSubClass(Params, RichEditClassName);

  with Params do
  begin
    Style := Style or HideScrollBars[Self.HideScrollBars] or
      HideSelections[HideSelection];
  end;
end;

procedure Register;
begin
  RegisterComponents('Keyman', [TRichEdit41]);
end;

initialization
finalization
  if FRichEditModule <> 0 then FreeLibrary(FRichEditModule);
end.

