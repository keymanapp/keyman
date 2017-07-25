//*****************************************************************
//                      Rich Edit for Web Browser                 *
//                                                                *
//                     For Delphi 5 to XE                         *
//                     Freeware Component                         *
//                            by                                  *
//                     Eran Bodankin (bsalsa)                     *
//                       bsalsa@gmail.com                         *
//      Based on a Ideas from:  http://www.torry.net/             *
//                                                                *
//     Documentation and updated versions:                        *
//               http://www.bsalsa.com                            *
//*****************************************************************

{*******************************************************************************}
{LICENSE:
THIS SOFTWARE IS PROVIDED TO YOU "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED INCLUDING BUT NOT LIMITED TO THE APPLIED
WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
YOU ASSUME THE ENTIRE RISK AS TO THE ACCURACY AND THE USE OF THE SOFTWARE
AND ALL OTHER RISK ARISING OUT OF THE USE OR PERFORMANCE OF THIS SOFTWARE
AND DOCUMENTATION. BSALSA PRODUCTIONS DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SYSTEMS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SYSTEMS. BSALSA PRODUCTIONS SPECIFICALLY
DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use, change or modify the component under 4 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@gmail.com) any code change in the unit
   for the benefit of the other users.
4. Please consider donation in our web site!
{*******************************************************************************}
//$Id: RichEditBrowser.pas,v 1.2 2006/11/15 21:01:44 sergev Exp $

unit RichEditBrowser;

interface

{$I EWB.inc}

uses
  System.UITypes,
  Windows, Messages, Classes, Controls, ComCtrls, ExtCtrls, Graphics, ComObj, Menus,
  HighLightHTML, HighLightXML, EmbeddedWB, ImgList, RichEdit, ClipBrd, ActiveX;

const
  REO_GETOBJ_NO_INTERFACES = $00000000;
  REO_GETOBJ_POLEOBJ = $00000001;
  REO_GETOBJ_PSTG = $00000002;
  REO_GETOBJ_POLESITE = $00000004;
  REO_GETOBJ_ALL_INTERFACES = $00000007;
  REO_CP_SELECTION = $FFFFFFFF;
  REO_IOB_SELECTION = $FFFFFFFF;
  REO_IOB_USE_CP = $FFFFFFFE;
  REO_NULL = $00000000;
  REO_READWRITEMASK = $0000003F;
  REO_DONTNEEDPALETTE = $00000020;
  REO_BLANK = $00000010;
  REO_DYNAMICSIZE = $00000008;
  REO_INVERTEDSELECT = $00000004;
  REO_BELOWBASELINE = $00000002;
  REO_RESIZABLE = $00000001;
  REO_LINK = $80000000;
  REO_STATIC = $40000000;
  REO_SELECTED = $08000000;
  REO_OPEN = $04000000;
  REO_INPLACEACTIVE = $02000000;
  REO_HILITED = $01000000;
  REO_LINKAVAILABLE = $00800000;
  REO_GETMETAFILE = $00400000;
  RECO_PASTE = $00000000;
  RECO_DROP = $00000001;
  RECO_COPY = $00000002;
  RECO_CUT = $00000003;
  RECO_DRAG = $00000004;
  READYSTATE_COMPLETE = $00000004;
  CLSID_NULL: TCLSID = '{00000000-0000-0000-0000-000000000000}';
  clSelColor = clHighlightText;
  clSelColorBk = clHighlight;
  clTextColor = clWindowText;
  clTextColorBk = clWindow;
  clHyperlink = clBlue;
  clHyperlinkBk = clWindow;

// type
 // TRichEditVersion = 1..4;
type
  TURLClickEvent = procedure(Sender: TObject; const URL: string) of object;
  TTextAlignment = (taLeftJustify, taRightJustify, taCenter);
  TThemes = (tDefault, tXP, tBlack, tAluminum, tLight);
type
  TEditStreamCallBack = function(dwCookie: Longint; pbBuff: PByte; cb:
    Longint; var pcb: Longint): DWORD; stdcall;

  TEditStream = record
    dwCookie: Longint;
    dwError: Longint;
    pfnCallback: TEditStreamCallBack;
  end;

  IRichEditOleCallback = interface(IUnknown)
    ['{00020d03-0000-0000-c000-000000000046}']
    function GetNewStorage(out stg: IStorage): HResult; stdcall;
    function GetInPlaceContext(out Frame: IOleInPlaceFrame;
      out Doc: IOleInPlaceUIWindow;
      lpFrameInfo: POleInPlaceFrameInfo): HResult; stdcall;
    function ShowContainerUI(fShow: BOOL): HResult; stdcall;
    function QueryInsertObject(const clsid: TCLSID; const stg: IStorage;
      cp: Longint): HResult; stdcall;
    function DeleteObject(const oleobj: IOleObject): HResult; stdcall;
    function QueryAcceptData(const dataobj: IDataObject;
      var cfFormat: TClipFormat; reco: DWORD; fReally: BOOL;
      hMetaPict: HGLOBAL): HResult; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; stdcall;
    function GetClipboardData(const chrg: TCharRange; reco: DWORD;
      out dataobj: IDataObject): HResult; stdcall;
    function GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD;
      var dwEffect: DWORD): HResult; stdcall;
    function GetContextMenu(seltype: Word; const oleobj: IOleObject;
      const chrg: TCharRange; out Menu: HMENU): HResult; stdcall;
  end;


type
  TRichEditOleCallback = class(TInterfacedObject, IRichEditOleCallback)
  public
    function QueryInterface(const iid: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Longint; stdcall;
    function _Release: Longint; stdcall;
    function GetNewStorage(out stg: IStorage): HResult; stdcall;
    function GetClipboardData(const chrg: TCharRange; reco: DWORD;
      out dataobj: IDataObject): HResult; stdcall;
    function GetContextMenu(seltype: Word; const oleobj: IOleObject;
      const chrg: TCharRange; out Menu: HMENU): HResult; stdcall;
    function GetInPlaceContext(out Frame: IOleInPlaceFrame;
      out Doc: IOleInPlaceUIWindow;
      lpFrameInfo: POleInPlaceFrameInfo): HResult; stdcall;
    function ShowContainerUI(fShow: BOOL): HResult; stdcall;
    function QueryInsertObject(const clsid: TCLSID; const stg: IStorage;
      cp: Longint): HResult; stdcall;
    function DeleteObject(const oleobj: IOleObject): HResult; stdcall;
    function QueryAcceptData(const dataobj: IDataObject; var cfFormat: TClipFormat;
      reco: DWORD; fReally: BOOL; hMetaPict: HGLOBAL): HResult; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; stdcall;
    function GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD;
      var dwEffect: DWORD): HResult; stdcall;
  end;

  TRichEditWB = class(TRichEdit)

  private
   // OldStatusBarW : Integer;
    FAcceptDragComponnents: boolean;
    FAcceptDragFiles: boolean;
    FAutoNavigate: Boolean;
    FEmbeddedWB: TEmbeddedWB;
    FFileName: string;
    FHideCaret: Boolean;
    FHighlightURL: Boolean;
    FHTMLHighlight: Boolean;
    FImage: TImage;
    FModified: Boolean;
    FMoreThen64KB: Boolean;
    FOnURLClick: TURLClickEvent;
    FSelPos: Integer;
    FStatusbar: TStatusbar;
    FStream: TMemoryStream;
    FThemes: TThemes;
    FTextAlignment: TAlignment;
    FTopGap, fLeftGap: Integer;
    FRightGap, fBottomGap: Integer;
    FXMLHighlight: Boolean;
    FPopupVerbMenu: TPopupMenu;
    FAutoVerbMenu: Boolean;
    FMyCallback: TRichEditOleCallback;
    inserted: Boolean;
    function GetCanUndo: Boolean;
    function GetModified: Boolean;
    function GetRTFText: string;
    procedure CheckFileSave;
    procedure ClearAll(Sender: TObject);
    procedure ClearSel(Sender: TObject);
    procedure CNNotify(var Msg: TWMNotify); message CN_NOTIFY;
    procedure CopySel(Sender: TObject);
    procedure CutSel(Sender: TObject);
    procedure DoHTMLrc(Sender: TObject);
    procedure DoXMLrc(Sender: TObject);
    procedure FindDialog(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure PasteSel(Sender: TObject);
    procedure PerformFileOpen(const AFileName: string);
    procedure PrintSel(Sender: TObject);
    procedure Prnt(Sender: TObject);
    procedure ReplaceDialogReplace(Sender: TObject);
    procedure SetEditRect;
    procedure SetFileName(const FileName: string);
    procedure SetHideCaret(const Value: Boolean);
    procedure SetHyperlink(Setlink: Boolean; wParam: Integer);
    procedure SetModified(Value: Boolean);
    procedure SetRTFText(RichText: string);
    procedure SetTextAlignment(al: TAlignment);
    procedure UpdateInfo;
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged);
      message WM_WINDOWPOSCHANGED;

  protected
    FMax: Integer;
    FSelection: TCharRange;
    function GetPopupMenu: TPopupMenu; override;
    function GetSelStart: Integer; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure DoSetMaxLength(Value: Integer); override;
    procedure DoURLClick(const URL: string);
    procedure EMExSetSel(var Message: TMessage); message EM_EXSETSEL;
    procedure EMReplaceSel(var Message: TMessage); message EM_REPLACESEL;
    procedure setBottomGap(Value: Integer);
    procedure setLeftGap(Value: Integer);
    procedure setRightGap(Value: Integer);
    procedure setTopGap(Value: Integer);
    procedure WndProc(var Msg: TMessage); override;

  public
    CapsLockKey: string;
    CarretPosition: string;
    CompCount: Integer;
    CursorPositionX, CursorPositionY: Integer;
    HTMLSyn: THighlightHTML;
    InsertKey: string;
    LineIndex: Integer;
    NumLockKey: string;
    XMLSyn: THighlightXML;
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    destructor Destroy; override;
    function AddBitmapFromImagelist(const ASource: TCustomImageList; const
      AImageIndex: System.UITypes.TImageIndex): Integer;
    function AddBullets: Integer;
    function AddButton(bCaption, bName: string; reLeft, bLeft, bTop: Integer): Integer;
    function AddCheckBox(cbCaption, cbName: string; reLeft, cbLeft, cbTop:
      Integer; Chk: Boolean): Integer;
    function AddDateAndTime: Integer;
    function AddEditBox(eText, eName: string; reLeft, eLeft, eTop: Integer): Integer;
    function AddEmptyLine: Integer;
    function AddFile(FilePath: string; Linked: bool; AsIcon:
      Bool): Integer;
    function AddFiles(Files: TStrings; Linked: bool; AsIcon:
      Bool): Integer;
    function AddFormatedText(const txt: string; Bold, Italic, Strikeout, Underline:
      boolean; txtColor: TColor): Integer;
    function AddImage(FilePath: string): Integer;
    function AddImages(Files: TStrings): Integer;
    function AddImageUsingClipboard(FilePath: string): Integer;
    function AddLineNumbering: Integer;
    function AddRadioButton(rbCaption, rbName: string; reLeft, rbLeft, rbTop:
      Integer; Chk: boolean): Integer;
    function AddRomanNumbering: Integer;
    function AddRTFSelection(sourceStream: TStream): Integer;
    function AddRtfText(str: string): Integer;
    function AddText(const txt: string): Integer;
    function AddTextByCursor(str: string): Integer;
    function ConvertBitmapToRTF(pict: TBitmap): string;
    function ChangeToANSIChangeCase(const S: string): string;
    function GetCharactersCount: Integer;
    function GetLineCount: Integer;
    function GetLineFromChar(CharIndex: Integer): Integer;
    function GetLineIndex(LineNo: Integer): Integer;
    function GetLineLength(CharIndex: Integer): Integer;
    function GetNextWord(var s: string; var PrevWord: string): string;
    function GetRTFSelection(intoStream: TStream): string;
    function GetRTFTextToString: string;
    function GetSelectedText(var SelectedText: string): boolean;
    function GetVisibleLines: Integer;
    function IsNumber(s: string): Boolean;
    function IsSeparator(Car: Char): Boolean;
    function RemoveTextFormats: Integer;
    function SearchAndReplace(InSearch, InReplace: string): Integer;
    function SearchForTextAndSelect(SearchText: string): string;
    function SelectLine(Index: Integer): boolean;
    procedure AlignText(alignment: TAlignment);
    procedure AppendRTF(str: string);
    procedure CheckCapslock;
    procedure CheckInsertKey;
    procedure CheckNumLock;
    procedure CreateSnapShot(Pic: TBitmap);
    procedure DoHighlightHTML;
    procedure DoHighlightXML;
    procedure Find;
    procedure GetMemStatus;
    procedure GoToPosition(LineNumber, CharNumber: Word);
    procedure LoadAsCopyFromBrowser;
    procedure LoadFromFile(FileName: string);
    procedure LoadFromStream(S: TStream);
    procedure LoadHTMLFromBrowser;
    procedure LoadStreamFromBrowser;
    procedure LoadStringsFromBrowser;
    procedure LoadTextFromBrowser;
    procedure MailContext;
    procedure MailSelected;
    procedure New;
    procedure Open;
    procedure PreviewInBrowser;
    procedure PrintAll;
    procedure PrintSelectedText;
    procedure Replace;
    procedure Save;
    procedure SaveAs;
    procedure SaveToFile(FileName: string);
    procedure SaveToStream(S: TStream);
    procedure ScrollToBottom;
    procedure ScrollToTop;
    procedure SelAll(Sender: TObject);
    procedure SelectFont;
    procedure SelectionChange; override;
    procedure SetColor;
    procedure SetFontBold;
    procedure SetFontColor;
    procedure SetFontItalic;
    procedure SetFontSize;
    procedure SetFontUnderLine;
    procedure SetLineSpacing(lineSpacing: Byte);
    procedure SetOffSetsValues(SetTo: Integer);
    procedure SetSelectedBgColor;
    procedure SetSelectionHyperLink(Hyperlink: Boolean);
    procedure SetTabWidth(FTabWidth: Integer);
    procedure SetThemes(Thm: TThemes);
    procedure SetToMoreThen64KB;
    procedure SetToOEM(var Key: AnsiChar);
    procedure SetWordHyperLink(Hyperlink: Boolean);
    procedure UndoLast(Sender: TObject);
    property CanUndo: Boolean read GetCanUndo;
    property Modified: Boolean read GetModified write SetModified;
    property AutoVerbMenu: boolean read FAutoVerbMenu write FAutoVerbMenu default True;

  published
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); override;
    property AcceptDragComponnents: Boolean read fAcceptDragComponnents write
      fAcceptDragComponnents default True;
    property AcceptDragFiles: Boolean read fAcceptDragFiles write
      fAcceptDragFiles default True;
    property AutoNavigate: boolean read fAutoNavigate write fAutoNavigate;
    property EmbeddedWB: TEmbeddedWB read FEmbeddedWB write FEmbeddedWB;
    property FileName: string read fFileName write SetFileName;
    property GapBottom: Integer read FBottomGap write setBottomGap default 0;
    property GapLeft: Integer read FLeftGap write setLeftGap default 0;
    property GapRight: Integer read FRightGap write setRightGap default 0;
    property GapTop: Integer read FTopGap write setTopGap default 0;
    property HighlightHTML: boolean read fHTMLHighlight write fHTMLHighlight;
    property HighlightURL: boolean read fHighlightURL write fHighlightURL;
    property HighlightXML: boolean read fXMLHighlight write fXMLHighlight;
    property Image: TImage read fImage write fImage;
    property OnURLClick: TURLClickEvent read FOnURLClick write FOnURLClick;
    property RTFText: string read GetRTFText write SetRTFText;
    property Statusbar: TStatusbar read fStatusbar write fStatusbar;
    property SupprtMoreThen64KB: boolean read fMoreThen64KB write fMoreThen64KB;
    property TextAlignment: TAlignment read fTextAlignment write fTextAlignment;
    property HideCaret: Boolean read FHideCaret write SetHideCaret;
    property Themes: TThemes read FThemes write FThemes;

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
    property Visible;
    property WantTabs;
    property WantReturns;
    property WordWrap;
    property OnChange;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnendDock;
    property OnendDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
{$IFDEF DELPHI9_UP}
    property OnMouseActivate;
{$ENDIF}
    property OnMouseDown;
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

type
  TREObject = packed record
    cbStruct: DWORD;
    cp: longint;
    clsid: TCLSID;
    oleobj: IOleObject;
    stg: IStorage;
    olesite: IOLEClientSite;
    SIZEL: TSize;
    dvaspect: DWORD;
    dwFlags: DWORD;
    dwUser: DWORD;
  end;

type
  IRichEditOle = interface(IUnknown)['{00020d00-0000-0000-c000-000000000046}']
    function GetClientSite(out clientSite: IOleClientSite): HResult; stdcall;
    function GetObjectCount: HResult; stdcall;
    function GetLinkCount: HResult; stdcall;
    function GetObject(iob: Longint; out reobject: TReObject;
      dwFlags: DWORD): HResult; stdcall;
    function InsertObject(var reobject: TReObject): HResult; stdcall;
    function ConvertObject(iob: Longint; rclsidNew: TIID;
      lpstrUsertypeNew: LPCSTR): HResult; stdcall;
    function ActivateAs(rclsid: TIID; rclsidAs: TIID): HResult; stdcall;
    function SetHostNames(lpstrContainerApp: LPCSTR;
      lpstrContainerObj: LPCSTR): HResult; stdcall;
    function SetLinkAvailable(iob: Longint; fAvailable: BOOL): HResult; stdcall;
    function SetDvaspect(iob: Longint; dvaspect: DWORD): HResult; stdcall;
    function HandsOffStorage(iob: Longint): HResult; stdcall;
    function SaveCompleted(iob: Longint; const stg: IStorage): HResult; stdcall;
    function InPlaceDeactivate: HResult; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; stdcall;
    function GetClipboardData(var chrg: TCharRange; reco: DWORD;
      out dataobj: IDataObject): HResult; stdcall;
    function ImportDataObject(dataobj: IDataObject; cf: TClipFormat;
      hMetaPict: HGLOBAL): HResult; stdcall;
  end;



procedure CreateIStorage(out Fstorage: Istorage);
function GetRichOleInterface(ARichEdit: TRichEdit; out RichOleInterface: IRichEditOle; out OleClientSite: IOleclientSite): boolean;
procedure REOleSetCallback(RichEdit: TRichEdit; OleInterface: IRichEditOleCallback);
procedure ReleaseObject(var Obj);
function SetFormatEtc(Cf: TClipFormat; med: Longint; td: PDVTargetDevice = nil;
  Asp: Longint = DVASPECT_CONTENT; li: Longint = -1): TFormatEtc;
function OleSwitchDisplayAspect(OleObject: IOleObject; var CurrentAspect: DWORD;
  NewAspect: DWORD; METAFILEPICT: THandle; DeleteOldAspect, SetUpViewAdvise: boolean;
  AdviseSink: IAdviseSink; var MustUpdate: boolean): HRESULT;
function GetOleClassFile(const Name: string): TCLSID;
function OleCopyPasString(const Source: string; Malloc: IMalloc = nil): POleStr;
function SetStgMedium(Stg, Handle: longint; Release: pointer = nil): TStgMedium;
procedure OleFreeString(Str: POleStr; Malloc: IMalloc = nil);
function OleMalloc(Size: Longword; Malloc: IMalloc = nil): pointer;
procedure OleFree(Mem: pointer; Malloc: IMalloc = nil);
procedure ChangeOleIcon(REdit: TRichEdit; HIcon: Hwnd; LabelIcon: string);
procedure AddBitmapToRichEdit(bmp: TBitmap; RichEdit: TRichEditWB);

var
  FRichEditModule: THandle;
  RichEditOle: IRichEditOle;
  RichEditOleCallback: IRichEditOleCallback;
 // RichEditVersion     : TRichEditVersion;

implementation

uses
{$IFDEF UNICODE}
  AnsiStrings,
{$ENDIF UNICODE}
  StdCtrls, dialogs, Forms, Printers, sysUtils, ShellAPI, JPEG;

resourcestring
  sSaveChanges = 'Save changes to %s?';
  sOverWrite = 'The file already exist. Do you want to overwrite %s ?';
  sUntitled = 'Untitled';
//  sModified = 'Modified';
  sColRowInfo = 'Line: %3d   Col: %3d';

type
  TImageDataObject = class(TInterfacedObject, IDataObject)
  private
    FMedium: STGMEDIUM;
    FFormat: FORMATETC;
    FHasData: Boolean;
  protected
    function GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium): HResult; stdcall;
    function GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium): HResult; stdcall;
    function QueryGetData(const formatetc: TFormatEtc): HResult; stdcall;
    function GetCanonicalFormatEtc(const formatetc: TFormatEtc; out formatetcOut: TFormatEtc): HResult; stdcall;
    function SetData(const formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL): HResult; stdcall;
    function EnumFormatEtc(dwDirection: Longint; out enumFormatEtc: IEnumFormatEtc): HResult; stdcall;
    function DAdvise(const formatetc: TFormatEtc; advf: Longint; const advSink: IAdviseSink; out dwConnection: Longint): HResult; stdcall;
    function DUnadvise(dwConnection: Longint): HResult; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumStatData): HResult; stdcall;
    procedure SetBitmap(const ASource: TBitmap);
    function GetOleObject(const AClient: IOleClientSite;
      const AStorage: IStorage): IOleObject;
  public
    class procedure InsertBitmap(ADest: TCustomRichEdit; ASource: TBitmap);
    destructor Destroy(); override;
  end;

class procedure TImageDataObject.InsertBitmap;
var
  idoImage: TImageDataObject;
  ifOLE: IRichEditOle;
  ifData: IDataObject;
  ifClient: IOleClientSite;
  ifStorage: IStorage;
  ifBytes: ILockBytes;
  ifOLEObject: IOleObject;
  sCode: HRESULT;
  reObj: TREObject;
  gdClass: TGUID;
begin
  ifOLE := nil;
  SendMessage(ADest.Handle, EM_GETOLEINTERFACE, 0, LPARAM(@ifOLE));
  if Assigned(ifOLE) then
  try
    idoImage := TImageDataObject.Create();
    if idoImage.GetInterface(IDataObject, ifData) then
    try
      idoImage.SetBitmap(ASource);
      ifClient := nil;
      ifOLE.GetClientSite(ifClient);
      if Assigned(ifClient) then
      try
        ifBytes := nil;
        sCode := CreateILockBytesOnHGlobal(0, True, ifBytes);
        if (sCode = S_OK) and (Assigned(ifBytes)) then
        try
          sCode := StgCreateDocfileOnILockBytes(ifBytes, STGM_SHARE_EXCLUSIVE or
            STGM_CREATE or STGM_READWRITE, 0, ifStorage);
          if sCode = S_OK then
          try
            ifOLEObject := idoImage.GetOleObject(ifClient, ifStorage);
            if Assigned(ifOLEObject) then
            try
              OleSetContainedObject(ifOLEObject, True);
              sCode := ifOLEObject.GetUserClassID(gdClass);
              if sCode = S_OK then
              begin
                with reObj do
                begin
                              //clsid       := '';
                  cp := LongInt(REO_CP_SELECTION);
                  dvaspect := DVASPECT_CONTENT;
                  oleobj := ifOLEObject;
                  olesite := ifClient;
                  stg := ifStorage;
                end;
                ifOLE.InsertObject(reObj);
              end;
            finally
              ifOLEObject := nil;
            end;
          finally
            ifStorage := nil;
          end;
        finally
          ifBytes := nil;
        end;
      finally
        ifClient := nil;
      end;
    finally
      ifData := nil;
    end;
  finally
    ifOLE := nil;
  end;
end;

procedure TImageDataObject.SetBitmap;
begin
  FMedium.tymed := TYMED_GDI;
  FMedium.hBitmap := ASource.Handle;
  FMedium.unkForRelease := nil;
  FFormat.cfFormat := CF_BITMAP;
  FFormat.ptd := nil;
  FFormat.dwAspect := DVASPECT_CONTENT;
  FFormat.lindex := -1;
  FFormat.tymed := TYMED_GDI;
end;

function TImageDataObject.GetOleObject;
var
  sCode: HRESULT;
begin
  sCode := OleCreateStaticFromData(Self, IOleObject, OLERendER_FORMAT,
    @FFormat, AClient, AStorage, Result);
  if sCode <> S_OK then
  begin
    OleCheck(sCode);
    Result := nil;
  end;
end;

destructor TImageDataObject.Destroy;
begin
  if FHasData then
    ReleaseStgMedium(FMedium);
  inherited;
end;

function TImageDataObject.GetData;
var
  hDest: THandle;

begin
  hDest := OleDuplicateData(FMedium.hBitmap, CF_BITMAP, 0);
  if (hDest <> 0) then
  begin
    medium.tymed := TYMED_GDI;
    medium.hBitmap := hDest;
    medium.unkForRelease := nil;
    Result := S_OK;
  end
  else
    Result := E_HANDLE;
end;

function TImageDataObject.GetDataHere;
begin
  Result := E_NOTIMPL;
end;

function TImageDataObject.QueryGetData;
begin
  Result := E_NOTIMPL;
end;

function TImageDataObject.GetCanonicalFormatEtc;
begin
  Result := E_NOTIMPL;
end;

function TImageDataObject.SetData;
begin
  FMedium := medium;
  FFormat := formatetc;
  FHasData := True;
  Result := S_OK;
end;

function TImageDataObject.EnumFormatEtc;
begin
  Result := E_NOTIMPL;
end;

function TImageDataObject.DAdvise;
begin
  Result := E_NOTIMPL;
end;

function TImageDataObject.DUnadvise;
begin
  Result := E_NOTIMPL;
end;

function TImageDataObject.EnumDAdvise;
begin
  Result := E_NOTIMPL;
end;

function TRichEditOleCallback.QueryInterface(const iid: TGUID; out Obj): HResult;
begin
  if GetInterface(iid, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TRichEditOleCallback._AddRef: LongInt;
begin
  Inc(FRefCount);
  Result := FRefCount;
end;

function TRichEditOleCallback._Release: LongInt;
begin
  Dec(FRefCount);
  Result := FRefCount;
end;

function TRichEditOleCallback.GetNewStorage(out stg: IStorage): HResult;
begin
  try
    CreateIStorage(stg);
    Result := S_OK;
  except
    Result := E_OUTOFMEMORY;
  end;
end;

function TRichEditOleCallback.GetClipboardData(const chrg: TCharRange; reco: DWORD;
  out dataobj: IDataObject): HResult;
begin
  Result := E_NOTIMPL;
end;

function TRichEditOleCallback.GetContextMenu(seltype: Word;
  const oleobj: IOleObject; const chrg: TCharRange;
  out Menu: HMENU): HResult;
begin
 // menu:=0;
  Result := S_OK; // Result := E_NOTIMPL;
end;

function TRichEditOleCallback.GetInPlaceContext(out Frame: IOleInPlaceFrame; out Doc: IOleInPlaceUIWindow; lpFrameInfo: POleInPlaceFrameInfo): HResult;
begin
  Result := S_OK;
end;

function TRichEditOleCallback.QueryInsertObject(const clsid: TCLSID; const stg: IStorage;
  cp: Longint): HResult;
begin
  Result := NOERROR;
end;

function TRichEditOleCallback.DeleteObject(const oleobj: IOleObject): HResult;
begin
  if Assigned(oleobj) then
    oleobj.Close(OLECLOSE_NOSAVE);
  Result := NOERROR;
end;

function TRichEditOleCallback.QueryAcceptData(const dataobj: IDataObject;
  var cfFormat: TClipFormat; reco: DWORD; fReally: BOOL;
  hMetaPict: HGLOBAL): HResult;
begin
  Result := S_OK;
end;

function TRichEditOleCallback.ContextSensitiveHelp(fEnterMode: BOOL): HResult;
begin
  Result := NOERROR;
end;

function TRichEditOleCallback.GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD;
  var dwEffect: DWORD): HResult;
begin
  Result := E_NOTIMPL;
end;

function TRichEditOleCallback.ShowContainerUI(fShow: BOOL): HResult;
begin
  Result := S_OK;
end;

procedure CreateIStorage(out Fstorage: Istorage);
var
  FlockBytes: IlockBytes;
begin
  OleCheck(CreateILockBytesOnHGlobal(0, True, FLockBytes));
  OleCheck(StgCreateDocfileOnILockBytes(FLockBytes, STGM_SHARE_EXCLUSIVE or STGM_CREATE or STGM_READWRITE, 0, FStorage))
end;

function GetRichOleInterface(ARichEdit: TRichEdit; out RichOleInterface: IRichEditOle; out OleClientSite: IOleclientSite): boolean;
var
  AppName: AnsiString;
begin
  Result := False;
  if boolean(SendMessage(ARichEdit.Handle, EM_GETOLEINTERFACE, 0, longint(@RichOleInterface))) then
  begin
    try
      AppName := AnsiString(Application.Title);
      if Trim(AppName) = '' then
        AppName := AnsiString(ExtractFileName(Application.ExeName));
      RichOleInterface.SetHostNames(PAnsiChar(AppName), PAnsiChar(AppName));
      RichOleInterface.GetClientSite(OleclientSite);
      Result := True;
    except
      Result := False;
    end;
  end;
end;

procedure REOleSetCallback(RichEdit: TRichEdit; OleInterface: IRichEditOleCallback);
begin
  SendMessage(RichEdit.Handle, EM_SETOLECALLBACK, 0, LPARAM(Oleinterface));
end;

procedure ReleaseObject(var Obj);
begin
  if IUnknown(Obj) <> nil then
  begin
    IUnknown(Obj)._Release;
    IUnknown(Obj) := nil;
  end;
end;

function SetFormatEtc(Cf: TClipFormat; med: Longint; td: PDVTargetDevice = nil;
  Asp: Longint = DVASPECT_CONTENT; li: Longint = -1): TFormatEtc;
begin
  with Result do
  begin
    cfFormat := cf;
    dwAspect := asp;
    ptd := td;
    tymed := med;
    lindex := li
  end
end;

function OleSwitchDisplayAspect(OleObject: IOleObject; var CurrentAspect: DWORD;
  NewAspect: DWORD; METAFILEPICT: THandle; DeleteOldAspect, SetUpViewAdvise: boolean;
  AdviseSink: IAdviseSink; var MustUpdate: boolean): HRESULT;
var
  OleCache: IOleCache;
  ViewObject: IViewObject;
  EnumStatData: IEnumStatData;
  StatData: TStatData;
  FormatEtc: TFormatEtc;
  Medium: TStgMedium;
  Advf,
    NewConnection,
    OldAspect: longint;
  Error: HRESULT;
begin
  OleCache := nil;
  ViewObject := nil;
  EnumStatData := nil;
  OldAspect := CurrentAspect;
  MustUpdate := False;
  if Failed(OleObject.QueryInterface(IOleCache, OleCache)) then
  begin
    Result := E_INVALIDARG;
    Exit
  end;
  FormatEtc := SetFormatEtc(0, TYMED_NULL, nil, NewAspect);
  if (NewAspect = dvaspect_Icon) and (METAFILEPICT <> 0) then
    Advf := advf_nodata
  else
    Advf := ADVF_PRIMEFIRST;
  Result := OleCache.Cache(FormatEtc, Advf, NewConnection);
  if Failed(Result) then
    Exit;
  CurrentAspect := NewAspect;
  if (NewAspect = dvaspect_Icon) and (METAFILEPICT <> 0) then
  begin
    FormatEtc := SetFormatEtc(CF_METAFILEPICT, TYMED_MFPICT, nil, dvaspect_Icon);
    Medium := SetStgMedium(TYMED_MFPICT, METAFILEPICT);
    OleCache.SetData(FormatEtc, Medium, False)
  end
  else
    MustUpdate := True;
  if SetUpViewAdvise and Assigned(AdviseSink) then
    if Succeeded(OleObject.QueryInterface(IViewObject, ViewObject)) then
    begin
      ViewObject.SetAdvise(NewAspect, 0, AdviseSink);
      ViewObject := nil
    end;
  if DeleteOldAspect then
  begin
    Error := OleCache.EnumCache(EnumStatData);
    while Error = S_OK do
    begin
      Error := EnumStatData.Next(1, StatData, nil);
      if Error = S_OK then
        if StatData.FormatEtc.dwAspect = OldAspect then
          OleCache.Uncache(StatData.dwConnection)
    end
  end;
  Result := S_OK
end;

function GetOleClassFile(const Name: string): TCLSID;
var
  Buffer: POleStr;
begin
  Result := CLSID_NULL;
  Buffer := OleCopyPasString(Name);
  try
    OleCheck(GetClassFile(Buffer, Result))
  finally
    if Assigned(Buffer) then
      OleFreeString(Buffer)
  end
end;

function OleCopyPasString(const Source: string; Malloc: IMalloc = nil): POleStr;
var
  Size: Integer;
begin
  Size := Length(Source);
  if Size = 0 then
    Result := nil
  else
  begin
    Inc(Size);
    Result := OleMalloc(Size * SizeOf(WideChar), Malloc);
    if not Assigned(Result) then
      OutOfMemoryError;
    StringToWideChar(Source, Result, Size)
  end
end;

function SetStgMedium(Stg, Handle: longint; Release: pointer = nil): TStgMedium;
begin
  Result.tymed := Stg;
  Result.hGlobal := Handle;
  Result.unkForRelease := Release
end;

procedure OleFreeString(Str: POleStr; Malloc: IMalloc = nil);
begin
  OleFree(Str, Malloc)
end;

function OleMalloc(Size: Longword; Malloc: IMalloc = nil): pointer;
begin
  if not Assigned(Malloc) then
    Result := CoTaskMemAlloc(Size)
  else
    Result := Malloc.Alloc(Size)
end;

procedure OleFree(Mem: pointer; Malloc: IMalloc = nil);
var
  Ok: Integer;
begin
  if not Assigned(Malloc) then
  begin
    Ok := CoGetMalloc(MEMCTX_TASK, Malloc);
    if Ok = NOERROR then
      Assert(Ok = NOERROR, 'CoGetMalloc');
  end;
  Ok := Malloc.DidAlloc(Mem);
  if Ok = S_False then
    Assert(Ok = S_False, 'Impossible to free the memory');
  Malloc.Free(Mem)
end;

procedure ChangeOleIcon(REdit: TRichEdit; HIcon: Hwnd; LabelIcon: string);
var
  Update: Boolean;
  Selectiontype: Integer;
  RichEditOle: IRichEditOle;
  OleClientSite: IOleClientSite;
  REObject: TReObject;
begin
  Update := True;
  FillChar(ReObject, SizeOf(ReObject), 0);
  ReObject.cbStruct := SizeOf(ReObject);
  Selectiontype := SendMessage(Redit.Handle, EM_SELECTIONtype, 0, 0);
  if selectionType = SEL_OBJECT then
    GetRichOleInterface(REdit, RichEditOle, OleClientSite);
  OleCheck(RichEditOle.GetObject(Longint(REO_IOB_SELECTION), ReObject, REO_GETOBJ_POLEOBJ or REO_GETOBJ_POLESITE));
  HIcon := OleMetafilePictFromIconAndLabel(Hicon, OleCopyPasString(LabelIcon), '', 0);
  OleSwitchDisplayAspect(REObject.oleobj, REObject.dvaspect, REObject.dvaspect, Hicon, False, False, nil, Update);
  OleCheck(REobject.oleobj.Update);
end;

function TRichEditWB.ConvertBitmapToRTF(pict: TBitmap): string;
var
  bi, bb, rtf: string;
  bis, bbs: Cardinal;
  achar: string;
  hexpict: string;
  I: Integer;
begin

  GetDIBSizes(pict.Handle, bis, bbs);
  SetLength(bi, bis);
  SetLength(bb, bbs);
  GetDIB(pict.Handle, pict.Palette, PChar(bi)^, PChar(bb)^);
  rtf := '{\rtf1 {\pict\dibitmap ';
  SetLength(hexpict, (Length(bb) + Length(bi)) * 2);
  I := 2;
  for bis := 1 to Length(bi) do
  begin
    achar := Format('%x', [Integer(bi[bis])]);
    if Length(achar) = 1 then
      achar := '0' + achar;
    hexpict[I - 1] := achar[1];
    hexpict[I] := achar[2];
    Inc(I, 2);
  end;
  for bbs := 1 to Length(bb) do
  begin
    achar := Format('%x', [Integer(bb[bbs])]);
    if Length(achar) = 1 then
      achar := '0' + achar;
    hexpict[I - 1] := achar[1];
    hexpict[I] := achar[2];
    Inc(I, 2);
  end;
  rtf := rtf + hexpict + ' }}';
  Result := rtf;
end;

function TRichEditWB.AddFiles(Files: TStrings; Linked: bool; AsIcon: Bool): Integer;
var
  I: Integer;
  FilePath: string;
  Ind: word;
  HIcon: hwnd;
  Update: boolean;
  OleClientSite: IOleClientSite;
  Storage: IStorage;
  OleObject: IOleObject;
  ReObject: TReObject;
  RichEditOle: IrichEditOle;
begin
  Ind := 1;
  Update := True;
  FillChar(ReObject, SizeOf(TReObject), 0);
  for I := 0 to Files.Count - 1 do
  begin
    FilePath := Files[I];
    if GetRichOleInterface(Self, RichEDitOle, OleClientSite) then
    begin
      Storage := nil;
      try
        CreateIStorage(Storage);
        if Linked then
          OleCheck(OleCreateLinkToFile(OleCopyPasString(FilePath), IOleObject, OLERendER_DRAW, nil, OleClientSite, Storage, OleObject))
        else
          OleCheck(OleCreateLinkToFile(OleCopyPasString(FilePath), IOleObject, OLERendER_DRAW, nil, OleClientSite, Storage, OleObject));
        with ReObject do
        begin
          cbStruct := SizeOf(TReObject);
          cp := Integer(REO_CP_SELECTION);
          OleObject.GetUserClassId(CLSID);
          oleobj := OleObject;
          stg := Storage;
          olesite := OleClientSite;
          if Asicon then
            DvAspect := DVASPECT_ICON
          else
            DvAspect := DVASPECT_CONTENT;
          dwFlags := REO_RESIZABLE or REO_DYNAMICSIZE;
        end;
        if IsEqualCLSID(REObject.CLSID, CLSID_NULL) then
          REObject.CLSID := GetOleClassFile(FilePath);
        HIcon := ShellAPI.ExtractAssociatedIcon(Application.Handle, PChar(FilePath), Ind);
        HIcon := OleMetafilePictFromIconAndLabel(Hicon, OleCopyPasString(ExtractFileName(FilePath)), '', 0);
        OleSwitchDisplayAspect(OleObject, REObject.dvaspect, REObject.dvaspect, Hicon, False, False, nil, Update);
        OleCheck(RichEditOle.InsertObject(ReObject));
        SendMessage(Self.Handle, EM_SCROLLCARET, 0, 0);
        OleCheck(OleObject.Update);
      finally
        OleClientSite := nil;
        Storage := nil;
      end;
    end;
  end;
  Result := Lines.Count;
end;

function TRichEditWB.AddFile(FilePath: string; Linked: bool; AsIcon: Bool): Integer;
var
  Ind: word;
  HIcon: hwnd;
  Update: boolean;
  OleClientSite: IOleClientSite;
  Storage: IStorage;
  OleObject: IOleObject;
  ReObject: TReObject;
  RichEditOle: IrichEditOle;
begin
  inserted := True;
  Ind := 1;
  Update := True;
  FillChar(ReObject, SizeOf(TReObject), 0);
  if GetRichOleInterface(Self, RichEDitOle, OleClientSite) then
  begin
    Storage := nil;
    try
      CreateIStorage(Storage);
      if Linked then
        OleCheck(OleCreateLinkToFile(OleCopyPasString(FilePath),
          IOleObject, OLERendER_DRAW, nil, OleClientSite, Storage, OleObject))
      else
        OleCheck(OleCreateLinkToFile(OleCopyPasString(FilePath),
          IOleObject, OLERendER_DRAW, nil, OleClientSite, Storage, OleObject));
      with ReObject do
      begin
        cbStruct := SizeOf(TReObject);
        cp := Integer(REO_CP_SELECTION);
        OleObject.GetUserClassId(CLSID);
        oleobj := OleObject;
        stg := Storage;
        olesite := OleClientSite;
        if Asicon then
          DvAspect := DVASPECT_ICON
        else
          DvAspect := DVASPECT_CONTENT;
        dwFlags := REO_RESIZABLE or REO_DYNAMICSIZE;
      end;
      if IsEqualCLSID(REObject.CLSID, CLSID_NULL) then
        REObject.CLSID := GetOleClassFile(FilePath);
      HIcon := ShellAPI.ExtractAssociatedIcon(Application.Handle, PChar(FilePath), Ind);
      HIcon := OleMetafilePictFromIconAndLabel(Hicon, OleCopyPasString(ExtractFileName(FilePath)), '', 0);
      OleSwitchDisplayAspect(OleObject, REObject.dvaspect, REObject.dvaspect, Hicon, False, False, nil, Update);
      OleCheck(RichEditOle.InsertObject(ReObject));
      SendMessage(Self.Handle, EM_SCROLLCARET, 0, 0);
      OleCheck(OleObject.Update);
    finally
      OleClientSite := nil;
      Storage := nil;
    end;
  end;
  result := Lines.Count;
end;

procedure AddBitmapToRichEdit(bmp: Tbitmap; RichEdit: TRichEditWB);

  function BitmapToRTF(pict: TBitmap): string;
  var
    bi, bb, rtf: string;
    bis, bbs: Cardinal;
    achar: string;
    hexpict: string;
    I: Integer;
  begin

    GetDIBSizes(pict.Handle, bis, bbs);
    SetLength(bi, bis);
    SetLength(bb, bbs);
    GetDIB(pict.Handle, pict.Palette, PChar(bi)^, PChar(bb)^);
    rtf := '{\rtf1 {\pict\dibitmap ';
    SetLength(hexpict, (Length(bb) + Length(bi)) * 2);
    I := 2;
    for bis := 1 to Length(bi) do
    begin
      achar := Format('%x', [Integer(bi[bis])]);
      if Length(achar) = 1 then
        achar := '0' + achar;
      hexpict[I - 1] := achar[1];
      hexpict[I] := achar[2];
      Inc(I, 2);
    end;
    for bbs := 1 to Length(bb) do
    begin
      achar := Format('%x', [Integer(bb[bbs])]);
      if Length(achar) = 1 then
        achar := '0' + achar;
      hexpict[I - 1] := achar[1];
      hexpict[I] := achar[2];
      Inc(I, 2);
    end;
    rtf := rtf + hexpict + ' }}';
    Result := rtf;
  end;
var
  s: TstringStream;
begin
  S := TStringStream.Create(BitmapToRTF(bmp));
  RichEdit.PlainText := False;
 // RichEdit.StreamMode := [smSelection];
  RichEdit.Lines.LoadFromStream(S);
  S.Free;
end;

function TRichEditWB.AddImages(Files: TStrings): Integer;
var
  Ext: string;
  Pict: TPicture;
  I: Integer;
begin
  Result := 0;
  Pict := TPicture.Create;
  try
    for I := 0 to Files.Count - 1 do
    begin
      Ext := ExtractFileExt(Files[I]);
      if (Ext = '.bmp') or (Ext = '.gif') or (Ext = '.jpg') or (Ext = '.jpeg') then
      begin
        Pict.LoadFromFile(Files[I]);
        Clipboard.Assign(Pict);
        PasteFromClipboard;
        SendMessage(Handle, WM_PASTE, 0, 0);
        Result := Lines.Count;
      end
      else
      begin
        MessageDlg('This format is not supported in this feature.', mtError, [mbOK], 0);
      end
    end;
  finally
    Pict.Free;
  end;
end;

function TRichEditWB.AddImageUsingClipboard(FilePath: string): Integer;
var
  Pict: TPicture;
begin
  Pict := TPicture.Create;
  try
    inserted := True;
    Pict.LoadFromFile(FilePath);
    Clipboard.Assign(Pict);
    PasteFromClipboard;
    Result := Lines.Count;
  finally
    Pict.Free;
  end;
end;

function TRichEditWB.AddImage(FilePath: string): Integer;
var
  ImageBMP: TBitmap;
  ImageJPG: TJPEGImage;
begin
  WordWrap := False;
  if (Pos('.bmp', FilePath) > 0) or (Pos('.BMP', FilePath) > 0) then
  begin
    try
      inserted := True;
      ImageBMP := TBitmap.Create;
      ImageBMP.LoadFromFile(FilePath);
      Clipboard.Assign(ImageBMP);
         // Clipboard.AsText:=ConvertBitmapToRTF(ImageBMP);
      Result := Lines.Count;
    finally
      PasteFromClipboard;
    end;
    ImageBMP.Free;
  end
  else
    if (Pos('.jp', FilePath) > 0) or (Pos('.JP', FilePath) > 0) then
    begin
      try
        ImageJPG := TJPEGImage.Create;
        ImageJPG.LoadFromFile(FilePath);
        Clipboard.Assign(ImageJPG);
        Result := Lines.Count;
      finally
        PasteFromClipboard;
      end;
      ImageJPG.Free;
    end
    else
    begin
      MessageDlg('This format is not supported in this feature.', mtError, [mbOK], 0);
      Result := 0;
    end
end;

function EditStreamInCallback(dwCookie: Longint; pbBuff: PByte;
  cb: Longint; var pcb: Longint): DWORD; stdcall;
var
  theStream: TStream;
  dataAvail: LongInt;
begin
  theStream := TStream(dwCookie);
  with theStream do
  begin
    dataAvail := Size - Position;
    Result := 0;
    if dataAvail <= cb then
    begin
      pcb := Read(pbBuff^, dataAvail);
      if pcb <> dataAvail then
        result := DWord(E_FAIL);
    end
    else
    begin
      pcb := Read(pbBuff^, cb);
      if pcb <> cb then
        result := DWord(E_FAIL);
    end;
  end;
end;

function EditStreamOutCallback(dwCookie: Longint; pbBuff: PByte; cb:
  Longint; var pcb: Longint): DWORD; stdcall;
var
  theStream: TStream;
begin
  theStream := TStream(dwCookie);
  with theStream do
  begin
    if cb > 0 then
      pcb := Write(pbBuff^, cb);
    Result := 0;
  end;
end;

function TRichEditWB.GetRTFSelection(intoStream: TStream): string;
var
  editstream: TEditStream;
begin
  with editstream do
  begin
    dwCookie := Longint(intoStream);
    dwError := 0;
    pfnCallback := EditStreamOutCallBack;
  end;
  Perform(EM_STREAMOUT, SF_RTF or SFF_SELECTION, longint(@editstream));
  Result := SelText;
end;

function TRichEditWB.AddRTFSelection(sourceStream: TStream): Integer;
var
  EditStream: TEditStream;
begin
  with EditStream do
  begin
    dwCookie := Longint(sourceStream);
    dwError := 0;
    pfnCallback := EditStreamInCallBack;
  end;
  Perform(EM_STREAMIN, SF_RTF or SFF_SELECTION, longint(@EditStream));
  Result := Lines.Count;
end;

function TRichEditWB.AddRtfText(str: string): Integer;
var
  aMemStream: TMemoryStream;
begin
  Result := 0;
  if Length(str) > 0 then
  begin
    aMemStream := TMemoryStream.Create;
    try
      aMemStream.Write(str[1], length(str));
      aMemStream.Position := 0;
      AddRTFSelection(aMemStream);
      Result := Lines.Count;
    finally
      aMemStream.Free;
    end;
  end;
end;

procedure TRichEditWB.AppendRTF(str: string);
var
  start, length, eventmask: Integer;
begin
  eventmask := SendMessage(Handle, EM_SETEVENTMASK, 0, 0);
  SendMessage(Handle, WM_SETREDRAW, 0, 0);
  start := SelStart;
  length := SelLength;
  SelLength := 0;
  SelStart := System.Length(Text);
  AddRtfText(str);
  SelStart := start;
  SelLength := length;
  SendMessage(Handle, WM_SETREDRAW, 1, 0);
  InvalidateRect(Handle, nil, True);
  SendMessage(Handle, EM_SETEVENTMASK, 0, eventmask);
end;

function TRichEditWB.AddBitmapFromImagelist(const ASource: TCustomImageList;
  const AImageIndex: System.UITypes.TImageIndex): Integer;
var
  bmpImage: TBitmap;
begin
  inserted := True;
  bmpImage := TBitmap.Create();
  try
    ASource.GetBitmap(AImageIndex, bmpImage);
    BmpImage.Width := ASource.Width + 1;
    BmpImage.Height := ASource.Height + 1;
    TImageDataObject.InsertBitmap(Self, bmpImage);
    Result := Lines.Count;
  finally
    FreeAndNil(bmpImage);
  end;
end;

procedure TRichEditWB.WMPaint(var Msg: TWMPaint);
var
  DC: HDC;
 // R, R1: TRect;
begin
  DC := GetDC(Handle);
  if Transparent = 1 then
    SetBkMode(DC, Windows.TRANSPARENT)
  else
    SetBkMode(DC, Windows.OPAQUE);
  ReleaseDC(Handle, DC);
 {   if RichEditVersion >= 2 then
    inherited
  else
  begin
    if GetUpdateRect(Handle, R, True) then
    begin
      with ClientRect do
        R1 := Rect(Right - 3, Top, Right, Bottom);
      if IntersectRect(R, R, R1) then
        InvalidateRect(Handle, @R1, True);
    end;
  end;}inherited
end;

procedure TRichEditWB.DoSetMaxLength(Value: Integer);
begin
  if Value = 0 then
    Value := $FFFFFF;
  SendMessage(Handle, EM_EXLIMITTEXT, 0, Value);
end;

procedure TRichEditWB.SetHideCaret(const Value: Boolean);
begin
  if FHideCaret <> Value then
    FHideCaret := Value;
  if FHideCaret then
    Windows.HideCaret(Handle);
end;

function TRichEditWB.GetLineFromChar(CharIndex: Integer): Integer;
begin
  Result := SendMessage(Handle, EM_EXLINEFROMCHAR, 0, CharIndex);
end;

function TRichEditWB.GetLineIndex(LineNo: Integer): Integer;
begin
  Result := SendMessage(Handle, EM_LINEINDEX, LineNo, 0);
end;

procedure TRichEditWB.SelectionChange;
begin
  if Assigned(OnSelectionChange) then
    OnSelectionChange(Self);
end;

procedure TRichEditWB.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
 {   case RichEditVersion of
    1: CreateSubClass(Params, RICHEDIT_CLASS10A);
  else
       CreateSubClass(Params, RICHEDIT_CLASS);
  end; }
  Params.Style := Params.Style or WS_CLIPCHILDREN;
  if FRichEditModule = 0 then
  begin
    FRichEditModule := LoadLibrary('RICHED20.DLL');
    if FRichEditModule <= HINSTANCE_ERROR then
      FRichEditModule := 0;
  end;
  CreateSubClass(Params, RICHEDIT_CLASS);
end;

procedure TRichEditWB.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  SetEditRect;
end;

procedure TRichEditWB.SetEditRect;
var
  Loc: TRect;
begin
  SetRect(Loc, FLeftGap, FTopGap, (ClientWidth - 1) - FRightGap, (ClientHeight + 1) - FBottomGap);
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
end;

procedure TRichEditWB.setLeftGap(Value: Integer);
begin
  if (FLeftGap <> Value) and (Value > -1) then
  begin
    FLeftGap := Value;
    ReCreateWnd;
  end;
end;

procedure TRichEditWB.setTopGap(Value: Integer);
begin
  if (FTopGap <> Value) and (Value > -1) then
  begin
    FTopGap := Value;
    ReCreateWnd;
  end;
end;

procedure TRichEditWB.setRightGap(Value: Integer);
begin
  if (FRightGap <> Value) and (Value > -1) then
  begin
    FRightGap := Value;
    ReCreateWnd;
  end;
end;

procedure TRichEditWB.setBottomGap(Value: Integer);
begin
  if (FBottomGap <> Value) and (Value > -1) then
  begin
    FBottomGap := Value;
    ReCreateWnd;
  end;
end;

procedure TRichEditWB.PrintAll;
var
  PD: TPrintDialog;
begin
  PD := TPrintDialog.Create(Self);
  try
    if PD.Execute then
    begin
      Print(Self.Lines.Text);
    end;
  finally
    PD.Free;
  end;
end;

function TRichEditWB.AddText(const txt: string): Integer;
begin
  Lines.Add(txt);
  Result := Lines.Count;
end;

function TRichEditWB.AddTextByCursor(str: string): Integer;
var
  Str1: string;
  i, ui: Integer;
begin
  ui := Length(Lines[CaretPos.y]);
  str1 := Lines[CaretPos.y];
  if Pos('<$Cursor$>', str) > 0 then
  begin
    i := Pos('<$Cursor$>', str);
    str := StringReplace(str, '<$Cursor$>', '', [rfReplaceAll, rfIgnoreCase]);
    i := i - 1 + ui;
  end
  else
    i := -30;
  System.Insert(str, Str1, CaretPos.x + 1);
  Lines[CaretPos.y] := str1;
  if i <> -30 then
  begin
    SelStart := Perform(EM_LINEINDEX, CaretPos.y, 0) + i;
    SetFocus;
  end;
  Result := Lines.Count;
end;

function TRichEditWB.GetCharactersCount: Integer;
begin
  Result := GetTextLen;
end;

procedure TRichEditWB.SetTabWidth(FTabWidth: Integer);
begin
  WantTabs := True;
  SendMessage(Handle, EM_SETTABSTOPS, 1, Longint(@FTabWidth));
end;

procedure TRichEditWB.SetOffSetsValues(SetTo: Integer);
var
  Rect: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Rect));
  Rect.Left := SetTo;
  SendMessage(Handle, EM_SETRECT, 0, LongInt(@Rect));
  Refresh;
end;

function TRichEditWB.GetLineLength(CharIndex: Integer): Integer;
begin
  Result := SendMessage(Handle, EM_LINELENGTH, CharIndex, 0);
end;

procedure TRichEditWB.SetToOEM(var Key: AnsiChar);
var
  ch: string[1];
begin

  Font.Handle := GetStockObject(OEM_FIXED_FONT);
  CharToOem(@Key, @ch[1]);
  Key := ch[1];
end;

procedure TRichEditWB.GetMemStatus;
var
  memory: TMemoryStatus;
begin
  memory.dwLength := SizeOf(memory);
  GlobalMemoryStatus(memory);
  ShowMessage('Total memory: ' + IntToStr(memory.dwTotalPhys) + ' Bytes'
    + #10 + #13 + 'Available memory: ' + IntToStr(memory.dwAvailPhys) + ' Bytes');
end;

function TRichEditWB.IsSeparator(Car: Char): Boolean;
begin
  case Car of
    '.', ';', ',', ':', '!', '"', '''', '^', '+', '-', '*', '/', '\', ' ',
      '`', '[', ']', '(', ')', '{', '}', '?', '%', '=': Result := True;
  else
    Result := False;
  end;
end;

function TRichEditWB.GetNextWord(var s: string; var PrevWord: string): string;
begin
  Result := '';
  PrevWord := '';
  if s = '' then
    Exit;
  while (s <> '') and IsSeparator(s[1]) do
  begin
    PrevWord := PrevWord + s[1];
    Delete(s, 1, 1);
  end;
  while (s <> '') and not IsSeparator(s[1]) do
  begin
    Result := Result + s[1];
    Delete(s, 1, 1);
  end;
end;

function TRichEditWB.IsNumber(s: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Length(s) do
    case s[i] of
      '0'..'9': ;
    else
      Exit;
    end;
  Result := True;
end;

function TRichEditWB.GetVisibleLines: Integer;
begin
  Result := Height div (Abs(Self.Font.Height) + 2);
end;

procedure TRichEditWB.DoHighlightHtml;
var
  ms: TMemoryStream;
begin
  if HighlightHTML then
  begin
    HTMLSyn := THighlightHTML.Create;
    try
      HTMLSyn.SetText(Text);
      ms := TMemoryStream.Create;
      try
        HTMLSyn.ConvertToRTFStream(ms);
        PlainText := False;
        ms.Position := 0;
        Lines.LoadFromStream(ms);
        PlainText := True;
      finally
        ms.Free;
      end;
    finally
      HTMLSyn.Free;
    end;
  end;
end;

procedure TRichEditWB.DoHighlightXML;
var
  ms: TMemoryStream;
begin
  if HighlightXML then
  begin
    XMLSyn := THighlightXML.Create;
    try
      XMLSyn.SetText(Text);
      ms := TMemoryStream.Create;
      try
        XMLSyn.ConvertToRTFStream(ms);
        PlainText := False;
        ms.Position := 0;
        Lines.LoadFromStream(ms);
        PlainText := True;
      finally
        ms.Free;
      end;
    finally
      XMLSyn.Free;
    end;
  end;
end;

procedure TRichEditWB.CreateSnapShot(Pic: TBitmap);
var
  psd: TSaveDialog;
  Range: TFormatRange;
  TextBounary: TRect;
begin
  Pic.Width := Width;
  Pic.Height := Height;
  if (Pic.Width <> 0) and (Pic.Height <> 0) then
    Pic.Canvas.Draw(0, 0, Pic)
  else
    with Pic.Canvas do
    begin
      Brush.Color := Color;
      FillRect(ClipRect);
    end;
  Pic.Canvas.Brush.Style := bsClear;
  TextBounary := Rect(0, 0, Width * Screen.PixelsPerInch, Height * Screen.PixelsPerInch);
  with Range do
  begin
    hdc := Pic.Canvas.Handle;
    hdcTarget := Pic.Canvas.Handle;
    rc := TextBounary;
    rcPage := TextBounary;
    chrg.cpMin := 0;
    chrg.cpMax := -1;
  end;
  SendMessage(Handle, EM_FORMATRANGE, 1, Longint(@Range));
  SendMessage(Handle, EM_FORMATRANGE, 0, 0);
  if not Assigned(fImage) then
  begin
    psd := TSaveDialog.Create(Self);
    psd.FileName := 'EditorImage.bmp';
    psd.Filter := 'BMP file | (*.bmp)';
    try
      if psd.Execute then
        if FileExists(psd.FileName) then
          if MessageDlg(Format(sOverWrite, [psd.FileName]), mtConfirmation, mbYesNoCancel, 0)
            <> idYes then
            Exit;
      Pic.SaveToFile(psd.FileName + '.bmp');
    finally
      psd.Free;
    end;
  end;
end;

procedure TRichEditWB.CutSel(Sender: TObject);
begin
  if not ReadOnly then
    CutToClipboard;
end;

procedure TRichEditWB.Prnt(Sender: TObject);
begin
  Print(Text);
end;

procedure TRichEditWB.CopySel(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TRichEditWB.ClearSel(Sender: TObject);
begin
  if not ReadOnly then
    ClearSelection;
end;

procedure TRichEditWB.PasteSel(Sender: TObject);
begin
  if not ReadOnly then
    PasteFromClipboard;
end;

procedure TRichEditWB.SelAll(Sender: TObject);
begin
  SelectAll;
end;

procedure TRichEditWB.ClearAll(Sender: TObject);
begin
  if not ReadOnly then
    Clear;
end;

procedure TRichEditWB.UndoLast(Sender: TObject);
begin
  Undo;
end;

procedure TRichEditWB.SetHyperLink(Setlink: Boolean; wParam: Integer);
var
  cf: TCharFormat;
begin
  FillChar(cf, SizeOf(cf), 0);
  cf.cbSize := SizeOf(cf);
  cf.dwMask := CFM_LINK or CFM_COLOR or CFM_UNDERLINE;

  if Setlink then
  begin
    cf.dwEffects := CFE_LINK or CFE_UNDERLINE;
    cf.crTextColor := COLORREF(clBlue);
  end
  else
    cf.crTextColor := Font.Color;
  SendMessage(Handle, EM_SETCHARFORMAT, wParam, Integer(@cf));
end;

procedure TRichEditWB.SetSelectionHyperLink(Hyperlink: Boolean);
begin
  SetHyperlink(Hyperlink, SCF_SELECTION);
end;

procedure TRichEditWB.SetWordHyperLink(Hyperlink: Boolean);
begin
  SetHyperlink(Hyperlink, SCF_WORD or SCF_SELECTION);
end;

procedure TRichEditWB.DoURLClick(const URL: string);
var
  X: Olevariant;
begin
  if fAutoNavigate then
  begin
    if Assigned(FOnURLClick) then
      OnURLClick(Self, URL)
    else
      if Assigned(FEmbeddedWB) then
      begin
        FEmbeddedWB.Navigate(Url, X, X, X, X);
        FEmbeddedWB.SetFocusToDoc;
      end;
  end;
end;

procedure TRichEditWB.CNNotify(var Msg: TWMNotify);
var
  p: TENLink;
  sURL: string;
begin
  if fHighlightURL then
  begin
    if (Msg.NMHdr^.code = EN_LINK) then
    begin
      p := TENLink(Pointer(Msg.NMHdr)^);
      if (p.Msg = WM_LBUTTONDOWN) then
      begin
        try
          SendMessage(Handle, EM_EXSETSEL, 0, Longint(@(p.chrg)));
          sURL := SelText;
          DoURLClick(sURL);
        except
        end;
      end;
    end;
    inherited;
  end;
end;

procedure TRichEditWB.CreateWnd;
{var
   mask: Word;}
begin
  inherited CreateWnd;
  Modified := FModified;
  if fHighlightURL then
    SendMessage(Handle, EM_AUTOURLDETECT, 1, 0);
 //  mask := SendMessage(Handle, EM_GETEVENTMASK, 0, 0);
//   SendMessage(Handle, EM_SETEVENTMASK, 0, mask or ENM_LINK);
  SendMessage(Handle, EM_SETBKGNDCOLOR, 0, ColorToRGB(Color));

  DoSetMaxLength(MaxLength);
end;

procedure TRichEditWB.DestroyWnd;
begin
  FModified := Modified;
  inherited DestroyWnd;
end;

procedure TRichEditWB.WndProc(var Msg: TMessage);

  procedure Scroll(Msg, ScrollCode: Integer);
  begin
    Perform(Msg, ScrollCode, 0);
    Perform(Msg, SB_endSCROLL, 0);
  end;

begin
  if FHideCaret and not (csDesigning in ComponentState) then
  begin
    case Msg.Msg of
      WM_LBUTTONDOWN, WM_LBUTTONUP, WM_MOUSEMOVE,
        WM_LBUTTONDBLCLK, WM_CHAR, WM_KEYUP:
        begin
          Msg.Result := 0;
          if Msg.Msg = WM_LBUTTONDOWN then
            if not Focused then
              SetFocus;
          Exit;
        end;
      WM_KEYDOWN:
        begin
          case Msg.WParam of
            VK_DOWN:
              Scroll(WM_VSCROLL, SB_LINEDOWN);
            VK_UP:
              Scroll(WM_VSCROLL, SB_LINEUP);
            VK_LEFT:
              Scroll(WM_HSCROLL, SB_LINELEFT);
            VK_RIGHT:
              Scroll(WM_HSCROLL, SB_LINERIGHT);
            VK_NEXT:
              Scroll(WM_VSCROLL, SB_PAGEDOWN);
            VK_PRIOR:
              Scroll(WM_VSCROLL, SB_PAGEUP);
            VK_HOME:
              Scroll(WM_VSCROLL, SB_TOP);
            VK_end:
              Scroll(WM_VSCROLL, SB_BOTTOM);
          end;
          Msg.Result := 0;
          Exit;
        end;
    end;
  end;
  inherited WndProc(Msg);
end;

constructor TRichEditWB.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AcceptDragComponnents then
  begin
    ControlStyle := ControlStyle + [csAcceptsControls];
  end;
  CompCount := 0;
  fAcceptDragComponnents := True;
  fAcceptDragFiles := True;
  fAutoNavigate := True;
  FBottomGap := 0;
  fFileName := sUntitled;
  fHideCaret := False;
  fHighlightURL := True;
  fHTMLHighlight := True;
  fLeftGap := 0;
  fMoreThen64KB := False;
  fRightGap := 0;
  fStream := TMemoryStream.Create;
  fTopGap := 0;
  fXMLHighlight := True;
  ScrollBars := ssBoth;
  ShowHint := True;
  WordWrap := True;
  FAutoVerbMenu := True;
  FMax := 0;
  FSelection.cpMin := 0;
  FSelection.cpMax := 0;
end;

function TRichEditWB.GetPopupMenu: TPopupMenu;
var
  canCopy: Boolean;
begin
  Result := inherited GetPopupMenu;
  canCopy := SelText <> '';
  if FAutoVerbMenu and not Assigned(PopupMenu) then
  begin
    FPopupVerbMenu := TPopupMenu.Create(Self);
    with FPopupVerbMenu do
    begin
      Items.Clear;
      CleanupInstance;
      with Items do
      begin
        Add(NewItem('Undo', 0, False, CanUndo, UndoLast, 0, 'MenuItem0'));
        Add(NewLine);
        Add(NewItem('Cut', 0, False, canCopy, CutSel, 2, 'MenuItem2'));
        Add(NewItem('Copy', 0, False, canCopy, CopySel, 3, 'MenuItem3'));
        Add(NewItem('Paste', 0, False, True, PasteSel, 4, 'MenuItem4'));
        Add(NewItem('Select All', 0, False, True, SelAll, 5, 'MenuItem5'));
        Add(NewLine);
        Add(NewItem('Clear', 0, False, True, ClearAll, 6, 'MenuItem6'));
        Add(NewItem('Clear Selection', 0, False, canCopy, ClearSel, 7, 'MenuItem7'));
        Add(NewLine);
        Add(NewItem('Find', 0, False, True, FindDialog, 8, 'MenuItem8'));
        Add(NewLine);
        if fXMLHighlight then
          Add(NewItem('HighLight XML', 0, False, True, DoXMLrc, 9, 'MenuItem9'));
        if fHTMLHighlight then
          Add(NewItem('HighLight HTML', 0, False, True, DoHTMLrc, 10, 'MenuItem10'));
        Add(NewLine);
        Add(NewItem('Print', 0, False, True, Prnt, 12, 'MenuItem12'));
        Add(NewItem('Print Selected Text', 0, False, canCopy, PrintSel, 13, 'MenuItem13'));
        PostMessage(Handle, WM_NULL, 0, 0);
      end;
      Result := FPopupVerbMenu;
    end;
  end;
end;

procedure TRichEditWB.EMExSetSel(var Message: TMessage);
var
  ISel: Integer;
  XSel: ^TCharRange absolute ISel;
begin
  inherited;
  ISel := Message.LParam;
  FSelection := XSel^;
end;

procedure TRichEditWB.EMReplaceSel(var Message: TMessage);
begin
  inherited;
  FMax := FSelection.cpMax + length(PChar(Message.LParam));
end;

function TRichEditWB.GetSelStart: Integer;
begin
  if FMax = 0 then
    Result := inherited GetSelStart
  else
  begin
    Result := FMax;
    FMax := 0;
  end;
end;

procedure TRichEditWB.SetTextAlignment(al: TAlignment);
begin
  Paragraph.Alignment := al;
end;

procedure TRichEditWB.SetThemes(thm: TThemes);
begin
  Themes := Thm;
  if Thm = tBlack then
  begin
    color := clBlack;
    Font.Color := clWhite;
  end
  else
    if Thm = tAluminum then
    begin
      color := clSilver;
      Font.Color := clWhite;
    end
    else
      if Thm = tLight then
      begin
        color := clInfoBk;
        Font.Color := clBlack;
      end
      else
        if Thm = tXP then
        begin
          color := RGB(237, 242, 251);
          Font.Color := clBlack;
        end
        else
          if Thm = tDefault then
          begin
            color := clWindow;
            Font.Color := clBlack;
          end;

end;

procedure TRichEditWB.Loaded;
begin
  inherited Loaded;
  FMyCallback := TRichEditOleCallback.Create;
  REOleSetCallback(Self, FMyCallBack);
  SetTextAlignment(TextAlignment);
  SetThemes(FThemes);
 // if assigned(Fstatusbar) then OldStatusBarW := Fstatusbar.Panels[0].Width;
  UpdateInfo;
  DragAcceptFiles(Handle, True);
  if fMoreThen64KB then
    SendMessage(Handle, EM_EXLIMITTEXT, 0, $7FFFFFF0);
  if ShowHint then
    SetModified(True);
  WordWrap := True;
  WordWrap := False;
  inserted := False;
end;

destructor TRichEditWB.Destroy;
begin
 // Fstatusbar.Panels[0].Width := OldStatusBarW;
  FMyCallback.Free;
  FStream.Free;
  inherited Destroy;
end;

procedure TRichEditWB.DblClick;
begin
  inherited;
end;

procedure TRichEditWB.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

function TRichEditWB.GetModified: Boolean;
begin
  Result := FModified;
  if HandleAllocated then
    Result := SendMessage(Handle, EM_GETMODIFY, 0, 0) <> 0;
end;

function TRichEditWB.GetCanUndo: Boolean;
begin
  Result := False;
  if HandleAllocated then
    Result := SendMessage(Handle, EM_CANUNDO, 0, 0) <> 0;
end;

procedure TRichEditWB.SelectFont;
var
  fd: TFontDialog;
begin
  fd := TFontDialog.Create(Self);
  try
    fd.Font.Assign(SelAttributes);

    if Fd.Execute then
      Font.Assign(fd.Font);
    SetFocus;
  finally
    fd.Free;
  end;
end;

procedure TRichEditWB.SetFontBold;
begin
  with SelAttributes do
    if fsBold in Style then
      Style := Style - [fsBold]
    else
      Style := Style + [fsBold];
end;

procedure TRichEditWB.SetFontUnderLine;
begin
  with SelAttributes do
    if fsUnderLine in Style then
      Style := Style - [fsUnderLine]
    else
      Style := Style + [fsUnderLine];
end;

procedure TRichEditWB.SetFontItalic;
begin
  with SelAttributes do
    if fsItalic in Style then
      Style := Style - [fsItalic]
    else
      Style := Style + [fsItalic];
end;

procedure TRichEditWB.SetFontSize;
begin
  SelAttributes.Size := SelAttributes.Size + 2;
end;

procedure TRichEditWB.SetFontColor;
var
  CD: TColorDialog;
begin
  CD := TColorDialog.Create(Self);
  try
    CD.Color := clBlack;
    if CD.Execute then
      SelAttributes.Color := CD.Color;
  finally
    CD.Free;
  end;
end;

procedure TRichEditWB.SetColor;
var
  CD: TColorDialog;
begin
  CD := TColorDialog.Create(Self);
  try
    CD.Color := clWhite;
    if CD.Execute then
      Color := CD.Color;
  finally
    CD.Free;
  end;
end;

procedure TRichEditWB.SetSelectedBgColor;
var
  Format: CHARFORMAT2;
  CD: TColorDialog;
begin
  CD := TColorDialog.Create(Self);
  try
    CD.Color := clYellow;
    if CD.Execute then
      FillChar(Format, SizeOf(Format), 0);
    with Format do
    begin
      cbSize := SizeOf(Format);
      dwMask := CFM_BACKCOLOR;
      crBackColor := CD.Color;
      Perform(EM_SETCHARFORMAT, SCF_SELECTION, Longint(@Format));
    end;
  finally
    CD.Free;
  end;
end;

procedure TRichEditWB.CheckCapslock;
begin
  if Odd(GetKeyState(VK_CAPITAL)) then
    CapsLockKey := 'Caps Lock: On'
  else
    CapsLockKey := 'Caps Lock: Off';
end;

procedure TRichEditWB.CheckInsertKey;
begin
  if Odd(GetKeyState(VK_INSERT)) then
    InsertKey := 'Insert: On'
  else
    InsertKey := 'Insert: Off';
end;

procedure TRichEditWB.CheckNumLock;
begin
  if Odd(GetKeyState(VK_NUMLOCK)) then
    NumLockKey := 'NumLock: On'
  else
    NumLockKey := 'NumLock: Off';
end;

procedure TRichEditWB.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{var
   CurPos: TPoint;
   Popup : TPopupMenu;  }
begin
  UpdateInfo;
  LineIndex := Perform(EM_LINEFROMCHAR, SelStart, 0);
  {if not Assigned(PopupMenu) then
    begin
     if button = mbRight then
      begin
      Popup := TPopupMenu.Create(self);
      PopupMenu := Popup;
       with popup do
       begin
           Items.Clear;
           CleanupInstance;
           GetCursorPos(CurPos);
           Popup(CurPos.x, CurPos.y);
         with Items do
         begin
           Add(NewItem('Undo',0, False, True, UndoLast, 0, 'MenuItem0'));
           Add(NewLine);
           Add(NewItem('Cut', 0, False, True, CutSel, 2, 'MenuItem2'));
           Add(NewItem('Copy', 0, False, True, CopySel, 3, 'MenuItem3'));
           Add(NewItem('Paste', 0, False, True, PasteSel, 4, 'MenuItem4'));
           Add(NewItem('Select All', 0, False, True, SelAll, 5, 'MenuItem5'));
           Add(NewLine);
           Add(NewItem('Clear', 0, False, True, ClearAll, 6, 'MenuItem6'));
           Add(NewItem('Clear Selection', 0, False, True, ClearSel, 7, 'MenuItem7'));
           Add(NewLine);
           Add(NewItem('Find', 0, False, True, FindDialog, 8, 'MenuItem8'));
           Add(NewLine);
           if fXMLHighlight then
           Add(NewItem('HighLight XML', 0, False, True, DoXMLrc, 9, 'MenuItem9'));
           if fHTMLHighlight then
           Add(NewItem('HighLight HTML', 0, False, True, DoHTMLrc, 10, 'MenuItem10'));
           Add(NewLine);
           Add(NewItem('Print', 0, False, True, Prnt, 12, 'MenuItem12'));
           Add(NewItem('Print Selected Text', 0, False, True, PrintSel, 13, 'MenuItem13'));
         end;
       end;
       PostMessage(Handle, WM_NULL, 0, 0);
     end;
   end;  }
  inherited;
end;

procedure TRichEditWB.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TRichEditWB.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  UpdateInfo;
  LineIndex := Perform(EM_LINEFROMCHAR, SelStart, 0);
end;

procedure TRichEditWB.KeyPress(var Key: Char);
const
  KEY_CTRL_A = 01;
  KEY_CTRL_B = 02;
  KEY_CTRL_F = 06;
  KEY_CTRL_I = 9;
  KEY_CTRL_P = 16;
  KEY_CTRL_S = 19;
  KEY_CTRL_U = 21;
begin
  if (Ord(Key) = KEY_CTRL_B) or (Ord(Key) = KEY_CTRL_I) or (Ord(Key) = KEY_CTRL_S)
    or (Ord(Key) = KEY_CTRL_U) then
  begin
    with SelAttributes do
      case Ord(Key) of
        KEY_CTRL_B:
          begin
            Key := #0;
            if fsBold in Style then
              Style := Style - [fsBold]
            else
              Style := Style + [fsBold];
          end;
        KEY_CTRL_I:
          begin
            Key := #0;
            if fsItalic in Style then
              Style := Style - [fsItalic]
            else
              Style := Style + [fsItalic];
          end;
        KEY_CTRL_S:
          begin
            Key := #0;
            if fsStrikeout in Style then
              Style := Style - [fsStrikeout]
            else
              Style := Style + [fsStrikeout];
          end;
        KEY_CTRL_U:
          begin
            Key := #0;
            if fsUnderline in Style then
              Style := Style - [fsUnderline]
            else
              Style := Style + [fsUnderline];
          end;
      end;
  end;
  if (Ord(Key) = KEY_CTRL_A) then
  begin
    SelectAll;
  end
  else
    if (Ord(Key) = KEY_CTRL_F) then
    begin
      Find;
    end
    else
      if (Ord(Key) = KEY_CTRL_P) then
      begin
        PrintAll;
      end;
end;

function TRichEditWB.SelectLine(Index: Integer): boolean;
var
  StartPos, endPos: Integer;
begin
  result := False;
  if Index < 0 then
    Exit;
  StartPos := Perform(EM_LINEINDEX, Index, 0);
  if StartPos <> -1 then
  begin
    endPos := SendMessage(Handle, EM_LINEINDEX, Index + 1, 0);
    if endPos = -1 then
      endPos := StartPos + Perform(EM_LINELENGTH, StartPos, 0);
    Perform(EM_SETSEL, StartPos, endPos);
    result := True;
  end;
end;

function TRichEditWB.GetSelectedText(var SelectedText: string): boolean;
begin
  SelectedText := SelText;
  if SelectedText <> '' then
    result := True
  else
  begin
    MessageDlg('Please select text before using this feature.', mtError, [mbOK], 0);
    result := False;
  end;
end;

procedure TRichEditWB.MailSelected;
var
  em_body, em_mail, em_subject: string;
begin
  if GetSelectedText(em_body) then
  begin
    em_subject := 'Check it out please.';
    em_mail := 'mailto:?subject=' +
      em_subject + '&body=' + em_body;
    ShellExecute(Handle, 'open', PChar(em_mail), nil, nil, SW_SHOWNORMAL);
  end;
end;

procedure TRichEditWB.MailContext;
var
  em_body, em_mail, em_subject: string;
begin
  em_body := Lines.GetText;
  if em_body <> '' then
  begin
    em_subject := 'Check it out please.';
    em_mail := 'mailto:?subject=' +
      em_subject + '&body=' + em_body;
    ShellExecute(Handle, 'open', PChar(em_mail), nil, nil, SW_SHOWNORMAL);
  end
  else
    MessageDlg('Please enter text before using this feature.', mtError, [mbOK], 0);
end;

function TRichEditWB.GetLineCount: Integer;
begin
  GetLineCount := lines.Count;
end;

function TRichEditWB.AddDateAndTime: Integer;
var
  lt: TSYSTEMTIME;
  st: TSYSTEMTIME;
begin
  GetLocalTime(lt);
  GetSystemTime(st);
  Lines.Add('Date: ' + IntToStr(lt.wMonth) + '/' + IntToStr(lt.wDay) + '/' +
    IntToStr(lt.wYear) + '     On: ' + IntToStr(lt.wHour) + ':' +
    IntToStr(lt.wMinute) + ':' + IntToStr(lt.wSecond));
  Result := Lines.Count;
end;

procedure TRichEditWB.AlignText(alignment: TAlignment);
begin
  Paragraph.Alignment := alignment;
end;

function TRichEditWB.ChangeToANSIChangeCase(const S: string): string;
var
  i: Integer;
  Up: ANSIChar;
begin

  Result := S;
  for i := 1 to Length(Result) do
  begin
    Up := ANSIChar(ANSIUpperCase(Result[i])[1]);
    if ANSIChar(Result[i]) = Up then
      Result[i] := (ANSILowerCase(Result[i])[1])
    else
      Result[i] := ANSIUpperCase(Result[i])[1];
  end;
end;

function TRichEditWB.AddFormatedText(const txt: string; Bold, Italic, Strikeout,
  Underline: boolean; txtColor: TColor): Integer;
begin
  with SelAttributes do
  begin
    if Bold then
      Style := Style + [fsBold]
    else
      Style := Style - [fsBold];
    if Italic then
      Style := Style + [fsItalic]
    else
      Style := Style - [fsItalic];
    if Strikeout then
      Style := Style + [fsStrikeout]
    else
      Style := Style - [fsStrikeout];
    if Underline then
      Style := Style + [fsUnderline]
    else
      Style := Style - [fsUnderline];
    Color := txtColor;
  end;
  SelText := (txt);
  Result := Lines.Count;
end;

function TRichEditWB.RemoveTextFormats: Integer;
begin
  Font.Size := 8;
  with SelAttributes do
  begin
    Style := Style - [fsBold];
    Style := Style - [fsItalic];
    Style := Style - [fsStrikeout];
    Style := Style - [fsUnderline];
    Color := clBlack;
  end;
  Result := Lines.Count;
end;

function TRichEditWB.AddEmptyLine: Integer;
begin
  Lines.Add('');
  Result := Lines.Count;
end;

procedure TRichEditWB.SetLineSpacing(lineSpacing: Byte);
var
  pf2: ParaFormat2;
begin
  FillChar(pf2, SizeOf(pf2), 0);
  pf2.cbSize := SizeOf(PARAFORMAT2);
  pf2.dwMask := PFM_LINESPACING;
  pf2.bLineSpacingRule := lineSpacing;
  SendMessage(Handle, EM_SETPARAFORMAT, 0, Longint(@pf2));
end;

procedure TRichEditWB.SetToMoreThen64KB;
begin
  SendMessage(Handle, EM_EXLIMITTEXT, 0, $7FFFFFF0);
end;

procedure TRichEditWB.PrintSelectedText;
var
  printarea: TRect;
  richedit_outputarea: TRect;
  printresX, printresY: Integer;
  fmtRange: TFormatRange;
  nextChar: Integer;
  S: string;
begin
  Printer.beginDoc;
  try
    with Printer.Canvas do
    begin
      printresX := GetDeviceCaps(Handle, LOGPIXELSX);
      printresY := GetDeviceCaps(Handle, LOGPIXELSY);
      printarea := Rect(printresX, printresY * 3 div 2, Printer.PageWidth -
        printresX, Printer.PageHeight - printresY * 3 div 2);
      richedit_outputarea := Rect(printarea.Left * 1440 div printresX,
        printarea.Top * 1440 div printresY, printarea.Right * 1440 div printresX,
        printarea.Bottom * 1440 div printresY);
      fmtRange.hDC := Handle;
      fmtRange.hdcTarget := Handle;
      fmtRange.rc := richedit_outputarea;
      fmtRange.rcPage := Rect(0, 0, Printer.PageWidth * 1440 div printresX,
        Printer.PageHeight * 1440 div printresY);
      fmtRange.chrg.cpMin := selstart;
      fmtRange.chrg.cpMax := selStart + sellength - 1;
      S := SelText;
      while (fmtRange.chrg.cpMax > 0) and
        (S[fmtRange.chrg.cpMax] <= ' ') do
        Dec(fmtRange.chrg.cpMax);
      repeat
        nextChar := Perform(EM_FORMATRANGE, 1, Longint(@fmtRange));
        if nextchar < fmtRange.chrg.cpMax then
        begin
          printer.newPage;
          fmtRange.chrg.cpMin := nextChar;
        end;
      until nextchar >= fmtRange.chrg.cpMax;
      Perform(EM_FORMATRANGE, 0, 0);
    end;
  finally
    Printer.endDoc;
  end;
end;

function TRichEditWB.SearchForTextAndSelect(SearchText: string): string;
var
  StartPos, Position, endpos: Integer;
begin
  StartPos := 0;
  endpos := Length(Text);
  Lines.beginUpdate;
  while FindText(SearchText, StartPos, endpos, [stMatchCase]) <> -1 do
  begin
    endpos := Length(Text) - startpos;
    Position := FindText(SearchText, StartPos, endpos, [stMatchCase]);
    Inc(StartPos, Length(SearchText));
    SetFocus;
    SelStart := Position;
    SelLength := Length(SearchText);
    result := SelText;
  end;
  Lines.endUpdate;
end;

procedure TRichEditWB.FindDialogFind(Sender: TObject);
var
  S: string;
  startpos: Integer;
begin
  SelStart := 0;
  with TFindDialog(Sender) do
  begin
    if FSelPos = 0 then
      Options := Options - [frFindNext];
    if frfindNext in Options then
    begin
      StartPos := FSelPos + Length(Findtext);
      S := Copy(Lines.Text, StartPos, MaxInt);
    end
    else
    begin
      S := Lines.Text;
      StartPos := 1;
    end;
    FSelPos := Pos(FindText, S);
    if FSelPos > 0 then
    begin
      FSelPos := FSelPos + StartPos - 1;
      SelStart := FSelPos - 1;
      SelLength := Length(FindText);
      SetFocus;
    end
    else
    begin
      if frfindNext in Options then
        S := Concat('There are no further occurences of "', FindText, '".')
      else
        S := Concat('Could not find "', FindText, '".');
      MessageDlg(S, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TRichEditWB.Find;
var
  f: TFindDialog;
begin
  if not inserted then
  begin
    FSelPos := 0;
    try
      f := TFindDialog.Create(Self);
      f.OnFind := FindDialogFind;
      F.Execute;
    finally
    end;
  end
  else
    MessageDlg('You can not use this feature after inserting files.', mtError, [mbOK], 0);
end;

procedure TRichEditWB.FindDialog(Sender: TObject);
var
  f: TFindDialog;
begin
  if not inserted then
  begin
    FSelPos := 0;
    try
      f := TFindDialog.Create(Self);
      f.OnFind := FindDialogFind;
      F.Execute;
    finally
    end;
  end
  else
    MessageDlg('You can not use this feature after inserting files.', mtError, [mbOK], 0);
end;

procedure TRichEditWB.ReplaceDialogReplace(Sender: TObject);
var
  SelPos, SPos, SLen, TextLength: Integer;
  SearchString: string;
begin
  with TReplaceDialog(Sender) do
  begin
    TextLength := Length(Lines.Text);
    SPos := SelStart;
    SLen := SelLength;
    SearchString := Copy(Lines.Text, SPos + SLen + 1, TextLength - SLen + 1);
    SelPos := Pos(FindText, SearchString);
    if SelPos > 0 then
    begin
      SelStart := (SelPos - 1) + (SPos + SLen);
      SelLength := Length(FindText);
      SelText := ReplaceText;
    end
    else
      MessageDlg('Could not find "' + FindText + '".', mtError, [mbOk], 0);
  end;
end;

procedure TRichEditWB.Replace;
var
  r: TReplaceDialog;
begin
  if not inserted then
  begin
    FSelPos := 0;
    try
      r := TReplaceDialog.Create(Self);
      r.OnReplace := ReplaceDialogReplace;
      r.Execute;
    finally
    end;
  end
  else
    MessageDlg('You can not use this feature after inserting files.', mtError, [mbOK], 0);
end;

procedure TRichEditWB.GoToPosition(LineNumber, CharNumber: Word);
begin
  SelStart := Perform(EM_LINEINDEX, LineNumber, 0) + CharNumber;
  SetFocus;
end;

procedure TRichEditWB.PrintSel(Sender: TObject);
var
  printarea: TRect;
  richedit_outputarea: TRect;
  printresX, printresY: Integer;
  fmtRange: TFormatRange;
  nextChar: Integer;
  S: string;
begin
  Printer.beginDoc;
  try
    with Printer.Canvas do
    begin
      printresX := GetDeviceCaps(Handle, LOGPIXELSX);
      printresY := GetDeviceCaps(Handle, LOGPIXELSY);
      printarea := Rect(printresX, printresY * 3 div 2, Printer.PageWidth -
        printresX, Printer.PageHeight - printresY * 3 div 2);
      richedit_outputarea := Rect(printarea.Left * 1440 div printresX,
        printarea.Top * 1440 div printresY, printarea.Right * 1440 div printresX,
        printarea.Bottom * 1440 div printresY);
      fmtRange.hDC := Handle;
      fmtRange.hdcTarget := Handle;
      fmtRange.rc := richedit_outputarea;
      fmtRange.rcPage := Rect(0, 0, Printer.PageWidth * 1440 div printresX,
        Printer.PageHeight * 1440 div printresY);
      fmtRange.chrg.cpMin := selstart;
      fmtRange.chrg.cpMax := selStart + sellength - 1;
      S := SelText;
      while (fmtRange.chrg.cpMax > 0) and
        (S[fmtRange.chrg.cpMax] <= ' ') do
        Dec(fmtRange.chrg.cpMax);
      repeat
        nextChar := Perform(EM_FORMATRANGE, 1, Longint(@fmtRange));
        if nextchar < fmtRange.chrg.cpMax then
        begin
          printer.newPage;
          fmtRange.chrg.cpMin := nextChar;
        end;
      until nextchar >= fmtRange.chrg.cpMax;
      Perform(EM_FORMATRANGE, 0, 0);
    end;
  finally
    Printer.endDoc;
  end;
end;

function TRichEditWB.SearchAndReplace(InSearch, InReplace: string): Integer;
var
  X, Toend: Integer;
  oldCursor: TCursor;
begin
  oldCursor := Forms.Screen.Cursor;
  Screen.Cursor := crHourglass;
  begin
    X := 0;
    Toend := length(Text);
    X := FindText(inSearch, X, Toend, []);
    while X <> -1 do
    begin
      SetFocus;
      SelStart := X;
      SelLength := length(inSearch);
      SelText := InReplace;
      X := FindText(inSearch, X + length(InReplace), Toend, []);
    end;
  end;
  Screen.Cursor := oldCursor;
  Result := Lines.Count;
end;

procedure TRichEditWB.SetRTFText(RichText: string);
begin
  FStream.Clear;
  FStream.WriteBuffer(RichText[1], Length(RichText));
  FStream.Position := 0;
  Lines.LoadFromStream(FStream);
end;

function TRichEditWB.GetRTFText: string;
begin
  FStream.Clear;
  Lines.SaveToStream(FStream);
  Result := PChar(FStream.Memory);
end;

procedure TRichEditWB.PreviewInBrowser;
var
  st: TStringList;
begin
  if Assigned(FEmbeddedWB) then
  begin
    while EmbeddedWB.ReadyState <> READYSTATE_COMPLETE do
      EmbeddedWB.Stop;
    try
      PlainText := False;
      st := TStringList.Create;
      st.Clear;
      st.Add(Text);
      EmbeddedWB.LoadFromStrings(st);
    finally
    end;
  end
  else
    MessageDlg('You must assign a TEmbeddedWB before using this feature.', mtError, [MbOk], 0);
end;

function TRichEditWB.GetRTFTextToString: string;
var
  ss: TStringStream;
  EmptyStr: string;
begin
  EmptyStr := '';
  ss := TStringStream.Create(EmptyStr);
  try
    PlainText := False;
    Lines.SaveToStream(ss);
    Result := ss.DataString;
  finally
    ss.Free
  end;
end;

procedure TRichEditWB.LoadStreamFromBrowser;
var
  Stm: TMemoryStream;
begin
  if Assigned(FEmbeddedWB) then
  begin
    while EmbeddedWB.ReadyState <> READYSTATE_COMPLETE do
      Forms.Application.ProcessMessages;
    Stm := TMemoryStream.Create;
    PlainText := False;
    Clear;
    try
      EmbeddedWB.SaveToStream(Stm);
      Stm.Position := 0;
      Lines.LoadFromStream(Stm);
      Stm.Position := 0;
      FFileName := EmbeddedWB.LocationName;
      UpdateInfo;
      ScrollToTop;
      SelStart := Perform(EM_LINEINDEX, 1, 1);
    finally
      Stm.Free;
    end;
  end
  else
    MessageDlg('You must assign a TEmbeddedWB before using this feature.', mtError, [MbOk], 0);
end;

procedure TRichEditWB.LoadHTMLFromBrowser;
begin
  if Assigned(FEmbeddedWB) then
  begin
    PlainText := False;
    Clear;
    while EmbeddedWB.ReadyState <> READYSTATE_COMPLETE do
      Forms.Application.ProcessMessages;
    if Assigned(EmbeddedWB.document) then
      Lines.Add(EmbeddedWB.OleObject.Document.documentElement.innerHTML);
    fFileName := EmbeddedWB.LocationName;
    UpdateInfo;
    ScrollToTop;
    SelStart := Perform(EM_LINEINDEX, 1, 1);
  end
  else
    MessageDlg('You must assign a TEmbeddedWB before using this feature.', mtError, [MbOk], 0);
end;

procedure TRichEditWB.LoadTextFromBrowser;
begin
  if Assigned(FEmbeddedWB) then
  begin
    Clear;
    PlainText := False;
    while EmbeddedWB.ReadyState <> READYSTATE_COMPLETE do
      Forms.Application.ProcessMessages;
    if Assigned(EmbeddedWB.document) then
      Lines.Add(EmbeddedWB.OleObject.Document.documentElement.innerText);
    fFileName := EmbeddedWB.LocationName;
    UpdateInfo;
    ScrollToTop;
    SelStart := Perform(EM_LINEINDEX, 1, 1);
  end
  else
    MessageDlg('You should Assign A web Browser before using this feature!', mtError, [MbOk], 0);
end;

procedure TRichEditWB.LoadStringsFromBrowser;
begin
  if Assigned(FEmbeddedWB) then
  begin
    Clear;
    PlainText := False;
    while EmbeddedWB.ReadyState <> READYSTATE_COMPLETE do
      Forms.Application.ProcessMessages;
    EmbeddedWB.SaveToStrings(Lines);
    fFileName := EmbeddedWB.LocationName;
    UpdateInfo;
    ScrollToTop;
    SelStart := Perform(EM_LINEINDEX, 1, 1);
  end
  else
    MessageDlg('You should Assign A web Browser before using this feature!', mtError, [MbOk], 0);
end;

procedure TRichEditWB.LoadAsCopyFromBrowser;
begin
  if Assigned(FEmbeddedWB) then
  begin
    Clear;
    PlainText := False;
    while EmbeddedWB.ReadyState <> READYSTATE_COMPLETE do
      Forms.Application.ProcessMessages;
    EmbeddedWB.SelectAll;
    EmbeddedWB.Copy;
    PasteFromClipboard;
    fFileName := EmbeddedWB.LocationName;
    UpdateInfo;
    ScrollToTop;
    SelStart := Perform(EM_LINEINDEX, 1, 1);
  end
  else
    MessageDlg('You should Assign A web Browser before using this feature!', mtError, [MbOk], 0);
end;

procedure TRichEditWB.ScrollToTop;
begin
  SendMessage(Handle, EM_LINESCROLL, 0, -Lines.Count);
  SetFocus;
end;

procedure TRichEditWB.ScrollToBottom;
begin
  SendMessage(Handle, EM_SCROLL, 0, Lines.Count);
  SetFocus;
end;

procedure TRichEditWB.DoHTMLrc(Sender: TObject);
var
  ms: TMemoryStream;
begin
  HTMLSyn := THighlightHTML.Create;
  try
    HTMLSyn.SetText(Text);
    ms := TMemoryStream.Create;
    try
      HTMLSyn.ConvertToRTFStream(ms);
      PlainText := False;
      ms.Position := 0;
      Lines.LoadFromStream(ms);
      PlainText := True;
    finally
      ms.Free;
    end;
  finally
    HTMLSyn.Free;
  end;
end;

procedure TRichEditWB.DoXMLrc(Sender: TObject);
var
  ms: TMemoryStream;
begin
  XMLSyn := THighlightXML.Create;
  try
    XMLSyn.SetText(Text);
    ms := TMemoryStream.Create;
    try
      XMLSyn.ConvertToRTFStream(ms);
      PlainText := False;
      ms.Position := 0;
      Lines.LoadFromStream(ms);
      PlainText := True;
    finally
      ms.Free;
    end;
  finally
    XMLSyn.Free;
  end;
end;

function TRichEditWB.AddLineNumbering: Integer;
var
  fmt: TParaFormat2;
begin
  FillChar(fmt, SizeOf(fmt), 0);
  fmt.cbSize := SizeOf(fmt);
  fmt.dwMask := PFM_NUMBERING or PFM_NUMBERINGSTART or PFM_NUMBERINGSTYLE or
    PFM_NUMBERINGTAB;
  fmt.wNumbering := 2;
  fmt.wNumberingStart := 1;
  fmt.wNumberingStyle := $200;
  fmt.wNumberingTab := 1440 div 4;
  Perform(EM_SETPARAFORMAT, 0, lParam(@fmt));
  Result := Lines.Count;
end;

function TRichEditWB.AddBullets: Integer;
var
  fmt: TParaFormat2;
begin
  SelectAll;
  FillChar(fmt, SizeOf(fmt), 0);
  with fmt do
  begin
    cbSize := SizeOf(fmt);
    dwMask := PFM_NUMBERING or PFM_NUMBERINGSTART or PFM_NUMBERINGSTYLE or
      PFM_NUMBERINGTAB;
    wNumbering := 1;
    wNumberingStart := 1;
    wNumberingStyle := $200;
    wNumberingTab := 1440 div 4;
  end;
  Perform(EM_SETPARAFORMAT, 0, lParam(@fmt));
  selStart := 0;
  Result := Lines.Count;
end;

function TRichEditWB.AddRomanNumbering: Integer;
var
  fmt: TParaFormat2;
begin
  SelectAll;
  FillChar(fmt, SizeOf(fmt), 0);
  with fmt do
  begin
    cbSize := SizeOf(fmt);
    dwMask := PFM_NUMBERING or PFM_NUMBERINGSTART or PFM_NUMBERINGSTYLE or
      PFM_NUMBERINGTAB;
    wNumbering := 6;
    wNumberingStart := 1;
    wNumberingStyle := $200;
    wNumberingTab := 1440 div 4;
  end;
  Perform(EM_SETPARAFORMAT, 0, lParam(@fmt));
  selStart := 0;
  Result := Lines.Count;
end;

function TRichEditWB.AddCheckBox(cbCaption, cbName: string; reLeft, cbLeft,
  cbTop: Integer; Chk: Boolean): Integer;
var
  cb: TCheckBox;
begin
  if AcceptDragComponnents then
  begin
    Self.Left := RELeft;
    cb := TCheckBox.Create(Self);
    with cb do
    begin
      Name := cbName + IntToStr(CompCount);
      Caption := cbCaption;
      Left := cbLeft;
      Top := cbTop;
      Parent := Self;
      Checked := Chk;
      inc(CompCount);
      Result := CompCount;
    end;
  end
  else
    Result := 0;
end;

function TRichEditWB.AddEditBox(eText, eName: string; reLeft, eLeft, eTop: Integer): Integer;
var
  E: TEdit;
begin
  if AcceptDragComponnents then
  begin
    Self.Left := RELeft;
    E := TEdit.Create(Self);
    with E do
    begin
      Name := eName + IntToStr(CompCount);
      Text := eText;
      Left := eLeft;
      Top := eTop;
      Parent := Self;
      inc(CompCount);
      Result := CompCount;
    end;
  end
  else
    Result := 0;
end;

function TRichEditWB.AddRadioButton(rbCaption, rbName: string; reLeft, rbLeft,
  rbTop: Integer; Chk: boolean): Integer;
var
  RB: TRadioButton;
begin
  if AcceptDragComponnents then
  begin
    Self.Left := reLeft;
    RB := TRadioButton.Create(Self);
    with RB do
    begin
      Name := rbName + IntToStr(CompCount);
      Caption := rbCaption;
      Left := rbLeft;
      Top := rbTop;
      Parent := Self;
      Checked := Chk;
      inc(CompCount);
      Result := CompCount;
    end;
  end
  else
    Result := 0;
end;

function TRichEditWB.AddButton(bCaption, bName: string; reLeft, bLeft, bTop:
  Integer): Integer;
var
  B: TButton;
begin
  if AcceptDragComponnents then
  begin
    Self.Left := reLeft;
    B := TButton.Create(Self);
    with B do
    begin
      Name := bName + IntToStr(CompCount);
      Caption := bCaption;
      Left := bLeft;
      Top := bTop;
      Parent := Self;
      inc(CompCount);
      Result := CompCount;
    end;
  end
  else
    Result := 0;
end;

procedure TRichEditWB.SetModified(Value: Boolean);
begin
  inherited Modified;
  UpdateInfo;
 // if Assigned(FStatusbar) then
  begin
     //FStatusbar.Panels[0].Width := 2000;
   //  FStatusbar.Panels[0].Text  := Hint;
  end;
end;

procedure TRichEditWB.UpdateInfo;
var
  CharPos: TPoint;
  sMod: string;
begin
  CharPos.Y := SendMessage(Handle, EM_EXLINEFROMCHAR, 0, SelStart);
  CharPos.X := (SelStart - SendMessage(Handle, EM_LINEINDEX, CharPos.Y, 0));
  Inc(CharPos.Y);
  Inc(CharPos.X);
  CarretPosition := Format(sColRowInfo, [CharPos.Y, CharPos.X]);
  CheckCapslock;
  CheckInsertKey;
  CheckNumLock;
  if Modified then
    sMod := 'Modified'
  else
    sMod := '';
  Hint := 'File Name: ' + fFileName + '. | ' +
    #10 + #13 + 'Position: ' + Format(sColRowInfo, [CharPos.Y, CharPos.X]) + '. | ' +
    #10 + #13 + sMod + '. | ' +
    #10 + #13 + CapsLockKey + '. | ' +
    #10 + #13 + NumLockKey + '. | ' +
    #10 + #13 + InsertKey + '. | ' +
    #10 + #13 + 'Total Lines Count: ' + IntToStr(GetLineCount) + '. |';
  CursorPositionX := CharPos.X;
  CursorPositionY := CharPos.Y;
end;

procedure TRichEditWB.New;
begin
  CheckFileSave;
  SetFileName(sUntitled);
  Lines.Clear;
  inserted := False;
  Modified := False;
  SetModified(False);
end;

procedure TRichEditWB.PerformFileOpen(const AFileName: string);
var
  Ext: string;
begin
  inserted := False;
  Ext := ExtractFileExt(AFileName);
  PlainText := Ext = '.txt';
  Lines.LoadFromFile(AFileName);
  SetFileName(AFileName);
  SetFocus;
  Modified := False;
  SetModified(False);
end;

procedure TRichEditWB.Open;
var
  OD: TOpendialog;
begin
  CheckFileSave;
  OD := TOpendialog.Create(Self);
  try
    OD.Title := Forms.Application.Title + ' - ' + 'Open Dialog';
    if OD.Execute then
    begin
      ReadOnly := ofReadOnly in OD.Options;
      PerformFileOpen(OD.FileName);
      Setfilename(OD.FileName);
      UpdateInfo;
    end;
  finally
    OD.Free;
  end;
end;

procedure TRichEditWB.LoadFromFile(FileName: string);
begin
  Lines.LoadFromFile(FileName);
end;

procedure TRichEditWB.LoadFromStream(S: TStream);
begin
  Lines.LoadFromStream(S);
end;

procedure TRichEditWB.SaveToFile(FileName: string);
begin
  Lines.SaveToFile(FileName);
end;

procedure TRichEditWB.SaveToStream(S: TStream);
begin
  Lines.SaveToStream(S);
end;

procedure TRichEditWB.Save;
var
  i: Integer;
begin
  if FFileName = sUntitled then
  begin
    SaveAs;
    Exit;
  end
  else
  begin
    if FileExists(Trim(FFileName + '.html')) then
    begin
      i := MessageDlg(Format(sOverWrite, [Trim(FFileName + '.html')]), mtConfirmation,
        mbYesNoCancel, 0);
      if i = mrCancel then Exit;
      if i = mrNo then SaveAs;
      if i = mrYes then
      begin
        Lines.SaveToFile(Trim(FFileName + '.html'));
        Modified := False;
        SetModified(False);
      end;
    end
    else
      Lines.SaveToFile(Trim(FFileName + '.html'));
    Modified := False;
    SetModified(False);
  end;
end;

procedure TRichEditWB.SaveAs;
var
  sd: TSaveDialog;
begin
  sd := TSaveDialog.Create(Self);
  try
    SD.FileName := (Trim(FFileName + '.html'));
    if SD.Execute then
    begin
      if FileExists(Trim(SD.FileName + '.html')) then
        if MessageDlg(Format(sOverWrite, [(Trim(SD.FileName + '.html'))]), mtConfirmation, mbYesNoCancel, 0)
        <> idYes then Exit;
      Lines.SaveToFile(SD.FileName + '.html');
      SetFileName(SD.FileName);
      Modified := False;
      SetModified(False);
    end;
  finally
    sd.Free;
  end;
end;

procedure TRichEditWB.SetFileName(const FileName: string);
begin
  fFileName := FileName;
end;

procedure TRichEditWB.CheckFileSave;
var
  SaveResp: Integer;
begin
  if not Modified then
    Exit;
  SaveResp := MessageDlg(Format(sSaveChanges, [FFileName]),
    mtConfirmation, mbYesNoCancel, 0);
  case SaveResp of
    idYes: Save;
    idNo: {Nothing};
    idCancel: Abort;
  end;
end;

procedure TRichEditWB.WMDropFiles(var Msg: TWMDropFiles);
var
  CFileName: array[0..MAX_PATH] of Char;
begin
  if AcceptDragFiles then
  begin
    try
      if DragQueryFile(Msg.Drop, 0, CFileName, MAX_PATH) > 0 then
      begin
        CheckFileSave;
        PerformFileOpen(CFileName);
        Msg.Result := 0;
      end;
    finally
      DragFinish(Msg.Drop);
    end;
  end;
end;

initialization

finalization
  if FRichEditModule <> 0 then
    FreeLibrary(FRichEditModule);
end.
