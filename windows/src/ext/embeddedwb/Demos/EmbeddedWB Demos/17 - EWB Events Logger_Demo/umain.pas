unit umain;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, OleCtrls, SHDocVw_EWB, EwbCore, ActiveX, EmbeddedWB,
  IEAddress, ComObj, URLMon, EwbAcc, CheckLst, ComCtrls, Buttons, ToolWin,
  ImgList;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    Panel1: TPanel;
    lblFocus: TLabel;
    IEAddress1: TIEAddress;
    btnGo: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    ListBox1: TListBox;
    TabSheet2: TTabSheet;
    CheckListBox1: TCheckListBox;
    Panel2: TPanel;
    chkFilterCheckAll: TCheckBox;
    Panel3: TPanel;
    SpeedButton1: TSpeedButton;
    StatusBar1: TStatusBar;
    btnSaveFilters: TSpeedButton;
    btnLoadFilters: TSpeedButton;
    Splitter1: TSplitter;
    TabSheet3: TTabSheet;
    Button1: TButton;
    Panel4: TPanel;
    EWB: TEmbeddedWB;
    ProgressBar1: TProgressBar;
    Button2: TButton;
    ImageListToolBar: TImageList;
    ToolBar1: TToolBar;
    ToolbtnBack: TToolButton;
    ToolBtnForward: TToolButton;
    ToolBtnStop: TToolButton;
    ToolButton10: TToolButton;
    ToolBtnRefresh: TToolButton;
    ToolBtnHome: TToolButton;
    ToolButton11: TToolButton;
    ToolBtnSearch: TToolButton;
    Button4: TButton;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EWBAllowFocusChange(Sender: TObject;
      var Value: LongBool);
    procedure EWBAuthenticate(Sender: TCustomEmbeddedWB;
      var hwnd: HWND; var szUserName, szPassWord: WideString;
      var Rezult: HRESULT);
    procedure EWBBeforeNavigate2(ASender: TObject;
      const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
      Headers: OleVariant; var Cancel: WordBool);
    procedure EWBClientToHostWindow(ASender: TObject; var CX,
      CY: Integer);
    procedure EWBCloseQuery(Sender: TObject;
      var CanClose: Boolean);
    procedure EWBCommandExec(Sender: TObject; CmdGroup: PGUID;
      nCmdID, nCmdexecopt: Cardinal; const vaIn: OleVariant;
      var vaOut: OleVariant; var Rezult: HRESULT);
    procedure EWBCommandStateChange(ASender: TObject;
      Command: Integer; Enable: WordBool);
    procedure EWBDocumentComplete(ASender: TObject;
      const pDisp: IDispatch; var URL: OleVariant);
    procedure EWBDownloadBegin(Sender: TObject);
    procedure EWBDownloadComplete(Sender: TObject);
    procedure EWBDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure EWBDragEnter(Sender: TObject;
      const dataObj: IDataObject; grfKeyState: Integer; pt: TPoint;
      var dwEffect: Integer; var Rezult: HRESULT);
    procedure EWBDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure EWBDragLeave(Sender: TObject);
    procedure EWBDropEvent(Sender: TObject;
      const dataObj: IDataObject; grfKeyState: Integer; pt: TPoint;
      var dwEffect: Integer; var Rezult: HRESULT);
    procedure EWBDragOver2(Sender: TObject; grfKeyState: Integer;
      pt: TPoint; var dwEffect: Integer; var Rezult: HRESULT);
    procedure EWBEnableModeless(Sender: TCustomEmbeddedWB;
      const fEnable: LongBool);
    procedure EWBEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure EWBEnter(Sender: TObject);
    procedure EWBEvaluateNewWindow(Sender: TCustomEmbeddedWB;
      pszUrl, pszName, pszUrlContext, pszFeatures: PWideChar;
      fReplace: LongBool; dwFlags, dwUserActionTime: Cardinal;
      var Rezult: HRESULT);
    procedure EWBExit(Sender: TObject);
    procedure EWBFileDownload(Sender: TCustomEmbeddedWB;
      pmk: IMoniker; pbc: IBindCtx; dwBindVerb, grfBINDF: Cardinal;
      pBindInfo: PBindInfo; pszHeaders, pszRedir: PWideChar;
      uiCP: Cardinal; var Rezult: HRESULT);
    procedure EWBFilterDataObject(Sender: TCustomEmbeddedWB;
      const pDO: IDataObject; var ppDORet: IDataObject);
    procedure EWBFilterPopupMenu(Sender: TObject; ID: Cardinal; Menu: HMENU;
      const Context: IDispatch);
    procedure EWBGetDropTarget(Sender: TCustomEmbeddedWB; var DropTarget: IDropTarget);
    procedure EWBGetExternal(Sender: TCustomEmbeddedWB;
      var ppDispatch: IDispatch);
    procedure EWBGetHostInfo(Sender: TCustomEmbeddedWB;
      var pInfo: TDOCHOSTUIINFO);
    procedure EWBGetIDsOfNames(Sender: TObject; const IID: TGUID;
      Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer;
      var Rezult: HRESULT);
    procedure EWBGetTypeInfo(Sender: TObject; Index,
      LocaleID: Integer; var TypeInfo: ITypeInfo; var Rezult: HRESULT);
    procedure EWBGetTypeInfoCount(Sender: TObject;
      var Count: Integer; var Rezult: HRESULT);
    procedure EWBHideUI(Sender: TObject; var Rezult: HRESULT);
    procedure EWBInvoke(Sender: TObject; DispID: Integer;
      const IID: TGUID; LocaleID: Integer; Flags: Word;
      var Params: tagDISPPARAMS; VarResult, ExcepInfo, ArgErr: Pointer;
      var Rezult: HRESULT);
    procedure EWBMaskedCtrlChar(Sender: TCustomEmbeddedWB;
      MaskedChar: Char);
    procedure EWBMessage(Sender: TObject; var Msg: TMessage; var Handled: Boolean);
    procedure EWBMove(Sender: TCustomEmbeddedWB; cx, cy: Integer);
    procedure EWBMoveBy(Sender: TCustomEmbeddedWB; cx,
      cy: Integer);
    procedure EWBNavigateComplete2(ASender: TObject;
      const pDisp: IDispatch; var URL: OleVariant);
    procedure EWBNavigateError(ASender: TObject;
      const pDisp: IDispatch; var URL, Frame, StatusCode: OleVariant;
      var Cancel: WordBool);
    procedure EWBNewWindow2(ASender: TObject;
      var ppDisp: IDispatch; var Cancel: WordBool);
    procedure EWBNewWindow3(ASender: TObject;
      var ppDisp: IDispatch; var Cancel: WordBool; dwFlags: Cardinal;
      const bstrUrlContext, bstrUrl: WideString);
    procedure EWBOnDocWindowActivate(Sender: TCustomEmbeddedWB;
      const fActivate: LongBool);
    procedure EWBOnFrameWindowActivate(Sender: TCustomEmbeddedWB;
      const fActivate: LongBool);
    procedure EWBPopulateNSTable(Sender: TObject);
    procedure EWBPrintTemplateInstantiation(ASender: TObject;
      const pDisp: IDispatch);
    procedure EWBPrintTemplateTeardown(ASender: TObject;
      const pDisp: IDispatch);
    procedure EWBPrivacyImpactedStateChange(ASender: TObject;
      bImpacted: WordBool);
    procedure EWBProgressChange(ASender: TObject; Progress,
      ProgressMax: Integer);
    procedure EWBQuit(Sender: TObject);
    procedure EWBRefresh(Sender: TCustomEmbeddedWB; CmdID: Integer;
      var Cancel: Boolean);
    procedure EWBResize(Sender: TCustomEmbeddedWB; cx,
      cy: Integer);
    procedure EWBResizeBorder(Sender: TCustomEmbeddedWB;
      const prcBorder: PRect; const pUIWindow: IOleInPlaceUIWindow;
      const fRameWindow: LongBool);
    procedure EWBResizeBy(Sender: TCustomEmbeddedWB; cx,
      cy: Integer);
    procedure EWBScriptError(Sender: TObject; ErrorLine,
      ErrorCharacter, ErrorCode, ErrorMessage, ErrorUrl: string;
      var ScriptErrorAction: TScriptErrorAction);
    procedure EWBSetSecureLockIcon(ASender: TObject;
      SecureLockIcon: Integer);
    function EWBShowHelpRequest(Sender: TObject; HWND: Cardinal;
      pszHelpFile: PWideChar; uCommand, dwData: Integer; ptMouse: TPoint;
      var pDispatchObjectHit: IDispatch): HRESULT;
    function EWBShowMessage(Sender: TObject; HWND: Cardinal; lpstrText,
      lpstrCaption: PWideChar; dwType: Integer; lpstrHelpFile: PWideChar;
      dwHelpContext: Integer; var plResult: Integer): HRESULT;
    procedure EWBShowUI(Sender: TCustomEmbeddedWB;
      const dwID: Cardinal; const pActiveObject: IOleInPlaceActiveObject;
      const pCommandTarget: IOleCommandTarget;
      const pFrame: IOleInPlaceFrame; const pDoc: IOleInPlaceUIWindow;
      var Rezult: HRESULT);
    procedure EWBStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure EWBStatusTextChange(ASender: TObject;
      const Text: WideString);
    procedure EWBTitleChange(ASender: TObject;
      const Text: WideString);
    procedure EWBTranslateAccelerator(Sender: TCustomEmbeddedWB;
      const lpMsg: PMsg; const pguidCmdGroup: PGUID;
      const nCmdID: Cardinal; var Done: Boolean);
    procedure EWBTranslateUrl(Sender: TCustomEmbeddedWB;
      const pchURLIn: PWideChar; var ppchURLOut: WideString);
    procedure EWBUnload(Sender: TObject);
    procedure EWBUpdatePageStatus(ASender: TObject;
      const pDisp: IDispatch; var nPage, fDone: OleVariant);
    procedure EWBUpdateUI(Sender: TObject; var Rezult: HRESULT);
    procedure EWBVisible(ASender: TObject; Visible: WordBool);
    procedure EWBWindowClosing(ASender: TObject;
      IsChildWindow: WordBool; var Cancel: WordBool);
    procedure EWBWindowSetHeight(ASender: TObject;
      Height: Integer);
    procedure EWBWindowSetLeft(ASender: TObject; Left: Integer);
    procedure EWBWindowSetResizable(ASender: TObject;
      Resizable: WordBool);
    procedure EWBWindowSetTop(ASender: TObject; Top: Integer);
    procedure EWBWindowSetWidth(ASender: TObject; Width: Integer);
    function EWBZoomPercentChange(Sender: TCustomEmbeddedWB;
      const ulZoomPercent: Cardinal): HRESULT;
    procedure chkFilterCheckAllClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure btnSaveFiltersClick(Sender: TObject);
    procedure btnLoadFiltersClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ToolBtnRefreshClick(Sender: TObject);
    procedure ToolbtnBackClick(Sender: TObject);
    procedure ToolBtnForwardClick(Sender: TObject);
    procedure ToolBtnStopClick(Sender: TObject);
    procedure ToolBtnHomeClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure EWBBusyWait(Sender: TEmbeddedWB; AStartTime: Cardinal;
      var TimeOut: Cardinal; var Cancel: Boolean);
    procedure EWBShowDialog(Sender: TObject; h: Cardinal; StyleEx: Integer;
      OldCaption: string; var NewCaption: WideString; var Cancel: Boolean);
    procedure EWBWindowStateChanged(Sender: TObject; dwWindowStateFlags,
      dwValidFlagsMask: Cardinal);
    procedure EWBSetPhishingFilterStatus(Sender: TObject;
      PhishingFilterStatus: Integer);
    procedure EWBMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EWBMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EWBKeyDown(Sender: TObject; var Key: Word; ScanCode: Word;
      Shift: TShiftState);
    procedure EWBKeyUp(Sender: TObject; var Key: Word; ScanCode: Word;
      Shift: TShiftState);
    procedure EWBQueryService(Sender: TObject; const rsid, iid: TGUID;
      var Obj: IUnknown);
    procedure EWBShowContextMenu(Sender: TCustomEmbeddedWB;
      const dwID: Cardinal; const ppt: PPoint;
      const CommandTarget: IUnknown; const Context: IDispatch;
      var Result: HRESULT);
    procedure EWBClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ToolBtnSearchClick(Sender: TObject);
  private
    { Private declarations }
    iBeforeNavigate2: Integer;
    iOnDownloadBegin: Integer;
    iOnDownloadComplete: Integer;
    iOnNavigateComplete2: Integer;
    procedure SelectFilters(CheckAll: Boolean);
    procedure LogEvent(Sender: TObject; const s: string; Param1: string = ''; Param2: string = ''; Param3: string = ''; ForceLog: Boolean = False);
    function IsEventFilterChecked(const s: string): Boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  IniFiles, Clipbrd, EWBTools;

{$R *.dfm}


procedure TForm1.EWBAllowFocusChange(Sender: TObject;
  var Value: LongBool);
begin
 // Queries for permission to grab the focus when loading the page or when a
 // script attempts to focus an element.
  LogEvent(Sender, 'OnAllowFocusChange');
end;

procedure TForm1.EWBAuthenticate(Sender: TCustomEmbeddedWB;
  var hwnd: HWND; var szUserName, szPassWord: WideString;
  var Rezult: HRESULT);
begin
  LogEvent(Sender, 'OnAuthenticate');
end;

procedure TForm1.EWBBeforeNavigate2(ASender: TObject;
  const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant; var Cancel: WordBool);
begin
  Inc(iBeforeNavigate2);
  LogEvent(ASender, 'OnBeforeNavigate2', ' (' + IntToStr(iBeforeNavigate2) + ')');
end;

procedure TForm1.EWBClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   LogEvent(Sender, 'OnClick');
end;

procedure TForm1.EWBClientToHostWindow(ASender: TObject; var CX,
  CY: Integer);
begin
  LogEvent(ASender, 'OnClientToHostWindow');
end;

procedure TForm1.EWBCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  LogEvent(Sender, 'OnCloseQuery');
end;

procedure TForm1.EWBCommandExec(Sender: TObject; CmdGroup: PGUID;
  nCmdID, nCmdexecopt: Cardinal; const vaIn: OleVariant;
  var vaOut: OleVariant; var Rezult: HRESULT);
begin
  LogEvent(Sender, 'OnCommandExec');
end;

function BoolEnableToStr(B: Boolean): string;
const
  cSimpleBoolStrs: array [Boolean] of String = ('Enable = True', 'Enable = False');
begin
  Result := cSimpleBoolStrs[B];
end;

procedure TForm1.EWBCommandStateChange(ASender: TObject;
  Command: Integer; Enable: WordBool);
const
  CSC_UPDATECOMMANDS = -1;
begin
  case Command of
    CSC_NAVIGATEBACK:
      begin
        ToolbtnBack.Enabled := Enable;
        LogEvent(ASender, 'OnCommandStateChange', 'CSC_NAVIGATEBACK', BoolEnableToStr(Enable));
      end;
    CSC_NAVIGATEFORWARD:
      begin
        ToolbtnForward.Enabled := Enable;
        LogEvent(ASender, 'OnCommandStateChange', 'CSC_NAVIGATEFORWARD', BoolEnableToStr(Enable));
      end;
    CSC_UPDATECOMMANDS:
      begin
        ToolbtnStop.Enabled := Enable;
        LogEvent(ASender, 'OnCommandStateChange', 'CSC_UPDATECOMMANDS', BoolEnableToStr(Enable));
      end;
  end;
end;

procedure TForm1.EWBDocumentComplete(ASender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
var
  CurWebrowser: IWebBrowser;
  TopWebBrowser: IWebBrowser;
  Document: OLEvariant;
  WindowName: string;
begin
  CurWebrowser := pDisp as IWebBrowser;
  TopWebBrowser := (ASender as TEmbeddedWB).DefaultInterface;
  if CurWebrowser = TopWebBrowser then
    LogEvent(ASender, 'OnDocumentComplete', 'All frames loaded.')
  else
  begin
    Document := CurWebrowser.Document;
    WindowName := Document.ParentWindow.Name;
    LogEvent(ASender, 'OnDocumentComplete', 'Frame ' + WindowName);
  end;
end;

procedure TForm1.EWBDownloadBegin(Sender: TObject);
begin
  Inc(iOnDownloadBegin);
  LogEvent(Sender, 'OnDownloadBegin', ' (' + IntToStr(iOnDownloadBegin) + ')');
end;

procedure TForm1.EWBDownloadComplete(Sender: TObject);
begin
  Inc(iOnDownloadComplete);
  LogEvent(Sender, 'OnDownloadComplete', ' (' + IntToStr(iOnDownloadComplete) + ')');
end;

procedure TForm1.EWBDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  LogEvent(Sender, 'OnDragDrop');
end;

procedure TForm1.EWBDragEnter(Sender: TObject;
  const dataObj: IDataObject; grfKeyState: Integer; pt: TPoint;
  var dwEffect: Integer; var Rezult: HRESULT);
begin
  LogEvent(Sender, 'OnDragEnter');
end;

procedure TForm1.EWBDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  LogEvent(Sender, 'OnDragOver');
end;

procedure TForm1.EWBDragLeave(Sender: TObject);
begin
  LogEvent(Sender, 'OnDragLeave');
end;

procedure TForm1.EWBDropEvent(Sender: TObject;
  const dataObj: IDataObject; grfKeyState: Integer; pt: TPoint;
  var dwEffect: Integer; var Rezult: HRESULT);
begin
  LogEvent(Sender, 'OnDropEvent');
end;

procedure TForm1.EWBDragOver2(Sender: TObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer;
  var Rezult: HRESULT);
begin
  LogEvent(Sender, 'OnDragOver2');
end;

procedure TForm1.EWBEnableModeless(Sender: TCustomEmbeddedWB;
  const fEnable: LongBool);
begin
  LogEvent(Sender, 'OnEnableModeless');
end;

procedure TForm1.EWBEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
  LogEvent(Sender, 'OnEndDrag');
end;

procedure TForm1.EWBEnter(Sender: TObject);
begin
  LogEvent(Sender, 'OnEnter');
end;

procedure TForm1.EWBEvaluateNewWindow(Sender: TCustomEmbeddedWB;
  pszUrl, pszName, pszUrlContext, pszFeatures: PWideChar;
  fReplace: LongBool; dwFlags, dwUserActionTime: Cardinal;
  var Rezult: HRESULT);
begin
  LogEvent(Sender, 'OnEvaluateNewWindow');
end;

procedure TForm1.EWBExit(Sender: TObject);
begin
  LogEvent(Sender, 'OnExit');
end;

procedure TForm1.EWBFileDownload(Sender: TCustomEmbeddedWB;
  pmk: IMoniker; pbc: IBindCtx; dwBindVerb, grfBINDF: Cardinal;
  pBindInfo: PBindInfo; pszHeaders, pszRedir: PWideChar; uiCP: Cardinal;
  var Rezult: HRESULT);
begin
  LogEvent(Sender, 'OnFileDownload');
end;

procedure TForm1.EWBFilterDataObject(Sender: TCustomEmbeddedWB;
  const pDO: IDataObject; var ppDORet: IDataObject);
begin
  LogEvent(Sender, 'OnFilterDataObject');
end;

procedure TForm1.EWBFilterPopupMenu(Sender: TObject; ID: Cardinal; Menu: HMENU;
  const Context: IDispatch);
begin
  LogEvent(Sender, 'OnFilterPopupMenu');
end;

procedure TForm1.EWBGetDropTarget(Sender: TCustomEmbeddedWB; var DropTarget: IDropTarget);
begin
  LogEvent(Sender, 'OnGetDropTarget');
end;

procedure TForm1.EWBGetExternal(Sender: TCustomEmbeddedWB;
  var ppDispatch: IDispatch);
begin
  LogEvent(Sender, 'OnGetExternal');
end;

procedure TForm1.EWBGetHostInfo(Sender: TCustomEmbeddedWB;
  var pInfo: TDOCHOSTUIINFO);
begin
  LogEvent(Sender, 'OnGetHostInfo');
end;

procedure TForm1.EWBGetIDsOfNames(Sender: TObject;
  const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer;
  DispIDs: Pointer; var Rezult: HRESULT);
begin
  LogEvent(Sender, 'OnGetIDsOfNames');
end;

procedure TForm1.EWBGetTypeInfo(Sender: TObject; Index,
  LocaleID: Integer; var TypeInfo: ITypeInfo; var Rezult: HRESULT);
begin
  LogEvent(Sender, 'OnGetTypeInfo');
end;

procedure TForm1.EWBGetTypeInfoCount(Sender: TObject;
  var Count: Integer; var Rezult: HRESULT);
begin
  LogEvent(Sender, 'OnGetTypeInfoCount');
end;

procedure TForm1.EWBHideUI(Sender: TObject; var Rezult: HRESULT);
begin
  LogEvent(Sender, 'OnHideUI');
end;

procedure TForm1.EWBInvoke(Sender: TObject; DispID: Integer;
  const IID: TGUID; LocaleID: Integer; Flags: Word;
  var Params: tagDISPPARAMS; VarResult, ExcepInfo, ArgErr: Pointer;
  var Rezult: HRESULT);
begin
  LogEvent(Sender, 'OnInvoke');
end;

procedure TForm1.EWBMaskedCtrlChar(Sender: TCustomEmbeddedWB;
  MaskedChar: Char);
begin
  LogEvent(Sender, 'OnMaskedCtrlChar');
end;

procedure TForm1.EWBMessage(Sender: TObject; var Msg: TMessage;
  var Handled: Boolean);
begin
  LogEvent(Sender, 'OnMessage', IntToStr(Msg.Msg), IntToHex(Msg.LParam, 4), IntToHex(Msg.WParam, 4))
end;

procedure TForm1.EWBMove(Sender: TCustomEmbeddedWB; cx,
  cy: Integer);
begin
  LogEvent(Sender, 'OnMove');
end;

procedure TForm1.EWBMoveBy(Sender: TCustomEmbeddedWB; cx,
  cy: Integer);
begin
  LogEvent(Sender, 'OnMoveBy');
end;

procedure TForm1.EWBNavigateComplete2(ASender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
begin
  Inc(iOnNavigateComplete2);
  LogEvent(ASender, 'OnNavigateComplete2', ' (' + IntToStr(iOnNavigateComplete2) + ')');
end;

procedure TForm1.EWBNavigateError(ASender: TObject;
  const pDisp: IDispatch; var URL, Frame, StatusCode: OleVariant;
  var Cancel: WordBool);
begin
  LogEvent(ASender, 'OnNavigateError');
end;

procedure TForm1.EWBNewWindow2(ASender: TObject;
  var ppDisp: IDispatch; var Cancel: WordBool);
begin
  LogEvent(ASender, 'OnNewWindow2');
end;

procedure TForm1.EWBNewWindow3(ASender: TObject;
  var ppDisp: IDispatch; var Cancel: WordBool; dwFlags: Cardinal;
  const bstrUrlContext, bstrUrl: WideString);
begin
  LogEvent(ASender, 'OnNewWindow3');
end;

procedure TForm1.EWBOnDocWindowActivate(Sender: TCustomEmbeddedWB;
  const fActivate: LongBool);
begin
  LogEvent(Sender, 'OnDocWindowActivate');
end;

procedure TForm1.EWBOnFrameWindowActivate(
  Sender: TCustomEmbeddedWB; const fActivate: LongBool);
begin
  LogEvent(Sender, 'OnFrameWindowActivate');
end;

procedure TForm1.EWBPopulateNSTable(Sender: TObject);
begin
  LogEvent(Sender, 'OnPopulateNSTable');
end;

procedure TForm1.EWBPrintTemplateInstantiation(ASender: TObject;
  const pDisp: IDispatch);
begin
  LogEvent(ASender, 'OnPrintTemplateInstantiation');
end;

procedure TForm1.EWBPrintTemplateTeardown(ASender: TObject;
  const pDisp: IDispatch);
begin
  LogEvent(ASender, 'OnPrintTemplateTeardown');
end;

procedure TForm1.EWBPrivacyImpactedStateChange(ASender: TObject;
  bImpacted: WordBool);
begin
  LogEvent(ASender, 'OnPrivacyImpactedStateChange');
end;

procedure TForm1.EWBProgressChange(ASender: TObject; Progress,
  ProgressMax: Integer);
begin
  if ProgressMax <> 0 then
    ProgressBar1.Position := Trunc((Progress / ProgressMax) * 100) + 1
  else
    ProgressBar1.Position := 0;

  LogEvent(ASender, 'OnProgressChange', IntToStr(Progress) + '/' + IntToStr(ProgressMax));
end;


procedure TForm1.EWBQuit(Sender: TObject);
begin
  LogEvent(Sender, 'OnQuit');
end;

procedure TForm1.EWBRefresh(Sender: TCustomEmbeddedWB;
  CmdID: Integer; var Cancel: Boolean);
begin
  LogEvent(Sender, 'OnRefresh');
end;

procedure TForm1.EWBResize(Sender: TCustomEmbeddedWB; cx,
  cy: Integer);
begin
  LogEvent(Sender, 'OnResize');
end;

procedure TForm1.EWBResizeBorder(Sender: TCustomEmbeddedWB;
  const prcBorder: PRect; const pUIWindow: IOleInPlaceUIWindow;
  const fRameWindow: LongBool);
begin
  LogEvent(Sender, 'OnResizeBorder');
end;

procedure TForm1.EWBResizeBy(Sender: TCustomEmbeddedWB; cx,
  cy: Integer);
begin
  LogEvent(Sender, 'OnResizeBy');
end;

procedure TForm1.EWBScriptError(Sender: TObject; ErrorLine,
  ErrorCharacter, ErrorCode, ErrorMessage, ErrorUrl: string;
  var ScriptErrorAction: TScriptErrorAction);
begin
  LogEvent(Sender, 'OnScriptError');
end;

procedure TForm1.EWBSetSecureLockIcon(ASender: TObject;
  SecureLockIcon: Integer);
begin
  LogEvent(ASender, 'OnSetSecureLockIcon');
end;

function TForm1.EWBShowHelpRequest(Sender: TObject; HWND: Cardinal;
  pszHelpFile: PWideChar; uCommand, dwData: Integer; ptMouse: TPoint;
  var pDispatchObjectHit: IDispatch): HRESULT;
begin
  Result := S_FALSE;
  LogEvent(Sender, 'OnShowHelpRequest');
end;

function TForm1.EWBShowMessage(Sender: TObject; HWND: Cardinal; lpstrText,
  lpstrCaption: PWideChar; dwType: Integer; lpstrHelpFile: PWideChar;
  dwHelpContext: Integer; var plResult: Integer): HRESULT;
begin
  Result := S_FALSE;
  LogEvent(Sender, 'OnShowMessage');
end;

procedure TForm1.EWBShowUI(Sender: TCustomEmbeddedWB;
  const dwID: Cardinal; const pActiveObject: IOleInPlaceActiveObject;
  const pCommandTarget: IOleCommandTarget; const pFrame: IOleInPlaceFrame;
  const pDoc: IOleInPlaceUIWindow; var Rezult: HRESULT);
begin
  LogEvent(Sender, 'OnShowUI');
end;

procedure TForm1.EWBStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  LogEvent(Sender, 'OnStartDrag');
end;

procedure TForm1.EWBStatusTextChange(ASender: TObject;
  const Text: WideString);
begin
  LogEvent(ASender, 'OnStatusTextChange');
  StatusBar1.Panels[0].Text := Text;
end;


procedure TForm1.EWBTitleChange(ASender: TObject;
  const Text: WideString);
begin
  LogEvent(ASender, 'OnTitleChange');
  Caption := Text;
end;

procedure TForm1.EWBTranslateAccelerator(Sender: TCustomEmbeddedWB;
  const lpMsg: PMsg; const pguidCmdGroup: PGUID; const nCmdID: Cardinal;
  var Done: Boolean);
var
  tempC: array[1..2] of Char;
  KeyState: TKeyboardState;
begin
  GetKeyboardState(KeyState);
  if ToAscii(lpMsg.wParam, MapVirtualKey(lpMsg.wParam, 0), keyState, @tempC[1], 0) > 0 then
    LogEvent(Sender, 'OnTranslateAccelerator', IntToStr(lpMsg.message), tempC[1])
  else
    LogEvent(Sender, 'OnTranslateAccelerator', IntToStr(lpMsg.message), IntToStr(lpMsg.wParam))
end;

procedure TForm1.EWBTranslateUrl(Sender: TCustomEmbeddedWB;
  const pchURLIn: PWideChar; var ppchURLOut: WideString);
begin
  LogEvent(Sender, 'OnTranslateUrl');
end;

procedure TForm1.EWBUnload(Sender: TObject);
begin
  LogEvent(Sender, 'OnUnload');
end;

procedure TForm1.EWBUpdatePageStatus(ASender: TObject;
  const pDisp: IDispatch; var nPage, fDone: OleVariant);
begin
  LogEvent(ASender, 'OnUpdatePageStatus');
end;

procedure TForm1.EWBUpdateUI(Sender: TObject; var Rezult: HRESULT);
begin
  LogEvent(Sender, 'OnUpdateUI');
end;

procedure TForm1.EWBVisible(ASender: TObject; Visible: WordBool);
begin
  LogEvent(ASender, 'OnVisible');
end;

procedure TForm1.EWBWindowClosing(ASender: TObject;
  IsChildWindow: WordBool; var Cancel: WordBool);
begin
  LogEvent(ASender, 'OnWindowClosing');
end;

procedure TForm1.EWBWindowSetHeight(ASender: TObject;
  Height: Integer);
begin
  LogEvent(ASender, 'OnWindowSetHeight');
end;

procedure TForm1.EWBWindowSetLeft(ASender: TObject; Left: Integer);
begin
  LogEvent(ASender, 'OnWindowSetLeft');
end;

procedure TForm1.EWBWindowSetResizable(ASender: TObject;
  Resizable: WordBool);
begin
  LogEvent(ASender, 'OnWindowSetResizable');
end;

procedure TForm1.EWBWindowSetTop(ASender: TObject; Top: Integer);
begin
  LogEvent(ASender, 'OnWindowSetTop');
end;

procedure TForm1.EWBShowDialog(Sender: TObject; h: Cardinal;
  StyleEx: Integer; OldCaption: string; var NewCaption: WideString;
  var Cancel: Boolean);
begin
  LogEvent(Sender, 'OnShowDialog');
end;

procedure TForm1.EWBWindowSetWidth(ASender: TObject;
  Width: Integer);
begin
  LogEvent(ASender, 'OnWindowSetWidth');
end;

function TForm1.EWBZoomPercentChange(Sender: TCustomEmbeddedWB;
  const ulZoomPercent: Cardinal): HRESULT;
begin
  Result := S_OK;
  LogEvent(Sender, 'OnZoomPercentChange');
end;

procedure TForm1.EWBBusyWait(Sender: TEmbeddedWB; AStartTime: Cardinal;
  var TimeOut: Cardinal; var Cancel: Boolean);
begin
  LogEvent(Sender, 'OnBusyWait', IntToStr(GetTickCount - AStartTime));
end;

// WindowStateChanged is available only in Microsoft Windows XP Service
//  Pack 2 (SP2) or later.

procedure TForm1.EWBWindowStateChanged(Sender: TObject; dwWindowStateFlags,
  dwValidFlagsMask: Cardinal);
begin
  LogEvent(Sender, 'OnWindowStateChanged');
end;

// IE7

procedure TForm1.EWBSetPhishingFilterStatus(Sender: TObject;
  PhishingFilterStatus: Integer);
begin
  LogEvent(Sender, 'OnSetPhishingFilterStatus');
end;

procedure TForm1.EWBQueryService(Sender: TObject; const rsid, iid: TGUID;
  var Obj: IUnknown);
begin
  LogEvent(Sender, 'OnQueryService');
end;

procedure TForm1.EWBShowContextMenu(Sender: TCustomEmbeddedWB;
  const dwID: Cardinal; const ppt: PPoint; const CommandTarget: IUnknown;
  const Context: IDispatch; var Result: HRESULT);
begin
  LogEvent(Sender, 'OnShowContextMenu');
end;

procedure TForm1.EWBMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  LogEvent(Sender, 'OnMouseDown');
end;

procedure TForm1.EWBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  LogEvent(Sender, 'OnMouseUp');
end;


procedure TForm1.EWBKeyDown(Sender: TObject; var Key: Word; ScanCode: Word;
  Shift: TShiftState);
begin
  LogEvent(Sender, 'OnKeyDown', Char(Key), IntToStr(ScanCode));
end;

procedure TForm1.EWBKeyUp(Sender: TObject; var Key: Word; ScanCode: Word;
  Shift: TShiftState);
begin
  LogEvent(Sender, 'OnKeyUp', Char(Key), IntToStr(ScanCode));
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  Listbox1.Clear;
  iBeforeNavigate2 := 0;
  iBeforeNavigate2 := 0;
  iOnDownloadBegin := 0;
  iOnDownloadComplete := 0;
  iOnNavigateComplete2 := 0;
end;

procedure TForm1.btnSaveFiltersClick(Sender: TObject);
var
  i: Integer;
  IniPath: string;
  ini: TIniFile;
begin
  IniPath := ExtractFilePath(Application.ExeName) + 'Filters.ini';
  ini := TIniFile.Create(IniPath);
  try
    for i := 0 to Checklistbox1.Items.Count - 1 do
      ini.WriteBool('items', Checklistbox1.Items[i], Checklistbox1.Checked[i]);
  finally
    ini.Free;
  end;
end;

procedure TForm1.btnLoadFiltersClick(Sender: TObject);
var
  i: Integer;
  IniPath: string;
  ini: TIniFile;
begin
  IniPath := ExtractFilePath(Application.ExeName) + 'Filters.ini';
  if FileExists(IniPath) then
  begin
    ini := TIniFile.Create(IniPath);
    try
      ini.ReadSection('items', Checklistbox1.Items);
      for i := 0 to Pred(Checklistbox1.Items.Count) do
        CheckListbox1.Checked[i] := ini.ReadBool('items', Checklistbox1.Items[i], False);
    finally
      ini.Free;
    end;
  end;
end;


procedure TForm1.SelectFilters(CheckAll: Boolean);
var
  i: integer;
begin
  for i := 0 to CheckListBox1.Items.Count - 1 do
  begin
    CheckListBox1.Checked[i] := CheckAll;
  end;
end;

function TForm1.IsEventFilterChecked(const s: string): Boolean;
var
  i: integer;
begin
  Result := False;
  if not Assigned(CheckListBox1) then Exit;
  i := CheckListBox1.Items.IndexOf(s);
  if i <> -1 then
  begin
    Result := CheckListBox1.Checked[i];
  end;
end;

procedure TForm1.LogEvent(Sender: TObject; const s: string; Param1: string = ''; Param2: string = ''; Param3: string = ''; ForceLog: Boolean = False);
begin
  if Application.Terminated then Exit;
  if Assigned(ListBox1) then
    if IsEventFilterChecked(s) or ForceLog then
    begin
      ListBox1.ItemIndex := ListBox1.Items.add(FormatDateTime('hh:mm:ss', now) + ' : ' +
        s + ' ' + Param1 + ' ' + Param2 + ' ' + Param3);
    end;
end;

procedure TForm1.chkFilterCheckAllClick(Sender: TObject);
begin
  SelectFilters(chkFilterCheckAll.Checked);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  Control: TWinControl;
begin
  Control := ActiveControl;
  if Control <> nil then
    lblFocus.Caption := 'Active Control: ' + ActiveControl.Name
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  btnLoadFiltersClick(nil);
  PageControl1.ActivePageIndex := 0;
end;

procedure TForm1.btnGoClick(Sender: TObject);
begin
  EWB.Go(IEAddress1.Text);
  LogEvent(EWB, '** WBTop complete***', '', '', '', True);
end;

function WaitForBrowserTimeOut(WB: TEmbeddedWB; const iTimeOut: Cardinal = 6000): Boolean;
var
  t1, t2: Cardinal;
begin
  t1 := GetTickCOunt;
  repeat
    Application.ProcessMessages;
    t2 := GetTickCOunt;
    Sleep(10);
  until (WB.ReadyState = READYSTATE_COMPLETE) or (Application.Terminated) or (t2 - t1 > iTimeOut);
  Result := WB.ReadyState = READYSTATE_COMPLETE;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  EWB.Visible := not EWB.Visible;
end;

procedure TForm1.ToolBtnRefreshClick(Sender: TObject);
begin
  EWB.Refresh2;
end;

procedure TForm1.ToolbtnBackClick(Sender: TObject);
begin
  EWB.GoBack;
end;

procedure TForm1.ToolBtnForwardClick(Sender: TObject);
begin
  EWB.GoForward;
end;

procedure TForm1.ToolBtnStopClick(Sender: TObject);
begin
  EWB.Stop;
end;

procedure TForm1.ToolBtnHomeClick(Sender: TObject);
begin
  EWB.GoHome;
end;

procedure TForm1.ToolBtnSearchClick(Sender: TObject);
begin
  EWB.ShowFindDialog;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  ClipBoard.SetTextbuf(PChar(Listbox1.Items.Text));
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  EWB.Go('http://www.microsoft.com/surface/');
  if WaitForBrowserTimeOut(EWB) then
    LogEvent(EWB, '** WBTop complete***', '', '', '', True)
  else
    LogEvent(EWB, '** WBTop complete Timout reached***', '', '', '', True)
end;






end.

