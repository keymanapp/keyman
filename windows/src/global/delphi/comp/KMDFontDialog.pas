(*
  Name:             KMDFontDialog
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Aug 2006

  Modified Date:    23 Aug 2006
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2006 - mcdurdin - Initial version
*)
unit KMDFontDialog;

interface

uses
  Windows, Graphics, Controls, Messages, Classes, SysUtils, Dialogs, StdActns;

type
{ TFontDialog }

  TFontDialogOption = (fdAnsiOnly, fdTrueTypeOnly, fdEffects,
    fdFixedPitchOnly, fdForceFontExist, fdNoFaceSel, fdNoOEMFonts,
    fdNoSimulations, fdNoSizeSel, fdNoStyleSel,  fdNoVectorFonts,
    fdShowHelp, fdWysiwyg, fdLimitSize, fdScalableOnly, fdApplyButton);
  TFontDialogOptions = set of TFontDialogOption;

  TFontDialogDevice = (fdScreen, fdPrinter, fdBoth);

  TFDApplyEvent = procedure(Sender: TObject; Wnd: HWND) of object;

  TKMDFontDialog = class(TCommonDialog)
  private
    FRedirector: TWinControl;
    FFont: TFont;
    FDevice: TFontDialogDevice;
    FOptions: TFontDialogOptions;
    FOnApply: TFDApplyEvent;
    FMinFontSize: Integer;
    FMaxFontSize: Integer;
    FFontCharsetModified: Boolean;
    FFontColorModified: Boolean;
    procedure DoApply(Wnd: HWND);
    procedure SetFont(Value: TFont);
    procedure UpdateFromLogFont(const LogFont: TLogFont);
  protected
    procedure Apply(Wnd: HWND); dynamic;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(ParentWnd: HWND): Boolean; override;
  published
    property Font: TFont read FFont write SetFont;
    property Device: TFontDialogDevice read FDevice write FDevice default fdScreen;
    property MinFontSize: Integer read FMinFontSize write FMinFontSize default 0;
    property MaxFontSize: Integer read FMaxFontSize write FMaxFontSize default 0;
    property Options: TFontDialogOptions read FOptions write FOptions default [fdEffects];
    property OnApply: TFDApplyEvent read FOnApply write FOnApply;
  end;

{ TKMDFontEdit }

  TKMDFontEdit = class(TCommonDialogAction)
  private
    function GetDialog: TKMDFontDialog;
  protected
    function GetDialogClass: TCommonDialogClass; override;
  published
    property Caption;
    property Dialog: TKMDFontDialog read GetDialog;
    property Enabled;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property BeforeExecute;
    property OnAccept;
    property OnCancel;
    property OnHint;
    property OnUpdate;
  end;

procedure Register;

implementation

uses
  ActnList, CommDlg, Dlgs, Forms, Printers;

{ Private globals }

var
  CreationControl: TKMDFontDialog = nil;
  HelpMsg: Cardinal;
  FindMsg: Cardinal;
  WndProcPtrAtom: TAtom = 0;

{ Center the given window on the screen }

procedure CenterWindow(Wnd: HWnd);
var
  Rect: TRect;
  Monitor: TMonitor;
begin
  GetWindowRect(Wnd, Rect);
  if Application.MainForm <> nil then
  begin
    if Assigned(Screen.ActiveForm) then
      Monitor := Screen.ActiveForm.Monitor
    else
      Monitor := Application.MainForm.Monitor;
  end
  else
    Monitor := Screen.Monitors[0];
  SetWindowPos(Wnd, 0,
    Monitor.Left + ((Monitor.Width - Rect.Right + Rect.Left) div 2),
    Monitor.Top + ((Monitor.Height - Rect.Bottom + Rect.Top) div 3),
    0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
end;
  
{ Generic dialog hook. Centers the dialog on the screen in response to
  the WM_INITDIALOG message }

function DialogHook(Wnd: HWnd; Msg: UINT; WParam: WPARAM; LParam: LPARAM): UINT; stdcall;
begin
  Result := 0;
  if Msg = WM_INITDIALOG then
  begin
    CenterWindow(Wnd);
    CreationControl.FHandle := Wnd;
    CreationControl.FDefWndProc := Pointer(SetWindowLong(Wnd, GWL_WNDPROC,
      Longint(CreationControl.FObjectInstance)));
    CallWindowProc(CreationControl.FObjectInstance, Wnd, Msg, WParam, LParam);
    CreationControl := nil;
  end;
end;
  
{ TRedirectorWindow }
{ A redirector window is used to put the find/replace dialog into the
  ownership chain of a form, but intercept messages that CommDlg.dll sends
  exclusively to the find/replace dialog's owner.  TRedirectorWindow
  creates its hidden window handle as owned by the target form, and the
  find/replace dialog handle is created as owned by the redirector.  The
  redirector wndproc forwards all messages to the find/replace component.
}

type
  TRedirectorWindow = class(TWinControl)
  private
    FCommonDialog: TKMDFontDialog;
    FFormHandle: THandle;
    procedure CMRelease(var Message); message CM_Release;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc(var Message: TMessage); override;
  end;

procedure TRedirectorWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_VISIBLE or WS_POPUP;
    WndParent := FFormHandle;
  end;
end;

procedure TRedirectorWindow.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  with Message do
  begin
    if (Result = 0) and (Msg <> CM_RELEASE) and Assigned(FCommonDialog) then
      Result := Integer(FCommonDialog.MessageHook(Message));
    if (Result = 0) and (Msg = WM_SETFOCUS) and (FFormHandle <> 0) then
      Result := SendMessage(FFormHandle, WM_SETFOCUS, wParam, lParam);
  end;
end;

procedure TRedirectorWindow.CMRelease(var Message);
begin
  Free;
end;

{ TFontDialog }

const
  IDAPPLYBTN = $402;

var
  FontDialog: TKMDFontDialog;

function FontDialogHook(Wnd: HWnd; Msg: UINT; WParam: WPARAM; LParam: LPARAM): UINT; stdcall;
begin
  if (Msg = WM_COMMAND) and (LongRec(WParam).Lo = IDAPPLYBTN) and
    (LongRec(WParam).Hi = BN_CLICKED) then
  begin
    FontDialog.DoApply(Wnd);
    Result := 1;
  end else
    Result := DialogHook(Wnd, Msg, wParam, lParam);
end;

constructor TKMDFontDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont := TFont.Create;
  FOptions := [fdEffects];
end;

destructor TKMDFontDialog.Destroy;
begin
  FFont.Free;
  if Assigned(FRedirector) then
    TRedirectorWindow(FRedirector).FCommonDialog := nil;
  FreeAndNil(FRedirector);
  inherited Destroy;
end;

procedure TKMDFontDialog.WndProc(var Message: TMessage);
begin
  { Make sure we only take values from the color combobox and script combobox
    if they have been changed. }
  if (Message.Msg = WM_COMMAND) and (Message.WParamHi = CBN_SELENDOK) then
    if (Message.WParamLo = cmb4) then FFontColorModified := True
    else if (Message.WParamLo = cmb5) then FFontCharsetModified := True;
  inherited WndProc(Message);
end;

procedure TKMDFontDialog.Apply(Wnd: HWND);
begin
  if Assigned(FOnApply) then FOnApply(Self, Wnd);
end;

procedure TKMDFontDialog.DoApply(Wnd: HWND);
const
  IDCOLORCMB = $473;
var
  I: Integer;
  LogFont: TLogFont;
begin
  SendMessage(Wnd, WM_CHOOSEFONT_GETLOGFONT, 0, LongInt(@LogFont));
  UpdateFromLogFont(LogFont);
  I := SendDlgItemMessage(Wnd, IDCOLORCMB, CB_GETCURSEL, 0, 0);
  if I <> CB_ERR then
    Font.Color := SendDlgItemMessage(Wnd, IDCOLORCMB, CB_GETITEMDATA, I, 0);
  try
    Apply(Wnd);
  except
    Application.HandleException(Self);
  end;
end;

function TKMDFontDialog.Execute(ParentWnd: HWND): Boolean;
const
  FontOptions: array[TFontDialogOption] of DWORD = (
    CF_ANSIONLY, CF_TTONLY, CF_EFFECTS, CF_FIXEDPITCHONLY, CF_FORCEFONTEXIST,
    CF_NOFACESEL, CF_NOOEMFONTS, CF_NOSIMULATIONS, CF_NOSIZESEL,
    CF_NOSTYLESEL, CF_NOVECTORFONTS, CF_SHOWHELP,
    CF_WYSIWYG or CF_BOTH or CF_SCALABLEONLY, CF_LIMITSIZE,
    CF_SCALABLEONLY, CF_APPLY);  
  Devices: array[TFontDialogDevice] of DWORD = (
    CF_SCREENFONTS, CF_PRINTERFONTS, CF_BOTH);
var
  ChooseFontRec: TChooseFont;
  LogFont: TLogFont;
  Option: TFontDialogOption;
  SaveFontDialog: TKMDFontDialog;
  OriginalFaceName: string;
begin
  with ChooseFontRec do
  begin
    lStructSize := SizeOf(ChooseFontRec);
    hDC := 0;
    if FDevice <> fdScreen then hDC := Printer.Handle;
    lpLogFont := @LogFont;
    GetObject(Font.Handle, SizeOf(LogFont), @LogFont);
    OriginalFaceName := LogFont.lfFaceName;
    Flags := Devices[FDevice] or (CF_INITTOLOGFONTSTRUCT or CF_ENABLEHOOK);
    for Option := Low(Option) to High(Option) do
      if Option in FOptions then
        Flags := Flags or FontOptions[Option];
    if Assigned(FOnApply) then Flags := Flags or CF_APPLY;
    if Template <> nil then
    begin
      Flags := Flags or CF_ENABLETEMPLATE;
      lpTemplateName := Template;
      if FTemplateModule <> 0 then
        hInstance := FTemplateModule;
    end;
    rgbColors := Font.Color;
    lCustData := 0;
    lpfnHook := FontDialogHook;
    nSizeMin := FMinFontSize;
    nSizeMax := FMaxFontSize;
    if nSizeMin > nSizeMax then Flags := Flags and (not CF_LIMITSIZE);
    if Application.ModalPopupMode <> pmNone then
    begin
      FRedirector := TRedirectorWindow.Create(nil);
      with TRedirectorWindow(FRedirector) do
      begin
        FCommonDialog := Self;
        FFormHandle := ParentWnd;
      end;
      hWndOwner := FRedirector.Handle;
    end
    else
      hWndOwner := Application.Handle;
    SaveFontDialog := FontDialog;
    FontDialog := Self;
    FFontColorModified := False;
    FFontCharsetModified := False;
    Result := TaskModalDialog(@ChooseFont, ChooseFontRec);
    FreeAndNil(FRedirector);
    FontDialog := SaveFontDialog;
    if Result then
    begin
      if AnsiCompareText(OriginalFaceName, LogFont.lfFaceName) <> 0 then
        FFontCharsetModified := True;
      UpdateFromLogFont(LogFont);
      if FFontColorModified then Font.Color := rgbColors;
    end;
  end;
end;

procedure TKMDFontDialog.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TKMDFontDialog.UpdateFromLogFont(const LogFont: TLogFont);
var
  Style: TFontStyles;
begin
  with LogFont do
  begin
    Font.Name := LogFont.lfFaceName;
    Font.Height := LogFont.lfHeight;
    if FFontCharsetModified then
      Font.Charset := TFontCharset(LogFont.lfCharSet);
    Style := [];
    with LogFont do
    begin
      if lfWeight >= FW_SEMIBOLD then Include(Style, fsBold);
      if lfItalic <> 0 then Include(Style, fsItalic);
      if lfUnderline <> 0 then Include(Style, fsUnderline);
      if lfStrikeOut <> 0 then Include(Style, fsStrikeOut);
    end;
    Font.Style := Style;
  end;
end;

{ TFontEdit }

function TKMDFontEdit.GetDialog: TKMDFontDialog;
begin
  Result := TKMDFontDialog(FDialog);
end;

function TKMDFontEdit.GetDialogClass: TCommonDialogClass;
begin
  Result := TKMDFontDialog;
end;

procedure Register;
begin
  RegisterActions('KeymanEdit', [TKMDFontEdit], nil);
end;

end.
