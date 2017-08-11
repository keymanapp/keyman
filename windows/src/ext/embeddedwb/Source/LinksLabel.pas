//***********************************************************
//                         TLinksLabel                      *
//                                                          *
//                     For Delphi 5 to XE                   *
//                     Freeware Component                   *
//                            by                            *
//                     Eran Bodankin (bsalsa)               *                                                            //                   per.lindsoe@larsen.dk                  *
//                                                          *
//  Documentation and updated versions:                     *
//                                                          *
//               http://www.bsalsa.com                      *
//***********************************************************
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
//$Id: LinksLabel.pas,v 1.2 2006/11/15 21:01:42 sergev Exp $

{$B-}

unit LinksLabel;

interface

{$I EWB.inc}

uses
  SysUtils, Windows, ExtCtrls, Dialogs, Controls, StdCtrls, Messages, Classes,
  Graphics, Menus, ImgList;

type
  TDirection = (diLeftToRight, diRightToLeft, diTopToBottom, diBottomToTop);
  TTextStyle = (tsNormal, ts3D_F, ts3D_B);
  TTextLayout = (tlTop, tlCenter, tlBottom);
  TLaunchSettings = (Http, MailTo, Application);
  TWindowState = (wsNormal, wsMinimized, wsMaximized);
  TOnLaunchEvent = procedure(var Success: Boolean; const ErrorText: string; var Result: Cardinal) of object;
  TTextEffects = (teNormal, teRunning, teFlickering);
//------------------------------------------------------------------------------
  TMail = class(TPersistent)
  private
    FAddress: string;
    FSubject: string;
    FBody: string;
    FWindowState: TWindowState;
  published
    property Address: string read FAddress write FAddress;
    property Subject: string read FSubject write FSubject;
    property Body: string read FBody write FBody;
    property WindowState: TWindowState read FWindowState write FWindowState default wsNormal;
  end;

  THttp = class(TPersistent)
  private
    FAddress: string;
    FWindowState: TWindowState;
    procedure SetAddress(const value: string);
  published
    property Address: string read FAddress write SetAddress;
    property WindowState: TWindowState read FWindowState write FWindowState default wsNormal;
  end;

  TApp = class(TPersistent)
  private
    FPath: string;
    FCommand: string;
    FDescription: string;
    FWindowState: TWindowState;
  published
    property Description: string read FDescription write FDescription;
    property Command: string read FCommand write FCommand;
    property Path: string read FPath write FPath;
    property WindowState: TWindowState read FWindowState write FWindowState default wsNormal;
  end;

  TLaunch = class(TPersistent)
  private
    FMail: TMail;
    FApp: TApp;
    FHttp: THttp;
    FLaunchSettings: TLaunchSettings;
    procedure SetLaunchOptions(Value: TLaunchSettings);
  published
    property AsHttp: THttp read FHttp write FHttp;
    property AsMail: TMail read FMail write FMail;
    property AsApplication: TApp read FApp write FApp;
    property Options: TLaunchSettings read FLaunchSettings write SetLaunchOptions default Http;
  end;
//------------------------------------------------------------------------------
type
  TLinksLabel = class;

  TTextFlick = class(TPersistent)
  private
    FEnableFlick: Boolean;
    FlickTimer: TTimer;
    FSpeed: Integer;
    procedure SetSpeed(const Value: integer);
  published
    property Enable: Boolean read FEnableFlick write FEnableFlick default false;
    property Speed: integer read FSpeed write SetSpeed default 500;
  end;

  TTextRun = class(TPersistent)
  private
    FSteps: integer;
    FCurCycle: Integer;
    FEnableRun: Boolean;
    FDirection: TDirection;
    FSpeed: integer;
    FRepeatNumber: Word;
    FContinuous: Boolean;
    FCurrentStep: Integer;
    procedure SetDirection(Value: TDirection);
    procedure SetSteps(Value: integer);
    procedure SetSpeed(Value: Integer);
    procedure SetEnableRun(Value: Boolean);
  published
    property Enable: Boolean read FEnableRun write SetEnableRun default False;
    property Direction: TDirection read FDirection write SetDirection default diLeftToRight;
    property RepeatNumber: Word read FRepeatNumber write FRepeatNumber default 0;
    property Continuous: Boolean read FContinuous write FContinuous default True;
    property Steps: Integer read FSteps write SetSteps default 100;
    property Speed: Integer read FSpeed write SetSpeed default 100;
  end;

  TLinksLabel = class(TCustomLabel)
  private
    FAutoSize: Boolean;
    FAlignment: TAlignment;
    FLayout: TTextLayout;
    FOnLaunch: TOnLaunchEvent;
    FImageIndex: integer;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    RunTimer: TTimer;
    FTextEffects: TTextEffects;
    FLaunch: TLaunch;
    FColorOnMouseClick: Boolean;
    FHighlightOnMouseOver: Boolean;
    FTextStyle: TTextStyle;
    FTextFlick: TTextFlick;
    FTextRun: TTextRun;
    FShift: integer;
    FLinkColor: TColor;
    FDownColor: TColor;
    FUpColor: TColor;
    F3d_F_Color: TColor;
    F3d_B_Color: TColor;
    procedure SetTextStyle(Value: TTextStyle);
    procedure OnRunTimer(Sender: TObject);
    procedure CanvasTextOut(Canvas: TCanvas; X, Y: Integer; Text: string);
    procedure SetImageIndex(const Value: integer);
    procedure SetImages(Value: TCustomImageList); virtual;
    function SetWinState(State: TWindowState): Cardinal;
    procedure CMTextChanged(var Message: TMessage); message CM_TextChanged;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure DoCopyAddress(Sender: TObject);
    procedure DoLaunch(Sender: TObject);
    procedure ImageListChange(Sender: TObject);
    procedure SetAlignment(Value: TAlignment);
    procedure SetLayout(Value: TTextLayout);

  protected
    function GetPopupMenu: TPopupMenu; override;
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave(Sender: TObject);
    procedure Loaded; override;
    procedure OnFlickTimer(Sedner: Tobject);
    procedure SetAutoSize(Value: Boolean); override;
    procedure SetLinkColor(Value: TColor);
    procedure SetShift(Value: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function GetProgramPathFromExt(const Ext: string): string;
  published
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property TextFlick: TTextFlick read FTextFlick write FTextFlick;
    property TextRun: TTextRun read FTextRun write FTextRun;
    property EnableColorOnClick: Boolean read FColorOnMouseClick write FColorOnMouseClick default True;
    property EnableHighlight: Boolean read FHighlightOnMouseOver write FHighlightOnMouseOver default True;
    property TextStyle: TTextStyle read FTextStyle write SetTextStyle default tsNormal;
    property ShiftShadow3D: integer read FShift write SetShift default 2;
    property Launch: TLaunch read FLaunch write FLaunch;
    property TextEffects: TTextEffects read FTextEffects write FTextEffects default teNormal;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property OnLaunch: TOnLaunchEvent read FOnLaunch write FOnLaunch;
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    property Layout: TTextLayout read FLayout write SetLayout default tlBottom;
    property LinkColor: TColor read FLinkColor write SetLinkColor default clBlue;
    property DownColor: TColor read FDownColor write FDownColor default clNavy;
    property UpColor: TColor read FUpColor write FUpColor default clPurple;
    property Color_3d_F: TColor read F3d_F_Color write F3d_F_Color default clNavy;
    property Color_3d_B: TColor read F3d_B_Color write F3d_B_Color default clHighLight;
    property Align;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Color nodefault;
    property Constraints;
    property Cursor default crHandPoint;
    property DragCursor;
    property DragKind;
    property DragMode;
{$IFDEF DELPHI9_UP}
    property EllipsisPosition;
{$ENDIF}
    property Enabled;
    property FocusControl;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont default False;
    property ParentShowHint default False;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint default True;
    property Transparent default False;
    property Visible;
    property WordWrap default True;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
{$IFDEF DELPHI9_UP}
    property OnMouseActivate;
{$ENDIF}
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IFDEF DELPHI6_UP}
    property OnMouseEnter;
    property OnMouseLeave;
{$ENDIF}
    property OnStartDock;
    property OnStartDrag;
  end;

implementation
uses
  Registry, ShellAPI, Clipbrd, CommCtrl;
//Acc---------------------------------------------------------------------------

function ErrorText(ErrorCode: Integer): string;
begin
  case ErrorCode of
    0: Result := 'The operating system is out of memory or resources.';
    ERROR_FILE_NOT_FOUND: Result := 'The specified file was not found.';
    ERROR_PATH_NOT_FOUND: Result := 'The specified path was not found.';
    ERROR_BAD_FORMAT: Result := 'The .exe file is invalid (non-Microsoft Win32® .exe or error in .exe image).';
    SE_ERR_ACCESSDENIED: Result := 'The operating system denied access to the specified file.';
    SE_ERR_ASSOCINCOMPLETE: Result := 'The file name association is incomplete or invalid. ';
    SE_ERR_DDEBUSY: Result := 'The Dynamic Data Exchange (DDE) transaction could not be completed because other DDE transactions were being processed.';
    SE_ERR_DDEFAIL: Result := 'The DDE transaction failed.';
    SE_ERR_DDETIMEOUT: Result := 'The DDE transaction could not be completed because the request timed out.';
    SE_ERR_DLLNOTFOUND: Result := 'The specified dynamic-link library (DLL) was not found.';
    SE_ERR_NOASSOC: Result := 'There is no application associated with the given file name extension. This error will also be returned if you attempt to print a file that is not printable.';
    SE_ERR_OOM: Result := 'There was not enough memory to complete the operation.';
    SE_ERR_SHARE: Result := 'A sharing violation occurred.';
    4: Result := 'Please assign a task to the label.';
  else
    if ErrorCode <> 0 then
      Result := 'Error: ';
  end;
end;
// Component--------------------------------------------------------------------

constructor TLinksLabel.Create(AOwner: TComponent);
begin
  inherited;
{$IFDEF DELPHI6_UP}
  OnMouseLeave := MouseLeave;
{$ENDIF}
  WordWrap := True;
  Font.Style := [fsUnderline];
  Cursor := crHandPoint;
  Caption := 'Home Page';
  ParentShowHint := False;
  ShowHint := True;
  Width := 150;
  Height := 100;
  FImageIndex := -1;
  FLayout := tlBottom;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FColorOnMouseClick := True;
  FHighlightOnMouseOver := True;
  FTextStyle := tsNormal;
  FTextEffects := teNormal;
  FTextFlick := TTextFlick.Create;
  FTextFlick.FSpeed := 500;
  FLinkColor := clBlue;
  Font.Color := FLinkColor;
  FUpColor := clPurple;
  FDownColor := clNavy;
  F3d_F_Color := clNavy;
  F3d_B_Color := clHighLight;
  FShift := 2;
  FTextRun := TTextRun.Create;
  with FTextRun do
  begin
    FSpeed := 100;
    FContinuous := True;
    FDirection := diLeftToRight;
    FSteps := 100;
    FRepeatNumber := 0;
  end;

  FLaunch := TLaunch.Create;
  with FLaunch do
  begin
    FMail := TMail.Create;
    FMail.FWindowState := wsNormal;
    FApp := TApp.Create;
    FApp.FWindowState := wsNormal;
    FHttp := THttp.Create;
    FHttp.FWindowState := wsNormal;
    FLaunchSettings := Http;
  end;
end;

destructor TLinksLabel.Destroy;
begin
  with FLaunch do
  begin
    FApp.Free;
    FMail.Free;
    FHttp.Free;
    Free;
  end;
  FImageChangeLink.Free;
  if FTextFlick.FEnableFlick then
    FTextFlick.FlickTimer.Free;
  if FTextRun.FEnableRun then
    RunTimer.Free;
  FTextRun.Free;
  FTextFlick.Free;
  inherited;
end;

procedure TLinksLabel.Loaded;
begin
  inherited;
  Font.Color := FLinkColor;
  if FTextFlick.FEnableFlick then
  begin
    FTextRun.FEnableRun := False;
    FAutoSize := True;
    with FTextFlick do
    begin
      FlickTimer := TTimer.Create(Self);
      FlickTimer.Interval := FTextFlick.FSpeed;
      FlickTimer.OnTimer := OnFlickTimer;
    end;
    invalidate;
  end;

  if FTextRun.FEnableRun then
  begin
    FTextFlick.FEnableFlick := False;
    FAutoSize := False;
    with FTextRun do
    begin
      FCurrentStep := 0;
      RunTimer := TTimer.Create(Self);
      with RunTimer do
      begin
        Enabled := FTextRun.FEnableRun;
        OnTimer := OnRunTimer;
        Interval := FTextRun.FSpeed;
      end;
    end;
    Paint;
  end;
end;

procedure TLinksLabel.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if Caption = '' then
    Caption := FLaunch.FHttp.FAddress;
  Invalidate;
end;

procedure TLinksLabel.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TLinksLabel.Click;
var
  WS: TWindowState;
  Param: string;
  Car: Cardinal;
  bOK: Boolean;
begin
  bOK := False;
  WS := wsNormal;
  case FLaunch.FLaunchSettings of
    Http:
      begin
        Param := 'http://' + FLaunch.FHttp.FAddress;
        WS := FLaunch.FHttp.FWindowState;
      end;
    MailTo:
      begin
        Param := 'mailto:' + FLaunch.FMail.FAddress + '?subject=' +
          FLaunch.FMail.FSubject + '&body=' + FLaunch.FMail.FBody;
        WS := FLaunch.FMail.FWindowState;
      end;
    Application:
      begin
        Param := FLaunch.Fapp.FPath + '' + FLaunch.Fapp.FCommand;
        WS := FLaunch.FApp.FWindowState;
      end;
  end;
  if (FLaunch.FHttp.FAddress <> '') or (FLaunch.FMail.FAddress <> '') or (FLaunch.FApp.FPath <> '') then
  begin
    Car := ShellExecute(0, 'open', PChar(Param), nil, nil, SetWinState(WS));
    bOK := Car > 32;
  end
  else
  begin
    bOK := False;
    Car := 4;
  end;
  if Assigned(FOnLaunch) then
    FonLaunch(bOK, ErrorText(Car), Car);
  Font.Color := FUpColor;
  inherited;
end;

procedure TLinksLabel.DoCopyAddress(Sender: TObject);
begin
  case FLaunch.FLaunchSettings of
    Http: Clipboard.AsText := FLaunch.FHttp.FAddress;
    MailTo: Clipboard.AsText := FLaunch.FMail.FAddress;
    Application: Clipboard.AsText := FLaunch.FApp.FPath;
  end;
end;

procedure TLinksLabel.CanvasTextOut(Canvas: TCanvas; X, Y: Integer; Text: string);
begin
  with Canvas do
  begin
    case FTextStyle of
      ts3D_F:
        begin
          Font.Color := F3d_F_Color;
          TextOut(X + FShift, Y + FShift, Text);
        end;
      ts3D_B:
        begin
          Font.Color := F3d_B_Color;
          TextOut(X - FShift, Y - FShift, Text);
        end;
    end;
    TextOut(X, Y, Text);
  end;
end;

procedure TLinksLabel.DoLaunch(Sender: TObject);
var
  Param: string;
  Car: Cardinal;
  bOK: Boolean;
  WS: TWindowState;
begin
  WS := wsNormal;
  bOK := False;
  case FLaunch.FLaunchSettings of
    Http:
      begin
        Param := 'http://' + FLaunch.FHttp.FAddress;
        WS := FLaunch.FHttp.FWindowState;
      end;
    MailTo:
      begin
        Param := 'mailto:' + FLaunch.FMail.FAddress + '?subject=' +
          FLaunch.FMail.FSubject + '&body=' + FLaunch.FMail.FBody;
        WS := FLaunch.FMail.FWindowState;
      end;
    Application:
      begin
        Param := FLaunch.Fapp.FPath + '' + FLaunch.Fapp.FCommand;
        WS := FLaunch.FApp.FWindowState;
      end;
  end;
  Car := ShellExecute(0, 'open', PChar(Param), nil, nil, SetWinState(WS));
  bOK := Car > 32;
  if Assigned(FOnLaunch) then
    FonLaunch(bOK, ErrorText(Car), Car);
  Font.Color := FUpColor;
end;

procedure TLinksLabel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FHighlightOnMouseOver then
    Font.Style := Font.Style + [fsBold];
  case FLaunch.FLaunchSettings of
    Http: Hint := 'Address:  ' + FLaunch.FHttp.FAddress;
    MailTo: Hint := 'Mail To:  ' + FLaunch.FMail.FAddress;
    Application: Hint := FLaunch.FApp.FDescription;
  end;
end;

procedure TLinksLabel.MouseLeave(Sender: TObject);
begin
  if FHighlightOnMouseOver then
    Font.Style := Font.Style - [fsBold];
end;

procedure TLinksLabel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Button = mbLeft) and (FColorOnMouseClick) then
    Font.Color := FDownColor
  else
    if (Button = mbRight) and (not Assigned(PopupMenu)) then
      GetPopUpMenu;
end;

procedure TLinksLabel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
    FImages := nil;
end;

procedure TLinksLabel.Paint;
var
  Height, PosHeightX, PosWidthY, ModWidth, ModHeight,
    ModWidthX, ModHeightY: Integer;
  StepPosition: Double;
begin
  inherited;
  ModWidthX := 0;
  ModHeightY := 0;
  with Canvas do
  begin
    ModWidth := TextWidth(Caption) + 2;
    ModHeight := TextHeight(Caption) + 2;
    Brush.Color := Self.Color;
    Brush.Style := bsSolid;
    Font := Self.Font;
    FillRect(ClientRect);
    if Assigned(FImages) and (FImageIndex >= 0) and (FImageIndex < FImages.Count) then
    begin
      ImageList_Draw(FImages.Handle, FImageIndex, Handle,
        (Width - FImages.Width) shr 1, 0, ILD_NORMAL or ILD_TRANSPARENT);
      Height := FImages.Height;
    end
    else
      Height := 0;
    case FAlignment of
      taRightJustify: ModWidthX := Width - ModWidth;
      taCenter: ModWidthX := (Width - ModWidth) div 2;
      taLeftJustify: ModWidthX := 0;
    end;
    case FLayout of
      tlBottom: ModHeightY := Height - ModHeight + 30;
      tlCenter: ModHeightY := (Height - ModHeight) div 2 + 15;
      tlTop: ModHeightY := 0;
    end;
    if FTextRun.FEnableRun then
    begin
      StepPosition := FTextRun.FCurrentStep / FTextRun.FSteps;
      PosHeightX := ModWidthX;
      PosWidthY := ModHeightY;
      case FTextRun.FDirection of
        diRightToLeft:
          begin
            PosWidthY := ModHeightY;
            PosHeightX := -ModWidth + Round((ModWidth + Width) * (1 - StepPosition));
          end;
        diLeftToRight:
          begin
            PosWidthY := ModHeightY;
            PosHeightX := -ModWidth + Round((ModWidth + Width) * (StepPosition));
          end;
        diTopToBottom:
          begin
            PosHeightX := ModWidthX;
            PosWidthY := -ModHeight + Round((ModHeight + Height) * (StepPosition));
          end;
        diBottomToTop:
          begin
            PosHeightX := ModWidthX;
            PosWidthY := -ModHeight + Round((ModHeight + Height) * (1 - StepPosition));
          end;
      end; //running text
      CanvasTextOut(Canvas, PosHeightX, PosWidthY, Caption);
    end
    else
    begin //images
      CanvasTextOut(Canvas, (Width - TextWidth(Caption)), Height, Caption)
    end;
  end;
end;

procedure TLinksLabel.ImageListChange(Sender: TObject);
begin
  Invalidate;
end;

function TLinksLabel.GetPopupMenu: TPopupMenu;
var
  Pop: TPopupMenu;
  p: TPoint;
begin
  Pop := TPopupMenu.Create(Self);
  with Pop do
  begin
    p := Self.ClientToScreen(Point(0, 0));
    Popup(P.X, P.Y);
    Items.Clear;
    with Items do
    begin
      NewTopLine;
      case FLaunch.FLaunchSettings of
        Http: Add(NewItem('Copy Address', 0, False, True, DoCopyAddress, 0, 'MenuItem1'));
        MailTo: Add(NewItem('Copy Address', 0, False, True, DoCopyAddress, 0, 'MenuItem1'));
      end;
      Add(NewItem('Launch', 0, False, True, DoLaunch, 0, 'MenuItem2'));
      NewBottomLine;
    end;
  end;
  Result := Pop;
end;

function TLinksLabel.GetProgramPathFromExt(const Ext: string): string;
var
  S: string;
begin
  Result := '';
  with TRegistry.Create do
  try
    RootKey := HKEY_CLASSES_ROOT;

    if OpenKey('\' + Ext, False) then
    begin
      S := ReadString('');
      if S <> '' then
      begin
        if OpenKey('\' + S + '\shell\open\command', False) then
        begin
          S := ReadString('');
          if S <> '' then
            Result := S;
        end;
      end
      else
      begin
        if OpenKey('\' + Ext + '\shell\open\command', False) then
        begin
          S := ReadString('');
          if S <> '' then
            Result := S;
        end;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TLinksLabel.OnFlickTimer(Sedner: Tobject);
begin
  if csDesigning in ComponentState then
  begin
    FTextFlick.FlickTimer.Enabled := False;
    Exit;
  end;
  if FTextFlick.FEnableFlick then
    Visible := not Visible;
end;

procedure TLinksLabel.OnRunTimer(Sender: TObject);
begin
  if not RunTimer.Enabled then
    Exit;
  with FTextRun do
  begin
    if (FCurCycle < FRepeatNumber) or FContinuous then
    begin
      Inc(FCurrentStep);
      Paint;
      if FCurrentStep = FSteps then
      begin
        FCurrentStep := 0;
        if not FContinuous then
          Inc(FCurCycle);
      end;
    end
    else
      FEnableRun := False;
  end;
end;
//Set---------------------------------------------------------------------------

function TLinksLabel.SetWinState(State: TWindowState): Cardinal;
begin
  Result := 0;
  case State of
    wsNormal: Result := SW_SHOWNORMAL;
    wsMinimized: Result := SW_MINIMIZE;
    wsMaximized: Result := SW_MAXIMIZE;
  end;
end;

procedure TLinksLabel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  Size: TSize;
begin
  if Assigned(Parent) then
  begin
    if Assigned(FImages) then
    begin
      Canvas.Font := Font;
      Size := Canvas.TextExtent(Caption);
      AWidth := Size.cx;
      if (AWidth < FImages.Width) then
        AWidth := FImages.Width;
      AHeight := Size.cy;
      Inc(AHeight, FImages.Height);
    end
    else
    begin
      Canvas.Font := Font;
      Size := Canvas.TextExtent(Caption);
      AHeight := Size.cy + 16;
    end;
  end;
  invalidate;
  inherited;
end;

procedure TLinksLabel.SetImageIndex(const Value: integer);
begin
  begin
    if FImageIndex <> Value then
    begin
      FImageIndex := Value;
      Invalidate;
    end;
  end;
end;

procedure TLinksLabel.SetImages(Value: TCustomImageList);
begin
  if Images <> nil then
    Images.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if Images <> nil then
  begin
    Images.RegisterChanges(FImageChangeLink);
    Images.FreeNotification(Self);
  end;
  Invalidate;
end;

procedure TLinksLabel.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TLinksLabel.SetLayout(Value: TTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TLinksLabel.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    AdjustBounds;
    Paint;
  end;
end;

procedure TLinksLabel.SetTextStyle(Value: TTextStyle);
begin
  if FTextStyle <> Value then
  begin
    FTextStyle := Value;
    Paint;
  end;
end;

procedure TLinksLabel.SetLinkColor(Value: TColor);
begin
  if FLinkColor <> Value then
  begin
    FLinkColor := Value;
    Paint;
  end;
end;

procedure TLinksLabel.SetShift(Value: integer);
begin
  if FShift <> Value then
  begin
    FShift := Value;
    Paint;
  end;
end;

//--------TTextFlick-----------------------------------------------------------

procedure TTextFlick.SetSpeed(const Value: integer);
begin
  if FSpeed <> Value then
    FSpeed := Value;
  if Value > 1000 then
    FSpeed := 1000;
  if Value < 1 then
    FSpeed := 1;
end;
//------------------------------------------------------------------------------

procedure THttp.SetAddress(const Value: string);
begin
  if FAddress <> Value then
    FAddress := Value;
end;
//--------TLaunch---------------------------------------------------------------

procedure TLaunch.SetLaunchOptions(Value: TLaunchSettings);
begin
  if FLaunchSettings <> Value then
    FLaunchSettings := Value;
end;
//--------TTextRun--------------------------------------------------------------

procedure TTextRun.SetDirection;
begin
  if (FDirection <> Value) then
    FDirection := Value;
end;

procedure TTextRun.SetSteps(Value: Integer);
begin
  if FSteps <> Value then
    FSteps := Value;
end;

procedure TTextRun.SetEnableRun(Value: Boolean);
begin
  if (FEnableRun <> Value) then
  begin
    FEnableRun := Value;
    FCurCycle := 0;
    FCurrentStep := 0;
  end;
end;

procedure TTextRun.SetSpeed(Value: Integer);
begin
  if FSpeed <> Value then
  begin
    FSpeed := Value;
    if Value > 1000 then
      FSpeed := 1000;
    if Value < 1 then
      FSpeed := 1;
  end;
end;
//---------------------------------------------------------------------------

end.
