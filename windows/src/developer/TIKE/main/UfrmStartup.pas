(*
  Name:             UfrmStartup
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    24 Jul 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Rework for Keyman 7
                    23 Aug 2006 - mcdurdin - Polish display of splash
                    23 Aug 2006 - mcdurdin - Allow click anywhere on screen to cancel activated dialog
                    14 Sep 2006 - mcdurdin - Unicode controls
                    28 Sep 2006 - mcdurdin - List activated modules in startup form
                    04 Dec 2006 - mcdurdin - Display application minimize during startup
                    30 Jan 2007 - mcdurdin - Remove Release Candidate/Beta text
                    25 May 2010 - mcdurdin - I2392 - Activation Client integration
                    26 Jul 2010 - mcdurdin - I2467 - 8.0 renumber
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
                    17 Aug 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    27 Feb 2014 - mcdurdin - I4084 - V9.0 - Splash should be version 9.0 style
                    24 Jul 2015 - mcdurdin - I4796 - Refresh Keyman Developer look and feel for release
*)
unit UfrmStartup;  // I3306   // I4084   // I4796

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  KeymanDeveloperUtils, UserMessages,
  ExtCtrls, StdCtrls, UfrmTike, Vcl.Imaging.pngimage, PaintPanel,
  Vcl.Themes;

type
//  TLabel = class (StdCtrls.TLabel)   // I4796
//    procedure DoDrawText(var Rect: TRect; Flags: Longint); override;
//  end;

  TfrmStartup = class(TTIKEForm)
    Timer: TTimer;
    PaintPanel1: TPaintPanel;
    lblVersion: TLabel;
    Image1: TImage;
    Shape1: TShape;
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure imgSplashClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    nalpha: Integer;
    procedure Modalate;
    procedure WMUserFormShown(var Message: TMessage); message WM_USER_FORMSHOWN;
    procedure AppMinimize(Sender: TObject);
  protected
    function GetHelpTopic: string; override;
    procedure WndProc(var Message: TMessage); override;
  end;

procedure ForceShowStartup(Parent: TForm);
procedure ShowStartup;
procedure ShowStartupModal(Parent: TForm);
procedure CloseStartup;
procedure StartupOnTop;

implementation

{$R *.DFM}

uses
  Dialogs,

  Keyman.Developer.System.HelpTopics,

  OnlineConstants,
  UfrmMain,
  utilexecute,
  VersionInfo;

var
  frmStartup: TfrmStartup = nil;

const
  WS_EX_LAYERED = $80000;
  LWA_COLORKEY  = $1;
  LWA_ALPHA     = $2;

  COUNTDOWN_TIME = 5;

type TSetLayeredWindowAttributes = function(hwnd: HWND; crKey: COLORREF; bAlpha: BYTE; dwFlags: DWORD): DWORD; stdcall;
var SetLayeredWindowAttributes: TSetLayeredWindowAttributes = nil;
const SProcSetLayeredWindowAttributes = 'SetLayeredWindowAttributes';

procedure StartupOnTop;
begin
  if Assigned(frmStartup) then
    frmStartup.BringToFront;
//    SetWindowPos(frmStartup.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
end;

procedure TfrmStartup.FormCreate(Sender: TObject);
begin
  inherited;
  Application.OnMinimize := AppMinimize;
  lblVersion.Caption := 'Version ' + GetVersionString;

  if ShouldShowStartup then
    SetLayeredWindowAttributes := TSetLayeredWindowAttributes(GetProcAddress(GetModuleHandle('user32.dll'),
      SProcSetLayeredWindowAttributes));
  if Assigned(SetLayeredWindowAttributes) then
  begin
    SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
    SetLayeredWindowAttributes(Handle, 0, 255, LWA_ALPHA);
  end;
  nalpha := 768;
  //Timer.Enabled := True;
end;

procedure TfrmStartup.FormDestroy(Sender: TObject);
begin
  inherited;
  Application.OnMinimize := nil;
end;

procedure TfrmStartup.Modalate;
begin
  Timer.Enabled := True;
end;

procedure TfrmStartup.TimerTimer(Sender: TObject);
begin
  nalpha := nalpha - 4;
  if nalpha < 0 then
    ModalResult := mrOk
  else if nalpha < 256 then
    if Assigned(SetLayeredWindowAttributes) then SetLayeredWindowAttributes(Handle, 0, nalpha, LWA_ALPHA);
  //Close;
end;

procedure TfrmStartup.AppMinimize(Sender: TObject);
begin
  Application.Restore;
end;

procedure TfrmStartup.WMUserFormShown(var Message: TMessage);
begin
  Windows.SetCapture(Handle);
end;

procedure TfrmStartup.WndProc(var Message: TMessage);
begin
  if Message.Msg = WM_LBUTTONDOWN then
  begin
    ModalResult := mrOk;
    Message.Result := 1;
  end
  else
    inherited;
end;

procedure TfrmStartup.imgSplashClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmStartup.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  frmStartup := nil;
  Action := caFree;
end;

procedure TfrmStartup.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Timer.Enabled := False
  else if (Key <> VK_CONTROL) and (Key <> VK_SHIFT) then
    ModalResult := mrOk;
end;

procedure TfrmStartup.FormShow(Sender: TObject);
begin
  PostMessage(Handle, WM_USER_FormShown, 0, 0);
end;

function TfrmStartup.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_Startup;
end;

procedure ForceShowStartup(Parent: TForm);
begin
  // Show the startup dialog in a subsequent display (used from About dialog)
  frmStartup := TfrmStartup.Create(Parent);
  with frmStartup do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure ShowStartup;
begin
  if ShouldShowStartup then
  begin
    frmStartup := TfrmStartup.Create(Application);
    frmStartup.Show;
    frmStartup.Update;
  end
  else
    frmStartup := nil;
end;

procedure CloseStartup;
begin
  if Assigned(frmStartup) then frmStartup.Free;
end;

procedure ShowStartupModal(Parent: TForm);
var
  FFocus: Hwnd;
  WindowList: Pointer;
begin
  if ShouldShowStartup then
  begin
    if Assigned(frmStartup) then
    begin
      FFocus := GetFocus;
      WindowList := DisableTaskWindows(frmStartup.Handle);
      EnableWindow(frmStartup.Handle, True);
      try
        SendMessage(frmStartup.Handle, CM_ACTIVATE, 0, 0);
        frmStartup.Modalate;
        frmStartup.SetFocus;
        //frmStartup.BringToFront;
        //SetWindowPos(frmStartup.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
        frmStartup.ModalResult := 0;
        repeat
          Application.HandleMessage;
          if Application.Terminated then frmStartup.ModalResult := mrCancel
          //else if frmStartup.ModalResult <> 0 then frmStartup.Close;
        until not Assigned(frmStartup) or (frmStartup.ModalResult <> 0);

        if Assigned(frmStartup) then
          SendMessage(frmStartup.Handle, CM_DEACTIVATE, 0, 0);
      finally
        if Assigned(frmStartup) then
          EnableWindow(frmStartup.Handle, False);
        EnableTaskWindows(WindowList);
        SetFocus(FFocus);
      end;

      frmStartup.Free;
    end
    else
    begin
      frmStartup := TfrmStartup.Create(Application);
      try
        frmStartup.ShowModal;
      finally
        frmStartup.Free;
      end;
    end;
  end;

  frmStartup := nil;
end;

procedure TfrmStartup.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
end;

{ All this to get a white font: (apparently XE5 improves this) }   // I4796

//type
//  TLabelHelper= class helper for TCustomLabel
//    procedure DrawNormalText(DC: HDC; const Text: UnicodeString; var TextRect: TRect; TextFlags: Cardinal);
//  end;

{ TLabelHelper }

//procedure TLabelHelper.DrawNormalText(DC: HDC; const Text: UnicodeString;
//  var TextRect: TRect; TextFlags: Cardinal);
//begin
//  Self.DoDrawNormalText(DC, Text, TextRect, TextFlags);
//end;


{ TLabel }
       (*
procedure TLabel.DoDrawText(var Rect: TRect; Flags: Integer);
const
  EllipsisStr = '...';
  Ellipsis: array[TEllipsisPosition] of Longint = (0, DT_PATH_ELLIPSIS, DT_END_ELLIPSIS, DT_WORD_ELLIPSIS);
var
  Text, DText: string;
  NewRect: TRect;
  Height, Delim: Integer;
begin
  Text := GetLabelText;
  if (Flags and DT_CALCRECT <> 0) and
     ((Text = '') or ShowAccelChar and (Text[1] = '&') and (Length(Text) = 1)) then
    Text := Text + ' ';

  if Text <> '' then
  begin
    if not ShowAccelChar then Flags := Flags or DT_NOPREFIX;
    Flags := DrawTextBiDiModeFlags(Flags);
    Canvas.Font := Font;
    if (EllipsisPosition <> epNone) and not AutoSize then
    begin
      DText := Text;
      Flags := Flags and not DT_EXPANDTABS;
      Flags := Flags or Ellipsis[EllipsisPosition];
      if WordWrap and (EllipsisPosition in [epEndEllipsis, epWordEllipsis]) then
      begin
        repeat
          NewRect := Rect;
          Dec(NewRect.Right, Canvas.TextWidth(EllipsisStr));
          DrawNormalText(Canvas.Handle, DText, NewRect, Flags or DT_CALCRECT);
          Height := NewRect.Bottom - NewRect.Top;
          if (Height > ClientHeight) and (Height > Canvas.Font.Height) then
          begin
            Delim := LastDelimiter(' '#9, Text);
            if Delim = 0 then
              Delim := Length(Text);
            Dec(Delim);
            if ByteType(Text, Delim) = mbLeadByte then
              Dec(Delim);
            Text := Copy(Text, 1, Delim);
            DText := Text + EllipsisStr;
            if Text = '' then
              Break;
          end else
            Break;
        until False;
      end;
      if Text <> '' then
        Text := DText;
    end;

    if Enabled or StyleServices.Enabled then
      DrawNormalText(Canvas.Handle, Text, Rect, Flags)
    else
    begin
      OffsetRect(Rect, 1, 1);
      Canvas.Font.Color := clBtnHighlight;
      DrawNormalText(Canvas.Handle, Text, Rect, Flags);
      OffsetRect(Rect, -1, -1);
      Canvas.Font.Color := clBtnShadow;
      DrawNormalText(Canvas.Handle, Text, Rect, Flags);
    end;
  end;
end;
*)
end.

