(*
  Name:             UfrmAboutTike
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    3 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    28 Sep 2006 - mcdurdin - Version 7
                    25 May 2010 - mcdurdin - I2392 - Activation Client integration
                    26 Jul 2010 - mcdurdin - I2467 - 8.0 renumber
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
                    26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    17 Aug 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    27 Feb 2014 - mcdurdin - I4086 - V9.0 - About should version 9.0 style
                    24 Jul 2015 - mcdurdin - I4796 - Refresh Keyman Developer look and feel for release
                    03 Aug 2015 - mcdurdin - I4819 - Keyman Developer home page in About dialog is incorrect
*)
unit UfrmAboutTike;   // I4086   // I4086   // I4796

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, UfrmTike, Vcl.Imaging.pngimage, PaintPanel;

type
//  TLabel = class (StdCtrls.TLabel)   // I4796
//    procedure DoDrawText(var Rect: TRect; Flags: Longint); override;
//  end;

  TfrmAboutTike = class(TTIKEForm)
    lblVersion: TLabel;
    lblCopyright: TLabel;
    lblWebsite: TLabel;
    Image2: TImage;
    Shape1: TShape;
    cmdOK: TButton;
    Image1: TImage;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure lblWebsiteClick(Sender: TObject);
    procedure imgKeymanClick(Sender: TObject);
  private
    FOnIconClick: TNotifyEvent;
  protected
    function GetHelpTopic: string; override;
  public
    property OnIconClick: TNotifyEvent read FOnIconClick write FOnIconClick;
  end;

implementation

uses
  VCL.Themes,

  Keyman.Developer.System.HelpTopics,

  KeymanVersion,
  onlineconstants,
  Upload_Settings,
  VersionInfo,
  utilexecute;

const
  SCaption = 'About Keyman Developer '+SKeymanVersion; // I4819

{$R *.DFM}

procedure TfrmAboutTike.FormCreate(Sender: TObject);
begin
  inherited;
  Caption := SCaption;
  lblWebsite.Caption := MakeKeymanURL(URLPath_KeymanDeveloperHome_Presentation);
  lblVersion.Caption := 'Version ' + GetVersionString;
  lblCopyright.Caption := GetVersionCopyright;
end;

procedure TfrmAboutTike.lblWebsiteClick(Sender: TObject);
begin
  TUtilExecute.URL(MakeKeymanURL(URLPath_KeymanDeveloperHome));  // I3349
end;

function TfrmAboutTike.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_About;
end;

procedure TfrmAboutTike.imgKeymanClick(Sender: TObject);
begin
  if ((GetKeyState(VK_SHIFT) and $8000) = $8000) and
     ((GetKeyState(VK_CONTROL) and $8000) = $8000) and
      Assigned(FOnIconClick) then
      FOnIconClick(Self);
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
//  inherited DrawNormalText(DC, Text, TextRect, TextFlags);
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

