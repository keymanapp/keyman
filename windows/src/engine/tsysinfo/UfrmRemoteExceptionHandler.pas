(*
  Name:             UfrmExceptionHandler
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    23 Jan 2013
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Initial version
                    04 Jan 2007 - mcdurdin - Add proxy support
                    30 May 2007 - mcdurdin - I809 - Rework exception reporting
                    05 Jun 2007 - mcdurdin - I809 - Allow user to view more information online about the error
                    13 Jul 2007 - mcdurdin - I939 - Allow script errors to be processed (and support Unicode)
                    05 Nov 2007 - mcdurdin - I1127 - Add system info to reports
                    27 Mar 2008 - mcdurdin - I1312 - Report errors in application event log
                    20 Jul 2008 - mcdurdin - I1553 - Initial version
                    28 Jul 2008 - mcdurdin - I1574 - Global exception reporting - report all errors
                    04 Jun 2009 - mcdurdin - I2003 - UTF8Encode replacement
                    01 Jan 2013 - mcdurdin - I3694 - V9.0 - When Keyman crashes, tsysinfo does not present any UI
                    23 Jan 2013 - mcdurdin - I3778 - V9.0 - Delete diag files after sending through a crash report
*)
unit UfrmRemoteExceptionHandler;   // I3694

interface

uses
  System.UITypes,
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.Imaging.pngimage;

type
  TfrmExceptionHandler = class(TForm)
    cmdAsk: TButton;
    cmdAbort: TButton;
    panTitle: TPanel;
    bevelTitle: TBevel;
    lblTitle: TLabel;
    imgIcon: TImage;
    lblText1: TLabel;
    lblText2: TLabel;
    lblNoPersonal: TLabel;
    panelDetails: TPanel;
    cmdCopyToClipboard: TButton;
    lblAppIDCaption: TLabel;
    lblAppID: TLabel;
    lblCrashID: TLabel;
    lblCrashIDCaption: TLabel;
    procedure lblPrivacyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cmdAskClick(Sender: TObject);
    procedure cmdCopyToClipboardClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure imgIconDblClick(Sender: TObject);
    procedure cmdAbortClick(Sender: TObject);
  private
    FAppID: string;
    FCrashID: string;
    FEventMessage: string;
    FEventClassName: string;
    FProjectName: string;
    procedure WMUser100(var Message: TMessage); message WM_USER+100;
    procedure SetAppID(const Value: string);
    procedure SetCrashID(const Value: string);
    procedure CopyToClipboard;
  public
    property CrashID: string read FCrashID write SetCrashID;
    property AppID: string read FAppID write SetAppID;
    property ProjectName: string read FProjectName write FProjectName;
    property EventClassName: string read FEventClassName write FEventClassName;
    property EventMessage: string read FEventMessage write FEventMessage;
  end;

function RunCrashReportHandler: Boolean;

implementation

uses
  Vcl.Clipbrd,

  Keyman.System.KeymanSentryClient,
  Upload_Settings,
  utilexecute;

{$R *.dfm}

const
  SClipboardMessage =
    'Details for error report for Keyman app:'#13#10+
    '  Application Identifier: %0:s'#13#10+
    '  Error Identifier: %1:s';

  SClipboardMessageTech =
    'Details for error report for Keyman app:'#13#10+
    '  Application Identifier: %0:s'#13#10+
    '  Error Identifier: %1:s'#13#10+
    '  Class Name: %2:s'#13#10+
    '  Message: %3:s';

  S_Title_Message = '%0:s has encountered a problem and needs to close.  We are sorry for the inconvenience.';

  SENTRY_PROJECT_NAME_DESKTOP = 'keyman-desktop';
  SENTRY_PROJECT_NAME_DEVELOPER = 'keyman-developer';

function RunCrashReportHandler: Boolean;
var
  frm: TfrmExceptionHandler;
  crashID, applicationName, applicationID, projectName, eventClassName, eventMessage: string;
  reportExceptions: Boolean;
begin
  if ParamStr(1) <> '-c' then
    Exit(False);

  crashID := ParamStr(2);
  applicationName := ParamStr(3);
  applicationID := ParamStr(4);
  projectName := ParamStr(5);
  eventClassName := ParamStr(6);
  eventMessage := ParamStr(7);
  reportExceptions := ParamStr(8) = 'report';

  Application.CreateForm(TfrmExceptionHandler, frm);
  Application.Title := applicationName;
  frm.CrashID := crashID;
  frm.Caption := applicationName;
  frm.AppID := applicationID;
  frm.ProjectName := projectName;
  frm.EventClassName := eventClassName;
  frm.EventMessage := eventMessage;

  frm.lblText2.Visible := reportExceptions;
  frm.lblNoPersonal.Visible := reportExceptions;
  //? frm.lblNotReported.Visible := not reportExceptions;

  frm.lblTitle.Caption := Format(S_Title_Message, [applicationName]);
  Application.Run;

  Exit(True);
end;

procedure TfrmExceptionHandler.WMUser100(var Message: TMessage);
begin
  SetForegroundWindow(Handle);
  SetFocus;
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE);
end;

procedure TfrmExceptionHandler.cmdCopyToClipboardClick(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TfrmExceptionHandler.CopyToClipboard;
begin
  if GetKeyState(VK_SHIFT) < 0
    then Clipboard.AsText := Format(SClipboardMessageTech, [FAppID, FCrashID, FEventClassName, FEventMessage])
    else Clipboard.AsText := Format(SClipboardMessage, [FAppID, FCrashID]);
end;

procedure TfrmExceptionHandler.cmdAbortClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmExceptionHandler.cmdAskClick(Sender: TObject);
begin
  TUtilExecute.URL(MakeKeymanURL(URLPath_Community));
end;

procedure TfrmExceptionHandler.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = Ord('C')) and (Shift * [ssCtrl, ssAlt] = [ssCtrl]) then
  begin
    CopyToClipboard;
    Key := 0;
  end;
end;

procedure TfrmExceptionHandler.FormShow(Sender: TObject);
begin
  PostMessage(Handle, WM_USER+100, 0, 0);
end;

procedure TfrmExceptionHandler.imgIconDblClick(Sender: TObject);
begin
  if GetKeyState(VK_CONTROL) < 0 then
    ShowMessage(Format(SClipboardMessageTech, [FAppID, FCrashID, FEventClassName, FEventMessage]))
  else if GetKeyState(VK_MENU) < 0 then
    TUtilExecute.URL(Format(TKeymanSentryClient.S_Sentry_ViewEvent_URL, [FProjectName, FCrashID]));
end;

procedure TfrmExceptionHandler.lblPrivacyClick(Sender: TObject);
begin
  TUtilExecute.URL(MakeKeymanURL(URLPath_Privacy));
end;

procedure TfrmExceptionHandler.SetAppID(const Value: string);
begin
  FAppID := Value;
  lblAppID.Caption := Value;
end;

procedure TfrmExceptionHandler.SetCrashID(const Value: string);
begin
  FCrashID := Value;
  lblCrashID.Caption := Value;
end;

end.
