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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  sysinfo_main;

type
  TfrmExceptionHandler = class(TForm)
    cmdSend: TButton;
    cmdAbort: TButton;
    panTitle: TPanel;
    bevelTitle: TBevel;
    lblTitle: TLabel;
    imgIcon: TImage;
    lblText1: TLabel;
    lblText2: TLabel;
    lblText3: TLabel;
    pages: TPageControl;
    tabInfo: TTabSheet;
    tabDetails: TTabSheet;
    lblEmail: TLabel;
    editEmail: TEdit;
    memoComments: TMemo;
    lblComments: TLabel;
    chkViewTechnicalReport: TCheckBox;
    memoCrashText: TMemo;
    memoDetails: TMemo;
    lblPrivacy: TLabel;
    chkSendDiagnostics: TCheckBox;
    lblStatus: TLabel;
    Label1: TLabel;
    procedure cmdAbortClick(Sender: TObject);
    procedure cmdSendClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lblPrivacyClick(Sender: TObject);
    procedure chkViewTechnicalReportClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure memoDetailsClick(Sender: TObject);
    procedure memoDetailsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FCrashID: WideString;
    FVersion: string;
    FFileName: string;
    FMainForm: TfrmDiagnostics;
    FLogFile: string;
    procedure UpdateTechnicalReportVisibility;
    procedure ShowFailureToUpload(const msg, tsifilename: string);
    procedure WMUser100(var Message: TMessage); message WM_USER+100;
    function AppMessage(var Message: TMessage): Boolean;
    procedure UpdateStatus;
    { Private declarations }
  public
    { Public declarations }
    property CrashID: WideString read FCrashID write FCrashID;
    property LogFile: string read FLogFile write FLogFile;
    property FileName: string read FFileName write FFileName;
    property Version: string read FVersion write FVersion;
    property MainForm: TfrmDiagnostics read FMainForm write FMainForm;
  end;

function ShowRemoteException(AOwner: TfrmDiagnostics; LogFileName: string): Boolean;

implementation

uses
  System.Zip,
  System.StrUtils,
  Xml.xmldom,

  ErrLogPath,
  GlobalProxySettings,
  //UfrmMain,
  Unicode,
  utilexecute,
  utilsystem,
  httpuploader,
  Upload_Settings,
  utildir;

{$R *.dfm}

var
  ApplicationTerminated: Boolean = False;

function ShowRemoteException(AOwner: TfrmDiagnostics; LogFileName: string): Boolean;
type
  TLogFileSection = (lsNone, lsSystem, lsMessage, lsDetail, lsLog);
const
  FSectionTitle: array[TLogFileSection] of string = ('','System','Message','Detail','Log');
var
  ExceptionLog: TStringList;
  i, n: Integer;
  s, t: string;
  FSection, j: TLogFileSection;
  ATitle, ACrashID, AFileName, AVersion, AMessage, ADetail, ALog: string;
  //AProductID: Integer;
  SystemName, SystemValue: string;
begin
  Result := False;

    { Log file format:
        [System]
        Title=APPLICATION TITLE
        CrashID=filename_version_address_exceptionname
        Application=filename.exe
        ProductID=1
        Version=1.2.3.4

        [Message]
        EXCEPTIONMESSAGE

        [Detail]
        EXCEPTIONDETAIL

        [Log]
        EXCEPTIONLOG
    }

  if not FileExists(LogFileName) then
    Exit;

  ATitle := '';
  ACrashID := '';
  AFileName := '';
  AVersion := '';
  AMessage := '';
  ADetail := '';
  ALog := '';
  //AProductID := 0;

  ExceptionLog := TStringList.Create;
  try
    ExceptionLog.LoadFromFile(LogFileName);

    FSection := lsNone;
    for i := 0 to ExceptionLog.Count - 1 do
    begin
      s := ExceptionLog[i]; t := Trim(s);
      if Copy(t, 1, 1) = '[' then
      begin
        FSection := lsNone;
        for j := Low(FSectionTitle) to High(FSectionTitle) do
          if SameText('['+FSectionTitle[j]+']', t) then
          begin
            FSection := j;
            Break;
          end;
      end
      else
      begin
        case FSection of
          lsNone: ;
          lsSystem:
            begin
              n := Pos('=', t);
              if n > 0 then
              begin
                SystemName := Trim(Copy(t, 1, n-1));
                SystemValue := Trim(Copy(t, n+1, Length(t)));

                if SameText(SystemName, 'title') then ATitle := SystemValue
                else if SameText(SystemName, 'crashid') then ACrashID := SystemValue
                else if SameText(SystemName, 'application') then AFileName := SystemValue
                else if SameText(SystemName, 'version') then AVersion := SystemValue
                //else if SameText(SystemName, 'productid') then AProductID := StrToIntDef(SystemValue, 0);
              end;
            end;
          lsMessage: AMessage := AMessage + s + #13#10;
          lsDetail: ADetail := ADetail + s + #13#10;
          lsLog: ALog := ALog + s + #13#10;
        end;
      end;
    end;

    with TfrmExceptionHandler.Create(nil) do
    try
      MainForm := AOwner;
      FormStyle := fsStayOnTop;
      CrashID := ACrashID;
      Caption := ATitle;
      Application.Title := ATitle;
      FileName := AFileName;
      Version := AVersion;
      lblTitle.Caption :=
        ATitle +' has encountered a problem and needs to close.  We are sorry for the inconvenience.';
      //imgIcon.Picture.Assign(Application.Icon);

      LogFile := LogFileName;

      memoCrashText.Text := AMessage + ADetail;
      memoDetails.Text := ALog;
      ShowModal;
    finally
      Free;
    end;
  finally
    ExceptionLog.Free;
  end;

  Result := True;
end;

procedure TfrmExceptionHandler.chkViewTechnicalReportClick(Sender: TObject);
begin
  UpdateTechnicalReportVisibility;
  UpdateStatus;
end;

procedure TfrmExceptionHandler.UpdateTechnicalReportVisibility;
var
  e: Boolean;
begin
  e := chkViewTechnicalReport.Checked;
  //tabInfo.TabVisible := e;
  tabDetails.TabVisible := e;
  tabDetails.Enabled := e;
end;

procedure TfrmExceptionHandler.WMUser100(var Message: TMessage);
begin
  SetForegroundWindow(Handle);
  SetFocus;
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE);
end;

procedure TfrmExceptionHandler.cmdAbortClick(Sender: TObject);
begin
  if cmdSend.Enabled then
    ModalResult := mrAbort
  else
    ModalResult := mrOk;
end;

procedure TfrmExceptionHandler.cmdSendClick(Sender: TObject);
var
  FTSIFileName, s: string;
  n: Integer;
  I: Integer;
  f: TSearchRec;
  ErrLogs: TStrings;
begin
  ErrLogs := TStringList.Create;
  try
    FormStyle := fsNormal;
    Screen.Cursor := crHourglass;

    if FindFirst(GetErrLogPath + '*.errlog', 0, f) = 0 then
    begin
      repeat
        ErrLogs.Add(GetErrLogPath + f.Name);
      until FindNext(f) <> 0;
      FindClose(f);
    end;

    if chkSendDiagnostics.Checked
      then FTSIFileName := FMainForm.CollectDiagnostics
      else FTSIFileName := '';

    with THTTPUploader.Create(nil) do
    try
      Request.HostName := API_Server;
      Request.Protocol := API_Protocol;
      Request.UrlPath := API_Path_Crash;
      Proxy.Server := GetProxySettings.Server;
      Proxy.Port := GetProxySettings.Port;
      Proxy.Username := GetProxySettings.Username;
      Proxy.Password := GetProxySettings.Password;
      Fields.Add('CrashID', UTF8Encode(FCrashID));   // I3694
      Fields.Add('Application', UTF8Encode(FFileName));   // I3694
      Fields.Add('Version', UTF8Encode(FVersion));   // I3694
      Fields.Add('Text', UTF8Encode(memoCrashText.Text));   // I3694
      Fields.Add('Details', UTF8Encode(memoDetails.Text + #13#10#13#10 + 'Email Address: '+editEmail.Text+#13#10#13#10+'Comments:'+#13#10+memoComments.Text));   // I3694
      for i := 0 to ErrLogs.Count - 1 do
        Files.Add('ErrLog'+UTF8Encode(IntToStr(i)), ErrLogs[i]);   // I3694
      if FileExists(FTSIFileName) then Files.Add('DiagnosticReport', FTSIFileName);
      try
        Upload;
      except
        on E:EHTTPUploader do
        begin
          ShowFailureToUpload(E.ClassName+': '+E.Message, FTSIFileName);
          Exit;
        end;
        on E:EHTTPUploaderCancel do
          Exit;
      end;
      if Response.StatusCode = 200 then
      begin
        s := UTF8ToString(Response.MessageBodyAsString);   // I3694
        if Copy(s, 1, 7) = '<error>' then
        begin
          Delete(s,1,7);
          n := Pos('</error>', s);
          if n > 0 then s := Copy(s,1,n-1);
          ShowMessage('Unable to successfully upload problem details: '+s);
        end
        else
        begin
          n := Pos('<id>', s);
          if n > 0 then
          begin
            Delete(s,1,n+3);
            n := Pos('</id>', s);
            if n > 0 then s := Copy(s,1,n-1);

            if MessageDlg('Details of the problem have been successfully sent to Keyman Support.  The reference number for this issue is I'+s+'.  '+
              'The Keyman Support team will review this issue as part of a future update to this product.  '+
              'If you need urgent assistance to resolve this problem, please contact Keyman Support, and '+
              'quote issue I'+s+'.'#13#10#13#10+
              'Would you like to view more information about this issue online?', mtConfirmation, mbYesNoCancel, 0) = mrYes then
            begin
              TUtilExecute.URL(MakeKeymanURL(Format(URLPath_KeymanException, [s])));
            end;
            cmdSend.Enabled := False;
            cmdSend.Default := False;
            cmdAbort.Default := True;
            if FileExists(FTSIFileName) then
            begin
              DeleteFile(FTSIFileName);
              FMainForm.DeleteDiagFiles;   // I3778
            end;
            if FileExists(FLogFile) then DeleteFile(FLogFile);
            for I := 0 to ErrLogs.Count - 1 do
              DeleteFile(ErrLogs[I]);
          end
          else
            ShowMessage('Unable to successfully upload problem details: '+s);
        end;
      end
      else
        ShowFailureToUpload('status code '+IntToStr(Response.StatusCode), FTSIFileName);
    finally
      Free;
      Screen.Cursor := crDefault;
    end;
  finally
    ErrLogs.Free;
  end;
end;

procedure TfrmExceptionHandler.ShowFailureToUpload(const msg, tsifilename: string);
var
  FTempFileName: string;
begin
  if FileExists(TSIFileName) then
  begin
    with TZipFile.Create do
    try
      Open(tsifilename, TZipMode.zmReadWrite);
      Add(FLogFile);
    finally
      Free;
    end;
    FTempFileName := TSIFileName;
  end
  else
    FTempFileName := FLogFile;

  if MessageDlg(
    'Keyman Desktop Diagnostics was unable to upload the problem report to the Keyman server ('+
    msg+').  The problem report has been saved in '+FTempFileName+'.'+#13#10#13#10+
    'Please send this problem report to Keyman Support.  '+
    'Do you want to open the folder with the problem report now?',
    mtError, [mbYes,mbNo], 0) = mrYes then
  begin
    OpenContainingFolder(FTempFileName);
  end;
end;

procedure TfrmExceptionHandler.FormCreate(Sender: TObject);
begin
  Application.HookMainWindow(AppMessage);
end;

procedure TfrmExceptionHandler.FormDestroy(Sender: TObject);
begin
  if FileExists(FLogFile) then
    DeleteFile(FLogFile);

  Application.UnhookMainWindow(AppMessage);
end;

procedure TfrmExceptionHandler.FormShow(Sender: TObject);
begin
  PostMessage(Handle, WM_USER+100, 0, 0);
end;

procedure TfrmExceptionHandler.lblPrivacyClick(Sender: TObject);
begin
  TUtilExecute.URL(MakeKeymanURL(URLPath_Privacy));
end;

procedure TfrmExceptionHandler.UpdateStatus;
begin
  lblStatus.Caption := WideFormat('Col %d, Line %d', [memoDetails.CaretPos.X+1, memoDetails.CaretPos.Y+1]); 
end;

procedure TfrmExceptionHandler.memoDetailsClick(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TfrmExceptionHandler.memoDetailsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  UpdateStatus;
end;

function TfrmExceptionHandler.AppMessage(var Message: TMessage): Boolean;
begin
  if Message.Msg = WM_ACTIVATEAPP then
  begin
    { Stop Delphi from de-topmosting Visual Keyboard!}
    with Message do
      Result := DefWindowProc(Handle, Msg, WParam, LParam);
    Result := True;
  end
  else if (Message.Msg = WM_SYSCOMMAND) and (Message.WParam = SC_RESTORE) then
  begin
    // Handle the case where Win+M pressed, window never restores
    PostMessage(Handle, WM_SYSCOMMAND, SC_RESTORE, 0);
    Result := False;
  end
  else
    Result := False;
end;

end.
