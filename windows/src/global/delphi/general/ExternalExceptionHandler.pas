(*
  Name:             UfrmExceptionHandler
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    1 Jan 2013
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
                    20 Jul 2008 - mcdurdin - I1553 - Report exceptions via tsysinfo
                    28 Jul 2008 - mcdurdin - I1574 - Global exception reporting
                    07 Sep 2009 - mcdurdin - I2099 - Exception records have lost full technical details
                    28 Feb 2011 - mcdurdin - I2773 - Fix crash when control has no window handle
                    03 May 2011 - mcdurdin - I2881 - Fix crash sending message back into window to get window text
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3337 - V9.0 - Review of input/output for Unicode
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
                    26 Jun 2012 - mcdurdin - I3379 - KM9 - Remove old Winapi references now in Delphi libraries
                    01 Jan 2013 - mcdurdin - I3671 - V9.0 - Script error dialog does not report error to website when asked
*)
unit ExternalExceptionHandler;  // I3306

interface

uses
  System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Controls, Forms,
  Dialogs, JclDebug;

procedure LogExceptionToExternalHandler(CrashID, Message, Detail, Log: string);
procedure SendExceptionToExternalHandler(ExceptionObject: TObject; Title, ExceptionMessage: String; CallStack: TStrings);

implementation

uses
  AppEvnts,
  ErrLogPath,
  KLog,
  //UfrmMain,
  RegistryKeys,
  utildir,
  utilexecute,
  utilsystem,
  StrUtils,
  VersionInfo,
  xmldom;

var
  ApplicationTerminated: Boolean = False;

{ TNewDialogFilter }

function InternalGetWindowText(h:HWND; lpStr: PWideChar; n: Integer): Integer; stdcall; external 'user32.dll';

procedure SendExceptionToExternalHandler(ExceptionObject: TObject; Title, ExceptionMessage: String; CallStack: TStrings);
var
  i: Integer;
  ExceptionDetail, ctrls: TStringList;
  vi: TOSVersionInfo;
  p: PChar;
  buf: array[0..1024] of char;
  freespace: Int64;
  totalspace: Int64;
  totalfree: Int64;
  stat: _MEMORYSTATUS;
  s: string;

      // I1312 - report errors in application event log
      procedure EventLogMessage(const Name, Message: String; EventType: DWord; Category: Word; ID: DWord);
      var
        P: Pointer;
        FEventLog: Integer;
      begin
        P := PChar(Message);
        FEventLog := RegisterEventSource(nil, PChar(Name));
        ReportEvent(FEventLog, EventType, Category, ID, nil, 1, 0, @P, nil);
        DeregisterEventSource(FEventLog);
      end;

      procedure EnumerateControls(control: TWinControl; depth: Integer);
      var
        i: Integer;
        flags, name, caption: string;
        buf: array[0..260] of WideChar;
      begin
        try
          if Control.HandleAllocated and (InternalGetWindowText(control.Handle, buf, 260) > 0)  // I2773  // I2881
            then caption := ' "'+buf+'"'
            else caption := '';
            
          if control.Name = '' then name := '<unnamed>' else name := control.Name;
          flags := '';
          if control = Screen.ActiveControl then flags := 'ControlActive'
          else if control = Screen.ActiveForm then flags := flags + IfThen(flags='', '', ', ')+'FormActive';
          ctrls.Add(StringOfChar(' ', depth*2)+name+': '+control.ClassName+caption+IfThen(flags='', '', ' ('+flags+')'));
        except
          on E:Exception do
          try
            ctrls.Add(StringOfChar(' ', depth*2)+ E.Message);
          except
            ;
          end;
        end;
        try
          for i := 0 to control.ControlCount - 1 do
            if control.Controls[i] is TWinControl then
              EnumerateControls(control.Controls[i] as TWinControl, depth+1);
        except
          on E:Exception do
            ctrls.Add(E.Message);
        end;
      end;
begin

  { show our custom error dialog }

  ExceptionDetail := TStringList.Create;
  ctrls := TStringList.Create;
  try
    if ExceptionObject is EDOMParseError then
      with ExceptionObject as EDOMParseError do
      begin
        ExceptionDetail.Add('');
        ExceptionDetail.Add('ErrorCode: '+IntToStr(ErrorCode));
        ExceptionDetail.Add('URL:       '+URL);
        ExceptionDetail.Add('Reason:    '+Reason);
        ExceptionDetail.Add('SrcText:   '+SrcText);
        ExceptionDetail.Add('Line:      '+IntToStr(Line));
        ExceptionDetail.Add('LinePos:   '+IntToStr(LinePos));
        ExceptionDetail.Add('FilePos:   '+IntToStr(FilePos));
      end;

    ctrls.Add('');
    ctrls.Add('CONTROLS');
    ctrls.Add('========');
    try
      for i := 0 to Screen.FormCount - 1 do
        EnumerateControls(Screen.Forms[i], 0);
    except
      ;
    end;

    { Windows Version and Service Pack }

    // I1127 - add system info

    FillChar(vi, sizeof(TOSVersionInfo), 0);
    vi.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
    GetVersionEx(vi);

    ctrls.Add('');
    ctrls.Add('SYSTEM INFO');
    ctrls.Add('===========');
    ctrls.Add('Major Version: '+IntToStr(vi.dwMajorVersion));
    ctrls.Add('Minor Version: '+IntToStr(vi.dwMajorVersion));
    ctrls.Add('Build Number:  '+IntToStr(vi.dwBuildNumber));
    ctrls.Add('Platform ID:   '+IntToStr(vi.dwPlatformId));
    ctrls.Add('CSDVersion:    '+vi.szCSDVersion);

    try
      i := GetLogicalDriveStrings(1024, buf);
      if (i > 0) and (i < 1024) then
      begin
        p := buf;
        while p^ <> #0 do
        begin
          if (GetDriveType(p) = DRIVE_FIXED) and GetDiskFreeSpaceEx(p, freespace, totalspace, @totalfree) then
          begin
            s := p;
            ctrls.Add('Drive '+s+' QuotaFreeSpace: '+IntToStr(freespace div 1024 div 1024)+'MB');
            ctrls.Add('Drive '+s+' TotalSpace:     '+IntToStr(totalspace div 1024 div 1024)+'MB');
            ctrls.Add('Drive '+s+' TotalFreeSpace: '+IntToStr(totalfree div 1024 div 1024)+'MB');
          end;
          p := StrScan(p, #0); Inc(p);
        end;
      end;
    except
      ;
    end;

    { Memory }

    GlobalMemoryStatus(stat);

    ctrls.Add('Memory Load: '+IntToStr(stat.dwMemoryLoad));
    ctrls.Add('Memory TotalPhysical: ' + IntToStr(stat.dwTotalPhys div 1024 div 1024)+'MB');
    ctrls.Add('Memory FreePhysical:  ' + IntToStr(stat.dwAvailPhys div 1024 div 1024)+'MB');
    ctrls.Add('Memory TotalPageFile: ' + IntToStr(stat.dwTotalPageFile div 1024 div 1024)+'MB');
    ctrls.Add('Memory FreePageFile:  ' + IntToStr(stat.dwAvailPageFile div 1024 div 1024)+'MB');
    ctrls.Add('Memory TotalVirtual:  ' + IntToStr(stat.dwTotalVirtual div 1024 div 1024)+'MB');
    ctrls.Add('Memory FreeVirtual:   ' + IntToStr(stat.dwAvailVirtual div 1024 div 1024)+'MB');

    EventLogMessage(ExtractFileName(ParamStr(0)),
      ExceptionMessage + ExceptionDetail.Text + #13#10 + CallStack.Text + #13#10 + ctrls.Text,
      1, 1, 1);  // I1312 - report errors in application event log

    if ApplicationTerminated then
      ExitProcess(200);

    ApplicationTerminated := True;

    LogExceptionToExternalHandler(
      ExtractFileName(ParamStr(0))+'_'+GetVersionString+'_'+IntToHex(Integer(ExceptAddr), 8)+'_'+ExceptionObject.ClassName,
      ExceptionMessage,
      ExceptionDetail.Text,
      CallStack.Text + #13#10 + ctrls.Text);

  finally
    ExceptionDetail.Free;
    ctrls.Free;
  end;

  Application.Terminate;
  Application.ShowMainForm := False;
end;

procedure LogExceptionToExternalHandler(
  CrashID, Message, Detail, Log: string);
const
  CSIDL_PROGRAM_FILES_COMMON = $2B;
var
  FLogFile: string;
begin
  with TStringList.Create do
  try
    Add('[System]');
    Add('Title='+Application.Title);
    Add('CrashID='+CrashID);
    Add('Application='+ExtractFileName(ParamStr(0)));
    Add('ProductID=1');
    Add('Version='+GetVersionString);

    Add('[Message]');
    Add(Message);

    Add('[Detail]');
    Add(Detail);

    Add('[Log]');
    Add(Log);

    FLogFile := GetErrLogFileName('exception');   // I3671

    SaveToFile(FLogFile, TEncoding.UTF8);  // I3337

    if not TUtilExecute.Shell(0, GetFolderPath(CSIDL_PROGRAM_FILES_COMMON) + SFolderKeymanEngine + '\tsysinfo.exe',  // I3349
      GetFolderPath(CSIDL_PROGRAM_FILES_COMMON) + SFolderKeymanEngine, '-c "'+FLogFile+'"') then
    begin
      MessageDlg(Application.Title+' has had a fatal error.  An additional error was encountered starting the exception manager ('+SysErrorMessage(GetLastError)+').  '+
        #13#10'Error log is stored in '#13#10#13#10+'  '+FLogFile+#13#10#13#10+
        message+#13#10+
        detail+#13#10+
        'Please send this information to Keyman Support',
        mtError, [mbOK], 0);
    end;
  finally
    Free;
  end;
end;

initialization
  ForceDirectories(GetErrLogPath);
finalization
end.
