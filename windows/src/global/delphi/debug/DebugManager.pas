(*
  Name:             DebugManager
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Sep 2006

  Modified Date:    9 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          14 Sep 2006 - mcdurdin - Initial version
                    04 Jan 2007 - mcdurdin - Add ShouldDebug function
                    19 Jun 2007 - mcdurdin - Widestring it
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    04 May 2010 - mcdurdin - I2348 - Rework columns in debug log
                    04 May 2010 - mcdurdin - I2349 - Hotkey to capture debug log
                    04 May 2010 - mcdurdin - I2350 - Keyman.exe should be able to report events in debug log
                    04 May 2010 - mcdurdin - I2352 - Debug logging not reliable in some apps due to security
                    15 Jun 2010 - mcdurdin - I2423 - Fix crash in debug manager
                    24 Jun 2010 - mcdurdin - I2422 - Recreate, not rewrite existing log files
                    29 Jun 2010 - mcdurdin - I2446 - Keyman Engine crashes due to debug manager being freed late
                    17 Dec 2010 - mcdurdin - Add extra column to raw strings
                    11 Jan 2011 - mcdurdin - I2640 - Raise error at appropriate location so it isn't masked by cascading issue
                    31 Jan 2011 - mcdurdin - I2685 - Reduce registry noise from ShouldDebug function
                    31 Jan 2011 - mcdurdin - I2690 - Add foreground window info to debug log
                    31 Jan 2011 - mcdurdin - I2691 - Fix handle leaks
                    18 Feb 2011 - mcdurdin - I2685 : 9827 - Fix shoulddebug refresh counter not resetting
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    28 Nov 2012 - mcdurdin - I3601 - V9.0 - Debug messages were written partially Unicode, partially ANSI
                    13 Dec 2012 - mcdurdin - I3656 - V9.0 - Debug logs show too many columns for host messages
                    01 Jan 2013 - mcdurdin - I3692 - V9.0 - [host] entries in system.log out by 1 tabstop
                    09 Aug 2015 - mcdurdin - I4843 - Log reported modifier state as well as Keyman current modifier state
*)
unit DebugManager;  // I3306

interface

uses
  Windows,
  Classes,
  SysUtils,
  UserMessages;

type
  TDebugManager = class(TThread)
  private
    FOwner: HWND;
    hLogFile, hMailSlot: THandle;
    hEvent: THandle;
    FDebugLogIndex: Integer;
    procedure WriteHeadingString;
    procedure WriteMessage(buf: PAnsiChar; buflen: Integer); overload;  // I3310
    procedure StartNewLogFile;
    procedure FindFirstLogFileName;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: HWND); reintroduce;
    destructor Destroy; override;

    class procedure WriteLastError(const ParentMethod, FailingMethod: string; const Message: string = '');
    class procedure WriteMessage(const Format: string; const Args: array of const); overload;

    class procedure CloseLogFile(var Handle: THandle);
    class procedure WriteString(Handle: THandle; const s: string);
    class procedure WriteMessage(Handle: THandle; buf: PAnsiChar; buflen: Integer); overload;  // I3310
    class procedure WriteRawString(Handle: THandle; const s: string); static;

    class function DebugLogFileName(n: Integer): WideString;
  end;

function GetDebugManager(AOwner: HWND): TDebugManager;

implementation

uses
  Accctrl,
  AclApi,
  ErrorControlledRegistry,
  RegistryKeys,
  SystemDebugPath,
  Unicode;

var
  FDebugManager: TDebugManager = nil;
  FTerminating: Boolean = False;

  FShouldDebug: Boolean = False;
  FShouldDebugLastTick: Cardinal = 0;

function ShouldDebug: Boolean;
var
  t1: Cardinal;
begin
  t1 := GetTickCount;
  if (t1 < FShouldDebugLastTick) or (t1 - FShouldDebugLastTick > 5000) then  // I2685
  begin
    with TRegistryErrorControlled.Create do  // I2890
    try
      if OpenKeyReadOnly(SRegKey_KeymanEngine) and ValueExists(SRegValue_KeymanDebug)
        then FShouldDebug := ReadBool(SRegValue_KeymanDebug)
        else FShouldDebug := False;
    finally
      Free;
    end;
    FShouldDebugLastTick := GetTickCount; // I2685:9827
  end;

  Result := FShouldDebug;
end;

function GetDebugManager(AOwner: HWND): TDebugManager;
begin
  Assert(not FTerminating);

  if ShouldDebug then
  begin
    if not Assigned(FDebugManager) then
      FDebugManager := TDebugManager.Create(AOwner);
    Result := FDebugManager;
  end
  else
    Result := nil;
end;

{ TDebugManager }

class procedure TDebugManager.CloseLogFile(var Handle: THandle);
begin
  if Handle <> 0 then
  begin
    WriteString(Handle, '---- Log file closed '+FormatDateTime('C', now) + '----');
    CloseHandle(Handle);
    Handle := 0;
  end;
end;


const LOW_INTEGRITY_SDDL_SACL_W: WideString = 'S:(ML;;NW;;;LW)';
const LABEL_SECURITY_INFORMATION = $00000010;
const SDDL_REVISION_1 = 1;

function ConvertStringSecurityDescriptorToSecurityDescriptor(
    {IN} StringSecurityDescriptor: LPCWSTR;
    {IN} StringSDRevision: DWORD;
    {OUT} var SecurityDescriptor: PSECURITY_DESCRIPTOR;
    {OUT} SecurityDescriptorSize: PULONG {OPTIONAL}
    ): BOOL; stdcall; external 'advapi32.dll' name 'ConvertStringSecurityDescriptorToSecurityDescriptorW';

function SetObjectToLowIntegrity(hObject: THandle; _type: SE_OBJECT_TYPE = SE_KERNEL_OBJECT): BOOL;
var
  dwErr: DWORD;
  pSD: PSECURITY_DESCRIPTOR;
  pSacl: PACL;
  fSaclPresent: BOOL;
  fSaclDefaulted: BOOL;
begin
  //BOOL bRet = FALSE;
  //dwErr := ERROR_SUCCESS;
  Result := False;
  
  pSD := nil;
  pSacl := nil;
  fSaclPresent := FALSE;
  fSaclDefaulted := FALSE;

  if LOBYTE(LOWORD(GetVersion())) >= 6 then
  begin
    if ConvertStringSecurityDescriptorToSecurityDescriptor(PWideChar(LOW_INTEGRITY_SDDL_SACL_W), SDDL_REVISION_1, pSD, nil) then
    begin
      if GetSecurityDescriptorSacl(pSD, fSaclPresent, pSacl, fSaclDefaulted) then
      begin
        dwErr := SetSecurityInfo(
                hObject, _type, LABEL_SECURITY_INFORMATION,
                nil, nil, nil, pSacl);
        Result := ERROR_SUCCESS = dwErr;
      end;
    end;

    LocalFree(Cardinal(pSD));
  end
  else
    Result := True;
end;

constructor TDebugManager.Create(AOwner: HWND);
const
  lpszSlotName: string = '\\.\mailslot\tavultesoft_keymanengine_debug';
var
  pSD: PSECURITY_DESCRIPTOR;
  //sd: SECURITY_DESCRIPTOR;
  sa: SECURITY_ATTRIBUTES;
  //bSaclPresent: LongBool;
  //pSacl: PACL;
  //bSaclDefaulted: LongBool;
  //fSaclPresent: LongBool;
begin
  FOwner := AOwner;
  
  if LOBYTE(LOWORD(GetVersion())) >= 6 then
  begin
    if ConvertStringSecurityDescriptorToSecurityDescriptor(PWideChar(LOW_INTEGRITY_SDDL_SACL_W), SDDL_REVISION_1, pSD, nil) then
    begin
      sa.nLength := sizeof(sa);
      sa.lpSecurityDescriptor := pSD;
      sa.bInheritHandle := FALSE;

      hMailSlot := CreateMailslot(PChar(lpszSlotName),
        0,          // no maximum message size
        10,         // 10 msec time-out for operations
        @sa);
      if hMailSlot = INVALID_HANDLE_VALUE then RaiseLastOSError;  // I2640 - Raise error at appropriate location so it isn't masked by cascading issue

      // For some reason the mailslot does not have full access for everyone so we need to set it afterwards anyway!
      if SetSecurityInfo(hMailSlot, SE_KERNEL_OBJECT, DACL_SECURITY_INFORMATION, nil, nil, nil, nil) <> ERROR_SUCCESS then
        RaiseLastOSError;

      LocalFree(Cardinal(pSD));
    end
    else RaiseLastOSError;
  end
  else
  begin
    hMailSlot := CreateMailslot(PChar(lpszSlotName),
      0,          // no maximum message size
      10,         // 10 msec time-out for operations
      nil);
    if hMailSlot = INVALID_HANDLE_VALUE then RaiseLastOSError;
  end;

  try
    FindFirstLogFileName;
    StartNewLogFile;

    try
      hEvent := CreateEvent(nil, FALSE, FALSE, 'Tavultesoft_KeymanEngine_Debug');
      if hEvent = 0 then RaiseLastOSError;
      if not SetObjectToLowIntegrity(hEvent) then RaiseLastOSError;

      try
        SetFilePointer(hLogFile, 0, nil, FILE_END);

        FreeOnTerminate := False;   // I2446 - Keyman Engine crashes due to debug manager being freed late
        inherited Create(False);
      except
        on E:Exception do
        begin
          CloseHandle(hEvent);
          hEvent := 0;
          raise;
        end;
      end;
    except
      on E:Exception do
      begin
        CloseHandle(hLogFile);
        hLogFile := 0;
        raise;
      end;
    end;
  except
    on E:Exception do
    begin
      CloseHandle(hMailSlot);
      hMailSlot := 0;
      raise;
    end;
  end;
end;

destructor TDebugManager.Destroy;
begin
  CloseLogFile(hLogFile);
  if hMailSlot <> 0 then CloseHandle(hMailSlot);
  if hEvent <> 0 then CloseHandle(hEvent);
  inherited Destroy;
end;

const
  MAXDEBUGLOGS = 16;

class function TDebugManager.DebugLogFileName(n: Integer): WideString;
begin
  Result := GetSystemDebugPath + 'system' + IntToStr(n) + '.log';
end;

procedure TDebugManager.FindFirstLogFileName;
var
  FSelectedTime: TDateTime;
  i: Integer;
  f: TSearchRec;
begin
  FSelectedTime := MaxDateTime;
  FDebugLogIndex := 0;

  for i := 0 to MAXDEBUGLOGS - 1 do
  begin
    if FindFirst(DebugLogFileName(i), 0, f) = 0 then
    begin
      if f.TimeStamp < FSelectedTime then
      begin
        FDebugLogIndex := i;
        FSelectedTime := f.TimeStamp;
      end;
      FindClose(f);
    end
    else
    begin
      FDebugLogIndex := i;
      Break;
    end;
  end;

  Dec(FDebugLogIndex); // It will be immediately incremented by StartNewLogFile
end;

procedure TDebugManager.StartNewLogFile;
var
  FDebugLogFileName: WideString;
begin
  if hLogFile <> 0 then
    CloseHandle(hLogFile);

  Inc(FDebugLogIndex);
  if FDebugLogIndex > MAXDEBUGLOGS then
    FDebugLogIndex := 0;

  FDebugLogFileName := 'system' + IntToStr(FDebugLogIndex) + '.log';

  hLogFile := CreateFile(PChar(DebugLogFileName(FDebugLogIndex)), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ, nil, CREATE_ALWAYS, 0, 0);
  if hLogFile = INVALID_HANDLE_VALUE then RaiseLastOSError;

  WriteHeadingString;
end;

procedure TDebugManager.Execute;
var
  szBuffer, cbMessage, cMessage, cbRead: DWord;
  lpszBuffer: PAnsiChar;  // I3310
  ov: TOverlapped;
begin
  //
  try
    cbMessage := 0;
    cMessage := 0;
    cbRead := 0;
    szBuffer := 0;
    lpszBuffer := nil;

    ov.Offset := 0;
    ov.OffsetHigh := 0;
    ov.hEvent := hEvent;

    try
      repeat
        if not GetMailslotInfo(hMailSlot, // mailslot handle
            nil,               // no maximum message size
            cbMessage,        // size of next message
            @cMessage,         // number of messages
            nil) then          // no read time-out
          RaiseLastOSError;

        if cbMessage = MAILSLOT_NO_MESSAGE then { Sleep 10 msec }
        begin
          Sleep(10);
          Continue;
        end;

        while (cMessage > 0) and not Terminated do  // retrieve all messages
        begin
          if szBuffer < cbMessage then
          begin
            if Assigned(lpszBuffer) then FreeMem(lpszBuffer);
            lpszBuffer := AllocMem(cbMessage);
          end;
          if not Assigned(lpszBuffer) then
            raise Exception.Create('Invalid buffer');

          lpszBuffer^ := #0;

          if not ReadFile(hMailSlot,
              lpszBuffer^,
              cbMessage,
              cbRead,
              @ov) then
            RaiseLastOSError;

          WriteMessage(lpszBuffer, cbRead - 1); // Ignore terminating nul

          // Concatenate the message and the message-number string.

          if not GetMailslotInfo(hMailSlot, // mailslot handle
              nil,               // no maximum message size
              cbMessage,        // size of next message
              @cMessage,         // number of messages
              nil) then          // no read time-out
            RaiseLastOSError;
        end;
      until Terminated;
    finally
      if Assigned(lpszBuffer) then FreeMem(lpszBuffer);
    end;
  except
    on E:Exception do
      WriteString(hLogFile, '*** Exception '+E.ClassName+' in TDebugManager: '+E.Message);
  end;
end;

procedure TDebugManager.WriteHeadingString;
begin
  WriteRawString(hLogFile,   // I3656
          'Platform' + #9 +
          'Process' + #9 +
          'PID' + #9 +
          'TID' + #9 +
          //'QueueStatus' + #9 +
          'ShiftState' + #9 +
          'ActualShiftState' + #9 +   // I4843
          'TickCount' + #9 +
          //'LogHWND' + #9 +
          //'LogWindowClassName' + #9 +
          //'LogWindowText' + #9 +
          'FocusHWND' + #9 +
          //'FocusWindowClassName' + #9 +
          //'FocusWindowText' + #9 +
          //'ActiveHWND' + #9 +
          //'ActiveWindowClassName' + #9 +
          //'ActiveWindowText' + #9 +
          //'ForegroundHWND' + #9 +  // I2690
          //'MessageType' + #9 +
          //'SourceLine' + #9 +
          'ActiveHKL' + #9 +
          'Message');
  WriteString(hLogFile, '---- Log file opened '+FormatDateTime('C', now) + '----');
end;

var
  hLogMailSlot: THandle = 0;
  hLogEvent: THandle = 0;

class procedure TDebugManager.WriteLastError(const ParentMethod, FailingMethod: string; const Message: string = '');
var
  FLastError: DWord;
  FLastErrorString: string;
begin
  FLastError := GetLastError;
  FLastErrorString := SysErrorMessage(GetLastError);
  WriteMessage('ERROR %d in %s [%s]: %s %s', [FLastError, FailingMethod, ParentMethod, FLastErrorString, Message]);
end;

class procedure TDebugManager.WriteMessage(const Format: string;
  const Args: array of const);
var
  bufa: ansistring;
  buf: string;
  ov: OVERLAPPED;
  cbWritten: DWORD;
  gti: TGUIThreadInfo;
begin
  if not ShouldDebug then Exit;

  if hLogMailSlot = 0 then
  begin
		hLogMailSlot := CreateFile('\\.\mailslot\Tavultesoft_KeymanEngine_Debug',
				GENERIC_WRITE,
				FILE_SHARE_READ or FILE_SHARE_WRITE,  // required to write to a mailslot
				nil,
				OPEN_EXISTING,
				FILE_FLAG_OVERLAPPED or FILE_ATTRIBUTE_NORMAL,
				0);

		hLogEvent := CreateEvent(nil, False, False, 'Tavultesoft_KeymanEngine_DebugWrite');

    if hLogMailSlot = 0 then Exit;
    if hLogEvent = 0 then Exit;
  end;

  gti.cbSize := SizeOf(TGUIThreadInfo);  // I2690
  GetGUIThreadInfo(0, gti);

  buf :=
    SysUtils.Format(   // I3656
      'x86' + #9 +
      'keyman[host]' + #9 +
      '%x' + #9 +
      '%x' + #9 +
      //'' + #9 +
      '' + #9 + // shift state
      '' + #9 + // actual shift state   // I4843
      IntToStr(GetTickCount) + #9 +
      //'' + #9 +
      //'' + #9 +
      //'' + #9 +
      '%x' + #9 +
      //'' + #9 +
      //'' + #9 +
      //'%x' + #9 +
      //'' + #9 +
      //'' + #9 +
      //'%x' + #9 +

      //'' + #9 +
      //'' + #9 +   // I3692
      '' + #9,
      [GetCurrentProcessId,
      GetCurrentThreadId,
      gti.hwndFocus
      //gti.hwndActive,
      {GetForegroundWindow}]) +
    SysUtils.Format(Format, Args);

  ov.Offset := 0;
  ov.OffsetHigh := 0;
  ov.hEvent := hLogEvent;

  bufa := String_UtoA(buf);   // I3601

  WriteFile(hLogMailSlot,
    bufa[1],   // I3601
    Length(bufa) + 1,
    cbWritten,
    @ov);
end;

procedure TDebugManager.WriteMessage(buf: PAnsiChar; buflen: Integer);  // I3310
var
  Control: AnsiChar;  // I3310
begin
  if (buflen > 0) then Control := buf[0] else Control := #0;

  if Control = '*' then
  begin
    if FOwner <> 0 then
    begin
      if PostMessage(FOwner, WM_USER_DebugNotify, FDebugLogIndex, hLogFile) then
        hLogFile := 0;
    end;
    StartNewLogFile;
  end
  else
  begin
    WriteMessage(hLogFile, buf, buflen);
  end;
end;

class procedure TDebugManager.WriteString(Handle: THandle; const s: string);
var
  gti: TGUIThreadInfo;
begin
  gti.cbSize := SizeOf(TGUIThreadInfo);
  GetGUIThreadInfo(0, gti);  // I2690

  WriteRawString(Handle,   // I3656
    SysUtils.Format(
      'x86' + #9 +
      'keyman[host]' + #9 +
      '%x' + #9 +
      '%x' + #9 +
      //'' + #9 +
      '' + #9 + // shift state
      '' + #9 + // actual shift state   // I4843
      IntToStr(GetTickCount) + #9 +
      //'' + #9 +
      //'' + #9 +
      //'' + #9 +
      '%x' + #9 +
      //'' + #9 +
      //'' + #9 +
      //'%x' + #9 +
      //'' + #9 +
      //'' + #9 +
      //'%x' + #9 +

      //'' + #9 +
      //'' + #9 +   // I3692
      '' + #9,
      [GetCurrentProcessId,
      GetCurrentThreadId,
      gti.hwndFocus {,
      gti.hwndActive,
      GetForegroundWindow}]) +
    s);
end;

class procedure TDebugManager.WriteRawString(Handle: THandle; const s: string);
var
  sa: AnsiString;  // I3310
begin
  sa := String_UtoA(s);  // I3310
  WriteMessage(Handle, PAnsiChar(sa), Length(sa));
end;

class procedure TDebugManager.WriteMessage(Handle: THandle; buf: PAnsiChar; buflen: Integer);  // I3310
var
  n: DWord;
const
  crlf: AnsiString = #13#10;
begin
  if Handle = 0 then Exit;

  WriteFile(Handle, buf^, buflen, n, nil);
  WriteFile(Handle, PAnsiChar(crlf)^, 2, n, nil);   // I3601
end;

procedure CloseHandles;  // I2691
begin
  if hLogEvent <> 0 then CloseHandle(hLogEvent);
  if hLogMailSlot <> 0 then CloseHandle(hLogMailSlot);
  hLogMailSlot := 0;
  hLogEvent := 0;
end;

initialization
finalization
  FTerminating := True;
  if Assigned(FDebugManager) then // I2446 - Keyman Engine crashes due to debug manager being freed late
    FDebugManager.Terminate;
  FreeAndNil(FDebugManager);
  CloseHandles;  // I2691
end.
