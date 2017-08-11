{ I got Permission to add the SendMail component and to change the code
   by my needs from Mike Shkolnik
   We thank him for that.
   You can find Mike's full components package at http://www.scalabium.com.
   bsalsa

Copyright (C) 1998-2004, written by Mike Shkolnik, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
  WEB: http://www.scalabium.com
  tel: 380-/44/-552-10-29
}

unit SendMail_For_Ewb;

interface

{$I EWB.inc}

uses
  Classes, Dialogs, Controls, Messages, MAPI;

type
  TEwbMapiMail = class;

  TSendMailThread = class(TThread)
  private
    EwbMapiMail: TEwbMapiMail;
  public
    procedure Execute; override;
    constructor Create(AMapiMail: TEwbMapiMail);
  end;

  TEwbMapiMail = class(TComponent)
  private
    { Private declarations }
    FLastError: Integer;

    FSubject: string;
    FBody: string;

    FSenderName: string;
    FSenderAddress: string;

    FRecipients: TStrings;
    FAttachments: TStrings;
    FAttachmentNames: TStrings;

    FEditDialog: Boolean;
    FResolveNames: Boolean;
    FRequestReceipt: Boolean;

    procedure SetRecipients(Value: TStrings);
    procedure SetAttachments(Value: TStrings);
    procedure SetAttachmentNames(Value: TStrings);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Send: Boolean;

    property LastError: Integer read FLastError;
  published
    { Published declarations }
    property Subject: string read FSubject write FSubject;
    property Body: string read FBody write FBody;

    property Recipients: TStrings read FRecipients write SetRecipients;
    property Attachments: TStrings read FAttachments write SetAttachments;
    property AttachmentNames: TStrings read FAttachmentNames write
      SetAttachmentNames;

    property EditDialog: Boolean read FEditDialog write FEditDialog;
    property ResolveNames: Boolean read FResolveNames write FResolveNames;
    property RequestReceipt: Boolean read FRequestReceipt write FRequestReceipt;

    property SenderName: string read FSenderName write FSenderName;
    property SenderAddress: string read FSenderAddress write FSenderAddress;
  end;

{$IFDEF UNICODE}
function SendEMailByMAPI(SenderName, SenderAddress, Subject, Body: string;
  Recipients, Attachments, AttachmentNames: TStrings; WithOpenMessage,
  ResolveNames, NeedReceipt: Boolean; intMAPISession:
  Integer): Integer; overload;
{$ENDIF UNICODE}
function SendEMailByMAPI(SenderName, SenderAddress, Subject, Body: Ansistring;
  Recipients, Attachments, AttachmentNames: TStrings; WithOpenMessage,
  ResolveNames, NeedReceipt: Boolean; intMAPISession:
  Integer): Integer; overload;

function MAPIErrorDescription(intErrorCode: Integer): string;

implementation

uses
{$IFDEF UNICODE}
  AnsiStrings,
{$ENDIF UNICODE}
  Windows, SysUtils, Registry, Forms;

function MAPIErrorDescription(intErrorCode: Integer): string;
begin
  case intErrorCode of
    MAPI_E_USER_ABORT: Result := 'User cancelled request';
    MAPI_E_FAILURE: Result := 'General MAPI failure';
    MAPI_E_LOGON_FAILURE: Result := 'Logon failure';
    MAPI_E_DISK_FULL: Result := 'Disk full';
    MAPI_E_INSUFFICIENT_MEMORY: Result := 'Insufficient memory';
    MAPI_E_ACCESS_DENIED: Result := 'Access denied';
    MAPI_E_TOO_MANY_SESSIONS: Result := 'Too many sessions';
    MAPI_E_TOO_MANY_FILES: Result := 'Too many files open';
    MAPI_E_TOO_MANY_RECIPIENTS: Result := 'Too many recipients';
    MAPI_E_ATTACHMENT_NOT_FOUND: Result := 'Attachment not found';
    MAPI_E_ATTACHMENT_OPEN_FAILURE: Result := 'Failed to open attachment';
    MAPI_E_ATTACHMENT_WRITE_FAILURE: Result := 'Failed to write attachment';
    MAPI_E_UNKNOWN_RECIPIENT: Result := 'Unknown recipient';
    MAPI_E_BAD_RECIPTYPE: Result := 'Invalid recipient type';
    MAPI_E_NO_MESSAGES: Result := 'No messages';
    MAPI_E_INVALID_MESSAGE: Result := 'Invalid message';
    MAPI_E_TEXT_TOO_LARGE: Result := 'Text too large.';
    MAPI_E_INVALID_SESSION: Result := 'Invalid session';
    MAPI_E_TYPE_NOT_SUPPORTED: Result := 'Type not supported';
    MAPI_E_AMBIGUOUS_RECIPIENT: Result := 'Ambiguous recipient';
    MAPI_E_MESSAGE_IN_USE: Result := 'Message in use';
    MAPI_E_NETWORK_FAILURE: Result := 'Network failure';
    MAPI_E_INVALID_EDITFIELDS: Result := 'Invalid edit fields';
    MAPI_E_INVALID_RECIPS: Result := 'Invalid recipients';
    MAPI_E_NOT_SUPPORTED: Result := 'Not supported';
  else
    Result := 'Unknown Error Code: ' + IntToStr(intErrorCode);
  end;
end;

function GetDefaultLogon(var strDefaultLogon: AnsiString): Boolean;
const
  KEYNAME1 = 'Software\Microsoft\Windows Messaging Subsystem\Profiles';
  KEYNAME2 =
    'Software\Microsoft\Windows NT\CurrentVersion\Windows Messaging Subsystem\Profiles';
  VALUESTR = 'DefaultProfile';
begin
  Result := False;
  strDefaultLogon := '';
  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKey(KEYNAME1, False) then
    begin
      try
        strDefaultLogon := AnsiString(ReadString(VALUESTR));
        Result := True;
      except
      end;
      CloseKey;
    end
    else if OpenKey(KEYNAME2, False) then
    begin
      try
        strDefaultLogon := AnsiString(ReadString(VALUESTR));
        Result := True;
      except
      end;
      CloseKey;
    end
    else
  finally
    Free;
  end;
end;

function SendEMailByMAPI(SenderName, SenderAddress, Subject, Body: AnsiString;
  Recipients, Attachments, AttachmentNames: TStrings; WithOpenMessage,
  ResolveNames, NeedReceipt: Boolean; intMAPISession:
  Integer): Integer;
const
  RECIP_MAX = MaxInt div SizeOf(TMapiRecipDesc);
  ATTACH_MAX = MaxInt div SizeOf(TMapiFileDesc);
type
  TRecipAccessArray = array[0..(RECIP_MAX - 1)] of TMapiRecipDesc;
  TlpRecipArray = ^TRecipAccessArray;

  TAttachAccessArray = array[0..(ATTACH_MAX - 1)] of TMapiFileDesc;
  TlpAttachArray = ^TAttachAccessArray;

  TszRecipName = array[0..256] of AnsiChar;
  TlpszRecipName = ^TszRecipName;

  TszPathName = array[0..256] of AnsiChar;
  TlpszPathname = ^TszPathname;

  TszFileName = array[0..256] of AnsiChar;
  TlpszFileName = ^TszFileName;

var
  i: Integer;

  Message: TMapiMessage;
  lpRecipArray: TlpRecipArray;
  lpAttachArray: TlpAttachArray;

  function CheckRecipient(strRecipient: Ansistring): Integer;
  var
    lpRecip: PMapiRecipDesc;
  begin
    try
      Result := MapiResolveName(0, 0, PAnsiChar(strRecipient), 0, 0, lpRecip);

      if (Result in [MAPI_E_AMBIGUOUS_RECIPIENT,
        MAPI_E_UNKNOWN_RECIPIENT]) then
        Result := MapiResolveName(0, 0, PAnsiChar(strRecipient), MAPI_DIALOG, 0,
          lpRecip);

      AnsiStrings.StrCopy(PAnsiChar(new(TlpszRecipName)), lpRecip^.lpszName);

      if Result = SUCCESS_SUCCESS then
      begin
        strRecipient := AnsiStrings.StrPas(lpRecip^.lpszName);
        with lpRecipArray^[i] do
        begin
          if lpRecip^.lpszAddress = nil then
          begin
            lpszAddress := AnsiStrings.StrCopy(new(TlpszRecipName)^,
              lpRecip^.lpszName);
          end
          else
          begin
            lpszAddress := AnsiStrings.StrCopy(new(TlpszRecipName)^,
              lpRecip^.lpszAddress);
          end;
          ulEIDSize := lpRecip^.ulEIDSize;
          lpEntryID := lpRecip^.lpEntryID;
          MapiFreeBuffer(lpRecip);
        end
      end;
    finally
    end;
  end;

  function SendMess: Integer;
  const
    arrMAPIFlag: array[Boolean] of Word = (0, MAPI_DIALOG);
    arrReceipt: array[Boolean] of Word = (0, MAPI_RECEIPT_REQUESTED);
    arrLogon: array[Boolean] of Word = (0, MAPI_LOGON_UI or MAPI_NEW_SESSION);
  begin
    try
      Result := MAPISendMail(0, Application.Handle {0}, Message,
        arrReceipt[NeedReceipt] or
        arrMAPIFlag[WithOpenMessage] or
        MAPI_LOGON_UI {or MAPI_NEW_SESSION} or
        arrLogon[{True}intMAPISession = 0],
        0);
    finally
    end;
  end;

var
  lpSender: TMapiRecipDesc;
  strDefaultProfile, s: AnsiString;
begin
  Result := 0;
  lpRecipArray := nil;
  lpAttachArray := nil;
  FillChar(Message, SizeOf(Message), 0);
  with Message do
  begin
    strDefaultProfile := '';
    if GetDefaultLogon(strDefaultProfile) then
    begin
      try
            { try to logon with default profile }
        Result := MapiLogOn(0, PAnsiChar(strDefaultProfile), nil,
          MAPI_NEW_SESSION, 0, @intMAPISession);
      finally
        if (Result <> SUCCESS_SUCCESS) then
        begin
          intMAPISession := 0;
          raise Exception.CreateFmt('MAPI Error %d: %s', [Result,
            MAPIErrorDescription(Result)]);
        end;
      end
    end;

    if (SenderAddress <> '') then
    begin
      lpSender.ulRecipClass := MAPI_ORIG;
      if (SenderName <> '') then
        lpSender.lpszName := PAnsiChar(SenderAddress)
      else
        lpSender.lpszName := PAnsiChar(SenderName);
      lpSender.lpszAddress := PAnsiChar(SenderAddress);
      lpSender.ulReserved := 0;
      lpSender.ulEIDSize := 0;
      lpSender.lpEntryID := nil;
      lpOriginator := @lpSender;
    end;

    lpszSubject := PAnsiChar(Subject);
    lpszNoteText := PAnsiChar(Body);

    if Assigned(Attachments) and (Attachments.Count > 0) then
    begin
      nFileCount := Attachments.Count;

{$IFDEF UNICODE}
      lpAttachArray := TlpAttachArray(AnsiStrings.AnsiStrAlloc(nFileCount *
        SizeOf(TMapiFileDesc)));
{$ELSE}
      lpAttachArray := TlpAttachArray(StrAlloc(nFileCount *
        SizeOf(TMapiFileDesc)));
{$ENDIF UNICODE}
      FillChar(lpAttachArray^, AnsiStrings.StrBufSize(PAnsiChar(lpAttachArray)), 0);
      for i := 0 to nFileCount - 1 do
      begin
        lpAttachArray^[i].nPosition := Cardinal(-1);
              //Cardinal($FFFFFFFF); //ULONG(-1);
        lpAttachArray^[i].lpszPathName := AnsiStrings.StrPCopy(new(TlpszPathname)^,
          AnsiString(Attachments[i]));
        if i < AttachmentNames.Count then
        begin
          lpAttachArray^[i].lpszFileName :=
            AnsiStrings.StrPCopy(new(TlpszFileName)^,
            AnsiString(AttachmentNames[i]))
        end
        else
        begin
          lpAttachArray^[i].lpszFileName :=
            AnsiStrings.StrPCopy(new(TlpszFileName)^,
            AnsiString(ExtractFileName(Attachments[i])));
        end;
      end;
      lpFiles := @lpAttachArray^
    end
    else
      nFileCount := 0;
  end;

  if Assigned(Recipients) and (Recipients.Count > 0) then
  begin
{$IFDEF UNICODE}
    lpRecipArray := TlpRecipArray(AnsiStrings.AnsiStrAlloc(Recipients.Count *
      SizeOf(TMapiRecipDesc)));
{$ELSE}
    lpRecipArray := TlpRecipArray(StrAlloc(Recipients.Count *
      SizeOf(TMapiRecipDesc)));
{$ENDIF UNICODE}
    FillChar(lpRecipArray^, AnsiStrings.StrBufSize(PAnsiChar(lpRecipArray)), 0);
    for i := 0 to Recipients.Count - 1 do
    begin
      s := AnsiString(Recipients[i]);
      if (UpperCase(Copy(s, 1, 3)) = 'CC:') then
      begin
        lpRecipArray^[i].ulRecipClass := MAPI_CC;
        Delete(s, 1, 3);
      end
      else if (UpperCase(Copy(s, 1, 4)) = 'BCC:') then
      begin
        lpRecipArray^[i].ulRecipClass := MAPI_BCC;
        Delete(s, 1, 4);
      end
      else
        lpRecipArray^[i].ulRecipClass := MAPI_TO;

      if ResolveNames then
        CheckRecipient(s)
      else
      begin
        lpRecipArray^[i].lpszName := AnsiStrings.StrCopy(new(TlpszRecipName)^,
          PAnsiChar(s));
        lpRecipArray^[i].lpszAddress := AnsiStrings.StrCopy(new(TlpszRecipName)^,
          PAnsiChar(s));
      end;
    end;
    Message.nRecipCount := Recipients.Count;
    Message.lpRecips := @lpRecipArray^;
  end
  else
    Message.nRecipCount := 0;

  Result := SendMess;

  if Assigned(Attachments) and (Message.nFileCount > 0) then
  begin
    for i := 0 to Message.nFileCount - 1 do
    begin
      Dispose(lpAttachArray^[i].lpszPathname);
      Dispose(lpAttachArray^[i].lpszFileName);
    end;
    AnsiStrings.StrDispose(PAnsiChar(lpAttachArray));
  end;

  if Assigned(Recipients) and (Recipients.Count > 0) then
  begin
    for i := 0 to Message.nRecipCount - 1 do
    begin
      if Assigned(lpRecipArray^[i].lpszName) then
        Dispose(lpRecipArray^[i].lpszName);

      if Assigned(lpRecipArray^[i].lpszAddress) then
        Dispose(lpRecipArray^[i].lpszAddress);
    end;
    AnsiStrings.StrDispose(PAnsiChar(lpRecipArray));
  end;

  if intMAPISession <> 0 then
  try
    MapiLogOff(intMAPISession, 0, 0, 0);
  except
  end;
end;

{$IFDEF UNICODE}

function SendEMailByMAPI(SenderName, SenderAddress, Subject, Body: string;
  Recipients, Attachments, AttachmentNames: TStrings; WithOpenMessage,
  ResolveNames, NeedReceipt: Boolean; intMAPISession:
  Integer): Integer;
begin
  Result := SendEMailByMAPI(AnsiString(SenderName), AnsiString(SenderAddress),
    AnsiString(Subject),
    AnsiString(Body), Recipients, Attachments, AttachmentNames, WithOpenMessage,
    ResolveNames, NeedReceipt,
    intMAPISession);
end;
{$ENDIF UNICODE}

{ TEwbMapiMail }

constructor TEwbMapiMail.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  EditDialog := True;
  FRecipients := TStringList.Create;
  FAttachments := TStringList.Create;
  FAttachmentNames := TStringList.Create;
end;

destructor TEwbMapiMail.Destroy;
begin
  FRecipients.Free;
  Attachments.Free;
  AttachmentNames.Free;
  inherited Destroy;
end;

procedure TEwbMapiMail.SetRecipients(Value: TStrings);
begin
  FRecipients.Assign(Value)
end;

procedure TEwbMapiMail.SetAttachments(Value: TStrings);
begin
  Attachments.Assign(Value)
end;

procedure TEwbMapiMail.SetAttachmentNames(Value: TStrings);
begin
  AttachmentNames.Assign(Value)
end;

function TEwbMapiMail.Send: Boolean;
var
  SendThread: TSendMailThread;
begin
  SendThread := TSendMailThread.Create(Self);
  SendThread.FreeOnTerminate := True;
  {$IFDEF DELPHI2010_UP}
    SendThread.Start;
  {$ELSE}
    SendThread.Resume;
  {$ENDIF}
  Result := True;
  //  FLastError := SendThread.WaitFor;
  //  FLastError := SendEMailByMAPI(SenderName, SenderAddress, Subject, Body, Recipients, Attachments, AttachmentNames, EditDialog, ResolveNames, RequestReceipt, 0);
  //  if FLastError = MAPI_E_FAILURE then
  //    PostMessage(TWinControl(Owner).Handle, WM_SendMail, 0, 0);
  //  Result := (LastError = SUCCESS_SUCCESS);
end;

{ TSendMailThread }

constructor TSendMailThread.Create(AMapiMail: TEwbMapiMail);
begin
  inherited Create(True);
  EwbMapiMail := AMapiMail;
end;

procedure TSendMailThread.Execute;
const
  WM_SendMail = WM_USER + $100;
var
  i: Integer;
begin
  with EwbMapiMail do
  begin
    try
      ReturnValue := SendEMailByMAPI(SenderName, SenderAddress, Subject, Body,
        Recipients, Attachments, AttachmentNames, EditDialog, ResolveNames,
        RequestReceipt, 0);
      case ReturnValue of
        MAPI_E_FAILURE:
          begin
            PostMessage(TWinControl(Owner).Handle, WM_SendMail, 0, 0);
          end;
      end;
    finally
      for i := 0 to EwbMapiMail.Attachments.Count - 1 do
      begin
        DeleteFile(EwbMapiMail.Attachments[i]);
      end;
    end;
  end;
  EwbMapiMail.Free;
  Terminate;
end;

end.
