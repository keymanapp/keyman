//*************************************************************************
//                                                                        *
//                        IEDownload Tools                                *
//                       For Delphi                                       *
//                                                                        *
//                     Freeware Unit                                      *
//                            by                                          *
//  Eran Bodankin -bsalsa(bsalsa@gmail.com)                              *
//                                                                        *
//                                                                        *
//  Updated versions:                                                     *
//               http://www.bsalsa.com                                    *
//*************************************************************************
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
4. Please, consider donation in our web site!
{*******************************************************************************}

unit IEDownloadTools;

interface

uses
  Windows, EwbIeConst, SysUtils, ActiveX;

function ResponseCodeToStr(dwResponse: Integer): string;
function FormatSize(Byte: Double): string;
function FormatTickToTime(TickCount: Cardinal): string;
function IsValidURL(const URL: Widestring): Boolean;
function WideStringToLPOLESTR(const Source: Widestring): POleStr;
function WideStringToWideChar(const inString: WideString): PWideChar;
function CodeInstallProblemToStr(ulStatusCode: Integer): string;
function DataAvalibleToStr(Code: Integer): string;
function SecToStr(const Seconds: LongInt): string;
procedure FreeWideChar(var inString: PWideChar);
function StrToCase(StringOf: string; CasesList: array of string): Integer;
function QueryRemoteFileInfo(const Url: string; dwInfoFlag: Integer): string;
function GetInfoByMimeType(const ContentType: string): boolean;
function StrContain(SubString: string; Str: string): boolean;
function IncludeTrailingPathDelimiter(const S: string): string;
function CharReplace(const Source: string; OldChar, NewChar: Char): string;


implementation

uses
  WinInet, UrlMon, Registry;

// Some constatants are missing in previous delphi versions.
// That's why they are declared here.
const
{$EXTERNALSYM BINDSTATUS_COOKIE_SENT}
  BINDSTATUS_COOKIE_SENT = BINDSTATUS_ACCEPTRANGES + 1;
{$EXTERNALSYM BINDSTATUS_COMPACT_POLICY_RECEIVED}
  BINDSTATUS_COMPACT_POLICY_RECEIVED = BINDSTATUS_COOKIE_SENT + 1;
{$EXTERNALSYM BINDSTATUS_COOKIE_SUPPRESSED}
  BINDSTATUS_COOKIE_SUPPRESSED = BINDSTATUS_COMPACT_POLICY_RECEIVED + 1;
{$EXTERNALSYM BINDSTATUS_COOKIE_STATE_UNKNOWN}
  BINDSTATUS_COOKIE_STATE_UNKNOWN = BINDSTATUS_COOKIE_SUPPRESSED + 1;
{$EXTERNALSYM BINDSTATUS_COOKIE_STATE_ACCEPT}
  BINDSTATUS_COOKIE_STATE_ACCEPT = BINDSTATUS_COOKIE_STATE_UNKNOWN + 1;
{$EXTERNALSYM BINDSTATUS_COOKIE_STATE_REJECT}
  BINDSTATUS_COOKIE_STATE_REJECT = BINDSTATUS_COOKIE_STATE_ACCEPT + 1;
{$EXTERNALSYM BINDSTATUS_COOKIE_STATE_PROMPT}
  BINDSTATUS_COOKIE_STATE_PROMPT = BINDSTATUS_COOKIE_STATE_REJECT + 1;
{$EXTERNALSYM BINDSTATUS_COOKIE_STATE_LEASH}
  BINDSTATUS_COOKIE_STATE_LEASH = BINDSTATUS_COOKIE_STATE_PROMPT + 1;
{$EXTERNALSYM BINDSTATUS_COOKIE_STATE_DOWNGRADE}
  BINDSTATUS_COOKIE_STATE_DOWNGRADE = BINDSTATUS_COOKIE_STATE_LEASH + 1;
{$EXTERNALSYM BINDSTATUS_POLICY_HREF}
  BINDSTATUS_POLICY_HREF = BINDSTATUS_COOKIE_STATE_DOWNGRADE + 1;
{$EXTERNALSYM BINDSTATUS_P3P_HEADER}
  BINDSTATUS_P3P_HEADER = BINDSTATUS_POLICY_HREF + 1;
{$EXTERNALSYM BINDSTATUS_SESSION_COOKIE_RECEIVED}
  BINDSTATUS_SESSION_COOKIE_RECEIVED = BINDSTATUS_P3P_HEADER + 1;
{$EXTERNALSYM BINDSTATUS_PERSISTENT_COOKIE_RECEIVED}
  BINDSTATUS_PERSISTENT_COOKIE_RECEIVED = BINDSTATUS_SESSION_COOKIE_RECEIVED +
    1;

function CodeInstallProblemToStr(ulStatusCode: Integer): string;
begin
  case ulStatusCode of
    CIP_DISK_FULL: Result := 'Destination can accept no more data.';
    CIP_ACCESS_DENIED: Result := 'Permissions problem.';
    CIP_NEWER_VERSION_EXISTS: Result :=
      'Destination contains a newer version than the source.';
    CIP_OLDER_VERSION_EXISTS: Result :=
      'Destination contains an older version than the source.';
    CIP_NAME_CONFLICT: Result :=
      'Destination does not allow the naming convention used by the source.';
    CIP_TRUST_VERIFICATION_COMPONENT_MISSING: Result :=
      'Destination cannot verify the source.';
    CIP_EXE_SELF_REGISTERATION_TIMEOUT: Result := 'Timeout has occurred. ';
    CIP_UNSAFE_TO_ABORT: Result :=
      'Installation or download should not be aborted.';
    CIP_NEED_REBOOT: Result := 'Destination machine requires rebooting.';
    CIP_NEED_REBOOT_UI_PERMISSION: Result :=
      'Destination machine should reboot without user interface (UI).';
  else
    Result := ResponseCodeToStr(ulStatusCode)
  end;
end;

function DataAvalibleToStr(Code: Integer): string;
begin
  case Code of
    BSCF_FIRSTDATANOTIFICATION: Result :=
      'Identifed the first call to OnDataAvailable.';
    BSCF_INTERMEDIATEDATANOTIFICATION: Result :=
      'Identifed the intermediate call to OnDataAvailable.';
    BSCF_LASTDATANOTIFICATION: Result :=
      'Identifed the last call to OnDataAvailable.';
    BSCF_DATAFULLYAVAILABLE: Result :=
      'All of the requested data is available.';
    BSCF_AVAILABLEDATASIZEUNKNOWN: Result :=
      'Size of the data available is unknown.';
    //  BSCF_SKIPDRAINDATAFORFILEURLS: Result := 'Bypass cache downloads.';
  else
    Result := ResponseCodeToStr(Code)
  end;
end;

function ResponseCodeToStr(dwResponse: Integer): string;
begin
  case dwResponse of
    100: Result := 'Continue';
    101: Result := 'Switching Protocols';
    200: Result := 'OK';
    201: Result := 'Created';
    202: Result := 'Accepted';
    203: Result := 'Non-Authoritative Information';
    204: Result := 'No Content';
    205: Result := 'Reset Content';
    206: Result := 'Partial Content';
    300: Result := 'Multiple Choices';
    301: Result := 'Moved Permanently';
    302: Result := 'Moved Temporarily';
    303: Result := 'See Other';
    304: Result := 'Not Modified';
    305: Result := 'Use Proxy';
    400: Result := 'Bad Request';
    401: Result := 'Unauthorized';
    402: Result := 'Payment Required';
    403: Result := 'Forbidden';
    404: Result := 'Not Found';
    405: Result := 'Method Not Allowed';
    406: Result := 'None Acceptable';
    407: Result := 'Proxy Authentication Required';
    408: Result := 'Request Timeout';
    409: Result := 'Conflict';
    410: Result := 'Gone';
    411: Result := 'Length Required';
    412: Result := 'Precondition Failed';
    413: Result := 'Request Entity Too Large';
    414: Result := 'Request-URI Too Long';
    415: Result := 'Unsupported Media Type';
    416: Result := 'Requested Range Not Satisfiable';
    417: Result := 'Expectation Failed';
    500: Result := 'Internal Server Error';
    501: Result := 'Not Implemented';
    502: Result := 'Bad Gateway';
    503: Result := 'Service Unavailable';
    504: Result := 'Gateway Timeout';
    505: Result := 'HTTP Version Not Supported';
    BINDSTATUS_FINDINGRESOURCE: Result := 'Finding resource...';
    BINDSTATUS_CONNECTING: Result := 'Connecting...';
    BINDSTATUS_REDIRECTING: Result := 'Server redirecting client...';
    BINDSTATUS_BEGINDOWNLOADDATA: Result := 'Beginning to download data...';
    BINDSTATUS_DOWNLOADINGDATA: Result := 'Downloading data...';
    BINDSTATUS_ENDDOWNLOADDATA: Result := 'Ending data download...';
    BINDSTATUS_BEGINDOWNLOADCOMPONENTS: Result :=
      'Beginning to download components...';
    BINDSTATUS_INSTALLINGCOMPONENTS: Result := 'Installing components...';
    BINDSTATUS_ENDDOWNLOADCOMPONENTS: Result := 'Ending component download...';
    BINDSTATUS_USINGCACHEDCOPY: Result := 'Using cached copy...';
    BINDSTATUS_SENDINGREQUEST: Result := 'Sending request...';
    BINDSTATUS_CLASSIDAVAILABLE: Result := 'CLSID available...'; //***
    BINDSTATUS_MIMETYPEAVAILABLE: Result := 'MIME type available...';
    BINDSTATUS_CACHEFILENAMEAVAILABLE: Result := 'Cache file name available...';
    BINDSTATUS_BEGINSYNCOPERATION: Result :=
      'Synchronous operation has started...';
    BINDSTATUS_ENDSYNCOPERATION: Result := 'Ending synchronous operation...';
    BINDSTATUS_BEGINUPLOADDATA: Result := 'Beginning to upload data...';
    BINDSTATUS_UPLOADINGDATA: Result := 'Uploading data...';
    BINDSTATUS_ENDUPLOADDATA: Result := 'Ending data upload...';
    BINDSTATUS_PROTOCOLCLASSID: Result :=
      'Protocol handler CLSID is available...';
    BINDSTATUS_ENCODING: Result := 'Encoding data...';
    BINDSTATUS_VERIFIEDMIMETYPEAVAILABLE: Result :=
      'Verified MIME type is available...';
    BINDSTATUS_CLASSINSTALLLOCATION: Result :=
      'Class install location is available...';
    BINDSTATUS_DECODING: Result := 'Decoding data...';
    BINDSTATUS_LOADINGMIMEHANDLER: Result :=
      'Pluggable MIME handler is being loaded...';
    BINDSTATUS_CONTENTDISPOSITIONATTACH: Result :=
      'Content-Disposition resource is an attachment...';
    BINDSTATUS_FILTERREPORTMIMETYPE: Result :=
      'New MIME type available for resource...';
    BINDSTATUS_CLSIDCANINSTANTIATE: Result :=
      'CLSID to return on BindToObject...';
    BINDSTATUS_IUNKNOWNAVAILABLE: Result :=
      'IUnknown interface has been released...';
    BINDSTATUS_DIRECTBIND: Result :=
      'Connected directly to the pluggable protocol handler...';
    BINDSTATUS_RAWMIMETYPE: Result := 'Raw MIME type...';
    BINDSTATUS_PROXYDETECTING: Result := 'Proxy server has been detected...';
    BINDSTATUS_ACCEPTRANGES: Result := 'Valid types of range requests...';
    BINDSTATUS_COOKIE_SENT: Result := 'A cookie was sent';
    BINDSTATUS_COMPACT_POLICY_RECEIVED: Result :=
      'A Platform for Privacy Preferences was received.';
    BINDSTATUS_COOKIE_SUPPRESSED: Result :=
      'A cookie was suppressed from being sent to the web server.';
    BINDSTATUS_COOKIE_STATE_UNKNOWN: Result :=
      'A a cookie has been initialized. ';
    BINDSTATUS_COOKIE_STATE_ACCEPT: Result :=
      'A cookie sent by the server was accepted.';
    BINDSTATUS_COOKIE_STATE_REJECT: Result :=
      'A cookie sent by the server was rejected.';
    BINDSTATUS_COOKIE_STATE_PROMPT: Result :=
      'User settings require a prompt before performing a cookie operation.';
    BINDSTATUS_COOKIE_STATE_LEASH: Result := 'A cookie is a leashed cookie.';
    BINDSTATUS_COOKIE_STATE_DOWNGRADE: Result :=
      'A cookie is a downgraded cookie.';
    BINDSTATUS_POLICY_HREF: Result :=
      'HTTP headers contain a link to the full privacy policy';
    BINDSTATUS_P3P_HEADER: Result :=
      'HTTP response from the server contains the P3P privacy header';
    BINDSTATUS_SESSION_COOKIE_RECEIVED: Result :=
      'A session cookie was received.';
    BINDSTATUS_PERSISTENT_COOKIE_RECEIVED: Result :=
      'A persistent cookie was received. ';
    BINDSTATUS_SESSION_COOKIES_ALLOWED: Result :=
      'The session cookies are allowed.';
    BINDSTATUS_CACHECONTROL: Result :=
      'A response from the server was written to memory only.'; //IE7
    BINDSTATUS_CONTENTDISPOSITIONFILENAME: Result :=
      'The Content-Disposition header contains a file name.';
    BINDSTATUS_MIMETEXTPLAINMISMATCH: Result :=
      'The reported Content-Type of the file does not match the content. ';
    BINDSTATUS_PUBLISHERAVAILABLE: Result :=
      'The publisher name is being downloaded and is available.';
    BINDSTATUS_DISPLAYNAMEAVAILABLE: Result :=
      'The display name being downloaded and is available.';
    BINDSTATUS_SSLUX_NAVBLOCKED: Result := 'A problem with the SSL certificate.';
    BINDSTATUS_SERVER_MIMETYPEAVAILABLE: Result :=
      'Server''s authoritative MIME type reported'; //IE8  - Attention: This documentation is preliminary and is subject to change
    BINDSTATUS_SNIFFED_CLASSIDAVAILABLE: Result :=
      'Class identifier (CLSID) generated from authoritative Content-Type HTTP response header'; //IE8  - Attention: This documentation is preliminary and is subject to change
    BINDSTATUS_64BIT_PROGRESS: Result :=
      'Download progress values are above the maximum 32-bit file size limit.'; //IE8  - Attention: This documentation is preliminary and is subject to change
    // E_PENDING: Result := 'The data necessary to complete this operation is not yet available';
    E_UNEXPECTED: Result := 'Catastrophic failure';
    E_FAIL: Result := 'Failed';
    E_NOINTERFACE: Result := 'No such interface supported';
    E_NOTIMPL: Result := 'Not implemented';
    E_ABORT: Result := 'Operation Aborted.';
    E_ACCESSDENIED: Result := 'Access Denied.';
    E_INVALIDARG: Result := 'One or more parameters are invalid.';
    E_OUTOFMEMORY: Result := 'There was insufficient memory ...';
    ERROR_HTTP_REDIRECT_NEEDS_CONFIRMATION: Result :=
      'The redirection requires user confirmation.';
    ERROR_INTERNET_CHG_POST_IS_NON_SECURE: Result :=
      'The application is posting and attempting to change multiple lines of text on a server that is not secure.';
    ERROR_INTERNET_CLIENT_AUTH_CERT_NEEDED: Result :=
      'The server is requesting client authentication.';
    ERROR_INTERNET_CLIENT_AUTH_NOT_SETUP: Result :=
      'Client authorization is not set up on this computer.';
    ERROR_INTERNET_FORTEZZA_LOGIN_NEEDED: Result :=
      'The requested resource requires Fortezza authentication.';
    ERROR_INTERNET_HTTP_TO_HTTPS_ON_REDIR: Result :=
      'The application is moving from a non-SSL to an SSL connection because of a redirect.';
    ERROR_INTERNET_HTTPS_TO_HTTP_ON_REDIR: Result :=
      'The application is moving from an SSL to an non-SSL connection because of a redirect.';
    ERROR_INTERNET_INVALID_CA: Result :=
      'The function is unfamiliar with the Certificate Authority that generated the server''s certificate.';
    ERROR_INTERNET_MIXED_SECURITY: Result :=
      'The content is not entirely secure. Some of the content being viewed may have come from unsecured servers.';
    ERROR_INTERNET_POST_IS_NON_SECURE: Result :=
      'The application is posting data to a server that is not secure.';
    ERROR_INTERNET_SEC_CERT_CN_INVALID: Result :=
      'SSL certificate common name (host name field) is incorrect???for example, if you entered www.server.com and the common name on the certificate says www.different.com.';
    ERROR_INTERNET_SEC_CERT_DATE_INVALID: Result :=
      'SSL certificate date that was received from the server is bad. The certificate is expired.';
    ERROR_INTERNET_SEC_CERT_REVOKED: Result := 'SSL certificate was revoked.';
    INET_E_AUTHENTICATION_REQUIRED: Result :=
      'Authentication is needed to access the object.';
    INET_E_CANNOT_CONNECT: Result :=
      'The attempt to connect to the Internet has failed.';
    INET_E_CANNOT_INSTANTIATE_OBJECT: Result := 'CoCreateInstance failed.';
    INET_E_CANNOT_LOAD_DATA: Result := 'The object could not be loaded.';
    INET_E_CANNOT_LOCK_REQUEST: Result :=
      'The requested resource could not be locked.';
    INET_E_CANNOT_REPLACE_SFP_FILE: Result :=
      'Cannot replace a protected System File Protection (SFP) file.';
    INET_E_CODE_DOWNLOAD_DECLINED: Result :=
      'Permission to download is declined.';
    INET_E_CONNECTION_TIMEOUT: Result :=
      'The Internet connection has timed out.';
    INET_E_DATA_NOT_AVAILABLE: Result :=
      'An Internet connection was established, but the data cannot be retrieved.';
    INET_E_DEFAULT_ACTION: Result := 'Use the default action.';
    INET_E_DOWNLOAD_FAILURE: Result :=
      'The download has failed (the connection was interrupted).';
    INET_E_INVALID_REQUEST: Result := 'The request was invalid.';
    INET_E_INVALID_URL: Result := 'The URL could not be parsed.';
    INET_E_NO_SESSION: Result := 'No Internet session was established.';
    INET_E_NO_VALID_MEDIA: Result :=
      'The object is not in one of the acceptable MIME types.';
    INET_E_OBJECT_NOT_FOUND: Result := 'The object was not found.';
    INET_E_QUERYOPTION_UNKNOWN: Result := 'The requested option is unknown.';
    INET_E_REDIRECT_TO_DIR: Result :=
      'The request is being redirected to a directory.';
    INET_E_REDIRECTING: Result := 'The request is being redirected.';
    INET_E_RESOURCE_NOT_FOUND: Result := 'The server or proxy was not found.';
    INET_E_RESULT_DISPATCHED: Result := 'Result is dispatched.';
    INET_E_SECURITY_PROBLEM: Result := 'A security problem was encountered.';
    INET_E_TERMINATED_BIND: Result := 'Binding is terminated.';
    INET_E_UNKNOWN_PROTOCOL: Result :=
      'The protocol is not known and no pluggable protocols have been entered that match.';
    //INET_E_USE_DEFAULT_PROTOCOLHANDLER: Result := 'Use the default protocol handler.';
    INET_E_USE_EXTEND_BINDING: Result :=
      'Re-issue request with extended Binding.';
    INET_E_USE_DEFAULT_SETTING: Result := 'Use the default settings.';
    MK_E_CANTOPENFILE: Result := 'Moniker cannot open file.';
    MK_E_CONNECTMANUALLY: Result :=
      'The operation was unable to connect to the storage, possibly because a network device could not be connected to. For more information, see IMoniker::BindToObject.';
    MK_E_ENUMERATION_FAILED: Result := 'Moniker could not be enumerated';
    MK_E_EXCEEDEDDEADLINE: Result :=
      'The operation could not be completed within the time limit specified by the bind context''s BIND_OPTS structure.';
    MK_E_INTERMEDIATEINTERFACENOTSUPPORTED: Result :=
      'An intermediate object was found but it did not support an interface required for an operation. For example, an item moniker returns this value if its container does not support the IOleItemContainer interface.';
    MK_E_INVALIDEXTENSION: Result := 'Bad extension for file.';
    MK_E_MUSTBOTHERUSER: Result :=
      'User input required for operation to succeed.';
    MK_E_NEEDGENERIC: Result := 'Moniker needs to be generic.';
    MK_E_NOINVERSE: Result := 'Moniker class has no inverse.';
    MK_E_NOOBJECT: Result := 'No object for moniker.';
    MK_E_NOPREFIX: Result := 'No common prefix.';
    MK_E_NOSTORAGE: Result :=
      'The object identified by the moniker does not have its own storage.';
    MK_E_NOTBINDABLE: Result := 'Moniker is not bindable.';
    MK_E_NOTBOUND: Result := 'Moniker is not bound.';
    MK_E_SYNTAX: Result :=
      'A moniker could not be created because of invalid URL syntax.';
    MK_E_UNAVAILABLE: Result := 'Operation unavailable.';
    S_OK: Result := 'The operation was successful';
    STG_E_ACCESSDENIED: Result := 'Unable to access the storage object.';
    MK_S_ASYNCHRONOUS: Result := 'Asynchronous mode.';
  else
    Result := 'Unknown Response: ' + IntToStr(dwResponse) + ' ' +
      SysErrorMessage(dwResponse)
  end;
end;

function WideStringToLPOLESTR(const Source: Widestring): POleStr;
var
  Len: Integer;
begin
  Len := Length(Source) * SizeOf(WideChar);
  Result := CoTaskMemAlloc(Len + 2);
  FillChar(Result^, Len + 2, 0);
  Move(Result^, PWideString(Source)^, Len);
end;

function SecToStr(const Seconds: LongInt): string;
var
  H: Integer;
  M: Integer;
  S: Integer;
begin
  h := (Seconds div 60) div 60 mod 60;
  m := (Seconds div 60) mod 60;
  s := Seconds mod 60;
  Result := Format('%2.2d:%2.2d:%2.2d', (.H, M, S.));
end;

function FormatTickToTime(TickCount: Cardinal): string;
var
  Hours, Minutes, Seconds, MSecs: Cardinal;
  S_HUR, S_MIN, S_SEC, S_MSC: string;
begin
  S_MSC := '000';
  S_SEC := '00';
  S_MIN := '00';
  S_HUR := '00';
  try
    if (TickCount > 0) then
    begin
      MSecs := TickCount mod MSecsPerSec;
      TickCount := TickCount div MSecsPerSec;
      S_MSC := Format('%.3d', [MSecs]);
      Seconds := TickCount mod SecsPerMin;
      TickCount := TickCount div SecsPerMin;
      S_SEC := Format('%.2d', [Seconds]);
      Minutes := TickCount mod MinsPerHour;
      TickCount := TickCount div MinsPerHour;
      S_MIN := Format('%.2d', [Minutes]);
      Hours := TickCount mod HoursPerDay;
      S_HUR := Format('%.2d', [Hours]);
    end;
  finally
    if S_HUR = '00' then
      Result := S_MIN + 'm :' + S_SEC + 's :' + S_MSC + 'ms';
    if (S_HUR = '00') and (S_MIN = '00') then
      Result := S_SEC + 's :' + S_MSC + 'ms';
    if S_HUR <> '00' then
      Result := S_HUR + 'h :' + S_MIN + 'm :' + S_SEC + 's :' + S_MSC + 'ms';
  end;
end;


function StrToCase(StringOf: string; CasesList: array of string): Integer;
var
  Idx: integer;
begin
  Result := -1;
  for Idx := 0 to Length(CasesList) - 1 do
  begin
    if CompareText(StringOf, CasesList[Idx]) = 0 then
    begin
      Result := Idx;
      Break;
    end;
  end;
end;

function FormatSize(Byte: Double): string;
begin
  if (Byte > 0) then
  begin
    if (Byte < 1024) then
      Result := Format('%.2n b', [Byte])
    else
      if (Byte > 1024) then
      begin
        Byte := (Byte / 1024);
        if (Byte < 1024) then
          Result := Format('%.2n Kb', [Byte])
        else
        begin
          Byte := (Byte / 1024);
          Result := Format('%.2n Mb', [Byte]);
        end;
      end;
  end;
end;

function IsValidURL(const URL: Widestring): Boolean;
begin
  Result := UrlMon.IsValidURL(nil, PWideChar(URL), 0) = S_OK;
end;


function WideStringToWideChar(const inString: WideString): PWideChar;
begin
  Result := CoTaskMemAlloc((Length(inString) + 1) * SizeOf(WideChar));
  Move(inString, Result, (Length(inString) + 1) * SizeOf(WideChar));
end;

procedure FreeWideChar(var inString: PWideChar);
begin
  if Assigned(inString) then
  begin
    CoTaskMemFree(inString);
    inString := nil;
  end;
end;

function QueryRemoteFileInfo(const Url: string; dwInfoFlag: Integer): string;
var
  hInet: HINTERNET;
  hConnect: HINTERNET;
  infoBuffer: array[0..512] of char;
  dummy: DWORD;
  bufLen: DWORD;
  lbResult: LongBool;
begin
  hInet := InternetOpen(PChar('TIDownload'),
    INTERNET_OPEN_TYPE_PRECONFIG_WITH_NO_AUTOPROXY, nil, nil, 0);
  hConnect := InternetOpenUrl(hInet, PChar(Url), nil, 0, INTERNET_FLAG_NO_UI, 0);
  if not Assigned(hConnect) then
  begin
    Result := '';
  end
  else
  begin
    dummy := 0;
    bufLen := Length(infoBuffer);
    lbResult := HttpQueryInfo(hConnect, dwInfoFlag, @infoBuffer[0], bufLen, dummy);
    if lbResult then
      Result := infoBuffer
    else
      Result := '';
    InternetCloseHandle(hConnect);
  end;
  InternetCloseHandle(hInet);
end;

function GetInfoByMimeType(const ContentType: string): boolean;
var
  Reg: TRegistry;
  CLSID: string;
begin
  Result := FALSE;

  Reg := TRegistry.Create(KEY_READ);
  try
    with Reg do
    begin
      RootKey := HKEY_CLASSES_ROOT;
      if OpenKeyReadOnly('MIME\Database\Content Type\' + ContentType) then
      begin
        if ValueExists('CLSID') then
        begin
          CLSID := ReadString('CLSID');
          if CLSID <> '' then
          begin
            if OpenKeyReadOnly('\CLSID\' + CLSID) then
            begin
              if KeyExists('Control') then
              begin
                if KeyExists('EnableFullPage') then
                begin
                  Result := TRUE;
                end;
              end;
            end;
          end;
        end;
      end;
      CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function StrContain(SubString: string; Str: string): boolean;
begin
  Result := AnsiPos(AnsiLowerCase(SubString), AnsiLowerCase(Str)) > 0;
end;

function IncludeTrailingPathDelimiter(const S: string): string;
begin
  Result := S;
  if not IsPathDelimiter(Result, Length(Result)) then
    Result := Result + PathDelim;
end;

function CharReplace(const Source: string; OldChar, NewChar: Char): string;
var
  i: Integer;
begin
  Result := Source;
  for i := 1 to Length(Result) do
    if Result[i] = OldChar then
      Result[i] := NewChar
end;

end.
