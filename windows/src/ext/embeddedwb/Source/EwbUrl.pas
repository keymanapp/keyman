//***********************************************************
//                        URL Tools                         *
//                (Uniform Resource identifier)             *
//                                                          *
//                     For Borland Delphi                   *
//                       Freeware Unit                      *
//       by Eran Bodankin - bsalsa - bsalsa@gmail.com      *
//                                                          *
//        QueryUrl function is based on Indy algorithm      *
//             from: http://www.indyproject.org/            *
//                                                          *
//           Documentation and updated versions:            *
//                  http://www.bsalsa.com                   *
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
3. Mail me (bsalsa@gmail.com) any code change in the unit
   for the benefit of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}
//$Id: EwbUrl.pas,v 1 2007/02/15 21:01:42 bsalsa Exp $
{
QueryUrl Structure:
  Protocol + :// + UserName + : + Password + HostName + Port + Path +
  Document + Parameters+ Bookmark

CrackUrl Structure:
<Scheme>://<UserName>:<Password>@<HostName>:<PortNumber>/<UrlPath><ExtraInfo>
 Note by MS:
(Some fields are optional.) For example, consider this URL:
http://someone:secret@www.microsoft.com:80/visualc/stuff.htm#contents

CrackUrl parses it as follows:
* Scheme: "http" or ATL_URL_SCHEME_HTTP
* UserName: "someone"
* Password: "secret"
* HostName: "www.microsoft.com"
* PortNumber: 80
* UrlPath: "visualc/stuff.htm"
* ExtraInfo: "#contents"

URL_COMPONENTS = record that contains:
    dwStructSize: DWORD;      =  size of this structure. Used in version check
    lpszScheme: LPSTR;        =  pointer to scheme name
    dwSchemeLength: DWORD;    =   length of scheme name
    nScheme: TInternetScheme; =  enumerated scheme type (if known)
    lpszHostName: LPSTR;      =  pointer to host name
    dwHostNameLength: DWORD;  =  length of host name
    nPort: INTERNET_PORT;     =  converted port number
    pad: WORD;                =  force correct allignment regardless of comp. flags
    lpszUserName: LPSTR;      =  pointer to user name
    dwUserNameLength: DWORD;  =  length of user name
    lpszPassword: LPSTR;      =  pointer to password
    dwPasswordLength: DWORD;  =  length of password
    lpszUrlPath: LPSTR;       =  pointer to URL-path
    dwUrlPathLength: DWORD;   =  length of URL-path
    lpszExtraInfo: LPSTR;     =  pointer to extra information (e.g. ?foo or #foo)
    dwExtraInfoLength: DWORD; =  length of extra information

URL_COMPONENTS on MSDN:
http://msdn2.microsoft.com/en-us/library/aa385420.aspx

CoInternetQueryInfo Function fags:
http://msdn.microsoft.com/library/default.asp?url=/workshop/networking/moniker/reference/enums/queryoption.asp
}

unit EwbUrl;

{$I EWB.inc}

{$DEFINE USE_DebugString}

interface

uses
  Dialogs, Windows, WinInet;

const
  TEMP_SIZE = 1024;
  MAX_BUFFER = 256;
  WebDelim = '/';
  ProtocolDelim = '://';
  QueryDelim = '?';
  BookmarkDelim = '#';
  EqualDelim = '=';
  DriveDelim = ':'; //I know it's in SysUtils already but, not in D5.
type
  TQueryOption = ULONG;
  TCoInternetQueryInfo = function(pwzUrl: LPCWSTR; QueryOptions: TQueryOption; dwQueryFlags: DWORD;
    pvBuffer: Pointer; cbBuffer: DWORD; var pcbBuffer: DWORD; dwReserved: DWORD): HResult; stdcall;

type
  TOnError = procedure(Sender: TObject; ErrorCode: integer; ErrMessage: string) of object;
type
  TUrl = class
  private
    FDocument: string;
    FProtocol: string;
    FUrl: string;
    FPort: Integer;
    FUrlPath: string;
    FHostName: string;
    FExtraInfo: string;
    FUserName: string;
    FPassword: string;
    FBookmark: string;
    FOnError: TOnError;
    FParameters: string;
    FUrlComponent: URL_COMPONENTS;
    CoInternetQueryInfo: TCoInternetQueryInfo;
    function initCoInternetQueryInfo: boolean;
  protected
    procedure SetUrl(const Value: string);
    procedure FillUrlComponent;
  public
    function FixUrl(Url: string): string;
    function BuildUrl: WideString;
    function CanonicalizeUrl(const Url: string; dwFlags: integer): WideString;
    function CombineUrl(const BaseUrl, RelativaUrl: string; dwFlags: DWord): WideString;
    function CompareUrl(const pwzUrl1, pwzUrl2: WideString): HResult;
    function CrackUrl(const Url: string; dwFlags: DWord): WideString;
    function CreateUrl(const dwFlags: DWord): WideString;
    function EncodeUrl(const InputStr: string; const bQueryStr: Boolean): string;
    function DecodeUrl(const InputStr: string): string;
    function IsUrlValid(const Url: string): boolean;
    function IsUrlCached(const Url: string): boolean;
    function GetUrlSize(const Url: string): string;
    function GetUrlType(const Url: string): string;
    function GetUrlProtocolVersion(const Url: string): string;
    function GetUrlServerDetails(const Url: string): string;
    function GetUrlCharSet(const Url: string): string;
    function GetUrlServer(const Url: string): string;
    function GetUrlLastModified(const Url: string): string;
    function GetUrlDate(const Url: string): string;
    function GetUrlStatusCode(const Url: string): string;
    function GetUrlEntityTag(const Url: string): string;
    function QueryInfo(const Url: string; dwInfoFlag: Integer): string;
    function CoInetQueryInfo(const Url: WideString; QueryOptions: Cardinal): Boolean;
    function ReadFile(const URL: string; TimeOut: LongWord): string;
    procedure Clear;
    procedure ClearUrlComponent;
    procedure QueryUrl(Url: string);
    constructor Create(const Url: string); overload;
  public
    property Bookmark: string read FBookmark write FBookmark;
    property Document: string read FDocument write FDocument;
    property ExtraInfo: string read FExtraInfo write FExtraInfo;
    property HostName: string read FHostName write FHostName;
    property Parameters: string read FParameters write FParameters;
    property Password: string read FPassword write FPassword;
    property Port: Integer read FPort write FPort;
    property Protocol: string read FProtocol write FProtocol;
    property OnError: TOnError read FOnError write FOnError;
    property Url: string read FUrl write SetUrl;
    property UrlComponent: URL_COMPONENTS read FUrlComponent write FUrlComponent;
    property UrlPath: string read FUrlPath write FUrlPath;
    property UserName: string read FUserName write FUserName;
  end;

implementation

uses
  EwbCoreTools, SysUtils, Forms, EwbIEConst;

constructor TUrl.Create(const Url: string);
begin
  if Length(Url) > 0 then
    FUrl := Url;
end;

procedure TUrl.SetUrl(const Value: string);
begin
  if Length(Value) > 0 then
    QueryUrl(Value);
end;

//==============================================================================

procedure TUrl.Clear;
begin
  FBookmark := '';
  FHostName := '';
  FProtocol := '';
  FUrlPath := '';
  FDocument := '';
  FPort := 80;
  FExtraInfo := '';
  FUserName := '';
  FPassword := '';
  FParameters := '';
  ClearUrlComponent;
end;

procedure TUrl.ClearUrlComponent;
begin
  with FUrlComponent do
  begin
    lpszScheme := nil;
    lpszHostName := nil;
    lpszUrlPath := nil;
    lpszUserName := nil;
    lpszPassword := nil;
    lpszExtraInfo := nil;
  end;
end;

procedure TUrl.FillUrlComponent;
begin
  ClearUrlComponent;
  with FUrlComponent do
  begin
    dwStructSize := SizeOf(URL_COMPONENTS);
    if FProtocol <> '' then
    begin
      lpszScheme := PChar(FProtocol);
      dwSchemeLength := Length(FProtocol);
    end
    else
      lpszScheme := nil;
    if FHostName <> '' then
    begin
      lpszHostName := PChar(FHostName);
      dwHostNameLength := Length(PChar(FHostName));
    end
    else
      lpszHostName := nil;
    if FUrlPath <> '' then
    begin
      lpszUrlPath := PChar(FUrlPath);
      dwUrlPathLength := Length(FUrlPath);
    end
    else
      lpszUrlPath := nil;
    if FUserName <> '' then
    begin
      lpszUserName := PChar(FUserName);
      dwUserNameLength := Length(FUserName);
    end
    else
      lpszUserName := nil;
    if FPassword <> '' then
    begin
      lpszPassword := PChar(FPassword);
      dwPasswordLength := Length(FPassword);
    end
    else
      lpszPassword := nil;
    if FExtraInfo = '' then
      FExtraInfo := FDocument + FParameters;
    if FBookmark <> '' then
      FExtraInfo := FExtraInfo + BookmarkDelim + FBookmark;
    if FExtraInfo <> '' then
    begin
      lpszExtraInfo := PChar(FExtraInfo);
      dwExtraInfoLength := Length(FExtraInfo);
    end
    else
      lpszExtraInfo := nil;
    if (FPort = 0) then
      nPort := FPort;
{$IFDEF DELPHI6_UP}
{$IFNDEF DELPHIXE2_UP}
    pad := 1; //force correct allignment regardless of comp. flags
{$ENDIF}
{$ENDIF}
  end;
end;

function TUrl.initCoInternetQueryInfo: boolean;
var
  lh: HMODULE;
begin
  Result := False;
  CoInternetQueryInfo := nil;
  lh := loadlibrary('URLMON.DLL');
  if lh = 0 then
    Exit;
  CoInternetQueryInfo := GetProcAddress(lh, 'CoInternetQueryInfo');
  Result := (@CoInternetQueryInfo) <> nil;
end;

procedure TUrl.QueryUrl(Url: string);
var
  TmpStr: string;
  IdxPos, CharPos: Integer;
begin
  Clear;
  Url := FixUrl(Url);
  FormatPath(Url);
  IdxPos := AnsiPos(ProtocolDelim, Url);
  if IdxPos > 0 then
  begin
    FProtocol := Copy(Url, 1, IdxPos - 1);
    Delete(Url, 1, IdxPos + 2);
    TmpStr := CutString(Url, WebDelim, True);
    IdxPos := AnsiPos('@', TmpStr);
    FPassword := Copy(TmpStr, 1, IdxPos - 1);
    if IdxPos > 0 then
      Delete(TmpStr, 1, IdxPos);
    FUserName := CutString(FPassword, DriveDelim, True);
    if Length(FUserName) = 0 then
    begin
      FPassword := '';
    end;
    if (AnsiPos('[', TmpStr) > 0) and (AnsiPos(']', TmpStr) > AnsiPos('[', TmpStr)) then
    begin
      FHostName := CutString(TmpStr, ']');
      CutString(FHostName, '[');
      CutString(TmpStr, DriveDelim);
    end
    else
    begin
      FHostName := CutString(TmpStr, DriveDelim, True);
    end;
    FPort := StrToIntDef(TmpStr, 80);
    CharPos := AnsiPos(QueryDelim, Url);
    if CharPos > 0 then
    begin
      IdxPos := GetPos(WebDelim, Url, CharPos);
    end
    else
    begin
      CharPos := AnsiPos(EqualDelim, Url);
      if CharPos > 0 then
      begin
        IdxPos := GetPos(WebDelim, Url, CharPos);
      end
      else
      begin
        IdxPos := GetPos(WebDelim, Url, -1);
      end;
    end;
    FUrlPath := WebDelim + Copy(Url, 1, IdxPos);
    if CharPos > 0 then
    begin
      FDocument := Copy(Url, 1, CharPos - 1);
      Delete(Url, 1, CharPos - 1);
      FParameters := Url;
    end
    else
      FDocument := Url;
    Delete(FDocument, 1, IdxPos);
    FBookmark := FDocument;
    FDocument := CutString(FBookmark, BookmarkDelim);
  end
  else
  begin
    CharPos := AnsiPos(QueryDelim, Url);
    if CharPos > 0 then
    begin
      IdxPos := GetPos(WebDelim, Url, CharPos);
    end
    else
    begin
      CharPos := AnsiPos(EqualDelim, Url);
      if CharPos > 0 then
      begin
        IdxPos := GetPos(WebDelim, Url, CharPos);
      end
      else
      begin
        IdxPos := GetPos(WebDelim, Url, -1);
      end;
    end;
    FUrlPath := Copy(Url, 1, IdxPos);
    if CharPos > 0 then
    begin
      FDocument := Copy(Url, 1, CharPos - 1);
      Delete(Url, 1, CharPos - 1);
      FParameters := Url;
    end
    else
    begin
      FDocument := Url;
    end;
    Delete(FDocument, 1, IdxPos);
  end;
  if FBookmark = '' then
  begin
    FBookmark := FParameters;
    FParameters := CutString(FBookmark, BookmarkDelim);
  end;
  FillUrlComponent;
end;

function TUrl.CrackUrl(const Url: string; dwFlags: DWord): WideString;
var
  Buffers: array[0..5, 0..MAX_BUFFER - 1] of Char;
  bResult: boolean;
begin
  Clear;
  FUrl := FixUrl(Url);
  ZeroMemory(@FUrlComponent, SizeOf(URL_COMPONENTS));
  with FUrlComponent do
  begin
    dwStructSize := SizeOf(URL_COMPONENTS);
    dwSchemeLength := INTERNET_MAX_SCHEME_LENGTH;
    lpszScheme := Buffers[0];
    dwHostNameLength := INTERNET_MAX_HOST_NAME_LENGTH;
    lpszHostName := Buffers[1];
    dwUserNameLength := INTERNET_MAX_USER_NAME_LENGTH;
    lpszUserName := Buffers[2];
    dwPasswordLength := INTERNET_MAX_PASSWORD_LENGTH;
    lpszPassword := Buffers[3];
    dwUrlPathLength := INTERNET_MAX_PATH_LENGTH;
    lpszUrlPath := Buffers[4];
    dwExtraInfoLength := INTERNET_MAX_URL_LENGTH;
    lpszExtraInfo := Buffers[5];
  end;
  bResult := InternetCrackURL(PChar(Url), 0, dwFlags, FUrlComponent);
  if bResult then
  begin
    with FUrlComponent do
    begin
      FHostName := lpszHostName;
      FProtocol := lpszScheme;
      FUrlPath := lpszUrlPath;
      FPort := nPort;
      FExtraInfo := lpszExtraInfo;
      FUserName := lpszUserName;
      FPassword := lpszPassword;
      Result := Url;
    end;
  end
  else
  begin
    Clear;
    if Assigned(FOnError) then
      FOnError(Self, GetLastError, SysErrorMessage(GetLastError));
{$IFDEF USE_DebugString}
    OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
    Result := '';
  end;
end;

function TUrl.CombineUrl(const BaseUrl, RelativaUrl: string; dwFlags: DWord): WideString;
var
  Buffer: array[0..255] of Char;
  Size: DWORD;
  bResult: boolean;
begin
  Size := SizeOf(Buffer);
  bResult := InternetCombineUrl(PChar(BaseUrl), PChar(RelativaUrl),
    Buffer, Size, dwFlags);
  if bResult then
  begin
    Result := Buffer;
    FUrl := Result;
  end
  else
  begin
    if Assigned(FOnError) then
      FOnError(Self, GetLastError, SysErrorMessage(GetLastError));
{$IFDEF USE_DebugString}
    OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
    Result := '';
  end;

end;

function TUrl.CanonicalizeUrl(const Url: string; dwFlags: integer): WideString;
var
  Buffer: array[0..255] of Char;
  Size: DWORD;
  bResult: boolean;
begin
  Size := SizeOf(Buffer);
  bResult := InternetCanonicalizeUrl(PChar(Url), Buffer, Size, dwFlags);
  if bResult then
  begin
    Result := Buffer;
    FUrl := Result;
  end
  else
  begin
    if Assigned(FOnError) then
      FOnError(Self, GetLastError, SysErrorMessage(GetLastError));
{$IFDEF USE_DebugString}
    OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
    Result := '';
  end;
end;

function TUrl.CreateUrl(const dwFlags: DWord): WideString;
var
  Size: DWORD;
  Buffer: array[0..511] of Char;
  bResult: boolean;
begin
  FillUrlComponent;
  Size := FUrlComponent.dwStructSize;
  bResult := InternetCreateUrl(FUrlComponent, dwFlags, Buffer, Size);
  if bResult then
  begin
    Result := Buffer;
    FUrl := Result;
  end
  else
  begin
{$IFDEF USE_DebugString}
    OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
    if Assigned(FOnError) then
      FOnError(Self, GetLastError, SysErrorMessage(GetLastError));
  end;
end;

function TUrl.FixUrl(Url: string): string;

  function AnsiEndsStr(const ASubText, AText: string): Boolean;
  var
    SubTextLocation: Integer;
  begin
    SubTextLocation := Length(AText) - Length(ASubText) + 1;
    if (SubTextLocation > 0) and (ASubText <> '') and
      (ByteType(AText, SubTextLocation) <> mbTrailByte) then
      Result := AnsiStrComp((PChar(ASubText)), Pointer(@AText[SubTextLocation])) = 0
    else
      Result := False;
  end;
var
  DotPos, ipos: Integer;
begin
  Result := Url;
  if not AnsiEndsStr('/', Url) then
  begin
    ipos := LastDelimiter('/', Url);
    DotPos := LastDelimiter('.', Url);
    if DotPos < ipos then
      Result := Url + '/';
  end;
end;

function TUrl.EncodeURL(const InputStr: string; const bQueryStr: Boolean): string;
var
  Idx: Integer;
begin
  Result := '';
  for Idx := 1 to Length(InputStr) do
  begin
    case InputStr[Idx] of
      'A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.':
        Result := Result + InputStr[Idx];
      ' ':
        if bQueryStr then
          Result := Result + '+'
        else
          Result := Result + '%20';
    else
      Result := Result + '%' + SysUtils.IntToHex(Ord(InputStr[Idx]), 2);
    end;
  end;
end;

function TUrl.DecodeUrl(const InputStr: string): string;
var
  Idx: Integer;
  Hex: string;
  Code: Integer;
begin
  Result := '';
  Idx := 1;
  while Idx <= Length(InputStr) do
  begin
    case InputStr[Idx] of
      '%':
        begin
          if Idx <= Length(InputStr) - 2 then
          begin
            Hex := InputStr[Idx + 1] + InputStr[Idx + 2];
            Code := SysUtils.StrToIntDef('$' + Hex, -1);
            Inc(Idx, 2);
          end
          else
            Code := -1;
          if Code = -1 then
            raise SysUtils.EConvertError.Create('Invalid hex digit in URL');
          Result := Result + Chr(Code);
        end;
      '+':
        Result := Result + ' '
    else
      Result := Result + InputStr[Idx];
    end;
    Inc(Idx);
  end;
end;

function TUrl.BuildUrl: WideString;
begin
  FillUrlComponent;
  if (FProtocol = '') or (FHostName = '') then
  begin
    if Assigned(FOnError) then
      FOnError(Self, 0, 'Can not Create Url. Protocol or HostName are not valid!');
{$IFDEF USE_DebugString}
    OutputDebugString('Can not Create Url. Protocol or HostName are not valid!');
{$ENDIF}
    Exit;
  end;
  Result := FProtocol + ProtocolDelim;
  if (FUserName <> '') then
  begin
    Result := Result + FUserName;
    if FPassword <> '' then
    begin
      Result := Result + DriveDelim + FPassword;
    end;
    Result := Result + '@';
  end;
  Result := Result + FHostName;
  if (FPort <> 0) and (FPort <> 80) then
  begin
    Result := Result + DriveDelim + IntToStr(FPort);
  end;
  Result := Result + FUrlPath + FDocument + FParameters;
  if (FBookmark <> '') then
  begin
    Result := Result + BookmarkDelim + FBookmark;
  end;
end;

function TUrl.CompareUrl(const pwzUrl1, pwzUrl2: WideString): HResult;
begin
  if (pwzUrl1 = '') or (pwzUrl2 = '') then
  begin
{$IFDEF USE_DebugString}
    OutputDebugString('Can not Compare Url. pwzUrl1  or pwzUrl2 are empty!');
{$ENDIF}
    if Assigned(FOnError) then
      FOnError(Self, 0, 'Can not Compare Url. pwzUrl1  or pwzUrl2 are empty!');
  end;
  Result := AnsiCompareText(pwzUrl1, pwzUrl2);
end;

function TUrl.CoInetQueryInfo(const Url: WideString; QueryOptions: Cardinal): boolean;
var
  pcbBuffer: DWORD;
  dwCached: DWORD;
begin
  if not initCoInternetQueryInfo then
  begin
    Result := False;
    Exit;
  end;
  pcbBuffer := SizeOf(dwCached);
  if CoInternetQueryInfo(PWideChar(Url), QueryOptions, 0, @dwCached,
    SizeOf(dwCached), pcbBuffer, 0) <> S_OK then
  begin
    if Assigned(FOnError) then
      FOnError(Self, GetLastError, SysErrorMessage(GetLastError));
{$IFDEF USE_DebugString}
    OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
  end;
  Result := dwCached <> 0;
end;

function TUrl.QueryInfo(const Url: string; dwInfoFlag: Integer): string;
var
  hInet: HINTERNET;
  hConnect: HINTERNET;
  infoBuffer: array[0..512] of char;
  dummy: DWORD;
  bufLen: DWORD;
  ok: LongBool;
begin
  hInet := InternetOpen(PChar(Forms.Application.Title),
    INTERNET_OPEN_TYPE_PRECONFIG_WITH_NO_AUTOPROXY, nil, nil, 0);
  hConnect := InternetOpenUrl(hInet, PChar(Url), nil, 0, INTERNET_FLAG_NO_UI, 0);
  if not Assigned(hConnect) then
  begin
    if Assigned(FOnError) then
      FOnError(Self, GetLastError, SysErrorMessage(GetLastError));
{$IFDEF USE_DebugString}
    OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
    Result := '';
  end
  else
  begin
    dummy := 0;
    bufLen := Length(infoBuffer);
    ok := HttpQueryInfo(hConnect, dwInfoFlag, @infoBuffer[0], bufLen, dummy);
    if ok then
      Result := infoBuffer
    else
      Result := '';
    InternetCloseHandle(hConnect);
  end;
  InternetCloseHandle(hInet);
end;

function TUrl.ReadFile(const URL: string; TimeOut: LongWord): string;
var
  hInet: HInternet;
  hConnect: HInternet;
  infoBuffer: array[0..TEMP_SIZE - 1] of Char;
  iRead, iTimeOut: DWORD;
  strRead: string;
begin
  strRead := '';
  hInet := InternetOpen(PChar(Forms.Application.Title), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, INTERNET_FLAG_NO_CACHE_WRITE);
  if Assigned(hInet) then
  begin
    InternetQueryOption(hInet, INTERNET_OPTION_CONNECT_TIMEOUT, @iTimeOut, iRead);
    iTimeOut := TimeOut;
    InternetSetOption(hInet, INTERNET_OPTION_CONNECT_TIMEOUT, @iTimeOut, iRead);
    try
      hConnect := InternetOpenURL(hInet, PChar(Url), nil, 0, 0, 0);
      if Assigned(hConnect) then
      try
        repeat
          FillChar(infoBuffer, SizeOf(infoBuffer), #0);
          InternetReadFile(hConnect, @infoBuffer, sizeof(infoBuffer), iRead);
          strRead := strRead + string(infoBuffer);
        until iRead < TEMP_SIZE;
      finally
        InternetCloseHandle(hConnect);
      end
      else
      begin
        if Assigned(FOnError) then
          FOnError(Self, GetLastError, SysErrorMessage(GetLastError));
{$IFDEF USE_DebugString}
        OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
        Result := '';
      end;
    finally
      InternetCloseHandle(hInet);
    end;
    Result := strRead;
  end
  else
  begin
    if Assigned(FOnError) then
      FOnError(Self, GetLastError, SysErrorMessage(GetLastError));
{$IFDEF USE_DebugString}
    OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
    Result := '';
  end;
end;

function TUrl.IsUrlValid(const Url: string): boolean;
var
  Reply: string;
begin
  Reply := QueryInfo(Url, HTTP_QUERY_STATUS_CODE);
  if (Reply = '200') or (Reply = '401') then
    Result := True
  else
    Result := False;
end;

function TUrl.GetUrlSize(const Url: string): string;
begin
  Result := QueryInfo(Url, HTTP_QUERY_CONTENT_LENGTH);
end;

function TUrl.GetUrlType(const Url: string): string;
begin
  Result := QueryInfo(Url, HTTP_QUERY_CONTENT_TYPE);
end;

function TUrl.GetUrlDate(const Url: string): string;
begin
  Result := QueryInfo(Url, HTTP_QUERY_DATE);
end;

function TUrl.GetUrlLastModified(const Url: string): string;
begin
  Result := QueryInfo(Url, HTTP_QUERY_LAST_MODIFIED);
end;

function TUrl.GetUrlStatusCode(const Url: string): string;
begin
  Result := QueryInfo(Url, HTTP_QUERY_STATUS_CODE);
end;

function TUrl.GetUrlServer(const Url: string): string;
begin
  Result := QueryInfo(Url, HTTP_QUERY_SERVER);
end;

function TUrl.GetUrlEntityTag(const Url: string): string;
begin
  Result := QueryInfo(Url, HTTP_QUERY_ETAG);
end;

function TUrl.GetUrlCharset(const Url: string): string;
begin
  Result := QueryInfo(Url, HTTP_QUERY_ACCEPT_CHARSET);
end;

function TUrl.GetUrlServerDetails(const Url: string): string;
begin
  Result := QueryInfo(Url, HTTP_QUERY_RAW_HEADERS_CRLF);
end;

function TUrl.GetUrlProtocolVersion(const Url: string): string;
begin
  Result := QueryInfo(Url, HTTP_QUERY_VERSION);
end;

function TUrl.IsUrlCached(const Url: string): boolean;
begin
  Result := CoInetQueryInfo(Url, QUERY_IS_CACHED);
end;
{=====================================================================================}

end.
