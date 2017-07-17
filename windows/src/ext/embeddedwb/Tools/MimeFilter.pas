{*****************************************************************}
{                                                                 }
{            Borland Delphi Runtime Library                       }
{            MimeFilter interface unit                            }
{                                                                 }
{            By Per Lindso Larsen and Eran Bodankin - bsalsa      }
{            Converted 6 May 2007 bsalsa@bsalsa.com               }
{            Last modified 27 Jan 2009 bsalsa@bsalsa.com          }
{            Version 2                                            }
{                                                                 }
{       Documentation and updated versions:                       }
{                                                                 }
{               http://www.bsalsa.com                             }
{*****************************************************************}
{LICENSE:
THIS SOFTWARE IS PROVIDED TO YOU "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED INCLUDING BUT NOT LIMITED TO THE APPLIED
WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
YOU ASSUME THE ENTIRE RISK AS TO THE ACCURACY AND THE USE OF THE SOFTWARE
AND ALL OTHER RISK ARISING OUT OF THE USE OR PERFORMANCE OF THIS SOFTWARE
AND DOCUMENTATION. [YOUR NAME] DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SYSTEMS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SYSTEMS. VSOFT SPECIFICALLY
DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use/ change/ modify the component under 4 conditions:
1. In your web site, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@bsalsa.com) any code change in the unit  for the benefit
   of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}

unit MimeFilter;
{For more info about MimeFilter:
 http://www.bsalsa.com/mime_filter.html
 http://msdn2.microsoft.com/en-us/library/aa767743.aspx
 http://msdn2.microsoft.com/en-us/library/Aa767916.aspx
 }

{$DEFINE USE_DebugString}
{ $DEFINE USE_TimeOut}

interface

uses
  Classes, ActiveX, ComObj, Windows, UrlMon, Forms;

const
{$IFDEF USE_TimeOut}
  TimeOut = 5000;
{$ENDIF}
  Class_StrMimeFilter: TGUID = '{0EB00690-8FA1-11D3-96C7-829E3EA50C29}';
// Create your own GUID (in Delphi IDE use Ctrl-Shift-G)

type
  TStrMimeFilter = class(TComObject,
      IInternetProtocol, //http://msdn2.microsoft.com/en-us/library/aa767883.aspx
      IInternetProtocolRoot, //http://msdn2.microsoft.com/en-us/library/aa767859.aspx
      IInternetProtocolSink) //http://msdn2.microsoft.com/en-us/library/aa767768.aspx
  private
    CacheFileName: string;
    Url: PWideChar;
    DataStream: IStream;
    UrlMonProtocol: IInternetProtocol;
    UrlMonProtocolSink: IInternetProtocolSink;
    Written, TotalSize: Integer;
{$IFDEF USE_TimeOut}
    StartedTime: DWORD;
{$ENDIF}
  protected
     { IInternetProtocolSink Methods }
    function Switch(const ProtocolData: TProtocolData): HResult; stdcall;
    function ReportProgress(ulStatusCode: ULONG; szStatusText: LPCWSTR): HResult; stdcall;
    function ReportData(grfBSCF: DWORD; ulProgress, ulProgressMax: ULONG): HResult; stdcall;
    function ReportResult(hrResult: HResult; dwError: DWORD; szResult: LPCWSTR): HResult; stdcall;
     { IInternetProtocolRoot Interface}
    function Start(szUrl: PWideChar; OIProtSink: IInternetProtocolSink;
      OIBindInfo: IInternetBindInfo; grfPI, dwReserved: DWORD): HResult; stdcall;
    function Continue(const ProtocolData: TProtocolData): HResult; stdcall;
    function Abort(hrReason: HResult; dwOptions: DWORD): HResult; stdcall;
    function Terminate(dwOptions: DWORD): HResult; stdcall;
    function Suspend: HResult; stdcall;
    function Resume: HResult; stdcall;
     { IInternetProtocol Methods}
    function Read(pv: Pointer; cb: ULONG; out cbRead: ULONG): HResult; stdcall;
    function Seek(dlibMove: LARGE_INTEGER; dwOrigin: DWORD;
      out libNewPosition: ULARGE_INTEGER): HResult; stdcall;
    function LockRequest(dwOptions: DWORD): HResult; stdcall;
    function UnlockRequest: HResult; stdcall;
  end;

var
  StrIn: string;
  StrOut: string;
  OutUrl: PWideChar;

implementation

uses
  AxCtrls, WinInet, SysUtils, ComServ;

 { IInternetProtocol Methods}

function TStrMimeFilter.Read(pv: Pointer; cb: ULONG; out cbRead: ULONG): HResult;
var
  hr: HResult;
begin
(* All data is avaiable, so we just keep reading while Written<totalsize *)
  hr := DataStream.Read(pv, cb, @cbRead);
  if hr <> S_OK then
  begin
{$IFDEF USE_DebugString}
    OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
    Abort(hr, 0); //An error was found so we should stop the com server.
  end;
  Inc(Written, cbRead);
  if (Written = TotalSize) then
    Result := S_FALSE
  else
  begin
    Result := S_OK;
    Abort(hr, 0);
  end;
end;

function TStrMimeFilter.Seek(dlibMove: LARGE_INTEGER; dwOrigin: DWORD;
  out libNewPosition: ULARGE_INTEGER): HResult;
begin
  Result := UrlMonProtocol.Seek(dlibMove, dwOrigin, libNewPosition);
  if Result <> S_OK then
  begin
{$IFDEF USE_DebugString}
    OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
    Abort(Result, 0);
  end;
end;

function TStrMimeFilter.LockRequest(dwOptions: DWORD): HResult;
begin
  Result := UrlMonProtocol.LockRequest(dwOptions);
  if Result <> S_OK then
  begin
{$IFDEF USE_DebugString}
    OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
    Abort(Result, 0);
  end;
end;

function TStrMimeFilter.UnlockRequest: HResult;
begin
  Result := UrlMonProtocol.UnlockRequest;
  if Result <> S_OK then
  begin
{$IFDEF USE_DebugString}
    OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
    Abort(Result, 0);
  end;
end;

 { IInternetProtocolSink Methods }

function TStrMimeFilter.ReportData(grfBSCF: DWORD; ulProgress,
  ulProgressMax: ULONG): HResult;
var
  TS: TStringStream;
  Dummy: Int64;
  hr: HResult;
  ReadTotal: ULONG;
  S: string;
  Fname: array[0..512] of Char;
  p: array[0..1000] of Char;
{$IFDEF USE_TimeOut}
  CurrentTime: DWORD;
  DiffTime: DWORD;
{$ENDIF}
begin
  (* This method is must likely called long before the file is downloaded, so
   ulProgressMax will be zero and ulProgress not tell anything reliable about
   the amount of data available. Instead you can use the outcoming Result of
   call to UrlMonProtocol.Read:

   S_OK : The read was successful, but there is still additional data available.
   S_FALSE : All the data has been completely downloaded.

   so we just repeat reading until we receive S_FALSE or an error:
   INET_E_DATA_NOT_AVAILABLE or INET_E_DOWNLOAD_FAILURE. *)

  Ts := TStringStream.Create('');
  repeat
    hr := UrlMonProtocol.Read(@P, SizeOf(p), ReadTotal);
    Ts.Write(P, ReadTotal);
    Application.ProcessMessages;
{$IFDEF USE_TimeOut}
    CurrentTime := GetTickCount; //I created a TimeOut but it's stupid
    DiffTime := CurrentTime - StartedTime;
    if (DiffTime >= TimeOut) then
      hr := S_FALSE;
{$ENDIF}
    Application.Processmessages;
  until (hr = S_FALSE) or (hr = INET_E_DOWNLOAD_FAILURE) or
    (hr = INET_E_DATA_NOT_AVAILABLE);

  if hr = S_FALSE then
  begin
(* Some pages like www.msn.com are not Written to cache, so we make a temporary
entry and call ReportProgress with CACHEFILENAMEAVAILABLE. *)
    if CacheFilename = '' then
    begin
      if not CreateUrlCacheEntry(@url, ts.Size, Pchar('htm'), FName, 0) then
      begin
{$IFDEF USE_DebugString}
        OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
      end;
      TMemoryStream(ts).SaveToFile(Fname);
      StringToWideChar(StrPas(FName), @FName, SizeOf(FName));
      if Failed(ReportProgress(BINDSTATUS_CACHEFILENAMEAVAILABLE, @FName)) then
      begin
{$IFDEF USE_DebugString}
        OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
        Abort(E_FAIL, 0);
      end;
    end;
///************************************
///*  FILTER DATA HERE:
    S := StringReplace(Ts.DataString, StrIn, StrOut, [rfReplaceAll, rfIgnoreCase]);
    ts.Size := 0;
    ts.WriteString(S);
///************************************

////***** Debug only ******************
  //  ts.Seek(0, 0);
  //  form1.Memo1.Lines.LoadFromStream(TS);
///************************************

    TotalSize := Ts.Size;
    ts.Seek(0, 0);
    if Failed(CreateStreamOnHGlobal(0, True, DataStream)) then
    begin
{$IFDEF USE_DebugString}
      OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
      Abort(E_FAIL, 0);
    end;

    TOleStream.Create(DataStream).CopyFrom(ts, ts.Size);
    ts.Free;
    DataStream.Seek(0, STREAM_SEEK_SET, Dummy);
(* Inform Transaction handler that all data is ready for the browser: *)
    if Failed(UrlMonProtocolSink.ReportData(BSCF_FIRSTDATANOTIFICATION or
      BSCF_LASTDATANOTIFICATION or BSCF_DATAFULLYAVAILABLE, TotalSize, Totalsize)) then
    begin
{$IFDEF USE_DebugString}
      OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
      Abort(E_FAIL, 0);
    end;
 (* Here transaction handler call our Read Method -> *)
    if Failed(UrlMonProtocolSink.ReportResult(S_OK, S_OK, nil)) then
    begin
{$IFDEF USE_DebugString}
      OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
      Abort(E_FAIL, 0);
    end;
(* Report Result OK after sending all data to browser *)
  end
  else
  begin
    Abort(hr, 0); //On Error: INET_E_DOWNLOAD_FAILURE or INET_E_DATA_NOT_AVAILABLE
  end;
  Result := S_OK;
end;

function TStrMimeFilter.ReportResult(hrResult: HResult; dwError: DWORD;
  szResult: LPCWSTR): HResult;
begin
  Result := UrlMonProtocolSink.ReportResult(hrResult, dwError, szResult);
  if Result <> S_OK then
  begin
{$IFDEF USE_DebugString}
    OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
  end;
end;

function TStrMimeFilter.ReportProgress(ulStatusCode: ULONG;
  szStatusText: LPCWSTR): HResult;
begin
  if ulStatusCode = BINDSTATUS_CACHEFILENAMEAVAILABLE then
    CacheFileName := SzStatusText;
(* szStatusText contains the name of the cache-file where the downloaded
data will be stored. *)
  Result := UrlMonProtocolSink.ReportProgress(ulStatusCode, szStatusText);
  if Result <> S_OK then
  begin
{$IFDEF USE_DebugString}
    OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
    Abort(Result, 0);
  end;
(* We pass all information on. *)
(* The transaction handler now call our ReportData -> *)
end;

function TStrMimeFilter.Switch(const ProtocolData: TProtocolData): HResult;
begin
  Result := UrlMonProtocolSink.Switch(ProtocolData);
  if Result <> S_OK then
  begin
{$IFDEF USE_DebugString}
    OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
    Abort(Result, 0);
  end;
end;

{ IInternetProtocolRoot Interface}

function TStrMimeFilter.Abort(hrReason: HResult; dwOptions: DWORD): HResult;
begin
  Result := UrlMonProtocol.Abort(hrReason, dwOptions);
{$WARNINGS OFF}
  if (Result = E_PENDING) then
    Exit;
{$WARNINGS ON}
  if (Result <> S_OK) then
  begin
{$IFDEF USE_DebugString}
    OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
  end;
end;

function TStrMimeFilter.Continue(const ProtocolData: TProtocolData): HResult;
begin
  Result := UrlMonProtocol.Continue(ProtocolData);
{$WARNINGS OFF}
  if (Result = E_PENDING) then
    Exit;
{$WARNINGS ON}
  if (Result <> S_OK) then
  begin
{$IFDEF USE_DebugString}
    OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
    Abort(Result, 0);
  end;
end;

function TStrMimeFilter.Resume: HResult;
begin
  Result := E_NOTIMPL; //Not Implemented by MS
end;

function TStrMimeFilter.Start(szUrl: PWideChar; OIProtSink: IInternetProtocolSink;
  OIBindInfo: IInternetBindInfo; grfPI, dwReserved: DWORD): HResult;
var
  Fetched: Cardinal;
begin
  CacheFileName := '';
  TotalSize := 0;
  Written := 0;
{$IFDEF USE_TimeOut}
  StartedTime := GetTickCount;
{$ENDIF}
(* Get an interface to transaction handlers IInternetProtocol and IInternetProtocolSink.
   I prefer the easy delphi-way: *)
  UrlMonProtocol := OIProtSink as IInternetProtocol;
  UrlMonProtocolSink := OIProtSink as IInternetProtocolSink;
(* If the page is not Written to cache, our ReportProgress is not
   called with CACHEFILENAMEAVAILABLE. We grab the url here so
   we later can create a proper temporary cachefile. Since we are
   in a mimefilter szURl don't have the url, so we use GetBindString. *)

  Result := OIBindinfo.GetBindString(BINDSTRING_URL, @Url, 1, Fetched);
  OutUrl := Url;
  if Result <> S_OK then
  begin
{$IFDEF USE_DebugString}
    OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
  end;
  Result := S_OK;
(* The transaction handler now call our ReportProgress -> *)
end;

function TStrMimeFilter.Suspend: HResult;
begin
  Result := E_NOTIMPL; //Not Implemented by MS
end;

function TStrMimeFilter.Terminate(dwOptions: DWORD): HResult;
begin
  if DataStream <> nil then
    DataStream._Release;
  if UrlMonProtocolSink <> nil then
    UrlMonProtocolSink._Release;
  Result := UrlmonProtocol.Terminate(dwOptions);
  if (Result <> S_OK) then
  begin
{$IFDEF USE_DebugString}
    OutputDebugString(PChar(SysErrorMessage(GetLastError)));
{$ENDIF}
    Abort(Result, 0);
  end;
  ComServer.UIInteractive := False; // If you still have Active Com objects then We will hide the UI
end;

initialization
  TComObjectFactory.Create(ComServer, TStrMimeFilter, Class_StrMimeFilter,
    'StrMimeFilter', 'StrMimeFilter', ciMultiInstance, tmApartment);

finalization

end.

