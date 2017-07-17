//***********************************************************
//               UrlSearchHook Demo (June 1, 2000)          *
//                                                          *
//                         ver. 1.0                         *
//                                                          *
//               For Delphi 5 - 2009                        *
//                                                          *
//                            by                            *
//                     Per Lindsø Larsen                    *
//                   per.lindsoe@larsen.dk                  *
//                                                          *
//                                                          *
//                                                          *
//        Updated versions:                                 *
//                                                          *
//               http://www.bsalsa.com                      *
//***********************************************************

unit _SearchHook;

interface

uses
  Registry, SysUtils, Windows, ActiveX, Classes, ComObj, Shlobj;

type

  TUrlSearchFactory = class(TComObjectFactory)
  private
    procedure AddKeys;
    procedure RemoveKeys;
  public
    procedure UpdateRegistry(Register: Boolean); override;
  end;

  TUrlSearch = class(TComObject, IUrlSearchHook)
  protected
    function Translate(lpwszSearchURL: PWideChar; cchBufferSize: DWORD): HResult; stdcall;
  end;

const
  Class_UrlSearch: TGUID = '{A4E833A2-3633-11D4-8689-444553540000}';
//Create your own unique identifier - in Delphi IDE: Ctrl-Shift-G


 {DE}DejaNews = 'http://www.deja.com/[ST_rn=ps]/qs.xp?ST=PS&svcclass=dnyr&QRY=%s&defaultOp=AND&DBS=1&OP=dnquery.xp&LNG=ALL&subjects=&groups=&authors=&fromdate=&todate=&showsort=score&maxhits=25';
 {AL}AltaVista = 'http://www.altavista.com/cgi-bin/query?pg=q&sc=on&hl=on&act=2006&par=0&q=%s&kl=XX&stype=stext';
 {YA}Yahoo = 'http://search.yahoo.com/bin/search?p=%s';
 {LY}Lycos = 'http://www-english.lycos.com/srch/?lpv=1&loc=searchhp&query=%s';
 {MI}Microsoft = 'http://search.microsoft.com/us/SearchMS.asp?so=RECCNT&qu=%s&boolean=ALL&intCat=0&intCat=1&intCat=2&intCat=3&intCat=4&intCat=5&intCat=6&intCat=7&intCat=8&intCat=9&p=1&nq=NEW&LOC=';
 {ME}Mers = 'http://www.mers.com/cgi-bin/srchcgi.exe/EXECSEARCH?pageno=1&linktype=search&searchtext=%s&grouptext=';


implementation

uses ComServ;



function TUrlSearch.Translate(lpwszSearchURL: PWideChar;
  cchBufferSize: DWORD): HResult;
(*
Called by the browser when the browser cannot determine the protocol of a URL address.

lpwszSearchURL: Address of a wide character buffer that, on entry, contains the
                URL address for which the browser is trying to determine the
                protocol. On exit, this buffer contains the modified URL address
                if the method was successful. See the return value for more information.
cchBufferSize:  Size, in characters, of the buffer at lpwszSearchURL.
*)
var
  Header, S: string;
begin
  Result := E_FAIL;
  S := lpwszSearchUrl;
  Header := Uppercase(Copy(s, 1, 3));
  S := Copy(S, 4, Length(S));
  S := Trim(s);
  S := StringReplace(S, ' ', '+', [rfReplaceAll]);
  if Header = 'DE ' then S := format(DejaNews, [S]) else
    if Header = 'AL ' then S := format(Altavista, [S]) else
      if Header = 'YA ' then S := format(Yahoo, [S]) else
        if Header = 'LY ' then S := format(Lycos, [S]) else
          if Header = 'MI ' then S := format(Microsoft, [S]) else
            if Header = 'ME ' then S := format(Mers, [S]) else Exit;
  StringToWideChar(S, lpwszSearchUrl, (length(S) + 1) * 2);
  Result := S_OK;

(* Return Values:
S_OK:  The URL address was completely translated. The lpwszSearchURL parameter
       contains the full URL address. The browser will not call any other URL
       Search Hooks and will attempt to browse to the modified address.
S_FALSE  The URL address has been partially processed, but further translation
       is still required. The lpwszSearchURL parameter contains the result of
       the processing. The browser will continue executing the rest of the URL
       Search Hooks.
E_FAIL:  The URL address was not translated. The lpwszSearchURL parameter has
       not been modified. The browser will continue executing the rest of the
       URL Search Hooks.
*)
end;


procedure TUrlSearchFactory.UpdateRegistry(Register: Boolean);
begin
  inherited UpdateRegistry(Register);
  if Register then AddKeys else RemoveKeys;
end;

procedure TUrlSearchFactory.AddKeys;
var S: string;
begin
  S := GUIDToString(CLASS_UrlSearch);
  with TRegistry.Create do
  try
    if OpenKey('SOFTWARE\Microsoft\Internet Explorer\UrlSearchHooks', True) then
    begin
      WriteString(S, '');
      CloseKey;
    end;
  finally
    Free;
  end;
end;

procedure TUrlSearchFactory.RemoveKeys;
var S: string;
begin
  S := GUIDToString(CLASS_URLSEARCH);
  with TRegistry.Create do
  try
    OpenKey('Software\Microsoft\Internet Explorer\UrlSearchHooks', TRUE);
    DeleteValue(s);
    CloseKey;
  finally
    Free;
  end;
end;

initialization
  OleInitialize(nil);
  TUrlSearchFactory.Create(ComServer, TUrlSearch, CLASS_UrlSearch, 'UrlSearch', 'UrlSearch', ciMultiInstance);

finalization
  OleUninitialize;
end.

