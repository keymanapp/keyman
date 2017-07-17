//*************************************************************************
//                                                                        *
//                    IEDownload_Acc                                      *
//                       For Delphi                                       *
//                                                                        *
//                       Freeware unit                                    *
//                            by                                          *
//                     and Eran Bodankin                                  *
//                     bsalsa@gmail.com                                   *
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

unit IEDownloadAcc;

interface

{$I EWB.inc}

uses
  ActiveX, SysUtils, ShlObj, Windows, UrlMon, EwbIEConst;

{$IFDEF DELPHI6_UP}
type
 {Authentiae }
  _AUTHENTICATEF = (
    AUTHENTICATEF_PROXY = $00000001,
    AUTHENTICATEF_BASIC = $00000002,
    AUTHENTICATEF_HTTP = $00000004);
  AUTHENTICATEF = _AUTHENTICATEF;

 {PutProperty }
  _MONIKERPROPERTY = (
    MIMETYPEPROP = 0,
    USE_SRC_URL = $00000001,
    CLASSIDPROP = $00000002,
    TRUSTEDDOWNLOADPROP = $00000003,
    POPUPLEVELPROP = $00000004);
  MONIKERPROPERTY = _MONIKERPROPERTY;

    {CIP_STATUS }
  _CIP_STATUS = (
    CIP_DISK_FULL = 0,
    CIP_ACCESS_DENIED = 1,
    CIP_NEWER_VERSION_EXISTS = 2,
    CIP_OLDER_VERSION_EXISTS = 3,
    CIP_NAME_CONFLICT = 4,
    CIP_TRUST_VERIFICATION_COMPONENT_MISSING = 5,
    CIP_EXE_SELF_REGISTERATION_TIMEOUT = 6,
    CIP_UNSAFE_TO_ABORT = 7,
    CIP_NEED_REBOOT = 8,
    CIP_NEED_REBOOT_UI_PERMISSION = 9);
  CIP_STATUS = _CIP_STATUS;

   {BSCF Enumerated Type}
  _BSCF = (
    BSCF_FIRSTDATANOTIFICATION = $00000001,
    BSCF_INTERMEDIATEDATANOTIFICATION = $00000002,
    BSCF_LASTDATANOTIFICATION = $00000004,
    BSCF_DATAFULLYAVAILABLE = $00000008,
    BSCF_AVAILABLEDATASIZEUNKNOWN = $00000010,
    BSCF_SKIPDRAINDATAFORFILEURLS = $00000020);
  BSCF = _BSCF;


type
  pauthinfo = ^TAUTHENTICATEINFO;
  _tagAUTHENTICATEINFO = record
    dwFlags: LongInt;
    dwReserved: LongInt;
  end;
  TAUTHENTICATEINFO = _tagAUTHENTICATEINFO;
  AUTHENTICATEINFO = _tagAUTHENTICATEINFO;

type
  IAuthenticateEx = interface(IAuthenticate)
    ['{2AD1EDAF-D83D-48B5-9ADF-03DBE19F53BD}']
    function AuthenticateEx(out phwnd: HWND; out pszUsername,
      pszPassword: LPWSTR; var pauthinfo: AUTHENTICATEINFO): HResult; stdcall;
  end;
{$ENDIF}


type
  IBindStatusCallbackEx = interface(IBindStatusCallback)
    ['{AAA74EF9-8EE7-4659-88D9-F8C504DA73CC}']
    function GetBindInfoEx(out grfBINDF: DWORD; var pbindinfo: BINDINFO;
      out grfBINDF2: DWORD; out pdwReserved: DWORD): HResult; stdcall;
  end;

type
  ICatalogFileInfo = interface(IUnknown)
    ['{711C7600-6B48-11D1-B403-00AA00B92AF1}']
    function GetCatalogFile(out ppszCatalogFile: LPSTR): HResult;
    function GetJavaTrust(out ppJavaTrust: Pointer): HResult;
  end;

type
  IHttpNegotiate2 = interface(IHttpNegotiate)
    ['{4F9F9FCB-E0F4-48EB-B7AB-FA2EA9365CB4}']
    function GetRootSecurityId(var SecurityIdBuffer: TByteArray; var
      BufferSize: DWord; dwReserved: DWORD): HResult; stdcall;
  end;

type
  IHttpNegotiate3 = interface(IHttpNegotiate2)
    ['{57b6c80a-34c2-4602-bc26-66a02fc57153}']
    function GetSerializedClientCertContext(out ppbCert: Byte;
      var pcbCert: DWORD): HResult; stdcall;
  end;
{$IFDEF DELPHI6_UP}
type
  IMonikerProp = interface(IUnknown)
    ['{A5CA5F7F-1847-4D87-9C5B-918509F7511D}']
    function PutProperty(mkp: MONIKERPROPERTY; val: LPCWSTR): HResult; stdcall;
  end;
{$ENDIF}



type
  IBindHost = interface(IUnknown)
    ['{fc4801a1-2ba9-11cf-a229-00aa003d7352}']
    function CreateMoniker(szName: POLEStr; BC: IBindCtx; out mk: IMoniker;
      dwReserved: DWORD): HResult; stdcall;
    function MonikerBindToStorage(Mk: IMoniker; BC: IBindCtx; BSC:
      IBindStatusCallback; const iid: TGUID; out pvObj{$IFNDEF DELPHI8_UP}: Pointer{$ENDIF}): HResult; stdcall;
    function MonikerBindToObject(Mk: IMoniker; BC: IBindCtx; BSC:
      IBindStatusCallback; const iid: TGUID; out pvObj{$IFNDEF DELPHI8_UP}: Pointer{$ENDIF}): HResult; stdcall;
  end;

type
  INTERNET_PER_CONN_OPTION = record
    dwOption: DWORD;
    Value: record
      case Integer of
        1: (dwValue: DWORD);
        2: (pszValue: PChar); {Unicode/ANSI}
        3: (ftValue: TFileTime);
    end;
  end;

  LPINTERNET_PER_CONN_OPTION = ^INTERNET_PER_CONN_OPTION;
  INTERNET_PER_CONN_OPTION_List = record
    dwSize: DWORD;
    pszConnection: LPTSTR;
    dwOptionCount: DWORD;
    dwOptionError: DWORD;
    intOptions: LPINTERNET_PER_CONN_OPTION;
  end;
  LPINTERNET_PER_CONN_OPTION_List = ^INTERNET_PER_CONN_OPTION_List;



function CreateURLMonikerEx(MkCtx: IMoniker; szURL: LPCWSTR;
  out mk: IMoniker; dwFlags: DWORD): HRESULT; stdcall; external UrlMonLib;

implementation

end.
