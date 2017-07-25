{******************************************************************************}
{                                                                              }
{       Microsoft Windows Internet Extensions interface unit                   }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-1999 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{                                                                              }
{ The initial developer of the Pascal code is Rudy Velthuis                    }
{                                                                              }
{ Portions created by Marcel van Brakel are Copyright (C) 1999-2000            }
{ Rudy Velthuis. All Rights Reserved.                                          }
{                                                                              }
{ The original file is: WinInet.h released 16 May 1999.                        }
{ The original Pascal code is: WinInet.pas, released 27 May 1999.              }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project JEDI         }
{ APILIB home page, located at http://jedi-apilib.sourceforge.net              }
{                                                                              }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{                                                                              }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{******************************************************************************}

{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaWinInet;
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "wininet.h"'}
{$HPPEMIT ''}


{$IFNDEF JWA_OMIT_SECTIONS}
interface

uses
  JwaWinBase, JwaWinUser, JwaWinType;
{
  Set up Structure Packing to be 4 bytes
  for all wininet structures
}
{$ALIGN ON}
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}
// 
// internet types 
//
type

  PHINTERNET = ^HINTERNET;
  {$EXTERNALSYM HINTERNET}
  HINTERNET = Pointer;



  PInternetPort = ^TInternetPort;
  PINTERNET_PORT = ^INTERNET_PORT;
  {$EXTERNALSYM INTERNET_PORT}
  INTERNET_PORT = Word;
  TInternetPort = INTERNET_PORT;


// 
// Internet APIs 
//
 
// manifests 

const
  {EXTERNALSYM INTERNET_INVALID_PORT_NUMBER}
  INTERNET_INVALID_PORT_NUMBER    = 0;          // use the protocol-specific default 
 
  {EXTERNALSYM INTERNET_DEFAULT_FTP_PORT}
  INTERNET_DEFAULT_FTP_PORT       = 21;         // default for FTP servers
  {EXTERNALSYM INTERNET_DEFAULT_GOPHER_PORT}
  INTERNET_DEFAULT_GOPHER_PORT    = 70;         //    "     "  gopher " 
  {EXTERNALSYM INTERNET_DEFAULT_HTTP_PORT}
  INTERNET_DEFAULT_HTTP_PORT      = 80;         //    "     "  HTTP   " 
  {EXTERNALSYM INTERNET_DEFAULT_HTTPS_PORT}
  INTERNET_DEFAULT_HTTPS_PORT     = 443;        //    "     "  HTTPS  " 
  {$EXTERNALSYM INTERNET_DEFAULT_SOCKS_PORT}
  INTERNET_DEFAULT_SOCKS_PORT     = 1080;       // default for SOCKS firewall servers. 

// 
// maximum field lengths (arbitrary) 
// 
  {$EXTERNALSYM INTERNET_MAX_HOST_NAME_LENGTH}
  INTERNET_MAX_HOST_NAME_LENGTH   = 256;
  {$EXTERNALSYM INTERNET_MAX_USER_NAME_LENGTH}
  INTERNET_MAX_USER_NAME_LENGTH   = 128;
  {$EXTERNALSYM INTERNET_MAX_PASSWORD_LENGTH}
  INTERNET_MAX_PASSWORD_LENGTH    = 128;

{$IFNDEF JWA_INCLUDEMODE}
  {..$NODEFINE DWORD_PTR}
  //DWORD_PTR = DWORD;
{$ENDIF JWA_INCLUDEMODE}

  {$EXTERNALSYM INTERNET_MAX_PORT_NUMBER_LENGTH}
  INTERNET_MAX_PORT_NUMBER_LENGTH = 5;          // INTERNET_PORT is unsigned short 
  {$EXTERNALSYM INTERNET_MAX_PORT_NUMBER_VALUE}
  INTERNET_MAX_PORT_NUMBER_VALUE  = 65535;      // maximum unsigned short value 
  {$EXTERNALSYM INTERNET_MAX_PATH_LENGTH}
  INTERNET_MAX_PATH_LENGTH        = 2048;
  {$EXTERNALSYM INTERNET_MAX_SCHEME_LENGTH}
  INTERNET_MAX_SCHEME_LENGTH      = 32;         // longest protocol name length 
  {$EXTERNALSYM INTERNET_MAX_URL_LENGTH}
  INTERNET_MAX_URL_LENGTH         = INTERNET_MAX_SCHEME_LENGTH + 
                                    Length('://') + 1 +
                                    INTERNET_MAX_PATH_LENGTH;

// values returned by InternetQueryOption() with INTERNET_OPTION_KEEP_CONNECTION:

  {$EXTERNALSYM INTERNET_KEEP_ALIVE_UNKNOWN}
  INTERNET_KEEP_ALIVE_UNKNOWN     = DWORD(-1);
  {$EXTERNALSYM INTERNET_KEEP_ALIVE_ENABLED}
  INTERNET_KEEP_ALIVE_ENABLED     = 1;
  {$EXTERNALSYM INTERNET_KEEP_ALIVE_DISABLED}
  INTERNET_KEEP_ALIVE_DISABLED    = 0;

// flags returned by InternetQueryOption() with INTERNET_OPTION_REQUEST_FLAGS

  {$EXTERNALSYM INTERNET_REQFLAG_FROM_CACHE}
  INTERNET_REQFLAG_FROM_CACHE     = $00000001;  // response came from cache
  {$EXTERNALSYM INTERNET_REQFLAG_ASYNC}
  INTERNET_REQFLAG_ASYNC          = $00000002;  // request was made asynchronously
  {$EXTERNALSYM INTERNET_REQFLAG_VIA_PROXY}
  INTERNET_REQFLAG_VIA_PROXY      = $00000004;  // request was made via a proxy
  {$EXTERNALSYM INTERNET_REQFLAG_NO_HEADERS}
  INTERNET_REQFLAG_NO_HEADERS     = $00000008;  // orginal response contained no headers
  {$EXTERNALSYM INTERNET_REQFLAG_PASSIVE}
  INTERNET_REQFLAG_PASSIVE        = $00000010;  // FTP: passive-mode connection
  {$EXTERNALSYM INTERNET_REQFLAG_CACHE_WRITE_DISABLED}
  INTERNET_REQFLAG_CACHE_WRITE_DISABLED = $00000040;  // HTTPS: this request not cacheable
  {$EXTERNALSYM INTERNET_REQFLAG_NET_TIMEOUT}
  INTERNET_REQFLAG_NET_TIMEOUT    = $00000080;  // w/ _FROM_CACHE: net request timed out

// flags common to open functions (not InternetOpen()):

  {$EXTERNALSYM INTERNET_FLAG_RELOAD}
  INTERNET_FLAG_RELOAD            = $80000000;  // retrieve the original item

// flags for InternetOpenUrl():

  {$EXTERNALSYM INTERNET_FLAG_RAW_DATA}
  INTERNET_FLAG_RAW_DATA          = $40000000;  // FTP/gopher find: receive the item as raw (structured) data
  {$EXTERNALSYM INTERNET_FLAG_EXISTING_CONNECT}
  INTERNET_FLAG_EXISTING_CONNECT  = $20000000;  // FTP: use existing InternetConnect handle for server if possible

// flags for InternetOpen():

  {$EXTERNALSYM INTERNET_FLAG_ASYNC}
  INTERNET_FLAG_ASYNC             = $10000000;  // this request is asynchronous (where supported)

// protocol-specific flags:

  {$EXTERNALSYM INTERNET_FLAG_PASSIVE}
  INTERNET_FLAG_PASSIVE           = $08000000;  // used for FTP connections

// additional cache flags

  {$EXTERNALSYM INTERNET_FLAG_NO_CACHE_WRITE}
  INTERNET_FLAG_NO_CACHE_WRITE    = $04000000;  // don't write this item to the cache
  {$EXTERNALSYM INTERNET_FLAG_DONT_CACHE}
  INTERNET_FLAG_DONT_CACHE        = INTERNET_FLAG_NO_CACHE_WRITE;
  {$EXTERNALSYM INTERNET_FLAG_MAKE_PERSISTENT}
  INTERNET_FLAG_MAKE_PERSISTENT   = $02000000;  // make this item persistent in cache
  {$EXTERNALSYM INTERNET_FLAG_FROM_CACHE}
  INTERNET_FLAG_FROM_CACHE        = $01000000;  // use offline semantics
  {$EXTERNALSYM INTERNET_FLAG_OFFLINE}
  INTERNET_FLAG_OFFLINE           = INTERNET_FLAG_FROM_CACHE;

// additional flags

  {$EXTERNALSYM INTERNET_FLAG_SECURE}
  INTERNET_FLAG_SECURE            = $00800000;  // use PCT/SSL if applicable (HTTP)
  {$EXTERNALSYM INTERNET_FLAG_KEEP_CONNECTION}
  INTERNET_FLAG_KEEP_CONNECTION   = $00400000;  // use keep-alive semantics
  {$EXTERNALSYM INTERNET_FLAG_NO_AUTO_REDIRECT}
  INTERNET_FLAG_NO_AUTO_REDIRECT  = $00200000;  // don't handle redirections automatically
  {$EXTERNALSYM INTERNET_FLAG_READ_PREFETCH}
  INTERNET_FLAG_READ_PREFETCH     = $00100000;  // do background read prefetch
  {$EXTERNALSYM INTERNET_FLAG_NO_COOKIES}
  INTERNET_FLAG_NO_COOKIES        = $00080000;  // no automatic cookie handling
  {$EXTERNALSYM INTERNET_FLAG_NO_AUTH}
  INTERNET_FLAG_NO_AUTH           = $00040000;  // no automatic authentication handling
  {$EXTERNALSYM INTERNET_FLAG_CACHE_IF_NET_FAIL}
  INTERNET_FLAG_CACHE_IF_NET_FAIL = $00010000;  // return cache file if net request fails

// Security Ignore Flags, Allow HttpOpenRequest to overide
//  Secure Channel (SSL/PCT) failures of the following types.

  {$EXTERNALSYM INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTP}
  INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTP   = $00008000; // ex: https:// to http://
  {$EXTERNALSYM INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS}
  INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS  = $00004000; // ex: http:// to https://
  {$EXTERNALSYM INTERNET_FLAG_IGNORE_CERT_DATE_INVALID}
  INTERNET_FLAG_IGNORE_CERT_DATE_INVALID  = $00002000; // expired X509 Cert.
  {$EXTERNALSYM INTERNET_FLAG_IGNORE_CERT_CN_INVALID}
  INTERNET_FLAG_IGNORE_CERT_CN_INVALID    = $00001000; // bad common name in X509 Cert.

// more caching flags

  {$EXTERNALSYM INTERNET_FLAG_RESYNCHRONIZE}
  INTERNET_FLAG_RESYNCHRONIZE      = $00000800; // asking wininet to update an item if it is newer
  {$EXTERNALSYM INTERNET_FLAG_HYPERLINK}
  INTERNET_FLAG_HYPERLINK          = $00000400; // asking wininet to do hyperlinking semantic which works right for scripts
  {$EXTERNALSYM INTERNET_FLAG_NO_UI}
  INTERNET_FLAG_NO_UI              = $00000200; // no cookie popup
  {$EXTERNALSYM INTERNET_FLAG_PRAGMA_NOCACHE}
  INTERNET_FLAG_PRAGMA_NOCACHE     = $00000100; // asking wininet to add "pragma: no-cache"
  {$EXTERNALSYM INTERNET_FLAG_CACHE_ASYNC}
  INTERNET_FLAG_CACHE_ASYNC        = $00000080; // ok to perform lazy cache-write
  {$EXTERNALSYM INTERNET_FLAG_FORMS_SUBMIT}
  INTERNET_FLAG_FORMS_SUBMIT       = $00000040; // this is a forms submit
  {$EXTERNALSYM INTERNET_FLAG_FWD_BACK}
  INTERNET_FLAG_FWD_BACK           = $00000020; // fwd-back button op
  {$EXTERNALSYM INTERNET_FLAG_NEED_FILE}
  INTERNET_FLAG_NEED_FILE          = $00000010; // need a file for this request
  {$EXTERNALSYM INTERNET_FLAG_MUST_CACHE_REQUEST}
  INTERNET_FLAG_MUST_CACHE_REQUEST = INTERNET_FLAG_NEED_FILE;

// FTP

// manifests

  {$EXTERNALSYM FTP_TRANSFER_TYPE_UNKNOWN}
  FTP_TRANSFER_TYPE_UNKNOWN   = $00000000;
  {$EXTERNALSYM FTP_TRANSFER_TYPE_ASCII}
  FTP_TRANSFER_TYPE_ASCII     = $00000001;
  {$EXTERNALSYM FTP_TRANSFER_TYPE_BINARY}
  FTP_TRANSFER_TYPE_BINARY    = $00000002;

  {$EXTERNALSYM FTP_TRANSFER_TYPE_MASK}
  FTP_TRANSFER_TYPE_MASK      = FTP_TRANSFER_TYPE_ASCII or FTP_TRANSFER_TYPE_BINARY;

// flags for FTP

  {$EXTERNALSYM INTERNET_FLAG_TRANSFER_ASCII}
  INTERNET_FLAG_TRANSFER_ASCII    = FTP_TRANSFER_TYPE_ASCII;     // = $00000001
  {$EXTERNALSYM INTERNET_FLAG_TRANSFER_BINARY}
  INTERNET_FLAG_TRANSFER_BINARY   = FTP_TRANSFER_TYPE_BINARY;    // = $00000002

// flags field masks

  {$EXTERNALSYM SECURITY_INTERNET_MASK}
  SECURITY_INTERNET_MASK  = INTERNET_FLAG_IGNORE_CERT_CN_INVALID or
                            INTERNET_FLAG_IGNORE_CERT_DATE_INVALID or
                            INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS or
                            INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTP;

  {$EXTERNALSYM INTERNET_FLAGS_MASK}
  INTERNET_FLAGS_MASK     = INTERNET_FLAG_RELOAD or
                            INTERNET_FLAG_RAW_DATA or
                            INTERNET_FLAG_EXISTING_CONNECT or
                            INTERNET_FLAG_ASYNC or
                            INTERNET_FLAG_PASSIVE or
                            INTERNET_FLAG_NO_CACHE_WRITE or
                            INTERNET_FLAG_MAKE_PERSISTENT or
                            INTERNET_FLAG_FROM_CACHE or
                            INTERNET_FLAG_SECURE or
                            INTERNET_FLAG_KEEP_CONNECTION or
                            INTERNET_FLAG_NO_AUTO_REDIRECT or
                            INTERNET_FLAG_READ_PREFETCH or
                            INTERNET_FLAG_NO_COOKIES or
                            INTERNET_FLAG_NO_AUTH or
                            INTERNET_FLAG_CACHE_IF_NET_FAIL or
                            SECURITY_INTERNET_MASK or
                            INTERNET_FLAG_RESYNCHRONIZE or
                            INTERNET_FLAG_HYPERLINK or
                            INTERNET_FLAG_NO_UI or
                            INTERNET_FLAG_PRAGMA_NOCACHE or
                            INTERNET_FLAG_CACHE_ASYNC or
                            INTERNET_FLAG_FORMS_SUBMIT or
                            INTERNET_FLAG_NEED_FILE or
                            INTERNET_FLAG_TRANSFER_BINARY or
                            INTERNET_FLAG_TRANSFER_ASCII or
                            INTERNET_FLAG_FWD_BACK;
// Note by translator: the following flag is not defined anywhere in the
// current platform SDK, so I excluded it. Older headers don't have it.
//                          or INTERNET_FLAG_BGUPDATE


  {$EXTERNALSYM INTERNET_ERROR_MASK_INSERT_CDROM}
  INTERNET_ERROR_MASK_INSERT_CDROM = $1;
  {$EXTERNALSYM INTERNET_ERROR_MASK_COMBINED_SEC_CERT}
  INTERNET_ERROR_MASK_COMBINED_SEC_CERT = $2;
  {$EXTERNALSYM INTERNET_ERROR_MASK_NEED_MSN_SSPI_PKG}
  INTERNET_ERROR_MASK_NEED_MSN_SSPI_PKG = $4;
  {$EXTERNALSYM INTERNET_ERROR_MASK_LOGIN_FAILURE_DISPLAY_ENTITY_BODY}
  INTERNET_ERROR_MASK_LOGIN_FAILURE_DISPLAY_ENTITY_BODY = $8;

  {$EXTERNALSYM INTERNET_OPTIONS_MASK}
  INTERNET_OPTIONS_MASK   = not INTERNET_FLAGS_MASK;

// common per-API flags (new APIs)

  {$EXTERNALSYM WININET_API_FLAG_ASYNC}
  WININET_API_FLAG_ASYNC          = $00000001;  // force async operation
  {$EXTERNALSYM WININET_API_FLAG_SYNC}
  WININET_API_FLAG_SYNC           = $00000004;  // force sync operation
  {$EXTERNALSYM WININET_API_FLAG_USE_CONTEXT}
  WININET_API_FLAG_USE_CONTEXT    = $00000008;  // use value supplied in dwContext (even if 0)

// INTERNET_NO_CALLBACK - if this value is presented as the dwContext parameter
// then no call-backs will be made for that API

  {$EXTERNALSYM INTERNET_NO_CALLBACK}
  INTERNET_NO_CALLBACK            = 0;

// structures/types

// INTERNET_SCHEME - enumerated URL scheme type

  {$EXTERNALSYM INTERNET_SCHEME_PARTIAL}
  INTERNET_SCHEME_PARTIAL    = -2;
  {$EXTERNALSYM INTERNET_SCHEME_UNKNOWN}
  INTERNET_SCHEME_UNKNOWN    = -1;
  {$EXTERNALSYM INTERNET_SCHEME_DEFAULT}
  INTERNET_SCHEME_DEFAULT    = 0;
  {$EXTERNALSYM INTERNET_SCHEME_FTP}
  INTERNET_SCHEME_FTP        = 1;
  {$EXTERNALSYM INTERNET_SCHEME_GOPHER}
  INTERNET_SCHEME_GOPHER     = 2;
  {$EXTERNALSYM INTERNET_SCHEME_HTTP}
  INTERNET_SCHEME_HTTP       = 3;
  {$EXTERNALSYM INTERNET_SCHEME_HTTPS}
  INTERNET_SCHEME_HTTPS      = 4;
  {$EXTERNALSYM INTERNET_SCHEME_FILE}
  INTERNET_SCHEME_FILE       = 5;
  {$EXTERNALSYM INTERNET_SCHEME_NEWS}
  INTERNET_SCHEME_NEWS       = 6;
  {$EXTERNALSYM INTERNET_SCHEME_MAILTO}
  INTERNET_SCHEME_MAILTO     = 7;
  {$EXTERNALSYM INTERNET_SCHEME_SOCKS}
  INTERNET_SCHEME_SOCKS      = 8;
  {$EXTERNALSYM INTERNET_SCHEME_JAVASCRIPT}
  INTERNET_SCHEME_JAVASCRIPT = 9;
  {$EXTERNALSYM INTERNET_SCHEME_VBSCRIPT}
  INTERNET_SCHEME_VBSCRIPT   = 10;
  {$EXTERNALSYM INTERNET_SCHEME_FIRST}
  INTERNET_SCHEME_FIRST    = INTERNET_SCHEME_FTP;
  {$EXTERNALSYM INTERNET_SCHEME_LAST}
  INTERNET_SCHEME_LAST     = INTERNET_SCHEME_VBSCRIPT;

type
  PINTERNET_SCHEME = ^INTERNET_SCHEME;
  {$EXTERNALSYM INTERNET_SCHEME}
  INTERNET_SCHEME = ULONG;


// INTERNET_ASYNC_RESULT - this structure is returned to the application via
// the callback with INTERNET_STATUS_REQUEST_COMPLETE. It is not sufficient to
// just return the result of the async operation. If the API failed then the
// app cannot call GetLastError() because the thread context will be incorrect.
// Both the value returned by the async API and any resultant error code are
// made available. The app need not check dwError if dwResult indicates that
// the API succeeded (in this case dwError will be ERROR_SUCCESS)

type
  PInternetAsyncResult = ^TInternetAsyncResult;
  {$EXTERNALSYM INTERNET_ASYNC_RESULT}
  INTERNET_ASYNC_RESULT = record // not packed!
    dwResult: DWORD;    // the HINTERNET, DWORD or BOOL return code from an async API
    dwError: DWORD;     // the error code if the API failed
  end;
  TInternetAsyncResult = INTERNET_ASYNC_RESULT;


// INTERNET_PROXY_INFO - structure supplied with INTERNET_OPTION_PROXY to get/
// set proxy information on a InternetOpen() handle

  PInternetProxyInfo = ^TInternetProxyInfo;
  {$EXTERNALSYM INTERNET_PROXY_INFO}
  INTERNET_PROXY_INFO = record // not packed!
    dwAccessType: DWORD;        // INTERNET_OPEN_TYPE_DIRECT, INTERNET_OPEN_TYPE_PROXY, or
                                // INTERNET_OPEN_TYPE_PRECONFIG (set only)
    lpszProxy: LPCTSTR;         // proxy server list
    lpszProxyBypass: LPCTSTR;   // proxy bypass list
  end;
  TInternetProxyInfo = INTERNET_PROXY_INFO;

//
// INTERNET_PER_CONN_OPTION_LIST - set per-connection options such as proxy
// and autoconfig info
//
// Set and queried using Internet[Set|Query]Option with
// INTERNET_OPTION_PER_CONNECTION_OPTION
//

  PInternetPerConnOptionA = ^TInternetPerConnOptionA;
  PInternetPerConnOptionW = ^TInternetPerConnOptionW;
  PInternetPerConnOption = PInternetPerConnOptionA;
  {$EXTERNALSYM INTERNET_PER_CONN_OPTIONA}
  INTERNET_PER_CONN_OPTIONA = record // not packed!
    dwOption: DWORD;            // option to be queried or set
    Value: record
      case Integer of
        1: (dwValue: DWORD);             // dword value for the option
        2: (pszValue: PAnsiChar);           // pointer to string value for the option
        3: (ftValue: TFileTime);         // file-time value for the option
    end;
  end;
  {$EXTERNALSYM INTERNET_PER_CONN_OPTIONW}
  INTERNET_PER_CONN_OPTIONW = record // not packed!
    dwOption: DWORD;            // option to be queried or set
    Value: record
      case Integer of
        1: (dwValue: DWORD);             // dword value for the option
        2: (pszValue: PWideChar);           // pointer to string value for the option
        3: (ftValue: TFileTime);         // file-time value for the option
    end;
  end;
  {$EXTERNALSYM INTERNET_PER_CONN_OPTION}
  INTERNET_PER_CONN_OPTION = INTERNET_PER_CONN_OPTIONA;
  TInternetPerConnOptionA = INTERNET_PER_CONN_OPTIONA;
  TInternetPerConnOptionW = INTERNET_PER_CONN_OPTIONW;
  TInternetPerConnOption = TInternetPerConnOptionA;

  PInternetPerConnOptionListA = ^TInternetPerConnOptionListA;
  PInternetPerConnOptionListW = ^TInternetPerConnOptionListW;
  PInternetPerConnOptionList = PInternetPerConnOptionListA;
  {$EXTERNALSYM INTERNET_PER_CONN_OPTION_LISTA}
  INTERNET_PER_CONN_OPTION_LISTA = record // not packed!
    dwSize: DWORD;              // size of the INTERNET_PER_CONN_OPTION_LIST struct
    pszConnection: PAnsiChar;      // connection name to set/query options
    dwOptionCount: DWORD;       // number of options to set/query
    dwOptionError: DWORD;       // on error, which option failed
    pOptions: PInternetPerConnOptionA; // array of options to set/query
  end;
  {$EXTERNALSYM INTERNET_PER_CONN_OPTION_LISTW}
  INTERNET_PER_CONN_OPTION_LISTW = record // not packed!
    dwSize: DWORD;              // size of the INTERNET_PER_CONN_OPTION_LIST struct
    pszConnection: PWideChar;      // connection name to set/query options
    dwOptionCount: DWORD;       // number of options to set/query
    dwOptionError: DWORD;       // on error, which option failed
    pOptions: PInternetPerConnOptionW; // array of options to set/query
  end;
  {$EXTERNALSYM INTERNET_PER_CONN_OPTION_LIST}
  INTERNET_PER_CONN_OPTION_LIST = INTERNET_PER_CONN_OPTION_LISTA;
  TInternetPerConnOptionListA = INTERNET_PER_CONN_OPTION_LISTA;
  TInternetPerConnOptionListW = INTERNET_PER_CONN_OPTION_LISTW;
  TInternetPerConnOptionList = TInternetPerConnOptionListA;


// Options used in INTERNET_PER_CONN_OPTION struct
const
  {$EXTERNALSYM INTERNET_PER_CONN_FLAGS}
  INTERNET_PER_CONN_FLAGS               = 1;
  {$EXTERNALSYM INTERNET_PER_CONN_PROXY_SERVER}
  INTERNET_PER_CONN_PROXY_SERVER        = 2;
  {$EXTERNALSYM INTERNET_PER_CONN_PROXY_BYPASS}
  INTERNET_PER_CONN_PROXY_BYPASS        = 3;
  {$EXTERNALSYM INTERNET_PER_CONN_AUTOCONFIG_URL}
  INTERNET_PER_CONN_AUTOCONFIG_URL      = 4;
  {$EXTERNALSYM INTERNET_PER_CONN_AUTODISCOVERY_FLAGS}
  INTERNET_PER_CONN_AUTODISCOVERY_FLAGS = 5;


// PER_CONN_FLAGS

  {$EXTERNALSYM PROXY_TYPE_DIRECT}
  PROXY_TYPE_DIRECT         = $00000001; // direct to net
  {$EXTERNALSYM PROXY_TYPE_PROXY}
  PROXY_TYPE_PROXY          = $00000002; // via named proxy
  {$EXTERNALSYM PROXY_TYPE_AUTO_PROXY_URL}
  PROXY_TYPE_AUTO_PROXY_URL = $00000004; // autoproxy URL
  {$EXTERNALSYM PROXY_TYPE_AUTO_DETECT}
  PROXY_TYPE_AUTO_DETECT    = $00000008; // use autoproxy detection


// PER_CONN_AUTODISCOVERY_FLAGS

  {$EXTERNALSYM AUTO_PROXY_FLAG_USER_SET}
  AUTO_PROXY_FLAG_USER_SET                = $00000001; // user changed this setting
  {$EXTERNALSYM AUTO_PROXY_FLAG_ALWAYS_DETECT}
  AUTO_PROXY_FLAG_ALWAYS_DETECT           = $00000002; // force detection even when its not needed
  {$EXTERNALSYM AUTO_PROXY_FLAG_DETECTION_RUN}
  AUTO_PROXY_FLAG_DETECTION_RUN           = $00000004; // detection has been run
  {$EXTERNALSYM AUTO_PROXY_FLAG_MIGRATED}
  AUTO_PROXY_FLAG_MIGRATED                = $00000008; // migration has just been done
  {$EXTERNALSYM AUTO_PROXY_FLAG_DONT_CACHE_PROXY_RESULT}
  AUTO_PROXY_FLAG_DONT_CACHE_PROXY_RESULT = $00000010; // don't cache result of host=proxy name
  {$EXTERNALSYM AUTO_PROXY_FLAG_CACHE_INIT_RUN}
  AUTO_PROXY_FLAG_CACHE_INIT_RUN          = $00000020; // don't initalize and run unless URL expired
  {$EXTERNALSYM AUTO_PROXY_FLAG_DETECTION_SUSPECT}
  AUTO_PROXY_FLAG_DETECTION_SUSPECT       = $00000040; // if we're on a LAN & Modem, with only one IP, bad?!?


// INTERNET_VERSION_INFO - version information returned via
// InternetQueryOption(..., INTERNET_OPTION_VERSION, ...)
type
  PInternetVersionInfo = ^TInternetVersionInfo;
  {$EXTERNALSYM INTERNET_VERSION_INFO}
  INTERNET_VERSION_INFO = record // not packed!
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
  end;
  TInternetVersionInfo = INTERNET_VERSION_INFO;


// HTTP_VERSION_INFO - query or set global HTTP version (1.0 or 1.1)

  PHttpVersionInfo = ^THttpVersionInfo;
  {$EXTERNALSYM HTTP_VERSION_INFO}
  HTTP_VERSION_INFO = record // not packed!
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
  end;
  THttpVersionInfo = HTTP_VERSION_INFO;


// INTERNET_CONNECTED_INFO - information used to set the global connected state

  PInternetConnectedInfo = ^TInternetConnectedInfo;
  {$EXTERNALSYM INTERNET_CONNECTED_INFO}
  INTERNET_CONNECTED_INFO = record // not packed!
    dwConnectedState: DWORD; // new connected/disconnected state.
                             // See INTERNET_STATE_CONNECTED, etc.
    dwFlags: DWORD;          // flags controlling connected->disconnected (or disconnected->
                             // connected) transition. See below
  end;
  TInternetConnectedInfo = INTERNET_CONNECTED_INFO;


// flags for INTERNET_CONNECTED_INFO dwFlags

// ISO_FORCE_DISCONNECTED - if set when putting Wininet into disconnected mode,
// all outstanding requests will be aborted with a cancelled error

const
  {$EXTERNALSYM ISO_FORCE_DISCONNECTED}
  ISO_FORCE_DISCONNECTED  = $00000001;


// URL_COMPONENTS - the constituent parts of an URL. Used in InternetCrackUrl()
// and InternetCreateUrl()
//
// For InternetCrackUrl(), if a pointer field and its corresponding length field
// are both 0 then that component is not returned. If the pointer field is NULL
// but the length field is not zero, then both the pointer and length fields are
// returned if both pointer and corresponding length fields are non-zero then
// the pointer field points to a buffer where the component is copied. The
// component may be un-escaped, depending on dwFlags
//
// For InternetCreateUrl(), the pointer fields should be NULL if the component
// is not required. If the corresponding length field is zero then the pointer
// field is the address of a zero-terminated string. If the length field is not
// zero then it is the string length of the corresponding pointer field

type
  PURLComponentsA = ^TURLComponentsA;
  PURLComponentsW = ^TURLComponentsW;
  PURLComponents = PURLComponentsA;
  {$EXTERNALSYM URL_COMPONENTSA}
  URL_COMPONENTSA = record // not packed!
    dwStructSize: DWORD;        // size of this structure. Used in version check
    lpszScheme: PAnsiChar;         // pointer to scheme name
    dwSchemeLength: DWORD;      // length of scheme name
    nScheme: INTERNET_SCHEME;   // enumerated scheme type (if known)
    lpszHostName: PAnsiChar;       // pointer to host name
    dwHostNameLength: DWORD;    // length of host name
    nPort: INTERNET_PORT;       // converted port number
    lpszUserName: PAnsiChar;       // pointer to user name
    dwUserNameLength: DWORD;    // length of user name
    lpszPassword: PAnsiChar;       // pointer to password
    dwPasswordLength: DWORD;    // length of password
    lpszUrlPath: PAnsiChar;        // pointer to URL-path
    dwUrlPathLength: DWORD;     // length of URL-path
    lpszExtraInfo: PAnsiChar;      // pointer to extra information (e.g. ?foo or #foo)
    dwExtraInfoLength: DWORD;   // length of extra information
  end;
  {$EXTERNALSYM URL_COMPONENTSW}
  URL_COMPONENTSW = record // not packed!
    dwStructSize: DWORD;        // size of this structure. Used in version check
    lpszScheme: PWideChar;         // pointer to scheme name
    dwSchemeLength: DWORD;      // length of scheme name
    nScheme: INTERNET_SCHEME;   // enumerated scheme type (if known)
    lpszHostName: PWideChar;       // pointer to host name
    dwHostNameLength: DWORD;    // length of host name
    nPort: INTERNET_PORT;       // converted port number
    lpszUserName: PWideChar;       // pointer to user name
    dwUserNameLength: DWORD;    // length of user name
    lpszPassword: PWideChar;       // pointer to password
    dwPasswordLength: DWORD;    // length of password
    lpszUrlPath: PWideChar;        // pointer to URL-path
    dwUrlPathLength: DWORD;     // length of URL-path
    lpszExtraInfo: PWideChar;      // pointer to extra information (e.g. ?foo or #foo)
    dwExtraInfoLength: DWORD;   // length of extra information
  end;
  {$EXTERNALSYM URL_COMPONENTS}
  URL_COMPONENTS = URL_COMPONENTSA;
  TURLComponentsA = URL_COMPONENTSA;
  TURLComponentsW = URL_COMPONENTSW;
  TURLComponents = TURLComponentsA;


// INTERNET_CERTIFICATE_INFO lpBuffer - contains the certificate returned from
// the server

  PInternetCertificateInfo = ^TInternetCertificateInfo;
  {$EXTERNALSYM INTERNET_CERTIFICATE_INFO}
  INTERNET_CERTIFICATE_INFO = record // not packed!
    ftExpiry: TFileTime;           // ftExpiry - date the certificate expires.
    ftStart: TFileTime;            // ftStart - date the certificate becomes valid.
    lpszSubjectInfo: LPTSTR;       // lpszSubjectInfo - the name of organization, site, and server
    lpszIssuerInfo: LPTSTR;        // lpszIssuerInfo - the name of orgainzation, site, and server
    lpszProtocolName: LPTSTR;      // lpszProtocolName - the name of the protocol used to provide the secure
                                   //   connection.
    lpszSignatureAlgName: LPTSTR;  // lpszSignatureAlgName - the name of the algorithm used for signing
                                   //   the certificate.
    lpszEncryptionAlgName: LPTSTR; // lpszEncryptionAlgName - the name of the algorithm used for
                                   //   doing encryption over the secure channel (SSL/PCT) connection.
    dwKeySize: DWORD;              // dwKeySize - size of the key.
  end;
  TInternetCertificateInfo = INTERNET_CERTIFICATE_INFO;

// INTERNET_BUFFERS - combines headers and data. May be chained for e.g. file
// upload or scatter/gather operations. For chunked read/write, lpcszHeader
// contains the chunked-ext

type
  PInternetBuffersA = ^TInternetBuffersA;
  PInternetBuffersW = ^TInternetBuffersW;
  PInternetBuffers = PInternetBuffersA;
  {$EXTERNALSYM _INTERNET_BUFFERSA}
  _INTERNET_BUFFERSA = record
    dwStructSize: DWORD;                 // used for API versioning. Set to sizeof(INTERNET_BUFFERS)
    Next: PInternetBuffers;              // chain of buffers
    lpcszHeader: PAnsiChar;              // pointer to headers (may be NULL)
    dwHeadersLength: DWORD;              // length of headers if not NULL
    dwHeadersTotal: DWORD;               // size of headers if not enough buffer
    lpvBuffer: Pointer;                  // pointer to data buffer (may be NULL)
    dwBufferLength: DWORD;               // length of data buffer if not NULL
    dwBufferTotal: DWORD;                // total size of chunk, or content-length if not chunked
    dwOffsetLow: DWORD;                  // used for read-ranges (only used in HttpSendRequest2)
    dwOffsetHigh: DWORD;
  end;
  {$EXTERNALSYM _INTERNET_BUFFERSW}
  _INTERNET_BUFFERSW = record
    dwStructSize: DWORD;                 // used for API versioning. Set to sizeof(INTERNET_BUFFERS)
    Next: PInternetBuffers;              // chain of buffers
    lpcszHeader: PWideChar;              // pointer to headers (may be NULL)
    dwHeadersLength: DWORD;              // length of headers if not NULL
    dwHeadersTotal: DWORD;               // size of headers if not enough buffer
    lpvBuffer: Pointer;                  // pointer to data buffer (may be NULL)
    dwBufferLength: DWORD;               // length of data buffer if not NULL
    dwBufferTotal: DWORD;                // total size of chunk, or content-length if not chunked
    dwOffsetLow: DWORD;                  // used for read-ranges (only used in HttpSendRequest2)
    dwOffsetHigh: DWORD;
  end;
  {$EXTERNALSYM _INTERNET_BUFFERS}
  _INTERNET_BUFFERS = _INTERNET_BUFFERSA;
  {$EXTERNALSYM _INTERNET_BUFFERSA}
  INTERNET_BUFFERSA = _INTERNET_BUFFERSA;
  {$EXTERNALSYM _INTERNET_BUFFERSW}
  INTERNET_BUFFERSW = _INTERNET_BUFFERSW;
  {$EXTERNALSYM INTERNET_BUFFERS}
  INTERNET_BUFFERS = INTERNET_BUFFERSA;
  TInternetBuffersA = _INTERNET_BUFFERSA;
  TInternetBuffersW = _INTERNET_BUFFERSW;
  TInternetBuffers = TInternetBuffersA;


// prototypes

{$EXTERNALSYM InternetTimeFromSystemTimeA}
function InternetTimeFromSystemTimeA(const pst: TSystemTime;
  dwRFC: DWORD; lpszTime: PAnsiChar; cbTime: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetTimeFromSystemTimeW}
function InternetTimeFromSystemTimeW(const pst: TSystemTime;
  dwRFC: DWORD; lpszTime: PWideChar; cbTime: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetTimeFromSystemTime}
function InternetTimeFromSystemTime(const pst: TSystemTime;
  dwRFC: DWORD; lpszTime: PTSTR; cbTime: DWORD): BOOL; stdcall;

// constants for InternetTimeFromSystemTime

const
  {$EXTERNALSYM INTERNET_RFC1123_FORMAT}
  INTERNET_RFC1123_FORMAT   =  0;
  {$EXTERNALSYM INTERNET_RFC1123_BUFSIZE}
  INTERNET_RFC1123_BUFSIZE  = 30;

{$EXTERNALSYM InternetTimeToSystemTimeA}
function InternetTimeToSystemTimeA(lpszTime: PAnsiChar;
  out pst: TSystemTime; dwReserved: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetTimeToSystemTimeW}
function InternetTimeToSystemTimeW(lpszTime: PWideChar;
  out pst: TSystemTime; dwReserved: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetTimeToSystemTime}
function InternetTimeToSystemTime(lpszTime: PTSTR;
  out pst: TSystemTime; dwReserved: DWORD): BOOL; stdcall;

{$EXTERNALSYM InternetCrackUrlA}
function InternetCrackUrlA(lpszUrl: PAnsiChar; dwUrlLength, dwFlags: DWORD;
  out UrlComponents: TURLComponentsA): BOOL; stdcall;
{$EXTERNALSYM InternetCrackUrlW}
function InternetCrackUrlW(lpszUrl: PWideChar; dwUrlLength, dwFlags: DWORD;
  out UrlComponents: TURLComponentsW): BOOL; stdcall;
{$EXTERNALSYM InternetCrackUrl}
function InternetCrackUrl(lpszUrl: PTSTR; dwUrlLength, dwFlags: DWORD;
  out UrlComponents: TURLComponents): BOOL; stdcall;

{$EXTERNALSYM InternetCreateUrlA}
function InternetCreateUrlA(var lpUrlComponents: TURLComponentsA;
  dwFlags: DWORD; lpszUrl: PAnsiChar; var lpdwUrlLength: DWORD): BOOL;  stdcall;
{$EXTERNALSYM InternetCreateUrlW}
function InternetCreateUrlW(var lpUrlComponents: TURLComponentsW;
  dwFlags: DWORD; lpszUrl: PWideChar; var lpdwUrlLength: DWORD): BOOL;  stdcall;
{$EXTERNALSYM InternetCreateUrl}
function InternetCreateUrl(var lpUrlComponents: TURLComponents;
  dwFlags: DWORD; lpszUrl: PTSTR; var lpdwUrlLength: DWORD): BOOL;  stdcall;

{$EXTERNALSYM InternetCanonicalizeUrlA}
function InternetCanonicalizeUrlA(lpszUrl: PAnsiChar;
  lpszBuffer: PAnsiChar; var lpdwBufferLength: DWORD; dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetCanonicalizeUrlW}
function InternetCanonicalizeUrlW(lpszUrl: PWideChar;
  lpszBuffer: PWideChar; var lpdwBufferLength: DWORD; dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetCanonicalizeUrl}
function InternetCanonicalizeUrl(lpszUrl: PTSTR;
  lpszBuffer: PTSTR; var lpdwBufferLength: DWORD; dwFlags: DWORD): BOOL; stdcall;

{$EXTERNALSYM InternetCombineUrlA}
function InternetCombineUrlA(lpszBaseUrl, lpszRelativeUrl: PAnsiChar;
  lpszBuffer: PAnsiChar; dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetCombineUrlW}
function InternetCombineUrlW(lpszBaseUrl, lpszRelativeUrl: PWideChar;
  lpszBuffer: PWideChar; dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetCombineUrl}
function InternetCombineUrl(lpszBaseUrl, lpszRelativeUrl: PTSTR;
  lpszBuffer: PTSTR; dwFlags: DWORD): BOOL; stdcall;


const

// flags for InternetCrackUrl() and InternetCreateUrl()

  {$EXTERNALSYM ICU_ESCAPE}
  ICU_ESCAPE      = $80000000;  // (un)escape URL characters
  {$EXTERNALSYM ICU_USERNAME}
  ICU_USERNAME    = $40000000;  // use internal username & password

// flags for InternetCanonicalizeUrl() and InternetCombineUrl()

  {$EXTERNALSYM ICU_NO_ENCODE}
  ICU_NO_ENCODE   = $20000000;  // Don't convert unsafe characters to escape sequence
  {$EXTERNALSYM ICU_DECODE}
  ICU_DECODE      = $10000000;  // Convert %XX escape sequences to characters
  {$EXTERNALSYM ICU_NO_META}
  ICU_NO_META     = $08000000;  // Don't convert .. etc. meta path sequences
  {$EXTERNALSYM ICU_ENCODE_SPACES_ONLY}
  ICU_ENCODE_SPACES_ONLY = $04000000;   // Encode spaces only
  {$EXTERNALSYM ICU_BROWSER_MODE}
  ICU_BROWSER_MODE = $02000000; // Special encode/decode rules for browser
  ICU_ENCODE_PERCENT = $00001000;       // Encode any percent (ASCII 25)
                                        // signs encountered, default is to
                                        // not encode percent.

function InternetOpenA(lpszAgent: PAnsiChar; dwAccessType: DWORD;
  lpszProxy, lpszProxyBypass: PAnsiChar; dwFlags: DWORD): HInternet; stdcall;
function InternetOpenW(lpszAgent: PWideChar; dwAccessType: DWORD;
  lpszProxy, lpszProxyBypass: PWideChar; dwFlags: DWORD): HInternet; stdcall;
function InternetOpen(lpszAgent: PTSTR; dwAccessType: DWORD;
  lpszProxy, lpszProxyBypass: PTSTR; dwFlags: DWORD): HInternet; stdcall;

const

// access types for InternetOpen()

  {$EXTERNALSYM INTERNET_OPEN_TYPE_PRECONFIG}
  INTERNET_OPEN_TYPE_PRECONFIG                    = 0;   // use registry configuration
  {$EXTERNALSYM INTERNET_OPEN_TYPE_DIRECT}
  INTERNET_OPEN_TYPE_DIRECT                       = 1;   // direct to net
  {$EXTERNALSYM INTERNET_OPEN_TYPE_PROXY}
  INTERNET_OPEN_TYPE_PROXY                        = 3;   // via named proxy
  {$EXTERNALSYM INTERNET_OPEN_TYPE_PRECONFIG_WITH_NO_AUTOPROXY}
  INTERNET_OPEN_TYPE_PRECONFIG_WITH_NO_AUTOPROXY  = 4;   // prevent using java/script/INS

// old names for access types

  {$EXTERNALSYM PRE_CONFIG_INTERNET_ACCESS}
  PRE_CONFIG_INTERNET_ACCESS  = INTERNET_OPEN_TYPE_PRECONFIG;
  {$EXTERNALSYM LOCAL_INTERNET_ACCESS}
  LOCAL_INTERNET_ACCESS       = INTERNET_OPEN_TYPE_DIRECT;
  {$EXTERNALSYM CERN_PROXY_INTERNET_ACCESS}
  CERN_PROXY_INTERNET_ACCESS  = INTERNET_OPEN_TYPE_PROXY;

{$EXTERNALSYM InternetCloseHandle}
function InternetCloseHandle(hInternet: HINTERNET): BOOL; stdcall;

{$EXTERNALSYM InternetConnectA}
function InternetConnectA(hInternet: HINTERNET;
  lpszServerName: PAnsiChar; nServerPort: TInternetPort;
  lpszUserName, lpszPassword: PAnsiChar;
  dwService, dwFlags, dwContext: DWORD): HINTERNET; stdcall;
{$EXTERNALSYM InternetConnectW}
function InternetConnectW(hInternet: HINTERNET;
  lpszServerName: PWideChar; nServerPort: TInternetPort;
  lpszUserName, lpszPassword: PWideChar;
  dwService, dwFlags, dwContext: DWORD): HINTERNET; stdcall;
{$EXTERNALSYM InternetConnect}
function InternetConnect(hInternet: HINTERNET;
  lpszServerName: PTSTR; nServerPort: TInternetPort;
  lpszUserName, lpszPassword: PTSTR;
  dwService, dwFlags, dwContext: DWORD): HINTERNET; stdcall;


const

// service types for InternetConnect()

  {$EXTERNALSYM INTERNET_SERVICE_URL}
  INTERNET_SERVICE_URL    = 0;
  {$EXTERNALSYM INTERNET_SERVICE_FTP}
  INTERNET_SERVICE_FTP    = 1;
  {$EXTERNALSYM INTERNET_SERVICE_GOPHER}
  INTERNET_SERVICE_GOPHER = 2;
  {$EXTERNALSYM INTERNET_SERVICE_HTTP}
  INTERNET_SERVICE_HTTP   = 3;

// InternetConnectUrl() - a macro which allows you to specify an URL instead of
// the component parts to InternetConnect(). If any API which uses the returned
// connect handle specifies a NULL path then the URL-path part of the URL
// specified in InternetConnectUrl() will be used

{$EXTERNALSYM InternetConnectUrlA}
function InternetConnectUrlA(hInternet: HINTERNET; lpszUrl: PAnsiChar;
  dwFlags, dwContext: DWORD): HINTERNET;
{$EXTERNALSYM InternetConnectUrlW}
function InternetConnectUrlW(hInternet: HINTERNET; lpszUrl: PWideChar;
  dwFlags, dwContext: DWORD): HINTERNET;
{$EXTERNALSYM InternetConnectUrl}
function InternetConnectUrl(hInternet: HINTERNET; lpszUrl: PTSTR;
  dwFlags, dwContext: DWORD): HINTERNET;

{$EXTERNALSYM InternetOpenUrlA}
function InternetOpenUrlA(hInternet: HINTERNET; lpszUrl, lpszHeaders: PAnsiChar;
  dwHeadersLength, dwFlags, dwContext: DWORD): HINTERNET; stdcall;
{$EXTERNALSYM InternetOpenUrlW}
function InternetOpenUrlW(hInternet: HINTERNET; lpszUrl, lpszHeaders: PWideChar;
  dwHeadersLength, dwFlags, dwContext: DWORD): HINTERNET; stdcall;
{$EXTERNALSYM InternetOpenUrl}
function InternetOpenUrl(hInternet: HINTERNET; lpszUrl, lpszHeaders: PTSTR;
  dwHeadersLength, dwFlags, dwContext: DWORD): HINTERNET; stdcall;

{$EXTERNALSYM InternetReadFile}
function InternetReadFile(hFile: HINTERNET; lpBuffer: Pointer;
  dwNumberOfBytesToRead: DWORD; var lpdwNumberOfBytesRead: DWORD): BOOL; stdcall;

{$EXTERNALSYM InternetReadFileExA}
function InternetReadFileExA(hFile: HINTERNET;
  var lpBuffersOut: TInternetBuffersA;
  dwFlags, dwContext: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetReadFileExW}
function InternetReadFileExW(hFile: HINTERNET;
  var lpBuffersOut: TInternetBuffersW;
  dwFlags, dwContext: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetReadFileEx}
function InternetReadFileEx(hFile: HINTERNET;
  var lpBuffersOut: TInternetBuffers;
  dwFlags, dwContext: DWORD): BOOL; stdcall;

const

// flags for InternetReadFileEx()

  {$EXTERNALSYM IRF_ASYNC}
  IRF_ASYNC       = WININET_API_FLAG_ASYNC;
  {$EXTERNALSYM IRF_SYNC}
  IRF_SYNC        = WININET_API_FLAG_SYNC;
  {$EXTERNALSYM IRF_USE_CONTEXT}
  IRF_USE_CONTEXT = WININET_API_FLAG_USE_CONTEXT;
  {$EXTERNALSYM IRF_NO_WAIT}
  IRF_NO_WAIT     = $00000008;

{$EXTERNALSYM InternetSetFilePointer}
function InternetSetFilePointer(hFile: HINTERNET;
  lDistanceToMove: Longint; pReserved: Pointer;
  dwMoveMethod, dwContext: DWORD): DWORD; stdcall;

{$EXTERNALSYM InternetWriteFile}
function InternetWriteFile(hFile: HINTERNET;
  lpBuffer: Pointer; dwNumberOfBytesToWrite: DWORD;
  var lpdwNumberOfBytesWritten: DWORD): BOOL; stdcall;

{$EXTERNALSYM InternetQueryDataAvailable}
function InternetQueryDataAvailable(hFile: HINTERNET;
  var lpdwNumberOfBytesAvailable: DWORD;
  dwFlags, dwContext: DWORD): BOOL; stdcall;

{$EXTERNALSYM InternetFindNextFileA}
function InternetFindNextFileA(hFind: HINTERNET; lpvFindData: Pointer): BOOL; stdcall;
{$EXTERNALSYM InternetFindNextFileW}
function InternetFindNextFileW(hFind: HINTERNET; lpvFindData: Pointer): BOOL; stdcall;
{$EXTERNALSYM InternetFindNextFile}
function InternetFindNextFile(hFind: HINTERNET; lpvFindData: Pointer): BOOL; stdcall;

{$EXTERNALSYM InternetQueryOptionA}
function InternetQueryOptionA(hInternet: HINTERNET; dwOption: DWORD;
 lpBuffer: Pointer; var lpdwBufferLength: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetQueryOptionW}
function InternetQueryOptionW(hInternet: HINTERNET; dwOption: DWORD;
 lpBuffer: Pointer; var lpdwBufferLength: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetQueryOption}
function InternetQueryOption(hInternet: HINTERNET; dwOption: DWORD;
 lpBuffer: Pointer; var lpdwBufferLength: DWORD): BOOL; stdcall;

{$EXTERNALSYM InternetSetOptionA}
function InternetSetOptionA(hInternet: HINTERNET; dwOption: DWORD;
  lpBuffer: Pointer; dwBufferLength: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetSetOptionW}
function InternetSetOptionW(hInternet: HINTERNET; dwOption: DWORD;
  lpBuffer: Pointer; dwBufferLength: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetSetOption}
function InternetSetOption(hInternet: HINTERNET; dwOption: DWORD;
  lpBuffer: Pointer; dwBufferLength: DWORD): BOOL; stdcall;

{$EXTERNALSYM InternetSetOptionExA}
function InternetSetOptionExA(hInternet: HINTERNET; dwOption: DWORD;
  lpBuffer: Pointer; dwBufferLength, dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetSetOptionExW}
function InternetSetOptionExW(hInternet: HINTERNET; dwOption: DWORD;
  lpBuffer: Pointer; dwBufferLength, dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetSetOptionEx}
function InternetSetOptionEx(hInternet: HINTERNET; dwOption: DWORD;
  lpBuffer: Pointer; dwBufferLength, dwFlags: DWORD): BOOL; stdcall;

{$EXTERNALSYM InternetLockRequestFile}
function InternetLockRequestFile(hInternet: HINTERNET;
  lphLockRequestInfo: PHandle): BOOL; stdcall;

{$EXTERNALSYM InternetUnlockRequestFile}
function InternetUnlockRequestFile(hLockRequestInfo: THandle): BOOL; stdcall;

const

// flags for InternetSetOptionEx()

  {$EXTERNALSYM ISO_GLOBAL}
  ISO_GLOBAL      = $00000001;  // modify option globally
  {$EXTERNALSYM ISO_REGISTRY}
  ISO_REGISTRY    = $00000002;  // write option to registry (where applicable)
  {$EXTERNALSYM ISO_VALID_FLAGS}
  ISO_VALID_FLAGS = ISO_GLOBAL or ISO_REGISTRY;

// options manifests for Internet{Query|Set}Option

  {$EXTERNALSYM INTERNET_OPTION_CALLBACK}
  INTERNET_OPTION_CALLBACK                = 1;
  {$EXTERNALSYM INTERNET_OPTION_CONNECT_TIMEOUT}
  INTERNET_OPTION_CONNECT_TIMEOUT         = 2;
  {$EXTERNALSYM INTERNET_OPTION_CONNECT_RETRIES}
  INTERNET_OPTION_CONNECT_RETRIES         = 3;
  {$EXTERNALSYM INTERNET_OPTION_CONNECT_BACKOFF}
  INTERNET_OPTION_CONNECT_BACKOFF         = 4;
  {$EXTERNALSYM INTERNET_OPTION_SEND_TIMEOUT}
  INTERNET_OPTION_SEND_TIMEOUT            = 5;
  {$EXTERNALSYM INTERNET_OPTION_CONTROL_SEND_TIMEOUT}
  INTERNET_OPTION_CONTROL_SEND_TIMEOUT    = INTERNET_OPTION_SEND_TIMEOUT;
  {$EXTERNALSYM INTERNET_OPTION_RECEIVE_TIMEOUT}
  INTERNET_OPTION_RECEIVE_TIMEOUT         = 6;
  {$EXTERNALSYM INTERNET_OPTION_CONTROL_RECEIVE_TIMEOUT}
  INTERNET_OPTION_CONTROL_RECEIVE_TIMEOUT = INTERNET_OPTION_RECEIVE_TIMEOUT;
  {$EXTERNALSYM INTERNET_OPTION_DATA_SEND_TIMEOUT}
  INTERNET_OPTION_DATA_SEND_TIMEOUT       = 7;
  {$EXTERNALSYM INTERNET_OPTION_DATA_RECEIVE_TIMEOUT}
  INTERNET_OPTION_DATA_RECEIVE_TIMEOUT    = 8;
  {$EXTERNALSYM INTERNET_OPTION_HANDLE_TYPE}
  INTERNET_OPTION_HANDLE_TYPE             = 9;
  {$EXTERNALSYM INTERNET_OPTION_LISTEN_TIMEOUT}
  INTERNET_OPTION_LISTEN_TIMEOUT          = 11;
  {$EXTERNALSYM INTERNET_OPTION_READ_BUFFER_SIZE}
  INTERNET_OPTION_READ_BUFFER_SIZE        = 12;
  {$EXTERNALSYM INTERNET_OPTION_WRITE_BUFFER_SIZE}
  INTERNET_OPTION_WRITE_BUFFER_SIZE       = 13;

  {$EXTERNALSYM INTERNET_OPTION_ASYNC_ID}
  INTERNET_OPTION_ASYNC_ID                = 15;
  {$EXTERNALSYM INTERNET_OPTION_ASYNC_PRIORITY}
  INTERNET_OPTION_ASYNC_PRIORITY          = 16;

  {$EXTERNALSYM INTERNET_OPTION_PARENT_HANDLE}
  INTERNET_OPTION_PARENT_HANDLE           = 21;
  {$EXTERNALSYM INTERNET_OPTION_KEEP_CONNECTION}
  INTERNET_OPTION_KEEP_CONNECTION         = 22;
  {$EXTERNALSYM INTERNET_OPTION_REQUEST_FLAGS}
  INTERNET_OPTION_REQUEST_FLAGS           = 23;
  {$EXTERNALSYM INTERNET_OPTION_EXTENDED_ERROR}
  INTERNET_OPTION_EXTENDED_ERROR          = 24;

  {$EXTERNALSYM INTERNET_OPTION_OFFLINE_MODE}
  INTERNET_OPTION_OFFLINE_MODE            = 26;
  {$EXTERNALSYM INTERNET_OPTION_CACHE_STREAM_HANDLE}
  INTERNET_OPTION_CACHE_STREAM_HANDLE     = 27;
  {$EXTERNALSYM INTERNET_OPTION_USERNAME}
  INTERNET_OPTION_USERNAME                = 28;
  {$EXTERNALSYM INTERNET_OPTION_PASSWORD}
  INTERNET_OPTION_PASSWORD                = 29;
  {$EXTERNALSYM INTERNET_OPTION_ASYNC}
  INTERNET_OPTION_ASYNC                   = 30;
  {$EXTERNALSYM INTERNET_OPTION_SECURITY_FLAGS}
  INTERNET_OPTION_SECURITY_FLAGS          = 31;
  {$EXTERNALSYM INTERNET_OPTION_SECURITY_CERTIFICATE_STRUCT}
  INTERNET_OPTION_SECURITY_CERTIFICATE_STRUCT = 32;
  {$EXTERNALSYM INTERNET_OPTION_DATAFILE_NAME}
  INTERNET_OPTION_DATAFILE_NAME           = 33;
  {$EXTERNALSYM INTERNET_OPTION_URL}
  INTERNET_OPTION_URL                     = 34;
  {$EXTERNALSYM INTERNET_OPTION_SECURITY_CERTIFICATE}
  INTERNET_OPTION_SECURITY_CERTIFICATE    = 35;
  {$EXTERNALSYM INTERNET_OPTION_SECURITY_KEY_BITNESS}
  INTERNET_OPTION_SECURITY_KEY_BITNESS    = 36;
  {$EXTERNALSYM INTERNET_OPTION_REFRESH}
  INTERNET_OPTION_REFRESH                 = 37;
  {$EXTERNALSYM INTERNET_OPTION_PROXY}
  INTERNET_OPTION_PROXY                   = 38;
  {$EXTERNALSYM INTERNET_OPTION_SETTINGS_CHANGED}
  INTERNET_OPTION_SETTINGS_CHANGED        = 39;
  {$EXTERNALSYM INTERNET_OPTION_VERSION}
  INTERNET_OPTION_VERSION                 = 40;
  {$EXTERNALSYM INTERNET_OPTION_USER_AGENT}
  INTERNET_OPTION_USER_AGENT              = 41;
  {$EXTERNALSYM INTERNET_OPTION_END_BROWSER_SESSION}
  INTERNET_OPTION_END_BROWSER_SESSION     = 42;
  {$EXTERNALSYM INTERNET_OPTION_PROXY_USERNAME}
  INTERNET_OPTION_PROXY_USERNAME          = 43;
  {$EXTERNALSYM INTERNET_OPTION_PROXY_PASSWORD}
  INTERNET_OPTION_PROXY_PASSWORD          = 44;
  {$EXTERNALSYM INTERNET_OPTION_CONTEXT_VALUE}
  INTERNET_OPTION_CONTEXT_VALUE           = 45;
  {$EXTERNALSYM INTERNET_OPTION_CONNECT_LIMIT}
  INTERNET_OPTION_CONNECT_LIMIT           = 46;
  {$EXTERNALSYM INTERNET_OPTION_SECURITY_SELECT_CLIENT_CERT}
  INTERNET_OPTION_SECURITY_SELECT_CLIENT_CERT = 47;
  {$EXTERNALSYM INTERNET_OPTION_POLICY}
  INTERNET_OPTION_POLICY                  = 48;
  {$EXTERNALSYM INTERNET_OPTION_DISCONNECTED_TIMEOUT}
  INTERNET_OPTION_DISCONNECTED_TIMEOUT    = 49;
  {$EXTERNALSYM INTERNET_OPTION_CONNECTED_STATE}
  INTERNET_OPTION_CONNECTED_STATE         = 50;
  {$EXTERNALSYM INTERNET_OPTION_IDLE_STATE}
  INTERNET_OPTION_IDLE_STATE              = 51;
  {$EXTERNALSYM INTERNET_OPTION_OFFLINE_SEMANTICS}
  INTERNET_OPTION_OFFLINE_SEMANTICS       = 52;
  {$EXTERNALSYM INTERNET_OPTION_SECONDARY_CACHE_KEY}
  INTERNET_OPTION_SECONDARY_CACHE_KEY     = 53;
  {$EXTERNALSYM INTERNET_OPTION_CALLBACK_FILTER}
  INTERNET_OPTION_CALLBACK_FILTER         = 54;
  {$EXTERNALSYM INTERNET_OPTION_CONNECT_TIME}
  INTERNET_OPTION_CONNECT_TIME            = 55;
  {$EXTERNALSYM INTERNET_OPTION_SEND_THROUGHPUT}
  INTERNET_OPTION_SEND_THROUGHPUT         = 56;
  {$EXTERNALSYM INTERNET_OPTION_RECEIVE_THROUGHPUT}
  INTERNET_OPTION_RECEIVE_THROUGHPUT      = 57;
  {$EXTERNALSYM INTERNET_OPTION_REQUEST_PRIORITY}
  INTERNET_OPTION_REQUEST_PRIORITY        = 58;
  {$EXTERNALSYM INTERNET_OPTION_HTTP_VERSION}
  INTERNET_OPTION_HTTP_VERSION            = 59;
  {$EXTERNALSYM INTERNET_OPTION_RESET_URLCACHE_SESSION}
  INTERNET_OPTION_RESET_URLCACHE_SESSION  = 60;
  {$EXTERNALSYM INTERNET_OPTION_ERROR_MASK}
  INTERNET_OPTION_ERROR_MASK              = 62;
  {$EXTERNALSYM INTERNET_OPTION_FROM_CACHE_TIMEOUT}
  INTERNET_OPTION_FROM_CACHE_TIMEOUT      = 63;
  {$EXTERNALSYM INTERNET_OPTION_BYPASS_EDITED_ENTRY}
  INTERNET_OPTION_BYPASS_EDITED_ENTRY     = 64;
  {$EXTERNALSYM INTERNET_OPTION_CODEPAGE}
  INTERNET_OPTION_CODEPAGE                = 68;
  {$EXTERNALSYM INTERNET_OPTION_CACHE_TIMESTAMPS}
  INTERNET_OPTION_CACHE_TIMESTAMPS        = 69;
  {$EXTERNALSYM INTERNET_OPTION_DISABLE_AUTODIAL}
  INTERNET_OPTION_DISABLE_AUTODIAL        = 70;
  {$EXTERNALSYM INTERNET_OPTION_MAX_CONNS_PER_SERVER}
  INTERNET_OPTION_MAX_CONNS_PER_SERVER    = 73;
  {$EXTERNALSYM INTERNET_OPTION_MAX_CONNS_PER_1_0_SERVER}
  INTERNET_OPTION_MAX_CONNS_PER_1_0_SERVER = 74;
  {$EXTERNALSYM INTERNET_OPTION_PER_CONNECTION_OPTION}
  INTERNET_OPTION_PER_CONNECTION_OPTION   = 75;
  {$EXTERNALSYM INTERNET_OPTION_DIGEST_AUTH_UNLOAD}
  INTERNET_OPTION_DIGEST_AUTH_UNLOAD      = 76;
  {$EXTERNALSYM INTERNET_OPTION_IGNORE_OFFLINE}
  INTERNET_OPTION_IGNORE_OFFLINE          = 77;

  {$EXTERNALSYM INTERNET_FIRST_OPTION}
  INTERNET_FIRST_OPTION                   = INTERNET_OPTION_CALLBACK;
  {$EXTERNALSYM INTERNET_LAST_OPTION}
  INTERNET_LAST_OPTION                    = INTERNET_OPTION_IGNORE_OFFLINE;


// values for INTERNET_OPTION_PRIORITY

  {$EXTERNALSYM INTERNET_PRIORITY_FOREGROUND}
  INTERNET_PRIORITY_FOREGROUND            = 1000;

// handle types

  {$EXTERNALSYM INTERNET_HANDLE_TYPE_INTERNET}
  INTERNET_HANDLE_TYPE_INTERNET           = 1;
  {$EXTERNALSYM INTERNET_HANDLE_TYPE_CONNECT_FTP}
  INTERNET_HANDLE_TYPE_CONNECT_FTP        = 2;
  {$EXTERNALSYM INTERNET_HANDLE_TYPE_CONNECT_GOPHER}
  INTERNET_HANDLE_TYPE_CONNECT_GOPHER     = 3;
  {$EXTERNALSYM INTERNET_HANDLE_TYPE_CONNECT_HTTP}
  INTERNET_HANDLE_TYPE_CONNECT_HTTP       = 4;
  {$EXTERNALSYM INTERNET_HANDLE_TYPE_FTP_FIND}
  INTERNET_HANDLE_TYPE_FTP_FIND           = 5;
  {$EXTERNALSYM INTERNET_HANDLE_TYPE_FTP_FIND_HTML}
  INTERNET_HANDLE_TYPE_FTP_FIND_HTML      = 6;
  {$EXTERNALSYM INTERNET_HANDLE_TYPE_FTP_FILE}
  INTERNET_HANDLE_TYPE_FTP_FILE           = 7;
  {$EXTERNALSYM INTERNET_HANDLE_TYPE_FTP_FILE_HTML}
  INTERNET_HANDLE_TYPE_FTP_FILE_HTML      = 8;
  {$EXTERNALSYM INTERNET_HANDLE_TYPE_GOPHER_FIND}
  INTERNET_HANDLE_TYPE_GOPHER_FIND        = 9;
  {$EXTERNALSYM INTERNET_HANDLE_TYPE_GOPHER_FIND_HTML}
  INTERNET_HANDLE_TYPE_GOPHER_FIND_HTML   = 10;
  {$EXTERNALSYM INTERNET_HANDLE_TYPE_GOPHER_FILE}
  INTERNET_HANDLE_TYPE_GOPHER_FILE        = 11;
  {$EXTERNALSYM INTERNET_HANDLE_TYPE_GOPHER_FILE_HTML}
  INTERNET_HANDLE_TYPE_GOPHER_FILE_HTML   = 12;
  {$EXTERNALSYM INTERNET_HANDLE_TYPE_HTTP_REQUEST}
  INTERNET_HANDLE_TYPE_HTTP_REQUEST       = 13;
  {$EXTERNALSYM INTERNET_HANDLE_TYPE_FILE_REQUEST}
  INTERNET_HANDLE_TYPE_FILE_REQUEST       = 14;

// values for INTERNET_OPTION_SECURITY_FLAGS

  // query only
  {$EXTERNALSYM SECURITY_FLAG_SECURE}
  SECURITY_FLAG_SECURE                    = $00000001; // can query only
  {$EXTERNALSYM SECURITY_FLAG_STRENGTH_WEAK}
  SECURITY_FLAG_STRENGTH_WEAK             = $10000000;
  {$EXTERNALSYM SECURITY_FLAG_STRENGTH_MEDIUM}
  SECURITY_FLAG_STRENGTH_MEDIUM           = $40000000;
  {$EXTERNALSYM SECURITY_FLAG_STRENGTH_STRONG}
  SECURITY_FLAG_STRENGTH_STRONG           = $20000000;
  {$EXTERNALSYM SECURITY_FLAG_UNKNOWNBIT}
  SECURITY_FLAG_UNKNOWNBIT                = $80000000;
  {$EXTERNALSYM SECURITY_FLAG_FORTEZZA}
  SECURITY_FLAG_FORTEZZA                  = $08000000;
  {$EXTERNALSYM SECURITY_FLAG_NORMALBITNESS}
  SECURITY_FLAG_NORMALBITNESS             = SECURITY_FLAG_STRENGTH_WEAK;

  // The following are unused
  {$EXTERNALSYM SECURITY_FLAG_SSL}
  SECURITY_FLAG_SSL                       = $00000002;
  {$EXTERNALSYM SECURITY_FLAG_SSL3}
  SECURITY_FLAG_SSL3                      = $00000004;
  {$EXTERNALSYM SECURITY_FLAG_PCT}
  SECURITY_FLAG_PCT                       = $00000008;
  {$EXTERNALSYM SECURITY_FLAG_PCT4}
  SECURITY_FLAG_PCT4                      = $00000010;
  {$EXTERNALSYM SECURITY_FLAG_IETFSSL4}
  SECURITY_FLAG_IETFSSL4                  = $00000020;

  // The following are for backwards compatability only.
  {$EXTERNALSYM SECURITY_FLAG_40BIT}
  SECURITY_FLAG_40BIT                     = SECURITY_FLAG_STRENGTH_WEAK;
  {$EXTERNALSYM SECURITY_FLAG_128BIT}
  SECURITY_FLAG_128BIT                    = SECURITY_FLAG_STRENGTH_STRONG;
  {$EXTERNALSYM SECURITY_FLAG_56BIT}
  SECURITY_FLAG_56BIT                     = SECURITY_FLAG_STRENGTH_MEDIUM;

  // setable flags
  {$EXTERNALSYM SECURITY_FLAG_IGNORE_REVOCATION}
  SECURITY_FLAG_IGNORE_REVOCATION         = $00000080;
  {$EXTERNALSYM SECURITY_FLAG_IGNORE_UNKNOWN_CA}
  SECURITY_FLAG_IGNORE_UNKNOWN_CA         = $00000100;
  {$EXTERNALSYM SECURITY_FLAG_IGNORE_WRONG_USAGE}
  SECURITY_FLAG_IGNORE_WRONG_USAGE        = $00000200;

  {$EXTERNALSYM SECURITY_FLAG_IGNORE_CERT_CN_INVALID}
  SECURITY_FLAG_IGNORE_CERT_CN_INVALID    = INTERNET_FLAG_IGNORE_CERT_CN_INVALID;
  {$EXTERNALSYM SECURITY_FLAG_IGNORE_CERT_DATE_INVALID}
  SECURITY_FLAG_IGNORE_CERT_DATE_INVALID  = INTERNET_FLAG_IGNORE_CERT_DATE_INVALID;


  {$EXTERNALSYM SECURITY_FLAG_IGNORE_REDIRECT_TO_HTTPS}
  SECURITY_FLAG_IGNORE_REDIRECT_TO_HTTPS  = INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS;
  {$EXTERNALSYM SECURITY_FLAG_IGNORE_REDIRECT_TO_HTTP}
  SECURITY_FLAG_IGNORE_REDIRECT_TO_HTTP   = INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTP;

  {$EXTERNALSYM SECURITY_SET_MASK}
  SECURITY_SET_MASK                       = SECURITY_FLAG_IGNORE_REVOCATION or
                                            SECURITY_FLAG_IGNORE_UNKNOWN_CA or
                                            SECURITY_FLAG_IGNORE_CERT_CN_INVALID or
                                            SECURITY_FLAG_IGNORE_CERT_DATE_INVALID or
                                            SECURITY_FLAG_IGNORE_WRONG_USAGE;
{$EXTERNALSYM InternetGetLastResponseInfoA}
function InternetGetLastResponseInfoA(var lpdwError: DWORD;
  lpszBuffer: PAnsiChar; var lpdwBufferLength: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetGetLastResponseInfoW}
function InternetGetLastResponseInfoW(var lpdwError: DWORD;
  lpszBuffer: PWideChar; var lpdwBufferLength: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetGetLastResponseInfo}
function InternetGetLastResponseInfo(var lpdwError: DWORD;
  lpszBuffer: PTSTR; var lpdwBufferLength: DWORD): BOOL; stdcall;

// callback function for InternetSetStatusCallback

type
  PInternetStatusCallback = ^TInternetStatusCallback;
  {$EXTERNALSYM INTERNET_STATUS_CALLBACK}
  INTERNET_STATUS_CALLBACK = procedure(hInternet: HINTERNET;
    dwContext, dwInternetStatus: DWORD; lpvStatusInformation: Pointer;
    dwStatusInformationLength: DWORD) stdcall;
  TInternetStatusCallback = INTERNET_STATUS_CALLBACK;

{$EXTERNALSYM InternetSetStatusCallbackA}
function InternetSetStatusCallbackA(hInternet: HINTERNET;
  lpfnInternetCallback: TInternetStatusCallback): TInternetStatusCallback; stdcall;
{$EXTERNALSYM InternetSetStatusCallbackW}
function InternetSetStatusCallbackW(hInternet: HINTERNET;
  lpfnInternetCallback: TInternetStatusCallback): TInternetStatusCallback; stdcall;
{$EXTERNALSYM InternetSetStatusCallback}
function InternetSetStatusCallback(hInternet: HINTERNET;
  lpfnInternetCallback: TInternetStatusCallback): TInternetStatusCallback; stdcall;

// status manifests for Internet status callback
const
  {$EXTERNALSYM INTERNET_STATUS_RESOLVING_NAME}
  INTERNET_STATUS_RESOLVING_NAME          = 10;
  {$EXTERNALSYM INTERNET_STATUS_NAME_RESOLVED}
  INTERNET_STATUS_NAME_RESOLVED           = 11;
  {$EXTERNALSYM INTERNET_STATUS_CONNECTING_TO_SERVER}
  INTERNET_STATUS_CONNECTING_TO_SERVER    = 20;
  {$EXTERNALSYM INTERNET_STATUS_CONNECTED_TO_SERVER}
  INTERNET_STATUS_CONNECTED_TO_SERVER     = 21;
  {$EXTERNALSYM INTERNET_STATUS_SENDING_REQUEST}
  INTERNET_STATUS_SENDING_REQUEST         = 30;
  {$EXTERNALSYM INTERNET_STATUS_REQUEST_SENT}
  INTERNET_STATUS_REQUEST_SENT            = 31;
  {$EXTERNALSYM INTERNET_STATUS_RECEIVING_RESPONSE}
  INTERNET_STATUS_RECEIVING_RESPONSE      = 40;
  {$EXTERNALSYM INTERNET_STATUS_RESPONSE_RECEIVED}
  INTERNET_STATUS_RESPONSE_RECEIVED       = 41;
  {$EXTERNALSYM INTERNET_STATUS_CTL_RESPONSE_RECEIVED}
  INTERNET_STATUS_CTL_RESPONSE_RECEIVED   = 42;
  {$EXTERNALSYM INTERNET_STATUS_PREFETCH}
  INTERNET_STATUS_PREFETCH                = 43;
  {$EXTERNALSYM INTERNET_STATUS_CLOSING_CONNECTION}
  INTERNET_STATUS_CLOSING_CONNECTION      = 50;
  {$EXTERNALSYM INTERNET_STATUS_CONNECTION_CLOSED}
  INTERNET_STATUS_CONNECTION_CLOSED       = 51;
  {$EXTERNALSYM INTERNET_STATUS_HANDLE_CREATED}
  INTERNET_STATUS_HANDLE_CREATED          = 60;
  {$EXTERNALSYM INTERNET_STATUS_HANDLE_CLOSING}
  INTERNET_STATUS_HANDLE_CLOSING          = 70;
  {$EXTERNALSYM INTERNET_STATUS_DETECTING_PROXY}
  INTERNET_STATUS_DETECTING_PROXY         = 80;
  {$EXTERNALSYM INTERNET_STATUS_REQUEST_COMPLETE}
  INTERNET_STATUS_REQUEST_COMPLETE        = 100;
  {$EXTERNALSYM INTERNET_STATUS_REDIRECT}
  INTERNET_STATUS_REDIRECT                = 110;
  {$EXTERNALSYM INTERNET_STATUS_INTERMEDIATE_RESPONSE}
  INTERNET_STATUS_INTERMEDIATE_RESPONSE   = 120;
  {$EXTERNALSYM INTERNET_STATUS_USER_INPUT_REQUIRED}
  INTERNET_STATUS_USER_INPUT_REQUIRED     = 140;
  {$EXTERNALSYM INTERNET_STATUS_STATE_CHANGE}
  INTERNET_STATUS_STATE_CHANGE            = 200;

// the following can be indicated in a state change notification:

  {$EXTERNALSYM INTERNET_STATE_CONNECTED}
  INTERNET_STATE_CONNECTED                = $00000001;  // connected state (mutually exclusive with disconnected)
  {$EXTERNALSYM INTERNET_STATE_DISCONNECTED}
  INTERNET_STATE_DISCONNECTED             = $00000002;  // disconnected from network
  {$EXTERNALSYM INTERNET_STATE_DISCONNECTED_BY_USER}
  INTERNET_STATE_DISCONNECTED_BY_USER     = $00000010;  // disconnected by user request
  {$EXTERNALSYM INTERNET_STATE_IDLE}
  INTERNET_STATE_IDLE                     = $00000100;  // no network requests being made (by Wininet)
  {$EXTERNALSYM INTERNET_STATE_BUSY}
  INTERNET_STATE_BUSY                     = $00000200;  // network requests being made (by Wininet)


// if the following value is returned by InternetSetStatusCallback, then
// probably an invalid (non-code) address was supplied for the callback

  {$EXTERNALSYM INTERNET_INVALID_STATUS_CALLBACK}
  INTERNET_INVALID_STATUS_CALLBACK        = PInternetStatusCallback(-1);

//
// FTP
//

// prototypes

{$EXTERNALSYM FtpFindFirstFileA}
function FtpFindFirstFileA(hConnect: HINTERNET;
  lpszSearchFile: PAnsiChar; lpFindFileData: PWin32FindData;
  dwFlags, dwContext: DWORD): HINTERNET; stdcall;
{$EXTERNALSYM FtpFindFirstFileW}
function FtpFindFirstFileW(hConnect: HINTERNET;
  lpszSearchFile: PWideChar; lpFindFileData: PWin32FindData;
  dwFlags, dwContext: DWORD): HINTERNET; stdcall;
{$EXTERNALSYM FtpFindFirstFile}
function FtpFindFirstFile(hConnect: HINTERNET;
  lpszSearchFile: PTSTR; lpFindFileData: PWin32FindData;
  dwFlags, dwContext: DWORD): HINTERNET; stdcall;

{$EXTERNALSYM FtpGetFileA}
function FtpGetFileA(hConnect: HINTERNET;
  lpszRemoteFile, lpszNewFile: PAnsiChar; fFailIfExists: BOOL;
  dwFlagsAndAttributes, dwFlags, dwContext: DWORD): BOOL; stdcall;
{$EXTERNALSYM FtpGetFileW}
function FtpGetFileW(hConnect: HINTERNET;
  lpszRemoteFile, lpszNewFile: PWideChar; fFailIfExists: BOOL;
  dwFlagsAndAttributes, dwFlags, dwContext: DWORD): BOOL; stdcall;
{$EXTERNALSYM FtpGetFile}
function FtpGetFile(hConnect: HINTERNET;
  lpszRemoteFile, lpszNewFile: PTSTR; fFailIfExists: BOOL;
  dwFlagsAndAttributes, dwFlags, dwContext: DWORD): BOOL; stdcall;

{$EXTERNALSYM FtpPutFileA}
function FtpPutFileA(hConnect: HINTERNET; lpszLocalFile,
  lpszNewRemoteFile: PAnsiChar; dwFlags, dwContext: DWORD): BOOL; stdcall;
{$EXTERNALSYM FtpPutFileW}
function FtpPutFileW(hConnect: HINTERNET; lpszLocalFile,
  lpszNewRemoteFile: PWideChar; dwFlags, dwContext: DWORD): BOOL; stdcall;
{$EXTERNALSYM FtpPutFile}
function FtpPutFile(hConnect: HINTERNET; lpszLocalFile,
  lpszNewRemoteFile: PTSTR; dwFlags, dwContext: DWORD): BOOL; stdcall;

{$EXTERNALSYM FtpGetFileEx}
function FtpGetFileEx(hFtpSession: HINTERNET; lpszRemoteFile: LPCSTR;
  lpszNewFile: LPCWSTR; fFailIfExists: BOOL;
  dwFlagsAndAttributes, dwFlags: DWORD; dwContext: DWORD_PTR): BOOL; stdcall;

{$EXTERNALSYM FtpPutFileEx}
function FtpPutFileEx(hFtpSession: HINTERNET; lpszLocalFile: LPCWSTR;
  lpszNewRemoteFile: LPCSTR; dwFlags: DWORD;
  dwContext: DWORD_PTR): BOOL; stdcall;

{$EXTERNALSYM FtpDeleteFileA}
function FtpDeleteFileA(hConnect: HINTERNET; lpszFileName: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM FtpDeleteFileW}
function FtpDeleteFileW(hConnect: HINTERNET; lpszFileName: PWideChar): BOOL; stdcall;
{$EXTERNALSYM FtpDeleteFile}
function FtpDeleteFile(hConnect: HINTERNET; lpszFileName: PTSTR): BOOL; stdcall;

{$EXTERNALSYM FtpRenameFileA}
function FtpRenameFileA(hConnect: HINTERNET;
  lpszExisting, lpszNew: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM FtpRenameFileW}
function FtpRenameFileW(hConnect: HINTERNET;
  lpszExisting, lpszNew: PWideChar): BOOL; stdcall;
{$EXTERNALSYM FtpRenameFile}
function FtpRenameFile(hConnect: HINTERNET;
  lpszExisting, lpszNew: PTSTR): BOOL; stdcall;

{$EXTERNALSYM FtpOpenFileA}
function FtpOpenFileA(hConnect: HINTERNET; lpszFileName: PAnsiChar;
  dwAccess, dwFlags, dwContext: DWORD): HINTERNET; stdcall;
{$EXTERNALSYM FtpOpenFileW}
function FtpOpenFileW(hConnect: HINTERNET; lpszFileName: PWideChar;
  dwAccess, dwFlags, dwContext: DWORD): HINTERNET; stdcall;
{$EXTERNALSYM FtpOpenFile}
function FtpOpenFile(hConnect: HINTERNET; lpszFileName: PTSTR;
  dwAccess, dwFlags, dwContext: DWORD): HINTERNET; stdcall;

{$EXTERNALSYM FtpCreateDirectoryA}
function FtpCreateDirectoryA(hConnect: HINTERNET; lpszDirectory: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM FtpCreateDirectoryW}
function FtpCreateDirectoryW(hConnect: HINTERNET; lpszDirectory: PWideChar): BOOL; stdcall;
{$EXTERNALSYM FtpCreateDirectory}
function FtpCreateDirectory(hConnect: HINTERNET; lpszDirectory: PTSTR): BOOL; stdcall;

{$EXTERNALSYM FtpRemoveDirectoryA}
function FtpRemoveDirectoryA(hConnect: HINTERNET; lpszDirectory: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM FtpRemoveDirectoryW}
function FtpRemoveDirectoryW(hConnect: HINTERNET; lpszDirectory: PWideChar): BOOL; stdcall;
{$EXTERNALSYM FtpRemoveDirectory}
function FtpRemoveDirectory(hConnect: HINTERNET; lpszDirectory: PTSTR): BOOL; stdcall;

{$EXTERNALSYM FtpSetCurrentDirectoryA}
function FtpSetCurrentDirectoryA(hConnect: HINTERNET;
  lpszDirectory: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM FtpSetCurrentDirectoryW}
function FtpSetCurrentDirectoryW(hConnect: HINTERNET;
  lpszDirectory: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM FtpSetCurrentDirectory}
function FtpSetCurrentDirectory(hConnect: HINTERNET;
  lpszDirectory: PAnsiChar): BOOL; stdcall;

{$EXTERNALSYM FtpGetCurrentDirectoryA}
function FtpGetCurrentDirectoryA(hConnect: HINTERNET; lpszCurrentDirectory: PAnsiChar;
  var lpdwCurrentDirectory: DWORD): BOOL; stdcall;
{$EXTERNALSYM FtpGetCurrentDirectoryW}
function FtpGetCurrentDirectoryW(hConnect: HINTERNET; lpszCurrentDirectory: PWideChar;
  var lpdwCurrentDirectory: DWORD): BOOL; stdcall;
{$EXTERNALSYM FtpGetCurrentDirectory}
function FtpGetCurrentDirectory(hConnect: HINTERNET; lpszCurrentDirectory: PTSTR;
  var lpdwCurrentDirectory: DWORD): BOOL; stdcall;

{$EXTERNALSYM FtpCommandA}
function FtpCommandA(hConnect: HINTERNET; fExpectResponse: BOOL;
  dwFlags: DWORD; lpszCommand: PAnsiChar; dwContext: DWORD_PTR;
  out phFtpCommand: HINTERNET): BOOL; stdcall;
{$EXTERNALSYM FtpCommandW}
function FtpCommandW(hConnect: HINTERNET; fExpectResponse: BOOL;
  dwFlags: DWORD; lpszCommand: PWideChar; dwContext: DWORD_PTR;
  out phFtpCommand: HINTERNET): BOOL; stdcall;
{$EXTERNALSYM FtpCommand}
function FtpCommand(hConnect: HINTERNET; fExpectResponse: BOOL;
  dwFlags: DWORD; lpszCommand: PTSTR; dwContext: DWORD_PTR;
  out phFtpCommand: HINTERNET): BOOL; stdcall;

{$EXTERNALSYM FtpGetFileSize}
function FtpGetFileSize(hFile: HINTERNET;
  out lpdwFileSizeHigh: DWORD): DWORD; stdcall;


//
// Gopher
//

// manifests

// string field lengths (in characters, not bytes)
const
  {$EXTERNALSYM MAX_GOPHER_DISPLAY_TEXT}
  MAX_GOPHER_DISPLAY_TEXT     = 128;
  {$EXTERNALSYM MAX_GOPHER_SELECTOR_TEXT}
  MAX_GOPHER_SELECTOR_TEXT    = 256;
  {$EXTERNALSYM MAX_GOPHER_HOST_NAME}
  MAX_GOPHER_HOST_NAME        = INTERNET_MAX_HOST_NAME_LENGTH; 
  {$EXTERNALSYM MAX_GOPHER_LOCATOR_LENGTH}
  MAX_GOPHER_LOCATOR_LENGTH   = 1 + MAX_GOPHER_DISPLAY_TEXT +
                                1 + MAX_GOPHER_SELECTOR_TEXT +
                                1 + MAX_GOPHER_HOST_NAME +  
                                1 + INTERNET_MAX_PORT_NUMBER_LENGTH +
                                1 + 1 + 2;
 
// structures/types 
 
// GOPHER_FIND_DATA - returns the results of a GopherFindFirstFile()/
// InternetFindNextFile() request 

type 
  PGopherFindDataA = ^TGopherFindDataA;
  PGopherFindDataW = ^TGopherFindDataW;
  PGopherFindData = PGopherFindDataA;
  {$EXTERNALSYM GOPHER_FIND_DATAA}
  GOPHER_FIND_DATAA = record // not packed!
    DisplayString: packed array [0..MAX_GOPHER_DISPLAY_TEXT] of AnsiChar;
    GopherType: DWORD;   // GOPHER_TYPE_, if known 
    SizeLow: DWORD;
    SizeHigh: DWORD;
    LastModificationTime: TFileTime;
    Locator: packed array [0..MAX_GOPHER_LOCATOR_LENGTH] of AnsiChar;
  end;
  {$EXTERNALSYM GOPHER_FIND_DATAW}
  GOPHER_FIND_DATAW = record // not packed!
    DisplayString: packed array [0..MAX_GOPHER_DISPLAY_TEXT] of WideChar;
    GopherType: DWORD;   // GOPHER_TYPE_, if known 
    SizeLow: DWORD;
    SizeHigh: DWORD;
    LastModificationTime: TFileTime;
    Locator: packed array [0..MAX_GOPHER_LOCATOR_LENGTH] of WideChar;
  end;
  {$EXTERNALSYM GOPHER_FIND_DATA}
  GOPHER_FIND_DATA = GOPHER_FIND_DATAA;
  TGopherFindDataA = GOPHER_FIND_DATAA;
  TGopherFindDataW = GOPHER_FIND_DATAW;
  TGopherFindData = TGopherFindDataA;

// manifests for GopherType 
const
  {$EXTERNALSYM GOPHER_TYPE_TEXT_FILE}
  GOPHER_TYPE_TEXT_FILE       = $00000001;
  {$EXTERNALSYM GOPHER_TYPE_DIRECTORY}
  GOPHER_TYPE_DIRECTORY       = $00000002;
  {$EXTERNALSYM GOPHER_TYPE_CSO}
  GOPHER_TYPE_CSO             = $00000004;
  {$EXTERNALSYM GOPHER_TYPE_ERROR}
  GOPHER_TYPE_ERROR           = $00000008;
  {$EXTERNALSYM GOPHER_TYPE_MAC_BINHEX}
  GOPHER_TYPE_MAC_BINHEX      = $00000010;
  {$EXTERNALSYM GOPHER_TYPE_DOS_ARCHIVE}
  GOPHER_TYPE_DOS_ARCHIVE     = $00000020;
  {$EXTERNALSYM GOPHER_TYPE_UNIX_UUENCODED}
  GOPHER_TYPE_UNIX_UUENCODED  = $00000040;
  {$EXTERNALSYM GOPHER_TYPE_INDEX_SERVER}
  GOPHER_TYPE_INDEX_SERVER    = $00000080;
  {$EXTERNALSYM GOPHER_TYPE_TELNET}
  GOPHER_TYPE_TELNET          = $00000100;
  {$EXTERNALSYM GOPHER_TYPE_BINARY}
  GOPHER_TYPE_BINARY          = $00000200;
  {$EXTERNALSYM GOPHER_TYPE_REDUNDANT}
  GOPHER_TYPE_REDUNDANT       = $00000400;
  {$EXTERNALSYM GOPHER_TYPE_TN3270}
  GOPHER_TYPE_TN3270          = $00000800;
  {$EXTERNALSYM GOPHER_TYPE_GIF}
  GOPHER_TYPE_GIF             = $00001000;
  {$EXTERNALSYM GOPHER_TYPE_IMAGE}
  GOPHER_TYPE_IMAGE           = $00002000;
  {$EXTERNALSYM GOPHER_TYPE_BITMAP}
  GOPHER_TYPE_BITMAP          = $00004000;
  {$EXTERNALSYM GOPHER_TYPE_MOVIE}
  GOPHER_TYPE_MOVIE           = $00008000;
  {$EXTERNALSYM GOPHER_TYPE_SOUND}
  GOPHER_TYPE_SOUND           = $00010000;
  {$EXTERNALSYM GOPHER_TYPE_HTML}
  GOPHER_TYPE_HTML            = $00020000;
  {$EXTERNALSYM GOPHER_TYPE_PDF}
  GOPHER_TYPE_PDF             = $00040000;
  {$EXTERNALSYM GOPHER_TYPE_CALENDAR}
  GOPHER_TYPE_CALENDAR        = $00080000;
  {$EXTERNALSYM GOPHER_TYPE_INLINE}
  GOPHER_TYPE_INLINE          = $00100000;
  {$EXTERNALSYM GOPHER_TYPE_UNKNOWN}
  GOPHER_TYPE_UNKNOWN         = $20000000;
  {$EXTERNALSYM GOPHER_TYPE_ASK}
  GOPHER_TYPE_ASK             = $40000000;
  {$EXTERNALSYM GOPHER_TYPE_GOPHER_PLUS}
  GOPHER_TYPE_GOPHER_PLUS     = $80000000;
 
// gopher type macros 

{$EXTERNALSYM IS_GOPHER_FILE}
function IS_GOPHER_FILE(AType: DWORD): BOOL;
{$EXTERNALSYM IS_GOPHER_DIRECTORY}
function IS_GOPHER_DIRECTORY(AType: DWORD): BOOL;
{$EXTERNALSYM IS_GOPHER_PHONE_SERVER}
function IS_GOPHER_PHONE_SERVER(AType: DWORD): BOOL;
{$EXTERNALSYM IS_GOPHER_ERROR}
function IS_GOPHER_ERROR(AType: DWORD): BOOL;
{$EXTERNALSYM IS_GOPHER_INDEX_SERVER}
function IS_GOPHER_INDEX_SERVER(AType: DWORD): BOOL;
{$EXTERNALSYM IS_GOPHER_TELNET_SESSION}
function IS_GOPHER_TELNET_SESSION(AType: DWORD): BOOL;
{$EXTERNALSYM IS_GOPHER_BACKUP_SERVER}
function IS_GOPHER_BACKUP_SERVER(AType: DWORD): BOOL;
{$EXTERNALSYM IS_GOPHER_TN3270_SESSION}
function IS_GOPHER_TN3270_SESSION(AType: DWORD): BOOL;
{$EXTERNALSYM IS_GOPHER_ASK}
function IS_GOPHER_ASK(AType: DWORD): BOOL;
{$EXTERNALSYM IS_GOPHER_PLUS}
function IS_GOPHER_PLUS(AType: DWORD): BOOL;
{$EXTERNALSYM IS_GOPHER_TYPE_KNOWN}
function IS_GOPHER_TYPE_KNOWN(AType: DWORD): BOOL;

// GOPHER_TYPE_FILE_MASK - use this to determine if a locator identifies a
// (known) file type

const 
  GOPHER_TYPE_FILE_MASK = GOPHER_TYPE_TEXT_FILE or 
                          GOPHER_TYPE_MAC_BINHEX or
                          GOPHER_TYPE_DOS_ARCHIVE or
                          GOPHER_TYPE_UNIX_UUENCODED or
                          GOPHER_TYPE_BINARY or
                          GOPHER_TYPE_GIF or
                          GOPHER_TYPE_IMAGE or
                          GOPHER_TYPE_BITMAP or
                          GOPHER_TYPE_MOVIE or
                          GOPHER_TYPE_SOUND or
                          GOPHER_TYPE_HTML or
                          GOPHER_TYPE_PDF or
                          GOPHER_TYPE_CALENDAR or
                          GOPHER_TYPE_INLINE;


// structured gopher attributes (as defined in gopher+ protocol document)

type
  PGopherAdminAttributeType = ^TGopherAdminAttributeType;
  {$EXTERNALSYM GOPHER_ADMIN_ATTRIBUTE_TYPE}
  GOPHER_ADMIN_ATTRIBUTE_TYPE = record // not packed!
    Comment: LPCTSTR;
    EmailAddress: LPCTSTR;
  end;
  TGopherAdminAttributeType = GOPHER_ADMIN_ATTRIBUTE_TYPE;

  PGopherModDateAttributeType = ^TGopherModDateAttributeType;
  {$EXTERNALSYM GOPHER_MOD_DATE_ATTRIBUTE_TYPE}
  GOPHER_MOD_DATE_ATTRIBUTE_TYPE = record // not packed!
    DateAndTime: TFileTime;
  end;
  TGopherModDateAttributeType = GOPHER_MOD_DATE_ATTRIBUTE_TYPE;
 
  PGopherTtlAttributeType = ^TGopherTtlAttributeType;
  {$EXTERNALSYM GOPHER_TTL_ATTRIBUTE_TYPE}
  GOPHER_TTL_ATTRIBUTE_TYPE = record
    Ttl: DWORD;
  end;
  TGopherTtlAttributeType = GOPHER_TTL_ATTRIBUTE_TYPE;

  PGopherScoreAttributeType = ^TGopherScoreAttributeType; 
  {$EXTERNALSYM GOPHER_SCORE_ATTRIBUTE_TYPE}
  GOPHER_SCORE_ATTRIBUTE_TYPE = record
    Score: Integer;
  end;
  TGopherScoreAttributeType = GOPHER_SCORE_ATTRIBUTE_TYPE;
  
  PGopherScoreRangeAttributeType = ^TGopherScoreRangeAttributeType;
  {$EXTERNALSYM GOPHER_SCORE_RANGE_ATTRIBUTE_TYPE}
  GOPHER_SCORE_RANGE_ATTRIBUTE_TYPE = record 
    LowerBound: Integer;
    UpperBound: Integer;
  end;
  TGopherScoreRangeAttributeType = GOPHER_SCORE_RANGE_ATTRIBUTE_TYPE;

  PGopherSiteAttributeType = ^TGopherSiteAttributeType;
  {$EXTERNALSYM GOPHER_SITE_ATTRIBUTE_TYPE}
  GOPHER_SITE_ATTRIBUTE_TYPE = record
    Site: LPCTSTR;
  end;
  TGopherSiteAttributeType = GOPHER_SITE_ATTRIBUTE_TYPE;

  PGopherOrganizationAttributeType = ^TGopherOrganizationAttributeType;
  {$EXTERNALSYM GOPHER_ORGANIZATION_ATTRIBUTE_TYPE}
  GOPHER_ORGANIZATION_ATTRIBUTE_TYPE = record
    Organization: LPCTSTR;
  end;
  TGopherOrganizationAttributeType = GOPHER_ORGANIZATION_ATTRIBUTE_TYPE; 

  PGopherLocationAttributeType = ^TGopherLocationAttributeType;
  {$EXTERNALSYM GOPHER_LOCATION_ATTRIBUTE_TYPE}
  GOPHER_LOCATION_ATTRIBUTE_TYPE = record
    Location: LPCTSTR;
  end;
  TGopherLocationAttributeType = GOPHER_LOCATION_ATTRIBUTE_TYPE; 
 
  PGopherGeographicalLocationAttributeType = ^TGopherGeographicalLocationAttributeType;
  {$EXTERNALSYM GOPHER_GEOGRAPHICAL_LOCATION_ATTRIBUTE_TYPE}
  GOPHER_GEOGRAPHICAL_LOCATION_ATTRIBUTE_TYPE = record
    DegreesNorth: Integer;
    MinutesNorth: Integer;
    SecondsNorth: Integer;
    DegreesEast: Integer;
    MinutesEast: Integer;
    SecondsEast: Integer;
  end;
  TGopherGeographicalLocationAttributeType = GOPHER_GEOGRAPHICAL_LOCATION_ATTRIBUTE_TYPE;
 
  PGopherTimeZoneAttributeType = ^TGopherTimeZoneAttributeType;
  {$EXTERNALSYM GOPHER_TIMEZONE_ATTRIBUTE_TYPE}
  GOPHER_TIMEZONE_ATTRIBUTE_TYPE = record
    Zone: Integer;
  end;
  TGopherTimeZoneAttributeType = GOPHER_TIMEZONE_ATTRIBUTE_TYPE; 

  PGopherProviderAttributeType = ^TGopherProviderAttributeType;
  {$EXTERNALSYM GOPHER_PROVIDER_ATTRIBUTE_TYPE}
  GOPHER_PROVIDER_ATTRIBUTE_TYPE = record
    Provider: LPCTSTR;
  end;
  TGopherProviderAttributeType = GOPHER_PROVIDER_ATTRIBUTE_TYPE;
 
  PGopherVersionAttributeType = ^TGopherVersionAttributeType;
  {$EXTERNALSYM GOPHER_VERSION_ATTRIBUTE_TYPE}
  GOPHER_VERSION_ATTRIBUTE_TYPE = record
    Version: LPCTSTR;
  end;
  TGopherVersionAttributeType = GOPHER_VERSION_ATTRIBUTE_TYPE;

  PGopherAbstractAttributeType= ^TGopherAbstractAttributeType;
  {$EXTERNALSYM GOPHER_ABSTRACT_ATTRIBUTE_TYPE}
  GOPHER_ABSTRACT_ATTRIBUTE_TYPE = record
    ShortAbstract: LPCTSTR;
    AbstractFile: LPCTSTR;
  end;
  TGopherAbstractAttributeType = GOPHER_ABSTRACT_ATTRIBUTE_TYPE; 

  PGopherViewAttributeType = ^TGopherViewAttributeType;
  {$EXTERNALSYM GOPHER_VIEW_ATTRIBUTE_TYPE}
  GOPHER_VIEW_ATTRIBUTE_TYPE = record
    ContentType: LPCTSTR;
    Language: LPCTSTR;
    Size: DWORD;
  end;
  TGopherViewAttributeType = GOPHER_VIEW_ATTRIBUTE_TYPE; 

  PGopherVeronicaAttributeType = ^TGopherVeronicaAttributeType; 
  {$EXTERNALSYM GOPHER_VERONICA_ATTRIBUTE_TYPE}
  GOPHER_VERONICA_ATTRIBUTE_TYPE = record
    TreeWalk: BOOL;
  end;
  TGopherVeronicaAttributeType = GOPHER_VERONICA_ATTRIBUTE_TYPE; 

  PGopherAskAttributeType = ^TGopherAskAttributeType;
  {$EXTERNALSYM GOPHER_ASK_ATTRIBUTE_TYPE}
  GOPHER_ASK_ATTRIBUTE_TYPE = record
    QuestionType: LPCTSTR;
    QuestionText: LPCTSTR;
  end;
  TGopherAskAttributeType = GOPHER_ASK_ATTRIBUTE_TYPE; 
 
  // GOPHER_UNKNOWN_ATTRIBUTE_TYPE - this is returned if we retrieve an attribute 
  // that is not specified in the current gopher/gopher+ documentation. It is up
  // to the application to parse the information 

  PGopherUnknownAttributeType = ^TGopherUnknownAttributeType;
  GOPHER_UNKNOWN_ATTRIBUTE_TYPE = record
    Text: LPCTSTR;
  end;
  TGopherUnknownAttributeType = GOPHER_UNKNOWN_ATTRIBUTE_TYPE;

  // GOPHER_ATTRIBUTE_TYPE - returned in the user's buffer when an enumerated
  // GopherGetAttribute call is made

  PGopherAttributeType = ^TGopherAttributeType;
  {$EXTERNALSYM GOPHER_ATTRIBUTE_TYPE}
  GOPHER_ATTRIBUTE_TYPE = record
    CategoryId: DWORD;   // e.g. GOPHER_CATEGORY_ID_ADMIN
    AttributeId: DWORD;  // e.g. GOPHER_ATTRIBUTE_ID_ADMIN
    AttributeType: record
      case Integer of
         1: (Admin: TGopherAdminAttributeType);
         2: (ModDate: TGopherModDateAttributeType);
         3: (Ttl: TGopherTtlAttributeType);
         4: (Score: TGopherScoreAttributeType);
         5: (ScoreRange: TGopherScoreRangeAttributeType);
         6: (Site: TGopherSiteAttributeType);
         7: (Organization: TGopherOrganizationAttributeType);
         8: (Location: TGopherLocationAttributeType);
         9: (GeographicalLocation: TGopherGeographicalLocationAttributeType);
        10: (TimeZone: TGopherTimeZoneAttributeType);
        12: (Provider: TGopherProviderAttributeType);
        13: (Version: TGopherVersionAttributeType);
        14: (Abstract: TGopherAbstractAttributeType);
        15: (View: TGopherViewAttributeType);
        16: (Veronica: TGopherVeronicaAttributeType);
        17: (Ask: TGopherAskAttributeType);
        18: (Unknown: TGopherUnknownAttributeType);
    end;
  end;
  TGopherAttributeType = GOPHER_ATTRIBUTE_TYPE;

const
  {$EXTERNALSYM MAX_GOPHER_CATEGORY_NAME}
  MAX_GOPHER_CATEGORY_NAME    = 128;     // arbitrary
  {$EXTERNALSYM MAX_GOPHER_ATTRIBUTE_NAME}
  MAX_GOPHER_ATTRIBUTE_NAME   = 128;     //     "
  {$EXTERNALSYM MIN_GOPHER_ATTRIBUTE_LENGTH}
  MIN_GOPHER_ATTRIBUTE_LENGTH = 256;     //     "

// known gopher attribute categories. See below for ordinals

  {$EXTERNALSYM GOPHER_INFO_CATEGORY}
  GOPHER_INFO_CATEGORY        = '+INFO';
  {$EXTERNALSYM GOPHER_ADMIN_CATEGORY}
  GOPHER_ADMIN_CATEGORY       = '+ADMIN';
  {$EXTERNALSYM GOPHER_VIEWS_CATEGORY}
  GOPHER_VIEWS_CATEGORY       = '+VIEWS'; 
  {$EXTERNALSYM GOPHER_ABSTRACT_CATEGORY}
  GOPHER_ABSTRACT_CATEGORY    = '+ABSTRACT'; 
  {$EXTERNALSYM GOPHER_VERONICA_CATEGORY}
  GOPHER_VERONICA_CATEGORY    = '+VERONICA'; 
 
// known gopher attributes. These are the attribute names as defined in the 
// gopher+ protocol document 
 
  {$EXTERNALSYM GOPHER_ADMIN_ATTRIBUTE}
  GOPHER_ADMIN_ATTRIBUTE      = 'Admin'; 
  {$EXTERNALSYM GOPHER_MOD_DATE_ATTRIBUTE}
  GOPHER_MOD_DATE_ATTRIBUTE   = 'Mod-Date'; 
  {$EXTERNALSYM GOPHER_TTL_ATTRIBUTE}
  GOPHER_TTL_ATTRIBUTE        = 'TTL'; 
  {$EXTERNALSYM GOPHER_SCORE_ATTRIBUTE}
  GOPHER_SCORE_ATTRIBUTE      = 'Score';
  {$EXTERNALSYM GOPHER_RANGE_ATTRIBUTE}
  GOPHER_RANGE_ATTRIBUTE      = 'Score-range'; 
  {$EXTERNALSYM GOPHER_SITE_ATTRIBUTE}
  GOPHER_SITE_ATTRIBUTE       = 'Site';
  {$EXTERNALSYM GOPHER_ORG_ATTRIBUTE}
  GOPHER_ORG_ATTRIBUTE        = 'Org'; 
  {$EXTERNALSYM GOPHER_LOCATION_ATTRIBUTE}
  GOPHER_LOCATION_ATTRIBUTE   = 'Loc'; 
  {$EXTERNALSYM GOPHER_GEOG_ATTRIBUTE}
  GOPHER_GEOG_ATTRIBUTE       = 'Geog'; 
  {$EXTERNALSYM GOPHER_TIMEZONE_ATTRIBUTE}
  GOPHER_TIMEZONE_ATTRIBUTE   = 'TZ'; 
  {$EXTERNALSYM GOPHER_PROVIDER_ATTRIBUTE}
  GOPHER_PROVIDER_ATTRIBUTE   = 'Provider';
  {$EXTERNALSYM GOPHER_VERSION_ATTRIBUTE}
  GOPHER_VERSION_ATTRIBUTE    = 'Version'; 
  {$EXTERNALSYM GOPHER_ABSTRACT_ATTRIBUTE}
  GOPHER_ABSTRACT_ATTRIBUTE   = 'Abstract'; 
  {$EXTERNALSYM GOPHER_VIEW_ATTRIBUTE}
  GOPHER_VIEW_ATTRIBUTE       = 'View'; 
  {$EXTERNALSYM GOPHER_TREEWALK_ATTRIBUTE}
  GOPHER_TREEWALK_ATTRIBUTE   = 'treewalk'; 
 
// identifiers for attribute strings 
 
  {$EXTERNALSYM GOPHER_ATTRIBUTE_ID_BASE}
  GOPHER_ATTRIBUTE_ID_BASE        = $ABCCCC00;
  {$EXTERNALSYM GOPHER_CATEGORY_ID_ALL}
  GOPHER_CATEGORY_ID_ALL          = GOPHER_ATTRIBUTE_ID_BASE + 1; 
  {$EXTERNALSYM GOPHER_CATEGORY_ID_INFO}
  GOPHER_CATEGORY_ID_INFO         = GOPHER_ATTRIBUTE_ID_BASE + 2; 
  {$EXTERNALSYM GOPHER_CATEGORY_ID_ADMIN}
  GOPHER_CATEGORY_ID_ADMIN        = GOPHER_ATTRIBUTE_ID_BASE + 3; 
  {$EXTERNALSYM GOPHER_CATEGORY_ID_VIEWS}
  GOPHER_CATEGORY_ID_VIEWS        = GOPHER_ATTRIBUTE_ID_BASE + 4;
  {$EXTERNALSYM GOPHER_CATEGORY_ID_ABSTRACT}
  GOPHER_CATEGORY_ID_ABSTRACT     = GOPHER_ATTRIBUTE_ID_BASE + 5; 
  {$EXTERNALSYM GOPHER_CATEGORY_ID_VERONICA}
  GOPHER_CATEGORY_ID_VERONICA     = GOPHER_ATTRIBUTE_ID_BASE + 6; 
  {$EXTERNALSYM GOPHER_CATEGORY_ID_ASK}
  GOPHER_CATEGORY_ID_ASK          = GOPHER_ATTRIBUTE_ID_BASE + 7; 
  {$EXTERNALSYM GOPHER_CATEGORY_ID_UNKNOWN}
  GOPHER_CATEGORY_ID_UNKNOWN      = GOPHER_ATTRIBUTE_ID_BASE + 8; 
  {$EXTERNALSYM GOPHER_ATTRIBUTE_ID_ALL}
  GOPHER_ATTRIBUTE_ID_ALL         = GOPHER_ATTRIBUTE_ID_BASE + 9; 
  {$EXTERNALSYM GOPHER_ATTRIBUTE_ID_ADMIN}
  GOPHER_ATTRIBUTE_ID_ADMIN       = GOPHER_ATTRIBUTE_ID_BASE + 10; 
  {$EXTERNALSYM GOPHER_ATTRIBUTE_ID_MOD_DATE}
  GOPHER_ATTRIBUTE_ID_MOD_DATE    = GOPHER_ATTRIBUTE_ID_BASE + 11; 
  {$EXTERNALSYM GOPHER_ATTRIBUTE_ID_TTL}
  GOPHER_ATTRIBUTE_ID_TTL         = GOPHER_ATTRIBUTE_ID_BASE + 12; 
  {$EXTERNALSYM GOPHER_ATTRIBUTE_ID_SCORE}
  GOPHER_ATTRIBUTE_ID_SCORE       = GOPHER_ATTRIBUTE_ID_BASE + 13; 
  {$EXTERNALSYM GOPHER_ATTRIBUTE_ID_RANGE}
  GOPHER_ATTRIBUTE_ID_RANGE       = GOPHER_ATTRIBUTE_ID_BASE + 14; 
  {$EXTERNALSYM GOPHER_ATTRIBUTE_ID_SITE}
  GOPHER_ATTRIBUTE_ID_SITE        = GOPHER_ATTRIBUTE_ID_BASE + 15; 
  {$EXTERNALSYM GOPHER_ATTRIBUTE_ID_ORG}
  GOPHER_ATTRIBUTE_ID_ORG         = GOPHER_ATTRIBUTE_ID_BASE + 16; 
  {$EXTERNALSYM GOPHER_ATTRIBUTE_ID_LOCATION}
  GOPHER_ATTRIBUTE_ID_LOCATION    = GOPHER_ATTRIBUTE_ID_BASE + 17; 
  {$EXTERNALSYM GOPHER_ATTRIBUTE_ID_GEOG}
  GOPHER_ATTRIBUTE_ID_GEOG        = GOPHER_ATTRIBUTE_ID_BASE + 18;
  {$EXTERNALSYM GOPHER_ATTRIBUTE_ID_TIMEZONE}
  GOPHER_ATTRIBUTE_ID_TIMEZONE    = GOPHER_ATTRIBUTE_ID_BASE + 19; 
  {$EXTERNALSYM GOPHER_ATTRIBUTE_ID_PROVIDER}
  GOPHER_ATTRIBUTE_ID_PROVIDER    = GOPHER_ATTRIBUTE_ID_BASE + 20; 
  {$EXTERNALSYM GOPHER_ATTRIBUTE_ID_VERSION}
  GOPHER_ATTRIBUTE_ID_VERSION     = GOPHER_ATTRIBUTE_ID_BASE + 21; 
  {$EXTERNALSYM GOPHER_ATTRIBUTE_ID_ABSTRACT}
  GOPHER_ATTRIBUTE_ID_ABSTRACT    = GOPHER_ATTRIBUTE_ID_BASE + 22; 
  {$EXTERNALSYM GOPHER_ATTRIBUTE_ID_VIEW}
  GOPHER_ATTRIBUTE_ID_VIEW        = GOPHER_ATTRIBUTE_ID_BASE + 23; 
  {$EXTERNALSYM GOPHER_ATTRIBUTE_ID_TREEWALK}
  GOPHER_ATTRIBUTE_ID_TREEWALK    = GOPHER_ATTRIBUTE_ID_BASE + 24; 
  {$EXTERNALSYM GOPHER_ATTRIBUTE_ID_UNKNOWN}
  GOPHER_ATTRIBUTE_ID_UNKNOWN     = GOPHER_ATTRIBUTE_ID_BASE + 25;
 
// prototypes 
 
{$EXTERNALSYM GopherCreateLocatorA}
function GopherCreateLocatorA(lpszHost: PAnsiChar; nServerPort: TInternetPort;
  lpszDisplayString, lpszSelectorString: PAnsiChar; dwGopherType: DWORD;
  lpszLocator: PAnsiChar; lpdwBufferLength: DWORD): BOOL; stdcall;
{$EXTERNALSYM GopherCreateLocatorW}
function GopherCreateLocatorW(lpszHost: PWideChar; nServerPort: TInternetPort;
  lpszDisplayString, lpszSelectorString: PWideChar; dwGopherType: DWORD;
  lpszLocator: PWideChar; lpdwBufferLength: DWORD): BOOL; stdcall;
{$EXTERNALSYM GopherCreateLocator}
function GopherCreateLocator(lpszHost: PTSTR; nServerPort: TInternetPort;
  lpszDisplayString, lpszSelectorString: PTSTR; dwGopherType: DWORD;
  lpszLocator: PTSTR; lpdwBufferLength: DWORD): BOOL; stdcall;

{$EXTERNALSYM GopherGetLocatorTypeA}
function GopherGetLocatorTypeA(lpszLocator: PAnsiChar;
  var lpdwGopherType: DWORD): BOOL; stdcall;
{$EXTERNALSYM GopherGetLocatorTypeW}
function GopherGetLocatorTypeW(lpszLocator: PWideChar;
  var lpdwGopherType: DWORD): BOOL; stdcall;
{$EXTERNALSYM GopherGetLocatorType}
function GopherGetLocatorType(lpszLocator: PTSTR;
  var lpdwGopherType: DWORD): BOOL; stdcall;

{$EXTERNALSYM GopherFindFirstFileA}
function GopherFindFirstFileA(hConnect: HINTERNET; lpszLocator, lpszSearchString: PAnsiChar;
  lpFindDat: PGopherFindDataA; dwFlags, dwContext: DWORD): HINTERNET; stdcall;
{$EXTERNALSYM GopherFindFirstFileW}
function GopherFindFirstFileW(hConnect: HINTERNET; lpszLocator, lpszSearchString: PWideChar;
  lpFindDat: PGopherFindDataW; dwFlags, dwContext: DWORD): HINTERNET; stdcall;
{$EXTERNALSYM GopherFindFirstFile}
function GopherFindFirstFile(hConnect: HINTERNET; lpszLocator, lpszSearchString: PTSTR;
  lpFindDat: PGopherFindData; dwFlags, dwContext: DWORD): HINTERNET; stdcall;

{$EXTERNALSYM GopherOpenFileA}
function GopherOpenFileA(hConnect: HINTERNET; lpszLocator, lpszView: PAnsiChar;
  dwFlags, dwContext: DWORD): HINTERNET; stdcall;
{$EXTERNALSYM GopherOpenFileW}
function GopherOpenFileW(hConnect: HINTERNET; lpszLocator, lpszView: PWideChar;
  dwFlags, dwContext: DWORD): HINTERNET; stdcall;
{$EXTERNALSYM GopherOpenFile}
function GopherOpenFile(hConnect: HINTERNET; lpszLocator, lpszView: PTSTR;
  dwFlags, dwContext: DWORD): HINTERNET; stdcall;

type
  {$EXTERNALSYM GOPHER_ATTRIBUTE_ENUMERATOR}
  GOPHER_ATTRIBUTE_ENUMERATOR = function(lpAttributeInfo: PGopherAttributeType;
    dwError: DWORD): BOOL stdcall;
  TGopherAttributeEnumerator = GOPHER_ATTRIBUTE_ENUMERATOR;  
 
{$EXTERNALSYM GopherGetAttributeA}
function GopherGetAttributeA(hConnect: HINTERNET; lpszLocator,
  lpszAttributeName: PAnsiChar; lpBuffer: PByte; dwBufferLength: DWORD;
  var lpdwCharactersReturned: DWORD; lpfnEnumerator:
  TGopherAttributeEnumerator; dwContext: DWORD): BOOL; stdcall;
{$EXTERNALSYM GopherGetAttributeW}
function GopherGetAttributeW(hConnect: HINTERNET; lpszLocator,
  lpszAttributeName: PWideChar; lpBuffer: PByte; dwBufferLength: DWORD;
  var lpdwCharactersReturned: DWORD; lpfnEnumerator:
  TGopherAttributeEnumerator; dwContext: DWORD): BOOL; stdcall;
{$EXTERNALSYM GopherGetAttribute}
function GopherGetAttribute(hConnect: HINTERNET; lpszLocator,
  lpszAttributeName: PTSTR; lpBuffer: PByte; dwBufferLength: DWORD;
  var lpdwCharactersReturned: DWORD; lpfnEnumerator:
  TGopherAttributeEnumerator; dwContext: DWORD): BOOL; stdcall;

//
// HTTP 
//
 
// manifests 

const
 
// the default major/minor HTTP version numbers 
 
  {$EXTERNALSYM HTTP_MAJOR_VERSION}
  HTTP_MAJOR_VERSION      = 1;
  {$EXTERNALSYM HTTP_MINOR_VERSION}
  HTTP_MINOR_VERSION      = 0;

  {$EXTERNALSYM HTTP_VERSIONA}
  HTTP_VERSIONA            = 'HTTP/1.0';
  {$EXTERNALSYM HTTP_VERSIONW}
  HTTP_VERSIONW            = 'HTTP/1.0';
  {$EXTERNALSYM HTTP_VERSION}
  HTTP_VERSION = HTTP_VERSIONA;


// HttpQueryInfo info levels. Generally, there is one info level
// for each potential RFC822/HTTP/MIME header that an HTTP server
// may send as part of a request response.
//
// The HTTP_QUERY_RAW_HEADERS info level is provided for clients
// that choose to perform their own header parsing.

  {$EXTERNALSYM HTTP_QUERY_MIME_VERSION}
  HTTP_QUERY_MIME_VERSION                 = 0;
  {$EXTERNALSYM HTTP_QUERY_CONTENT_TYPE}
  HTTP_QUERY_CONTENT_TYPE                 = 1;
  {$EXTERNALSYM HTTP_QUERY_CONTENT_TRANSFER_ENCODING}
  HTTP_QUERY_CONTENT_TRANSFER_ENCODING    = 2;
  {$EXTERNALSYM HTTP_QUERY_CONTENT_ID}
  HTTP_QUERY_CONTENT_ID                   = 3;
  {$EXTERNALSYM HTTP_QUERY_CONTENT_DESCRIPTION}
  HTTP_QUERY_CONTENT_DESCRIPTION          = 4;
  {$EXTERNALSYM HTTP_QUERY_CONTENT_LENGTH}
  HTTP_QUERY_CONTENT_LENGTH               = 5;
  {$EXTERNALSYM HTTP_QUERY_CONTENT_LANGUAGE}
  HTTP_QUERY_CONTENT_LANGUAGE             = 6;
  {$EXTERNALSYM HTTP_QUERY_ALLOW}
  HTTP_QUERY_ALLOW                        = 7;
  {$EXTERNALSYM HTTP_QUERY_PUBLIC}
  HTTP_QUERY_PUBLIC                       = 8;
  {$EXTERNALSYM HTTP_QUERY_DATE}
  HTTP_QUERY_DATE                         = 9;
  {$EXTERNALSYM HTTP_QUERY_EXPIRES}
  HTTP_QUERY_EXPIRES                      = 10;
  {$EXTERNALSYM HTTP_QUERY_LAST_MODIFIED}
  HTTP_QUERY_LAST_MODIFIED                = 11;
  {$EXTERNALSYM HTTP_QUERY_MESSAGE_ID}
  HTTP_QUERY_MESSAGE_ID                   = 12;
  {$EXTERNALSYM HTTP_QUERY_URI}
  HTTP_QUERY_URI                          = 13;
  {$EXTERNALSYM HTTP_QUERY_DERIVED_FROM}
  HTTP_QUERY_DERIVED_FROM                 = 14;
  {$EXTERNALSYM HTTP_QUERY_COST}
  HTTP_QUERY_COST                         = 15;
  {$EXTERNALSYM HTTP_QUERY_LINK}
  HTTP_QUERY_LINK                         = 16;
  {$EXTERNALSYM HTTP_QUERY_PRAGMA}
  HTTP_QUERY_PRAGMA                       = 17;
  {$EXTERNALSYM HTTP_QUERY_VERSION}
  HTTP_QUERY_VERSION                      = 18;  // special: part of status line
  {$EXTERNALSYM HTTP_QUERY_STATUS_CODE}
  HTTP_QUERY_STATUS_CODE                  = 19;  // special: part of status line
  {$EXTERNALSYM HTTP_QUERY_STATUS_TEXT}
  HTTP_QUERY_STATUS_TEXT                  = 20;  // special: part of status line
  {$EXTERNALSYM HTTP_QUERY_RAW_HEADERS}
  HTTP_QUERY_RAW_HEADERS                  = 21;  // special: all headers as ASCIIZ
  {$EXTERNALSYM HTTP_QUERY_RAW_HEADERS_CRLF}
  HTTP_QUERY_RAW_HEADERS_CRLF             = 22;  // special: all headers
  {$EXTERNALSYM HTTP_QUERY_CONNECTION}
  HTTP_QUERY_CONNECTION                   = 23;
  {$EXTERNALSYM HTTP_QUERY_ACCEPT}
  HTTP_QUERY_ACCEPT                       = 24;
  {$EXTERNALSYM HTTP_QUERY_ACCEPT_CHARSET}
  HTTP_QUERY_ACCEPT_CHARSET               = 25;
  {$EXTERNALSYM HTTP_QUERY_ACCEPT_ENCODING}
  HTTP_QUERY_ACCEPT_ENCODING              = 26;
  {$EXTERNALSYM HTTP_QUERY_ACCEPT_LANGUAGE}
  HTTP_QUERY_ACCEPT_LANGUAGE              = 27;
  {$EXTERNALSYM HTTP_QUERY_AUTHORIZATION}
  HTTP_QUERY_AUTHORIZATION                = 28;
  {$EXTERNALSYM HTTP_QUERY_CONTENT_ENCODING}
  HTTP_QUERY_CONTENT_ENCODING             = 29;
  {$EXTERNALSYM HTTP_QUERY_FORWARDED}
  HTTP_QUERY_FORWARDED                    = 30;
  {$EXTERNALSYM HTTP_QUERY_FROM}
  HTTP_QUERY_FROM                         = 31;
  {$EXTERNALSYM HTTP_QUERY_IF_MODIFIED_SINCE}
  HTTP_QUERY_IF_MODIFIED_SINCE            = 32;
  {$EXTERNALSYM HTTP_QUERY_LOCATION}
  HTTP_QUERY_LOCATION                     = 33;
  {$EXTERNALSYM HTTP_QUERY_ORIG_URI}
  HTTP_QUERY_ORIG_URI                     = 34;
  {$EXTERNALSYM HTTP_QUERY_REFERER}
  HTTP_QUERY_REFERER                      = 35;
  {$EXTERNALSYM HTTP_QUERY_RETRY_AFTER}
  HTTP_QUERY_RETRY_AFTER                  = 36;
  {$EXTERNALSYM HTTP_QUERY_SERVER}
  HTTP_QUERY_SERVER                       = 37;
  {$EXTERNALSYM HTTP_QUERY_TITLE}
  HTTP_QUERY_TITLE                        = 38;
  {$EXTERNALSYM HTTP_QUERY_USER_AGENT}
  HTTP_QUERY_USER_AGENT                   = 39;
  {$EXTERNALSYM HTTP_QUERY_WWW_AUTHENTICATE}
  HTTP_QUERY_WWW_AUTHENTICATE             = 40;
  {$EXTERNALSYM HTTP_QUERY_PROXY_AUTHENTICATE}
  HTTP_QUERY_PROXY_AUTHENTICATE           = 41;
  {$EXTERNALSYM HTTP_QUERY_ACCEPT_RANGES}
  HTTP_QUERY_ACCEPT_RANGES                = 42;
  {$EXTERNALSYM HTTP_QUERY_SET_COOKIE}
  HTTP_QUERY_SET_COOKIE                   = 43;
  {$EXTERNALSYM HTTP_QUERY_COOKIE}
  HTTP_QUERY_COOKIE                       = 44;
  {$EXTERNALSYM HTTP_QUERY_REQUEST_METHOD}
  HTTP_QUERY_REQUEST_METHOD               = 45;  // special: GET/POST etc.
  {$EXTERNALSYM HTTP_QUERY_REFRESH}
  HTTP_QUERY_REFRESH                      = 46;
  {$EXTERNALSYM HTTP_QUERY_CONTENT_DISPOSITION}
  HTTP_QUERY_CONTENT_DISPOSITION          = 47;

// HTTP 1.1 defined headers

  {$EXTERNALSYM HTTP_QUERY_AGE}
  HTTP_QUERY_AGE                          = 48;
  {$EXTERNALSYM HTTP_QUERY_CACHE_CONTROL}
  HTTP_QUERY_CACHE_CONTROL                = 49;
  {$EXTERNALSYM HTTP_QUERY_CONTENT_BASE}
  HTTP_QUERY_CONTENT_BASE                 = 50;
  {$EXTERNALSYM HTTP_QUERY_CONTENT_LOCATION}
  HTTP_QUERY_CONTENT_LOCATION             = 51;
  {$EXTERNALSYM HTTP_QUERY_CONTENT_MD5}
  HTTP_QUERY_CONTENT_MD5                  = 52;
  {$EXTERNALSYM HTTP_QUERY_CONTENT_RANGE}
  HTTP_QUERY_CONTENT_RANGE                = 53;
  {$EXTERNALSYM HTTP_QUERY_ETAG}
  HTTP_QUERY_ETAG                         = 54;
  {$EXTERNALSYM HTTP_QUERY_HOST}
  HTTP_QUERY_HOST                         = 55;
  {$EXTERNALSYM HTTP_QUERY_IF_MATCH}
  HTTP_QUERY_IF_MATCH                     = 56;
  {$EXTERNALSYM HTTP_QUERY_IF_NONE_MATCH}
  HTTP_QUERY_IF_NONE_MATCH                = 57;
  {$EXTERNALSYM HTTP_QUERY_IF_RANGE}
  HTTP_QUERY_IF_RANGE                     = 58;
  {$EXTERNALSYM HTTP_QUERY_IF_UNMODIFIED_SINCE}
  HTTP_QUERY_IF_UNMODIFIED_SINCE          = 59;
  {$EXTERNALSYM HTTP_QUERY_MAX_FORWARDS}
  HTTP_QUERY_MAX_FORWARDS                 = 60;
  {$EXTERNALSYM HTTP_QUERY_PROXY_AUTHORIZATION}
  HTTP_QUERY_PROXY_AUTHORIZATION          = 61;
  {$EXTERNALSYM HTTP_QUERY_RANGE}
  HTTP_QUERY_RANGE                        = 62;
  {$EXTERNALSYM HTTP_QUERY_TRANSFER_ENCODING}
  HTTP_QUERY_TRANSFER_ENCODING            = 63;
  {$EXTERNALSYM HTTP_QUERY_UPGRADE}
  HTTP_QUERY_UPGRADE                      = 64;
  {$EXTERNALSYM HTTP_QUERY_VARY}
  HTTP_QUERY_VARY                         = 65;
  {$EXTERNALSYM HTTP_QUERY_VIA}
  HTTP_QUERY_VIA                          = 66;
  {$EXTERNALSYM HTTP_QUERY_WARNING}
  HTTP_QUERY_WARNING                      = 67;
  {$EXTERNALSYM HTTP_QUERY_EXPECT}
  HTTP_QUERY_EXPECT                       = 68;
  {$EXTERNALSYM HTTP_QUERY_PROXY_CONNECTION}
  HTTP_QUERY_PROXY_CONNECTION             = 69;
  {$EXTERNALSYM HTTP_QUERY_UNLESS_MODIFIED_SINCE}
  HTTP_QUERY_UNLESS_MODIFIED_SINCE        = 70;

  {$EXTERNALSYM HTTP_QUERY_ECHO_REQUEST}
  HTTP_QUERY_ECHO_REQUEST                 = 71;
  {$EXTERNALSYM HTTP_QUERY_ECHO_REPLY}
  HTTP_QUERY_ECHO_REPLY                   = 72;

// These are the set of headers that should be added back to a request when
// re-doing a request after a RETRY_WITH response.
  {$EXTERNALSYM HTTP_QUERY_ECHO_HEADERS}
  HTTP_QUERY_ECHO_HEADERS                 = 73;
  {$EXTERNALSYM HTTP_QUERY_ECHO_HEADERS_CRLF}
  HTTP_QUERY_ECHO_HEADERS_CRLF            = 74;

  {$EXTERNALSYM HTTP_QUERY_MAX}
  HTTP_QUERY_MAX                          = 74;


  // HTTP_QUERY_CUSTOM - if this special value is supplied as the dwInfoLevel
  // parameter of HttpQueryInfo() then the lpBuffer parameter contains the name
  // of the header we are to query

  {$EXTERNALSYM HTTP_QUERY_CUSTOM}
  HTTP_QUERY_CUSTOM                       = 65535;

  // HTTP_QUERY_FLAG_REQUEST_HEADERS - if this bit is set in the dwInfoLevel
  // parameter of HttpQueryInfo() then the request headers will be queried for the
  // request information

  {$EXTERNALSYM HTTP_QUERY_FLAG_REQUEST_HEADERS}
  HTTP_QUERY_FLAG_REQUEST_HEADERS         = $80000000;

  // HTTP_QUERY_FLAG_SYSTEMTIME - if this bit is set in the dwInfoLevel parameter
  // of HttpQueryInfo() AND the header being queried contains date information,
  // e.g. the 'Expires:' header then lpBuffer will contain a SYSTEMTIME structure
  // containing the date and time information converted from the header string

  {$EXTERNALSYM HTTP_QUERY_FLAG_SYSTEMTIME}
  HTTP_QUERY_FLAG_SYSTEMTIME              = $40000000;

  // HTTP_QUERY_FLAG_NUMBER - if this bit is set in the dwInfoLevel parameter of
  // HttpQueryInfo(), then the value of the header will be converted to a number
  // before being returned to the caller, if applicable

  {$EXTERNALSYM HTTP_QUERY_FLAG_NUMBER}
  HTTP_QUERY_FLAG_NUMBER                  = $20000000;

  // HTTP_QUERY_FLAG_COALESCE - combine the values from several headers of the
  // same name into the output buffer

  {$EXTERNALSYM HTTP_QUERY_FLAG_COALESCE}
  HTTP_QUERY_FLAG_COALESCE                = $10000000;


  {$EXTERNALSYM HTTP_QUERY_MODIFIER_FLAGS_MASK}
  HTTP_QUERY_MODIFIER_FLAGS_MASK          = HTTP_QUERY_FLAG_REQUEST_HEADERS or
                                            HTTP_QUERY_FLAG_SYSTEMTIME or
                                            HTTP_QUERY_FLAG_NUMBER or
                                            HTTP_QUERY_FLAG_COALESCE;

  {$EXTERNALSYM HTTP_QUERY_HEADER_MASK}
  HTTP_QUERY_HEADER_MASK                  = not HTTP_QUERY_MODIFIER_FLAGS_MASK;

  // HTTP Response Status Codes:

  {$EXTERNALSYM HTTP_STATUS_CONTINUE}
  HTTP_STATUS_CONTINUE            = 100; // OK to continue with request
  {$EXTERNALSYM HTTP_STATUS_SWITCH_PROTOCOLS}
  HTTP_STATUS_SWITCH_PROTOCOLS    = 101; // server has switched protocols in upgrade header

  {$EXTERNALSYM HTTP_STATUS_OK}
  HTTP_STATUS_OK                  = 200; // request completed
  {$EXTERNALSYM HTTP_STATUS_CREATED}
  HTTP_STATUS_CREATED             = 201; // object created, reason = new URI
  {$EXTERNALSYM HTTP_STATUS_ACCEPTED}
  HTTP_STATUS_ACCEPTED            = 202; // async completion (TBS)
  {$EXTERNALSYM HTTP_STATUS_PARTIAL}
  HTTP_STATUS_PARTIAL             = 203; // partial completion
  {$EXTERNALSYM HTTP_STATUS_NO_CONTENT}
  HTTP_STATUS_NO_CONTENT          = 204; // no info to return
  {$EXTERNALSYM HTTP_STATUS_RESET_CONTENT}
  HTTP_STATUS_RESET_CONTENT       = 205; // request completed, but clear form
  {$EXTERNALSYM HTTP_STATUS_PARTIAL_CONTENT}
  HTTP_STATUS_PARTIAL_CONTENT     = 206; // partial GET furfilled

  {$EXTERNALSYM HTTP_STATUS_AMBIGUOUS}
  HTTP_STATUS_AMBIGUOUS           = 300; // server couldn't decide what to return
  {$EXTERNALSYM HTTP_STATUS_MOVED}
  HTTP_STATUS_MOVED               = 301; // object permanently moved
  {$EXTERNALSYM HTTP_STATUS_REDIRECT}
  HTTP_STATUS_REDIRECT            = 302; // object temporarily moved
  {$EXTERNALSYM HTTP_STATUS_REDIRECT_METHOD}
  HTTP_STATUS_REDIRECT_METHOD     = 303; // redirection w/ new access method
  {$EXTERNALSYM HTTP_STATUS_NOT_MODIFIED}
  HTTP_STATUS_NOT_MODIFIED        = 304; // if-modified-since was not modified
  {$EXTERNALSYM HTTP_STATUS_USE_PROXY}
  HTTP_STATUS_USE_PROXY           = 305; // redirection to proxy, location header specifies proxy to use
  {$EXTERNALSYM HTTP_STATUS_REDIRECT_KEEP_VERB}
  HTTP_STATUS_REDIRECT_KEEP_VERB  = 307; // HTTP/1.1: keep same verb

  {$EXTERNALSYM HTTP_STATUS_BAD_REQUEST}
  HTTP_STATUS_BAD_REQUEST         = 400; // invalid syntax
  {$EXTERNALSYM HTTP_STATUS_DENIED}
  HTTP_STATUS_DENIED              = 401; // access denied
  {$EXTERNALSYM HTTP_STATUS_PAYMENT_REQ}
  HTTP_STATUS_PAYMENT_REQ         = 402; // payment required
  {$EXTERNALSYM HTTP_STATUS_FORBIDDEN}
  HTTP_STATUS_FORBIDDEN           = 403; // request forbidden
  {$EXTERNALSYM HTTP_STATUS_NOT_FOUND}
  HTTP_STATUS_NOT_FOUND           = 404; // object not found
  {$EXTERNALSYM HTTP_STATUS_BAD_METHOD}
  HTTP_STATUS_BAD_METHOD          = 405; // method is not allowed
  {$EXTERNALSYM HTTP_STATUS_NONE_ACCEPTABLE}
  HTTP_STATUS_NONE_ACCEPTABLE     = 406; // no response acceptable to client found
  {$EXTERNALSYM HTTP_STATUS_PROXY_AUTH_REQ}
  HTTP_STATUS_PROXY_AUTH_REQ      = 407; // proxy authentication required
  {$EXTERNALSYM HTTP_STATUS_REQUEST_TIMEOUT}
  HTTP_STATUS_REQUEST_TIMEOUT     = 408; // server timed out waiting for request
  {$EXTERNALSYM HTTP_STATUS_CONFLICT}
  HTTP_STATUS_CONFLICT            = 409; // user should resubmit with more info
  {$EXTERNALSYM HTTP_STATUS_GONE}
  HTTP_STATUS_GONE                = 410; // the resource is no longer available
  {$EXTERNALSYM HTTP_STATUS_LENGTH_REQUIRED}
  HTTP_STATUS_LENGTH_REQUIRED     = 411; // the server refused to accept request w/o a length
  {$EXTERNALSYM HTTP_STATUS_PRECOND_FAILED}
  HTTP_STATUS_PRECOND_FAILED      = 412; // precondition given in request failed
  {$EXTERNALSYM HTTP_STATUS_REQUEST_TOO_LARGE}
  HTTP_STATUS_REQUEST_TOO_LARGE   = 413; // request entity was too large
  {$EXTERNALSYM HTTP_STATUS_URI_TOO_LONG}
  HTTP_STATUS_URI_TOO_LONG        = 414; // request URI too long
  {$EXTERNALSYM HTTP_STATUS_UNSUPPORTED_MEDIA}
  HTTP_STATUS_UNSUPPORTED_MEDIA   = 415; // unsupported media type
  {$EXTERNALSYM HTTP_STATUS_RETRY_WITH}
  HTTP_STATUS_RETRY_WITH          = 449; // retry after doing the appropriate action.

  {$EXTERNALSYM HTTP_STATUS_SERVER_ERROR}
  HTTP_STATUS_SERVER_ERROR        = 500; // internal server error
  {$EXTERNALSYM HTTP_STATUS_NOT_SUPPORTED}
  HTTP_STATUS_NOT_SUPPORTED       = 501; // required not supported
  {$EXTERNALSYM HTTP_STATUS_BAD_GATEWAY}
  HTTP_STATUS_BAD_GATEWAY         = 502; // error response received from gateway
  {$EXTERNALSYM HTTP_STATUS_SERVICE_UNAVAIL}
  HTTP_STATUS_SERVICE_UNAVAIL     = 503; // temporarily overloaded
  {$EXTERNALSYM HTTP_STATUS_GATEWAY_TIMEOUT}
  HTTP_STATUS_GATEWAY_TIMEOUT     = 504; // timed out waiting for gateway
  {$EXTERNALSYM HTTP_STATUS_VERSION_NOT_SUP}
  HTTP_STATUS_VERSION_NOT_SUP     = 505; // HTTP version not supported

  {$EXTERNALSYM HTTP_STATUS_FIRST}
  HTTP_STATUS_FIRST               = HTTP_STATUS_CONTINUE;
  {$EXTERNALSYM HTTP_STATUS_LAST}
  HTTP_STATUS_LAST                = HTTP_STATUS_VERSION_NOT_SUP;

// prototypes

{$EXTERNALSYM HttpOpenRequestA}
function HttpOpenRequestA(hConnect: HINTERNET; lpszVerb, lpszObjectName,
  lpszVersion,lpszReferrer: PAnsiChar; lplpszAcceptTypes: PAnsiChar;
  dwFlags, dwContext: DWORD): HINTERNET; stdcall;
{$EXTERNALSYM HttpOpenRequestW}
function HttpOpenRequestW(hConnect: HINTERNET; lpszVerb, lpszObjectName,
  lpszVersion,lpszReferrer: PWideChar; lplpszAcceptTypes: PWideChar;
  dwFlags, dwContext: DWORD): HINTERNET; stdcall;
{$EXTERNALSYM HttpOpenRequest}
function HttpOpenRequest(hConnect: HINTERNET; lpszVerb, lpszObjectName,
  lpszVersion,lpszReferrer: PTSTR; lplpszAcceptTypes: PTSTR;
  dwFlags, dwContext: DWORD): HINTERNET; stdcall;

{$EXTERNALSYM HttpAddRequestHeadersA}
function HttpAddRequestHeadersA(hRequest: HINTERNET; lpszHeaders: PAnsiChar;
  dwHeadersLength, dwModifiers: DWORD): BOOL; stdcall;
{$EXTERNALSYM HttpAddRequestHeadersW}
function HttpAddRequestHeadersW(hRequest: HINTERNET; lpszHeaders: PWideChar;
  dwHeadersLength, dwModifiers: DWORD): BOOL; stdcall;
{$EXTERNALSYM HttpAddRequestHeaders}
function HttpAddRequestHeaders(hRequest: HINTERNET; lpszHeaders: PTSTR;
  dwHeadersLength, dwModifiers: DWORD): BOOL; stdcall;

// values for dwModifiers parameter of HttpAddRequestHeaders()

const
  {$EXTERNALSYM HTTP_ADDREQ_INDEX_MASK}
  HTTP_ADDREQ_INDEX_MASK      = $0000FFFF;
  {$EXTERNALSYM HTTP_ADDREQ_FLAGS_MASK}
  HTTP_ADDREQ_FLAGS_MASK      = $FFFF0000;

  // HTTP_ADDREQ_FLAG_ADD_IF_NEW - the header will only be added if it doesn't
  // already exist

  {$EXTERNALSYM HTTP_ADDREQ_FLAG_ADD_IF_NEW}
  HTTP_ADDREQ_FLAG_ADD_IF_NEW = $10000000;

  // HTTP_ADDREQ_FLAG_ADD - if HTTP_ADDREQ_FLAG_REPLACE is set but the header is
  // not found then if this flag is set, the header is added anyway, so long as
  // there is a valid header-value

  {$EXTERNALSYM HTTP_ADDREQ_FLAG_ADD}
  HTTP_ADDREQ_FLAG_ADD        = $20000000;

  // HTTP_ADDREQ_FLAG_COALESCE - coalesce headers with same name. e.g.
  // 'Accept: text/*' and 'Accept: audio/*' with this flag results in a single
  // header: 'Accept: text/*, audio/*'

  {$EXTERNALSYM HTTP_ADDREQ_FLAG_COALESCE_WITH_COMMA}
  HTTP_ADDREQ_FLAG_COALESCE_WITH_COMMA       = $40000000;
  {$EXTERNALSYM HTTP_ADDREQ_FLAG_COALESCE_WITH_SEMICOLON}
  HTTP_ADDREQ_FLAG_COALESCE_WITH_SEMICOLON   = $01000000;
  {$EXTERNALSYM HTTP_ADDREQ_FLAG_COALESCE}
  HTTP_ADDREQ_FLAG_COALESCE                  = HTTP_ADDREQ_FLAG_COALESCE_WITH_COMMA;

  // HTTP_ADDREQ_FLAG_REPLACE - replaces the specified header. Only one header can
  // be supplied in the buffer. If the header to be replaced is not the first
  // in a list of headers with the same name, then the relative index should be
  // supplied in the low 8 bits of the dwModifiers parameter. If the header-value
  // part is missing, then the header is removed

  {$EXTERNALSYM HTTP_ADDREQ_FLAG_REPLACE}
  HTTP_ADDREQ_FLAG_REPLACE    = $80000000;

{$EXTERNALSYM HttpSendRequestA}
function HttpSendRequestA(hRequest: HINTERNET; lpszHeaders: PAnsiChar;
  dwHeadersLength: DWORD; lpOptional: Pointer; dwOptionalLength: DWORD): BOOL; stdcall;
{$EXTERNALSYM HttpSendRequestW}
function HttpSendRequestW(hRequest: HINTERNET; lpszHeaders: PWideChar;
  dwHeadersLength: DWORD; lpOptional: Pointer; dwOptionalLength: DWORD): BOOL; stdcall;
{$EXTERNALSYM HttpSendRequest}
function HttpSendRequest(hRequest: HINTERNET; lpszHeaders: PTSTR;
  dwHeadersLength: DWORD; lpOptional: Pointer; dwOptionalLength: DWORD): BOOL; stdcall;

{$EXTERNALSYM HttpSendRequestExA}
function HttpSendRequestExA(hRequest: HINTERNET; lpBuffersIn,
  lpBuffersOut: PInternetBuffersA; dwFlags, dwContext: DWORD): BOOL; stdcall;
{$EXTERNALSYM HttpSendRequestExW}
function HttpSendRequestExW(hRequest: HINTERNET; lpBuffersIn,
  lpBuffersOut: PInternetBuffersW; dwFlags, dwContext: DWORD): BOOL; stdcall;
{$EXTERNALSYM HttpSendRequestEx}
function HttpSendRequestEx(hRequest: HINTERNET; lpBuffersIn,
  lpBuffersOut: PInternetBuffers; dwFlags, dwContext: DWORD): BOOL; stdcall;

// flags for HttpSendRequestEx(), HttpEndRequest()
const
  {$EXTERNALSYM HSR_ASYNC}
  HSR_ASYNC       = WININET_API_FLAG_ASYNC;       // force async
  {$EXTERNALSYM HSR_SYNC}
  HSR_SYNC        = WININET_API_FLAG_SYNC;        // force sync
  {$EXTERNALSYM HSR_USE_CONTEXT}
  HSR_USE_CONTEXT = WININET_API_FLAG_USE_CONTEXT; // use dwContext value
  {$EXTERNALSYM HSR_INITIATE}
  HSR_INITIATE    = $00000008;                    // iterative operation (completed by HttpEndRequest)
  {$EXTERNALSYM HSR_DOWNLOAD}
  HSR_DOWNLOAD    = $00000010;                    // download to file
  {$EXTERNALSYM HSR_CHUNKED}
  HSR_CHUNKED     = $00000020;                    // operation is send of chunked data

{$EXTERNALSYM HttpEndRequestA}
function HttpEndRequestA(hRequest: HINTERNET; lpBuffersOut: PInternetBuffersA;
  dwFlags, dwContext: DWORD): BOOL; stdcall;
{$EXTERNALSYM HttpEndRequestW}
function HttpEndRequestW(hRequest: HINTERNET; lpBuffersOut: PInternetBuffersW;
  dwFlags, dwContext: DWORD): BOOL; stdcall;
{$EXTERNALSYM HttpEndRequest}
function HttpEndRequest(hRequest: HINTERNET; lpBuffersOut: PInternetBuffers;
  dwFlags, dwContext: DWORD): BOOL; stdcall;

{$EXTERNALSYM HttpQueryInfoA}
function HttpQueryInfoA(hRequest: HINTERNET; dwInfoLevel: DWORD;
  lpBuffer: Pointer; var lpdwBufferLength, lpdwIndex: DWORD): BOOL; stdcall;
{$EXTERNALSYM HttpQueryInfoW}
function HttpQueryInfoW(hRequest: HINTERNET; dwInfoLevel: DWORD;
  lpBuffer: Pointer; var lpdwBufferLength, lpdwIndex: DWORD): BOOL; stdcall;
{$EXTERNALSYM HttpQueryInfo}
function HttpQueryInfo(hRequest: HINTERNET; dwInfoLevel: DWORD;
  lpBuffer: Pointer; var lpdwBufferLength, lpdwIndex: DWORD): BOOL; stdcall;


//
// Cookie APIs
//

{$EXTERNALSYM InternetSetCookieA}
function InternetSetCookieA(lpszUrl, lpszCookieName,
  lpszCookieData: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM InternetSetCookieW}
function InternetSetCookieW(lpszUrl, lpszCookieName,
  lpszCookieData: PWideChar): BOOL; stdcall;
{$EXTERNALSYM InternetSetCookie}
function InternetSetCookie(lpszUrl, lpszCookieName,
  lpszCookieData: PTSTR): BOOL; stdcall;

{$EXTERNALSYM InternetGetCookieA}
function InternetGetCookieA(lpszUrl, lpszCookieName: PAnsiChar;
  lpCookieData: PAnsiChar; var lpdwSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetGetCookieW}
function InternetGetCookieW(lpszUrl, lpszCookieName: PWideChar;
  lpCookieData: PWideChar; var lpdwSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetGetCookie}
function InternetGetCookie(lpszUrl, lpszCookieName: PTSTR;
  lpCookieData: PTSTR; var lpdwSize: DWORD): BOOL; stdcall;


//
// offline browsing
//

function InternetAttemptConnect(dwReserved: DWORD): DWORD; stdcall;

{$EXTERNALSYM InternetCheckConnectionA}
function InternetCheckConnectionA(lpszUrl: PAnsiChar;
  dwFlags, dwReserved: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetCheckConnectionW}
function InternetCheckConnectionW(lpszUrl: PWideChar;
  dwFlags, dwReserved: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetCheckConnection}
function InternetCheckConnection(lpszUrl: PTSTR;
  dwFlags, dwReserved: DWORD): BOOL; stdcall;

const
  {$EXTERNALSYM FLAG_ICC_FORCE_CONNECTION}
  FLAG_ICC_FORCE_CONNECTION       = $00000001;


//
// Internet UI
//

// InternetErrorDlg - Provides UI for certain Errors.

  {$EXTERNALSYM FLAGS_ERROR_UI_FILTER_FOR_ERRORS}
  FLAGS_ERROR_UI_FILTER_FOR_ERRORS        = $01;
  {$EXTERNALSYM FLAGS_ERROR_UI_FLAGS_CHANGE_OPTIONS}
  FLAGS_ERROR_UI_FLAGS_CHANGE_OPTIONS     = $02;
  {$EXTERNALSYM FLAGS_ERROR_UI_FLAGS_GENERATE_DATA}
  FLAGS_ERROR_UI_FLAGS_GENERATE_DATA      = $04;
  {$EXTERNALSYM FLAGS_ERROR_UI_FLAGS_NO_UI}
  FLAGS_ERROR_UI_FLAGS_NO_UI              = $08;
  {$EXTERNALSYM FLAGS_ERROR_UI_SERIALIZE_DIALOGS}
  FLAGS_ERROR_UI_SERIALIZE_DIALOGS        = $10;

// If SERIALIZE_DIALOGS flag set, client should implement thread-safe non-blocking callback...

{$EXTERNALSYM InternetAuthNotifyCallback}
function InternetAuthNotifyCallback(dwContext: DWORD_PTR; dwReturn: DWORD;
  lpReserved: Pointer): DWORD; stdcall;

type
  {$EXTERNALSYM PFN_AUTH_NOTIFY}
  PFN_AUTH_NOTIFY = function(dwContext: DWORD_PTR; dwReturn: DWORD;
    lpReserved: Pointer): DWORD stdcall;
  PFNAuthNotify = PFN_AUTH_NOTIFY;

//
// ... and last parameter of InternetErrorDlg should point to...
//

type
  PInternetAuthNotifyData = ^TInternetAuthNotifyData;
  {$EXTERNALSYM INTERNET_AUTH_NOTIFY_DATA}
  INTERNET_AUTH_NOTIFY_DATA = record
    cbStruct: DWORD;    // size of this structure
    dwOptions: DWORD;   // reserved: must set to 0
    pfnNotify: PFNAuthNotify;  // notification callback to retry InternetErrorDlg
    dwContext: DWORD_PTR;  // context to pass to to notification function
  end;
  TInternetAuthNotifyData = INTERNET_AUTH_NOTIFY_DATA;

{$EXTERNALSYM ResumeSuspendedDownload}
function ResumeSuspendedDownload(hRequest: HINTERNET;
  dwResultCode: DWORD): BOOL; stdcall;


{$EXTERNALSYM InternetErrorDlg}
function InternetErrorDlg(hWnd: HWND; hRequest: HINTERNET;
  dwError, dwFlags: DWORD; var lppvData: Pointer): DWORD; stdcall;

{$EXTERNALSYM InternetConfirmZoneCrossingA}
function InternetConfirmZoneCrossingA(hWnd: HWND; szUrlPrev, szUrlNew: PAnsiChar;
  bPost: BOOL): DWORD; stdcall;
{$EXTERNALSYM InternetConfirmZoneCrossingW}
function InternetConfirmZoneCrossingW(hWnd: HWND; szUrlPrev, szUrlNew: PWideChar;
  bPost: BOOL): DWORD; stdcall;
{$EXTERNALSYM InternetConfirmZoneCrossing}
function InternetConfirmZoneCrossing(hWnd: HWND; szUrlPrev, szUrlNew: PTSTR;
  bPost: BOOL): DWORD; stdcall;

//
// Internet API error returns
//

const
  {$EXTERNALSYM INTERNET_ERROR_BASE}
  INTERNET_ERROR_BASE                     = 12000;

  {$EXTERNALSYM ERROR_INTERNET_OUT_OF_HANDLES}
  ERROR_INTERNET_OUT_OF_HANDLES           = INTERNET_ERROR_BASE + 1;
  {$EXTERNALSYM ERROR_INTERNET_TIMEOUT}
  ERROR_INTERNET_TIMEOUT                  = INTERNET_ERROR_BASE + 2;
  {$EXTERNALSYM ERROR_INTERNET_EXTENDED_ERROR}
  ERROR_INTERNET_EXTENDED_ERROR           = INTERNET_ERROR_BASE + 3;
  {$EXTERNALSYM ERROR_INTERNET_INTERNAL_ERROR}
  ERROR_INTERNET_INTERNAL_ERROR           = INTERNET_ERROR_BASE + 4;
  {$EXTERNALSYM ERROR_INTERNET_INVALID_URL}
  ERROR_INTERNET_INVALID_URL              = INTERNET_ERROR_BASE + 5;
  {$EXTERNALSYM ERROR_INTERNET_UNRECOGNIZED_SCHEME}
  ERROR_INTERNET_UNRECOGNIZED_SCHEME      = INTERNET_ERROR_BASE + 6;
  {$EXTERNALSYM ERROR_INTERNET_NAME_NOT_RESOLVED}
  ERROR_INTERNET_NAME_NOT_RESOLVED        = INTERNET_ERROR_BASE + 7;
  {$EXTERNALSYM ERROR_INTERNET_PROTOCOL_NOT_FOUND}
  ERROR_INTERNET_PROTOCOL_NOT_FOUND       = INTERNET_ERROR_BASE + 8;
  {$EXTERNALSYM ERROR_INTERNET_INVALID_OPTION}
  ERROR_INTERNET_INVALID_OPTION           = INTERNET_ERROR_BASE + 9;
  {$EXTERNALSYM ERROR_INTERNET_BAD_OPTION_LENGTH}
  ERROR_INTERNET_BAD_OPTION_LENGTH        = INTERNET_ERROR_BASE + 10;
  {$EXTERNALSYM ERROR_INTERNET_OPTION_NOT_SETTABLE}
  ERROR_INTERNET_OPTION_NOT_SETTABLE      = INTERNET_ERROR_BASE + 11;
  {$EXTERNALSYM ERROR_INTERNET_SHUTDOWN}
  ERROR_INTERNET_SHUTDOWN                 = INTERNET_ERROR_BASE + 12;
  {$EXTERNALSYM ERROR_INTERNET_INCORRECT_USER_NAME}
  ERROR_INTERNET_INCORRECT_USER_NAME      = INTERNET_ERROR_BASE + 13;
  {$EXTERNALSYM ERROR_INTERNET_INCORRECT_PASSWORD}
  ERROR_INTERNET_INCORRECT_PASSWORD       = INTERNET_ERROR_BASE + 14;
  {$EXTERNALSYM ERROR_INTERNET_LOGIN_FAILURE}
  ERROR_INTERNET_LOGIN_FAILURE            = INTERNET_ERROR_BASE + 15;
  {$EXTERNALSYM ERROR_INTERNET_INVALID_OPERATION}
  ERROR_INTERNET_INVALID_OPERATION        = INTERNET_ERROR_BASE + 16;
  {$EXTERNALSYM ERROR_INTERNET_OPERATION_CANCELLED}
  ERROR_INTERNET_OPERATION_CANCELLED      = INTERNET_ERROR_BASE + 17;
  {$EXTERNALSYM ERROR_INTERNET_INCORRECT_HANDLE_TYPE}
  ERROR_INTERNET_INCORRECT_HANDLE_TYPE    = INTERNET_ERROR_BASE + 18;
  {$EXTERNALSYM ERROR_INTERNET_INCORRECT_HANDLE_STATE}
  ERROR_INTERNET_INCORRECT_HANDLE_STATE   = INTERNET_ERROR_BASE + 19;
  {$EXTERNALSYM ERROR_INTERNET_NOT_PROXY_REQUEST}
  ERROR_INTERNET_NOT_PROXY_REQUEST        = INTERNET_ERROR_BASE + 20;
  {$EXTERNALSYM ERROR_INTERNET_REGISTRY_VALUE_NOT_FOUND}
  ERROR_INTERNET_REGISTRY_VALUE_NOT_FOUND = INTERNET_ERROR_BASE + 21;
  {$EXTERNALSYM ERROR_INTERNET_BAD_REGISTRY_PARAMETER}
  ERROR_INTERNET_BAD_REGISTRY_PARAMETER   = INTERNET_ERROR_BASE + 22;
  {$EXTERNALSYM ERROR_INTERNET_NO_DIRECT_ACCESS}
  ERROR_INTERNET_NO_DIRECT_ACCESS         = INTERNET_ERROR_BASE + 23;
  {$EXTERNALSYM ERROR_INTERNET_NO_CONTEXT}
  ERROR_INTERNET_NO_CONTEXT               = INTERNET_ERROR_BASE + 24;
  {$EXTERNALSYM ERROR_INTERNET_NO_CALLBACK}
  ERROR_INTERNET_NO_CALLBACK              = INTERNET_ERROR_BASE + 25;
  {$EXTERNALSYM ERROR_INTERNET_REQUEST_PENDING}
  ERROR_INTERNET_REQUEST_PENDING          = INTERNET_ERROR_BASE + 26;
  {$EXTERNALSYM ERROR_INTERNET_INCORRECT_FORMAT}
  ERROR_INTERNET_INCORRECT_FORMAT         = INTERNET_ERROR_BASE + 27;
  {$EXTERNALSYM ERROR_INTERNET_ITEM_NOT_FOUND}
  ERROR_INTERNET_ITEM_NOT_FOUND           = INTERNET_ERROR_BASE + 28;
  {$EXTERNALSYM ERROR_INTERNET_CANNOT_CONNECT}
  ERROR_INTERNET_CANNOT_CONNECT           = INTERNET_ERROR_BASE + 29;
  {$EXTERNALSYM ERROR_INTERNET_CONNECTION_ABORTED}
  ERROR_INTERNET_CONNECTION_ABORTED       = INTERNET_ERROR_BASE + 30;
  {$EXTERNALSYM ERROR_INTERNET_CONNECTION_RESET}
  ERROR_INTERNET_CONNECTION_RESET         = INTERNET_ERROR_BASE + 31;
  {$EXTERNALSYM ERROR_INTERNET_FORCE_RETRY}
  ERROR_INTERNET_FORCE_RETRY              = INTERNET_ERROR_BASE + 32;
  {$EXTERNALSYM ERROR_INTERNET_INVALID_PROXY_REQUEST}
  ERROR_INTERNET_INVALID_PROXY_REQUEST    = INTERNET_ERROR_BASE + 33;
  {$EXTERNALSYM ERROR_INTERNET_NEED_UI}
  ERROR_INTERNET_NEED_UI                  = INTERNET_ERROR_BASE + 34;

  {$EXTERNALSYM ERROR_INTERNET_HANDLE_EXISTS}
  ERROR_INTERNET_HANDLE_EXISTS            = INTERNET_ERROR_BASE + 36;
  {$EXTERNALSYM ERROR_INTERNET_SEC_CERT_DATE_INVALID}
  ERROR_INTERNET_SEC_CERT_DATE_INVALID    = INTERNET_ERROR_BASE + 37;
  {$EXTERNALSYM ERROR_INTERNET_SEC_CERT_CN_INVALID}
  ERROR_INTERNET_SEC_CERT_CN_INVALID      = INTERNET_ERROR_BASE + 38;
  {$EXTERNALSYM ERROR_INTERNET_HTTP_TO_HTTPS_ON_REDIR}
  ERROR_INTERNET_HTTP_TO_HTTPS_ON_REDIR   = INTERNET_ERROR_BASE + 39;
  {$EXTERNALSYM ERROR_INTERNET_HTTPS_TO_HTTP_ON_REDIR}
  ERROR_INTERNET_HTTPS_TO_HTTP_ON_REDIR   = INTERNET_ERROR_BASE + 40;
  {$EXTERNALSYM ERROR_INTERNET_MIXED_SECURITY}
  ERROR_INTERNET_MIXED_SECURITY           = INTERNET_ERROR_BASE + 41;
  {$EXTERNALSYM ERROR_INTERNET_CHG_POST_IS_NON_SECURE}
  ERROR_INTERNET_CHG_POST_IS_NON_SECURE   = INTERNET_ERROR_BASE + 42;
  {$EXTERNALSYM ERROR_INTERNET_POST_IS_NON_SECURE}
  ERROR_INTERNET_POST_IS_NON_SECURE       = INTERNET_ERROR_BASE + 43;
  {$EXTERNALSYM ERROR_INTERNET_CLIENT_AUTH_CERT_NEEDED}
  ERROR_INTERNET_CLIENT_AUTH_CERT_NEEDED  = INTERNET_ERROR_BASE + 44;
  {$EXTERNALSYM ERROR_INTERNET_INVALID_CA}
  ERROR_INTERNET_INVALID_CA               = INTERNET_ERROR_BASE + 45;
  {$EXTERNALSYM ERROR_INTERNET_CLIENT_AUTH_NOT_SETUP}
  ERROR_INTERNET_CLIENT_AUTH_NOT_SETUP    = INTERNET_ERROR_BASE + 46;
  {$EXTERNALSYM ERROR_INTERNET_ASYNC_THREAD_FAILED}
  ERROR_INTERNET_ASYNC_THREAD_FAILED      = INTERNET_ERROR_BASE + 47;
  {$EXTERNALSYM ERROR_INTERNET_REDIRECT_SCHEME_CHANGE}
  ERROR_INTERNET_REDIRECT_SCHEME_CHANGE   = INTERNET_ERROR_BASE + 48;
  {$EXTERNALSYM ERROR_INTERNET_DIALOG_PENDING}
  ERROR_INTERNET_DIALOG_PENDING           = INTERNET_ERROR_BASE + 49;
  {$EXTERNALSYM ERROR_INTERNET_RETRY_DIALOG}
  ERROR_INTERNET_RETRY_DIALOG             = INTERNET_ERROR_BASE + 50;
  {$EXTERNALSYM ERROR_INTERNET_HTTPS_HTTP_SUBMIT_REDIR}
  ERROR_INTERNET_HTTPS_HTTP_SUBMIT_REDIR  = INTERNET_ERROR_BASE + 52;
  {$EXTERNALSYM ERROR_INTERNET_INSERT_CDROM}
  ERROR_INTERNET_INSERT_CDROM             = INTERNET_ERROR_BASE + 53;
  {$EXTERNALSYM ERROR_INTERNET_FORTEZZA_LOGIN_NEEDED}
  ERROR_INTERNET_FORTEZZA_LOGIN_NEEDED    = INTERNET_ERROR_BASE + 54;
  {$EXTERNALSYM ERROR_INTERNET_SEC_CERT_ERRORS}
  ERROR_INTERNET_SEC_CERT_ERRORS          = INTERNET_ERROR_BASE + 55;
  {$EXTERNALSYM ERROR_INTERNET_SEC_CERT_NO_REV}
  ERROR_INTERNET_SEC_CERT_NO_REV          = INTERNET_ERROR_BASE + 56;
  {$EXTERNALSYM ERROR_INTERNET_SEC_CERT_REV_FAILED}
  ERROR_INTERNET_SEC_CERT_REV_FAILED      = INTERNET_ERROR_BASE + 57;


// FTP API errors

  {$EXTERNALSYM ERROR_FTP_TRANSFER_IN_PROGRESS}
  ERROR_FTP_TRANSFER_IN_PROGRESS          = INTERNET_ERROR_BASE + 110;
  {$EXTERNALSYM ERROR_FTP_DROPPED}
  ERROR_FTP_DROPPED                       = INTERNET_ERROR_BASE + 111;
  {$EXTERNALSYM ERROR_FTP_NO_PASSIVE_MODE}
  ERROR_FTP_NO_PASSIVE_MODE               = INTERNET_ERROR_BASE + 112;

  // gopher API errors

  {$EXTERNALSYM ERROR_GOPHER_PROTOCOL_ERROR}
  ERROR_GOPHER_PROTOCOL_ERROR             = INTERNET_ERROR_BASE + 130;
  {$EXTERNALSYM ERROR_GOPHER_NOT_FILE}
  ERROR_GOPHER_NOT_FILE                   = INTERNET_ERROR_BASE + 131;
  {$EXTERNALSYM ERROR_GOPHER_DATA_ERROR}
  ERROR_GOPHER_DATA_ERROR                 = INTERNET_ERROR_BASE + 132;
  {$EXTERNALSYM ERROR_GOPHER_END_OF_DATA}
  ERROR_GOPHER_END_OF_DATA                = INTERNET_ERROR_BASE + 133;
  {$EXTERNALSYM ERROR_GOPHER_INVALID_LOCATOR}
  ERROR_GOPHER_INVALID_LOCATOR            = INTERNET_ERROR_BASE + 134;
  {$EXTERNALSYM ERROR_GOPHER_INCORRECT_LOCATOR_TYPE}
  ERROR_GOPHER_INCORRECT_LOCATOR_TYPE     = INTERNET_ERROR_BASE + 135;
  {$EXTERNALSYM ERROR_GOPHER_NOT_GOPHER_PLUS}
  ERROR_GOPHER_NOT_GOPHER_PLUS            = INTERNET_ERROR_BASE + 136;
  {$EXTERNALSYM ERROR_GOPHER_ATTRIBUTE_NOT_FOUND}
  ERROR_GOPHER_ATTRIBUTE_NOT_FOUND        = INTERNET_ERROR_BASE + 137;
  {$EXTERNALSYM ERROR_GOPHER_UNKNOWN_LOCATOR}
  ERROR_GOPHER_UNKNOWN_LOCATOR            = INTERNET_ERROR_BASE + 138;

  // HTTP API errors

  {$EXTERNALSYM ERROR_HTTP_HEADER_NOT_FOUND}
  ERROR_HTTP_HEADER_NOT_FOUND             = INTERNET_ERROR_BASE + 150;
  {$EXTERNALSYM ERROR_HTTP_DOWNLEVEL_SERVER}
  ERROR_HTTP_DOWNLEVEL_SERVER             = INTERNET_ERROR_BASE + 151;
  {$EXTERNALSYM ERROR_HTTP_INVALID_SERVER_RESPONSE}
  ERROR_HTTP_INVALID_SERVER_RESPONSE      = INTERNET_ERROR_BASE + 152;
  {$EXTERNALSYM ERROR_HTTP_INVALID_HEADER}
  ERROR_HTTP_INVALID_HEADER               = INTERNET_ERROR_BASE + 153;
  {$EXTERNALSYM ERROR_HTTP_INVALID_QUERY_REQUEST}
  ERROR_HTTP_INVALID_QUERY_REQUEST        = INTERNET_ERROR_BASE + 154;
  {$EXTERNALSYM ERROR_HTTP_HEADER_ALREADY_EXISTS}
  ERROR_HTTP_HEADER_ALREADY_EXISTS        = INTERNET_ERROR_BASE + 155;
  {$EXTERNALSYM ERROR_HTTP_REDIRECT_FAILED}
  ERROR_HTTP_REDIRECT_FAILED              = INTERNET_ERROR_BASE + 156;
  {$EXTERNALSYM ERROR_HTTP_NOT_REDIRECTED}
  ERROR_HTTP_NOT_REDIRECTED               = INTERNET_ERROR_BASE + 160;
  {$EXTERNALSYM ERROR_HTTP_COOKIE_NEEDS_CONFIRMATION}
  ERROR_HTTP_COOKIE_NEEDS_CONFIRMATION    = INTERNET_ERROR_BASE + 161;
  {$EXTERNALSYM ERROR_HTTP_COOKIE_DECLINED}
  ERROR_HTTP_COOKIE_DECLINED              = INTERNET_ERROR_BASE + 162;
  {$EXTERNALSYM ERROR_HTTP_REDIRECT_NEEDS_CONFIRMATION}
  ERROR_HTTP_REDIRECT_NEEDS_CONFIRMATION  = INTERNET_ERROR_BASE + 168;

  // additional Internet API error codes

  {$EXTERNALSYM ERROR_INTERNET_SECURITY_CHANNEL_ERROR}
  ERROR_INTERNET_SECURITY_CHANNEL_ERROR   = INTERNET_ERROR_BASE + 157;
  {$EXTERNALSYM ERROR_INTERNET_UNABLE_TO_CACHE_FILE}
  ERROR_INTERNET_UNABLE_TO_CACHE_FILE     = INTERNET_ERROR_BASE + 158;
  {$EXTERNALSYM ERROR_INTERNET_TCPIP_NOT_INSTALLED}
  ERROR_INTERNET_TCPIP_NOT_INSTALLED      = INTERNET_ERROR_BASE + 159;
  {$EXTERNALSYM ERROR_INTERNET_DISCONNECTED}
  ERROR_INTERNET_DISCONNECTED             = INTERNET_ERROR_BASE + 163;
  {$EXTERNALSYM ERROR_INTERNET_SERVER_UNREACHABLE}
  ERROR_INTERNET_SERVER_UNREACHABLE       = INTERNET_ERROR_BASE + 164;
  {$EXTERNALSYM ERROR_INTERNET_PROXY_SERVER_UNREACHABLE}
  ERROR_INTERNET_PROXY_SERVER_UNREACHABLE = INTERNET_ERROR_BASE + 165;

  {$EXTERNALSYM ERROR_INTERNET_BAD_AUTO_PROXY_SCRIPT}
  ERROR_INTERNET_BAD_AUTO_PROXY_SCRIPT    = INTERNET_ERROR_BASE + 166;
  {$EXTERNALSYM ERROR_INTERNET_UNABLE_TO_DOWNLOAD_SCRIPT}
  ERROR_INTERNET_UNABLE_TO_DOWNLOAD_SCRIPT = INTERNET_ERROR_BASE + 167;
  {$EXTERNALSYM ERROR_INTERNET_SEC_INVALID_CERT}
  ERROR_INTERNET_SEC_INVALID_CERT         = INTERNET_ERROR_BASE + 169;
  {$EXTERNALSYM ERROR_INTERNET_SEC_CERT_REVOKED}
  ERROR_INTERNET_SEC_CERT_REVOKED         = INTERNET_ERROR_BASE + 170;

// InternetAutodial specific errors

  {$EXTERNALSYM ERROR_INTERNET_FAILED_DUETOSECURITYCHECK}
  ERROR_INTERNET_FAILED_DUETOSECURITYCHECK = INTERNET_ERROR_BASE + 171;
  {$EXTERNALSYM ERROR_INTERNET_NOT_INITIALIZED}
  ERROR_INTERNET_NOT_INITIALIZED          = INTERNET_ERROR_BASE + 172;
  {$EXTERNALSYM ERROR_INTERNET_NEED_MSN_SSPI_PKG}
  ERROR_INTERNET_NEED_MSN_SSPI_PKG        = INTERNET_ERROR_BASE + 173;
  {$EXTERNALSYM ERROR_INTERNET_LOGIN_FAILURE_DISPLAY_ENTITY_BODY}
  ERROR_INTERNET_LOGIN_FAILURE_DISPLAY_ENTITY_BODY = INTERNET_ERROR_BASE + 174;

  {$EXTERNALSYM INTERNET_ERROR_LAST}
  INTERNET_ERROR_LAST = ERROR_INTERNET_LOGIN_FAILURE_DISPLAY_ENTITY_BODY;



//
// URLCACHE APIs
//

// datatype definitions.

// cache entry type flags.

  {$EXTERNALSYM NORMAL_CACHE_ENTRY}
  NORMAL_CACHE_ENTRY              = $00000001;
  {$EXTERNALSYM STICKY_CACHE_ENTRY}
  STICKY_CACHE_ENTRY              = $00000004;
  {$EXTERNALSYM EDITED_CACHE_ENTRY}
  EDITED_CACHE_ENTRY              = $00000008;
  {$EXTERNALSYM TRACK_OFFLINE_CACHE_ENTRY}
  TRACK_OFFLINE_CACHE_ENTRY       = $00000010;
  {$EXTERNALSYM TRACK_ONLINE_CACHE_ENTRY}
  TRACK_ONLINE_CACHE_ENTRY        = $00000020;
  {$EXTERNALSYM COOKIE_CACHE_ENTRY}
  COOKIE_CACHE_ENTRY              = $00100000;
  {$EXTERNALSYM URLHISTORY_CACHE_ENTRY}
  URLHISTORY_CACHE_ENTRY          = $00200000;
  {$EXTERNALSYM SPARSE_CACHE_ENTRY}
  SPARSE_CACHE_ENTRY              = $00010000;

  {$EXTERNALSYM URLCACHE_FIND_DEFAULT_FILTER}
  URLCACHE_FIND_DEFAULT_FILTER    = NORMAL_CACHE_ENTRY or
                                    COOKIE_CACHE_ENTRY or
                                    URLHISTORY_CACHE_ENTRY or
                                    TRACK_OFFLINE_CACHE_ENTRY or
                                    TRACK_ONLINE_CACHE_ENTRY or
                                    STICKY_CACHE_ENTRY;

type

  PInternetCacheEntryInfoA = ^TInternetCacheEntryInfoA;
  PInternetCacheEntryInfoW = ^TInternetCacheEntryInfoW;
  PInternetCacheEntryInfo = PInternetCacheEntryInfoA;
  {$EXTERNALSYM _INTERNET_CACHE_ENTRY_INFOA}
  _INTERNET_CACHE_ENTRY_INFOA = record
    dwStructSize: DWORD;          // version of cache system.
    lpszSourceUrlName: PAnsiChar; // embedded pointer to the URL name string.
    lpszLocalFileName: PAnsiChar; // embedded pointer to the local file name.
    CacheEntryType: DWORD;        // cache type bit mask.
    dwUseCount: DWORD;            // current users count of the cache entry.
    dwHitRate: DWORD;             // num of times the cache entry was retrieved.
    dwSizeLow: DWORD;             // low DWORD of the file size.
    dwSizeHigh: DWORD;            // high DWORD of the file size.
    LastModifiedTime: TFileTime;  // last modified time of the file in GMT format.
    ExpireTime: TFileTime;        // expire time of the file in GMT format
    LastAccessTime: TFileTime;    // last accessed time in GMT format
    LastSyncTime: TFileTime;      // last time the URL was synchronized
                                  // with the source
    lpHeaderInfo: PByte;          // embedded pointer to the header info.
    dwHeaderInfoSiz: DWORD;       // size of the above header.
    lpszFileExtension: PAnsiChar;    // File extension used to retrive the urldata as a file.
    case Integer of
      1: (dwReserved: DWORD);
      2: (dwExemptDelta: DWORD);  // Exemption delta from last access time.
  end;
  {$EXTERNALSYM _INTERNET_CACHE_ENTRY_INFOW}
  _INTERNET_CACHE_ENTRY_INFOW = record
    dwStructSize: DWORD;          // version of cache system.
    lpszSourceUrlName: PWideChar; // embedded pointer to the URL name string.
    lpszLocalFileName: PWideChar; // embedded pointer to the local file name.
    CacheEntryType: DWORD;        // cache type bit mask.
    dwUseCount: DWORD;            // current users count of the cache entry.
    dwHitRate: DWORD;             // num of times the cache entry was retrieved.
    dwSizeLow: DWORD;             // low DWORD of the file size.
    dwSizeHigh: DWORD;            // high DWORD of the file size.
    LastModifiedTime: TFileTime;  // last modified time of the file in GMT format.
    ExpireTime: TFileTime;        // expire time of the file in GMT format
    LastAccessTime: TFileTime;    // last accessed time in GMT format
    LastSyncTime: TFileTime;      // last time the URL was synchronized
                                  // with the source
    lpHeaderInfo: PByte;          // embedded pointer to the header info.
    dwHeaderInfoSiz: DWORD;       // size of the above header.
    lpszFileExtension: PWideChar;    // File extension used to retrive the urldata as a file.
    case Integer of
      1: (dwReserved: DWORD);
      2: (dwExemptDelta: DWORD);  // Exemption delta from last access time.
  end;
  {$EXTERNALSYM _INTERNET_CACHE_ENTRY_INFO}
  _INTERNET_CACHE_ENTRY_INFO = _INTERNET_CACHE_ENTRY_INFOA;
  {$EXTERNALSYM INTERNET_CACHE_ENTRY_INFOA}
  INTERNET_CACHE_ENTRY_INFOA = _INTERNET_CACHE_ENTRY_INFOA;
  {$EXTERNALSYM INTERNET_CACHE_ENTRY_INFOW}
  INTERNET_CACHE_ENTRY_INFOW = _INTERNET_CACHE_ENTRY_INFOW;
  {$EXTERNALSYM INTERNET_CACHE_ENTRY_INFO}
  INTERNET_CACHE_ENTRY_INFO = INTERNET_CACHE_ENTRY_INFOA;
  TInternetCacheEntryInfoA = _INTERNET_CACHE_ENTRY_INFOA;
  TInternetCacheEntryInfoW = _INTERNET_CACHE_ENTRY_INFOW;
  TInternetCacheEntryInfo = TInternetCacheEntryInfoA;

type
  PInternetCacheTimeStamps = ^TInternetCacheTimeStamps;
  {$EXTERNALSYM _INTERNET_CACHE_TIMESTAMPS}
  _INTERNET_CACHE_TIMESTAMPS = record
    ftExpires: TFileTime;
    ftLastModified: TFileTime;
  end;
  {$EXTERNALSYM INTERNET_CACHE_TIMESTAMPS}
  INTERNET_CACHE_TIMESTAMPS = _INTERNET_CACHE_TIMESTAMPS;
  TInternetCacheTimeStamps = _INTERNET_CACHE_TIMESTAMPS;


//
// Cache Group
//
type
  {$EXTERNALSYM GROUPID}
  GROUPID = Int64;
  TGroupID = GROUPID;

//
// Cache Group Flags
//
const
  {$EXTERNALSYM CACHEGROUP_ATTRIBUTE_GET_ALL}
  CACHEGROUP_ATTRIBUTE_GET_ALL      = $FFFFFFFF;
  {$EXTERNALSYM CACHEGROUP_ATTRIBUTE_BASIC}
  CACHEGROUP_ATTRIBUTE_BASIC        = $00000001;
  {$EXTERNALSYM CACHEGROUP_ATTRIBUTE_FLAG}
  CACHEGROUP_ATTRIBUTE_FLAG         = $00000002;
  {$EXTERNALSYM CACHEGROUP_ATTRIBUTE_TYPE}
  CACHEGROUP_ATTRIBUTE_TYPE         = $00000004;
  {$EXTERNALSYM CACHEGROUP_ATTRIBUTE_QUOTA}
  CACHEGROUP_ATTRIBUTE_QUOTA        = $00000008;
  {$EXTERNALSYM CACHEGROUP_ATTRIBUTE_GROUPNAME}
  CACHEGROUP_ATTRIBUTE_GROUPNAME    = $00000010;
  {$EXTERNALSYM CACHEGROUP_ATTRIBUTE_STORAGE}
  CACHEGROUP_ATTRIBUTE_STORAGE      = $00000020;

  {$EXTERNALSYM CACHEGROUP_FLAG_NONPURGEABLE}
  CACHEGROUP_FLAG_NONPURGEABLE      = $00000001;
  {$EXTERNALSYM CACHEGROUP_FLAG_GIDONLY}
  CACHEGROUP_FLAG_GIDONLY           = $00000004;

  {$EXTERNALSYM CACHEGROUP_FLAG_FLUSHURL_ONDELETE}
  CACHEGROUP_FLAG_FLUSHURL_ONDELETE = $00000002;

  {$EXTERNALSYM CACHEGROUP_SEARCH_ALL}
  CACHEGROUP_SEARCH_ALL             = $00000000;
  {$EXTERNALSYM CACHEGROUP_SEARCH_BYURL}
  CACHEGROUP_SEARCH_BYURL           = $00000001;

  {$EXTERNALSYM CACHEGROUP_TYPE_INVALID}
  CACHEGROUP_TYPE_INVALID           = $00000001;


//
// updatable cache group fields
//

  {$EXTERNALSYM CACHEGROUP_READWRITE_MASK}
  CACHEGROUP_READWRITE_MASK = CACHEGROUP_ATTRIBUTE_TYPE or
                              CACHEGROUP_ATTRIBUTE_QUOTA or
                              CACHEGROUP_ATTRIBUTE_GROUPNAME or
                              CACHEGROUP_ATTRIBUTE_STORAGE;

//
// INTERNET_CACHE_GROUP_INFO
//

  {$EXTERNALSYM GROUPNAME_MAX_LENGTH}
  GROUPNAME_MAX_LENGTH     = 120;
  {$EXTERNALSYM GROUP_OWNER_STORAGE_SIZE}
  GROUP_OWNER_STORAGE_SIZE = 4;

type
  PInternetCacheGroupInfoA = ^TInternetCacheGroupInfoA;
  PInternetCacheGroupInfoW = ^TInternetCacheGroupInfoW;
  PInternetCacheGroupInfo = PInternetCacheGroupInfoA;
  {$EXTERNALSYM _INTERNET_CACHE_GROUP_INFOA}
  _INTERNET_CACHE_GROUP_INFOA = record
    dwGroupSize: DWORD;
    dwGroupFlags: DWORD;
    dwGroupType: DWORD;
    dwDiskUsage: DWORD;    // in KB
    dwDiskQuota: DWORD;    // in KB
    dwOwnerStorage: array[0..GROUP_OWNER_STORAGE_SIZE - 1] of DWORD;
    szGroupName: array[0..GROUPNAME_MAX_LENGTH - 1] of AnsiChar;
  end;
  {$EXTERNALSYM _INTERNET_CACHE_GROUP_INFOW}
  _INTERNET_CACHE_GROUP_INFOW = record
    dwGroupSize: DWORD;
    dwGroupFlags: DWORD;
    dwGroupType: DWORD;
    dwDiskUsage: DWORD;    // in KB
    dwDiskQuota: DWORD;    // in KB
    dwOwnerStorage: array[0..GROUP_OWNER_STORAGE_SIZE - 1] of DWORD;
    szGroupName: array[0..GROUPNAME_MAX_LENGTH - 1] of WideChar;
  end;
  {$EXTERNALSYM _INTERNET_CACHE_GROUP_INFO}
  _INTERNET_CACHE_GROUP_INFO = _INTERNET_CACHE_GROUP_INFOA;
  {$EXTERNALSYM INTERNET_CACHE_GROUP_INFOA}
  INTERNET_CACHE_GROUP_INFOA = _INTERNET_CACHE_GROUP_INFOA;
  {$EXTERNALSYM INTERNET_CACHE_GROUP_INFOW}
  INTERNET_CACHE_GROUP_INFOW = _INTERNET_CACHE_GROUP_INFOW;
  {$EXTERNALSYM INTERNET_CACHE_GROUP_INFO}
  INTERNET_CACHE_GROUP_INFO = INTERNET_CACHE_GROUP_INFOA;
  TInternetCacheGroupInfoA = _INTERNET_CACHE_GROUP_INFOA;
  TInternetCacheGroupInfoW = _INTERNET_CACHE_GROUP_INFOW;
  TInternetCacheGroupInfo = TInternetCacheGroupInfoA;

//
// Cache APIs
//

{$EXTERNALSYM CreateUrlCacheEntryA}
function CreateUrlCacheEntryA(lpszUrlName: PAnsiChar; dwExpectedFileSize: DWORD;
 lpszFileExtension: PAnsiChar; lpszFileName: PAnsiChar; dwReserved: DWORD): BOOL; stdcall;
{$EXTERNALSYM CreateUrlCacheEntryW}
function CreateUrlCacheEntryW(lpszUrlName: PWideChar; dwExpectedFileSize: DWORD;
 lpszFileExtension: PWideChar; lpszFileName: PWideChar; dwReserved: DWORD): BOOL; stdcall;
{$EXTERNALSYM CreateUrlCacheEntry}
function CreateUrlCacheEntry(lpszUrlName: PTSTR; dwExpectedFileSize: DWORD;
 lpszFileExtension: PTSTR; lpszFileName: PTSTR; dwReserved: DWORD): BOOL; stdcall;

// Temporary state of affairs until we reconcile our apis.

// Why are we doing this? HeaderInfo _should_ be string data.
// However, one group is passing binary data instead. For the
// unicode api, we've decided to disallow this, but this
// brings up an inconsistency between the u and a apis, which
// is undesirable.

// For Beta 1, we'll go with this behaviour, but in future releases
// we want to make these apis consistent.

{$EXTERNALSYM CommitUrlCacheEntryA}
function CommitUrlCacheEntryA(lpszUrlName, lpszLocalFileName: PAnsiChar;
  ExpireTime, LastModifiedTime: TFileTime; CacheEntryType: DWORD;
  lpHeaderInfo: PByte; dwHeaderSize: DWORD; lpszFileExtension: PAnsiChar;
  lpszOriginalUrl: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM CommitUrlCacheEntryW}
function CommitUrlCacheEntryW(lpszUrlName, lpszLocalFileName: PWideChar;
  ExpireTime, LastModifiedTime: TFileTime; CacheEntryType: DWORD;
  lpHeaderInfo: PByte; dwHeaderSize: DWORD; lpszFileExtension: PWideChar;
  lpszOriginalUrl: PWideChar): BOOL; stdcall;
{$EXTERNALSYM CommitUrlCacheEntry}
function CommitUrlCacheEntry(lpszUrlName, lpszLocalFileName: PTSTR;
  ExpireTime, LastModifiedTime: TFileTime; CacheEntryType: DWORD;
  lpHeaderInfo: PByte; dwHeaderSize: DWORD; lpszFileExtension: PTSTR;
  lpszOriginalUrl: PTSTR): BOOL; stdcall;

{$EXTERNALSYM RetrieveUrlCacheEntryFileA}
function RetrieveUrlCacheEntryFileA(lpszUrlName: PAnsiChar; lpCacheEntryInfo:
  PInternetCacheEntryInfoA; var lpdwCacheEntryInfoBufferSize: DWORD;
  dwReserved: DWORD): BOOL; stdcall;
{$EXTERNALSYM RetrieveUrlCacheEntryFileW}
function RetrieveUrlCacheEntryFileW(lpszUrlName: PAnsiChar; lpCacheEntryInfo:
  PInternetCacheEntryInfoW; var lpdwCacheEntryInfoBufferSize: DWORD;
  dwReserved: DWORD): BOOL; stdcall;
{$EXTERNALSYM RetrieveUrlCacheEntryFile}
function RetrieveUrlCacheEntryFile(lpszUrlName: PAnsiChar; lpCacheEntryInfo:
  PInternetCacheEntryInfo; var lpdwCacheEntryInfoBufferSize: DWORD;
  dwReserved: DWORD): BOOL; stdcall;

{$EXTERNALSYM UnlockUrlCacheEntryFileA}
function UnlockUrlCacheEntryFileA(lpszUrlName: PAnsiChar;
  dwReserved: DWORD): BOOL; stdcall;
{$EXTERNALSYM UnlockUrlCacheEntryFileW}
function UnlockUrlCacheEntryFileW(lpszUrlName: PWideChar;
  dwReserved: DWORD): BOOL; stdcall;
{$EXTERNALSYM UnlockUrlCacheEntryFile}
function UnlockUrlCacheEntryFile(lpszUrlName: PTSTR;
  dwReserved: DWORD): BOOL; stdcall;

{$EXTERNALSYM RetrieveUrlCacheEntryStreamA}
function RetrieveUrlCacheEntryStreamA(lpszUrlName: PAnsiChar; lpCacheEntryInfo:
  PInternetCacheEntryInfoA; var lpdwCacheEntryInfoBufferSize: DWORD;
  fRandomRead: BOOL; dwReserved: DWORD): THandle; stdcall;
{$EXTERNALSYM RetrieveUrlCacheEntryStreamW}
function RetrieveUrlCacheEntryStreamW(lpszUrlName: PWideChar; lpCacheEntryInfo:
  PInternetCacheEntryInfoW; var lpdwCacheEntryInfoBufferSize: DWORD;
  fRandomRead: BOOL; dwReserved: DWORD): THandle; stdcall;
{$EXTERNALSYM RetrieveUrlCacheEntryStream}
function RetrieveUrlCacheEntryStream(lpszUrlName: PTSTR; lpCacheEntryInfo:
  PInternetCacheEntryInfo; var lpdwCacheEntryInfoBufferSize: DWORD;
  fRandomRead: BOOL; dwReserved: DWORD): THandle; stdcall;

{$EXTERNALSYM ReadUrlCacheEntryStream}
function ReadUrlCacheEntryStream(hUrlCacheStream: THandle; dwLocation: DWORD;
  lpBuffer: Pointer; var lpdwLen: DWORD; Reserved: DWORD): BOOL; stdcall;
{$EXTERNALSYM UnlockUrlCacheEntryStream}
function UnlockUrlCacheEntryStream(hUrlCacheStream: THandle; Reserved: DWORD): BOOL; stdcall;

{$EXTERNALSYM GetUrlCacheEntryInfoA}
function GetUrlCacheEntryInfoA(lpszUrlName: PAnsiChar; lpCacheEntryInfo:
  PInternetCacheEntryInfoA; var lpdwCacheEntryInfoBufferSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM GetUrlCacheEntryInfoW}
function GetUrlCacheEntryInfoW(lpszUrlName: PWideChar; lpCacheEntryInfo:
  PInternetCacheEntryInfoW; var lpdwCacheEntryInfoBufferSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM GetUrlCacheEntryInfo}
function GetUrlCacheEntryInfo(lpszUrlName: PTSTR; lpCacheEntryInfo:
  PInternetCacheEntryInfo; var lpdwCacheEntryInfoBufferSize: DWORD): BOOL; stdcall;

//
// Cache Group Functions
//

{$EXTERNALSYM FindFirstUrlCacheGroup}
function FindFirstUrlCacheGroup(dwFlags, dwFilter: DWORD;
  lpSearchCondition: Pointer; dwSearchCondition: DWORD;
  var lpGroupId: GROUPID; lpReserved: Pointer): THandle; stdcall;

{$EXTERNALSYM FindNextUrlCacheGroup}
function FindNextUrlCacheGroup(hFind: THandle;
 var lpGroupId: GROUPID; lpReserved: Pointer): BOOL; stdcall;

{$EXTERNALSYM GetUrlCacheGroupAttributeA}
function GetUrlCacheGroupAttributeA(gid: GROUPID;
  dwFlags, dwAttributes: DWORD; var GroupInfo: TInternetCacheGroupInfoA;
  var dwGroupInfo: DWORD; lpReserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM GetUrlCacheGroupAttributeW}
function GetUrlCacheGroupAttributeW(gid: GROUPID;
  dwFlags, dwAttributes: DWORD; var GroupInfo: TInternetCacheGroupInfoW;
  var dwGroupInfo: DWORD; lpReserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM GetUrlCacheGroupAttribute}
function GetUrlCacheGroupAttribute(gid: GROUPID;
  dwFlags, dwAttributes: DWORD; var GroupInfo: TInternetCacheGroupInfo;
  var dwGroupInfo: DWORD; lpReserved: Pointer): BOOL; stdcall;

{$EXTERNALSYM SetUrlCacheGroupAttributeA}
function SetUrlCacheGroupAttributeA(gid: GROUPID;
  dwFlags, dwAttributes: DWORD;
  var lpGroupInfo: TInternetCacheGroupInfoA;
  lpReserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetUrlCacheGroupAttributeW}
function SetUrlCacheGroupAttributeW(gid: GROUPID;
  dwFlags, dwAttributes: DWORD;
  var lpGroupInfo: TInternetCacheGroupInfoW;
  lpReserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetUrlCacheGroupAttribute}
function SetUrlCacheGroupAttribute(gid: GROUPID;
  dwFlags, dwAttributes: DWORD;
  var lpGroupInfo: TInternetCacheGroupInfo;
  lpReserved: Pointer): BOOL; stdcall;

{$EXTERNALSYM CreateUrlCacheGroup}
function CreateUrlCacheGroup(dwFlags: DWORD;
  lpReserved: Pointer): GROUPID; stdcall;

{$EXTERNALSYM DeleteUrlCacheGroup}
function DeleteUrlCacheGroup(GroupId: GROUPID; dwFlags: DWORD;
  lpReserved: Pointer): BOOL; stdcall;

{$EXTERNALSYM GetUrlCacheEntryInfoExA}
function GetUrlCacheEntryInfoExA(lpszUrl: PAnsiChar; lpCacheEntryInfo:
  PInternetCacheEntryInfoA; var lpdwCacheEntryInfoBufSize: DWORD;
  lpszReserved: PAnsiChar; lpdwReserved: LPDWORD; lpReserved: Pointer;
  dwFlag: DWORD): BOOL; stdcall;
{$EXTERNALSYM GetUrlCacheEntryInfoExW}
function GetUrlCacheEntryInfoExW(lpszUrl: PWideChar; lpCacheEntryInfo:
  PInternetCacheEntryInfoW; var lpdwCacheEntryInfoBufSize: DWORD;
  lpszReserved: PWideChar; lpdwReserved: LPDWORD; lpReserved: Pointer;
  dwFlag: DWORD): BOOL; stdcall;
{$EXTERNALSYM GetUrlCacheEntryInfoEx}
function GetUrlCacheEntryInfoEx(lpszUrl: PTSTR; lpCacheEntryInfo:
  PInternetCacheEntryInfo; var lpdwCacheEntryInfoBufSize: DWORD;
  lpszReserved: PTSTR; lpdwReserved: LPDWORD; lpReserved: Pointer;
  dwFlag: DWORD): BOOL; stdcall;

const
  {$EXTERNALSYM CACHE_ENTRY_ATTRIBUTE_FC}
  CACHE_ENTRY_ATTRIBUTE_FC    = $00000004;
  {$EXTERNALSYM CACHE_ENTRY_HITRATE_FC}
  CACHE_ENTRY_HITRATE_FC      = $00000010;
  {$EXTERNALSYM CACHE_ENTRY_MODTIME_FC}
  CACHE_ENTRY_MODTIME_FC      = $00000040;
  {$EXTERNALSYM CACHE_ENTRY_EXPTIME_FC}
  CACHE_ENTRY_EXPTIME_FC      = $00000080;
  {$EXTERNALSYM CACHE_ENTRY_ACCTIME_FC}
  CACHE_ENTRY_ACCTIME_FC      = $00000100;
  {$EXTERNALSYM CACHE_ENTRY_SYNCTIME_FC}
  CACHE_ENTRY_SYNCTIME_FC     = $00000200;
  {$EXTERNALSYM CACHE_ENTRY_HEADERINFO_FC}
  CACHE_ENTRY_HEADERINFO_FC   = $00000400;
  {$EXTERNALSYM CACHE_ENTRY_EXEMPT_DELTA_FC}
  CACHE_ENTRY_EXEMPT_DELTA_FC = $00000800;


{$EXTERNALSYM SetUrlCacheEntryInfoA}
function SetUrlCacheEntryInfoA(lpszUrlName: PAnsiChar; lpCacheEntryInfo:
  PInternetCacheEntryInfoA; dwFieldControl: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetUrlCacheEntryInfoW}
function SetUrlCacheEntryInfoW(lpszUrlName: PWideChar; lpCacheEntryInfo:
  PInternetCacheEntryInfoW; dwFieldControl: DWORD): BOOL; stdcall;
{$EXTERNALSYM SetUrlCacheEntryInfo}
function SetUrlCacheEntryInfo(lpszUrlName: PTSTR; lpCacheEntryInfo:
  PInternetCacheEntryInfo; dwFieldControl: DWORD): BOOL; stdcall;

// Flags for SetUrlCacheEntryGroup
const
  {$EXTERNALSYM INTERNET_CACHE_GROUP_ADD}
  INTERNET_CACHE_GROUP_ADD      = 0;
  {$EXTERNALSYM INTERNET_CACHE_GROUP_REMOVE}
  INTERNET_CACHE_GROUP_REMOVE   = 1;

{$EXTERNALSYM SetUrlCacheEntryGroupA}
function SetUrlCacheEntryGroupA(lpszUrlName: PAnsiChar; dwFlags: DWORD; GroupId: GROUPID;
  pbGroupAttributes: PByte; cbGroupAttributes: DWORD; lpReserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetUrlCacheEntryGroupW}
function SetUrlCacheEntryGroupW(lpszUrlName: PWideChar; dwFlags: DWORD; GroupId: GROUPID;
  pbGroupAttributes: PByte; cbGroupAttributes: DWORD; lpReserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM SetUrlCacheEntryGroup}
function SetUrlCacheEntryGroup(lpszUrlName: PTSTR; dwFlags: DWORD; GroupId: GROUPID;
  pbGroupAttributes: PByte; cbGroupAttributes: DWORD; lpReserved: Pointer): BOOL; stdcall;

{$EXTERNALSYM FindFirstUrlCacheEntryExA}
function FindFirstUrlCacheEntryExA(lpszUrlSearchPattern: PAnsiChar; dwFlags,
  dwFilter: DWORD; GroupId: TGroupID; lpFirstCacheEntryInfo:
  PInternetCacheEntryInfoA; var lpdwFirstCacheEntryInfoBufferSize: DWORD;
  lpGroupAttributes: Pointer; pcbGroupAttributes: LPDWORD;
  lpReserved: Pointer): THandle; stdcall;
{$EXTERNALSYM FindFirstUrlCacheEntryExW}
function FindFirstUrlCacheEntryExW(lpszUrlSearchPattern: PWideChar; dwFlags,
  dwFilter: DWORD; GroupId: TGroupID; lpFirstCacheEntryInfo:
  PInternetCacheEntryInfoW; var lpdwFirstCacheEntryInfoBufferSize: DWORD;
  lpGroupAttributes: Pointer; pcbGroupAttributes: LPDWORD;
  lpReserved: Pointer): THandle; stdcall;
{$EXTERNALSYM FindFirstUrlCacheEntryEx}
function FindFirstUrlCacheEntryEx(lpszUrlSearchPattern: PTSTR; dwFlags,
  dwFilter: DWORD; GroupId: TGroupID; lpFirstCacheEntryInfo:
  PInternetCacheEntryInfo; var lpdwFirstCacheEntryInfoBufferSize: DWORD;
  lpGroupAttributes: Pointer; pcbGroupAttributes: LPDWORD;
  lpReserved: Pointer): THandle; stdcall;

{$EXTERNALSYM FindNextUrlCacheEntryExA}
function FindNextUrlCacheEntryExA(hEnumHandle: THandle; lpFirstCacheEntryInfo:
  PInternetCacheEntryInfoA; var lpdwFirstCacheEntryInfoBufferSize: DWORD;
  lpGroupAttributes: Pointer; pcbGroupAttributes: LPDWORD;
  lpReserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM FindNextUrlCacheEntryExW}
function FindNextUrlCacheEntryExW(hEnumHandle: THandle; lpFirstCacheEntryInfo:
  PInternetCacheEntryInfoW; var lpdwFirstCacheEntryInfoBufferSize: DWORD;
  lpGroupAttributes: Pointer; pcbGroupAttributes: LPDWORD;
  lpReserved: Pointer): BOOL; stdcall;
{$EXTERNALSYM FindNextUrlCacheEntryEx}
function FindNextUrlCacheEntryEx(hEnumHandle: THandle; lpFirstCacheEntryInfo:
  PInternetCacheEntryInfo; var lpdwFirstCacheEntryInfoBufferSize: DWORD;
  lpGroupAttributes: Pointer; pcbGroupAttributes: LPDWORD;
  lpReserved: Pointer): BOOL; stdcall;

{$EXTERNALSYM FindFirstUrlCacheEntryA}
function FindFirstUrlCacheEntryA(lpszUrlSearchPattern: PAnsiChar;
  lpFirstCacheEntryInfo: PInternetCacheEntryInfoA;
  var lpdwFirstCacheEntryInfoBufferSize: DWORD): THandle; stdcall;
{$EXTERNALSYM FindFirstUrlCacheEntryW}
function FindFirstUrlCacheEntryW(lpszUrlSearchPattern: PWideChar;
  lpFirstCacheEntryInfo: PInternetCacheEntryInfoW;
  var lpdwFirstCacheEntryInfoBufferSize: DWORD): THandle; stdcall;
{$EXTERNALSYM FindFirstUrlCacheEntry}
function FindFirstUrlCacheEntry(lpszUrlSearchPattern: PTSTR;
  lpFirstCacheEntryInfo: PInternetCacheEntryInfo;
  var lpdwFirstCacheEntryInfoBufferSize: DWORD): THandle; stdcall;

{$EXTERNALSYM FindNextUrlCacheEntryA}
function FindNextUrlCacheEntryA(hEnumHandle: THandle; lpNextCacheEntryInfo:
  PInternetCacheEntryInfoA; var lpdwNextCacheEntryInfoBufferSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM FindNextUrlCacheEntryW}
function FindNextUrlCacheEntryW(hEnumHandle: THandle; lpNextCacheEntryInfo:
  PInternetCacheEntryInfoW; var lpdwNextCacheEntryInfoBufferSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM FindNextUrlCacheEntry}
function FindNextUrlCacheEntry(hEnumHandle: THandle; lpNextCacheEntryInfo:
  PInternetCacheEntryInfo; var lpdwNextCacheEntryInfoBufferSize: DWORD): BOOL; stdcall;

{$EXTERNALSYM FindCloseUrlCache}
function FindCloseUrlCache(hEnumHandle: THandle): BOOL; stdcall;

{$EXTERNALSYM DeleteUrlCacheEntry}
function DeleteUrlCacheEntryA(lpszUrlName: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM DeleteUrlCacheEntry}
function DeleteUrlCacheEntryW(lpszUrlName: PWideChar): BOOL; stdcall;
{$EXTERNALSYM DeleteUrlCacheEntry}
function DeleteUrlCacheEntry(lpszUrlName: PTSTR): BOOL; stdcall;

//
// Autodial APIs
//

{$EXTERNALSYM InternetDialA}
function InternetDialA(hwndParent: HWND; lpszConnectoid: PAnsiChar; dwFlags: DWORD;
  out lpdwConnection: DWORD_PTR; dwReserved: DWORD): DWORD; stdcall;
{$EXTERNALSYM InternetDialW}
function InternetDialW(hwndParent: HWND; lpszConnectoid: PWideChar; dwFlags: DWORD;
  out lpdwConnection: DWORD_PTR; dwReserved: DWORD): DWORD; stdcall;
{$EXTERNALSYM InternetDial}
function InternetDial(hwndParent: HWND; lpszConnectoid: PTSTR; dwFlags: DWORD;
  out lpdwConnection: DWORD_PTR; dwReserved: DWORD): DWORD; stdcall;

// Flags for InternetDial - must not conflict with InternetAutodial flags
//                          as they are valid here also.
const
  {$EXTERNALSYM INTERNET_DIAL_FORCE_PROMPT}
  INTERNET_DIAL_FORCE_PROMPT = $2000;
  {$EXTERNALSYM INTERNET_DIAL_SHOW_OFFLINE}
  INTERNET_DIAL_SHOW_OFFLINE = $4000;
  {$EXTERNALSYM INTERNET_DIAL_UNATTENDED}
  INTERNET_DIAL_UNATTENDED   = $8000;

{$EXTERNALSYM InternetHangUp}
function InternetHangUp(dwConnection, dwReserved: DWORD): DWORD; stdcall;

const
  {$EXTERNALSYM INTERENT_GOONLINE_REFRESH}
  INTERENT_GOONLINE_REFRESH = $00000001;
  {$EXTERNALSYM INTERENT_GOONLINE_MASK}
  INTERENT_GOONLINE_MASK    = $00000001;


{$EXTERNALSYM InternetGoOnlineA}
function InternetGoOnlineA(lpszURL: PAnsiChar; hwndParent: HWND; dwFlag: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetGoOnlineW}
function InternetGoOnlineW(lpszURL: PWideChar; hwndParent: HWND; dwFlag: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetGoOnline}
function InternetGoOnline(lpszURL: PTSTR; hwndParent: HWND; dwFlag: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetAutodial}
function InternetAutodial(dwFlags, dwReserved: DWORD): BOOL; stdcall;

// Flags for InternetAutodial
const
  {$EXTERNALSYM INTERNET_AUTODIAL_FORCE_ONLINE}
  INTERNET_AUTODIAL_FORCE_ONLINE          = 1;
  {$EXTERNALSYM INTERNET_AUTODIAL_FORCE_UNATTENDED}
  INTERNET_AUTODIAL_FORCE_UNATTENDED      = 2;
  {$EXTERNALSYM INTERNET_AUTODIAL_FAILIFSECURITYCHECK}
  INTERNET_AUTODIAL_FAILIFSECURITYCHECK   = 4;
  {$EXTERNALSYM INTERNET_AUTODIAL_FLAGS_MASK}
  INTERNET_AUTODIAL_FLAGS_MASK            = INTERNET_AUTODIAL_FORCE_ONLINE or
                                            INTERNET_AUTODIAL_FORCE_UNATTENDED or
                                            INTERNET_AUTODIAL_FAILIFSECURITYCHECK;

{$EXTERNALSYM InternetAutodialHangup}
function InternetAutodialHangup(dwReserved: DWORD): DWORD; stdcall;
{$EXTERNALSYM InternetGetConnectedState}
function InternetGetConnectedState(var lpdwFlags: DWORD; dwReserved: DWORD): BOOL; stdcall;

{$EXTERNALSYM InternetGetConnectedStateExA}
function InternetGetConnectedStateExA(out lpdwFlags: DWORD;
  lpszConnectionName: PAnsiChar; dwNameLen, dwReserved: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetGetConnectedStateExW}
function InternetGetConnectedStateExW(out lpdwFlags: DWORD;
  lpszConnectionName: PWideChar; dwNameLen, dwReserved: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetGetConnectedStateEx}
function InternetGetConnectedStateEx(out lpdwFlags: DWORD;
  lpszConnectionName: PTSTR; dwNameLen, dwReserved: DWORD): BOOL; stdcall;

{$EXTERNALSYM InternetInitializeAutoProxyDll}
function InternetInitializeAutoProxyDll(dwReserved: DWORD): BOOL; stdcall;


// Flags for InternetGetConnectedState and Ex
const
  {$EXTERNALSYM INTERNET_CONNECTION_MODEM}
  INTERNET_CONNECTION_MODEM           = $01;
  {$EXTERNALSYM INTERNET_CONNECTION_LAN}
  INTERNET_CONNECTION_LAN             = $02;
  {$EXTERNALSYM INTERNET_CONNECTION_PROXY}
  INTERNET_CONNECTION_PROXY           = $04;
  {$EXTERNALSYM INTERNET_CONNECTION_MODEM_BUSY}
  INTERNET_CONNECTION_MODEM_BUSY      = $08; // no longer used
  {$EXTERNALSYM INTERNET_RAS_INSTALLED}
  INTERNET_RAS_INSTALLED              = $10;
  {$EXTERNALSYM INTERNET_CONNECTION_OFFLINE}
  INTERNET_CONNECTION_OFFLINE         = $20;
  {$EXTERNALSYM INTERNET_CONNECTION_CONFIGURED}
  INTERNET_CONNECTION_CONFIGURED      = $40;

//
// Custom dial handler functions
//

// Custom dial handler prototype
type
  {$EXTERNALSYM PFN_DIAL_HANDLER}
  PFN_DIAL_HANDLER = function(p1: HWND; p2: LPCSTR; p3: DWORD;
                       var p4: DWORD): DWORD stdcall;
  PFNDialHandler = PFN_DIAL_HANDLER;

// Flags for custom dial handler
const
  {$EXTERNALSYM INTERNET_CUSTOMDIAL_CONNECT}
  INTERNET_CUSTOMDIAL_CONNECT         = 0;
  {$EXTERNALSYM INTERNET_CUSTOMDIAL_UNATTENDED}
  INTERNET_CUSTOMDIAL_UNATTENDED      = 1;
  {$EXTERNALSYM INTERNET_CUSTOMDIAL_DISCONNECT}
  INTERNET_CUSTOMDIAL_DISCONNECT      = 2;
  {$EXTERNALSYM INTERNET_CUSTOMDIAL_SHOWOFFLINE}
  INTERNET_CUSTOMDIAL_SHOWOFFLINE     = 4;

  // Custom dial handler supported functionality flags
  {$EXTERNALSYM INTERNET_CUSTOMDIAL_SAFE_FOR_UNATTENDED}
  INTERNET_CUSTOMDIAL_SAFE_FOR_UNATTENDED = 1;
  {$EXTERNALSYM INTERNET_CUSTOMDIAL_WILL_SUPPLY_STATE}
  INTERNET_CUSTOMDIAL_WILL_SUPPLY_STATE   = 2;
  {$EXTERNALSYM INTERNET_CUSTOMDIAL_CAN_HANGUP}
  INTERNET_CUSTOMDIAL_CAN_HANGUP          = 4;

{$EXTERNALSYM InternetSetDialStateA}
function InternetSetDialStateA(lpszConnectoid: PAnsiChar;
  dwState, dwReserved: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetSetDialStateW}
function InternetSetDialStateW(lpszConnectoid: PWideChar;
  dwState, dwReserved: DWORD): BOOL; stdcall;
{$EXTERNALSYM InternetSetDialState}
function InternetSetDialState(lpszConnectoid: PTSTR;
  dwState, dwReserved: DWORD): BOOL; stdcall;

const
// States for InternetSetDialState
  {$EXTERNALSYM INTERNET_DIALSTATE_DISCONNECTED}
  INTERNET_DIALSTATE_DISCONNECTED     = 1;

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation

{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  winetdll = 'wininet.dll';
{$ENDIF JWA_INCLUDEMODE}

function InternetConnectUrlA(hInternet: HINTERNET; lpszUrl: PAnsiChar;
  dwFlags, dwContext: DWORD): HINTERNET;
begin
  Result := InternetConnectA(hInternet, lpszUrl, INTERNET_INVALID_PORT_NUMBER,
    nil, nil, INTERNET_SERVICE_URL, dwFlags, dwContext);
end;
function InternetConnectUrlW(hInternet: HINTERNET; lpszUrl: PWideChar;
  dwFlags, dwContext: DWORD): HINTERNET;
begin
  Result := InternetConnectW(hInternet, lpszUrl, INTERNET_INVALID_PORT_NUMBER,
    nil, nil, INTERNET_SERVICE_URL, dwFlags, dwContext);
end;
function InternetConnectUrl(hInternet: HINTERNET; lpszUrl: PTSTR;
  dwFlags, dwContext: DWORD): HINTERNET;
begin
  Result := InternetConnect(hInternet, lpszUrl, INTERNET_INVALID_PORT_NUMBER,
    nil, nil, INTERNET_SERVICE_URL, dwFlags, dwContext);
end;

function IS_GOPHER_FILE(AType: DWORD): BOOL;
begin
  Result := AType and GOPHER_TYPE_FILE_MASK <> 0;
end;

function IS_GOPHER_DIRECTORY(AType: DWORD): BOOL;
begin
  Result := AType and GOPHER_TYPE_DIRECTORY <> 0;
end;

function IS_GOPHER_PHONE_SERVER(AType: DWORD): BOOL;
begin
  Result := AType and GOPHER_TYPE_CSO <> 0;
end;

function IS_GOPHER_ERROR(AType: DWORD): BOOL;
begin
  Result := AType and GOPHER_TYPE_ERROR <> 0;
end;

function IS_GOPHER_INDEX_SERVER(AType: DWORD): BOOL;
begin
  Result := AType and GOPHER_TYPE_INDEX_SERVER <> 0;
end;

function IS_GOPHER_TELNET_SESSION(AType: DWORD): BOOL;
begin
  Result := AType and GOPHER_TYPE_TELNET <> 0;
end;

function IS_GOPHER_BACKUP_SERVER(AType: DWORD): BOOL;
begin
  Result := AType and GOPHER_TYPE_REDUNDANT <> 0;
end;

function IS_GOPHER_TN3270_SESSION(AType: DWORD): BOOL;
begin
  Result := AType and GOPHER_TYPE_TN3270 <> 0;
end;

function IS_GOPHER_ASK(AType: DWORD): BOOL;
begin
  Result := AType and GOPHER_TYPE_ASK <> 0;
end;

function IS_GOPHER_PLUS(AType: DWORD): BOOL;
begin
  Result := AType and GOPHER_TYPE_GOPHER_PLUS <> 0;
end;

function IS_GOPHER_TYPE_KNOWN(AType: DWORD): BOOL;
begin
  Result := AType and GOPHER_TYPE_UNKNOWN = 0;
end;


{$IFDEF DYNAMIC_LINK}

var
  _InternetConnectA: Pointer;

function InternetConnectA;
begin
  GetProcedureAddress(_InternetConnectA, winetdll, 'InternetConnectA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetConnectA]
  end;
end;

var
  _InternetConnectW: Pointer;

function InternetConnectW;
begin
  GetProcedureAddress(_InternetConnectW, winetdll, 'InternetConnectW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetConnectW]
  end;
end;

var
  _InternetConnect: Pointer;

function InternetConnect;
begin
  GetProcedureAddress(_InternetConnect, winetdll, 'InternetConnect');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetConnect]
  end;
end;

var
  _InternetCloseHandle: Pointer;

function InternetCloseHandle;
begin
  GetProcedureAddress(_InternetCloseHandle, winetdll, 'InternetCloseHandle');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetCloseHandle]
  end;
end;

var
  _InternetTimeFromSystemTimeA: Pointer;

function InternetTimeFromSystemTimeA;
begin
  GetProcedureAddress(_InternetTimeFromSystemTimeA, winetdll, 'InternetTimeFromSystemTimeA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetTimeFromSystemTimeA]
  end;
end;

var
  _InternetTimeToSystemTimeA: Pointer;

function InternetTimeToSystemTimeA;
begin
  GetProcedureAddress(_InternetTimeToSystemTimeA, winetdll, 'InternetTimeToSystemTimeA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetTimeToSystemTimeA]
  end;
end;

var
  _InternetCrackUrlA: Pointer;

function InternetCrackUrlA;
begin
  GetProcedureAddress(_InternetCrackUrlA, winetdll, 'InternetCrackUrlA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetCrackUrlA]
  end;
end;

var
  _InternetCreateUrlA: Pointer;

function InternetCreateUrlA;
begin
  GetProcedureAddress(_InternetCreateUrlA, winetdll, 'InternetCreateUrlA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetCreateUrlA]
  end;
end;

var
  _InternetCanonicalizeUrlA: Pointer;

function InternetCanonicalizeUrlA;
begin
  GetProcedureAddress(_InternetCanonicalizeUrlA, winetdll, 'InternetCanonicalizeUrlA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetCanonicalizeUrlA]
  end;
end;

var
  _InternetCombineUrlA: Pointer;

function InternetCombineUrlA;
begin
  GetProcedureAddress(_InternetCombineUrlA, winetdll, 'InternetCombineUrlA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetCombineUrlA]
  end;
end;

var
  _InternetOpenUrlA: Pointer;

function InternetOpenUrlA;
begin
  GetProcedureAddress(_InternetOpenUrlA, winetdll, 'InternetOpenUrlA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetOpenUrlA]
  end;
end;

var
  _InternetTimeFromSystemTimeW: Pointer;

function InternetTimeFromSystemTimeW;
begin
  GetProcedureAddress(_InternetTimeFromSystemTimeW, winetdll, 'InternetTimeFromSystemTimeW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetTimeFromSystemTimeW]
  end;
end;

var
  _InternetTimeToSystemTimeW: Pointer;

function InternetTimeToSystemTimeW;
begin
  GetProcedureAddress(_InternetTimeToSystemTimeW, winetdll, 'InternetTimeToSystemTimeW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetTimeToSystemTimeW]
  end;
end;

var
  _InternetCrackUrlW: Pointer;

function InternetCrackUrlW;
begin
  GetProcedureAddress(_InternetCrackUrlW, winetdll, 'InternetCrackUrlW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetCrackUrlW]
  end;
end;

var
  _InternetCreateUrlW: Pointer;

function InternetCreateUrlW;
begin
  GetProcedureAddress(_InternetCreateUrlW, winetdll, 'InternetCreateUrlW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetCreateUrlW]
  end;
end;

var
  _InternetCanonicalizeUrlW: Pointer;

function InternetCanonicalizeUrlW;
begin
  GetProcedureAddress(_InternetCanonicalizeUrlW, winetdll, 'InternetCanonicalizeUrlW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetCanonicalizeUrlW]
  end;
end;

var
  _InternetCombineUrlW: Pointer;

function InternetCombineUrlW;
begin
  GetProcedureAddress(_InternetCombineUrlW, winetdll, 'InternetCombineUrlW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetCombineUrlW]
  end;
end;

var
  _InternetOpenUrlW: Pointer;

function InternetOpenUrlW;
begin
  GetProcedureAddress(_InternetOpenUrlW, winetdll, 'InternetOpenUrlW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetOpenUrlW]
  end;
end;

var
  _InternetTimeFromSystemTime: Pointer;

function InternetTimeFromSystemTime;
begin
  GetProcedureAddress(_InternetTimeFromSystemTime, winetdll, 'InternetTimeFromSystemTime');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetTimeFromSystemTime]
  end;
end;

var
  _InternetTimeToSystemTime: Pointer;

function InternetTimeToSystemTime;
begin
  GetProcedureAddress(_InternetTimeToSystemTime, winetdll, 'InternetTimeToSystemTime');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetTimeToSystemTime]
  end;
end;

var
  _InternetCrackUrl: Pointer;

function InternetCrackUrl;
begin
  GetProcedureAddress(_InternetCrackUrl, winetdll, 'InternetCrackUrl');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetCrackUrl]
  end;
end;

var
  _InternetCreateUrl: Pointer;

function InternetCreateUrl;
begin
  GetProcedureAddress(_InternetCreateUrl, winetdll, 'InternetCreateUrl');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetCreateUrl]
  end;
end;

var
  _InternetCanonicalizeUrl: Pointer;

function InternetCanonicalizeUrl;
begin
  GetProcedureAddress(_InternetCanonicalizeUrl, winetdll, 'InternetCanonicalizeUrl');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetCanonicalizeUrl]
  end;
end;

var
  _InternetCombineUrl: Pointer;

function InternetCombineUrl;
begin
  GetProcedureAddress(_InternetCombineUrl, winetdll, 'InternetCombineUrl');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetCombineUrl]
  end;
end;

var
  _InternetOpenUrl: Pointer;

function InternetOpenUrl;
begin
  GetProcedureAddress(_InternetOpenUrl, winetdll, 'InternetOpenUrl');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetOpenUrl]
  end;
end;

var
  _InternetReadFile: Pointer;

function InternetReadFile;
begin
  GetProcedureAddress(_InternetReadFile, winetdll, 'InternetReadFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetReadFile]
  end;
end;

var
  _InternetReadFileExA: Pointer;

function InternetReadFileExA;
begin
  GetProcedureAddress(_InternetReadFileExA, winetdll, 'InternetReadFileExA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetReadFileExA]
  end;
end;

var
  _InternetReadFileExW: Pointer;

function InternetReadFileExW;
begin
  GetProcedureAddress(_InternetReadFileExW, winetdll, 'InternetReadFileExW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetReadFileExW]
  end;
end;

var
  _InternetReadFileEx: Pointer;

function InternetReadFileEx;
begin
  GetProcedureAddress(_InternetReadFileEx, winetdll, 'InternetReadFileEx');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetReadFileEx]
  end;
end;

var
  _InternetSetFilePointer: Pointer;

function InternetSetFilePointer;
begin
  GetProcedureAddress(_InternetSetFilePointer, winetdll, 'InternetSetFilePointer');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetSetFilePointer]
  end;
end;

var
  _InternetWriteFile: Pointer;

function InternetWriteFile;
begin
  GetProcedureAddress(_InternetWriteFile, winetdll, 'InternetWriteFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetWriteFile]
  end;
end;

var
  _InternetQueryDataAvailable: Pointer;

function InternetQueryDataAvailable;
begin
  GetProcedureAddress(_InternetQueryDataAvailable, winetdll, 'InternetQueryDataAvailable');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetQueryDataAvailable]
  end;
end;

var
  _InternetFindNextFileA: Pointer;

function InternetFindNextFileA;
begin
  GetProcedureAddress(_InternetFindNextFileA, winetdll, 'InternetFindNextFileA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetFindNextFileA]
  end;
end;

var
  _InternetQueryOptionA: Pointer;

function InternetQueryOptionA;
begin
  GetProcedureAddress(_InternetQueryOptionA, winetdll, 'InternetQueryOptionA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetQueryOptionA]
  end;
end;

var
  _InternetSetOptionA: Pointer;

function InternetSetOptionA;
begin
  GetProcedureAddress(_InternetSetOptionA, winetdll, 'InternetSetOptionA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetSetOptionA]
  end;
end;

var
  _InternetSetOptionExA: Pointer;

function InternetSetOptionExA;
begin
  GetProcedureAddress(_InternetSetOptionExA, winetdll, 'InternetSetOptionExA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetSetOptionExA]
  end;
end;

var
  _InternetFindNextFileW: Pointer;

function InternetFindNextFileW;
begin
  GetProcedureAddress(_InternetFindNextFileW, winetdll, 'InternetFindNextFileW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetFindNextFileW]
  end;
end;

var
  _InternetQueryOptionW: Pointer;

function InternetQueryOptionW;
begin
  GetProcedureAddress(_InternetQueryOptionW, winetdll, 'InternetQueryOptionW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetQueryOptionW]
  end;
end;

var
  _InternetSetOptionW: Pointer;

function InternetSetOptionW;
begin
  GetProcedureAddress(_InternetSetOptionW, winetdll, 'InternetSetOptionW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetSetOptionW]
  end;
end;

var
  _InternetSetOptionExW: Pointer;

function InternetSetOptionExW;
begin
  GetProcedureAddress(_InternetSetOptionExW, winetdll, 'InternetSetOptionExW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetSetOptionExW]
  end;
end;

var
  _InternetFindNextFile: Pointer;

function InternetFindNextFile;
begin
  GetProcedureAddress(_InternetFindNextFile, winetdll, 'InternetFindNextFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetFindNextFile]
  end;
end;

var
  _InternetQueryOption: Pointer;

function InternetQueryOption;
begin
  GetProcedureAddress(_InternetQueryOption, winetdll, 'InternetQueryOption');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetQueryOption]
  end;
end;

var
  _InternetSetOption: Pointer;

function InternetSetOption;
begin
  GetProcedureAddress(_InternetSetOption, winetdll, 'InternetSetOption');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetSetOption]
  end;
end;

var
  _InternetSetOptionEx: Pointer;

function InternetSetOptionEx;
begin
  GetProcedureAddress(_InternetSetOptionEx, winetdll, 'InternetSetOptionEx');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetSetOptionEx]
  end;
end;

var
  _InternetLockRequestFile: Pointer;

function InternetLockRequestFile;
begin
  GetProcedureAddress(_InternetLockRequestFile, winetdll, 'InternetLockRequestFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetLockRequestFile]
  end;
end;

var
  _InternetUnlockRequestFile: Pointer;

function InternetUnlockRequestFile;
begin
  GetProcedureAddress(_InternetUnlockRequestFile, winetdll, 'InternetUnlockRequestFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetUnlockRequestFile]
  end;
end;

var
  _InternetGetLastResponseInfoA: Pointer;

function InternetGetLastResponseInfoA;
begin
  GetProcedureAddress(_InternetGetLastResponseInfoA, winetdll, 'InternetGetLastResponseInfoA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetGetLastResponseInfoA]
  end;
end;

var
  _InternetSetStatusCallbackA: Pointer;

function InternetSetStatusCallbackA;
begin
  GetProcedureAddress(_InternetSetStatusCallbackA, winetdll, 'InternetSetStatusCallbackA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetSetStatusCallbackA]
  end;
end;

var
  _FtpFindFirstFileA: Pointer;

function FtpFindFirstFileA;
begin
  GetProcedureAddress(_FtpFindFirstFileA, winetdll, 'FtpFindFirstFileA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpFindFirstFileA]
  end;
end;

var
  _FtpGetFileA: Pointer;

function FtpGetFileA;
begin
  GetProcedureAddress(_FtpGetFileA, winetdll, 'FtpGetFileA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpGetFileA]
  end;
end;

var
  _FtpPutFileA: Pointer;

function FtpPutFileA;
begin
  GetProcedureAddress(_FtpPutFileA, winetdll, 'FtpPutFileA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpPutFileA]
  end;
end;

var
  _InternetGetLastResponseInfoW: Pointer;

function InternetGetLastResponseInfoW;
begin
  GetProcedureAddress(_InternetGetLastResponseInfoW, winetdll, 'InternetGetLastResponseInfoW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetGetLastResponseInfoW]
  end;
end;

var
  _InternetSetStatusCallbackW: Pointer;

function InternetSetStatusCallbackW;
begin
  GetProcedureAddress(_InternetSetStatusCallbackW, winetdll, 'InternetSetStatusCallbackW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetSetStatusCallbackW]
  end;
end;

var
  _FtpFindFirstFileW: Pointer;

function FtpFindFirstFileW;
begin
  GetProcedureAddress(_FtpFindFirstFileW, winetdll, 'FtpFindFirstFileW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpFindFirstFileW]
  end;
end;

var
  _FtpGetFileW: Pointer;

function FtpGetFileW;
begin
  GetProcedureAddress(_FtpGetFileW, winetdll, 'FtpGetFileW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpGetFileW]
  end;
end;

var
  _FtpPutFileW: Pointer;

function FtpPutFileW;
begin
  GetProcedureAddress(_FtpPutFileW, winetdll, 'FtpPutFileW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpPutFileW]
  end;
end;

var
  _InternetGetLastResponseInfo: Pointer;

function InternetGetLastResponseInfo;
begin
  GetProcedureAddress(_InternetGetLastResponseInfo, winetdll, 'InternetGetLastResponseInfo');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetGetLastResponseInfo]
  end;
end;

var
  _InternetSetStatusCallback: Pointer;

function InternetSetStatusCallback;
begin
  GetProcedureAddress(_InternetSetStatusCallback, winetdll, 'InternetSetStatusCallback');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetSetStatusCallback]
  end;
end;

var
  _FtpFindFirstFile: Pointer;

function FtpFindFirstFile;
begin
  GetProcedureAddress(_FtpFindFirstFile, winetdll, 'FtpFindFirstFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpFindFirstFile]
  end;
end;

var
  _FtpGetFile: Pointer;

function FtpGetFile;
begin
  GetProcedureAddress(_FtpGetFile, winetdll, 'FtpGetFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpGetFile]
  end;
end;

var
  _FtpPutFile: Pointer;

function FtpPutFile;
begin
  GetProcedureAddress(_FtpPutFile, winetdll, 'FtpPutFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpPutFile]
  end;
end;

var
  _FtpGetFileEx: Pointer;

function FtpGetFileEx;
begin
  GetProcedureAddress(_FtpGetFileEx, winetdll, 'FtpGetFileEx');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpGetFileEx]
  end;
end;

var
  _FtpPutFileEx: Pointer;

function FtpPutFileEx;
begin
  GetProcedureAddress(_FtpPutFileEx, winetdll, 'FtpPutFileEx');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpPutFileEx]
  end;
end;

var
  _FtpDeleteFileA: Pointer;

function FtpDeleteFileA;
begin
  GetProcedureAddress(_FtpDeleteFileA, winetdll, 'FtpDeleteFileA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpDeleteFileA]
  end;
end;

var
  _FtpRenameFileA: Pointer;

function FtpRenameFileA;
begin
  GetProcedureAddress(_FtpRenameFileA, winetdll, 'FtpRenameFileA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpRenameFileA]
  end;
end;

var
  _FtpOpenFileA: Pointer;

function FtpOpenFileA;
begin
  GetProcedureAddress(_FtpOpenFileA, winetdll, 'FtpOpenFileA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpOpenFileA]
  end;
end;

var
  _FtpCreateDirectoryA: Pointer;

function FtpCreateDirectoryA;
begin
  GetProcedureAddress(_FtpCreateDirectoryA, winetdll, 'FtpCreateDirectoryA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpCreateDirectoryA]
  end;
end;

var
  _FtpRemoveDirectoryA: Pointer;

function FtpRemoveDirectoryA;
begin
  GetProcedureAddress(_FtpRemoveDirectoryA, winetdll, 'FtpRemoveDirectoryA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpRemoveDirectoryA]
  end;
end;

var
  _FtpSetCurrentDirectoryA: Pointer;

function FtpSetCurrentDirectoryA;
begin
  GetProcedureAddress(_FtpSetCurrentDirectoryA, winetdll, 'FtpSetCurrentDirectoryA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpSetCurrentDirectoryA]
  end;
end;

var
  _FtpGetCurrentDirectoryA: Pointer;

function FtpGetCurrentDirectoryA;
begin
  GetProcedureAddress(_FtpGetCurrentDirectoryA, winetdll, 'FtpGetCurrentDirectoryA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpGetCurrentDirectoryA]
  end;
end;

var
  _FtpCommandA: Pointer;

function FtpCommandA;
begin
  GetProcedureAddress(_FtpCommandA, winetdll, 'FtpCommandA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpCommandA]
  end;
end;

var
  _FtpDeleteFileW: Pointer;

function FtpDeleteFileW;
begin
  GetProcedureAddress(_FtpDeleteFileW, winetdll, 'FtpDeleteFileW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpDeleteFileW]
  end;
end;

var
  _FtpRenameFileW: Pointer;

function FtpRenameFileW;
begin
  GetProcedureAddress(_FtpRenameFileW, winetdll, 'FtpRenameFileW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpRenameFileW]
  end;
end;

var
  _FtpOpenFileW: Pointer;

function FtpOpenFileW;
begin
  GetProcedureAddress(_FtpOpenFileW, winetdll, 'FtpOpenFileW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpOpenFileW]
  end;
end;

var
  _FtpCreateDirectoryW: Pointer;

function FtpCreateDirectoryW;
begin
  GetProcedureAddress(_FtpCreateDirectoryW, winetdll, 'FtpCreateDirectoryW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpCreateDirectoryW]
  end;
end;

var
  _FtpRemoveDirectoryW: Pointer;

function FtpRemoveDirectoryW;
begin
  GetProcedureAddress(_FtpRemoveDirectoryW, winetdll, 'FtpRemoveDirectoryW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpRemoveDirectoryW]
  end;
end;

var
  _FtpSetCurrentDirectoryW: Pointer;

function FtpSetCurrentDirectoryW;
begin
  GetProcedureAddress(_FtpSetCurrentDirectoryW, winetdll, 'FtpSetCurrentDirectoryW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpSetCurrentDirectoryW]
  end;
end;

var
  _FtpGetCurrentDirectoryW: Pointer;

function FtpGetCurrentDirectoryW;
begin
  GetProcedureAddress(_FtpGetCurrentDirectoryW, winetdll, 'FtpGetCurrentDirectoryW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpGetCurrentDirectoryW]
  end;
end;

var
  _FtpCommandW: Pointer;

function FtpCommandW;
begin
  GetProcedureAddress(_FtpCommandW, winetdll, 'FtpCommandW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpCommandW]
  end;
end;

var
  _FtpDeleteFile: Pointer;

function FtpDeleteFile;
begin
  GetProcedureAddress(_FtpDeleteFile, winetdll, 'FtpDeleteFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpDeleteFile]
  end;
end;

var
  _FtpRenameFile: Pointer;

function FtpRenameFile;
begin
  GetProcedureAddress(_FtpRenameFile, winetdll, 'FtpRenameFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpRenameFile]
  end;
end;

var
  _FtpOpenFile: Pointer;

function FtpOpenFile;
begin
  GetProcedureAddress(_FtpOpenFile, winetdll, 'FtpOpenFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpOpenFile]
  end;
end;

var
  _FtpCreateDirectory: Pointer;

function FtpCreateDirectory;
begin
  GetProcedureAddress(_FtpCreateDirectory, winetdll, 'FtpCreateDirectory');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpCreateDirectory]
  end;
end;

var
  _FtpRemoveDirectory: Pointer;

function FtpRemoveDirectory;
begin
  GetProcedureAddress(_FtpRemoveDirectory, winetdll, 'FtpRemoveDirectory');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpRemoveDirectory]
  end;
end;

var
  _FtpSetCurrentDirectory: Pointer;

function FtpSetCurrentDirectory;
begin
  GetProcedureAddress(_FtpSetCurrentDirectory, winetdll, 'FtpSetCurrentDirectory');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpSetCurrentDirectory]
  end;
end;

var
  _FtpGetCurrentDirectory: Pointer;

function FtpGetCurrentDirectory;
begin
  GetProcedureAddress(_FtpGetCurrentDirectory, winetdll, 'FtpGetCurrentDirectory');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpGetCurrentDirectory]
  end;
end;

var
  _FtpCommand: Pointer;

function FtpCommand;
begin
  GetProcedureAddress(_FtpCommand, winetdll, 'FtpCommand');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpCommand]
  end;
end;

var
  _FtpGetFileSize: Pointer;

function FtpGetFileSize;
begin
  GetProcedureAddress(_FtpGetFileSize, winetdll, 'FtpGetFileSize');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtpGetFileSize]
  end;
end;

var
  _GopherCreateLocatorA: Pointer;

function GopherCreateLocatorA;
begin
  GetProcedureAddress(_GopherCreateLocatorA, winetdll, 'GopherCreateLocatorA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GopherCreateLocatorA]
  end;
end;

var
  _GopherGetLocatorTypeA: Pointer;

function GopherGetLocatorTypeA;
begin
  GetProcedureAddress(_GopherGetLocatorTypeA, winetdll, 'GopherGetLocatorTypeA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GopherGetLocatorTypeA]
  end;
end;

var
  _GopherFindFirstFileA: Pointer;

function GopherFindFirstFileA;
begin
  GetProcedureAddress(_GopherFindFirstFileA, winetdll, 'GopherFindFirstFileA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GopherFindFirstFileA]
  end;
end;

var
  _GopherOpenFileA: Pointer;

function GopherOpenFileA;
begin
  GetProcedureAddress(_GopherOpenFileA, winetdll, 'GopherOpenFileA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GopherOpenFileA]
  end;
end;

var
  _GopherGetAttributeA: Pointer;

function GopherGetAttributeA;
begin
  GetProcedureAddress(_GopherGetAttributeA, winetdll, 'GopherGetAttributeA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GopherGetAttributeA]
  end;
end;

var
  _HttpOpenRequestA: Pointer;

function HttpOpenRequestA;
begin
  GetProcedureAddress(_HttpOpenRequestA, winetdll, 'HttpOpenRequestA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HttpOpenRequestA]
  end;
end;

var
  _HttpAddRequestHeadersA: Pointer;

function HttpAddRequestHeadersA;
begin
  GetProcedureAddress(_HttpAddRequestHeadersA, winetdll, 'HttpAddRequestHeadersA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HttpAddRequestHeadersA]
  end;
end;

var
  _HttpSendRequestA: Pointer;

function HttpSendRequestA;
begin
  GetProcedureAddress(_HttpSendRequestA, winetdll, 'HttpSendRequestA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HttpSendRequestA]
  end;
end;

var
  _HttpSendRequestExA: Pointer;

function HttpSendRequestExA;
begin
  GetProcedureAddress(_HttpSendRequestExA, winetdll, 'HttpSendRequestExA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HttpSendRequestExA]
  end;
end;

var
  _HttpEndRequestA: Pointer;

function HttpEndRequestA;
begin
  GetProcedureAddress(_HttpEndRequestA, winetdll, 'HttpEndRequestA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HttpEndRequestA]
  end;
end;

var
  _HttpQueryInfoA: Pointer;

function HttpQueryInfoA;
begin
  GetProcedureAddress(_HttpQueryInfoA, winetdll, 'HttpQueryInfoA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HttpQueryInfoA]
  end;
end;

var
  _InternetSetCookieA: Pointer;

function InternetSetCookieA;
begin
  GetProcedureAddress(_InternetSetCookieA, winetdll, 'InternetSetCookieA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetSetCookieA]
  end;
end;

var
  _InternetGetCookieA: Pointer;

function InternetGetCookieA;
begin
  GetProcedureAddress(_InternetGetCookieA, winetdll, 'InternetGetCookieA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetGetCookieA]
  end;
end;

var
  _GopherCreateLocatorW: Pointer;

function GopherCreateLocatorW;
begin
  GetProcedureAddress(_GopherCreateLocatorW, winetdll, 'GopherCreateLocatorW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GopherCreateLocatorW]
  end;
end;

var
  _GopherGetLocatorTypeW: Pointer;

function GopherGetLocatorTypeW;
begin
  GetProcedureAddress(_GopherGetLocatorTypeW, winetdll, 'GopherGetLocatorTypeW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GopherGetLocatorTypeW]
  end;
end;

var
  _GopherFindFirstFileW: Pointer;

function GopherFindFirstFileW;
begin
  GetProcedureAddress(_GopherFindFirstFileW, winetdll, 'GopherFindFirstFileW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GopherFindFirstFileW]
  end;
end;

var
  _GopherOpenFileW: Pointer;

function GopherOpenFileW;
begin
  GetProcedureAddress(_GopherOpenFileW, winetdll, 'GopherOpenFileW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GopherOpenFileW]
  end;
end;

var
  _GopherGetAttributeW: Pointer;

function GopherGetAttributeW;
begin
  GetProcedureAddress(_GopherGetAttributeW, winetdll, 'GopherGetAttributeW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GopherGetAttributeW]
  end;
end;

var
  _HttpOpenRequestW: Pointer;

function HttpOpenRequestW;
begin
  GetProcedureAddress(_HttpOpenRequestW, winetdll, 'HttpOpenRequestW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HttpOpenRequestW]
  end;
end;

var
  _HttpAddRequestHeadersW: Pointer;

function HttpAddRequestHeadersW;
begin
  GetProcedureAddress(_HttpAddRequestHeadersW, winetdll, 'HttpAddRequestHeadersW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HttpAddRequestHeadersW]
  end;
end;

var
  _HttpSendRequestW: Pointer;

function HttpSendRequestW;
begin
  GetProcedureAddress(_HttpSendRequestW, winetdll, 'HttpSendRequestW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HttpSendRequestW]
  end;
end;

var
  _HttpSendRequestExW: Pointer;

function HttpSendRequestExW;
begin
  GetProcedureAddress(_HttpSendRequestExW, winetdll, 'HttpSendRequestExW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HttpSendRequestExW]
  end;
end;

var
  _HttpEndRequestW: Pointer;

function HttpEndRequestW;
begin
  GetProcedureAddress(_HttpEndRequestW, winetdll, 'HttpEndRequestW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HttpEndRequestW]
  end;
end;

var
  _HttpQueryInfoW: Pointer;

function HttpQueryInfoW;
begin
  GetProcedureAddress(_HttpQueryInfoW, winetdll, 'HttpQueryInfoW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HttpQueryInfoW]
  end;
end;

var
  _InternetSetCookieW: Pointer;

function InternetSetCookieW;
begin
  GetProcedureAddress(_InternetSetCookieW, winetdll, 'InternetSetCookieW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetSetCookieW]
  end;
end;

var
  _InternetGetCookieW: Pointer;

function InternetGetCookieW;
begin
  GetProcedureAddress(_InternetGetCookieW, winetdll, 'InternetGetCookieW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetGetCookieW]
  end;
end;

var
  _GopherCreateLocator: Pointer;

function GopherCreateLocator;
begin
  GetProcedureAddress(_GopherCreateLocator, winetdll, 'GopherCreateLocator');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GopherCreateLocator]
  end;
end;

var
  _GopherGetLocatorType: Pointer;

function GopherGetLocatorType;
begin
  GetProcedureAddress(_GopherGetLocatorType, winetdll, 'GopherGetLocatorType');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GopherGetLocatorType]
  end;
end;

var
  _GopherFindFirstFile: Pointer;

function GopherFindFirstFile;
begin
  GetProcedureAddress(_GopherFindFirstFile, winetdll, 'GopherFindFirstFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GopherFindFirstFile]
  end;
end;

var
  _GopherOpenFile: Pointer;

function GopherOpenFile;
begin
  GetProcedureAddress(_GopherOpenFile, winetdll, 'GopherOpenFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GopherOpenFile]
  end;
end;

var
  _GopherGetAttribute: Pointer;

function GopherGetAttribute;
begin
  GetProcedureAddress(_GopherGetAttribute, winetdll, 'GopherGetAttribute');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GopherGetAttribute]
  end;
end;

var
  _HttpOpenRequest: Pointer;

function HttpOpenRequest;
begin
  GetProcedureAddress(_HttpOpenRequest, winetdll, 'HttpOpenRequest');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HttpOpenRequest]
  end;
end;

var
  _HttpAddRequestHeaders: Pointer;

function HttpAddRequestHeaders;
begin
  GetProcedureAddress(_HttpAddRequestHeaders, winetdll, 'HttpAddRequestHeaders');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HttpAddRequestHeaders]
  end;
end;

var
  _HttpSendRequest: Pointer;

function HttpSendRequest;
begin
  GetProcedureAddress(_HttpSendRequest, winetdll, 'HttpSendRequest');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HttpSendRequest]
  end;
end;

var
  _HttpSendRequestEx: Pointer;

function HttpSendRequestEx;
begin
  GetProcedureAddress(_HttpSendRequestEx, winetdll, 'HttpSendRequestEx');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HttpSendRequestEx]
  end;
end;

var
  _HttpEndRequest: Pointer;

function HttpEndRequest;
begin
  GetProcedureAddress(_HttpEndRequest, winetdll, 'HttpEndRequest');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HttpEndRequest]
  end;
end;

var
  _HttpQueryInfo: Pointer;

function HttpQueryInfo;
begin
  GetProcedureAddress(_HttpQueryInfo, winetdll, 'HttpQueryInfo');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HttpQueryInfo]
  end;
end;

var
  _InternetSetCookie: Pointer;

function InternetSetCookie;
begin
  GetProcedureAddress(_InternetSetCookie, winetdll, 'InternetSetCookie');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetSetCookie]
  end;
end;

var
  _InternetGetCookie: Pointer;

function InternetGetCookie;
begin
  GetProcedureAddress(_InternetGetCookie, winetdll, 'InternetGetCookie');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetGetCookie]
  end;
end;

var
  _InternetAttemptConnect: Pointer;

function InternetAttemptConnect;
begin
  GetProcedureAddress(_InternetAttemptConnect, winetdll, 'InternetAttemptConnect');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetAttemptConnect]
  end;
end;

var
  _InternetCheckConnectionA: Pointer;

function InternetCheckConnectionA;
begin
  GetProcedureAddress(_InternetCheckConnectionA, winetdll, 'InternetCheckConnectionA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetCheckConnectionA]
  end;
end;

var
  _InternetCheckConnectionW: Pointer;

function InternetCheckConnectionW;
begin
  GetProcedureAddress(_InternetCheckConnectionW, winetdll, 'InternetCheckConnectionW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetCheckConnectionW]
  end;
end;

var
  _InternetCheckConnection: Pointer;

function InternetCheckConnection;
begin
  GetProcedureAddress(_InternetCheckConnection, winetdll, 'InternetCheckConnection');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetCheckConnection]
  end;
end;

var
  _InternetAuthNotifyCallback: Pointer;

function InternetAuthNotifyCallback;
begin
  GetProcedureAddress(_InternetAuthNotifyCallback, winetdll, 'InternetAuthNotifyCallback');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetAuthNotifyCallback]
  end;
end;

var
  _InternetErrorDlg: Pointer;

function InternetErrorDlg;
begin
  GetProcedureAddress(_InternetErrorDlg, winetdll, 'InternetErrorDlg');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetErrorDlg]
  end;
end;

var
  _ResumeSuspendedDownload: Pointer;

function ResumeSuspendedDownload;
begin
  GetProcedureAddress(_ResumeSuspendedDownload, winetdll, 'ResumeSuspendedDownload');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ResumeSuspendedDownload]
  end;
end;

var
  _InternetConfirmZoneCrossingA: Pointer;

function InternetConfirmZoneCrossingA;
begin
  GetProcedureAddress(_InternetConfirmZoneCrossingA, winetdll, 'InternetConfirmZoneCrossingA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetConfirmZoneCrossingA]
  end;
end;

var
  _CreateUrlCacheEntryA: Pointer;

function CreateUrlCacheEntryA;
begin
  GetProcedureAddress(_CreateUrlCacheEntryA, winetdll, 'CreateUrlCacheEntryA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateUrlCacheEntryA]
  end;
end;

var
  _CommitUrlCacheEntryA: Pointer;

function CommitUrlCacheEntryA;
begin
  GetProcedureAddress(_CommitUrlCacheEntryA, winetdll, 'CommitUrlCacheEntryA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CommitUrlCacheEntryA]
  end;
end;

var
  _RetrieveUrlCacheEntryFileA: Pointer;

function RetrieveUrlCacheEntryFileA;
begin
  GetProcedureAddress(_RetrieveUrlCacheEntryFileA, winetdll, 'RetrieveUrlCacheEntryFileA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RetrieveUrlCacheEntryFileA]
  end;
end;

var
  _UnlockUrlCacheEntryFileA: Pointer;

function UnlockUrlCacheEntryFileA;
begin
  GetProcedureAddress(_UnlockUrlCacheEntryFileA, winetdll, 'UnlockUrlCacheEntryFileA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UnlockUrlCacheEntryFileA]
  end;
end;

var
  _RetrieveUrlCacheEntryStreamA: Pointer;

function RetrieveUrlCacheEntryStreamA;
begin
  GetProcedureAddress(_RetrieveUrlCacheEntryStreamA, winetdll, 'RetrieveUrlCacheEntryStreamA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RetrieveUrlCacheEntryStreamA]
  end;
end;

var
  _InternetConfirmZoneCrossingW: Pointer;

function InternetConfirmZoneCrossingW;
begin
  GetProcedureAddress(_InternetConfirmZoneCrossingW, winetdll, 'InternetConfirmZoneCrossingW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetConfirmZoneCrossingW]
  end;
end;

var
  _CreateUrlCacheEntryW: Pointer;

function CreateUrlCacheEntryW;
begin
  GetProcedureAddress(_CreateUrlCacheEntryW, winetdll, 'CreateUrlCacheEntryW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateUrlCacheEntryW]
  end;
end;

var
  _CommitUrlCacheEntryW: Pointer;

function CommitUrlCacheEntryW;
begin
  GetProcedureAddress(_CommitUrlCacheEntryW, winetdll, 'CommitUrlCacheEntryW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CommitUrlCacheEntryW]
  end;
end;

var
  _RetrieveUrlCacheEntryFileW: Pointer;

function RetrieveUrlCacheEntryFileW;
begin
  GetProcedureAddress(_RetrieveUrlCacheEntryFileW, winetdll, 'RetrieveUrlCacheEntryFileW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RetrieveUrlCacheEntryFileW]
  end;
end;

var
  _UnlockUrlCacheEntryFileW: Pointer;

function UnlockUrlCacheEntryFileW;
begin
  GetProcedureAddress(_UnlockUrlCacheEntryFileW, winetdll, 'UnlockUrlCacheEntryFileW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UnlockUrlCacheEntryFileW]
  end;
end;

var
  _RetrieveUrlCacheEntryStreamW: Pointer;

function RetrieveUrlCacheEntryStreamW;
begin
  GetProcedureAddress(_RetrieveUrlCacheEntryStreamW, winetdll, 'RetrieveUrlCacheEntryStreamW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RetrieveUrlCacheEntryStreamW]
  end;
end;

var
  _InternetConfirmZoneCrossing: Pointer;

function InternetConfirmZoneCrossing;
begin
  GetProcedureAddress(_InternetConfirmZoneCrossing, winetdll, 'InternetConfirmZoneCrossing');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetConfirmZoneCrossing]
  end;
end;

var
  _CreateUrlCacheEntry: Pointer;

function CreateUrlCacheEntry;
begin
  GetProcedureAddress(_CreateUrlCacheEntry, winetdll, 'CreateUrlCacheEntry');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateUrlCacheEntry]
  end;
end;

var
  _CommitUrlCacheEntry: Pointer;

function CommitUrlCacheEntry;
begin
  GetProcedureAddress(_CommitUrlCacheEntry, winetdll, 'CommitUrlCacheEntry');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CommitUrlCacheEntry]
  end;
end;

var
  _RetrieveUrlCacheEntryFile: Pointer;

function RetrieveUrlCacheEntryFile;
begin
  GetProcedureAddress(_RetrieveUrlCacheEntryFile, winetdll, 'RetrieveUrlCacheEntryFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RetrieveUrlCacheEntryFile]
  end;
end;

var
  _UnlockUrlCacheEntryFile: Pointer;

function UnlockUrlCacheEntryFile;
begin
  GetProcedureAddress(_UnlockUrlCacheEntryFile, winetdll, 'UnlockUrlCacheEntryFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UnlockUrlCacheEntryFile]
  end;
end;

var
  _RetrieveUrlCacheEntryStream: Pointer;

function RetrieveUrlCacheEntryStream;
begin
  GetProcedureAddress(_RetrieveUrlCacheEntryStream, winetdll, 'RetrieveUrlCacheEntryStream');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RetrieveUrlCacheEntryStream]
  end;
end;

var
  _ReadUrlCacheEntryStream: Pointer;

function ReadUrlCacheEntryStream;
begin
  GetProcedureAddress(_ReadUrlCacheEntryStream, winetdll, 'ReadUrlCacheEntryStream');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ReadUrlCacheEntryStream]
  end;
end;

var
  _UnlockUrlCacheEntryStream: Pointer;

function UnlockUrlCacheEntryStream;
begin
  GetProcedureAddress(_UnlockUrlCacheEntryStream, winetdll, 'UnlockUrlCacheEntryStream');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UnlockUrlCacheEntryStream]
  end;
end;

var
  _GetUrlCacheEntryInfoA: Pointer;

function GetUrlCacheEntryInfoA;
begin
  GetProcedureAddress(_GetUrlCacheEntryInfoA, winetdll, 'GetUrlCacheEntryInfoA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetUrlCacheEntryInfoA]
  end;
end;

var
  _GetUrlCacheEntryInfoW: Pointer;

function GetUrlCacheEntryInfoW;
begin
  GetProcedureAddress(_GetUrlCacheEntryInfoW, winetdll, 'GetUrlCacheEntryInfoW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetUrlCacheEntryInfoW]
  end;
end;

var
  _GetUrlCacheEntryInfo: Pointer;

function GetUrlCacheEntryInfo;
begin
  GetProcedureAddress(_GetUrlCacheEntryInfo, winetdll, 'GetUrlCacheEntryInfo');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetUrlCacheEntryInfo]
  end;
end;

var
  _FindFirstUrlCacheGroup: Pointer;

function FindFirstUrlCacheGroup;
begin
  GetProcedureAddress(_FindFirstUrlCacheGroup, winetdll, 'FindFirstUrlCacheGroup');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FindFirstUrlCacheGroup]
  end;
end;

var
  _FindNextUrlCacheGroup: Pointer;

function FindNextUrlCacheGroup;
begin
  GetProcedureAddress(_FindNextUrlCacheGroup, winetdll, 'FindNextUrlCacheGroup');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FindNextUrlCacheGroup]
  end;
end;

var
  _GetUrlCacheGroupAttributeA: Pointer;

function GetUrlCacheGroupAttributeA;
begin
  GetProcedureAddress(_GetUrlCacheGroupAttributeA, winetdll, 'GetUrlCacheGroupAttributeA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetUrlCacheGroupAttributeA]
  end;
end;

var
  _SetUrlCacheGroupAttributeA: Pointer;

function SetUrlCacheGroupAttributeA;
begin
  GetProcedureAddress(_SetUrlCacheGroupAttributeA, winetdll, 'SetUrlCacheGroupAttributeA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetUrlCacheGroupAttributeA]
  end;
end;

var
  _GetUrlCacheEntryInfoExA: Pointer;

function GetUrlCacheEntryInfoExA;
begin
  GetProcedureAddress(_GetUrlCacheEntryInfoExA, winetdll, 'GetUrlCacheEntryInfoExA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetUrlCacheEntryInfoExA]
  end;
end;

var
  _SetUrlCacheEntryInfoA: Pointer;

function SetUrlCacheEntryInfoA;
begin
  GetProcedureAddress(_SetUrlCacheEntryInfoA, winetdll, 'SetUrlCacheEntryInfoA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetUrlCacheEntryInfoA]
  end;
end;

var
  _GetUrlCacheGroupAttributeW: Pointer;

function GetUrlCacheGroupAttributeW;
begin
  GetProcedureAddress(_GetUrlCacheGroupAttributeW, winetdll, 'GetUrlCacheGroupAttributeW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetUrlCacheGroupAttributeW]
  end;
end;

var
  _SetUrlCacheGroupAttributeW: Pointer;

function SetUrlCacheGroupAttributeW;
begin
  GetProcedureAddress(_SetUrlCacheGroupAttributeW, winetdll, 'SetUrlCacheGroupAttributeW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetUrlCacheGroupAttributeW]
  end;
end;

var
  _GetUrlCacheEntryInfoExW: Pointer;

function GetUrlCacheEntryInfoExW;
begin
  GetProcedureAddress(_GetUrlCacheEntryInfoExW, winetdll, 'GetUrlCacheEntryInfoExW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetUrlCacheEntryInfoExW]
  end;
end;

var
  _SetUrlCacheEntryInfoW: Pointer;

function SetUrlCacheEntryInfoW;
begin
  GetProcedureAddress(_SetUrlCacheEntryInfoW, winetdll, 'SetUrlCacheEntryInfoW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetUrlCacheEntryInfoW]
  end;
end;

var
  _GetUrlCacheGroupAttribute: Pointer;

function GetUrlCacheGroupAttribute;
begin
  GetProcedureAddress(_GetUrlCacheGroupAttribute, winetdll, 'GetUrlCacheGroupAttribute');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetUrlCacheGroupAttribute]
  end;
end;

var
  _SetUrlCacheGroupAttribute: Pointer;

function SetUrlCacheGroupAttribute;
begin
  GetProcedureAddress(_SetUrlCacheGroupAttribute, winetdll, 'SetUrlCacheGroupAttribute');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetUrlCacheGroupAttribute]
  end;
end;

var
  _GetUrlCacheEntryInfoEx: Pointer;

function GetUrlCacheEntryInfoEx;
begin
  GetProcedureAddress(_GetUrlCacheEntryInfoEx, winetdll, 'GetUrlCacheEntryInfoEx');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetUrlCacheEntryInfoEx]
  end;
end;

var
  _SetUrlCacheEntryInfo: Pointer;

function SetUrlCacheEntryInfo;
begin
  GetProcedureAddress(_SetUrlCacheEntryInfo, winetdll, 'SetUrlCacheEntryInfo');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetUrlCacheEntryInfo]
  end;
end;

var
  _CreateUrlCacheGroup: Pointer;

function CreateUrlCacheGroup;
begin
  GetProcedureAddress(_CreateUrlCacheGroup, winetdll, 'CreateUrlCacheGroup');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateUrlCacheGroup]
  end;
end;

var
  _DeleteUrlCacheGroup: Pointer;

function DeleteUrlCacheGroup;
begin
  GetProcedureAddress(_DeleteUrlCacheGroup, winetdll, 'DeleteUrlCacheGroup');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DeleteUrlCacheGroup]
  end;
end;

var
  _SetUrlCacheEntryGroupA: Pointer;

function SetUrlCacheEntryGroupA;
begin
  GetProcedureAddress(_SetUrlCacheEntryGroupA, winetdll, 'SetUrlCacheEntryGroupA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetUrlCacheEntryGroupA]
  end;
end;

var
  _FindFirstUrlCacheEntryExA: Pointer;

function FindFirstUrlCacheEntryExA;
begin
  GetProcedureAddress(_FindFirstUrlCacheEntryExA, winetdll, 'FindFirstUrlCacheEntryExA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FindFirstUrlCacheEntryExA]
  end;
end;

var
  _FindNextUrlCacheEntryExA: Pointer;

function FindNextUrlCacheEntryExA;
begin
  GetProcedureAddress(_FindNextUrlCacheEntryExA, winetdll, 'FindNextUrlCacheEntryExA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FindNextUrlCacheEntryExA]
  end;
end;

var
  _FindFirstUrlCacheEntryA: Pointer;

function FindFirstUrlCacheEntryA;
begin
  GetProcedureAddress(_FindFirstUrlCacheEntryA, winetdll, 'FindFirstUrlCacheEntryA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FindFirstUrlCacheEntryA]
  end;
end;

var
  _FindNextUrlCacheEntryA: Pointer;

function FindNextUrlCacheEntryA;
begin
  GetProcedureAddress(_FindNextUrlCacheEntryA, winetdll, 'FindNextUrlCacheEntryA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FindNextUrlCacheEntryA]
  end;
end;

var
  _SetUrlCacheEntryGroupW: Pointer;

function SetUrlCacheEntryGroupW;
begin
  GetProcedureAddress(_SetUrlCacheEntryGroupW, winetdll, 'SetUrlCacheEntryGroupW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetUrlCacheEntryGroupW]
  end;
end;

var
  _FindFirstUrlCacheEntryExW: Pointer;

function FindFirstUrlCacheEntryExW;
begin
  GetProcedureAddress(_FindFirstUrlCacheEntryExW, winetdll, 'FindFirstUrlCacheEntryExW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FindFirstUrlCacheEntryExW]
  end;
end;

var
  _FindNextUrlCacheEntryExW: Pointer;

function FindNextUrlCacheEntryExW;
begin
  GetProcedureAddress(_FindNextUrlCacheEntryExW, winetdll, 'FindNextUrlCacheEntryExW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FindNextUrlCacheEntryExW]
  end;
end;

var
  _FindFirstUrlCacheEntryW: Pointer;

function FindFirstUrlCacheEntryW;
begin
  GetProcedureAddress(_FindFirstUrlCacheEntryW, winetdll, 'FindFirstUrlCacheEntryW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FindFirstUrlCacheEntryW]
  end;
end;

var
  _FindNextUrlCacheEntryW: Pointer;

function FindNextUrlCacheEntryW;
begin
  GetProcedureAddress(_FindNextUrlCacheEntryW, winetdll, 'FindNextUrlCacheEntryW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FindNextUrlCacheEntryW]
  end;
end;

var
  _SetUrlCacheEntryGroup: Pointer;

function SetUrlCacheEntryGroup;
begin
  GetProcedureAddress(_SetUrlCacheEntryGroup, winetdll, 'SetUrlCacheEntryGroup');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetUrlCacheEntryGroup]
  end;
end;

var
  _FindFirstUrlCacheEntryEx: Pointer;

function FindFirstUrlCacheEntryEx;
begin
  GetProcedureAddress(_FindFirstUrlCacheEntryEx, winetdll, 'FindFirstUrlCacheEntryEx');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FindFirstUrlCacheEntryEx]
  end;
end;

var
  _FindNextUrlCacheEntryEx: Pointer;

function FindNextUrlCacheEntryEx;
begin
  GetProcedureAddress(_FindNextUrlCacheEntryEx, winetdll, 'FindNextUrlCacheEntryEx');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FindNextUrlCacheEntryEx]
  end;
end;

var
  _FindFirstUrlCacheEntry: Pointer;

function FindFirstUrlCacheEntry;
begin
  GetProcedureAddress(_FindFirstUrlCacheEntry, winetdll, 'FindFirstUrlCacheEntry');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FindFirstUrlCacheEntry]
  end;
end;

var
  _FindNextUrlCacheEntry: Pointer;

function FindNextUrlCacheEntry;
begin
  GetProcedureAddress(_FindNextUrlCacheEntry, winetdll, 'FindNextUrlCacheEntry');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FindNextUrlCacheEntry]
  end;
end;

var
  _FindCloseUrlCache: Pointer;

function FindCloseUrlCache;
begin
  GetProcedureAddress(_FindCloseUrlCache, winetdll, 'FindCloseUrlCache');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FindCloseUrlCache]
  end;
end;

var
  _DeleteUrlCacheEntryA: Pointer;

function DeleteUrlCacheEntryA;
begin
  GetProcedureAddress(_DeleteUrlCacheEntryA, winetdll, 'DeleteUrlCacheEntryA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DeleteUrlCacheEntryA]
  end;
end;

var
  _InternetDialA: Pointer;

function InternetDialA;
begin
  GetProcedureAddress(_InternetDialA, winetdll, 'InternetDialA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetDialA]
  end;
end;

var
  _DeleteUrlCacheEntryW: Pointer;

function DeleteUrlCacheEntryW;
begin
  GetProcedureAddress(_DeleteUrlCacheEntryW, winetdll, 'DeleteUrlCacheEntryW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DeleteUrlCacheEntryW]
  end;
end;

var
  _InternetDialW: Pointer;

function InternetDialW;
begin
  GetProcedureAddress(_InternetDialW, winetdll, 'InternetDialW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetDialW]
  end;
end;

var
  _DeleteUrlCacheEntry: Pointer;

function DeleteUrlCacheEntry;
begin
  GetProcedureAddress(_DeleteUrlCacheEntry, winetdll, 'DeleteUrlCacheEntry');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DeleteUrlCacheEntry]
  end;
end;

var
  _InternetDial: Pointer;

function InternetDial;
begin
  GetProcedureAddress(_InternetDial, winetdll, 'InternetDial');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetDial]
  end;
end;

var
  _InternetHangUp: Pointer;

function InternetHangUp;
begin
  GetProcedureAddress(_InternetHangUp, winetdll, 'InternetHangUp');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetHangUp]
  end;
end;

var
  _InternetGoOnlineA: Pointer;

function InternetGoOnlineA;
begin
  GetProcedureAddress(_InternetGoOnlineA, winetdll, 'InternetGoOnlineA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetGoOnlineA]
  end;
end;

var
  _InternetGoOnlineW: Pointer;

function InternetGoOnlineW;
begin
  GetProcedureAddress(_InternetGoOnlineW, winetdll, 'InternetGoOnlineW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetGoOnlineW]
  end;
end;

var
  _InternetGoOnline: Pointer;

function InternetGoOnline;
begin
  GetProcedureAddress(_InternetGoOnline, winetdll, 'InternetGoOnline');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetGoOnline]
  end;
end;

var
  _InternetAutodial: Pointer;

function InternetAutodial;
begin
  GetProcedureAddress(_InternetAutodial, winetdll, 'InternetAutodial');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetAutodial]
  end;
end;

var
  _InternetAutodialHangup: Pointer;

function InternetAutodialHangup;
begin
  GetProcedureAddress(_InternetAutodialHangup, winetdll, 'InternetAutodialHangup');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetAutodialHangup]
  end;
end;

var
  _InternetGetConnectedState: Pointer;

function InternetGetConnectedState;
begin
  GetProcedureAddress(_InternetGetConnectedState, winetdll, 'InternetGetConnectedState');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetGetConnectedState]
  end;
end;

var
  _InternetGetConnectedStateExA: Pointer;

function InternetGetConnectedStateExA;
begin
  GetProcedureAddress(_InternetGetConnectedStateExA, winetdll, 'InternetGetConnectedStateExA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetGetConnectedStateExA]
  end;
end;

var
  _InternetGetConnectedStateExW: Pointer;

function InternetGetConnectedStateExW;
begin
  GetProcedureAddress(_InternetGetConnectedStateExW, winetdll, 'InternetGetConnectedStateExW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetGetConnectedStateExW]
  end;
end;

var
  _InternetGetConnectedStateEx: Pointer;

function InternetGetConnectedStateEx;
begin
  GetProcedureAddress(_InternetGetConnectedStateEx, winetdll, 'InternetGetConnectedStateEx');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetGetConnectedStateEx]
  end;
end;

var
  _InternetInitializeAutoProxyDll: Pointer;

function InternetInitializeAutoProxyDll;
begin
  GetProcedureAddress(_InternetInitializeAutoProxyDll, winetdll, 'InternetInitializeAutoProxyDll');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetInitializeAutoProxyDll]
  end;
end;

var
  _InternetSetDialStateA: Pointer;

function InternetSetDialStateA;
begin
  GetProcedureAddress(_InternetSetDialStateA, winetdll, 'InternetSetDialStateA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetSetDialStateA]
  end;
end;

var
  _InternetOpenA: Pointer;

function InternetOpenA;
begin
  GetProcedureAddress(_InternetOpenA, winetdll, 'InternetOpenA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetOpenA]
  end;
end;

var
  _InternetSetDialStateW: Pointer;

function InternetSetDialStateW;
begin
  GetProcedureAddress(_InternetSetDialStateW, winetdll, 'InternetSetDialStateW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetSetDialStateW]
  end;
end;

var
  _InternetOpenW: Pointer;

function InternetOpenW;
begin
  GetProcedureAddress(_InternetOpenW, winetdll, 'InternetOpenW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetOpenW]
  end;
end;

var
  _InternetSetDialState: Pointer;

function InternetSetDialState;
begin
  GetProcedureAddress(_InternetSetDialState, winetdll, 'InternetSetDialState');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetSetDialState]
  end;
end;

var
  _InternetOpen: Pointer;

function InternetOpen;
begin
  GetProcedureAddress(_InternetOpen, winetdll, 'InternetOpen');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InternetOpen]
  end;
end;



{$ELSE}

function InternetConnectA; external winetdll name 'InternetConnectA';
function InternetConnectW; external winetdll name 'InternetConnectW';
function InternetConnect; external winetdll name 'InternetConnectA';
function InternetCloseHandle; external winetdll name 'InternetCloseHandle';
function InternetTimeFromSystemTimeA; external winetdll name 'InternetTimeFromSystemTimeA';
function InternetTimeToSystemTimeA; external winetdll name 'InternetTimeToSystemTimeA';
function InternetCrackUrlA; external winetdll name 'InternetCrackUrlA';
function InternetCreateUrlA; external winetdll name 'InternetCreateUrlA';
function InternetCanonicalizeUrlA; external winetdll name 'InternetCanonicalizeUrlA';
function InternetCombineUrlA; external winetdll name 'InternetCombineUrlA';
function InternetOpenUrlA; external winetdll name 'InternetOpenUrlA';
function InternetTimeFromSystemTimeW; external winetdll name 'InternetTimeFromSystemTimeW';
function InternetTimeToSystemTimeW; external winetdll name 'InternetTimeToSystemTimeW';
function InternetCrackUrlW; external winetdll name 'InternetCrackUrlW';
function InternetCreateUrlW; external winetdll name 'InternetCreateUrlW';
function InternetCanonicalizeUrlW; external winetdll name 'InternetCanonicalizeUrlW';
function InternetCombineUrlW; external winetdll name 'InternetCombineUrlW';
function InternetOpenUrlW; external winetdll name 'InternetOpenUrlW';
function InternetTimeFromSystemTime; external winetdll name 'InternetTimeFromSystemTimeA';
function InternetTimeToSystemTime; external winetdll name 'InternetTimeToSystemTimeA';
function InternetCrackUrl; external winetdll name 'InternetCrackUrlA';
function InternetCreateUrl; external winetdll name 'InternetCreateUrlA';
function InternetCanonicalizeUrl; external winetdll name 'InternetCanonicalizeUrlA';
function InternetCombineUrl; external winetdll name 'InternetCombineUrlA';
function InternetOpenUrl; external winetdll name 'InternetOpenUrlA';
function InternetReadFile; external winetdll name 'InternetReadFile';
function InternetReadFileExA; external winetdll name 'InternetReadFileExA';
function InternetReadFileExW; external winetdll name 'InternetReadFileExW';
function InternetReadFileEx; external winetdll name 'InternetReadFileExA';
function InternetSetFilePointer; external winetdll name 'InternetSetFilePointer';
function InternetWriteFile; external winetdll name 'InternetWriteFile';
function InternetQueryDataAvailable; external winetdll name 'InternetQueryDataAvailable';
function InternetFindNextFileA; external winetdll name 'InternetFindNextFileA';
function InternetQueryOptionA; external winetdll name 'InternetQueryOptionA';
function InternetSetOptionA; external winetdll name 'InternetSetOptionA';
function InternetSetOptionExA; external winetdll name 'InternetSetOptionExA';
function InternetFindNextFileW; external winetdll name 'InternetFindNextFileW';
function InternetQueryOptionW; external winetdll name 'InternetQueryOptionW';
function InternetSetOptionW; external winetdll name 'InternetSetOptionW';
function InternetSetOptionExW; external winetdll name 'InternetSetOptionExW';
function InternetFindNextFile; external winetdll name 'InternetFindNextFileA';
function InternetQueryOption; external winetdll name 'InternetQueryOptionA';
function InternetSetOption; external winetdll name 'InternetSetOptionA';
function InternetSetOptionEx; external winetdll name 'InternetSetOptionExA';
function InternetLockRequestFile; external winetdll name 'InternetLockRequestFile';
function InternetUnlockRequestFile; external winetdll name 'InternetUnlockRequestFile';
function InternetGetLastResponseInfoA; external winetdll name 'InternetGetLastResponseInfoA';
function InternetSetStatusCallbackA; external winetdll name 'InternetSetStatusCallbackA';
function FtpFindFirstFileA; external winetdll name 'FtpFindFirstFileA';
function FtpGetFileA; external winetdll name 'FtpGetFileA';
function FtpPutFileA; external winetdll name 'FtpPutFileA';
function InternetGetLastResponseInfoW; external winetdll name 'InternetGetLastResponseInfoW';
function InternetSetStatusCallbackW; external winetdll name 'InternetSetStatusCallbackW';
function FtpFindFirstFileW; external winetdll name 'FtpFindFirstFileW';
function FtpGetFileW; external winetdll name 'FtpGetFileW';
function FtpPutFileW; external winetdll name 'FtpPutFileW';
function InternetGetLastResponseInfo; external winetdll name 'InternetGetLastResponseInfoA';
function InternetSetStatusCallback; external winetdll name 'InternetSetStatusCallbackA';
function FtpFindFirstFile; external winetdll name 'FtpFindFirstFileA';
function FtpGetFile; external winetdll name 'FtpGetFileA';
function FtpPutFile; external winetdll name 'FtpPutFileA';
function FtpGetFileEx; external winetdll name 'FtpGetFileEx';
function FtpPutFileEx; external winetdll name 'FtpPutFileEx';
function FtpDeleteFileA; external winetdll name 'FtpDeleteFileA';
function FtpRenameFileA; external winetdll name 'FtpRenameFileA';
function FtpOpenFileA; external winetdll name 'FtpOpenFileA';
function FtpCreateDirectoryA; external winetdll name 'FtpCreateDirectoryA';
function FtpRemoveDirectoryA; external winetdll name 'FtpRemoveDirectoryA';
function FtpSetCurrentDirectoryA; external winetdll name 'FtpSetCurrentDirectoryA';
function FtpGetCurrentDirectoryA; external winetdll name 'FtpGetCurrentDirectoryA';
function FtpCommandA; external winetdll name 'FtpCommandA';
function FtpDeleteFileW; external winetdll name 'FtpDeleteFileW';
function FtpRenameFileW; external winetdll name 'FtpRenameFileW';
function FtpOpenFileW; external winetdll name 'FtpOpenFileW';
function FtpCreateDirectoryW; external winetdll name 'FtpCreateDirectoryW';
function FtpRemoveDirectoryW; external winetdll name 'FtpRemoveDirectoryW';
function FtpSetCurrentDirectoryW; external winetdll name 'FtpSetCurrentDirectoryW';
function FtpGetCurrentDirectoryW; external winetdll name 'FtpGetCurrentDirectoryW';
function FtpCommandW; external winetdll name 'FtpCommandW';
function FtpDeleteFile; external winetdll name 'FtpDeleteFileA';
function FtpRenameFile; external winetdll name 'FtpRenameFileA';
function FtpOpenFile; external winetdll name 'FtpOpenFileA';
function FtpCreateDirectory; external winetdll name 'FtpCreateDirectoryA';
function FtpRemoveDirectory; external winetdll name 'FtpRemoveDirectoryA';
function FtpSetCurrentDirectory; external winetdll name 'FtpSetCurrentDirectoryA';
function FtpGetCurrentDirectory; external winetdll name 'FtpGetCurrentDirectoryA';
function FtpCommand; external winetdll name 'FtpCommandA';
function FtpGetFileSize; external winetdll name 'FtpGetFileSize';
function GopherCreateLocatorA; external winetdll name 'GopherCreateLocatorA';
function GopherGetLocatorTypeA; external winetdll name 'GopherGetLocatorTypeA';
function GopherFindFirstFileA; external winetdll name 'GopherFindFirstFileA';
function GopherOpenFileA; external winetdll name 'GopherOpenFileA';
function GopherGetAttributeA; external winetdll name 'GopherGetAttributeA';
function HttpOpenRequestA; external winetdll name 'HttpOpenRequestA';
function HttpAddRequestHeadersA; external winetdll name 'HttpAddRequestHeadersA';
function HttpSendRequestA; external winetdll name 'HttpSendRequestA';
function HttpSendRequestExA; external winetdll name 'HttpSendRequestExA';
function HttpEndRequestA; external winetdll name 'HttpEndRequestA';
function HttpQueryInfoA; external winetdll name 'HttpQueryInfoA';
function InternetSetCookieA; external winetdll name 'InternetSetCookieA';
function InternetGetCookieA; external winetdll name 'InternetGetCookieA';
function GopherCreateLocatorW; external winetdll name 'GopherCreateLocatorW';
function GopherGetLocatorTypeW; external winetdll name 'GopherGetLocatorTypeW';
function GopherFindFirstFileW; external winetdll name 'GopherFindFirstFileW';
function GopherOpenFileW; external winetdll name 'GopherOpenFileW';
function GopherGetAttributeW; external winetdll name 'GopherGetAttributeW';
function HttpOpenRequestW; external winetdll name 'HttpOpenRequestW';
function HttpAddRequestHeadersW; external winetdll name 'HttpAddRequestHeadersW';
function HttpSendRequestW; external winetdll name 'HttpSendRequestW';
function HttpSendRequestExW; external winetdll name 'HttpSendRequestExW';
function HttpEndRequestW; external winetdll name 'HttpEndRequestW';
function HttpQueryInfoW; external winetdll name 'HttpQueryInfoW';
function InternetSetCookieW; external winetdll name 'InternetSetCookieW';
function InternetGetCookieW; external winetdll name 'InternetGetCookieW';
function GopherCreateLocator; external winetdll name 'GopherCreateLocatorA';
function GopherGetLocatorType; external winetdll name 'GopherGetLocatorTypeA';
function GopherFindFirstFile; external winetdll name 'GopherFindFirstFileA';
function GopherOpenFile; external winetdll name 'GopherOpenFileA';
function GopherGetAttribute; external winetdll name 'GopherGetAttributeA';
function HttpOpenRequest; external winetdll name 'HttpOpenRequestA';
function HttpAddRequestHeaders; external winetdll name 'HttpAddRequestHeadersA';
function HttpSendRequest; external winetdll name 'HttpSendRequestA';
function HttpSendRequestEx; external winetdll name 'HttpSendRequestExA';
function HttpEndRequest; external winetdll name 'HttpEndRequestA';
function HttpQueryInfo; external winetdll name 'HttpQueryInfoA';
function InternetSetCookie; external winetdll name 'InternetSetCookieA';
function InternetGetCookie; external winetdll name 'InternetGetCookieA';
function InternetAttemptConnect; external winetdll name 'InternetAttemptConnect';
function InternetCheckConnectionA; external winetdll name 'InternetCheckConnectionA';
function InternetCheckConnectionW; external winetdll name 'InternetCheckConnectionW';
function InternetCheckConnection; external winetdll name 'InternetCheckConnectionA';
function InternetAuthNotifyCallback; external winetdll name 'InternetAuthNotifyCallback';
function InternetErrorDlg; external winetdll name 'InternetErrorDlg';
function ResumeSuspendedDownload; external winetdll name 'ResumeSuspendedDownload';
function InternetConfirmZoneCrossingA; external winetdll name 'InternetConfirmZoneCrossingA';
function CreateUrlCacheEntryA; external winetdll name 'CreateUrlCacheEntryA';
function CommitUrlCacheEntryA; external winetdll name 'CommitUrlCacheEntryA';
function RetrieveUrlCacheEntryFileA; external winetdll name 'RetrieveUrlCacheEntryFileA';
function UnlockUrlCacheEntryFileA; external winetdll name 'UnlockUrlCacheEntryFileA';
function RetrieveUrlCacheEntryStreamA; external winetdll name 'RetrieveUrlCacheEntryStreamA';
function InternetConfirmZoneCrossingW; external winetdll name 'InternetConfirmZoneCrossingW';
function CreateUrlCacheEntryW; external winetdll name 'CreateUrlCacheEntryW';
function CommitUrlCacheEntryW; external winetdll name 'CommitUrlCacheEntryW';
function RetrieveUrlCacheEntryFileW; external winetdll name 'RetrieveUrlCacheEntryFileW';
function UnlockUrlCacheEntryFileW; external winetdll name 'UnlockUrlCacheEntryFileW';
function RetrieveUrlCacheEntryStreamW; external winetdll name 'RetrieveUrlCacheEntryStreamW';
function InternetConfirmZoneCrossing; external winetdll name 'InternetConfirmZoneCrossingA';
function CreateUrlCacheEntry; external winetdll name 'CreateUrlCacheEntryA';
function CommitUrlCacheEntry; external winetdll name 'CommitUrlCacheEntryA';
function RetrieveUrlCacheEntryFile; external winetdll name 'RetrieveUrlCacheEntryFileA';
function UnlockUrlCacheEntryFile; external winetdll name 'UnlockUrlCacheEntryFileA';
function RetrieveUrlCacheEntryStream; external winetdll name 'RetrieveUrlCacheEntryStreamA';
function ReadUrlCacheEntryStream; external winetdll name 'ReadUrlCacheEntryStream';
function UnlockUrlCacheEntryStream; external winetdll name 'UnlockUrlCacheEntryStream';
function GetUrlCacheEntryInfoA; external winetdll name 'GetUrlCacheEntryInfoA';
function GetUrlCacheEntryInfoW; external winetdll name 'GetUrlCacheEntryInfoW';
function GetUrlCacheEntryInfo; external winetdll name 'GetUrlCacheEntryInfoA';
function FindFirstUrlCacheGroup; external winetdll name 'FindFirstUrlCacheGroup';
function FindNextUrlCacheGroup; external winetdll name 'FindNextUrlCacheGroup';
function GetUrlCacheGroupAttributeA; external winetdll name 'GetUrlCacheGroupAttributeA';
function SetUrlCacheGroupAttributeA; external winetdll name 'SetUrlCacheGroupAttributeA';
function GetUrlCacheEntryInfoExA; external winetdll name 'GetUrlCacheEntryInfoExA';
function SetUrlCacheEntryInfoA; external winetdll name 'SetUrlCacheEntryInfoA';
function GetUrlCacheGroupAttributeW; external winetdll name 'GetUrlCacheGroupAttributeW';
function SetUrlCacheGroupAttributeW; external winetdll name 'SetUrlCacheGroupAttributeW';
function GetUrlCacheEntryInfoExW; external winetdll name 'GetUrlCacheEntryInfoExW';
function SetUrlCacheEntryInfoW; external winetdll name 'SetUrlCacheEntryInfoW';
function GetUrlCacheGroupAttribute; external winetdll name 'GetUrlCacheGroupAttributeA';
function SetUrlCacheGroupAttribute; external winetdll name 'SetUrlCacheGroupAttributeA';
function GetUrlCacheEntryInfoEx; external winetdll name 'GetUrlCacheEntryInfoExA';
function SetUrlCacheEntryInfo; external winetdll name 'SetUrlCacheEntryInfoA';
function CreateUrlCacheGroup; external winetdll name 'CreateUrlCacheGroup';
function DeleteUrlCacheGroup; external winetdll name 'DeleteUrlCacheGroup';
function SetUrlCacheEntryGroupA; external winetdll name 'SetUrlCacheEntryGroupA';
function FindFirstUrlCacheEntryExA; external winetdll name 'FindFirstUrlCacheEntryExA';
function FindNextUrlCacheEntryExA; external winetdll name 'FindNextUrlCacheEntryExA';
function FindFirstUrlCacheEntryA; external winetdll name 'FindFirstUrlCacheEntryA';
function FindNextUrlCacheEntryA; external winetdll name 'FindNextUrlCacheEntryA';
function SetUrlCacheEntryGroupW; external winetdll name 'SetUrlCacheEntryGroupW';
function FindFirstUrlCacheEntryExW; external winetdll name 'FindFirstUrlCacheEntryExW';
function FindNextUrlCacheEntryExW; external winetdll name 'FindNextUrlCacheEntryExW';
function FindFirstUrlCacheEntryW; external winetdll name 'FindFirstUrlCacheEntryW';
function FindNextUrlCacheEntryW; external winetdll name 'FindNextUrlCacheEntryW';
function SetUrlCacheEntryGroup; external winetdll name 'SetUrlCacheEntryGroupA';
function FindFirstUrlCacheEntryEx; external winetdll name 'FindFirstUrlCacheEntryExA';
function FindNextUrlCacheEntryEx; external winetdll name 'FindNextUrlCacheEntryExA';
function FindFirstUrlCacheEntry; external winetdll name 'FindFirstUrlCacheEntryA';
function FindNextUrlCacheEntry; external winetdll name 'FindNextUrlCacheEntryA';
function FindCloseUrlCache; external winetdll name 'FindCloseUrlCache';
function DeleteUrlCacheEntryA; external winetdll name 'DeleteUrlCacheEntryA';
function InternetDialA; external winetdll name 'InternetDialA';
function DeleteUrlCacheEntryW; external winetdll name 'DeleteUrlCacheEntryW';
function InternetDialW; external winetdll name 'InternetDialW';
function DeleteUrlCacheEntry; external winetdll name 'DeleteUrlCacheEntryA';
function InternetDial; external winetdll name 'InternetDialA';
function InternetHangUp; external winetdll name 'InternetHangUp';
function InternetGoOnlineA; external winetdll name 'InternetGoOnlineA';
function InternetGoOnlineW; external winetdll name 'InternetGoOnlineW';
function InternetGoOnline; external winetdll name 'InternetGoOnlineA';
function InternetAutodial; external winetdll name 'InternetAutodial';
function InternetAutodialHangup; external winetdll name 'InternetAutodialHangup';
function InternetGetConnectedState; external winetdll name 'InternetGetConnectedState';
function InternetGetConnectedStateExA; external winetdll name 'InternetGetConnectedStateExA';
function InternetGetConnectedStateExW; external winetdll name 'InternetGetConnectedStateExW';
function InternetGetConnectedStateEx; external winetdll name 'InternetGetConnectedStateExA';
function InternetInitializeAutoProxyDll; external winetdll name 'InternetInitializeAutoProxyDll';
function InternetSetDialStateA; external winetdll name 'InternetSetDialStateA';
function InternetOpenA; external winetdll name 'InternetOpenA';
function InternetSetDialStateW; external winetdll name 'InternetSetDialStateW';
function InternetOpenW; external winetdll name 'InternetOpenW';
function InternetSetDialState; external winetdll name 'InternetSetDialStateA';
function InternetOpen; external winetdll name 'InternetOpenA';

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
