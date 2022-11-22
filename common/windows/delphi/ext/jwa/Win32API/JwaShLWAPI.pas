{******************************************************************************}
{                                                                              }
{ Shell Light Weight API Interface Unit for Object Pascal                      }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2005 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The initial developer of the original translation is Rudy Velthuis		       }
{                                                                              }
{ Portions created by Rudy Velthuis are Copyright (C) 2005-2008                }
{ All Rights Reserved.                                      				           }
{                                                                              }
{ Adapted for JEDI API Library by Christian Wimmer                             }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{ The original code is: shlwapi.h, released 2005.                			         }
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
{                                                                              }
{******************************************************************************}
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaShLWAPI;
{$I ..\Includes\JediAPILib.inc}


interface



{$HPPEMIT '#include <shlwapi.h>'}
{$HPPEMIT ''}
{$HPPEMIT 'interface DECLSPEC_UUID("C46CA590-3C3F-11D2-BEE6-0000F805CA57") IQueryAssociations;'}
{$HPPEMIT 'typedef System::DelphiInterface<IQueryAssociations> _di_IQueryAssociations;'}

{$IFDEF DELPHI6_UP}
{$ALIGN 8}
{$ELSE}
{$A+}
//Warning: Record alignment 4
{$ENDIF DELPHI6_UP}

uses
  Windows, CommCtrl,ActiveX,
  {$IFDEF DELPHI6_UP}msxml,{$ENDIF DELPHI6_UP}
  JwaWinBase, JwaWinUser, JwaWinType,
  JwaShlObj, JwaActiveX, JwaWinReg;

{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}

{$EXTERNALSYM StrChrA}
function StrChrA(lpStart: PAnsiChar; wMatch: Word): PAnsiChar; stdcall;
{$EXTERNALSYM StrChrW}
function StrChrW(lpStart: PWideChar; wMatch: WideChar): PWideChar; stdcall;
{$EXTERNALSYM StrChr}
function StrChr(lpStart: PTSTR; wMatch: Word): PTSTR; stdcall;
{$EXTERNALSYM StrChrIA}
function StrChrIA(lpStart: PAnsiChar; wMatch: Word): PAnsiChar; stdcall;
{$EXTERNALSYM StrChrIW}
function StrChrIW(lpStart: PWideChar; wMatch: WideChar): PWideChar; stdcall;
{$EXTERNALSYM StrChrI}
function StrChrI(lpStart: PTSTR; wMatch: Word): PTSTR; stdcall;
{$EXTERNALSYM StrCmpNA}
function StrCmpNA(lpStr1, lpStr2: PAnsiChar; nChar: Integer): Integer; stdcall;
{$EXTERNALSYM StrCmpNW}
function StrCmpNW(lpStr1, lpStr2: PWideChar; nChar: Integer): Integer; stdcall;
{$EXTERNALSYM StrCmpN}
function StrCmpN(lpStr1, lpStr2: PTSTR; nChar: Integer): Integer; stdcall;
{$EXTERNALSYM StrCmpNIA}
function StrCmpNIA(lpStr1, lpStr2: PAnsiChar; nChar: Integer): Integer; stdcall;
{$EXTERNALSYM StrCmpNIW}
function StrCmpNIW(lpStr1, lpStr2: PWideChar; nChar: Integer): Integer; stdcall;
{$EXTERNALSYM StrCmpNI}
function StrCmpNI(lpStr1, lpStr2: PTSTR; nChar: Integer): Integer; stdcall;
{$EXTERNALSYM StrCSpnA}
function StrCSpnA(lpStr, lpSet: PAnsiChar): Integer; stdcall;
{$EXTERNALSYM StrCSpnW}
function StrCSpnW(lpStr, lpSet: PWideChar): Integer; stdcall;
{$EXTERNALSYM StrCSpn}
function StrCSpn(lpStr, lpSet: PTSTR): Integer; stdcall;
{$EXTERNALSYM StrCSpnIA}
function StrCSpnIA(lpStr, lpSet: PAnsiChar): Integer; stdcall;
{$EXTERNALSYM StrCSpnIW}
function StrCSpnIW(lpStr, lpSet: PWideChar): Integer; stdcall;
{$EXTERNALSYM StrCSpnI}
function StrCSpnI(lpStr, lpSet: PTSTR): Integer; stdcall;
{$EXTERNALSYM StrDupA}
function StrDupA(lpSrch: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM StrDupW}
function StrDupW(lpSrch: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM StrDup}
function StrDup(lpSrch: PTSTR): PTSTR; stdcall;
{$EXTERNALSYM StrFormatByteSizeA}
function StrFormatByteSizeA(dw: DWORD; szBuf: PAnsiChar; uiBufSize: UINT): PAnsiChar; stdcall;
{$EXTERNALSYM StrFormatByteSize64A}
function StrFormatByteSize64A(qdw: LONGLONG; szBuf: PAnsiChar; uiBufSize: UINT): PAnsiChar; stdcall;
{$EXTERNALSYM StrFormatByteSizeW}
function StrFormatByteSizeW(qdw: LONGLONG; szBuf: PWideChar; uiBufSize: UINT): PWideChar; stdcall;
{$EXTERNALSYM StrFormatByteSize}
function StrFormatByteSize(dw: DWORD; szBuf: PTSTR; uiBufSize: UINT): PTSTR; stdcall;
{$EXTERNALSYM StrFormatByteSize64}
function StrFormatByteSize64(qdw: LONGLONG; szBuf: PTSTR; uiBufSize: UINT): PTSTR; stdcall;
{$EXTERNALSYM StrFormatKBSizeW}
function StrFormatKBSizeW(qdw: LONGLONG; szBuf: PWideChar; uiBufSize: UINT): PWideChar; stdcall;
{$EXTERNALSYM StrFormatKBSizeA}
function StrFormatKBSizeA(qdw: LONGLONG; szBuf: PAnsiChar; uiBufSize: UINT): PAnsiChar; stdcall;
{$EXTERNALSYM StrFormatKBSize}
function StrFormatKBSize(qdw: LONGLONG; szBuf: PTSTR; uiBufSize: UINT): PTSTR; stdcall;
{$EXTERNALSYM StrFromTimeIntervalA}
function StrFromTimeIntervalA(pszOut: PAnsiChar; cchMax: UINT; dwTimeMS: DWORD; digits: Integer): Integer; stdcall;
{$EXTERNALSYM StrFromTimeIntervalW}
function StrFromTimeIntervalW(pszOut: PWideChar; cchMax: UINT; dwTimeMS: DWORD; digits: Integer): Integer; stdcall;
{$EXTERNALSYM StrFromTimeInterval}
function StrFromTimeInterval(pszOut: PTSTR; cchMax: UINT; dwTimeMS: DWORD; digits: Integer): Integer; stdcall;
{$EXTERNALSYM StrIsIntlEqualA}
function StrIsIntlEqualA(fCaseSens: BOOL; lpString1, lpString2: PAnsiChar; nChar: Integer): BOOL; stdcall;
{$EXTERNALSYM StrIsIntlEqualW}
function StrIsIntlEqualW(fCaseSens: BOOL; lpString1, lpString2: PWideChar; nChar: Integer): BOOL; stdcall;
{$EXTERNALSYM StrIsIntlEqual}
function StrIsIntlEqual(fCaseSens: BOOL; lpString1, lpString2: PTSTR; nChar: Integer): BOOL; stdcall;
{$EXTERNALSYM StrNCatA}
function StrNCatA(psz1, psz2: PAnsiChar; cchMax: Integer): PAnsiChar; stdcall;
{$EXTERNALSYM StrNCatW}
function StrNCatW(psz1, psz2: PWideChar; cchMax: Integer): PWideChar; stdcall;
{$EXTERNALSYM StrNCat}
function StrNCat(psz1, psz2: PTSTR; cchMax: Integer): PTSTR; stdcall;
{$EXTERNALSYM StrPBrkA}
function StrPBrkA(psz, pszSet: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM StrPBrkW}
function StrPBrkW(psz, pszSet: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM StrPBrk}
function StrPBrk(psz, pszSet: PTSTR): PTSTR; stdcall;
{$EXTERNALSYM StrRChrA}
function StrRChrA(lpStart, lpEnd: PAnsiChar; wMatch: Word): PAnsiChar; stdcall;
{$EXTERNALSYM StrRChrW}
function StrRChrW(lpStart, lpEnd: PWideChar; wMatch: WideChar): PWideChar; stdcall;
{$EXTERNALSYM StrRChr}
function StrRChr(lpStart, lpEnd: PTSTR; wMatch: Word): PTSTR; stdcall;
{$EXTERNALSYM StrRChrIA}
function StrRChrIA(lpStart, lpEnd: PAnsiChar; wMatch: Word): PAnsiChar; stdcall;
{$EXTERNALSYM StrRChrIW}
function StrRChrIW(lpStart, lpEnd: PWideChar; wMatch: WideChar): PWideChar; stdcall;
{$EXTERNALSYM StrRChrI}
function StrRChrI(lpStart, lpEnd: PTSTR; wMatch: Word): PTSTR; stdcall;
{$EXTERNALSYM StrRStrIA}
function StrRStrIA(lpSource, lpLast, lpSrch: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM StrRStrIW}
function StrRStrIW(lpSource, lpLast, lpSrch: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM StrRStrI}
function StrRStrI(lpSource, lpLast, lpSrch: PTSTR): PTSTR; stdcall;
{$EXTERNALSYM StrSpnA}
function StrSpnA(psz, pszSet: PAnsiChar): Integer; stdcall;
{$EXTERNALSYM StrSpnW}
function StrSpnW(psz, pszSet: PWideChar): Integer; stdcall;
{$EXTERNALSYM StrSpn}
function StrSpn(psz, pszSet: PTSTR): Integer; stdcall;
{$EXTERNALSYM StrStrA}
function StrStrA(lpFirst, lpSrch: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM StrStrW}
function StrStrW(lpFirst, lpSrch: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM StrStr}
function StrStr(lpFirst, lpSrch: PTSTR): PTSTR; stdcall;
{$EXTERNALSYM StrStrIA}
function StrStrIA(lpFirst, lpSrch: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM StrStrIW}
function StrStrIW(lpFirst, lpSrch: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM StrStrI}
function StrStrI(lpFirst, lpSrch: PTSTR): PTSTR; stdcall;
{$EXTERNALSYM StrToIntA}
function StrToIntA(lpSrc: PAnsiChar): Integer; stdcall;
{$EXTERNALSYM StrToIntW}
function StrToIntW(lpSrc: PWideChar): Integer; stdcall;
{$EXTERNALSYM StrToInt}
function StrToInt(lpSrc: PTSTR): Integer; stdcall;
{$EXTERNALSYM StrToIntExA}
function StrToIntExA(pszString: PAnsiChar; dwFlags: DWORD; var piRet: Integer): BOOL; stdcall;
{$EXTERNALSYM StrToIntExW}
function StrToIntExW(pszString: PWideChar; dwFlags: DWORD; var piRet: Integer): BOOL; stdcall;
{$EXTERNALSYM StrToIntEx}
function StrToIntEx(pszString: PTSTR; dwFlags: DWORD; var piRet: Integer): BOOL; stdcall;

 {$EXTERNALSYM StrToInt64ExA}
function StrToInt64ExA(pszString: PAnsiChar; dwFlags: DWORD; var pllRet: LONGLONG): BOOL stdcall;
{$EXTERNALSYM StrToInt64ExW}
function StrToInt64ExW(pszString: PWideChar; dwFlags: DWORD; var pllRet: LONGLONG): BOOL stdcall;
{$EXTERNALSYM StrToInt64Ex}
function StrToInt64Ex(pszString: PTSTR; dwFlags: DWORD; var pllRet: LONGLONG): BOOL stdcall;

{$EXTERNALSYM StrTrimA}
function StrTrimA(psz, pszTrimChars: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM StrTrimW}
function StrTrimW(psz, pszTrimChars: PWideChar): BOOL; stdcall;
{$EXTERNALSYM StrTrim}
function StrTrim(psz, pszTrimChars: PTSTR): BOOL; stdcall;

{$EXTERNALSYM StrCatW}
function StrCatW(psz1, psz2: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM StrCmpW}
function StrCmpW(psz1, psz2: PWideChar): Integer; stdcall;
{$EXTERNALSYM StrCmpIW}
function StrCmpIW(psz1, psz2: PWideChar): Integer; stdcall;
{$EXTERNALSYM StrCpyW}
function StrCpyW(psz1, psz2: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM StrCpyNW}
function StrCpyNW(psz1, psz2: PWideChar; cchMax: Integer): PWideChar; stdcall;

{$EXTERNALSYM StrCatBuffW}
function StrCatBuffW(pszDest, pszSrc: PWideChar; cchDestBuffSize: Integer): PWideChar; stdcall;
{$EXTERNALSYM StrCatBuffA}
function StrCatBuffA(pszDest, pszSrc: PAnsiChar; cchDestBuffSize: Integer): PAnsiChar; stdcall;
{$EXTERNALSYM StrCatBuff}
function StrCatBuff(pszDest, pszSrc: PTSTR; cchDestBuffSize: Integer): PTSTR; stdcall;

{$EXTERNALSYM ChrCmpIA}
function ChrCmpIA(w1, w2: Word): BOOL; stdcall;
{$EXTERNALSYM ChrCmpIW}
function ChrCmpIW(w1, w2: WideChar): BOOL; stdcall;
{$EXTERNALSYM ChrCmpI}
function ChrCmpI(w1, w2: Word): BOOL; stdcall;

{$EXTERNALSYM wvnsprintfA}
function wvnsprintfA(lpOut: PAnsiChar; cchLimitIn: Integer; lpFmt: PAnsiChar; arglist: Pointer): Integer; stdcall;
{$EXTERNALSYM wvnsprintfW}
function wvnsprintfW(lpOut: PWideChar; cchLimitIn: Integer; lpFmt: PWideChar; arglist: Pointer): Integer; stdcall;
{$EXTERNALSYM wvnsprintf}
function wvnsprintf(lpOut: PTSTR; cchLimitIn: Integer; lpFmt: PTSTR; arglist: Pointer): Integer; stdcall;
{$IFDEF DELPHI6_UP}
{$EXTERNALSYM wnsprintfA}
function wnsprintfA(lpOut: PAnsiChar; cchLimitIn: Integer; lpFmt: PAnsiChar): Integer; cdecl; varargs;
{$EXTERNALSYM wnsprintfW}
function wnsprintfW(lpOut: PWideChar; cchLimitIn: Integer; lpFmt: PWideChar): Integer; cdecl; varargs;
{$EXTERNALSYM wnsprintf}
function wnsprintf(lpOut: PTSTR; cchLimitIn: Integer; lpFmt: PTSTR): Integer; cdecl; varargs;
{$ENDIF DELPHI6_UP}

{$EXTERNALSYM StrIntlEqNA}
function StrIntlEqNA(s1, s2: PAnsiChar; nChar: Integer): BOOL;
{$EXTERNALSYM StrIntlEqNW}
function StrIntlEqNW(s1, s2: PWideChar; nChar: Integer): BOOL;
{$EXTERNALSYM StrIntlEqN}
function StrIntlEqN(s1, s2: PTSTR; nChar: Integer): BOOL;
{$EXTERNALSYM StrIntlEqNIA}
function StrIntlEqNIA(s1, s2: PAnsiChar; nChar: Integer): BOOL;
{$EXTERNALSYM StrIntlEqNIW}
function StrIntlEqNIW(s1, s2: PWideChar; nChar: Integer): BOOL;
{$EXTERNALSYM StrIntlEqNI}
function StrIntlEqNI(s1, s2: PTSTR; nChar: Integer): BOOL;

{$EXTERNALSYM StrRetToStrA}
function StrRetToStrA(var pstr: TStrRet; pidl: PItemIDList; var ppsz: PAnsiChar): HResult; stdcall;
{$EXTERNALSYM StrRetToStrW}
function StrRetToStrW(var pstr: TStrRet; pidl: PItemIDList; var ppsz: PWideChar): HResult; stdcall;
{$EXTERNALSYM StrRetToStr}
function StrRetToStr(var pstr: TStrRet; pidl: PItemIDList; var ppsz: PTSTR): HResult; stdcall;
{$EXTERNALSYM StrRetToBufA}
function StrRetToBufA(var pstr: TStrRet; pidl: PItemIDList; pszBuf: PAnsiChar; cchBuf: UINT): HResult; stdcall;
{$EXTERNALSYM StrRetToBufW}
function StrRetToBufW(var pstr: TStrRet; pidl: PItemIDList; pszBuf: PWideChar; cchBuf: UINT): HResult; stdcall;
{$EXTERNALSYM StrRetToBuf}
function StrRetToBuf(var pstr: TStrRet; pidl: PItemIDList; pszBuf: PTSTR; cchBuf: UINT): HResult; stdcall;
{$EXTERNALSYM StrRetToBSTR}
function StrRetToBSTR(var pstr: TStrRet; pidl: PItemIDList; var pbstr: TBStr): HResult; stdcall;

// helper to duplicate a string using the task allocator

{$EXTERNALSYM SHStrDupA}
function SHStrDupA(psz: PAnsiChar; var pwsz: PWideChar): HResult; stdcall;
{$EXTERNALSYM SHStrDupW}
function SHStrDupW(psz: PWideChar; var pwsz: PWideChar): HResult; stdcall;
{$EXTERNALSYM SHStrDup}
function SHStrDup(psz: PTSTR; var pwsz: PTSTR): HResult; stdcall;

{$EXTERNALSYM StrCmpLogicalW}
function StrCmpLogicalW(psz1, psz2: PWideChar): Integer; stdcall;
{$EXTERNALSYM StrCatChainW}
function StrCatChainW(pszDst: PWideChar; cchDst, ichAt: DWORD; pszSrc: PWideChar): DWORD; stdcall;

{$EXTERNALSYM SHLoadIndirectString}
function SHLoadIndirectString(pszSource, pszOutBuf: PWideChar; cchOutBuf: UINT; var ppvReserved: Pointer): HResult; stdcall;

  {$EXTERNALSYM IsCharSpaceA}
  function IsCharSpaceA(wch: AnsiChar): BOOL stdcall;
  {$EXTERNALSYM IsCharSpaceW}
  function IsCharSpaceW(wch: WideChar): BOOL stdcall;
  {$EXTERNALSYM IsCharSpace}
  function IsCharSpace(wch: AnsiChar): BOOL; stdcall;

  {$EXTERNALSYM StrCmpCA}
  function StrCmpCA(pszStr1, pszStr2: PAnsiChar): Integer stdcall;
  {$EXTERNALSYM StrCmpCW}
  function StrCmpCW(pszStr1, pszStr2: PWideChar): Integer stdcall;
  {$EXTERNALSYM StrCmpC}
  function StrCmpC(pszStr1, pszStr2: PTSTR): Integer; stdcall;

  {$EXTERNALSYM StrCmpICA}
  function StrCmpICA(pszStr1, pszStr2: PAnsiChar): Integer stdcall;
  {$EXTERNALSYM StrCmpICW}
  function StrCmpICW(pszStr1, pszStr2: PWideChar): Integer stdcall;
  {$EXTERNALSYM StrCmpIC}
  function StrCmpIC(pszStr1, pszStr2: PTSTR): Integer; stdcall;


// Backward compatible to NT's non-standard naming (strictly
// for comctl32)
//
{$EXTERNALSYM IntlStrEqWorkerA}
function IntlStrEqWorkerA(fCaseSens: BOOL; lpString1, lpString2: PAnsiChar; nChar: Integer): BOOL; stdcall;
{$EXTERNALSYM IntlStrEqWorkerW}
function IntlStrEqWorkerW(fCaseSens: BOOL; lpString1, lpString2: PWideChar; nChar: Integer): BOOL; stdcall;

{$EXTERNALSYM IntlStrEqNA}
function IntlStrEqNA(s1, s2: PAnsiChar; nChar: Integer): BOOL;
{$EXTERNALSYM IntlStrEqNW}
function IntlStrEqNW(s1, s2: PWideChar; nChar: Integer): BOOL;
{$EXTERNALSYM IntlStrEqN}
function IntlStrEqN(s1, s2: PTSTR; nChar: Integer): BOOL;
{$EXTERNALSYM IntlStrEqNIA}
function IntlStrEqNIA(s1, s2: PAnsiChar; nChar: Integer): BOOL;
{$EXTERNALSYM IntlStrEqNIW}
function IntlStrEqNIW(s1, s2: PWideChar; nChar: Integer): BOOL;
{$EXTERNALSYM IntlStrEqNI}
function IntlStrEqNI(s1, s2: PTSTR; nChar: Integer): BOOL;

const
  {$EXTERNALSYM SZ_CONTENTTYPE_HTMLA}
  SZ_CONTENTTYPE_HTMLA       = 'text/html';
  {$EXTERNALSYM SZ_CONTENTTYPE_HTMLW}
  SZ_CONTENTTYPE_HTMLW       = 'text/html';
  {$EXTERNALSYM SZ_CONTENTTYPE_HTML}
  SZ_CONTENTTYPE_HTML        = 'text/html';
  {$EXTERNALSYM SZ_CONTENTTYPE_CDFA}
  SZ_CONTENTTYPE_CDFA        = 'application/x-cdf';
  {$EXTERNALSYM SZ_CONTENTTYPE_CDFW}
  SZ_CONTENTTYPE_CDFW        = 'application/x-cdf';
  {$EXTERNALSYM SZ_CONTENTTYPE_CDFA}
  SZ_CONTENTTYPE_CDF         = 'application/x-cdf';

{$EXTERNALSYM PathIsHTMLFileA}
function PathIsHTMLFileA(pszPath: PAnsiChar): BOOL;
{$EXTERNALSYM PathIsHTMLFileW}
function PathIsHTMLFileW(pszPath: PWideChar): BOOL;
{$EXTERNALSYM PathIsHTMLFile}
function PathIsHTMLFile(pszPath: PTSTR): BOOL;

// Flags for StrToIntEx
const
  {$EXTERNALSYM STIF_DEFAULT}
  STIF_DEFAULT        = $00000000;
  {$EXTERNALSYM STIF_SUPPORT_HEX}
  STIF_SUPPORT_HEX    = $00000001;

{$EXTERNALSYM StrCatA}
function StrCatA(lpString1, lpString2: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM StrCmpA}
function StrCmpA(lpString1, lpString2: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM StrCmpIA}
function StrCmpIA(lpString1, lpString2: PAnsiChar): Integer; stdcall;
{$EXTERNALSYM StrCpyA}
function StrCpyA(lpString1, lpString2: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM StrCpyNA}
function StrCpyNA(lpString1, lpString2: PAnsiChar; iMaxLength: Integer): PAnsiChar; stdcall;

{$EXTERNALSYM StrToLong}
function StrToLong(lpSrc: PTSTR): Integer; stdcall;
{.$EXTERNALSYM StrNCmp}
//function StrNCmp(lpStr1, lpStr2: PTSTR; nChar: Integer): Integer; stdcall;
{.$EXTERNALSYM StrNCmpI}
//function StrNCmpI(lpStr1, lpStr2: PTSTR; nChar: Integer): Integer; stdcall;
{$EXTERNALSYM StrNCpy}
function StrNCpy(lpString1, lpString2: PTSTR; iMaxLength: Integer): PTSTR; stdcall;

{.$EXTERNALSYM StrCatN}
//function StrCatN(psz1, psz2: PTSTR; cchMax: Integer): PTSTR; stdcall;

{$EXTERNALSYM StrCat}
function StrCat(lpString1, lpString2: PTSTR): PTSTR; stdcall;
{$EXTERNALSYM StrCmp}
function StrCmp(lpString1, lpString2: PTSTR): PTSTR; stdcall;
{$EXTERNALSYM StrCmpI}
function StrCmpI(lpString1, lpString2: PTSTR): Integer; stdcall;
{$EXTERNALSYM StrCpy}
function StrCpy(lpString1, lpString2: PTSTR): PTSTR; stdcall;
{$EXTERNALSYM StrCpyN}
function StrCpyN(lpString1, lpString2: PTSTR; iMaxLength: Integer): PTSTR; stdcall;

//
//=============== Path Routines ===================================
//

{$EXTERNALSYM PathAddBackslashA}
function PathAddBackslashA(pszPath: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM PathAddBackslashW}
function PathAddBackslashW(pszPath: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM PathAddBackslash}
function PathAddBackslash(pszPath: PTSTR): PTSTR; stdcall;
{$EXTERNALSYM PathAddExtensionA}
function PathAddExtensionA(pszPath, pszExt: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathAddExtensionW}
function PathAddExtensionW(pszPath, pszExt: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathAddExtension}
function PathAddExtension(pszPath, pszExt: PTSTR): BOOL; stdcall;
{$EXTERNALSYM PathAppendA}
function PathAppendA(pszPath, pMore: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathAppendW}
function PathAppendW(pszPath, pMore: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathAppend}
function PathAppend(pszPath, pMore: PTSTR): BOOL; stdcall;
{$EXTERNALSYM PathBuildRootA}
function PathBuildRootA(pszRoot: PAnsiChar; iDrive: Integer): PAnsiChar; stdcall;
{$EXTERNALSYM PathBuildRootW}
function PathBuildRootW(pszRoot: PWideChar; iDrive: Integer): PWideChar; stdcall;
{$EXTERNALSYM PathBuildRoot}
function PathBuildRoot(pszRoot: PTSTR; iDrive: Integer): PTSTR; stdcall;
{$EXTERNALSYM PathCanonicalizeA}
function PathCanonicalizeA(pszBuf, pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathCanonicalizeW}
function PathCanonicalizeW(pszBuf, pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathCanonicalize}
function PathCanonicalize(pszBuf, pszPath: PTSTR): BOOL; stdcall;
{$EXTERNALSYM PathCombineA}
function PathCombineA(pszDest, pszDir, pszFile: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM PathCombineW}
function PathCombineW(pszDest, pszDir, pszFile: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM PathCombine}
function PathCombine(pszDest, pszDir, pszFile: PTSTR): PTSTR; stdcall;
{$EXTERNALSYM PathCompactPathA}
function PathCompactPathA(hDC: HDC; pszPath: PAnsiChar; dx: UINT): BOOL; stdcall;
{$EXTERNALSYM PathCompactPathW}
function PathCompactPathW(hDC: HDC; pszPath: PWideChar; dx: UINT): BOOL; stdcall;
{$EXTERNALSYM PathCompactPath}
function PathCompactPath(hDC: HDC; pszPath: PTSTR; dx: UINT): BOOL; stdcall;
{$EXTERNALSYM PathCompactPathExA}
function PathCompactPathExA(pszOut, pszSrc: PAnsiChar; cchMax: UINT; dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM PathCompactPathExW}
function PathCompactPathExW(pszOut, pszSrc: PWideChar; cchMax: UINT; dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM PathCompactPathEx}
function PathCompactPathEx(pszOut, pszSrc: PTSTR; cchMax: UINT; dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM PathCommonPrefixA}
function PathCommonPrefixA(pszFile1, pszFile2, achPath: PAnsiChar): Integer; stdcall;
{$EXTERNALSYM PathCommonPrefixW}
function PathCommonPrefixW(pszFile1, pszFile2, achPath: PWideChar): Integer; stdcall;
{$EXTERNALSYM PathCommonPrefix}
function PathCommonPrefix(pszFile1, pszFile2, achPath: PTSTR): Integer; stdcall;
{$EXTERNALSYM PathFileExistsA}
function PathFileExistsA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathFileExistsW}
function PathFileExistsW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathFileExists}
function PathFileExists(pszPath: PTSTR): BOOL; stdcall;
{$EXTERNALSYM PathFindExtensionA}
function PathFindExtensionA(pszPath: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM PathFindExtensionW}
function PathFindExtensionW(pszPath: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM PathFindExtension}
function PathFindExtension(pszPath: PTSTR): PTSTR; stdcall;
{$EXTERNALSYM PathFindFileNameA}
function PathFindFileNameA(pszPath: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM PathFindFileNameW}
function PathFindFileNameW(pszPath: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM PathFindFileName}
function PathFindFileName(pszPath: PTSTR): PTSTR; stdcall;
{$EXTERNALSYM PathFindNextComponentA}
function PathFindNextComponentA(pszPath: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM PathFindNextComponentW}
function PathFindNextComponentW(pszPath: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM PathFindNextComponent}
function PathFindNextComponent(pszPath: PTSTR): PTSTR; stdcall;
{$EXTERNALSYM PathFindOnPathA}
function PathFindOnPathA(pszPath: PAnsiChar; ppszOtherDirs: PPAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathFindOnPathW}
function PathFindOnPathW(pszPath: PWideChar; ppszOtherDirs: PPWideChar): BOOL; stdcall;
{$EXTERNALSYM PathFindOnPath}
function PathFindOnPath(pszPath: PTSTR; ppszOtherDirs: PPTSTR): BOOL; stdcall;
{$EXTERNALSYM PathGetArgsA}
function PathGetArgsA(pszPath: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM PathGetArgsW}
function PathGetArgsW(pszPath: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM PathGetArgs}
function PathGetArgs(pszPath: PTSTR): PTSTR; stdcall;
{$EXTERNALSYM PathFindSuffixArrayA}
function PathFindSuffixArrayA(pszPath: PAnsiChar; apszSuffix: PPAnsiChar; iArraySize: Integer): PAnsiChar; stdcall;
{$EXTERNALSYM PathFindSuffixArrayW}
function PathFindSuffixArrayW(pszPath: PWideChar; apszSuffix: PPWideChar; iArraySize: Integer): PWideChar; stdcall;
{$EXTERNALSYM PathFindSuffixArray}
function PathFindSuffixArray(pszPath: PTSTR; apszSuffix: PPTSTR; iArraySize: Integer): PTSTR; stdcall;
{$EXTERNALSYM PathIsLFNFileSpecA}
function PathIsLFNFileSpecA(lpName: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsLFNFileSpecW}
function PathIsLFNFileSpecW(lpName: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsLFNFileSpec}
function PathIsLFNFileSpec(lpName: PTSTR): BOOL; stdcall;

{$EXTERNALSYM PathGetCharTypeA}
function PathGetCharTypeA(ch: Byte): UINT; stdcall;
{$EXTERNALSYM PathGetCharTypeW}
function PathGetCharTypeW(ch: WideChar): UINT; stdcall;
{$EXTERNALSYM PathGetCharType}
function PathGetCharType(ch: Byte): UINT; stdcall;

// Return flags for PathGetCharType
const
  {$EXTERNALSYM GCT_INVALID}
  GCT_INVALID             = $0000;
  {$EXTERNALSYM GCT_LFNCHAR}
  GCT_LFNCHAR             = $0001;
  {$EXTERNALSYM GCT_SHORTCHAR}
  GCT_SHORTCHAR           = $0002;
  {$EXTERNALSYM GCT_WILD}
  GCT_WILD                = $0004;
  {$EXTERNALSYM GCT_SEPARATOR}
  GCT_SEPARATOR           = $0008;

{$EXTERNALSYM PathGetDriveNumberA}
function PathGetDriveNumberA(pszPath: PAnsiChar): Integer; stdcall;
{$EXTERNALSYM PathGetDriveNumberW}
function PathGetDriveNumberW(pszPath: PWideChar): Integer; stdcall;
{$EXTERNALSYM PathGetDriveNumber}
function PathGetDriveNumber(pszPath: PTSTR): Integer; stdcall;
{$EXTERNALSYM PathIsDirectoryA}
function PathIsDirectoryA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsDirectoryW}
function PathIsDirectoryW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsDirectory}
function PathIsDirectory(pszPath: PTSTR): BOOL; stdcall;
{$EXTERNALSYM PathIsDirectoryEmptyA}
function PathIsDirectoryEmptyA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsDirectoryEmptyW}
function PathIsDirectoryEmptyW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsDirectoryEmpty}
function PathIsDirectoryEmpty(pszPath: PTSTR): BOOL; stdcall;
{$EXTERNALSYM PathIsFileSpecA}
function PathIsFileSpecA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsFileSpecW}
function PathIsFileSpecW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsFileSpec}
function PathIsFileSpec(pszPath: PTSTR): BOOL; stdcall;
{$EXTERNALSYM PathIsPrefixA}
function PathIsPrefixA(pszPrefix, pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsPrefixW}
function PathIsPrefixW(pszPrefix, pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsPrefix}
function PathIsPrefix(pszPrefix, pszPath: PTSTR): BOOL; stdcall;
{$EXTERNALSYM PathIsRelativeA}
function PathIsRelativeA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsRelativeW}
function PathIsRelativeW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsRelative}
function PathIsRelative(pszPath: PTSTR): BOOL; stdcall;
{$EXTERNALSYM PathIsRootA}
function PathIsRootA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsRootW}
function PathIsRootW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsRoot}
function PathIsRoot(pszPath: PTSTR): BOOL; stdcall;
{$EXTERNALSYM PathIsSameRootA}
function PathIsSameRootA(pszPath1, pszPath2: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsSameRootW}
function PathIsSameRootW(pszPath1, pszPath2: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsSameRoot}
function PathIsSameRoot(pszPath1, pszPath2: PTSTR): BOOL; stdcall;
{$EXTERNALSYM PathIsUNCA}
function PathIsUNCA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsUNCW}
function PathIsUNCW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsUNC}
function PathIsUNC(pszPath: PTSTR): BOOL; stdcall;
{$EXTERNALSYM PathIsNetworkPathA}
function PathIsNetworkPathA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsNetworkPathW}
function PathIsNetworkPathW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsNetworkPath}
function PathIsNetworkPath(pszPath: PTSTR): BOOL; stdcall;
{$EXTERNALSYM PathIsUNCServerA}
function PathIsUNCServerA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsUNCServerW}
function PathIsUNCServerW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsUNCServer}
function PathIsUNCServer(pszPath: PTSTR): BOOL; stdcall;
{$EXTERNALSYM PathIsUNCServerShareA}
function PathIsUNCServerShareA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsUNCServerShareW}
function PathIsUNCServerShareW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsUNCServerShare}
function PathIsUNCServerShare(pszPath: PTSTR): BOOL; stdcall;
{$EXTERNALSYM PathIsContentTypeA}
function PathIsContentTypeA(pszPath, pszContentType: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsContentTypeW}
function PathIsContentTypeW(pszPath, pszContentType: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsContentType}
function PathIsContentType(pszPath, pszContentType: PTSTR): BOOL; stdcall;
{$EXTERNALSYM PathIsURLA}
function PathIsURLA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsURLW}
function PathIsURLW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsURL}
function PathIsURL(pszPath: PTSTR): BOOL; stdcall;
{$EXTERNALSYM PathMakePrettyA}
function PathMakePrettyA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathMakePrettyW}
function PathMakePrettyW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathMakePretty}
function PathMakePretty(pszPath: PTSTR): BOOL; stdcall;
{$EXTERNALSYM PathMatchSpecA}
function PathMatchSpecA(pszFile, pszSpec: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathMatchSpecW}
function PathMatchSpecW(pszFile, pszSpec: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathMatchSpec}
function PathMatchSpec(pszFile, pszSpec: PTSTR): BOOL; stdcall;
{$EXTERNALSYM PathParseIconLocationA}
function PathParseIconLocationA(pszIconFile: PAnsiChar): Integer; stdcall;
{$EXTERNALSYM PathParseIconLocationW}
function PathParseIconLocationW(pszIconFile: PWideChar): Integer; stdcall;
{$EXTERNALSYM PathParseIconLocation}
function PathParseIconLocation(pszIconFile: PTSTR): Integer; stdcall;
{$EXTERNALSYM PathQuoteSpacesA}
procedure PathQuoteSpacesA(lpsz: PAnsiChar); stdcall;
{$EXTERNALSYM PathQuoteSpacesW}
procedure PathQuoteSpacesW(lpsz: PWideChar); stdcall;
{$EXTERNALSYM PathQuoteSpaces}
procedure PathQuoteSpaces(lpsz: PTSTR); stdcall;
{$EXTERNALSYM PathRelativePathToA}
function PathRelativePathToA(pszPath, pszFrom: PAnsiChar; dwAttrFrom: DWORD; pszTo: PAnsiChar; dwAttrTo: DWORD): BOOL; stdcall;
{$EXTERNALSYM PathRelativePathToW}
function PathRelativePathToW(pszPath, pszFrom: PWideChar; dwAttrFrom: DWORD; pszTo: PWideChar; dwAttrTo: DWORD): BOOL; stdcall;
{$EXTERNALSYM PathRelativePathTo}
function PathRelativePathTo(pszPath, pszFrom: PTSTR; dwAttrFrom: DWORD; pszTo: PTSTR; dwAttrTo: DWORD): BOOL; stdcall;
{$EXTERNALSYM PathRemoveArgsA}
procedure PathRemoveArgsA(pszPath: PAnsiChar); stdcall;
{$EXTERNALSYM PathRemoveArgsW}
procedure PathRemoveArgsW(pszPath: PWideChar); stdcall;
{$EXTERNALSYM PathRemoveArgs}
procedure PathRemoveArgs(pszPath: PTSTR); stdcall;
{$EXTERNALSYM PathRemoveBackslashA}
function PathRemoveBackslashA(pszPath: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM PathRemoveBackslashW}
function PathRemoveBackslashW(pszPath: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM PathRemoveBackslash}
function PathRemoveBackslash(pszPath: PTSTR): PTSTR; stdcall;
{$EXTERNALSYM PathRemoveBlanksA}
procedure PathRemoveBlanksA(pszPath: PAnsiChar); stdcall;
{$EXTERNALSYM PathRemoveBlanksW}
procedure PathRemoveBlanksW(pszPath: PWideChar); stdcall;
{$EXTERNALSYM PathRemoveBlanks}
procedure PathRemoveBlanks(pszPath: PTSTR); stdcall;
{$EXTERNALSYM PathRemoveExtensionA}
procedure PathRemoveExtensionA(pszPath: PAnsiChar); stdcall;
{$EXTERNALSYM PathRemoveExtensionW}
procedure PathRemoveExtensionW(pszPath: PWideChar); stdcall;
{$EXTERNALSYM PathRemoveExtension}
procedure PathRemoveExtension(pszPath: PTSTR); stdcall;
{$EXTERNALSYM PathRemoveFileSpecA}
function PathRemoveFileSpecA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathRemoveFileSpecW}
function PathRemoveFileSpecW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathRemoveFileSpec}
function PathRemoveFileSpec(pszPath: PTSTR): BOOL; stdcall;
{$EXTERNALSYM PathRenameExtensionA}
function PathRenameExtensionA(pszPath, pszExt: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathRenameExtensionW}
function PathRenameExtensionW(pszPath, pszExt: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathRenameExtension}
function PathRenameExtension(pszPath, pszExt: PTSTR): BOOL; stdcall;
{$EXTERNALSYM PathSearchAndQualifyA}
function PathSearchAndQualifyA(pszPath, pszBuf: PAnsiChar; cchBuf: UINT): BOOL; stdcall;
{$EXTERNALSYM PathSearchAndQualifyW}
function PathSearchAndQualifyW(pszPath, pszBuf: PWideChar; cchBuf: UINT): BOOL; stdcall;
{$EXTERNALSYM PathSearchAndQualify}
function PathSearchAndQualify(pszPath, pszBuf: PTSTR; cchBuf: UINT): BOOL; stdcall;
{$EXTERNALSYM PathSetDlgItemPathA}
procedure PathSetDlgItemPathA(hDlg: HWND; id: Integer; pszPath: PAnsiChar); stdcall;
{$EXTERNALSYM PathSetDlgItemPathW}
procedure PathSetDlgItemPathW(hDlg: HWND; id: Integer; pszPath: PWideChar); stdcall;
{$EXTERNALSYM PathSetDlgItemPath}
procedure PathSetDlgItemPath(hDlg: HWND; id: Integer; pszPath: PTSTR); stdcall;
{$EXTERNALSYM PathSkipRootA}
function PathSkipRootA(pszPath: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM PathSkipRootW}
function PathSkipRootW(pszPath: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM PathSkipRoot}
function PathSkipRoot(pszPath: PTSTR): PTSTR; stdcall;
{$EXTERNALSYM PathStripPathA}
procedure PathStripPathA(pszPath: PAnsiChar); stdcall;
{$EXTERNALSYM PathStripPathW}
procedure PathStripPathW(pszPath: PWideChar); stdcall;
{$EXTERNALSYM PathStripPath}
procedure PathStripPath(pszPath: PTSTR); stdcall;
{$EXTERNALSYM PathStripToRootA}
function PathStripToRootA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathStripToRootW}
function PathStripToRootW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathStripToRoot}
function PathStripToRoot(pszPath: PTSTR): BOOL; stdcall;
{$EXTERNALSYM PathUnquoteSpacesA}
procedure PathUnquoteSpacesA(lpsz: PAnsiChar); stdcall;
{$EXTERNALSYM PathUnquoteSpacesW}
procedure PathUnquoteSpacesW(lpsz: PWideChar); stdcall;
{$EXTERNALSYM PathUnquoteSpaces}
procedure PathUnquoteSpaces(lpsz: PTSTR); stdcall;
{$EXTERNALSYM PathMakeSystemFolderA}
function PathMakeSystemFolderA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathMakeSystemFolderW}
function PathMakeSystemFolderW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathMakeSystemFolder}
function PathMakeSystemFolder(pszPath: PTSTR): BOOL; stdcall;
{$EXTERNALSYM PathUnmakeSystemFolderA}
function PathUnmakeSystemFolderA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathUnmakeSystemFolderW}
function PathUnmakeSystemFolderW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathUnmakeSystemFolder}
function PathUnmakeSystemFolder(pszPath: PTSTR): BOOL; stdcall;
{$EXTERNALSYM PathIsSystemFolderA}
function PathIsSystemFolderA(pszPath: PAnsiChar; dwAttrb: DWORD): BOOL; stdcall;
{$EXTERNALSYM PathIsSystemFolderW}
function PathIsSystemFolderW(pszPath: PWideChar; dwAttrb: DWORD): BOOL; stdcall;
{$EXTERNALSYM PathIsSystemFolder}
function PathIsSystemFolder(pszPath: PTSTR; dwAttrb: DWORD): BOOL; stdcall;
{$EXTERNALSYM PathUndecorateA}
procedure PathUndecorateA(pszPath: PAnsiChar); stdcall;
{$EXTERNALSYM PathUndecorateW}
procedure PathUndecorateW(pszPath: PWideChar); stdcall;
{$EXTERNALSYM PathUndecorate}
procedure PathUndecorate(pszPath: PTSTR); stdcall;
{$EXTERNALSYM PathUnExpandEnvStringsA}
function PathUnExpandEnvStringsA(pszPath, pszBuf: PAnsiChar; cchBuf: UINT): BOOL; stdcall;
{$EXTERNALSYM PathUnExpandEnvStringsW}
function PathUnExpandEnvStringsW(pszPath, pszBuf: PWideChar; cchBuf: UINT): BOOL; stdcall;
{$EXTERNALSYM PathUnExpandEnvStrings}
function PathUnExpandEnvStrings(pszPath, pszBuf: PTSTR; cchBuf: UINT): BOOL; stdcall;

type
  {$EXTERNALSYM URL_SCHEME}
  URL_SCHEME = DWORD;
  TUrlScheme = DWORD;

const
  {$EXTERNALSYM URL_SCHEME_INVALID}
  URL_SCHEME_INVALID       = -1;
  {$EXTERNALSYM URL_SCHEME_UNKNOWN}
  URL_SCHEME_UNKNOWN       = 0;
  {$EXTERNALSYM URL_SCHEME_FTP}
  URL_SCHEME_FTP           = 1;
  {$EXTERNALSYM URL_SCHEME_HTTP}
  URL_SCHEME_HTTP          = 2;
  {$EXTERNALSYM URL_SCHEME_GOPHER}
  URL_SCHEME_GOPHER        = 3;
  {$EXTERNALSYM URL_SCHEME_MAILTO}
  URL_SCHEME_MAILTO        = 4;
  {$EXTERNALSYM URL_SCHEME_NEWS}
  URL_SCHEME_NEWS          = 5;
  {$EXTERNALSYM URL_SCHEME_NNTP}
  URL_SCHEME_NNTP          = 6;
  {$EXTERNALSYM URL_SCHEME_TELNET}
  URL_SCHEME_TELNET        = 7;
  {$EXTERNALSYM URL_SCHEME_WAIS}
  URL_SCHEME_WAIS          = 8;
  {$EXTERNALSYM URL_SCHEME_FILE}
  URL_SCHEME_FILE          = 9;
  {$EXTERNALSYM URL_SCHEME_MK}
  URL_SCHEME_MK            = 10;
  {$EXTERNALSYM URL_SCHEME_HTTPS}
  URL_SCHEME_HTTPS         = 11;
  {$EXTERNALSYM URL_SCHEME_SHELL}
  URL_SCHEME_SHELL         = 12;
  {$EXTERNALSYM URL_SCHEME_SNEWS}
  URL_SCHEME_SNEWS         = 13;
  {$EXTERNALSYM URL_SCHEME_LOCAL}
  URL_SCHEME_LOCAL         = 14;
  {$EXTERNALSYM URL_SCHEME_JAVASCRIPT}
  URL_SCHEME_JAVASCRIPT    = 15;
  {$EXTERNALSYM URL_SCHEME_VBSCRIPT}
  URL_SCHEME_VBSCRIPT      = 16;
  {$EXTERNALSYM URL_SCHEME_ABOUT}
  URL_SCHEME_ABOUT         = 17;
  {$EXTERNALSYM URL_SCHEME_RES}
  URL_SCHEME_RES           = 18;
  {$EXTERNALSYM URL_SCHEME_MSSHELLROOTED}
  URL_SCHEME_MSSHELLROOTED = 19;
  {$EXTERNALSYM URL_SCHEME_MSSHELLIDLIST}
  URL_SCHEME_MSSHELLIDLIST = 20;
  {$EXTERNALSYM URL_SCHEME_MSHELP}
  URL_SCHEME_MSHELP        = 21;
  {$EXTERNALSYM URL_SCHEME_MAXVALUE}
  URL_SCHEME_MAXVALUE      = 22;

type
  {$EXTERNALSYM URL_PART}
  URL_PART = DWORD;
  TUrlPart = DWORD;

const
  {$EXTERNALSYM URL_PART_NONE}
  URL_PART_NONE     = 0;
  {$EXTERNALSYM URL_PART_SCHEME}
  URL_PART_SCHEME   = 1;
  {$EXTERNALSYM URL_PART_HOSTNAME}
  URL_PART_HOSTNAME = 2;
  {$EXTERNALSYM URL_PART_USERNAME}
  URL_PART_USERNAME = 3;
  {$EXTERNALSYM URL_PART_PASSWORD}
  URL_PART_PASSWORD = 4;
  {$EXTERNALSYM URL_PART_PORT}
  URL_PART_PORT     = 5;
  {$EXTERNALSYM URL_PART_QUERY}
  URL_PART_QUERY    = 6;

  // Cannot declare this type as URLIS, since that is the name of a function as well.
  // This is not a problem in case sensitive C, but it is one in Delphi.

const
  {$EXTERNALSYM URLIS_URL}
  URLIS_URL        = 0;
  {$EXTERNALSYM URLIS_OPAQUE}
  URLIS_OPAQUE     = 1;
  {$EXTERNALSYM URLIS_NOHISTORY}
  URLIS_NOHISTORY  = 2;
  {$EXTERNALSYM URLIS_FILEURL}
  URLIS_FILEURL    = 3;
  {$EXTERNALSYM URLIS_APPLIABLE}
  URLIS_APPLIABLE  = 4;
  {$EXTERNALSYM URLIS_DIRECTORY}
  URLIS_DIRECTORY  = 5;
  {$EXTERNALSYM URLIS_HASQUERY}
  URLIS_HASQUERY   = 6;

  {$EXTERNALSYM URL_UNESCAPE}
  URL_UNESCAPE                    = $10000000;
  {$EXTERNALSYM URL_ESCAPE_UNSAFE}
  URL_ESCAPE_UNSAFE               = $20000000;
  {$EXTERNALSYM URL_PLUGGABLE_PROTOCOL}
  URL_PLUGGABLE_PROTOCOL          = $40000000;
  {$EXTERNALSYM URL_WININET_COMPATIBILITY}
  URL_WININET_COMPATIBILITY       = $80000000;
  {$EXTERNALSYM URL_DONT_ESCAPE_EXTRA_INFO}
  URL_DONT_ESCAPE_EXTRA_INFO      = $02000000;
  {$EXTERNALSYM URL_DONT_UNESCAPE_EXTRA_INFO}
  URL_DONT_UNESCAPE_EXTRA_INFO    = URL_DONT_ESCAPE_EXTRA_INFO;
  {$EXTERNALSYM URL_BROWSER_MODE}
  URL_BROWSER_MODE                = URL_DONT_ESCAPE_EXTRA_INFO;
  {$EXTERNALSYM URL_ESCAPE_SPACES_ONLY}
  URL_ESCAPE_SPACES_ONLY          = $04000000;
  {$EXTERNALSYM URL_DONT_SIMPLIFY}
  URL_DONT_SIMPLIFY               = $08000000;
  {$EXTERNALSYM URL_NO_META}
  URL_NO_META                     = URL_DONT_SIMPLIFY;
  {$EXTERNALSYM URL_UNESCAPE_INPLACE}
  URL_UNESCAPE_INPLACE            = $00100000;
  {$EXTERNALSYM URL_CONVERT_IF_DOSPATH}
  URL_CONVERT_IF_DOSPATH          = $00200000;
  {$EXTERNALSYM URL_UNESCAPE_HIGH_ANSI_ONLY}
  URL_UNESCAPE_HIGH_ANSI_ONLY     = $00400000;
  {$EXTERNALSYM URL_INTERNAL_PATH}
  URL_INTERNAL_PATH               = $00800000;  // Will escape #'s in paths
  {$EXTERNALSYM URL_FILE_USE_PATHURL}
  URL_FILE_USE_PATHURL            = $00010000;
  {$EXTERNALSYM URL_DONT_UNESCAPE}
  URL_DONT_UNESCAPE               = $00020000;  // Do not unescape the path/url at all
  {$EXTERNALSYM URL_ESCAPE_PERCENT}
  URL_ESCAPE_PERCENT              = $00001000;
  {$EXTERNALSYM URL_ESCAPE_SEGMENT_ONLY}
  URL_ESCAPE_SEGMENT_ONLY         = $00002000;  // Treat the entire URL param as one URL segment.

  {$EXTERNALSYM URL_PARTFLAG_KEEPSCHEME}
  URL_PARTFLAG_KEEPSCHEME         = $00000001;

  {$EXTERNALSYM URL_APPLY_DEFAULT}
  URL_APPLY_DEFAULT               = $00000001;
  {$EXTERNALSYM URL_APPLY_GUESSSCHEME}
  URL_APPLY_GUESSSCHEME           = $00000002;
  {$EXTERNALSYM URL_APPLY_GUESSFILE}
  URL_APPLY_GUESSFILE             = $00000004;
  {$EXTERNALSYM URL_APPLY_FORCEAPPLY}
  URL_APPLY_FORCEAPPLY            = $00000008;


{$EXTERNALSYM UrlCompareA}
function UrlCompareA(psz1, psz2: PAnsiChar; fIgnoreSlash: BOOL): Integer; stdcall;
{$EXTERNALSYM UrlCompareW}
function UrlCompareW(psz1, psz2: PWideChar; fIgnoreSlash: BOOL): Integer; stdcall;
{$EXTERNALSYM UrlCompare}
function UrlCompare(psz1, psz2: PTSTR; fIgnoreSlash: BOOL): Integer; stdcall;
{$EXTERNALSYM UrlCombineA}
function UrlCombineA(pszBase, pszRelative, pszCombined: PAnsiChar; var pcchCombined: DWORD; dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM UrlCombineW}
function UrlCombineW(pszBase, pszRelative, pszCombined: PWideChar; var pcchCombined: DWORD; dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM UrlCombine}
function UrlCombine(pszBase, pszRelative, pszCombined: PTSTR; var pcchCombined: DWORD; dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM UrlCanonicalizeA}
function UrlCanonicalizeA(pszUrl, pszCanonicalized: PAnsiChar; var pcchCanonicalized: DWORD; dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM UrlCanonicalizeW}
function UrlCanonicalizeW(pszUrl, pszCanonicalized: PWideChar; var pcchCanonicalized: DWORD; dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM UrlCanonicalize}
function UrlCanonicalize(pszUrl, pszCanonicalized: PTSTR; var pcchCanonicalized: DWORD; dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM UrlIsOpaqueA}
function UrlIsOpaqueA(pszURL: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM UrlIsOpaqueW}
function UrlIsOpaqueW(pszURL: PWideChar): BOOL; stdcall;
{$EXTERNALSYM UrlIsOpaque}
function UrlIsOpaque(pszURL: PTSTR): BOOL; stdcall;
{$EXTERNALSYM UrlIsNoHistoryA}
function UrlIsNoHistoryA(pszURL: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM UrlIsNoHistoryW}
function UrlIsNoHistoryW(pszURL: PWideChar): BOOL; stdcall;
{$EXTERNALSYM UrlIsNoHistory}
function UrlIsNoHistory(pszURL: PTSTR): BOOL; stdcall;
{$EXTERNALSYM UrlIsFileUrlA}
function UrlIsFileUrlA(pszURL: PAnsiChar): BOOL;
{$EXTERNALSYM UrlIsFileUrlW}
function UrlIsFileUrlW(pszURL: PWideChar): BOOL;
{$EXTERNALSYM UrlIsFileUrl}
function UrlIsFileUrl(pszURL: PTSTR): BOOL;
{$EXTERNALSYM UrlIsA}
function UrlIsA(pszUrl: PAnsiChar; UrlIs: DWORD): BOOL; stdcall;
{$EXTERNALSYM UrlIsW}
function UrlIsW(pszUrl: PWideChar; UrlIs: DWORD): BOOL; stdcall;
{$EXTERNALSYM UrlIs}
function UrlIs(pszUrl: PTSTR; UrlIs: DWORD): BOOL; stdcall;
{$EXTERNALSYM UrlGetLocationA}
function UrlGetLocationA(psz1: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM UrlGetLocationW}
function UrlGetLocationW(psz1: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM UrlGetLocation}
function UrlGetLocation(psz1: PTSTR): PTSTR; stdcall;
{$EXTERNALSYM UrlUnescapeA}
function UrlUnescapeA(pszUrl, pszUnescaped: PAnsiChar; pcchUnescaped: PDWORD; dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM UrlUnescapeW}
function UrlUnescapeW(pszUrl, pszUnescaped: PWideChar; pcchUnescaped: PDWORD; dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM UrlUnescape}
function UrlUnescape(pszUrl, pszUnescaped: PTSTR; pcchUnescaped: PDWORD; dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM UrlEscapeA}
function UrlEscapeA(pszUrl, pszEscaped: PAnsiChar; pcchEscaped: PDWORD; dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM UrlEscapeW}
function UrlEscapeW(pszUrl, pszEscaped: PWideChar; pcchEscaped: PDWORD; dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM UrlEscape}
function UrlEscape(pszUrl, pszEscaped: PTSTR; pcchEscaped: PDWORD; dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM UrlCreateFromPathA}
function UrlCreateFromPathA(pszPath, pszUrl: PAnsiChar; var pcchUrl: DWORD; dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM UrlCreateFromPathW}
function UrlCreateFromPathW(pszPath, pszUrl: PWideChar; var pcchUrl: DWORD; dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM UrlCreateFromPath}
function UrlCreateFromPath(pszPath, pszUrl: PTSTR; var pcchUrl: DWORD; dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM PathCreateFromUrlA}
function PathCreateFromUrlA(pszUrl, pszPath: PAnsiChar; var pcchPath: DWORD; dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM PathCreateFromUrlW}
function PathCreateFromUrlW(pszUrl, pszPath: PWideChar; var pcchPath: DWORD; dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM PathCreateFromUrl}
function PathCreateFromUrl(pszUrl, pszPath: PTSTR; var pcchPath: DWORD; dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM UrlHashA}
function UrlHashA(pszUrl: PAnsiChar; var pbHash: Byte; cbHash: DWORD): HResult; stdcall;
{$EXTERNALSYM UrlHashW}
function UrlHashW(pszUrl: PWideChar; var pbHash: Byte; cbHash: DWORD): HResult; stdcall;
{$EXTERNALSYM UrlHash}
function UrlHash(pszUrl: PTSTR; var pbHash: Byte; cbHash: DWORD): HResult; stdcall;
{$EXTERNALSYM UrlGetPartW}
function UrlGetPartW(pszIn, pszOut: PWideChar; var pcchOut: DWORD; dwPart, dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM UrlGetPartA}
function UrlGetPartA(pszIn, pszOut: PAnsiChar; var pcchOut: DWORD; dwPart, dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM UrlGetPart}
function UrlGetPart(pszIn, pszOut: PTSTR; var pcchOut: DWORD; dwPart, dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM UrlApplySchemeA}
function UrlApplySchemeA(pszIn, pszOut: PAnsiChar; var pcchOut: DWORD; dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM UrlApplySchemeW}
function UrlApplySchemeW(pszIn, pszOut: PWideChar; var pcchOut: DWORD; dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM UrlApplyScheme}
function UrlApplyScheme(pszIn, pszOut: PTSTR; var pcchOut: DWORD; dwFlags: DWORD): HResult; stdcall;
{$EXTERNALSYM HashData}
function HashData(pbData: PByte; cbData: DWORD; pbHash: PByte; cbHash: DWORD): HResult; stdcall;

{$EXTERNALSYM UrlEscapeSpaces}
function UrlEscapeSpaces(pszUrl, pszEscaped: PTSTR; var pcchEscaped: DWORD): HResult;
{$EXTERNALSYM UrlUnescapeInPlace}
function UrlUnescapeInPlace(pszUrl: PTSTR; dwFlags: DWORD): HResult;

//
//=============== Registry Routines ===================================
//


// SHDeleteEmptyKey mimics RegDeleteKey as it behaves on NT.
// SHDeleteKey mimics RegDeleteKey as it behaves on Win95.

{$EXTERNALSYM SHDeleteEmptyKeyA}
function SHDeleteEmptyKeyA(hkey: HKEY; pszSubKey: PAnsiChar): DWORD; stdcall;
{$EXTERNALSYM SHDeleteEmptyKeyW}
function SHDeleteEmptyKeyW(hkey: HKEY; pszSubKey: PWideChar): DWORD; stdcall;
{$EXTERNALSYM SHDeleteEmptyKey}
function SHDeleteEmptyKey(hkey: HKEY; pszSubKey: PTSTR): DWORD; stdcall;
{$EXTERNALSYM SHDeleteKeyA}
function SHDeleteKeyA(hkey: HKEY; pszSubKey: PAnsiChar): DWORD; stdcall;
{$EXTERNALSYM SHDeleteKeyW}
function SHDeleteKeyW(hkey: HKEY; pszSubKey: PWideChar): DWORD; stdcall;
{$EXTERNALSYM SHDeleteKey}
function SHDeleteKey(hkey: HKEY; pszSubKey: PTSTR): DWORD; stdcall;
{$EXTERNALSYM SHRegDuplicateHKey}
function SHRegDuplicateHKey(hkey: HKEY): HKEY; stdcall;


// These functions open the key, get/set/delete the value, then close
// the key.

{$EXTERNALSYM SHDeleteValueA}
function SHDeleteValueA(hkey: HKEY; pszSubKey, pszValue: PAnsiChar): DWORD; stdcall;
{$EXTERNALSYM SHDeleteValueW}
function SHDeleteValueW(hkey: HKEY; pszSubKey, pszValue: PWideChar): DWORD; stdcall;
{$EXTERNALSYM SHDeleteValue}
function SHDeleteValue(hkey: HKEY; pszSubKey, pszValue: PTSTR): DWORD; stdcall;
{$EXTERNALSYM SHGetValueA}
function SHGetValueA(hkey: HKEY; pszSubKey, pszValue: PAnsiChar; var pdwType: DWORD; pvData: Pointer; var pcbData: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHGetValueW}
function SHGetValueW(hkey: HKEY; pszSubKey, pszValue: PWideChar; var pdwType: DWORD; pvData: Pointer; var pcbData: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHGetValue}
function SHGetValue(hkey: HKEY; pszSubKey, pszValue: PTSTR; var pdwType: DWORD; pvData: Pointer; var pcbData: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHSetValueA}
function SHSetValueA(hkey: HKEY; pszSubKey: PAnsiChar; pszValue: PAnsiChar; dwType: DWORD; pvData: Pointer; cbData: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHSetValueW}
function SHSetValueW(hkey: HKEY; pszSubKey: PWideChar; pszValue: PWideChar; dwType: DWORD; pvData: Pointer; cbData: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHSetValue}
function SHSetValue(hkey: HKEY; pszSubKey: PTSTR; pszValue: PAnsiChar; dwType: DWORD; pvData: Pointer; cbData: DWORD): DWORD; stdcall;


//
// SRRF - Shell Registry Routine Flags (for SHRegGetValue)
//

type
  {$EXTERNALSYM SRRF}
  SRRF = DWORD;
  TSRRF = DWORD;

const
  {$EXTERNALSYM SRRF_RT_REG_NONE}
  SRRF_RT_REG_NONE        = $00000001;  // restrict type to REG_NONE      (other data types will not return ERROR_SUCCESS)
  {$EXTERNALSYM SRRF_RT_REG_SZ}
  SRRF_RT_REG_SZ          = $00000002;  // restrict type to REG_SZ        (other data types will not return ERROR_SUCCESS)
  {$EXTERNALSYM SRRF_RT_REG_EXPAND_SZ}
  SRRF_RT_REG_EXPAND_SZ   = $00000004;  // restrict type to REG_EXPAND_SZ (other data types will not return ERROR_SUCCESS)
  {$EXTERNALSYM SRRF_RT_REG_BINARY}
  SRRF_RT_REG_BINARY      = $00000008;  // restrict type to REG_BINARY    (other data types will not return ERROR_SUCCESS)
  {$EXTERNALSYM SRRF_RT_REG_DWORD}
  SRRF_RT_REG_DWORD       = $00000010;  // restrict type to REG_DWORD     (other data types will not return ERROR_SUCCESS)
  {$EXTERNALSYM SRRF_RT_REG_MULTI_SZ}
  SRRF_RT_REG_MULTI_SZ    = $00000020;  // restrict type to REG_MULTI_SZ  (other data types will not return ERROR_SUCCESS)
  {$EXTERNALSYM SRRF_RT_REG_QWORD}
  SRRF_RT_REG_QWORD       = $00000040;  // restrict type to REG_QWORD     (other data types will not return ERROR_SUCCESS)

  {$EXTERNALSYM SRRF_RT_DWORD}
  SRRF_RT_DWORD           = SRRF_RT_REG_BINARY or SRRF_RT_REG_DWORD; // restrict type to *32-bit* SRRF_RT_REG_BINARY or SRRF_RT_REG_DWORD (other data types will not return ERROR_SUCCESS)
  {$EXTERNALSYM SRRF_RT_QWORD}
  SRRF_RT_QWORD           = SRRF_RT_REG_BINARY or SRRF_RT_REG_QWORD; // restrict type to *64-bit* SRRF_RT_REG_BINARY or SRRF_RT_REG_DWORD (other data types will not return ERROR_SUCCESS)
  {$EXTERNALSYM SRRF_RT_ANY}
  SRRF_RT_ANY             = $0000FFFF;                               // no type restriction

  {$EXTERNALSYM SRRF_RM_ANY}
  SRRF_RM_ANY             = $00000000;  // no mode restriction (default is to allow any mode)
  {$EXTERNALSYM SRRF_RM_NORMAL}
  SRRF_RM_NORMAL          = $00010000;  // restrict system startup mode to "normal boot"               (other startup modes will not return ERROR_SUCCESS)
  {$EXTERNALSYM SRRF_RM_SAFE}
  SRRF_RM_SAFE            = $00020000;  // restrict system startup mode to "safe mode"                 (other startup modes will not return ERROR_SUCCESS)
  {$EXTERNALSYM SRRF_RM_SAFENETWORK}
  SRRF_RM_SAFENETWORK     = $00040000;  // restrict system startup mode to "safe mode with networking" (other startup modes will not return ERROR_SUCCESS)

  {$EXTERNALSYM SRRF_NOEXPAND}
  SRRF_NOEXPAND           = $10000000;  // do not automatically expand environment strings if value is of type REG_EXPAND_SZ
  {$EXTERNALSYM SRRF_ZEROONFAILURE}
  SRRF_ZEROONFAILURE      = $20000000;  // if pvData is not NULL, set content to all zeros on failure
  
// Function:
//
//  SHRegGetValue()
//
// Purpose:
//
//  Gets a registry value.  SHRegGetValue() provides the following benefits:
//
//  - data type checking
//  - boot mode checking
//  - auto-expansion of REG_EXPAND_SZ data
//  - guaranteed NULL termination of REG_SZ, REG_EXPAND_SZ, REG_MULTI_SZ data
//
// Parameters:
//
//  hkey        - handle to a currently open key.
//
//  pszSubKey   - pointer to a null-terminated string specifying the relative
//                path from hkey to one of its subkeys from which the data is
//                to be retrieved.  this will be opened with KEY_READ sam.
//
//                Note1: pszSubKey can be NULL or "".  In either of these two
//                       cases, the data is retrieved from the hkey itself.
//                Note2: *** PERF ***
//                       If pszSubKey is not NULL or "", the subkey will be
//                       automatically be opened and closed by this routine
//                       in order to obtain the data.  If you are retrieving
//                       multiple values from the same subkey, it is better
//                       for perf to open the subkey via RegOpenKeyEx() prior
//                       to calling this method, and using this opened key as
//                       hkey with pszSubKey set to NULL.
//
//  pszValue    - pointer to a null-terminated string specifying the name of
//                the value to query for data
//
//                Note1: pszValue can be NULL or "".  In either of these two
//                       cases, the data is retrieved from the unnamed or
//                       default value.
//
//  dwFlags     - bitwise or of SRRF_ flags, which cannot be 0:  at least one
//                type restriction must be specified (SRRF_RT_...), or if any
//                type is desired then SRRF_RT_ANY can be specified
//
//                Note1: SRRF_RT_ANY will allow any data type to be returned.
//                Note2: The following two type restrictions have special
//                       handling semantics:
//
//                         SRRF_RT_DWORD == SRRF_RT_REG_BINARY | SRRF_RT_REG_DWORD
//                         SRRF_RT_QWORD == SRRF_RT_REG_BINARY | SRRF_RT_REG_QWORD
//
//                       If either of these are specified, with no other type
//                       restrictions, then in the prior case the restriction
//                       will limit "valid" returned data to either REG_DWORD
//                       or 32-bit REG_BINARY data, and in the latter case
//                       the restriction will limit "valid" returned data to
//                       either REG_QWORD or 64-bit REG_BINARY.
//
//  pdwType     - pointer to a dword which receives a code indicating the
//                type of data stored in the specified value
//
//                Note1: pdwType can be NULL if no type information is wanted
//                Note2: If pdwType is not NULL, and the SRRF_NOEXPAND flag
//                       has not been set, data types of REG_EXPAND_SZ will
//                       be returned as REG_SZ since they are automatically
//                       expanded in this method.
//
//  pvData      - pointer to a buffer that receives the value's data
//
//                Note1: pvData can be NULL if the data is not required.
//                       pvData is usually NULL if doing either a simple
//                       existence test, or if interested in the size only.
//                Note2: *** PERF ***
//                       Reference 'perf' note for pcbData.
//
//  pcbData     - when pvData is NULL:
//                  optional pointer to a dword that receives a size in bytes
//                  which would be sufficient to hold the registry data (note
//                  this size is not guaranteed to be exact, merely sufficient)
//                when pvData is not NULL:
//                  required pointer to a dword that specifies the size in
//                  bytes of the buffer pointed to by the pvData parameter
//                  and receives a size in bytes of:
//                  a) the number of bytes read into pvData on ERROR_SUCCESS
//                     (note this size is guaranteed to be exact)
//                  b) the number of bytes which would be sufficient to hold
//                     the registry data on ERROR_MORE_DATA -- pvData was of
//                     insufficient size (note this size is not guaranteed to
//                     be exact, merely sufficient)
//
//                Note1: pcbData can be NULL only if pvData is NULL.
//                Note2: *** PERF ***
//                       The potential for an 'extra' call to the registry to
//                       read (or re-read) in the data exists when the data
//                       type is REG_EXPAND_SZ and the SRRF_NOEXPAND flag has
//                       not been set.  The following conditions will result
//                       in this 'extra' read operation:
//                       i)  when pvData is NULL and pcbData is not NULL
//                           we must read in the data from the registry
//                           anyway in order to obtain the string and perform
//                           an expand on it to obtain and return the total
//                           required size in pcbData
//                       ii) when pvData is not NULL but is of insufficient
//                           size we must re-read in the data from the
//                           registry in order to obtain the entire string
//                           and perform an expand on it to obtain and return
//                           the total required size in pcbData
//
// Remarks:
//
//  The key identified by hkey must have been opened with KEY_QUERY_VALUE
//  access.  If pszSubKey is not NULL or "", it must be able to be opened
//  with KEY_QUERY_VALUE access in the current calling context.
//
//  If the data type is REG_SZ, REG_EXPAND_SZ or REG_MULTI_SZ then any
//  returned data is guaranteed to take into account proper null termination.
//  For example:  if pcbData is not NULL, its returned size will include the
//  bytes for a null terminator  if pvData is not NULL, its returned data
//  will be properly null terminated.
//
//  If the data type is REG_EXPAND_SZ, then unless the SRRF_NOEXPAND flag
//  is set the data will be automatically expanded prior to being returned.
//  For example:  if pdwType is not NULL, its returned type will be changed
//  to REG_SZ,  if pcbData is not NULL, its returned size will include the
//  bytes for a properly expanded string.  if pvData is not NULL, its
//  returned data will be the expanded version of the string.
//
//  Reference MSDN documentation for RegQueryValueEx() for more information
//  of the behaviour when pdwType, pvData, and/or pcbData are equal to NULL.
//
// Return Values:
//
//  If the function succeeds, the return value is ERROR_SUCCESS and all out
//  parameters requested (pdwType, pvData, pcbData) are valid.
//
//  If the function fails due to insufficient space in a provided non-NULL
//  pvData, the return value is ERROR_MORE_DATA and only pdwType and pcbData
//  can contain valid data.  The content of pvData in this case is undefined.
//
// Examples:
//
//  1) read REG_SZ (or REG_EXPAND_SZ as REG_SZ) "string" data from the (default) value of an open hkey
//
//      TCHAR szData[128]
//      DWORD cbData = sizeof(pszData)
//      if (ERROR_SUCCESS == SHRegGetValue(hkey, NULL, NULL, SRRF_RT_REG_SZ, NULL, szData, &cbData))
//      {
//          // use sz (successful read)
//      }
//
//  2) read REG_SZ (or REG_EXPAND_SZ as REG_SZ) "string" data of unknown size from the "MyValue" value of an open hkey
//
//      DWORD cbData
//      if (ERROR_SUCCESS == SHRegGetValue(hkey, NULL, TEXT("MyValue"), SRRF_RT_REG_SZ, NULL, NULL, &cbData))
//      {
//          TCHAR *pszData = new TCHAR[cbData/sizeof(TCHAR)]
//          if (pszData)
//          {
//              if (ERROR_SUCCESS == SHRegGetValue(hkey, NULL, TEXT("MyValue"), SRRF_RT_REG_SZ, NULL, pszData, &cbData))
//              {
//                  // use pszData (successful read)
//              }
//              delete[] pszData
//          }
//      }
//
//  3) read "dword" data from the "MyValue" value of the "MySubKey" subkey of an open hkey
//
//      DWORD dwData
//      DWORD cbData = sizeof(dwData)
//      if (ERROR_SUCCESS == SHRegGetValue(hkey, TEXT("MySubKey"), TEXT("MyValue"), SRRF_RT_REG_DWORD, NULL, &dwData, &cbData))
//      {
//          // use dwData (successful read)
//      }
//
//  4) read "dword" data from the "MyValue" value of the "MySubKey" subkey of an open hkey (32-bit binary data also ok)
//
//      DWORD dwData
//      DWORD cbData = sizeof(dwData)
//      if (ERROR_SUCCESS == SHRegGetValue(hkey, TEXT("MySubKey"), TEXT("MyValue"), SRRF_RT_DWORD, NULL, &dwData, &cbData))
//      {
//          // use dwData (successful read)
//      }
//
//  5) determine existence of "MyValue" value of an open hkey
//
//      BOOL bExists = ERROR_SUCCESS == SHRegGetValue(hkey, NULL, TEXT("MyValue"), SRRF_RT_ANY, NULL, NULL, NULL)

{$EXTERNALSYM SHRegGetValueA}
function SHRegGetValueA(hkey: HKEY; pszSubKey, pszValue: PAnsiChar; dwFlags: TSRRF; var pdwType: DWORD; pvData: Pointer; var pcbData: DWORD): Longint stdcall;
{$EXTERNALSYM SHRegGetValueW}
function SHRegGetValueW(hkey: HKEY; pszSubKey, pszValue: PWideChar; dwFlags: TSRRF; var pdwType: DWORD; pvData: Pointer; var pcbData: DWORD): Longint stdcall;
{$EXTERNALSYM SHRegGetValue}
function SHRegGetValue(hkey: HKEY; pszSubKey, pszValue: PTSTR; dwFlags: TSRRF; var pdwType: DWORD; pvData: Pointer; var pcbData: DWORD): Longint stdcall;

// These functions behave just like RegQueryValueEx(), except if the data
// type is REG_SZ, REG_EXPAND_SZ or REG_MULTI_SZ then the string is
// guaranteed to be properly null terminated.
//
// Additionally, if the data type is REG_EXPAND_SZ these functions will
// go ahead and expand out the string, and "massage" the returned *pdwType
// to be REG_SZ.

{$EXTERNALSYM SHQueryValueExA}
function SHQueryValueExA(hkey: HKEY; pszValue: PAnsiChar; var pdwReserved, pdwType: DWORD; pvData: Pointer; var pcbData: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHQueryValueExW}
function SHQueryValueExW(hkey: HKEY; pszValue: PWideChar; var pdwReserved, pdwType: DWORD; pvData: Pointer; var pcbData: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHQueryValueEx}
function SHQueryValueEx(hkey: HKEY; pszValue: PTSTR; var pdwReserved, pdwType: DWORD; pvData: Pointer; var pcbData: DWORD): DWORD; stdcall;

// Enumeration functions support.

{$EXTERNALSYM SHEnumKeyExA}
function SHEnumKeyExA(hkey: HKEY; dwIndex: DWORD; pszName: PAnsiChar; var pcchName: DWORD): Longint; stdcall;
{$EXTERNALSYM SHEnumKeyExW}
function SHEnumKeyExW(hkey: HKEY; dwIndex: DWORD; pszName: PWideChar; var pcchName: DWORD): Longint; stdcall;
{$EXTERNALSYM SHEnumKeyEx}
function SHEnumKeyEx(hkey: HKEY; dwIndex: DWORD; pszName: PTSTR; var pcchName: DWORD): Longint; stdcall;
{$EXTERNALSYM SHEnumValueA}
function SHEnumValueA(hkey: HKEY; dwIndex: DWORD; pszValueName: PAnsiChar; var pcchValueName, pdwType: DWORD; pvData: Pointer; var pcbData: DWORD): Longint; stdcall;
{$EXTERNALSYM SHEnumValueW}
function SHEnumValueW(hkey: HKEY; dwIndex: DWORD; pszValueName: PWideChar; var pcchValueName, pdwType: DWORD; pvData: Pointer; var pcbData: DWORD): Longint; stdcall;
{$EXTERNALSYM SHEnumValue}
function SHEnumValue(hkey: HKEY; dwIndex: DWORD; pszValueName: PTSTR; var pcchValueName, pdwType: DWORD; pvData: Pointer; var pcbData: DWORD): Longint; stdcall;
{$EXTERNALSYM SHQueryInfoKeyA}
function SHQueryInfoKeyA(hkey: HKEY; var pcSubKeys, pcchMaxSubKeyLen, pcValues, pcchMaxValueNameLen: DWORD): Longint; stdcall;
{$EXTERNALSYM SHQueryInfoKeyW}
function SHQueryInfoKeyW(hkey: HKEY; var pcSubKeys, pcchMaxSubKeyLen, pcValues, pcchMaxValueNameLen: DWORD): Longint; stdcall;
{$EXTERNALSYM SHQueryInfoKey}
function SHQueryInfoKey(hkey: HKEY; var pcSubKeys, pcchMaxSubKeyLen, pcValues, pcchMaxValueNameLen: DWORD): Longint; stdcall;

// recursive key copy
{$EXTERNALSYM SHCopyKeyA}
function SHCopyKeyA(hkeySrc: HKEY; szSrcSubKey: PAnsiChar; hkeyDest: HKEY; fReserved: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHCopyKeyW}
function SHCopyKeyW(hkeySrc: HKEY; wszSrcSubKey: PWideChar; hkeyDest: HKEY; fReserved: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHCopyKey}
function SHCopyKey(hkeySrc: HKEY; szSrcSubKey: PTSTR; hkeyDest: HKEY; fReserved: DWORD): DWORD; stdcall;

// Getting and setting file system paths with environment variables

{$EXTERNALSYM SHRegGetPathA}
function SHRegGetPathA(hKey: HKEY; pcszSubKey, pcszValue, pszPath: PAnsiChar; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHRegGetPathW}
function SHRegGetPathW(hKey: HKEY; pcszSubKey, pcszValue, pszPath: PWideChar; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHRegGetPath}
function SHRegGetPath(hKey: HKEY; pcszSubKey, pcszValue, pszPath: PTSTR; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHRegSetPathA}
function SHRegSetPathA(hKey: HKEY; pcszSubKey, pcszValue, pcszPath: PAnsiChar; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHRegSetPathW}
function SHRegSetPathW(hKey: HKEY; pcszSubKey, pcszValue, pcszPath: PWideChar; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHRegSetPath}
function SHRegSetPath(hKey: HKEY; pcszSubKey, pcszValue, pcszPath: PTSTR; dwFlags: DWORD): DWORD; stdcall;


//////////////////////////////////////////////
// User Specific Registry Access Functions
//////////////////////////////////////////////

//
// Type definitions.
//

type
  {$EXTERNALSYM SHREGDEL_FLAGS}
  SHREGDEL_FLAGS = DWORD;
  TSHRegDelFlags = DWORD;

const
  {$EXTERNALSYM SHREGDEL_DEFAULT}
  SHREGDEL_DEFAULT = $00000000;   // Delete's HKCU ;  or HKLM if HKCU is not found.
  {$EXTERNALSYM SHREGDEL_HKCU}
  SHREGDEL_HKCU    = $00000001;   // Delete HKCU only
  {$EXTERNALSYM SHREGDEL_HKLM}
  SHREGDEL_HKLM    = $00000010;   // Delete HKLM only.
  {$EXTERNALSYM SHREGDEL_BOTH}
  SHREGDEL_BOTH    = $00000011;   // Delete both HKCU and HKLM.

type
  {$EXTERNALSYM SHREGENUM_FLAGS}
  SHREGENUM_FLAGS = DWORD;
  TSHRegEnumFlags = DWORD;

const
  {$EXTERNALSYM SHREGENUM_DEFAULT}
  SHREGENUM_DEFAULT = $00000000;   // Enumerates HKCU or HKLM if not found.
  {$EXTERNALSYM SHREGENUM_HKCU}
  SHREGENUM_HKCU    = $00000001;   // Enumerates HKCU only
  {$EXTERNALSYM SHREGENUM_HKLM}
  SHREGENUM_HKLM    = $00000010;   // Enumerates HKLM only.
  {$EXTERNALSYM SHREGENUM_BOTH}
  SHREGENUM_BOTH    = $00000011;   // Enumerates both HKCU and HKLM without duplicates.
                                   // This option is NYI.

  {$EXTERNALSYM SHREGSET_HKCU}
  SHREGSET_HKCU           = $00000001;       // Write to HKCU if empty.
  {$EXTERNALSYM SHREGSET_FORCE_HKCU}
  SHREGSET_FORCE_HKCU     = $00000002;       // Write to HKCU.
  {$EXTERNALSYM SHREGSET_HKLM}
  SHREGSET_HKLM           = $00000004;       // Write to HKLM if empty.
  {$EXTERNALSYM SHREGSET_FORCE_HKLM}
  SHREGSET_FORCE_HKLM     = $00000008;       // Write to HKLM.
  {$EXTERNALSYM SHREGSET_DEFAULT}
  SHREGSET_DEFAULT        = SHREGSET_FORCE_HKCU or SHREGSET_HKLM; // Default is SHREGSET_FORCE_HKCU or SHREGSET_HKLM.

type
  // HUSKEY is a Handle to a User Specific KEY.
  PHUSkey = ^THUSKey;
  {$EXTERNALSYM HUSKEY}
  HUSKEY = THandle;
  THUSKey = HUSKEY;

{$EXTERNALSYM SHRegCreateUSKeyA}
function SHRegCreateUSKeyA(pszPath: PAnsiChar; samDesired: REGSAM; hRelativeUSKey: THUSKey; var phNewUSKey: THUSKey; dwFlags: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegCreateUSKeyW}
function SHRegCreateUSKeyW(pwzPath: PWideChar; samDesired: REGSAM; hRelativeUSKey: THUSKey; var phNewUSKey: THUSKey; dwFlags: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegCreateUSKey}
function SHRegCreateUSKey(pszPath: PTSTR; samDesired: REGSAM; hRelativeUSKey: THUSKey; var phNewUSKey: THUSKey; dwFlags: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegOpenUSKeyA}
function SHRegOpenUSKeyA(pszPath: PAnsiChar; samDesired: REGSAM; hRelativeUSKey: THUSKey; var phNewUSKey: THUSKey; fIgnoreHKCU: BOOL): Longint; stdcall;
{$EXTERNALSYM SHRegOpenUSKeyW}
function SHRegOpenUSKeyW(pwzPath: PWideChar; samDesired: REGSAM; hRelativeUSKey: THUSKey; var phNewUSKey: THUSKey; fIgnoreHKCU: BOOL): Longint; stdcall;
{$EXTERNALSYM SHRegOpenUSKey}
function SHRegOpenUSKey(pszPath: PTSTR; samDesired: REGSAM; hRelativeUSKey: THUSKey; var phNewUSKey: THUSKey; fIgnoreHKCU: BOOL): Longint; stdcall;
{$EXTERNALSYM SHRegQueryUSValueA}
function SHRegQueryUSValueA(hUSKey: THUSKey; pszValue: PAnsiChar; var pdwType: DWORD; pvData: Pointer; var pcbData: DWORD; fIgnoreHKCU: BOOL; pvDefaultData: Pointer; dwDefaultDataSize: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegQueryUSValueW}
function SHRegQueryUSValueW(hUSKey: THUSKey; pwzValue: PWideChar; var pdwType: DWORD; pvData: Pointer; var pcbData: DWORD; fIgnoreHKCU: BOOL; pvDefaultData: Pointer; dwDefaultDataSize: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegQueryUSValue}
function SHRegQueryUSValue(hUSKey: THUSKey; pszValue: PTSTR; var pdwType: DWORD; pvData: Pointer; var pcbData: DWORD; fIgnoreHKCU: BOOL; pvDefaultData: Pointer; dwDefaultDataSize: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegWriteUSValueA}
function SHRegWriteUSValueA(hUSKey: THUSKey; pszValue: PAnsiChar; dwType: DWORD; pvData: Pointer; cbData, dwFlags: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegWriteUSValueW}
function SHRegWriteUSValueW(hUSKey: THUSKey; pwzValue: PWideChar; dwType: DWORD; pvData: Pointer; cbData, dwFlags: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegWriteUSValue}
function SHRegWriteUSValue(hUSKey: THUSKey; pszValue: PTSTR; dwType: DWORD; pvData: Pointer; cbData, dwFlags: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegDeleteUSValueA}
function SHRegDeleteUSValueA(hUSKey: THUSKey; pszValue: PAnsiChar; delRegFlags: TSHRegDelFlags): Longint; stdcall;
{$EXTERNALSYM SHRegDeleteUSValueW}
function SHRegDeleteUSValueW(hUSKey: THUSKey; pwzValue: PWideChar; delRegFlags: TSHRegDelFlags): Longint; stdcall;
{$EXTERNALSYM SHRegDeleteUSValue}
function SHRegDeleteUSValue(hUSKey: THUSKey; pszValue: PTSTR; delRegFlags: TSHRegDelFlags): Longint; stdcall;
{$EXTERNALSYM SHRegDeleteEmptyUSKeyW}
function SHRegDeleteEmptyUSKeyW(hUSKey: THUSKey; pwzSubKey: PWideChar; delRegFlags: TSHRegDelFlags): Longint; stdcall;
{$EXTERNALSYM SHRegDeleteEmptyUSKeyA}
function SHRegDeleteEmptyUSKeyA(hUSKey: THUSKey; pszSubKey: PAnsiChar; delRegFlags: TSHRegDelFlags): Longint; stdcall;
{$EXTERNALSYM SHRegDeleteEmptyUSKey}
function SHRegDeleteEmptyUSKey(hUSKey: THUSKey; pszSubKey: PTSTR; delRegFlags: TSHRegDelFlags): Longint; stdcall;
{$EXTERNALSYM SHRegEnumUSKeyA}
function SHRegEnumUSKeyA(hUSKey: THUSKey; dwIndex: DWORD; pszName: PAnsiChar; var pcchName: DWORD; enumRegFlags: TSHRegEnumFlags): Longint; stdcall;
{$EXTERNALSYM SHRegEnumUSKeyW}
function SHRegEnumUSKeyW(hUSKey: THUSKey; dwIndex: DWORD; pwzName: PWideChar; var pcchName: DWORD; enumRegFlags: TSHRegEnumFlags): Longint; stdcall;
{$EXTERNALSYM SHRegEnumUSKey}
function SHRegEnumUSKey(hUSKey: THUSKey; dwIndex: DWORD; pszName: PTSTR; var pcchName: DWORD; enumRegFlags: TSHRegEnumFlags): Longint; stdcall;
{$EXTERNALSYM SHRegEnumUSValueA}
function SHRegEnumUSValueA(hUSkey: THUSKey; dwIndex: DWORD; pszValueName: PAnsiChar; var pcchValueName, pdwType: DWORD; pvData: Pointer; var pcbData: DWORD; enumRegFlags: TSHRegEnumFlags): Longint; stdcall;
{$EXTERNALSYM SHRegEnumUSValueW}
function SHRegEnumUSValueW(hUSkey: THUSKey; dwIndex: DWORD; pszValueName: PWideChar; var pcchValueName, pdwType: DWORD; pvData: Pointer; var pcbData: DWORD; enumRegFlags: TSHRegEnumFlags): Longint; stdcall;
{$EXTERNALSYM SHRegEnumUSValue}
function SHRegEnumUSValue(hUSkey: THUSKey; dwIndex: DWORD; pszValueName: PTSTR; var pcchValueName, pdwType: DWORD; pvData: Pointer; var pcbData: DWORD; enumRegFlags: TSHRegEnumFlags): Longint; stdcall;
{$EXTERNALSYM SHRegQueryInfoUSKeyA}
function SHRegQueryInfoUSKeyA(hUSKey: THUSKey; var pcSubKeys, pcchMaxSubKeyLen, pcValues, pcchMaxValueNameLen: DWORD; enumRegFlags: TSHRegEnumFlags): Longint; stdcall;
{$EXTERNALSYM SHRegQueryInfoUSKeyW}
function SHRegQueryInfoUSKeyW(hUSKey: THUSKey; var pcSubKeys, pcchMaxSubKeyLen, pcValues, pcchMaxValueNameLen: DWORD; enumRegFlags: TSHRegEnumFlags): Longint; stdcall;
{$EXTERNALSYM SHRegQueryInfoUSKey}
function SHRegQueryInfoUSKey(hUSKey: THUSKey; var pcSubKeys, pcchMaxSubKeyLen, pcValues, pcchMaxValueNameLen: DWORD; enumRegFlags: TSHRegEnumFlags): Longint; stdcall;
{$EXTERNALSYM SHRegCloseUSKey}
function SHRegCloseUSKey(hUSKey: THUSKey): Longint; stdcall;


// These calls are equal to an SHRegOpenUSKey, SHRegQueryUSValue, and then a SHRegCloseUSKey.
{$EXTERNALSYM SHRegGetUSValueA}
function SHRegGetUSValueA(pszSubKey, pszValue: PAnsiChar; var pdwType: DWORD; pvData: Pointer; var pcbData: DWORD; fIgnoreHKCU: BOOL; pvDefaultData: Pointer; dwDefaultDataSize: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegGetUSValueW}
function SHRegGetUSValueW(pwzSubKey, pwzValue: PWideChar; var pdwType: DWORD; pvData: Pointer; var pcbData: DWORD; fIgnoreHKCU: BOOL; pvDefaultData: Pointer; dwDefaultDataSize: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegGetUSValue}
function SHRegGetUSValue(pszSubKey, pszValue: PTSTR; var pdwType: DWORD; pvData: Pointer; var pcbData: DWORD; fIgnoreHKCU: BOOL; pvDefaultData: Pointer; dwDefaultDataSize: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegSetUSValueA}
function SHRegSetUSValueA(pszSubKey, pszValue: PAnsiChar; dwType: DWORD; pvData: Pointer; cbData, dwFlags: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegSetUSValueW}
function SHRegSetUSValueW(pwzSubKey, pwzValue: PWideChar; dwType: DWORD; pvData: Pointer; cbData, dwFlags: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegSetUSValue}
function SHRegSetUSValue(pszSubKey, pszValue: PTSTR; dwType: DWORD; pvData: Pointer; cbData, dwFlags: DWORD): Longint; stdcall;


{$EXTERNALSYM SHRegGetIntW}
function SHRegGetIntW(hk: HKEY; pwzKey: PWideChar; iDefault: Integer): Integer stdcall;
{$EXTERNALSYM SHRegGetInt}
function SHRegGetInt(hk: HKEY; pwzKey: PWideChar; iDefault: Integer): Integer stdcall;

{$EXTERNALSYM SHRegGetBoolUSValueA}
function SHRegGetBoolUSValueA(pszSubKey, pszValue: PAnsiChar; fIgnoreHKCU, fDefault: BOOL): BOOL; stdcall;
{$EXTERNALSYM SHRegGetBoolUSValueW}
function SHRegGetBoolUSValueW(pszSubKey, pszValue: PWideChar; fIgnoreHKCU, fDefault: BOOL): BOOL; stdcall;
{$EXTERNALSYM SHRegGetBoolUSValue}
function SHRegGetBoolUSValue(pszSubKey, pszValue: PTSTR; fIgnoreHKCU, fDefault: BOOL): BOOL; stdcall;

//
//  Association APIs
//
//  these APIs are to assist in accessing the data in HKCR
//  getting the Command strings and exe paths
//  for different verbs and extensions are simplified this way
//

const
  {$EXTERNALSYM ASSOCF_INIT_NOREMAPCLSID}
  ASSOCF_INIT_NOREMAPCLSID    = $00000001;   //  do not remap clsids to progids
  {$EXTERNALSYM ASSOCF_INIT_BYEXENAME}
  ASSOCF_INIT_BYEXENAME       = $00000002;   //  executable is being passed in
  {$EXTERNALSYM ASSOCF_OPEN_BYEXENAME}
  ASSOCF_OPEN_BYEXENAME       = $00000002;   //  executable is being passed in
  {$EXTERNALSYM ASSOCF_INIT_DEFAULTTOSTAR}
  ASSOCF_INIT_DEFAULTTOSTAR   = $00000004;   //  treat "*" as the BaseClass
  {$EXTERNALSYM ASSOCF_INIT_DEFAULTTOFOLDER}
  ASSOCF_INIT_DEFAULTTOFOLDER = $00000008;   //  treat "Folder" as the BaseClass
  {$EXTERNALSYM ASSOCF_NOUSERSETTINGS}
  ASSOCF_NOUSERSETTINGS       = $00000010;   //  dont use HKCU
  {$EXTERNALSYM ASSOCF_NOTRUNCATE}
  ASSOCF_NOTRUNCATE           = $00000020;   //  dont truncate the return string
  {$EXTERNALSYM ASSOCF_VERIFY}
  ASSOCF_VERIFY               = $00000040;   //  verify data is accurate (DISK HITS)
  {$EXTERNALSYM ASSOCF_REMAPRUNDLL}
  ASSOCF_REMAPRUNDLL          = $00000080;   //  actually gets info about rundlls target if applicable
  {$EXTERNALSYM ASSOCF_NOFIXUPS}
  ASSOCF_NOFIXUPS             = $00000100;   //  attempt to fix errors if found
  {$EXTERNALSYM ASSOCF_IGNOREBASECLASS}
  ASSOCF_IGNOREBASECLASS      = $00000200;   //  dont recurse into the baseclass

type
  {$EXTERNALSYM ASSOCF}
  ASSOCF = DWORD;
  TAssocF = ASSOCF;

  {$EXTERNALSYM ASSOCSTR}
  ASSOCSTR = DWORD;
  TAssocStr = DWORD;

const
  {$EXTERNALSYM ASSOCSTR_COMMAND}
  ASSOCSTR_COMMAND         = 1;  //  shell\verb\command string
  {$EXTERNALSYM ASSOCSTR_EXECUTABLE}
  ASSOCSTR_EXECUTABLE      = 2;  //  the executable part of command string
  {$EXTERNALSYM ASSOCSTR_FRIENDLYDOCNAME}
  ASSOCSTR_FRIENDLYDOCNAME = 3;  //  friendly name of the document type
  {$EXTERNALSYM ASSOCSTR_FRIENDLYAPPNAME}
  ASSOCSTR_FRIENDLYAPPNAME = 4;  //  friendly name of executable
  {$EXTERNALSYM ASSOCSTR_NOOPEN}
  ASSOCSTR_NOOPEN          = 5;  //  noopen value
  {$EXTERNALSYM ASSOCSTR_SHELLNEWVALUE}
  ASSOCSTR_SHELLNEWVALUE   = 6;  //  query values under the shellnew key
  {$EXTERNALSYM ASSOCSTR_DDECOMMAND}
  ASSOCSTR_DDECOMMAND      = 7;  //  template for DDE commands
  {$EXTERNALSYM ASSOCSTR_DDEIFEXEC}
  ASSOCSTR_DDEIFEXEC       = 8;  //  DDECOMMAND to use if just create a process
  {$EXTERNALSYM ASSOCSTR_DDEAPPLICATION}
  ASSOCSTR_DDEAPPLICATION  = 9;  //  Application name in DDE broadcast
  {$EXTERNALSYM ASSOCSTR_DDETOPIC}
  ASSOCSTR_DDETOPIC        = 10; //  Topic Name in DDE broadcast
  {$EXTERNALSYM ASSOCSTR_INFOTIP}
  ASSOCSTR_INFOTIP         = 11; //  info tip for an item or list of properties to create info tip from
  {$EXTERNALSYM ASSOCSTR_QUICKTIP}
  ASSOCSTR_QUICKTIP        = 12; //  same as ASSOCSTR_INFOTIP except this list contains only quickly retrievable properties
  {$EXTERNALSYM ASSOCSTR_TILEINFO}
  ASSOCSTR_TILEINFO        = 13; //  similar to ASSOCSTR_INFOTIP - lists important properties for tileview
  {$EXTERNALSYM ASSOCSTR_CONTENTTYPE}
  ASSOCSTR_CONTENTTYPE     = 14; //  MIME Content type
  {$EXTERNALSYM ASSOCSTR_DEFAULTICON}
  ASSOCSTR_DEFAULTICON     = 15; //  Default icon source
  {$EXTERNALSYM ASSOCSTR_SHELLEXTENSION}
  ASSOCSTR_SHELLEXTENSION  = 16; //  Guid string pointing to the Shellex\Shellextensionhandler value.
  {$EXTERNALSYM ASSOCSTR_MAX}
  ASSOCSTR_MAX             = 17;

type
  {$EXTERNALSYM ASSOCKEY}
  ASSOCKEY = DWORD;
  TAssocKey = DWORD;

const
  {$EXTERNALSYM ASSOCKEY_SHELLEXECCLASS}
  ASSOCKEY_SHELLEXECCLASS = 1;  //  the key that should be passed to ShellExec(hkeyClass)
  {$EXTERNALSYM ASSOCKEY_APP}
  ASSOCKEY_APP            = 2;  //  the "Application" key for the association
  {$EXTERNALSYM ASSOCKEY_CLASS}
  ASSOCKEY_CLASS          = 3;  //  the progid or class key
  {$EXTERNALSYM ASSOCKEY_BASECLASS}
  ASSOCKEY_BASECLASS      = 4; //  the BaseClass key
  {$EXTERNALSYM ASSOCKEY_MAX}
  ASSOCKEY_MAX            = 5;

type
  {$EXTERNALSYM ASSOCDATA}
  ASSOCDATA = DWORD;
  TAssocData = DWORD;

const
  {$EXTERNALSYM ASSOCDATA_MSIDESCRIPTOR}
  ASSOCDATA_MSIDESCRIPTOR     = 1;  //  Component Descriptor to pass to MSI APIs
  {$EXTERNALSYM ASSOCDATA_NOACTIVATEHANDLER}
  ASSOCDATA_NOACTIVATEHANDLER = 2;  //  restrict attempts to activate window
  {$EXTERNALSYM ASSOCDATA_QUERYCLASSSTORE}
  ASSOCDATA_QUERYCLASSSTORE   = 3;  //  should check with the NT Class Store
  {$EXTERNALSYM ASSOCDATA_HASPERUSERASSOC}
  ASSOCDATA_HASPERUSERASSOC   = 4;  //  defaults to user specified association
  {$EXTERNALSYM ASSOCDATA_EDITFLAGS}
  ASSOCDATA_EDITFLAGS         = 5;  //  Edit flags.
  {$EXTERNALSYM ASSOCDATA_VALUE}
  ASSOCDATA_VALUE             = 6; //  use pszExtra as the Value name
  {$EXTERNALSYM ASSOCDATA_MAX}
  ASSOCDATA_MAX               = 7;

type
  {$EXTERNALSYM ASSOCENUM}
  ASSOCENUM = DWORD;
  TAssocEnum = DWORD;

const
  {$EXTERNALSYM ASSOCENUM_NONE}
  ASSOCENUM_NONE = 0;

type
  {$EXTERNALSYM IQueryAssociations}
  IQueryAssociations = interface(IUnknown)
  ['{C46CA590-3C3F-11D2-BEE6-0000F805CA57}']
    function Init(flags: TAssocF; pszAssoc: PWideChar; hkProgid: HKEY; hwnd: HWND): HResult; stdcall;
    function GetString(flags: TAssocF; str: TAssocStr; pszExtra, pszOut: PWideChar; out pcchOut: DWORD): HResult; stdcall;
    function GetKey(flags: TAssocF; key: TAssocKey; pszExtra: PWideChar; out phkeyOut: HKEY): HResult; stdcall;
    function GetData(flags: TAssocF; data: TAssocData; pszExtra: PWideChar; pvOut: Pointer; var pcbOut: DWORD): HResult; stdcall;
    function GetEnum(flags: TAssocF; assocenum: TAssocEnum; pszExtra: PWideChar; riid: TIID; out ppvOut: Pointer): HResult; stdcall;
  end;

{$EXTERNALSYM AssocCreate}
function AssocCreate(clsid: TCLSID; riid: TIID; out ppv: Pointer): HResult; stdcall;

//  wrappers for the interface
{$EXTERNALSYM AssocQueryStringA}
function AssocQueryStringA(flags: TAssocF; str: TAssocStr; pszAssoc, pszExtra, pszOut: PAnsiChar; var pcchOut: DWORD): HResult; stdcall;
{$EXTERNALSYM AssocQueryStringW}
function AssocQueryStringW(flags: TAssocF; str: TAssocStr; pszAssoc, pszExtra, pszOut: PWideChar; var pcchOut: DWORD): HResult; stdcall;
{$EXTERNALSYM AssocQueryString}
function AssocQueryString(flags: TAssocF; str: TAssocStr; pszAssoc, pszExtra, pszOut: PTSTR; var pcchOut: DWORD): HResult; stdcall;
{$EXTERNALSYM AssocQueryStringByKeyA}
function AssocQueryStringByKeyA(flags: TAssocF; str: TAssocStr; hkAssoc: HKEY; pszExtra, pszOut: PAnsiChar; var pcchOut: DWORD): HResult; stdcall;
{$EXTERNALSYM AssocQueryStringByKeyW}
function AssocQueryStringByKeyW(flags: TAssocF; str: TAssocStr; hkAssoc: HKEY; pszExtra, pszOut: PWideChar; var pcchOut: DWORD): HResult; stdcall;
{$EXTERNALSYM AssocQueryStringByKey}
function AssocQueryStringByKey(flags: TAssocF; str: TAssocStr; hkAssoc: HKEY; pszExtra, pszOut: PTSTR; var pcchOut: DWORD): HResult; stdcall;
{$EXTERNALSYM AssocQueryKeyA}
function AssocQueryKeyA(flags: TAssocF; key: TAssocKey; pszAssoc, pszExtra: PAnsiChar; var phkeyOut: HKEY): HResult; stdcall;
{$EXTERNALSYM AssocQueryKeyW}
function AssocQueryKeyW(flags: TAssocF; key: TAssocKey; pszAssoc, pszExtra: PWideChar; var phkeyOut: HKEY): HResult; stdcall;
{$EXTERNALSYM AssocQueryKey}
function AssocQueryKey(flags: TAssocF; key: TAssocKey; pszAssoc, pszExtra: PTSTR; var phkeyOut: HKEY): HResult; stdcall;

//  AssocIsDangerous() checks a file type to determine whether it is "Dangerous"
//      this maps to the IE download dialog's forcing a prompt to open or save.
//      dangerous file types should be handled more carefully than other file types.
//
//  Parameter:  pszAssoc - type to check.  may be an extension or progid.  (".exe" or "exefile" would both be valid)
//
//  Returns: TRUE if the file type is dangerous.
//
//  NOTES:
//
//      this API first checks a hardcoded list of known dangerous types.
//      then it checks the editflags for the file type looking for the FTA_AlwaysUnsafe bit.
//      then it checks Safer policies.
//

{$EXTERNALSYM AssocIsDangerous}
function AssocIsDangerous(pszAssoc: PWideChar): BOOL stdcall;

//  PERCEIVED types:

type
  {$EXTERNALSYM PERCEIVED}
  PERCEIVED = DWORD;
  TPerceived = DWORD;

const
  {$EXTERNALSYM PERCEIVED_TYPE_CUSTOM}
  PERCEIVED_TYPE_CUSTOM      = -3;
  {$EXTERNALSYM PERCEIVED_TYPE_UNSPECIFIED}
  PERCEIVED_TYPE_UNSPECIFIED = -2;
  {$EXTERNALSYM PERCEIVED_TYPE_FOLDER}
  PERCEIVED_TYPE_FOLDER      = -1;
  {$EXTERNALSYM PERCEIVED_TYPE_UNKNOWN}
  PERCEIVED_TYPE_UNKNOWN     = 0;
  {$EXTERNALSYM PERCEIVED_TYPE_TEXT}
  PERCEIVED_TYPE_TEXT        = 1;
  {$EXTERNALSYM PERCEIVED_TYPE_IMAGE}
  PERCEIVED_TYPE_IMAGE       = 2;
  {$EXTERNALSYM PERCEIVED_TYPE_AUDIO}
  PERCEIVED_TYPE_AUDIO       = 3;
  {$EXTERNALSYM PERCEIVED_TYPE_VIDEO}
  PERCEIVED_TYPE_VIDEO       = 4;
  {$EXTERNALSYM PERCEIVED_TYPE_COMPRESSED}
  PERCEIVED_TYPE_COMPRESSED  = 5;
  {$EXTERNALSYM PERCEIVED_TYPE_DOCUMENT}
  PERCEIVED_TYPE_DOCUMENT    = 6;
  {$EXTERNALSYM PERCEIVED_TYPE_SYSTEM}
  PERCEIVED_TYPE_SYSTEM      = 7;
  {$EXTERNALSYM PERCEIVED_TYPE_APPLICATION}
  PERCEIVED_TYPE_APPLICATION = 8;

type
  {$EXTERNALSYM PERCEIVEDFLAG}
  PERCEIVEDFLAG = DWORD;
  TPerceivedFlag = PERCEIVEDFLAG;

const
  {$EXTERNALSYM PERCEIVEDFLAG_UNDEFINED}
  PERCEIVEDFLAG_UNDEFINED     = $0000;
  {$EXTERNALSYM PERCEIVEDFLAG_SOFTCODED}
  PERCEIVEDFLAG_SOFTCODED     = $0001;
  {$EXTERNALSYM PERCEIVEDFLAG_HARDCODED}
  PERCEIVEDFLAG_HARDCODED     = $0002;
  {$EXTERNALSYM PERCEIVEDFLAG_NATIVESUPPORT}
  PERCEIVEDFLAG_NATIVESUPPORT = $0004;
  {$EXTERNALSYM PERCEIVEDFLAG_GDIPLUS}
  PERCEIVEDFLAG_GDIPLUS       = $0010;
  {$EXTERNALSYM PERCEIVEDFLAG_WMSDK}
  PERCEIVEDFLAG_WMSDK         = $0020;
  {$EXTERNALSYM PERCEIVEDFLAG_ZIPFOLDER}
  PERCEIVEDFLAG_ZIPFOLDER     = $0040;

  {$EXTERNALSYM AssocGetPerceivedType}
function AssocGetPerceivedType(pszExt: PWideChar; out ptype: TPerceived; out pflag: TPerceivedFlag; ppszType: PPWideChar): HResult stdcall;

//
//=============== Stream Routines ===================================
//

{$EXTERNALSYM SHOpenRegStreamA}
function SHOpenRegStreamA(hkey: HKEY; pszSubkey, pszValue: PAnsiChar; grfMode: DWORD): IStream; stdcall;
{$EXTERNALSYM SHOpenRegStreamW}
function SHOpenRegStreamW(hkey: HKEY; pszSubkey, pszValue: PWideChar; grfMode: DWORD): IStream; stdcall;
{$EXTERNALSYM SHOpenRegStream}
function SHOpenRegStream(hkey: HKEY; pszSubkey, pszValue: PTSTR; grfMode: DWORD): IStream; stdcall;
{$EXTERNALSYM SHOpenRegStream2A}
function SHOpenRegStream2A(hkey: HKEY; pszSubkey, pszValue: PAnsiChar; grfMode: DWORD): IStream; stdcall;
{$EXTERNALSYM SHOpenRegStream2W}
function SHOpenRegStream2W(hkey: HKEY; pszSubkey, pszValue: PWideChar; grfMode: DWORD): IStream; stdcall;
{$EXTERNALSYM SHOpenRegStream2}
function SHOpenRegStream2(hkey: HKEY; pszSubkey, pszValue: PTSTR; grfMode: DWORD): IStream; stdcall;

{$EXTERNALSYM SHCreateStreamOnFileA}
function SHCreateStreamOnFileA(pszFile: PAnsiChar; grfMode: DWORD; out ppstm: IStream): HResult; stdcall;
{$EXTERNALSYM SHCreateStreamOnFileW}
function SHCreateStreamOnFileW(pszFile: PWideChar; grfMode: DWORD; out ppstm: IStream): HResult; stdcall;
{$EXTERNALSYM SHCreateStreamOnFile}
function SHCreateStreamOnFile(pszFile: PTSTR; grfMode: DWORD; out ppstm: IStream): HResult; stdcall;

  {$EXTERNALSYM SHCreateStreamOnFileEx}
function SHCreateStreamOnFileEx(pszFile: PWideChar; grfMode, dwAttributes: DWORD; fCreate: BOOL; pstmTemplate: IStream; out ppstm: IStream): HResult stdcall;


//
//=============== HTTP helper Routines ===================================
//

{$EXTERNALSYM GetAcceptLanguagesA}
function GetAcceptLanguagesA(psz: PAnsiChar; var pcch: DWORD): HResult; stdcall;
{$EXTERNALSYM GetAcceptLanguagesW}
function GetAcceptLanguagesW(psz: PWideChar; var pcch: DWORD): HResult; stdcall;
{$EXTERNALSYM GetAcceptLanguages}
function GetAcceptLanguages(psz: PTSTR; var pcch: DWORD): HResult; stdcall;

const
  {$EXTERNALSYM SHGVSPB_PERUSER}
  SHGVSPB_PERUSER             = $00000001; // must have one of PERUSER or ALLUSERS
  {$EXTERNALSYM SHGVSPB_ALLUSERS}
  SHGVSPB_ALLUSERS            = $00000002;
  {$EXTERNALSYM SHGVSPB_PERFOLDER}
  SHGVSPB_PERFOLDER           = $00000004; // must have one of PERFOLDER ALLFOLDERS or INHERIT
  {$EXTERNALSYM SHGVSPB_ALLFOLDERS}
  SHGVSPB_ALLFOLDERS          = $00000008;
  {$EXTERNALSYM SHGVSPB_INHERIT}
  SHGVSPB_INHERIT             = $00000010;
  {$EXTERNALSYM SHGVSPB_ROAM}
  SHGVSPB_ROAM                = $00000020; // modifies the above
  {$EXTERNALSYM SHGVSPB_NOAUTODEFAULTS}
  SHGVSPB_NOAUTODEFAULTS      = $80000000; // turns off read delegation to more general property bags

  {$EXTERNALSYM SHGVSPB_FOLDER}
  SHGVSPB_FOLDER              = SHGVSPB_PERUSER or  SHGVSPB_PERFOLDER;
  {$EXTERNALSYM SHGVSPB_FOLDERNODEFAULTS}
  SHGVSPB_FOLDERNODEFAULTS    = SHGVSPB_PERUSER or SHGVSPB_PERFOLDER or SHGVSPB_NOAUTODEFAULTS;
  {$EXTERNALSYM SHGVSPB_USERDEFAULTS}
  SHGVSPB_USERDEFAULTS        = SHGVSPB_PERUSER or SHGVSPB_ALLFOLDERS;
  {$EXTERNALSYM SHGVSPB_GLOBALDEAFAULTS}
  SHGVSPB_GLOBALDEAFAULTS     = SHGVSPB_ALLUSERS or SHGVSPB_ALLFOLDERS;

  {$EXTERNALSYM SHGetViewStatePropertyBag}
function SHGetViewStatePropertyBag(pidl: PItemIDList; pszBagName: PWideChar; dwFlags: DWORD; riid: TIID; out ppv: Pointer): HResult; stdcall;

// Shared memory apis

{$EXTERNALSYM SHAllocShared}
function SHAllocShared(pvData: Pointer; dwSize, dwProcessId: DWORD): THandle; stdcall;
{$EXTERNALSYM SHFreeShared}
function SHFreeShared(hData: THandle; dwProcessId: DWORD): BOOL; stdcall;
{$EXTERNALSYM SHLockShared}
function SHLockShared(hData: THandle; dwProcessId: DWORD): Pointer; stdcall;
{$EXTERNALSYM SHUnlockShared}
function SHUnlockShared(pvData: Pointer): BOOL; stdcall;


// SHAutoComplete
//      hwndEdit - HWND of editbox, ComboBox or ComboBoxEx.
//      dwFlags - Flags to indicate what to AutoAppend or AutoSuggest for the editbox.
//
// WARNING:
//    Caller needs to have called CoInitialize() or OleInitialize()
//    and cannot call CoUninit/OleUninit until after
//    WM_DESTROY on hwndEdit.
//
//  dwFlags values:
const
  {$EXTERNALSYM SHACF_DEFAULT}
  SHACF_DEFAULT                   = $00000000;  // Currently (SHACF_FILESYSTEM | SHACF_URLALL)
  {$EXTERNALSYM SHACF_FILESYSTEM}
  SHACF_FILESYSTEM                = $00000001;  // This includes the File System as well as the rest of the shell (Desktop\My Computer\Control Panel\)
  {$EXTERNALSYM SHACF_URLHISTORY}
  SHACF_URLHISTORY                = $00000002;  // URLs in the User's History
  {$EXTERNALSYM SHACF_URLMRU}
  SHACF_URLMRU                    = $00000004;  // URLs in the User's Recently Used list.
  {$EXTERNALSYM SHACF_URLALL}
  SHACF_URLALL                    = SHACF_URLHISTORY or SHACF_URLMRU;
  {$EXTERNALSYM SHACF_USETAB}
  SHACF_USETAB                    = $00000008;  // Use the tab to move thru the autocomplete possibilities instead of to the next dialog/window control.
  {$EXTERNALSYM SHACF_FILESYS_ONLY}
  SHACF_FILESYS_ONLY              = $00000010;  // This includes the File System

  {$EXTERNALSYM SHACF_FILESYS_DIRS}
  SHACF_FILESYS_DIRS              = $00000020;  // Same as SHACF_FILESYS_ONLY except it only includes directories, UNC servers, and UNC server shares.

  {$EXTERNALSYM SHACF_AUTOSUGGEST_FORCE_ON}
  SHACF_AUTOSUGGEST_FORCE_ON      = $10000000;  // Ignore the registry default and force the feature on.
  {$EXTERNALSYM SHACF_AUTOSUGGEST_FORCE_OFF}
  SHACF_AUTOSUGGEST_FORCE_OFF     = $20000000;  // Ignore the registry default and force the feature off.
  {$EXTERNALSYM SHACF_AUTOAPPEND_FORCE_ON}
  SHACF_AUTOAPPEND_FORCE_ON       = $40000000;  // Ignore the registry default and force the feature on. (Also know as AutoComplete)
  {$EXTERNALSYM SHACF_AUTOAPPEND_FORCE_OFF}
  SHACF_AUTOAPPEND_FORCE_OFF      = $80000000;  // Ignore the registry default and force the feature off. (Also know as AutoComplete)

{$EXTERNALSYM SHAutoComplete}
function SHAutoComplete(hwndEdit: HWND; dwFlags: DWORD): HResult; stdcall;

{$EXTERNALSYM SHSetThreadRef}
function SHSetThreadRef(punk: IUnknown): HResult; stdcall;
{$EXTERNALSYM SHGetThreadRef}
function SHGetThreadRef(out ppunk: IUnknown): HResult; stdcall;

{$EXTERNALSYM SHSkipJunction}
function SHSkipJunction(pbc: IBindCtx; var pclsid: TCLSID): BOOL; stdcall;

 {$EXTERNALSYM SHCreateThreadRef}
function SHCreateThreadRef(var pcRef: Longint; out punk: IUnknown): HResult; stdcall;

const
  {$EXTERNALSYM CTF_INSIST}
  CTF_INSIST          = $00000001;      // SHCreateThread() dwFlags - call pfnThreadProc synchronously if CreateThread() fails
  {$EXTERNALSYM CTF_THREAD_REF}
  CTF_THREAD_REF      = $00000002;      // hold a reference to the creating thread
  {$EXTERNALSYM CTF_PROCESS_REF}
  CTF_PROCESS_REF     = $00000004;      // hold a reference to the creating process
  {$EXTERNALSYM CTF_COINIT}
  CTF_COINIT          = $00000008;      // init COM for the created thread
  {$EXTERNALSYM CTF_FREELIBANDEXIT}
  CTF_FREELIBANDEXIT  = $00000010;      // hold a ref to the DLL and call FreeLibraryAndExitThread() when done
  {$EXTERNALSYM CTF_REF_COUNTED}
  CTF_REF_COUNTED     = $00000020;      // thread supports ref counting via SHGetThreadRef() or CTF_THREAD_REF so that child threads can keep this thread alive
  {$EXTERNALSYM CTF_WAIT_ALLOWCOM}
  CTF_WAIT_ALLOWCOM   = $00000040;      // while waiting for pfnCallback, allow COM marshaling to the blocked calling thread

{$EXTERNALSYM SHCreateThread}
function SHCreateThread(pfnThreadProc: TThreadStartRoutine; pData: Pointer; dwFlags: DWORD; pfnCallback: TThreadStartRoutine): BOOL; stdcall;

{$EXTERNALSYM SHReleaseThreadRef}
function SHReleaseThreadRef: HResult; stdcall; // release a CTF_THREAD_REF reference earlier than the return of pfnThreadProc

//
//====== GDI helper functions  ================================================
//

{$EXTERNALSYM SHCreateShellPalette}
function SHCreateShellPalette(hdc: HDC): HPALETTE; stdcall;

{$EXTERNALSYM ColorRGBToHLS}
procedure ColorRGBToHLS(clrRGB: TColorRef; var pwHue, pwLuminance, pwSaturation: Word); stdcall;
{$EXTERNALSYM ColorHLSToRGB}
function ColorHLSToRGB(wHue, wLuminance, wSaturation: Word): TColorRef; stdcall;
{$EXTERNALSYM ColorAdjustLuma}
function ColorAdjustLuma(clrRGB: TColorRef; n: Integer; fScale: BOOL): TColorRef; stdcall;


//
//====== DllGetVersion  =======================================================
//

type
  PDLLVersionInfo = ^TDLLVersionInfo;
  {$EXTERNALSYM _DLLVERSIONINFO}
  _DLLVERSIONINFO = record
    cbSize: DWORD;
    dwMajorVersion: DWORD;                   // Major version
    dwMinorVersion: DWORD;                   // Minor version
    dwBuildNumber: DWORD;                    // Build number
    dwPlatformID: DWORD;                     // DLLVER_PLATFORM_*
  end;
  {$EXTERNALSYM DLLVERSIONINFO}
  DLLVERSIONINFO = _DLLVERSIONINFO;
  TDLLVersionInfo = _DLLVERSIONINFO;

// Platform IDs for DLLVERSIONINFO
const
  {$EXTERNALSYM DLLVER_PLATFORM_WINDOWS}
  DLLVER_PLATFORM_WINDOWS         = $00000001;      // Windows 95
  {$EXTERNALSYM DLLVER_PLATFORM_NT}
  DLLVER_PLATFORM_NT              = $00000002;      // Windows NT

type
  PDLLVersionInfo2 = ^TDLLVersionInfo2;
  {$EXTERNALSYM _DLLVERSIONINFO2}
  _DLLVERSIONINFO2 = record
    info1: TDLLVersionInfo;
    dwFlags: DWORD;                          // No flags currently defined
    ullVersion: Int64;                   // Encoded as:
                                             // Major 0xFFFF 0000 0000 0000
                                             // Minor 0x0000 FFFF 0000 0000
                                             // Build 0x0000 0000 FFFF 0000
                                             // QFE   0x0000 0000 0000 FFFF
  end;
  {$EXTERNALSYM DLLVERSIONINFO2}
  DLLVERSIONINFO2 = _DLLVERSIONINFO2;
  TDLLVersionInfo2 = _DLLVERSIONINFO2;

const
  {$EXTERNALSYM DLLVER_MAJOR_MASK}
  DLLVER_MAJOR_MASK                    = $FFFF000000000000;
  {$EXTERNALSYM DLLVER_MINOR_MASK}
  DLLVER_MINOR_MASK                    = $0000FFFF00000000;
  {$EXTERNALSYM DLLVER_BUILD_MASK}
  DLLVER_BUILD_MASK                    = $00000000FFFF0000;
  {$EXTERNALSYM DLLVER_QFE_MASK}
  DLLVER_QFE_MASK                      = $000000000000FFFF;

{$EXTERNALSYM MakeDLLVerULL}
function MakeDLLVerULL(major, minor, build, qfe: Word): Int64;


type
  // The caller should always GetProcAddress("DllGetVersion"), not
  // implicitly link to it.
  TDLLGetVersionProc = function(var DLLVersionInfo: TDLLVersionInfo): HResult stdcall;

  // DllInstall (to be implemented by self-installing DLLs)
  TDllInstall = function(bInstall: BOOL; pszCmdLine: PWideChar): HResult stdcall;

{$EXTERNALSYM IsInternetESCEnabled}
function IsInternetESCEnabled: BOOL stdcall;

{$IFDEF WINXP_UP}

//stOrM!------------------------------------------------------------------------------------------------------------------------------------------------

const
  MB_TIMEDOUT = 32000; 

function MessageBoxTimeOut(
      hWnd: HWND; lpText: PTSTR; lpCaption: PTSTR;
      uType: UINT; wLanguageId: WORD; dwMilliseconds: DWORD): Integer; stdcall;

function MessageBoxTimeOutA(
      hWnd: HWND; lpText: PAnsiChar; lpCaption: PAnsiChar;
      uType: UINT; wLanguageId: WORD; dwMilliseconds: DWORD): Integer; stdcall;

function MessageBoxTimeOutW(
      hWnd: HWND; lpText: PWideChar; lpCaption: PWideChar;
      uType: UINT; wLanguageId: WORD; dwMilliseconds: DWORD): Integer; stdcall;

//-------------------------------------------------------------------------------------------------------------------------------------------stOrM!
{$ENDIF WINXP_UP}

//stOrM!------------------------------------------------------------------------------------------------------------------------------------------

{$IFDEF WIN2000_UP}

function MessageBoxCheck(
      hWnd: HWND; lpText: PTSTR; lpCaption: PTSTR;
      uType: UINT;  Default: Integer; RegVal: PTSTR) : Integer; stdcall;

function MessageBoxCheckA(
      hWnd: HWND; lpText: PAnsiChar; lpCaption: PAnsiChar;
      uType: UINT;  Default: Integer; RegVal: PAnsiChar) : Integer; stdcall;


function MessageBoxCheckW(
      hWnd: HWND; lpText: PWideChar; lpCaption: PWideChar;
      uType: UINT;  Default: Integer; RegVal: PWideChar) : Integer; stdcall;

{$ENDIF WIN2000_UP}

//------------------------------------------------------------------------------------------------------------------------------------------stOrM!

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
uses JwaWinDLLNames;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INCLUDEMODE}
const
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

{$IFNDEF JWA_INTERFACESECTION}


// Macro functions
function StrIntlEqNA(s1, s2: PAnsiChar; nChar: Integer): BOOL;
begin
  Result := StrIsIntlEqualA(True, s1, s2, nChar);
end;

function StrIntlEqNW(s1, s2: PWideChar; nChar: Integer): BOOL;
begin
  Result := StrIsIntlEqualW(True, s1, s2, nChar);
end;

function StrIntlEqN(s1, s2: PTSTR; nChar: Integer): BOOL;
begin
  Result := {$IFDEF UNICODE}StrIsIntlEqualW{$ELSE}StrIsIntlEqualA{$ENDIF}(True, s1, s2, nChar);
end;

function StrIntlEqNIA(s1, s2: PAnsiChar; nChar: Integer): BOOL;
begin
  Result := StrIsIntlEqualA(False, s1, s2, nChar);
end;

function StrIntlEqNIW(s1, s2: PWideChar; nChar: Integer): BOOL;
begin
  Result := StrIsIntlEqualW(False, s1, s2, nChar);
end;

function StrIntlEqNI(s1, s2: PTSTR; nChar: Integer): BOOL;
begin
  Result := {$IFDEF UNICODE}StrIsIntlEqualW{$ELSE}StrIsIntlEqualA{$ENDIF}(False, s1, s2, nChar);
end;

function IntlStrEqNA(s1, s2: PAnsiChar; nChar: Integer): BOOL;
begin
  Result := IntlStrEqWorkerA(True, s1, s2, nChar);
end;

function IntlStrEqNW(s1, s2: PWideChar; nChar: Integer): BOOL;
begin
  Result := IntlStrEqWorkerW(True, s1, s2, nChar);
end;

function IntlStrEqN(s1, s2: PTSTR; nChar: Integer): BOOL;
begin
  Result := {$IFDEF UNICODE}IntlStrEqWorkerW{$ELSE}IntlStrEqWorkerA{$ENDIF}(True, s1, s2, nChar);
end;

function IntlStrEqNIA(s1, s2: PAnsiChar; nChar: Integer): BOOL;
begin
  Result := IntlStrEqWorkerA(False, s1, s2, nChar);
end;

function IntlStrEqNIW(s1, s2: PWideChar; nChar: Integer): BOOL;
begin
  Result := IntlStrEqWorkerW(False, s1, s2, nChar);
end;

function IntlStrEqNI(s1, s2: PTSTR; nChar: Integer): BOOL;
begin
  Result := {$IFDEF UNICODE}IntlStrEqWorkerW{$ELSE}IntlStrEqWorkerA{$ENDIF}(False, s1, s2, nChar);
end;

function PathIsHTMLFileA(pszPath: PAnsiChar): BOOL;
begin
  Result := PathIsContentTypeA(pszPath, SZ_CONTENTTYPE_HTMLA);
end;

function PathIsHTMLFileW(pszPath: PWideChar): BOOL;
begin
  Result := PathIsContentTypeW(pszPath, SZ_CONTENTTYPE_HTMLW);
end;

function PathIsHTMLFile(pszPath: PTSTR): BOOL;
begin
  Result := PathIsContentType(pszPath, SZ_CONTENTTYPE_HTML);
end;

function UrlIsFileUrlA(pszURL: PAnsiChar): BOOL;
begin
  Result := UrlIsA(pszURL, URLIS_FILEURL);
end;

function UrlIsFileUrlW(pszURL: PWideChar): BOOL;
begin
  Result := UrlIsW(pszURL, URLIS_FILEURL);
end;

function UrlIsFileUrl(pszURL: PTSTR): BOOL;
begin
  Result := {$IFDEF UNICODE}UrlIsW{$ELSE}UrlIsA{$ENDIF}(pszURL, URLIS_FILEURL);
end;

function UrlEscapeSpaces(pszUrl, pszEscaped: PTSTR; var pcchEscaped: DWORD): HResult;
begin
  Result := UrlCanonicalize(pszUrl, pszEscaped, pcchEscaped, URL_ESCAPE_SPACES_ONLY or URL_DONT_ESCAPE_EXTRA_INFO);
end;

function UrlUnescapeInPlace(pszUrl: PTSTR; dwFlags: DWORD): HResult;
begin
  Result := UrlUnescape(pszUrl, nil, nil, dwFlags or URL_UNESCAPE_INPLACE);
end;

function MakeDLLVerULL(major, minor, build, qfe: Word): Int64;
begin
  Result := Int64(major) shl 48 or
            Int64(minor) shl 32 or
            Int64(build) shl 16 or
            Int64(qfe);
end;


{$IFDEF DELPHI6_UP}
//only available as static
function wnsprintfA; external shlwapidll name 'wnsprintfA';
function wnsprintfW; external shlwapidll name 'wnsprintfW';
function wnsprintf; external shlwapidll name 'wnsprintf'+AWSuffix;
{$ENDIF DELPHI6_UP}

{$IFNDEF DYNAMIC_LINK}

function StrChrA; external shlwapidll name 'StrChrA';
function StrChrW; external shlwapidll name 'StrChrW';
function StrChr; external shlwapidll name 'StrChr'+AWSuffix;
function StrChrIA; external shlwapidll name 'StrChrIA';
function StrChrIW; external shlwapidll name 'StrChrIW';
function StrChrI; external shlwapidll name 'StrChrI'+AWSuffix;
function StrCmpNA; external shlwapidll name 'StrCmpNA';
function StrCmpNW; external shlwapidll name 'StrCmpNW';
function StrCmpN; external shlwapidll name 'StrCmpN'+AWSuffix;
function StrCmpNIA; external shlwapidll name 'StrCmpNIA';
function StrCmpNIW; external shlwapidll name 'StrCmpNIW';
function StrCmpNI; external shlwapidll name 'StrCmpNI'+AWSuffix;
function StrCSpnA; external shlwapidll name 'StrCSpnA';
function StrCSpnW; external shlwapidll name 'StrCSpnW';
function StrCSpn; external shlwapidll name 'StrCSpn'+AWSuffix;
function StrCSpnIA; external shlwapidll name 'StrCSpnIA';
function StrCSpnIW; external shlwapidll name 'StrCSpnIW';
function StrCSpnI; external shlwapidll name 'StrCSpnI'+AWSuffix;
function StrDupA; external shlwapidll name 'StrDupA';
function StrDupW; external shlwapidll name 'StrDupW';
function StrDup; external shlwapidll name 'StrDup'+AWSuffix;
function StrFormatByteSizeA; external shlwapidll name 'StrFormatByteSizeA';
function StrFormatByteSize64A; external shlwapidll name 'StrFormatByteSize64A';
function StrFormatByteSizeW; external shlwapidll name 'StrFormatByteSizeW';
function StrFormatByteSize; external shlwapidll name 'StrFormatByteSize'+AWSuffix;
function StrFormatByteSize64; external shlwapidll name 'StrFormatByteSize64A';
function StrFormatKBSizeW; external shlwapidll name 'StrFormatKBSizeW';
function StrFormatKBSizeA; external shlwapidll name 'StrFormatKBSizeA';
function StrFormatKBSize; external shlwapidll name 'StrFormatKBSize'+AWSuffix;
function StrFromTimeIntervalA; external shlwapidll name 'StrFromTimeIntervalA';
function StrFromTimeIntervalW; external shlwapidll name 'StrFromTimeIntervalW';
function StrFromTimeInterval; external shlwapidll name 'StrFromTimeInterval'+AWSuffix;
function StrIsIntlEqualA; external shlwapidll name 'StrIsIntlEqualA';
function StrIsIntlEqualW; external shlwapidll name 'StrIsIntlEqualW';
function StrIsIntlEqual; external shlwapidll name 'StrIsIntlEqual'+AWSuffix;
function StrNCatA; external shlwapidll name 'StrNCatA';
function StrNCatW; external shlwapidll name 'StrNCatW';
function StrNCat; external shlwapidll name 'StrNCat'+AWSuffix;
function StrPBrkA; external shlwapidll name 'StrPBrkA';
function StrPBrkW; external shlwapidll name 'StrPBrkW';
function StrPBrk; external shlwapidll name 'StrPBrk'+AWSuffix;
function StrRChrA; external shlwapidll name 'StrRChrA';
function StrRChrW; external shlwapidll name 'StrRChrW';
function StrRChr; external shlwapidll name 'StrRChr'+AWSuffix;
function StrRChrIA; external shlwapidll name 'StrRChrIA';
function StrRChrIW; external shlwapidll name 'StrRChrIW';
function StrRChrI; external shlwapidll name 'StrRChrI'+AWSuffix;
function StrRStrIA; external shlwapidll name 'StrRStrIA';
function StrRStrIW; external shlwapidll name 'StrRStrIW';
function StrRStrI; external shlwapidll name 'StrRStrI'+AWSuffix;
function StrSpnA; external shlwapidll name 'StrSpnA';
function StrSpnW; external shlwapidll name 'StrSpnW';
function StrSpn; external shlwapidll name 'StrSpn'+AWSuffix;
function StrStrA; external shlwapidll name 'StrStrA';
function StrStrW; external shlwapidll name 'StrStrW';
function StrStr; external shlwapidll name 'StrStr'+AWSuffix;
function StrStrIA; external shlwapidll name 'StrStrIA';
function StrStrIW; external shlwapidll name 'StrStrIW';
function StrStrI; external shlwapidll name 'StrStrI'+AWSuffix;
function StrToIntA; external shlwapidll name 'StrToIntA';
function StrToIntW; external shlwapidll name 'StrToIntW';
function StrToInt; external shlwapidll name 'StrToInt'+AWSuffix;
function StrToIntExA; external shlwapidll name 'StrToIntExA';
function StrToIntExW; external shlwapidll name 'StrToIntExW';
function StrToIntEx; external shlwapidll name 'StrToIntEx'+AWSuffix;
function StrTrimA; external shlwapidll name 'StrTrimA';
function StrTrimW; external shlwapidll name 'StrTrimW';
function StrTrim; external shlwapidll name 'StrTrim'+AWSuffix;
function StrCatW; external shlwapidll name 'StrCatW';
function StrCmpW; external shlwapidll name 'StrCmpW';
function StrCmpIW; external shlwapidll name 'StrCmpIW';
function StrCpyW; external shlwapidll name 'StrCpyW';
function StrCpyNW; external shlwapidll name 'StrCpyNW';
function StrCatBuffW; external shlwapidll name 'StrCatBuffW';
function StrCatBuffA; external shlwapidll name 'StrCatBuffA';
function StrCatBuff; external shlwapidll name 'StrCatBuff'+AWSuffix;
function ChrCmpIA; external shlwapidll name 'ChrCmpIA';
function ChrCmpIW; external shlwapidll name 'ChrCmpIW';
function ChrCmpI; external shlwapidll name 'ChrCmpI'+AWSuffix;
function wvnsprintfA; external shlwapidll name 'wvnsprintfA';
function wvnsprintfW; external shlwapidll name 'wvnsprintfW';
function wvnsprintf; external shlwapidll name 'wvnsprintf'+AWSuffix;
function StrRetToStrA; external shlwapidll name 'StrRetToStrA';
function StrRetToStrW; external shlwapidll name 'StrRetToStrW';
function StrRetToStr; external shlwapidll name 'StrRetToStr'+AWSuffix;
function StrRetToBufA; external shlwapidll name 'StrRetToBufA';
function StrRetToBufW; external shlwapidll name 'StrRetToBufW';
function StrRetToBuf; external shlwapidll name 'StrRetToBuf'+AWSuffix;
function StrRetToBSTR; external shlwapidll name 'StrRetToBSTR';
function SHStrDupA; external shlwapidll name 'SHStrDupA';
function SHStrDupW; external shlwapidll name 'SHStrDupW';
function SHStrDup; external shlwapidll name 'SHStrDup'+AWSuffix;
function StrCmpLogicalW; external shlwapidll name 'StrCmpLogicalW';
function StrCatChainW; external shlwapidll name 'StrCatChainW';
function SHLoadIndirectString; external shlwapidll name 'SHLoadIndirectString';
function IntlStrEqWorkerA; external shlwapidll name 'IntlStrEqWorkerA';
function IntlStrEqWorkerW; external shlwapidll name 'IntlStrEqWorkerW';

function StrToLong; external shlwapidll name 'StrToInt'+AWSuffix;

function StrCatA; external kernel32dll name 'lstrcatA';
function StrCmpA; external kernel32dll name 'lstrcmpA';
function StrCmpIA; external kernel32dll name 'lstrcmpiA';

function StrCpyA; external kernel32dll name 'lstrcpyA';
function StrCpyNA; external kernel32dll name 'lstrcpynA';

//function StrNCmp; external shlwapidll name 'lstrcmp'+AWSuffix;
//function StrNCmpI; external shlwapidll name 'lstrcmpi'+AWSuffix;
function StrNCpy; external kernel32dll name 'lstrcpyn'+AWSuffix;
//function StrCatN; external shlwapidll name 'lStrNCat'+AWSuffix;
function StrCat; external kernel32dll name 'lstrcat'+AWSuffix;
function StrCmp; external kernel32dll name 'lstrcmp'+AWSuffix;
function StrCmpI; external kernel32dll name 'lstrcmpi'+AWSuffix;
function StrCpy; external kernel32dll name 'lstrcpy'+AWSuffix;
function StrCpyN; external kernel32dll name 'lstrcpyn'+AWSuffix;


function PathAddBackslashA; external shlwapidll name 'PathAddBackslashA';
function PathAddBackslashW; external shlwapidll name 'PathAddBackslashW';
function PathAddBackslash; external shlwapidll name 'PathAddBackslash'+AWSuffix;
function PathAddExtensionA; external shlwapidll name 'PathAddExtensionA';
function PathAddExtensionW; external shlwapidll name 'PathAddExtensionW';
function PathAddExtension; external shlwapidll name 'PathAddExtension'+AWSuffix;
function PathAppendA; external shlwapidll name 'PathAppendA';
function PathAppendW; external shlwapidll name 'PathAppendW';
function PathAppend; external shlwapidll name 'PathAppend'+AWSuffix;
function PathBuildRootA; external shlwapidll name 'PathBuildRootA';
function PathBuildRootW; external shlwapidll name 'PathBuildRootW';
function PathBuildRoot; external shlwapidll name 'PathBuildRoot'+AWSuffix;
function PathCanonicalizeA; external shlwapidll name 'PathCanonicalizeA';
function PathCanonicalizeW; external shlwapidll name 'PathCanonicalizeW';
function PathCanonicalize; external shlwapidll name 'PathCanonicalize'+AWSuffix;
function PathCombineA; external shlwapidll name 'PathCombineA';
function PathCombineW; external shlwapidll name 'PathCombineW';
function PathCombine; external shlwapidll name 'PathCombine'+AWSuffix;
function PathCompactPathA; external shlwapidll name 'PathCompactPathA';
function PathCompactPathW; external shlwapidll name 'PathCompactPathW';
function PathCompactPath; external shlwapidll name 'PathCompactPath'+AWSuffix;
function PathCompactPathExA; external shlwapidll name 'PathCompactPathExA';
function PathCompactPathExW; external shlwapidll name 'PathCompactPathExW';
function PathCompactPathEx; external shlwapidll name 'PathCompactPathEx'+AWSuffix;
function PathCommonPrefixA; external shlwapidll name 'PathCommonPrefixA';
function PathCommonPrefixW; external shlwapidll name 'PathCommonPrefixW';
function PathCommonPrefix; external shlwapidll name 'PathCommonPrefix'+AWSuffix;
function PathFileExistsA; external shlwapidll name 'PathFileExistsA';
function PathFileExistsW; external shlwapidll name 'PathFileExistsW';
function PathFileExists; external shlwapidll name 'PathFileExists'+AWSuffix;
function PathFindExtensionA; external shlwapidll name 'PathFindExtensionA';
function PathFindExtensionW; external shlwapidll name 'PathFindExtensionW';
function PathFindExtension; external shlwapidll name 'PathFindExtension'+AWSuffix;
function PathFindFileNameA; external shlwapidll name 'PathFindFileNameA';
function PathFindFileNameW; external shlwapidll name 'PathFindFileNameW';
function PathFindFileName; external shlwapidll name 'PathFindFileName'+AWSuffix;
function PathFindNextComponentA; external shlwapidll name 'PathFindNextComponentA';
function PathFindNextComponentW; external shlwapidll name 'PathFindNextComponentW';
function PathFindNextComponent; external shlwapidll name 'PathFindNextComponent'+AWSuffix;
function PathFindOnPathA; external shlwapidll name 'PathFindOnPathA';
function PathFindOnPathW; external shlwapidll name 'PathFindOnPathW';
function PathFindOnPath; external shlwapidll name 'PathFindOnPath'+AWSuffix;
function PathGetArgsA; external shlwapidll name 'PathGetArgsA';
function PathGetArgsW; external shlwapidll name 'PathGetArgsW';
function PathGetArgs; external shlwapidll name 'PathGetArgs'+AWSuffix;
function PathFindSuffixArrayA; external shlwapidll name 'PathFindSuffixArrayA';
function PathFindSuffixArrayW; external shlwapidll name 'PathFindSuffixArrayW';
function PathFindSuffixArray; external shlwapidll name 'PathFindSuffixArray'+AWSuffix;
function PathIsLFNFileSpecA; external shlwapidll name 'PathIsLFNFileSpecA';
function PathIsLFNFileSpecW; external shlwapidll name 'PathIsLFNFileSpecW';
function PathIsLFNFileSpec; external shlwapidll name 'PathIsLFNFileSpec'+AWSuffix;
function PathGetCharTypeA; external shlwapidll name 'PathGetCharTypeA';
function PathGetCharTypeW; external shlwapidll name 'PathGetCharTypeW';
function PathGetCharType; external shlwapidll name 'PathGetCharType'+AWSuffix;
function PathGetDriveNumberA; external shlwapidll name 'PathGetDriveNumberA';
function PathGetDriveNumberW; external shlwapidll name 'PathGetDriveNumberW';
function PathGetDriveNumber; external shlwapidll name 'PathGetDriveNumber'+AWSuffix;
function PathIsDirectoryA; external shlwapidll name 'PathIsDirectoryA';
function PathIsDirectoryW; external shlwapidll name 'PathIsDirectoryW';
function PathIsDirectory; external shlwapidll name 'PathIsDirectory'+AWSuffix;
function PathIsDirectoryEmptyA; external shlwapidll name 'PathIsDirectoryEmptyA';
function PathIsDirectoryEmptyW; external shlwapidll name 'PathIsDirectoryEmptyW';
function PathIsDirectoryEmpty; external shlwapidll name 'PathIsDirectoryEmpty'+AWSuffix;
function PathIsFileSpecA; external shlwapidll name 'PathIsFileSpecA';
function PathIsFileSpecW; external shlwapidll name 'PathIsFileSpecW';
function PathIsFileSpec; external shlwapidll name 'PathIsFileSpec'+AWSuffix;
function PathIsPrefixA; external shlwapidll name 'PathIsPrefixA';
function PathIsPrefixW; external shlwapidll name 'PathIsPrefixW';
function PathIsPrefix; external shlwapidll name 'PathIsPrefix'+AWSuffix;
function PathIsRelativeA; external shlwapidll name 'PathIsRelativeA';
function PathIsRelativeW; external shlwapidll name 'PathIsRelativeW';
function PathIsRelative; external shlwapidll name 'PathIsRelative'+AWSuffix;
function PathIsRootA; external shlwapidll name 'PathIsRootA';
function PathIsRootW; external shlwapidll name 'PathIsRootW';
function PathIsRoot; external shlwapidll name 'PathIsRoot'+AWSuffix;
function PathIsSameRootA; external shlwapidll name 'PathIsSameRootA';
function PathIsSameRootW; external shlwapidll name 'PathIsSameRootW';
function PathIsSameRoot; external shlwapidll name 'PathIsSameRoot'+AWSuffix;
function PathIsUNCA; external shlwapidll name 'PathIsUNCA';
function PathIsUNCW; external shlwapidll name 'PathIsUNCW';
function PathIsUNC; external shlwapidll name 'PathIsUNC'+AWSuffix;
function PathIsNetworkPathA; external shlwapidll name 'PathIsNetworkPathA';
function PathIsNetworkPathW; external shlwapidll name 'PathIsNetworkPathW';
function PathIsNetworkPath; external shlwapidll name 'PathIsNetworkPath'+AWSuffix;
function PathIsUNCServerA; external shlwapidll name 'PathIsUNCServerA';
function PathIsUNCServerW; external shlwapidll name 'PathIsUNCServerW';
function PathIsUNCServer; external shlwapidll name 'PathIsUNCServer'+AWSuffix;
function PathIsUNCServerShareA; external shlwapidll name 'PathIsUNCServerShareA';
function PathIsUNCServerShareW; external shlwapidll name 'PathIsUNCServerShareW';
function PathIsUNCServerShare; external shlwapidll name 'PathIsUNCServerShare'+AWSuffix;
function PathIsContentTypeA; external shlwapidll name 'PathIsContentTypeA';
function PathIsContentTypeW; external shlwapidll name 'PathIsContentTypeW';
function PathIsContentType; external shlwapidll name 'PathIsContentType'+AWSuffix;
function PathIsURLA; external shlwapidll name 'PathIsURLA';
function PathIsURLW; external shlwapidll name 'PathIsURLW';
function PathIsURL; external shlwapidll name 'PathIsURL'+AWSuffix;
function PathMakePrettyA; external shlwapidll name 'PathMakePrettyA';
function PathMakePrettyW; external shlwapidll name 'PathMakePrettyW';
function PathMakePretty; external shlwapidll name 'PathMakePretty'+AWSuffix;
function PathMatchSpecA; external shlwapidll name 'PathMatchSpecA';
function PathMatchSpecW; external shlwapidll name 'PathMatchSpecW';
function PathMatchSpec; external shlwapidll name 'PathMatchSpec'+AWSuffix;
function PathParseIconLocationA; external shlwapidll name 'PathParseIconLocationA';
function PathParseIconLocationW; external shlwapidll name 'PathParseIconLocationW';
function PathParseIconLocation; external shlwapidll name 'PathParseIconLocation'+AWSuffix;
procedure PathQuoteSpacesA; external shlwapidll name 'PathQuoteSpacesA';
procedure PathQuoteSpacesW; external shlwapidll name 'PathQuoteSpacesW';
procedure PathQuoteSpaces; external shlwapidll name 'PathQuoteSpaces'+AWSuffix;
function PathRelativePathToA; external shlwapidll name 'PathRelativePathToA';
function PathRelativePathToW; external shlwapidll name 'PathRelativePathToW';
function PathRelativePathTo; external shlwapidll name 'PathRelativePathTo'+AWSuffix;
procedure PathRemoveArgsA; external shlwapidll name 'PathRemoveArgsA';
procedure PathRemoveArgsW; external shlwapidll name 'PathRemoveArgsW';
procedure PathRemoveArgs; external shlwapidll name 'PathRemoveArgs'+AWSuffix;
function PathRemoveBackslashA; external shlwapidll name 'PathRemoveBackslashA';
function PathRemoveBackslashW; external shlwapidll name 'PathRemoveBackslashW';
function PathRemoveBackslash; external shlwapidll name 'PathRemoveBackslash'+AWSuffix;
procedure PathRemoveBlanksA; external shlwapidll name 'PathRemoveBlanksA';
procedure PathRemoveBlanksW; external shlwapidll name 'PathRemoveBlanksW';
procedure PathRemoveBlanks; external shlwapidll name 'PathRemoveBlanks'+AWSuffix;
procedure PathRemoveExtensionA; external shlwapidll name 'PathRemoveExtensionA';
procedure PathRemoveExtensionW; external shlwapidll name 'PathRemoveExtensionW';
procedure PathRemoveExtension; external shlwapidll name 'PathRemoveExtension'+AWSuffix;
function PathRemoveFileSpecA; external shlwapidll name 'PathRemoveFileSpecA';
function PathRemoveFileSpecW; external shlwapidll name 'PathRemoveFileSpecW';
function PathRemoveFileSpec; external shlwapidll name 'PathRemoveFileSpec'+AWSuffix;
function PathRenameExtensionA; external shlwapidll name 'PathRenameExtensionA';
function PathRenameExtensionW; external shlwapidll name 'PathRenameExtensionW';
function PathRenameExtension; external shlwapidll name 'PathRenameExtension'+AWSuffix;
function PathSearchAndQualifyA; external shlwapidll name 'PathSearchAndQualifyA';
function PathSearchAndQualifyW; external shlwapidll name 'PathSearchAndQualifyW';
function PathSearchAndQualify; external shlwapidll name 'PathSearchAndQualify'+AWSuffix;
procedure PathSetDlgItemPathA; external shlwapidll name 'PathSetDlgItemPathA';
procedure PathSetDlgItemPathW; external shlwapidll name 'PathSetDlgItemPathW';
procedure PathSetDlgItemPath; external shlwapidll name 'PathSetDlgItemPath'+AWSuffix;
function PathSkipRootA; external shlwapidll name 'PathSkipRootA';
function PathSkipRootW; external shlwapidll name 'PathSkipRootW';
function PathSkipRoot; external shlwapidll name 'PathSkipRoot'+AWSuffix;
procedure PathStripPathA; external shlwapidll name 'PathStripPathA';
procedure PathStripPathW; external shlwapidll name 'PathStripPathW';
procedure PathStripPath; external shlwapidll name 'PathStripPath'+AWSuffix;
function PathStripToRootA; external shlwapidll name 'PathStripToRootA';
function PathStripToRootW; external shlwapidll name 'PathStripToRootW';
function PathStripToRoot; external shlwapidll name 'PathStripToRoot'+AWSuffix;
procedure PathUnquoteSpacesA; external shlwapidll name 'PathUnquoteSpacesA';
procedure PathUnquoteSpacesW; external shlwapidll name 'PathUnquoteSpacesW';
procedure PathUnquoteSpaces; external shlwapidll name 'PathUnquoteSpaces'+AWSuffix;
function PathMakeSystemFolderA; external shlwapidll name 'PathMakeSystemFolderA';
function PathMakeSystemFolderW; external shlwapidll name 'PathMakeSystemFolderW';
function PathMakeSystemFolder; external shlwapidll name 'PathMakeSystemFolder'+AWSuffix;
function PathUnmakeSystemFolderA; external shlwapidll name 'PathUnmakeSystemFolderA';
function PathUnmakeSystemFolderW; external shlwapidll name 'PathUnmakeSystemFolderW';
function PathUnmakeSystemFolder; external shlwapidll name 'PathUnmakeSystemFolder'+AWSuffix;
function PathIsSystemFolderA; external shlwapidll name 'PathIsSystemFolderA';
function PathIsSystemFolderW; external shlwapidll name 'PathIsSystemFolderW';
function PathIsSystemFolder; external shlwapidll name 'PathIsSystemFolder'+AWSuffix;
procedure PathUndecorateA; external shlwapidll name 'PathUndecorateA';
procedure PathUndecorateW; external shlwapidll name 'PathUndecorateW';
procedure PathUndecorate; external shlwapidll name 'PathUndecorate'+AWSuffix;
function PathUnExpandEnvStringsA; external shlwapidll name 'PathUnExpandEnvStringsA';
function PathUnExpandEnvStringsW; external shlwapidll name 'PathUnExpandEnvStringsW';
function PathUnExpandEnvStrings; external shlwapidll name 'PathUnExpandEnvStrings'+AWSuffix;
function UrlCompareA; external shlwapidll name 'UrlCompareA';
function UrlCompareW; external shlwapidll name 'UrlCompareW';
function UrlCompare; external shlwapidll name 'UrlCompare'+AWSuffix;
function UrlCombineA; external shlwapidll name 'UrlCombineA';
function UrlCombineW; external shlwapidll name 'UrlCombineW';
function UrlCombine; external shlwapidll name 'UrlCombine'+AWSuffix;
function UrlCanonicalizeA; external shlwapidll name 'UrlCanonicalizeA';
function UrlCanonicalizeW; external shlwapidll name 'UrlCanonicalizeW';
function UrlCanonicalize; external shlwapidll name 'UrlCanonicalize'+AWSuffix;
function UrlIsOpaqueA; external shlwapidll name 'UrlIsOpaqueA';
function UrlIsOpaqueW; external shlwapidll name 'UrlIsOpaqueW';
function UrlIsOpaque; external shlwapidll name 'UrlIsOpaque'+AWSuffix;
function UrlIsNoHistoryA; external shlwapidll name 'UrlIsNoHistoryA';
function UrlIsNoHistoryW; external shlwapidll name 'UrlIsNoHistoryW';
function UrlIsNoHistory; external shlwapidll name 'UrlIsNoHistory'+AWSuffix;
function UrlIsA; external shlwapidll name 'UrlIsA';
function UrlIsW; external shlwapidll name 'UrlIsW';
function UrlIs; external shlwapidll name 'UrlIs'+AWSuffix;
function UrlGetLocationA; external shlwapidll name 'UrlGetLocationA';
function UrlGetLocationW; external shlwapidll name 'UrlGetLocationW';
function UrlGetLocation; external shlwapidll name 'UrlGetLocation'+AWSuffix;
function UrlUnescapeA; external shlwapidll name 'UrlUnescapeA';
function UrlUnescapeW; external shlwapidll name 'UrlUnescapeW';
function UrlUnescape; external shlwapidll name 'UrlUnescape'+AWSuffix;
function UrlEscapeA; external shlwapidll name 'UrlEscapeA';
function UrlEscapeW; external shlwapidll name 'UrlEscapeW';
function UrlEscape; external shlwapidll name 'UrlEscape'+AWSuffix;
function UrlCreateFromPathA; external shlwapidll name 'UrlCreateFromPathA';
function UrlCreateFromPathW; external shlwapidll name 'UrlCreateFromPathW';
function UrlCreateFromPath; external shlwapidll name 'UrlCreateFromPath'+AWSuffix;
function PathCreateFromUrlA; external shlwapidll name 'PathCreateFromUrlA';
function PathCreateFromUrlW; external shlwapidll name 'PathCreateFromUrlW';
function PathCreateFromUrl; external shlwapidll name 'PathCreateFromUrl'+AWSuffix;
function UrlHashA; external shlwapidll name 'UrlHashA';
function UrlHashW; external shlwapidll name 'UrlHashW';
function UrlHash; external shlwapidll name 'UrlHash'+AWSuffix;
function UrlGetPartW; external shlwapidll name 'UrlGetPartW';
function UrlGetPartA; external shlwapidll name 'UrlGetPartA';
function UrlGetPart; external shlwapidll name 'UrlGetPart'+AWSuffix;
function UrlApplySchemeA; external shlwapidll name 'UrlApplySchemeA';
function UrlApplySchemeW; external shlwapidll name 'UrlApplySchemeW';
function UrlApplyScheme; external shlwapidll name 'UrlApplyScheme'+AWSuffix;
function HashData; external shlwapidll name 'HashData';
function SHDeleteEmptyKeyA; external shlwapidll name 'SHDeleteEmptyKeyA';
function SHDeleteEmptyKeyW; external shlwapidll name 'SHDeleteEmptyKeyW';
function SHDeleteEmptyKey; external shlwapidll name 'SHDeleteEmptyKey'+AWSuffix;
function SHDeleteKeyA; external shlwapidll name 'SHDeleteKeyA';
function SHDeleteKeyW; external shlwapidll name 'SHDeleteKeyW';
function SHDeleteKey; external shlwapidll name 'SHDeleteKey'+AWSuffix;
function SHRegDuplicateHKey; external shlwapidll name 'SHRegDuplicateHKey';
function SHDeleteValueA; external shlwapidll name 'SHDeleteValueA';
function SHDeleteValueW; external shlwapidll name 'SHDeleteValueW';
function SHDeleteValue; external shlwapidll name 'SHDeleteValue'+AWSuffix;
function SHGetValueA; external shlwapidll name 'SHGetValueA';
function SHGetValueW; external shlwapidll name 'SHGetValueW';
function SHGetValue; external shlwapidll name 'SHGetValue'+AWSuffix;
function SHSetValueA; external shlwapidll name 'SHSetValueA';
function SHSetValueW; external shlwapidll name 'SHSetValueW';
function SHSetValue; external shlwapidll name 'SHSetValue'+AWSuffix;
function SHQueryValueExA; external shlwapidll name 'SHQueryValueExA';
function SHQueryValueExW; external shlwapidll name 'SHQueryValueExW';
function SHQueryValueEx; external shlwapidll name 'SHQueryValueEx'+AWSuffix;
function SHEnumKeyExA; external shlwapidll name 'SHEnumKeyExA';
function SHEnumKeyExW; external shlwapidll name 'SHEnumKeyExW';
function SHEnumKeyEx; external shlwapidll name 'SHEnumKeyEx'+AWSuffix;
function SHEnumValueA; external shlwapidll name 'SHEnumValueA';
function SHEnumValueW; external shlwapidll name 'SHEnumValueW';
function SHEnumValue; external shlwapidll name 'SHEnumValue'+AWSuffix;
function SHQueryInfoKeyA; external shlwapidll name 'SHQueryInfoKeyA';
function SHQueryInfoKeyW; external shlwapidll name 'SHQueryInfoKeyW';
function SHQueryInfoKey; external shlwapidll name 'SHQueryInfoKey'+AWSuffix;
function SHCopyKeyA; external shlwapidll name 'SHCopyKeyA';
function SHCopyKeyW; external shlwapidll name 'SHCopyKeyW';
function SHCopyKey; external shlwapidll name 'SHCopyKey'+AWSuffix;
function SHRegGetPathA; external shlwapidll name 'SHRegGetPathA';
function SHRegGetPathW; external shlwapidll name 'SHRegGetPathW';
function SHRegGetPath; external shlwapidll name 'SHRegGetPath'+AWSuffix;
function SHRegSetPathA; external shlwapidll name 'SHRegSetPathA';
function SHRegSetPathW; external shlwapidll name 'SHRegSetPathW';
function SHRegSetPath; external shlwapidll name 'SHRegSetPath'+AWSuffix;
function SHRegCreateUSKeyA; external shlwapidll name 'SHRegCreateUSKeyA';
function SHRegCreateUSKeyW; external shlwapidll name 'SHRegCreateUSKeyW';
function SHRegCreateUSKey; external shlwapidll name 'SHRegCreateUSKey'+AWSuffix;
function SHRegOpenUSKeyA; external shlwapidll name 'SHRegOpenUSKeyA';
function SHRegOpenUSKeyW; external shlwapidll name 'SHRegOpenUSKeyW';
function SHRegOpenUSKey; external shlwapidll name 'SHRegOpenUSKey'+AWSuffix;
function SHRegQueryUSValueA; external shlwapidll name 'SHRegQueryUSValueA';
function SHRegQueryUSValueW; external shlwapidll name 'SHRegQueryUSValueW';
function SHRegQueryUSValue; external shlwapidll name 'SHRegQueryUSValue'+AWSuffix;
function SHRegWriteUSValueA; external shlwapidll name 'SHRegWriteUSValueA';
function SHRegWriteUSValueW; external shlwapidll name 'SHRegWriteUSValueW';
function SHRegWriteUSValue; external shlwapidll name 'SHRegWriteUSValue'+AWSuffix;
function SHRegDeleteUSValueA; external shlwapidll name 'SHRegDeleteUSValueA';
function SHRegDeleteUSValueW; external shlwapidll name 'SHRegDeleteUSValueW';
function SHRegDeleteUSValue; external shlwapidll name 'SHRegDeleteUSValue'+AWSuffix;
function SHRegDeleteEmptyUSKeyW; external shlwapidll name 'SHRegDeleteEmptyUSKeyW';
function SHRegDeleteEmptyUSKeyA; external shlwapidll name 'SHRegDeleteEmptyUSKeyA';
function SHRegDeleteEmptyUSKey; external shlwapidll name 'SHRegDeleteEmptyUSKey'+AWSuffix;
function SHRegEnumUSKeyA; external shlwapidll name 'SHRegEnumUSKeyA';
function SHRegEnumUSKeyW; external shlwapidll name 'SHRegEnumUSKeyW';
function SHRegEnumUSKey; external shlwapidll name 'SHRegEnumUSKey'+AWSuffix;
function SHRegEnumUSValueA; external shlwapidll name 'SHRegEnumUSValueA';
function SHRegEnumUSValueW; external shlwapidll name 'SHRegEnumUSValueW';
function SHRegEnumUSValue; external shlwapidll name 'SHRegEnumUSValue'+AWSuffix;
function SHRegQueryInfoUSKeyA; external shlwapidll name 'SHRegQueryInfoUSKeyA';
function SHRegQueryInfoUSKeyW; external shlwapidll name 'SHRegQueryInfoUSKeyW';
function SHRegQueryInfoUSKey; external shlwapidll name 'SHRegQueryInfoUSKey'+AWSuffix;
function SHRegCloseUSKey; external shlwapidll name 'SHRegCloseUSKey';
function SHRegGetUSValueA; external shlwapidll name 'SHRegGetUSValueA';
function SHRegGetUSValueW; external shlwapidll name 'SHRegGetUSValueW';
function SHRegGetUSValue; external shlwapidll name 'SHRegGetUSValue'+AWSuffix;
function SHRegSetUSValueA; external shlwapidll name 'SHRegSetUSValueA';
function SHRegSetUSValueW; external shlwapidll name 'SHRegSetUSValueW';
function SHRegSetUSValue; external shlwapidll name 'SHRegSetUSValue'+AWSuffix;
function SHRegGetBoolUSValueA; external shlwapidll name 'SHRegGetBoolUSValueA';
function SHRegGetBoolUSValueW; external shlwapidll name 'SHRegGetBoolUSValueW';
function SHRegGetBoolUSValue; external shlwapidll name 'SHRegGetBoolUSValue'+AWSuffix;
function AssocCreate; external shlwapidll name 'AssocCreate';
function AssocQueryStringA; external shlwapidll name 'AssocQueryStringA';
function AssocQueryStringW; external shlwapidll name 'AssocQueryStringW';
function AssocQueryString; external shlwapidll name 'AssocQueryString'+AWSuffix;
function AssocQueryStringByKeyA; external shlwapidll name 'AssocQueryStringByKeyA';
function AssocQueryStringByKeyW; external shlwapidll name 'AssocQueryStringByKeyW';
function AssocQueryStringByKey; external shlwapidll name 'AssocQueryStringByKey'+AWSuffix;
function AssocQueryKeyA; external shlwapidll name 'AssocQueryKeyA';
function AssocQueryKeyW; external shlwapidll name 'AssocQueryKeyW';
function AssocQueryKey; external shlwapidll name 'AssocQueryKey'+AWSuffix;
function SHOpenRegStreamA; external shlwapidll name 'SHOpenRegStream2A';
function SHOpenRegStreamW; external shlwapidll name 'SHOpenRegStream2W';
function SHOpenRegStream; external shlwapidll name 'SHOpenRegStream2'+AWSuffix;
function SHOpenRegStream2A; external shlwapidll name 'SHOpenRegStream2A';
function SHOpenRegStream2W; external shlwapidll name 'SHOpenRegStream2W';
function SHOpenRegStream2; external shlwapidll name 'SHOpenRegStream2'+AWSuffix;
function SHCreateStreamOnFileA; external shlwapidll name 'SHCreateStreamOnFileA';
function SHCreateStreamOnFileW; external shlwapidll name 'SHCreateStreamOnFileW';
function SHCreateStreamOnFile; external shlwapidll name 'SHCreateStreamOnFile'+AWSuffix;
function SHAutoComplete; external shlwapidll name 'SHAutoComplete';
function SHSetThreadRef; external shlwapidll name 'SHSetThreadRef';
function SHGetThreadRef; external shlwapidll name 'SHGetThreadRef';
function SHSkipJunction; external shlwapidll name 'SHSkipJunction';
function SHCreateThread; external shlwapidll name 'SHCreateThread';
function SHReleaseThreadRef; external shlwapidll name 'SHReleaseThreadRef';
function SHCreateShellPalette; external shlwapidll name 'SHCreateShellPalette';
procedure ColorRGBToHLS; external shlwapidll name 'ColorRGBToHLS';
function ColorHLSToRGB; external shlwapidll name 'ColorHLSToRGB';
function ColorAdjustLuma; external shlwapidll name 'ColorAdjustLuma';

function StrToInt64ExA; external shlwapidll name 'StrToInt64ExA';
function StrToInt64ExW; external shlwapidll name 'StrToInt64ExW';
function StrToInt64Ex; external shlwapidll name 'StrToInt64Ex'+AWSuffix;
function IsCharSpaceA; external shlwapidll name 'IsCharSpaceA';
function IsCharSpaceW; external shlwapidll name 'IsCharSpaceW';
function IsCharSpace; external shlwapidll name 'IsCharSpace'+AWSuffix;
function StrCmpCA; external shlwapidll name 'StrCmpCA';
function StrCmpCW; external shlwapidll name 'StrCmpCW';
function StrCmpC; external shlwapidll name 'StrCmpC'+AWSuffix;
function StrCmpICA; external shlwapidll name 'StrCmpICA';
function StrCmpICW; external shlwapidll name 'StrCmpICW';
function StrCmpIC; external shlwapidll name 'StrCmpIC'+AWSuffix;
function SHRegGetValueA; external shlwapidll name 'SHRegGetValueA';
function SHRegGetValueW; external shlwapidll name 'SHRegGetValueW';
function SHRegGetValue; external shlwapidll name 'SHRegGetValue'+AWSuffix;
function SHRegGetIntW; external shlwapidll name 'SHRegGetIntW';
function SHRegGetInt; external shlwapidll name 'SHRegGetIntW';
function AssocIsDangerous; external shlwapidll name 'AssocIsDangerous';
function AssocGetPerceivedType; external shlwapidll name 'AssocGetPerceivedType';
function SHCreateStreamOnFileEx; external shlwapidll name 'SHCreateStreamOnFileEx';
function GetAcceptLanguagesA; external shlwapidll name 'GetAcceptLanguagesA';
function GetAcceptLanguagesW; external shlwapidll name 'GetAcceptLanguagesW';
function GetAcceptLanguages; external shlwapidll name 'GetAcceptLanguages'+AWSuffix;
function SHGetViewStatePropertyBag; external shlwapidll name 'SHGetViewStatePropertyBag';
function SHAllocShared; external shlwapidll name 'SHAllocShared';
function SHFreeShared; external shlwapidll name 'SHFreeShared';
function SHLockShared; external shlwapidll name 'SHLockShared';
function SHUnlockShared; external shlwapidll name 'SHUnlockShared';
function SHCreateThreadRef; external shlwapidll name 'SHCreateThreadRef';
function IsInternetESCEnabled; external shlwapidll name 'IsInternetESCEnabled';

{$IFDEF WINXP_UP}
//stOrM!------------------------------------------------------------------------------------------------------------------------------------------

function MessageBoxTimeOut;  external user32 name 'MessageBoxTimeout'+AWSuffix;
function MessageBoxTimeOutA; external user32 name 'MessageBoxTimeoutA';
function MessageBoxTimeOutW; external user32 name 'MessageBoxTimeoutW';

//------------------------------------------------------------------------------------------------------------------------------------------stOrM!
{$ENDIF WINXP_UP}

//stOrM!------------------------------------------------------------------------------------------------------------------------------------------

{$IFDEF WIN2000_UP}

//function MessageBoxCheck, function MessageBoxCheckA, function MessageBoxCheckW

function MessageBoxCheck; external shlwapidll Index {$IFDEF UNICODE}191{$ELSE}185{$ENDIF UNICODE};
function MessageBoxCheckA; external shlwapidll Index 185; //'SHMessageBoxCheckA'
function MessageBoxCheckW; external shlwapidll Index 191; //'SHMessageBoxCheckW'

{$ENDIF WIN2000_UP}

//------------------------------------------------------------------------------------------------------------------------------------------stOrM!
{$ELSE}
var
  _StrChrA: Pointer;

function StrChrA;
begin
  GetProcedureAddress(_StrChrA, shlwapidll, 'StrChrA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrChrA]
  end;
end;

var
  _StrChrW: Pointer;

function StrChrW;
begin
  GetProcedureAddress(_StrChrW, shlwapidll, 'StrChrW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrChrW]
  end;
end;

var
  _StrChr: Pointer;

function StrChr;
begin
  GetProcedureAddress(_StrChr, shlwapidll, 'StrChr'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrChr]
  end;
end;

var
  _StrChrIA: Pointer;

function StrChrIA;
begin
  GetProcedureAddress(_StrChrIA, shlwapidll, 'StrChrIA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrChrIA]
  end;
end;

var
  _StrChrIW: Pointer;

function StrChrIW;
begin
  GetProcedureAddress(_StrChrIW, shlwapidll, 'StrChrIW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrChrIW]
  end;
end;

var
  _StrChrI: Pointer;

function StrChrI;
begin
  GetProcedureAddress(_StrChrI, shlwapidll, 'StrChrI'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrChrI]
  end;
end;

var
  _StrCmpNA: Pointer;

function StrCmpNA;
begin
  GetProcedureAddress(_StrCmpNA, shlwapidll, 'StrCmpNA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCmpNA]
  end;
end;

var
  _StrCmpNW: Pointer;

function StrCmpNW;
begin
  GetProcedureAddress(_StrCmpNW, shlwapidll, 'StrCmpNW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCmpNW]
  end;
end;

var
  _StrCmpN: Pointer;

function StrCmpN;
begin
  GetProcedureAddress(_StrCmpN, shlwapidll, 'StrCmpN'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCmpN]
  end;
end;

var
  _StrCmpNIA: Pointer;

function StrCmpNIA;
begin
  GetProcedureAddress(_StrCmpNIA, shlwapidll, 'StrCmpNIA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCmpNIA]
  end;
end;

var
  _StrCmpNIW: Pointer;

function StrCmpNIW;
begin
  GetProcedureAddress(_StrCmpNIW, shlwapidll, 'StrCmpNIW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCmpNIW]
  end;
end;

var
  _StrCmpNI: Pointer;

function StrCmpNI;
begin
  GetProcedureAddress(_StrCmpNI, shlwapidll, 'StrCmpNI'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCmpNI]
  end;
end;

var
  _StrCSpnA: Pointer;

function StrCSpnA;
begin
  GetProcedureAddress(_StrCSpnA, shlwapidll, 'StrCSpnA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCSpnA]
  end;
end;

var
  _StrCSpnW: Pointer;

function StrCSpnW;
begin
  GetProcedureAddress(_StrCSpnW, shlwapidll, 'StrCSpnW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCSpnW]
  end;
end;

var
  _StrCSpn: Pointer;

function StrCSpn;
begin
  GetProcedureAddress(_StrCSpn, shlwapidll, 'StrCSpn'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCSpn]
  end;
end;

var
  _StrCSpnIA: Pointer;

function StrCSpnIA;
begin
  GetProcedureAddress(_StrCSpnIA, shlwapidll, 'StrCSpnIA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCSpnIA]
  end;
end;

var
  _StrCSpnIW: Pointer;

function StrCSpnIW;
begin
  GetProcedureAddress(_StrCSpnIW, shlwapidll, 'StrCSpnIW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCSpnIW]
  end;
end;

var
  _StrCSpnI: Pointer;

function StrCSpnI;
begin
  GetProcedureAddress(_StrCSpnI, shlwapidll, 'StrCSpnI'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCSpnI]
  end;
end;

var
  _StrDupA: Pointer;

function StrDupA;
begin
  GetProcedureAddress(_StrDupA, shlwapidll, 'StrDupA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrDupA]
  end;
end;

var
  _StrDupW: Pointer;

function StrDupW;
begin
  GetProcedureAddress(_StrDupW, shlwapidll, 'StrDupW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrDupW]
  end;
end;

var
  _StrDup: Pointer;

function StrDup;
begin
  GetProcedureAddress(_StrDup, shlwapidll, 'StrDup'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrDup]
  end;
end;

var
  _StrFormatByteSizeA: Pointer;

function StrFormatByteSizeA;
begin
  GetProcedureAddress(_StrFormatByteSizeA, shlwapidll, 'StrFormatByteSizeA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrFormatByteSizeA]
  end;
end;

var
  _StrFormatByteSize64A: Pointer;

function StrFormatByteSize64A;
begin
  GetProcedureAddress(_StrFormatByteSize64A, shlwapidll, 'StrFormatByteSize64A');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrFormatByteSize64A]
  end;
end;

var
  _StrFormatByteSizeW: Pointer;

function StrFormatByteSizeW;
begin
  GetProcedureAddress(_StrFormatByteSizeW, shlwapidll, 'StrFormatByteSizeW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrFormatByteSizeW]
  end;
end;

var
  _StrFormatByteSize: Pointer;

function StrFormatByteSize;
begin
  GetProcedureAddress(_StrFormatByteSize, shlwapidll, 'StrFormatByteSize'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrFormatByteSize]
  end;
end;

var
  _StrFormatByteSize64: Pointer;

function StrFormatByteSize64;
begin
  GetProcedureAddress(_StrFormatByteSize64, shlwapidll, 'StrFormatByteSize64');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrFormatByteSize64]
  end;
end;

var
  _StrFormatKBSizeW: Pointer;

function StrFormatKBSizeW;
begin
  GetProcedureAddress(_StrFormatKBSizeW, shlwapidll, 'StrFormatKBSizeW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrFormatKBSizeW]
  end;
end;

var
  _StrFormatKBSizeA: Pointer;

function StrFormatKBSizeA;
begin
  GetProcedureAddress(_StrFormatKBSizeA, shlwapidll, 'StrFormatKBSizeA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrFormatKBSizeA]
  end;
end;

var
  _StrFormatKBSize: Pointer;

function StrFormatKBSize;
begin
  GetProcedureAddress(_StrFormatKBSize, shlwapidll, 'StrFormatKBSize'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrFormatKBSize]
  end;
end;

var
  _StrFromTimeIntervalA: Pointer;

function StrFromTimeIntervalA;
begin
  GetProcedureAddress(_StrFromTimeIntervalA, shlwapidll, 'StrFromTimeIntervalA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrFromTimeIntervalA]
  end;
end;

var
  _StrFromTimeIntervalW: Pointer;

function StrFromTimeIntervalW;
begin
  GetProcedureAddress(_StrFromTimeIntervalW, shlwapidll, 'StrFromTimeIntervalW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrFromTimeIntervalW]
  end;
end;

var
  _StrFromTimeInterval: Pointer;

function StrFromTimeInterval;
begin
  GetProcedureAddress(_StrFromTimeInterval, shlwapidll, 'StrFromTimeInterval'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrFromTimeInterval]
  end;
end;

var
  _StrIsIntlEqualA: Pointer;

function StrIsIntlEqualA;
begin
  GetProcedureAddress(_StrIsIntlEqualA, shlwapidll, 'StrIsIntlEqualA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrIsIntlEqualA]
  end;
end;

var
  _StrIsIntlEqualW: Pointer;

function StrIsIntlEqualW;
begin
  GetProcedureAddress(_StrIsIntlEqualW, shlwapidll, 'StrIsIntlEqualW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrIsIntlEqualW]
  end;
end;

var
  _StrIsIntlEqual: Pointer;

function StrIsIntlEqual;
begin
  GetProcedureAddress(_StrIsIntlEqual, shlwapidll, 'StrIsIntlEqual'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrIsIntlEqual]
  end;
end;

var
  _StrNCatA: Pointer;

function StrNCatA;
begin
  GetProcedureAddress(_StrNCatA, shlwapidll, 'StrNCatA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrNCatA]
  end;
end;

var
  _StrNCatW: Pointer;

function StrNCatW;
begin
  GetProcedureAddress(_StrNCatW, shlwapidll, 'StrNCatW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrNCatW]
  end;
end;

var
  _StrNCat: Pointer;

function StrNCat;
begin
  GetProcedureAddress(_StrNCat, shlwapidll, 'StrNCat'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrNCat]
  end;
end;

var
  _StrPBrkA: Pointer;

function StrPBrkA;
begin
  GetProcedureAddress(_StrPBrkA, shlwapidll, 'StrPBrkA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrPBrkA]
  end;
end;

var
  _StrPBrkW: Pointer;

function StrPBrkW;
begin
  GetProcedureAddress(_StrPBrkW, shlwapidll, 'StrPBrkW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrPBrkW]
  end;
end;

var
  _StrPBrk: Pointer;

function StrPBrk;
begin
  GetProcedureAddress(_StrPBrk, shlwapidll, 'StrPBrk'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrPBrk]
  end;
end;

var
  _StrRChrA: Pointer;

function StrRChrA;
begin
  GetProcedureAddress(_StrRChrA, shlwapidll, 'StrRChrA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrRChrA]
  end;
end;

var
  _StrRChrW: Pointer;

function StrRChrW;
begin
  GetProcedureAddress(_StrRChrW, shlwapidll, 'StrRChrW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrRChrW]
  end;
end;

var
  _StrRChr: Pointer;

function StrRChr;
begin
  GetProcedureAddress(_StrRChr, shlwapidll, 'StrRChr'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrRChr]
  end;
end;

var
  _StrRChrIA: Pointer;

function StrRChrIA;
begin
  GetProcedureAddress(_StrRChrIA, shlwapidll, 'StrRChrIA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrRChrIA]
  end;
end;

var
  _StrRChrIW: Pointer;

function StrRChrIW;
begin
  GetProcedureAddress(_StrRChrIW, shlwapidll, 'StrRChrIW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrRChrIW]
  end;
end;

var
  _StrRChrI: Pointer;

function StrRChrI;
begin
  GetProcedureAddress(_StrRChrI, shlwapidll, 'StrRChrI'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrRChrI]
  end;
end;

var
  _StrRStrIA: Pointer;

function StrRStrIA;
begin
  GetProcedureAddress(_StrRStrIA, shlwapidll, 'StrRStrIA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrRStrIA]
  end;
end;

var
  _StrRStrIW: Pointer;

function StrRStrIW;
begin
  GetProcedureAddress(_StrRStrIW, shlwapidll, 'StrRStrIW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrRStrIW]
  end;
end;

var
  _StrRStrI: Pointer;

function StrRStrI;
begin
  GetProcedureAddress(_StrRStrI, shlwapidll, 'StrRStrI'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrRStrI]
  end;
end;

var
  _StrSpnA: Pointer;

function StrSpnA;
begin
  GetProcedureAddress(_StrSpnA, shlwapidll, 'StrSpnA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrSpnA]
  end;
end;

var
  _StrSpnW: Pointer;

function StrSpnW;
begin
  GetProcedureAddress(_StrSpnW, shlwapidll, 'StrSpnW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrSpnW]
  end;
end;

var
  _StrSpn: Pointer;

function StrSpn;
begin
  GetProcedureAddress(_StrSpn, shlwapidll, 'StrSpn'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrSpn]
  end;
end;

var
  _StrStrA: Pointer;

function StrStrA;
begin
  GetProcedureAddress(_StrStrA, shlwapidll, 'StrStrA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrStrA]
  end;
end;

var
  _StrStrW: Pointer;

function StrStrW;
begin
  GetProcedureAddress(_StrStrW, shlwapidll, 'StrStrW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrStrW]
  end;
end;

var
  _StrStr: Pointer;

function StrStr;
begin
  GetProcedureAddress(_StrStr, shlwapidll, 'StrStr'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrStr]
  end;
end;

var
  _StrStrIA: Pointer;

function StrStrIA;
begin
  GetProcedureAddress(_StrStrIA, shlwapidll, 'StrStrIA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrStrIA]
  end;
end;

var
  _StrStrIW: Pointer;

function StrStrIW;
begin
  GetProcedureAddress(_StrStrIW, shlwapidll, 'StrStrIW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrStrIW]
  end;
end;

var
  _StrStrI: Pointer;

function StrStrI;
begin
  GetProcedureAddress(_StrStrI, shlwapidll, 'StrStrI'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrStrI]
  end;
end;

var
  _StrToIntA: Pointer;

function StrToIntA;
begin
  GetProcedureAddress(_StrToIntA, shlwapidll, 'StrToIntA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrToIntA]
  end;
end;

var
  _StrToIntW: Pointer;

function StrToIntW;
begin
  GetProcedureAddress(_StrToIntW, shlwapidll, 'StrToIntW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrToIntW]
  end;
end;

var
  _StrToInt: Pointer;

function StrToInt;
begin
  GetProcedureAddress(_StrToInt, shlwapidll, 'StrToInt'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrToInt]
  end;
end;

var
  _StrToIntExA: Pointer;

function StrToIntExA;
begin
  GetProcedureAddress(_StrToIntExA, shlwapidll, 'StrToIntExA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrToIntExA]
  end;
end;

var
  _StrToIntExW: Pointer;

function StrToIntExW;
begin
  GetProcedureAddress(_StrToIntExW, shlwapidll, 'StrToIntExW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrToIntExW]
  end;
end;

var
  _StrToIntEx: Pointer;

function StrToIntEx;
begin
  GetProcedureAddress(_StrToIntEx, shlwapidll, 'StrToIntEx'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrToIntEx]
  end;
end;

var
  _StrTrimA: Pointer;

function StrTrimA;
begin
  GetProcedureAddress(_StrTrimA, shlwapidll, 'StrTrimA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrTrimA]
  end;
end;

var
  _StrTrimW: Pointer;

function StrTrimW;
begin
  GetProcedureAddress(_StrTrimW, shlwapidll, 'StrTrimW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrTrimW]
  end;
end;

var
  _StrTrim: Pointer;

function StrTrim;
begin
  GetProcedureAddress(_StrTrim, shlwapidll, 'StrTrim'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrTrim]
  end;
end;

var
  _StrCatW: Pointer;

function StrCatW;
begin
  GetProcedureAddress(_StrCatW, shlwapidll, 'StrCatW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCatW]
  end;
end;

var
  _StrCmpW: Pointer;

function StrCmpW;
begin
  GetProcedureAddress(_StrCmpW, shlwapidll, 'StrCmpW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCmpW]
  end;
end;

var
  _StrCmpIW: Pointer;

function StrCmpIW;
begin
  GetProcedureAddress(_StrCmpIW, shlwapidll, 'StrCmpIW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCmpIW]
  end;
end;

var
  _StrCpyW: Pointer;

function StrCpyW;
begin
  GetProcedureAddress(_StrCpyW, shlwapidll, 'StrCpyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCpyW]
  end;
end;

var
  _StrCpyNW: Pointer;

function StrCpyNW;
begin
  GetProcedureAddress(_StrCpyNW, shlwapidll, 'StrCpyNW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCpyNW]
  end;
end;

var
  _StrCatBuffW: Pointer;

function StrCatBuffW;
begin
  GetProcedureAddress(_StrCatBuffW, shlwapidll, 'StrCatBuffW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCatBuffW]
  end;
end;

var
  _StrCatBuffA: Pointer;

function StrCatBuffA;
begin
  GetProcedureAddress(_StrCatBuffA, shlwapidll, 'StrCatBuffA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCatBuffA]
  end;
end;

var
  _StrCatBuff: Pointer;

function StrCatBuff;
begin
  GetProcedureAddress(_StrCatBuff, shlwapidll, 'StrCatBuff'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCatBuff]
  end;
end;

var
  _ChrCmpIA: Pointer;

function ChrCmpIA;
begin
  GetProcedureAddress(_ChrCmpIA, shlwapidll, 'ChrCmpIA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ChrCmpIA]
  end;
end;

var
  _ChrCmpIW: Pointer;

function ChrCmpIW;
begin
  GetProcedureAddress(_ChrCmpIW, shlwapidll, 'ChrCmpIW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ChrCmpIW]
  end;
end;

var
  _ChrCmpI: Pointer;

function ChrCmpI;
begin
  GetProcedureAddress(_ChrCmpI, shlwapidll, 'ChrCmpI'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ChrCmpI]
  end;
end;

var
  _wvnsprintfA: Pointer;

function wvnsprintfA;
begin
  GetProcedureAddress(_wvnsprintfA, shlwapidll, 'wvnsprintfA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_wvnsprintfA]
  end;
end;

var
  _wvnsprintfW: Pointer;

function wvnsprintfW;
begin
  GetProcedureAddress(_wvnsprintfW, shlwapidll, 'wvnsprintfW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_wvnsprintfW]
  end;
end;

var
  _wvnsprintf: Pointer;

function wvnsprintf;
begin
  GetProcedureAddress(_wvnsprintf, shlwapidll, 'wvnsprintf'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_wvnsprintf]
  end;
end;


var
  _StrRetToStrA: Pointer;

function StrRetToStrA;
begin
  GetProcedureAddress(_StrRetToStrA, shlwapidll, 'StrRetToStrA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrRetToStrA]
  end;
end;

var
  _StrRetToStrW: Pointer;

function StrRetToStrW;
begin
  GetProcedureAddress(_StrRetToStrW, shlwapidll, 'StrRetToStrW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrRetToStrW]
  end;
end;

var
  _StrRetToStr: Pointer;

function StrRetToStr;
begin
  GetProcedureAddress(_StrRetToStr, shlwapidll, 'StrRetToStr'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrRetToStr]
  end;
end;

var
  _StrRetToBufA: Pointer;

function StrRetToBufA;
begin
  GetProcedureAddress(_StrRetToBufA, shlwapidll, 'StrRetToBufA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrRetToBufA]
  end;
end;

var
  _StrRetToBufW: Pointer;

function StrRetToBufW;
begin
  GetProcedureAddress(_StrRetToBufW, shlwapidll, 'StrRetToBufW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrRetToBufW]
  end;
end;

var
  _StrRetToBuf: Pointer;

function StrRetToBuf;
begin
  GetProcedureAddress(_StrRetToBuf, shlwapidll, 'StrRetToBuf');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrRetToBuf]
  end;
end;

var
  _StrRetToBSTR: Pointer;

function StrRetToBSTR;
begin
  GetProcedureAddress(_StrRetToBSTR, shlwapidll, 'StrRetToBSTR');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrRetToBSTR]
  end;
end;

var
  _SHStrDupA: Pointer;

function SHStrDupA;
begin
  GetProcedureAddress(_SHStrDupA, shlwapidll, 'SHStrDupA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHStrDupA]
  end;
end;

var
  _SHStrDupW: Pointer;

function SHStrDupW;
begin
  GetProcedureAddress(_SHStrDupW, shlwapidll, 'SHStrDupW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHStrDupW]
  end;
end;

var
  _SHStrDup: Pointer;

function SHStrDup;
begin
  GetProcedureAddress(_SHStrDup, shlwapidll, 'SHStrDup'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHStrDup]
  end;
end;

var
  _StrCmpLogicalW: Pointer;

function StrCmpLogicalW;
begin
  GetProcedureAddress(_StrCmpLogicalW, shlwapidll, 'StrCmpLogicalW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCmpLogicalW]
  end;
end;

var
  _StrCatChainW: Pointer;

function StrCatChainW;
begin
  GetProcedureAddress(_StrCatChainW, shlwapidll, 'StrCatChainW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCatChainW]
  end;
end;

var
  _SHLoadIndirectString: Pointer;

function SHLoadIndirectString;
begin
  GetProcedureAddress(_SHLoadIndirectString, shlwapidll, 'SHLoadIndirectString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHLoadIndirectString]
  end;
end;

var
  _IntlStrEqWorkerA: Pointer;

function IntlStrEqWorkerA;
begin
  GetProcedureAddress(_IntlStrEqWorkerA, shlwapidll, 'IntlStrEqWorkerA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_IntlStrEqWorkerA]
  end;
end;

var
  _IntlStrEqWorkerW: Pointer;

function IntlStrEqWorkerW;
begin
  GetProcedureAddress(_IntlStrEqWorkerW, shlwapidll, 'IntlStrEqWorkerW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_IntlStrEqWorkerW]
  end;
end;

var
  _StrCatA: Pointer;

function StrCatA;
begin
  GetProcedureAddress(_StrCatA, kernel32dll, 'lStrCatA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCatA]
  end;
end;

var
  _StrCmpA: Pointer;

function StrCmpA;
begin
  GetProcedureAddress(_StrCmpA, kernel32dll, 'lStrCmpA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCmpA]
  end;
end;

var
  _StrCmpIA: Pointer;

function StrCmpIA;
begin
  GetProcedureAddress(_StrCmpIA, kernel32dll, 'lStrCmpIA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCmpIA]
  end;
end;

var
  _StrCpyA: Pointer;

function StrCpyA;
begin
  GetProcedureAddress(_StrCpyA, kernel32dll, 'lStrCpyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCpyA]
  end;
end;

var
  _StrCpyNA: Pointer;

function StrCpyNA;
begin
  GetProcedureAddress(_StrCpyNA, kernel32dll, 'lStrCpyNA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCpyNA]
  end;
end;

var
  _StrToLong: Pointer;

function StrToLong;
begin
  GetProcedureAddress(_StrToLong, shlwapidll, 'StrToLong'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrToLong]
  end;
end;

(*
var
  _StrNCmp: Pointer;

function StrNCmp;
begin
  GetProcedureAddress(_StrNCmp, kernel32dll, 'lStrNCmp'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrNCmp]
  end;
end;

var
  _StrNCmpI: Pointer;

function StrNCmpI;
begin
  GetProcedureAddress(_StrNCmpI, kernel32dll, 'lStrNCmpI'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrNCmpI]
  end;
end;
        *)
var
  _StrNCpy: Pointer;

function StrNCpy;
begin
  GetProcedureAddress(_StrNCpy, kernel32dll, 'lStrCpyn'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrNCpy]
  end;
end;
(*
var
  _StrCatN: Pointer;

function StrCatN;
begin
  GetProcedureAddress(_StrCatN, shlwapidll, 'StrCatN'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCatN]
  end;
end;
  *)
var
  _StrCat: Pointer;

function StrCat;
begin
  GetProcedureAddress(_StrCat, kernel32dll, 'lStrCat'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCat]
  end;
end;

var
  _StrCmp: Pointer;

function StrCmp;
begin
  GetProcedureAddress(_StrCmp, kernel32dll, 'lStrCmp'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCmp]
  end;
end;

var
  _StrCmpI: Pointer;

function StrCmpI;
begin
  GetProcedureAddress(_StrCmpI, kernel32dll, 'lStrCmpI'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCmpI]
  end;
end;

var
  _StrCpy: Pointer;

function StrCpy;
begin
  GetProcedureAddress(_StrCpy, kernel32dll, 'lStrCpy'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCpy]
  end;
end;

var
  _StrCpyN: Pointer;

function StrCpyN;
begin
  GetProcedureAddress(_StrCpyN, kernel32dll, 'lStrCpyN'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCpyN]
  end;
end;

var
  _PathAddBackslashA: Pointer;

function PathAddBackslashA;
begin
  GetProcedureAddress(_PathAddBackslashA, shlwapidll, 'PathAddBackslashA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathAddBackslashA]
  end;
end;

var
  _PathAddBackslashW: Pointer;

function PathAddBackslashW;
begin
  GetProcedureAddress(_PathAddBackslashW, shlwapidll, 'PathAddBackslashW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathAddBackslashW]
  end;
end;

var
  _PathAddBackslash: Pointer;

function PathAddBackslash;
begin
  GetProcedureAddress(_PathAddBackslash, shlwapidll, 'PathAddBackslash'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathAddBackslash]
  end;
end;

var
  _PathAddExtensionA: Pointer;

function PathAddExtensionA;
begin
  GetProcedureAddress(_PathAddExtensionA, shlwapidll, 'PathAddExtensionA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathAddExtensionA]
  end;
end;

var
  _PathAddExtensionW: Pointer;

function PathAddExtensionW;
begin
  GetProcedureAddress(_PathAddExtensionW, shlwapidll, 'PathAddExtensionW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathAddExtensionW]
  end;
end;

var
  _PathAddExtension: Pointer;

function PathAddExtension;
begin
  GetProcedureAddress(_PathAddExtension, shlwapidll, 'PathAddExtension'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathAddExtension]
  end;
end;

var
  _PathAppendA: Pointer;

function PathAppendA;
begin
  GetProcedureAddress(_PathAppendA, shlwapidll, 'PathAppendA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathAppendA]
  end;
end;

var
  _PathAppendW: Pointer;

function PathAppendW;
begin
  GetProcedureAddress(_PathAppendW, shlwapidll, 'PathAppendW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathAppendW]
  end;
end;

var
  _PathAppend: Pointer;

function PathAppend;
begin
  GetProcedureAddress(_PathAppend, shlwapidll, 'PathAppend'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathAppend]
  end;
end;

var
  _PathBuildRootA: Pointer;

function PathBuildRootA;
begin
  GetProcedureAddress(_PathBuildRootA, shlwapidll, 'PathBuildRootA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathBuildRootA]
  end;
end;

var
  _PathBuildRootW: Pointer;

function PathBuildRootW;
begin
  GetProcedureAddress(_PathBuildRootW, shlwapidll, 'PathBuildRootW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathBuildRootW]
  end;
end;

var
  _PathBuildRoot: Pointer;

function PathBuildRoot;
begin
  GetProcedureAddress(_PathBuildRoot, shlwapidll, 'PathBuildRoot'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathBuildRoot]
  end;
end;

var
  _PathCanonicalizeA: Pointer;

function PathCanonicalizeA;
begin
  GetProcedureAddress(_PathCanonicalizeA, shlwapidll, 'PathCanonicalizeA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathCanonicalizeA]
  end;
end;

var
  _PathCanonicalizeW: Pointer;

function PathCanonicalizeW;
begin
  GetProcedureAddress(_PathCanonicalizeW, shlwapidll, 'PathCanonicalizeW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathCanonicalizeW]
  end;
end;

var
  _PathCanonicalize: Pointer;

function PathCanonicalize;
begin
  GetProcedureAddress(_PathCanonicalize, shlwapidll, 'PathCanonicalize'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathCanonicalize]
  end;
end;

var
  _PathCombineA: Pointer;

function PathCombineA;
begin
  GetProcedureAddress(_PathCombineA, shlwapidll, 'PathCombineA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathCombineA]
  end;
end;

var
  _PathCombineW: Pointer;

function PathCombineW;
begin
  GetProcedureAddress(_PathCombineW, shlwapidll, 'PathCombineW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathCombineW]
  end;
end;

var
  _PathCombine: Pointer;

function PathCombine;
begin
  GetProcedureAddress(_PathCombine, shlwapidll, 'PathCombine'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathCombine]
  end;
end;

var
  _PathCompactPathA: Pointer;

function PathCompactPathA;
begin
  GetProcedureAddress(_PathCompactPathA, shlwapidll, 'PathCompactPathA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathCompactPathA]
  end;
end;

var
  _PathCompactPathW: Pointer;

function PathCompactPathW;
begin
  GetProcedureAddress(_PathCompactPathW, shlwapidll, 'PathCompactPathW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathCompactPathW]
  end;
end;

var
  _PathCompactPath: Pointer;

function PathCompactPath;
begin
  GetProcedureAddress(_PathCompactPath, shlwapidll, 'PathCompactPath'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathCompactPath]
  end;
end;

var
  _PathCompactPathExA: Pointer;

function PathCompactPathExA;
begin
  GetProcedureAddress(_PathCompactPathExA, shlwapidll, 'PathCompactPathExA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathCompactPathExA]
  end;
end;

var
  _PathCompactPathExW: Pointer;

function PathCompactPathExW;
begin
  GetProcedureAddress(_PathCompactPathExW, shlwapidll, 'PathCompactPathExW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathCompactPathExW]
  end;
end;

var
  _PathCompactPathEx: Pointer;

function PathCompactPathEx;
begin
  GetProcedureAddress(_PathCompactPathEx, shlwapidll, 'PathCompactPathEx'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathCompactPathEx]
  end;
end;

var
  _PathCommonPrefixA: Pointer;

function PathCommonPrefixA;
begin
  GetProcedureAddress(_PathCommonPrefixA, shlwapidll, 'PathCommonPrefixA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathCommonPrefixA]
  end;
end;

var
  _PathCommonPrefixW: Pointer;

function PathCommonPrefixW;
begin
  GetProcedureAddress(_PathCommonPrefixW, shlwapidll, 'PathCommonPrefixW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathCommonPrefixW]
  end;
end;

var
  _PathCommonPrefix: Pointer;

function PathCommonPrefix;
begin
  GetProcedureAddress(_PathCommonPrefix, shlwapidll, 'PathCommonPrefix'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathCommonPrefix]
  end;
end;

var
  _PathFileExistsA: Pointer;

function PathFileExistsA;
begin
  GetProcedureAddress(_PathFileExistsA, shlwapidll, 'PathFileExistsA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathFileExistsA]
  end;
end;

var
  _PathFileExistsW: Pointer;

function PathFileExistsW;
begin
  GetProcedureAddress(_PathFileExistsW, shlwapidll, 'PathFileExistsW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathFileExistsW]
  end;
end;

var
  _PathFileExists: Pointer;

function PathFileExists;
begin
  GetProcedureAddress(_PathFileExists, shlwapidll, 'PathFileExists'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathFileExists]
  end;
end;

var
  _PathFindExtensionA: Pointer;

function PathFindExtensionA;
begin
  GetProcedureAddress(_PathFindExtensionA, shlwapidll, 'PathFindExtensionA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathFindExtensionA]
  end;
end;

var
  _PathFindExtensionW: Pointer;

function PathFindExtensionW;
begin
  GetProcedureAddress(_PathFindExtensionW, shlwapidll, 'PathFindExtensionW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathFindExtensionW]
  end;
end;

var
  _PathFindExtension: Pointer;

function PathFindExtension;
begin
  GetProcedureAddress(_PathFindExtension, shlwapidll, 'PathFindExtension'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathFindExtension]
  end;
end;

var
  _PathFindFileNameA: Pointer;

function PathFindFileNameA;
begin
  GetProcedureAddress(_PathFindFileNameA, shlwapidll, 'PathFindFileNameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathFindFileNameA]
  end;
end;

var
  _PathFindFileNameW: Pointer;

function PathFindFileNameW;
begin
  GetProcedureAddress(_PathFindFileNameW, shlwapidll, 'PathFindFileNameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathFindFileNameW]
  end;
end;

var
  _PathFindFileName: Pointer;

function PathFindFileName;
begin
  GetProcedureAddress(_PathFindFileName, shlwapidll, 'PathFindFileName'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathFindFileName]
  end;
end;

var
  _PathFindNextComponentA: Pointer;

function PathFindNextComponentA;
begin
  GetProcedureAddress(_PathFindNextComponentA, shlwapidll, 'PathFindNextComponentA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathFindNextComponentA]
  end;
end;

var
  _PathFindNextComponentW: Pointer;

function PathFindNextComponentW;
begin
  GetProcedureAddress(_PathFindNextComponentW, shlwapidll, 'PathFindNextComponentW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathFindNextComponentW]
  end;
end;

var
  _PathFindNextComponent: Pointer;

function PathFindNextComponent;
begin
  GetProcedureAddress(_PathFindNextComponent, shlwapidll, 'PathFindNextComponent'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathFindNextComponent]
  end;
end;

var
  _PathFindOnPathA: Pointer;

function PathFindOnPathA;
begin
  GetProcedureAddress(_PathFindOnPathA, shlwapidll, 'PathFindOnPathA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathFindOnPathA]
  end;
end;

var
  _PathFindOnPathW: Pointer;

function PathFindOnPathW;
begin
  GetProcedureAddress(_PathFindOnPathW, shlwapidll, 'PathFindOnPathW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathFindOnPathW]
  end;
end;

var
  _PathFindOnPath: Pointer;

function PathFindOnPath;
begin
  GetProcedureAddress(_PathFindOnPath, shlwapidll, 'PathFindOnPath'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathFindOnPath]
  end;
end;

var
  _PathGetArgsA: Pointer;

function PathGetArgsA;
begin
  GetProcedureAddress(_PathGetArgsA, shlwapidll, 'PathGetArgsA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathGetArgsA]
  end;
end;

var
  _PathGetArgsW: Pointer;

function PathGetArgsW;
begin
  GetProcedureAddress(_PathGetArgsW, shlwapidll, 'PathGetArgsW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathGetArgsW]
  end;
end;

var
  _PathGetArgs: Pointer;

function PathGetArgs;
begin
  GetProcedureAddress(_PathGetArgs, shlwapidll, 'PathGetArgs'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathGetArgs]
  end;
end;

var
  _PathFindSuffixArrayA: Pointer;

function PathFindSuffixArrayA;
begin
  GetProcedureAddress(_PathFindSuffixArrayA, shlwapidll, 'PathFindSuffixArrayA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathFindSuffixArrayA]
  end;
end;

var
  _PathFindSuffixArrayW: Pointer;

function PathFindSuffixArrayW;
begin
  GetProcedureAddress(_PathFindSuffixArrayW, shlwapidll, 'PathFindSuffixArrayW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathFindSuffixArrayW]
  end;
end;

var
  _PathFindSuffixArray: Pointer;

function PathFindSuffixArray;
begin
  GetProcedureAddress(_PathFindSuffixArray, shlwapidll, 'PathFindSuffixArray'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathFindSuffixArray]
  end;
end;

var
  _PathIsLFNFileSpecA: Pointer;

function PathIsLFNFileSpecA;
begin
  GetProcedureAddress(_PathIsLFNFileSpecA, shlwapidll, 'PathIsLFNFileSpecA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsLFNFileSpecA]
  end;
end;

var
  _PathIsLFNFileSpecW: Pointer;

function PathIsLFNFileSpecW;
begin
  GetProcedureAddress(_PathIsLFNFileSpecW, shlwapidll, 'PathIsLFNFileSpecW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsLFNFileSpecW]
  end;
end;

var
  _PathIsLFNFileSpec: Pointer;

function PathIsLFNFileSpec;
begin
  GetProcedureAddress(_PathIsLFNFileSpec, shlwapidll, 'PathIsLFNFileSpec'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsLFNFileSpec]
  end;
end;

var
  _PathGetCharTypeA: Pointer;

function PathGetCharTypeA;
begin
  GetProcedureAddress(_PathGetCharTypeA, shlwapidll, 'PathGetCharTypeA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathGetCharTypeA]
  end;
end;

var
  _PathGetCharTypeW: Pointer;

function PathGetCharTypeW;
begin
  GetProcedureAddress(_PathGetCharTypeW, shlwapidll, 'PathGetCharTypeW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathGetCharTypeW]
  end;
end;

var
  _PathGetCharType: Pointer;

function PathGetCharType;
begin
  GetProcedureAddress(_PathGetCharType, shlwapidll, 'PathGetCharType'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathGetCharType]
  end;
end;

var
  _PathGetDriveNumberA: Pointer;

function PathGetDriveNumberA;
begin
  GetProcedureAddress(_PathGetDriveNumberA, shlwapidll, 'PathGetDriveNumberA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathGetDriveNumberA]
  end;
end;

var
  _PathGetDriveNumberW: Pointer;

function PathGetDriveNumberW;
begin
  GetProcedureAddress(_PathGetDriveNumberW, shlwapidll, 'PathGetDriveNumberW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathGetDriveNumberW]
  end;
end;

var
  _PathGetDriveNumber: Pointer;

function PathGetDriveNumber;
begin
  GetProcedureAddress(_PathGetDriveNumber, shlwapidll, 'PathGetDriveNumber'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathGetDriveNumber]
  end;
end;

var
  _PathIsDirectoryA: Pointer;

function PathIsDirectoryA;
begin
  GetProcedureAddress(_PathIsDirectoryA, shlwapidll, 'PathIsDirectoryA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsDirectoryA]
  end;
end;

var
  _PathIsDirectoryW: Pointer;

function PathIsDirectoryW;
begin
  GetProcedureAddress(_PathIsDirectoryW, shlwapidll, 'PathIsDirectoryW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsDirectoryW]
  end;
end;

var
  _PathIsDirectory: Pointer;

function PathIsDirectory;
begin
  GetProcedureAddress(_PathIsDirectory, shlwapidll, 'PathIsDirectory'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsDirectory]
  end;
end;

var
  _PathIsDirectoryEmptyA: Pointer;

function PathIsDirectoryEmptyA;
begin
  GetProcedureAddress(_PathIsDirectoryEmptyA, shlwapidll, 'PathIsDirectoryEmptyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsDirectoryEmptyA]
  end;
end;

var
  _PathIsDirectoryEmptyW: Pointer;

function PathIsDirectoryEmptyW;
begin
  GetProcedureAddress(_PathIsDirectoryEmptyW, shlwapidll, 'PathIsDirectoryEmptyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsDirectoryEmptyW]
  end;
end;

var
  _PathIsDirectoryEmpty: Pointer;

function PathIsDirectoryEmpty;
begin
  GetProcedureAddress(_PathIsDirectoryEmpty, shlwapidll, 'PathIsDirectoryEmpty'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsDirectoryEmpty]
  end;
end;

var
  _PathIsFileSpecA: Pointer;

function PathIsFileSpecA;
begin
  GetProcedureAddress(_PathIsFileSpecA, shlwapidll, 'PathIsFileSpecA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsFileSpecA]
  end;
end;

var
  _PathIsFileSpecW: Pointer;

function PathIsFileSpecW;
begin
  GetProcedureAddress(_PathIsFileSpecW, shlwapidll, 'PathIsFileSpecW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsFileSpecW]
  end;
end;

var
  _PathIsFileSpec: Pointer;

function PathIsFileSpec;
begin
  GetProcedureAddress(_PathIsFileSpec, shlwapidll, 'PathIsFileSpec'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsFileSpec]
  end;
end;

var
  _PathIsPrefixA: Pointer;

function PathIsPrefixA;
begin
  GetProcedureAddress(_PathIsPrefixA, shlwapidll, 'PathIsPrefixA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsPrefixA]
  end;
end;

var
  _PathIsPrefixW: Pointer;

function PathIsPrefixW;
begin
  GetProcedureAddress(_PathIsPrefixW, shlwapidll, 'PathIsPrefixW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsPrefixW]
  end;
end;

var
  _PathIsPrefix: Pointer;

function PathIsPrefix;
begin
  GetProcedureAddress(_PathIsPrefix, shlwapidll, 'PathIsPrefix'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsPrefix]
  end;
end;

var
  _PathIsRelativeA: Pointer;

function PathIsRelativeA;
begin
  GetProcedureAddress(_PathIsRelativeA, shlwapidll, 'PathIsRelativeA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsRelativeA]
  end;
end;

var
  _PathIsRelativeW: Pointer;

function PathIsRelativeW;
begin
  GetProcedureAddress(_PathIsRelativeW, shlwapidll, 'PathIsRelativeW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsRelativeW]
  end;
end;

var
  _PathIsRelative: Pointer;

function PathIsRelative;
begin
  GetProcedureAddress(_PathIsRelative, shlwapidll, 'PathIsRelative'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsRelative]
  end;
end;

var
  _PathIsRootA: Pointer;

function PathIsRootA;
begin
  GetProcedureAddress(_PathIsRootA, shlwapidll, 'PathIsRootA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsRootA]
  end;
end;

var
  _PathIsRootW: Pointer;

function PathIsRootW;
begin
  GetProcedureAddress(_PathIsRootW, shlwapidll, 'PathIsRootW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsRootW]
  end;
end;

var
  _PathIsRoot: Pointer;

function PathIsRoot;
begin
  GetProcedureAddress(_PathIsRoot, shlwapidll, 'PathIsRoot'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsRoot]
  end;
end;

var
  _PathIsSameRootA: Pointer;

function PathIsSameRootA;
begin
  GetProcedureAddress(_PathIsSameRootA, shlwapidll, 'PathIsSameRootA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsSameRootA]
  end;
end;

var
  _PathIsSameRootW: Pointer;

function PathIsSameRootW;
begin
  GetProcedureAddress(_PathIsSameRootW, shlwapidll, 'PathIsSameRootW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsSameRootW]
  end;
end;

var
  _PathIsSameRoot: Pointer;

function PathIsSameRoot;
begin
  GetProcedureAddress(_PathIsSameRoot, shlwapidll, 'PathIsSameRoot'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsSameRoot]
  end;
end;

var
  _PathIsUNCA: Pointer;

function PathIsUNCA;
begin
  GetProcedureAddress(_PathIsUNCA, shlwapidll, 'PathIsUNCA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsUNCA]
  end;
end;

var
  _PathIsUNCW: Pointer;

function PathIsUNCW;
begin
  GetProcedureAddress(_PathIsUNCW, shlwapidll, 'PathIsUNCW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsUNCW]
  end;
end;

var
  _PathIsUNC: Pointer;

function PathIsUNC;
begin
  GetProcedureAddress(_PathIsUNC, shlwapidll, 'PathIsUNC'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsUNC]
  end;
end;

var
  _PathIsNetworkPathA: Pointer;

function PathIsNetworkPathA;
begin
  GetProcedureAddress(_PathIsNetworkPathA, shlwapidll, 'PathIsNetworkPathA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsNetworkPathA]
  end;
end;

var
  _PathIsNetworkPathW: Pointer;

function PathIsNetworkPathW;
begin
  GetProcedureAddress(_PathIsNetworkPathW, shlwapidll, 'PathIsNetworkPathW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsNetworkPathW]
  end;
end;

var
  _PathIsNetworkPath: Pointer;

function PathIsNetworkPath;
begin
  GetProcedureAddress(_PathIsNetworkPath, shlwapidll, 'PathIsNetworkPath'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsNetworkPath]
  end;
end;

var
  _PathIsUNCServerA: Pointer;

function PathIsUNCServerA;
begin
  GetProcedureAddress(_PathIsUNCServerA, shlwapidll, 'PathIsUNCServerA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsUNCServerA]
  end;
end;

var
  _PathIsUNCServerW: Pointer;

function PathIsUNCServerW;
begin
  GetProcedureAddress(_PathIsUNCServerW, shlwapidll, 'PathIsUNCServerW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsUNCServerW]
  end;
end;

var
  _PathIsUNCServer: Pointer;

function PathIsUNCServer;
begin
  GetProcedureAddress(_PathIsUNCServer, shlwapidll, 'PathIsUNCServer'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsUNCServer]
  end;
end;

var
  _PathIsUNCServerShareA: Pointer;

function PathIsUNCServerShareA;
begin
  GetProcedureAddress(_PathIsUNCServerShareA, shlwapidll, 'PathIsUNCServerShareA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsUNCServerShareA]
  end;
end;

var
  _PathIsUNCServerShareW: Pointer;

function PathIsUNCServerShareW;
begin
  GetProcedureAddress(_PathIsUNCServerShareW, shlwapidll, 'PathIsUNCServerShareW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsUNCServerShareW]
  end;
end;

var
  _PathIsUNCServerShare: Pointer;

function PathIsUNCServerShare;
begin
  GetProcedureAddress(_PathIsUNCServerShare, shlwapidll, 'PathIsUNCServerShare'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsUNCServerShare]
  end;
end;

var
  _PathIsContentTypeA: Pointer;

function PathIsContentTypeA;
begin
  GetProcedureAddress(_PathIsContentTypeA, shlwapidll, 'PathIsContentTypeA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsContentTypeA]
  end;
end;

var
  _PathIsContentTypeW: Pointer;

function PathIsContentTypeW;
begin
  GetProcedureAddress(_PathIsContentTypeW, shlwapidll, 'PathIsContentTypeW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsContentTypeW]
  end;
end;

var
  _PathIsContentType: Pointer;

function PathIsContentType;
begin
  GetProcedureAddress(_PathIsContentType, shlwapidll, 'PathIsContentType'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsContentType]
  end;
end;

var
  _PathIsURLA: Pointer;

function PathIsURLA;
begin
  GetProcedureAddress(_PathIsURLA, shlwapidll, 'PathIsURLA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsURLA]
  end;
end;

var
  _PathIsURLW: Pointer;

function PathIsURLW;
begin
  GetProcedureAddress(_PathIsURLW, shlwapidll, 'PathIsURLW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsURLW]
  end;
end;

var
  _PathIsURL: Pointer;

function PathIsURL;
begin
  GetProcedureAddress(_PathIsURL, shlwapidll, 'PathIsURL'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsURL]
  end;
end;

var
  _PathMakePrettyA: Pointer;

function PathMakePrettyA;
begin
  GetProcedureAddress(_PathMakePrettyA, shlwapidll, 'PathMakePrettyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathMakePrettyA]
  end;
end;

var
  _PathMakePrettyW: Pointer;

function PathMakePrettyW;
begin
  GetProcedureAddress(_PathMakePrettyW, shlwapidll, 'PathMakePrettyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathMakePrettyW]
  end;
end;

var
  _PathMakePretty: Pointer;

function PathMakePretty;
begin
  GetProcedureAddress(_PathMakePretty, shlwapidll, 'PathMakePretty'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathMakePretty]
  end;
end;

var
  _PathMatchSpecA: Pointer;

function PathMatchSpecA;
begin
  GetProcedureAddress(_PathMatchSpecA, shlwapidll, 'PathMatchSpecA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathMatchSpecA]
  end;
end;

var
  _PathMatchSpecW: Pointer;

function PathMatchSpecW;
begin
  GetProcedureAddress(_PathMatchSpecW, shlwapidll, 'PathMatchSpecW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathMatchSpecW]
  end;
end;

var
  _PathMatchSpec: Pointer;

function PathMatchSpec;
begin
  GetProcedureAddress(_PathMatchSpec, shlwapidll, 'PathMatchSpec'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathMatchSpec]
  end;
end;

var
  _PathParseIconLocationA: Pointer;

function PathParseIconLocationA;
begin
  GetProcedureAddress(_PathParseIconLocationA, shlwapidll, 'PathParseIconLocationA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathParseIconLocationA]
  end;
end;

var
  _PathParseIconLocationW: Pointer;

function PathParseIconLocationW;
begin
  GetProcedureAddress(_PathParseIconLocationW, shlwapidll, 'PathParseIconLocationW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathParseIconLocationW]
  end;
end;

var
  _PathParseIconLocation: Pointer;

function PathParseIconLocation;
begin
  GetProcedureAddress(_PathParseIconLocation, shlwapidll, 'PathParseIconLocation'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathParseIconLocation]
  end;
end;

var
  _PathQuoteSpacesA: Pointer;

procedure PathQuoteSpacesA;
begin
  GetProcedureAddress(_PathQuoteSpacesA, shlwapidll, 'PathQuoteSpacesA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathQuoteSpacesA]
  end;
end;

var
  _PathQuoteSpacesW: Pointer;

procedure PathQuoteSpacesW;
begin
  GetProcedureAddress(_PathQuoteSpacesW, shlwapidll, 'PathQuoteSpacesW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathQuoteSpacesW]
  end;
end;

var
  _PathQuoteSpaces: Pointer;

procedure PathQuoteSpaces;
begin
  GetProcedureAddress(_PathQuoteSpaces, shlwapidll, 'PathQuoteSpaces'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathQuoteSpaces]
  end;
end;

var
  _PathRelativePathToA: Pointer;

function PathRelativePathToA;
begin
  GetProcedureAddress(_PathRelativePathToA, shlwapidll, 'PathRelativePathToA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathRelativePathToA]
  end;
end;

var
  _PathRelativePathToW: Pointer;

function PathRelativePathToW;
begin
  GetProcedureAddress(_PathRelativePathToW, shlwapidll, 'PathRelativePathToW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathRelativePathToW]
  end;
end;

var
  _PathRelativePathTo: Pointer;

function PathRelativePathTo;
begin
  GetProcedureAddress(_PathRelativePathTo, shlwapidll, 'PathRelativePathTo'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathRelativePathTo]
  end;
end;

var
  _PathRemoveArgsA: Pointer;

procedure PathRemoveArgsA;
begin
  GetProcedureAddress(_PathRemoveArgsA, shlwapidll, 'PathRemoveArgsA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathRemoveArgsA]
  end;
end;

var
  _PathRemoveArgsW: Pointer;

procedure PathRemoveArgsW;
begin
  GetProcedureAddress(_PathRemoveArgsW, shlwapidll, 'PathRemoveArgsW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathRemoveArgsW]
  end;
end;

var
  _PathRemoveArgs: Pointer;

procedure PathRemoveArgs;
begin
  GetProcedureAddress(_PathRemoveArgs, shlwapidll, 'PathRemoveArgs'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathRemoveArgs]
  end;
end;

var
  _PathRemoveBackslashA: Pointer;

function PathRemoveBackslashA;
begin
  GetProcedureAddress(_PathRemoveBackslashA, shlwapidll, 'PathRemoveBackslashA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathRemoveBackslashA]
  end;
end;

var
  _PathRemoveBackslashW: Pointer;

function PathRemoveBackslashW;
begin
  GetProcedureAddress(_PathRemoveBackslashW, shlwapidll, 'PathRemoveBackslashW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathRemoveBackslashW]
  end;
end;

var
  _PathRemoveBackslash: Pointer;

function PathRemoveBackslash;
begin
  GetProcedureAddress(_PathRemoveBackslash, shlwapidll, 'PathRemoveBackslash'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathRemoveBackslash]
  end;
end;

var
  _PathRemoveBlanksA: Pointer;

procedure PathRemoveBlanksA;
begin
  GetProcedureAddress(_PathRemoveBlanksA, shlwapidll, 'PathRemoveBlanksA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathRemoveBlanksA]
  end;
end;

var
  _PathRemoveBlanksW: Pointer;

procedure PathRemoveBlanksW;
begin
  GetProcedureAddress(_PathRemoveBlanksW, shlwapidll, 'PathRemoveBlanksW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathRemoveBlanksW]
  end;
end;

var
  _PathRemoveBlanks: Pointer;

procedure PathRemoveBlanks;
begin
  GetProcedureAddress(_PathRemoveBlanks, shlwapidll, 'PathRemoveBlanks'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathRemoveBlanks]
  end;
end;

var
  _PathRemoveExtensionA: Pointer;

procedure PathRemoveExtensionA;
begin
  GetProcedureAddress(_PathRemoveExtensionA, shlwapidll, 'PathRemoveExtensionA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathRemoveExtensionA]
  end;
end;

var
  _PathRemoveExtensionW: Pointer;

procedure PathRemoveExtensionW;
begin
  GetProcedureAddress(_PathRemoveExtensionW, shlwapidll, 'PathRemoveExtensionW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathRemoveExtensionW]
  end;
end;

var
  _PathRemoveExtension: Pointer;

procedure PathRemoveExtension;
begin
  GetProcedureAddress(_PathRemoveExtension, shlwapidll, 'PathRemoveExtension'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathRemoveExtension]
  end;
end;

var
  _PathRemoveFileSpecA: Pointer;

function PathRemoveFileSpecA;
begin
  GetProcedureAddress(_PathRemoveFileSpecA, shlwapidll, 'PathRemoveFileSpecA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathRemoveFileSpecA]
  end;
end;

var
  _PathRemoveFileSpecW: Pointer;

function PathRemoveFileSpecW;
begin
  GetProcedureAddress(_PathRemoveFileSpecW, shlwapidll, 'PathRemoveFileSpecW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathRemoveFileSpecW]
  end;
end;

var
  _PathRemoveFileSpec: Pointer;

function PathRemoveFileSpec;
begin
  GetProcedureAddress(_PathRemoveFileSpec, shlwapidll, 'PathRemoveFileSpec'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathRemoveFileSpec]
  end;
end;

var
  _PathRenameExtensionA: Pointer;

function PathRenameExtensionA;
begin
  GetProcedureAddress(_PathRenameExtensionA, shlwapidll, 'PathRenameExtensionA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathRenameExtensionA]
  end;
end;

var
  _PathRenameExtensionW: Pointer;

function PathRenameExtensionW;
begin
  GetProcedureAddress(_PathRenameExtensionW, shlwapidll, 'PathRenameExtensionW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathRenameExtensionW]
  end;
end;

var
  _PathRenameExtension: Pointer;

function PathRenameExtension;
begin
  GetProcedureAddress(_PathRenameExtension, shlwapidll, 'PathRenameExtension'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathRenameExtension]
  end;
end;

var
  _PathSearchAndQualifyA: Pointer;

function PathSearchAndQualifyA;
begin
  GetProcedureAddress(_PathSearchAndQualifyA, shlwapidll, 'PathSearchAndQualifyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathSearchAndQualifyA]
  end;
end;

var
  _PathSearchAndQualifyW: Pointer;

function PathSearchAndQualifyW;
begin
  GetProcedureAddress(_PathSearchAndQualifyW, shlwapidll, 'PathSearchAndQualifyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathSearchAndQualifyW]
  end;
end;

var
  _PathSearchAndQualify: Pointer;

function PathSearchAndQualify;
begin
  GetProcedureAddress(_PathSearchAndQualify, shlwapidll, 'PathSearchAndQualify'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathSearchAndQualify]
  end;
end;

var
  _PathSetDlgItemPathA: Pointer;

procedure PathSetDlgItemPathA;
begin
  GetProcedureAddress(_PathSetDlgItemPathA, shlwapidll, 'PathSetDlgItemPathA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathSetDlgItemPathA]
  end;
end;

var
  _PathSetDlgItemPathW: Pointer;

procedure PathSetDlgItemPathW;
begin
  GetProcedureAddress(_PathSetDlgItemPathW, shlwapidll, 'PathSetDlgItemPathW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathSetDlgItemPathW]
  end;
end;

var
  _PathSetDlgItemPath: Pointer;

procedure PathSetDlgItemPath;
begin
  GetProcedureAddress(_PathSetDlgItemPath, shlwapidll, 'PathSetDlgItemPath'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathSetDlgItemPath]
  end;
end;

var
  _PathSkipRootA: Pointer;

function PathSkipRootA;
begin
  GetProcedureAddress(_PathSkipRootA, shlwapidll, 'PathSkipRootA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathSkipRootA]
  end;
end;

var
  _PathSkipRootW: Pointer;

function PathSkipRootW;
begin
  GetProcedureAddress(_PathSkipRootW, shlwapidll, 'PathSkipRootW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathSkipRootW]
  end;
end;

var
  _PathSkipRoot: Pointer;

function PathSkipRoot;
begin
  GetProcedureAddress(_PathSkipRoot, shlwapidll, 'PathSkipRoot'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathSkipRoot]
  end;
end;

var
  _PathStripPathA: Pointer;

procedure PathStripPathA;
begin
  GetProcedureAddress(_PathStripPathA, shlwapidll, 'PathStripPathA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathStripPathA]
  end;
end;

var
  _PathStripPathW: Pointer;

procedure PathStripPathW;
begin
  GetProcedureAddress(_PathStripPathW, shlwapidll, 'PathStripPathW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathStripPathW]
  end;
end;

var
  _PathStripPath: Pointer;

procedure PathStripPath;
begin
  GetProcedureAddress(_PathStripPath, shlwapidll, 'PathStripPath'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathStripPath]
  end;
end;

var
  _PathStripToRootA: Pointer;

function PathStripToRootA;
begin
  GetProcedureAddress(_PathStripToRootA, shlwapidll, 'PathStripToRootA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathStripToRootA]
  end;
end;

var
  _PathStripToRootW: Pointer;

function PathStripToRootW;
begin
  GetProcedureAddress(_PathStripToRootW, shlwapidll, 'PathStripToRootW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathStripToRootW]
  end;
end;

var
  _PathStripToRoot: Pointer;

function PathStripToRoot;
begin
  GetProcedureAddress(_PathStripToRoot, shlwapidll, 'PathStripToRoot'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathStripToRoot]
  end;
end;

var
  _PathUnquoteSpacesA: Pointer;

procedure PathUnquoteSpacesA;
begin
  GetProcedureAddress(_PathUnquoteSpacesA, shlwapidll, 'PathUnquoteSpacesA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathUnquoteSpacesA]
  end;
end;

var
  _PathUnquoteSpacesW: Pointer;

procedure PathUnquoteSpacesW;
begin
  GetProcedureAddress(_PathUnquoteSpacesW, shlwapidll, 'PathUnquoteSpacesW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathUnquoteSpacesW]
  end;
end;

var
  _PathUnquoteSpaces: Pointer;

procedure PathUnquoteSpaces;
begin
  GetProcedureAddress(_PathUnquoteSpaces, shlwapidll, 'PathUnquoteSpaces'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathUnquoteSpaces]
  end;
end;

var
  _PathMakeSystemFolderA: Pointer;

function PathMakeSystemFolderA;
begin
  GetProcedureAddress(_PathMakeSystemFolderA, shlwapidll, 'PathMakeSystemFolderA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathMakeSystemFolderA]
  end;
end;

var
  _PathMakeSystemFolderW: Pointer;

function PathMakeSystemFolderW;
begin
  GetProcedureAddress(_PathMakeSystemFolderW, shlwapidll, 'PathMakeSystemFolderW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathMakeSystemFolderW]
  end;
end;

var
  _PathMakeSystemFolder: Pointer;

function PathMakeSystemFolder;
begin
  GetProcedureAddress(_PathMakeSystemFolder, shlwapidll, 'PathMakeSystemFolder'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathMakeSystemFolder]
  end;
end;

var
  _PathUnmakeSystemFolderA: Pointer;

function PathUnmakeSystemFolderA;
begin
  GetProcedureAddress(_PathUnmakeSystemFolderA, shlwapidll, 'PathUnmakeSystemFolderA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathUnmakeSystemFolderA]
  end;
end;

var
  _PathUnmakeSystemFolderW: Pointer;

function PathUnmakeSystemFolderW;
begin
  GetProcedureAddress(_PathUnmakeSystemFolderW, shlwapidll, 'PathUnmakeSystemFolderW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathUnmakeSystemFolderW]
  end;
end;

var
  _PathUnmakeSystemFolder: Pointer;

function PathUnmakeSystemFolder;
begin
  GetProcedureAddress(_PathUnmakeSystemFolder, shlwapidll, 'PathUnmakeSystemFolder'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathUnmakeSystemFolder]
  end;
end;

var
  _PathIsSystemFolderA: Pointer;

function PathIsSystemFolderA;
begin
  GetProcedureAddress(_PathIsSystemFolderA, shlwapidll, 'PathIsSystemFolderA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsSystemFolderA]
  end;
end;

var
  _PathIsSystemFolderW: Pointer;

function PathIsSystemFolderW;
begin
  GetProcedureAddress(_PathIsSystemFolderW, shlwapidll, 'PathIsSystemFolderW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsSystemFolderW]
  end;
end;

var
  _PathIsSystemFolder: Pointer;

function PathIsSystemFolder;
begin
  GetProcedureAddress(_PathIsSystemFolder, shlwapidll, 'PathIsSystemFolder'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathIsSystemFolder]
  end;
end;

var
  _PathUndecorateA: Pointer;

procedure PathUndecorateA;
begin
  GetProcedureAddress(_PathUndecorateA, shlwapidll, 'PathUndecorateA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathUndecorateA]
  end;
end;

var
  _PathUndecorateW: Pointer;

procedure PathUndecorateW;
begin
  GetProcedureAddress(_PathUndecorateW, shlwapidll, 'PathUndecorateW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathUndecorateW]
  end;
end;

var
  _PathUndecorate: Pointer;

procedure PathUndecorate;
begin
  GetProcedureAddress(_PathUndecorate, shlwapidll, 'PathUndecorate'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathUndecorate]
  end;
end;

var
  _PathUnExpandEnvStringsA: Pointer;

function PathUnExpandEnvStringsA;
begin
  GetProcedureAddress(_PathUnExpandEnvStringsA, shlwapidll, 'PathUnExpandEnvStringsA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathUnExpandEnvStringsA]
  end;
end;

var
  _PathUnExpandEnvStringsW: Pointer;

function PathUnExpandEnvStringsW;
begin
  GetProcedureAddress(_PathUnExpandEnvStringsW, shlwapidll, 'PathUnExpandEnvStringsW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathUnExpandEnvStringsW]
  end;
end;

var
  _PathUnExpandEnvStrings: Pointer;

function PathUnExpandEnvStrings;
begin
  GetProcedureAddress(_PathUnExpandEnvStrings, shlwapidll, 'PathUnExpandEnvStrings'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathUnExpandEnvStrings]
  end;
end;

var
  _UrlCompareA: Pointer;

function UrlCompareA;
begin
  GetProcedureAddress(_UrlCompareA, shlwapidll, 'UrlCompareA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlCompareA]
  end;
end;

var
  _UrlCompareW: Pointer;

function UrlCompareW;
begin
  GetProcedureAddress(_UrlCompareW, shlwapidll, 'UrlCompareW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlCompareW]
  end;
end;

var
  _UrlCompare: Pointer;

function UrlCompare;
begin
  GetProcedureAddress(_UrlCompare, shlwapidll, 'UrlCompare'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlCompare]
  end;
end;

var
  _UrlCombineA: Pointer;

function UrlCombineA;
begin
  GetProcedureAddress(_UrlCombineA, shlwapidll, 'UrlCombineA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlCombineA]
  end;
end;

var
  _UrlCombineW: Pointer;

function UrlCombineW;
begin
  GetProcedureAddress(_UrlCombineW, shlwapidll, 'UrlCombineW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlCombineW]
  end;
end;

var
  _UrlCombine: Pointer;

function UrlCombine;
begin
  GetProcedureAddress(_UrlCombine, shlwapidll, 'UrlCombine'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlCombine]
  end;
end;

var
  _UrlCanonicalizeA: Pointer;

function UrlCanonicalizeA;
begin
  GetProcedureAddress(_UrlCanonicalizeA, shlwapidll, 'UrlCanonicalizeA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlCanonicalizeA]
  end;
end;

var
  _UrlCanonicalizeW: Pointer;

function UrlCanonicalizeW;
begin
  GetProcedureAddress(_UrlCanonicalizeW, shlwapidll, 'UrlCanonicalizeW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlCanonicalizeW]
  end;
end;

var
  _UrlCanonicalize: Pointer;

function UrlCanonicalize;
begin
  GetProcedureAddress(_UrlCanonicalize, shlwapidll, 'UrlCanonicalize'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlCanonicalize]
  end;
end;

var
  _UrlIsOpaqueA: Pointer;

function UrlIsOpaqueA;
begin
  GetProcedureAddress(_UrlIsOpaqueA, shlwapidll, 'UrlIsOpaqueA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlIsOpaqueA]
  end;
end;

var
  _UrlIsOpaqueW: Pointer;

function UrlIsOpaqueW;
begin
  GetProcedureAddress(_UrlIsOpaqueW, shlwapidll, 'UrlIsOpaqueW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlIsOpaqueW]
  end;
end;

var
  _UrlIsOpaque: Pointer;

function UrlIsOpaque;
begin
  GetProcedureAddress(_UrlIsOpaque, shlwapidll, 'UrlIsOpaque'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlIsOpaque]
  end;
end;

var
  _UrlIsNoHistoryA: Pointer;

function UrlIsNoHistoryA;
begin
  GetProcedureAddress(_UrlIsNoHistoryA, shlwapidll, 'UrlIsNoHistoryA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlIsNoHistoryA]
  end;
end;

var
  _UrlIsNoHistoryW: Pointer;

function UrlIsNoHistoryW;
begin
  GetProcedureAddress(_UrlIsNoHistoryW, shlwapidll, 'UrlIsNoHistoryW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlIsNoHistoryW]
  end;
end;

var
  _UrlIsNoHistory: Pointer;

function UrlIsNoHistory;
begin
  GetProcedureAddress(_UrlIsNoHistory, shlwapidll, 'UrlIsNoHistory'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlIsNoHistory]
  end;
end;


var
  _UrlIsA: Pointer;

function UrlIsA;
begin
  GetProcedureAddress(_UrlIsA, shlwapidll, 'UrlIsA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlIsA]
  end;
end;

var
  _UrlIsW: Pointer;

function UrlIsW;
begin
  GetProcedureAddress(_UrlIsW, shlwapidll, 'UrlIsW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlIsW]
  end;
end;

var
  _UrlIs: Pointer;

function UrlIs;
begin
  GetProcedureAddress(_UrlIs, shlwapidll, 'UrlIs'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlIs]
  end;
end;

var
  _UrlGetLocationA: Pointer;

function UrlGetLocationA;
begin
  GetProcedureAddress(_UrlGetLocationA, shlwapidll, 'UrlGetLocationA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlGetLocationA]
  end;
end;

var
  _UrlGetLocationW: Pointer;

function UrlGetLocationW;
begin
  GetProcedureAddress(_UrlGetLocationW, shlwapidll, 'UrlGetLocationW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlGetLocationW]
  end;
end;

var
  _UrlGetLocation: Pointer;

function UrlGetLocation;
begin
  GetProcedureAddress(_UrlGetLocation, shlwapidll, 'UrlGetLocation'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlGetLocation]
  end;
end;

var
  _UrlUnescapeA: Pointer;

function UrlUnescapeA;
begin
  GetProcedureAddress(_UrlUnescapeA, shlwapidll, 'UrlUnescapeA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlUnescapeA]
  end;
end;

var
  _UrlUnescapeW: Pointer;

function UrlUnescapeW;
begin
  GetProcedureAddress(_UrlUnescapeW, shlwapidll, 'UrlUnescapeW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlUnescapeW]
  end;
end;

var
  _UrlUnescape: Pointer;

function UrlUnescape;
begin
  GetProcedureAddress(_UrlUnescape, shlwapidll, 'UrlUnescape'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlUnescape]
  end;
end;

var
  _UrlEscapeA: Pointer;

function UrlEscapeA;
begin
  GetProcedureAddress(_UrlEscapeA, shlwapidll, 'UrlEscapeA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlEscapeA]
  end;
end;

var
  _UrlEscapeW: Pointer;

function UrlEscapeW;
begin
  GetProcedureAddress(_UrlEscapeW, shlwapidll, 'UrlEscapeW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlEscapeW]
  end;
end;

var
  _UrlEscape: Pointer;

function UrlEscape;
begin
  GetProcedureAddress(_UrlEscape, shlwapidll, 'UrlEscape'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlEscape]
  end;
end;

var
  _UrlCreateFromPathA: Pointer;

function UrlCreateFromPathA;
begin
  GetProcedureAddress(_UrlCreateFromPathA, shlwapidll, 'UrlCreateFromPathA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlCreateFromPathA]
  end;
end;

var
  _UrlCreateFromPathW: Pointer;

function UrlCreateFromPathW;
begin
  GetProcedureAddress(_UrlCreateFromPathW, shlwapidll, 'UrlCreateFromPathW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlCreateFromPathW]
  end;
end;

var
  _UrlCreateFromPath: Pointer;

function UrlCreateFromPath;
begin
  GetProcedureAddress(_UrlCreateFromPath, shlwapidll, 'UrlCreateFromPath'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlCreateFromPath]
  end;
end;

var
  _PathCreateFromUrlA: Pointer;

function PathCreateFromUrlA;
begin
  GetProcedureAddress(_PathCreateFromUrlA, shlwapidll, 'PathCreateFromUrlA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathCreateFromUrlA]
  end;
end;

var
  _PathCreateFromUrlW: Pointer;

function PathCreateFromUrlW;
begin
  GetProcedureAddress(_PathCreateFromUrlW, shlwapidll, 'PathCreateFromUrlW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathCreateFromUrlW]
  end;
end;

var
  _PathCreateFromUrl: Pointer;

function PathCreateFromUrl;
begin
  GetProcedureAddress(_PathCreateFromUrl, shlwapidll, 'PathCreateFromUrl'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PathCreateFromUrl]
  end;
end;

var
  _UrlHashA: Pointer;

function UrlHashA;
begin
  GetProcedureAddress(_UrlHashA, shlwapidll, 'UrlHashA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlHashA]
  end;
end;

var
  _UrlHashW: Pointer;

function UrlHashW;
begin
  GetProcedureAddress(_UrlHashW, shlwapidll, 'UrlHashW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlHashW]
  end;
end;

var
  _UrlHash: Pointer;

function UrlHash;
begin
  GetProcedureAddress(_UrlHash, shlwapidll, 'UrlHash'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlHash]
  end;
end;

var
  _UrlGetPartW: Pointer;

function UrlGetPartW;
begin
  GetProcedureAddress(_UrlGetPartW, shlwapidll, 'UrlGetPartW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlGetPartW]
  end;
end;

var
  _UrlGetPartA: Pointer;

function UrlGetPartA;
begin
  GetProcedureAddress(_UrlGetPartA, shlwapidll, 'UrlGetPartA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlGetPartA]
  end;
end;

var
  _UrlGetPart: Pointer;

function UrlGetPart;
begin
  GetProcedureAddress(_UrlGetPart, shlwapidll, 'UrlGetPart'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlGetPart]
  end;
end;

var
  _UrlApplySchemeA: Pointer;

function UrlApplySchemeA;
begin
  GetProcedureAddress(_UrlApplySchemeA, shlwapidll, 'UrlApplySchemeA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlApplySchemeA]
  end;
end;

var
  _UrlApplySchemeW: Pointer;

function UrlApplySchemeW;
begin
  GetProcedureAddress(_UrlApplySchemeW, shlwapidll, 'UrlApplySchemeW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlApplySchemeW]
  end;
end;

var
  _UrlApplyScheme: Pointer;

function UrlApplyScheme;
begin
  GetProcedureAddress(_UrlApplyScheme, shlwapidll, 'UrlApplyScheme'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UrlApplyScheme]
  end;
end;

var
  _HashData: Pointer;

function HashData;
begin
  GetProcedureAddress(_HashData, shlwapidll, 'HashData');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HashData]
  end;
end;

var
  _SHDeleteEmptyKeyA: Pointer;

function SHDeleteEmptyKeyA;
begin
  GetProcedureAddress(_SHDeleteEmptyKeyA, shlwapidll, 'SHDeleteEmptyKeyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHDeleteEmptyKeyA]
  end;
end;

var
  _SHDeleteEmptyKeyW: Pointer;

function SHDeleteEmptyKeyW;
begin
  GetProcedureAddress(_SHDeleteEmptyKeyW, shlwapidll, 'SHDeleteEmptyKeyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHDeleteEmptyKeyW]
  end;
end;

var
  _SHDeleteEmptyKey: Pointer;

function SHDeleteEmptyKey;
begin
  GetProcedureAddress(_SHDeleteEmptyKey, shlwapidll, 'SHDeleteEmptyKey'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHDeleteEmptyKey]
  end;
end;

var
  _SHDeleteKeyA: Pointer;

function SHDeleteKeyA;
begin
  GetProcedureAddress(_SHDeleteKeyA, shlwapidll, 'SHDeleteKeyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHDeleteKeyA]
  end;
end;

var
  _SHDeleteKeyW: Pointer;

function SHDeleteKeyW;
begin
  GetProcedureAddress(_SHDeleteKeyW, shlwapidll, 'SHDeleteKeyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHDeleteKeyW]
  end;
end;

var
  _SHDeleteKey: Pointer;

function SHDeleteKey;
begin
  GetProcedureAddress(_SHDeleteKey, shlwapidll, 'SHDeleteKey'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHDeleteKey]
  end;
end;

var
  _SHRegDuplicateHKey: Pointer;

function SHRegDuplicateHKey;
begin
  GetProcedureAddress(_SHRegDuplicateHKey, shlwapidll, 'SHRegDuplicateHKey');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegDuplicateHKey]
  end;
end;

var
  _SHDeleteValueA: Pointer;

function SHDeleteValueA;
begin
  GetProcedureAddress(_SHDeleteValueA, shlwapidll, 'SHDeleteValueA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHDeleteValueA]
  end;
end;

var
  _SHDeleteValueW: Pointer;

function SHDeleteValueW;
begin
  GetProcedureAddress(_SHDeleteValueW, shlwapidll, 'SHDeleteValueW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHDeleteValueW]
  end;
end;

var
  _SHDeleteValue: Pointer;

function SHDeleteValue;
begin
  GetProcedureAddress(_SHDeleteValue, shlwapidll, 'SHDeleteValue'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHDeleteValue]
  end;
end;

var
  _SHGetValueA: Pointer;

function SHGetValueA;
begin
  GetProcedureAddress(_SHGetValueA, shlwapidll, 'SHGetValueA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetValueA]
  end;
end;

var
  _SHGetValueW: Pointer;

function SHGetValueW;
begin
  GetProcedureAddress(_SHGetValueW, shlwapidll, 'SHGetValueW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetValueW]
  end;
end;

var
  _SHGetValue: Pointer;

function SHGetValue;
begin
  GetProcedureAddress(_SHGetValue, shlwapidll, 'SHGetValue'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetValue]
  end;
end;

var
  _SHSetValueA: Pointer;

function SHSetValueA;
begin
  GetProcedureAddress(_SHSetValueA, shlwapidll, 'SHSetValueA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHSetValueA]
  end;
end;

var
  _SHSetValueW: Pointer;

function SHSetValueW;
begin
  GetProcedureAddress(_SHSetValueW, shlwapidll, 'SHSetValueW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHSetValueW]
  end;
end;

var
  _SHSetValue: Pointer;

function SHSetValue;
begin
  GetProcedureAddress(_SHSetValue, shlwapidll, 'SHSetValue'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHSetValue]
  end;
end;

var
  _SHQueryValueExA: Pointer;

function SHQueryValueExA;
begin
  GetProcedureAddress(_SHQueryValueExA, shlwapidll, 'SHQueryValueExA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHQueryValueExA]
  end;
end;

var
  _SHQueryValueExW: Pointer;

function SHQueryValueExW;
begin
  GetProcedureAddress(_SHQueryValueExW, shlwapidll, 'SHQueryValueExW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHQueryValueExW]
  end;
end;

var
  _SHQueryValueEx: Pointer;

function SHQueryValueEx;
begin
  GetProcedureAddress(_SHQueryValueEx, shlwapidll, 'SHQueryValueEx'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHQueryValueEx]
  end;
end;

var
  _SHEnumKeyExA: Pointer;

function SHEnumKeyExA;
begin
  GetProcedureAddress(_SHEnumKeyExA, shlwapidll, 'SHEnumKeyExA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHEnumKeyExA]
  end;
end;

var
  _SHEnumKeyExW: Pointer;

function SHEnumKeyExW;
begin
  GetProcedureAddress(_SHEnumKeyExW, shlwapidll, 'SHEnumKeyExW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHEnumKeyExW]
  end;
end;

var
  _SHEnumKeyEx: Pointer;

function SHEnumKeyEx;
begin
  GetProcedureAddress(_SHEnumKeyEx, shlwapidll, 'SHEnumKeyEx'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHEnumKeyEx]
  end;
end;

var
  _SHEnumValueA: Pointer;

function SHEnumValueA;
begin
  GetProcedureAddress(_SHEnumValueA, shlwapidll, 'SHEnumValueA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHEnumValueA]
  end;
end;

var
  _SHEnumValueW: Pointer;

function SHEnumValueW;
begin
  GetProcedureAddress(_SHEnumValueW, shlwapidll, 'SHEnumValueW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHEnumValueW]
  end;
end;

var
  _SHEnumValue: Pointer;

function SHEnumValue;
begin
  GetProcedureAddress(_SHEnumValue, shlwapidll, 'SHEnumValue'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHEnumValue]
  end;
end;

var
  _SHQueryInfoKeyA: Pointer;

function SHQueryInfoKeyA;
begin
  GetProcedureAddress(_SHQueryInfoKeyA, shlwapidll, 'SHQueryInfoKeyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHQueryInfoKeyA]
  end;
end;

var
  _SHQueryInfoKeyW: Pointer;

function SHQueryInfoKeyW;
begin
  GetProcedureAddress(_SHQueryInfoKeyW, shlwapidll, 'SHQueryInfoKeyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHQueryInfoKeyW]
  end;
end;

var
  _SHQueryInfoKey: Pointer;

function SHQueryInfoKey;
begin
  GetProcedureAddress(_SHQueryInfoKey, shlwapidll, 'SHQueryInfoKey'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHQueryInfoKey]
  end;
end;

var
  _SHCopyKeyA: Pointer;

function SHCopyKeyA;
begin
  GetProcedureAddress(_SHCopyKeyA, shlwapidll, 'SHCopyKeyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCopyKeyA]
  end;
end;

var
  _SHCopyKeyW: Pointer;

function SHCopyKeyW;
begin
  GetProcedureAddress(_SHCopyKeyW, shlwapidll, 'SHCopyKeyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCopyKeyW]
  end;
end;

var
  _SHCopyKey: Pointer;

function SHCopyKey;
begin
  GetProcedureAddress(_SHCopyKey, shlwapidll, 'SHCopyKey'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCopyKey]
  end;
end;

var
  _SHRegGetPathA: Pointer;

function SHRegGetPathA;
begin
  GetProcedureAddress(_SHRegGetPathA, shlwapidll, 'SHRegGetPathA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegGetPathA]
  end;
end;

var
  _SHRegGetPathW: Pointer;

function SHRegGetPathW;
begin
  GetProcedureAddress(_SHRegGetPathW, shlwapidll, 'SHRegGetPathW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegGetPathW]
  end;
end;

var
  _SHRegGetPath: Pointer;

function SHRegGetPath;
begin
  GetProcedureAddress(_SHRegGetPath, shlwapidll, 'SHRegGetPath'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegGetPath]
  end;
end;

var
  _SHRegSetPathA: Pointer;

function SHRegSetPathA;
begin
  GetProcedureAddress(_SHRegSetPathA, shlwapidll, 'SHRegSetPathA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegSetPathA]
  end;
end;

var
  _SHRegSetPathW: Pointer;

function SHRegSetPathW;
begin
  GetProcedureAddress(_SHRegSetPathW, shlwapidll, 'SHRegSetPathW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegSetPathW]
  end;
end;

var
  _SHRegSetPath: Pointer;

function SHRegSetPath;
begin
  GetProcedureAddress(_SHRegSetPath, shlwapidll, 'SHRegSetPath'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegSetPath]
  end;
end;

var
  _SHRegCreateUSKeyA: Pointer;

function SHRegCreateUSKeyA;
begin
  GetProcedureAddress(_SHRegCreateUSKeyA, shlwapidll, 'SHRegCreateUSKeyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegCreateUSKeyA]
  end;
end;

var
  _SHRegCreateUSKeyW: Pointer;

function SHRegCreateUSKeyW;
begin
  GetProcedureAddress(_SHRegCreateUSKeyW, shlwapidll, 'SHRegCreateUSKeyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegCreateUSKeyW]
  end;
end;

var
  _SHRegCreateUSKey: Pointer;

function SHRegCreateUSKey;
begin
  GetProcedureAddress(_SHRegCreateUSKey, shlwapidll, 'SHRegCreateUSKey'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegCreateUSKey]
  end;
end;

var
  _SHRegOpenUSKeyA: Pointer;

function SHRegOpenUSKeyA;
begin
  GetProcedureAddress(_SHRegOpenUSKeyA, shlwapidll, 'SHRegOpenUSKeyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegOpenUSKeyA]
  end;
end;

var
  _SHRegOpenUSKeyW: Pointer;

function SHRegOpenUSKeyW;
begin
  GetProcedureAddress(_SHRegOpenUSKeyW, shlwapidll, 'SHRegOpenUSKeyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegOpenUSKeyW]
  end;
end;

var
  _SHRegOpenUSKey: Pointer;

function SHRegOpenUSKey;
begin
  GetProcedureAddress(_SHRegOpenUSKey, shlwapidll, 'SHRegOpenUSKey'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegOpenUSKey]
  end;
end;

var
  _SHRegQueryUSValueA: Pointer;

function SHRegQueryUSValueA;
begin
  GetProcedureAddress(_SHRegQueryUSValueA, shlwapidll, 'SHRegQueryUSValueA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegQueryUSValueA]
  end;
end;

var
  _SHRegQueryUSValueW: Pointer;

function SHRegQueryUSValueW;
begin
  GetProcedureAddress(_SHRegQueryUSValueW, shlwapidll, 'SHRegQueryUSValueW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegQueryUSValueW]
  end;
end;

var
  _SHRegQueryUSValue: Pointer;

function SHRegQueryUSValue;
begin
  GetProcedureAddress(_SHRegQueryUSValue, shlwapidll, 'SHRegQueryUSValue'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegQueryUSValue]
  end;
end;

var
  _SHRegWriteUSValueA: Pointer;

function SHRegWriteUSValueA;
begin
  GetProcedureAddress(_SHRegWriteUSValueA, shlwapidll, 'SHRegWriteUSValueA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegWriteUSValueA]
  end;
end;

var
  _SHRegWriteUSValueW: Pointer;

function SHRegWriteUSValueW;
begin
  GetProcedureAddress(_SHRegWriteUSValueW, shlwapidll, 'SHRegWriteUSValueW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegWriteUSValueW]
  end;
end;

var
  _SHRegWriteUSValue: Pointer;

function SHRegWriteUSValue;
begin
  GetProcedureAddress(_SHRegWriteUSValue, shlwapidll, 'SHRegWriteUSValue'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegWriteUSValue]
  end;
end;

var
  _SHRegDeleteUSValueA: Pointer;

function SHRegDeleteUSValueA;
begin
  GetProcedureAddress(_SHRegDeleteUSValueA, shlwapidll, 'SHRegDeleteUSValueA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegDeleteUSValueA]
  end;
end;

var
  _SHRegDeleteUSValueW: Pointer;

function SHRegDeleteUSValueW;
begin
  GetProcedureAddress(_SHRegDeleteUSValueW, shlwapidll, 'SHRegDeleteUSValueW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegDeleteUSValueW]
  end;
end;

var
  _SHRegDeleteUSValue: Pointer;

function SHRegDeleteUSValue;
begin
  GetProcedureAddress(_SHRegDeleteUSValue, shlwapidll, 'SHRegDeleteUSValue'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegDeleteUSValue]
  end;
end;

var
  _SHRegDeleteEmptyUSKeyW: Pointer;

function SHRegDeleteEmptyUSKeyW;
begin
  GetProcedureAddress(_SHRegDeleteEmptyUSKeyW, shlwapidll, 'SHRegDeleteEmptyUSKeyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegDeleteEmptyUSKeyW]
  end;
end;

var
  _SHRegDeleteEmptyUSKeyA: Pointer;

function SHRegDeleteEmptyUSKeyA;
begin
  GetProcedureAddress(_SHRegDeleteEmptyUSKeyA, shlwapidll, 'SHRegDeleteEmptyUSKeyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegDeleteEmptyUSKeyA]
  end;
end;

var
  _SHRegDeleteEmptyUSKey: Pointer;

function SHRegDeleteEmptyUSKey;
begin
  GetProcedureAddress(_SHRegDeleteEmptyUSKey, shlwapidll, 'SHRegDeleteEmptyUSKey'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegDeleteEmptyUSKey]
  end;
end;

var
  _SHRegEnumUSKeyA: Pointer;

function SHRegEnumUSKeyA;
begin
  GetProcedureAddress(_SHRegEnumUSKeyA, shlwapidll, 'SHRegEnumUSKeyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegEnumUSKeyA]
  end;
end;

var
  _SHRegEnumUSKeyW: Pointer;

function SHRegEnumUSKeyW;
begin
  GetProcedureAddress(_SHRegEnumUSKeyW, shlwapidll, 'SHRegEnumUSKeyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegEnumUSKeyW]
  end;
end;

var
  _SHRegEnumUSKey: Pointer;

function SHRegEnumUSKey;
begin
  GetProcedureAddress(_SHRegEnumUSKey, shlwapidll, 'SHRegEnumUSKey'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegEnumUSKey]
  end;
end;

var
  _SHRegEnumUSValueA: Pointer;

function SHRegEnumUSValueA;
begin
  GetProcedureAddress(_SHRegEnumUSValueA, shlwapidll, 'SHRegEnumUSValueA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegEnumUSValueA]
  end;
end;

var
  _SHRegEnumUSValueW: Pointer;

function SHRegEnumUSValueW;
begin
  GetProcedureAddress(_SHRegEnumUSValueW, shlwapidll, 'SHRegEnumUSValueW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegEnumUSValueW]
  end;
end;

var
  _SHRegEnumUSValue: Pointer;

function SHRegEnumUSValue;
begin
  GetProcedureAddress(_SHRegEnumUSValue, shlwapidll, 'SHRegEnumUSValue'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegEnumUSValue]
  end;
end;

var
  _SHRegQueryInfoUSKeyA: Pointer;

function SHRegQueryInfoUSKeyA;
begin
  GetProcedureAddress(_SHRegQueryInfoUSKeyA, shlwapidll, 'SHRegQueryInfoUSKeyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegQueryInfoUSKeyA]
  end;
end;

var
  _SHRegQueryInfoUSKeyW: Pointer;

function SHRegQueryInfoUSKeyW;
begin
  GetProcedureAddress(_SHRegQueryInfoUSKeyW, shlwapidll, 'SHRegQueryInfoUSKeyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegQueryInfoUSKeyW]
  end;
end;

var
  _SHRegQueryInfoUSKey: Pointer;

function SHRegQueryInfoUSKey;
begin
  GetProcedureAddress(_SHRegQueryInfoUSKey, shlwapidll, 'SHRegQueryInfoUSKey'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegQueryInfoUSKey]
  end;
end;

var
  _SHRegCloseUSKey: Pointer;

function SHRegCloseUSKey;
begin
  GetProcedureAddress(_SHRegCloseUSKey, shlwapidll, 'SHRegCloseUSKey');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegCloseUSKey]
  end;
end;

var
  _SHRegGetUSValueA: Pointer;

function SHRegGetUSValueA;
begin
  GetProcedureAddress(_SHRegGetUSValueA, shlwapidll, 'SHRegGetUSValueA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegGetUSValueA]
  end;
end;

var
  _SHRegGetUSValueW: Pointer;

function SHRegGetUSValueW;
begin
  GetProcedureAddress(_SHRegGetUSValueW, shlwapidll, 'SHRegGetUSValueW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegGetUSValueW]
  end;
end;

var
  _SHRegGetUSValue: Pointer;

function SHRegGetUSValue;
begin
  GetProcedureAddress(_SHRegGetUSValue, shlwapidll, 'SHRegGetUSValue'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegGetUSValue]
  end;
end;

var
  _SHRegSetUSValueA: Pointer;

function SHRegSetUSValueA;
begin
  GetProcedureAddress(_SHRegSetUSValueA, shlwapidll, 'SHRegSetUSValueA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegSetUSValueA]
  end;
end;

var
  _SHRegSetUSValueW: Pointer;

function SHRegSetUSValueW;
begin
  GetProcedureAddress(_SHRegSetUSValueW, shlwapidll, 'SHRegSetUSValueW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegSetUSValueW]
  end;
end;

var
  _SHRegSetUSValue: Pointer;

function SHRegSetUSValue;
begin
  GetProcedureAddress(_SHRegSetUSValue, shlwapidll, 'SHRegSetUSValue'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegSetUSValue]
  end;
end;

var
  _SHRegGetBoolUSValueA: Pointer;

function SHRegGetBoolUSValueA;
begin
  GetProcedureAddress(_SHRegGetBoolUSValueA, shlwapidll, 'SHRegGetBoolUSValueA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegGetBoolUSValueA]
  end;
end;

var
  _SHRegGetBoolUSValueW: Pointer;

function SHRegGetBoolUSValueW;
begin
  GetProcedureAddress(_SHRegGetBoolUSValueW, shlwapidll, 'SHRegGetBoolUSValueW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegGetBoolUSValueW]
  end;
end;

var
  _SHRegGetBoolUSValue: Pointer;

function SHRegGetBoolUSValue;
begin
  GetProcedureAddress(_SHRegGetBoolUSValue, shlwapidll, 'SHRegGetBoolUSValue'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegGetBoolUSValue]
  end;
end;

var
  _AssocCreate: Pointer;

function AssocCreate;
begin
  GetProcedureAddress(_AssocCreate, shlwapidll, 'AssocCreate');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AssocCreate]
  end;
end;

var
  _AssocQueryStringA: Pointer;

function AssocQueryStringA;
begin
  GetProcedureAddress(_AssocQueryStringA, shlwapidll, 'AssocQueryStringA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AssocQueryStringA]
  end;
end;

var
  _AssocQueryStringW: Pointer;

function AssocQueryStringW;
begin
  GetProcedureAddress(_AssocQueryStringW, shlwapidll, 'AssocQueryStringW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AssocQueryStringW]
  end;
end;

var
  _AssocQueryString: Pointer;

function AssocQueryString;
begin
  GetProcedureAddress(_AssocQueryString, shlwapidll, 'AssocQueryString'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AssocQueryString]
  end;
end;

var
  _AssocQueryStringByKeyA: Pointer;

function AssocQueryStringByKeyA;
begin
  GetProcedureAddress(_AssocQueryStringByKeyA, shlwapidll, 'AssocQueryStringByKeyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AssocQueryStringByKeyA]
  end;
end;

var
  _AssocQueryStringByKeyW: Pointer;

function AssocQueryStringByKeyW;
begin
  GetProcedureAddress(_AssocQueryStringByKeyW, shlwapidll, 'AssocQueryStringByKeyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AssocQueryStringByKeyW]
  end;
end;

var
  _AssocQueryStringByKey: Pointer;

function AssocQueryStringByKey;
begin
  GetProcedureAddress(_AssocQueryStringByKey, shlwapidll, 'AssocQueryStringByKey'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AssocQueryStringByKey]
  end;
end;

var
  _AssocQueryKeyA: Pointer;

function AssocQueryKeyA;
begin
  GetProcedureAddress(_AssocQueryKeyA, shlwapidll, 'AssocQueryKeyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AssocQueryKeyA]
  end;
end;

var
  _AssocQueryKeyW: Pointer;

function AssocQueryKeyW;
begin
  GetProcedureAddress(_AssocQueryKeyW, shlwapidll, 'AssocQueryKeyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AssocQueryKeyW]
  end;
end;

var
  _AssocQueryKey: Pointer;

function AssocQueryKey;
begin
  GetProcedureAddress(_AssocQueryKey, shlwapidll, 'AssocQueryKey'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AssocQueryKey]
  end;
end;

var
  _SHOpenRegStreamA: Pointer;

function SHOpenRegStreamA;
begin
  GetProcedureAddress(_SHOpenRegStreamA, shlwapidll, 'SHOpenRegStreamA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHOpenRegStreamA]
  end;
end;

var
  _SHOpenRegStreamW: Pointer;

function SHOpenRegStreamW;
begin
  GetProcedureAddress(_SHOpenRegStreamW, shlwapidll, 'SHOpenRegStreamW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHOpenRegStreamW]
  end;
end;

var
  _SHOpenRegStream: Pointer;

function SHOpenRegStream;
begin
  GetProcedureAddress(_SHOpenRegStream, shlwapidll, 'SHOpenRegStream'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHOpenRegStream]
  end;
end;

var
  _SHOpenRegStream2A: Pointer;

function SHOpenRegStream2A;
begin
  GetProcedureAddress(_SHOpenRegStream2A, shlwapidll, 'SHOpenRegStream2A');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHOpenRegStream2A]
  end;
end;

var
  _SHOpenRegStream2W: Pointer;

function SHOpenRegStream2W;
begin
  GetProcedureAddress(_SHOpenRegStream2W, shlwapidll, 'SHOpenRegStream2W');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHOpenRegStream2W]
  end;
end;

var
  _SHOpenRegStream2: Pointer;

function SHOpenRegStream2;
begin
  GetProcedureAddress(_SHOpenRegStream2, shlwapidll, 'SHOpenRegStream2'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHOpenRegStream2]
  end;
end;

var
  _SHCreateStreamOnFileA: Pointer;

function SHCreateStreamOnFileA;
begin
  GetProcedureAddress(_SHCreateStreamOnFileA, shlwapidll, 'SHCreateStreamOnFileA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCreateStreamOnFileA]
  end;
end;

var
  _SHCreateStreamOnFileW: Pointer;

function SHCreateStreamOnFileW;
begin
  GetProcedureAddress(_SHCreateStreamOnFileW, shlwapidll, 'SHCreateStreamOnFileW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCreateStreamOnFileW]
  end;
end;

var
  _SHCreateStreamOnFile: Pointer;

function SHCreateStreamOnFile;
begin
  GetProcedureAddress(_SHCreateStreamOnFile, shlwapidll, 'SHCreateStreamOnFile'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCreateStreamOnFile]
  end;
end;

var
  _SHAutoComplete: Pointer;

function SHAutoComplete;
begin
  GetProcedureAddress(_SHAutoComplete, shlwapidll, 'SHAutoComplete');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHAutoComplete]
  end;
end;

var
  _SHSetThreadRef: Pointer;

function SHSetThreadRef;
begin
  GetProcedureAddress(_SHSetThreadRef, shlwapidll, 'SHSetThreadRef');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHSetThreadRef]
  end;
end;

var
  _SHGetThreadRef: Pointer;

function SHGetThreadRef;
begin
  GetProcedureAddress(_SHGetThreadRef, shlwapidll, 'SHGetThreadRef');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetThreadRef]
  end;
end;

var
  _SHSkipJunction: Pointer;

function SHSkipJunction;
begin
  GetProcedureAddress(_SHSkipJunction, shlwapidll, 'SHSkipJunction');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHSkipJunction]
  end;
end;

var
  _SHCreateThread: Pointer;

function SHCreateThread;
begin
  GetProcedureAddress(_SHCreateThread, shlwapidll, 'SHCreateThread');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCreateThread]
  end;
end;

var
  _SHReleaseThreadRef: Pointer;

function SHReleaseThreadRef;
begin
  GetProcedureAddress(_SHReleaseThreadRef, shlwapidll, 'SHReleaseThreadRef');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHReleaseThreadRef]
  end;
end;

var
  _SHCreateShellPalette: Pointer;

function SHCreateShellPalette;
begin
  GetProcedureAddress(_SHCreateShellPalette, shlwapidll, 'SHCreateShellPalette');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCreateShellPalette]
  end;
end;

var
  _ColorRGBToHLS: Pointer;

procedure ColorRGBToHLS;
begin
  GetProcedureAddress(_ColorRGBToHLS, shlwapidll, 'ColorRGBToHLS');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ColorRGBToHLS]
  end;
end;

var
  _ColorHLSToRGB: Pointer;

function ColorHLSToRGB;
begin
  GetProcedureAddress(_ColorHLSToRGB, shlwapidll, 'ColorHLSToRGB');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ColorHLSToRGB]
  end;
end;

var
  _ColorAdjustLuma: Pointer;

function ColorAdjustLuma;
begin
  GetProcedureAddress(_ColorAdjustLuma, shlwapidll, 'ColorAdjustLuma');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ColorAdjustLuma]
  end;
end;

var
  _StrToInt64ExA: Pointer;

function StrToInt64ExA;
begin
  GetProcedureAddress(_StrToInt64ExA, shlwapidll, 'StrToInt64ExA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrToInt64ExA]
  end;
end;

var
  _StrToInt64ExW: Pointer;

function StrToInt64ExW;
begin
  GetProcedureAddress(_StrToInt64ExW, shlwapidll, 'StrToInt64ExW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrToInt64ExW]
  end;
end;

var
  _StrToInt64Ex: Pointer;

function StrToInt64Ex;
begin
  GetProcedureAddress(_StrToInt64Ex, shlwapidll, 'StrToInt64Ex'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrToInt64Ex]
  end;
end;

var
  _IsCharSpaceA: Pointer;

function IsCharSpaceA;
begin
  GetProcedureAddress(_IsCharSpaceA, shlwapidll, 'IsCharSpaceA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_IsCharSpaceA]
  end;
end;

var
  _IsCharSpaceW: Pointer;

function IsCharSpaceW;
begin
  GetProcedureAddress(_IsCharSpaceW, shlwapidll, 'IsCharSpaceW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_IsCharSpaceW]
  end;
end;

var
  _IsCharSpace: Pointer;

function IsCharSpace;
begin
  GetProcedureAddress(_IsCharSpace, shlwapidll, 'IsCharSpace'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_IsCharSpace]
  end;
end;

var
  _StrCmpCA: Pointer;

function StrCmpCA;
begin
  GetProcedureAddress(_StrCmpCA, shlwapidll, 'StrCmpCA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCmpCA]
  end;
end;

var
  _StrCmpCW: Pointer;

function StrCmpCW;
begin
  GetProcedureAddress(_StrCmpCW, shlwapidll, 'StrCmpCW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCmpCW]
  end;
end;

var
  _StrCmpC: Pointer;

function StrCmpC;
begin
  GetProcedureAddress(_StrCmpC, shlwapidll, 'StrCmpC'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCmpC]
  end;
end;

var
  _StrCmpICA: Pointer;

function StrCmpICA;
begin
  GetProcedureAddress(_StrCmpICA, shlwapidll, 'StrCmpICA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCmpICA]
  end;
end;

var
  _StrCmpICW: Pointer;

function StrCmpICW;
begin
  GetProcedureAddress(_StrCmpICW, shlwapidll, 'StrCmpICW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCmpICW]
  end;
end;

var
  _StrCmpIC: Pointer;

function StrCmpIC;
begin
  GetProcedureAddress(_StrCmpIC, shlwapidll, 'StrCmpIC'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_StrCmpIC]
  end;
end;

var
  _SHRegGetValueA: Pointer;

function SHRegGetValueA;
begin
  GetProcedureAddress(_SHRegGetValueA, shlwapidll, 'SHRegGetValueA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegGetValueA]
  end;
end;

var
  _SHRegGetValueW: Pointer;

function SHRegGetValueW;
begin
  GetProcedureAddress(_SHRegGetValueW, shlwapidll, 'SHRegGetValueW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegGetValueW]
  end;
end;

var
  _SHRegGetValue: Pointer;

function SHRegGetValue;
begin
  GetProcedureAddress(_SHRegGetValue, shlwapidll, 'SHRegGetValue'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegGetValue]
  end;
end;

var
  _SHRegGetIntW: Pointer;

function SHRegGetIntW;
begin
  GetProcedureAddress(_SHRegGetIntW, shlwapidll, 'SHRegGetIntW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegGetIntW]
  end;
end;

var
  _SHRegGetInt: Pointer;

function SHRegGetInt;
begin
  GetProcedureAddress(_SHRegGetInt, shlwapidll, 'SHRegGetInt'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHRegGetInt]
  end;
end;

var
  _AssocIsDangerous: Pointer;

function AssocIsDangerous;
begin
  GetProcedureAddress(_AssocIsDangerous, shlwapidll, 'AssocIsDangerous');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AssocIsDangerous]
  end;
end;

var
  _AssocGetPerceivedType: Pointer;

function AssocGetPerceivedType;
begin
  GetProcedureAddress(_AssocGetPerceivedType, shlwapidll, 'AssocGetPerceivedType');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AssocGetPerceivedType]
  end;
end;

var
  _SHCreateStreamOnFileEx: Pointer;

function SHCreateStreamOnFileEx;
begin
  GetProcedureAddress(_SHCreateStreamOnFileEx, shlwapidll, 'SHCreateStreamOnFileEx');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCreateStreamOnFileEx]
  end;
end;

var
  _GetAcceptLanguagesA: Pointer;

function GetAcceptLanguagesA;
begin
  GetProcedureAddress(_GetAcceptLanguagesA, shlwapidll, 'GetAcceptLanguagesA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetAcceptLanguagesA]
  end;
end;

var
  _GetAcceptLanguagesW: Pointer;

function GetAcceptLanguagesW;
begin
  GetProcedureAddress(_GetAcceptLanguagesW, shlwapidll, 'GetAcceptLanguagesW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetAcceptLanguagesW]
  end;
end;

var
  _GetAcceptLanguages: Pointer;

function GetAcceptLanguages;
begin
  GetProcedureAddress(_GetAcceptLanguages, shlwapidll, 'GetAcceptLanguages'+AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetAcceptLanguages]
  end;
end;

var
  _SHGetViewStatePropertyBag: Pointer;

function SHGetViewStatePropertyBag;
begin
  GetProcedureAddress(_SHGetViewStatePropertyBag, shlwapidll, 'SHGetViewStatePropertyBag');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetViewStatePropertyBag]
  end;
end;

var
  _SHAllocShared: Pointer;

function SHAllocShared;
begin
  GetProcedureAddress(_SHAllocShared, shlwapidll, 'SHAllocShared');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHAllocShared]
  end;
end;

var
  _SHFreeShared: Pointer;

function SHFreeShared;
begin
  GetProcedureAddress(_SHFreeShared, shlwapidll, 'SHFreeShared');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHFreeShared]
  end;
end;

var
  _SHLockShared: Pointer;

function SHLockShared;
begin
  GetProcedureAddress(_SHLockShared, shlwapidll, 'SHLockShared');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHLockShared]
  end;
end;

var
  _SHUnlockShared: Pointer;

function SHUnlockShared;
begin
  GetProcedureAddress(_SHUnlockShared, shlwapidll, 'SHUnlockShared');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHUnlockShared]
  end;
end;

var
  _SHCreateThreadRef: Pointer;

function SHCreateThreadRef;
begin
  GetProcedureAddress(_SHCreateThreadRef, shlwapidll, 'SHCreateThreadRef');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHCreateThreadRef]
  end;
end;

var
  _IsInternetESCEnabled: Pointer;

function IsInternetESCEnabled;
begin
  GetProcedureAddress(_IsInternetESCEnabled, shlwapidll, 'IsInternetESCEnabled');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_IsInternetESCEnabled]
  end;
end;

{$IFDEF WINXP_UP}
//stOrM!------------------------------------------------------------------------------------------------------------------------------------------

var
  _MessageBoxTimeOutA: Pointer;

function MessageBoxTimeOutA;
begin
  GetProcedureAddress(_MessageBoxTimeOutA, user32, 'MessageBoxTimeoutA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MessageBoxTimeOutA]
  end;
end;

var
  _MessageBoxTimeOutW: Pointer;

function MessageBoxTimeOutW;
begin
  GetProcedureAddress(_MessageBoxTimeOutW, user32, 'MessageBoxTimeoutW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MessageBoxTimeOutW]
  end;
end;

var
  _MessageBoxTimeOut: Pointer;

function MessageBoxTimeOut;
begin
  GetProcedureAddress(_MessageBoxTimeOut, user32, 'MessageBoxTimeout' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MessageBoxTimeOut]
  end;
end;


//------------------------------------------------------------------------------------------------------------------------------------------stOrM!
{$ENDIF WINXP_UP}

{$IFDEF WIN2000_UP}

var
  _MessageBoxCheckA: Pointer;

function MessageBoxCheckA;
begin
  GetProcedureAddress(_MessageBoxCheckA, shlwapidll,  185);  //'SHMessageBoxCheckA'
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MessageBoxCheckA]
  end;
end;

var
  _MessageBoxCheckW: Pointer;

function MessageBoxCheckW;
begin
  GetProcedureAddress(_MessageBoxCheckW, shlwapidll, 191); //'SHMessageBoxCheckW'
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MessageBoxCheckW]
  end;
end;

var
  _MessageBoxCheck: Pointer;

function MessageBoxCheck;
begin
  GetProcedureAddress(_MessageBoxCheck, shlwapidll, {$IFDEF UNICODE}191{$ELSE}185{$ENDIF UNICODE});
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MessageBoxCheck]
  end;
end;

{$ENDIF WIN2000_UP}

//------------------------------------------------------------------------------------------------------------------------------------------stOrM!

{$ENDIF DYNAMIC_LINK}
                
{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}

end.
{$ENDIF JWA_OMIT_SECTIONS}
