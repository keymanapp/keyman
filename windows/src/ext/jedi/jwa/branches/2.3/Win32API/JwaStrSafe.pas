{******************************************************************************}
{                                                                              }
{ Safer C library string routine replacements                                  }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: strsafe.h. The original Pascal                         }
{ code is: strsafe.pas, released March 2008. The initial developer of the      }
{ Pascal code is Christian Wimmer (dezipaitor at users.sourceforge dot net).   }
{                                                                              }
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
{ Description                                                                  }
{  strsafe.h -- This module defines safer C library string                     }
{               routine replacements. These are meant to make C                }
{               a bit more safe in reference to security and                   }
{               robustness                                                     }
{                                                                              }
{ Following functions are not implemented                                      }
{ StringCchPrintfEx, StringCbPrintfEx, StringCchVPrintfEx, StringCbVPrintfExA  }
{ and inline only                                                              }
{  StringCchGets, StringCbGets, StringCchGetsEx, StringCbGetsEx                }
{                                                                              }
{ The functions are not exported by any DLL.                                   }
{                                                                              }
{                                                                              }
{******************************************************************************}
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaStrSafe;
{$ENDIF JWA_OMIT_SECTIONS}




{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}
interface
//use either JwaWindows.pas or single mode units
{$IFDEF JWA_WINDOWS}
uses JwaWindows;
{$ELSE}
uses JwaWinType, JwaWinBase;
{$ENDIF}

{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}


{$IFDEF PACKAGE_CONDITIONS}
  //use by the jedi api units
  {$LINK ..\..\win32api\JwaStrSafe.obj}
{$ELSE}
  {$IFDEF JW_SINGLE_UNITS_PACKAGE}
    //well this only works when the jedi api single unit package is compiled
    //if it does not work though, the single unit is missing the
    //JW_SINGLE_UNITS_PACKAGE directive. Add it to the options of the package
    {$LINK ..\..\win32api\JwaStrSafe.obj}
  {$ELSE}
    //You have to include the source path to \win32api
    //to make it work
    {$LINK JwaStrSafe.obj}
  {$ENDIF JW_SINGLE_UNITS_PACKAGE}
{$ENDIF PACKAGE_CONDITIONS}


type
{$IFNDEF JWA_OMIT_SECTIONS}
//strsafe.obj must include 64bit code to work with _WIN64
{$IFDEF _WIN64}
  size_t = ULONGLONG;
{$ELSE}
  size_t = Cardinal;//ULONG_PTR;
{$ENDIF}
  PSize_t = ^size_t;
{$ENDIF JWA_OMIT_SECTIONS}

  STRSAFE_LPSTR = PAnsiChar;
  PSTRSAFE_LPSTR = ^STRSAFE_LPSTR;

  STRSAFE_LPCSTR = PAnsiChar;
  PSTRSAFE_LPCSTR = ^STRSAFE_LPCSTR;

  STRSAFE_LPWSTR = PWIDECHAR;
  PSTRSAFE_LPWSTR = ^STRSAFE_LPWSTR;

  STRSAFE_LPCWSTR = PWIDECHAR;
  PSTRSAFE_LPCWSTR = ^STRSAFE_LPCWSTR;


{$IFDEF UNICODE}
  STRSAFE_LPTSTR = STRSAFE_LPWSTR;
  PSTRSAFE_LPTSTR = ^STRSAFE_LPTSTR;

  STRSAFE_LPCTSTR = STRSAFE_LPCWSTR;
  PSTRSAFE_LPCTSTR = ^STRSAFE_LPCTSTR;
{$ELSE}
  STRSAFE_LPTSTR = STRSAFE_LPSTR;
  PSTRSAFE_LPTSTR = ^STRSAFE_LPTSTR;

  STRSAFE_LPCTSTR = STRSAFE_LPCSTR;
  PSTRSAFE_LPCTSTR = ^STRSAFE_LPCTSTR;
{$ENDIF}


const
  STRSAFE_MAX_CCH     = 2147483647; // max # of characters we support (same as INT_MAX)

// If both strsafe.h and ntstrsafe.h are included, only use definitions below from one.
{$IFNDEF _NTSTRSAFE_H_INCLUDED_}
const
  // Flags for controling the Ex functions
  //
  //      STRSAFE_FILL_BYTE(0xFF)                         $000000FF  // bottom byte specifies fill pattern
  STRSAFE_IGNORE_NULLS                            = $00000100;  // treat null string pointers as TEXT("") -- don't fault on NULL buffers
  STRSAFE_FILL_BEHIND_NULL                        = $00000200;  // fill in extra space behind the null terminator
  STRSAFE_FILL_ON_FAILURE                         = $00000400;  // on failure, overwrite pszDest with fill pattern and null terminate it
  STRSAFE_NULL_ON_FAILURE                         = $00000800;  // on failure, set *pszDest = TEXT('\0')
  STRSAFE_NO_TRUNCATION                           = $00001000;  // instead of returning a truncated result, copy/append nothing to pszDest and null terminate it
  STRSAFE_IGNORE_NULL_UNICODE_STRINGS             = $00010000;  // don't crash on null UNICODE_STRING pointers (STRSAFE_IGNORE_NULLS implies this flag)
  STRSAFE_UNICODE_STRING_DEST_NULL_TERMINATED     = $00020000;  // we will fail if the Dest PUNICODE_STRING's Buffer cannot be null terminated


  STRSAFE_VALID_FLAGS                    = ($000000FF or STRSAFE_IGNORE_NULLS or STRSAFE_FILL_BEHIND_NULL or STRSAFE_FILL_ON_FAILURE or STRSAFE_NULL_ON_FAILURE or STRSAFE_NO_TRUNCATION);
  STRSAFE_UNICODE_STRING_VALID_FLAGS     = (STRSAFE_VALID_FLAGS or STRSAFE_IGNORE_NULL_UNICODE_STRINGS or STRSAFE_UNICODE_STRING_DEST_NULL_TERMINATED);

  // helper macro to set the fill character and specify buffer filling
  //STRSAFE_FILL_BYTE(x)                    ((unsigned long)((x & $000000FF) or STRSAFE_FILL_BEHIND_NULL))
  function STRSAFE_FILL_BYTE(const x : Cardinal) : Cardinal;
  //STRSAFE_FAILURE_BYTE(x)                 ((unsigned long)((x & $000000FF) or STRSAFE_FILL_ON_FAILURE))
  function STRSAFE_FAILURE_BYTE(const x : Cardinal) : Cardinal;
  //STRSAFE_GET_FILL_PATTERN(dwFlags)       ((int)(dwFlags & $000000FF))
  function STRSAFE_GET_FILL_PATTERN(const dwFlags : Cardinal) : Cardinal;

{$ENDIF} // _NTSTRSAFE_H_INCLUDED_

const
  // STRSAFE error return codes
  //
  STRSAFE_E_INSUFFICIENT_BUFFER      = HRESULT($8007007A);  // $7A = 122L = ERROR_INSUFFICIENT_BUFFER
  STRSAFE_E_INVALID_PARAMETER        = HRESULT($80070057);  // $57 =  87L = ERROR_INVALID_PARAMETER
  STRSAFE_E_END_OF_FILE              = HRESULT($80070026);  // $26 =  38L = ERROR_HANDLE_EOF



{

STDAPI
StringCchPrintfX(
    __out_ecount(cchDest) LPTSTR  pszDest,
    __in size_t  cchDest,
    __in __format_string  LPCTSTR pszFormat,
    ...
    );

Routine Description:

    This routine is a safer version of the C built-in function 'sprintf'.
    The size of the destination buffer (in characters) is a parameter and
    this function will not write past the end of this buffer and it will
    ALWAYS null terminate the destination buffer (unless it is zero length).

    This function returns a hresult, and not a pointer.  It returns
    S_OK if the string was printed without truncation and null terminated,
    otherwise it will return a failure code. In failure cases it will return
    a truncated version of the ideal result.

    Usage in Delphi

    var
      S : array[0..100] of WideChar;
      SA : array[0..100] of AnsiChar;
     ...
    StringCchPrintfW(@S, sizeof(S) div 2, 'String %d %d %s', 5,6, Ansistring('Str'));
    StringCchPrintfA(@SA, sizeof(SA), 'String %d %d %s', 5,6, WideString('Str'));

Arguments:

    pszDest     -  destination string

    cchDest     -  size of destination buffer in characters
                   length must be sufficient to hold the resulting formatted
                   string, including the null terminator.

    pszFormat   -  format string which must be null terminated

    ...         -  additional parameters to be formatted according to
                   the format string

Notes:
    Behavior is undefined if destination, format strings or any arguments
    strings overlap.

    pszDest and pszFormat should not be NULL.  See StringCchPrintfEx if you
    require the handling of NULL values.

Return Value:

    S_OK           -   if there was sufficient space in the dest buffer for
                       the resultant string and it was null terminated.

    failure        -   you can use the macro HRESULT_CODE() to get a win32
                       error code for all hresult failure cases

      STRSAFE_E_INSUFFICIENT_BUFFER /
      HRESULT_CODE(hr) == ERROR_INSUFFICIENT_BUFFER
                   -   this return value is an indication that the print
                       operation failed due to insufficient space. When this
                       error occurs, the destination buffer is modified to
                       contain a truncated version of the ideal result and is
                       null terminated. This is useful for situations where
                       truncation is ok.

    It is strongly recommended to use the SUCCEEDED() / FAILED() macros to test the
    return value of this function

}



(*StringCchPrintf cannot be declared in Delphi
unless *)
(*function StringCchPrintf(
    {__out_bcount(cbDest)} pszDest : STRSAFE_LPTSTR;
    {__in} cchDest : size_t;
    {__in __format_string} pszFormat : STRSAFE_LPCTSTR
    {...}
    ) : HRESULT; stdcall;
  *)

  function StringCchPrintfW(
    {__out_bcount(cbDest)} pszDest : STRSAFE_LPWSTR;
    {__in} cchDest : size_t;
    {__in __format_string} pszFormat : STRSAFE_LPCWSTR
    {...}
    ) : HRESULT; stdcall; varargs; external;

  function StringCchPrintfA(
    {__out_bcount(cbDest)} pszDest : STRSAFE_LPSTR;
    {__in} cchDest : size_t;
    {__in __format_string} pszFormat : STRSAFE_LPCSTR
    {...}
    ) : HRESULT; stdcall; varargs; external;

{
STDAPI
StringCbPrintf(
    __out_bcount(cbDest) LPTSTR  pszDest,
    __in size_t  cbDest,
    __in __format_string LPCTSTR pszFormat,
    ...
    );

Routine Description:

    This routine is a safer version of the C built-in function 'sprintf'.
    The size of the destination buffer (in bytes) is a parameter and
    this function will not write past the end of this buffer and it will
    ALWAYS null terminate the destination buffer (unless it is zero length).

    This function returns a hresult, and not a pointer.  It returns
    S_OK if the string was printed without truncation and null terminated,
    otherwise it will return a failure code. In failure cases it will return
    a truncated version of the ideal result.

    Usage in Delphi

    var
      S : array[0..100] of WideChar;
      SA : array[0..100] of AnsiChar;
     ...
    StringCbPrintfW(@S, sizeof(S), 'String %d %d %s', 5,6, Ansistring('Str'));
    StringCbPrintfA(@SA, sizeof(SA), 'String %d %d %s', 5,6, WideString('Str'));


Arguments:

    pszDest     -  destination string

    cbDest      -  size of destination buffer in bytes
                   length must be sufficient to hold the resulting formatted
                   string, including the null terminator.

    pszFormat   -  format string which must be null terminated

    ...         -  additional parameters to be formatted according to
                   the format string

Notes:
    Behavior is undefined if destination, format strings or any arguments
    strings overlap.

    pszDest and pszFormat should not be NULL.  See StringCbPrintfEx if you
    require the handling of NULL values.


Return Value:

    S_OK           -   if there was sufficient space in the dest buffer for
                       the resultant string and it was null terminated.

    failure        -   you can use the macro HRESULT_CODE() to get a win32
                       error code for all hresult failure cases

      STRSAFE_E_INSUFFICIENT_BUFFER /
      HRESULT_CODE(hr) == ERROR_INSUFFICIENT_BUFFER
                   -   this return value is an indication that the print
                       operation failed due to insufficient space. When this
                       error occurs, the destination buffer is modified to
                       contain a truncated version of the ideal result and is
                       null terminated. This is useful for situations where
                       truncation is ok.

    It is strongly recommended to use the SUCCEEDED() / FAILED() macros to test the
    return value of this function


}



(*StringCchPrintf cannot be declared in Delphi
unless *)
(*function StringCchPrintf(
    {__out_bcount(cbDest)} pszDest : STRSAFE_LPTSTR;
    {__in} cchDest : size_t;
    {__in __format_string} pszFormat : STRSAFE_LPCTSTR
    {...}
    ) : HRESULT; stdcall;
  *)

function StringCbPrintfW(
    {__out_bcount(cbDest)} pszDest : STRSAFE_LPWSTR;
    {__in} cbDest : size_t;
    {__in __format_string} pszFormat : STRSAFE_LPCWSTR
    {...}
      ) : HRESULT; stdcall; varargs; external;

function StringCbPrintfA(
    {__out_bcount(cbDest)} pszDest : STRSAFE_LPSTR;
    {__in} cbDest : size_t;
    {__in __format_string} pszFormat : STRSAFE_LPCSTR
    {...}
    ) : HRESULT; stdcall; varargs; external;


//StringCchVPrintf, StringCbVPrintf, StringCchPrintf, StringCbPrintf,
//StringCchPrintfEx, StringCbPrintfEx, StringCchVPrintfEx, StringCbVPrintfExA


{++

STDAPI
StringCchCopy(
    OUT LPTSTR  pszDest,
    IN  size_t  cchDest,
    IN  LPCTSTR pszSrc
    );

Routine Description:

    This routine is a safer version of the C built-in function 'strcpy'.
    The size of the destination buffer (in characters) is a parameter and
    this function will not write past the end of this buffer and it will
    ALWAYS null terminate the destination buffer (unless it is zero length).

    This routine is not a replacement for strncpy.  That function will pad the
    destination string with extra null termination characters if the count is
    greater than the length of the source string, and it will fail to null
    terminate the destination string if the source string length is greater
    than or equal to the count. You can not blindly use this instead of strncpy:
    it is common for code to use it to "patch" strings and you would introduce
    errors if the code started null terminating in the middle of the string.

    This function returns a hresult, and not a pointer.  It returns
    S_OK if the string was copied without truncation and null terminated,
    otherwise it will return a failure code. In failure cases as much of
    pszSrc will be copied to pszDest as possible, and pszDest will be null
    terminated.

Arguments:

    pszDest        -   destination string

    cchDest        -   size of destination buffer in characters.
                       length must be = (_tcslen(src) + 1) to hold all of the
                       source including the null terminator

    pszSrc         -   source string which must be null terminated

Notes:
    Behavior is undefined if source and destination strings overlap.

    pszDest and pszSrc should not be NULL. See StringCchCopyEx if you require
    the handling of NULL values.

Return Value:

    S_OK           -   if there was source data and it was all copied and the
                       resultant dest string was null terminated

    failure        -   you can use the macro HRESULT_CODE() to get a win32
                       error code for all hresult failure cases

      STRSAFE_E_INSUFFICIENT_BUFFER /
      HRESULT_CODE(hr) == ERROR_INSUFFICIENT_BUFFER
                   -   this return value is an indication that the copy
                       operation failed due to insufficient space. When this
                       error occurs, the destination buffer is modified to
                       contain a truncated version of the ideal result and is
                       null terminated. This is useful for situations where
                       truncation is ok

    It is strongly recommended to use the SUCCEEDED() / FAILED() macros to test the
    return value of this function.

--}
function StringCchCopyW(
    {__out_ecount(cchDest)}pszDest : STRSAFE_LPWSTR;
    {__in} cchDest : size_t;
    {__in} const pszSrc : STRSAFE_LPCWSTR) : HRESULT; stdcall; forward; external;

function StringCchCopyA(
    {__out_ecount(cchDest)}pszDest : STRSAFE_LPSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCSTR) : HRESULT; stdcall; forward; external;

function StringCchCopy(
    {__out_ecount(cchDest)}pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCTSTR) : HRESULT; stdcall;


{++

STDAPI
StringCbCopy(
    OUT LPTSTR pszDest,
    IN  size_t cbDest,
    IN  LPCTSTR pszSrc
    );

Routine Description:

    This routine is a safer version of the C built-in function 'strcpy'.
    The size of the destination buffer (in bytes) is a parameter and this
    function will not write past the end of this buffer and it will ALWAYS
    null terminate the destination buffer (unless it is zero length).

    This routine is not a replacement for strncpy.  That function will pad the
    destination string with extra null termination characters if the count is
    greater than the length of the source string, and it will fail to null
    terminate the destination string if the source string length is greater
    than or equal to the count. You can not blindly use this instead of strncpy:
    it is common for code to use it to "patch" strings and you would introduce
    errors if the code started null terminating in the middle of the string.

    This function returns a hresult, and not a pointer.  It returns
    S_OK if the string was copied without truncation and null terminated,
    otherwise it will return a failure code. In failure cases as much of pszSrc
    will be copied to pszDest as possible, and pszDest will be null terminated.

Arguments:

    pszDest        -   destination string

    cbDest         -   size of destination buffer in bytes.
                       length must be = ((_tcslen(src) + 1) * sizeof(TCHAR)) to
                       hold all of the source including the null terminator

    pszSrc         -   source string which must be null terminated

Notes:
    Behavior is undefined if source and destination strings overlap.

    pszDest and pszSrc should not be NULL.  See StringCbCopyEx if you require
    the handling of NULL values.

Return Value:

    S_OK           -   if there was source data and it was all copied and the
                       resultant dest string was null terminated

    failure        -   you can use the macro HRESULT_CODE() to get a win32
                       error code for all hresult failure cases

      STRSAFE_E_INSUFFICIENT_BUFFER /
      HRESULT_CODE(hr) == ERROR_INSUFFICIENT_BUFFER
                   -   this return value is an indication that the copy
                       operation failed due to insufficient space. When this
                       error occurs, the destination buffer is modified to
                       contain a truncated version of the ideal result and is
                       null terminated. This is useful for situations where
                       truncation is ok

    It is strongly recommended to use the SUCCEEDED() / FAILED() macros to test the
    return value of this function.

--}


function StringCbCopyA(
    {__out_bcount(cbDest)}pszDest : STRSAFE_LPSTR;
    {__in}cbDest  : size_t;
    {__in}const pszSrc : STRSAFE_LPCSTR) : HRESULT; stdcall; forward; external;

function StringCbCopyW(
    {__out_bcount(cbDest)}pszDest : STRSAFE_LPWSTR;
    {__in}cbDest  : size_t;
    {__in}const pszSrc : STRSAFE_LPCWSTR) : HRESULT; stdcall; forward; external;

function StringCbCopy(
    {__out_bcount(cbDest)}pszDest : STRSAFE_LPTSTR;
    {__in}cbDest  : size_t;
    {__in}const pszSrc : STRSAFE_LPCTSTR) : HRESULT; stdcall;

{++

STDAPI
StringCchCopyEx(
    OUT LPTSTR  pszDest         OPTIONAL,
    IN  size_t  cchDest,
    IN  LPCTSTR pszSrc          OPTIONAL,
    OUT LPTSTR* ppszDestEnd     OPTIONAL,
    OUT size_t* pcchRemaining   OPTIONAL,
    IN  DWORD   dwFlags
    );

Routine Description:

    This routine is a safer version of the C built-in function 'strcpy' with
    some additional parameters.  In addition to functionality provided by
    StringCchCopy, this routine also returns a pointer to the end of the
    destination string and the number of characters left in the destination string
    including the null terminator. The flags parameter allows additional controls.

Arguments:

    pszDest         -   destination string

    cchDest         -   size of destination buffer in characters.
                        length must be = (_tcslen(pszSrc) + 1) to hold all of
                        the source including the null terminator

    pszSrc          -   source string which must be null terminated

    ppszDestEnd     -   if ppszDestEnd is non-null, the function will return a
                        pointer to the end of the destination string.  If the
                        function copied any data, the result will point to the
                        null termination character

    pcchRemaining   -   if pcchRemaining is non-null, the function will return the
                        number of characters left in the destination string,
                        including the null terminator

    dwFlags         -   controls some details of the string copy:

        STRSAFE_FILL_BEHIND_NULL
                    if the function succeeds, the low byte of dwFlags will be
                    used to fill the uninitialize part of destination buffer
                    behind the null terminator

        STRSAFE_IGNORE_NULLS
                    treat NULL string pointers like empty strings (TEXT("")).
                    this flag is useful for emulating functions like lstrcpy

        STRSAFE_FILL_ON_FAILURE
                    if the function fails, the low byte of dwFlags will be
                    used to fill all of the destination buffer, and it will
                    be null terminated. This will overwrite any truncated
                    string returned when the failure is
                    STRSAFE_E_INSUFFICIENT_BUFFER

        STRSAFE_NO_TRUNCATION /
        STRSAFE_NULL_ON_FAILURE
                    if the function fails, the destination buffer will be set
                    to the empty string. This will overwrite any truncated string
                    returned when the failure is STRSAFE_E_INSUFFICIENT_BUFFER.

Notes:
    Behavior is undefined if source and destination strings overlap.

    pszDest and pszSrc should not be NULL unless the STRSAFE_IGNORE_NULLS flag
    is specified.  If STRSAFE_IGNORE_NULLS is passed, both pszDest and pszSrc
    may be NULL.  An error may still be returned even though NULLS are ignored
    due to insufficient space.

Return Value:

    S_OK           -   if there was source data and it was all copied and the
                       resultant dest string was null terminated

    failure        -   you can use the macro HRESULT_CODE() to get a win32
                       error code for all hresult failure cases

      STRSAFE_E_INSUFFICIENT_BUFFER /
      HRESULT_CODE(hr) == ERROR_INSUFFICIENT_BUFFER
                   -   this return value is an indication that the copy
                       operation failed due to insufficient space. When this
                       error occurs, the destination buffer is modified to
                       contain a truncated version of the ideal result and is
                       null terminated. This is useful for situations where
                       truncation is ok.

    It is strongly recommended to use the SUCCEEDED() / FAILED() macros to test the
    return value of this function

--}

function StringCchCopyExA(
    {__out_ecount(cchDest)}pszDest : STRSAFE_LPSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCSTR;
    {__deref_opt_out_ecount(pcchRemaining^)}ppszDestEnd : PSTRSAFE_LPSTR;
    {__out_opt}pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal) : HRESULT; stdcall; forward; external;

function StringCchCopyExW(
    {__out_ecount(cchDest)}pszDest : STRSAFE_LPWSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCWSTR;
    {__deref_opt_out_ecount(pcchRemaining^)}ppszDestEnd : PSTRSAFE_LPWSTR;
    {__out_opt}pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal) : HRESULT; stdcall; forward; external;

function StringCchCopyEx(
    {__out_ecount(cchDest)}pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCTSTR;
    {__deref_opt_out_ecount(pcchRemaining^)}ppszDestEnd : PSTRSAFE_LPTSTR;
    {__out_opt}pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal) : HRESULT; stdcall;

{++

STDAPI
StringCbCopyEx(
    OUT LPTSTR  pszDest         OPTIONAL,
    IN  size_t  cbDest,
    IN  LPCTSTR pszSrc          OPTIONAL,
    OUT LPTSTR* ppszDestEnd     OPTIONAL,
    OUT size_t* pcbRemaining    OPTIONAL,
    IN  DWORD   dwFlags
    );

Routine Description:

    This routine is a safer version of the C built-in function 'strcpy' with
    some additional parameters.  In addition to functionality provided by
    StringCbCopy, this routine also returns a pointer to the end of the
    destination string and the number of bytes left in the destination string
    including the null terminator. The flags parameter allows additional controls.

Arguments:

    pszDest         -   destination string

    cbDest          -   size of destination buffer in bytes.
                        length must be ((_tcslen(pszSrc) + 1) * sizeof(TCHAR)) to
                        hold all of the source including the null terminator

    pszSrc          -   source string which must be null terminated

    ppszDestEnd     -   if ppszDestEnd is non-null, the function will return a
                        pointer to the end of the destination string.  If the
                        function copied any data, the result will point to the
                        null termination character

    pcbRemaining    -   pcbRemaining is non-null,the function will return the
                        number of bytes left in the destination string,
                        including the null terminator

    dwFlags         -   controls some details of the string copy:

        STRSAFE_FILL_BEHIND_NULL
                    if the function succeeds, the low byte of dwFlags will be
                    used to fill the uninitialize part of destination buffer
                    behind the null terminator

        STRSAFE_IGNORE_NULLS
                    treat NULL string pointers like empty strings (TEXT("")).
                    this flag is useful for emulating functions like lstrcpy

        STRSAFE_FILL_ON_FAILURE
                    if the function fails, the low byte of dwFlags will be
                    used to fill all of the destination buffer, and it will
                    be null terminated. This will overwrite any truncated
                    string returned when the failure is
                    STRSAFE_E_INSUFFICIENT_BUFFER

        STRSAFE_NO_TRUNCATION /
        STRSAFE_NULL_ON_FAILURE
                    if the function fails, the destination buffer will be set
                    to the empty string. This will overwrite any truncated string
                    returned when the failure is STRSAFE_E_INSUFFICIENT_BUFFER.

Notes:
    Behavior is undefined if source and destination strings overlap.

    pszDest and pszSrc should not be NULL unless the STRSAFE_IGNORE_NULLS flag
    is specified.  If STRSAFE_IGNORE_NULLS is passed, both pszDest and pszSrc
    may be NULL.  An error may still be returned even though NULLS are ignored
    due to insufficient space.

Return Value:

    S_OK           -   if there was source data and it was all copied and the
                       resultant dest string was null terminated

    failure        -   you can use the macro HRESULT_CODE() to get a win32
                       error code for all hresult failure cases

      STRSAFE_E_INSUFFICIENT_BUFFER /
      HRESULT_CODE(hr) == ERROR_INSUFFICIENT_BUFFER
                   -   this return value is an indication that the copy
                       operation failed due to insufficient space. When this
                       error occurs, the destination buffer is modified to
                       contain a truncated version of the ideal result and is
                       null terminated. This is useful for situations where
                       truncation is ok.

    It is strongly recommended to use the SUCCEEDED() / FAILED() macros to test the
    return value of this function

--}
function StringCbCopyExA(
    {__out_bcount(cbDest)}pszDest : STRSAFE_LPSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCSTR;
    {__deref_opt_out_bcount(pcbRemaining^)}ppszDestEnd : PSTRSAFE_LPSTR;
    {__out_opt}pcbRemaining : PSize_t;
    {__in}dwFlags : Cardinal) : HRESULT; stdcall; forward; external;

function StringCbCopyExW(
    {__out_bcount(cbDest)}pszDest : STRSAFE_LPWSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCWSTR;
    {__deref_opt_out_bcount(pcbRemaining^)}ppszDestEnd : PSTRSAFE_LPWSTR;
    {__out_opt}pcbRemaining : PSize_t;
    {__in}dwFlags : Cardinal) : HRESULT; stdcall; forward; external;

function StringCbCopyEx(
    {__out_bcount(cbDest)}pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCTSTR;
    {__deref_opt_out_bcount(pcbRemaining^)}ppszDestEnd : PSTRSAFE_LPTSTR;
    {__out_opt}pcbRemaining : PSize_t;
    {__in}dwFlags : Cardinal) : HRESULT; stdcall;


{++

STDAPI
StringCchCopyN(
    OUT LPTSTR  pszDest,
    IN  size_t  cchDest,
    IN  LPCTSTR pszSrc,
    IN  size_t  cchToCopy
    );

Routine Description:

    This routine is a safer version of the C built-in function 'strncpy'.
    The size of the destination buffer (in characters) is a parameter and
    this function will not write past the end of this buffer and it will
    ALWAYS null terminate the destination buffer (unless it is zero length).

    This routine is meant as a replacement for strncpy, but it does behave
    differently. This function will not pad the destination buffer with extra
    null termination characters if cchToCopy is greater than the length of pszSrc.

    This function returns a hresult, and not a pointer.  It returns
    S_OK if the entire string or the first cchToCopy characters were copied
    without truncation and the resultant destination string was null terminated,
    otherwise it will return a failure code. In failure cases as much of pszSrc
    will be copied to pszDest as possible, and pszDest will be null terminated.

Arguments:

    pszDest        -   destination string

    cchDest        -   size of destination buffer in characters.
                       length must be = (_tcslen(src) + 1) to hold all of the
                       source including the null terminator

    pszSrc         -   source string

    cchToCopy      -   maximum number of characters to copy from source string,
                       not including the null terminator.

Notes:
    Behavior is undefined if source and destination strings overlap.

    pszDest and pszSrc should not be NULL. See StringCchCopyNEx if you require
    the handling of NULL values.

Return Value:

    S_OK           -   if there was source data and it was all copied and the
                       resultant dest string was null terminated

    failure        -   you can use the macro HRESULT_CODE() to get a win32
                       error code for all hresult failure cases

      STRSAFE_E_INSUFFICIENT_BUFFER /
      HRESULT_CODE(hr) == ERROR_INSUFFICIENT_BUFFER
                   -   this return value is an indication that the copy
                       operation failed due to insufficient space. When this
                       error occurs, the destination buffer is modified to
                       contain a truncated version of the ideal result and is
                       null terminated. This is useful for situations where
                       truncation is ok

    It is strongly recommended to use the SUCCEEDED() / FAILED() macros to test the
    return value of this function.

--}
function StringCchCopyNA(
    {__out_ecount(cchDest)}pszDest : STRSAFE_LPSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCSTR;
    {__in}cchToCopy : size_t) : HRESULT; stdcall; forward; external;

function StringCchCopyNW(
    {__out_ecount(cchDest)}pszDest : STRSAFE_LPWSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCWSTR;
    {__in}cchToCopy : size_t) : HRESULT; stdcall; forward; external;

function StringCchCopyN(
    {__out_ecount(cchDest)}pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCTSTR;
    {__in}cchToCopy : size_t) : HRESULT; stdcall;

{++

STDAPI
StringCbCopyN(
    OUT LPTSTR  pszDest,
    IN  size_t  cbDest,
    IN  LPCTSTR pszSrc,
    IN  size_t  cbToCopy
    );

Routine Description:

    This routine is a safer version of the C built-in function 'strncpy'.
    The size of the destination buffer (in bytes) is a parameter and this
    function will not write past the end of this buffer and it will ALWAYS
    null terminate the destination buffer (unless it is zero length).

    This routine is meant as a replacement for strncpy, but it does behave
    differently. This function will not pad the destination buffer with extra
    null termination characters if cbToCopy is greater than the size of pszSrc.

    This function returns a hresult, and not a pointer.  It returns
    S_OK if the entire string or the first cbToCopy characters were
    copied without truncation and the resultant destination string was null
    terminated, otherwise it will return a failure code. In failure cases as
    much of pszSrc will be copied to pszDest as possible, and pszDest will be
    null terminated.

Arguments:

    pszDest        -   destination string

    cbDest         -   size of destination buffer in bytes.
                       length must be = ((_tcslen(src) + 1) * sizeof(TCHAR)) to
                       hold all of the source including the null terminator

    pszSrc         -   source string

    cbToCopy       -   maximum number of bytes to copy from source string,
                       not including the null terminator.

Notes:
    Behavior is undefined if source and destination strings overlap.

    pszDest and pszSrc should not be NULL.  See StringCbCopyEx if you require
    the handling of NULL values.

Return Value:

    S_OK           -   if there was source data and it was all copied and the
                       resultant dest string was null terminated

    failure        -   you can use the macro HRESULT_CODE() to get a win32
                       error code for all hresult failure cases

      STRSAFE_E_INSUFFICIENT_BUFFER /
      HRESULT_CODE(hr) == ERROR_INSUFFICIENT_BUFFER
                   -   this return value is an indication that the copy
                       operation failed due to insufficient space. When this
                       error occurs, the destination buffer is modified to
                       contain a truncated version of the ideal result and is
                       null terminated. This is useful for situations where
                       truncation is ok

    It is strongly recommended to use the SUCCEEDED() / FAILED() macros to test the
    return value of this function.

--}

function StringCbCopyNA(
    {__out_bcount(cbDest)}pszDest : STRSAFE_LPSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCSTR;
    {__in}cchToCopy : size_t) : HRESULT; stdcall; forward; external;

function StringCbCopyNW(
    {__out_bcount(cbDest)}pszDest : STRSAFE_LPWSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCWSTR;
    {__in}cchToCopy : size_t) : HRESULT; stdcall; forward; external;

function StringCbCopyN(
    {__out_bcount(cbDest)}pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCTSTR;
    {__in}cchToCopy : size_t) : HRESULT; stdcall;


{++

STDAPI
StringCchCopyNEx(
    OUT LPTSTR  pszDest         OPTIONAL,
    IN  size_t  cchDest,
    IN  LPCTSTR pszSrc          OPTIONAL,
    IN  size_t  cchToCopy,
    OUT LPTSTR* ppszDestEnd     OPTIONAL,
    OUT size_t* pcchRemaining   OPTIONAL,
    IN  DWORD   dwFlags
    );

Routine Description:

    This routine is a safer version of the C built-in function 'strncpy' with
    some additional parameters.  In addition to functionality provided by
    StringCchCopyN, this routine also returns a pointer to the end of the
    destination string and the number of characters left in the destination
    string including the null terminator. The flags parameter allows
    additional controls.

    This routine is meant as a replacement for strncpy, but it does behave
    differently. This function will not pad the destination buffer with extra
    null termination characters if cchToCopy is greater than the length of pszSrc.

Arguments:

    pszDest         -   destination string

    cchDest         -   size of destination buffer in characters.
                        length must be = (_tcslen(pszSrc) + 1) to hold all of
                        the source including the null terminator

    pszSrc          -   source string

    cchToCopy       -   maximum number of characters to copy from the source
                        string

    ppszDestEnd     -   if ppszDestEnd is non-null, the function will return a
                        pointer to the end of the destination string.  If the
                        function copied any data, the result will point to the
                        null termination character

    pcchRemaining   -   if pcchRemaining is non-null, the function will return the
                        number of characters left in the destination string,
                        including the null terminator

    dwFlags         -   controls some details of the string copy:

        STRSAFE_FILL_BEHIND_NULL
                    if the function succeeds, the low byte of dwFlags will be
                    used to fill the uninitialize part of destination buffer
                    behind the null terminator

        STRSAFE_IGNORE_NULLS
                    treat NULL string pointers like empty strings (TEXT("")).
                    this flag is useful for emulating functions like lstrcpy

        STRSAFE_FILL_ON_FAILURE
                    if the function fails, the low byte of dwFlags will be
                    used to fill all of the destination buffer, and it will
                    be null terminated. This will overwrite any truncated
                    string returned when the failure is
                    STRSAFE_E_INSUFFICIENT_BUFFER

        STRSAFE_NO_TRUNCATION /
        STRSAFE_NULL_ON_FAILURE
                    if the function fails, the destination buffer will be set
                    to the empty string. This will overwrite any truncated string
                    returned when the failure is STRSAFE_E_INSUFFICIENT_BUFFER.

Notes:
    Behavior is undefined if source and destination strings overlap.

    pszDest and pszSrc should not be NULL unless the STRSAFE_IGNORE_NULLS flag
    is specified. If STRSAFE_IGNORE_NULLS is passed, both pszDest and pszSrc
    may be NULL. An error may still be returned even though NULLS are ignored
    due to insufficient space.

Return Value:

    S_OK           -   if there was source data and it was all copied and the
                       resultant dest string was null terminated

    failure        -   you can use the macro HRESULT_CODE() to get a win32
                       error code for all hresult failure cases

      STRSAFE_E_INSUFFICIENT_BUFFER /
      HRESULT_CODE(hr) == ERROR_INSUFFICIENT_BUFFER
                   -   this return value is an indication that the copy
                       operation failed due to insufficient space. When this
                       error occurs, the destination buffer is modified to
                       contain a truncated version of the ideal result and is
                       null terminated. This is useful for situations where
                       truncation is ok.

    It is strongly recommended to use the SUCCEEDED() / FAILED() macros to test the
    return value of this function

--}
function StringCchCopyNExA(
    {__in_ecount(cchDest)}pszDest : STRSAFE_LPSTR;
    {__in}cchDest : size_t;
    {__in_ecount(cchToCopy)}const pszSrc : STRSAFE_LPCSTR;
    {__in}cchToCopy : size_t;
    {__deref_opt_out_ecount(pcchRemaining^)}ppszDestEnd : PSTRSAFE_LPSTR;
    {__out_opt size_t}pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal)  : HRESULT; stdcall; forward; external;

function StringCchCopyNExW(
    {__in_ecount(cchDest)}pszDest : STRSAFE_LPWSTR;
    {__in}cchDest : size_t;
    {__in_ecount(cchToCopy)}const pszSrc : STRSAFE_LPCWSTR;
    {__in}cchToCopy : size_t;
    {__deref_opt_out_ecount(pcchRemaining^)}ppszDestEnd : PSTRSAFE_LPWSTR;
    {__out_opt size_t}pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal)  : HRESULT; stdcall; forward; external;


function StringCchCopyNEx(
    {__in_ecount(cchDest)}pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in_ecount(cchToCopy)}const pszSrc : STRSAFE_LPCTSTR;
    {__in}cchToCopy : size_t;
    {__deref_opt_out_ecount(pcchRemaining^)}ppszDestEnd : PSTRSAFE_LPTSTR;
    {__out_opt size_t}pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal)  : HRESULT; stdcall;
{++

STDAPI
StringCbCopyNEx(
    OUT LPTSTR  pszDest         OPTIONAL,
    IN  size_t  cbDest,
    IN  LPCTSTR pszSrc          OPTIONAL,
    IN  size_t  cbToCopy,
    OUT LPTSTR* ppszDestEnd     OPTIONAL,
    OUT size_t* pcbRemaining    OPTIONAL,
    IN  DWORD   dwFlags
    );

Routine Description:

    This routine is a safer version of the C built-in function 'strncpy' with
    some additional parameters.  In addition to functionality provided by
    StringCbCopyN, this routine also returns a pointer to the end of the
    destination string and the number of bytes left in the destination string
    including the null terminator. The flags parameter allows additional controls.

    This routine is meant as a replacement for strncpy, but it does behave
    differently. This function will not pad the destination buffer with extra
    null termination characters if cbToCopy is greater than the size of pszSrc.

Arguments:

    pszDest         -   destination string

    cbDest          -   size of destination buffer in bytes.
                        length must be ((_tcslen(pszSrc) + 1) * sizeof(TCHAR)) to
                        hold all of the source including the null terminator

    pszSrc          -   source string

    cbToCopy        -   maximum number of bytes to copy from source string

    ppszDestEnd     -   if ppszDestEnd is non-null, the function will return a
                        pointer to the end of the destination string.  If the
                        function copied any data, the result will point to the
                        null termination character

    pcbRemaining    -   pcbRemaining is non-null,the function will return the
                        number of bytes left in the destination string,
                        including the null terminator

    dwFlags         -   controls some details of the string copy:

        STRSAFE_FILL_BEHIND_NULL
                    if the function succeeds, the low byte of dwFlags will be
                    used to fill the uninitialize part of destination buffer
                    behind the null terminator

        STRSAFE_IGNORE_NULLS
                    treat NULL string pointers like empty strings (TEXT("")).
                    this flag is useful for emulating functions like lstrcpy

        STRSAFE_FILL_ON_FAILURE
                    if the function fails, the low byte of dwFlags will be
                    used to fill all of the destination buffer, and it will
                    be null terminated. This will overwrite any truncated
                    string returned when the failure is
                    STRSAFE_E_INSUFFICIENT_BUFFER

        STRSAFE_NO_TRUNCATION /
        STRSAFE_NULL_ON_FAILURE
                    if the function fails, the destination buffer will be set
                    to the empty string. This will overwrite any truncated string
                    returned when the failure is STRSAFE_E_INSUFFICIENT_BUFFER.

Notes:
    Behavior is undefined if source and destination strings overlap.

    pszDest and pszSrc should not be NULL unless the STRSAFE_IGNORE_NULLS flag
    is specified.  If STRSAFE_IGNORE_NULLS is passed, both pszDest and pszSrc
    may be NULL.  An error may still be returned even though NULLS are ignored
    due to insufficient space.

Return Value:

    S_OK           -   if there was source data and it was all copied and the
                       resultant dest string was null terminated

    failure        -   you can use the macro HRESULT_CODE() to get a win32
                       error code for all hresult failure cases

      STRSAFE_E_INSUFFICIENT_BUFFER /
      HRESULT_CODE(hr) == ERROR_INSUFFICIENT_BUFFER
                   -   this return value is an indication that the copy
                       operation failed due to insufficient space. When this
                       error occurs, the destination buffer is modified to
                       contain a truncated version of the ideal result and is
                       null terminated. This is useful for situations where
                       truncation is ok.

    It is strongly recommended to use the SUCCEEDED() / FAILED() macros to test the
    return value of this function

--}
function StringCbCopyNExA(
    {__out_bcount(cbDest) }pszDest : STRSAFE_LPSTR;
    {__in}cchDest : size_t;
    {__in_bcount(cbToCopy) }const pszSrc : STRSAFE_LPCSTR;
    {__in}cchToCopy : size_t;
    {__deref_opt_out_bcount(pcbRemaining^)}ppszDestEnd : PSTRSAFE_LPSTR;
    {__out_opt}pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal)  : HRESULT; stdcall; forward; external;

function StringCbCopyNExW(
    {__out_bcount(cbDest) }pszDest : STRSAFE_LPWSTR;
    {__in}cchDest : size_t;
    {__in_bcount(cbToCopy) }const pszSrc : STRSAFE_LPCWSTR;
    {__in}cchToCopy : size_t;
    {__deref_opt_out_bcount(pcbRemaining^)}ppszDestEnd : PSTRSAFE_LPWSTR;
    {__out_opt}pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal)  : HRESULT; stdcall; forward; external;

function StringCbCopyNEx(
    {__out_bcount(cbDest) }pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in_bcount(cbToCopy) }const pszSrc : STRSAFE_LPCTSTR;
    {__in}cchToCopy : size_t;
    {__deref_opt_out_bcount(pcbRemaining^)}ppszDestEnd : PSTRSAFE_LPTSTR;
    {__out_opt}pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal)  : HRESULT; stdcall;


{

STDAPI
StringCchCat(
    IN OUT LPTSTR  pszDest,
    IN     size_t  cchDest,
    IN     LPCTSTR pszSrc
    );

Routine Description:

    This routine is a safer version of the C built-in function 'strcat'.
    The size of the destination buffer (in characters) is a parameter and this
    function will not write past the end of this buffer and it will ALWAYS
    null terminate the destination buffer (unless it is zero length).

    This function returns a hresult, and not a pointer.  It returns
    S_OK if the string was concatenated without truncation and null terminated,
    otherwise it will return a failure code. In failure cases as much of pszSrc
    will be appended to pszDest as possible, and pszDest will be null
    terminated.

Arguments:

    pszDest     -  destination string which must be null terminated

    cchDest     -  size of destination buffer in characters.
                   length must be = (_tcslen(pszDest) + _tcslen(pszSrc) + 1)
                   to hold all of the combine string plus the null
                   terminator

    pszSrc      -  source string which must be null terminated

Notes:
    Behavior is undefined if source and destination strings overlap.

    pszDest and pszSrc should not be NULL.  See StringCchCatEx if you require
    the handling of NULL values.

Return Value:

    S_OK           -   if there was source data and it was all concatenated and
                       the resultant dest string was null terminated

    failure        -   you can use the macro HRESULT_CODE() to get a win32
                       error code for all hresult failure cases

      STRSAFE_E_INSUFFICIENT_BUFFER /
      HRESULT_CODE(hr) == ERROR_INSUFFICIENT_BUFFER
                   -   this return value is an indication that the operation
                       failed due to insufficient space. When this error occurs,
                       the destination buffer is modified to contain a truncated
                       version of the ideal result and is null terminated. This
                       is useful for situations where truncation is ok.

    It is strongly recommended to use the SUCCEEDED() / FAILED() macros to test the
    return value of this function

--}
function StringCchCatA(
    {__inout_ecount(cchDest)}pszDest : STRSAFE_LPSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCSTR) : HRESULT; stdcall; forward; external;

function StringCchCatW(
    {__inout_ecount(cchDest)}pszDest : STRSAFE_LPWSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCWSTR) : HRESULT; stdcall; forward; external;

function StringCchCat(
    {__inout_ecount(cchDest)}pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCTSTR) : HRESULT; stdcall;

{++

STDAPI
StringCbCat(
    IN OUT LPTSTR  pszDest,
    IN     size_t  cbDest,
    IN     LPCTSTR pszSrc
    );

Routine Description:

    This routine is a safer version of the C built-in function 'strcat'.
    The size of the destination buffer (in bytes) is a parameter and this
    function will not write past the end of this buffer and it will ALWAYS
    null terminate the destination buffer (unless it is zero length).

    This function returns a hresult, and not a pointer.  It returns
    S_OK if the string was concatenated without truncation and null terminated,
    otherwise it will return a failure code. In failure cases as much of pszSrc
    will be appended to pszDest as possible, and pszDest will be null
    terminated.

Arguments:

    pszDest     -  destination string which must be null terminated

    cbDest      -  size of destination buffer in bytes.
                   length must be = ((_tcslen(pszDest) + _tcslen(pszSrc) + 1) * sizeof(TCHAR)
                   to hold all of the combine string plus the null
                   terminator

    pszSrc      -  source string which must be null terminated

Notes:
    Behavior is undefined if source and destination strings overlap.

    pszDest and pszSrc should not be NULL.  See StringCbCatEx if you require
    the handling of NULL values.

Return Value:

    S_OK           -   if there was source data and it was all concatenated and
                       the resultant dest string was null terminated

    failure        -   you can use the macro HRESULT_CODE() to get a win32
                       error code for all hresult failure cases

      STRSAFE_E_INSUFFICIENT_BUFFER /
      HRESULT_CODE(hr) == ERROR_INSUFFICIENT_BUFFER
                   -   this return value is an indication that the operation
                       failed due to insufficient space. When this error occurs,
                       the destination buffer is modified to contain a truncated
                       version of the ideal result and is null terminated. This
                       is useful for situations where truncation is ok.

    It is strongly recommended to use the SUCCEEDED() / FAILED() macros to test the
    return value of this function

--}

function StringCbCatA(
    {__inout_bcount(cbDest)}pszDest : STRSAFE_LPSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCSTR) : HRESULT; stdcall; forward; external;


function StringCbCatW(
    {__inout_bcount(cbDest)}pszDest : STRSAFE_LPWSTR;
    {__in}cbDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCWSTR) : HRESULT; stdcall; forward; external;

function StringCbCat(
    {__inout_bcount(cbDest)}pszDest : STRSAFE_LPTSTR;
    {__in}cbDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCTSTR) : HRESULT; stdcall;
{++

STDAPI
StringCchCatEx(
    IN OUT LPTSTR  pszDest         OPTIONAL,
    IN     size_t  cchDest,
    IN     LPCTSTR pszSrc          OPTIONAL,
    OUT    LPTSTR* ppszDestEnd     OPTIONAL,
    OUT    size_t* pcchRemaining   OPTIONAL,
    IN     DWORD   dwFlags
    );

Routine Description:

    This routine is a safer version of the C built-in function 'strcat' with
    some additional parameters.  In addition to functionality provided by
    StringCchCat, this routine also returns a pointer to the end of the
    destination string and the number of characters left in the destination string
    including the null terminator. The flags parameter allows additional controls.

Arguments:

    pszDest         -   destination string which must be null terminated

    cchDest         -   size of destination buffer in characters
                        length must be (_tcslen(pszDest) + _tcslen(pszSrc) + 1)
                        to hold all of the combine string plus the null
                        terminator.

    pszSrc          -   source string which must be null terminated

    ppszDestEnd     -   if ppszDestEnd is non-null, the function will return a
                        pointer to the end of the destination string.  If the
                        function appended any data, the result will point to the
                        null termination character

    pcchRemaining   -   if pcchRemaining is non-null, the function will return the
                        number of characters left in the destination string,
                        including the null terminator

    dwFlags         -   controls some details of the string copy:

        STRSAFE_FILL_BEHIND_NULL
                    if the function succeeds, the low byte of dwFlags will be
                    used to fill the uninitialize part of destination buffer
                    behind the null terminator

        STRSAFE_IGNORE_NULLS
                    treat NULL string pointers like empty strings (TEXT("")).
                    this flag is useful for emulating functions like lstrcat

        STRSAFE_FILL_ON_FAILURE
                    if the function fails, the low byte of dwFlags will be
                    used to fill all of the destination buffer, and it will
                    be null terminated. This will overwrite any pre-existing
                    or truncated string

        STRSAFE_NULL_ON_FAILURE
                    if the function fails, the destination buffer will be set
                    to the empty string. This will overwrite any pre-existing or
                    truncated string

        STRSAFE_NO_TRUNCATION
                    if the function returns STRSAFE_E_INSUFFICIENT_BUFFER, pszDest
                    will not contain a truncated string, it will remain unchanged.

Notes:
    Behavior is undefined if source and destination strings overlap.

    pszDest and pszSrc should not be NULL unless the STRSAFE_IGNORE_NULLS flag
    is specified.  If STRSAFE_IGNORE_NULLS is passed, both pszDest and pszSrc
    may be NULL.  An error may still be returned even though NULLS are ignored
    due to insufficient space.

Return Value:

    S_OK           -   if there was source data and it was all concatenated and
                       the resultant dest string was null terminated

    failure        -   you can use the macro HRESULT_CODE() to get a win32
                       error code for all hresult failure cases

      STRSAFE_E_INSUFFICIENT_BUFFER /
      HRESULT_CODE(hr) == ERROR_INSUFFICIENT_BUFFER
                   -   this return value is an indication that the operation
                       failed due to insufficient space. When this error
                       occurs, the destination buffer is modified to contain
                       a truncated version of the ideal result and is null
                       terminated. This is useful for situations where
                       truncation is ok.

    It is strongly recommended to use the SUCCEEDED() / FAILED() macros to test the
    return value of this function

--}
function StringCchCatExA(
    {__inout_ecount(cchDest)}pszDest : STRSAFE_LPSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCSTR;
    {__deref_opt_out_ecount(pcchRemaining^)}ppszDestEnd : PSTRSAFE_LPSTR;
    {__out_opt} pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal) : HRESULT; stdcall; forward; external;

function StringCchCatExW(
    {__inout_ecount(cchDest)}pszDest : STRSAFE_LPWSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCWSTR;
    {__deref_opt_out_ecount(pcchRemaining^)}ppszDestEnd : PSTRSAFE_LPWSTR;
    {__out_opt} pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal) : HRESULT; stdcall; forward; external;

function StringCchCatEx(
    {__inout_ecount(cchDest)}pszDest : STRSAFE_LPTSTR;
    {__in} cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCTSTR;
    {__deref_opt_out_ecount(pcchRemaining^)}ppszDestEnd : PSTRSAFE_LPTSTR;
    {__out_opt} pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal) : HRESULT; stdcall;

{++

STDAPI
StringCbCatEx(
    IN OUT LPTSTR  pszDest         OPTIONAL,
    IN     size_t  cbDest,
    IN     LPCTSTR pszSrc          OPTIONAL,
    OUT    LPTSTR* ppszDestEnd     OPTIONAL,
    OUT    size_t* pcbRemaining    OPTIONAL,
    IN     DWORD   dwFlags
    );

Routine Description:

    This routine is a safer version of the C built-in function 'strcat' with
    some additional parameters.  In addition to functionality provided by
    StringCbCat, this routine also returns a pointer to the end of the
    destination string and the number of bytes left in the destination string
    including the null terminator. The flags parameter allows additional controls.

Arguments:

    pszDest         -   destination string which must be null terminated

    cbDest          -   size of destination buffer in bytes.
                        length must be ((_tcslen(pszDest) + _tcslen(pszSrc) + 1) * sizeof(TCHAR)
                        to hold all of the combine string plus the null
                        terminator.

    pszSrc          -   source string which must be null terminated

    ppszDestEnd     -   if ppszDestEnd is non-null, the function will return a
                        pointer to the end of the destination string.  If the
                        function appended any data, the result will point to the
                        null termination character

    pcbRemaining    -   if pcbRemaining is non-null, the function will return
                        the number of bytes left in the destination string,
                        including the null terminator

    dwFlags         -   controls some details of the string copy:

        STRSAFE_FILL_BEHIND_NULL
                    if the function succeeds, the low byte of dwFlags will be
                    used to fill the uninitialize part of destination buffer
                    behind the null terminator

        STRSAFE_IGNORE_NULLS
                    treat NULL string pointers like empty strings (TEXT("")).
                    this flag is useful for emulating functions like lstrcat

        STRSAFE_FILL_ON_FAILURE
                    if the function fails, the low byte of dwFlags will be
                    used to fill all of the destination buffer, and it will
                    be null terminated. This will overwrite any pre-existing
                    or truncated string

        STRSAFE_NULL_ON_FAILURE
                    if the function fails, the destination buffer will be set
                    to the empty string. This will overwrite any pre-existing or
                    truncated string

        STRSAFE_NO_TRUNCATION
                    if the function returns STRSAFE_E_INSUFFICIENT_BUFFER, pszDest
                    will not contain a truncated string, it will remain unchanged.

Notes:
    Behavior is undefined if source and destination strings overlap.

    pszDest and pszSrc should not be NULL unless the STRSAFE_IGNORE_NULLS flag
    is specified.  If STRSAFE_IGNORE_NULLS is passed, both pszDest and pszSrc
    may be NULL.  An error may still be returned even though NULLS are ignored
    due to insufficient space.

Return Value:

    S_OK           -   if there was source data and it was all concatenated
                       and the resultant dest string was null terminated

    failure        -   you can use the macro HRESULT_CODE() to get a win32
                       error code for all hresult failure cases

      STRSAFE_E_INSUFFICIENT_BUFFER /
      HRESULT_CODE(hr) == ERROR_INSUFFICIENT_BUFFER
                   -   this return value is an indication that the operation
                       failed due to insufficient space. When this error
                       occurs, the destination buffer is modified to contain
                       a truncated version of the ideal result and is null
                       terminated. This is useful for situations where
                       truncation is ok.

    It is strongly recommended to use the SUCCEEDED() / FAILED() macros to test the
    return value of this function

--}
function StringCbCatExA(
    {__inout_bcount(cbDest)}pszDest : STRSAFE_LPSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCSTR;
    {__deref_opt_out_bcount(pcbRemaining^)}ppszDestEnd : PSTRSAFE_LPSTR;
    {__out_opt} pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal) : HRESULT; stdcall; forward; external;

function StringCbCatExW(
    {__inout_bcount(cbDest)}pszDest : STRSAFE_LPWSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCWSTR;
    {__deref_opt_out_bcount(pcbRemaining^)}ppszDestEnd : PSTRSAFE_LPWSTR;
    {__out_opt} pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal) : HRESULT; stdcall; forward; external;

function StringCbCatEx(
    {__inout_bcount(cbDest)}pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCTSTR;
    {__deref_opt_out_bcount(pcbRemaining^)}ppszDestEnd : PSTRSAFE_LPTSTR;
    {__out_opt} pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal) : HRESULT; stdcall;

{++

STDAPI
StringCchCatN(
    IN OUT LPTSTR  pszDest,
    IN     size_t  cchDest,
    IN     LPCTSTR pszSrc,
    IN     size_t  cchToAppend
    );

Routine Description:

    This routine is a safer version of the C built-in function 'strncat'.
    The size of the destination buffer (in characters) is a parameter as well as
    the maximum number of characters to append, excluding the null terminator.
    This function will not write past the end of the destination buffer and it will
    ALWAYS null terminate pszDest (unless it is zero length).

    This function returns a hresult, and not a pointer.  It returns
    S_OK if all of pszSrc or the first cchToAppend characters were appended
    to the destination string and it was null terminated, otherwise it will
    return a failure code. In failure cases as much of pszSrc will be appended
    to pszDest as possible, and pszDest will be null terminated.

Arguments:

    pszDest         -   destination string which must be null terminated

    cchDest         -   size of destination buffer in characters.
                        length must be (_tcslen(pszDest) + min(cchToAppend, _tcslen(pszSrc)) + 1)
                        to hold all of the combine string plus the null
                        terminator.

    pszSrc          -   source string

    cchToAppend     -   maximum number of characters to append

Notes:
    Behavior is undefined if source and destination strings overlap.

    pszDest and pszSrc should not be NULL. See StringCchCatNEx if you require
    the handling of NULL values.

Return Value:

    S_OK           -   if all of pszSrc or the first cchToAppend characters
                       were concatenated to pszDest and the resultant dest
                       string was null terminated

    failure        -   you can use the macro HRESULT_CODE() to get a win32
                       error code for all hresult failure cases

      STRSAFE_E_INSUFFICIENT_BUFFER /
      HRESULT_CODE(hr) == ERROR_INSUFFICIENT_BUFFER
                   -   this return value is an indication that the operation
                       failed due to insufficient space. When this error
                       occurs, the destination buffer is modified to contain
                       a truncated version of the ideal result and is null
                       terminated. This is useful for situations where
                       truncation is ok.

    It is strongly recommended to use the SUCCEEDED() / FAILED() macros to test the
    return value of this function

--}
function StringCchCatNA(
    {__inout_ecount(cchDest)}pszDest : STRSAFE_LPSTR;
    {__in}cchDest : size_t;
    {__in_ecount(cchToAppend)}const pszSrc : STRSAFE_LPCSTR;
    {__in}cchToAppend : Cardinal) : HRESULT; stdcall; forward; external;

function StringCchCatNW(
    {__inout_ecount(cchDest)}pszDest : STRSAFE_LPWSTR;
    {__in}cchDest : size_t;
    {__in_ecount(cchToAppend)}const pszSrc : STRSAFE_LPCWSTR;
    {__in}cchToAppend : Cardinal) : HRESULT; stdcall; forward; external;

function StringCchCatN(
    {__inout_ecount(cchDest)}pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in_ecount(cchToAppend)}const pszSrc : STRSAFE_LPCTSTR;
    {__in}cchToAppend : Cardinal) : HRESULT; stdcall;

{++

STDAPI
StringCbCatN(
    IN OUT LPTSTR  pszDest,
    IN     size_t  cbDest,
    IN     LPCTSTR pszSrc,
    IN     size_t  cbToAppend
    );

Routine Description:

    This routine is a safer version of the C built-in function 'strncat'.
    The size of the destination buffer (in bytes) is a parameter as well as
    the maximum number of bytes to append, excluding the null terminator.
    This function will not write past the end of the destination buffer and it will
    ALWAYS null terminate pszDest (unless it is zero length).

    This function returns a hresult, and not a pointer.  It returns
    S_OK if all of pszSrc or the first cbToAppend bytes were appended
    to the destination string and it was null terminated, otherwise it will
    return a failure code. In failure cases as much of pszSrc will be appended
    to pszDest as possible, and pszDest will be null terminated.

Arguments:

    pszDest         -   destination string which must be null terminated

    cbDest          -   size of destination buffer in bytes.
                        length must be ((_tcslen(pszDest) + min(cbToAppend / sizeof(TCHAR), _tcslen(pszSrc)) + 1) * sizeof(TCHAR)
                        to hold all of the combine string plus the null
                        terminator.

    pszSrc          -   source string

    cbToAppend      -   maximum number of bytes to append

Notes:
    Behavior is undefined if source and destination strings overlap.

    pszDest and pszSrc should not be NULL. See StringCbCatNEx if you require
    the handling of NULL values.

Return Value:

    S_OK           -   if all of pszSrc or the first cbToAppend bytes were
                       concatenated to pszDest and the resultant dest string
                       was null terminated

    failure        -   you can use the macro HRESULT_CODE() to get a win32
                       error code for all hresult failure cases

      STRSAFE_E_INSUFFICIENT_BUFFER /
      HRESULT_CODE(hr) == ERROR_INSUFFICIENT_BUFFER
                   -   this return value is an indication that the operation
                       failed due to insufficient space. When this error
                       occurs, the destination buffer is modified to contain
                       a truncated version of the ideal result and is null
                       terminated. This is useful for situations where
                       truncation is ok.

    It is strongly recommended to use the SUCCEEDED() / FAILED() macros to test the
    return value of this function

--}
function StringCbCatNA(
    {__inout_bcount(cbDest)}pszDest : STRSAFE_LPSTR;
    {__in}cchDest : size_t;
    {__in_bcount(cbToAppend)}const pszSrc : STRSAFE_LPCSTR;
    {__in}cchToAppend : Cardinal) : HRESULT; stdcall; forward; external;

function StringCbCatNW(
    {__inout_bcount(cbDest)}pszDest : STRSAFE_LPWSTR;
    {__in}cchDest : size_t;
    {__in_bcount(cbToAppend)}const pszSrc : STRSAFE_LPCWSTR;
    {__in}cchToAppend : Cardinal) : HRESULT; stdcall; forward; external;

function StringCbCatN(
    {__inout_bcount(cbDest)}pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in_bcount(cbToAppend)}const pszSrc : STRSAFE_LPCTSTR;
    {__in}cchToAppend : Cardinal) : HRESULT; stdcall;

{++

STDAPI
StringCchCatNEx(
    IN OUT LPTSTR  pszDest         OPTIONAL,
    IN     size_t  cchDest,
    IN     LPCTSTR pszSrc          OPTIONAL,
    IN     size_t  cchToAppend,
    OUT    LPTSTR* ppszDestEnd     OPTIONAL,
    OUT    size_t* pcchRemaining   OPTIONAL,
    IN     DWORD   dwFlags
    );

Routine Description:

    This routine is a safer version of the C built-in function 'strncat', with
    some additional parameters.  In addition to functionality provided by
    StringCchCatN, this routine also returns a pointer to the end of the
    destination string and the number of characters left in the destination string
    including the null terminator. The flags parameter allows additional controls.

Arguments:

    pszDest         -   destination string which must be null terminated

    cchDest         -   size of destination buffer in characters.
                        length must be (_tcslen(pszDest) + min(cchToAppend, _tcslen(pszSrc)) + 1)
                        to hold all of the combine string plus the null
                        terminator.

    pszSrc          -   source string

    cchToAppend     -   maximum number of characters to append

    ppszDestEnd     -   if ppszDestEnd is non-null, the function will return a
                        pointer to the end of the destination string.  If the
                        function appended any data, the result will point to the
                        null termination character

    pcchRemaining   -   if pcchRemaining is non-null, the function will return the
                        number of characters left in the destination string,
                        including the null terminator

    dwFlags         -   controls some details of the string copy:

        STRSAFE_FILL_BEHIND_NULL
                    if the function succeeds, the low byte of dwFlags will be
                    used to fill the uninitialize part of destination buffer
                    behind the null terminator

        STRSAFE_IGNORE_NULLS
                    treat NULL string pointers like empty strings (TEXT(""))

        STRSAFE_FILL_ON_FAILURE
                    if the function fails, the low byte of dwFlags will be
                    used to fill all of the destination buffer, and it will
                    be null terminated. This will overwrite any pre-existing
                    or truncated string

        STRSAFE_NULL_ON_FAILURE
                    if the function fails, the destination buffer will be set
                    to the empty string. This will overwrite any pre-existing or
                    truncated string

        STRSAFE_NO_TRUNCATION
                    if the function returns STRSAFE_E_INSUFFICIENT_BUFFER, pszDest
                    will not contain a truncated string, it will remain unchanged.

Notes:
    Behavior is undefined if source and destination strings overlap.

    pszDest and pszSrc should not be NULL unless the STRSAFE_IGNORE_NULLS flag
    is specified.  If STRSAFE_IGNORE_NULLS is passed, both pszDest and pszSrc
    may be NULL.  An error may still be returned even though NULLS are ignored
    due to insufficient space.

Return Value:

    S_OK           -   if all of pszSrc or the first cchToAppend characters
                       were concatenated to pszDest and the resultant dest
                       string was null terminated

    failure        -   you can use the macro HRESULT_CODE() to get a win32
                       error code for all hresult failure cases

      STRSAFE_E_INSUFFICIENT_BUFFER /
      HRESULT_CODE(hr) == ERROR_INSUFFICIENT_BUFFER
                   -   this return value is an indication that the operation
                       failed due to insufficient space. When this error
                       occurs, the destination buffer is modified to contain
                       a truncated version of the ideal result and is null
                       terminated. This is useful for situations where
                       truncation is ok.

    It is strongly recommended to use the SUCCEEDED() / FAILED() macros to test the
    return value of this function

--}
function StringCchCatNExA(
    {__inout_ecount(cchDest)}pszDest : STRSAFE_LPSTR;
    {__in} cchDest : size_t;
    {__in_ecount(cchToAppend)}const pszSrc : STRSAFE_LPCSTR;
    {__in}cchToAppend : Cardinal;
    {__deref_opt_out_ecount(pcchRemaining^)}ppszDestEnd : PSTRSAFE_LPSTR;
    {__out_opt}pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal) : HRESULT; stdcall; forward; external;

function StringCchCatNExW(
    {__inout_ecount(cchDest)}pszDest : STRSAFE_LPWSTR;
    {__in}cchDest : size_t;
    {__in_ecount(cchToAppend)}const pszSrc : STRSAFE_LPCWSTR;
    {__in}cchToAppend : Cardinal;
    {__deref_opt_out_ecount(pcchRemaining^)}ppszDestEnd : PSTRSAFE_LPWSTR;
    {__out_opt}pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal) : HRESULT; stdcall; forward; external;

function StringCchCatNEx(
    {__inout_ecount(cchDest)}pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in_ecount(cchToAppend)}const pszSrc : STRSAFE_LPCTSTR;
    {__in}cchToAppend : Cardinal;
    {__deref_opt_out_ecount(pcchRemaining^)}ppszDestEnd : PSTRSAFE_LPTSTR;
    {__out_opt}pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal) : HRESULT; stdcall;

{++

STDAPI
StringCbCatNEx(
    IN OUT LPTSTR  pszDest         OPTIONAL,
    IN     size_t  cbDest,
    IN     LPCTSTR pszSrc          OPTIONAL,
    IN     size_t  cbToAppend,
    OUT    LPTSTR* ppszDestEnd     OPTIONAL,
    OUT    size_t* pcchRemaining   OPTIONAL,
    IN     DWORD   dwFlags
    );

Routine Description:

    This routine is a safer version of the C built-in function 'strncat', with
    some additional parameters.  In addition to functionality provided by
    StringCbCatN, this routine also returns a pointer to the end of the
    destination string and the number of bytes left in the destination string
    including the null terminator. The flags parameter allows additional controls.

Arguments:

    pszDest         -   destination string which must be null terminated

    cbDest          -   size of destination buffer in bytes.
                        length must be ((_tcslen(pszDest) + min(cbToAppend / sizeof(TCHAR), _tcslen(pszSrc)) + 1) * sizeof(TCHAR)
                        to hold all of the combine string plus the null
                        terminator.

    pszSrc          -   source string

    cbToAppend      -   maximum number of bytes to append

    ppszDestEnd     -   if ppszDestEnd is non-null, the function will return a
                        pointer to the end of the destination string.  If the
                        function appended any data, the result will point to the
                        null termination character

    pcbRemaining    -   if pcbRemaining is non-null, the function will return the
                        number of bytes left in the destination string,
                        including the null terminator

    dwFlags         -   controls some details of the string copy:

        STRSAFE_FILL_BEHIND_NULL
                    if the function succeeds, the low byte of dwFlags will be
                    used to fill the uninitialize part of destination buffer
                    behind the null terminator

        STRSAFE_IGNORE_NULLS
                    treat NULL string pointers like empty strings (TEXT(""))

        STRSAFE_FILL_ON_FAILURE
                    if the function fails, the low byte of dwFlags will be
                    used to fill all of the destination buffer, and it will
                    be null terminated. This will overwrite any pre-existing
                    or truncated string

        STRSAFE_NULL_ON_FAILURE
                    if the function fails, the destination buffer will be set
                    to the empty string. This will overwrite any pre-existing or
                    truncated string

        STRSAFE_NO_TRUNCATION
                    if the function returns STRSAFE_E_INSUFFICIENT_BUFFER, pszDest
                    will not contain a truncated string, it will remain unchanged.

Notes:
    Behavior is undefined if source and destination strings overlap.

    pszDest and pszSrc should not be NULL unless the STRSAFE_IGNORE_NULLS flag
    is specified.  If STRSAFE_IGNORE_NULLS is passed, both pszDest and pszSrc
    may be NULL.  An error may still be returned even though NULLS are ignored
    due to insufficient space.

Return Value:

    S_OK           -   if all of pszSrc or the first cbToAppend bytes were
                       concatenated to pszDest and the resultant dest string
                       was null terminated

    failure        -   you can use the macro HRESULT_CODE() to get a win32
                       error code for all hresult failure cases

      STRSAFE_E_INSUFFICIENT_BUFFER /
      HRESULT_CODE(hr) == ERROR_INSUFFICIENT_BUFFER
                   -   this return value is an indication that the operation
                       failed due to insufficient space. When this error
                       occurs, the destination buffer is modified to contain
                       a truncated version of the ideal result and is null
                       terminated. This is useful for situations where
                       truncation is ok.

    It is strongly recommended to use the SUCCEEDED() / FAILED() macros to test the
    return value of this function

--}
function StringCbCatNExA(
    {__inout_bcount(cbDest)}pszDest : STRSAFE_LPSTR;
    {__in}cchDest : size_t;
    {__in_bcount(cbToAppend)}const pszSrc : STRSAFE_LPCSTR;
    {__in}cchToAppend : Cardinal;
    {__deref_opt_out_bcount(pcbRemaining^)}ppszDestEnd : PSTRSAFE_LPSTR;
    {__out_opt}pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal) : HRESULT; stdcall; forward; external;

function StringCbCatNExW(
    {__inout_bcount(cbDest)}pszDest : STRSAFE_LPWSTR;
    {__in}cchDest : size_t;
    {__in_bcount(cbToAppend)}const pszSrc : STRSAFE_LPCWSTR;
    {__in}cchToAppend : Cardinal;
    {__deref_opt_out_bcount(pcbRemaining^)}ppszDestEnd : PSTRSAFE_LPWSTR;
    {__out_opt}pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal) : HRESULT; stdcall; forward; external;

function StringCbCatNEx(
    {__inout_bcount(cbDest)}pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in_bcount(cbToAppend)}const pszSrc : STRSAFE_LPCTSTR;
    {__in}cchToAppend : Cardinal;
    {__deref_opt_out_bcount(pcbRemaining^)}ppszDestEnd : PSTRSAFE_LPTSTR;
    {__out_opt}pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal) : HRESULT; stdcall;
 
{++

STDAPI
StringCchLength(
    IN  LPCTSTR psz,
    IN  size_t  cchMax,
    OUT size_t* pcchLength  OPTIONAL
    );

Routine Description:

    This routine is a safer version of the C built-in function 'strlen'.
    It is used to make sure a string is not larger than a given length, and
    it optionally returns the current length in characters not including
    the null terminator.

    This function returns a hresult, and not a pointer.  It returns
    S_OK if the string is non-null and the length including the null
    terminator is less than or equal to cchMax characters.

Arguments:

    psz         -   string to check the length of

    cchMax      -   maximum number of characters including the null terminator
                    that psz is allowed to contain

    pcch        -   if the function succeeds and pcch is non-null, the current length
                    in characters of psz excluding the null terminator will be returned.
                    This out parameter is equivalent to the return value of strlen(psz)

Notes:
    psz can be null but the function will fail

    cchMax should be greater than zero or the function will fail

Return Value:

    S_OK           -   psz is non-null and the length including the null
                       terminator is less than or equal to cchMax characters

    failure        -   you can use the macro HRESULT_CODE() to get a win32
                       error code for all hresult failure cases

    It is strongly recommended to use the SUCCEEDED() / FAILED() macros to test the
    return value of this function.

--}
function StringCchLengthA(
    {__in}const psz : STRSAFE_LPCSTR;
    {__in}cchMax : size_t;
    {__out_opt}pcchLength : PSize_t) : HRESULT; stdcall; forward; external;

function StringCchLengthW(
    {__in}const psz : STRSAFE_LPCWSTR;
    {__in}cchMax : size_t;
    {__out_opt}pcchLength : PSize_t) : HRESULT; stdcall; forward; external;

function StringCchLength(
    {__in}const psz : STRSAFE_LPCTSTR;
    {__in}cchMax : size_t;
    {__out_opt}pcchLength : PSize_t) : HRESULT; stdcall;

{++

STDAPI
StringCbLength(
    IN  LPCTSTR psz,
    IN  size_t  cbMax,
    OUT size_t* pcbLength   OPTIONAL
    );

Routine Description:

    This routine is a safer version of the C built-in function 'strlen'.
    It is used to make sure a string is not larger than a given length, and
    it optionally returns the current length in bytes not including
    the null terminator.

    This function returns a hresult, and not a pointer.  It returns
    S_OK if the string is non-null and the length including the null
    terminator is less than or equal to cbMax bytes.

Arguments:

    psz         -   string to check the length of

    cbMax       -   maximum number of bytes including the null terminator
                    that psz is allowed to contain

    pcb         -   if the function succeeds and pcb is non-null, the current length
                    in bytes of psz excluding the null terminator will be returned.
                    This out parameter is equivalent to the return value of strlen(psz) * sizeof(TCHAR)

Notes:
    psz can be null but the function will fail

    cbMax should be greater than or equal to sizeof(TCHAR) or the function will fail

Return Value:

    S_OK           -   psz is non-null and the length including the null
                       terminator is less than or equal to cbMax bytes

    failure        -   you can use the macro HRESULT_CODE() to get a win32
                       error code for all hresult failure cases

    It is strongly recommended to use the SUCCEEDED() / FAILED() macros to test the
    return value of this function.

--}
function StringCbLengthA(
    {__in}const psz : STRSAFE_LPCSTR;
    {__in}cbMax : size_t;
    {__out_opt}pcbLength : PSize_t) : HRESULT; stdcall; forward;external;

function StringCbLengthW(
    {__in}const psz : STRSAFE_LPCWSTR;
    {__in}cbMax : size_t;
    {__out_opt}pcbLength : PSize_t) : HRESULT; stdcall; forward;external;

function StringCbLength(
    {__in}const psz : STRSAFE_LPCTSTR;
    {__in}cbMax : size_t;
    {__out_opt}pcbLength : PSize_t) : HRESULT; stdcall;


{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
uses Sysutils;

{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INTERFACESECTION}



function _memset ( Ptr : Pointer; Value : Integer; Num : Size_T) : Pointer; cdecl;
begin
  FillChar(Ptr, Num, Value);
  result := ptr;
end;

type
  t_vsnprintf = function (
   buffer : PAnsiChar;
   count : size_t;
   const format : PAnsiChar;
   const Args : Pointer) : Integer;  cdecl;

var _vsnprintf : t_vsnprintf;

function __vsnprintf(pszDest : PAnsiChar; cchDest : size_t; pszFormat : PAnsiChar;
  argList : Pointer) : size_t; cdecl;
begin
  try
    GetProcedureAddress(Pointer(@_vsnprintf), 'msvcr71.dll', '_vsnprintf');
  except
    GetProcedureAddress(Pointer(@_vsnprintf), 'msvcrt.dll', '_vsnprintf');
  end;
  result := _vsnprintf(pszDest, cchDest, pszFormat, argList);
end;


type
  t_vsnwprintf = function (
   buffer : PWideChar;
   count : size_t;
   const format : PWideChar;
   const Args : Pointer) : Integer;  cdecl;

var _vsnwprintf : t_vsnwprintf;

function __vsnwprintf(pszDest : PWideChar; cchDest : size_t; pszFormat : PWideChar;
  argList : Pointer) : size_t; cdecl;
begin
  try
    GetProcedureAddress(Pointer(@_vsnwprintf), 'msvcr71.dll', '_vsnwprintf');
  except
    GetProcedureAddress(Pointer(@_vsnwprintf), 'msvcrt.dll', '_vsnwprintf');
  end;
  result := _vsnwprintf(pszDest, cchDest, pszFormat, argList);
end;


function STRSAFE_FILL_BYTE(const x : Cardinal) : Cardinal;
begin
  result := Cardinal(x and $000000FF) or STRSAFE_FILL_BEHIND_NULL;
end;

function STRSAFE_FAILURE_BYTE(const x : Cardinal) : Cardinal;
begin
  result := Cardinal(x and $000000FF) or STRSAFE_FILL_ON_FAILURE;
end;

function STRSAFE_GET_FILL_PATTERN(const dwFlags : Cardinal) : Cardinal;
begin
  result := Integer(dwFlags and $000000FF);
end;


var _StringCchCopy : Pointer;
function StringCchCopy(
    {__out_ecount(cchDest)}pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCTSTR) : HRESULT; stdcall;
begin
  result := ERROR_SEVERITY_SUCCESS;
{$IFDEF UNICODE}
    _StringCchCopy := @StringCchCopyW;
{$ELSE}
    _StringCchCopy := @StringCchCopyA;
{$ENDIF}
  asm
    MOV     ESP, EBP
    POP     EBP
    JMP     [_StringCchCopy]
  end;
end;

var _StringCbCopy : Pointer;

function StringCbCopy(
    {__out_bcount(cbDest)}pszDest : STRSAFE_LPTSTR;
    {__in}cbDest  : size_t;
    {__in}const pszSrc : STRSAFE_LPCTSTR) : HRESULT; stdcall;
begin
  result := ERROR_SEVERITY_SUCCESS;
  _StringCbCopy :=
{$IFDEF UNICODE}@StringCbCopyW{$ELSE}@StringCbCopyA{$ENDIF};
  asm
    MOV     ESP, EBP
    POP     EBP
    JMP     [_StringCbCopy]
  end; 
end;


var _StringCchCopyEx : Pointer;
function StringCchCopyEx(
    {__out_ecount(cchDest)}pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCTSTR;
    {__deref_opt_out_ecount(pcchRemaining^)}ppszDestEnd : PSTRSAFE_LPTSTR;
    {__out_opt}pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal) : HRESULT; stdcall;
begin
  result := ERROR_SEVERITY_SUCCESS;
  _StringCchCopyEx :=
{$IFDEF UNICODE}@StringCchCopyExW{$ELSE}@StringCchCopyExA{$ENDIF};
  asm
    MOV     ESP, EBP
    POP     EBP
    JMP     [_StringCchCopyEx]
  end; 
end;
	

var _StringCbCopyEx : Pointer;
function StringCbCopyEx(
    {__out_bcount(cbDest)} pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCTSTR;
    {__deref_opt_out_bcount(pcbRemaining^)}ppszDestEnd : PSTRSAFE_LPTSTR;
    {__out_opt}pcbRemaining : PSize_t;
    {__in}dwFlags : Cardinal) : HRESULT; stdcall;
begin
  result := ERROR_SEVERITY_SUCCESS;
  _StringCbCopyEx :=
{$IFDEF UNICODE}@StringCbCopyExA{$ELSE}@StringCbCopyExA{$ENDIF};
  asm
    MOV     ESP, EBP
    POP     EBP
    JMP     [_StringCbCopyEx]
  end;
end;
	
	
var _StringCchCopyN : Pointer;	
function StringCchCopyN(
    {__out_ecount(cchDest)} pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCTSTR;
    {__in}cchToCopy : size_t) : HRESULT; stdcall;
begin
  result := ERROR_SEVERITY_SUCCESS;
  _StringCchCopyN :=
{$IFDEF UNICODE}@StringCchCopyNW{$ELSE}@StringCchCopyNA{$ENDIF};
  asm
    MOV     ESP, EBP
    POP     EBP
    JMP     [_StringCchCopyN]
  end; 
end;
	
var _StringCbCopyN : Pointer;		
function StringCbCopyN(
    {__out_bcount(cbDest)} pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCTSTR;
    {__in}cchToCopy : size_t) : HRESULT; stdcall;
begin
  result := ERROR_SEVERITY_SUCCESS;
  _StringCbCopyN :=
{$IFDEF UNICODE}@StringCbCopyNW{$ELSE}@StringCbCopyNA{$ENDIF};
  asm
    MOV     ESP, EBP
    POP     EBP
    JMP     [_StringCbCopyN]
  end;
end;


var _StringCchCopyNEx : Pointer;
function StringCchCopyNEx(
    {__in_ecount(cchDest)} pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in_ecount(cchToCopy)}const pszSrc : STRSAFE_LPCTSTR;
    {__in}cchToCopy : size_t;
    {__deref_opt_out_ecount(pcchRemaining^)}ppszDestEnd : PSTRSAFE_LPTSTR;
    {__out_opt size_t}pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal)  : HRESULT; stdcall;
begin
  result := ERROR_SEVERITY_SUCCESS;
  _StringCchCopyNEx :=
{$IFDEF UNICODE}@StringCchCopyNExW{$ELSE}@StringCchCopyNExA{$ENDIF};
  asm
    MOV     ESP, EBP
    POP     EBP
    JMP     [_StringCchCopyNEx]
  end;
end;


var _StringCbCopyNEx : Pointer;
function StringCbCopyNEx(
    {__out_bcount(cbDest) } pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in_bcount(cbToCopy) }const pszSrc : STRSAFE_LPCTSTR;
    {__in}cchToCopy : size_t;
    {__deref_opt_out_bcount(pcbRemaining^)}ppszDestEnd : PSTRSAFE_LPTSTR;
    {__out_opt}pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal)  : HRESULT; stdcall;
begin
  result := ERROR_SEVERITY_SUCCESS;
  _StringCbCopyNEx :=
{$IFDEF UNICODE}@StringCbCopyNExW{$ELSE}@StringCbCopyNExA{$ENDIF};
  asm
    MOV     ESP, EBP
    POP     EBP
    JMP     [_StringCbCopyNEx]
  end;
end;


var _StringCchCat : Pointer;
function StringCchCat(
    {__inout_ecount(cchDest)} pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCTSTR) : HRESULT; stdcall;
begin
  result := ERROR_SEVERITY_SUCCESS;
  _StringCchCat :=
{$IFDEF UNICODE}@StringCchCatW{$ELSE}@StringCchCatA{$ENDIF};
  asm
    MOV     ESP, EBP
    POP     EBP
    JMP     [_StringCchCat]
  end; 
end;	


var _StringCbCat : Pointer;		
function StringCbCat(
    {__inout_bcount(cbDest)} pszDest : STRSAFE_LPTSTR;
    {__in}cbDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCTSTR) : HRESULT; stdcall;	
begin
  result := ERROR_SEVERITY_SUCCESS;
  _StringCbCat :=
{$IFDEF UNICODE}@StringCbCatW{$ELSE}@StringCbCatA{$ENDIF};
  asm
    MOV     ESP, EBP
    POP     EBP
    JMP     [_StringCbCat]
  end; 
end;	
	

var _StringCchCatEx : Pointer;
function StringCchCatEx(
    {__inout_ecount(cchDest)} pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCTSTR;
    {__deref_opt_out_ecount(pcchRemaining^)}ppszDestEnd : PSTRSAFE_LPTSTR;
    {__out_opt} pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal) : HRESULT; stdcall;
begin
  result := ERROR_SEVERITY_SUCCESS;
  _StringCchCatEx :=
{$IFDEF UNICODE}@StringCchCatExW{$ELSE}@StringCchCatExA{$ENDIF};
  asm
    MOV     ESP, EBP
    POP     EBP
    JMP     [_StringCchCatEx]
  end;
end;
	
	
var _StringCbCatEx : Pointer;		
function StringCbCatEx(
    {__inout_bcount(cbDest)} pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in}const pszSrc : STRSAFE_LPCTSTR;
    {__deref_opt_out_bcount(pcbRemaining^)}ppszDestEnd : PSTRSAFE_LPTSTR;
    {__out_opt} pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal) : HRESULT; stdcall;
begin
  result := ERROR_SEVERITY_SUCCESS;
  _StringCbCatEx :=
{$IFDEF UNICODE}@StringCbCatExW{$ELSE}@StringCbCatExA{$ENDIF};
  asm
    MOV     ESP, EBP
    POP     EBP
    JMP     [_StringCbCatEx]
  end; 
end;	
	

var _StringCchCatN : Pointer;		
function StringCchCatN(
    {__inout_ecount(cchDest)} pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in_ecount(cchToAppend)}const pszSrc : STRSAFE_LPCTSTR;
    {__in}cchToAppend : Cardinal) : HRESULT; stdcall;
begin
  result := ERROR_SEVERITY_SUCCESS;
  _StringCchCatN :=
{$IFDEF UNICODE}@StringCchCatNW{$ELSE}@StringCchCatNA{$ENDIF};
  asm
    MOV     ESP, EBP
    POP     EBP
    JMP     [_StringCchCatN]
  end; 
end;	
	
	
var _StringCbCatN : Pointer;		
function StringCbCatN(
    {__inout_bcount(cbDest)} pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in_bcount(cbToAppend)}const pszSrc : STRSAFE_LPCTSTR;
    {__in}cchToAppend : Cardinal) : HRESULT; stdcall;
begin
  result := ERROR_SEVERITY_SUCCESS;
  _StringCbCatN :=
{$IFDEF UNICODE}@StringCbCatNW{$ELSE}@StringCbCatNA{$ENDIF};
  asm
    MOV     ESP, EBP
    POP     EBP
    JMP     [_StringCbCatN]
  end; 
end;	
	
	
var _StringCchCatNEx : Pointer;		
function StringCchCatNEx(
    {__inout_ecount(cchDest)} pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in_ecount(cchToAppend)}const pszSrc : STRSAFE_LPCTSTR;
    {__in}cchToAppend : Cardinal;
    {__deref_opt_out_ecount(pcchRemaining^)}ppszDestEnd : PSTRSAFE_LPTSTR;
    {__out_opt}pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal) : HRESULT; stdcall;
begin
  result := ERROR_SEVERITY_SUCCESS;
  _StringCchCatNEx :=
{$IFDEF UNICODE}@StringCchCatNExW{$ELSE}@StringCchCatNExA{$ENDIF};
  asm
    MOV     ESP, EBP
    POP     EBP
    JMP     [_StringCchCatNEx]
  end; 
end;	
	
	
	
	
var _StringCbCatNEx : Pointer;
function StringCbCatNEx(
    {__inout_bcount(cbDest)} pszDest : STRSAFE_LPTSTR;
    {__in}cchDest : size_t;
    {__in_bcount(cbToAppend)}const pszSrc : STRSAFE_LPCTSTR;
    {__in}cchToAppend : Cardinal;
    {__deref_opt_out_bcount(pcbRemaining^)}ppszDestEnd : PSTRSAFE_LPTSTR;
    {__out_opt}pcchRemaining : PSize_t;
    {__in}dwFlags : Cardinal) : HRESULT; stdcall;
begin
  result := ERROR_SEVERITY_SUCCESS;
  _StringCbCatNEx :=
{$IFDEF UNICODE}@StringCbCatNExW{$ELSE}@StringCbCatNExA{$ENDIF};
  asm
    MOV     ESP, EBP
    POP     EBP
    JMP     [_StringCbCatNEx]
  end; 
end;	

var _StringCchLength : Pointer;
function StringCchLength(
    {__in}const psz : STRSAFE_LPCTSTR;
    {__in}cchMax : size_t;
    {__out_opt} pcchLength : PSize_t) : HRESULT; stdcall;
begin
  result := ERROR_SEVERITY_SUCCESS;
  _StringCchLength :=
{$IFDEF UNICODE}@StringCchLengthW{$ELSE}@StringCchLengthA{$ENDIF};
  asm
    MOV     ESP, EBP
    POP     EBP
    JMP     [_StringCchLength]
  end;
end;	




var _StringCbLength : Pointer;
function StringCbLength(
    {__in}const psz : STRSAFE_LPCTSTR;
    {__in}cbMax : size_t;
    {__out_opt} pcbLength : PSize_t) : HRESULT; stdcall;
begin
  result := ERROR_SEVERITY_SUCCESS;
  _StringCbLength :=
{$IFDEF UNICODE}@StringCbLengthW{$ELSE}@StringCbLengthA{$ENDIF};
{$IFDEF UNICODE}
  asm
    MOV     ESP, EBP
    POP     EBP
    JMP     [_StringCbLength]
  end;
{$ELSE}
  asm
    MOV     ESP, EBP
    POP     EBP
    JMP     [_StringCbLength]
  end;
{$ENDIF}
end;



{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}


end.

{$ENDIF JWA_OMIT_SECTIONS}
