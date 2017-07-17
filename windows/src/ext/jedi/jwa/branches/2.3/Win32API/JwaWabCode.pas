{******************************************************************************}
{                                                                              }
{ Windows Address Book (WAB) API interface Unit for Object Pascal              }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2008 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ Portions created by Microsoft are                                            }
{ Copyright (C) 1995-2000 Microsoft Corporation.                               }
{ All Rights Reserved.                                                         }
{                                                                              }
{ The original file is: wabcode.h, released 31 Jan 2000.                       }
{ The original Pascal code is: WabCode.pas, released 15 Mar 2000.  			   }
{ The initial developer of the Pascal code is Petr Vones                       }
{ (petr.v@mujmail.cz).                                                         }
{                                                                              }
{ Portions created by Petr Vones are                               	           }
{ Copyright (C) 2000 Petr Vones                                    			   }
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
unit JwaWabCode;

interface

uses
  Windows, ActiveX;

{$I ..\Includes\JediAPILib.inc}
 

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

(*$HPPEMIT '#include <wabcode.h>'*)
(*$HPPEMIT '#include <objerror.h>'*)

{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}

{*
 *  WAB Status codes follow the style of OLE 2.0 sCodes as defined in the
 *  OLE 2.0 Programmer's Reference and header file scode.h (Windows 3.x)
 *  or objerror.h (Windows NT 3.5 and Windows 95).
 *
 */

/*  On Windows 3.x, status codes have 32-bit values as follows:
 *
 *   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
 *   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
 *  +-+---------------------+-------+-------------------------------+
 *  |S|       Context       | Facil |               Code            |
 *  +-+---------------------+-------+-------------------------------+
 *
 *  where
 *
 *      S - is the severity code
 *
 *          0 - SEVERITY_SUCCESS
 *          1 - SEVERITY_ERROR
 *
 *      Context - context info
 *
 *      Facility - is the facility code
 *
 *          0x0 - FACILITY_NULL     generally useful errors ([SE]_*)
 *          0x1 - FACILITY_RPC      remote procedure call errors (RPC_E_*)
 *          0x2 - FACILITY_DISPATCH late binding dispatch errors
 *          0x3 - FACILITY_STORAGE  storage errors (STG_E_*)
 *          0x4 - FACILITY_ITF      interface-specific errors
 *
 *      Code - is the facility's status code
 *
 *
 *}

{*
 *  On Windows NT 3.5 and Windows 95, scodes are 32-bit values
 *  laid out as follows:
 *
 *    3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
 *    1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
 *   +-+-+-+-+-+---------------------+-------------------------------+
 *   |S|R|C|N|r|    Facility         |               Code            |
 *   +-+-+-+-+-+---------------------+-------------------------------+
 *
 *   where
 *
 *      S - Severity - indicates success/fail
 *
 *          0 - Success
 *          1 - Fail (COERROR)
 *
 *      R - reserved portion of the facility code, corresponds to NT's
 *          second severity bit.
 *
 *      C - reserved portion of the facility code, corresponds to NT's
 *          C field.
 *
 *      N - reserved portion of the facility code. Used to indicate a
 *          mapped NT status value.
 *
 *      r - reserved portion of the facility code. Reserved for internal
 *          use. Used to indicate HRESULT values that are not status
 *          values, but are instead message ids for display strings.
 *
 *      Facility - is the facility code
 *          FACILITY_NULL                    0x0
 *          FACILITY_RPC                     0x1
 *          FACILITY_DISPATCH                0x2
 *          FACILITY_STORAGE                 0x3
 *          FACILITY_ITF                     0x4
 *          FACILITY_WIN32                   0x7
 *          FACILITY_WINDOWS                 0x8
 *
 *      Code - is the facility's status code
 *
 *}


{ *  We can't use OLE 2.0 macros to build sCodes because the definition has
  *  changed and we wish to conform to the new definition.
}

function MAKE_MAPI_SCODE(sev,fac,code: DWORD): SCODE;
{$EXTERNALSYM MAKE_MAPI_SCODE}

{ The following two macros are used to build OLE 2.0 style sCodes }

function MAKE_MAPI_E(err: DWORD): SCODE;
{$EXTERNALSYM MAKE_MAPI_E}
function MAKE_MAPI_S(warn: DWORD): SCODE;
{$EXTERNALSYM MAKE_MAPI_S}

const
  SUCCESS_SUCCESS     = 0;
  {$EXTERNALSYM SUCCESS_SUCCESS}

{ General errors (used by more than one WAB object) }

  MAPI_E_CALL_FAILED               = E_FAIL;
  {$EXTERNALSYM MAPI_E_CALL_FAILED}
  MAPI_E_NOT_ENOUGH_MEMORY         = E_OUTOFMEMORY;
  {$EXTERNALSYM MAPI_E_NOT_ENOUGH_MEMORY}
  MAPI_E_INVALID_PARAMETER         = E_INVALIDARG;
  {$EXTERNALSYM MAPI_E_INVALID_PARAMETER}
  MAPI_E_INTERFACE_NOT_SUPPORTED   = E_NOINTERFACE;
  {$EXTERNALSYM MAPI_E_INTERFACE_NOT_SUPPORTED}
  MAPI_E_NO_ACCESS                 = E_ACCESSDENIED;
  {$EXTERNALSYM MAPI_E_NO_ACCESS}

  MAPI_E_NO_SUPPORT                = (1 shl 31 or FACILITY_ITF shl 16 or $102);
  {$EXTERNALSYM MAPI_E_NO_SUPPORT}
  MAPI_E_BAD_CHARWIDTH             = (1 shl 31 or FACILITY_ITF shl 16 or $103);
  {$EXTERNALSYM MAPI_E_BAD_CHARWIDTH}
  MAPI_E_STRING_TOO_LONG           = (1 shl 31 or FACILITY_ITF shl 16 or $105);
  {$EXTERNALSYM MAPI_E_STRING_TOO_LONG}
  MAPI_E_UNKNOWN_FLAGS             = (1 shl 31 or FACILITY_ITF shl 16 or $106);
  {$EXTERNALSYM MAPI_E_UNKNOWN_FLAGS}
  MAPI_E_INVALID_ENTRYID           = (1 shl 31 or FACILITY_ITF shl 16 or $107);
  {$EXTERNALSYM MAPI_E_INVALID_ENTRYID}
  MAPI_E_INVALID_OBJECT            = (1 shl 31 or FACILITY_ITF shl 16 or $108);
  {$EXTERNALSYM MAPI_E_INVALID_OBJECT}
  MAPI_E_OBJECT_CHANGED            = (1 shl 31 or FACILITY_ITF shl 16 or $109);
  {$EXTERNALSYM MAPI_E_OBJECT_CHANGED}
  MAPI_E_OBJECT_DELETED            = (1 shl 31 or FACILITY_ITF shl 16 or $10A);
  {$EXTERNALSYM MAPI_E_OBJECT_DELETED}
  MAPI_E_BUSY                      = (1 shl 31 or FACILITY_ITF shl 16 or $10B);
  {$EXTERNALSYM MAPI_E_BUSY}
  MAPI_E_NOT_ENOUGH_DISK           = (1 shl 31 or FACILITY_ITF shl 16 or $10D);
  {$EXTERNALSYM MAPI_E_NOT_ENOUGH_DISK}
  MAPI_E_NOT_ENOUGH_RESOURCES      = (1 shl 31 or FACILITY_ITF shl 16 or $10E);
  {$EXTERNALSYM MAPI_E_NOT_ENOUGH_RESOURCES}
  MAPI_E_NOT_FOUND                 = (1 shl 31 or FACILITY_ITF shl 16 or $10F);
  {$EXTERNALSYM MAPI_E_NOT_FOUND}
  MAPI_E_VERSION                   = (1 shl 31 or FACILITY_ITF shl 16 or $110);
  {$EXTERNALSYM MAPI_E_VERSION}
  MAPI_E_LOGON_FAILED              = (1 shl 31 or FACILITY_ITF shl 16 or $111);
  {$EXTERNALSYM MAPI_E_LOGON_FAILED}
  MAPI_E_SESSION_LIMIT             = (1 shl 31 or FACILITY_ITF shl 16 or $112);
  {$EXTERNALSYM MAPI_E_SESSION_LIMIT}
  MAPI_E_USER_CANCEL               = (1 shl 31 or FACILITY_ITF shl 16 or $113);
  {$EXTERNALSYM MAPI_E_USER_CANCEL}
  MAPI_E_UNABLE_TO_ABORT           = (1 shl 31 or FACILITY_ITF shl 16 or $114);
  {$EXTERNALSYM MAPI_E_UNABLE_TO_ABORT}
  MAPI_E_NETWORK_ERROR             = (1 shl 31 or FACILITY_ITF shl 16 or $115);
  {$EXTERNALSYM MAPI_E_NETWORK_ERROR}
  MAPI_E_DISK_ERROR                = (1 shl 31 or FACILITY_ITF shl 16 or $116);
  {$EXTERNALSYM MAPI_E_DISK_ERROR}
  MAPI_E_TOO_COMPLEX               = (1 shl 31 or FACILITY_ITF shl 16 or $117);
  {$EXTERNALSYM MAPI_E_TOO_COMPLEX}
  MAPI_E_BAD_COLUMN                = (1 shl 31 or FACILITY_ITF shl 16 or $118);
  {$EXTERNALSYM MAPI_E_BAD_COLUMN}
  MAPI_E_EXTENDED_ERROR            = (1 shl 31 or FACILITY_ITF shl 16 or $119);
  {$EXTERNALSYM MAPI_E_EXTENDED_ERROR}
  MAPI_E_COMPUTED                  = (1 shl 31 or FACILITY_ITF shl 16 or $11A);
  {$EXTERNALSYM MAPI_E_COMPUTED}
  MAPI_E_CORRUPT_DATA              = (1 shl 31 or FACILITY_ITF shl 16 or $11B);
  {$EXTERNALSYM MAPI_E_CORRUPT_DATA}
  MAPI_E_UNCONFIGURED              = (1 shl 31 or FACILITY_ITF shl 16 or $11C);
  {$EXTERNALSYM MAPI_E_UNCONFIGURED}
  MAPI_E_FAILONEPROVIDER           = (1 shl 31 or FACILITY_ITF shl 16 or $11D);
  {$EXTERNALSYM MAPI_E_FAILONEPROVIDER}

{ WAB base function and status object specific errors and warnings }

  MAPI_E_END_OF_SESSION            = (1 shl 31 or FACILITY_ITF shl 16 or $200);
  {$EXTERNALSYM MAPI_E_END_OF_SESSION}
  MAPI_E_UNKNOWN_ENTRYID           = (1 shl 31 or FACILITY_ITF shl 16 or $201);
  {$EXTERNALSYM MAPI_E_UNKNOWN_ENTRYID}
  MAPI_E_MISSING_REQUIRED_COLUMN   = (1 shl 31 or FACILITY_ITF shl 16 or $202);
  {$EXTERNALSYM MAPI_E_MISSING_REQUIRED_COLUMN}
  MAPI_W_NO_SERVICE                = (FACILITY_ITF shl 16 or $203);
  {$EXTERNALSYM MAPI_W_NO_SERVICE}

{ Property specific errors and warnings }

  MAPI_E_BAD_VALUE                 = (1 shl 31 or FACILITY_ITF shl 16 or $301);
  {$EXTERNALSYM MAPI_E_BAD_VALUE}
  MAPI_E_INVALID_TYPE              = (1 shl 31 or FACILITY_ITF shl 16 or $302);
  {$EXTERNALSYM MAPI_E_INVALID_TYPE}
  MAPI_E_TYPE_NO_SUPPORT           = (1 shl 31 or FACILITY_ITF shl 16 or $303);
  {$EXTERNALSYM MAPI_E_TYPE_NO_SUPPORT}
  MAPI_E_UNEXPECTED_TYPE           = (1 shl 31 or FACILITY_ITF shl 16 or $304);
  {$EXTERNALSYM MAPI_E_UNEXPECTED_TYPE}
  MAPI_E_TOO_BIG                   = (1 shl 31 or FACILITY_ITF shl 16 or $305);
  {$EXTERNALSYM MAPI_E_TOO_BIG}
  MAPI_E_DECLINE_COPY              = (1 shl 31 or FACILITY_ITF shl 16 or $306);
  {$EXTERNALSYM MAPI_E_DECLINE_COPY}
  MAPI_E_UNEXPECTED_ID             = (1 shl 31 or FACILITY_ITF shl 16 or $307);
  {$EXTERNALSYM MAPI_E_UNEXPECTED_ID}

  MAPI_W_ERRORS_RETURNED           = (FACILITY_ITF shl 16 or $380);
  {$EXTERNALSYM MAPI_W_ERRORS_RETURNED}

{ Table specific errors and warnings }

  MAPI_E_UNABLE_TO_COMPLETE        = (1 shl 31 or FACILITY_ITF shl 16 or $400);
  {$EXTERNALSYM MAPI_E_UNABLE_TO_COMPLETE}
  MAPI_E_TIMEOUT                   = (1 shl 31 or FACILITY_ITF shl 16 or $401);
  {$EXTERNALSYM MAPI_E_TIMEOUT}
  MAPI_E_TABLE_EMPTY               = (1 shl 31 or FACILITY_ITF shl 16 or $402);
  {$EXTERNALSYM MAPI_E_TABLE_EMPTY}
  MAPI_E_TABLE_TOO_BIG             = (1 shl 31 or FACILITY_ITF shl 16 or $403);
  {$EXTERNALSYM MAPI_E_TABLE_TOO_BIG}

  MAPI_E_INVALID_BOOKMARK          = (1 shl 31 or FACILITY_ITF shl 16 or $405);
  {$EXTERNALSYM MAPI_E_INVALID_BOOKMARK}

  MAPI_W_POSITION_CHANGED          = (FACILITY_ITF shl 16 or $481);
  {$EXTERNALSYM MAPI_W_POSITION_CHANGED}
  MAPI_W_APPROX_COUNT              = (FACILITY_ITF shl 16 or $482);
  {$EXTERNALSYM MAPI_W_APPROX_COUNT}

  MAPI_W_PARTIAL_COMPLETION        = (FACILITY_ITF shl 16 or $680);
  {$EXTERNALSYM MAPI_W_PARTIAL_COMPLETION}

{ Address Book specific errors and warnings }

  MAPI_E_AMBIGUOUS_RECIP           = (1 shl 31 or FACILITY_ITF shl 16 or $700);
  {$EXTERNALSYM MAPI_E_AMBIGUOUS_RECIP}

{ Miscellaneous errors }

  MAPI_E_COLLISION                 = (1 shl 31 or FACILITY_ITF shl 16 or $604);
  {$EXTERNALSYM MAPI_E_COLLISION}
  MAPI_E_NOT_INITIALIZED           = (1 shl 31 or FACILITY_ITF shl 16 or $605);
  {$EXTERNALSYM MAPI_E_NOT_INITIALIZED}
  MAPI_E_FOLDER_CYCLE              = (1 shl 31 or FACILITY_ITF shl 16 or $60B);
  {$EXTERNALSYM MAPI_E_FOLDER_CYCLE}

{ The range 0x0800 to 0x08FF is reserved }

{* We expect these to eventually be defined by OLE, but for now,
 * here they are.  When OLE defines them they can be much more
 * efficient than these, but these are "proper" and don't make
 * use of any hidden tricks.
}

function HR_SUCCEEDED(_hr: HRESULT): Boolean;
{$EXTERNALSYM HR_SUCCEEDED}
function HR_FAILED(_hr: HRESULT): Boolean;
{$EXTERNALSYM HR_FAILED}

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INTERFACESECTION}

function MAKE_MAPI_SCODE(sev,fac,code: DWORD): SCODE;
begin
  Result := sev shl 31 or fac shl 16 or code;
end;

function MAKE_MAPI_E(err: DWORD): SCODE;
begin
  Result := MAKE_MAPI_SCODE(1, FACILITY_ITF, err);
end;

function MAKE_MAPI_S(warn: DWORD): SCODE;
begin
  Result := MAKE_MAPI_SCODE(0, FACILITY_ITF, warn);
end;

function HR_SUCCEEDED(_hr: HRESULT): Boolean;
begin
  Result := Succeeded(_hr);
end;

function HR_FAILED(_hr: HRESULT): Boolean;
begin
  Result := Failed(_hr);
end;

{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
