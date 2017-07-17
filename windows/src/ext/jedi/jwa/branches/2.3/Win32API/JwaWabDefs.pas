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
{ The original file is: wabdefs.h, released 31 Jan 2000.           			   }
{ The original Pascal code is: WabDefs.pas, released 15 Mar 2000.  			   }
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
unit JwaWabDefs;

interface

uses
  Windows, ActiveX;

{$I ..\Includes\JediAPILib.inc}


{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

(*$HPPEMIT '#include <wabdefs.h>'*)
(*$HPPEMIT '#include <windows.h>'*)
(*$HPPEMIT '#include <objerror.h>'*)
(*$HPPEMIT '#include <objbase.h>'*)
(*$HPPEMIT '#include <stddef.h>'*)

{$ENDIF JWA_OMIT_SECTIONS}

{ Array dimension for structures with variable-sized arrays at the end. }

{$IFNDEF JWA_IMPLEMENTATIONSECTION}

const
  MAPI_DIM    = 1;
  {$EXTERNALSYM MAPI_DIM}


{*  This flag is used in many different MAPI calls to signify that
 *  the object opened by the call should be modifiable (MAPI_MODIFY).
 *  If the flag MAPI_MAX_ACCESS is set, the object returned should be
 *  returned at the maximum access level allowed.  An additional
 *  property available on the object (PR_ACCESS_LEVEL) uses the same
 *  MAPI_MODIFY flag to say just what this new access level is.
}

  MAPI_MODIFY               = ULONG($00000001);
  {$EXTERNALSYM MAPI_MODIFY}

{*  The following flags are used to indicate to the client what access
 *  level is permissible in the object. They appear in PR_ACCESS in
 *  message and folder objects as well as in contents and associated
 *  contents tables
}

  MAPI_ACCESS_MODIFY                    = ULONG($00000001);
  {$EXTERNALSYM MAPI_ACCESS_MODIFY}
  MAPI_ACCESS_READ                      = ULONG($00000002);
  {$EXTERNALSYM MAPI_ACCESS_READ}
  MAPI_ACCESS_DELETE                    = ULONG($00000004);
  {$EXTERNALSYM MAPI_ACCESS_DELETE}
  MAPI_ACCESS_CREATE_HIERARCHY          = ULONG($00000008);
  {$EXTERNALSYM MAPI_ACCESS_CREATE_HIERARCHY}
  MAPI_ACCESS_CREATE_CONTENTS           = ULONG($00000010);
  {$EXTERNALSYM MAPI_ACCESS_CREATE_CONTENTS}
  MAPI_ACCESS_CREATE_ASSOCIATED         = ULONG($00000020);
  {$EXTERNALSYM MAPI_ACCESS_CREATE_ASSOCIATED}

{*  The MAPI_UNICODE flag is used in many different MAPI calls to signify
 *  that strings passed through the interface are in Unicode (a 16-bit
 *  character set). The default is an 8-bit character set.
 *
 *  The value fMapiUnicode can be used as the 'normal' value for
 *  that bit, given the application's default character set.
}

  MAPI_UNICODE = ULONG($80000000);
  {$EXTERNALSYM MAPI_UNICODE}

  hrSuccess    = 0;
  {$EXTERNALSYM hrSuccess}

{ Recipient types }
  MAPI_ORIG   = 0;             // Recipient is message originator
  {$EXTERNALSYM MAPI_ORIG}
  MAPI_TO     = 1;             // Recipient is a primary recipient
  {$EXTERNALSYM MAPI_TO}
  MAPI_CC     = 2;             // Recipient is a copy recipient
  {$EXTERNALSYM MAPI_CC}
  MAPI_BCC    = 3;             // Recipient is blind copy recipient
  {$EXTERNALSYM MAPI_BCC}
  MAPI_P1        = $10000000;  // Recipient is a P1 resend recipient
  {$EXTERNALSYM MAPI_P1}
  MAPI_SUBMITTED = $80000000;  // Recipient is already processed
  {$EXTERNALSYM MAPI_SUBMITTED}
  MAPI_AUTHORIZE = 4;          // Recipient is a CMC authorizing user
  {$EXTERNALSYM MAPI_AUTHORIZE}
  MAPI_DISCRETE  = $10000000;  // Recipient is a P1 resend recipient
  {$EXTERNALSYM MAPI_DISCRETE}

{ Bit definitions for abFlags[0] of ENTRYID }
  MAPI_SHORTTERM          = $80;
  {$EXTERNALSYM MAPI_SHORTTERM}
  MAPI_NOTRECIP           = $40;
  {$EXTERNALSYM MAPI_NOTRECIP}
  MAPI_THISSESSION        = $20;
  {$EXTERNALSYM MAPI_THISSESSION}
  MAPI_NOW                = $10;
  {$EXTERNALSYM MAPI_NOW}
  MAPI_NOTRESERVED        = $08;
  {$EXTERNALSYM MAPI_NOTRESERVED}

{ Bit definitions for abFlags[1] of ENTRYID }
  MAPI_COMPOUND           = $80;
  {$EXTERNALSYM MAPI_COMPOUND}

type
  PEntryID = ^TEntryID;
  _ENTRYID = record
    abFlags: array[0..3] of Byte;
    ab: array[0..MAPI_DIM-1] of Byte;
  end;
  {$EXTERNALSYM _ENTRYID}
  TEntryID = _ENTRYID;
  ENTRYID = _ENTRYID;
  {$EXTERNALSYM ENTRYID}

(*!!!
#define CbNewENTRYID(_cb)       (offsetof(ENTRYID,ab) + (_cb))
#define CbENTRYID(_cb)          (offsetof(ENTRYID,ab) + (_cb))
#define SizedENTRYID(_cb, _name) \
    struct _ENTRYID_ ## _name \
{ \
    BYTE    abFlags[4]; \
    BYTE    ab[_cb]; \
} _name
*)

{ Byte-order-independent version of GUID (world-unique identifier) }

  PMapiUID = ^TMapiUID;
  _MAPIUID = record
    ab: array[0..15] of Byte;
  end;
  {$EXTERNALSYM _MAPIUID}
  TMapiUID = _MAPIUID;
  MAPIUID = _MAPIUID;
  {$EXTERNALSYM MAPIUID}

function IsEqualMAPIUID(lpuid1, lpuid2: TMapiUID): Boolean;
{$EXTERNALSYM IsEqualMAPIUID}

const
  MAPI_STORE      = ULONG($00000001);    // Message Store
  {$EXTERNALSYM MAPI_STORE}
  MAPI_ADDRBOOK   = ULONG($00000002);    // Address Book
  {$EXTERNALSYM MAPI_ADDRBOOK}
  MAPI_FOLDER     = ULONG($00000003);    // Folder
  {$EXTERNALSYM MAPI_FOLDER}
  MAPI_ABCONT     = ULONG($00000004);    // Address Book Container
  {$EXTERNALSYM MAPI_ABCONT}
  MAPI_MESSAGE    = ULONG($00000005);    // Message
  {$EXTERNALSYM MAPI_MESSAGE}
  MAPI_MAILUSER   = ULONG($00000006);    // Individual Recipient
  {$EXTERNALSYM MAPI_MAILUSER}
  MAPI_ATTACH     = ULONG($00000007);    // Attachment
  {$EXTERNALSYM MAPI_ATTACH}
  MAPI_DISTLIST   = ULONG($00000008);    // Distribution List Recipient
  {$EXTERNALSYM MAPI_DISTLIST}
  MAPI_PROFSECT   = ULONG($00000009);    // Profile Section
  {$EXTERNALSYM MAPI_PROFSECT}
  MAPI_STATUS     = ULONG($0000000A);    // Status Object
  {$EXTERNALSYM MAPI_STATUS}
  MAPI_SESSION    = ULONG($0000000B);    // Session
  {$EXTERNALSYM MAPI_SESSION}
  MAPI_FORMINFO   = ULONG($0000000C);    // Form Information
  {$EXTERNALSYM MAPI_FORMINFO}


{ Maximum length of profile names and passwords, not including the null termination character. }

  cchProfileNameMax   = 64;
  {$EXTERNALSYM cchProfileNameMax}
  cchProfilePassMax   = 64;
  {$EXTERNALSYM cchProfilePassMax}

{ Property Types }

  MV_FLAG         = $1000;        // Multi-value flag 
  {$EXTERNALSYM MV_FLAG}

  PT_UNSPECIFIED  = ULONG(0);     // (Reserved for interface use) type doesn't matter to caller
  {$EXTERNALSYM PT_UNSPECIFIED}
  PT_NULL         = ULONG(1);     // NULL property value
  {$EXTERNALSYM PT_NULL}
  PT_I2           = ULONG(2);     // Signed 16-bit value
  {$EXTERNALSYM PT_I2}
  PT_LONG         = ULONG(3);     // Signed 32-bit value
  {$EXTERNALSYM PT_LONG}
  PT_R4           = ULONG(4);     // 4-byte floating point
  {$EXTERNALSYM PT_R4}
  PT_DOUBLE       = ULONG(5);     // Floating point double
  {$EXTERNALSYM PT_DOUBLE}
  PT_CURRENCY     = ULONG(6);     // Signed 64-bit int (decimal w/    4 digits right of decimal pt)
  {$EXTERNALSYM PT_CURRENCY}
  PT_APPTIME      = ULONG(7);     // Application time
  {$EXTERNALSYM PT_APPTIME}
  PT_ERROR        = ULONG(10);    // 32-bit error value
  {$EXTERNALSYM PT_ERROR}
  PT_BOOLEAN      = ULONG(11);    // 16-bit boolean (non-zero true)
  {$EXTERNALSYM PT_BOOLEAN}
  PT_OBJECT       = ULONG(13);    // Embedded object in a property
  {$EXTERNALSYM PT_OBJECT}
  PT_I8           = ULONG(20);    // 8-byte signed integer
  {$EXTERNALSYM PT_I8}
  PT_STRING8      = ULONG(30);    // Null terminated 8-bit character string
  {$EXTERNALSYM PT_STRING8}
  PT_UNICODE      = ULONG(31);    // Null terminated Unicode string
  {$EXTERNALSYM PT_UNICODE}
  PT_SYSTIME      = ULONG(64);    // FILETIME 64-bit int w/ number of 100ns periods since Jan 1,1601
  {$EXTERNALSYM PT_SYSTIME}
  PT_CLSID        = ULONG(72);    // OLE GUID
  {$EXTERNALSYM PT_CLSID}
  PT_BINARY       = ULONG(258);   // Uninterpreted (counted byte array)
  {$EXTERNALSYM PT_BINARY}

{ Alternate property type names for ease of use }
  PT_SHORT    = PT_I2;
  {$EXTERNALSYM PT_SHORT}
  PT_I4       = PT_LONG;
  {$EXTERNALSYM PT_I4}
  PT_FLOAT    = PT_R4;
  {$EXTERNALSYM PT_FLOAT}
  PT_R8       = PT_DOUBLE;
  {$EXTERNALSYM PT_R8}
  PT_LONGLONG = PT_I8;
  {$EXTERNALSYM PT_LONGLONG}

{*  The type of a MAPI-defined string property is indirected, so
 *  that it defaults to Unicode string on a Unicode platform and to
 *  String8 on an ANSI or DBCS platform.
 *
 *  Macros are defined here both for the property type, and for the
 *  field of the property value structure which should be
 *  dereferenced to obtain the string pointer.
}

{$IFDEF UNICODE}
  PT_TSTRING          = PT_UNICODE;
  {$EXTERNALSYM PT_TSTRING}
  PT_MV_TSTRING       = MV_FLAG or PT_UNICODE;
  {$EXTERNALSYM PT_MV_TSTRING}
{$ELSE}
  PT_TSTRING          = PT_STRING8;
  {$EXTERNALSYM PT_TSTRING}
  PT_MV_TSTRING       = MV_FLAG or PT_STRING8;
  {$EXTERNALSYM PT_MV_TSTRING}
{$ENDIF}

{* Property Tags
 *
 * By convention, MAPI never uses 0 or FFFF as a property ID.
 * Use as null values, initializers, sentinels, or what have you.
}

  PROP_TYPE_MASK          = ULONG($0000FFFF); // Mask for Property type
  {$EXTERNALSYM PROP_TYPE_MASK}
  PROP_ID_NULL            = 0;
  {$EXTERNALSYM PROP_ID_NULL}
  PROP_ID_INVALID         = $FFFF;
  {$EXTERNALSYM PROP_ID_INVALID}
  PR_NULL                 = PT_NULL shl 16 or PROP_ID_NULL;
  {$EXTERNALSYM PR_NULL}

function PROP_TYPE(ulPropTag: ULONG): ULONG;
{$EXTERNALSYM PROP_TYPE}
function PROP_ID(ulPropTag: ULONG): ULONG;
{$EXTERNALSYM PROP_ID}
function PROP_TAG(ulPropType, ulPropID: ULONG): ULONG;
{$EXTERNALSYM PROP_TAG}
function CHANGE_PROP_TYPE(ulPropTag, ulPropType: ULONG): ULONG;
{$EXTERNALSYM CHANGE_PROP_TYPE}

const
{ Multi-valued Property Types }
  PT_MV_I2        = (MV_FLAG or PT_I2);
  {$EXTERNALSYM PT_MV_I2}
  PT_MV_LONG      = (MV_FLAG or PT_LONG);
  {$EXTERNALSYM PT_MV_LONG}
  PT_MV_R4        = (MV_FLAG or PT_R4);
  {$EXTERNALSYM PT_MV_R4}
  PT_MV_DOUBLE    = (MV_FLAG or PT_DOUBLE);
  {$EXTERNALSYM PT_MV_DOUBLE}
  PT_MV_CURRENCY  = (MV_FLAG or PT_CURRENCY);
  {$EXTERNALSYM PT_MV_CURRENCY}
  PT_MV_APPTIME   = (MV_FLAG or PT_APPTIME);
  {$EXTERNALSYM PT_MV_APPTIME}
  PT_MV_SYSTIME   = (MV_FLAG or PT_SYSTIME);
  {$EXTERNALSYM PT_MV_SYSTIME}
  PT_MV_STRING8   = (MV_FLAG or PT_STRING8);
  {$EXTERNALSYM PT_MV_STRING8}
  PT_MV_BINARY    = (MV_FLAG or PT_BINARY);
  {$EXTERNALSYM PT_MV_BINARY}
  PT_MV_UNICODE   = (MV_FLAG or PT_UNICODE);
  {$EXTERNALSYM PT_MV_UNICODE}
  PT_MV_CLSID     = (MV_FLAG or PT_CLSID);
  {$EXTERNALSYM PT_MV_CLSID}
  PT_MV_I8        = (MV_FLAG or PT_I8);
  {$EXTERNALSYM PT_MV_I8}

{ Alternate property type names for ease of use }
  PT_MV_SHORT     = PT_MV_I2;
  {$EXTERNALSYM PT_MV_SHORT}
  PT_MV_I4        = PT_MV_LONG;
  {$EXTERNALSYM PT_MV_I4}
  PT_MV_FLOAT     = PT_MV_R4;
  {$EXTERNALSYM PT_MV_FLOAT}
  PT_MV_R8        = PT_MV_DOUBLE;
  {$EXTERNALSYM PT_MV_R8}
  PT_MV_LONGLONG  = PT_MV_I8;
  {$EXTERNALSYM PT_MV_LONGLONG}

{*  Property type reserved bits
 *
 *  MV_INSTANCE is used as a flag in table operations to request
 *  that a multi-valued property be presented as a single-valued
 *  property appearing in multiple rows.
}

  MV_INSTANCE     = $2000;
  {$EXTERNALSYM MV_INSTANCE}
  MVI_FLAG        = (MV_FLAG or MV_INSTANCE);
  {$EXTERNALSYM MVI_FLAG}
function MVI_PROP(tag: ULONG): ULONG;
{$EXTERNALSYM MVI_PROP}

{ Data Structures }

{ Property Tag Array }

type
  PSPropTagArray = ^TSPropTagArray;
  _SPropTagArray = record
    cValues: ULONG;
    aulPropTag: array[0..MAPI_DIM-1] of ULONG;
  end;
  {$EXTERNALSYM _SPropTagArray}
  TSPropTagArray = _SPropTagArray;
  SPropTagArray = _SPropTagArray;
  {$EXTERNALSYM SPropTagArray}

(*!!!
#define CbNewSPropTagArray(_ctag) \
    (offsetof(SPropTagArray,aulPropTag) + (_ctag)*sizeof(ULONG))
#define CbSPropTagArray(_lparray) \
    (offsetof(SPropTagArray,aulPropTag) + \
    (UINT)((_lparray)->cValues)*sizeof(ULONG))
/*  SPropTagArray
#define SizedSPropTagArray(_ctag, _name) \
struct _SPropTagArray_ ## _name \
{ \
    ULONG   cValues; \
    ULONG   aulPropTag[_ctag]; \
} _name
*)

{ 32-bit CURRENCY definition stolen from oaidl.h }

{ real definition that makes the C++ compiler happy }

type
  PCY = ^TCY;
  tagCY = record
    case Integer of
      1: (
           {$IFDEF _MAC}
           Hi : Longint;
           Lo : Longint;
           {$ELSE}
           Lo : Longint;
           Hi : Longint;
           {$ENDIF}
         );
      2: (
         int64: LONGLONG;
         );
  end;
  {$EXTERNALSYM tagCY}
  TCY = tagCY;
  CY = tagCY;
  {$EXTERNALSYM CY}

  TCURRENCY = TCY;

  PSBinary = ^TSBinary;
  _SBinary = record
    cb: ULONG;
    lpb: Pointer;
  end;
  {$EXTERNALSYM _SBinary}
  TSBinary = _SBinary;
  SBinary = _SBinary;
  {$EXTERNALSYM SBinary}

  PSShortArray = ^TSShortArray;
  _SShortArray = record
    cValues: ULONG;
    lpi: ^SmallInt;
  end;
  {$EXTERNALSYM _SShortArray}
  TSShortArray = _SShortArray;
  SShortArray = _SShortArray;
  {$EXTERNALSYM SShortArray}

  PSGuidArray = ^TSGuidArray;
  _SGuidArray = record
    cValues: ULONG;
    lpguid: ^TGUID;
  end;
  {$EXTERNALSYM _SGuidArray}
  TSGuidArray = _SGuidArray;
  SGuidArray = _SGuidArray;
  {$EXTERNALSYM SGuidArray}

  PSRealArray = ^TSRealArray;
  _SRealArray = record
    cValues: ULONG;
    lpflt: ^Single;
  end;
  {$EXTERNALSYM _SRealArray}
  TSRealArray = _SRealArray;
  SRealArray = _SRealArray;
  {$EXTERNALSYM SRealArray}

  PSLongArray = ^TSLongArray;
  _SLongArray = record
    cValues: ULONG;
    lpl: ^LongInt;
  end;
  {$EXTERNALSYM _SLongArray}
  TSLongArray = _SLongArray;
  SLongArray = _SLongArray;
  {$EXTERNALSYM SLongArray}

  PSLargeIntegerArray = ^TSLargeIntegerArray;
  _SLargeIntegerArray = record
    cValues: ULONG;
    lpli: ^TLargeInteger;
  end;
  {$EXTERNALSYM _SLargeIntegerArray}
  TSLargeIntegerArray = _SLargeIntegerArray;
  SLargeIntegerArray = _SLargeIntegerArray;
  {$EXTERNALSYM SLargeIntegerArray}

  PSDateTimeArray = ^TSDateTimeArray;
  _SDateTimeArray = record
    cValues: ULONG;
    lpft: ^TFileTime;
  end;
  {$EXTERNALSYM _SDateTimeArray}
  TSDateTimeArray = _SDateTimeArray;
  SDateTimeArray = _SDateTimeArray;
  {$EXTERNALSYM SDateTimeArray}

  PSAppTimeArray = ^TSAppTimeArray;
  _SAppTimeArray = record
    cValues: ULONG;
    lpat: ^Double;
  end;
  {$EXTERNALSYM _SAppTimeArray}
  TSAppTimeArray = _SAppTimeArray;
  SAppTimeArray = _SAppTimeArray;
  {$EXTERNALSYM SAppTimeArray}

  PSCurrencyArray = ^TSCurrencyArray;
  _SCurrencyArray = record
    cValues: ULONG;
    lpcur: ^Currency;
  end;
  {$EXTERNALSYM _SCurrencyArray}
  TSCurrencyArray = _SCurrencyArray;
  SCurrencyArray = _SCurrencyArray;
  {$EXTERNALSYM SCurrencyArray}

  PSBinaryArray = ^TSBinaryArray;
  _SBinaryArray = record
    cValues: ULONG;
    lpbin: PSBinary;
  end;
  {$EXTERNALSYM _SBinaryArray}
  TSBinaryArray = _SBinaryArray;
  SBinaryArray = _SBinaryArray;
  {$EXTERNALSYM SBinaryArray}

  PSDoubleArray = ^TSDoubleArray;
  _SDoubleArray = record
    cValues: ULONG;
    lpdbl: ^Double;
  end;
  {$EXTERNALSYM _SDoubleArray}
  TSDoubleArray = _SDoubleArray;
  SDoubleArray = _SDoubleArray;
  {$EXTERNALSYM SDoubleArray}

  PSWStringArray = ^TSWStringArray;
  _SWStringArray = record
    cValues: ULONG;
    lppszW: ^LPWSTR;
  end;
  {$EXTERNALSYM _SWStringArray}
  TSWStringArray = _SWStringArray;
  SWStringArray = _SWStringArray;
  {$EXTERNALSYM SWStringArray}

  PSLPSTRArray = ^TSLPSTRArray;
  _SLPSTRArray = record
    cValues: ULONG;
    lppszA: ^LPSTR;
  end;
  {$EXTERNALSYM _SLPSTRArray}
  TSLPSTRArray = _SLPSTRArray;
  SLPSTRArray = _SLPSTRArray;
  {$EXTERNALSYM SLPSTRArray}

  __UPV = record
    case Integer of
      0: (i: SmallInt;);             // case PT_I2
      1: (l: LongInt;);              // case PT_LONG
      2: (ul: ULONG;);               // alias for PT_LONG
      3: (flt: Single;);             // case PT_R4
      4: (dbl: Double;);             // case PT_DOUBLE
      5: (b: Word;);                 // case PT_BOOLEAN
      6: (cur: Currency;);           // case PT_CURRENCY
      7: (at: Double;);              // case PT_APPTIME
      8: (ft: TFileTime;);           // case PT_SYSTIME
      9: (lpszA: LPSTR;);            // case PT_STRING8
     10: (bin: TSBinary;);           // case PT_BINARY
     11: (lpszW: LPWSTR;);           // case PT_UNICODE
     12: (lpguid: PGUID;);           // case PT_CLSID
     13: (li: TLargeInteger;);       // case PT_I8
     14: (MVi: TSShortArray;);       // case PT_MV_I2
     15: (MVl: TSLongArray;);        // case PT_MV_LONG
     16: (MVflt: TSRealArray;);      // case PT_MV_R4
     17: (MVdbl: TSDoubleArray;);    // case PT_MV_DOUBLE
     18: (MVcur: TSCurrencyArray;);  // case PT_MV_CURRENCY
     19: (MVat: TSAppTimeArray;);    // case PT_MV_APPTIME
     20: (MVft: TSDateTimeArray;);   // case PT_MV_SYSTIME
     21: (MVbin: TSBinaryArray;);    // case PT_MV_BINARY
     22: (MVszA: TSLPSTRArray;);     // case PT_MV_STRING8
     23: (MVszW: TSWStringArray;);   // case PT_MV_UNICODE
     24: (MVguid: TSGuidArray;);     // case PT_MV_CLSID
     25: (MVli: TSLargeIntegerArray;);  // case PT_MV_I8
     26: (err: SCODE;);              // case PT_ERROR
     27: (x: LongInt;);              // case PT_NULL, PT_OBJECT (no usable value) *
  end;
  {$EXTERNALSYM __UPV}
  _PV = __UPV;
  {$EXTERNALSYM _PV}

  PSPropValue = ^TSPropValue;
  _SPropValue = record
    ulPropTag: ULONG;
    dwAlignPad: ULONG;
    Value: _PV;
  end;
  {$EXTERNALSYM _SPropValue}
  TSPropValue = _SPropValue;
  SPropValue = _SPropValue;
  {$EXTERNALSYM SPropValue}

{ Property Problem and Property Problem Arrays }

  PSPropProblem = ^TSPropProblem;
  _SPropProblem = record
    ulIndex: ULONG;
    ulPropTag: ULONG;
    _scode: SCODE;
  end;
  {$EXTERNALSYM _SPropProblem}
  TSPropProblem = _SPropProblem;
  SPropProblem = _SPropProblem;
  {$EXTERNALSYM SPropProblem}

  PSPropProblemArray = ^TSPropProblemArray;
  _SPropProblemArray = record
    cProblem: ULONG;
    aProblem: array[0..MAPI_DIM-1] of TSPropProblem;
  end;
  {$EXTERNALSYM _SPropProblemArray}
  TSPropProblemArray = _SPropProblemArray;
  SPropProblemArray = _SPropProblemArray;
  {$EXTERNALSYM SPropProblemArray}

(*!!!
#define CbNewSPropProblemArray(_cprob) \
    (offsetof(SPropProblemArray,aProblem) + (_cprob)*sizeof(SPropProblem))
#define CbSPropProblemArray(_lparray) \
    (offsetof(SPropProblemArray,aProblem) + \
    (UINT) ((_lparray)->cProblem*sizeof(SPropProblem)))
#define SizedSPropProblemArray(_cprob, _name) \
struct _SPropProblemArray_ ## _name \
{ \
    ULONG           cProblem; \
    SPropProblem    aProblem[_cprob]; \
} _name
*)

{ ENTRYLIST }

  PEntryList = ^TEntryList;
  ENTRYLIST = TSBinaryArray;
  {$EXTERNALSYM ENTRYLIST}
  TEntryList = ENTRYLIST;

{ FLATENTRYLIST }
{ MTSID }
{ FLATMTSIDLIST }

  PFlatEntry = ^TFlatEntry;
  FLATENTRY = record
    cb: ULONG;
    abEntry: array[0..MAPI_DIM-1] of Byte;
  end;
  {$EXTERNALSYM FLATENTRY}
  TFlatEntry = FLATENTRY;

  PFlatEntryList = ^TFlatEntryList;
  FLATENTRYLIST = record
    cEntries: ULONG;
    cbEntries: ULONG;
    abEntries: array[0..MAPI_DIM-1] of Byte;
  end;
  {$EXTERNALSYM FLATENTRYLIST}
  TFlatEntryList = FLATENTRYLIST;

  PMTSID = ^TMTSID;
  TMTSID = record
    cb: ULONG;
    ab: array[0..MAPI_DIM-1] of Byte;
  end;
  {$EXTERNALSYM TMTSID}

  PFlatMtsIdList = ^TFlatMtsIdList;
  FLATMTSIDLIST = record
    cMTSIDs: ULONG;
    cbMTSIDs: ULONG;
    abMTSIDs: array[0..MAPI_DIM-1] of Byte;
  end;
  {$EXTERNALSYM FLATMTSIDLIST}
  TFlatMtsIdList = FLATMTSIDLIST;

(*!!!
#define CbNewFLATENTRY(_cb)     (offsetof(FLATENTRY,abEntry) + (_cb))
#define CbFLATENTRY(_lpentry)   (offsetof(FLATENTRY,abEntry) + (_lpentry)->cb)
#define CbNewFLATENTRYLIST(_cb) (offsetof(FLATENTRYLIST,abEntries) + (_cb))
#define CbFLATENTRYLIST(_lplist) (offsetof(FLATENTRYLIST,abEntries) + (_lplist)->cbEntries)
#define CbNewMTSID(_cb)         (offsetof(MTSID,ab) + (_cb))
#define CbMTSID(_lpentry)       (offsetof(MTSID,ab) + (_lpentry)->cb)
#define CbNewFLATMTSIDLIST(_cb) (offsetof(FLATMTSIDLIST,abMTSIDs) + (_cb))
#define CbFLATMTSIDLIST(_lplist) (offsetof(FLATMTSIDLIST,abMTSIDs) + (_lplist)->cbMTSIDs)
/* No SizedXXX macros for these types.
*)

  PAdrEntry = ^TAdrEntry;
  _ADRENTRY = record
    ulReserved1: ULONG;    // Never used
    cValues: ULONG;
    rgPropVals: PSPropValue;
  end;
  {$EXTERNALSYM _ADRENTRY}
  TAdrEntry = _ADRENTRY;
  ADRENTRY = _ADRENTRY;
  {$EXTERNALSYM ADRENTRY}

  PAdrList = ^TAdrList;
  _ADRLIST = record
    cEntries: ULONG;
    aEntries: array[0..MAPI_DIM-1] of TAdrEntry;
  end;
  {$EXTERNALSYM _ADRLIST}
  TAdrList = _ADRLIST;
  ADRLIST = _ADRLIST;
  {$EXTERNALSYM ADRLIST}

(*!!!
#define CbNewADRLIST(_centries) \
    (offsetof(ADRLIST,aEntries) + (_centries)*sizeof(ADRENTRY))
#define CbADRLIST(_lpadrlist) \
    (offsetof(ADRLIST,aEntries) + (UINT)(_lpadrlist)->cEntries*sizeof(ADRENTRY))
#define SizedADRLIST(_centries, _name) \
struct _ADRLIST_ ## _name \
{ \
    ULONG           cEntries; \
    ADRENTRY        aEntries[_centries]; \
} _name
*)

  PSPropsArray = ^TSPropsArray;
  TSPropsArray = array[Byte] of TSPropValue;
  {$NODEFINE TSPropsArray}

  PSRow = ^TSRow;
  _SRow = record
    ulAdrEntryPad: ULONG;  // Pad so SRow's can map to ADRENTRY's
    cValues: ULONG;        // Count of property values
    lpProps: PSPropsArray ; // Property value array
  end;
  {$EXTERNALSYM _SRow}
  TSRow = _SRow;
  SRow = _SRow;
  {$EXTERNALSYM SRow}

  PSRowSet = ^TSRowSet;
  _SRowSet = record
    cRows: ULONG;          // Count of rows
    aRow: array[0..MAPI_DIM-1] of TSRow; // Array of rows
  end;
  {$EXTERNALSYM _SRowSet}
  TSRowSet = _SRowSet;
  SRowSet = _SRowSet;
  {$EXTERNALSYM SRowSet}

(*!!!
#define CbNewSRowSet(_crow)     (offsetof(SRowSet,aRow) + (_crow)*sizeof(SRow))
#define CbSRowSet(_lprowset)    (offsetof(SRowSet,aRow) + \
                                    (UINT)((_lprowset)->cRows*sizeof(SRow)))
#define SizedSRowSet(_crow, _name) \
struct _SRowSet_ ## _name \
{ \
    ULONG           cRows; \
    SRow            aRow[_crow]; \
} _name
*)

{ MAPI Allocation Routines ------------------------------------------------ }

type
  PAllocateBuffer = ^TAllocateBuffer;
  ALLOCATEBUFFER = function (cbSize: ULONG; var lppBuffer: Pointer): SCODE; stdcall;
  {$EXTERNALSYM ALLOCATEBUFFER}
  TAllocateBuffer = ALLOCATEBUFFER;

  PAllocateMore = ^TAllocateMore;
  ALLOCATEMORE = function (cbSize: ULONG; lpObject: Pointer; var lppBuffer: Pointer): SCODE; stdcall;
  {$EXTERNALSYM ALLOCATEMORE}
  TAllocateMore = ALLOCATEMORE;

  PFreeBuffer = ^TFreeBuffer;
  FREEBUFFER = function (lpBuffer: Pointer): ULONG; stdcall;
  {$EXTERNALSYM FREEBUFFER}
  TFreeBuffer = FREEBUFFER;

{ Pointers to MAPI Interfaces --------------------------------------------- }

  PCIID = ^TIID;

{ Extended MAPI Error Information ----------------------------------------- }

  PMapiError = ^TMapiError;
  _MAPIERROR = record
    ulVersion: ULONG;
    lpszError: LPTSTR;
    lpszComponent: LPTSTR;
    ulLowLevelError: ULONG;
    ulContext: ULONG;
  end;
  {$EXTERNALSYM _MAPIERROR}
  TMapiError = _MAPIERROR;
  MAPIERROR = _MAPIERROR;
  {$EXTERNALSYM MAPIERROR}

{ IMAPIAdviseSink Interface ----------------------------------------------- }

{*
 *  Notification event types. The event types can be combined in a bitmask
 *  for filtering. Each one has a parameter structure associated with it:
 *
 *      fnevCriticalError       ERROR_NOTIFICATION
 *      fnevNewMail             NEWMAIL_NOTIFICATION
 *      fnevObjectCreated       OBJECT_NOTIFICATION
 *      fnevObjectDeleted       OBJECT_NOTIFICATION
 *      fnevObjectModified      OBJECT_NOTIFICATION
 *      fnevObjectCopied        OBJECT_NOTIFICATION
 *      fnevSearchComplete      OBJECT_NOTIFICATION
 *      fnevTableModified       TABLE_NOTIFICATION
 *      fnevStatusObjectModified OBJECT_NOTIFICATION
 *
 *      fnevExtended            EXTENDED_NOTIFICATION
}

const
  fnevCriticalError           = ULONG($00000001);
  {$EXTERNALSYM fnevCriticalError}
  fnevNewMail                 = ULONG($00000002);
  {$EXTERNALSYM fnevNewMail}
  fnevObjectCreated           = ULONG($00000004);
  {$EXTERNALSYM fnevObjectCreated}
  fnevObjectDeleted           = ULONG($00000008);
  {$EXTERNALSYM fnevObjectDeleted}
  fnevObjectModified          = ULONG($00000010);
  {$EXTERNALSYM fnevObjectModified}
  fnevObjectMoved             = ULONG($00000020);
  {$EXTERNALSYM fnevObjectMoved}
  fnevObjectCopied            = ULONG($00000040);
  {$EXTERNALSYM fnevObjectCopied}
  fnevSearchComplete          = ULONG($00000080);
  {$EXTERNALSYM fnevSearchComplete}
  fnevTableModified           = ULONG($00000100);
  {$EXTERNALSYM fnevTableModified}
  fnevStatusObjectModified    = ULONG($00000200);
  {$EXTERNALSYM fnevStatusObjectModified}
  fnevReservedForMapi         = ULONG($40000000);
  {$EXTERNALSYM fnevReservedForMapi}
  fnevExtended                = ULONG($80000000);
  {$EXTERNALSYM fnevExtended}

{ TABLE_NOTIFICATION event types passed in ulTableEvent }

  TABLE_CHANGED       = 1;
  {$EXTERNALSYM TABLE_CHANGED}
  TABLE_ERROR         = 2;
  {$EXTERNALSYM TABLE_ERROR}
  TABLE_ROW_ADDED     = 3;
  {$EXTERNALSYM TABLE_ROW_ADDED}
  TABLE_ROW_DELETED   = 4;
  {$EXTERNALSYM TABLE_ROW_DELETED}
  TABLE_ROW_MODIFIED  = 5;
  {$EXTERNALSYM TABLE_ROW_MODIFIED}
  TABLE_SORT_DONE     = 6;
  {$EXTERNALSYM TABLE_SORT_DONE}
  TABLE_RESTRICT_DONE = 7;
  {$EXTERNALSYM TABLE_RESTRICT_DONE}
  TABLE_SETCOL_DONE   = 8;
  {$EXTERNALSYM TABLE_SETCOL_DONE}
  TABLE_RELOAD        = 9;
  {$EXTERNALSYM TABLE_RELOAD}

{ Event Structures }

type
  PErrorNotification = ^TErrorNotification;
  _ERROR_NOTIFICATION = record
    cbEntryID: ULONG;
    lpEntryID: PEntryID;
    scode: SCODE;
    ulFlags: ULONG;                   // 0 or MAPI_UNICODE
    lpMAPIError: PMapiError;          // Detailed error information
  end;
  {$EXTERNALSYM _ERROR_NOTIFICATION}
  TErrorNotification = _ERROR_NOTIFICATION;
  ERROR_NOTIFICATION = _ERROR_NOTIFICATION;
  {$EXTERNALSYM ERROR_NOTIFICATION}

  PNewMailNotification = ^TNewMailNotification;
  _NEWMAIL_NOTIFICATION = record
    cbEntryID: ULONG;
    lpEntryID: PEntryID;             // identifies the new message
    cbParentID: ULONG;
    lpParentID: PEntryID;            // identifies the folder it lives in
    ulFlags: ULONG;                  // 0 or MAPI_UNICODE
    lpszMessageClass: LPTSTR;        // message class (UNICODE or string8)
    ulMessageFlags: ULONG;           // copy of PR_MESSAGE_FLAGS
  end;
  {$EXTERNALSYM _NEWMAIL_NOTIFICATION}
  TNewMailNotification = _NEWMAIL_NOTIFICATION;
  NEWMAIL_NOTIFICATION = _NEWMAIL_NOTIFICATION;
  {$EXTERNALSYM NEWMAIL_NOTIFICATION}

  PObjectNotification = ^TObjectNotification;
  _OBJECT_NOTIFICATION = record
    cbEntryID: ULONG;
    lpEntryID: PEntryID;                 // EntryID of object
    ulObjType: ULONG;                    // Type of object
    cbParentID: ULONG;
    lpParentID: PEntryID;                // EntryID of parent object
    cbOldID: ULONG;
    lpOldID: PEntryID;                   // EntryID of old object
    cbOldParentID: ULONG;
    lpOldParentID: PEntryID;             // EntryID of old parent
    lpPropTagArray: PSPropTagArray;
  end;
  {$EXTERNALSYM _OBJECT_NOTIFICATION}
  TObjectNotification = _OBJECT_NOTIFICATION;
  OBJECT_NOTIFICATION = _OBJECT_NOTIFICATION;
  {$EXTERNALSYM OBJECT_NOTIFICATION}

  PTableNotification = ^TTableNotification;
  _TABLE_NOTIFICATION = record
    ulTableEvent: ULONG;                 // Identifies WHICH table event
    hResult: HRESULT;                    // Value for TABLE_ERROR
    propIndex: TSPropValue;              // This row's "index property"
    propPrior: TSPropValue;              // Preceding row's "index property"
    row: TSRow;                          // New data of added/modified row
    ulPad: ULONG;                        // Force to 8-byte boundary
  end;
  {$EXTERNALSYM _TABLE_NOTIFICATION}
  TTableNotification = _TABLE_NOTIFICATION;
  TABLE_NOTIFICATION = _TABLE_NOTIFICATION;
  {$EXTERNALSYM TABLE_NOTIFICATION}

  PExtendedNotification = ^TExtendedNotification;
  _EXTENDED_NOTIFICATION = record
    ulEvent: ULONG;                       // extended event code
    cb: ULONG;                            // size of event parameters
    pbEventParameters: Pointer;           // event parameters
  end;
  {$EXTERNALSYM _EXTENDED_NOTIFICATION}
  TExtendedNotification = _EXTENDED_NOTIFICATION;
  EXTENDED_NOTIFICATION = _EXTENDED_NOTIFICATION;
  {$EXTERNALSYM EXTENDED_NOTIFICATION}

  PStatusObjectNotification = ^TStatusObjectNotification;
  STATUS_OBJECT_NOTIFICATION = record
    cbEntryID: ULONG;
    lpEntryID: PEntryID;
    cValues: ULONG;
    lpPropVals: PSPropValue;
  end;
  TStatusObjectNotification = STATUS_OBJECT_NOTIFICATION;
  {$EXTERNALSYM STATUS_OBJECT_NOTIFICATION}

  PNotification = ^TNotification;
  _NOTIFICATION = record
    ulEventType: ULONG;           // notification type, i.e. fnevSomething
    ulAlignPad: ULONG;            // Force to 8-byte boundary
    case Integer of
      1: (err: TErrorNotification;);
      2: (newmail: TNewMailNotification;);
      3: (obj: TObjectNotification;);
      4: (tab: TTableNotification;);
      5: (ext: TExtendedNotification;);
      6: (statobj: TStatusObjectNotification;);
  end;
  {$EXTERNALSYM _NOTIFICATION}
  TNotification = _NOTIFICATION;
  NOTIFICATION = _NOTIFICATION;
  {$EXTERNALSYM NOTIFICATION}

{ Interface used for registering and issuing notification callbacks. }

  IMAPIAdviseSink = interface(IUnknown)
    function OnNotify(cNotif: ULONG; lpNotifications: PNotification): ULONG; stdcall;
  end;
  {$EXTERNALSYM IMAPIAdviseSink}

{ Callback function type for MAPIAllocAdviseSink }

  PNotifyCallback = ^TNotifyCallback;
  NOTIFCALLBACK = function (lpvContext: Pointer; cNotification: ULONG;
    lpNotifications: PNotification): Integer; stdcall;
  {$EXTERNALSYM NOTIFCALLBACK}
  TNotifyCallback = NOTIFCALLBACK;

{ IMAPIProgress Interface ------------------------------------------------- }

{ Flag values for the progress indicator }

const
  MAPI_TOP_LEVEL      = ULONG($00000001);
  {$EXTERNALSYM MAPI_TOP_LEVEL}

type
  IMAPIProgress = interface(IUnknown)
    function Progress(ulValue, ulCount, ulTotal: ULONG): HResult; stdcall;
    function GetFlags(var lpulFlags: ULONG): HResult; stdcall;
    function GetMax(var lpulMax: ULONG): HResult; stdcall;
    function GetMin(var lpulMin: ULONG): HResult; stdcall;
    function SetLimits(lpulMin, lpulMax, lpulFlags: PULONG): HResult; stdcall;
  end;
  {$EXTERNALSYM IMAPIProgress}

{ IMAPIProp Interface ----------------------------------------------------- }

const
  MAPI_ERROR_VERSION      = $00000000;
  {$EXTERNALSYM MAPI_ERROR_VERSION}

{ SaveChanges }

  KEEP_OPEN_READONLY      = ULONG($00000001);
  {$EXTERNALSYM KEEP_OPEN_READONLY}
  KEEP_OPEN_READWRITE     = ULONG($00000002);
  {$EXTERNALSYM KEEP_OPEN_READWRITE}
  FORCE_SAVE              = ULONG($00000004);
  {$EXTERNALSYM FORCE_SAVE}
// define MAPI_DEFERRED_ERRORS  ((ULONG) 0x00000008) below

{ OpenProperty  - ulFlags }
//***** MAPI_MODIFY             ((ULONG) 0x00000001) above
  MAPI_CREATE             = ULONG($00000002);
  {$EXTERNALSYM MAPI_CREATE}
  STREAM_APPEND           = ULONG($00000004);
  {$EXTERNALSYM STREAM_APPEND}
//***** MAPI_DEFERRED_ERRORS    ((ULONG) 0x00000008) below

{ OpenProperty  - ulInterfaceOptions, IID_IMAPITable }
//***** MAPI_UNICODE            ((ULONG) 0x80000000) above

{ CopyTo, CopyProps }

  MAPI_MOVE               = ULONG($00000001);
  {$EXTERNALSYM MAPI_MOVE}
  MAPI_NOREPLACE          = ULONG($00000002);
  {$EXTERNALSYM MAPI_NOREPLACE}
  MAPI_DECLINE_OK         = ULONG($00000004);
  {$EXTERNALSYM MAPI_DECLINE_OK}

  MAPI_DIALOG             = ULONG($00000008);
  {$EXTERNALSYM MAPI_DIALOG}

  MAPI_USE_DEFAULT        = $00000040;  // Use default profile in logon
  {$EXTERNALSYM MAPI_USE_DEFAULT}

{ Flags used in GetIDsFromNames }
//***** MAPI_CREATE             ((ULONG) 0x00000002) above

{ Flags used in GetNamesFromIDs  (bit fields) }
  MAPI_NO_STRINGS         = ULONG($00000001);
  {$EXTERNALSYM MAPI_NO_STRINGS}
  MAPI_NO_IDS             = ULONG($00000002);
  {$EXTERNALSYM MAPI_NO_IDS}

{ Union discriminator }
  MNID_ID                 = 0;
  {$EXTERNALSYM MNID_ID}
  MNID_STRING             = 1;
  {$EXTERNALSYM MNID_STRING}

type
  PMapiNameID = ^TMapiNameID;
  _MAPINAMEID = record
    lpguid: PGUID;
    ulKind: ULONG;
    case Integer of
      MNID_ID: (lID: LongInt;);
      MNID_STRING: (lpwstrName: LPWSTR;);
  end;
  {$EXTERNALSYM _MAPINAMEID}
  TMapiNameID = _MAPINAMEID;
  MAPINAMEID = _MAPINAMEID;
  {$EXTERNALSYM MAPINAMEID}

  IMAPIProp = interface(IUnknown)
    function GetLastError(hResult: HRESULT; ulFlags: ULONG;
      var lppMAPIError: TMapiError): HResult; stdcall;
    function SaveChanges(ulFlags: ULONG): HResult; stdcall;
    function GetProps(lpPropTagArray: PSPropTagArray; ulFlags: ULONG;
      lpcValues: PULONG; var lppPropArray: PSPropValue): HResult; stdcall;
    function GetPropList(ulFlags: ULONG; var lppPropTagArray: PSPropTagArray): HResult; stdcall;
    function OpenProperty(ulPropTag: ULONG; const lpiid: TIID;
      ulInterfaceOptions, ulFlags: ULONG; out lppUnk: IUnknown): HResult; stdcall;
    function SetProps(cValues: ULONG; lpPropArray: PSPropValue;
      lppProblems: PSPropProblemArray): HResult; stdcall;
    function DeleteProps(lpPropTagArray: PSPropTagArray;
      lppProblems: PSPropProblemArray): HResult; stdcall;
    function CopyTo(ciidExclude: ULONG; rgiidExclude: PCIID;
      lpExcludeProps: PSPropTagArray; ulUIParam: ULONG; lpProgress: IMAPIProgress;
      lpInterface: PIID; lpDestObj: Pointer; ulFlags: ULONG;
      lppProblems: PSPropProblemArray): HResult; stdcall;
    function CopyProps(lpIncludeProps: PSPropTagArray; ulUIParam: ULONG;
      lpProgress: IMAPIProgress; lpInterface: PIID; lpDestObj: Pointer;
      ulFlags: ULONG; lppProblems: PSPropProblemArray): HResult; stdcall;
    function GetNamesFromIDs(lppPropTags: PSPropTagArray; lpPropSetGuid: PGUID;
      ulFlags: ULONG; var lpcPropNames: ULONG; var lpppPropNames: TMapiNameID): HResult; stdcall;
    function GetIDsFromNames(cPropNames: ULONG; lppPropNames: PMapiNameID;
      ulFlags: ULONG; var lppPropTags: PSPropTagArray): HResult; stdcall;
  end;
  {$EXTERNALSYM IMAPIProp}

{ IMAPITable Interface ---------------------------------------------------- }

{ Table status }

const
  TBLSTAT_COMPLETE            = ULONG(0);
  {$EXTERNALSYM TBLSTAT_COMPLETE}
  TBLSTAT_QCHANGED            = ULONG(7);
  {$EXTERNALSYM TBLSTAT_QCHANGED}
  TBLSTAT_SORTING             = ULONG(9);
  {$EXTERNALSYM TBLSTAT_SORTING}
  TBLSTAT_SORT_ERROR          = ULONG(10);
  {$EXTERNALSYM TBLSTAT_SORT_ERROR}
  TBLSTAT_SETTING_COLS        = ULONG(11);
  {$EXTERNALSYM TBLSTAT_SETTING_COLS}
  TBLSTAT_SETCOL_ERROR        = ULONG(13);
  {$EXTERNALSYM TBLSTAT_SETCOL_ERROR}
  TBLSTAT_RESTRICTING         = ULONG(14);
  {$EXTERNALSYM TBLSTAT_RESTRICTING}
  TBLSTAT_RESTRICT_ERROR      = ULONG(15);
  {$EXTERNALSYM TBLSTAT_RESTRICT_ERROR}

{ Table Type }

  TBLTYPE_SNAPSHOT            = ULONG(0);
  {$EXTERNALSYM TBLTYPE_SNAPSHOT}
  TBLTYPE_KEYSET              = ULONG(1);
  {$EXTERNALSYM TBLTYPE_KEYSET}
  TBLTYPE_DYNAMIC             = ULONG(2);
  {$EXTERNALSYM TBLTYPE_DYNAMIC}

{ Sort order }

{ bit 0: set if descending, clear if ascending }

  TABLE_SORT_ASCEND       = ULONG($00000000);
  {$EXTERNALSYM TABLE_SORT_ASCEND}
  TABLE_SORT_DESCEND      = ULONG($00000001);
  {$EXTERNALSYM TABLE_SORT_DESCEND}
  TABLE_SORT_COMBINE      = ULONG($00000002);
  {$EXTERNALSYM TABLE_SORT_COMBINE}

{ Data structures }

type
  PSSortOrder = ^TSSortOrder;
  _SSortOrder = record
    ulPropTag: ULONG;             // Column to sort on
    ulOrder: ULONG;               // Ascending, descending, combine to left
  end;
  {$EXTERNALSYM _SSortOrder}
  TSSortOrder = _SSortOrder;
  SSortOrder = _SSortOrder;
  {$EXTERNALSYM SSortOrder}

  PSSortOrderSet = ^TSSortOrderSet;
  _SSortOrderSet = record
    cSorts: ULONG;                // Number of sort columns in aSort below
    cCategories: ULONG;           // 0 for non-categorized, up to cSorts
    cExpanded: ULONG;             // 0 if no categories start expanded,
                                  //      up to cExpanded
    aSort: array[0..MAPI_DIM-1] of TSSortOrder;  // The sort orders
  end;
  {$EXTERNALSYM _SSortOrderSet}
  TSSortOrderSet = _SSortOrderSet;
  SSortOrderSet = _SSortOrderSet;
  {$EXTERNALSYM SSortOrderSet}

(*!!!
#define CbNewSSortOrderSet(_csort) \
    (offsetof(SSortOrderSet,aSort) + (_csort)*sizeof(SSortOrder))
#define CbSSortOrderSet(_lpset) \
    (offsetof(SSortOrderSet,aSort) + \
    (UINT)((_lpset)->cSorts*sizeof(SSortOrder)))
#define SizedSSortOrderSet(_csort, _name) \
struct _SSortOrderSet_ ## _name \
{ \
    ULONG           cSorts;         \
    ULONG           cCategories;    \
    ULONG           cExpanded;      \
    SSortOrder      aSort[_csort];  \
} _name
*)

  PBookMark = ^TBookMark;
  BOOKMARK = ULONG;
  {$EXTERNALSYM BOOKMARK}
  TBookMark = BOOKMARK;

const
  BOOKMARK_BEGINNING  = TBookMark(0);      // Before first row
  {$EXTERNALSYM BOOKMARK_BEGINNING}
  BOOKMARK_CURRENT    = TBookMark(1);      // Before current row
  {$EXTERNALSYM BOOKMARK_CURRENT}
  BOOKMARK_END        = TBookMark(2);      // After last row
  {$EXTERNALSYM BOOKMARK_END}

{ Fuzzy Level }

  FL_FULLSTRING        = ULONG($00000000);
  {$EXTERNALSYM FL_FULLSTRING}
  FL_SUBSTRING         = ULONG($00000001);
  {$EXTERNALSYM FL_SUBSTRING}
  FL_PREFIX            = ULONG($00000002);
  {$EXTERNALSYM FL_PREFIX}

  FL_IGNORECASE        = ULONG($00010000);
  {$EXTERNALSYM FL_IGNORECASE}
  FL_IGNORENONSPACE    = ULONG($00020000);
  {$EXTERNALSYM FL_IGNORENONSPACE}
  FL_LOOSE             = ULONG($00040000);
  {$EXTERNALSYM FL_LOOSE}

{ Restrictions }

{ Restriction types }

const
  RES_AND              = ULONG($00000000);
  {$EXTERNALSYM RES_AND}
  RES_OR               = ULONG($00000001);
  {$EXTERNALSYM RES_OR}
  RES_NOT              = ULONG($00000002);
  {$EXTERNALSYM RES_NOT}
  RES_CONTENT          = ULONG($00000003);
  {$EXTERNALSYM RES_CONTENT}
  RES_PROPERTY         = ULONG($00000004);
  {$EXTERNALSYM RES_PROPERTY}
  RES_COMPAREPROPS     = ULONG($00000005);
  {$EXTERNALSYM RES_COMPAREPROPS}
  RES_BITMASK          = ULONG($00000006);
  {$EXTERNALSYM RES_BITMASK}
  RES_SIZE             = ULONG($00000007);
  {$EXTERNALSYM RES_SIZE}
  RES_EXIST            = ULONG($00000008);
  {$EXTERNALSYM RES_EXIST}
  RES_SUBRESTRICTION   = ULONG($00000009);
  {$EXTERNALSYM RES_SUBRESTRICTION}
  RES_COMMENT          = ULONG($0000000A);
  {$EXTERNALSYM RES_COMMENT}

{ Relational operators. These apply to all property comparison restrictions. }

  RELOP_LT         = ULONG(0);     // <
  {$EXTERNALSYM RELOP_LT}
  RELOP_LE         = ULONG(1);     // <=
  {$EXTERNALSYM RELOP_LE}
  RELOP_GT         = ULONG(2);     // >
  {$EXTERNALSYM RELOP_GT}
  RELOP_GE         = ULONG(3);     // >=
  {$EXTERNALSYM RELOP_GE}
  RELOP_EQ         = ULONG(4);     // ==
  {$EXTERNALSYM RELOP_EQ}
  RELOP_NE         = ULONG(5);     // !=
  {$EXTERNALSYM RELOP_NE}
  RELOP_RE         = ULONG(6);     // LIKE (Regular expression)
  {$EXTERNALSYM RELOP_RE}

{ Bitmask operators, for RES_BITMASK only. }

  BMR_EQZ          = ULONG(0);     // ==0
  {$EXTERNALSYM BMR_EQZ}
  BMR_NEZ          = ULONG(1);     // !=0
  {$EXTERNALSYM BMR_NEZ}

{ Subobject identifiers for RES_SUBRESTRICTION only. See MAPITAGS.H. }

// #define PR_MESSAGE_RECIPIENTS  PROP_TAG(PT_OBJECT,0x0E12)
// #define PR_MESSAGE_ATTACHMENTS PROP_TAG(PT_OBJECT,0x0E13)

type

  PSRestriction = ^TSRestriction;

  PSAndRestriction = ^TSAndRestriction;
  _SAndRestriction = record
    cRes: ULONG;
    lpRes: PSRestriction;
  end;
  {$EXTERNALSYM _SAndRestriction}
  TSAndRestriction = _SAndRestriction;
  SAndRestriction = _SAndRestriction;
  {$EXTERNALSYM SAndRestriction}

  PSOrRestriction = ^TSOrRestriction;
  _SOrRestriction = record
    cRes: ULONG;
    lpRes: PSRestriction;
  end;
  {$EXTERNALSYM _SOrRestriction}
  TSOrRestriction = _SOrRestriction;
  SOrRestriction = _SOrRestriction;
  {$EXTERNALSYM SOrRestriction}

  PSNotRestriction = ^TSNotRestriction;
  _SNotRestriction = record
    ulReserved: ULONG;
    lpRes: PSRestriction;
  end;
  {$EXTERNALSYM _SNotRestriction}
  TSNotRestriction = _SNotRestriction;
  SNotRestriction = _SNotRestriction;
  {$EXTERNALSYM SNotRestriction}

  PSContentRestriction = ^TSContentRestriction;
  _SContentRestriction = record
    ulFuzzyLevel: ULONG;
    ulPropTag: ULONG;
    lpProp: PSPropValue;
  end;
  {$EXTERNALSYM _SContentRestriction}
  TSContentRestriction = _SContentRestriction;
  SContentRestriction = _SContentRestriction;
  {$EXTERNALSYM SContentRestriction}

  PSBitMaskRestriction = ^TSBitMaskRestriction;
  _SBitMaskRestriction = record
    relBMR: ULONG;
    ulPropTag: ULONG;
    ulMask: ULONG;
  end;
  {$EXTERNALSYM _SBitMaskRestriction}
  TSBitMaskRestriction = _SBitMaskRestriction;
  SBitMaskRestriction = _SBitMaskRestriction;
  {$EXTERNALSYM SBitMaskRestriction}

  PSPropertyRestriction = ^TSPropertyRestriction;
  _SPropertyRestriction = record
    relop: ULONG;
    ulPropTag: ULONG;
    lpProp: PSPropValue;
  end;
  {$EXTERNALSYM _SPropertyRestriction}
  TSPropertyRestriction = _SPropertyRestriction;
  SPropertyRestriction = _SPropertyRestriction;
  {$EXTERNALSYM SPropertyRestriction}

  PSComparePropsRestriction = ^TSComparePropsRestriction;
  _SComparePropsRestriction = record
    relop: ULONG;
    ulPropTag1: ULONG;
    ulPropTag2: ULONG;
  end;
  {$EXTERNALSYM _SComparePropsRestriction}
  TSComparePropsRestriction = _SComparePropsRestriction;
  SComparePropsRestriction = _SComparePropsRestriction;
  {$EXTERNALSYM SComparePropsRestriction}

  PSSizeRestriction = ^TSSizeRestriction;
  _SSizeRestriction = record
    relop: ULONG;
    ulPropTag: ULONG;
    cb: ULONG;
  end;
  {$EXTERNALSYM _SSizeRestriction}
  TSSizeRestriction = _SSizeRestriction;
  SSizeRestriction = _SSizeRestriction;
  {$EXTERNALSYM SSizeRestriction}

  PSExistRestriction = ^TSExistRestriction;
  _SExistRestriction = record
    ulReserved1: ULONG;
    ulPropTag: ULONG;
    ulReserved2: ULONG;
  end;
  {$EXTERNALSYM _SExistRestriction}
  TSExistRestriction = _SExistRestriction;
  SExistRestriction = _SExistRestriction;
  {$EXTERNALSYM SExistRestriction}

  PSSubRestriction = ^TSSubRestriction;
  _SSubRestriction = record
    ulSubObject: ULONG;
    lpRes: PSRestriction;
  end;
  {$EXTERNALSYM _SSubRestriction}
  TSSubRestriction = _SSubRestriction;
  SSubRestriction = _SSubRestriction;
  {$EXTERNALSYM SSubRestriction}

  PSCommentRestriction = ^TSCommentRestriction;
  _SCommentRestriction = record
    cValues: ULONG;     // # of properties in lpProp *:
    lpRes: PSRestriction;
    lpProp: PSPropValue;
  end;
  {$EXTERNALSYM _SCommentRestriction}
  TSCommentRestriction = _SCommentRestriction;
  SCommentRestriction = _SCommentRestriction;
  {$EXTERNALSYM SCommentRestriction}

  _SRestriction = record
    rt: ULONG;         // Restriction type
    case Integer of
      1: (resCompareProps: SComparePropsRestriction;);    // first
      2: (resAnd: TSAndRestriction;);
      3: (resOr: TSOrRestriction;);
      4: (resNot: TSNotRestriction;);
      5: (resContent: TSContentRestriction;);
      6: (resProperty: TSPropertyRestriction;);
      7: (resBitMask: TSBitMaskRestriction;);
      8: (resSize: TSSizeRestriction;);
      9: (resExist: TSExistRestriction;);
     10: (resSub: TSSubRestriction;);
     11: (resComment: TSCommentRestriction;);
  end;
  {$EXTERNALSYM _SRestriction}
  TSRestriction = _SRestriction;

{ SComparePropsRestriction is first in the union so that static initializations }
{ of 3-value restriction work }

{ Flags of the methods of IMAPITable }

{ QueryColumn }

const
  TBL_ALL_COLUMNS     = ULONG($00000001);
  {$EXTERNALSYM TBL_ALL_COLUMNS}

{ QueryRows }
{ Possible values for PR_ROW_TYPE (for categorization) }

  TBL_LEAF_ROW            = ULONG(1);
  {$EXTERNALSYM TBL_LEAF_ROW}
  TBL_EMPTY_CATEGORY      = ULONG(2);
  {$EXTERNALSYM TBL_EMPTY_CATEGORY}
  TBL_EXPANDED_CATEGORY   = ULONG(3);
  {$EXTERNALSYM TBL_EXPANDED_CATEGORY}
  TBL_COLLAPSED_CATEGORY  = ULONG(4);
  {$EXTERNALSYM TBL_COLLAPSED_CATEGORY}

{ Table wait flag }

  TBL_NOWAIT              = ULONG($00000001);
  {$EXTERNALSYM TBL_NOWAIT}
{ alternative name for TBL_NOWAIT }
  TBL_ASYNC               = ULONG($00000001);
  {$EXTERNALSYM TBL_ASYNC}
  TBL_BATCH               = ULONG($00000002);
  {$EXTERNALSYM TBL_BATCH}

{ FindRow }

  DIR_BACKWARD            = ULONG($00000001);
  {$EXTERNALSYM DIR_BACKWARD}

{ Table cursor states }

  TBL_NOADVANCE           = ULONG($00000001);
  {$EXTERNALSYM TBL_NOADVANCE}

type
  IMAPITable = interface(IUnknown)
    function GetLastError(hResult: HRESULT; ulFlags: ULONG;
      var lppMAPIError: TMapiError): HResult; stdcall;
    function Advise(ulEventMask: ULONG; lpAdviseSink: IMAPIAdviseSink;
      var lpulConnection: ULONG): HResult; stdcall;
    function Unadvise(ulConnection: ULONG): HResult; stdcall;
    function GetStatus(var lpulTableStatus, lpulTableType: ULONG): HResult; stdcall;
    function SetColumns(lpPropTagArray: PSPropTagArray; ulFlags: ULONG): HResult; stdcall;
    function QueryColumns(ulFlags: ULONG; var lpPropTagArray: PSPropTagArray): HResult; stdcall;
    function GetRowCount(ulFlags: ULONG; var lpulCount: ULONG): HResult; stdcall;
    function SeekRow(bkOrigin: TBookMark; lRowCount: LongInt; lplRowsSought: PLongInt): HResult; stdcall;
    function SeekRowApprox(ulNumerator, ulDenominator: ULONG): HResult; stdcall;
    function QueryPosition(var lpulRow, lpulNumerator, lpulDenominator: ULONG): HResult; stdcall;
    function FindRow(lpRestriction: PSRestriction; bkOrigin: TBookMark;
      ulFlags: ULONG): HResult; stdcall;
    function Restrict(lpRestriction: PSRestriction; ulFlags: ULONG): HResult; stdcall;
    function CreateBookmark(var lpbkPosition: TBookMark): HResult; stdcall;
    function FreeBookmark(bkPosition: TBookMark): HResult; stdcall;
    function SortTable(lpSortCriteria: PSSortOrderSet; ulFlags: ULONG): HResult; stdcall;
    function QuerySortOrder(var lppSortCriteria: PSSortOrderSet): HResult; stdcall;
    function QueryRows(lRowCount: LongInt; ulFlags: ULONG; var lppRows: PSRowSet): HResult; stdcall;
    function Abort: HResult; stdcall;
    function ExpandRow(cbInstanceKey: ULONG; pbInstanceKey: Pointer;
      ulRowCount, ulFlags: ULONG; lppRows: PSRowSet; var lpulMoreRows: ULONG): HResult; stdcall;
    function CollapseRow(cbInstanceKey: ULONG; pbInstanceKey: Pointer;
      ulFlags: ULONG; var lpulRowCount: ULONG): HResult; stdcall;
    function WaitForCompletion(ulFlags, ulTimeout: ULONG; lpulTableStatus: PULONG): HResult; stdcall;
    function GetCollapseState(ulFlags, cbInstanceKey: ULONG; lpbInstanceKey: Pointer;
      var lpcbCollapseState: ULONG; var lppbCollapseState: Pointer): HResult; stdcall;
    function SetCollapseState(ulFlags, cbCollapseState: ULONG; pbCollapseState: Pointer;
      var lpbkLocation: TBookMark): HResult; stdcall;
  end;
  {$EXTERNALSYM IMAPITable}

{ IProfSect Interface ----------------------------------------------------- }

{ Standard section for public profile properties }

const
  PS_PROFILE_PROPERTIES_INIT: array[0..15] of Byte =
   ($98, $15, $AC, $08, $AA, $B0, $10, $1A,
    $8C, $93, $08, $00, $2B, $2A, $56, $C2);
  {$EXTERNALSYM PS_PROFILE_PROPERTIES_INIT}

type
  IProfSect = interface(IMAPIProp)
  end;
  {$EXTERNALSYM IProfSect}

{ IMAPIStatus Interface --------------------------------------------------- }

{ Values for PR_RESOURCE_TYPE, _METHODS, _FLAGS }

const
  MAPI_STORE_PROVIDER     = ULONG(33);    // Message Store
  {$EXTERNALSYM MAPI_STORE_PROVIDER}
  MAPI_AB                 = ULONG(34);    // Address Book
  {$EXTERNALSYM MAPI_AB}
  MAPI_AB_PROVIDER        = ULONG(35);    // Address Book Provider
  {$EXTERNALSYM MAPI_AB_PROVIDER}
  MAPI_TRANSPORT_PROVIDER = ULONG(36);    // Transport Provider
  {$EXTERNALSYM MAPI_TRANSPORT_PROVIDER}
  MAPI_SPOOLER            = ULONG(37);    // Message Spooler
  {$EXTERNALSYM MAPI_SPOOLER}
  MAPI_PROFILE_PROVIDER   = ULONG(38);    // Profile Provider
  {$EXTERNALSYM MAPI_PROFILE_PROVIDER}
  MAPI_SUBSYSTEM          = ULONG(39);    // Overall Subsystem Status
  {$EXTERNALSYM MAPI_SUBSYSTEM}
  MAPI_HOOK_PROVIDER      = ULONG(40);    // Spooler Hook
  {$EXTERNALSYM MAPI_HOOK_PROVIDER}

  STATUS_VALIDATE_STATE   = ULONG($00000001);
  {$EXTERNALSYM STATUS_VALIDATE_STATE}
  STATUS_SETTINGS_DIALOG  = ULONG($00000002);
  {$EXTERNALSYM STATUS_SETTINGS_DIALOG}
  STATUS_CHANGE_PASSWORD  = ULONG($00000004);
  {$EXTERNALSYM STATUS_CHANGE_PASSWORD}
  STATUS_FLUSH_QUEUES     = ULONG($00000008);
  {$EXTERNALSYM STATUS_FLUSH_QUEUES}

  STATUS_DEFAULT_OUTBOUND = ULONG($00000001);
  {$EXTERNALSYM STATUS_DEFAULT_OUTBOUND}
  STATUS_DEFAULT_STORE    = ULONG($00000002);
  {$EXTERNALSYM STATUS_DEFAULT_STORE}
  STATUS_PRIMARY_IDENTITY = ULONG($00000004);
  {$EXTERNALSYM STATUS_PRIMARY_IDENTITY}
  STATUS_SIMPLE_STORE     = ULONG($00000008);
  {$EXTERNALSYM STATUS_SIMPLE_STORE}
  STATUS_XP_PREFER_LAST   = ULONG($00000010);
  {$EXTERNALSYM STATUS_XP_PREFER_LAST}
  STATUS_NO_PRIMARY_IDENTITY = ULONG($00000020);
  {$EXTERNALSYM STATUS_NO_PRIMARY_IDENTITY}
  STATUS_NO_DEFAULT_STORE = ULONG($00000040);
  {$EXTERNALSYM STATUS_NO_DEFAULT_STORE}
  STATUS_TEMP_SECTION     = ULONG($00000080);
  {$EXTERNALSYM STATUS_TEMP_SECTION}
  STATUS_OWN_STORE        = ULONG($00000100);
  {$EXTERNALSYM STATUS_OWN_STORE}
//***** HOOK_INBOUND            ((ULONG) 0x00000200) Defined in MAPIHOOK.H
//***** HOOK_OUTBOUND           ((ULONG) 0x00000400) Defined in MAPIHOOK.H
  STATUS_NEED_IPM_TREE    = ULONG($00000800);
  {$EXTERNALSYM STATUS_NEED_IPM_TREE}
  STATUS_PRIMARY_STORE    = ULONG($00001000);
  {$EXTERNALSYM STATUS_PRIMARY_STORE}
  STATUS_SECONDARY_STORE  = ULONG($00002000);
  {$EXTERNALSYM STATUS_SECONDARY_STORE}


{* PR_STATUS_CODE bit. Low 16 bits for common values; High 16 bits
 * for provider type-specific values. (DCR 304)
}

  STATUS_AVAILABLE        = ULONG($00000001);
  {$EXTERNALSYM STATUS_AVAILABLE}
  STATUS_OFFLINE          = ULONG($00000002);
  {$EXTERNALSYM STATUS_OFFLINE}
  STATUS_FAILURE          = ULONG($00000004);
  {$EXTERNALSYM STATUS_FAILURE}

{ Transport values of PR_STATUS_CODE }

  STATUS_INBOUND_ENABLED  = ULONG($00010000);
  {$EXTERNALSYM STATUS_INBOUND_ENABLED}
  STATUS_INBOUND_ACTIVE   = ULONG($00020000);
  {$EXTERNALSYM STATUS_INBOUND_ACTIVE}
  STATUS_INBOUND_FLUSH    = ULONG($00040000);
  {$EXTERNALSYM STATUS_INBOUND_FLUSH}
  STATUS_OUTBOUND_ENABLED = ULONG($00100000);
  {$EXTERNALSYM STATUS_OUTBOUND_ENABLED}
  STATUS_OUTBOUND_ACTIVE  = ULONG($00200000);
  {$EXTERNALSYM STATUS_OUTBOUND_ACTIVE}
  STATUS_OUTBOUND_FLUSH   = ULONG($00400000);
  {$EXTERNALSYM STATUS_OUTBOUND_FLUSH}
  STATUS_REMOTE_ACCESS    = ULONG($00800000);
  {$EXTERNALSYM STATUS_REMOTE_ACCESS}

{ ValidateState flags }

  SUPPRESS_UI                 = ULONG($00000001);
  {$EXTERNALSYM SUPPRESS_UI}
  REFRESH_XP_HEADER_CACHE     = ULONG($00010000);
  {$EXTERNALSYM REFRESH_XP_HEADER_CACHE}
  PROCESS_XP_HEADER_CACHE     = ULONG($00020000);
  {$EXTERNALSYM PROCESS_XP_HEADER_CACHE}
  FORCE_XP_CONNECT            = ULONG($00040000);
  {$EXTERNALSYM FORCE_XP_CONNECT}
  FORCE_XP_DISCONNECT         = ULONG($00080000);
  {$EXTERNALSYM FORCE_XP_DISCONNECT}
  CONFIG_CHANGED              = ULONG($00100000);
  {$EXTERNALSYM CONFIG_CHANGED}
  ABORT_XP_HEADER_OPERATION   = ULONG($00200000);
  {$EXTERNALSYM ABORT_XP_HEADER_OPERATION}
  SHOW_XP_SESSION_UI          = ULONG($00400000);
  {$EXTERNALSYM SHOW_XP_SESSION_UI}

{ SettingsDialog flags }

  UI_READONLY     = ULONG($00000001);
  {$EXTERNALSYM UI_READONLY}

{ FlushQueues flags }

  FLUSH_UPLOAD        = ULONG($00000002);
  {$EXTERNALSYM FLUSH_UPLOAD}
  FLUSH_DOWNLOAD      = ULONG($00000004);
  {$EXTERNALSYM FLUSH_DOWNLOAD}
  FLUSH_FORCE         = ULONG($00000008);
  {$EXTERNALSYM FLUSH_FORCE}
  FLUSH_NO_UI         = ULONG($00000010);
  {$EXTERNALSYM FLUSH_NO_UI}
  FLUSH_ASYNC_OK      = ULONG($00000020);
  {$EXTERNALSYM FLUSH_ASYNC_OK}

type
  IMAPIStatus = interface(IMAPIProp)
    function ValidateState(ulUIParam, ulFlags: ULONG): HResult; stdcall;
    function SettingsDialog(ulUIParam, ulFlags: ULONG): HResult; stdcall;
    function ChangePassword(lpOldPass, lpNewPass: LPTSTR; ulFlags: ULONG): HResult; stdcall;
    function FlushQueues(ulUIParam, cbTargetTransport: ULONG;
     lpTargetTransport: PEntryID; ulFlags: ULONG): HResult; stdcall;
  end;
  {$EXTERNALSYM IMAPIStatus}

{ IMAPIContainer Interface ------------------------------------------------ }

{ Flags for OpenEntry() }

const
//***** MAPI_MODIFY             ((ULONG) 0x00000001) above
//***** MAPI_DEFERRED_ERRORS    ((ULONG) 0x00000008) below
  MAPI_BEST_ACCESS        = ULONG($00000010);
  {$EXTERNALSYM MAPI_BEST_ACCESS}

{ GetContentsTable() }
//***** MAPI_UNICODE            ((ULONG) 0x80000000) above
//***** MAPI_DEFERRED_ERRORS    ((ULONG) 0x00000008) below
//***** MAPI_ASSOCIATED         ((ULONG) 0x00000040) below
  WAB_LOCAL_CONTAINERS    = $00100000;
  {$EXTERNALSYM WAB_LOCAL_CONTAINERS}
  WAB_PROFILE_CONTENTS    = $00200000;
  {$EXTERNALSYM WAB_PROFILE_CONTENTS}

{ GetHierarchyTable() }
//***** MAPI_UNICODE            ((ULONG) 0x80000000) above
  CONVENIENT_DEPTH        = ULONG($00000001);
//***** MAPI_DEFERRED_ERRORS    ((ULONG) 0x00000008) below

{ GetSearchCriteria }
//***** MAPI_UNICODE            ((ULONG) 0x80000000) above
  SEARCH_RUNNING          = ULONG($00000001);
  {$EXTERNALSYM SEARCH_RUNNING}
  SEARCH_REBUILD          = ULONG($00000002);
  {$EXTERNALSYM SEARCH_REBUILD}
  SEARCH_RECURSIVE        = ULONG($00000004);
  {$EXTERNALSYM SEARCH_RECURSIVE}
  SEARCH_FOREGROUND       = ULONG($00000008);
  {$EXTERNALSYM SEARCH_FOREGROUND}

{ SetSearchCriteria }
  STOP_SEARCH             = ULONG($00000001);
  {$EXTERNALSYM STOP_SEARCH}
  RESTART_SEARCH          = ULONG($00000002);
  {$EXTERNALSYM RESTART_SEARCH}
  RECURSIVE_SEARCH        = ULONG($00000004);
  {$EXTERNALSYM RECURSIVE_SEARCH}
  SHALLOW_SEARCH          = ULONG($00000008);
  {$EXTERNALSYM SHALLOW_SEARCH}
  FOREGROUND_SEARCH       = ULONG($00000010);
  {$EXTERNALSYM FOREGROUND_SEARCH}
  BACKGROUND_SEARCH       = ULONG($00000020);
  {$EXTERNALSYM BACKGROUND_SEARCH}

type
  IMAPIContainer = interface(IMAPIProp)
    function GetContentsTable(ulFlags: ULONG; out lppTable: IMAPITable): HResult; stdcall;
    function GetHierarchyTable(ulFlags: ULONG; out lppTable: IMAPITable): HResult; stdcall;
    function OpenEntry(cbEntryID: ULONG; lpEntryID: PEntryID; lpInterface: PIID;
      ulFlags: ULONG; var lpulObjType: ULONG; out lppUnk: IUnknown): HResult; stdcall;
    function SetSearchCriteria(lpRestriction: PSRestriction;
      lpContainerList: PEntryList; ulSearchFlags: ULONG): HResult; stdcall;
    function GetSearchCriteria(ulFlags: ULONG; var lppRestriction: PSRestriction;
      var lppContainerList: PEntryList; var lpulSearchState: ULONG): HResult; stdcall;
   end;
  {$EXTERNALSYM IMAPIContainer}

{ IABContainer Interface -------------------------------------------------- }

{*  IABContainer PR_CONTAINER_FLAGS values
 *  If AB_UNMODIFIABLE and AB_MODIFIABLE are both set, it means the container
 *  doesn't know if it's modifiable or not, and the client should
 *  try to modify the contents but we won't expect it to work.
 *  If the AB_RECIPIENTS flag is set and neither AB_MODIFIABLE or AB_UNMODIFIABLE
 *  bits are set, it is an error.
}

  PFlagList = ^TFlagList;
  _flaglist = record
    cFlags: ULONG;
    ulFlag: array[0..MAPI_DIM-1] of ULONG;
  end;
  {$EXTERNALSYM _flaglist}
  TFlagList = _flaglist;

{ Container flags }

const
  AB_RECIPIENTS           = ULONG($00000001);
  {$EXTERNALSYM AB_RECIPIENTS}
  AB_SUBCONTAINERS        = ULONG($00000002);
  {$EXTERNALSYM AB_SUBCONTAINERS}
  AB_MODIFIABLE           = ULONG($00000004);
  {$EXTERNALSYM AB_MODIFIABLE}
  AB_UNMODIFIABLE         = ULONG($00000008);
  {$EXTERNALSYM AB_UNMODIFIABLE}
  AB_FIND_ON_OPEN         = ULONG($00000010);
  {$EXTERNALSYM AB_FIND_ON_OPEN}
  AB_NOT_DEFAULT          = ULONG($00000020);
  {$EXTERNALSYM AB_NOT_DEFAULT}

{ CreateEntry() }

  CREATE_CHECK_DUP_STRICT = ULONG($00000001);
  {$EXTERNALSYM CREATE_CHECK_DUP_STRICT}
  CREATE_CHECK_DUP_LOOSE  = ULONG($00000002);
  {$EXTERNALSYM CREATE_CHECK_DUP_LOOSE}
  CREATE_REPLACE          = ULONG($00000004);
  {$EXTERNALSYM CREATE_REPLACE}
  CREATE_MERGE            = ULONG($00000008);
  {$EXTERNALSYM CREATE_MERGE}

{ ResolveNames() - ulFlags }
//***** MAPI_UNICODE            ((ULONG) 0x80000000) above
  WAB_IGNORE_PROFILES     = $00800000;
  {$EXTERNALSYM WAB_IGNORE_PROFILES}

{ ResolveNames() - rgulFlags }
  MAPI_UNRESOLVED         = ULONG($00000000);
  {$EXTERNALSYM MAPI_UNRESOLVED}
  MAPI_AMBIGUOUS          = ULONG($00000001);
  {$EXTERNALSYM MAPI_AMBIGUOUS}
  MAPI_RESOLVED           = ULONG($00000002);
  {$EXTERNALSYM MAPI_RESOLVED}

type
  IABContainer = interface(IMAPIContainer)
    function CreateEntry(cbEntryID: ULONG; lpEntryID: PEntryID;
      ulCreateFlags: ULONG; out lppMAPIPropEntry: IMAPIProp): HResult; stdcall;
    function CopyEntries(lpEntries: PEntryList; ulUIParam: ULONG;
      lpProgress: IMAPIProgress; ulFlags: ULONG): HResult; stdcall;
    function DeleteEntries(lpEntries: PEntryList; ulFlags: ULONG): HResult; stdcall;
    function ResolveNames(lpPropTagArray: PSPropTagArray; ulFlags: ULONG;
      lpAdrList: PAdrList; lpFlagList: PFlagList): HResult; stdcall;
  end;
  {$EXTERNALSYM IABContainer}

{ IMailUser Interface ----------------------------------------------------- }

{*  Any call which can create a one-off entryID (i.e. MAPISupport::CreateOneOff
    or IAddrBook::CreateOneOff) can encode the value for PR_SEND_RICH_INFO by
    passing in the following flag in the ulFlags parameter.  Setting this flag
    indicates that PR_SEND_RICH_INFO will be FALSE.
*}

const
  MAPI_SEND_NO_RICH_INFO      = ULONG($00010000);
  {$EXTERNALSYM MAPI_SEND_NO_RICH_INFO}

{ Values of PR_NDR_DIAG_CODE }

  MAPI_DIAG_NO_DIAGNOSTIC                     = -1;
  {$EXTERNALSYM MAPI_DIAG_NO_DIAGNOSTIC}
  MAPI_DIAG_OR_NAME_UNRECOGNIZED              = 0;
  {$EXTERNALSYM MAPI_DIAG_OR_NAME_UNRECOGNIZED}
  MAPI_DIAG_OR_NAME_AMBIGUOUS                 = 1;
  {$EXTERNALSYM MAPI_DIAG_OR_NAME_AMBIGUOUS}
  MAPI_DIAG_MTS_CONGESTED                     = 2;
  {$EXTERNALSYM MAPI_DIAG_MTS_CONGESTED}
  MAPI_DIAG_LOOP_DETECTED                     = 3;
  {$EXTERNALSYM MAPI_DIAG_LOOP_DETECTED}
  MAPI_DIAG_RECIPIENT_UNAVAILABLE             = 4;
  {$EXTERNALSYM MAPI_DIAG_RECIPIENT_UNAVAILABLE}
  MAPI_DIAG_MAXIMUM_TIME_EXPIRED              = 5;
  {$EXTERNALSYM MAPI_DIAG_MAXIMUM_TIME_EXPIRED}
  MAPI_DIAG_EITS_UNSUPPORTED                  = 6;
  {$EXTERNALSYM MAPI_DIAG_EITS_UNSUPPORTED}
  MAPI_DIAG_CONTENT_TOO_LONG                  = 7;
  {$EXTERNALSYM MAPI_DIAG_CONTENT_TOO_LONG}
  MAPI_DIAG_IMPRACTICAL_TO_CONVERT            = 8;
  {$EXTERNALSYM MAPI_DIAG_IMPRACTICAL_TO_CONVERT}
  MAPI_DIAG_PROHIBITED_TO_CONVERT             = 9;
  {$EXTERNALSYM MAPI_DIAG_PROHIBITED_TO_CONVERT}
  MAPI_DIAG_CONVERSION_UNSUBSCRIBED           = 10;
  {$EXTERNALSYM MAPI_DIAG_CONVERSION_UNSUBSCRIBED}
  MAPI_DIAG_PARAMETERS_INVALID                = 11;
  {$EXTERNALSYM MAPI_DIAG_PARAMETERS_INVALID}
  MAPI_DIAG_CONTENT_SYNTAX_IN_ERROR           = 12;
  {$EXTERNALSYM MAPI_DIAG_CONTENT_SYNTAX_IN_ERROR}
  MAPI_DIAG_LENGTH_CONSTRAINT_VIOLATD         = 13;
  {$EXTERNALSYM MAPI_DIAG_LENGTH_CONSTRAINT_VIOLATD}
  MAPI_DIAG_NUMBER_CONSTRAINT_VIOLATD         = 14;
  {$EXTERNALSYM MAPI_DIAG_NUMBER_CONSTRAINT_VIOLATD}
  MAPI_DIAG_CONTENT_TYPE_UNSUPPORTED          = 15;
  {$EXTERNALSYM MAPI_DIAG_CONTENT_TYPE_UNSUPPORTED}
  MAPI_DIAG_TOO_MANY_RECIPIENTS               = 16;
  {$EXTERNALSYM MAPI_DIAG_TOO_MANY_RECIPIENTS}
  MAPI_DIAG_NO_BILATERAL_AGREEMENT            = 17;
  {$EXTERNALSYM MAPI_DIAG_NO_BILATERAL_AGREEMENT}
  MAPI_DIAG_CRITICAL_FUNC_UNSUPPORTED         = 18;
  {$EXTERNALSYM MAPI_DIAG_CRITICAL_FUNC_UNSUPPORTED}
  MAPI_DIAG_CONVERSION_LOSS_PROHIB            = 19;
  {$EXTERNALSYM MAPI_DIAG_CONVERSION_LOSS_PROHIB}
  MAPI_DIAG_LINE_TOO_LONG                     = 20;
  {$EXTERNALSYM MAPI_DIAG_LINE_TOO_LONG}
  MAPI_DIAG_PAGE_TOO_LONG                     = 21;
  {$EXTERNALSYM MAPI_DIAG_PAGE_TOO_LONG}
  MAPI_DIAG_PICTORIAL_SYMBOL_LOST             = 22;
  {$EXTERNALSYM MAPI_DIAG_PICTORIAL_SYMBOL_LOST}
  MAPI_DIAG_PUNCTUATION_SYMBOL_LOST           = 23;
  {$EXTERNALSYM MAPI_DIAG_PUNCTUATION_SYMBOL_LOST}
  MAPI_DIAG_ALPHABETIC_CHARACTER_LOST         = 24;
  {$EXTERNALSYM MAPI_DIAG_ALPHABETIC_CHARACTER_LOST}
  MAPI_DIAG_MULTIPLE_INFO_LOSSES              = 25;
  {$EXTERNALSYM MAPI_DIAG_MULTIPLE_INFO_LOSSES}
  MAPI_DIAG_REASSIGNMENT_PROHIBITED           = 26;
  {$EXTERNALSYM MAPI_DIAG_REASSIGNMENT_PROHIBITED}
  MAPI_DIAG_REDIRECTION_LOOP_DETECTED         = 27;
  {$EXTERNALSYM MAPI_DIAG_REDIRECTION_LOOP_DETECTED}
  MAPI_DIAG_EXPANSION_PROHIBITED              = 28;
  {$EXTERNALSYM MAPI_DIAG_EXPANSION_PROHIBITED}
  MAPI_DIAG_SUBMISSION_PROHIBITED             = 29;
  {$EXTERNALSYM MAPI_DIAG_SUBMISSION_PROHIBITED}
  MAPI_DIAG_EXPANSION_FAILED                  = 30;
  {$EXTERNALSYM MAPI_DIAG_EXPANSION_FAILED}
  MAPI_DIAG_RENDITION_UNSUPPORTED             = 31;
  {$EXTERNALSYM MAPI_DIAG_RENDITION_UNSUPPORTED}
  MAPI_DIAG_MAIL_ADDRESS_INCORRECT            = 32;
  {$EXTERNALSYM MAPI_DIAG_MAIL_ADDRESS_INCORRECT}
  MAPI_DIAG_MAIL_OFFICE_INCOR_OR_INVD         = 33;
  {$EXTERNALSYM MAPI_DIAG_MAIL_OFFICE_INCOR_OR_INVD}
  MAPI_DIAG_MAIL_ADDRESS_INCOMPLETE           = 34;
  {$EXTERNALSYM MAPI_DIAG_MAIL_ADDRESS_INCOMPLETE}
  MAPI_DIAG_MAIL_RECIPIENT_UNKNOWN            = 35;
  {$EXTERNALSYM MAPI_DIAG_MAIL_RECIPIENT_UNKNOWN}
  MAPI_DIAG_MAIL_RECIPIENT_DECEASED           = 36;
  {$EXTERNALSYM MAPI_DIAG_MAIL_RECIPIENT_DECEASED}
  MAPI_DIAG_MAIL_ORGANIZATION_EXPIRED         = 37;
  {$EXTERNALSYM MAPI_DIAG_MAIL_ORGANIZATION_EXPIRED}
  MAPI_DIAG_MAIL_REFUSED                      = 38;
  {$EXTERNALSYM MAPI_DIAG_MAIL_REFUSED}
  MAPI_DIAG_MAIL_UNCLAIMED                    = 39;
  {$EXTERNALSYM MAPI_DIAG_MAIL_UNCLAIMED}
  MAPI_DIAG_MAIL_RECIPIENT_MOVED              = 40;
  {$EXTERNALSYM MAPI_DIAG_MAIL_RECIPIENT_MOVED}
  MAPI_DIAG_MAIL_RECIPIENT_TRAVELLING         = 41;
  {$EXTERNALSYM MAPI_DIAG_MAIL_RECIPIENT_TRAVELLING}
  MAPI_DIAG_MAIL_RECIPIENT_DEPARTED           = 42;
  {$EXTERNALSYM MAPI_DIAG_MAIL_RECIPIENT_DEPARTED}
  MAPI_DIAG_MAIL_NEW_ADDRESS_UNKNOWN          = 43;
  {$EXTERNALSYM MAPI_DIAG_MAIL_NEW_ADDRESS_UNKNOWN}
  MAPI_DIAG_MAIL_FORWARDING_UNWANTED          = 44;
  {$EXTERNALSYM MAPI_DIAG_MAIL_FORWARDING_UNWANTED}
  MAPI_DIAG_MAIL_FORWARDING_PROHIB            = 45;
  {$EXTERNALSYM MAPI_DIAG_MAIL_FORWARDING_PROHIB}
  MAPI_DIAG_SECURE_MESSAGING_ERROR            = 46;
  {$EXTERNALSYM MAPI_DIAG_SECURE_MESSAGING_ERROR}
  MAPI_DIAG_DOWNGRADING_IMPOSSIBLE            = 47;
  {$EXTERNALSYM MAPI_DIAG_DOWNGRADING_IMPOSSIBLE}

type
  IMailUser = interface(IMAPIProp)
  end;
  {$EXTERNALSYM IMailUser}


{ IDistList Interface ----------------------------------------------------- }

  IDistList = interface(IMAPIContainer)
    function CreateEntry(cbEntryID: ULONG; lpEntryID: PEntryID;
      ulCreateFlags: ULONG; out lppMAPIPropEntry: IMAPIProp): HResult; stdcall;
    function CopyEntries(lpEntries: PEntryList; ulUIParam: ULONG;
      lpProgress: IMAPIProgress; ulFlags: ULONG): HResult; stdcall;
    function DeleteEntries(lpEntries: PEntryList; ulFlags: ULONG): HResult; stdcall;
    function ResolveNames(lpPropTagArray: PSPropTagArray; ulFlags: ULONG;
      lpAdrList: PAdrList; lpFlagList: PFlagList): HResult; stdcall;
  end;
  {$EXTERNALSYM IDistList}

{ IMAPIFolder Interface --------------------------------------------------- }

{ IMAPIFolder folder type (enum) }

const
  FOLDER_ROOT               = ULONG($00000000);
  {$EXTERNALSYM FOLDER_ROOT}
  FOLDER_GENERIC            = ULONG($00000001);
  {$EXTERNALSYM FOLDER_GENERIC}
  FOLDER_SEARCH             = ULONG($00000002);
  {$EXTERNALSYM FOLDER_SEARCH}

{ CreateMessage }
//***** MAPI_DEFERRED_ERRORS    ((ULONG) 0x00000008) below
//***** MAPI_ASSOCIATED         ((ULONG) 0x00000040) below

{ CopyMessages }

  MESSAGE_MOVE              = ULONG($00000001);
  {$EXTERNALSYM MESSAGE_MOVE}
  MESSAGE_DIALOG            = ULONG($00000002);
  {$EXTERNALSYM MESSAGE_DIALOG}
//***** MAPI_DECLINE_OK         ((ULONG) 0x00000004) above

{ CreateFolder }

  OPEN_IF_EXISTS            = ULONG($00000001);
  {$EXTERNALSYM OPEN_IF_EXISTS}
//***** MAPI_DEFERRED_ERRORS    ((ULONG) 0x00000008) below
//***** MAPI_UNICODE            ((ULONG) 0x80000000) above

{ DeleteFolder }

  DEL_MESSAGES              = ULONG($00000001);
  {$EXTERNALSYM DEL_MESSAGES}
  FOLDER_DIALOG             = ULONG($00000002);
  {$EXTERNALSYM FOLDER_DIALOG}
  DEL_FOLDERS               = ULONG($00000004);
  {$EXTERNALSYM DEL_FOLDERS}

{ EmptyFolder }

  DEL_ASSOCIATED            = ULONG($00000008);
  {$EXTERNALSYM DEL_ASSOCIATED}

{ CopyFolder }

  FOLDER_MOVE               = ULONG($00000001);
  {$EXTERNALSYM FOLDER_MOVE}
//***** FOLDER_DIALOG           ((ULONG) 0x00000002) above
//***** MAPI_DECLINE_OK         ((ULONG) 0x00000004) above
  COPY_SUBFOLDERS           = ULONG($00000010);
  {$EXTERNALSYM COPY_SUBFOLDERS}
//***** MAPI_UNICODE            ((ULONG) 0x80000000) above


{ SetReadFlags }

//***** SUPPRESS_RECEIPT        ((ULONG) 0x00000001) below
//***** MESSAGE_DIALOG          ((ULONG) 0x00000002) above
//***** CLEAR_READ_FLAG         ((ULONG) 0x00000004) below
//***** MAPI_DEFERRED_ERRORS    ((ULONG) 0x00000008) below
  GENERATE_RECEIPT_ONLY     = ULONG($00000010);
  {$EXTERNALSYM GENERATE_RECEIPT_ONLY}


{ GetMessageStatus }

  MSGSTATUS_HIGHLIGHTED     = ULONG($00000001);
  {$EXTERNALSYM MSGSTATUS_HIGHLIGHTED}
  MSGSTATUS_TAGGED          = ULONG($00000002);
  {$EXTERNALSYM MSGSTATUS_TAGGED}
  MSGSTATUS_HIDDEN          = ULONG($00000004);
  {$EXTERNALSYM MSGSTATUS_HIDDEN}
  MSGSTATUS_DELMARKED       = ULONG($00000008);
  {$EXTERNALSYM MSGSTATUS_DELMARKED}

{ Bits for remote message status }

  MSGSTATUS_REMOTE_DOWNLOAD     = ULONG($00001000);
  {$EXTERNALSYM MSGSTATUS_REMOTE_DOWNLOAD}
  MSGSTATUS_REMOTE_DELETE       = ULONG($00002000);
  {$EXTERNALSYM MSGSTATUS_REMOTE_DELETE}

{ SaveContentsSort }

  RECURSIVE_SORT            = ULONG($00000002);
  {$EXTERNALSYM RECURSIVE_SORT}

{ PR_STATUS property }

  FLDSTATUS_HIGHLIGHTED     = ULONG($00000001);
  {$EXTERNALSYM FLDSTATUS_HIGHLIGHTED}
  FLDSTATUS_TAGGED          = ULONG($00000002);
  {$EXTERNALSYM FLDSTATUS_TAGGED}
  FLDSTATUS_HIDDEN          = ULONG($00000004);
  {$EXTERNALSYM FLDSTATUS_HIDDEN}
  FLDSTATUS_DELMARKED       = ULONG($00000008);
  {$EXTERNALSYM FLDSTATUS_DELMARKED}


{ IMsgStore Interface ----------------------------------------------------- }

{ PR_STORE_SUPPORT_MASK bits }
const
  STORE_ENTRYID_UNIQUE      = ULONG($00000001);
  {$EXTERNALSYM STORE_ENTRYID_UNIQUE}
  STORE_READONLY            = ULONG($00000002);
  {$EXTERNALSYM STORE_READONLY}
  STORE_SEARCH_OK           = ULONG($00000004);
  {$EXTERNALSYM STORE_SEARCH_OK}
  STORE_MODIFY_OK           = ULONG($00000008);
  {$EXTERNALSYM STORE_MODIFY_OK}
  STORE_CREATE_OK           = ULONG($00000010);
  {$EXTERNALSYM STORE_CREATE_OK}
  STORE_ATTACH_OK           = ULONG($00000020);
  {$EXTERNALSYM STORE_ATTACH_OK}
  STORE_OLE_OK              = ULONG($00000040);
  {$EXTERNALSYM STORE_OLE_OK}
  STORE_SUBMIT_OK           = ULONG($00000080);
  {$EXTERNALSYM STORE_SUBMIT_OK}
  STORE_NOTIFY_OK           = ULONG($00000100);
  {$EXTERNALSYM STORE_NOTIFY_OK}
  STORE_MV_PROPS_OK         = ULONG($00000200);
  {$EXTERNALSYM STORE_MV_PROPS_OK}
  STORE_CATEGORIZE_OK       = ULONG($00000400);
  {$EXTERNALSYM STORE_CATEGORIZE_OK}
  STORE_RTF_OK              = ULONG($00000800);
  {$EXTERNALSYM STORE_RTF_OK}
  STORE_RESTRICTION_OK      = ULONG($00001000);
  {$EXTERNALSYM STORE_RESTRICTION_OK}
  STORE_SORT_OK             = ULONG($00002000);
  {$EXTERNALSYM STORE_SORT_OK}

{ PR_STORE_STATE bits, try not to collide with PR_STORE_SUPPORT_MASK }

  STORE_HAS_SEARCHES        = ULONG($01000000);
  {$EXTERNALSYM STORE_HAS_SEARCHES}

{ OpenEntry() }

//***** MAPI_MODIFY             ((ULONG) 0x00000001) above
//***** MAPI_DEFERRED_ERRORS    ((ULONG) 0x00000008) below
//***** MAPI_BEST_ACCESS        ((ULONG) 0x00000010) above

{ SetReceiveFolder() }

//***** MAPI_UNICODE            ((ULONG) 0x80000000) above

{ GetReceiveFolder() }

//***** MAPI_UNICODE            ((ULONG) 0x80000000) above

{ GetReceiveFolderTable() }

//***** MAPI_DEFERRED_ERRORS    ((ULONG) 0x00000008) below

{ StoreLogoff() }

  LOGOFF_NO_WAIT            = ULONG($00000001);
  {$EXTERNALSYM LOGOFF_NO_WAIT}
  LOGOFF_ORDERLY            = ULONG($00000002);
  {$EXTERNALSYM LOGOFF_ORDERLY}
  LOGOFF_PURGE              = ULONG($00000004);
  {$EXTERNALSYM LOGOFF_PURGE}
  LOGOFF_ABORT              = ULONG($00000008);
  {$EXTERNALSYM LOGOFF_ABORT}
  LOGOFF_QUIET              = ULONG($00000010);
  {$EXTERNALSYM LOGOFF_QUIET}

  LOGOFF_COMPLETE           = ULONG($00010000);
  {$EXTERNALSYM LOGOFF_COMPLETE}
  LOGOFF_INBOUND            = ULONG($00020000);
  {$EXTERNALSYM LOGOFF_INBOUND}
  LOGOFF_OUTBOUND           = ULONG($00040000);
  {$EXTERNALSYM LOGOFF_OUTBOUND}
  LOGOFF_OUTBOUND_QUEUE     = ULONG($00080000);
  {$EXTERNALSYM LOGOFF_OUTBOUND_QUEUE}

{ SetLockState() }

  MSG_LOCKED                = ULONG($00000001);
  {$EXTERNALSYM MSG_LOCKED}
  MSG_UNLOCKED              = ULONG($00000000);
  {$EXTERNALSYM MSG_UNLOCKED}

{ Flag bits for PR_VALID_FOLDER_MASK }

  FOLDER_IPM_SUBTREE_VALID          = ULONG($00000001);
  {$EXTERNALSYM FOLDER_IPM_SUBTREE_VALID}
  FOLDER_IPM_INBOX_VALID            = ULONG($00000002);
  {$EXTERNALSYM FOLDER_IPM_INBOX_VALID}
  FOLDER_IPM_OUTBOX_VALID           = ULONG($00000004);
  {$EXTERNALSYM FOLDER_IPM_OUTBOX_VALID}
  FOLDER_IPM_WASTEBASKET_VALID      = ULONG($00000008);
  {$EXTERNALSYM FOLDER_IPM_WASTEBASKET_VALID}
  FOLDER_IPM_SENTMAIL_VALID         = ULONG($00000010);
  {$EXTERNALSYM FOLDER_IPM_SENTMAIL_VALID}
  FOLDER_VIEWS_VALID                = ULONG($00000020);
  {$EXTERNALSYM FOLDER_VIEWS_VALID}
  FOLDER_COMMON_VIEWS_VALID         = ULONG($00000040);
  {$EXTERNALSYM FOLDER_COMMON_VIEWS_VALID}
  FOLDER_FINDER_VALID               = ULONG($00000080);
  {$EXTERNALSYM FOLDER_FINDER_VALID}

{ IMessage Interface ------------------------------------------------------ }

{ SubmitMessage }

const
  FORCE_SUBMIT                  = ULONG($00000001);
  {$EXTERNALSYM FORCE_SUBMIT}

{ Flags defined in PR_MESSAGE_FLAGS }

  MSGFLAG_READ              = ULONG($00000001);
  {$EXTERNALSYM MSGFLAG_READ}
  MSGFLAG_UNMODIFIED        = ULONG($00000002);
  {$EXTERNALSYM MSGFLAG_UNMODIFIED}
  MSGFLAG_SUBMIT            = ULONG($00000004);
  {$EXTERNALSYM MSGFLAG_SUBMIT}
  MSGFLAG_UNSENT            = ULONG($00000008);
  {$EXTERNALSYM MSGFLAG_UNSENT}
  MSGFLAG_HASATTACH         = ULONG($00000010);
  {$EXTERNALSYM MSGFLAG_HASATTACH}
  MSGFLAG_FROMME            = ULONG($00000020);
  {$EXTERNALSYM MSGFLAG_FROMME}
  MSGFLAG_ASSOCIATED        = ULONG($00000040);
  {$EXTERNALSYM MSGFLAG_ASSOCIATED}
  MSGFLAG_RESEND            = ULONG($00000080);
  {$EXTERNALSYM MSGFLAG_RESEND}
  MSGFLAG_RN_PENDING        = ULONG($00000100);
  {$EXTERNALSYM MSGFLAG_RN_PENDING}
  MSGFLAG_NRN_PENDING       = ULONG($00000200);
  {$EXTERNALSYM MSGFLAG_NRN_PENDING}

{ Flags defined in PR_SUBMIT_FLAGS }

  SUBMITFLAG_LOCKED         = ULONG($00000001);
  {$EXTERNALSYM SUBMITFLAG_LOCKED}
  SUBMITFLAG_PREPROCESS     = ULONG($00000002);
  {$EXTERNALSYM SUBMITFLAG_PREPROCESS}

{ GetAttachmentTable() }
//***** MAPI_UNICODE            ((ULONG) 0x80000000) above

{ GetRecipientTable() }
//***** MAPI_UNICODE            ((ULONG) 0x80000000) above

{ ModifyRecipients }

{ ((ULONG) 0x00000001 is not a valid flag on ModifyRecipients. }
  MODRECIP_ADD              = ULONG($00000002);
  {$EXTERNALSYM MODRECIP_ADD}
  MODRECIP_MODIFY           = ULONG($00000004);
  {$EXTERNALSYM MODRECIP_MODIFY}
  MODRECIP_REMOVE           = ULONG($00000008);
  {$EXTERNALSYM MODRECIP_REMOVE}

{ SetReadFlag }

  SUPPRESS_RECEIPT          = ULONG($00000001);
  {$EXTERNALSYM SUPPRESS_RECEIPT}
  CLEAR_READ_FLAG           = ULONG($00000004);
  {$EXTERNALSYM CLEAR_READ_FLAG}
//***** MAPI_DEFERRED_ERRORS    ((ULONG) 0x00000008) below
//***** GENERATE_RECEIPT_ONLY   ((ULONG) 0x00000010) above
  CLEAR_RN_PENDING          = ULONG($00000020);
  {$EXTERNALSYM CLEAR_RN_PENDING}
  CLEAR_NRN_PENDING         = ULONG($00000040);
  {$EXTERNALSYM CLEAR_NRN_PENDING}

{ DeleteAttach }

  ATTACH_DIALOG             = ULONG($00000001);
  {$EXTERNALSYM ATTACH_DIALOG}

{ PR_SECURITY values }
  SECURITY_SIGNED           = ULONG($00000001);
  {$EXTERNALSYM SECURITY_SIGNED}
  SECURITY_ENCRYPTED        = ULONG($00000002);
  {$EXTERNALSYM SECURITY_ENCRYPTED}

{ PR_PRIORITY values }
  PRIO_URGENT               = LongInt(1);
  {$EXTERNALSYM PRIO_URGENT}
  PRIO_NORMAL               = LongInt(0);
  {$EXTERNALSYM PRIO_NORMAL}
  PRIO_NONURGENT            = LongInt(-1);
  {$EXTERNALSYM PRIO_NONURGENT}

{ PR_SENSITIVITY values }
  SENSITIVITY_NONE                      = ULONG($00000000);
  {$EXTERNALSYM SENSITIVITY_NONE}
  SENSITIVITY_PERSONAL                  = ULONG($00000001);
  {$EXTERNALSYM SENSITIVITY_PERSONAL}
  SENSITIVITY_PRIVATE                   = ULONG($00000002);
  {$EXTERNALSYM SENSITIVITY_PRIVATE}
  SENSITIVITY_COMPANY_CONFIDENTIAL      = ULONG($00000003);
  {$EXTERNALSYM SENSITIVITY_COMPANY_CONFIDENTIAL}

{ PR_IMPORTANCE values }
  IMPORTANCE_LOW            = LongInt(0);
  {$EXTERNALSYM IMPORTANCE_LOW}
  IMPORTANCE_NORMAL         = LongInt(1);
  {$EXTERNALSYM IMPORTANCE_NORMAL}
  IMPORTANCE_HIGH           = LongInt(2);
  {$EXTERNALSYM IMPORTANCE_HIGH}

{ IAttach Interface ------------------------------------------------------- }

{ IAttach attachment methods: PR_ATTACH_METHOD values }

const
  NO_ATTACHMENT             = ULONG($00000000);
  {$EXTERNALSYM NO_ATTACHMENT}
  ATTACH_BY_VALUE           = ULONG($00000001);
  {$EXTERNALSYM ATTACH_BY_VALUE}
  ATTACH_BY_REFERENCE       = ULONG($00000002);
  {$EXTERNALSYM ATTACH_BY_REFERENCE}
  ATTACH_BY_REF_RESOLVE     = ULONG($00000003);
  {$EXTERNALSYM ATTACH_BY_REF_RESOLVE}
  ATTACH_BY_REF_ONLY        = ULONG($00000004);
  {$EXTERNALSYM ATTACH_BY_REF_ONLY}
  ATTACH_EMBEDDED_MSG       = ULONG($00000005);
  {$EXTERNALSYM ATTACH_EMBEDDED_MSG}
  ATTACH_OLE                = ULONG($00000006);
  {$EXTERNALSYM ATTACH_OLE}

{ Address Book interface definition }

{ ADRPARM ulFlags - top 4 bits used for versioning }

function GET_ADRPARM_VERSION(ulFlags: ULONG): ULONG;
{$EXTERNALSYM GET_ADRPARM_VERSION}
function SET_ADRPARM_VERSION(ulFlags, ulVersion: ULONG): ULONG;
{$EXTERNALSYM SET_ADRPARM_VERSION}

{ Current versions of ADRPARM }
const
  ADRPARM_HELP_CTX          = ULONG($00000000);
  {$EXTERNALSYM ADRPARM_HELP_CTX}

{ ulFlags   - bit fields }
  DIALOG_MODAL              = ULONG($00000001);
  {$EXTERNALSYM DIALOG_MODAL}
  DIALOG_SDI                = ULONG($00000002);
  {$EXTERNALSYM DIALOG_SDI}
  DIALOG_OPTIONS            = ULONG($00000004);
  {$EXTERNALSYM DIALOG_OPTIONS}
  ADDRESS_ONE               = ULONG($00000008);
  {$EXTERNALSYM ADDRESS_ONE}
  AB_SELECTONLY             = ULONG($00000010);
  {$EXTERNALSYM AB_SELECTONLY}
  AB_RESOLVE                = ULONG($00000020);
  {$EXTERNALSYM AB_RESOLVE}

// ---------------------------------
//  PR_DISPLAY_TYPEs
//
// *  These standard display types are
// *  by default handled by MAPI.
// *  They have default icons associated
// *  with them.

{ For address book contents tables }
  DT_MAILUSER           = ULONG($00000000);
  {$EXTERNALSYM DT_MAILUSER}
  DT_DISTLIST           = ULONG($00000001);
  {$EXTERNALSYM DT_DISTLIST}
  DT_FORUM              = ULONG($00000002);
  {$EXTERNALSYM DT_FORUM}
  DT_AGENT              = ULONG($00000003);
  {$EXTERNALSYM DT_AGENT}
  DT_ORGANIZATION       = ULONG($00000004);
  {$EXTERNALSYM DT_ORGANIZATION}
  DT_PRIVATE_DISTLIST   = ULONG($00000005);
  {$EXTERNALSYM DT_PRIVATE_DISTLIST}
  DT_REMOTE_MAILUSER    = ULONG($00000006);
  {$EXTERNALSYM DT_REMOTE_MAILUSER}

{ For address book hierarchy tables }
  DT_MODIFIABLE         = ULONG($00010000);
  {$EXTERNALSYM DT_MODIFIABLE}
  DT_GLOBAL             = ULONG($00020000);
  {$EXTERNALSYM DT_GLOBAL}
  DT_LOCAL              = ULONG($00030000);
  {$EXTERNALSYM DT_LOCAL}
  DT_WAN                = ULONG($00040000);
  {$EXTERNALSYM DT_WAN}
  DT_NOT_SPECIFIC       = ULONG($00050000);
  {$EXTERNALSYM DT_NOT_SPECIFIC}

{ For folder hierarchy tables }
  DT_FOLDER             = ULONG($01000000);
  {$EXTERNALSYM DT_FOLDER}
  DT_FOLDER_LINK        = ULONG($02000000);
  {$EXTERNALSYM DT_FOLDER_LINK}

{ Accelerator callback for DIALOG_SDI form of AB UI }
type
  PFnABSDI = ^TAccelerateABSDI;
  ACCELERATEABSDI = function (ulUIParam: ULONG; lpvmsg: Pointer): BOOL; stdcall;
  {$EXTERNALSYM ACCELERATEABSDI}
  TAccelerateABSDI = ACCELERATEABSDI;

{ Callback to application telling it that the DIALOG_SDI form of the
  AB UI has been dismissed.  This is so that the above LPFNABSDI
  function doesn't keep being called. }

  PFnDismiss = ^TDismissModeless;
  DISMISSMODELESS = function (ulUIParam: ULONG; lpvContext: Pointer): Pointer; stdcall;
  {$EXTERNALSYM DISMISSMODELESS}
  TDismissModeless = DISMISSMODELESS;

{ Prototype for the client function hooked to an optional button on
  the address book dialog }

  PFnButton = ^TFnButton;
  FNBUTTON = function (ulUIParam: ULONG; lpvContext: Pointer; cbEntryID: ULONG;
    lpSelection: PEntryID; ulFlags: ULONG): SCODE; stdcall;
  {$EXTERNALSYM FNBUTTON}
  TFnButton = FNBUTTON;

{ Parameters for the address book dialog }

  PAdrParam = ^TAdrParam;
  _ADRPARM = record
    cbABContEntryID: ULONG;
    lpABContEntryID: PEntryID;
    ulFlags: ULONG;
    lpReserved: Pointer;
    ulHelpContext: ULONG;
    lpszHelpFileName: LPTSTR;
    lpfnABSDI: PFnABSDI;
    lpfnDismiss: PFnDismiss;
    lpvDismissContext: Pointer;
    lpszCaption: LPTSTR;
    lpszNewEntryTitle: LPTSTR;
    lpszDestWellsTitle: LPTSTR;
    cDestFields: ULONG;
    nDestFieldFocus: ULONG;
    lppszDestTitles: ^LPTSTR;
    lpulDestComps: ^ULONG;
    lpContRestriction: PSRestriction;
    lpHierRestriction: PSRestriction;
  end;
  {$EXTERNALSYM _ADRPARM}
  TAdrParam = _ADRPARM;
  ADRPARM = _ADRPARM;
  {$EXTERNALSYM ADRPARM}

{ Random flags }
const
{ Flag set in MAPI one off entryids }
  MAPI_ONE_OFF_NO_RICH_INFO = $0001;
  {$EXTERNALSYM MAPI_ONE_OFF_NO_RICH_INFO}

{ Flag for deferred error }
  MAPI_DEFERRED_ERRORS      = ULONG($00000008);
  {$EXTERNALSYM MAPI_DEFERRED_ERRORS}

{ Flag for creating and using Folder Associated Information Messages }
  MAPI_ASSOCIATED           = ULONG($00000040);
  {$EXTERNALSYM MAPI_ASSOCIATED}

{ Flags for OpenMessageStore() }

  MDB_NO_DIALOG             = ULONG($00000001);
  {$EXTERNALSYM MDB_NO_DIALOG}
  MDB_WRITE                 = ULONG($00000004);
  {$EXTERNALSYM MDB_WRITE}
//***** MAPI_DEFERRED_ERRORS    ((ULONG) 0x00000008) above
//***** MAPI_BEST_ACCESS        ((ULONG) 0x00000010) above
  MDB_TEMPORARY             = ULONG($00000020);
  {$EXTERNALSYM MDB_TEMPORARY}
  MDB_NO_MAIL               = ULONG($00000080);
  {$EXTERNALSYM MDB_NO_MAIL}

{ Flags for OpenAddressBook }

  AB_NO_DIALOG              = ULONG($00000001);
  {$EXTERNALSYM AB_NO_DIALOG}

{ IMAPIControl Interface -------------------------------------------------- }

{ Interface used in controls (particularly the button) defined by Display Tables }

{ Flags for GetState }

  MAPI_ENABLED         = ULONG($00000000);
  {$EXTERNALSYM MAPI_ENABLED}
  MAPI_DISABLED        = ULONG($00000001);
  {$EXTERNALSYM MAPI_DISABLED}

type
  IMAPIControl = interface(IUnknown)
    function GetLastError(hResult: HRESULT; ulFlags: ULONG;
      var lppMAPIError: TMapiError): HResult; stdcall;
    function Activate(ulFlags, ulUIParam: ULONG): HResult; stdcall;
    function GetState(ulFlags: ULONG; var lpulState: ULONG): HResult; stdcall;
  end;
  {$EXTERNALSYM IMAPIControl}

{ Display Tables ---------------------------------------------------------- }

{ Flags used in display tables - that is, PR_CONTROL_FLAGS }

const
  DT_MULTILINE          = ULONG($00000001);
  {$EXTERNALSYM DT_MULTILINE}
  DT_EDITABLE           = ULONG($00000002);
  {$EXTERNALSYM DT_EDITABLE}
  DT_REQUIRED           = ULONG($00000004);
  {$EXTERNALSYM DT_REQUIRED}
  DT_SET_IMMEDIATE      = ULONG($00000008);
  {$EXTERNALSYM DT_SET_IMMEDIATE}
  DT_PASSWORD_EDIT      = ULONG($00000010);
  {$EXTERNALSYM DT_PASSWORD_EDIT}
  DT_ACCEPT_DBCS        = ULONG($00000020);
  {$EXTERNALSYM DT_ACCEPT_DBCS}
  DT_SET_SELECTION      = ULONG($00000040);
  {$EXTERNALSYM DT_SET_SELECTION}

{ Display Table structures }

  DTCT_LABEL            = ULONG($00000000);
  {$EXTERNALSYM DTCT_LABEL}
  DTCT_EDIT             = ULONG($00000001);
  {$EXTERNALSYM DTCT_EDIT}
  DTCT_LBX              = ULONG($00000002);
  {$EXTERNALSYM DTCT_LBX}
  DTCT_COMBOBOX         = ULONG($00000003);
  {$EXTERNALSYM DTCT_COMBOBOX}
  DTCT_DDLBX            = ULONG($00000004);
  {$EXTERNALSYM DTCT_DDLBX}
  DTCT_CHECKBOX         = ULONG($00000005);
  {$EXTERNALSYM DTCT_CHECKBOX}
  DTCT_GROUPBOX         = ULONG($00000006);
  {$EXTERNALSYM DTCT_GROUPBOX}
  DTCT_BUTTON           = ULONG($00000007);
  {$EXTERNALSYM DTCT_BUTTON}
  DTCT_PAGE             = ULONG($00000008);
  {$EXTERNALSYM DTCT_PAGE}
  DTCT_RADIOBUTTON      = ULONG($00000009);
  {$EXTERNALSYM DTCT_RADIOBUTTON}
  DTCT_MVLISTBOX        = ULONG($0000000B);
  {$EXTERNALSYM DTCT_MVLISTBOX}
  DTCT_MVDDLBX          = ULONG($0000000C);
  {$EXTERNALSYM DTCT_MVDDLBX}

type
{ Labels }
  PDTblLabel = ^TDTblLabel;
  _DTBLLABEL = record
    ulbLpszLabelName: ULONG;
    ulFlags: ULONG;
  end;
  {$EXTERNALSYM _DTBLLABEL}
  TDTblLabel = _DTBLLABEL;
  DTBLLABEL = _DTBLLABEL;
  {$EXTERNALSYM DTBLLABEL}

(*!!!
#define SizedDtblLabel(n,u) \
struct _DTBLLABEL_ ## u \
{ \
    DTBLLABEL   dtbllabel; \
    TCHAR       lpszLabelName[n]; \
} u
*)

{ Simple Text Edits }
  PDTblEdit = ^TDTblEdit;
  _DTBLEDIT = record
    ulbLpszCharsAllowed: ULONG;
    ulFlags: ULONG;
    ulNumCharsAllowed: ULONG;
    ulPropTag: ULONG;
  end;
  {$EXTERNALSYM _DTBLEDIT}
  TDTblEdit = _DTBLEDIT;
  DTBLEDIT = _DTBLEDIT;
  {$EXTERNALSYM DTBLEDIT}

(*!!!
#define SizedDtblEdit(n,u) \
struct _DTBLEDIT_ ## u \
{ \
    DTBLEDIT    dtbledit; \
    TCHAR       lpszCharsAllowed[n]; \
} u
*)

{ List Box }
const
  MAPI_NO_HBAR          = ULONG($00000001);
  {$EXTERNALSYM MAPI_NO_HBAR}
  MAPI_NO_VBAR          = ULONG($00000002);
  {$EXTERNALSYM MAPI_NO_VBAR}

type
  PDTblLbx = ^TDTblLbx;
  _DTBLLBX = record
    ulFlags: ULONG;
    ulPRSetProperty: ULONG;
    ulPRTableName: ULONG;
  end;
  {$EXTERNALSYM _DTBLLBX}
  TDTblLbx = _DTBLLBX;
  DTBLLBX = _DTBLLBX;
  {$EXTERNALSYM DTBLLBX}

{ Combo Box }
  PDTblComboBox = ^TDTblComboBox;
  _DTBLCOMBOBOX = record
    ulbLpszCharsAllowed: ULONG;
    ulFlags: ULONG;
    ulNumCharsAllowed: ULONG;
    ulPRPropertyName: ULONG;
    ulPRTableName: ULONG;
  end;
  {$EXTERNALSYM _DTBLCOMBOBOX}
  TDTblComboBox = _DTBLCOMBOBOX;
  DTBLCOMBOBOX = _DTBLCOMBOBOX;
  {$EXTERNALSYM DTBLCOMBOBOX}

(*!!!
#define SizedDtblComboBox(n,u) \
struct _DTBLCOMBOBOX_ ## u \
{ \
    DTBLCOMBOBOX    dtblcombobox; \
    TCHAR           lpszCharsAllowed[n]; \
} u
*)

{ Drop Down }
  PDTblDDLbx = ^TDTblDDLbx;
  _DTBLDDLBX = record
    ulFlags: ULONG;
    ulPRDisplayProperty: ULONG;
    ulPRSetProperty: ULONG;
    ulPRTableName: ULONG;
  end;
  {$EXTERNALSYM _DTBLDDLBX}
  TDTblDDLbx = _DTBLDDLBX;
  DTBLDDLBX = _DTBLDDLBX;
  {$EXTERNALSYM DTBLDDLBX}

{ Check Box }
  PDTblCheckBox = ^TDTblCheckBox;
  _DTBLCHECKBOX = record
    ulbLpszLabel: ULONG;
    ulFlags: ULONG;
    ulPRPropertyName: ULONG;
  end;
  {$EXTERNALSYM _DTBLCHECKBOX}
  TDTblCheckBox = _DTBLCHECKBOX;
  DTBLCHECKBOX = _DTBLCHECKBOX;
  {$EXTERNALSYM DTBLCHECKBOX}

(*!!!
#define SizedDtblCheckBox(n,u) \
struct _DTBLCHECKBOX_ ## u \
{ \
    DTBLCHECKBOX    dtblcheckbox; \
    TCHAR       lpszLabel[n]; \
} u
*)

{ Group Box }
  PDTblGroupBox = ^TDTblGroupBox;
  _DTBLGROUPBOX = record
    ulbLpszLabel: ULONG;
    ulFlags: ULONG;
  end;
  {$EXTERNALSYM _DTBLGROUPBOX}
  TDTblGroupBox = _DTBLGROUPBOX;
  DTBLGROUPBOX = _DTBLGROUPBOX;
  {$EXTERNALSYM DTBLGROUPBOX}

(*!!!
#define SizedDtblGroupBox(n,u) \
struct _DTBLGROUPBOX_ ## u \
{ \
    DTBLGROUPBOX    dtblgroupbox; \
    TCHAR           lpszLabel[n]; \
} u
*)

{ Button control }
  PDTblButton = ^TDTblButton;
  _DTBLBUTTON = record
    ulbLpszLabel: ULONG;
    ulFlags: ULONG;
    ulPRControl: ULONG;
  end;
  {$EXTERNALSYM _DTBLBUTTON}
  TDTblButton = _DTBLBUTTON;
  DTBLBUTTON = _DTBLBUTTON;
  {$EXTERNALSYM DTBLBUTTON}

(*!!!
#define SizedDtblButton(n,u) \
struct _DTBLBUTTON_ ## u \
{ \
    DTBLBUTTON  dtblbutton; \
    TCHAR       lpszLabel[n]; \
} u
*)

{ Pages }
  PDTblPage = ^TDTblPage;
  _DTBLPAGE = record
    ulbLpszLabel: ULONG;
    ulFlags: ULONG;
    ulbLpszComponent: ULONG;
    ulContext: ULONG;
  end;
  {$EXTERNALSYM _DTBLPAGE}
  TDTblPage = _DTBLPAGE;
  DTBLPAGE = _DTBLPAGE;
  {$EXTERNALSYM DTBLPAGE}

(*!!!
#define SizedDtblPage(n,n1,u) \
struct _DTBLPAGE_ ## u \
{ \
    DTBLPAGE    dtblpage; \
    TCHAR       lpszLabel[n]; \
    TCHAR       lpszComponent[n1]; \
} u
*)

{ Radio button }
  PDTblRadioButton = ^TDTblRadioButton;
  _DTBLRADIOBUTTON = record
    ulbLpszLabel: ULONG;
    ulFlags: ULONG;
    ulcButtons: ULONG;
    ulPropTag: ULONG;
    lReturnValue: LongInt;
  end;
  {$EXTERNALSYM _DTBLRADIOBUTTON}
  TDTblRadioButton = _DTBLRADIOBUTTON;
  DTBLRADIOBUTTON = _DTBLRADIOBUTTON;
  {$EXTERNALSYM DTBLRADIOBUTTON}

(*!!!
#define SizedDtblRadioButton(n,u) \
struct _DTBLRADIOBUTTON_ ## u \
{ \
    DTBLRADIOBUTTON dtblradiobutton; \
    TCHAR           lpszLabel[n]; \
} u
*)

{ MultiValued listbox }
  PDTblMvListBox = ^TDTblMvListBox;
  _DTBLMVLISTBOX = record
    ulFlags: ULONG;
    ulMVPropTag: ULONG;
  end;
  {$EXTERNALSYM _DTBLMVLISTBOX}
  TDTblMvListBox = _DTBLMVLISTBOX;
  DTBLMVLISTBOX = _DTBLMVLISTBOX;
  {$EXTERNALSYM DTBLMVLISTBOX}

{ MultiValued dropdown }
  PDTblMvDDLbx = ^TDTblMvDDLbx;
  _DTBLMVDDLBX = record
    ulFlags: ULONG;
    ulMVPropTag: ULONG;
  end;
  {$EXTERNALSYM _DTBLMVDDLBX}
  TDTblMvDDLbx = _DTBLMVDDLBX;
  DTBLMVDDLBX = _DTBLMVDDLBX;
  {$EXTERNALSYM DTBLMVDDLBX}

{ IProviderAdmin Interface ---------------------------------------------- }

{ Flags for ConfigureMsgService }
const
  UI_SERVICE                  = $00000002;
  {$EXTERNALSYM UI_SERVICE}
  SERVICE_UI_ALWAYS           = $00000002; // Duplicate UI_SERVICE for consistency and compatibility
  {$EXTERNALSYM SERVICE_UI_ALWAYS}
  SERVICE_UI_ALLOWED          = $00000010;
  {$EXTERNALSYM SERVICE_UI_ALLOWED}
  UI_CURRENT_PROVIDER_FIRST   = $00000004;
  {$EXTERNALSYM UI_CURRENT_PROVIDER_FIRST}
// MSG_SERVICE_UI_READ_ONLY         0x00000008 - in MAPISPI.H

{ GetProviderTable() }
//***** MAPI_UNICODE            ((ULONG) 0x80000000) above

{ Values for PR_RESOURCE_FLAGS in message service table }

type
  IMessage = interface;

  IMAPIFolder = interface(IMAPIContainer)
    function CreateMessage(lpInterface: PIID; ulFlags: ULONG;
      out lppMessage: IMessage): HResult; stdcall;
    function CopyMessages(lpMsgList: PEntryList; lpInterface: PIID;
      lpDestFolder: Pointer; ulUIParam: ULONG; lpProgress: IMAPIProgress;
      ulFlags: ULONG): HResult; stdcall;
    function DeleteMessages(lpMsgList: PEntryList; ulUIParam: ULONG;
      lpProgress: IMAPIProgress; ulFlags: ULONG): HResult; stdcall;
    function CreateFolder(ulFolderType: ULONG; lpszFolderName,
      lpszFolderComment: LPTSTR; lpInterface: PIID; ulFlags: ULONG;
      out lppFolder: IMAPIFolder): HResult; stdcall;
    function CopyFolder(cbEntryID: ULONG; lpEntryID: PEntryID;
      lpInterface: PIID; lpDestFolder: Pointer; lpszNewFolderName: LPTSTR;
      ulUIParam: ULONG; lpProgress: IMAPIProgress; ulFlags: ULONG): HResult; stdcall;
    function DeleteFolder(cbEntryID: ULONG; lpEntryID: PEntryID; ulUIParam: ULONG;
      lpProgress: IMAPIProgress; ulFlags: ULONG): HResult; stdcall;
    function SetReadFlags(lpMsgList: PEntryList; ulUIParam: ULONG;
      lpProgress: IMAPIProgress; ulFlags: ULONG): HResult; stdcall;
    function GetMessageStatus(cbEntryID: ULONG; lpEntryID: PEntryID;
      ulFlags: ULONG; var lpulMessageStatus: ULONG): HResult; stdcall;
    function SetMessageStatus(cbEntryID: ULONG; lpEntryID: PEntryID;
      ulNewStatus, ulNewStatusMask: ULONG; var lpulOldStatus: ULONG): HResult; stdcall;
    function SaveContentsSort(lpSortCriteria: PSSortOrderSet;
      ulFlags: ULONG): HResult; stdcall;
    function EmptyFolder(ulUIParam: ULONG; lpProgress: IMAPIProgress;
      ulFlags: ULONG): HResult; stdcall;
  end;
  {$EXTERNALSYM IMAPIFolder}

  IMsgStore = interface(IMAPIProp)
    function Advise(cbEntryID: ULONG; lpEntryID: PEntryID; ulEventMask: ULONG;
      lpAdviseSink: IMAPIAdviseSink; var lpulConnection: ULONG): HResult; stdcall;
    function Unadvise(ulConnection: ULONG): HResult; stdcall;
    function CompareEntryIDs(cbEntryID1: ULONG; lpEntryID1: PEntryID;
      cbEntryID2: ULONG; lpEntryID2: PEntryID; ulFlags: ULONG;
      var lpulResult: ULONG): HResult; stdcall;
    function OpenEntry(cbEntryID: ULONG; lpEntryID: PEntryID;
      lpInterface: PIID; ulFlags: ULONG; var lpulObjType: ULONG;
      out lppUnk: IUnknown): HResult; stdcall;
    function SetReceiveFolder(lpszMessageClass: LPTSTR; ulFlags, cbEntryID: ULONG;
      lpEntryID: PEntryID): HResult; stdcall;
    function GetReceiveFolder(lpszMessageClass: LPTSTR; ulFlags: ULONG;
      var lpcbEntryID: ULONG; lppEntryID: PEntryID;
      lppszExplicitClass: LPTSTR): HResult; stdcall;
    function GetReceiveFolderTable(ulFlags: ULONG; out lppTable: IMAPITable): HResult; stdcall;
    function StoreLogoff(var lpulFlags: ULONG): HResult; stdcall;
    function AbortSubmit(cbEntryID: ULONG; lpEntryID: PEntryID;
      ulFlags: ULONG): HResult; stdcall;
    function GetOutgoingQueue(ulFlags: ULONG; out lppTable: IMAPITable): HResult; stdcall;
    function SetLockState(lpMessage: IMessage; ulLockState: ULONG): HResult; stdcall;
    function FinishedMsg(ulFlags, cbEntryID: ULONG; lpEntryID: PEntryID): HResult; stdcall;
    function NotifyNewMail(lpNotification: PNotification): HResult; stdcall;
  end;
  {$EXTERNALSYM IMsgStore}

  IAttach = interface(IMAPIProp)
  end;
  {$EXTERNALSYM IAttach}

  IMessage = interface(IMAPIProp)
    function GetAttachmentTable(ulFlags: ULONG; out lppTable: IMAPITable): HResult; stdcall;
    function OpenAttach(ulAttachmentNum: ULONG; lpInterface: PIID;
      ulFlags: ULONG; out lppAttach: IAttach): HResult; stdcall;
    function CreateAttach(lpInterface: PIID; ulFlags: ULONG;
      var lpulAttachmentNum: ULONG; out lppAttach: IAttach): HResult; stdcall;
    function DeleteAttach(ulAttachmentNum, ulUIParam: ULONG; lpProgress: IMAPIProgress;
      ulFlags: ULONG): HResult; stdcall;
    function GetRecipientTable(ulFlags: ULONG; out lppTable: IMAPITable): HResult; stdcall;
    function ModifyRecipients(ulFlags: ULONG; lpMods: PAdrList): HResult; stdcall;
    function SubmitMessage(ulFlags: ULONG): HResult; stdcall;
    function SetReadFlag(ulFlags: ULONG): HResult; stdcall;
  end;
  {$EXTERNALSYM IMessage}

  IProviderAdmin = interface(IUnknown)
    function GetLastError(hResult: HRESULT; ulFlags: ULONG;
      var lppMAPIError: TMapiError): HResult; stdcall;
    function GetProviderTable(ulFlags: ULONG; out lppTable: IMAPITable): HResult; stdcall;
    function CreateProvider(lpszProvider: LPTSTR; cValues: ULONG;
      lpProps: PSPropValue; ulUIParam, ulFlags: ULONG; lpUID: TMapiUID): HResult; stdcall;
    function DeleteProvider(lpUID: PMapiUID): HResult; stdcall;
    function OpenProfileSection(lpUID: PMapiUID; lpInterface: PIID;
      ulFlags: ULONG; out lppProfSect: IProfSect): HResult; stdcall;
  end;
  {$EXTERNALSYM IProviderAdmin}
{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation

uses
  SysUtils;
{$ENDIF JWA_OMIT_SECTIONS}


{$IFNDEF JWA_INTERFACESECTION}

function IsEqualMAPIUID(lpuid1, lpuid2: TMapiUID): Boolean;
begin
  Result := CompareMem(@lpuid1, @lpuid2, Sizeof(TMapiUID));
end;

function PROP_TYPE(ulPropTag: ULONG): ULONG;
begin
  Result := ulPropTag and PROP_TYPE_MASK;
end;

function PROP_ID(ulPropTag: ULONG): ULONG;
begin
  Result := ulPropTag shr 16;
end;

function PROP_TAG(ulPropType, ulPropID: ULONG): ULONG;
begin
  Result := (ulPropID shl 16) or ulPropType;
end;

function CHANGE_PROP_TYPE(ulPropTag, ulPropType: ULONG): ULONG;
begin
  Result := (ULONG($FFFF0000) and ulPropTag) or ulPropType;
end;

function MVI_PROP(tag: ULONG): ULONG;
begin
  Result := tag or MVI_FLAG;
end;

function GET_ADRPARM_VERSION(ulFlags: ULONG): ULONG;
begin
  Result := ulFlags and $F0000000;
end;

function SET_ADRPARM_VERSION(ulFlags, ulVersion: ULONG): ULONG;
begin
  Result := ulVersion or (ulFlags and $0FFFFFFF);
end;

{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
