{******************************************************************************}
{                                                                              }
{ Windows Installer API interface Unit for Object Pascal                       }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: msiquery.h, released June 2000. The original Pascal    }
{ code is: MsiQuery.pas, released June 2001. The initial developer of the      }
{ Pascal code is Marcel van Brakel (brakelm att chello dott nl).               }
{                                                                              }
{ Portions created by Marcel van Brakel are Copyright (C) 1999-2001            }
{ Marcel van Brakel. All Rights Reserved.                                      }
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

// $Id: JwaMsiQuery.pas,v 1.11 2007/09/05 11:58:51 dezipaitor Exp $
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaMsiQuery;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "msiquery.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaMsi, JwaWinBase, JwaWinType;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}
(*****************************************************************************\
*                                                                             *
* MsiQuery.h - Interface to running installer for custom actions and tools    *
*                                                                             *
* Version 1.0 - 1.2                                                           *
*                                                                             *
* NOTES:  All buffers sizes are TCHAR count, null included only on input      *
*         Return argument pointers may be null if not interested in value     *
*         Returned handles of all types must be closed: MsiCloseHandle(h)     *
*         Functions with UINT return type return a system error code          *
*         Designated functions will set or clear the last error record,       *
*         which is then accessible with MsiGetLastErrorRecord. However,       *
*         the following argument errors do not register an error record:      *
*         ERROR_INVALID_HANDLE, ERROR_INVALID_PARAMETER, ERROR_MORE_DATA.     *
*                                                                             *
* Copyright (c) 1999-2000, Microsoft Corp.      All rights reserved.          *
*                                                                             *
\*****************************************************************************)

const
  MSI_NULL_INTEGER = DWORD($80000000);  // integer value reserved for null
  {$EXTERNALSYM MSI_NULL_INTEGER}

// MsiOpenDatabase persist predefine values, otherwise output database path is used

  MSIDBOPEN_READONLY     = LPCTSTR(0);  // database open read-only, no persistent changes
  {$EXTERNALSYM MSIDBOPEN_READONLY}
  MSIDBOPEN_TRANSACT     = LPCTSTR(1);  // database read/write in transaction mode
  {$EXTERNALSYM MSIDBOPEN_TRANSACT}
  MSIDBOPEN_DIRECT       = LPCTSTR(2);  // database direct read/write without transaction
  {$EXTERNALSYM MSIDBOPEN_DIRECT}
  MSIDBOPEN_CREATE       = LPCTSTR(3);  // create new database, transact mode read/write
  {$EXTERNALSYM MSIDBOPEN_CREATE}
  MSIDBOPEN_CREATEDIRECT = LPCTSTR(4);  // create new database, direct mode read/write
  {$EXTERNALSYM MSIDBOPEN_CREATEDIRECT}

  MSIDBOPEN_PATCHFILE    = 32 div SizeOf(TCHAR); // add flag to indicate patch file

  MSIDBSTATE_ERROR    = DWORD(-1);  // invalid database handle
  {$EXTERNALSYM MSIDBSTATE_ERROR}
  MSIDBSTATE_READ     =  0;  // database open read-only, no persistent changes
  {$EXTERNALSYM MSIDBSTATE_READ}
  MSIDBSTATE_WRITE    =  1;  // database readable and updatable
  {$EXTERNALSYM MSIDBSTATE_WRITE}

type
  MSIDBSTATE = DWORD;
  {$EXTERNALSYM MSIDBSTATE}
  TMsiDbState = MSIDBSTATE;

const
  MSIMODIFY_SEEK             = DWORD(-1);  // reposition to current record primary key
  {$EXTERNALSYM MSIMODIFY_SEEK}
  MSIMODIFY_REFRESH          = 0;  // refetch current record data
  {$EXTERNALSYM MSIMODIFY_REFRESH}
  MSIMODIFY_INSERT           = 1;  // insert new record, fails if matching key exists
  {$EXTERNALSYM MSIMODIFY_INSERT}
  MSIMODIFY_UPDATE           = 2;  // update existing non-key data of fetched record
  {$EXTERNALSYM MSIMODIFY_UPDATE}
  MSIMODIFY_ASSIGN           = 3;  // insert record, replacing any existing record
  {$EXTERNALSYM MSIMODIFY_ASSIGN}
  MSIMODIFY_REPLACE          = 4;  // update record, delete old if primary key edit
  {$EXTERNALSYM MSIMODIFY_REPLACE}
  MSIMODIFY_MERGE            = 5;  // fails if record with duplicate key not identical
  {$EXTERNALSYM MSIMODIFY_MERGE}
  MSIMODIFY_DELETE           = 6;  // remove row referenced by this record from table
  {$EXTERNALSYM MSIMODIFY_DELETE}
  MSIMODIFY_INSERT_TEMPORARY = 7;  // insert a temporary record
  {$EXTERNALSYM MSIMODIFY_INSERT_TEMPORARY}
  MSIMODIFY_VALIDATE         = 8;  // validate a fetched record
  {$EXTERNALSYM MSIMODIFY_VALIDATE}
  MSIMODIFY_VALIDATE_NEW     = 9;  // validate a new record
  {$EXTERNALSYM MSIMODIFY_VALIDATE_NEW}
  MSIMODIFY_VALIDATE_FIELD   = 10; // validate field(s) of an incomplete record
  {$EXTERNALSYM MSIMODIFY_VALIDATE_FIELD}
  MSIMODIFY_VALIDATE_DELETE  = 11; // validate before deleting record
  {$EXTERNALSYM MSIMODIFY_VALIDATE_DELETE}

type
  MSIMODIFY = DWORD;
  {$EXTERNALSYM MSIMODIFY}
  TMsiModify = MSIMODIFY;

const
  MSICOLINFO_NAMES = 0;  // return column names
  {$EXTERNALSYM MSICOLINFO_NAMES}
  MSICOLINFO_TYPES = 1;  // return column definitions, datatype code followed by width
  {$EXTERNALSYM MSICOLINFO_TYPES}

type
  MSICOLINFO = DWORD;
  {$EXTERNALSYM MSICOLINFO}
  TMsiColInfo = MSICOLINFO;

const
  MSICONDITION_FALSE = 0;  // expression evaluates to False
  {$EXTERNALSYM MSICONDITION_FALSE}
  MSICONDITION_TRUE  = 1;  // expression evaluates to True
  {$EXTERNALSYM MSICONDITION_TRUE}
  MSICONDITION_NONE  = 2;  // no expression present
  {$EXTERNALSYM MSICONDITION_NONE}
  MSICONDITION_ERROR = 3;  // syntax error in expression
  {$EXTERNALSYM MSICONDITION_ERROR}

type
  MSICONDITION = DWORD;
  {$EXTERNALSYM MSICONDITION}
  TMsiCondition = MSICONDITION;

const
  MSICOSTTREE_SELFONLY = 0;
  {$EXTERNALSYM MSICOSTTREE_SELFONLY}
  MSICOSTTREE_CHILDREN = 1;
  {$EXTERNALSYM MSICOSTTREE_CHILDREN}
  MSICOSTTREE_PARENTS  = 2;
  {$EXTERNALSYM MSICOSTTREE_PARENTS}
  MSICOSTTREE_RESERVED = 3;  // Reserved for future use
  {$EXTERNALSYM MSICOSTTREE_RESERVED}

type
  MSICOSTTREE = DWORD;
  {$EXTERNALSYM MSICOSTTREE}
  TMsiCostTree = MSICOSTTREE;

const
  MSIDBERROR_INVALIDARG        = DWORD(-3); //  invalid argument
  {$EXTERNALSYM MSIDBERROR_INVALIDARG}
  MSIDBERROR_MOREDATA          = DWORD(-2); //  buffer too small
  {$EXTERNALSYM MSIDBERROR_MOREDATA}
  MSIDBERROR_FUNCTIONERROR     = DWORD(-1); //  function error
  {$EXTERNALSYM MSIDBERROR_FUNCTIONERROR}
  MSIDBERROR_NOERROR           = 0;  //  no error
  {$EXTERNALSYM MSIDBERROR_NOERROR}
  MSIDBERROR_DUPLICATEKEY      = 1;  //  new record duplicates primary keys of existing record in table
  {$EXTERNALSYM MSIDBERROR_DUPLICATEKEY}
  MSIDBERROR_REQUIRED          = 2;  //  non-nullable column, no null values allowed
  {$EXTERNALSYM MSIDBERROR_REQUIRED}
  MSIDBERROR_BADLINK           = 3;  //  corresponding record in foreign table not found
  {$EXTERNALSYM MSIDBERROR_BADLINK}
  MSIDBERROR_OVERFLOW          = 4;  //  data greater than maximum value allowed
  {$EXTERNALSYM MSIDBERROR_OVERFLOW}
  MSIDBERROR_UNDERFLOW         = 5;  //  data less than minimum value allowed
  {$EXTERNALSYM MSIDBERROR_UNDERFLOW}
  MSIDBERROR_NOTINSET          = 6;  //  data not a member of the values permitted in the set
  {$EXTERNALSYM MSIDBERROR_NOTINSET}
  MSIDBERROR_BADVERSION        = 7;  //  invalid version string
  {$EXTERNALSYM MSIDBERROR_BADVERSION}
  MSIDBERROR_BADCASE           = 8;  //  invalid case, must be all upper-case or all lower-case
  {$EXTERNALSYM MSIDBERROR_BADCASE}
  MSIDBERROR_BADGUID           = 9;  //  invalid GUID
  {$EXTERNALSYM MSIDBERROR_BADGUID}
  MSIDBERROR_BADWILDCARD       = 10; //  invalid wildcardfilename or use of wildcards
  {$EXTERNALSYM MSIDBERROR_BADWILDCARD}
  MSIDBERROR_BADIDENTIFIER     = 11; //  bad identifier
  {$EXTERNALSYM MSIDBERROR_BADIDENTIFIER}
  MSIDBERROR_BADLANGUAGE       = 12; //  bad language Id(s)
  {$EXTERNALSYM MSIDBERROR_BADLANGUAGE}
  MSIDBERROR_BADFILENAME       = 13; //  bad filename
  {$EXTERNALSYM MSIDBERROR_BADFILENAME}
  MSIDBERROR_BADPATH           = 14; //  bad path
  {$EXTERNALSYM MSIDBERROR_BADPATH}
  MSIDBERROR_BADCONDITION      = 15; //  bad conditional statement
  {$EXTERNALSYM MSIDBERROR_BADCONDITION}
  MSIDBERROR_BADFORMATTED      = 16; //  bad format string
  {$EXTERNALSYM MSIDBERROR_BADFORMATTED}
  MSIDBERROR_BADTEMPLATE       = 17; //  bad template string
  {$EXTERNALSYM MSIDBERROR_BADTEMPLATE}
  MSIDBERROR_BADDEFAULTDIR     = 18; //  bad string in DefaultDir column of Directory table
  {$EXTERNALSYM MSIDBERROR_BADDEFAULTDIR}
  MSIDBERROR_BADREGPATH        = 19; //  bad registry path string
  {$EXTERNALSYM MSIDBERROR_BADREGPATH}
  MSIDBERROR_BADCUSTOMSOURCE   = 20; //  bad string in CustomSource column of CustomAction table
  {$EXTERNALSYM MSIDBERROR_BADCUSTOMSOURCE}
  MSIDBERROR_BADPROPERTY       = 21; //  bad property string
  {$EXTERNALSYM MSIDBERROR_BADPROPERTY}
  MSIDBERROR_MISSINGDATA       = 22; //  _Validation table missing reference to column
  {$EXTERNALSYM MSIDBERROR_MISSINGDATA}
  MSIDBERROR_BADCATEGORY       = 23; //  Category column of _Validation table for column is invalid
  {$EXTERNALSYM MSIDBERROR_BADCATEGORY}
  MSIDBERROR_BADKEYTABLE       = 24; //  table in KeyTable column of _Validation table could not be found/loaded
  {$EXTERNALSYM MSIDBERROR_BADKEYTABLE}
  MSIDBERROR_BADMAXMINVALUES   = 25; //  value in MaxValue column of _Validation table is less than value in MinValue column
  {$EXTERNALSYM MSIDBERROR_BADMAXMINVALUES}
  MSIDBERROR_BADCABINET        = 26; //  bad cabinet name
  {$EXTERNALSYM MSIDBERROR_BADCABINET}
  MSIDBERROR_BADSHORTCUT       = 27; //  bad shortcut target
  {$EXTERNALSYM MSIDBERROR_BADSHORTCUT}
  MSIDBERROR_STRINGOVERFLOW    = 28; //  string overflow (greater than length allowed in column def)
  {$EXTERNALSYM MSIDBERROR_STRINGOVERFLOW}
  MSIDBERROR_BADLOCALIZEATTRIB = 29; //  invalid localization attribute (primary keys cannot be localized)
  {$EXTERNALSYM MSIDBERROR_BADLOCALIZEATTRIB}

type
  MSIDBERROR = DWORD;
  {$EXTERNALSYM MSIDBERROR}
  TMsiDbError = MSIDBERROR;

const
  MSIRUNMODE_ADMIN           =  0; // admin mode install, else product install
  {$EXTERNALSYM MSIRUNMODE_ADMIN}
  MSIRUNMODE_ADVERTISE       =  1; // installing advertisements, else installing or updating product
  {$EXTERNALSYM MSIRUNMODE_ADVERTISE}
  MSIRUNMODE_MAINTENANCE     =  2; // modifying an existing installation, else new installation
  {$EXTERNALSYM MSIRUNMODE_MAINTENANCE}
  MSIRUNMODE_ROLLBACKENABLED =  3; // rollback is enabled
  {$EXTERNALSYM MSIRUNMODE_ROLLBACKENABLED}
  MSIRUNMODE_LOGENABLED      =  4; // log file active, enabled prior to install session
  {$EXTERNALSYM MSIRUNMODE_LOGENABLED}
  MSIRUNMODE_OPERATIONS      =  5; // spooling execute operations, else in determination phase
  {$EXTERNALSYM MSIRUNMODE_OPERATIONS}
  MSIRUNMODE_REBOOTATEND     =  6; // reboot needed after successful installation (settable)
  {$EXTERNALSYM MSIRUNMODE_REBOOTATEND}
  MSIRUNMODE_REBOOTNOW       =  7; // reboot needed to continue installation (settable)
  {$EXTERNALSYM MSIRUNMODE_REBOOTNOW}
  MSIRUNMODE_CABINET         =  8; // installing files from cabinets and files using Media table
  {$EXTERNALSYM MSIRUNMODE_CABINET}
  MSIRUNMODE_SOURCESHORTNAMES=  9; // source LongFileNames suppressed via PID_MSISOURCE summary property
  {$EXTERNALSYM MSIRUNMODE_SOURCESHORTNAMES}
  MSIRUNMODE_TARGETSHORTNAMES= 10; // target LongFileNames suppressed via SHORTFILENAMES property
  {$EXTERNALSYM MSIRUNMODE_TARGETSHORTNAMES}
  MSIRUNMODE_RESERVED11      = 11; // future use
  {$EXTERNALSYM MSIRUNMODE_RESERVED11}
  MSIRUNMODE_WINDOWS9X       = 12; // operating systems is Windows9?, else Windows NT
  {$EXTERNALSYM MSIRUNMODE_WINDOWS9X}
  MSIRUNMODE_ZAWENABLED      = 13; // operating system supports demand installation
  {$EXTERNALSYM MSIRUNMODE_ZAWENABLED}
  MSIRUNMODE_RESERVED14      = 14; // future use
  {$EXTERNALSYM MSIRUNMODE_RESERVED14}
  MSIRUNMODE_RESERVED15      = 15; // future use
  {$EXTERNALSYM MSIRUNMODE_RESERVED15}
  MSIRUNMODE_SCHEDULED       = 16; // custom action call from install script execution
  {$EXTERNALSYM MSIRUNMODE_SCHEDULED}
  MSIRUNMODE_ROLLBACK        = 17; // custom action call from rollback execution script
  {$EXTERNALSYM MSIRUNMODE_ROLLBACK}
  MSIRUNMODE_COMMIT          = 18; // custom action call from commit execution script
  {$EXTERNALSYM MSIRUNMODE_COMMIT}

type
  MSIRUNMODE = DWORD;
  {$EXTERNALSYM MSIRUNMODE}
  TMsiRunMode = MSIRUNMODE;

const
  INSTALLMESSAGE_TYPEMASK = DWORD($FF000000);  // mask for type code
  {$EXTERNALSYM INSTALLMESSAGE_TYPEMASK}

// Note: INSTALLMESSAGE_ERROR, INSTALLMESSAGE_WARNING, INSTALLMESSAGE_USER are to or'd
// with a message box style to indicate the buttons to display and return:
// MB_OK,MB_OKCANCEL,MB_ABORTRETRYIGNORE,MB_YESNOCANCEL,MB_YESNO,MB_RETRYCANCEL
// the default button (MB_DEFBUTTON1 is normal default):
// MB_DEFBUTTON1, MB_DEFBUTTON2, MB_DEFBUTTON3
// and optionally an icon style:
// MB_ICONERROR, MB_ICONQUESTION, MB_ICONWARNING, MB_ICONINFORMATION

const
  MSITRANSFORM_ERROR_ADDEXISTINGROW   = $00000001;
  {$EXTERNALSYM MSITRANSFORM_ERROR_ADDEXISTINGROW}
  MSITRANSFORM_ERROR_DELMISSINGROW    = $00000002;
  {$EXTERNALSYM MSITRANSFORM_ERROR_DELMISSINGROW}
  MSITRANSFORM_ERROR_ADDEXISTINGTABLE = $00000004;
  {$EXTERNALSYM MSITRANSFORM_ERROR_ADDEXISTINGTABLE}
  MSITRANSFORM_ERROR_DELMISSINGTABLE  = $00000008;
  {$EXTERNALSYM MSITRANSFORM_ERROR_DELMISSINGTABLE}
  MSITRANSFORM_ERROR_UPDATEMISSINGROW = $00000010;
  {$EXTERNALSYM MSITRANSFORM_ERROR_UPDATEMISSINGROW}
  MSITRANSFORM_ERROR_CHANGECODEPAGE   = $00000020;
  {$EXTERNALSYM MSITRANSFORM_ERROR_CHANGECODEPAGE}
  MSITRANSFORM_ERROR_VIEWTRANSFORM    = $00000100;
  {$EXTERNALSYM MSITRANSFORM_ERROR_VIEWTRANSFORM}

type
  MSITRANSFORM_ERROR = DWORD;
  {$EXTERNALSYM MSITRANSFORM_ERROR}
  TMsiTransformError = MSITRANSFORM_ERROR;

const
  MSITRANSFORM_VALIDATE_LANGUAGE                   = $00000001;
  {$EXTERNALSYM MSITRANSFORM_VALIDATE_LANGUAGE}
  MSITRANSFORM_VALIDATE_PRODUCT                    = $00000002;
  {$EXTERNALSYM MSITRANSFORM_VALIDATE_PRODUCT}
  MSITRANSFORM_VALIDATE_PLATFORM                   = $00000004;
  {$EXTERNALSYM MSITRANSFORM_VALIDATE_PLATFORM}
  MSITRANSFORM_VALIDATE_MAJORVERSION               = $00000008;
  {$EXTERNALSYM MSITRANSFORM_VALIDATE_MAJORVERSION}
  MSITRANSFORM_VALIDATE_MINORVERSION               = $00000010;
  {$EXTERNALSYM MSITRANSFORM_VALIDATE_MINORVERSION}
  MSITRANSFORM_VALIDATE_UPDATEVERSION              = $00000020;
  {$EXTERNALSYM MSITRANSFORM_VALIDATE_UPDATEVERSION}
  MSITRANSFORM_VALIDATE_NEWLESSBASEVERSION         = $00000040;
  {$EXTERNALSYM MSITRANSFORM_VALIDATE_NEWLESSBASEVERSION}
  MSITRANSFORM_VALIDATE_NEWLESSEQUALBASEVERSION    = $00000080;
  {$EXTERNALSYM MSITRANSFORM_VALIDATE_NEWLESSEQUALBASEVERSION}
  MSITRANSFORM_VALIDATE_NEWEQUALBASEVERSION        = $00000100;
  {$EXTERNALSYM MSITRANSFORM_VALIDATE_NEWEQUALBASEVERSION}
  MSITRANSFORM_VALIDATE_NEWGREATEREQUALBASEVERSION = $00000200;
  {$EXTERNALSYM MSITRANSFORM_VALIDATE_NEWGREATEREQUALBASEVERSION}
  MSITRANSFORM_VALIDATE_NEWGREATERBASEVERSION      = $00000400;
  {$EXTERNALSYM MSITRANSFORM_VALIDATE_NEWGREATERBASEVERSION}
  MSITRANSFORM_VALIDATE_UPGRADECODE                = $00000800;
  {$EXTERNALSYM MSITRANSFORM_VALIDATE_UPGRADECODE}

type
  MSITRANSFORM_VALIDATE = DWORD;
  {$EXTERNALSYM MSITRANSFORM_VALIDATE}
  TMsiTransformValidate = MSITRANSFORM_VALIDATE;

// -----------------------------------------------------------------------------
// Installer database access functions
// -----------------------------------------------------------------------------

// Prepare a database query, creating a view object
// Returns ERROR_SUCCESS if successful, and the view handle is returned,
// else ERROR_INVALID_HANDLE, ERROR_INVALID_HANDLE_STATE, ERROR_BAD_QUERY_SYNTAX, ERROR_GEN_FAILURE
// Execution of this function sets the error record, accessible via MsiGetLastErrorRecord

function MsiDatabaseOpenViewA(hDatabase: MSIHANDLE; szQuery: LPCSTR; var phView: MSIHANDLE): UINT; stdcall;
{$EXTERNALSYM MsiDatabaseOpenViewA}
function MsiDatabaseOpenViewW(hDatabase: MSIHANDLE; szQuery: LPCWSTR; var phView: MSIHANDLE): UINT; stdcall;
{$EXTERNALSYM MsiDatabaseOpenViewW}
function MsiDatabaseOpenView(hDatabase: MSIHANDLE; szQuery: LPCTSTR; var phView: MSIHANDLE): UINT; stdcall;
{$EXTERNALSYM MsiDatabaseOpenView}

// Returns the MSIDBERROR enum and name of the column corresponding to the error
// Similar to a GetLastError function, but for the view. NOT the same as MsiGetLastErrorRecord
// Returns errors of MsiViewModify.

function MsiViewGetErrorA(hView: MSIHANDLE; szColumnNameBuffer: LPSTR;
  var pcchBuf: DWORD): MSIDBERROR; stdcall;
{$EXTERNALSYM MsiViewGetErrorA}
function MsiViewGetErrorW(hView: MSIHANDLE; szColumnNameBuffer: LPWSTR;
  var pcchBuf: DWORD): MSIDBERROR; stdcall;
{$EXTERNALSYM MsiViewGetErrorW}
function MsiViewGetError(hView: MSIHANDLE; szColumnNameBuffer: LPTSTR;
  var pcchBuf: DWORD): MSIDBERROR; stdcall;
{$EXTERNALSYM MsiViewGetError}

// Exectute the view query, supplying parameters as required
// Returns ERROR_SUCCESS, ERROR_INVALID_HANDLE, ERROR_INVALID_HANDLE_STATE, ERROR_GEN_FAILURE
// Execution of this function sets the error record, accessible via MsiGetLastErrorRecord

function MsiViewExecute(hView: MSIHANDLE; hRecord: MSIHANDLE): UINT; stdcall;
{$EXTERNALSYM MsiViewExecute}

// Fetch the next sequential record from the view
// Result is ERROR_SUCCESS if a row is found, and its handle is returned
// else ERROR_NO_MORE_ITEMS if no records remain, and a null handle is returned
// else result is error: ERROR_INVALID_HANDLE_STATE, ERROR_INVALID_HANDLE, ERROR_GEN_FAILURE

function MsiViewFetch(hView: MSIHANDLE; var phRecord: MSIHANDLE): UINT; stdcall;
{$EXTERNALSYM MsiViewFetch}

// Modify a database record, parameters must match types in query columns
// Returns ERROR_SUCCESS, ERROR_INVALID_HANDLE, ERROR_INVALID_HANDLE_STATE, ERROR_GEN_FAILURE, ERROR_ACCESS_DENIED
// Execution of this function sets the error record, accessible via MsiGetLastErrorRecord

function MsiViewModify(hView: MSIHANDLE; eModifyMode: MSIMODIFY; hRecord: MSIHANDLE): UINT; stdcall;
{$EXTERNALSYM MsiViewModify}

// Return the column names or specifications for the current view
// Returns ERROR_SUCCESS, ERROR_INVALID_HANDLE, ERROR_INVALID_PARAMETER, or ERROR_INVALID_HANDLE_STATE

function MsiViewGetColumnInfo(hView: MSIHANDLE; eColumnInfo: MSICOLINFO;
  var phRecord: MSIHANDLE): UINT; stdcall;
{$EXTERNALSYM MsiViewGetColumnInfo}

// Release the result set for an executed view, to allow re-execution
// Only needs to be called if not all records have been fetched
// Returns ERROR_SUCCESS, ERROR_INVALID_HANDLE, ERROR_INVALID_HANDLE_STATE

function MsiViewClose(hView: MSIHANDLE): UINT; stdcall;
{$EXTERNALSYM MsiViewClose}

// Return a record containing the names of all primary key columns for a given table
// Returns an MSIHANDLE for a record containing the name of each column.
// The field count of the record corresponds to the number of primary key columns.
// Field [0] of the record contains the table name.
// Returns ERROR_SUCCESS, ERROR_INVALID_HANDLE, ERROR_INVALID_TABLE

function MsiDatabaseGetPrimaryKeysA(hDatabase: MSIHANDLE; szTableName: LPCSTR;
  var phRecord: MSIHANDLE): UINT; stdcall;
{$EXTERNALSYM MsiDatabaseGetPrimaryKeysA}
function MsiDatabaseGetPrimaryKeysW(hDatabase: MSIHANDLE; szTableName: LPCWSTR;
  var phRecord: MSIHANDLE): UINT; stdcall;
{$EXTERNALSYM MsiDatabaseGetPrimaryKeysW}
function MsiDatabaseGetPrimaryKeys(hDatabase: MSIHANDLE; szTableName: LPCTSTR;
  var phRecord: MSIHANDLE): UINT; stdcall;
{$EXTERNALSYM MsiDatabaseGetPrimaryKeys}

// Return an enum defining the state of the table (temporary, unknown, or persistent).
// Returns MSICONDITION_ERROR, MSICONDITION_FALSE, MSICONDITION_TRUE, MSICONDITION_NONE

function MsiDatabaseIsTablePersistentA(hDatabase: MSIHANDLE; szTableName: LPCSTR): MSICONDITION; stdcall;
{$EXTERNALSYM MsiDatabaseIsTablePersistentA}
function MsiDatabaseIsTablePersistentW(hDatabase: MSIHANDLE; szTableName: LPCWSTR): MSICONDITION; stdcall;
{$EXTERNALSYM MsiDatabaseIsTablePersistentW}
function MsiDatabaseIsTablePersistent(hDatabase: MSIHANDLE; szTableName: LPCTSTR): MSICONDITION; stdcall;
{$EXTERNALSYM MsiDatabaseIsTablePersistent}

// --------------------------------------------------------------------------
// Summary information stream management functions
// --------------------------------------------------------------------------

// Integer Property IDs:    1, 14, 15, 16, 19
// DateTime Property IDs:   10, 11, 12, 13
// Text Property IDs:       2, 3, 4, 5, 6, 7, 8, 9, 18
// Unsupported Propery IDs: 0 (PID_DICTIONARY), 17 (PID_THUMBNAIL)

// Obtain a handle for the _SummaryInformation stream for an MSI database
// Execution of this function sets the error record, accessible via MsiGetLastErrorRecord

function MsiGetSummaryInformationA(hDatabase: MSIHANDLE; szDatabasePath: LPCSTR;
  uiUpdateCount: UINT; var phSummaryInfo: MSIHANDLE): UINT; stdcall;
{$EXTERNALSYM MsiGetSummaryInformationA}
function MsiGetSummaryInformationW(hDatabase: MSIHANDLE; szDatabasePath: LPCWSTR;
  uiUpdateCount: UINT; var phSummaryInfo: MSIHANDLE): UINT; stdcall;
{$EXTERNALSYM MsiGetSummaryInformationW}
function MsiGetSummaryInformation(hDatabase: MSIHANDLE; szDatabasePath: LPCTSTR;
  uiUpdateCount: UINT; var phSummaryInfo: MSIHANDLE): UINT; stdcall;
{$EXTERNALSYM MsiGetSummaryInformation}

// Obtain the number of existing properties in the SummaryInformation stream

function MsiSummaryInfoGetPropertyCount(hSummaryInfo: MSIHANDLE; var puiPropertyCount: UINT): UINT; stdcall;
{$EXTERNALSYM MsiSummaryInfoGetPropertyCount}

// Set a single summary information property
// Returns ERROR_SUCCESS, ERROR_INVALID_HANDLE, ERROR_UNKNOWN_PROPERTY

function MsiSummaryInfoSetPropertyA(hSummaryInfo: MSIHANDLE; uiProperty: UINT;
  uiDataType: UINT; iValue: Integer; const pftValue: FILETIME; szValue: LPCSTR): UINT; stdcall;
{$EXTERNALSYM MsiSummaryInfoSetPropertyA}
function MsiSummaryInfoSetPropertyW(hSummaryInfo: MSIHANDLE; uiProperty: UINT;
  uiDataType: UINT; iValue: Integer; const pftValue: FILETIME; szValue: LPCWSTR): UINT; stdcall;
{$EXTERNALSYM MsiSummaryInfoSetPropertyW}
function MsiSummaryInfoSetProperty(hSummaryInfo: MSIHANDLE; uiProperty: UINT;
  uiDataType: UINT; iValue: Integer; const pftValue: FILETIME; szValue: LPCTSTR): UINT; stdcall;
{$EXTERNALSYM MsiSummaryInfoSetProperty}

// Get a single property from the summary information
// Returns ERROR_SUCCESS, ERROR_INVALID_HANDLE, ERROR_UNKNOWN_PROPERTY

function MsiSummaryInfoGetPropertyA(hSummaryInfo: MSIHANDLE; uiProperty: UINT;
  var puiDataType: UINT; var piValue: Integer; var pftValue: FILETIME; szValueBuf: LPSTR;
  var pcchValueBuf: DWORD): UINT; stdcall;
{$EXTERNALSYM MsiSummaryInfoGetPropertyA}
function MsiSummaryInfoGetPropertyW(hSummaryInfo: MSIHANDLE; uiProperty: UINT;
  var puiDataType: UINT; var piValue: Integer; var pftValue: FILETIME; szValueBuf: LPWSTR;
  var pcchValueBuf: DWORD): UINT; stdcall;
{$EXTERNALSYM MsiSummaryInfoGetPropertyW}
function MsiSummaryInfoGetProperty(hSummaryInfo: MSIHANDLE; uiProperty: UINT;
  var puiDataType: UINT; var piValue: Integer; var pftValue: FILETIME; szValueBuf: LPTSTR;
  var pcchValueBuf: DWORD): UINT; stdcall;
{$EXTERNALSYM MsiSummaryInfoGetProperty}

// Write back changed information to summary information stream

function MsiSummaryInfoPersist(hSummaryInfo: MSIHANDLE): UINT; stdcall;
{$EXTERNALSYM MsiSummaryInfoPersist}

// --------------------------------------------------------------------------
// Installer database management functions - not used by custom actions
// --------------------------------------------------------------------------

// Open an installer database, specifying the persistance mode, which is a pointer.
// Predefined persist values are reserved pointer values, requiring pointer arithmetic.
// Execution of this function sets the error record, accessible via MsiGetLastErrorRecord

function MsiOpenDatabaseA(szDatabasePath: LPCSTR; szPersist: LPCSTR;
  var phDatabase: MSIHANDLE): UINT; stdcall;
{$EXTERNALSYM MsiOpenDatabaseA}
function MsiOpenDatabaseW(szDatabasePath: LPCWSTR; szPersist: LPCWSTR;
  var phDatabase: MSIHANDLE): UINT; stdcall;
{$EXTERNALSYM MsiOpenDatabaseW}
function MsiOpenDatabase(szDatabasePath: LPCTSTR; szPersist: LPCTSTR;
  var phDatabase: MSIHANDLE): UINT; stdcall;
{$EXTERNALSYM MsiOpenDatabase}

// Import an MSI text archive table into an open database
// Execution of this function sets the error record, accessible via MsiGetLastErrorRecord

function MsiDatabaseImportA(hDatabase: MSIHANDLE; szFolderPath: LPCSTR;
  szFileName: LPCSTR): UINT; stdcall;
{$EXTERNALSYM MsiDatabaseImportA}
function MsiDatabaseImportW(hDatabase: MSIHANDLE; szFolderPath: LPCWSTR;
  szFileName: LPCWSTR): UINT; stdcall;
{$EXTERNALSYM MsiDatabaseImportW}
function MsiDatabaseImport(hDatabase: MSIHANDLE; szFolderPath: LPCTSTR;
  szFileName: LPCTSTR): UINT; stdcall;
{$EXTERNALSYM MsiDatabaseImport}

// Export an MSI table from an open database to a text archive file
// Execution of this function sets the error record, accessible via MsiGetLastErrorRecord

function MsiDatabaseExportA(hDatabase: MSIHANDLE; szTableName: LPCSTR;
  szFolderPath: LPCSTR; szFileName: LPCSTR): UINT; stdcall;
{$EXTERNALSYM MsiDatabaseExportA}
function MsiDatabaseExportW(hDatabase: MSIHANDLE; szTableName: LPCWSTR;
  szFolderPath: LPCWSTR; szFileName: LPCWSTR): UINT; stdcall;
{$EXTERNALSYM MsiDatabaseExportW}
function MsiDatabaseExport(hDatabase: MSIHANDLE; szTableName: LPCTSTR;
  szFolderPath: LPCTSTR; szFileName: LPCTSTR): UINT; stdcall;
{$EXTERNALSYM MsiDatabaseExport}

// Merge two database together, allowing duplicate rows
// Execution of this function sets the error record, accessible via MsiGetLastErrorRecord

function MsiDatabaseMergeA(hDatabase: MSIHANDLE; hDatabaseMerge: MSIHANDLE;
  szTableName: LPCSTR): UINT; stdcall;
{$EXTERNALSYM MsiDatabaseMergeA}
function MsiDatabaseMergeW(hDatabase: MSIHANDLE; hDatabaseMerge: MSIHANDLE;
  szTableName: LPCWSTR): UINT; stdcall;
{$EXTERNALSYM MsiDatabaseMergeW}
function MsiDatabaseMerge(hDatabase: MSIHANDLE; hDatabaseMerge: MSIHANDLE;
  szTableName: LPCTSTR): UINT; stdcall;
{$EXTERNALSYM MsiDatabaseMerge}

// Generate a transform file of differences between two databases
// Execution of this function sets the error record, accessible via MsiGetLastErrorRecord

function MsiDatabaseGenerateTransformA(hDatabase: MSIHANDLE; hDatabaseReference: MSIHANDLE;
  szTransformFile: LPCSTR; iReserved1: Integer; iReserved2: Integer): UINT; stdcall;
{$EXTERNALSYM MsiDatabaseGenerateTransformA}
function MsiDatabaseGenerateTransformW(hDatabase: MSIHANDLE; hDatabaseReference: MSIHANDLE;
  szTransformFile: LPCWSTR; iReserved1: Integer; iReserved2: Integer): UINT; stdcall;
{$EXTERNALSYM MsiDatabaseGenerateTransformW}
function MsiDatabaseGenerateTransform(hDatabase: MSIHANDLE; hDatabaseReference: MSIHANDLE;
  szTransformFile: LPCTSTR; iReserved1: Integer; iReserved2: Integer): UINT; stdcall;
{$EXTERNALSYM MsiDatabaseGenerateTransform}

// Apply a transform file containing database difference
// Execution of this function sets the error record, accessible via MsiGetLastErrorRecord

function MsiDatabaseApplyTransformA(hDatabase: MSIHANDLE; szTransformFile: LPCSTR;
  iErrorConditions: Integer): UINT; stdcall;
{$EXTERNALSYM MsiDatabaseApplyTransformA}
function MsiDatabaseApplyTransformW(hDatabase: MSIHANDLE; szTransformFile: LPCWSTR;
  iErrorConditions: Integer): UINT; stdcall;
{$EXTERNALSYM MsiDatabaseApplyTransformW}
function MsiDatabaseApplyTransform(hDatabase: MSIHANDLE; szTransformFile: LPCTSTR;
  iErrorConditions: Integer): UINT; stdcall;
{$EXTERNALSYM MsiDatabaseApplyTransform}

// Create summary information of existing transform to include validation and error conditions
// Execution of this function sets the error record, accessible via MsiGetLastErrorRecord

function MsiCreateTransformSummaryInfoA(hDatabase: MSIHANDLE; hDatabaseReference: MSIHANDLE;
  szTransformFile: LPCSTR; iErrorConditions: Integer; iValidation: Integer): UINT; stdcall;
{$EXTERNALSYM MsiCreateTransformSummaryInfoA}
function MsiCreateTransformSummaryInfoW(hDatabase: MSIHANDLE; hDatabaseReference: MSIHANDLE;
  szTransformFile: LPCWSTR; iErrorConditions: Integer; iValidation: Integer): UINT; stdcall;
{$EXTERNALSYM MsiCreateTransformSummaryInfoW}
function MsiCreateTransformSummaryInfo(hDatabase: MSIHANDLE; hDatabaseReference: MSIHANDLE;
  szTransformFile: LPCTSTR; iErrorConditions: Integer; iValidation: Integer): UINT; stdcall;
{$EXTERNALSYM MsiCreateTransformSummaryInfo}

// Write out all persistent table data, ignored if database opened read-only
// Execution of this function sets the error record, accessible via MsiGetLastErrorRecord

function MsiDatabaseCommit(hDatabase: MSIHANDLE): UINT; stdcall;
{$EXTERNALSYM MsiDatabaseCommit}

// Return the update state of a database

function MsiGetDatabaseState(hDatabase: MSIHANDLE): MSIDBSTATE; stdcall;
{$EXTERNALSYM MsiGetDatabaseState}

// --------------------------------------------------------------------------
// Record object functions
// --------------------------------------------------------------------------

// Create a new record object with the requested number of fields
// Field 0, not included in count, is used for format strings and op codes
// All fields are initialized to null
// Returns a handle to the created record, or 0 if memory could not be allocated

function MsiCreateRecord(cParams: UINT): MSIHANDLE; stdcall;
{$EXTERNALSYM MsiCreateRecord}

// Report whether a record field is NULL
// Returns TRUE if the field is null or does not exist
// Returns FALSE if the field contains data, or the handle is invalid

function MsiRecordIsNull(hRecord: MSIHANDLE; iField: UINT): BOOL; stdcall;
{$EXTERNALSYM MsiRecordIsNull}

// Return the length of a record field
// Returns 0 if field is NULL or non-existent
// Returns sizeof(Integer) if integer data
// Returns character count if string data (not counting null terminator)
// Returns bytes count if stream data

function MsiRecordDataSize(hRecord: MSIHANDLE; iField: UINT): UINT; stdcall;
{$EXTERNALSYM MsiRecordDataSize}

// Set a record field to an integer value
// Returns ERROR_SUCCESS, ERROR_INVALID_HANDLE, ERROR_INVALID_FIELD

function MsiRecordSetInteger(hRecord: MSIHANDLE; iField: UINT; iValue: Integer): UINT; stdcall;
{$EXTERNALSYM MsiRecordSetInteger}

// Copy a string into the designated field
// A null string pointer and an empty string both set the field to null
// Returns ERROR_SUCCESS, ERROR_INVALID_HANDLE, ERROR_INVALID_FIELD

function MsiRecordSetStringA(hRecord: MSIHANDLE; iField: UINT; szValue: LPCSTR): UINT; stdcall;
{$EXTERNALSYM MsiRecordSetStringA}
function MsiRecordSetStringW(hRecord: MSIHANDLE; iField: UINT; szValue: LPCWSTR): UINT; stdcall;
{$EXTERNALSYM MsiRecordSetStringW}
function MsiRecordSetString(hRecord: MSIHANDLE; iField: UINT; szValue: LPCTSTR): UINT; stdcall;
{$EXTERNALSYM MsiRecordSetString}

// Return the integer value from a record field
// Returns the value MSI_NULL_INTEGER if the field is null
// or if the field is a string that cannot be converted to an integer

function MsiRecordGetInteger(hRecord: MSIHANDLE; iField: UINT): Integer; stdcall;
{$EXTERNALSYM MsiRecordGetInteger}

// Return the string value of a record field
// Integer fields will be converted to a string
// Null and non-existent fields will report a value of 0
// Fields containing stream data will return ERROR_INVALID_DATATYPE
// Returns ERROR_SUCCESS, ERROR_MORE_DATA,
//         ERROR_INVALID_HANDLE, ERROR_INVALID_FIELD, ERROR_BAD_ARGUMENTS

function MsiRecordGetStringA(hRecord: MSIHANDLE; iField: UINT; szValueBuf: LPSTR;
  var pcchValueBuf: DWORD): UINT; stdcall;
{$EXTERNALSYM MsiRecordGetStringA}
function MsiRecordGetStringW(hRecord: MSIHANDLE; iField: UINT; szValueBuf: LPWSTR;
  var pcchValueBuf: DWORD): UINT; stdcall;
{$EXTERNALSYM MsiRecordGetStringW}
function MsiRecordGetString(hRecord: MSIHANDLE; iField: UINT; szValueBuf: LPTSTR;
  var pcchValueBuf: DWORD): UINT; stdcall;
{$EXTERNALSYM MsiRecordGetString}

// Returns the number of fields allocated in the record
// Does not count field 0, used for formatting and op codes

function MsiRecordGetFieldCount(hRecord: MSIHANDLE): UINT; stdcall;
{$EXTERNALSYM MsiRecordGetFieldCount}

// Set a record stream field from a file
// The contents of the specified file will be read into a stream object
// The stream will be persisted if the record is inserted into the database
// Execution of this function sets the error record, accessible via MsiGetLastErrorRecord

function MsiRecordSetStreamA(hRecord: MSIHANDLE; iField: UINT; szFilePath: LPCSTR): UINT; stdcall;
{$EXTERNALSYM MsiRecordSetStreamA}
function MsiRecordSetStreamW(hRecord: MSIHANDLE; iField: UINT; szFilePath: LPCWSTR): UINT; stdcall;
{$EXTERNALSYM MsiRecordSetStreamW}
function MsiRecordSetStream(hRecord: MSIHANDLE; iField: UINT; szFilePath: LPCTSTR): UINT; stdcall;
{$EXTERNALSYM MsiRecordSetStream}

// Read bytes from a record stream field into a buffer
// Must set the in/out argument to the requested byte count to read
// The number of bytes transferred is returned through the argument
// If no more bytes are available, ERROR_SUCCESS is still returned

function MsiRecordReadStream(hRecord: MSIHANDLE; iField: UINT; szDataBuf: PAnsiChar;
  var pcbDataBuf: DWORD): UINT; stdcall;
{$EXTERNALSYM MsiRecordReadStream}

// Clears all data fields in a record to NULL

function MsiRecordClearData(hRecord: MSIHANDLE): UINT; stdcall;
{$EXTERNALSYM MsiRecordClearData}

// --------------------------------------------------------------------------
// Functions to access a running installation, called from custom actions
// The install handle is the single argument passed to custom actions
// --------------------------------------------------------------------------

// Return a handle to the database currently in use by this installer instance

function MsiGetActiveDatabase(hInstall: MSIHANDLE): MSIHANDLE; stdcall;
{$EXTERNALSYM MsiGetActiveDatabase}

// Set the value for an installer property
// If the property is not defined, it will be created
// If the value is null or an empty string, the property will be removed
// Returns ERROR_SUCCESS, ERROR_INVALID_HANDLE, ERROR_BAD_ARGUMENTS

function MsiSetPropertyA(hInstall: MSIHANDLE; szName: LPCSTR; szValue: LPCSTR): UINT; stdcall;
{$EXTERNALSYM MsiSetPropertyA}
function MsiSetPropertyW(hInstall: MSIHANDLE; szName: LPCWSTR; szValue: LPCWSTR): UINT; stdcall;
{$EXTERNALSYM MsiSetPropertyW}
function MsiSetProperty(hInstall: MSIHANDLE; szName: LPCTSTR; szValue: LPCTSTR): UINT; stdcall;
{$EXTERNALSYM MsiSetProperty}

// Get the value for an installer property
// If the property is not defined, it is equivalent to a 0-length value, not error
// Returns ERROR_SUCCESS, ERROR_MORE_DATA, ERROR_INVALID_HANDLE, ERROR_BAD_ARGUMENTS

function MsiGetPropertyA(hInstall: MSIHANDLE; szName: LPCSTR; szValueBuf: LPSTR;
  var pcchValueBuf: DWORD): UINT; stdcall;
{$EXTERNALSYM MsiGetPropertyA}
function MsiGetPropertyW(hInstall: MSIHANDLE; szName: LPCWSTR; szValueBuf: LPWSTR;
  var pcchValueBuf: DWORD): UINT; stdcall;
{$EXTERNALSYM MsiGetPropertyW}
function MsiGetProperty(hInstall: MSIHANDLE; szName: LPCTSTR; szValueBuf: LPTSTR;
  var pcchValueBuf: DWORD): UINT; stdcall;
{$EXTERNALSYM MsiGetProperty}

// Return the numeric language for the currently running install
// Returns 0 if an install not running

function MsiGetLanguage(hInstall: MSIHANDLE): LANGID; stdcall;
{$EXTERNALSYM MsiGetLanguage}

// Return one of the boolean internal installer states
// Returns FALSE if the handle is not active or if the mode is not implemented

function MsiGetMode(hInstall: MSIHANDLE; eRunMode: MSIRUNMODE): BOOL; stdcall;
{$EXTERNALSYM MsiGetMode}

// Set an internal install session boolean mode - Note: most modes are read-only
// Returns ERROR_SUCCESS if the mode can be set to the desired state
// Returns ERROR_ACCESS_DENIED if the mode is not settable
// Returns ERROR_INVALID_HANDLE if the handle is not an active install session

function MsiSetMode(hInstall: MSIHANDLE; eRunMode: MSIRUNMODE; fState: BOOL): UINT; stdcall;
{$EXTERNALSYM MsiSetMode}

// Format record data using a format string containing field markers and/or properties
// Record field 0 must contain the format string
// Other fields must contain data that may be referenced by the format string.

function MsiFormatRecordA(hInstall: MSIHANDLE; hRecord: MSIHANDLE; szResultBuf: LPSTR;
  var pcchResultBuf: DWORD): UINT; stdcall;
{$EXTERNALSYM MsiFormatRecordA}
function MsiFormatRecordW(hInstall: MSIHANDLE; hRecord: MSIHANDLE; szResultBuf: LPWSTR;
  var pcchResultBuf: DWORD): UINT; stdcall;
{$EXTERNALSYM MsiFormatRecordW}
function MsiFormatRecord(hInstall: MSIHANDLE; hRecord: MSIHANDLE; szResultBuf: LPTSTR;
  var pcchResultBuf: DWORD): UINT; stdcall;
{$EXTERNALSYM MsiFormatRecord}

// Execute another action, either built-in, custom, or UI wizard
// Returns ERROR_FUNCTION_NOT_CALLED if action not found
// Returns ERROR_SUCCESS if action completed succesfully
// Returns ERROR_INSTALL_USEREXIT if user cancelled during action
// Returns ERROR_INSTALL_FAILURE if action failed
// Returns ERROR_INSTALL_SUSPEND if user suspended installation
// Returns ERROR_MORE_DATA if action wishes to skip remaining actions
// Returns ERROR_INVALID_HANDLE_STATE if install session not active
// Returns ERROR_INVALID_DATA if failure calling custom action
// Returns ERROR_INVALID_HANDLE or ERROR_INVALID_PARAMETER if arguments invalid

function MsiDoActionA(hInstall: MSIHANDLE; szAction: LPCSTR): UINT; stdcall;
{$EXTERNALSYM MsiDoActionA}
function MsiDoActionW(hInstall: MSIHANDLE; szAction: LPCWSTR): UINT; stdcall;
{$EXTERNALSYM MsiDoActionW}
function MsiDoAction(hInstall: MSIHANDLE; szAction: LPCTSTR): UINT; stdcall;
{$EXTERNALSYM MsiDoAction}

// Execute another action sequence, as descibed in the specified table
// Returns the same error codes as MsiDoAction

function MsiSequenceA(hInstall: MSIHANDLE; szTable: LPCSTR; iSequenceMode: Integer): UINT; stdcall;
{$EXTERNALSYM MsiSequenceA}
function MsiSequenceW(hInstall: MSIHANDLE; szTable: LPCWSTR; iSequenceMode: Integer): UINT; stdcall;
{$EXTERNALSYM MsiSequenceW}
function MsiSequence(hInstall: MSIHANDLE; szTable: LPCTSTR; iSequenceMode: Integer): UINT; stdcall;
{$EXTERNALSYM MsiSequence}

// Send an error record to the installer for processing.
// If field 0 (template) is not set, field 1 must be set to the error code,
//   corresponding the the error message in the Error database table,
//   and the message will be formatted using the template from the Error table
//   before passing it to the UI handler for display.
// Returns Win32 button codes: IDOK IDCANCEL IDABORT IDRETRY IDIGNORE IDYES IDNO
//   or 0 if no action taken, or -1 if invalid argument or handle

function MsiProcessMessage(hInstall: MSIHANDLE; eMessageType: INSTALLMESSAGE;
  hRecord: MSIHANDLE): Integer; stdcall;
{$EXTERNALSYM MsiProcessMessage}

// Evaluate a conditional expression containing property names and values

function MsiEvaluateConditionA(hInstall: MSIHANDLE; szCondition: LPCSTR): MSICONDITION; stdcall;
{$EXTERNALSYM MsiEvaluateConditionA}
function MsiEvaluateConditionW(hInstall: MSIHANDLE; szCondition: LPCWSTR): MSICONDITION; stdcall;
{$EXTERNALSYM MsiEvaluateConditionW}
function MsiEvaluateCondition(hInstall: MSIHANDLE; szCondition: LPCTSTR): MSICONDITION; stdcall;
{$EXTERNALSYM MsiEvaluateCondition}

// Get the installed state and requested action state of a feature
// Execution of this function sets the error record, accessible via MsiGetLastErrorRecord

function MsiGetFeatureStateA(hInstall: MSIHANDLE; szFeature: LPCSTR;
  var piInstalled: INSTALLSTATE; var piAction: INSTALLSTATE): UINT; stdcall;
{$EXTERNALSYM MsiGetFeatureStateA}
function MsiGetFeatureStateW(hInstall: MSIHANDLE; szFeature: LPCWSTR;
  var piInstalled: INSTALLSTATE; var piAction: INSTALLSTATE): UINT; stdcall;
{$EXTERNALSYM MsiGetFeatureStateW}
function MsiGetFeatureState(hInstall: MSIHANDLE; szFeature: LPCTSTR;
  var piInstalled: INSTALLSTATE; var piAction: INSTALLSTATE): UINT; stdcall;
{$EXTERNALSYM MsiGetFeatureState}

// Request a feature to be set to a specified state
// Execution of this function sets the error record, accessible via MsiGetLastErrorRecord

function MsiSetFeatureStateA(hInstall: MSIHANDLE; szFeature: LPCSTR;
  iState: INSTALLSTATE): UINT; stdcall;
{$EXTERNALSYM MsiSetFeatureStateA}
function MsiSetFeatureStateW(hInstall: MSIHANDLE; szFeature: LPCWSTR;
  iState: INSTALLSTATE): UINT; stdcall;
{$EXTERNALSYM MsiSetFeatureStateW}
function MsiSetFeatureState(hInstall: MSIHANDLE; szFeature: LPCTSTR;
  iState: INSTALLSTATE): UINT; stdcall;
{$EXTERNALSYM MsiSetFeatureState}

// Set the attribute bits of a specified feature at runtime.
// Execution of this function sets the error record, accessible via MsiGetLastErrorRecord

function MsiSetFeatureAttributesA(hInstall: MSIHANDLE; szFeature: LPCSTR;
  dwAttributes: DWORD): UINT; stdcall;
{$EXTERNALSYM MsiSetFeatureAttributesA}
function MsiSetFeatureAttributesW(hInstall: MSIHANDLE; szFeature: LPCWSTR;
  dwAttributes: DWORD): UINT; stdcall;
{$EXTERNALSYM MsiSetFeatureAttributesW}
function MsiSetFeatureAttributes(hInstall: MSIHANDLE; szFeature: LPCTSTR;
  dwAttributes: DWORD): UINT; stdcall;
{$EXTERNALSYM MsiSetFeatureAttributes}

// Get the installed state and requested action state of a component
// Execution of this function sets the error record, accessible via MsiGetLastErrorRecord

function MsiGetComponentStateA(hInstall: MSIHANDLE; szComponent: LPCSTR;
  var piInstalled: INSTALLSTATE; var piAction: INSTALLSTATE): UINT; stdcall;
{$EXTERNALSYM MsiGetComponentStateA}
function MsiGetComponentStateW(hInstall: MSIHANDLE; szComponent: LPCWSTR;
  var piInstalled: INSTALLSTATE; var piAction: INSTALLSTATE): UINT; stdcall;
{$EXTERNALSYM MsiGetComponentStateW}
function MsiGetComponentState(hInstall: MSIHANDLE; szComponent: LPCTSTR;
  var piInstalled: INSTALLSTATE; var piAction: INSTALLSTATE): UINT; stdcall;
{$EXTERNALSYM MsiGetComponentState}

// Request a component to be set to a specified state
// Execution of this function sets the error record, accessible via MsiGetLastErrorRecord

function MsiSetComponentStateA(hInstall: MSIHANDLE; szComponent: LPCSTR;
  iState: INSTALLSTATE): UINT; stdcall;
{$EXTERNALSYM MsiSetComponentStateA}
function MsiSetComponentStateW(hInstall: MSIHANDLE; szComponent: LPCWSTR;
  iState: INSTALLSTATE): UINT; stdcall;
{$EXTERNALSYM MsiSetComponentStateW}
function MsiSetComponentState(hInstall: MSIHANDLE; szComponent: LPCTSTR;
  iState: INSTALLSTATE): UINT; stdcall;
{$EXTERNALSYM MsiSetComponentState}

// Return the disk cost for a feature and related features
// Can specify either current feature state or proposed state
// Can specify extent of related features to cost
// Note that adding costs for several features may produce an
// excessively large cost due to shared components and parents.
// Execution of this function sets the error record, accessible via MsiGetLastErrorRecord

function MsiGetFeatureCostA(hInstall: MSIHANDLE; szFeature: LPCSTR;
  iCostTree: MSICOSTTREE; iState: INSTALLSTATE; var piCost: Integer): UINT; stdcall;
{$EXTERNALSYM MsiGetFeatureCostA}
function MsiGetFeatureCostW(hInstall: MSIHANDLE; szFeature: LPCWSTR;
  iCostTree: MSICOSTTREE; iState: INSTALLSTATE; var piCost: Integer): UINT; stdcall;
{$EXTERNALSYM MsiGetFeatureCostW}
function MsiGetFeatureCost(hInstall: MSIHANDLE; szFeature: LPCTSTR;
  iCostTree: MSICOSTTREE; iState: INSTALLSTATE; var piCost: Integer): UINT; stdcall;
{$EXTERNALSYM MsiGetFeatureCost}

// Enumerates the costs and temporary costs per drives for
// szComponent. If szComponent is set to NULL, it enumerates
// the above costs for the engine, per drives.
//
// The enumeration is 0-based, i.e. it returns the data for
// the first drive when called w/ dwIndex set to 0.
//
// Can specify either current feature state or proposed state.
//
// Execution of this function sets the error record, accessible
// via MsiGetLastErrorRecord.

function MsiEnumComponentCostsA(hInstall: MSIHANDLE; szComponent: LPCSTR; dwIndex: DWORD; iState: INSTALLSTATE;
  szDriveBuf: LPSTR; var pcchDriveBuf: DWORD; var piCost: INT; piTempCost: INT): UINT; stdcall;
{$EXTERNALSYM MsiEnumComponentCostsA}
function MsiEnumComponentCostsW(hInstall: MSIHANDLE; szComponent: LPCWSTR; dwIndex: DWORD; iState: INSTALLSTATE;
  szDriveBuf: LPWSTR; var pcchDriveBuf: DWORD; var piCost: INT; var piTempCost: INT): UINT; stdcall;
{$EXTERNALSYM MsiEnumComponentCostsW}
function MsiEnumComponentCosts(hInstall: MSIHANDLE; szComponent: LPCTSTR; dwIndex: DWORD; iState: INSTALLSTATE;
  szDriveBuf: LPTSTR; var pcchDriveBuf: DWORD; var piCost: INT; var piTempCost: INT): UINT; stdcall;
{$EXTERNALSYM MsiEnumComponentCosts}

// Set the install level for a full product installation (not a feature request)
// Setting the value to 0 initialized components and features to the default level
// Execution of this function sets the error record, accessible via MsiGetLastErrorRecord

function MsiSetInstallLevel(hInstall: MSIHANDLE; iInstallLevel: Integer): UINT; stdcall;
{$EXTERNALSYM MsiSetInstallLevel}

// Get the valid install states for a feature, represented by bit flags
// For each valid install state, a bit is set of value: (1 << INSTALLSTATE)
// Execution of this function sets the error record, accessible via MsiGetLastErrorRecord

function MsiGetFeatureValidStatesA(hInstall: MSIHANDLE; szFeature: LPCSTR;
  var dwInstallStates: DWORD): UINT; stdcall;
{$EXTERNALSYM MsiGetFeatureValidStatesA}
function MsiGetFeatureValidStatesW(hInstall: MSIHANDLE; szFeature: LPCWSTR;
  var dwInstallStates: DWORD): UINT; stdcall;
{$EXTERNALSYM MsiGetFeatureValidStatesW}
function MsiGetFeatureValidStates(hInstall: MSIHANDLE; szFeature: LPCTSTR;
  var dwInstallStates: DWORD): UINT; stdcall;
{$EXTERNALSYM MsiGetFeatureValidStates}

// Return the full source path for a folder in the Directory table
// Execution of this function sets the error record, accessible via MsiGetLastErrorRecord

function MsiGetSourcePathA(hInstall: MSIHANDLE; szFolder: LPCSTR; szPathBuf: LPSTR;
  var pcchPathBuf: DWORD): UINT; stdcall;
{$EXTERNALSYM MsiGetSourcePathA}
function MsiGetSourcePathW(hInstall: MSIHANDLE; szFolder: LPCWSTR; szPathBuf: LPWSTR;
  var pcchPathBuf: DWORD): UINT; stdcall;
{$EXTERNALSYM MsiGetSourcePathW}
function MsiGetSourcePath(hInstall: MSIHANDLE; szFolder: LPCTSTR; szPathBuf: LPTSTR;
  var pcchPathBuf: DWORD): UINT; stdcall;
{$EXTERNALSYM MsiGetSourcePath}

// Return the full target path for a folder in the Directory table
// Execution of this function sets the error record, accessible via MsiGetLastErrorRecord

function MsiGetTargetPathA(hInstall: MSIHANDLE; szFolder: LPCSTR;
  szPathBuf: LPSTR; var pcchPathBuf: DWORD): UINT; stdcall;
{$EXTERNALSYM MsiGetTargetPathA}
function MsiGetTargetPathW(hInstall: MSIHANDLE; szFolder: LPCWSTR;
  szPathBuf: LPWSTR; var pcchPathBuf: DWORD): UINT; stdcall;
{$EXTERNALSYM MsiGetTargetPathW}
function MsiGetTargetPath(hInstall: MSIHANDLE; szFolder: LPCTSTR;
  szPathBuf: LPTSTR; var pcchPathBuf: DWORD): UINT; stdcall;
{$EXTERNALSYM MsiGetTargetPath}

// Set the full target path for a folder in the Directory table
// Execution of this function sets the error record, accessible via MsiGetLastErrorRecord

function MsiSetTargetPathA(hInstall: MSIHANDLE; szFolder: LPCSTR;
  szFolderPath: LPCSTR): UINT; stdcall;
{$EXTERNALSYM MsiSetTargetPathA}
function MsiSetTargetPathW(hInstall: MSIHANDLE; szFolder: LPCWSTR;
  szFolderPath: LPCWSTR): UINT; stdcall;
{$EXTERNALSYM MsiSetTargetPathW}
function MsiSetTargetPath(hInstall: MSIHANDLE; szFolder: LPCTSTR;
  szFolderPath: LPCTSTR): UINT; stdcall;
{$EXTERNALSYM MsiSetTargetPath}

// Check to see if sufficent disk space is present for the current installation
// Returns ERROR_SUCCESS, ERROR_DISK_FULL, ERROR_INVALID_HANDLE_STATE, or ERROR_INVALID_HANDLE

function MsiVerifyDiskSpace(hInstall: MSIHANDLE): UINT; stdcall;
{$EXTERNALSYM MsiVerifyDiskSpace}

// --------------------------------------------------------------------------
// Functions for rendering UI dialogs from the database representations.
// Purpose is for product development, not for use during installation.
// --------------------------------------------------------------------------

// Enable UI in preview mode to facilitate authoring of UI dialogs.
// The preview mode will end when the handle is closed.

function MsiEnableUIPreview(hDatabase: MSIHANDLE; var phPreview: MSIHANDLE): UINT; stdcall;
{$EXTERNALSYM MsiEnableUIPreview}

// Display any UI dialog as modeless and inactive.
// Supplying a null name will remove any current dialog.

function MsiPreviewDialogA(hPreview: MSIHANDLE; szDialogName: LPCSTR): UINT; stdcall;
{$EXTERNALSYM MsiPreviewDialogA}
function MsiPreviewDialogW(hPreview: MSIHANDLE; szDialogName: LPCWSTR): UINT; stdcall;
{$EXTERNALSYM MsiPreviewDialogW}
function MsiPreviewDialog(hPreview: MSIHANDLE; szDialogName: LPCTSTR): UINT; stdcall;
{$EXTERNALSYM MsiPreviewDialog}

// Display a billboard within a host control in the displayed dialog.
// Supplying a null billboard name will remove any billboard displayed.

function MsiPreviewBillboardA(hPreview: MSIHANDLE; szControlName: LPCSTR;
  szBillboard: LPCSTR): UINT; stdcall;
{$EXTERNALSYM MsiPreviewBillboardA}
function MsiPreviewBillboardW(hPreview: MSIHANDLE; szControlName: LPCWSTR;
  szBillboard: LPCWSTR): UINT; stdcall;
{$EXTERNALSYM MsiPreviewBillboardW}
function MsiPreviewBillboard(hPreview: MSIHANDLE; szControlName: LPCTSTR;
  szBillboard: LPCTSTR): UINT; stdcall;
{$EXTERNALSYM MsiPreviewBillboard}

// --------------------------------------------------------------------------
// Error handling not associated with any particular object
// --------------------------------------------------------------------------

// Return a record handle to the last function that generated an error record
// Only specified functions will set the error record, or clear it if success
// Field 1 of the record will contain the internal MSI error code
// Other fields will contain data specific to the particular error
// The error record is released internally after this function is executed

function MsiGetLastErrorRecord: MSIHANDLE; stdcall;
{$EXTERNALSYM MsiGetLastErrorRecord}

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  msilib = 'msi.dll';
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}


{$IFDEF DYNAMIC_LINK}

var
  _MsiDatabaseOpenViewA: Pointer;

function MsiDatabaseOpenViewA;
begin
  GetProcedureAddress(_MsiDatabaseOpenViewA, msilib, 'MsiDatabaseOpenViewA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDatabaseOpenViewA]
  end;
end;

var
  _MsiDatabaseOpenViewW: Pointer;

function MsiDatabaseOpenViewW;
begin
  GetProcedureAddress(_MsiDatabaseOpenViewW, msilib, 'MsiDatabaseOpenViewW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDatabaseOpenViewW]
  end;
end;

var
  _MsiDatabaseOpenView: Pointer;

function MsiDatabaseOpenView;
begin
  GetProcedureAddress(_MsiDatabaseOpenView, msilib, 'MsiDatabaseOpenView' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDatabaseOpenView]
  end;
end;

var
  _MsiViewGetErrorA: Pointer;

function MsiViewGetErrorA;
begin
  GetProcedureAddress(_MsiViewGetErrorA, msilib, 'MsiViewGetErrorA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiViewGetErrorA]
  end;
end;

var
  _MsiViewGetErrorW: Pointer;

function MsiViewGetErrorW;
begin
  GetProcedureAddress(_MsiViewGetErrorW, msilib, 'MsiViewGetErrorW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiViewGetErrorW]
  end;
end;

var
  _MsiViewGetError: Pointer;

function MsiViewGetError;
begin
  GetProcedureAddress(_MsiViewGetError, msilib, 'MsiViewGetError' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiViewGetError]
  end;
end;

var
  _MsiViewExecute: Pointer;

function MsiViewExecute;
begin
  GetProcedureAddress(_MsiViewExecute, msilib, 'MsiViewExecute');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiViewExecute]
  end;
end;

var
  _MsiViewFetch: Pointer;

function MsiViewFetch;
begin
  GetProcedureAddress(_MsiViewFetch, msilib, 'MsiViewFetch');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiViewFetch]
  end;
end;

var
  _MsiViewModify: Pointer;

function MsiViewModify;
begin
  GetProcedureAddress(_MsiViewModify, msilib, 'MsiViewModify');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiViewModify]
  end;
end;

var
  _MsiViewGetColumnInfo: Pointer;

function MsiViewGetColumnInfo;
begin
  GetProcedureAddress(_MsiViewGetColumnInfo, msilib, 'MsiViewGetColumnInfo');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiViewGetColumnInfo]
  end;
end;

var
  _MsiViewClose: Pointer;

function MsiViewClose;
begin
  GetProcedureAddress(_MsiViewClose, msilib, 'MsiViewClose');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiViewClose]
  end;
end;

var
  _MsiDatabaseGetPrimaryKeysA: Pointer;

function MsiDatabaseGetPrimaryKeysA;
begin
  GetProcedureAddress(_MsiDatabaseGetPrimaryKeysA, msilib, 'MsiDatabaseGetPrimaryKeysA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDatabaseGetPrimaryKeysA]
  end;
end;

var
  _MsiDatabaseGetPrimaryKeysW: Pointer;

function MsiDatabaseGetPrimaryKeysW;
begin
  GetProcedureAddress(_MsiDatabaseGetPrimaryKeysW, msilib, 'MsiDatabaseGetPrimaryKeysW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDatabaseGetPrimaryKeysW]
  end;
end;

var
  _MsiDatabaseGetPrimaryKeys: Pointer;

function MsiDatabaseGetPrimaryKeys;
begin
  GetProcedureAddress(_MsiDatabaseGetPrimaryKeys, msilib, 'MsiDatabaseGetPrimaryKeys' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDatabaseGetPrimaryKeys]
  end;
end;

var
  _MsiDatabaseIsTablePersistentA: Pointer;

function MsiDatabaseIsTablePersistentA;
begin
  GetProcedureAddress(_MsiDatabaseIsTablePersistentA, msilib, 'MsiDatabaseIsTablePersistentA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDatabaseIsTablePersistentA]
  end;
end;

var
  _MsiDatabaseIsTablePersistentW: Pointer;

function MsiDatabaseIsTablePersistentW;
begin
  GetProcedureAddress(_MsiDatabaseIsTablePersistentW, msilib, 'MsiDatabaseIsTablePersistentW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDatabaseIsTablePersistentW]
  end;
end;

var
  _MsiDatabaseIsTablePersistent: Pointer;

function MsiDatabaseIsTablePersistent;
begin
  GetProcedureAddress(_MsiDatabaseIsTablePersistent, msilib, 'MsiDatabaseIsTablePersistent' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDatabaseIsTablePersistent]
  end;
end;

var
  _MsiGetSummaryInformationA: Pointer;

function MsiGetSummaryInformationA;
begin
  GetProcedureAddress(_MsiGetSummaryInformationA, msilib, 'MsiGetSummaryInformationA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetSummaryInformationA]
  end;
end;

var
  _MsiGetSummaryInformationW: Pointer;

function MsiGetSummaryInformationW;
begin
  GetProcedureAddress(_MsiGetSummaryInformationW, msilib, 'MsiGetSummaryInformationW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetSummaryInformationW]
  end;
end;

var
  _MsiGetSummaryInformation: Pointer;

function MsiGetSummaryInformation;
begin
  GetProcedureAddress(_MsiGetSummaryInformation, msilib, 'MsiGetSummaryInformation' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetSummaryInformation]
  end;
end;

var
  _MsiSummaryInfoGetPropertyCount: Pointer;

function MsiSummaryInfoGetPropertyCount;
begin
  GetProcedureAddress(_MsiSummaryInfoGetPropertyCount, msilib, 'MsiSummaryInfoGetPropertyCount');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSummaryInfoGetPropertyCount]
  end;
end;

var
  _MsiSummaryInfoSetPropertyA: Pointer;

function MsiSummaryInfoSetPropertyA;
begin
  GetProcedureAddress(_MsiSummaryInfoSetPropertyA, msilib, 'MsiSummaryInfoSetPropertyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSummaryInfoSetPropertyA]
  end;
end;

var
  _MsiSummaryInfoSetPropertyW: Pointer;

function MsiSummaryInfoSetPropertyW;
begin
  GetProcedureAddress(_MsiSummaryInfoSetPropertyW, msilib, 'MsiSummaryInfoSetPropertyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSummaryInfoSetPropertyW]
  end;
end;

var
  _MsiSummaryInfoSetProperty: Pointer;

function MsiSummaryInfoSetProperty;
begin
  GetProcedureAddress(_MsiSummaryInfoSetProperty, msilib, 'MsiSummaryInfoSetProperty' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSummaryInfoSetProperty]
  end;
end;

var
  _MsiSummaryInfoGetPropertyA: Pointer;

function MsiSummaryInfoGetPropertyA;
begin
  GetProcedureAddress(_MsiSummaryInfoGetPropertyA, msilib, 'MsiSummaryInfoGetPropertyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSummaryInfoGetPropertyA]
  end;
end;

var
  _MsiSummaryInfoGetPropertyW: Pointer;

function MsiSummaryInfoGetPropertyW;
begin
  GetProcedureAddress(_MsiSummaryInfoGetPropertyW, msilib, 'MsiSummaryInfoGetPropertyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSummaryInfoGetPropertyW]
  end;
end;

var
  _MsiSummaryInfoGetProperty: Pointer;

function MsiSummaryInfoGetProperty;
begin
  GetProcedureAddress(_MsiSummaryInfoGetProperty, msilib, 'MsiSummaryInfoGetProperty' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSummaryInfoGetProperty]
  end;
end;

var
  _MsiSummaryInfoPersist: Pointer;

function MsiSummaryInfoPersist;
begin
  GetProcedureAddress(_MsiSummaryInfoPersist, msilib, 'MsiSummaryInfoPersist');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSummaryInfoPersist]
  end;
end;

var
  _MsiOpenDatabaseA: Pointer;

function MsiOpenDatabaseA;
begin
  GetProcedureAddress(_MsiOpenDatabaseA, msilib, 'MsiOpenDatabaseA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiOpenDatabaseA]
  end;
end;

var
  _MsiOpenDatabaseW: Pointer;

function MsiOpenDatabaseW;
begin
  GetProcedureAddress(_MsiOpenDatabaseW, msilib, 'MsiOpenDatabaseW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiOpenDatabaseW]
  end;
end;

var
  _MsiOpenDatabase: Pointer;

function MsiOpenDatabase;
begin
  GetProcedureAddress(_MsiOpenDatabase, msilib, 'MsiOpenDatabase' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiOpenDatabase]
  end;
end;

var
  _MsiDatabaseImportA: Pointer;

function MsiDatabaseImportA;
begin
  GetProcedureAddress(_MsiDatabaseImportA, msilib, 'MsiDatabaseImportA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDatabaseImportA]
  end;
end;

var
  _MsiDatabaseImportW: Pointer;

function MsiDatabaseImportW;
begin
  GetProcedureAddress(_MsiDatabaseImportW, msilib, 'MsiDatabaseImportW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDatabaseImportW]
  end;
end;

var
  _MsiDatabaseImport: Pointer;

function MsiDatabaseImport;
begin
  GetProcedureAddress(_MsiDatabaseImport, msilib, 'MsiDatabaseImport' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDatabaseImport]
  end;
end;

var
  _MsiDatabaseExportA: Pointer;

function MsiDatabaseExportA;
begin
  GetProcedureAddress(_MsiDatabaseExportA, msilib, 'MsiDatabaseExportA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDatabaseExportA]
  end;
end;

var
  _MsiDatabaseExportW: Pointer;

function MsiDatabaseExportW;
begin
  GetProcedureAddress(_MsiDatabaseExportW, msilib, 'MsiDatabaseExportW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDatabaseExportW]
  end;
end;

var
  _MsiDatabaseExport: Pointer;

function MsiDatabaseExport;
begin
  GetProcedureAddress(_MsiDatabaseExport, msilib, 'MsiDatabaseExport' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDatabaseExport]
  end;
end;

var
  _MsiDatabaseMergeA: Pointer;

function MsiDatabaseMergeA;
begin
  GetProcedureAddress(_MsiDatabaseMergeA, msilib, 'MsiDatabaseMergeA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDatabaseMergeA]
  end;
end;

var
  _MsiDatabaseMergeW: Pointer;

function MsiDatabaseMergeW;
begin
  GetProcedureAddress(_MsiDatabaseMergeW, msilib, 'MsiDatabaseMergeW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDatabaseMergeW]
  end;
end;

var
  _MsiDatabaseMerge: Pointer;

function MsiDatabaseMerge;
begin
  GetProcedureAddress(_MsiDatabaseMerge, msilib, 'MsiDatabaseMerge' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDatabaseMerge]
  end;
end;

var
  _MsiDatabaseGenerateTransformA: Pointer;

function MsiDatabaseGenerateTransformA;
begin
  GetProcedureAddress(_MsiDatabaseGenerateTransformA, msilib, 'MsiDatabaseGenerateTransformA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDatabaseGenerateTransformA]
  end;
end;

var
  _MsiDatabaseGenerateTransformW: Pointer;

function MsiDatabaseGenerateTransformW;
begin
  GetProcedureAddress(_MsiDatabaseGenerateTransformW, msilib, 'MsiDatabaseGenerateTransformW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDatabaseGenerateTransformW]
  end;
end;

var
  _MsiDatabaseGenerateTransform: Pointer;

function MsiDatabaseGenerateTransform;
begin
  GetProcedureAddress(_MsiDatabaseGenerateTransform, msilib, 'MsiDatabaseGenerateTransform' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDatabaseGenerateTransform]
  end;
end;

var
  _MsiDatabaseApplyTransformA: Pointer;

function MsiDatabaseApplyTransformA;
begin
  GetProcedureAddress(_MsiDatabaseApplyTransformA, msilib, 'MsiDatabaseApplyTransformA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDatabaseApplyTransformA]
  end;
end;

var
  _MsiDatabaseApplyTransformW: Pointer;

function MsiDatabaseApplyTransformW;
begin
  GetProcedureAddress(_MsiDatabaseApplyTransformW, msilib, 'MsiDatabaseApplyTransformW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDatabaseApplyTransformW]
  end;
end;

var
  _MsiDatabaseApplyTransform: Pointer;

function MsiDatabaseApplyTransform;
begin
  GetProcedureAddress(_MsiDatabaseApplyTransform, msilib, 'MsiDatabaseApplyTransform' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDatabaseApplyTransform]
  end;
end;

var
  _MsiCreateTransformSummaryInfoA: Pointer;

function MsiCreateTransformSummaryInfoA;
begin
  GetProcedureAddress(_MsiCreateTransformSummaryInfoA, msilib, 'MsiCreateTransformSummaryInfoA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiCreateTransformSummaryInfoA]
  end;
end;

var
  _MsiCreateTransformSummaryInfoW: Pointer;

function MsiCreateTransformSummaryInfoW;
begin
  GetProcedureAddress(_MsiCreateTransformSummaryInfoW, msilib, 'MsiCreateTransformSummaryInfoW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiCreateTransformSummaryInfoW]
  end;
end;

var
  _MsiCreateTransformSummaryInfo: Pointer;

function MsiCreateTransformSummaryInfo;
begin
  GetProcedureAddress(_MsiCreateTransformSummaryInfo, msilib, 'MsiCreateTransformSummaryInfo' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiCreateTransformSummaryInfo]
  end;
end;

var
  _MsiDatabaseCommit: Pointer;

function MsiDatabaseCommit;
begin
  GetProcedureAddress(_MsiDatabaseCommit, msilib, 'MsiDatabaseCommit');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDatabaseCommit]
  end;
end;

var
  _MsiGetDatabaseState: Pointer;

function MsiGetDatabaseState;
begin
  GetProcedureAddress(_MsiGetDatabaseState, msilib, 'MsiGetDatabaseState');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetDatabaseState]
  end;
end;

var
  _MsiCreateRecord: Pointer;

function MsiCreateRecord;
begin
  GetProcedureAddress(_MsiCreateRecord, msilib, 'MsiCreateRecord');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiCreateRecord]
  end;
end;

var
  _MsiRecordIsNull: Pointer;

function MsiRecordIsNull;
begin
  GetProcedureAddress(_MsiRecordIsNull, msilib, 'MsiRecordIsNull');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiRecordIsNull]
  end;
end;

var
  _MsiRecordDataSize: Pointer;

function MsiRecordDataSize;
begin
  GetProcedureAddress(_MsiRecordDataSize, msilib, 'MsiRecordDataSize');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiRecordDataSize]
  end;
end;

var
  _MsiRecordSetInteger: Pointer;

function MsiRecordSetInteger;
begin
  GetProcedureAddress(_MsiRecordSetInteger, msilib, 'MsiRecordSetInteger');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiRecordSetInteger]
  end;
end;

var
  _MsiRecordSetStringA: Pointer;

function MsiRecordSetStringA;
begin
  GetProcedureAddress(_MsiRecordSetStringA, msilib, 'MsiRecordSetStringA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiRecordSetStringA]
  end;
end;

var
  _MsiRecordSetStringW: Pointer;

function MsiRecordSetStringW;
begin
  GetProcedureAddress(_MsiRecordSetStringW, msilib, 'MsiRecordSetStringW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiRecordSetStringW]
  end;
end;

var
  _MsiRecordSetString: Pointer;

function MsiRecordSetString;
begin
  GetProcedureAddress(_MsiRecordSetString, msilib, 'MsiRecordSetString' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiRecordSetString]
  end;
end;

var
  _MsiRecordGetInteger: Pointer;

function MsiRecordGetInteger;
begin
  GetProcedureAddress(_MsiRecordGetInteger, msilib, 'MsiRecordGetInteger');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiRecordGetInteger]
  end;
end;

var
  _MsiRecordGetStringA: Pointer;

function MsiRecordGetStringA;
begin
  GetProcedureAddress(_MsiRecordGetStringA, msilib, 'MsiRecordGetStringA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiRecordGetStringA]
  end;
end;

var
  _MsiRecordGetStringW: Pointer;

function MsiRecordGetStringW;
begin
  GetProcedureAddress(_MsiRecordGetStringW, msilib, 'MsiRecordGetStringW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiRecordGetStringW]
  end;
end;

var
  _MsiRecordGetString: Pointer;

function MsiRecordGetString;
begin
  GetProcedureAddress(_MsiRecordGetString, msilib, 'MsiRecordGetString' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiRecordGetString]
  end;
end;

var
  _MsiRecordGetFieldCount: Pointer;

function MsiRecordGetFieldCount;
begin
  GetProcedureAddress(_MsiRecordGetFieldCount, msilib, 'MsiRecordGetFieldCount');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiRecordGetFieldCount]
  end;
end;

var
  _MsiRecordSetStreamA: Pointer;

function MsiRecordSetStreamA;
begin
  GetProcedureAddress(_MsiRecordSetStreamA, msilib, 'MsiRecordSetStreamA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiRecordSetStreamA]
  end;
end;

var
  _MsiRecordSetStreamW: Pointer;

function MsiRecordSetStreamW;
begin
  GetProcedureAddress(_MsiRecordSetStreamW, msilib, 'MsiRecordSetStreamW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiRecordSetStreamW]
  end;
end;

var
  _MsiRecordSetStream: Pointer;

function MsiRecordSetStream;
begin
  GetProcedureAddress(_MsiRecordSetStream, msilib, 'MsiRecordSetStream' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiRecordSetStream]
  end;
end;

var
  _MsiRecordReadStream: Pointer;

function MsiRecordReadStream;
begin
  GetProcedureAddress(_MsiRecordReadStream, msilib, 'MsiRecordReadStream');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiRecordReadStream]
  end;
end;

var
  _MsiRecordClearData: Pointer;

function MsiRecordClearData;
begin
  GetProcedureAddress(_MsiRecordClearData, msilib, 'MsiRecordClearData');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiRecordClearData]
  end;
end;

var
  _MsiGetActiveDatabase: Pointer;

function MsiGetActiveDatabase;
begin
  GetProcedureAddress(_MsiGetActiveDatabase, msilib, 'MsiGetActiveDatabase');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetActiveDatabase]
  end;
end;

var
  _MsiSetPropertyA: Pointer;

function MsiSetPropertyA;
begin
  GetProcedureAddress(_MsiSetPropertyA, msilib, 'MsiSetPropertyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSetPropertyA]
  end;
end;

var
  _MsiSetPropertyW: Pointer;

function MsiSetPropertyW;
begin
  GetProcedureAddress(_MsiSetPropertyW, msilib, 'MsiSetPropertyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSetPropertyW]
  end;
end;

var
  _MsiSetProperty: Pointer;

function MsiSetProperty;
begin
  GetProcedureAddress(_MsiSetProperty, msilib, 'MsiSetProperty' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSetProperty]
  end;
end;

var
  _MsiGetPropertyA: Pointer;

function MsiGetPropertyA;
begin
  GetProcedureAddress(_MsiGetPropertyA, msilib, 'MsiGetPropertyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetPropertyA]
  end;
end;

var
  _MsiGetPropertyW: Pointer;

function MsiGetPropertyW;
begin
  GetProcedureAddress(_MsiGetPropertyW, msilib, 'MsiGetPropertyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetPropertyW]
  end;
end;

var
  _MsiGetProperty: Pointer;

function MsiGetProperty;
begin
  GetProcedureAddress(_MsiGetProperty, msilib, 'MsiGetProperty' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetProperty]
  end;
end;

var
  _MsiGetLanguage: Pointer;

function MsiGetLanguage;
begin
  GetProcedureAddress(_MsiGetLanguage, msilib, 'MsiGetLanguage');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetLanguage]
  end;
end;

var
  _MsiGetMode: Pointer;

function MsiGetMode;
begin
  GetProcedureAddress(_MsiGetMode, msilib, 'MsiGetMode');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetMode]
  end;
end;

var
  _MsiSetMode: Pointer;

function MsiSetMode;
begin
  GetProcedureAddress(_MsiSetMode, msilib, 'MsiSetMode');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSetMode]
  end;
end;

var
  _MsiFormatRecordA: Pointer;

function MsiFormatRecordA;
begin
  GetProcedureAddress(_MsiFormatRecordA, msilib, 'MsiFormatRecordA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiFormatRecordA]
  end;
end;

var
  _MsiFormatRecordW: Pointer;

function MsiFormatRecordW;
begin
  GetProcedureAddress(_MsiFormatRecordW, msilib, 'MsiFormatRecordW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiFormatRecordW]
  end;
end;

var
  _MsiFormatRecord: Pointer;

function MsiFormatRecord;
begin
  GetProcedureAddress(_MsiFormatRecord, msilib, 'MsiFormatRecord' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiFormatRecord]
  end;
end;

var
  _MsiDoActionA: Pointer;

function MsiDoActionA;
begin
  GetProcedureAddress(_MsiDoActionA, msilib, 'MsiDoActionA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDoActionA]
  end;
end;

var
  _MsiDoActionW: Pointer;

function MsiDoActionW;
begin
  GetProcedureAddress(_MsiDoActionW, msilib, 'MsiDoActionW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDoActionW]
  end;
end;

var
  _MsiDoAction: Pointer;

function MsiDoAction;
begin
  GetProcedureAddress(_MsiDoAction, msilib, 'MsiDoAction' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiDoAction]
  end;
end;

var
  _MsiSequenceA: Pointer;

function MsiSequenceA;
begin
  GetProcedureAddress(_MsiSequenceA, msilib, 'MsiSequenceA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSequenceA]
  end;
end;

var
  _MsiSequenceW: Pointer;

function MsiSequenceW;
begin
  GetProcedureAddress(_MsiSequenceW, msilib, 'MsiSequenceW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSequenceW]
  end;
end;

var
  _MsiSequence: Pointer;

function MsiSequence;
begin
  GetProcedureAddress(_MsiSequence, msilib, 'MsiSequence' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSequence]
  end;
end;

var
  _MsiProcessMessage: Pointer;

function MsiProcessMessage;
begin
  GetProcedureAddress(_MsiProcessMessage, msilib, 'MsiProcessMessage');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiProcessMessage]
  end;
end;

var
  _MsiEvaluateConditionA: Pointer;

function MsiEvaluateConditionA;
begin
  GetProcedureAddress(_MsiEvaluateConditionA, msilib, 'MsiEvaluateConditionA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiEvaluateConditionA]
  end;
end;

var
  _MsiEvaluateConditionW: Pointer;

function MsiEvaluateConditionW;
begin
  GetProcedureAddress(_MsiEvaluateConditionW, msilib, 'MsiEvaluateConditionW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiEvaluateConditionW]
  end;
end;

var
  _MsiEvaluateCondition: Pointer;

function MsiEvaluateCondition;
begin
  GetProcedureAddress(_MsiEvaluateCondition, msilib, 'MsiEvaluateCondition' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiEvaluateCondition]
  end;
end;

var
  _MsiGetFeatureStateA: Pointer;

function MsiGetFeatureStateA;
begin
  GetProcedureAddress(_MsiGetFeatureStateA, msilib, 'MsiGetFeatureStateA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetFeatureStateA]
  end;
end;

var
  _MsiGetFeatureStateW: Pointer;

function MsiGetFeatureStateW;
begin
  GetProcedureAddress(_MsiGetFeatureStateW, msilib, 'MsiGetFeatureStateW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetFeatureStateW]
  end;
end;

var
  _MsiGetFeatureState: Pointer;

function MsiGetFeatureState;
begin
  GetProcedureAddress(_MsiGetFeatureState, msilib, 'MsiGetFeatureState' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetFeatureState]
  end;
end;

var
  _MsiSetFeatureStateA: Pointer;

function MsiSetFeatureStateA;
begin
  GetProcedureAddress(_MsiSetFeatureStateA, msilib, 'MsiSetFeatureStateA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSetFeatureStateA]
  end;
end;

var
  _MsiSetFeatureStateW: Pointer;

function MsiSetFeatureStateW;
begin
  GetProcedureAddress(_MsiSetFeatureStateW, msilib, 'MsiSetFeatureStateW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSetFeatureStateW]
  end;
end;

var
  _MsiSetFeatureState: Pointer;

function MsiSetFeatureState;
begin
  GetProcedureAddress(_MsiSetFeatureState, msilib, 'MsiSetFeatureState' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSetFeatureState]
  end;
end;

var
  _MsiSetFeatureAttributesA: Pointer;

function MsiSetFeatureAttributesA;
begin
  GetProcedureAddress(_MsiSetFeatureAttributesA, msilib, 'MsiSetFeatureAttributesA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSetFeatureAttributesA]
  end;
end;

var
  _MsiSetFeatureAttributesW: Pointer;

function MsiSetFeatureAttributesW;
begin
  GetProcedureAddress(_MsiSetFeatureAttributesW, msilib, 'MsiSetFeatureAttributesW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSetFeatureAttributesW]
  end;
end;

var
  _MsiSetFeatureAttributes: Pointer;

function MsiSetFeatureAttributes;
begin
  GetProcedureAddress(_MsiSetFeatureAttributes, msilib, 'MsiSetFeatureAttributes' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSetFeatureAttributes]
  end;
end;

var
  _MsiGetComponentStateA: Pointer;

function MsiGetComponentStateA;
begin
  GetProcedureAddress(_MsiGetComponentStateA, msilib, 'MsiGetComponentStateA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetComponentStateA]
  end;
end;

var
  _MsiGetComponentStateW: Pointer;

function MsiGetComponentStateW;
begin
  GetProcedureAddress(_MsiGetComponentStateW, msilib, 'MsiGetComponentStateW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetComponentStateW]
  end;
end;

var
  _MsiGetComponentState: Pointer;

function MsiGetComponentState;
begin
  GetProcedureAddress(_MsiGetComponentState, msilib, 'MsiGetComponentState' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetComponentState]
  end;
end;

var
  _MsiSetComponentStateA: Pointer;

function MsiSetComponentStateA;
begin
  GetProcedureAddress(_MsiSetComponentStateA, msilib, 'MsiSetComponentStateA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSetComponentStateA]
  end;
end;

var
  _MsiSetComponentStateW: Pointer;

function MsiSetComponentStateW;
begin
  GetProcedureAddress(_MsiSetComponentStateW, msilib, 'MsiSetComponentStateW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSetComponentStateW]
  end;
end;

var
  _MsiSetComponentState: Pointer;

function MsiSetComponentState;
begin
  GetProcedureAddress(_MsiSetComponentState, msilib, 'MsiSetComponentState' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSetComponentState]
  end;
end;

var
  _MsiGetFeatureCostA: Pointer;

function MsiGetFeatureCostA;
begin
  GetProcedureAddress(_MsiGetFeatureCostA, msilib, 'MsiGetFeatureCostA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetFeatureCostA]
  end;
end;

var
  _MsiGetFeatureCostW: Pointer;

function MsiGetFeatureCostW;
begin
  GetProcedureAddress(_MsiGetFeatureCostW, msilib, 'MsiGetFeatureCostW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetFeatureCostW]
  end;
end;

var
  _MsiGetFeatureCost: Pointer;

function MsiGetFeatureCost;
begin
  GetProcedureAddress(_MsiGetFeatureCost, msilib, 'MsiGetFeatureCost' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetFeatureCost]
  end;
end;

var
  _MsiEnumComponentCostsA: Pointer;

function MsiEnumComponentCostsA;
begin
  GetProcedureAddress(_MsiEnumComponentCostsA, msilib, 'MsiEnumComponentCostsA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiEnumComponentCostsA]
  end;
end;

var
  _MsiEnumComponentCostsW: Pointer;

function MsiEnumComponentCostsW;
begin
  GetProcedureAddress(_MsiEnumComponentCostsW, msilib, 'MsiEnumComponentCostsW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiEnumComponentCostsW]
  end;
end;

var
  _MsiEnumComponentCosts: Pointer;

function MsiEnumComponentCosts;
begin
  GetProcedureAddress(_MsiEnumComponentCosts, msilib, 'MsiEnumComponentCosts' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiEnumComponentCosts]
  end;
end;

var
  _MsiSetInstallLevel: Pointer;

function MsiSetInstallLevel;
begin
  GetProcedureAddress(_MsiSetInstallLevel, msilib, 'MsiSetInstallLevel');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSetInstallLevel]
  end;
end;

var
  _MsiGetFeatureValidStatesA: Pointer;

function MsiGetFeatureValidStatesA;
begin
  GetProcedureAddress(_MsiGetFeatureValidStatesA, msilib, 'MsiGetFeatureValidStatesA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetFeatureValidStatesA]
  end;
end;

var
  _MsiGetFeatureValidStatesW: Pointer;

function MsiGetFeatureValidStatesW;
begin
  GetProcedureAddress(_MsiGetFeatureValidStatesW, msilib, 'MsiGetFeatureValidStatesW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetFeatureValidStatesW]
  end;
end;

var
  _MsiGetFeatureValidStates: Pointer;

function MsiGetFeatureValidStates;
begin
  GetProcedureAddress(_MsiGetFeatureValidStates, msilib, 'MsiGetFeatureValidStates' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetFeatureValidStates]
  end;
end;

var
  _MsiGetSourcePathA: Pointer;

function MsiGetSourcePathA;
begin
  GetProcedureAddress(_MsiGetSourcePathA, msilib, 'MsiGetSourcePathA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetSourcePathA]
  end;
end;

var
  _MsiGetSourcePathW: Pointer;

function MsiGetSourcePathW;
begin
  GetProcedureAddress(_MsiGetSourcePathW, msilib, 'MsiGetSourcePathW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetSourcePathW]
  end;
end;

var
  _MsiGetSourcePath: Pointer;

function MsiGetSourcePath;
begin
  GetProcedureAddress(_MsiGetSourcePath, msilib, 'MsiGetSourcePath' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetSourcePath]
  end;
end;

var
  _MsiGetTargetPathA: Pointer;

function MsiGetTargetPathA;
begin
  GetProcedureAddress(_MsiGetTargetPathA, msilib, 'MsiGetTargetPathA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetTargetPathA]
  end;
end;

var
  _MsiGetTargetPathW: Pointer;

function MsiGetTargetPathW;
begin
  GetProcedureAddress(_MsiGetTargetPathW, msilib, 'MsiGetTargetPathW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetTargetPathW]
  end;
end;

var
  _MsiGetTargetPath: Pointer;

function MsiGetTargetPath;
begin
  GetProcedureAddress(_MsiGetTargetPath, msilib, 'MsiGetTargetPath' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetTargetPath]
  end;
end;

var
  _MsiSetTargetPathA: Pointer;

function MsiSetTargetPathA;
begin
  GetProcedureAddress(_MsiSetTargetPathA, msilib, 'MsiSetTargetPathA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSetTargetPathA]
  end;
end;

var
  _MsiSetTargetPathW: Pointer;

function MsiSetTargetPathW;
begin
  GetProcedureAddress(_MsiSetTargetPathW, msilib, 'MsiSetTargetPathW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSetTargetPathW]
  end;
end;

var
  _MsiSetTargetPath: Pointer;

function MsiSetTargetPath;
begin
  GetProcedureAddress(_MsiSetTargetPath, msilib, 'MsiSetTargetPath' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiSetTargetPath]
  end;
end;

var
  _MsiVerifyDiskSpace: Pointer;

function MsiVerifyDiskSpace;
begin
  GetProcedureAddress(_MsiVerifyDiskSpace, msilib, 'MsiVerifyDiskSpace');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiVerifyDiskSpace]
  end;
end;

var
  _MsiEnableUIPreview: Pointer;

function MsiEnableUIPreview;
begin
  GetProcedureAddress(_MsiEnableUIPreview, msilib, 'MsiEnableUIPreview');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiEnableUIPreview]
  end;
end;

var
  _MsiPreviewDialogA: Pointer;

function MsiPreviewDialogA;
begin
  GetProcedureAddress(_MsiPreviewDialogA, msilib, 'MsiPreviewDialogA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiPreviewDialogA]
  end;
end;

var
  _MsiPreviewDialogW: Pointer;

function MsiPreviewDialogW;
begin
  GetProcedureAddress(_MsiPreviewDialogW, msilib, 'MsiPreviewDialogW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiPreviewDialogW]
  end;
end;

var
  _MsiPreviewDialog: Pointer;

function MsiPreviewDialog;
begin
  GetProcedureAddress(_MsiPreviewDialog, msilib, 'MsiPreviewDialog' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiPreviewDialog]
  end;
end;

var
  _MsiPreviewBillboardA: Pointer;

function MsiPreviewBillboardA;
begin
  GetProcedureAddress(_MsiPreviewBillboardA, msilib, 'MsiPreviewBillboardA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiPreviewBillboardA]
  end;
end;

var
  _MsiPreviewBillboardW: Pointer;

function MsiPreviewBillboardW;
begin
  GetProcedureAddress(_MsiPreviewBillboardW, msilib, 'MsiPreviewBillboardW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiPreviewBillboardW]
  end;
end;

var
  _MsiPreviewBillboard: Pointer;

function MsiPreviewBillboard;
begin
  GetProcedureAddress(_MsiPreviewBillboard, msilib, 'MsiPreviewBillboard' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiPreviewBillboard]
  end;
end;

var
  _MsiGetLastErrorRecord: Pointer;

function MsiGetLastErrorRecord;
begin
  GetProcedureAddress(_MsiGetLastErrorRecord, msilib, 'MsiGetLastErrorRecord');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiGetLastErrorRecord]
  end;
end;

{$ELSE}

function MsiDatabaseOpenViewA; external msilib name 'MsiDatabaseOpenViewA';
function MsiDatabaseOpenViewW; external msilib name 'MsiDatabaseOpenViewW';
function MsiDatabaseOpenView; external msilib name 'MsiDatabaseOpenView' + AWSuffix;
function MsiViewGetErrorA; external msilib name 'MsiViewGetErrorA';
function MsiViewGetErrorW; external msilib name 'MsiViewGetErrorW';
function MsiViewGetError; external msilib name 'MsiViewGetError' + AWSuffix;
function MsiViewExecute; external msilib name 'MsiViewExecute';
function MsiViewFetch; external msilib name 'MsiViewFetch';
function MsiViewModify; external msilib name 'MsiViewModify';
function MsiViewGetColumnInfo; external msilib name 'MsiViewGetColumnInfo';
function MsiViewClose; external msilib name 'MsiViewClose';
function MsiDatabaseGetPrimaryKeysA; external msilib name 'MsiDatabaseGetPrimaryKeysA';
function MsiDatabaseGetPrimaryKeysW; external msilib name 'MsiDatabaseGetPrimaryKeysW';
function MsiDatabaseGetPrimaryKeys; external msilib name 'MsiDatabaseGetPrimaryKeys' + AWSuffix;
function MsiDatabaseIsTablePersistentA; external msilib name 'MsiDatabaseIsTablePersistentA';
function MsiDatabaseIsTablePersistentW; external msilib name 'MsiDatabaseIsTablePersistentW';
function MsiDatabaseIsTablePersistent; external msilib name 'MsiDatabaseIsTablePersistent' + AWSuffix;
function MsiGetSummaryInformationA; external msilib name 'MsiGetSummaryInformationA';
function MsiGetSummaryInformationW; external msilib name 'MsiGetSummaryInformationW';
function MsiGetSummaryInformation; external msilib name 'MsiGetSummaryInformation' + AWSuffix;
function MsiSummaryInfoGetPropertyCount; external msilib name 'MsiSummaryInfoGetPropertyCount';
function MsiSummaryInfoSetPropertyA; external msilib name 'MsiSummaryInfoSetPropertyA';
function MsiSummaryInfoSetPropertyW; external msilib name 'MsiSummaryInfoSetPropertyW';
function MsiSummaryInfoSetProperty; external msilib name 'MsiSummaryInfoSetProperty' + AWSuffix;
function MsiSummaryInfoGetPropertyA; external msilib name 'MsiSummaryInfoGetPropertyA';
function MsiSummaryInfoGetPropertyW; external msilib name 'MsiSummaryInfoGetPropertyW';
function MsiSummaryInfoGetProperty; external msilib name 'MsiSummaryInfoGetProperty' + AWSuffix;
function MsiSummaryInfoPersist; external msilib name 'MsiSummaryInfoPersist';
function MsiOpenDatabaseA; external msilib name 'MsiOpenDatabaseA';
function MsiOpenDatabaseW; external msilib name 'MsiOpenDatabaseW';
function MsiOpenDatabase; external msilib name 'MsiOpenDatabase' + AWSuffix;
function MsiDatabaseImportA; external msilib name 'MsiDatabaseImportA';
function MsiDatabaseImportW; external msilib name 'MsiDatabaseImportW';
function MsiDatabaseImport; external msilib name 'MsiDatabaseImport' + AWSuffix;
function MsiDatabaseExportA; external msilib name 'MsiDatabaseExportA';
function MsiDatabaseExportW; external msilib name 'MsiDatabaseExportW';
function MsiDatabaseExport; external msilib name 'MsiDatabaseExport' + AWSuffix;
function MsiDatabaseMergeA; external msilib name 'MsiDatabaseMergeA';
function MsiDatabaseMergeW; external msilib name 'MsiDatabaseMergeW';
function MsiDatabaseMerge; external msilib name 'MsiDatabaseMerge' + AWSuffix;
function MsiDatabaseGenerateTransformA; external msilib name 'MsiDatabaseGenerateTransformA';
function MsiDatabaseGenerateTransformW; external msilib name 'MsiDatabaseGenerateTransformW';
function MsiDatabaseGenerateTransform; external msilib name 'MsiDatabaseGenerateTransform' + AWSuffix;
function MsiDatabaseApplyTransformA; external msilib name 'MsiDatabaseApplyTransformA';
function MsiDatabaseApplyTransformW; external msilib name 'MsiDatabaseApplyTransformW';
function MsiDatabaseApplyTransform; external msilib name 'MsiDatabaseApplyTransform' + AWSuffix;
function MsiCreateTransformSummaryInfoA; external msilib name 'MsiCreateTransformSummaryInfoA';
function MsiCreateTransformSummaryInfoW; external msilib name 'MsiCreateTransformSummaryInfoW';
function MsiCreateTransformSummaryInfo; external msilib name 'MsiCreateTransformSummaryInfo' + AWSuffix;
function MsiDatabaseCommit; external msilib name 'MsiDatabaseCommit';
function MsiGetDatabaseState; external msilib name 'MsiGetDatabaseState';
function MsiCreateRecord; external msilib name 'MsiCreateRecord';
function MsiRecordIsNull; external msilib name 'MsiRecordIsNull';
function MsiRecordDataSize; external msilib name 'MsiRecordDataSize';
function MsiRecordSetInteger; external msilib name 'MsiRecordSetInteger';
function MsiRecordSetStringA; external msilib name 'MsiRecordSetStringA';
function MsiRecordSetStringW; external msilib name 'MsiRecordSetStringW';
function MsiRecordSetString; external msilib name 'MsiRecordSetString' + AWSuffix;
function MsiRecordGetInteger; external msilib name 'MsiRecordGetInteger';
function MsiRecordGetStringA; external msilib name 'MsiRecordGetStringA';
function MsiRecordGetStringW; external msilib name 'MsiRecordGetStringW';
function MsiRecordGetString; external msilib name 'MsiRecordGetString' + AWSuffix;
function MsiRecordGetFieldCount; external msilib name 'MsiRecordGetFieldCount';
function MsiRecordSetStreamA; external msilib name 'MsiRecordSetStreamA';
function MsiRecordSetStreamW; external msilib name 'MsiRecordSetStreamW';
function MsiRecordSetStream; external msilib name 'MsiRecordSetStream' + AWSuffix;
function MsiRecordReadStream; external msilib name 'MsiRecordReadStream';
function MsiRecordClearData; external msilib name 'MsiRecordClearData';
function MsiGetActiveDatabase; external msilib name 'MsiGetActiveDatabase';
function MsiSetPropertyA; external msilib name 'MsiSetPropertyA';
function MsiSetPropertyW; external msilib name 'MsiSetPropertyW';
function MsiSetProperty; external msilib name 'MsiSetProperty' + AWSuffix;
function MsiGetPropertyA; external msilib name 'MsiGetPropertyA';
function MsiGetPropertyW; external msilib name 'MsiGetPropertyW';
function MsiGetProperty; external msilib name 'MsiGetProperty' + AWSuffix;
function MsiGetLanguage; external msilib name 'MsiGetLanguage';
function MsiGetMode; external msilib name 'MsiGetMode';
function MsiSetMode; external msilib name 'MsiSetMode';
function MsiFormatRecordA; external msilib name 'MsiFormatRecordA';
function MsiFormatRecordW; external msilib name 'MsiFormatRecordW';
function MsiFormatRecord; external msilib name 'MsiFormatRecord' + AWSuffix;
function MsiDoActionA; external msilib name 'MsiDoActionA';
function MsiDoActionW; external msilib name 'MsiDoActionW';
function MsiDoAction; external msilib name 'MsiDoAction' + AWSuffix;
function MsiSequenceA; external msilib name 'MsiSequenceA';
function MsiSequenceW; external msilib name 'MsiSequenceW';
function MsiSequence; external msilib name 'MsiSequence' + AWSuffix;
function MsiProcessMessage; external msilib name 'MsiProcessMessage';
function MsiEvaluateConditionA; external msilib name 'MsiEvaluateConditionA';
function MsiEvaluateConditionW; external msilib name 'MsiEvaluateConditionW';
function MsiEvaluateCondition; external msilib name 'MsiEvaluateCondition' + AWSuffix;
function MsiGetFeatureStateA; external msilib name 'MsiGetFeatureStateA';
function MsiGetFeatureStateW; external msilib name 'MsiGetFeatureStateW';
function MsiGetFeatureState; external msilib name 'MsiGetFeatureState' + AWSuffix;
function MsiSetFeatureStateA; external msilib name 'MsiSetFeatureStateA';
function MsiSetFeatureStateW; external msilib name 'MsiSetFeatureStateW';
function MsiSetFeatureState; external msilib name 'MsiSetFeatureState' + AWSuffix;
function MsiSetFeatureAttributesA; external msilib name 'MsiSetFeatureAttributesA';
function MsiSetFeatureAttributesW; external msilib name 'MsiSetFeatureAttributesW';
function MsiSetFeatureAttributes; external msilib name 'MsiSetFeatureAttributes' + AWSuffix;
function MsiGetComponentStateA; external msilib name 'MsiGetComponentStateA';
function MsiGetComponentStateW; external msilib name 'MsiGetComponentStateW';
function MsiGetComponentState; external msilib name 'MsiGetComponentState' + AWSuffix;
function MsiSetComponentStateA; external msilib name 'MsiSetComponentStateA';
function MsiSetComponentStateW; external msilib name 'MsiSetComponentStateW';
function MsiSetComponentState; external msilib name 'MsiSetComponentState' + AWSuffix;
function MsiGetFeatureCostA; external msilib name 'MsiGetFeatureCostA';
function MsiGetFeatureCostW; external msilib name 'MsiGetFeatureCostW';
function MsiGetFeatureCost; external msilib name 'MsiGetFeatureCost' + AWSuffix;
function MsiEnumComponentCostsA; external msilib name 'MsiEnumComponentCostsA';
function MsiEnumComponentCostsW; external msilib name 'MsiEnumComponentCostsW';
function MsiEnumComponentCosts; external msilib name 'MsiEnumComponentCosts' + AWSuffix;
function MsiSetInstallLevel; external msilib name 'MsiSetInstallLevel';
function MsiGetFeatureValidStatesA; external msilib name 'MsiGetFeatureValidStatesA';
function MsiGetFeatureValidStatesW; external msilib name 'MsiGetFeatureValidStatesW';
function MsiGetFeatureValidStates; external msilib name 'MsiGetFeatureValidStates' + AWSuffix;
function MsiGetSourcePathA; external msilib name 'MsiGetSourcePathA';
function MsiGetSourcePathW; external msilib name 'MsiGetSourcePathW';
function MsiGetSourcePath; external msilib name 'MsiGetSourcePath' + AWSuffix;
function MsiGetTargetPathA; external msilib name 'MsiGetTargetPathA';
function MsiGetTargetPathW; external msilib name 'MsiGetTargetPathW';
function MsiGetTargetPath; external msilib name 'MsiGetTargetPath' + AWSuffix;
function MsiSetTargetPathA; external msilib name 'MsiSetTargetPathA';
function MsiSetTargetPathW; external msilib name 'MsiSetTargetPathW';
function MsiSetTargetPath; external msilib name 'MsiSetTargetPath' + AWSuffix;
function MsiVerifyDiskSpace; external msilib name 'MsiVerifyDiskSpace';
function MsiEnableUIPreview; external msilib name 'MsiEnableUIPreview';
function MsiPreviewDialogA; external msilib name 'MsiPreviewDialogA';
function MsiPreviewDialogW; external msilib name 'MsiPreviewDialogW';
function MsiPreviewDialog; external msilib name 'MsiPreviewDialog' + AWSuffix;
function MsiPreviewBillboardA; external msilib name 'MsiPreviewBillboardA';
function MsiPreviewBillboardW; external msilib name 'MsiPreviewBillboardW';
function MsiPreviewBillboard; external msilib name 'MsiPreviewBillboard' + AWSuffix;
function MsiGetLastErrorRecord; external msilib name 'MsiGetLastErrorRecord';

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}


{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
