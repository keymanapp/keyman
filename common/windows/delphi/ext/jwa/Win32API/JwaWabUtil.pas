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
{ The original file is: wabutil.h, released 31 Jan 2000.           			   }
{ The original Pascal code is: WabUtil.pas, released 31 Mar 2000.  			   }
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
unit JwaWabUtil;

interface

uses
  JwaWinType, JwaWinBase, ActiveX, JwaWabDefs, JwaWabMem;

{$I ..\Includes\JediAPILib.inc}


{$ALIGN ON}
{$MINENUMSIZE 4}
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}

{ IMAPITable in memory }

{ ITableData Interface ---------------------------------------------------- }

type
  ITableData = interface;

  PCallRelease = ^TCallRelease;
  CALLERRELEASE = procedure (ulCallerData: ULONG; lpTblData: ITableData;
    lpVue: IMAPITable); stdcall;
  TCallRelease = CALLERRELEASE;

  ITableData = interface(IUnknown)
    function HrGetView(lpSSortOrderSet: PSSortOrderSet;	lpfCallerRelease: PCallRelease;
      ulCallerData: ULONG; out lppMAPITable: IMAPITable): HResult; stdcall;
    function HrModifyRow(lpSRow: PSRow): HResult; stdcall;
    function HrDeleteRow(lpSPropValue: PSPropValue): HResult; stdcall;
    function HrQueryRow(lpsPropValue: PSPropValue; var lppSRow: PSRow;
      lpuliRow: PULONG): HResult; stdcall;
    function HrEnumRow(ulRowNumber: ULONG; var lppSRow: PSRow): HResult; stdcall;
    function HrNotify(ulFlags, cValues: ULONG; lpSPropValue: PSPropValue): HResult; stdcall;
    function HrInsertRow(uliRow: ULONG; lpSRow: PSRow): HResult; stdcall;
    function HrModifyRows(ulFlags: ULONG; lpSRowSet: PSRowSet): HResult; stdcall;
    function HrDeleteRows(ulFlags: ULONG; lprowsetToDelete: PSRowSet;
      var cRowsDeleted: ULONG): HResult; stdcall;
  end;
  {$EXTERNALSYM ITableData}


{ Entry Point for in memory ITable }

{*	CreateTable()
 *		Creates the internal memory structures and object handle
 *		to bring a new table into existence.
 *
 *	lpInterface
 *		Interface ID of the TableData object (IID_IMAPITableData)
 *
 *	lpAllocateBuffer, lpAllocateMore, and lpFreeBuffer
 *		Function addresses are provided by the caller so that
 *		this DLL allocates/frees memory appropriately.
 *	lpvReserved
 *		Reserved.  Should be NULL.
 *	ulTableType
 *		TBLTYPE_DYNAMIC, etc.  Visible to the calling application
 *		as part of the GetStatus return data on its views
 *	ulPropTagIndexColumn
 *		Index column for use when changing the data
 *	lpSPropTagArrayColumns
 *		Column proptags for the minimum set of columns in the table
 *	lppTableData
 *		Address of the pointer which will receive the TableData object
 *}

function CreateTable(lpInterface: PCIID; lpAllocateBuffer: PAllocateBuffer;
  lpAllocateMore: PAllocateMore; lpFreeBuffer: PFreeBuffer; lpvReserved: Pointer;
  ulTableType, ulPropTagIndexColumn: ULONG; lpSPropTagArrayColumns: PSPropTagArray;
  out lppTableData: ITableData): SCODE; stdcall;
{$EXTERNALSYM CreateTable}


{*	HrGetView()
 *		This function obtains a new view on the underlying data
 *		which supports the IMAPITable interface.  All rows and columns
 *		of the underlying table data are initially visible
 *	lpSSortOrderSet
 *		if specified, results in the view being sorted
 *	lpfCallerRelease
 *		pointer to a routine to be called when the view is released, or
 *		NULL.
 *	ulCallerData
 *		arbitrary data the caller wants saved with this view and returned in
 *		the Release callback.
 *}

{*	HrModifyRows()
 *		Add or modify a set of rows in the table data
 *	ulFlags
 *		Must be zero
 *	lpSRowSet
 *		Each row in the row set contains all the properties for one row
 *		in the table.  One of the properties must be the index column.  Any
 *		row in the table with the same value for its index column is
 *		replaced, or if there is no current row with that value the
 *		row is added.
 *		Each row in LPSRowSet MUST have a unique Index column!
 *		If any views are open, the view is updated as well.
 *		The properties do not have to be in the same order as the
 *		columns in the current table
 *}

{*	HrModifyRow()
 *		Add or modify one row in the table
 *	lpSRow
 *		This row contains all the properties for one row in the table.
 *		One of the properties must be the index column.	 Any row in
 *		the table with the same value for its index column is
 *		replaced, or if there is no current row with that value the
 *		row is added
 *		If any views are open, the view is updated as well.
 *		The properties do not have to be in the same order as the
 *		columns in the current table
 *}

{*	HrDeleteRows()
 *		Delete a row in the table.
 *	ulFlags
 *		TAD_ALL_ROWS - Causes all rows in the table to be deleted
 *					   lpSRowSet is ignored in this case.
 *	lpSRowSet
 *		Each row in the row set contains all the properties for one row
 *		in the table.  One of the properties must be the index column.  Any
 *		row in the table with the same value for its index column is
 *		deleted.
 *		Each row in LPSRowSet MUST have a unique Index column!
 *		If any views are open, the view is updated as well.
 *		The properties do not have to be in the same order as the
 *		columns in the current table
 *}

const
  TAD_ALL_ROWS = 1;
  {$EXTERNALSYM TAD_ALL_ROWS}

{*	HrDeleteRow()
 *		Delete a row in the table.
 *	lpSPropValue
 *		This property value specifies the row which has this value
 *		for its index column
 *}

{*	HrQueryRow()
 *		Returns the values of a specified row in the table
 *	lpSPropValue
 *		This property value specifies the row which has this value
 *		for its index column
 *	lppSRow
 *		Address of where to return a pointer to an SRow
 *	lpuliRow
 *	  Address of where to return the row number. This can be NULL
 *	  if the row number is not required.
 *
 *}

{*	HrEnumRow()
 *		Returns the values of a specific (numbered) row in the table
 *	ulRowNumber
 *		Indicates row number 0 to n-1
 *	lppSRow
 *		Address of where to return a pointer to a SRow
 *}

{*	HrInsertRow()
 *		Inserts a row into the table.
 *	uliRow
 *		The row number before which this row will be inserted into the table.
 *		Row numbers can be from 0 to n where o to n-1 result in row insertion
 *	  a row number of n results in the row being appended to the table.
 *	lpSRow
 *		This row contains all the properties for one row in the table.
 *		One of the properties must be the index column.	 Any row in
 *		the table with the same value for its index column is
 *		replaced, or if there is no current row with that value the
 *		row is added
 *		If any views are open, the view is updated as well.
 *		The properties do not have to be in the same order as the
 *		columns in the current table
 *}


{ IMAPIProp in memory }

{ IPropData Interface ---------------------------------------------------- }

type
  IPropData = interface(IMAPIProp)
    function HrSetObjAccess(ulAccess: ULONG): HResult; stdcall;
    function HrSetPropAccess(lpPropTagArray: PSPropTagArray; rgulAccess: PULONG): HResult; stdcall;
    function HrGetPropAccess(lppPropTagArray: PSPropTagArray;
      var lprgulAccess: PULONG): HResult; stdcall;
    function HrAddObjProps(lppPropTagArray: PSPropTagArray;
      lprgulAccess: PSPropProblemArray): HResult; stdcall;
  end;
  {$EXTERNALSYM IPropData}

{ Entry Point for in memory IMAPIProp }


{*	CreateIProp()
 *		Creates the internal memory structures and object handle
 *		to bring a new property interface into existance.
 *
 *	lpInterface
 *		Interface ID of the TableData object (IID_IMAPIPropData)
 *
 *	lpAllocateBuffer, lpAllocateMore, and lpFreeBuffer
 *		Function addresses are provided by the caller so that
 *		this DLL allocates/frees memory appropriately.
 *	lppPropData
 *		Address of the pointer which will receive the IPropData object
 *	lpvReserved
 *		Reserved.  Should be NULL.
 *}

type
  TWABCreateIProp = function(lpInterface: PCIID; lpAllocateBuffer: PAllocateBuffer;
    lpAllocateMore: PAllocateMore; lpFreeBuffer: PFreeBuffer; lpvReserved: Pointer;
    out lppPropData: IPropData): SCODE; stdcall;


{ Defines for prop/obj access }
const
  IPROP_READONLY  = ULONG($00000001);
  {$EXTERNALSYM IPROP_READONLY}
  IPROP_READWRITE = ULONG($00000002);
  {$EXTERNALSYM IPROP_READWRITE}
  IPROP_CLEAN     = ULONG($00010000);
  {$EXTERNALSYM IPROP_CLEAN}
  IPROP_DIRTY     = ULONG($00020000);
  {$EXTERNALSYM IPROP_DIRTY}

{*
 -	HrSetPropAccess
 -
 *	Sets access right attributes on a per-property basis.  By default,
 *	all properties are read/write.
 *}

{*
 -	HrSetObjAccess
 -
 *	Sets access rights for the object itself.  By default, the object has
 *	read/write access.
 *}

{ Idle time scheduler }

{*
 *	PRI
 *
 *	Priority of an idle task.
 *	The idle engine sorts tasks by priority, and the one with the higher
 *	value runs first. Within a priority level, the functions are called
 *	round-robin.
 *}

  PRILOWEST  = -32768;
  {$EXTERNALSYM PRILOWEST}
  PRIHIGHEST = 32767;
  {$EXTERNALSYM PRIHIGHEST}
  PRIUSER    = 0;
  {$EXTERNALSYM PRIUSER}

{*
 *	IRO
 *
 *	Idle routine options.  This is a combined bit mask consisting of
 *	individual firo's.	Listed below are the possible bit flags.
 *
 *		FIROWAIT and FIROINTERVAL are mutually exclusive.
 *		If neither of the flags are specified, the default action
 *		is to ignore the time parameter of the idle function and
 *		call it as often as possible if firoPerBlock is not set;
 *		otherwise call it one time only during the idle block
 *		once the time constraint has been set. FIROINTERVAL
 *		is also incompatible with FIROPERBLOCK.
 *
 *		FIROWAIT		- time given is minimum idle time before calling
 *						  for the first time in the block of idle time,
 *						  afterwhich call as often as possible.
 *		FIROINTERVAL	- time given is minimum interval between each
 *						  successive call
 *		FIROPERBLOCK	- called only once per contiguous block of idle
 *						  time
 *		FIRODISABLED	- initially disabled when registered, the
 *						  default is to enable the function when registered.
 *		FIROONCEONLY	- called only one time by the scheduler and then
 *						  deregistered automatically.
 *}

  IRONULL       = Word($0000);
  {$EXTERNALSYM IRONULL}
  FIROWAIT      = Word($0001);
  {$EXTERNALSYM FIROWAIT}
  FIROINTERVAL  = Word($0002);
  {$EXTERNALSYM FIROINTERVAL}
  FIROPERBLOCK  = Word($0004);
  {$EXTERNALSYM FIROPERBLOCK}
  FIRODISABLED  = Word($0020);
  {$EXTERNALSYM FIRODISABLED}
  FIROONCEONLY  = Word($0040);
  {$EXTERNALSYM FIROONCEONLY}

{*
 *	IRC
 *
 *	Idle routine change options. This is a combined bit mask consisting
 *	of individual firc's; each one identifies an aspect of the idle task
 *	that can be changed.
 *
 *}

  IRCNULL  = Word($0000);
  {$EXTERNALSYM IRCNULL}
  FIRCPFN  = Word($0001);	// change function pointer
  {$EXTERNALSYM FIRCPFN}
  FIRCPV   = Word($0002);	// change parameter block
  {$EXTERNALSYM FIRCPV}
  FIRCPRI  = Word($0004);	// change priority
  {$EXTERNALSYM FIRCPRI}
  FIRCCSEC = Word($0008);	// change time
  {$EXTERNALSYM FIRCCSEC}
  FIRCIRO  = Word($0010);	// change routine options
  {$EXTERNALSYM FIRCIRO}

{*
 *	Type definition for idle functions.	 An idle function takes one
 *	parameter, an PV, and returns a BOOL value.
 *}

type
  PFnIdle = ^TFnIdle;
  FNIDLE = function (lpvContext: Pointer): BOOL; stdcall;
  {$EXTERNALSYM FNIDLE}
  TFnIdle = FNIDLE;

{*
 *	FTG
 *
 *	Function Tag.  Used to identify a registered idle function.
 *
 *}

  FTG = Pointer;
  {$EXTERNALSYM FTG}

const
  FTGNULL = nil;
  {$EXTERNALSYM FTGNULL}

{*
 -	MAPIInitIdle/MAPIDeinitIdle
 -
 *	Purpose:
 *		Initialises the idle engine
 *		If the initialisation succeded, returns 0, else returns -1
 *
 *	Arguments:
 *		lpvReserved		Reserved, must be NULL.
 *}

function MAPIInitIdle(lpvReserved: Pointer): LongInt; stdcall;
{$EXTERNALSYM MAPIInitIdle}
procedure MAPIDeinitIdle; stdcall;
{$EXTERNALSYM MAPIDeinitIdle}

{*
 *	FtgRegisterIdleRoutine
 *
 *		Registers the function pfn of type PFNIDLE, i.e., (BOOL (*)(LPVOID))
 *		as an idle function.
 *
 *		The idle function will be called with the parameter pv by the
 *		idle engine. The function has initial priority priIdle,
 *		associated time csecIdle, and options iroIdle.
 *}

function FtgRegisterIdleRoutine(lpfnIdle: PFnIdle; lpvIdleParam: Pointer;
  priIdle: SmallInt; csecIdle: ULONG; iroIdle: Word): FTG; stdcall;
{$EXTERNALSYM FtgRegisterIdleRoutine}

{*
 *	DeregisterIdleRoutine
 *
 *		Removes the given routine from the list of idle routines.
 *		The routine will not be called again.  It is the responsibility
 *		of the caller to clean up any data structures pointed to by the
 *		pvIdleParam parameter; this routine does not free the block.
 *}

procedure DeregisterIdleRoutine(ftg: FTG); stdcall;
{$EXTERNALSYM DeregisterIdleRoutine}

{*
 *	EnableIdleRoutine
 *
 *		Enables or disables an idle routine.
 *}

procedure EnableIdleRoutine(ftg: FTG; fEnable: BOOL); stdcall;
{$EXTERNALSYM EnableIdleRoutine}

{*
 *	ChangeIdleRoutine
 *
 *		Changes some or all of the characteristics of the given idle
 *		function. The changes to make are indicated with flags in the
 *		ircIdle parameter.
 *}

procedure ChangeIdleRoutine(ftg: FTG; lpfnIdle: PFnIdle; lpvIdleParam: Pointer;
  priIdle: SmallInt; csecIdle: ULONG; iroIdle: Word; ircIdle: Word); stdcall;
{$EXTERNALSYM ChangeIdleRoutine}

{ IMalloc Utilities }

function MAPIGetDefaultMalloc: IMalloc; stdcall;
{$EXTERNALSYM MAPIGetDefaultMalloc}

{ StreamOnFile (SOF) }

{*
 *	Methods and #define's for implementing an OLE 2.0 storage stream
 *	(as defined in the OLE 2.0 specs) on top of a system file.
 *}

const
  SOF_UNIQUEFILENAME = ULONG($80000000);
  {$EXTERNALSYM SOF_UNIQUEFILENAME}

function OpenStreamOnFile(lpAllocateBuffer: PAllocateBuffer;
  lpFreeBuffer: PFreeBuffer; ulFlags: ULONG; lpszFileName, lpszPrefix: LPTSTR;
  out lppStream: IStream): HResult; stdcall;
{$EXTERNALSYM OpenStreamOnFile}


{ Property interface utilities }

{*
 *	Copies a single SPropValue from Src to Dest.  Handles all the various
 *	types of properties and will link its allocations given the master
 *	allocation object and an allocate more function.
 *}

function PropCopyMore(lpSPropValueDest, lpSPropValueSrc: PSPropValue;
  lpfAllocMore: PAllocateMore; lpvObject: Pointer): SCODE; stdcall;
{$EXTERNALSYM PropCopyMore}

{*
 *	Returns the size in bytes of structure at lpSPropValue, including the
 *	Value.
 *}

function UlPropSize(lpSPropValue: PSPropValue): ULONG; stdcall;
{$EXTERNALSYM UlPropSize}

function FEqualNames(lpName1, lpName2: PMapiNameID): BOOL; stdcall;
{$EXTERNALSYM FEqualNames}

//procedure GetInstance(lpPropMv, lpPropSv: PSPropValue; uliInst: ULONG); stdcall;
//{$EXTERNALSYM GetInstance}

function FPropContainsProp(const lpSPropValueDst, lpSPropValueSrc: TSPropValue;
  ulFuzzyLevel: ULONG): BOOL; stdcall;
{$EXTERNALSYM FPropContainsProp}

function FPropCompareProp(const lpSPropValue1: TSPropValue; ulRelOp: ULONG;
  const lpSPropValue2: TSPropValue): BOOL; stdcall;
{$EXTERNALSYM FPropCompareProp}

function LPropCompareProp(const lpSPropValueA, lpSPropValueB: TSPropValue): LongInt; stdcall;
{$EXTERNALSYM LPropCompareProp}

function HrAddColumns(lptbl: IMAPITable; lpproptagColumnsNew: PSPropTagArray;
  lpAllocateBuffer: PAllocateBuffer; lpFreeBuffer: PFreeBuffer): HResult; stdcall;
{$EXTERNALSYM HrAddColumns}

function HrAddColumnsEx(lptbl: IMAPITable; lpproptagColumnsNew: PSPropTagArray;
  lpAllocateBuffer: PAllocateBuffer; lpFreeBuffer: PFreeBuffer;
  ptaga: PSPropTagArray): HResult; stdcall;
{$EXTERNALSYM HrAddColumnsEx}


{ Notification utilities }

{*
 *	Function that creates an advise sink object given a notification
 *	callback function and context.
 *}

procedure HrAllocAdviseSink(lpfnCallback: TNotifyCallback; lpvContext: Pointer;
  out lppAdviseSink: IMAPIAdviseSink);
{$EXTERNALSYM HrAllocAdviseSink}

{*
 *	Wraps an existing advise sink with another one which guarantees
 *	that the original advise sink will be called in the thread on
 *	which it was created.
 *}

procedure HrThisThreadAdviseSink(lpAdviseSink: IMAPIAdviseSink;
  out lppAdviseSink: IMAPIAdviseSink);
{$EXTERNALSYM HrThisThreadAdviseSink}

{*
 *	Allows a client and/or provider to force notifications
 *	which are currently queued in the MAPI notification engine
 *	to be dispatched without doing a message dispatch.
 *}

function HrDispatchNotifications(ulFlags: ULONG): HResult; stdcall;
{$EXTERNALSYM HrDispatchNotifications}

{ Service Provider Utilities }

{*
 *	Structures and utility function for building a display table
 *	from resources.
 *}

type
  PDtCtl = ^TDtCtl;
  DTCTL = record
    ulCtlType: ULONG;   // DTCT_LABEL, etc.
    ulCtlFlags: ULONG;  // DT_REQUIRED, etc.
    lpbNotif: Pointer;  // pointer to notification data
    cbNotif: ULONG;     // count of bytes of notification data
    lpszFilter: LPTSTR;	// character filter for edit/combobox
    ulItemID: ULONG;    // to validate parallel dlg template entry
    case Integer of
      -1: (lpv: Pointer;);			// Initialize this to avoid warnings
      DTCT_LABEL: (lplabel: PDTblLabel;);
      DTCT_EDIT: (lpedit: PDTblEdit;);
      DTCT_LBX: (lplbx: PDTblLbx;);
      DTCT_COMBOBOX: (lpcombobox: PDTblComboBox;);
      DTCT_DDLBX: (lpddlbx: PDTblDDLbx;);
      DTCT_CHECKBOX: (lpcheckbox: PDTblCheckBox;);
      DTCT_GROUPBOX: (lpgroupbox: PDTblGroupBox;);
      DTCT_BUTTON: (lpbutton: PDTblButton;);
      DTCT_RADIOBUTTON: (lpradiobutton: PDTblRadioButton;);
      DTCT_MVLISTBOX: (lpmvlbx: PDTblMvListBox;);
      DTCT_MVDDLBX: (lpmvddlbx: PDTblMvDDLbx;);
      DTCT_PAGE: (lppage: PDTblPage;);
  end;
  {$EXTERNALSYM DTCTL}
  TDtCtl = DTCTL;

  PDtPage = ^TDtPage;
  DTPAGE = record
    cctl: ULONG;
    lpszResourceName: LPTSTR; // as usual, may be an integer ID
    case Integer of
      1: (lpszComponent: LPTSTR;);
      2: (ulItemID: ULONG;);
  end;
  {$EXTERNALSYM DTPAGE}
  TDtPage = DTPAGE;

procedure BuildDisplayTable(lpAllocateBuffer: PAllocateBuffer;
  lpAllocateMore: PAllocateMore; lpFreeBuffer: PFreeBuffer; lpMalloc: IMAlloc;
  hInstance: Cardinal; cPages: UINT; lpPage: PDtPage; ulFlags: ULONG;
  out lppTable: IMAPITable; lppTblData: ITableData);
{$EXTERNALSYM BuildDisplayTable}


{ MAPI structure validation/copy utilities }

{*
 *	Validate, copy, and adjust pointers in MAPI structures:
 *		notification
 *		property value array
 *		option data
 *}

function ScCountNotifications(cNotifications: Integer;
  lpNotifications: PNotification; var lpcb: ULONG): SCODE; stdcall;
{$EXTERNALSYM ScCountNotifications}

function ScCopyNotifications(cNotifications: Integer;
  lpNotifications: PNotification; lpvDst: Pointer; var lpcb: ULONG): SCODE; stdcall;
{$EXTERNALSYM ScCopyNotifications}

function ScRelocNotifications(cNotifications: Integer;
  lpNotifications: PNotification; lpvBaseOld, lpvBaseNew: Pointer;
  var lpcb: ULONG): SCODE; stdcall;
{$EXTERNALSYM ScRelocNotifications}

function ScCountProps(cValues: Integer; lpPropArray: PSPropValue;
  var lpcb: ULONG): SCODE; stdcall;
{$EXTERNALSYM ScCountProps}

function LpValFindProp(ulPropTag, cValues: ULONG; lpPropArray: PSPropValue): PSPropValue; stdcall;
{$EXTERNALSYM LpValFindProp}

function ScCopyProps(cValues: Integer; lpPropArray: PSPropValue; lpvDst: Pointer;
  var lpcb: ULONG): SCODE; stdcall;
{$EXTERNALSYM ScCopyProps}

function ScRelocProps(cValues: Integer; lpPropArray: PSPropValue;
  lpvBaseOld, lpvBaseNew: Pointer; var lpcb: ULONG): SCODE; stdcall;
{$EXTERNALSYM ScRelocProps}

function ScDupPropset(cValues: Integer; lpPropArray: PSPropValue;
  lpAllocateBuffer: PAllocateBuffer; var lppPropArray: PSPropValue): SCODE; stdcall;
{$EXTERNALSYM ScDupPropset}

{ General utility functions }

{ Related to the OLE Component object model }

function UlAddRef(lpunk: IUnknown): ULONG;
{$EXTERNALSYM UlAddRef}
function UlRelease(lpunk: IUnknown): ULONG;
{$EXTERNALSYM UlRelease}

{ Related to the MAPI interface }

procedure HrGetOneProp(lpMapiProp: IMAPIProp; ulPropTag: ULONG;
  var lppProp: PSPropValue); stdcall;
{$EXTERNALSYM HrGetOneProp}

procedure HrSetOneProp(lpMapiProp: IMAPIProp; lpProp: PSPropValue); stdcall;
{$EXTERNALSYM HrSetOneProp}

function FPropExists(lpMapiProp: IMAPIProp; ulPropTag: ULONG): BOOL; stdcall;
{$EXTERNALSYM FPropExists}

function PpropFindProp(lpPropArray: PSPropValue; cValues: ULONG;
{$EXTERNALSYM PpropFindProp}
  ulPropTag: ULONG): PSPropValue; stdcall;

procedure FreePadrlist(lpAdrlist: PAdrList); stdcall;
{$EXTERNALSYM FreePadrlist}
procedure FreeProws(lpRows: PSRowSet); stdcall;
{$EXTERNALSYM FreeProws}

function HrQueryAllRows(lpTable: IMAPITable; lpPropTags: PSPropTagArray;
  lpRestriction: PSRestriction; lpSortOrderSet: PSSortOrderSet;
  crowsMax: LongInt; var lppRows: PSRowSet): HResult; stdcall;
{$EXTERNALSYM HrQueryAllRows}


{ C runtime substitutes }

function SzFindCh(lpsz: LPCTSTR; ch: Byte): LPCTSTR;      // strchr
{$EXTERNALSYM SzFindCh}
function SzFindLastCh(lpsz: LPCTSTR; ch: Byte): LPCTSTR;  // strrchr
{$EXTERNALSYM SzFindLastCh}
function SzFindSz(lpsz, lpszKey: LPCTSTR): LPCTSTR;       // strstr
{$EXTERNALSYM SzFindSz}
function UFromSz(lpsz: LPCTSTR): DWORD;                   // atoi
{$EXTERNALSYM UFromSz}

function ScUNCFromLocalPath(lpszLocal, lpszUNC: LPSTR; cchUNC: UINT): SCODE; stdcall;
{$EXTERNALSYM ScUNCFromLocalPath}
function ScLocalPathFromUNC(lpszUNC, lpszLocal: LPSTR; cchLocal: UINT): SCODE; stdcall;
{$EXTERNALSYM ScLocalPathFromUNC}

{ 64-bit arithmetic with times }

function FtAddFt(ftAddend1, ftAddend2: TFileTime): TFileTime; stdcall;
{$EXTERNALSYM FtAddFt}
function FtMulDwDw(ftMultiplicand, ftMultiplier: DWORD): TFileTime; stdcall;
{$EXTERNALSYM FtMulDwDw}
function FtMulDw(ftMultiplier: DWORD; ftMultiplicand: TFileTime): TFileTime; stdcall;
{$EXTERNALSYM FtMulDw}
function FtSubFt(ftMinuend, ftSubtrahend: TFileTime): TFileTime; stdcall;
{$EXTERNALSYM FtSubFt}
function FtNegFt(ft: TFileTime): TFileTime; stdcall;
{$EXTERNALSYM FtNegFt}

{ Message composition }

function ScCreateConversationIndex(cbParent: ULONG; lpbParent: Pointer;
  var lpcbConvIndex: ULONG; var lppbConvIndex: Pointer): SCODE; stdcall;
{$EXTERNALSYM ScCreateConversationIndex}

{ Store support }

procedure WrapStoreEntryID(ulFlags: ULONG; lpszDLLName: LPTSTR; cbOrigEntry: ULONG;
  lpOrigEntry: PEntryID; var lpcbWrappedEntry: ULONG; var lppWrappedEntry: PEntryID); stdcall;
{$EXTERNALSYM WrapStoreEntryID}

{ RTF Sync Utilities }
const
  RTF_SYNC_RTF_CHANGED	= $00000001;
  {$EXTERNALSYM RTF_SYNC_RTF_CHANGED}
  RTF_SYNC_BODY_CHANGED	= $00000002;
  {$EXTERNALSYM RTF_SYNC_BODY_CHANGED}

function RTFSync(lpMessage: IMessage; ulFlags: ULONG; var lpfMessageUpdated: Boolean): HResult; stdcall;
{$EXTERNALSYM RTFSync}

{ Flags for WrapCompressedRTFStream() }

//****** MAPI_MODIFY				((ULONG) 0x00000001) mapidefs.h */
//****** STORE_UNCOMPRESSED_RTF	((ULONG) 0x00008000) mapidefs.h */

function WrapCompressedRTFStream(lpCompressedRTFStream: IStream; ulFlags: ULONG;
  out lpUncompressedRTFStream: IStream): HResult; stdcall;
{$EXTERNALSYM WrapCompressedRTFStream}

{ Storage on Stream }

function HrIStorageFromStream(lpUnkIn: IUnknown; lpInterface: PCIID;
  ulFlags: ULONG; out lppStorageOut: IStorage): HResult; stdcall;
{$EXTERNALSYM HrIStorageFromStream}

{*
 * Setup and cleanup.
 *
 * Providers never need to make these calls.
 *
 * Test applications and the like which do not call MAPIInitialize
 * may want to call them, so that the few utility functions which
 * need MAPI allocators (and do not ask for them explicitly)
 * will work.
 *}

{ All flags are reserved for ScInitMapiUtil. }

function ScInitMapiUtil(ulFlags: ULONG): SCODE; stdcall;
{$EXTERNALSYM ScInitMapiUtil}
procedure DeinitMapiUtil; stdcall;
{$EXTERNALSYM DeinitMapiUtil}

{$IFNDEF JWA_INCLUDEMODE}
function WabUtilLoaded: Boolean;
{$IFDEF WAB_DYNAMIC_LINK_EXPLICIT}
function LoadWabUtil: Boolean;
function UnloadWabUtil: Boolean;
{$ENDIF JWA_INCLUDEMODE}
{$ENDIF}

var
  WABCreateIProp: TWABCreateIProp = nil;
{$EXTERNALSYM WABCreateIProp}


{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation

uses
  JwaWinDLLNames, JwaWabApi, SysUtils;

const
  mapi32 = 'mapi32.dll';
{$ENDIF JWA_OMIT_SECTIONS}  


{$IFNDEF JWA_INTERFACESECTION}  

function SzFindCh(lpsz: LPCTSTR; ch: Byte): LPCTSTR;
begin
  Result := StrScan(lpsz, {$IFDEF UNICODE}WideChar{$ELSE}AnsiChar{$ENDIF}(ch));
end;

function SzFindLastCh(lpsz: LPCTSTR; ch: Byte): LPCTSTR;
begin
  Result := StrRScan(lpsz, {$IFDEF UNICODE}WideChar{$ELSE}AnsiChar{$ENDIF}(ch)); //must be CHAR !!
end;

function SzFindSz(lpsz, lpszKey: LPCTSTR): LPCTSTR;
begin
  Result := StrPos(lpsz, lpszKey);
end;

function UFromSz(lpsz: LPCTSTR): DWORD;
begin
  Result := StrToIntDef(StrPas(lpsz), 0);
end;

{$IFNDEF JWA_INCLUDEMODE}
var
  WabLibHandle: THandle = 0;

function WabUtilLoaded: Boolean;
begin
  Result := WabLibHandle <> 0;
end;

function UnloadWabUtil: Boolean;
begin
  if WabUtilLoaded then
  begin
    Result := FreeLibrary(WabLibHandle);
    WabLibHandle := 0;
    @WABCreateIProp := nil;
  end else Result := True;
end;

function LoadWabUtil: Boolean;
var
  WabDllPath: Widestring;
begin
  Result := WabUtilLoaded;
  if (not Result) and GetWabDllPath(WabDllPath) then
  begin
    WabLibHandle := LoadLibraryW(PWideChar(WabDllPath));
    if WabUtilLoaded then
    begin
      WABCreateIProp := GetProcAddress(WabLibHandle, 'WABCreateIProp');
      Result := Assigned(WABCreateIProp);
      if not Result then UnloadWabUtil;
    end;
  end;
end;
{$ENDIF JWA_INCLUDEMODE}


{$IFNDEF DYNAMIC_LINK}
function CreateTable; external mapi32 name 'CreateTable@36';
function MAPIInitIdle; external mapi32 name 'MAPIInitIdle@4';
procedure MAPIDeinitIdle; external mapi32 name 'MAPIDeinitIdle@0';
function FtgRegisterIdleRoutine; external mapi32 name 'FtgRegisterIdleRoutine@20';
procedure DeregisterIdleRoutine; external mapi32 name 'DeregisterIdleRoutine@4';
procedure EnableIdleRoutine; external mapi32 name 'EnableIdleRoutine@8';
procedure ChangeIdleRoutine; external mapi32 name 'ChangeIdleRoutine@28';
function MAPIGetDefaultMalloc; external mapi32 name 'MAPIGetDefaultMalloc@0';
function OpenStreamOnFile; external mapi32 name 'OpenStreamOnFile';
function PropCopyMore; external mapi32 name 'PropCopyMore@16';
function UlPropSize; external mapi32 name 'UlPropSize@4';
function FEqualNames; external mapi32 name 'FEqualNames@8';
//procedure GetInstance; external mapi32 name 'GetInstance';
function FPropContainsProp; external mapi32 name 'FPropContainsProp@12';
function FPropCompareProp; external mapi32 name 'FPropCompareProp@12';
function LPropCompareProp; external mapi32 name 'LPropCompareProp@8';
function HrAddColumns; external mapi32 name 'HrAddColumns@16';
function HrAddColumnsEx; external mapi32 name 'HrAddColumnsEx@20';
procedure HrAllocAdviseSink; external mapi32 name 'HrAllocAdviseSink@12';
procedure HrThisThreadAdviseSink; external mapi32 name 'HrThisThreadAdviseSink@8';
function HrDispatchNotifications; external mapi32 name 'HrDispatchNotifications@4';
procedure BuildDisplayTable; external mapi32 name 'BuildDisplayTable@40';
function ScCountNotifications; external mapi32 name 'ScCountNotifications@12';
function ScCopyNotifications; external mapi32 name 'ScCopyNotifications@16';
function ScRelocNotifications; external mapi32 name 'ScRelocNotifications@20';
function ScCountProps; external mapi32 name 'ScCountProps@12';
function LpValFindProp; external mapi32 name 'LpValFindProp@12';
function ScCopyProps; external mapi32 name 'ScCopyProps@16';
function ScRelocProps; external mapi32 name 'ScRelocProps@20';
function ScDupPropset; external mapi32 name 'ScDupPropset@16';
function UlAddRef; external mapi32 name 'UlAddRef@4';
function UlRelease; external mapi32 name 'UlRelease@4';
procedure HrGetOneProp; external mapi32 name 'HrGetOneProp@12';
procedure HrSetOneProp; external mapi32 name 'HrSetOneProp@8';
function FPropExists; external mapi32 name 'FPropExists@8';
function PpropFindProp; external mapi32 name 'PpropFindProp@12';
procedure FreePadrlist; external mapi32 name 'FreePadrlist@4';
procedure FreeProws; external mapi32 name 'FreeProws@4';
function HrQueryAllRows; external mapi32 name 'HrQueryAllRows@24';
function ScUNCFromLocalPath; external mapi32 name 'ScUNCFromLocalPath@12';
function ScLocalPathFromUNC; external mapi32 name 'ScLocalPathFromUNC@12';
function FtAddFt; external mapi32 name 'FtAddFt@16';
function FtMulDwDw; external mapi32 name 'FtMulDwDw@8';
function FtMulDw; external mapi32 name 'FtMulDw@12';
function FtSubFt; external mapi32 name 'FtSubFt@16';
function FtNegFt; external mapi32 name 'FtNegFt@8';
function ScCreateConversationIndex; external mapi32 name 'ScCreateConversationIndex@16';
procedure WrapStoreEntryID; external mapi32 name 'WrapStoreEntryID@24';
function RTFSync; external mapi32 name 'RTFSync';
function WrapCompressedRTFStream; external mapi32 name 'WrapCompressedRTFStream';
function HrIStorageFromStream; external mapi32 name 'HrIStorageFromStream@16';
function ScInitMapiUtil; external mapi32 name 'ScInitMapiUtil@4';
procedure DeinitMapiUtil; external mapi32 name 'DeinitMapiUtil@0';
{$ELSE}


var
  _CreateTable: Pointer;

function CreateTable;
begin
  GetProcedureAddress(_CreateTable, mapi32, 'CreateTable');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateTable]
  end;
end;

var
  _MAPIInitIdle: Pointer;

function MAPIInitIdle;
begin
  GetProcedureAddress(_MAPIInitIdle, mapi32, 'MAPIInitIdle');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MAPIInitIdle]
  end;
end;

var
  _FtgRegisterIdleRoutine: Pointer;

function FtgRegisterIdleRoutine;
begin
  GetProcedureAddress(_FtgRegisterIdleRoutine, mapi32, 'FtgRegisterIdleRoutine');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtgRegisterIdleRoutine]
  end;
end;

var
  _MAPIGetDefaultMalloc: Pointer;

function MAPIGetDefaultMalloc;
begin
  GetProcedureAddress(_MAPIGetDefaultMalloc, mapi32, 'MAPIGetDefaultMalloc');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MAPIGetDefaultMalloc]
  end;
end;

var
  _OpenStreamOnFile: Pointer;

function OpenStreamOnFile;
begin
  GetProcedureAddress(_OpenStreamOnFile, mapi32, 'OpenStreamOnFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_OpenStreamOnFile]
  end;
end;

var
  _PropCopyMore: Pointer;

function PropCopyMore;
begin
  GetProcedureAddress(_PropCopyMore, mapi32, 'PropCopyMore');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PropCopyMore]
  end;
end;

var
  _UlPropSize: Pointer;

function UlPropSize;
begin
  GetProcedureAddress(_UlPropSize, mapi32, 'UlPropSize');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UlPropSize]
  end;
end;

var
  _FEqualNames: Pointer;

function FEqualNames;
begin
  GetProcedureAddress(_FEqualNames, mapi32, 'FEqualNames');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FEqualNames]
  end;
end;

var
  _FPropContainsProp: Pointer;

function FPropContainsProp;
begin
  GetProcedureAddress(_FPropContainsProp, mapi32, 'FPropContainsProp');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FPropContainsProp]
  end;
end;

var
  _FPropCompareProp: Pointer;

function FPropCompareProp;
begin
  GetProcedureAddress(_FPropCompareProp, mapi32, 'FPropCompareProp');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FPropCompareProp]
  end;
end;

var
  _LPropCompareProp: Pointer;

function LPropCompareProp;
begin
  GetProcedureAddress(_LPropCompareProp, mapi32, 'LPropCompareProp');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_LPropCompareProp]
  end;
end;

var
  _HrAddColumns: Pointer;

function HrAddColumns;
begin
  GetProcedureAddress(_HrAddColumns, mapi32, 'HrAddColumns');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HrAddColumns]
  end;
end;

var
  _HrAddColumnsEx: Pointer;

function HrAddColumnsEx;
begin
  GetProcedureAddress(_HrAddColumnsEx, mapi32, 'HrAddColumnsEx');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HrAddColumnsEx]
  end;
end;

var
  _HrDispatchNotifications: Pointer;

function HrDispatchNotifications;
begin
  GetProcedureAddress(_HrDispatchNotifications, mapi32, 'HrDispatchNotifications');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HrDispatchNotifications]
  end;
end;

var
  _ScCountNotifications: Pointer;

function ScCountNotifications;
begin
  GetProcedureAddress(_ScCountNotifications, mapi32, 'ScCountNotifications');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ScCountNotifications]
  end;
end;

var
  _ScCopyNotifications: Pointer;

function ScCopyNotifications;
begin
  GetProcedureAddress(_ScCopyNotifications, mapi32, 'ScCopyNotifications');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ScCopyNotifications]
  end;
end;

var
  _ScRelocNotifications: Pointer;

function ScRelocNotifications;
begin
  GetProcedureAddress(_ScRelocNotifications, mapi32, 'ScRelocNotifications');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ScRelocNotifications]
  end;
end;

var
  _ScCountProps: Pointer;

function ScCountProps;
begin
  GetProcedureAddress(_ScCountProps, mapi32, 'ScCountProps');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ScCountProps]
  end;
end;

var
  _LpValFindProp: Pointer;

function LpValFindProp;
begin
  GetProcedureAddress(_LpValFindProp, mapi32, 'LpValFindProp');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_LpValFindProp]
  end;
end;

var
  _ScCopyProps: Pointer;

function ScCopyProps;
begin
  GetProcedureAddress(_ScCopyProps, mapi32, 'ScCopyProps');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ScCopyProps]
  end;
end;

var
  _ScRelocProps: Pointer;

function ScRelocProps;
begin
  GetProcedureAddress(_ScRelocProps, mapi32, 'ScRelocProps');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ScRelocProps]
  end;
end;

var
  _ScDupPropset: Pointer;

function ScDupPropset;
begin
  GetProcedureAddress(_ScDupPropset, mapi32, 'ScDupPropset');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ScDupPropset]
  end;
end;

var
  _UlAddRef: Pointer;

function UlAddRef;
begin
  GetProcedureAddress(_UlAddRef, mapi32, 'UlAddRef');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UlAddRef]
  end;
end;

var
  _UlRelease: Pointer;

function UlRelease;
begin
  GetProcedureAddress(_UlRelease, mapi32, 'UlRelease');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UlRelease]
  end;
end;

var
  _FPropExists: Pointer;

function FPropExists;
begin
  GetProcedureAddress(_FPropExists, mapi32, 'FPropExists');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FPropExists]
  end;
end;

var
  _PpropFindProp: Pointer;

function PpropFindProp;
begin
  GetProcedureAddress(_PpropFindProp, mapi32, 'PpropFindProp');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PpropFindProp]
  end;
end;

var
  _HrQueryAllRows: Pointer;

function HrQueryAllRows;
begin
  GetProcedureAddress(_HrQueryAllRows, mapi32, 'HrQueryAllRows');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HrQueryAllRows]
  end;
end;

var
  _ScUNCFromLocalPath: Pointer;

function ScUNCFromLocalPath;
begin
  GetProcedureAddress(_ScUNCFromLocalPath, mapi32, 'ScUNCFromLocalPath');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ScUNCFromLocalPath]
  end;
end;

var
  _ScLocalPathFromUNC: Pointer;

function ScLocalPathFromUNC;
begin
  GetProcedureAddress(_ScLocalPathFromUNC, mapi32, 'ScLocalPathFromUNC');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ScLocalPathFromUNC]
  end;
end;

var
  _FtAddFt: Pointer;

function FtAddFt;
begin
  GetProcedureAddress(_FtAddFt, mapi32, 'FtAddFt');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtAddFt]
  end;
end;

var
  _FtMulDwDw: Pointer;

function FtMulDwDw;
begin
  GetProcedureAddress(_FtMulDwDw, mapi32, 'FtMulDwDw');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtMulDwDw]
  end;
end;

var
  _FtMulDw: Pointer;

function FtMulDw;
begin
  GetProcedureAddress(_FtMulDw, mapi32, 'FtMulDw');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtMulDw]
  end;
end;

var
  _FtSubFt: Pointer;

function FtSubFt;
begin
  GetProcedureAddress(_FtSubFt, mapi32, 'FtSubFt');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtSubFt]
  end;
end;

var
  _FtNegFt: Pointer;

function FtNegFt;
begin
  GetProcedureAddress(_FtNegFt, mapi32, 'FtNegFt');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FtNegFt]
  end;
end;

var
  _ScCreateConversationIndex: Pointer;

function ScCreateConversationIndex;
begin
  GetProcedureAddress(_ScCreateConversationIndex, mapi32, 'ScCreateConversationIndex');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ScCreateConversationIndex]
  end;
end;

var
  _RTFSync: Pointer;

function RTFSync;
begin
  GetProcedureAddress(_RTFSync, mapi32, 'RTFSync');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RTFSync]
  end;
end;

var
  _WrapCompressedRTFStream: Pointer;

function WrapCompressedRTFStream;
begin
  GetProcedureAddress(_WrapCompressedRTFStream, mapi32, 'WrapCompressedRTFStream');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WrapCompressedRTFStream]
  end;
end;

var
  _HrIStorageFromStream: Pointer;

function HrIStorageFromStream;
begin
  GetProcedureAddress(_HrIStorageFromStream, mapi32, 'HrIStorageFromStream');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HrIStorageFromStream]
  end;
end;

var
  _ScInitMapiUtil: Pointer;

function ScInitMapiUtil;
begin
  GetProcedureAddress(_ScInitMapiUtil, mapi32, 'ScInitMapiUtil');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ScInitMapiUtil]
  end;
end;

var
  _MAPIDeinitIdle: Pointer;

procedure MAPIDeinitIdle;
begin
  GetProcedureAddress(_MAPIDeinitIdle, mapi32, 'MAPIDeinitIdle');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MAPIDeinitIdle]
  end;
end;

var
  _DeregisterIdleRoutine: Pointer;

procedure DeregisterIdleRoutine;
begin
  GetProcedureAddress(_DeregisterIdleRoutine, mapi32, 'DeregisterIdleRoutine');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DeregisterIdleRoutine]
  end;
end;

var
  _EnableIdleRoutine: Pointer;

procedure EnableIdleRoutine;
begin
  GetProcedureAddress(_EnableIdleRoutine, mapi32, 'EnableIdleRoutine');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EnableIdleRoutine]
  end;
end;

var
  _ChangeIdleRoutine: Pointer;

procedure ChangeIdleRoutine;
begin
  GetProcedureAddress(_ChangeIdleRoutine, mapi32, 'ChangeIdleRoutine');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ChangeIdleRoutine]
  end;
end;

var
  _HrAllocAdviseSink: Pointer;

procedure HrAllocAdviseSink;
begin
  GetProcedureAddress(_HrAllocAdviseSink, mapi32, 'HrAllocAdviseSink');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HrAllocAdviseSink]
  end;
end;

var
  _HrThisThreadAdviseSink: Pointer;

procedure HrThisThreadAdviseSink;
begin
  GetProcedureAddress(_HrThisThreadAdviseSink, mapi32, 'HrThisThreadAdviseSink');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HrThisThreadAdviseSink]
  end;
end;

var
  _BuildDisplayTable: Pointer;

procedure BuildDisplayTable;
begin
  GetProcedureAddress(_BuildDisplayTable, mapi32, 'BuildDisplayTable');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BuildDisplayTable]
  end;
end;

var
  _HrGetOneProp: Pointer;

procedure HrGetOneProp;
begin
  GetProcedureAddress(_HrGetOneProp, mapi32, 'HrGetOneProp');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HrGetOneProp]
  end;
end;

var
  _HrSetOneProp: Pointer;

procedure HrSetOneProp;
begin
  GetProcedureAddress(_HrSetOneProp, mapi32, 'HrSetOneProp');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_HrSetOneProp]
  end;
end;

var
  _FreePadrlist: Pointer;

procedure FreePadrlist;
begin
  GetProcedureAddress(_FreePadrlist, mapi32, 'FreePadrlist');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FreePadrlist]
  end;
end;

var
  _FreeProws: Pointer;

procedure FreeProws;
begin
  GetProcedureAddress(_FreeProws, mapi32, 'FreeProws');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FreeProws]
  end;
end;

var
  _WrapStoreEntryID: Pointer;

procedure WrapStoreEntryID;
begin
  GetProcedureAddress(_WrapStoreEntryID, mapi32, 'WrapStoreEntryID');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WrapStoreEntryID]
  end;
end;

var
  _DeinitMapiUtil: Pointer;

procedure DeinitMapiUtil;
begin
  GetProcedureAddress(_DeinitMapiUtil, mapi32, 'DeinitMapiUtil');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DeinitMapiUtil]
  end;
end;

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}


{$IFNDEF JWA_OMIT_SECTIONS}
initialization
{$IFNDEF JWA_INCLUDEMODE}

{$IFNDEF WAB_DYNAMIC_LINK_EXPLICIT}
  LoadWabUtil;
{$ENDIF}
finalization
  UnloadWabUtil;
{$ENDIF JWA_INCLUDEMODE}

end.

{$ENDIF JWA_OMIT_SECTIONS}