{******************************************************************************}
{                                                                              }
{ Event Provider API interface Unit for Object Pascal                          }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2008 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ Portions created by Christian Wimmer are Copyright (C) 20008                 }
{ Christian Wimmer. All Rights Reserved.                                       }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ The original file is: evntprov.h, released <unknown>.                        }
{ The initial developer of the Pascal code is Christian Wimmer.                }
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
unit JwaEvntProv; 
{$IFDEF BCB1_UP}
This unit does not support Borland C++ yet!
{$ENDIF BCB1_UP}
{$ENDIF JWA_OMIT_SECTIONS}


{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

{$IFDEF JWA_WINDOWS}
  uses JwaWindows;
{$ELSE}
  uses JwaWinBase, JwaWinType, JwaWmiStr, JwaEventDefs;
{$ENDIF JWA_WINDOWS}


{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}
const
  EVENT_MIN_LEVEL                      = 0;
  EVENT_MAX_LEVEL                      = $ff;

  EVENT_ACTIVITY_CTRL_GET_ID           = 1;
  EVENT_ACTIVITY_CTRL_SET_ID           = 2;
  EVENT_ACTIVITY_CTRL_CREATE_ID        = 3;
  EVENT_ACTIVITY_CTRL_GET_SET_ID       = 4;
  EVENT_ACTIVITY_CTRL_CREATE_SET_ID    = 5;

  MAX_EVENT_DATA_DESCRIPTORS           = 128;
  MAX_EVENT_FILTER_DATA_SIZE           = 1024;

type
  REGHANDLE = ULONGLONG;
  PREGHANDLE = ^REGHANDLE;

  TRegHandle = REGHANDLE;
  //PRegHandle = ^TRegHandle;

//
// EVENT_DATA_DESCRIPTOR is used to pass in user data items
// in events.
// 
  _EVENT_DATA_DESCRIPTOR = record
    Ptr  : ULONGLONG;        // Pointer to data
    Size : ULONG;       // Size of data in bytes
    Reserved : ULONG;
  end;

  EVENT_DATA_DESCRIPTOR = _EVENT_DATA_DESCRIPTOR;
  PEVENT_DATA_DESCRIPTOR = ^EVENT_DATA_DESCRIPTOR;

  TEventDataDescriptor = EVENT_DATA_DESCRIPTOR;
  PEventDataDescriptor = ^TEventDataDescriptor;







//
// Optional callback function that users provide
//

  PENABLECALLBACK = function(
    {__in} const SourceId  : PGUID;
    {__in} IsEnabled : ULONG;
    {__in} Level : UCHAR;
    {__in} MatchAnyKeyword : ULONGLONG;
    {__in} MatchAllKeyword : ULONGLONG;
    {__in_opt} FilterData : PEVENT_FILTER_DESCRIPTOR;
    {__in_opt} CallbackContext : PVOID) : ULONG; stdcall;

//
// Registration APIs
//

{$IFDEF WINVISTA_UP}
function EventRegister(
    {__in} ProviderId : PGUID;
    {__in_opt} EnableCallback : PENABLECALLBACK;
    {__in_opt} CallbackContext : PVOID;
    {__out}var RegHandle : TRegHandle) : ULONG; stdcall;


function EventUnregister(
    {__in} RegHandle : TRegHandle) : ULONG; stdcall;


//
// Control (Is Enabled) APIs
//


function EventEnabled(
    {__in} RegHandle : TRegHandle;
    {__in} const EventDescriptor : PEVENT_DESCRIPTOR) : ULONG; stdcall;



function EventProviderEnabled(
    {__in} RegHandle : TRegHandle;
    {__in} Level : UCHAR;
    {__in} Keyword : ULONGLONG) : ULONG; stdcall;


//
// Writing (Publishing/Logging) APIs
//

function EventWrite(
    {__in} RegHandle : TRegHandle;
    {__in} const EventDescriptor : PEVENT_DESCRIPTOR;
    {__in} UserDataCount : ULONG;
    {__in_ecount_opt(UserDataCount)} UserData : PEVENT_DATA_DESCRIPTOR) : ULONG; stdcall;

function EventWriteTransfer(
    {__in} RegHandle : TRegHandle;
    {__in} const EventDescriptor : PEVENT_DESCRIPTOR;
    {__in_opt} ActivityId : PGUID;
    {__in} const RelatedActivityId : PGUID;
    {__in} UserDataCount : ULONG;
    {__in_ecount_opt(UserDataCount)} UserData : PEVENT_DATA_DESCRIPTOR) : ULONG; stdcall;

function EventWriteString(
    {__in} RegHandle : TRegHandle;
    {__in} Level : UCHAR;
    {__in} Keyword : ULONGLONG;
    {__in} const String_ : PWSTR) : ULONG; stdcall;


//
// ActivityId Control APIs
//

function EventActivityIdControl(
    {__in}ControlCode : ULONG;
    {__inout}out ActivityId : TGUID) : ULONG; stdcall;
{$ENDIF WINVISTA_UP}


//
// Macros to create Event and Event Data Descriptors
//

procedure EventDataDescCreate(
    {__out}out EventDataDescriptor : EVENT_DATA_DESCRIPTOR;
    {__in} const DataPtr : PVOID;
    {__in} DataSize : ULONG);


procedure EventDescCreate(
    {__out} out EventDescriptor : PEVENT_DESCRIPTOR;
    {__in}  Id : USHORT;
    {__in}  Version : UCHAR;
    {__in}  Channel : UCHAR;
    {__in}  Level : UCHAR;
    {__in}  Task : USHORT;
    {__in}  Opcode : UCHAR;
    {__in}  Keyword : ULONGLONG);

procedure EventDescZero(
    {__out}out EventDescriptor : PEVENT_DESCRIPTOR);


//
// Macros to extract info from an Event Descriptor
//

function EventDescGetId(
    {__in}const EventDescriptor : PEVENT_DESCRIPTOR) : USHORT;


function EventDescGetVersion(
    {__in}const EventDescriptor : PEVENT_DESCRIPTOR) : UCHAR;



function EventDescGetTask(
    {__in}const EventDescriptor : PEVENT_DESCRIPTOR) : USHORT;


function EventDescGetOpcode(
    {__in}const EventDescriptor : PEVENT_DESCRIPTOR) : UCHAR;


function EventDescGetChannel(
    {__in}const EventDescriptor : PEVENT_DESCRIPTOR) : UCHAR;


function EventDescGetLevel(
    {__in}const EventDescriptor : PEVENT_DESCRIPTOR) : UCHAR;



function EventDescGetKeyword(
    {__in}const EventDescriptor : PEVENT_DESCRIPTOR) : ULONGLONG;


//
// Macros to set info into an Event Descriptor
//



function EventDescSetId(
    {__in}EventDescriptor : PEVENT_DESCRIPTOR;
    {__in}Id : USHORT) : PEVENT_DESCRIPTOR;


function EventDescSetVersion(
    {__in}EventDescriptor : PEVENT_DESCRIPTOR;
    {__in}Version : UCHAR) : PEVENT_DESCRIPTOR;


function EventDescSetTask(
    {__in}EventDescriptor : PEVENT_DESCRIPTOR;
    {__in}Task : USHORT) : PEVENT_DESCRIPTOR;


function EventDescSetOpcode(
    {__in}EventDescriptor : PEVENT_DESCRIPTOR;
    {__in}Opcode : UCHAR) : PEVENT_DESCRIPTOR;


function EventDescSetLevel(
    {__in}EventDescriptor : PEVENT_DESCRIPTOR;
    {__in}Level : UCHAR) : PEVENT_DESCRIPTOR;

function EventDescSetChannel(
    {__in}EventDescriptor : PEVENT_DESCRIPTOR;
    {__in}Channel : UCHAR) : PEVENT_DESCRIPTOR;

function EventDescSetKeyword(
    {__in}EventDescriptor : PEVENT_DESCRIPTOR;
    {__in}Keyword : ULONGLONG) : PEVENT_DESCRIPTOR;


function EventDescOrKeyword(
    {__in}EventDescriptor : PEVENT_DESCRIPTOR;
    {__in}Keyword : ULONGLONG) : PEVENT_DESCRIPTOR;


{$ENDIF JWA_IMPLEMENTATIONSECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
implementation
uses JwaWinDllNames;
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}
//add here implementation stuff




{$IFNDEF JWA_INCLUDEMODE}
const
  advapi32 = 'Advapi32.dll';
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}



//
// Macros to create Event and Event Data Descriptors
//

procedure EventDataDescCreate(
    {__out}out EventDataDescriptor : EVENT_DATA_DESCRIPTOR;
    {__in} const DataPtr : PVOID;
    {__in} DataSize : ULONG);
begin
  EventDataDescriptor.Ptr := ULONGLONG(DataPtr);
  EventDataDescriptor.Size := DataSize;
  EventDataDescriptor.Reserved := 0;
end;

procedure EventDescCreate(
    {__out} out EventDescriptor : PEVENT_DESCRIPTOR;
    {__in}  Id : USHORT;
    {__in}  Version : UCHAR;
    {__in}  Channel : UCHAR;
    {__in}  Level : UCHAR;
    {__in}  Task : USHORT;
    {__in}  Opcode : UCHAR;
    {__in}  Keyword : ULONGLONG);
begin
  EventDescriptor.Id         := Id;
  EventDescriptor.Version    := Version;
  EventDescriptor.Channel    := Channel;
  EventDescriptor.Level      := Level;
  EventDescriptor.Task       := Task;
  EventDescriptor.Opcode     := Opcode;
  EventDescriptor.Keyword    := Keyword;
end;

procedure EventDescZero(
    {__out}out EventDescriptor : PEVENT_DESCRIPTOR);
begin
  ZeroMemory(@EventDescriptor, sizeof(EVENT_DESCRIPTOR));
end;

//
// Macros to extract info from an Event Descriptor
//

function EventDescGetId(
    {__in}const EventDescriptor : PEVENT_DESCRIPTOR) : USHORT;
begin
  result := EventDescriptor.Id;
end;


function EventDescGetVersion(
    {__in}const EventDescriptor : PEVENT_DESCRIPTOR) : UCHAR;
begin
  result := EventDescriptor.Version;
end;



function EventDescGetTask(
    {__in}const EventDescriptor : PEVENT_DESCRIPTOR) : USHORT;
begin
  result := EventDescriptor.Task;
end;


function EventDescGetOpcode(
    {__in}const EventDescriptor : PEVENT_DESCRIPTOR) : UCHAR;
begin
  result := EventDescriptor.Opcode;
end;



function EventDescGetChannel(
    {__in}const EventDescriptor : PEVENT_DESCRIPTOR) : UCHAR;
begin
  result := EventDescriptor.Channel;
end;


function EventDescGetLevel(
    {__in}const EventDescriptor : PEVENT_DESCRIPTOR) : UCHAR;
begin
  result := EventDescriptor.Level;
end;



function EventDescGetKeyword(
    {__in}const EventDescriptor : PEVENT_DESCRIPTOR) : ULONGLONG;
begin
  result := EventDescriptor.Keyword;
end;

//
// Macros to set info into an Event Descriptor
//



function EventDescSetId(
    {__in}EventDescriptor : PEVENT_DESCRIPTOR;
    {__in}Id : USHORT) : PEVENT_DESCRIPTOR;
begin
  EventDescriptor.Id         := Id;
  result := EventDescriptor;
end;


function EventDescSetVersion(
    {__in}EventDescriptor : PEVENT_DESCRIPTOR;
    {__in}Version : UCHAR) : PEVENT_DESCRIPTOR;
begin
  EventDescriptor.Version    := Version;
  result := EventDescriptor;
end;


function EventDescSetTask(
    {__in}EventDescriptor : PEVENT_DESCRIPTOR;
    {__in}Task : USHORT) : PEVENT_DESCRIPTOR;
begin
  EventDescriptor.Task       := Task;
  result := EventDescriptor;
end;



function EventDescSetOpcode(
    {__in}EventDescriptor : PEVENT_DESCRIPTOR;
    {__in}Opcode : UCHAR) : PEVENT_DESCRIPTOR;
begin
  EventDescriptor.Opcode     := Opcode;
  result := EventDescriptor;
end;


function EventDescSetLevel(
    {__in}EventDescriptor : PEVENT_DESCRIPTOR;
    {__in}Level : UCHAR) : PEVENT_DESCRIPTOR;
begin
  EventDescriptor.Level      := Level;
  result := EventDescriptor;
end;

function EventDescSetChannel(
    {__in}EventDescriptor : PEVENT_DESCRIPTOR;
    {__in}Channel : UCHAR) : PEVENT_DESCRIPTOR;
begin
  EventDescriptor.Channel    := Channel;
  result := EventDescriptor;
end;

function EventDescSetKeyword(
    {__in}EventDescriptor : PEVENT_DESCRIPTOR;
    {__in}Keyword : ULONGLONG) : PEVENT_DESCRIPTOR;
begin
  EventDescriptor.Keyword    := Keyword;
  result := EventDescriptor;
end;


function EventDescOrKeyword(
    {__in}EventDescriptor : PEVENT_DESCRIPTOR;
    {__in}Keyword : ULONGLONG) : PEVENT_DESCRIPTOR;
begin
  EventDescriptor.Keyword  := EventDescriptor.Keyword or Keyword;
  result := EventDescriptor;
end;


{$IFDEF DYNAMIC_LINK}

{$IFDEF WINVISTA_UP}
var
  _EventRegister: Pointer;

function EventRegister;
begin
  GetProcedureAddress(_EventRegister, advapi32, 'EventRegister');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EventRegister]
  end;
end;

var
  _EventUnregister: Pointer;

function EventUnregister;
begin
  GetProcedureAddress(_EventUnregister, advapi32, 'EventUnregister');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EventUnregister]
  end;
end;

var
  _EventEnabled: Pointer;

function EventEnabled;
begin
  GetProcedureAddress(_EventEnabled, advapi32, 'EventEnabled');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EventEnabled]
  end;
end;

var
  _EventProviderEnabled: Pointer;

function EventProviderEnabled;
begin
  GetProcedureAddress(_EventProviderEnabled, advapi32, 'EventProviderEnabled');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EventProviderEnabled]
  end;
end;

var
  _EventWrite: Pointer;

function EventWrite;
begin
  GetProcedureAddress(_EventWrite, advapi32, 'EventWrite');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EventWrite]
  end;
end;

var
  _EventWriteTransfer: Pointer;

function EventWriteTransfer;
begin
  GetProcedureAddress(_EventWriteTransfer, advapi32, 'EventWriteTransfer');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EventWriteTransfer]
  end;
end;

var
  _EventWriteString: Pointer;

function EventWriteString;
begin
  GetProcedureAddress(_EventWriteString, advapi32, 'EventWriteString');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EventWriteString]
  end;
end;

var
  _EventActivityIdControl: Pointer;

function EventActivityIdControl;
begin
  GetProcedureAddress(_EventActivityIdControl, advapi32, 'EventActivityIdControl');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EventActivityIdControl]
  end;
end;
{$ENDIF WINVISTA_UP}

{$ELSE}

//function <ConvertedFunction>; external advapi32 name 'X';

{$IFDEF WINVISTA_UP}
function EventRegister; external advapi32 name 'EventRegister';
function EventUnregister; external advapi32 name 'EventUnregister';
function EventEnabled; external advapi32 name 'EventEnabled';
function EventProviderEnabled; external advapi32 name 'EventProviderEnabled';
function EventWrite; external advapi32 name 'EventWrite';
function EventWriteTransfer; external advapi32 name 'EventWriteTransfer';
function EventWriteString; external advapi32 name 'EventWriteString';
function EventActivityIdControl; external advapi32 name 'EventActivityIdControl';
{$ENDIF WINVISTA_UP}


{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}





{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
