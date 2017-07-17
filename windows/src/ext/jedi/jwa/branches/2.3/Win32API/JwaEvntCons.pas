{******************************************************************************}
{                                                                              }
{ Event Tracing API interface Unit for Object Pascal                           }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2008 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ Portions created by Christian Wimmer are Copyright (C) 20008                 }
{ Christian Wimmer. All Rights Reserved.                                       }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ The original file is: evntcons.h, released <unknown>.                        }
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
unit JwaEvntCons; 
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
  uses JwaWinBase, JwaWinNT, JwaWinType, JwaWmiStr, JwaEventTracing, JwaEvntProv,
    JwaEventDefs;
{$ENDIF JWA_WINDOWS}

{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}
//add here public interface definition

const
  EVENT_HEADER_EXT_TYPE_RELATED_ACTIVITYID   = $0001;
  EVENT_HEADER_EXT_TYPE_SID                  = $0002;
  EVENT_HEADER_EXT_TYPE_TS_ID                = $0003;
  EVENT_HEADER_EXT_TYPE_INSTANCE_INFO        = $0004;


  //
  // Structures for extended items.
  //
type
  _EVENT_EXTENDED_ITEM_INSTANCE = record
      InstanceId : ULONG;
      ParentInstanceId : ULONG;
      ParentGuid : GUID;
  end;
  EVENT_EXTENDED_ITEM_INSTANCE = _EVENT_EXTENDED_ITEM_INSTANCE;
  PEVENT_EXTENDED_ITEM_INSTANCE = ^EVENT_EXTENDED_ITEM_INSTANCE;

  TEventExtendedItemInstance = EVENT_EXTENDED_ITEM_INSTANCE;
  PEventExtendedItemInstance = ^TEventExtendedItemInstance;



  _EVENT_EXTENDED_ITEM_RELATED_ACTIVITYID = record
    RelatedActivityId : GUID;
  end;
  EVENT_EXTENDED_ITEM_RELATED_ACTIVITYID = _EVENT_EXTENDED_ITEM_RELATED_ACTIVITYID;
  PEVENT_EXTENDED_ITEM_RELATED_ACTIVITYID = ^EVENT_EXTENDED_ITEM_RELATED_ACTIVITYID;

  TEventExtendedItemRelatedActivityID = EVENT_EXTENDED_ITEM_RELATED_ACTIVITYID;
  PEventExtendedItemRelatedActivityID = ^TEventExtendedItemRelatedActivityID;

  _EVENT_EXTENDED_ITEM_TS_ID = record
    SessionId : ULONG;
  end;
  EVENT_EXTENDED_ITEM_TS_ID = _EVENT_EXTENDED_ITEM_TS_ID;
  PEVENT_EXTENDED_ITEM_TS_ID = ^EVENT_EXTENDED_ITEM_TS_ID;

  TEventExtendedItemTSID = EVENT_EXTENDED_ITEM_TS_ID;
  PEventExtendedItemTSID = ^TEventExtendedItemTSID; 

const
  EVENT_HEADER_PROPERTY_XML               = $0001;
  EVENT_HEADER_PROPERTY_FORWARDED_XML     = $0002;
  EVENT_HEADER_PROPERTY_LEGACY_EVENTLOG   = $0004;

  EVENT_HEADER_FLAG_EXTENDED_INFO         = $0001;
  EVENT_HEADER_FLAG_PRIVATE_SESSION       = $0002;
  EVENT_HEADER_FLAG_STRING_ONLY           = $0004;
  EVENT_HEADER_FLAG_TRACE_MESSAGE         = $0008;
  EVENT_HEADER_FLAG_NO_CPUTIME            = $0010;
  EVENT_HEADER_FLAG_32_BIT_HEADER         = $0020;
  EVENT_HEADER_FLAG_64_BIT_HEADER         = $0040;
  EVENT_HEADER_FLAG_CLASSIC_HEADER        = $0100;






const
  EVENT_ENABLE_PROPERTY_SID                   = $00000001;
  EVENT_ENABLE_PROPERTY_TS_ID                 = $00000002;

//
// Consumer API
//
  PROCESS_TRACE_MODE_REAL_TIME                = $00000100;
  PROCESS_TRACE_MODE_RAW_TIMESTAMP            = $00001000;
  PROCESS_TRACE_MODE_EVENT_RECORD             = $10000000;


//
// Event Security APIs
//
type
  EVENTSECURITYOPERATION = (
    EventSecuritySetDACL,
    EventSecuritySetSACL,
    EventSecurityAddDACL,
    EventSecurityAddSACL,
    EventSecurityMax
  );
  TEventSecurityOperation = EVENTSECURITYOPERATION;

{$IFDEF WINVISTA_UP}
function EventAccessControl(
    {IN} Guid : LPGUID;
    {IN} Operation : ULONG;
    {IN} Sid : PSID;
    {IN} Rights : ULONG;
    {IN} AllowOrDeny : BOOLEAN) : ULONG; stdcall;


function EventAccessQuery(
    {IN} Guid : LPGUID;
    {IN OUT} Buffer : PSECURITY_DESCRIPTOR;
    {IN OUT}var BufferSize : ULONG) : ULONG; stdcall;

function EventAccessRemove(
    {IN} Guid : LPGUID) : ULONG; stdcall;
{$ENDIF WINVISTA_UP}

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

{$IFNDEF DYNAMIC_LINK}
{$IFDEF WINVISTA_UP}
function EventAccessControl; external advapi32 name 'EventAccessControl';
function EventAccessQuery; external advapi32 name 'EventAccessQuery';
function EventAccessRemove; external advapi32 name 'EventAccessRemove';
{$ENDIF WINVISTA_UP}
{$ELSE}

{$IFDEF WINVISTA_UP}
var
  _EventAccessControl: Pointer;

function EventAccessControl;
begin
  GetProcedureAddress(_EventAccessControl, advapi32, 'EventAccessControl');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EventAccessControl]
  end;
end;

var
  _EventAccessQuery: Pointer;

function EventAccessQuery;
begin
  GetProcedureAddress(_EventAccessQuery, advapi32, 'EventAccessQuery');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EventAccessQuery]
  end;
end;

var
  _EventAccessRemove: Pointer;

function EventAccessRemove;
begin
  GetProcedureAddress(_EventAccessRemove, advapi32, 'EventAccessRemove');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EventAccessRemove]
  end;
end;
{$ENDIF WINVISTA_UP}

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}




{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
