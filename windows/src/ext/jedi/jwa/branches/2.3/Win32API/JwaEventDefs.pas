{******************************************************************************}
{                                                                              }
{ Event Tracing Definition interface Unit for Object Pascal                    }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2008 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ Portions created by Christian Wimmer are Copyright (C) 20008                 }
{ Christian Wimmer. All Rights Reserved.                                       }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ The original files are: Evntrace.h, evntcons.h, evntprov.h                   }
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
unit JwaEventDefs;
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
  uses JwaWinBase, JwaWinNT, JwaWinType, JwaWmiStr, JwaBitFields;
{$ENDIF JWA_WINDOWS}



{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_IMPLEMENTATIONSECTION}
//add here public interface definition

type
//
// EVENT_DESCRIPTOR describes and categorizes an event.
// 
  _EVENT_DESCRIPTOR = record
    Id : USHORT;
    Version : UCHAR;
    Channel : UCHAR;
    Level   : UCHAR;
    Opcode  : UCHAR;
    Task    : USHORT;
    Keyword : ULONGLONG;
  end;

  EVENT_DESCRIPTOR = _EVENT_DESCRIPTOR;
  PEVENT_DESCRIPTOR = ^EVENT_DESCRIPTOR;

  TEventDescriptor = EVENT_DESCRIPTOR;
  PEventDescriptor = ^TEventDescriptor;


  _EVENT_HEADER = record
    Size : USHORT;                   // Event Size
    HeaderType : USHORT;             // Header Type
    Flags : USHORT;                  // Flags
    EventProperty : USHORT;          // User given event property
    ThreadId : ULONG;               // Thread Id
    ProcessId : ULONG;              // Process Id
    TimeStamp : LARGE_INTEGER;              // Event Timestamp
    ProviderId : GUID;             // Provider Id
    EventDescriptor : EVENT_DESCRIPTOR;        // Event Descriptor
    Time : record
      case Boolean of
        true : (
                KernelTime : ULONG;           // Kernel Mode CPU ticks
                UserTime : ULONG);            // User mode CPU ticks
        false : (ProcessorTime : ULONG64);        // Processor Clock
    end;
    ActivityId : GUID;             // Activity Id
  end;

  EVENT_HEADER = _EVENT_HEADER;
  PEVENT_HEADER = ^EVENT_HEADER;

  TEventHeader = EVENT_HEADER;
  PEventHeader = ^TEventHeader;

  _ETW_BUFFER_CONTEXT = record
    ProcessorNumber : UCHAR;
    Alignment : UCHAR;
    LoggerId  : USHORT;
  end;
  ETW_BUFFER_CONTEXT  = _ETW_BUFFER_CONTEXT;
  PETW_BUFFER_CONTEXT = ^ETW_BUFFER_CONTEXT;

  TETWBufferContext = ETW_BUFFER_CONTEXT;
  PETWBufferContext = ^TETWBufferContext;

  _EVENT_HEADER_EXTENDED_DATA_ITEM  = record
    Reserved1 : USHORT;                      // Reserved for internal use
    ExtType : USHORT;                        // Extended info type
    //Bitfield member. Use utility functions from JwaBitFields.pas
    //Use GetEventHeaderExtendedDataItemLinkage to get linkage flag
    ExtTypeValue: Set Of(
           Linkage,
           Reserved2,
{$IFDEF DELPHI6_UP}
           _AlignExtTypeValue = al16bit
{$ELSE}
          _AlignExtTypeValue1,
          _AlignExtTypeValue2,
          _AlignExtTypeValue3,
          _AlignExtTypeValue4,
          _AlignExtTypeValue5,
          _AlignExtTypeValue6,
          _AlignExtTypeValue7,
          _AlignExtTypeValue8,
          _AlignExtTypeValue9,
          _AlignExtTypeValue10,
          _AlignExtTypeValue11,
          _AlignExtTypeValue12,
          _AlignExtTypeValue13,
          _AlignExtTypeValue14,
          _AlignExtTypeValue15,
          _AlignExtTypeValue16
{$ENDIF}
           ); //contains Linkage and Reserverd2 !
    {
    Linkage is really a bit field
    struct {
        USHORT  Linkage             :  1;       // Indicates additional extended
                                                // data item
        USHORT  Reserved2           : 15;
    }
    DataSize : USHORT;                       // Size of extended info data
    DataPtr : ULONGLONG;                        // Pointer to extended info data
  end;

  EVENT_HEADER_EXTENDED_DATA_ITEM = _EVENT_HEADER_EXTENDED_DATA_ITEM;
  PEVENT_HEADER_EXTENDED_DATA_ITEM = ^EVENT_HEADER_EXTENDED_DATA_ITEM;

  TEventHeaderExtendedDataItem = EVENT_HEADER_EXTENDED_DATA_ITEM;
  PEventHeaderExtendedDataItem = ^TEventHeaderExtendedDataItem;  

  _EVENT_RECORD = record
    EventHeader : EVENT_HEADER;            // Event header
    BufferContext : ETW_BUFFER_CONTEXT;          // Buffer context
    ExtendedDataCount : USHORT;      // Number of extended
                                                // data items
    UserDataLength : USHORT;         // User data length
    ExtendedData : PEVENT_HEADER_EXTENDED_DATA_ITEM;     // Pointer to an array of
                                                         // extended data items
    UserData : PVOID;               // Pointer to user data
    UserContext : PVOID;            // Context from OpenTrace
  end;

  EVENT_RECORD = _EVENT_RECORD;
  PEVENT_RECORD = ^EVENT_RECORD;

  TEventRecord = EVENT_RECORD;
  PEventRecord = ^TEventRecord;

//
// EVENT_FILTER_DESCRIPTOR is used to pass in enable filter
// data item to a user callback function.
//
  _EVENT_FILTER_DESCRIPTOR = record
    Ptr   : ULONGLONG;
    Size  : ULONG;
    FilterType : ULONG;
  end;

  EVENT_FILTER_DESCRIPTOR = _EVENT_FILTER_DESCRIPTOR;
  PEVENT_FILTER_DESCRIPTOR = ^EVENT_FILTER_DESCRIPTOR;

  TEventFilterDescriptor = EVENT_FILTER_DESCRIPTOR;
  PEventFilterDescriptor = ^TEventFilterDescriptor;

{GetEventHeaderExtendedDataItemLinkage returns the linkage flag from
 structure TEventHeaderExtendedDataItem.
}
function GetEventHeaderExtendedDataItemLinkage(const
  DataItem : TEventHeaderExtendedDataItem) : Boolean;
{$ENDIF JWA_IMPLEMENTATIONSECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
implementation
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}
//add here implementation stuff


function GetEventHeaderExtendedDataItemLinkage(const
  DataItem : TEventHeaderExtendedDataItem) : Boolean;
begin
  //  result := Boolean(ValueFromBitSet(DataItem.ExtTypeValue, Byte(Linkage), 1);
  result := Linkage in DataItem.ExtTypeValue;
end;


{$ENDIF JWA_INTERFACESECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
