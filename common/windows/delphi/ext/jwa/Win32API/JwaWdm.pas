{******************************************************************************}
{                                                                              }
{ WDM API interface Unit for Object Pascal                                     }
{                                                                              }
{ This header file contains some parts of wdm.h from the DDK that are usefull  }
{ for reading RES_RESOURCE_LIST keys from the registry. This type seems to be  }
{ undocumented.                                                                }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2008 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ Portions created by Remko Weijnen are Copyright (C) 2009                     }
{ Remko Weijnen. All Rights Reserved.                                          }
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
unit JwaWdm;


{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}
interface

{$IFDEF JWA_WINDOWS}
  uses JwaWindows;
{$ELSE}
  uses JwaWinType;
{$ENDIF JWA_WINDOWS}


{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_IMPLEMENTATIONSECTION}
const
  CmResourceTypeNull = 0; // Meaning 'All' or 'None'
  CmResourceTypePort = 1;
  CmResourceTypeInterrupt = 2;
  CmResourceTypeMemory = 3;
  CmResourceTypeDma = 4;
  CmResourceTypeDeviceSpecific = 5;
  CmResourceTypeBusNumber = 6;
  CmResourceTypeMaximum = 7;

type
  { Defines the ShareDisposition in the RESOURCE_DESCRIPTOR }
  {$MINENUMSIZE 1}   // CM_SHARE_DISPOSITION must be 1 byte
  CM_SHARE_DISPOSITION = (
    CmResourceShareUndetermined,
    CmResourceShareDeviceExclusive, { Reserved }
    CmResourceShareDriverExclusive
  );

const
  { Define the bit masks for Flags common for all Cm Resource types }
  CM_RESOURCE_COMMON_COMPUTE_LENGTH_FROM_DEPENDENTS = $8000;
  CM_RESOURCE_COMMON_NOT_REASSIGNED = $4000;
  CM_RESOURCE_COMMON_SUBSTRACTIVE = $2000;

  { Define the bit masks for Flags when type is CmResourceTypeInterrupt }
  CM_RESOURCE_INTERRUPT_LEVEL_SENSITIVE = 0;
  CM_RESOURCE_INTERRUPT_LATCHED = 1;

  { Define the bit masks for Flags when type is CmResourceTypeMemory }
  CM_RESOURCE_MEMORY_READ_WRITE = $0000;
  CM_RESOURCE_MEMORY_READ_ONLY = $0001;
  CM_RESOURCE_MEMORY_WRITE_ONLY = $0002;
  CM_RESOURCE_MEMORY_PREFETCHABLE = $0004;
  CM_RESOURCE_MEMORY_COMBINEDWRITE = $0008;
  CM_RESOURCE_MEMORY_24 = $0010;

  { Define the bit masks for Flags when type is CmResourceTypePort }
  CM_RESOURCE_PORT_MEMORY = $0000;
  CM_RESOURCE_PORT_IO = $0001;
  CM_RESOURCE_PORT_10_BIT_DECODE = $0004;
  CM_RESOURCE_PORT_12_BIT_DECODE = $0008;
  CM_RESOURCE_PORT_16_BIT_DECODE = $0010;
  CM_RESOURCE_PORT_POSITIVE_DECODE = $0020;

  { Define the bit masks for Flags when type is CmResourceTypeDma }
  CM_RESOURCE_DMA_8 = $0000;
  CM_RESOURCE_DMA_16 = $0001;
  CM_RESOURCE_DMA_32 = $0002;

type
  { Range of resources, inclusive. These are physical, bus relative. }
  { Port and Memory below have the same layout as (unused) Generic }
  { Note: these RDD records need to be Packed to arrive unalligned in }
  { the variant record CM_PARTIAL_RESOURCE_DESCRIPTOR }
  RDD_Generic = packed record
    Start: PHYSICAL_ADDRESS;
    Length: ULONG;
  end;
  PRDD_Generic = ^RDD_Generic;

  // This is for x64
  RDD_GENERIC_EX = packed record
    Start: PHYSICAL_ADDRESS;
    Length: ULONGLONG;
  end;
  PRDD_GENERIC_EX = ^RDD_GENERIC_EX;


  RDD_PORT = RDD_GENERIC;
  PRDD_Port = ^RDD_Port;

  // This is for x64
  RDD_PORT_EX = RDD_GENERIC_EX;
  PRDD_PORT_EX = ^RDD_PORT_EX;

  RDD_MEMORY = RDD_GENERIC;
  PRDD_MEMORY = ^RDD_MEMORY;

  // This is for x64
  RDD_MEMORY_EX = RDD_GENERIC_EX;
  PRDD_MEMORY_EX = ^RDD_MEMORY_EX;

  { IRQL and vector. Should be same values as were passed to }
  { HalGetInterruptVector(). }
  RDD_INTERRUPT = packed record
    Level: ULONG;
    Vector: ULONG;
    Affinity: ULONG;
  end;
  PRDD_INTERRUPT = ^RDD_INTERRUPT;

  // This is for x64
  RDD_INTERRUPT_EX = packed record
    Level: ULONG;
    Vector: ULONG;
    Affinity: ULONG;
  end;
  PRDD_INTERRUPT_EX = ^RDD_INTERRUPT_EX;


  { Physical DMA channel. }
  RDD_DMA = packed record
    Channel: ULONG;
    Port: ULONG;
    Reserved1: ULONG;
    end;
  PRDD_DMA = ^RDD_DMA;


  { Device driver private data, usually used to help it figure }
  { what the resource assignments decisions that were made. }
  RDD_DevicePrivate = packed record
    Data: array[0..2] of ULONG;
  end;
  PRDD_DevicePrivate = ^RDD_DevicePrivate;

  { Bus Number information. }
  RDD_BusNumber = packed record
    Start: ULONG;
    Length: ULONG;
    Reserved: ULONG;
    end;
  PRDD_BusNumber = ^RDD_BusNumber;


  { Device Specific information defined by the driver. }
  { The DataSize field indicates the size of the data in bytes. The }
  { data is located immediately after the DeviceSpecificData field in }
  { the structure. }
  RDD_DeviceSpecificData = packed record
    DataSize: ULONG;
    Reserved1: ULONG;
    Reserved2: ULONG;
  end;
  PRDD_DeviceSpecificData = ^RDD_DeviceSpecificData;


  CM_PARTIAL_RESOURCE_DESCRIPTOR = packed record
    ResType: UCHAR;
    ShareDisposition: CM_SHARE_DISPOSITION; // has UCHAR = Byte size
    Flags: USHORT;
    case Byte of
      0: (Generic: RDD_Generic);
      1: (Port: RDD_Port);
      2: (Interrupt: RDD_Interrupt);
      3: (Memory: RDD_Memory);
      4: (Dma: RDD_DMA);
      5: (DevicePrivate: RDD_DevicePrivate);
      6: (BusNumber: RDD_BusNumber);
      7: (DeviceSpecificData: RDD_DeviceSpecificData);
  end;
  PCM_PARTIAL_RESOURCE_DESCRIPTOR = ^CM_PARTIAL_RESOURCE_DESCRIPTOR;

  // This is for x64
  CM_PARTIAL_RESOURCE_DESCRIPTOR_EX = packed record
    ResType: UCHAR;
    ShareDisposition: CM_SHARE_DISPOSITION; // has UCHAR = Byte size
    Flags: USHORT;
    case Byte of
      0: (Generic: RDD_GENERIC_EX);
      1: (Port: RDD_PORT_EX);
      2: (Interrupt: RDD_INTERRUPT_EX);
      3: (Memory: RDD_MEMORY_EX);
      4: (Dma: RDD_DMA);
      5: (DevicePrivate: RDD_DevicePrivate);
      6: (BusNumber: RDD_BusNumber);
      7: (DeviceSpecificData: RDD_DeviceSpecificData);
  end;
  PCM_PARTIAL_RESOURCE_DESCRIPTOR_EX = ^CM_PARTIAL_RESOURCE_DESCRIPTOR_EX;

  CM_PARTIAL_RESOURCE_LIST = packed record
    Version: USHORT;
    Revision: USHORT;
    Count: ULONG;
    PartialDescriptors: array[0..ANYSIZE_ARRAY-1] of CM_PARTIAL_RESOURCE_DESCRIPTOR;
  end;
  PCM_PARTIAL_RESOURCE_LIST = ^CM_PARTIAL_RESOURCE_LIST;

  CM_FULL_RESOURCE_DESCRIPTOR = packed record
    InterfaceType: ULONG;//INTERFACE_TYPE;
    BusNumber: ULONG;
    PartialResourceList: CM_PARTIAL_RESOURCE_LIST;
  end;
  PCM_FULL_RESOURCE_DESCRIPTOR = ^CM_FULL_RESOURCE_DESCRIPTOR;

  CM_RESOURCE_LIST = packed record
    Count: ULONG;
    List: array[0..ANYSIZE_ARRAY-1] of CM_FULL_RESOURCE_DESCRIPTOR;
  end;
  PCM_RESOURCE_LIST = ^CM_RESOURCE_LIST;

{$ENDIF JWA_IMPLEMENTATIONSECTION}


{$IFNDEF JWA_OMIT_SECTIONS}
implementation
uses JwaWinDllNames;
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}
  // Dummy;
{$ELSE}
  // Dummy;
{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}

