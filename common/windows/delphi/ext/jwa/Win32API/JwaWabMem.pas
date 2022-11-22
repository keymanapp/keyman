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
{ The original file is: wabmem.h, released 31 Jan 2000.                        }
{ The original Pascal code is: WabMem.pas, released 15 Mar 2000.   			   }
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
unit JwaWabMem;

interface

uses
  Windows, ActiveX, JwaWabApi;

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

(*$HPPEMIT '#include <wabmem.h>'*)

{$I ..\Includes\JediAPILib.inc}


{$ENDIF JWA_OMIT_SECTIONS}


{$IFNDEF JWA_IMPLEMENTATIONSECTION}

type
  PMapiAllocateBuffer = ^TMapiAllocateBuffer;
  MAPIALLOCATEBUFFER = function (cbSize: ULONG; var lppBuffer: Pointer): SCODE; stdcall;
  {$EXTERNALSYM MAPIALLOCATEBUFFER}
  TMapiAllocateBuffer = MAPIALLOCATEBUFFER;

  PMapiAllocateMore = ^TMapiAllocateMore;
  MAPIALLOCATEMORE = function (cbSize: ULONG; lpObject: Pointer;
    var lppBuffer: Pointer): SCODE; stdcall;
  {$EXTERNALSYM MAPIALLOCATEMORE}
  TMapiAllocateMore = MAPIALLOCATEMORE;

  PMapiFreeBuffer = ^TMapiFreeBuffer;
  MAPIFREEBUFFER = function (lpBuffer: Pointer): SCODE; stdcall;
  {$EXTERNALSYM MAPIFREEBUFFER}
  TMapiFreeBuffer = MAPIFREEBUFFER;

  PWabAllocateBuffer = ^TWabAllocateBuffer;
  WABALLOCATEBUFFER = function (lpWABObject: IWabObject; cbSize: ULONG;
    var lppBuffer: Pointer): SCODE; stdcall;
  {$EXTERNALSYM WABALLOCATEBUFFER}
  TWabAllocateBuffer = WABALLOCATEBUFFER;

  PWabAllocateMore = ^TWabAllocateMore;
  WABALLOCATEMORE = function (lpWABObject: IWabObject; cbSize: ULONG;
    lpObject: Pointer; var lppBuffer: Pointer): SCODE; stdcall;
  {$EXTERNALSYM WABALLOCATEMORE}
  TWabAllocateMore = WABALLOCATEMORE;

  PWabFreeBuffer = ^TWabFreeBuffer;
  WABFREEBUFFER = function (lpWABObject: IWabObject; lppBuffer: Pointer): SCODE; stdcall;
  {$EXTERNALSYM WABFREEBUFFER}
  TWabFreeBuffer = WABFREEBUFFER;

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INTERFACESECTION}
{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
