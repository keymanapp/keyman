{******************************************************************************}
{                                                                              }
{ Winsock2 Service Provider ordering API interface Unit for Object Pascal      }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: sporder.h, released June 2000. The original Pascal     }
{ code is: SpOrder.pas, released June 2001. The initial developer of the       }
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

// $Id: JwaSpOrder.pas,v 1.9 2007/09/05 11:58:52 dezipaitor Exp $
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaSpOrder;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "sporder.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaWinType;
{$ENDIF JWA_OMIT_SECTIONS}
{$IFNDEF JWA_IMPLEMENTATIONSECTION}
  
function WSCWriteProviderOrder(lpwdCatalogEntryId: LPDWORD; dwNumberOfEntries: DWORD): Integer; stdcall;
{$EXTERNALSYM WSCWriteProviderOrder}

type
  LPWSCWRITEPROVIDERORDER = function(lpwdCatalogEntryId: LPDWORD; dwNumberOfEntries: DWORD): Integer; stdcall;
  {$EXTERNALSYM LPWSCWRITEPROVIDERORDER}

function WSCWriteNameSpaceOrder(const lpProviderId: TGUID; dwNumberOfEntries: DWORD): Integer; stdcall;
{$EXTERNALSYM WSCWriteNameSpaceOrder}

type
  LPWSCWRITENAMESPACEORDER = function(const lpProviderId: TGUID; dwNumberOfEntries: DWORD): Integer; stdcall;
  {$EXTERNALSYM LPWSCWRITENAMESPACEORDER}

{$ENDIF JWA_IMPLEMENTATIONSECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  sporderlib = 'sporder.dll';
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}

var
  _WSCWriteProviderOrder: Pointer;

function WSCWriteProviderOrder;
begin
  GetProcedureAddress(_WSCWriteProviderOrder, sporderlib, 'WSCWriteProviderOrder');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WSCWriteProviderOrder]
  end;
end;

var
  _WSCWriteNameSpaceOrder: Pointer;

function WSCWriteNameSpaceOrder;
begin
  GetProcedureAddress(_WSCWriteNameSpaceOrder, sporderlib, 'WSCWriteNameSpaceOrder');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WSCWriteNameSpaceOrder]
  end;
end;

{$ELSE}

function WSCWriteProviderOrder; external sporderlib name 'WSCWriteProviderOrder';
function WSCWriteNameSpaceOrder; external sporderlib name 'WSCWriteNameSpaceOrder';

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
