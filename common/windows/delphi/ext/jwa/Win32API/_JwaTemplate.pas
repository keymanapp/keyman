{******************************************************************************}
{                                                                              }
{ XXXX API interface Unit for Object Pascal                                    }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2008 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ Portions created by XXXXXXXXXXXXXXXXX are Copyright (C) xxxx-xxxx            }
{ XXXXXXXXXXXXXXXXX. All Rights Reserved.                                      }
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

This unit is a template! Adapt it for new JEDI API units. Don't override it.

{$IFNDEF JWA_OMIT_SECTIONS}
unit Jwa_UnitName; 

{$HPPEMIT ''}
{$HPPEMIT '#include "xxxx.h"'}
{$HPPEMIT ''}

{$I ..\Includes\JediAPILib.inc}
{$IFNDEF JWA_OMIT_SECTIONS}


interface

{$IFDEF JWA_WINDOWS}
  uses JwaWindows;
{$ELSE}
  uses <unit list>;  
{$ENDIF JWA_WINDOWS}


{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_IMPLEMENTATIONSECTION}
//add here public interface definition

{$ENDIF JWA_IMPLEMENTATIONSECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
implementation
uses JwaWinDllNames;
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}
//add here implementation stuff




{$IFNDEF JWA_INCLUDEMODE}
const
  LIBConstantName_LIB = '<includedlib>.dll'; //or use constants from JwaWinDllNames
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}

var
  _ConvertedFunction: Pointer;

function ConvertedFunction;
begin
  GetProcedureAddress(_ConvertedFunction, LIBConstantName_LIB, 'ConvertedFunction');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ConvertedFunction]
  end;
end;


{$ELSE}

function ConvertedFunction; external LIBConstantName_LIB name 'ConvertedFunction';

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
