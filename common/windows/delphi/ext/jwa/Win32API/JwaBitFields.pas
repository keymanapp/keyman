{******************************************************************************}
{                                                                              }
{ Bitfield Emulation Unit for Object Pascal                                    }
{ for explantion of how to use this unit see:                                  }
{ http://blog.delphi-jedi.net/2008/05/01/working-with-bitfields-in-delphi/     }
{                                                                              }
{ Portions created by Remko Weijnen are Copyright (C) 2008                     }
{ All Rights Reserved.                                                         }
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
unit JwaBitFields;


{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

{$IFDEF JWA_WINDOWS}
  uses SysUtils // SysUtils is needed for raising Exception in ValueFromBitSet;
{$ELSE}
  uses SysUtils;
{$ENDIF JWA_WINDOWS}


{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_IMPLEMENTATIONSECTION}

const
// constants below are used to align bitfields
  al16bit=15;
  al32Bit=31;
  al64bit=63;
  al96bit=95;
  al128bit=127;
  al160bit=159;
  al192bit=191;
  al224bit=221;
  al256bit=255;

type
// TMaxBitSet is the largest possible bitset (256 bits)
  TMaxBitSet = Set of Byte;
  PMaxBitSet = ^TMaxBitSet;

// This function can be used to retreive the value of a bitfield consisting of
// multiple bits
function ValueFromBitSet(var ABitSet; const StartBit: Byte;
  const Count: Byte): Int64;

{$ENDIF JWA_IMPLEMENTATIONSECTION}


{$IFNDEF JWA_OMIT_SECTIONS}
implementation
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}
function ValueFromBitSet(var ABitSet; const StartBit: Byte;
  const Count: Byte): Int64;
var
  MaxBitSet: TMaxBitSet;
  i, BitValue: Integer;
begin
  // The result can contain max. 64 bit value, Raise if Count > 64
  if Count > 64 then Raise EIntOverflow.Create('Count cannot exceed 64');

  // A Delphi Set contains at most 256 bits. So we raise Exception is we exceed
  if StartBit + Count > 255 then Raise
    EIntOverflow.Create('Startbit + Count cannot exceed max. set value (255)');

  Result := 0;
  BitValue := 1;

  // A Delphi Set Of can hold a maximum of 256 bits, since we do not know
  // which size was passed to us we cast to 256 bits.
  MaxBitSet := TMaxBitSet(ABitSet);

  // Loop through the requested bits
  for i := StartBit to StartBit+(Count-1) do
  begin

    // is the bit set?
    if i in MaxBitSet then
    begin
      // Multiply with BitValue and add to result
      Result := Result + BitValue;
    end;

    // Multiply BitValue by 2
    BitValue := BitValue shl 1;
  end;
end;




{$IFNDEF JWA_INCLUDEMODE}
const
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}

{$ELSE}

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
