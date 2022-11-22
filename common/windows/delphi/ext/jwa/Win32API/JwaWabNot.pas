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
{ The original file is: wabnot.h, released 31 Jan 2000.            			   }
{ The original Pascal code is: WabNot.pas, released 15 Mar 2000.   			   }
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
unit JwaWabNot;

interface

uses
  Windows, ActiveX, JwaWabDefs;

{$I ..\Includes\JediAPILib.inc}

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

(*$HPPEMIT '#include <wabnot.h>'*)

{$ENDIF JWA_OMIT_SECTIONS}


{$IFNDEF JWA_IMPLEMENTATIONSECTION}
{ Notification key structure for the MAPI notification engine }

type
  PNotifyKey = ^TNotifyKey;
  NOTIFKEY = record
    cb: ULONG;                        // How big the key is
    ab: array[0..MAPI_DIM-1] of Byte; // Key contents
  end;
  {$EXTERNALSYM NOTIFKEY}
  TNotifyKey = NOTIFKEY;

(*!!!
#define CbNewNOTIFKEY(_cb)		(offsetof(NOTIFKEY,ab) + (_cb))
#define CbNOTIFKEY(_lpkey)		(offsetof(NOTIFKEY,ab) + (_lpkey)->cb)
#define SizedNOTIFKEY(_cb, _name) \
	struct _NOTIFKEY_ ## _name \
{ \
	ULONG		cb; \
	BYTE		ab[_cb]; \
} _name
*)

{ For Subscribe() }

const
  NOTIFY_SYNC           = ULONG($40000000);
  {$EXTERNALSYM NOTIFY_SYNC}

{ For Notify() }

  NOTIFY_CANCELED       = ULONG($80000000);
  {$EXTERNALSYM NOTIFY_CANCELED}

{ From the Notification Callback function (well, this is really a ulResult) }

  CALLBACK_DISCONTINUE	= ULONG($80000000);
  {$EXTERNALSYM CALLBACK_DISCONTINUE}

{ For Transport's SpoolerNotify() }

  NOTIFY_NEWMAIL        = ULONG($00000001);
  {$EXTERNALSYM NOTIFY_NEWMAIL}
  NOTIFY_READYTOSEND    = ULONG($00000002);
  {$EXTERNALSYM NOTIFY_READYTOSEND}
  NOTIFY_SENTDEFERRED   = ULONG($00000004);
  {$EXTERNALSYM NOTIFY_SENTDEFERRED}
  NOTIFY_CRITSEC        = ULONG($00001000);
  {$EXTERNALSYM NOTIFY_CRITSEC}
  NOTIFY_NONCRIT        = ULONG($00002000);
  {$EXTERNALSYM NOTIFY_NONCRIT}
  NOTIFY_CONFIG_CHANGE	= ULONG($00004000);
  {$EXTERNALSYM NOTIFY_CONFIG_CHANGE}
  NOTIFY_CRITICAL_ERROR	= ULONG($10000000);
  {$EXTERNALSYM NOTIFY_CRITICAL_ERROR}

{ For Message Store's SpoolerNotify() }

  NOTIFY_NEWMAIL_RECEIVED = ULONG($20000000);
  {$EXTERNALSYM NOTIFY_NEWMAIL_RECEIVED}

{ For ModifyStatusRow() }

  STATUSROW_UPDATE = ULONG($10000000);
  {$EXTERNALSYM STATUSROW_UPDATE}

{ For IStorageFromStream() }

  STGSTRM_RESET   = ULONG($00000000);
  {$EXTERNALSYM STGSTRM_RESET}
  STGSTRM_CURRENT = ULONG($10000000);
  {$EXTERNALSYM STGSTRM_CURRENT}
  STGSTRM_MODIFY  = ULONG($00000002);
  {$EXTERNALSYM STGSTRM_MODIFY}
  STGSTRM_CREATE  = ULONG($00001000);
  {$EXTERNALSYM STGSTRM_CREATE}

{ For GetOneOffTable() }
//****** MAPI_UNICODE			((ULONG) 0x80000000) */

{ For CreateOneOff() }
//****** MAPI_UNICODE			((ULONG) 0x80000000) */
//****** MAPI_SEND_NO_RICH_INFO	((ULONG) 0x00010000) */

{ For ReadReceipt() }
  MAPI_NON_READ = ULONG($00000001);
  {$EXTERNALSYM MAPI_NON_READ}

{ For DoConfigPropSheet() }
//****** MAPI_UNICODE			((ULONG) 0x80000000) */
{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INTERFACESECTION}
{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}