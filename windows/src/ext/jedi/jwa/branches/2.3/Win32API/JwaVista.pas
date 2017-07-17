{******************************************************************************}
{                                                                              }
{ Windows API interface Unit for Object Pascal                                 }
{ Master file for Windows Vista applications                                   }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) Microsoft Corporation.       }
{  All Rights Reserved.                                                        }
{                                                                              }
{ The original Pascal code is: JwaVista.pas, released Octobre 2007.            }
{                                                                              }
{ Portions created by Christian Wimmer are Copyright (C) 2007                  }
{ Christian Wimmer. All Rights Reserved.                                       }
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
{ This unit contains declarations that are new in Windows Vista.               }
{ This unit can be used in programs to be run under older versions of windows. }
{ However you should not use these types in a semantic way.                    }
{ This unit is not part of JwaWindows.pas and MUST be included by uses clause. }
{ To use this unit you must compile JwaWindows.pas with include mode.}

{$IFDEF JWA_INCLUDEMODE}
This unit must not be included in JwaWindows.pas because it is not compatible!
{$ENDIF JWA_INCLUDEMODE}

unit JwaVista {$IFDEF DELPHI6_UP}deprecated{$ENDIF};
{
JwaVista was merged with the JEDI API headers and is no more supported.
}


interface


uses
{$IFDEF JWA_WINDOWS}
  JwaWindows,
{$ELSE}
  JwaWinType, JwaWinNT,
{$ENDIF}
  ActiveX;


implementation
uses JwaWinDLLNames;


end.
