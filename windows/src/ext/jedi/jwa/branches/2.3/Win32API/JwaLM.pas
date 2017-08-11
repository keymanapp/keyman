{******************************************************************************}
{                                                                              }
{ Windows API interface Unit for Object Pascal                                 }
{ Master file for Lan Manager applications                                     }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) Microsoft Corporation.       }
{  All Rights Reserved.                                                        }
{                                                                              }
{ The original Pascal code is: JwaLM.pas, released September 2005.             }
{ The initial developer of the Pascal code is                                  }
{ Robert Marquardt (robert_marquardt att gmx dott de).                         }
{                                                                              }
{ Portions created by Robert Marquardt are Copyright (C) 2005                  }
{ Robert Marquardt. All Rights Reserved.                                       }
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

// $Id: JwaLM.pas,v 1.11 2007/09/05 11:58:50 dezipaitor Exp $

{$IFNDEF JWA_INCLUDEMODE}
This unit must not be included in JwaWindows.pas because the members are
already declared.
{$ENDIF JWA_INCLUDEMODE}

{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaLM;

{$WEAKPACKAGEUNIT}

{$ENDIF JWA_OMIT_SECTIONS}

{$DEFINE JWALM_PAS}
{.$DEFINE JWA_INCLUDEMODE}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaWindows;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}

{$DEFINE JWA_INTERFACESECTION}
{$DEFINE JWA_OMIT_SECTIONS_LM}



 {$I JwaLmCons.pas}     // LAN Manager common definitions
 {$I JwaLmErr.pas}      // LAN Manager network error definitions
 {$I JwaLmAccess.pas}   // Access, Domain, Group and User classes
 {$I JwaLmAlert.pas}    // Alerter
 {$I JwaLmShare.pas}    // Connection, File, Session and Share classes
 {$I JwaLmMsg.pas}      // Message class
{$I JwaLmRemUtl.pas}   // Remote Utility class
{$I JwaLmRepl.pas}     // Replicator class
{$I JwaLmServer.pas}   // Server class
{$I JwaLmSvc.pas}      // Service class
{$I JwaLmUse.pas}      // Use class
{$I JwaLmWkSta.pas}    // Workstation class
{$I JwaLmApiBuf.pas}   // NetApiBuffer class
{$I JwaLmErrLog.pas}   // NetErrorLog class
{$I JwaLmConfig.pas}   // NetConfig class
{$I JwaLmStats.pas}    // NetStats class
{$I JwaLmAudit.pas}    // NetAudit class
{$I JwaLmJoin.pas}     // NetJoinDomain class



{$UNDEF JWA_INTERFACESECTION}
{$UNDEF JWA_OMIT_SECTIONS_LM}

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation

uses
  JwaWinDLLNames;
{$ENDIF JWA_OMIT_SECTIONS}

{$DEFINE JWA_IMPLEMENTATIONSECTION}
{$DEFINE JWA_OMIT_SECTIONS_LM}

 {$I JwaLmCons.pas}     // LAN Manager common definitions
 {$I JwaLmErr.pas}      // LAN Manager network error definitions
 {$I JwaLmAccess.pas}   // Access, Domain, Group and User classes
 {$I JwaLmAlert.pas}    // Alerter
 {$I JwaLmShare.pas}    // Connection, File, Session and Share classes
 {$I JwaLmMsg.pas}      // Message class
{$I JwaLmRemUtl.pas}   // Remote Utility class
{$I JwaLmRepl.pas}     // Replicator class
{$I JwaLmServer.pas}   // Server class
{$I JwaLmSvc.pas}      // Service class
{$I JwaLmUse.pas}      // Use class
{$I JwaLmWkSta.pas}    // Workstation class
{$I JwaLmApiBuf.pas}   // NetApiBuffer class
{$I JwaLmErrLog.pas}   // NetErrorLog class
{$I JwaLmConfig.pas}   // NetConfig class
{$I JwaLmStats.pas}    // NetStats class
{$I JwaLmAudit.pas}    // NetAudit class
{$I JwaLmJoin.pas}     // NetJoinDomain class

{$UNDEF JWA_OMIT_SECTIONS_LM}
{$UNDEF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}















