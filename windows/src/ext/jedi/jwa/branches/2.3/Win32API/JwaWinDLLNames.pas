{******************************************************************************}
{                                                                              }
{  API helper Unit for Object Pascal                                           }
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
{$IFNDEF JWA_OMIT_SECTIONS}
// $Id: JwaWinDLLNames.pas,v 1.2 2007/09/05 11:58:53 dezipaitor Exp $
unit JwaWinDLLNames;

{$WEAKPACKAGEUNIT}

{$I ..\Includes\JediAPILib.inc}

interface
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}
const
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}

  {The list is sorted alphabetically!}
  aclapilib = 'advapi32.dll';
  acluilib = 'aclui.dll';
  advapi32 = 'advapi32.dll';
  authzlib = 'authz.dll';
  adslib = 'activeds.dll';
  bcryptdll = 'bcrypt.dll';
  btapi = 'irprops.cpl';
  cfgmgrdllname = 'cfgmgr32.dll';
  comctl32 = 'comctl32.dll';
  credapi = 'advapi32.dll';
  credui = 'credui.dll';
  crypt32 = 'crypt32.dll';
  cryptnet = 'cryptnet.dll';
  cryptuiapi = 'cryptui.dll';
  dhcpapi = 'dhcpcsvc.dll';
  dhcplib = 'dhcpsapi.dll';
  dnsapi = 'dnsapi.dll';
  dsprop = 'dsprop.dll';
  dssec = 'dssec.dll';
  dsuiext = 'dsuiext.dll';
  dwmlib = 'dwmapi.dll';
  faultreplib = 'faultrep.dll';
  gdi32 = 'gdi32.dll';
  gpeditlib = 'gpedit.dll';
  hhctrl = 'hhctrl.ocx';
  icmplib = 'icmp.dll';
  imagehlplib = 'imagehlp.dll';
  imelib = 'user32.dll';
  iphlpapilib = 'iphlpapi.dll';
  kernel32 = 'kernel32.dll';
  kernel32dll = kernel32;
  ktmw32 = 'ktmw32.dll';
  ldaplib = 'wldap32.dll';
  loadperflib = 'loadperf.dll';
  lpmlib = 'msidlpm.dll';
  mapi32 = 'mapi32.dll';
  mprlib = 'mpr.dll';
  msgina = 'msgina.dll';
  msilib = 'msi.dll';
  msimg32 = 'msimg32.dll';
  mswsocklib = 'mswsock.dll';
  mydocs = 'mydocs.dll';
  ncryptdll = 'ncrypt.dll';
  netapi32 = 'netapi32.dll';
  netsh = 'netsh.exe';
  nsplib = 'wsock32.dll';
  ntdll = 'ntdll.dll';
  ntdsapilib = 'ntdsapi.dll';
  ntdsbclilib = 'ntdsbclilib.dll';
  opengl32 = 'opengl32.dll';
  patchapi = 'mspatcha.dll';
  patchwiz = 'patchwiz.dll';
  pdhLib = 'pdh.dll';
  powrproflib = 'powrprof.dll';
  psapiLib = 'psapi.dll';
  querylib = 'query.dll';
  qosname = 'qosname.dll';
  rpclib = 'rpcrt4.dll';
  rpcns4 = 'rpcns4.dll';
  secur32 = 'secur32.dll';
  sensapilib = 'sensapi.dll';
  setupapimodulename = 'SetupApi.dll';
  sfclib = 'sfc.dll';
  sisbkuplib = 'sisbkup.dll';
  shdocvwDll = 'shdocvw.dll';
  shell32 = 'shell32.dll';
  shfolderdll = 'shfolder.dll';
  shlwapidll  = 'shlwapi.dll';
  snmpapilib = 'snmpapi.dll';
  softpub = 'softpub.dll';
  sporderlib = 'sporder.dll';
  srclient = 'srclient.dll';
  themelib = 'uxtheme.dll';
  trafficlib = 'traffic.dll';
  urlmondll = 'urlmon.dll';
  user32 = 'user32.dll';
  userenvlib = 'userenv.dll';
  utildll = 'utildll.dll';
  versionlib = 'version.dll';
  winberapi = 'wldap32.dll';
  winfax = 'winfax.dll';
  wimgapi = 'wimgapi.dll';
  winetdll = 'wininet.dll';
  winspool32 = 'winspool32.drv';
  winstadll = 'winsta.dll';
  winternl_lib = 'ntdll.dll';
  wow16lib = 'kernel32.dll';
  wow32lib = 'wow32.dll';
  wpapilib = 'wpapi.dll';
  ws2_32 = 'ws2_32.dll';
  wsock32 = 'wsock32.dll';
  wtsapi = 'wtsapi32.dll';
  {The list is sorted alphabetically!}
  


  
  

{$ENDIF JWA_IMPLEMENTATIONSECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}
//your implementation here
{$ENDIF JWA_INTERFACESECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}




