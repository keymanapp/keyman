{******************************************************************************}
{                                                                              }
{ Windows Networking API interface Unit for Object Pascal                      }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: winnetwk.h, released June 2000. The original Pascal    }
{ code is: WinNetWk.pas, released December 2000. The initial developer of the  }
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

// $Id: JwaWinNetWk.pas,v 1.13 2007/09/05 11:58:54 dezipaitor Exp $

{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaWinNetWk;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "WinNetWk.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}

{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaWinError, JwaWinType;
{$ENDIF JWA_OMIT_SECTIONS}


{$IFNDEF JWA_IMPLEMENTATIONSECTION}
//
// Network types
//

const
  WNNC_NET_MSNET       = $00010000;
  {$EXTERNALSYM WNNC_NET_MSNET}
  WNNC_NET_LANMAN      = $00020000;
  {$EXTERNALSYM WNNC_NET_LANMAN}
  WNNC_NET_NETWARE     = $00030000;
  {$EXTERNALSYM WNNC_NET_NETWARE}
  WNNC_NET_VINES       = $00040000;
  {$EXTERNALSYM WNNC_NET_VINES}
  WNNC_NET_10NET       = $00050000;
  {$EXTERNALSYM WNNC_NET_10NET}
  WNNC_NET_LOCUS       = $00060000;
  {$EXTERNALSYM WNNC_NET_LOCUS}
  WNNC_NET_SUN_PC_NFS  = $00070000;
  {$EXTERNALSYM WNNC_NET_SUN_PC_NFS}
  WNNC_NET_LANSTEP     = $00080000;
  {$EXTERNALSYM WNNC_NET_LANSTEP}
  WNNC_NET_9TILES      = $00090000;
  {$EXTERNALSYM WNNC_NET_9TILES}
  WNNC_NET_LANTASTIC   = $000A0000;
  {$EXTERNALSYM WNNC_NET_LANTASTIC}
  WNNC_NET_AS400       = $000B0000;
  {$EXTERNALSYM WNNC_NET_AS400}
  WNNC_NET_FTP_NFS     = $000C0000;
  {$EXTERNALSYM WNNC_NET_FTP_NFS}
  WNNC_NET_PATHWORKS   = $000D0000;
  {$EXTERNALSYM WNNC_NET_PATHWORKS}
  WNNC_NET_LIFENET     = $000E0000;
  {$EXTERNALSYM WNNC_NET_LIFENET}
  WNNC_NET_POWERLAN    = $000F0000;
  {$EXTERNALSYM WNNC_NET_POWERLAN}
  WNNC_NET_BWNFS       = $00100000;
  {$EXTERNALSYM WNNC_NET_BWNFS}
  WNNC_NET_COGENT      = $00110000;
  {$EXTERNALSYM WNNC_NET_COGENT}
  WNNC_NET_FARALLON    = $00120000;
  {$EXTERNALSYM WNNC_NET_FARALLON}
  WNNC_NET_APPLETALK   = $00130000;
  {$EXTERNALSYM WNNC_NET_APPLETALK}
  WNNC_NET_INTERGRAPH  = $00140000;
  {$EXTERNALSYM WNNC_NET_INTERGRAPH}
  WNNC_NET_SYMFONET    = $00150000;
  {$EXTERNALSYM WNNC_NET_SYMFONET}
  WNNC_NET_CLEARCASE   = $00160000;
  {$EXTERNALSYM WNNC_NET_CLEARCASE}
  WNNC_NET_FRONTIER    = $00170000;
  {$EXTERNALSYM WNNC_NET_FRONTIER}
  WNNC_NET_BMC         = $00180000;
  {$EXTERNALSYM WNNC_NET_BMC}
  WNNC_NET_DCE         = $00190000;
  {$EXTERNALSYM WNNC_NET_DCE}
  WNNC_NET_AVID        = $001A0000;
  {$EXTERNALSYM WNNC_NET_AVID}
  WNNC_NET_DOCUSPACE   = $001B0000;
  {$EXTERNALSYM WNNC_NET_DOCUSPACE}
  WNNC_NET_MANGOSOFT   = $001C0000;
  {$EXTERNALSYM WNNC_NET_MANGOSOFT}
  WNNC_NET_SERNET      = $001D0000;
  {$EXTERNALSYM WNNC_NET_SERNET}
  WNNC_NET_RIVERFRONT1 = $001E0000;
  {$EXTERNALSYM WNNC_NET_RIVERFRONT1}
  WNNC_NET_RIVERFRONT2 = $001F0000;
  {$EXTERNALSYM WNNC_NET_RIVERFRONT2}
  WNNC_NET_DECORB      = $00200000;
  {$EXTERNALSYM WNNC_NET_DECORB}
  WNNC_NET_PROTSTOR    = $00210000;
  {$EXTERNALSYM WNNC_NET_PROTSTOR}
  WNNC_NET_FJ_REDIR    = $00220000;
  {$EXTERNALSYM WNNC_NET_FJ_REDIR}
  WNNC_NET_DISTINCT    = $00230000;
  {$EXTERNALSYM WNNC_NET_DISTINCT}
  WNNC_NET_TWINS       = $00240000;
  {$EXTERNALSYM WNNC_NET_TWINS}
  WNNC_NET_RDR2SAMPLE  = $00250000;
  {$EXTERNALSYM WNNC_NET_RDR2SAMPLE}
  WNNC_NET_CSC         = $00260000;
  {$EXTERNALSYM WNNC_NET_CSC}
  WNNC_NET_3IN1        = $00270000;
  {$EXTERNALSYM WNNC_NET_3IN1}
  WNNC_NET_EXTENDNET   = $00290000;
  {$EXTERNALSYM WNNC_NET_EXTENDNET}
  WNNC_NET_STAC        = $002A0000;
  {$EXTERNALSYM WNNC_NET_STAC}
  WNNC_NET_FOXBAT      = $002B0000;
  {$EXTERNALSYM WNNC_NET_FOXBAT}
  WNNC_NET_YAHOO       = $002C0000;
  {$EXTERNALSYM WNNC_NET_YAHOO}
  WNNC_NET_EXIFS       = $002D0000;
  {$EXTERNALSYM WNNC_NET_EXIFS}
  WNNC_NET_DAV         = $002E0000;
  {$EXTERNALSYM WNNC_NET_DAV}
  WNNC_NET_KNOWARE     = $002F0000;
  {$EXTERNALSYM WNNC_NET_KNOWARE}
  WNNC_NET_OBJECT_DIRE = $00300000;
  {$EXTERNALSYM WNNC_NET_OBJECT_DIRE}
  WNNC_NET_MASFAX      = $00310000;
  {$EXTERNALSYM WNNC_NET_MASFAX}
  WNNC_NET_HOB_NFS     = $00320000;
  {$EXTERNALSYM WNNC_NET_HOB_NFS}
  WNNC_NET_SHIVA       = $00330000;
  {$EXTERNALSYM WNNC_NET_SHIVA}
  WNNC_NET_IBMAL       = $00340000;
  {$EXTERNALSYM WNNC_NET_IBMAL}
  WNNC_NET_LOCK        = $00350000;
  {$EXTERNALSYM WNNC_NET_LOCK}
  WNNC_NET_TERMSRV     = $00360000;
  {$EXTERNALSYM WNNC_NET_TERMSRV}
  WNNC_NET_SRT         = $00370000;
  {$EXTERNALSYM WNNC_NET_SRT}
  WNNC_NET_QUINCY      = $00380000;
  {$EXTERNALSYM WNNC_NET_QUINCY}

  WNNC_CRED_MANAGER = DWORD($FFFF0000);
  {$EXTERNALSYM WNNC_CRED_MANAGER}

//
//  Network Resources.
//

  RESOURCE_CONNECTED  = $00000001;
  {$EXTERNALSYM RESOURCE_CONNECTED}
  RESOURCE_GLOBALNET  = $00000002;
  {$EXTERNALSYM RESOURCE_GLOBALNET}
  RESOURCE_REMEMBERED = $00000003;
  {$EXTERNALSYM RESOURCE_REMEMBERED}
  RESOURCE_RECENT     = $00000004;
  {$EXTERNALSYM RESOURCE_RECENT}
  RESOURCE_CONTEXT    = $00000005;
  {$EXTERNALSYM RESOURCE_CONTEXT}

  RESOURCETYPE_ANY      = $00000000;
  {$EXTERNALSYM RESOURCETYPE_ANY}
  RESOURCETYPE_DISK     = $00000001;
  {$EXTERNALSYM RESOURCETYPE_DISK}
  RESOURCETYPE_PRINT    = $00000002;
  {$EXTERNALSYM RESOURCETYPE_PRINT}
  RESOURCETYPE_RESERVED = $00000008;
  {$EXTERNALSYM RESOURCETYPE_RESERVED}
  RESOURCETYPE_UNKNOWN  = DWORD($FFFFFFFF);
  {$EXTERNALSYM RESOURCETYPE_UNKNOWN}

  RESOURCEUSAGE_CONNECTABLE   = $00000001;
  {$EXTERNALSYM RESOURCEUSAGE_CONNECTABLE}
  RESOURCEUSAGE_CONTAINER     = $00000002;
  {$EXTERNALSYM RESOURCEUSAGE_CONTAINER}
  RESOURCEUSAGE_NOLOCALDEVICE = $00000004;
  {$EXTERNALSYM RESOURCEUSAGE_NOLOCALDEVICE}
  RESOURCEUSAGE_SIBLING       = $00000008;
  {$EXTERNALSYM RESOURCEUSAGE_SIBLING}
  RESOURCEUSAGE_ATTACHED      = $00000010;
  {$EXTERNALSYM RESOURCEUSAGE_ATTACHED}
  RESOURCEUSAGE_ALL           = (RESOURCEUSAGE_CONNECTABLE or
    RESOURCEUSAGE_CONTAINER or RESOURCEUSAGE_ATTACHED);
  {$EXTERNALSYM RESOURCEUSAGE_ALL}
  RESOURCEUSAGE_RESERVED      = DWORD($80000000);
  {$EXTERNALSYM RESOURCEUSAGE_RESERVED}

  RESOURCEDISPLAYTYPE_GENERIC      = $00000000;
  {$EXTERNALSYM RESOURCEDISPLAYTYPE_GENERIC}
  RESOURCEDISPLAYTYPE_DOMAIN       = $00000001;
  {$EXTERNALSYM RESOURCEDISPLAYTYPE_DOMAIN}
  RESOURCEDISPLAYTYPE_SERVER       = $00000002;
  {$EXTERNALSYM RESOURCEDISPLAYTYPE_SERVER}
  RESOURCEDISPLAYTYPE_SHARE        = $00000003;
  {$EXTERNALSYM RESOURCEDISPLAYTYPE_SHARE}
  RESOURCEDISPLAYTYPE_FILE         = $00000004;
  {$EXTERNALSYM RESOURCEDISPLAYTYPE_FILE}
  RESOURCEDISPLAYTYPE_GROUP        = $00000005;
  {$EXTERNALSYM RESOURCEDISPLAYTYPE_GROUP}
  RESOURCEDISPLAYTYPE_NETWORK      = $00000006;
  {$EXTERNALSYM RESOURCEDISPLAYTYPE_NETWORK}
  RESOURCEDISPLAYTYPE_ROOT         = $00000007;
  {$EXTERNALSYM RESOURCEDISPLAYTYPE_ROOT}
  RESOURCEDISPLAYTYPE_SHAREADMIN   = $00000008;
  {$EXTERNALSYM RESOURCEDISPLAYTYPE_SHAREADMIN}
  RESOURCEDISPLAYTYPE_DIRECTORY    = $00000009;
  {$EXTERNALSYM RESOURCEDISPLAYTYPE_DIRECTORY}
  RESOURCEDISPLAYTYPE_TREE         = $0000000A;
  {$EXTERNALSYM RESOURCEDISPLAYTYPE_TREE}
  RESOURCEDISPLAYTYPE_NDSCONTAINER = $0000000B;
  {$EXTERNALSYM RESOURCEDISPLAYTYPE_NDSCONTAINER}

type
  LPNETRESOURCEA = ^NETRESOURCEA;
  {$EXTERNALSYM LPNETRESOURCEA}
  _NETRESOURCEA = record
    dwScope: DWORD;
    dwType: DWORD;
    dwDisplayType: DWORD;
    dwUsage: DWORD;
    lpLocalName: LPSTR;
    lpRemoteName: LPSTR;
    lpComment: LPSTR;
    lpProvider: LPSTR;
  end;
  {$EXTERNALSYM _NETRESOURCEA}
  NETRESOURCEA = _NETRESOURCEA;
  {$EXTERNALSYM NETRESOURCEA}
  TNetResourceA = NETRESOURCEA;
  PNetResourceA = LPNETRESOURCEA;

  LPNETRESOURCEW = ^NETRESOURCEW;
  {$EXTERNALSYM LPNETRESOURCEW}
  _NETRESOURCEW = record
    dwScope: DWORD;
    dwType: DWORD;
    dwDisplayType: DWORD;
    dwUsage: DWORD;
    lpLocalName: LPWSTR;
    lpRemoteName: LPWSTR;
    lpComment: LPWSTR;
    lpProvider: LPWSTR;
  end;
  {$EXTERNALSYM _NETRESOURCEW}
  NETRESOURCEW = _NETRESOURCEW;
  {$EXTERNALSYM NETRESOURCEW}
  TNetResourceW = NETRESOURCEW;
  PNetResourceW = LPNETRESOURCEW;

  {$IFDEF UNICODE}
  NETRESOURCE = NETRESOURCEW;
  {$EXTERNALSYM NETRESOURCE}
  LPNETRESOURCE = LPNETRESOURCEW;
  {$EXTERNALSYM LPNETRESOURCE}
  TNetResource = TNetResourceW;
  PNetResource = PNetResourceW;
  {$ELSE}
  NETRESOURCE = NETRESOURCEA;
  {$EXTERNALSYM NETRESOURCE}
  LPNETRESOURCE = LPNETRESOURCEA;
  {$EXTERNALSYM LPNETRESOURCE}
  TNetResource = TNetResourceA;
  PNetResource = PNetResourceA;
  {$ENDIF UNICODE}

//
//  Network Connections.
//

const
  NETPROPERTY_PERSISTENT = 1;
  {$EXTERNALSYM NETPROPERTY_PERSISTENT}

  CONNECT_UPDATE_PROFILE = $00000001;
  {$EXTERNALSYM CONNECT_UPDATE_PROFILE}
  CONNECT_UPDATE_RECENT  = $00000002;
  {$EXTERNALSYM CONNECT_UPDATE_RECENT}
  CONNECT_TEMPORARY      = $00000004;
  {$EXTERNALSYM CONNECT_TEMPORARY}
  CONNECT_INTERACTIVE    = $00000008;
  {$EXTERNALSYM CONNECT_INTERACTIVE}
  CONNECT_PROMPT         = $00000010;
  {$EXTERNALSYM CONNECT_PROMPT}
  CONNECT_NEED_DRIVE     = $00000020;
  {$EXTERNALSYM CONNECT_NEED_DRIVE}
  CONNECT_REFCOUNT       = $00000040;
  {$EXTERNALSYM CONNECT_REFCOUNT}
  CONNECT_REDIRECT       = $00000080;
  {$EXTERNALSYM CONNECT_REDIRECT}
  CONNECT_LOCALDRIVE     = $00000100;
  {$EXTERNALSYM CONNECT_LOCALDRIVE}
  CONNECT_CURRENT_MEDIA  = $00000200;
  {$EXTERNALSYM CONNECT_CURRENT_MEDIA}
  CONNECT_DEFERRED       = $00000400;
  {$EXTERNALSYM CONNECT_DEFERRED}
  CONNECT_RESERVED       = DWORD($FF000000);
  {$EXTERNALSYM CONNECT_RESERVED}
  CONNECT_COMMANDLINE    = $00000800;
  {$EXTERNALSYM CONNECT_COMMANDLINE}
  CONNECT_CMD_SAVECRED   = $00001000;
  {$EXTERNALSYM CONNECT_CMD_SAVECRED}

function WNetAddConnectionA(lpRemoteName, lpPassword, lpLocalName: LPCSTR): DWORD; stdcall;
{$EXTERNALSYM WNetAddConnectionA}
function WNetAddConnectionW(lpRemoteName, lpPassword, lpLocalName: LPCWSTR): DWORD; stdcall;
{$EXTERNALSYM WNetAddConnectionW}
function WNetAddConnection(lpRemoteName, lpPassword, lpLocalName: LPCTSTR): DWORD; stdcall;
{$EXTERNALSYM WNetAddConnection}

function WNetAddConnection2A(const lpNetResource: NETRESOURCEA; lpPassword: LPCSTR;
  lpUserName: LPCSTR; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetAddConnection2A}
function WNetAddConnection2W(const lpNetResource: NETRESOURCEW; lpPassword: LPCWSTR;
  lpUserName: LPCWSTR; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetAddConnection2W}
function WNetAddConnection2(const lpNetResource: NETRESOURCE; lpPassword: LPCTSTR;
  lpUserName: LPCTSTR; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetAddConnection2}

function WNetAddConnection3A(hwndOwner: HWND; const lpNetResource: NETRESOURCEA;
  lpPassword: LPCSTR; lpUserName: LPCSTR; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetAddConnection3A}
function WNetAddConnection3W(hwndOwner: HWND; const lpNetResource: NETRESOURCEW;
  lpPassword: LPCWSTR; lpUserName: LPCWSTR; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetAddConnection3W}
function WNetAddConnection3(hwndOwner: HWND; const lpNetResource: LPNETRESOURCE;
  lpPassword: LPCTSTR; lpUserName: LPCTSTR; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetAddConnection3}

function WNetCancelConnectionA(lpName: LPCSTR; fForce: BOOL): DWORD; stdcall;
{$EXTERNALSYM WNetCancelConnectionA}
function WNetCancelConnectionW(lpName: LPCWSTR; fForce: BOOL): DWORD; stdcall;
{$EXTERNALSYM WNetCancelConnectionW}
function WNetCancelConnection(lpName: LPCTSTR; fForce: BOOL): DWORD; stdcall;
{$EXTERNALSYM WNetCancelConnection}

function WNetCancelConnection2A(lpName: LPCSTR; dwFlags: DWORD; fForce: BOOL): DWORD; stdcall;
{$EXTERNALSYM WNetCancelConnection2A}
function WNetCancelConnection2W(lpName: LPCWSTR; dwFlags: DWORD; fForce: BOOL): DWORD; stdcall;
{$EXTERNALSYM WNetCancelConnection2W}
function WNetCancelConnection2(lpName: LPCTSTR; dwFlags: DWORD; fForce: BOOL): DWORD; stdcall;
{$EXTERNALSYM WNetCancelConnection2}

function WNetGetConnectionA(lpLocalName, lpRemoteName: LPSTR; var lpnLength: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetConnectionA}
function WNetGetConnectionW(lpLocalName, lpRemoteName: LPWSTR; var lpnLength: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetConnectionW}
function WNetGetConnection(lpLocalName, lpRemoteName: LPTSTR; var lpnLength: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetConnection}

{WNetRestoreConnectionA is no more available
See bug tracker
http://sourceforge.net/tracker/index.php?func=detail&aid=1846980&group_id=121894&atid=694029
}
//function WNetRestoreConnectionA(hwndParent: HWND; lpDevice: LPCSTR): DWORD; stdcall;
{.$EXTERNALSYM WNetRestoreConnectionA}

{$IFNDEF WINVISTA_UP}
//These functions are no longer available in Windows Vista and newer
                    
function WNetRestoreConnectionW(hwndParent: HWND; lpDevice: LPCWSTR): DWORD; stdcall;
{$EXTERNALSYM WNetRestoreConnectionW}
function WNetRestoreConnection(hwndParent: HWND; lpDevice: LPCTSTR): DWORD; stdcall;
{$EXTERNALSYM WNetRestoreConnection}
{$ENDIF WINVISTA_UP}

function WNetUseConnectionA(hwndOwner: HWND; const lpNetResource: NETRESOURCEA;
  lpPassword, lpUserID: LPCSTR; dwFlags: DWORD; lpAccessName: LPSTR;
  var lpBufferSize, lpResult: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetUseConnectionA}
function WNetUseConnectionW(hwndOwner: HWND; const lpNetResource: NETRESOURCEW;
  lpPassword, lpUserID: LPCWSTR; dwFlags: DWORD; lpAccessName: LPWSTR;
  var lpBufferSize, lpResult: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetUseConnectionW}
function WNetUseConnection(hwndOwner: HWND; const lpNetResource: NETRESOURCE;
  lpPassword, lpUserID: LPCTSTR; dwFlags: DWORD; lpAccessName: LPTSTR;
  var lpBufferSize, lpResult: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetUseConnection}

//
//  Network Connection Dialogs.
//

function WNetConnectionDialog(hwnd: HWND; dwType: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetConnectionDialog}

function WNetDisconnectDialog(hwnd: HWND; dwType: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetDisconnectDialog}

type
  LPCONNECTDLGSTRUCTA = ^CONNECTDLGSTRUCTA;
  {$EXTERNALSYM LPCONNECTDLGSTRUCTA}
  _CONNECTDLGSTRUCTA = record
    cbStructure: DWORD; // size of this structure in bytes
    hwndOwner: HWND; // owner window for the dialog
    lpConnRes: LPNETRESOURCEA; // Requested Resource info
    dwFlags: DWORD; // flags (see below)
    dwDevNum: DWORD; // number of devices connected to
  end;
  {$EXTERNALSYM _CONNECTDLGSTRUCTA}
  CONNECTDLGSTRUCTA = _CONNECTDLGSTRUCTA;
  {$EXTERNALSYM CONNECTDLGSTRUCTA}
  TConnectDlgStructA = CONNECTDLGSTRUCTA;
  PConnectDlgStructA = LPCONNECTDLGSTRUCTA;

  LPCONNECTDLGSTRUCTW = ^CONNECTDLGSTRUCTW;
  {$EXTERNALSYM LPCONNECTDLGSTRUCTW}
  _CONNECTDLGSTRUCTW = record
    cbStructure: DWORD; // size of this structure in bytes
    hwndOwner: HWND; // owner window for the dialog
    lpConnRes: LPNETRESOURCEW; // Requested Resource info
    dwFlags: DWORD; // flags (see below)
    dwDevNum: DWORD; // number of devices connected to
  end;
  {$EXTERNALSYM _CONNECTDLGSTRUCTW}
  CONNECTDLGSTRUCTW = _CONNECTDLGSTRUCTW;
  {$EXTERNALSYM CONNECTDLGSTRUCTW}
  TConnectDlgStructW = CONNECTDLGSTRUCTW;
  PConnectDlgStructW = LPCONNECTDLGSTRUCTW;

  {$IFDEF UNICODE}
  CONNECTDLGSTRUCT = CONNECTDLGSTRUCTW;
  {$EXTERNALSYM CONNECTDLGSTRUCT}
  LPCONNECTDLGSTRUCT = LPCONNECTDLGSTRUCTW;
  {$EXTERNALSYM LPCONNECTDLGSTRUCT}
  TConnectDlgStruct = TConnectDlgStructW;
  PConnectDlgStruct = PConnectDlgStructW;
  {$ELSE}
  CONNECTDLGSTRUCT = CONNECTDLGSTRUCTA;
  {$EXTERNALSYM CONNECTDLGSTRUCT}
  LPCONNECTDLGSTRUCT = LPCONNECTDLGSTRUCTA;
  {$EXTERNALSYM LPCONNECTDLGSTRUCT}
  TConnectDlgStruct = TConnectDlgStructA;
  PConnectDlgStruct = PConnectDlgStructA;
  {$ENDIF UNICODE}

const
  CONNDLG_RO_PATH    = $00000001; // Resource path should be read-only
  {$EXTERNALSYM CONNDLG_RO_PATH}
  CONNDLG_CONN_POINT = $00000002; // Netware -style movable connection point enabled
  {$EXTERNALSYM CONNDLG_CONN_POINT}
  CONNDLG_USE_MRU    = $00000004; // Use MRU combobox
  {$EXTERNALSYM CONNDLG_USE_MRU}
  CONNDLG_HIDE_BOX   = $00000008; // Hide persistent connect checkbox
  {$EXTERNALSYM CONNDLG_HIDE_BOX}

//
// NOTE:  Set at most ONE of the below flags.  If neither flag is set,
//        then the persistence is set to whatever the user chose during
//        a previous connection
//

  CONNDLG_PERSIST     = $00000010; // Force persistent connection
  {$EXTERNALSYM CONNDLG_PERSIST}
  CONNDLG_NOT_PERSIST = $00000020; // Force connection NOT persistent
  {$EXTERNALSYM CONNDLG_NOT_PERSIST}

function WNetConnectionDialog1A(var lpConnDlgStruct: CONNECTDLGSTRUCTA): DWORD; stdcall;
{$EXTERNALSYM WNetConnectionDialog1A}
function WNetConnectionDialog1W(var lpConnDlgStruct: CONNECTDLGSTRUCTW): DWORD; stdcall;
{$EXTERNALSYM WNetConnectionDialog1W}
function WNetConnectionDialog1(var lpConnDlgStruct: CONNECTDLGSTRUCT): DWORD; stdcall;
{$EXTERNALSYM WNetConnectionDialog1}

type
  LPDISCDLGSTRUCTA = ^DISCDLGSTRUCTA;
  {$EXTERNALSYM LPDISCDLGSTRUCTA}
  _DISCDLGSTRUCTA = record
    cbStructure: DWORD; // size of this structure in bytes
    hwndOwner: HWND; // owner window for the dialog
    lpLocalName: LPSTR; // local device name
    lpRemoteName: LPSTR; // network resource name
    dwFlags: DWORD; // flags
  end;
  {$EXTERNALSYM _DISCDLGSTRUCTA}
  DISCDLGSTRUCTA = _DISCDLGSTRUCTA;
  {$EXTERNALSYM DISCDLGSTRUCTA}
  TDiscDlgStructA = DISCDLGSTRUCTA;
  PDiscDlgStructA = LPDISCDLGSTRUCTA;

  LPDISCDLGSTRUCTW = ^DISCDLGSTRUCTW;
  {$EXTERNALSYM LPDISCDLGSTRUCTW}
  _DISCDLGSTRUCTW = record
    cbStructure: DWORD; // size of this structure in bytes
    hwndOwner: HWND; // owner window for the dialog
    lpLocalName: LPWSTR; // local device name
    lpRemoteName: LPWSTR; // network resource name
    dwFlags: DWORD; // flags
  end;
  {$EXTERNALSYM _DISCDLGSTRUCTW}
  DISCDLGSTRUCTW = _DISCDLGSTRUCTW;
  {$EXTERNALSYM DISCDLGSTRUCTW}
  TDiscDlgStructW = DISCDLGSTRUCTW;
  PDiscDlgStructW = LPDISCDLGSTRUCTW;

  {$IFDEF UNICODE}
  DISCDLGSTRUCT = DISCDLGSTRUCTW;
  {$EXTERNALSYM DISCDLGSTRUCT}
  LPDISCDLGSTRUCT = LPDISCDLGSTRUCTW;
  {$EXTERNALSYM LPDISCDLGSTRUCT}
  TDiscDlgStruct = TDiscDlgStructW;
  PDiscDlgStruct = PDiscDlgStructW;
  {$ELSE}
  DISCDLGSTRUCT = DISCDLGSTRUCTA;
  {$EXTERNALSYM DISCDLGSTRUCT}
  LPDISCDLGSTRUCT = LPDISCDLGSTRUCTA;
  {$EXTERNALSYM LPDISCDLGSTRUCT}
  TDiscDlgStruct = TDiscDlgStructA;
  PDiscDlgStruct = PDiscDlgStructA;
  {$ENDIF UNICODE}

const
  DISC_UPDATE_PROFILE = $00000001;
  {$EXTERNALSYM DISC_UPDATE_PROFILE}
  DISC_NO_FORCE       = $00000040;
  {$EXTERNALSYM DISC_NO_FORCE}

function WNetDisconnectDialog1A(const lpConnDlgStruct: DISCDLGSTRUCTA): DWORD; stdcall;
{$EXTERNALSYM WNetDisconnectDialog1A}
function WNetDisconnectDialog1W(const lpConnDlgStruct: DISCDLGSTRUCTW): DWORD; stdcall;
{$EXTERNALSYM WNetDisconnectDialog1W}
function WNetDisconnectDialog1(const lpConnDlgStruct: DISCDLGSTRUCT): DWORD; stdcall;
{$EXTERNALSYM WNetDisconnectDialog1}

//
//  Network Browsing.
//

function WNetOpenEnumA(dwScope, dwType, dwUsage: DWORD; lpNetResource: LPNETRESOURCEA;
  var lphEnum: HANDLE): DWORD; stdcall;
{$EXTERNALSYM WNetOpenEnumA}
function WNetOpenEnumW(dwScope, dwType, dwUsage: DWORD; lpNetResource: LPNETRESOURCEW;
  var lphEnum: HANDLE): DWORD; stdcall;
{$EXTERNALSYM WNetOpenEnumW}
function WNetOpenEnum(dwScope, dwType, dwUsage: DWORD; lpNetResource: LPNETRESOURCE;
  var lphEnum: HANDLE): DWORD; stdcall;
{$EXTERNALSYM WNetOpenEnum}

function WNetEnumResourceA(hEnum: HANDLE; var lpcCount: DWORD; lpBuffer: LPVOID;
  var lpBufferSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetEnumResourceA}
function WNetEnumResourceW(hEnum: HANDLE; var lpcCount: DWORD; lpBuffer: LPVOID;
  var lpBufferSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetEnumResourceW}
function WNetEnumResource(hEnum: HANDLE; var lpcCount: DWORD; lpBuffer: LPVOID;
  var lpBufferSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetEnumResource}

function WNetCloseEnum(hEnum: HANDLE): DWORD; stdcall;
{$EXTERNALSYM WNetCloseEnum}

function WNetGetResourceParentA(const lpNetResource: NETRESOURCEA;
  lpBuffer: LPVOID; var lpcbBuffer: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetResourceParentA}
function WNetGetResourceParentW(const lpNetResource: NETRESOURCEW;
  lpBuffer: LPVOID; var lpcbBuffer: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetResourceParentW}
function WNetGetResourceParent(const lpNetResource: NETRESOURCE;
  lpBuffer: LPVOID; var lpcbBuffer: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetResourceParent}

function WNetGetResourceInformationA(const lpNetResource: NETRESOURCEA;
  lpBuffer: LPVOID; var lpcbBuffer: DWORD; var lplpSystem: LPSTR): DWORD; stdcall;
{$EXTERNALSYM WNetGetResourceInformationA}
function WNetGetResourceInformationW(const lpNetResource: NETRESOURCEW;
  lpBuffer: LPVOID; var lpcbBuffer: DWORD; var lplpSystem: LPWSTR): DWORD; stdcall;
{$EXTERNALSYM WNetGetResourceInformationW}
function WNetGetResourceInformation(const lpNetResource: NETRESOURCE;
  lpBuffer: LPVOID; var lpcbBuffer: DWORD; var lplpSystem: LPTSTR): DWORD; stdcall;
{$EXTERNALSYM WNetGetResourceInformation}

//
//  Universal Naming.
//

const
  UNIVERSAL_NAME_INFO_LEVEL = $00000001;
  {$EXTERNALSYM UNIVERSAL_NAME_INFO_LEVEL}
  REMOTE_NAME_INFO_LEVEL    = $00000002;
  {$EXTERNALSYM REMOTE_NAME_INFO_LEVEL}

type
  LPUNIVERSAL_NAME_INFOA = ^UNIVERSAL_NAME_INFOA;
  {$EXTERNALSYM LPUNIVERSAL_NAME_INFOA}
  _UNIVERSAL_NAME_INFOA = record
    lpUniversalName: LPSTR;
  end;
  {$EXTERNALSYM _UNIVERSAL_NAME_INFOA}
  UNIVERSAL_NAME_INFOA = _UNIVERSAL_NAME_INFOA;
  {$EXTERNALSYM UNIVERSAL_NAME_INFOA}
  TUniversalNameInfoA = UNIVERSAL_NAME_INFOA;
  PUniversalNameInfoA = LPUNIVERSAL_NAME_INFOA;

  LPUNIVERSAL_NAME_INFOW = ^UNIVERSAL_NAME_INFOW;
  {$EXTERNALSYM LPUNIVERSAL_NAME_INFOW}
  _UNIVERSAL_NAME_INFOW = record
    lpUniversalName: LPWSTR;
  end;
  {$EXTERNALSYM _UNIVERSAL_NAME_INFOW}
  UNIVERSAL_NAME_INFOW = _UNIVERSAL_NAME_INFOW;
  {$EXTERNALSYM UNIVERSAL_NAME_INFOW}
  TUniversalNameInfoW = UNIVERSAL_NAME_INFOW;
  PUniversalNameInfoW = LPUNIVERSAL_NAME_INFOW;

  {$IFDEF UNICODE}
  UNIVERSAL_NAME_INFO = UNIVERSAL_NAME_INFOW;
  {$EXTERNALSYM UNIVERSAL_NAME_INFO}
  LPUNIVERSAL_NAME_INFO = LPUNIVERSAL_NAME_INFOW;
  {$EXTERNALSYM LPUNIVERSAL_NAME_INFO}
  TUniversalNameInfo = TUniversalNameInfoW;
  PUniversalNameInfo = PUniversalNameInfoW;
  {$ELSE}
  UNIVERSAL_NAME_INFO = UNIVERSAL_NAME_INFOA;
  {$EXTERNALSYM UNIVERSAL_NAME_INFO}
  LPUNIVERSAL_NAME_INFO = LPUNIVERSAL_NAME_INFOA;
  {$EXTERNALSYM LPUNIVERSAL_NAME_INFO}
  TUniversalNameInfo = TUniversalNameInfoA;
  PUniversalNameInfo = PUniversalNameInfoA;
  {$ENDIF UNICODE}

  LPREMOTE_NAME_INFOA = ^REMOTE_NAME_INFOA;
  {$EXTERNALSYM LPREMOTE_NAME_INFOA}
  _REMOTE_NAME_INFOA = record
    lpUniversalName: LPSTR;
    lpConnectionName: LPSTR;
    lpRemainingPath: LPSTR;
  end;
  {$EXTERNALSYM _REMOTE_NAME_INFOA}
  REMOTE_NAME_INFOA = _REMOTE_NAME_INFOA;
  {$EXTERNALSYM REMOTE_NAME_INFOA}
  TRemoteNameInfoA = REMOTE_NAME_INFOA;
  PRemoteNameInfoA = LPREMOTE_NAME_INFOA;

  LPREMOTE_NAME_INFOW = ^REMOTE_NAME_INFOW;
  {$EXTERNALSYM LPREMOTE_NAME_INFOW}
  _REMOTE_NAME_INFOW = record
    lpUniversalName: LPWSTR;
    lpConnectionName: LPWSTR;
    lpRemainingPath: LPWSTR;
  end;
  {$EXTERNALSYM _REMOTE_NAME_INFOW}
  REMOTE_NAME_INFOW = _REMOTE_NAME_INFOW;
  {$EXTERNALSYM REMOTE_NAME_INFOW}
  TRemoteNameInfoW = REMOTE_NAME_INFOW;
  PRemoteNameInfoW = LPREMOTE_NAME_INFOW;

  {$IFDEF UNICODE}
  REMOTE_NAME_INFO = REMOTE_NAME_INFOW;
  {$EXTERNALSYM REMOTE_NAME_INFO}
  LPREMOTE_NAME_INFO = LPREMOTE_NAME_INFOW;
  {$EXTERNALSYM LPREMOTE_NAME_INFO}
  TRemoteNameInfo = TRemoteNameInfoW;
  PRemoteNameInfo = PRemoteNameInfoW;
  {$ELSE}
  REMOTE_NAME_INFO = REMOTE_NAME_INFOA;
  {$EXTERNALSYM REMOTE_NAME_INFO}
  LPREMOTE_NAME_INFO = LPREMOTE_NAME_INFOA;
  {$EXTERNALSYM LPREMOTE_NAME_INFO}
  TRemoteNameInfo = TRemoteNameInfoA;
  PRemoteNameInfo = PRemoteNameInfoA;
  {$ENDIF UNICODE}

function WNetGetUniversalNameA(lpLocalPath: LPCSTR; dwInfoLevel: DWORD;
  lpBuffer: LPVOID; var lpBufferSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetUniversalNameA}
function WNetGetUniversalNameW(lpLocalPath: LPCWSTR; dwInfoLevel: DWORD;
  lpBuffer: LPVOID; var lpBufferSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetUniversalNameW}
function WNetGetUniversalName(lpLocalPath: LPCTSTR; dwInfoLevel: DWORD;
  lpBuffer: LPVOID; var lpBufferSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetUniversalName}

//
//  Authentication and Logon/Logoff.
//

function WNetGetUserA(lpName: LPCSTR; lpUserName: LPSTR; var lpnLength: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetUserA}
function WNetGetUserW(lpName: LPCWSTR; lpUserName: LPWSTR; var lpnLength: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetUserW}
function WNetGetUser(lpName: LPCTSTR; lpUserName: LPTSTR; var lpnLength: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetUser}

//
// Other.
//

const
  WNFMT_MULTILINE   = $01;
  {$EXTERNALSYM WNFMT_MULTILINE}
  WNFMT_ABBREVIATED = $02;
  {$EXTERNALSYM WNFMT_ABBREVIATED}
  WNFMT_INENUM      = $10;
  {$EXTERNALSYM WNFMT_INENUM}
  WNFMT_CONNECTION  = $20;
  {$EXTERNALSYM WNFMT_CONNECTION}

function WNetGetProviderNameA(dwNetType: DWORD; lpProviderName: LPSTR;
  var lpBufferSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetProviderNameA}
function WNetGetProviderNameW(dwNetType: DWORD; lpProviderName: LPWSTR;
  var lpBufferSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetProviderNameW}
function WNetGetProviderName(dwNetType: DWORD; lpProviderName: LPTSTR;
  var lpBufferSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetProviderName}

type
  LPNETINFOSTRUCT = ^NETINFOSTRUCT;
  {$EXTERNALSYM LPNETINFOSTRUCT}
  _NETINFOSTRUCT = record
    cbStructure: DWORD;
    dwProviderVersion: DWORD;
    dwStatus: DWORD;
    dwCharacteristics: DWORD;
    dwHandle: ULONG_PTR;
    wNetType: WORD;
    dwPrinters: DWORD;
    dwDrives: DWORD;
  end;
  {$EXTERNALSYM _NETINFOSTRUCT}
  NETINFOSTRUCT = _NETINFOSTRUCT;
  {$EXTERNALSYM NETINFOSTRUCT}
  TNetInfoStruct = NETINFOSTRUCT;
  PNetInfoStruct = LPNETINFOSTRUCT;

const
  NETINFO_DLL16      = $00000001; // Provider running as 16 bit Winnet Driver
  {$EXTERNALSYM NETINFO_DLL16}
  NETINFO_DISKRED    = $00000004; // Provider requires disk redirections to connect
  {$EXTERNALSYM NETINFO_DISKRED}
  NETINFO_PRINTERRED = $00000008; // Provider requires printer redirections to connect
  {$EXTERNALSYM NETINFO_PRINTERRED}

function WNetGetNetworkInformationA(lpProvider: LPCSTR;
  var lpNetInfoStruct: NETINFOSTRUCT): DWORD; stdcall;
{$EXTERNALSYM WNetGetNetworkInformationA}
function WNetGetNetworkInformationW(lpProvider: LPCWSTR;
  var lpNetInfoStruct: NETINFOSTRUCT): DWORD; stdcall;
{$EXTERNALSYM WNetGetNetworkInformationW}
function WNetGetNetworkInformation(lpProvider: LPCTSTR;
  var lpNetInfoStruct: NETINFOSTRUCT): DWORD; stdcall;
{$EXTERNALSYM WNetGetNetworkInformation}

//
//  User Profiles.
//

type
  PFNGETPROFILEPATHA = function(pszUsername: LPCSTR; pszBuffer: LPSTR;
    cbBuffer: UINT): UINT; stdcall;
  {$EXTERNALSYM PFNGETPROFILEPATHA}

  PFNGETPROFILEPATHW = function(pszUsername: LPCWSTR; pszBuffer: LPWSTR;
    cbBuffer: UINT): UINT; stdcall;
  {$EXTERNALSYM PFNGETPROFILEPATHW}

  {$IFDEF UNICODE}
  PFNGETPROFILEPATH = PFNGETPROFILEPATHW;
  {$EXTERNALSYM PFNGETPROFILEPATH}
  {$ELSE}
  PFNGETPROFILEPATH = PFNGETPROFILEPATHA;
  {$EXTERNALSYM PFNGETPROFILEPATH}
  {$ENDIF UNICODE}

  PFNRECONCILEPROFILEA = function(pszCentralFile, pszLocalFile: LPCSTR;
    dwFlags: DWORD): UINT; stdcall;
  {$EXTERNALSYM PFNRECONCILEPROFILEA}
  PFNRECONCILEPROFILEW = function(pszCentralFile, pszLocalFile: LPCWSTR;
    dwFlags: DWORD): UINT; stdcall;
  {$EXTERNALSYM PFNRECONCILEPROFILEW}
  PFNRECONCILEPROFILE = function(pszCentralFile, pszLocalFile: LPCTSTR;
    dwFlags: DWORD): UINT; stdcall;
  {$EXTERNALSYM PFNRECONCILEPROFILE}

const
  RP_LOGON   = $01; // if set, do for logon, else for logoff
  {$EXTERNALSYM RP_LOGON}
  RP_INIFILE = $02; // if set, reconcile .INI file, else reg. hive
  {$EXTERNALSYM RP_INIFILE}

//
//  Policies.
//

type
  PFNPROCESSPOLICIESA = function(hwnd: HWND; pszPath, pszUsername,
    pszComputerName: LPCSTR; dwFlags: DWORD): BOOL; stdcall;
  {$EXTERNALSYM PFNPROCESSPOLICIESA}
  PFNPROCESSPOLICIESW = function(hwnd: HWND; pszPath, pszUsername,
    pszComputerName: LPCWSTR; dwFlags: DWORD): BOOL; stdcall;
  {$EXTERNALSYM PFNPROCESSPOLICIESW}
  PFNPROCESSPOLICIES = function(hwnd: HWND; pszPath, pszUsername,
    pszComputerName: LPCTSTR; dwFlags: DWORD): BOOL; stdcall;
  {$EXTERNALSYM PFNPROCESSPOLICIES}

const
  PP_DISPLAYERRORS = $01; // if set, display error messages, else fail silently if error
  {$EXTERNALSYM PP_DISPLAYERRORS}

//
//  Error handling.
//

function WNetGetLastErrorA(var lpError: DWORD; lpErrorBuf: LPSTR;
  nErrorBufSize: DWORD; lpNameBuf: LPSTR; nNameBufSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetLastErrorA}
function WNetGetLastErrorW(var lpError: DWORD; lpErrorBuf: LPWSTR;
  nErrorBufSize: DWORD; lpNameBuf: LPWSTR; nNameBufSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetLastErrorW}
function WNetGetLastError(var lpError: DWORD; lpErrorBuf: LPTSTR;
  nErrorBufSize: DWORD; lpNameBuf: LPTSTR; nNameBufSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM WNetGetLastError}

//
//  STATUS CODES
//

// General

const
  WN_SUCCESS          = NO_ERROR;
  {$EXTERNALSYM WN_SUCCESS}
  WN_NO_ERROR         = NO_ERROR;
  {$EXTERNALSYM WN_NO_ERROR}
  WN_NOT_SUPPORTED    = ERROR_NOT_SUPPORTED;
  {$EXTERNALSYM WN_NOT_SUPPORTED}
  WN_CANCEL           = ERROR_CANCELLED;
  {$EXTERNALSYM WN_CANCEL}
  WN_RETRY            = ERROR_RETRY;
  {$EXTERNALSYM WN_RETRY}
  WN_NET_ERROR        = ERROR_UNEXP_NET_ERR;
  {$EXTERNALSYM WN_NET_ERROR}
  WN_MORE_DATA        = ERROR_MORE_DATA;
  {$EXTERNALSYM WN_MORE_DATA}
  WN_BAD_POINTER      = ERROR_INVALID_ADDRESS;
  {$EXTERNALSYM WN_BAD_POINTER}
  WN_BAD_VALUE        = ERROR_INVALID_PARAMETER;
  {$EXTERNALSYM WN_BAD_VALUE}
  WN_BAD_USER         = ERROR_BAD_USERNAME;
  {$EXTERNALSYM WN_BAD_USER}
  WN_BAD_PASSWORD     = ERROR_INVALID_PASSWORD;
  {$EXTERNALSYM WN_BAD_PASSWORD}
  WN_ACCESS_DENIED    = ERROR_ACCESS_DENIED;
  {$EXTERNALSYM WN_ACCESS_DENIED}
  WN_FUNCTION_BUSY    = ERROR_BUSY;
  {$EXTERNALSYM WN_FUNCTION_BUSY}
  WN_WINDOWS_ERROR    = ERROR_UNEXP_NET_ERR;
  {$EXTERNALSYM WN_WINDOWS_ERROR}
  WN_OUT_OF_MEMORY    = ERROR_NOT_ENOUGH_MEMORY;
  {$EXTERNALSYM WN_OUT_OF_MEMORY}
  WN_NO_NETWORK       = ERROR_NO_NETWORK;
  {$EXTERNALSYM WN_NO_NETWORK}
  WN_EXTENDED_ERROR   = ERROR_EXTENDED_ERROR;
  {$EXTERNALSYM WN_EXTENDED_ERROR}
  WN_BAD_LEVEL        = ERROR_INVALID_LEVEL;
  {$EXTERNALSYM WN_BAD_LEVEL}
  WN_BAD_HANDLE       = ERROR_INVALID_HANDLE;
  {$EXTERNALSYM WN_BAD_HANDLE}
  WN_NOT_INITIALIZING = ERROR_ALREADY_INITIALIZED;
  {$EXTERNALSYM WN_NOT_INITIALIZING}
  WN_NO_MORE_DEVICES  = ERROR_NO_MORE_DEVICES;
  {$EXTERNALSYM WN_NO_MORE_DEVICES}

// Connection

  WN_NOT_CONNECTED             = ERROR_NOT_CONNECTED;
  {$EXTERNALSYM WN_NOT_CONNECTED}
  WN_OPEN_FILES                = ERROR_OPEN_FILES;
  {$EXTERNALSYM WN_OPEN_FILES}
  WN_DEVICE_IN_USE             = ERROR_DEVICE_IN_USE;
  {$EXTERNALSYM WN_DEVICE_IN_USE}
  WN_BAD_NETNAME               = ERROR_BAD_NET_NAME;
  {$EXTERNALSYM WN_BAD_NETNAME}
  WN_BAD_LOCALNAME             = ERROR_BAD_DEVICE;
  {$EXTERNALSYM WN_BAD_LOCALNAME}
  WN_ALREADY_CONNECTED         = ERROR_ALREADY_ASSIGNED;
  {$EXTERNALSYM WN_ALREADY_CONNECTED}
  WN_DEVICE_ERROR              = ERROR_GEN_FAILURE;
  {$EXTERNALSYM WN_DEVICE_ERROR}
  WN_CONNECTION_CLOSED         = ERROR_CONNECTION_UNAVAIL;
  {$EXTERNALSYM WN_CONNECTION_CLOSED}
  WN_NO_NET_OR_BAD_PATH        = ERROR_NO_NET_OR_BAD_PATH;
  {$EXTERNALSYM WN_NO_NET_OR_BAD_PATH}
  WN_BAD_PROVIDER              = ERROR_BAD_PROVIDER;
  {$EXTERNALSYM WN_BAD_PROVIDER}
  WN_CANNOT_OPEN_PROFILE       = ERROR_CANNOT_OPEN_PROFILE;
  {$EXTERNALSYM WN_CANNOT_OPEN_PROFILE}
  WN_BAD_PROFILE               = ERROR_BAD_PROFILE;
  {$EXTERNALSYM WN_BAD_PROFILE}
  WN_BAD_DEV_TYPE              = ERROR_BAD_DEV_TYPE;
  {$EXTERNALSYM WN_BAD_DEV_TYPE}
  WN_DEVICE_ALREADY_REMEMBERED = ERROR_DEVICE_ALREADY_REMEMBERED;
  {$EXTERNALSYM WN_DEVICE_ALREADY_REMEMBERED}
  WN_CONNECTED_OTHER_PASSWORD  = ERROR_CONNECTED_OTHER_PASSWORD;
  {$EXTERNALSYM WN_CONNECTED_OTHER_PASSWORD}
  WN_CONNECTED_OTHER_PASSWORD_DEFAULT = ERROR_CONNECTED_OTHER_PASSWORD_DEFAULT;
  {$EXTERNALSYM WN_CONNECTED_OTHER_PASSWORD_DEFAULT}

// Enumeration

  WN_NO_MORE_ENTRIES = ERROR_NO_MORE_ITEMS;
  {$EXTERNALSYM WN_NO_MORE_ENTRIES}
  WN_NOT_CONTAINER   = ERROR_NOT_CONTAINER;
  {$EXTERNALSYM WN_NOT_CONTAINER}

// Authentication

  WN_NOT_AUTHENTICATED = ERROR_NOT_AUTHENTICATED;
  {$EXTERNALSYM WN_NOT_AUTHENTICATED}
  WN_NOT_LOGGED_ON     = ERROR_NOT_LOGGED_ON;
  {$EXTERNALSYM WN_NOT_LOGGED_ON}
  WN_NOT_VALIDATED     = ERROR_NO_LOGON_SERVERS;
  {$EXTERNALSYM WN_NOT_VALIDATED}

//
//  For Shell
//

type
  LPNETCONNECTINFOSTRUCT = ^NETCONNECTINFOSTRUCT;
  {$EXTERNALSYM LPNETCONNECTINFOSTRUCT}
  _NETCONNECTINFOSTRUCT = record
    cbStructure: DWORD;
    dwFlags: DWORD;
    dwSpeed: DWORD;
    dwDelay: DWORD;
    dwOptDataSize: DWORD;
  end;
  {$EXTERNALSYM _NETCONNECTINFOSTRUCT}
  NETCONNECTINFOSTRUCT = _NETCONNECTINFOSTRUCT;
  {$EXTERNALSYM NETCONNECTINFOSTRUCT}
  TNetConnectInfoStruct = NETCONNECTINFOSTRUCT;
  PNetConnectInfoStruct = LPNETCONNECTINFOSTRUCT;

const
  WNCON_FORNETCARD = $00000001;
  {$EXTERNALSYM WNCON_FORNETCARD}
  WNCON_NOTROUTED  = $00000002;
  {$EXTERNALSYM WNCON_NOTROUTED}
  WNCON_SLOWLINK   = $00000004;
  {$EXTERNALSYM WNCON_SLOWLINK}
  WNCON_DYNAMIC    = $00000008;
  {$EXTERNALSYM WNCON_DYNAMIC}

function MultinetGetConnectionPerformanceA(const lpNetResource: NETRESOURCEA;
  var lpNetConnectInfoStruct: NETCONNECTINFOSTRUCT): DWORD; stdcall;
{$EXTERNALSYM MultinetGetConnectionPerformanceA}
function MultinetGetConnectionPerformanceW(const lpNetResource: NETRESOURCEW;
  var lpNetConnectInfoStruct: NETCONNECTINFOSTRUCT): DWORD; stdcall;
{$EXTERNALSYM MultinetGetConnectionPerformanceW}
function MultinetGetConnectionPerformance(const lpNetResource: NETRESOURCE;
  var lpNetConnectInfoStruct: NETCONNECTINFOSTRUCT): DWORD; stdcall;
{$EXTERNALSYM MultinetGetConnectionPerformance}

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  mpr = 'mpr.dll';
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}

var
  _WNetAddConnectionA: Pointer;

function WNetAddConnectionA;
begin
  GetProcedureAddress(_WNetAddConnectionA, mpr, 'WNetAddConnectionA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetAddConnectionA]
  end;
end;

var
  _WNetAddConnectionW: Pointer;

function WNetAddConnectionW;
begin
  GetProcedureAddress(_WNetAddConnectionW, mpr, 'WNetAddConnectionW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetAddConnectionW]
  end;
end;

var
  _WNetAddConnection: Pointer;

function WNetAddConnection;
begin
  GetProcedureAddress(_WNetAddConnection, mpr, 'WNetAddConnection' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetAddConnection]
  end;
end;

var
  _WNetAddConnection2A: Pointer;

function WNetAddConnection2A;
begin
  GetProcedureAddress(_WNetAddConnection2A, mpr, 'WNetAddConnection2A');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetAddConnection2A]
  end;
end;

var
  _WNetAddConnection2W: Pointer;

function WNetAddConnection2W;
begin
  GetProcedureAddress(_WNetAddConnection2W, mpr, 'WNetAddConnection2W');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetAddConnection2W]
  end;
end;

var
  _WNetAddConnection2: Pointer;

function WNetAddConnection2;
begin
  GetProcedureAddress(_WNetAddConnection2, mpr, 'WNetAddConnection2' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetAddConnection2]
  end;
end;

var
  _WNetAddConnection3A: Pointer;

function WNetAddConnection3A;
begin
  GetProcedureAddress(_WNetAddConnection3A, mpr, 'WNetAddConnection3A');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetAddConnection3A]
  end;
end;

var
  _WNetAddConnection3W: Pointer;

function WNetAddConnection3W;
begin
  GetProcedureAddress(_WNetAddConnection3W, mpr, 'WNetAddConnection3W');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetAddConnection3W]
  end;
end;

var
  _WNetAddConnection3: Pointer;

function WNetAddConnection3;
begin
  GetProcedureAddress(_WNetAddConnection3, mpr, 'WNetAddConnection3' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetAddConnection3]
  end;
end;

var
  _WNetCancelConnectionA: Pointer;

function WNetCancelConnectionA;
begin
  GetProcedureAddress(_WNetCancelConnectionA, mpr, 'WNetCancelConnectionA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetCancelConnectionA]
  end;
end;

var
  _WNetCancelConnectionW: Pointer;

function WNetCancelConnectionW;
begin
  GetProcedureAddress(_WNetCancelConnectionW, mpr, 'WNetCancelConnectionW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetCancelConnectionW]
  end;
end;

var
  _WNetCancelConnection: Pointer;

function WNetCancelConnection;
begin
  GetProcedureAddress(_WNetCancelConnection, mpr, 'WNetCancelConnection' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetCancelConnection]
  end;
end;

var
  _WNetCancelConnection2A: Pointer;

function WNetCancelConnection2A;
begin
  GetProcedureAddress(_WNetCancelConnection2A, mpr, 'WNetCancelConnection2A');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetCancelConnection2A]
  end;
end;

var
  _WNetCancelConnection2W: Pointer;

function WNetCancelConnection2W;
begin
  GetProcedureAddress(_WNetCancelConnection2W, mpr, 'WNetCancelConnection2W');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetCancelConnection2W]
  end;
end;

var
  _WNetCancelConnection2: Pointer;

function WNetCancelConnection2;
begin
  GetProcedureAddress(_WNetCancelConnection2, mpr, 'WNetCancelConnection2' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetCancelConnection2]
  end;
end;

var
  _WNetGetConnectionA: Pointer;

function WNetGetConnectionA;
begin
  GetProcedureAddress(_WNetGetConnectionA, mpr, 'WNetGetConnectionA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetGetConnectionA]
  end;
end;

var
  _WNetGetConnectionW: Pointer;

function WNetGetConnectionW;
begin
  GetProcedureAddress(_WNetGetConnectionW, mpr, 'WNetGetConnectionW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetGetConnectionW]
  end;
end;

var
  _WNetGetConnection: Pointer;

function WNetGetConnection;
begin
  GetProcedureAddress(_WNetGetConnection, mpr, 'WNetGetConnection' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetGetConnection]
  end;
end;

{WNetRestoreConnectionA is no more available
See bug tracker
http://sourceforge.net/tracker/index.php?func=detail&aid=1846980&group_id=121894&atid=694029

var
  _WNetRestoreConnectionA: Pointer;

function WNetRestoreConnectionA;
begin
  GetProcedureAddress(_WNetRestoreConnectionA, mpr, 'WNetRestoreConnectionA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetRestoreConnectionA]
  end;
end;
}

{$IFNDEF WINVISTA_UP}

var
  _WNetRestoreConnectionW: Pointer;

function WNetRestoreConnectionW;
begin
  GetProcedureAddress(_WNetRestoreConnectionW, mpr, 'WNetRestoreConnectionW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetRestoreConnectionW]
  end;
end;

{WNetRestoreConnectionA is no more available
See bug tracker
http://sourceforge.net/tracker/index.php?func=detail&aid=1846980&group_id=121894&atid=694029
}

var
  _WNetRestoreConnection: Pointer;

function WNetRestoreConnection;
begin
  GetProcedureAddress(_WNetRestoreConnection, mpr, 'WNetRestoreConnection' +'W'{ + AWSuffix});
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetRestoreConnection]
  end;
end;
{$ENDIF WINVISTA_UP}

var
  _WNetUseConnectionA: Pointer;

function WNetUseConnectionA;
begin
  GetProcedureAddress(_WNetUseConnectionA, mpr, 'WNetUseConnectionA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetUseConnectionA]
  end;
end;

var
  _WNetUseConnectionW: Pointer;

function WNetUseConnectionW;
begin
  GetProcedureAddress(_WNetUseConnectionW, mpr, 'WNetUseConnectionW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetUseConnectionW]
  end;
end;

var
  _WNetUseConnection: Pointer;

function WNetUseConnection;
begin
  GetProcedureAddress(_WNetUseConnection, mpr, 'WNetUseConnection' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetUseConnection]
  end;
end;

var
  _WNetConnectionDialog: Pointer;

function WNetConnectionDialog;
begin
  GetProcedureAddress(_WNetConnectionDialog, mpr, 'WNetConnectionDialog');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetConnectionDialog]
  end;
end;

var
  _WNetDisconnectDialog: Pointer;

function WNetDisconnectDialog;
begin
  GetProcedureAddress(_WNetDisconnectDialog, mpr, 'WNetDisconnectDialog');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetDisconnectDialog]
  end;
end;

var
  _WNetConnectionDialog1A: Pointer;

function WNetConnectionDialog1A;
begin
  GetProcedureAddress(_WNetConnectionDialog1A, mpr, 'WNetConnectionDialog1A');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetConnectionDialog1A]
  end;
end;

var
  _WNetConnectionDialog1W: Pointer;

function WNetConnectionDialog1W;
begin
  GetProcedureAddress(_WNetConnectionDialog1W, mpr, 'WNetConnectionDialog1W');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetConnectionDialog1W]
  end;
end;

var
  _WNetConnectionDialog1: Pointer;

function WNetConnectionDialog1;
begin
  GetProcedureAddress(_WNetConnectionDialog1, mpr, 'WNetConnectionDialog1' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetConnectionDialog1]
  end;
end;

var
  _WNetDisconnectDialog1A: Pointer;

function WNetDisconnectDialog1A;
begin
  GetProcedureAddress(_WNetDisconnectDialog1A, mpr, 'WNetDisconnectDialog1A');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetDisconnectDialog1A]
  end;
end;

var
  _WNetDisconnectDialog1W: Pointer;

function WNetDisconnectDialog1W;
begin
  GetProcedureAddress(_WNetDisconnectDialog1W, mpr, 'WNetDisconnectDialog1W');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetDisconnectDialog1W]
  end;
end;

var
  _WNetDisconnectDialog1: Pointer;

function WNetDisconnectDialog1;
begin
  GetProcedureAddress(_WNetDisconnectDialog1, mpr, 'WNetDisconnectDialog1' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetDisconnectDialog1]
  end;
end;

var
  _WNetOpenEnumA: Pointer;

function WNetOpenEnumA;
begin
  GetProcedureAddress(_WNetOpenEnumA, mpr, 'WNetOpenEnumA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetOpenEnumA]
  end;
end;

var
  _WNetOpenEnumW: Pointer;

function WNetOpenEnumW;
begin
  GetProcedureAddress(_WNetOpenEnumW, mpr, 'WNetOpenEnumW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetOpenEnumW]
  end;
end;

var
  _WNetOpenEnum: Pointer;

function WNetOpenEnum;
begin
  GetProcedureAddress(_WNetOpenEnum, mpr, 'WNetOpenEnum' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetOpenEnum]
  end;
end;

var
  _WNetEnumResourceA: Pointer;

function WNetEnumResourceA;
begin
  GetProcedureAddress(_WNetEnumResourceA, mpr, 'WNetEnumResourceA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetEnumResourceA]
  end;
end;

var
  _WNetEnumResourceW: Pointer;

function WNetEnumResourceW;
begin
  GetProcedureAddress(_WNetEnumResourceW, mpr, 'WNetEnumResourceW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetEnumResourceW]
  end;
end;

var
  _WNetEnumResource: Pointer;

function WNetEnumResource;
begin
  GetProcedureAddress(_WNetEnumResource, mpr, 'WNetEnumResource' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetEnumResource]
  end;
end;

var
  _WNetCloseEnum: Pointer;

function WNetCloseEnum;
begin
  GetProcedureAddress(_WNetCloseEnum, mpr, 'WNetCloseEnum');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetCloseEnum]
  end;
end;

var
  _WNetGetResourceParentA: Pointer;

function WNetGetResourceParentA;
begin
  GetProcedureAddress(_WNetGetResourceParentA, mpr, 'WNetGetResourceParentA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetGetResourceParentA]
  end;
end;

var
  _WNetGetResourceParentW: Pointer;

function WNetGetResourceParentW;
begin
  GetProcedureAddress(_WNetGetResourceParentW, mpr, 'WNetGetResourceParentW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetGetResourceParentW]
  end;
end;

var
  _WNetGetResourceParent: Pointer;

function WNetGetResourceParent;
begin
  GetProcedureAddress(_WNetGetResourceParent, mpr, 'WNetGetResourceParent' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetGetResourceParent]
  end;
end;

var
  _WNetGetResourceInformationA: Pointer;

function WNetGetResourceInformationA;
begin
  GetProcedureAddress(_WNetGetResourceInformationA, mpr, 'WNetGetResourceInformationA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetGetResourceInformationA]
  end;
end;

var
  _WNetGetResourceInformationW: Pointer;

function WNetGetResourceInformationW;
begin
  GetProcedureAddress(_WNetGetResourceInformationW, mpr, 'WNetGetResourceInformationW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetGetResourceInformationW]
  end;
end;

var
  _WNetGetResourceInformation: Pointer;

function WNetGetResourceInformation;
begin
  GetProcedureAddress(_WNetGetResourceInformation, mpr, 'WNetGetResourceInformation' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetGetResourceInformation]
  end;
end;

var
  _WNetGetUniversalNameA: Pointer;

function WNetGetUniversalNameA;
begin
  GetProcedureAddress(_WNetGetUniversalNameA, mpr, 'WNetGetUniversalNameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetGetUniversalNameA]
  end;
end;

var
  _WNetGetUniversalNameW: Pointer;

function WNetGetUniversalNameW;
begin
  GetProcedureAddress(_WNetGetUniversalNameW, mpr, 'WNetGetUniversalNameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetGetUniversalNameW]
  end;
end;

var
  _WNetGetUniversalName: Pointer;

function WNetGetUniversalName;
begin
  GetProcedureAddress(_WNetGetUniversalName, mpr, 'WNetGetUniversalName' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetGetUniversalName]
  end;
end;

var
  _WNetGetUserA: Pointer;

function WNetGetUserA;
begin
  GetProcedureAddress(_WNetGetUserA, mpr, 'WNetGetUserA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetGetUserA]
  end;
end;

var
  _WNetGetUserW: Pointer;

function WNetGetUserW;
begin
  GetProcedureAddress(_WNetGetUserW, mpr, 'WNetGetUserW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetGetUserW]
  end;
end;

var
  _WNetGetUser: Pointer;

function WNetGetUser;
begin
  GetProcedureAddress(_WNetGetUser, mpr, 'WNetGetUser' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetGetUser]
  end;
end;

var
  _WNetGetProviderNameA: Pointer;

function WNetGetProviderNameA;
begin
  GetProcedureAddress(_WNetGetProviderNameA, mpr, 'WNetGetProviderNameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetGetProviderNameA]
  end;
end;

var
  _WNetGetProviderNameW: Pointer;

function WNetGetProviderNameW;
begin
  GetProcedureAddress(_WNetGetProviderNameW, mpr, 'WNetGetProviderNameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetGetProviderNameW]
  end;
end;

var
  _WNetGetProviderName: Pointer;

function WNetGetProviderName;
begin
  GetProcedureAddress(_WNetGetProviderName, mpr, 'WNetGetProviderName' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetGetProviderName]
  end;
end;

var
  _WNetGetNetworkInformationA: Pointer;

function WNetGetNetworkInformationA;
begin
  GetProcedureAddress(_WNetGetNetworkInformationA, mpr, 'WNetGetNetworkInformationA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetGetNetworkInformationA]
  end;
end;

var
  _WNetGetNetworkInformationW: Pointer;

function WNetGetNetworkInformationW;
begin
  GetProcedureAddress(_WNetGetNetworkInformationW, mpr, 'WNetGetNetworkInformationW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetGetNetworkInformationW]
  end;
end;

var
  _WNetGetNetworkInformation: Pointer;

function WNetGetNetworkInformation;
begin
  GetProcedureAddress(_WNetGetNetworkInformation, mpr, 'WNetGetNetworkInformation' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetGetNetworkInformation]
  end;
end;

var
  _WNetGetLastErrorA: Pointer;

function WNetGetLastErrorA;
begin
  GetProcedureAddress(_WNetGetLastErrorA, mpr, 'WNetGetLastErrorA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetGetLastErrorA]
  end;
end;

var
  _WNetGetLastErrorW: Pointer;

function WNetGetLastErrorW;
begin
  GetProcedureAddress(_WNetGetLastErrorW, mpr, 'WNetGetLastErrorW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetGetLastErrorW]
  end;
end;

var
  _WNetGetLastError: Pointer;

function WNetGetLastError;
begin
  GetProcedureAddress(_WNetGetLastError, mpr, 'WNetGetLastError' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WNetGetLastError]
  end;
end;

var
  _MultinetGetConnectionPerfA: Pointer;

function MultinetGetConnectionPerformanceA;
begin
  GetProcedureAddress(_MultinetGetConnectionPerfA, mpr, 'MultinetGetConnectionPerformanceA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MultinetGetConnectionPerfA]
  end;
end;

var
  _MultinetGetConnectionPerfW: Pointer;

function MultinetGetConnectionPerformanceW;
begin
  GetProcedureAddress(_MultinetGetConnectionPerfW, mpr, 'MultinetGetConnectionPerformanceW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MultinetGetConnectionPerfW]
  end;
end;

var
  _MultinetGetConnectionPerf: Pointer;

function MultinetGetConnectionPerformance;
begin
  GetProcedureAddress(_MultinetGetConnectionPerf, mpr, 'MultinetGetConnectionPerformance' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MultinetGetConnectionPerf]
  end;
end;

{$ELSE}

function WNetAddConnectionA; external mpr name 'WNetAddConnectionA';
function WNetAddConnectionW; external mpr name 'WNetAddConnectionW';
function WNetAddConnection; external mpr name 'WNetAddConnection' + AWSuffix;
function WNetAddConnection2A; external mpr name 'WNetAddConnection2A';
function WNetAddConnection2W; external mpr name 'WNetAddConnection2W';
function WNetAddConnection2; external mpr name 'WNetAddConnection2' + AWSuffix;
function WNetAddConnection3A; external mpr name 'WNetAddConnection3A';
function WNetAddConnection3W; external mpr name 'WNetAddConnection3W';
function WNetAddConnection3; external mpr name 'WNetAddConnection3' + AWSuffix;
function WNetCancelConnectionA; external mpr name 'WNetCancelConnectionA';
function WNetCancelConnectionW; external mpr name 'WNetCancelConnectionW';
function WNetCancelConnection; external mpr name 'WNetCancelConnection' + AWSuffix;
function WNetCancelConnection2A; external mpr name 'WNetCancelConnection2A';
function WNetCancelConnection2W; external mpr name 'WNetCancelConnection2W';
function WNetCancelConnection2; external mpr name 'WNetCancelConnection2' + AWSuffix;
function WNetGetConnectionA; external mpr name 'WNetGetConnectionA';
function WNetGetConnectionW; external mpr name 'WNetGetConnectionW';
function WNetGetConnection; external mpr name 'WNetGetConnection' + AWSuffix;
{WNetRestoreConnectionA is no more available
See bug tracker
http://sourceforge.net/tracker/index.php?func=detail&aid=1846980&group_id=121894&atid=694029
function WNetRestoreConnectionA; external mpr name 'WNetRestoreConnectionA';
}
{$IFNDEF WINVISTA_UP}
function WNetRestoreConnectionW; external mpr name 'WNetRestoreConnectionW';
function WNetRestoreConnection; external mpr name 'WNetRestoreConnection' +'A'{+ AWSuffix};
{$ENDIF WINVISTA_UP}
function WNetUseConnectionA; external mpr name 'WNetUseConnectionA';
function WNetUseConnectionW; external mpr name 'WNetUseConnectionW';
function WNetUseConnection; external mpr name 'WNetUseConnection' + AWSuffix;
function WNetConnectionDialog; external mpr name 'WNetConnectionDialog';
function WNetDisconnectDialog; external mpr name 'WNetDisconnectDialog';
function WNetConnectionDialog1A; external mpr name 'WNetConnectionDialog1A';
function WNetConnectionDialog1W; external mpr name 'WNetConnectionDialog1W';
function WNetConnectionDialog1; external mpr name 'WNetConnectionDialog1' + AWSuffix;
function WNetDisconnectDialog1A; external mpr name 'WNetDisconnectDialog1A';
function WNetDisconnectDialog1W; external mpr name 'WNetDisconnectDialog1W';
function WNetDisconnectDialog1; external mpr name 'WNetDisconnectDialog1' + AWSuffix;
function WNetOpenEnumA; external mpr name 'WNetOpenEnumA';
function WNetOpenEnumW; external mpr name 'WNetOpenEnumW';
function WNetOpenEnum; external mpr name 'WNetOpenEnum' + AWSuffix;
function WNetEnumResourceA; external mpr name 'WNetEnumResourceA';
function WNetEnumResourceW; external mpr name 'WNetEnumResourceW';
function WNetEnumResource; external mpr name 'WNetEnumResource' + AWSuffix;
function WNetCloseEnum; external mpr name 'WNetCloseEnum';
function WNetGetResourceParentA; external mpr name 'WNetGetResourceParentA';
function WNetGetResourceParentW; external mpr name 'WNetGetResourceParentW';
function WNetGetResourceParent; external mpr name 'WNetGetResourceParent' + AWSuffix;
function WNetGetResourceInformationA; external mpr name 'WNetGetResourceInformationA';
function WNetGetResourceInformationW; external mpr name 'WNetGetResourceInformationW';
function WNetGetResourceInformation; external mpr name 'WNetGetResourceInformation' + AWSuffix;
function WNetGetUniversalNameA; external mpr name 'WNetGetUniversalNameA';
function WNetGetUniversalNameW; external mpr name 'WNetGetUniversalNameW';
function WNetGetUniversalName; external mpr name 'WNetGetUniversalName' + AWSuffix;
function WNetGetUserA; external mpr name 'WNetGetUserA';
function WNetGetUserW; external mpr name 'WNetGetUserW';
function WNetGetUser; external mpr name 'WNetGetUser' + AWSuffix;
function WNetGetProviderNameA; external mpr name 'WNetGetProviderNameA';
function WNetGetProviderNameW; external mpr name 'WNetGetProviderNameW';
function WNetGetProviderName; external mpr name 'WNetGetProviderName' + AWSuffix;
function WNetGetNetworkInformationA; external mpr name 'WNetGetNetworkInformationA';
function WNetGetNetworkInformationW; external mpr name 'WNetGetNetworkInformationW';
function WNetGetNetworkInformation; external mpr name 'WNetGetNetworkInformation' + AWSuffix;
function WNetGetLastErrorA; external mpr name 'WNetGetLastErrorA';
function WNetGetLastErrorW; external mpr name 'WNetGetLastErrorW';
function WNetGetLastError; external mpr name 'WNetGetLastError' + AWSuffix;
function MultinetGetConnectionPerformanceA; external mpr name 'MultinetGetConnectionPerformanceA';
function MultinetGetConnectionPerformanceW; external mpr name 'MultinetGetConnectionPerformanceW';
function MultinetGetConnectionPerformance; external mpr name 'MultinetGetConnectionPerformance' + AWSuffix;

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
