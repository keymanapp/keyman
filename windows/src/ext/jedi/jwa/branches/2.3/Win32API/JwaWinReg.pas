{******************************************************************************}
{                                                                              }
{ Windows Registry API interface Unit for Object Pascal                        }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: winreg.h, released June 2000. The original Pascal      }
{ code is: WinReg.pas, released December 2000. The initial developer of the    }
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

// $Id: JwaWinReg.pas,v 1.13 2007/09/05 11:58:54 dezipaitor Exp $

{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaWinReg;

{$WEAKPACKAGEUNIT}

{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "WinReg.h"'}
{$HPPEMIT ''}



{$IFNDEF JWA_OMIT_SECTIONS}

{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaReason, JwaWinBase, JwaWinNT, JwaWinType;

{$ENDIF JWA_OMIT_SECTIONS}
{$IFNDEF JWA_IMPLEMENTATIONSECTION}
//
// Requested Key access mask type.
//

type
  REGSAM = ACCESS_MASK;
  {$EXTERNALSYM REGSAM}

//
// Reserved Key Handles.
//

const
  HKEY_CLASSES_ROOT     = HKEY(ULONG_PTR(LONG($80000000)));
  {$EXTERNALSYM HKEY_CLASSES_ROOT}
  HKEY_CURRENT_USER     = HKEY(ULONG_PTR(LONG($80000001)));
  {$EXTERNALSYM HKEY_CURRENT_USER}
  HKEY_LOCAL_MACHINE    = HKEY(ULONG_PTR(LONG($80000002)));
  {$EXTERNALSYM HKEY_LOCAL_MACHINE}
  HKEY_USERS            = HKEY(ULONG_PTR(LONG($80000003)));
  {$EXTERNALSYM HKEY_USERS}
  HKEY_PERFORMANCE_DATA = HKEY(ULONG_PTR(LONG($80000004)));
  {$EXTERNALSYM HKEY_PERFORMANCE_DATA}
  HKEY_PERFORMANCE_TEXT    = HKEY(ULONG_PTR(LONG($80000050)));
  {$EXTERNALSYM HKEY_PERFORMANCE_TEXT}
  HKEY_PERFORMANCE_NLSTEXT = HKEY(ULONG_PTR(LONG($80000060)));
  {$EXTERNALSYM HKEY_PERFORMANCE_NLSTEXT}
  HKEY_CURRENT_CONFIG   = HKEY(ULONG_PTR(LONG($80000005)));
  {$EXTERNALSYM HKEY_CURRENT_CONFIG}
  HKEY_DYN_DATA         = HKEY(ULONG_PTR(LONG($80000006)));
  {$EXTERNALSYM HKEY_DYN_DATA}

  PROVIDER_KEEPS_VALUE_LENGTH = $1;
  {$EXTERNALSYM PROVIDER_KEEPS_VALUE_LENGTH}

type
  val_context = record
    valuelen: Integer;       // the total length of this value
    value_context: LPVOID;   // provider's context
    val_buff_ptr: LPVOID;    // where in the ouput buffer the value is.
  end;
  {$EXTERNALSYM val_context}
  PVALCONTEXT = ^val_context;
  {$EXTERNALSYM PVALCONTEXT}
  TValContext = val_context;

  PVALUEA = record           // Provider supplied value/context.
    pv_valuename: LPSTR;     // The value name pointer
    pv_valuelen: Integer;
    pv_value_context: LPVOID;
    pv_type: DWORD;
  end;
  {$EXTERNALSYM PVALUEA}
  PPVALUEA = ^PVALUEA;
  {$EXTERNALSYM PPVALUEA}
  TPValueA = PVALUEA;

  PVALUEW = record           // Provider supplied value/context.
    pv_valuename: LPWSTR;    // The value name pointer
    pv_valuelen: Integer;
    pv_value_context: LPVOID;
    pv_type: DWORD;
  end;
  {$EXTERNALSYM PVALUEW}
  PPVALUEW = ^PVALUEW;
  {$EXTERNALSYM PPVALUEW}
  TPValueW = PVALUEW;

  {$IFDEF UNICODE}
  PVALUE = PVALUEW;
  {$EXTERNALSYM PVALUE}
  PPVALUE = PPVALUEW;
  {$EXTERNALSYM PPVALUE}
  TPValue = TPValueW;
  {$ELSE}
  PVALUE = PVALUEA;
  {$EXTERNALSYM PVALUE}
  PPVALUE = PPVALUEA;
  {$EXTERNALSYM PPVALUE}
  TPValue = TPValueA;
  {$ENDIF UNICODE}

  QUERYHANDLER = function(keycontext: LPVOID; val_list: PVALCONTEXT;
    num_vals: DWORD; outputbuffer: LPVOID; total_outlen: LPDWORD;
    input_blen: DWORD): DWORD; cdecl;
  {$EXTERNALSYM QUERYHANDLER}
  PQUERYHANDLER = ^QUERYHANDLER;
  {$EXTERNALSYM PQUERYHANDLER}
  TQueryHandler = QUERYHANDLER;

  provider_info = record
    pi_R0_1val: PQUERYHANDLER;
    pi_R0_allvals: PQUERYHANDLER;
    pi_R3_1val: PQUERYHANDLER;
    pi_R3_allvals: PQUERYHANDLER;
    pi_flags: DWORD;    // capability flags (none defined yet).
    pi_key_context: LPVOID;
  end;
  {$EXTERNALSYM provider_info}
  REG_PROVIDER = provider_info;
  {$EXTERNALSYM REG_PROVIDER}
  PPROVIDER = ^provider_info;
  {$EXTERNALSYM PPROVIDER}
  TProviderInfo = provider_info;
  PProviderInfo = ^provider_info;

  value_entA = record
    ve_valuename: LPSTR;
    ve_valuelen: DWORD;
    ve_valueptr: DWORD_PTR;
    ve_type: DWORD;
  end;
  {$EXTERNALSYM value_entA}
  VALENTA = value_entA;
  {$EXTERNALSYM VALENTA}
  PVALENTA = ^VALENTA;
  {$EXTERNALSYM PVALENTA}
  TValueEntA = value_entA;
  PValueEntA = ^value_entA;

  value_entW = record
    ve_valuename: LPWSTR;
    ve_valuelen: DWORD;
    ve_valueptr: DWORD_PTR;
    ve_type: DWORD;
  end;
  {$EXTERNALSYM value_entW}
  VALENTW = value_entW;
  {$EXTERNALSYM VALENTW}
  PVALENTW = ^VALENTW;
  {$EXTERNALSYM PVALENTW}
  TValueEntW = value_entW;
  PValueEntW = ^value_entW;

  {$IFDEF UNICODE}
  VALENT = VALENTW;
  {$EXTERNALSYM VALENT}
  PVALENT = PVALENTW;
  {$EXTERNALSYM PVALENT}
  TValueEnt = TValueEntW;
  PValueEnt = PValueEntW;
  {$ELSE}
  VALENT = VALENTA;
  {$EXTERNALSYM VALENT}
  PVALENT = PVALENTA;
  {$EXTERNALSYM PVALENT}
  TValueEnt = TValueEntA;
  PValueEnt = PValueEntA;
  {$ENDIF UNICODE}

//
// Default values for parameters that do not exist in the Win 3.1
// compatible APIs.
//

const
  WIN31_CLASS = nil;
  {$EXTERNALSYM WIN31_CLASS}

//
// API Prototypes.
//

function RegCloseKey(hKey: HKEY): LONG; stdcall;
{$EXTERNALSYM RegCloseKey}

function RegOverridePredefKey(hKey: HKEY; hNewHKey: HKEY): LONG; stdcall;
{$EXTERNALSYM RegOverridePredefKey}

function RegOpenUserClassesRoot(hToken: HANDLE; dwOptions: DWORD;
  samDesired: REGSAM; var phkResult: HKEY): LONG; stdcall;
{$EXTERNALSYM RegOpenUserClassesRoot}

function RegOpenCurrentUser(samDesired: REGSAM; var phkResult: HKEY): LONG; stdcall;
{$EXTERNALSYM RegOpenCurrentUser}

function RegDisablePredefinedCache: LONG; stdcall;
{$EXTERNALSYM RegDisablePredefinedCache}

function RegConnectRegistryA(lpMachineName: LPCSTR; hKey: HKEY;
  var phkResult: HKEY): LONG; stdcall;
{$EXTERNALSYM RegConnectRegistryA}
function RegConnectRegistryW(lpMachineName: LPCWSTR; hKey: HKEY;
  var phkResult: HKEY): LONG; stdcall;
{$EXTERNALSYM RegConnectRegistryW}
function RegConnectRegistry(lpMachineName: LPCTSTR; hKey: HKEY;
  var phkResult: HKEY): LONG; stdcall;
{$EXTERNALSYM RegConnectRegistry}

function RegCreateKeyA(hKey: HKEY; lpSubKey: LPCSTR; var phkResult: HKEY): LONG; stdcall;
{$EXTERNALSYM RegCreateKeyA}
function RegCreateKeyW(hKey: HKEY; lpSubKey: LPCWSTR; var phkResult: HKEY): LONG; stdcall;
{$EXTERNALSYM RegCreateKeyW}
function RegCreateKey(hKey: HKEY; lpSubKey: LPCTSTR; var phkResult: HKEY): LONG; stdcall;
{$EXTERNALSYM RegCreateKey}

function RegCreateKeyExA(hKey: HKEY; lpSubKey: LPCSTR; Reserved: DWORD;
  lpClass: LPSTR; dwOptions: DWORD; samDesired: REGSAM;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES; var phkResult: HKEY;
  lpdwDisposition: LPDWORD): LONG; stdcall;
{$EXTERNALSYM RegCreateKeyExA}
function RegCreateKeyExW(hKey: HKEY; lpSubKey: LPCWSTR; Reserved: DWORD;
  lpClass: LPWSTR; dwOptions: DWORD; samDesired: REGSAM;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES; var phkResult: HKEY;
  lpdwDisposition: LPDWORD): LONG; stdcall;
{$EXTERNALSYM RegCreateKeyExW}
function RegCreateKeyEx(hKey: HKEY; lpSubKey: LPCTSTR; Reserved: DWORD;
  lpClass: LPTSTR; dwOptions: DWORD; samDesired: REGSAM;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES; var phkResult: HKEY;
  lpdwDisposition: LPDWORD): LONG; stdcall;
{$EXTERNALSYM RegCreateKeyEx}

function RegDeleteKeyA(hKey: HKEY; lpSubKey: LPCSTR): LONG; stdcall;
{$EXTERNALSYM RegDeleteKeyA}
function RegDeleteKeyW(hKey: HKEY; lpSubKey: LPCWSTR): LONG; stdcall;
{$EXTERNALSYM RegDeleteKeyW}
function RegDeleteKey(hKey: HKEY; lpSubKey: LPCTSTR): LONG; stdcall;
{$EXTERNALSYM RegDeleteKey}

function RegDeleteKeyExA(hKey: HKEY; lpSubKey: LPCSTR; samDesired : REGSAM; Reserved : DWORD): LONG; stdcall;
{$EXTERNALSYM RegDeleteKeyExA}
function RegDeleteKeyExW(hKey: HKEY; lpSubKey: LPCWSTR; samDesired : REGSAM; Reserved : DWORD): LONG; stdcall;
{$EXTERNALSYM RegDeleteKeyExW}
function RegDeleteKeyEx(hKey: HKEY; lpSubKey: LPCTSTR; samDesired : REGSAM; Reserved : DWORD): LONG; stdcall;
{$EXTERNALSYM RegDeleteKeyEx}


function RegDeleteValueA(hKey: HKEY; lpValueName: LPCSTR): LONG; stdcall;
{$EXTERNALSYM RegDeleteValueA}
function RegDeleteValueW(hKey: HKEY; lpValueName: LPCWSTR): LONG; stdcall;
{$EXTERNALSYM RegDeleteValueW}
function RegDeleteValue(hKey: HKEY; lpValueName: LPCTSTR): LONG; stdcall;
{$EXTERNALSYM RegDeleteValue}

function RegEnumKeyA(hKey: HKEY; dwIndex: DWORD; lpName: LPSTR; cbName: DWORD): LONG; stdcall;
{$EXTERNALSYM RegEnumKeyA}
function RegEnumKeyW(hKey: HKEY; dwIndex: DWORD; lpName: LPWSTR; cbName: DWORD): LONG; stdcall;
{$EXTERNALSYM RegEnumKeyW}
function RegEnumKey(hKey: HKEY; dwIndex: DWORD; lpName: LPTSTR; cbName: DWORD): LONG; stdcall;
{$EXTERNALSYM RegEnumKey}

function RegEnumKeyExA(hKey: HKEY; dwIndex: DWORD; lpName: LPSTR;
  var lpcbName: DWORD; lpReserved: LPDWORD; lpClass: LPSTR; lpcbClass: LPDWORD;
  lpftLastWriteTime: PFILETIME): LONG; stdcall;
{$EXTERNALSYM RegEnumKeyExA}
function RegEnumKeyExW(hKey: HKEY; dwIndex: DWORD; lpName: LPWSTR;
  var lpcbName: DWORD; lpReserved: LPDWORD; lpClass: LPWSTR; lpcbClass: LPDWORD;
  lpftLastWriteTime: PFILETIME): LONG; stdcall;
{$EXTERNALSYM RegEnumKeyExW}
function RegEnumKeyEx(hKey: HKEY; dwIndex: DWORD; lpName: LPTSTR;
  var lpcbName: LPDWORD; lpReserved: LPDWORD; lpClass: LPTSTR; lpcbClass: LPDWORD;
  lpftLastWriteTime: PFILETIME): LONG; stdcall;
{$EXTERNALSYM RegEnumKeyEx}

function RegEnumValueA(hKey: HKEY; dwIndex: DWORD; lpValueName: LPSTR;
  var lpcbValueName: DWORD; lpReserved, lpType: LPDWORD; lpData: LPBYTE;
  lpcbData: LPDWORD): LONG; stdcall;
{$EXTERNALSYM RegEnumValueA}
function RegEnumValueW(hKey: HKEY; dwIndex: DWORD; lpValueName: LPWSTR;
  var lpcbValueName: DWORD; lpReserved, lpType: LPDWORD; lpData: LPBYTE;
  lpcbData: LPDWORD): LONG; stdcall;
{$EXTERNALSYM RegEnumValueW}
function RegEnumValue(hKey: HKEY; dwIndex: DWORD; lpValueName: LPTSTR;
  var lpcbValueName: DWORD; lpReserved, lpType: LPDWORD; lpData: LPBYTE;
  lpcbData: LPDWORD): LONG; stdcall;
{$EXTERNALSYM RegEnumValue}

function RegFlushKey(hKey: HKEY): LONG; stdcall;
{$EXTERNALSYM RegFlushKey}

function RegGetKeySecurity(hKey: HKEY; SecurityInformation: SECURITY_INFORMATION;
  pSecurityDescriptor: PSECURITY_DESCRIPTOR; var lpcbSecurityDescriptor: DWORD): LONG; stdcall;
{$EXTERNALSYM RegGetKeySecurity}

function RegLoadKeyA(hKey: HKEY; lpSubKey: LPCSTR; lpFile: LPCSTR): LONG; stdcall;
{$EXTERNALSYM RegLoadKeyA}
function RegLoadKeyW(hKey: HKEY; lpSubKey: LPCWSTR; lpFile: LPCWSTR): LONG; stdcall;
{$EXTERNALSYM RegLoadKeyW}
function RegLoadKey(hKey: HKEY; lpSubKey: LPCTSTR; lpFile: LPCTSTR): LONG; stdcall;
{$EXTERNALSYM RegLoadKey}

function RegNotifyChangeKeyValue(hKey: HKEY; bWatchSubtree: BOOL;
  dwNotifyFilter: DWORD; hEvent: HANDLE; fAsynchronus: BOOL): LONG;
{$EXTERNALSYM RegNotifyChangeKeyValue}

function RegOpenKeyA(hKey: HKEY; lpSubKey: LPCSTR; var phkResult: HKEY): LONG; stdcall;
{$EXTERNALSYM RegOpenKeyA}
function RegOpenKeyW(hKey: HKEY; lpSubKey: LPCWSTR; var phkResult: HKEY): LONG; stdcall;
{$EXTERNALSYM RegOpenKeyW}
function RegOpenKey(hKey: HKEY; lpSubKey: LPCTSTR; var phkResult: HKEY): LONG; stdcall;
{$EXTERNALSYM RegOpenKey}

function RegOpenKeyExA(hKey: HKEY; lpSubKey: LPCSTR; ulOptions: DWORD;
  samDesired: REGSAM; var phkResult: HKEY): LONG; stdcall;
{$EXTERNALSYM RegOpenKeyExA}
function RegOpenKeyExW(hKey: HKEY; lpSubKey: LPCWSTR; ulOptions: DWORD;
  samDesired: REGSAM; var phkResult: HKEY): LONG; stdcall;
{$EXTERNALSYM RegOpenKeyExW}
function RegOpenKeyEx(hKey: HKEY; lpSubKey: LPCTSTR; ulOptions: DWORD;
  samDesired: REGSAM; var phkResult: HKEY): LONG; stdcall;
{$EXTERNALSYM RegOpenKeyEx}

function RegQueryInfoKeyA(hKey: HKEY; lpClass: LPSTR; lpcbClass, lpReserved,
  lpcSubKeys, lpcbMaxSubKeyLen, lpcbMaxClassLen, lpcValues, lpcbMaxValueNameLen,
  lpcbMaxValueLen, lpcbSecurityDescriptor: LPDWORD; lpftLastWriteTime: PFILETIME): LONG; stdcall;
{$EXTERNALSYM RegQueryInfoKeyA}
function RegQueryInfoKeyW(hKey: HKEY; lpClass: LPWSTR; lpcbClass, lpReserved,
  lpcSubKeys, lpcbMaxSubKeyLen, lpcbMaxClassLen, lpcValues, lpcbMaxValueNameLen,
  lpcbMaxValueLen, lpcbSecurityDescriptor: LPDWORD; lpftLastWriteTime: PFILETIME): LONG; stdcall;
{$EXTERNALSYM RegQueryInfoKeyW}
function RegQueryInfoKey(hKey: HKEY; lpClass: LPTSTR; lpcbClass, lpReserved,
  lpcSubKeys, lpcbMaxSubKeyLen, lpcbMaxClassLen, lpcValues, lpcbMaxValueNameLen,
  lpcbMaxValueLen, lpcbSecurityDescriptor: LPDWORD; lpftLastWriteTime: PFILETIME): LONG; stdcall;
{$EXTERNALSYM RegQueryInfoKey}

function RegQueryValueA(hKey: HKEY; lpSubKey: LPCSTR; lpValue: LPSTR;
  var lpcbValue: LONG): LONG; stdcall;
{$EXTERNALSYM RegQueryValueA}
function RegQueryValueW(hKey: HKEY; lpSubKey: LPCWSTR; lpValue: LPWSTR;
  var lpcbValue: LONG): LONG; stdcall;
{$EXTERNALSYM RegQueryValueW}
function RegQueryValue(hKey: HKEY; lpSubKey: LPCTSTR; lpValue: LPTSTR;
  var lpcbValue: LONG): LONG; stdcall;
{$EXTERNALSYM RegQueryValue}

function RegQueryMultipleValuesA(hKey: HKEY; val_list: PVALENTA; num_vals: DWORD;
  lpValueBuf: LPSTR; var ldwTotsize: DWORD): LONG; stdcall;
{$EXTERNALSYM RegQueryMultipleValuesA}
function RegQueryMultipleValuesW(hKey: HKEY; val_list: PVALENTW; num_vals: DWORD;
  lpValueBuf: LPWSTR; var ldwTotsize: DWORD): LONG; stdcall;
{$EXTERNALSYM RegQueryMultipleValuesW}
function RegQueryMultipleValues(hKey: HKEY; val_list: PVALENT; num_vals: DWORD;
  lpValueBuf: LPTSTR; var ldwTotsize: DWORD): LONG; stdcall;
{$EXTERNALSYM RegQueryMultipleValues}

function RegQueryValueExA(hKey: HKEY; lpValueName: LPCSTR; lpReserved: LPDWORD;
  lpType: LPDWORD; lpData: LPBYTE; lpcbData: LPDWORD): LONG; stdcall;
{$EXTERNALSYM RegQueryValueExA}
function RegQueryValueExW(hKey: HKEY; lpValueName: LPCWSTR; lpReserved: LPDWORD;
  lpType: LPDWORD; lpData: LPBYTE; lpcbData: LPDWORD): LONG; stdcall;
{$EXTERNALSYM RegQueryValueExW}
function RegQueryValueEx(hKey: HKEY; lpValueName: LPCTSTR; lpReserved: LPDWORD;
  lpType: LPDWORD; lpData: LPBYTE; lpcbData: LPDWORD): LONG; stdcall;
{$EXTERNALSYM RegQueryValueEx}

function RegReplaceKeyA(hKey: HKEY; lpSubKey: LPCSTR; lpNewFile: LPCSTR;
  lpOldFile: LPCSTR): LONG; stdcall;
{$EXTERNALSYM RegReplaceKeyA}
function RegReplaceKeyW(hKey: HKEY; lpSubKey: LPCWSTR; lpNewFile: LPCWSTR;
  lpOldFile: LPCWSTR): LONG; stdcall;
{$EXTERNALSYM RegReplaceKeyW}
function RegReplaceKey(hKey: HKEY; lpSubKey: LPCTSTR; lpNewFile: LPCTSTR;
  lpOldFile: LPCTSTR): LONG; stdcall;
{$EXTERNALSYM RegReplaceKey}

function RegRestoreKeyA(hKey: HKEY; lpFile: LPCSTR; dwFlags: DWORD): LONG; stdcall;
{$EXTERNALSYM RegRestoreKeyA}
function RegRestoreKeyW(hKey: HKEY; lpFile: LPCWSTR; dwFlags: DWORD): LONG; stdcall;
{$EXTERNALSYM RegRestoreKeyW}
function RegRestoreKey(hKey: HKEY; lpFile: LPCTSTR; dwFlags: DWORD): LONG; stdcall;
{$EXTERNALSYM RegRestoreKey}

function RegSaveKeyA(hKey: HKEY; lpFile: LPCSTR;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES): LONG; stdcall;
{$EXTERNALSYM RegSaveKeyA}
function RegSaveKeyW(hKey: HKEY; lpFile: LPCWSTR;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES): LONG; stdcall;
{$EXTERNALSYM RegSaveKeyW}
function RegSaveKey(hKey: HKEY; lpFile: LPCTSTR;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES): LONG; stdcall;
{$EXTERNALSYM RegSaveKey}

function RegSetKeySecurity(hKey: HKEY; SecurityInformation: SECURITY_INFORMATION;
  pSecurityDescriptor: PSECURITY_DESCRIPTOR): LONG; stdcall;
{$EXTERNALSYM RegSetKeySecurity}

function RegSetValueA(hKey: HKEY; lpSubKey: LPCSTR; dwType: DWORD;
  lpData: LPCSTR; cbData: DWORD): LONG; stdcall;
{$EXTERNALSYM RegSetValueA}
function RegSetValueW(hKey: HKEY; lpSubKey: LPCWSTR; dwType: DWORD;
  lpData: LPCWSTR; cbData: DWORD): LONG; stdcall;
{$EXTERNALSYM RegSetValueW}
function RegSetValue(hKey: HKEY; lpSubKey: LPCTSTR; dwType: DWORD;
  lpData: LPCTSTR; cbData: DWORD): LONG; stdcall;
{$EXTERNALSYM RegSetValue}

function RegSetValueExA(hKey: HKEY; lpValueName: LPCSTR; Reserved: DWORD;
  dwType: DWORD; lpData: LPBYTE; cbData: DWORD): LONG; stdcall;
{$EXTERNALSYM RegSetValueExA}
function RegSetValueExW(hKey: HKEY; lpValueName: LPCWSTR; Reserved: DWORD;
  dwType: DWORD; lpData: LPBYTE; cbData: DWORD): LONG; stdcall;
{$EXTERNALSYM RegSetValueExW}
function RegSetValueEx(hKey: HKEY; lpValueName: LPCTSTR; Reserved: DWORD;
  dwType: DWORD; lpData: LPBYTE; cbData: DWORD): LONG; stdcall;
{$EXTERNALSYM RegSetValueEx}

function RegUnLoadKeyA(hKey: HKEY; lpSubKey: LPCSTR): LONG; stdcall;
{$EXTERNALSYM RegUnLoadKeyA}
function RegUnLoadKeyW(hKey: HKEY; lpSubKey: LPCWSTR): LONG; stdcall;
{$EXTERNALSYM RegUnLoadKeyW}
function RegUnLoadKey(hKey: HKEY; lpSubKey: LPCTSTR): LONG; stdcall;
{$EXTERNALSYM RegUnLoadKey}

//
// Remoteable System Shutdown APIs
//

function InitiateSystemShutdownA(lpMachineName: LPSTR; lpMessage: LPSTR;
  dwTimeout: DWORD; bForceAppsClosed, bRebootAfterShutdown: BOOL): BOOL; stdcall;
{$EXTERNALSYM InitiateSystemShutdownA}
function InitiateSystemShutdownW(lpMachineName: LPWSTR; lpMessage: LPWSTR;
  dwTimeout: DWORD; bForceAppsClosed, bRebootAfterShutdown: BOOL): BOOL; stdcall;
{$EXTERNALSYM InitiateSystemShutdownW}
function InitiateSystemShutdown(lpMachineName: LPTSTR; lpMessage: LPTSTR;
  dwTimeout: DWORD; bForceAppsClosed, bRebootAfterShutdown: BOOL): BOOL; stdcall;
{$EXTERNALSYM InitiateSystemShutdown}

function AbortSystemShutdownA(lpMachineName: LPSTR): BOOL; stdcall;
{$EXTERNALSYM AbortSystemShutdownA}
function AbortSystemShutdownW(lpMachineName: LPWSTR): BOOL; stdcall;
{$EXTERNALSYM AbortSystemShutdownW}
function AbortSystemShutdown(lpMachineName: LPTSTR): BOOL; stdcall;
{$EXTERNALSYM AbortSystemShutdown}

//
// defines for InitiateSystemShutdownEx reason codes
//

const
  REASON_SWINSTALL    = SHTDN_REASON_MAJOR_SOFTWARE or SHTDN_REASON_MINOR_INSTALLATION;
  {$EXTERNALSYM REASON_SWINSTALL}
  REASON_HWINSTALL    = SHTDN_REASON_MAJOR_HARDWARE or SHTDN_REASON_MINOR_INSTALLATION;
  {$EXTERNALSYM REASON_HWINSTALL}
  REASON_SERVICEHANG  = SHTDN_REASON_MAJOR_SOFTWARE or SHTDN_REASON_MINOR_HUNG;
  {$EXTERNALSYM REASON_SERVICEHANG}
  REASON_UNSTABLE     = SHTDN_REASON_MAJOR_SYSTEM or SHTDN_REASON_MINOR_UNSTABLE;
  {$EXTERNALSYM REASON_UNSTABLE}
  REASON_SWHWRECONF   = SHTDN_REASON_MAJOR_SOFTWARE or SHTDN_REASON_MINOR_RECONFIG;
  {$EXTERNALSYM REASON_SWHWRECONF}
  REASON_OTHER        = SHTDN_REASON_MAJOR_OTHER or SHTDN_REASON_MINOR_OTHER;
  {$EXTERNALSYM REASON_OTHER}
  REASON_UNKNOWN      = SHTDN_REASON_UNKNOWN;
  {$EXTERNALSYM REASON_UNKNOWN}
  REASON_LEGACY_API   = SHTDN_REASON_LEGACY_API;
  {$EXTERNALSYM REASON_LEGACY_API}
  REASON_PLANNED_FLAG = SHTDN_REASON_FLAG_PLANNED;
  {$EXTERNALSYM REASON_PLANNED_FLAG}

//
// MAX Shutdown TimeOut == 10 Years in seconds
//

  MAX_SHUTDOWN_TIMEOUT = 10 * 365 * 24 * 60 * 60;
  {$EXTERNALSYM MAX_SHUTDOWN_TIMEOUT}

function InitiateSystemShutdownExA(lpMachineName: LPSTR; lpMessage: LPSTR;
  dwTimeout: DWORD; bForceAppsClosed, bRebootAfterShutdown: BOOL; dwReason: DWORD): BOOL; stdcall;
{$EXTERNALSYM InitiateSystemShutdownExA}
function InitiateSystemShutdownExW(lpMachineName: LPWSTR; lpMessage: LPWSTR;
  dwTimeout: DWORD; bForceAppsClosed, bRebootAfterShutdown: BOOL; dwReason: DWORD): BOOL; stdcall;
{$EXTERNALSYM InitiateSystemShutdownExW}
function InitiateSystemShutdownEx(lpMachineName: LPTSTR; lpMessage: LPTSTR;
  dwTimeout: DWORD; bForceAppsClosed, bRebootAfterShutdown: BOOL; dwReason: DWORD): BOOL; stdcall;
{$EXTERNALSYM InitiateSystemShutdownEx}

function RegSaveKeyExA(hKey: HKEY; lpFile: LPCSTR;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES; Flags: DWORD): LONG; stdcall;
{$EXTERNALSYM RegSaveKeyExA}
function RegSaveKeyExW(hKey: HKEY; lpFile: LPCWSTR;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES; Flags: DWORD): LONG; stdcall;
{$EXTERNALSYM RegSaveKeyExW}
function RegSaveKeyEx(hKey: HKEY; lpFile: LPCTSTR;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES; Flags: DWORD): LONG; stdcall;
{$EXTERNALSYM RegSaveKeyEx}

function Wow64Win32ApiEntry(dwFuncNumber, dwFlag, dwRes: DWORD): LONG; stdcall;
{$EXTERNALSYM Wow64Win32ApiEntry}

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}


{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
const
  advapi32 = 'advapi32.dll';
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_OMIT_SECTIONS}

type
  TRegNotifyChangeKeyValue = function(hKey: HKEY; bWatchSubtree: LongBool; dwNotifyFilter: DWORD; hEvent: HANDLE; fAsynchronus: LongBool): LONG; stdcall;

var
  _RegNotifyChangeKeyValue: Pointer;

function RegNotifyChangeKeyValue(hKey: HKEY; bWatchSubtree: LongBool; dwNotifyFilter: DWORD; hEvent: HANDLE; fAsynchronus: LongBool): LONG;
begin
  GetProcedureAddress(_RegNotifyChangeKeyValue, advapi32, 'RegNotifyChangeKeyValue');
  if bWatchSubTree then
    Result := TRegNotifyChangeKeyValue(_RegNotifyChangeKeyValue)(hKey, LongBool(1), dwNotifyFilter, hEvent, fAsynchronus)
  else
    Result := TRegNotifyChangeKeyValue(_RegNotifyChangeKeyValue)(hKey, LongBool(0), dwNotifyFilter, hEvent, fAsynchronus);
end;

{$IFDEF DYNAMIC_LINK}

var
  _RegCloseKey: Pointer;

function RegCloseKey;
begin
  GetProcedureAddress(_RegCloseKey, advapi32, 'RegCloseKey');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegCloseKey]
  end;
end;

var
  _RegOverridePredefKey: Pointer;

function RegOverridePredefKey;
begin
  GetProcedureAddress(_RegOverridePredefKey, advapi32, 'RegOverridePredefKey');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegOverridePredefKey]
  end;
end;

var
  _RegOpenUserClassesRoot: Pointer;

function RegOpenUserClassesRoot;
begin
  GetProcedureAddress(_RegOpenUserClassesRoot, advapi32, 'RegOpenUserClassesRoot');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegOpenUserClassesRoot]
  end;
end;

var
  _RegOpenCurrentUser: Pointer;

function RegOpenCurrentUser;
begin
  GetProcedureAddress(_RegOpenCurrentUser, advapi32, 'RegOpenCurrentUser');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegOpenCurrentUser]
  end;
end;

var
  _RegDisablePredefinedCache: Pointer;

function RegDisablePredefinedCache;
begin
  GetProcedureAddress(_RegDisablePredefinedCache, advapi32, 'RegDisablePredefinedCache');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegDisablePredefinedCache]
  end;
end;

var
  _RegConnectRegistryA: Pointer;

function RegConnectRegistryA;
begin
  GetProcedureAddress(_RegConnectRegistryA, advapi32, 'RegConnectRegistryA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegConnectRegistryA]
  end;
end;

var
  _RegConnectRegistryW: Pointer;

function RegConnectRegistryW;
begin
  GetProcedureAddress(_RegConnectRegistryW, advapi32, 'RegConnectRegistryW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegConnectRegistryW]
  end;
end;

var
  _RegConnectRegistry: Pointer;

function RegConnectRegistry;
begin
  GetProcedureAddress(_RegConnectRegistry, advapi32, 'RegConnectRegistry' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegConnectRegistry]
  end;
end;

var
  _RegCreateKeyA: Pointer;

function RegCreateKeyA;
begin
  GetProcedureAddress(_RegCreateKeyA, advapi32, 'RegCreateKeyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegCreateKeyA]
  end;
end;

var
  _RegCreateKeyW: Pointer;

function RegCreateKeyW;
begin
  GetProcedureAddress(_RegCreateKeyW, advapi32, 'RegCreateKeyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegCreateKeyW]
  end;
end;

var
  _RegCreateKey: Pointer;

function RegCreateKey;
begin
  GetProcedureAddress(_RegCreateKey, advapi32, 'RegCreateKey' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegCreateKey]
  end;
end;

var
  _RegCreateKeyExA: Pointer;

function RegCreateKeyExA;
begin
  GetProcedureAddress(_RegCreateKeyExA, advapi32, 'RegCreateKeyExA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegCreateKeyExA]
  end;
end;

var
  _RegCreateKeyExW: Pointer;

function RegCreateKeyExW;
begin
  GetProcedureAddress(_RegCreateKeyExW, advapi32, 'RegCreateKeyExW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegCreateKeyExW]
  end;
end;

var
  _RegCreateKeyEx: Pointer;

function RegCreateKeyEx;
begin
  GetProcedureAddress(_RegCreateKeyEx, advapi32, 'RegCreateKeyEx' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegCreateKeyEx]
  end;
end;

var
  _RegDeleteKeyA: Pointer;

function RegDeleteKeyA;
begin
  GetProcedureAddress(_RegDeleteKeyA, advapi32, 'RegDeleteKeyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegDeleteKeyA]
  end;
end;

var
  _RegDeleteKeyW: Pointer;

function RegDeleteKeyW;
begin
  GetProcedureAddress(_RegDeleteKeyW, advapi32, 'RegDeleteKeyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegDeleteKeyW]
  end;
end;

var
  _RegDeleteKey: Pointer;

function RegDeleteKey;
begin
  GetProcedureAddress(_RegDeleteKey, advapi32, 'RegDeleteKey' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegDeleteKey]
  end;
end;

//

var
  _RegDeleteKeyExA: Pointer;

function RegDeleteKeyExA;
begin
  GetProcedureAddress(_RegDeleteKeyExA, advapi32, 'RegDeleteKeyExA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegDeleteKeyExA]
  end;
end;

var
  _RegDeleteKeyExW: Pointer;

function RegDeleteKeyExW;
begin
  GetProcedureAddress(_RegDeleteKeyExW, advapi32, 'RegDeleteKeyExW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegDeleteKeyExW]
  end;
end;

var
  _RegDeleteKeyEx: Pointer;

function RegDeleteKeyEx;
begin
  GetProcedureAddress(_RegDeleteKeyEx, advapi32, 'RegDeleteKeyEx' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegDeleteKeyEx]
  end;
end;



var
  _RegDeleteValueA: Pointer;

function RegDeleteValueA;
begin
  GetProcedureAddress(_RegDeleteValueA, advapi32, 'RegDeleteValueA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegDeleteValueA]
  end;
end;

var
  _RegDeleteValueW: Pointer;

function RegDeleteValueW;
begin
  GetProcedureAddress(_RegDeleteValueW, advapi32, 'RegDeleteValueW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegDeleteValueW]
  end;
end;

var
  _RegDeleteValue: Pointer;

function RegDeleteValue;
begin
  GetProcedureAddress(_RegDeleteValue, advapi32, 'RegDeleteValue' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegDeleteValue]
  end;
end;

var
  _RegEnumKeyA: Pointer;

function RegEnumKeyA;
begin
  GetProcedureAddress(_RegEnumKeyA, advapi32, 'RegEnumKeyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegEnumKeyA]
  end;
end;

var
  _RegEnumKeyW: Pointer;

function RegEnumKeyW;
begin
  GetProcedureAddress(_RegEnumKeyW, advapi32, 'RegEnumKeyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegEnumKeyW]
  end;
end;

var
  _RegEnumKey: Pointer;

function RegEnumKey;
begin
  GetProcedureAddress(_RegEnumKey, advapi32, 'RegEnumKey' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegEnumKey]
  end;
end;

var
  _RegEnumKeyExA: Pointer;

function RegEnumKeyExA;
begin
  GetProcedureAddress(_RegEnumKeyExA, advapi32, 'RegEnumKeyExA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegEnumKeyExA]
  end;
end;

var
  _RegEnumKeyExW: Pointer;

function RegEnumKeyExW;
begin
  GetProcedureAddress(_RegEnumKeyExW, advapi32, 'RegEnumKeyExW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegEnumKeyExW]
  end;
end;

var
  _RegEnumKeyEx: Pointer;

function RegEnumKeyEx;
begin
  GetProcedureAddress(_RegEnumKeyEx, advapi32, 'RegEnumKeyEx' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegEnumKeyEx]
  end;
end;

var
  _RegEnumValueA: Pointer;

function RegEnumValueA;
begin
  GetProcedureAddress(_RegEnumValueA, advapi32, 'RegEnumValueA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegEnumValueA]
  end;
end;

var
  _RegEnumValueW: Pointer;

function RegEnumValueW;
begin
  GetProcedureAddress(_RegEnumValueW, advapi32, 'RegEnumValueW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegEnumValueW]
  end;
end;

var
  _RegEnumValue: Pointer;

function RegEnumValue;
begin
  GetProcedureAddress(_RegEnumValue, advapi32, 'RegEnumValue' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegEnumValue]
  end;
end;

var
  _RegFlushKey: Pointer;

function RegFlushKey;
begin
  GetProcedureAddress(_RegFlushKey, advapi32, 'RegFlushKey');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegFlushKey]
  end;
end;

var
  _RegGetKeySecurity: Pointer;

function RegGetKeySecurity;
begin
  GetProcedureAddress(_RegGetKeySecurity, advapi32, 'RegGetKeySecurity');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegGetKeySecurity]
  end;
end;

var
  _RegLoadKeyA: Pointer;

function RegLoadKeyA;
begin
  GetProcedureAddress(_RegLoadKeyA, advapi32, 'RegLoadKeyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegLoadKeyA]
  end;
end;

var
  _RegLoadKeyW: Pointer;

function RegLoadKeyW;
begin
  GetProcedureAddress(_RegLoadKeyW, advapi32, 'RegLoadKeyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegLoadKeyW]
  end;
end;

var
  _RegLoadKey: Pointer;

function RegLoadKey;
begin
  GetProcedureAddress(_RegLoadKey, advapi32, 'RegLoadKey' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegLoadKey]
  end;
end;

var
  _RegOpenKeyA: Pointer;

function RegOpenKeyA;
begin
  GetProcedureAddress(_RegOpenKeyA, advapi32, 'RegOpenKeyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegOpenKeyA]
  end;
end;

var
  _RegOpenKeyW: Pointer;

function RegOpenKeyW;
begin
  GetProcedureAddress(_RegOpenKeyW, advapi32, 'RegOpenKeyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegOpenKeyW]
  end;
end;

var
  _RegOpenKey: Pointer;

function RegOpenKey;
begin
  GetProcedureAddress(_RegOpenKey, advapi32, 'RegOpenKey' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegOpenKey]
  end;
end;

var
  _RegOpenKeyExA: Pointer;

function RegOpenKeyExA;
begin
  GetProcedureAddress(_RegOpenKeyExA, advapi32, 'RegOpenKeyExA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegOpenKeyExA]
  end;
end;

var
  _RegOpenKeyExW: Pointer;

function RegOpenKeyExW;
begin
  GetProcedureAddress(_RegOpenKeyExW, advapi32, 'RegOpenKeyExW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegOpenKeyExW]
  end;
end;

var
  _RegOpenKeyEx: Pointer;

function RegOpenKeyEx;
begin
  GetProcedureAddress(_RegOpenKeyEx, advapi32, 'RegOpenKeyEx' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegOpenKeyEx]
  end;
end;

var
  _RegQueryInfoKeyA: Pointer;

function RegQueryInfoKeyA;
begin
  GetProcedureAddress(_RegQueryInfoKeyA, advapi32, 'RegQueryInfoKeyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegQueryInfoKeyA]
  end;
end;

var
  _RegQueryInfoKeyW: Pointer;

function RegQueryInfoKeyW;
begin
  GetProcedureAddress(_RegQueryInfoKeyW, advapi32, 'RegQueryInfoKeyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegQueryInfoKeyW]
  end;
end;

var
  _RegQueryInfoKey: Pointer;

function RegQueryInfoKey;
begin
  GetProcedureAddress(_RegQueryInfoKey, advapi32, 'RegQueryInfoKey' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegQueryInfoKey]
  end;
end;

var
  _RegQueryValueA: Pointer;

function RegQueryValueA;
begin
  GetProcedureAddress(_RegQueryValueA, advapi32, 'RegQueryValueA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegQueryValueA]
  end;
end;

var
  _RegQueryValueW: Pointer;

function RegQueryValueW;
begin
  GetProcedureAddress(_RegQueryValueW, advapi32, 'RegQueryValueW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegQueryValueW]
  end;
end;

var
  _RegQueryValue: Pointer;

function RegQueryValue;
begin
  GetProcedureAddress(_RegQueryValue, advapi32, 'RegQueryValue' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegQueryValue]
  end;
end;

var
  _RegQueryMultipleValuesA: Pointer;

function RegQueryMultipleValuesA;
begin
  GetProcedureAddress(_RegQueryMultipleValuesA, advapi32, 'RegQueryMultipleValuesA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegQueryMultipleValuesA]
  end;
end;

var
  _RegQueryMultipleValuesW: Pointer;

function RegQueryMultipleValuesW;
begin
  GetProcedureAddress(_RegQueryMultipleValuesW, advapi32, 'RegQueryMultipleValuesW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegQueryMultipleValuesW]
  end;
end;

var
  _RegQueryMultipleValues: Pointer;

function RegQueryMultipleValues;
begin
  GetProcedureAddress(_RegQueryMultipleValues, advapi32, 'RegQueryMultipleValues' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegQueryMultipleValues]
  end;
end;

var
  _RegQueryValueExA: Pointer;

function RegQueryValueExA;
begin
  GetProcedureAddress(_RegQueryValueExA, advapi32, 'RegQueryValueExA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegQueryValueExA]
  end;
end;

var
  _RegQueryValueExW: Pointer;

function RegQueryValueExW;
begin
  GetProcedureAddress(_RegQueryValueExW, advapi32, 'RegQueryValueExW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegQueryValueExW]
  end;
end;

var
  _RegQueryValueEx: Pointer;

function RegQueryValueEx;
begin
  GetProcedureAddress(_RegQueryValueEx, advapi32, 'RegQueryValueEx' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegQueryValueEx]
  end;
end;

var
  _RegReplaceKeyA: Pointer;

function RegReplaceKeyA;
begin
  GetProcedureAddress(_RegReplaceKeyA, advapi32, 'RegReplaceKeyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegReplaceKeyA]
  end;
end;

var
  _RegReplaceKeyW: Pointer;

function RegReplaceKeyW;
begin
  GetProcedureAddress(_RegReplaceKeyW, advapi32, 'RegReplaceKeyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegReplaceKeyW]
  end;
end;

var
  _RegReplaceKey: Pointer;

function RegReplaceKey;
begin
  GetProcedureAddress(_RegReplaceKey, advapi32, 'RegReplaceKey' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegReplaceKey]
  end;
end;

var
  _RegRestoreKeyA: Pointer;

function RegRestoreKeyA;
begin
  GetProcedureAddress(_RegRestoreKeyA, advapi32, 'RegRestoreKeyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegRestoreKeyA]
  end;
end;

var
  _RegRestoreKeyW: Pointer;

function RegRestoreKeyW;
begin
  GetProcedureAddress(_RegRestoreKeyW, advapi32, 'RegRestoreKeyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegRestoreKeyW]
  end;
end;

var
  _RegRestoreKey: Pointer;

function RegRestoreKey;
begin
  GetProcedureAddress(_RegRestoreKey, advapi32, 'RegRestoreKey' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegRestoreKey]
  end;
end;

var
  _RegSaveKeyA: Pointer;

function RegSaveKeyA;
begin
  GetProcedureAddress(_RegSaveKeyA, advapi32, 'RegSaveKeyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegSaveKeyA]
  end;
end;

var
  _RegSaveKeyW: Pointer;

function RegSaveKeyW;
begin
  GetProcedureAddress(_RegSaveKeyW, advapi32, 'RegSaveKeyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegSaveKeyW]
  end;
end;

var
  _RegSaveKey: Pointer;

function RegSaveKey;
begin
  GetProcedureAddress(_RegSaveKey, advapi32, 'RegSaveKey' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegSaveKey]
  end;
end;

var
  _RegSetKeySecurity: Pointer;

function RegSetKeySecurity;
begin
  GetProcedureAddress(_RegSetKeySecurity, advapi32, 'RegSetKeySecurity');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegSetKeySecurity]
  end;
end;

var
  _RegSetValueA: Pointer;

function RegSetValueA;
begin
  GetProcedureAddress(_RegSetValueA, advapi32, 'RegSetValueA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegSetValueA]
  end;
end;

var
  _RegSetValueW: Pointer;

function RegSetValueW;
begin
  GetProcedureAddress(_RegSetValueW, advapi32, 'RegSetValueW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegSetValueW]
  end;
end;

var
  _RegSetValue: Pointer;

function RegSetValue;
begin
  GetProcedureAddress(_RegSetValue, advapi32, 'RegSetValue' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegSetValue]
  end;
end;

var
  _RegSetValueExA: Pointer;

function RegSetValueExA;
begin
  GetProcedureAddress(_RegSetValueExA, advapi32, 'RegSetValueExA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegSetValueExA]
  end;
end;

var
  _RegSetValueExW: Pointer;

function RegSetValueExW;
begin
  GetProcedureAddress(_RegSetValueExW, advapi32, 'RegSetValueExW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegSetValueExW]
  end;
end;

var
  _RegSetValueEx: Pointer;

function RegSetValueEx;
begin
  GetProcedureAddress(_RegSetValueEx, advapi32, 'RegSetValueEx' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegSetValueEx]
  end;
end;

var
  _RegUnLoadKeyA: Pointer;

function RegUnLoadKeyA;
begin
  GetProcedureAddress(_RegUnLoadKeyA, advapi32, 'RegUnLoadKeyA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegUnLoadKeyA]
  end;
end;

var
  _RegUnLoadKeyW: Pointer;

function RegUnLoadKeyW;
begin
  GetProcedureAddress(_RegUnLoadKeyW, advapi32, 'RegUnLoadKeyW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegUnLoadKeyW]
  end;
end;

var
  _RegUnLoadKey: Pointer;

function RegUnLoadKey;
begin
  GetProcedureAddress(_RegUnLoadKey, advapi32, 'RegUnLoadKey' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegUnLoadKey]
  end;
end;

var
  _InitiateSystemShutdownA: Pointer;

function InitiateSystemShutdownA;
begin
  GetProcedureAddress(_InitiateSystemShutdownA, advapi32, 'InitiateSystemShutdownA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InitiateSystemShutdownA]
  end;
end;

var
  _InitiateSystemShutdownW: Pointer;

function InitiateSystemShutdownW;
begin
  GetProcedureAddress(_InitiateSystemShutdownW, advapi32, 'InitiateSystemShutdownW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InitiateSystemShutdownW]
  end;
end;

var
  _InitiateSystemShutdown: Pointer;

function InitiateSystemShutdown;
begin
  GetProcedureAddress(_InitiateSystemShutdown, advapi32, 'InitiateSystemShutdown' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InitiateSystemShutdown]
  end;
end;

var
  _AbortSystemShutdownA: Pointer;

function AbortSystemShutdownA;
begin
  GetProcedureAddress(_AbortSystemShutdownA, advapi32, 'AbortSystemShutdownA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AbortSystemShutdownA]
  end;
end;

var
  _AbortSystemShutdownW: Pointer;

function AbortSystemShutdownW;
begin
  GetProcedureAddress(_AbortSystemShutdownW, advapi32, 'AbortSystemShutdownW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AbortSystemShutdownW]
  end;
end;

var
  _AbortSystemShutdown: Pointer;

function AbortSystemShutdown;
begin
  GetProcedureAddress(_AbortSystemShutdown, advapi32, 'AbortSystemShutdown' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AbortSystemShutdown]
  end;
end;

var
  _InitiateSystemShutdownExA: Pointer;

function InitiateSystemShutdownExA;
begin
  GetProcedureAddress(_InitiateSystemShutdownExA, advapi32, 'InitiateSystemShutdownExA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InitiateSystemShutdownExA]
  end;
end;

var
  _InitiateSystemShutdownExW: Pointer;

function InitiateSystemShutdownExW;
begin
  GetProcedureAddress(_InitiateSystemShutdownExW, advapi32, 'InitiateSystemShutdownExW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InitiateSystemShutdownExW]
  end;
end;

var
  _InitiateSystemShutdownEx: Pointer;

function InitiateSystemShutdownEx;
begin
  GetProcedureAddress(_InitiateSystemShutdownEx, advapi32, 'InitiateSystemShutdownEx' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InitiateSystemShutdownEx]
  end;
end;

var
  _RegSaveKeyExA: Pointer;

function RegSaveKeyExA;
begin
  GetProcedureAddress(_RegSaveKeyExA, advapi32, 'RegSaveKeyExA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegSaveKeyExA]
  end;
end;

var
  _RegSaveKeyExW: Pointer;

function RegSaveKeyExW;
begin
  GetProcedureAddress(_RegSaveKeyExW, advapi32, 'RegSaveKeyExW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegSaveKeyExW]
  end;
end;

var
  _RegSaveKeyEx: Pointer;

function RegSaveKeyEx;
begin
  GetProcedureAddress(_RegSaveKeyEx, advapi32, 'RegSaveKeyEx' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegSaveKeyEx]
  end;
end;

var
  _Wow64Win32ApiEntry: Pointer;

function Wow64Win32ApiEntry;
begin
  GetProcedureAddress(_Wow64Win32ApiEntry, advapi32, 'Wow64Win32ApiEntry');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_Wow64Win32ApiEntry]
  end;
end;

{$ELSE}

function RegCloseKey; external advapi32 name 'RegCloseKey';
function RegOverridePredefKey; external advapi32 name 'RegOverridePredefKey';
function RegOpenUserClassesRoot; external advapi32 name 'RegOpenUserClassesRoot';
function RegOpenCurrentUser; external advapi32 name 'RegOpenCurrentUser';
function RegDisablePredefinedCache; external advapi32 name 'RegDisablePredefinedCache';
function RegConnectRegistryA; external advapi32 name 'RegConnectRegistryA';
function RegConnectRegistryW; external advapi32 name 'RegConnectRegistryW';
function RegConnectRegistry; external advapi32 name 'RegConnectRegistry' + AWSuffix;
function RegCreateKeyA; external advapi32 name 'RegCreateKeyA';
function RegCreateKeyW; external advapi32 name 'RegCreateKeyW';
function RegCreateKey; external advapi32 name 'RegCreateKey' + AWSuffix;
function RegCreateKeyExA; external advapi32 name 'RegCreateKeyExA';
function RegCreateKeyExW; external advapi32 name 'RegCreateKeyExW';
function RegCreateKeyEx; external advapi32 name 'RegCreateKeyEx' + AWSuffix;
function RegDeleteKeyA; external advapi32 name 'RegDeleteKeyA';
function RegDeleteKeyW; external advapi32 name 'RegDeleteKeyW';
function RegDeleteKey; external advapi32 name 'RegDeleteKey' + AWSuffix;
function RegDeleteKeyExA; external advapi32 name 'RegDeleteKeyExA';
function RegDeleteKeyExW; external advapi32 name 'RegDeleteKeyExW';
function RegDeleteKeyEx; external advapi32 name 'RegDeleteKeyEx' + AWSuffix;
function RegDeleteValueA; external advapi32 name 'RegDeleteValueA';
function RegDeleteValueW; external advapi32 name 'RegDeleteValueW';
function RegDeleteValue; external advapi32 name 'RegDeleteValue' + AWSuffix;
function RegEnumKeyA; external advapi32 name 'RegEnumKeyA';
function RegEnumKeyW; external advapi32 name 'RegEnumKeyW';
function RegEnumKey; external advapi32 name 'RegEnumKey' + AWSuffix;
function RegEnumKeyExA; external advapi32 name 'RegEnumKeyExA';
function RegEnumKeyExW; external advapi32 name 'RegEnumKeyExW';
function RegEnumKeyEx; external advapi32 name 'RegEnumKeyEx' + AWSuffix;
function RegEnumValueA; external advapi32 name 'RegEnumValueA';
function RegEnumValueW; external advapi32 name 'RegEnumValueW';
function RegEnumValue; external advapi32 name 'RegEnumValue' + AWSuffix;
function RegFlushKey; external advapi32 name 'RegFlushKey';
function RegGetKeySecurity; external advapi32 name 'RegGetKeySecurity';
function RegLoadKeyA; external advapi32 name 'RegLoadKeyA';
function RegLoadKeyW; external advapi32 name 'RegLoadKeyW';
function RegLoadKey; external advapi32 name 'RegLoadKey' + AWSuffix;
function RegOpenKeyA; external advapi32 name 'RegOpenKeyA';
function RegOpenKeyW; external advapi32 name 'RegOpenKeyW';
function RegOpenKey; external advapi32 name 'RegOpenKey' + AWSuffix;
function RegOpenKeyExA; external advapi32 name 'RegOpenKeyExA';
function RegOpenKeyExW; external advapi32 name 'RegOpenKeyExW';
function RegOpenKeyEx; external advapi32 name 'RegOpenKeyEx' + AWSuffix;
function RegQueryInfoKeyA; external advapi32 name 'RegQueryInfoKeyA';
function RegQueryInfoKeyW; external advapi32 name 'RegQueryInfoKeyW';
function RegQueryInfoKey; external advapi32 name 'RegQueryInfoKey' + AWSuffix;
function RegQueryValueA; external advapi32 name 'RegQueryValueA';
function RegQueryValueW; external advapi32 name 'RegQueryValueW';
function RegQueryValue; external advapi32 name 'RegQueryValue' + AWSuffix;
function RegQueryMultipleValuesA; external advapi32 name 'RegQueryMultipleValuesA';
function RegQueryMultipleValuesW; external advapi32 name 'RegQueryMultipleValuesW';
function RegQueryMultipleValues; external advapi32 name 'RegQueryMultipleValues' + AWSuffix;
function RegQueryValueExA; external advapi32 name 'RegQueryValueExA';
function RegQueryValueExW; external advapi32 name 'RegQueryValueExW';
function RegQueryValueEx; external advapi32 name 'RegQueryValueEx' + AWSuffix;
function RegReplaceKeyA; external advapi32 name 'RegReplaceKeyA';
function RegReplaceKeyW; external advapi32 name 'RegReplaceKeyW';
function RegReplaceKey; external advapi32 name 'RegReplaceKey' + AWSuffix;
function RegRestoreKeyA; external advapi32 name 'RegRestoreKeyA';
function RegRestoreKeyW; external advapi32 name 'RegRestoreKeyW';
function RegRestoreKey; external advapi32 name 'RegRestoreKey' + AWSuffix;
function RegSaveKeyA; external advapi32 name 'RegSaveKeyA';
function RegSaveKeyW; external advapi32 name 'RegSaveKeyW';
function RegSaveKey; external advapi32 name 'RegSaveKey' + AWSuffix;
function RegSetKeySecurity; external advapi32 name 'RegSetKeySecurity';
function RegSetValueA; external advapi32 name 'RegSetValueA';
function RegSetValueW; external advapi32 name 'RegSetValueW';
function RegSetValue; external advapi32 name 'RegSetValue' + AWSuffix;
function RegSetValueExA; external advapi32 name 'RegSetValueExA';
function RegSetValueExW; external advapi32 name 'RegSetValueExW';
function RegSetValueEx; external advapi32 name 'RegSetValueEx' + AWSuffix;
function RegUnLoadKeyA; external advapi32 name 'RegUnLoadKeyA';
function RegUnLoadKeyW; external advapi32 name 'RegUnLoadKeyW';
function RegUnLoadKey; external advapi32 name 'RegUnLoadKey' + AWSuffix;
function InitiateSystemShutdownA; external advapi32 name 'InitiateSystemShutdownA';
function InitiateSystemShutdownW; external advapi32 name 'InitiateSystemShutdownW';
function InitiateSystemShutdown; external advapi32 name 'InitiateSystemShutdown' + AWSuffix;
function AbortSystemShutdownA; external advapi32 name 'AbortSystemShutdownA';
function AbortSystemShutdownW; external advapi32 name 'AbortSystemShutdownW';
function AbortSystemShutdown; external advapi32 name 'AbortSystemShutdown' + AWSuffix;
function InitiateSystemShutdownExA; external advapi32 name 'InitiateSystemShutdownExA';
function InitiateSystemShutdownExW; external advapi32 name 'InitiateSystemShutdownExW';
function InitiateSystemShutdownEx; external advapi32 name 'InitiateSystemShutdownEx' + AWSuffix;
function RegSaveKeyExA; external advapi32 name 'RegSaveKeyExA';
function RegSaveKeyExW; external advapi32 name 'RegSaveKeyExW';
function RegSaveKeyEx; external advapi32 name 'RegSaveKeyEx' + AWSuffix;
function Wow64Win32ApiEntry; external advapi32 name 'Wow64Win32ApiEntry';

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
