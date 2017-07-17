{******************************************************************************}
{                                                                              }
{ Directory Services API interface Unit for Object Pascal                      }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: dsgetdc.h, released June 2000. The original Pascal     }
{ code is: DsGetDc.pas, released December 2000. The initial developer of the   }
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

// $Id: JwaDSGetDc.pas,v 1.13 2007/09/14 06:48:45 marquardt Exp $
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaDSGetDc;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "DsGetDC.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaNtSecApi, JwaWinNT, JwaWinType;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}

//
// Structure definitions
//

//
// Flags to passed to DsGetDcName
//

const
  DS_FORCE_REDISCOVERY = $00000001;
  {$EXTERNALSYM DS_FORCE_REDISCOVERY}

  DS_DIRECTORY_SERVICE_REQUIRED  = $00000010;
  {$EXTERNALSYM DS_DIRECTORY_SERVICE_REQUIRED}
  DS_DIRECTORY_SERVICE_PREFERRED = $00000020;
  {$EXTERNALSYM DS_DIRECTORY_SERVICE_PREFERRED}
  DS_GC_SERVER_REQUIRED          = $00000040;
  {$EXTERNALSYM DS_GC_SERVER_REQUIRED}
  DS_PDC_REQUIRED                = $00000080;
  {$EXTERNALSYM DS_PDC_REQUIRED}
  DS_BACKGROUND_ONLY             = $00000100;
  {$EXTERNALSYM DS_BACKGROUND_ONLY}
  DS_IP_REQUIRED                 = $00000200;
  {$EXTERNALSYM DS_IP_REQUIRED}
  DS_KDC_REQUIRED                = $00000400;
  {$EXTERNALSYM DS_KDC_REQUIRED}
  DS_TIMESERV_REQUIRED           = $00000800;
  {$EXTERNALSYM DS_TIMESERV_REQUIRED}
  DS_WRITABLE_REQUIRED           = $00001000;
  {$EXTERNALSYM DS_WRITABLE_REQUIRED}
  DS_GOOD_TIMESERV_PREFERRED     = $00002000;
  {$EXTERNALSYM DS_GOOD_TIMESERV_PREFERRED}
  DS_AVOID_SELF                  = $00004000;
  {$EXTERNALSYM DS_AVOID_SELF}
  DS_ONLY_LDAP_NEEDED            = $00008000;
  {$EXTERNALSYM DS_ONLY_LDAP_NEEDED}

  DS_IS_FLAT_NAME = $00010000;
  {$EXTERNALSYM DS_IS_FLAT_NAME}
  DS_IS_DNS_NAME  = $00020000;
  {$EXTERNALSYM DS_IS_DNS_NAME}

  DS_RETURN_DNS_NAME  = $40000000;
  {$EXTERNALSYM DS_RETURN_DNS_NAME}
  DS_RETURN_FLAT_NAME = DWORD($80000000);
  {$EXTERNALSYM DS_RETURN_FLAT_NAME}

  DSGETDC_VALID_FLAGS =
    DS_FORCE_REDISCOVERY or
    DS_DIRECTORY_SERVICE_REQUIRED or
    DS_DIRECTORY_SERVICE_PREFERRED or
    DS_GC_SERVER_REQUIRED or
    DS_PDC_REQUIRED or
    DS_BACKGROUND_ONLY or
    DS_IP_REQUIRED or
    DS_KDC_REQUIRED or
    DS_TIMESERV_REQUIRED or
    DS_WRITABLE_REQUIRED or
    DS_GOOD_TIMESERV_PREFERRED or
    DS_AVOID_SELF or
    DS_ONLY_LDAP_NEEDED or
    DS_IS_FLAT_NAME or
    DS_IS_DNS_NAME or
    DS_RETURN_FLAT_NAME or
    DS_RETURN_DNS_NAME;
  {$EXTERNALSYM DSGETDC_VALID_FLAGS}

//
// Structure returned from DsGetDcName
//

type
  PDOMAIN_CONTROLLER_INFOA = ^DOMAIN_CONTROLLER_INFOA;
  {$EXTERNALSYM PDOMAIN_CONTROLLER_INFOA}
  _DOMAIN_CONTROLLER_INFOA = record
    DomainControllerName: LPSTR;
    DomainControllerAddress: LPSTR;
    DomainControllerAddressType: ULONG;
    DomainGuid: GUID;
    DomainName: LPSTR;
    DnsForestName: LPSTR;
    Flags: ULONG;
    DcSiteName: LPSTR;
    ClientSiteName: LPSTR;
  end;
  {$EXTERNALSYM _DOMAIN_CONTROLLER_INFOA}
  DOMAIN_CONTROLLER_INFOA = _DOMAIN_CONTROLLER_INFOA;
  {$EXTERNALSYM DOMAIN_CONTROLLER_INFOA}
  TDomainControllerInfoA = DOMAIN_CONTROLLER_INFOA;
  PDomainControllerInfoA = PDOMAIN_CONTROLLER_INFOA;

  PDOMAIN_CONTROLLER_INFOW = ^DOMAIN_CONTROLLER_INFOW;
  {$EXTERNALSYM PDOMAIN_CONTROLLER_INFOW}
  _DOMAIN_CONTROLLER_INFOW = record
    DomainControllerName: LPWSTR;
    DomainControllerAddress: LPWSTR;
    DomainControllerAddressType: ULONG;
    DomainGuid: GUID;
    DomainName: LPWSTR;
    DnsForestName: LPWSTR;
    Flags: ULONG;
    DcSiteName: LPWSTR;
    ClientSiteName: LPWSTR;
  end;
  {$EXTERNALSYM _DOMAIN_CONTROLLER_INFOW}
  DOMAIN_CONTROLLER_INFOW = _DOMAIN_CONTROLLER_INFOW;
  {$EXTERNALSYM DOMAIN_CONTROLLER_INFOW}
  TDomainControllerInfoW = DOMAIN_CONTROLLER_INFOW;
  PDomainControllerInfoW = PDOMAIN_CONTROLLER_INFOW;

  {$IFDEF UNICODE}
  DOMAIN_CONTROLLER_INFO = DOMAIN_CONTROLLER_INFOW;
  {$EXTERNALSYM DOMAIN_CONTROLLER_INFO}
  PDOMAIN_CONTROLLER_INFO = PDOMAIN_CONTROLLER_INFOW;
  {$EXTERNALSYM PDOMAIN_CONTROLLER_INFO}
  TDomainControllerInfo = TDomainControllerInfoW;
  PDomainControllerInfo = PDomainControllerInfoW;
  {$ELSE}
  DOMAIN_CONTROLLER_INFO = DOMAIN_CONTROLLER_INFOA;
  {$EXTERNALSYM DOMAIN_CONTROLLER_INFO}
  PDOMAIN_CONTROLLER_INFO = PDOMAIN_CONTROLLER_INFOA;
  {$EXTERNALSYM PDOMAIN_CONTROLLER_INFO}
  TDomainControllerInfo = TDomainControllerInfoA;
  PDomainControllerInfo = PDomainControllerInfoA;
  {$ENDIF UNICODE}

//
// Values for DomainControllerAddressType
//

const
  DS_INET_ADDRESS    = 1;
  {$EXTERNALSYM DS_INET_ADDRESS}
  DS_NETBIOS_ADDRESS = 2;
  {$EXTERNALSYM DS_NETBIOS_ADDRESS}

//
// Values for returned Flags
//

  DS_PDC_FLAG           = $00000001; // DC is PDC of Domain
  {$EXTERNALSYM DS_PDC_FLAG}
  DS_GC_FLAG            = $00000004; // DC is a GC of forest
  {$EXTERNALSYM DS_GC_FLAG}
  DS_LDAP_FLAG          = $00000008; // Server supports an LDAP server
  {$EXTERNALSYM DS_LDAP_FLAG}
  DS_DS_FLAG            = $00000010; // DC supports a DS and is a Domain Controller
  {$EXTERNALSYM DS_DS_FLAG}
  DS_KDC_FLAG           = $00000020; // DC is running KDC service
  {$EXTERNALSYM DS_KDC_FLAG}
  DS_TIMESERV_FLAG      = $00000040; // DC is running time service
  {$EXTERNALSYM DS_TIMESERV_FLAG}
  DS_CLOSEST_FLAG       = $00000080; // DC is in closest site to client
  {$EXTERNALSYM DS_CLOSEST_FLAG}
  DS_WRITABLE_FLAG      = $00000100; // DC has a writable DS
  {$EXTERNALSYM DS_WRITABLE_FLAG}
  DS_GOOD_TIMESERV_FLAG = $00000200; // DC is running time service (and has clock hardware)
  {$EXTERNALSYM DS_GOOD_TIMESERV_FLAG}
  DS_NDNC_FLAG          = $00000400; // DomainName is non-domain NC serviced by the LDAP server
  {$EXTERNALSYM DS_NDNC_FLAG}
  DS_PING_FLAGS         = $0000FFFF; // Flags returned on ping
  {$EXTERNALSYM DS_PING_FLAGS}

  DS_DNS_CONTROLLER_FLAG = $20000000; // DomainControllerName is a DNS name
  {$EXTERNALSYM DS_DNS_CONTROLLER_FLAG}
  DS_DNS_DOMAIN_FLAG     = $40000000; // DomainName is a DNS name
  {$EXTERNALSYM DS_DNS_DOMAIN_FLAG}
  DS_DNS_FOREST_FLAG     = DWORD($80000000); // DnsForestName is a DNS name
  {$EXTERNALSYM DS_DNS_FOREST_FLAG}

//
// Function Prototypes
//

function DsGetDcNameA(ComputerName, DomainName: LPCSTR; DomainGuid: LPGUID;
  SiteName: LPCSTR; Flags: ULONG; var DomainControllerInfo: PDOMAIN_CONTROLLER_INFOA): DWORD; stdcall;
{$EXTERNALSYM DsGetDcNameA}
function DsGetDcNameW(ComputerName, DomainName: LPCWSTR; DomainGuid: LPGUID;
  SiteName: LPCWSTR; Flags: ULONG; var DomainControllerInfo: PDOMAIN_CONTROLLER_INFOW): DWORD; stdcall;
{$EXTERNALSYM DsGetDcNameW}
function DsGetDcName(ComputerName, DomainName: LPCTSTR; DomainGuid: LPGUID;
  SiteName: LPCTSTR; Flags: ULONG; var DomainControllerInfo: PDOMAIN_CONTROLLER_INFO): DWORD; stdcall;
{$EXTERNALSYM DsGetDcName}

function DsGetSiteNameA(ComputerName: LPCSTR; var SiteName: LPSTR): DWORD; stdcall;
{$EXTERNALSYM DsGetSiteNameA}
function DsGetSiteNameW(ComputerName: LPCWSTR; var SiteName: LPWSTR): DWORD; stdcall;
{$EXTERNALSYM DsGetSiteNameW}
function DsGetSiteName(ComputerName: LPCTSTR; var SiteName: LPTSTR): DWORD; stdcall;
{$EXTERNALSYM DsGetSiteName}

function DsValidateSubnetNameA(SubnetName: LPCSTR): DWORD; stdcall;
{$EXTERNALSYM DsValidateSubnetNameA}
function DsValidateSubnetNameW(SubnetName: LPCWSTR): DWORD; stdcall;
{$EXTERNALSYM DsValidateSubnetNameW}
function DsValidateSubnetName(SubnetName: LPCTSTR): DWORD; stdcall;
{$EXTERNALSYM DsValidateSubnetName}

//
// Only include if winsock2.h has been included
//

// Types from Winsock2.h 



{$IFNDEF JWA_INCLUDEMODE}
type
  sockaddr = record
    sa_family: Word;                  // address family
    sa_data: array [0..13] of AnsiChar;   // up to 14 bytes of direct address
  end;
  {$EXTERNALSYM sockaddr}

  PSOCKADDR = ^SOCKADDR;
  {$EXTERNALSYM PSOCKADDR}
{$ENDIF JWA_INCLUDEMODE}

{$IFNDEF JWA_WINSOCK_2}
//include only if not defined in jwaWinsock2.pas
type

  LPSOCKADDR = PSOCKADDR;
  {$EXTERNALSYM LPSOCKADDR}

  _SOCKET_ADDRESS = record
    lpSockaddr: LPSOCKADDR;
    iSockaddrLength: Integer;
  end;
  {$EXTERNALSYM _SOCKET_ADDRESS}
  SOCKET_ADDRESS = _SOCKET_ADDRESS;
  {$EXTERNALSYM SOCKET_ADDRESS}
  PSOCKET_ADDRESS = ^SOCKET_ADDRESS;
  {$EXTERNALSYM PSOCKET_ADDRESS}
  LPSOCKET_ADDRESS = PSOCKET_ADDRESS;
  {$EXTERNALSYM LPSOCKET_ADDRESS}
  TSocketAddress = SOCKET_ADDRESS;
  PSocketAddress = LPSOCKET_ADDRESS;
{$ENDIF JWA_WINSOCK_2}

function DsAddressToSiteNamesA(ComputerName: LPCSTR; EntryCount: DWORD;
  SocketAddresses: PSOCKET_ADDRESS; var SiteNames: PPAnsiChar): DWORD; stdcall;
{$EXTERNALSYM DsAddressToSiteNamesA}
function DsAddressToSiteNamesW(ComputerName: LPCWSTR; EntryCount: DWORD;
  SocketAddresses: PSOCKET_ADDRESS; var SiteNames: PPWideChar): DWORD; stdcall;
{$EXTERNALSYM DsAddressToSiteNamesW}
function DsAddressToSiteNames(ComputerName: LPCTSTR; EntryCount: DWORD;
  SocketAddresses: PSOCKET_ADDRESS; var SiteNames: PPTCHAR): DWORD; stdcall;
{$EXTERNALSYM DsAddressToSiteNames}

function DsAddressToSiteNamesExA(ComputerName: LPCSTR; EntryCount: DWORD;
  SocketAddresses: PSOCKET_ADDRESS; var SiteNames, SubnetNames: PPAnsiChar): DWORD; stdcall;
{$EXTERNALSYM DsAddressToSiteNamesExA}
function DsAddressToSiteNamesExW(ComputerName: LPCWSTR; EntryCount: DWORD;
  SocketAddresses: PSOCKET_ADDRESS; var SiteNames, SubnetNames: PPWideChar): DWORD; stdcall;
{$EXTERNALSYM DsAddressToSiteNamesExW}
function DsAddressToSiteNamesEx(ComputerName: LPCTSTR; EntryCount: DWORD;
  SocketAddresses: PSOCKET_ADDRESS; var SiteNames, SubnetNames: PPTCHAR): DWORD; stdcall;
{$EXTERNALSYM DsAddressToSiteNamesEx}

//
// API to enumerate trusted domains
//

const
  DS_DOMAIN_IN_FOREST       = $0001; // Domain is a member of the forest
  {$EXTERNALSYM DS_DOMAIN_IN_FOREST}
  DS_DOMAIN_DIRECT_OUTBOUND = $0002; // Domain is directly trusted
  {$EXTERNALSYM DS_DOMAIN_DIRECT_OUTBOUND}
  DS_DOMAIN_TREE_ROOT       = $0004; // Domain is root of a tree in the forest
  {$EXTERNALSYM DS_DOMAIN_TREE_ROOT}
  DS_DOMAIN_PRIMARY         = $0008; // Domain is the primary domain of queried server
  {$EXTERNALSYM DS_DOMAIN_PRIMARY}
  DS_DOMAIN_NATIVE_MODE     = $0010; // Primary domain is running in native mode
  {$EXTERNALSYM DS_DOMAIN_NATIVE_MODE}
  DS_DOMAIN_DIRECT_INBOUND  = $0020; // Domain is directly trusting
  {$EXTERNALSYM DS_DOMAIN_DIRECT_INBOUND}
  DS_DOMAIN_VALID_FLAGS = DS_DOMAIN_IN_FOREST or DS_DOMAIN_DIRECT_OUTBOUND or
    DS_DOMAIN_TREE_ROOT or DS_DOMAIN_PRIMARY or DS_DOMAIN_NATIVE_MODE or
    DS_DOMAIN_DIRECT_INBOUND;
  {$EXTERNALSYM DS_DOMAIN_VALID_FLAGS}

type
  PDS_DOMAIN_TRUSTSW = ^DS_DOMAIN_TRUSTSW;
  {$EXTERNALSYM PDS_DOMAIN_TRUSTSW}
  _DS_DOMAIN_TRUSTSW = record
    //
    // Name of the trusted domain.
    //
    NetbiosDomainName: LPWSTR;
    DnsDomainName: LPWSTR;
    //
    // Flags defining attributes of the trust.
    //
    Flags: ULONG;
    //
    // Index to the domain that is the parent of this domain.
    //  Only defined if NETLOGON_DOMAIN_IN_FOREST is set and
    //      NETLOGON_DOMAIN_TREE_ROOT is not set.
    //
    ParentIndex: ULONG;
    //
    // The trust type and attributes of this trust.
    //
    // If NETLOGON_DOMAIN_DIRECTLY_TRUSTED is not set,
    //  these value are infered.
    //
    TrustType: ULONG;
    TrustAttributes: ULONG;
    //
    // The SID of the trusted domain.
    //
    // If NETLOGON_DOMAIN_DIRECTLY_TRUSTED is not set,
    //  this value will be NULL.
    //
    DomainSid: PSID;
    //
    // The GUID of the trusted domain.
    //
    DomainGuid: GUID;
  end;
  {$EXTERNALSYM _DS_DOMAIN_TRUSTSW}
  DS_DOMAIN_TRUSTSW = _DS_DOMAIN_TRUSTSW;
  {$EXTERNALSYM DS_DOMAIN_TRUSTSW}
  TDsDomainTrustsW = DS_DOMAIN_TRUSTSW;
  PDsDomainTrustsW = PDS_DOMAIN_TRUSTSW;

//
// ANSI version of the above struct
//

  PDS_DOMAIN_TRUSTSA = ^DS_DOMAIN_TRUSTSA;
  {$EXTERNALSYM PDS_DOMAIN_TRUSTSA}
  _DS_DOMAIN_TRUSTSA = record
    NetbiosDomainName: LPSTR;
    DnsDomainName: LPSTR;
    Flags: ULONG;
    ParentIndex: ULONG;
    TrustType: ULONG;
    TrustAttributes: ULONG;
    DomainSid: PSID;
    DomainGuid: GUID;
  end;
  {$EXTERNALSYM _DS_DOMAIN_TRUSTSA}
  DS_DOMAIN_TRUSTSA = _DS_DOMAIN_TRUSTSA;
  {$EXTERNALSYM DS_DOMAIN_TRUSTSA}
  TDsDomainTrustsA = DS_DOMAIN_TRUSTSA;
  PDsDomainTrustsA = PDS_DOMAIN_TRUSTSA;

  {$IFDEF UNICODE}
  DS_DOMAIN_TRUSTS = DS_DOMAIN_TRUSTSW;
  {$EXTERNALSYM DS_DOMAIN_TRUSTS}
  PDS_DOMAIN_TRUSTS = PDS_DOMAIN_TRUSTSW;
  {$EXTERNALSYM PDS_DOMAIN_TRUSTS}
  TDsDomainTrusts = TDsDomainTrustsW;
  PDsDomainTrusts = PDsDomainTrustsW;
  {$ELSE}
  DS_DOMAIN_TRUSTS = DS_DOMAIN_TRUSTSA;
  {$EXTERNALSYM DS_DOMAIN_TRUSTS}
  PDS_DOMAIN_TRUSTS = PDS_DOMAIN_TRUSTSA;
  {$EXTERNALSYM PDS_DOMAIN_TRUSTS}
  TDsDomainTrusts = TDsDomainTrustsA;
  PDsDomainTrusts = PDsDomainTrustsA;
  {$ENDIF UNICODE}

function DsEnumerateDomainTrustsA(ServerName: LPSTR; Flags: ULONG;
  var Domains: PDS_DOMAIN_TRUSTSA; var DomainCount: ULONG): DWORD; stdcall;
{$EXTERNALSYM DsEnumerateDomainTrustsA}
function DsEnumerateDomainTrustsW(ServerName: LPWSTR; Flags: ULONG;
  var Domains: PDS_DOMAIN_TRUSTSW; var DomainCount: ULONG): DWORD; stdcall;
{$EXTERNALSYM DsEnumerateDomainTrustsW}
function DsEnumerateDomainTrusts(ServerName: LPTSTR; Flags: ULONG;
  var Domains: PDS_DOMAIN_TRUSTS; var DomainCount: ULONG): DWORD; stdcall;
{$EXTERNALSYM DsEnumerateDomainTrusts}

//
// Only define this API if the caller has #included the pre-requisite 
// ntlsa.h or ntsecapi.h  
//

function DsGetForestTrustInformationW(ServerName, TrustedDomainName: LPCWSTR;
  Flags: DWORD; var ForestTrustInfo: PLSA_FOREST_TRUST_INFORMATION): DWORD; stdcall;
{$EXTERNALSYM DsGetForestTrustInformationW}

const
  DS_GFTI_UPDATE_TDO    = $1;     // Update TDO with information returned
  {$EXTERNALSYM DS_GFTI_UPDATE_TDO}
  DS_GFTI_VALID_FLAGS   = $1;     // All valid flags to DsGetForestTrustInformation
  {$EXTERNALSYM DS_GFTI_VALID_FLAGS}

function DsMergeForestTrustInformationW(DomainName: LPCWSTR; NewForestTrustInfo,
  OldForestTrustInfo: PLSA_FOREST_TRUST_INFORMATION;
  var MergedForestTrustInfo: PLSA_FOREST_TRUST_INFORMATION): DWORD; stdcall;
{$EXTERNALSYM DsMergeForestTrustInformationW}

function DsGetDcSiteCoverageA(ServerName: LPCSTR; var EntryCount: ULONG;
  var SiteNames: PPAnsiChar): DWORD; stdcall;
{$EXTERNALSYM DsGetDcSiteCoverageA}
function DsGetDcSiteCoverageW(ServerName: LPCWSTR; var EntryCount: ULONG;
  var SiteNames: PPWideChar): DWORD; stdcall;
{$EXTERNALSYM DsGetDcSiteCoverageW}
function DsGetDcSiteCoverage(ServerName: LPCTSTR; var EntryCount: ULONG;
  var SiteNames: PPTCHAR): DWORD; stdcall;
{$EXTERNALSYM DsGetDcSiteCoverage}

function DsDeregisterDnsHostRecordsA(ServerName, DnsDomainName: LPSTR;
  DomainGuid, DsaGuid: LPGUID; DnsHostName: LPSTR): DWORD; stdcall;
{$EXTERNALSYM DsDeregisterDnsHostRecordsA}
function DsDeregisterDnsHostRecordsW(ServerName, DnsDomainName: LPWSTR;
  DomainGuid, DsaGuid: LPGUID; DnsHostName: LPWSTR): DWORD; stdcall;
{$EXTERNALSYM DsDeregisterDnsHostRecordsW}
function DsDeregisterDnsHostRecords(ServerName, DnsDomainName: LPTSTR;
  DomainGuid, DsaGuid: LPGUID; DnsHostName: LPTSTR): DWORD; stdcall;
{$EXTERNALSYM DsDeregisterDnsHostRecords}

//
// Option flags passed to DsGetDcOpen
//

const
  DS_ONLY_DO_SITE_NAME         = $01;   // Non-site specific names should be avoided.
  {$EXTERNALSYM DS_ONLY_DO_SITE_NAME}
  DS_NOTIFY_AFTER_SITE_RECORDS = $02;   // Return ERROR_FILEMARK_DETECTED after all
  {$EXTERNALSYM DS_NOTIFY_AFTER_SITE_RECORDS}
                                        //  site specific records have been processed.

  DS_OPEN_VALID_OPTION_FLAGS = DS_ONLY_DO_SITE_NAME or DS_NOTIFY_AFTER_SITE_RECORDS;
  {$EXTERNALSYM DS_OPEN_VALID_OPTION_FLAGS}

//
// Valid DcFlags for DsGetDcOpen
//

  DS_OPEN_VALID_FLAGS =
    DS_FORCE_REDISCOVERY or
    DS_ONLY_LDAP_NEEDED or
    DS_KDC_REQUIRED or
    DS_PDC_REQUIRED or
    DS_GC_SERVER_REQUIRED or
    DS_WRITABLE_REQUIRED;
  {$EXTERNALSYM DS_OPEN_VALID_FLAGS}

function DsGetDcOpenW(DnsName: LPCWSTR; OptionFlags: ULONG; SiteName: LPCWSTR;
  DomainGuid: PGUID; DnsForestName: LPCWSTR; DcFlags: ULONG;
  var RetGetDcContext: HANDLE): DWORD; stdcall;
{$EXTERNALSYM DsGetDcOpenW}

function DsGetDcOpenA(DnsName: LPCSTR; OptionFlags: ULONG; SiteName: LPCSTR;
  DomainGuid: PGUID; DnsForestName: LPCSTR; DcFlags: ULONG;
  var RetGetDcContext: HANDLE): DWORD; stdcall;
{$EXTERNALSYM DsGetDcOpenA}
function DsGetDcOpen(DnsName: LPCTSTR; OptionFlags: ULONG; SiteName: LPCTSTR;
  DomainGuid: PGUID; DnsForestName: LPCTSTR; DcFlags: ULONG;
  var RetGetDcContext: HANDLE): DWORD; stdcall;
{$EXTERNALSYM DsGetDcOpen}

function DsGetDcNextA(GetDcContextHandle: HANDLE; SockAddressCount: PULONG;
  SockAddresses: LPSOCKET_ADDRESS; DnsHostName: LPSTR): DWORD; stdcall;
{$EXTERNALSYM DsGetDcNextA}
function DsGetDcNextW(GetDcContextHandle: HANDLE; SockAddressCount: PULONG;
  SockAddresses: LPSOCKET_ADDRESS; DnsHostName: LPWSTR): DWORD; stdcall;
{$EXTERNALSYM DsGetDcNextW}
function DsGetDcNext(GetDcContextHandle: HANDLE; SockAddressCount: PULONG;
  SockAddresses: LPSOCKET_ADDRESS; DnsHostName: LPTSTR): DWORD; stdcall;
{$EXTERNALSYM DsGetDcNext}

procedure DsGetDcCloseW(GetDcContextHandle: HANDLE); stdcall;
{$EXTERNALSYM DsGetDcCloseW}
procedure DsGetDcClose(GetDcContextHandle: HANDLE); stdcall;
{$EXTERNALSYM DsGetDcClose}

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  netapi32 = 'netapi32.dll';
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}

var
  _DsGetDcNameA: Pointer;

function DsGetDcNameA;
begin
  GetProcedureAddress(_DsGetDcNameA, netapi32, 'DsGetDcNameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsGetDcNameA]
  end;
end;

var
  _DsGetDcNameW: Pointer;

function DsGetDcNameW;
begin
  GetProcedureAddress(_DsGetDcNameW, netapi32, 'DsGetDcNameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsGetDcNameW]
  end;
end;

var
  _DsGetDcName: Pointer;

function DsGetDcName;
begin
  GetProcedureAddress(_DsGetDcName, netapi32, 'DsGetDcName' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsGetDcName]
  end;
end;

var
  _DsGetSiteNameA: Pointer;

function DsGetSiteNameA;
begin
  GetProcedureAddress(_DsGetSiteNameA, netapi32, 'DsGetSiteNameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsGetSiteNameA]
  end;
end;

var
  _DsGetSiteNameW: Pointer;

function DsGetSiteNameW;
begin
  GetProcedureAddress(_DsGetSiteNameW, netapi32, 'DsGetSiteNameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsGetSiteNameW]
  end;
end;

var
  _DsGetSiteName: Pointer;

function DsGetSiteName;
begin
  GetProcedureAddress(_DsGetSiteName, netapi32, 'DsGetSiteName' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsGetSiteName]
  end;
end;

var
  _DsValidateSubnetNameA: Pointer;

function DsValidateSubnetNameA;
begin
  GetProcedureAddress(_DsValidateSubnetNameA, netapi32, 'DsValidateSubnetNameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsValidateSubnetNameA]
  end;
end;

var
  _DsValidateSubnetNameW: Pointer;

function DsValidateSubnetNameW;
begin
  GetProcedureAddress(_DsValidateSubnetNameW, netapi32, 'DsValidateSubnetNameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsValidateSubnetNameW]
  end;
end;

var
  _DsValidateSubnetName: Pointer;

function DsValidateSubnetName;
begin
  GetProcedureAddress(_DsValidateSubnetName, netapi32, 'DsValidateSubnetName' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsValidateSubnetName]
  end;
end;

var
  _DsAddressToSiteNamesA: Pointer;

function DsAddressToSiteNamesA;
begin
  GetProcedureAddress(_DsAddressToSiteNamesA, netapi32, 'DsAddressToSiteNamesA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsAddressToSiteNamesA]
  end;
end;

var
  _DsAddressToSiteNamesW: Pointer;

function DsAddressToSiteNamesW;
begin
  GetProcedureAddress(_DsAddressToSiteNamesW, netapi32, 'DsAddressToSiteNamesW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsAddressToSiteNamesW]
  end;
end;

var
  _DsAddressToSiteNames: Pointer;

function DsAddressToSiteNames;
begin
  GetProcedureAddress(_DsAddressToSiteNames, netapi32, 'DsAddressToSiteNames' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsAddressToSiteNames]
  end;
end;

var
  _DsAddressToSiteNamesExA: Pointer;

function DsAddressToSiteNamesExA;
begin
  GetProcedureAddress(_DsAddressToSiteNamesExA, netapi32, 'DsAddressToSiteNamesExA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsAddressToSiteNamesExA]
  end;
end;

var
  _DsAddressToSiteNamesExW: Pointer;

function DsAddressToSiteNamesExW;
begin
  GetProcedureAddress(_DsAddressToSiteNamesExW, netapi32, 'DsAddressToSiteNamesExW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsAddressToSiteNamesExW]
  end;
end;

var
  _DsAddressToSiteNamesEx: Pointer;

function DsAddressToSiteNamesEx;
begin
  GetProcedureAddress(_DsAddressToSiteNamesEx, netapi32, 'DsAddressToSiteNamesEx' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsAddressToSiteNamesEx]
  end;
end;

var
  _DsEnumerateDomainTrustsA: Pointer;

function DsEnumerateDomainTrustsA;
begin
  GetProcedureAddress(_DsEnumerateDomainTrustsA, netapi32, 'DsEnumerateDomainTrustsA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsEnumerateDomainTrustsA]
  end;
end;

var
  _DsEnumerateDomainTrustsW: Pointer;

function DsEnumerateDomainTrustsW;
begin
  GetProcedureAddress(_DsEnumerateDomainTrustsW, netapi32, 'DsEnumerateDomainTrustsW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsEnumerateDomainTrustsW]
  end;
end;

var
  _DsEnumerateDomainTrusts: Pointer;

function DsEnumerateDomainTrusts;
begin
  GetProcedureAddress(_DsEnumerateDomainTrusts, netapi32, 'DsEnumerateDomainTrusts' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsEnumerateDomainTrusts]
  end;
end;

var
  _DsGetForestTrustInformationW: Pointer;

function DsGetForestTrustInformationW;
begin
  GetProcedureAddress(_DsGetForestTrustInformationW, netapi32, 'DsGetForestTrustInformationW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsGetForestTrustInformationW]
  end;
end;

var
  _DsMergeForestTrustInformationW: Pointer;

function DsMergeForestTrustInformationW;
begin
  GetProcedureAddress(_DsMergeForestTrustInformationW, netapi32, 'DsMergeForestTrustInformationW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsMergeForestTrustInformationW]
  end;
end;

var
  _DsGetDcSiteCoverageA: Pointer;

function DsGetDcSiteCoverageA;
begin
  GetProcedureAddress(_DsGetDcSiteCoverageA, netapi32, 'DsGetDcSiteCoverageA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsGetDcSiteCoverageA]
  end;
end;

var
  _DsGetDcSiteCoverageW: Pointer;

function DsGetDcSiteCoverageW;
begin
  GetProcedureAddress(_DsGetDcSiteCoverageW, netapi32, 'DsGetDcSiteCoverageW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsGetDcSiteCoverageW]
  end;
end;

var
  _DsGetDcSiteCoverage: Pointer;

function DsGetDcSiteCoverage;
begin
  GetProcedureAddress(_DsGetDcSiteCoverage, netapi32, 'DsGetDcSiteCoverage' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsGetDcSiteCoverage]
  end;
end;

var
  _DsDeregisterDnsHostRecordsA: Pointer;

function DsDeregisterDnsHostRecordsA;
begin
  GetProcedureAddress(_DsDeregisterDnsHostRecordsA, netapi32, 'DsDeregisterDnsHostRecordsA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsDeregisterDnsHostRecordsA]
  end;
end;

var
  _DsDeregisterDnsHostRecordsW: Pointer;

function DsDeregisterDnsHostRecordsW;
begin
  GetProcedureAddress(_DsDeregisterDnsHostRecordsW, netapi32, 'DsDeregisterDnsHostRecordsW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsDeregisterDnsHostRecordsW]
  end;
end;

var
  _DsDeregisterDnsHostRecords: Pointer;

function DsDeregisterDnsHostRecords;
begin
  GetProcedureAddress(_DsDeregisterDnsHostRecords, netapi32, 'DsDeregisterDnsHostRecords' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsDeregisterDnsHostRecords]
  end;
end;

var
  _DsGetDcOpenW: Pointer;

function DsGetDcOpenW;
begin
  GetProcedureAddress(_DsGetDcOpenW, netapi32, 'DsGetDcOpenW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsGetDcOpenW]
  end;
end;

var
  _DsGetDcOpenA: Pointer;

function DsGetDcOpenA;
begin
  GetProcedureAddress(_DsGetDcOpenA, netapi32, 'DsGetDcOpenA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsGetDcOpenA]
  end;
end;

var
  _DsGetDcOpen: Pointer;

function DsGetDcOpen;
begin
  GetProcedureAddress(_DsGetDcOpen, netapi32, 'DsGetDcOpen' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsGetDcOpen]
  end;
end;

var
  _DsGetDcNextW: Pointer;

function DsGetDcNextW;
begin
  GetProcedureAddress(_DsGetDcNextW, netapi32, 'DsGetDcNextW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsGetDcNextW]
  end;
end;

var
  _DsGetDcNextA: Pointer;

function DsGetDcNextA;
begin
  GetProcedureAddress(_DsGetDcNextA, netapi32, 'DsGetDcNextA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsGetDcNextA]
  end;
end;

var
  _DsGetDcNext: Pointer;

function DsGetDcNext;
begin
  GetProcedureAddress(_DsGetDcNext, netapi32, 'DsGetDcNext' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsGetDcNext]
  end;
end;

var
  _DsGetDcCloseW: Pointer;

procedure DsGetDcCloseW;
begin
  GetProcedureAddress(_DsGetDcCloseW, netapi32, 'DsGetDcCloseW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsGetDcCloseW]
  end;
end;

var
  _DsGetDcClose: Pointer;

procedure DsGetDcClose;
begin
  GetProcedureAddress(_DsGetDcClose, netapi32, 'DsGetDcClose');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DsGetDcClose]
  end;
end;

{$ELSE}

function DsGetDcNameA; external netapi32 name 'DsGetDcNameA';
function DsGetDcNameW; external netapi32 name 'DsGetDcNameW';
function DsGetDcName; external netapi32 name 'DsGetDcName' + AWSuffix;
function DsGetSiteNameA; external netapi32 name 'DsGetSiteNameA';
function DsGetSiteNameW; external netapi32 name 'DsGetSiteNameW';
function DsGetSiteName; external netapi32 name 'DsGetSiteName' + AWSuffix;
function DsValidateSubnetNameA; external netapi32 name 'DsValidateSubnetNameA';
function DsValidateSubnetNameW; external netapi32 name 'DsValidateSubnetNameW';
function DsValidateSubnetName; external netapi32 name 'DsValidateSubnetName' + AWSuffix;
function DsAddressToSiteNamesA; external netapi32 name 'DsAddressToSiteNamesA';
function DsAddressToSiteNamesW; external netapi32 name 'DsAddressToSiteNamesW';
function DsAddressToSiteNames; external netapi32 name 'DsAddressToSiteNames' + AWSuffix;
function DsAddressToSiteNamesExA; external netapi32 name 'DsAddressToSiteNamesExA';
function DsAddressToSiteNamesExW; external netapi32 name 'DsAddressToSiteNamesExW';
function DsAddressToSiteNamesEx; external netapi32 name 'DsAddressToSiteNamesEx' + AWSuffix;
function DsEnumerateDomainTrustsA; external netapi32 name 'DsEnumerateDomainTrustsA';
function DsEnumerateDomainTrustsW; external netapi32 name 'DsEnumerateDomainTrustsW';
function DsEnumerateDomainTrusts; external netapi32 name 'DsEnumerateDomainTrusts' + AWSuffix;
function DsGetForestTrustInformationW; external netapi32 name 'DsGetForestTrustInformationW';
function DsMergeForestTrustInformationW; external netapi32 name 'DsMergeForestTrustInformationW';
function DsGetDcSiteCoverageA; external netapi32 name 'DsGetDcSiteCoverageA';
function DsGetDcSiteCoverageW; external netapi32 name 'DsGetDcSiteCoverageW';
function DsGetDcSiteCoverage; external netapi32 name 'DsGetDcSiteCoverage' + AWSuffix;
function DsDeregisterDnsHostRecordsA; external netapi32 name 'DsDeregisterDnsHostRecordsA';
function DsDeregisterDnsHostRecordsW; external netapi32 name 'DsDeregisterDnsHostRecordsW';
function DsDeregisterDnsHostRecords; external netapi32 name 'DsDeregisterDnsHostRecords' + AWSuffix;
function DsGetDcOpenW; external netapi32 name 'DsGetDcOpenW';
function DsGetDcOpenA; external netapi32 name 'DsGetDcOpenA';
function DsGetDcOpen; external netapi32 name 'DsGetDcOpen' + AWSuffix;
function DsGetDcNextW; external netapi32 name 'DsGetDcNextW';
function DsGetDcNextA; external netapi32 name 'DsGetDcNextA';
function DsGetDcNext; external netapi32 name 'DsGetDcNext' + AWSuffix;
procedure DsGetDcCloseW; external netapi32 name 'DsGetDcCloseW';
procedure DsGetDcClose; external netapi32 name 'DsGetDcClose';

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
