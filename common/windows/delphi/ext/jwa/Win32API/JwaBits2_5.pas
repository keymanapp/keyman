{******************************************************************************}
{                                                                              }
{ Background Intelligent Transfer API interface Unit for Object Pascal         }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: bits2_5.h, The initial developer of the                }
{ Pascal code is TUO (http://www.TheUnknownOnes.net).                          }
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
unit JwaBits2_5;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "bits2_5.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}


interface

uses
  JwaWinBase,
  JwaWinType,
  JwaBits,
  JwaBits1_5,
  JwaBits2_0;
  
{$ENDIF JWA_OMIT_SECTIONS}


{$IFNDEF JWA_IMPLEMENTATIONSECTION}
type
  BG_CERT_STORE_LOCATION = (BG_CERT_STORE_LOCATION_CURRENT_USER,
                            BG_CERT_STORE_LOCATION_LOCAL_MACHINE,
                            BG_CERT_STORE_LOCATION_CURRENT_SERVICE,
                            BG_CERT_STORE_LOCATION_SERVICES,
                            BG_CERT_STORE_LOCATION_USERS,
                            BG_CERT_STORE_LOCATION_CURRENT_USER_GROUP_POLICY,
                            BG_CERT_STORE_LOCATION_LOCAL_MACHINE_GROUP_POLICY,
                            BG_CERT_STORE_LOCATION_LOCAL_MACHINE_ENTERPRISE);
  {$EXTERNALSYM BG_CERT_STORE_LOCATION}
  TBgCertStoreLocation = BG_CERT_STORE_LOCATION;
  PBgCertStoreLocation = ^BG_CERT_STORE_LOCATION;

const
  IID_IBackgroundCopyJobHttpOptions : TGUID = '{f1bd1079-9f01-4bdc-8036-f09b70095066}';
  {$EXTERNALSYM IID_IBackgroundCopyJobHttpOptions}

type
  IBackgroundCopyJobHttpOptions = interface(IUnknown)
  ['{f1bd1079-9f01-4bdc-8036-f09b70095066}']
    function SetClientCertificateByID(StoreLocation : TBgCertStoreLocation; StoreName : LPCWSTR; CertHashBlob : PBYTE) : HRESULT; stdcall;
    function SetClientCertificateByName(StoreName : LPCWSTR; SubjectName : LPWSTR) : HRESULT; stdcall;
    function RemoveClientCertificate() : HRESULT; stdcall;
    function GetClientCertificate(out StoreLocation : TBgCertStoreLocation; out StoreName : LPWSTR; out CertHashBlob : PByte; out SubjectName : LPWSTR) : HRESULT; stdcall;
    function SetCustomHeaders(RequestHeaders : LPCWSTR) : HRESULT; stdcall;
    function GetCustomHeaders(out RequestHeaders : LPCWSTR) : HRESULT; stdcall;
    function SetSecurityFlags(Flags : ULONG) : HRESULT; stdcall;
    function GetSecurityFlags(out Flags : ULONG) : HRESULT; stdcall;
  end;
  {$EXTERNALSYM IBackgroundCopyJobHttpOptions}

//---------------------------------------------------------------------------

const
  LIBID_BackgroundCopyManager2_5: GUID = '{03ca98d6-ff5d-49b8-abc6-03dd84127020}';
  {$EXTERNALSYM LIBID_BackgroundCopyManager2_5}
  CLSID_BackgroundCopyManager2_5: GUID = '{03ca98d6-ff5d-49b8-abc6-03dd84127020}';
  {$EXTERNALSYM CLSID_BackgroundCopyManager2_5}

  BG_SSL_ENABLE_CRL_CHECK                     = $0001;
  {$EXTERNALSYM BG_SSL_ENABLE_CRL_CHECK}
  BG_SSL_IGNORE_CERT_CN_INVALID               = $0002;
  {$EXTERNALSYM BG_SSL_IGNORE_CERT_CN_INVALID}
  BG_SSL_IGNORE_CERT_DATE_INVALID             = $0004;
  {$EXTERNALSYM BG_SSL_IGNORE_CERT_DATE_INVALID}
  BG_SSL_IGNORE_UNKNOWN_CA                    = $0008;
  {$EXTERNALSYM BG_SSL_IGNORE_UNKNOWN_CA}
  BG_SSL_IGNORE_CERT_WRONG_USAGE              = $0010;
  {$EXTERNALSYM BG_SSL_IGNORE_CERT_WRONG_USAGE}
  BG_HTTP_REDIRECT_POLICY_MASK                = $0700;
  {$EXTERNALSYM BG_HTTP_REDIRECT_POLICY_MASK}
  BG_HTTP_REDIRECT_POLICY_ALLOW_SILENT        = $0000;
  {$EXTERNALSYM BG_HTTP_REDIRECT_POLICY_ALLOW_SILENT}
  BG_HTTP_REDIRECT_POLICY_ALLOW_REPORT        = $0100;
  {$EXTERNALSYM BG_HTTP_REDIRECT_POLICY_ALLOW_REPORT}
  BG_HTTP_REDIRECT_POLICY_DISALLOW            = $0200;
  {$EXTERNALSYM BG_HTTP_REDIRECT_POLICY_DISALLOW}
  BG_HTTP_REDIRECT_POLICY_ALLOW_HTTPS_TO_HTTP = $0800;
  {$EXTERNALSYM BG_HTTP_REDIRECT_POLICY_ALLOW_HTTPS_TO_HTTP}
  
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
