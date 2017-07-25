{******************************************************************************}
{                                                                              }
{ NCrypt Interface Unit for Object Pascal                     		   		   }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2005 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The initial developer of the original translation is Rudy Velthuis		   }
{                                                                              }
{ Portions created by Rudy Velthuis are Copyright (C) 2005-2008                }
{ All Rights Reserved.                                      				   }
{                                                                              }
{ Adapted for JEDI API Library by Christian Wimmer                             }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{ The original code is: ncrypt.h, released 1992-1999.                 		   }	
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
{                                                                              }
{******************************************************************************}
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaNCrypt;

{$IFDEF DELPHI6_UP}
{$ALIGN 8}
{$ELSE}
{$A+}
//Warning: Record alignment 4
{$ENDIF DELPHI6_UP}

interface

uses
  JwaWinType, JwaWinBase, JwaWinCrypt, JwaBCrypt;

{$HPPEMIT '#include <ncrypt.h>'}

{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_INCLUDEMODE}
type
  {$EXTERNALSYM SECURITY_STATUS}
  SECURITY_STATUS = Longint;
{$ENDIF JWA_INCLUDEMODE}

//
// Microsoft built-in providers.
//

const
  {$EXTERNALSYM MS_KEY_STORAGE_PROVIDER}
  MS_KEY_STORAGE_PROVIDER            = 'Microsoft Software Key Storage Provider';
  {$EXTERNALSYM MS_SMART_CARD_KEY_STORAGE_PROVIDER}
  MS_SMART_CARD_KEY_STORAGE_PROVIDER = 'Microsoft Smart Card Key Storage Provider';

//
// Common algorithm identifiers.
//

  {$EXTERNALSYM NCRYPT_RSA_ALGORITHM}
  NCRYPT_RSA_ALGORITHM            = BCRYPT_RSA_ALGORITHM;
  {$EXTERNALSYM NCRYPT_RSA_SIGN_ALGORITHM}
  NCRYPT_RSA_SIGN_ALGORITHM       = BCRYPT_RSA_SIGN_ALGORITHM;
  {$EXTERNALSYM NCRYPT_DH_ALGORITHM}
  NCRYPT_DH_ALGORITHM             = BCRYPT_DH_ALGORITHM;
  {$EXTERNALSYM NCRYPT_DSA_ALGORITHM}
  NCRYPT_DSA_ALGORITHM            = BCRYPT_DSA_ALGORITHM;
  {$EXTERNALSYM NCRYPT_MD2_ALGORITHM}
  NCRYPT_MD2_ALGORITHM            = BCRYPT_MD2_ALGORITHM;
  {$EXTERNALSYM NCRYPT_MD4_ALGORITHM}
  NCRYPT_MD4_ALGORITHM            = BCRYPT_MD4_ALGORITHM;
  {$EXTERNALSYM NCRYPT_MD5_ALGORITHM}
  NCRYPT_MD5_ALGORITHM            = BCRYPT_MD5_ALGORITHM;
  {$EXTERNALSYM NCRYPT_SHA1_ALGORITHM}
  NCRYPT_SHA1_ALGORITHM           = BCRYPT_SHA1_ALGORITHM;
  {$EXTERNALSYM NCRYPT_SHA256_ALGORITHM}
  NCRYPT_SHA256_ALGORITHM         = BCRYPT_SHA256_ALGORITHM;
  {$EXTERNALSYM NCRYPT_SHA384_ALGORITHM}
  NCRYPT_SHA384_ALGORITHM         = BCRYPT_SHA384_ALGORITHM;
  {$EXTERNALSYM NCRYPT_SHA512_ALGORITHM}
  NCRYPT_SHA512_ALGORITHM         = BCRYPT_SHA512_ALGORITHM;
  {$EXTERNALSYM NCRYPT_ECDSA_P256_ALGORITHM}
  NCRYPT_ECDSA_P256_ALGORITHM     = BCRYPT_ECDSA_P256_ALGORITHM;
  {$EXTERNALSYM NCRYPT_ECDSA_P384_ALGORITHM}
  NCRYPT_ECDSA_P384_ALGORITHM     = BCRYPT_ECDSA_P384_ALGORITHM;
  {$EXTERNALSYM NCRYPT_ECDSA_P521_ALGORITHM}
  NCRYPT_ECDSA_P521_ALGORITHM     = BCRYPT_ECDSA_P521_ALGORITHM;
  {$EXTERNALSYM NCRYPT_ECDH_P256_ALGORITHM}
  NCRYPT_ECDH_P256_ALGORITHM      = BCRYPT_ECDH_P256_ALGORITHM;
  {$EXTERNALSYM NCRYPT_ECDH_P384_ALGORITHM}
  NCRYPT_ECDH_P384_ALGORITHM      = BCRYPT_ECDH_P384_ALGORITHM;
  {$EXTERNALSYM NCRYPT_ECDH_P521_ALGORITHM}
  NCRYPT_ECDH_P521_ALGORITHM      = BCRYPT_ECDH_P521_ALGORITHM;

  {$EXTERNALSYM NCRYPT_KEY_STORAGE_ALGORITHM}
  NCRYPT_KEY_STORAGE_ALGORITHM    = 'KEY_STORAGE';

//
// Interfaces
//

  {$EXTERNALSYM NCRYPT_HASH_INTERFACE}
  NCRYPT_HASH_INTERFACE                   = BCRYPT_HASH_INTERFACE;
  {$EXTERNALSYM NCRYPT_ASYMMETRIC_ENCRYPTION_INTERFACE}
  NCRYPT_ASYMMETRIC_ENCRYPTION_INTERFACE  = BCRYPT_ASYMMETRIC_ENCRYPTION_INTERFACE;

  {$EXTERNALSYM NCRYPT_SECRET_AGREEMENT_INTERFACE}
  NCRYPT_SECRET_AGREEMENT_INTERFACE       = BCRYPT_SECRET_AGREEMENT_INTERFACE;

  {$EXTERNALSYM NCRYPT_SIGNATURE_INTERFACE}
  NCRYPT_SIGNATURE_INTERFACE              = BCRYPT_SIGNATURE_INTERFACE;

  {$EXTERNALSYM NCRYPT_KEY_STORAGE_INTERFACE}
  NCRYPT_KEY_STORAGE_INTERFACE            = $00010001;
  {$EXTERNALSYM NCRYPT_SCHANNEL_INTERFACE}
  NCRYPT_SCHANNEL_INTERFACE               = $00010002;

//
// algorithm groups.
//

  {$EXTERNALSYM NCRYPT_RSA_ALGORITHM_GROUP}
  NCRYPT_RSA_ALGORITHM_GROUP      = NCRYPT_RSA_ALGORITHM;
  {$EXTERNALSYM NCRYPT_DH_ALGORITHM_GROUP}
  NCRYPT_DH_ALGORITHM_GROUP       = NCRYPT_DH_ALGORITHM;
  {$EXTERNALSYM NCRYPT_DSA_ALGORITHM_GROUP}
  NCRYPT_DSA_ALGORITHM_GROUP      = NCRYPT_DSA_ALGORITHM;
  {$EXTERNALSYM NCRYPT_ECDSA_ALGORITHM_GROUP}
  NCRYPT_ECDSA_ALGORITHM_GROUP    = 'ECDSA';
  {$EXTERNALSYM NCRYPT_ECDH_ALGORITHM_GROUP}
  NCRYPT_ECDH_ALGORITHM_GROUP     = 'ECDH';

//
// NCrypt generic memory descriptors
//

  {$EXTERNALSYM NCRYPTBUFFER_VERSION}
  NCRYPTBUFFER_VERSION                = 0;

  {$EXTERNALSYM NCRYPTBUFFER_EMPTY}
  NCRYPTBUFFER_EMPTY                  = 0;
  {$EXTERNALSYM NCRYPTBUFFER_DATA}
  NCRYPTBUFFER_DATA                   = 1;
  {$EXTERNALSYM NCRYPTBUFFER_SSL_CLIENT_RANDOM}
  NCRYPTBUFFER_SSL_CLIENT_RANDOM      = 20;
  {$EXTERNALSYM NCRYPTBUFFER_SSL_SERVER_RANDOM}
  NCRYPTBUFFER_SSL_SERVER_RANDOM      = 21;
  {$EXTERNALSYM NCRYPTBUFFER_SSL_HIGHEST_VERSION}
  NCRYPTBUFFER_SSL_HIGHEST_VERSION    = 22;
  {$EXTERNALSYM NCRYPTBUFFER_SSL_CLEAR_KEY}
  NCRYPTBUFFER_SSL_CLEAR_KEY          = 23;
  {$EXTERNALSYM NCRYPTBUFFER_SSL_KEY_ARG_DATA}
  NCRYPTBUFFER_SSL_KEY_ARG_DATA       = 24;

  {$EXTERNALSYM NCRYPTBUFFER_PKCS_OID}
  NCRYPTBUFFER_PKCS_OID               = 40;
  {$EXTERNALSYM NCRYPTBUFFER_PKCS_ALG_OID}
  NCRYPTBUFFER_PKCS_ALG_OID           = 41;
  {$EXTERNALSYM NCRYPTBUFFER_PKCS_ALG_PARAM}
  NCRYPTBUFFER_PKCS_ALG_PARAM         = 42;
  {$EXTERNALSYM NCRYPTBUFFER_PKCS_ALG_ID}
  NCRYPTBUFFER_PKCS_ALG_ID            = 43;
  {$EXTERNALSYM NCRYPTBUFFER_PKCS_ATTRS}
  NCRYPTBUFFER_PKCS_ATTRS             = 44;
  {$EXTERNALSYM NCRYPTBUFFER_PKCS_KEY_NAME}
  NCRYPTBUFFER_PKCS_KEY_NAME          = 45;
  {$EXTERNALSYM NCRYPTBUFFER_PKCS_SECRET}
  NCRYPTBUFFER_PKCS_SECRET            = 46;

  {$EXTERNALSYM NCRYPTBUFFER_CERT_BLOB}
  NCRYPTBUFFER_CERT_BLOB              = 47;

// NCRYPT shares the same BCRYPT definitions
type
  {$EXTERNALSYM NCryptBuffer}
  NCryptBuffer = BCryptBuffer;
  TNCryptBuffer = BCryptBuffer;
  {$EXTERNALSYM PNCryptBuffer}
  PNCryptBuffer = PBCryptBuffer;
  {$EXTERNALSYM NCryptBufferDesc}
  NCryptBufferDesc = BCryptBufferDesc;
  TNCryptBufferDesc = BCryptBufferDesc;
  {$EXTERNALSYM PNCryptBufferDesc}
  PNCryptBufferDesc = PBCryptBufferDesc;

//
// NCrypt handles
//

  {$EXTERNALSYM NCRYPT_HANDLE}
  NCRYPT_HANDLE = ULONG_PTR;
  {$EXTERNALSYM NCRYPT_PROV_HANDLE}
  NCRYPT_PROV_HANDLE = ULONG_PTR;
  {$EXTERNALSYM NCRYPT_KEY_HANDLE}
  NCRYPT_KEY_HANDLE = ULONG_PTR;
  {$EXTERNALSYM NCRYPT_HASH_HANDLE}
  NCRYPT_HASH_HANDLE = ULONG_PTR;
  {$EXTERNALSYM NCRYPT_SECRET_HANDLE}
  NCRYPT_SECRET_HANDLE = ULONG_PTR;

//
// NCrypt API Flags
//

const
  {$EXTERNALSYM NCRYPT_NO_PADDING_FLAG}
  NCRYPT_NO_PADDING_FLAG      = BCRYPT_PAD_NONE;
  {$EXTERNALSYM NCRYPT_PAD_PKCS1_FLAG}
  NCRYPT_PAD_PKCS1_FLAG       = BCRYPT_PAD_PKCS1;  // NCryptEncrypt/Decrypt NCryptSignHash/VerifySignature
  {$EXTERNALSYM NCRYPT_PAD_OAEP_FLAG}
  NCRYPT_PAD_OAEP_FLAG        = BCRYPT_PAD_OAEP;   // BCryptEncrypt/Decrypt
  {$EXTERNALSYM NCRYPT_PAD_PSS_FLAG}
  NCRYPT_PAD_PSS_FLAG         = BCRYPT_PAD_PSS;    // BCryptSignHash/VerifySignature
  {$EXTERNALSYM NCRYPT_NO_KEY_VALIDATION}
  NCRYPT_NO_KEY_VALIDATION    = BCRYPT_NO_KEY_VALIDATION;
  {$EXTERNALSYM NCRYPT_MACHINE_KEY_FLAG}
  NCRYPT_MACHINE_KEY_FLAG                 = $00000020;  // same as CAPI CRYPT_MACHINE_KEYSET
  {$EXTERNALSYM NCRYPT_SILENT_FLAG}
  NCRYPT_SILENT_FLAG                      = $00000040;  // same as CAPI CRYPT_SILENT
  {$EXTERNALSYM NCRYPT_OVERWRITE_KEY_FLAG}
  NCRYPT_OVERWRITE_KEY_FLAG               = $00000080;
  {$EXTERNALSYM NCRYPT_WRITE_KEY_TO_LEGACY_STORE_FLAG}
  NCRYPT_WRITE_KEY_TO_LEGACY_STORE_FLAG   = $00000200;
  {$EXTERNALSYM NCRYPT_DO_NOT_FINALIZE_FLAG}
  NCRYPT_DO_NOT_FINALIZE_FLAG             = $00000400;
  {$EXTERNALSYM NCRYPT_PERSIST_ONLY_FLAG}
  NCRYPT_PERSIST_ONLY_FLAG                = $40000000;
  {$EXTERNALSYM NCRYPT_PERSIST_FLAG}
  NCRYPT_PERSIST_FLAG                     = $80000000;
  {$EXTERNALSYM NCRYPT_REGISTER_NOTIFY_FLAG}
  NCRYPT_REGISTER_NOTIFY_FLAG             = $00000001;
  {$EXTERNALSYM NCRYPT_UNREGISTER_NOTIFY_FLAG}
  NCRYPT_UNREGISTER_NOTIFY_FLAG           = $00000002;

//
// Functions used to manage persisted keys.
//

{$EXTERNALSYM NCryptOpenStorageProvider}
function NCryptOpenStorageProvider(out phProvider: NCRYPT_PROV_HANDLE;
  pszProviderName: LPCWSTR; dwFlags: DWORD): SECURITY_STATUS; stdcall;

const
// AlgOperations flags for use with NCryptEnumAlgorithms()
  {$EXTERNALSYM NCRYPT_CIPHER_OPERATION}
  NCRYPT_CIPHER_OPERATION                 = BCRYPT_CIPHER_OPERATION;
  {$EXTERNALSYM NCRYPT_HASH_OPERATION}
  NCRYPT_HASH_OPERATION                   = BCRYPT_HASH_OPERATION;
  {$EXTERNALSYM NCRYPT_ASYMMETRIC_ENCRYPTION_OPERATION}
  NCRYPT_ASYMMETRIC_ENCRYPTION_OPERATION  = BCRYPT_ASYMMETRIC_ENCRYPTION_OPERATION;
  {$EXTERNALSYM NCRYPT_SECRET_AGREEMENT_OPERATION}
  NCRYPT_SECRET_AGREEMENT_OPERATION       = BCRYPT_SECRET_AGREEMENT_OPERATION;
  {$EXTERNALSYM NCRYPT_SIGNATURE_OPERATION}
  NCRYPT_SIGNATURE_OPERATION              = BCRYPT_SIGNATURE_OPERATION;
  {$EXTERNALSYM NCRYPT_RNG_OPERATION}
  NCRYPT_RNG_OPERATION                    = BCRYPT_RNG_OPERATION;

// USE EXTREME CAUTION: editing comments that contain "certenrolls_*" tokens
// could break building CertEnroll idl files:
// certenrolls_begin -- NCryptAlgorithmName
type
  PPNCryptAlgorithmName = ^PNCryptAlgorithmName;
  PNCryptAlgorithmName = ^TNCryptAlgorithmName;
  {$EXTERNALSYM _NCryptAlgorithmName}
  _NCryptAlgorithmName = record
    pszName: LPWSTR;
    dwClass: DWORD;            // the CNG interface that supports this algorithm
    dwAlgOperations: DWORD;    // the types of operations supported by this algorithm
    dwFlags: DWORD;
  end;
  {$EXTERNALSYM NCryptAlgorithmName}
  NCryptAlgorithmName = _NCryptAlgorithmName;
  TNCryptAlgorithmName = _NCryptAlgorithmName;
// certenrolls_end

{$EXTERNALSYM NCryptEnumAlgorithms}
function NCryptEnumAlgorithms(hProvider: NCRYPT_PROV_HANDLE;
  dwAlgOperations: DWORD; out pdwAlgCount: DWORD;
  ppAlgList: PPNCryptAlgorithmName; dwFlags: DWORD): SECURITY_STATUS; stdcall;

{$EXTERNALSYM NCryptIsAlgSupported}
function NCryptIsAlgSupported(hProvider: NCRYPT_PROV_HANDLE; pszAlgId: LPCWSTR;
  dwFlags: DWORD): SECURITY_STATUS; stdcall;

type
  PNCryptKeyName = ^TNCryptKeyName;
  {$EXTERNALSYM NCryptKeyName}
  NCryptKeyName = record
    pszName: LPWSTR;
    pszAlgid: LPWSTR;
    dwLegacyKeySpec: DWORD;
    dwFlags: DWORD;
  end;
  TNCryptKeyName = NCryptKeyName;

{$EXTERNALSYM NCryptEnumKeys}
function NCryptEnumKeys(hProvider: NCRYPT_PROV_HANDLE; pszScope: LPCWSTR;
  out ppKeyName: PNCryptKeyName; var ppEnumState: Pointer;
  dwFlags: DWORD): SECURITY_STATUS; stdcall;

type
  PNCryptProviderName = ^TNCryptProviderName;
  {$EXTERNALSYM NCryptProviderName}
  NCryptProviderName = record
    pszName: LPWSTR;
    pszComment: LPWSTR;
  end;
  TNCryptProviderName = NCryptProviderName;

{$EXTERNALSYM NCryptEnumStorageProviders}
function NCryptEnumStorageProviders(out pdwProviderCount: DWORD;
  out ppProviderList: PNCryptProviderName;
  dwFlags: DWORD): SECURITY_STATUS; stdcall;

{$EXTERNALSYM NCryptFreeBuffer}
function NCryptFreeBuffer(pvInput: Pointer): SECURITY_STATUS; stdcall;

{$EXTERNALSYM NCryptOpenKey}
function NCryptOpenKey(hProvider: NCRYPT_PROV_HANDLE;
  out phKey: NCRYPT_KEY_HANDLE; pszKeyName: LPCWSTR;
  dwLegacyKeySpec, dwFlags: DWORD): SECURITY_STATUS; stdcall;

{$EXTERNALSYM NCryptCreatePersistedKey}
function NCryptCreatePersistedKey(hProvider: NCRYPT_PROV_HANDLE;
  out phKey: NCRYPT_KEY_HANDLE; pszAlgId, pszKeyName: LPCWSTR;
  dwLegacyKeySpec, dwFlags: DWORD): SECURITY_STATUS; stdcall;

const
// Standard property names.
  {$EXTERNALSYM NCRYPT_NAME_PROPERTY}
  NCRYPT_NAME_PROPERTY                    = 'Name';
  {$EXTERNALSYM NCRYPT_UNIQUE_NAME_PROPERTY}
  NCRYPT_UNIQUE_NAME_PROPERTY             = 'Unique Name';
  {$EXTERNALSYM NCRYPT_ALGORITHM_PROPERTY}
  NCRYPT_ALGORITHM_PROPERTY               = 'Algorithm Name';
  {$EXTERNALSYM NCRYPT_LENGTH_PROPERTY}
  NCRYPT_LENGTH_PROPERTY                  = 'Length';
  {$EXTERNALSYM NCRYPT_LENGTHS_PROPERTY}
  NCRYPT_LENGTHS_PROPERTY                 = 'Lengths';
  {$EXTERNALSYM NCRYPT_BLOCK_LENGTH_PROPERTY}
  NCRYPT_BLOCK_LENGTH_PROPERTY            = 'Block Length';
  {$EXTERNALSYM NCRYPT_UI_POLICY_PROPERTY}
  NCRYPT_UI_POLICY_PROPERTY               = 'UI Policy';
  {$EXTERNALSYM NCRYPT_EXPORT_POLICY_PROPERTY}
  NCRYPT_EXPORT_POLICY_PROPERTY           = 'Export Policy';
  {$EXTERNALSYM NCRYPT_WINDOW_HANDLE_PROPERTY}
  NCRYPT_WINDOW_HANDLE_PROPERTY           = 'HWND Handle';
  {$EXTERNALSYM NCRYPT_USE_CONTEXT_PROPERTY}
  NCRYPT_USE_CONTEXT_PROPERTY             = 'Use Context';
  {$EXTERNALSYM NCRYPT_IMPL_TYPE_PROPERTY}
  NCRYPT_IMPL_TYPE_PROPERTY               = 'Impl Type';
  {$EXTERNALSYM NCRYPT_KEY_USAGE_PROPERTY}
  NCRYPT_KEY_USAGE_PROPERTY               = 'Key Usage';
  {$EXTERNALSYM NCRYPT_KEY_TYPE_PROPERTY}
  NCRYPT_KEY_TYPE_PROPERTY                = 'Key Type';
  {$EXTERNALSYM NCRYPT_VERSION_PROPERTY}
  NCRYPT_VERSION_PROPERTY                 = 'Version';
  {$EXTERNALSYM NCRYPT_SECURITY_DESCR_SUPPORT_PROPERTY}
  NCRYPT_SECURITY_DESCR_SUPPORT_PROPERTY  = 'Security Descr Support';
  {$EXTERNALSYM NCRYPT_SECURITY_DESCR_PROPERTY}
  NCRYPT_SECURITY_DESCR_PROPERTY          = 'Security Descr';
  {$EXTERNALSYM NCRYPT_USE_COUNT_ENABLED_PROPERTY}
  NCRYPT_USE_COUNT_ENABLED_PROPERTY       = 'Enabled Use Count';
  {$EXTERNALSYM NCRYPT_USE_COUNT_PROPERTY}
  NCRYPT_USE_COUNT_PROPERTY               = 'Use Count';
  {$EXTERNALSYM NCRYPT_LAST_MODIFIED_PROPERTY}
  NCRYPT_LAST_MODIFIED_PROPERTY           = 'Modified';
  {$EXTERNALSYM NCRYPT_MAX_NAME_LENGTH_PROPERTY}
  NCRYPT_MAX_NAME_LENGTH_PROPERTY         = 'Max Name Length';
  {$EXTERNALSYM NCRYPT_ALGORITHM_GROUP_PROPERTY}
  NCRYPT_ALGORITHM_GROUP_PROPERTY         = 'Algorithm Group';
  {$EXTERNALSYM NCRYPT_DH_PARAMETERS_PROPERTY}
  NCRYPT_DH_PARAMETERS_PROPERTY           = BCRYPT_DH_PARAMETERS;
  {$EXTERNALSYM NCRYPT_PROVIDER_HANDLE_PROPERTY}
  NCRYPT_PROVIDER_HANDLE_PROPERTY         = 'Provider Handle';
  {$EXTERNALSYM NCRYPT_PIN_PROPERTY}
  NCRYPT_PIN_PROPERTY                     = 'SmartCardPin';
  {$EXTERNALSYM NCRYPT_READER_PROPERTY}
  NCRYPT_READER_PROPERTY                  = 'SmartCardReader';
  {$EXTERNALSYM NCRYPT_SMARTCARD_GUID_PROPERTY}
  NCRYPT_SMARTCARD_GUID_PROPERTY          = 'SmartCardGuid';
  {$EXTERNALSYM NCRYPT_CERTIFICATE_PROPERTY}
  NCRYPT_CERTIFICATE_PROPERTY             = 'SmartCardKeyCertificate';
  {$EXTERNALSYM NCRYPT_PIN_PROMPT_PROPERTY}
  NCRYPT_PIN_PROMPT_PROPERTY              = 'SmartCardPinPrompt';
  {$EXTERNALSYM NCRYPT_USER_CERTSTORE_PROPERTY}
  NCRYPT_USER_CERTSTORE_PROPERTY          = 'SmartCardUserCertStore';
  {$EXTERNALSYM NCRYPT_ROOT_CERTSTORE_PROPERTY}
  NCRYPT_ROOT_CERTSTORE_PROPERTY          = 'SmartcardRootCertStore';

// Maximum length of property name (in characters)
  {$EXTERNALSYM NCRYPT_MAX_PROPERTY_NAME}
  NCRYPT_MAX_PROPERTY_NAME        = 64;

// Maximum length of property data (in bytes)
  {$EXTERNALSYM NCRYPT_MAX_PROPERTY_DATA}
  NCRYPT_MAX_PROPERTY_DATA        = $100000;

// NCRYPT_EXPORT_POLICY_PROPERTY property flags.
  {$EXTERNALSYM NCRYPT_ALLOW_EXPORT_FLAG}
  NCRYPT_ALLOW_EXPORT_FLAG                = $00000001;
  {$EXTERNALSYM NCRYPT_ALLOW_PLAINTEXT_EXPORT_FLAG}
  NCRYPT_ALLOW_PLAINTEXT_EXPORT_FLAG      = $00000002;
  {$EXTERNALSYM NCRYPT_ALLOW_ARCHIVING_FLAG}
  NCRYPT_ALLOW_ARCHIVING_FLAG             = $00000004;
  {$EXTERNALSYM NCRYPT_ALLOW_PLAINTEXT_ARCHIVING_FLAG}
  NCRYPT_ALLOW_PLAINTEXT_ARCHIVING_FLAG   = $00000008;

// NCRYPT_IMPL_TYPE_PROPERTY property flags.
  {$EXTERNALSYM NCRYPT_IMPL_HARDWARE_FLAG}
  NCRYPT_IMPL_HARDWARE_FLAG               = $00000001;
  {$EXTERNALSYM NCRYPT_IMPL_SOFTWARE_FLAG}
  NCRYPT_IMPL_SOFTWARE_FLAG               = $00000002;
  {$EXTERNALSYM NCRYPT_IMPL_REMOVABLE_FLAG}
  NCRYPT_IMPL_REMOVABLE_FLAG              = $00000008;
  {$EXTERNALSYM NCRYPT_IMPL_HARDWARE_RNG_FLAG}
  NCRYPT_IMPL_HARDWARE_RNG_FLAG           = $00000010;

// NCRYPT_KEY_USAGE_PROPERTY property flags.
  {$EXTERNALSYM NCRYPT_ALLOW_DECRYPT_FLAG}
  NCRYPT_ALLOW_DECRYPT_FLAG               = $00000001;
  {$EXTERNALSYM NCRYPT_ALLOW_SIGNING_FLAG}
  NCRYPT_ALLOW_SIGNING_FLAG               = $00000002;
  {$EXTERNALSYM NCRYPT_ALLOW_KEY_AGREEMENT_FLAG}
  NCRYPT_ALLOW_KEY_AGREEMENT_FLAG         = $00000004;
  {$EXTERNALSYM NCRYPT_ALLOW_ALL_USAGES}
  NCRYPT_ALLOW_ALL_USAGES                 = $00ffffff;

// NCRYPT_UI_POLICY_PROPERTY property flags and structure
  {$EXTERNALSYM NCRYPT_UI_PROTECT_KEY_FLAG}
  NCRYPT_UI_PROTECT_KEY_FLAG              = $00000001;
  {$EXTERNALSYM NCRYPT_UI_FORCE_HIGH_PROTECTION_FLAG}
  NCRYPT_UI_FORCE_HIGH_PROTECTION_FLAG    = $00000002;

type
  PNCryptUIPolicyBlob = ^TNCryptUIPolicyBlob;
  {$EXTERNALSYM __NCRYPT_UI_POLICY_BLOB}
  __NCRYPT_UI_POLICY_BLOB = record
    dwVersion: DWORD;
    dwFlags: DWORD;
    cbCreationTitle: DWORD;
    cbFriendlyName: DWORD;
    cbDescription: DWORD;
    // creation title string
    // friendly name string
    // description string
  end;
  {$EXTERNALSYM NCRYPT_UI_POLICY_BLOB}
  NCRYPT_UI_POLICY_BLOB = __NCRYPT_UI_POLICY_BLOB;
  TNCryptUIPolicyBlob = __NCRYPT_UI_POLICY_BLOB;

  PNCryptUIPolicy = ^TNCryptUIPolicy;
  {$EXTERNALSYM __NCRYPT_UI_POLICY}
  __NCRYPT_UI_POLICY = record
    dwVersion: DWORD;
    dwFlags: DWORD;
    pszCreationTitle: LPCWSTR;
    pszFriendlyName: LPCWSTR;
    pszDescription: LPCWSTR;
  end;
  {$EXTERNALSYM NCRYPT_UI_POLICY}
  NCRYPT_UI_POLICY = __NCRYPT_UI_POLICY;
  TNCryptUIPolicy = __NCRYPT_UI_POLICY;


// NCRYPT_LENGTHS_PROPERTY property structure.
  PNCryptSupportedLengths = ^TNCryptSupportedLengths;
  {$EXTERNALSYM __NCRYPT_SUPPORTED_LENGTHS}
  __NCRYPT_SUPPORTED_LENGTHS = record
    dwMinLength: DWORD;
    dwMaxLength: DWORD;
    dwIncrement: DWORD;
    dwDefaultLength: DWORD;
  end;
  {$EXTERNALSYM NCRYPT_SUPPORTED_LENGTHS}
  NCRYPT_SUPPORTED_LENGTHS = __NCRYPT_SUPPORTED_LENGTHS;
  TNCryptSupportedLengths = __NCRYPT_SUPPORTED_LENGTHS;

{$EXTERNALSYM NCryptGetProperty}
function NCryptGetProperty(hObject: NCRYPT_HANDLE; pszProperty: LPCWSTR;
  pbOutput: PBYTE; cbOutput: DWORD; out pcbResult: DWORD;
  dwFlags: DWORD): SECURITY_STATUS; stdcall;

{$EXTERNALSYM NCryptSetProperty}
function NCryptSetProperty(hObject: NCRYPT_HANDLE; pszProperty: LPCWSTR;
  pbInput: PBYTE; cbInput, dwFlags: DWORD): SECURITY_STATUS; stdcall;

{$EXTERNALSYM NCryptFinalizeKey}
function NCryptFinalizeKey(hKey: NCRYPT_KEY_HANDLE;
  dwFlags: DWORD): SECURITY_STATUS; stdcall;

{$EXTERNALSYM NCryptEncrypt}
function NCryptEncrypt(hKey: NCRYPT_KEY_HANDLE; pbInput: PBYTE;
  cbInput: DWORD; pPaddingInfo: Pointer; pbOutput: PBYTE;
  cbOutput: DWORD; out pcbResult: DWORD;
  dwFlags: DWORD): SECURITY_STATUS; stdcall;

{$EXTERNALSYM NCryptDecrypt}
function NCryptDecrypt(hKey: NCRYPT_KEY_HANDLE; pbInput: PBYTE;
  cbInput: DWORD; pPaddingInfo: Pointer; pbOutput: PBYTE;
  cbOutput: DWORD; out pcbResult: DWORD;
  dwFlags: DWORD): SECURITY_STATUS; stdcall;

const
  {$EXTERNALSYM NCRYPT_PKCS7_ENVELOPE_BLOB}
  NCRYPT_PKCS7_ENVELOPE_BLOB      = 'PKCS7_ENVELOPE';
  {$EXTERNALSYM NCRYPT_PKCS8_PRIVATE_KEY_BLOB}
  NCRYPT_PKCS8_PRIVATE_KEY_BLOB   = 'PKCS8_PRIVATEKEY';
  {$EXTERNALSYM NCRYPT_OPAQUETRANSPORT_BLOB}
  NCRYPT_OPAQUETRANSPORT_BLOB     = 'OpaqueTransport';

{$EXTERNALSYM NCryptImportKey}
function NCryptImportKey(hProvider: NCRYPT_PROV_HANDLE;
  hImportKey: NCRYPT_KEY_HANDLE; pszBlobType: LPCWSTR;
  pParameterList: PNCryptBufferDesc; out phKey: NCRYPT_KEY_HANDLE;
  pbData: PBYTE; cbData, dwFlags: DWORD): SECURITY_STATUS; stdcall;

{$EXTERNALSYM NCryptExportKey}
function NCryptExportKey(hKey, hExportKey: NCRYPT_KEY_HANDLE;
  pszBlobType: LPCWSTR; pParameterList: PNCryptBufferDesc;
  pbOutput: PBYTE; cbOutput: DWORD; out pcbResult: DWORD;
  dwFlags: DWORD): SECURITY_STATUS; stdcall;

{$EXTERNALSYM NCryptSignHash}
function NCryptSignHash(hKey: NCRYPT_KEY_HANDLE; pPaddingInfo: Pointer;
  pbHashValue: PBYTE; cbHashValue: DWORD; pbSignature: PBYTE;
  cbSignature: DWORD; out pcbResult: DWORD;
  dwFlags: DWORD): SECURITY_STATUS; stdcall;

{$EXTERNALSYM NCryptVerifySignature}
function NCryptVerifySignature(hKey: NCRYPT_KEY_HANDLE; pPaddingInfo: Pointer;
  pbHashValue: PBYTE; cbHashValue: DWORD; pbSignature: PBYTE;
  cbSignature, dwFlags: DWORD): SECURITY_STATUS; stdcall;

{$EXTERNALSYM NCryptDeleteKey}
function NCryptDeleteKey(hKey: NCRYPT_KEY_HANDLE;
  dwFlags: DWORD): SECURITY_STATUS; stdcall;

{$EXTERNALSYM NCryptFreeObject}
function NCryptFreeObject(hObject: NCRYPT_HANDLE): SECURITY_STATUS; stdcall;

{$EXTERNALSYM NCryptIsKeyHandle}
function NCryptIsKeyHandle(hKey: NCRYPT_KEY_HANDLE): BOOL; stdcall;

{$EXTERNALSYM NCryptTranslateHandle}
function NCryptTranslateHandle(out phProvider: NCRYPT_PROV_HANDLE;
  out phKey: NCRYPT_KEY_HANDLE; hLegacyProv: ULONG_PTR {HCRYPTPROV};
  hLegacyKey: ULONG_PTR {HCRYPTKEY};
  dwLegacyKeySpec, dwFlags: DWORD): SECURITY_STATUS; stdcall;

{$EXTERNALSYM NCryptNotifyChangeKey}
function NCryptNotifyChangeKey(hProvider: NCRYPT_PROV_HANDLE;
  var phEvent: THANDLE; dwFlags: DWORD): SECURITY_STATUS; stdcall;

{$EXTERNALSYM NCryptSecretAgreement}
function NCryptSecretAgreement(hPrivKey, hPubKey: NCRYPT_KEY_HANDLE;
  out phAgreedSecret: NCRYPT_SECRET_HANDLE;
  dwFlags: DWORD): SECURITY_STATUS; stdcall;

function NCryptDeriveKey(hSharedSecret: NCRYPT_SECRET_HANDLE;
  pwszKDF: LPCWSTR; pParameterList: PNCryptBufferDesc;
  pbDerivedKey: PBYTE; cbDerivedKey: DWORD; out pcbResult: DWORD;
  dwFlags: ULONG): SECURITY_STATUS; stdcall;

const
  {$EXTERNALSYM NCRYPT_KEY_STORAGE_INTERFACE_VERSION}
  NCRYPT_KEY_STORAGE_INTERFACE_VERSION: TBCryptInterfaceVersion =
    (MajorVersion: 1; MinorVersion: 0);

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
uses JwaWinDLLNames;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF DYNAMIC_LINK}
function NCryptOpenStorageProvider; external ncryptdll name 'NCryptOpenStorageProvider';
function NCryptEnumAlgorithms; external ncryptdll name 'NCryptEnumAlgorithms';
function NCryptIsAlgSupported; external ncryptdll name 'NCryptIsAlgSupported';
function NCryptEnumKeys; external ncryptdll name 'NCryptEnumKeys';
function NCryptEnumStorageProviders; external ncryptdll name 'NCryptEnumStorageProviders';
function NCryptFreeBuffer; external ncryptdll name 'NCryptFreeBuffer';
function NCryptOpenKey; external ncryptdll name 'NCryptOpenKey';
function NCryptCreatePersistedKey; external ncryptdll name 'NCryptCreatePersistedKey';
function NCryptGetProperty; external ncryptdll name 'NCryptGetProperty';
function NCryptSetProperty; external ncryptdll name 'NCryptSetProperty';
function NCryptFinalizeKey; external ncryptdll name 'NCryptFinalizeKey';
function NCryptEncrypt; external ncryptdll name 'NCryptEncrypt';
function NCryptDecrypt; external ncryptdll name 'NCryptDecrypt';
function NCryptImportKey; external ncryptdll name 'NCryptImportKey';
function NCryptExportKey; external ncryptdll name 'NCryptExportKey';
function NCryptSignHash; external ncryptdll name 'NCryptSignHash';
function NCryptVerifySignature; external ncryptdll name 'NCryptVerifySignature';
function NCryptDeleteKey; external ncryptdll name 'NCryptDeleteKey';
function NCryptFreeObject; external ncryptdll name 'NCryptFreeObject';
function NCryptIsKeyHandle; external ncryptdll name 'NCryptIsKeyHandle';
function NCryptTranslateHandle; external ncryptdll name 'NCryptTranslateHandle';
function NCryptNotifyChangeKey; external ncryptdll name 'NCryptNotifyChangeKey';
function NCryptSecretAgreement; external ncryptdll name 'NCryptSecretAgreement';
function NCryptDeriveKey; external ncryptdll name 'NCryptDeriveKey';

{$ELSE}

var
  _NCryptOpenStorageProvider: Pointer;

function NCryptOpenStorageProvider;
begin
  GetProcedureAddress(_NCryptOpenStorageProvider, ncryptdll, 'NCryptOpenStorageProvider');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NCryptOpenStorageProvider]
  end;
end;

var
  _NCryptEnumAlgorithms: Pointer;

function NCryptEnumAlgorithms;
begin
  GetProcedureAddress(_NCryptEnumAlgorithms, ncryptdll, 'NCryptEnumAlgorithms');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NCryptEnumAlgorithms]
  end;
end;

var
  _NCryptIsAlgSupported: Pointer;

function NCryptIsAlgSupported;
begin
  GetProcedureAddress(_NCryptIsAlgSupported, ncryptdll, 'NCryptIsAlgSupported');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NCryptIsAlgSupported]
  end;
end;

var
  _NCryptEnumKeys: Pointer;

function NCryptEnumKeys;
begin
  GetProcedureAddress(_NCryptEnumKeys, ncryptdll, 'NCryptEnumKeys');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NCryptEnumKeys]
  end;
end;

var
  _NCryptEnumStorageProviders: Pointer;

function NCryptEnumStorageProviders;
begin
  GetProcedureAddress(_NCryptEnumStorageProviders, ncryptdll, 'NCryptEnumStorageProviders');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NCryptEnumStorageProviders]
  end;
end;

var
  _NCryptFreeBuffer: Pointer;

function NCryptFreeBuffer;
begin
  GetProcedureAddress(_NCryptFreeBuffer, ncryptdll, 'NCryptFreeBuffer');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NCryptFreeBuffer]
  end;
end;

var
  _NCryptOpenKey: Pointer;

function NCryptOpenKey;
begin
  GetProcedureAddress(_NCryptOpenKey, ncryptdll, 'NCryptOpenKey');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NCryptOpenKey]
  end;
end;

var
  _NCryptCreatePersistedKey: Pointer;

function NCryptCreatePersistedKey;
begin
  GetProcedureAddress(_NCryptCreatePersistedKey, ncryptdll, 'NCryptCreatePersistedKey');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NCryptCreatePersistedKey]
  end;
end;

var
  _NCryptGetProperty: Pointer;

function NCryptGetProperty;
begin
  GetProcedureAddress(_NCryptGetProperty, ncryptdll, 'NCryptGetProperty');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NCryptGetProperty]
  end;
end;

var
  _NCryptSetProperty: Pointer;

function NCryptSetProperty;
begin
  GetProcedureAddress(_NCryptSetProperty, ncryptdll, 'NCryptSetProperty');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NCryptSetProperty]
  end;
end;

var
  _NCryptFinalizeKey: Pointer;

function NCryptFinalizeKey;
begin
  GetProcedureAddress(_NCryptFinalizeKey, ncryptdll, 'NCryptFinalizeKey');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NCryptFinalizeKey]
  end;
end;

var
  _NCryptEncrypt: Pointer;

function NCryptEncrypt;
begin
  GetProcedureAddress(_NCryptEncrypt, ncryptdll, 'NCryptEncrypt');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NCryptEncrypt]
  end;
end;

var
  _NCryptDecrypt: Pointer;

function NCryptDecrypt;
begin
  GetProcedureAddress(_NCryptDecrypt, ncryptdll, 'NCryptDecrypt');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NCryptDecrypt]
  end;
end;

var
  _NCryptImportKey: Pointer;

function NCryptImportKey;
begin
  GetProcedureAddress(_NCryptImportKey, ncryptdll, 'NCryptImportKey');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NCryptImportKey]
  end;
end;

var
  _NCryptExportKey: Pointer;

function NCryptExportKey;
begin
  GetProcedureAddress(_NCryptExportKey, ncryptdll, 'NCryptExportKey');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NCryptExportKey]
  end;
end;

var
  _NCryptSignHash: Pointer;

function NCryptSignHash;
begin
  GetProcedureAddress(_NCryptSignHash, ncryptdll, 'NCryptSignHash');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NCryptSignHash]
  end;
end;

var
  _NCryptVerifySignature: Pointer;

function NCryptVerifySignature;
begin
  GetProcedureAddress(_NCryptVerifySignature, ncryptdll, 'NCryptVerifySignature');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NCryptVerifySignature]
  end;
end;

var
  _NCryptDeleteKey: Pointer;

function NCryptDeleteKey;
begin
  GetProcedureAddress(_NCryptDeleteKey, ncryptdll, 'NCryptDeleteKey');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NCryptDeleteKey]
  end;
end;

var
  _NCryptFreeObject: Pointer;

function NCryptFreeObject;
begin
  GetProcedureAddress(_NCryptFreeObject, ncryptdll, 'NCryptFreeObject');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NCryptFreeObject]
  end;
end;

var
  _NCryptIsKeyHandle: Pointer;

function NCryptIsKeyHandle;
begin
  GetProcedureAddress(_NCryptIsKeyHandle, ncryptdll, 'NCryptIsKeyHandle');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NCryptIsKeyHandle]
  end;
end;

var
  _NCryptTranslateHandle: Pointer;

function NCryptTranslateHandle;
begin
  GetProcedureAddress(_NCryptTranslateHandle, ncryptdll, 'NCryptTranslateHandle');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NCryptTranslateHandle]
  end;
end;

var
  _NCryptNotifyChangeKey: Pointer;

function NCryptNotifyChangeKey;
begin
  GetProcedureAddress(_NCryptNotifyChangeKey, ncryptdll, 'NCryptNotifyChangeKey');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NCryptNotifyChangeKey]
  end;
end;

var
  _NCryptSecretAgreement: Pointer;

function NCryptSecretAgreement;
begin
  GetProcedureAddress(_NCryptSecretAgreement, ncryptdll, 'NCryptSecretAgreement');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NCryptSecretAgreement]
  end;
end;

var
  _NCryptDeriveKey: Pointer;

function NCryptDeriveKey;
begin
  GetProcedureAddress(_NCryptDeriveKey, ncryptdll, 'NCryptDeriveKey');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NCryptDeriveKey]
  end;
end;



{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}

