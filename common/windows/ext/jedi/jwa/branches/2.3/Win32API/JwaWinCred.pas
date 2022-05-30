{******************************************************************************}
{                                                                              }
{ Credentials Manager API interface Unit for Object Pascal                     }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: wincred.h, released November 2001. The original Pascal }
{ code is: WinCred.pas, released March 2002. The initial developer of the      }
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

// $Id: JwaWinCred.pas,v 1.15 2007/09/14 06:48:48 marquardt Exp $

{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaWinCred;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "wincred.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}
{$I ..\Includes\jedi.inc} //used for D5 compiling

interface

uses
  JwaLmCons, JwaWinBase, JwaWinError, JwaWinType, JwaNtSecApi;
{$ENDIF JWA_OMIT_SECTIONS}


{$IFNDEF JWA_IMPLEMENTATIONSECTION}
type
  PCtxtHandle = PSecHandle;
  {$EXTERNALSYM PCtxtHandle}

//-----------------------------------------------------------------------------
// Macros
//-----------------------------------------------------------------------------

//
// Macro to determine whether CredUIPromptForCredentials should be called upon a failed
//      authentication attempt.
//
// Implemented as a macro so that the caller can delay load credui.dll only if this
//      macro returns TRUE.
//
// Include only status codes that imply the username/password are wrong or that the
//      password is expired.  In the former case, asking for a another username or password
//      is appropriate.  In the later case, we put up a different dialog asking the
//      user to change the password on the server.
//
// Don't include status codes such as ERROR_ACCOUNT_DISABLED, ERROR_ACCOUNT_RESTRICTION,
//      ERROR_ACCOUNT_LOCKED_OUT, ERROR_ACCOUNT_EXPIRED, ERROR_LOGON_TYPE_NOT_GRANTED.
//      For those, the user isn't going to have another account so prompting him
//      won't help.
//
// STATUS_DOWNGRADE_DETECTED is included to handle the case where a corporate laptop
//      is brought to another LAN.  A downgrade attack will indeed be detected,
//      but we want to popup UI to allow the user to connect to resources in the
//      other LAN.
//
// Don't use the CREDUIP_* macros directly.  Their definition is private to credui.dll.
//

// Don't require ntstatus.h

const
  {$IFNDEF JWA_INCLUDEMODE}
  STATUS_LOGON_FAILURE          = NTSTATUS($C000006D); // ntsubauth
  {$EXTERNALSYM STATUS_LOGON_FAILURE}
  STATUS_WRONG_PASSWORD         = NTSTATUS($C000006A); // ntsubauth
  {$EXTERNALSYM STATUS_WRONG_PASSWORD}
  STATUS_PASSWORD_EXPIRED       = NTSTATUS($C0000071); // ntsubauth
  {$EXTERNALSYM STATUS_PASSWORD_EXPIRED}
  STATUS_PASSWORD_MUST_CHANGE   = NTSTATUS($C0000224); // ntsubauth
  {$EXTERNALSYM STATUS_PASSWORD_MUST_CHANGE}
  STATUS_ACCESS_DENIED          = NTSTATUS($C0000022);
  {$EXTERNALSYM STATUS_ACCESS_DENIED}
  {$ENDIF JWA_INCLUDEMODE}
  STATUS_DOWNGRADE_DETECTED     = NTSTATUS($C0000388);
  {$EXTERNALSYM STATUS_DOWNGRADE_DETECTED}
  STATUS_AUTHENTICATION_FIREWALL_FAILED = NTSTATUS($C0000413);
  {$EXTERNALSYM STATUS_AUTHENTICATION_FIREWALL_FAILED}
  {$IFNDEF JWA_INCLUDEMODE}
  STATUS_ACCOUNT_DISABLED       = NTSTATUS($C0000072);   // ntsubauth
  {$EXTERNALSYM STATUS_ACCOUNT_DISABLED}
  STATUS_ACCOUNT_RESTRICTION    = NTSTATUS($C000006E);   // ntsubauth
  {$EXTERNALSYM STATUS_ACCOUNT_RESTRICTION}
  STATUS_ACCOUNT_LOCKED_OUT     = NTSTATUS($C0000234);   // ntsubauth
  {$EXTERNALSYM STATUS_ACCOUNT_LOCKED_OUT}
  STATUS_ACCOUNT_EXPIRED        = NTSTATUS($C0000193);   // ntsubauth
  {$EXTERNALSYM STATUS_ACCOUNT_EXPIRED}
  STATUS_LOGON_TYPE_NOT_GRANTED = NTSTATUS($C000015B);
  {$EXTERNALSYM STATUS_LOGON_TYPE_NOT_GRANTED}

// Don't require lmerr.h

  NERR_BASE            = 2100;
  {$EXTERNALSYM NERR_BASE}
  NERR_PasswordExpired = NERR_BASE + 142; // The password of this user has expired.
  {$EXTERNALSYM NERR_PasswordExpired}
  {$ENDIF JWA_INCLUDEMODE}

function CREDUIP_IS_USER_PASSWORD_ERROR(_Status: NTSTATUS): BOOL;
{$EXTERNALSYM CREDUIP_IS_USER_PASSWORD_ERROR}

function CREDUIP_IS_DOWNGRADE_ERROR(_Status: NTSTATUS): BOOL;
{$EXTERNALSYM CREDUIP_IS_DOWNGRADE_ERROR}

function CREDUIP_IS_EXPIRED_ERROR(_Status: NTSTATUS): BOOL;
{$EXTERNALSYM CREDUIP_IS_EXPIRED_ERROR}

function CREDUI_IS_AUTHENTICATION_ERROR(_Status: NTSTATUS): BOOL;
{$EXTERNALSYM CREDUI_IS_AUTHENTICATION_ERROR}

function CREDUI_NO_PROMPT_AUTHENTICATION_ERROR(_Status: NTSTATUS): BOOL;
{$EXTERNALSYM CREDUI_NO_PROMPT_AUTHENTICATION_ERROR}

//-----------------------------------------------------------------------------
// Structures
//-----------------------------------------------------------------------------

//
// Credential Attribute
//

const

// Maximum length of the various credential string fields (in characters)

  CRED_MAX_STRING_LENGTH = 256;
  {$EXTERNALSYM CRED_MAX_STRING_LENGTH}

// Maximum length of the UserName field.  The worst case is <User>@<DnsDomain>

  CRED_MAX_USERNAME_LENGTH = 256 + 1 + 256;
  {$EXTERNALSYM CRED_MAX_USERNAME_LENGTH}

// Maximum length of the TargetName field for CRED_TYPE_GENERIC (in characters)

  CRED_MAX_GENERIC_TARGET_NAME_LENGTH = 32767;
  {$EXTERNALSYM CRED_MAX_GENERIC_TARGET_NAME_LENGTH}

// Maximum length of the TargetName field for CRED_TYPE_DOMAIN_* (in characters)
//      Largest one is <DfsRoot>\<DfsShare>

  CRED_MAX_DOMAIN_TARGET_NAME_LENGTH = 256 + 1 + 80;
  {$EXTERNALSYM CRED_MAX_DOMAIN_TARGET_NAME_LENGTH}

// Maximum size of the Credential Attribute Value field (in bytes)

  CRED_MAX_VALUE_SIZE = 256;
  {$EXTERNALSYM CRED_MAX_VALUE_SIZE}

// Maximum number of attributes per credential

  CRED_MAX_ATTRIBUTES = 64;
  {$EXTERNALSYM CRED_MAX_ATTRIBUTES}

type
  PCREDENTIAL_ATTRIBUTEA = ^CREDENTIAL_ATTRIBUTEA;
  {$EXTERNALSYM PCREDENTIAL_ATTRIBUTEA}
  _CREDENTIAL_ATTRIBUTEA = record
    Keyword: LPSTR;
    Flags: DWORD;
    ValueSize: DWORD;
    Value: LPBYTE;
  end;
  {$EXTERNALSYM _CREDENTIAL_ATTRIBUTEA}
  CREDENTIAL_ATTRIBUTEA = _CREDENTIAL_ATTRIBUTEA;
  {$EXTERNALSYM CREDENTIAL_ATTRIBUTEA}
  TCredentialAttributeA = CREDENTIAL_ATTRIBUTEA;
  PCredentialAttributeA = PCREDENTIAL_ATTRIBUTEA;

  PCREDENTIAL_ATTRIBUTEW = ^CREDENTIAL_ATTRIBUTEW;
  {$EXTERNALSYM PCREDENTIAL_ATTRIBUTEW}
  _CREDENTIAL_ATTRIBUTEW = record
    Keyword: LPWSTR;
    Flags: DWORD;
    ValueSize: DWORD;
    Value: LPBYTE;
  end;
  {$EXTERNALSYM _CREDENTIAL_ATTRIBUTEW}
  CREDENTIAL_ATTRIBUTEW = _CREDENTIAL_ATTRIBUTEW;
  {$EXTERNALSYM CREDENTIAL_ATTRIBUTEW}
  TCredentialAttributeW = CREDENTIAL_ATTRIBUTEW;
  PCredentialAttributeW = PCREDENTIAL_ATTRIBUTEW;

  {$IFDEF UNICODE}
  CREDENTIAL_ATTRIBUTE = CREDENTIAL_ATTRIBUTEW;
  {$EXTERNALSYM CREDENTIAL_ATTRIBUTE}
  PCREDENTIAL_ATTRIBUTE = PCREDENTIAL_ATTRIBUTEW;
  {$EXTERNALSYM PCREDENTIAL_ATTRIBUTE}
  TCredentialAttribute = TCredentialAttributeW;
  PCredentialAttribute = PCredentialAttributeW;
  {$ELSE}
  CREDENTIAL_ATTRIBUTE = CREDENTIAL_ATTRIBUTEA;
  {$EXTERNALSYM CREDENTIAL_ATTRIBUTE}
  PCREDENTIAL_ATTRIBUTE = PCREDENTIAL_ATTRIBUTEA;
  {$EXTERNALSYM PCREDENTIAL_ATTRIBUTE}
  TCredentialAttribute = TCredentialAttributeA;
  PCredentialAttribute = PCredentialAttributeA;
  {$ENDIF UNICODE}

//
// Special values of the TargetName field
//

const
  CRED_SESSION_WILDCARD_NAME_W      = WideString('*Session');
  {$EXTERNALSYM CRED_SESSION_WILDCARD_NAME_W}
  CRED_SESSION_WILDCARD_NAME_A      = '*Session';
  {$EXTERNALSYM CRED_SESSION_WILDCARD_NAME_A}
  CRED_SESSION_WILDCARD_NAME_LENGTH = SizeOf(CRED_SESSION_WILDCARD_NAME_A) - 1;
  {$EXTERNALSYM CRED_SESSION_WILDCARD_NAME_LENGTH}

  {$IFDEF UNICODE}
  CRED_SESSION_WILDCARD_NAME = CRED_SESSION_WILDCARD_NAME_W;
  {$EXTERNALSYM CRED_SESSION_WILDCARD_NAME}
  {$ELSE}
  CRED_SESSION_WILDCARD_NAME = CRED_SESSION_WILDCARD_NAME_A;
  {$EXTERNALSYM CRED_SESSION_WILDCARD_NAME}
  {$ENDIF UNICODE}

//
// Values of the Credential Flags field.
//

const
  CRED_FLAGS_PASSWORD_FOR_CERT  = $0001;
  {$EXTERNALSYM CRED_FLAGS_PASSWORD_FOR_CERT}
  CRED_FLAGS_PROMPT_NOW         = $0002;
  {$EXTERNALSYM CRED_FLAGS_PROMPT_NOW}
  CRED_FLAGS_USERNAME_TARGET    = $0004;
  {$EXTERNALSYM CRED_FLAGS_USERNAME_TARGET}
  CRED_FLAGS_OWF_CRED_BLOB      = $0008;
  {$EXTERNALSYM CRED_FLAGS_OWF_CRED_BLOB}
  CRED_FLAGS_VALID_FLAGS        = $000F;  // Mask of all valid flags
  {$EXTERNALSYM CRED_FLAGS_VALID_FLAGS}

//
// Values of the Credential Type field.
//

  CRED_TYPE_GENERIC                 = 1;
  {$EXTERNALSYM CRED_TYPE_GENERIC}
  CRED_TYPE_DOMAIN_PASSWORD         = 2;
  {$EXTERNALSYM CRED_TYPE_DOMAIN_PASSWORD}
  CRED_TYPE_DOMAIN_CERTIFICATE      = 3;
  {$EXTERNALSYM CRED_TYPE_DOMAIN_CERTIFICATE}
  CRED_TYPE_DOMAIN_VISIBLE_PASSWORD = 4;
  {$EXTERNALSYM CRED_TYPE_DOMAIN_VISIBLE_PASSWORD}
  CRED_TYPE_MAXIMUM                 = 5; // Maximum supported cred type
  {$EXTERNALSYM CRED_TYPE_MAXIMUM}
  CRED_TYPE_MAXIMUM_EX              = CRED_TYPE_MAXIMUM + 1000;  // Allow new applications to run on old OSes
  {$EXTERNALSYM CRED_TYPE_MAXIMUM_EX}

//
// Maximum size of the CredBlob field (in bytes)
//

  CRED_MAX_CREDENTIAL_BLOB_SIZE = 512;
  {$EXTERNALSYM CRED_MAX_CREDENTIAL_BLOB_SIZE}

//
// Values of the Credential Persist field
//

  CRED_PERSIST_NONE          = 0;
  {$EXTERNALSYM CRED_PERSIST_NONE}
  CRED_PERSIST_SESSION       = 1;
  {$EXTERNALSYM CRED_PERSIST_SESSION}
  CRED_PERSIST_LOCAL_MACHINE = 2;
  {$EXTERNALSYM CRED_PERSIST_LOCAL_MACHINE}
  CRED_PERSIST_ENTERPRISE    = 3;
  {$EXTERNALSYM CRED_PERSIST_ENTERPRISE}

//
// A credential
//

type
  PCREDENTIALA = ^CREDENTIALA;
  {$EXTERNALSYM PCREDENTIALA}
  _CREDENTIALA = record
    Flags: DWORD;
    Type_: DWORD;
    TargetName: LPSTR;
    Comment: LPSTR;
    LastWritten: FILETIME;
    CredentialBlobSize: DWORD;
    CredentialBlob: LPBYTE;
    Persist: DWORD;
    AttributeCount: DWORD;
    Attributes: PCREDENTIAL_ATTRIBUTEA;
    TargetAlias: LPSTR;
    UserName: LPSTR;
  end;
  {$EXTERNALSYM _CREDENTIALA}
  CREDENTIALA = _CREDENTIALA;
  {$EXTERNALSYM CREDENTIALA}
  TCredentialA = CREDENTIALA;

  PCREDENTIALW = ^CREDENTIALW;
  {$EXTERNALSYM PCREDENTIALW}
  _CREDENTIALW = record
    Flags: DWORD;
    Type_: DWORD;
    TargetName: LPWSTR;
    Comment: LPWSTR;
    LastWritten: FILETIME;
    CredentialBlobSize: DWORD;
    CredentialBlob: LPBYTE;
    Persist: DWORD;
    AttributeCount: DWORD;
    Attributes: PCREDENTIAL_ATTRIBUTEW;
    TargetAlias: LPWSTR;
    UserName: LPWSTR;
  end;
  {$EXTERNALSYM _CREDENTIALW}
  CREDENTIALW = _CREDENTIALW;
  {$EXTERNALSYM CREDENTIALW}
  TCredentialW = CREDENTIALW;

  {$IFDEF UNICODE}
  CREDENTIAL = CREDENTIALW;
  {$EXTERNALSYM CREDENTIAL}
  PCREDENTIAL = PCREDENTIALW;
  {$EXTERNALSYM PCREDENTIAL}
  TCredential = TCredentialW;
  {$ELSE}
  CREDENTIAL = CREDENTIALA;
  {$EXTERNALSYM CREDENTIAL}
  PCREDENTIAL = PCREDENTIALA;
  {$EXTERNALSYM PCREDENTIAL}
  TCredential = TCredentialA;
  {$ENDIF UNICODE}

//
// Value of the Flags field in CREDENTIAL_TARGET_INFORMATION
//

const
  CRED_TI_SERVER_FORMAT_UNKNOWN  = $0001; // Don't know if server name is DNS or netbios format
  {$EXTERNALSYM CRED_TI_SERVER_FORMAT_UNKNOWN}
  CRED_TI_DOMAIN_FORMAT_UNKNOWN  = $0002; // Don't know if domain name is DNS or netbios format
  {$EXTERNALSYM CRED_TI_DOMAIN_FORMAT_UNKNOWN}
  CRED_TI_ONLY_PASSWORD_REQUIRED = $0004; // Server only requires a password and not a username
  {$EXTERNALSYM CRED_TI_ONLY_PASSWORD_REQUIRED}
  CRED_TI_USERNAME_TARGET        = $0008; // TargetName is username
  {$EXTERNALSYM CRED_TI_USERNAME_TARGET}
  CRED_TI_CREATE_EXPLICIT_CRED   = $0010; // When creating a cred, create one named TargetInfo->TargetName
  {$EXTERNALSYM CRED_TI_CREATE_EXPLICIT_CRED}
  CRED_TI_WORKGROUP_MEMBER       = $0020; // Indicates the machine is a member of a workgroup
  {$EXTERNALSYM CRED_TI_WORKGROUP_MEMBER}
  CRED_TI_VALID_FLAGS            = $003F;
  {$EXTERNALSYM CRED_TI_VALID_FLAGS}

//
// A credential target
//

type
  PCREDENTIAL_TARGET_INFORMATIONA = ^CREDENTIAL_TARGET_INFORMATIONA;
  {$EXTERNALSYM PCREDENTIAL_TARGET_INFORMATIONA}
  _CREDENTIAL_TARGET_INFORMATIONA = record
    TargetName: LPSTR;
    NetbiosServerName: LPSTR;
    DnsServerName: LPSTR;
    NetbiosDomainName: LPSTR;
    DnsDomainName: LPSTR;
    DnsTreeName: LPSTR;
    PackageName: LPSTR;
    Flags: ULONG;
    CredTypeCount: DWORD;
    CredTypes: LPDWORD;
  end;
  {$EXTERNALSYM _CREDENTIAL_TARGET_INFORMATIONA}
  CREDENTIAL_TARGET_INFORMATIONA = _CREDENTIAL_TARGET_INFORMATIONA;
  {$EXTERNALSYM CREDENTIAL_TARGET_INFORMATIONA}
  TCredentialTargetInformationA = CREDENTIAL_TARGET_INFORMATIONA;
  PCredentialTargetInformationA = PCREDENTIAL_TARGET_INFORMATIONA;

  PCREDENTIAL_TARGET_INFORMATIONW = ^CREDENTIAL_TARGET_INFORMATIONW;
  {$EXTERNALSYM PCREDENTIAL_TARGET_INFORMATIONW}
  _CREDENTIAL_TARGET_INFORMATIONW = record
    TargetName: LPWSTR;
    NetbiosServerName: LPWSTR;
    DnsServerName: LPWSTR;
    NetbiosDomainName: LPWSTR;
    DnsDomainName: LPWSTR;
    DnsTreeName: LPWSTR;
    PackageName: LPWSTR;
    Flags: ULONG;
    CredTypeCount: DWORD;
    CredTypes: LPDWORD;
  end;
  {$EXTERNALSYM _CREDENTIAL_TARGET_INFORMATIONW}
  CREDENTIAL_TARGET_INFORMATIONW = _CREDENTIAL_TARGET_INFORMATIONW;
  {$EXTERNALSYM CREDENTIAL_TARGET_INFORMATIONW}
  TCredentialTargetInformationW = CREDENTIAL_TARGET_INFORMATIONW;
  PCredentialTargetInformationW = PCREDENTIAL_TARGET_INFORMATIONW;

  {$IFDEF UNICODE}
  CREDENTIAL_TARGET_INFORMATION = CREDENTIAL_TARGET_INFORMATIONW;
  {$EXTERNALSYM CREDENTIAL_TARGET_INFORMATION}
  PCREDENTIAL_TARGET_INFORMATION = PCREDENTIAL_TARGET_INFORMATIONW;
  {$EXTERNALSYM PCREDENTIAL_TARGET_INFORMATION}
  TCredentialTargetInformation = TCredentialTargetInformationW;
  PCredentialTargetInformation = PCredentialTargetInformationW;
  {$ELSE}
  CREDENTIAL_TARGET_INFORMATION = CREDENTIAL_TARGET_INFORMATIONA;
  {$EXTERNALSYM CREDENTIAL_TARGET_INFORMATION}
  PCREDENTIAL_TARGET_INFORMATION = PCREDENTIAL_TARGET_INFORMATIONA;
  {$EXTERNALSYM PCREDENTIAL_TARGET_INFORMATION}
  TCredentialTargetInformation = TCredentialTargetInformationA;
  PCredentialTargetInformation = PCredentialTargetInformationA;
  {$ENDIF UNICODE}

//
// Certificate credential information
//
// The cbSize should be the size of the structure, sizeof(CERT_CREDENTIAL_INFO),
// rgbHashofCert is the hash of the cert which is to be used as the credential.
//

const
  CERT_HASH_LENGTH = 20; // SHA1 hashes are used for cert hashes
  {$EXTERNALSYM CERT_HASH_LENGTH}

type
  PCERT_CREDENTIAL_INFO = ^CERT_CREDENTIAL_INFO;
  {$EXTERNALSYM PCERT_CREDENTIAL_INFO}
  _CERT_CREDENTIAL_INFO = record
    cbSize: ULONG;
    rgbHashOfCert: array [0..CERT_HASH_LENGTH - 1] of UCHAR;
  end;
  {$EXTERNALSYM _CERT_CREDENTIAL_INFO}
  CERT_CREDENTIAL_INFO = _CERT_CREDENTIAL_INFO;
  {$EXTERNALSYM CERT_CREDENTIAL_INFO}
  TCertCredentialInfo = CERT_CREDENTIAL_INFO;
  PCertCredentialInfo = PCERT_CREDENTIAL_INFO;

//
// Username Target credential information
//
// This credential can be pass to LsaLogonUser to ask it to find a credential with a
// TargetName of UserName.
//

  PUSERNAME_TARGET_CREDENTIAL_INFO = ^USERNAME_TARGET_CREDENTIAL_INFO;
  {$EXTERNALSYM PUSERNAME_TARGET_CREDENTIAL_INFO}
  _USERNAME_TARGET_CREDENTIAL_INFO = record
    UserName: LPWSTR;
  end;
  {$EXTERNALSYM _USERNAME_TARGET_CREDENTIAL_INFO}
  USERNAME_TARGET_CREDENTIAL_INFO = _USERNAME_TARGET_CREDENTIAL_INFO;
  {$EXTERNALSYM USERNAME_TARGET_CREDENTIAL_INFO}
  TUsernameTargetCredentialInfo = USERNAME_TARGET_CREDENTIAL_INFO;
  PUsernameTargetCredentialInfo = PUSERNAME_TARGET_CREDENTIAL_INFO;

//
// Credential type for credential marshaling routines
//

  _CRED_MARSHAL_TYPE = DWORD;
  {$EXTERNALSYM _CRED_MARSHAL_TYPE}
  CRED_MARSHAL_TYPE = _CRED_MARSHAL_TYPE;
  {$EXTERNALSYM CRED_MARSHAL_TYPE}
  PCRED_MARSHAL_TYPE = ^CRED_MARSHAL_TYPE;
  {$EXTERNALSYM PCRED_MARSHAL_TYPE}
  TCredMarshalType = CRED_MARSHAL_TYPE;
  PCredMarshalType = PCRED_MARSHAL_TYPE;

const
  CertCredential = 1;
  {$EXTERNALSYM CertCredential}
  UsernameTargetCredential = 2;
  {$EXTERNALSYM UsernameTargetCredential}

//
// Credential UI info
//

type
  PCREDUI_INFOA = ^CREDUI_INFOA;
  {$EXTERNALSYM PCREDUI_INFOA}
  _CREDUI_INFOA = record
    cbSize: DWORD;
    hwndParent: HWND;
    pszMessageText: PCSTR;
    pszCaptionText: PCSTR;
    hbmBanner: HBITMAP;
  end;
  {$EXTERNALSYM _CREDUI_INFOA}
  CREDUI_INFOA = _CREDUI_INFOA;
  {$EXTERNALSYM CREDUI_INFOA}
  TCredUIInfoA = CREDUI_INFOA;
  PCredUIInfoA = PCREDUI_INFOA;

  PCREDUI_INFOW = ^CREDUI_INFOW;
  {$EXTERNALSYM PCREDUI_INFOW}
  _CREDUI_INFOW = record
    cbSize: DWORD;
    hwndParent: HWND;
    pszMessageText: LPCWSTR;
    pszCaptionText: LPCWSTR;
    hbmBanner: HBITMAP;
  end;
  {$EXTERNALSYM _CREDUI_INFOW}
  CREDUI_INFOW = _CREDUI_INFOW;
  {$EXTERNALSYM CREDUI_INFOW}
  TCredUIInfoW = CREDUI_INFOW;
  PCredUIInfoW = PCREDUI_INFOW;

  {$IFDEF UNICODE}
  CREDUI_INFO = CREDUI_INFOW;
  {$EXTERNALSYM CREDUI_INFO}
  PCREDUI_INFO = PCREDUI_INFOW;
  {$EXTERNALSYM PCREDUI_INFO}
  TCredUIInfo = TCredUIInfoW;
  PCredUIInfo = PCredUIInfoW;
  {$ELSE}
  CREDUI_INFO = CREDUI_INFOA;
  {$EXTERNALSYM CREDUI_INFO}
  PCREDUI_INFO = PCREDUI_INFOA;
  {$EXTERNALSYM PCREDUI_INFO}
  TCredUIInfo = TCredUIInfoA;
  PCredUIInfo = PCredUIInfoA;
  {$ENDIF UNICODE}

//-----------------------------------------------------------------------------
// Values
//-----------------------------------------------------------------------------

// String length limits:

const
  CREDUI_MAX_MESSAGE_LENGTH        = 32767;
  {$EXTERNALSYM CREDUI_MAX_MESSAGE_LENGTH}
  CREDUI_MAX_CAPTION_LENGTH        = 128;
  {$EXTERNALSYM CREDUI_MAX_CAPTION_LENGTH}
  CREDUI_MAX_GENERIC_TARGET_LENGTH = CRED_MAX_GENERIC_TARGET_NAME_LENGTH;
  {$EXTERNALSYM CREDUI_MAX_GENERIC_TARGET_LENGTH}
  CREDUI_MAX_DOMAIN_TARGET_LENGTH  = CRED_MAX_DOMAIN_TARGET_NAME_LENGTH;
  {$EXTERNALSYM CREDUI_MAX_DOMAIN_TARGET_LENGTH}
  CREDUI_MAX_USERNAME_LENGTH       = CRED_MAX_USERNAME_LENGTH;
  {$EXTERNALSYM CREDUI_MAX_USERNAME_LENGTH}
  CREDUI_MAX_PASSWORD_LENGTH       = CRED_MAX_CREDENTIAL_BLOB_SIZE div 2;
  {$EXTERNALSYM CREDUI_MAX_PASSWORD_LENGTH}

//
// Flags for CredUIPromptForCredentials and/or CredUICmdLinePromptForCredentials
//

  CREDUI_FLAGS_INCORRECT_PASSWORD          = $00001; // indicates the username is valid, but password is not
  {$EXTERNALSYM CREDUI_FLAGS_INCORRECT_PASSWORD}
  CREDUI_FLAGS_DO_NOT_PERSIST              = $00002; // Do not show "Save" checkbox, and do not persist credentials
  {$EXTERNALSYM CREDUI_FLAGS_DO_NOT_PERSIST}
  CREDUI_FLAGS_REQUEST_ADMINISTRATOR       = $00004; // Populate list box with admin accounts
  {$EXTERNALSYM CREDUI_FLAGS_REQUEST_ADMINISTRATOR}
  CREDUI_FLAGS_EXCLUDE_CERTIFICATES        = $00008; // do not include certificates in the drop list
  {$EXTERNALSYM CREDUI_FLAGS_EXCLUDE_CERTIFICATES}
  CREDUI_FLAGS_REQUIRE_CERTIFICATE         = $00010;
  {$EXTERNALSYM CREDUI_FLAGS_REQUIRE_CERTIFICATE}
  CREDUI_FLAGS_SHOW_SAVE_CHECK_BOX         = $00040;
  {$EXTERNALSYM CREDUI_FLAGS_SHOW_SAVE_CHECK_BOX}
  CREDUI_FLAGS_ALWAYS_SHOW_UI              = $00080;
  {$EXTERNALSYM CREDUI_FLAGS_ALWAYS_SHOW_UI}
  CREDUI_FLAGS_REQUIRE_SMARTCARD           = $00100;
  {$EXTERNALSYM CREDUI_FLAGS_REQUIRE_SMARTCARD}
  CREDUI_FLAGS_PASSWORD_ONLY_OK            = $00200;
  {$EXTERNALSYM CREDUI_FLAGS_PASSWORD_ONLY_OK}
  CREDUI_FLAGS_VALIDATE_USERNAME           = $00400;
  {$EXTERNALSYM CREDUI_FLAGS_VALIDATE_USERNAME}
  CREDUI_FLAGS_COMPLETE_USERNAME           = $00800;
  {$EXTERNALSYM CREDUI_FLAGS_COMPLETE_USERNAME}
  CREDUI_FLAGS_PERSIST                     = $01000; // Do not show "Save" checkbox, but persist credentials anyway
  {$EXTERNALSYM CREDUI_FLAGS_PERSIST}
  CREDUI_FLAGS_SERVER_CREDENTIAL           = $04000;
  {$EXTERNALSYM CREDUI_FLAGS_SERVER_CREDENTIAL}
  CREDUI_FLAGS_EXPECT_CONFIRMATION         = $20000; // do not persist unless caller later confirms credential via CredUIConfirmCredential() api
  {$EXTERNALSYM CREDUI_FLAGS_EXPECT_CONFIRMATION}
  CREDUI_FLAGS_GENERIC_CREDENTIALS         = $40000; // Credential is a generic credential
  {$EXTERNALSYM CREDUI_FLAGS_GENERIC_CREDENTIALS}
  CREDUI_FLAGS_USERNAME_TARGET_CREDENTIALS = $80000; // Credential has a username as the target
  {$EXTERNALSYM CREDUI_FLAGS_USERNAME_TARGET_CREDENTIALS}
  CREDUI_FLAGS_KEEP_USERNAME               = $100000; // don't allow the user to change the supplied username
  {$EXTERNALSYM CREDUI_FLAGS_KEEP_USERNAME}

//
// Mask of flags valid for CredUIPromptForCredentials
//

  CREDUI_FLAGS_PROMPT_VALID =
    CREDUI_FLAGS_INCORRECT_PASSWORD or
    CREDUI_FLAGS_DO_NOT_PERSIST or
    CREDUI_FLAGS_REQUEST_ADMINISTRATOR or
    CREDUI_FLAGS_EXCLUDE_CERTIFICATES or
    CREDUI_FLAGS_REQUIRE_CERTIFICATE or
    CREDUI_FLAGS_SHOW_SAVE_CHECK_BOX or
    CREDUI_FLAGS_ALWAYS_SHOW_UI or
    CREDUI_FLAGS_REQUIRE_SMARTCARD or
    CREDUI_FLAGS_PASSWORD_ONLY_OK or
    CREDUI_FLAGS_VALIDATE_USERNAME or
    CREDUI_FLAGS_COMPLETE_USERNAME or
    CREDUI_FLAGS_PERSIST or
    CREDUI_FLAGS_SERVER_CREDENTIAL or
    CREDUI_FLAGS_EXPECT_CONFIRMATION or
    CREDUI_FLAGS_GENERIC_CREDENTIALS or
    CREDUI_FLAGS_USERNAME_TARGET_CREDENTIALS or
    CREDUI_FLAGS_KEEP_USERNAME;
  {$EXTERNALSYM CREDUI_FLAGS_PROMPT_VALID}

//-----------------------------------------------------------------------------
// Functions
//-----------------------------------------------------------------------------

//
// Values of flags to CredWrite and CredWriteDomainCredentials
//

const
  CRED_PRESERVE_CREDENTIAL_BLOB = $1;
  {$EXTERNALSYM CRED_PRESERVE_CREDENTIAL_BLOB}

function CredWriteA(Credential: PCREDENTIALA; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredWriteA}
function CredWriteW(Credential: PCREDENTIALW; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredWriteW}
function CredWrite(Credential: PCREDENTIAL; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredWrite}

function CredReadA(TargetName: LPCSTR; Type_: DWORD; Flags: DWORD; var Credential: PCREDENTIALA): BOOL; stdcall;
{$EXTERNALSYM CredReadA}
function CredReadW(TargetName: LPCWSTR; Type_: DWORD; Flags: DWORD; var Credential: PCREDENTIALW): BOOL; stdcall;
{$EXTERNALSYM CredReadW}
function CredRead(TargetName: LPCTSTR; Type_: DWORD; Flags: DWORD; var Credential: PCREDENTIAL): BOOL; stdcall;
{$EXTERNALSYM CredRead}

function CredEnumerateA(Filter: LPCSTR; Flags: DWORD; var Count: DWORD; var Credential: PCREDENTIALA): BOOL; stdcall;
{$EXTERNALSYM CredEnumerateA}
function CredEnumerateW(Filter: LPCWSTR; Flags: DWORD; var Count: DWORD; var Credential: PCREDENTIALW): BOOL; stdcall;
{$EXTERNALSYM CredEnumerateW}
function CredEnumerate(Filter: LPCTSTR; Flags: DWORD; var Count: DWORD; var Credential: PCREDENTIAL): BOOL; stdcall;
{$EXTERNALSYM CredEnumerate}

function CredWriteDomainCredentialsA(TargetInfo: PCREDENTIAL_TARGET_INFORMATIONA; Credential: PCREDENTIALA; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredWriteDomainCredentialsA}
function CredWriteDomainCredentialsW(TargetInfo: PCREDENTIAL_TARGET_INFORMATIONW; Credential: PCREDENTIALW; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredWriteDomainCredentialsW}
function CredWriteDomainCredentials(TargetInfo: PCREDENTIAL_TARGET_INFORMATION; Credential: PCREDENTIAL; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredWriteDomainCredentials}

//
// Values of flags to CredReadDomainCredentials
//

const
  CRED_CACHE_TARGET_INFORMATION = $1;
  {$EXTERNALSYM CRED_CACHE_TARGET_INFORMATION}

function CredReadDomainCredentialsA(TargetInfo: PCREDENTIAL_TARGET_INFORMATIONA; Flags: DWORD; var Count: DWORD; var Credential: PCREDENTIALA): BOOL; stdcall;
{$EXTERNALSYM CredReadDomainCredentialsA}
function CredReadDomainCredentialsW(TargetInfo: PCREDENTIAL_TARGET_INFORMATIONW; Flags: DWORD; var Count: DWORD; var Credential: PCREDENTIALW): BOOL; stdcall;
{$EXTERNALSYM CredReadDomainCredentialsW}
function CredReadDomainCredentials(TargetInfo: PCREDENTIAL_TARGET_INFORMATION; Flags: DWORD; var Count: DWORD; var Credential: PCREDENTIAL): BOOL; stdcall;
{$EXTERNALSYM CredReadDomainCredentials}

function CredDeleteA(TargetName: LPCSTR; Type_: DWORD; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredDeleteA}
function CredDeleteW(TargetName: LPCWSTR; Type_: DWORD; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredDeleteW}
function CredDelete(TargetName: LPCTSTR; Type_: DWORD; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredDelete}

function CredRenameA(OldTargetName: LPCSTR; NewTargetName: LPCSTR; Type_: DWORD; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredRenameA}
function CredRenameW(OldTargetName: LPCWSTR; NewTargetName: LPCWSTR; Type_: DWORD; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredRenameW}
function CredRename(OldTargetName: LPCTSTR; NewTargetName: LPCTSTR; Type_: DWORD; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredRename}

//
// Values of flags to CredGetTargetInfo
//

const
  CRED_ALLOW_NAME_RESOLUTION = $1;
  {$EXTERNALSYM CRED_ALLOW_NAME_RESOLUTION}

function CredGetTargetInfoA(TargetName: LPCSTR; Flags: DWORD; var TargetInfo: PCREDENTIAL_TARGET_INFORMATIONA): BOOL; stdcall;
{$EXTERNALSYM CredGetTargetInfoA}
function CredGetTargetInfoW(TargetName: LPCWSTR; Flags: DWORD; var TargetInfo: PCREDENTIAL_TARGET_INFORMATIONW): BOOL; stdcall;
{$EXTERNALSYM CredGetTargetInfoW}
function CredGetTargetInfo(TargetName: LPCTSTR; Flags: DWORD; var TargetInfo: PCREDENTIAL_TARGET_INFORMATION): BOOL; stdcall;
{$EXTERNALSYM CredGetTargetInfo}

function CredMarshalCredentialA(CredType: CRED_MARSHAL_TYPE; Credential: PVOID; MarshaledCredential: LPSTR): BOOL; stdcall;
{$EXTERNALSYM CredMarshalCredentialA}
function CredMarshalCredentialW(CredType: CRED_MARSHAL_TYPE; Credential: PVOID; var MarshaledCredential: LPWSTR): BOOL; stdcall;
{$EXTERNALSYM CredMarshalCredentialW}
function CredMarshalCredential(CredType: CRED_MARSHAL_TYPE; Credential: PVOID; var MarshaledCredential: LPTSTR): BOOL; stdcall;
{$EXTERNALSYM CredMarshalCredential}

function CredUnmarshalCredentialA(MarshaledCredential: LPCSTR; CredType: PCRED_MARSHAL_TYPE; Credential: PVOID): BOOL; stdcall;
{$EXTERNALSYM CredUnmarshalCredentialA}
function CredUnmarshalCredentialW(MarshaledCredential: LPCWSTR; CredType: PCRED_MARSHAL_TYPE; var Credential: PVOID): BOOL; stdcall;
{$EXTERNALSYM CredUnmarshalCredentialW}
function CredUnmarshalCredential(MarshaledCredential: LPCTSTR; CredType: PCRED_MARSHAL_TYPE; var Credential: PVOID): BOOL; stdcall;
{$EXTERNALSYM CredUnmarshalCredential}

function CredIsMarshaledCredentialA(MarshaledCredential: LPCSTR): BOOL; stdcall;
{$EXTERNALSYM CredIsMarshaledCredentialA}
function CredIsMarshaledCredentialW(MarshaledCredential: LPCWSTR): BOOL; stdcall;
{$EXTERNALSYM CredIsMarshaledCredentialW}
function CredIsMarshaledCredential(MarshaledCredential: LPCTSTR): BOOL; stdcall;
{$EXTERNALSYM CredIsMarshaledCredential}

function CredGetSessionTypes(MaximumPersistCount: DWORD; MaximumPersist: LPDWORD): BOOL; stdcall;
{$EXTERNALSYM CredGetSessionTypes}

procedure CredFree(Buffer: PVOID); stdcall;
{$EXTERNALSYM CredFree}

function CredUIPromptForCredentialsA(pUiInfo: PCREDUI_INFOA; pszTargetName: PCSTR; pContext: PCtxtHandle; dwAuthError: DWORD; pszUserName: PSTR; ulUserNameBufferSize: ULONG; pszPassword: PSTR; ulPasswordBufferSize: ULONG; var save: BOOL; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM CredUIPromptForCredentialsA}
function CredUIPromptForCredentialsW(pUiInfo: PCREDUI_INFOW; pszTargetName: LPCWSTR; pContext: PCtxtHandle; dwAuthError: DWORD; pszUserName: PWSTR; ulUserNameBufferSize: ULONG; pszPassword: PWSTR; ulPasswordBufferSize: ULONG; var save: BOOL; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM CredUIPromptForCredentialsW}
function CredUIPromptForCredentials(pUiInfo: PCREDUI_INFO; pszTargetName: LPCTSTR; pContext: PCtxtHandle; dwAuthError: DWORD; pszUserName: PTSTR; ulUserNameBufferSize: ULONG; pszPassword: PTSTR; ulPasswordBufferSize: ULONG; var save: BOOL; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM CredUIPromptForCredentials}

function CredUIParseUserNameA(pszUserName: PCSTR; pszUser: PSTR; ulUserBufferSize: ULONG; pszDomain: PSTR; ulDomainBufferSize: ULONG): DWORD; stdcall;
{$EXTERNALSYM CredUIParseUserNameA}
function CredUIParseUserNameW(pszUserName: LPCWSTR; pszUser: PWSTR; ulUserBufferSize: ULONG; pszDomain: PWSTR; ulDomainBufferSize: ULONG): DWORD; stdcall;
{$EXTERNALSYM CredUIParseUserNameW}
function CredUIParseUserName(pszUserName: LPCTSTR; pszUser: PTSTR; ulUserBufferSize: ULONG; pszDomain: PTSTR; ulDomainBufferSize: ULONG): DWORD; stdcall;
{$EXTERNALSYM CredUIParseUserName}

function CredUICmdLinePromptForCredentialsA(pszTargetName: PCSTR; pContext: PCtxtHandle; dwAuthError: DWORD; UserName: PSTR; ulUserBufferSize: ULONG; pszPassword: PSTR; ulPasswordBufferSize: ULONG; pfSave: PBOOL; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM CredUICmdLinePromptForCredentialsA}
function CredUICmdLinePromptForCredentialsW(pszTargetName: LPCWSTR; pContext: PCtxtHandle; dwAuthError: DWORD; UserName: PWSTR; ulUserBufferSize: ULONG; pszPassword: PWSTR; ulPasswordBufferSize: ULONG; pfSave: PBOOL; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM CredUICmdLinePromptForCredentialsW}
function CredUICmdLinePromptForCredentials(pszTargetName: LPCTSTR; pContext: PCtxtHandle; dwAuthError: DWORD; UserName: PTSTR; ulUserBufferSize: ULONG; pszPassword: PTSTR; ulPasswordBufferSize: ULONG; pfSave: PBOOL; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM CredUICmdLinePromptForCredentials}

//
// Call this API with bConfirm set to TRUE to confirm that the credential (previously created
// via CredUIGetCredentials or CredUIPromptForCredentials worked, or with bConfirm set to FALSE
// to indicate it didn't

function CredUIConfirmCredentialsA(pszTargetName: PCSTR; bConfirm: BOOL): DWORD; stdcall;
{$EXTERNALSYM CredUIConfirmCredentialsA}
function CredUIConfirmCredentialsW(pszTargetName: LPCWSTR; bConfirm: BOOL): DWORD; stdcall;
{$EXTERNALSYM CredUIConfirmCredentialsW}
function CredUIConfirmCredentials(pszTargetName: LPCTSTR; bConfirm: BOOL): DWORD; stdcall;
{$EXTERNALSYM CredUIConfirmCredentials}

function CredUIStoreSSOCredW(pszRealm, pszUsername, pszPassword: LPCWSTR; bPersist: BOOL): DWORD; stdcall;
{$EXTERNALSYM CredUIStoreSSOCredW}

function CredUIReadSSOCredW(pszRealm: LPCWSTR; out ppszUsername: PWSTR): DWORD; stdcall;
{$EXTERNALSYM CredUIReadSSOCredW}

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  credapi = 'advapi32.dll';
  credui = 'credui.dll';
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

function CREDUIP_IS_USER_PASSWORD_ERROR(_Status: NTSTATUS): BOOL;
begin
  Result :=
    (DWORD(_Status) = ERROR_LOGON_FAILURE) or
    (_Status = HRESULT_FROM_WIN32(ERROR_LOGON_FAILURE)) or
    (_Status = STATUS_LOGON_FAILURE) or
    (_Status = HRESULT_FROM_NT(STATUS_LOGON_FAILURE)) or
    (DWORD(_Status) = ERROR_ACCESS_DENIED) or
    (_Status = HRESULT_FROM_WIN32(ERROR_ACCESS_DENIED)) or
    (_Status = STATUS_ACCESS_DENIED) or
    (_Status = HRESULT_FROM_NT(STATUS_ACCESS_DENIED)) or
    (DWORD(_Status) = ERROR_INVALID_PASSWORD) or
    (_Status = HRESULT_FROM_WIN32(ERROR_INVALID_PASSWORD)) or
    (_Status = STATUS_WRONG_PASSWORD) or
    (_Status = HRESULT_FROM_NT(STATUS_WRONG_PASSWORD)) or
    (_Status = SEC_E_NO_CREDENTIALS) or
    (_Status = SEC_E_LOGON_DENIED);
end;

function CREDUIP_IS_DOWNGRADE_ERROR(_Status: NTSTATUS): BOOL;
begin
  Result :=
    (DWORD(_Status) = ERROR_DOWNGRADE_DETECTED) or
    (_Status = HRESULT_FROM_WIN32(ERROR_DOWNGRADE_DETECTED)) or
    (_Status = STATUS_DOWNGRADE_DETECTED) or
    (_Status = HRESULT_FROM_NT(STATUS_DOWNGRADE_DETECTED))
end;

function CREDUIP_IS_EXPIRED_ERROR(_Status: NTSTATUS): BOOL;
begin
  Result :=
    (DWORD(_Status) = ERROR_PASSWORD_EXPIRED) or
    (_Status = HRESULT_FROM_WIN32( ERROR_PASSWORD_EXPIRED)) or
    (_Status = STATUS_PASSWORD_EXPIRED) or
    (_Status = HRESULT_FROM_NT( STATUS_PASSWORD_EXPIRED)) or
    (DWORD(_Status) = ERROR_PASSWORD_MUST_CHANGE) or
    (_Status = HRESULT_FROM_WIN32( ERROR_PASSWORD_MUST_CHANGE)) or
    (_Status = STATUS_PASSWORD_MUST_CHANGE) or
    (_Status = HRESULT_FROM_NT( STATUS_PASSWORD_MUST_CHANGE)) or
    (_Status = NERR_PasswordExpired) or
    (_Status = HRESULT_FROM_WIN32(NERR_PasswordExpired));
end;

function CREDUI_IS_AUTHENTICATION_ERROR(_Status: NTSTATUS): BOOL;
begin
  Result := CREDUIP_IS_USER_PASSWORD_ERROR(_Status) or CREDUIP_IS_DOWNGRADE_ERROR(_Status) or CREDUIP_IS_EXPIRED_ERROR(_Status);
end;

function CREDUI_NO_PROMPT_AUTHENTICATION_ERROR(_Status: NTSTATUS): BOOL;
begin
  Result :=
    (_Status = NTSTATUS(ERROR_AUTHENTICATION_FIREWALL_FAILED)) or
    (_Status = HRESULT_FROM_WIN32(ERROR_AUTHENTICATION_FIREWALL_FAILED)) or
    (_Status = STATUS_AUTHENTICATION_FIREWALL_FAILED) or
    (_Status = HRESULT_FROM_NT(STATUS_AUTHENTICATION_FIREWALL_FAILED)) or
    (DWORD(_Status) = ERROR_ACCOUNT_DISABLED) or
    (_Status = HRESULT_FROM_WIN32(ERROR_ACCOUNT_DISABLED)) or
    (_Status = STATUS_ACCOUNT_DISABLED) or
    (_Status = HRESULT_FROM_NT(STATUS_ACCOUNT_DISABLED)) or
    (DWORD(_Status) = ERROR_ACCOUNT_RESTRICTION) or
    (_Status = HRESULT_FROM_WIN32(ERROR_ACCOUNT_RESTRICTION)) or
    (_Status = STATUS_ACCOUNT_RESTRICTION) or
    (_Status = HRESULT_FROM_NT(STATUS_ACCOUNT_RESTRICTION)) or
    (DWORD(_Status) = ERROR_ACCOUNT_LOCKED_OUT) or
    (_Status = HRESULT_FROM_WIN32(ERROR_ACCOUNT_LOCKED_OUT)) or
    (_Status = STATUS_ACCOUNT_LOCKED_OUT) or
    (_Status = HRESULT_FROM_NT(STATUS_ACCOUNT_LOCKED_OUT)) or
    (DWORD(_Status) = ERROR_ACCOUNT_EXPIRED) or
    (_Status = HRESULT_FROM_WIN32(ERROR_ACCOUNT_EXPIRED)) or
    (_Status = STATUS_ACCOUNT_EXPIRED) or
    (_Status = HRESULT_FROM_NT(STATUS_ACCOUNT_EXPIRED)) or
    (DWORD(_Status) = ERROR_LOGON_TYPE_NOT_GRANTED) or
    (_Status = HRESULT_FROM_WIN32(ERROR_LOGON_TYPE_NOT_GRANTED)) or
    (_Status = STATUS_LOGON_TYPE_NOT_GRANTED) or
    (_Status = HRESULT_FROM_NT(STATUS_LOGON_TYPE_NOT_GRANTED));
end;

{$IFDEF DYNAMIC_LINK}

var
  _CredWriteW: Pointer;

function CredWriteW;
begin
  GetProcedureAddress(_CredWriteW, credapi, 'CredWriteW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredWriteW]
  end;
end;

var
  _CredWriteA: Pointer;

function CredWriteA;
begin
  GetProcedureAddress(_CredWriteA, credapi, 'CredWriteA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredWriteA]
  end;
end;

var
  _CredWrite: Pointer;

function CredWrite;
begin
  GetProcedureAddress(_CredWrite, credapi, 'CredWrite' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredWrite]
  end;
end;

var
  _CredReadW: Pointer;

function CredReadW;
begin
  GetProcedureAddress(_CredReadW, credapi, 'CredReadW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredReadW]
  end;
end;

var
  _CredReadA: Pointer;

function CredReadA;
begin
  GetProcedureAddress(_CredReadA, credapi, 'CredReadA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredReadA]
  end;
end;

var
  _CredRead: Pointer;

function CredRead;
begin
  GetProcedureAddress(_CredRead, credapi, 'CredRead' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredRead]
  end;
end;

var
  _CredEnumerateW: Pointer;

function CredEnumerateW;
begin
  GetProcedureAddress(_CredEnumerateW, credapi, 'CredEnumerateW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredEnumerateW]
  end;
end;

var
  _CredEnumerateA: Pointer;

function CredEnumerateA;
begin
  GetProcedureAddress(_CredEnumerateA, credapi, 'CredEnumerateA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredEnumerateA]
  end;
end;

var
  _CredEnumerate: Pointer;

function CredEnumerate;
begin
  GetProcedureAddress(_CredEnumerate, credapi, 'CredEnumerate' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredEnumerate]
  end;
end;

var
  _CredWriteDomainCredentialsW: Pointer;

function CredWriteDomainCredentialsW;
begin
  GetProcedureAddress(_CredWriteDomainCredentialsW, credapi, 'CredWriteDomainCredentialsW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredWriteDomainCredentialsW]
  end;
end;

var
  _CredWriteDomainCredentialsA: Pointer;

function CredWriteDomainCredentialsA;
begin
  GetProcedureAddress(_CredWriteDomainCredentialsA, credapi, 'CredWriteDomainCredentialsA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredWriteDomainCredentialsA]
  end;
end;

var
  _CredWriteDomainCredentials: Pointer;

function CredWriteDomainCredentials;
begin
  GetProcedureAddress(_CredWriteDomainCredentials, credapi, 'CredWriteDomainCredentials' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredWriteDomainCredentials]
  end;
end;

var
  _CredReadDomainCredentialsW: Pointer;

function CredReadDomainCredentialsW;
begin
  GetProcedureAddress(_CredReadDomainCredentialsW, credapi, 'CredReadDomainCredentialsW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredReadDomainCredentialsW]
  end;
end;

var
  _CredReadDomainCredentialsA: Pointer;

function CredReadDomainCredentialsA;
begin
  GetProcedureAddress(_CredReadDomainCredentialsA, credapi, 'CredReadDomainCredentialsA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredReadDomainCredentialsA]
  end;
end;

var
  _CredReadDomainCredentials: Pointer;

function CredReadDomainCredentials;
begin
  GetProcedureAddress(_CredReadDomainCredentials, credapi, 'CredReadDomainCredentials' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredReadDomainCredentials]
  end;
end;

var
  _CredDeleteW: Pointer;

function CredDeleteW;
begin
  GetProcedureAddress(_CredDeleteW, credapi, 'CredDeleteW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredDeleteW]
  end;
end;

var
  _CredDeleteA: Pointer;

function CredDeleteA;
begin
  GetProcedureAddress(_CredDeleteA, credapi, 'CredDeleteA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredDeleteA]
  end;
end;

var
  _CredDelete: Pointer;

function CredDelete;
begin
  GetProcedureAddress(_CredDelete, credapi, 'CredDelete' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredDelete]
  end;
end;

var
  _CredRenameW: Pointer;

function CredRenameW;
begin
  GetProcedureAddress(_CredRenameW, credapi, 'CredRenameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredRenameW]
  end;
end;

var
  _CredRenameA: Pointer;

function CredRenameA;
begin
  GetProcedureAddress(_CredRenameA, credapi, 'CredRenameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredRenameA]
  end;
end;

var
  _CredRename: Pointer;

function CredRename;
begin
  GetProcedureAddress(_CredRename, credapi, 'CredRename' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredRename]
  end;
end;

var
  _CredGetTargetInfoW: Pointer;

function CredGetTargetInfoW;
begin
  GetProcedureAddress(_CredGetTargetInfoW, credapi, 'CredGetTargetInfoW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredGetTargetInfoW]
  end;
end;

var
  _CredGetTargetInfoA: Pointer;

function CredGetTargetInfoA;
begin
  GetProcedureAddress(_CredGetTargetInfoA, credapi, 'CredGetTargetInfoA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredGetTargetInfoA]
  end;
end;

var
  _CredGetTargetInfo: Pointer;

function CredGetTargetInfo;
begin
  GetProcedureAddress(_CredGetTargetInfo, credapi, 'CredGetTargetInfoA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredGetTargetInfo]
  end;
end;

var
  _CredMarshalCredentialW: Pointer;

function CredMarshalCredentialW;
begin
  GetProcedureAddress(_CredMarshalCredentialW, credapi, 'CredMarshalCredentialW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredMarshalCredentialW]
  end;
end;

var
  _CredMarshalCredentialA: Pointer;

function CredMarshalCredentialA;
begin
  GetProcedureAddress(_CredMarshalCredentialA, credapi, 'CredMarshalCredentialA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredMarshalCredentialA]
  end;
end;

var
  _CredMarshalCredential: Pointer;

function CredMarshalCredential;
begin
  GetProcedureAddress(_CredMarshalCredential, credapi, 'CredMarshalCredential' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredMarshalCredential]
  end;
end;

var
  _CredUnmarshalCredentialW: Pointer;

function CredUnmarshalCredentialW;
begin
  GetProcedureAddress(_CredUnmarshalCredentialW, credapi, 'CredUnmarshalCredentialW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredUnmarshalCredentialW]
  end;
end;

var
  _CredUnmarshalCredentialA: Pointer;

function CredUnmarshalCredentialA;
begin
  GetProcedureAddress(_CredUnmarshalCredentialA, credapi, 'CredUnmarshalCredentialA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredUnmarshalCredentialA]
  end;
end;

var
  _CredUnmarshalCredential: Pointer;

function CredUnmarshalCredential;
begin
  GetProcedureAddress(_CredUnmarshalCredential, credapi, 'CredUnmarshalCredential' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredUnmarshalCredential]
  end;
end;

var
  _CredIsMarshaledCredentialW: Pointer;

function CredIsMarshaledCredentialW;
begin
  GetProcedureAddress(_CredIsMarshaledCredentialW, credapi, 'CredIsMarshaledCredentialW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredIsMarshaledCredentialW]
  end;
end;

var
  _CredIsMarshaledCredentialA: Pointer;

function CredIsMarshaledCredentialA;
begin
  GetProcedureAddress(_CredIsMarshaledCredentialA, credapi, 'CredIsMarshaledCredentialA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredIsMarshaledCredentialA]
  end;
end;

var
  _CredIsMarshaledCredential: Pointer;

function CredIsMarshaledCredential;
begin
  GetProcedureAddress(_CredIsMarshaledCredential, credapi, 'CredIsMarshaledCredential' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredIsMarshaledCredential]
  end;
end;

var
  _CredGetSessionTypes: Pointer;

function CredGetSessionTypes;
begin
  GetProcedureAddress(_CredGetSessionTypes, credapi, 'CredGetSessionTypes');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredGetSessionTypes]
  end;
end;

var
  _CredFree: Pointer;

procedure CredFree;
begin
  GetProcedureAddress(_CredFree, credapi, 'CredFree');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredFree]
  end;
end;

var
  _CredUIPromptForCredentialsW: Pointer;

function CredUIPromptForCredentialsW;
begin
  GetProcedureAddress(_CredUIPromptForCredentialsW, credui, 'CredUIPromptForCredentialsW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredUIPromptForCredentialsW]
  end;
end;

var
  _CredUIPromptForCredentialsA: Pointer;

function CredUIPromptForCredentialsA;
begin
  GetProcedureAddress(_CredUIPromptForCredentialsA, credui, 'CredUIPromptForCredentialsA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredUIPromptForCredentialsA]
  end;
end;

var
  _CredUIPromptForCredentials: Pointer;

function CredUIPromptForCredentials;
begin
  GetProcedureAddress(_CredUIPromptForCredentials, credui, 'CredUIPromptForCredentials' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredUIPromptForCredentials]
  end;
end;

var
  _CredUIParseUserNameW: Pointer;

function CredUIParseUserNameW;
begin
  GetProcedureAddress(_CredUIParseUserNameW, credui, 'CredUIParseUserNameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredUIParseUserNameW]
  end;
end;

var
  _CredUIParseUserNameA: Pointer;

function CredUIParseUserNameA;
begin
  GetProcedureAddress(_CredUIParseUserNameA, credui, 'CredUIParseUserNameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredUIParseUserNameA]
  end;
end;

var
  _CredUIParseUserName: Pointer;

function CredUIParseUserName;
begin
  GetProcedureAddress(_CredUIParseUserName, credui, 'CredUIParseUserName' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredUIParseUserName]
  end;
end;

var
{$IFDEF SUPPORT_LONG_VARNAMES}
   _CredUICmdLinePromptForCredentialsW: Pointer;
{$ELSE}
  _CredUICmdLinePromptFCW: Pointer;
{$ENDIF}

function CredUICmdLinePromptForCredentialsW;
begin
{$IFDEF SUPPORT_LONG_VARNAMES}
  GetProcedureAddress(_CredUICmdLinePromptForCredentialsW, credui, 'CredUICmdLinePromptForCredentialsW');
{$ELSE}
  GetProcedureAddress(_CredUICmdLinePromptFCW, credui, 'CredUICmdLinePromptForCredentialsW');
{$ENDIF}

  asm
        MOV     ESP, EBP
        POP     EBP
{$IFDEF SUPPORT_LONG_VARNAMES}
        JMP     [_CredUICmdLinePromptForCredentialsW]
{$ELSE}
        JMP     [_CredUICmdLinePromptFCW]
{$ENDIF}
  end;
end;

var
{$IFDEF SUPPORT_LONG_VARNAMES}
   _CredUICmdLinePromptForCredentialsA: Pointer;
{$ELSE}
  _CredUICmdLinePromptFCA: Pointer;
{$ENDIF}


function CredUICmdLinePromptForCredentialsA;
begin
{$IFDEF SUPPORT_LONG_VARNAMES}
  GetProcedureAddress(_CredUICmdLinePromptForCredentialsA, credui, 'CredUICmdLinePromptForCredentialsA');
{$ELSE}
  GetProcedureAddress(_CredUICmdLinePromptFCA, credui, 'CredUICmdLinePromptForCredentialsA');
{$ENDIF}
  asm
        MOV     ESP, EBP
        POP     EBP
{$IFDEF SUPPORT_LONG_VARNAMES}
        JMP     [_CredUICmdLinePromptForCredentialsA]
{$ELSE}
        JMP     [_CredUICmdLinePromptFCA]
{$ENDIF}
  end;
end;

var

{$IFDEF SUPPORT_LONG_VARNAMES}
        _CredUICmdLinePromptForCredentials: Pointer;
{$ELSE}
        _CredUICmdLinePromptFC: Pointer;
{$ENDIF}

function CredUICmdLinePromptForCredentials;
begin
{$IFDEF SUPPORT_LONG_VARNAMES}
  GetProcedureAddress(_CredUICmdLinePromptForCredentials, credui, 'CredUICmdLinePromptForCredentials' + AWSuffix);
{$ELSE}
  GetProcedureAddress(_CredUICmdLinePromptFC, credui, 'CredUICmdLinePromptForCredentials' + AWSuffix);
{$ENDIF}
  asm
        MOV     ESP, EBP
        POP     EBP
{$IFDEF SUPPORT_LONG_VARNAMES}
        JMP     [_CredUICmdLinePromptForCredentials]
{$ELSE}
        JMP     [_CredUICmdLinePromptFC]
{$ENDIF}
  end;
end;

var
  _CredUIConfirmCredentialsW: Pointer;

function CredUIConfirmCredentialsW;
begin
  GetProcedureAddress(_CredUIConfirmCredentialsW, credui, 'CredUIConfirmCredentialsW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredUIConfirmCredentialsW]
  end;
end;

var
  _CredUIConfirmCredentialsA: Pointer;

function CredUIConfirmCredentialsA;
begin
  GetProcedureAddress(_CredUIConfirmCredentialsA, credui, 'CredUIConfirmCredentialsA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredUIConfirmCredentialsA]
  end;
end;

var
  _CredUIConfirmCredentials: Pointer;

function CredUIConfirmCredentials;
begin
  GetProcedureAddress(_CredUIConfirmCredentials, credui, 'CredUIConfirmCredentials' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredUIConfirmCredentials]
  end;
end;

var
  _CredUIStoreSSOCredW: Pointer;

function CredUIStoreSSOCredW;
begin
  GetProcedureAddress(_CredUIStoreSSOCredW, credui, 'CredUIStoreSSOCredW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredUIStoreSSOCredW]
  end;
end;

var
  _CredUIReadSSOCredW: Pointer;

function CredUIReadSSOCredW;
begin
  GetProcedureAddress(_CredUIReadSSOCredW, credui, 'CredUIReadSSOCredW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CredUIReadSSOCredW]
  end;
end;

{$ELSE}

function CredWriteW; external credapi name 'CredWriteW';
function CredWriteA; external credapi name 'CredWriteA';
function CredWrite; external credapi name 'CredWrite' + AWSuffix;
function CredReadW; external credapi name 'CredReadW';
function CredReadA; external credapi name 'CredReadA';
function CredRead; external credapi name 'CredRead' + AWSuffix;
function CredEnumerateW; external credapi name 'CredEnumerateW';
function CredEnumerateA; external credapi name 'CredEnumerateA';
function CredEnumerate; external credapi name 'CredEnumerate' + AWSuffix;
function CredWriteDomainCredentialsW; external credapi name 'CredWriteDomainCredentialsW';
function CredWriteDomainCredentialsA; external credapi name 'CredWriteDomainCredentialsA';
function CredWriteDomainCredentials; external credapi name 'CredWriteDomainCredentials' + AWSuffix;
function CredReadDomainCredentialsW; external credapi name 'CredReadDomainCredentialsW';
function CredReadDomainCredentialsA; external credapi name 'CredReadDomainCredentialsA';
function CredReadDomainCredentials; external credapi name 'CredReadDomainCredentials' + AWSuffix;
function CredDeleteW; external credapi name 'CredDeleteW';
function CredDeleteA; external credapi name 'CredDeleteA';
function CredDelete; external credapi name 'CredDelete' + AWSuffix;
function CredRenameW; external credapi name 'CredRenameW';
function CredRenameA; external credapi name 'CredRenameA';
function CredRename; external credapi name 'CredRename' + AWSuffix;
function CredGetTargetInfoW; external credapi name 'CredGetTargetInfoW';
function CredGetTargetInfoA; external credapi name 'CredGetTargetInfoA';
function CredGetTargetInfo; external credapi name 'CredGetTargetInfoA';
function CredMarshalCredentialW; external credapi name 'CredMarshalCredentialW';
function CredMarshalCredentialA; external credapi name 'CredMarshalCredentialA';
function CredMarshalCredential; external credapi name 'CredMarshalCredential' + AWSuffix;
function CredUnmarshalCredentialW; external credapi name 'CredUnmarshalCredentialW';
function CredUnmarshalCredentialA; external credapi name 'CredUnmarshalCredentialA';
function CredUnmarshalCredential; external credapi name 'CredUnmarshalCredential' + AWSuffix;
function CredIsMarshaledCredentialW; external credapi name 'CredIsMarshaledCredentialW';
function CredIsMarshaledCredentialA; external credapi name 'CredIsMarshaledCredentialA';
function CredIsMarshaledCredential; external credapi name 'CredIsMarshaledCredential' + AWSuffix;
function CredGetSessionTypes; external credapi name 'CredGetSessionTypes';
procedure CredFree; external credapi name 'CredFree';
function CredUIPromptForCredentialsW; external credui name 'CredUIPromptForCredentialsW';
function CredUIPromptForCredentialsA; external credui name 'CredUIPromptForCredentialsA';
function CredUIPromptForCredentials; external credui name 'CredUIPromptForCredentials' + AWSuffix;
function CredUIParseUserNameW; external credui name 'CredUIParseUserNameW';
function CredUIParseUserNameA; external credui name 'CredUIParseUserNameA';
function CredUIParseUserName; external credui name 'CredUIParseUserName' + AWSuffix;
function CredUICmdLinePromptForCredentialsW; external credui name 'CredUICmdLinePromptForCredentialsW';
function CredUICmdLinePromptForCredentialsA; external credui name 'CredUICmdLinePromptForCredentialsA';
function CredUICmdLinePromptForCredentials; external credui name 'CredUICmdLinePromptForCredentials' + AWSuffix;
function CredUIConfirmCredentialsW; external credui name 'CredUIConfirmCredentialsW';
function CredUIConfirmCredentialsA; external credui name 'CredUIConfirmCredentialsA';
function CredUIConfirmCredentials; external credui name 'CredUIConfirmCredentials' + AWSuffix;
function CredUIStoreSSOCredW; external credui name 'CredUIStoreSSOCredW';
function CredUIReadSSOCredW; external credui name 'CredUIReadSSOCredW';

{$ENDIF DYNAMIC_LINK}
{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
