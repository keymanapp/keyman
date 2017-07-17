{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ Cryptographic API interface unit                                 }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1993-1998 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: wincrypt.h, 1992 - 1997                    }
{ The original Pascal code is: wcrypt2.pas, released 01 Jan 1998   }
{ The initial developer of the Pascal code is                      }
{  Massimo Maria Ghisalberti  (nissl@dada.it)                      }
{                                                                  }
{ Portions created by Massimo Maria Ghisalberti are                }
{ Copyright (C) 1997-1998 Massimo Maria Ghisalberti                }
{                                                                  }
{ Contributor(s):                                                  }
{     Peter Tang (peter.tang@citicorp.com)                         }
{     Phil Shrimpton (phil@shrimpton.co.uk)                        }
{                                                                  }
{ Obtained through:                                                }
{                                                                  }
{ Joint Endeavour of Delphi Innovators (Project JEDI)              }
{                                                                  }
{ You may retrieve the latest version of this file at the Project  }
{ JEDI home page, located at http://delphi-jedi.org                }
{                                                                  }
{ The contents of this file are used with permission, subject to   }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/MPL/MPL-1.1.html                          }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{******************************************************************}
//$Id: Wcrypt2.pas,v 1.2 2006/11/15 21:01:44 sergev Exp $

unit wcrypt2;

{.DEFINE NT5}

{$ALIGN ON}

{$IFNDEF VER90}
{$WEAKPACKAGEUNIT}
{$ENDIF}

interface

uses
  Windows
{$IFDEF VER90}
  , Ole2
{$ENDIF};

const
  ADVAPI32 = 'advapi32.dll';
  CRYPT32 = 'crypt32.dll';
  SOFTPUB = 'softpub.dll';
{$IFDEF NT5}
  ADVAPI32NT5 = 'advapi32.dll';
{$ENDIF}

{Support Type}

type
  PVOID = Pointer;
  LONG = DWORD;
{$IFDEF UNICODE}
  LPAWSTR = PWideChar;
{$ELSE}
  LPAWSTR = PAnsiChar;
{$ENDIF}

//-----------------------------------------------------------------------------
    // Type support for a pointer to an array of pointer (type **name)
  PLPSTR = Pointer; // type for a pointer to Array of pointer a type
  PPCERT_INFO = Pointer; // type for a pointer to Array of pointer a type
  PPVOID = Pointer; // type for a pointer to Array of pointer a type
  PPCCERT_CONTEXT = Pointer; // type for a pointer to Array of pointer a type
  PPCCTL_CONTEXT = Pointer; // type for a pointer to Array of pointer a type
  PPCCRL_CONTEXT = Pointer; // type for a pointer to Array of pointer a type
//-----------------------------------------------------------------------------

//+---------------------------------------------------------------------------
//
//  Microsoft Windows
//  Copyright (C) Microsoft Corporation, 1992 - 1997.
//
//  File:       wincrypt.h
//
//  Contents:   Cryptographic API Prototypes and Definitions
//
//----------------------------------------------------------------------------


//
// Algorithm IDs and Flags
//

// ALG_ID crackers
function GET_ALG_CLASS(x: integer): integer;
function GET_ALG_TYPE(x: integer): integer;
function GET_ALG_SID(x: integer): integer;

const
  // Algorithm classes
  ALG_CLASS_ANY = 0;
  ALG_CLASS_SIGNATURE = (1 shl 13);
  ALG_CLASS_MSG_ENCRYPT = (2 shl 13);
  ALG_CLASS_DATA_ENCRYPT = (3 shl 13);
  ALG_CLASS_HASH = (4 shl 13);
  ALG_CLASS_KEY_EXCHANGE = (5 shl 13);

  // Algorithm types
  ALG_TYPE_ANY = 0;
  ALG_TYPE_DSS = (1 shl 9);
  ALG_TYPE_RSA = (2 shl 9);
  ALG_TYPE_BLOCK = (3 shl 9);
  ALG_TYPE_STREAM = (4 shl 9);
  ALG_TYPE_DH = (5 shl 9);
  ALG_TYPE_SECURECHANNEL = (6 shl 9);

  // Generic sub-ids
  ALG_SID_ANY = 0;

  // Some RSA sub-ids
  ALG_SID_RSA_ANY = 0;
  ALG_SID_RSA_PKCS = 1;
  ALG_SID_RSA_MSATWORK = 2;
  ALG_SID_RSA_ENTRUST = 3;
  ALG_SID_RSA_PGP = 4;

  // Some DSS sub-ids
  ALG_SID_DSS_ANY = 0;
  ALG_SID_DSS_PKCS = 1;
  ALG_SID_DSS_DMS = 2;

  // Block cipher sub ids
  // DES sub_ids
  ALG_SID_DES = 1;
  ALG_SID_3DES = 3;
  ALG_SID_DESX = 4;
  ALG_SID_IDEA = 5;
  ALG_SID_CAST = 6;
  ALG_SID_SAFERSK64 = 7;
  ALD_SID_SAFERSK128 = 8;
  ALG_SID_SAFERSK128 = 8;
  ALG_SID_3DES_112 = 9;
  ALG_SID_CYLINK_MEK = 12;
  ALG_SID_RC5 = 13;

  // Fortezza sub-ids
  ALG_SID_SKIPJACK = 10;
  ALG_SID_TEK = 11;

  // KP_MODE
  CRYPT_MODE_CBCI = 6; {ANSI CBC Interleaved}
  CRYPT_MODE_CFBP = 7; {ANSI CFB Pipelined}
  CRYPT_MODE_OFBP = 8; {ANSI OFB Pipelined}
  CRYPT_MODE_CBCOFM = 9; {ANSI CBC + OF Masking}
  CRYPT_MODE_CBCOFMI = 10; {ANSI CBC + OFM Interleaved}

  // RC2 sub-ids
  ALG_SID_RC2 = 2;

  // Stream cipher sub-ids
  ALG_SID_RC4 = 1;
  ALG_SID_SEAL = 2;

  // Diffie-Hellman sub-ids
  ALG_SID_DH_SANDF = 1;
  ALG_SID_DH_EPHEM = 2;
  ALG_SID_AGREED_KEY_ANY = 3;
  ALG_SID_KEA = 4;

  // Hash sub ids
  ALG_SID_MD2 = 1;
  ALG_SID_MD4 = 2;
  ALG_SID_MD5 = 3;
  ALG_SID_SHA = 4;
  ALG_SID_SHA1 = 4;
  ALG_SID_MAC = 5;
  ALG_SID_RIPEMD = 6;
  ALG_SID_RIPEMD160 = 7;
  ALG_SID_SSL3SHAMD5 = 8;
  ALG_SID_HMAC = 9;

  // secure channel sub ids
  ALG_SID_SSL3_MASTER = 1;
  ALG_SID_SCHANNEL_MASTER_HASH = 2;
  ALG_SID_SCHANNEL_MAC_KEY = 3;
  ALG_SID_PCT1_MASTER = 4;
  ALG_SID_SSL2_MASTER = 5;
  ALG_SID_TLS1_MASTER = 6;
  ALG_SID_SCHANNEL_ENC_KEY = 7;

  // Our silly example sub-id
  ALG_SID_EXAMPLE = 80;

{$IFNDEF ALGIDDEF}
{$DEFINE ALGIDDEF}
type ALG_ID = ULONG;
{$ENDIF}

// algorithm identifier definitions
const
  CALG_MD2 = (ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_MD2);
  CALG_MD4 = (ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_MD4);
  CALG_MD5 = (ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_MD5);
  CALG_SHA = (ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_SHA);
  CALG_SHA1 = (ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_SHA1);
  CALG_MAC = (ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_MAC);
  CALG_RSA_SIGN = (ALG_CLASS_SIGNATURE or ALG_TYPE_RSA or ALG_SID_RSA_ANY);
  CALG_DSS_SIGN = (ALG_CLASS_SIGNATURE or ALG_TYPE_DSS or ALG_SID_DSS_ANY);
  CALG_RSA_KEYX = (ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_RSA or ALG_SID_RSA_ANY);
  CALG_DES = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_DES);
  CALG_3DES_112 = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_3DES_112);
  CALG_3DES = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_3DES);
  CALG_RC2 = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_RC2);
  CALG_RC4 = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_STREAM or ALG_SID_RC4);
  CALG_SEAL = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_STREAM or ALG_SID_SEAL);
  CALG_DH_SF = (ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_DH or ALG_SID_DH_SANDF);
  CALG_DH_EPHEM = (ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_DH or ALG_SID_DH_EPHEM);
  CALG_AGREEDKEY_ANY = (ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_DH or ALG_SID_AGREED_KEY_ANY);
  CALG_KEA_KEYX = (ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_DH or ALG_SID_KEA);
  CALG_HUGHES_MD5 = (ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_ANY or ALG_SID_MD5);
  CALG_SKIPJACK = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_SKIPJACK);
  CALG_TEK = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_TEK);
  CALG_CYLINK_MEK = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_CYLINK_MEK);
  CALG_SSL3_SHAMD5 = (ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_SSL3SHAMD5);
  CALG_SSL3_MASTER = (ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_SSL3_MASTER);
  CALG_SCHANNEL_MASTER_HASH = (ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_SCHANNEL_MASTER_HASH);
  CALG_SCHANNEL_MAC_KEY = (ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_SCHANNEL_MAC_KEY);
  CALG_SCHANNEL_ENC_KEY = (ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_SCHANNEL_ENC_KEY);
  CALG_PCT1_MASTER = (ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_PCT1_MASTER);
  CALG_SSL2_MASTER = (ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_SSL2_MASTER);
  CALG_TLS1_MASTER = (ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_TLS1_MASTER);
  CALG_RC5 = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_RC5);
  CALG_HMAC = (ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_HMAC);

type
  PVTableProvStruc = ^VTableProvStruc;
  VTableProvStruc = record
    Version: DWORD;
    FuncVerifyImage: TFarProc;
    FuncReturnhWnd: TFarProc;
    dwProvType: DWORD;
    pbContextInfo: PBYTE;
    cbContextInfo: DWORD;
  end;

//type HCRYPTPROV = ULONG;
//type HCRYPTKEY  = ULONG;
//type HCRYPTHASH = ULONG;


const
  // dwFlags definitions for CryptAcquireContext
  CRYPT_VERIFYCONTEXT = $F0000000;
  CRYPT_NEWKEYSET = $00000008;
  CRYPT_DELETEKEYSET = $00000010;
  CRYPT_MACHINE_KEYSET = $00000020;

  // dwFlag definitions for CryptGenKey
  CRYPT_EXPORTABLE = $00000001;
  CRYPT_USER_PROTECTED = $00000002;
  CRYPT_CREATE_SALT = $00000004;
  CRYPT_UPDATE_KEY = $00000008;
  CRYPT_NO_SALT = $00000010;
  CRYPT_PREGEN = $00000040;
  CRYPT_RECIPIENT = $00000010;
  CRYPT_INITIATOR = $00000040;
  CRYPT_ONLINE = $00000080;
  CRYPT_SF = $00000100;
  CRYPT_CREATE_IV = $00000200;
  CRYPT_KEK = $00000400;
  CRYPT_DATA_KEY = $00000800;

  // dwFlags definitions for CryptDeriveKey
  CRYPT_SERVER = $00000400;

  KEY_LENGTH_MASK = $FFFF0000;

  // dwFlag definitions for CryptExportKey
  CRYPT_Y_ONLY = $00000001;
  CRYPT_SSL2_SLUMMING = $00000002;

  // dwFlags definitions for CryptHashSessionKey
  CRYPT_LITTLE_ENDIAN = $00000001;

  // dwFlag definitions for CryptSetProviderEx and CryptGetDefaultProvider
  CRYPT_MACHINE_DEFAULT = $00000001;
  CRYPT_USER_DEFAULT = $00000002;
  CRYPT_DELETE_DEFAULT = $00000004;

  // exported key blob definitions
  SIMPLEBLOB = $1;
  PUBLICKEYBLOB = $6;
  PRIVATEKEYBLOB = $7;
  PLAINTEXTKEYBLOB = $8;
  AT_KEYEXCHANGE = 1;
  AT_SIGNATURE = 2;
  CRYPT_USERDATA = 1;

  // dwParam
  KP_IV = 1; // Initialization vector
  KP_SALT = 2; // Salt value
  KP_PADDING = 3; // Padding values
  KP_MODE = 4; // Mode of the cipher
  KP_MODE_BITS = 5; // Number of bits to feedback
  KP_PERMISSIONS = 6; // Key permissions DWORD
  KP_ALGID = 7; // Key algorithm
  KP_BLOCKLEN = 8; // Block size of the cipher
  KP_KEYLEN = 9; // Length of key in bits
  KP_SALT_EX = 10; // Length of salt in bytes
  KP_P = 11; // DSS/Diffie-Hellman P value
  KP_G = 12; // DSS/Diffie-Hellman G value
  KP_Q = 13; // DSS Q value
  KP_X = 14; // Diffie-Hellman X value
  KP_Y = 15; // Y value
  KP_RA = 16; // Fortezza RA value
  KP_RB = 17; // Fortezza RB value
  KP_INFO = 18; // for putting information into an RSA envelope
  KP_EFFECTIVE_KEYLEN = 19; // setting and getting RC2 effective key length
  KP_SCHANNEL_ALG = 20; // for setting the Secure Channel algorithms
  KP_CLIENT_RANDOM = 21; // for setting the Secure Channel client random data
  KP_SERVER_RANDOM = 22; // for setting the Secure Channel server random data
  KP_RP = 23;
  KP_PRECOMP_MD5 = 24;
  KP_PRECOMP_SHA = 25;
  KP_CERTIFICATE = 26; // for setting Secure Channel certificate data (PCT1)
  KP_CLEAR_KEY = 27; // for setting Secure Channel clear key data (PCT1)
  KP_PUB_EX_LEN = 28;
  KP_PUB_EX_VAL = 29;

  // KP_PADDING
  PKCS5_PADDING = 1; {PKCS 5 (sec 6.2) padding method}
  RANDOM_PADDING = 2;
  ZERO_PADDING = 3;

  // KP_MODE
  CRYPT_MODE_CBC = 1; // Cipher block chaining
  CRYPT_MODE_ECB = 2; // Electronic code book
  CRYPT_MODE_OFB = 3; // Output feedback mode
  CRYPT_MODE_CFB = 4; // Cipher feedback mode
  CRYPT_MODE_CTS = 5; // Ciphertext stealing mode

  // KP_PERMISSIONS
  CRYPT_ENCRYPT = $0001; // Allow encryption
  CRYPT_DECRYPT = $0002; // Allow decryption
  CRYPT_EXPORT = $0004; // Allow key to be exported
  CRYPT_READ = $0008; // Allow parameters to be read
  CRYPT_WRITE = $0010; // Allow parameters to be set
  CRYPT_MAC = $0020; // Allow MACs to be used with key
  CRYPT_EXPORT_KEY = $0040; // Allow key to be used for exporting keys
  CRYPT_IMPORT_KEY = $0080; // Allow key to be used for importing keys

  HP_ALGID = $0001; // Hash algorithm
  HP_HASHVAL = $0002; // Hash value
  HP_HASHSIZE = $0004; // Hash value size

  HP_HMAC_INFO = $0005; // information for creating an HMAC

  CRYPT_FAILED = FALSE;
  CRYPT_SUCCEED = TRUE;

function RCRYPT_SUCCEEDED(rt: BOOL): BOOL;
function RCRYPT_FAILED(rt: BOOL): BOOL;

const
  // CryptGetProvParam
  PP_ENUMALGS = 1;
  PP_ENUMCONTAINERS = 2;
  PP_IMPTYPE = 3;
  PP_NAME = 4;
  PP_VERSION = 5;
  PP_CONTAINER = 6;
  PP_CHANGE_PASSWORD = 7;
  PP_KEYSET_SEC_DESCR = 8; // get/set security descriptor of keyset
  PP_CERTCHAIN = 9; // for retrieving certificates from tokens
  PP_KEY_TYPE_SUBTYPE = 10;
  PP_PROVTYPE = 16;
  PP_KEYSTORAGE = 17;
  PP_APPLI_CERT = 18;
  PP_SYM_KEYSIZE = 19;
  PP_SESSION_KEYSIZE = 20;
  PP_UI_PROMPT = 21;
  PP_ENUMALGS_EX = 22;
  CRYPT_FIRST = 1;
  CRYPT_NEXT = 2;
  CRYPT_IMPL_HARDWARE = 1;
  CRYPT_IMPL_SOFTWARE = 2;
  CRYPT_IMPL_MIXED = 3;
  CRYPT_IMPL_UNKNOWN = 4;

  // key storage flags
  CRYPT_SEC_DESCR = $00000001;
  CRYPT_PSTORE = $00000002;
  CRYPT_UI_PROMPT = $00000004;

  // protocol flags
  CRYPT_FLAG_PCT1 = $0001;
  CRYPT_FLAG_SSL2 = $0002;
  CRYPT_FLAG_SSL3 = $0004;
  CRYPT_FLAG_TLS1 = $0008;

  // CryptSetProvParam
  PP_CLIENT_HWND = 1;
  PP_CONTEXT_INFO = 11;
  PP_KEYEXCHANGE_KEYSIZE = 12;
  PP_SIGNATURE_KEYSIZE = 13;
  PP_KEYEXCHANGE_ALG = 14;
  PP_SIGNATURE_ALG = 15;
  PP_DELETEKEY = 24;

  PROV_RSA_FULL = 1;
  PROV_RSA_SIG = 2;
  PROV_DSS = 3;
  PROV_FORTEZZA = 4;
  PROV_MS_EXCHANGE = 5;
  PROV_SSL = 6;

  PROV_RSA_SCHANNEL = 12;
  PROV_DSS_DH = 13;
  PROV_EC_ECDSA_SIG = 14;
  PROV_EC_ECNRA_SIG = 15;
  PROV_EC_ECDSA_FULL = 16;
  PROV_EC_ECNRA_FULL = 17;
  PROV_SPYRUS_LYNKS = 20;


  // STT defined Providers
  PROV_STT_MER = 7;
  PROV_STT_ACQ = 8;
  PROV_STT_BRND = 9;
  PROV_STT_ROOT = 10;
  PROV_STT_ISS = 11;

  // Provider friendly names
  MS_DEF_PROV_A = 'Microsoft Base Cryptographic Provider v1.0';
{$IFNDEF VER90}
  MS_DEF_PROV_W = WideString('Microsoft Base Cryptographic Provider v1.0');
{$ELSE}
  MS_DEF_PROV_W = ('Microsoft Base Cryptographic Provider v1.0');
{$ENDIF}

{$IFDEF UNICODE}
  MS_DEF_PROV = MS_DEF_PROV_W;
{$ELSE}
  MS_DEF_PROV = MS_DEF_PROV_A;
{$ENDIF}

  MS_ENHANCED_PROV_A = 'Microsoft Enhanced Cryptographic Provider v1.0';
{$IFNDEF VER90}
  MS_ENHANCED_PROV_W = WideString('Microsoft Enhanced Cryptographic Provider v1.0');
{$ELSE}
  MS_ENHANCED_PROV_W = ('Microsoft Enhanced Cryptographic Provider v1.0');
{$ENDIF}

{$IFDEF UNICODE}
  MS_ENHANCED_PROV = MS_ENHANCED_PROV_W;
{$ELSE}
  MS_ENHANCED_PROV = MS_ENHANCED_PROV_A;
{$ENDIF}

  MS_DEF_RSA_SIG_PROV_A = 'Microsoft RSA Signature Cryptographic Provider';
{$IFNDEF VER90}
  MS_DEF_RSA_SIG_PROV_W = WideString('Microsoft RSA Signature Cryptographic Provider');
{$ELSE}
  MS_DEF_RSA_SIG_PROV_W = ('Microsoft RSA Signature Cryptographic Provider');
{$ENDIF}

{$IFDEF UNICODE}
  MS_DEF_RSA_SIG_PROV = MS_DEF_RSA_SIG_PROV_W;
{$ELSE}
  MS_DEF_RSA_SIG_PROV = MS_DEF_RSA_SIG_PROV_A;
{$ENDIF}

  MS_DEF_RSA_SCHANNEL_PROV_A = 'Microsoft Base RSA SChannel Cryptographic Provider';
{$IFNDEF VER90}
  MS_DEF_RSA_SCHANNEL_PROV_W = WideString('Microsoft Base RSA SChannel Cryptographic Provider');
{$ELSE}
  MS_DEF_RSA_SCHANNEL_PROV_W = ('Microsoft Base RSA SChannel Cryptographic Provider');
{$ENDIF}


{$IFDEF UNICODE}
  MS_DEF_RSA_SCHANNEL_PROV = MS_DEF_RSA_SCHANNEL_PROV_W;
{$ELSE}
  MS_DEF_RSA_SCHANNEL_PROV = MS_DEF_RSA_SCHANNEL_PROV_A;
{$ENDIF}

  MS_ENHANCED_RSA_SCHANNEL_PROV_A = 'Microsoft Enhanced RSA SChannel Cryptographic Provider';
{$IFNDEF VER90}
  MS_ENHANCED_RSA_SCHANNEL_PROV_W = WideString('Microsoft Enhanced RSA SChannel Cryptographic Provider');
{$ELSE}
  MS_ENHANCED_RSA_SCHANNEL_PROV_W = ('Microsoft Enhanced RSA SChannel Cryptographic Provider');
{$ENDIF}

{$IFDEF UNICODE}
  MS_ENHANCED_RSA_SCHANNEL_PROV = MS_ENHANCED_RSA_SCHANNEL_PROV_W;
{$ELSE}
  MS_ENHANCED_RSA_SCHANNEL_PROV = MS_ENHANCED_RSA_SCHANNEL_PROV_A;
{$ENDIF}

  MS_DEF_DSS_PROV_A = 'Microsoft Base DSS Cryptographic Provider';
{$IFNDEF VER90}
  MS_DEF_DSS_PROV_W = WideString('Microsoft Base DSS Cryptographic Provider');
{$ELSE}
  MS_DEF_DSS_PROV_W = ('Microsoft Base DSS Cryptographic Provider');
{$ENDIF}

{$IFDEF UNICODE}
  MS_DEF_DSS_PROV = MS_DEF_DSS_PROV_W;
{$ELSE}
  MS_DEF_DSS_PROV = MS_DEF_DSS_PROV_A;
{$ENDIF}

  MS_DEF_DSS_DH_PROV_A = 'Microsoft Base DSS and Diffie-Hellman Cryptographic Provider';
{$IFNDEF VER90}
  MS_DEF_DSS_DH_PROV_W = WideString('Microsoft Base DSS and Diffie-Hellman Cryptographic Provider');
{$ELSE}
  MS_DEF_DSS_DH_PROV_W = ('Microsoft Base DSS and Diffie-Hellman Cryptographic Provider');
{$ENDIF}

{$IFDEF UNICODE}
  MS_DEF_DSS_DH_PROV = MS_DEF_DSS_DH_PROV_W;
{$ELSE}
  MS_DEF_DSS_DH_PROV = MS_DEF_DSS_DH_PROV_A;
{$ENDIF}

  MAXUIDLEN = 64;
  CUR_BLOB_VERSION = 2;

{structure for use with CryptSetHashParam with CALG_HMAC}
type
  PHMAC_INFO = ^HMAC_INFO;
  HMAC_INFO = record
    HashAlgid: ALG_ID;
    pbInnerString: PBYTE;
    cbInnerString: DWORD;
    pbOuterString: PBYTE;
    cbOuterString: DWORD;
  end;

// structure for use with CryptSetHashParam with CALG_HMAC
type
  PSCHANNEL_ALG = ^SCHANNEL_ALG;
  SCHANNEL_ALG = record
    dwUse: DWORD;
    Algid: ALG_ID;
    cBits: DWORD;
  end;

// uses of algortihms for SCHANNEL_ALG structure
const
  SCHANNEL_MAC_KEY = $00000000;
  SCHANNEL_ENC_KEY = $00000001;

type
  PPROV_ENUMALGS = ^PROV_ENUMALGS;
  PROV_ENUMALGS = record
    aiAlgid: ALG_ID;
    dwBitLen: DWORD;
    dwNameLen: DWORD;
    szName: array[0..20 - 1] of Char;
  end;

type
  PPROV_ENUMALGS_EX = ^PROV_ENUMALGS_EX;
  PROV_ENUMALGS_EX = record
    aiAlgid: ALG_ID;
    dwDefaultLen: DWORD;
    dwMinLen: DWORD;
    dwMaxLen: DWORD;
    dwProtocols: DWORD;
    dwNameLen: DWORD;
    szName: array[0..20 - 1] of Char;
    dwLongNameLen: DWORD;
    szLongName: array[0..40 - 1] of Char;
  end;

type
  PPUBLICKEYSTRUC = ^PUBLICKEYSTRUC;
  PUBLICKEYSTRUC = record
    bType: BYTE;
    bVersion: BYTE;
    reserved: Word;
    aiKeyAlg: ALG_ID;
  end;

type
  BLOBHEADER = PUBLICKEYSTRUC;
  PBLOBHEADER = ^BLOBHEADER;

type
  PRSAPUBKEY = ^RSAPUBKEY;
  RSAPUBKEY = record
    magic: DWORD; // Has to be RSA1
    bitlen: DWORD; // # of bits in modulus
    pubexp: DWORD; // public exponent
                    // Modulus data follows
  end;

type
  PPUBKEY = ^PUBKEY;
  PUBKEY = record
    magic: DWORD;
    bitlen: DWORD; // # of bits in modulus
  end;

type
  DHPUBKEY = PUBKEY;
  DSSPUBKEY = PUBKEY;
  KEAPUBKEY = PUBKEY;
  TEKPUBKEY = PUBKEY;


type
  PDSSSEED = ^DSSSEED;
  DSSSEED = record
    counter: DWORD;
    seed: array[0..20 - 1] of BYTE;
  end;

type
  PKEY_TYPE_SUBTYPE = ^KEY_TYPE_SUBTYPE;
  KEY_TYPE_SUBTYPE = record
    dwKeySpec: DWORD;
    Type_: TGUID; {conflict with base Delphi type: original name 'Type'}
    Subtype: TGUID;
  end;

type
  HCRYPTPROV = ULONG;
  PHCRYPTPROV = ^HCRYPTPROV;
  HCRYPTKEY = ULONG;
  PHCRYPTKEY = ^HCRYPTKEY;
  HCRYPTHASH = ULONG;
  PHCRYPTHASH = ^HCRYPTHASH;

function CryptAcquireContextA(phProv: PHCRYPTPROV;
  pszContainer: PAnsiChar;
  pszProvider: PAnsiChar;
  dwProvType: DWORD;
  dwFlags: DWORD): BOOL; stdcall;

function CryptAcquireContext(phProv: PHCRYPTPROV;
  pszContainer: LPAWSTR;
  pszProvider: LPAWSTR;
  dwProvType: DWORD;
  dwFlags: DWORD): BOOL; stdcall;

function CryptAcquireContextW(phProv: PHCRYPTPROV;
  pszContainer: PWideChar;
  pszProvider: PWideChar;
  dwProvType: DWORD;
  dwFlags: DWORD): BOOL; stdcall;


function CryptReleaseContext(hProv: HCRYPTPROV;
  dwFlags: DWORD): BOOL; stdcall;



function CryptGenKey(hProv: HCRYPTPROV;
  Algid: ALG_ID;
  dwFlags: DWORD;
  phKey: PHCRYPTKEY): BOOL; stdcall;


function CryptDeriveKey(hProv: HCRYPTPROV;
  Algid: ALG_ID;
  hBaseData: HCRYPTHASH;
  dwFlags: DWORD;
  phKey: PHCRYPTKEY): BOOL; stdcall;



function CryptDestroyKey(hKey: HCRYPTKEY): BOOL; stdcall;


function CryptSetKeyParam(hKey: HCRYPTKEY;
  dwParam: DWORD;
  pbData: PBYTE;
  dwFlags: DWORD): BOOL; stdcall;


function CryptGetKeyParam(hKey: HCRYPTKEY;
  dwParam: DWORD;
  pbData: PBYTE;
  pdwDataLen: PDWORD;
  dwFlags: DWORD): BOOL; stdcall;


function CryptSetHashParam(hHash: HCRYPTHASH;
  dwParam: DWORD;
  pbData: PBYTE;
  dwFlags: DWORD): BOOL; stdcall;


function CryptGetHashParam(hHash: HCRYPTHASH;
  dwParam: DWORD;
  pbData: PBYTE;
  pdwDataLen: PDWORD;
  dwFlags: DWORD): BOOL; stdcall;


function CryptSetProvParam(hProv: HCRYPTPROV;
  dwParam: DWORD;
  pbData: PBYTE;
  dwFlags: DWORD): BOOL; stdcall;


function CryptGetProvParam(hProv: HCRYPTPROV;
  dwParam: DWORD;
  pbData: PBYTE;
  pdwDataLen: PDWORD;
  dwFlags: DWORD): BOOL; stdcall;


function CryptGenRandom(hProv: HCRYPTPROV;
  dwLen: DWORD;
  pbBuffer: PBYTE): BOOL; stdcall;


function CryptGetUserKey(hProv: HCRYPTPROV;
  dwKeySpec: DWORD;
  phUserKey: PHCRYPTKEY): BOOL; stdcall;


function CryptExportKey(hKey: HCRYPTKEY;
  hExpKey: HCRYPTKEY;
  dwBlobType: DWORD;
  dwFlags: DWORD;
  pbData: PBYTE;
  pdwDataLen: PDWORD): BOOL; stdcall;


function CryptImportKey(hProv: HCRYPTPROV;
  pbData: PBYTE;
  dwDataLen: DWORD;
  hPubKey: HCRYPTKEY;
  dwFlags: DWORD;
  phKey: PHCRYPTKEY): BOOL; stdcall;


function CryptEncrypt(hKey: HCRYPTKEY;
  hHash: HCRYPTHASH;
  Final: BOOL;
  dwFlags: DWORD;
  pbData: PBYTE;
  pdwDataLen: PDWORD;
  dwBufLen: DWORD): BOOL; stdcall;


function CryptDecrypt(hKey: HCRYPTKEY;
  hHash: HCRYPTHASH;
  Final: BOOL;
  dwFlags: DWORD;
  pbData: PBYTE;
  pdwDataLen: PDWORD): BOOL; stdcall;


function CryptCreateHash(hProv: HCRYPTPROV;
  Algid: ALG_ID;
  hKey: HCRYPTKEY;
  dwFlags: DWORD;
  phHash: PHCRYPTHASH): BOOL; stdcall;


function CryptHashData(hHash: HCRYPTHASH;
  const pbData: PBYTE;
  dwDataLen: DWORD;
  dwFlags: DWORD): BOOL; stdcall;


function CryptHashSessionKey(hHash: HCRYPTHASH;
  hKey: HCRYPTKEY;
  dwFlags: DWORD): BOOL; stdcall;


function CryptDestroyHash(hHash: HCRYPTHASH): BOOL; stdcall;


function CryptSignHashA(hHash: HCRYPTHASH;
  dwKeySpec: DWORD;
  sDescription: PAnsiChar;
  dwFlags: DWORD;
  pbSignature: PBYTE;
  pdwSigLen: PDWORD): BOOL; stdcall;


function CryptSignHash(hHash: HCRYPTHASH;
  dwKeySpec: DWORD;
  sDescription: LPAWSTR;
  dwFlags: DWORD;
  pbSignature: PBYTE;
  pdwSigLen: PDWORD): BOOL; stdcall;

function CryptSignHashW(hHash: HCRYPTHASH;
  dwKeySpec: DWORD;
  sDescription: PWideChar;
  dwFlags: DWORD;
  pbSignature: PBYTE;
  pdwSigLen: PDWORD): BOOL; stdcall;

function CryptSignHashU(hHash: HCRYPTHASH;
  dwKeySpec: DWORD;
  sDescription: PWideChar;
  dwFlags: DWORD;
  pbSignature: PBYTE;
  pdwSigLen: PDWORD): BOOL; stdcall;

function CryptVerifySignatureA(hHash: HCRYPTHASH;
  const pbSignature: PBYTE;
  dwSigLen: DWORD;
  hPubKey: HCRYPTKEY;
  sDescription: PAnsiChar;
  dwFlags: DWORD): BOOL; stdcall;

function CryptVerifySignature(hHash: HCRYPTHASH;
  const pbSignature: PBYTE;
  dwSigLen: DWORD;
  hPubKey: HCRYPTKEY;
  sDescription: LPAWSTR;
  dwFlags: DWORD): BOOL; stdcall;


function CryptVerifySignatureW(hHash: HCRYPTHASH;
  const pbSignature: PBYTE;
  dwSigLen: DWORD;
  hPubKey: HCRYPTKEY;
  sDescription: PWideChar;
  dwFlags: DWORD): BOOL; stdcall;


function CryptSetProviderA(pszProvName: PAnsiChar;
  dwProvType: DWORD): BOOL; stdcall;

function CryptSetProvider(pszProvName: LPAWSTR;
  dwProvType: DWORD): BOOL; stdcall;

function CryptSetProviderW(pszProvName: PWideChar;
  dwProvType: DWORD): BOOL; stdcall;

function CryptSetProviderU(pszProvName: PWideChar;
  dwProvType: DWORD): BOOL; stdcall;

{$IFDEF NT5}

function CryptSetProviderExA(pszProvName: LPCSTR;
  dwProvType: DWORD;
  pdwReserved: PDWORD;
  dwFlags: DWORD): BOOL; stdcall;

function CryptSetProviderExW(pszProvName: LPCWSTR;
  dwProvType: DWORD;
  pdwReserved: PDWORD;
  dwFlags: DWORD): BOOL; stdcall;

function CryptSetProviderEx(pszProvName: LPAWSTR;
  dwProvType: DWORD;
  pdwReserved: PDWORD;
  dwFlags: DWORD): BOOL; stdcall;


function CryptGetDefaultProviderA(dwProvType: DWORD;
  pdwReserved: DWORD;
  dwFlags: DWORD;
  pszProvName: LPSTR;
  pcbProvName: PDWORD): BOOL; stdcall;

function CryptGetDefaultProviderW(dwProvType: DWORD;
  pdwReserved: DWORD;
  dwFlags: DWORD;
  pszProvName: LPWSTR;
  pcbProvName: PDWORD): BOOL; stdcall;

function CryptGetDefaultProvider(dwProvType: DWORD;
  pdwReserved: DWORD;
  dwFlags: DWORD;
  pszProvName: LPAWSTR;
  pcbProvName: PDWORD): BOOL; stdcall;

function CryptEnumProviderTypesA(dwIndex: DWORD;
  pdwReserved: PDWORD;
  dwFlags: DWORD;
  pdwProvType: PDWORD;
  pszTypeName: LPSTR;
  pcbTypeName: PDWORD): BOOL; stdcall;

function CryptEnumProviderTypesW(dwIndex: DWORD;
  pdwReserved: PDWORD;
  dwFlags: DWORD;
  pdwProvType: PDWORD;
  pszTypeName: LPWSTR;
  pcbTypeName: PDWORD): BOOL; stdcall;

function CryptEnumProviderTypes(dwIndex: DWORD;
  pdwReserved: PDWORD;
  dwFlags: DWORD;
  pdwProvType: PDWORD;
  pszTypeName: LPAWSTR;
  pcbTypeName: PDWORD): BOOL; stdcall;

function CryptEnumProvidersA(dwIndex: DWORD;
  pdwReserved: PDWORD;
  dwFlags: DWORD;
  pdwProvType: PDWORD;
  pszProvName: LPSTR;
  pcbProvName: PDWORD): BOOL; stdcall;

function CryptEnumProvidersW(dwIndex: DWORD;
  pdwReserved: PDWORD;
  dwFlags: DWORD;
  pdwProvType: PDWORD;
  pszProvName: LPWSTR;
  pcbProvName: PDWORD): BOOL; stdcall;

function CryptEnumProviders(dwIndex: DWORD;
  pdwReserved: PDWORD;
  dwFlags: DWORD;
  pdwProvType: PDWORD;
  pszProvName: LPAWSTR;
  pcbProvName: PDWORD): BOOL; stdcall;

function CryptContextAddRef(hProv: HCRYPTPROV;
  pdwReserved: PDWORD;
  dwFlags: DWORD): BOOL; stdcall;

function CryptDuplicateKey(hKey: HCRYPTKEY;
  pdwReserved: PDWORD;
  dwFlags: DWORD;
  phKey: PHCRYPTKEY): BOOL; stdcall;

function CryptDuplicateHash(hHash: HCRYPTHASH;
  pdwReserved: PDWORD;
  dwFlags: DWORD;
  phHash: PHCRYPTHASH): BOOL; stdcall;

{$ENDIF NT5}

function CryptEnumProvidersU(dwIndex: DWORD;
  pdwReserved: PDWORD;
  dwFlags: DWORD;
  pdwProvType: PDWORD;
  pszProvName: LPWSTR;
  pcbProvName: PDWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  CRYPTOAPI BLOB definitions
//--------------------------------------------------------------------------

type
  PCRYPTOAPI_BLOB = ^CRYPTOAPI_BLOB;
  CRYPTOAPI_BLOB = record
    cbData: DWORD;
    pbData: PBYTE;
  end;

type
  CRYPT_INTEGER_BLOB = CRYPTOAPI_BLOB;
  PCRYPT_INTEGER_BLOB = ^CRYPT_INTEGER_BLOB;
  CRYPT_UINT_BLOB = CRYPTOAPI_BLOB;
  PCRYPT_UINT_BLOB = ^CRYPT_UINT_BLOB;
  CRYPT_OBJID_BLOB = CRYPTOAPI_BLOB;
  PCRYPT_OBJID_BLOB = ^CRYPT_OBJID_BLOB;
  CERT_NAME_BLOB = CRYPTOAPI_BLOB;
  PCERT_NAME_BLOB = ^CERT_NAME_BLOB;
  CERT_RDN_VALUE_BLOB = CRYPTOAPI_BLOB;
  PCERT_RDN_VALUE_BLOB = ^CERT_RDN_VALUE_BLOB;
  CERT_BLOB = CRYPTOAPI_BLOB;
  PCERT_BLOB = ^CERT_BLOB;
  CRL_BLOB = CRYPTOAPI_BLOB;
  PCRL_BLOB = ^CRL_BLOB;
  DATA_BLOB = CRYPTOAPI_BLOB;
  PDATA_BLOB = ^DATA_BLOB; // JEFFJEFF temporary (too generic)
  CRYPT_DATA_BLOB = CRYPTOAPI_BLOB;
  PCRYPT_DATA_BLOB = ^CRYPT_DATA_BLOB;
  CRYPT_HASH_BLOB = CRYPTOAPI_BLOB;
  PCRYPT_HASH_BLOB = ^CRYPT_HASH_BLOB;
  CRYPT_DIGEST_BLOB = CRYPTOAPI_BLOB;
  PCRYPT_DIGEST_BLOB = ^CRYPT_DIGEST_BLOB;
  CRYPT_DER_BLOB = CRYPTOAPI_BLOB;
  PCRYPT_DER_BLOB = ^CRYPT_DER_BLOB;
  CRYPT_ATTR_BLOB = CRYPTOAPI_BLOB;
  PCRYPT_ATTR_BLOB = ^CRYPT_ATTR_BLOB;

//+-------------------------------------------------------------------------
//  In a CRYPT_BIT_BLOB the last byte may contain 0-7 unused bits. Therefore, the
//  overall bit length is cbData * 8 - cUnusedBits.
//--------------------------------------------------------------------------

type
  PCRYPT_BIT_BLOB = ^CRYPT_BIT_BLOB;
  CRYPT_BIT_BLOB = record
    cbData: DWORD;
    pbData: PBYTE;
    cUnusedBits: DWORD;
  end;

//+-------------------------------------------------------------------------
//  Type used for any algorithm
//
//  Where the Parameters CRYPT_OBJID_BLOB is in its encoded representation. For most
//  algorithm types, the Parameters CRYPT_OBJID_BLOB is NULL (Parameters.cbData = 0).
//--------------------------------------------------------------------------

type
  PCRYPT_ALGORITHM_IDENTIFIER = ^CRYPT_ALGORITHM_IDENTIFIER;
  CRYPT_ALGORITHM_IDENTIFIER = record
    pszObjId: LPSTR;
    Parameters: CRYPT_OBJID_BLOB;
  end;

// Following are the definitions of various algorithm object identifiers
// RSA
const
  szOID_RSA = '1.2.840.113549';
  szOID_PKCS = '1.2.840.113549.1';
  szOID_RSA_HASH = '1.2.840.113549.2';
  szOID_RSA_ENCRYPT = '1.2.840.113549.3';

  szOID_PKCS_1 = '1.2.840.113549.1.1';
  szOID_PKCS_2 = '1.2.840.113549.1.2';
  szOID_PKCS_3 = '1.2.840.113549.1.3';
  szOID_PKCS_4 = '1.2.840.113549.1.4';
  szOID_PKCS_5 = '1.2.840.113549.1.5';
  szOID_PKCS_6 = '1.2.840.113549.1.6';
  szOID_PKCS_7 = '1.2.840.113549.1.7';
  szOID_PKCS_8 = '1.2.840.113549.1.8';
  szOID_PKCS_9 = '1.2.840.113549.1.9';
  szOID_PKCS_10 = '1.2.840.113549.1.10';

  szOID_RSA_RSA = '1.2.840.113549.1.1.1';
  szOID_RSA_MD2RSA = '1.2.840.113549.1.1.2';
  szOID_RSA_MD4RSA = '1.2.840.113549.1.1.3';
  szOID_RSA_MD5RSA = '1.2.840.113549.1.1.4';
  szOID_RSA_SHA1RSA = '1.2.840.113549.1.1.5';
  szOID_RSA_SETOAEP_RSA = '1.2.840.113549.1.1.6';

  szOID_RSA_data = '1.2.840.113549.1.7.1';
  szOID_RSA_signedData = '1.2.840.113549.1.7.2';
  szOID_RSA_envelopedData = '1.2.840.113549.1.7.3';
  szOID_RSA_signEnvData = '1.2.840.113549.1.7.4';
  szOID_RSA_digestedData = '1.2.840.113549.1.7.5';
  szOID_RSA_hashedData = '1.2.840.113549.1.7.5';
  szOID_RSA_encryptedData = '1.2.840.113549.1.7.6';

  szOID_RSA_emailAddr = '1.2.840.113549.1.9.1';
  szOID_RSA_unstructName = '1.2.840.113549.1.9.2';
  szOID_RSA_contentType = '1.2.840.113549.1.9.3';
  szOID_RSA_messageDigest = '1.2.840.113549.1.9.4';
  szOID_RSA_signingTime = '1.2.840.113549.1.9.5';
  szOID_RSA_counterSign = '1.2.840.113549.1.9.6';
  szOID_RSA_challengePwd = '1.2.840.113549.1.9.7';
  szOID_RSA_unstructAddr = '1.2.840.113549.1.9.8';
  szOID_RSA_extCertAttrs = '1.2.840.113549.1.9.9';
  szOID_RSA_SMIMECapabilities = '1.2.840.113549.1.9.15';
  szOID_RSA_preferSignedData = '1.2.840.113549.1.9.15.1';

  szOID_RSA_MD2 = '1.2.840.113549.2.2';
  szOID_RSA_MD4 = '1.2.840.113549.2.4';
  szOID_RSA_MD5 = '1.2.840.113549.2.5';

  szOID_RSA_RC2CBC = '1.2.840.113549.3.2';
  szOID_RSA_RC4 = '1.2.840.113549.3.4';
  szOID_RSA_DES_EDE3_CBC = '1.2.840.113549.3.7';
  szOID_RSA_RC5_CBCPad = '1.2.840.113549.3.9';

// ITU-T UsefulDefinitions
  szOID_DS = '2.5';
  szOID_DSALG = '2.5.8';
  szOID_DSALG_CRPT = '2.5.8.1';
  szOID_DSALG_HASH = '2.5.8.2';
  szOID_DSALG_SIGN = '2.5.8.3';
  szOID_DSALG_RSA = '2.5.8.1.1';

// NIST OSE Implementors' Workshop (OIW)
// http://nemo.ncsl.nist.gov/oiw/agreements/stable/OSI/12s_9506.w51
// http://nemo.ncsl.nist.gov/oiw/agreements/working/OSI/12w_9503.w51
  szOID_OIW = '1.3.14';
// NIST OSE Implementors' Workshop (OIW) Security SIG algorithm identifiers
  szOID_OIWSEC = '1.3.14.3.2';
  szOID_OIWSEC_md4RSA = '1.3.14.3.2.2';
  szOID_OIWSEC_md5RSA = '1.3.14.3.2.3';
  szOID_OIWSEC_md4RSA2 = '1.3.14.3.2.4';
  szOID_OIWSEC_desECB = '1.3.14.3.2.6';
  szOID_OIWSEC_desCBC = '1.3.14.3.2.7';
  szOID_OIWSEC_desOFB = '1.3.14.3.2.8';
  szOID_OIWSEC_desCFB = '1.3.14.3.2.9';
  szOID_OIWSEC_desMAC = '1.3.14.3.2.10';
  szOID_OIWSEC_rsaSign = '1.3.14.3.2.11';
  szOID_OIWSEC_dsa = '1.3.14.3.2.12';
  szOID_OIWSEC_shaDSA = '1.3.14.3.2.13';
  szOID_OIWSEC_mdc2RSA = '1.3.14.3.2.14';
  szOID_OIWSEC_shaRSA = '1.3.14.3.2.15';
  szOID_OIWSEC_dhCommMod = '1.3.14.3.2.16';
  szOID_OIWSEC_desEDE = '1.3.14.3.2.17';
  szOID_OIWSEC_sha = '1.3.14.3.2.18';
  szOID_OIWSEC_mdc2 = '1.3.14.3.2.19';
  szOID_OIWSEC_dsaComm = '1.3.14.3.2.20';
  szOID_OIWSEC_dsaCommSHA = '1.3.14.3.2.21';
  szOID_OIWSEC_rsaXchg = '1.3.14.3.2.22';
  szOID_OIWSEC_keyHashSeal = '1.3.14.3.2.23';
  szOID_OIWSEC_md2RSASign = '1.3.14.3.2.24';
  szOID_OIWSEC_md5RSASign = '1.3.14.3.2.25';
  szOID_OIWSEC_sha1 = '1.3.14.3.2.26';
  szOID_OIWSEC_dsaSHA1 = '1.3.14.3.2.27';
  szOID_OIWSEC_dsaCommSHA1 = '1.3.14.3.2.28';
  szOID_OIWSEC_sha1RSASign = '1.3.14.3.2.29';
// NIST OSE Implementors' Workshop (OIW) Directory SIG algorithm identifiers
  szOID_OIWDIR = '1.3.14.7.2';
  szOID_OIWDIR_CRPT = '1.3.14.7.2.1';
  szOID_OIWDIR_HASH = '1.3.14.7.2.2';
  szOID_OIWDIR_SIGN = '1.3.14.7.2.3';
  szOID_OIWDIR_md2 = '1.3.14.7.2.2.1';
  szOID_OIWDIR_md2RSA = '1.3.14.7.2.3.1';


// INFOSEC Algorithms
// joint-iso-ccitt(2) country(16) us(840) organization(1) us-government(101) dod(2) id-infosec(1)
  szOID_INFOSEC = '2.16.840.1.101.2.1';
  szOID_INFOSEC_sdnsSignature = '2.16.840.1.101.2.1.1.1';
  szOID_INFOSEC_mosaicSignature = '2.16.840.1.101.2.1.1.2';
  szOID_INFOSEC_sdnsConfidentiality = '2.16.840.1.101.2.1.1.3';
  szOID_INFOSEC_mosaicConfidentiality = '2.16.840.1.101.2.1.1.4';
  szOID_INFOSEC_sdnsIntegrity = '2.16.840.1.101.2.1.1.5';
  szOID_INFOSEC_mosaicIntegrity = '2.16.840.1.101.2.1.1.6';
  szOID_INFOSEC_sdnsTokenProtection = '2.16.840.1.101.2.1.1.7';
  szOID_INFOSEC_mosaicTokenProtection = '2.16.840.1.101.2.1.1.8';
  szOID_INFOSEC_sdnsKeyManagement = '2.16.840.1.101.2.1.1.9';
  szOID_INFOSEC_mosaicKeyManagement = '2.16.840.1.101.2.1.1.10';
  szOID_INFOSEC_sdnsKMandSig = '2.16.840.1.101.2.1.1.11';
  szOID_INFOSEC_mosaicKMandSig = '2.16.840.1.101.2.1.1.12';
  szOID_INFOSEC_SuiteASignature = '2.16.840.1.101.2.1.1.13';
  szOID_INFOSEC_SuiteAConfidentiality = '2.16.840.1.101.2.1.1.14';
  szOID_INFOSEC_SuiteAIntegrity = '2.16.840.1.101.2.1.1.15';
  szOID_INFOSEC_SuiteATokenProtection = '2.16.840.1.101.2.1.1.16';
  szOID_INFOSEC_SuiteAKeyManagement = '2.16.840.1.101.2.1.1.17';
  szOID_INFOSEC_SuiteAKMandSig = '2.16.840.1.101.2.1.1.18';
  szOID_INFOSEC_mosaicUpdatedSig = '2.16.840.1.101.2.1.1.19';
  szOID_INFOSEC_mosaicKMandUpdSig = '2.16.840.1.101.2.1.1.20';
  szOID_INFOSEC_mosaicUpdatedInteg = '2.16.840.1.101.2.1.1.21';

type
  PCRYPT_OBJID_TABLE = ^CRYPT_OBJID_TABLE;
  CRYPT_OBJID_TABLE = record
    dwAlgId: DWORD;
    pszObjId: LPCSTR;
  end;

//+-------------------------------------------------------------------------
//  PKCS #1 HashInfo (DigestInfo)
//--------------------------------------------------------------------------

type
  PCRYPT_HASH_INFO = ^CRYPT_HASH_INFO;
  CRYPT_HASH_INFO = record
    HashAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    Hash: CRYPT_HASH_BLOB;
  end;

//+-------------------------------------------------------------------------
//  Type used for an extension to an encoded content
//
//  Where the Value's CRYPT_OBJID_BLOB is in its encoded representation.
//--------------------------------------------------------------------------

type
  PCERT_EXTENSION = ^CERT_EXTENSION;
  CERT_EXTENSION = record
    pszObjId: LPSTR;
    fCritical: BOOL;
    Value: CRYPT_OBJID_BLOB;
  end;

//+-------------------------------------------------------------------------
//  AttributeTypeValue
//
//  Where the Value's CRYPT_OBJID_BLOB is in its encoded representation.
//--------------------------------------------------------------------------

type
  PCRYPT_ATTRIBUTE_TYPE_VALUE = ^CRYPT_ATTRIBUTE_TYPE_VALUE;
  CRYPT_ATTRIBUTE_TYPE_VALUE = record
    pszObjId: LPSTR;
    Value: CRYPT_OBJID_BLOB;
  end;

//+-------------------------------------------------------------------------
//  Attributes
//
//  Where the Value's PATTR_BLOBs are in their encoded representation.
//--------------------------------------------------------------------------

type
  PCRYPT_ATTRIBUTE = ^CRYPT_ATTRIBUTE;
  CRYPT_ATTRIBUTE = record
    pszObjId: LPSTR;
    cValue: DWORD;
    rgValue: PCRYPT_ATTR_BLOB;
  end;

type
  PCRYPT_ATTRIBUTES = ^CRYPT_ATTRIBUTES;
  CRYPT_ATTRIBUTES = record
    cAttr: DWORD; {IN}
    rgAttr: PCRYPT_ATTRIBUTE; {IN}
  end;

//+-------------------------------------------------------------------------
//  Attributes making up a Relative Distinguished Name (CERT_RDN)
//
//  The interpretation of the Value depends on the dwValueType.
//  See below for a list of the types.
//--------------------------------------------------------------------------

type
  PCERT_RDN_ATTR = ^CERT_RDN_ATTR;
  CERT_RDN_ATTR = record
    pszObjId: LPSTR;
    dwValueType: DWORD;
    Value: CERT_RDN_VALUE_BLOB;
  end;

//+-------------------------------------------------------------------------
//  CERT_RDN attribute Object Identifiers
//--------------------------------------------------------------------------
// Labeling attribute types:
const
  szOID_COMMON_NAME = '2.5.4.3'; // case-ignore string
  szOID_SUR_NAME = '2.5.4.4'; // case-ignore string
  szOID_DEVICE_SERIAL_NUMBER = '2.5.4.5'; // printable string

// Geographic attribute types:
  szOID_COUNTRY_NAME = '2.5.4.6'; // printable 2char string
  szOID_LOCALITY_NAME = '2.5.4.7'; // case-ignore string
  szOID_STATE_OR_PROVINCE_NAME = '2.5.4.8'; // case-ignore string
  szOID_STREET_ADDRESS = '2.5.4.9'; // case-ignore string

// Organizational attribute types:
  szOID_ORGANIZATION_NAME = '2.5.4.10'; // case-ignore string
  szOID_ORGANIZATIONAL_UNIT_NAME = '2.5.4.11'; // case-ignore string
  szOID_TITLE = '2.5.4.12'; // case-ignore string

// Explanatory attribute types:
  szOID_DESCRIPTION = '2.5.4.13'; // case-ignore string
  szOID_SEARCH_GUIDE = '2.5.4.14';
  szOID_BUSINESS_CATEGORY = '2.5.4.15'; // case-ignore string

// Postal addressing attribute types:
  szOID_POSTAL_ADDRESS = '2.5.4.16';
  szOID_POSTAL_CODE = '2.5.4.17'; // case-ignore string
  szOID_POST_OFFICE_BOX = '2.5.4.18'; // case-ignore string
  szOID_PHYSICAL_DELIVERY_OFFICE_NAME = '2.5.4.19'; // case-ignore string

// Telecommunications addressing attribute types:
  szOID_TELEPHONE_NUMBER = '2.5.4.20'; // telephone number
  szOID_TELEX_NUMBER = '2.5.4.21';
  szOID_TELETEXT_TERMINAL_IDENTIFIER = '2.5.4.22';
  szOID_FACSIMILE_TELEPHONE_NUMBER = '2.5.4.23';
  szOID_X21_ADDRESS = '2.5.4.24'; // numeric string
  szOID_INTERNATIONAL_ISDN_NUMBER = '2.5.4.25'; // numeric string
  szOID_REGISTERED_ADDRESS = '2.5.4.26';
  szOID_DESTINATION_INDICATOR = '2.5.4.27'; // printable string

// Preference attribute types:
  szOID_PREFERRED_DELIVERY_METHOD = '2.5.4.28';

// OSI application attribute types:
  szOID_PRESENTATION_ADDRESS = '2.5.4.29';
  szOID_SUPPORTED_APPLICATION_CONTEXT = '2.5.4.30';

// Relational application attribute types:
  szOID_MEMBER = '2.5.4.31';
  szOID_OWNER = '2.5.4.32';
  szOID_ROLE_OCCUPANT = '2.5.4.33';
  szOID_SEE_ALSO = '2.5.4.34';

// Security attribute types:
  szOID_USER_PASSWORD = '2.5.4.35';
  szOID_USER_CERTIFICATE = '2.5.4.36';
  szOID_CA_CERTIFICATE = '2.5.4.37';
  szOID_AUTHORITY_REVOCATION_LIST = '2.5.4.38';
  szOID_CERTIFICATE_REVOCATION_LIST = '2.5.4.39';
  szOID_CROSS_CERTIFICATE_PAIR = '2.5.4.40';

// Undocumented attribute types???
//#define szOID_???                         '2.5.4.41'
  szOID_GIVEN_NAME = '2.5.4.42'; // case-ignore string
  szOID_INITIALS = '2.5.4.43'; // case-ignore string

// Pilot user attribute types:
  szOID_DOMAIN_COMPONENT = '0.9.2342.19200300.100.1.25'; // IA5 string

//+-------------------------------------------------------------------------
//  CERT_RDN Attribute Value Types
//
//  For RDN_ENCODED_BLOB, the Value's CERT_RDN_VALUE_BLOB is in its encoded
//  representation. Otherwise, its an array of bytes.
//
//  For all CERT_RDN types, Value.cbData is always the number of bytes, not
//  necessarily the number of elements in the string. For instance,
//  RDN_UNIVERSAL_STRING is an array of ints (cbData == intCnt * 4) and
//  RDN_BMP_STRING is an array of unsigned shorts (cbData == ushortCnt * 2).
//
//  For CertDecodeName, two 0 bytes are always appended to the end of the
//  string (ensures a CHAR or WCHAR string is null terminated).
//  These added 0 bytes are't included in the BLOB.cbData.
//--------------------------------------------------------------------------

const
  CERT_RDN_ANY_TYPE = 0;
  CERT_RDN_ENCODED_BLOB = 1;
  CERT_RDN_OCTET_STRING = 2;
  CERT_RDN_NUMERIC_STRING = 3;
  CERT_RDN_PRINTABLE_STRING = 4;
  CERT_RDN_TELETEX_STRING = 5;
  CERT_RDN_T61_STRING = 5;
  CERT_RDN_VIDEOTEX_STRING = 6;
  CERT_RDN_IA5_STRING = 7;
  CERT_RDN_GRAPHIC_STRING = 8;
  CERT_RDN_VISIBLE_STRING = 9;
  CERT_RDN_ISO646_STRING = 9;
  CERT_RDN_GENERAL_STRING = 10;
  CERT_RDN_UNIVERSAL_STRING = 11;
  CERT_RDN_INT4_STRING = 11;
  CERT_RDN_BMP_STRING = 12;
  CERT_RDN_UNICODE_STRING = 12;


// Macro to check that the dwValueType is a character string and not an
// encoded blob or octet string
function IS_CERT_RDN_CHAR_STRING(X: DWORD): BOOL;

//+-------------------------------------------------------------------------
//  A CERT_RDN consists of an array of the above attributes
//--------------------------------------------------------------------------

type
  PCERT_RDN = ^CERT_RDN;
  CERT_RDN = record
    cRDNAttr: DWORD;
    rgRDNAttr: PCERT_RDN_ATTR;
  end;

//+-------------------------------------------------------------------------
//  Information stored in a subject's or issuer's name. The information
//  is represented as an array of the above RDNs.
//--------------------------------------------------------------------------

type
  PCERT_NAME_INFO = ^CERT_NAME_INFO;
  CERT_NAME_INFO = record
    cRDN: DWORD;
    rgRDN: PCERT_RDN;
  end;

//+-------------------------------------------------------------------------
//  Name attribute value without the Object Identifier
//
//  The interpretation of the Value depends on the dwValueType.
//  See above for a list of the types.
//--------------------------------------------------------------------------

type
  PCERT_NAME_VALUE = ^CERT_NAME_VALUE;
  CERT_NAME_VALUE = record
    dwValueType: DWORD;
    Value: CERT_RDN_VALUE_BLOB;
  end;

//+-------------------------------------------------------------------------
//  Public Key Info
//
//  The PublicKey is the encoded representation of the information as it is
//  stored in the bit string
//--------------------------------------------------------------------------

type
  PCERT_PUBLIC_KEY_INFO = ^CERT_PUBLIC_KEY_INFO;
  CERT_PUBLIC_KEY_INFO = record
    Algorithm: CRYPT_ALGORITHM_IDENTIFIER;
    PublicKey: CRYPT_BIT_BLOB;
  end;

const
  CERT_RSA_PUBLIC_KEY_OBJID = szOID_RSA_RSA;
  CERT_DEFAULT_OID_PUBLIC_KEY_SIGN = szOID_RSA_RSA;
  CERT_DEFAULT_OID_PUBLIC_KEY_XCHG = szOID_RSA_RSA;

//+-------------------------------------------------------------------------
//  Information stored in a certificate
//
//  The Issuer, Subject, Algorithm, PublicKey and Extension BLOBs are the
//  encoded representation of the information.
//--------------------------------------------------------------------------

type
  PCERT_INFO = ^CERT_INFO;
  CERT_INFO = record
    dwVersion: DWORD;
    SerialNumber: CRYPT_INTEGER_BLOB;
    SignatureAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    Issuer: CERT_NAME_BLOB;
    NotBefore: TFILETIME;
    NotAfter: TFILETIME;
    Subject: CERT_NAME_BLOB;
    SubjectPublicKeyInfo: CERT_PUBLIC_KEY_INFO;
    IssuerUniqueId: CRYPT_BIT_BLOB;
    SubjectUniqueId: CRYPT_BIT_BLOB;
    cExtension: DWORD;
    rgExtension: PCERT_EXTENSION;
  end;

//+-------------------------------------------------------------------------
//  Certificate versions
//--------------------------------------------------------------------------
const
  CERT_V1 = 0;
  CERT_V2 = 1;
  CERT_V3 = 2;

//+-------------------------------------------------------------------------
//  Certificate Information Flags
//--------------------------------------------------------------------------

  CERT_INFO_VERSION_FLAG = 1;
  CERT_INFO_SERIAL_NUMBER_FLAG = 2;
  CERT_INFO_SIGNATURE_ALGORITHM_FLAG = 3;
  CERT_INFO_ISSUER_FLAG = 4;
  CERT_INFO_NOT_BEFORE_FLAG = 5;
  CERT_INFO_NOT_AFTER_FLAG = 6;
  CERT_INFO_SUBJECT_FLAG = 7;
  CERT_INFO_SUBJECT_PUBLIC_KEY_INFO_FLAG = 8;
  CERT_INFO_ISSUER_UNIQUE_ID_FLAG = 9;
  CERT_INFO_SUBJECT_UNIQUE_ID_FLAG = 10;
  CERT_INFO_EXTENSION_FLAG = 11;

//+-------------------------------------------------------------------------
//  An entry in a CRL
//
//  The Extension BLOBs are the encoded representation of the information.
//--------------------------------------------------------------------------

type
  PCRL_ENTRY = ^CRL_ENTRY;
  CRL_ENTRY = record
    SerialNumber: CRYPT_INTEGER_BLOB;
    RevocationDate: TFILETIME;
    cExtension: DWORD;
    rgExtension: PCERT_EXTENSION;
  end;

//+-------------------------------------------------------------------------
//  Information stored in a CRL
//
//  The Issuer, Algorithm and Extension BLOBs are the encoded
//  representation of the information.
//--------------------------------------------------------------------------

type
  PCRL_INFO = ^CRL_INFO;
  CRL_INFO = record
    dwVersion: DWORD;
    SignatureAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    Issuer: CERT_NAME_BLOB;
    ThisUpdate: TFILETIME;
    NextUpdate: TFILETIME;
    cCRLEntry: DWORD;
    rgCRLEntry: PCRL_ENTRY;
    cExtension: DWORD;
    rgExtension: PCERT_EXTENSION;
  end;

//+-------------------------------------------------------------------------
//  CRL versions
//--------------------------------------------------------------------------
const
  CRL_V1 = 0;
  CRL_V2 = 1;

//+-------------------------------------------------------------------------
//  Information stored in a certificate request
//
//  The Subject, Algorithm, PublicKey and Attribute BLOBs are the encoded
//  representation of the information.
//--------------------------------------------------------------------------

type
  PCERT_REQUEST_INFO = ^CERT_REQUEST_INFO;
  CERT_REQUEST_INFO = record
    dwVersion: DWORD;
    Subject: CERT_NAME_BLOB;
    SubjectPublicKeyInfo: CERT_PUBLIC_KEY_INFO;
    cAttribute: DWORD;
    rgAttribute: PCRYPT_ATTRIBUTE;
  end;

//+-------------------------------------------------------------------------
//  Certificate Request versions
//--------------------------------------------------------------------------
const CERT_REQUEST_V1 = 0;

//+-------------------------------------------------------------------------
//  Information stored in Netscape's Keygen request
//--------------------------------------------------------------------------
type
  PCERT_KEYGEN_REQUEST_INFO = ^CERT_KEYGEN_REQUEST_INFO;
  CERT_KEYGEN_REQUEST_INFO = record
    dwVersion: DWORD;
    SubjectPublicKeyInfo: CERT_PUBLIC_KEY_INFO;
    pwszChallengeString: LPWSTR; // encoded as IA5
  end;

const
  CERT_KEYGEN_REQUEST_V1 = 0;


//+-------------------------------------------------------------------------
//  Certificate, CRL, Certificate Request or Keygen Request Signed Content
//
//  The "to be signed" encoded content plus its signature. The ToBeSigned
//  is the encoded CERT_INFO, CRL_INFO, CERT_REQUEST_INFO or
//  CERT_KEYGEN_REQUEST_INFO.
//--------------------------------------------------------------------------
type
  PCERT_SIGNED_CONTENT_INFO = ^CERT_SIGNED_CONTENT_INFO;
  CERT_SIGNED_CONTENT_INFO = record
    ToBeSigned: CRYPT_DER_BLOB;
    SignatureAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    Signature: CRYPT_BIT_BLOB;
  end;

//+-------------------------------------------------------------------------
//  Certificate Trust List (CTL)
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CTL Usage. Also used for EnhancedKeyUsage extension.
//--------------------------------------------------------------------------

type
  PCTL_USAGE = ^CTL_USAGE;
  CTL_USAGE = record
    cUsageIdentifier: DWORD;
    rgpszUsageIdentifier: PLPSTR; // array of pszObjId
  end;

type
  CERT_ENHKEY_USAGE = CTL_USAGE;
  PCERT_ENHKEY_USAGE = ^CERT_ENHKEY_USAGE;


//+-------------------------------------------------------------------------
//  An entry in a CTL
//--------------------------------------------------------------------------
type
  PCTL_ENTRY = ^CTL_ENTRY;
  CTL_ENTRY = record
    SubjectIdentifier: CRYPT_DATA_BLOB; // For example, its hash
    cAttribute: DWORD;
    rgAttribute: PCRYPT_ATTRIBUTE; // OPTIONAL
  end;

//+-------------------------------------------------------------------------
//  Information stored in a CTL
//--------------------------------------------------------------------------
type
  PCTL_INFO = ^CTL_INFO;
  CTL_INFO = record
    dwVersion: DWORD;
    SubjectUsage: CTL_USAGE;
    ListIdentifier: CRYPT_DATA_BLOB; // OPTIONAL
    SequenceNumber: CRYPT_INTEGER_BLOB; // OPTIONAL
    ThisUpdate: TFILETIME;
    NextUpdate: TFILETIME; // OPTIONAL
    SubjectAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    cCTLEntry: DWORD;
    rgCTLEntry: PCTL_ENTRY; // OPTIONAL
    cExtension: DWORD;
    rgExtension: PCERT_EXTENSION; // OPTIONAL
  end;

//+-------------------------------------------------------------------------
//  CTL versions
//--------------------------------------------------------------------------
const
  CTL_V1 = 0;

//+-------------------------------------------------------------------------
//  TimeStamp Request
//
//  The pszTimeStamp is the OID for the Time type requested
//  The pszContentType is the Content Type OID for the content, usually DATA
//  The Content is a un-decoded blob
//--------------------------------------------------------------------------

type
  PCRYPT_TIME_STAMP_REQUEST_INFO = ^CRYPT_TIME_STAMP_REQUEST_INFO;
  CRYPT_TIME_STAMP_REQUEST_INFO = record
    pszTimeStampAlgorithm: LPSTR; // pszObjId
    pszContentType: LPSTR; // pszObjId
    Content: CRYPT_OBJID_BLOB;
    cAttribute: DWORD;
    rgAttribute: PCRYPT_ATTRIBUTE;
  end;

//+-------------------------------------------------------------------------
//  Certificate and Message encoding types
//
//  The encoding type is a DWORD containing both the certificate and message
//  encoding types. The certificate encoding type is stored in the LOWORD.
//  The message encoding type is stored in the HIWORD. Some functions or
//  structure fields require only one of the encoding types. The following
//  naming convention is used to indicate which encoding type(s) are
//  required:
//      dwEncodingType              (both encoding types are required)
//      dwMsgAndCertEncodingType    (both encoding types are required)
//      dwMsgEncodingType           (only msg encoding type is required)
//      dwCertEncodingType          (only cert encoding type is required)
//
//  Its always acceptable to specify both.
//--------------------------------------------------------------------------

const
  CERT_ENCODING_TYPE_MASK = $0000FFFF;
  CMSG_ENCODING_TYPE_MASK = $FFFF0000;

//#define GET_CERT_ENCODING_TYPE(X)   (X & CERT_ENCODING_TYPE_MASK)
//#define GET_CMSG_ENCODING_TYPE(X)   (X & CMSG_ENCODING_TYPE_MASK)
function GET_CERT_ENCODING_TYPE(X: DWORD): DWORD;
function GET_CMSG_ENCODING_TYPE(X: DWORD): DWORD;

const
  CRYPT_ASN_ENCODING = $00000001;
  CRYPT_NDR_ENCODING = $00000002;
  X509_ASN_ENCODING = $00000001;
  X509_NDR_ENCODING = $00000002;
  PKCS_7_ASN_ENCODING = $00010000;
  PKCS_7_NDR_ENCODING = $00020000;

//+-------------------------------------------------------------------------
//  format the specified data structure according to the certificate
//  encoding type.
//
//--------------------------------------------------------------------------

function CryptFormatObject(dwCertEncodingType: DWORD;
  dwFormatType: DWORD;
  dwFormatStrType: DWORD;
  pFormatStruct: PVOID;
  lpszStructType: LPCSTR;
  const pbEncoded: PBYTE;
  cbEncoded: DWORD;
  pbFormat: PVOID;
  pcbFormat: PDWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Encode / decode the specified data structure according to the certificate
//  encoding type.
//
//  See below for a list of the predefined data structures.
//--------------------------------------------------------------------------

function CryptEncodeObject(dwCertEncodingType: DWORD;
  lpszStructType: LPCSTR;
  const pvStructInfo: PVOID;
  pbEncoded: PBYTE;
  pcbEncoded: PDWORD): BOOL; stdcall;

function CryptDecodeObject(dwCertEncodingType: DWORD;
  lpszStructType: LPCSTR;
  const pbEncoded: PBYTE;
  cbEncoded: DWORD;
  dwFlags: DWORD;
  pvStructInfo: PVOID;
  pcbStructInfo: PDWORD): BOOL; stdcall;

// When the following flag is set the nocopy optimization is enabled.
// This optimization where appropriate, updates the pvStructInfo fields
// to point to content residing within pbEncoded instead of making a copy
// of and appending to pvStructInfo.
//
// Note, when set, pbEncoded can't be freed until pvStructInfo is freed.
const
  CRYPT_DECODE_NOCOPY_FLAG = $1;

//+-------------------------------------------------------------------------
//  Predefined X509 certificate data structures that can be encoded / decoded.
//--------------------------------------------------------------------------
  CRYPT_ENCODE_DECODE_NONE = 0;
  X509_CERT = (LPCSTR(1));
  X509_CERT_TO_BE_SIGNED = (LPCSTR(2));
  X509_CERT_CRL_TO_BE_SIGNED = (LPCSTR(3));
  X509_CERT_REQUEST_TO_BE_SIGNED = (LPCSTR(4));
  X509_EXTENSIONS = (LPCSTR(5));
  X509_NAME_VALUE = (LPCSTR(6));
  X509_NAME = (LPCSTR(7));
  X509_PUBLIC_KEY_INFO = (LPCSTR(8));

//+-------------------------------------------------------------------------
//  Predefined X509 certificate extension data structures that can be
//  encoded / decoded.
//--------------------------------------------------------------------------
  X509_AUTHORITY_KEY_ID = (LPCSTR(9));
  X509_KEY_ATTRIBUTES = (LPCSTR(10));
  X509_KEY_USAGE_RESTRICTION = (LPCSTR(11));
  X509_ALTERNATE_NAME = (LPCSTR(12));
  X509_BASIC_CONSTRAINTS = (LPCSTR(13));
  X509_KEY_USAGE = (LPCSTR(14));
  X509_BASIC_CONSTRAINTS2 = (LPCSTR(15));
  X509_CERT_POLICIES = (LPCSTR(16));

//+-------------------------------------------------------------------------
//  Additional predefined data structures that can be encoded / decoded.
//--------------------------------------------------------------------------
  PKCS_UTC_TIME = (LPCSTR(17));
  PKCS_TIME_REQUEST = (LPCSTR(18));
  RSA_CSP_PUBLICKEYBLOB = (LPCSTR(19));
  X509_UNICODE_NAME = (LPCSTR(20));

  X509_KEYGEN_REQUEST_TO_BE_SIGNED = (LPCSTR(21));
  PKCS_ATTRIBUTE = (LPCSTR(22));
  PKCS_CONTENT_INFO_SEQUENCE_OF_ANY = (LPCSTR(23));

//+-------------------------------------------------------------------------
//  Predefined primitive data structures that can be encoded / decoded.
//--------------------------------------------------------------------------
  X509_UNICODE_NAME_VALUE = (LPCSTR(24));
  X509_ANY_STRING = X509_NAME_VALUE;
  X509_UNICODE_ANY_STRING = X509_UNICODE_NAME_VALUE;
  X509_OCTET_STRING = (LPCSTR(25));
  X509_BITS = (LPCSTR(26));
  X509_INTEGER = (LPCSTR(27));
  X509_MULTI_BYTE_INTEGER = (LPCSTR(28));
  X509_ENUMERATED = (LPCSTR(29));
  X509_CHOICE_OF_TIME = (LPCSTR(30));

//+-------------------------------------------------------------------------
//  More predefined X509 certificate extension data structures that can be
//  encoded / decoded.
//--------------------------------------------------------------------------

  X509_AUTHORITY_KEY_ID2 = (LPCSTR(31));
//  X509_AUTHORITY_INFO_ACCESS          (LPCSTR(32));
  X509_CRL_REASON_CODE = X509_ENUMERATED;
  PKCS_CONTENT_INFO = (LPCSTR(33));
  X509_SEQUENCE_OF_ANY = (LPCSTR(34));
  X509_CRL_DIST_POINTS = (LPCSTR(35));
  X509_ENHANCED_KEY_USAGE = (LPCSTR(36));
  PKCS_CTL = (LPCSTR(37));

  X509_MULTI_BYTE_UINT = (LPCSTR(38));
  X509_DSS_PUBLICKEY = X509_MULTI_BYTE_UINT;
  X509_DSS_PARAMETERS = (LPCSTR(39));
  X509_DSS_SIGNATURE = (LPCSTR(40));
  PKCS_RC2_CBC_PARAMETERS = (LPCSTR(41));
  PKCS_SMIME_CAPABILITIES = (LPCSTR(42));

//+-------------------------------------------------------------------------
//  Predefined PKCS #7 data structures that can be encoded / decoded.
//--------------------------------------------------------------------------
  PKCS7_SIGNER_INFO = (LPCSTR(500));

//+-------------------------------------------------------------------------
//  Predefined Software Publishing Credential (SPC)  data structures that
//  can be encoded / decoded.
//
//  Predefined values: 2000 .. 2999
//
//  See spc.h for value and data structure definitions.
//--------------------------------------------------------------------------
//+-------------------------------------------------------------------------
//  Extension Object Identifiers
//--------------------------------------------------------------------------
const
  szOID_AUTHORITY_KEY_IDENTIFIER = '2.5.29.1';
  szOID_KEY_ATTRIBUTES = '2.5.29.2';
  szOID_KEY_USAGE_RESTRICTION = '2.5.29.4';
  szOID_SUBJECT_ALT_NAME = '2.5.29.7';
  szOID_ISSUER_ALT_NAME = '2.5.29.8';
  szOID_BASIC_CONSTRAINTS = '2.5.29.10';
  szOID_KEY_USAGE = '2.5.29.15';
  szOID_BASIC_CONSTRAINTS2 = '2.5.29.19';
  szOID_CERT_POLICIES = '2.5.29.32';

  szOID_AUTHORITY_KEY_IDENTIFIER2 = '2.5.29.35';
  szOID_SUBJECT_KEY_IDENTIFIER = '2.5.29.14';
  szOID_SUBJECT_ALT_NAME2 = '2.5.29.17';
  szOID_ISSUER_ALT_NAME2 = '2.5.29.18';
  szOID_CRL_REASON_CODE = '2.5.29.21';
  szOID_CRL_DIST_POINTS = '2.5.29.31';
  szOID_ENHANCED_KEY_USAGE = '2.5.29.37';


// Internet Public Key Infrastructure
  szOID_PKIX = '1.3.6.1.5.5.7';
  szOID_AUTHORITY_INFO_ACCESS = '1.3.6.1.5.5.7.2';

// Microsoft extensions or attributes
  szOID_CERT_EXTENSIONS = '1.3.6.1.4.1.311.2.1.14';
  szOID_NEXT_UPDATE_LOCATION = '1.3.6.1.4.1.311.10.2';

//  Microsoft PKCS #7 ContentType Object Identifiers
  szOID_CTL = '1.3.6.1.4.1.311.10.1';

//+-------------------------------------------------------------------------
//  Extension Object Identifiers (currently not implemented)
//--------------------------------------------------------------------------
  szOID_POLICY_MAPPINGS = '2.5.29.5';
  szOID_SUBJECT_DIR_ATTRS = '2.5.29.9';

//+-------------------------------------------------------------------------
//  Enhanced Key Usage (Purpose) Object Identifiers
//--------------------------------------------------------------------------
const szOID_PKIX_KP = '1.3.6.1.5.5.7.3';

// Consistent key usage bits: DIGITAL_SIGNATURE, KEY_ENCIPHERMENT
// or KEY_AGREEMENT
  szOID_PKIX_KP_SERVER_AUTH = '1.3.6.1.5.5.7.3.1';

// Consistent key usage bits: DIGITAL_SIGNATURE
  szOID_PKIX_KP_CLIENT_AUTH = '1.3.6.1.5.5.7.3.2';

// Consistent key usage bits: DIGITAL_SIGNATURE
  szOID_PKIX_KP_CODE_SIGNING = '1.3.6.1.5.5.7.3.3';

// Consistent key usage bits: DIGITAL_SIGNATURE, NON_REPUDIATION and/or
// (KEY_ENCIPHERMENT or KEY_AGREEMENT)
  szOID_PKIX_KP_EMAIL_PROTECTION = '1.3.6.1.5.5.7.3.4';

//+-------------------------------------------------------------------------
//  Microsoft Enhanced Key Usage (Purpose) Object Identifiers
//+-------------------------------------------------------------------------

//  Signer of CTLs
  szOID_KP_CTL_USAGE_SIGNING = '1.3.6.1.4.1.311.10.3.1';

//  Signer of TimeStamps
  szOID_KP_TIME_STAMP_SIGNING = '1.3.6.1.4.1.311.10.3.2';

//+-------------------------------------------------------------------------
//  Microsoft Attribute Object Identifiers
//+-------------------------------------------------------------------------
  szOID_YESNO_TRUST_ATTR = '1.3.6.1.4.1.311.10.4.1';

//+-------------------------------------------------------------------------
//  X509_CERT
//
//  The "to be signed" encoded content plus its signature. The ToBeSigned
//  content is the CryptEncodeObject() output for one of the following:
//  X509_CERT_TO_BE_SIGNED, X509_CERT_CRL_TO_BE_SIGNED or
//  X509_CERT_REQUEST_TO_BE_SIGNED.
//
//  pvStructInfo points to CERT_SIGNED_CONTENT_INFO.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_CERT_TO_BE_SIGNED
//
//  pvStructInfo points to CERT_INFO.
//
//  For CryptDecodeObject(), the pbEncoded is the "to be signed" plus its
//  signature (output of a X509_CERT CryptEncodeObject()).
//
//  For CryptEncodeObject(), the pbEncoded is just the "to be signed".
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_CERT_CRL_TO_BE_SIGNED
//
//  pvStructInfo points to CRL_INFO.
//
//  For CryptDecodeObject(), the pbEncoded is the "to be signed" plus its
//  signature (output of a X509_CERT CryptEncodeObject()).
//
//  For CryptEncodeObject(), the pbEncoded is just the "to be signed".
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_CERT_REQUEST_TO_BE_SIGNED
//
//  pvStructInfo points to CERT_REQUEST_INFO.
//
//  For CryptDecodeObject(), the pbEncoded is the "to be signed" plus its
//  signature (output of a X509_CERT CryptEncodeObject()).
//
//  For CryptEncodeObject(), the pbEncoded is just the "to be signed".
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_EXTENSIONS
//  szOID_CERT_EXTENSIONS
//
//  pvStructInfo points to following CERT_EXTENSIONS.
//--------------------------------------------------------------------------
type
  PCERT_EXTENSIONS = ^CERT_EXTENSIONS;
  CERT_EXTENSIONS = record
    cExtension: DWORD;
    rgExtension: PCERT_EXTENSION;
  end;

//+-------------------------------------------------------------------------
//  X509_NAME_VALUE
//  X509_ANY_STRING
//
//  pvStructInfo points to CERT_NAME_VALUE.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_UNICODE_NAME_VALUE
//  X509_UNICODE_ANY_STRING
//
//  pvStructInfo points to CERT_NAME_VALUE.
//
//  The name values are unicode strings.
//
//  For CryptEncodeObject:
//    Value.pbData points to the unicode string.
//    If Value.cbData = 0, then, the unicode string is NULL terminated.
//    Otherwise, Value.cbData is the unicode string byte count. The byte count
//    is twice the character count.
//
//    If the unicode string contains an invalid character for the specified
//    dwValueType, then, *pcbEncoded is updated with the unicode character
//    index of the first invalid character. LastError is set to:
//    CRYPT_E_INVALID_NUMERIC_STRING, CRYPT_E_INVALID_PRINTABLE_STRING or
//    CRYPT_E_INVALID_IA5_STRING.
//
//    The unicode string is converted before being encoded according to
//    the specified dwValueType. If dwValueType is set to 0, LastError
//    is set to E_INVALIDARG.
//
//    If the dwValueType isn't one of the character strings (its a
//    CERT_RDN_ENCODED_BLOB or CERT_RDN_OCTET_STRING), then, CryptEncodeObject
//    will return FALSE with LastError set to CRYPT_E_NOT_CHAR_STRING.
//
//  For CryptDecodeObject:
//    Value.pbData points to a NULL terminated unicode string. Value.cbData
//    contains the byte count of the unicode string excluding the NULL
//    terminator. dwValueType contains the type used in the encoded object.
//    Its not forced to CERT_RDN_UNICODE_STRING. The encoded value is
//    converted to the unicode string according to the dwValueType.
//
//    If the encoded object isn't one of the character string types, then,
//    CryptDecodeObject will return FALSE with LastError set to
//    CRYPT_E_NOT_CHAR_STRING. For a non character string, decode using
//    X509_NAME_VALUE or X509_ANY_STRING.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_NAME
//
//  pvStructInfo points to CERT_NAME_INFO.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_UNICODE_NAME
//
//  pvStructInfo points to CERT_NAME_INFO.
//
//  The RDN attribute values are unicode strings except for the dwValueTypes of
//  CERT_RDN_ENCODED_BLOB or CERT_RDN_OCTET_STRING. These dwValueTypes are
//  the same as for a X509_NAME. Their values aren't converted to/from unicode.
//
//  For CryptEncodeObject:
//    Value.pbData points to the unicode string.
//    If Value.cbData = 0, then, the unicode string is NULL terminated.
//    Otherwise, Value.cbData is the unicode string byte count. The byte count
//    is twice the character count.
//
//    If dwValueType = 0 (CERT_RDN_ANY_TYPE), the pszObjId is used to find
//    an acceptable dwValueType. If the unicode string contains an
//    invalid character for the found or specified dwValueType, then,
//    *pcbEncoded is updated with the error location of the invalid character.
//    See below for details. LastError is set to:
//    CRYPT_E_INVALID_NUMERIC_STRING, CRYPT_E_INVALID_PRINTABLE_STRING or
//    CRYPT_E_INVALID_IA5_STRING.
//
//    The unicode string is converted before being encoded according to
//    the specified or ObjId matching dwValueType.
//
//  For CryptDecodeObject:
//    Value.pbData points to a NULL terminated unicode string. Value.cbData
//    contains the byte count of the unicode string excluding the NULL
//    terminator. dwValueType contains the type used in the encoded object.
//    Its not forced to CERT_RDN_UNICODE_STRING. The encoded value is
//    converted to the unicode string according to the dwValueType.
//
//    If the dwValueType of the encoded value isn't a character string
//    type, then, it isn't converted to UNICODE. Use the
//    IS_CERT_RDN_CHAR_STRING() macro on the dwValueType to check
//    that Value.pbData points to a converted unicode string.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  Unicode Name Value Error Location Definitions
//
//  Error location is returned in *pcbEncoded by
//  CryptEncodeObject(X509_UNICODE_NAME)
//
//  Error location consists of:
//    RDN_INDEX     - 10 bits << 22
//    ATTR_INDEX    - 6 bits << 16
//    VALUE_INDEX   - 16 bits (unicode character index)
//--------------------------------------------------------------------------
const
  CERT_UNICODE_RDN_ERR_INDEX_MASK = $3FF;
  CERT_UNICODE_RDN_ERR_INDEX_SHIFT = 22;
  CERT_UNICODE_ATTR_ERR_INDEX_MASK = $003F;
  CERT_UNICODE_ATTR_ERR_INDEX_SHIFT = 16;
  CERT_UNICODE_VALUE_ERR_INDEX_MASK = $0000FFFF;
  CERT_UNICODE_VALUE_ERR_INDEX_SHIFT = 0;

{#define GET_CERT_UNICODE_RDN_ERR_INDEX(X)   \
    ((X >> CERT_UNICODE_RDN_ERR_INDEX_SHIFT) & CERT_UNICODE_RDN_ERR_INDEX_MASK)}
function GET_CERT_UNICODE_RDN_ERR_INDEX(X: integer): integer;
{#define GET_CERT_UNICODE_ATTR_ERR_INDEX(X)  \
    ((X >> CERT_UNICODE_ATTR_ERR_INDEX_SHIFT) & CERT_UNICODE_ATTR_ERR_INDEX_MASK)}
function GET_CERT_UNICODE_ATTR_ERR_INDEX(X: integer): integer;
{#define GET_CERT_UNICODE_VALUE_ERR_INDEX(X) \
    (X & CERT_UNICODE_VALUE_ERR_INDEX_MASK)}
function GET_CERT_UNICODE_VALUE_ERR_INDEX(X: integer): integer;

//+-------------------------------------------------------------------------
//  X509_PUBLIC_KEY_INFO
//
//  pvStructInfo points to CERT_PUBLIC_KEY_INFO.
//--------------------------------------------------------------------------


//+-------------------------------------------------------------------------
//  X509_AUTHORITY_KEY_ID
//  szOID_AUTHORITY_KEY_IDENTIFIER
//
//  pvStructInfo points to following CERT_AUTHORITY_KEY_ID_INFO.
//--------------------------------------------------------------------------
type
  PCERT_AUTHORITY_KEY_ID_INFO = ^CERT_AUTHORITY_KEY_ID_INFO;
  CERT_AUTHORITY_KEY_ID_INFO = record
    KeyId: CRYPT_DATA_BLOB;
    CertIssuer: CERT_NAME_BLOB;
    CertSerialNumber: CRYPT_INTEGER_BLOB;
  end;

//+-------------------------------------------------------------------------
//  X509_KEY_ATTRIBUTES
//  szOID_KEY_ATTRIBUTES
//
//  pvStructInfo points to following CERT_KEY_ATTRIBUTES_INFO.
//--------------------------------------------------------------------------
type
  PCERT_PRIVATE_KEY_VALIDITY = ^CERT_PRIVATE_KEY_VALIDITY;
  CERT_PRIVATE_KEY_VALIDITY = record
    NotBefore: TFILETIME;
    NotAfter: TFILETIME;
  end;

type
  PCERT_KEY_ATTRIBUTES_INFO = ^CERT_KEY_ATTRIBUTES_INFO;
  CERT_KEY_ATTRIBUTES_INFO = record
    KeyId: CRYPT_DATA_BLOB;
    IntendedKeyUsage: CRYPT_BIT_BLOB;
    pPrivateKeyUsagePeriod: PCERT_PRIVATE_KEY_VALIDITY; // OPTIONAL
  end;

const
  CERT_DIGITAL_SIGNATURE_KEY_USAGE = $80;
  CERT_NON_REPUDIATION_KEY_USAGE = $40;
  CERT_KEY_ENCIPHERMENT_KEY_USAGE = $20;
  CERT_DATA_ENCIPHERMENT_KEY_USAGE = $10;
  CERT_KEY_AGREEMENT_KEY_USAGE = $08;
  CERT_KEY_CERT_SIGN_KEY_USAGE = $04;
  CERT_OFFLINE_CRL_SIGN_KEY_USAGE = $02;

  CERT_CRL_SIGN_KEY_USAGE = $02;

//+-------------------------------------------------------------------------
//  X509_KEY_USAGE_RESTRICTION
//  szOID_KEY_USAGE_RESTRICTION
//
//  pvStructInfo points to following CERT_KEY_USAGE_RESTRICTION_INFO.
//--------------------------------------------------------------------------
type
  PCERT_POLICY_ID = ^CERT_POLICY_ID;
  CERT_POLICY_ID = record
    cCertPolicyElementId: DWORD;
    rgpszCertPolicyElementId: PLPSTR; // pszObjId
  end;

type
  PCERT_KEY_USAGE_RESTRICTION_INFO = ^CERT_KEY_USAGE_RESTRICTION_INFO;
  CERT_KEY_USAGE_RESTRICTION_INFO = record
    cCertPolicyId: DWORD;
    rgCertPolicyId: PCERT_POLICY_ID;
    RestrictedKeyUsage: CRYPT_BIT_BLOB;
  end;

// See CERT_KEY_ATTRIBUTES_INFO for definition of the RestrictedKeyUsage bits

//+-------------------------------------------------------------------------
//  X509_ALTERNATE_NAME
//  szOID_SUBJECT_ALT_NAME
//  szOID_ISSUER_ALT_NAME
//  szOID_SUBJECT_ALT_NAME2
//  szOID_ISSUER_ALT_NAME2
//
//  pvStructInfo points to following CERT_ALT_NAME_INFO.
//--------------------------------------------------------------------------

type
  PCERT_ALT_NAME_ENTRY = ^CERT_ALT_NAME_ENTRY;
  CERT_ALT_NAME_ENTRY = record
    dwAltNameChoice: DWORD;
    case integer of
    {1}0: ({OtherName :Not implemented});
    {2}1: (pwszRfc822Name: LPWSTR); //(encoded IA5)
    {3}2: (pwszDNSName: LPWSTR); //(encoded IA5)
    {4}3: ({x400Address    :Not implemented});
    {5}4: (DirectoryName: CERT_NAME_BLOB);
    {6}5: ({pEdiPartyName  :Not implemented});
    {7}6: (pwszURL: LPWSTR); //(encoded IA5)
    {8}7: (IPAddress: CRYPT_DATA_BLOB); //(Octet String)
    {9}8: (pszRegisteredID: LPSTR); //(Octet String)
  end;

const
  CERT_ALT_NAME_OTHER_NAME = 1;
  CERT_ALT_NAME_RFC822_NAME = 2;
  CERT_ALT_NAME_DNS_NAME = 3;
  CERT_ALT_NAME_X400_ADDRESS = 4;
  CERT_ALT_NAME_DIRECTORY_NAME = 5;
  CERT_ALT_NAME_EDI_PARTY_NAME = 6;
  CERT_ALT_NAME_URL = 7;
  CERT_ALT_NAME_IP_ADDRESS = 8;
  CERT_ALT_NAME_REGISTERED_ID = 9;

type
  PCERT_ALT_NAME_INFO = ^CERT_ALT_NAME_INFO;
  CERT_ALT_NAME_INFO = record
    cAltEntry: DWORD;
    rgAltEntry: PCERT_ALT_NAME_ENTRY;
  end;

//+-------------------------------------------------------------------------
//  Alternate name IA5 Error Location Definitions for
//  CRYPT_E_INVALID_IA5_STRING.
//
//  Error location is returned in *pcbEncoded by
//  CryptEncodeObject(X509_ALTERNATE_NAME)
//
//  Error location consists of:
//    ENTRY_INDEX   - 8 bits << 16
//    VALUE_INDEX   - 16 bits (unicode character index)
//--------------------------------------------------------------------------

const
  CERT_ALT_NAME_ENTRY_ERR_INDEX_MASK = $FF;
  CERT_ALT_NAME_ENTRY_ERR_INDEX_SHIFT = 16;
  CERT_ALT_NAME_VALUE_ERR_INDEX_MASK = $0000FFFF;
  CERT_ALT_NAME_VALUE_ERR_INDEX_SHIFT = 0;

{#define GET_CERT_ALT_NAME_ENTRY_ERR_INDEX(X)   \
    ((X >> CERT_ALT_NAME_ENTRY_ERR_INDEX_SHIFT) & \
                                  CERT_ALT_NAME_ENTRY_ERR_INDEX_MASK)}
function GET_CERT_ALT_NAME_ENTRY_ERR_INDEX(X: DWORD): DWORD;

{#define GET_CERT_ALT_NAME_VALUE_ERR_INDEX(X) \
    (X & CERT_ALT_NAME_VALUE_ERR_INDEX_MASK)}
function GET_CERT_ALT_NAME_VALUE_ERR_INDEX(X: DWORD): DWORD;

//+-------------------------------------------------------------------------
//  X509_BASIC_CONSTRAINTS
//  szOID_BASIC_CONSTRAINTS
//
//  pvStructInfo points to following CERT_BASIC_CONSTRAINTS_INFO.
//--------------------------------------------------------------------------

type
  PCERT_BASIC_CONSTRAINTS_INFO = ^CERT_BASIC_CONSTRAINTS_INFO;
  CERT_BASIC_CONSTRAINTS_INFO = record
    SubjectType: CRYPT_BIT_BLOB;
    fPathLenConstraint: BOOL;
    dwPathLenConstraint: DWORD;
    cSubtreesConstraint: DWORD;
    rgSubtreesConstraint: PCERT_NAME_BLOB;
  end;

const
  CERT_CA_SUBJECT_FLAG = $80;
  CERT_END_ENTITY_SUBJECT_FLAG = $40;

//+-------------------------------------------------------------------------
//  X509_BASIC_CONSTRAINTS2
//  szOID_BASIC_CONSTRAINTS2
//
//  pvStructInfo points to following CERT_BASIC_CONSTRAINTS2_INFO.
//--------------------------------------------------------------------------

type
  PCERT_BASIC_CONSTRAINTS2_INFO = ^CERT_BASIC_CONSTRAINTS2_INFO;
  CERT_BASIC_CONSTRAINTS2_INFO = record
    fCA: BOOL;
    fPathLenConstraint: BOOL;
    dwPathLenConstraint: DWORD;
  end;

//+-------------------------------------------------------------------------
//  X509_KEY_USAGE
//  szOID_KEY_USAGE
//
//  pvStructInfo points to a CRYPT_BIT_BLOB. Has same bit definitions as
//  CERT_KEY_ATTRIBUTES_INFO's IntendedKeyUsage.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_CERT_POLICIES
//  szOID_CERT_POLICIES
//
//  pvStructInfo points to following CERT_POLICIES_INFO.
//--------------------------------------------------------------------------

type
  PCERT_POLICY_QUALIFIER_INFO = ^CERT_POLICY_QUALIFIER_INFO;
  CERT_POLICY_QUALIFIER_INFO = record
    pszPolicyQualifierId: LPSTR; // pszObjId
    Qualifier: CRYPT_OBJID_BLOB; // optional
  end;

type
  PCERT_POLICY_INFO = ^CERT_POLICY_INFO;
  CERT_POLICY_INFO = record
    pszPolicyIdentifier: LPSTR; // pszObjId
    cPolicyQualifier: DWORD; // optional
    rgPolicyQualifier: PCERT_POLICY_QUALIFIER_INFO;
  end;

type
  PCERT_POLICIES_INFO = ^CERT_POLICIES_INFO;
  CERT_POLICIES_INFO = record
    cPolicyInfo: DWORD;
    rgPolicyInfo: PCERT_POLICY_INFO;
  end;

//+-------------------------------------------------------------------------
//  RSA_CSP_PUBLICKEYBLOB
//
//  pvStructInfo points to a PUBLICKEYSTRUC immediately followed by a
//  RSAPUBKEY and the modulus bytes.
//
//  CryptExportKey outputs the above StructInfo for a dwBlobType of
//  PUBLICKEYBLOB. CryptImportKey expects the above StructInfo when
//  importing a public key.
//
//  For dwCertEncodingType = X509_ASN_ENCODING, the RSA_CSP_PUBLICKEYBLOB is
//  encoded as a PKCS #1 RSAPublicKey consisting of a SEQUENCE of a
//  modulus INTEGER and a publicExponent INTEGER. The modulus is encoded
//  as being a unsigned integer. When decoded, if the modulus was encoded
//  as unsigned integer with a leading 0 byte, the 0 byte is removed before
//  converting to the CSP modulus bytes.
//
//  For decode, the aiKeyAlg field of PUBLICKEYSTRUC is always set to
//  CALG_RSA_KEYX.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_KEYGEN_REQUEST_TO_BE_SIGNED
//
//  pvStructInfo points to CERT_KEYGEN_REQUEST_INFO.
//
//  For CryptDecodeObject(), the pbEncoded is the "to be signed" plus its
//  signature (output of a X509_CERT CryptEncodeObject()).
//
//  For CryptEncodeObject(), the pbEncoded is just the "to be signed".
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  PKCS_ATTRIBUTE data structure
//
//  pvStructInfo points to a CRYPT_ATTRIBUTE.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  PKCS_CONTENT_INFO_SEQUENCE_OF_ANY data structure
//
//  pvStructInfo points to following CRYPT_CONTENT_INFO_SEQUENCE_OF_ANY.
//
//  For X509_ASN_ENCODING: encoded as a PKCS#7 ContentInfo structure wrapping
//  a sequence of ANY. The value of the contentType field is pszObjId,
//  while the content field is the following structure:
//      SequenceOfAny ::= SEQUENCE OF ANY
//
//  The CRYPT_DER_BLOBs point to the already encoded ANY content.
//--------------------------------------------------------------------------

type
  PCRYPT_CONTENT_INFO_SEQUENCE_OF_ANY = ^CRYPT_CONTENT_INFO_SEQUENCE_OF_ANY;
  CRYPT_CONTENT_INFO_SEQUENCE_OF_ANY = record
    pszObjId: LPSTR;
    cValue: DWORD;
    rgValue: PCRYPT_DER_BLOB;
  end;

//+-------------------------------------------------------------------------
//  PKCS_CONTENT_INFO data structure
//
//  pvStructInfo points to following CRYPT_CONTENT_INFO.
//
//  For X509_ASN_ENCODING: encoded as a PKCS#7 ContentInfo structure.
//  The CRYPT_DER_BLOB points to the already encoded ANY content.
//--------------------------------------------------------------------------

type
  PCRYPT_CONTENT_INFO = ^CRYPT_CONTENT_INFO;
  CRYPT_CONTENT_INFO = record
    pszObjId: LPSTR;
    Content: CRYPT_DER_BLOB;
  end;


//+-------------------------------------------------------------------------
//  X509_OCTET_STRING data structure
//
//  pvStructInfo points to a CRYPT_DATA_BLOB.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_BITS data structure
//
//  pvStructInfo points to a CRYPT_BIT_BLOB.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_INTEGER data structure
//
//  pvStructInfo points to an int.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_MULTI_BYTE_INTEGER data structure
//
//  pvStructInfo points to a CRYPT_INTEGER_BLOB.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_ENUMERATED data structure
//
//  pvStructInfo points to an int containing the enumerated value
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_CHOICE_OF_TIME data structure
//
//  pvStructInfo points to a FILETIME.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_SEQUENCE_OF_ANY data structure
//
//  pvStructInfo points to following CRYPT_SEQUENCE_OF_ANY.
//
//  The CRYPT_DER_BLOBs point to the already encoded ANY content.
//--------------------------------------------------------------------------

type
  PCRYPT_SEQUENCE_OF_ANY = ^CRYPT_SEQUENCE_OF_ANY;
  CRYPT_SEQUENCE_OF_ANY = record
    cValue: DWORD;
    rgValue: PCRYPT_DER_BLOB;
  end;

//+-------------------------------------------------------------------------
//  X509_AUTHORITY_KEY_ID2
//  szOID_AUTHORITY_KEY_IDENTIFIER2
//
//  pvStructInfo points to following CERT_AUTHORITY_KEY_ID2_INFO.
//
//  For CRYPT_E_INVALID_IA5_STRING, the error location is returned in
//  *pcbEncoded by CryptEncodeObject(X509_AUTHORITY_KEY_ID2)
//
//  See X509_ALTERNATE_NAME for error location defines.
//--------------------------------------------------------------------------

type
  PCERT_AUTHORITY_KEY_ID2_INFO = ^CERT_AUTHORITY_KEY_ID2_INFO;
  CERT_AUTHORITY_KEY_ID2_INFO = record
    KeyId: CRYPT_DATA_BLOB;
    AuthorityCertIssuer: CERT_ALT_NAME_INFO; // Optional, set cAltEntry to 0 to omit.
    AuthorityCertSerialNumber: CRYPT_INTEGER_BLOB;
  end;

//+-------------------------------------------------------------------------
//  szOID_SUBJECT_KEY_IDENTIFIER
//
//  pvStructInfo points to a CRYPT_DATA_BLOB.
//--------------------------------------------------------------------------


//+-------------------------------------------------------------------------
//  X509_CRL_REASON_CODE
//  szOID_CRL_REASON_CODE
//
//  pvStructInfo points to an int which can be set to one of the following
//  enumerated values:
//--------------------------------------------------------------------------

const
  CRL_REASON_UNSPECIFIED = 0;
  CRL_REASON_KEY_COMPROMISE = 1;
  CRL_REASON_CA_COMPROMISE = 2;
  CRL_REASON_AFFILIATION_CHANGED = 3;
  CRL_REASON_SUPERSEDED = 4;
  CRL_REASON_CESSATION_OF_OPERATION = 5;
  CRL_REASON_CERTIFICATE_HOLD = 6;
  CRL_REASON_REMOVE_FROM_CRL = 8;

//+-------------------------------------------------------------------------
//  X509_CRL_DIST_POINTS
//  szOID_CRL_DIST_POINTS
//
//  pvStructInfo points to following CRL_DIST_POINTS_INFO.
//
//  For CRYPT_E_INVALID_IA5_STRING, the error location is returned in
//  *pcbEncoded by CryptEncodeObject(X509_CRL_DIST_POINTS)
//
//  Error location consists of:
//    CRL_ISSUER_BIT    - 1 bit  << 31 (0 for FullName, 1 for CRLIssuer)
//    POINT_INDEX       - 7 bits << 24
//    ENTRY_INDEX       - 8 bits << 16
//    VALUE_INDEX       - 16 bits (unicode character index)
//
//  See X509_ALTERNATE_NAME for ENTRY_INDEX and VALUE_INDEX error location
//  defines.
//--------------------------------------------------------------------------

type
  PCRL_DIST_POINT_NAME = ^CRL_DIST_POINT_NAME;
  CRL_DIST_POINT_NAME = record
    dwDistPointNameChoice: DWORD;
    case integer of
      0: (FullName: CERT_ALT_NAME_INFO); {1}
      1: ({IssuerRDN :Not implemented}); {2}
  end;

const
  CRL_DIST_POINT_NO_NAME = 0;
  CRL_DIST_POINT_FULL_NAME = 1;
  CRL_DIST_POINT_ISSUER_RDN_NAME = 2;

type
  PCRL_DIST_POINT = ^CRL_DIST_POINT;
  CRL_DIST_POINT = record
    DistPointName: CRL_DIST_POINT_NAME; // OPTIONAL
    ReasonFlags: CRYPT_BIT_BLOB; // OPTIONAL
    CRLIssuer: CERT_ALT_NAME_INFO; // OPTIONAL
  end;

const
  CRL_REASON_UNUSED_FLAG = $80;
  CRL_REASON_KEY_COMPROMISE_FLAG = $40;
  CRL_REASON_CA_COMPROMISE_FLAG = $20;
  CRL_REASON_AFFILIATION_CHANGED_FLAG = $10;
  CRL_REASON_SUPERSEDED_FLAG = $08;
  CRL_REASON_CESSATION_OF_OPERATION_FLAG = $04;
  CRL_REASON_CERTIFICATE_HOLD_FLAG = $02;

type
  PCRL_DIST_POINTS_INFO = ^CRL_DIST_POINTS_INFO;
  CRL_DIST_POINTS_INFO = record
    cDistPoint: DWORD;
    rgDistPoint: PCRL_DIST_POINT;
  end;

const
  CRL_DIST_POINT_ERR_INDEX_MASK = $7F;
  CRL_DIST_POINT_ERR_INDEX_SHIFT = 24;

{#define GET_CRL_DIST_POINT_ERR_INDEX(X)   \
    ((X >> CRL_DIST_POINT_ERR_INDEX_SHIFT) & CRL_DIST_POINT_ERR_INDEX_MASK)}
function GET_CRL_DIST_POINT_ERR_INDEX(X: DWORD): DWORD;
const CRL_DIST_POINT_ERR_CRL_ISSUER_BIT = (DWORD($80000000));

{#define IS_CRL_DIST_POINT_ERR_CRL_ISSUER(X)   \
    (0 != (X & CRL_DIST_POINT_ERR_CRL_ISSUER_BIT))}
function IS_CRL_DIST_POINT_ERR_CRL_ISSUER(X: DWORD): BOOL;

//+-------------------------------------------------------------------------
//  X509_ENHANCED_KEY_USAGE
//  szOID_ENHANCED_KEY_USAGE
//
//  pvStructInfo points to a CERT_ENHKEY_USAGE, CTL_USAGE.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  szOID_NEXT_UPDATE_LOCATION
//
//  pvStructInfo points to a CERT_ALT_NAME_INFO.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  PKCS_CTL
//  szOID_CTL
//
//  pvStructInfo points to a CTL_INFO.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_MULTI_BYTE_UINT
//
//  pvStructInfo points to a CRYPT_UINT_BLOB. Before encoding, inserts a
//  leading 0x00. After decoding, removes a leading 0x00.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_DSS_PUBLICKEY
//
//  pvStructInfo points to a CRYPT_UINT_BLOB.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_DSS_PARAMETERS
//
//  pvStructInfo points to following CERT_DSS_PARAMETERS data structure.
//--------------------------------------------------------------------------

type
  PCERT_DSS_PARAMETERS = ^CERT_DSS_PARAMETERS;
  CERT_DSS_PARAMETERS = record
    p: CRYPT_UINT_BLOB;
    q: CRYPT_UINT_BLOB;
    g: CRYPT_UINT_BLOB;
  end;

//+-------------------------------------------------------------------------
//  X509_DSS_SIGNATURE
//
//  pvStructInfo is a BYTE rgbSignature[CERT_DSS_SIGNATURE_LEN]. The
//  bytes are ordered as output by the DSS CSP's CryptSignHash().
//--------------------------------------------------------------------------

const
  CERT_DSS_R_LEN = 20;
  CERT_DSS_S_LEN = 20;
  CERT_DSS_SIGNATURE_LEN = (CERT_DSS_R_LEN + CERT_DSS_S_LEN);

// Sequence of 2 unsigned integers (the extra +1 is for a potential leading
// 0x00 to make the integer unsigned)
  CERT_MAX_ASN_ENCODED_DSS_SIGNATURE_LEN = (2 + 2 * (2 + 20 + 1));

//+-------------------------------------------------------------------------
//  PKCS_RC2_CBC_PARAMETERS
//  szOID_RSA_RC2CBC
//
//  pvStructInfo points to following CRYPT_RC2_CBC_PARAMETERS data structure.
//--------------------------------------------------------------------------

type
  PCRYPT_RC2_CBC_PARAMETERS = ^CRYPT_RC2_CBC_PARAMETERS;
  CRYPT_RC2_CBC_PARAMETERS = record
    dwVersion: DWORD;
    fIV: BOOL; // set if has following IV
    rgbIV: array[0..8 - 1] of BYTE;
  end;

const
  CRYPT_RC2_40BIT_VERSION = 160;
  CRYPT_RC2_64BIT_VERSION = 120;
  CRYPT_RC2_128BIT_VERSION = 58;


//+-------------------------------------------------------------------------
//  PKCS_SMIME_CAPABILITIES
//  szOID_RSA_SMIMECapabilities
//
//  pvStructInfo points to following CRYPT_SMIME_CAPABILITIES data structure.
//
//  Note, for CryptEncodeObject(X509_ASN_ENCODING), Parameters.cbData == 0
//  causes the encoded parameters to be omitted and not encoded as a NULL
//  (05 00) as is done when encoding a CRYPT_ALGORITHM_IDENTIFIER. This
//  is per the SMIME specification for encoding capabilities.
//--------------------------------------------------------------------------

type
  PCRYPT_SMIME_CAPABILITY = ^CRYPT_SMIME_CAPABILITY;
  CRYPT_SMIME_CAPABILITY = record
    pszObjId: LPSTR;
    Parameters: CRYPT_OBJID_BLOB;
  end;

type
  PCRYPT_SMIME_CAPABILITIES = ^CRYPT_SMIME_CAPABILITIES;
  CRYPT_SMIME_CAPABILITIES = record
    cCapability: DWORD;
    rgCapability: PCRYPT_SMIME_CAPABILITY;
  end;


//+-------------------------------------------------------------------------
//  PKCS7_SIGNER_INFO
//
//  pvStructInfo points to CMSG_SIGNER_INFO.
//--------------------------------------------------------------------------


//+-------------------------------------------------------------------------
//  Netscape Certificate Extension Object Identifiers
//--------------------------------------------------------------------------

const
  szOID_NETSCAPE = '2.16.840.1.113730';
  szOID_NETSCAPE_CERT_EXTENSION = '2.16.840.1.113730.1';
  szOID_NETSCAPE_CERT_TYPE = '2.16.840.1.113730.1.1';
  szOID_NETSCAPE_BASE_URL = '2.16.840.1.113730.1.2';
  szOID_NETSCAPE_REVOCATION_URL = '2.16.840.1.113730.1.3';
  szOID_NETSCAPE_CA_REVOCATION_URL = '2.16.840.1.113730.1.4';
  szOID_NETSCAPE_CERT_RENEWAL_URL = '2.16.840.1.113730.1.7';
  szOID_NETSCAPE_CA_POLICY_URL = '2.16.840.1.113730.1.8';
  szOID_NETSCAPE_SSL_SERVER_NAME = '2.16.840.1.113730.1.12';
  szOID_NETSCAPE_COMMENT = '2.16.840.1.113730.1.13';

//+-------------------------------------------------------------------------
//  Netscape Certificate Data Type Object Identifiers
//--------------------------------------------------------------------------

const
  szOID_NETSCAPE_DATA_TYPE = '2.16.840.1.113730.2';
  szOID_NETSCAPE_CERT_SEQUENCE = '2.16.840.1.113730.2.5';

//+-------------------------------------------------------------------------
//  szOID_NETSCAPE_CERT_TYPE extension
//
//  Its value is a bit string. CryptDecodeObject/CryptEncodeObject using
//  X509_BITS.
//
//  The following bits are defined:
//--------------------------------------------------------------------------

const
  NETSCAPE_SSL_CLIENT_AUTH_CERT_TYPE = $80;
  NETSCAPE_SSL_SERVER_AUTH_CERT_TYPE = $40;
  NETSCAPE_SSL_CA_CERT_TYPE = $04;

//+-------------------------------------------------------------------------
//  szOID_NETSCAPE_BASE_URL extension
//
//  Its value is an IA5_STRING. CryptDecodeObject/CryptEncodeObject using
//  X509_ANY_STRING or X509_UNICODE_ANY_STRING, where,
//  dwValueType = CERT_RDN_IA5_STRING.
//
//  When present this string is added to the beginning of all relative URLs
//  in the certificate.  This extension can be considered an optimization
//  to reduce the size of the URL extensions.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  szOID_NETSCAPE_REVOCATION_URL extension
//
//  Its value is an IA5_STRING. CryptDecodeObject/CryptEncodeObject using
//  X509_ANY_STRING or X509_UNICODE_ANY_STRING, where,
//  dwValueType = CERT_RDN_IA5_STRING.
//
//  It is a relative or absolute URL that can be used to check the
//  revocation status of a certificate. The revocation check will be
//  performed as an HTTP GET method using a url that is the concatenation of
//  revocation-url and certificate-serial-number.
//  Where the certificate-serial-number is encoded as a string of
//  ascii hexadecimal digits. For example, if the netscape-base-url is
//  https://www.certs-r-us.com/, the netscape-revocation-url is
//  cgi-bin/check-rev.cgi?, and the certificate serial number is 173420,
//  the resulting URL would be:
//  https://www.certs-r-us.com/cgi-bin/check-rev.cgi?02a56c
//
//  The server should return a document with a Content-Type of
//  application/x-netscape-revocation.  The document should contain
//  a single ascii digit, '1' if the certificate is not curently valid,
//  and '0' if it is curently valid.
//
//  Note: for all of the URLs that include the certificate serial number,
//  the serial number will be encoded as a string which consists of an even
//  number of hexadecimal digits.  If the number of significant digits is odd,
//  the string will have a single leading zero to ensure an even number of
//  digits is generated.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  szOID_NETSCAPE_CA_REVOCATION_URL extension
//
//  Its value is an IA5_STRING. CryptDecodeObject/CryptEncodeObject using
//  X509_ANY_STRING or X509_UNICODE_ANY_STRING, where,
//  dwValueType = CERT_RDN_IA5_STRING.
//
//  It is a relative or absolute URL that can be used to check the
//  revocation status of any certificates that are signed by the CA that
//  this certificate belongs to. This extension is only valid in CA
//  certificates.  The use of this extension is the same as the above
//  szOID_NETSCAPE_REVOCATION_URL extension.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  szOID_NETSCAPE_CERT_RENEWAL_URL extension
//
//  Its value is an IA5_STRING. CryptDecodeObject/CryptEncodeObject using
//  X509_ANY_STRING or X509_UNICODE_ANY_STRING, where,
//  dwValueType = CERT_RDN_IA5_STRING.
//
//  It is a relative or absolute URL that points to a certificate renewal
//  form. The renewal form will be accessed with an HTTP GET method using a
//  url that is the concatenation of renewal-url and
//  certificate-serial-number. Where the certificate-serial-number is
//  encoded as a string of ascii hexadecimal digits. For example, if the
//  netscape-base-url is https://www.certs-r-us.com/, the
//  netscape-cert-renewal-url is cgi-bin/check-renew.cgi?, and the
//  certificate serial number is 173420, the resulting URL would be:
//  https://www.certs-r-us.com/cgi-bin/check-renew.cgi?02a56c
//  The document returned should be an HTML form that will allow the user
//  to request a renewal of their certificate.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  szOID_NETSCAPE_CA_POLICY_URL extension
//
//  Its value is an IA5_STRING. CryptDecodeObject/CryptEncodeObject using
//  X509_ANY_STRING or X509_UNICODE_ANY_STRING, where,
//  dwValueType = CERT_RDN_IA5_STRING.
//
//  It is a relative or absolute URL that points to a web page that
//  describes the policies under which the certificate was issued.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  szOID_NETSCAPE_SSL_SERVER_NAME extension
//
//  Its value is an IA5_STRING. CryptDecodeObject/CryptEncodeObject using
//  X509_ANY_STRING or X509_UNICODE_ANY_STRING, where,
//  dwValueType = CERT_RDN_IA5_STRING.
//
//  It is a "shell expression" that can be used to match the hostname of the
//  SSL server that is using this certificate.  It is recommended that if
//  the server's hostname does not match this pattern the user be notified
//  and given the option to terminate the SSL connection.  If this extension
//  is not present then the CommonName in the certificate subject's
//  distinguished name is used for the same purpose.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  szOID_NETSCAPE_COMMENT extension
//
//  Its value is an IA5_STRING. CryptDecodeObject/CryptEncodeObject using
//  X509_ANY_STRING or X509_UNICODE_ANY_STRING, where,
//  dwValueType = CERT_RDN_IA5_STRING.
//
//  It is a comment that may be displayed to the user when the certificate
//  is viewed.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  szOID_NETSCAPE_CERT_SEQUENCE
//
//  Its value is a PKCS#7 ContentInfo structure wrapping a sequence of
//  certificates. The value of the contentType field is
//  szOID_NETSCAPE_CERT_SEQUENCE, while the content field is the following
//  structure:
//      CertificateSequence ::= SEQUENCE OF Certificate.
//
//  CryptDecodeObject/CryptEncodeObject using
//  PKCS_CONTENT_INFO_SEQUENCE_OF_ANY, where,
//  pszObjId = szOID_NETSCAPE_CERT_SEQUENCE and the CRYPT_DER_BLOBs point
//  to encoded X509 certificates.
//--------------------------------------------------------------------------


//+=========================================================================
//  Object IDentifier (OID) Installable Functions:  Data Structures and APIs
//==========================================================================

type
  HCRYPTOIDFUNCSET = procedure;
  HCRYPTOIDFUNCADDR = procedure;

// Predefined OID Function Names
const
  CRYPT_OID_ENCODE_OBJECT_FUNC = 'CryptDllEncodeObject';
  CRYPT_OID_DECODE_OBJECT_FUNC = 'CryptDllDecodeObject';
  CRYPT_OID_CREATE_COM_OBJECT_FUNC = 'CryptDllCreateCOMObject';
  CRYPT_OID_VERIFY_REVOCATION_FUNC = 'CertDllVerifyRevocation';
  CRYPT_OID_VERIFY_CTL_USAGE_FUNC = 'CertDllVerifyCTLUsage';
  CRYPT_OID_FORMAT_OBJECT_FUNC = 'CryptDllFormatObject';
  CRYPT_OID_FIND_OID_INFO_FUNC = 'CryptDllFindOIDInfo';

// CryptDllEncodeObject has same function signature as CryptEncodeObject.

// CryptDllDecodeObject has same function signature as CryptDecodeObject.

// CryptDllCreateCOMObject has the following signature:
//      BOOL WINAPI CryptDllCreateCOMObject(
//          IN DWORD dwEncodingType,
//          IN LPCSTR pszOID,
//          IN PCRYPT_DATA_BLOB pEncodedContent,
//          IN DWORD dwFlags,
//          IN REFIID riid,
//          OUT void **ppvObj);

// CertDllVerifyRevocation has the same signature as CertVerifyRevocation
//  (See CertVerifyRevocation for details on when called)

// CertDllVerifyCTLUsage has the same signature as CertVerifyCTLUsage

// CryptDllFindOIDInfo currently is only used to store values used by
// CryptFindOIDInfo. See CryptFindOIDInfo() for more details.

//  Example of a complete OID Function Registry Name:
//    HKEY_LOCAL_MACHINE\Software\Microsoft\Cryptography\OID
//      Encoding Type 1\CryptDllEncodeObject\1.2.3
//
//  The key's L"Dll" value contains the name of the Dll.
//  The key's L"FuncName" value overrides the default function name

const
  CRYPT_OID_REGPATH = 'Software\\Microsoft\\Cryptography\\OID';
  CRYPT_OID_REG_ENCODING_TYPE_PREFIX = 'EncodingType ';
{$IFNDEF VER90}
  CRYPT_OID_REG_DLL_VALUE_NAME = WideString('Dll');
  CRYPT_OID_REG_FUNC_NAME_VALUE_NAME = WideString('FuncName');
{$ELSE}
  CRYPT_OID_REG_DLL_VALUE_NAME = ('Dll');
  CRYPT_OID_REG_FUNC_NAME_VALUE_NAME = ('FuncName');
{$ENDIF}
  CRYPT_OID_REG_FUNC_NAME_VALUE_NAME_A = 'FuncName';

// OID used for Default OID functions
  CRYPT_DEFAULT_OID = 'DEFAULT';

type
  PCRYPT_OID_FUNC_ENTRY = ^CRYPT_OID_FUNC_ENTRY;
  CRYPT_OID_FUNC_ENTRY = record
    pszOID: LPCSTR;
    pvFuncAddr: PVOID;
  end;

const
  CRYPT_INSTALL_OID_FUNC_BEFORE_FLAG = 1;

//+-------------------------------------------------------------------------
//  Install a set of callable OID function addresses.
//
//  By default the functions are installed at end of the list.
//  Set CRYPT_INSTALL_OID_FUNC_BEFORE_FLAG to install at beginning of list.
//
//  hModule should be updated with the hModule passed to DllMain to prevent
//  the Dll containing the function addresses from being unloaded by
//  CryptGetOIDFuncAddress/CryptFreeOIDFunctionAddress. This would be the
//  case when the Dll has also regsvr32'ed OID functions via
//  CryptRegisterOIDFunction.
//
//  DEFAULT functions are installed by setting rgFuncEntry[].pszOID =
//  CRYPT_DEFAULT_OID.
//--------------------------------------------------------------------------

function CryptInstallOIDFunctionAddress(hModule: HMODULE; // hModule passed to DllMain
  dwEncodingType: DWORD;
  pszFuncName: LPCSTR;
  cFuncEntry: DWORD;
  const rgFuncEntry: array of CRYPT_OID_FUNC_ENTRY;
  dwFlags: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Initialize and return handle to the OID function set identified by its
//  function name.
//
//  If the set already exists, a handle to the existing set is returned.
//--------------------------------------------------------------------------

function CryptInitOIDFunctionSet(pszFuncName: LPCSTR;
  dwFlags: DWORD): HCRYPTOIDFUNCSET; stdcall;
//+-------------------------------------------------------------------------
//  Search the list of installed functions for an encoding type and OID match.
//  If not found, search the registry.
//
//  For success, returns TRUE with *ppvFuncAddr updated with the function's
//  address and *phFuncAddr updated with the function address's handle.
//  The function's handle is AddRef'ed. CryptFreeOIDFunctionAddress needs to
//  be called to release it.
//
//  For a registry match, the Dll containing the function is loaded.
//--------------------------------------------------------------------------

function CryptGetOIDFunctionAddress(hFuncSet: HCRYPTOIDFUNCSET;
  dwEncodingType: DWORD;
  pszOID: LPCSTR;
  dwFlags: DWORD;
  var ppvFuncAddr: array of PVOID;
  var phFuncAddr: HCRYPTOIDFUNCADDR): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Get the list of registered default Dll entries for the specified
//  function set and encoding type.
//
//  The returned list consists of none, one or more null terminated Dll file
//  names. The list is terminated with an empty (L"\0") Dll file name.
//  For example: L"first.dll" L"\0" L"second.dll" L"\0" L"\0"
//--------------------------------------------------------------------------

function CryptGetDefaultOIDDllList(hFuncSet: HCRYPTOIDFUNCSET;
  dwEncodingType: DWORD;
  pwszDllList: LPWSTR;
  pcchDllList: PDWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Either: get the first or next installed DEFAULT function OR
//  load the Dll containing the DEFAULT function.
//
//  If pwszDll is NULL, search the list of installed DEFAULT functions.
//  *phFuncAddr must be set to NULL to get the first installed function.
//  Successive installed functions are returned by setting *phFuncAddr
//  to the hFuncAddr returned by the previous call.
//
//  If pwszDll is NULL, the input *phFuncAddr
//  is always CryptFreeOIDFunctionAddress'ed by this function, even for
//  an error.
//
//  If pwszDll isn't NULL, then, attempts to load the Dll and the DEFAULT
//  function. *phFuncAddr is ignored upon entry and isn't
//  CryptFreeOIDFunctionAddress'ed.
//
//  For success, returns TRUE with *ppvFuncAddr updated with the function's
//  address and *phFuncAddr updated with the function address's handle.
//  The function's handle is AddRef'ed. CryptFreeOIDFunctionAddress needs to
//  be called to release it or CryptGetDefaultOIDFunctionAddress can also
//  be called for a NULL pwszDll.
//--------------------------------------------------------------------------

function CryptGetDefaultOIDFunctionAddress(hFuncSet: HCRYPTOIDFUNCSET;
  dwEncodingType: DWORD;
  pwszDll: DWORD;
  dwFlags: LPCWSTR;
  var ppvFuncAddr: array of PVOID;
  var phFuncAddr: HCRYPTOIDFUNCADDR): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Releases the handle AddRef'ed and returned by CryptGetOIDFunctionAddress
//  or CryptGetDefaultOIDFunctionAddress.
//
//  If a Dll was loaded for the function its unloaded. However, before doing
//  the unload, the DllCanUnloadNow function exported by the loaded Dll is
//  called. It should return S_FALSE to inhibit the unload or S_TRUE to enable
//  the unload. If the Dll doesn't export DllCanUnloadNow, the Dll is unloaded.
//
//  DllCanUnloadNow has the following signature:
//      STDAPI  DllCanUnloadNow(void);
//--------------------------------------------------------------------------

function CryptFreeOIDFunctionAddress(hFuncAddr: HCRYPTOIDFUNCADDR;
  dwFlags: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Register the Dll containing the function to be called for the specified
//  encoding type, function name and OID.
//
//  pwszDll may contain environment-variable strings
//  which are ExpandEnvironmentStrings()'ed before loading the Dll.
//
//  In addition to registering the DLL, you may override the
//  name of the function to be called. For example,
//      pszFuncName = "CryptDllEncodeObject",
//      pszOverrideFuncName = "MyEncodeXyz".
//  This allows a Dll to export multiple OID functions for the same
//  function name without needing to interpose its own OID dispatcher function.
//--------------------------------------------------------------------------

function CryptRegisterOIDFunction(dwEncodingType: DWORD;
  pszFuncName: LPCSTR;
  pszOID: LPCSTR; //OPTIONAL
  pwszDll: LPCWSTR; //OPTIONAL
  pszOverrideFuncName: LPCSTR): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Unregister the Dll containing the function to be called for the specified
//  encoding type, function name and OID.
//--------------------------------------------------------------------------

function CryptUnregisterOIDFunction(dwEncodingType: DWORD;
  pszFuncName: LPCSTR;
  pszOID: LPCSTR): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Register the Dll containing the default function to be called for the
//  specified encoding type and function name.
//
//  Unlike CryptRegisterOIDFunction, you can't override the function name
//  needing to be exported by the Dll.
//
//  The Dll is inserted before the entry specified by dwIndex.
//    dwIndex == 0, inserts at the beginning.
//    dwIndex == CRYPT_REGISTER_LAST_INDEX, appends at the end.
//
//  pwszDll may contain environment-variable strings
//  which are ExpandEnvironmentStrings()'ed before loading the Dll.
//--------------------------------------------------------------------------

function CryptRegisterDefaultOIDFunction(dwEncodingType: DWORD;
  pszFuncName: LPCSTR;
  dwIndex: DWORD;
  pwszDll: LPCWSTR): BOOL; stdcall;

const
  CRYPT_REGISTER_FIRST_INDEX = 0;
  CRYPT_REGISTER_LAST_INDEX = $FFFFFFFF;

//+-------------------------------------------------------------------------
//  Unregister the Dll containing the default function to be called for
//  the specified encoding type and function name.
//--------------------------------------------------------------------------

function CryptUnregisterDefaultOIDFunction(dwEncodingType: DWORD;
  pszFuncName: LPCSTR;
  pwszDll: LPCWSTR): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Set the value for the specified encoding type, function name, OID and
//  value name.
//
//  See RegSetValueEx for the possible value types.
//
//  String types are UNICODE.
//--------------------------------------------------------------------------

function CryptSetOIDFunctionValue(dwEncodingType: DWORD;
  pszFuncName: LPCSTR;
  pszOID: LPCSTR;
  pwszValueName: LPCWSTR;
  dwValueType: DWORD;
  const pbValueData: PBYTE;
  cbValueData: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Get the value for the specified encoding type, function name, OID and
//  value name.
//
//  See RegEnumValue for the possible value types.
//
//  String types are UNICODE.
//--------------------------------------------------------------------------

function CryptGetOIDFunctionValue(dwEncodingType: DWORD;
  pszFuncName: LPCSTR;
  pwszValueName: LPCSTR;
  pszOID: LPCWSTR;
  pdwValueType: PDWORD;
  pbValueData: PBYTE;
  pcbValueData: PDWORD): BOOL; stdcall;

type
  PFN_CRYPT_ENUM_OID_FUNC = function(dwEncodingType: DWORD;
    pszFuncName: LPCSTR;
    pszOID: LPCSTR;
    cValue: DWORD;
    const rgdwValueType: array of DWORD;
    const rgpwszValueName: array of LPCWSTR;
    const rgpbValueData: array of PBYTE;
    const rgcbValueData: array of DWORD;
    pvArg: PVOID): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Enumerate the OID functions identified by their encoding type,
//  function name and OID.
//
//  pfnEnumOIDFunc is called for each registry key matching the input
//  parameters. Setting dwEncodingType to CRYPT_MATCH_ANY_ENCODING_TYPE matches
//  any. Setting pszFuncName or pszOID to NULL matches any.
//
//  Set pszOID == CRYPT_DEFAULT_OID to restrict the enumeration to only the
//  DEFAULT functions
//
//  String types are UNICODE.
//--------------------------------------------------------------------------

function CryptEnumOIDFunction(dwEncodingType: DWORD;
  pszFuncName: LPCSTR; //OPTIONAL
  pszOID: LPCSTR; //OPTIONAL
  dwFlags: DWORD;
  pvArg: PVOID;
  pfnEnumOIDFunc: PFN_CRYPT_ENUM_OID_FUNC): BOOL; stdcall;

const
  CRYPT_MATCH_ANY_ENCODING_TYPE = $FFFFFFFF;

//+=========================================================================
//  Object IDentifier (OID) Information:  Data Structures and APIs
//==========================================================================

//+-------------------------------------------------------------------------
//  OID Information
//--------------------------------------------------------------------------

type
  PCRYPT_OID_INFO = ^CRYPT_OID_INFO;
  CRYPT_OID_INFO = record
    cbSize: DWORD;
    pszOID: LPCSTR;
    pwszName: LPCWSTR;
    dwGroupId: DWORD;
    EnumValue: record {type EnumValue for the union part of the original struct --max--}
      case integer of
        0: (dwValue: DWORD);
        1: (Algid: ALG_ID);
        2: (dwLength: DWORD);
    end;
    ExtraInfo: CRYPT_DATA_BLOB;
  end;

type
  CCRYPT_OID_INFO = CRYPT_OID_INFO;
  PCCRYPT_OID_INFO = ^CCRYPT_OID_INFO;

//+-------------------------------------------------------------------------
//  OID Group IDs
//--------------------------------------------------------------------------

const
  CRYPT_HASH_ALG_OID_GROUP_ID = 1;
  CRYPT_ENCRYPT_ALG_OID_GROUP_ID = 2;
  CRYPT_PUBKEY_ALG_OID_GROUP_ID = 3;
  CRYPT_SIGN_ALG_OID_GROUP_ID = 4;
  CRYPT_RDN_ATTR_OID_GROUP_ID = 5;
  CRYPT_EXT_OR_ATTR_OID_GROUP_ID = 6;
  CRYPT_ENHKEY_USAGE_OID_GROUP_ID = 7;
  CRYPT_POLICY_OID_GROUP_ID = 8;
  CRYPT_LAST_OID_GROUP_ID = 8;

  CRYPT_FIRST_ALG_OID_GROUP_ID = CRYPT_HASH_ALG_OID_GROUP_ID;
  CRYPT_LAST_ALG_OID_GROUP_ID = CRYPT_SIGN_ALG_OID_GROUP_ID;


// The CRYPT_*_ALG_OID_GROUP_ID's have an Algid. The CRYPT_RDN_ATTR_OID_GROUP_ID
// has a dwLength. The CRYPT_EXT_OR_ATTR_OID_GROUP_ID,
// CRYPT_ENHKEY_USAGE_OID_GROUP_ID or CRYPT_POLICY_OID_GROUP_ID don't have a
// dwValue.

// CRYPT_PUBKEY_ALG_OID_GROUP_ID has the following optional ExtraInfo:
//  DWORD[0] - Flags. CRYPT_OID_INHIBIT_SIGNATURE_FORMAT_FLAG can be set to
//             inhibit the reformatting of the signature before
//             CryptVerifySignature is called or after CryptSignHash
//             is called. CRYPT_OID_USE_PUBKEY_PARA_FOR_PKCS7_FLAG can
//             be set to include the public key algorithm's parameters
//             in the PKCS7's digestEncryptionAlgorithm's parameters.

  CRYPT_OID_INHIBIT_SIGNATURE_FORMAT_FLAG = $1;
  CRYPT_OID_USE_PUBKEY_PARA_FOR_PKCS7_FLAG = $2;

// CRYPT_SIGN_ALG_OID_GROUP_ID has the following optional ExtraInfo:
//  DWORD[0] - Public Key Algid.
//  DWORD[1] - Flags. Same as above for CRYPT_PUBKEY_ALG_OID_GROUP_ID.

// CRYPT_RDN_ATTR_OID_GROUP_ID has the following optional ExtraInfo:
//  Array of DWORDs:
//   [0 ..] - Null terminated list of acceptable RDN attribute
//            value types. An empty list implies CERT_RDN_PRINTABLE_STRING,
//            CERT_RDN_T61_STRING, 0.

//+-------------------------------------------------------------------------
//  Find OID information. Returns NULL if unable to find any information
//  for the specified key and group. Note, returns a pointer to a constant
//  data structure. The returned pointer MUST NOT be freed.
//
//  dwKeyType's:
//    CRYPT_OID_INFO_OID_KEY, pvKey points to a szOID
//    CRYPT_OID_INFO_NAME_KEY, pvKey points to a wszName
//    CRYPT_OID_INFO_ALGID_KEY, pvKey points to an ALG_ID
//    CRYPT_OID_INFO_SIGN_KEY, pvKey points to an array of two ALG_ID's:
//      ALG_ID[0] - Hash Algid
//      ALG_ID[1] - PubKey Algid
//
//  Setting dwGroupId to 0, searches all groups according to the dwKeyType.
//  Otherwise, only the dwGroupId is searched.
//--------------------------------------------------------------------------

function CryptFindOIDInfo(dwKeyType: DWORD;
  pvKey: PVOID;
  dwGroupId: DWORD): PCCRYPT_OID_INFO; stdcall;

const
  CRYPT_OID_INFO_OID_KEY = 1;
  CRYPT_OID_INFO_NAME_KEY = 2;
  CRYPT_OID_INFO_ALGID_KEY = 3;
  CRYPT_OID_INFO_SIGN_KEY = 4;

//+-------------------------------------------------------------------------
//  Register OID information. The OID information specified in the
//  CCRYPT_OID_INFO structure is persisted to the registry.
//
//  crypt32.dll contains information for the commonly known OIDs. This function
//  allows applications to augment crypt32.dll's OID information. During
//  CryptFindOIDInfo's first call, the registered OID information is installed.
//
//  By default the registered OID information is installed after crypt32.dll's
//  OID entries. Set CRYPT_INSTALL_OID_INFO_BEFORE_FLAG to install before.
//--------------------------------------------------------------------------

function CryptRegisterOIDInfo(pInfo: PCCRYPT_OID_INFO;
  dwFlags: DWORD): BOOL; stdcall;

const
  CRYPT_INSTALL_OID_INFO_BEFORE_FLAG = 1;

//+-------------------------------------------------------------------------
//  Unregister OID information. Only the pszOID and dwGroupId fields are
//  used to identify the OID information to be unregistered.
//--------------------------------------------------------------------------

function CryptUnregisterOIDInfo(pInfo: PCCRYPT_OID_INFO): BOOL; stdcall;

//+=========================================================================
//  Low Level Cryptographic Message Data Structures and APIs
//==========================================================================

type
  HCRYPTMSG = Pointer;

const
  szOID_PKCS_7_DATA = '1.2.840.113549.1.7.1';
  szOID_PKCS_7_SIGNED = '1.2.840.113549.1.7.2';
  szOID_PKCS_7_ENVELOPED = '1.2.840.113549.1.7.3';
  szOID_PKCS_7_SIGNEDANDENVELOPED = '1.2.840.113549.1.7.4';
  szOID_PKCS_7_DIGESTED = '1.2.840.113549.1.7.5';
  szOID_PKCS_7_ENCRYPTED = '1.2.840.113549.1.7.6';

  szOID_PKCS_9_CONTENT_TYPE = '1.2.840.113549.1.9.3';
  szOID_PKCS_9_MESSAGE_DIGEST = '1.2.840.113549.1.9.4';

//+-------------------------------------------------------------------------
//  Message types
//--------------------------------------------------------------------------

const
  CMSG_DATA = 1;
  CMSG_SIGNED = 2;
  CMSG_ENVELOPED = 3;
  CMSG_SIGNED_AND_ENVELOPED = 4;
  CMSG_HASHED = 5;
  CMSG_ENCRYPTED = 6;

//+-------------------------------------------------------------------------
//  Message Type Bit Flags
//--------------------------------------------------------------------------

  CMSG_ALL_FLAGS = (not ULONG(0));
  CMSG_DATA_FLAG = (1 shl CMSG_DATA);
  CMSG_SIGNED_FLAG = (1 shl CMSG_SIGNED);
  CMSG_ENVELOPED_FLAG = (1 shl CMSG_ENVELOPED);
  CMSG_SIGNED_AND_ENVELOPED_FLAG = (1 shl CMSG_SIGNED_AND_ENVELOPED);
  CMSG_HASHED_FLAG = (1 shl CMSG_HASHED);
  CMSG_ENCRYPTED_FLAG = (1 shl CMSG_ENCRYPTED);

//+-------------------------------------------------------------------------
//  The message encode information (pvMsgEncodeInfo) is message type dependent
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_DATA: pvMsgEncodeInfo = NULL
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_SIGNED
//
//  The pCertInfo in the CMSG_SIGNER_ENCODE_INFO provides the Issuer, SerialNumber
//  and PublicKeyInfo.Algorithm. The PublicKeyInfo.Algorithm implicitly
//  specifies the HashEncryptionAlgorithm to be used.
//
//  The hCryptProv and dwKeySpec specify the private key to use. If dwKeySpec
//  == 0, then, defaults to AT_SIGNATURE.
//
//  pvHashAuxInfo currently isn't used and must be set to NULL.
//--------------------------------------------------------------------------

type
  PCMSG_SIGNER_ENCODE_INFO = ^CMSG_SIGNER_ENCODE_INFO;
  CMSG_SIGNER_ENCODE_INFO = record
    cbSize: DWORD;
    pCertInfo: PCERT_INFO;
    hCryptProv: HCRYPTPROV;
    dwKeySpec: DWORD;
    HashAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pvHashAuxInfo: PVOID;
    cAuthAttr: DWORD;
    rgAuthAttr: PCRYPT_ATTRIBUTE;
    cUnauthAttr: DWORD;
    rgUnauthAttr: PCRYPT_ATTRIBUTE;
  end;

type
  PCMSG_SIGNED_ENCODE_INFO = ^CMSG_SIGNED_ENCODE_INFO;
  CMSG_SIGNED_ENCODE_INFO = record
    cbSize: DWORD;
    cSigners: DWORD;
    rgSigners: PCMSG_SIGNER_ENCODE_INFO;
    cCertEncoded: DWORD;
    rgCertEncoded: PCERT_BLOB;
    cCrlEncoded: DWORD;
    rgCrlEncoded: PCRL_BLOB;
  end;

//+-------------------------------------------------------------------------
//  CMSG_ENVELOPED
//
//  The PCERT_INFO for the rgRecipients provides the Issuer, SerialNumber
//  and PublicKeyInfo. The PublicKeyInfo.Algorithm implicitly
//  specifies the KeyEncryptionAlgorithm to be used.
//
//  The PublicKeyInfo.PublicKey in PCERT_INFO is used to encrypt the content
//  encryption key for the recipient.
//
//  hCryptProv is used to do the content encryption, recipient key encryption
//  and export. The hCryptProv's private keys aren't used.
//
//  Note: CAPI currently doesn't support more than one KeyEncryptionAlgorithm
//  per provider. This will need to be fixed.
//
//  pvEncryptionAuxInfo currently isn't used and must be set to NULL.
//--------------------------------------------------------------------------

type
  PCMSG_ENVELOPED_ENCODE_INFO = ^CMSG_ENVELOPED_ENCODE_INFO;
  CMSG_ENVELOPED_ENCODE_INFO = record
    cbSize: DWORD;
    hCryptProv: HCRYPTPROV;
    ContentEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pvEncryptionAuxInfo: PVOID;
    cRecipients: DWORD;
    rgpRecipients: PPCERT_INFO; // pointer to array of PCERT_INFO
  end;

//+-------------------------------------------------------------------------
//  CMSG_SIGNED_AND_ENVELOPED
//
//  For PKCS #7, a signed and enveloped message doesn't have the
//  signer's authenticated or unauthenticated attributes. Otherwise, a
//  combination of the CMSG_SIGNED_ENCODE_INFO and CMSG_ENVELOPED_ENCODE_INFO.
//--------------------------------------------------------------------------

type
  PCMSG_SIGNED_AND_ENVELOPED_ENCODE_INFO = ^CMSG_SIGNED_AND_ENVELOPED_ENCODE_INFO;
  CMSG_SIGNED_AND_ENVELOPED_ENCODE_INFO = record
    cbSize: DWORD;
    SignedInfo: CMSG_SIGNED_ENCODE_INFO;
    EnvelopedInfo: CMSG_ENVELOPED_ENCODE_INFO;
  end;

//+-------------------------------------------------------------------------
//  CMSG_HASHED
//
//  hCryptProv is used to do the hash. Doesn't need to use a private key.
//
//  If fDetachedHash is set, then, the encoded message doesn't contain
//  any content (its treated as NULL Data)
//
//  pvHashAuxInfo currently isn't used and must be set to NULL.
//--------------------------------------------------------------------------

type
  PCMSG_HASHED_ENCODE_INFO = ^CMSG_HASHED_ENCODE_INFO;
  CMSG_HASHED_ENCODE_INFO = record
    cbSize: DWORD;
    hCryptProv: HCRYPTPROV;
    HashAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pvHashAuxInfo: PVOID;
  end;

//+-------------------------------------------------------------------------
//  CMSG_ENCRYPTED
//
//  The key used to encrypt the message is identified outside of the message
//  content (for example, password).
//
//  The content input to CryptMsgUpdate has already been encrypted.
//
//  pvEncryptionAuxInfo currently isn't used and must be set to NULL.
//--------------------------------------------------------------------------

type
  PCMSG_ENCRYPTED_ENCODE_INFO = ^CMSG_ENCRYPTED_ENCODE_INFO;
  CMSG_ENCRYPTED_ENCODE_INFO = record
    cbSize: DWORD;
    ContentEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pvEncryptionAuxInfo: PVOID;
  end;

//+-------------------------------------------------------------------------
//  This parameter allows messages to be of variable length with streamed
//  output.
//
//  By default, messages are of a definite length and
//  CryptMsgGetParam(CMSG_CONTENT_PARAM) is
//  called to get the cryptographically processed content. Until closed,
//  the handle keeps a copy of the processed content.
//
//  With streamed output, the processed content can be freed as its streamed.
//
//  If the length of the content to be updated is known at the time of the
//  open, then, ContentLength should be set to that length. Otherwise, it
//  should be set to CMSG_INDEFINITE_LENGTH.
//--------------------------------------------------------------------------

type
  PFN_CMSG_STREAM_OUTPUT = function(
    const pvArg: PVOID;
    pbData: PBYTE;
    cbData: DWORD;
    fFinal: BOOL): BOOL; stdcall;

const
  CMSG_INDEFINITE_LENGTH = ($FFFFFFFF);

type
  PCMSG_STREAM_INFO = ^CMSG_STREAM_INFO;
  CMSG_STREAM_INFO = record
    cbContent: DWORD;
    pfnStreamOutput: PFN_CMSG_STREAM_OUTPUT;
    pvArg: PVOID;
  end;

//+-------------------------------------------------------------------------
//  Open dwFlags
//--------------------------------------------------------------------------

const
  CMSG_BARE_CONTENT_FLAG = $00000001;
  CMSG_LENGTH_ONLY_FLAG = $00000002;
  CMSG_DETACHED_FLAG = $00000004;
  CMSG_AUTHENTICATED_ATTRIBUTES_FLAG = $00000008;
  CMSG_CONTENTS_OCTETS_FLAG = $00000010;
  CMSG_MAX_LENGTH_FLAG = $00000020;

//+-------------------------------------------------------------------------
//  Open a cryptographic message for encoding
//
//  For PKCS #7:
//  If the content to be passed to CryptMsgUpdate has already
//  been message encoded (the input to CryptMsgUpdate is the streamed output
//  from another message encode), then, the CMSG_ENCODED_CONTENT_INFO_FLAG should
//  be set in dwFlags. If not set, then, the inner ContentType is Data and
//  the input to CryptMsgUpdate is treated as the inner Data type's Content,
//  a string of bytes.
//  If CMSG_BARE_CONTENT_FLAG is specified for a streamed message,
//  the streamed output will not have an outer ContentInfo wrapper. This
//  makes it suitable to be streamed into an enclosing message.
//
//  The pStreamInfo parameter needs to be set to stream the encoded message
//  output.
//--------------------------------------------------------------------------

function CryptMsgOpenToEncode(dwMsgEncodingType: DWORD;
  dwFlags: DWORD;
  dwMsgType: DWORD;
  pvMsgEncodeInfo: PVOID;
  pszInnerContentObjID: LPSTR; //OPTIONAL
  pStreamInfo: PCMSG_STREAM_INFO //OPTIONAL
  ): HCRYPTMSG; stdcall;

//+-------------------------------------------------------------------------
//  Calculate the length of an encoded cryptographic message.
//
//  Calculates the length of the encoded message given the
//  message type, encoding parameters and total length of
//  the data to be updated. Note, this might not be the exact length. However,
//  it will always be greater than or equal to the actual length.
//--------------------------------------------------------------------------

function CryptMsgCalculateEncodedLength(dwMsgEncodingType: DWORD;
  dwFlags: DWORD;
  dwMsgType: DWORD;
  pvMsgEncodeInfo: PVOID;
  pszInnerContentObjID: LPSTR; //OPTIONAL
  cbData: DWORD): DWORD; stdcall;

//+-------------------------------------------------------------------------
//  Open a cryptographic message for decoding
//
//  For PKCS #7: if the inner ContentType isn't Data, then, the inner
//  ContentInfo consisting of both ContentType and Content is output.
//  To also enable ContentInfo output for the Data ContentType, then,
//  the CMSG_ENCODED_CONTENT_INFO_FLAG should be set
//  in dwFlags. If not set, then, only the content portion of the inner
//  ContentInfo is output for the Data ContentType.
//
//  To only calculate the length of the decoded message, set the
//  CMSG_LENGTH_ONLY_FLAG in dwFlags. After the final CryptMsgUpdate get the
//  MSG_CONTENT_PARAM. Note, this might not be the exact length. However,
//  it will always be greater than or equal to the actual length.
//
//  hCryptProv specifies the crypto provider to use for hashing and/or
//  decrypting the message. For enveloped messages, hCryptProv also specifies
//  the private exchange key to use. For signed messages, hCryptProv is used
//  when CryptMsgVerifySigner is called.
//
//  For enveloped messages, the pRecipientInfo contains the Issuer and
//  SerialNumber identifying the RecipientInfo in the message.
//
//  Note, the pRecipientInfo should correspond to the provider's private
//  exchange key.
//
//  If pRecipientInfo is NULL, then, the message isn't decrypted. To decrypt
//  the message, CryptMsgControl(CMSG_CTRL_DECRYPT) is called after the final
//  CryptMsgUpdate.
//
//  The pStreamInfo parameter needs to be set to stream the decoded content
//  output. Note, if pRecipientInfo is NULL, then, the streamed output isn't
//  decrypted.
//--------------------------------------------------------------------------

function CryptMsgOpenToDecode(dwMsgEncodingType: DWORD;
  dwFlags: DWORD;
  dwMsgType: DWORD;
  hCryptProv: HCRYPTPROV;
  pRecipientInfo: PCERT_INFO; //OPTIONAL
  pStreamInfo: PCMSG_STREAM_INFO //OPTIONAL
  ): HCRYPTMSG; stdcall;

//+-------------------------------------------------------------------------
//  Close a cryptographic message handle
//
//  LastError is preserved unless FALSE is returned.
//--------------------------------------------------------------------------

function CryptMsgClose(hCryptMsg: HCRYPTMSG): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Update the content of a cryptographic message. Depending on how the
//  message was opened, the content is either encoded or decoded.
//
//  This function is repetitively called to append to the message content.
//  fFinal is set to identify the last update. On fFinal, the encode/decode
//  is completed. The encoded/decoded content and the decoded parameters
//  are valid until the open and all duplicated handles are closed.
//--------------------------------------------------------------------------

function CryptMsgUpdate(hCryptMsg: HCRYPTMSG;
  const pbData: PBYTE;
  cbData: DWORD;
  fFinal: BOOL): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Perform a special "control" function after the final CryptMsgUpdate of a
//  encoded/decoded cryptographic message.
//
//  The dwCtrlType parameter specifies the type of operation to be performed.
//
//  The pvCtrlPara definition depends on the dwCtrlType value.
//
//  See below for a list of the control operations and their pvCtrlPara
//  type definition.
//--------------------------------------------------------------------------

function CryptMsgControl(hCryptMsg: HCRYPTMSG;
  dwFlags: DWORD;
  dwCtrlType: DWORD;
  pvCtrlPara: PVOID): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Message control types
//--------------------------------------------------------------------------

const
  CMSG_CTRL_VERIFY_SIGNATURE = 1;
  CMSG_CTRL_DECRYPT = 2;
  CMSG_CTRL_VERIFY_HASH = 5;
  CMSG_CTRL_ADD_SIGNER = 6;
  CMSG_CTRL_DEL_SIGNER = 7;
  CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR = 8;
  CMSG_CTRL_DEL_SIGNER_UNAUTH_ATTR = 9;
  CMSG_CTRL_ADD_CERT = 10;
  CMSG_CTRL_DEL_CERT = 11;
  CMSG_CTRL_ADD_CRL = 12;
  CMSG_CTRL_DEL_CRL = 13;

//+-------------------------------------------------------------------------
//  CMSG_CTRL_VERIFY_SIGNATURE
//
//  Verify the signature of a SIGNED or SIGNED_AND_ENVELOPED
//  message after it has been decoded.
//
//  For a SIGNED_AND_ENVELOPED message, called after
//  CryptMsgControl(CMSG_CTRL_DECRYPT), if CryptMsgOpenToDecode was called
//  with a NULL pRecipientInfo.
//
//  pvCtrlPara points to a CERT_INFO struct.
//
//  The CERT_INFO contains the Issuer and SerialNumber identifying
//  the Signer of the message. The CERT_INFO also contains the
//  PublicKeyInfo
//  used to verify the signature. The cryptographic provider specified
//  in CryptMsgOpenToDecode is used.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CTRL_DECRYPT
//
//  Decrypt an ENVELOPED or SIGNED_AND_ENVELOPED message after it has been
//  decoded.
//
//  hCryptProv and dwKeySpec specify the private key to use. For dwKeySpec ==
//  0, defaults to AT_KEYEXCHANGE.
//
//  dwRecipientIndex is the index of the recipient in the message associated
//  with the hCryptProv's private key.
//
//  This control function needs to be called, if you don't know the appropriate
//  recipient before calling CryptMsgOpenToDecode. After the final
//  CryptMsgUpdate, the list of recipients is obtained by iterating through
//  CMSG_RECIPIENT_INFO_PARAM. The recipient corresponding to a private
//  key owned by the caller is selected and passed to this function to decrypt
//  the message.
//
//  Note, the message can only be decrypted once.
//--------------------------------------------------------------------------

type
  PCMSG_CTRL_DECRYPT_PARA = ^CMSG_CTRL_DECRYPT_PARA;
  CMSG_CTRL_DECRYPT_PARA = record
    cbSize: DWORD;
    hCryptProv: HCRYPTPROV;
    dwKeySpec: DWORD;
    dwRecipientIndex: DWORD;
  end;

//+-------------------------------------------------------------------------
//  CMSG_CTRL_VERIFY_HASH
//
//  Verify the hash of a HASHED message after it has been decoded.
//
//  Only the hCryptMsg parameter is used, to specify the message whose
//  hash is being verified.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CTRL_ADD_SIGNER
//
//  Add a signer to a signed-data or signed-and-enveloped-data message.
//
//  pvCtrlPara points to a CMSG_SIGNER_ENCODE_INFO.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CTRL_DEL_SIGNER
//
//  Remove a signer from a signed-data or signed-and-enveloped-data message.
//
//  pvCtrlPara points to a DWORD containing the 0-based index of the
//  signer to be removed.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR
//
//  Add an unauthenticated attribute to the SignerInfo of a signed-data or
//  signed-and-enveloped-data message.
//
//  The unauthenticated attribute is input in the form of an encoded blob.
//--------------------------------------------------------------------------

type
  PCMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR_PARA = ^CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR_PARA;
  CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR_PARA = record
    cbSize: DWORD;
    dwSignerIndex: DWORD;
    blob: CRYPT_DATA_BLOB;
  end;

//+-------------------------------------------------------------------------
//  CMSG_CTRL_DEL_SIGNER_UNAUTH_ATTR
//
//  Delete an unauthenticated attribute from the SignerInfo of a signed-data
//  or signed-and-enveloped-data message.
//
//  The unauthenticated attribute to be removed is specified by
//  a 0-based index.
//--------------------------------------------------------------------------

type
  PCMSG_CTRL_DEL_SIGNER_UNAUTH_ATTR_PARA = ^CMSG_CTRL_DEL_SIGNER_UNAUTH_ATTR_PARA;
  CMSG_CTRL_DEL_SIGNER_UNAUTH_ATTR_PARA = record
    cbSize: DWORD;
    dwSignerIndex: DWORD;
    dwUnauthAttrIndex: DWORD;
  end;

//+-------------------------------------------------------------------------
//  CMSG_CTRL_ADD_CERT
//
//  Add a certificate to a signed-data or signed-and-enveloped-data message.
//
//  pvCtrlPara points to a CRYPT_DATA_BLOB containing the certificate's
//  encoded bytes.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CTRL_DEL_CERT
//
//  Delete a certificate from a signed-data or signed-and-enveloped-data
//  message.
//
//  pvCtrlPara points to a DWORD containing the 0-based index of the
//  certificate to be removed.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CTRL_ADD_CRL
//
//  Add a CRL to a signed-data or signed-and-enveloped-data message.
//
//  pvCtrlPara points to a CRYPT_DATA_BLOB containing the CRL's
//  encoded bytes.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CTRL_DEL_CRL
//
//  Delete a CRL from a signed-data or signed-and-enveloped-data message.
//
//  pvCtrlPara points to a DWORD containing the 0-based index of the CRL
//  to be removed.
//--------------------------------------------------------------------------


//+-------------------------------------------------------------------------
//  Verify a countersignature, at the SignerInfo level.
//  ie. verify that pbSignerInfoCountersignature contains the encrypted
//  hash of the encryptedDigest field of pbSignerInfo.
//
//  hCryptProv is used to hash the encryptedDigest field of pbSignerInfo.
//  The only fields referenced from pciCountersigner are SerialNumber, Issuer,
//  and SubjectPublicKeyInfo.
//--------------------------------------------------------------------------

function CryptMsgVerifyCountersignatureEncoded(hCryptProv: HCRYPTPROV;
  dwEncodingType: DWORD;
  pbSignerInfo: PBYTE;
  cbSignerInfo: DWORD;
  pbSignerInfoCountersignature: PBYTE;
  cbSignerInfoCountersignature: DWORD;
  pciCountersigner: PCERT_INFO): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Countersign an already-existing signature in a message
//
//  dwIndex is a zero-based index of the SignerInfo to be countersigned.
//--------------------------------------------------------------------------

function CryptMsgCountersign(hCryptMsg: HCRYPTMSG;
  dwIndex: DWORD;
  cCountersigners: DWORD;
  rgCountersigners: PCMSG_SIGNER_ENCODE_INFO): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Countersign an already-existing signature (encoded SignerInfo).
//  Output an encoded SignerInfo blob, suitable for use as a countersignature
//  attribute in the unauthenticated attributes of a signed-data or
//  signed-and-enveloped-data message.
//--------------------------------------------------------------------------

function CryptMsgCountersignEncoded(dwEncodingType: DWORD;
  pbSignerInfo: PBYTE;
  cbSignerInfo: DWORD;
  cCountersigners: DWORD;
  rgCountersigners: PCMSG_SIGNER_ENCODE_INFO;
  pbCountersignature: PBYTE;
  pcbCountersignature: PDWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Get a parameter after encoding/decoding a cryptographic message. Called
//  after the final CryptMsgUpdate. Only the CMSG_CONTENT_PARAM and
//  CMSG_COMPUTED_HASH_PARAM are valid for an encoded message.
//
//  For an encoded HASHED message, the CMSG_COMPUTED_HASH_PARAM can be got
//  before any CryptMsgUpdates to get its length.
//
//  The pvData type definition depends on the dwParamType value.
//
//  Elements pointed to by fields in the pvData structure follow the
//  structure. Therefore, *pcbData may exceed the size of the structure.
//
//  Upon input, if *pcbData == 0, then, *pcbData is updated with the length
//  of the data and the pvData parameter is ignored.
//
//  Upon return, *pcbData is updated with the length of the data.
//
//  The OBJID BLOBs returned in the pvData structures point to
//  their still encoded representation. The appropriate functions
//  must be called to decode the information.
//
//  See below for a list of the parameters to get.
//--------------------------------------------------------------------------

function CryptMsgGetParam(hCryptMsg: HCRYPTMSG;
  dwParamType: DWORD;
  dwIndex: DWORD;
  pvData: PVOID;
  pcbData: PDWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Get parameter types and their corresponding data structure definitions.
//--------------------------------------------------------------------------

const
  CMSG_TYPE_PARAM = 1;
  CMSG_CONTENT_PARAM = 2;
  CMSG_BARE_CONTENT_PARAM = 3;
  CMSG_INNER_CONTENT_TYPE_PARAM = 4;
  CMSG_SIGNER_COUNT_PARAM = 5;
  CMSG_SIGNER_INFO_PARAM = 6;
  CMSG_SIGNER_CERT_INFO_PARAM = 7;
  CMSG_SIGNER_HASH_ALGORITHM_PARAM = 8;
  CMSG_SIGNER_AUTH_ATTR_PARAM = 9;
  CMSG_SIGNER_UNAUTH_ATTR_PARAM = 10;
  CMSG_CERT_COUNT_PARAM = 11;
  CMSG_CERT_PARAM = 12;
  CMSG_CRL_COUNT_PARAM = 13;
  CMSG_CRL_PARAM = 14;
  CMSG_ENVELOPE_ALGORITHM_PARAM = 15;
  CMSG_RECIPIENT_COUNT_PARAM = 17;
  CMSG_RECIPIENT_INDEX_PARAM = 18;
  CMSG_RECIPIENT_INFO_PARAM = 19;
  CMSG_HASH_ALGORITHM_PARAM = 20;
  CMSG_HASH_DATA_PARAM = 21;
  CMSG_COMPUTED_HASH_PARAM = 22;
  CMSG_ENCRYPT_PARAM = 26;
  CMSG_ENCRYPTED_DIGEST = 27;
  CMSG_ENCODED_SIGNER = 28;
  CMSG_ENCODED_MESSAGE = 29;

//+-------------------------------------------------------------------------
//  CMSG_TYPE_PARAM
//
//  The type of the decoded message.
//
//  pvData points to a DWORD
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CONTENT_PARAM
//
//  The encoded content of a cryptographic message. Depending on how the
//  message was opened, the content is either the whole PKCS#7
//  message (opened to encode) or the inner content (opened to decode).
//  In the decode case, the decrypted content is returned, if enveloped.
//  If not enveloped, and if the inner content is of type DATA, the returned
//  data is the contents octets of the inner content.
//
//  pvData points to the buffer receiving the content bytes
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_BARE_CONTENT_PARAM
//
//  The encoded content of an encoded cryptographic message, without the
//  outer layer of ContentInfo. That is, only the encoding of the
//  ContentInfo.content field is returned.
//
//  pvData points to the buffer receiving the content bytes
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_INNER_CONTENT_TYPE_PARAM
//
//  The type of the inner content of a decoded cryptographic message,
//  in the form of a NULL-terminated object identifier string
//  (eg. "1.2.840.113549.1.7.1").
//
//  pvData points to the buffer receiving the object identifier string
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_SIGNER_COUNT_PARAM
//
//  Count of signers in a SIGNED or SIGNED_AND_ENVELOPED message
//
//  pvData points to a DWORD
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_SIGNER_CERT_INFO_PARAM
//
//  To get all the signers, repetitively call CryptMsgGetParam, with
//  dwIndex set to 0 .. SignerCount - 1.
//
//  pvData points to a CERT_INFO struct.
//
//  Only the following fields have been updated in the CERT_INFO struct:
//  Issuer and SerialNumber.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_SIGNER_INFO_PARAM
//
//  To get all the signers, repetitively call CryptMsgGetParam, with
//  dwIndex set to 0 .. SignerCount - 1.
//
//  pvData points to a CMSG_SIGNER_INFO struct.
//--------------------------------------------------------------------------

type
  PCMSG_SIGNER_INFO = ^CMSG_SIGNER_INFO;
  CMSG_SIGNER_INFO = record
    dwVersion: DWORD;
    Issuer: CERT_NAME_BLOB;
    SerialNumber: CRYPT_INTEGER_BLOB;
    HashAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    HashEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    EncryptedHash: CRYPT_DATA_BLOB;
    AuthAttrs: CRYPT_ATTRIBUTES;
    UnauthAttrs: CRYPT_ATTRIBUTES;
  end;

//+-------------------------------------------------------------------------
//  CMSG_SIGNER_HASH_ALGORITHM_PARAM
//
//  This parameter specifies the HashAlgorithm that was used for the signer.
//
//  Set dwIndex to iterate through all the signers.
//
//  pvData points to an CRYPT_ALGORITHM_IDENTIFIER struct.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_SIGNER_AUTH_ATTR_PARAM
//
//  The authenticated attributes for the signer.
//
//  Set dwIndex to iterate through all the signers.
//
//  pvData points to a CMSG_ATTR struct.
//--------------------------------------------------------------------------

type
  CMSG_ATTR = CRYPT_ATTRIBUTES;
  PCMSG_ATTR = ^CRYPT_ATTRIBUTES;

//+-------------------------------------------------------------------------
//  CMSG_SIGNER_UNAUTH_ATTR_PARAM
//
//  The unauthenticated attributes for the signer.
//
//  Set dwIndex to iterate through all the signers.
//
//  pvData points to a CMSG_ATTR struct.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CERT_COUNT_PARAM
//
//  Count of certificates in a SIGNED or SIGNED_AND_ENVELOPED message.
//
//  pvData points to a DWORD
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CERT_PARAM
//
//  To get all the certificates, repetitively call CryptMsgGetParam, with
//  dwIndex set to 0 .. CertCount - 1.
//
//  pvData points to an array of the certificate's encoded bytes.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CRL_COUNT_PARAM
//
//  Count of CRLs in a SIGNED or SIGNED_AND_ENVELOPED message.
//
//  pvData points to a DWORD
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CRL_PARAM
//
//  To get all the CRLs, repetitively call CryptMsgGetParam, with
//  dwIndex set to 0 .. CrlCount - 1.
//
//  pvData points to an array of the CRL's encoded bytes.
//--------------------------------------------------------------------------


//+-------------------------------------------------------------------------
//  CMSG_ENVELOPE_ALGORITHM_PARAM
//
//  The ContentEncryptionAlgorithm that was used in
//  an ENVELOPED or SIGNED_AND_ENVELOPED message.
//
//  pvData points to an CRYPT_ALGORITHM_IDENTIFIER struct.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_RECIPIENT_COUNT_PARAM
//
//  Count of recipients in an ENVELOPED or SIGNED_AND_ENVELOPED message.
//
//  pvData points to a DWORD
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_RECIPIENT_INDEX_PARAM
//
//  Index of the recipient used to decrypt an ENVELOPED or SIGNED_AND_ENVELOPED
//  message.
//
//  pvData points to a DWORD
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_RECIPIENT_INFO_PARAM
//
//  To get all the recipients, repetitively call CryptMsgGetParam, with
//  dwIndex set to 0 .. RecipientCount - 1.
//
//  pvData points to a CERT_INFO struct.
//
//  Only the following fields have been updated in the CERT_INFO struct:
//  Issuer, SerialNumber and PublicKeyAlgorithm. The PublicKeyAlgorithm
//  specifies the KeyEncryptionAlgorithm that was used.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_HASH_ALGORITHM_PARAM
//
//  The HashAlgorithm in a HASHED message.
//
//  pvData points to an CRYPT_ALGORITHM_IDENTIFIER struct.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_HASH_DATA_PARAM
//
//  The hash in a HASHED message.
//
//  pvData points to an array of bytes.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_COMPUTED_HASH_PARAM
//
//  The computed hash for a HASHED message.
//
//  This may be called for either an encoded or decoded message.
//  It also may be called before any encoded CryptMsgUpdates to get its length.
//
//  pvData points to an array of bytes.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_ENCRYPT_PARAM
//
//  The ContentEncryptionAlgorithm that was used in an ENCRYPTED message.
//
//  pvData points to an CRYPT_ALGORITHM_IDENTIFIER struct.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_ENCODED_MESSAGE
//
//  The full encoded message. This is useful in the case of a decoded
//  message which has been modified (eg. a signed-data or
//  signed-and-enveloped-data message which has been countersigned).
//
//  pvData points to an array of the message's encoded bytes.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CryptMsg OID installable functions
//--------------------------------------------------------------------------

// If *phCryptProv is NULL upon entry, then, if supported, the installable
// function should acquire a default provider and return. Note, its up
// to the installable function to release at process detach.

const
  CMSG_OID_GEN_ENCRYPT_KEY_FUNC = 'CryptMsgDllGenEncryptKey';

type
  PFN_CMSG_GEN_ENCRYPT_KEY = function(phCryptProv: PHCRYPTPROV;
    paiEncrypt: PCRYPT_ALGORITHM_IDENTIFIER;
    pvEncryptAuxInfo: PVOID;
    pPublicKeyInfo: PCERT_PUBLIC_KEY_INFO;
    phEncryptKey: PHCRYPTKEY
    ): BOOL; stdcall;

const
  CMSG_OID_EXPORT_ENCRYPT_KEY_FUNC = 'CryptMsgDllExportEncryptKey';

type
  PFN_CMSG_EXPORT_ENCRYPT_KEY = function(hCryptProv: HCRYPTPROV;
    hEncryptKey: HCRYPTKEY;
    pPublicKeyInfo: PCERT_PUBLIC_KEY_INFO;
    pbData: PBYTE;
    pcbData: PDWORD): BOOL; stdcall;

const
  CMSG_OID_IMPORT_ENCRYPT_KEY_FUNC = 'CryptMsgDllImportEncryptKey';

type
  PFN_CMSG_IMPORT_ENCRYPT_KEY = function(hCryptProv: HCRYPTPROV;
    dwKeySpec: DWORD;
    paiEncrypt: PCRYPT_ALGORITHM_IDENTIFIER;
    paiPubKey: PCRYPT_ALGORITHM_IDENTIFIER;
    pbEncodedKey: PBYTE;
    cbEncodedKey: DWORD;
    phEncryptKey: PHCRYPTKEY
    ): BOOL; stdcall;

//+=========================================================================
//  Certificate Store Data Structures and APIs
//==========================================================================

//+-------------------------------------------------------------------------
//              In its most basic implementation, a cert store is simply a
//              collection of certificates and/or CRLs. This is the case when
//              a cert store is opened with all of its certificates and CRLs
//              coming from a PKCS #7 encoded cryptographic message.
//
//              Nonetheless, all cert stores have the following properties:
//               - A public key may have more than one certificate in the store.
//                 For example, a private/public key used for signing may have a
//                 certificate issued for VISA and another issued for
//                 Mastercard. Also, when a certificate is renewed there might
//                 be more than one certificate with the same subject and
//                 issuer.
//               - However, each certificate in the store is uniquely
//                 identified by its Issuer and SerialNumber.
//               - There's an issuer of subject certificate relationship. A
//                 certificate's issuer is found by doing a match of
//                 pSubjectCert->Issuer with pIssuerCert->Subject.
//                 The relationship is verified by using
//                 the issuer's public key to verify the subject certificate's
//                 signature. Note, there might be X.509 v3 extensions
//                 to assist in finding the issuer certificate.
//               - Since issuer certificates might be renewed, a subject
//                 certificate might have more than one issuer certificate.
//               - There's an issuer of CRL relationship. An
//                 issuer's CRL is found by doing a match of
//                 pIssuerCert->Subject with pCrl->Issuer.
//                 The relationship is verified by using
//                 the issuer's public key to verify the CRL's
//                 signature. Note, there might be X.509 v3 extensions
//                 to assist in finding the CRL.
//               - Since some issuers might support the X.509 v3 delta CRL
//                 extensions, an issuer might have more than one CRL.
//               - The store shouldn't have any redundant certificates or
//                 CRLs. There shouldn't be two certificates with the same
//                 Issuer and SerialNumber. There shouldn't be two CRLs with
//                 the same Issuer, ThisUpdate and NextUpdate.
//               - The store has NO policy or trust information. No
//                 certificates are tagged as being "root". Its up to
//                 the application to maintain a list of CertIds (Issuer +
//                 SerialNumber) for certificates it trusts.
//               - The store might contain bad certificates and/or CRLs.
//                 The issuer's signature of a subject certificate or CRL may
//                 not verify. Certificates or CRLs may not satisfy their
//                 time validity requirements. Certificates may be
//                 revoked.
//
//              In addition to the certificates and CRLs, properties can be
//              stored. There are two predefined property IDs for a user
//              certificate: CERT_KEY_PROV_HANDLE_PROP_ID and
//              CERT_KEY_PROV_INFO_PROP_ID. The CERT_KEY_PROV_HANDLE_PROP_ID
//              is a HCRYPTPROV handle to the private key assoicated
//              with the certificate. The CERT_KEY_PROV_INFO_PROP_ID contains
//              information to be used to call
//              CryptAcquireContext and CryptProvSetParam to get a handle
//              to the private key associated with the certificate.
//
//              There exists two more predefined property IDs for certificates
//              and CRLs, CERT_SHA1_HASH_PROP_ID and CERT_MD5_HASH_PROP_ID.
//              If these properties don't already exist, then, a hash of the
//              content is computed. (CERT_HASH_PROP_ID maps to the default
//              hash algorithm, currently, CERT_SHA1_HASH_PROP_ID).
//
//              There are additional APIs for creating certificate and CRL
//      contexts not in a store (CertCreateCertificateContext and
//      CertCreateCRLContext).
//
//--------------------------------------------------------------------------

type
  HCERTSTORE = PVOID;

//+-------------------------------------------------------------------------
//  Certificate context.
//
//  A certificate context contains both the encoded and decoded representation
//  of a certificate. A certificate context returned by a cert store function
//  must be freed by calling the CertFreeCertificateContext function. The
//  CertDuplicateCertificateContext function can be called to make a duplicate
//  copy (which also must be freed by calling CertFreeCertificateContext).
//--------------------------------------------------------------------------

type
  PCERT_CONTEXT = ^CERT_CONTEXT;
  CERT_CONTEXT = record
    dwCertEncodingType: DWORD;
    pbCertEncoded: PBYTE;
    cbCertEncoded: DWORD;
    pCertInfo: PCERT_INFO;
    hCertStore: HCERTSTORE;
  end;

type
  PCCERT_CONTEXT = ^CERT_CONTEXT;

//+-------------------------------------------------------------------------
//  CRL context.
//
//  A CRL context contains both the encoded and decoded representation
//  of a CRL. A CRL context returned by a cert store function
//  must be freed by calling the CertFreeCRLContext function. The
//  CertDuplicateCRLContext function can be called to make a duplicate
//  copy (which also must be freed by calling CertFreeCRLContext).
//--------------------------------------------------------------------------

type
  PCRL_CONTEXT = ^CRL_CONTEXT;
  CRL_CONTEXT = record
    dwCertEncodingType: DWORD;
    pbCrlEncoded: PBYTE;
    cbCrlEncoded: DWORD;
    pCrlInfo: PCRL_INFO;
    hCertStore: HCERTSTORE;
  end;

type
  PCCRL_CONTEXT = ^CRL_CONTEXT;

//+-------------------------------------------------------------------------
//  Certificate Trust List (CTL) context.
//
//  A CTL context contains both the encoded and decoded representation
//  of a CTL. Also contains an opened HCRYPTMSG handle to the decoded
//  cryptographic signed message containing the CTL_INFO as its inner content.
//  pbCtlContent is the encoded inner content of the signed message.
//
//  The CryptMsg APIs can be used to extract additional signer information.
//--------------------------------------------------------------------------

type
  PCTL_CONTEXT = ^CTL_CONTEXT;
  CTL_CONTEXT = record
    dwMsgAndCertEncodingType: DWORD;
    pbCtlEncoded: PBYTE;
    cbCtlEncoded: DWORD;
    pCtlInfo: PCTL_INFO;
    hCertStore: HCERTSTORE;
    hCryptMsg: HCRYPTMSG;
    pbCtlContent: PBYTE;
    cbCtlContent: DWORD;
  end;

type
  PCCTL_CONTEXT = ^CTL_CONTEXT;

//+-------------------------------------------------------------------------
//  Certificate, CRL and CTL property IDs
//
//  See CertSetCertificateContextProperty or CertGetCertificateContextProperty
//  for usage information.
//--------------------------------------------------------------------------

const
  CERT_KEY_PROV_HANDLE_PROP_ID = 1;
  CERT_KEY_PROV_INFO_PROP_ID = 2;
  CERT_SHA1_HASH_PROP_ID = 3;
  CERT_MD5_HASH_PROP_ID = 4;
  CERT_HASH_PROP_ID = CERT_SHA1_HASH_PROP_ID;
  CERT_KEY_CONTEXT_PROP_ID = 5;
  CERT_KEY_SPEC_PROP_ID = 6;
  CERT_IE30_RESERVED_PROP_ID = 7;
  CERT_PUBKEY_HASH_RESERVED_PROP_ID = 8;
  CERT_ENHKEY_USAGE_PROP_ID = 9;
  CERT_CTL_USAGE_PROP_ID = CERT_ENHKEY_USAGE_PROP_ID;
  CERT_NEXT_UPDATE_LOCATION_PROP_ID = 10;
  CERT_FRIENDLY_NAME_PROP_ID = 11;
  CERT_PVK_FILE_PROP_ID = 12;
// Note, 32 - 34 are reserved for the CERT, CRL and CTL file element IDs.
  CERT_FIRST_RESERVED_PROP_ID = 13;

  CERT_LAST_RESERVED_PROP_ID = $00007FFF;
  CERT_FIRST_USER_PROP_ID = $00008000;
  CERT_LAST_USER_PROP_ID = $0000FFFF;

function IS_CERT_HASH_PROP_ID(X: DWORD): BOOL;

//+-------------------------------------------------------------------------
//  Cryptographic Key Provider Information
//
//  CRYPT_KEY_PROV_INFO defines the CERT_KEY_PROV_INFO_PROP_ID's pvData.
//
//  The CRYPT_KEY_PROV_INFO fields are passed to CryptAcquireContext
//  to get a HCRYPTPROV handle. The optional CRYPT_KEY_PROV_PARAM fields are
//  passed to CryptProvSetParam to further initialize the provider.
//
//  The dwKeySpec field identifies the private key to use from the container
//  For example, AT_KEYEXCHANGE or AT_SIGNATURE.
//--------------------------------------------------------------------------

type
  PCRYPT_KEY_PROV_PARAM = ^CRYPT_KEY_PROV_PARAM;
  CRYPT_KEY_PROV_PARAM = record
    dwParam: DWORD;
    pbData: PBYTE;
    cbData: DWORD;
    dwFlags: DWORD;
  end;

type
  PCRYPT_KEY_PROV_INFO = ^CRYPT_KEY_PROV_INFO;
  CRYPT_KEY_PROV_INFO = record
    pwszContainerName: LPWSTR;
    pwszProvName: LPWSTR;
    dwProvType: DWORD;
    dwFlags: DWORD;
    cProvParam: DWORD;
    rgProvParam: PCRYPT_KEY_PROV_PARAM;
    dwKeySpec: DWORD;
  end;

//+-------------------------------------------------------------------------
//  The following flag should be set in the above dwFlags to enable
//  a CertSetCertificateContextProperty(CERT_KEY_CONTEXT_PROP_ID) after a
//  CryptAcquireContext is done in the Sign or Decrypt Message functions.
//
//  The following define must not collide with any of the
//  CryptAcquireContext dwFlag defines.
//--------------------------------------------------------------------------

const
  CERT_SET_KEY_PROV_HANDLE_PROP_ID = $00000001;
  CERT_SET_KEY_CONTEXT_PROP_ID = $00000001;

//+-------------------------------------------------------------------------
//  Certificate Key Context
//
//  CERT_KEY_CONTEXT defines the CERT_KEY_CONTEXT_PROP_ID's pvData.
//--------------------------------------------------------------------------

type
  PCERT_KEY_CONTEXT = ^CERT_KEY_CONTEXT;
  CERT_KEY_CONTEXT = record
    cbSize: DWORD; // sizeof(CERT_KEY_CONTEXT)
    hCryptProv: HCRYPTPROV;
    dwKeySpec: DWORD;
  end;

//+-------------------------------------------------------------------------
//  Certificate Store Provider Types
//--------------------------------------------------------------------------

const
  CERT_STORE_PROV_MSG = (LPCSTR(1));
  CERT_STORE_PROV_MEMORY = (LPCSTR(2));
  CERT_STORE_PROV_FILE = (LPCSTR(3));
  CERT_STORE_PROV_REG = (LPCSTR(4));

  CERT_STORE_PROV_PKCS7 = (LPCSTR(5));
  CERT_STORE_PROV_SERIALIZED = (LPCSTR(6));
  CERT_STORE_PROV_FILENAME_A = (LPCSTR(7));
  CERT_STORE_PROV_FILENAME_W = (LPCSTR(8));
  CERT_STORE_PROV_FILENAME = CERT_STORE_PROV_FILENAME_W;
  CERT_STORE_PROV_SYSTEM_A = (LPCSTR(9));
  CERT_STORE_PROV_SYSTEM_W = (LPCSTR(10));
  CERT_STORE_PROV_SYSTEM = CERT_STORE_PROV_SYSTEM_W;

  sz_CERT_STORE_PROV_MEMORY = 'Memory';
  sz_CERT_STORE_PROV_FILENAME_W = 'File';
  sz_CERT_STORE_PROV_FILENAME = sz_CERT_STORE_PROV_FILENAME_W;
  sz_CERT_STORE_PROV_SYSTEM_W = 'System';
  sz_CERT_STORE_PROV_SYSTEM = sz_CERT_STORE_PROV_SYSTEM_W;
  sz_CERT_STORE_PROV_PKCS7 = 'PKCS7';
  sz_CERT_STORE_PROV_SERIALIZED = 'Serialized';

//+-------------------------------------------------------------------------
//  Certificate Store verify/results flags
//--------------------------------------------------------------------------

  CERT_STORE_SIGNATURE_FLAG = $00000001;
  CERT_STORE_TIME_VALIDITY_FLAG = $00000002;
  CERT_STORE_REVOCATION_FLAG = $00000004;
  CERT_STORE_NO_CRL_FLAG = $00010000;
  CERT_STORE_NO_ISSUER_FLAG = $00020000;

//+-------------------------------------------------------------------------
//  Certificate Store open/property flags
//--------------------------------------------------------------------------

  CERT_STORE_NO_CRYPT_RELEASE_FLAG = $00000001;
  CERT_STORE_READONLY_FLAG = $00008000;

//+-------------------------------------------------------------------------
//  Certificate Store Provider flags are in the HiWord (0xFFFF0000)
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  Certificate System Store Flag Values
//--------------------------------------------------------------------------
// Location of the system store in the registry:
//  HKEY_CURRENT_USER or HKEY_LOCAL_MACHINE
  CERT_SYSTEM_STORE_LOCATION_MASK = $00030000;
  CERT_SYSTEM_STORE_CURRENT_USER = $00010000;
  CERT_SYSTEM_STORE_LOCAL_MACHINE = $00020000;

//+-------------------------------------------------------------------------
//  Open the cert store using the specified store provider.
//
//  hCryptProv specifies the crypto provider to use to create the hash
//  properties or verify the signature of a subject certificate or CRL.
//  The store doesn't need to use a private
//  key. If the CERT_STORE_NO_CRYPT_RELEASE_FLAG isn't set, hCryptProv is
//  CryptReleaseContext'ed on the final CertCloseStore.
//
//  Note, if the open fails, hCryptProv is released if it would have been
//  released when the store was closed.
//
//  If hCryptProv is zero, then, the default provider and container for the
//  PROV_RSA_FULL provider type is CryptAcquireContext'ed with
//  CRYPT_VERIFYCONTEXT access. The CryptAcquireContext is deferred until
//  the first create hash or verify signature. In addition, once acquired,
//  the default provider isn't released until process exit when crypt32.dll
//  is unloaded. The acquired default provider is shared across all stores
//  and threads.
//
//  After initializing the store's data structures and optionally acquiring a
//  default crypt provider, CertOpenStore calls CryptGetOIDFunctionAddress to
//  get the address of the CRYPT_OID_OPEN_STORE_PROV_FUNC specified by
//  lpszStoreProvider. Since a store can contain certificates with different
//  encoding types, CryptGetOIDFunctionAddress is called with dwEncodingType
//  set to 0 and not the dwEncodingType passed to CertOpenStore.
//  PFN_CERT_DLL_OPEN_STORE_FUNC specifies the signature of the provider's
//  open function. This provider open function is called to load the
//  store's certificates and CRLs. Optionally, the provider may return an
//  array of functions called before a certificate or CRL is added or deleted
//  or has a property that is set.
//
//  Use of the dwEncodingType parameter is provider dependent. The type
//  definition for pvPara also depends on the provider.
//
//  Store providers are installed or registered via
//  CryptInstallOIDFunctionAddress or CryptRegisterOIDFunction, where,
//  dwEncodingType is 0 and pszFuncName is CRYPT_OID_OPEN_STORE_PROV_FUNC.
//
//  Here's a list of the predefined provider types (implemented in crypt32.dll):
//
//  CERT_STORE_PROV_MSG:
//      Gets the certificates and CRLs from the specified cryptographic message.
//      dwEncodingType contains the message and certificate encoding types.
//      The message's handle is passed in pvPara. Given,
//          HCRYPTMSG hCryptMsg; pvPara = (const void *) hCryptMsg;
//
//  CERT_STORE_PROV_MEMORY
//  sz_CERT_STORE_PROV_MEMORY:
//      Opens a store without any initial certificates or CRLs. pvPara
//      isn't used.
//
//  CERT_STORE_PROV_FILE:
//      Reads the certificates and CRLs from the specified file. The file's
//      handle is passed in pvPara. Given,
//          HANDLE hFile; pvPara = (const void *) hFile;
//
//      For a successful open, the file pointer is advanced past
//      the certificates and CRLs and their properties read from the file.
//      Note, only expects a serialized store and not a file containing
//      either a PKCS #7 signed message or a single encoded certificate.
//
//      The hFile isn't closed.
//
//  CERT_STORE_PROV_REG:
//      Reads the certificates and CRLs from the registry. The registry's
//      key handle is passed in pvPara. Given,
//          HKEY hKey; pvPara = (const void *) hKey;
//
//      The input hKey isn't closed by the provider. Before returning, the
//      provider opens/creates "Certificates" and "CRLs" subkeys. These
//      subkeys remain open until the store is closed.
//
//      If CERT_STORE_READONLY_FLAG is set, then, the registry subkeys are
//      RegOpenKey'ed with KEY_READ_ACCESS. Otherwise, the registry subkeys
//      are RegCreateKey'ed with KEY_ALL_ACCESS.
//
//      This provider returns the array of functions for reading, writing,
//      deleting and property setting certificates and CRLs.
//      Any changes to the opened store are immediately pushed through to
//      the registry. However, if CERT_STORE_READONLY_FLAG is set, then,
//      writing, deleting or property setting results in a
//      SetLastError(E_ACCESSDENIED).
//
//      Note, all the certificates and CRLs are read from the registry
//      when the store is opened. The opened store serves as a write through
//      cache. However, the opened store isn't notified of other changes
//      made to the registry. Note, RegNotifyChangeKeyValue is supported
//      on NT but not supported on Windows95.
//
//  CERT_STORE_PROV_PKCS7:
//  sz_CERT_STORE_PROV_PKCS7:
//      Gets the certificates and CRLs from the encoded PKCS #7 signed message.
//      dwEncodingType specifies the message and certificate encoding types.
//      The pointer to the encoded message's blob is passed in pvPara. Given,
//          CRYPT_DATA_BLOB EncodedMsg; pvPara = (const void *) &EncodedMsg;
//
//      Note, also supports the IE3.0 special version of a
//      PKCS #7 signed message referred to as a "SPC" formatted message.
//
//  CERT_STORE_PROV_SERIALIZED:
//  sz_CERT_STORE_PROV_SERIALIZED:
//      Gets the certificates and CRLs from memory containing a serialized
//      store.  The pointer to the serialized memory blob is passed in pvPara.
//      Given,
//          CRYPT_DATA_BLOB Serialized; pvPara = (const void *) &Serialized;
//
//  CERT_STORE_PROV_FILENAME_A:
//  CERT_STORE_PROV_FILENAME_W:
//  CERT_STORE_PROV_FILENAME:
//  sz_CERT_STORE_PROV_FILENAME_W:
//  sz_CERT_STORE_PROV_FILENAME:
//      Opens the file and first attempts to read as a serialized store. Then,
//      as a PKCS #7 signed message. Finally, as a single encoded certificate.
//      The filename is passed in pvPara. The filename is UNICODE for the
//      "_W" provider and ASCII for the "_A" provider. For "_W": given,
//          LPCWSTR pwszFilename; pvPara = (const void *) pwszFilename;
//      For "_A": given,
//          LPCSTR pszFilename; pvPara = (const void *) pszFilename;
//
//      Note, the default (without "_A" or "_W") is unicode.
//
//      Note, also supports the reading of the IE3.0 special version of a
//      PKCS #7 signed message file referred to as a "SPC" formatted file.
//
//  CERT_STORE_PROV_SYSTEM_A:
//  CERT_STORE_PROV_SYSTEM_W:
//  CERT_STORE_PROV_SYSTEM:
//  sz_CERT_STORE_PROV_SYSTEM_W:
//  sz_CERT_STORE_PROV_SYSTEM:
//      Opens the specified "system" store. Currently, all the system
//      stores are stored in the registry. The upper word of the dwFlags
//      parameter is used to specify the location of the system store. It
//      should be set to either CERT_SYSTEM_STORE_CURRENT_USER for
//      HKEY_CURRENT_USER or CERT_SYSTEM_STORE_LOCAL_MACHINE for
//      HKEY_LOCAL_MACHINE.
//
//      After opening the registry key associated with the system name,
//      the CERT_STORE_PROV_REG provider is called to complete the open.
//
//      The system store name is passed in pvPara. The name is UNICODE for the
//      "_W" provider and ASCII for the "_A" provider. For "_W": given,
//          LPCWSTR pwszSystemName; pvPara = (const void *) pwszSystemName;
//      For "_A": given,
//          LPCSTR pszSystemName; pvPara = (const void *) pszSystemName;
//
//      Note, the default (without "_A" or "_W") is UNICODE.
//
//      If CERT_STORE_READONLY_FLAG is set, then, the registry is
//      RegOpenKey'ed with KEY_READ_ACCESS. Otherwise, the registry is
//      RegCreateKey'ed with KEY_ALL_ACCESS.
//
//      The "root" store is treated differently from the other system
//      stores. Before a certificate is added to or deleted from the "root"
//      store, a pop up message box is displayed. The certificate's subject,
//      issuer, serial number, time validity, sha1 and md5 thumbprints are
//      displayed. The user is given the option to do the add or delete.
//      If they don't allow the operation, LastError is set to E_ACCESSDENIED.
//--------------------------------------------------------------------------

function CertOpenStore(lpszStoreProvider: LPCSTR;
  dwEncodingType: DWORD;
  hCryptProv: HCRYPTPROV;
  dwFlags: DWORD;
  const pvPara: PVOID): HCERTSTORE; stdcall;

//+-------------------------------------------------------------------------
//  OID Installable Certificate Store Provider Data Structures
//--------------------------------------------------------------------------

// Handle returned by the store provider when opened.
type
  HCERTSTOREPROV = PVOID;

// Store Provider OID function's pszFuncName.
const
  CRYPT_OID_OPEN_STORE_PROV_FUNC = 'CertDllOpenStoreProv';

// Note, the Store Provider OID function's dwEncodingType is always 0.

// The following information is returned by the provider when opened. Its
// zeroed with cbSize set before the provider is called. If the provider
// doesn't need to be called again after the open it doesn't need to
// make any updates to the CERT_STORE_PROV_INFO.

type
  PCERT_STORE_PROV_INFO = ^CERT_STORE_PROV_INFO;
  CERT_STORE_PROV_INFO = record
    cbSize: DWORD;
    cStoreProvFunc: DWORD;
    rgpvStoreProvFunc: PPVOID;
    hStoreProv: HCERTSTOREPROV;
    dwStoreProvFlags: DWORD;
  end;

// Definition of the store provider's open function.
//
// *pStoreProvInfo has been zeroed before the call.
//
// Note, pStoreProvInfo->cStoreProvFunc should be set last.  Once set,
// all subsequent store calls, such as CertAddSerializedElementToStore will
// call the appropriate provider callback function.

type
  PFN_CERT_DLL_OPEN_STORE_PROV_FUNC = function(lpszStoreProvider: LPCSTR;
    dwEncodingType: DWORD;
    hCryptProv: HCRYPTPROV;
    dwFlags: DWORD;
    const pvPara: PVOID;
    hCertStore: HCERTSTORE;
    pStoreProvInfo: PCERT_STORE_PROV_INFO
    ): BOOL; stdcall;

// Indices into the store provider's array of callback functions.
//
// The provider can implement any subset of the following functions. It
// sets pStoreProvInfo->cStoreProvFunc to the last index + 1 and any
// preceding not implemented functions to NULL.

const
  CERT_STORE_PROV_CLOSE_FUNC = 0;
  CERT_STORE_PROV_READ_CERT_FUNC = 1;
  CERT_STORE_PROV_WRITE_CERT_FUNC = 2;
  CERT_STORE_PROV_DELETE_CERT_FUNC = 3;
  CERT_STORE_PROV_SET_CERT_PROPERTY_FUNC = 4;
  CERT_STORE_PROV_READ_CRL_FUNC = 5;
  CERT_STORE_PROV_WRITE_CRL_FUNC = 6;
  CERT_STORE_PROV_DELETE_CRL_FUNC = 7;
  CERT_STORE_PROV_SET_CRL_PROPERTY_FUNC = 8;
  CERT_STORE_PROV_READ_CTL_FUNC = 9;
  CERT_STORE_PROV_WRITE_CTL_FUNC = 10;
  CERT_STORE_PROV_DELETE_CTL_FUNC = 11;
  CERT_STORE_PROV_SET_CTL_PROPERTY_FUNC = 12;

// Called by CertCloseStore when the store's reference count is
// decremented to 0.

type
  PFN_CERT_STORE_PROV_CLOSE = procedure(hStoreProv: HCERTSTOREPROV;
    dwFlags: DWORD); stdcall;

// Currently not called directly by the store APIs. However, may be exported
// to support other providers based on it.
//
// Reads the provider's copy of the certificate context. If it exists,
// creates a new certificate context.

type
  PFN_CERT_STORE_PROV_READ_CERT = function(hStoreProv: HCERTSTOREPROV;
    pStoreCertContext: PCCERT_CONTEXT;
    dwFlags: DWORD;
    var ppProvCertContext: PCCERT_CONTEXT
    ): BOOL; stdcall;

const
  CERT_STORE_PROV_WRITE_ADD_FLAG = $1;

// Called by CertAddEncodedCertificateToStore,
// CertAddCertificateContextToStore or CertAddSerializedElementToStore before
// adding to the store. The CERT_STORE_PROV_WRITE_ADD_FLAG is set. In
// addition to the encoded certificate, the added pCertContext might also
// have properties.
//
// Returns TRUE if its OK to update the the store.

type
  PFN_CERT_STORE_PROV_WRITE_CERT = function(hStoreProv: HCERTSTOREPROV;
    pCertContext: PCCERT_CONTEXT;
    dwFlags: DWORD): BOOL; stdcall;

// Called by CertDeleteCertificateFromStore before deleting from the
// store.
//
// Returns TRUE if its OK to delete from the store.

type
  PFN_CERT_STORE_PROV_DELETE_CERT = function(hStoreProv: HCERTSTOREPROV;
    pCertContext: PCCERT_CONTEXT;
    dwFlags: DWORD): BOOL; stdcall;

// Called by CertSetCertificateContextProperty before setting the
// certificate's property. Also called by CertGetCertificateContextProperty,
// when getting a hash property that needs to be created and then persisted
// via the set.
//
// Upon input, the property hasn't been set for the pCertContext parameter.
//
// Returns TRUE if its OK to set the property.

type
  PFN_CERT_STORE_PROV_SET_CERT_PROPERTY = function(hStoreProv: HCERTSTOREPROV;
    pCertContext: PCCERT_CONTEXT;
    dwPropId: DWORD;
    dwFlags: DWORD;
    const pvData: PVOID
    ): BOOL; stdcall;

// Currently not called directly by the store APIs. However, may be exported
// to support other providers based on it.
//
// Reads the provider's copy of the CRL context. If it exists,
// creates a new CRL context.

type
  PFN_CERT_STORE_PROV_READ_CRL = function(hStoreProv: HCERTSTOREPROV;
    pStoreCrlContext: PCCRL_CONTEXT;
    dwFlags: DWORD;
    var ppProvCrlContext: PCCRL_CONTEXT
    ): BOOL; stdcall;

// Called by CertAddEncodedCRLToStore,
// CertAddCRLContextToStore or CertAddSerializedElementToStore before
// adding to the store. The CERT_STORE_PROV_WRITE_ADD_FLAG is set. In
// addition to the encoded CRL, the added pCertContext might also
// have properties.
//
// Returns TRUE if its OK to update the the store.

type
  PFN_CERT_STORE_PROV_WRITE_CRL = function(hStoreProv: HCERTSTOREPROV;
    pCrlContext: PCCRL_CONTEXT;
    dwFlags: DWORD): BOOL; stdcall;

// Called by CertDeleteCRLFromStore before deleting from the store.
//
// Returns TRUE if its OK to delete from the store.

type
  PFN_CERT_STORE_PROV_DELETE_CRL = function(hStoreProv: HCERTSTOREPROV;
    pCrlContext: PCCRL_CONTEXT;
    dwFlags: DWORD): BOOL; stdcall;

// Called by CertDeleteCRLFromStore before deleting from the store.
//
// Returns TRUE if its OK to delete from the store.

type
  PFN_CERT_STORE_PROV_SET_CRL_PROPERTY = function(hStoreProv: HCERTSTOREPROV;
    pCrlContext: PCCRL_CONTEXT;
    dwPropId: DWORD;
    dwFlags: DWORD;
    pvData: PVOID): BOOL; stdcall;

// Currently not called directly by the store APIs. However, may be exported
// to support other providers based on it.
//
// Reads the provider's copy of the CTL context. If it exists,
// creates a new CTL context.

type
  PFN_CERT_STORE_PROV_READ_CTL = function(hStoreProv: HCERTSTOREPROV;
    pStoreCtlContext: PCCTL_CONTEXT;
    dwFlags: DWORD;
    var ppProvCtlContext: PCCTL_CONTEXT
    ): BOOL; stdcall;

// Called by CertAddEncodedCTLToStore,
// CertAddCTLContextToStore or CertAddSerializedElementToStore before
// adding to the store. The CERT_STORE_PROV_WRITE_ADD_FLAG is set. In
// addition to the encoded CTL, the added pCertContext might also
// have properties.
//
// Returns TRUE if its OK to update the the store.

type
  PFN_CERT_STORE_PROV_WRITE_CTL = function(hStoreProv: HCERTSTOREPROV;
    pCtlContext: PCCTL_CONTEXT;
    dwFlags: DWORD): BOOL; stdcall;

// Called by CertDeleteCTLFromStore before deleting from the store.
//
// Returns TRUE if its OK to delete from the store.

type
  PFN_CERT_STORE_PROV_DELETE_CTL = function(hStoreProv: HCERTSTOREPROV;
    pCtlContext: PCCTL_CONTEXT;
    dwFlags: DWORD): BOOL; stdcall;

// Called by CertSetCTLContextProperty before setting the
// CTL's property. Also called by CertGetCTLContextProperty,
// when getting a hash property that needs to be created and then persisted
// via the set.
//
// Upon input, the property hasn't been set for the pCtlContext parameter.
//
// Returns TRUE if its OK to set the property.

type
  PFN_CERT_STORE_PROV_SET_CTL_PROPERTY = function(hStoreProv: HCERTSTOREPROV;
    pCtlContext: PCCTL_CONTEXT;
    dwPropId: DWORD;
    dwFlags: DWORD;
    const pvData: PVOID
    ): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Duplicate a cert store handle
//--------------------------------------------------------------------------

function CertDuplicateStore(hCertStore: HCERTSTORE): HCERTSTORE; stdcall;

const CERT_STORE_SAVE_AS_STORE = 1;
const CERT_STORE_SAVE_AS_PKCS7 = 2;

const CERT_STORE_SAVE_TO_FILE = 1;
const CERT_STORE_SAVE_TO_MEMORY = 2;
const CERT_STORE_SAVE_TO_FILENAME_A = 3;
const CERT_STORE_SAVE_TO_FILENAME_W = 4;
const CERT_STORE_SAVE_TO_FILENAME = CERT_STORE_SAVE_TO_FILENAME_W;

//+-------------------------------------------------------------------------
//  Save the cert store. Extended version with lots of options.
//
//  According to the dwSaveAs parameter, the store can be saved as a
//  serialized store (CERT_STORE_SAVE_AS_STORE) containing properties in
//  addition to encoded certificates, CRLs and CTLs or the store can be saved
//  as a PKCS #7 signed message (CERT_STORE_SAVE_AS_PKCS7) which doesn't
//  include the properties or CTLs.
//
//  Note, the CERT_KEY_CONTEXT_PROP_ID property (and its
//  CERT_KEY_PROV_HANDLE_PROP_ID or CERT_KEY_SPEC_PROP_ID) isn't saved into
//  a serialized store.
//
//  For CERT_STORE_SAVE_AS_PKCS7, the dwEncodingType specifies the message
//  encoding type. The dwEncodingType parameter isn't used for
//  CERT_STORE_SAVE_AS_STORE.
//
//  The dwFlags parameter currently isn't used and should be set to 0.
//
//  The dwSaveTo and pvSaveToPara parameters specify where to save the
//  store as follows:
//    CERT_STORE_SAVE_TO_FILE:
//      Saves to the specified file. The file's handle is passed in
//      pvSaveToPara. Given,
//          HANDLE hFile; pvSaveToPara = (void *) hFile;
//
//      For a successful save, the file pointer is positioned after the
//      last write.
//
//    CERT_STORE_SAVE_TO_MEMORY:
//      Saves to the specified memory blob. The pointer to
//      the memory blob is passed in pvSaveToPara. Given,
//          CRYPT_DATA_BLOB SaveBlob; pvSaveToPara = (void *) &SaveBlob;
//      Upon entry, the SaveBlob's pbData and cbData need to be initialized.
//      Upon return, cbData is updated with the actual length.
//      For a length only calculation, pbData should be set to NULL. If
//      pbData is non-NULL and cbData isn't large enough, FALSE is returned
//      with a last error of ERRROR_MORE_DATA.
//
//    CERT_STORE_SAVE_TO_FILENAME_A:
//    CERT_STORE_SAVE_TO_FILENAME_W:
//    CERT_STORE_SAVE_TO_FILENAME:
//      Opens the file and saves to it. The filename is passed in pvSaveToPara.
//      The filename is UNICODE for the "_W" option and ASCII for the "_A"
//      option. For "_W": given,
//          LPCWSTR pwszFilename; pvSaveToPara = (void *) pwszFilename;
//      For "_A": given,
//          LPCSTR pszFilename; pvSaveToPara = (void *) pszFilename;
//
//      Note, the default (without "_A" or "_W") is UNICODE.
//
//--------------------------------------------------------------------------

function CertSaveStore(hCertStore: HCERTSTORE;
  dwEncodingType: DWORD;
  dwSaveAs: DWORD;
  dwSaveTo: DWORD;
  pvSaveToPara: PVOID;
  dwFlags: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Certificate Store close flags
//--------------------------------------------------------------------------
const CERT_CLOSE_STORE_FORCE_FLAG = $00000001;
const CERT_CLOSE_STORE_CHECK_FLAG = $00000002;

//+-------------------------------------------------------------------------
//  Close a cert store handle.
//
//  There needs to be a corresponding close for each open and duplicate.
//
//  Even on the final close, the cert store isn't freed until all of its
//  certificate and CRL contexts have also been freed.
//
//  On the final close, the hCryptProv passed to CertStoreOpen is
//  CryptReleaseContext'ed.
//
//  To force the closure of the store with all of its memory freed, set the
//  CERT_STORE_CLOSE_FORCE_FLAG. This flag should be set when the caller does
//  its own reference counting and wants everything to vanish.
//
//  To check if all the store's certificates and CRLs have been freed and that
//  this is the last CertCloseStore, set the CERT_CLOSE_STORE_CHECK_FLAG. If
//  set and certs, CRLs or stores still need to be freed/closed, FALSE is
//  returned with LastError set to CRYPT_E_PENDING_CLOSE. Note, for FALSE,
//  the store is still closed. This is a diagnostic flag.
//
//  LastError is preserved unless CERT_CLOSE_STORE_CHECK_FLAG is set and FALSE
//  is returned.
//--------------------------------------------------------------------------

function CertCloseStore(hCertStore: HCERTSTORE; dwFlags: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Get the subject certificate context uniquely identified by its Issuer and
//  SerialNumber from the store.
//
//  If the certificate isn't found, NULL is returned. Otherwise, a pointer to
//  a read only CERT_CONTEXT is returned. CERT_CONTEXT must be freed by calling
//  CertFreeCertificateContext. CertDuplicateCertificateContext can be called to make a
//  duplicate.
//
//  The returned certificate might not be valid. Normally, it would be
//  verified when getting its issuer certificate (CertGetIssuerCertificateFromStore).
//--------------------------------------------------------------------------

function CertGetSubjectCertificateFromStore(hCertStore: HCERTSTORE;
  dwCertEncodingType: DWORD;
  pCertId: PCERT_INFO // Only the Issuer and SerialNumber
  ): PCCERT_CONTEXT; stdcall; // fields are used

//+-------------------------------------------------------------------------
//  Enumerate the certificate contexts in the store.
//
//  If a certificate isn't found, NULL is returned.
//  Otherwise, a pointer to a read only CERT_CONTEXT is returned. CERT_CONTEXT
//  must be freed by calling CertFreeCertificateContext or is freed when passed as the
//  pPrevCertContext on a subsequent call. CertDuplicateCertificateContext
//  can be called to make a duplicate.
//
//  pPrevCertContext MUST BE NULL to enumerate the first
//  certificate in the store. Successive certificates are enumerated by setting
//  pPrevCertContext to the CERT_CONTEXT returned by a previous call.
//
//  NOTE: a NON-NULL pPrevCertContext is always CertFreeCertificateContext'ed by
//  this function, even for an error.
//--------------------------------------------------------------------------

function CertEnumCertificatesInStore(hCertStore: HCERTSTORE;
  pPrevCertContext: PCCERT_CONTEXT
  ): PCCERT_CONTEXT; stdcall;

//+-------------------------------------------------------------------------
//  Find the first or next certificate context in the store.
//
//  The certificate is found according to the dwFindType and its pvFindPara.
//  See below for a list of the find types and its parameters.
//
//  Currently dwFindFlags is only used for CERT_FIND_SUBJECT_ATTR,
//  CERT_FIND_ISSUER_ATTR or CERT_FIND_CTL_USAGE. Otherwise, must be set to 0.
//
//  Usage of dwCertEncodingType depends on the dwFindType.
//
//  If the first or next certificate isn't found, NULL is returned.
//  Otherwise, a pointer to a read only CERT_CONTEXT is returned. CERT_CONTEXT
//  must be freed by calling CertFreeCertificateContext or is freed when passed as the
//  pPrevCertContext on a subsequent call. CertDuplicateCertificateContext
//  can be called to make a duplicate.
//
//  pPrevCertContext MUST BE NULL on the first
//  call to find the certificate. To find the next certificate, the
//  pPrevCertContext is set to the CERT_CONTEXT returned by a previous call.
//
//  NOTE: a NON-NULL pPrevCertContext is always CertFreeCertificateContext'ed by
//  this function, even for an error.
//--------------------------------------------------------------------------

function CertFindCertificateInStore(hCertStore: HCERTSTORE;
  dwCertEncodingType: DWORD;
  dwFindFlags: DWORD;
  dwFindType: DWORD;
  const pvFindPara: PVOID;
  pPrevCertContext: PCCERT_CONTEXT
  ): PCCERT_CONTEXT; stdcall;

//+-------------------------------------------------------------------------
// Certificate comparison functions
//--------------------------------------------------------------------------

const CERT_COMPARE_SHIFT = 16;
const CERT_COMPARE_ANY = 0;
const CERT_COMPARE_SHA1_HASH = 1;
const CERT_COMPARE_NAME = 2;
const CERT_COMPARE_ATTR = 3;
const CERT_COMPARE_MD5_HASH = 4;
const CERT_COMPARE_PROPERTY = 5;
const CERT_COMPARE_PUBLIC_KEY = 6;
const CERT_COMPARE_HASH = CERT_COMPARE_SHA1_HASH;
const CERT_COMPARE_NAME_STR_A = 7;
const CERT_COMPARE_NAME_STR_W = 8;
const CERT_COMPARE_KEY_SPEC = 9;
const CERT_COMPARE_ENHKEY_USAGE = 10;
const CERT_COMPARE_CTL_USAGE = CERT_COMPARE_ENHKEY_USAGE;

//+-------------------------------------------------------------------------
//  dwFindType
//
//  The dwFindType definition consists of two components:
//   - comparison function
//   - certificate information flag
//--------------------------------------------------------------------------

const CERT_FIND_ANY = (CERT_COMPARE_ANY shl CERT_COMPARE_SHIFT);
const CERT_FIND_SHA1_HASH = (CERT_COMPARE_SHA1_HASH shl CERT_COMPARE_SHIFT);
const CERT_FIND_MD5_HASH = (CERT_COMPARE_MD5_HASH shl CERT_COMPARE_SHIFT);
const CERT_FIND_HASH = CERT_FIND_SHA1_HASH;
const CERT_FIND_PROPERTY = (CERT_COMPARE_PROPERTY shl CERT_COMPARE_SHIFT);
const CERT_FIND_PUBLIC_KEY = (CERT_COMPARE_PUBLIC_KEY shl CERT_COMPARE_SHIFT);

const CERT_FIND_SUBJECT_NAME = (CERT_COMPARE_NAME shl CERT_COMPARE_SHIFT or CERT_INFO_SUBJECT_FLAG);
const CERT_FIND_SUBJECT_ATTR = (CERT_COMPARE_ATTR shl CERT_COMPARE_SHIFT or CERT_INFO_SUBJECT_FLAG);
const CERT_FIND_ISSUER_NAME = (CERT_COMPARE_NAME shl CERT_COMPARE_SHIFT or CERT_INFO_ISSUER_FLAG);
const CERT_FIND_ISSUER_ATTR = (CERT_COMPARE_ATTR shl CERT_COMPARE_SHIFT or CERT_INFO_ISSUER_FLAG);
const CERT_FIND_SUBJECT_STR_A = (CERT_COMPARE_NAME_STR_A shl CERT_COMPARE_SHIFT or CERT_INFO_SUBJECT_FLAG);
const CERT_FIND_SUBJECT_STR_W = (CERT_COMPARE_NAME_STR_W shl CERT_COMPARE_SHIFT or CERT_INFO_SUBJECT_FLAG);
const CERT_FIND_SUBJECT_STR = CERT_FIND_SUBJECT_STR_W;
const CERT_FIND_ISSUER_STR_A = (CERT_COMPARE_NAME_STR_A shl CERT_COMPARE_SHIFT or CERT_INFO_ISSUER_FLAG);
const CERT_FIND_ISSUER_STR_W = (CERT_COMPARE_NAME_STR_W shl CERT_COMPARE_SHIFT or CERT_INFO_ISSUER_FLAG);
const CERT_FIND_ISSUER_STR = CERT_FIND_ISSUER_STR_W;
const CERT_FIND_KEY_SPEC = (CERT_COMPARE_KEY_SPEC shl CERT_COMPARE_SHIFT);
const CERT_FIND_ENHKEY_USAGE = (CERT_COMPARE_ENHKEY_USAGE shl CERT_COMPARE_SHIFT);
const CERT_FIND_CTL_USAGE = CERT_FIND_ENHKEY_USAGE;

//+-------------------------------------------------------------------------
//  CERT_FIND_ANY
//
//  Find any certificate.
//
//  pvFindPara isn't used.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_HASH
//
//  Find a certificate with the specified hash.
//
//  pvFindPara points to a CRYPT_HASH_BLOB.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_PROPERTY
//
//  Find a certificate having the specified property.
//
//  pvFindPara points to a DWORD containing the PROP_ID
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_PUBLIC_KEY
//
//  Find a certificate matching the specified public key.
//
//  pvFindPara points to a CERT_PUBLIC_KEY_INFO containing the public key
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_SUBJECT_NAME
//  CERT_FIND_ISSUER_NAME
//
//  Find a certificate with the specified subject/issuer name. Does an exact
//  match of the entire name.
//
//  Restricts search to certificates matching the dwCertEncodingType.
//
//  pvFindPara points to a CERT_NAME_BLOB.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_SUBJECT_ATTR
//  CERT_FIND_ISSUER_ATTR
//
//  Find a certificate with the specified subject/issuer attributes.
//
//  Compares the attributes in the subject/issuer name with the
//  Relative Distinguished Name's (CERT_RDN) array of attributes specified in
//  pvFindPara. The comparison iterates through the CERT_RDN attributes and looks
//  for an attribute match in any of the subject/issuer's RDNs.
//
//  The CERT_RDN_ATTR fields can have the following special values:
//    pszObjId == NULL              - ignore the attribute object identifier
//    dwValueType == RDN_ANY_TYPE   - ignore the value type
//    Value.pbData == NULL          - match any value
//
//  Currently only an exact, case sensitive match is supported.
//
//  CERT_UNICODE_IS_RDN_ATTRS_FLAG should be set in dwFindFlags if the RDN was
//  initialized with unicode strings as for
//  CryptEncodeObject(X509_UNICODE_NAME).
//
//  Restricts search to certificates matching the dwCertEncodingType.
//
//  pvFindPara points to a CERT_RDN (defined in wincert.h).
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_SUBJECT_STR_A
//  CERT_FIND_SUBJECT_STR_W | CERT_FIND_SUBJECT_STR
//  CERT_FIND_ISSUER_STR_A
//  CERT_FIND_ISSUER_STR_W  | CERT_FIND_ISSUER_STR
//
//  Find a certificate containing the specified subject/issuer name string.
//
//  First, the certificate's subject/issuer is converted to a name string
//  via CertNameToStrA/CertNameToStrW(CERT_SIMPLE_NAME_STR). Then, a
//  case insensitive substring within string match is performed.
//
//  Restricts search to certificates matching the dwCertEncodingType.
//
//  For *_STR_A, pvFindPara points to a null terminated character string.
//  For *_STR_W, pvFindPara points to a null terminated wide character string.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_KEY_SPEC
//
//  Find a certificate having a CERT_KEY_SPEC_PROP_ID property matching
//  the specified KeySpec.
//
//  pvFindPara points to a DWORD containing the KeySpec.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_ENHKEY_USAGE
//
//  Find a certificate having the szOID_ENHANCED_KEY_USAGE extension or
//  the CERT_ENHKEY_USAGE_PROP_ID and matching the specified pszUsageIdentifers.
//
//  pvFindPara points to a CERT_ENHKEY_USAGE data structure. If pvFindPara
//  is NULL or CERT_ENHKEY_USAGE's cUsageIdentifier is 0, then, matches any
//  certificate having enhanced key usage.
//
//  The CERT_FIND_OPTIONAL_ENHKEY_USAGE_FLAG can be set in dwFindFlags to
//  also match a certificate without either the extension or property.
//
//  If CERT_FIND_NO_ENHKEY_USAGE_FLAG is set in dwFindFlags, finds
//  certificates without the key usage extension or property. Setting this
//  flag takes precedence over pvFindPara being NULL.
//
//  If the CERT_FIND_EXT_ONLY_ENHKEY_USAGE_FLAG is set, then, only does a match
//  using the extension. If pvFindPara is NULL or cUsageIdentifier is set to
//  0, finds certificates having the extension. If
//  CERT_FIND_OPTIONAL_ENHKEY_USAGE_FLAG is set, also matches a certificate
//  without the extension. If CERT_FIND_NO_ENHKEY_USAGE_FLAG is set, finds
//  certificates without the extension.
//
//  If the CERT_FIND_EXT_PROP_ENHKEY_USAGE_FLAG is set, then, only does a match
//  using the property. If pvFindPara is NULL or cUsageIdentifier is set to
//  0, finds certificates having the property. If
//  CERT_FIND_OPTIONAL_ENHKEY_USAGE_FLAG is set, also matches a certificate
//  without the property. If CERT_FIND_NO_ENHKEY_USAGE_FLAG is set, finds
//  certificates without the property.
//--------------------------------------------------------------------------

const CERT_FIND_OPTIONAL_ENHKEY_USAGE_FLAG = $1;
const CERT_FIND_EXT_ONLY_ENHKEY_USAGE_FLAG = $2;
const CERT_FIND_PROP_ONLY_ENHKEY_USAGE_FLAG = $4;
const CERT_FIND_NO_ENHKEY_USAGE_FLAG = $8;
const CERT_FIND_OPTIONAL_CTL_USAGE_FLAG = CERT_FIND_OPTIONAL_ENHKEY_USAGE_FLAG;
const CERT_FIND_EXT_ONLY_CTL_USAGE_FLAG = CERT_FIND_EXT_ONLY_ENHKEY_USAGE_FLAG;
const CERT_FIND_PROP_ONLY_CTL_USAGE_FLAG = CERT_FIND_PROP_ONLY_ENHKEY_USAGE_FLAG;
const CERT_FIND_NO_CTL_USAGE_FLAG = CERT_FIND_NO_ENHKEY_USAGE_FLAG;

//+-------------------------------------------------------------------------
//  Get the certificate context from the store for the first or next issuer
//  of the specified subject certificate. Perform the enabled
//  verification checks on the subject. (Note, the checks are on the subject
//  using the returned issuer certificate.)
//
//  If the first or next issuer certificate isn't found, NULL is returned.
//  Otherwise, a pointer to a read only CERT_CONTEXT is returned. CERT_CONTEXT
//  must be freed by calling CertFreeCertificateContext or is freed when passed as the
//  pPrevIssuerContext on a subsequent call. CertDuplicateCertificateContext
//  can be called to make a duplicate.
//
//  For a self signed subject certificate, NULL is returned with LastError set
//  to CERT_STORE_SELF_SIGNED. The enabled verification checks are still done.
//
//  The pSubjectContext may have been obtained from this store, another store
//  or created by the caller application. When created by the caller, the
//  CertCreateCertificateContext function must have been called.
//
//  An issuer may have multiple certificates. This may occur when the validity
//  period is about to change. pPrevIssuerContext MUST BE NULL on the first
//  call to get the issuer. To get the next certificate for the issuer, the
//  pPrevIssuerContext is set to the CERT_CONTEXT returned by a previous call.
//
//  NOTE: a NON-NULL pPrevIssuerContext is always CertFreeCertificateContext'ed by
//  this function, even for an error.
//
//  The following flags can be set in *pdwFlags to enable verification checks
//  on the subject certificate context:
//      CERT_STORE_SIGNATURE_FLAG     - use the public key in the returned
//                                      issuer certificate to verify the
//                                      signature on the subject certificate.
//                                      Note, if pSubjectContext->hCertStore ==
//                                      hCertStore, the store provider might
//                                      be able to eliminate a redo of
//                                      the signature verify.
//      CERT_STORE_TIME_VALIDITY_FLAG - get the current time and verify that
//                                      its within the subject certificate's
//                                      validity period
//      CERT_STORE_REVOCATION_FLAG    - check if the subject certificate is on
//                                      the issuer's revocation list
//
//  If an enabled verification check fails, then, its flag is set upon return.
//  If CERT_STORE_REVOCATION_FLAG was enabled and the issuer doesn't have a
//  CRL in the store, then, CERT_STORE_NO_CRL_FLAG is set in addition to
//  the CERT_STORE_REVOCATION_FLAG.
//
//  If CERT_STORE_SIGNATURE_FLAG or CERT_STORE_REVOCATION_FLAG is set, then,
//  CERT_STORE_NO_ISSUER_FLAG is set if it doesn't have an issuer certificate
//  in the store.
//
//  For a verification check failure, a pointer to the issuer's CERT_CONTEXT
//  is still returned and SetLastError isn't updated.
//--------------------------------------------------------------------------
function CertGetIssuerCertificateFromStore(hCertStore: HCERTSTORE;
  pSubjectContext: PCCERT_CONTEXT;
  pPrevIssuerContext: PCCERT_CONTEXT; //OPTIONAL
  pdwFlags: PDWORD): PCCERT_CONTEXT; stdcall;
//+-------------------------------------------------------------------------
//  Perform the enabled verification checks on the subject certificate
//  using the issuer. Same checks and flags definitions as for the above
//  CertGetIssuerCertificateFromStore.
//
//  If you are only checking CERT_STORE_TIME_VALIDITY_FLAG, then, the
//  issuer can be NULL.
//
//  For a verification check failure, SUCCESS is still returned.
//--------------------------------------------------------------------------

function CertVerifySubjectCertificateContext(pSubject: PCCERT_CONTEXT;
  pIssuer: PCCERT_CONTEXT; //OPTIONAL
  pdwFlags: PDWORD): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Duplicate a certificate context
//--------------------------------------------------------------------------

function CertDuplicateCertificateContext(pCertContext: PCCERT_CONTEXT): PCCERT_CONTEXT; stdcall;
//+-------------------------------------------------------------------------
//  Create a certificate context from the encoded certificate. The created
//  context isn't put in a store.
//
//  Makes a copy of the encoded certificate in the created context.
//
//  If unable to decode and create the certificate context, NULL is returned.
//  Otherwise, a pointer to a read only CERT_CONTEXT is returned.
//  CERT_CONTEXT must be freed by calling CertFreeCertificateContext.
//  CertDuplicateCertificateContext can be called to make a duplicate.
//
//  CertSetCertificateContextProperty and CertGetCertificateContextProperty can be called
//  to store properties for the certificate.
//--------------------------------------------------------------------------
function CertCreateCertificateContext(dwCertEncodingType: DWORD;
  pbCertEncoded: PBYTE;
  cbCertEncoded: DWORD): PCCERT_CONTEXT; stdcall;
//+-------------------------------------------------------------------------
//  Free a certificate context
//
//  There needs to be a corresponding free for each context obtained by a
//  get, find, duplicate or create.
//--------------------------------------------------------------------------
function CertFreeCertificateContext(pCertContext: PCCERT_CONTEXT): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Set the property for the specified certificate context.
//
//  The type definition for pvData depends on the dwPropId value. There are
//  five predefined types:
//      CERT_KEY_PROV_HANDLE_PROP_ID - a HCRYPTPROV for the certificate's
//      private key is passed in pvData. Updates the hCryptProv field
//      of the CERT_KEY_CONTEXT_PROP_ID. If the CERT_KEY_CONTEXT_PROP_ID
//      doesn't exist, its created with all the other fields zeroed out. If
//      CERT_STORE_NO_CRYPT_RELEASE_FLAG isn't set, HCRYPTPROV is implicitly
//      released when either the property is set to NULL or on the final
//      free of the CertContext.
//
//      CERT_KEY_PROV_INFO_PROP_ID - a PCRYPT_KEY_PROV_INFO for the certificate's
//      private key is passed in pvData.
//
//      CERT_SHA1_HASH_PROP_ID -
//      CERT_MD5_HASH_PROP_ID  - normally, either property is implicitly
//      set by doing a CertGetCertificateContextProperty. pvData points to a
//      CRYPT_HASH_BLOB.
//
//      CERT_KEY_CONTEXT_PROP_ID - a PCERT_KEY_CONTEXT for the certificate's
//      private key is passed in pvData. The CERT_KEY_CONTEXT contains both the
//      hCryptProv and dwKeySpec for the private key.
//      See the CERT_KEY_PROV_HANDLE_PROP_ID for more information about
//      the hCryptProv field and dwFlags settings. Note, more fields may
//      be added for this property. The cbSize field value will be adjusted
//      accordingly.
//
//      CERT_KEY_SPEC_PROP_ID - the dwKeySpec for the private key. pvData
//      points to a DWORD containing the KeySpec
//
//      CERT_ENHKEY_USAGE_PROP_ID - enhanced key usage definition for the
//      certificate. pvData points to a CRYPT_DATA_BLOB containing an
//      ASN.1 encoded CERT_ENHKEY_USAGE (encoded via
//      CryptEncodeObject(X509_ENHANCED_KEY_USAGE).
//
//      CERT_NEXT_UPDATE_LOCATION_PROP_ID - location of the next update.
//      Currently only applicable to CTLs. pvData points to a CRYPT_DATA_BLOB
//      containing an ASN.1 encoded CERT_ALT_NAME_INFO (encoded via
//      CryptEncodeObject(X509_ALTERNATE_NAME)).
//
//      CERT_FRIENDLY_NAME_PROP_ID - friendly name for the cert, CRL or CTL.
//      pvData points to a CRYPT_DATA_BLOB. pbData is a pointer to a NULL
//      terminated unicode, wide character string.
//      cbData = (wcslen((LPWSTR) pbData) + 1) * sizeof(WCHAR).
//
//  For all the other PROP_IDs: an encoded PCRYPT_DATA_BLOB is passed in pvData.
//
//  If the property already exists, then, the old value is deleted and silently
//  replaced. Setting, pvData to NULL, deletes the property.
//--------------------------------------------------------------------------
function CertSetCertificateContextProperty(pCertContext: PCCERT_CONTEXT;
  dwPropId: DWORD;
  dwFlags: DWORD;
  pvData: PVOID): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Get the property for the specified certificate context.
//
//  For CERT_KEY_PROV_HANDLE_PROP_ID, pvData points to a HCRYPTPROV.
//
//  For CERT_KEY_PROV_INFO_PROP_ID, pvData points to a CRYPT_KEY_PROV_INFO structure.
//  Elements pointed to by fields in the pvData structure follow the
//  structure. Therefore, *pcbData may exceed the size of the structure.
//
//  For CERT_KEY_CONTEXT_PROP_ID, pvData points to a CERT_KEY_CONTEXT structure.
//
//  For CERT_KEY_SPEC_PROP_ID, pvData points to a DWORD containing the KeySpec.
//  If the CERT_KEY_CONTEXT_PROP_ID exists, the KeySpec is obtained from there.
//  Otherwise, if the CERT_KEY_PROV_INFO_PROP_ID exists, its the source
//  of the KeySpec.
//
//  For CERT_SHA1_HASH_PROP_ID or CERT_MD5_HASH_PROP_ID, if the hash
//  doesn't already exist, then, its computed via CryptHashCertificate()
//  and then set. pvData points to the computed hash. Normally, the length
//  is 20 bytes for SHA and 16 for MD5.
//
//  For all other PROP_IDs, pvData points to an encoded array of bytes.
//--------------------------------------------------------------------------
function CertGetCertificateContextProperty(pCertContext: PCCERT_CONTEXT;
  dwPropId: DWORD;
  pvData: PVOID;
  pcbData: PDWORD): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Enumerate the properties for the specified certificate context.
//
//  To get the first property, set dwPropId to 0. The ID of the first
//  property is returned. To get the next property, set dwPropId to the
//  ID returned by the last call. To enumerate all the properties continue
//  until 0 is returned.
//
//  CertGetCertificateContextProperty is called to get the property's data.
//
//  Note, since, the CERT_KEY_PROV_HANDLE_PROP_ID and CERT_KEY_SPEC_PROP_ID
//  properties are stored as fields in the CERT_KEY_CONTEXT_PROP_ID
//  property, they aren't enumerated individually.
//--------------------------------------------------------------------------
function CertEnumCertificateContextProperties(pCertContext: PCCERT_CONTEXT;
  dwPropId: DWORD): DWORD; stdcall;
//+-------------------------------------------------------------------------
//  Get the first or next CRL context from the store for the specified
//  issuer certificate. Perform the enabled verification checks on the CRL.
//
//  If the first or next CRL isn't found, NULL is returned.
//  Otherwise, a pointer to a read only CRL_CONTEXT is returned. CRL_CONTEXT
//  must be freed by calling CertFreeCRLContext. However, the free must be
//  pPrevCrlContext on a subsequent call. CertDuplicateCRLContext
//  can be called to make a duplicate.
//
//  The pIssuerContext may have been obtained from this store, another store
//  or created by the caller application. When created by the caller, the
//  CertCreateCertificateContext function must have been called.
//
//  If pIssuerContext == NULL, finds all the CRLs in the store.
//
//  An issuer may have multiple CRLs. For example, it generates delta CRLs
//  using a X.509 v3 extension. pPrevCrlContext MUST BE NULL on the first
//  call to get the CRL. To get the next CRL for the issuer, the
//  pPrevCrlContext is set to the CRL_CONTEXT returned by a previous call.
//
//  NOTE: a NON-NULL pPrevCrlContext is always CertFreeCRLContext'ed by
//  this function, even for an error.
//
//  The following flags can be set in *pdwFlags to enable verification checks
//  on the returned CRL:
//      CERT_STORE_SIGNATURE_FLAG     - use the public key in the
//                                      issuer's certificate to verify the
//                                      signature on the returned CRL.
//                                      Note, if pIssuerContext->hCertStore ==
//                                      hCertStore, the store provider might
//                                      be able to eliminate a redo of
//                                      the signature verify.
//      CERT_STORE_TIME_VALIDITY_FLAG - get the current time and verify that
//                                      its within the CRL's ThisUpdate and
//                                      NextUpdate validity period.
//
//  If an enabled verification check fails, then, its flag is set upon return.
//
//  If pIssuerContext == NULL, then, an enabled CERT_STORE_SIGNATURE_FLAG
//  always fails and the CERT_STORE_NO_ISSUER_FLAG is also set.
//
//  For a verification check failure, a pointer to the first or next
//  CRL_CONTEXT is still returned and SetLastError isn't updated.
//--------------------------------------------------------------------------
function CertGetCRLFromStore(hCertStore: HCERTSTORE;
  pIssuerContext: PCCERT_CONTEXT; //OPTIONAL
  pPrevCrlContext: PCCRL_CONTEXT;
  pdwFlags: PDWORD): PCCRL_CONTEXT; stdcall;
//+-------------------------------------------------------------------------
//  Duplicate a CRL context
//--------------------------------------------------------------------------
function CertDuplicateCRLContext(pCrlContext: PCCRL_CONTEXT): PCCRL_CONTEXT; stdcall;
//+-------------------------------------------------------------------------
//  Create a CRL context from the encoded CRL. The created
//  context isn't put in a store.
//
//  Makes a copy of the encoded CRL in the created context.
//
//  If unable to decode and create the CRL context, NULL is returned.
//  Otherwise, a pointer to a read only CRL_CONTEXT is returned.
//  CRL_CONTEXT must be freed by calling CertFreeCRLContext.
//  CertDuplicateCRLContext can be called to make a duplicate.
//
//  CertSetCRLContextProperty and CertGetCRLContextProperty can be called
//  to store properties for the CRL.
//--------------------------------------------------------------------------
function CertCreateCRLContext(dwCertEncodingType: DWORD;
  pbCrlEncoded: PBYTE;
  cbCrlEncoded: DWORD): PCCRL_CONTEXT; stdcall;
//+-------------------------------------------------------------------------
//  Free a CRL context
//
//  There needs to be a corresponding free for each context obtained by a
//  get, duplicate or create.
//--------------------------------------------------------------------------
function CertFreeCRLContext(pCrlContext: PCCRL_CONTEXT): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Set the property for the specified CRL context.
//
//  Same Property Ids and semantics as CertSetCertificateContextProperty.
//--------------------------------------------------------------------------
function CertSetCRLContextProperty(pCrlContext: PCCRL_CONTEXT;
  dwPropId: DWORD;
  dwFlags: DWORD;
  const pvData: PVOID): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Get the property for the specified CRL context.
//
//  Same Property Ids and semantics as CertGetCertificateContextProperty.
//
//  CERT_SHA1_HASH_PROP_ID or CERT_MD5_HASH_PROP_ID is the predefined
//  property of most interest.
//--------------------------------------------------------------------------
function CertGetCRLContextProperty(pCrlContext: PCCRL_CONTEXT;
  dwPropId: DWORD;
  pvData: PVOID;
  pcbData: PDWORD): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Enumerate the properties for the specified CRL context.
//
//  To get the first property, set dwPropId to 0. The ID of the first
//  property is returned. To get the next property, set dwPropId to the
//  ID returned by the last call. To enumerate all the properties continue
//  until 0 is returned.
//
//  CertGetCRLContextProperty is called to get the property's data.
//--------------------------------------------------------------------------
function CertEnumCRLContextProperties(pCrlContext: PCCRL_CONTEXT;
  dwPropId: DWORD): DWORD; stdcall;
//+-------------------------------------------------------------------------
// Add certificate/CRL, encoded, context or element disposition values.
//--------------------------------------------------------------------------
const CERT_STORE_ADD_NEW = 1;
const CERT_STORE_ADD_USE_EXISTING = 2;
const CERT_STORE_ADD_REPLACE_EXISTING = 3;
const CERT_STORE_ADD_ALWAYS = 4;

//+-------------------------------------------------------------------------
//  Add the encoded certificate to the store according to the specified
//  disposition action.
//
//  Makes a copy of the encoded certificate before adding to the store.
//
//  dwAddDispostion specifies the action to take if the certificate
//  already exists in the store. This parameter must be one of the following
//  values:
//    CERT_STORE_ADD_NEW
//      Fails if the certificate already exists in the store. LastError
//      is set to CRYPT_E_EXISTS.
//    CERT_STORE_ADD_USE_EXISTING
//      If the certifcate already exists, then, its used and if ppCertContext
//      is non-NULL, the existing context is duplicated.
//    CERT_STORE_ADD_REPLACE_EXISTING
//      If the certificate already exists, then, the existing certificate
//      context is deleted before creating and adding the new context.
//    CERT_STORE_ADD_ALWAYS
//      No check is made to see if the certificate already exists. A
//      new certificate context is always created. This may lead to
//      duplicates in the store.
//
//  CertGetSubjectCertificateFromStore is called to determine if the
//  certificate already exists in the store.
//
//  ppCertContext can be NULL, indicating the caller isn't interested
//  in getting the CERT_CONTEXT of the added or existing certificate.
//--------------------------------------------------------------------------
function CertAddEncodedCertificateToStore(hCertStore: HCERTSTORE;
  dwCertEncodingType: DWORD;
  const pbCertEncoded: PBYTE;
  cbCertEncoded: DWORD;
  dwAddDisposition: DWORD;
  var ppCertContext: PCCERT_CONTEXT): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Add the certificate context to the store according to the specified
//  disposition action.
//
//  In addition to the encoded certificate, the context's properties are
//  also copied.  Note, the CERT_KEY_CONTEXT_PROP_ID property (and its
//  CERT_KEY_PROV_HANDLE_PROP_ID or CERT_KEY_SPEC_PROP_ID) isn't copied.
//
//  Makes a copy of the certificate context before adding to the store.
//
//  dwAddDispostion specifies the action to take if the certificate
//  already exists in the store. This parameter must be one of the following
//  values:
//    CERT_STORE_ADD_NEW
//      Fails if the certificate already exists in the store. LastError
//      is set to CRYPT_E_EXISTS.
//    CERT_STORE_ADD_USE_EXISTING
//      If the certifcate already exists, then, its used and if ppStoreContext
//      is non-NULL, the existing context is duplicated. Iterates
//      through pCertContext's properties and only copies the properties
//      that don't already exist. The SHA1 and MD5 hash properties aren't
//      copied.
//    CERT_STORE_ADD_REPLACE_EXISTING
//      If the certificate already exists, then, the existing certificate
//      context is deleted before creating and adding a new context.
//      Properties are copied before doing the add.
//    CERT_STORE_ADD_ALWAYS
//      No check is made to see if the certificate already exists. A
//      new certificate context is always created and added. This may lead to
//      duplicates in the store. Properties are
//      copied before doing the add.
//
//  CertGetSubjectCertificateFromStore is called to determine if the
//  certificate already exists in the store.
//
//  ppStoreContext can be NULL, indicating the caller isn't interested
//  in getting the CERT_CONTEXT of the added or existing certificate.
//--------------------------------------------------------------------------
function CertAddCertificateContextToStore(hCertStore: HCERTSTORE;
  pCertContext: PCCERT_CONTEXT;
  dwAddDisposition: DWORD;
  var ppStoreContext: PCCERT_CONTEXT //OPTIONAL
  ): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Certificate Store Context Types
//--------------------------------------------------------------------------
const CERT_STORE_CERTIFICATE_CONTEXT = 1;
const CERT_STORE_CRL_CONTEXT = 2;
const CERT_STORE_CTL_CONTEXT = 3;

//+-------------------------------------------------------------------------
//  Certificate Store Context Bit Flags
//--------------------------------------------------------------------------
const CERT_STORE_ALL_CONTEXT_FLAG = (not ULONG(0));
const CERT_STORE_CERTIFICATE_CONTEXT_FLAG = (1 shl CERT_STORE_CERTIFICATE_CONTEXT);
const CERT_STORE_CRL_CONTEXT_FLAG = (1 shl CERT_STORE_CRL_CONTEXT);
const CERT_STORE_CTL_CONTEXT_FLAG = (1 shl CERT_STORE_CTL_CONTEXT);

//+-------------------------------------------------------------------------
//  Add the serialized certificate or CRL element to the store.
//
//  The serialized element contains the encoded certificate, CRL or CTL and
//  its properties, such as, CERT_KEY_PROV_INFO_PROP_ID.
//
//  If hCertStore is NULL, creates a certificate, CRL or CTL context not
//  residing in any store.
//
//  dwAddDispostion specifies the action to take if the certificate or CRL
//  already exists in the store. See CertAddCertificateContextToStore for a
//  list of and actions taken.
//
//  dwFlags currently isn't used and should be set to 0.
//
//  dwContextTypeFlags specifies the set of allowable contexts. For example, to
//  add either a certificate or CRL, set dwContextTypeFlags to:
//      CERT_STORE_CERTIFICATE_CONTEXT_FLAG | CERT_STORE_CRL_CONTEXT_FLAG
//
//  *pdwContextType is updated with the type of the context returned in
//  *ppvContxt. pdwContextType or ppvContext can be NULL, indicating the
//  caller isn't interested in getting the output. If *ppvContext is
//  returned it must be freed by calling CertFreeCertificateContext or
//  CertFreeCRLContext.
//--------------------------------------------------------------------------
function CertAddSerializedElementToStore(hCertStore: HCERTSTORE;
  pbElement: PBYTE;
  cbElement: DWORD;
  dwAddDisposition: DWORD;
  dwFlags: DWORD;
  dwContextTypeFlags: DWORD;
  pdwContextType: PDWORD;
  var ppvContext: array of PVOID): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Delete the specified certificate from the store.
//
//  All subsequent gets or finds for the certificate will fail. However,
//  memory allocated for the certificate isn't freed until all of its contexts
//  have also been freed.
//
//  The pCertContext is obtained from a get, enum, find or duplicate.
//
//  Some store provider implementations might also delete the issuer's CRLs
//  if this is the last certificate for the issuer in the store.
//
//  NOTE: the pCertContext is always CertFreeCertificateContext'ed by
//  this function, even for an error.
//--------------------------------------------------------------------------
function CertDeleteCertificateFromStore(pCertContext: PCCERT_CONTEXT): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Add the encoded CRL to the store according to the specified
//  disposition option.
//
//  Makes a copy of the encoded CRL before adding to the store.
//
//  dwAddDispostion specifies the action to take if the CRL
//  already exists in the store. See CertAddEncodedCertificateToStore for a
//  list of and actions taken.
//
//  Compares the CRL's Issuer to determine if the CRL already exists in the
//  store.
//
//  ppCrlContext can be NULL, indicating the caller isn't interested
//  in getting the CRL_CONTEXT of the added or existing CRL.
//--------------------------------------------------------------------------
function CertAddEncodedCRLToStore(hCertStore: HCERTSTORE;
  dwCertEncodingType: DWORD;
  pbCrlEncoded: PBYTE;
  cbCrlEncoded: DWORD;
  dwAddDisposition: DWORD;
  var ppCrlContext: PCCRL_CONTEXT
  ): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Add the CRL context to the store according to the specified
//  disposition option.
//
//  In addition to the encoded CRL, the context's properties are
//  also copied.  Note, the CERT_KEY_CONTEXT_PROP_ID property (and its
//  CERT_KEY_PROV_HANDLE_PROP_ID or CERT_KEY_SPEC_PROP_ID) isn't copied.
//
//  Makes a copy of the encoded CRL before adding to the store.
//
//  dwAddDispostion specifies the action to take if the CRL
//  already exists in the store. See CertAddCertificateContextToStore for a
//  list of and actions taken.
//
//  Compares the CRL's Issuer, ThisUpdate and NextUpdate to determine
//  if the CRL already exists in the store.
//
//  ppStoreContext can be NULL, indicating the caller isn't interested
//  in getting the CRL_CONTEXT of the added or existing CRL.
//--------------------------------------------------------------------------
function CertAddCRLContextToStore(hCertStore: HCERTSTORE;
  pCrlContext: PCCRL_CONTEXT;
  dwAddDisposition: DWORD;
  var ppStoreContext: PCCRL_CONTEXT
  ): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Delete the specified CRL from the store.
//
//  All subsequent gets for the CRL will fail. However,
//  memory allocated for the CRL isn't freed until all of its contexts
//  have also been freed.
//
//  The pCrlContext is obtained from a get or duplicate.
//
//  NOTE: the pCrlContext is always CertFreeCRLContext'ed by
//  this function, even for an error.
//--------------------------------------------------------------------------
function CertDeleteCRLFromStore(pCrlContext: PCCRL_CONTEXT): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Serialize the certificate context's encoded certificate and its
//  properties.
//--------------------------------------------------------------------------
function CertSerializeCertificateStoreElement(pCertContext: PCCERT_CONTEXT;
  dwFlags: DWORD;
  pbElement: PBYTE;
  pcbElement: PDWORD): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Serialize the CRL context's encoded CRL and its properties.
//--------------------------------------------------------------------------
function CertSerializeCRLStoreElement(pCrlContext: PCCRL_CONTEXT;
  dwFlags: DWORD;
  pbElement: PBYTE;
  pcbElement: PDWORD): BOOL; stdcall;
//+=========================================================================
//  Certificate Trust List (CTL) Store Data Structures and APIs
//==========================================================================

//+-------------------------------------------------------------------------
//  Duplicate a CTL context
//--------------------------------------------------------------------------
function CertDuplicateCTLContext(pCtlContext: PCCTL_CONTEXT): PCCTL_CONTEXT; stdcall;
//+-------------------------------------------------------------------------
//  Create a CTL context from the encoded CTL. The created
//  context isn't put in a store.
//
//  Makes a copy of the encoded CTL in the created context.
//
//  If unable to decode and create the CTL context, NULL is returned.
//  Otherwise, a pointer to a read only CTL_CONTEXT is returned.
//  CTL_CONTEXT must be freed by calling CertFreeCTLContext.
//  CertDuplicateCTLContext can be called to make a duplicate.
//
//  CertSetCTLContextProperty and CertGetCTLContextProperty can be called
//  to store properties for the CTL.
//--------------------------------------------------------------------------
function CertCreateCTLContext(dwMsgAndCertEncodingType: DWORD;
  const pbCtlEncoded: PBYTE;
  cbCtlEncoded: DWORD): PCCTL_CONTEXT; stdcall;
//+-------------------------------------------------------------------------
//  Free a CTL context
//
//  There needs to be a corresponding free for each context obtained by a
//  get, duplicate or create.
//--------------------------------------------------------------------------
function CertFreeCTLContext(pCtlContext: PCCTL_CONTEXT): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Set the property for the specified CTL context.
//
//  Same Property Ids and semantics as CertSetCertificateContextProperty.
//--------------------------------------------------------------------------
function CertSetCTLContextProperty(pCtlContext: PCCTL_CONTEXT;
  dwPropId: DWORD;
  dwFlags: DWORD;
  const pvData: PVOID): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Get the property for the specified CTL context.
//
//  Same Property Ids and semantics as CertGetCertificateContextProperty.
//
//  CERT_SHA1_HASH_PROP_ID or CERT_NEXT_UPDATE_LOCATION_PROP_ID are the
//  predefined properties of most interest.
//--------------------------------------------------------------------------
function CertGetCTLContextProperty(pCtlContext: PCCTL_CONTEXT;
  dwPropId: DWORD;
  pvData: PVOID;
  pcbData: PDWORD): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Enumerate the properties for the specified CTL context.
//--------------------------------------------------------------------------
function CertEnumCTLContextProperties(pCtlContext: PCCTL_CONTEXT;
  dwPropId: DWORD): DWORD; stdcall;
//+-------------------------------------------------------------------------
//  Enumerate the CTL contexts in the store.
//
//  If a CTL isn't found, NULL is returned.
//  Otherwise, a pointer to a read only CTL_CONTEXT is returned. CTL_CONTEXT
//  must be freed by calling CertFreeCTLContext or is freed when passed as the
//  pPrevCtlContext on a subsequent call. CertDuplicateCTLContext
//  can be called to make a duplicate.
//
//  pPrevCtlContext MUST BE NULL to enumerate the first
//  CTL in the store. Successive CTLs are enumerated by setting
//  pPrevCtlContext to the CTL_CONTEXT returned by a previous call.
//
//  NOTE: a NON-NULL pPrevCtlContext is always CertFreeCTLContext'ed by
//  this function, even for an error.
//--------------------------------------------------------------------------
function CertEnumCTLsInStore(hCertStore: HCERTSTORE;
  pPrevCtlContext: PCCTL_CONTEXT
  ): PCCTL_CONTEXT; stdcall;
//+-------------------------------------------------------------------------
//  Attempt to find the specified subject in the CTL.
//
//  For CTL_CERT_SUBJECT_TYPE, pvSubject points to a CERT_CONTEXT. The CTL's
//  SubjectAlgorithm is examined to determine the representation of the
//  subject's identity. Initially, only SHA1 or MD5 hash will be supported.
//  The appropriate hash property is obtained from the CERT_CONTEXT.
//
//  For CTL_ANY_SUBJECT_TYPE, pvSubject points to the CTL_ANY_SUBJECT_INFO
//  structure which contains the SubjectAlgorithm to be matched in the CTL
//  and the SubjectIdentifer to be matched in one of the CTL entries.
//
//  The certificate's hash or the CTL_ANY_SUBJECT_INFO's SubjectIdentifier
//  is used as the key in searching the subject entries. A binary
//  memory comparison is done between the key and the entry's SubjectIdentifer.
//
//  dwEncodingType isn't used for either of the above SubjectTypes.
//--------------------------------------------------------------------------
function CertFindSubjectInCTL(dwEncodingType: DWORD;
  dwSubjectType: DWORD;
  pvSubject: PVOID;
  pCtlContext: PCCTL_CONTEXT;
  dwFlags: DWORD): PCTL_ENTRY; stdcall;
// Subject Types:
//  CTL_ANY_SUBJECT_TYPE, pvSubject points to following CTL_ANY_SUBJECT_INFO.
//  CTL_CERT_SUBJECT_TYPE, pvSubject points to CERT_CONTEXT.
const CTL_ANY_SUBJECT_TYPE = 1;
const CTL_CERT_SUBJECT_TYPE = 2;

type
  PCTL_ANY_SUBJECT_INFO = ^CTL_ANY_SUBJECT_INFO;
  CTL_ANY_SUBJECT_INFO = record
    SubjectAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    SubjectIdentifier: CRYPT_DATA_BLOB;
  end;

//+-------------------------------------------------------------------------
//  Find the first or next CTL context in the store.
//
//  The CTL is found according to the dwFindType and its pvFindPara.
//  See below for a list of the find types and its parameters.
//
//  Currently dwFindFlags isn't used and must be set to 0.
//
//  Usage of dwMsgAndCertEncodingType depends on the dwFindType.
//
//  If the first or next CTL isn't found, NULL is returned.
//  Otherwise, a pointer to a read only CTL_CONTEXT is returned. CTL_CONTEXT
//  must be freed by calling CertFreeCTLContext or is freed when passed as the
//  pPrevCtlContext on a subsequent call. CertDuplicateCTLContext
//  can be called to make a duplicate.
//
//  pPrevCtlContext MUST BE NULL on the first
//  call to find the CTL. To find the next CTL, the
//  pPrevCtlContext is set to the CTL_CONTEXT returned by a previous call.
//
//  NOTE: a NON-NULL pPrevCtlContext is always CertFreeCTLContext'ed by
//  this function, even for an error.
//--------------------------------------------------------------------------
function CertFindCTLInStore(hCertStore: HCERTSTORE;
  dwMsgAndCertEncodingType: DWORD;
  dwFindFlags: DWORD;
  dwFindType: DWORD;
  const pvFindPara: PVOID;
  pPrevCtlContext: PCCTL_CONTEXT): PCCTL_CONTEXT; stdcall;

const CTL_FIND_ANY = 0;
const CTL_FIND_SHA1_HASH = 1;
const CTL_FIND_MD5_HASH = 2;
const CTL_FIND_USAGE = 3;
const CTL_FIND_SUBJECT = 4;

type
  PCTL_FIND_USAGE_PARA = ^CTL_FIND_USAGE_PARA;
  CTL_FIND_USAGE_PARA = record
    cbSize: DWORD;
    SubjectUsage: CTL_USAGE; // optional
    ListIdentifier: CRYPT_DATA_BLOB; // optional
    pSigner: PCERT_INFO; // optional
  end;

const CTL_FIND_NO_LIST_ID_CBDATA = $FFFFFFFF;
const CTL_FIND_NO_SIGNER_PTR = (PCERT_INFO($FFFFFFFF));

const CTL_FIND_SAME_USAGE_FLAG = $1;

type
  PCTL_FIND_SUBJECT_PARA = ^CTL_FIND_SUBJECT_PARA;
  CTL_FIND_SUBJECT_PARA = record
    cbSize: DWORD;
    pUsagePara: PCTL_FIND_USAGE_PARA; // optional
    dwSubjectType: DWORD;
    pvSubject: PVOID;
  end;

//+-------------------------------------------------------------------------
//  CTL_FIND_ANY
//
//  Find any CTL.
//
//  pvFindPara isn't used.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CTL_FIND_SHA1_HASH
//  CTL_FIND_MD5_HASH
//
//  Find a CTL with the specified hash.
//
//  pvFindPara points to a CRYPT_HASH_BLOB.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CTL_FIND_USAGE
//
//  Find a CTL having the specified usage identifiers, list identifier or
//  signer. The CertEncodingType of the signer is obtained from the
//  dwMsgAndCertEncodingType parameter.
//
//  pvFindPara points to a CTL_FIND_USAGE_PARA data structure. The
//  SubjectUsage.cUsageIdentifer can be 0 to match any usage. The
//  ListIdentifier.cbData can be 0 to match any list identifier. To only match
//  CTLs without a ListIdentifier, cbData must be set to
//  CTL_FIND_NO_LIST_ID_CBDATA. pSigner can be NULL to match any signer. Only
//  the Issuer and SerialNumber fields of the pSigner's PCERT_INFO are used.
//  To only match CTLs without a signer, pSigner must be set to
//  CTL_FIND_NO_SIGNER_PTR.
//
//  The CTL_FIND_SAME_USAGE_FLAG can be set in dwFindFlags to
//  only match CTLs with the same usage identifiers. CTLs having additional
//  usage identifiers aren't matched. For example, if only "1.2.3" is specified
//  in CTL_FIND_USAGE_PARA, then, for a match, the CTL must only contain
//  "1.2.3" and not any additional usage identifers.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CTL_FIND_SUBJECT
//
//  Find a CTL having the specified subject. CertFindSubjectInCTL can be
//  called to get a pointer to the subject's entry in the CTL.  pUsagePara can
//  optionally be set to enable the above CTL_FIND_USAGE matching.
//
//  pvFindPara points to a CTL_FIND_SUBJECT_PARA data structure.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  Add the encoded CTL to the store according to the specified
//  disposition option.
//
//  Makes a copy of the encoded CTL before adding to the store.
//
//  dwAddDispostion specifies the action to take if the CTL
//  already exists in the store. See CertAddEncodedCertificateToStore for a
//  list of and actions taken.
//
//  Compares the CTL's SubjectUsage, ListIdentifier and any of its signers
//  to determine if the CTL already exists in the store.
//
//  ppCtlContext can be NULL, indicating the caller isn't interested
//  in getting the CTL_CONTEXT of the added or existing CTL.
//--------------------------------------------------------------------------
function CertAddEncodedCTLToStore(hCertStore: HCERTSTORE;
  dwMsgAndCertEncodingType: DWORD;
  const pbCtlEncoded: PBYTE;
  cbCtlEncoded: DWORD;
  dwAddDisposition: DWORD;
  var ppCtlContext: PCCTL_CONTEXT //OPTIONAL
  ): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Add the CTL context to the store according to the specified
//  disposition option.
//
//  In addition to the encoded CTL, the context's properties are
//  also copied.  Note, the CERT_KEY_CONTEXT_PROP_ID property (and its
//  CERT_KEY_PROV_HANDLE_PROP_ID or CERT_KEY_SPEC_PROP_ID) isn't copied.
//
//  Makes a copy of the encoded CTL before adding to the store.
//
//  dwAddDispostion specifies the action to take if the CTL
//  already exists in the store. See CertAddCertificateContextToStore for a
//  list of and actions taken.
//
//  Compares the CTL's SubjectUsage, ListIdentifier and any of its signers
//  to determine if the CTL already exists in the store.
//
//  ppStoreContext can be NULL, indicating the caller isn't interested
//  in getting the CTL_CONTEXT of the added or existing CTL.
//--------------------------------------------------------------------------
function CertAddCTLContextToStore(hCertStore: HCERTSTORE;
  pCtlContext: PCCTL_CONTEXT;
  dwAddDisposition: DWORD;
  var ppStoreContext: PCCTL_CONTEXT
  ): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Serialize the CTL context's encoded CTL and its properties.
//--------------------------------------------------------------------------
function CertSerializeCTLStoreElement(pCtlContext: PCCTL_CONTEXT;
  dwFlags: DWORD;
  pbElement: PBYTE;
  pcbElement: PDWORD): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Delete the specified CTL from the store.
//
//  All subsequent gets for the CTL will fail. However,
//  memory allocated for the CTL isn't freed until all of its contexts
//  have also been freed.
//
//  The pCtlContext is obtained from a get or duplicate.
//
//  NOTE: the pCtlContext is always CertFreeCTLContext'ed by
//  this function, even for an error.
//--------------------------------------------------------------------------
function CertDeleteCTLFromStore(pCtlContext: PCCTL_CONTEXT): BOOL; stdcall;
//+=========================================================================
//  Enhanced Key Usage Helper Functions
//==========================================================================

//+-------------------------------------------------------------------------
//  Get the enhanced key usage extension or property from the certificate
//  and decode.
//
//  If the CERT_FIND_EXT_ONLY_ENHKEY_USAGE_FLAG is set, then, only get the
//  extension.
//
//  If the CERT_FIND_PROP_ONLY_ENHKEY_USAGE_FLAG is set, then, only get the
//  property.
//--------------------------------------------------------------------------
function CertGetEnhancedKeyUsage(pCertContext: PCCERT_CONTEXT;
  dwFlags: DWORD;
  pUsage: PCERT_ENHKEY_USAGE;
  pcbUsage: PDWORD): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Set the enhanced key usage property for the certificate.
//--------------------------------------------------------------------------
function CertSetEnhancedKeyUsage(pCertContext: PCCERT_CONTEXT;
  pUsage: PCERT_ENHKEY_USAGE
  ): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Add the usage identifier to the certificate's enhanced key usage property.
//--------------------------------------------------------------------------
function CertAddEnhancedKeyUsageIdentifier(pCertContext: PCCERT_CONTEXT;
  pszUsageIdentifier: LPCSTR
  ): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Remove the usage identifier from the certificate's enhanced key usage
//  property.
//--------------------------------------------------------------------------
function CertRemoveEnhancedKeyUsageIdentifier(pCertContext: PCCERT_CONTEXT;
  pszUsageIdentifier: LPCSTR
  ): BOOL; stdcall;

//+=========================================================================
//  Cryptographic Message helper functions for verifying and signing a
//  CTL.
//==========================================================================

//+-------------------------------------------------------------------------
//  Get and verify the signer of a cryptographic message.
//
//  To verify a CTL, the hCryptMsg is obtained from the CTL_CONTEXT's
//  hCryptMsg field.
//
//  If CMSG_TRUSTED_SIGNER_FLAG is set, then, treat the Signer stores as being
//  trusted and only search them to find the certificate corresponding to the
//  signer's issuer and serial number.  Otherwise, the SignerStores are
//  optionally provided to supplement the message's store of certificates.
//  If a signer certificate is found, its public key is used to verify
//  the message signature. The CMSG_SIGNER_ONLY_FLAG can be set to
//  return the signer without doing the signature verify.
//
//  If CMSG_USE_SIGNER_INDEX_FLAG is set, then, only get the signer specified
//  by *pdwSignerIndex. Otherwise, iterate through all the signers
//  until a signer verifies or no more signers.
//
//  For a verified signature, *ppSigner is updated with certificate context
//  of the signer and *pdwSignerIndex is updated with the index of the signer.
//  ppSigner and/or pdwSignerIndex can be NULL, indicating the caller isn't
//  interested in getting the CertContext and/or index of the signer.
//--------------------------------------------------------------------------
function CryptMsgGetAndVerifySigner(hCryptMsg: HCRYPTMSG;
  cSignerStore: DWORD;
  var rghSignerStore: HCERTSTORE;
  dwFlags: DWORD;
  var ppSigner: PCCERT_CONTEXT;
  pdwSignerIndex: PDWORD): BOOL; stdcall;

const CMSG_TRUSTED_SIGNER_FLAG = $1;
const CMSG_SIGNER_ONLY_FLAG = $2;
const CMSG_USE_SIGNER_INDEX_FLAG = $4;

//+-------------------------------------------------------------------------
//  Sign an encoded CTL.
//
//  The pbCtlContent can be obtained via a CTL_CONTEXT's pbCtlContent
//  field or via a CryptEncodeObject(PKCS_CTL).
//--------------------------------------------------------------------------
function CryptMsgSignCTL(dwMsgEncodingType: DWORD;
  pbCtlContent: PBYTE;
  cbCtlContent: DWORD;
  pSignInfo: PCMSG_SIGNED_ENCODE_INFO;
  dwFlags: DWORD;
  pbEncoded: PBYTE;
  pcbEncoded: PDWORD): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Encode the CTL and create a signed message containing the encoded CTL.
//--------------------------------------------------------------------------
function CryptMsgEncodeAndSignCTL(dwMsgEncodingType: DWORD;
  pCtlInfo: PCTL_INFO;
  pSignInfo: PCMSG_SIGNED_ENCODE_INFO;
  dwFlags: DWORD;
  pbEncoded: PBYTE;
  pcbEncoded: PDWORD): BOOL; stdcall;

//+=========================================================================
//  Certificate Verify CTL Usage Data Structures and APIs
//==========================================================================
type
  PHCERTSTORE = ^HCERTSTORE;

type
  PCTL_VERIFY_USAGE_PARA = ^CTL_VERIFY_USAGE_PARA;
  CTL_VERIFY_USAGE_PARA = record
    cbSize: DWORD;
    ListIdentifier: CRYPT_DATA_BLOB; // OPTIONAL
    cCtlStore: DWORD;
    rghCtlStore: PHCERTSTORE; // OPTIONAL
    cSignerStore: DWORD;
    rghSignerStore: PHCERTSTORE; // OPTIONAL
  end;

type
  PCTL_VERIFY_USAGE_STATUS = ^CTL_VERIFY_USAGE_STATUS;
  CTL_VERIFY_USAGE_STATUS = record
    cbSize: DWORD;
    dwError: DWORD;
    dwFlags: DWORD;
    ppCtl: PPCCTL_CONTEXT; // IN OUT OPTIONAL
    dwCtlEntryIndex: DWORD;
    ppSigner: PPCCERT_CONTEXT; // IN OUT OPTIONAL
    dwSignerIndex: DWORD;
  end;

const CERT_VERIFY_INHIBIT_CTL_UPDATE_FLAG = $1;
const CERT_VERIFY_TRUSTED_SIGNERS_FLAG = $2;
const CERT_VERIFY_NO_TIME_CHECK_FLAG = $4;
const CERT_VERIFY_ALLOW_MORE_USAGE_FLAG = $8;

const CERT_VERIFY_UPDATED_CTL_FLAG = $1;

//+-------------------------------------------------------------------------
//  Verify that a subject is trusted for the specified usage by finding a
//  signed and time valid CTL with the usage identifiers and containing the
//  the subject. A subject can be identified by either its certificate context
//  or any identifier such as its SHA1 hash.
//
//  See CertFindSubjectInCTL for definition of dwSubjectType and pvSubject
//  parameters.
//
//  Via pVerifyUsagePara, the caller can specify the stores to be searched
//  to find the CTL. The caller can also specify the stores containing
//  acceptable CTL signers. By setting the ListIdentifier, the caller
//  can also restrict to a particular signer CTL list.
//
//  Via pVerifyUsageStatus, the CTL containing the subject, the subject's
//  index into the CTL's array of entries, and the signer of the CTL
//  are returned. If the caller is not interested, ppCtl and ppSigner can be set
//  to NULL. Returned contexts must be freed via the store's free context APIs.
//
//  If the CERT_VERIFY_INHIBIT_CTL_UPDATE_FLAG isn't set, then, a time
//  invalid CTL in one of the CtlStores may be replaced. When replaced, the
//  CERT_VERIFY_UPDATED_CTL_FLAG is set in pVerifyUsageStatus->dwFlags.
//
//  If the CERT_VERIFY_TRUSTED_SIGNERS_FLAG is set, then, only the
//  SignerStores specified in pVerifyUsageStatus are searched to find
//  the signer. Otherwise, the SignerStores provide additional sources
//  to find the signer's certificate.
//
//  If CERT_VERIFY_NO_TIME_CHECK_FLAG is set, then, the CTLs aren't checked
//  for time validity.
//
//  If CERT_VERIFY_ALLOW_MORE_USAGE_FLAG is set, then, the CTL may contain
//  additional usage identifiers than specified by pSubjectUsage. Otherwise,
//  the found CTL will contain the same usage identifers and no more.
//
//  CertVerifyCTLUsage will be implemented as a dispatcher to OID installable
//  functions. First, it will try to find an OID function matching the first
//  usage object identifier in the pUsage sequence. Next, it will dispatch
//  to the default CertDllVerifyCTLUsage functions.
//
//  If the subject is trusted for the specified usage, then, TRUE is
//  returned. Otherwise, FALSE is returned with dwError set to one of the
//  following:
//      CRYPT_E_NO_VERIFY_USAGE_DLL
//      CRYPT_E_NO_VERIFY_USAGE_CHECK
//      CRYPT_E_VERIFY_USAGE_OFFLINE
//      CRYPT_E_NOT_IN_CTL
//      CRYPT_E_NO_TRUSTED_SIGNER
//--------------------------------------------------------------------------
function CertVerifyCTLUsage(dwEncodingType: DWORD;
  dwSubjectType: DWORD;
  pvSubject: PVOID;
  pSubjectUsage: PCTL_USAGE;
  dwFlags: DWORD;
  pVerifyUsagePara: PCTL_VERIFY_USAGE_PARA;
  pVerifyUsageStatus: PCTL_VERIFY_USAGE_STATUS
  ): BOOL; stdcall;

//+=========================================================================
//  Certificate Revocation Data Structures and APIs
//==========================================================================

//+-------------------------------------------------------------------------
//  The following data structure may be passed to CertVerifyRevocation to
//  assist in finding the issuer of the context to be verified.
//
//  When pIssuerCert is specified, pIssuerCert is the issuer of
//  rgpvContext[cContext - 1].
//
//  When cCertStore and rgCertStore are specified, these stores may contain
//  an issuer certificate.
//--------------------------------------------------------------------------
type
  PCERT_REVOCATION_PARA = ^CERT_REVOCATION_PARA;
  CERT_REVOCATION_PARA = record
    cbSize: DWORD;
    pIssuerCert: PCCERT_CONTEXT;
    cCertStore: DWORD;
    rgCertStore: PHCERTSTORE;
  end;

//+-------------------------------------------------------------------------
//  The following data structure is returned by CertVerifyRevocation to
//  specify the status of the revoked or unchecked context. Review the
//  following CertVerifyRevocation comments for details.
//
//  Upon input to CertVerifyRevocation, cbSize must be set to a size
//  >= sizeof(CERT_REVOCATION_STATUS). Otherwise, CertVerifyRevocation
//  returns FALSE and sets LastError to E_INVALIDARG.
//
//  Upon input to the installed or registered CRYPT_OID_VERIFY_REVOCATION_FUNC
//  functions, the dwIndex, dwError and dwReason have been zero'ed.
//--------------------------------------------------------------------------
type
  PCERT_REVOCATION_STATUS = ^CERT_REVOCATION_STATUS;
  CERT_REVOCATION_STATUS = record
    cbSize: DWORD;
    dwIndex: DWORD;
    dwError: DWORD;
    dwReason: DWORD;
  end;
//+-------------------------------------------------------------------------
//  Verifies the array of contexts for revocation. The dwRevType parameter
//  indicates the type of the context data structure passed in rgpvContext.
//  Currently only the revocation of certificates is defined.
//
//  If the CERT_VERIFY_REV_CHAIN_FLAG flag is set, then, CertVerifyRevocation
//  is verifying a chain of certs where, rgpvContext[i + 1] is the issuer
//  of rgpvContext[i]. Otherwise, CertVerifyRevocation makes no assumptions
//  about the order of the contexts.
//
//  To assist in finding the issuer, the pRevPara may optionally be set. See
//  the CERT_REVOCATION_PARA data structure for details.
//
//  The contexts must contain enough information to allow the
//  installable or registered revocation DLLs to find the revocation server. For
//  certificates, this information would normally be conveyed in an
//  extension such as the IETF's AuthorityInfoAccess extension.
//
//  CertVerifyRevocation returns TRUE if all of the contexts were successfully
//  checked and none were revoked. Otherwise, returns FALSE and updates the
//  returned pRevStatus data structure as follows:
//    dwIndex
//      Index of the first context that was revoked or unable to
//      be checked for revocation
//    dwError
//      Error status. LastError is also set to this error status.
//      dwError can be set to one of the following error codes defined
//      in winerror.h:
//        ERROR_SUCCESS - good context
//        CRYPT_E_REVOKED - context was revoked. dwReason contains the
//           reason for revocation
//        CRYPT_E_REVOCATION_OFFLINE - unable to connect to the
//           revocation server
//        CRYPT_E_NOT_IN_REVOCATION_DATABASE - the context to be checked
//           was not found in the revocation server's database.
//        CRYPT_E_NO_REVOCATION_CHECK - the called revocation function
//           wasn't able to do a revocation check on the context
//        CRYPT_E_NO_REVOCATION_DLL - no installed or registered Dll was
//           found to verify revocation
//    dwReason
//      The dwReason is currently only set for CRYPT_E_REVOKED and contains
//      the reason why the context was revoked. May be one of the following
//      CRL reasons defined by the CRL Reason Code extension ("2.5.29.21")
//          CRL_REASON_UNSPECIFIED              0
//          CRL_REASON_KEY_COMPROMISE           1
//          CRL_REASON_CA_COMPROMISE            2
//          CRL_REASON_AFFILIATION_CHANGED      3
//          CRL_REASON_SUPERSEDED               4
//          CRL_REASON_CESSATION_OF_OPERATION   5
//          CRL_REASON_CERTIFICATE_HOLD         6
//
//  For each entry in rgpvContext, CertVerifyRevocation iterates
//  through the CRYPT_OID_VERIFY_REVOCATION_FUNC
//  function set's list of installed DEFAULT functions.
//  CryptGetDefaultOIDFunctionAddress is called with pwszDll = NULL. If no
//  installed functions are found capable of doing the revocation verification,
//  CryptVerifyRevocation iterates through CRYPT_OID_VERIFY_REVOCATION_FUNC's
//  list of registered DEFAULT Dlls. CryptGetDefaultOIDDllList is called to
//  get the list. CryptGetDefaultOIDFunctionAddress is called to load the Dll.
//
//  The called functions have the same signature as CertVerifyRevocation. A
//  called function returns TRUE if it was able to successfully check all of
//  the contexts and none were revoked. Otherwise, the called function returns
//  FALSE and updates pRevStatus. dwIndex is set to the index of
//  the first context that was found to be revoked or unable to be checked.
//  dwError and LastError are updated. For CRYPT_E_REVOKED, dwReason
//  is updated. Upon input to the called function, dwIndex, dwError and
//  dwReason have been zero'ed. cbSize has been checked to be >=
//  sizeof(CERT_REVOCATION_STATUS).
//
//  If the called function returns FALSE, and dwError isn't set to
//  CRYPT_E_REVOKED, then, CertVerifyRevocation either continues on to the
//  next DLL in the list for a returned dwIndex of 0 or for a returned
//  dwIndex > 0, restarts the process of finding a verify function by
//  advancing the start of the context array to the returned dwIndex and
//  decrementing the count of remaining contexts.
//--------------------------------------------------------------------------
function CertVerifyRevocation(dwEncodingType: DWORD;
  dwRevType: DWORD;
  cContext: DWORD;
  rgpvContext: array of PVOID;
  dwFlags: DWORD;
  pRevPara: PCERT_REVOCATION_PARA;
  pRevStatus: PCERT_REVOCATION_STATUS
  ): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Revocation types
//--------------------------------------------------------------------------
const CERT_CONTEXT_REVOCATION_TYPE = 1;

//+-------------------------------------------------------------------------
//  When the following flag is set, rgpvContext[] consists of a chain
//  of certificates, where rgpvContext[i + 1] is the issuer of rgpvContext[i].
//--------------------------------------------------------------------------
const CERT_VERIFY_REV_CHAIN_FLAG = $1;

//+-------------------------------------------------------------------------
//  CERT_CONTEXT_REVOCATION_TYPE
//
//  pvContext points to a const CERT_CONTEXT.
//--------------------------------------------------------------------------

//+=========================================================================
//  Certificate Helper APIs
//==========================================================================


//+-------------------------------------------------------------------------
//  Compare two multiple byte integer blobs to see if they are identical.
//
//  Before doing the comparison, leading zero bytes are removed from a
//  positive number and leading 0xFF bytes are removed from a negative
//  number.
//
//  The multiple byte integers are treated as Little Endian. pbData[0] is the
//  least significant byte and pbData[cbData - 1] is the most significant
//  byte.
//
//  Returns TRUE if the integer blobs are identical after removing leading
//  0 or 0xFF bytes.
//--------------------------------------------------------------------------
function CertCompareIntegerBlob(pInt1: PCRYPT_INTEGER_BLOB;
  pInt2: PCRYPT_INTEGER_BLOB
  ): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Compare two certificates to see if they are identical.
//
//  Since a certificate is uniquely identified by its Issuer and SerialNumber,
//  these are the only fields needing to be compared.
//
//  Returns TRUE if the certificates are identical.
//--------------------------------------------------------------------------
function CertCompareCertificate(dwCertEncodingType: DWORD;
  pCertId1: PCERT_INFO;
  pCertId2: PCERT_INFO): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Compare two certificate names to see if they are identical.
//
//  Returns TRUE if the names are identical.
//--------------------------------------------------------------------------
function CertCompareCertificateName(dwCertEncodingType: DWORD;
  pCertName1: PCERT_NAME_BLOB;
  pCertName2: PCERT_NAME_BLOB): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Compare the attributes in the certificate name with the specified
//  Relative Distinguished Name's (CERT_RDN) array of attributes.
//  The comparison iterates through the CERT_RDN attributes and looks for an
//  attribute match in any of the certificate name's RDNs.
//  Returns TRUE if all the attributes are found and match.
//
//  The CERT_RDN_ATTR fields can have the following special values:
//    pszObjId == NULL              - ignore the attribute object identifier
//    dwValueType == RDN_ANY_TYPE   - ignore the value type
//
//  Currently only an exact, case sensitive match is supported.
//
//  CERT_UNICODE_IS_RDN_ATTRS_FLAG should be set if the pRDN was initialized
//  with unicode strings as for CryptEncodeObject(X509_UNICODE_NAME).
//--------------------------------------------------------------------------
function CertIsRDNAttrsInCertificateName(dwCertEncodingType: DWORD;
  dwFlags: DWORD;
  pCertName: PCERT_NAME_BLOB;
  pRDN: PCERT_RDN): BOOL; stdcall;

const CERT_UNICODE_IS_RDN_ATTRS_FLAG = $1;

//+-------------------------------------------------------------------------
//  Compare two public keys to see if they are identical.
//
//  Returns TRUE if the keys are identical.
//--------------------------------------------------------------------------
function CertComparePublicKeyInfo(dwCertEncodingType: DWORD;
  pPublicKey1: PCERT_PUBLIC_KEY_INFO;
  pPublicKey2: PCERT_PUBLIC_KEY_INFO
  ): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Get the public/private key's bit length.
//
//  Returns 0 if unable to determine the key's length.
//--------------------------------------------------------------------------
function CertGetPublicKeyLength(dwCertEncodingType: DWORD;
  pPublicKey: PCERT_PUBLIC_KEY_INFO
  ): DWORD; stdcall;
//+-------------------------------------------------------------------------
//  Verify the signature of a subject certificate or a CRL using the
//  public key info
//
//  Returns TRUE for a valid signature.
//
//  hCryptProv specifies the crypto provider to use to verify the signature.
//  It doesn't need to use a private key.
//--------------------------------------------------------------------------
function CryptVerifyCertificateSignature(hCryptProv: HCRYPTPROV;
  dwCertEncodingType: DWORD;
  const pbEncoded: PBYTE;
  cbEncoded: DWORD;
  pPublicKey: PCERT_PUBLIC_KEY_INFO
  ): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Compute the hash of the "to be signed" information in the encoded
//  signed content (CERT_SIGNED_CONTENT_INFO).
//
//  hCryptProv specifies the crypto provider to use to compute the hash.
//  It doesn't need to use a private key.
//--------------------------------------------------------------------------
function CryptHashToBeSigned(hCryptProv: HCRYPTPROV;
  dwCertEncodingType: DWORD;
  const pbEncoded: PBYTE;
  cbEncoded: DWORD;
  pbComputedHash: PBYTE;
  pcbComputedHash: PDWORD): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Hash the encoded content.
//
//  hCryptProv specifies the crypto provider to use to compute the hash.
//  It doesn't need to use a private key.
//
//  Algid specifies the CAPI hash algorithm to use. If Algid is 0, then, the
//  default hash algorithm (currently SHA1) is used.
//--------------------------------------------------------------------------
function CryptHashCertificate(hCryptProv: HCRYPTPROV;
  Algid: ALG_ID;
  dwFlags: DWORD;
  const pbEncoded: PBYTE;
  cbEncoded: DWORD;
  pbComputedHash: PBYTE;
  pcbComputedHash: PDWORD): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Sign the "to be signed" information in the encoded signed content.
//
//  hCryptProv specifies the crypto provider to use to do the signature.
//  It uses the specified private key.
//--------------------------------------------------------------------------
function CryptSignCertificate(hCryptProv: HCRYPTPROV;
  dwKeySpec: DWORD;
  dwCertEncodingType: DWORD;
  const pbEncodedToBeSigned: PBYTE;
  cbEncodedToBeSigned: DWORD;
  pSignatureAlgorithm: PCRYPT_ALGORITHM_IDENTIFIER;
  const pvHashAuxInfo: PVOID;
  pbSignature: PBYTE;
  pcbSignature: PDWORD): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Encode the "to be signed" information. Sign the encoded "to be signed".
//  Encode the "to be signed" and the signature.
//
//  hCryptProv specifies the crypto provider to use to do the signature.
//  It uses the specified private key.
//--------------------------------------------------------------------------
function CryptSignAndEncodeCertificate(hCryptProv: HCRYPTPROV;
  dwKeySpec: DWORD;
  dwCertEncodingType: DWORD;
  const lpszStructType: LPCSTR; // "to be signed"
  pvStructInfo: PVOID;
  pSignatureAlgorithm: PCRYPT_ALGORITHM_IDENTIFIER;
  const pvHashAuxInfo: PVOID;
  pbEncoded: PBYTE;
  pcbEncoded: PDWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Verify the time validity of a certificate.
//
//  Returns -1 if before NotBefore, +1 if after NotAfter and otherwise 0 for
//  a valid certificate
//
//  If pTimeToVerify is NULL, uses the current time.
//--------------------------------------------------------------------------
function CertVerifyTimeValidity(pTimeToVerify: PFILETIME;
  pCertInfo: PCERT_INFO): LONG; stdcall;

//+-------------------------------------------------------------------------
//  Verify the time validity of a CRL.
//
//  Returns -1 if before ThisUpdate, +1 if after NextUpdate and otherwise 0 for
//  a valid CRL
//
//  If pTimeToVerify is NULL, uses the current time.
//--------------------------------------------------------------------------
function CertVerifyCRLTimeValidity(pTimeToVerify: PFILETIME;
  pCrlInfo: PCRL_INFO): LONG; stdcall;
//+-------------------------------------------------------------------------
//  Verify that the subject's time validity nests within the issuer's time
//  validity.
//
//  Returns TRUE if it nests. Otherwise, returns FALSE.
//--------------------------------------------------------------------------
function CertVerifyValidityNesting(pSubjectInfo: PCERT_INFO;
  pIssuerInfo: PCERT_INFO): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Verify that the subject certificate isn't on its issuer CRL.
//
//  Returns true if the certificate isn't on the CRL.
//--------------------------------------------------------------------------
function CertVerifyCRLRevocation(dwCertEncodingType: DWORD;
  pCertId: PCERT_INFO; // Only the Issuer and SerialNumber
  cCrlInfo: DWORD; // fields are used
  rgpCrlInfo: array of PCRL_INFO): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Convert the CAPI AlgId to the ASN.1 Object Identifier string
//
//  Returns NULL if there isn't an ObjId corresponding to the AlgId.
//--------------------------------------------------------------------------
function CertAlgIdToOID(dwAlgId: DWORD): LPCSTR; stdcall;
//+-------------------------------------------------------------------------
//  Convert the ASN.1 Object Identifier string to the CAPI AlgId.
//
//  Returns 0 if there isn't an AlgId corresponding to the ObjId.
//--------------------------------------------------------------------------
function CertOIDToAlgId(pszObjId: LPCSTR): DWORD; stdcall;
//+-------------------------------------------------------------------------
//  Find an extension identified by its Object Identifier.
//
//  If found, returns pointer to the extension. Otherwise, returns NULL.
//--------------------------------------------------------------------------
function CertFindExtension(pszObjId: LPCSTR;
  cExtensions: DWORD;
  rgExtensions: array of CERT_EXTENSION): PCERT_EXTENSION; stdcall;
//+-------------------------------------------------------------------------
//  Find the first attribute identified by its Object Identifier.
//
//  If found, returns pointer to the attribute. Otherwise, returns NULL.
//--------------------------------------------------------------------------
function CertFindAttribute(pszObjId: LPCSTR;
  cAttr: DWORD;
  rgAttr: array of CRYPT_ATTRIBUTE): PCRYPT_ATTRIBUTE; stdcall;
//+-------------------------------------------------------------------------
//  Find the first CERT_RDN attribute identified by its Object Identifier in
//  the name's list of Relative Distinguished Names.
//
//  If found, returns pointer to the attribute. Otherwise, returns NULL.
//--------------------------------------------------------------------------
function CertFindRDNAttr(pszObjId: LPCSTR;
  pName: PCERT_NAME_INFO): PCERT_RDN_ATTR; stdcall;
//+-------------------------------------------------------------------------
//  Get the intended key usage bytes from the certificate.
//
//  If the certificate doesn't have any intended key usage bytes, returns FALSE
//  and *pbKeyUsage is zeroed. Otherwise, returns TRUE and up through
//  cbKeyUsage bytes are copied into *pbKeyUsage. Any remaining uncopied
//  bytes are zeroed.
//--------------------------------------------------------------------------
function CertGetIntendedKeyUsage(dwCertEncodingType: DWORD;
  pCertInfo: PCERT_INFO;
  pbKeyUsage: PBYTE;
  cbKeyUsage: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Export the public key info associated with the provider's corresponding
//  private key.
//
//  Calls CryptExportPublicKeyInfo with pszPublicKeyObjId = szOID_RSA_RSA,
//  dwFlags = 0 and pvAuxInfo = NULL.
//--------------------------------------------------------------------------
function CryptExportPublicKeyInfo(hCryptProv: HCRYPTPROV;
  dwKeySpec: DWORD;
  dwCertEncodingType: DWORD;
  pInfo: PCERT_PUBLIC_KEY_INFO;
  pcbInfo: PDWORD): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Export the public key info associated with the provider's corresponding
//  private key.
//
//  Uses the dwCertEncodingType and pszPublicKeyObjId to call the
//  installable CRYPT_OID_EXPORT_PUBLIC_KEY_INFO_FUNC. The called function
//  has the same signature as CryptExportPublicKeyInfoEx.
//
//  If unable to find an installable OID function for the pszPublicKeyObjId,
//  attempts to export as a RSA Public Key (szOID_RSA_RSA).
//
//  The dwFlags and pvAuxInfo aren't used for szOID_RSA_RSA.
//--------------------------------------------------------------------------
const CRYPT_OID_EXPORT_PUBLIC_KEY_INFO_FUNC = 'CryptDllExportPublicKeyInfoEx';

function CryptExportPublicKeyInfoEx(hCryptProv: HCRYPTPROV;
  dwKeySpec: DWORD;
  dwCertEncodingType: DWORD;
  pszPublicKeyObjId: LPSTR;
  dwFlags: DWORD;
  pvAuxInfo: PVOID;
  pInfo: PCERT_PUBLIC_KEY_INFO;
  pcbInfo: PDWORD): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Convert and import the public key info into the provider and return a
//  handle to the public key.
//
//  Calls CryptImportPublicKeyInfoEx with aiKeyAlg = 0, dwFlags = 0 and
//  pvAuxInfo = NULL.
//--------------------------------------------------------------------------
function CryptImportPublicKeyInfo(hCryptProv: HCRYPTPROV;
  dwCertEncodingType: DWORD;
  pInfo: PCERT_PUBLIC_KEY_INFO;
  phKey: PHCRYPTKEY): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Convert and import the public key info into the provider and return a
//  handle to the public key.
//
//  Uses the dwCertEncodingType and pInfo->Algorithm.pszObjId to call the
//  installable CRYPT_OID_IMPORT_PUBLIC_KEY_INFO_FUNC. The called function
//  has the same signature as CryptImportPublicKeyInfoEx.
//
//  If unable to find an installable OID function for the pszObjId,
//  attempts to import as a RSA Public Key (szOID_RSA_RSA).
//
//  For szOID_RSA_RSA: aiKeyAlg may be set to CALG_RSA_SIGN or CALG_RSA_KEYX.
//  Defaults to CALG_RSA_KEYX. The dwFlags and pvAuxInfo aren't used.
//--------------------------------------------------------------------------
const CRYPT_OID_IMPORT_PUBLIC_KEY_INFO_FUNC = 'CryptDllImportPublicKeyInfoEx';

function CryptImportPublicKeyInfoEx(hCryptProv: HCRYPTPROV;
  dwCertEncodingType: DWORD;
  pInfo: PCERT_PUBLIC_KEY_INFO;
  aiKeyAlg: ALG_ID;
  dwFlags: DWORD;
  pvAuxInfo: PVOID;
  phKey: PHCRYPTKEY
  ): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Compute the hash of the encoded public key info.
//
//  The public key info is encoded and then hashed.
//--------------------------------------------------------------------------
function CryptHashPublicKeyInfo(hCryptProv: HCRYPTPROV;
  Algid: ALG_ID;
  dwFlags: DWORD;
  dwCertEncodingType: DWORD;
  pInfo: PCERT_PUBLIC_KEY_INFO;
  pbComputedHash: PBYTE;
  pcbComputedHash: PDWORD): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Convert a Name Value to a null terminated char string
//
//  Returns the number of characters converted including the terminating null
//  character. If psz is NULL or csz is 0, returns the required size of the
//  destination string (including the terminating null char).
//
//  If psz != NULL && csz != 0, returned psz is always NULL terminated.
//
//  Note: csz includes the NULL char.
//--------------------------------------------------------------------------
function CertRDNValueToStrA(dwValueType: DWORD;
  pValue: PCERT_RDN_VALUE_BLOB;
  psz: LPSTR; //OPTIONAL
  csz: DWORD): DWORD; stdcall;
//+-------------------------------------------------------------------------
//  Convert a Name Value to a null terminated char string
//
//  Returns the number of characters converted including the terminating null
//  character. If psz is NULL or csz is 0, returns the required size of the
//  destination string (including the terminating null char).
//
//  If psz != NULL && csz != 0, returned psz is always NULL terminated.
//
//  Note: csz includes the NULL char.
//--------------------------------------------------------------------------
function CertRDNValueToStrW(dwValueType: DWORD;
  pValue: PCERT_RDN_VALUE_BLOB;
  psz: LPWSTR; //OPTIONAL
  csz: DWORD): DWORD; stdcall;

function CertRDNValueToStr(dwValueType: DWORD;
  pValue: PCERT_RDN_VALUE_BLOB;
  psz: LPAWSTR; //OPTIONAL
  csz: DWORD): DWORD; stdcall;

//+-------------------------------------------------------------------------
//  Convert the certificate name blob to a null terminated char string.
//
//  Follows the string representation of distinguished names specified in
//  RFC 1779. (Note, added double quoting "" for embedded quotes, quote
//  empty strings and don't quote strings containing consecutive spaces).
//  RDN values of type CERT_RDN_ENCODED_BLOB or CERT_RDN_OCTET_STRING are
//  formatted in hexadecimal (e.g. #0A56CF).
//
//  The name string is formatted according to the dwStrType:
//    CERT_SIMPLE_NAME_STR
//      The object identifiers are discarded. CERT_RDN entries are separated
//      by ", ". Multiple attributes per CERT_RDN are separated by " + ".
//      For example:
//          Microsoft, Joe Cool + Programmer
//    CERT_OID_NAME_STR
//      The object identifiers are included with a "=" separator from their
//      attribute value. CERT_RDN entries are separated by ", ".
//      Multiple attributes per CERT_RDN are separated by " + ". For example:
//          2.5.4.11=Microsoft, 2.5.4.3=Joe Cool + 2.5.4.12=Programmer
//    CERT_X500_NAME_STR
//      The object identifiers are converted to their X500 key name. Otherwise,
//      same as CERT_OID_NAME_STR. If the object identifier doesn't have
//      a corresponding X500 key name, then, the object identifier is used with
//      a "OID." prefix. For example:
//          OU=Microsoft, CN=Joe Cool + T=Programmer, OID.1.2.3.4.5.6=Unknown
//
//  We quote the RDN value if it contains leading or trailing whitespace
//  or one of the following characters: ",", "+", "=", """, "\n",  "<", ">",
//  "#" or ";". The quoting character is ". If the the RDN Value contains
//  a " it is double quoted (""). For example:
//      OU="  Microsoft", CN="Joe ""Cool""" + T="Programmer, Manager"
//
//  CERT_NAME_STR_SEMICOLON_FLAG can be or'ed into dwStrType to replace
//  the ", " separator with a "; " separator.
//
//  CERT_NAME_STR_CRLF_FLAG can be or'ed into dwStrType to replace
//  the ", " separator with a "\r\n" separator.
//
//  CERT_NAME_STR_NO_PLUS_FLAG can be or'ed into dwStrType to replace the
//  " + " separator with a single space, " ".
//
//  CERT_NAME_STR_NO_QUOTING_FLAG can be or'ed into dwStrType to inhibit
//  the above quoting.
//
//  Returns the number of characters converted including the terminating null
//  character. If psz is NULL or csz is 0, returns the required size of the
//  destination string (including the terminating null char).
//
//  If psz != NULL && csz != 0, returned psz is always NULL terminated.
//
//  Note: csz includes the NULL char.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//--------------------------------------------------------------------------
function CertNameToStrA(dwCertEncodingType: DWORD;
  pName: PCERT_NAME_BLOB;
  dwStrType: DWORD;
  psz: LPSTR; //OPTIONAL
  csz: DWORD): DWORD; stdcall;
//+-------------------------------------------------------------------------
//--------------------------------------------------------------------------
function CertNameToStrW(dwCertEncodingType: DWORD;
  pName: PCERT_NAME_BLOB;
  dwStrType: DWORD;
  psz: LPWSTR; //OPTIONAL
  csz: DWORD): DWORD; stdcall;

function CertNameToStr(dwCertEncodingType: DWORD;
  pName: PCERT_NAME_BLOB;
  dwStrType: DWORD;
  psz: LPAWSTR; //OPTIONAL
  csz: DWORD): DWORD; stdcall;

//+-------------------------------------------------------------------------
//  Certificate name string types
//--------------------------------------------------------------------------
const CERT_SIMPLE_NAME_STR = 1;
const CERT_OID_NAME_STR = 2;
const CERT_X500_NAME_STR = 3;

//+-------------------------------------------------------------------------
//  Certificate name string type flags OR'ed with the above types
//--------------------------------------------------------------------------
const CERT_NAME_STR_SEMICOLON_FLAG = $40000000;
const CERT_NAME_STR_NO_PLUS_FLAG = $20000000;
const CERT_NAME_STR_NO_QUOTING_FLAG = $10000000;
const CERT_NAME_STR_CRLF_FLAG = $08000000;
const CERT_NAME_STR_COMMA_FLAG = $04000000;


//+-------------------------------------------------------------------------
//  Convert the null terminated X500 string to an encoded certificate name.
//
//  The input string is expected to be formatted the same as the output
//  from the above CertNameToStr API.
//
//  The CERT_SIMPLE_NAME_STR type isn't supported. Otherwise, when dwStrType
//  is set to 0, CERT_OID_NAME_STR or CERT_X500_NAME_STR, allow either a
//  case insensitive X500 key (CN=), case insensitive "OID." prefixed
//  object identifier (OID.1.2.3.4.5.6=) or an object identifier (1.2.3.4=).
//
//  If no flags are OR'ed into dwStrType, then, allow "," or ";" as RDN
//  separators and "+" as the multiple RDN value separator. Quoting is
//  supported. A quote may be included in a quoted value by double quoting,
//  for example (CN="Joe ""Cool"""). A value starting with a "#" is treated
//  as ascii hex and converted to a CERT_RDN_OCTET_STRING. Embedded whitespace
//  is skipped (1.2.3 = # AB CD 01  is the same as 1.2.3=#ABCD01).
//
//  Whitespace surrounding the keys, object identifers and values is removed.
//
//  CERT_NAME_STR_COMMA_FLAG can be or'ed into dwStrType to only allow the
//  "," as the RDN separator.
//
//  CERT_NAME_STR_SEMICOLON_FLAG can be or'ed into dwStrType to only allow the
//  ";" as the RDN separator.
//
//  CERT_NAME_STR_CRLF_FLAG can be or'ed into dwStrType to only allow
//  "\r" or "\n" as the RDN separator.
//
//  CERT_NAME_STR_NO_PLUS_FLAG can be or'ed into dwStrType to ignore "+"
//  as a separator and not allow multiple values per RDN.
//
//  CERT_NAME_STR_NO_QUOTING_FLAG can be or'ed into dwStrType to inhibit
//  quoting.
//
//  Support the following X500 Keys:
//
//  Key         Object Identifier               RDN Value Type(s)
//  ---         -----------------               -----------------
//  CN          szOID_COMMON_NAME               Printable, T61
//  L           szOID_LOCALITY_NAME             Printable, T61
//  O           szOID_ORGANIZATION_NAME         Printable, T61
//  OU          szOID_ORGANIZATIONAL_UNIT_NAME  Printable, T61
//  Email       szOID_RSA_emailAddr             Only IA5
//  C           szOID_COUNTRY_NAME              Only Printable
//  S           szOID_STATE_OR_PROVINCE_NAME    Printable, T61
//  ST          szOID_STATE_OR_PROVINCE_NAME    Printable, T61
//  STREET      szOID_STREET_ADDRESS            Printable, T61
//  T           szOID_TITLE                     Printable, T61
//  Title       szOID_TITLE                     Printable, T61
//  G           szOID_GIVEN_NAME                Printable, T61
//  GivenName   szOID_GIVEN_NAME                Printable, T61
//  I           szOID_INITIALS                  Printable, T61
//  Initials    szOID_INITIALS                  Printable, T61
//  SN          szOID_SUR_NAME                  Printable, T61
//  DC          szOID_DOMAIN_COMPONENT          Only IA5
//
//  The T61 types are UTF-8 encoded.
//
//  Returns TRUE if successfully parsed the input string and encoded
//  the name.
//
//  If the input string is detected to be invalid, *ppszError is updated
//  to point to the beginning of the invalid character sequence. Otherwise,
//  *ppszError is set to NULL. *ppszError is updated with a non-NULL pointer
//  for the following errors:
//      CRYPT_E_INVALID_X500_STRING
//      CRYPT_E_INVALID_NUMERIC_STRING
//      CRYPT_E_INVALID_PRINTABLE_STRING
//      CRYPT_E_INVALID_IA5_STRING
//
//  ppszError can be set to NULL if not interested in getting a pointer
//  to the invalid character sequence.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//--------------------------------------------------------------------------
function CertStrToNameA(dwCertEncodingType: DWORD;
  pszX500: LPCSTR;
  dwStrType: DWORD;
  pvReserved: PVOID;
  pbEncoded: PBYTE;
  pcbEncoded: PDWORD;
  var ppszError: array of LPCSTR): BOOL; stdcall; {--max-- iniziato qui}
//+-------------------------------------------------------------------------
//--------------------------------------------------------------------------
function CertStrToNameW(dwCertEncodingType: DWORD;
  pszX500: LPCWSTR;
  dwStrType: DWORD;
  pvReserved: PVOID;
  pbEncoded: PBYTE;
  pcbEncoded: PDWORD;
  var ppszError: array of LPWSTR): BOOL; stdcall;

function CertStrToName(dwCertEncodingType: DWORD;
  pszX500: LPAWSTR;
  dwStrType: DWORD;
  pvReserved: PVOID;
  pbEncoded: PBYTE;
  pcbEncoded: PDWORD;
  var ppszError: array of LPAWSTR): BOOL; stdcall;

//+=========================================================================
//  Simplified Cryptographic Message Data Structures and APIs
//==========================================================================


//+-------------------------------------------------------------------------
//              Conventions for the *pb and *pcb output parameters:
//
//              Upon entry to the function:
//                  if pcb is OPTIONAL && pcb == NULL, then,
//                      No output is returned
//                  else if pb == NULL && pcb != NULL, then,
//                      Length only determination. No length error is
//                      returned.
//                  otherwise where (pb != NULL && pcb != NULL && *pcb != 0)
//                      Output is returned. If *pcb isn't big enough a
//                      length error is returned. In all cases *pcb is updated
//                      with the actual length needed/returned.
//--------------------------------------------------------------------------


//+-------------------------------------------------------------------------
//  Type definitions of the parameters used for doing the cryptographic
//  operations.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  Callback to get and verify the signer's certificate.
//
//  Passed the CertId of the signer (its Issuer and SerialNumber) and a
//  handle to its cryptographic signed message's cert store.
//
//  For CRYPT_E_NO_SIGNER, called with pSignerId == NULL.
//
//  For a valid signer certificate, returns a pointer to a read only
//  CERT_CONTEXT. The returned CERT_CONTEXT is either obtained from a
//  cert store or was created via CertCreateCertificateContext. For either case,
//  its freed via CertFreeCertificateContext.
//
//  If a valid certificate isn't found, this callback returns NULL with
//  LastError set via SetLastError().
//
//  The NULL implementation tries to get the Signer certificate from the
//  message cert store. It doesn't verify the certificate.
//--------------------------------------------------------------------------

type
  PFN_CRYPT_GET_SIGNER_CERTIFICATE = function(pvGetArg: PVOID;
    dwCertEncodingType: DWORD;
    pSignerId: PCERT_INFO; // Only the Issuer and SerialNumber
    hMsgCertStore: HCERTSTORE // fields have been updated
    ): PCCERT_CONTEXT; stdcall;
//+-------------------------------------------------------------------------
//  The CRYPT_SIGN_MESSAGE_PARA are used for signing messages using the
//  specified signing certificate contexts. (Note, allows multiple signers.)
//
//  Either the CERT_KEY_PROV_HANDLE_PROP_ID or CERT_KEY_PROV_INFO_PROP_ID must
//  be set for each rgpSigningCert[]. Either one specifies the private
//  signature key to use.
//
//  If any certificates and/or CRLs are to be included in the signed message,
//  then, the MsgCert and MsgCrl parameters need to be updated. If the
//  rgpSigningCerts are to be included, then, they must also be in the
//  rgpMsgCert array.
//
//  cbSize must be set to the sizeof(CRYPT_SIGN_MESSAGE_PARA) or else
//  LastError will be updated with E_INVALIDARG.
//
//  pvHashAuxInfo currently isn't used and must be set to NULL.
//
//  dwFlags normally is set to 0. However, if the encoded output
//  is to be a CMSG_SIGNED inner content of an outer cryptographic message,
//  such as a CMSG_ENVELOPED, then, the CRYPT_MESSAGE_BARE_CONTENT_OUT_FLAG
//  should be set. If not set, then it would be encoded as an inner content
//  type of CMSG_DATA.
//
//  dwInnerContentType is normally set to 0. It needs to be set if the
//  ToBeSigned input is the encoded output of another cryptographic
//  message, such as, an CMSG_ENVELOPED. When set, it's one of the cryptographic
//  message types, for example, CMSG_ENVELOPED.
//
//  If the inner content of a nested cryptographic message is data (CMSG_DATA
//  the default), then, neither dwFlags or dwInnerContentType need to be set.
//--------------------------------------------------------------------------

type
  PCRYPT_SIGN_MESSAGE_PARA = ^CRYPT_SIGN_MESSAGE_PARA;
  CRYPT_SIGN_MESSAGE_PARA = record
    cbSize: DWORD;
    dwMsgEncodingType: DWORD;
    pSigningCert: PCCERT_CONTEXT;
    HashAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pvHashAuxInfo: PVOID;
    cMsgCert: DWORD;
    rgpMsgCert: PPCCERT_CONTEXT; // pointer to array of PCCERT_CONTEXT
    cMsgCrl: DWORD;
    rgpMsgCrl: PPCCRL_CONTEXT; // pointer to array of PCCERT_CO
    cAuthAttr: DWORD;
    rgAuthAttr: PCRYPT_ATTRIBUTE;
    cUnauthAttr: DWORD;
    rgUnauthAttr: PCRYPT_ATTRIBUTE;
    dwFlags: DWORD;
    dwInnerContentType: DWORD;
  end;
const CRYPT_MESSAGE_BARE_CONTENT_OUT_FLAG = $1;

//+-------------------------------------------------------------------------
//  The CRYPT_VERIFY_MESSAGE_PARA are used to verify signed messages.
//
//  hCryptProv is used to do hashing and signature verification.
//
//  The dwCertEncodingType specifies the encoding type of the certificates
//  and/or CRLs in the message.
//
//  pfnGetSignerCertificate is called to get and verify the message signer's
//  certificate.
//
//  cbSize must be set to the sizeof(CRYPT_VERIFY_MESSAGE_PARA) or else
//  LastError will be updated with E_INVALIDARG.
//--------------------------------------------------------------------------
type
  PCRYPT_VERIFY_MESSAGE_PARA = ^CRYPT_VERIFY_MESSAGE_PARA;
  CRYPT_VERIFY_MESSAGE_PARA = record
    cbSize: DWORD;
    dwMsgAndCertEncodingType: DWORD;
    hCryptProv: HCRYPTPROV;
    pfnGetSignerCertificate: PFN_CRYPT_GET_SIGNER_CERTIFICATE;
    pvGetArg: PVOID;
  end;

//+-------------------------------------------------------------------------
//  The CRYPT_ENCRYPT_MESSAGE_PARA are used for encrypting messages.
//
//  hCryptProv is used to do content encryption, recipient key
//  encryption, and recipient key export. Its private key
//  isn't used.
//
//  pvEncryptionAuxInfo currently isn't used and must be set to NULL.
//
//  cbSize must be set to the sizeof(CRYPT_ENCRYPT_MESSAGE_PARA) or else
//  LastError will be updated with E_INVALIDARG.
//
//  dwFlags normally is set to 0. However, if the encoded output
//  is to be a CMSG_ENVELOPED inner content of an outer cryptographic message,
//  such as a CMSG_SIGNED, then, the CRYPT_MESSAGE_BARE_CONTENT_OUT_FLAG
//  should be set. If not set, then it would be encoded as an inner content
//  type of CMSG_DATA.
//
//  dwInnerContentType is normally set to 0. It needs to be set if the
//  ToBeEncrypted input is the encoded output of another cryptographic
//  message, such as, an CMSG_SIGNED. When set, it's one of the cryptographic
//  message types, for example, CMSG_SIGNED.
//
//  If the inner content of a nested cryptographic message is data (CMSG_DATA
//  the default), then, neither dwFlags or dwInnerContentType need to be set.
//--------------------------------------------------------------------------
type
  PCRYPT_ENCRYPT_MESSAGE_PARA = ^CRYPT_ENCRYPT_MESSAGE_PARA;
  CRYPT_ENCRYPT_MESSAGE_PARA = record
    cbSize: DWORD;
    dwMsgEncodingType: DWORD;
    hCryptProv: HCRYPTPROV;
    ContentEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pvEncryptionAuxInfo: PVOID;
    dwFlags: DWORD;
    dwInnerContentType: DWORD;
  end;

//+-------------------------------------------------------------------------
//  The CRYPT_DECRYPT_MESSAGE_PARA are used for decrypting messages.
//
//  The CertContext to use for decrypting a message is obtained from one
//  of the specified cert stores. An encrypted message can have one or
//  more recipients. The recipients are identified by their CertId (Issuer
//  and SerialNumber). The cert stores are searched to find the CertContext
//  corresponding to the CertId.
//
//  Only CertContexts in the store with either
//  the CERT_KEY_PROV_HANDLE_PROP_ID or CERT_KEY_PROV_INFO_PROP_ID set
//  can be used. Either property specifies the private exchange key to use.
//
//  cbSize must be set to the sizeof(CRYPT_DECRYPT_MESSAGE_PARA) or else
//  LastError will be updated with E_INVALIDARG.
//--------------------------------------------------------------------------
type
  PCRYPT_DECRYPT_MESSAGE_PARA = ^CRYPT_DECRYPT_MESSAGE_PARA;
  CRYPT_DECRYPT_MESSAGE_PARA = record
    cbSize: DWORD;
    dwMsgAndCertEncodingType: DWORD;
    cCertStore: DWORD;
    rghCertStore: PHCERTSTORE;
  end;
//+-------------------------------------------------------------------------
//  The CRYPT_HASH_MESSAGE_PARA are used for hashing or unhashing
//  messages.
//
//  hCryptProv is used to compute the hash.
//
//  pvHashAuxInfo currently isn't used and must be set to NULL.
//
//  cbSize must be set to the sizeof(CRYPT_HASH_MESSAGE_PARA) or else
//  LastError will be updated with E_INVALIDARG.
//--------------------------------------------------------------------------
type
  PCRYPT_HASH_MESSAGE_PARA = ^CRYPT_HASH_MESSAGE_PARA;
  CRYPT_HASH_MESSAGE_PARA = record
    cbSize: DWORD;
    dwMsgEncodingType: DWORD;
    hCryptProv: HCRYPTPROV;
    HashAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pvHashAuxInfo: PVOID;
  end;

//+-------------------------------------------------------------------------
//  The CRYPT_KEY_SIGN_MESSAGE_PARA are used for signing messages until a
//  certificate has been created for the signature key.
//
//  pvHashAuxInfo currently isn't used and must be set to NULL.
//
//  If PubKeyAlgorithm isn't set, defaults to szOID_RSA_RSA.
//
//  cbSize must be set to the sizeof(CRYPT_KEY_SIGN_MESSAGE_PARA) or else
//  LastError will be updated with E_INVALIDARG.
//--------------------------------------------------------------------------
type
  PCRYPT_KEY_SIGN_MESSAGE_PARA = ^CRYPT_KEY_SIGN_MESSAGE_PARA;
  CRYPT_KEY_SIGN_MESSAGE_PARA = record
    cbSize: DWORD;
    dwMsgAndCertEncodingType: DWORD;
    hCryptProv: HCRYPTPROV;
    dwKeySpec: DWORD;
    HashAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pvHashAuxInfo: PVOID;
    PubKeyAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
  end;

//+-------------------------------------------------------------------------
//  The CRYPT_KEY_VERIFY_MESSAGE_PARA are used to verify signed messages without
//  a certificate for the signer.
//
//  Normally used until a certificate has been created for the key.
//
//  hCryptProv is used to do hashing and signature verification.
//
//  cbSize must be set to the sizeof(CRYPT_KEY_VERIFY_MESSAGE_PARA) or else
//  LastError will be updated with E_INVALIDARG.
//--------------------------------------------------------------------------
type
  PCRYPT_KEY_VERIFY_MESSAGE_PARA = ^CRYPT_KEY_VERIFY_MESSAGE_PARA;
  CRYPT_KEY_VERIFY_MESSAGE_PARA = record
    cbSize: DWORD;
    dwMsgEncodingType: DWORD;
    hCryptProv: HCRYPTPROV;
  end;

//+-------------------------------------------------------------------------
//  Sign the message.
//
//  If fDetachedSignature is TRUE, the "to be signed" content isn't included
//  in the encoded signed blob.
//--------------------------------------------------------------------------
function CryptSignMessage(pSignPara: PCRYPT_SIGN_MESSAGE_PARA;
  fDetachedSignature: BOOL;
  cToBeSigned: DWORD;
  const rgpbToBeSigned: array of PBYTE;
  rgcbToBeSigned: array of DWORD;
  pbSignedBlob: PBYTE;
  pcbSignedBlob: PDWORD): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Verify a signed message.
//
//  If pbDecoded == NULL, then, *pcbDecoded is implicitly set to 0 on input.
//  For *pcbDecoded == 0 && ppSignerCert == NULL on input, the signer isn't
//  verified.
//
//  A message might have more than one signer. Set dwSignerIndex to iterate
//  through all the signers. dwSignerIndex == 0 selects the first signer.
//
//  pVerifyPara's pfnGetSignerCertificate is called to get the signer's
//  certificate.
//
//  For a verified signer and message, *ppSignerCert is updated
//  with the CertContext of the signer. It must be freed by calling
//  CertFreeCertificateContext. Otherwise, *ppSignerCert is set to NULL.
//
//  ppSignerCert can be NULL, indicating the caller isn't interested
//  in getting the CertContext of the signer.
//
//  pcbDecoded can be NULL, indicating the caller isn't interested in getting
//  the decoded content. Furthermore, if the message doesn't contain any
//  content or signers, then, pcbDecoded must be set to NULL, to allow the
//  pVerifyPara->pfnGetCertificate to be called. Normally, this would be
//  the case when the signed message contains only certficates and CRLs.
//  If pcbDecoded is NULL and the message doesn't have the indicated signer,
//  pfnGetCertificate is called with pSignerId set to NULL.
//
//  If the message doesn't contain any signers || dwSignerIndex > message's
//  SignerCount, then, an error is returned with LastError set to
//  CRYPT_E_NO_SIGNER. Also, for CRYPT_E_NO_SIGNER, pfnGetSignerCertificate
//  is still called with pSignerId set to NULL.
//
//  Note, an alternative way to get the certificates and CRLs from a
//  signed message is to call CryptGetMessageCertificates.
//--------------------------------------------------------------------------
function CryptVerifyMessageSignature(pVerifyPara: PCRYPT_VERIFY_MESSAGE_PARA;
  dwSignerIndex: DWORD;
  const pbSignedBlob: PBYTE;
  cbSignedBlob: DWORD;
  pbDecoded: PBYTE;
  pcbDecoded: DWORD;
  ppSignerCert: PCCERT_CONTEXT
  ): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Returns the count of signers in the signed message. For no signers, returns
//  0. For an error returns -1 with LastError updated accordingly.
//--------------------------------------------------------------------------
function CryptGetMessageSignerCount(dwMsgEncodingType: DWORD;
  const pbSignedBlob: PBYTE;
  cbSignedBlob: DWORD): LONG; stdcall;
//+-------------------------------------------------------------------------
//  Returns the cert store containing the message's certs and CRLs.
//  For an error, returns NULL with LastError updated.
//--------------------------------------------------------------------------
function CryptGetMessageCertificates(dwMsgAndCertEncodingType: DWORD;
  hCryptProv: HCRYPTPROV; // passed to CertOpenStore
  dwFlags: DWORD; // passed to CertOpenStore
  const pbSignedBlob: PBYTE;
  cbSignedBlob: DWORD): HCERTSTORE; stdcall;
//+-------------------------------------------------------------------------
//  Verify a signed message containing detached signature(s).
//  The "to be signed" content is passed in separately. No
//  decoded output. Otherwise, identical to CryptVerifyMessageSignature.
//--------------------------------------------------------------------------
function CryptVerifyDetachedMessageSignature(pVerifyPara: PCRYPT_VERIFY_MESSAGE_PARA;
  dwSignerIndex: DWORD;
  const pbDetachedSignBlob: PBYTE;
  cbDetachedSignBlob: DWORD;
  cToBeSigned: DWORD;
  const rgpbToBeSigned: array of PBYTE;
  rgcbToBeSigned: array of DWORD;
  ppSignerCert: PPCCERT_CONTEXT): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Encrypts the message for the recipient(s).
//--------------------------------------------------------------------------
function CryptEncryptMessage(pEncryptPara: PCRYPT_ENCRYPT_MESSAGE_PARA;
  cRecipientCert: DWORD;
  rgpRecipientCert: array of PCCERT_CONTEXT;
  const pbToBeEncrypted: PBYTE;
  cbToBeEncrypted: DWORD;
  pbEncryptedBlob: PBYTE;
  pcbEncryptedBlob: PDWORD): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Decrypts the message.
//
//  If pbDecrypted == NULL, then, *pcbDecrypted is implicitly set to 0 on input.
//  For *pcbDecrypted == 0 && ppXchgCert == NULL on input, the message isn't
//  decrypted.
//
//  For a successfully decrypted message, *ppXchgCert is updated
//  with the CertContext used to decrypt. It must be freed by calling
//  CertStoreFreeCert. Otherwise, *ppXchgCert is set to NULL.
//
//  ppXchgCert can be NULL, indicating the caller isn't interested
//  in getting the CertContext used to decrypt.
//--------------------------------------------------------------------------
function CryptDecryptMessage(pDecryptPara: PCRYPT_DECRYPT_MESSAGE_PARA;
  const pbEncryptedBlob: PBYTE;
  cbEncryptedBlob: DWORD;
  pbDecrypted: PBYTE;
  pcbDecrypted: PDWORD;
  ppXchgCert: PPCCERT_CONTEXT): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Sign the message and encrypt for the recipient(s). Does a CryptSignMessage
//  followed with a CryptEncryptMessage.
//
//  Note: this isn't the CMSG_SIGNED_AND_ENVELOPED. Its a CMSG_SIGNED
//  inside of an CMSG_ENVELOPED.
//--------------------------------------------------------------------------
function CryptSignAndEncryptMessage(pSignPara: PCRYPT_SIGN_MESSAGE_PARA;
  pEncryptPara: PCRYPT_ENCRYPT_MESSAGE_PARA;
  cRecipientCert: DWORD;
  rgpRecipientCert: array of PCCERT_CONTEXT;
  const pbToBeSignedAndEncrypted: PBYTE;
  cbToBeSignedAndEncrypted: DWORD;
  pbSignedAndEncryptedBlob: PBYTE;
  pcbSignedAndEncryptedBlob: PDWORD
  ): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Decrypts the message and verifies the signer. Does a CryptDecryptMessage
//  followed with a CryptVerifyMessageSignature.
//
//  If pbDecrypted == NULL, then, *pcbDecrypted is implicitly set to 0 on input.
//  For *pcbDecrypted == 0 && ppSignerCert == NULL on input, the signer isn't
//  verified.
//
//  A message might have more than one signer. Set dwSignerIndex to iterate
//  through all the signers. dwSignerIndex == 0 selects the first signer.
//
//  The pVerifyPara's VerifySignerPolicy is called to verify the signer's
//  certificate.
//
//  For a successfully decrypted and verified message, *ppXchgCert and
//  *ppSignerCert are updated. They must be freed by calling
//  CertStoreFreeCert. Otherwise, they are set to NULL.
//
//  ppXchgCert and/or ppSignerCert can be NULL, indicating the
//  caller isn't interested in getting the CertContext.
//
//  Note: this isn't the CMSG_SIGNED_AND_ENVELOPED. Its a CMSG_SIGNED
//  inside of an CMSG_ENVELOPED.
//
//  The message always needs to be decrypted to allow access to the
//  signed message. Therefore, if ppXchgCert != NULL, its always updated.
//--------------------------------------------------------------------------
function CryptDecryptAndVerifyMessageSignature(pDecryptPara: PCRYPT_DECRYPT_MESSAGE_PARA;
  pVerifyPara: PCRYPT_VERIFY_MESSAGE_PARA;
  dwSignerIndex: DWORD;
  const pbEncryptedBlob: PBYTE;
  cbEncryptedBlob: DWORD;
  pbDecrypted: PBYTE;
  pcbDecrypted: PDWORD;
  var ppXchgCert: array of PCCERT_CONTEXT;
  var ppSignerCert: array of PCCERT_CONTEXT
  ): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Decodes a cryptographic message which may be one of the following types:
//    CMSG_DATA
//    CMSG_SIGNED
//    CMSG_ENVELOPED
//    CMSG_SIGNED_AND_ENVELOPED
//    CMSG_HASHED
//
//  dwMsgTypeFlags specifies the set of allowable messages. For example, to
//  decode either SIGNED or ENVELOPED messages, set dwMsgTypeFlags to:
//      CMSG_SIGNED_FLAG | CMSG_ENVELOPED_FLAG.
//
//  dwProvInnerContentType is only applicable when processing nested
//  crytographic messages. When processing an outer crytographic message
//  it must be set to 0. When decoding a nested cryptographic message
//  its the dwInnerContentType returned by a previous CryptDecodeMessage
//  of the outer message. The InnerContentType can be any of the CMSG types,
//  for example, CMSG_DATA, CMSG_SIGNED, ...
//
//  The optional *pdwMsgType is updated with the type of message.
//
//  The optional *pdwInnerContentType is updated with the type of the inner
//  message. Unless there is cryptographic message nesting, CMSG_DATA
//  is returned.
//
//  For CMSG_DATA: returns decoded content.
//  For CMSG_SIGNED: same as CryptVerifyMessageSignature.
//  For CMSG_ENVELOPED: same as CryptDecryptMessage.
//  For CMSG_SIGNED_AND_ENVELOPED: same as CryptDecryptMessage plus
//      CryptVerifyMessageSignature.
//  For CMSG_HASHED: verifies the hash and returns decoded content.
//--------------------------------------------------------------------------
function CryptDecodeMessage(dwMsgTypeFlags: DWORD;
  pDecryptPara: PCRYPT_DECRYPT_MESSAGE_PARA;
  pVerifyPara: PCRYPT_VERIFY_MESSAGE_PARA;
  dwSignerIndex: DWORD;
  const pbEncodedBlob: PBYTE;
  cbEncodedBlob: DWORD;
  dwPrevInnerContentType: DWORD;
  pdwMsgType: PDWORD;
  pdwInnerContentType: PDWORD;
  pbDecoded: PBYTE;
  pcbDecoded: PDWORD;
  var ppXchgCert: array of PCCERT_CONTEXT;
  var ppSignerCert: array of PCCERT_CONTEXT
  ): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Hash the message.
//
//  If fDetachedHash is TRUE, only the ComputedHash is encoded in the
//  pbHashedBlob. Otherwise, both the ToBeHashed and ComputedHash
//  are encoded.
//
//  pcbHashedBlob or pcbComputedHash can be NULL, indicating the caller
//  isn't interested in getting the output.
//--------------------------------------------------------------------------
function CryptHashMessage(pHashPara: PCRYPT_HASH_MESSAGE_PARA;
  fDetachedHash: BOOL;
  cToBeHashed: DWORD;
  const rgpbToBeHashed: array of PBYTE;
  rgcbToBeHashed: array of DWORD;
  pbHashedBlob: PBYTE;
  pcbHashedBlob: PDWORD;
  pbComputedHash: PBYTE;
  pcbComputedHash: PDWORD): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Verify a hashed message.
//
//  pcbToBeHashed or pcbComputedHash can be NULL,
//  indicating the caller isn't interested in getting the output.
//--------------------------------------------------------------------------
function CryptVerifyMessageHash(pHashPara: PCRYPT_HASH_MESSAGE_PARA;
  pbHashedBlob: PBYTE;
  cbHashedBlob: DWORD;
  pbToBeHashed: PBYTE;
  pcbToBeHashed: PDWORD;
  pbComputedHash: PBYTE;
  pcbComputedHash: PDWORD): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Verify a hashed message containing a detached hash.
//  The "to be hashed" content is passed in separately. No
//  decoded output. Otherwise, identical to CryptVerifyMessageHash.
//
//  pcbComputedHash can be NULL, indicating the caller isn't interested
//  in getting the output.
//--------------------------------------------------------------------------
function CryptVerifyDetachedMessageHash(pHashPara: PCRYPT_HASH_MESSAGE_PARA;
  pbDetachedHashBlob: PBYTE;
  cbDetachedHashBlob: DWORD;
  cToBeHashed: DWORD;
  rgpbToBeHashed: array of PBYTE;
  rgcbToBeHashed: array of DWORD;
  pbComputedHash: PBYTE;
  pcbComputedHash: PDWORD): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Sign the message using the provider's private key specified in the
//  parameters. A dummy SignerId is created and stored in the message.
//
//  Normally used until a certificate has been created for the key.
//--------------------------------------------------------------------------
function CryptSignMessageWithKey(pSignPara: PCRYPT_KEY_SIGN_MESSAGE_PARA;
  const pbToBeSigned: PBYTE;
  cbToBeSigned: DWORD;
  pbSignedBlob: PBYTE;
  pcbSignedBlob: PDWORD): BOOL; stdcall;
//+-------------------------------------------------------------------------
//  Verify a signed message using the specified public key info.
//
//  Normally called by a CA until it has created a certificate for the
//  key.
//
//  pPublicKeyInfo contains the public key to use to verify the signed
//  message. If NULL, the signature isn't verified (for instance, the decoded
//  content may contain the PublicKeyInfo).
//
//  pcbDecoded can be NULL, indicating the caller isn't interested
//  in getting the decoded content.
//--------------------------------------------------------------------------
function CryptVerifyMessageSignatureWithKey(pVerifyPara: PCRYPT_KEY_VERIFY_MESSAGE_PARA;
  pPublicKeyInfo: PCERT_PUBLIC_KEY_INFO;
  const pbSignedBlob: PBYTE;
  cbSignedBlob: DWORD;
  pbDecoded: PBYTE;
  pcbDecoded: PDWORD): BOOL; stdcall;

//+=========================================================================
//  System Certificate Store Data Structures and APIs
//==========================================================================


//+-------------------------------------------------------------------------
//  Get a system certificate store based on a subsystem protocol.
//
//  Current examples of subsystems protocols are:
//      "MY"    Cert Store hold certs with associated Private Keys
//      "CA"    Certifying Authority certs
//      "ROOT"  Root Certs
//      "SPC"   Software publisher certs
//
//
//  If hProv is NULL the default provider "1" is opened for you.
//  When the store is closed the provider is release. Otherwise
//  if hProv is not NULL, no provider is created or released.
//
//  The returned Cert Store can be searched for an appropriate Cert
//  using the Cert Store API's (see certstor.h)
//
//  When done, the cert store should be closed using CertStoreClose
//--------------------------------------------------------------------------

function CertOpenSystemStoreA(hProv: HCRYPTPROV;
  szSubsystemProtocol: LPCSTR): HCERTSTORE; stdcall;

function CertOpenSystemStoreW(hProv: HCRYPTPROV;
  szSubsystemProtocol: LPCWSTR): HCERTSTORE; stdcall;

function CertOpenSystemStore(hProv: HCRYPTPROV;
  szSubsystemProtocol: LPAWSTR): HCERTSTORE; stdcall;

function CertAddEncodedCertificateToSystemStoreA(szCertStoreName: LPCSTR;
  const pbCertEncoded: PBYTE;
  cbCertEncoded: DWORD): BOOL; stdcall;

function CertAddEncodedCertificateToSystemStoreW(szCertStoreName: LPCWSTR;
  const pbCertEncoded: PBYTE;
  cbCertEncoded: DWORD): BOOL; stdcall;

function CertAddEncodedCertificateToSystemStore(szCertStoreName: LPAWSTR;
  const pbCertEncoded: PBYTE;
  cbCertEncoded: DWORD): BOOL; stdcall;


//+-------------------------------------------------------------------------
//  Find all certificate chains tying the given issuer name to any certificate
//  that the current user has a private key for.
//
//  If no certificate chain is found, FALSE is returned with LastError set
//  to CRYPT_E_NOT_FOUND and the counts zeroed.
//
//  IE 3.0 ASSUMPTION:
//   The client certificates are in the "My" system store. The issuer
//   cerificates may be in the "Root", "CA" or "My" system stores.
//--------------------------------------------------------------------------
type
  PCERT_CHAIN = ^CERT_CHAIN;
  CERT_CHAIN = record
    cCerts: DWORD; // number of certs in chain
    certs: PCERT_BLOB; // pointer to array of cert chain blobs representing the certs
    keyLocatorInfo: CRYPT_KEY_PROV_INFO; // key locator for cert
  end;


// WINCRYPT32API    This is not exported by crypt32, it is exported by softpub
function FindCertsByIssuer(pCertChains: PCERT_CHAIN;
  pcbCertChains: PDWORD;
  pcCertChains: PDWORD; // count of certificates chains returned
  pbEncodedIssuerName: PBYTE; // DER encoded issuer name
  cbEncodedIssuerName: DWORD; // count in bytes of encoded issuer name
  pwszPurpose: LPCWSTR; // "ClientAuth" or "CodeSigning"
  dwKeySpec: DWORD // only return signers supporting this keyspec
  ): HRESULT; stdcall;

//////////////////////////// VERSION 2 ////////////////////////////////////////////////////////////////////

implementation

{Macro inplementation}

function GET_ALG_CLASS(x: integer): integer;
begin
  Result := (x and (7 shl 13));
end;

function GET_ALG_TYPE(x: integer): integer;
begin
  Result := (x and (15 shl 9));
end;

function GET_ALG_SID(x: integer): integer;
begin
  Result := (x and (511));
end;

function RCRYPT_SUCCEEDED(rt: BOOL): BOOL;
begin
  Result := rt = CRYPT_SUCCEED;
end;

function RCRYPT_FAILED(rt: BOOL): BOOL;
begin
  Result := rt = CRYPT_FAILED;
end;

function GET_CERT_UNICODE_RDN_ERR_INDEX(X: integer): integer;
begin
  Result := ((X shr CERT_UNICODE_RDN_ERR_INDEX_SHIFT) and CERT_UNICODE_RDN_ERR_INDEX_MASK);
end;

function GET_CERT_UNICODE_ATTR_ERR_INDEX(X: integer): integer;
begin
  Result := ((X shr CERT_UNICODE_ATTR_ERR_INDEX_SHIFT) and CERT_UNICODE_ATTR_ERR_INDEX_MASK);
end;

function GET_CERT_UNICODE_VALUE_ERR_INDEX(X: integer): integer;
begin
  Result := (X and CERT_UNICODE_VALUE_ERR_INDEX_MASK);
end;

function GET_CERT_ALT_NAME_ENTRY_ERR_INDEX(X: DWORD): DWORD;
begin
  Result := ((X shr CERT_ALT_NAME_ENTRY_ERR_INDEX_SHIFT) and CERT_ALT_NAME_ENTRY_ERR_INDEX_MASK);
end;

function GET_CERT_ALT_NAME_VALUE_ERR_INDEX(X: DWORD): DWORD;
begin
  Result := (X and CERT_ALT_NAME_VALUE_ERR_INDEX_MASK);
end;

function GET_CRL_DIST_POINT_ERR_INDEX(X: DWORD): DWORD;
begin
  Result := ((X shr CRL_DIST_POINT_ERR_INDEX_SHIFT) and CRL_DIST_POINT_ERR_INDEX_MASK);
end;

function IS_CRL_DIST_POINT_ERR_CRL_ISSUER(X: DWORD): BOOL;
begin
  Result := (0 <> (X and CRL_DIST_POINT_ERR_CRL_ISSUER_BIT));
end;
///////////////////////////////////////version 2 /////////////////////////

function IS_CERT_RDN_CHAR_STRING(X: DWORD): BOOL;
begin
  Result := BOOL(X >= CERT_RDN_NUMERIC_STRING);
end;

function GET_CERT_ENCODING_TYPE(X: DWORD): DWORD;
begin
  Result := (X and CERT_ENCODING_TYPE_MASK);
end;

function GET_CMSG_ENCODING_TYPE(X: DWORD): DWORD;
begin
  Result := (X and CMSG_ENCODING_TYPE_MASK);
end;

function IS_CERT_HASH_PROP_ID(X: DWORD): BOOL;
begin
  if (X = CERT_SHA1_HASH_PROP_ID) or (X = CERT_MD5_HASH_PROP_ID) then
    Result := True
  else
    Result := False;
end;
{end Macro}

function CryptAcquireContextA; external ADVAPI32 name 'CryptAcquireContextA';
{$IFDEF UNICODE}
function CryptAcquireContext; external ADVAPI32 name 'CryptAcquireContextW';
{$ELSE}
function CryptAcquireContext; external ADVAPI32 name 'CryptAcquireContextA';
{$ENDIF}
function CryptAcquireContextW; external ADVAPI32 name 'CryptAcquireContextW';
function CryptReleaseContext; external ADVAPI32 name 'CryptReleaseContext';
function CryptGenKey; external ADVAPI32 name 'CryptGenKey';
function CryptDeriveKey; external ADVAPI32 name 'CryptDeriveKey';
function CryptDestroyKey; external ADVAPI32 name 'CryptDestroyKey';
function CryptSetKeyParam; external ADVAPI32 name 'CryptSetKeyParam';
function CryptGetKeyParam; external ADVAPI32 name 'CryptGetKeyParam';
function CryptSetHashParam; external ADVAPI32 name 'CryptSetHashParam';
function CryptGetHashParam; external ADVAPI32 name 'CryptGetHashParam';
function CryptSetProvParam; external ADVAPI32 name 'CryptSetProvParam';
function CryptGetProvParam; external ADVAPI32 name 'CryptGetProvParam';
function CryptGenRandom; external ADVAPI32 name 'CryptGenRandom';
function CryptGetUserKey; external ADVAPI32 name 'CryptGetUserKey';
function CryptExportKey; external ADVAPI32 name 'CryptExportKey';
function CryptImportKey; external ADVAPI32 name 'CryptImportKey';
function CryptEncrypt; external ADVAPI32 name 'CryptEncrypt';
function CryptDecrypt; external ADVAPI32 name 'CryptDecrypt';
function CryptCreateHash; external ADVAPI32 name 'CryptCreateHash';
function CryptHashData; external ADVAPI32 name 'CryptHashData';
function CryptHashSessionKey; external ADVAPI32 name 'CryptHashSessionKey';
function CryptDestroyHash; external ADVAPI32 name 'CryptDestroyHash';
function CryptSignHashA; external ADVAPI32 name 'CryptSignHashA';
function CryptSignHashW; external ADVAPI32 name 'CryptSignHashW';
function CryptSignHashU; external CRYPT32 name 'CryptSignHashU';
{$IFDEF UNICODE}
function CryptSignHash; external ADVAPI32 name 'CryptSignHashW';
{$ELSE}
function CryptSignHash; external ADVAPI32 name 'CryptSignHashA';
{$ENDIF}
function CryptVerifySignatureA; external ADVAPI32 name 'CryptVerifySignatureA';
function CryptVerifySignatureW; external ADVAPI32 name 'CryptVerifySignatureW';
{$IFDEF UNICODE}
function CryptVerifySignature; external ADVAPI32 name 'CryptVerifySignatureW';
{$ELSE}
function CryptVerifySignature; external ADVAPI32 name 'CryptVerifySignatureA';
{$ENDIF}
function CryptSetProviderW; external ADVAPI32 name 'CryptSetProviderW';
function CryptSetProviderA; external ADVAPI32 name 'CryptSetProviderA';
function CryptSetProviderU; external CRYPT32 name 'CryptSetProviderU';
{$IFDEF UNICODE}
function CryptSetProvider; external ADVAPI32 name 'CryptSetProviderW';
{$ELSE}
function CryptSetProvider; external ADVAPI32 name 'CryptSetProviderA';
{$ENDIF}

{$IFDEF NT5}
function CryptSetProviderExA; external ADVAPI32NT5 name 'CryptSetProviderExA'; //nt5 advapi32
function CryptSetProviderExW; external ADVAPI32NT5 name 'CryptSetProviderExW';
{$IFDEF UNICODE}
function CryptSetProviderEx; external ADVAPI32NT5 name 'CryptSetProviderExW';
{$ELSE}
function CryptSetProviderEx; external ADVAPI32NT5 name 'CryptSetProviderExA';
{$ENDIF} // !UNICODE

function CryptGetDefaultProviderA; external ADVAPI32NT5 name 'CryptGetDefaultProviderA'; //nt5 advapi32
function CryptGetDefaultProviderW; external ADVAPI32NT5 name 'CryptGetDefaultProviderW';
{$IFDEF UNICODE}
function CryptGetDefaultProvider; external ADVAPI32NT5 name 'CryptGetDefaultProviderW';
{$ELSE}
function CryptGetDefaultProvider; external ADVAPI32NT5 name 'CryptGetDefaultProviderA';
{$ENDIF} // !UNICODE

function CryptEnumProviderTypesA; external ADVAPI32NT5 name 'CryptEnumProviderTypesA'; //nt5 advapi32
function CryptEnumProviderTypesW; external ADVAPI32NT5 name 'CryptEnumProviderTypesW';
{$IFDEF UNICODE}
function CryptEnumProviderTypes; external ADVAPI32NT5 name 'CryptEnumProviderTypesW';
{$ELSE}
function CryptEnumProviderTypes; external ADVAPI32NT5 name 'CryptEnumProviderTypesA';
{$ENDIF} // !UNICODE

function CryptEnumProvidersA; external ADVAPI32NT5 name 'CryptEnumProvidersA'; //nt5 advapi32
function CryptEnumProvidersW; external ADVAPI32NT5 name 'CryptEnumProvidersW';

{$IFDEF UNICODE}
function CryptEnumProviders; external ADVAPI32NT5 name 'CryptEnumProvidersW';
{$ELSE}
function CryptEnumProviders; external ADVAPI32NT5 name 'CryptEnumProvidersA';
{$ENDIF} // !UNICODE
function CryptContextAddRef; external ADVAPI32NT5 name 'CryptContextAddRef'; //nt5 advapi32
function CryptDuplicateKey; external ADVAPI32NT5 name 'CryptDuplicateKey'; //nt5 advapi32
function CryptDuplicateHash; external ADVAPI32NT5 name 'CryptDuplicateHash'; //nt5 advapi32
{$ENDIF NT5}

function CryptEnumProvidersU; external CRYPT32 name 'CryptEnumProvidersU';
function CryptFormatObject; external CRYPT32 name 'CryptFormatObject';
function CryptEncodeObject; external CRYPT32 name 'CryptEncodeObject';
function CryptDecodeObject; external CRYPT32 name 'CryptDecodeObject';
function CryptInstallOIDFunctionAddress; external CRYPT32 name 'CryptInstallOIDFunctionAddress';
function CryptInitOIDFunctionSet; external CRYPT32 name 'CryptInitOIDFunctionSet';
function CryptGetOIDFunctionAddress; external CRYPT32 name 'CryptGetOIDFunctionAddress';
function CryptGetDefaultOIDDllList; external CRYPT32 name 'CryptGetDefaultOIDDllList';
function CryptGetDefaultOIDFunctionAddress; external CRYPT32 name 'CryptGetDefaultOIDFunctionAddress';
function CryptFreeOIDFunctionAddress; external CRYPT32 name 'CryptFreeOIDFunctionAddress';
function CryptRegisterOIDFunction; external CRYPT32 name 'CryptRegisterOIDFunction';
function CryptUnregisterOIDFunction; external CRYPT32 name 'CryptUnregisterOIDFunction';
function CryptRegisterDefaultOIDFunction; external CRYPT32 name 'CryptRegisterDefaultOIDFunction';
function CryptUnregisterDefaultOIDFunction; external CRYPT32 name 'CryptUnregisterDefaultOIDFunction';
function CryptSetOIDFunctionValue; external CRYPT32 name 'CryptSetOIDFunctionValue';
function CryptGetOIDFunctionValue; external CRYPT32 name 'CryptGetOIDFunctionValue';
function CryptEnumOIDFunction; external CRYPT32 name 'CryptEnumOIDFunction';
function CryptFindOIDInfo; external CRYPT32 name 'CryptFindOIDInfo';

function CryptRegisterOIDInfo; external CRYPT32 name 'CryptRegisterOIDInfo';
function CryptUnregisterOIDInfo; external CRYPT32 name 'CryptUnregisterOIDInfo';
function CryptMsgOpenToEncode; external CRYPT32 name 'CryptMsgOpenToEncode';
function CryptMsgCalculateEncodedLength; external CRYPT32 name 'CryptMsgCalculateEncodedLength';
function CryptMsgOpenToDecode; external CRYPT32 name 'CryptMsgOpenToDecode';
function CryptMsgClose; external CRYPT32 name 'CryptMsgClose';
function CryptMsgUpdate; external CRYPT32 name 'CryptMsgUpdate';
function CryptMsgControl; external CRYPT32 name 'CryptMsgControl';
function CryptMsgVerifyCountersignatureEncoded; external CRYPT32 name 'CryptMsgVerifyCountersignatureEncoded';
function CryptMsgCountersign; external CRYPT32 name 'CryptMsgCountersign';
function CryptMsgCountersignEncoded; external CRYPT32 name 'CryptMsgCountersignEncoded';
function CryptMsgGetParam; external CRYPT32 name 'CryptMsgGetParam';
function CertOpenStore; external CRYPT32 name 'CertOpenStore';
function CertDuplicateStore; external CRYPT32 name 'CertDuplicateStore';
function CertSaveStore; external CRYPT32 name 'CertSaveStore';
function CertCloseStore; external CRYPT32 name 'CertCloseStore';
function CertGetSubjectCertificateFromStore; external CRYPT32 name 'CertGetSubjectCertificateFromStore';
function CertEnumCertificatesInStore; external CRYPT32 name 'CertEnumCertificatesInStore';
function CertFindCertificateInStore; external CRYPT32 name 'CertFindCertificateInStore';
function CertGetIssuerCertificateFromStore; external CRYPT32 name 'CertGetIssuerCertificateFromStore';
function CertVerifySubjectCertificateContext; external CRYPT32 name 'CertVerifySubjectCertificateContext';
function CertDuplicateCertificateContext; external CRYPT32 name 'CertDuplicateCertificateContext';
function CertCreateCertificateContext; external CRYPT32 name 'CertCreateCertificateContext';
function CertFreeCertificateContext; external CRYPT32 name 'CertFreeCertificateContext';
function CertSetCertificateContextProperty; external CRYPT32 name 'CertSetCertificateContextProperty';
function CertGetCertificateContextProperty; external CRYPT32 name 'CertGetCertificateContextProperty';
function CertEnumCertificateContextProperties; external CRYPT32 name 'CertEnumCertificateContextProperties';
function CertGetCRLFromStore; external CRYPT32 name 'CertGetCRLFromStore';
function CertDuplicateCRLContext; external CRYPT32 name 'CertDuplicateCRLContext';
function CertCreateCRLContext; external CRYPT32 name 'CertCreateCRLContext';
function CertFreeCRLContext; external CRYPT32 name 'CertFreeCRLContext';
function CertSetCRLContextProperty; external CRYPT32 name 'CertSetCRLContextProperty';
function CertGetCRLContextProperty; external CRYPT32 name 'CertGetCRLContextProperty';
function CertEnumCRLContextProperties; external CRYPT32 name 'CertEnumCRLContextProperties';
function CertAddEncodedCertificateToStore; external CRYPT32 name 'CertAddEncodedCertificateToStore';
function CertAddCertificateContextToStore; external CRYPT32 name 'CertAddCertificateContextToStore';
function CertAddSerializedElementToStore; external CRYPT32 name 'CertAddSerializedElementToStore';
function CertDeleteCertificateFromStore; external CRYPT32 name 'CertDeleteCertificateFromStore';
function CertAddEncodedCRLToStore; external CRYPT32 name 'CertAddEncodedCRLToStore';
function CertAddCRLContextToStore; external CRYPT32 name 'CertAddCRLContextToStore';
function CertDeleteCRLFromStore; external CRYPT32 name 'CertDeleteCRLFromStore';
function CertSerializeCertificateStoreElement; external CRYPT32 name 'CertSerializeCertificateStoreElement';
function CertSerializeCRLStoreElement; external CRYPT32 name 'CertSerializeCRLStoreElement';
function CertDuplicateCTLContext; external CRYPT32 name 'CertDuplicateCTLContext';
function CertCreateCTLContext; external CRYPT32 name 'CertCreateCTLContext';
function CertFreeCTLContext; external CRYPT32 name 'CertFreeCTLContext';
function CertSetCTLContextProperty; external CRYPT32 name 'CertSetCTLContextProperty';
function CertGetCTLContextProperty; external CRYPT32 name 'CertGetCTLContextProperty';
function CertEnumCTLContextProperties; external CRYPT32 name 'CertEnumCTLContextProperties';
function CertEnumCTLsInStore; external CRYPT32 name 'CertEnumCTLsInStore';
function CertFindSubjectInCTL; external CRYPT32 name 'CertFindSubjectInCTL';
function CertFindCTLInStore; external CRYPT32 name 'CertFindCTLInStore';
function CertAddEncodedCTLToStore; external CRYPT32 name 'CertAddEncodedCTLToStore';
function CertAddCTLContextToStore; external CRYPT32 name 'CertAddCTLContextToStore';
function CertSerializeCTLStoreElement; external CRYPT32 name 'CertSerializeCTLStoreElement';
function CertDeleteCTLFromStore; external CRYPT32 name 'CertDeleteCTLFromStore';
function CertGetEnhancedKeyUsage; external CRYPT32 name 'CertGetEnhancedKeyUsage';
function CertSetEnhancedKeyUsage; external CRYPT32 name 'CertSetEnhancedKeyUsage';
function CertAddEnhancedKeyUsageIdentifier; external CRYPT32 name 'CertAddEnhancedKeyUsageIdentifier';
function CertRemoveEnhancedKeyUsageIdentifier; external CRYPT32 name 'CertRemoveEnhancedKeyUsageIdentifier';
function CryptMsgGetAndVerifySigner; external CRYPT32 name 'CryptMsgGetAndVerifySigner';
function CryptMsgSignCTL; external CRYPT32 name 'CryptMsgSignCTL';
function CryptMsgEncodeAndSignCTL; external CRYPT32 name 'CryptMsgEncodeAndSignCTL';
function CertVerifyCTLUsage; external CRYPT32 name 'CertVerifyCTLUsage';
function CertVerifyRevocation; external CRYPT32 name 'CertVerifyRevocation';
function CertCompareIntegerBlob; external CRYPT32 name 'CertCompareIntegerBlob';
function CertCompareCertificate; external CRYPT32 name 'CertCompareCertificate';
function CertCompareCertificateName; external CRYPT32 name 'CertCompareCertificateName';
function CertIsRDNAttrsInCertificateName; external CRYPT32 name 'CertIsRDNAttrsInCertificateName';
function CertComparePublicKeyInfo; external CRYPT32 name 'CertComparePublicKeyInfo';
function CertGetPublicKeyLength; external CRYPT32 name 'CertGetPublicKeyLength';
function CryptVerifyCertificateSignature; external CRYPT32 name 'CryptVerifyCertificateSignature';
function CryptHashToBeSigned; external CRYPT32 name 'CryptHashToBeSigned';
function CryptHashCertificate; external CRYPT32 name 'CryptHashCertificate';
function CryptSignCertificate; external CRYPT32 name 'CryptSignCertificate';
function CryptSignAndEncodeCertificate; external CRYPT32 name 'CryptSignAndEncodeCertificate';
function CertVerifyTimeValidity; external CRYPT32 name 'CertVerifyTimeValidity';
function CertVerifyCRLTimeValidity; external CRYPT32 name 'CertVerifyCRLTimeValidity';
function CertVerifyValidityNesting; external CRYPT32 name 'CertVerifyValidityNesting';
function CertVerifyCRLRevocation; external CRYPT32 name 'CertVerifyCRLRevocation';
function CertAlgIdToOID; external CRYPT32 name 'CertAlgIdToOID';
function CertOIDToAlgId; external CRYPT32 name 'CertOIDToAlgId';
function CertFindExtension; external CRYPT32 name 'CertFindExtension';
function CertFindAttribute; external CRYPT32 name 'CertFindAttribute';
function CertFindRDNAttr; external CRYPT32 name 'CertFindRDNAttr';
function CertGetIntendedKeyUsage; external CRYPT32 name 'CertGetIntendedKeyUsage';
function CryptExportPublicKeyInfo; external CRYPT32 name 'CryptExportPublicKeyInfo';
function CryptExportPublicKeyInfoEx; external CRYPT32 name 'CryptExportPublicKeyInfoEx';
function CryptImportPublicKeyInfo; external CRYPT32 name 'CryptImportPublicKeyInfo';
function CryptImportPublicKeyInfoEx; external CRYPT32 name 'CryptImportPublicKeyInfoEx';
function CryptHashPublicKeyInfo; external CRYPT32 name 'CryptHashPublicKeyInfo';
function CertRDNValueToStrA; external CRYPT32 name 'CertRDNValueToStrA';
function CertRDNValueToStrW; external CRYPT32 name 'CertRDNValueToStrW';
{$IFDEF UNICODE}
function CertRDNValueToStr; external CRYPT32 name 'CertRDNValueToStrW';
{$ELSE}
function CertRDNValueToStr; external CRYPT32 name 'CertRDNValueToStrA';
{$ENDIF} // !UNICODE
function CertNameToStrA; external CRYPT32 name 'CertNameToStrA';
function CertNameToStrW; external CRYPT32 name 'CertNameToStrW';
{$IFDEF UNICODE}
function CertNameToStr; external CRYPT32 name 'CertNameToStrW';
{$ELSE}
function CertNameToStr; external CRYPT32 name 'CertNameToStrA';
{$ENDIF} // !UNICODE
function CertStrToNameW; external CRYPT32 name 'CertStrToNameW';
function CertStrToNameA; external CRYPT32 name 'CertStrToNameA';
{$IFDEF UNICODE}
function CertStrToName; external CRYPT32 name 'CertStrToNameW';
{$ELSE}
function CertStrToName; external CRYPT32 name 'CertStrToNameA';
{$ENDIF} // !UNICODE
function CryptSignMessage; external CRYPT32 name 'CryptSignMessage';
//function CryptSignMessageWithKey; external CRYPT32 name 'CryptSignMessageWithKey';
function CryptVerifyMessageSignature; external CRYPT32 name 'CryptVerifyMessageSignature';
//function CryptVerifyMessageSignatureWithKey; external CRYPT32 name 'CryptVerifyMessageSignatureWithKey';
function CryptGetMessageSignerCount; external CRYPT32 name 'CryptGetMessageSignerCount';
function CryptGetMessageCertificates; external CRYPT32 name 'CryptGetMessageCertificates';
function CryptVerifyDetachedMessageSignature; external CRYPT32 name 'CryptVerifyDetachedMessageSignature';
function CryptEncryptMessage; external CRYPT32 name 'CryptEncryptMessage';
function CryptDecryptMessage; external CRYPT32 name 'CryptDecryptMessage';
function CryptSignAndEncryptMessage; external CRYPT32 name 'CryptSignAndEncryptMessage';
function CryptDecryptAndVerifyMessageSignature; external CRYPT32 name 'CryptDecryptAndVerifyMessageSignature';
function CryptDecodeMessage; external CRYPT32 name 'CryptDecodeMessage';
function CryptHashMessage; external CRYPT32 name 'CryptHashMessage';
function CryptVerifyMessageHash; external CRYPT32 name 'CryptVerifyMessageHash';
function CryptVerifyDetachedMessageHash; external CRYPT32 name 'CryptVerifyDetachedMessageHash';
function CryptSignMessageWithKey; external CRYPT32 name 'CryptSignMessageWithKey';
function CryptVerifyMessageSignatureWithKey; external CRYPT32 name 'CryptVerifyMessageSignatureWithKey';
function CertOpenSystemStoreA; external CRYPT32 name 'CertOpenSystemStoreA';
function CertOpenSystemStoreW; external CRYPT32 name 'CertOpenSystemStoreW';
{$IFDEF UNICODE}
function CertOpenSystemStore; external CRYPT32 name 'CertOpenSystemStoreW';
{$ELSE}
function CertOpenSystemStore; external CRYPT32 name 'CertOpenSystemStoreA';
{$ENDIF} // !UNICODE
function CertAddEncodedCertificateToSystemStoreA; external CRYPT32 name 'CertAddEncodedCertificateToSystemStoreA';
function CertAddEncodedCertificateToSystemStoreW; external CRYPT32 name 'CertAddEncodedCertificateToSystemStoreW';
{$IFDEF UNICODE}
function CertAddEncodedCertificateToSystemStore; external CRYPT32 name 'CertAddEncodedCertificateToSystemStoreW';
{$ELSE}
function CertAddEncodedCertificateToSystemStore; external CRYPT32 name 'CertAddEncodedCertificateToSystemStoreA';
{$ENDIF} // !UNICODE
function FindCertsByIssuer; external SOFTPUB name 'FindCertsByIssuer';
end.
