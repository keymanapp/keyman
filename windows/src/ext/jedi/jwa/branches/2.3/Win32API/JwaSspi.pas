{******************************************************************************}
{                                                                              }
{ Security Service Provider API interface Unit for Object Pascal               }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: sspi.h, released June 2000. The original Pascal        }
{ code is: Sspi.pas, released December 2000. The initial developer of the      }
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

// $Id: JwaSspi.pas,v 1.13 2007/09/14 06:48:47 marquardt Exp $
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaSspi;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "sspi.h"'}
{$HPPEMIT ''}
{$HPPEMIT '#typedef SEC_CHAR *PSEC_CHAR'}
{$HPPEMIT '#typedef SEC_WCHAR *PSEC_WCHAR'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaWinType;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}
//
// Determine environment:
//

const
  ISSP_LEVEL = 32;
  {$EXTERNALSYM ISSP_LEVEL}
  ISSP_MODE  = 1;
  {$EXTERNALSYM ISSP_MODE}

//
// Now, define platform specific mappings:
//

//
// For NT-2 and up, wtypes will define HRESULT to be long.
//

type
  SEC_WCHAR = WCHAR;
  {$EXTERNALSYM SEC_WCHAR}
  PSEC_WCHAR = ^SEC_CHAR;
  {$NODEFINE PSEC_WCHAR}
  PSecWChar = ^TSecWChar;
  TSecWChar = SEC_WCHAR;

  SEC_CHAR = AnsiChar;
  {$EXTERNALSYM SEC_CHAR}
  PSEC_CHAR = ^SEC_CHAR;
  {$NODEFINE PSEC_CHAR}
  PSecChar = ^TSecChar;
  TSecChar = SEC_CHAR;

  SECURITY_STATUS = LONG;
  {$EXTERNALSYM SECURITY_STATUS}
  PSecurityStatus = ^TSecurityStatus;
  TSecurityStatus = SECURITY_STATUS;

//
// Decide what a string - 32 bits only since for 16 bits it is clear.
//

  {$IFDEF UNICODE}
  SECURITY_PSTR = ^SEC_WCHAR;
  {$EXTERNALSYM SECURITY_PSTR}
  SECURITY_PCSTR = ^SEC_WCHAR;
  {$EXTERNALSYM SECURITY_PCSTR}
  {$ELSE}
  SECURITY_PSTR = ^SEC_CHAR;
  {$EXTERNALSYM SECURITY_PSTR}
  SECURITY_PCSTR = ^SEC_CHAR;
  {$EXTERNALSYM SECURITY_PCSTR}
  {$ENDIF UNICODE}

//
// Okay, security specific types:
//

  {$IFNDEF JWA_INCLUDEMODE}
  PSecHandle = ^SecHandle;
  {$EXTERNALSYM PSecHandle}
  _SecHandle = record
    dwLower: ULONG_PTR;
    dwUpper: ULONG_PTR;
  end;
  {$EXTERNALSYM _SecHandle}
  SecHandle = _SecHandle;
  {$EXTERNALSYM SecHandle}
  TSecHandle = SecHandle;
  {$ENDIF JWA_INCLUDEMODE}

procedure SecInvalidateHandle(var x: SecHandle);
{$EXTERNALSYM SecInvalidateHandle}

function SecIsValidHandle(x: SecHandle): Boolean;
{$EXTERNALSYM SecIsValidHandle}

type
  CredHandle = SecHandle;
  {$EXTERNALSYM CredHandle}
  PCredHandle = ^CredHandle;
  {$EXTERNALSYM PCredHandle}
  TCredHandle = CredHandle;

  CtxtHandle = SecHandle;
  {$EXTERNALSYM CtxtHandle}

  {$IFNDEF JWA_INCLUDEMODE}
  PCtxtHandle = ^CtxtHandle;
  {$EXTERNALSYM PCtxthandle}
  {$ENDIF JWA_INCLUDEMODE}
  TCtxthandle = CtxtHandle;

  _SECURITY_INTEGER = LARGE_INTEGER;
  {$EXTERNALSYM _SECURITY_INTEGER}
  SECURITY_INTEGER = _SECURITY_INTEGER;
  {$EXTERNALSYM SECURITY_INTEGER}
  PSECURITY_INTEGER = ^SECURITY_INTEGER;
  {$EXTERNALSYM PSECURITY_INTEGER}

// todo Timestamp was removed from SSPI in August 2001 PSDK, where is it now?!

  {$IFNDEF JWA_INCLUDEMODE}
  TimeStamp = SECURITY_INTEGER;
  {$EXTERNALSYM TimeStamp}
  {$ENDIF JWA_INCLUDEMODE}

  PTimeStamp = ^SECURITY_INTEGER;
  {$EXTERNALSYM PTimeStamp}

  {$IFNDEF JWA_INCLUDEMODE}
  TTimeStamp = TimeStamp;
  {$ENDIF JWA_INCLUDEMODE}

//
// If we are in 32 bit mode, define the SECURITY_STRING structure,
// as a clone of the base UNICODE_STRING structure.  This is used
// internally in security components, an as the string interface
// for kernel components (e.g. FSPs)
//

  SECURITY_STRING = UNICODE_STRING;
  {$EXTERNALSYM SECURITY_STRING}
  PSECURITY_STRING = ^SECURITY_STRING;
  {$EXTERNALSYM PSECURITY_STRING}
  TSecurityString = SECURITY_STRING;
  PSecurityString = PSECURITY_STRING;

//
// SecPkgInfo structure
//
//  Provides general information about a security provider
//

  PSecPkgInfoW = ^SecPkgInfoW;
  {$EXTERNALSYM PSecPkgInfoW}
  _SecPkgInfoW = record
    fCapabilities: Cardinal; // Capability bitmask
    wVersion: Word; // Version of driver
    wRPCID: Word; // ID for RPC Runtime
    cbMaxToken: Cardinal; // Size of authentication token (max)
    Name: PSecWChar; // Text name
    Comment: PSecWChar; // Comment
  end;
  {$EXTERNALSYM _SecPkgInfoW}
  SecPkgInfoW = _SecPkgInfoW;
  {$EXTERNALSYM SecPkgInfoW}
  TSecPkgInfoW = SecPkgInfoW;

  PSecPkgInfoA = ^SecPkgInfoA;
  {$EXTERNALSYM PSecPkgInfoA}
  _SecPkgInfoA = record
    fCapabilities: Cardinal; // Capability bitmask
    wVersion: Word; // Version of driver
    wRPCID: Word; // ID for RPC Runtime
    cbMaxToken: Cardinal; // Size of authentication token (max)
    Name: PSecChar; // Text name
    Comment: PSecChar; // Comment
  end;
  {$EXTERNALSYM _SecPkgInfoA}
  SecPkgInfoA = _SecPkgInfoA;
  {$EXTERNALSYM SecPkgInfoA}
  TSecPkgInfoA = SecPkgInfoA;

  {$IFDEF UNICODE}
  SecPkgInfo = SecPkgInfoW;
  {$EXTERNALSYM SecPkgInfo}
  PSecPkgInfo = PSecPkgInfoW;
  {$EXTERNALSYM PSecPkgInfo}
  TSecPkgInfo = TSecPkgInfoW;
  {$ELSE}
  SecPkgInfo = SecPkgInfoA;
  {$EXTERNALSYM SecPkgInfo}
  PSecPkgInfo = PSecPkgInfoA;
  {$EXTERNALSYM PSecPkgInfo}
  TSecPkgInfo = TSecPkgInfoA;
  {$ENDIF UNICODE}

//
//  Security Package Capabilities
//

const
  SECPKG_FLAG_INTEGRITY         = $00000001; // Supports integrity on messages
  {$EXTERNALSYM SECPKG_FLAG_INTEGRITY}
  SECPKG_FLAG_PRIVACY           = $00000002; // Supports privacy (confidentiality)
  {$EXTERNALSYM SECPKG_FLAG_PRIVACY}
  SECPKG_FLAG_TOKEN_ONLY        = $00000004; // Only security token needed
  {$EXTERNALSYM SECPKG_FLAG_TOKEN_ONLY}
  SECPKG_FLAG_DATAGRAM          = $00000008; // Datagram RPC support
  {$EXTERNALSYM SECPKG_FLAG_DATAGRAM}
  SECPKG_FLAG_CONNECTION        = $00000010; // Connection oriented RPC support
  {$EXTERNALSYM SECPKG_FLAG_CONNECTION}
  SECPKG_FLAG_MULTI_REQUIRED    = $00000020; // Full 3-leg required for re-auth.
  {$EXTERNALSYM SECPKG_FLAG_MULTI_REQUIRED}
  SECPKG_FLAG_CLIENT_ONLY       = $00000040; // Server side functionality not available
  {$EXTERNALSYM SECPKG_FLAG_CLIENT_ONLY}
  SECPKG_FLAG_EXTENDED_ERROR    = $00000080; // Supports extended error msgs
  {$EXTERNALSYM SECPKG_FLAG_EXTENDED_ERROR}
  SECPKG_FLAG_IMPERSONATION     = $00000100; // Supports impersonation
  {$EXTERNALSYM SECPKG_FLAG_IMPERSONATION}
  SECPKG_FLAG_ACCEPT_WIN32_NAME = $00000200; // Accepts Win32 names
  {$EXTERNALSYM SECPKG_FLAG_ACCEPT_WIN32_NAME}
  SECPKG_FLAG_STREAM            = $00000400; // Supports stream semantics
  {$EXTERNALSYM SECPKG_FLAG_STREAM}
  SECPKG_FLAG_NEGOTIABLE        = $00000800; // Can be used by the negotiate package
  {$EXTERNALSYM SECPKG_FLAG_NEGOTIABLE}
  SECPKG_FLAG_GSS_COMPATIBLE    = $00001000; // GSS Compatibility Available
  {$EXTERNALSYM SECPKG_FLAG_GSS_COMPATIBLE}
  SECPKG_FLAG_LOGON             = $00002000; // Supports common LsaLogonUser
  {$EXTERNALSYM SECPKG_FLAG_LOGON}
  SECPKG_FLAG_ASCII_BUFFERS     = $00004000; // Token Buffers are in ASCII
  {$EXTERNALSYM SECPKG_FLAG_ASCII_BUFFERS}
  SECPKG_FLAG_FRAGMENT          = $00008000; // Package can fragment to fit
  {$EXTERNALSYM SECPKG_FLAG_FRAGMENT}
  SECPKG_FLAG_MUTUAL_AUTH       = $00010000; // Package can perform mutual authentication
  {$EXTERNALSYM SECPKG_FLAG_MUTUAL_AUTH}
  SECPKG_FLAG_DELEGATION        = $00020000; // Package can delegate
  {$EXTERNALSYM SECPKG_FLAG_DELEGATION}

  SECPKG_ID_NONE = $FFFF;
  {$EXTERNALSYM SECPKG_ID_NONE}

//
// SecBuffer
//
//  Generic memory descriptors for buffers passed in to the security
//  API
//

type
  PSecBuffer = ^SecBuffer;
  {$EXTERNALSYM PSecBuffer}
  _SecBuffer = record
    cbBuffer: Cardinal;   // Size of the buffer, in bytes
    BufferType: Cardinal; // Type of the buffer (below)
    pvBuffer: Pointer;    // Pointer to the buffer
  end;
  {$EXTERNALSYM _SecBuffer}
  SecBuffer = _SecBuffer;
  {$EXTERNALSYM SecBuffer}
  TSecBuffer = SecBuffer;

  PSecBufferDesc = ^SecBufferDesc;
  {$EXTERNALSYM PSecBufferDesc}
  _SecBufferDesc = record
    ulVersion: Cardinal;  // Version number
    cBuffers: Cardinal;   // Number of buffers
    pBuffers: PSecBuffer; // Pointer to array of buffers
  end;
  {$EXTERNALSYM _SecBufferDesc}
  SecBufferDesc = _SecBufferDesc;
  {$EXTERNALSYM SecBufferDesc}
  TSecBufferDesc = SecBufferDesc;

const
  SECBUFFER_VERSION = 0;
  {$EXTERNALSYM SECBUFFER_VERSION}

  SECBUFFER_EMPTY              = 0; // Undefined, replaced by provider
  {$EXTERNALSYM SECBUFFER_EMPTY}
  SECBUFFER_DATA               = 1; // Packet data
  {$EXTERNALSYM SECBUFFER_DATA}
  SECBUFFER_TOKEN              = 2; // Security token
  {$EXTERNALSYM SECBUFFER_TOKEN}
  SECBUFFER_PKG_PARAMS         = 3; // Package specific parameters
  {$EXTERNALSYM SECBUFFER_PKG_PARAMS}
  SECBUFFER_MISSING            = 4; // Missing Data indicator
  {$EXTERNALSYM SECBUFFER_MISSING}
  SECBUFFER_EXTRA              = 5; // Extra data
  {$EXTERNALSYM SECBUFFER_EXTRA}
  SECBUFFER_STREAM_TRAILER     = 6; // Security Trailer
  {$EXTERNALSYM SECBUFFER_STREAM_TRAILER}
  SECBUFFER_STREAM_HEADER      = 7; // Security Header
  {$EXTERNALSYM SECBUFFER_STREAM_HEADER}
  SECBUFFER_NEGOTIATION_INFO   = 8; // Hints from the negotiation pkg
  {$EXTERNALSYM SECBUFFER_NEGOTIATION_INFO}
  SECBUFFER_PADDING            = 9; // non-data padding
  {$EXTERNALSYM SECBUFFER_PADDING}
  SECBUFFER_STREAM             = 10; // whole encrypted message
  {$EXTERNALSYM SECBUFFER_STREAM}
  SECBUFFER_MECHLIST           = 11;
  {$EXTERNALSYM SECBUFFER_MECHLIST}
  SECBUFFER_MECHLIST_SIGNATURE = 12;
  {$EXTERNALSYM SECBUFFER_MECHLIST_SIGNATURE}
  SECBUFFER_TARGET             = 13;
  {$EXTERNALSYM SECBUFFER_TARGET}
  SECBUFFER_CHANNEL_BINDINGS   = 14;
  {$EXTERNALSYM SECBUFFER_CHANNEL_BINDINGS}

  SECBUFFER_ATTRMASK = DWORD($F0000000);
  {$EXTERNALSYM SECBUFFER_ATTRMASK}
  SECBUFFER_READONLY = DWORD($80000000); // Buffer is read-only
  {$EXTERNALSYM SECBUFFER_READONLY}
  SECBUFFER_READONLY_WITH_CHECKSUM = $10000000;  // Buffer is read-only, and checksummed
  {$EXTERNALSYM SECBUFFER_READONLY_WITH_CHECKSUM}
  SECBUFFER_RESERVED = DWORD($60000000); // Flags reserved to security system
  {$EXTERNALSYM SECBUFFER_RESERVED}

type
  PSEC_NEGOTIATION_INFO = ^SEC_NEGOTIATION_INFO;
  {$EXTERNALSYM PSEC_NEGOTIATION_INFO}
  _SEC_NEGOTIATION_INFO = record
    Size: Cardinal;       // Size of this structure
    NameLength: Cardinal; // Length of name hint
    Name: PSecWChar;     // Name hint
    Reserved: Pointer;    // Reserved
  end;
  {$EXTERNALSYM _SEC_NEGOTIATION_INFO}
  SEC_NEGOTIATION_INFO = _SEC_NEGOTIATION_INFO;
  {$EXTERNALSYM SEC_NEGOTIATION_INFO}
  TSecNegotiationInfo = SEC_NEGOTIATION_INFO;
  PSecNegotiationInfo = PSEC_NEGOTIATION_INFO;

  _SEC_CHANNEL_BINDINGS = record
    dwInitiatorAddrType: Cardinal;
    cbInitiatorLength: Cardinal;
    dwInitiatorOffset: Cardinal;
    dwAcceptorAddrType: Cardinal;
    cbAcceptorLength: Cardinal;
    dwAcceptorOffset: Cardinal;
    cbApplicationDataLength: Cardinal;
    dwApplicationDataOffset: Cardinal;
  end;
  {$EXTERNALSYM _SEC_CHANNEL_BINDINGS}
  SEC_CHANNEL_BINDINGS = _SEC_CHANNEL_BINDINGS;
  {$EXTERNALSYM SEC_CHANNEL_BINDINGS}
  PSEC_CHANNEL_BINDINGS = ^SEC_CHANNEL_BINDINGS;
  {$EXTERNALSYM PSEC_CHANNEL_BINDINGS}
  TSecChannelBindings = SEC_CHANNEL_BINDINGS;
  PSecChannelBindings = PSEC_CHANNEL_BINDINGS;  

//
//  Data Representation Constant:
//

const
  SECURITY_NATIVE_DREP  = $00000010;
  {$EXTERNALSYM SECURITY_NATIVE_DREP}
  SECURITY_NETWORK_DREP = $00000000;
  {$EXTERNALSYM SECURITY_NETWORK_DREP}

//
//  Credential Use Flags
//

  SECPKG_CRED_INBOUND  = $00000001;
  {$EXTERNALSYM SECPKG_CRED_INBOUND}
  SECPKG_CRED_OUTBOUND = $00000002;
  {$EXTERNALSYM SECPKG_CRED_OUTBOUND}
  SECPKG_CRED_BOTH     = $00000003;
  {$EXTERNALSYM SECPKG_CRED_BOTH}
  SECPKG_CRED_DEFAULT  = $00000004;
  {$EXTERNALSYM SECPKG_CRED_DEFAULT}
  SECPKG_CRED_RESERVED = DWORD($F0000000);
  {$EXTERNALSYM SECPKG_CRED_RESERVED}

//
//  InitializeSecurityContext Requirement and return flags:
//

  ISC_REQ_DELEGATE               = $00000001;
  {$EXTERNALSYM ISC_REQ_DELEGATE}
  ISC_REQ_MUTUAL_AUTH            = $00000002;
  {$EXTERNALSYM ISC_REQ_MUTUAL_AUTH}
  ISC_REQ_REPLAY_DETECT          = $00000004;
  {$EXTERNALSYM ISC_REQ_REPLAY_DETECT}
  ISC_REQ_SEQUENCE_DETECT        = $00000008;
  {$EXTERNALSYM ISC_REQ_SEQUENCE_DETECT}
  ISC_REQ_CONFIDENTIALITY        = $00000010;
  {$EXTERNALSYM ISC_REQ_CONFIDENTIALITY}
  ISC_REQ_USE_SESSION_KEY        = $00000020;
  {$EXTERNALSYM ISC_REQ_USE_SESSION_KEY}
  ISC_REQ_PROMPT_FOR_CREDS       = $00000040;
  {$EXTERNALSYM ISC_REQ_PROMPT_FOR_CREDS}
  ISC_REQ_USE_SUPPLIED_CREDS     = $00000080;
  {$EXTERNALSYM ISC_REQ_USE_SUPPLIED_CREDS}
  ISC_REQ_ALLOCATE_MEMORY        = $00000100;
  {$EXTERNALSYM ISC_REQ_ALLOCATE_MEMORY}
  ISC_REQ_USE_DCE_STYLE          = $00000200;
  {$EXTERNALSYM ISC_REQ_USE_DCE_STYLE}
  ISC_REQ_DATAGRAM               = $00000400;
  {$EXTERNALSYM ISC_REQ_DATAGRAM}
  ISC_REQ_CONNECTION             = $00000800;
  {$EXTERNALSYM ISC_REQ_CONNECTION}
  ISC_REQ_CALL_LEVEL             = $00001000;
  {$EXTERNALSYM ISC_REQ_CALL_LEVEL}
  ISC_REQ_FRAGMENT_SUPPLIED      = $00002000;
  {$EXTERNALSYM ISC_REQ_FRAGMENT_SUPPLIED}
  ISC_REQ_EXTENDED_ERROR         = $00004000;
  {$EXTERNALSYM ISC_REQ_EXTENDED_ERROR}
  ISC_REQ_STREAM                 = $00008000;
  {$EXTERNALSYM ISC_REQ_STREAM}
  ISC_REQ_INTEGRITY              = $00010000;
  {$EXTERNALSYM ISC_REQ_INTEGRITY}
  ISC_REQ_IDENTIFY               = $00020000;
  {$EXTERNALSYM ISC_REQ_IDENTIFY}
  ISC_REQ_NULL_SESSION           = $00040000;
  {$EXTERNALSYM ISC_REQ_NULL_SESSION}
  ISC_REQ_MANUAL_CRED_VALIDATION = $00080000;
  {$EXTERNALSYM ISC_REQ_MANUAL_CRED_VALIDATION}
  ISC_REQ_RESERVED1              = $00100000;
  {$EXTERNALSYM ISC_REQ_RESERVED1}
  ISC_REQ_FRAGMENT_TO_FIT        = $00200000;
  {$EXTERNALSYM ISC_REQ_FRAGMENT_TO_FIT}

  ISC_RET_DELEGATE               = $00000001;
  {$EXTERNALSYM ISC_RET_DELEGATE}
  ISC_RET_MUTUAL_AUTH            = $00000002;
  {$EXTERNALSYM ISC_RET_MUTUAL_AUTH}
  ISC_RET_REPLAY_DETECT          = $00000004;
  {$EXTERNALSYM ISC_RET_REPLAY_DETECT}
  ISC_RET_SEQUENCE_DETECT        = $00000008;
  {$EXTERNALSYM ISC_RET_SEQUENCE_DETECT}
  ISC_RET_CONFIDENTIALITY        = $00000010;
  {$EXTERNALSYM ISC_RET_CONFIDENTIALITY}
  ISC_RET_USE_SESSION_KEY        = $00000020;
  {$EXTERNALSYM ISC_RET_USE_SESSION_KEY}
  ISC_RET_USED_COLLECTED_CREDS   = $00000040;
  {$EXTERNALSYM ISC_RET_USED_COLLECTED_CREDS}
  ISC_RET_USED_SUPPLIED_CREDS    = $00000080;
  {$EXTERNALSYM ISC_RET_USED_SUPPLIED_CREDS}
  ISC_RET_ALLOCATED_MEMORY       = $00000100;
  {$EXTERNALSYM ISC_RET_ALLOCATED_MEMORY}
  ISC_RET_USED_DCE_STYLE         = $00000200;
  {$EXTERNALSYM ISC_RET_USED_DCE_STYLE}
  ISC_RET_DATAGRAM               = $00000400;
  {$EXTERNALSYM ISC_RET_DATAGRAM}
  ISC_RET_CONNECTION             = $00000800;
  {$EXTERNALSYM ISC_RET_CONNECTION}
  ISC_RET_INTERMEDIATE_RETURN    = $00001000;
  {$EXTERNALSYM ISC_RET_INTERMEDIATE_RETURN}
  ISC_RET_CALL_LEVEL             = $00002000;
  {$EXTERNALSYM ISC_RET_CALL_LEVEL}
  ISC_RET_EXTENDED_ERROR         = $00004000;
  {$EXTERNALSYM ISC_RET_EXTENDED_ERROR}
  ISC_RET_STREAM                 = $00008000;
  {$EXTERNALSYM ISC_RET_STREAM}
  ISC_RET_INTEGRITY              = $00010000;
  {$EXTERNALSYM ISC_RET_INTEGRITY}
  ISC_RET_IDENTIFY               = $00020000;
  {$EXTERNALSYM ISC_RET_IDENTIFY}
  ISC_RET_NULL_SESSION           = $00040000;
  {$EXTERNALSYM ISC_RET_NULL_SESSION}
  ISC_RET_MANUAL_CRED_VALIDATION = $00080000;
  {$EXTERNALSYM ISC_RET_MANUAL_CRED_VALIDATION}
  ISC_RET_RESERVED1              = $00100000;
  {$EXTERNALSYM ISC_RET_RESERVED1}
  ISC_RET_FRAGMENT_ONLY          = $00200000;
  {$EXTERNALSYM ISC_RET_FRAGMENT_ONLY}

  ASC_REQ_DELEGATE              = $00000001;
  {$EXTERNALSYM ASC_REQ_DELEGATE}
  ASC_REQ_MUTUAL_AUTH           = $00000002;
  {$EXTERNALSYM ASC_REQ_MUTUAL_AUTH}
  ASC_REQ_REPLAY_DETECT         = $00000004;
  {$EXTERNALSYM ASC_REQ_REPLAY_DETECT}
  ASC_REQ_SEQUENCE_DETECT       = $00000008;
  {$EXTERNALSYM ASC_REQ_SEQUENCE_DETECT}
  ASC_REQ_CONFIDENTIALITY       = $00000010;
  {$EXTERNALSYM ASC_REQ_CONFIDENTIALITY}
  ASC_REQ_USE_SESSION_KEY       = $00000020;
  {$EXTERNALSYM ASC_REQ_USE_SESSION_KEY}
  ASC_REQ_ALLOCATE_MEMORY       = $00000100;
  {$EXTERNALSYM ASC_REQ_ALLOCATE_MEMORY}
  ASC_REQ_USE_DCE_STYLE         = $00000200;
  {$EXTERNALSYM ASC_REQ_USE_DCE_STYLE}
  ASC_REQ_DATAGRAM              = $00000400;
  {$EXTERNALSYM ASC_REQ_DATAGRAM}
  ASC_REQ_CONNECTION            = $00000800;
  {$EXTERNALSYM ASC_REQ_CONNECTION}
  ASC_REQ_CALL_LEVEL            = $00001000;
  {$EXTERNALSYM ASC_REQ_CALL_LEVEL}
  ASC_REQ_EXTENDED_ERROR        = $00008000;
  {$EXTERNALSYM ASC_REQ_EXTENDED_ERROR}
  ASC_REQ_STREAM                = $00010000;
  {$EXTERNALSYM ASC_REQ_STREAM}
  ASC_REQ_INTEGRITY             = $00020000;
  {$EXTERNALSYM ASC_REQ_INTEGRITY}
  ASC_REQ_LICENSING             = $00040000;
  {$EXTERNALSYM ASC_REQ_LICENSING}
  ASC_REQ_IDENTIFY              = $00080000;
  {$EXTERNALSYM ASC_REQ_IDENTIFY}
  ASC_REQ_ALLOW_NULL_SESSION    = $00100000;
  {$EXTERNALSYM ASC_REQ_ALLOW_NULL_SESSION}
  ASC_REQ_ALLOW_NON_USER_LOGONS = $00200000;
  {$EXTERNALSYM ASC_REQ_ALLOW_NON_USER_LOGONS}
  ASC_REQ_ALLOW_CONTEXT_REPLAY  = $00400000;
  {$EXTERNALSYM ASC_REQ_ALLOW_CONTEXT_REPLAY}
  ASC_REQ_FRAGMENT_TO_FIT       = $00800000;
  {$EXTERNALSYM ASC_REQ_FRAGMENT_TO_FIT}
  ASC_REQ_FRAGMENT_SUPPLIED     = $00002000;
  {$EXTERNALSYM ASC_REQ_FRAGMENT_SUPPLIED}
  ASC_REQ_NO_TOKEN              = $01000000;
  {$EXTERNALSYM ASC_REQ_NO_TOKEN}

  ASC_RET_DELEGATE              = $00000001;
  {$EXTERNALSYM ASC_RET_DELEGATE}
  ASC_RET_MUTUAL_AUTH           = $00000002;
  {$EXTERNALSYM ASC_RET_MUTUAL_AUTH}
  ASC_RET_REPLAY_DETECT         = $00000004;
  {$EXTERNALSYM ASC_RET_REPLAY_DETECT}
  ASC_RET_SEQUENCE_DETECT       = $00000008;
  {$EXTERNALSYM ASC_RET_SEQUENCE_DETECT}
  ASC_RET_CONFIDENTIALITY       = $00000010;
  {$EXTERNALSYM ASC_RET_CONFIDENTIALITY}
  ASC_RET_USE_SESSION_KEY       = $00000020;
  {$EXTERNALSYM ASC_RET_USE_SESSION_KEY}
  ASC_RET_ALLOCATED_MEMORY      = $00000100;
  {$EXTERNALSYM ASC_RET_ALLOCATED_MEMORY}
  ASC_RET_USED_DCE_STYLE        = $00000200;
  {$EXTERNALSYM ASC_RET_USED_DCE_STYLE}
  ASC_RET_DATAGRAM              = $00000400;
  {$EXTERNALSYM ASC_RET_DATAGRAM}
  ASC_RET_CONNECTION            = $00000800;
  {$EXTERNALSYM ASC_RET_CONNECTION}
  ASC_RET_CALL_LEVEL            = $00002000; // skipped 1000 to be like ISC_
  {$EXTERNALSYM ASC_RET_CALL_LEVEL}
  ASC_RET_THIRD_LEG_FAILED      = $00004000;
  {$EXTERNALSYM ASC_RET_THIRD_LEG_FAILED}
  ASC_RET_EXTENDED_ERROR        = $00008000;
  {$EXTERNALSYM ASC_RET_EXTENDED_ERROR}
  ASC_RET_STREAM                = $00010000;
  {$EXTERNALSYM ASC_RET_STREAM}
  ASC_RET_INTEGRITY             = $00020000;
  {$EXTERNALSYM ASC_RET_INTEGRITY}
  ASC_RET_LICENSING             = $00040000;
  {$EXTERNALSYM ASC_RET_LICENSING}
  ASC_RET_IDENTIFY              = $00080000;
  {$EXTERNALSYM ASC_RET_IDENTIFY}
  ASC_RET_NULL_SESSION          = $00100000;
  {$EXTERNALSYM ASC_RET_NULL_SESSION}
  ASC_RET_ALLOW_NON_USER_LOGONS = $00200000;
  {$EXTERNALSYM ASC_RET_ALLOW_NON_USER_LOGONS}
  ASC_RET_ALLOW_CONTEXT_REPLAY  = $00400000;
  {$EXTERNALSYM ASC_RET_ALLOW_CONTEXT_REPLAY}
  ASC_RET_FRAGMENT_ONLY         = $00800000;
  {$EXTERNALSYM ASC_RET_FRAGMENT_ONLY}
  ASC_RET_NO_TOKEN              = $01000000;
  {$EXTERNALSYM ASC_RET_NO_TOKEN}

//
//  Security Credentials Attributes:
//

  SECPKG_CRED_ATTR_NAMES = 1;
  {$EXTERNALSYM SECPKG_CRED_ATTR_NAMES}

type
  PSecPkgCredentials_NamesW = ^SecPkgCredentials_NamesW;
  {$EXTERNALSYM PSecPkgCredentials_NamesW}
  _SecPkgCredentials_NamesW = record
    sUserName: PSecWChar;
  end;
  {$EXTERNALSYM _SecPkgCredentials_NamesW}
  SecPkgCredentials_NamesW = _SecPkgCredentials_NamesW;
  {$EXTERNALSYM SecPkgCredentials_NamesW}
  TSecPkgCredentialsNamesW = SecPkgCredentials_NamesW;
  PSecPkgCredentialsNamesW = PSecPkgCredentials_NamesW;

  PSecPkgCredentials_NamesA = ^SecPkgCredentials_NamesA;
  {$EXTERNALSYM PSecPkgCredentials_NamesA}
  _SecPkgCredentials_NamesA = record
    sUserName: PSecChar;
  end;
  {$EXTERNALSYM _SecPkgCredentials_NamesA}
  SecPkgCredentials_NamesA = _SecPkgCredentials_NamesA;
  {$EXTERNALSYM SecPkgCredentials_NamesA}
  TSecPkgCredentialsNamesA = SecPkgCredentials_NamesA;
  PSecPkgCredentialsNamesA = PSecPkgCredentials_NamesA;

  {$IFDEF UNICODE}
  SecPkgCredentials_Names = SecPkgCredentials_NamesW;
  {$EXTERNALSYM SecPkgCredentials_Names}
  PSecPkgCredentials_Names = PSecPkgCredentials_NamesW;
  {$EXTERNALSYM PSecPkgCredentials_Names}
  TSecPkgCredentialsNames = TSecPkgCredentialsNamesW;
  PSecPkgCredentialsNames = PSecPkgCredentialsNamesW;
  {$ELSE}
  SecPkgCredentials_Names = SecPkgCredentials_NamesA;
  {$EXTERNALSYM SecPkgCredentials_Names}
  PSecPkgCredentials_Names = PSecPkgCredentials_NamesA;
  {$EXTERNALSYM PSecPkgCredentials_Names}
  TSecPkgCredentialsNames = TSecPkgCredentialsNamesA;
  PSecPkgCredentialsNames = PSecPkgCredentialsNamesA;
  {$ENDIF UNICODE}

//
//  Security Context Attributes:
//

const
  SECPKG_ATTR_SIZES            = 0;
  {$EXTERNALSYM SECPKG_ATTR_SIZES}
  SECPKG_ATTR_NAMES            = 1;
  {$EXTERNALSYM SECPKG_ATTR_NAMES}
  SECPKG_ATTR_LIFESPAN         = 2;
  {$EXTERNALSYM SECPKG_ATTR_LIFESPAN}
  SECPKG_ATTR_DCE_INFO         = 3;
  {$EXTERNALSYM SECPKG_ATTR_DCE_INFO}
  SECPKG_ATTR_STREAM_SIZES     = 4;
  {$EXTERNALSYM SECPKG_ATTR_STREAM_SIZES}
  SECPKG_ATTR_KEY_INFO         = 5;
  {$EXTERNALSYM SECPKG_ATTR_KEY_INFO}
  SECPKG_ATTR_AUTHORITY        = 6;
  {$EXTERNALSYM SECPKG_ATTR_AUTHORITY}
  SECPKG_ATTR_PROTO_INFO       = 7;
  {$EXTERNALSYM SECPKG_ATTR_PROTO_INFO}
  SECPKG_ATTR_PASSWORD_EXPIRY  = 8;
  {$EXTERNALSYM SECPKG_ATTR_PASSWORD_EXPIRY}
  SECPKG_ATTR_SESSION_KEY      = 9;
  {$EXTERNALSYM SECPKG_ATTR_SESSION_KEY}
  SECPKG_ATTR_PACKAGE_INFO     = 10;
  {$EXTERNALSYM SECPKG_ATTR_PACKAGE_INFO}
  SECPKG_ATTR_USER_FLAGS       = 11;
  {$EXTERNALSYM SECPKG_ATTR_USER_FLAGS}
  SECPKG_ATTR_NEGOTIATION_INFO = 12;
  {$EXTERNALSYM SECPKG_ATTR_NEGOTIATION_INFO}
  SECPKG_ATTR_NATIVE_NAMES     = 13;
  {$EXTERNALSYM SECPKG_ATTR_NATIVE_NAMES}
  SECPKG_ATTR_FLAGS            = 14;
  {$EXTERNALSYM SECPKG_ATTR_FLAGS}
  SECPKG_ATTR_USE_VALIDATED    = 15;
  {$EXTERNALSYM SECPKG_ATTR_USE_VALIDATED}
  SECPKG_ATTR_CREDENTIAL_NAME  = 16;
  {$EXTERNALSYM SECPKG_ATTR_CREDENTIAL_NAME}
  SECPKG_ATTR_TARGET_INFORMATION = 17;
  {$EXTERNALSYM SECPKG_ATTR_TARGET_INFORMATION}
  SECPKG_ATTR_ACCESS_TOKEN     = 18;
  {$EXTERNALSYM SECPKG_ATTR_ACCESS_TOKEN}
  SECPKG_ATTR_TARGET           = 19;
  {$EXTERNALSYM SECPKG_ATTR_TARGET}
  SECPKG_ATTR_AUTHENTICATION_ID = 20;
  {$EXTERNALSYM SECPKG_ATTR_AUTHENTICATION_ID}

type
  PSecPkgContext_Sizes = ^SecPkgContext_Sizes;
  {$EXTERNALSYM PSecPkgContext_Sizes}
  _SecPkgContext_Sizes = record
    cbMaxToken: Cardinal;
    cbMaxSignature: Cardinal;
    cbBlockSize: Cardinal;
    cbSecurityTrailer: Cardinal;
  end;
  {$EXTERNALSYM _SecPkgContext_Sizes}
  SecPkgContext_Sizes = _SecPkgContext_Sizes;
  {$EXTERNALSYM SecPkgContext_Sizes}
  TSecPkgContextSizes = SecPkgContext_Sizes;
  PSecPkgContextSizes = PSecPkgContext_Sizes;

  PSecPkgContext_StreamSizes = ^SecPkgContext_StreamSizes;
  {$EXTERNALSYM PSecPkgContext_StreamSizes}
  _SecPkgContext_StreamSizes = record
    cbHeader: Cardinal;
    cbTrailer: Cardinal;
    cbMaximumMessage: Cardinal;
    cBuffers: Cardinal;
    cbBlockSize: Cardinal;
  end;
  {$EXTERNALSYM _SecPkgContext_StreamSizes}
  SecPkgContext_StreamSizes = _SecPkgContext_StreamSizes;
  {$EXTERNALSYM SecPkgContext_StreamSizes}
  TSecPkgContextStreamSizes = SecPkgContext_StreamSizes;
  PSecPkgContextStreamSizes = PSecPkgContext_StreamSizes;

  PSecPkgContext_NamesW = ^SecPkgContext_NamesW;
  {$EXTERNALSYM PSecPkgContext_NamesW}
  _SecPkgContext_NamesW = record
    sUserName: PSecWChar;
  end;
  {$EXTERNALSYM _SecPkgContext_NamesW}
  SecPkgContext_NamesW = _SecPkgContext_NamesW;
  {$EXTERNALSYM SecPkgContext_NamesW}
  TSecPkgContextNamesW = SecPkgContext_NamesW;
  PSecPkgContextNamesW = PSecPkgContext_NamesW;

  PSecPkgContext_NamesA = ^SecPkgContext_NamesA;
  {$EXTERNALSYM PSecPkgContext_NamesA}
  _SecPkgContext_NamesA = record
    sUserName: PSecChar;
  end;
  {$EXTERNALSYM _SecPkgContext_NamesA}
  SecPkgContext_NamesA = _SecPkgContext_NamesA;
  {$EXTERNALSYM SecPkgContext_NamesA}
  TSecPkgContextNamesA = SecPkgContext_NamesA;
  PSecPkgContextNamesA = PSecPkgContext_NamesA;

  {$IFDEF UNICODE}
  SecPkgContext_Names = SecPkgContext_NamesW;
  {$EXTERNALSYM SecPkgContext_Names}
  PSecPkgContext_Names = PSecPkgContext_NamesW;
  {$EXTERNALSYM PSecPkgContext_Names}
  TSecPkgContextNames = TSecPkgContextNamesW;
  PSecPkgContextNames = PSecPkgContextNamesW;
  {$ELSE}
  SecPkgContext_Names = SecPkgContext_NamesA;
  {$EXTERNALSYM SecPkgContext_Names}
  PSecPkgContext_Names = PSecPkgContext_NamesA;
  {$EXTERNALSYM PSecPkgContext_Names}
  TSecPkgContextNames = TSecPkgContextNamesA;
  PSecPkgContextNames = PSecPkgContextNamesA;
  {$ENDIF UNICODE}

  PSecPkgContext_LifeSpan = ^SecPkgContext_LifeSpan;
  {$EXTERNALSYM PSecPkgContext_LifeSpan}
  _SecPkgContext_Lifespan = record
    tsStart: TimeStamp;
    tsExpiry: TimeStamp;
  end;
  {$EXTERNALSYM _SecPkgContext_Lifespan}
  SecPkgContext_Lifespan = _SecPkgContext_Lifespan;
  {$EXTERNALSYM SecPkgContext_Lifespan}
  TSecPkgContextLifeSpan = SecPkgContext_Lifespan;
  PSecPkgContextLifeSpan = PSecPkgContext_LifeSpan;

  PSecPkgContext_DceInfo = ^SecPkgContext_DceInfo;
  {$EXTERNALSYM PSecPkgContext_DceInfo}
  _SecPkgContext_DceInfo = record
    AuthzSvc: Cardinal;
    pPac: Pointer;
  end;
  {$EXTERNALSYM _SecPkgContext_DceInfo}
  SecPkgContext_DceInfo = _SecPkgContext_DceInfo;
  {$EXTERNALSYM SecPkgContext_DceInfo}
  TSecPkgContextDceInfo = SecPkgContext_DceInfo;
  PSecPkgContextDceInfo = PSecPkgContext_DceInfo;

  PSecPkgContext_KeyInfoA = ^SecPkgContext_KeyInfoA;
  {$EXTERNALSYM PSecPkgContext_KeyInfoA}
  _SecPkgContext_KeyInfoA = record
    sSignatureAlgorithmName: PSecChar;
    sEncryptAlgorithmName: PSecChar;
    KeySize: Cardinal;
    SignatureAlgorithm: Cardinal;
    EncryptAlgorithm: Cardinal;
  end;
  {$EXTERNALSYM _SecPkgContext_KeyInfoA}
  SecPkgContext_KeyInfoA = _SecPkgContext_KeyInfoA;
  {$EXTERNALSYM SecPkgContext_KeyInfoA}
  TSecPkgContextKeyInfoA = SecPkgContext_KeyInfoA;
  PSecPkgContextKeyInfoA = PSecPkgContext_KeyInfoA;

  PSecPkgContext_KeyInfoW = ^SecPkgContext_KeyInfoW;
  {$EXTERNALSYM PSecPkgContext_KeyInfoW}
  _SecPkgContext_KeyInfoW = record
    sSignatureAlgorithmName: PSecWChar;
    sEncryptAlgorithmName: PSecWChar;
    KeySize: Cardinal;
    SignatureAlgorithm: Cardinal;
    EncryptAlgorithm: Cardinal;
  end;
  {$EXTERNALSYM _SecPkgContext_KeyInfoW}
  SecPkgContext_KeyInfoW = _SecPkgContext_KeyInfoW;
  {$EXTERNALSYM SecPkgContext_KeyInfoW}
  TSecPkgContextKeyInfoW = SecPkgContext_KeyInfoW;
  PSecPkgContextKeyInfoW = PSecPkgContext_KeyInfoW;

  {$IFDEF UNICODE}
  SecPkgContext_KeyInfo  = SecPkgContext_KeyInfoW;
  {$EXTERNALSYM SecPkgContext_KeyInfo}
  PSecPkgContext_KeyInfo = PSecPkgContext_KeyInfoW;
  {$EXTERNALSYM PSecPkgContext_KeyInfo}
  TSecPkgContextKeyInfo = TSecPkgContextKeyInfoW;
  PSecPkgContextKeyInfo = PSecPkgContextKeyInfoW;
  {$ELSE}
  SecPkgContext_KeyInfo  = SecPkgContext_KeyInfoA;
  {$EXTERNALSYM SecPkgContext_KeyInfo}
  PSecPkgContext_KeyInfo = PSecPkgContext_KeyInfoA;
  {$EXTERNALSYM PSecPkgContext_KeyInfo}
  TSecPkgContextKeyInfo = TSecPkgContextKeyInfoA;
  PSecPkgContextKeyInfo = PSecPkgContextKeyInfoA;
  {$ENDIF UNICODE}

  PSecPkgContext_AuthorityA = ^SecPkgContext_AuthorityA;
  {$EXTERNALSYM PSecPkgContext_AuthorityA}
  _SecPkgContext_AuthorityA = record
    sAuthorityName: PSecChar;
  end;
  {$EXTERNALSYM _SecPkgContext_AuthorityA}
  SecPkgContext_AuthorityA = _SecPkgContext_AuthorityA;
  {$EXTERNALSYM SecPkgContext_AuthorityA}
  TSecPkgContextAuthorityA = SecPkgContext_AuthorityA;
  PSecPkgContextAuthorityA = PSecPkgContext_AuthorityA;

  PSecPkgContext_AuthorityW = ^SecPkgContext_AuthorityW;
  {$EXTERNALSYM PSecPkgContext_AuthorityW}
  _SecPkgContext_AuthorityW = record
    sAuthorityName: PSecWChar;
  end;
  {$EXTERNALSYM _SecPkgContext_AuthorityW}
  SecPkgContext_AuthorityW = _SecPkgContext_AuthorityW;
  {$EXTERNALSYM SecPkgContext_AuthorityW}
  TSecPkgContextAuthorityW = SecPkgContext_AuthorityW;
  PSecPkgContextAuthorityW = PSecPkgContext_AuthorityW;

  {$IFDEF UNICODE}
  SecPkgContext_Authority  = SecPkgContext_AuthorityW;
  {$EXTERNALSYM SecPkgContext_Authority}
  PSecPkgContext_Authority = PSecPkgContext_AuthorityW;
  {$EXTERNALSYM PSecPkgContext_Authority}
  TSecPkgContextAuthority = TSecPkgContextAuthorityW;
  PSecPkgContextAuthority = PSecPkgContextAuthorityW;
  {$ELSE}
  SecPkgContext_Authority  = SecPkgContext_AuthorityA;
  {$EXTERNALSYM SecPkgContext_Authority}
  PSecPkgContext_Authority = PSecPkgContext_AuthorityA;
  {$EXTERNALSYM PSecPkgContext_Authority}
  TSecPkgContextAuthority = SecPkgContext_AuthorityA;
  PSecPkgContextAuthority = PSecPkgContext_AuthorityA;
  {$ENDIF UNICODE}

  PSecPkgContext_ProtoInfoA = ^SecPkgContext_ProtoInfoA;
  {$EXTERNALSYM PSecPkgContext_ProtoInfoA}
  _SecPkgContext_ProtoInfoA = record
    sProtocolName: PSecChar;
    majorVersion: Cardinal;
    minorVersion: Cardinal;
  end;
  {$EXTERNALSYM _SecPkgContext_ProtoInfoA}
  SecPkgContext_ProtoInfoA = _SecPkgContext_ProtoInfoA;
  {$EXTERNALSYM SecPkgContext_ProtoInfoA}
  TSecPkgContextProtoInfoA = SecPkgContext_ProtoInfoA;
  PSecPkgContextProtoInfoA = PSecPkgContext_ProtoInfoA;

  PSecPkgContext_ProtoInfoW = ^SecPkgContext_ProtoInfoW;
  {$EXTERNALSYM PSecPkgContext_ProtoInfoW}
  _SecPkgContext_ProtoInfoW = record
    sProtocolName: PSecWChar;
    majorVersion: Cardinal;
    minorVersion: Cardinal;
  end;
  {$EXTERNALSYM _SecPkgContext_ProtoInfoW}
  SecPkgContext_ProtoInfoW = _SecPkgContext_ProtoInfoW;
  {$EXTERNALSYM SecPkgContext_ProtoInfoW}
  TSecPkgContextProtoInfoW = SecPkgContext_ProtoInfoW;
  PSecPkgContextProtoInfoW = PSecPkgContext_ProtoInfoW;

  {$IFDEF UNICODE}
  SecPkgContext_ProtoInfo  = SecPkgContext_ProtoInfoW;
  {$EXTERNALSYM SecPkgContext_ProtoInfo}
  PSecPkgContext_ProtoInfo = PSecPkgContext_ProtoInfoW;
  {$EXTERNALSYM PSecPkgContext_ProtoInfo}
  TSecPkgContextProtoInfo = TSecPkgContextProtoInfoW;
  PSecPkgContextProtoInfo = PSecPkgContextProtoInfoW;
  {$ELSE}
  SecPkgContext_ProtoInfo  = SecPkgContext_ProtoInfoA;
  {$EXTERNALSYM SecPkgContext_ProtoInfo}
  PSecPkgContext_ProtoInfo = PSecPkgContext_ProtoInfoA;
  {$EXTERNALSYM PSecPkgContext_ProtoInfo}
  TSecPkgContextProtoInfo = TSecPkgContextProtoInfoA;
  PSecPkgContextProtoInfo = PSecPkgContextProtoInfoA;
  {$ENDIF UNICODE}

  PSecPkgContext_PasswordExpiry = ^SecPkgContext_PasswordExpiry;
  {$EXTERNALSYM PSecPkgContext_PasswordExpiry}
  _SecPkgContext_PasswordExpiry = record
    tsPasswordExpires: TimeStamp;
  end;
  {$EXTERNALSYM _SecPkgContext_PasswordExpiry}
  SecPkgContext_PasswordExpiry = _SecPkgContext_PasswordExpiry;
  {$EXTERNALSYM SecPkgContext_PasswordExpiry}
  TSecPkgContextPasswordExpiry = SecPkgContext_PasswordExpiry;
  PSecPkgContextPasswordExpiry = PSecPkgContext_PasswordExpiry;

  PSecPkgContext_SessionKey = ^SecPkgContext_SessionKey;
  {$EXTERNALSYM PSecPkgContext_SessionKey}
  _SecPkgContext_SessionKey = record
    SessionKeyLength: Cardinal;
    SessionKey: PByte;
  end;
  {$EXTERNALSYM _SecPkgContext_SessionKey}
  SecPkgContext_SessionKey = _SecPkgContext_SessionKey;
  {$EXTERNALSYM SecPkgContext_SessionKey}
  TSecPkgContextSessionKey = SecPkgContext_SessionKey;
  PSecPkgContextSessionKey = PSecPkgContext_SessionKey;

  PSecPkgContext_PackageInfoW = ^SecPkgContext_PackageInfoW;
  {$EXTERNALSYM PSecPkgContext_PackageInfoW}
  _SecPkgContext_PackageInfoW = record
    PackageInfo: PSecPkgInfoW;
  end;
  {$EXTERNALSYM _SecPkgContext_PackageInfoW}
  SecPkgContext_PackageInfoW = _SecPkgContext_PackageInfoW;
  {$EXTERNALSYM SecPkgContext_PackageInfoW}
  TSecPkgContextPackageInfoW = SecPkgContext_PackageInfoW;
  PSecPkgContextPackageInfoW = PSecPkgContext_PackageInfoW;

  PSecPkgContext_PackageInfoA = ^SecPkgContext_PackageInfoA;
  {$EXTERNALSYM PSecPkgContext_PackageInfoA}
  _SecPkgContext_PackageInfoA = record
    PackageInfo: PSecPkgInfoA;
  end;
  {$EXTERNALSYM _SecPkgContext_PackageInfoA}
  SecPkgContext_PackageInfoA = _SecPkgContext_PackageInfoA;
  {$EXTERNALSYM SecPkgContext_PackageInfoA}
  TSecPkgContextPackageInfoA = SecPkgContext_PackageInfoA;
  PSecPkgContextPackageInfoA = PSecPkgContext_PackageInfoA;

  PSecPkgContext_UserFlags = ^SecPkgContext_UserFlags;
  {$EXTERNALSYM PSecPkgContext_UserFlags}
  _SecPkgContext_UserFlags = record
    UserFlags: Cardinal;
  end;
  {$EXTERNALSYM _SecPkgContext_UserFlags}
  SecPkgContext_UserFlags = _SecPkgContext_UserFlags;
  {$EXTERNALSYM SecPkgContext_UserFlags}
  TSecPkgContextUserFlags = SecPkgContext_UserFlags;
  PSecPkgContextUserFlags = PSecPkgContext_UserFlags;

  PSecPkgContext_Flags = ^SecPkgContext_Flags;
  {$EXTERNALSYM PSecPkgContext_Flags}
  _SecPkgContext_Flags = record
    Flags: Cardinal;
  end;
  {$EXTERNALSYM _SecPkgContext_Flags}
  SecPkgContext_Flags = _SecPkgContext_Flags;
  {$EXTERNALSYM SecPkgContext_Flags}
  TSecPkgContextFlags = SecPkgContext_Flags;
  PSecPkgContextFlags = PSecPkgContext_Flags;

  {$IFDEF UNICODE}
  SecPkgContext_PackageInfo  = SecPkgContext_PackageInfoW;
  {$EXTERNALSYM SecPkgContext_PackageInfo}
  PSecPkgContext_PackageInfo = PSecPkgContext_PackageInfoW;
  {$EXTERNALSYM PSecPkgContext_PackageInfo}
  TSecPkgContextPackageInfo = TSecPkgContextPackageInfoW;
  PSecPkgContextPackageInfo = PSecPkgContextPackageInfoW;
  {$ELSE}
  SecPkgContext_PackageInfo  = SecPkgContext_PackageInfoA;
  {$EXTERNALSYM SecPkgContext_PackageInfo}
  PSecPkgContext_PackageInfo = PSecPkgContext_PackageInfoA;
  {$EXTERNALSYM PSecPkgContext_PackageInfo}
  TSecPkgContextPackageInfo = TSecPkgContextPackageInfoA;
  PSecPkgContextPackageInfo = PSecPkgContextPackageInfoA;
  {$ENDIF UNICODE}

  PSecPkgContext_NegotiationInfoA = ^SecPkgContext_NegotiationInfoA;
  {$EXTERNALSYM PSecPkgContext_NegotiationInfoA}
  _SecPkgContext_NegotiationInfoA = record
    PackageInfo: PSecPkgInfoA;
    NegotiationState: Cardinal;
  end;
  {$EXTERNALSYM _SecPkgContext_NegotiationInfoA}
  SecPkgContext_NegotiationInfoA = _SecPkgContext_NegotiationInfoA;
  {$EXTERNALSYM SecPkgContext_NegotiationInfoA}
  TSecPkgContextNegotiationInfoA = SecPkgContext_NegotiationInfoA;
  PSecPkgContextNegotiationInfoA = PSecPkgContext_NegotiationInfoA;

  PSecPkgContext_NegotiationInfoW = ^SecPkgContext_NegotiationInfoW;
  {$EXTERNALSYM PSecPkgContext_NegotiationInfoW}
  _SecPkgContext_NegotiationInfoW = record
    PackageInfo: PSecPkgInfoW;
    NegotiationState: Cardinal;
  end;
  {$EXTERNALSYM _SecPkgContext_NegotiationInfoW}
  SecPkgContext_NegotiationInfoW = _SecPkgContext_NegotiationInfoW;
  {$EXTERNALSYM SecPkgContext_NegotiationInfoW}
  TSecPkgContextNegotiationInfoW = SecPkgContext_NegotiationInfoW;
  PSecPkgContextNegotiationInfoW = PSecPkgContext_NegotiationInfoW ;

  {$IFDEF UNICODE}
  SecPkgContext_NegotiationInfo  = SecPkgContext_NegotiationInfoW;
  {$EXTERNALSYM SecPkgContext_NegotiationInfo}
  PSecPkgContext_NegotiationInfo = PSecPkgContext_NegotiationInfoW;
  {$EXTERNALSYM PSecPkgContext_NegotiationInfo}
  TSecPkgContextNegotiationInfo = TSecPkgContextNegotiationInfoW;
  PSecPkgContextNegotiationInfo = PSecPkgContextNegotiationInfoW;
  {$ELSE}
  SecPkgContext_NegotiationInfo  = SecPkgContext_NegotiationInfoA;
  {$EXTERNALSYM SecPkgContext_NegotiationInfo}
  PSecPkgContext_NegotiationInfo = PSecPkgContext_NegotiationInfoA;
  {$EXTERNALSYM PSecPkgContext_NegotiationInfo}
  TSecPkgContextNegotiationInfo = TSecPkgContextNegotiationInfoA;
  PSecPkgContextNegotiationInfo = PSecPkgContextNegotiationInfoA;
  {$ENDIF UNICODE}

const
  SECPKG_NEGOTIATION_COMPLETE    = 0;
  {$EXTERNALSYM SECPKG_NEGOTIATION_COMPLETE}
  SECPKG_NEGOTIATION_OPTIMISTIC  = 1;
  {$EXTERNALSYM SECPKG_NEGOTIATION_OPTIMISTIC}
  SECPKG_NEGOTIATION_IN_PROGRESS = 2;
  {$EXTERNALSYM SECPKG_NEGOTIATION_IN_PROGRESS}
  SECPKG_NEGOTIATION_DIRECT      = 3;
  {$EXTERNALSYM SECPKG_NEGOTIATION_DIRECT}
  SECPKG_NEGOTIATION_TRY_MULTICRED = 4;
  {$EXTERNALSYM SECPKG_NEGOTIATION_TRY_MULTICRED}

type
  PSecPkgContext_NativeNamesW = ^SecPkgContext_NativeNamesW;
  {$EXTERNALSYM PSecPkgContext_NativeNamesW}
  _SecPkgContext_NativeNamesW = record
    sClientName: PSecWChar;
    sServerName: PSecWChar;
  end;
  {$EXTERNALSYM _SecPkgContext_NativeNamesW}
  SecPkgContext_NativeNamesW = _SecPkgContext_NativeNamesW;
  {$EXTERNALSYM SecPkgContext_NativeNamesW}
  TSecPkgContextNativeNamesW = SecPkgContext_NativeNamesW;
  PSecPkgContextNativeNamesW = PSecPkgContext_NativeNamesW;

  PSecPkgContext_NativeNamesA = ^SecPkgContext_NativeNamesA;
  {$EXTERNALSYM PSecPkgContext_NativeNamesA}
  _SecPkgContext_NativeNamesA = record
    sClientName: PSecChar;
    sServerName: PSecChar;
  end;
  {$EXTERNALSYM _SecPkgContext_NativeNamesA}
  SecPkgContext_NativeNamesA = _SecPkgContext_NativeNamesA;
  {$EXTERNALSYM SecPkgContext_NativeNamesA}
  TSecPkgContextNativeNamesA = SecPkgContext_NativeNamesA;
  PSecPkgContextNativeNamesA = PSecPkgContext_NativeNamesA;

  {$IFDEF UNICODE}
  SecPkgContext_NativeNames  = SecPkgContext_NativeNamesW;
  {$EXTERNALSYM SecPkgContext_NativeNames}
  PSecPkgContext_NativeNames = PSecPkgContext_NativeNamesW;
  {$EXTERNALSYM PSecPkgContext_NativeNames}
  TSecPkgContextNativeNames = TSecPkgContextNativeNamesW;
  PSecPkgContextNativeNames = PSecPkgContextNativeNamesW;
  {$ELSE}
  SecPkgContext_NativeNames  = SecPkgContext_NativeNamesA;
  {$EXTERNALSYM SecPkgContext_NativeNames}
  PSecPkgContext_NativeNames = PSecPkgContext_NativeNamesA;
  {$EXTERNALSYM PSecPkgContext_NativeNames}
  TSecPkgContextNativeNames = TSecPkgContextNativeNamesA;
  PSecPkgContextNativeNames = PSecPkgContextNativeNamesA;
  {$ENDIF UNICODE}

  _SecPkgContext_CredentialNameW = record
    CredentialType: Cardinal;
    sCredentialName: PSEC_WCHAR;
  end;
  {$EXTERNALSYM _SecPkgContext_CredentialNameW}
  SecPkgContext_CredentialNameW = _SecPkgContext_CredentialNameW;
  {$EXTERNALSYM SecPkgContext_CredentialNameW}
  PSecPkgContext_CredentialNameW = ^SecPkgContext_CredentialNameW;
  {$EXTERNALSYM PSecPkgContext_CredentialNameW}
  TSecPkgContextCredentialNameW = SecPkgContext_CredentialNameW;
  PSecPkgContextCredentialNameW = PSecPkgContext_CredentialNameW;

  _SecPkgContext_CredentialNameA = record
    CredentialType: Cardinal;
    sCredentialName: PSEC_CHAR;
  end;
  {$EXTERNALSYM _SecPkgContext_CredentialNameA}
  SecPkgContext_CredentialNameA = _SecPkgContext_CredentialNameA;
  {$EXTERNALSYM SecPkgContext_CredentialNameA}
  PSecPkgContext_CredentialNameA = ^SecPkgContext_CredentialNameA;
  {$EXTERNALSYM PSecPkgContext_CredentialNameA}
  TSecPkgContextCredentialNameA = SecPkgContext_CredentialNameA;
  PSecPkgContextCredentialNameA = PSecPkgContext_CredentialNameA;

  {$IFDEF UNICODE}
  SecPkgContext_CredentialName = SecPkgContext_CredentialNameW;
  {$EXTERNALSYM SecPkgContext_CredentialName}
  PSecPkgContext_CredentialName = PSecPkgContext_CredentialNameW;
  {$EXTERNALSYM PSecPkgContext_CredentialName}
  TSecPkgContextCredentialName = TSecPkgContextCredentialNameW;
  PSecPkgContextCredentialName = PSecPkgContextCredentialNameW;
  {$ELSE}
  SecPkgContext_CredentialName = SecPkgContext_CredentialNameA;
  {$EXTERNALSYM SecPkgContext_CredentialName}
  PSecPkgContext_CredentialName = PSecPkgContext_CredentialNameA;
  {$EXTERNALSYM PSecPkgContext_CredentialName}
  TSecPkgContextCredentialName = TSecPkgContextCredentialNameA;
  PSecPkgContextCredentialName = PSecPkgContextCredentialNameA;
  {$ENDIF UNICODE}

  _SecPkgContext_AccessToken = record
    AccessToken: Pointer;
  end;
  {$EXTERNALSYM _SecPkgContext_AccessToken}
  SecPkgContext_AccessToken = _SecPkgContext_AccessToken;
  {$EXTERNALSYM SecPkgContext_AccessToken}
  PSecPkgContext_AccessToken = ^SecPkgContext_AccessToken;
  {$EXTERNALSYM PSecPkgContext_AccessToken}
  TSecPkgContextAccessToken = SecPkgContext_AccessToken;
  PSecPkgContextAccessToken = PSecPkgContext_AccessToken;

  _SecPkgContext_TargetInformation = record
    MarshalledTargetInfoLength: Cardinal;
    MarshalledTargetInfo: PWideChar;
  end;
  {$EXTERNALSYM _SecPkgContext_TargetInformation}
  SecPkgContext_TargetInformation = _SecPkgContext_TargetInformation;
  {$EXTERNALSYM SecPkgContext_TargetInformation}
  PSecPkgContext_TargetInformation = ^SecPkgContext_TargetInformation;
  {$EXTERNALSYM PSecPkgContext_TargetInformation}
  TSecPkgContextTargetInformation = SecPkgContext_TargetInformation;
  PSecPkgContextTargetInformation = PSecPkgContext_TargetInformation;

  _SecPkgContext_AuthzID = record
    AuthzIDLength: Cardinal;
    AuthzID: PAnsiChar;
  end;
  {$EXTERNALSYM _SecPkgContext_AuthzID}
  SecPkgContext_AuthzID = _SecPkgContext_AuthzID;
  {$EXTERNALSYM SecPkgContext_AuthzID}
  PSecPkgContext_AuthzID = ^SecPkgContext_AuthzID;
  {$EXTERNALSYM PSecPkgContext_AuthzID}
  TSecPkgContextAuthzID = SecPkgContext_AuthzID;
  PSecPkgContextAuthzID = PSecPkgContext_AuthzID;  

  _SecPkgContext_Target = record
    TargetLength: Cardinal;
    Target: PAnsiChar;
  end;
  {$EXTERNALSYM _SecPkgContext_Target}
  SecPkgContext_Target = _SecPkgContext_Target;
  {$EXTERNALSYM SecPkgContext_Target}
  PSecPkgContext_Target = ^SecPkgContext_Target;
  {$EXTERNALSYM PSecPkgContext_Target}
  TSecPkgContextTarget = SecPkgContext_Target;
  PSecPkgContextTarget = PSecPkgContext_Target;  

  SEC_GET_KEY_FN = procedure(
    Arg: Pointer;                           // Argument passed in
    Principal: Pointer;                     // Principal ID
    KeyVer: Cardinal;                       // Key Version
    var Key: Pointer;                       // Returned ptr to key
    var Status: SECURITY_STATUS); stdcall;  // returned status
  {$EXTERNALSYM SEC_GET_KEY_FN}
  TSecGetKeyFn = SEC_GET_KEY_FN;

//
// Flags for ExportSecurityContext
//

const
  SECPKG_CONTEXT_EXPORT_RESET_NEW  = $00000001; // New context is reset to initial state
  {$EXTERNALSYM SECPKG_CONTEXT_EXPORT_RESET_NEW}
  SECPKG_CONTEXT_EXPORT_DELETE_OLD = $00000002; // Old context is deleted during export
  {$EXTERNALSYM SECPKG_CONTEXT_EXPORT_DELETE_OLD}

function AcquireCredentialsHandleW(pszPrincipal, pszPackage: PSecWChar;
  fCredentialUse: Cardinal; pvLogonId, pAuthData: Pointer;
  pGetKeyFn: SEC_GET_KEY_FN; pvGetKeyArgument: Pointer; phCredential: PCredHandle;
  var ptsExpiry: TTimeStamp): SECURITY_STATUS; stdcall;
{$EXTERNALSYM AcquireCredentialsHandleW}

type
  ACQUIRE_CREDENTIALS_HANDLE_FN_W = function(
    pszPrincipal: PSecWChar;
    pszPackage: PSecWChar;
    fCredentialsUse: Cardinal;
    pvLogonId: Pointer;
    pAuthData: Pointer;
    pGetKeyFn: SEC_GET_KEY_FN;
    pvGetKeyArgument: Pointer;
    phCredential: PCredHandle;
    ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM ACQUIRE_CREDENTIALS_HANDLE_FN_W}
  TAcquireCredentialsHandleFnW = ACQUIRE_CREDENTIALS_HANDLE_FN_W;

function AcquireCredentialsHandleA(pszPrincipal, pszPackage: PSecChar;
  fCredentialUse: Cardinal; pvLogonId, pAuthData: Pointer;
  pGetKeyFn: SEC_GET_KEY_FN; pvGetKeyArgument: Pointer; phCredential: PCredHandle;
  var ptsExpiry: TTimeStamp): SECURITY_STATUS; stdcall;
{$EXTERNALSYM AcquireCredentialsHandleA}

type
  ACQUIRE_CREDENTIALS_HANDLE_FN_A = function(
    pszPrincipal: PSecChar;
    pszPackage: PSecChar;
    fCredentialsUse: Cardinal;
    pvLogonId: Pointer;
    pAuthData: Pointer;
    pGetKeyFn: SEC_GET_KEY_FN;
    pvGetKeyArgument: Pointer;
    phCredential: PCredHandle;
    ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM ACQUIRE_CREDENTIALS_HANDLE_FN_A}
  TAcquireCredentialsHandleFnA = ACQUIRE_CREDENTIALS_HANDLE_FN_A;

{$IFDEF UNICODE}
function AcquireCredentialsHandle(pszPrincipal, pszPackage: PSecWChar;
  fCredentialUse: Cardinal; pvLogonId, pAuthData: Pointer;
  pGetKeyFn: SEC_GET_KEY_FN; pvGetKeyArgument: Pointer; phCredential: PCredHandle;
  var ptsExpiry: TTimeStamp): SECURITY_STATUS; stdcall;
{$EXTERNALSYM AcquireCredentialsHandle}
type
  ACQUIRE_CREDENTIALS_HANDLE_FN = ACQUIRE_CREDENTIALS_HANDLE_FN_W;
  {$EXTERNALSYM ACQUIRE_CREDENTIALS_HANDLE_FN}
  TAcquireCredentialsHandleFn = TAcquireCredentialsHandleFnW;
{$ELSE}
function AcquireCredentialsHandle(pszPrincipal, pszPackage: PSecChar;
  fCredentialUse: Cardinal; pvLogonId, pAuthData: Pointer;
  pGetKeyFn: SEC_GET_KEY_FN; pvGetKeyArgument: Pointer; phCredential: PCredHandle;
  var ptsExpiry: TTimeStamp): SECURITY_STATUS; stdcall;
{$EXTERNALSYM AcquireCredentialsHandle}
type
  ACQUIRE_CREDENTIALS_HANDLE_FN = ACQUIRE_CREDENTIALS_HANDLE_FN_A;
  {$EXTERNALSYM ACQUIRE_CREDENTIALS_HANDLE_FN}
  TAcquireCredentialsHandleFn = TAcquireCredentialsHandleFnA;
{$ENDIF UNICODE}

function FreeCredentialsHandle(phCredential: PCredHandle): SECURITY_STATUS; stdcall;
{$EXTERNALSYM FreeCredentialsHandle}

type
  FREE_CREDENTIALS_HANDLE_FN = function(phCredential: PCredHandle): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM FREE_CREDENTIALS_HANDLE_FN}

function AddCredentialsW(hCredentials: PCredHandle; pszPrincipal: PSecWChar;
  pszPackage: PSecWChar; fCredentialUse: Cardinal; pAuthData: Pointer;
  pGetKeyFn: SEC_GET_KEY_FN; pvGetKeyArgument: Pointer;
  ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;
{$EXTERNALSYM AddCredentialsW}

type
  ADD_CREDENTIALS_FN_W = function(hCredentials: PCredHandle; pszPrincipal: PSecWChar;
    pszPackage: PSecWChar; fCredentialUse: Cardinal; pAuthData: Pointer;
    pGetKeyFn: SEC_GET_KEY_FN; pvGetKeyArgument: Pointer;
    ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM ADD_CREDENTIALS_FN_W}

function AddCredentialsA(hCredentials: PCredHandle; pszPrincipal: PSecChar;
  pszPackage: PSecChar; fCredentialUse: Cardinal; pAuthData: Pointer;
  pGetKeyFn: SEC_GET_KEY_FN; pvGetKeyArgument: Pointer; ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;
{$EXTERNALSYM AddCredentialsA}

type
  ADD_CREDENTIALS_FN_A = function(hCredentials: PCredHandle; pszPrincipal: PSecChar;
    pszPackage: PSecChar; fCredentialUse: Cardinal; pAuthData: Pointer;
    pGetKeyFn: SEC_GET_KEY_FN; pvGetKeyArgument: Pointer; ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM ADD_CREDENTIALS_FN_A}

{$IFDEF UNICODE}
function AddCredentials(hCredentials: PCredHandle; pszPrincipal: PSecWChar;
  pszPackage: PSecWChar; fCredentialUse: Cardinal; pAuthData: Pointer;
  pGetKeyFn: SEC_GET_KEY_FN; pvGetKeyArgument: Pointer;
  ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;
{$EXTERNALSYM AddCredentials}

type
  ADD_CREDENTIALS_FN = function(hCredentials: PCredHandle; pszPrincipal: PSecWChar;
    pszPackage: PSecWChar; fCredentialUse: Cardinal; pAuthData: Pointer;
    pGetKeyFn: SEC_GET_KEY_FN; pvGetKeyArgument: Pointer;
    ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM ADD_CREDENTIALS_FN}
{$ELSE}
function AddCredentials(hCredentials: PCredHandle; pszPrincipal: PSecChar;
  pszPackage: PSecChar; fCredentialUse: Cardinal; pAuthData: Pointer;
  pGetKeyFn: SEC_GET_KEY_FN; pvGetKeyArgument: Pointer; ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;
{$EXTERNALSYM AddCredentials}

type
  ADD_CREDENTIALS_FN = function(hCredentials: PCredHandle; pszPrincipal: PSecChar;
    pszPackage: PSecChar; fCredentialUse: Cardinal; pAuthData: Pointer;
    pGetKeyFn: SEC_GET_KEY_FN; pvGetKeyArgument: Pointer; ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM ADD_CREDENTIALS_FN}
{$ENDIF UNICODE}

////////////////////////////////////////////////////////////////////////
///
/// Context Management Functions
///
////////////////////////////////////////////////////////////////////////

function InitializeSecurityContextW(phCredential: PCredHandle; phContext: PCtxtHandle;
  pszTargetName: PSecWChar; fContextReq, Reserved1, TargetDataRep: Cardinal;
  pInput: PSecBufferDesc; Reserved2: Cardinal; phNewContext: PCtxtHandle;
  pOutput: PSecBufferDesc; var pfContextAttr: Cardinal; ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;
{$EXTERNALSYM InitializeSecurityContextW}

type
  INITIALIZE_SECURITY_CONTEXT_FN_W = function(phCredential: PCredHandle; phContext: PCtxtHandle;
    pszTargetName: PSecWChar; fContextReq, Reserved1, TargetDataRep: Cardinal;
    pInput: PSecBufferDesc; Reserved2: Cardinal; phNewContext: PCtxtHandle;
    pOutput: PSecBufferDesc; var pfContextAttr: Cardinal; ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM INITIALIZE_SECURITY_CONTEXT_FN_W}

function InitializeSecurityContextA(phCredential: PCredHandle; phContext: PCtxtHandle;
  pszTargetName: PSecChar; fContextReq, Reserved1, TargetDataRep: Cardinal;
  pInput: PSecBufferDesc; Reserved2: Cardinal; phNewContext: PCtxtHandle;
  pOutput: PSecBufferDesc; var pfContextAttr: Cardinal; ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;
{$EXTERNALSYM InitializeSecurityContextA}

type
  INITIALIZE_SECURITY_CONTEXT_FN_A = function(phCredential: PCredHandle; phContext: PCtxtHandle;
    pszTargetName: PSecChar; fContextReq, Reserved1, TargetDataRep: Cardinal;
    pInput: PSecBufferDesc; Reserved2: Cardinal; phNewContext: PCtxtHandle;
    pOutput: PSecBufferDesc; var pfContextAttr: Cardinal; ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM INITIALIZE_SECURITY_CONTEXT_FN_A}

{$IFDEF UNICODE}
function InitializeSecurityContext(phCredential: PCredHandle; phContext: PCtxtHandle;
  pszTargetName: PSecWChar; fContextReq, Reserved1, TargetDataRep: Cardinal;
  pInput: PSecBufferDesc; Reserved2: Cardinal; phNewContext: PCtxtHandle;
  pOutput: PSecBufferDesc; var pfContextAttr: Cardinal; ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;
{$EXTERNALSYM InitializeSecurityContext}

type
  INITIALIZE_SECURITY_CONTEXT_FN = function(phCredential: PCredHandle; phContext: PCtxtHandle;
    pszTargetName: PSecWChar; fContextReq, Reserved1, TargetDataRep: Cardinal;
    pInput: PSecBufferDesc; Reserved2: Cardinal; phNewContext: PCtxtHandle;
    pOutput: PSecBufferDesc; var pfContextAttr: Cardinal; ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM INITIALIZE_SECURITY_CONTEXT_FN}
{$ELSE}
function InitializeSecurityContext(phCredential: PCredHandle; phContext: PCtxtHandle;
  pszTargetName: PSecChar; fContextReq, Reserved1, TargetDataRep: Cardinal;
  pInput: PSecBufferDesc; Reserved2: Cardinal; phNewContext: PCtxtHandle;
  pOutput: PSecBufferDesc; var pfContextAttr: Cardinal; ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;
{$EXTERNALSYM InitializeSecurityContext}

type
  INITIALIZE_SECURITY_CONTEXT_FN = function(phCredential: PCredHandle; phContext: PCtxtHandle;
    pszTargetName: PSecChar; fContextReq, Reserved1, TargetDataRep: Cardinal;
    pInput: PSecBufferDesc; Reserved2: Cardinal; phNewContext: PCtxtHandle;
    pOutput: PSecBufferDesc; var pfContextAttr: Cardinal; ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM INITIALIZE_SECURITY_CONTEXT_FN}
{$ENDIF UNICODE}

function AcceptSecurityContext(phCredential: PCredHandle; phContext: PCtxtHandle;
  pInput: PSecBufferDesc; fContextReq, TargetDataRep: Cardinal;
  phNewContext: PCtxtHandle; pOutput: PSecBufferDesc; var pfContextAttr: Cardinal;
  ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;
{$EXTERNALSYM AcceptSecurityContext}

type
  ACCEPT_SECURITY_CONTEXT_FN = function(phCredential: PCredHandle; phContext: PCtxtHandle;
    pInput: PSecBufferDesc; fContextReq, TargetDataRep: Cardinal;
    phNewContext: PCtxtHandle; pOutput: PSecBufferDesc; var pfContextAttr: Cardinal;
    ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM ACCEPT_SECURITY_CONTEXT_FN}

function CompleteAuthToken(phContext: PCtxtHandle; pToken: PSecBufferDesc): SECURITY_STATUS; stdcall;
{$EXTERNALSYM CompleteAuthToken}

type
  COMPLETE_AUTH_TOKEN_FN = function(phContext: PCtxtHandle; pToken: PSecBufferDesc): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM COMPLETE_AUTH_TOKEN_FN}

function ImpersonateSecurityContext(phContext: PCtxtHandle): SECURITY_STATUS; stdcall;
{$EXTERNALSYM ImpersonateSecurityContext}

type
  IMPERSONATE_SECURITY_CONTEXT_FN = function(phContext: PCtxtHandle): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM IMPERSONATE_SECURITY_CONTEXT_FN}

function RevertSecurityContext(phContext: PCtxtHandle): SECURITY_STATUS; stdcall;
{$EXTERNALSYM RevertSecurityContext}

type
  REVERT_SECURITY_CONTEXT_FN = function(phContext: PCtxtHandle): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM REVERT_SECURITY_CONTEXT_FN}

function QuerySecurityContextToken(phContext: PCtxtHandle; var Token: Pointer): SECURITY_STATUS; stdcall;
{$EXTERNALSYM QuerySecurityContextToken}

type
  QUERY_SECURITY_CONTEXT_TOKEN_FN = function(phContext: PCtxtHandle;
    var Token: Pointer): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM QUERY_SECURITY_CONTEXT_TOKEN_FN}

function DeleteSecurityContext(phContext: PCtxtHandle): SECURITY_STATUS; stdcall;
{$EXTERNALSYM DeleteSecurityContext}

type
  DELETE_SECURITY_CONTEXT_FN = function(phContext: PCtxtHandle): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM DELETE_SECURITY_CONTEXT_FN}

function ApplyControlToken(phContext: PCtxtHandle; pInput: PSecBufferDesc): SECURITY_STATUS; stdcall;
{$EXTERNALSYM ApplyControlToken}

type
  APPLY_CONTROL_TOKEN_FN = function(phContext: PCtxtHandle; pInput: PSecBufferDesc): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM APPLY_CONTROL_TOKEN_FN}

function QueryContextAttributesW(phContext: PCtxtHandle; ulAttribute: Cardinal;
  pBuffer: Pointer): SECURITY_STATUS; stdcall;
{$EXTERNALSYM QueryContextAttributesW}

type
  QUERY_CONTEXT_ATTRIBUTES_FN_W = function(phContext: PCtxtHandle;
    ulAttribute: Cardinal; pBuffer: Pointer): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM QUERY_CONTEXT_ATTRIBUTES_FN_W}

function QueryContextAttributesA(phContext: PCtxtHandle; ulAttribute: Cardinal;
  pBuffer: Pointer): SECURITY_STATUS; stdcall;
{$EXTERNALSYM QueryContextAttributesA}

type
  QUERY_CONTEXT_ATTRIBUTES_FN_A = function(phContext: PCtxtHandle;
    ulAttribute: Cardinal; pBuffer: Pointer): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM QUERY_CONTEXT_ATTRIBUTES_FN_A}

{$IFDEF UNICODE}
function QueryContextAttributes(phContext: PCtxtHandle; ulAttribute: Cardinal;
  pBuffer: Pointer): SECURITY_STATUS; stdcall;
{$EXTERNALSYM QueryContextAttributes}

type
  QUERY_CONTEXT_ATTRIBUTES_FN = function(phContext: PCtxtHandle;
    ulAttribute: Cardinal; pBuffer: Pointer): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM QUERY_CONTEXT_ATTRIBUTES_FN}
{$ELSE}
function QueryContextAttributes(phContext: PCtxtHandle; ulAttribute: Cardinal;
  pBuffer: Pointer): SECURITY_STATUS; stdcall;
{$EXTERNALSYM QueryContextAttributes}

type
  QUERY_CONTEXT_ATTRIBUTES_FN = function(phContext: PCtxtHandle;
    ulAttribute: Cardinal; pBuffer: Pointer): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM QUERY_CONTEXT_ATTRIBUTES_FN}
{$ENDIF UNICODE}

function SetContextAttributesW(phContext: PCtxtHandle; ulAttribute: Cardinal;
  pBuffer: Pointer; cbBuffer: Cardinal): SECURITY_STATUS; stdcall;
{$EXTERNALSYM SetContextAttributesW}

type
  SET_CONTEXT_ATTRIBUTES_FN_W = function(phContext: PCtxtHandle; ulAttribute: Cardinal;
    pBuffer: Pointer; cbBuffer: Cardinal): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM SET_CONTEXT_ATTRIBUTES_FN_W}

function SetContextAttributesA(phContext: PCtxtHandle; ulAttribute: Cardinal;
  pBuffer: Pointer; cbBuffer: Cardinal): SECURITY_STATUS; stdcall;
{$EXTERNALSYM SetContextAttributesA}

type
  SET_CONTEXT_ATTRIBUTES_FN_A = function(phContext: PCtxtHandle; ulAttribute: Cardinal;
    pBuffer: Pointer; cbBuffer: Cardinal): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM SET_CONTEXT_ATTRIBUTES_FN_A}

{$IFDEF UNICODE}
function SetContextAttributes(phContext: PCtxtHandle; ulAttribute: Cardinal;
  pBuffer: Pointer; cbBuffer: Cardinal): SECURITY_STATUS; stdcall;
{$EXTERNALSYM SetContextAttributes}

type
  SET_CONTEXT_ATTRIBUTES_FN = SET_CONTEXT_ATTRIBUTES_FN_W;
  {$EXTERNALSYM SET_CONTEXT_ATTRIBUTES_FN}
{$ELSE}
function SetContextAttributes(phContext: PCtxtHandle; ulAttribute: Cardinal;
  pBuffer: Pointer; cbBuffer: Cardinal): SECURITY_STATUS; stdcall;
{$EXTERNALSYM SetContextAttributes}

type
  SET_CONTEXT_ATTRIBUTES_FN = SET_CONTEXT_ATTRIBUTES_FN_A;
  {$EXTERNALSYM SET_CONTEXT_ATTRIBUTES_FN}
{$ENDIF UNICODE}

function QueryCredentialsAttributesW(phCredential: PCredHandle;
  ulAttribute: Cardinal; pBuffer: Pointer): SECURITY_STATUS; stdcall;
{$EXTERNALSYM QueryCredentialsAttributesW}

type
  QUERY_CREDENTIALS_ATTRIBUTES_FN_W = function(phCredential: PCredHandle;
    ulAttribute: Cardinal; pBuffer: Pointer): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM QUERY_CREDENTIALS_ATTRIBUTES_FN_W}

function QueryCredentialsAttributesA(phCredential: PCredHandle;
  ulAttribute: Cardinal; pBuffer: Pointer): SECURITY_STATUS; stdcall;
{$EXTERNALSYM QueryCredentialsAttributesA}

type
  QUERY_CREDENTIALS_ATTRIBUTES_FN_A = function(phCredential: PCredHandle;
    ulAttribute: Cardinal; pBuffer: Pointer): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM QUERY_CREDENTIALS_ATTRIBUTES_FN_A}

{$IFDEF UNICODE}
function QueryCredentialsAttributes(phCredential: PCredHandle;
  ulAttribute: Cardinal; pBuffer: Pointer): SECURITY_STATUS; stdcall;
{$EXTERNALSYM QueryCredentialsAttributes}

type
  QUERY_CREDENTIALS_ATTRIBUTES_FN = function(phCredential: PCredHandle;
    ulAttribute: Cardinal; pBuffer: Pointer): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM QUERY_CREDENTIALS_ATTRIBUTES_FN}
{$ELSE}
function QueryCredentialsAttributes(phCredential: PCredHandle;
  ulAttribute: Cardinal; pBuffer: Pointer): SECURITY_STATUS; stdcall;
{$EXTERNALSYM QueryCredentialsAttributes}

type
  QUERY_CREDENTIALS_ATTRIBUTES_FN = function(phCredential: PCredHandle;
    ulAttribute: Cardinal; pBuffer: Pointer): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM QUERY_CREDENTIALS_ATTRIBUTES_FN}
{$ENDIF UNICODE}

function FreeContextBuffer(pvContextBuffer: Pointer): SECURITY_STATUS; stdcall;
{$EXTERNALSYM FreeContextBuffer}

type
  FREE_CONTEXT_BUFFER_FN = function(pvContextBuffer: Pointer): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM FREE_CONTEXT_BUFFER_FN}

///////////////////////////////////////////////////////////////////
////
////    Message Support API
////
//////////////////////////////////////////////////////////////////

function MakeSignature(phContext: PCtxtHandle; fQOP: Cardinal;
  pMessage: PSecBufferDesc; MessageSeqNo: Cardinal): SECURITY_STATUS; stdcall;
{$EXTERNALSYM MakeSignature}

type
  MAKE_SIGNATURE_FN = function(phContext: PCtxtHandle; fQOP: Cardinal;
    pMessage: PSecBufferDesc; MessageSeqNo: Cardinal): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM MAKE_SIGNATURE_FN}

function VerifySignature(phContext: PCtxtHandle; pMessage: PSecBufferDesc;
  MessageSeqNo: Cardinal; var pfQOP: Cardinal): SECURITY_STATUS; stdcall;
{$EXTERNALSYM VerifySignature}

type
  VERIFY_SIGNATURE_FN = function(phContext: PCtxtHandle; pMessage: PSecBufferDesc;
    MessageSeqNo: Cardinal; var pfQOP: Cardinal): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM VERIFY_SIGNATURE_FN}

const
  SECQOP_WRAP_NO_ENCRYPT = DWORD($80000001);
  {$EXTERNALSYM SECQOP_WRAP_NO_ENCRYPT}

function EncryptMessage(phContext: PCtxtHandle; fQOP: Cardinal;
  pMessage: PSecBufferDesc; MessageSeqNo: Cardinal): SECURITY_STATUS; stdcall;
{$EXTERNALSYM EncryptMessage}

type
  ENCRYPT_MESSAGE_FN = function(phContext: PCtxtHandle; fQOP: Cardinal;
    pMessage: PSecBufferDesc; MessageSeqNo: Cardinal): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM ENCRYPT_MESSAGE_FN}

function DecryptMessage(phContext: PCtxtHandle; pMessage: PSecBufferDesc;
  MessageSeqNo: Cardinal; var pfQOP: Cardinal): SECURITY_STATUS; stdcall;
{$EXTERNALSYM DecryptMessage}

type
  DECRYPT_MESSAGE_FN = function(phContext: PCtxtHandle; pMessage: PSecBufferDesc;
    MessageSeqNo: Cardinal; var pfQOP: Cardinal): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM DECRYPT_MESSAGE_FN}

///////////////////////////////////////////////////////////////////////////
////
////    Misc.
////
///////////////////////////////////////////////////////////////////////////

function EnumerateSecurityPackagesW(var pcPackages: Cardinal;
  var ppPackageInfo: PSecPkgInfoW): SECURITY_STATUS; stdcall;
{$EXTERNALSYM EnumerateSecurityPackagesW}

type
  ENUMERATE_SECURITY_PACKAGES_FN_W = function(var pcPackages: Cardinal;
    var ppPackageInfo: PSecPkgInfoW): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM ENUMERATE_SECURITY_PACKAGES_FN_W}

function EnumerateSecurityPackagesA(var pcPackages: Cardinal;
  var ppPackageInfo: PSecPkgInfoA): SECURITY_STATUS; stdcall;
{$EXTERNALSYM EnumerateSecurityPackagesA}

type
  ENUMERATE_SECURITY_PACKAGES_FN_A = function(var pcPackages: Cardinal;
    var ppPackageInfo: PSecPkgInfoA): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM ENUMERATE_SECURITY_PACKAGES_FN_A}

{$IFDEF UNICODE}
function EnumerateSecurityPackages(var pcPackages: Cardinal;
  var ppPackageInfo: PSecPkgInfoW): SECURITY_STATUS; stdcall;
{$EXTERNALSYM EnumerateSecurityPackages}

type
  ENUMERATE_SECURITY_PACKAGES_FN = function(var pcPackages: Cardinal;
    var ppPackageInfo: PSecPkgInfoW): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM ENUMERATE_SECURITY_PACKAGES_FN}
{$ELSE}
function EnumerateSecurityPackages(var pcPackages: Cardinal;
  var ppPackageInfo: PSecPkgInfoA): SECURITY_STATUS; stdcall;
{$EXTERNALSYM EnumerateSecurityPackages}

type
  ENUMERATE_SECURITY_PACKAGES_FN = function(var pcPackages: Cardinal;
    var ppPackageInfo: PSecPkgInfoA): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM ENUMERATE_SECURITY_PACKAGES_FN}
{$ENDIF UNICODE}

function QuerySecurityPackageInfoW(pszPackageName: PSecWChar;
  var ppPackageInfo: PSecPkgInfoW): SECURITY_STATUS; stdcall;
{$EXTERNALSYM QuerySecurityPackageInfoW}

type
  QUERY_SECURITY_PACKAGE_INFO_FN_W = function(pszPackageName: PSecWChar;
    var ppPackageInfo: PSecPkgInfoW): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM QUERY_SECURITY_PACKAGE_INFO_FN_W}

function QuerySecurityPackageInfoA(pszPackageName: PSecChar;
  var ppPackageInfo: PSecPkgInfoA): SECURITY_STATUS; stdcall;
{$EXTERNALSYM QuerySecurityPackageInfoA}

type
  QUERY_SECURITY_PACKAGE_INFO_FN_A = function(pszPackageName: PSecChar;
    var ppPackageInfo: PSecPkgInfoA): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM QUERY_SECURITY_PACKAGE_INFO_FN_A}

{$IFDEF UNICODE}
function QuerySecurityPackageInfo(pszPackageName: PSecWChar;
  var ppPackageInfo: PSecPkgInfoW): SECURITY_STATUS; stdcall;
{$EXTERNALSYM QuerySecurityPackageInfo}

type
  QUERY_SECURITY_PACKAGE_INFO_FN = function(pszPackageName: PSecWChar;
    var ppPackageInfo: PSecPkgInfoW): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM QUERY_SECURITY_PACKAGE_INFO_FN}
{$ELSE}
function QuerySecurityPackageInfo(pszPackageName: PSecChar;
  var ppPackageInfo: PSecPkgInfoA): SECURITY_STATUS; stdcall;
{$EXTERNALSYM QuerySecurityPackageInfo}

type
  QUERY_SECURITY_PACKAGE_INFO_FN = function(pszPackageName: PSecChar;
    var ppPackageInfo: PSecPkgInfoA): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM QUERY_SECURITY_PACKAGE_INFO_FN}
{$ENDIF UNICODE}

type
  _SecDelegationType = (
    SecFull,
    SecService,
    SecTree,
    SecDirectory,
    SecObject);
  {$EXTERNALSYM _SecDelegationType}
  SecDelegationType = _SecDelegationType;
  {$EXTERNALSYM SecDelegationType}
  PSecDelegationType = ^SecDelegationType;
  {$EXTERNALSYM PSecDelegationType}
  TSecDelegationType = SecDelegationType;

//function DelegateSecurityContext(phContext: PCtxtHandle; pszTarget: PSecChar;
//  DelegationType: SecDelegationType; pExpiry: PTimeStamp;
//  pPackageParameters: PSecBuffer; pOutput: PSecBufferDesc): SECURITY_STATUS; stdcall;
//{$EXTERNALSYM DelegateSecurityContext}

///////////////////////////////////////////////////////////////////////////
////
////    Proxies
////
///////////////////////////////////////////////////////////////////////////

//
// Proxies are only available on NT platforms
//

///////////////////////////////////////////////////////////////////////////
////
////    Context export/import
////
///////////////////////////////////////////////////////////////////////////

function ExportSecurityContext(phContext: PCtxtHandle; fFlags: ULONG;
  pPackedContext: PSecBuffer; var pToken: Pointer): SECURITY_STATUS; stdcall;
{$EXTERNALSYM ExportSecurityContext}

type
  EXPORT_SECURITY_CONTEXT_FN = function(phContext: PCtxtHandle; fFlags: ULONG;
    pPackedContext: PSecBuffer; var pToken: Pointer): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM EXPORT_SECURITY_CONTEXT_FN}

function ImportSecurityContextW(pszPackage: PSecWChar; pPackedContext: PSecBuffer;
  Token: Pointer; phContext: PCtxtHandle): SECURITY_STATUS; stdcall;
{$EXTERNALSYM ImportSecurityContextW}

type
  IMPORT_SECURITY_CONTEXT_FN_W = function(pszPackage: PSecWChar; pPackedContext: PSecBuffer;
    Token: Pointer; phContext: PCtxtHandle): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM IMPORT_SECURITY_CONTEXT_FN_W}

function ImportSecurityContextA(pszPackage: PSecChar; pPackedContext: PSecBuffer;
  Token: Pointer; phContext: PCtxtHandle): SECURITY_STATUS; stdcall;
{$EXTERNALSYM ImportSecurityContextA}

type
  IMPORT_SECURITY_CONTEXT_FN_A = function(pszPackage: PSecChar; pPackedContext: PSecBuffer;
    Token: Pointer; phContext: PCtxtHandle): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM IMPORT_SECURITY_CONTEXT_FN_A}

{$IFDEF UNICODE}
function ImportSecurityContext(pszPackage: PSecWChar; pPackedContext: PSecBuffer;
  Token: Pointer; phContext: PCtxtHandle): SECURITY_STATUS; stdcall;
{$EXTERNALSYM ImportSecurityContext}

type
  IMPORT_SECURITY_CONTEXT_FN = function(pszPackage: PSecWChar; pPackedContext: PSecBuffer;
    Token: Pointer; phContext: PCtxtHandle): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM IMPORT_SECURITY_CONTEXT_FN}
{$ELSE}
function ImportSecurityContext(pszPackage: PSecChar; pPackedContext: PSecBuffer;
  Token: Pointer; phContext: PCtxtHandle): SECURITY_STATUS; stdcall;
{$EXTERNALSYM ImportSecurityContext}

type
  IMPORT_SECURITY_CONTEXT_FN = function(pszPackage: PSecChar; pPackedContext: PSecBuffer;
    Token: Pointer; phContext: PCtxtHandle): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM IMPORT_SECURITY_CONTEXT_FN}
{$ENDIF UNICODE}

(*

#if ISSP_MODE == 0

NTSTATUS
NTAPI
SecMakeSPN(
    IN PUNICODE_STRING ServiceClass,
    IN PUNICODE_STRING ServiceName,
    IN PUNICODE_STRING InstanceName OPTIONAL,
    IN USHORT InstancePort OPTIONAL,
    IN PUNICODE_STRING Referrer OPTIONAL,
    IN OUT PUNICODE_STRING Spn,
    OUT PULONG Length OPTIONAL,
    IN BOOLEAN Allocate
    );

NTSTATUS
NTAPI
SecMakeSPNEx(
    IN PUNICODE_STRING ServiceClass,
    IN PUNICODE_STRING ServiceName,
    IN PUNICODE_STRING InstanceName OPTIONAL,
    IN USHORT InstancePort OPTIONAL,
    IN PUNICODE_STRING Referrer OPTIONAL,
    IN PUNICODE_STRING TargetInfo OPTIONAL,
    IN OUT PUNICODE_STRING Spn,
    OUT PULONG Length OPTIONAL,
    IN BOOLEAN Allocate
    );

NTSTATUS
SEC_ENTRY
SecLookupAccountSid(
    IN PSID Sid,
    IN OUT PULONG NameSize,
    OUT PUNICODE_STRING NameBuffer,
    IN OUT PULONG DomainSize OPTIONAL,
    OUT PUNICODE_STRING DomainBuffer OPTIONAL,
    OUT PSID_NAME_USE NameUse
    );

NTSTATUS
SEC_ENTRY
SecLookupAccountName(
    IN PUNICODE_STRING Name,
    IN OUT PULONG SidSize,
    OUT PSID Sid,
    OUT PSID_NAME_USE NameUse,
    IN OUT PULONG DomainSize OPTIONAL,
    OUT PUNICODE_STRING ReferencedDomain OPTIONAL
    );

NTSTATUS
SEC_ENTRY
SecLookupWellKnownSid(
    IN WELL_KNOWN_SID_TYPE SidType,
    OUT PSID Sid,
    ULONG SidBufferSize,
    OUT PULONG SidSize OPTIONAL
    );

#endif

*)

///////////////////////////////////////////////////////////////////////////////
////
////  Fast access for RPC:
////
///////////////////////////////////////////////////////////////////////////////

const
  SECURITY_ENTRYPOINT_ANSIW = 'InitSecurityInterfaceW';
  {$EXTERNALSYM SECURITY_ENTRYPOINT_ANSIW}
  SECURITY_ENTRYPOINT_ANSIA = 'InitSecurityInterfaceA';
  {$EXTERNALSYM SECURITY_ENTRYPOINT_ANSIA}
  SECURITY_ENTRYPOINTW      = 'InitSecurityInterfaceW';
  {$EXTERNALSYM SECURITY_ENTRYPOINTW}
  SECURITY_ENTRYPOINTA      = 'InitSecurityInterfaceA';
  {$EXTERNALSYM SECURITY_ENTRYPOINTA}
  SECURITY_ENTRYPOINT16     = 'INITSECURITYINTERFACEA';
  {$EXTERNALSYM SECURITY_ENTRYPOINT16}

  {$IFDEF UNICODE}
  SECURITY_ENTRYPOINT = SECURITY_ENTRYPOINTW;
  {$EXTERNALSYM SECURITY_ENTRYPOINT}
  SECURITY_ENTRYPOINT_ANSI = SECURITY_ENTRYPOINT_ANSIW;
  {$EXTERNALSYM SECURITY_ENTRYPOINT_ANSI}
  {$ELSE}
  SECURITY_ENTRYPOINT = SECURITY_ENTRYPOINTA;
  {$EXTERNALSYM SECURITY_ENTRYPOINT}
  SECURITY_ENTRYPOINT_ANSI = SECURITY_ENTRYPOINT_ANSIA;
  {$EXTERNALSYM SECURITY_ENTRYPOINT_ANSI}
  {$ENDIF UNICODE}

function FreeCredentialHandle(phCredential: PCredHandle): SECURITY_STATUS;
{$EXTERNALSYM FreeCredentialHandle}

type
  PSecurityFunctionTableW = ^SecurityFunctionTableW;
  {$EXTERNALSYM PSecurityFunctionTableW}
  _SECURITY_FUNCTION_TABLE_W = record
    dwVersion: Cardinal;
    EnumerateSecurityPackagesW: ENUMERATE_SECURITY_PACKAGES_FN_W;
    QueryCredentialsAttributesW: QUERY_CREDENTIALS_ATTRIBUTES_FN_W;
    AcquireCredentialsHandleW: ACQUIRE_CREDENTIALS_HANDLE_FN_W;
    FreeCredentialsHandle: FREE_CREDENTIALS_HANDLE_FN;
    Reserved2: Pointer;
    InitializeSecurityContextW: INITIALIZE_SECURITY_CONTEXT_FN_W;
    AcceptSecurityContext: ACCEPT_SECURITY_CONTEXT_FN;
    CompleteAuthToken: COMPLETE_AUTH_TOKEN_FN;
    DeleteSecurityContext: DELETE_SECURITY_CONTEXT_FN;
    ApplyControlToken: APPLY_CONTROL_TOKEN_FN;
    QueryContextAttributesW: QUERY_CONTEXT_ATTRIBUTES_FN_W;
    ImpersonateSecurityContext: IMPERSONATE_SECURITY_CONTEXT_FN;
    RevertSecurityContext: REVERT_SECURITY_CONTEXT_FN;
    MakeSignature: MAKE_SIGNATURE_FN;
    VerifySignature: VERIFY_SIGNATURE_FN;
    FreeContextBuffer: FREE_CONTEXT_BUFFER_FN;
    QuerySecurityPackageInfoW: QUERY_SECURITY_PACKAGE_INFO_FN_W;
    Reserved3: Pointer;
    Reserved4: Pointer;
    ExportSecurityContext: EXPORT_SECURITY_CONTEXT_FN;
    ImportSecurityContextW: IMPORT_SECURITY_CONTEXT_FN_W;
    AddCredentialsW: ADD_CREDENTIALS_FN_W;
    Reserved8: Pointer;
    QuerySecurityContextToken: QUERY_SECURITY_CONTEXT_TOKEN_FN;
    EncryptMessage: ENCRYPT_MESSAGE_FN;
    DecryptMessage: DECRYPT_MESSAGE_FN;
    SetContextAttributesW: SET_CONTEXT_ATTRIBUTES_FN_W;
  end;
  {$EXTERNALSYM _SECURITY_FUNCTION_TABLE_W}
  SecurityFunctionTableW = _SECURITY_FUNCTION_TABLE_W;
  {$EXTERNALSYM SecurityFunctionTableW}
  TSecurityFunctionTableW = SecurityFunctionTableW;

  PSecurityFunctionTableA = ^SecurityFunctionTableA;
  {$EXTERNALSYM PSecurityFunctionTableA}
  _SECURITY_FUNCTION_TABLE_A = record
    dwVersion: Cardinal;
    EnumerateSecurityPackagesA: ENUMERATE_SECURITY_PACKAGES_FN_A;
    QueryCredentialsAttributesA: QUERY_CREDENTIALS_ATTRIBUTES_FN_A;
    AcquireCredentialsHandleA: ACQUIRE_CREDENTIALS_HANDLE_FN_A;
    FreeCredentialHandle: FREE_CREDENTIALS_HANDLE_FN;
    Reserved2: Pointer;
    InitializeSecurityContextA: INITIALIZE_SECURITY_CONTEXT_FN_A;
    AcceptSecurityContext: ACCEPT_SECURITY_CONTEXT_FN;
    CompleteAuthToken: COMPLETE_AUTH_TOKEN_FN;
    DeleteSecurityContext: DELETE_SECURITY_CONTEXT_FN;
    ApplyControlToken: APPLY_CONTROL_TOKEN_FN;
    QueryContextAttributesA: QUERY_CONTEXT_ATTRIBUTES_FN_A;
    ImpersonateSecurityContext: IMPERSONATE_SECURITY_CONTEXT_FN;
    RevertSecurityContext: REVERT_SECURITY_CONTEXT_FN;
    MakeSignature: MAKE_SIGNATURE_FN;
    VerifySignature: VERIFY_SIGNATURE_FN;
    FreeContextBuffer: FREE_CONTEXT_BUFFER_FN;
    QuerySecurityPackageInfoA: QUERY_SECURITY_PACKAGE_INFO_FN_A;
    Reserved3: Pointer;
    Reserved4: Pointer;
    ExportSecurityContext: EXPORT_SECURITY_CONTEXT_FN;
    ImportSecurityContextA: IMPORT_SECURITY_CONTEXT_FN_A;
    AddCredentialsA: ADD_CREDENTIALS_FN_A;
    Reserved8: Pointer;
    QuerySecurityContextToken: QUERY_SECURITY_CONTEXT_TOKEN_FN;
    EncryptMessage: ENCRYPT_MESSAGE_FN;
    DecryptMessage: DECRYPT_MESSAGE_FN;
    SetContextAttributesA: SET_CONTEXT_ATTRIBUTES_FN_A;
  end;
  {$EXTERNALSYM _SECURITY_FUNCTION_TABLE_A}
  SecurityFunctionTableA = _SECURITY_FUNCTION_TABLE_A;
  {$EXTERNALSYM SecurityFunctionTableA}
  TSecurityFunctionTableA = SecurityFunctionTableA;

  {$IFDEF UNICODE}
  SecurityFunctionTable  = SecurityFunctionTableW;
  {$EXTERNALSYM SecurityFunctionTable}
  PSecurityFunctionTable = PSecurityFunctionTableW;
  {$EXTERNALSYM PSecurityFunctionTable}
  TSecurityFunctionTable = TSecurityFunctionTableW;
  {$ELSE}
  SecurityFunctionTable  = SecurityFunctionTableA;
  {$EXTERNALSYM SecurityFunctionTable}
  PSecurityFunctionTable = PSecurityFunctionTableA;
  {$EXTERNALSYM PSecurityFunctionTable}
  TSecurityFunctionTable = TSecurityFunctionTableA;
  {$ENDIF UNICODE}

const
  // Function table has all routines through DecryptMessage
  SECURITY_SUPPORT_PROVIDER_INTERFACE_VERSION = 1;
  {$EXTERNALSYM SECURITY_SUPPORT_PROVIDER_INTERFACE_VERSION}

  // Function table has all routines through SetContextAttributes
  SECURITY_SUPPORT_PROVIDER_INTERFACE_VERSION_2 = 2;
  {$EXTERNALSYM SECURITY_SUPPORT_PROVIDER_INTERFACE_VERSION_2}

function InitSecurityInterfaceA: PSecurityFunctionTableA; stdcall;
{$EXTERNALSYM InitSecurityInterfaceA}

type
  INIT_SECURITY_INTERFACE_A = function: PSecurityFunctionTableA; stdcall;
  {$EXTERNALSYM INIT_SECURITY_INTERFACE_A}

function InitSecurityInterfaceW: PSecurityFunctionTableW; stdcall;
{$EXTERNALSYM InitSecurityInterfaceW}

type
  INIT_SECURITY_INTERFACE_W = function: PSecurityFunctionTableW; stdcall;
  {$EXTERNALSYM INIT_SECURITY_INTERFACE_W}

{$IFDEF UNICODE}
function InitSecurityInterface: PSecurityFunctionTableW; stdcall;
{$EXTERNALSYM InitSecurityInterface}

type
  INIT_SECURITY_INTERFACE = function: PSecurityFunctionTableW; stdcall;
  {$EXTERNALSYM INIT_SECURITY_INTERFACE}
{$ELSE}
function InitSecurityInterface: PSecurityFunctionTableA; stdcall;
{$EXTERNALSYM InitSecurityInterface}

type
  INIT_SECURITY_INTERFACE = function: PSecurityFunctionTableA; stdcall;
  {$EXTERNALSYM INIT_SECURITY_INTERFACE}
{$ENDIF UNICODE}

//
// SASL Profile Support
//

function SaslEnumerateProfilesA(var ProfileList: LPSTR;
  var ProfileCount: ULONG): SECURITY_STATUS; stdcall;
{$EXTERNALSYM SaslEnumerateProfilesA}
function SaslEnumerateProfilesW(var ProfileList: LPWSTR;
  var ProfileCount: ULONG): SECURITY_STATUS; stdcall;
{$EXTERNALSYM SaslEnumerateProfilesW}

{$IFDEF UNICODE}
function SaslEnumerateProfiles(var ProfileList: LPWSTR;
  var ProfileCount: ULONG): SECURITY_STATUS; stdcall;
{$EXTERNALSYM SaslEnumerateProfiles}
{$ELSE}
function SaslEnumerateProfiles(var ProfileList: LPSTR;
  var ProfileCount: ULONG): SECURITY_STATUS; stdcall;
{$EXTERNALSYM SaslEnumerateProfiles}
{$ENDIF UNICODE}

function SaslGetProfilePackageA(ProfileName: LPSTR;
  var PackageInfo: PSecPkgInfoA): SECURITY_STATUS; stdcall;
{$EXTERNALSYM SaslGetProfilePackageA}
function SaslGetProfilePackageW(ProfileName: LPWSTR;
  var PackageInfo: PSecPkgInfoW): SECURITY_STATUS; stdcall;
{$EXTERNALSYM SaslGetProfilePackageW}

{$IFDEF UNICODE}
function SaslGetProfilePackage(ProfileName: LPWSTR;
  var PackageInfo: PSecPkgInfoW): SECURITY_STATUS; stdcall;
{$EXTERNALSYM SaslGetProfilePackage}
{$ELSE}
function SaslGetProfilePackage(ProfileName: LPSTR;
  var PackageInfo: PSecPkgInfoA): SECURITY_STATUS; stdcall;
{$EXTERNALSYM SaslGetProfilePackage}
{$ENDIF UNICODE}

function SaslIdentifyPackageA(pInput: PSecBufferDesc;
  var PackageInfo: PSecPkgInfoA): SECURITY_STATUS; stdcall;
{$EXTERNALSYM SaslIdentifyPackageA}
function SaslIdentifyPackageW(pInput: PSecBufferDesc;
  var PackageInfo: PSecPkgInfoW): SECURITY_STATUS; stdcall;
{$EXTERNALSYM SaslIdentifyPackageW}

{$IFDEF UNICODE}
function SaslIdentifyPackage(pInput: PSecBufferDesc;
  var PackageInfo: PSecPkgInfoW): SECURITY_STATUS; stdcall;
{$EXTERNALSYM SaslIdentifyPackage}
{$ELSE}
function SaslIdentifyPackage(pInput: PSecBufferDesc;
  var PackageInfo: PSecPkgInfoA): SECURITY_STATUS; stdcall;
{$EXTERNALSYM SaslIdentifyPackage}
{$ENDIF UNICODE}

function SaslInitializeSecurityContextW(phCredential: PCredHandle;
  phContext: PCtxtHandle; pszTargetName: LPWSTR; fContextReq, Reserved1: Cardinal;
  TargetDataRep: Cardinal; pInput: PSecBufferDesc; Reserved2: Cardinal;
  phNewContext: PCtxtHandle; pOutput: PSecBufferDesc; var pfContextAttr: Cardinal;
  ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;
{$EXTERNALSYM SaslInitializeSecurityContextW}
function SaslInitializeSecurityContextA(phCredential: PCredHandle;
  phContext: PCtxtHandle; pszTargetName: LPSTR; fContextReq, Reserved1: Cardinal;
  TargetDataRep: Cardinal; pInput: PSecBufferDesc; Reserved2: Cardinal;
  phNewContext: PCtxtHandle; pOutput: PSecBufferDesc; var pfContextAttr: Cardinal;
  ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;
{$EXTERNALSYM SaslInitializeSecurityContextA}

{$IFDEF UNICODE}
function SaslInitializeSecurityContext(phCredential: PCredHandle;
  phContext: PCtxtHandle; pszTargetName: LPWSTR; fContextReq, Reserved1: Cardinal;
  TargetDataRep: Cardinal; pInput: PSecBufferDesc; Reserved2: Cardinal;
  phNewContext: PCtxtHandle; pOutput: PSecBufferDesc; var pfContextAttr: Cardinal;
  ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;
{$EXTERNALSYM SaslInitializeSecurityContext}
{$ELSE}
function SaslInitializeSecurityContext(phCredential: PCredHandle;
  phContext: PCtxtHandle; pszTargetName: LPSTR; fContextReq, Reserved1: Cardinal;
  TargetDataRep: Cardinal; pInput: PSecBufferDesc; Reserved2: Cardinal;
  phNewContext: PCtxtHandle; pOutput: PSecBufferDesc; var pfContextAttr: Cardinal;
  ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;
{$EXTERNALSYM SaslInitializeSecurityContext}
{$ENDIF UNICODE}

function SaslAcceptSecurityContext(phCredential: PCredHandle;
  phContext: PCtxtHandle; pInput: PSecBufferDesc; fContextReq: Cardinal;
  TargetDataRep: Cardinal; phNewContext: PCtxtHandle; pOutput: PSecBufferDesc;
  var pfContextAttr: Cardinal; ptsExpiry: PTimeStamp): SECURITY_STATUS; stdcall;
{$EXTERNALSYM SaslAcceptSecurityContext}

const
  SASL_OPTION_SEND_SIZE        = 1;       // Maximum size to send to peer
  {$EXTERNALSYM SASL_OPTION_SEND_SIZE}
  SASL_OPTION_RECV_SIZE        = 2;       // Maximum size willing to receive
  {$EXTERNALSYM SASL_OPTION_RECV_SIZE}
  SASL_OPTION_AUTHZ_STRING     = 3;       // Authorization string
  {$EXTERNALSYM SASL_OPTION_AUTHZ_STRING}
  SASL_OPTION_AUTHZ_PROCESSING = 4;       // Authorization string processing
  {$EXTERNALSYM SASL_OPTION_AUTHZ_PROCESSING}

type
  _SASL_AUTHZID_STATE = (
    Sasl_AuthZIDForbidden,             // allow no AuthZID strings to be specified - error out (default)
    Sasl_AuthZIDProcessed);           // AuthZID Strings processed by Application or SSP
  {$EXTERNALSYM _SASL_AUTHZID_STATE}
  SASL_AUTHZID_STATE = _SASL_AUTHZID_STATE;
  {$EXTERNALSYM SASL_AUTHZID_STATE}
  TSaslAuthzIDState = SASL_AUTHZID_STATE;

function SaslSetContextOption(ContextHandle: PCtxtHandle; Option: ULONG; Value: PVOID; Size: ULONG): SECURITY_STATUS; stdcall;
{$EXTERNALSYM SaslSetContextOption}

function SaslGetContextOption(ContextHandle: PCtxtHandle; Option: ULONG; Value: PVOID;
  Size: ULONG; Needed: PULONG): SECURITY_STATUS; stdcall;
{$EXTERNALSYM SaslGetContextOption}

//
// This is the legacy credentials structure.
// The EX version below is preferred.

const
  SEC_WINNT_AUTH_IDENTITY_ANSI    = $1;
  {$EXTERNALSYM SEC_WINNT_AUTH_IDENTITY_ANSI}
  SEC_WINNT_AUTH_IDENTITY_UNICODE = $2;
  {$EXTERNALSYM SEC_WINNT_AUTH_IDENTITY_UNICODE}

type
  _SEC_WINNT_AUTH_IDENTITY_W = record
    User: PWideChar;
    UserLength: Cardinal;
    Domain: PWideChar;
    DomainLength: Cardinal;
    Password: PWideChar;
    PasswordLength: Cardinal;
    Flags: Cardinal;
  end;
  {$EXTERNALSYM _SEC_WINNT_AUTH_IDENTITY_W}
  SEC_WINNT_AUTH_IDENTITY_W = _SEC_WINNT_AUTH_IDENTITY_W;
  {$EXTERNALSYM SEC_WINNT_AUTH_IDENTITY_W}
  PSEC_WINNT_AUTH_IDENTITY_W = ^SEC_WINNT_AUTH_IDENTITY_W;
  {$EXTERNALSYM PSEC_WINNT_AUTH_IDENTITY_W}
  TSecWinNTAuthIdentityW = SEC_WINNT_AUTH_IDENTITY_W;
  PSecWinNTAuthIdentityW = PSEC_WINNT_AUTH_IDENTITY_W;

  _SEC_WINNT_AUTH_IDENTITY_A = record
    User: PAnsiChar;
    UserLength: Cardinal;
    Domain: PAnsiChar;
    DomainLength: Cardinal;
    Password: PAnsiChar;
    PasswordLength: Cardinal;
    Flags: Cardinal;
  end;
  {$EXTERNALSYM _SEC_WINNT_AUTH_IDENTITY_A}
  SEC_WINNT_AUTH_IDENTITY_A = _SEC_WINNT_AUTH_IDENTITY_A;
  {$EXTERNALSYM SEC_WINNT_AUTH_IDENTITY_A}
  PSEC_WINNT_AUTH_IDENTITY_A = ^SEC_WINNT_AUTH_IDENTITY_A;
  {$EXTERNALSYM PSEC_WINNT_AUTH_IDENTITY_A}
  TSecWinNTAuthIdentityA = SEC_WINNT_AUTH_IDENTITY_A;
  PSecWinNTAuthIdentityA = PSEC_WINNT_AUTH_IDENTITY_A;

  {$IFDEF UNICODE}
  SEC_WINNT_AUTH_IDENTITY = SEC_WINNT_AUTH_IDENTITY_W;
  {$EXTERNALSYM SEC_WINNT_AUTH_IDENTITY}
  PSEC_WINNT_AUTH_IDENTITY = PSEC_WINNT_AUTH_IDENTITY_W;
  {$EXTERNALSYM PSEC_WINNT_AUTH_IDENTITY}
  _SEC_WINNT_AUTH_IDENTITY = _SEC_WINNT_AUTH_IDENTITY_W;
  {$EXTERNALSYM _SEC_WINNT_AUTH_IDENTITY}
  TSecWinNTAuthIdentity = TSecWinNTAuthIdentityW;
  PSecWinNTAuthIdentity = PSecWinNTAuthIdentityW;
  {$ELSE}
  SEC_WINNT_AUTH_IDENTITY = SEC_WINNT_AUTH_IDENTITY_A;
  {$EXTERNALSYM SEC_WINNT_AUTH_IDENTITY}
  PSEC_WINNT_AUTH_IDENTITY = PSEC_WINNT_AUTH_IDENTITY_A;
  {$EXTERNALSYM PSEC_WINNT_AUTH_IDENTITY}
  _SEC_WINNT_AUTH_IDENTITY = _SEC_WINNT_AUTH_IDENTITY_A;
  {$EXTERNALSYM _SEC_WINNT_AUTH_IDENTITY}
  TSecWinNTAuthIdentity = TSecWinNTAuthIdentityA;
  PSecWinNTAuthIdentity = PSecWinNTAuthIdentityA;
  {$ENDIF UNICODE}

//
// This is the combined authentication identity structure that may be
// used with the negotiate package, NTLM, Kerberos, or SCHANNEL
//

const
  SEC_WINNT_AUTH_IDENTITY_VERSION = $200;

type
  _SEC_WINNT_AUTH_IDENTITY_EXW = record
    Version: Cardinal;
    Length: Cardinal;
    User: PWideChar;
    UserLength: Cardinal;
    Domain: PWideChar;
    DomainLength: Cardinal;
    Password: PWideChar;
    PasswordLength: Cardinal;
    Flags: Cardinal;
    PackageList: PWideChar;
    PackageListLength: Cardinal;
  end;
  {$EXTERNALSYM _SEC_WINNT_AUTH_IDENTITY_EXW}
  SEC_WINNT_AUTH_IDENTITY_EXW = _SEC_WINNT_AUTH_IDENTITY_EXW;
  {$EXTERNALSYM SEC_WINNT_AUTH_IDENTITY_EXW}
  PSEC_WINNT_AUTH_IDENTITY_EXW = ^SEC_WINNT_AUTH_IDENTITY_EXW;
  {$EXTERNALSYM PSEC_WINNT_AUTH_IDENTITY_EXW}
  TSecWinNTAuthIdentityExW = SEC_WINNT_AUTH_IDENTITY_EXW;
  PSecWinNTAuthIdentityExW = PSEC_WINNT_AUTH_IDENTITY_EXW;

  _SEC_WINNT_AUTH_IDENTITY_EXA = record
    Version: Cardinal;
    Length: Cardinal;
    User: PAnsiChar;
    UserLength: Cardinal;
    Domain: PAnsiChar;
    DomainLength: Cardinal;
    Password: PAnsiChar;
    PasswordLength: Cardinal;
    Flags: Cardinal;
    PackageList: PAnsiChar;
    PackageListLength: Cardinal;
  end;
  {$EXTERNALSYM _SEC_WINNT_AUTH_IDENTITY_EXA}
  SEC_WINNT_AUTH_IDENTITY_EXA = _SEC_WINNT_AUTH_IDENTITY_EXA;
  {$EXTERNALSYM SEC_WINNT_AUTH_IDENTITY_EXA}
  PSEC_WINNT_AUTH_IDENTITY_EXA = ^SEC_WINNT_AUTH_IDENTITY_EXA;
  {$EXTERNALSYM PSEC_WINNT_AUTH_IDENTITY_EXA}
  TSecWinNTAuthIdentityExA = SEC_WINNT_AUTH_IDENTITY_EXA;
  PSecWinNTAuthIdentityExA = PSEC_WINNT_AUTH_IDENTITY_EXA;

  {$IFDEF UNICODE}
  SEC_WINNT_AUTH_IDENTITY_EX = SEC_WINNT_AUTH_IDENTITY_EXW;
  {$EXTERNALSYM SEC_WINNT_AUTH_IDENTITY_EX}
  PSEC_WINNT_AUTH_IDENTITY_EX = PSEC_WINNT_AUTH_IDENTITY_EXW;
  {$EXTERNALSYM PSEC_WINNT_AUTH_IDENTITY_EX}
  TSecWinNTAuthIdentityEx = TSecWinNTAuthIdentityExW;
  PSecWinNTAuthIdentityEx = PSecWinNTAuthIdentityExW;
  {$ELSE}
  SEC_WINNT_AUTH_IDENTITY_EX = SEC_WINNT_AUTH_IDENTITY_EXA;
  {$EXTERNALSYM SEC_WINNT_AUTH_IDENTITY_EX}
  PSEC_WINNT_AUTH_IDENTITY_EX = PSEC_WINNT_AUTH_IDENTITY_EXA;
  {$EXTERNALSYM PSEC_WINNT_AUTH_IDENTITY_EX}
  TSecWinNTAuthIdentityEx = TSecWinNTAuthIdentityExA;
  PSecWinNTAuthIdentityEx = PSecWinNTAuthIdentityExA;
  {$ENDIF UNICODE}

//
// Common types used by negotiable security packages
//

const
  SEC_WINNT_AUTH_IDENTITY_MARSHALLED     = $4;     // all data is in one buffer
  {$EXTERNALSYM SEC_WINNT_AUTH_IDENTITY_MARSHALLED}
  SEC_WINNT_AUTH_IDENTITY_ONLY           = $8;     // these credentials are for identity only - no PAC needed
  {$EXTERNALSYM SEC_WINNT_AUTH_IDENTITY_ONLY}

//
// Routines for manipulating packages
//

type
  _SECURITY_PACKAGE_OPTIONS = record
    Size: Cardinal;
    Type_: Cardinal;
    Flags: Cardinal;
    SignatureSize: Cardinal;
    Signature: Pointer;
  end;
  {$EXTERNALSYM _SECURITY_PACKAGE_OPTIONS}
  SECURITY_PACKAGE_OPTIONS = _SECURITY_PACKAGE_OPTIONS;
  {$EXTERNALSYM SECURITY_PACKAGE_OPTIONS}
  PSECURITY_PACKAGE_OPTIONS = ^SECURITY_PACKAGE_OPTIONS;
  TSecurityPackageOptions = SECURITY_PACKAGE_OPTIONS;
  PSecurityPackageOptions = PSECURITY_PACKAGE_OPTIONS;

const
  SECPKG_OPTIONS_TYPE_UNKNOWN = 0;
  {$EXTERNALSYM SECPKG_OPTIONS_TYPE_UNKNOWN}
  SECPKG_OPTIONS_TYPE_LSA     = 1;
  {$EXTERNALSYM SECPKG_OPTIONS_TYPE_LSA}
  SECPKG_OPTIONS_TYPE_SSPI    = 2;
  {$EXTERNALSYM SECPKG_OPTIONS_TYPE_SSPI}

  SECPKG_OPTIONS_PERMANENT    = $00000001;
  {$EXTERNALSYM SECPKG_OPTIONS_PERMANENT}

function AddSecurityPackageA(pszPackageName: PSEC_CHAR; Options: PSECURITY_PACKAGE_OPTIONS): SECURITY_STATUS; stdcall;
{$EXTERNALSYM AddSecurityPackageA}

function AddSecurityPackageW(pszPackageName: PSEC_WCHAR; Options: PSECURITY_PACKAGE_OPTIONS): SECURITY_STATUS; stdcall;
{$EXTERNALSYM AddSecurityPackageW}

{$IFDEF UNICODE}
function AddSecurityPackage(pszPackageName: PSEC_WCHAR; Options: PSECURITY_PACKAGE_OPTIONS): SECURITY_STATUS; stdcall;
{$EXTERNALSYM AddSecurityPackage}
{$ELSE}
function AddSecurityPackage(pszPackageName: PSEC_CHAR; Options: PSECURITY_PACKAGE_OPTIONS): SECURITY_STATUS; stdcall;
{$EXTERNALSYM AddSecurityPackage}
{$ENDIF UNICODE}

function DeleteSecurityPackageA(pszPackageName: PSEC_CHAR): SECURITY_STATUS; stdcall;
{$EXTERNALSYM DeleteSecurityPackageA}

function DeleteSecurityPackageW(pszPackageName: PSEC_WCHAR): SECURITY_STATUS; stdcall;
{$EXTERNALSYM DeleteSecurityPackageW}

{$IFDEF UNICODE}
function DeleteSecurityPackage(pszPackageName: PSEC_WCHAR): SECURITY_STATUS; stdcall;
{$EXTERNALSYM DeleteSecurityPackage}
{$ELSE}
function DeleteSecurityPackage(pszPackageName: PSEC_CHAR): SECURITY_STATUS; stdcall;
{$EXTERNALSYM DeleteSecurityPackage}
{$ENDIF UNICODE}

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}


{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  secur32 = 'secur32.dll';
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

procedure SecInvalidateHandle(var x: SecHandle);
begin
  x.dwLower := ULONG_PTR(-1);
  x.dwUpper := ULONG_PTR(-1);
end;

function SecIsValidHandle(x: SecHandle): Boolean;
begin
  Result := (x.dwLower <> ULONG_PTR(-1)) and (x.dwUpper <> ULONG_PTR(-1));
end;

function FreeCredentialHandle(phCredential: PCredHandle): SECURITY_STATUS;
begin
  Result := FreeCredentialsHandle(phCredential);
end;

//function SspiLogonUserW; external secur32 name 'SspiLogonUserW';
//function SspiLogonUserA; external secur32 name 'SspiLogonUserA';
//{$IFDEF UNICODE}
//function SspiLogonUser; external secur32 name 'SspiLogonUserW';
//{$ELSE}
//function SspiLogonUser; external secur32 name 'SspiLogonUserA';
//{$ENDIF UNICODE}

//function DelegateSecurityContext; external secur32 name 'DelegateSecurityContext';

{$IFDEF DYNAMIC_LINK}

var
  _AcquireCredentialsHandleW: Pointer;

function AcquireCredentialsHandleW;
begin
  GetProcedureAddress(_AcquireCredentialsHandleW, secur32, 'AcquireCredentialsHandleW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AcquireCredentialsHandleW]
  end;
end;

var
  _AcquireCredentialsHandleA: Pointer;

function AcquireCredentialsHandleA;
begin
  GetProcedureAddress(_AcquireCredentialsHandleA, secur32, 'AcquireCredentialsHandleA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AcquireCredentialsHandleA]
  end;
end;

var
  _AcquireCredentialsHandle: Pointer;

function AcquireCredentialsHandle;
begin
  GetProcedureAddress(_AcquireCredentialsHandle, secur32, 'AcquireCredentialsHandle' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AcquireCredentialsHandle]
  end;
end;

var
  _FreeCredentialsHandle: Pointer;

function FreeCredentialsHandle;
begin
  GetProcedureAddress(_FreeCredentialsHandle, secur32, 'FreeCredentialsHandle');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FreeCredentialsHandle]
  end;
end;

var
  _AddCredentialsW: Pointer;

function AddCredentialsW;
begin
  GetProcedureAddress(_AddCredentialsW, secur32, 'AddCredentialsW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AddCredentialsW]
  end;
end;

var
  _AddCredentialsA: Pointer;

function AddCredentialsA;
begin
  GetProcedureAddress(_AddCredentialsA, secur32, 'AddCredentialsA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AddCredentialsA]
  end;
end;

var
  _AddCredentials: Pointer;

function AddCredentials;
begin
  GetProcedureAddress(_AddCredentials, secur32, 'AddCredentials' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AddCredentials]
  end;
end;

var
  _InitializeSecurityContextW: Pointer;

function InitializeSecurityContextW;
begin
  GetProcedureAddress(_InitializeSecurityContextW, secur32, 'InitializeSecurityContextW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InitializeSecurityContextW]
  end;
end;

var
  _InitializeSecurityContextA: Pointer;

function InitializeSecurityContextA;
begin
  GetProcedureAddress(_InitializeSecurityContextA, secur32, 'InitializeSecurityContextA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InitializeSecurityContextA]
  end;
end;

var
  _InitializeSecurityContext: Pointer;

function InitializeSecurityContext;
begin
  GetProcedureAddress(_InitializeSecurityContext, secur32, 'InitializeSecurityContext' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InitializeSecurityContext]
  end;
end;

var
  _AcceptSecurityContext: Pointer;

function AcceptSecurityContext;
begin
  GetProcedureAddress(_AcceptSecurityContext, secur32, 'AcceptSecurityContext');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AcceptSecurityContext]
  end;
end;

var
  _CompleteAuthToken: Pointer;

function CompleteAuthToken;
begin
  GetProcedureAddress(_CompleteAuthToken, secur32, 'CompleteAuthToken');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CompleteAuthToken]
  end;
end;

var
  _ImpersonateSecurityContext: Pointer;

function ImpersonateSecurityContext;
begin
  GetProcedureAddress(_ImpersonateSecurityContext, secur32, 'ImpersonateSecurityContext');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ImpersonateSecurityContext]
  end;
end;

var
  _RevertSecurityContext: Pointer;

function RevertSecurityContext;
begin
  GetProcedureAddress(_RevertSecurityContext, secur32, 'RevertSecurityContext');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RevertSecurityContext]
  end;
end;

var
  _QuerySecurityContextToken: Pointer;

function QuerySecurityContextToken;
begin
  GetProcedureAddress(_QuerySecurityContextToken, secur32, 'QuerySecurityContextToken');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_QuerySecurityContextToken]
  end;
end;

var
  _DeleteSecurityContext: Pointer;

function DeleteSecurityContext;
begin
  GetProcedureAddress(_DeleteSecurityContext, secur32, 'DeleteSecurityContext');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DeleteSecurityContext]
  end;
end;

var
  _ApplyControlToken: Pointer;

function ApplyControlToken;
begin
  GetProcedureAddress(_ApplyControlToken, secur32, 'ApplyControlToken');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ApplyControlToken]
  end;
end;

var
  _QueryContextAttributesW: Pointer;

function QueryContextAttributesW;
begin
  GetProcedureAddress(_QueryContextAttributesW, secur32, 'QueryContextAttributesW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_QueryContextAttributesW]
  end;
end;

var
  _QueryContextAttributesA: Pointer;

function QueryContextAttributesA;
begin
  GetProcedureAddress(_QueryContextAttributesA, secur32, 'QueryContextAttributesA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_QueryContextAttributesA]
  end;
end;

var
  _QueryContextAttributes: Pointer;

function QueryContextAttributes;
begin
  GetProcedureAddress(_QueryContextAttributes, secur32, 'QueryContextAttributes' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_QueryContextAttributes]
  end;
end;

var
  _SetContextAttributesW: Pointer;

function SetContextAttributesW;
begin
  GetProcedureAddress(_SetContextAttributesW, secur32, 'SetContextAttributesW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetContextAttributesW]
  end;
end;

var
  _SetContextAttributesA: Pointer;

function SetContextAttributesA;
begin
  GetProcedureAddress(_SetContextAttributesA, secur32, 'SetContextAttributesA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetContextAttributesA]
  end;
end;

var
  _SetContextAttributes: Pointer;

function SetContextAttributes;
begin
  GetProcedureAddress(_SetContextAttributes, secur32, 'SetContextAttributes' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetContextAttributes]
  end;
end;

var
  _QueryCredentialsAttributesW: Pointer;

function QueryCredentialsAttributesW;
begin
  GetProcedureAddress(_QueryCredentialsAttributesW, secur32, 'QueryCredentialsAttributesW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_QueryCredentialsAttributesW]
  end;
end;

var
  _QueryCredentialsAttributesA: Pointer;

function QueryCredentialsAttributesA;
begin
  GetProcedureAddress(_QueryCredentialsAttributesA, secur32, 'QueryCredentialsAttributesA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_QueryCredentialsAttributesA]
  end;
end;

var
  _QueryCredentialsAttributes: Pointer;

function QueryCredentialsAttributes;
begin
  GetProcedureAddress(_QueryCredentialsAttributes, secur32, 'QueryCredentialsAttributes' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_QueryCredentialsAttributes]
  end;
end;

var
  _FreeContextBuffer: Pointer;

function FreeContextBuffer;
begin
  GetProcedureAddress(_FreeContextBuffer, secur32, 'FreeContextBuffer');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FreeContextBuffer]
  end;
end;

var
  _MakeSignature: Pointer;

function MakeSignature;
begin
  GetProcedureAddress(_MakeSignature, secur32, 'MakeSignature');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MakeSignature]
  end;
end;

var
  _VerifySignature: Pointer;

function VerifySignature;
begin
  GetProcedureAddress(_VerifySignature, secur32, 'VerifySignature');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_VerifySignature]
  end;
end;

var
  _EncryptMessage: Pointer;

function EncryptMessage;
begin
  GetProcedureAddress(_EncryptMessage, secur32, 'EncryptMessage');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EncryptMessage]
  end;
end;

var
  _DecryptMessage: Pointer;

function DecryptMessage;
begin
  GetProcedureAddress(_DecryptMessage, secur32, 'DecryptMessage');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DecryptMessage]
  end;
end;

var
  _EnumerateSecurityPackagesW: Pointer;

function EnumerateSecurityPackagesW;
begin
  GetProcedureAddress(_EnumerateSecurityPackagesW, secur32, 'EnumerateSecurityPackagesW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EnumerateSecurityPackagesW]
  end;
end;

var
  _EnumerateSecurityPackagesA: Pointer;

function EnumerateSecurityPackagesA;
begin
  GetProcedureAddress(_EnumerateSecurityPackagesA, secur32, 'EnumerateSecurityPackagesA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EnumerateSecurityPackagesA]
  end;
end;

var
  _EnumerateSecurityPackages: Pointer;

function EnumerateSecurityPackages;
begin
  GetProcedureAddress(_EnumerateSecurityPackages, secur32, 'EnumerateSecurityPackages' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EnumerateSecurityPackages]
  end;
end;

var
  _QuerySecurityPackageInfoW: Pointer;

function QuerySecurityPackageInfoW;
begin
  GetProcedureAddress(_QuerySecurityPackageInfoW, secur32, 'QuerySecurityPackageInfoW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_QuerySecurityPackageInfoW]
  end;
end;

var
  _QuerySecurityPackageInfoA: Pointer;

function QuerySecurityPackageInfoA;
begin
  GetProcedureAddress(_QuerySecurityPackageInfoA, secur32, 'QuerySecurityPackageInfoA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_QuerySecurityPackageInfoA]
  end;
end;

var
  _QuerySecurityPackageInfo: Pointer;

function QuerySecurityPackageInfo;
begin
  GetProcedureAddress(_QuerySecurityPackageInfo, secur32, 'QuerySecurityPackageInfo' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_QuerySecurityPackageInfo]
  end;
end;

var
  _ExportSecurityContext: Pointer;

function ExportSecurityContext;
begin
  GetProcedureAddress(_ExportSecurityContext, secur32, 'ExportSecurityContext');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ExportSecurityContext]
  end;
end;

var
  _ImportSecurityContextW: Pointer;

function ImportSecurityContextW;
begin
  GetProcedureAddress(_ImportSecurityContextW, secur32, 'ImportSecurityContextW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ImportSecurityContextW]
  end;
end;

var
  _ImportSecurityContextA: Pointer;

function ImportSecurityContextA;
begin
  GetProcedureAddress(_ImportSecurityContextA, secur32, 'ImportSecurityContextA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ImportSecurityContextA]
  end;
end;

var
  _ImportSecurityContext: Pointer;

function ImportSecurityContext;
begin
  GetProcedureAddress(_ImportSecurityContext, secur32, 'ImportSecurityContext' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ImportSecurityContext]
  end;
end;

var
  _InitSecurityInterfaceA: Pointer;

function InitSecurityInterfaceA;
begin
  GetProcedureAddress(_InitSecurityInterfaceA, secur32, 'InitSecurityInterfaceA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InitSecurityInterfaceA]
  end;
end;

var
  _InitSecurityInterfaceW: Pointer;

function InitSecurityInterfaceW;
begin
  GetProcedureAddress(_InitSecurityInterfaceW, secur32, 'InitSecurityInterfaceW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InitSecurityInterfaceW]
  end;
end;

var
  _InitSecurityInterface: Pointer;

function InitSecurityInterface;
begin
  GetProcedureAddress(_InitSecurityInterface, secur32, 'InitSecurityInterface' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InitSecurityInterface]
  end;
end;

var
  _SaslEnumerateProfilesA: Pointer;

function SaslEnumerateProfilesA;
begin
  GetProcedureAddress(_SaslEnumerateProfilesA, secur32, 'SaslEnumerateProfilesA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SaslEnumerateProfilesA]
  end;
end;

var
  _SaslEnumerateProfilesW: Pointer;

function SaslEnumerateProfilesW;
begin
  GetProcedureAddress(_SaslEnumerateProfilesW, secur32, 'SaslEnumerateProfilesW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SaslEnumerateProfilesW]
  end;
end;

var
  _SaslEnumerateProfiles: Pointer;

function SaslEnumerateProfiles;
begin
  GetProcedureAddress(_SaslEnumerateProfiles, secur32, 'SaslEnumerateProfiles' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SaslEnumerateProfiles]
  end;
end;

var
  _SaslGetProfilePackageA: Pointer;

function SaslGetProfilePackageA;
begin
  GetProcedureAddress(_SaslGetProfilePackageA, secur32, 'SaslGetProfilePackageA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SaslGetProfilePackageA]
  end;
end;

var
  _SaslGetProfilePackageW: Pointer;

function SaslGetProfilePackageW;
begin
  GetProcedureAddress(_SaslGetProfilePackageW, secur32, 'SaslGetProfilePackageW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SaslGetProfilePackageW]
  end;
end;

var
  _SaslGetProfilePackage: Pointer;

function SaslGetProfilePackage;
begin
  GetProcedureAddress(_SaslGetProfilePackage, secur32, 'SaslGetProfilePackage' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SaslGetProfilePackage]
  end;
end;

var
  _SaslIdentifyPackageA: Pointer;

function SaslIdentifyPackageA;
begin
  GetProcedureAddress(_SaslIdentifyPackageA, secur32, 'SaslIdentifyPackageA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SaslIdentifyPackageA]
  end;
end;

var
  _SaslIdentifyPackageW: Pointer;

function SaslIdentifyPackageW;
begin
  GetProcedureAddress(_SaslIdentifyPackageW, secur32, 'SaslIdentifyPackageW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SaslIdentifyPackageW]
  end;
end;

var
  _SaslIdentifyPackage: Pointer;

function SaslIdentifyPackage;
begin
  GetProcedureAddress(_SaslIdentifyPackage, secur32, 'SaslIdentifyPackage' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SaslIdentifyPackage]
  end;
end;

var
  _SaslInitializeSecurityContextW: Pointer;

function SaslInitializeSecurityContextW;
begin
  GetProcedureAddress(_SaslInitializeSecurityContextW, secur32, 'SaslInitializeSecurityContextW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SaslInitializeSecurityContextW]
  end;
end;

var
  _SaslInitializeSecurityContextA: Pointer;

function SaslInitializeSecurityContextA;
begin
  GetProcedureAddress(_SaslInitializeSecurityContextA, secur32, 'SaslInitializeSecurityContextA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SaslInitializeSecurityContextA]
  end;
end;

var
  _SaslInitializeSecurityContext: Pointer;

function SaslInitializeSecurityContext;
begin
  GetProcedureAddress(_SaslInitializeSecurityContext, secur32, 'SaslInitializeSecurityContext' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SaslInitializeSecurityContext]
  end;
end;

var
  _SaslAcceptSecurityContext: Pointer;

function SaslAcceptSecurityContext;
begin
  GetProcedureAddress(_SaslAcceptSecurityContext, secur32, 'SaslAcceptSecurityContext');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SaslAcceptSecurityContext]
  end;
end;

var
  _SaslSetContextOption: Pointer;

function SaslSetContextOption;
begin
  GetProcedureAddress(_SaslSetContextOption, secur32, 'SaslSetContextOption');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SaslSetContextOption]
  end;
end;

var
  _SaslGetContextOption: Pointer;

function SaslGetContextOption;
begin
  GetProcedureAddress(_SaslGetContextOption, secur32, 'SaslGetContextOption');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SaslGetContextOption]
  end;
end;

var
  _AddSecurityPackageA: Pointer;

function AddSecurityPackageA;
begin
  GetProcedureAddress(_AddSecurityPackageA, secur32, 'AddSecurityPackageA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AddSecurityPackageA]
  end;
end;

var
  _AddSecurityPackageW: Pointer;

function AddSecurityPackageW;
begin
  GetProcedureAddress(_AddSecurityPackageW, secur32, 'AddSecurityPackageW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AddSecurityPackageW]
  end;
end;

var
  _AddSecurityPackage: Pointer;

function AddSecurityPackage;
begin
  GetProcedureAddress(_AddSecurityPackage, secur32, 'AddSecurityPackage' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AddSecurityPackage]
  end;
end;

var
  _DeleteSecurityPackageA: Pointer;

function DeleteSecurityPackageA;
begin
  GetProcedureAddress(_DeleteSecurityPackageA, secur32, 'DeleteSecurityPackageA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DeleteSecurityPackageA]
  end;
end;

var
  _DeleteSecurityPackageW: Pointer;

function DeleteSecurityPackageW;
begin
  GetProcedureAddress(_DeleteSecurityPackageW, secur32, 'DeleteSecurityPackageW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DeleteSecurityPackageW]
  end;
end;

var
  _DeleteSecurityPackage: Pointer;

function DeleteSecurityPackage;
begin
  GetProcedureAddress(_DeleteSecurityPackage, secur32, 'DeleteSecurityPackage' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DeleteSecurityPackage]
  end;
end;

{$ELSE}

function AcquireCredentialsHandleW; external secur32 name 'AcquireCredentialsHandleW';
function AcquireCredentialsHandleA; external secur32 name 'AcquireCredentialsHandleA';
function AcquireCredentialsHandle; external secur32 name 'AcquireCredentialsHandle' + AWSuffix;
function FreeCredentialsHandle; external secur32 name 'FreeCredentialsHandle';
function AddCredentialsW; external secur32 name 'AddCredentialsW';
function AddCredentialsA; external secur32 name 'AddCredentialsA';
function AddCredentials; external secur32 name 'AddCredentials' + AWSuffix;
function InitializeSecurityContextW; external secur32 name 'InitializeSecurityContextW';
function InitializeSecurityContextA; external secur32 name 'InitializeSecurityContextA';
function InitializeSecurityContext; external secur32 name 'InitializeSecurityContext' + AWSuffix;
function AcceptSecurityContext; external secur32 name 'AcceptSecurityContext';
function CompleteAuthToken; external secur32 name 'CompleteAuthToken';
function ImpersonateSecurityContext; external secur32 name 'ImpersonateSecurityContext';
function RevertSecurityContext; external secur32 name 'RevertSecurityContext';
function QuerySecurityContextToken; external secur32 name 'QuerySecurityContextToken';
function DeleteSecurityContext; external secur32 name 'DeleteSecurityContext';
function ApplyControlToken; external secur32 name 'ApplyControlToken';
function QueryContextAttributesW; external secur32 name 'QueryContextAttributesW';
function QueryContextAttributesA; external secur32 name 'QueryContextAttributesA';
function QueryContextAttributes; external secur32 name 'QueryContextAttributes' + AWSuffix;
function SetContextAttributesW; external secur32 name 'SetContextAttributesW';
function SetContextAttributesA; external secur32 name 'SetContextAttributesA';
function SetContextAttributes; external secur32 name 'SetContextAttributes' + AWSuffix;
function QueryCredentialsAttributesW; external secur32 name 'QueryCredentialsAttributesW';
function QueryCredentialsAttributesA; external secur32 name 'QueryCredentialsAttributesA';
function QueryCredentialsAttributes; external secur32 name 'QueryCredentialsAttributes' + AWSuffix;
function FreeContextBuffer; external secur32 name 'FreeContextBuffer';
function MakeSignature; external secur32 name 'MakeSignature';
function VerifySignature; external secur32 name 'VerifySignature';
function EncryptMessage; external secur32 name 'EncryptMessage';
function DecryptMessage; external secur32 name 'DecryptMessage';
function EnumerateSecurityPackagesW; external secur32 name 'EnumerateSecurityPackagesW';
function EnumerateSecurityPackagesA; external secur32 name 'EnumerateSecurityPackagesA';
function EnumerateSecurityPackages; external secur32 name 'EnumerateSecurityPackages' + AWSuffix;
function QuerySecurityPackageInfoW; external secur32 name 'QuerySecurityPackageInfoW';
function QuerySecurityPackageInfoA; external secur32 name 'QuerySecurityPackageInfoA';
function QuerySecurityPackageInfo; external secur32 name 'QuerySecurityPackageInfo' + AWSuffix;
function ExportSecurityContext; external secur32 name 'ExportSecurityContext';
function ImportSecurityContextW; external secur32 name 'ImportSecurityContextW';
function ImportSecurityContextA; external secur32 name 'ImportSecurityContextA';
function ImportSecurityContext; external secur32 name 'ImportSecurityContext' + AWSuffix;
function InitSecurityInterfaceA; external secur32 name 'InitSecurityInterfaceA';
function InitSecurityInterfaceW; external secur32 name 'InitSecurityInterfaceW';
function InitSecurityInterface; external secur32 name 'InitSecurityInterface' + AWSuffix;
function SaslEnumerateProfilesA; external secur32 name 'SaslEnumerateProfilesA';
function SaslEnumerateProfilesW; external secur32 name 'SaslEnumerateProfilesW';
function SaslEnumerateProfiles; external secur32 name 'SaslEnumerateProfiles' + AWSuffix;
function SaslGetProfilePackageA; external secur32 name 'SaslGetProfilePackageA';
function SaslGetProfilePackageW; external secur32 name 'SaslGetProfilePackageW';
function SaslGetProfilePackage; external secur32 name 'SaslGetProfilePackage' + AWSuffix;
function SaslIdentifyPackageA; external secur32 name 'SaslIdentifyPackageA';
function SaslIdentifyPackageW; external secur32 name 'SaslIdentifyPackageW';
function SaslIdentifyPackage; external secur32 name 'SaslIdentifyPackage' + AWSuffix;
function SaslInitializeSecurityContextW; external secur32 name 'SaslInitializeSecurityContextW';
function SaslInitializeSecurityContextA; external secur32 name 'SaslInitializeSecurityContextA';
function SaslInitializeSecurityContext; external secur32 name 'SaslInitializeSecurityContext' + AWSuffix;
function SaslAcceptSecurityContext; external secur32 name 'SaslAcceptSecurityContext';
function SaslSetContextOption; external secur32 name 'SaslSetContextOption';
function SaslGetContextOption; external secur32 name 'SaslGetContextOption';
function AddSecurityPackageA; external secur32 name 'AddSecurityPackageA';
function AddSecurityPackageW; external secur32 name 'AddSecurityPackageW';
function AddSecurityPackage; external secur32 name 'AddSecurityPackage' + AWSuffix;
function DeleteSecurityPackageA; external secur32 name 'DeleteSecurityPackageA';
function DeleteSecurityPackageW; external secur32 name 'DeleteSecurityPackageW';
function DeleteSecurityPackage; external secur32 name 'DeleteSecurityPackage' + AWSuffix;

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}


{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
