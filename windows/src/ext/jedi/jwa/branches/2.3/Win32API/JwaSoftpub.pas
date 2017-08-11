{******************************************************************************}
{                                                                              }
{ Internet Security Authenticode Policy Provider API interface Unit            }
{ for Object Pascal                                                            }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2008 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ Portions created by Philip Dittmann are Copyright (C) 2008                   }
{ Philip Dittmann. All Rights Reserved.                                        }
{ Portions created by Christian Wimmer are Copyright (C) 2008                  }
{ Christian Wimmer. All Rights Reserved.                                       }
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
unit JwaSoftpub;


{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "Softpub.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}


{$IFDEF DELPHI6_UP}
{$ALIGN 8}
{$ELSE}
{$ALIGN+} //WARNING: Incorrect alignment in this Delphi
{$ENDIF DELPHI6_UP}
interface

{$IFDEF JWA_WINDOWS}
  uses JwaWindows;
{$ELSE}
  uses JwaWinType, JwaWinCrypt;
{$ENDIF JWA_WINDOWS}


{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_IMPLEMENTATIONSECTION}


//////////////////////////////////////////////////////////////////////////////
//
// Softpub Policy Provider defines
//----------------------------------------------------------------------------
//  The following are definitions of the Microsoft Authenticode Policy Provider
//  (WINTRUST.DLL's Policy Provider)
//

const
  {$EXTERNALSYM SP_POLICY_PROVIDER_DLL_NAME}
  SP_POLICY_PROVIDER_DLL_NAME        = 'WINTRUST.DLL';


//////////////////////////////////////////////////////////////////////////////
//
// WINTRUST_ACTION_GENERIC_VERIFY_V2 Guid  (Authenticode)
//----------------------------------------------------------------------------
//  Assigned to the pgActionID parameter of WinVerifyTrust to verify the
//  authenticity of a file/object using the Microsoft Authenticode
//  Policy Provider,
//
//          {00AAC56B-CD44-11d0-8CC2-00C04FC295EE}
//
  {$EXTERNALSYM WINTRUST_ACTION_GENERIC_VERIFY_V2}
  WINTRUST_ACTION_GENERIC_VERIFY_V2:    TGUID = '{00aac56b-cd44-11d0-8cc2-00c04fc295ee}';

  {$EXTERNALSYM SP_INIT_FUNCTION}
  SP_INIT_FUNCTION                    = 'SoftpubInitialize';
  {$EXTERNALSYM SP_OBJTRUST_FUNCTION}
  SP_OBJTRUST_FUNCTION                = 'SoftpubLoadMessage';
  {$EXTERNALSYM SP_SIGTRUST_FUNCTION}
  SP_SIGTRUST_FUNCTION                = 'SoftpubLoadSignature';
  {$EXTERNALSYM SP_CHKCERT_FUNCTION}
  SP_CHKCERT_FUNCTION                 = 'SoftpubCheckCert';
  {$EXTERNALSYM SP_FINALPOLICY_FUNCTION}
  SP_FINALPOLICY_FUNCTION             = 'SoftpubAuthenticode';
  {$EXTERNALSYM SP_CLEANUPPOLICY_FUNCTION}
  SP_CLEANUPPOLICY_FUNCTION           = 'SoftpubCleanup';

//////////////////////////////////////////////////////////////////////////////
//
// WINTRUST_ACTION_TRUSTPROVIDER_TEST (Authenticode TEST)
//----------------------------------------------------------------------------
//  Assigned to the pgActionID parameter of WinVerifyTrust to dump
//  the CRYPT_PROVIDER_DATA structure to a file after calling the
//  Authenticode Policy Provider.
//
//          {573E31F8-DDBA-11d0-8CCB-00C04FC295EE}
//
  {$EXTERNALSYM WINTRUST_ACTION_TRUSTPROVIDER_TEST}
  WINTRUST_ACTION_TRUSTPROVIDER_TEST:   TGUID = '{573e31f8-ddba-11d0-8ccb-00c04fc295ee}';

  {$EXTERNALSYM SP_TESTDUMPPOLICY_FUNCTION_TEST}
  SP_TESTDUMPPOLICY_FUNCTION_TEST     = 'SoftpubDumpStructure';


//////////////////////////////////////////////////////////////////////////////
//
// WINTRUST_ACTION_GENERIC_CERT_VERIFY
//----------------------------------------------------------------------------
//  Assigned to the pgActionID parameter of WinVerifyTrust to verify
//  a certificate chain only.  This is only valid when passing in a
//  certificate context in the WinVerifyTrust input structures.
//
//          {189A3842-3041-11d1-85E1-00C04FC295EE}
//
  {$EXTERNALSYM WINTRUST_ACTION_GENERIC_CERT_VERIFY}
  WINTRUST_ACTION_GENERIC_CERT_VERIFY:  TGUID = '{189a3842-3041-11d1-85e1-00c04fc295ee}';

  {$EXTERNALSYM SP_GENERIC_CERT_INIT_FUNCTION}
  SP_GENERIC_CERT_INIT_FUNCTION       = 'SoftpubDefCertInit';


//////////////////////////////////////////////////////////////////////////////
//
// WINTRUST_ACTION_GENERIC_CHAIN_VERIFY
//----------------------------------------------------------------------------
//  Assigned to the pgActionID parameter of WinVerifyTrust to verify
//  certificate chains created from any object type: file, cert, signer, ...
//  A callback is provided to implement the final chain policy using
//  the chain context for each signer and counter signer.
//
//          {fc451c16-ac75-11d1-b4b8-00c04fb66ea0}
//
  {$EXTERNALSYM WINTRUST_ACTION_GENERIC_CHAIN_VERIFY}
	WINTRUST_ACTION_GENERIC_CHAIN_VERIFY: TGUID = '{fc451c16-ac75-11d1-b4b8-00c04fb66ea0}';

  {$EXTERNALSYM GENERIC_CHAIN_FINALPOLICY_FUNCTION}
  GENERIC_CHAIN_FINALPOLICY_FUNCTION      = 'GenericChainFinalProv';
  {$EXTERNALSYM GENERIC_CHAIN_CERTTRUST_FUNCTION}
  GENERIC_CHAIN_CERTTRUST_FUNCTION        = 'GenericChainCertificateTrust';

type
  PPWTD_GENERIC_CHAIN_POLICY_SIGNER_INFO = ^PWTD_GENERIC_CHAIN_POLICY_SIGNER_INFO; //no external!

  {$EXTERNALSYM PWTD_GENERIC_CHAIN_POLICY_SIGNER_INFO}
  PWTD_GENERIC_CHAIN_POLICY_SIGNER_INFO = ^_WTD_GENERIC_CHAIN_POLICY_SIGNER_INFO;

  {$EXTERNALSYM _WTD_GENERIC_CHAIN_POLICY_SIGNER_INFO}
  _WTD_GENERIC_CHAIN_POLICY_SIGNER_INFO = record
    cbStructOrSize : DWORD;

    pChainContext : PCCERT_CHAIN_CONTEXT;

    // SGNR_TYPE_TIMESTAMP defined in wintrust.h
    dwSignerType : DWORD;
    pMsgSignerInfo : PCMSG_SIGNER_INFO;
    dwError : DWORD;

    cCounterSigner : DWORD;
    rgpCounterSigner : PPWTD_GENERIC_CHAIN_POLICY_SIGNER_INFO;
  end;

  {$EXTERNALSYM WTD_GENERIC_CHAIN_POLICY_SIGNER_INFO}
  WTD_GENERIC_CHAIN_POLICY_SIGNER_INFO = _WTD_GENERIC_CHAIN_POLICY_SIGNER_INFO;

//TODO: define PCRYPT_PROVIDER_DATA and restore it here
  {$EXTERNALSYM PFN_WTD_GENERIC_CHAIN_POLICY_CALLBACK}
  PFN_WTD_GENERIC_CHAIN_POLICY_CALLBACK = function(
    {IN} pProvData : {PCRYPT_PROVIDER_DATA in JwaWintrust}Pointer;
    {IN} dwStepError : DWORD;
    {IN} dwRegPolicySettings : DWORD;
    {IN} cSigner : DWORD;
    {IN} rgpSigner : PPWTD_GENERIC_CHAIN_POLICY_SIGNER_INFO;
    {IN} pvPolicyArg : Pointer
    ) : HRESULT; stdcall;

    // The fields in the following data structure are passed to
// CertGetCertificateChain().

  {$EXTERNALSYM _WTD_GENERIC_CHAIN_POLICY_CREATE_INFO}
  _WTD_GENERIC_CHAIN_POLICY_CREATE_INFO = record
    cbStructOrSize : DWORD;

    hChainEngine : HCERTCHAINENGINE;
    pChainPara : PCERT_CHAIN_PARA;
    dwFlags : DWORD;
    pvReserved : Pointer;
  end;
  {$EXTERNALSYM WTD_GENERIC_CHAIN_POLICY_CREATE_INFO}
  WTD_GENERIC_CHAIN_POLICY_CREATE_INFO = _WTD_GENERIC_CHAIN_POLICY_CREATE_INFO;
  {$EXTERNALSYM PWTD_GENERIC_CHAIN_POLICY_CREATE_INFO}
  PWTD_GENERIC_CHAIN_POLICY_CREATE_INFO = ^WTD_GENERIC_CHAIN_POLICY_CREATE_INFO;

  {$EXTERNALSYM _WTD_GENERIC_CHAIN_POLICY_DATA}
  _WTD_GENERIC_CHAIN_POLICY_DATA = record
    cbStructOrSize : DWORD;

    pSignerChainInfo : PWTD_GENERIC_CHAIN_POLICY_CREATE_INFO;
    pCounterSignerChainInfo : PWTD_GENERIC_CHAIN_POLICY_CREATE_INFO;
    pfnPolicyCallback : PFN_WTD_GENERIC_CHAIN_POLICY_CALLBACK;
    pvPolicyArg : Pointer;
  end;
  {$EXTERNALSYM WTD_GENERIC_CHAIN_POLICY_DATA}
  WTD_GENERIC_CHAIN_POLICY_DATA = _WTD_GENERIC_CHAIN_POLICY_DATA;
  {$EXTERNALSYM PWTD_GENERIC_CHAIN_POLICY_DATA}
  PWTD_GENERIC_CHAIN_POLICY_DATA = ^WTD_GENERIC_CHAIN_POLICY_DATA;

//////////////////////////////////////////////////////////////////////////////
//
// HTTPSPROV_ACTION Guid  (Authenticode add-on)
//----------------------------------------------------------------------------
//  Assigned to the pgActionID parameter of WinVerifyTrust to verify the
//  SSL/PCT connections through IE.
//
//          {573E31F8-AABA-11d0-8CCB-00C04FC295EE}
//
const
  {$EXTERNALSYM HTTPSPROV_ACTION}
  HTTPSPROV_ACTION:                     TGUID = '{573e31f8-aaba-11d0-8ccb-00c04fc295ee}';

  {$EXTERNALSYM HTTPS_FINALPOLICY_FUNCTION}
  HTTPS_FINALPOLICY_FUNCTION          = 'HTTPSFinalProv';
  {$EXTERNALSYM HTTPS_CHKCERT_FUNCTION}
  HTTPS_CHKCERT_FUNCTION              = 'HTTPSCheckCertProv';
  {$EXTERNALSYM HTTPS_CERTTRUST_FUNCTION}
  HTTPS_CERTTRUST_FUNCTION            = 'HTTPSCertificateTrust';

//////////////////////////////////////////////////////////////////////////////
//
// OFFICESIGN_ACTION_VERIFY Guid  (Authenticode add-on)
//----------------------------------------------------------------------------
//  Assigned to the pgActionID parameter of WinVerifyTrust to verify the
//  authenticity of a Structured Storage file using the Microsoft Office
//  Authenticode add-on Policy Provider,
//
//          {5555C2CD-17FB-11d1-85C4-00C04FC295EE}
//
  {$EXTERNALSYM OFFICESIGN_ACTION_VERIFY}
  OFFICESIGN_ACTION_VERIFY:             TGUID = '{5555c2cd-17fb-11d1-85c4-00c04fc295ee}';

  {$EXTERNALSYM OFFICE_POLICY_PROVIDER_DLL_NAME}
  OFFICE_POLICY_PROVIDER_DLL_NAME             = SP_POLICY_PROVIDER_DLL_NAME;
  {$EXTERNALSYM OFFICE_INITPROV_FUNCTION}
  OFFICE_INITPROV_FUNCTION                    = 'OfficeInitializePolicy';
  {$EXTERNALSYM OFFICE_CLEANUPPOLICY_FUNCTION}
  OFFICE_CLEANUPPOLICY_FUNCTION               = 'OfficeCleanupPolicy';


//////////////////////////////////////////////////////////////////////////////
//
// DRIVER_ACTION_VERIFY Guid  (Authenticode add-on)
//----------------------------------------------------------------------------
//  Assigned to the pgActionID parameter of WinVerifyTrust to verify the
//  authenticity of a WHQL signed driver.  This is an Authenticode add-on
//  Policy Provider,
//
//          {F750E6C3-38EE-11d1-85E5-00C04FC295EE}
//
  {$EXTERNALSYM DRIVER_ACTION_VERIFY}
  DRIVER_ACTION_VERIFY:                 TGUID = '{c3e650f7-38ee-11d1-85e5-00c04fc295ee}';

  {$EXTERNALSYM DRIVER_INITPROV_FUNCTION}
  DRIVER_INITPROV_FUNCTION                    = 'DriverInitializePolicy';
  {$EXTERNALSYM DRIVER_FINALPOLPROV_FUNCTION}
  DRIVER_FINALPOLPROV_FUNCTION                = 'DriverFinalPolicy';
  {$EXTERNALSYM DRIVER_CLEANUPPOLICY_FUNCTION}
  DRIVER_CLEANUPPOLICY_FUNCTION               = 'DriverCleanupPolicy';

type
  {$EXTERNALSYM DRIVER_VER_MAJORMINOR_}
  DRIVER_VER_MAJORMINOR_ = record
    dwMajor,
    dwMinor : DWORD;
  end;

  {$EXTERNALSYM DRIVER_VER_MAJORMINOR}
  DRIVER_VER_MAJORMINOR = DRIVER_VER_MAJORMINOR_;

  {$EXTERNALSYM DRIVER_VER_INFO_}
  DRIVER_VER_INFO_ = record
    cbStruct : DWORD;               // IN - set to sizeof(DRIVER_VER_INFO)

    dwReserved1,            // IN - set to NULL
    dwReserved2 : ULONG_PTR;            // IN - set to NULL

    dwPlatform,             // IN - OPTIONAL: platform to use
    dwVersion : DWORD;              // IN - OPTIONAL: major version to use (NOT USED!!!)

    wszVersion,   // OUT: version string from catalog file
    wszSignedBy : array[0..MAX_PATH-1] of WideChar;  // OUT: signer display name from certificate
    pcSignerCertContext : PCCERT_CONTEXT;    // OUT: client MUST free this!!!

    sOSVersionLow,          // IN - OPTIONAL: lowest compatible version
    sOSVersionHigh : DRIVER_VER_MAJORMINOR;         // IN - OPTIONAL: highest compatible version

    dwBuildNumberLow,       // IN - OPTIONAL: added to sOSVersionLow as
                                                                //      third node for finer version granularity
    dwBuildNumberHigh : DWORD;      // IN - OPTIONAL: added to sOSVersionHigh as
                                                                //      third node for finer version granularity

    //
    // NOTES:
    // 1. dwPlatform _must_ be set to a non-zero value in order for proper version checking to be done.
    // 2. dwVersion is no longer used, sOSVersionLow and sOsVersionhigh have taken its place
    // 3. If dwBuildNumberLow and dwBuildNumberHigh are 0, they are unused.  Otherwise, they are considered
    //    to be extensions of sOSVersionLow and sOSVersionHigh respectively.  Make special note of this when
    //    reading note 4.
    // 4. If you are validating against a single OS version, then set both sOSVersionLow and sOSVersion high,
    //    to the version you are validating against.  If sOSVersionLow and sOSVersionHigh are different, then
    //    the validation is done for the whole version range, from sOSVersionLow to sOSVersionHigh.
    //
  end;

  {$EXTERNALSYM DRIVER_VER_INFO}
  DRIVER_VER_INFO = DRIVER_VER_INFO_;
  {$EXTERNALSYM PDRIVER_VER_INFO}
  PDRIVER_VER_INFO = ^DRIVER_VER_INFO;

//  WINTRUST_ACTION_GENERIC_VERIFY //is no longer defined in the PSDK headers

{$ENDIF JWA_IMPLEMENTATIONSECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
implementation
uses JwaWinDllNames;
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}


{$IFNDEF JWA_INCLUDEMODE}
const
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}

{$ELSE}


{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
