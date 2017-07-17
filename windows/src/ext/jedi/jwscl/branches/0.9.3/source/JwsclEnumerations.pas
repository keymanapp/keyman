{
Description
Project JEDI Windows Security Code Library (JWSCL)

This unit provides conversion functions from windows api constants to delphi enumeration types and vice versa.

Author
Christian Wimmer

License
The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
you may not use this file except in compliance with the License. You may obtain a copy of the
License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the
GNU Lesser General Public License (the  "LGPL License"), in which case the
provisions of the LGPL License are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the LGPL License and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting  the provisions above and
replace  them with the notice and other provisions required by the LGPL
License.  If you do not delete the provisions above, a recipient may use
your version of this file under either the MPL or the LGPL License.

For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html

Note
The Original Code is JwsclEnumerations.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.




}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclEnumerations;
{$INCLUDE ..\includes\Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $
//do not move header comment from above unit declaration!

interface

uses
  SysUtils,
  jwaWindows,
  JwsclTypes,
  JwsclConstants
  ;
{$ENDIF SL_OMIT_SECTIONS}
{$IFNDEF SL_IMPLEMENTATION_SECTION}

type
  {<B>TJwEnumMap</B> provides class methods to convert windows api constants
   to delphi enumeration types and vice versa.
   There is no need to create an instance of it.}
  TJwEnumMap = class
  public
    class function ConvertInheritFlags(
      const FlagSet: TJwInheritFlagSet): Cardinal; overload; virtual;
    class function ConvertInheritFlags(
      const FlagBits: Cardinal): TJwInheritFlagSet; overload; virtual;

    
    class function ConvertSecurityInformation(
      const FlagSet: TJwSecurityInformationFlagSet): Cardinal;
      overload; virtual;
    class function ConvertSecurityInformation(
      const FlagBits: TSecurityInformation): TJwSecurityInformationFlagSet;
      overload; virtual;

    class function ConvertSecurityControl(const ControlSet:
      TJwSecurityDescriptorControlSet): jwaWindows.TSecurityDescriptorControl;
      overload; virtual;
    class function ConvertSecurityControl(const Control:
      jwaWindows.TSecurityDescriptorControl): TJwSecurityDescriptorControlSet;
      overload; virtual;

    class function ConvertFlags(FlagSet: TJwSecurityDialogFlags): Cardinal;
      overload; virtual;
    class function ConvertFlags(Flags: Cardinal): TJwSecurityDialogFlags;
      overload; virtual;

    {<B>ConvertAceFlags</B> converts a set of ACE flags to a bit combined Cardinal value.
     @param AceFlags receives the set of flags to be converted. It can be emtpy [].
            See TJwAceFlags  for more information. 
     @return The return value contains the set as a value. 
    }
    class function ConvertAceFlags(const AceFlags: TJwAceFlags): Cardinal; overload; virtual;

    {<B>ConvertAceFlags</B> converts a cardianl value to set of ACE flags.
     @param AceFlags receives the value to be converted to a set of flags.
            Unknown bits are ignored in the result.  
     @return The return value contains the set of ace flags. See TJwAceFlags  for more information. 
    }
    class function ConvertAceFlags(
      const AceFlags: Cardinal): TJwAceFlags; overload; virtual;

    class function ConvertToCredentialFlag(
      const CredFlags: Cardinal): TJwCredentialFlagSet; overload; virtual;
    class function ConvertToCredentialFlag(
      const CredFlags: TJwCredentialFlagSet): Cardinal; overload; virtual;

     class function ConvertProtectFlags(const Flags : DWORD)
      : TJwCryptProtectFlagSet; overload;
    class function ConvertProtectFlags(const Flags : TJwCryptProtectFlagSet)
      : DWORD; overload;


    class function ConvertProtectPromptFlags(const Flags : DWORD)
      : TJwCryptProtectOnPromptFlagSet; overload;
    class function ConvertProtectPromptFlags(const Flags : TJwCryptProtectOnPromptFlagSet)
      : DWORD; overload;

    class function ConvertProtectMemoryFlags(const Flags : DWORD)
      : TJwProtectMemoryFlagSet; overload;
    class function ConvertProtectMemoryFlags(const Flags : TJwProtectMemoryFlagSet)
      : DWORD; overload;


    class function ConvertAttributes(
      const Attributes: Cardinal): TJwSidAttributeSet; overload;
    class function ConvertAttributes(
      const Attributes: TJwSidAttributeSet): Cardinal; overload;

    class function ConvertHashAlgorithm(
      const Alg: TJwHashAlgorithm): DWORD; overload;
    class function ConvertHashAlgorithm(
      const Alg: DWORD): TJwHashAlgorithm; overload;

    class function ConvertCSPType(
      const CSPType: TJwCSPType): DWORD; overload;
    class function ConvertCSPType(
      const CSPType: DWORD): TJwCSPType; overload;

    class function ConvertCSPCreationFlags(
      const FlagSet: TJwCSPCreationFlagSet): Cardinal; overload;
    class function ConvertCSPCreationFlags(
      const FlagBits: Cardinal): TJwCSPCreationFlagSet; overload;

    class function ConvertKeyPairType(
      const KeyPairType: TJwKeyPairType): Cardinal; overload;
    class function ConvertKeyPairType(
      const KeyPairType: Cardinal): TJwKeyPairType; overload;

    class function ConvertKeyFlagSet(
      const FlagSet: TJwKeyFlagSet): Cardinal; overload;
    class function ConvertKeyFlagSet(
      const FlagBits: Cardinal): TJwKeyFlagSet; overload;

    class function ConvertKeyExportKind(
      const KeyExportKind: TJwKeyExportKind): Cardinal; overload;
    class function ConvertKeyExportKind(
      const KeyExportKind: Cardinal): TJwKeyExportKind; overload;

    class function ConvertEncryptionAlgorithm(
      const Algorithm: TJwEncryptionAlgorithm): Cardinal; overload;
    class function ConvertEncryptionAlgorithm(
      const AlgId: Cardinal): TJwEncryptionAlgorithm; overload;


    class function ConvertMandatoryPolicyFlags(
      const FlagSet: TJwMandatoryPolicyFlagSet): Cardinal; overload;
    class function ConvertMandatoryPolicyFlags(
      const FlagBits: Cardinal): TJwMandatoryPolicyFlagSet; overload;

    class function ConvertTokenMandatoryPolicyFlags(
      const FlagSet: TJwTokenMandatoryPolicies): Cardinal; overload;
    class function ConvertTokenMandatoryPolicyFlags(
      const FlagBits: Cardinal): TJwTokenMandatoryPolicies; overload;


    class function ConvertAuthZResourceManager(
      const FlagSet: TJwAuthZResourceManagerFlags): Cardinal; overload;
    class function ConvertAuthZResourceManager(
      const FlagBits: Cardinal): TJwAuthZResourceManagerFlags; overload;

    class function ConvertAuthZSidContextFlags(
      const FlagSet: TAuthZSidContextFlags): Cardinal; overload;
    class function ConvertAuthZSidContextFlags(
      const FlagBits: Cardinal): TAuthZSidContextFlags; overload;

    class function ConvertReplyErrorEnum(
      const Flags: TJwReplyErrorEnum): Cardinal; overload;
    class function ConvertReplyErrorEnum(
      const Flags: Cardinal): TJwReplyErrorEnum; overload;


    class function ConvertAceType(
      const AceType: TJwAceType): Cardinal; overload;
    class function ConvertAceType(
      const AceType: Cardinal): TJwAceType; overload;


     class function ConvertJobLimitType(
      const FlagSet: TJwJobLimits): Cardinal; overload;
    class function ConvertJobLimit(
      const FlagBits: Cardinal): TJwJobLimits; overload;

    class function ConvertJobUiLimitType(
      const FlagSet: TJwJobUiLimits): Cardinal; overload;
    class function ConvertJobUiLimit(
      const FlagBits: Cardinal): TJwJobUiLimits; overload;

    class function ConvertJobMessageType(
      const FlagSet: TJwJobMessages): Cardinal; overload;
    class function ConvertJobMessage(
      const FlagBits: Cardinal): TJwJobMessages; overload;

    class function ConvertSecurityCapabilityType(
      const FlagSet: TJwSecurityCapabilities): Cardinal; overload;
    class function ConvertSecurityCapabilityMessage(
      const FlagBits: Cardinal): TJwSecurityCapabilities; overload;

  end;


{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INTERFACE_SECTION}

{The following declarations are private.
If you decide to extend or change the delphi enumeration type
you must make sure that the map from enum typename to constant is correct.

I use comma in front of the enumerations because pasdoc can recognize
comments behind of declaration
}

const
   InheritFlagsValues : array[TJwInheritFlag] of Cardinal = (
    SEF_DACL_AUTO_INHERIT //ifDaclAutoInherit
    ,SEF_SACL_AUTO_INHERIT //ifSaclAutoInherit
    ,SEF_DEFAULT_DESCRIPTOR_FOR_OBJECT //ifDefaultDescriptor
    ,SEF_AVOID_PRIVILEGE_CHECK //ifAvoidPrivilegeCheck
    ,SEF_AVOID_OWNER_CHECK //ifAvoidOwnerCheck
    ,SEF_DEFAULT_OWNER_FROM_PARENT //ifDefaultOwnerFromPArent
    ,SEF_DEFAULT_GROUP_FROM_PARENT //ifDefaultGroupFromParent
    ,$100//SEF_MACL_NO_WRITE_UP //ifMaclNoWriteUp
    ,$200//SEF_MACL_NO_READ_UP //ifMaclNoReadUp
    ,$400//SEF_MACL_NO_EXECUTE_UP //ifMaclNoExecuteUp
    ,$1000//SEF_AVOID_OWNER_RESTRICTION //ifAvoidOwnerRestriction
    );

  SecurityInformationValues : array[TJwSecurityInformationFlag]
    of Cardinal = (
    OWNER_SECURITY_INFORMATION//siOwnerSecurityInformation
    ,GROUP_SECURITY_INFORMATION//siGroupSecurityInformation
    ,DACL_SECURITY_INFORMATION//siDaclSecurityInformation
    ,SACL_SECURITY_INFORMATION//siSaclSecurityInformation
    ,LABEL_SECURITY_INFORMATION//siLabelSecurityInformation
    ,PROTECTED_DACL_SECURITY_INFORMATION//siProtectedDaclSecurityInformation
    ,PROTECTED_SACL_SECURITY_INFORMATION//siProtectedSaclSecurityInformation
    ,UNPROTECTED_DACL_SECURITY_INFORMATION//siUnprotectedDaclSecurityInformation
    ,UNPROTECTED_SACL_SECURITY_INFORMATION//siUnprotectedSaclSecurityInformation
    );

  SecurityDescriptorControlValues : array[TJwSecurityDescriptorControl]
    of Cardinal = (
    SE_OWNER_DEFAULTED//sdcOwnerDefaulted
    ,SE_GROUP_DEFAULTED//sdcGroupDefaulted
    ,SE_DACL_PRESENT//sdcDaclPresent
    ,SE_DACL_DEFAULTED//sdcDaclDefaulted
    ,SE_SACL_PRESENT//sdcSaclPresent
    ,SE_SACL_DEFAULTED//sdcSaclDefaulted
    ,SE_DACL_AUTO_INHERIT_REQ//sdcDaclAutoInheritReq
    ,SE_SACL_AUTO_INHERIT_REQ//sdcSaclAutoInheritReq
    ,SE_DACL_AUTO_INHERITED//sdcDaclAutoInherited
    ,SE_SACL_AUTO_INHERITED//sdcSaclAutoInherited
    ,SE_DACL_PROTECTED//sdcDaclProtected
    ,SE_SACL_PROTECTED//sdcSaclProtected
    ,SE_RM_CONTROL_VALID//sdcRmControlValid
    ,SE_SELF_RELATIVE//sdcSelfRelative
    );

  SecurityDialogFlagValues : array[TJwSecurityDialogFlag]
    of Cardinal = (
    SI_EDIT_PERMS//sdfEditDacl
    ,SI_EDIT_AUDITS//sdfEditSacl
    ,SI_EDIT_OWNER//sdfEditOwner
    ,SI_CONTAINER//sdfContainer
    ,SI_READONLY//sdfReadOnly
    ,SI_ADVANCED//sdfAdvanced
    ,SI_RESET//sdfReset
    ,SI_OWNER_READONLY//sdfOwnerReadOnly
    ,SI_EDIT_PROPERTIES//sdfEditProperties
    ,SI_OWNER_RECURSE//sdfOwnerRecurse
    ,SI_NO_ACL_PROTECT//sdfNoAclProtect
    ,SI_NO_TREE_APPLY//sdfNoTreeApply
    ,SI_SERVER_IS_DC//sdfServerIsDc
    ,SI_RESET_DACL_TREE//sdfResetDaclTree
    ,SI_RESET_SACL_TREE//sdfResetSaclTree
    ,SI_OBJECT_GUID//sdfObjectGuid
    ,SI_EDIT_EFFECTIVE//sdfEditEffective
    ,SI_RESET_DACL//sdfResetDacl
    ,SI_RESET_SACL//sdfResetSacl
    ,SI_RESET_OWNER//sdfResetOwner
    ,SI_NO_ADDITIONAL_PERMISSION//sdfNoAdditionalPermission
    ,SI_MAY_WRITE//sdfMayWrite
    ,SI_PAGE_TITLE//sdfPageTitle
  );

  AceFlagValues : Array[TJwAceFlag] of Cardinal = (
    OBJECT_INHERIT_ACE//afObjectInheritAce
    ,CONTAINER_INHERIT_ACE//afContainerInheritAce
    ,NO_PROPAGATE_INHERIT_ACE//afNoPropagateInheritAce
    ,INHERIT_ONLY_ACE//afInheritOnlyAce
    ,INHERITED_ACE//afInheritedAce
    ,VALID_INHERIT_FLAGS//afValidInheritFlags
    ,SUCCESSFUL_ACCESS_ACE_FLAG//afSuccessfulAccessAceFlag
    ,FAILED_ACCESS_ACE_FLAG//afFailedAccessAceFlag
  );

  //TJwCredentialFlag    = (
  CredentialFlagValues : Array[TJwCredentialFlag] of Cardinal = (
    CREDUI_FLAGS_ALWAYS_SHOW_UI//cfFlagsAlwaysShowUi
    ,CREDUI_FLAGS_DO_NOT_PERSIST//cfFlagsDoNotPersist
    ,CREDUI_FLAGS_EXCLUDE_CERTIFICATES//cfFlagsExcludeCertificates
    ,CREDUI_FLAGS_EXPECT_CONFIRMATION//cfFlagsExpectConfirmation
    ,CREDUI_FLAGS_GENERIC_CREDENTIALS//cfFlagsGenericCredentials
    ,CREDUI_FLAGS_INCORRECT_PASSWORD//cfFlagsIncorrectPassword
    ,CREDUI_FLAGS_PERSIST//cfFlagsPersist
    ,CREDUI_FLAGS_REQUEST_ADMINISTRATOR//cfFlagsRequestAdministrator
    ,CREDUI_FLAGS_REQUIRE_CERTIFICATE//cfFlagsRequireCertificate
    ,CREDUI_FLAGS_REQUIRE_SMARTCARD//cfFlagsRequireSmartCard
    ,CREDUI_FLAGS_SERVER_CREDENTIAL//cfFlagsServerCredential
    ,CREDUI_FLAGS_SHOW_SAVE_CHECK_BOX//cfFlagsShowSaveCheckBox
    ,CREDUI_FLAGS_USERNAME_TARGET_CREDENTIALS//cfFlagsUserNameTargetCredentials
  );

  CryptProtectOnPromptFlagValues : Array[TJwCryptProtectOnPromptFlag] of Cardinal = (
    0
    ,CRYPTPROTECT_PROMPT_ON_PROTECT //cppf_PromptOnProtect
    ,CRYPTPROTECT_PROMPT_ON_UNPROTECT //cppf_PromptOnUnprotect
  );

  ProtectMemoryFlagSetValues : Array[TJwProtectMemoryFlag] of Cardinal = (
    CRYPTPROTECTMEMORY_SAME_PROCESS //pmSameProcess
    ,CRYPTPROTECTMEMORY_CROSS_PROCESS //pmCrossProcess
    ,CRYPTPROTECTMEMORY_SAME_LOGON //pmSameLogon
  );

  CryptProtectFlag : Array[TJwCryptProtectFlag] of Cardinal = (
    CRYPTPROTECT_LOCAL_MACHINE//cfLocalMachine
    ,CRYPTPROTECT_UI_FORBIDDEN//cfUiFobidden
    //Vista only 
   { ,CRYPTPROTECT_AUDIT//cfAudit
    CRYPTPROTECT_VERIFY_PROTECTION//cfVerifyProtection }
  );

  HashAlgorithmValues: array[TJwHashAlgorithm] of Cardinal = (
    0  //haUnknown
   ,CALG_MD2 //haMD2
   ,CALG_MD4 //haMD4
   ,CALG_MD5 //haMD5
   ,CALG_SHA //haSHA
   ,CALG_MAC //haMAC
   ,CALG_HMAC //haHMAC
   );


  CSPTypeValues: array[TJwCSPType] of Cardinal = (
    0 //ctUnknown
   ,PROV_RSA_FULL //ctRsaFull
   //,PROV_RSA_AES //ctRsaAes
   ,PROV_RSA_SIG  //ctRsaSig
   ,PROV_RSA_SCHANNEL //ctRsaSchannel
   ,PROV_DSS  //ctDss
   ,PROV_DSS_DH   //ctDssDh
   ,PROV_DH_SCHANNEL   //ctDhSchannel
   ,PROV_FORTEZZA   //ctFortezza
   ,PROV_MS_EXCHANGE //ctMsExchange
   ,PROV_SSL   //ctSsl
   );

  CSPCreationFlagValues: array[TJwCSPCreationFlag] of Cardinal = (
    CRYPT_VERIFYCONTEXT
   ,CRYPT_NEWKEYSET
   ,CRYPT_MACHINE_KEYSET
   //, CRYPT_DELETEKEYSET
   ,CRYPT_SILENT
   );

  KeyPairTypeValues: array[TJwKeyPairType] of Cardinal = (
    AT_KEYEXCHANGE
   ,AT_SIGNATURE
   ,0
   );

  KeyFlagValues: array[TJwKeyFlag] of Cardinal = (
    CRYPT_CREATE_SALT
  // ,CRYPT_ARCHIVABLE
   ,CRYPT_PREGEN
   ,CRYPT_EXPORTABLE
   ,CRYPT_NO_SALT
   ,CRYPT_USER_PROTECTED
   ,CRYPT_OAEP
   ,CRYPT_UPDATE_KEY
   ,CRYPT_DESTROYKEY
   ,CRYPT_SSL2_FALLBACK
   );

  KeyExportKindValues: array[TJwKeyExportKind] of Cardinal = (
    OPAQUEKEYBLOB //kekOpaque
   ,PRIVATEKEYBLOB  //kekPrivate
   ,PUBLICKEYBLOB   //kekPublic
   ,SIMPLEBLOB      //kekSimple
   ,PLAINTEXTKEYBLOB //kekPlaintext
   ,SYMMETRICWRAPKEYBLOB //kekSymmetricWrap
   );

  EncryptionAlgorithmValues: array[TJwEncryptionAlgorithm] of Cardinal =(
    0 //eaUnknown,
   ,CALG_RSA_SIGN //eaRsaSign
   ,CALG_RSA_KEYX //eaRsaKeyX
   ,CALG_DES //eaDes
   ,CALG_3DES //ea3Des
   ,CALG_3DES_112 //ea3Des112
   ,CALG_RC2 //eaRc2
   ,CALG_RC4 //eaRc4
   ,CALG_RC5 //eaRc5
   ,CALG_SEAL //eaSeal
   ,CALG_DH_SF //eaDhSf
   ,CALG_DH_EPHEM //eaDhEphem
   ,CALG_AGREEDKEY_ANY //eaAgreedKeyAny
   ,CALG_KEA_KEYX //eaKeaKeyX
   ,CALG_SKIPJACK //eaSkipjack
   ,CALG_TEK //eaTek
   ,CALG_CYLINK_MEK //eaCylinkMek
   );


  MandatoryPolicyFlagValues : array[TJwMandatoryPolicy] of Cardinal = (
   SYSTEM_MANDATORY_LABEL_NO_WRITE_UP
   ,SYSTEM_MANDATORY_LABEL_NO_READ_UP
   ,SYSTEM_MANDATORY_LABEL_NO_EXECUTE_UP
   );

  TokenMandatoryPolicyFlagValues : array[TJwTokenMandatoryPolicy] of Cardinal =
   (
    TOKEN_MANDATORY_POLICY_OFF,
    TOKEN_MANDATORY_POLICY_NO_WRITE_UP,
    TOKEN_MANDATORY_POLICY_NEW_PROCESS_MIN
   );
  AuthZResourceManagerValues : array[TJwAuthZResourceManagerFlag] of Cardinal = (
   0
   ,AUTHZ_RM_FLAG_NO_AUDIT
   ,AUTHZ_RM_FLAG_INITIALIZE_UNDER_IMPERSONATION
   );

  AuthZSidContextValues : array[TAuthZSidContextFlag] of Cardinal = (
   0
   ,AUTHZ_SKIP_TOKEN_GROUPS
   ,AUTHZ_REQUIRE_S4U_LOGON
   ,$8//,AUTHZ_COMPUTE_PRIVILEGES
   );

  ReplyErrorEnumValues : array[TJwReplyErrorEnum] of Cardinal = (
    ERROR_SUCCESS,//reSuccess,
    ERROR_PRIVILEGE_NOT_HELD,//rePrivilegeNotHeld,
    ERROR_ACCESS_DENIED,//reAccessDenied,
    0//reUnknown
    );

   AceTypeEnumValues : array[TJwAceType] of Cardinal = (
    SYSTEM_AUDIT_ACE_TYPE,//actAudit,
    SYSTEM_AUDIT_CALLBACK_ACE_TYPE,//actAuditCallback,
    SYSTEM_AUDIT_OBJECT_ACE_TYPE,//actAuditObject,
    SYSTEM_AUDIT_CALLBACK_OBJECT_ACE_TYPE,//actAuditCallbackObject,

    SYSTEM_MANDATORY_LABEL_ACE_TYPE,//actMandatory,

    ACCESS_ALLOWED_ACE_TYPE,//actAllow,
    ACCESS_ALLOWED_CALLBACK_ACE_TYPE,//actAllowCallback,
    ACCESS_ALLOWED_OBJECT_ACE_TYPE,//actAllowObject,
    ACCESS_ALLOWED_CALLBACK_ACE_TYPE,//actAllowCallbackObject,

    ACCESS_DENIED_ACE_TYPE,//actDeny,
    ACCESS_DENIED_CALLBACK_ACE_TYPE,//actDenyCallback,
    ACCESS_DENIED_OBJECT_ACE_TYPE,//actDenyObject,
    ACCESS_DENIED_CALLBACK_OBJECT_ACE_TYPE,//actDenyCallbackObject

    $FFFFF
   );




   JwJobLimitEnumValues : array[TJwJobLimit] of Cardinal = (
      JOB_OBJECT_LIMIT_WORKINGSET,
      JOB_OBJECT_LIMIT_PROCESS_TIME,
      JOB_OBJECT_LIMIT_JOB_TIME,
      JOB_OBJECT_LIMIT_ACTIVE_PROCESS,
      JOB_OBJECT_LIMIT_AFFINITY,
      JOB_OBJECT_LIMIT_PRIORITY_CLASS,
      JOB_OBJECT_LIMIT_PRESERVE_JOB_TIME,
      JOB_OBJECT_LIMIT_SCHEDULING_CLASS,
      JOB_OBJECT_LIMIT_PROCESS_MEMORY,
      JOB_OBJECT_LIMIT_JOB_MEMORY,
      JOB_OBJECT_LIMIT_DIE_ON_UNHANDLED_EXCEPTION,
      JOB_OBJECT_LIMIT_BREAKAWAY_OK,
      JOB_OBJECT_LIMIT_SILENT_BREAKAWAY_OK,
      JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE,
      JOB_OBJECT_LIMIT_RESERVED2,
      JOB_OBJECT_LIMIT_RESERVED3,
      JOB_OBJECT_LIMIT_RESERVED4,
      JOB_OBJECT_LIMIT_RESERVED5,
      JOB_OBJECT_LIMIT_RESERVED6,
      0
   );

   JwJobUiLimitEnumValues : array[TJwJobUiLimit] of Cardinal = (
      JOB_OBJECT_UILIMIT_HANDLES,
      JOB_OBJECT_UILIMIT_READCLIPBOARD,
      JOB_OBJECT_UILIMIT_WRITECLIPBOARD,
      JOB_OBJECT_UILIMIT_SYSTEMPARAMETERS,
      JOB_OBJECT_UILIMIT_DISPLAYSETTINGS,
      JOB_OBJECT_UILIMIT_GLOBALATOMS,
      JOB_OBJECT_UILIMIT_DESKTOP,
      JOB_OBJECT_UILIMIT_EXITWINDOWS,
      0
   );

   JwJobMessagesEnumValues : array[TJwJobMessage] of Cardinal = (
      0,
      JOB_OBJECT_MSG_ACTIVE_PROCESS_ZERO,
      JOB_OBJECT_MSG_END_OF_PROCESS_TIME,
      JOB_OBJECT_MSG_ACTIVE_PROCESS_LIMIT,
      JOB_OBJECT_MSG_PROCESS_MEMORY_LIMIT,
      JOB_OBJECT_MSG_JOB_MEMORY_LIMIT,
      JOB_OBJECT_MSG_NEW_PROCESS,
      JOB_OBJECT_MSG_EXIT_PROCESS,
      JOB_OBJECT_MSG_ABNORMAL_EXIT_PROCESS,
      JOB_OBJECT_MSG_END_OF_JOB_TIME
   );

   JwSecurityCapabilities : array[TJwSecurityCapability] of Cardinal = (
      SECPKG_FLAG_INTEGRITY,
      SECPKG_FLAG_PRIVACY,
      SECPKG_FLAG_TOKEN_ONLY,
      SECPKG_FLAG_DATAGRAM,
      SECPKG_FLAG_CONNECTION,
      SECPKG_FLAG_MULTI_REQUIRED,
      SECPKG_FLAG_CLIENT_ONLY,
      SECPKG_FLAG_EXTENDED_ERROR,
      SECPKG_FLAG_IMPERSONATION,
      SECPKG_FLAG_ACCEPT_WIN32_NAME,
      SECPKG_FLAG_STREAM,
      SECPKG_FLAG_NEGOTIABLE,
      SECPKG_FLAG_GSS_COMPATIBLE,
      SECPKG_FLAG_LOGON,
      SECPKG_FLAG_ASCII_BUFFERS,
      SECPKG_FLAG_FRAGMENT,
      SECPKG_FLAG_MUTUAL_AUTH,
      SECPKG_FLAG_DELEGATION
   );




{ TJwEnumMap }

class function TJwEnumMap.ConvertMandatoryPolicyFlags(
      const FlagSet: TJwMandatoryPolicyFlagSet): Cardinal;
var I : TJwMandatoryPolicy;
begin
  result := 0;
  for I := Low(TJwMandatoryPolicy) to High(TJwMandatoryPolicy) do
  begin
    if I in FlagSet then
      result := result or MandatoryPolicyFlagValues[I];
  end;
end;

class function TJwEnumMap.ConvertMandatoryPolicyFlags(
      const FlagBits: Cardinal): TJwMandatoryPolicyFlagSet;
var I : TJwMandatoryPolicy;
begin
  result := [];
  for I := Low(TJwMandatoryPolicy) to High(TJwMandatoryPolicy) do
  begin
    if (FlagBits and MandatoryPolicyFlagValues[I]) = MandatoryPolicyFlagValues[I] then
      Include(result, I);
  end;
end;

class function TJwEnumMap.ConvertTokenMandatoryPolicyFlags(
  const FlagSet: TJwTokenMandatoryPolicies): Cardinal;
var I : TJwTokenMandatoryPolicy;
begin
  result := 0;
  if tmpOff in Flagset then
    exit;

  for I := Low(TJwTokenMandatoryPolicy) to High(TJwTokenMandatoryPolicy) do
  begin
    if I in FlagSet then
      result := result or TokenMandatoryPolicyFlagValues[I];
  end;
end;

class function TJwEnumMap.ConvertTokenMandatoryPolicyFlags(
  const FlagBits: Cardinal): TJwTokenMandatoryPolicies;
var I : TJwTokenMandatoryPolicy;
begin
  result := [tmpOff];
  if FlagBits = 0 then
    exit;

  result := [];
  for I := Low(TJwTokenMandatoryPolicy) to High(TJwTokenMandatoryPolicy) do
  begin
    if (FlagBits and TokenMandatoryPolicyFlagValues[I]) = TokenMandatoryPolicyFlagValues[I] then
      Include(result, I);
  end;
  Exclude(result, tmpoff);
end;


class function TJwEnumMap.ConvertInheritFlags(
  const FlagSet: TJwInheritFlagSet): Cardinal;
var I : TJwInheritFlag;
begin
  result := 0;
  for I := Low(TJwInheritFlag) to High(TJwInheritFlag) do
  begin
    if I in FlagSet then
      result := result or InheritFlagsValues[I];
  end;
end;

class function TJwEnumMap.ConvertInheritFlags(
  const FlagBits: Cardinal): TJwInheritFlagSet;
var I : TJwInheritFlag;
begin
  result := [];
  for I := Low(TJwInheritFlag) to High(TJwInheritFlag) do
  begin
    if (FlagBits and InheritFlagsValues[I]) = InheritFlagsValues[I] then
      Include(result, I);
  end;
end;


class function TJwEnumMap.ConvertSecurityInformation(
  const FlagSet: TJwSecurityInformationFlagSet): Cardinal;
var I : TJwSecurityInformationFlag;
begin
  result := 0;
  for I := Low(TJwSecurityInformationFlag) to High(TJwSecurityInformationFlag) do
  begin
    if I in FlagSet then
      result := result or SecurityInformationValues[I];
  end;
end;

class function TJwEnumMap.ConvertSecurityInformation(
  const FlagBits: TSecurityInformation): TJwSecurityInformationFlagSet;
var I : TJwSecurityInformationFlag;
begin
  result := [];
  for I := Low(TJwSecurityInformationFlag) to High(TJwSecurityInformationFlag) do
  begin
    if (FlagBits and SecurityInformationValues[I]) = SecurityInformationValues[I] then
      Include(result, I);
  end;
end;

class function TJwEnumMap.ConvertSecurityControl(
  const ControlSet: TJwSecurityDescriptorControlSet): jwaWindows.TSecurityDescriptorControl;
var I : TJwSecurityDescriptorControl;
begin
  result := 0;
  for I := Low(TJwSecurityDescriptorControl) to High(TJwSecurityDescriptorControl) do
  begin
    if I in ControlSet then
      result := result or SecurityDescriptorControlValues[I];
  end;
end;

class function TJwEnumMap.ConvertSecurityControl(
  const Control: TSecurityDescriptorControl): TJwSecurityDescriptorControlSet;
var I : TJwSecurityDescriptorControl;
begin
  result := [];
  for I := Low(TJwSecurityDescriptorControl) to High(TJwSecurityDescriptorControl) do
  begin
    if (Control and SecurityDescriptorControlValues[I]) = SecurityDescriptorControlValues[I] then
      Include(result, I);
  end;
end;

class function TJwEnumMap.ConvertFlags(
  FlagSet: TJwSecurityDialogFlags): Cardinal;
var I : TJwSecurityDialogFlag;
begin
  result := 0;
  for I := Low(TJwSecurityDialogFlag) to High(TJwSecurityDialogFlag) do
  begin
    if I in FlagSet then
      result := result or SecurityDialogFlagValues[I];
  end;
end;

class function TJwEnumMap.ConvertFlags(
  Flags: Cardinal): TJwSecurityDialogFlags;
var I : TJwSecurityDialogFlag;
begin
  result := [];
  for I := Low(TJwSecurityDialogFlag) to High(TJwSecurityDialogFlag) do
  begin
    if (Flags and SecurityDialogFlagValues[I]) = SecurityDialogFlagValues[I] then
      Include(result, I);
  end;
end;


class function TJwEnumMap.ConvertAceFlags(
  const AceFlags: TJwAceFlags): Cardinal;
var I : TJwAceFlag;
begin
  result := 0;
  for I := Low(TJwAceFlag) to High(TJwAceFlag) do
  begin
    if I in AceFlags then
      result := result or AceFlagValues[I];
  end;
end;

class function TJwEnumMap.ConvertAceFlags(
  const AceFlags: Cardinal): TJwAceFlags;
var I : TJwAceFlag;
begin
  result := [];
  for I := Low(TJwAceFlag) to High(TJwAceFlag) do
  begin
    if (AceFlags and AceFlagValues[I]) = AceFlagValues[I] then
      Include(result, I);
  end;
end;

class function TJwEnumMap.ConvertToCredentialFlag(
  const CredFlags: Cardinal): TJwCredentialFlagSet;
var I : TJwCredentialFlag;
begin
  result := [];
  for I := Low(TJwCredentialFlag) to High(TJwCredentialFlag) do
  begin
    if (CredFlags and CredentialFlagValues[I]) = CredentialFlagValues[I] then
      Include(result, I);
  end;
end;

class function TJwEnumMap.ConvertToCredentialFlag(
  const CredFlags: TJwCredentialFlagSet): Cardinal;
var I : TJwCredentialFlag;
begin
  result := 0;
  for I := Low(TJwCredentialFlag) to High(TJwCredentialFlag) do
  begin
    if I in CredFlags then
      result := result or CredentialFlagValues[I];
  end;
end;






class function TJwEnumMap.ConvertProtectFlags(
  const Flags: DWORD): TJwCryptProtectFlagSet;
var I : TJwCryptProtectFlag;
begin
  result := [];
  for I := Low(TJwCryptProtectFlag) to High(TJwCryptProtectFlag) do
  begin
    if (Flags and CryptProtectFlag[I]) = CryptProtectFlag[I] then
      Include(result, I);
  end;
end;

class function TJwEnumMap.ConvertProtectFlags(
  const Flags: TJwCryptProtectFlagSet): DWORD;
var I : TJwCryptProtectFlag;
begin
  result := 0;
  for I := Low(TJwCryptProtectFlag) to High(TJwCryptProtectFlag) do
  begin
    if I in Flags then
      result := result or CryptProtectFlag[I];
  end;
end;

class function TJwEnumMap.ConvertProtectMemoryFlags(
  const Flags: DWORD): TJwProtectMemoryFlagSet;
var I : TJwProtectMemoryFlag;
begin
  result := [];
  for I := Low(TJwProtectMemoryFlag) to High(TJwProtectMemoryFlag) do
  begin
    if (Flags and ProtectMemoryFlagSetValues[I]) = ProtectMemoryFlagSetValues[I] then
      Include(result, I);
  end;
end;


class function TJwEnumMap.ConvertProtectMemoryFlags(
  const Flags: TJwProtectMemoryFlagSet): DWORD;
var I : TJwProtectMemoryFlag;
begin
  result := 0;
  for I := Low(TJwProtectMemoryFlag) to High(TJwProtectMemoryFlag) do
  begin
    if I in Flags then
      result := result or ProtectMemoryFlagSetValues[I];
  end;
end;

class function TJwEnumMap.ConvertProtectPromptFlags(
  const Flags: DWORD): TJwCryptProtectOnPromptFlagSet;
var I : TJwCryptProtectOnPromptFlag;
begin
  result := [];
  for I := Low(TJwCryptProtectOnPromptFlag) to High(TJwCryptProtectOnPromptFlag) do
  begin
    if (Flags and CryptProtectOnPromptFlagValues[I]) = CryptProtectOnPromptFlagValues[I] then
      Include(result, I);
  end;
end;

class function TJwEnumMap.ConvertProtectPromptFlags(
  const Flags: TJwCryptProtectOnPromptFlagSet): DWORD;
var I : TJwCryptProtectOnPromptFlag;
begin
  result := 0;
  for I := Low(TJwCryptProtectOnPromptFlag) to High(TJwCryptProtectOnPromptFlag) do
  begin
    if I in Flags then
      result := result or CryptProtectOnPromptFlagValues[I];
  end;
end;




class function TJwEnumMap.ConvertAttributes(
  const Attributes: Cardinal): TJwSidAttributeSet;
begin
  Result := [];
  if Attributes and SE_GROUP_MANDATORY = SE_GROUP_MANDATORY then
    Include(Result, sidaGroupMandatory);

  if Attributes and SE_GROUP_ENABLED_BY_DEFAULT =
    SE_GROUP_ENABLED_BY_DEFAULT then
    Include(Result, sidaGroupEnabledByDefault);

  if Attributes and SE_GROUP_ENABLED = SE_GROUP_ENABLED then
    Include(Result, sidaGroupEnabled);

  if Attributes and SE_GROUP_OWNER = SE_GROUP_OWNER then
    Include(Result, sidaGroupOwner);

  if Attributes and SE_GROUP_USE_FOR_DENY_ONLY =
    SE_GROUP_USE_FOR_DENY_ONLY then
    Include(Result, sidaGroupUseForDenyOnly);

  if Attributes and SE_GROUP_LOGON_ID = SE_GROUP_LOGON_ID then
    Include(Result, sidaGroupLogonId);

  if Attributes and SE_GROUP_RESOURCE = SE_GROUP_RESOURCE then
    Include(Result, sidaGroupResource);


  if Attributes and SE_GROUP_INTEGRITY = SE_GROUP_INTEGRITY then
    Include(Result, sidaGroupIntegrity);

  if Attributes and SE_GROUP_INTEGRITY_ENABLED = SE_GROUP_INTEGRITY_ENABLED then
    Include(Result, sidaGroupIntegrityEnabled);

end;

class function TJwEnumMap.ConvertAttributes(
  const Attributes: TJwSidAttributeSet): Cardinal;
begin
  Result := 0;
  if sidaGroupMandatory in Attributes then
    Result := Result or SE_GROUP_MANDATORY;

  if sidaGroupEnabledByDefault in Attributes then
    Result := Result or SE_GROUP_ENABLED_BY_DEFAULT;

  if sidaGroupEnabled in Attributes then
    Result := Result or SE_GROUP_ENABLED;

  if sidaGroupOwner in Attributes then
    Result := Result or SE_GROUP_OWNER;

  if sidaGroupUseForDenyOnly in Attributes then
    Result := Result or SE_GROUP_USE_FOR_DENY_ONLY;

  if sidaGroupLogonId in Attributes then
    Result := Result or SE_GROUP_LOGON_ID;

  if sidaGroupResource in Attributes then
    Result := Result or SE_GROUP_RESOURCE;


  if sidaGroupIntegrity in Attributes then
    Result := Result or SE_GROUP_INTEGRITY;

  if sidaGroupIntegrityEnabled in Attributes then
    Result := Result or SE_GROUP_INTEGRITY_ENABLED;

end;


class function TJwEnumMap.ConvertHashAlgorithm(
  const Alg: TJwHashAlgorithm): DWORD;
begin
  Result:=HashAlgorithmValues[Alg];
end;

class function TJwEnumMap.ConvertHashAlgorithm(
  const Alg: DWORD): TJwHashAlgorithm;
var i: TJwHashAlgorithm;
begin
  Result := haUnknown;
  for i := Low(TJwHashAlgorithm) to High(TJwHashAlgorithm) do
    if HashAlgorithmValues[i] = Alg then
    begin
      Result := i;
      Break;
    end;
end;

class function TJwEnumMap.ConvertCSPType(
  const CSPType: TJwCSPType): DWORD;
begin
  Result := CSPTypeValues[CSPType];
end;

class function TJwEnumMap.ConvertCSPType(
  const CSPType: DWORD): TJwCSPType;
var i: TJwCSPType;
begin
  Result := ctUnknown;
  for i := Low(TJwCSPType) to High(TJwCSPType) do
    if CSPTypeValues[i] = CSPType then
    begin
      Result := i;
      Break;
    end;
end;

class function TJwEnumMap.ConvertCSPCreationFlags(
  const FlagSet: TJwCSPCreationFlagSet): Cardinal;
var I : TJwCSPCreationFlag;
begin
  result := 0;
  for I := Low(TJwCSPCreationFlag) to High(TJwCSPCreationFlag) do
  begin
    if I in FlagSet then
      result := result or CSPCreationFlagValues[I];
  end;
end;

class function TJwEnumMap.ConvertCSPCreationFlags(
  const FlagBits: Cardinal): TJwCSPCreationFlagSet;
var I : TJwCSPCreationFlag;
begin
  result := [];
  for I := Low(TJwCSPCreationFlag) to High(TJwCSPCreationFlag) do
  begin
    if (FlagBits and CSPCreationFlagValues[I]) = CSPCreationFlagValues[I] then
      Include(result, I);
  end;
end;

class function TJwEnumMap.ConvertKeyPairType(
  const KeyPairType: TJwKeyPairType): Cardinal;
begin
  Result := KeyPairTypeValues[KeyPairType];
end;

class function TJwEnumMap.ConvertKeyPairType(
  const KeyPairType: Cardinal): TJwKeyPairType;
var i: TJwKeyPairType;
begin
  result := kptUnknown;
  for i:=Low(TJwKeyPairType) to High(TJwKeyPairType) do
    if KeyPairTypeValues[i] = KeyPairType then
    begin
      Result := i;
      Break;
    end;
end;

class function TJwEnumMap.ConvertKeyFlagSet(
  const FlagSet: TJwKeyFlagSet): Cardinal;
var I : TJwKeyFlag;
begin
  result := 0;
  for I := Low(TJwKeyFlag) to High(TJwKeyFlag) do
  begin
    if I in FlagSet then
      result := result or KeyFlagValues[I];
  end;
end;

class function TJwEnumMap.ConvertKeyFlagSet(
  const FlagBits: Cardinal): TJwKeyFlagSet;
var I : TJwKeyFlag;
begin
  result := [];
  for I := Low(TJwKeyFlag) to High(TJwKeyFlag) do
  begin
    if (FlagBits and KeyFlagValues[I]) = KeyFlagValues[I] then
      Include(result, I);
  end;
end;


class function TJwEnumMap.ConvertKeyExportKind(
  const KeyExportKind: TJwKeyExportKind): Cardinal;
begin
  Result := KeyExportKindValues[KeyExportKind];
end;

class function TJwEnumMap.ConvertKeyExportKind(const KeyExportKind: Cardinal): TJwKeyExportKind;
var I: TJwKeyExportKind;
begin
  Result := kekPlaintext;
  for I := Low(TJwKeyExportKind) to High(TJwKeyExportKind) do
  begin
    if KeyExportKindValues[I] = KeyExportKind then
    begin
      Result := I;
      break;
    end;
  end;
end;

class function TJwEnumMap.ConvertEncryptionAlgorithm(
  const Algorithm: TJwEncryptionAlgorithm): Cardinal;
begin
  Result := EncryptionAlgorithmValues[Algorithm];
end;

class function TJwEnumMap.ConvertEncryptionAlgorithm(
  const AlgId: Cardinal): TJwEncryptionAlgorithm;
var i: TJwEncryptionAlgorithm;
begin
  Result := eaUnknown;
  for i := Low(TJwEncryptionAlgorithm) to High(TJwEncryptionAlgorithm) do
    if EncryptionAlgorithmValues[i] = AlgId then
    begin
      Result := i;
      Break;
    end;
end;

class function TJwEnumMap.ConvertAuthZResourceManager(
  const FlagSet: TJwAuthZResourceManagerFlags): Cardinal;
var I : TJwAuthZResourceManagerFlag;
begin
  result := 0;
  for I := Low(TJwAuthZResourceManagerFlag) to High(TJwAuthZResourceManagerFlag) do
  begin
    if I in FlagSet then
      result := result or AuthZResourceManagerValues[I];
  end;
end;

class function TJwEnumMap.ConvertAuthZResourceManager(
  const FlagBits: Cardinal): TJwAuthZResourceManagerFlags;
var I : TJwAuthZResourceManagerFlag;
begin
  result := [];
  for I := Low(TJwAuthZResourceManagerFlag) to High(TJwAuthZResourceManagerFlag) do
  begin
    if (FlagBits and AuthZResourceManagerValues[I]) = AuthZResourceManagerValues[I] then
      Include(result, I);
  end;
end;

class function TJwEnumMap.ConvertAuthZSidContextFlags(
 const FlagSet: TAuthZSidContextFlags): Cardinal;
var I : TAuthZSidContextFlag;
begin
  result := 0;
  for I := Low(TAuthZSidContextFlag) to High(TAuthZSidContextFlag) do
  begin
    if I in FlagSet then
      result := result or AuthZSidContextValues[I];
  end;
end;

class function TJwEnumMap.ConvertAuthZSidContextFlags(
  const FlagBits: Cardinal): TAuthZSidContextFlags;
var I : TAuthZSidContextFlag;
begin
  result := [];
  for I := Low(TAuthZSidContextFlag) to High(TAuthZSidContextFlag) do
  begin
    if (FlagBits and AuthZSidContextValues[I]) = AuthZSidContextValues[I] then
      Include(result, I);
  end;
end;


class function TJwEnumMap.ConvertReplyErrorEnum(
  const Flags: TJwReplyErrorEnum): Cardinal;
begin
  result := ReplyErrorEnumValues[Flags];
  //hier weiter
end;

class function TJwEnumMap.ConvertReplyErrorEnum(
  const Flags: Cardinal): TJwReplyErrorEnum;
var i : TJwReplyErrorEnum;
begin
  result := reUnknown;
  for I := Low(TJwReplyErrorEnum) to High(TJwReplyErrorEnum) do
  begin
    if (Flags = ReplyErrorEnumValues[I]) then
    begin
      result := I;
      exit;
    end;
  end;
end;


class function TJwEnumMap.ConvertAceType(
  const AceType: TJwAceType): Cardinal;
begin
  result := AceTypeEnumValues[AceType];
end;

class function TJwEnumMap.ConvertAceType(
  const AceType: Cardinal): TJwAceType;
var i : TJwAceType;
begin
  result := actUnknown;
  for i := Low(AceTypeEnumValues) to high(AceTypeEnumValues) do
  begin
    if AceType = AceTypeEnumValues[i] then
    begin
      result := i;
      exit;
    end;
  end;
end;



class function TJwEnumMap.ConvertJobLimit(
  const FlagBits: Cardinal): TJwJobLimits;
var I : TJwJobLimit;
begin
  result := [];
  for I := Low(TJwJobLimit) to High(TJwJobLimit) do
  begin
    if (FlagBits and JwJobLimitEnumValues[I]) = JwJobLimitEnumValues[I] then
      Include(result, I);
  end;
end;

class function TJwEnumMap.ConvertJobLimitType(
  const FlagSet: TJwJobLimits): Cardinal;
var I : TJwJobLimit;
begin
  result := 0;
  for I := Low(TJwJobLimit) to High(TJwJobLimit) do
  begin
    if I in FlagSet then
      result := result or JwJobLimitEnumValues[I];
  end;
end;

class function TJwEnumMap.ConvertJobUiLimit(
  const FlagBits: Cardinal): TJwJobUiLimits;
var I : TJwJobUiLimit;
begin
  result := [];
  for I := Low(TJwJobUiLimit) to High(TJwJobUiLimit) do
  begin
    if (FlagBits and JwJobUiLimitEnumValues[I]) = JwJobUiLimitEnumValues[I] then
      Include(result, I);
  end;
end;

class function TJwEnumMap.ConvertJobUiLimitType(
  const FlagSet: TJwJobUiLimits): Cardinal;
var I : TJwJobUiLimit;
begin
  result := 0;
  for I := Low(TJwJobUiLimit) to High(TJwJobUiLimit) do
  begin
    if I in FlagSet then
      result := result or JwJobUiLimitEnumValues[I];
  end;
end;

class function TJwEnumMap.ConvertJobMessageType(
      const FlagSet: TJwJobMessages): Cardinal;
var I : TJwJobMessage;
begin
  result := 0;
  for I := Low(TJwJobMessage) to High(TJwJobMessage) do
  begin
    if I in FlagSet then
      result := result or JwJobMessagesEnumValues[I];
  end;
end;


class function TJwEnumMap.ConvertJobMessage(
      const FlagBits: Cardinal): TJwJobMessages;
var I : TJwJobMessage;
begin
  result := [];
  for I := Low(TJwJobMessage) to High(TJwJobMessage) do
  begin
    if (FlagBits and JwJobMessagesEnumValues[I]) = JwJobMessagesEnumValues[I] then
      Include(result, I);
  end;
end;

class function TJwEnumMap.ConvertSecurityCapabilityMessage(
  const FlagBits: Cardinal): TJwSecurityCapabilities;
var I : TJwSecurityCapability;
begin
  result := [];
  for I := Low(TJwSecurityCapability) to High(TJwSecurityCapability) do
  begin
    if (FlagBits and JwSecurityCapabilities[I]) = JwSecurityCapabilities[I] then
      Include(result, I);
  end;
end;

class function TJwEnumMap.ConvertSecurityCapabilityType(
  const FlagSet: TJwSecurityCapabilities): Cardinal;
var I : TJwSecurityCapability;
begin
  result := 0;
  for I := Low(TJwSecurityCapability) to High(TJwSecurityCapability) do
  begin
    if I in FlagSet then
      result := result or JwSecurityCapabilities[I];
  end;
end;





{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}





initialization
{$ENDIF SL_OMIT_SECTIONS}



{$IFNDEF SL_INITIALIZATION_SECTION}
{$ENDIF SL_INITIALIZATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}
