unit Keyman.System.Security;

interface

uses
  System.SysUtils,
  Winapi.Accctrl,
  Winapi.AclApi,
  Winapi.Windows;

type
  EKeymanSecurity = class(Exception);

procedure SetObjectToLowIntegrity(hObject: THandle; _type: SE_OBJECT_TYPE = SE_KERNEL_OBJECT); overload;
function SetObjectToLowIntegrity(var psd: PSECURITY_DESCRIPTOR; _type: SE_OBJECT_TYPE = SE_KERNEL_OBJECT): Boolean; overload;
procedure GrantPermissionToAllApplicationPackages(handle: THandle; dwAccessPermissions: DWORD; _type: SE_OBJECT_TYPE = SE_KERNEL_OBJECT); overload;
//procedure GrantPermissionToAllApplicationPackages(var psa: SECURITY_ATTRIBUTES; dwAccessPermissions: DWORD); overload;

implementation

uses
  Keyman.Winapi.VersionHelpers;

const LOW_INTEGRITY_SDDL_SACL_W: WideString = 'S:(ML;;NW;;;LW)';
const LABEL_SECURITY_INFORMATION = $00000010;
const SDDL_REVISION_1 = 1;

function ConvertStringSecurityDescriptorToSecurityDescriptor(
    {IN} StringSecurityDescriptor: LPCWSTR;
    {IN} StringSDRevision: DWORD;
    {OUT} var SecurityDescriptor: PSECURITY_DESCRIPTOR;
    {OUT} SecurityDescriptorSize: PULONG {OPTIONAL}
    ): BOOL; stdcall; external 'advapi32.dll' name 'ConvertStringSecurityDescriptorToSecurityDescriptorW';

procedure SetObjectToLowIntegrity(hObject: THandle; _type: SE_OBJECT_TYPE = SE_KERNEL_OBJECT); overload;
var
  dwErr: DWORD;
  pSD: PSECURITY_DESCRIPTOR;
  pSacl: PACL;
  fSaclPresent: BOOL;
  fSaclDefaulted: BOOL;
begin
  pSD := nil;
  pSacl := nil;
  fSaclPresent := FALSE;
  fSaclDefaulted := FALSE;

  if not IsWindowsVistaOrGreater then
    Exit;

  if not ConvertStringSecurityDescriptorToSecurityDescriptor(PWideChar(LOW_INTEGRITY_SDDL_SACL_W), SDDL_REVISION_1, pSD, nil) then
    RaiseLastOSError;

  if not GetSecurityDescriptorSacl(pSD, fSaclPresent, pSacl, fSaclDefaulted) then
    RaiseLastOSError;

  dwErr := SetSecurityInfo(
    hObject, _type, LABEL_SECURITY_INFORMATION,
    nil, nil, nil, pSacl);
  if ERROR_SUCCESS <> dwErr then
    RaiseLastOSError(dwErr);

  LocalFree(Cardinal(pSD));
end;

function SetObjectToLowIntegrity(var psd: PSECURITY_DESCRIPTOR; _type: SE_OBJECT_TYPE = SE_KERNEL_OBJECT): Boolean; overload;
begin
  pSD := nil;

  if not IsWindowsVistaOrGreater then
    Exit(False);

  if not ConvertStringSecurityDescriptorToSecurityDescriptor(PWideChar(LOW_INTEGRITY_SDDL_SACL_W), SDDL_REVISION_1, pSD, nil) then
    RaiseLastOSError;

  Result := True;
end;


type WELL_KNOWN_SID_TYPE = DWORD;

const
  WinNullSid                                     = $00000000;
  WinWorldSid                                    = $00000001;
  WinLocalSid                                    = $00000002;
  WinCreatorOwnerSid                             = $00000003;
  WinCreatorGroupSid                             = $00000004;
  WinCreatorOwnerServerSid                       = $00000005;
  WinCreatorGroupServerSid                       = $00000006;
  WinNtAuthoritySid                              = $00000007;
  WinDialupSid                                   = $00000008;
  WinNetworkSid                                  = $00000009;
  WinBatchSid                                    = $0000000A;
  WinInteractiveSid                              = $0000000B;
  WinServiceSid                                  = $0000000C;
  WinAnonymousSid                                = $0000000D;
  WinProxySid                                    = $0000000E;
  WinEnterpriseControllersSid                    = $0000000F;
  WinSelfSid                                     = $00000010;
  WinAuthenticatedUserSid                        = $00000011;
  WinRestrictedCodeSid                           = $00000012;
  WinTerminalServerSid                           = $00000013;
  WinRemoteLogonIdSid                            = $00000014;
  WinLogonIdsSid                                 = $00000015;
  WinLocalSystemSid                              = $00000016;
  WinLocalServiceSid                             = $00000017;
  WinNetworkServiceSid                           = $00000018;
  WinBuiltinDomainSid                            = $00000019;
  WinBuiltinAdministratorsSid                    = $0000001A;
  WinBuiltinUsersSid                             = $0000001B;
  WinBuiltinGuestsSid                            = $0000001C;
  WinBuiltinPowerUsersSid                        = $0000001D;
  WinBuiltinAccountOperatorsSid                  = $0000001E;
  WinBuiltinSystemOperatorsSid                   = $0000001F;
  WinBuiltinPrintOperatorsSid                    = $00000020;
  WinBuiltinBackupOperatorsSid                   = $00000021;
  WinBuiltinReplicatorSid                        = $00000022;
  WinBuiltinPreWindows2000CompatibleAccessSid    = $00000023;
  WinBuiltinRemoteDesktopUsersSid                = $00000024;
  WinBuiltinNetworkConfigurationOperatorsSid     = $00000025;
  WinAccountAdministratorSid                     = $00000026;
  WinAccountGuestSid                             = $00000027;
  WinAccountKrbtgtSid                            = $00000028;
  WinAccountDomainAdminsSid                      = $00000029;
  WinAccountDomainUsersSid                       = $0000002A;
  WinAccountDomainGuestsSid                      = $0000002B;
  WinAccountComputersSid                         = $0000002C;
  WinAccountControllersSid                       = $0000002D;
  WinAccountCertAdminsSid                        = $0000002E;
  WinAccountSchemaAdminsSid                      = $0000002F;
  WinAccountEnterpriseAdminsSid                  = $00000030;
  WinAccountPolicyAdminsSid                      = $00000031;
  WinAccountRasAndIasServersSid                  = $00000032;
  WinNTLMAuthenticationSid                       = $00000033;
  WinDigestAuthenticationSid                     = $00000034;
  WinSChannelAuthenticationSid                   = $00000035;
  WinThisOrganizationSid                         = $00000036;
  WinOtherOrganizationSid                        = $00000037;
  WinBuiltinIncomingForestTrustBuildersSid       = $00000038;
  WinBuiltinPerfMonitoringUsersSid               = $00000039;
  WinBuiltinPerfLoggingUsersSid                  = $0000003A;
  WinBuiltinAuthorizationAccessSid               = $0000003B;
  WinBuiltinTerminalServerLicenseServersSid      = $0000003C;
  WinBuiltinDCOMUsersSid                         = $0000003D;
  WinBuiltinIUsersSid                            = $0000003E;
  WinIUserSid                                    = $0000003F;
  WinBuiltinCryptoOperatorsSid                   = $00000040;
  WinUntrustedLabelSid                           = $00000041;
  WinLowLabelSid                                 = $00000042;
  WinMediumLabelSid                              = $00000043;
  WinHighLabelSid                                = $00000044;
  WinSystemLabelSid                              = $00000045;
  WinWriteRestrictedCodeSid                      = $00000046;
  WinCreatorOwnerRightsSid                       = $00000047;
  WinCacheablePrincipalsGroupSid                 = $00000048;
  WinNonCacheablePrincipalsGroupSid              = $00000049;
  WinEnterpriseReadonlyControllersSid            = $0000004A;
  WinAccountReadonlyControllersSid               = $0000004B;
  WinBuiltinEventLogReadersGroup                 = $0000004C;
  WinNewEnterpriseReadonlyControllersSid         = $0000004D;
  WinBuiltinCertSvcDComAccessGroup               = $0000004E;
  WinMediumPlusLabelSid                          = $0000004F;
  WinLocalLogonSid                               = $00000050;
  WinConsoleLogonSid                             = $00000051;
  WinThisOrganizationCertificateSid              = $00000052;
  WinApplicationPackageAuthoritySid              = $00000053;
  WinBuiltinAnyPackageSid                        = $00000054;
  WinCapabilityInternetClientSid                 = $00000055;
  WinCapabilityInternetClientServerSid           = $00000056;
  WinCapabilityPrivateNetworkClientServerSid     = $00000057;
  WinCapabilityPicturesLibrarySid                = $00000058;
  WinCapabilityVideosLibrarySid                  = $00000059;
  WinCapabilityMusicLibrarySid                   = $0000005A;
  WinCapabilityDocumentsLibrarySid               = $0000005B;
  WinCapabilitySharedUserCertificatesSid         = $0000005C;
  WinCapabilityEnterpriseAuthenticationSid       = $0000005D;
  WinCapabilityRemovableStorageSid               = $0000005E;
  WinBuiltinRDSRemoteAccessServersSid            = $0000005F;
  WinBuiltinRDSEndpointServersSid                = $00000060;
  WinBuiltinRDSManagementServersSid              = $00000061;
  WinUserModeDriversSid                          = $00000062;
  WinBuiltinHyperVAdminsSid                      = $00000063;
  WinAccountCloneableControllersSid              = $00000064;
  WinBuiltinAccessControlAssistanceOperatorsSid  = $00000065;
  WinBuiltinRemoteManagementUsersSid             = $00000066;
  WinAuthenticationAuthorityAssertedSid          = $00000067;
  WinAuthenticationServiceAssertedSid            = $00000068;
  WinLocalAccountSid                             = $00000069;
  WinLocalAccountAndAdministratorSid             = $0000006A;
  WinAccountProtectedUsersSid                    = $0000006B;
  WinCapabilityAppointmentsSid                   = $0000006C;
  WinCapabilityContactsSid                       = $0000006D;
  WinAccountDefaultSystemManagedSid              = $0000006E;
  WinBuiltinDefaultSystemManagedGroupSid         = $0000006F;
  WinBuiltinStorageReplicaAdminsSid              = $00000070;
  WinAccountKeyAdminsSid                         = $00000071;
  WinAccountEnterpriseKeyAdminsSid               = $00000072;
  WinAuthenticationKeyTrustSid                   = $00000073;
  WinAuthenticationKeyPropertyMFASid             = $00000074;
  WinAuthenticationKeyPropertyAttestationSid     = $00000075;
  WinAuthenticationFreshKeyAuthSid               = $00000076;
  WinBuiltinDeviceOwnersSid                      = $00000077; //verified against WinNT.h

function CreateWellKnownSid(
  WellKnownSidType: WELL_KNOWN_SID_TYPE;
  DomainSid: PSID;
  pSID: PSID;
  cbSid: PDWORD): BOOL; stdcall; external 'advapi32.dll';

procedure GrantPermissionToAllApplicationPackages(handle: THandle; dwAccessPermissions: DWORD; _type: SE_OBJECT_TYPE = SE_KERNEL_OBJECT); overload;
var
  pOldDACL, pNewDACL: PACL;
  pSD: PSECURITY_DESCRIPTOR;
  ea: EXPLICIT_ACCESS;
  si: SECURITY_INFORMATION;
  pSID: Winapi.Windows.PSID;
  cbSid: DWORD;
  dwRes: DWORD;
// From winnt.h
type
  SID = record
    Revision: BYTE;
    SubAuthorityCount: BYTE;
    IdentifierAuthority: SID_IDENTIFIER_AUTHORITY;
    SubAuthority: array[0..0] of DWORD;
  end;
const
  SID_MAX_SUB_AUTHORITIES = 15;
  SECURITY_MAX_SID_SIZE =  (sizeof(SID) - sizeof(DWORD) + (SID_MAX_SUB_AUTHORITIES * sizeof(DWORD)));
begin
  // ALL APPLICATION PACKAGES group is introduced in Windows 8
  if not IsWindows8OrGreater then
    Exit;

  pOldDACL := nil;
  pNewDACL := nil;
  pSD := nil;
  si := DACL_SECURITY_INFORMATION;
  cbSid := SECURITY_MAX_SID_SIZE;

  // Get a pointer to the existing DACL.
  dwRes := GetSecurityInfo(handle, _type, DACL_SECURITY_INFORMATION, nil, nil, @pOldDACL, nil, @pSD);
  if ERROR_SUCCESS <> dwRes then
    RaiseLastOSError(dwRes);
  try
    // Allocate enough memory for the largest possible SID.
    pSID := Winapi.Windows.PSID(LocalAlloc(LMEM_FIXED, cbSid));
    if pSID = nil then
      RaiseLastOSError;
    try
      // Create a SID for the WinBuiltinAnyPackageSid group on the local computer.
      if not CreateWellKnownSid(WinBuiltinAnyPackageSid, nil, pSID, @cbSid) then
        RaiseLastOSError;

      // Initialize an EXPLICIT_ACCESS structure for the new ACE.
      ZeroMemory(@ea, sizeof(EXPLICIT_ACCESS));
      ea.grfAccessPermissions := dwAccessPermissions;
      ea.grfAccessMode := SET_ACCESS;
      ea.grfInheritance := SUB_CONTAINERS_AND_OBJECTS_INHERIT;

      ea.Trustee.TrusteeForm := TRUSTEE_IS_SID;
      ea.Trustee.TrusteeType := TRUSTEE_IS_WELL_KNOWN_GROUP;
      ea.Trustee.ptstrName := LPWSTR(pSID);

      // Create a new ACL that merges the new ACE into the existing DACL.
      dwRes := SetEntriesInAcl(1, @ea, pOldDACL, pNewDACL);
      if ERROR_SUCCESS <> dwRes then
        RaiseLastOSError(dwRes);

      // Attach the new ACL as the object's DACL.
      dwRes := SetSecurityInfo(handle, _type, si, nil, nil, pNewDACL, nil);
      if ERROR_SUCCESS <> dwRes then
        RaiseLastOSError(dwRes);

    finally
      LocalFree(HLOCAL(pSID));
    end;
  finally
    LocalFree(HLOCAL(pSD));
    LocalFree(HLOCAL(pNewDACL));
  end;
end;

end.
