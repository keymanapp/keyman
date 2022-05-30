{
Description
Project JEDI Windows Security Code Library (JWSCL)

Contains resource strings used by JSWCL

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

The Original Code is JwsclResource.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.

}
unit JwsclResource;
{$INCLUDE ..\includes\Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $

interface

{WARNING: Do not use resourcestring!
All Jwscl-exceptions use GetLastError to obtain the winapi error code.
They also use these text messages to show information.
However if resourcestring is used - implicitly LoadString is called
which does use SetLastError(0); which always results in last error code 0.
}
{
Use always an index based format string. That means that every placeholder
(starts with percent character) must use a zero based index that defines
where it is located in the value array of function Format. In this way
a location can mix up the order of the used placeholders.
}


const
  RsExceptionMessage = 'Message : %0:s\r\n';

  RsExceptionNoProc  = '(no proc)';
  RsExceptionNoClass = '(no class)';
  RsExceptionNoFile  = '(no file)';

  RsExceptionErrors  = 'GetLastError.....: %0:d ($%1:s)\r\n' +
                       'GetLastErrorMsg..: %2:s \r\n';

  RsExceptionMainMessage = '\r\n\r\n'+
    'An Exception of type %0:s was raised. \r\n' +
    '(Data was given by programmer and can vary from actual source.)\r\n'+
    'Source method....: %1:s\r\n'+
    'Source class.....: %2:s\r\n'+
    'Source file......: %3:s\r\n'+
    'Source line......: %4:d\r\n'+
    '%5:s\r\n'+ //LastError string [optional]
    '%6:s\r\n'+ //Exception message
    '%7:s'; //JCL debug info [optional]

  RsExceptionJCLText1 = '%0:s of %1:s';

  RsUnknownGetLastError = 'unknown GetLastError';

// Unit Acl.pas  RsACLClass


//
  RsACLClassNilACLPointer = 'ACL must not be nil. To create a deny only ACL use simply Create without an ACL.';
  RsACLClassInvalidACL    = 'Invalid ACL.';
  RsACLClassGetAceFailed  = 'Call to GetAce #%0:d failed.';
  RsACLClassNilParameter  = 'Parameter %0:s must not be nil';
  RsACLClassClassMismatch = 'Class mismatch. Expected "%0:s" but found aObject="%1:s"';
  RsACLClassNilSid        = 'The SID of ACE #%0:d must not be nil! Did you use a SID from XXXXKnownSid.pas and did not call JwInitWellKnownSIDs ?';
  RsACLClassNewAclNotEnoughMemory = 'Not enough memory to allocate memory for a new acl.';
  RsACLClassAddXAccessAceFailed = 'Call to AddXXXXXAccessAce failed in Item Index: %0:d';
  RsACLClassUnknownAccessAce = 'An unknown and unsupported item class %0:s was found for item index: %0:d';
  RsACLClassAceAlreadyInList = 'AccessControlEntry is already in the list. You cannot add the same instance twice to the list. Make a copy first using copy constructor.';
  RsACLClassInvalidAceInDacl = 'A discretionary access control list can onl' +
    'y list discretionary access control entries. (but found %0:s)';
  RsACLRevisionMisMatch = 'An ACE with an higher revision number (%0:d) was tried to add to an ACL with a lower version (%0:d).';
  RsACLClassInvaldAceInSacl = 'An system access control list can only list a' +
    'uditing access control entries.';
  RsACLClassInvalidAce = 'Could not add ACE to list. Did you use TSecurityAc' +
    'cessControlList ?';
  RsACLClassDaclName = 'Discretionary Control List';
  RsACLClassSaclName = 'System Control List (Audit)';
  RsACLClassAceCount = '\r\n ACE Count: %0:d\r\';
    RsACLClassAceNotInList = 'AccessControlEntry is not in list';

  RsACLClassAceRemovingDenied = 'ACE cannot be removed in that way. Use Remo' +
    've of the ListOwner';
  RsACLClassPropertyReadOnly = 'Property is readonly.';
  RsACLClassInvalidAceSid = 'The ACE must define a SID to use this method.';
  RsACLClassGetTextMap = '\r\nClassName: %0:s\r\nAceType: %1:s\r\nFlags: %2:' +
    's\r\nAccessmask: %3:s\r\nAccessMaskBits: %5:s \r\nSID: %4:s';
  RsMapNoMapGiven = 'No map class given.';
  RsUnsupportedACE = 'The given ACE type is not supported by this library.';
  RsInvalidMandatoryLevelType = 'The given SID has so sub authority or the sub authority is invalid or unknown. The RID must be a number of SECURITY_MANDATORY_XXXXX constants.';
  RsInvalidAceType = 'The type of the ACE structure is unknown.';
  RsACEMismatch = 'The type of the ACE class and the type of the ACE header do not match.';
  RsInvalidRevision = 'The given revision level %0:d is not supported.';

  RsRightGeneric = '[Generic]';
  RsRightReserved = '[Reserved]';
  RsRightMaximumAllowed = '[maximum allowed]';
  RsRightSacl = '[SACL]';
  RsRightStandard = '[Standard]';
  RsRightSpecific = '[Specific]';

//Unit  JwsclCredentials.pas

//
  RsCredentialsLocalName = 'local';
  RsUNCredentialsInvalidParameters = 'One or more parameters of CredUIParseU' +
  'serName are invalid!';
  RsUNCredentialsInvalidUserName = 'The user name is not valid.';
  RsUNCredentialsTooSmallBuffer = 'One of the buffers is too small.';
  RsUNCredentialsMessageTextDefault = 'Messagetext';
  RsUNCredentialsCaptionDefault = 'Caption';
  RsUNCredentialsLocalServerNameDefault = 'local';
  RsUNCredentialsUnsupported = 'The operating system does not support creden' +
  'tials prompt!';
  RsUNCredentialsInvalidEmptyServerName = 'ServerName must not be empty!';
  RsUNCredentialsInvalidUseOfCredFlags = 'The Flag cf_CREDUI_FLAGS_EXPECT_CO' +
  'NFIRMATION is set but the property OnConfirmCredential is nil!';
  RsUNCredentialsTooLongServerName = 'Length of property ServerName is to lo' +
  'ng.!';
  RsUNCredentialsEmptyServerName = 'Length of property ServerName must not b' +
  'e zero.!';
  RsUNCredentialsInvalidPropertyFlags = 'The property Flags is invalid!';
  RsUNCredentialsInvalidParametersCUIPFC = 'One or more parameters of CredUI' +
  'PromptForCredentials are invalid!';
  RsUNCredentialsInvalidLogonSession = 'There is no such logon session!';




//Unit JwsclTypes.pas

//
   RsAttributeHumanString0  = 'unknown';
   RsAttributeHumanString1  = 'mandatory';
   RsAttributeHumanString2  = 'default';
   RsAttributeHumanString3  = 'enabled';
   RsAttributeHumanString4  = 'owner';
   RsAttributeHumanString5  = 'deny';
   RsAttributeHumanString6  = 'logon ID';
   RsAttributeHumanString7  = 'domain-local';
   RsAttributeHumanString8  = 'integrity';
   RsAttributeHumanString9  = 'integrity enabled';
   RsAttributeHumanString10 = 'pad0';
   RsAttributeHumanString11 = 'pad1';
   RsAttributeHumanString12 = 'pad2';
   RsAttributeHumanString13 = 'pad3';
   RsAttributeHumanString14 = 'pad4';
   RsAttributeHumanString15 = 'pad5';

   RsAceTypeStringAllow = 'Allow';
   RsAceTypeStringDeny  = 'Deny';
   RsAceTypeStringAudit = 'Audit';
   RsAceMandatory = 'Mandatory';
   RsAceAllowObject = 'Allow object';
   RsAceDenyObject = 'Deny object'; 
   RsAceAllowCallbackObject = 'Allow callback object';
   RsAceDenyCallbackObject = 'Deny callback object';
   RsAceAuditCallback = 'Audit callback';
   RsAceAuditObject = 'Audit object';
   RsAceAuditCallbackObject = 'Audit callback object';
   RsAceAllowCallback = 'Allow callback';
   RsAceDenyCallback = 'Deny callback';
   RsAceUnknown = 'Unknown ace';

//Unit Jwscl_Descriptor.pas

//
  RsSecurityDescriptorInvalidAttributesSize = 'Securityattributes has invali' +
    'd size';
  RsSecurityDescriptorInvalid = 'The security descriptor is invalid. ';
  RsSecurityDescriptorInvalidHeader = 'Invalid Magicheader found: "%0:s" at ' +
    '%1:d';
  RsSecurityDescriptorTooSmallStreamSize = 'Streamsize is too small: %0:d ne' +
    'eded but %1:d remained.';
  RsSecurityDescriptorUnequalHash = 'Hashcode from stream is not equal to ca' +
    'lculated one (%0:d <-> %1:d) ';
  RsSecurityDescriptorInvalidStreamSD = 'The security descriptor read from s' +
    'tream is invalid';

//Unit Jwscl_SecurityDialogs.pas

//
  RsSecurityDialogsInheritanceThisDefault = 'This object';
  RsSecurityDialogsInheritanceObjectContainerDefault = 'This object, inherited obj' +
    'ects and containers';
  RsSecurityDialogsInheritanceContainerDefault = 'This object and containers';
  RsSecurityDialogsInheritanceObjectDefault = 'This object and inherited obj' +
    'ects';
  RsSecurityDialogsInheritanceOnlyObjectContainerDefault = 'Inherited contai' +
    'ners/objects';
  RsSecurityDialogsInheritanceOnlyContainerDefault = 'Inherited containers';
  RsSecurityDialogsInheritanceOnlyObjectDefault = 'Inherited objects';

//Unit JwsclLSA.pas

//
  RsLSALogonUserFailedSubStatus = 'Call to LsaLogonUser failed. SubStatus: %' +
    '0:d ';

//Unit JwsclMapping.pas

//
  RsStringOr = 'or';
  RsMappingNotAGenericMask = 'The specified AccessMask is not one of the gen' +
    'eric constants (GENERIC_READ, GENERIC_WRITE, GENERIC_EXECUTE, GENERIC_ALL' +
    ').';
  RsMappingBitString = 'Bit ';
  RsMappingTypeGeneric = '[Generic]';
  RsMappingTypeReserved = '[Reserved]';
  RsMappingTypeMaximumAllowed = '[maximum allowed]';
  RsMappingTypeSacl = '[SACL]';
  RsMappingTypeStandard = '[Standard]';
  RsMappingTypeSpecific = '[Specific]';
  RsMappingGeneralExecute = 'Execute';
  RsMappingGeneralWrite = 'Write';
  RsMappingGeneralRead = 'Read';
  RsMappingGeneralFullControll = 'Full access';
  RsMappingInvalid = 'The given generic map class is invalid.';

//Unit JwsclSecureObjects.pas

//
  RsSecureObjectsInvalidGroup = 'The group of the SecurityDescriptor must no' +
    't be nil. Use JwNullSID to remove influence of group to AccessCheck call.';
  RsSecureObjectsInvalidOwner = 'The owner of the SecurityDescriptor must no' +
    't be nil. Use JwNullSID to remove influence of owner to AccessCheck call.';
  RsSecureObjectsInvalidEmptyAclParameter = 'Parameter ACL must contain at l' +
    'east one ACE!';
  RsSecureObjectsUnsupportedInheritanceFunction =
    'The system reported that GetInheritanceSource is not supported.\r\n' +
    'Server Requires Windows Server 2008 or Windows Server 2003\r\n' +
    'Client Requires Windows Vista or Windows XP.';
  RsSecureObjectsNoCopyOfObjectHandle = 'Object handle could not be copied.';
  RsSecureObjectsFileFolderNotFound = 'File or directory not found: %s';
  RsSecureObjectsInvalidFileNameHandle = 'Filename and handle is invalid';
  RsSecureObjectsDaclAdaptionFailed = 'The function failed to adapt the DACL of the secured object. You must have either WRITE_DAC access rights or be the owner.';
  RsSecureObjectsInvalidFileOrFolder = 'The instance must be created with a file or folder name. GetFileInheritanceSource cannot be used with a handle! ';
    RsSecureObjectsNilSdParameter = 'The security descriptor must not be nil. ' +
    '(path: %s)';
  RsSecureObjectsCallFailedRegSetKeySecurity = 'Call to RegSetKeySecurity fa' +
    'iled. Tried to get security information.';
  RsSecureObjectsRegOpenKeyEx = 'Call to RegOpenKeyEx failed. Could not open' +
    ' key with READ access..';
  RsSecureObjectsCallFailedRegGetKeySecurity = 'Call to RegGetKeySecurity fa' +
    'iled. Tried to get buffer size for reg key ';
  RsSecureObjectsCallFailedRegGetKeySecurity2 = 'Call to RegGetKeySecurity f' +
    'ailed. Tried to get security information.';
  RsSecureObjectsCallFailedRegOpenKeyEx = 'Call to RegOpenKeyEx failed. Coul' +
    'd not open key with READ access..';
  RsSecureObjectsMissingDaclOrSaclInSiParameter = 'Parameter aSecurityInfo m' +
    'ust be [siDaclSecurityInformation] or [siSaclSecurityInformation]' +
    '. ';
  RsSecureObjectsEmptyRootKey = 'The KeyName "%s" does not contain a valid r' +
    'oot key (e.g. CURRENT_USER))! ';
  RsSecureObjectsInvalidKeyPathMissingRootElement = 'The key path "%s" is in' +
    'valid. Missing root element (e.h. current_user)';
  RsSecureObjectsInvalidEmptyKeyPath = 'The key path "%s" is invalid. It mus' +
    't not be empty.';
  RsSecureObjectsMissingFileOrFolder = 'The instance must be created with a ' +
    'file or folder name. GetFileInheritanceSource cannot be used with a handl' +
    'e! ';
  RsSecureObjectsInvalidRegPathHandle = 'Registry path and handle is invalid';
  RsSecureObjectsFailedReadingVolumeInformation = 'Could not retrieve volume' +
    ' information for %s';
  RsSecureObjectsInvalidRootPathName = 'RootPathName is invalid: %s';
  RsSecureObjectsPrivilegeSecurityMissing = 'To obtain audit information the' +
    ' privilege SE_SECURITY_NAME must be held by the token. But it could not b' +
    'e found and activated! (%s)';
  RsSecureObjectsPrivilegeSecurityMissing2 = 'To obtain audit information the' +
    ' privilege SE_SECURITY_NAME must be held by the token. But it could not b' +
    'e found! ';

  RsSecureObjectsParameterSiMissesDacl = 'Parameter aSecurityInfo must be [s' +
    'if_DACL_SECURITY_INFORMATION] or [siSaclSecurityInformation]. ';
  RsSecureObjectsNotEnoughMemoryForThreadData = 'Not enough memory to alloca' +
    'te thread data.';

  RsPrivilegeLabelBug = 'To obtain the integrity level from a named service object you need to '+
    ' apply siSaclSecurityInformation and the privilege SE_SECURITY_NAME.';
    
  RsUnimplementedSACLInheritance = 'Inheritance of SACL.';
  RsInvalidParameterAccessCheck = 'The parameter DesiredAccess must not contain MAXIMUM_ALLOWED for this method. '+
    'If you need to retrieve maximum allowed rights you should call the other AccessCheck method with parameter '+
    'GrantedAccess present.';



//unit JwsclSid.pas

//
  RsSidNotEnoughMemoryPSid = 'Not enough memory to allocate memory for a new' +
    ' sid.';
  RsSidInvalidAuthoritiesLength = 'Parameter Authorities must have length of' +
    ' 8 authorities.';
  RsSidCallLookupAccountNameFailed = 'Call to LookupAccountName failed. Coul' +
    'd not get required buffer size.';
  RsSidCallFailedCreateSIDString = 'Call to ConvertStringSidToSid failed. "%' +
    's"';
  RsSidSubAuthorityOutOfBound = 'Sub authority (%d) index out of bounds.';
  RsSidAlreadyInList = 'SID already in list';
  RsSidListGetText = 'Class : %0:s\r\nCount : %1:d';
  RsAccountNameUseLabel = 'AccountNameUse: ';
  RsSidAccountDomainNameLabel = 'AccountDomainName: ';
  RsSidAccountNameLabel = 'AccountName: ';
  RsSidStringSIDLabel = 'StringSID: ';
  RsSidSIDLengthLabel = 'SIDLength: ';
  RsSidIdentifierAuthorityLabel = 'IdentifierAuthority: ';
  RsSidSubAuthorityLabel = 'SubAuthority: ';
  RsSidSubAuthCountText = 'SubAuthorityCount: %d';
  RsSidAndAttributesMemoryAllocationFailed = 'Not enough memory to allocate ' +
    'memory for a new sid and attributes structure.';
  RsSidCallLookupAccountSidFailed = 'Call to LookupAccountSid failed. Could ' +
    'not get required buffer size.';
  RsSidUnknownDomain = '(?Domain?)\r\n%s';
  RsSidUnknownName   = '(?Name?)\r\n%s';
  RsSidUnknownSid    = '(S-1-????)\r\n%s';
  RsSidTextString    = '%0:s %1:s (%2:s) [%3:s]';
  RsInvalidSidAuthorityValue = 'The identifier authority value must not be larger than $FFFFFFFFFFFF.';
  RsInvalidDomainSid = 'The given SID (%0:s) does not match "S-1-5-21-xx-yy-zz" pattern.';


//Unit JwsclToken.pas

//
  RsPrivilegeEnabledByDefault = 'enabled by default';
  RsPrivilegeEnabled          = 'enabled';
  RsPrivilegeRemoved          = 'removed';
  RsPrivilegeUsedForAccess    = 'used for access';
  RsPrivilegeUnknown          = 'unknown (0x%x)';
  RsPrivilegeNone             = 'none';

  RsTokenEnabledText = '[enabled]';
  RsTokenDisabledText = '[disabled]';
  RsTokenUnavailableText = '[not available]';
  RsTokenPrivilegeNotAvailable = 'Privilege %s is not available.';
  RsTokenGlobalClassName = '(global)';
  RsTokenInvalidPrivilegeIndex = 'Invalid index %0:d (must be between 0 and ' +
    '%1:d).';
  RsTokenRemovePrivilegeDenied = 'A set of privilege without an assigned tok' +
    'en cannot be removed. Use instead the user functions.';
  RsTokenRemovePrivilegeDeniedByPrivilege = 'To remove a privilege the token' +
    ' must held the privilege SeTcbPrivilege.';
  RsTokenInvalidPrivilegePointer = 'Given Privilege set was not created by C' +
    'reate_PLUID_AND_ATTRIBUTES.';
  RsTokenPrivlegeNotInList = 'Could not remove privilege because it is not i' +
    'n list. ';
  RsTokenCallLookUpPrivilegeValueFailed = 'Call to LookupPrivilegeValue fail' +
    'ed for privilege: %s ';
  RsTokenPrivilegeAssignDenied = 'A set of privilege with an assigned token ' +
    'cannot be changed.';
  RsTokenPrivilegeAlreadyInList = 'The privilege %dx%d is already included i' +
    'n the set. ';
  RsTokenNotEnoughMemoryForPrivName = 'Not enough memory to allocate memory ' +
    'for privilege name.';
  RsPrivilegeFormatText = 'LUID: %0:s\r\nName: %1:s\r\nDisplayName: %2:s\r\n' +
    'Attributes: %3:s\r\n';
  RsTokenNotAssignedPrivilege = 'A privilege without an assigned token c' +
    'annot be enabled. ';
  RsPrivilegeCallAdjustTokenFailed = 'Call to AdjustToken failed with privil' +
    'ege %s. Could not set previous state.';
  RsTokenInvalidTokenHandle = 'Invalid token handle or handle is closed.';
  RsTokenInvalidThreadToken = 'Trying to open a thread token where only ' +
    'a process token exists! Use CreateTokenByProcess instead. ';
  RsTokenUnableToOpenThreadToken = 'Could not open a thread token';
  RsTokenUnsupportedWtsCall = 'The current Windows does not support WTS-' +
    'Function calls.';
  RsTokenTerminalServiceFailed = 'This function needs Terminal services to be installed'+
      ' and running. Supported in Windows 2000 Terminal Server and XP and above. ';
  RsPrimaryTokenMustBeSystem = 'This functions needs the primary Token to be'+
   ' a SYSTEM token.';


  RsTokenPrivilegeNotHeld = 'The privilege "%s" must be held to call this function.';
  RsTokenPrivilegeNotSet = 'The privilege "%s" must be set to call this function.';

  RsTokenCallWtsQueryUserTokenFailed = 'A call to WTSQueryUserToken fail' +
    'ed. Session ID: %d';
  RsTokenFailedImpersonateAnonymousToken = 'Could not impersonate anonym' +
    'ous token.';
  RsTokenCallDuplicateTokenFailed1 = 'Call to DuplicateTokenEx failed. T' +
    'o create an impersonated token you need to open a token with TOKEN_DUPLIC' +
    'ATE access.';
  RsTokenUnableTokenInformationLength = 'Cannot retrieve token informati' +
    'on length.';
  RsTokenNotEnoughMemoryTokenSave = 'Not enough memory to save token inf' +
    'ormation.';
  RsTokenUnableGetTokenInformation = 'Cannot retrieve token information.';
  RsTokenPrivilegeNotFound = 'Privilege "%s" not found.';
  RsTokenCallNTCompareTokensFailed = 'NtCompareTokens returned a NTSTATUS er' +
    'ror code %d.';
  RsTokenUnsupportedNTCompareTokens = 'NtCompareTokens is not implemented on' +
    ' this machine.';
  RsTokenUnableRemoveToken = 'Could not remove thread token.';
  RsTokeOnlyAttachImpersonatedToken = 'Only impersonation tokens can be atta' +
    'ched to threads.';
  RsTokenFailedSetToken = 'Could not set thread token.';
  RsTokenFailedImpLoggedOnUser = 'Could not impersonate logged on user.';
  RsTokenFailedImpAnonymousToken = 'Could not impersonate anonymous token.';
  RsTokenFailedImpSelf = 'Could not impersonate self.';
  RsTokenFailedRevertSelf = 'Could not revert to self.';
  RsTokenFailedImpPipe = 'Could not impersonate pipe client.';
  RsTokenCheckAccessTypeText = 'The desired access mask is not included in t' +
    'he token access mask!\r\nyour  mask: \r\ntoken mask: \r\n%0:s (%1:s)\r\%2' +
    ':s\r\nCheckTokenAccessType called by %3:s'+#13#10+
    'Token access rights:\r%4:s\rYour requested rights:\r%5:s';
	
  RsTokenStatisticsText = 'TokenID: %0:s\r\AuthenticationId: %1:s\r\nExpirat' +
    'ionTime: %2:s\r\nToken type: %3:d\r\nImpersonation level: 0x%4:x\r\nDynam' +
    'ic charged: 0x%5:x\r\nDynamic available: 0x%6:x\r\nGroup count: %7:d\r\nP' +
    'rivilege count: %8:d\r\nModified ID: %9:s\r\n';
  RsPrivilegeCallAdjustTokenFailed1 = 'Call to AdjustToken failed with privilege ' +
    '%s.';
  RsPrivilegeLuidText = 'hi: 0x%0:x, lo: 0x%1:x (0x%2:x)';
  RsTokenInvalidClass = 'The token instance "%s" is not a TJwSecurityToken.';
  RsProcessNotFound = 'The process "%0:s" could not be found.';  

//Unit JwsclVersion.pas

//
  RsVersionOrHigher = 'or higher';
  RsVersionUnsupportedVersion = 'Unsupported version "%0:s". Need "%1:s" %2:' +
    's';
  RsUnknownSuppliedOS = '(The supplied WindowsVersion to CheckWindowsVersion is unknown. Add it to JwsclResource.pas::sOSVerString)';

//Unit JwsclDesktop.pas

//
  RsDesktopFailedGetHeapSize = 'GetSecurityDesktopHeapSize : Could not get d' +
    'esktops heap size from registry';
  RsDesktopFailedEnumDesktops = 'GetSecurityDesktops : Could not get desktop' +
    's from station "%0:d"';
  RsDesktopFailedGetThreadDesktop = 'Could not get thread desktop by ThreadI' +
    'D "%0:d"';
  RsDesktopInvalidClosedDesktop = 'Desktop "%s" is not opened. ';
  RsDesktopCloseFailed = 'Could not close desktop named "%0:s" : (%1:d) %2:s';
  RsDesktopCreateFailed = 'Could not create desktop "%0:s"';
  RsDesktopAlreadyOpened = 'Desktop "%s" already opened. ';
  RsDesktopOpenFailed = 'Could not open to desktop "%0:s".';
  RsDesktopFailedSetFlags = 'Could not set DesktopFlags of desktop "%0:s"';
  RsDesktopFailedSetInheritFlag = 'Could not set Inherit flag of desktop "%0:s"';
  RsDesktopFailedSetThreadDesktop = 'Could not set thread to desktop "%0:s".';
  RsDesktopSwitchFailed = 'Could not switch to desktop "%s".';
  RsDesktopFailedSwitchBack = 'Could not switch back to desktop "%0:s"';
  RsDesktopFailedOpenDesktop = 'Could not open to desktop "%0:s"';

//Unit JwsclWinStation.pas

//
  RsWinStationCreateFailed = 'Call to CreateWindowStation failed. Tried to c' +
    'reate "%s"';
  RsWinStationOpenFailed = 'Call to OpenWindowStation failed. Tried to open ' +
    '"%s"';

//Unit JwsclUtils.pas

//
  RsInvalidLocalPointer  = 'Given pointer was not created by JwLocalAllocMem.'; 
  RsInvalidGlobalPointer = 'Given pointer was not created by JwGlobalAllocMem.';
  RsInvalidClassType = 'The given class type %0:s is invalid. Should be %1:s or derivates.';

//Unit JwsclEncryption.pas

//
  RsCryptUnsupportedMemManager = 'The memorymanager Local or Global is not supported by this method.';
  RsCryptNotImplemented = 'This methd %0:s is not implemented yet.';

//Unit JwsclSecurePrivateObjects.pas

//
  RsPrivateSaclAccessDenied = 'The audit acl could not be retrieved because the access was denied. Privilege SE_SECURITY_NAME could not be activated.';
  RsPrivateInvalidOwnerOrGroup = 'The security descriptor of a private object cannot be changed because the owner or group of the private object and also its parents is nil. Every object must have its own owner and group.';
  RsPrivateInvalidParentDescriptor = 'The method cannot be used for objects that do not have a parent of do not support inheritance.';

//Unit JwsclElevation.pas

//
  RsElevationRegDeleteError = 'The key %0:s could not be deleted.';
  RsElevationRegCreateError = 'The key %0:s could not be created.';
  RsElevationAbort          = 'The user aborted the elevation process.';
  RsSuRunShellExecute       = 'ShellExecute failed while trying to run SuRun.';
  RsSunRunFailed            = 'SuRun failed and returned status code: %0:d';

//Unit JwsclCryptProvider.pas

//
  RsKeyedHashNeedsKey     = 'A valid key is needed to create a hash object with a keyed algorithm.';
  RsNonKeyedHash          = 'A non-keyed hash cannot be created with a key.';

//Unit JwsclAuthCtx.pas

//
  RsInvalidObjectTypeList = 'The given array of ObjectType is invalid around index %0:d. ';

//Unit JwsclComUtils.pas

//
  RsInvalidPointerType = 'The given pointer type is not applicable.';
  RsInvalidWrapHandle = 'The given handle cannot be auto with TJwAutoPointer. Handles must not be zero (0) or INVALID_HANDLE_VALE.';

// Unit JwsclStreams.pas

//
  RsStreamsDataOutOfBounds = 'Stream data out of bounds.';


// Unit JwsclFirewall.pas

//
  RsFWInactive = 'Windows Firewall is inactive';
  RsFWNoExceptionsAllowed = 'Windows Firewall does not allow exceptions';



//General strings

//
  RsBracketNil = '(nil)';
  RsWinCallFailedWithNTStatus = 'Call to %0:s failed. NTError: %1:d';
  RsWinCallFailedWithSecurityStatus = 'Call to %0:s failed. SecurityStatus: %1:d';
  RsWinCallFailed = 'Call to %0:s failed.';
  RsNilParameter  = 'Parameter %0:s must not be nil';
  RsVistaFeaturesDisabled = 'EJwsclVistaFeaturesDisabled</B> is raised if the JWSCL library'+
    'was compiled with the compiler directive VISTA deactivated.'+
    'To use vista features you must activate the directive in file'+
    'includes\Jwscl.inc and make sure that you also compiled JwaWindows'+
    'with at least WINVISTA or WIN2008 to enable Vista features.';
  RsInfinite      = '(infinite)';

  RsResourceInitFailed = 'Loadstring failed when tried to load first string index %d.';
  RsResourceNotFound = 'The resource %d could not be loaded from string table. ';
  RsResourceUnequalCount = 'The high index %0:d does not correspond to the '+
    'given mapping array size %2:d.\r\nThe high index %0:d was read from '+
    'the string resource at string index given by parameter StartStringID %1:d.';
  RsProcessIdNotFound = 'The process "%0:s" (Id: 0x%1:x) could not be found.';
  RsOpenProcessFailed = 'The process "%0:s" (Id: 0x%1:x) could not be opened.';

  RsInvalidResultValue = 'The value result of a method is invalid: %0:d ';

  RsAccessDenied = 'Access denied!';

  RsInvalidFlags = 'Invalid flags!';

  RsInitWellKnownNotCalled = 'This method needs JwInitWellKnownSIDs from unit JwsclKnownSid to be called.';
  RsInitWellKnownNotCalled2 = 'One or more SIDs from unit JwsclKnownSid are not initialized. '+
    'The method called needs them. Following variables are needed: %0:s\r\n'+RsInitWellKnownNotCalled;

  RsNilPointer = 'A given pointer was unexpectedly NIL.';

  RsInvalidComputer = 'The given computer name is invalid.';

  RsInvalidIndex = 'The given index %0:d is invalid for this call.';

  RsUnimplementedFeature = 'The feature "%s" is not yet implemented.';

// jwsclProcess.pas

//
  RsInvalidParameterIndex = 'The given index "%0:d" in parameter "%1:s" is out of range.';
  RsMissingEvent = 'The event "%0:s" needs to be assigned. It must not be nil.';
  RsInvalidJobObject = 'The returned job object is invalid and cannot be used to assign a job to.';
  RsEnumerateProcessesFailed = 'The enumeration of processes failed. ';
  RsInvalidStartupInfo = 'The given Startupinfo must be initialized!';

// Unit names - do not localize
// use these constants in the source parameter of any JWSCL Exception.

//
const
  RsUNAccountControl     = 'JwsclAccountControl.pas';
  RsUNAcl                = 'JwsclAcl.pas.pas';
  RsUNAnsi_UniCode       = 'JwsclAnsi_Unicode.pas';
  RsUNAuthZCtx           = 'JwsclAuthCtx.pas';
  RsUNCertificates       = 'JwsclCertificates.pas';
  RsUNComUtils           = 'JwsclComUtils.pas';
  RsUNConstants          = 'JwsclConstants.pas';
  RsUNCredentials        = 'JwsclCredentials.pas';
  RsUNCryptProvider      = 'JwsclCryptProvider.pas';
  RsUNDescriptor         = 'JwsclDescriptor.pas';
  RsUNDesktops           = 'JwsclDesktops.pas';
  RsUNElevation          = 'JwsclElevation.pas'; 
  RsUNExceptions         = 'JwsclExceptions.pas';
  RsUNFirewall           = 'JwsclFirewall.pas';
  RsUNImpersonation      = 'JwsclImpersonation.pas';
  RsUNKnownSid           = 'JwsclKnownSid.pas';
  RsUNLSA                = 'JwsclLSA.pas';
  RsUNMapping            = 'JwsclMapping.pas';
  RsUNProcess            = 'JwsclProcess.pas';
  RsUNResource           = 'JwsclResource.pas';
  RsUNSecureObjects      = 'JwsclSecureObjects.pas';
  RsUNSecurityDialogs    = 'JwsclSecurityDialogs.pas';
  RsUNSid                = 'JwsclSid.pas';
  RsUNTerminalServer     = 'JwsclTerminalServer.pas';
  RsUNToken              = 'JwsclToken.pas';
  RsUNTypes              = 'JwsclTypes.pas';
  RsUNUtils              = 'JwsclUtils.pas';
  RsUNVersion            = 'JwsclVersion.pas';
  RsUNWinStation         = 'JwsclWinStation.pas';
  RsUNEncryption         = 'JwsclEncryption.pas';
  RsUNSecurePrivateObjects = 'JwsclSecurePrivateObjects.pas';
  RsUNStreams            = 'JwsclStreams.pas';

  RsUnLibrary = 'JwscLibrary.pas';


{<B>GetResourceStringIdentifier</B> converts a Delphi resourcestring into a resource identifier.
Use ResourcestringName as parameter rs (e.g. RsInvalidIndex).

The function cannot fail for correct delphi resourcestring identifiers otherwise
the return value is undefined.
}
function GetResourceStringIdentifier(rs: PResStringRec) : Integer;


implementation
uses JwaWindows;


function GetResourceStringIdentifier(rs: PResStringRec) : Integer;
var oldProtect : Cardinal;
begin
  VirtualProtect(rs, SizeOf(rs^), PAGE_EXECUTE_READWRITE, @oldProtect);
  result := rs^.Identifier;
  VirtualProtect(rs, SizeOf(rs^), oldProtect, @oldProtect);
end;

end.
