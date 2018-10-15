#include "pch.h"
#include "security.h"
#include <accctrl.h>
#include <sddl.h>
#include <VersionHelpers.h>
// TODO: replace GetVersion, GetVersionEx calls with IsWindowsXYOrGreater

LPCWSTR LOW_INTEGRITY_SDDL_SACL_W = L"S:(ML;;NW;;;LW)";

#define LABEL_SECURITY_INFORMATION (0x00000010L)

#pragma warning(push)
#pragma warning(disable: 4996)
/**
 Sets the security information label for an object to Low Integrity
*/
BOOL SetObjectToLowIntegrity(HANDLE hObject, SE_OBJECT_TYPE type)
{
  BOOL bRet = FALSE;
  DWORD dwErr = ERROR_SUCCESS;
  PSECURITY_DESCRIPTOR pSD = NULL;
  PACL pSacl = NULL;
  BOOL fSaclPresent = FALSE;
  BOOL fSaclDefaulted = FALSE;

  if (LOBYTE(LOWORD(GetVersion())) < 6) return TRUE;

  if (ConvertStringSecurityDescriptorToSecurityDescriptorW(LOW_INTEGRITY_SDDL_SACL_W, SDDL_REVISION_1, &pSD, NULL))
  {
    if (GetSecurityDescriptorSacl(pSD, &fSaclPresent, &pSacl, &fSaclDefaulted))
    {
      dwErr = SetSecurityInfo(
        hObject, type, LABEL_SECURITY_INFORMATION,
        NULL, NULL, NULL, pSacl);

      bRet = (ERROR_SUCCESS == dwErr);
    }

    LocalFree(pSD);
  }

  return bRet;
}
#pragma warning(pop)

/**
 Grants access to metro-style applications such as Skype, Edge, Windows Search for a given object. This permits
 IPC and synchronization
 See https://stackoverflow.com/questions/17761826/assigning-folder-permissions-to-all-application-packages-group
*/
BOOL GrantPermissionToAllApplicationPackages(HANDLE handle, DWORD dwAccessPermissions, SE_OBJECT_TYPE type) {
  // ALL APPLICATION PACKAGES group is introduced in Windows 8
  if (!IsWindows8OrGreater())
    return TRUE;

  BOOL bRet = FALSE;
  PACL pOldDACL = NULL, pNewDACL = NULL;
  PSECURITY_DESCRIPTOR pSD = NULL;
  EXPLICIT_ACCESS ea;
  SECURITY_INFORMATION si = DACL_SECURITY_INFORMATION;
  PSID pSID = NULL;
  DWORD cbSid = SECURITY_MAX_SID_SIZE;

  // Get a pointer to the existing DACL.
  DWORD dwRes = GetSecurityInfo(handle, type, DACL_SECURITY_INFORMATION, NULL, NULL, &pOldDACL, NULL, &pSD);
  if (ERROR_SUCCESS != dwRes) {
    DebugLastError0(dwRes, "GetSecurityInfo");
    goto Cleanup;
  }

  // Allocate enough memory for the largest possible SID.
  pSID = LocalAlloc(LMEM_FIXED, cbSid);
  if (pSID == NULL) {
    DebugLastError("LocalAlloc");
    goto Cleanup;
  }

  // Create a SID for the WinBuiltinAnyPackageSid group on the local computer.
  if (!CreateWellKnownSid(WinBuiltinAnyPackageSid, NULL, pSID, &cbSid)) {
    DebugLastError("CreateWellKnownSid");
    goto Cleanup;
  }

  // Initialize an EXPLICIT_ACCESS structure for the new ACE. 
  ZeroMemory(&ea, sizeof(EXPLICIT_ACCESS));
  ea.grfAccessPermissions = dwAccessPermissions;
  ea.grfAccessMode = SET_ACCESS;
  ea.grfInheritance = SUB_CONTAINERS_AND_OBJECTS_INHERIT;

  ea.Trustee.TrusteeForm = TRUSTEE_IS_SID;
  ea.Trustee.TrusteeType = TRUSTEE_IS_WELL_KNOWN_GROUP;
  ea.Trustee.ptstrName = (LPSTR)pSID;

  // Create a new ACL that merges the new ACE into the existing DACL.
  dwRes = SetEntriesInAcl(1, &ea, pOldDACL, &pNewDACL);
  if (ERROR_SUCCESS != dwRes) {
    DebugLastError0(dwRes, "SetEntriesInAcl");
    goto Cleanup;
  }

  // Attach the new ACL as the object's DACL.
  dwRes = SetSecurityInfo(handle, type, si, NULL, NULL, pNewDACL, NULL);
  if (ERROR_SUCCESS != dwRes) {
    DebugLastError0(dwRes, "SetSecurityInfo");
    goto Cleanup;
  }

  bRet = TRUE;

Cleanup:
  if (pSID != NULL)
    LocalFree((HLOCAL)pSID);
  if (pSD != NULL)
    LocalFree((HLOCAL)pSD);
  if (pNewDACL != NULL)
    LocalFree((HLOCAL)pNewDACL);

  return bRet;
}
