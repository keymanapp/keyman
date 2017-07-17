program Impersonate1;

{$APPTYPE CONSOLE}

uses
  JwaWindows,
  JwaVista,
  jwsclConstants,
  jwsclTypes,
  JwsclMapping,
  JwsclSid,
  JwsclToken,
  JwsclResource,
  JwsclDescriptor,
  JwsclKnownSid;


 (*
procedure ImpersonateAndCheckAccess(
  hNamedPipe : THANDLE;               // handle of pipe to impersonate
  pSD : TJwSecurityDescriptor;        // security descriptor to check
  dwAccessDesired : DWORD;           // access rights to check
  pGeneric : TJwSecurityGenericMappingClass;       // generic mapping for object
  out pdwAccessAllowed : TJwAccessMask         // returns allowed access rights
);
var Token : TJwSecurityToken;
begin
  Token := TJwSecurityToken.C
   PRIVILEGE_SET PrivilegeSet;
   DWORD dwPrivSetSize = sizeof( PRIVILEGE_SET );
   BOOL fAccessGranted=FALSE;

// Impersonate the client.

   ImpersonateNamedPipeClient(hNamedPipe);   

// Get an impersonation token with the client's security context.

   OpenThreadToken( GetCurrentThread(), TOKEN_ALL_ACCESS,
         TRUE, &hToken);

// Use the GENERIC_MAPPING structure to convert any
// generic access rights to object-specific access rights.

   MapGenericMask( &dwAccessDesired, pGeneric );

   // Check the client's access rights.

   if( !AccessCheck(
      pSD,                 // security descriptor to check
      hToken,              // impersonation token
      dwAccessDesired,     // requested access rights
      pGeneric,            // pointer to GENERIC_MAPPING
      &PrivilegeSet,       // receives privileges used in check
      &dwPrivSetSize,      // size of PrivilegeSet buffer
      pdwAccessAllowed,    // receives mask of allowed access rights
      &fAccessGranted ))   // receives results of access check
   {
      goto Cleanup;
   }

Cleanup:

   RevertToSelf();

   if (hToken != INVALID_HANDLE_VALUE)
      CloseHandle(hToken);

   return fAccessGranted;
}  *)
var S : TJwSecurityId;
begin
  S := TJwSecurityId.CreateWellKnownSid(WinWorldSid,nil);





end.
