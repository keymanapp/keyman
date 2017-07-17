program CreateRestrictedToken;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  JwaWindows,
  JwsclToken,
  JwsclKnownsid,
  JwsclSid,
  JwsclTypes;

var RestrToken : TJwSecurityToken;
    Privs : TJwPrivilegeSet;
    Sids : TJwSecurityIdList;

    lpStartupInfo        : STARTUPINFOW;
    lpProcessInformation : PROCESS_INFORMATION;
begin
  JwInitWellKnownSIDs;

  Privs := TJwPrivilegeSet.Create;
  Privs.AddPrivilege(SE_CHANGE_NOTIFY_NAME);
  Sids := TJwSecurityIdList.Create();
  //Sids.Add(JwAdministratorsSID);



  RestrToken := nil;
  try
    RestrToken := TJwSecurityToken.CreateRestrictedToken(
      0,
      TOKEN_ALL_ACCESS,//const TokenAccessMask: TJwTokenAccessMask;
      0,//DISABLE_MAX_PRIVILEGE,//const Flags: cardinal;
      Sids,//const SidsToDisable: TJwSecurityIdList;
      Privs,//const PrivilegesToDelete: TJwPrivilegeSet;
      nil//const RestrictedSids: TJwSecurityIdList);
    );

    ZeroMemory(@lpStartupInfo, sizeof(lpStartupInfo));
    lpStartupInfo.cb := sizeof(lpStartupInfo);

    if not CreateProcessAsUserW(
      RestrToken.TokenHandle,//hToken: HANDLE;
      'C:\Windows\explorer.exe',
      //'P:\Eigene Dateien\Dezipaitor\Projekte\Delphi\7\jedi-api-lib\jwscl\trunk\examples\Manual\filenotify.exe',
      //'C:\Windows\PsTools\Procmon.exe',
      //'C:\Windows\System32\cmd.exe',//lpApplicationName: LPCSTR;

      nil,//'/root C:\',//lpCommandLine: LPSTR;
      nil,//lpProcessAttributes: LPSECURITY_ATTRIBUTES;
      nil,//lpThreadAttributes: LPSECURITY_ATTRIBUTES;
      true,//bInheritHandles: BOOL;
      0,//dwCreationFlags: DWORD;
      nil,//lpEnvironment: LPVOID;
      'C:\',//lpCurrentDirectory: LPCSTR;
      lpStartupInfo,//const lpStartupInfo: STARTUPINFOA;
      lpProcessInformation//var lpProcessInformation: PROCESS_INFORMATION
      ) then
       RaiseLastOSError;


  finally
    Privs.Free;
    RestrToken.Free;   
    Sids.free;
  end;
end.
