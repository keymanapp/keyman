(*
  Name:             main
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    10 Oct 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Added rootdir to product install
                    28 Sep 2006 - mcdurdin - Added Check for Updates and Start With Windows
                    06 Oct 2006 - mcdurdin - Always install keyboards and packages for all users
                    04 Dec 2006 - mcdurdin - Cleanup keyboard and package installation
                    15 Jan 2007 - mcdurdin - Don't reinstall an existing product.pxx (otherwise keyboards are uninstalled!)
                    16 May 2007 - mcdurdin - I810 - Open uninstall feedback webpage at the end of the Keyman Desktop uninstaller
                    23 Aug 2007 - mcdurdin - I985 - Fix desktop_xxx.pxx not removed from registry at uninstall
                    14 Sep 2007 - mcdurdin - I985 - desktop_xxx.pxx removal must be done under CU not LM
                    01 Jun 2009 - mcdurdin - Vista TC23 - delete hotkeys key on rollback
                    04 Jun 2009 - mcdurdin - Vista TC23 - delete languagehotkeys key on rollback
                    14 Jun 2009 - mcdurdin - Vista TC23
                    31 Jan 2011 - mcdurdin - I1729 - List keyboards in uninstall feedback
                    28 Feb 2011 - mcdurdin - I1729 - List keyboards in uninstall feedback (fix for backing up keyboard list before msi uninstalls)
                    18 Mar 2011 - mcdurdin - I1729 - Uninstall feedback should also list keyboards
                    10 Oct 2014 - mcdurdin - I4436 - V9.0 - browser emulation control for kmshell breaks downlevel versions of Keyman
*)

{$WARN SYMBOL_PLATFORM OFF}

unit main;

interface

procedure Run;

implementation

uses SysUtils, Windows, wininet, klog, utilfiletypes, RegistryKeys, versioninfo,
  KeymanPaths,
  utilexecute;

procedure UninstallUser;
var
  hk: Windows.HKEY;
  hkeyBackup: Windows.HKEY;
  n: Integer;
  szValueName, szData: array[0..127] of char;
  cValueName, cbData, dwType: Cardinal;
  hkey: Windows.HKEY;
begin
  KL.MethodEnter(nil, 'Uninstall', []);
  try
    { I985: desktop_xxx.pxx not removed from registry at uninstall }
    if RegOpenKeyEx(HKEY_CURRENT_USER, PChar(SRegKey_WindowsRun_CU), 0, KEY_ALL_ACCESS, hk) = ERROR_SUCCESS then
    begin
      RegDeleteValue(hk, PChar(SRegValue_WindowsRun_Keyman));
      RegCloseKey(hk);
    end
    else
      KL.LogError('Uninstall: unable to delete startup entry '+SRegValue_WindowsRun_Keyman+' in '+SRegKey_WindowsRun_CU+': '+SysErrorMessage(GetLastError));

    if RegOpenKeyEx(HKEY_CURRENT_USER, SRegKey_ActiveKeyboards_CU, 0, KEY_READ, hkey) = ERROR_SUCCESS then  // I1729
    begin
      if RegCreateKeyEx(HKEY_CURRENT_USER, SRegKey_UninstallBackupKeyboards_CU, 0, nil, 0, KEY_ALL_ACCESS, nil, hkeyBackup, nil) = ERROR_SUCCESS then
      begin
        cValueName := SizeOf(szValueName) div SizeOf(szValueName[0]);
        cbData := SizeOf(szData);
        n := 0;
        while RegEnumValue(hkey, n, szValueName, cValueName, nil, @dwType, @szData, @cbData) = ERROR_SUCCESS do
        begin
          if dwType = REG_SZ then
            RegSetValueEx(hkeyBackup, szValueName, 0, REG_SZ, @szData, cbData);
          cbData := SizeOf(szData);
          cValueName := SizeOf(szValueName) div SizeOf(szValueName[0]);
          Inc(n);
        end;
        RegCloseKey(hkeyBackup);
      end;
      RegCloseKey(hkey);
    end;
  finally
    KL.MethodExit(nil, 'Uninstall');
  end;
end;

procedure RollbackCU;
begin
  RegDeleteKey(HKEY_CURRENT_USER, PChar(SregKey_KeymanHotkeys_CU));
  RegDeleteKey(HKEY_CURRENT_USER, PChar(SRegKey_LanguageHotkeys_CU));
  RegDeleteKey(HKEY_CURRENT_USER, PChar(SRegKey_KeymanEngine_CU));
  RegDeleteKey(HKEY_CURRENT_USER, PChar(SRegKey_KeymanEngineRoot_CU));
  RegDeleteKey(HKEY_CURRENT_USER, PChar(SRegKey_KeymanRoot_CU));
end;

procedure RollbackLM;
begin
  RollbackCU; // Because we could be in a different user context, we'll clean CU as well as LM now
  RegDeleteKey(HKEY_LOCAL_MACHINE, PChar(SRegKey_KeymanEngine_LM));
  RegDeleteKey(HKEY_LOCAL_MACHINE, PChar(SRegKey_KeymanEngineRoot_LM));
  RegDeleteKey(HKEY_LOCAL_MACHINE, PChar(SRegKey_KeymanRoot_LM));
end;

procedure Run;
var
  FCommand: string;
begin
  //Windows.MessageBox(0, PChar('Welcome to insthelp - '+CmdLine), 'insthelp', MB_ICONERROR or MB_OK);
  try
    KL.MethodEnter(nil, 'Run', []);
    try
      KL.Log('Command line: '+CmdLine);

      FCommand := LowerCase(ParamStr(1));

      if FCommand = '-rcu' then
        RollbackCU
      else if FCommand = '-rlm' then
        RollbackLM
      else if FCommand = '-uu' then
        UninstallUser
      else
      begin
        KL.LogError('Invalid parameters: '+CmdLine);
        Windows.MessageBox(0, 'Usage: insthelp -uu|-rcu|-rlm', 'insthelp', MB_ICONERROR or MB_OK);
        Exit;
      end;
    finally
      KL.MethodExit(nil, 'Run');
    end;
  except
    on E:Exception do
    begin
      KL.LogError('Exception %s: %s', [E.ClassName, E.Message]);
      Windows.MessageBox(0, PChar('Exception '+E.ClassName+': '+E.Message), 'insthelp', MB_ICONERROR or MB_OK);
    end;
  end;
end;

end.

