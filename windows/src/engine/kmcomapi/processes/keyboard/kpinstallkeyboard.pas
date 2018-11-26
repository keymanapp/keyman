(*
  Name:             kpinstallkeyboard
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    30 Apr 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Add AutoApplyKeyman call
                    04 Dec 2006 - mcdurdin - Check product licence on install
                    12 Dec 2006 - mcdurdin - Test for installed keyboard count
                    14 Dec 2006 - mcdurdin - Fix unregistered keyboard message
                    30 Jan 2007 - mcdurdin - Don't associate with language for light edition
                    30 May 2007 - mcdurdin - I850 - Fix shadow keyboard install in Vista
                    05 Nov 2007 - mcdurdin - I1088 - Language switching for Light
                    20 Jul 2008 - mcdurdin - I1543 - Fixup wrong character output for Vista+ANSI+System Shadow keyboard
                    16 Jan 2009 - mcdurdin - I1626 - Fix crash trying to read a missing file
                    26 Jul 2010 - mcdurdin - I2467 - 8.0 renumber
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    17 Aug 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    20 Nov 2012 - mcdurdin - I3581 - V9.0 - KMTip needs to pass activated profile guid through to Keyman32 to switch keyboards
                    28 Nov 2012 - mcdurdin - I3599 - V9.0 - Refactor GetKeyboardIconFileName
                    01 Dec 2012 - mcdurdin - I3619 - V9.0 - Substitute KBDUS or default HKL for kmtip per mnemonic status
                    01 Dec 2012 - mcdurdin - I3612 - V9.0 - Keyboard install should run as Admin only
                    01 Dec 2012 - mcdurdin - I3613 - V9.0 - System shadow keyboards obsolete, strip out remaining code
                    01 Jan 2013 - mcdurdin - I3707 - V9.0 - Installed keyboards show keyboard short name instead of full name in Language Bar
                    11 Aug 2013 - mcdurdin - I3768 - V9.0 - Remove TSF substitution code
                    16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard
                    28 May 2014 - mcdurdin - I4220 - V9.0 - Remove references to LoadKeyboardLayout, Preload, Substitutes, etc. and use only TSF
                    16 Jun 2014 - mcdurdin - I4273 - V9.0 - Convert keyboards to Unicode before installing
                    03 Jul 2014 - mcdurdin - I4316 - V9.0 - Keyboard icons should be converted to 32BPP RGBA on install
                    03 Feb 2015 - mcdurdin - I4574 - V9.0 - If any files are read-only, they need the read-only flag removed on install
                    04 Mar 2015 - mcdurdin - I4607 - V9.0 - Support install of keyboard against fallback locales
                    30 Apr 2015 - mcdurdin - I4682 - V9.0 - Installing a keyboard with an OSK from Developer fails to install the OSK
*)
unit kpinstallkeyboard;  // I3306

interface

uses
  Winapi.Windows,
  PackageInfo,
  kpbase;

type
  TKPInstallKeyboardOptions = set of (ikPartOfPackage);

  TKPInstallKeyboard = class(TKPBase)
    procedure Execute(const FileName, PackageName: string; FInstallOptions: TKPInstallKeyboardOptions; Languages: TPackageKeyboardLanguageList; Force: Boolean);
  private
    procedure RegisterLanguageProfile(Langs: array of Integer;
      const KeyboardName, KeyboardDescription, IconFileName: string); overload;
    function RegisterLanguageProfile(const BCP47Tag, KeyboardName,
      KeyboardDescription, IconFileName, LanguageName: string): Boolean; overload;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Variants,
  Vcl.Graphics,

  Keyman.System.LanguageCodeUtils,

  ErrorControlledRegistry,
  kmxfile,
  keymanerror,
  utilkeyman,
  utilsystem,
  isadmin,
  RegistryKeys,
  GetOsVersion,
  glossary,
  utilexecute,
  keymanerrorcodes,
  keymancontext,
  kpinstallkeyboardlanguageprofiles,
  OnlineConstants,
  KLog,
  custinterfaces,
  utilolepicture,
  kprecompilemnemonickeyboard,
  KPInstallVisualKeyboard,
  utildir,
  KeymanPaths,
  kmxfileconsts,
  utilicon,
  utilstr,
  keymanapi_TLB;

procedure TKPInstallKeyboard.Execute(const FileName, PackageName: string; FInstallOptions: TKPInstallKeyboardOptions; Languages: TPackageKeyboardLanguageList; Force: Boolean);
var
  ki: TKeyboardInfo;
  FDestPath: string;
  kbdname: string;
  FInstByAdmin: Boolean;
  FDestFileName: string;
  FIconFileName: string;
  FDefaultHKL: HKL;
  FMCompilePath: string;
  FAllLanguagesW: WideString;
  FAllLanguages: string;
  FLanguage, FLogText: string;
  FLanguages: array of Integer;
  FExitCode: Integer;
  FKVKName: WideString;
  FLanguageInstalled, FCreatedIcon: Boolean;
  FLanguageID: Integer;
  i: Integer;

type
  TWSLCallback = reference to procedure(r: TRegistryErrorControlled);
  procedure WriteSuggestedLanguages(c: TWSLCallback);
  var
    r: TRegistryErrorControlled;
  begin
    r := TRegistryErrorControlled.Create;
    try
      r.RootKey := HKEY_LOCAL_MACHINE;
      if not r.OpenKey('\'+GetRegistryKeyboardInstallKey_LM(FileName)+'\'+SRegSubKey_SuggestedLanguages, True) then
        r.RaiseLastRegistryError;
      c(r);
    finally
      r.Free;
    end;
  end;

  procedure AddLanguage(FLanguageID: Integer);
  var
    i: Integer;
  begin
    for i := 0 to High(FLanguages) do
      if FLanguages[i] = FLanguageID then
        Exit;

    SetLength(FLanguages, Length(FLanguages)+1);
    FLanguages[High(FLanguages)] := FLanguageID;
  end;


begin
  KL.MethodEnter(Self, 'Execute', [FileName,PackageName,ikPartOfPackage in FInstallOptions ,Force]);
  try
    kbdname := GetShortKeyboardName(FileName);

    if not IsAdministrator then
      Error(KMN_E_Install_KeyboardMustBeInstalledByAdmin);

    { Get information and show install dialog }

    try
      GetKeyboardInfo(FileName, True, ki, True);   // I3581
    except
      on E:EFOpenError do // I1626
        ErrorFmt(KMN_E_Install_InvalidFile, VarArrayOf([ExtractFileName(FileName), E.Message]));
      on E:EKMXError do
        ErrorFmt(KMN_E_Install_InvalidFile, VarArrayOf([ExtractFileName(FileName), E.Message]));
    end;

    try
      { Uninstall existing keyboard }

      if KeyboardInstalled(kbdname, FInstByAdmin) and not Force then
        ErrorFmt(KMN_E_Install_AlreadyInstalled, VarArrayOf([kbdname]));

      if ikPartOfPackage in FInstallOptions
        then FDestPath := GetPackageInstallPath(PackageName)   // I3581
        else FDestPath := GetKeyboardInstallPath(FileName);   // I3581

      FDestFileName := FDestPath + '\' + ExtractFileName(FileName);   // I3581
   // I3613
      { Copy the keyboard into the dest dir.  If it is a package, this is already done. }

      if not (ikPartOfPackage in FInstallOptions) then
      begin
        if not ForceDirectories(FDestPath) then ErrorFmt(KMN_E_Install_FailureToCreateDirectories, VarArrayOf([kbdname]));
        if not CopyFileCleanAttr(PChar(FileName), PChar(FDestFileName), False) then   // I4574
          ErrorFmt(KMN_E_KeyboardInstall_UnableToCopyFile, VarArrayOf([FileName, FDestPath]));
      end;

      { Extract icon from keyboard and save to the folder }

      FIconFileName := GetKeyboardIconFileName(FDestFileName);   // I3581   // I3599

      try
        FCreatedIcon := ConvertKeyboardBitmapToAlphaIcon(ki, FIconFileName);   // I4316
      except
        on E:Exception do
        begin
          WarnFmt(KMN_W_KeyboardInstall_InvalidIcon, VarArrayOf([FileName, E.ClassName + '-' + E.Message]));
          FCreatedIcon := False;
        end;
      end;

      if not FCreatedIcon then
      begin
        with TIcon.Create do
        try
          LoadFromResourceName(HInstance, 'kbd_noicon');
          SaveToFile(FIconFileName);
        finally
          Free;
        end;
      end;

      { Create registry entries }

      with TRegistryErrorControlled.Create do  // I2890
      try
        { Write Installed Keyboards entry }

        RootKey := HKEY_LOCAL_MACHINE;
        if not OpenKey('\'+GetRegistryKeyboardInstallKey_LM(FileName), True) then  // I2890
          RaiseLastRegistryError;

        WriteString(SRegValue_KeymanFile, FDestFileName);

        WriteString(SRegValue_Legacy_DefaultLanguageID, IntToHex(ki.KeyboardID, 8));   // I4220

        if ikPartOfPackage in FInstallOptions
          then WriteString(SRegValue_PackageName, GetShortPackageName(PackageName))
          else if ValueExists(SRegValue_PackageName) then DeleteValue(SRegValue_PackageName);

        FDefaultHKL := GetDefaultHKL;   // I3619

        //
        // Use BCP47ID from package metadata if it has been passed in
        //
        if Assigned(Languages) and (Languages.Count > 0) then
        begin
          // Use language data from package to install; we only install
          // the first language now and add the rest to the registry for
          // future addition by the user

          FLanguageInstalled := False;
          for i := 0 to Languages.Count - 1 do
          begin
            FLanguageInstalled := RegisterLanguageProfile(Languages[i].ID, kbdname, ki.KeyboardName, FIconFileName, Languages[i].Name);
            if FLanguageInstalled then
              Break;
          end;

          if not FLanguageInstalled then
          begin
            // All languages failed to install, so add to the default language.
            // This is most likely to happen on Win7 where custom BCP 47 tags
            // are not allowed
            AddLanguage(HKLToLanguageID(FDefaultHKL));
            RegisterLanguageProfile(FLanguages, kbdname, ki.KeyboardName, FIconFileName);   // I3581   // I3707
          end;

          // Save the list of preferred languages for the keyboard, to the registry, for future installation.
          WriteSuggestedLanguages(
            procedure(r: TRegistryErrorControlled)
            var
              i: Integer;
            begin
              for i := 0 to Languages.Count - 1 do
                r.WriteString(Languages[i].ID, Languages[i].Name);
            end
          );
        end
        else
        begin
          //
          // Use keyboard-defined default profile first   // I4607
          //
          if ki.KeyboardID <> 0 then
          begin
            AddLanguage(ki.KeyboardID);
          end;

          //
          // Then enumerate all defined languages to try next   // I4607
          //
          GetSystemStore(ki.MemoryDump.Memory, TSS_WINDOWSLANGUAGES, FAllLanguagesW);

          FAllLanguages := FAllLanguagesW;
          while FAllLanguages <> '' do
          begin
            FLanguage := StrToken(FAllLanguages, ' ');
            Delete(FLanguage, 1, 1); // 'x'
            if TryStrToInt('$'+FLanguage, FLanguageID) then
            begin
              AddLanguage(FLanguageID);
            end;
          end;

          //
          // Final fallback is to install against default language for system   // I4607
          //
          if Length(FLanguages) = 0 then
            AddLanguage(HKLToLanguageID(FDefaultHKL));

          // Registers only the first language
          RegisterLanguageProfile(FLanguages, kbdname, ki.KeyboardName, FIconFileName);   // I3581   // I3707

          // Save the list of preferred languages for the keyboard, translated to BCP47 tags
          // to the registry, for future installation.
          WriteSuggestedLanguages(
            procedure(r: TRegistryErrorControlled)
            var
              i: Integer;
            begin
              for i := 0 to High(FLanguages) do
                r.WriteString(TLanguageCodeUtils.TranslateWindowsLanguagesToBCP47(FLanguages[i]), '');
            end
          );
        end;

        CloseKey;
      finally
        Free;
      end;

      // If a standalone keyboard has a visual keyboard, copy it too and install it   // I4682
      if GetSystemStore(ki.MemoryDump.Memory, TSS_VISUALKEYBOARD, FKVKName) and
        not (ikPartOfPackage in FInstallOptions) then
      begin
        if FileExists(ExtractFilePath(FileName) + FKVKName) then
          with TKPInstallVisualKeyboard.Create(Context) do
          try
            Execute(ExtractFilePath(FileName) + FKVKName, kbdname);
          finally
            Free;
          end;
      end;

      // Convert the keyboard from ANSI to Unicode
      if not (kmxfile.keUnicode in ki.Encodings) then   // I4273
      begin
        FMCompilePath := TKeymanPaths.KeymanEngineInstallPath(TKeymanPaths.S_MCompileExe);
        // Run mcompile to convert to Unicode, in place
        if not TUtilExecute.Console(Format('"%s" -u "%s" "%s"', [FMCompilePath, FDestFileName, FDestFileName]),
            ExtractFilePath(FDestFileName), FLogText, FExitCode) then
        begin
          KL.Log('Could not recompile keyboard for Unicode: '+FLogText);
          Exit;
        end;
        KL.Log(FLogText);
      end;

      // Recompile a mnemonic layout to the user's selected base layout
      if ki.MnemonicLayout then   // I4169
      begin
        with TKPRecompileMnemonicKeyboard.Create(Context) do
        try
          Execute(FDestFileName, PackageName);
        finally
          Free;
        end;
      end;
    finally
      if Assigned(ki.Icon) then   // I4316
        FreeAndNil(ki.Icon);
      if Assigned(ki.Bitmap) then
        FreeAndNil(ki.Bitmap);
      ki.MemoryDump.Free;
    end;

    Context.Control.AutoApplyKeyman;

    KL.Log('Keyboard '+FileName+' installed successfully.');
  finally
    KL.MethodExit(Self, 'Execute');
  end;
end;

procedure TKPInstallKeyboard.RegisterLanguageProfile(Langs: array of Integer; const KeyboardName, KeyboardDescription, IconFileName: string);   // I3581   // I3619   // I3707   // I3768   // I4607
begin
  with TKPInstallKeyboardLanguageProfiles.Create(Context) do
  try
    Execute(KeyboardName, KeyboardDescription, Langs, IconFileName, True);   // I3707   // I3768   // I4607
  finally
    Free;
  end;
end;

function TKPInstallKeyboard.RegisterLanguageProfile(const BCP47Tag, KeyboardName, KeyboardDescription, IconFileName, LanguageName: string): Boolean;   // I3581   // I3619   // I3707   // I3768   // I4607
begin
  with TKPInstallKeyboardLanguageProfiles.Create(Context) do
  try
    Result := Execute(KeyboardName, KeyboardDescription, BCP47Tag, IconFileName, LanguageName);   // I3707   // I3768   // I4607
  finally
    Free;
  end;
end;

end.

