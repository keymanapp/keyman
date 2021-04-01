unit KPInstallPackageStartMenu;

interface

uses
  keymanapi_TLB,
  kmpinffile;

type
  TKPInstallPackageStartMenu = class
  private
    inf: TKMPInfFile;
    PackageName, dest: string;
  public
    constructor Create(Ainf: TKMPInfFile; APackageName, Adest: string);
    function Execute(AllUsers: Boolean; const ShortcutRootPath: WideString): Boolean;
  end;

implementation

uses
  custinterfaces,
  KeymanPaths,

  System.Classes,
  System.IniFiles,
  System.SysUtils,
  Winapi.ShlObj,
  Winapi.Windows,

  PackageInfo,
  StockFileNames,
  utilsystem,
  utilwow64;

function GetSystemWow64Directory(buf: PChar; uSize: UINT): UINT; stdcall; external 'kernel32' name 'GetSystemWow64DirectoryW';  // I2176

constructor TKPInstallPackageStartMenu.Create(Ainf: TKMPInfFile; APackageName, Adest: string);
begin
  inherited Create;
  inf := Ainf;
  PackageName := APackageName;
  dest := Adest;
end;

function TKPInstallPackageStartMenu.Execute(AllUsers: Boolean; const ShortcutRootPath: WideString): Boolean;
var
  buf: array[0..260] of char;
  programs: string;
  lnkfile: string;
  i: Integer;
  srcfile: string;
  iconfile: string;
  params: string;
  n: Integer;
  workingdir: string;
  guidUniquifier: TGUID;
  Uniquifier, desc: WideString;

  function GetHHIcon: string;
  var
    buf: array[0..260] of char;
  begin
    GetWindowsDirectory(buf, 260);
    Result := IncludeTrailingPathDelimiter(buf)+'hh.exe';
    if FileExists(Result) then Exit;
    GetSystemDirectory(buf, 260);
    Result := IncludeTrailingPathDelimiter(buf)+'hh.exe';
    if FileExists(Result) then Exit;
    Result := '';
  end;
begin
  if inf.StartMenu.DoCreate then
  begin
    // This ensures that multiple start menu entries will be visible even when
    // they have the same target. See https://superuser.com/a/1395288/521575
    CreateGuid(guidUniquifier);
    Uniquifier := '-unique-'+GuidToString(guidUniquifier);

    if ShortcutRootPath <> '' then programs := ShortcutRootPath
    else if not AllUsers
      then programs := GetFolderPath(CSIDL_PROGRAMS)          // Programs menu for 95/98, or restricted user in NT
      else programs := GetFolderPath(CSIDL_COMMON_PROGRAMS);  // Common programs menu for WinNT


    if programs = '' then
      Exit(False); //Warn(KMN_W_InstallPackage_UnableToFindProgramsFolder)

    programs := IncludeTrailingPathDelimiter(programs);

    with TIniFile.Create(dest+PackageFile_KMPInf) do
    try
      WriteString('Uninstall','StartMenuPath', programs + inf.StartMenu.Path);
    finally
      Free;
    end;

    if inf.StartMenu.AddUninstallEntry then
    begin
      lnkfile := IncludeTrailingPathDelimiter(programs + inf.StartMenu.Path) + 'Uninstall ' + inf.Info.Desc[PackageInfo_Name] + '.lnk';

      ForceDirectories(ExtractFileDir(lnkfile));

      with TIniFile.Create(dest+PackageFile_KMPInf) do
      try
        WriteString('Files', 'uninst.vbs', 'uninst.vbs');
      finally
        Free;
      end;
      with TStringList.Create do
      try
        Add('dim kmcom, package');
        Add('Set kmcom = CreateObject("keymanapi.Keyman")');
        Add('n = kmcom.Packages.IndexOf("'+PackageName+'")');
        Add('if n >= 0 then');
        Add('  Set package = kmcom.Packages(n)');
        Add('  if msgbox("Uninstall package "+package.Name+"?", vbOKCancel, "Keyman") = vbOK then');
        Add('    package.Uninstall(True)');
        Add('  end if');
        Add('else');
        Add('  msgbox "The package '+PackageName+' could not be found."');
        Add('end if');
        SaveToFile(dest+'uninst.vbs', TEncoding.Default);  // I3337
      finally
        Free;
      end;

      if not DirectoryExists(ExtractFileDir(lnkfile)) then
        Exit(False);
//        WarnFmt(KMN_W_InstallPackage_UnableToCreateStartMenuEntry, VarArrayOf([ExtractFileName(lnkfile)]));

      if IsWow64 then  // I2176
      begin
        if GetSystemWow64Directory(buf, 260) > 0 then
          CreateLink(IncludeTrailingPathDelimiter(buf) + 'wscript.exe', '"'+dest+'uninst.vbs" /nologo', //ExtractFileDir(KMShellExe),
            lnkfile, 'Uninstall ' + inf.Info.Desc[PackageInfo_Name], '', '', 0, AllUsers); //, '', '', 0);
      end
      else
      begin
        if GetSystemDirectory(buf, 260) > 0 then
          CreateLink(IncludeTrailingPathDelimiter(buf) + 'wscript.exe', '"'+dest+'uninst.vbs" /nologo', //ExtractFileDir(KMShellExe),
            lnkfile, 'Uninstall ' + inf.Info.Desc[PackageInfo_Name], '', '', 0, AllUsers); //, '', '', 0);
      end;
    end;

    for i := 0 to inf.StartMenu.Entries.Count - 1 do
    begin
      lnkfile := IncludeTrailingPathDelimiter(programs + inf.StartMenu.Path) + inf.StartMenu.Entries[i].Name + '.lnk';
      srcfile := Trim(inf.StartMenu.Entries[i].Prog);
      iconfile := '';
      if srcfile = '' then Continue;


      params := StringReplace(inf.StartMenu.Entries[i].Params, '$keyman',
        TKeymanPaths.KeymanDesktopInstallPath(TKeymanPaths.S_KMShell), [rfIgnoreCase, rfReplaceAll]);

      // Keyman 6 compatability
      if AnsiCompareText(srcfile, '$keyman\KMSHELL.EXE') = 0 then
      begin
        srcfile := '"'+TKeymanPaths.KeymanDesktopInstallPath(TKeymanPaths.S_KMShell) + '" -c '+Uniquifier;
        iconfile := TKeymanPaths.KeymanDesktopInstallPath(TKeymanPaths.S_CfgIcon);
      end
      else if AnsiCompareText(srcfile, '$keyman\KEYMAN.EXE') = 0 then
      begin
        srcfile := '"'+TKeymanPaths.KeymanDesktopInstallPath(TKeymanPaths.S_KMShell)+'" '+Uniquifier;
        iconfile := TKeymanPaths.KeymanDesktopInstallPath(TKeymanPaths.S_AppIcon);
      end;

      // Keyman 7 tags
      if AnsiCompareText(srcfile, '(Start Product)') = 0 then
      begin
        srcfile := '"'+TKeymanPaths.KeymanDesktopInstallPath(TKeymanPaths.S_KMShell)+'" '+Uniquifier;
        iconfile := TKeymanPaths.KeymanDesktopInstallPath(TKeymanPaths.S_AppIcon);
      end
      else if AnsiCompareText(srcfile, '(Product Configuration)') = 0 then
      begin
        srcfile := '"'+TKeymanPaths.KeymanDesktopInstallPath(TKeymanPaths.S_KMShell)+'" -c '+Uniquifier;
        iconfile := TKeymanPaths.KeymanDesktopInstallPath(TKeymanPaths.S_CfgIcon);
      end
      else if AnsiCompareText(srcfile, '(Product Help)') = 0 then
      begin
        srcfile := '"'+TKeymanPaths.KeymanDesktopInstallPath(TKeymanPaths.S_KMShell) + '" -h '+Uniquifier;
        iconfile := GetHHIcon;
      end
      else if AnsiCompareText(srcfile, '(About Product)') = 0 then
      begin
        srcfile := '"'+TKeymanPaths.KeymanDesktopInstallPath(TKeymanPaths.S_KMShell) + '" -a '+Uniquifier;
        iconfile := TKeymanPaths.KeymanDesktopInstallPath(TKeymanPaths.S_CfgIcon);
      end;

      //
      // Split source file and parameters
      //

      if Copy(srcfile, 1, 1) = '"' then
      begin
        Delete(srcfile, 1, 1);
        n := Pos('"', srcfile);
        if n > 0 then
        begin
          params := Trim(Copy(srcfile, n+1, MAXINT) + ' ' + params);
          Delete(srcfile, n, MAXINT);
        end;
      end;

      srcfile := StringReplace(srcfile, '$INSTALLDIR', TKeymanPaths.KeymanDesktopInstallDir, [rfIgnoreCase, rfReplaceAll]);


      if (srcfile[1] <> '\') and (Copy(srcfile,2,1) <> ':') then
      begin
        // 6.0.153.0: fixed: add the destination directory to the link path.
        // Not a fixed path, so add the srcfile path, and the working directory details
        srcfile := dest + srcfile;
        // 6.0.153.0: fixed: add the working directory
        workingdir := dest;
      end
      else
        // 6.0.153.0: fixed: add the working directory for other programs
        workingdir := ExtractFilePath(srcfile);


      desc := inf.StartMenu.Entries[i].Name;

      ForceDirectories(ExtractFileDir(lnkfile));
      if not DirectoryExists(ExtractFileDir(lnkfile)) then
        Exit(False);
//        WarnFmt(KMN_W_InstallPackage_UnableToCreateStartMenuEntry, VarArrayOf([ExtractFileName(lnkfile)]));

      CreateLink(srcfile, params, lnkfile, desc, ExtractFileDir(workingdir), iconfile);
    end;
  end;

  Result := True;
end;

end.
