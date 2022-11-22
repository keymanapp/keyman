(*
  Name:             DevInstallPackages
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      4 May 2012

  Modified Date:    17 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 May 2012 - mcdurdin - I3307 - V9.0 - Delphi XE2 path and package manager
                    17 May 2012 - mcdurdin - I3321 - V9.0 - Fixup paths in Delphi source for v9.0
*)
unit DevInstallPackages;  // I3307

interface

type
  TInstallPackages = class
    class function InstallPackage(path: string): Boolean;
    class function Reset: Boolean;
  private
    class function GetCustomVersionString(const ExeName,
      CustomStringName: string): string; static;
  end;

implementation

uses
  DevUtils,
  SourceRootPath,
  System.Classes,
  System.SysUtils,
  System.Win.Registry,
  WinApi.ShlObj,
  WinApi.Windows;

{ TInstallPackages }

const
  SKey_Delphi = 'Software\Embarcadero\BDS\'+DelphiMajorVersion;
  SPath_DelphiCommonPackageBPLFiles = '%0:sEmbarcadero\Studio\'+DelphiMajorVersion+'\bpl\';
  SPath_DelphiCommonPackageDCPFiles = '%0:sEmbarcadero\Studio\'+DelphiMajorVersion+'\dcp\';

  SKey_DelphiKnownPackages = SKey_Delphi + '\Known Packages';
  SKey_DelphiDisabledPackages = SKey_Delphi + '\Disabled Packages';
  SKey_DelphiPackageCache = SKey_Delphi + '\Package Cache';
  SValue_RootDir = 'RootDir';

{  SRemovePackages: array[0..4] of string = (
    'dclIndyCore160.bpl',
    'dclIndyProtocols160.bpl',
    'IndyCore160.bpl',
    'IndyProtocols160.bpl',
    'IndySystem160.bpl'
    );}

type
  TLangAndCodePage = record
    wLanguage: WORD;
    wCodePage: WORD;
  end;

  PLangAndCodePage = ^TLangAndCodePage;


class function TInstallPackages.GetCustomVersionString(const ExeName, CustomStringName: string): string;
var
  n, Len: DWord;
  buf, value: PChar;
  lpTranslate: PLangAndCodePage;
  cbTranslate: DWord;
  Query: string;
begin
  Result := '';

  n := GetFileVersionInfoSize(PChar(ExeName), n); // returns size in bytes
  if n > 0 Then
  begin
    buf := AllocMem(n);
    try
      GetFileVersionInfo(PChar(ExeName),0,n,Buf) ;

      // Read the list of languages and code pages.
      if VerQueryValue(Buf, '\VarFileInfo\Translation', Pointer(lpTranslate), cbTranslate) then
      begin
        // Read the file description for each language and code page.
        if cbTranslate >= sizeof(TLangAndCodePage) then
        begin
          Query := Format('\StringFileInfo\%0.04x%0.04x\%s', [lpTranslate.wLanguage, lpTranslate.wCodePage, CustomStringName]);
          if VerQueryValue(buf, PChar(Query),
            Pointer(Value), Len) then
          begin
            Result := Trim(Value);
          end;
        end;
      end;
    finally
      FreeMem(buf);
    end;
  end;
end;

class function TInstallPackages.InstallPackage(path: string): Boolean;
var
  FDescription: string;
begin
  DevUtils.DevLog('installpackage '+path,True);
  if not FileExists(path) then
  begin
    DevUtils.DevLog('File '+path+' does not exist.',True);
    Result := False;
    Exit;
  end;

  FDescription := GetCustomVersionString(path, 'FileDescription');
  if FDescription = '' then FDescription := '(untitled)'; // This is what Delphi does

  with TRegistry.Create do
  try
    if OpenKey(SKey_DelphiKnownPackages, True) then
    begin
      WriteString(path, FDescription);
    end;
    if OpenKey('\'+SKey_DelphiDisabledPackages, True) then
      if ValueExists(path) then
        DeleteValue(path);
  finally
    Free;
  end;

  Result := True;
end;

function GetCSFolderPath(csidl: Integer): string;
var
  buf: array[0..260] of char;
  res: Integer;
begin
  res := SHGetFolderPath(0, csidl, 0, SHGFP_TYPE_CURRENT, buf);
  if res = S_OK
    then Result := IncludeTrailingPathDelimiter(buf)
    else RaiseLastOSError(res);
end;

class function TInstallPackages.Reset: Boolean;
var
  FValues: TStringList;
  I: Integer;
//  FRoot: string;
//  FFiles: string;
  f: TSearchRec;
  path: string;
begin
  DevUtils.DevLog('resetpackages',True);

  // Delete all packages from common bpl folder
  path := Format(SPath_DelphiCommonPackageBPLFiles, [GetCSFolderPath(CSIDL_COMMON_DOCUMENTS)]);
  if FindFirst(path + '*.bpl', 0, f) = 0 then
  begin
    repeat
      System.SysUtils.DeleteFile(path + f.Name);
    until FindNext(f) <> 0;
    System.SysUtils.FindClose(f);
  end;

  // Delete all packages from common bpl folder
  path := Format(SPath_DelphiCommonPackageDCPFiles, [GetCSFolderPath(CSIDL_COMMON_DOCUMENTS)]);
  if FindFirst(path + '*.dcp', 0, f) = 0 then
  begin
    repeat
      System.SysUtils.DeleteFile(path + f.Name);
    until FindNext(f) <> 0;
    System.SysUtils.FindClose(f);
  end;

  with TRegistry.Create do
  try
    // Remove all Keyman-installed packages

    if OpenKey(SKey_DelphiKnownPackages, True) then
    begin
      FValues := TStringList.Create;
      try
        GetValueNames(FValues);
        for I := 0 to FValues.Count - 1 do
        begin
          if SameText(Copy(FValues[I], 1, Length(CSourceRootLibPath)+1), CSourceRootLibPath+'\') then
            DeleteValue(FValues[I]);
        end;
      finally
        FValues.Free;
      end;
    end;

    if OpenKey('\'+SKey_DelphiDisabledPackages, True) then
    begin
      FValues := TStringList.Create;
      try
        GetValueNames(FValues);
        for I := 0 to FValues.Count - 1 do
          if SameText(Copy(FValues[I], 1, Length(CSourceRootLibPath)+1), CSourceRootLibPath+'\') then
            DeleteValue(FValues[I]);
      finally
        FValues.Free;
      end;
    end;

    // Clean the package cache

    if KeyExists('\'+SKey_DelphiPackageCache) then DeleteKey('\'+SKey_DelphiPackageCache);

    // Add C:\keyman\<ver>\lib to the user PATH environment variable

    if OpenKey('\Environment', True) then
    begin
      with TStringList.Create do
      try
        StrictDelimiter := True;
        Delimiter := ';';
        if ValueExists('PATH') then DelimitedText := ReadString('PATH');
        if IndexOf(CSourceRootLibPath) < 0 then  // I3321
        begin
          Add(CSourceRootLibPath);  // I3321
          WriteString('PATH', DelimitedText);
        end;
      finally
        Free;
      end;
    end;
  finally
    Free;
  end;

  with TStringList.Create do
  try
    StrictDelimiter := True;
    Delimiter := ';';
    DelimitedText := GetEnvironmentVariable('PATH');
    if IndexOf(CSourceRootLibPath) < 0 then  // I3321
    begin
      Add(CSourceRootLibPath);  // I3321
      SetEnvironmentVariable('PATH', PWideChar(DelimitedText));
    end;
  finally
    Free;
  end;

  (*
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(SKey_Delphi) and ValueExists(SValue_RootDir) then
    begin
      FRoot := IncludeTrailingPathDelimiter(ReadString(SValue_RootDir)) + 'bin\';
      for i := 0 to High(SRemovePackages) do
        if FileExists(FRoot + SRemovePackages[i]) then
          FFiles := FFiles + SRemovePackages[i] + #13#10;

      if FFiles <> '' then
      begin
        DevUtils.DevLog('Please delete the following files from '+FRoot+':'#13#10#13#10+FFiles+#13#10+
          'This will prevent conflicts with Keyman versions of these packages.',True);
        Result := False;
        Exit;
      end;
    end;
  finally
    Free;
  end;
  *)

  Result := True;
end;

end.
