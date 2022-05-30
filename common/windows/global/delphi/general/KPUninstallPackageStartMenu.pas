(*
  Name:             KPUninstallPackageStartMenu
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      3 Feb 2015

  Modified Date:    3 Feb 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          03 Feb 2015 - mcdurdin - I4574 - V9.0 - If any files are read-only, they need the read-only flag removed on install
                    
*)
unit KPUninstallPackageStartMenu;

interface

uses
  kmpinffile;

type
  TKPUninstallPackageStartMenu = class
  private
    inf: TKMPInfFile;
    dest: string;
  public
    constructor Create(Ainf: TKMPInfFile; Adest: string);
    function Execute(FIsAdmin: Boolean; programs: string): Boolean;
  end;

implementation

uses
  custinterfaces,

  System.Classes,
  System.IniFiles,
  System.SysUtils,
  Winapi.ShlObj,
  Winapi.Windows,

  PackageInfo,
  utildir,
  utilsystem;

constructor TKPUninstallPackageStartMenu.Create(Ainf: TKMPInfFile;
  Adest: string);
begin
  inherited Create;
  inf := Ainf;
  dest := Adest;
end;

function TKPUninstallPackageStartMenu.Execute(FIsAdmin: Boolean; programs: string): Boolean;
var
  lnkfile: string;
  i: Integer;
begin
  if programs = '' then
  begin
    if not FIsAdmin
      then programs := GetFolderPath(CSIDL_PROGRAMS)         // Current programs for Win95/98, or not admin
      else programs := GetFolderPath(CSIDL_COMMON_PROGRAMS); // Common programs for WinNT
    programs := IncludeTrailingPathDelimiter(programs+inf.StartMenu.Path);
  end;

  if inf.StartMenu.DoCreate then
  begin
    if inf.StartMenu.AddUninstallEntry then
    begin
      lnkfile := programs+'Uninstall '+inf.Info.Desc[PackageInfo_Name]+'.lnk';
      if FileExists(lnkfile) then
      begin
        DeleteFileCleanAttr(lnkfile);   // I4574
        RemoveDir(ExtractFileDir(lnkfile));
      end;

      if FileExists(dest + 'uninst.vbs') then // I617
        DeleteFileCleanAttr(dest + 'uninst.vbs');   // I4181   // I4574
    end;

    for i := 0 to inf.StartMenu.Entries.Count - 1 do
    begin
      lnkfile := programs+inf.StartMenu.Entries[i].Name+'.lnk';
      if FileExists(lnkfile) then
      begin
        DeleteFileCleanAttr(lnkfile);   // I4574
        RemoveDir(ExtractFileDir(lnkfile));
      end;
    end;
  end;

  Result := True;
end;

end.
