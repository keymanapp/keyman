(*
  Name:             kccompileproject
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      5 May 2015

  Modified Date:    11 May 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          05 May 2015 - mcdurdin - I4699 - V9.0 - Compile .kpj files from kmcomp
                    11 May 2015 - mcdurdin - I4706 - V9.0 - Update compile logging for silent and warning-as-error cleanness
                    11 May 2015 - mcdurdin - I4709 - V9.0 - Use static hashing for id for project files to avoid unnecessary changes
*)
unit kccompileproject;   // I4699

interface

function DoKCCompileProject(AProjectFilename: string; AFullySilent, ASilent, ADebug, AClean, AWarnAsError: Boolean; ATarget: string): Boolean;   // I4706

implementation

uses
  System.SysUtils,

  kmnProjectFile,
  kpsProjectFile,
  ProjectLog,
  ProjectFile;

type
  TProjectConsole = class(TProject)
  private
    FSilent: Boolean;
    FFullySilent: Boolean;
  public
    procedure Log(AState: TProjectLogState; Filename: string; Msg: string); override;   // I4706
    function Save: Boolean; override;   // I4709

    property Silent: Boolean read FSilent write FSilent;
    property FullySilent: Boolean read FFullySilent write FFullySilent;
  end;

function DoKCCompileProject(AProjectFilename: string; AFullySilent, ASilent, ADebug, AClean, AWarnAsError: Boolean; ATarget: string): Boolean;   // I4706
var
  i: Integer;
  Found: Boolean;
  kmn: TkmnProjectFile;
  kps: TkpsProjectFile;

  function Matches(AFile: TProjectFile; AClass: TProjectFileClass): Boolean;
  begin
    Result :=
      (AFile is AClass) and
      ((ATarget = '') or
      SameText(ExtractFileName(ATarget), ExtractFileName(AFile.FileName)));
  end;

begin
  Result := False;
  Found := False;
  with TProjectConsole.Create(AProjectFilename, False) do
  try
    FullySilent := AFullySilent;
    Silent := ASilent;
    for i := 0 to Files.Count - 1 do
      if Matches(Files[i], TkmnProjectFile) then
      begin
        kmn := Files[i] as TkmnProjectFile;
        kmn.Debug := ADebug;
        kmn.WarnAsError := AWarnAsError;
        if AClean then
        begin
          if not kmn.Clean then Exit;
        end
        else
          if not kmn.CompileKeyboard then Exit;
        Found := True;
      end;

    for i := 0 to Files.Count - 1 do
      if Matches(Files[i], TkpsProjectFile) then
      begin
        kps := Files[i] as TkpsProjectFile;
        kps.WarnAsError := AWarnAsError;
        if AClean then
        begin
          if not kps.Clean then Exit;
        end
        else
          if not kps.CompilePackage(nil, False) then Exit;
        Found := True;
      end;

(*    if HasKPPFile and not AClean then
      for i := 0 to Files.Count - 1 do
        if Matches(Files[i], TkpsProjectFile) then
        begin
          kps := Files[i] as TkpsProjectFile;
          kps.WarnAsError := AWarnAsError;
          if not kps.CompilePackageInstaller(nil, False) then Exit;
          Found := True;
        end;*)
  finally
    Free;
  end;

  if not Found and not ASilent then
    writeln(ExtractFileName(AProjectFilename)+': Target not found (or project empty)');
  Result := Found;
end;

{ TProjectConsole }

procedure TProjectConsole.Log(AState: TProjectLogState; Filename, Msg: string);   // I4706
begin
  case AState of
    plsInfo:
      if not FSilent then
        writeln(ExtractFileName(Filename)+': '+Msg);
    plsWarning:
      if not FFullySilent then
        writeln(ExtractFileName(Filename)+': Warning: '+Msg);
    plsError:
      if not FFullySilent then
        writeln(ExtractFileName(Filename)+': Error: '+Msg);
    plsFatal:
      writeln(ExtractFileName(Filename)+': Fatal error: '+Msg);
  end;
end;

function TProjectConsole.Save: Boolean;   // I4709
begin
  // We don't modify the project file in the console
  Result := True;
end;

end.
