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

function DoKCCompileProject(AProjectFilename: string; ADebug, AClean, AWarnAsError, ACheckFilenameConventions: Boolean; ATarget: string): Boolean;   // I4706

implementation

uses
  System.SysUtils,

  Keyman.Developer.System.Project.kmnProjectFileAction,
  Keyman.Developer.System.Project.kpsProjectFileAction,
  Keyman.Developer.System.Project.modelTsProjectFileAction,
  Keyman.Developer.System.Project.ProjectLog,
  Keyman.Developer.System.Project.ProjectLogConsole,
  Keyman.Developer.System.Project.ProjectFile;

type
  TProjectConsole = class(TProject)
  private
    FSilent: Boolean;
    FFullySilent: Boolean;
  public
    procedure Log(AState: TProjectLogState; Filename: string; Msg: string; MsgCode, Line: Integer); override;   // I4706
    function Save: Boolean; override;   // I4709

    property Silent: Boolean read FSilent write FSilent;
    property FullySilent: Boolean read FFullySilent write FFullySilent;
  end;

function DoKCCompileProject(AProjectFilename: string; ADebug, AClean, AWarnAsError, ACheckFilenameConventions: Boolean; ATarget: string): Boolean;   // I4706
var
  i: Integer;
  Found: Boolean;
  kmn: TkmnProjectFileAction;
  kps: TkpsProjectFileAction;
  modelTs: TmodelTsProjectFileAction;

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
  with TProjectConsole.Create(ptUnknown, AProjectFilename, False) do
  try
    Options.CheckFilenameConventions := Options.CheckFilenameConventions or ACheckFilenameConventions; // never downgrade this option
    for i := 0 to Files.Count - 1 do
      if Matches(Files[i], TkmnProjectFileAction) then
      begin
        kmn := Files[i] as TkmnProjectFileAction;
        kmn.Debug := ADebug;
        kmn.WarnAsError := AWarnAsError;
        if AClean then
        begin
          if not kmn.Clean then Exit;
        end
        else
          if not kmn.CompileKeyboard then Exit;
        Found := True;
      end
      else if Matches(Files[i], TmodelTsProjectFileAction) then
      begin
        modelTs := Files[i] as TmodelTsProjectFileAction;
        modelTs.Debug := ADebug;
        modelTs.WarnAsError := AWarnAsError;
        if AClean then
        begin
          if not modelTs.Clean then Exit;
        end
        else
          if not modelTs.CompileModel then Exit;
        Found := True;
      end;


    for i := 0 to Files.Count - 1 do
      if Matches(Files[i], TkpsProjectFileAction) then
      begin
        kps := Files[i] as TkpsProjectFileAction;
        kps.WarnAsError := AWarnAsError;
        if AClean then
        begin
          if not kps.Clean then Exit;
        end
        else
          if not kps.CompilePackage then Exit;
        Found := True;
      end;

  finally
    Free;
  end;

  if not Found then
    TProjectLogConsole.Instance.Log(plsFatal, AProjectFileName, 'Target not found (or project empty)', 0, 0);
  Result := Found;
end;

{ TProjectConsole }

procedure TProjectConsole.Log(AState: TProjectLogState; Filename, Msg: string; MsgCode, Line: Integer);   // I4706
begin
  TProjectLogConsole.Instance.Log(AState, Filename, Msg, MsgCode, Line);
end;

function TProjectConsole.Save: Boolean;   // I4709
begin
  // We don't modify the project file in the console
  Result := True;
end;

end.
