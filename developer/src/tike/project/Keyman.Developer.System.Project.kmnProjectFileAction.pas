unit Keyman.Developer.System.Project.kmnProjectFileAction;

interface

uses
  System.SysUtils,

  Keyman.Developer.System.Project.kmnProjectFile,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.Project.ProjectFiles,
  Keyman.Developer.System.Project.ProjectFileType,
  UKeymanTargets;

type
  TkmnProjectFileAction = class(TkmnProjectFile)
  private
    procedure CheckFilenameConventions;
  public
    function CompileKeyboard: Boolean;
    function Clean: Boolean;
  end;

implementation

uses
  System.Classes,
  System.StrUtils,
  System.Variants,
  Winapi.Windows,

  compile,
  Keyman.Developer.System.Project.ProjectLog,
  Keyman.System.KeyboardUtils,
  Keyman.Developer.System.KmcWrapper;

function TkmnProjectFileAction.Clean: Boolean;
var
  FJS: string;
begin
  CleanFile(OutputFileName);

  if Targets * KMXKeymanTargets <> [] then
    CleanFile(ExtractFilePath(FileName) + ExtractFileName(ChangeFileExt(KVKFileName, '.kvk')), True);

  if Targets * KMWKeymanTargets <> [] then
  begin
    FJS := TKeyboardUtils.GetKeymanWebCompiledFileName(FileName);
    CleanFile(FJS); // keyboard-x.y.js
  end;

  Result := True;
end;

procedure TkmnProjectFileAction.CheckFilenameConventions;
begin
  if not OwnerProject.Options.CheckFilenameConventions then
    Exit;

  if not TKeyboardUtils.DoesKeyboardFilenameFollowConventions(FileName) then
  begin
    HasCompileWarning := True;
    Log(plsWarning, Format(TKeyboardUtils.SKeyboardNameDoesNotFollowConventions_Message, [ExtractFileName(FileName)]), CERR_WARNING, 0);
  end;

  if KVKFileName <> '' then
  begin
    if not TKeyboardUtils.DoesKeyboardFilenameFollowConventions(KVKFileName) then
    begin
      HasCompileWarning := True;
      Log(plsWarning, Format(TKeyboardUtils.SKeyboardNameDoesNotFollowConventions_Message, [ExtractFileName(KVKFileName)]), CERR_WARNING, 0);
    end;
  end;
end;

function TkmnProjectFileAction.CompileKeyboard: Boolean;
var
  w: TKmcWrapper;
begin
  w := TKmcWrapper.Create;
  try
    Result := w.Compile(Self, FileName, TargetFilename, Debug);
  finally
    w.Free;
  end;
end;

initialization
  RegisterProjectFileType('.kmn', TkmnProjectFileAction);
end.
