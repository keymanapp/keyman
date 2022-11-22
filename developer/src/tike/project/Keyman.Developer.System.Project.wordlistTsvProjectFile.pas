unit Keyman.Developer.System.Project.wordlistTsvProjectFile;

interface

uses
  System.SysUtils,
  Xml.XMLIntf,

  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.Project.ProjectFiles,
  Keyman.Developer.System.Project.ProjectFileType,
  UKeymanTargets;

type
  TwordlistTsvProjectFile = class;

  TwordlistTsvProjectFile = class(TOpenableProjectFile)
  private
  protected
    function GetRelativeOrder: Integer; override;
    procedure GetFileParameters; override;
  public
  end;

implementation

{-------------------------------------------------------------------------------
 - TwordlistTsvProjectFile                                                             -
 -------------------------------------------------------------------------------}

function TwordlistTsvProjectFile.GetRelativeOrder: Integer;
begin
  Result := 21;
end;

procedure TwordlistTsvProjectFile.GetFileParameters;
begin
  // TODO: ?
end;

initialization
  RegisterProjectFileType('.tsv', TwordlistTsvProjectFile);
end.

