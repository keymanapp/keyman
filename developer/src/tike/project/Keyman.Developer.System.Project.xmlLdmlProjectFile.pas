{
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * xmlLdmlProjectFile: LDML keyboard files
}
unit Keyman.Developer.System.Project.xmlLdmlProjectFile;

interface

uses
  System.SysUtils,
  Xml.XMLIntf,

  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.Project.ProjectFiles,
  Keyman.Developer.System.Project.ProjectFileType,
  UKeymanTargets;

type
  TxmlLdmlProjectFile = class;

  TxmlLdmlProjectFile = class(TOpenableProjectFile)
  private
    FDebug: Boolean;
    FHeader_Name: WideString;
    FTargets: TKeymanTargets;
    FKVKFileName: string;

    function GetOutputFilename: string;
    function GetTargetFilename: string;
    function GetJSTargetFilename: string;
  protected
    function GetRelativeOrder: Integer; override;
    procedure GetFileParameters; override;

    property KVKFileName: string read FKVKFileName;
    property IsDebug: Boolean read FDebug;
  public
    class function IsFileTypeSupported(const Filename: string): Boolean; override;
    function IsCompilable: Boolean; override;
    procedure Load(node: IXMLNode); override;   // I4698
    procedure Save(node: IXMLNode); override;   // I4698
    procedure LoadState(node: IXMLNode); override;   // I4698
    procedure SaveState(node: IXMLNode); override;   // I4698

    property Debug: Boolean read FDebug write FDebug;

    property OutputFilename: string read GetOutputFilename;
    property TargetFilename: string read GetTargetFilename;
    property JSTargetFilename: string read GetJSTargetFilename;
    property Header_Name: WideString read FHeader_Name;
    property Targets: TKeymanTargets read FTargets;
  end;

implementation

uses
  System.Classes,
  System.Variants,
  Winapi.Windows,

  KeyboardParser,
  kmxfileconsts,
  KeyboardFonts,
  Keyman.System.KeyboardUtils,
  utilsystem;

{-------------------------------------------------------------------------------
 - TxmlLdmlProjectFile                                                         -
 -------------------------------------------------------------------------------}

function TxmlLdmlProjectFile.IsCompilable: Boolean;
begin
  Result := True;
end;

class function TxmlLdmlProjectFile.IsFileTypeSupported(const Filename: string): Boolean;
var
  ss: TStringStream;
begin
  try
    // Look for DTD in plain text as an adequate heuristic
    ss := TStringStream.Create('', TEncoding.UTF8);
    try
      ss.LoadFromFile(Filename);
      Result := ss.DataString.IndexOf('<keyboard3') > 0;
    finally
      ss.Free;
    end;
  except
    // If we can't read the file, e.g. it is missing, or is not in UTF-8, or
    // is locked, etc., then we'll just say it's not an XML LDML keyboard
    // file. Yes, this is a catch-all exception handler, but that should be
    // permissible for this scenario
    Exit(False);
  end;
end;

procedure TxmlLdmlProjectFile.Save(node: IXMLNode);   // I4698
begin
  inherited Save(node);   // I4698

  // We override the FileType set by TProjectFile so that we
  // can distinguish .xml keyboard files in the project view
  // from other arbitrary .xml files that may be in the project
  // FileType is _only_ used by the project view xsl files
  node.ChildValues['FileType'] := '.xml-ldml-keyboard';

  node := node.AddChild('Details');
  if FHeader_Name <> '' then node.AddChild('Name').NodeValue := FHeader_Name;
end;

procedure TxmlLdmlProjectFile.SaveState(node: IXMLNode);   // I4698
begin
  inherited SaveState(node);
  node.AddChild('Debug').NodeValue := FDebug;
end;

procedure TxmlLdmlProjectFile.Load(node: IXMLNode);   // I4698
begin
  inherited Load(node);

  if node.ChildNodes.IndexOf('Details') < 0 then Exit;
  node := node.ChildNodes['Details'];
  if node.ChildNodes.IndexOf('Name') >= 0 then FHeader_Name := VarToWideStr(node.ChildValues['Name']);
end;

procedure TxmlLdmlProjectFile.LoadState(node: IXMLNode);   // I4698
begin
  inherited LoadState(node);
  try
    if node.ChildNodes.IndexOf('Debug') >= 0 then FDebug := node.ChildValues['Debug'];
  except
    FDebug := False;
  end;
end;

function TxmlLdmlProjectFile.GetRelativeOrder: Integer;
begin
  Result := 20;
end;

function TxmlLdmlProjectFile.GetTargetFilename: string;
var
  FTempFileVersion: string;
begin
  // https://github.com/keymanapp/keyman/issues/631
  // This appears to be a Delphi compiler bug (RSP-20457)
  // Workaround is to make a copy of the parameter locally
  // which fixes the reference counting.
  FTempFileVersion := FileVersion;
  Result := OwnerProject.GetTargetFilename(OutputFileName, FileName, FTempFileVersion);
end;

procedure TxmlLdmlProjectFile.GetFileParameters;
begin
  FHeader_Name := '';
  FKVKFileName := '';
  SetFileVersion('1.0');   // I4701
  FTargets := AllKeymanTargets;

  if not FileExists(FileName) then Exit;

  // TODO: Load from XML?
end;

function TxmlLdmlProjectFile.GetJSTargetFilename: string;
begin
  if FTargets = [] then
    GetFileParameters;

  // There is no JS target if no target is specified
  if FTargets * KMWKeymanTargets = [] then
    Exit('');

  Result := OwnerProject.GetTargetFilename(TKeyboardUtils.GetKeymanWebCompiledFileName(FileName), FileName, FileVersion);
end;

function TxmlLdmlProjectFile.GetOutputFilename: string;
begin
  if FTargets = [] then
    GetFileParameters;

  // If no target is specified, we'll fall back to .kmx
  // so we always have at least one target filename
  if (FTargets <> []) and (FTargets * KMXKeymanTargets = []) then
    Exit('');
  Result := ChangeFileExt(FileName, '.kmx');
end;


initialization
  RegisterProjectFileType('.xml', TxmlLdmlProjectFile);
end.

