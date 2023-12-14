unit Keyman.Developer.System.ModelProjectTemplate;

interface

uses
  System.SysUtils,

  Keyman.Developer.System.ProjectTemplate,
  kpsfile,
  UKeymanTargets,
  utilfiletypes;

type
  EModelProjectTemplate = class(Exception);

  TModelProjectTemplate = class(TProjectTemplate)
  private
    procedure WriteModel;
    procedure WriteWordlist;
    procedure WriteKPS;
    procedure WriteKPJ;
    procedure WriteModelInfo;
    function GetModelFilename: string;
    function GetWordlistFilename: string;

  protected
    const
      SFileTemplate_ModelInfo = '%s.model_info'; // in root
      SFileTemplate_Package = '%s.model.kps';
      SFilename_Model = 'model.ts'; // Only for source
      SFilename_Wordlist = 'wordlist.tsv';
      SFileTemplate_Model = '%s.model.ts';
      SFileTemplate_CompiledModel = '%s.model.js';
      SDataPath_WordlistLexicalModel = 'wordlist-lexical-model\';

    function DataPath: string; override;
    function GetPackageFilename: string; override;

  public
    constructor Create(const BasePath, ModelID: string);
    procedure Generate; override;

    property ModelFilename: string read GetModelFilename;
    property WordlistFilename: string read GetWordlistFilename;
  end;

implementation

uses
  System.Classes,
  System.StrUtils,

  BCP47Tag,
  Keyman.System.LanguageCodeUtils,
  KeymanVersion,
  Keyman.Developer.System.Project.modelTsProjectFile,
  Keyman.Developer.System.Project.kpsProjectFile,
  Keyman.Developer.System.Project.wordlistTsvProjectFile,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.Project.ProjectFiles,
  PackageInfo,
  RedistFiles,
  utilstr;

{ TModelProjectTemplate }

constructor TModelProjectTemplate.Create(const BasePath, ModelID: string);
begin
  inherited Create(BasePath, ModelID, TouchKeymanTargets);
end;

procedure TModelProjectTemplate.Generate;
begin
  if not ForceDirectories(BasePath + ID + '\' + SFolder_Source) then
    raise EModelProjectTemplate.Create('Could not create destination path '+BasePath+ID);

  WriteDocumentation;

  WriteModel;
  WriteWordlist;
  WriteKPS;
  WriteKPJ;

  WriteRepositoryMetadata;

  WriteModelInfo;
end;

function TModelProjectTemplate.GetModelFilename: string;
begin
  Result := GetFilename(Format(SFileTemplate_Model, [ID]));
end;

function TModelProjectTemplate.GetWordlistFilename: string;
begin
  Result := GetFilename(SFilename_Wordlist);
end;

function TModelProjectTemplate.GetPackageFilename: string;
begin
  Result := GetFilename(Format(SFileTemplate_Package, [ID]));
end;

procedure TModelProjectTemplate.WriteModel;
begin
  Transform(SFolder_Source + '\' + SFilename_Model,
    Format(SFolder_Source + '\' + SFileTemplate_Model, [ID]));
end;

procedure TModelProjectTemplate.WriteModelInfo;
begin
  // Write modelid.model_info
  Transform(
    Format(SFileTemplate_ModelInfo, ['model']),
    Format(SFileTemplate_ModelInfo, [ID])
  );
end;

procedure TModelProjectTemplate.WriteWordlist;
begin
  Transform(SFolder_Source + '\' + SFilename_Wordlist);
end;

procedure TModelProjectTemplate.WriteKPJ;
var
  kpj: TProject;
begin
  kpj := TProject.Create(ptLexicalModel, GetProjectFilename, False);
  try
    kpj.Options.BuildPath := '$PROJECTPATH\' + SFolder_Build;
    kpj.Options.WarnDeprecatedCode := True;
    kpj.Options.CompilerWarningsAsErrors := True;
    kpj.Options.CheckFilenameConventions := True;

    // Add model and package to project
    kpj.Files.Add(TmodelTsProjectFile.Create(kpj, GetModelFilename, nil));
    kpj.Files.Add(TkpsProjectFile.Create(kpj, GetPackageFilename, nil));

    // Add metadata files to project
    kpj.Files.Add(TOpenableProjectFile.Create(kpj, BasePath + ID + '\' + SFile_HistoryMD, nil));
    kpj.Files.Add(TOpenableProjectFile.Create(kpj, BasePath + ID + '\' + SFile_LicenseMD, nil));
    kpj.Files.Add(TOpenableProjectFile.Create(kpj, BasePath + ID + '\' + SFile_ReadmeMD, nil));
    kpj.Files.Add(TOpenableProjectFile.Create(kpj, BasePath + ID + '\' + Format(SFileTemplate_ModelInfo, [ID]), nil));

    kpj.Save;
  finally
    kpj.Free;
  end;
end;

procedure TModelProjectTemplate.WriteKPS;
var
  kps: TKPSFile;
  f: TPackageContentFile;
  plm: TPackageLexicalModel;
begin
  kps := TKPSFile.Create;
  try
    // Set kps metadata
    kps.Info.Desc[PackageInfo_Name] := Name;
    kps.Info.Desc[PackageInfo_Copyright] := Copyright;
    kps.Info.Desc[PackageInfo_Author] := Author;
    kps.Info.Desc[PackageInfo_Version] := Version;
    kps.Info.Desc[PackageInfo_Description] := Description;
    kps.FileName := GetPackageFilename;

    // Add model.js
    f := TPackageContentFile.Create(kps);
    f.FileName := BasePath + ID + '\' + SFolder_Build + '\' + Format(SFileTemplate_CompiledModel, [ID]);
    kps.Files.Add(f);

    // Add welcome
    f := TPackageContentFile.Create(kps);
    f.FileName := BasePath + ID + '\' + SFolder_Source + '\' + SFile_WelcomeHTM;
    kps.Files.Add(f);

    // Add readme
    f := TPackageContentFile.Create(kps);
    f.FileName := BasePath + ID + '\' + SFolder_Source + '\' + SFile_ReadmeHTM;
    kps.Files.Add(f);
    kps.Options.ReadmeFile := f;

    // Add license
    f := TPackageContentFile.Create(kps);
    f.FileName := BasePath + ID + '\' + SFile_LicenseMD;
    kps.Files.Add(f);
    kps.Options.LicenseFile := f;

    // Add metadata about the lexical model
    plm := TPackageLexicalModel.Create(kps);
    plm.Name := Name;
    plm.ID := ID;
    kps.LexicalModels.Add(plm);

    SetPackageLanguageMetadata(kps, plm.Languages);

    kps.SaveXML;
  finally
    kps.Free;
  end;
end;

function TModelProjectTemplate.DataPath: string;
begin
  Result := inherited DataPath + SDataPath_WordlistLexicalModel;
end;

end.
