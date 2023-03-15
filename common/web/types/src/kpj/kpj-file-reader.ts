import * as xml2js from 'xml2js';
import { KPJFile, KPJFileProject } from './kpj-file.js';
import { default as AjvModule } from 'ajv';
const Ajv = AjvModule.default; // The actual expected Ajv type.
import { boxXmlArray } from '../util/util.js';
import { KeymanDeveloperProject, KeymanDeveloperProjectFile10, KeymanDeveloperProjectType } from './keyman-developer-project.js';

export class KPJFileReader {
  public read(file: Uint8Array): KPJFile {
    let data: KPJFile;

    const parser = new xml2js.Parser({
      explicitArray: false,
      mergeAttrs: false,
      includeWhiteChars: false,
      normalize: false,
      emptyTag: ''
    });

    parser.parseString(file, (e: unknown, r: unknown) => { data = r as KPJFile });
    data = this.boxArrays(data);
    return data as KPJFile;
  }

  public validate(source: KPJFile, schemaBuffer: Buffer): void {
    const schema = JSON.parse(schemaBuffer.toString('utf8'));
    const ajv = new Ajv();
    if(!ajv.validate(schema, source)) {
      throw new Error(ajv.errorsText());
    }
  }

  private boolFromString(value: string, def: boolean) {
    value = (value || '').toLowerCase();
    if(value === 'true') return true;
    if(value === 'false') return false;
    return def;
  }

  public transform(projectPath: string, source: KPJFile): KeymanDeveloperProject {
    // NOTE: at this point, the xml should have been validated
    // and matched the schema result so we can assume the source
    // is a valid shape
    let project = source.KeymanDeveloperProject;
    let result: KeymanDeveloperProject = new KeymanDeveloperProject(project.Options?.Version || "1.0");
    if(result.options.version == '2.0') {
      result.options.buildPath = (project.Options?.BuildPath || result.options.buildPath).replace(/\\/g, '/');
      result.options.sourcePath = (project.Options?.SourcePath || result.options.sourcePath).replace(/\\/g, '/');
    } else {
      result.options.buildPath = (project.Options?.BuildPath || '').replace(/\\/g, '/');
    }
    result.options.checkFilenameConventions = this.boolFromString(project.Options?.CheckFilenameConventions, true);
    result.options.compilerWarningsAsErrors = this.boolFromString(project.Options?.CompilerWarningsAsErrors, false);
    result.options.warnDeprecatedCode = this.boolFromString(project.Options?.WarnDeprecatedCode, true);
    result.options.projectType =
      project.Options?.ProjectType == 'keyboard' ? KeymanDeveloperProjectType.Keyboard :
      project.Options?.ProjectType == 'lexicalmodel' ? KeymanDeveloperProjectType.LexicalModel :
      KeymanDeveloperProjectType.Keyboard; // Default is keyboard if missing

    if(result.options.version == '1.0') {
      this.transformFilesVersion10(project, result);
    } else {
      result.populateFiles(projectPath);
    }

    return result;
  }

  private transformFilesVersion10(project: KPJFileProject, result: KeymanDeveloperProject) {
    let ids: { [id: string]: KeymanDeveloperProjectFile10; } = {};
    for (let sourceFile of project.Files?.File) {
      let file: KeymanDeveloperProjectFile10 = new KeymanDeveloperProjectFile10(
        sourceFile.ID || '',
        sourceFile.Filename || '',
        (sourceFile.Filepath || '').replace(/\\/g, '/'),
        sourceFile.FileVersion || '',
        sourceFile.FileType || ''
      );
      if (sourceFile.Details) {
        file.details.copyright = sourceFile.Details.Copyright;
        file.details.name = sourceFile.Details.Name;
        file.details.message = sourceFile.Details.Message;
        file.details.version = sourceFile.Details.Version;
      }
      if (sourceFile.ParentFileID && ids[sourceFile.ParentFileID]) {
        ids[sourceFile.ParentFileID].childFiles.push(file);
      } else {
        result.files.push(file);
        ids[file.id] = file;
      }
    }
  }

  /**
   * xml2js will not place single-entry objects into arrays.
   * Easiest way to fix this is to box them ourselves as needed
   * @param source KVKSourceFile
   */
  private boxArrays(source: KPJFile) {
    boxXmlArray(source.KeymanDeveloperProject?.Files, 'File');
    return source;
  }
}