import * as xml2js from 'xml2js';
import { KPJFile, KPJFileProject } from './kpj-file.js';
import { boxXmlArray } from '../util/util.js';
import { KeymanDeveloperProject, KeymanDeveloperProjectFile10, KeymanDeveloperProjectType } from './keyman-developer-project.js';
import { CompilerCallbacks } from '../util/compiler-interfaces.js';
import SchemaValidators from '../schema-validators.js';

export class KPJFileReader {
  constructor(private callbacks: CompilerCallbacks) {

  }

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
    for(let file of data.KeymanDeveloperProject?.Files?.File) {
      // xml2js imports <Details/> as '' so we will just delete the empty string
      if(typeof file.Details == 'string') {
        delete file.Details;
      }
    }
    return data as KPJFile;
  }

  public validate(source: KPJFile): void {
    if(!SchemaValidators.kpj(source)) {
      if(!SchemaValidators.kpj90(source)) {
        // If the legacy schema also does not validate, then we will only report
        // the errors against the modern schema
        throw new Error((<any>SchemaValidators.kpj).errors);
      }
    }
  }

  private boolFromString(value: string, def: boolean) {
    value = (value || '').toLowerCase();
    if(value === 'true') return true;
    if(value === 'false') return false;
    return def;
  }

  public transform(projectFilename: string, source: KPJFile): KeymanDeveloperProject {
    // NOTE: at this point, the xml should have been validated
    // and matched the schema result so we can assume the source
    // is a valid shape
    let project = source.KeymanDeveloperProject;
    let result: KeymanDeveloperProject = new KeymanDeveloperProject(projectFilename, project.Options?.Version || "1.0", this.callbacks);
    if(result.options.version == '2.0') {
      result.options.buildPath = (project.Options?.BuildPath || result.options.buildPath).replace(/\\/g, '/');
      result.options.sourcePath = (project.Options?.SourcePath || result.options.sourcePath).replace(/\\/g, '/');
      result.options.skipMetadataFiles = this.boolFromString(project.Options?.SkipMetadataFiles, false);
    } else {
      result.options.buildPath = (project.Options?.BuildPath || '').replace(/\\/g, '/');
      result.options.skipMetadataFiles = this.boolFromString(project.Options?.SkipMetadataFiles, true);
    }
    result.options.checkFilenameConventions = this.boolFromString(project.Options?.CheckFilenameConventions, false);
    result.options.compilerWarningsAsErrors = this.boolFromString(project.Options?.CompilerWarningsAsErrors, false);
    result.options.warnDeprecatedCode = this.boolFromString(project.Options?.WarnDeprecatedCode, true);
    result.options.projectType =
      project.Options?.ProjectType == 'keyboard' ? KeymanDeveloperProjectType.Keyboard :
      project.Options?.ProjectType == 'lexicalmodel' ? KeymanDeveloperProjectType.LexicalModel :
      KeymanDeveloperProjectType.Keyboard; // Default is keyboard if missing

    if(result.options.version == '1.0') {
      this.transformFilesVersion10(project, result);
    } else {
      result.populateFiles();
    }

    return result;
  }

  private transformFilesVersion10(project: KPJFileProject, result: KeymanDeveloperProject) {
    let ids: { [id: string]: KeymanDeveloperProjectFile10; } = {};
    for (let sourceFile of project.Files?.File) {
      let file: KeymanDeveloperProjectFile10 = new KeymanDeveloperProjectFile10(
        sourceFile.ID || '',
        (sourceFile.Filepath || '').replace(/\\/g, '/'),
        sourceFile.FileVersion || '',
        this.callbacks
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