/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2024-10-11.
 *
 * Write a Keyman Developer Project file to a string in .kpj XML format
 */
import { KPJFile, KPJFileFile } from './kpj-file.js';
import { KeymanXMLWriter } from '../../index.js';
import { KeymanDeveloperProject, KeymanDeveloperProjectOptions, KeymanDeveloperProjectType, KeymanDeveloperProjectVersion } from './keyman-developer-project.js';

export class KPJFileWriter {
  public write(project: KeymanDeveloperProject): string {
    const defaultOptions = new KeymanDeveloperProjectOptions(project.options.version);

    const kpj: KPJFile = {
      KeymanDeveloperProject: {
        Options: {
          BuildPath: project.options.buildPath === defaultOptions.buildPath ? null : this.normalizePath(project.options.buildPath, project.options.version),
          CheckFilenameConventions: project.options.checkFilenameConventions === defaultOptions.checkFilenameConventions ? null : this.boolToString(project.options.checkFilenameConventions),
          CompilerWarningsAsErrors: project.options.compilerWarningsAsErrors === defaultOptions.compilerWarningsAsErrors ? null : this.boolToString(project.options.compilerWarningsAsErrors),
          ProjectType: project.options.projectType == KeymanDeveloperProjectType.LexicalModel ? 'lexicalmodel' : 'keyboard',
          SkipMetadataFiles: project.options.skipMetadataFiles === defaultOptions.skipMetadataFiles ? null : this.boolToString(project.options.skipMetadataFiles),
          SourcePath: project.options.sourcePath === defaultOptions.sourcePath ? null : this.normalizePath(project.options.sourcePath, project.options.version),
          Version: project.options.version == '1.0' ? null : project.options.version,
          WarnDeprecatedCode: project.options.warnDeprecatedCode === defaultOptions.warnDeprecatedCode ? null : this.boolToString(project.options.warnDeprecatedCode),
        },
      }
    };

    if(project.options.version == '1.0') {
      kpj.KeymanDeveloperProject.Files = { File: [] };
      for(const file of project.files) {
        // we skip any parented files, and any file details
        const File: KPJFileFile = {
          ID: file.filename,
          Filename: file.filename,
          Filepath: this.normalizePath(file.filePath, project.options.version),
          FileType: file.fileType,
        };
        kpj.KeymanDeveloperProject.Files.File.push(File);
      }
    }

    this.stripEmptyMembers(kpj);

    const result = new KeymanXMLWriter('kpj').write(kpj);
    return result;
  }

  private normalizePath(path: string, version: KeymanDeveloperProjectVersion) {
    return version == '1.0'
      ? path?.replaceAll(/\//g, '\\')
      : path?.replaceAll(/\\/g, '/');
  }

  private boolToString(b?: boolean) {
    if(b === null || b === undefined) {
      return null;
    }
    // .kpj expects title case True/False
    return b ? 'True' : 'False';
  }

  private stripEmptyMembers(o: any) {
    for(const p of Object.keys(o)) {
      if(typeof o[p] === 'undefined' || o[p] === null) {
        delete o[p];
      } else if(this.isEmptyObject(o[p])) {
        delete o[p];
      } else if(typeof o[p] == 'object') {
        this.stripEmptyMembers(o[p]);
      }
    }
  }

  private isEmptyObject(value: any) {
    // https://stackoverflow.com/a/32108184/1836776

    if (value == null) {
      // null or undefined
      return false;
    }

    if (typeof value !== 'object') {
      // boolean, number, string, function, etc.
      return false;
    }

    const proto = Object.getPrototypeOf(value);

    // consider `Object.create(null)`, commonly used as a safe map
    // before `Map` support, an empty object as well as `{}`
    if (proto !== null && proto !== Object.prototype) {
      return false;
    }

    return this.isEmpty(value);
  }

  private isEmpty(obj: any) {
    for (const prop in obj) {
      if (Object.hasOwn(obj, prop)) {
        return false;
      }
    }

    return true;
  }
}
