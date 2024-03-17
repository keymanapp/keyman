import { KeymanFileTypes } from '@keymanapp/common-types';
import { BasicGenerator } from './basic-generator.js';

/**
 * Future: we probably want to have a more abstract implementation so that we
 * can use this for both generate and clone keyboard?
 *
 * So we have a structure of an entire project passed into to a writer. Even if
 * we can't cleanly reuse at least we can copy the code more easily and it will
 * be more maintainable.
 *
 * But for now we are working with plain text approach
 */

export class LexicalModelGenerator extends BasicGenerator {
  static readonly SFile_Model = 'model';
  static readonly SFile_ModelTs = `${this.SPath_Source}${this.SFile_Model}.ts`;
  static readonly SFile_WordlistTsv = `${this.SPath_Source}wordlist.tsv`;
  static readonly SFile_Project = `${this.SFile_Model}${KeymanFileTypes.Source.Project}`;
  static readonly SFile_ModelInfo = `${this.SFile_Model}${KeymanFileTypes.Binary.ModelInfo}`;
  static readonly SFile_Package = `${this.SPath_Source}${this.SFile_Model}${KeymanFileTypes.Source.Package}`;

  override async init(id: string): Promise<boolean> {
    if(!await super.init(id)) {
      return false;
    }

    this.templatePath = 'wordlist-lexical-model';
    return true;
  }

  override async run(): Promise<boolean> {
    this.filenameMap[LexicalModelGenerator.SFile_Project] = this.id+KeymanFileTypes.Source.Project;
    this.filenameMap[LexicalModelGenerator.SFile_ModelInfo] = this.id+KeymanFileTypes.Binary.ModelInfo;
    this.filenameMap[LexicalModelGenerator.SFile_ModelTs] = LexicalModelGenerator.SPath_Source+this.id+KeymanFileTypes.Source.Model;
    this.filenameMap[LexicalModelGenerator.SFile_WordlistTsv] = LexicalModelGenerator.SFile_WordlistTsv;
    this.filenameMap[LexicalModelGenerator.SFile_Package] = LexicalModelGenerator.SPath_Source+this.id+KeymanFileTypes.Source.Package;
    this.tokenMap['$LANGUAGES_MODEL_INFO'] = this.generateLanguageTagListForModelInfo();

    if(!await super.run()) {
      return false;
    }

    return true;
  }

  private readonly generateLanguageTagListForModelInfo = (): string =>
    this.options.languageTags.map(tag => (new Intl.Locale(tag)).minimize()).join(', ');
}
