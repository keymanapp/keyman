/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Generate a Keyman lexical model project
 */

import { KeymanFileTypes } from '@keymanapp/common-types';
import { KeymanCompiler, SourceFilenamePatterns, ValidIds } from '@keymanapp/developer-utils';
import { GeneratorArtifacts, GeneratorResult } from './abstract-generator.js';
import { BasicGenerator } from './basic-generator.js';
import { GeneratorMessages } from './generator-messages.js';

/**
 * @public
 * Generate a Keyman lexical model project. The generator does not read or write
 * from filesystem or network directly, but relies on callbacks for all external
 * IO.
 */
export class LexicalModelGenerator extends BasicGenerator implements KeymanCompiler {
  static readonly SFile_Model = 'model';
  static readonly SFile_ModelTs = `${this.SPath_Source}${this.SFile_Model}.ts`;
  static readonly SFile_WordlistTsv = `${this.SPath_Source}wordlist.tsv`;
  static readonly SFile_Project = `${this.SFile_Model}${KeymanFileTypes.Source.Project}`;
  static readonly SFile_Package = `${this.SPath_Source}${this.SFile_Model}${KeymanFileTypes.Source.Package}`;

  /**
   * Generate a Lexical Model project. Returns an object containing binary
   * artifacts on success. The files are passed in by name, and the compiler
   * will use callbacks as passed to the {@link AbstractGenerator.init}
   * function to read any input files by disk.
   * @returns         Binary artifacts on success, null on failure.
   */
  async run(): Promise<GeneratorResult> {
    if(!ValidIds.isValidLexicalModelId(this.options.id)) {
      this.callbacks.reportMessage(GeneratorMessages.Error_InvalidLexicalModelId({id:this.options.id}));
      return null;
    }

    if(!this.preGenerate()) {
      // errors will have been reported in preGenerate
      return null;
    }

    const artifacts: GeneratorArtifacts = this.defaultArtifacts();

    this.templatePath = 'wordlist-lexical-model';
    this.filenameMap[LexicalModelGenerator.SFile_Project] =
      this.options.id+KeymanFileTypes.Source.Project;
    this.filenameMap[LexicalModelGenerator.SFile_ModelTs] =
      LexicalModelGenerator.SPath_Source+this.options.id+KeymanFileTypes.Source.Model;
    this.filenameMap[LexicalModelGenerator.SFile_WordlistTsv] =
      LexicalModelGenerator.SFile_WordlistTsv;

    // Note: lexical models use .model.kps for package extension
    const packageFilename = this.options.id+'.model'+KeymanFileTypes.Source.Package;
    if(!SourceFilenamePatterns.MODEL_ID_PATTERN_PACKAGE.test(packageFilename)) {
      this.callbacks.reportMessage(GeneratorMessages.Warn_ModelIdDoesNotFollowLexicalModelConventions({id: this.options.id}));
    }

    this.filenameMap[LexicalModelGenerator.SFile_Package] = LexicalModelGenerator.SPath_Source+packageFilename;

    if(!this.generate(artifacts)) {
      return null;
    }

    return {artifacts};
  }
}
