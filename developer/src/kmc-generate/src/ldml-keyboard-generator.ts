/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Generate a Keyman LDML keyboard project (.xml source)
 */

import { KeymanFileTypes } from '@keymanapp/common-types';
import { KeymanCompiler, } from '@keymanapp/developer-utils';
import { GeneratorArtifacts, GeneratorResult } from './abstract-generator.js';
import { BasicGenerator } from './basic-generator.js';

/**
 * @public
 * Generate a LDML keyboard project. The generator does not read or write from
 * filesystem or network directly, but relies on callbacks for all external IO.
 */
export class LdmlKeyboardGenerator extends BasicGenerator implements KeymanCompiler {
  protected static readonly SFile_Keyboard = 'keyboard';
  static readonly SFile_KeyboardXML = `${this.SPath_Source}${this.SFile_Keyboard}${KeymanFileTypes.Source.LdmlKeyboard}`;
  static readonly SFile_Package = `${this.SPath_Source}${this.SFile_Keyboard}${KeymanFileTypes.Source.Package}`;
  static readonly SFile_Project = `${this.SFile_Keyboard}${KeymanFileTypes.Source.Project}`;

  /**
   * Generate a LDML Keyboard project. Returns an object containing binary
   * artifacts on success. The files are passed in by name, and the compiler
   * will use callbacks as passed to the {@link AbstractGenerator.init}
   * function to read any input files by disk.
   * @returns         Binary artifacts on success, null on failure.
   */
  async run(): Promise<GeneratorResult> {
    this.preGenerate();

    const artifacts: GeneratorArtifacts = this.defaultArtifacts();
    this.templatePath = 'ldml-keyboard';
    this.filenameMap[LdmlKeyboardGenerator.SFile_Project] =
      this.options.id+KeymanFileTypes.Source.Project;
    this.filenameMap[LdmlKeyboardGenerator.SFile_KeyboardXML] =
      LdmlKeyboardGenerator.SPath_Source+this.options.id+KeymanFileTypes.Source.LdmlKeyboard;
    this.filenameMap[LdmlKeyboardGenerator.SFile_Package] =
      LdmlKeyboardGenerator.SPath_Source+this.options.id+KeymanFileTypes.Source.Package;

    this.tokenMap['$LANG_TAG'] = this.generateFirstLangTag();
    this.tokenMap['$ADDITIONAL_LANG_TAGS'] = this.generateAlternateLangTags();

    if(!this.generate(artifacts)) {
      return null;
    }

    return {artifacts};
  }

  private readonly generateFirstLangTag = () => this.languageTags?.[0] ?? this.DEFAULT_LOCALE;
  private readonly generateAlternateLangTags = () => {
    if(this.languageTags.length < 2) {
      return '';
    }

    const result =
      `  <locales>\n` +
      this.languageTags.slice(1).map(tag => `    <locale id="${tag}" />`).join('\n')+
      `\n  </locales>`;

    return result;
  }
}
