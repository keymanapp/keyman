import { KeymanFileTypes } from '@keymanapp/common-types';
import { BasicGenerator } from './basic-generator.js';

export class LdmlKeyboardGenerator extends BasicGenerator {
  protected static readonly SFile_Keyboard = 'keyboard';
  static readonly SFile_KeyboardXML = `${this.SPath_Source}${this.SFile_Keyboard}${KeymanFileTypes.Source.LdmlKeyboard}`;
  static readonly SFile_Package = `${this.SPath_Source}${this.SFile_Keyboard}${KeymanFileTypes.Source.Package}`;
  static readonly SFile_Project = `${this.SFile_Keyboard}${KeymanFileTypes.Source.Project}`;

  override async init(id: string): Promise<boolean> {
    if(!await super.init(id)) {
      return false;
    }

    this.templatePath = 'ldml-keyboard';
    return true;
  }

  override async run(): Promise<boolean> {
    this.filenameMap[LdmlKeyboardGenerator.SFile_Project] = this.id+KeymanFileTypes.Source.Project;
    this.filenameMap[LdmlKeyboardGenerator.SFile_KeyboardXML] = LdmlKeyboardGenerator.SPath_Source+this.id+KeymanFileTypes.Source.LdmlKeyboard;
    this.filenameMap[LdmlKeyboardGenerator.SFile_Package] = LdmlKeyboardGenerator.SPath_Source+this.id+KeymanFileTypes.Source.Package;

    this.tokenMap['$LANG_TAG'] = this.generateFirstLangTag();
    this.tokenMap['$ADDITIONAL_LANG_TAGS'] = this.generateAlternateLangTags();

    if(!await super.run()) {
      return false;
    }

    return true;
  }

  private readonly generateFirstLangTag = () => '???'; //TODO
  private readonly generateAlternateLangTags = () => '???'; //TODO
}
