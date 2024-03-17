import { KeymanFileTypes, KeymanTargets } from '@keymanapp/common-types';
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

export class KeymanKeyboardGenerator extends BasicGenerator {
  static readonly SFile_Keyboard = 'keyboard';
  static readonly SFile_KeyboardKMN = `${this.SPath_Source}${this.SFile_Keyboard}${KeymanFileTypes.Source.KeymanKeyboard}`;
  static readonly SFile_KeyboardKPS = `${this.SPath_Source}${this.SFile_Keyboard}${KeymanFileTypes.Source.Package}`;
  static readonly SFile_KeyboardKVKS = `${this.SPath_Source}${this.SFile_Keyboard}${KeymanFileTypes.Source.VisualKeyboard}`;
  static readonly SFile_TouchLayout = `${this.SPath_Source}${this.SFile_Keyboard}${KeymanFileTypes.Source.TouchLayout}`;
  static readonly SFile_Project = `${this.SFile_Keyboard}${KeymanFileTypes.Source.Project}`;

  override async init(id: string): Promise<boolean> {
    if(!await super.init(id)) {
      return false;
    }

    this.templatePath = 'kmn-keyboard';
    return true;
  }

  override async run(): Promise<boolean> {
    this.filenameMap[KeymanKeyboardGenerator.SFile_Project] = this.id+KeymanFileTypes.Source.Project;

    if(this.hasKVKS()) {
      this.includedPrefixes.push('KVKS');
      this.filenameMap[KeymanKeyboardGenerator.SFile_KeyboardKVKS] = KeymanKeyboardGenerator.SPath_Source+this.id+KeymanFileTypes.Source.VisualKeyboard;
    }

    if(this.hasWeb()) {
      this.includedPrefixes.push('Web');
    }

    if(this.hasTouchLayout()) {
      this.includedPrefixes.push('TouchLayout');
      this.filenameMap[KeymanKeyboardGenerator.SFile_TouchLayout] = KeymanKeyboardGenerator.SPath_Source+this.id+KeymanFileTypes.Source.TouchLayout;
    }

    if(this.hasIcon()) {
      this.includedPrefixes.push('Icon');
    }

    if(this.hasKMX()) {
      this.includedPrefixes.push('KMX');
    }

    this.filenameMap[KeymanKeyboardGenerator.SFile_KeyboardKMN] = KeymanKeyboardGenerator.SPath_Source+this.id+KeymanFileTypes.Source.KeymanKeyboard;
    this.filenameMap[KeymanKeyboardGenerator.SFile_KeyboardKPS] = KeymanKeyboardGenerator.SPath_Source+this.id+KeymanFileTypes.Source.Package;

    if(!await super.run()) {
      return false;
    }

    // Special case for creating icon, run after successful creation of other
    // project bits and pieces
    if(this.hasIcon()) {
      this.writeIcon();
    }

    return true;
  }

  private readonly targetIncludes = (targets: KeymanTargets.KeymanTarget[]) => {
    const tx: string[] = [...targets, KeymanTargets.KeymanTarget.any];
    return this.options.targets.some(t => tx.includes(t));
  }

  private readonly hasKVKS        = () => this.targetIncludes(KeymanTargets.KeymanTargetsUsingKVK);
  private readonly hasWeb         = () => this.targetIncludes(KeymanTargets.KMWKeymanTargets);
  private readonly hasKMX         = () => this.targetIncludes(KeymanTargets.KMXKeymanTargets);
  private readonly hasTouchLayout = () => this.targetIncludes(KeymanTargets.TouchKeymanTargets);

  // TODO, once writeIcon is implemented:
  // hasIcon = () => this.options.icon && this.targetIncludes(KeymanTargets.KMXKeymanTargets);
  private readonly hasIcon = () => false;

  private writeIcon() {
    // TODO: this will require some effort
    // proposal: generate 16x16 icon with 2-3 letters. Following TKeyboardIconGenerator.GenerateIcon
  }
}
