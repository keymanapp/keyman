/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Basic generator -- common file generation functionality
 */

import { KeymanTargets } from "@keymanapp/common-types";
import KEYMAN_VERSION from "@keymanapp/keyman-version";
import { AbstractGenerator, GeneratorArtifacts } from "./abstract-generator.js";
import { GeneratorMessages } from "./generator-messages.js";

/**
 * @internal
 * Common functionality for generating projects. Do not instantiate
 * this class, rather instantiate a subclass
 */
export class BasicGenerator extends AbstractGenerator {

  // We'll generate a keyboard with a default 'en' locale if none is passed in
  protected readonly DEFAULT_LOCALE = 'en';

  protected templatePath: string;
  protected languageTags: string[];
  protected resolvedTargets: KeymanTargets.KeymanTarget[];

  protected preGenerate() {
    const dt = new Date();

    this.languageTags = this.options.languageTags.length
      ? this.options.languageTags.map(tag => new Intl.Locale(tag).minimize().toString())
      : [this.DEFAULT_LOCALE];

    // Verify that targets passed in are valid
    let failed = false;
    for(const target of this.options.targets) {
      if(!KeymanTargets.AllKeymanTargets.includes(target)) {
        this.callbacks.reportMessage(GeneratorMessages.Error_InvalidTarget({target}));
        failed = true;
      }
    }
    if(failed) {
      return false;
    }

    if(this.options.targets.includes(KeymanTargets.KeymanTarget.any) || this.options.targets.length == 0) {
      this.resolvedTargets = KeymanTargets.AllKeymanTargets.filter(target => target != KeymanTargets.KeymanTarget.any);
    } else {
      this.resolvedTargets = [].concat(this.options.targets);
    }

    this.tokenMap['$NAME'] = this.options.name;
    this.tokenMap['$ID'] = this.options.id;
    this.tokenMap['$KEYMANVERSION'] = (this.options.keymanVersion || KEYMAN_VERSION.VERSION) + '.0';
    this.tokenMap['$VERSION'] = this.options.version;
    this.tokenMap['$COPYRIGHT'] = '© ' + this.options.copyright;
    this.tokenMap['$FULLCOPYRIGHT'] = '© ' + dt.getFullYear().toString() + ' ' + this.options.copyright;
    this.tokenMap['$AUTHOR'] = this.options.author || '';
    this.tokenMap['$TARGETS'] = this.resolvedTargets.join(' ');
    this.tokenMap['$DESCRIPTION'] = this.options.description || this.options.name;
    this.tokenMap['$DATE'] =
      dt.getFullYear().toString() + '-' +
      (dt.getMonth()+1).toString().padStart(2, '0') + '-' +
      dt.getDate().toString().padStart(2, '0');
    this.tokenMap['$PACKAGE_LANGUAGES'] = this.generateLanguageListForPackage();
    this.tokenMap['$PLATFORMS_DOTLIST_README'] = this.getPlatformDotListForReadme();

    return true;
  }

  protected generate(artifacts: GeneratorArtifacts): boolean {
    return this.transformAll(artifacts);
  }

  private getLanguageName(tag: string) {
    // TODO-GENERATE: probably need to use langtags.json
    return new Intl.Locale(tag).language;
  }

  private generateLanguageListForPackage(): string {
    // <Languages>
    //   <Language ID="km">Central Khmer (Khmer, Cambodia)</Language>
    // </Languages>
    const result =
      `      <Languages>\n`+
      this.languageTags.map(tag => `        <Language ID="${tag}">${this.getLanguageName(tag)}</Language>`).join('\n')+
      `\n      </Languages>`;
    return result;
  }

  private getPlatformDotListForReadme() {
    const result = KeymanTargets.AllKeymanTargets
      .filter(target => this.resolvedTargets.includes(target))
      .map(target => ` * ${KeymanTargets.SKeymanTargetNames[target]}`)
      .join('\n');
    return result;
  }

  private readonly transformAll = (artifacts: GeneratorArtifacts) => Object
    .keys(this.filenameMap)
    .every(src => this.transform(src, this.filenameMap[src], artifacts));

  private transform(sourceFile: string, destFile: string, artifacts: GeneratorArtifacts) {
    destFile = this.callbacks.path.join(this.options.outPath, this.options.id, destFile == '' ? sourceFile : destFile);
    sourceFile = this.callbacks.path.join(this.templateBasePath, this.templatePath, sourceFile);

    const sourceData = this.callbacks.loadFile(sourceFile);

    /* c8 ignore next 4 */
    if(sourceData == null) {
      // internal error -- source file should be readable
      throw new Error(`source file ${sourceFile} does not exist`);
    }
    const template = new TextDecoder('utf-8').decode(sourceData).replace(/\r/g, '').split('\n');

    const source: string[] = [];

    // Filter out unused lines

    for(const line of template) {
      if(!line.match(/\$[A-Z0-9_-]+:/i)) {
        source.push(line);
      } else {
        const tokens = line.split(':', 2);
        if(this.includedPrefixes.includes(tokens[0].substring(1))) {
          source.push(tokens[1]);
        }
      }
    }

    // Replace all tokens
    let dest = source.join('\n');
    Object.keys(this.tokenMap).forEach(token => dest = dest.replaceAll(token, this.tokenMap[token]));

    artifacts[destFile] = {
      filename: destFile,
      data: new TextEncoder().encode(dest)
    };

    return true;
  }

  /**
   * @internal
   * these are exported only for unit tests, do not use
   */
  public readonly test_templatePath = () => this.templatePath;
  public readonly test_languageTags = () => this.languageTags;

  public readonly test_preGenerate = () => this.preGenerate();
  public readonly test_generate = (artifacts: GeneratorArtifacts) => this.generate(artifacts);
  public readonly test_getLanguageName = (tag: string)  => this.getLanguageName(tag);
  public readonly test_generateLanguageListForPackage = () => this.generateLanguageListForPackage();
  public readonly test_getPlatformDotListForReadme = () => this.getPlatformDotListForReadme();
  public readonly test_transformAll = (artifacts: GeneratorArtifacts) => this.transformAll(artifacts);
  public readonly test_transform = (sourceFile: string, destFile: string, artifacts: GeneratorArtifacts) =>
    this.transform(sourceFile, destFile, artifacts);
}
