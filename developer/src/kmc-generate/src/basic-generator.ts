import { KeymanTargets } from "@keymanapp/common-types";
import KEYMAN_VERSION from "@keymanapp/keyman-version";
import { AbstractGenerator, GeneratorArtifacts } from "./abstract-generator.js";

/**
 * @internal
 */
export class BasicGenerator extends AbstractGenerator {

  // We'll generate a keyboard with a default 'en' locale if none is passed in
  protected readonly DEFAULT_LOCALE = 'en';

  protected templatePath: string;
  protected languageTags: string[];

  protected preGenerate() {
    const dt = new Date();

    this.languageTags = this.options.languageTags.length
      ? this.options.languageTags.map(tag => new Intl.Locale(tag).minimize().toString())
      : [this.DEFAULT_LOCALE];

    this.tokenMap['$NAME'] = this.options.name;
    this.tokenMap['$ID'] = this.options.id;
    this.tokenMap['$KEYMANVERSION'] = KEYMAN_VERSION.VERSION+'.0';
    this.tokenMap['$VERSION'] = this.options.version;
    this.tokenMap['$COPYRIGHT'] = '© ' + this.options.copyright;
    this.tokenMap['$FULLCOPYRIGHT'] = '© ' + dt.getFullYear().toString() + ' ' + this.options.copyright;
    this.tokenMap['$AUTHOR'] = this.options.author ?? '';
    this.tokenMap['$TARGETS'] = this.options.targets.join(' ') ?? 'any'; //TODO: validate targets
    this.tokenMap['$DESCRIPTION'] = this.options.description;
    this.tokenMap['$DATE'] =
      dt.getFullYear().toString() + '-' +
      (dt.getMonth()+1).toString().padStart(2, '0') + '-' +
      dt.getDate().toString().padStart(2, '0');
    this.tokenMap['$PACKAGE_LANGUAGES'] = this.generateLanguageListForPackage();
    this.tokenMap['$PLATFORMS_DOTLIST_README'] = this.getPlatformDotListForReadme();
  }

  protected generate(artifacts: GeneratorArtifacts): boolean {


    return this.transformAll(artifacts);
  }

  private getLanguageName(tag: string) {
    // TODO: probably need to use langtags.json
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
    const t = this.options.targets;
    const result = KeymanTargets.AllKeymanTargets
      .filter(target =>
        target != KeymanTargets.KeymanTarget.any &&
        (t.includes(target) || t.includes(KeymanTargets.KeymanTarget.any))
      )
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
}
