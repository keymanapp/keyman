import { KeymanTargets } from "@keymanapp/common-types";
import KEYMAN_VERSION from "@keymanapp/keyman-version";
import { AbstractGenerator } from "./abstract-generator.js";

export class BasicGenerator extends AbstractGenerator {

  protected templatePath: string;

  override async run(): Promise<boolean> {
    this.fullCopyright = '© ' + new Date().getFullYear().toString() + ' ' + this.options.copyright;
    this.options.copyright = '© ' + this.options.copyright;
    this.options.languageTags = this.options.languageTags.length
      ? this.options.languageTags.map(tag => new Intl.Locale(tag).minimize().toString())
      : ['en'];

    this.tokenMap['$NAME'] = this.options.name;
    this.tokenMap['$ID'] = this.id;
    this.tokenMap['$KEYMANVERSION'] = KEYMAN_VERSION.VERSION+'.0';
    this.tokenMap['$VERSION'] = this.options.version;
    this.tokenMap['$COPYRIGHT'] = this.options.copyright;
    this.tokenMap['$FULLCOPYRIGHT'] = this.fullCopyright;
    this.tokenMap['$AUTHOR'] = this.options.author ?? '';
    this.tokenMap['$TARGETS'] = this.options.targets.join(' ') ?? 'any'; //TODO: validate targets
    this.tokenMap['$DESCRIPTION'] = this.options.description;
    this.tokenMap['$DATE'] = new Date().toString(); //TODO: only yyyy-mm-dd
    this.tokenMap['$PACKAGE_LANGUAGES'] = this.generateLanguageListForPackage();
    this.tokenMap['$PLATFORMS_DOTLIST_README'] = this.getPlatformDotListForReadme();

    if(!await super.run()) {
      return false;
    }

    return this.transformAll();
  }

  private getLanguageName(tag: string) {
    // TODO: probably need to use langtags.json
    return new Intl.Locale(tag).language;
  }

  private generateLanguageListForPackage(): string {
    // <Languages>
    //   <Language ID="km">Central Khmer (Khmer, Cambodia)</Language>
    // </Languages>
    const langs = this.options.languageTags.length ? this.options.languageTags : ['en'];
    const result =
      `      <Languages>\n`+
      langs.map(tag => `        <Language ID="${tag}">${this.getLanguageName(tag)}</Language>`).join('\n')+
      `      </Languages>`;
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

  private readonly transformAll = () => Object.keys(this.filenameMap).every(src => this.transform(src, this.filenameMap[src]));

  private transform(sourceFile: string, destFile: string = '') {
    destFile = this.callbacks.path.join(this.options.outPath, this.id, destFile == '' ? sourceFile : destFile);
    sourceFile = this.callbacks.path.join(this.templateBasePath, this.templatePath, sourceFile);

    if(!this.callbacks.fs.existsSync(sourceFile)) {
      // internal error -- source file should exist
      throw new Error(`sourcepath ${sourceFile} does not exist`);
    }

    const sourceData = this.callbacks.loadFile(sourceFile);
    if(sourceData == null) {
      // internal error -- source file should be readable
      throw new Error(`source file ${sourceFile} could not be read`);
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

    // TODO: consider using a map
    let dest = source.join('\n');
    Object.keys(this.tokenMap).forEach(token => dest = dest.replaceAll(token, this.tokenMap[token]));

    // TODO: this needs to be pulled out of here
    this.callbacks.fs.writeFileSync(destFile, new TextEncoder().encode(dest));

    return true;
  }
}
