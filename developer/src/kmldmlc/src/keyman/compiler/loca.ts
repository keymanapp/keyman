import { SectionCompiler } from "./section-compiler";

export class LocaCompiler extends SectionCompiler {

  public execute() {
    this.kmx.kmxplus.loca.locales.push(this.source.keyboard.locale);
  }
}
