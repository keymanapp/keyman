import { SectionCompiler } from "./section-compiler";

export class MetaCompiler extends SectionCompiler {

  public execute() {
    this.kmx.kmxplus.meta.name = this.source.keyboard.names?.name?.[0]?.value;
    this.kmx.kmxplus.meta.author = this.source.keyboard.info?.author;
    this.kmx.kmxplus.meta.conform = this.source.keyboard.conformsTo;
    this.kmx.kmxplus.meta.layout = this.source.keyboard.info?.layout;
    this.kmx.kmxplus.meta.normalization = this.source.keyboard.info?.normalization;
    this.kmx.kmxplus.meta.indicator = this.source.keyboard.info?.indicator;
  }
}