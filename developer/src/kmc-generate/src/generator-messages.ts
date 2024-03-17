import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageSpec as m } from "@keymanapp/common-types";

const Namespace = CompilerErrorNamespace.Generator;
const SevInfo = CompilerErrorSeverity.Info | Namespace;
// const SevHint = CompilerErrorSeverity.Hint | Namespace;
// const SevWarn = CompilerErrorSeverity.Warn | Namespace;
// const SevError = CompilerErrorSeverity.Error | Namespace;
const SevFatal = CompilerErrorSeverity.Fatal | Namespace;

export class GeneratorMessages {
  static Fatal_UnexpectedException = (o:{e: any}) => m(this.FATAL_UnexpectedException, null, o.e ?? 'unknown error');
  static FATAL_UnexpectedException = SevFatal | 0x0001;

  static Info_GeneratingProject = (o:{type: string, name: string}) => m(this.INFO_GeneratingProject,
    `Scanning ${o.type} file ${o.name}`);
  static INFO_GeneratingProject = SevInfo | 0x0002;
};
