/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Converter messages
 */
import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageSpec as m, CompilerMessageDef as def } from '@keymanapp/developer-utils';

const Namespace = CompilerErrorNamespace.Converter;
// const SevInfo = CompilerErrorSeverity.Info | Namespace;
// const SevHint = CompilerErrorSeverity.Hint | Namespace;
// const SevWarn = CompilerErrorSeverity.Warn | Namespace;
const SevError = CompilerErrorSeverity.Error | Namespace;
// const SevFatal = CompilerErrorSeverity.Fatal | Namespace;

/**
 * @internal
 */
export class ConverterMessages {
  static ERROR_OutputFilenameIsRequired = SevError | 0x0001;
  static Error_OutputFilenameIsRequired = () =>
  m(this.ERROR_OutputFilenameIsRequired, `An output filename is required for keyboard conversion.`);

  static ERROR_NoConverterFound = SevError | 0x0002;
  static Error_NoConverterFound = (o:{inputFilename: string, outputFilename: string}) =>
  m(this.ERROR_NoConverterFound, `No converter is available that can convert from '${def(o.inputFilename)}' to '${def(o.outputFilename)}'.`);

  static ERROR_FileNotFound = SevError | 0x0003;
  static Error_FileNotFound = (o:{inputFilename: string}) =>
  m(this.ERROR_FileNotFound, `Input filename '${def(o.inputFilename)}' does not exist or could not be loaded.`);
}
