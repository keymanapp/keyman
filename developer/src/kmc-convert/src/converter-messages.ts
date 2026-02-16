/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Converter messages
 */
import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageSpec as m, CompilerMessageDef as def } from '@keymanapp/developer-utils';

const Namespace = CompilerErrorNamespace.Converter;
//const SevInfo = CompilerErrorSeverity.Info | Namespace;
// const SevHint = CompilerErrorSeverity.Hint | Namespace;
// const SevWarn = CompilerErrorSeverity.Warn | Namespace;
const SevError = CompilerErrorSeverity.Error | Namespace;
// const SevFatal = CompilerErrorSeverity.Fatal | Namespace;

/**
 * @internal
 */
export class ConverterMessages {
  static ERROR_OutputFilenameIsRequired = SevError | 0x0001;
  static Error_OutputFilenameIsRequired = () => m(
    this.ERROR_OutputFilenameIsRequired,
    `An output filename is required for keyboard conversion.`
  );

  static ERROR_NoConverterFound = SevError | 0x0002;
  static Error_NoConverterFound = (o: { inputFilename: string, outputFilename: string; }) => m(
    this.ERROR_NoConverterFound,
    `No converter is available that can convert from '${def(o.inputFilename)}' to '${def(o.outputFilename)}'.`
  );

  static ERROR_FileNotFound = SevError | 0x0003;
  static Error_FileNotFound =
    (o: { inputFilename: string; }) => m(
      this.ERROR_FileNotFound,
      `Input filename '${def(o.inputFilename)}' does not exist or could not be loaded.`
    );

  static ERROR_UnableToRead = SevError | 0x0004;
  static Error_UnableToRead = (o: { inputFilename: string; }) => m(
    this.ERROR_UnableToRead,
    `Input file '${def(o.inputFilename)}' could not be read.`
  );

  static ERROR_UnableToConvert = SevError | 0x0005;
  static Error_UnableToConvert = (o: { inputFilename: string; }) => m(
    this.ERROR_UnableToConvert,
    `Input file '${def(o.inputFilename)}' could not be converted.`
  );

  static ERROR_UnableToWrite = SevError | 0x0006;
  static Error_UnableToWrite = (o: { outputFilename: string; }) => m(
    this.ERROR_UnableToWrite,
    `Output file for '${def(o.outputFilename)}' could not be written.`
  );

  static ERROR_UnsupportedCharactersDetected = SevError | 0x0007;
  static Error_UnsupportedCharactersDetected = (o: { inputFilename: string, keymap_index: string,  KeyName: string, output: string; }) => m(
    this.ERROR_UnsupportedCharactersDetected,
    `Input file ${def(o.inputFilename)}
    contains unsupported character '${def(o.output)}'
    at ${def(o.keymap_index)} ${def(o.KeyName)} .`
  );

  static ERROR_InvalidFile = SevError | 0x0008;
  static Error_InvalidFile = (o: { errorText: string; }) => m(
    this.ERROR_InvalidFile,
    `The source file has an invalid structure: ${def(o.errorText)}`
  );
  
}
