import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageSpec as m } from './compiler-interfaces.js';
import { constants } from '@keymanapp/ldml-keyboard-constants';

const CommonTypesErrMask   = CompilerErrorNamespace.CommonTypes;
// const SevInfo = CompilerErrorSeverity.Info   | CommonTypesErrMask;
// const SevHint = CompilerErrorSeverity.Hint   | CommonTypesErrMask;
// const SevWarn = CompilerErrorSeverity.Warn   | CommonTypesErrMask;
const SevError = CompilerErrorSeverity.Error | CommonTypesErrMask;
// const SevFatal = CompilerErrorSeverity.Fatal | CommonTypesErrMask;

export class CommonTypesMessages {
  // structured Ajv validation error
  static Error_SchemaValidationError = (o:{instancePath:string, keyword:string, message: string, params: string}) => m(this.ERROR_SchemaValidationError,
    `Error validating LDML XML file: ${o.instancePath}: ${o.keyword}: ${o.message} ${o.params}`);
  static ERROR_SchemaValidationError = SevError | 0x0001;

  static Error_ImportInvalidBase = (o: { base: string, path: string, subtag: string }) =>
    m(this.ERROR_ImportInvalidBase,
      `Import element with base ${o.base} is unsupported. Only ${constants.cldr_import_base} is supported.`);
  static ERROR_ImportInvalidBase = SevError | 0x0002;

  static Error_ImportInvalidPath = (o: { base: string, path: string, subtag: string }) =>
    m(this.ERROR_ImportInvalidPath,
      `Import element with invalid path ${o.path}: expected the form '${constants.cldr_version_latest}./*.xml`);
  static ERROR_ImportInvalidPath = SevError | 0x0003;

  static Error_ImportReadFail = (o: { base: string, path: string, subtag: string }) =>
    m(this.ERROR_ImportReadFail,
      `Import could not read data with path ${o.path}: expected the form '${constants.cldr_version_latest}./*.xml'`);
  static ERROR_ImportReadFail = SevError | 0x0004;

  static Error_ImportWrongRoot = (o: { base: string, path: string, subtag: string }) =>
    m(this.ERROR_ImportWrongRoot,
      `Invalid import file ${o.path}: expected ${o.subtag} as root element.`);
  static ERROR_ImportWrongRoot = SevError | 0x0005;

  static Error_ImportMergeFail = (o: { base: string, path: string, subtag: string, subsubtag: string }) =>
    m(this.ERROR_ImportMergeFail,
      `Problem importing ${o.path}: not sure how to handle non-array ${o.subtag}.${o.subsubtag}`);
  static ERROR_ImportMergeFail = SevError | 0x0006;

  static Error_TestDataUnexpectedArray = (o: {subtag: string}) =>
    m(this.ERROR_TestDataUnexpectedArray,
      `Problem reading test data: expected single ${o.subtag} element, found multiple`);
  static ERROR_TestDataUnexpectedArray = SevError | 0x0007;
  static Error_TestDataUnexpectedAction = (o: {subtag: string}) =>
    m(this.ERROR_TestDataUnexpectedAction,
      `Problem reading test data: unexpected action element ${o.subtag}`);
  static ERROR_TestDataUnexpectedAction = SevError | 0x0008;
};
