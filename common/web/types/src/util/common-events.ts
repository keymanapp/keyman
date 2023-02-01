import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerEvent } from './compiler-interfaces.js';
import { ErrorObject } from "ajv";
import { constants } from '@keymanapp/ldml-keyboard-constants';

const m = (code: number, message: string) : CompilerEvent => { return { code, message } };

const KmcErrMask   = CompilerErrorNamespace.Types;
// const SevInfo = CompilerErrorSeverity.Info | KmcErrMask;
// const SevHint = CompilerErrorSeverity.Hint | KmcErrMask;
// const SevWarn = CompilerErrorSeverity.Warn | KmcErrMask;
const SevError = CompilerErrorSeverity.Error | KmcErrMask;
const SevFatal = CompilerErrorSeverity.Fatal | KmcErrMask;

export class CommonTypesMessages {
  // structured Ajv validation error
  static Fatal_AJVError = (o:ErrorObject) => m(this.FATAL_XMLValidationError,
    `${o.instancePath}: ${o.keyword}: ${o.message} ${JSON.stringify(o.params||{})}`);
  // some other unknown Ajv error. Same as above, but without structure
  static Fatal_UnknownXMLValidationError = (o: { errorsText: string }) => m(this.FATAL_XMLValidationError,
    `Validation failed: ${o.errorsText}`);
  static FATAL_XMLValidationError = SevFatal | 0x0001;

  static Error_ImportInvalidBase = (o: { base: string, path: string, subtag: string }) =>
    m(this.ERROR_ImportInvalidBase,
      `Import element with base ${o.base} is unsupported. Only ${constants.cldr_import_base} is supported.`);
  static ERROR_ImportInvalidBase = SevError | 0x0001;

  static Error_ImportInvalidPath = (o: { base: string, path: string, subtag: string }) =>
    m(this.ERROR_ImportInvalidPath,
      `import element with invalid path ${o.path}: expect the form '${constants.cldr_version_latest}./*.xml`);
  static ERROR_ImportInvalidPath = SevError | 0x0002;

  static Error_ImportReadFail = (o: { base: string, path: string, subtag: string }) =>
    m(this.ERROR_ImportReadFail,
      `Import could not read data with path ${o.path}: expect the form '${constants.cldr_version_latest}./*.xml'`);
  static ERROR_ImportReadFail = SevError | 0x0003;

  static Error_ImportWrongRoot = (o: { base: string, path: string, subtag: string }) =>
    m(this.ERROR_ImportWrongRoot,
      `Invalid import file ${o.path}: expected ${o.subtag} as root element.`);
  static ERROR_ImportWrongRoot = SevError | 0x0004;

  static Error_ImportMergeFail = (o: { base: string, path: string, subtag: string, subsubtag: string }) =>
    m(this.ERROR_ImportMergeFail,
      `Problem importing ${o.path}: not sure how to handle non-array ${o.subtag}.${o.subsubtag}`);
  static ERROR_ImportMergeFail = SevError | 0x0005;
};
