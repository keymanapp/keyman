/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { CompilerErrorNamespace, CompilerErrorSeverity,  CompilerMessageDef as def, CompilerMessageSpec as m, CompilerMessageObjectSpec as mx } from './compiler-interfaces.js';
import { constants } from '@keymanapp/ldml-keyboard-constants';
import { ObjectWithCompileContext } from '@keymanapp/common-types';

const DeveloperUtilsErrMask   = CompilerErrorNamespace.DeveloperUtils;
// const SevInfo = CompilerErrorSeverity.Info   | DeveloperUtilsErrMask;
// const SevHint = CompilerErrorSeverity.Hint   | DeveloperUtilsErrMask;
// const SevWarn = CompilerErrorSeverity.Warn   | DeveloperUtilsErrMask;
const SevError = CompilerErrorSeverity.Error | DeveloperUtilsErrMask;
// const SevFatal = CompilerErrorSeverity.Fatal | DeveloperUtilsErrMask;

export class DeveloperUtilsMessages {
  // structured Ajv validation error
  static ERROR_SchemaValidationError = SevError | 0x0001;
  static Error_SchemaValidationError = (o:{instancePath:string, keyword:string, message: string, params: string}, compileContext?: ObjectWithCompileContext) => mx(
    this.ERROR_SchemaValidationError, compileContext,
    `Error validating LDML XML file: ${def(o.instancePath)}: ${def(o.keyword)}: ${def(o.message)} ${def(o.params)}`,
  );

  static ERROR_ImportInvalidBase = SevError | 0x0002;
  static Error_ImportInvalidBase = (o: { base: string, path: string, subtag: string }) =>
    m(this.ERROR_ImportInvalidBase,
      `Import element with base ${def(o.base)} is unsupported. Only ${constants.cldr_import_base} or empty (for local) are supported.`);

  static ERROR_ImportInvalidPath = SevError | 0x0003;
  static Error_ImportInvalidPath = (o: { base: string, path: string, subtag: string }) =>
    m(this.ERROR_ImportInvalidPath,
      `Import element with invalid path ${def(o.path)}: expected the form '${constants.cldr_version_latest}/*.xml'`);

  static ERROR_ImportReadFail = SevError | 0x0004;
  static Error_ImportReadFail = (o: { base: string, path: string, subtag: string }) =>
    m(this.ERROR_ImportReadFail,
      `Import could not read data with path ${def(o.path)}`,
      // for CLDR, give guidance on the suggested path
      (o.base === constants.cldr_import_base) ? `expected the form '${constants.cldr_version_latest}/*.xml' for ${o.base}` : undefined);

  static ERROR_ImportWrongRoot = SevError | 0x0005;
  static Error_ImportWrongRoot = (o: { base: string, path: string, subtag: string }) =>
    m(this.ERROR_ImportWrongRoot,
      `Invalid import file ${def(o.path)}: expected ${def(o.subtag)} as root element.`);

  static ERROR_ImportMergeFail = SevError | 0x0006;
  static Error_ImportMergeFail = (o: { base: string, path: string, subtag: string, subsubtag: string }) =>
    m(this.ERROR_ImportMergeFail,
      `Problem importing ${def(o.path)}: not sure how to handle non-array ${def(o.subtag)}.${def(o.subsubtag)}`);

  static ERROR_TestDataUnexpectedArray = SevError | 0x0007;
  static Error_TestDataUnexpectedArray = (o: {subtag: string}) =>
    m(this.ERROR_TestDataUnexpectedArray,
      `Problem reading test data: expected single ${def(o.subtag)} element, found multiple`);

  static ERROR_InvalidXml = SevError | 0x0008;
  static Error_InvalidXml = (o:{e: any}) =>
  m(this.ERROR_InvalidXml, `The XML file could not be read: ${(o.e ?? '').toString()}`);

  static ERROR_InvalidPackageFile = SevError | 0x0009;
  static Error_InvalidPackageFile = (o:{e:any}) => m(
    this.ERROR_InvalidPackageFile,
    `Package source file is invalid: ${(o.e ?? 'unknown error').toString()}`
  );

  static ERROR_InvalidProjectFile = SevError | 0x000A;
  static Error_InvalidProjectFile = (o:{message:string}) => m(
    this.ERROR_InvalidProjectFile,
    `Project file is not valid: ${def(o.message)}`,
  );

  static ERROR_UnsupportedProjectVersion = SevError | 0x000B;
  static Error_UnsupportedProjectVersion = (o:{version:string}) => m(
    this.ERROR_UnsupportedProjectVersion,
    `Project version ${def(o.version)} is not supported by this version of Keyman Developer.`,
  );

  static ERROR_ProjectFileCouldNotBeRead = SevError | 0x000C;
  static Error_ProjectFileCouldNotBeRead = () => m(
    this.ERROR_ProjectFileCouldNotBeRead,
    `Project file could not be read`
  );

  static ERROR_UnsupportedPackageFileVersion = SevError | 0x000D;
  static Error_UnsupportedPackageFileVersion = (o:{version:string}) => m(
    this.ERROR_UnsupportedPackageFileVersion,
    `Package source file is an unsupported version '${def(o.version)}'`,
  );
};
