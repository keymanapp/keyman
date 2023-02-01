import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerEvent } from './compiler-interfaces.js';
import { ErrorObject } from "ajv";

const m = (code: number, message: string) : CompilerEvent => { return { code, message } };

const KmcErrMask   = CompilerErrorNamespace.Types;
// const SevInfo = CompilerErrorSeverity.Info | KmcErrMask;
// const SevHint = CompilerErrorSeverity.Hint | KmcErrMask;
// const SevWarn = CompilerErrorSeverity.Warn | KmcErrMask;
// const SevError = CompilerErrorSeverity.Error | KmcErrMask;
const SevFatal = CompilerErrorSeverity.Fatal | KmcErrMask;

export class CommonTypesMessages {
  // structured Ajv validation error
  static Fatal_AJVError = (o:ErrorObject) => m(this.FATAL_XMLValidationError,
    `${o.instancePath}: ${o.keyword}: ${o.message} ${JSON.stringify(o.params||{})}`);
  // some other unknown Ajv error. Same as above, but without structure
  static Fatal_UnknownXMLValidationError = (o:{errorsText: string}) => m(this.FATAL_XMLValidationError,
    `Validation failed: ${o.errorsText}`);
  static FATAL_XMLValidationError = SevFatal | 0x0001;
};
