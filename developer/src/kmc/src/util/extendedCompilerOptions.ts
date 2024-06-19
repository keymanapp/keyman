import { CompilerCallbacks, CompilerError, CompilerErrorSeverity, CompilerMessageOverride, CompilerMessageOverrideMap, CompilerOptions } from '@keymanapp/common-types';
import { InfrastructureMessages } from '../messages/infrastructureMessages.js';
import { messageNamespaceKeys, messageSources } from '../messages/messageNamespaces.js';

export interface ExtendedCompilerOptions extends CompilerOptions {
  /**
   * Verify that the project meets the requirements of the keymanapp/keyboards
   * or keymanapp/lexical-models repository, e.g. verify that project license is
   * MIT
   */
  forPublishing?: boolean;
  /**
   * Overrides for message reporting
   */
  messageOverrides?: CompilerMessageOverrideMap;
};

/**
 * converts an --message command line parameter `<id>[:level]` and verifies that
 * it is a valid message, returning `null` if it is not a valid id or level. If
 * level is omitted, the message is disabled. Valid levels are Disable, Info,
 * Hint, Warn or Error (can shorten or first character of each).
 *
 * Will report details of any errors to callbacks.reportMessage
 * @param message    command line parameter in format '["KM"]<id>[":"level]'
 * @param callbacks
 * @returns  CompilerMessageOverride map with the new severity level for the
 * message; or `null` if parameter is invalid.
 */
function commandOptionsMessageToCompilerOptionsMessage(message: string, callbacks: CompilerCallbacks): CompilerMessageOverride {
  const pattern = /^(KM)?([0-9a-f]+)(:(D|Disable|I|Info|H|Hint|W|Warn|E|Error))?$/i;
  const result = message.match(pattern);
  if(!result) {
    callbacks.reportMessage(InfrastructureMessages.Error_InvalidMessageFormat({message}));
    return null;
  }

  const code = Number.parseInt(result[2], 16);
  const inputLevel = (result[4] ?? '').toLowerCase();
  const level =
    (inputLevel == '' || 'disable'.startsWith(inputLevel)) ? 'disable' :
    CompilerError.severityNameToValue(inputLevel);

  const override: CompilerMessageOverride = { code, level };

  if(!checkMessageOverride(override, callbacks)) {
    return null;
  }

  return override;
}

function commandOptionsMessagesToCompilerOptionsMessages(messages: any, callbacks: CompilerCallbacks): CompilerMessageOverrideMap {
  if(!messages || !Array.isArray(messages)) {
    return {};
  }

  const result: CompilerMessageOverrideMap = {};
  for(let message of messages) {
    const override = commandOptionsMessageToCompilerOptionsMessage(message, callbacks);
    if(!override) {
      return null;
    }
    result[override.code] = override.level;
  }
  return result;
}

export interface CompilerMessageDetail {
  code: number;
  id: any;
  module: string;
  class: any;
}

export function findMessageDetails(code: number, callbacks: CompilerCallbacks): CompilerMessageDetail {
  const namespace = CompilerError.namespace(code);
  if(!messageNamespaceKeys.includes(namespace)) {
    callbacks.reportMessage(InfrastructureMessages.Error_MessageNamespaceNotFound({code}));
    return null;
  }

  const source = messageSources[namespace];
  if(!source) {
    throw new Error(`Unexpected missing namespace for code ${code.toString(16)}`);
  }

  // For the given namespace object, iterate through the members to find the matching value

  const keys = Object.keys(source.class);
  const m = source.class as Record<string,any>;
  const id = keys.find(key => typeof m[key] == 'number' && CompilerError.error(m[key]) === code);
  if(!id) {
    callbacks.reportMessage(InfrastructureMessages.Error_MessageCodeNotFound({code}));
    return null;
  }
  return {code: m[id], id, module: source.module, class: source.class};
}

/**
 * Verifies that a given message is valid and that the severity is allowed to be
 * modified (Info, Hint and Warn only -- Error and Fatal cannot be modified)
 * @param override
 * @param callbacks
 * @returns true if the message can be overridden
 */
function checkMessageOverride(override: CompilerMessageOverride, callbacks: CompilerCallbacks) {
  const details = findMessageDetails(override.code, callbacks);
  if(!details) {
    return false;
  }

  const validSeverityMasks = [CompilerErrorSeverity.Info,CompilerErrorSeverity.Hint,CompilerErrorSeverity.Warn];
  if(!validSeverityMasks.includes(CompilerError.severity(details.code))) {
    callbacks.reportMessage(InfrastructureMessages.Error_MessageCannotBeCoerced({code:override.code}));
    return false;
  }

  return true;
}

/**
 * Maps command line compiler options to the compiler API options.
 * @param options
 * @param callbacks
 * @returns
 */
export function commandOptionsToCompilerOptions(options: any, callbacks: CompilerCallbacks): ExtendedCompilerOptions {
  const overrides = commandOptionsMessagesToCompilerOptionsMessages(options.message, callbacks);
  if(!overrides) {
    return null;
  }

  // We don't want to rename command line options to match the precise
  // properties that we have in CompilerOptions, but nor do we want to rename
  // CompilerOptions properties...
  return {
    // CompilerBaseOptions
    logLevel: options.logLevel,
    logFormat: options.logFormat,
    color: options.color,
    // CompilerOptions
    shouldAddCompilerVersion: options.compilerVersion,
    saveDebug: options.debug,
    compilerWarningsAsErrors: options.compilerWarningsAsErrors,
    warnDeprecatedCode: options.warnDeprecatedCode,
    // ExtendedOptions
    forPublishing: options.forPublishing,
    messageOverrides: overrides,
  }
}


/**
 * these are exported only for unit tests, do not use
 */
export const unitTestEndpoints = {
  checkMessageOverride,
  commandOptionsMessageToCompilerOptionsMessage
};
