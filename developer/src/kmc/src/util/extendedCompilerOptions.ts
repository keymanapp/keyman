import { CompilerCallbacks, CompilerError, CompilerErrorSeverity, CompilerMessageOverride, CompilerMessageOverrideMap, CompilerOptions } from '@keymanapp/common-types';
import { InfrastructureMessages } from '../messages/infrastructureMessages.js';
import { messageNamespaceKeys, messageNamespaces } from '../messages/messageNamespaces.js';

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
    return [];
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

function checkMessageOverride(override: CompilerMessageOverride, callbacks: CompilerCallbacks) {
  const namespace = CompilerError.namespace(override.code);
  if(!messageNamespaceKeys.includes(namespace)) {
    callbacks.reportMessage(InfrastructureMessages.Error_MessageNamespaceNotFound({code:override.code}));
    return false;
  }

  const source = messageNamespaces[namespace];
  if(!source) {
    throw new Error(`Unexpected missing namespace for code ${override.code.toString(16)}`);
  }

  // For the given namespace object, iterate through the members to find the matching value

  const keys = Object.keys(source);
  const m = source as Record<string,any>;
  const key = keys.find(key => typeof m[key] == 'number' && CompilerError.error(m[key]) === override.code);
  if(!key) {
    callbacks.reportMessage(InfrastructureMessages.Error_MessageCodeNotFound({code:override.code}));
    return false;
  }

  const validSeverityMasks = [CompilerErrorSeverity.Info,CompilerErrorSeverity.Hint,CompilerErrorSeverity.Warn];
  if(!validSeverityMasks.includes(CompilerError.severity(m[key]))) {
    callbacks.reportMessage(InfrastructureMessages.Error_MessageCannotBeCoerced({code:override.code}));
    return false;
  }

  return true;
}

export function commandOptionsToCompilerOptions(options: any, callbacks: CompilerCallbacks): ExtendedCompilerOptions {
  // We don't want to rename command line options to match the precise
  // properties that we have in CompilerOptions, but nor do we want to rename
  // CompilerOptions properties...
  const overrides = commandOptionsMessagesToCompilerOptionsMessages(options.message, callbacks);
  if(!overrides) {
    return null;
  }

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
