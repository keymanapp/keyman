/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

export { validateMITLicense } from './utils/validate-mit-license.js';
export { KeymanSentry, SentryNodeOptions } from './utils/KeymanSentry.js';
export { getOption, loadOptions, clearOptions } from './utils/options.js';
export { escapeMarkdownChar } from './utils/markdown.js';
export { KeymanUrls } from './utils/keyman-urls.js';

export * as KPJ from './types/kpj/kpj-file.js';
export { KPJFileReader } from './types/kpj/kpj-file-reader.js';
export { KPJFileWriter } from './types/kpj/kpj-file-writer.js';
export { KeymanDeveloperProject, KeymanDeveloperProjectFile, KeymanDeveloperProjectType, KeymanDeveloperProjectOptions } from './types/kpj/keyman-developer-project.js';
export { isValidEmail } from './is-valid-email.js';

export * as KpsFile from './types/kps/kps-file.js';
export { KpsFileReader } from './types/kps/kps-file-reader.js';
export { KpsFileWriter } from './types/kps/kps-file-writer.js';

export { default as KvksFileReader } from './types/kvks/kvks-file-reader.js';
export { default as KvksFileWriter } from './types/kvks/kvks-file-writer.js';
export * as KvksFile from './types/kvks/kvks-file.js';

export { TouchLayoutFileReader } from './types/keyman-touch-layout/keyman-touch-layout-file-reader.js';
export { TouchLayoutFileWriter, TouchLayoutFileWriterOptions } from './types/keyman-touch-layout/keyman-touch-layout-file-writer.js';

export { default as KMXBuilder } from './types/kmx/kmx-builder.js';
export { default as KMXPlusBuilder}  from './types/kmx/kmx-plus-builder/kmx-plus-builder.js';

export * as LDMLKeyboard from './types/ldml-keyboard/ldml-keyboard-xml.js';
export { LDMLKeyboardTestDataXMLSourceFile } from './types/ldml-keyboard/ldml-keyboard-testdata-xml.js';
export { LDMLKeyboardXMLSourceFileReader, LDMLKeyboardXMLSourceFileReaderOptions } from './types/ldml-keyboard/ldml-keyboard-xml-reader.js';

export {
  CompilerAsyncCallbacks,
  CompilerCallbacks,
  CompilerFileCallbacks,
  CompilerFileSystemCallbacks,
  CompilerFileSystemAsyncCallbacks,
  CompilerNetAsyncCallbacks,
  CompilerPathCallbacks,
  CompilerFileSystemCallbacksFolderEntry as FileSystemFolderEntry,
  DefaultCompilerFileSystemAsyncCallbacks,
} from './compiler-callbacks.js';

export { defaultCompilerOptions, CompilerBaseOptions, CompilerOptions, CompilerEvent, CompilerErrorNamespace,
  CompilerErrorSeverity,  CompilerCallbackOptions,
  CompilerError, CompilerMessageSpec, CompilerMessageSpecWithException, compilerErrorSeverity, CompilerErrorMask, compilerErrorSeverityName,
  compilerErrorFormatCode, CompilerMessageDef,
  compilerLogLevelToSeverity, CompilerLogLevel, compilerEventFormat, ALL_COMPILER_LOG_LEVELS,
  ALL_COMPILER_LOG_FORMATS, CompilerLogFormat,
  CompilerMessageOverride,
  CompilerMessageOverrideMap,

  KeymanCompilerArtifact,
  KeymanCompilerArtifactOptional,
  KeymanCompilerArtifacts,
  KeymanCompilerResult,
  KeymanCompiler

  } from './compiler-interfaces.js';

export { UrlSubpathCompilerCallback } from './utils/UrlSubpathCompilerCallback.js';

export { CommonTypesMessages } from './common-messages.js';
export * as SourceFilenamePatterns from './source-filename-patterns.js';
export { KeymanXMLType, KeymanXMLWriter, KeymanXMLReader } from './xml-utils.js';

export * as GitHubUrls from './github-urls.js';