/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

export { validateMITLicense } from './utils/validate-mit-license.js';
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
  EventResolver,
  NullEventResolver,
  DelegatingCompilerCallbacks,
  ResolvingCompilerCallbacks,
} from './compiler-callbacks.js';

export { defaultCompilerOptions, CompilerBaseOptions, CompilerOptions, CompilerEvent, CompilerErrorNamespace,
  CompilerErrorSeverity,  CompilerCallbackOptions,
  CompilerError, CompilerMessageSpec, CompilerMessageObjectSpec, CompilerMessageSpecWithException, compilerErrorSeverity, CompilerErrorMask, compilerErrorSeverityName,
  compilerErrorFormatCode, CompilerMessageDef,
  compilerLogLevelToSeverity, CompilerLogLevel, compilerEventFormat, ALL_COMPILER_LOG_LEVELS,
  ALL_COMPILER_LOG_FORMATS, CompilerLogFormat,
  CompilerMessageOverride,
  CompilerMessageOverrideMap,
  dedentCompilerMessageDetail,
  KeymanCompilerArtifact,
  KeymanCompilerArtifactOptional,
  KeymanCompilerArtifacts,
  KeymanCompilerResult,
  KeymanCompiler

  } from './compiler-interfaces.js';

export { UrlSubpathCompilerCallback } from './utils/UrlSubpathCompilerCallback.js';

export { DeveloperUtilsMessages } from './developer-utils-messages.js';
export * as SourceFilenamePatterns from './source-filename-patterns.js';
export { KeymanXMLType, KeymanXMLWriter, KeymanXMLReader, KeymanXMLMetadata, XML_FILENAME_SYMBOL, withOffset } from './xml-utils.js';
export { SymbolUtils } from './symbol-utils.js';
export * as LineUtils from './line-utils.js';
export * as GitHubUrls from './github-urls.js';
export * as CloudUrls from './cloud-urls.js';

export { getFontFamily, getFontFamilySync } from './font-family.js';

export * as ValidIds from './valid-ids.js';

export * as ProjectLoader from './project-loader.js';
