
/**
 * Registered source file types for Keyman. Some of these file types (e.g. .xml)
 * may have multiple uses outside Keyman.
 */
export const enum Source {
  Model = '.model.ts',
  Project = '.kpj',
  KeymanKeyboard = '.kmn',
  LdmlKeyboard = '.xml',  // Warning, also other possible uses
  Package = '.kps',
  VisualKeyboard = '.kvks',
  TouchLayout = '.keyman-touch-layout'
};

/**
 * List of all registered source file types for Keyman. Some of these file types
 * (e.g. .xml) may have multiple uses outside Keyman.
 */
export const ALL_SOURCE: ReadonlyArray<Source> = [
  Source.Model,
  Source.Project,
  Source.KeymanKeyboard,
  Source.LdmlKeyboard,
  Source.Package,
  Source.VisualKeyboard,
  Source.TouchLayout
] as const;

/**
 * Registered binary file types for Keyman. Some of these file types (e.g. .js)
 * may have multiple uses outside Keyman.
 */
export const enum Binary {
  Model = '.model.js',
  WebKeyboard = '.js',  // Warning, also other possible uses
  Keyboard = '.kmx',
  Package = '.kmp',
  VisualKeyboard = '.kvk',
  KeyboardInfo = '.keyboard_info',
  ModelInfo = '.model_info',
}

/**
 * List of all registered binary file types for Keyman. Some of these file types
 * (e.g. .js) may have multiple uses outside Keyman.
 */
export const ALL_BINARY: ReadonlyArray<Binary> = [
  // Note: .model.js is first because we need to test it before .js
  Binary.Model,
  Binary.WebKeyboard,
  Binary.Keyboard,
  Binary.Package,
  Binary.VisualKeyboard,
  Binary.KeyboardInfo,
  Binary.ModelInfo,
 ] as const;

export const ALL = [...ALL_SOURCE, ...ALL_BINARY] as const;
export type All = Source | Binary;

/**
 * Alias for '.*', any file extension, not just Keyman ones.
 */
export type Any = string;

/**
 * Standard project file name - history of project in Markdown format
 */
export const HISTORY_MD = 'HISTORY.md';

/**
 * Standard project file name - README in Markdown format
 */
export const README_MD = 'README.md';

/**
 * Standard project file name - LICENSE in Markdown format
 */
export const LICENSE_MD = 'LICENSE.md';

/**
 * Gets the file type based on extension, dealing with multi-part file
 * extensions. Does not sniff contents of file or assume file existence. Does
 * transform upper-cased file extensions to lower-case.
 * @param filename
 * @returns file extension, or `""` if no extension. Note that this return value
 *          differs from the other, more-specific fromFilename functions below,
 *          which return `null` if a supported extension is not found.
 */
export function fromFilename(filename: string): Binary | Source | Any {
  const result =
    sourceOrBinaryTypeFromFilename(filename) ??
    filename.match(/\.[^\.]+$/)?.[0] ??
    "";
  return result;
}

/**
 * Removes the file extension, include known .model.* patterns, from a filename
 * @param filename
 * @returns
 */
export function removeExtension(filename: string): string {
  const ext = fromFilename(filename);
  return filename.substring(0, filename.length - ext.length);
}

/**
 * Gets the file type based on extension, dealing with multi-part file
 * extensions. Does not sniff contents of file or assume file existence.
 * Does transform upper-cased file extensions to lower-case.
 * @param filename
 * @returns file type, or `null` if not found
 */
export function sourceOrBinaryTypeFromFilename(filename: string): Binary | Source {
  filename = filename.toLowerCase();
  const result =
    ALL_SOURCE.find(type => filename.endsWith(type)) ??
    ALL_BINARY.find(type => filename.endsWith(type)) ??
    null;
  return result;
}

/**
 * Gets the source file type based on extension, dealing with multi-part file
 * extensions. Does not sniff contents of file or assume file existence. Does
 * transform upper-cased file extensions to lower-case.
 * @param filename
 * @returns file type, or `null` if not found
 */
export function sourceTypeFromFilename(filename: string): Source {
  filename = filename.toLowerCase();
  const result =
    ALL_SOURCE.find(type => filename.endsWith(type)) ??
    null;
  return result;
}

/**
 * Gets the binary file type based on extension, dealing with multi-part file
 * extensions. Does not sniff contents of file or assume file existence. Does
 * transform upper-cased file extensions to lower-case.
 * @param filename
 * @returns file type, or `null` if not found
 */
export function binaryTypeFromFilename(filename: string): Binary {
  filename = filename.toLowerCase();
  const result =
    ALL_BINARY.find(type => filename.endsWith(type)) ??
    null;
  return result;
}

/**
 * Returns true if filenmae has a specific file extension. Does transform
 * upper-cased file extensions to lower-case.
 * @param filename
 * @param fileType
 * @returns true if file is of type fileType
 */
export function filenameIs(filename: string, fileType: Source | Binary) {
  // Special case for .model.js
  if(fileType == Binary.WebKeyboard && filenameIs(filename, Binary.Model)) {
    return false;
  }
  return filename.toLowerCase().endsWith(fileType);
}
