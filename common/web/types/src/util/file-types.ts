
/**
 * List of all registered source file types for Keyman. Some of these file types
 * may have multiple uses (e.g. .xml) outside Keyman.
 */
export const ALL_SOURCE = [
  '.model.ts',
  '.kpj',
  '.kmn',
  '.xml',
  '.kps',
  '.kvks',
  '.keyman-touch-layout',
] as const;

type SourceFileTypeTuple = typeof ALL_SOURCE;
export type Source = SourceFileTypeTuple[number];


/**
 * List of all registered binary file types for Keyman. Some of these file types
 * may have multiple uses (e.g. .js) outside Keyman.
 */
export const ALL_BINARY = [
  // Note: .model.js is first because we need to test it before .js
  '.model.js',
  '.js',
  '.kmx',
  '.kmp',
  '.kvk',
 ] as const;

type BinaryFileTypeTuple = typeof ALL_BINARY;
export type Binary = BinaryFileTypeTuple[number];

export const ALL = [...ALL_SOURCE, ...ALL_BINARY] as const;
export type All = Source | Binary;

/**
 * Alias for '.*', any file extension, not just Keyman ones.
 */
export type Any = string;

/**
 * Gets the file type based on extension, dealing with multi-part file
 * extensions. Does not sniff contents of file or assume file existence.
 * Does transform upper-cased file extensions to lower-case.
 * @param filename
 * @returns file type, or `null` if not found
 */
export function fromFilename(filename: string): Binary | Source {
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
 * upp-ercased file extensions to lower-case.
 * @param filename
 * @param fileType
 * @returns true if file is of type fileType
 */
export function filenameIs(filename: string, fileType: Source | Binary) {
  // Special case for .model.js
  if(fileType == '.js' && filenameIs(filename, '.model.js')) {
    return false;
  }
  return filename.toLowerCase().endsWith(fileType);
}