import ttfMeta from './ttfmeta/lib/index.js';

/**
 * Extracts the font-family from an in-memory TTF or WOFF blob in `source`
 * parameter.
 *
 * @param source   In-memory TTF or WOFF font blob
 *
 * @throws Uncaught exceptions from ttfMeta.promise if the font file is invalid.
 *
 * @returns If the file is invalid or cannot be parsed, returns `null`,
 *          otherwise returns the font family as a string.
 */
export async function getFontFamily(source: Uint8Array) {
  /* c8 ignore next 3 */
  if(!source) {
    return null;
  }

  const buffer = Buffer.from(source);
  const font = await ttfMeta.promise(buffer);
  /* c8 ignore next 3 */
  if(!font) {
    return null;
  }

  return font.meta.property.find(prop => prop.name == 'font-family')?.text ?? null;
}

/**
 * Extracts the font-family from an in-memory TTF or WOFF blob in `source`
 * parameter.
 *
 * @param source   In-memory TTF or WOFF font blob
 *
 * @throws Uncaught exceptions from ttfMeta.promise if the font file is invalid.
 *
 * @returns If the file is invalid or cannot be parsed, returns `null`,
 *          otherwise returns the font family as a string.
 */
export function getFontFamilySync(source: Uint8Array) {
  /* c8 ignore next 3 */
  if(!source) {
    return null;
  }

  const buffer = Buffer.from(source);
  const font = ttfMeta.ttfInfoSync(buffer);
  /* c8 ignore next 3 */
  if(!font) {
    return null;
  }

  return font.meta.property.find(prop => prop.name == 'font-family')?.text ?? null;
}