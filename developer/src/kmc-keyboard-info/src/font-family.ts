import ttfMeta from 'ttfmeta';

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