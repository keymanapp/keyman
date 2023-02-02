/**
 * Given a function, this utility returns the source code within it, as a string.
 * This is intended to unwrap the "wrapped" source code created in the LMLayerWorker
 * build process.
 *
 * @param fn The function whose body will be returned.
 */
export default function unwrap(fnCode: string): string {
  let match = fnCode.match(/function[^{]+{((?:.|\r|\n)+)}[^}]*$/);
  return match[1];
}