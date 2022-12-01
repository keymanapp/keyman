/**
 * Given a function, this utility returns the source code within it, as a string.
 * This is intended to unwrap the "wrapped" source code created in the LMLayerWorker
 * build process.
 *
 * @param fn The function whose body will be returned.
 */
export default function unwrap(fn: Function): string {
  let wrapper = fn.toString();
  let match = wrapper.match(/function[^{]+{((?:.|\r|\n)+)}[^}]*$/);
  return match[1];
}