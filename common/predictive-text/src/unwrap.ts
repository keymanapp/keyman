/**
 * Given a function, this utility returns the source code within it, as a string.
 * This is intended to unwrap the "wrapped" source code created in the LMLayerWorker
 * build process.
 *
 * @param fn The function whose body will be returned.
 */
export default function unwrap(encodedSrc: string): string {
  // There used to be more to this, but now it's a pretty simple passthrough!
  return encodedSrc;
}