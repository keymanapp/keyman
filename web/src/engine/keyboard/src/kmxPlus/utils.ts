const SINGLE_CHAR_RANGE = 256;
const ENCODED_NUM_BASE = 0;

export function decodeNumber(data: Uint8Array, start: number) {
  // All numbers in the LDML spec are 4-byte little-endian.
  const end = start + 4;
  let num = 0;

  for(let i = end - 1; i >= start; i--) {
    let val = data[i];
    num = num * SINGLE_CHAR_RANGE + val - ENCODED_NUM_BASE;
  }

  return num;
}

export function decodeSectName(data: Uint8Array, start: number) {
  return [...data.slice(start, start+4)].map((x) => String.fromCharCode(x)).join('');
}

export function decodeString(data: Uint8Array, start: number, len: number) {
  const charCodes: number[] = [];

  for(let i = 0; i < len; i++) {
    const offset = start + 2 * i;

    charCodes.push(data[offset] + data[offset+1] * 256);
  }

  return String.fromCharCode(...charCodes);
}