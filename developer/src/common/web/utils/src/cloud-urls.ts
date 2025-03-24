/**
 * Matches a Keyman keyboard resource, based on the permanent home page for the
 * keyboard on keyman.com, `https://keyman.com/keyboards/<id>`
 */
export const KEYMANCOM_CLOUD_URI = /^(?:http(?:s)?:\/\/)?keyman\.com\/keyboards\/(?<id>[a-z0-9_.-]+)/i;

/**
 * Matches a `cloud:<id>` URI for a Keyman resource (e.g. keyboard or lexical
 * model)
 */
export const CLOUD_URI = /^cloud:(?<id>.+)$/i;


export interface CloudUriRegexMatchArray extends RegExpMatchArray {
  groups?: {
    id?: string;
  }
}