/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { DOMKeyboardLoader, KeyboardHarness } from 'keyman/engine/keyboard';

export class WebviewKeyboardLoader extends DOMKeyboardLoader {
  constructor(harness: KeyboardHarness, cacheBust: boolean) {
    super(harness, cacheBust);
  }

  /**
   * Fetches a resource from the specified URL.
   *
   * @param uri
   * @returns A response promise
   *
   * @see https://stackoverflow.com/a/63582110
   *
   * Note: Using XMLHttpRequest allows us to work around the limitations of
   * Fetch API's fetch() which doesn't support file:// URLs. At least in
   * Keyman for Android we still have to explicitly allow file URLs.
   */
  protected fetch(uri: string): Promise<Response> {
    return new Promise(function (resolve, reject) {
      const httpRequest = new XMLHttpRequest();
      httpRequest.onload = function () {
        resolve(new Response(httpRequest.response, { status: httpRequest.status }));
      };
      httpRequest.onerror = (e) => {
        reject(e);
      };
      httpRequest.open('GET', uri);
      httpRequest.responseType = "arraybuffer";
      httpRequest.send(null);
    });
  };

}
