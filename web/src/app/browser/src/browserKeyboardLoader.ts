/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { DOMKeyboardLoader, KeyboardHarness } from 'keyman/engine/keyboard';

export class BrowserKeyboardLoader extends DOMKeyboardLoader {
  constructor(harness: KeyboardHarness, cacheBust: boolean) {
    super(harness, cacheBust);
  }

  protected fetch(uri: string): Promise<Response> {
    return window.fetch(uri);
  }
}
