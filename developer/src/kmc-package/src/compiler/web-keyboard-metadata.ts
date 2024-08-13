
export interface WebKeyboardMetadata {
  keyboardName: string;
  keyboardVersion: string;
  minKeymanVersion: string;
  isRtl: boolean;
  isMnemonic: boolean;
  targets?: string;
  hasTouchLayout: boolean;
};

/**
 * This function parses the JavaScript to do a best-effort retrieval of
 * keyboard data. It assumes a format similar to what the compiler produces.
 * It may be possible for it to miss something if the code is hand written and
 * diverges enough from the compiler code (e.g. if there is a comment between
 * a member variable and its value or something crazy like that).
 *
 * Long-term, we will require package to include .kmx so this kind of parsing
 * won't be necessary.
 */
export function getCompiledWebKeyboardMetadata(js: string): WebKeyboardMetadata {
    const nameRegex =   /this.KN\s*=\s*(['"])(.*?)\1/;
    const kbverRegex =  /this.KBVER\s*=\s*(['"])(.*?)\1/;
    const minverRegex = /this.KMINVER\s*=\s*([''"])(.*?)\1/;
    const rtlRegex =    /this.KRTL\s*=\s*(.*?)\s*;/;
    const mnemonicRegex = /this.KM\s*=\s*(.*?)\s*;/;
    const touchLayoutRegex = /this.KVKL\s*=\s*{/;

    const name = nameRegex.exec(js);
    const kbver = kbverRegex.exec(js);
    const minver = minverRegex.exec(js);
    const rtl = rtlRegex.exec(js);
    const mnemonic = mnemonicRegex.exec(js);
    const touchLayout = touchLayoutRegex.exec(js);

    const SKeymanVersion70 = '7.0';

    return {
      keyboardName: name ? name[2] : null,
      keyboardVersion: kbver ? kbver[2] : null,
      minKeymanVersion: minver ? minver[2] : SKeymanVersion70,
      isRtl: !!(rtl && rtl[1].match(/^(1|true)$/)),
      isMnemonic: !!(mnemonic && mnemonic[1].match(/^(1|true)$/)),
      hasTouchLayout: !!touchLayout
    };
  }
