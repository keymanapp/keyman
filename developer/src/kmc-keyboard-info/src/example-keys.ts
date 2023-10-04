import { KeyboardInfoFileExampleKey } from "./keyboard-info-file.js";

/**
 * Converts a .kps or .kmp example keys string into an array of key objects
 * matching the .keyboard_info example file format
 * @param keysString
 * @returns
 */
export function packageKeysExamplesToKeyboardInfo(keysString: string): KeyboardInfoFileExampleKey[] {
  const items = keysString.trim().split(/ +/);
  const result: KeyboardInfoFileExampleKey[] = [];
  for(const item of items) {
    const keyAndModifiers = item.split('+');
    if(keyAndModifiers.length > 0) {
      const key: KeyboardInfoFileExampleKey = {key: keyAndModifiers.pop()}
      if(keyAndModifiers.length) {
        key.modifiers = [...keyAndModifiers];
      };
      result.push(key);
    }
  }
  return result;
}