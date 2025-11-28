/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { Page } from '@playwright/test';

function getKeycodeFromVk(vk: string): string {
  const virtualKeyToKeyCodeMap = new Map([
    ['K_BKSP', 'Backspace'],
    ['K_TAB', 'Tab'],
    ['K_ENTER', 'Enter'],
    ['K_SHIFT', 'Shift'],
    ['SHIFT', 'Shift'],
    ['K_CONTROL', 'Control'],
    ['CTRL', 'Control'],
    ['K_ALT', 'Alt'],
    ['K_PAUSE', 'Pause'],
    ['K_CAPS', 'CapsLock'],
    ['CAPSLOCK', 'CapsLock'],
    ['K_ESC', 'Escape'],
    ['K_SPACE', 'Space'],
    ['K_PGUP', 'PageUp'],
    ['K_PGDN', 'PageDown'],
    ['K_END', 'End'],
    ['K_HOME', 'Home'],
    ['K_LEFT', 'ArrowLeft'],
    ['K_UP', 'ArrowUp'],
    ['K_RIGHT', 'ArrowRight'],
    ['K_DOWN', 'ArrowDown'],
    ['K_SEL', 'Select'],
    ['K_PRINT', 'PrintScreen'],
    ['K_EXEC', 'Execute'],
    ['K_INS', 'Insert'],
    ['K_DEL', 'Delete'],
    ['K_HELP', 'Help'],
    ['K_0', 'Digit0'],
    ['K_1', 'Digit1'],
    ['K_2', 'Digit2'],
    ['K_3', 'Digit3'],
    ['K_4', 'Digit4'],
    ['K_5', 'Digit5'],
    ['K_6', 'Digit6'],
    ['K_7', 'Digit7'],
    ['K_8', 'Digit8'],
    ['K_9', 'Digit9'],
    ['K_A', 'KeyA'],
    ['K_B', 'KeyB'],
    ['K_C', 'KeyC'],
    ['K_D', 'KeyD'],
    ['K_E', 'KeyE'],
    ['K_F', 'KeyF'],
    ['K_G', 'KeyG'],
    ['K_H', 'KeyH'],
    ['K_I', 'KeyI'],
    ['K_J', 'KeyJ'],
    ['K_K', 'KeyK'],
    ['K_L', 'KeyL'],
    ['K_M', 'KeyM'],
    ['K_N', 'KeyN'],
    ['K_O', 'KeyO'],
    ['K_P', 'KeyP'],
    ['K_Q', 'KeyQ'],
    ['K_R', 'KeyR'],
    ['K_S', 'KeyS'],
    ['K_T', 'KeyT'],
    ['K_U', 'KeyU'],
    ['K_V', 'KeyV'],
    ['K_W', 'KeyW'],
    ['K_X', 'KeyX'],
    ['K_Y', 'KeyY'],
    ['K_Z', 'KeyZ'],
    ['K_NP0', 'Numpad0'],
    ['K_NP1', 'Numpad1'],
    ['K_NP2', 'Numpad2'],
    ['K_NP3', 'Numpad3'],
    ['K_NP4', 'Numpad4'],
    ['K_NP5', 'Numpad5'],
    ['K_NP6', 'Numpad6'],
    ['K_NP7', 'Numpad7'],
    ['K_NP8', 'Numpad8'],
    ['K_NP9', 'Numpad9'],
    ['K_NPSTAR', 'NumpadMultiply'],
    ['K_NPPLUS', 'NumpadAdd'],
    ['K_SEPARATOR', 'NumpadComma'],
    ['K_NPMINUS', 'NumpadSubtract'],
    ['K_NPDOT', 'NumpadDecimal'],
    ['K_NPSLASH', 'NumpadDivide'],
    ['K_F1', 'F1'],
    ['K_F2', 'F2'],
    ['K_F3', 'F3'],
    ['K_F4', 'F4'],
    ['K_F5', 'F5'],
    ['K_F6', 'F6'],
    ['K_F7', 'F7'],
    ['K_F8', 'F8'],
    ['K_F9', 'F9'],
    ['K_F10', 'F10'],
    ['K_F11', 'F11'],
    ['K_F12', 'F12'],
    ['K_NUMLOCK', 'NumLock'],
    ['K_SCROLL', 'Scroll'],
    ['K_LSHIFT', 'ShiftLeft'],
    ['K_RSHIFT', 'ShiftRight'],
    ['LCTRL', 'ControlLeft'],
    ['K_LCONTROL', 'ControlLeft'],
    ['RCTRL', 'ControlRight'],
    ['K_RCONTROL', 'ControlRight'],
    ['LALT', 'Alt'],
    ['K_LALT', 'Alt'],
    ['RALT', 'AltGraph'],
    ['K_RALT', 'AltGraph'],
    ['K_COLON', 'Semicolon'],
    ['K_EQUAL', 'Equal'],
    ['K_COMMA', 'Comma'],
    ['K_HYPHEN', 'Minus'],
    ['K_PERIOD', 'Period'],
    ['K_SLASH', 'Slash'],
    ['K_BKQUOTE', 'Backquote'],
    ['K_LBRKT', 'BracketLeft'],
    ['K_BKSLASH', 'Backslash'],
    ['K_RBRKT', 'BracketRight'],
    ['K_QUOTE', 'Quote'],
    ['K_oE2', 'IntlBackslash'],
    ['K_OE2', 'IntlBackslash'],
    // ['K_oC1', ''],
    // ['K_OC1', ''],
    // ['K_?C1', ''],
    // ['k_?C1', ''],
    // ['K_oDF', ''],
    // ['K_ODF', ''],
    // ['K_LOPT', ''],
    // ['K_ROPT', ''],
    // ['K_NUMERALS', ''],
    // ['K_SYMBOLS', ''],
    // ['K_CURRENCIES', ''],
    // ['K_UPPER', ''],
    // ['K_LOWER', ''],
    // ['K_ALPHA', ''],
    // ['K_SHIFTED', ''],
    ['K_ALTGR', 'AltGraph'],
    // ['K_TABBACK', ''],
    // ['K_TABFWD', ''],
  ]);
  if (virtualKeyToKeyCodeMap.has(vk)) {
    return virtualKeyToKeyCodeMap.get(vk);
  } else {
    throw new Error(`Unknown virtual key: ${vk}`);
  }
}

function getKeySequences(keys: string): string[][] {
  // e.g. [K_A][SHIFT K_B][K_PERIOD][RALT K_C][LCTRL K_D]
  const keyEvents = keys.match(/\[([^\]]+)\]/g);
  if (!keyEvents) {
    return [];
  }

  const result: string[][]= [];

  for (const keyEvent of keyEvents) {
    const parts = keyEvent.slice(1, -1).trim().split(' ');

    const eventKeys: string[] = [];
    for (const vk of parts) {
      eventKeys.push(getKeycodeFromVk(vk));
    }
    result.push(eventKeys);
  }
  return result;
}

export async function pressKeys(page: Page, keys: string): Promise<void> {
  const keySequences = getKeySequences(keys);
  for (const keySequence of keySequences) {
    for (let i = 0; i < keySequence.length - 1; i++) {
      await page.keyboard.down(keySequence[i]);
    }
    await page.keyboard.down(keySequence[keySequence.length - 1]);

    await page.keyboard.up(keySequence[keySequence.length - 1]);
    for (let i = keySequence.length - 2; i >= 0; i--) {
      await page.keyboard.up(keySequence[i]);
    }
  }
}