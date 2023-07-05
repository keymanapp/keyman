//
// Visual Keyboard in-memory data
//
// Corresponds to .kvk / .kvks file data
//

import { BUILDER_KVK_SHIFT_STATE, BUILDER_KVK_HEADER_FLAGS, BUILDER_KVK_KEY_FLAGS } from "./kvk-file.js";

/**
 * The default font and size are not very important, as generally they are
 * unused, because font information is provided in the package, but we include
 * them for completeness. The size is included as a negative value which is
 * defined in the LOGFONT structure in Windows API.
 */
export const DEFAULT_KVK_FONT = {
  name: "Arial",
  size: -12
};

export class VisualKeyboard {
  header: VisualKeyboardHeader = {flags: 0, ansiFont:{}, unicodeFont:{}, underlyingLayout: undefined};
  keys: VisualKeyboardKey[] = [];
};

export { BUILDER_KVK_HEADER_FLAGS as VisualKeyboardHeaderFlags } from "./kvk-file.js";

export class VisualKeyboardHeader {
  version?: number;                      // 0x0600
  flags: BUILDER_KVK_HEADER_FLAGS;
  associatedKeyboard?: string;
  ansiFont: VisualKeyboardFont;          // generally unused
  unicodeFont: VisualKeyboardFont;
  underlyingLayout?: string;
};

export class VisualKeyboardFont {
  name?: string;
  size?: number;
};

export { BUILDER_KVK_KEY_FLAGS as VisualKeyboardKeyFlags } from "./kvk-file.js";

export { BUILDER_KVK_SHIFT_STATE as VisualKeyboardShiftState } from "./kvk-file.js";

export class VisualKeyboardKey {
  flags?: BUILDER_KVK_KEY_FLAGS;
  shift?: BUILDER_KVK_SHIFT_STATE;
  vkey?: number;
  text?: string;
  bitmap?: Uint8Array;  // .bmp, unsupported in ldml keyboards; included for round-tripabilty
};

interface VisualKeyboardLegalShiftState {
  desc: string;
  name: string;
  shift: number;
  vkeys: number[];
};

import { BUILDER_KVK_SHIFT_STATE as ss } from "./kvk-file.js";
import { USVirtualKeyCodes as vk } from "../consts/virtual-key-constants.js";

export const VisualKeyboardLegalShiftStates: VisualKeyboardLegalShiftState[] = [
  {desc: 'Unshifted',                  name: '',      shift: ss.KVKS_NORMAL, vkeys: []},    //1

  {desc: 'Shift',                      name: 'S',     shift: ss.KVKS_SHIFT, vkeys: [vk.K_SHIFT]},
  {desc: 'Ctrl',                       name: 'C',     shift: ss.KVKS_CTRL, vkeys: [vk.K_CONTROL]},
  {desc: 'Alt',                        name: 'A',     shift: ss.KVKS_ALT, vkeys: [vk.K_ALT]},
  {desc: 'Shift+Ctrl',                 name: 'SC',    shift: ss.KVKS_SHIFT | ss.KVKS_CTRL, vkeys: [vk.K_SHIFT,vk.K_CONTROL]},
  {desc: 'Shift+Alt',                  name: 'SA',    shift: ss.KVKS_SHIFT | ss.KVKS_ALT, vkeys: [vk.K_SHIFT,vk.K_ALT]},
  {desc: 'Ctrl+Alt',                   name: 'CA',    shift: ss.KVKS_CTRL | ss.KVKS_ALT, vkeys: [vk.K_CONTROL,vk.K_ALT]},
  {desc: 'Shift+Ctrl+Alt',             name: 'SCA',   shift: ss.KVKS_SHIFT | ss.KVKS_CTRL | ss.KVKS_ALT, vkeys: [vk.K_SHIFT,vk.K_CONTROL,vk.K_ALT]},  //7

  {desc: 'Left Ctrl',                  name: 'LC',    shift: ss.KVKS_LCTRL, vkeys: [vk.K_LCONTROL]},
  {desc: 'Right Ctrl',                 name: 'RC',    shift: ss.KVKS_RCTRL, vkeys: [vk.K_RCONTROL]},
  {desc: 'Left Alt',                   name: 'LA',    shift: ss.KVKS_LALT, vkeys: [vk.K_LALT]},
  {desc: 'Right Alt',                  name: 'RA',    shift: ss.KVKS_RALT, vkeys: [vk.K_RALT]},
  {desc: 'Shift+Left Ctrl',            name: 'SLC',   shift: ss.KVKS_SHIFT | ss.KVKS_LCTRL, vkeys: [vk.K_SHIFT,vk.K_LCONTROL]},
  {desc: 'Shift+Right Ctrl',           name: 'SRC',   shift: ss.KVKS_SHIFT | ss.KVKS_RCTRL, vkeys: [vk.K_SHIFT,vk.K_RCONTROL]},
  {desc: 'Shift+Left Alt',             name: 'SLA',   shift: ss.KVKS_SHIFT | ss.KVKS_LALT, vkeys: [vk.K_SHIFT,vk.K_LALT]},
  {desc: 'Shift+Right Alt',            name: 'SRA',   shift: ss.KVKS_SHIFT | ss.KVKS_RALT, vkeys: [vk.K_SHIFT,vk.K_RALT]},
  {desc: 'Left Ctrl+Left Alt',         name: 'LCLA',  shift: ss.KVKS_LCTRL | ss.KVKS_LALT, vkeys: [vk.K_LCONTROL,vk.K_LALT]},
  {desc: 'Left Ctrl+Right Alt',        name: 'LCRA',  shift: ss.KVKS_LCTRL | ss.KVKS_RALT, vkeys: [vk.K_LCONTROL,vk.K_RALT]},
  {desc: 'Right Ctrl+Left Alt',        name: 'RCLA',  shift: ss.KVKS_RCTRL | ss.KVKS_LALT, vkeys: [vk.K_RCONTROL,vk.K_LALT]},
  {desc: 'Right Ctrl+Right Alt',       name: 'RCRA',  shift: ss.KVKS_RCTRL | ss.KVKS_RALT, vkeys: [vk.K_RCONTROL,vk.K_RALT]},
  {desc: 'Shift+Left Ctrl+Left Alt',   name: 'SLCLA', shift: ss.KVKS_SHIFT | ss.KVKS_LCTRL | ss.KVKS_LALT, vkeys: [vk.K_SHIFT, vk.K_LCONTROL,vk.K_LALT]},
  {desc: 'Shift+Left Ctrl+Right Alt',  name: 'SLCRA', shift: ss.KVKS_SHIFT | ss.KVKS_LCTRL | ss.KVKS_RALT, vkeys: [vk.K_SHIFT, vk.K_LCONTROL,vk.K_RALT]},
  {desc: 'Shift+Right Ctrl+Left Alt',  name: 'SRCLA', shift: ss.KVKS_SHIFT | ss.KVKS_RCTRL | ss.KVKS_LALT, vkeys: [vk.K_SHIFT, vk.K_RCONTROL,vk.K_LALT]},
  {desc: 'Shift+Right Ctrl+Right Alt', name: 'SRCRA', shift: ss.KVKS_SHIFT | ss.KVKS_RCTRL | ss.KVKS_RALT, vkeys: [vk.K_SHIFT, vk.K_RCONTROL,vk.K_RALT]},  //16
];
