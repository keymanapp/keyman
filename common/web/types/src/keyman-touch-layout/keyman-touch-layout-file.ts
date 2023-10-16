//
// .keyman-touch-layout JSON format
//
// Follows /common/schemas/keyman-touch-layout/keyman-touch-layout.spec.json for
// reading and
// /common/schemas/keyman-touch-layout/keyman-touch-layout.clean.spec.json for
// writing
//

export interface TouchLayoutFile {
  tablet?: TouchLayoutPlatform;
  phone?: TouchLayoutPlatform;
  desktop?: TouchLayoutPlatform;
};

export type TouchLayoutFont = string;
export type TouchLayoutFontSize = string;
export type TouchLayoutDefaultHint = "none"|"dot"|"longpress"|"multitap"|"flick"|"flick-n"|"flick-ne"|"flick-e"|"flick-se"|"flick-s"|"flick-sw"|"flick-w"|"flick-nw";

export interface TouchLayoutPlatform {
  font?: TouchLayoutFont;
  fontsize?: TouchLayoutFontSize;
  layer: TouchLayoutLayer[];
  displayUnderlying?: boolean;
  defaultHint: TouchLayoutDefaultHint;
};

export type TouchLayoutLayerId = string;  // pattern = /^[a-zA-Z0-9_-]+$/

export interface TouchLayoutLayer {
  id: TouchLayoutLayerId;
  row: TouchLayoutRow[];
};

export type TouchLayoutRowId = number;

export interface TouchLayoutRow {
  id: TouchLayoutRowId;
  key: TouchLayoutKey[];
};

type Key_Type = 'T'|'K'|'U'|'t'|'k'|'u';
type Key_Id = string;
export type TouchLayoutKeyId = `${Key_Type}_${Key_Id}`; // pattern = /^[TKUtku]_[a-zA-Z0-9_]+$/

export interface TouchLayoutKey {
  id?: TouchLayoutKeyId;
  text?: string;
  layer?: TouchLayoutLayerId;
  nextlayer?: TouchLayoutLayerId;
  font?: TouchLayoutFont;
  fontsize?: TouchLayoutFontSize;
  sp?: TouchLayoutKeySp;
  pad?: TouchLayoutKeyPad;
  width?: TouchLayoutKeyWidth;
  sk?: TouchLayoutSubKey[];
  flick?: TouchLayoutFlick;
  multitap?: TouchLayoutSubKey[];
  hint?: string;
};

export const enum TouchLayoutKeySp {
  normal=0,
  special=1,
  specialActive=2,
  // Special use in the Web OSK - resolved at runtime, is a variant of `special` with the keyboard font.
  customSpecial=3,
  // Special use in the Web OSK - resolved at runtime, is a runtime variant of `specialActive` with the keyboard font.
  customSpecialActive=4,
  deadkey=8,
  blank=9,
  spacer=10
};

export type TouchLayoutKeyPad = number; // 0-100000
export type TouchLayoutKeyWidth = number; // 0-100000

export interface TouchLayoutSubKey {
  id: TouchLayoutKeyId;
  text?: string;
  layer?: TouchLayoutLayerId;
  nextlayer?: TouchLayoutLayerId;
  font?: TouchLayoutFont;
  fontsize?: TouchLayoutFontSize;
  sp?: TouchLayoutKeySp;
  pad?: TouchLayoutKeyPad;
  width?: TouchLayoutKeyWidth;
  default?: boolean;  // Only used for longpress currently
};

export interface TouchLayoutFlick {
  n?: TouchLayoutSubKey;
  s?: TouchLayoutSubKey;
  e?: TouchLayoutSubKey;
  w?: TouchLayoutSubKey;
  ne?: TouchLayoutSubKey;
  nw?: TouchLayoutSubKey;
  se?: TouchLayoutSubKey;
  sw?: TouchLayoutSubKey;
};
