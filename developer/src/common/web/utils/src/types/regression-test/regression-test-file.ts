/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Keyman Developer kmn keyboard regression test .xml format,
 * normalized for JS patterns of use.
 */

export interface RegressionTestSourceFile {
  info: RegTestInfo;
  events: RegTestEvent[];
};

export interface RegTestInfo {
  version?: string;
  systemkeyboard?: string;
  keyboard?: string;
  beginmode?: string;
};

export interface RegTestEvent {
  key: RegTestKey;
  postcontext: (RegTestDeadkey|string)[];
};

export interface RegTestKey {
  vkey: string;
  shiftstate: RegTestShiftState[];
};

export enum RegTestShiftState {
  shift,
  ctrl,
  rctrl,
  alt,
  altgr,
  caps,
}

export interface RegTest_Modifier {};

export interface RegTestDeadkey {
  deadkey: string;
};
