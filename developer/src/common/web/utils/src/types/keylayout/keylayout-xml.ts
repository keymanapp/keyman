/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by S. Schmitt on 2025-06-12
 *
 * The interfaces in this file are designed with reference to the mapped
 * structures produced by xml2js when passed keylayout .xml file.
 * The prefix KL is stands for 'Keylayout'
 */

export interface KeylayoutXMLSourceFile {
  /**
   * <keyboard> -- the root element.
   */
  keyboard: KL_keyboard;
};

export interface KL_keyboard {
  group?: string;
  id?: string;
  name?: string;
  maxoutS?: string;

  layoutsMM: KL_Layouts[];
  layouts?: KL_Layouts[];
  modifierMap?: KL_modifierMap[];
  keyMapSet?: KL_keyMapSet[];
  actions?: KL_actions[];
  terminators?: KL_terminators[];
};

export interface KL_Layouts {
  layouts?: KL_Layout;
};

export interface KL_Layout {
  first?: string;
  last?: string;
  mapSet?: string;
  modifiers?: string;
};

export interface KL_modifierMap {
  id?: string;
  defaultIndex?: string;

  keyMapSelect?: KL_keyMapSelect[];
};

export interface KL_keyMapSelect {
  mapIndex?: string;
  modifier?: KL_modifier[];
};

export interface KL_modifier {
  keys?: string;
};

export interface KL_keyMapSet {
  id?: string;
  keyMap?: KL_keyMap[];
};

export interface KL_keyMap {
  index?: string;
  key?: KL_key[];
};

export interface KL_key {
  code?: string;
  action?: string;
  output?: string;
};

export interface KL_actions {
  action?: KL_action[];
};

export interface KL_action {
  id?: string;
  when?: KL_when[];
};

export interface KL_when {
  state?: string;
  output?: string;
  next?: string;
};

export interface KL_terminators {
  when?: KL_when[];
};

