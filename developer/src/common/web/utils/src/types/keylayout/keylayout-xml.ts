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
  keyboard: KL_Keyboard;
};

export interface KL_Keyboard {
  group?: string;
  id?: string;
  name?: string;
  maxoutS?: string;

  layoutsMM: KL_Layouts[];
  layouts?: KL_Layouts[];
  modifierMap?: KL_ModifierMap[];
  keyMapSet?: KL_KeyMapSet[];
  actions?: KL_Actions[];
  terminators?: KL_Terminators[];
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

export interface KL_ModifierMap {
  id?: string;
  defaultIndex?: string;

  keyMapSelect?: KL_KeyMapSelect[];
};

export interface KL_KeyMapSelect {
  mapIndex?: string;
  modifier?: KL_Modifier[];
};

export interface KL_Modifier {
  keys?: string;
};

export interface KL_KeyMapSet {
  id?: string;
  keyMap?: KL_KeyMap[];
};

export interface KL_KeyMap {
  index?: string;
  key?: KL_Key[];
};

export interface KL_Key {
  code?: string;
  action?: string;
  output?: string;
};

export interface KL_Actions {
  action?: KL_Action[];
};

export interface KL_Action {
  id?: string;
  when?: KL_When[];
};

export interface KL_When {
  state?: string;
  output?: string;
  next?: string;
};

export interface KL_Terminators {
  when?: KL_When[];
};

