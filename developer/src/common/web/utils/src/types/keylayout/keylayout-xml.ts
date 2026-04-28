/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by S. Schmitt on 2025-06-12
 *
 * The interfaces in this file are designed with reference to the mapped
 * structures produced by xml2js when passed keylayout .xml file.
 * The prefix KL is stands for 'Keylayout'
 *
 * A keylayout file is a configuration file used macOS to define custom
 * keyboard mappings, mapping physical key presses to specific characters
 * or Unicode symbols. These XML-based files, created with Ukelele, allow
 * users to create custom layouts for different languages or special characters,
 * such as mapping Option + Key combinations.  With kmc-convert we can convert
 * these keylayout files to .kmn files which can be used in Keyman keyboards.
 */

export interface KeylayoutXMLSourceFile {
  /**
   * <keyboard> -- the root element
   */
  keyboard: KL_Keyboard;
};

export interface KL_Keyboard {
  /**
   * attributes of the root element <keyboard>
   */
  group: string;
  id: string;
  name: string;
  maxout?: string;
  /**
   * <layouts>, <modifierMap>, <keyMapSet>, <actions>, <terminators>
   * the 5 main elements.
   */
  layouts: KL_Layouts[];
  modifierMap: KL_ModifierMap[];
  keyMapSet: KL_KeyMapSet[];
  actions?: KL_Actions;
  terminators?: KL_Terminators;
};

export interface KL_Layouts {
  /**
   * <layout> the sub element of <layouts>,
   * containing information about the use of (different) mapSet and modifiers
   */
  layout: KL_Layout[];
};

export interface KL_Layout {
  /**
   * attributes of the sub element <layout>
   * containing information about the use of mapSet and modifiers for certain keys
   * e.g. key 4 - key 5 use mapSet 2a4 and modifiers 19c
   * ( <layout first="4" last="5" mapSet="2a4" modifiers="19c"/> )
   * <mapSet> referencing <keyMapSet id>
   */
  first: string;
  last: string;
  mapSet: string;
  modifiers: string;
};

export interface KL_ModifierMap {
  /**
   * attributes of the element <modifierMap>
   */
  id: string;
  defaultIndex: string;
  /**
   * <keyMapSelect> the sub element of <modifierMap>
   */
  keyMapSelect: KL_KeyMapSelect[];
};

export interface KL_KeyMapSelect {
  /**
   * attributes of the element <keyMapSelect>
   * containing a set of modifier combinations for a behavior
   * <mapIndex> referencing <keyMap index>
   */
  mapIndex: string;
  /**
   * <modifier> the sub element of <keyMapSelect>
   */
  modifier: KL_Modifier[];
};

export interface KL_Modifier {
  /**
   * attributes of the element <modifier>
   * containing one combination of modifier keys
   */
  keys: string;
};

export interface KL_KeyMapSet {
  /**
   * attributes of the element <keyMapSet>
   * <id> referencing <layouts mapSet>
   */
  id: string;
  /**
   * <keyMap> the sub element of <keyMapSet>
   */
  keyMap: KL_KeyMap[];
};

export interface KL_KeyMap {
  /**
   * attributes of the element <keyMap>
   * <index> referencing <keyMapSelect mapIndex>
   */
  index: string;
  baseMapSet?: string;
  baseIndex?: string;
  /**
   * <key> the sub element of <keyMap>
   */
  key: KL_Key[];
};

export interface KL_Key {
  /**
   * attributes of the element <key>
   * containing a keycode and its output or action
   */
  code: string;
  action?: string;  //TODO-KMC-CONVERT: Support <action> sub-element 'anonymous actions'
  output?: string;
};

export interface KL_Actions {
  /**
   * <action> the sub element of <actions>
   */
  action: KL_Action[];
};

export interface KL_Action {
  /**
   * attributes of the element <action>
   * defining an action id
   */
  id?: string;
  /**
   * <when> a sub element of <action>
   */
  when?: KL_When[];
};

export interface KL_When {
  /**
   * attributes of the element <when>
   * contain either a state-output pair or a state-next pair
   * to define which output or next is followed by a state
   */
  state?: string;
  through?: string;
  output?: string;
  multiplier?: string;
  next?: string;
};

export interface KL_Terminators {
  /**
   * <when> a sub element of <terminators>
   */
  when?: KL_When[];
};
