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
 * see also TN2056 (https://developer.apple.com/library/archive/technotes/tn2056/_index.html)
 */


export interface KeylayoutXMLSourceFile {
  /**
   * The keyboard element is the root element of the .keylayout file and contains all other elements.
   */
  keyboard: KL_Keyboard;
};

export interface KL_Keyboard {
  /**
   * The group attribute is a string that categorizes the keyboard layout.
   * This attribute is not processed within kmc-convert at present.
   */
  group: string;
  /**
   * The id attribute is a unique identifier for the keyboard layout.
   * This attribute is not processed within kmc-convert at present.
   */
  id: string;
  /**
   * The name attribute provides a human-readable name for the layout.
   */
  name: string;
  /**
   * The maxout attribute specifies the maximum number of output characters that can be generated from a single key press.
   * This attribute is not processed within kmc-convert at present.
   */
  maxout?: string;

  /**
   * The layouts element defines several possible layout options of a keyboard.
   */
  layouts: KL_Layouts[];
  /**
   * The modifierMap element, (sometimes referenced as 'behaviours' in kmc-convert) defines the mapping between
   * modifier combinations and key mapping tables.
   */
  modifierMap: KL_ModifierMap[];
  /**
   * The keyMapSet element contains the key mapping tables, which define the output for each key code
   * and modifier combination.
   */
  keyMapSet: KL_KeyMapSet[];
  /**
   * The actions element contains action definitions, which specify custom behaviors for certain key combinations.
   */
  actions?: KL_Actions;
  /**
   * The terminators element contains definitions for state transitions that can be used to define when
   * certain states should be terminated, based on specific conditions.
   * This element is not processed within kmc-convert at present.
   */
  terminators?: KL_Terminators;
};

export interface KL_Layouts {
  /**
   * The layout element defines the physical layout of a range of keys of the keyboard.
   */
  layout: KL_Layout[];
};

export interface KL_Layout {
  /**
   * The first attribute specifies the first key code in the range defined by the layout.
   * This attribute is not processed within kmc-convert at present.
   */
  first: string;
  /**
   * The last attribute specifies the last key code in the range defined by the layout.
   * This attribute is not processed within kmc-convert at present.
   */
  last: string;
  /**
   * The mapSet attribute references the id of a keyMapSet, which defines the key mapping tables
   * that should be used for the keys in this layout.
   * This attribute is not processed within kmc-convert at present.
   */
  mapSet: string;
  /**
   * The modifiers attribute references the modifier combinations defined in the modifierMap.
   * This attribute is not processed within kmc-convert at present.
   */
  modifiers: string;
};

export interface KL_ModifierMap {
  /**
   * The id attribute is a unique identifier for the modifier map, which is referenced
   * by the modifiers attribute of the layout element.
   */
  id: string;
  /**
   * The defaultIndex attribute specifies the default key mapping table to use when no specific modifier
   * combination is active.
   * This attribute is not processed within kmc-convert at present.
   */
  defaultIndex: string;

  /**
   * The keyMapSelect element defines a specific combination of modifier keys and the corresponding
   * key mapping table to use when that combination is active.
   */
  keyMapSelect: KL_KeyMapSelect[];
};

export interface KL_KeyMapSelect {
  /**
   * The mapIndex attribute specifies the index of the key mapping table to use when the specified
   * modifier combination is active.
   */
  mapIndex: string;

  /**
   * The modifier element defines a specific combination of modifier keys.
   */
  modifier: KL_Modifier[];
};

export interface KL_Modifier {
  /*
   * The keys attribute specifies the combination of modifier keys
   * for the corresponding key mapping table to be used.
   */
  keys: string;
};

export interface KL_KeyMapSet {
  /**
   * The id attribute is a unique identifier for the key map set, which
   * is referenced by the mapSet attribute of the layout element.
   * This attribute is not processed within kmc-convert at present.
   */
  id: string;

  /**
   * The keyMap element defines the output for each key code and modifier combination.
   */
  keyMap: KL_KeyMap[];
};

export interface KL_KeyMap {
  /**
   * The index attribute specifies the index of the key mapping table, which is referenced
   * by the mapIndex attribute of the keyMapSelect element in the modifierMap.
   */
  index: string;
  /**
   * The baseMapSet attribute specifies the id of another keyMapSet that serves as a base
   * for this key mapping table.
   * This attribute is not processed within kmc-convert at present.
   */
  baseMapSet?: string;
  /**
   * The baseIndex attribute specifies the index of the key mapping table
   * within the baseMapSet that serves as a base for this key mapping table.
   * This attribute is not processed within kmc-convert at present.
   */
  baseIndex?: string;

  /**
   * The key element defines the output for a specific key code and modifier combination.
   */
  key: KL_Key[];
};

export interface KL_Key {
  /**
   * The code attribute specifies the key code of a specific key on the keyboard.
   */
  code: string;
  /**
   * The action attribute specifies a custom behavior triggered when a key is pressed.
   * Ether the action attribute or the output attribute must be specified for a key, but not both.
   * kmc-convert does not use the concept of 'anonymous actions' at present.
   */
  action?: string;  //TODO-KMC-CONVERT: Support <action> sub-element 'anonymous actions' in the future
  /**
   * The output attribute specifies the character that is produced when a key is pressed.
   * Ether the action attribute or the output attribute must be specified for a key, but not both.
   */
  output?: string;
};

export interface KL_Actions {
  /**
   * The actions element contains action definitions, which specify custom behaviors for
   * certain key combinations.
   */
  action: KL_Action[];
};

export interface KL_Action {
  /**
   * The id attribute is a unique identifier for the action.
   */
  id?: string;

  /**
   * The when element defines a specific condition under which an action should be triggered.
   */
  when?: KL_When[];
};

export interface KL_When {
  /**
   * The state attribute specifies the state of the keyboard that should trigger an action.
   */
  state?: string;
  /**
   * The through attribute specifies a state transition that should occur when the specified condition is met.
   * This attribute is not processed within kmc-convert at present.
   */
  through?: string;
  /**
   * The output attribute specifies the character that is produced.
   */
  output?: string;
  /**
   * The multiplier attribute specifies a multiplier value that can be used to modify the behavior.
   * This attribute is not processed within kmc-convert at present.
   */
  multiplier?: string;
  /**
   * The next attribute specifies the next state to transition to when a specified
   * condition is met.
   */
  next?: string;
};

export interface KL_Terminators {
  /**
   * The when element defines a specific condition under which certain states should be terminated,
   * This element is not processed within kmc-convert at present.
  */
  when?: KL_When[];
};
