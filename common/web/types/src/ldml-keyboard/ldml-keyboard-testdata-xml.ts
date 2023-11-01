//
// Conforms to techpreview
//
// The interfaces in this file are designed with reference to the mapped
// structures produced by xml2js when passed a LDML keyboard test data .xml file.
//
// Using prefix LKT for LDML Keyboard Test
//

export interface LDMLKeyboardTestDataXMLSourceFile {
  /**
   * <keyboardTest> -- the root element.
   */
  keyboardTest3: LKTKeyboardTest;
}

export interface LKTKeyboardTest {
  conformsTo?: string;
  info?: LKTInfo;
  repertoire?: LKTRepertoire[];
  tests?: LKTTests[];
};

export interface LKTInfo {
  author?: string;
  keyboard?: string;
  name?: string;
};

export interface LKTRepertoire {
  name?: string;
  chars?: string;
  type?: string;
};

export interface LKTTests {
  name?: string;
  test?: LKTTest[];
};

export interface LKTTest {
  name?: string;
  startContext?: LKTStartContext;
  actions?: LKTAnyAction[];  // differs from XML, to represent order of actions
};

export interface LKTStartContext {
  to?: string;
};

/**
 * Test Actions.
 * The expectation is that each LKTAction object will have exactly one non-falsy field.
 */
export interface LKTAction {
  type?: "check" | "emit" | "keystroke" | "backspace";
};

export interface LKTCheck extends LKTAction {
  type: "check";
  result?: string;
};

export interface LKTEmit extends LKTAction {
  type: "emit";
  to?: string;
};

export interface LKTKeystroke extends LKTAction {
  type: "keystroke";
  key?: string;
  flick?: string;
  longPress?: string;
  tapCount?: string;
};

export interface LKTBackspace extends LKTAction {
  type: "backspace";
}

export type LKTAnyAction = LKTCheck | LKTEmit | LKTKeystroke | LKTBackspace;
