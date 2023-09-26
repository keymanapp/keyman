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
  actions?: LKTAction[];  // differs from XML, to represent order of actions
};

export interface LKTStartContext {
  to?: string;
};

export interface LKTCheck {
  result?: string;
};

export interface LKTEmit {
  to?: string;
};

export interface LKTKeystroke {
  key?: string;
  flick?: string;
  longPress?: string;
  tapCount?: string;
};

/**
 * Test Actions.
 * The expectation is that each LKTAction object will have exactly one non-falsy field.
 */
export interface LKTAction {
  check?: LKTCheck;
  emit?: LKTEmit;
  keystroke?: LKTKeystroke;
};
