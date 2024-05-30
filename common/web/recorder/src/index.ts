import { type OutputTarget } from "@keymanapp/keyboard-processor";
import { KeyDistribution, KeyEvent, Mock } from "@keymanapp/keyboard-processor";

import Proctor from "./proctor.js";

export { default as Proctor } from "./proctor.js";
export { default as NodeProctor } from "./nodeProctor.js";

import * as utils from "@keymanapp/web-utils";

type EventTypeTag = 'key' | 'osk';
type TaggedEventObject = { type: EventTypeTag }

//#region Defines the InputEventSpec set, used to reconstruct DOM-based events for browser-based simulation
export abstract class InputEventSpec {
  type: EventTypeTag;
  static fromJSONObject(obj: TaggedEventObject): InputEventSpec {
    if(obj) {
      if(obj.type == "key") {
        return new PhysicalInputEventSpec(obj as PhysicalInputEventSpec);
      } else if(obj.type == "osk") {
        return new OSKInputEventSpec(obj as OSKInputEventSpec);
      }
    }

    throw new SyntaxError("Error in JSON format corresponding to an InputEventSpec!");
  }

  toPrettyJSON(): string {
    // We want the default, non-spaced JSON for this class, even when otherwise adding whitespace.
    var str = JSON.stringify(this);
    return str;
  }
}

export class PhysicalInputEventSpec extends InputEventSpec {
  static readonly modifierCodes: { [mod:string]: number } = {
    "Shift":0x0001,
    "Control":0x0002,
    "Alt":0x0004,
    "Meta":0x0008,
    "CapsLock":0x0010,
    "NumLock":0x0020,
    "ScrollLock":0x0040
  };

  // KeyboardEvent properties
  type: "key" = "key";
  key: string;
  code: string;
  keyCode: number;
  modifierSet: number;
  location: number;

  constructor(e?: PhysicalInputEventSpec | Partial<Pick<PhysicalInputEventSpec, 'key' | 'code' | 'keyCode' | 'modifierSet' | 'location'>>) { // parameter is used to reconstruct from JSON.
    super();

    if(e) {
      this.key = e.key;
      this.code = e.code;
      this.keyCode = e.keyCode;
      this.modifierSet = e.modifierSet;
      this.location = e.location;
    }
  }

  getModifierState(key: string): boolean {
    return (PhysicalInputEventSpec.modifierCodes[key] & this.modifierSet) != 0;
  }

  generateModifierString(): string {
    var list: string = "";

    for(var key in PhysicalInputEventSpec.modifierCodes) {
      if(this.getModifierState(key)) {
        list += ((list != "" ? " " : "") + key);
      }
    }

    return list;
  }
}

export class OSKInputEventSpec extends InputEventSpec {
  type: "osk" = "osk";
  keyID: string;

  // The parameter may be used to reconstruct the item from raw JSON.
  constructor(e?: OSKInputEventSpec) {
    super();
    if(e) {
      this.keyID = e.keyID;
    }
  }
}
//#endregion

export abstract class RecordedKeystroke {
  type: EventTypeTag;

  static fromJSONObject(obj: TaggedEventObject): RecordedKeystroke {
    if(obj) {
      if(obj.type == "key") {
        return new RecordedPhysicalKeystroke(obj as RecordedPhysicalKeystroke);
      } else if(obj.type == "osk") {
        return new RecordedSyntheticKeystroke(obj as RecordedSyntheticKeystroke);
      }
    }

    throw new SyntaxError("Error in JSON format corresponding to a RecordedKeystroke!");
  }

  toPrettyJSON(): string {
    // We want the default, non-spaced JSON for this class, even when otherwise adding whitespace.
    var str = JSON.stringify(this);
    return str;
  }

  /**
   * Returns an InputEventSpec that may be used to simulate the keystroke within a browser-based environment.
   */
  abstract get inputEventSpec(): InputEventSpec;
}

export class RecordedPhysicalKeystroke extends RecordedKeystroke {
  // KeyboardEvent properties
  type: "key" = "key";

  keyCode: number;  // may be different from eventSpec's value b/c keymapping
  states: number;
  modifiers: number;
  modifierChanged: boolean;
  isVirtualKey: boolean;
  vkCode: number;  // may be possible to eliminate; differences arise from mnemonics.

  eventSpec: PhysicalInputEventSpec;

  constructor(keystroke: RecordedPhysicalKeystroke)
  constructor(keystroke: KeyEvent, eventSpec: PhysicalInputEventSpec)
  constructor(keystroke: RecordedPhysicalKeystroke|KeyEvent, eventSpec?: PhysicalInputEventSpec) {
    super();

    if(keystroke instanceof KeyEvent || typeof keystroke.type === 'undefined') {
      // Store what is necessary for headless event reconstruction.
      keystroke = keystroke as KeyEvent;
      this.keyCode = keystroke.Lcode;
      this.states = keystroke.Lstates;
      this.modifiers = keystroke.Lmodifiers;
      this.modifierChanged = !!keystroke.LmodifierChange;
      this.isVirtualKey = keystroke.LisVirtualKey;
      this.vkCode = keystroke.vkCode;

      // Also store the DOM-based event spec for use in integrated testing.
      this.eventSpec = eventSpec;
    } else {
      // It might be a raw object, from JSON.
      this.keyCode = keystroke.keyCode;
      this.states = keystroke.states;
      this.modifiers = keystroke.modifiers;
      this.modifierChanged = keystroke.modifierChanged;
      this.isVirtualKey = keystroke.isVirtualKey;
      this.vkCode = keystroke.vkCode;

      this.eventSpec = new PhysicalInputEventSpec(keystroke.eventSpec); // must also be reconstructed.
    }
  }

  get inputEventSpec(): InputEventSpec {
    return this.eventSpec;
  }
}

export class RecordedSyntheticKeystroke extends RecordedKeystroke {
  // KeyboardEvent properties
  type: "osk" = "osk";

  keyName: string;
  layer: string;

  keyDistribution?: KeyDistribution;

  constructor(keystroke: RecordedSyntheticKeystroke)
  constructor(keystroke: KeyEvent)
  constructor(keystroke: RecordedSyntheticKeystroke|KeyEvent) {
    super();

    if(keystroke instanceof KeyEvent || typeof keystroke.type === 'undefined') {
      keystroke = keystroke as KeyEvent;
      // Store what is necessary for headless event reconstruction.

      // Also store the DOM-based event spec for use in integrated testing.
      this.layer = keystroke.kbdLayer;
      this.keyName = keystroke.kName;
      this.keyDistribution = keystroke.keyDistribution;
    } else {
      // It might be a raw object, from JSON.
      this.layer = keystroke.layer;
      this.keyName = keystroke.keyName;
      this.keyDistribution = keystroke.keyDistribution;
    }
  }

  get inputEventSpec(): InputEventSpec {
    let eventSpec = new OSKInputEventSpec();
    eventSpec.keyID = this.layer + '-' + this.keyName;

    return eventSpec;
  }
}

export abstract class TestSequence<KeyRecord extends RecordedKeystroke | InputEventSpec> {
  inputs: KeyRecord[];
  output: string;
  msg?: string;

  abstract hasOSKInteraction(): boolean;

  async test(proctor: Proctor, target?: OutputTarget): Promise<{success: boolean, result: string}> {
    // Start with an empty OutputTarget and a fresh KeyboardProcessor.
    if(!target) {
      target = new Mock();
    }

    proctor.before();

    let result = await proctor.simulateSequence(this, target);
    proctor.assertEquals(result, this.output, this.msg);

    return {success: (result == this.output), result: result};
  }

  toPrettyJSON(): string {
    var str = "{ ";
    if(this.output) {
      str += "\"output\": \"" + this.output + "\", ";
    }
    str += "\"inputs\": [\n";
    for(var i = 0; i < this.inputs.length; i++) {
      str += "  " + this.inputs[i].toPrettyJSON() + ((i == this.inputs.length-1) ? "\n" : ",\n");
    }
    if(this.msg) {
      str += "], \"message\": \"" + this.msg + "\" }";
    } else {
      str += "]}";
    }
    return str;
  }
}

export class InputEventSpecSequence extends TestSequence<InputEventSpec> {
  inputs: InputEventSpec[];
  output: string;
  msg?: string;

  constructor(ins?: InputEventSpec[] | InputEventSpecSequence, outs?: string, msg?: string) {
    super();

    if(ins) {
      if(ins instanceof Array) {
        this.inputs = [].concat(ins);
      } else {
        // We're constructing from existing JSON.
        this.inputs = [];

        for(var ie=0; ie < ins.inputs.length; ie++) {
          this.inputs.push(InputEventSpec.fromJSONObject(ins.inputs[ie]));
        }

        this.output = ins.output;
        this.msg = ins.msg;
        return;
      }
    } else {
      this.inputs = [];
    }

    if(outs) {
      this.output = outs;
    }

    if(msg) {
      this.msg = msg;
    }
  }

  addInput(event: InputEventSpec, output: string) {
    this.inputs.push(event);
    this.output = output;
  }

  hasOSKInteraction(): boolean {
    for(var i=0; i < this.inputs.length; i++) {
      if(this.inputs[i] instanceof OSKInputEventSpec) {
        return true;
      }
    }

    return false;
  }
}

export class RecordedKeystrokeSequence extends TestSequence<RecordedKeystroke> {
  inputs: RecordedKeystroke[];
  output: string;
  msg?: string;

  constructor(ins?: RecordedKeystroke[], outs?: string, msg?: string)
  constructor(sequence: RecordedKeystrokeSequence)
  constructor(ins?: RecordedKeystroke[] | RecordedKeystrokeSequence, outs?: string, msg?: string) {
    super();

    if(ins) {
      if(ins instanceof Array) {
        this.inputs = [].concat(ins);
      } else {
        // We're constructing from existing JSON.
        this.inputs = [];

        for(var ie=0; ie < ins.inputs.length; ie++) {
          this.inputs.push(RecordedKeystroke.fromJSONObject(ins.inputs[ie]));
        }

        this.output = ins.output;
        this.msg = ins.msg;
        return;
      }
    } else {
      this.inputs = [];
    }

    if(outs) {
      this.output = outs;
    }

    if(msg) {
      this.msg = msg;
    }
  }

  addInput(event: RecordedKeystroke, output: string) {
    this.inputs.push(event);
    this.output = output;
  }

  hasOSKInteraction(): boolean {
    for(var i=0; i < this.inputs.length; i++) {
      if(this.inputs[i] instanceof RecordedSyntheticKeystroke) {
        return true;
      }
    }

    return false;
  }
}

class FontStubForLanguage {
  family: string;
  source: string[];

  constructor(activeStubEntry: any) {
    this.family = activeStubEntry.family;

    var src = activeStubEntry.files;
    if(!(src instanceof Array)) {
      src = [ src ];
    }

    this.source = [];
    for(var i=0; i < src.length; i++) {
      this.source.push(activeStubEntry.path + src[i]);
    }
  }
}

export class LanguageStubForKeyboard {
  id: string;
  name: string;
  region: string;
  font?: FontStubForLanguage;
  oskFont?: FontStubForLanguage;

  constructor(activeStub: any) {
    if(activeStub.KLC) {
      this.id = activeStub.KLC;
      this.name = activeStub.KL;
      this.region = activeStub.KR;

      // Fonts.
      if(activeStub.KFont) {
        this.font = new FontStubForLanguage(activeStub.KFont);
      }
      if(activeStub.KOskFont) {
        this.oskFont = new FontStubForLanguage(activeStub.KOskFont);
      }
    } else {
      this.id = activeStub.id;
      this.name = activeStub.name;
      this.region = activeStub.region;

      // If we end up adding functionality to FontStubForLanguage, we'll need to properly reconstruct these.
      this.font = activeStub.font;
      this.oskFont = activeStub.oskFont;
    }
  }
}

export class KeyboardStub {
  id: string;
  name: string;
  filename: string;
  languages: LanguageStubForKeyboard | LanguageStubForKeyboard[];

  // Constructs a stub usable with KeymanWeb's addKeyboards() API function from
  // the internally-tracked ActiveStub value for that keyboard.
  constructor(json?: KeyboardStub) {
    if(json) {
      this.id = json.id;
      this.name = json.name;
      this.filename = json.filename;

      if(!Array.isArray(json.languages)) {
        this.languages = new LanguageStubForKeyboard(json.languages);
      } else {
        this.languages = [];
        for(var i=0; i < json.languages.length; i++) {
          this.languages.push(new LanguageStubForKeyboard(json.languages[i]));
        }
      }
    }
  }

  getFirstLanguage() {
    if(this.languages instanceof LanguageStubForKeyboard) {
      return this.languages.id;
    } else {
      return this.languages[0].id;
    }
  }
}

type TARGET = 'hardware'|'desktop'|'phone'|'tablet';
type OS = 'windows'|'android'|'ios'|'macosx'|'linux';
type BROWSER = 'chrome'|'firefox'|'safari'|'opera';  // ! no 'edge' detection in KMW!

export class Constraint {
  target: TARGET;
  validOSList?: OS[];
  validBrowsers?: BROWSER[];

  constructor(target: TARGET|Constraint, validOSList?: OS[], validBrowsers?: BROWSER[]) {
    if(typeof(target) == 'string') {
      this.target = target;
      this.validOSList = validOSList;
      this.validBrowsers = validBrowsers;
    } else {
      var json = target;
      this.target = json.target;
      this.validOSList = json.validOSList;
      this.validBrowsers = json.validBrowsers;
    }
  }

  matchesClient(device: utils.DeviceSpec, usingOSK?: boolean) {
    // #1:  Platform check.
    if(usingOSK === true) {
      if(this.target != device.formFactor) {
        return false;
      }
    } else if(usingOSK === false) {
      if(this.target != 'hardware') {
        return false;
      }
    } else if(this.target != device.formFactor && this.target != 'hardware') {
      return false;
    }

    if(this.validOSList) {
      if(this.validOSList.indexOf(device.OS as OS) == -1) {
        return false;
      }
    }

    if(this.validBrowsers) {
      if(this.validBrowsers.indexOf(device.browser as BROWSER) == -1) {
        return false;
      }
    }

    return true;
  }

  // Checks if another Constraint instance is functionally identical to this one.
  equals(other: Constraint) {
    if(this.target != other.target) {
      return false;
    }

    var list1 = this.validOSList ? this.validOSList : ['any'];
    var list2 = other.validOSList ? other.validOSList : ['any'];

    if(list1.sort().join(',') != list2.sort().join(',')) {
      return false;
    }

    list1 = this.validBrowsers ? this.validBrowsers : ['web'];
    list2 = other.validBrowsers ? other.validBrowsers : ['web'];

    if(list1.sort().join(',') != list2.sort().join(',')) {
      return false;
    }

    return true;
  }
}

export class TestFailure {
  constraint: Constraint;
  test: InputEventSpecSequence;
  result: string;

  constructor(constraint: Constraint, test: InputEventSpecSequence, output: string) {
    this.constraint = constraint;
    this.test = test;
    this.result = output;
  }
}

export interface TestSet<Sequence extends TestSequence<any>> {
  constraint: Constraint;

  addTest(seq: Sequence): void;
  isValidForDevice(device: utils.DeviceSpec, usingOSK?: boolean): boolean;
  test(proctor: Proctor): Promise<TestFailure[]>;
}

/**
 * The core constraint-specific test set definition used for testing versions 10.0 to 13.0.
 */
export class EventSpecTestSet implements TestSet<InputEventSpecSequence> {
  constraint: Constraint;
  testSet: InputEventSpecSequence[];

  constructor(constraint: Constraint|EventSpecTestSet) {
    if("target" in constraint) {
      this.constraint = constraint as Constraint;
      this.testSet = [];
    } else {
      var json = constraint as EventSpecTestSet;
      this.constraint = new Constraint(json.constraint);
      this.testSet = [];

      // Clone each test sequence / reconstruct from methodless JSON object.
      for(var i=0; i < json.testSet.length; i++) {
        this.testSet.push(new InputEventSpecSequence(json.testSet[i]));
      }
    }
  }

  addTest(seq: InputEventSpecSequence) {
    this.testSet.push(seq);
  }

  // Used to determine if the current EventSpecTestSet is applicable to be run on a device.
  isValidForDevice(device: utils.DeviceSpec, usingOSK?: boolean) {
    return this.constraint.matchesClient(device, usingOSK);
  }

  // Validity should be checked before calling this method.
  async test(proctor: Proctor): Promise<TestFailure[]> {
    var failures: TestFailure[] = [];
    let testSet = this.testSet;

    for(var i=0; i < testSet.length; i++) {
      const testSeq = this.testSet[i];
      const simResult = await testSeq.test(proctor);
      if(!simResult.success) {
        // Failed test!
        failures.push(new TestFailure(this.constraint, testSeq, simResult.result));
      }
    }

    return failures.length > 0 ? failures : null;
  }
}

/**
 * The core constraint-specific test set definition used for testing versions 10.0 to 13.0.
 */
export class RecordedSequenceTestSet implements TestSet<RecordedKeystrokeSequence> {
  constraint: Constraint;
  testSet: RecordedKeystrokeSequence[];

  constructor(constraint: Constraint|RecordedSequenceTestSet) {
    if("target" in constraint) {
      this.constraint = constraint as Constraint;
      this.testSet = [];
    } else {
      var json = constraint as RecordedSequenceTestSet;
      this.constraint = new Constraint(json.constraint);
      this.testSet = [];

      // Clone each test sequence / reconstruct from methodless JSON object.
      for(var i=0; i < json.testSet.length; i++) {
        this.testSet.push(new RecordedKeystrokeSequence(json.testSet[i]));
      }
    }
  }

  addTest(seq: RecordedKeystrokeSequence) {
    this.testSet.push(seq);
  }

  // Used to determine if the current EventSpecTestSet is applicable to be run on a device.
  isValidForDevice(device: utils.DeviceSpec, usingOSK?: boolean) {
    return this.constraint.matchesClient(device, usingOSK);
  }

  // Validity should be checked before calling this method.
  async test(proctor: Proctor): Promise<TestFailure[]> {
    var failures: TestFailure[] = [];
    let testSet = this.testSet;

    for(var i=0; i < testSet.length; i++) {
      const testSeq = this.testSet[i];
      const simResult = await testSeq.test(proctor);
      if(!simResult.success) {
        // Failed test!
        failures.push(new TestFailure(this.constraint, testSeq, simResult.result));
      }
    }

    return failures.length > 0 ? failures : null;
  }

  toTestName(): string {
    let name = "constraint: for " + this.constraint.target;

    if(this.constraint.target == 'hardware') {
      name += " keyboard";
    } else {
      name += " OSK";
    }
    if(this.constraint.validOSList) {
      name += " on OS of " + JSON.stringify(this.constraint.validOSList);
    }
    if(this.constraint.validBrowsers) {
      name += " in browser of " + JSON.stringify(this.constraint.validBrowsers);
    }

    return name;
  }
}

export class KeyboardTest {
  /**
   * Indicates what version of KMW's recorder the spec conforms to.
   */
  public specVersion: utils.Version = KeyboardTest.CURRENT_VERSION;

  /**
   * The version of KMW in which the Recorder was first written.  Worked from 10.0 to 13.0 with
   * only backward-compatible changes and minor tweaks to conform to internal API shifts.
   */
  public static readonly FALLBACK_VERSION = new utils.Version("10.0");
  public static readonly CURRENT_VERSION  = new utils.Version("14.0");

  /**
   * The stub information to be passed into keyman.addKeyboards() in order to run the test.
   */
  keyboard: KeyboardStub;

  /**
   * The master array of test sets, each of which specifies constraints a client must fulfill for
   * the tests contained therein to be valid.
   */
  inputTestSets: TestSet<any>[];

  /**
   * Reconstructs a KeyboardTest object from its JSON representation, restoring its methods.
   * @param fromJSON
   */
  constructor(fromJSON?: string|KeyboardStub|KeyboardTest) {
    if(!fromJSON) {
      this.keyboard = null;
      this.inputTestSets = [];
      return;
    } else if(typeof(fromJSON) == 'string') {
      fromJSON = JSON.parse(fromJSON) as KeyboardTest;
    } else if(fromJSON instanceof KeyboardStub) {
      this.keyboard = fromJSON;
      this.inputTestSets = [];
      return;
    }

    if(!fromJSON.specVersion) {
      fromJSON.specVersion = KeyboardTest.FALLBACK_VERSION;
    } else {
      // Is serialized to a String when saved.
      fromJSON.specVersion = new utils.Version(fromJSON.specVersion as unknown as string);
    }

    this.keyboard = new KeyboardStub(fromJSON.keyboard);
    this.inputTestSets = [];
    this.specVersion = fromJSON.specVersion;

    if(this.specVersion.equals(KeyboardTest.FALLBACK_VERSION)) {
      // Top-level test spec:  EventSpecTestSet, based entirely on browser events.
      for(var i=0; i < fromJSON.inputTestSets.length; i++) {
        this.inputTestSets[i] = new EventSpecTestSet(fromJSON.inputTestSets[i] as EventSpecTestSet);
      }
    } else {
      for(var i=0; i < fromJSON.inputTestSets.length; i++) {
        this.inputTestSets[i] = new RecordedSequenceTestSet(fromJSON.inputTestSets[i] as RecordedSequenceTestSet);
      }
    }
  }

  addTest(constraint: Constraint, seq: RecordedKeystrokeSequence) {
    if(!this.specVersion.equals(KeyboardTest.CURRENT_VERSION)) {
      throw new Error("The currently-loaded test was built to an outdated specification and may not be altered.");
    }

    for(var i=0; i < this.inputTestSets.length; i++) {
      if(this.inputTestSets[i].constraint.equals(constraint)) {
        this.inputTestSets[i].addTest(seq);
        return;
      }
    }

    var newSet = new RecordedSequenceTestSet(new Constraint(constraint));
    this.inputTestSets.push(newSet);
    newSet.addTest(seq);
  }

  async test(proctor: Proctor) {
    var setHasRun = false;
    var failures: TestFailure[] = [];

    await proctor.beforeAll();

    // The original test spec requires a browser environment and thus requires its own `.run` implementation.
    if(!(proctor.compatibleWithSuite(this))) {
      throw Error("Cannot perform version " + KeyboardTest.FALLBACK_VERSION + "-based testing outside of browser-based environments.");
    }

    // Otherwise, the test spec instances will know how to run in any currently-supported environment.
    for(var i = 0; i < this.inputTestSets.length; i++) {
      var testSet = this.inputTestSets[i];

      if(proctor.matchesTestSet(testSet)) {
        var testFailures = await testSet.test(proctor);
        if(testFailures) {
          failures = failures.concat(testFailures);
        }
        setHasRun = true;
      }
    }

    if(!setHasRun) {
      // The sets CAN be empty, allowing silent failure if/when we actually want that.
      console.warn("No test sets for this keyboard were applicable for this device!");
    }

    // Allow the method's caller to trigger a 'fail'.
    if(failures.length > 0) {
      return failures;
    } else {
      return null;
    }
  }

  isEmpty() {
    return this.inputTestSets.length == 0;
  }

  toPrettyJSON() {
    return JSON.stringify(this, null, '  ');
  }

  get isLegacy(): boolean {
    return !this.specVersion.equals(KeyboardTest.CURRENT_VERSION);
  }
}