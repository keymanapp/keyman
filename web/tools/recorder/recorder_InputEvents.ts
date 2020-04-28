/// <reference path="../../node_modules/@keymanapp/keyboard-processor/src/text/engineDeviceSpec.ts" />
/// <reference path="scribe.ts" />

namespace KMWRecorder {
  export abstract class InputEventSpec {
    type: "key" | "osk";
    static fromJSONObject(obj: any): InputEventSpec {
      if(obj && obj.type) {
        if(obj.type == "key") {
          return new PhysicalInputEventSpec(obj);
        } else if(obj.type == "osk") {
          return new OSKInputEventSpec(obj);
        }
      } else {
        throw new SyntaxError("Error in JSON format corresponding to an InputEventSpec!");
      }
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

    constructor(e?: PhysicalInputEventSpec) { // parameter is used to reconstruct from JSON.
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

  export class InputEventSpecSequence {
    inputs: InputEventSpec[];
    output: string;
    msg?: string;

    constructor(ins?: InputEventSpec[] | InputEventSpecSequence, outs?: string, msg?: string) {
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

    test(proctor: BrowserProctor): {success: boolean, result: string} {
      proctor.before();

      let result = proctor.simulateSequence(this);
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

    hasOSKInteraction(): boolean {
      for(var i=0; i < this.inputs.length; i++) {
        if(this.inputs[i] instanceof OSKInputEventSpec) {
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
  type BROWSER = 'ie'|'chrome'|'firefox'|'safari'|'opera';  // ! no 'edge' detection in KMW!

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

    matchesClient(device: com.keyman.text.EngineDeviceSpec, usingOSK?: boolean) {
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

  export class EventSpecTestSet {
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
    isValidForDevice(device: com.keyman.text.EngineDeviceSpec, usingOSK?: boolean) {
      return this.constraint.matchesClient(device, usingOSK);
    }

    // Validity should be checked before calling this method.
    test(proctor: BrowserProctor): TestFailure[] {
      var failures: TestFailure[] = [];
      let testSet = this.testSet;

      for(var i=0; i < testSet.length; i++) {
        var testSeq = this[i];
        var simResult = testSet[i].test(proctor);
        if(!simResult.success) {
          // Failed test!
          failures.push(new TestFailure(this.constraint, testSeq, simResult.result));
        }
      }

      return failures.length > 0 ? failures : null;
    }
  }

  export class KeyboardTest {
    /**
     * The stub information to be passed into keyman.addKeyboards() in order to run the test.
     */
    keyboard: KeyboardStub;

    /**
     * The master array of test sets, each of which specifies constraints a client must fulfill for
     * the tests contained therein to be valid.
     */
    inputTestSets: EventSpecTestSet[];

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
        fromJSON = JSON.parse(fromJSON);
      } else if(fromJSON instanceof KeyboardStub) {
        this.keyboard = fromJSON;
        this.inputTestSets = [];
        return;
      }

      fromJSON = fromJSON as KeyboardTest;

      this.keyboard = new KeyboardStub(fromJSON.keyboard);
      this.inputTestSets = [];

      for(var i=0; i < fromJSON.inputTestSets.length; i++) {
        this.inputTestSets[i] = new EventSpecTestSet(fromJSON.inputTestSets[i]);
      }
    }

    addTest(constraint: Constraint, seq: InputEventSpecSequence) {
      for(var i=0; i < this.inputTestSets.length; i++) {
        if(this.inputTestSets[i].constraint.equals(constraint)) {
          this.inputTestSets[i].addTest(seq);
          return;
        }
      }

      var newSet = new EventSpecTestSet(new Constraint(constraint));
      this.inputTestSets.push(newSet);
      newSet.addTest(seq);      
    }

    test(proctor: BrowserProctor) {
      var setHasRun = false;
      var failures: TestFailure[] = [];

      proctor.beforeAll();

      for(var i = 0; i < this.inputTestSets.length; i++) {
        var testSet = this.inputTestSets[i];

        if(proctor.matchesTestSet(testSet)) {
          var testFailures = testSet.test(proctor);
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
  }
}