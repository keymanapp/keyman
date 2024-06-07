
import { EventEmitter } from "eventemitter3";

import type { KeyEvent } from "@keymanapp/keyboard-processor";

import {
  Constraint,
  InputEventSpec,
  KeyboardTest,
  KeyboardStub,
  LanguageStubForKeyboard,
  OSKInputEventSpec,
  PhysicalInputEventSpec,
  RecordedKeystroke,
  RecordedKeystrokeSequence,
  RecordedPhysicalKeystroke,
  RecordedSyntheticKeystroke
} from "@keymanapp/recorder-core";

import { outputTargetForElement } from 'keyman/engine/attachment';

import { type KeymanEngine } from 'keyman/app/browser';

declare var keyman: KeymanEngine;

// // Makes sure the code below knows that the namespaces exist.
// namespace com.keyman {
//   export namespace dom {
//     export declare var DOMEventHandlers: any;
//     export declare class Utils {
//       static getOutputTarget(elem: HTMLElement): any; // text.OutputTarget;
//     }
//   }

//   export namespace osk {
//     export declare var PreProcessor: any;
//     export declare var VisualKeyboard: any;
//   }
// }

/**
 * Contains browser-dependent code used to transcribe browser-based events
 * so that thay may be reconstructed for use in KMW testing.
 */
export class Scribe extends EventEmitter {
  //#region Static methods for recording input events
  static recordKeystroke(e: KeyEvent, eventSpec: InputEventSpec) {
    var recording: RecordedKeystroke;
    if(e.isSynthetic) {
      recording = new RecordedSyntheticKeystroke(e);
    } else {
      recording = new RecordedPhysicalKeystroke(e, eventSpec as PhysicalInputEventSpec);
    }

    return recording;
  }

  static recordKeyboardEvent(e: KeyboardEvent): PhysicalInputEventSpec {
    let recording = new PhysicalInputEventSpec();

    recording.key    = e.key;
    recording.code    = e.code;
    recording.keyCode = e.keyCode;
    recording.location = e.location;

    var flagSet: number = 0;

    for(var key in PhysicalInputEventSpec.modifierCodes) {
      if(e.getModifierState(key)) {
        flagSet |= PhysicalInputEventSpec.modifierCodes[key];
      }
    }

    recording.modifierSet = flagSet;

    return recording;
  }

  static recordOSKEvent(e: HTMLDivElement): OSKInputEventSpec {
    let recording = new OSKInputEventSpec();
    recording.keyID = e.id;
    return recording;
  }

  static recordKeyboardStub(activeStub: any, basePath: string): KeyboardStub {
    let recording = new KeyboardStub();

    recording.id = activeStub.KI;
    recording.id = recording.id.replace('Keyboard_', '');

    recording.name = activeStub.KN;
    recording.filename = Scribe._setStubBasePath(activeStub.KF, basePath);

    recording.languages = [new LanguageStubForKeyboard(activeStub)];

    return recording;
  }

  private static _setStubBasePath(filename: string, filePath: string, force?: boolean): string {
    var linkParser = document.createElement<"a">("a");
    linkParser.href = filePath;

    if(force === undefined) {
      force = true;
    }

    if(force || (filename.indexOf(linkParser.protocol) < 0 && filename.indexOf('/') != 0)) {
      var file = filename.substr(filename.lastIndexOf('/')+1);
      return filePath + '/' + file;
    } else {
      return file;
    }
  }
  //#endregion

  _currentSequence: RecordedKeystrokeSequence = new RecordedKeystrokeSequence();
  _testDefinition: KeyboardTest = new KeyboardTest();

  keyboardJustActivated: boolean = false;

  get currentSequence(): RecordedKeystrokeSequence {
    return this._currentSequence;
  }

  set currentSequence(value: RecordedKeystrokeSequence) {
    this._currentSequence = value;
    this.raiseRecordChanged();
  }

  get testDefinition(): KeyboardTest {
    return this._testDefinition;
  }

  set testDefinition(value: KeyboardTest) {
    if(!value) {
      // It should never be null or undefined.
      throw new Error("Scribe's test definition must never be null or undefined!");
    }
    this._testDefinition = value;
    this.raiseTestChanged();
  }

  addInputRecord(json: RecordedKeystroke, currentOutput: string) {
    this.currentSequence.addInput(json, currentOutput);
    this.raiseRecordChanged();
  }

  resetInputRecord() {
    keyman.resetContext();
    this.currentSequence = new RecordedKeystrokeSequence();

    this.emit('record-reset', null);
  }

  errorUpdate(msg: string) {
    if(msg) {
      this.currentSequence.msg = msg;
    } else {
      delete this.currentSequence.msg;
    }

    this.raiseRecordChanged();
  }

  private raiseRecordChanged() {
    this.emit('record-changed', this.currentSequence.toPrettyJSON());
  }

  saveInputRecord(config: Constraint) {
    if(this.currentSequence.inputs.length > 0) {
      this.testDefinition.addTest(config, this.currentSequence);
    }
    this.resetInputRecord();
    this.raiseTestChanged();
  }

  private raiseTestChanged() {
    this.emit('test-changed', this.testDefinition ? this.testDefinition.toPrettyJSON() : '');
  }

  // Time for the 'magic'.  Yay, JavaScript method extension strategies...
  initHooks(recordingElement: HTMLElement) {
    let recorderScribe = this;

    keyman.hardKeyboard.on('keyevent', (e) => {
      let in_output = outputTargetForElement(recordingElement);
      if(!in_output || keyman.contextManager.activeTarget != in_output) {
        return;
      }

      let event = Scribe.recordKeyboardEvent(e.source as KeyboardEvent);
      let recording = Scribe.recordKeystroke(e, event);

      // Record the keystroke as part of a test sequence!
      // Miniature delay in case the keyboard relies upon default backspace/delete behavior!
      window.setTimeout(function() {
        recorderScribe.addInputRecord(recording, in_output.getText());
      }, 1);
    });

    keyman.osk.on('keyevent', (e) => {
      let in_output = outputTargetForElement(recordingElement);
      if(!in_output || keyman.contextManager.activeTarget != in_output) {
        return;
      }

      let event = Scribe.recordOSKEvent((e.source).target);
      let recording = Scribe.recordKeystroke(e, event);

      // Record the click/touch as part of a test sequence!
      recorderScribe.addInputRecord(recording, in_output.getText());
    });

    keyman.contextManager.on('keyboardchange', (kbdTuple) => {
      let in_output = outputTargetForElement(recordingElement);
      // If it's not on our recording control, ignore the change and do nothing special.
      if(!in_output || document.activeElement != in_output.getElement()) {
        return;
      }

      let testDefinition = recorderScribe.testDefinition;
      var sameKbd = (testDefinition.keyboard && ("Keyboard_" + testDefinition.keyboard.id) == kbdTuple.keyboard.id)
        && (!testDefinition.keyboard.getFirstLanguage() || (testDefinition.keyboard.getFirstLanguage() == kbdTuple.metadata.langId));

      // if(!testDefinition.isEmpty() && !sameKbd && !recorderScribe.keyboardJustActivated) {
      //   if(!confirm("Changing the keyboard will clear the current test set.  Are you sure?")) {
      //     return _originalSetActiveKeyboard("Keyboard_" + testDefinition.keyboard.id, testDefinition.keyboard.languages[0].id);
      //   }
      // }

      // What's the active stub immediately after our _SetActiveKeyboard call?
      var internalStub = kbdTuple.metadata;
      if(internalStub && (keyman.contextManager.activeTarget == in_output)) {
        var kbdRecord = Scribe.recordKeyboardStub(internalStub, 'resources/keyboards');

        recorderScribe.emit('stub-changed', JSON.stringify(kbdRecord));
        // var ta_activeStub = document.getElementById('activeStub');
        // ta_activeStub.value = JSON.stringify(kbdRecord);

        if(!sameKbd && !recorderScribe.keyboardJustActivated) {
          recorderScribe.testDefinition = new KeyboardTest(kbdRecord);
        }
      }
      recorderScribe.keyboardJustActivated = false;
    });
  }
}