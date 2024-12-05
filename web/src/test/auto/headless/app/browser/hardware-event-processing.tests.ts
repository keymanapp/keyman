import { assert } from 'chai';

import { preprocessKeyboardEvent } from 'keyman/app/browser';
import { processForMnemonicsAndLegacy } from 'keyman/engine/main';
import { PhysicalInputEventSpec } from '@keymanapp/recorder-core';
import { DeviceSpec } from '@keymanapp/web-utils';
import { Codes, JSKeyboard, KeyEvent } from 'keyman/engine/keyboard';

const ModifierCodes = Codes.modifierCodes;
const KeyCodes = Codes.keyCodes;

const DUMMY_DEVICE = new DeviceSpec('chrome', 'desktop', 'windows', false);

// Compare and contrast the unit tests here with those for keyboard unit testing
// in the non-positional-rules set; the output objects here should have the same format
// as the inputs for rules as used there.

describe("app/browser: hardware event processing", () => {
  describe('preprocessKeyboardEvent', () => {
    describe('positional keyboards', () => {
      it("simple shifted 'a'", () => {
        const processedEvent = preprocessKeyboardEvent(new PhysicalInputEventSpec({
          keyCode: KeyCodes.K_A,
          modifierSet: PhysicalInputEventSpec.modifierCodes.Shift,
          location: 1
        }) as any as KeyboardEvent, {
          activeKeyboard: new JSKeyboard({ KM: 0 }),
          modStateFlags: 0,
          baseLayout: 'us'
        },
          DUMMY_DEVICE
        );

        assert.isTrue(processedEvent.LisVirtualKey);
        assert.equal(processedEvent.Lcode, KeyCodes.K_A);
        assert.equal(processedEvent.Lmodifiers, ModifierCodes.SHIFT);
        assert.equal(processedEvent.Lstates, ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK);

        // Note:  Codes.modifierCodes.VIRTUAL_KEY is NOT applied here.  It's applied by the
        // compiler & filtered out directly within the `keyMatch` keyboard-API func.  It's
        // never part of the actual processed event object within KMW.
      });

      it("simple unshifted 'a'", () => {
        const processedEvent = preprocessKeyboardEvent(new PhysicalInputEventSpec({
          keyCode: KeyCodes.K_A,
          modifierSet: 0
        }) as any as KeyboardEvent, {
          activeKeyboard: new JSKeyboard({ KM: 0 }),
          modStateFlags: 0,
          baseLayout: 'us'
        },
          DUMMY_DEVICE
        );

        assert.isTrue(processedEvent.LisVirtualKey);
        assert.equal(processedEvent.Lcode, KeyCodes.K_A);
        assert.equal(processedEvent.Lmodifiers, 0);
        assert.equal(processedEvent.Lstates, ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK);
      });

      it("left-alt press", () => {
        const processedEvent = preprocessKeyboardEvent(new PhysicalInputEventSpec({
          keyCode: KeyCodes.K_ALT,
          modifierSet: PhysicalInputEventSpec.modifierCodes.Alt,
          location: 1
        }) as any as KeyboardEvent, {
          // KMBM:  Keyman Modifier BitMask
          activeKeyboard: new JSKeyboard({KM: 0}),
          modStateFlags: 0,
          baseLayout: 'us'
        },
          DUMMY_DEVICE
        );

        assert.isTrue(processedEvent.LisVirtualKey);
        assert.equal(processedEvent.Lcode, KeyCodes.K_ALT);
        assert.equal(processedEvent.Lmodifiers, ModifierCodes.ALT);
        assert.equal(processedEvent.Lstates, ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK);
      });

      it("left-alt release", () => {
        const processedEvent = preprocessKeyboardEvent(new PhysicalInputEventSpec({
          keyCode: KeyCodes.K_ALT,
          modifierSet: 0,
          location: 1
        }) as any as KeyboardEvent, {
          // KMBM:  Keyman Modifier BitMask
          activeKeyboard: new JSKeyboard({KM: 0}),
          modStateFlags: ModifierCodes.ALT,
          baseLayout: 'us'
        },
          DUMMY_DEVICE
        );

        assert.isTrue(processedEvent.LisVirtualKey);
        assert.equal(processedEvent.Lcode, KeyCodes.K_ALT);
        assert.equal(processedEvent.Lmodifiers, 0);
        assert.equal(processedEvent.Lstates, ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK);
      });
    });

    describe('positional+chiral keyboards', () => {
      it("simple shifted 'a'", () => {
        const processedEvent = preprocessKeyboardEvent(new PhysicalInputEventSpec({
          keyCode: KeyCodes.K_A,
          modifierSet: PhysicalInputEventSpec.modifierCodes.Shift,
          location: 1
        }) as any as KeyboardEvent, {
          // KMBM:  Keyman Modifier BitMask
          activeKeyboard: new JSKeyboard({KM:0, KMBM: Codes.modifierBitmasks.CHIRAL}),
          modStateFlags: 0,
          baseLayout: 'us'
        },
          DUMMY_DEVICE
        );

        assert.isTrue(processedEvent.LisVirtualKey);
        assert.equal(processedEvent.Lcode, KeyCodes.K_A);
        assert.equal(processedEvent.Lmodifiers, ModifierCodes.SHIFT);
        assert.equal(processedEvent.Lstates, ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK);
      });

      it("simple unshifted 'a'", () => {
        const processedEvent = preprocessKeyboardEvent(new PhysicalInputEventSpec({
          keyCode: KeyCodes.K_A,
          modifierSet: 0
        }) as any as KeyboardEvent, {
          // KMBM:  Keyman Modifier BitMask
          activeKeyboard: new JSKeyboard({KM: 0, KMBM: Codes.modifierBitmasks.CHIRAL}),
          modStateFlags: 0,
          baseLayout: 'us'
        },
          DUMMY_DEVICE
        );

        assert.isTrue(processedEvent.LisVirtualKey);
        assert.equal(processedEvent.Lcode, KeyCodes.K_A);
        assert.equal(processedEvent.Lmodifiers, 0);
        assert.equal(processedEvent.Lstates, ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK);
      });

      it("left-alt press", () => {
        const processedEvent = preprocessKeyboardEvent(new PhysicalInputEventSpec({
          keyCode: KeyCodes.K_ALT,
          modifierSet: PhysicalInputEventSpec.modifierCodes.Alt,
          location: 1
        }) as any as KeyboardEvent, {
          // KMBM:  Keyman Modifier BitMask
          activeKeyboard: new JSKeyboard({KM: 0, KMBM: Codes.modifierBitmasks.CHIRAL}),
          modStateFlags: 0,
          baseLayout: 'us'
        },
          DUMMY_DEVICE
        );

        assert.isTrue(processedEvent.LisVirtualKey);
        assert.equal(processedEvent.Lcode, KeyCodes.K_ALT);
        assert.equal(processedEvent.Lmodifiers, ModifierCodes.LALT);
        assert.equal(processedEvent.Lstates, ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK);
      });

      it("left-alt release", () => {
        const processedEvent = preprocessKeyboardEvent(new PhysicalInputEventSpec({
          keyCode: KeyCodes.K_ALT,
          modifierSet: 0,
          location: 1
        }) as any as KeyboardEvent, {
          // KMBM:  Keyman Modifier BitMask
          activeKeyboard: new JSKeyboard({KM: 0, KMBM: Codes.modifierBitmasks.CHIRAL}),
          modStateFlags: ModifierCodes.LALT,
          baseLayout: 'us'
        },
          DUMMY_DEVICE
        );

        assert.isTrue(processedEvent.LisVirtualKey);
        assert.equal(processedEvent.Lcode, KeyCodes.K_ALT);
        assert.equal(processedEvent.Lmodifiers, 0);
        assert.equal(processedEvent.Lstates, ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK);
      });
    });

    describe('mnemonic keyboards', () => {
      it("simple shifted 'a'", () => {
        const processedEvent = preprocessKeyboardEvent(new PhysicalInputEventSpec({
          keyCode: KeyCodes.K_A,
          modifierSet: PhysicalInputEventSpec.modifierCodes.Shift,
          location: 1
        }) as any as KeyboardEvent, {
          activeKeyboard: new JSKeyboard({ KM: 1 }),
          modStateFlags: 0,
          baseLayout: 'us'
        },
          DUMMY_DEVICE
        );

        assert.isTrue(processedEvent.LisVirtualKey);
        assert.equal(processedEvent.Lcode, 'A'.charCodeAt(0));
        assert.equal(processedEvent.Lmodifiers, ModifierCodes.SHIFT);
        assert.equal(processedEvent.Lstates, ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK);
      });

      it("simple unshifted 'a'", () => {
        const processedEvent = preprocessKeyboardEvent(new PhysicalInputEventSpec({
          keyCode: KeyCodes.K_A,
          modifierSet: 0
        }) as any as KeyboardEvent, {
          activeKeyboard: new JSKeyboard({ KM: 1 }),
          modStateFlags: 0,
          baseLayout: 'us'
        },
          DUMMY_DEVICE
        );

        assert.isTrue(processedEvent.LisVirtualKey);
        assert.equal(processedEvent.Lcode, 'a'.charCodeAt(0));
        assert.equal(processedEvent.Lmodifiers, 0);
        assert.equal(processedEvent.Lstates, ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK);
      });
    });

    describe('legacy keyboards', () => {
      it("simple shifted 'a'", () => {
        const processedEvent = preprocessKeyboardEvent(new PhysicalInputEventSpec({
          keyCode: KeyCodes.K_A,
          modifierSet: PhysicalInputEventSpec.modifierCodes.Shift,
          location: 1
        }) as any as KeyboardEvent, {
          activeKeyboard: new JSKeyboard({ KM: undefined }),
          modStateFlags: 0,
          baseLayout: 'us'
        },
          DUMMY_DEVICE
        );

        assert.isFalse(processedEvent.LisVirtualKey);
        assert.equal(processedEvent.Lcode, 'A'.charCodeAt(0));
        assert.equal(processedEvent.Lmodifiers, 0);
        assert.equal(processedEvent.Lstates, ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK);
      });

      it("simple unshifted 'a'", () => {
        const processedEvent = preprocessKeyboardEvent(new PhysicalInputEventSpec({
          keyCode: KeyCodes.K_A,
          modifierSet: 0
        }) as any as KeyboardEvent, {
          activeKeyboard: new JSKeyboard({ KM: undefined }),
          modStateFlags: 0,
          baseLayout: 'us'
        },
          DUMMY_DEVICE
        );

        assert.isFalse(processedEvent.LisVirtualKey);
        assert.equal(processedEvent.Lcode, 'a'.charCodeAt(0));
        assert.equal(processedEvent.Lmodifiers, 0);
        assert.equal(processedEvent.Lstates, ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK);
      });

      it("'a' during left-alt", () => {
        const processedEvent = preprocessKeyboardEvent(new PhysicalInputEventSpec({
          keyCode: KeyCodes.K_A,
          modifierSet: PhysicalInputEventSpec.modifierCodes.Alt,
          location: 0
        }) as any as KeyboardEvent, {
          activeKeyboard: new JSKeyboard({ KM: undefined }),
          // Internally assumes that the ALT-press event was previously handled, with this as
          // the resulting modifier state at the time of key-press.
          modStateFlags: ModifierCodes.LALT,
          baseLayout: 'us'
        },
          DUMMY_DEVICE
        );

        // ... which is used to disable the key within the rule-processing `keyMatch` function, as
        // the keyboard's rules weren't compiled with the Codes.VIRTUAL_KEY bit set.
        assert.isTrue(processedEvent.LisVirtualKey);

        assert.equal(processedEvent.Lcode, KeyCodes.K_A);
        assert.equal(processedEvent.Lmodifiers, ModifierCodes.ALT);
        assert.equal(processedEvent.Lstates, ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK);
      });
    });
  });

  describe('processForMnemonicsAndLegacy', () => {
    describe('positional keyboards', () => {
      it("simple shifted 'a'", () => {
        const baseKeyEvent = new KeyEvent({
          Lcode: KeyCodes.K_A,
          Lmodifiers: ModifierCodes.SHIFT,
          Lstates: ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK,
          LisVirtualKey: true,
          device: DUMMY_DEVICE,
          kName: "K_A"
        });

        const finalKeyEvent = processForMnemonicsAndLegacy(
          baseKeyEvent,
          new JSKeyboard({KM: 0}),
          'us'
        );

        assert.strictEqual(finalKeyEvent, baseKeyEvent);
      });

      it("simple unshifted 'a'", () => {
        const baseKeyEvent = new KeyEvent({
          Lcode: KeyCodes.K_A,
          Lmodifiers: 0,
          Lstates: ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK,
          LisVirtualKey: true,
          device: DUMMY_DEVICE,
          kName: "K_A"
        });

        const finalKeyEvent = processForMnemonicsAndLegacy(
          baseKeyEvent,
          new JSKeyboard({KM: 0}),
          'us'
        );

        assert.strictEqual(finalKeyEvent, baseKeyEvent);
      });

      it("left-alt press", () => {
        const baseKeyEvent = new KeyEvent({
          Lcode: KeyCodes.K_ALT,
          Lmodifiers: ModifierCodes.ALT,
          Lstates: ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK,
          LisVirtualKey: true,
          device: DUMMY_DEVICE,
          kName: "K_ALT"
        });

        const finalKeyEvent = processForMnemonicsAndLegacy(
          baseKeyEvent,
          new JSKeyboard({KM: 0}),
          'us'
        );

        assert.strictEqual(finalKeyEvent, baseKeyEvent);
      });

      it("left-alt release", () => {
        const baseKeyEvent = new KeyEvent({
          Lcode: KeyCodes.K_ALT,
          Lmodifiers: 0,
          Lstates: ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK,
          LisVirtualKey: true,
          device: DUMMY_DEVICE,
          kName: "K_A"
        });

        const finalKeyEvent = processForMnemonicsAndLegacy(
          baseKeyEvent,
          new JSKeyboard({KM: 0}),
          'us'
        );

        assert.strictEqual(finalKeyEvent, baseKeyEvent);
      });
    });

    describe('positional+chiral keyboards', () => {
      it("simple shifted 'a'", () => {
        const baseKeyEvent = new KeyEvent({
          Lcode: KeyCodes.K_A,
          Lmodifiers: ModifierCodes.SHIFT,
          Lstates: ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK,
          LisVirtualKey: true,
          device: DUMMY_DEVICE,
          kName: "K_A"
        });

        const finalKeyEvent = processForMnemonicsAndLegacy(
          baseKeyEvent,
          new JSKeyboard({KM:0, KMBM: Codes.modifierBitmasks.CHIRAL}),
          'us'
        );

        assert.strictEqual(finalKeyEvent, baseKeyEvent);
      });

      it("simple unshifted 'a'", () => {
        const baseKeyEvent = new KeyEvent({
          Lcode: KeyCodes.K_A,
          Lmodifiers: 0,
          Lstates: ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK,
          LisVirtualKey: true,
          device: DUMMY_DEVICE,
          kName: "K_A"
        });

        const finalKeyEvent = processForMnemonicsAndLegacy(
          baseKeyEvent,
          new JSKeyboard({KM:0, KMBM: Codes.modifierBitmasks.CHIRAL}),
          'us'
        );

        assert.strictEqual(finalKeyEvent, baseKeyEvent);
      });

      it("left-alt press", () => {
        const baseKeyEvent = new KeyEvent({
          Lcode: KeyCodes.K_ALT,
          Lmodifiers: ModifierCodes.LALT,
          Lstates: ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK,
          LisVirtualKey: true,
          device: DUMMY_DEVICE,
          kName: "K_ALT"
        });

        const finalKeyEvent = processForMnemonicsAndLegacy(
          baseKeyEvent,
          new JSKeyboard({KM:0, KMBM: Codes.modifierBitmasks.CHIRAL}),
          'us'
        );

        assert.strictEqual(finalKeyEvent, baseKeyEvent);
      });

      it("left-alt release", () => {
        const baseKeyEvent = new KeyEvent({
          Lcode: KeyCodes.K_LALT,
          Lmodifiers: 0,
          Lstates: ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK,
          LisVirtualKey: true,
          device: DUMMY_DEVICE,
          kName: "K_A"
        });

        const finalKeyEvent = processForMnemonicsAndLegacy(
          baseKeyEvent,
          new JSKeyboard({KM:0, KMBM: Codes.modifierBitmasks.CHIRAL}),
          'us'
        );

        assert.strictEqual(finalKeyEvent, baseKeyEvent);
      });
    });

    describe('mnemonic keyboards', () => {
      it("simple shifted 'a'", () => {
        const baseKeyEvent = new KeyEvent({
          Lcode: KeyCodes.K_A,
          Lmodifiers: ModifierCodes.SHIFT,
          Lstates: ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK,
          LisVirtualKey: true,
          device: DUMMY_DEVICE,
          kName: "K_A"
        });

        const finalKeyEvent = processForMnemonicsAndLegacy(
          baseKeyEvent,
          new JSKeyboard({KM:1}),
          'us'
        );

        // May be the same instance, but the instance gets mutated.  Compare against literals
        // for expected values.
        assert.equal(finalKeyEvent.Lcode, 'A'.charCodeAt(0));
        assert.equal(finalKeyEvent.Lmodifiers, ModifierCodes.SHIFT);
        assert.isTrue(finalKeyEvent.LisVirtualKey);
      });

      it("simple unshifted 'a'", () => {
        const baseKeyEvent = new KeyEvent({
          Lcode: KeyCodes.K_A,
          Lmodifiers: 0,
          Lstates: ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK,
          LisVirtualKey: true,
          device: DUMMY_DEVICE,
          kName: "K_A"
        });

        const finalKeyEvent = processForMnemonicsAndLegacy(
          baseKeyEvent,
          new JSKeyboard({KM:1}),
          'us'
        );

        // May be the same instance, but the instance gets mutated.  Compare against literals
        // for expected values.
        assert.equal(finalKeyEvent.Lcode, 'a'.charCodeAt(0));
        assert.equal(finalKeyEvent.Lmodifiers, 0);
        assert.isTrue(finalKeyEvent.LisVirtualKey);
      });
    });

    describe('legacy keyboards', () => {

      it("simple shifted 'a'", () => {
        const baseKeyEvent = new KeyEvent({
          Lcode: KeyCodes.K_A,
          Lmodifiers: ModifierCodes.SHIFT,
          Lstates: ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK,
          LisVirtualKey: true,
          device: DUMMY_DEVICE,
          kName: "K_A"
        });

        const finalKeyEvent = processForMnemonicsAndLegacy(
          baseKeyEvent,
          new JSKeyboard({KM:undefined}),
          'us'
        );

        // The event object gets wholesale-replaced.
        assert.notStrictEqual(finalKeyEvent, baseKeyEvent);

        assert.isFalse(finalKeyEvent.LisVirtualKey);
        assert.equal(finalKeyEvent.Lcode, 'A'.charCodeAt(0));
        assert.equal(finalKeyEvent.Lmodifiers, 0);
      });

      it("simple unshifted 'a'", () => {
        const baseKeyEvent = new KeyEvent({
          Lcode: KeyCodes.K_A,
          Lmodifiers: 0,
          Lstates: ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK,
          LisVirtualKey: true,
          device: DUMMY_DEVICE,
          kName: "K_A"
        });

        const finalKeyEvent = processForMnemonicsAndLegacy(
          baseKeyEvent,
          new JSKeyboard({KM:undefined}),
          'us'
        );

        // The event object gets wholesale-replaced.
        assert.notStrictEqual(finalKeyEvent, baseKeyEvent);

        assert.isFalse(finalKeyEvent.LisVirtualKey);
        assert.equal(finalKeyEvent.Lcode, 'a'.charCodeAt(0));
        assert.equal(finalKeyEvent.Lmodifiers, 0);
      });

      it("'a' during alt", () => {
        const baseKeyEvent = new KeyEvent({
          Lcode: KeyCodes.K_A,
          Lmodifiers: ModifierCodes.ALT,
          Lstates: ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK,
          LisVirtualKey: true,
          device: DUMMY_DEVICE,
          kName: "K_A"
        });

        const finalKeyEvent = processForMnemonicsAndLegacy(
          baseKeyEvent,
          new JSKeyboard({KM:undefined}),
          'us'
        );

        // KMW 1.0 keyboards expect this to be false; setting it to true prevents the keyboard
        // from matching the event to a rule.  As KMW 1.0 keyboards shouldn't handle modifiers
        // other than shift, this acts as a filter to prevent rule processing for the unsupported
        // modifier.
        assert.isTrue(finalKeyEvent.LisVirtualKey);
        assert.equal(finalKeyEvent.Lcode, KeyCodes.K_A);
        assert.equal(finalKeyEvent.Lmodifiers, ModifierCodes.ALT);
      });

      it("'a' during left-alt (invalid state)", () => {
        // Not that such a state should ever occur, but if it did... we can ensure it's
        // handled robustly.
        const baseKeyEvent = new KeyEvent({
          Lcode: KeyCodes.K_A,
          Lmodifiers: ModifierCodes.LALT,
          Lstates: ModifierCodes.NO_CAPS | ModifierCodes.NO_NUM_LOCK | ModifierCodes.NO_SCROLL_LOCK,
          LisVirtualKey: true,
          device: DUMMY_DEVICE,
          kName: "K_A"
        });

        const finalKeyEvent = processForMnemonicsAndLegacy(
          baseKeyEvent,
          new JSKeyboard({KM:undefined}),
          'us'
        );

        // KMW 1.0 keyboards expect this to be false; setting it to true prevents the keyboard
        // from matching the event to a rule.  As KMW 1.0 keyboards shouldn't handle modifiers
        // other than shift, this acts internally as a filter to prevent rule matching for
        // events with the unsupported modifier.
        assert.isTrue(finalKeyEvent.LisVirtualKey);
        assert.equal(finalKeyEvent.Lcode, KeyCodes.K_A);
        assert.equal(finalKeyEvent.Lmodifiers, ModifierCodes.LALT);
      });
    });
  });
});