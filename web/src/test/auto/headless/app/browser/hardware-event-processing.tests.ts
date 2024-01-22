import { assert } from 'chai';
import sinon from 'sinon';

import { preprocessKeyboardEvent } from 'keyman/app/browser';
import { processForMnemonicsAndLegacy } from 'keyman/engine/main';
import { PhysicalInputEventSpec } from '@keymanapp/recorder-core';
import { DeviceSpec } from '@keymanapp/web-utils';

describe("app/browser: hardware event processing", () => {
  describe('preprocessKeyboardEvent', () => {
    it('dummy', () => {
      let test = preprocessKeyboardEvent(new PhysicalInputEventSpec({
        keyCode: 20,
        modifierSet: PhysicalInputEventSpec.modifierCodes.Shift,
        location: 1
      }) as any as KeyboardEvent, {
        // Only needs to be non-null for chiral-keystroke detection and for mnemonic detection.
        activeKeyboard: null,
        modStateFlags: 0,
        baseLayout: 'us'
      },
        new DeviceSpec('chrome', 'desktop', 'windows', false)
      );

      console.log(JSON.stringify(test));
    });
  });
});