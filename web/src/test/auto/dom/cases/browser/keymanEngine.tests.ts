/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { KeymanEngine } from 'keyman/app/browser';
import { StubAndKeyboardCache } from 'keyman/engine/keyboard-storage';
import { assert } from 'chai';

const mockWorkerFactory = {
  constructInstance: (): null => null
};

describe('KeymanEngine.getKeyboardForControl', () => {
  let engine: KeymanEngine;
  let keyboardCache: StubAndKeyboardCache;

  beforeEach(() => {
    engine = new KeymanEngine(mockWorkerFactory, '');
    keyboardCache = new StubAndKeyboardCache();
    (engine as any).keyboardRequisitioner = {
      cache: keyboardCache
    };
  });

  it('returns null for controls in global mode', () => {
    const input = document.createElement('input');
    document.body.appendChild(input);
    engine.attachToControl(input);

    assert.isNull(engine.getKeyboardForControl(input));
  });

  it('returns empty string for explicit system-keyboard mode', () => {
    const input = document.createElement('input');
    document.body.appendChild(input);
    engine.attachToControl(input);

    engine.setKeyboardForControl(input, '', '');
    assert.equal(engine.getKeyboardForControl(input), '');
  });

  it('returns canonical prefixed ID after setting an unprefixed ID', () => {
    const stub = {
      KI: 'Keyboard_lao_2008_basic',
      KN: 'Lao 2008 Basic',
      KL: 'Lao',
      KLC: 'lo',
      KF: 'resources/keyboards/lao_2008_basic.js',
    } as any;
    keyboardCache.addStub(stub);

    const input = document.createElement('input');
    document.body.appendChild(input);
    engine.attachToControl(input);

    engine.setKeyboardForControl(input, 'lao_2008_basic', 'lo');
    assert.equal(engine.getKeyboardForControl(input), 'Keyboard_lao_2008_basic');
  });
});
