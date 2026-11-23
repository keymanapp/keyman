/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { assert } from 'chai';
import sinon from 'sinon';
import { unitTestEndPoints } from 'keyman/engine/core-processor';
import { VariableStoreTestSerializer } from 'keyman/test/headless-resources';
import { KM_Core, KM_CORE_STATUS, KM_CORE_OPTION_SCOPE, type km_core_option_item } from 'keyman/engine/core-adapter';
import { coreurl } from '../loadKeyboardHelper.js';

describe('CoreKeyboardInterface tests', function () {
  let sandbox: sinon.SinonSandbox;
  const mockKeyboard = {
    keyboard: {} as any,
    id: 'test_keyboard',
    state: {} as any
  };

  beforeEach(async function () {
    sandbox = sinon.createSandbox();
    await KM_Core.createCoreProcessor(coreurl);
  });

  afterEach(function () {
    sandbox.restore();
  });

  describe('loadSerializedOptions', function () {
    it('returns empty array if there are no default options', function () {
      // Setup
      const mockAttrs = {
        object: {
          id: 'test_keyboard',
          version_string: '1.234',
          default_options: [] as km_core_option_item[],
        },
        status: KM_CORE_STATUS.OK,
        delete: sandbox.stub()
      };
      sandbox.stub(KM_Core.instance, 'keyboard_get_attrs').returns(mockAttrs as any);
      const keyboardInterface = new unitTestEndPoints.CoreKeyboardInterface(new VariableStoreTestSerializer());
      keyboardInterface['_activeKeyboard'] = mockKeyboard as any;

      // Execute
      const options = keyboardInterface['loadSerializedOptions']();

      // Verify
      assert.isArray(options);
      assert.lengthOf(options, 0);
    });

    it('returns no options if no options serialized', function () {
      // Setup
      const mockAttrs = {
        object: {
          id: 'test_keyboard',
          version_string: '1.234',
          default_options: [
            { key: 'opt1', value: 'val1', scope: KM_CORE_OPTION_SCOPE.OPT_KEYBOARD }
          ],
        },
        status: KM_CORE_STATUS.OK,
        delete: sandbox.stub(),
      };
      sandbox.stub(KM_Core.instance, 'keyboard_get_attrs').returns(mockAttrs as any);
      const keyboardInterface = new unitTestEndPoints.CoreKeyboardInterface(new VariableStoreTestSerializer());
      keyboardInterface['_activeKeyboard'] = mockKeyboard as any;

      // Execute
      const options = keyboardInterface['loadSerializedOptions']();

      // Verify
      assert.isArray(options);
      assert.lengthOf(options, 0);
    });

    it('returns serialized options from keyboard attrs', function () {
      // Setup
      const mockAttrs = {
        object: {
          id: 'test_keyboard',
          version_string: '1.234',
          default_options: [
            { key: 'opt1', value: 'default1', scope: KM_CORE_OPTION_SCOPE.OPT_KEYBOARD },
            { key: 'opt2', value: 'default2', scope: KM_CORE_OPTION_SCOPE.OPT_KEYBOARD }, // ignored because not serialized
            { key: 'opt3', value: 'default3', scope: KM_CORE_OPTION_SCOPE.OPT_KEYBOARD },
            { key: 'opt4', value: 'default4', scope: KM_CORE_OPTION_SCOPE.OPT_ENVIRONMENT },
          ],
        },
        status: KM_CORE_STATUS.OK,
        delete: sandbox.stub(),
      };
      sandbox.stub(KM_Core.instance, 'keyboard_get_attrs').returns(mockAttrs as any);
      const serializer = new VariableStoreTestSerializer();
      serializer.saveStore('Keyboard_test_keyboard', 'opt1', 'val1');
      serializer.saveStore('Keyboard_test_keyboard', 'opt3', '');
      serializer.saveStore('Keyboard_test_keyboard', 'opt4', 'val4'); // ignored since not keyboard-scoped
      serializer.saveStore('Keyboard_test_keyboard', 'opt5', 'val5'); // ignored since not in default options
      const keyboardInterface = new unitTestEndPoints.CoreKeyboardInterface(serializer);
      keyboardInterface['_activeKeyboard'] = mockKeyboard as any;

      // Execute
      const options = keyboardInterface['loadSerializedOptions']();

      // Verify
      assert.isArray(options);
      assert.lengthOf(options,2);
      assert.deepEqual(options[0], { key: 'opt1', value: 'val1', scope: KM_CORE_OPTION_SCOPE.OPT_KEYBOARD });
      assert.deepEqual(options[1], { key: 'opt3', value: '', scope: KM_CORE_OPTION_SCOPE.OPT_KEYBOARD });
    });
  });
});