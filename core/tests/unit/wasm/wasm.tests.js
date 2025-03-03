import { assert } from 'chai';
import { fileURLToPath, pathToFileURL } from 'url';
import fs from 'fs';
import path from 'path';

let km_core = null;

describe('Unit tests for wasm Core API', function () {
  let __dirname = null;

  before(async function () {
    // Define __dirname for ES modules
    const filename = fileURLToPath(import.meta.url);
    __dirname = path.join(path.dirname(filename), '../../../build/wasm/debug');

    let km_core_file = path.join(__dirname, 'src/km-core-node.mjs');
    if (!fs.existsSync(km_core_file)) {
      __dirname = path.join(path.dirname(filename), '../../../build/wasm/release');
      km_core_file = path.join(__dirname, 'src/km-core-node.mjs');
    }
    const wasmdir = path.dirname(km_core_file);
    const module = await import(pathToFileURL(km_core_file));
    const createCoreProcessor = module.default;
    km_core = await createCoreProcessor({
      locateFile: function (path, scriptDirectory) {
        return wasmdir + '/' + path;
      }
    });
  });

  it('can load keyboard from blob', function () {
    // Setup
    const kmxdata = fs.readFileSync(path.join(__dirname, 'tests/unit/kmx/k_020___deadkeys_and_backspace.kmx'));

    // Execute
    const result = km_core.keyboard_load_from_blob('k_020___deadkeys_and_backspace', kmxdata);

    // Verify
    assert.equal(result.status, 0);
    assert.isOk(result.object);
  });

  function loadKeyboard(name) {
    const kmxdata = fs.readFileSync(path.join(__dirname, `tests/unit/kmx/${name}.kmx`));
    const result = km_core.keyboard_load_from_blob(name, kmxdata);
    assert.equal(result.status, 0);
    assert.isOk(result.object);
    return result.object;
  }

  it('can dispose keyboard', function () {
    // Setup
    const keyboard = loadKeyboard('k_020___deadkeys_and_backspace');

    // Execute
    km_core.keyboard_dispose(keyboard);
  });

  it('can get keyboard attributes', function () {
    // Setup
    const keyboard = loadKeyboard('k_022___options_with_preset');

    // Execute
    const result = km_core.keyboard_get_attrs(keyboard);

    // Verify
    assert.equal(result.status, 0);
    assert.isOk(result.object);
    assert.equal(result.object.id, 'k_022___options_with_preset');
    assert.equal(result.object.version_string, '0.0');
    assert.deepEqual(result.object.default_options, [{ key: 'foo', value: '0', scope: 1 }]);
  });

  it('can create the state object', function () {
    // Setup
    const keyboard = loadKeyboard('k_020___deadkeys_and_backspace');

    // Execute
    const result = km_core.state_create(keyboard, []);

    // Verify
    assert.equal(result.status, 0);
    assert.isOk(result.object);
  });

  it('can dispose the state object', function () {
    // Setup
    const keyboard = loadKeyboard('k_020___deadkeys_and_backspace');
    const state = km_core.state_create(keyboard, []);

    // Execute
    km_core.state_dispose(state.object);
  });

  it('can clone the state object', function () {
    // Setup
    const keyboard = loadKeyboard('k_020___deadkeys_and_backspace');
    const state = km_core.state_create(keyboard, []);

    // Execute
    const result = km_core.state_clone(state.object);

    // Verify
    assert.equal(result.status, 0);
    assert.isOk(result.object);
    // TODO-web-core: add deeper tests
  });

  it('can process event', function () {
    // Setup
    const keyboard = loadKeyboard('k_020___deadkeys_and_backspace');
    const state = km_core.state_create(keyboard, []).object;

    // Execute
    const status = km_core.process_event(state, 0x20, 0, 1, 0);

    // Verify
    assert.equal(status, 0);

    const actions = km_core.state_get_actions(state);
    assert.equal(actions.output, ' ');
    assert.lengthOf(actions.persist_options, 0);
    assert.equal(actions.do_alert, false);
    assert.equal(actions.emit_keystroke, false);
    assert.equal(actions.new_caps_lock_state, -1);
    assert.equal(actions.deleted_context, '');
  });

  it('can set the context', function () {
    const keyboard = loadKeyboard('k_020___deadkeys_and_backspace');
    const state = km_core.state_create(keyboard, []).object;

    // Execute
    const status = km_core.state_context_set_if_needed(state, 'abc');

    // Verify
    assert.equal(status, 1); // KM_CORE_CONTEXT_STATUS_UPDATED

    const context = km_core.state_context_debug(state, 0);
    assert.equal(context, '|abc| (len: 3) [ U+0061 U+0062 U+0063 ]');
  });
});
