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
    assert.isNotNull(result.keyboard);
  });
});
