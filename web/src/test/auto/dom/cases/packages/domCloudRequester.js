import { assert } from '/node_modules/chai/chai.js';
import sinon from '/node_modules/sinon/pkg/sinon-esm.js';

import DOMCloudRequester from '/@keymanapp/keyman/build/engine/package-cache/lib/dom-cloud-requester.mjs';

describe("Mocked cloud query results ('canary' testing)", () => {
  function performMockedRequest(mockedResultsPath) {
    const requester = new DOMCloudRequester(true);

    /*
     * We aren't actually attaching to `CloudQueryEngine`, so we need two things:
     *
     * 1. A proper endpoint for the query results' `keyman.register` call.
     * 2. Resolution of the request's `Promise` (to prevent unwanted errors)
     */
    const mockedRegisterOwner = {
      // Query promises are stored for resolution by the registration function.
      register: sinon.spy(() => mockedRegisterOwner.promise.resolve())
    };
    // Must be available via keyman.register().
    window.keyman = mockedRegisterOwner;

    const queryString = mockedResultsPath;
    const queryHandle = requester.request(queryString).promise;

    // Store the query's promise for resolution by our mocked register (as seen above).
    mockedRegisterOwner.promise = queryHandle;

    return {
      promise: queryHandle.corePromise,
      mockedRegister: mockedRegisterOwner.register
    };
  }

  it('sil_euro_latin@no,sv', async () => {
    const query = performMockedRequest('base/web/src/test/auto/resources/query-mock-results/sil_euro_latin@no_sv.js.fixture');
    await query.promise;

    assert.isTrue(query.mockedRegister.called);

    const queryResult = query.mockedRegister.firstCall.args[0];
    assert.equal(queryResult.keyboard.length, 2);
    assert.equal(queryResult.keyboard[0].id, 'sil_euro_latin');
    assert.equal(queryResult.keyboard[1].id, 'sil_euro_latin');

    // Verify the languages object somewhat - since the request was per-id, we expect
    // single-language specs on each of the returned `keyboard` entries.
    assert.equal(queryResult.keyboard[0].languages.length, 1);
    assert.equal(queryResult.keyboard[1].languages.length, 1);

    const langCodes = ['no', 'sv'];
    assert.sameOrderedMembers(queryResult.keyboard.map((kbd) => kbd.languages[0].id), langCodes);
  });

  it('sil_cameroon_azerty', async () => {
    const query = performMockedRequest('base/web/src/test/auto/resources/query-mock-results/sil_cameroon_azerty.js.fixture');
    await query.promise;

    assert.isTrue(query.mockedRegister.called);

    const queryResult = query.mockedRegister.firstCall.args[0];
    assert.equal(queryResult.keyboard.length, 1);
    assert.equal(queryResult.keyboard[0].id, 'sil_cameroon_azerty');

    // Verify the languages object somewhat - since the request was per-id, we expect
    // single-language specs on each of the returned `keyboard` entries.
    assert.equal(queryResult.keyboard[0].languages.length, 278); //... that's a LOT.

    assert.isOk(queryResult.keyboard[0].languages.find((value) => value.id == 'pny'));
  });

  it('@dz', async () => {
    const query = performMockedRequest('base/web/src/test/auto/resources/query-mock-results/@dz.js.fixture');
    await query.promise;

    assert.isTrue(query.mockedRegister.called);

    const queryResult = query.mockedRegister.firstCall.args[0];
    assert.equal(queryResult.keyboard.length, 1);
    // For some reason, this cloud query nests keyboards inside two arrays, not one.
    const keyboards = queryResult.keyboard[0];
    assert.equal(keyboards.length, 7);

    const keyboardIds = [
      'basic_kbddzo',
      'tibetan_direct_input',
      'tibetan_ewts',
      'dzongkha',
      'tibetan_direct_unicode',
      'tibetan_unicode_direct_input',
      'tibetan_unicode_ewts'
    ];
    assert.sameOrderedMembers(keyboards.map((kbd) => kbd.id), keyboardIds);

    for(const keyboard of keyboards) {
      assert.isOk(keyboard.languages.find((language) => language.id == 'dz'));
    }
  });
});