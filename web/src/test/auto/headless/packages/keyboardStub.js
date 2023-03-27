import { assert } from 'chai';
import sinon from 'sinon';

import { KeyboardStub } from 'keyman/engine/package-cache';
import NodeCloudRequester from 'keyman/engine/package-cache/node-requester';

import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

describe("KeyboardStub", () => {
  function performMockedRequest(mockedResultsFile) {
    const requester = new NodeCloudRequester(true /* mocked mode:  local files */);

    /*
     * We aren't actually attaching to `CloudQueryEngine`, so we need two things:
     *
     * 1. A proper endpoint for the query results' `keyman.register` call.
     * 2. Resolution of the request's `Promise` (to prevent unwanted errors)
     */
    const mockedRegisterOwner = {
      // Query promises are stored for resolution by the registration function.
      registerFromCloud: sinon.spy(() => mockedRegisterOwner.promise.resolve())
    };
    requester.link(mockedRegisterOwner);

    const queryString = mockedResultsFile;
    const queryHandle = requester.request(queryString).promise;

    // Store the query's promise for resolution by our mocked register (as seen above).
    mockedRegisterOwner.promise = queryHandle;

    return {
      promise: queryHandle.corePromise,
      mockedRegister: mockedRegisterOwner.registerFromCloud
    };
  }

  it('merge(): barebones stub + fetched sil_euro_latin@no', async () => {
    const query = performMockedRequest(`${__dirname}/../../resources/query-mock-results/sil_euro_latin@no_sv.js.fixture`);
    await query.promise;

    assert.isTrue(query.mockedRegister.called);

    // Two keyboard-language pairings:  []`sil_euro_latin@no`, `sil_euro_latin@sv`]
    const queryResult = query.mockedRegister.firstCall.args[0];
    const queried_stub = KeyboardStub.toStubs(queryResult.keyboard[0])[0];

    const barebonesStub = new KeyboardStub('sil_euro_latin', 'no');

    assert.notDeepEqual(barebonesStub, queried_stub);
    barebonesStub.merge(queried_stub);
    assert.deepEqual(barebonesStub, queried_stub);

    assert.notStrictEqual(barebonesStub, queried_stub);
  });
});