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

  it('construction from internal stub format, partially configured paths', () => {
    const rawStub = {
      KI: 'dummy',
      KN: 'test dummy',
      KL: 'English',
      KLC: 'en',
      KF: 'dummy.js',
      // The way font paths are currently handled feels pretty rough and unclear.
      // Their paths aren't updated in the same way as the KF entry.
      // So... leaving font stuff out of the test for now.
      // (Also, Developer doesn't seem to bother specifying font files in its stubs, so it's
      // less criitcal.)
    };

    const stub = new KeyboardStub(rawStub, 'http://localhost/keyboards/', 'http://localhost/fonts/');

    assert.equal(stub.KF, 'http://localhost/keyboards/dummy.js');
  });

  it('construction from internal stub format, pre-configured paths', () => {
    // Based on actual font pathing as hosted by the Android app.
    const absolutePath = '/data/user/0/com.tavultesoft.kmapro.debug/app_data/packages/dummy/dummy.ttf';

    const rawStub = {
      KI: 'dummy',
      KN: 'test dummy',
      KL: 'English',
      KLC: 'en',
      KF: absolutePath,
      // The way font paths are currently handled feels pretty rough and unclear.
      // Their paths aren't updated in the same way as the KF entry.
      // So... leaving font stuff out of the test for now.
      // (Also, Developer doesn't seem to bother specifying font files in its stubs, so it's
      // less criitcal.)
    };

    // These components... are not, but that's OK - the test is to ignore them.
    const stub = new KeyboardStub(rawStub, 'http://localhost/keyboards/', 'http://localhost/fonts/');

    assert.equal(stub.KF, absolutePath);
  });

  it('merge(): barebones stub + fetched sil_euro_latin@no', async () => {
    const query = performMockedRequest(`${__dirname}/../../../resources/query-mock-results/sil_euro_latin@no_sv.js.fixture`);
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