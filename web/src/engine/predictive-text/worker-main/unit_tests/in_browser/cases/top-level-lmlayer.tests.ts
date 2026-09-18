import { assert } from 'chai';

import { LMLayer, webPredictiveTextWorkerFactory }  from "@keymanapp/lexical-model-layer/web";

import { DEFAULT_BROWSER_TIMEOUT } from '@keymanapp/common-test-resources/test-timeouts.mjs';
import { defaultCapabilities } from '../helpers.mjs';

describe('LMLayer', function () {
  this.timeout(DEFAULT_BROWSER_TIMEOUT);

  describe('[[constructor]]', function () {
    it('should construct with a single argument', function () {
      let lmLayer = new LMLayer(defaultCapabilities, webPredictiveTextWorkerFactory.constructTestInstance(), true);
      assert.instanceOf(lmLayer, LMLayer);
      lmLayer.shutdown();
    });
  });
});
