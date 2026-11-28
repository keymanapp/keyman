/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2026-11-26
 *
 * KMC KMN Next Generation Semantic Model Builder
 *
 * Tests for the KMX enums for the Semantic Model Builder (KmxBuilder)
 */

import 'mocha';
import { assert } from 'chai';
import { VKeyName } from '../../src/ng-compiler/kmx-constants.js';

type VKeyNameKey = keyof typeof VKeyName;

describe("KMX Constants Tests", () => {
  describe("VKeyName Tests", () => {
    it("has enum keys that exactly match their enum values", () => {
      for(const key of (Object.keys(VKeyName) as VKeyNameKey[])) {
        assert.equal(VKeyName[key], key);
      }
    });
  });
});