/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-11-10
 *
 * Tests for KMC KMN Next Generation Parser
 *
 * NodeTypes for the Recursive Descent Parser
 */

import 'mocha';
import { assert } from 'chai';
import { NodeType } from '../../src/ng-compiler/node-type.js';

type NodeTypeKey = keyof typeof NodeType;

describe("NodeType Tests", () => {
  it("is sorted in alphabetical order", () => {
    const keys = Object.keys(NodeType) as NodeTypeKey[];
    const sortedKeys = [...keys].sort();
    assert.deepEqual(keys, sortedKeys);
  });
  it("has enum keys that exactly match their enum values", () => {
    for(const key of (Object.keys(NodeType) as NodeTypeKey[])) {
      assert.equal(NodeType[key], key);
    }
  });
});
