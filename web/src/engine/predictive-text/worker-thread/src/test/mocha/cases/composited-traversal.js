import { assert } from 'chai';

import { CompositedTraversal } from '#./models/composited-traversal.js';
import { TrieBuilder } from '@keymanapp/models-templates';

const simpleLexicalModel = (function() {
  const builder = new TrieBuilder((str) => str);

  // Sum total: 10.
  builder.addEntry('are', 1);
  builder.addEntry('and', 2);

  builder.addEntry('not', 1);

  builder.addEntry('the', 3);
  builder.addEntry('try', 1);
  builder.addEntry('to',  1);
  builder.addEntry('tom', 1);

  return builder;
}());

const namesModel = (function() {
  const builder = new TrieBuilder((str) => str);

  builder.addEntry('amy',  1);
  builder.addEntry('bob',  1);
  builder.addEntry('mark', 1);
  builder.addEntry('tom',  1);

  return builder;
}());

const blendedModel = new CompositedTraversal([
  { sample: simpleLexicalModel.traverseFromRoot(), p: 0.5 },
  { sample: namesModel.traverseFromRoot(), p: 0.5 }
]);

describe('CompositedTraversal', () => {
  describe('direct prefix lookup', () => {
    it('returns undefined for non-existent paths', () => {
      assert.isUndefined(blendedModel.child('tomcat'));
      assert.isUndefined(blendedModel.child('tone'));
      assert.isUndefined(blendedModel.child('ton'));
      assert.isUndefined(blendedModel.child('apple'));
      assert.isUndefined(blendedModel.child('aim'));
      assert.isUndefined(blendedModel.child('xylophone'));
    });

    it('reweighs entries found in only one source', () => {
      const andNode = blendedModel.child('and');
      assert.equal(andNode.p, 0.1);

      assert.equal(andNode.entries.length, 1);
      const and = andNode.entries[0];
      assert.equal(and.text, 'and');
      assert.equal(and.p, 0.1);
    });

    it('merges entries found in multiple sources, combining their probability and entries', () => {
      const tomNode = blendedModel.child('tom');
      assert.equal(tomNode.p, 0.175);

      assert.equal(tomNode.entries.length, 1);
      const tom = tomNode.entries[0];
      assert.equal(tom.text, 'tom');
      assert.equal(tom.p, 0.175);
    });

    it('correctly returns the probability of intermediate child paths', () => {
      // simpleLexical:  best is 'and', 0.2
      // names:          best is 'amy', 0.25
      // Weight of each is equal at 0.5; 0.25 * 0.5 = 0.125.
      // Should not combine.
      const aNode = blendedModel.child('a');
      assert.equal(aNode.p, 0.125, "probability of paths were merged despite lack of duplicate");
      assert.equal(aNode.entries.length, 0);

      // 'tom' is on both paths, and is highest-frequency entry when merged.
      // We want the combined, net probability to show up on its predecessor paths.
      const toNode = blendedModel.child('to');
      assert.equal(toNode.p, 0.175, "probability of path with winning duplicate not merged");

      // simpleLexical - p('to') = 0.1; is then multiplied by model weight of 0.5.
      assert.equal(toNode.entries.length, 1);
      const to = toNode.entries[0];
      assert.equal(to.text, 'to');
      assert.equal(to.p, 0.05);
    });
  });

  describe('iteration for correction-search', () => {
    it('correctly enumerates child paths from root', () => {
      const topChildren = blendedModel.children();
      // 'n' from "simpleLexical", 'b' and 'm' from "names"
      assert.sameMembers(topChildren.map((child) => child.char), ['a', 'b', 'm', 'n', 't']);
      assert.sameDeepMembers(topChildren.map((child) => {
        return {
          char: child.char,
          p: child.p
        }
      }), [
        { char: 'a', p: .125 }, // highest:  'amy'
        { char: 'b', p: .125 }, // highest:  'bob'
        { char: 'm', p: .125 }, // highest:  'mark'
        { char: 'n', p: .05  }, // highest:  'not'
        { char: 't', p: .175 }  // highest:  'tom'
      ]);
      topChildren.forEach((entry) => assert.equal(entry.p, entry.traversal().p));
    });

    it('correctly enumerates child paths from section supported by a single member', () => {
      const topChildren = blendedModel.children();
      const bNode = topChildren.find((entry) => entry.char == 'b').traversal();
      const bChildren = bNode.children();
      assert.sameDeepMembers(bChildren.map((child) => {
        return {
          char: child.char,
          p: child.p
        };
      }), [
        { char: 'o', p: 0.125 }
      ]);
      bChildren.forEach((entry) => assert.equal(entry.p, entry.traversal().p));

      const boNode = bChildren.find((entry) => entry.char == 'o').traversal();
      const boChildren = boNode.children();
      assert.sameDeepMembers(boChildren.map((child) => {
        return {
          char: child.char,
          p: child.p
        };
      }), [
        { char: 'b', p: 0.125 }
      ]);
      boChildren.forEach((entry) => assert.equal(entry.p, entry.traversal().p));
    });

    it('correctly enumerates child paths from section supported by multiple members', () => {
      const topChildren = blendedModel.children();
      const tNode = topChildren.find((entry) => entry.char == 't').traversal();
      const tChildren = tNode.children();

      assert.sameMembers(tChildren.map((child) => child.char), ['h', 'o', 'r']);
      assert.sameDeepMembers(tChildren.map((child) => {
        return {
          char: child.char,
          p: child.p
        }
      }), [
        { char: 'h', p: 0.15 },
        { char: 'o', p: 0.175 },
        { char: 'r', p: 0.05 }
      ]);
      tChildren.forEach((entry) => assert.equal(entry.p, entry.traversal().p));

      const toNode = tChildren.find((entry) => entry.char == 'o').traversal();
      const toChildren = toNode.children();

      assert.sameMembers(toChildren.map((child) => child.char), ['m']);
      assert.sameDeepMembers(toChildren.map((child) => {
        return {
          char: child.char,
          p: child.p
        };
      }), [
        { char: 'm', p: 0.175 }
      ]);
      toChildren.forEach((entry) => assert.equal(entry.p, entry.traversal().p));
    });
  });
});