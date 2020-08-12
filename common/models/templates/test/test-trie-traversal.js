/*
 * Unit tests for the Trie prediction model.
 */

var assert = require('chai').assert;
var TrieModel = require('../').models.TrieModel;

describe('Trie traversal abstractions', function() {
  it('root-level iteration over child nodes', function() {
    var model = new TrieModel(jsonFixture('tries/english-1000'));

    let rootTraversal = model.getRootTraversal();
    assert.isDefined(rootTraversal);

    let rootKeys = ['t', 'o', 'a', 'i', 'w', 'h', 'f', 'b', 'n', 'y', 's', 'm',
                    'u', 'c', 'd', 'l', 'e', 'j', 'p', 'g', 'v', 'k', 'r', 'q']

    for(child of rootTraversal.children()) {
      let keyIndex = rootKeys.indexOf(child.key);
      assert.notEqual(keyIndex, -1);
      rootKeys.splice(keyIndex, 1);
    }

    assert.isEmpty(rootKeys);
  });

  it('iteration over simple internal node', function() {
    var model = new TrieModel(jsonFixture('tries/english-1000'));

    let rootTraversal = model.getRootTraversal();
    assert.isDefined(rootTraversal);

    let eKeys = ['y', 'r', 'i', 'm', 's', 'n', 'o'];

    let tSuccess = false;
    let hSuccess = false;
    let eSuccess = false;
    for(child of rootTraversal.children()) {
      if(child.key == 't') {
        tSuccess = true;
        let traversalInner1 = child.traversal();
        assert.isDefined(traversalInner1);
        assert.isUndefined(child.entries);

        for(tChild of traversalInner1.children()) {
          if(tChild.key == 'h') { 
            hSuccess = true;
            let traversalInner2 = tChild.traversal();
            assert.isDefined(traversalInner2);
            assert.isUndefined(tChild.entries);

            for(hChild of traversalInner2.children()) {
              if(hChild.key == 'e') {
                eSuccess = true;
                let traversalInner3 = hChild.traversal();
                assert.isDefined(traversalInner3);
                
                assert.isDefined(traversalInner3.entries);
                assert.equal(traversalInner3.entries[0], "the");

                for(eChild of traversalInner3.children()) {
                  let keyIndex = eKeys.indexOf(eChild.key);
                  assert.notEqual(keyIndex, -1, "Did not find char '" + eChild.key + "' in array!");
                  eKeys.splice(keyIndex, 1);
                }
              }
            }
          }
        }
      }
    }

    assert.isTrue(tSuccess);
    assert.isTrue(hSuccess);
    assert.isTrue(eSuccess);

    assert.isEmpty(eKeys);
  });

  // Next test:  "tro" + "uble" - only "tro" has actual representation!
});
