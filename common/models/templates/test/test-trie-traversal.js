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

  it('iteration over compact leaf node', function() {
    var model = new TrieModel(jsonFixture('tries/english-1000'));

    let rootTraversal = model.getRootTraversal();
    assert.isDefined(rootTraversal);

    // 't', 'r', 'o' have siblings, but these don't.
    let leafChildSequence = ['u', 'b', 'l', 'e'];
    let eSuccess = false;
    for(child of rootTraversal.children()) {
      if(child.key == 't') {
        let traversalInner1 = child.traversal();
        assert.isDefined(traversalInner1);
        assert.isUndefined(child.entries);

        for(tChild of traversalInner1.children()) {
          if(tChild.key == 'r') { 
            let traversalInner2 = tChild.traversal();
            assert.isDefined(traversalInner2);
            assert.isUndefined(tChild.entries);

            for(rChild of traversalInner2.children()) {
              if(rChild.key == 'o') {
                let curChild = rChild;

                // At this point, we're already at the trie's actual leaf node for "trouble".
                // But for edit-distance trie traversal, we want to decompress this and model
                // an uncompacted Trie.
                do {
                  assert.isNotEmpty(leafChildSequence);

                  let oIter = curChild.traversal().children();
                  let curr = oIter.next();
                  curChild = curr.value;

                  // Test generator behavior - there should be one child, then the 'done' state.
                  assert.isDefined(curChild);
                  assert.equal(curChild.key, leafChildSequence[0]);
                  curr = oIter.next();
                  assert.isTrue(curr.done);

                  // Prepare for iteration.
                  leafChildSequence.shift();

                  // Conditional test - if that was not the final character, entries should be undefined.
                  if(leafChildSequence.length > 0) {
                    assert.isUndefined(curChild.traversal().entries);
                  } else {
                    let finalTraversal = curChild.traversal();
                    assert.isDefined(finalTraversal.entries);
                    assert.equal(finalTraversal.entries[0], 'trouble');
                    eSuccess = true;
                  }
                } while (leafChildSequence.length > 0);
              }
            }
          }
        }
      }
    }

    assert.isTrue(eSuccess);
  });
});
