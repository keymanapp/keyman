/*
 * Unit tests for the Trie prediction model.
 */

import { assert } from 'chai';
import { TrieModel } from '@keymanapp/models-templates';

// Useful for tests related to strings with supplementary pairs.
var smpForUnicode = function(code){
  var H = Math.floor((code - 0x10000) / 0x400) + 0xD800;
  var L = (code - 0x10000) % 0x400 + 0xDC00;

  return String.fromCharCode(H, L);
}

// Prob:  entry weight / total weight
// "the" is the highest-weighted word in the fixture.
const PROB_OF_THE     = 1000 / 500500;
const PROB_OF_TRUE    =  607 / 500500;
const PROB_OF_TROUBLE =  267 / 500500;

describe('Trie traversal abstractions', function() {
  it('root-level iteration over child nodes', function() {
    var model = new TrieModel(jsonFixture('tries/english-1000'));

    let rootTraversal = model.traverseFromRoot();
    assert.isDefined(rootTraversal);

    let rootKeys = ['t', 'o', 'a', 'i', 'w', 'h', 'f', 'b', 'n', 'y', 's', 'm',
                    'u', 'c', 'd', 'l', 'e', 'j', 'p', 'g', 'v', 'k', 'r', 'q'];

    for(let child of rootTraversal.children()) {
      let keyIndex = rootKeys.indexOf(child.char);
      assert.notEqual(keyIndex, -1);
      rootKeys.splice(keyIndex, 1);
    }

    assert.isEmpty(rootKeys);
  });

  it('traversal with simple internal nodes', function() {
    var model = new TrieModel(jsonFixture('tries/english-1000'));

    let rootTraversal = model.traverseFromRoot();
    assert.isDefined(rootTraversal);

    let eKeys = ['y', 'r', 'i', 'm', 's', 'n', 'o'];

    let tSuccess = false;
    let hSuccess = false;
    let eSuccess = false;
    for(let child of rootTraversal.children()) {
      if(child.char == 't') {
        tSuccess = true;
        let traversalInner1 = child.traversal();
        assert.isDefined(traversalInner1);
        assert.isArray(child.traversal().entries);
        assert.isEmpty(child.traversal().entries);
        assert.equal(traversalInner1.p, PROB_OF_THE);

        for(let tChild of traversalInner1.children()) {
          if(tChild.char == 'h') {
            hSuccess = true;
            let traversalInner2 = tChild.traversal();
            assert.isDefined(traversalInner2);
            assert.isEmpty(tChild.traversal().entries);
            assert.isArray(tChild.traversal().entries);
            assert.equal(traversalInner2.p, PROB_OF_THE);

            for(let hChild of traversalInner2.children()) {
              if(hChild.char == 'e') {
                eSuccess = true;
                let traversalInner3 = hChild.traversal();
                assert.isDefined(traversalInner3);
                assert.isDefined(traversalInner3.entries);
                assert.deepEqual(traversalInner3.entries, [
                  {
                    text: "the",
                    p: PROB_OF_THE
                  }
                ]);
                assert.equal(traversalInner3.p, PROB_OF_THE);

                for(let eChild of traversalInner3.children()) {
                  let keyIndex = eKeys.indexOf(eChild.char);
                  assert.notEqual(keyIndex, -1, "Did not find char '" + eChild.char + "' in array!");

                  // THE is not accessible if any of the sub-tries of our 'e' node (traversalInner3).
                  assert.isBelow(eChild.traversal().p, PROB_OF_THE);
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

  it('traversal over compact leaf node', function() {
    var model = new TrieModel(jsonFixture('tries/english-1000'));

    let rootTraversal = model.traverseFromRoot();
    assert.isDefined(rootTraversal);

    // 't', 'r', 'o' have siblings, but these don't.
    let leafChildSequence = ['u', 'b', 'l', 'e'];
    let eSuccess = false;
    for(let child of rootTraversal.children()) {
      if(child.char == 't') {
        let traversalInner1 = child.traversal();
        assert.isDefined(traversalInner1);
        assert.isArray(child.traversal().entries);
        assert.isEmpty(child.traversal().entries);
        assert.equal(traversalInner1.p, PROB_OF_THE);

        for(let tChild of traversalInner1.children()) {
          if(tChild.char == 'r') {
            let traversalInner2 = tChild.traversal();
            assert.isDefined(traversalInner2);
            assert.isArray(tChild.traversal().entries);
            assert.isEmpty(tChild.traversal().entries);
            assert.equal(traversalInner2.p, PROB_OF_TRUE);

            for(let rChild of traversalInner2.children()) {
              if(rChild.char == 'o') {
                let curChild = rChild;

                // At this point, we're already at the trie's actual leaf node for "trouble".
                // But for edit-distance trie traversal, we want to decompress this and model
                // an uncompacted Trie.
                do {
                  assert.isNotEmpty(leafChildSequence);

                  let iter = curChild.traversal().children();
                  let curr = iter.next();
                  curChild = curr.value;

                  // Test generator behavior - there should be one child, then the 'done' state.
                  assert.isDefined(curChild);
                  assert.equal(curChild.char, leafChildSequence[0]);
                  curr = iter.next();
                  assert.isTrue(curr.done);

                  // Prepare for iteration.
                  leafChildSequence.shift();

                  // Conditional test - if that was not the final character, entries should be undefined.
                  if(leafChildSequence.length > 0) {
                    assert.isArray(curChild.traversal().entries);
                    assert.isEmpty(curChild.traversal().entries);
                    assert.equal(curChild.traversal().p, PROB_OF_TROUBLE);
                  } else {
                    let finalTraversal = curChild.traversal();
                    assert.equal(finalTraversal.p, PROB_OF_TROUBLE);
                    assert.isDefined(finalTraversal.entries);
                    assert.deepEqual(finalTraversal.entries, [
                      {
                        text: 'trouble',
                        p: PROB_OF_TROUBLE
                      }
                    ]);
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


  it('traversal with SMP entries', function() {
    // Two entries, both of which read "apple" to native English speakers.
    // One solely uses SMP characters, the other of which uses a mix of SMP and standard.
    var model = new TrieModel(jsonFixture('tries/smp-apple'));

    let rootTraversal = model.traverseFromRoot();
    assert.isDefined(rootTraversal);

    let smpA = smpForUnicode(0x1d5ba);
    let smpP = smpForUnicode(0x1d5c9);
    let smpE = smpForUnicode(0x1d5be);

    // Just to be sure our utility function is working right.
    assert.equal(smpA + smpP + 'pl' + smpE, "𝖺𝗉pl𝖾");

    let pKeys = ['p', smpP];
    let leafChildSequence = ['l', smpE];

    let aSuccess = false;
    let pSuccess = false;
    let eSuccess = false;
    for(let child of rootTraversal.children()) {
      if(child.char == smpA) {
        aSuccess = true;
        const traversalInner1 = child.traversal();
        assert.isDefined(traversalInner1);
        assert.isArray(traversalInner1.entries);
        assert.isEmpty(traversalInner1.entries);
        assert.equal(traversalInner1.p, 0.5); // The two entries are equally weighted.

        for(let aChild of traversalInner1.children()) {
          if(aChild.char == smpP) {
            pSuccess = true;
            const traversalInner2 = aChild.traversal();
            assert.isDefined(traversalInner2);
            assert.isArray(traversalInner2.entries);
            assert.isEmpty(traversalInner2.entries);
            assert.equal(traversalInner2.p, 0.5);

            for(let pChild of traversalInner2.children()) {
              let keyIndex = pKeys.indexOf(pChild.char);
              assert.notEqual(keyIndex, -1, "Did not find char '" + pChild.char + "' in array!");
              pKeys.splice(keyIndex, 1);

              if(pChild.char == 'p') { // We'll test traversal with the 'mixed' entry from here.
                const traversalInner3 = pChild.traversal();
                assert.isDefined(traversalInner3);
                assert.isArray(traversalInner3.entries);
                assert.isEmpty(traversalInner3.entries);
                assert.equal(traversalInner3.p, 0.5);

                // Now to handle the rest, knowing it's backed by a leaf node.
                let curChild = pChild;

                // At this point, we're already at the trie's actual leaf node for "trouble".
                // But for edit-distance trie traversal, we want to decompress this and model
                // an uncompacted Trie.
                do {
                  assert.isNotEmpty(leafChildSequence);

                  let iter = curChild.traversal().children();
                  let curr = iter.next();
                  curChild = curr.value;

                  // Test generator behavior - there should be one child, then the 'done' state.
                  assert.isDefined(curChild);
                  assert.equal(curChild.char, leafChildSequence[0]);
                  curr = iter.next();
                  assert.isTrue(curr.done);

                  // Prepare for iteration.
                  leafChildSequence.shift();

                  // Conditional test - if that was not the final character, entries should be undefined.
                  if(leafChildSequence.length > 0) {
                    const nextTraversal = curChild.traversal()
                    assert.isArray(nextTraversal.entries);
                    assert.isEmpty(nextTraversal.entries);
                    assert.equal(nextTraversal.p, 0.5);
                  } else {
                    let finalTraversal = curChild.traversal();
                    assert.isDefined(finalTraversal.entries);
                    assert.deepEqual(finalTraversal.entries, [
                      {
                        text: smpA + smpP + 'pl' + smpE,
                        p: 1/2
                      }
                    ]);
                    assert.equal(finalTraversal.p, 0.5);
                    eSuccess = true;
                  }
                } while (leafChildSequence.length > 0);
              }
            }
          }
        }
      }
    }

    assert.isTrue(aSuccess);
    assert.isTrue(pSuccess);
    assert.isTrue(eSuccess);

    assert.isEmpty(pKeys);
  });
});
