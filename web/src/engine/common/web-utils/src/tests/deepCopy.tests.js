import { assert } from 'chai';

import { deepCopy } from '@keymanapp/web-utils';


describe('deepCopy', function() {
  it('simple object', () => {
    const original = {
      a: 1,
      b: '2',
      c: () => 3
    };
    const clone = deepCopy(original);

    assert.deepEqual(clone, original);
    assert.notEqual(clone, original);

    original.b = 'two';
    assert.equal(clone.b, '2');
    assert.equal(clone.c(), 3);
  });

  it('object with simple array', () => {
    const original = { arr: [1, 2, 3, 4, 5] };
    const clone = deepCopy(original);

    assert.deepEqual(clone, original)
    assert.sameDeepOrderedMembers(clone.arr, original.arr);
    assert.notEqual(clone.arr, original.arr);
    assert.notEqual(clone, original);

    original.arr[2] = 13;
    assert.equal(clone.arr[2], 3);
  });

  it('complex object', () => {
    const original = {
      arr: [1, 2, {entries: [3, [4, 5]]}],
      nested: {
        character: {
          first: 'inigo',
          last: 'montoya'
        },
        actor: {
          first: 'mandy',
          last: 'patinkin'
        },
        getQuote: () => "My name is Inigo Montoya.  You killed my father; prepare to die!"
      }
    }

    const clone = deepCopy(original);

    assert.deepEqual(clone, original);
    assert.notEqual(clone, original);

    assert.sameDeepOrderedMembers(clone.arr, original.arr);
    assert.notEqual(clone.arr, original.arr);
    assert.notEqual(clone.arr[2], original.arr[2]);
    assert.sameDeepOrderedMembers(clone.arr[2].entries, original.arr[2].entries);
    assert.notEqual(clone.arr[2].entries, original.arr[2].entries);
    assert.notEqual(clone.arr[2].entries[1], original.arr[2].entries[1]);

    assert.deepEqual(clone.nested, original.nested);
    assert.notEqual(clone.nested, original.nested);

    assert.notEqual(clone.nested.character, original.nested.character);
    assert.notEqual(clone.nested.actor, original.nested.actor);

    assert.isFunction(clone.nested.getQuote);
    assert.equal(clone.nested.getQuote(), original.nested.getQuote());
    // the function will actually be the same instance.
  });
});
