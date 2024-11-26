import 'mocha';
import { assert } from 'chai';
//import { ListIndex, ListItem } from '../../src/ldml-keyboard/string-list.js';
import { ListItem } from '../../src/ldml-keyboard/string-list.js';

describe('Test of String-List', () => {
  describe('should test ListItem', () => {
    it('fromStrings returns an empty ListItem if source is null', () => {
      const actual   = ListItem.fromStrings(null, null, null);
      const expected = new ListItem();
      assert.deepEqual(actual, expected);
    });
  });
});
