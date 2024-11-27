import 'mocha';
import { assert } from 'chai';
import { StrsItem, StrsOptions, DependencySections, Strs } from '../../src/kmx/kmx-plus/kmx-plus.js';
import { ListIndex, ListItem } from '../../src/ldml-keyboard/string-list.js';

describe('Test of String-List', () => {
  describe('should test ListIndex', () => {
    it('can construct a ListIndex', () => {
      const strsItem = new StrsItem("abc");
      const actual   = new ListIndex(strsItem);
      assert.deepEqual(actual.value, strsItem);
    });
    it('can check two ListIndex for equality', () => {
      const strsItemOne = new StrsItem("abc");
      const strsItemTwo = new StrsItem("abc");
      const listItemOne = new ListIndex(strsItemOne);
      const listItemTwo = new ListIndex(strsItemTwo);
      assert.isTrue(listItemOne.isEqual(listItemTwo));
    });
    it('can check two different ListIndex are not equal', () => {
      const strsItemOne = new StrsItem("abc");
      const strsItemTwo = new StrsItem("def");
      const listItemOne = new ListIndex(strsItemOne);
      const listItemTwo = new ListIndex(strsItemTwo);
      assert.isFalse(listItemOne.isEqual(listItemTwo));
    });
    it('can check a ListIndex and string for equality', () => {
      const strsItem = new StrsItem("abc");
      const listItem = new ListIndex(strsItem);
      const aString = "abc";
      assert.isTrue(listItem.isEqual(aString));
    });
    it('can check a ListIndex and string for inequality', () => {
      const strsItem = new StrsItem("abc");
      const listItem = new ListIndex(strsItem);
      const aString = "def";
      assert.isFalse(listItem.isEqual(aString));
    });
    it('can provide a correct string representation', () => {
      const strsItem = new StrsItem("abc");
      const listItem = new ListIndex(strsItem);
      const expected = "abc";
      assert.deepEqual(listItem.toString(), expected);
    });
  });
  describe('should test ListItem', () => {
    it('fromStrings should return an empty ListItem if source is null', () => {
      const actual   = ListItem.fromStrings(null, null, null);
      const expected = new ListItem();
      assert.deepEqual(actual, expected);
    });
    it('fromStrings should return a valid ListItem from a single source string', () => {
      const source = ["abc"];
      const sections = { strs: new Strs };
      sections.strs.allocString = stubSectionsStrsAllocString;
      const actual   = ListItem.fromStrings(source, null, sections);
      const expected = new ListItem();
      expected.push(new ListIndex(new StrsItem("abc")));
      assert.deepEqual(actual, expected);
    });
  });
});

function stubSectionsStrsAllocString(s?: string, opts?: StrsOptions, sections?: DependencySections): StrsItem {
  return new StrsItem(s);
}
