import 'mocha';
import { assert } from 'chai';
import { StrsItem, StrsOptions, DependencySections, Strs } from '../../src/kmx/kmx-plus/kmx-plus.js';
import { ListIndex, ListItem } from '../../src/ldml-keyboard/string-list.js';

describe('Test of String-List', () => {
  describe('Test ListIndex', () => {
    it('can construct a ListIndex', () => {
      const strsItem = new StrsItem("abc");
      const actual   = new ListIndex(strsItem);
      assert.deepEqual(actual.value, strsItem);
    });
    it('can check two ListIndex for equality', () => {
      const listItemOne = new ListIndex(new StrsItem("abc"));
      const listItemTwo = new ListIndex(new StrsItem("abc"));
      assert.isTrue(listItemOne.isEqual(listItemTwo));
    });
    it('can check two different ListIndex are not equal', () => {
      const listItemOne = new ListIndex(new StrsItem("abc"));
      const listItemTwo = new ListIndex(new StrsItem("def"));
      assert.isFalse(listItemOne.isEqual(listItemTwo));
    });
    it('can check a ListIndex and string for equality', () => {
      const listItem = new ListIndex(new StrsItem("abc"));
      const aString = "abc";
      assert.isTrue(listItem.isEqual(aString));
    });
    it('can check a ListIndex and string for inequality', () => {
      const listItem = new ListIndex(new StrsItem("abc"));
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
  describe('Test ListItem', () => {
    describe('Test fromStrings()', () => {
      it('should return an empty ListItem if source is null', () => {
        const actual   = ListItem.fromStrings(null, null, null);
        const expected = new ListItem();
        assert.deepEqual(actual, expected);
      });
      it('should return a valid ListItem from a single source string', () => {
        const source = ["abc"];
        const sections = { strs: new Strs };
        sections.strs.allocString = stubSectionsStrsAllocString;
        const actual   = ListItem.fromStrings(source, null, sections);
        const expected = initListItem(source);
        assert.deepEqual(actual, expected);
      });
      it('should return a valid ListItem from a longer source', () => {
        const source = ["abc", "def", "ghi"];
        const sections = { strs: new Strs };
        sections.strs.allocString = stubSectionsStrsAllocString;
        const actual   = ListItem.fromStrings(source, null, sections);
        const expected = initListItem(source);
        assert.deepEqual(actual, expected);
      });
    });
    describe('Test getItemOrder()', () => {
      it('should return a valid index for the first item', () => {
        const listItem = initListItem(["abc", "def", "ghi"]);
        const index = listItem.getItemOrder("abc");
        assert.equal(index, 0);
      });
      it('should return a valid index for a later item', () => {
        const listItem = initListItem(["abc", "def", "ghi"]);
        const index = listItem.getItemOrder("ghi");
        assert.equal(index, 2);
      });
      it('should return -1 for a missing item', () => {
        const listItem = initListItem(["abc", "def", "ghi"]);
        const index    = listItem.getItemOrder("jkl");
        assert.equal(index, -1);
      });
    });
    describe('Test isEqual()', () => {
      it('should return true for two empty ListItems', () => {
        const listItemOne = new ListItem();
        const listItemTwo = new ListItem();
        assert.isTrue(listItemOne.isEqual(listItemTwo));
      });
      it('should return false for empty and non-empty ListItems', () => {
        const listItemOne = new ListItem();
        const listItemTwo = initListItem(["abc"]);
        assert.isFalse(listItemOne.isEqual(listItemTwo));
      });
      it('should return false for non-empty and empty ListItems', () => {
        const listItemOne = initListItem(["abc"]);
        const listItemTwo = new ListItem();
        assert.isFalse(listItemOne.isEqual(listItemTwo));
      });
      it('should return true for identical ListItems', () => {
        const listItemOne = initListItem(["abc", "def", "ghi"]);
        const listItemTwo = initListItem(["abc", "def", "ghi"]);
        assert.isTrue(listItemOne.isEqual(listItemTwo));
      });
      it('should return false for different ListItems', () => {
        const listItemOne = initListItem(["abc", "def", "ghi"]);
        const listItemTwo = initListItem(["abd", "def", "ghi"]);
        assert.isFalse(listItemOne.isEqual(listItemTwo));
      });
      it('should return false for different length ListItems', () => {
        const listItemOne = initListItem(["abc", "def"]);
        const listItemTwo = initListItem(["abc", "def", "ghi"]);
        assert.isFalse(listItemOne.isEqual(listItemTwo));
      });
      it('should return true for empty ListItem and string[]', () => {
        const listItem = new ListItem();
        assert.isTrue(listItem.isEqual([]));
      });
      it('should return false for empty ListItem and non-empty string[]', () => {
        const listItem = new ListItem();
        assert.isFalse(listItem.isEqual(["abc"]));
      });
      it('should return false for non-empty ListItem and empty string[]', () => {
        const listItem = initListItem(["abc"]);;
        assert.isFalse(listItem.isEqual([]));
      });
      it('should return true for identical ListItem and string[]', () => {
        const listItem = initListItem(["abc", "def", "ghi"]);
        assert.isTrue(listItem.isEqual(["abc", "def", "ghi"]));
      });
      it('should return false for different ListItem and string[]', () => {
        const listItem = initListItem(["abc", "def", "ghi"]);
        assert.isFalse(listItem.isEqual(["abd", "def", "ghi"]));
      });
      it('should return false for different length ListItem and string[]', () => {
        const listItem = initListItem(["abc", "def"]);
        assert.isFalse(listItem.isEqual(["abc", "def", "ghi"]));
      });
    });
    describe('Test compareTo()', () => {
      it('should return 0 for identical ListItems', () => {
        const listItemOne = initListItem(["abc", "def", "ghi"]);
        const listItemTwo = initListItem(["abc", "def", "ghi"]);
        assert.equal(listItemOne.compareTo(listItemTwo), 0);
      });
      it('should return -1 for ListItems with different first items (smallest first)', () => {
        const listItemOne = initListItem(["abc", "def", "ghi"]);
        const listItemTwo = initListItem(["abd", "def", "ghi"]);
        assert.equal(listItemOne.compareTo(listItemTwo), -1);
      });
      it('should return 1 for ListItems with different first items (smallest second)', () => {
        const listItemOne = initListItem(["abd", "def", "ghi"]);
        const listItemTwo = initListItem(["abc", "def", "ghi"]);
        assert.equal(listItemOne.compareTo(listItemTwo), 1);
      });
      it('should return -1 for ListItems with different later items (smallest first)', () => {
        const listItemOne = initListItem(["abc", "def", "ghi"]);
        const listItemTwo = initListItem(["abc", "def", "ghj"]);
        assert.equal(listItemOne.compareTo(listItemTwo), -1);
      });
      it('should return 1 for ListItems with different later items (smallest second)', () => {
        const listItemOne = initListItem(["abc", "def", "ghj"]);
        const listItemTwo = initListItem(["abc", "def", "ghi"]);
        assert.equal(listItemOne.compareTo(listItemTwo), 1);
      });
      it('should return -1 for identical ListItems, except shorter first', () => {
        const listItemOne = initListItem(["abc", "def", "ghi"]);
        const listItemTwo = initListItem(["abc", "def", "ghi", "jkl"]);
        assert.equal(listItemOne.compareTo(listItemTwo), -1);
      });
      it('should return 1 for identical ListItems, except longer first', () => {
        const listItemOne = initListItem(["abc", "def", "ghi", "jkl"]);
        const listItemTwo = initListItem(["abc", "def", "ghi"]);
        assert.equal(listItemOne.compareTo(listItemTwo), 1);
      });
    });
    describe('Test toString()', () => {
      it('should return correct string', () => {
        const listItem = initListItem(["abc", "def", "ghi"]);
        assert.deepEqual(listItem.toString(), "abc def ghi");
      });
      it('should return correct string for empty ListItem', () => {
        const listItem = new ListItem;
        assert.deepEqual(listItem.toString(), "");
      });
    });
    describe('Test toStringArray()', () => {
      it('should return correct string[]', () => {
        const source   = ["abc", "def", "ghi"];
        const listItem = initListItem(source);
        assert.deepEqual(listItem.toStringArray(), source);
      });
      it('should return correct string[] for empty ListItem', () => {
        const listItem = new ListItem;
        assert.deepEqual(listItem.toStringArray(), []);
      });
    }); 
  });
});

function stubSectionsStrsAllocString(s?: string, opts?: StrsOptions, sections?: DependencySections): StrsItem {
  return new StrsItem(s);
}

function initListItem(source: Array<string>): ListItem {
  const listItem = new ListItem();
  for (const s of source)
    listItem.push(new ListIndex(new StrsItem(s)));
  return listItem;
}
