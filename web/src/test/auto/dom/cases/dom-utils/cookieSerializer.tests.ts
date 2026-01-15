import { CookieSerializer } from 'keyman/engine/dom-utils';
import { assert } from 'chai';

const RESET="max-age=0";

describe('CookieSerializer', function () {
  describe('SimpleTestCookie', () => {
    const COOKIE_ID = "SimpleTestCookie";

    afterEach(() => {
      // Purge the cookie!
      document.cookie = `${COOKIE_ID}=; ${RESET}`;
    })

    it('serializes & reloads when all values are strings', () => {
      const cookieWriter = new CookieSerializer(COOKIE_ID);
      const obj = {
        foo: 'bar',
        widget: 'sprog',
        fruity: 'tooty',
        flagProp: ''
      }
      cookieWriter.save(obj);

      const cookieReader = new CookieSerializer(COOKIE_ID);
      const reloadedObj = cookieReader.load();

      assert.deepEqual(reloadedObj, obj);
      assert.notStrictEqual(reloadedObj, obj);
    });

    it('serializes all values to strings', () => {
      const cookieWriter = new CookieSerializer(COOKIE_ID);
      const obj = {
        foo: 'bar',
        two: 2,
        true: true
      }
      const expectedObj = {
        foo: 'bar',
        two: `${2}`,
        true: `${true}`
      }
      cookieWriter.save(obj);

      const cookieReader = new CookieSerializer(COOKIE_ID);
      const reloadedObj = cookieReader.load();

      assert.deepEqual(reloadedObj, expectedObj);
      assert.notStrictEqual(reloadedObj, expectedObj);
    });

    it('accepts custom deserialization', () => {
      const cookieWriter = new CookieSerializer(COOKIE_ID);
      const obj = {
        foo: 'bar',
        two: 2,
        true: true
      }
      cookieWriter.save(obj);

      const cookieReader = new CookieSerializer(COOKIE_ID);
      const reloadedObj = cookieReader.load((value, key) => {
        switch(key) {
          case 'two':
            return Number.parseInt(value, 10);
          case 'true':
            return value === 'true';
          default:
            return value;
        }
      });

      assert.deepEqual(reloadedObj, obj);
      assert.notStrictEqual(reloadedObj, obj);
    });

    it('works with encodeURIComponent + decodeURIComponent', () => {
      const cookieWriter = new CookieSerializer(COOKIE_ID);
      const obj = {
        foo:     'bar',
        symbols: ':;+&?@ =\n', // Stuff that'll royally wreck cookies if not encoded.
        star:    '⭐' // Sure, why not test with an emoji?
      }
      cookieWriter.save(obj, encodeURIComponent);

      const cookieReader = new CookieSerializer(COOKIE_ID);
      const reloadedObj = cookieReader.load(decodeURIComponent);

      assert.deepEqual(reloadedObj, obj);
      assert.notStrictEqual(reloadedObj, obj);
    });
  });

  describe('loadAllMatching', () => {
    type TestStore = { [name: string]: string; };
    const COOKIE_ID_1 = "TestCookie_One";
    const COOKIE_ID_2 = "TestCookie_Two";
    const NONMATCHING_COOKIE_ID = "NonmatchingCookie";

    beforeEach(() => {
      const cookieSerializer1 = new CookieSerializer<TestStore>(COOKIE_ID_1);
      cookieSerializer1.save({ COOKIE_ID_1: 'foobar1' });

      const cookieSerializer2 = new CookieSerializer<TestStore>(COOKIE_ID_2);
      cookieSerializer2.save({ COOKIE_ID_2: 'foobar2' });

      const nonmatchingCookieSerializer = new CookieSerializer<TestStore>(NONMATCHING_COOKIE_ID);
      nonmatchingCookieSerializer.save({ NONMATCHING_COOKIE_ID: 'foobar3' });
    });

    afterEach(() => {
      document.cookie = `${COOKIE_ID_1}=; ${RESET}`;
      document.cookie = `${COOKIE_ID_2}=; ${RESET}`;
      document.cookie = `${NONMATCHING_COOKIE_ID}=; ${RESET}`;
    });

    it('finds all matching cookies starting with TestCookie_', () => {
      const result = CookieSerializer.loadAllMatching(/^TestCookie_/);
      assert.deepEqual(result, [
        { name: COOKIE_ID_1, value: { COOKIE_ID_1: 'foobar1' } },
        { name: COOKIE_ID_2, value: { COOKIE_ID_2: 'foobar2' } }
      ]);
    });

    it('finds all matching cookies containing Cookie', () => {
      const result = CookieSerializer.loadAllMatching(/Cookie/);
      assert.deepEqual(result, [
        { name: COOKIE_ID_1, value: { COOKIE_ID_1: 'foobar1' } },
        { name: COOKIE_ID_2, value: { COOKIE_ID_2: 'foobar2' } },
        { name: NONMATCHING_COOKIE_ID, value: { NONMATCHING_COOKIE_ID: 'foobar3' } }
      ]);
    });

    it('returns empty array if no matching cookies', () => {
      const result = CookieSerializer.loadAllMatching(/^UnknownCookie_/);
      assert.deepEqual(result, []);
    });
  });
});