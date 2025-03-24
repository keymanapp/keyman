import { CookieSerializer } from 'keyman/engine/dom-utils';
import { assert } from 'chai';

const RESET="max-age=0";

describe('CookieSerializer', function () {
  describe('SimpleTestCookie', () => {
    const COOKIE_ID = "SimpleTestCookie";

    beforeEach(() => {
      // Purge the cookie!
      document.cookie = `${COOKIE_ID}=foobar; ${RESET}`;

      const cookieLoader = new CookieSerializer(COOKIE_ID);
      assert.deepEqual(cookieLoader.load(), {});
    });

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
});