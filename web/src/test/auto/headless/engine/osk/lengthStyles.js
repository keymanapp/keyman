import { assert } from 'chai';
import sinon from 'sinon';

import { ParsedLengthStyle } from 'keyman/engine/osk';

describe('Length style processing', () => {
  it("constructs from empty-string", () => {
    let length = new ParsedLengthStyle('');
    assert.equal(length.val, 1);
    assert.equal(length.absolute, false);
  });

  it("constructs from garbage", () => {
    let length = new ParsedLengthStyle('garbage');
    assert.equal(length.val, 1);
    assert.equal(length.absolute, false);
  })

  it("constructs from 'px' styling", () => {
    let length = new ParsedLengthStyle("20px");

    assert.equal(length.styleString, "20px");
    assert.equal(length.val, 20);
    assert.equal(length.absolute, true);
    assert.isNotOk(length.special);
  });

  it("constructs from 'pt' styling", () => {
    let length = new ParsedLengthStyle("20pt");

    assert.equal(length.absolute, true);
    assert.isAbove(length.val, 20);
    assert.isNotOk(length.special);
  });

  it("constructs from '%' styling", () => {
    let length = new ParsedLengthStyle("50%");

    assert.equal(length.val, 0.5);
    assert.equal(length.absolute, false);
    assert.equal(length.styleString, "50%");
    assert.isNotOk(length.special);
  });

  it("constructs from 'em' styling", () => {
    let length = new ParsedLengthStyle("0.5em");

    assert.equal(length.val, 0.5);
    assert.equal(length.absolute, false);
    assert.equal(length.special, 'em');
    assert.equal(length.styleString, "0.5em");
  });

  it("constructs from 'rem' styling", () => {
    let length = new ParsedLengthStyle("0.5rem");

    assert.equal(length.val, 0.5);
    assert.equal(length.absolute, false);
    assert.equal(length.special, 'rem');
    assert.equal(length.styleString, "0.5rem");
  });

  it("clone-constructs", () => {
    let lengths = [
      new ParsedLengthStyle("20px"),
      new ParsedLengthStyle("20pt"),
      new ParsedLengthStyle("20%"),
      new ParsedLengthStyle("2.0em")
    ];

    let clonedLengths = lengths.map((length) => new ParsedLengthStyle(length));

    assert.deepEqual(clonedLengths, lengths);
    assert.notSameOrderedMembers(clonedLengths, lengths);
  });

  it('provides scaling via scaledBy() - returns separate, mutated instance', () => {
    let fixedLength = new ParsedLengthStyle("20px");
    let relLength = new ParsedLengthStyle("20%");

    let scaledFixedLength = fixedLength.scaledBy(2);
    let scaledRelLength = relLength.scaledBy(2);

    assert.notStrictEqual(scaledFixedLength, fixedLength);
    assert.notStrictEqual(scaledRelLength, relLength);

    assert.equal(scaledFixedLength.absolute, true);
    assert.equal(scaledRelLength.absolute, false);

    assert.equal(scaledFixedLength.styleString, "40px");
    assert.equal(scaledRelLength.styleString, "40%");
  });
});