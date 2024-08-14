import { assert } from 'chai';

import { Mock, findCommonSubstringEndIndex } from 'keyman/engine/keyboard';
import { extendString } from '@keymanapp/web-utils';

extendString();  // Ensure KMW's string-extension functionality is available.

String.kmwEnableSupplementaryPlane(false);

// A unicode-coding like alias for use in constructing non-BMP strings.
const u = String.fromCodePoint;

/**
 * Returns the "Mathematical Sans-Serif Small" non-BMP encoding for
 * a passed-in lowercase char between 'a' and 'z', inclusive.
 * @param {*} char
 * @returns
 */
const ss = (char) => {
  const charCodeOffset = char.charCodeAt(0) - 'a'.charCodeAt(0);
  return u(0x1d5ba + charCodeOffset);
}

describe("String divergence calculations", function() {
  describe("Common prefix", () => {
    it("BMP text", () => {
      const result1 = findCommonSubstringEndIndex("apple", "applause", false);
      assert.equal(result1, 4);

      const result2 = findCommonSubstringEndIndex("applesauce", "applause", false);
      assert.equal(result2, 4);
    });

    it("BMP edge cases", () => {
      const result1 = findCommonSubstringEndIndex("applesauce", "applesauce", false);
      assert.equal(result1, 10);

      const result2 = findCommonSubstringEndIndex("applesauce", "banana bread", false);
      assert.equal(result2, 0);
    });

    it("non-BMP text", () => {
      const smp_ify = (str) => str.split('').map(ss).join('');

      const result1 = findCommonSubstringEndIndex(
        smp_ify('apple'),
        smp_ify('applause'),
        false
      );

      // 2 per non-BMP char; is in code-unit... units.
      // Will avoid splitting code points, though.
      assert.equal(result1, 8);

      const result2 = findCommonSubstringEndIndex(
        smp_ify('applesauce'),
        smp_ify('applause'),
        false
      );

      assert.equal(result2, 8);
    });

    it("non-BMP edge cases", () => {
      const smp_ify = (str) => str.split('').map(ss).join('');

      const result1 = findCommonSubstringEndIndex(
        smp_ify('applesauce'),
        smp_ify('applesauce'),
        false
      );

      assert.equal(result1, 20);

      const result2 = findCommonSubstringEndIndex(
        smp_ify('applesauce'),
        smp_ify('banana bread'),
        false
      );

      assert.equal(result2, 0);
    })
  });

  describe("Common suffix", () => {
    it("BMP text", () => {
      //    att|endance
      // transc|endance
      const result1 = findCommonSubstringEndIndex("attendance", "transcendance", true);
      assert.equal(result1, 2);

      // transcend|ance
      //  happenst|ance
      const result2 = findCommonSubstringEndIndex("transcendance", "happenstance", true);
      assert.equal(result2, 8);

    });

    it("BMP edge cases", () => {
      // If the two are equal...
      const result1 = findCommonSubstringEndIndex("post-caret text", "post-caret text", true);
      assert.equal(result1, -1);

      // If the two are completely different...
      const result2 = findCommonSubstringEndIndex("post-caret text", "supercalifragilistic", true);
      assert.equal(result2, "post-caret text".length-1);
    })

    it("non-BMP text", () => {
      const smp_ify = (str) => str.split('').map(ss).join('');

      //   att|endance
      // trans|endance
      const result1 = findCommonSubstringEndIndex(
        smp_ify("attendance"),
        smp_ify("transcendance"),
        true
      );

      // 2 per non-BMP char; is in code-unit... units.
      // Will avoid splitting code points; is odd b/c we get the index of the LAST char of the pair.
      assert.equal(result1, 5);

      // transcend|ance
      //  happenst|ance
      const result2 = findCommonSubstringEndIndex(
        smp_ify("transcendance"),
        smp_ify("happenstance"),
        true
      );
      assert.equal(result2, 17);

    });

    it("non-BMP edge cases", () => {
      const smp_ify = (str) => str.split('').map(ss).join('');

      // If the two are equal...
      const result3 = findCommonSubstringEndIndex(
        smp_ify("post-caret text"),
        smp_ify("post-caret text"),
        true
      );
      assert.equal(result3, -1);

      // If the two are completely different...
      const result2 = findCommonSubstringEndIndex(
        smp_ify("post-caret text"),
        smp_ify("supercalifragilistic"),
        true
      );
      assert.equal(result2, smp_ify("post-caret text").length-1);
    })
  })
});

describe("Transcriptions and Transforms", function() {
  // Built in-line via function.  Looks functionally equivalent to "apple", but with non-BMP characters.
  let smpApple = u(0x1d5ba)+u(0x1d5c9)+u(0x1d5c9)+u(0x1d5c5)+u(0x1d5be);

  it("does not store an alias for related OutputTargets", function() {
    // We have other texts validating Mocks; by using them as our base 'element', this unit test file
    // could eventually run in 'headless' mode.
    var target = new Mock("apple");
    var original = Mock.from(target);
    target.insertTextBeforeCaret("s");

    /* It's not exactly black box, but presently we don't NEED the keyEvent object for the method to work.
     * Other modules coming later will need it, though.
     */
    var transcription = target.buildTranscriptionFrom(original, null);

    assert.notEqual(transcription.preInput, original, "The transcription's input snapshot is an alias");
    assert.notEqual(transcription.preInput, target, "The transcription's input snapshot is an alias");
  })

  describe("Plain text operations", function() {
    it("handles context-free single-char output rules", function() {
      // We have other texts validating Mocks; by using them as our base 'element', this unit test file
      // could eventually run in 'headless' mode.
      var target = new Mock("apple");
      var original = Mock.from(target);
      target.insertTextBeforeCaret("s");

      /* It's not exactly black box, but presently we don't NEED the keyEvent object for the method to work.
      * Other modules coming later will need it, though.
      */
      var transcription = target.buildTranscriptionFrom(original, null);

      assert.equal(transcription.transform.insert, "s", "Failed to recognize inserted text");
      assert.equal(transcription.transform.deleteLeft, 0, "Incorrectly detected left-of-caret deletions");
      assert.equal(transcription.transform.deleteRight, 0, "Incorrectly detected right-of-caret deletions");

      target = new Mock("apple", 3);
      original = Mock.from(target);
      target.insertTextBeforeCaret("s"); // "appsle"

      var transcription = target.buildTranscriptionFrom(original, null);

      assert.equal(transcription.transform.insert, "s", "Failed to recognize inserted text when right-of-caret text exists");
      assert.equal(transcription.transform.deleteLeft, 0);
      assert.equal(transcription.transform.deleteRight, 0);
    });

    it("handles operations with moderately long text", function() {
      var target = new Mock("The quick brown cat jumped onto the lazy dog.", 19);
      var original = Mock.from(target);
      target.setSelection(30); // 19 + 11:  moves it to after "onto".
      target.deleteCharsBeforeCaret(14); // delete:  "cat jumped onto"
      target.insertTextBeforeCaret("fox jumped over");

      /* It's not exactly black box, but presently we don't NEED the keyEvent object for the method to work.
      * Other modules coming later will need it, though.
      */
      var transcription = target.buildTranscriptionFrom(original, null);

      assert.equal(transcription.transform.insert, "fox jumped over", "Reported inserted text when only deletions exist");
      assert.equal(transcription.transform.deleteLeft, 3, "Incorrect count for left-of-caret deletions");
      assert.equal(transcription.transform.deleteRight, 11, "Incorrect count for right-of-caret deletions");
    });

    it("handles operations with long text", function() {
      // Eh... had to pick SOMETHING.
      let text = `Did you ever hear the Tragedy of Darth Plagueis the wise? I thought not.
It's not a story the Jedi would tell you. It's a Sith legend. Darth Plagueis was a
Dark Lord of the Sith, so powerful and so wise he could use the Force to influence
the midichlorians to create life... He had such a knowledge of the dark side that
he could even keep the ones he cared about from dying. The dark side of the Force
is a pathway to many abilities some consider to be unnatural. He became so powerful...
the only thing he was afraid of was losing his power, which eventually, of course,
he did. Unfortunately, he taught his apprentice everything he knew, then his
apprentice killed him in his sleep. It's ironic he could save others from death,
but not himself.`;  // Sheev Palpatine, in the Star Wars prequels.

      var target = new Mock(text, text.length);
      var original = Mock.from(target);
      target.deleteCharsBeforeCaret(1);
      target.insertTextBeforeCaret("!");

      /* It's not exactly black box, but presently we don't NEED the keyEvent object for the method to work.
      * Other modules coming later will need it, though.
      */
      var transcription = target.buildTranscriptionFrom(original, null);

      assert.equal(transcription.transform.insert, "!", "Reported inserted text when only deletions exist");
      assert.equal(transcription.transform.deleteLeft, 1, "Incorrect count for left-of-caret deletions");
      assert.equal(transcription.transform.deleteRight, 0, "Incorrect count for right-of-caret deletions");
    });

    it("handles deletions around the caret without text insertion", function() {
      var target = new Mock("apple", 2);
      var original = Mock.from(target);
      target.setSelection(3);
      target.deleteCharsBeforeCaret(2); // "ale"

      /* It's not exactly black box, but presently we don't NEED the keyEvent object for the method to work.
      * Other modules coming later will need it, though.
      */
      var transcription = target.buildTranscriptionFrom(original, null);

      assert.equal(transcription.transform.insert, "", "Reported inserted text when only deletions exist");
      assert.equal(transcription.transform.deleteLeft, 1, "Incorrect count for left-of-caret deletions");
      assert.equal(transcription.transform.deleteRight, 1, "Incorrect count for right-of-caret deletions");
    });

    it("handles deletions around the caret without text insertion (non-BMP text)", function() {
      try {
        String.kmwEnableSupplementaryPlane(true);
        var target = new Mock(smpApple, 2);
        var original = Mock.from(target);
        target.setSelection(3);
        target.deleteCharsBeforeCaret(2); // "ale"

        /* It's not exactly black box, but presently we don't NEED the keyEvent object for the method to work.
        * Other modules coming later will need it, though.
        */
        var transcription = target.buildTranscriptionFrom(original, null);

        assert.equal(transcription.transform.insert, "", "Reported inserted text when only deletions exist");
        assert.equal(transcription.transform.deleteLeft, 1, "Incorrect count for left-of-caret deletions");
        assert.equal(transcription.transform.deleteRight, 1, "Incorrect count for right-of-caret deletions");
      } finally {
        String.kmwEnableSupplementaryPlane(false);
      }
    });

    it("handles deletions around the caret with text insertion", function() {
      // We have other texts validating Mocks; by using them as our base 'element', this unit test file
      // could eventually run in 'headless' mode.
      var target = new Mock("apple", 2);
      var original = Mock.from(target);
      target.setSelection(3);
      target.deleteCharsBeforeCaret(2);
      target.insertTextBeforeCaret("PP"); // "aPPle"

      /* It's not exactly black box, but presently we don't NEED the keyEvent object for the method to work.
      * Other modules coming later will need it, though.
      */
      var transcription = target.buildTranscriptionFrom(original, null);

      assert.equal(transcription.transform.insert, "PP", "Reported inserted text when only deletions exist");
      assert.equal(transcription.transform.deleteLeft, 1, "Incorrect count for left-of-caret deletions");
      assert.equal(transcription.transform.deleteRight, 1, "Incorrect count for right-of-caret deletions");

      // CASE 2

      var target = new Mock("apple", 2);
      var original = Mock.from(target);
      target.setSelection(4);
      target.deleteCharsBeforeCaret(3);
      target.insertTextBeforeCaret("P"); // "aPe"

      /* It's not exactly black box, but presently we don't NEED the keyEvent object for the method to work.
      * Other modules coming later will need it, though.
      */
      var transcription = target.buildTranscriptionFrom(original, null);

      assert.equal(transcription.transform.insert, "P", "Reported inserted text when only deletions exist");
      assert.equal(transcription.transform.deleteLeft, 1, "Incorrect count for left-of-caret deletions");
      assert.equal(transcription.transform.deleteRight, 2, "Incorrect count for right-of-caret deletions");

      // CASE 3

      var target = new Mock("apple", 2);
      var original = Mock.from(target);
      target.setSelection(4);
      target.deleteCharsBeforeCaret(3);
      target.insertTextBeforeCaret("aaaaaaaaaaaaaa"); // "aaaaaaaaaaaaaaae"

      /* It's not exactly black box, but presently we don't NEED the keyEvent object for the method to work.
      * Other modules coming later will need it, though.
      */
      var transcription = target.buildTranscriptionFrom(original, null);

      assert.equal(transcription.transform.insert, "aaaaaaaaaaaaaa", "Reported inserted text when only deletions exist");
      assert.equal(transcription.transform.deleteLeft, 1, "Incorrect count for left-of-caret deletions");
      assert.equal(transcription.transform.deleteRight, 2, "Incorrect count for right-of-caret deletions");

      // CASE 4

      var target = new Mock("apple", 2);
      var original = Mock.from(target);
      target.setSelection(5);
      target.deleteCharsBeforeCaret(4);
      target.insertTextBeforeCaret("les"); // "ales" - since we've appended a letter at the very end, the whole right-hand is indeed an insertion.

      /* It's not exactly black box, but presently we don't NEED the keyEvent object for the method to work.
        * Other modules coming later will need it, though.
        */
      var transcription = target.buildTranscriptionFrom(original, null);

      // Again, while the "le" portion is original text, the appended "s" means it must have been re-inserted.
      assert.equal(transcription.transform.insert, "les", "Reported inserted text when only deletions exist");
      assert.equal(transcription.transform.deleteLeft, 1, "Incorrect count for left-of-caret deletions");
      assert.equal(transcription.transform.deleteRight, 3, "Incorrect count for right-of-caret deletions");
    });

    it("handles deletions around the caret with text insertion (non-BMP text)", function() {
      try {
        String.kmwEnableSupplementaryPlane(true);

        // We have other texts validating Mocks; by using them as our base 'element', this unit test file
        // could eventually run in 'headless' mode.
        var target = new Mock(smpApple, 2);
        let smpLE = u(0x1d5c5)+u(0x1d5be);
        var original = Mock.from(target);
        target.setSelection(3);
        target.deleteCharsBeforeCaret(2);
        target.insertTextBeforeCaret(smpLE); // "alele"

        /* It's not exactly black box, but presently we don't NEED the keyEvent object for the method to work.
        * Other modules coming later will need it, though.
        *
        * This will trigger the "step 1.2" component of buildTransformFrom.
        */
        var transcription = target.buildTranscriptionFrom(original, null);

        assert.equal(transcription.transform.insert, smpLE, "Reported inserted text when only deletions exist");
        assert.equal(transcription.transform.deleteLeft, 1, "Incorrect count for left-of-caret deletions");
        assert.equal(transcription.transform.deleteRight, 1, "Incorrect count for right-of-caret deletions");

        // CASE 2

        var target = new Mock(smpApple, 2);
        let smpB = u(0x1d5bb);
        var original = Mock.from(target);
        target.setSelection(4);
        target.deleteCharsBeforeCaret(3);
        target.insertTextBeforeCaret(smpB); // "aPe"

        /* It's not exactly black box, but presently we don't NEED the keyEvent object for the method to work.
        * Other modules coming later will need it, though.
        */
        var transcription = target.buildTranscriptionFrom(original, null);

        assert.equal(transcription.transform.insert, smpB, "Reported inserted text when only deletions exist");
        assert.equal(transcription.transform.deleteLeft, 1, "Incorrect count for left-of-caret deletions");
        assert.equal(transcription.transform.deleteRight, 2, "Incorrect count for right-of-caret deletions");

        // CASE 3

        var target = new Mock(smpApple, 2);
        var original = Mock.from(target);
        target.setSelection(4);
        target.deleteCharsBeforeCaret(3);
        target.insertTextBeforeCaret("aaaaaaaaaaaaaa"); // "aaaaaaaaaaaaaaae"

        /* It's not exactly black box, but presently we don't NEED the keyEvent object for the method to work.
        * Other modules coming later will need it, though.
        */
        var transcription = target.buildTranscriptionFrom(original, null);

        assert.equal(transcription.transform.insert, "aaaaaaaaaaaaaa", "Reported inserted text when only deletions exist");
        assert.equal(transcription.transform.deleteLeft, 1, "Incorrect count for left-of-caret deletions");
        assert.equal(transcription.transform.deleteRight, 2, "Incorrect count for right-of-caret deletions");

        // CASE 4

        var target = new Mock(smpApple, 2);
        let smpLES = u(0x1d5c5)+u(0x1d5be)+u(0x1d5cb);
        var original = Mock.from(target);
        target.setSelection(5);
        target.deleteCharsBeforeCaret(4);
        target.insertTextBeforeCaret(smpLES); // "ales" - since we've appended a letter at the very end, the whole right-hand is indeed an insertion.

        /* It's not exactly black box, but presently we don't NEED the keyEvent object for the method to work.
          * Other modules coming later will need it, though.
          */
        var transcription = target.buildTranscriptionFrom(original, null);

        // Again, while the "le" portion is original text, the appended "s" means it must have been re-inserted.
        assert.equal(transcription.transform.insert, smpLES, "Reported inserted text when only deletions exist");
        assert.equal(transcription.transform.deleteLeft, 1, "Incorrect count for left-of-caret deletions");
        assert.equal(transcription.transform.deleteRight, 3, "Incorrect count for right-of-caret deletions");
      } finally {
        String.kmwEnableSupplementaryPlane(false);
      }
    });

    it('from targets with existing selection', () => {
      //                              |            |
      const target = new Mock("testing testing one two three");
      target.setSelection(8, 20)
      const original = Mock.from(target);
      target.clearSelection();

      const transform = target.buildTransformFrom(original);
      assert.deepEqual(transform, {
        insert: '',
        deleteLeft: 0,
        deleteRight: 0,
        erasedSelection: true
      });
    });

    it('to targets with existing selection', () => {
      //                              |            |
      const target = new Mock("testing testing one two three");
      target.setSelection(8, 20)
      const transform = {
        insert: '',
        deleteLeft: 0,
        deleteRight: 0,
        erasedSelection: true
      };

      target.apply(transform);
      assert.equal(target.getText(), 'testing two three');
    });
  });

  /*describe("Operations with deadkeys", function() {
    // Just one, less nuanced/subdivided; it's not a present priority for our work, but it should provide a decent basis if/when it's needed.
    it("Correctly recognizes deadkey set mutations", function() {

      // We have other texts validating Mocks; by using them as our base 'element', this unit test file
      // could eventually run in 'headless' mode.
      var target = new Mock("apple");
      var original = Mock.from(target);

      target.setSelection(4);
      target.insertDeadkeyBeforeCaret(0);
      target.setSelection(1);
      target.insertDeadkeyBeforeCaret(1);
      target.setSelection(2);
      target.insertDeadkeyBeforeCaret(2); // 'a' dk(1) 'p' dk(2) | 'p' 'l' dk(0) 'e'

      var original = Mock.from(target);

      target.hasDeadkeyMatch(0, 2);
      target.deadkeys().deleteMatched();

      target.setSelection(3);
      target.deleteCharsBeforeCaret(2);
      target.insertTextBeforeCaret("b");
      target.insertDeadkeyBeforeCaret(3); // In effect: 'a' dk(1) 'b' dk(3) | 'l' dk(0) 'e'

      var transcription = target.buildTranscriptionFrom(original, null);

      assert.equal(transcription.transform.insert, "b", "Reported inserted text when only deletions exist");
      assert.equal(transcription.transform.deleteLeft, 1, "Incorrect count for left-of-caret deletions");
      assert.equal(transcription.transform.deleteRight, 1, "Incorrect count for right-of-caret deletions");

      //removedDks, insertedDks
      var rem = transcription.removedDks;
      assert.equal(rem.length, 1, "Incorrect count for removed deadkeys");
      assert.deepEqual({d: rem[0].d, p: rem[0].p}, {d: 2, p: 2}, "Selected wrong deadkey as removed");

      var ins = transcription.insertedDks;
      assert.equal(ins.length, 1, "Incorrect count for inserted deadkeys");
      assert.deepEqual({d: ins[0].d, p: ins[0].p}, {d: 3, p:2}, "Selected wrong deadkey as inserted");
    });
  });
  */
});