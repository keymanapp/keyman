var assert = require('chai').assert;
let KeyboardProcessor = require('../../dist');

// Required initialization setup.
global.com = KeyboardProcessor.com; // exports all keyboard-processor namespacing.

String.kmwEnableSupplementaryPlane(false);

describe("Transcriptions and Transforms", function() {
  it("does not store an alias for related OutputTargets", function() {
    var Mock = com.keyman.text.Mock;

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
      var Mock = com.keyman.text.Mock;

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

    it("handles deletions around the caret without text insertion", function() {
      var Mock = com.keyman.text.Mock;

      // We have other texts validating Mocks; by using them as our base 'element', this unit test file
      // could eventually run in 'headless' mode.
      var target = new Mock("apple", 2);
      var original = Mock.from(target);
      target.setDeadkeyCaret(3);
      target.deleteCharsBeforeCaret(2); // "ale"

      /* It's not exactly black box, but presently we don't NEED the keyEvent object for the method to work.
      * Other modules coming later will need it, though.
      */
      var transcription = target.buildTranscriptionFrom(original, null);

      assert.equal(transcription.transform.insert, "", "Reported inserted text when only deletions exist");
      assert.equal(transcription.transform.deleteLeft, 1, "Incorrect count for left-of-caret deletions");
      assert.equal(transcription.transform.deleteRight, 1, "Incorrect count for right-of-caret deletions");
    });

    it("handles deletions around the caret with text insertion", function() {
      var Mock = com.keyman.text.Mock;

      // We have other texts validating Mocks; by using them as our base 'element', this unit test file
      // could eventually run in 'headless' mode.
      var target = new Mock("apple", 2);
      var original = Mock.from(target);
      target.setDeadkeyCaret(3);
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
      target.setDeadkeyCaret(4);
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
      target.setDeadkeyCaret(4);
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
      target.setDeadkeyCaret(5);
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
  });

  /*describe("Operations with deadkeys", function() {
    // Just one, less nuanced/subdivided; it's not a present priority for our work, but it should provide a decent basis if/when it's needed.
    it("Correctly recognizes deadkey set mutations", function() {
      var Mock = com.keyman.text.Mock;

      // We have other texts validating Mocks; by using them as our base 'element', this unit test file
      // could eventually run in 'headless' mode.
      var target = new Mock("apple");
      var original = Mock.from(target);
      
      target.setDeadkeyCaret(4);
      target.insertDeadkeyBeforeCaret(0);
      target.setDeadkeyCaret(1);
      target.insertDeadkeyBeforeCaret(1); 
      target.setDeadkeyCaret(2);
      target.insertDeadkeyBeforeCaret(2); // 'a' dk(1) 'p' dk(2) | 'p' 'l' dk(0) 'e'

      var original = Mock.from(target);

      target.hasDeadkeyMatch(0, 2);
      target.deadkeys().deleteMatched();

      target.setDeadkeyCaret(3);
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