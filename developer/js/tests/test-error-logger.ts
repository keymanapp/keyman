import { MAX_MESSAGES, KeymanCompilerError, log } from "../dist/errors"
import { LogHoarder } from "./helpers"
import { assert } from "chai"

describe('error logger', function () {
  beforeEach(function () {
    this.logHoarder = (new LogHoarder).install()
  })

  afterEach(function () {
    this.logHoarder.uninstall();
    delete this.logHoarder;
  })

  it('should stop logging messages **after** a maximum', function () {
    for (let i = 0; i < MAX_MESSAGES; i++) {
      log(KeymanCompilerError.CWARN_DuplicateWordInSameFile, "fake error");
    }

    // We've logged *just enough messages. This error should not be found:
    assert.isFalse(this.logHoarder.hasSeenCode(
      KeymanCompilerError.CWARN_TooManyErrorsOrWarnings
    ));

    // Log just one too many:
    log(KeymanCompilerError.CWARN_DuplicateWordInSameFile, "fake error");

    assert.isTrue(this.logHoarder.hasSeenCode(
      KeymanCompilerError.CWARN_TooManyErrorsOrWarnings
    ));

    // Log a DIFFERENT error -- it should not appear in the log
    log(KeymanCompilerError.CWARN_MixedNormalizationForms, "fake error");
    assert.isFalse(this.logHoarder.hasSeenCode(
      KeymanCompilerError.CWARN_MixedNormalizationForms
    ));
  })
})