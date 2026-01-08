/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { assert } from 'chai';
import 'mocha';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { AnalyzeOskCharacterUse } from '../src/osk-character-use/index.js';
import { AnalyzerMessages } from '../src/analyzer-messages.js';
import { makePathToFixture } from './helpers/index.js';

describe('AnalyzeOskCharacterUse warnings', function() {
  const callbacks = new TestCompilerCallbacks();

  const MOCK_MAP_NO_COUNTS = makePathToFixture(
    'osk-character-use',
    'mock-map-no-counts.json'
  );
  const MOCK_MAP_WITH_COUNTS = makePathToFixture(
    'osk-character-use',
    'mock-map-with-counts.json'
  );

  this.beforeEach(function() {
    callbacks.clear();
  });

  this.afterEach(function() {
    if (this.currentTest?.isFailed()) {
      callbacks.printMessages();
    }
  });

  it('warns if previous map did not include counts but includeCounts=true', function() {
    const a = new AnalyzeOskCharacterUse(callbacks, {
      includeCounts: true
    });

    const result = a.unitTestEndPoints.loadPreviousMap(MOCK_MAP_NO_COUNTS);
    assert.isNotNull(result, 'Expected map to be loaded successfully');
    
    assert.isTrue(
      callbacks.hasMessage(AnalyzerMessages.WARN_PreviousMapDidNotIncludeCounts),
      'Expected Warn_PreviousMapDidNotIncludeCounts warning'
    );
  });

  it('warns if previous map did include counts but includeCounts=false', function() {
    const a = new AnalyzeOskCharacterUse(callbacks, {
      includeCounts: false
    });

    const result = a.unitTestEndPoints.loadPreviousMap(MOCK_MAP_WITH_COUNTS);
    assert.isNotNull(result, 'Expected map to be loaded successfully');

    assert.isTrue(
      callbacks.hasMessage(AnalyzerMessages.WARN_PreviousMapDidIncludeCounts),
      'Expected Warn_PreviousMapDidIncludeCounts warning'
    );
  });
});
