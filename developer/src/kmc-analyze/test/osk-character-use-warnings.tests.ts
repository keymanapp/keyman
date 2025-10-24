/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import * as fs from 'fs';
import { assert } from 'chai';
import 'mocha';
import { AnalyzeOskCharacterUse } from '../src/osk-character-use/index.js';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { AnalyzerMessages } from '../src/analyzer-messages.js';

describe('AnalyzeOskCharacterUse warnings', function() {
    const callbacks = new TestCompilerCallbacks();
    const TMP_NO_COUNTS = '/tmp/mock-map-no-counts.json';
    const TMP_WITH_COUNTS = '/tmp/mock-map-with-counts.json';

    before(() => {
        fs.writeFileSync(TMP_NO_COUNTS, JSON.stringify({
            map: [{ usages: ['U+1780', 'U+1781'] }]
        }));
        fs.writeFileSync(TMP_WITH_COUNTS, JSON.stringify({
            map: [{ usages: [{ char: 'U+1780', count: 2 }] }]
        }));
    });

    beforeEach(() => callbacks.clear());

    it('warns if previous map did not include counts but includeCounts=true', function() {
        const a = new AnalyzeOskCharacterUse(callbacks, {
            includeCounts: true
        });

        a.unitTestEndPoints.loadPreviousMap(TMP_NO_COUNTS);

        assert.isTrue(
            callbacks.hasMessage(AnalyzerMessages.WARN_PreviousMapDidNotIncludeCounts),
            'Expected Warn_PreviousMapDidNotIncludeCounts warning'
        );
    });

    it('warns if previous map did include counts but includeCounts=false', function() {
        const a = new AnalyzeOskCharacterUse(callbacks, {
            includeCounts: false
        });

        a.unitTestEndPoints.loadPreviousMap(TMP_WITH_COUNTS);

        assert.isTrue(
            callbacks.hasMessage(AnalyzerMessages.WARN_PreviousMapDidIncludeCounts),
            'Expected Warn_PreviousMapDidIncludeCounts warning'
        );
    });
});
