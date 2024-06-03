import { CompilerEvent } from '@keymanapp/common-types';
import 'mocha';
import {assert} from 'chai';
import { CompilerEventOrMatch, matchCompilerEvents } from './helpers/index.js';
import { CompilerMessages } from '../src/compiler/messages.js';


describe('test of test/helpers/CompilerMatch', () => {
    /** the message(s) to test */
    const messages : CompilerEvent[] = [
        CompilerMessages.Error_InvalidHardware({ formId: 'outdated' }),
        CompilerMessages.Hint_NormalizationDisabled(),
    ];

    it('should work for exact matches', () => {
        const spec : CompilerEventOrMatch[] = [
            CompilerMessages.Hint_NormalizationDisabled(),
            CompilerMessages.Error_InvalidHardware({ formId: 'outdated' }),
        ];
        const matched = matchCompilerEvents(messages, spec);
        assert.sameDeepMembers(messages, spec); // exact match works here
        assert.sameDeepMembers(messages, matched);
    });
    it('should work for a fuzzy matches', () => {
        const spec : CompilerEventOrMatch[] = [
            // mixed messages here - this one is a CompilerEvent
            CompilerMessages.Hint_NormalizationDisabled(),
            // fuzzy match on this one - CompilerMatch
            { code: CompilerMessages.ERROR_InvalidHardware, matchMessage: /(out|up|inun)dated/ },
        ];
        const matched = matchCompilerEvents(messages, spec);
        assert.sameDeepMembers(messages, matched);
    });
});
