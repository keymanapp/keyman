import { CompilerEvent } from '@keymanapp/common-types';
import 'mocha';
import {assert} from 'chai';
import { CompilerEventOrMatch, matchCompilerEvents } from './helpers/index.js';
import { LdmlCompilerMessages } from '../src/compiler/ldml-compiler-messages.js';


describe('test of test/helpers/CompilerMatch', () => {
    /** the message(s) to test */
    const messages : CompilerEvent[] = [
        LdmlCompilerMessages.Error_InvalidHardware({ formId: 'outdated' }),
        LdmlCompilerMessages.Hint_NormalizationDisabled(),
    ];

    it('should work for exact matches', () => {
        const spec : CompilerEventOrMatch[] = [
            LdmlCompilerMessages.Hint_NormalizationDisabled(),
            LdmlCompilerMessages.Error_InvalidHardware({ formId: 'outdated' }),
        ];
        const matched = matchCompilerEvents(messages, spec);
        assert.sameDeepMembers(messages, spec); // exact match works here
        assert.sameDeepMembers(messages, matched);
    });
    it('should work for a fuzzy matches', () => {
        const spec : CompilerEventOrMatch[] = [
            // mixed messages here - this one is a CompilerEvent
            LdmlCompilerMessages.Hint_NormalizationDisabled(),
            // fuzzy match on this one - CompilerMatch
            { code: LdmlCompilerMessages.ERROR_InvalidHardware, matchMessage: /(out|up|inun)dated/ },
        ];
        const matched = matchCompilerEvents(messages, spec);
        assert.sameDeepMembers(messages, matched);
    });
});
