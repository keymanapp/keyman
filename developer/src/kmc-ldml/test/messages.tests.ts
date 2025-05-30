import 'mocha';
import {expect} from 'chai';
import { LdmlCompilerMessages } from '../src/compiler/ldml-compiler-messages.js';
import { verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';
import { CompilerErrorNamespace, CompilerEvent } from '@keymanapp/developer-utils';
import { withOffset } from './helpers/index.js';

describe('LdmlCompilerMessages', function () {
  it('should have a valid LdmlCompilerMessages object', function() {
    return verifyCompilerMessagesObject(LdmlCompilerMessages, CompilerErrorNamespace.LdmlKeyboardCompiler);
  });
  it('should have offset (line) reporting on all messages', function() {
    const m = LdmlCompilerMessages as Record<string,any>;
    const keys = Object.keys(LdmlCompilerMessages);
    /** all fns */
    let total = 0;
    /** does not take line numbers */
    let noLines = 0;
    /** takes line numbers */
    let lines = 0;
    const fakeOffsetNumber = 1234;
    const fakeOffsetObject = withOffset(fakeOffsetNumber);
    for(const key of keys) {
      if(typeof m[key] == 'function') {
        total++;
        const f = m[key] as Function;
        // console.log(`${f.name}: ${f.length}`);
        if (f.length === 0) { // Error_foo()
          noLines++;
          continue;
        }
        // now try to call it
        let resp : CompilerEvent;
        if (f.length === 1) { // Error_foo(x)
          resp = f(fakeOffsetObject);
        } else if(f.length >= 2) { // Error_foo(o, x)
          resp = f({}, fakeOffsetObject);
        }
        expect(resp).to.be.ok; // should get an object one way or another
        if(resp.offset) {
          lines++;
          expect(resp.offset).to.equal(fakeOffsetNumber, `Offset number round trip for error ${f.name} did not work, check the message function`);
        } else {
          // did not get a column number back
          noLines++;
        }
      }
    }
    expect(lines).to.not.be.equal(0, `None of ${total} messages had offset reporting.`);
    if (noLines > 0) {
      // Once this goes to zero, make it an error if it goes up!
      // Oh, and while you're here, once this is zero, uncomment the code in testCompilationCases
      // that asserts that all messages are actually generated with an offset.
      console.warn(`TODO-LDML (#10622) ${noLines}/${total} ${Number((noLines/total)*1000).toFixed(0)}‰ of message(s) did not have detectable offset (line number) reporting.`);
    }
  });
});
