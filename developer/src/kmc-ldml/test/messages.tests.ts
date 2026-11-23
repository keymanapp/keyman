import 'mocha';
import {expect} from 'chai';
import { LdmlCompilerMessages } from '../src/compiler/ldml-compiler-messages.js';
import { verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';
import { CompilerErrorNamespace, CompilerEvent, withOffset } from '@keymanapp/developer-utils';

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
    const noLines = new Set<string>();
    /** takes line numbers */
    let lines = 0;
    const fakeOffsetNumber = 1234;
    const fakeOffsetObject = withOffset(fakeOffsetNumber);
    for(const key of keys) {
      // exclude these ones, do not need line numbers
      if (key == 'Error_InvalidFile' || key == 'Error_InvalidTargetVersion') continue;
      if (typeof m[key] == 'function') {
        total++;
        const f = m[key] as Function;
        // console.log(`${f.name}: ${f.length}`);
        if (f.length === 0) { // Error_foo()
          noLines.add(key);
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
          noLines.add(key);
        }
      }
    }
    expect(lines).to.not.be.equal(0, `None of ${total} messages had offset reporting.`);
    expect(Array.from(noLines.values())).to.deep.equal([], `${noLines.size}/${total} ${Number((noLines.size/total)*100).toFixed(0)}% of message(s) did not have detectable offset (line number) reporting`);
  });
  it('Should only have simple ${def(o...)} interpolation', () => {
      const m = LdmlCompilerMessages as Record<string,any>;
    const keys = Object.keys(LdmlCompilerMessages);
    const complexInterpolation : string[] = [];
    for(const key of keys) {
      // exclude this one, does not need line numbers
      if (key == 'Error_InvalidFile') continue;
      if (typeof m[key] == 'function') {
        const f = m[key] as Function;
        const s = f.toString();
        // scan for interpolation
        // Interpolation.. A non-escaped ${ that's NOT followed by `def(o.` triggers this error.
        if (/.*`.*(?<!\\)(?:\\\\)*\$\{(?!def\(o\.)/.test(s)) {
          complexInterpolation.push(key);
        }
      }
    }
    expect(complexInterpolation).to.deep.equal([], 'Complex interpolation found, please only use the ${def(o... form')
  });
});
