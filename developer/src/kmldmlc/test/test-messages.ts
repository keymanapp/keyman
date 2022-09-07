import 'mocha';
import {assert, expect} from 'chai';
import { CompilerErrorSeverity, CompilerMessages } from '../src/keyman/compiler/messages';

const toTitleCase = (s: string) => s.substring(0, 1).toUpperCase() + s.substring(1).toLowerCase();

//
// The purpose of these tests is to ensure that our slightly WET constant
// definitions for compiler errors are consistent in their usage:
//
//   * that names are consistent between the message functions and constants
//   * that constants have unique codes
//   * that constants have the right error mask for their type
//
// This means we don't have to worry about DRYing out the constants, which would
// probably require us to have a preprocessing step, which has its own negatives.
//

describe('compiler messages', function () {
  it('each compiler message function has a corresponding constant', function() {
    let keys = Object.keys(CompilerMessages);

    const m = CompilerMessages as Record<string,any>;

    for(let key of keys) {
      if(key == 'severityName') {
        // any helper functions we skip here
        continue;
      }

      if(typeof m[key] == 'function') {
        const o = /^(Info|Hint|Warning|Error|Fatal)_([A-Za-z0-9_]+)$/.exec(key);
        expect(o).to.be.instanceOf(Array);

        const c = o[1].toUpperCase() + '_' + o[2];
        expect(m[c]).to.be.a('number', `Expected constant name ${c} to exist`);

        let v = m[key]('','','','','','','','','','','','' /* ignore arguments*/);
        expect(v.code).to.equal(m[c], `Function ${key} returns the wrong code`);
      }
      else if(typeof m[key] == 'number') {
        const o = /^(INFO|HINT|WARNING|ERROR|FATAL)_([A-Za-z0-9_]+)$/.exec(key);
        expect(o).to.be.instanceOf(Array);

        const f = toTitleCase(o[1]) + '_' + o[2];
        expect(m[f]).to.be.a('function', `Expected function name ${f} to exist`);
      } else {
        assert.fail(`Unexepected compiler message member ${key}`);
      }
    }
  });

  it('each compiler message constant to be have the right mask and be unique', function() {
    let keys = Object.keys(CompilerMessages);

    const m = CompilerMessages as Record<string,any>;

    let codes: number[] = [];

    for(let key of keys) {
      if(typeof m[key] == 'number') {
        const o = /^(INFO|HINT|WARNING|ERROR|FATAL)_([A-Za-z0-9_]+)$/.exec(key);
        expect(o).to.be.instanceOf(Array);

        const mask = CompilerMessages.severityName(m[key]);
        expect(o[1]).to.equal(mask, `Mask value for ${key} does not match`);

        const code = m[key] & CompilerErrorSeverity.Error_Mask;
        expect(codes).to.not.contain(code, `Constant value ${key} is not unique`);
        codes.push(code);
      }
    }
  });
});