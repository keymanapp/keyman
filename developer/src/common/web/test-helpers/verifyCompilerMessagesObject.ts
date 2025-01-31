import { CompilerError, CompilerErrorMask } from '@keymanapp/developer-utils';
import {assert, expect} from 'chai';

//
// The purpose of these tests is to ensure that our slightly WET constant
// definitions for compiler errors are consistent in their usage:
//
//   * that names are consistent between the message functions and constants
//   * that constants have unique codes
//   * that constants have the right error mask for their type
//   * that constants don't creep outside their namespace
//
// This means we don't have to worry about DRYing out the constants, which would
// probably require us to have a preprocessing step, which has its own
// negatives.
//
// These are used in each of the test-messages.ts units to verify the
// CompilerMessages classes

const toTitleCase = (s: string) => s.substring(0, 1).toUpperCase() + s.substring(1).toLowerCase();

export function verifyCompilerMessagesObject(source: Record<string,any>, namespace: number) {
  const keys = Object.keys(source);

  const m = source as Record<string,any>;

  const codes: number[] = [];

  for(const key of keys) {

    // Verify each object member matches the pattern we expect

    if(typeof m[key] == 'function') {
      const o = /^(Debug|Verbose|Info|Hint|Warn|Error|Fatal)_([A-Za-z0-9_]+)$/.exec(key);
      expect(o).to.be.instanceOf(Array, `Expected member ${key} to be a valid message function name`);

      const c = o[1].toUpperCase() + '_' + o[2];
      expect(m[c]).to.be.a('number', `Expected constant name ${c} to exist`);

      const v = m[key]('','','','','','','','','','','','' /* ignore arguments*/);
      expect(v.code).to.equal(m[c], `Function ${key} returns the wrong code`);
    }
    else if(typeof m[key] == 'number') {
      const o = /^(DEBUG|VERBOSE|INFO|HINT|WARN|ERROR|FATAL)_([A-Za-z0-9_]+)$/.exec(key);
      expect(o).to.be.instanceOf(Array);

      const f = toTitleCase(o[1]) + '_' + o[2];
      expect(m[f]).to.be.a('function', `Expected function name ${f} to exist`);
    } else {
      assert.fail(`Unexepected compiler message member ${key}`);
    }

    // Verify severify masks

    if(typeof m[key] == 'number') {
      const o = /^(DEBUG|VERBOSE|INFO|HINT|WARN|ERROR|FATAL)_([A-Za-z0-9_]+)$/.exec(key);
      expect(o).to.be.instanceOf(Array);

      const mask = CompilerError.formatSeverity(m[key]).toUpperCase();
      expect(o[1]).to.equal(mask, `Mask value for ${key} does not match`);

      expect(m[key] & CompilerErrorMask.Reserved).to.equal(0, `Constant value ${key} uses a reserved value`);

      const code = m[key] & CompilerErrorMask.Error;
      expect(codes).to.not.contain(code, `Constant value ${key} is not unique`);
      expect(m[key] & CompilerErrorMask.Namespace).to.equal(namespace,
        `Constant value ${key} is not in the correct namespace (0x${namespace.toString(16)})`);
      codes.push(code);
    }
  }
}
