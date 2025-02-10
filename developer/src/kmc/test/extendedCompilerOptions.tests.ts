import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { assert } from 'chai';
import 'mocha';
import { unitTestEndpoints } from '../src/util/extendedCompilerOptions.js';
import { InfrastructureMessages } from '../src/messages/infrastructureMessages.js';
import { CompilerError, CompilerMessageOverride, CompilerErrorSeverity } from '@keymanapp/developer-utils';
import { KmnCompilerMessages } from '@keymanapp/kmc-kmn';

interface MessageTest {input: string, result: CompilerMessageOverride};
interface InvalidMessageTest {input: string, code: number};

describe('commandOptionsMessageToCompilerOptionsMessage', function () {
  const callbacks = new TestCompilerCallbacks();
  this.beforeEach(function() {
    callbacks.clear();
  });

  const valid: MessageTest[] = [
    // Test the allowable coercions
    {input: 'KM05002:DISABLE', result: {code: InfrastructureMessages.INFO_BuildingFile, level: 'disable' } },
    {input: 'KM05002:HINT', result: {code: InfrastructureMessages.INFO_BuildingFile, level: CompilerErrorSeverity.Hint } },
    {input: 'KM05002:INFO', result: {code: InfrastructureMessages.INFO_BuildingFile, level: CompilerErrorSeverity.Info } },
    {input: 'KM05002:WARN', result: {code: InfrastructureMessages.INFO_BuildingFile, level: CompilerErrorSeverity.Warn } },
    {input: 'KM05002:ERROR', result: {code: InfrastructureMessages.INFO_BuildingFile, level: CompilerErrorSeverity.Error } },

    // Test hint and warn messages
    {input: 'KM05009:E', result: {code: InfrastructureMessages.HINT_FilenameHasDifferingCase, level: CompilerErrorSeverity.Error } },
    {input: 'KM02082:E', result: {code: KmnCompilerMessages.WARN_BitmapNotUsed, level: CompilerErrorSeverity.Error } },

    // Test different allowable patterns
    {input: '5002', result: {code: InfrastructureMessages.INFO_BuildingFile, level: 'disable' } },
    {input: '5002:D', result: {code: InfrastructureMessages.INFO_BuildingFile, level: 'disable' } },
    {input: '5002:H', result: {code: InfrastructureMessages.INFO_BuildingFile, level: CompilerErrorSeverity.Hint } },
    {input: '5002:I', result: {code: InfrastructureMessages.INFO_BuildingFile, level: CompilerErrorSeverity.Info } },
    {input: '5002:W', result: {code: InfrastructureMessages.INFO_BuildingFile, level: CompilerErrorSeverity.Warn } },
    {input: '5002:E', result: {code: InfrastructureMessages.INFO_BuildingFile, level: CompilerErrorSeverity.Error } },
    {input: '05002', result: {code: InfrastructureMessages.INFO_BuildingFile, level: 'disable' } },
    {input: '05002:H', result: {code: InfrastructureMessages.INFO_BuildingFile, level: CompilerErrorSeverity.Hint } },
    {input: '05002:I', result: {code: InfrastructureMessages.INFO_BuildingFile, level: CompilerErrorSeverity.Info } },
    {input: '05002:W', result: {code: InfrastructureMessages.INFO_BuildingFile, level: CompilerErrorSeverity.Warn } },
    {input: '05002:E', result: {code: InfrastructureMessages.INFO_BuildingFile, level: CompilerErrorSeverity.Error } },
  ];
  for(const test of valid) {
    it(`should recognize valid message definition ${test.input}`, () => {
      const result = unitTestEndpoints.commanderOptionsMessageToCompilerOptionsMessage(test.input, callbacks);
      callbacks.printMessages();
      assert.deepEqual(result, { code: CompilerError.error(test.result.code), level: test.result.level });
      assert.isEmpty(callbacks.messages);
    });
  }
  const invalid: InvalidMessageTest[] = [
    {input: 'K05002:D', code: InfrastructureMessages.ERROR_InvalidMessageFormat },
    {input: 'blah', code: InfrastructureMessages.ERROR_InvalidMessageFormat },
    {input: 'KM05002:X', code: InfrastructureMessages.ERROR_InvalidMessageFormat },
    {input: 'KM99002:D', code: InfrastructureMessages.ERROR_MessageNamespaceNotFound },
    {input: 'KM05FFF:D', code: InfrastructureMessages.ERROR_MessageCodeNotFound },
    {input: 'KM05019:D', code: InfrastructureMessages.ERROR_MessageCannotBeCoerced },
  ];
  for(const test of invalid) {
    it(`should reject invalid message definition ${test.input}`, () => {
      assert.isNull(unitTestEndpoints.commanderOptionsMessageToCompilerOptionsMessage(test.input, callbacks));
      assert.isTrue(callbacks.hasMessage(test.code));
    });
  }
});