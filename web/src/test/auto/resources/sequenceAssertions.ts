import { assert } from 'chai'
import sinon from 'sinon';

import * as PromiseStatusModule from 'promise-status-async';
const PromiseStatuses     = PromiseStatusModule.PromiseStatuses;
import { assertingPromiseStatus as promiseStatus } from './assertingPromiseStatus.js';

import { GestureSource, gestures } from '@keymanapp/gesture-recognizer';
const { matchers } = gestures;

// Huh... gotta do BOTH here?  One for constructor use, the other for generic-parameter use?
const { GestureSequence, GestureStageReport } = matchers;
type GestureSequence<Type> = gestures.matchers.GestureSequence<Type>;
type GestureStageReport<Type> = gestures.matchers.GestureStageReport<Type>;

import { ManagedPromise } from '@keymanapp/web-utils';

export interface StageReportAssertion<Type> {
  matchedId: string,
  item?: Type,
  linkType?: typeof GestureStageReport.prototype['linkType']
  sources?: (sources: GestureSource<Type>[]) => void;
}

export type SequenceAssertion<Type> = StageReportAssertion<Type>[];

export async function assertGestureSequence<Type>(
  sequence: GestureSequence<Type>,
  emulationCompletion: Promise<void>,
  reportAssertions: StageReportAssertion<Type>[]
) {
  const completionCheck = sinon.fake();
  const stagePromises: ManagedPromise<GestureStageReport<Type>>[] = [
    new ManagedPromise()
  ];

  sequence.on('stage', (report) => {
    stagePromises[stagePromises.length - 1].resolve(report);
    stagePromises.push(new ManagedPromise());
  });
  sequence.on('complete', completionCheck);

  let index: number;
  for(index = 0; index < reportAssertions.length; index++) {
    // Assert that the expected stage actually occurs for the simulated sequence.
    await Promise.race([stagePromises[index].corePromise, emulationCompletion]);
    assert.equal(await promiseStatus(stagePromises[index].corePromise), PromiseStatuses.PROMISE_RESOLVED, `Expected gesture stage with index ${index} did not occur`);

    // Assert that the detected stage has the expected properties for the simulated sequence.
    const report = await stagePromises[index].corePromise;
    const assertValue = reportAssertions[index];
    const expectation = `Expected stage (index ${index}, id ${assertValue.matchedId})`;
    assert.equal(report.matchedId, assertValue.matchedId, `${expectation} did not match expected type`);
    if(assertValue.item !== undefined) {
      if(assertValue.item) {
        assert.equal(report.item, assertValue.item, `${expectation} did not result in expected item`);
      } else {
        assert.equal(report.item, assertValue.item, `${expectation} resulted in unexpected item`);
      }
    }
    if(assertValue.linkType !== undefined) {
      assert.equal(report.linkType, assertValue.linkType, `${expectation} specified an unexpected stage transition type`);
    }
    if(assertValue.sources) {
      assertValue.sources(report.sources);
    }
  }

  // There should be no unexpected stage in the sequence's analysis; we should reach completion
  // with the last specified stage.
  await Promise.race([stagePromises[index].corePromise, emulationCompletion]);
  assert.equal(await promiseStatus(emulationCompletion), PromiseStatuses.PROMISE_RESOLVED, `Unexpected stage with index ${index}; sequence should have terminated`);
  assert.equal(await promiseStatus(stagePromises[index].corePromise), PromiseStatuses.PROMISE_PENDING, `Unexpected stage with index ${index}; sequence should have terminated`);

  await emulationCompletion;
  await Promise.resolve();

  assert.isTrue(completionCheck.called, `Sequence ${index} did not reach completion by the end of emulation`);   // issue:  simple-tap tests could, in theory, still go multi-tap!
}