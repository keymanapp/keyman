// @ts-check
import BASE_CONFIG from './web-test-runner.config.mjs';
import teamcityReporter from '@keymanapp/common-test-resources/test-runner-TC-reporter.mjs';

/** @type {import('@web/test-runner').TestRunnerConfig} */
export default {
  ...BASE_CONFIG,
  reporters: [
    teamcityReporter() /* custom-written, for CI-friendly reports */
  ]
}