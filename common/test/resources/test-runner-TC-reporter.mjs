import path from 'path';

// Yay for references!
// https://www.jetbrains.com/help/teamcity/service-messages.html#Supported+Test+ServiceMessages
// https://modern-web.dev/docs/test-runner/reporters/write-your-own/
export default function teamcityReporter({ name="Web Test Runner JavaScript testing", reportResults = true, reportProgress = false } = {}) {
  /** @type {Map<import('@web/test-runner').BrowserLauncher, number>} */
  let browserFlowIdMap = new Map();
  /** @type {import('@web/test-runner').Logger} */
  let logger;

  /** @type {string} */
  let rootDir;

  /**
   * @param {import('@web/test-runner').Logger} logger
   * @param {import('@web/test-runner').TestSuiteResult} suiteResult
   * @param {number=} flowId Not actually used... but the pattern may prove very useful for custom naming in the reports.
   */
  const generateSuiteReport = (logger, suiteResult, flowId) => {
    // TODO:  have seen in some outputs:  [, ] escaped by |.
    // Need to check re: ' and | as well, since formatting will matter.
    // Also need to check \n.

    if(suiteResult.name) {
      logger.log(`##teamcity[testSuiteStarted name='${suiteResult.name}']`);
    }
    for(const test of suiteResult?.tests ?? []) {
      if(test.skipped) {
        logger.log(`##teamcity[testIgnored name='${test.name}']`);
      } else {
        logger.log(`##teamcity[testStarted name='${test.name}' captureStandardOutput='true']`);
        if(!test.passed) {
          const message = test.error ? `message='${test.error.message}'` : '';
          const details = test.error ? `\ndetails='${test.error.stack}\n`: '';

          if(test.error?.actual !== undefined && test.error?.expected !== undefined) {
            logger.log(`##teamcity[testFailed type='comparisonFailure' name='${test.name}' ${message}] ${details} expected='${test.error?.expected}' actual='${test.error?.actual}']`);
          }
          logger.log(`##teamcity[testFailed name='${test.name}' ${message}] ${details}']`);
        }
        logger.log(`##teamcity[testFinished name='${test.name}' duration='${test.duration}']`);
      }
    }
    for(const suite of suiteResult?.suites ?? []) {
      generateSuiteReport(logger, suite, flowId);
    }
    if(suiteResult.name) {
      logger.log(`##teamcity[testSuiteFinished name='${suiteResult.name}']`);
    }
  }

  /** @type {import('@web/test-runner').Reporter} */
  const reporter = {
    start({browsers, config}) {
      rootDir = config.rootDir;
      const existingIds = Array.from(browserFlowIdMap.values());

      for(const browser of browsers) {
        /** @type {number} */
        let flowId;
        do {
        flowId = Math.floor(Math.random() * 1e9);
        } while(existingIds.find((val) => val == flowId));

        browserFlowIdMap.set(browser, flowId);
      }

      logger = config.logger;
      logger.log(`##teamcity[blockOpened name='${name}']`);
    },
    stop({testCoverage}) {
      logger.log(`##teamcity[blockClosed name='${name}']`);

      // To consider:  could link in coverage reports via
      // https://www.jetbrains.com/help/teamcity/service-messages.html#Reporting+Build+Statistics
      // (is what my sources narrow down to)
      // Can hard-link in the statistics numbers.
      // There are other ways to at least "attach" the coverage map if desired, I think.
      // https://www.jetbrains.com/help/teamcity/including-third-party-reports-in-the-build-results.html
      // (ooh, custom tab!?  ... via artifact HTML)
      if(testCoverage) {
        console.log(testCoverage?.coverageMap.toJSON());
      }
    },
    reportTestFileResults({logger, sessionsForTestFile, testFile}) {
      if(!reportResults) {
        return;
      }

      // Add extra "suite" layers:  testFile (filename), then group-browser.

      // What is TC's "flowId" bit for?  [Answer: resolving asynchronous text output -
      // which mattered for BrowserStack.  @web/test-runner handles that for us.]

      const repoPath = path.relative(rootDir, testFile);

      logger.log(`##teamcity[testSuiteStarted name='${repoPath}']`);
      for(let session of sessionsForTestFile) {
        const sessionName = `${session.group.name} - ${session.browser.name}`;
        logger.log(`##teamcity[testSuiteStarted name='${sessionName}']`);

        const results = session.testResults;
        if(results) {
          generateSuiteReport(logger, results, browserFlowIdMap.get(session.browser));
        }

        logger.log(`##teamcity[testSuiteFinished name='${sessionName}']`);
      }
      logger.log(`##teamcity[testSuiteFinished name='file ${repoPath}']`);
    }
  };

  return reporter;
}