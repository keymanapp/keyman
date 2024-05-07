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

  function tcReportEscaping(data) {
    /** @type{string} */
    let text;

    if(typeof data == 'string') {
      text = data;
    } else if(data) {
      text = data.toString();
    } else {
      text = '';
    }

    return text
      .replace(/\|/g, '||')
      .replace(/\[/g, '|[')
      .replace(/]/g, '|]')
      .replace(/\'/g, '|\'');
  }

  const e = tcReportEscaping;

  /** @type {Map<string, Map<string, {passed: number, failed: number, skipped: number}>>} */
  const testDefMap = new Map();

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
      logger.log(`##teamcity[testSuiteStarted name='${e(suiteResult.name)}']`);
    }

    const summary = {
      passed: 0,
      failed: 0,
      skipped: 0
    }

    for(const test of suiteResult?.tests ?? []) {
      if(test.skipped) {
        logger.log(`##teamcity[testIgnored name='${e(test.name)}']`);
        summary.skipped++;
      } else {
        logger.log(`##teamcity[testStarted name='${e(test.name)}' captureStandardOutput='true']`);
        if(test.passed) {
          summary.passed++;
        } else {
          summary.failed++;
          const message = test.error ? `message='${e(test.error.message)}'` : '';
          const details = test.error ? `\ndetails='${e(test.error.stack)}\n`: '';

          if(test.error?.actual !== undefined && test.error?.expected !== undefined) {
            logger.log(`##teamcity[testFailed type='comparisonFailure' name='${e(test.name)}' ${e(message)}] ${e(details)} expected='${e(test.error?.expected)}' actual='${e(test.error?.actual)}']`);
          }
          logger.log(`##teamcity[testFailed name='${e(test.name)}' ${e(message)}] ${e(details)}']`);
        }

        logger.log(`##teamcity[testFinished name='${e(test.name)}' duration='${e(test.duration)}']`);
      }
    }
    for(const suite of suiteResult?.suites ?? []) {
      const subSummary = generateSuiteReport(logger, suite, flowId);
      summary.passed += subSummary.passed;
      summary.failed += subSummary.failed;
      summary.skipped += subSummary.skipped;
    }
    if(suiteResult.name) {
      logger.log(`##teamcity[testSuiteFinished name='${e(suiteResult.name)}']`);
    }

    return summary;
  }

  /** @type {import('@web/test-runner').Reporter} */
  const reporter = {
    start({browsers, config, sessions}) {
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

      for(const session of sessions.all()) {
        testDefMap.set(buildSessionName(session), new Map());
      }

      logger = config.logger;
      logger.log(`##teamcity[blockOpened name='${e(name)}']`);
    },
    stop({testCoverage}) {
      logger.log(`##teamcity[blockClosed name='${e(name)}']`);

      // If there are failures, what can we report about the cause of failures?
      // Step 1: collate per test-group & platform, as it makes a nicer summary.
      /** @type {Map<string, import('@web/test-runner').BasicTestSession[]} */
      let sessionFailureMap = new Map();

      for(const session of args.sessions) {
        if(!session.passed) {
          const sessionName = buildSessionName(session);
          const failureList = sessionFailureMap.get(sessionName) || [];
          if(!failureList.length) {
            sessionFailureMap.set(sessionName, failureList)
          }

          failureList.push(session);
        }
      }

      for(const sessionName of sessionFailureMap.keys()) {
        const sessionTestData = testDefMap.get(sessionName);

        logger.log('');
        logger.log(`Failure data for '${sessionName}'`);
        for(const sessionFile of sessionFailureMap.get(sessionName)) {
          const testFile = sessionFile.testFile;
          const fileTestData = sessionTestData.get(testFile);

          logger.log(`  Failed test-file: ${testFile}`);
          if(fileTestData) {
            logger.log(`    Test result summary: ${JSON.stringify(fileTestData)}`);
          } else {
            logger.log(`    Tests failed to load and run`);
          }

          // None of these showed anything useful for a mystery WebKit failure.
          const request404s = sessionFile.request404s;
          if(request404s?.length) {
            logger.log(`    404s observed: ${JSON.stringify(request404s)}`);
          }
          if(sessionFile.logs.length > 0) {
            logger.log(`    Logs:`);
            for(const log of sessionFile.logs) {
              // Note:  Is not connected to `logger.log`.  So... what data DO we get?
              logger.log(`    ${JSON.stringify(log)}`);
            }
          }
        }
      }

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

      logger.log(`##teamcity[testSuiteStarted name='${e(repoPath)}']`);
      for(let session of sessionsForTestFile) {
        const sessionName = `${session.group.name} - ${session.browser.name}`;
        logger.log(`##teamcity[testSuiteStarted name='${e(sessionName)}']`);

        const results = session.testResults;
        if(results) {
          const summary = generateSuiteReport(logger, results, browserFlowIdMap.get(session.browser));
          testDefMap.get(sessionName).set(testFile, summary);
        }

        logger.log(`##teamcity[testSuiteFinished name='${e(sessionName)}']`);
      }
      logger.log(`##teamcity[testSuiteFinished name='file ${e(repoPath)}']`);
    }
  };

  return reporter;
}