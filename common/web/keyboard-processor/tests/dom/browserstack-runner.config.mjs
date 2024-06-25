import { browserstackLauncher } from '@web/test-runner-browserstack';
// import standardConfig from './web-test-runner.CI.config.mjs';
import standardConfig from './web-test-runner.config.mjs';
import { legacyPlugin } from '@web/dev-server-legacy';
import named from '@keymanapp/common-test-resources/test-runner-rename-browser.mjs';

// TODO:  always rebuild KEYMAN_VERSION before running this; we want it to show
// the absolute current version in the BrowserStack logs, even if run locally.
import { KEYMAN_VERSION } from '@keymanapp/keyman-version';
import { defaultReporter, summaryReporter } from '@web/test-runner';

// options shared between all browsers
/** @type {Record<string, unknown>} */
const sharedCapabilities = {
  // your username and key for browserstack, you can get this from your browserstack account
  // it's recommended to store these as environment variables
  'browserstack.user': process.env.BROWSER_STACK_USERNAME,
  'browserstack.key': process.env.BROWSER_STACK_ACCESS_KEY,
  'browserstack.networkLogs': true,
  'browserstack.console': 'verbose',

  project: 'Keyman',
  // for the child package.
  name: `@keymanapp/keyboard-processor`,
  // if you are running tests in a CI, the build id might be available as an
  // environment variable. this is useful for identifying test runs
  // this is for example the name for github actions
  build: `${KEYMAN_VERSION.VERSION_WITH_TAG || '-version tagging broken-'}`,
};

/** @type {import('@web/test-runner').TestRunnerConfig} */
export default {
  ...standardConfig,
  // how many browsers to run concurrently in browserstack. increasing this significantly
  // reduces testing time, but your subscription might limit concurrent connections
  concurrentBrowsers: 1,
  // amount of test files to execute concurrently in a browser. the default value is based
  // on amount of available CPUs locally which is irrelevant when testing remotely
  concurrency: 1,
  coverage: false,
  browsers: [
    // create a browser launcher per browser you want to test
    // you can get the browser capabilities from the browserstack website
    browserstackLauncher({
      capabilities: {
        // Requires the dev-server-legacy plugin to operate properly.
        ...sharedCapabilities,
        browserName: 'Chrome',
        os: 'Windows',
        os_version: '10',
        /* 57 is our absolute min-version target for 18.0. */
        /* 95 is the highest supported on Android 5.x devices, even when updated. */
        browser_version: '57'
      },
    }),

    named(browserstackLauncher({
      capabilities: {
        ...sharedCapabilities,
        browserName: 'Safari',
        // '13.4' does not appear to work, nor does '14'.
        // '15', however, does!
        os_version: '15',
        deviceName: 'iPhone SE 2022'
      },
    }), 'Safari for iOS'),

    named(browserstackLauncher({
      capabilities: {
        ...sharedCapabilities,
        browserName: 'Chrome',
        os_version: '6.0',
        deviceName: 'Samsung Galaxy S7'
      },
    }), 'Chrome for Android'),

    browserstackLauncher({
      capabilities: {
        ...sharedCapabilities,
        browserName: 'Firefox',
        // 'latest' would work, but not 90.
        // '100' does work, at least.
        browser_version: '100',
        os: 'Windows',
        os_version: '10',
      },
    }),
  ],
  plugins: [
    ...(standardConfig.plugins ?? []),
    legacyPlugin()
  ],

  // @web/test-runner uses these settings in their BrowserStack auto-testing.
  browserStartTimeout: 1000 * 60 * 2, // * 60 * 2
  testsStartTimeout: 1000 * 60 * 2,
  testsFinishTimeout: 1000 * 60 * 2,

  reporters: [...standardConfig.reporters, summaryReporter, defaultReporter]
  // debug: true
};