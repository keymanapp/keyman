import { browserstackLauncher } from '@web/test-runner-browserstack';
import { legacyPlugin } from '@web/dev-server-legacy';
import named from '@keymanapp/common-test-resources/test-runner-rename-browser.mjs';
import { importMapsPlugin } from '@web/dev-server-import-maps';

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
  // if you are running tests in a CI, the build id might be available as an
  // environment variable. this is useful for identifying test runs
  // this is for example the name for github actions
  build: `${KEYMAN_VERSION.VERSION_WITH_TAG || '-version tagging broken-'}`,
};

/**
 * @param {import('@web/test-runner').TestRunnerConfig} config
 * @returns {import('@web/test-runner').TestRunnerConfig}
 * */
export function buildLegacyTestingConfig(config, projectName) {
  return {
    ...config,
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
          // The display name visible on browserstack.com's dashboard
          name: projectName || `${KEYMAN_VERSION.VERSION_WITH_TAG}`,
          browserName: 'Chrome',
          os: 'Windows',
          os_version: '10',
          /* 57 is our absolute min-version target for 18.0. */
          /* 95 is the highest supported on Android 5.x devices, even when updated. */
          browser_version: '57'
        },
      }),

      // macOS... seems to just... time out some of the time.
      named(browserstackLauncher({
        capabilities: {
          ...sharedCapabilities,
          name: projectName || `${KEYMAN_VERSION.VERSION_WITH_TAG}`,
          browserName: 'Safari',
          os: "OS X",
          // // ... none of these is actually connecting for remote testing via BrowserStack.
          // // No such issue when running the tests locally, though.
          os_version: "Catalina",
          browser_version: '13.1'
          // os_version: "Mojave",
          // browser_version: "12.1"
          // os_version: "Monterey",
          // browser_version: "15.6"
          // os_version: "Big Sur",
          // browser_version: '14.1'
        },
      }), 'Safari'),

      // // iOS devices seem to flake regularly when I attempt to use 'em.
      // named(browserstackLauncher({
      //   capabilities: {
      //     ...sharedCapabilities,
      //     name: projectName || `${KEYMAN_VERSION.VERSION_WITH_TAG}`,
      //     browserName: 'Safari',
      //     // '13.4' does not appear to work, nor does '14'.
      //     // '15', however, does!
      //     os_version: '15',
      //     deviceName: 'iPhone SE 2022'
      //     // os_version: '13.4',
      //     // deviceName: 'iPhone SE 2020' // 2022 for Safari 15.
      //   },
      // }), 'Safari for iOS'),

      // was S7, 6.0 - but the API gave us a 'deprecated' report and errored there.
      // S8, 7.0 ... seems that the test-process outright crashes for no clearly stated reason?
      named(browserstackLauncher({
        capabilities: {
          ...sharedCapabilities,
            name: projectName || `${KEYMAN_VERSION.VERSION_WITH_TAG}`,
          browserName: 'Chrome',
          os_version: '7.0',
          deviceName: 'Samsung Galaxy S8'
        },
      }), 'Chrome for Android'),

      // // And sometimes the Firefox test targets flake.  Why!?
      // browserstackLauncher({
      //   capabilities: {
      //     ...sharedCapabilities,
      //     name: projectName || `${KEYMAN_VERSION.VERSION_WITH_TAG}`,
      //     browserName: 'Firefox',
      //     // 'latest' would work, but not 90.
      //     // '100' does work, at least.
      //     browser_version: '100',
      //     os: 'Windows',
      //     os_version: '10',
      //   },
      // }),
    ],
    plugins: [
      ...(config.plugins ?? []),
      legacyPlugin(),
      // Redirects our timeout-defining import to one with more generous
      // settings in order to compensate for delays due to serving files
      // remotely.
      importMapsPlugin({
        inject: {
          importMap: {
            imports: {
              // Note: this cannot resolve a node-style import on the right-hand side.
              '@keymanapp/common-test-resources/test-timeouts.mjs': '/common/test/resources/test-timeouts-remote.mjs'
            }
          }
        }
      })
    ],

    // @web/test-runner uses these settings in their BrowserStack auto-testing.
    browserStartTimeout: 1000 * 60 * 2, // * 60 * 2
    testsStartTimeout: 1000 * 60 * 2,
    testsFinishTimeout: 1000 * 60 * 2,

    reporters: [...config.reporters, summaryReporter, defaultReporter],

    testFramework: {
      ...(config.testFramework ?? {}),
      config: {
        ...(config.testFramework?.config ?? {}),
        /* remote tunneling for BrowserStack tends to add serious delays to script loads */
        // 2000ms = the Mocha default.
        timeout: (config.testFramework?.config?.timeout ?? 2000) * 10
      }
    },

    debug: true
  };
};