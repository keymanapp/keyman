// @ts-check
import { devices, playwrightLauncher } from '@web/test-runner-playwright';
import { defaultReporter, summaryReporter } from '@web/test-runner';
import { esbuildPlugin } from '@web/dev-server-esbuild';
import named from '@keymanapp/common-test-resources/test-runner-rename-browser.mjs'
import { fileURLToPath } from 'url';
import { dirname, resolve } from 'path';
import { LauncherWrapper, sessionStabilityReporter } from '@keymanapp/common-test-resources/test-runner-stability-reporter.mjs';

const dir = dirname(fileURLToPath(import.meta.url));
const KEYMAN_ROOT = resolve(dir, '../../../../../../');

/** @type {import('@web/test-runner').TestRunnerConfig} */
export default {
  // debug: true,
  browsers: [
    new LauncherWrapper(playwrightLauncher({ product: 'chromium' })),
    new LauncherWrapper(playwrightLauncher({ product: 'firefox' })),
    new LauncherWrapper(playwrightLauncher({ product: 'webkit', concurrency: 1 })),
    named(new LauncherWrapper(playwrightLauncher({ product: 'webkit', concurrency: 1, createBrowserContext({browser}) {
      return browser.newContext({...devices['iPhone X'] });
    }})), 'iOS Phone (emulated)'),
    named(new LauncherWrapper(playwrightLauncher({ product: 'chromium' , createBrowserContext({browser}) {
      return browser.newContext({...devices['Pixel 4'] })
    }})), 'Android Phone (emulated)'),
  ],
  concurrency: 10,
  nodeResolve: true,
  files: [
    '**/*.tests.ts'
  ],
  middleware: [
    // Rewrites short-hand paths for test resources, making them fully relative to the repo root.
    function rewriteResourcePath(context, next) {
      if(context.url.startsWith('/resources/')) {
        context.url = '/common/test' + context.url;
      }

      return next();
    }
  ],
  plugins: [
    esbuildPlugin({ts: true, target: 'auto'})
  ],
  reporters: [
    summaryReporter({}), /* local-dev mocha-style */
    sessionStabilityReporter({}),
    defaultReporter({})
  ],
  /*
    Un-comment the next two lines for easy interactive debugging; it'll launch the
    test page in your preferred browser.
  */
  // open: true,
  // manual: true,
  rootDir: KEYMAN_ROOT // b/c that's where node_modules is,
}