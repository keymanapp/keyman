// @ts-check
import { devices, playwrightLauncher } from '@web/test-runner-playwright';
import { summaryReporter } from '@web/test-runner';
import named from '@keymanapp/common-test-resources/test-runner-rename-browser.mjs'
import { fileURLToPath } from 'url';
import { dirname, resolve } from 'path';

const dir = dirname(fileURLToPath(import.meta.url));
const KEYMAN_ROOT = resolve(dir, '../../../../../../');

/** @type {import('@web/test-runner').TestRunnerConfig} */
export default {
  // debug: true,
  browsers: [
    playwrightLauncher({ product: 'chromium' }),
    playwrightLauncher({ product: 'firefox' }),
    playwrightLauncher({ product: 'webkit' })
  ],
  concurrency: 10,
  nodeResolve: true,
  files: [
    '**/*.spec.mjs'
  ],
  reporters: [
    summaryReporter({}), /* local-dev mocha-style */
  ],
  /*
    Un-comment the next two lines for easy interactive debugging; it'll launch the
    test page in your preferred browser.
  */
  // open: true,
  // manual: true,
  rootDir: KEYMAN_ROOT // b/c that's where node_modules is,
}