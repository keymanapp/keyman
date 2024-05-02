// @ts-check
import { devices, playwrightLauncher } from '@web/test-runner-playwright';
import { summaryReporter } from '@web/test-runner';
import teamcityReporter from '@keymanapp/common-test-resources/test-runner-TC-reporter.mjs';
import { importMapsPlugin } from '@web/dev-server-import-maps';
import { dirname, resolve } from 'path';
import { fileURLToPath } from 'url';

const dir = dirname(fileURLToPath(import.meta.url));
const KEYMAN_ROOT = resolve(dir, '../../../../../../../');

/** @type {import('@web/test-runner').TestRunnerConfig} */
export default {
  // debug: true,
  browsers: [
    // These are the same type - and probably the same _instances_ - as are visible within the reporter!
    // Probably a helpful fact to resolve name disambiguation.
    playwrightLauncher({ product: 'chromium' }),
    playwrightLauncher({ product: 'firefox' }),
    playwrightLauncher({ product: 'webkit' })
  ],
  concurrency: 10,
  nodeResolve: true,
  files: [
    '**/*.spec.html'
  ],
  middleware: [
    // Rewrites short-hand paths for test resources, making them fully relative to the repo root.
    function rewriteResourcePath(context, next) {
      if(context.url.startsWith('/resources/')) {
        context.url = '/common/web/gesture-recognizer/src/test' + context.url;
      }

      return next();
    }
  ],
  plugins: [
    importMapsPlugin({
      inject: {
        importMap: {
          // Redirects `eventemitter3` imports to the bundled ESM library.  The standard import is an
          // ESM wrapper around the CommonJS implementation, and WTR fails when it hits the CommonJS.
          imports: {
            'eventemitter3': '/node_modules/eventemitter3/dist/eventemitter3.esm.js'
          }
        }
      }
    })
  ],
  reporters: [
    summaryReporter({}), /* local-dev mocha-style */
    teamcityReporter() /* custom-written, for CI-friendly reports */
  ],
  /*
    Un-comment the next two lines for easy interactive debugging; it'll launch the
    test page in your preferred browser.

    WARNING: https://github.com/modernweb-dev/web/issues/2721 may cause issues when
    using manual mode.  Changing rootDir to the drive root (or similar) may provide
    a decent workaround; it appears that Web Test Runner can do a little searching
    for node_modules if and when necessary.
  */
  // open: true,
  // manual: true,
  rootDir: KEYMAN_ROOT
}