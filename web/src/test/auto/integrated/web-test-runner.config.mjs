// @ts-check
import { devices, playwrightLauncher } from '@web/test-runner-playwright';
import { defaultReporter, summaryReporter } from '@web/test-runner';
import { esbuildPlugin } from '@web/dev-server-esbuild';
import { importMapsPlugin } from '@web/dev-server-import-maps';
import { dirname, resolve } from 'path';
import { fileURLToPath } from 'url';
import { LauncherWrapper, sessionStabilityReporter } from '@keymanapp/common-test-resources/test-runner-stability-reporter.mjs';

const dir = dirname(fileURLToPath(import.meta.url));
const KEYMAN_ROOT = resolve(dir, '../../../../../');

/** @type {import('@web/test-runner').TestRunnerConfig} */
export default {
  browsers: [
    new LauncherWrapper(playwrightLauncher({ product: 'chromium' })),
    new LauncherWrapper(playwrightLauncher({ product: 'firefox' })),
    // Setting it higher makes things faster... but Webkit experiences stability
    // issues for some of the tests if this is set higher than 1.  Notably,
    // engine.spec.mjs, events.spec.mjs, and text_selection.spec.mjs.  All the
    // text-simulation ones.
    new LauncherWrapper(playwrightLauncher({ product: 'webkit', concurrency: 1}))
  ],
  concurrency: 10,
  nodeResolve: true,
  files: [
    'src/test/auto/integrated/**/*.spec.ts',
    // '**/*.spec.html'
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
    esbuildPlugin({ts: true}),
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
    sessionStabilityReporter({}),
    defaultReporter()
  ],
  /*
    Un-comment the next two lines for easy interactive debugging; it'll launch the
    test page in your preferred browser.

    WARNING: https://github.com/modernweb-dev/web/issues/2721 may cause issues when
    using manual mode.  Changing rootDir to the drive root (or similar) may provide
    a decent workaround; it appears that Web Test Runner can do a little searching
    for node_modules if and when necessary.
  */
  // debug: true,
  // open: true,
  // manual: true,
  rootDir: KEYMAN_ROOT
}