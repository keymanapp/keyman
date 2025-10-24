// @ts-check
import { /*devices,*/ playwrightLauncher } from '@web/test-runner-playwright';
import { defaultReporter, summaryReporter } from '@web/test-runner';
import { esbuildPlugin } from '@web/dev-server-esbuild';
import { importMapsPlugin } from '@web/dev-server-import-maps';
import { dirname, resolve } from 'path';
import { fileURLToPath } from 'url';
import { LauncherWrapper, sessionStabilityReporter } from '@keymanapp/common-test-resources/test-runner-stability-reporter.mjs';
import { platform } from 'node:process';

const dir = dirname(fileURLToPath(import.meta.url));
const KEYMAN_ROOT = resolve(dir, '../../../../../');

const allBrowsers = platform === 'win32'
  ? [
    new LauncherWrapper(playwrightLauncher({ product: 'chromium' })),
    new LauncherWrapper(playwrightLauncher({ product: 'firefox' })),
    // no testing on webkit on Windows, see #14858
  ] : [
    new LauncherWrapper(playwrightLauncher({ product: 'chromium' })),
    new LauncherWrapper(playwrightLauncher({ product: 'firefox' })),
    // Setting it higher makes things faster... but Webkit experiences stability
    // issues for some of the tests if this is set higher than 1.  Notably,
    // engine.tests.mjs, events.tests.mjs, and text_selection.tests.mjs.  All the
    // text-simulation ones.
    new LauncherWrapper(playwrightLauncher({ product: 'webkit', concurrency: 1 })),
  ];

/** @type {import('@web/test-runner').TestRunnerConfig} */
export default {
  browsers: allBrowsers,
  concurrency: 10,
  nodeResolve: true,
  files: [
    'web/build/test/integrated/**/*.tests.mjs',
    // '**/*.tests.html'
  ],
  middleware: [
    // Rewrites short-hand paths for test resources, making them fully relative to the repo root.
    function rewriteResourcePath(context, next) {
      if(context.url.startsWith('/resources/')) {
        context.url = '/common/test' + context.url;
      }

      return next();
    },
    function rewriteWasmContentType(context, next) {
      if (context.url.endsWith('.wasm')) {
        context.headers['content-type'] = 'application/wasm';
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
  */
  // debug: true,
  // open: true,
  // manual: true,
  rootDir: KEYMAN_ROOT
}