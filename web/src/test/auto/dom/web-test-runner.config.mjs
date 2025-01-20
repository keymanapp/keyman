// @ts-check
import { devices, playwrightLauncher } from '@web/test-runner-playwright';
import { defaultReporter, summaryReporter } from '@web/test-runner';
import { LauncherWrapper, sessionStabilityReporter } from '@keymanapp/common-test-resources/test-runner-stability-reporter.mjs';
import named from '@keymanapp/common-test-resources/test-runner-rename-browser.mjs'
import { importMapsPlugin } from '@web/dev-server-import-maps';
import { dirname, resolve } from 'path';
import { fileURLToPath } from 'url';
import { esbuildPlugin } from '@web/dev-server-esbuild';

const dir = dirname(fileURLToPath(import.meta.url));
const KEYMAN_ROOT = resolve(dir, '../../../../../');

/** @type {import('@web/test-runner').TestRunnerConfig} */
export default {
  // debug: true,
  browsers: [
    new LauncherWrapper(playwrightLauncher({ product: 'chromium' })),
    new LauncherWrapper(playwrightLauncher({ product: 'firefox' })),
    new LauncherWrapper(playwrightLauncher({ product: 'webkit', concurrency: 1 })),
    // named(new LauncherWrapper(playwrightLauncher({
    //   product: 'webkit', concurrency: 1, createBrowserContext({ browser }) {
    //     return browser.newContext({ ...devices['iPhone X'] });
    //   }
    // })), 'iOS Phone (emulated)'),
    // named(new LauncherWrapper(playwrightLauncher({
    //   product: 'chromium', createBrowserContext({ browser }) {
    //     return browser.newContext({ ...devices['Pixel 4'] })
    //   }
    // })), 'Android Phone (emulated)'),
  ],
  concurrency: 10,
  nodeResolve: true,
  // Top-level, implicit 'default' group
  files: [
    'src/test/auto/dom/init_check.tests.ts',
    // '**/*.tests.html'
  ],
  groups: [
    {
      name: 'engine/attachment',
      // Relative, from the containing package.json
      files: [
        'build/test/dom/cases/attachment/**/*.tests.html',
        'build/test/dom/cases/attachment/**/*.tests.mjs'
      ]
    },
    {
      name: 'app/browser',
      // Relative, from the containing package.json
      files: ['build/test/dom/cases/browser/**/*.tests.mjs']
    },
    {
      name: 'engine/dom-utils',
      // Relative, from the containing package.json
      files: ['build/test/dom/cases/dom-utils/**/*.tests.mjs']
    },
    {
      name: 'engine/element-wrappers',
      // Relative, from the containing package.json
      files: ['build/test/dom/cases/element-wrappers/**/*.tests.mjs']
    },
    {
      name: 'engine/gesture-processor',
      // Relative, from the containing package.json
      // Note: here we use the .tests.html file in the src directory!
      files: ['src/test/auto/dom/cases/gesture-processor/**/*.tests.html']
    },
    {
      name: 'engine/keyboard',
      // Relative, from the containing package.json
      files: ['build/test/dom/cases/keyboard/**/*.tests.mjs']
    },
    {
      name: 'engine/keyboard-storage',
      // Relative, from the containing package.json
      files: ['build/test/dom/cases/keyboard-storage/**/*.tests.mjs']
    },
    {
      name: 'engine/osk',
      // Relative, from the containing package.json
      files: ['build/test/dom/cases/osk/**/*.tests.mjs']
    }
  ],
  middleware: [
    // Rewrites short-hand paths for test resources, making them fully relative to the repo root.
    function rewriteResourcePath(context, next) {
      if(context.url.startsWith('/resources/')) {
        context.url = '/web/src/test/auto' + context.url;
      }

      return next();
    }
  ],
  plugins: [
    esbuildPlugin({ts: true, target: 'auto'}),
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
    defaultReporter({})
  ],
  /*
    Un-comment the next two lines for easy interactive debugging; it'll launch the
    test page in your preferred browser.
  */
  // open: true,
  // manual: true,
  rootDir: KEYMAN_ROOT
}