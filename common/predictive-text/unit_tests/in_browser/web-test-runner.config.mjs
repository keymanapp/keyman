// @ts-check
import { devices, playwrightLauncher } from '@web/test-runner-playwright';
import { summaryReporter } from '@web/test-runner';
import { esbuildPlugin } from '@web/dev-server-esbuild';
import { fileURLToPath } from 'url';
import { dirname, resolve } from 'path';

const dir = dirname(fileURLToPath(import.meta.url));
const KEYMAN_ROOT = resolve(dir, '../../../../');

/** @type {import('@web/test-runner').TestRunnerConfig} */
export default {
  browsers: [
    // These are the same type - and probably the same _instances_ - as are visible within the reporter!
    // Probably a helpful fact to resolve name disambiguation.
    playwrightLauncher({ product: 'chromium' }),
    playwrightLauncher({ product: 'webkit' }),
    playwrightLauncher({ product: 'firefox' }),
    // playwrightLauncher({ product: 'webkit', createBrowserContext({browser}) {
    //   return browser.newContext({...devices['iPhone X'] }); // to resolve:  name disambiguation.
    // }})
  ],
  concurrency: 1,
  nodeResolve: true,
  files: [
    '**/*.spec.ts'
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
    esbuildPlugin({ ts: true, target: 'auto'})
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
  // debug: true,
  rootDir: KEYMAN_ROOT // b/c that's where node_modules is
}