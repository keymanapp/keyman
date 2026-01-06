/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import * as fs from 'node:fs';
import * as path from 'node:path';
import { expect, test, /*expect, Page, Locator */} from '@playwright/test';
import { KmxTestSource } from './kmxTestSource.js';
import { pressKeys } from './keyHandling';

function KeymanRoot(): string {
  // Find the Keyman root directory, i.e. the directory containing .git
  let dir = process.cwd();
  while (true) {
    if (fs.existsSync(dir + '/.git')) {
      return dir;
    }
    dir = path.dirname(dir);
    if (dir === '/' || dir.match(/^[A-Za-z]:[\/\\]?$/)) {
      throw new Error('Could not find Keyman root directory');
    }
  }
}

// Tests that are not supported in Web due to missing functionality
const testsToSkip = {
  'all': new Map([
    ['k_0107___punctkeys.kmn', 'Playwright does not support 102nd key']
  ]),
  '.kmx': new Map([
  ]),
  '.js': new Map([
    ['k_0812___nul_and_contextex.kmn', 'contextex is not supported in context() match in Web']
  ]),
};

// Tests that should work but fail due to not-yet implemented functionality
const testsToFix = {
  // TODO-web-core: fix these tests
  '.kmx': [
    'k_0000___null_keyboard.kmn',
    'k_0103___vkey_input__shift_ctrl_.kmn', // kmx only
    'k_0104___vkey_input__ctrl_alt_.kmn',
    'k_0105___vkey_input__ctrl_alt_2_.kmn', // kmx only
    'k_0106___smp.kmn', // kmx only
    'k_0200___ralt.kmn', // kmx only
    'k_0201___ralt_2.kmn', // kmx only
    'k_0203___generic_ctrlalt.kmn',
    'k_0400___groups_and_virtual_keys.kmn', // kmx only
    'k_0501___options_with_preset.kmn',
    'k_0503___options_with_save_and_preset.kmn',
    'k_0504___options_with_reset.kmn',
    'k_0505___options_double_set_reset.kmn',
    'k_0506___options_double_set_staged.kmn',
    'k_0507___options___double_reset_staged.kmn',
    'k_0508___options___double_reset.kmn',
    'k_0600___system_stores.kmn',
    'k_0601___system_stores_2.kmn',
    'k_0700___caps_lock.kmn',
    'k_0701___caps_control.kmn',
    'k_0807___enter_invalidates_context.kmn',
    'k_0813___deadkey_cancelled_by_arrow.kmn',
  ],
  // TODO: fix these tests (#15342)
  '.js': [
    'k_0000___null_keyboard.kmn',
    'k_0104___vkey_input__ctrl_alt_.kmn',
    'k_0203___generic_ctrlalt.kmn',
    'k_0501___options_with_preset.kmn',
    'k_0503___options_with_save_and_preset.kmn',
    'k_0504___options_with_reset.kmn',
    'k_0505___options_double_set_reset.kmn',
    'k_0506___options_double_set_staged.kmn',
    'k_0507___options___double_reset_staged.kmn',
    'k_0508___options___double_reset.kmn',
    'k_0600___system_stores.kmn',
    'k_0601___system_stores_2.kmn',
    'k_0700___caps_lock.kmn',
    'k_0701___caps_control.kmn',
    'k_0801___long_context_and_deadkeys.kmn', // js only
    'k_0802___long_context_and_split_deadkeys.kmn', // js only
    'k_0807___enter_invalidates_context.kmn',
    'k_0808___nul_and_context.kmn', // js only
    'k_0810___nul_and_index.kmn', // js only
    'k_0813___deadkey_cancelled_by_arrow.kmn',
  ]
};

test.describe('Baseline tests', () => {
  const testDir = '/common/test/keyboards/baseline';

  // Find all k_*.kmn files in testDir
  const files = fs.readdirSync(KeymanRoot() + testDir).filter(f => f.match(/^k_.*\.kmn$/));
  for (const ext of ['.kmx', '.js']) {
    test.describe(`${ext} tests`, () => {
      for (const file of files) {
        test(file, async ({ page }) => {
          // Setup
          test.info().annotations.push({ type: 'name', description: test.info().title });

          const kbdFile = file.replace(/\.kmn$/, ext);
          if (!fs.existsSync(path.join(KeymanRoot(), testDir, kbdFile))) {
            const msg = `Skipping ${file} test - ${ext} file doesn't exist`;
            console.log(msg);
            test.skip(true, msg);
            return;
          }

          const testSource = new KmxTestSource();
          const result = testSource.loadSource(path.join(KeymanRoot(), testDir, file));
          if (result !== 0) {
            throw new Error(`Error loading ${ext} test source ${file} at line ${result}`);
          }

          test.info().annotations.push({ type: 'description', description: testSource.description });

          if (testsToSkip[ext].has(file)) {
            const msg = `Skipping ${file} test - ${testsToSkip[ext].get(file)}`;
            console.log(msg);
            test.skip(true, msg);
            return;
          }
          if (testsToSkip['all'].has(file)) {
            const msg = `Skipping ${file} test - ${testsToSkip['all'].get(file)}`;
            console.log(msg);
            test.skip(true, msg);
            return;
          }
          if (testsToFix[ext].includes(file)) {
            const msg = `Skipping ${file} test - requires not-yet implemented functionality`;
            console.log(msg);
            test.fixme(true, msg);
            return;
          }

          await page.goto(`http://localhost:3000/src/test/auto/e2e/baseline/baseline.tests.html?keyboard=${testDir}/${kbdFile}`);
          await page.evaluate(async () => { await window.KmwLoaded; });
          const textarea = page.locator('#inputarea');
          await textarea.click();

          // TODO-web-core: set options from test source
          // TODO-web-core: set capslock state

          // set context
          if (testSource.context) {
            await textarea.fill(testSource.context);
          }

          // type keys
          await pressKeys(page, testSource.keys);

          // verify output
          await expect(textarea).toHaveValue(testSource.expected, { timeout: 500 });
        });

      }
    });
  }
});