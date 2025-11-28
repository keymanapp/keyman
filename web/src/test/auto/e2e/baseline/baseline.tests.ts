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
const testsToSkip = new Map([
  ['k_038___punctkeys.kmn', 'Playwright does not support 102nd key']
]);

// Tests that should work but fail due to not-yet implemented functionality
// TODO-web-core: fix these tests
const testsToFix = [
  'k_000___null_keyboard.kmn',
  'k_006___vkey_input__shift_ctrl_.kmn',
  'k_007___vkey_input__ctrl_alt_.kmn',
  'k_008___vkey_input__ctrl_alt_2_.kmn',
  'k_012___ralt.kmn',
  'k_014___groups_and_virtual_keys.kmn',
  'k_015___ralt_2.kmn',
  'k_022___options_with_preset.kmn',
  'k_024___options_with_save_and_preset.kmn',
  'k_025___options_with_reset.kmn',
  'k_026___system_stores.kmn',
  'k_027___system_stores_2.kmn',
  'k_028___smp.kmn',
  'k_031___caps_lock.kmn',
  'k_032___caps_control.kmn',
  'k_034___options_double_set_reset.kmn',
  'k_035___options_double_set_staged.kmn',
  'k_036___options___double_reset_staged.kmn',
  'k_037___options___double_reset.kmn',
  'k_039___generic_ctrlalt.kmn',
  'k_049___enter_invalidates_context.kmn',
  'k_055___deadkey_cancelled_by_arrow.kmn',
];

test.describe('Baseline tests', () => {
  const testDir = '/common/test/keyboards/baseline';

  // Find all k_*.kmn files in testDir
  const files = fs.readdirSync(KeymanRoot() + testDir).filter(f => f.match(/^k_.*\.kmn$/));
  for (const file of files) {
    test(file, async ({ page }) => {
      const kmxFile = file.replace(/\.kmn$/, '.kmx');
      const testSource = new KmxTestSource();
      const result = testSource.loadSource(path.join(KeymanRoot(), testDir, file));
      if (result !== 0) {
        throw new Error(`Error loading KMX test source ${file} at line ${result}`);
      }

      // Setup
      test.info().annotations.push({ type: 'name', description: test.info().title });
      test.info().annotations.push({ type: 'description', description: testSource.description });

      if (testsToSkip.has(file)) {
        const msg = `Skipping ${file} test - ${testsToSkip.get(file)}`;
        console.log(msg);
        test.skip(true, msg);
        return;
      }

      if (testsToFix.includes(file)) {
        const msg = `Skipping ${file} test - requires not-yet implemented functionality`;
        console.log(msg);
        test.fixme(true, msg);
        return;
      }

      await page.goto(`http://localhost:3000/src/test/auto/e2e/baseline/baseline.tests.html?keyboard=${testDir}/${kmxFile}`);
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