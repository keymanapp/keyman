/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { test, expect, type Page } from '@playwright/test';
import { clickFieldAndWaitForOSK, getAllKeyboardMenuText, getSelectedKeyboardMenuText, loadPage } from './e2eUtils';

declare const keyman: any;

async function setTimeoutAndLoadPage(page: Page, url: string, numKeyboards: number): Promise<void> {
  test.setTimeout(5000);

  await loadPage(page, url);

  await page.waitForFunction(
    (num: number) => typeof keyman !== 'undefined' && keyman.getKeyboards().length >= num,
    numKeyboards
  );

  // Now that we know that the expected number of keyboards were loaded, we can
  // force trigger updateKeyboardList() instead of waiting for the timeout which
  // might come in the middle of the tests
  await page.evaluate(() => {
    if (keyman.ui && keyman.ui.updateTimer) {
      clearTimeout(keyman.ui.updateTimer);
    }
    keyman.ui?.updateKeyboardList();
  });
}

test.describe('First example from the guide', function () {
  const beforeEach = async (page: Page) => {
    // output messages from the browser console to the test output, for debugging
    page.on('console', msg => console.log(msg.text()));

    await setTimeoutAndLoadPage(page, 'http://localhost:3000/build/docs/engine/guide/examples/__first-example.html', 2);
  }

  test('Input field shows US keyboard', async ({ page } : { page: Page }) => {
    // Setup
    await beforeEach(page);
    const oskTitleBar = await clickFieldAndWaitForOSK(page, page.getByPlaceholder('Hello World'));

    // Verify OSK shows US keyboard
    await expect(page.getByRole('img', { name: 'Use Web Keyboard' })).toBeVisible();
    await expect(page.getByRole('img', { name: 'Show On Screen Keyboard' })).toBeVisible();
    await expect.poll(() => page.evaluate(() => keyman.osk.isEnabled())).toBeTruthy();
    await expect.poll(() => page.evaluate(() => keyman.osk.isVisible())).toBeTruthy();
    await expect(oskTitleBar).toContainText('US');

    await expect(await getSelectedKeyboardMenuText(page)).toBe('English - US');
  });

  test('Keyman menu has expected keyboards', async ({ page } : { page: Page }) => {
    // Setup
    await beforeEach(page);
    await clickFieldAndWaitForOSK(page, page.getByPlaceholder('Hello World'));

    // Verify OSK menu has expected entries
    await expect(page.getByRole('img', { name: 'Use Web Keyboard' })).toBeVisible();
    await expect(page.getByRole('img', { name: 'Show On Screen Keyboard' })).toBeVisible();
    await expect(await getAllKeyboardMenuText(page)).toEqual(['(System keyboard)', 'English - US', 'Thai - Thai Kedmanee Basic'])
  });
});

test.describe('Auto-control example from the guide', function () {
  const beforeEach = async (page: Page) => {
    // output messages from the browser console to the test output, for debugging
    page.on('console', msg => console.log(msg.text()));

    await setTimeoutAndLoadPage(page, 'http://localhost:3000/build/docs/engine/guide/examples/__auto-control.html', 1);
  }

  test('Input field shows Lao keyboard', async ({ page } : { page: Page }) => {
    // Setup
    await beforeEach(page);
    await page.getByTestId('multilingual' ).click();

    // Verify OSK is shown
    await expect.poll(() => page.evaluate(() => keyman.osk.isEnabled())).toBeTruthy();
    await expect.poll(() => page.evaluate(() => keyman.osk.isVisible())).toBeTruthy();
    await expect(page.locator('#keymanweb_title_bar')).toContainText('Lao (Phonetic)');
  });

  test('Textarea shows Lao keyboard', async ({ page } : { page: Page }) => {
    // Setup
    await beforeEach(page);
    await page.getByTestId('textarea').click();

    // Verify OSK is shown
    await expect(await page.evaluate(() => keyman.osk.isEnabled())).toBeTruthy();
    await expect(await page.evaluate(() => keyman.osk.isVisible())).toBeTruthy();
    await expect(page.locator('#keymanweb_title_bar')).toContainText('Lao (Phonetic)');
  });
});

test.describe('Control-by-control example from the guide', function () {
  const beforeEach = async (page: Page) => {
    // output messages from the browser console to the test output, for debugging
    page.on('console', msg => console.log(msg.text()));

    await setTimeoutAndLoadPage(page, 'http://localhost:3000/build/docs/engine/guide/examples/__control-by-control.html', 6);
  }

  test('address field does not have KeymanWeb enabled', async ({ page } : { page: Page }) => {
    // Setup
    await beforeEach(page);
    await page.getByPlaceholder('id = address').click();

    // Verify OSK is not shown
    await expect(await page.evaluate(() => keyman.osk.isEnabled())).toBeTruthy();
    await expect(await page.evaluate(() => keyman.osk.isVisible())).toBeFalsy();
    await expect(page.getByRole('img', { name: 'Use Web Keyboard' })).not.toBeVisible();
    await expect(page.getByRole('img', { name: 'Show On Screen Keyboard' })).not.toBeVisible();
  });

  test('subject field does not show keyboard and defaults to system keyboard', async ({ page } : { page: Page }) => {
    // Setup
    await beforeEach(page);
    await page.getByPlaceholder('id = subject').click();

    // Verify the control is in system-keyboard mode: the OSK stays hidden,
    // while the toggle UI remains available for switching keyboards.
    await expect(await page.evaluate(() => keyman.osk.isEnabled())).toBeTruthy();
    await expect(await page.evaluate(() => keyman.osk.isVisible())).toBeFalsy();
    await expect(page.getByRole('img', { name: 'Use Web Keyboard' })).toBeVisible();
    await expect(page.getByRole('img', { name: 'Show On Screen Keyboard' }).isHidden()).toBeTruthy();

    await expect(await getSelectedKeyboardMenuText(page)).toBe('(System keyboard)');
  });

  test('message body field shows Lao keyboard', async ({ page } : { page: Page }) => {
    // Setup
    await beforeEach(page);
    await page.getByPlaceholder('id = text').click();

    // Verify OSK is shown
    await expect(await page.evaluate(() => keyman.osk.isEnabled())).toBeTruthy();
    await expect(await page.evaluate(() => keyman.osk.isVisible())).toBeTruthy();
    await expect(page.getByRole('img', { name: 'Use Web Keyboard' })).toBeVisible();
    await expect(page.getByRole('img', { name: 'Show On Screen Keyboard' })).toBeVisible();

    // Verify Lao (Phonetic) keyboard is active
    await expect(page.locator('#keymanweb_title_bar')).toContainText('Lao (Phonetic)');
    // Verify "Lao - Lao (Phonetic)" is selected (bold) in the menu
    await expect(await getSelectedKeyboardMenuText(page)).toBe('Lao - Lao (Phonetic)');
  });
});

test.describe('Full manual control example from the guide', function () {
  const beforeEach = async (page: Page) => {
    // output messages from the browser console to the test output, for debugging
    page.on('console', msg => console.log(msg.text()));

    await setTimeoutAndLoadPage(page, 'http://localhost:3000/build/docs/engine/guide/examples/__full-manual-control.html', 6);
  }

  test('Shows English and no OSK after loading page', async ({ page } : { page: Page }) => {
    // Setup
    await beforeEach(page);

    // Verify 'English' selected (which has the value '') and no OSK showing
    await expect(page.getByLabel('Keyboard')).toHaveValue('');
    await expect(await page.evaluate(() => keyman.osk.isEnabled())).toBeTruthy();
    await expect(await page.evaluate(() => keyman.osk.isVisible())).not.toBeTruthy();
  });

  test('Selecting English keyboard shows no OSK', async ({ page } : { page: Page }) => {
    // Setup
    await beforeEach(page);
    // first switch to Hebrew
    let keyboardchangePromise = page.evaluate(async () => {
      return new Promise((resolve) => {
        keyman.addEventListener('keyboardchange', function (kbd) {
          resolve(kbd);
        });
      });
    });
    await page.getByLabel('Keyboard').selectOption('Hebrew');
    await keyboardchangePromise;

    // then back to English
    keyboardchangePromise = page.evaluate(async () => {
      return new Promise((resolve) => {
        keyman.addEventListener('keyboardchange', function (kbd) {
          resolve(kbd);
        });
      });
    });
    await page.getByLabel('Keyboard').selectOption('English');
    await keyboardchangePromise;

    // Verify no OSK showing
    await expect(await page.evaluate(() => keyman.osk.isEnabled())).toBeTruthy();
    await expect(await page.evaluate(() => keyman.osk.isVisible())).not.toBeTruthy();
  });

  test('Selecting Devanagari keyboard shows Devanagari OSK', async ({ page } : { page: Page }) => {
    // Setup
    await beforeEach(page);
    const keyboardchangePromise = page.evaluate(async () => {
      return new Promise((resolve) => {
        keyman.addEventListener('keyboardchange', function (kbd) {
          resolve(kbd);
        });
      });
    });
    await page.getByLabel('Keyboard').selectOption('Devanagari (INSCRIPT)');
    await keyboardchangePromise;

    // Verify Devanagari OSK showing
    await expect(await page.evaluate(() => keyman.osk.isEnabled())).toBeTruthy();
    await expect(await page.evaluate(() => keyman.osk.isVisible())).toBeTruthy();
    await expect(page.locator('#keymanweb_title_bar')).toContainText('Devanagari (INSCRIPT)');
  });

  test('Selecting Hebrew shows Hebrew OSK', async ({ page } : { page: Page }) => {
    // Setup
    await beforeEach(page);
    const keyboardchangePromise = page.evaluate(async () => {
      return new Promise((resolve) => {
        keyman.addEventListener('keyboardchange', function (kbd) {
          resolve(kbd);
        });
      });
    });
    await page.getByLabel('Keyboard').selectOption('Hebrew');
    await keyboardchangePromise;

    // Verify Hebrew OSK showing
    await expect(await page.evaluate(() => keyman.osk.isEnabled())).toBeTruthy();
    await expect(await page.evaluate(() => keyman.osk.isVisible())).toBeTruthy();
    await expect(page.locator('#keymanweb_title_bar')).toContainText('Hebrew');
  });
});

test.describe('Manual control example from the guide', function () {
  const beforeEach = async (page: Page) => {
    // output messages from the browser console to the test output, for debugging
    page.on('console', msg => console.log(msg.text()));

    await setTimeoutAndLoadPage(page, 'http://localhost:3000/build/docs/engine/guide/examples/__manual-control.html', 1);
  }

  test('Does not show OSK after loading', async ({ page } : { page: Page }) => {
    // Setup
    await beforeEach(page);
    await page.getByTestId('multilingual').click();

    // Verify no OSK showing
    await expect(await page.evaluate(() => keyman.osk.isEnabled())).not.toBeTruthy();
    await expect(await page.evaluate(() => keyman.osk.isVisible())).not.toBeTruthy();
  });

  test('Shows Lao OSK after clicking button', async ({ page } : { page: Page }) => {
    // Setup
    await beforeEach(page);
    await page.getByAltText('KeymanWeb').click();
    await page.getByTestId('multilingual').click();

    // Verify Lao OSK showing
    await expect(await page.evaluate(() => keyman.osk.isEnabled())).toBeTruthy();
    await expect(await page.evaluate(() => keyman.osk.isVisible())).toBeTruthy();
    await expect(page.locator('#keymanweb_title_bar')).toContainText('Lao');
  });

  test('Hides Lao OSK after clicking button', async ({ page } : { page: Page }) => {
    // Setup
    await beforeEach(page);

    // click button
    await page.getByAltText('KeymanWeb').click();

    // Verify Lao OSK showing
    await page.getByTestId('multilingual').click();
    await expect(await page.evaluate(() => keyman.osk.isEnabled())).toBeTruthy();
    await expect(await page.evaluate(() => keyman.osk.isVisible())).toBeTruthy();
    await expect(page.locator('#keymanweb_title_bar')).toContainText('Lao');

    // Click button again to hide OSK
    await page.getByAltText('KeymanWeb').click();

    // Verify Lao OSK not showing
    await page.getByTestId('multilingual').click();
    await expect(await page.evaluate(() => keyman.osk.isEnabled())).not.toBeTruthy();
    await expect(await page.evaluate(() => keyman.osk.isVisible())).not.toBeTruthy();

  });
});

