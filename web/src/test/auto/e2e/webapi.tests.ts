/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { test, expect, type Page } from '@playwright/test';
import { setTimeoutAndLoadPage, waitForKeyboardSelection } from './e2eUtils';

declare const keyman: any;

// mapping from keyboard names to internal keyboard ids and osk title
// for the keyboards the page will load
const LoadedKeyboards: { [key: string]: { id: string, title: string } } = {
  'English (EuroLatin2)': { id: 'Keyboard_european2', title: 'EuroLatin2 Keyboard' },
  'Lao (Phonetic)': { id: 'Keyboard_laokeys', title: 'Lao (Phonetic)' },
  'Hebrew': { id: 'Keyboard_hebrew', title: 'Hebrew' },
  'Devanagari (INSCRIPT)': { id: 'Keyboard_devanagari_inscript', title: 'Devanagari (INSCRIPT)' },
  'Korean Korda': { id: 'Keyboard_korean_korda', title: 'Korean (KORDA) - 30 Day Evaluation' },
};

async function verifyActiveKeyboard(page: Page, expectedKeyboard: string): Promise<void> {
  await expect.poll(() => page.evaluate(() => keyman.osk.isEnabled())).toBeTruthy();
  await expect.poll(() => page.evaluate(() => keyman.osk.isVisible())).toBeTruthy();
  await expect(page.locator('#keymanweb_title_bar')).toContainText(LoadedKeyboards[expectedKeyboard].title);
  await expect.poll(() => page.evaluate(() => keyman.getActiveKeyboard())).toEqual(LoadedKeyboards[expectedKeyboard].id);
}

async function switchKeyboard(page: Page, keyboard: string): Promise<void> {
  await page.waitForFunction(() => document.getElementById('kmwico'));
  await page.locator('#kmwico').hover();
  await page.waitForFunction(() => document.querySelector('#KeymanWeb_KbdList.sfhover'));
  await waitForKeyboardSelection(page);
  await page.getByText(keyboard).click();
}

test.describe('Global and independent mode', function () {
  const beforeEach = async (page: Page) => {
    // output messages from the browser console to the test output, for debugging
    page.on('console', msg => console.log(msg.text()));

    await setTimeoutAndLoadPage(page, 'http://localhost:3000/src/test/auto/e2e/webapi.tests.html', 5);
  }

  test('activate default keyboards (click)', async ({ page }: { page: Page }) => {
    // Setup
    await beforeEach(page);

    // Verify
    await page.getByLabel('input1').click();
    await verifyActiveKeyboard(page, 'Lao (Phonetic)');
    await page.getByLabel('input2').click();
    await verifyActiveKeyboard(page, 'Lao (Phonetic)');
    await page.getByLabel('input3').click();
    await verifyActiveKeyboard(page, 'Hebrew');
    await page.getByLabel('input4').click();
    await verifyActiveKeyboard(page, 'Devanagari (INSCRIPT)');
    await page.getByLabel('input1').click();
    await verifyActiveKeyboard(page, 'Lao (Phonetic)');
  });

  test('activate default keyboards (tab)', async ({ page }: { page: Page }) => {
    // Setup
    await beforeEach(page);
    await page.getByLabel('input1').focus();

    // Verify
    await page.locator('*:focus').press('Tab'); // --> input2
    await verifyActiveKeyboard(page, 'Lao (Phonetic)');
    await page.locator('*:focus').press('Tab'); // --> input3
    await verifyActiveKeyboard(page, 'Hebrew');
    await page.locator('*:focus').press('Tab'); // --> input4
    await verifyActiveKeyboard(page, 'Devanagari (INSCRIPT)');
  });

  test('switch global keyboard', async ({ page }: { page: Page }) => {
    // Setup
    await beforeEach(page);
    await page.getByLabel('input1').click();

    // Execute
    await switchKeyboard(page, 'Korean - Korean Korda');

    // Verify
    await verifyActiveKeyboard(page, 'Korean Korda');
    await page.getByLabel('input2').click();
    await verifyActiveKeyboard(page, 'Korean Korda');
    await page.getByLabel('input3').click();
    await verifyActiveKeyboard(page, 'Hebrew');
    await page.getByLabel('input4').click();
    await verifyActiveKeyboard(page, 'Devanagari (INSCRIPT)');
  });

  test('switch keyboard on independent mode control', async ({ page }: { page: Page }) => {
    // Setup
    await beforeEach(page);
    await page.getByLabel('input3').click();

    // Execute
    await switchKeyboard(page, 'Korean - Korean Korda');

    // Verify
    await verifyActiveKeyboard(page, 'Korean Korda');
    await page.getByLabel('input4').click();
    await verifyActiveKeyboard(page, 'Devanagari (INSCRIPT)');
    await page.getByLabel('input1').click();
    await verifyActiveKeyboard(page, 'Lao (Phonetic)');
    await page.getByLabel('input2').click();
    await verifyActiveKeyboard(page, 'Lao (Phonetic)');
  });
});

test.describe('Typing with hardware keyboard', function () {
  const beforeEach = async (page: Page) => {
    // output messages from the browser console to the test output, for debugging
    page.on('console', msg => console.log(msg.text()));

    await setTimeoutAndLoadPage(page, 'http://localhost:3000/src/test/auto/e2e/webapi.tests.html', 5);
  }

  test('typing with global mode keyboard', async ({ page }: { page: Page }) => {
    // Setup
    await beforeEach(page);
    const inputLocator = page.getByLabel('input1');
    await inputLocator.click();

    // Execute
    await inputLocator.press('s');

    // Verify
    await expect(inputLocator).toHaveValue('ຊ', { timeout: 500 });
  });

  test('typing with independent mode keyboard - input3', async ({ page }: { page: Page }) => {
    // Setup
    await beforeEach(page);
    const inputLocator = page.getByLabel('input3');
    await inputLocator.click();

    // Execute
    await inputLocator.press('e');

    // Verify
    await expect(inputLocator).toHaveValue('ק', { timeout: 500 });
  });

  test('typing with independent mode keyboard - input4', async ({ page }: { page: Page }) => {
    // Setup
    await beforeEach(page);
    const inputLocator = page.getByLabel('input4');
    await inputLocator.click();

    // Execute
    await inputLocator.press('y');

    // Verify
    await expect(inputLocator).toHaveValue('ब', { timeout: 500 });
  });
});

test.describe('Typing with OSK', function () {
  const beforeEach = async (page: Page) => {
    // output messages from the browser console to the test output, for debugging
    page.on('console', msg => console.log(msg.text()));

    await setTimeoutAndLoadPage(page, 'http://localhost:3000/src/test/auto/e2e/webapi.tests.html', 5);
  }

  const pressKeyOnOsk = async (page: Page, keyId: string): Promise<void> => {
    await page.locator(`#default-${keyId}`).click();
  }

  test('typing with global mode keyboard', async ({ page }: { page: Page }) => {
    // Setup
    await beforeEach(page);
    const inputLocator = page.getByLabel('input1');
    await inputLocator.click();

    // Execute
    await pressKeyOnOsk(page, 'K_S');

    // Verify
    await expect(inputLocator).toHaveValue('ຊ', { timeout: 500 });
  });

  test('typing with independent mode keyboard - input3', async ({ page }: { page: Page }) => {
    // Setup
    await beforeEach(page);
    const inputLocator = page.getByLabel('input3');
    await inputLocator.click();

    // Execute
    await pressKeyOnOsk(page, 'K_E');

    // Verify
    await expect(inputLocator).toHaveValue('ק', { timeout: 500 });
  });

  test('typing with independent mode keyboard - input4', async ({ page }: { page: Page }) => {
    // Setup
    await beforeEach(page);
    const inputLocator = page.getByLabel('input4');
    await inputLocator.click();

    // Execute
    await pressKeyOnOsk(page, 'K_Y');

    // Verify
    await expect(inputLocator).toHaveValue('ब', { timeout: 500 });
  });
});
