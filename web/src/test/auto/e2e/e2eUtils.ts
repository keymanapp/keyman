/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { type Locator, type Page } from "@playwright/test";

/**
 * Expands the keyboard selection menu and returns the text content of the
 * currently selected keyboard.
 */
export async function getSelectedKeyboardMenuText(page: Page): Promise<string | undefined> {
  const watchDog = page.waitForFunction(() => !!document.getElementById('KeymanWeb_KbdList'));
  await page.getByRole('img', { name: 'Use Web Keyboard' }).click();
  await watchDog;
  return page.evaluate(() => {
    const selectedKbd = document.querySelector('#kmwico .selected');
    return selectedKbd?.textContent;
  });
};

/**
 * Expands the keyboard selection menu and returns the menu items as an array
 */
export async function getAllKeyboardMenuText(page: Page): Promise<(string|undefined)[]> {
  const watchDog = page.waitForFunction(() => !!document.getElementById('KeymanWeb_KbdList'));
  await page.getByRole('img', { name: 'Use Web Keyboard' }).hover();
  await watchDog;
  return page.evaluate(() => {
    const menuItems = [];
    const menuDiv = document.querySelector('#kmwico');
    const kbdList = menuDiv?.lastElementChild;
    for (let i = 0; i < (kbdList ? kbdList.children.length : 0); i++) {
      const item = kbdList?.children[i];
      menuItems.push(item?.textContent);
    }
    return menuItems;
  });
}

/**
 * Loads the specified URL and waits for the page load event.
 */
export async function loadPage(page: Page, url: string): Promise<Page> {
  const loadPromise = page.waitForEvent('load');
  await page.goto(url);
  return loadPromise;
}

/**
 * Clicks the specified field and waits for the OSK to be shown, returning a
 * locator for the OSK title bar.
 */
export async function clickFieldAndWaitForOSK(page: Page, fieldLocator: Locator): Promise<Locator> {
  const keyboardchangePromise = page.evaluate(async () => {
    return new Promise((resolve) => {
      keyman.addEventListener('keyboardchange', function (kbd) {
        resolve(kbd);
      });
    });
  });
  await fieldLocator.click();
  await keyboardchangePromise;
  return page.locator('#keymanweb_title_bar');
}
