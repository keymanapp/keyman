/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { type Locator, type Page } from "@playwright/test";

declare const keyman: any;

/**
 * Wait until the keyboard menu is updated and shows the active keyboard
 */
export async function waitForKeyboardSelection(page: Page): Promise<void> {
  await page.waitForFunction(() => {
    const activeKbd = keyman.getActiveKeyboard();
    const activeLang = keyman.getActiveLanguage();
    const selectedElem = document.querySelector('#kmwico .selected');
    if (!selectedElem) {
      return false;
    }
    const selectedText = (selectedElem.textContent || '').trim();

    if (!activeKbd) {
      return selectedText === '(System keyboard)';
    }

    const keyboards = keyman.getKeyboards();
    for (const kbd of keyboards) {
      if (kbd.InternalName === activeKbd && kbd.LanguageCode === activeLang) {
        return selectedText === `${kbd.LanguageName} - ${kbd.Name}`;
      }
    }
    return false;
  }, { timeout: 5000 });
}

/**
 * Expands the keyboard selection menu and returns the text content of the
 * currently selected keyboard.
 */
export async function getSelectedKeyboardMenuText(page: Page): Promise<string | undefined> {
  await page.waitForFunction(() => document.getElementById('kmwico'));
  await page.locator('#kmwico').hover();
  await page.waitForFunction(() => document.querySelector('#KeymanWeb_KbdList.sfhover'));
  await waitForKeyboardSelection(page);
  return page.evaluate(() => {
    const selectedKbd = document.querySelector('#kmwico .selected');
    return selectedKbd?.textContent?.trim();
  });
}

/**
 * Expands the keyboard selection menu and returns the menu items as an array
 */
export async function getAllKeyboardMenuText(page: Page): Promise<(string|undefined)[]> {
  await page.waitForFunction(() => document.getElementById('kmwico'));
  await page.locator('#kmwico').hover();
  await page.waitForFunction(() => document.querySelector('#KeymanWeb_KbdList.sfhover'));
  return page.evaluate(() => {
    const menuItems: (string | undefined)[] = [];
    const menuDiv = document.querySelector('#kmwico');
    const kbdList = menuDiv?.lastElementChild;
    for (let i = 0; i < (kbdList ? kbdList.children.length : 0); i++) {
      const item = kbdList?.children[i];
      menuItems.push(item?.textContent?.trim());
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
  await fieldLocator.click();
  await waitForKeyboardSelection(page);
  await page.waitForFunction(() => keyman.osk.isVisible());
  return page.locator('#keymanweb_title_bar');
}
