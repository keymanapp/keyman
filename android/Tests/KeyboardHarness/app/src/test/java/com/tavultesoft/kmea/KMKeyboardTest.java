package com.tavultesoft.kmea;

import android.content.Context;
import android.util.Log;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;


import java.util.List;
import java.util.Set;

import static org.junit.Assert.*;

/**
 * Created by jahorton on 3/26/2018.
 *
 * Designed for integration testing between KMEA and embedded KMW.
 */
public class KMKeyboardTest extends com.saucelabs.appium.BaseDriver {

  private static final String NATIVE_CONTEXT = "NATIVE_APP";
  private static final String KEYBOARD_CONTEXT = "WEBVIEW_com.keyman.android.tests.keyboardHarness";

  @Test
  public void initTest() {
    // Acquires the Android app elements for our two main points of interest.
    WebElement textView = driver.findElement(By.id("kmTextView"));
    WebElement kbdWebView = driver.findElement(By.className("android.webkit.WebView"));

    assertNotNull(textView);
    assertNotNull(kbdWebView);

    assertEquals("", textView.getText());

    System.out.println(driver.getContextHandles());
    driver.context(KEYBOARD_CONTEXT);

    // Ensures the OSK is visible
    WebElement osk = driver.findElement(By.cssSelector(".kmw-osk-frame"));
    assertNotNull(osk);
    assertTrue(osk.isDisplayed());

    // Ensures the OSK for the default language is built.
    List<WebElement> keys = osk.findElements(By.cssSelector(".kmw-key-layer-group"));
    assertFalse(keys.isEmpty());
  }
}