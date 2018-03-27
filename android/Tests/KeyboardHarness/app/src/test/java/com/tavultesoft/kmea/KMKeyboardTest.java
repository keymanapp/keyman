package com.tavultesoft.kmea;

import android.content.Context;
import android.util.Log;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.remote.Response;


import java.util.List;
import java.util.Map;
import java.util.Set;

import io.appium.java_client.MobileElement;
import io.appium.java_client.PerformsTouchActions;
import io.appium.java_client.TouchAction;

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
    MobileElement textView = driver.findElement(By.id("kmTextView"));
    MobileElement kbdWebView = driver.findElement(By.className("android.webkit.WebView"));

    assertNotNull(textView);
    assertNotNull(kbdWebView);

    assertEquals("", textView.getText());

    //System.out.println(driver.getContextHandles());
    driver.context(KEYBOARD_CONTEXT);

    // Ensures the OSK is visible
    MobileElement osk = driver.findElement(By.cssSelector(".kmw-osk-frame"));
    assertNotNull(osk);
    assertTrue(osk.isDisplayed());

    // Interesting part of how we display the OSK:
    // <div class="kmw-osk-inner-frame kmw-keyboard-khmer_angkor" style="font-size: 120%;"></div>
    // The keyboard ID is accessible through the class!

    // Ensures the OSK for the default language is built.
    List<MobileElement> keys = osk.findElements(By.cssSelector(".kmw-key-layer-group"));
    assertFalse(keys.isEmpty());
  }

  @Test
  public void keyboardSwapTest() throws InterruptedException {
    // Acquires the Android app elements for our two main points of interest.
    MobileElement textView = driver.findElement(By.id("kmTextView"));
    MobileElement kbdWebView = driver.findElement(By.className("android.webkit.WebView"));

    driver.context(KEYBOARD_CONTEXT);

    // Ensures the OSK is visible
    MobileElement osk = driver.findElement(By.cssSelector(".kmw-osk-frame"));
    assertNotNull(osk);
    assertTrue(osk.isDisplayed());

    // Gets the "Globe" key for keyboard selection.
    // FIXME:  We miiight be making an assumption on the exact key ID.
    MobileElement kbdSelectKey = osk.findElement(By.id("default-K_LOPT"));
    assertNotNull(kbdSelectKey);

    // Sigh.  Just _couldn't_ be that easy, could it?
    // The AndroidDriver refuses to perform TouchActions within a WebView.
    //driver.performTouchAction(new TouchAction(driver).press(kbdSelectKey));
    tapKeyboardElement(kbdSelectKey);
    // We'll need to simulate a KMW key touch command.

    // KMW transfers control to the main app for this.
    driver.context(NATIVE_CONTEXT);

    checkForAlerts();

    MobileElement keyboardList = null;
    do {
      try {
        keyboardList = driver.findElement(By.id("com.keyman.android.tests.keyboardHarness:id/listView"));
      } catch (Exception e) {
        checkForAlerts();
        Thread.sleep(1000);
        System.out.println("Waiting for the listView to become available.");
      }
    } while(keyboardList == null);

    List<MobileElement> keyboardListItems = keyboardList.findElements(By.className("android.widget.RelativeLayout"));

    assertEquals(2, keyboardListItems.size());
  }

  public void tapKeyboardElement(WebElement element) throws InterruptedException {
    // Utilizes the KMW Recorder's keyboard automation interface script.
    String script = "(function() {" +
      "var clickEvent = new KMWRecorder.OSKInputEvent('" + element.getAttribute("id") + "');" +
      "clickEvent.simulateEventOn(document.getElementById('ta'));" +
      "var key = document.getElementById('" + element.getAttribute("id") + "');" +
      "console.log('" + element.getAttribute("id") + ": ' + (key ? 'found' : 'missing'));" +
      "})()";

    Object a = driver.executeScript(script);

    Thread.sleep(1000);
  }

  public void checkForAlerts() throws InterruptedException {
    // First issue - that darn update checker.
    int updateTimeLeft = 10;
    String msg = "Checking keyboard updates...";

    // The alert message is contained in an element with this ID.
    MobileElement updateMsg;
    try {
      updateMsg = driver.findElement(By.id("message"));
    } catch(Exception e) {
      updateMsg = null;
    }

    while(updateMsg != null && updateTimeLeft > 0) {
      if(!updateMsg.getText().equals(msg)) {
        msg = updateMsg.getText();

        // We have a new alert!  Reset our message timer.
        updateTimeLeft = 11;
      }

      if(updateTimeLeft-- == 0) {
        fail("App locked on an alert message!");
      }

      Thread.sleep(1000);

      try {
        updateMsg = driver.findElement(By.id("message"));
      } catch(Exception e) {
        updateMsg = null;
      }
    }
  }
}