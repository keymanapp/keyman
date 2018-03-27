package com.saucelabs.appium;

import io.appium.java_client.AppiumDriver;
import io.appium.java_client.MobileBy;
import io.appium.java_client.MobileElement;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.service.local.AppiumDriverLocalService;
import io.appium.java_client.service.local.AppiumServerHasNotBeenStartedLocallyException;
import io.appium.java_client.service.local.AppiumServiceBuilder;

import org.junit.After;
import org.junit.Before;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.remote.DesiredCapabilities;

import java.io.File;
import java.net.URL;

// In order to successfully run this test, you must use "Build APKs" before running it,
// rather than the default Run/Debug Android Studio options.
public class BaseDriver {
  public AppiumDriver<MobileElement> driver;
  private static AppiumDriverLocalService service;

  @Before
  public void setUp() throws Exception {
    // We use a local copy of Appium's JS library, so we need to set its path properly.
    // The path appears to vary between build.sh and runs in Android Studio; the following should handle both.
    File appiumJS = new File("../../node_modules/appium/build/lib/appium.js");
    boolean fromShellScript = false;

    if(!appiumJS.exists()) {
      appiumJS = new File("../", appiumJS.toString());
      fromShellScript = true;
    }
    AppiumServiceBuilder serviceBuilder = new AppiumServiceBuilder();
    serviceBuilder.withAppiumJS(appiumJS);

    service = AppiumDriverLocalService.buildService(serviceBuilder);
    service.start();

    File classpathRoot = new File(System.getProperty("user.dir"));
    File appDir = new File(classpathRoot, (fromShellScript ? "" : "app/") + "build/outputs/apk/debug");
    File app = new File(appDir.getCanonicalPath(), "app-debug.apk");
    DesiredCapabilities capabilities = new DesiredCapabilities();
    capabilities.setCapability("deviceName","Android Emulator");
    capabilities.setCapability("platformName", "Android");
    capabilities.setCapability("app", app.getAbsolutePath());
    capabilities.setCapability("appPackage", "com.keyman.android.tests.keyboardHarness");
    capabilities.setCapability("appActivity", ".MainActivity");
    capabilities.setCapability("automationName", "UiAutomator2");
    capabilities.setCapability("skipUnlock", true);
    driver = new AndroidDriver<>(service.getUrl(), capabilities);
  }

  @After
  public void tearDown() throws Exception {
    if (driver != null) {
      driver.quit();
    }
    if (service != null) {
      service.stop();
    }
  }


  public MobileElement scrollTo(String text){
    return (MobileElement) driver.findElement(MobileBy.
      AndroidUIAutomator("new UiScrollable(new UiSelector()"
        + ".scrollable(true)).scrollIntoView(resourceId(\"android:id/list\")).scrollIntoView("
        + "new UiSelector().text(\""+text+"\"))"));
  }
}