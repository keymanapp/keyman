/**
 * Keyman is copyright (C) SIL International. MIT License.
 */
package com.keyman.engine.util;

import android.content.Context;

import androidx.test.core.app.ApplicationProvider;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import com.keyman.engine.util.WebViewUtils;

@RunWith(RobolectricTestRunner.class)
public class WebViewUtilsTest {
  private Context context = null;

  @Before
  public void getContext() {
    context = ApplicationProvider.getApplicationContext();
  }

  @Test
  public void test_ChromeEmpty_EngineWebViewVersionStatusDisabled() {
    String chromeVersion = "";
    Assert.assertEquals(WebViewUtils.getEngineWebViewVersionStatus(context, null, chromeVersion),
      WebViewUtils.EngineWebViewVersionStatus.DISABLED);
  }

  @Test
  public void test_Chrome36_EngineWebViewVersionStatusDisabled() {
    double chromeVersionFloat = Float.parseFloat(WebViewUtils.KEYMAN_MIN_TARGET_VERSION_DEGRADED_ANDROID_CHROME) - 1.0;
    String chromeVersion = String.valueOf(chromeVersionFloat);
    Assert.assertEquals(WebViewUtils.getEngineWebViewVersionStatus(context, null, chromeVersion),
      WebViewUtils.EngineWebViewVersionStatus.DISABLED);
  }

  @Test
  public void test_Chrome37_EngineWebViewVersionStatusDegraded() {
    String chromeVersion = WebViewUtils.KEYMAN_MIN_TARGET_VERSION_DEGRADED_ANDROID_CHROME;
    Assert.assertEquals(WebViewUtils.getEngineWebViewVersionStatus(context, null, chromeVersion),
      WebViewUtils.EngineWebViewVersionStatus.DEGRADED);
  }

  @Test
  public void test_Chrome94_EngineWebViewVersionStatusDegraded() {
    double chromeVersionFloat = Float.parseFloat(WebViewUtils.KEYMAN_MIN_TARGET_VERSION_ANDROID_CHROME) - 1.0;
    String chromeVersion = String.valueOf(chromeVersionFloat);
    Assert.assertEquals(WebViewUtils.getEngineWebViewVersionStatus(context, null, chromeVersion),
      WebViewUtils.EngineWebViewVersionStatus.DEGRADED);
  }

  @Test
  public void test_Chrome95_EngineWebViewVersionStatusFull() {
    String chromeVersion = WebViewUtils.KEYMAN_MIN_TARGET_VERSION_ANDROID_CHROME;
    Assert.assertEquals(WebViewUtils.getEngineWebViewVersionStatus(context, null, chromeVersion),
      WebViewUtils.EngineWebViewVersionStatus.FULL);
  }
}
