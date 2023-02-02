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
    String chromeVersion = "36.0";
    Assert.assertEquals(WebViewUtils.getEngineWebViewVersionStatus(context, null, chromeVersion),
      WebViewUtils.EngineWebViewVersionStatus.DISABLED);
  }

  @Test
  public void test_Chrome37_EngineWebViewVersionStatusDegraded() {
    String chromeVersion = "37.0";
    Assert.assertEquals(WebViewUtils.getEngineWebViewVersionStatus(context, null, chromeVersion),
      WebViewUtils.EngineWebViewVersionStatus.DEGRADED);
  }

  @Test
  public void test_Chrome56_EngineWebViewVersionStatusDegraded() {
    String chromeVersion = "56.0";
    Assert.assertEquals(WebViewUtils.getEngineWebViewVersionStatus(context, null, chromeVersion),
      WebViewUtils.EngineWebViewVersionStatus.DEGRADED);
  }

  @Test
  public void test_Chrome57_EngineWebViewVersionStatusFull() {
    String chromeVersion = "57.0";
    Assert.assertEquals(WebViewUtils.getEngineWebViewVersionStatus(context, null, chromeVersion),
      WebViewUtils.EngineWebViewVersionStatus.FULL);
  }
}
