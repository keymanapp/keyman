/**
 * Keyman is copyright (C) SIL International. MIT License.
 */
package com.tavultesoft.kmea.util;

import android.content.Context;

import androidx.test.core.app.ApplicationProvider;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import com.tavultesoft.kmea.util.WebViewUtils;

@RunWith(RobolectricTestRunner.class)
public class WebViewUtilsTest {
  private Context context = null;

  @Before
  public void getContext() {
    context = ApplicationProvider.getApplicationContext();
  }

  @Test
  public void test_ChromeEmpty_EngineModeTypeDisabled() {
    String chromeVersion = "";
    Assert.assertEquals(WebViewUtils.getEngineModeType(context, chromeVersion),
      WebViewUtils.EngineModeType.ENGINE_MODE_TYPE_DISABLED);
  }

  @Test
  public void test_Chrome36_EngineModeTypeDisabled() {
    String chromeVersion = "36.0";
    Assert.assertEquals(WebViewUtils.getEngineModeType(context, chromeVersion),
      WebViewUtils.EngineModeType.ENGINE_MODE_TYPE_DISABLED);
  }

  @Test
  public void test_Chrome57_EngineModeTypeDegraded() {
    String chromeVersion = "57.0";
    Assert.assertEquals(WebViewUtils.getEngineModeType(context, chromeVersion),
      WebViewUtils.EngineModeType.ENGINE_MODE_TYPE_DEGRADED);
  }

  @Test
  public void test_Chrome58_EngineModeTypeFull() {
    String chromeVersion = "58.0";
    Assert.assertEquals(WebViewUtils.getEngineModeType(context, chromeVersion),
      WebViewUtils.EngineModeType.ENGINE_MODE_TYPE_FULL);
  }
}
