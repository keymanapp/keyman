package com.tavultesoft.kmea;

import android.content.Context;
import android.util.Log;

import com.google.firebase.analytics.FirebaseAnalytics;
import com.tavultesoft.kmea.util.SimpleFuture;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;
import org.robolectric.shadows.ShadowApplication;
import org.robolectric.shadows.ShadowLog;
import org.robolectric.shadows.ShadowWebView;

import java.util.concurrent.TimeUnit;

import static org.junit.Assert.*;
import static org.robolectric.Shadows.shadowOf;

/**
 * Created by jahorton on 3/21/2018.
 *
 * Designed for integration testing between KMEA and embedded KMW.
 */
@RunWith(RobolectricTestRunner.class)
@Config(constants = BuildConfig.class)
public class KMKeyboardTest {
  @Mock(stubOnly = true) private FirebaseAnalytics analytics;

  @Before
  public void initMocks() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void initialization() throws Exception {
    // ShadowApplication.getInstance().getApplicationContext() - provides a nice mocking for any Context requirements.
    Context context = ShadowApplication.getInstance().getApplicationContext();
    // Ensures Android logging goes to our system output.
    ShadowLog.stream = System.out;

    // Needed KMManager setup.
    KMManager.test_initialize(context, analytics);
    // Loads the keyboard's HTML page, initializing KMW.
    SimpleFuture<Boolean> initSuccess = KMManager.initInAppKeyboard(context);

    boolean result = false;
    try {
      result = initSuccess.get(1, TimeUnit.SECONDS);
    } finally {
      if (!result) {
        KMKeyboard kbdInterface = KMManager.InAppKeyboard;

        // ... Robolectric's implementation is just a hollow shell.  *sigh*.
        String url = shadowOf(kbdInterface).getLastLoadedUrl();
        assertNotNull(url);
      }

      assertTrue(result);
    }
  }

//  @Test
//  public void loadKeyboard() throws Exception {
//  }
//
//  @Test
//  public void setKeyboard() throws Exception {
//  }
//
//  @Test
//  public void getChirality() throws Exception {
//  }

}