package com.tavultesoft.kmea;

import android.content.Context;

import com.google.firebase.analytics.FirebaseAnalytics;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.shadows.ShadowApplication;

import static org.junit.Assert.*;

/**
 * Created by joshua on 3/21/2018.
 */
@RunWith(RobolectricTestRunner.class)
public class KMKeyboardTest {
  @Mock(stubOnly = true) FirebaseAnalytics analytics;

  @Before
  public void initMocks() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void initialization() throws Exception {
    // ShadowApplication.getInstance().getApplicationContext() - provides a nice mocking for any Context requirements.
    Context context = ShadowApplication.getInstance().getApplicationContext();
    KMManager.mFirebaseAnalytics = analytics;

    // Loads the keyboard's HTML page, initializing KMW.
    KMManager.initInAppKeyboard(context);

    //KMKeyboard keyboardInterface = new KMKeyboard(context, KMManager.KeyboardType.KEYBOARD_TYPE_INAPP, analytics);
    KMKeyboard keyboardInterface = KMManager.InAppKeyboard;
    //keyboardInterface.loadUrl("javascript:");
  }

  @Test
  public void loadKeyboard() throws Exception {
  }

  @Test
  public void setKeyboard() throws Exception {
  }

  @Test
  public void getChirality() throws Exception {
  }

}