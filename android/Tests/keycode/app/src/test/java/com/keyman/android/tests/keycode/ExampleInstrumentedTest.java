package com.keyman.android.tests.keycode;

import androidx.test.core.app.ApplicationProvider;

import android.content.Context;

import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;

/**
 * Instrumented test, which will execute on an Android device.
 *
 * @see <a href="http://d.android.com/tools/testing">Testing documentation</a>
 */
@RunWith(RobolectricTestRunner.class)
public class ExampleInstrumentedTest {
  @Test
  public void useAppContext() {
    // Context of the app under test.
    Context appContext = ApplicationProvider.getApplicationContext();

    assertEquals("com.keyman.android.tests.keycode", appContext.getPackageName());
  }
}
