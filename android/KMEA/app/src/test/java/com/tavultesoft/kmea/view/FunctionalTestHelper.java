package com.keyman.engine.view;

import android.Manifest;
import android.app.Application;
import android.content.Context;
import android.content.res.AssetManager;
import android.util.Log;

import androidx.test.core.app.ApplicationProvider;

import com.keyman.engine.KMManager;
import com.keyman.engine.data.Keyboard;
import com.keyman.engine.packages.PackageProcessor;
import com.keyman.engine.util.FileUtils;

import org.json.JSONException;
import org.junit.Assert;
import org.robolectric.Shadows;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Helper methods for functional tests.
 */
public class FunctionalTestHelper {

  /**
   * initialize keyman for tests.
   */
  static void initializeKeyman()
  {

    if(!KMManager.isTestMode())
      throw new IllegalStateException("Set system property kmeaTestmode=true");
    KMManager.initialize(
      ApplicationProvider.getApplicationContext(), KMManager.KeyboardType.KEYBOARD_TYPE_INAPP);
    Shadows.shadowOf((Application) ApplicationProvider.getApplicationContext()).grantPermissions(
      Manifest.permission.READ_EXTERNAL_STORAGE);
  }

  /**
   * set initial keyboard English.
   */
  static void setInitialKeyboard()
  {
    KMManager.setKeyboard(KMManager.getDefaultKeyboard(ApplicationProvider.getApplicationContext()));
  }

  /**
   * install a keyboard.
   * @param aKPMFile the file
   * @throws IOException
   * @throws JSONException
   */
  static void installCustomKeyboard(File aKPMFile) throws IOException, JSONException {
    PackageProcessor kmpProcessor =  new PackageProcessor(new File(KMManager.getResourceRoot()));

    File tempPackagePath = kmpProcessor.unzipKMP(aKPMFile);
    List<Map<String, String>> installedKbds = kmpProcessor.processKMP(aKPMFile, tempPackagePath, PackageProcessor.PP_KEYBOARDS_KEY);

    Assert.assertEquals(installedKbds.size(),1);

    KMManager.addKeyboard(ApplicationProvider.getApplicationContext(),(HashMap<String, String>) installedKbds.get(0));
  }


}
