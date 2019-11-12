package com.tavultesoft.kmea.view;

import android.content.Context;
import android.content.res.AssetManager;
import android.util.Log;

import androidx.test.core.app.ApplicationProvider;

import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.packages.PackageProcessor;
import com.tavultesoft.kmea.util.FileUtils;

import org.json.JSONException;
import org.junit.Assert;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class FunctionalTestHelper {

  static void initializeKeyman()
  {
    KMManager.setTestMode(true);
    KMManager.initialize(
      ApplicationProvider.getApplicationContext(), KMManager.KeyboardType.KEYBOARD_TYPE_INAPP);
  }

  static void setInitialKeyboard()
  {
    HashMap<String, String> keyboardInfo = KMManager.getKeyboardInfo(
      ApplicationProvider.getApplicationContext(), 0);
    if (keyboardInfo != null) {
      String pkgId = keyboardInfo.get(KMManager.KMKey_PackageID);
      String kbId = keyboardInfo.get(KMManager.KMKey_KeyboardID);
      String langId = keyboardInfo.get(KMManager.KMKey_LanguageID);
      String kbName = keyboardInfo.get(KMManager.KMKey_KeyboardName);
      String langName = keyboardInfo.get(KMManager.KMKey_LanguageName);
      String kFont = keyboardInfo.get(KMManager.KMKey_Font);
      String kOskFont = keyboardInfo.get(KMManager.KMKey_OskFont);
      KMManager.setKeyboard(pkgId, kbId, langId, kbName, langName, kFont, kOskFont);
    }
  }

  static void installCustomKeyboard(File aKPMFile) throws IOException, JSONException {
    PackageProcessor kmpProcessor =  new PackageProcessor(new File(KMManager.getResourceRoot()));

    File tempPackagePath = kmpProcessor.unzipKMP(aKPMFile);
    List<Map<String, String>> installedKbds = kmpProcessor.processKMP(aKPMFile, tempPackagePath, PackageProcessor.PP_KEYBOARDS_KEY);

    Assert.assertEquals(installedKbds.size(),1);

    KMManager.addKeyboard(ApplicationProvider.getApplicationContext(),(HashMap<String, String>) installedKbds.get(0));
  }


}
