/*
 * Copyright (C) 2024 SIL International. All rights reserved.
 */

package com.tavultesoft.kmapro;

import android.content.Context;
import android.content.SharedPreferences;

import com.keyman.engine.KMManager;
import com.keyman.engine.data.LexicalModel;

import java.util.HashMap;

public class DefaultLanguageResource {
  private static final String defaultKeyboardInstalled = "DefaultKeyboardInstalled";
  private static final String defaultDictionaryInstalled = "DefaultDictionaryInstalled";

  /**
   * Check if app has installed default keyboard and dictionary. If not, install them
   * @param context
   */
  public static void install(Context context) {
    SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);

    // Add default keyboard
    boolean installDefaultKeyboard = prefs.getBoolean(defaultKeyboardInstalled, false);
    if (!installDefaultKeyboard) {
      if (!KMManager.keyboardExists(context, KMManager.KMDefault_PackageID, KMManager.KMDefault_KeyboardID,
        KMManager.KMDefault_LanguageID)) {
        KMManager.addKeyboard(context, KMManager.getDefaultKeyboard(context.getApplicationContext()));
      }
      SharedPreferences.Editor editor = prefs.edit();
      editor.putBoolean(defaultKeyboardInstalled, true);
      editor.commit();
    }

    // Add default dictionary
    boolean installDefaultDictionary = prefs.getBoolean(defaultDictionaryInstalled, false);
    if (!installDefaultDictionary) {
      LexicalModel defaultLexicalModel = LexicalModel.getDefaultLexicalModel(context);
      HashMap<String, String> lexicalModelInfo = new HashMap<String, String>();
      lexicalModelInfo.put(KMManager.KMKey_PackageID, defaultLexicalModel.getPackageID());
      lexicalModelInfo.put(KMManager.KMKey_LanguageID, defaultLexicalModel.getLanguageID());
      lexicalModelInfo.put(KMManager.KMKey_LanguageName, defaultLexicalModel.getLanguageName());
      lexicalModelInfo.put(KMManager.KMKey_LexicalModelID, defaultLexicalModel.getLexicalModelID());
      lexicalModelInfo.put(KMManager.KMKey_LexicalModelName, defaultLexicalModel.getLexicalModelName());
      lexicalModelInfo.put(KMManager.KMKey_LexicalModelVersion, defaultLexicalModel.getVersion());
      /*
      // If welcome.htm exists, add custom help link
      welcomeFile = new File(KMManager.getLexicalModelsDir(), KMManager.KMDefault_DictionaryPackageID + File.separator + FileUtils.WELCOME_HTM);
      lexicalModelInfo.put(KMManager.KMKey_CustomHelpLink, welcomeFile.getPath());
       */
      KMManager.addLexicalModel(context, lexicalModelInfo);
      KMManager.registerAssociatedLexicalModel(KMManager.KMDefault_LanguageID);

      SharedPreferences.Editor editor = prefs.edit();
      editor.putBoolean(defaultDictionaryInstalled, true);
      editor.commit();
    }
  }
}
