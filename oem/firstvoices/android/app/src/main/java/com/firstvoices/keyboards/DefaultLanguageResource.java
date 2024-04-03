package com.firstvoices.keyboards;

import android.content.Context;
import android.content.SharedPreferences;

import com.keyman.engine.KMManager;
import com.keyman.engine.data.Keyboard;
import com.keyman.engine.data.KeyboardController;
import com.keyman.engine.data.LexicalModel;

import java.util.HashMap;

public class DefaultLanguageResource {
  private static final String defaultKeyboardInstalled = "DefaultKeyboardInstalled";
  private static final String defaultDictionaryInstalled = "DefaultDictionaryInstalled";

  /**
   * Check if app has installed default keyboard. If not, install it as fallback
   * @param context
   */
  public static void install(Context context) {
    /**
     * We need to set the default (fallback) keyboard to sil_euro_latin inside the fv_all package
     * rather than the normal default of sil_euro_latin inside the sil_euro_latin package.
     * Fallback keyboard needed in case the user never selects a FV keyboard to add
     * as a system keyboard.
     */
    String version = KMManager.getLatestKeyboardFileVersion(
      context, FVShared.FVDefault_PackageID, KMManager.KMDefault_KeyboardID);
    KMManager.setDefaultKeyboard(
      new Keyboard(
        FVShared.FVDefault_PackageID,
        KMManager.KMDefault_KeyboardID,
        KMManager.KMDefault_KeyboardName,
        KMManager.KMDefault_LanguageID,
        KMManager.KMDefault_LanguageName,
        version,
        null, // will use help.keyman.com link because context required to determine local welcome.htm path,
        "",
        false,
        KMManager.KMDefault_KeyboardFont,
        KMManager.KMDefault_KeyboardFont)
    );

    SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);

    // Add default keyboard if no keyboards exist
    if (!prefs.getBoolean(defaultKeyboardInstalled, false)) {
      if (!KMManager.keyboardExists(context, FVShared.FVDefault_PackageID, KMManager.KMDefault_KeyboardID,
        KMManager.KMDefault_LanguageID) && KeyboardController.getInstance().get().size() < 1) {

        KMManager.addKeyboard(context, KMManager.getDefaultKeyboard(context.getApplicationContext()));
      }
      SharedPreferences.Editor editor = prefs.edit();
      editor.putBoolean(defaultKeyboardInstalled, true);
      editor.commit();
    }

    // No default dictionary to install

  }
}
