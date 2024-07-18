package com.firstvoices.android;

import android.content.Context;

import com.keyman.engine.KMManager;
import com.keyman.engine.util.FileUtils;

import java.io.File;

public class BannerController {

  // Paths relative to assets folder for banner themes
  public static final String FV_BANNER_DIR = "banner";
  public static final String FV_BANNER_THEME = "banner.html";

  public static void setHTMLBanner(Context context, KMManager.KeyboardType keyboardType) {
    if (keyboardType == KMManager.KeyboardType.KEYBOARD_TYPE_UNDEFINED) {
      return;
    }

    KMManager.copyHTMLBannerAssets(context, FV_BANNER_DIR);

    // Always use FirstVoices banner theme
    String contents = FileUtils.readContents(context, FV_BANNER_THEME);
    KMManager.setHTMLBanner(keyboardType, contents);
    KMManager.setBanner(keyboardType, KMManager.BannerType.HTML);
    KMManager.showBanner(true);
  }
}
