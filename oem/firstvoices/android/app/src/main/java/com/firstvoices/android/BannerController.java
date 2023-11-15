package com.firstvoices.android;

import android.content.Context;

import com.keyman.engine.KMManager;
import com.keyman.engine.util.FileUtils;

import java.io.File;

public class BannerController {

  // Paths relative to assets folder for banner themes
  public static final String BANNER_THEME_FV = "svg/banner.html";
  public static final String BANNER_THEME_FV_SVG = "svg/red-logo.svg";

  public static void setHTMLBanner(Context context, KMManager.KeyboardType keyboardType) {
    if (keyboardType == KMManager.KeyboardType.KEYBOARD_TYPE_UNDEFINED) {
      return;
    }

    // Always use FirstVoices banner theme
    String contents = FileUtils.readContents(context, BANNER_THEME_FV);

    // If $BANNER string exists, replace with actual path
    File bannerPath = new File(KMManager.getResourceRoot(), BANNER_THEME_FV_SVG);
    if (bannerPath.exists()) {
      contents = contents.replace("$BANNER", bannerPath.getAbsolutePath());
    }

    KMManager.setHTMLBanner(keyboardType, contents);
    KMManager.setBanner(keyboardType, KMManager.BannerType.HTML);
    KMManager.showBanner(true);
  }
}
