package com.keyman.android;

import android.content.Context;

import com.keyman.engine.KMManager;
import com.keyman.engine.util.FileUtils;
import com.keyman.engine.util.KMLog;
import com.keyman.engine.util.KMString;

import java.io.File;

public class BannerController {

  // Paths relative to assets folder for banner themes
  public static final String KM_BANNER_THEME_KEYMAN = "svg/banner.html";
  public static final String KM_BANNER_THEME_KEYMAN_SVG = "svg/keyman_banner.svg";


  public static void setHTMLBanner(Context context, KMManager.KeyboardType keyboardType) {
    if (keyboardType == KMManager.KeyboardType.KEYBOARD_TYPE_UNDEFINED) {
      return;
    }

    // Always use Keyman banner theme
    String contents = FileUtils.readContents(context, KM_BANNER_THEME_KEYMAN);

    // If $BANNER string exists, replace with actual path
    File bannerPath = new File(KMManager.getResourceRoot(), KM_BANNER_THEME_KEYMAN_SVG);
    if (bannerPath.exists()) {
      contents = contents.replace("$BANNER", bannerPath.getAbsolutePath());
    }

    KMManager.setHTMLBanner(keyboardType, contents);
    KMManager.setBanner(keyboardType, KMManager.BannerType.HTML);
    KMManager.showBanner(true);
  }
}
