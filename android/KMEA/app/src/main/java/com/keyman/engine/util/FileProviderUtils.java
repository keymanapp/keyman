package com.keyman.engine.util;

import android.content.Context;
import android.content.ContentProviderClient;

public final class FileProviderUtils {
  public static String getAuthority(Context context) {
    return context.getApplicationContext().getPackageName() + ".fileProvider";
  }

  public static boolean exists(Context context) {
    String authority = getAuthority(context);
    ContentProviderClient client = context.getContentResolver().acquireContentProviderClient(authority);
    return (client != null);
  }
}