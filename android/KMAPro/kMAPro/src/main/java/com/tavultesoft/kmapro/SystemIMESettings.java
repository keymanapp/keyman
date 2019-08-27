package com.tavultesoft.kmapro;

import android.content.Context;
import android.provider.Settings;
import android.view.inputmethod.InputMethodInfo;
import android.view.inputmethod.InputMethodManager;

import java.util.List;

public class SystemIMESettings {
  static boolean isEnabledAsSystemKB(Context context) {
    InputMethodManager imManager = (InputMethodManager) context.getSystemService(Context.INPUT_METHOD_SERVICE);
    List<InputMethodInfo> imList = imManager.getEnabledInputMethodList();
    boolean isEnabled = false;
    int size = imList.size();
    for (int i = 0; i < size; i++) {
      if (imList.get(i).getServiceName().equals("com.keyman.android.SystemKeyboard")) {
        isEnabled = true;
        break;
      }
    }

    return isEnabled;
  }

  static boolean isDefaultKB(Context context) {
    String inputMethod = Settings.Secure.getString(context.getContentResolver(), Settings.Secure.DEFAULT_INPUT_METHOD);
    return inputMethod.equals(context.getPackageName() + "/com.keyman.android.SystemKeyboard");
  }
}
