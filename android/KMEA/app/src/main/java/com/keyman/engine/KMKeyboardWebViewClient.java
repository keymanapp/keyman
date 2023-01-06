/**
 * Copyright (C) 2023 SIL International. All rights reserved.
 */
package com.keyman.engine;

import android.content.Context;
import android.content.SharedPreferences;
import android.graphics.Bitmap;
import android.graphics.RectF;
import android.net.Uri;
import android.util.Log;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.RelativeLayout;

import com.keyman.engine.KeyboardEventHandler.EventType;
import com.keyman.engine.KMManager.KeyboardType;
import com.keyman.engine.util.KMLog;
import com.keyman.engine.data.Keyboard;

import org.json.JSONObject;

import java.util.ArrayList;
import java.util.HashMap;

protected static final class KMWKeyboardWebViewClient extends WebViewClient {
  public static Context context;

  KMInAppKeyboardWebViewClient(Context context) {
    KMInAppKeyboardWebViewClient.context = context;
  }

  @Override
  public void onPageStarted(WebView view, String url, Bitmap favicon) {
  }

  @Override
  public void onPageFinished(WebView view, String url) {
    Log.d("KMEA", "onPageFinished: [inapp] " + url);
    shouldOverrideUrlLoading(view, url);
  }

  private void pageLoaded(WebView view, String url) {
    Log.d("KMEA", "pageLoaded: [inapp] " + url);
    if (InAppKeyboard == null) {
      KMLog.LogError(TAG, "pageLoaded and InAppKeyboard null");
      return;
    }
    InAppKeyboard.keyboardSet = false;
    currentLexicalModel = null;

    if (url.startsWith("file")) { // TODO: is this test necessary?
      InAppKeyboardLoaded = true;

      SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
      int index = prefs.getInt(KMManager.KMKey_UserKeyboardIndex, 0);
      if (index < 0) {
        index = 0;
      }
      Keyboard keyboardInfo = KMManager.getKeyboardInfo(context, index);
      String langId = null;
      if (keyboardInfo != null) {
        langId = keyboardInfo.getLanguageID();
        InAppKeyboard.setKeyboard(keyboardInfo);
      } else {
        // Revert to default (index 0) or fallback keyboard
        keyboardInfo = KMManager.getKeyboardInfo(context, 0);
        if (keyboardInfo == null) {
          // Not logging to Sentry because some keyboard apps like FV don't install keyboards until the user chooses
          keyboardInfo = KMManager.getDefaultKeyboard(context);
        }
        if (keyboardInfo != null) {
          langId = keyboardInfo.getLanguageID();
          InAppKeyboard.setKeyboard(keyboardInfo);
        }
      }

      registerAssociatedLexicalModel(langId);

      InAppKeyboard.showHelpBubbleAfterDelay(2000, true); // check if it should be shown at that time!

      InAppKeyboard.callJavascriptAfterLoad();
      InAppKeyboard.setSpacebarText(spacebarText);

      KeyboardEventHandler.notifyListeners(KMTextView.kbEventListeners, KeyboardType.KEYBOARD_TYPE_INAPP, EventType.KEYBOARD_LOADED, null);

      // Special handling for in-app TextView context keymanapp/keyman#3809
      if (KMTextView.activeView != null && KMTextView.activeView.getClass() == KMTextView.class) {
        KMTextView.updateTextContext();
      }
    }
  }

  @Override
  public boolean shouldOverrideUrlLoading(WebView view, String url) {
    Log.d("KMEA", "shouldOverrideUrlLoading [inapp]: " + url);
    if (InAppKeyboard == null) {
      KMLog.LogError(TAG, "shouldOverrideUrlLoading and InAppKeyboard null");
      return false;
    }

    // URL has actual path to the keyboard.html file as a prefix!  We need to replace
    // just the first intended '#' to get URI-based query param processing.
    // At some point, other parts of the function should be redone to allow use of ? instead
    // of # in our WebView command "queries" entirely.
    String cmd = url.replace("keyboard.html#", "keyboard.html?");
    Uri urlCommand = Uri.parse(cmd);
    if (url.indexOf("pageLoaded") >= 0) {
      pageLoaded(view, url);
    } else if (url.indexOf("hideKeyboard") >= 0) {
      if (KMTextView.activeView != null && KMTextView.activeView.getClass() == KMTextView.class) {
        InAppKeyboard.dismissHelpBubble();
        KMTextView textView = (KMTextView) KMTextView.activeView;
        textView.dismissKeyboard();
      }
    } else if (urlCommand.getQueryParameter("globeKeyAction") != null) {
      InAppKeyboard.dismissHelpBubble();
      InAppKeyboard.setShouldShowHelpBubble(false);

      // Globe key has been used; disable the internal preference setting.
      KMManager.setPersistentShouldShowHelpBubble(false);

      handleGlobeKeyAction(context, urlCommand.getBooleanQueryParameter("keydown", false),
        KeyboardType.KEYBOARD_TYPE_INAPP);
    } else if (url.indexOf("helpBubbleDismissed") >= 0) {
      // The user has begun interacting with the keyboard; we'll disable the help bubble
      // for the rest of the lifetime of this keyboard instance.
      InAppKeyboard.setShouldShowHelpBubble(false);
    } else if (url.indexOf("showKeyPreview") >= 0) {
      String deviceType = context.getResources().getString(R.string.device_type);
      if (deviceType.equals("AndroidTablet")) {
        return false;
      }

      if (InAppKeyboard.subKeysWindow != null) {
        return false;
      }

      int start = url.indexOf("x=") + 2;
      int end = url.indexOf("+y=");
      float x = Float.valueOf(url.substring(start, end));

      start = url.indexOf("y=") + 2;
      end = url.indexOf("+w=");
      float y = Float.valueOf(url.substring(start, end));

      start = url.indexOf("w=") + 2;
      end = url.indexOf("+h=");
      float w = Float.valueOf(url.substring(start, end));

      start = url.indexOf("h=") + 2;
      end = url.indexOf("+t=");
      float h = Float.valueOf(url.substring(start, end));

      start = url.indexOf("t=") + 2;
      String t = url.substring(start);
      String text = InAppKeyboard.convertKeyText(t);

      float left = x - w / 2.0f;
      float right = left + w;
      float top = y - 1;
      float bottom = top + h;

      RectF keyFrame = new RectF(left, top, right, bottom);
      InAppKeyboard.showKeyPreview(context, (int) x, (int) y, keyFrame, text);
    } else if (url.indexOf("dismissKeyPreview") >= 0) {
      InAppKeyboard.dismissKeyPreview(100);
    } else if (url.indexOf("showMore") >= 0) {
      if (InAppKeyboard.subKeysWindow != null && InAppKeyboard.subKeysWindow.isShowing()) {
        return false;
      }

      int start = url.indexOf("keyPos=") + 7;
      int end = url.indexOf("+keys=");
      InAppKeyboard.subKeysWindowPos = url.substring(start, end).split("\\,");

      start = end + 6;
      end = url.indexOf("+font=");
      if (end < 0) {
        end = url.length();
        InAppKeyboard.specialOskFont = "";
      } else {
        InAppKeyboard.specialOskFont = KMFilename_Osk_Ttf_Font;
      }

      String keys = url.substring(start, end);

      String[] keyList = keys.split("\\;");
      int klCount = keyList.length;
      InAppKeyboard.subKeysList = new ArrayList<HashMap<String, String>>();
      for (int i = 0; i < klCount; i++) {
        String[] values = keyList[i].split("\\:");
        String keyId = (values.length > 0) ? values[0] : "";
        String keyText = (values.length > 1) ? values[1] : "";

        HashMap<String, String> hashMap = new HashMap<String, String>();
        hashMap.put("keyId", keyId);
        hashMap.put("keyText", keyText);
        InAppKeyboard.subKeysList.add(hashMap);
      }
    } else if (url.indexOf("refreshBannerHeight") >= 0) {
      int start = url.indexOf("change=") + 7;
      String change = url.substring(start);
      boolean isModelActive = change.equals("active");
      SharedPreferences prefs = appContext.getSharedPreferences(appContext.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
      boolean modelPredictionPref = false;
      if (currentLexicalModel != null) {
        modelPredictionPref = prefs.getBoolean(getLanguagePredictionPreferenceKey(currentLexicalModel.get(KMManager.KMKey_LanguageID)), true);
      }
      currentBanner = (isModelActive && modelPredictionPref) ?
        KM_BANNER_STATE_SUGGESTION : KM_BANNER_STATE_BLANK;
      RelativeLayout.LayoutParams params = getKeyboardLayoutParams();
      InAppKeyboard.setLayoutParams(params);
    } else if (url.indexOf("suggestPopup") >= 0) {
      double x = Float.parseFloat(urlCommand.getQueryParameter("x"));
      double y = Float.parseFloat(urlCommand.getQueryParameter("y"));
      double width = Float.parseFloat(urlCommand.getQueryParameter("w"));
      double height = Float.parseFloat(urlCommand.getQueryParameter("h"));
      String suggestionJSON = urlCommand.getQueryParameter("suggestion");

      JSONParser parser = new JSONParser();
      JSONObject obj = parser.getJSONObjectFromURIString(suggestionJSON);

        /*  // For future implementation
        InAppKeyboard.suggestionWindowPos = new double[]{x, y};
        InAppKeyboard.suggestionJSON = suggestionJSON;
        try {
          Log.v("KMEA", "Suggestion display: " + obj.getString("displayAs"));
          Log.v("KMEA", "Suggestion's banner coords: " + x + ", " + y + ", " + width + ", " + height);
          Log.v("KMEA", "Is a <keep> suggestion: "); // likely outdated now that tags exist.
        } catch (JSONException e) {
          //e.printStackTrace();
          Log.v("KMEA", "JSON parsing error: " + e.getMessage());
        }
        */
    } else if (url.indexOf("reloadAfterError") >= 0) {
      InAppKeyboard.reloadAfterError();
    }
    return false;
  }
}