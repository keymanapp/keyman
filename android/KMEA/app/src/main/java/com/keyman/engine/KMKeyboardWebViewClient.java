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
import com.keyman.engine.KMManager;
import com.keyman.engine.KMManager.KeyboardType;
import com.keyman.engine.util.KMLog;
import com.keyman.engine.data.Keyboard;

import org.json.JSONObject;

import java.util.ArrayList;
import java.util.HashMap;

public final class KMKeyboardWebViewClient extends WebViewClient {
  public static final String TAG = "KMKeyboardWebViewClient";

  public Context context;
  private KeyboardType keyboardType;
  private boolean keyboardLoaded;

  KMKeyboardWebViewClient(Context context, KeyboardType keyboardType) {
    this.context = context;
    this.keyboardType = keyboardType;
    this.keyboardLoaded = false;

    if (keyboardType != KeyboardType.KEYBOARD_TYPE_INAPP && keyboardType != KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      KMLog.LogError(TAG, String.format("Cannot initialize: Invalid keyboard type: %s", keyboardType.toString()));
    }
  }

  public void setContext(Context context) {
    this.context = context;
  }

  public boolean getKeyboardLoaded() {
    return keyboardLoaded;
  }

  public void setKeyboardLoaded(boolean keyboardLoaded) {
    this.keyboardLoaded = keyboardLoaded;
  }

  @Override
  public void onPageStarted(WebView view, String url, Bitmap favicon) {
  }

  @Override
  public void onPageFinished(WebView view, String url) {
    Log.d("KMEA", String.format("onPageFinished: [%s] %s", keyboardType.toString(), url));
    shouldOverrideUrlLoading(view, url);
  }

  private void pageLoaded(WebView view, String url) {
    Log.d("KMEA", String.format("pageLoaded: [%s] %s", keyboardType.toString(), url));
    KMKeyboard kmKeyboard = KMManager.getKMKeyboard(keyboardType);
    if (kmKeyboard == null) {
      KMLog.LogError(TAG, String.format("pageLoaded and %s keyboard null", keyboardType.toString()));
      return;
    }
    kmKeyboard.keyboardSet = false;
    KMManager.currentLexicalModel = null;

    if (url.startsWith("file")) { // TODO: is this test necessary?
      this.keyboardLoaded = true;

      SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
      int index = prefs.getInt(KMManager.KMKey_UserKeyboardIndex, 0);
      if (index < 0) {
        index = 0;
      }
      Keyboard keyboardInfo = KMManager.getKeyboardInfo(context, index);
      String langId = null;
      if (keyboardInfo != null) {
        langId = keyboardInfo.getLanguageID();
        kmKeyboard.setKeyboard(keyboardInfo);
      } else {
        // Revert to default (index 0) or fallback keyboard
        keyboardInfo = KMManager.getKeyboardInfo(context, 0);
        if (keyboardInfo == null) {
          // Only log SystemKeyboard to Sentry because some keyboard apps like FV don't install keyboards until the user chooses
          if (keyboardType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
            KMLog.LogError(TAG, "No keyboards installed. Reverting to fallback");
          }
          keyboardInfo = KMManager.getDefaultKeyboard(context);
        }
        if (keyboardInfo != null) {
          langId = keyboardInfo.getLanguageID();
          kmKeyboard.setKeyboard(keyboardInfo);
        }
      }

      KMManager.registerAssociatedLexicalModel(langId);

      kmKeyboard.showHelpBubbleAfterDelay(2000, true); // check if it should be shown at that time!

      kmKeyboard.callJavascriptAfterLoad();
      kmKeyboard.setSpacebarText(KMManager.getSpacebarText());

      KeyboardEventHandler.notifyListeners(KMTextView.kbEventListeners, keyboardType, EventType.KEYBOARD_LOADED, null);

      // Special handling for in-app TextView context keymanapp/keyman#3809
      if (keyboardType == KeyboardType.KEYBOARD_TYPE_INAPP &&
          KMTextView.activeView != null && KMTextView.activeView.getClass() == KMTextView.class) {
        KMTextView.updateTextContext();
      }
    }
  }

  @Override
  public boolean shouldOverrideUrlLoading(WebView view, String url) {
    Log.d("KMEA", String.format("shouldOverrideUrlLoading [%s]: %s", keyboardType.toString(), url));
    KMKeyboard kmKeyboard = KMManager.getKMKeyboard(keyboardType);
    if (kmKeyboard == null) {
      KMLog.LogError(TAG, String.format("shouldOverrideUrlLoading and %s null", keyboardType.toString()));
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
      // Dismiss help bubble - though it should only ever display with KEYBOARD_TYPE_INAPP
      kmKeyboard.dismissHelpBubble();
      if (keyboardType == KeyboardType.KEYBOARD_TYPE_INAPP) {
        if (KMTextView.activeView != null && KMTextView.activeView.getClass() == KMTextView.class) {
          KMTextView textView = (KMTextView) KMTextView.activeView;
          textView.dismissKeyboard();
        }
      } else if (keyboardType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
        if (KMManager.IMService != null) {
          KMManager.IMService.requestHideSelf(0);
        }
      }
    } else if (urlCommand.getQueryParameter("globeKeyAction") != null) {
      kmKeyboard.dismissHelpBubble();
      kmKeyboard.setShouldShowHelpBubble(false);

      // Globe key has been used; disable the internal preference setting.
      KMManager.setPersistentShouldShowHelpBubble(false);

      KMManager.handleGlobeKeyAction(context, urlCommand.getBooleanQueryParameter("keydown", false),
        keyboardType);
    } else if (urlCommand.getQueryParameter("helpBubbleDismissed") != null) {
      // The user has begun interacting with the keyboard; we'll disable the help bubble
      // for the rest of the lifetime of this keyboard instance.
      kmKeyboard.setShouldShowHelpBubble(false);
    } else if (url.indexOf("showKeyPreview") >= 0) {
      String deviceType = context.getResources().getString(R.string.device_type);
      if (deviceType.equals("AndroidTablet")) {
        return false;
      }

      if (kmKeyboard.subKeysWindow != null) {
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
      String text = kmKeyboard.convertKeyText(t);

      float left = x - w / 2.0f;
      float right = left + w;
      float top = y - 1;
      float bottom = top + h;

      RectF keyFrame = new RectF(left, top, right, bottom);
      kmKeyboard.showKeyPreview(context, (int) x, (int) y, keyFrame, text);
    } else if (url.indexOf("dismissKeyPreview") >= 0) {
      kmKeyboard.dismissKeyPreview(100);
    } else if (url.indexOf("showMore") >= 0) {
      if (kmKeyboard.subKeysWindow != null && kmKeyboard.subKeysWindow.isShowing()) {
        return false;
      }

      int start = url.indexOf("keyPos=") + 7;
      int end = url.indexOf("+keys=");
      kmKeyboard.subKeysWindowPos = url.substring(start, end).split("\\,");

      start = end + 6;
      end = url.indexOf("+font=");
      if (end < 0) {
        end = url.length();
        kmKeyboard.specialOskFont = "";
      } else {
        kmKeyboard.specialOskFont = KMManager.KMFilename_Osk_Ttf_Font;
      }

      String keys = url.substring(start, end);

      String[] keyList = keys.split("\\;");
      int klCount = keyList.length;
      kmKeyboard.subKeysList = new ArrayList<HashMap<String, String>>();
      for (int i = 0; i < klCount; i++) {
        String[] values = keyList[i].split("\\:");
        String keyId = (values.length > 0) ? values[0] : "";
        String keyText = (values.length > 1) ? values[1] : "";

        HashMap<String, String> hashMap = new HashMap<String, String>();
        hashMap.put("keyId", keyId);
        hashMap.put("keyText", keyText);
        kmKeyboard.subKeysList.add(hashMap);
      }
    } else if (url.indexOf("refreshBannerHeight") >= 0) {
      int start = url.indexOf("change=") + 7;
      String change = url.substring(start);
      boolean isModelActive = change.equals("active");
      // appContext instead of context?
      SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
      boolean modelPredictionPref = false;
      if (KMManager.currentLexicalModel != null) {
        modelPredictionPref = prefs.getBoolean(KMManager.getLanguagePredictionPreferenceKey(KMManager.currentLexicalModel.get(KMManager.KMKey_LanguageID)), true);
      }
      kmKeyboard.setCurrentBanner((isModelActive && modelPredictionPref) ?
        KMKeyboard.KM_BANNER_STATE_SUGGESTION : KMKeyboard.KM_BANNER_STATE_BLANK);
      RelativeLayout.LayoutParams params = KMManager.getKeyboardLayoutParams();
      kmKeyboard.setLayoutParams(params);
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
      kmKeyboard.reloadAfterError();
    }
    return false;
  }
}
