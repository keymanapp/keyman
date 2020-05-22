/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.json.JSONArray;
import org.json.JSONObject;
import com.tavultesoft.kmea.cloud.CloudDataJsonUtil;
import com.tavultesoft.kmea.util.MapCompat;

import androidx.annotation.NonNull;
import android.content.Context;
import android.content.Intent;
import android.util.Log;

/**
 * "Add new keyboard"
 * Gets the list of installable languages from Keyman cloud and allows user to download a keyboard.
 * After downloading, the new keyboard is selected.
 *
 */
public final class LanguageListUtil {

  private Context context;

  private static final String TAG = "LanguageListUtility";

  // These two JSON objects and their getters are still used by legacy metadata functions.
  private static JSONArray languages = null;

  protected static JSONArray languages() {
    return languages;
  }

  private static JSONObject options = null;

  protected static JSONObject options() {
    return options;
  }

  // Also these.
  private static HashMap<String, HashMap<String, String>> keyboardsInfo = null;
  private static HashMap<String, String> keyboardModifiedDates = null;
}