/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmapro;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import com.tavultesoft.kmapro.R;
import com.tavultesoft.kmea.KMManager;

import android.os.Bundle;
import android.provider.Settings;
import android.util.Log;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.view.inputmethod.InputMethodInfo;
import android.view.inputmethod.InputMethodManager;
import android.widget.AdapterView;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.CompoundButton.OnCheckedChangeListener;
import android.widget.ImageButton;
import android.widget.ListView;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;

public class GetStartedActivity extends Activity {

  private static ListView listView = null;
  private static ArrayList<HashMap<String, String>> list = null;
  private static KMListAdapter listAdapter = null;
  private final String iconKey = "icon";
  private final String textKey = "text";
  private final String isEnabledKey = "isEnabled";

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    final Context context = this;
    requestWindowFeature(Window.FEATURE_CUSTOM_TITLE);
    try {
      int titleContainerId = (Integer) Class.forName("com.android.internal.R$id").getField("title_container").get(null);
      ((ViewGroup) getWindow().findViewById(titleContainerId)).removeAllViews();
    } catch (Exception e) {
      Log.e("GetStartedActivity", e.getMessage());
    }

    getWindow().setFeatureInt(Window.FEATURE_CUSTOM_TITLE, R.layout.get_started_title_layout);
    setContentView(R.layout.get_started_list_layout);
    listView = (ListView) findViewById(R.id.listView);

    final ImageButton closeButton = (ImageButton) findViewById(R.id.close_button);
    closeButton.setOnClickListener(new View.OnClickListener() {
      public void onClick(View v) {
        finish();
      }
    });

    final SharedPreferences prefs = getSharedPreferences(getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    boolean dontShowGetStarted = prefs.getBoolean(MainActivity.dontShowGetStartedKey, false);

    final CheckBox checkBox = (CheckBox) findViewById(R.id.checkBox);
    checkBox.setChecked(dontShowGetStarted);
    checkBox.setOnCheckedChangeListener(new OnCheckedChangeListener() {
      @Override
      public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
        SharedPreferences.Editor editor = prefs.edit();
        editor.putBoolean(MainActivity.dontShowGetStartedKey, isChecked);
        editor.commit();
      }
    });

    list = new ArrayList<HashMap<String, String>>();

    HashMap<String, String> hashMap = new HashMap<String, String>();
    hashMap.put(iconKey, "0");
    hashMap.put(textKey, "Add a keyboard for your language");
    hashMap.put(isEnabledKey, "true");
    list.add(hashMap);

    hashMap = new HashMap<String, String>();
    hashMap.put(iconKey, "0");
    hashMap.put(textKey, "Enable Keyman as system-wide keyboard");
    hashMap.put(isEnabledKey, "true");
    list.add(hashMap);

    hashMap = new HashMap<String, String>();
    hashMap.put(iconKey, "0");
    hashMap.put(textKey, "Set Keyman as default keyboard");
    hashMap.put(isEnabledKey, "false");
    list.add(hashMap);

    hashMap = new HashMap<String, String>();
    hashMap.put(iconKey, String.valueOf(R.drawable.ic_light_action_info));
    hashMap.put(textKey, "More info");
    hashMap.put(isEnabledKey, "true");
    list.add(hashMap);

    String[] from = new String[]{iconKey, textKey};
    int[] to = new int[]{R.id.left_icon, R.id.text};
    listAdapter = new KMListAdapter(context, list, R.layout.get_started_row_layout, from, to);
    listView.setAdapter(listAdapter);
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
      @Override
      public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        if (position == 0) {
          KMManager.showLanguageList(context);
        } else if (position == 1) {
          startActivity(new Intent(Settings.ACTION_INPUT_METHOD_SETTINGS));
        } else if (position == 2) {
          InputMethodManager imManager = (InputMethodManager) getSystemService(Context.INPUT_METHOD_SERVICE);
          imManager.showInputMethodPicker();
        } else if (position == 3) {
          Intent i = new Intent(context, InfoActivity.class);
          i.addFlags(Intent.FLAG_ACTIVITY_CLEAR_WHEN_TASK_RESET);
          startActivity(i);
          overridePendingTransition(android.R.anim.fade_in, R.anim.hold);
        }
      }
    });
  }

  @Override
  protected void onResume() {
    super.onResume();
  }

  @Override
  protected void onPause() {
    super.onPause();
  }

  @Override
  public void onWindowFocusChanged(boolean hasFocus) {
    super.onWindowFocusChanged(hasFocus);
    if (hasFocus) {
      String checkbox_off = String.valueOf(android.R.drawable.checkbox_off_background);
      String checkbox_on = String.valueOf(android.R.drawable.checkbox_on_background);

      ArrayList<HashMap<String, String>> kbList = KMManager.getKeyboardsList(this);
      if (kbList != null && kbList.size() > 1) {
        list.get(0).put(iconKey, checkbox_on);
      } else {
        list.get(0).put(iconKey, checkbox_off);
      }

      if (isEnabledAsSystemKB(this)) {
        list.get(1).put(iconKey, checkbox_on);
        list.get(2).put(isEnabledKey, "true");
      } else {
        list.get(1).put(iconKey, checkbox_off);
        list.get(2).put(isEnabledKey, "false");
      }

      if (isDefaultKB(this)) {
        list.get(2).put(iconKey, checkbox_on);
      } else {
        list.get(2).put(iconKey, checkbox_off);
      }

      String[] from = new String[]{iconKey, textKey};
      int[] to = new int[]{R.id.left_icon, R.id.text};
      listAdapter = new KMListAdapter(this, list, R.layout.get_started_row_layout, from, to);
      listView.setAdapter(listAdapter);
    }
  }

  protected static boolean isEnabledAsSystemKB(Context context) {
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

  protected static boolean isDefaultKB(Context context) {
    String inputMethod = Settings.Secure.getString(context.getContentResolver(), Settings.Secure.DEFAULT_INPUT_METHOD);
    return inputMethod.equals("com.tavultesoft.kmapro/com.keyman.android.SystemKeyboard");
  }
}