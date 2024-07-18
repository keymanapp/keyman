/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmapro;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import com.keyman.engine.BaseActivity;
import com.keyman.engine.KMManager;
import com.keyman.engine.data.Keyboard;

import android.os.Bundle;
import android.provider.Settings;
import android.view.View;
import android.view.Window;
import android.view.inputmethod.InputMethodInfo;
import android.view.inputmethod.InputMethodManager;
import android.widget.AdapterView;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.CompoundButton.OnCheckedChangeListener;
import android.widget.ImageButton;
import android.widget.ListView;
import androidx.appcompat.app.AppCompatActivity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.widget.TextView;

public class GetStartedActivity extends BaseActivity {

  private static ListView listView = null;
  private static ArrayList<HashMap<String, String>> list = null;
  private static KMListAdapter listAdapter = null;
  protected static final String showGetStartedKey = "ShowGetStarted";
  private final String iconKey = "icon";
  private final String textKey = "text";
  private final String isEnabledKey = "isEnabled";

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    supportRequestWindowFeature(Window.FEATURE_NO_TITLE);
    final Context context = this;
    setContentView(R.layout.get_started_list_layout);

    listView = (ListView) findViewById(R.id.listView);

    final ImageButton closeButton = (ImageButton) findViewById(R.id.close_button);
    closeButton.setOnClickListener(new View.OnClickListener() {
      public void onClick(View v) {
        finish();
      }
    });

    final SharedPreferences prefs = getSharedPreferences(getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    boolean showGetStarted = prefs.getBoolean(showGetStartedKey, true);

    final CheckBox checkBox = (CheckBox) findViewById(R.id.checkBox);
    checkBox.setChecked(showGetStarted);
    checkBox.setOnCheckedChangeListener(new OnCheckedChangeListener() {
      @Override
      public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
        // Save user preference on showing "Get Started"
        SharedPreferences.Editor editor = prefs.edit();
        editor.putBoolean(showGetStartedKey, isChecked);
        editor.commit();
      }
    });

    final TextView getStartedText = findViewById(R.id.getStartedText);
    getStartedText.setText(String.format(getString(R.string.show_get_started), getString(R.string.get_started)));
    getStartedText.setOnClickListener(new View.OnClickListener() {
      @Override
      public void onClick(View v) {
        checkBox.setChecked(!checkBox.isChecked());
      }
    });

    list = new ArrayList<HashMap<String, String>>();

    HashMap<String, String> hashMap = new HashMap<String, String>();
    hashMap.put(iconKey, "0");
    hashMap.put(textKey, getString(R.string.add_a_keyboard));
    hashMap.put(isEnabledKey, "true");
    list.add(hashMap);

    hashMap = new HashMap<String, String>();
    hashMap.put(iconKey, "0");
    hashMap.put(textKey, getString(R.string.enable_system_keyboard));
    hashMap.put(isEnabledKey, "true");
    list.add(hashMap);

    hashMap = new HashMap<String, String>();
    hashMap.put(iconKey, "0");
    hashMap.put(textKey, getString(R.string.set_keyman_as_default));
    hashMap.put(isEnabledKey, "false");
    list.add(hashMap);

    hashMap = new HashMap<String, String>();
    hashMap.put(iconKey, "0");
    hashMap.put(textKey, getString(R.string.more_info));
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
          // Keyman Settings install activity
          Intent i = new Intent(context, KeymanSettingsInstallActivity.class);
          context.startActivity(i);
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

        uncheckGetStartedIfComplete();
      }
    });
  }

  /**
   * Uncheck show "Get Started" on startup if:
   * User hasn't set a preference on showing "Get Started",
   * Keyman enabled as a system-wide keyboard, and
   * Keyman set as default keyboard
   */
  private void uncheckGetStartedIfComplete() {
    if (SystemIMESettings.isEnabledAsSystemKB(this) &&
        SystemIMESettings.isDefaultKB(this)) {

      final SharedPreferences prefs = getSharedPreferences(getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
      if (!prefs.contains(showGetStartedKey)) {
        // Everything is completed, so un-check "Get Started" on startup.
        // onCheckedChanged() will save the preference
        final CheckBox checkBox = (CheckBox) findViewById(R.id.checkBox);
        checkBox.setChecked(false);

      }
    }
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
      // Enumerated steps are replaced with checkboxes when the user completes a step
      String one = String.valueOf(R.drawable.ic_looks_one);
      String two = String.valueOf(R.drawable.ic_looks_two);
      String three = String.valueOf(R.drawable.ic_looks_three);
      String checkbox_on = String.valueOf(android.R.drawable.checkbox_on_background);
      String info = String.valueOf(R.drawable.ic_info_outline);

      List<Keyboard> kbList = KMManager.getKeyboardsList(this);
      if (kbList != null && kbList.size() > 1) {
        list.get(0).put(iconKey, checkbox_on);
      } else {
        list.get(0).put(iconKey, one);
      }

      if (SystemIMESettings.isEnabledAsSystemKB(this)) {
        list.get(1).put(iconKey, checkbox_on);
        list.get(2).put(isEnabledKey, "true");
      } else {
        list.get(1).put(iconKey, two);
        list.get(2).put(isEnabledKey, "false");
      }

      if (SystemIMESettings.isDefaultKB(this)) {
        list.get(2).put(iconKey, checkbox_on);
      } else {
        list.get(2).put(iconKey, three);
      }

      list.get(3).put(iconKey, info);

      String[] from = new String[]{iconKey, textKey};
      int[] to = new int[]{R.id.left_icon, R.id.text};
      listAdapter = new KMListAdapter(this, list, R.layout.get_started_row_layout, from, to);
      listView.setAdapter(listAdapter);

      uncheckGetStartedIfComplete();
    }
  }
}