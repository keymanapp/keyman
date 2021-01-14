/**
 * Copyright (C) 2021 SIL International. All rights reserved.
 */

package com.tavultesoft.kmapro;

import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.graphics.Typeface;
import android.os.Bundle;
import android.view.View;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.widget.SimpleAdapter;
import android.widget.TextView;

import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;
import androidx.preference.PreferenceManager;

import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.data.KeyboardController;
import com.tavultesoft.kmea.util.MapCompat;

import java.util.ArrayList;
import java.util.HashMap;

public class KeymanSettingsLocalizeActivity extends AppCompatActivity {
  private static final String TAG = "SettingsLocalizeActivity";
  private static ArrayList<HashMap<String, String>> localizeOptionList = null;
  private static Typeface titleFont = null;
  private static final String titleKey = "title";
  private static final String subtitleKey = "subtitle";
  private static final String iconKey = "icon";
  private final String isEnabledKey = "isEnabled";
  private static Context context;

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    supportRequestWindowFeature(Window.FEATURE_NO_TITLE);
    context = this;

    setContentView(R.layout.activity_list_layout);
    final Toolbar toolbar = findViewById(R.id.list_toolbar);
    setSupportActionBar(toolbar);
    getSupportActionBar().setDisplayHomeAsUpEnabled(true);
    getSupportActionBar().setDisplayShowHomeEnabled(true);
    getSupportActionBar().setDisplayShowTitleEnabled(false);

    final ListView listView = findViewById(com.tavultesoft.kmea.R.id.listView);

    Bundle bundle = getIntent().getExtras();

    final TextView textView = findViewById(R.id.bar_title);
    textView.setText(getString(R.string.change_display_language));
    if (titleFont != null) {
      textView.setTypeface(titleFont, Typeface.BOLD);
    }

    localizeOptionList = new ArrayList<>();

    final String noIcon = "0";
    for(DisplayLanguages.DisplayLanguageType l: DisplayLanguages.DisplayLanguages) {
      HashMap<String, String> hashMap = new HashMap<>();
      hashMap.put(titleKey, l.getLanguageName());
      String icon = String.valueOf(R.drawable.ic_content_add);
      hashMap.put(iconKey, icon);
      localizeOptionList.add(hashMap);
    }

    String[] from = new String[]{titleKey, subtitleKey, iconKey};
    int[] to = new int[]{com.tavultesoft.kmea.R.id.text1, com.tavultesoft.kmea.R.id.text2, com.tavultesoft.kmea.R.id.image1};

    ListAdapter adapter = new SimpleAdapter(context, localizeOptionList, com.tavultesoft.kmea.R.layout.list_row_layout2, from, to) {
      /*
      @Override
      public boolean isEnabled(int position) {
        HashMap<String, String> hashMap = localizeOptionList.get(position);
        String itemTitle = MapCompat.getOrDefault(hashMap, titleKey, "");
        String icon = MapCompat.getOrDefault(hashMap, iconKey, noIcon);
        if (itemTitle.equals(getString(R.string.install_from_other_device))) {
          // Scan QR code not implemented yet
          return false;
        } else if (itemTitle.equals(getString(R.string.add_languages_to_installed_keyboard))) {
          if (KeyboardController.getInstance().getInstalledPackagesList() == null) {
            // Disable if no keyboard packages installed
            return false;
          }
        }

        return super.isEnabled(position);
      }
       */
    };
    listView.setAdapter(adapter);
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {

      @Override
      public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        HashMap<String, String> hashMap = (HashMap<String, String>) parent.getItemAtPosition(position);

        // Store the BCP-47 language tag in shared preference
        if (position > 0 && position < DisplayLanguages.DisplayLanguages.length) {

          DisplayLanguages.DisplayLanguageType l = DisplayLanguages.DisplayLanguages[position];
          String languageTag = l.getLanguageTag();

          SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(context);
          SharedPreferences.Editor editor = prefs.edit();
          editor.putString(KeymanSettingsActivity.displayLanguageKey, languageTag);
          editor.commit();
          finish();
        }
      }
    });
  }

  @Override
  public boolean onSupportNavigateUp() {
    super.onBackPressed();
    return true;
  }
}