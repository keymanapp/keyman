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

import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;
import androidx.preference.PreferenceManager;

import com.keyman.engine.DisplayLanguages;

import java.util.ArrayList;
import java.util.HashMap;

public class KeymanSettingsLocalizeActivity extends AppCompatActivity {
  private static final String TAG = "SettingsLocalizeActivity";
  private static ArrayList<HashMap<String, String>> localizeOptionList = null;
  private static Typeface titleFont = null;
  private static final String titleKey = "title";
  private static final String subtitleKey = "subtitle";
  private static final String iconKey = "icon";
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

    final ListView listView = findViewById(com.keyman.engine.R.id.listView);

    Bundle bundle = getIntent().getExtras();

    final TextView textView = findViewById(R.id.bar_title);
    textView.setText(getString(R.string.change_display_language));
    if (titleFont != null) {
      textView.setTypeface(titleFont, Typeface.BOLD);
    }

    localizeOptionList = new ArrayList<>();

    final String noIcon = "0";
    for(DisplayLanguages.DisplayLanguageType l: DisplayLanguages.getDisplayLanguages(context)) {
      HashMap<String, String> hashMap = new HashMap<>();
      hashMap.put(titleKey, l.getLanguageName());
      hashMap.put(iconKey, noIcon);
      localizeOptionList.add(hashMap);
    }

    String[] from = new String[]{titleKey, subtitleKey, iconKey};
    int[] to = new int[]{com.keyman.engine.R.id.text1, com.keyman.engine.R.id.image1};

    ListAdapter adapter = new SimpleAdapter(context, localizeOptionList, com.keyman.engine.R.layout.list_row_layout1, from, to) {
    };
    listView.setAdapter(adapter);
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {

      @Override
      public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        // Store the BCP-47 language tag in shared preference
        if (position >= 0 && position < DisplayLanguages.getDisplayLanguages(context).length) {

          DisplayLanguages.DisplayLanguageType l = DisplayLanguages.getDisplayLanguages(context)[position];
          String languageTag = l.getLanguageTag();

          SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(context);
          SharedPreferences.Editor editor = prefs.edit();
          editor.putString(DisplayLanguages.displayLanguageKey, languageTag);
          editor.commit();

          // Restart the app to use selected locale
          Intent intent = new Intent(context, MainActivity.class);
          intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
          startActivity(intent);
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