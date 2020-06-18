/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */

package com.tavultesoft.kmapro;

import android.content.Context;
import android.graphics.Typeface;
import android.os.Bundle;
import android.view.View;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.widget.SimpleAdapter;
import android.widget.TextView;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;

import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.data.Keyboard;
import com.tavultesoft.kmea.packages.PackageProcessor;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Keyman Settings --> KeymanInstallActivity --> SelectPackageActivity --> SelectLanguageActivity
 * Displays a list of available languages for the user to add for a given installed packageID/keyboardID.
 */
public final class SelectLanguageActivity extends AppCompatActivity {
  private static final String TAG = "SelectLanguageActivity";
  private static ArrayList<HashMap<String, String>> list = null;
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

    final ListView listView = findViewById(R.id.listView);
    listView.setFastScrollEnabled(true);

    final Keyboard keyboard = (Keyboard)getIntent().getSerializableExtra("keyboard");
    final String packageID = keyboard.getPackageID();
    final String keyboardID = keyboard.getKeyboardID();
    final String keyboardName = keyboard.getKeyboardName();
    String title = String.format(getString(R.string.title_select_language_for_package), keyboardName);
    final TextView textView = findViewById(R.id.bar_title);
    textView.setText(title);
    if (titleFont != null) {
      textView.setTypeface(titleFont, Typeface.BOLD);
    }

    // Get the list of available languages from kmp.json
    File resourceRoot =  new File(context.getDir("data", Context.MODE_PRIVATE).toString() + File.separator);
    PackageProcessor kmpProcessor =  new PackageProcessor(resourceRoot);
    List<Keyboard> availableKeyboardsList = kmpProcessor.getLanguageList(packageID, keyboardID);

    list = new ArrayList<HashMap<String, String>>();
    for (Keyboard k : availableKeyboardsList) {
      final String noIcon = "0";
      HashMap<String, String> hashMap = new HashMap<>();
      hashMap.put(titleKey, k.getLanguageName());
      hashMap.put(subtitleKey, k.getLanguageID());
      String icon = String.valueOf(R.drawable.ic_arrow_forward);
      hashMap.put(iconKey, icon);
      list.add(hashMap);
    }

    String[] from = new String[]{titleKey, subtitleKey, iconKey};
    int[] to = new int[]{com.tavultesoft.kmea.R.id.text1, com.tavultesoft.kmea.R.id.text2, com.tavultesoft.kmea.R.id.image1};

    ListAdapter adapter = new SimpleAdapter(context, list, com.tavultesoft.kmea.R.layout.list_row_layout2, from, to) {
    };
    listView.setAdapter(adapter);
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {

      @Override
      public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        HashMap<String, String> hashMap = (HashMap<String, String>) parent.getItemAtPosition(position);
        Keyboard k = availableKeyboardsList.get(position);
        KMManager.addKeyboard(context, k);
        Toast.makeText(context, "Keyboard added", Toast.LENGTH_LONG).show();
        finish();
      }
    });
  }

  @Override
  public boolean onSupportNavigateUp() {
    super.onBackPressed();
    return true;
  }
}
