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
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;

import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.data.Keyboard;
import com.tavultesoft.kmea.data.KeyboardController;
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
  private static KMListAdapter adapter = null;
  private static Typeface titleFont = null;
  private static final String titleKey = "title";
  private static final String subtitleKey = "subtitle";
  private static final String iconKey = "icon";
  private static final String isEnabledKey = "isEnabled";
  private static Context context;
  private static final boolean excludeInstalledLanguages = false;

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
    String title_install = String.format(getString(R.string.title_select_language_for_package), keyboardName);
    String title_no_install = getString(R.string.all_languages_installed);
    final TextView textView = findViewById(R.id.bar_title);
    textView.setText(title_no_install);
    if (titleFont != null) {
      textView.setTypeface(titleFont, Typeface.BOLD);
    }

    // Get the list of available Keyboards from kmp.json
    File resourceRoot =  new File(context.getDir("data", Context.MODE_PRIVATE).toString() + File.separator);
    PackageProcessor kmpProcessor =  new PackageProcessor(resourceRoot);
    List<Keyboard> availableKeyboardsList = kmpProcessor.getKeyboardList(
      packageID, keyboardID, excludeInstalledLanguages);

    final String noIcon = "0";
    list = new ArrayList<HashMap<String, String>>();
    for (Keyboard k : availableKeyboardsList) {
      HashMap<String, String> hashMap = new HashMap<>();
      hashMap.put(titleKey, k.getLanguageName());
      hashMap.put(subtitleKey, k.getLanguageID());
      String enable = "true";
      String icon = String.valueOf(R.drawable.ic_arrow_forward);
      if (!excludeInstalledLanguages && KeyboardController.getInstance().keyboardExists(
          k.getPackageID(), k.getKeyboardID(), k.getLanguageID())) {
        icon = String.valueOf(R.drawable.ic_check);
        enable = "false";
      } else {
        // Update title
        textView.setText(title_install);
      }
      hashMap.put(iconKey, icon);
      hashMap.put(isEnabledKey, enable);
      list.add(hashMap);
    }

    String[] from = new String[]{titleKey, subtitleKey, iconKey};
    int[] to = new int[]{R.id.text1, R.id.text2, com.tavultesoft.kmea.R.id.image1};

    adapter = new KMListAdapter(context, list, R.layout.list_row_layout2, from, to);
    listView.setAdapter(adapter);
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {

      @Override
      public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        HashMap<String, String> hashMap = (HashMap<String, String>) parent.getItemAtPosition(position);
        Keyboard k = availableKeyboardsList.get(position);
        KMManager.addKeyboard(context, k);
        String confirmation = String.format(getString(R.string.added_language_to_keyboard),
          k.getLanguageName(), k.getKeyboardName());
        Toast.makeText(context, confirmation, Toast.LENGTH_LONG).show();
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
