/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */

package com.tavultesoft.kmapro;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.graphics.Typeface;
import android.os.Bundle;
import android.view.View;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.Button;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;

import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.data.Keyboard;
import com.tavultesoft.kmea.data.KeyboardController;
import com.tavultesoft.kmea.packages.PackageProcessor;
import com.tavultesoft.kmea.util.KMLog;

import org.json.JSONObject;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Keyman Settings --> KeymanInstallActivity --> SelectPackageActivity --> SelectLanguageActivity
 * Also available during keyboard package installation that involves a language choice.
 * Displays a list of available language names for the user to add for a given installed packageID/keyboardID.
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
  private static String packageID = null;
  private ArrayList<String> languageList = null;

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    supportRequestWindowFeature(Window.FEATURE_NO_TITLE);
    context = this;

    setContentView(R.layout.activity_select_language);
    final Toolbar toolbar = findViewById(R.id.list_toolbar);
    setSupportActionBar(toolbar);
    getSupportActionBar().setDisplayHomeAsUpEnabled(true);
    getSupportActionBar().setDisplayShowHomeEnabled(true);
    getSupportActionBar().setDisplayShowTitleEnabled(false);
    languageList = new ArrayList<>();

    final ListView listView = findViewById(R.id.listView);
    listView.setChoiceMode(ListView.CHOICE_MODE_MULTIPLE);
    listView.setItemsCanFocus(false);
    listView.setFastScrollEnabled(true);

    Bundle bundle = getIntent().getExtras();
    if (bundle == null) {
      KMLog.LogError(TAG, "No bundle parameters");
      return;
    }

    Boolean tempPath = bundle.getBoolean("tempPath");
    File resourceRoot =  new File(context.getDir("data", Context.MODE_PRIVATE).toString() + File.separator);
    PackageProcessor kmpProcessor =  new PackageProcessor(resourceRoot);
    // Get the list of available Keyboards from the keyboard package kmp.json (could be temp path or installed path)
    File packagePath = (File)getIntent().getSerializableExtra("packagePath");
    packageID = bundle.getString("packageID");
    JSONObject pkgInfo = kmpProcessor.loadPackageInfo(packagePath);
    Keyboard keyboard = bundle.containsKey("keyboard") ? (Keyboard)bundle.getSerializable("keyboard") :
      kmpProcessor.getKeyboard(pkgInfo, packageID, 0);
    final String keyboardID = keyboard.getKeyboardID();
    final String keyboardName = keyboard.getKeyboardName();
    String title_install = String.format(getString(R.string.title_select_language_for_package), keyboardName);
    String title_no_install = getString(R.string.all_languages_installed);
    final TextView textView = findViewById(R.id.bar_title);
    textView.setText(title_no_install);
    if (titleFont != null) {
      textView.setTypeface(titleFont, Typeface.BOLD);
    }

    List<Keyboard> availableKeyboardsList = kmpProcessor.getKeyboardList(
      pkgInfo, packageID, keyboardID, tempPath, excludeInstalledLanguages);

    final String noIcon = "0";
    list = new ArrayList<HashMap<String, String>>();
    for (Keyboard k : availableKeyboardsList) {
      HashMap<String, String> hashMap = new HashMap<>();
      hashMap.put(titleKey, k.getLanguageName());
      // No need to display subtitleKey - k.getLanguageID()
      hashMap.put("packageID", packageID);
      String enable = "true";
      String icon = noIcon;
      if (!excludeInstalledLanguages && !tempPath && KeyboardController.getInstance().keyboardExists(
          k.getPackageID(), k.getKeyboardID(), k.getLanguageID())) {
        // If Activity is listing an installed keyboard package, mark installed keyboards with a check
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
        Keyboard k = availableKeyboardsList.get(position);
        if (tempPath) {
          // Add/Remove the selected language from languageList
          if (languageList == null) {
            languageList = new ArrayList<String>();
          }
          String languageID = k.getLanguageID();
          if (languageList.contains(languageID)) {
            languageList.remove(languageID);
          } else {
            languageList.add(languageID);
          }
        } else {
          // Otherwise, add the language association
          KMManager.addKeyboard(context, k);
          String confirmation = String.format(getString(R.string.added_language_to_keyboard),
            k.getLanguageName(), k.getKeyboardName());
          Toast.makeText(context, confirmation, Toast.LENGTH_LONG).show();
          finish();
        }
      }
    });

    // Initialize buttons
    final Button backButton = (Button) findViewById(R.id.backButton);
    final Button forwardButton = (Button) findViewById(R.id.forwardButton);
    if (!tempPath) {
      // When adding a language from the Settings menu, we don't need the bottom nav bar
      backButton.setVisibility(View.GONE);
      forwardButton.setVisibility(View.GONE);
    }

    backButton.setOnClickListener(new View.OnClickListener() {
      @Override
      public void onClick(View view) {
        finish();
      }
    });

    forwardButton.setOnClickListener(new View.OnClickListener() {
      @Override
      public void onClick(View view) {
        // Temporary path for kmp.json means return the selected languageList back to PackageActivity to install the keyboard package
        Intent intent = new Intent();
        intent.putExtra("pkgTarget", PackageProcessor.PP_TARGET_KEYBOARDS);
        intent.putExtra("packageID", packageID);
        //intent.putExtra("languageID", k.getLanguageID());
        Bundle bundle = new Bundle();
        bundle.putSerializable("languageList", languageList);
        intent.putExtras(bundle);
        setResult(2, intent);
       finish();
      }
    });
  }

  @Override
  public boolean onSupportNavigateUp() {
    super.onBackPressed();
    setResult(Activity.RESULT_CANCELED);
    return true;
  }
}
