/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */

package com.tavultesoft.kmapro;

import android.content.Context;
import android.content.Intent;
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

import com.keyman.engine.KMManager;
import com.keyman.engine.data.Keyboard;
import com.keyman.engine.data.KeyboardController;
import com.keyman.engine.util.KMLog;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Keyman Settings --> KeymanInstallActivity --> SelectPackageActivity
 * Displays a list of installed package ID / keyboard IDs so the user can select a language from the kmp.json
 */
public final class SelectPackageActivity extends AppCompatActivity {
  private static final String TAG = "SelectPackageActivity";
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

    Bundle bundle = getIntent().getExtras();

    final TextView textView = findViewById(R.id.bar_title);
    textView.setText(getString(R.string.title_select_keyboard_package_list));
    if (titleFont != null) {
      textView.setTypeface(titleFont, Typeface.BOLD);
    }

    List<Keyboard> packagesList = KeyboardController.getInstance().getInstalledPackagesList();
    if (packagesList == null) {
      // Should never actually happen
      KMLog.LogError(TAG, "Installed keyboard package list is empty");
      finish();
    }

    list = new ArrayList<HashMap<String, String>>();
    for (Keyboard k : packagesList) {
      String keyboardName = k.getKeyboardName();
      String pkgID = k.getPackageID();
      final String noIcon = "0";
      HashMap<String, String> hashMap = new HashMap<>();
      hashMap.put(titleKey, keyboardName);
      hashMap.put(subtitleKey, pkgID);
      String icon = String.valueOf(R.drawable.ic_arrow_forward);
      hashMap.put(iconKey, icon);
      list.add(hashMap);
    }

    String[] from = new String[]{titleKey, subtitleKey, iconKey};
    int[] to = new int[]{com.keyman.engine.R.id.text1, com.keyman.engine.R.id.text2, com.keyman.engine.R.id.image1};

    ListAdapter adapter = new SimpleAdapter(context, list, com.keyman.engine.R.layout.list_row_layout2, from, to) {
    };
    listView.setAdapter(adapter);
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {

      @Override
      public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        HashMap<String, String> hashMap = (HashMap<String, String>) parent.getItemAtPosition(position);
        Bundle bundle = new Bundle();
        bundle.putSerializable("keyboard", packagesList.get(position));
        String pkgID = hashMap.get(subtitleKey);
        File packagePath = new File(KMManager.getPackagesDir(), pkgID);
        bundle.putSerializable("packagePath", packagePath);
        bundle.putString("packageID", pkgID);
        bundle.putBoolean("isInstallingPackage", false);
        Intent intent = new Intent(context, SelectLanguageActivity.class);
        intent.putExtras(bundle);
        context.startActivity(intent);
      }
    });
  }

  @Override
  public boolean onSupportNavigateUp() {
    super.onBackPressed();
    return true;
  }
}
