/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */
package com.tavultesoft.kmapro;

import java.util.ArrayList;
import java.util.HashMap;

import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;

import android.content.Context;
import android.content.Intent;
import android.graphics.Typeface;
import android.net.Uri;
import android.os.Bundle;
import android.view.View;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.os.Bundle;
import android.widget.SimpleAdapter;
import android.widget.TextView;

import com.keyman.engine.KMManager;
import com.keyman.engine.data.KeyboardController;
import com.keyman.engine.util.MapCompat;

public class KeymanSettingsInstallActivity extends AppCompatActivity {
  private static final String TAG = "SettingsInstallActivity";
  private static ArrayList<HashMap<String, String>> installOptionList = null;
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

    final ListView listView = findViewById(com.keyman.engine.R.id.listView);

    Bundle bundle = getIntent().getExtras();

    final TextView textView = findViewById(R.id.bar_title);
    textView.setText(getString(R.string.install_keyboard_or_dictionary));
    if (titleFont != null) {
      textView.setTypeface(titleFont, Typeface.BOLD);
    }

    installOptionList = new ArrayList<>();

    final String noIcon = "0";
    HashMap<String, String> hashMap = new HashMap<>();
    hashMap.put(titleKey, getString(R.string.install_from_keyman_dot_com));
    String icon = String.valueOf(R.drawable.ic_content_add);
    hashMap.put(iconKey, icon);
    installOptionList.add(hashMap);

    hashMap = new HashMap<>();
    hashMap.put(titleKey, getString(R.string.install_from_local_file));
    icon = String.valueOf(R.drawable.ic_folder_open);
    hashMap.put(iconKey, icon);
    hashMap.put(isEnabledKey, "true");
    installOptionList.add(hashMap);

    // TODO: Install by scanning QR code
    /*
    hashMap = new HashMap<>();
    hashMap.put(titleKey, getString(R.string.install_from_other_device));
    icon = String.valueOf(R.drawable.ic_content_add);
    hashMap.put(iconKey, icon);
    hashMap.put(isEnabledKey, "false");
    installOptionList.add(hashMap);
    */

    hashMap = new HashMap<>();
    hashMap.put(titleKey, getString(R.string.add_languages_to_installed_keyboard));
    hashMap.put(subtitleKey, getString(R.string.add_language_subtext));
    icon = String.valueOf(R.drawable.ic_content_add);
    hashMap.put(iconKey, icon);
    hashMap.put(isEnabledKey, "true");
    installOptionList.add(hashMap);

    String[] from = new String[]{titleKey, subtitleKey, iconKey};
    int[] to = new int[]{com.keyman.engine.R.id.text1, com.keyman.engine.R.id.text2, com.keyman.engine.R.id.image1};

    ListAdapter adapter = new SimpleAdapter(context, installOptionList, com.keyman.engine.R.layout.list_row_layout2, from, to) {
      @Override
      public boolean isEnabled(int position) {
        HashMap<String, String> hashMap = installOptionList.get(position);
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
    };
    listView.setAdapter(adapter);
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {

      @Override
      public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        HashMap<String, String> hashMap = (HashMap<String, String>) parent.getItemAtPosition(position);
        String itemTitle = MapCompat.getOrDefault(hashMap, titleKey, "");

        // Install from keyman.com
        if (itemTitle.equals(getString(R.string.install_from_keyman_dot_com))) {
          if (KMManager.hasConnection(context)) {
            Intent i = new Intent(context, KMPBrowserActivity.class);
            i.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            i.addFlags(Intent.FLAG_ACTIVITY_SINGLE_TOP);
            context.startActivity(i);
          }  else {
            AlertDialog.Builder dialogBuilder = new AlertDialog.Builder(context);
            dialogBuilder.setTitle(getString(R.string.title_add_keyboard));
            dialogBuilder.setMessage(getString(R.string.cannot_connect));
            dialogBuilder.setPositiveButton(getString(R.string.label_ok), null);
            AlertDialog dialog = dialogBuilder.create();
            dialog.show();
          }
        // Install from local file
        } else if (itemTitle.equals(getString(R.string.install_from_local_file))) {
          Intent intent = new Intent(Intent.ACTION_OPEN_DOCUMENT);
          intent.addCategory(Intent.CATEGORY_OPENABLE);
          intent.putExtra("android.content.extra.SHOW_ADVANCED", false);
          // Unfortunately, we can't filter for a "kmp" mime type
          intent.setType("*/*");
          startActivityForResult(intent, MainActivity.READ_REQUEST_CODE);

        // Install by scanning QR code
        } else if (itemTitle.equals(getString(R.string.install_from_other_device))) {
          // TODO

        // Add language from keyboard package already installed
        } else if (itemTitle.equals(getString(R.string.add_languages_to_installed_keyboard))) {
          Intent intent = new Intent(context, SelectPackageActivity.class);
          context.startActivity(intent);
        }
      }
    });
  }

  @Override
  public boolean onSupportNavigateUp() {
    super.onBackPressed();
    return true;
  }

  @Override
  public void onActivityResult(int requestCode, int resultCode, Intent returnIntent) {
    super.onActivityResult(requestCode, resultCode, returnIntent);

    // Handle kmp file selected from file browser
    if ((requestCode == MainActivity.READ_REQUEST_CODE) && (returnIntent != null)) {
      String kmpFilename = returnIntent.getDataString();
      Uri data = Uri.parse(kmpFilename);
      MainActivity.useLocalKMP(context, data);
    }
  }

}
