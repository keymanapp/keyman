/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardDownloadEventListener;
import com.tavultesoft.kmea.util.MapCompat;

import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

// Public access is necessary to avoid IllegalAccessException
public final class KeyboardListActivity extends AppCompatActivity implements OnKeyboardDownloadEventListener {

  private static Toolbar toolbar = null;
  private static ListView listView = null;
  private static JSONArray languages = LanguageListActivity.languages();
  private static JSONArray keyboards = null;
  private static ArrayList<HashMap<String, String>> keyboardsArrayList = null;
  private static int langIndex = 0;
  private final String iconKey = "icon";

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    supportRequestWindowFeature(Window.FEATURE_NO_TITLE);
    final Context context = this;

    setContentView(R.layout.activity_list_layout);
    toolbar = (Toolbar) findViewById(R.id.list_toolbar);
    setSupportActionBar(toolbar);
    getSupportActionBar().setDisplayHomeAsUpEnabled(true);
    getSupportActionBar().setDisplayShowHomeEnabled(true);
    getSupportActionBar().setDisplayShowTitleEnabled(false);

    listView = (ListView) findViewById(R.id.listView);

    final TextView textView = (TextView) findViewById(R.id.bar_title);
    textView.setText(getString(R.string.title_add_keyboard));

    try {
      langIndex = getIntent().getIntExtra("selectedIndex", 0);
      JSONObject language = languages.getJSONObject(langIndex);
      final String langID = language.getString(KMManager.KMKey_ID);
      final String langName = language.getString(KMManager.KMKey_Name);
      textView.setText(langName);

      keyboards = language.getJSONArray(KMKeyboardDownloaderActivity.KMKey_LanguageKeyboards);
      keyboardsArrayList = new ArrayList<HashMap<String, String>>();

      int length = keyboards.length();
      for (int i = 0; i < length; i++) {
        String kbID = keyboards.getJSONObject(i).getString(KMManager.KMKey_ID);
        String kbName = keyboards.getJSONObject(i).getString(KMManager.KMKey_Name);
        String isEnabled = "true";
        String icon = "0";
        String kbKey = String.format("%s_%s", langID, kbID);
        if (KeyboardPickerActivity.containsKeyboard(context, kbKey)) {
          isEnabled = "false";
          icon = String.valueOf(R.drawable.ic_check);
        }

        HashMap<String, String> hashMap = new HashMap<String, String>();
        hashMap.put(KMManager.KMKey_KeyboardName, kbName);
        hashMap.put(iconKey, icon);
        hashMap.put("isEnabled", isEnabled);
        keyboardsArrayList.add(hashMap);
      }

      String[] from = new String[]{KMManager.KMKey_KeyboardName, iconKey};
      int[] to = new int[]{R.id.text1, R.id.image1};
      ListAdapter adapter = new KMListAdapter(context, keyboardsArrayList, R.layout.list_row_layout1, from, to);
      listView.setAdapter(adapter);
      listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
        @Override
        public void onItemClick(AdapterView<?> parent, View view, final int position, long id) {
          HashMap<String, String> kbInfo = LanguageListActivity.getKeyboardInfo(langIndex, position);
          final String pkgID = MapCompat.getOrDefault(kbInfo, KMManager.KMKey_PackageID, KMManager.KMDefault_UndefinedPackageID);
          final String kbID = kbInfo.get(KMManager.KMKey_KeyboardID);
          final String langID = kbInfo.get(KMManager.KMKey_LanguageID);
          String kbName = kbInfo.get(KMManager.KMKey_KeyboardName);
          String langName = kbInfo.get(KMManager.KMKey_LanguageName);
          String kFont = MapCompat.getOrDefault(kbInfo, KMManager.KMKey_Font, "");
          String kOskFont = MapCompat.getOrDefault(kbInfo, KMManager.KMKey_OskFont, kFont);
          String isCustom = MapCompat.getOrDefault(kbInfo, KMManager.KMKey_CustomKeyboard, "N");

          if (!pkgID.equals(KMManager.KMDefault_UndefinedPackageID)) {
            // keyboard already exists in packages/ so just add the language association
            KeyboardPickerActivity.addKeyboard(context, kbInfo);
            KMManager.setKeyboard(pkgID, kbID, langID, kbName, langName, kFont, kOskFont);
            Toast.makeText(context, "Keyboard installed", Toast.LENGTH_SHORT).show();
            // Setting result to 1 so calling activity will finish too
            setResult(1);
            ((AppCompatActivity) context).finish();
            return;
          }

          Bundle args = new Bundle();
          args.putString(KMKeyboardDownloaderActivity.ARG_PKG_ID, pkgID);
          args.putString(KMKeyboardDownloaderActivity.ARG_KB_ID, kbID);
          args.putString(KMKeyboardDownloaderActivity.ARG_LANG_ID, langID);
          args.putString(KMKeyboardDownloaderActivity.ARG_KB_NAME, kbName);
          args.putString(KMKeyboardDownloaderActivity.ARG_LANG_NAME, langName);
          args.putBoolean(KMKeyboardDownloaderActivity.ARG_IS_CUSTOM, isCustom.toUpperCase().equals("Y"));
          Intent i = new Intent(getApplicationContext(), KMKeyboardDownloaderActivity.class);
          i.putExtras(args);
          startActivity(i);

        }
      });
    } catch (JSONException e) {
      Log.e("JSON Error", (e.getMessage() == null) ? "JSONException" : e.getMessage());
    }
  }

  @Override
  protected void onResume() {
    super.onResume();
    KMKeyboardDownloaderActivity.addKeyboardDownloadEventListener(this);
  }

  @Override
  protected void onPause() {
    super.onPause();

    // Intentionally not removing KeyboardDownloadEventListener to
    // ensure onKeyboardDownloadFinished() gets called
  }

  @Override
  public boolean onSupportNavigateUp() {
    onBackPressed();
    return true;
  }

  @Override
  public void onBackPressed() {
    finish();
  }

  @Override
  public void onKeyboardDownloadStarted(HashMap<String, String> keyboardInfo) {
    // Do nothing
  }

  @Override
  public void onKeyboardDownloadFinished(HashMap<String, String> keyboardInfo, int result) {
    if (result > 0) {
      String packageID = keyboardInfo.get(KMManager.KMKey_PackageID);
      String keyboardID = keyboardInfo.get(KMManager.KMKey_KeyboardID);
      String languageID = keyboardInfo.get(KMManager.KMKey_LanguageID);
      String keyboardName = keyboardInfo.get(KMManager.KMKey_KeyboardName);
      String languageName = keyboardInfo.get(KMManager.KMKey_LanguageName);
      String kFont = keyboardInfo.get(KMManager.KMKey_Font);
      String kOskFont = keyboardInfo.get(KMManager.KMKey_OskFont);

      KeyboardPickerActivity.addKeyboard(this, keyboardInfo);
      KMManager.setKeyboard(packageID, keyboardID, languageID, keyboardName, languageName, kFont, kOskFont);
    }
    finish();
  }

  @Override
  public void onPackageInstalled(List<Map<String, String>> keyboardsInstalled) {
    // Do nothing.
  }

  @Override
  public void onLexicalModelInstalled(List<Map<String, String>> lexicalModelsInstalled) {
    // Do nothing.
  }
}
