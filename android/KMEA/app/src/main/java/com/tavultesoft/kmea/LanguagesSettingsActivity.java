/**
 * Copyright (C) 2019 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import android.content.Context;
import android.graphics.Typeface;
import android.inputmethodservice.Keyboard;
import android.os.Bundle;
import android.view.View;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.widget.TextView;

import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Keyman Settings --> Languages Settings
 * Displays a list of installed languages and a count of their associated installed keyboards.
 */
public final class LanguagesSettingsActivity extends AppCompatActivity {

  private Context context;
  private static Toolbar toolbar = null;
  private static ListView listView = null;
  private static KMListAdapter listAdapter = null;
  private static ArrayList<HashMap<String, String>> languagesList = null;
  private boolean didExecuteParser = false;

  protected static Typeface listFont = null;

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    supportRequestWindowFeature(Window.FEATURE_NO_TITLE);
    context = this;
    setContentView(R.layout.activity_list_layout);

    toolbar = (Toolbar) findViewById(R.id.list_toolbar);
    setSupportActionBar(toolbar);
    getSupportActionBar().setDisplayHomeAsUpEnabled(true);
    getSupportActionBar().setDisplayShowHomeEnabled(true);
    getSupportActionBar().setDisplayShowTitleEnabled(false);
    TextView textView = (TextView) findViewById(R.id.bar_title);
    textView.setText(getString(R.string.title_languages_settings));

    listView = (ListView) findViewById(R.id.listView);
    listView.setFastScrollEnabled(true);

    languagesList = getLanguagesList(context);

    String[] from = new String[]{KMManager.KMKey_LanguageName, KMManager.KMKey_KeyboardCount};
    int[] to = new int[]{R.id.text1, R.id.text2};
    listAdapter = new KMListAdapter(context, languagesList, R.layout.list_row_layout2, from, to);
    //listAdapter.listFont = listFont;
    listView.setAdapter(listAdapter);
    listView.setChoiceMode(ListView.CHOICE_MODE_SINGLE);
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
      @Override
      public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
      }
    });
  }

  @Override
  public void onResume() {
    super.onResume();
  }

  @Override
  public void onPause() {
    super.onPause();
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

  /**
   * Convert from HashMap of installed keyboards list into a mapping of
   * installed languages and their counts of associated keyboards
   * @param context
   * @return ArrayList<HashMap<String, Integer>>
   */
  public static ArrayList<HashMap<String, String>> getLanguagesList(Context context) {
    ArrayList<HashMap<String, String>> keyboardsList = KeyboardPickerActivity.getKeyboardsList(context);
    ArrayList<HashMap<String, String>> list = new ArrayList<HashMap<String, String>>();
    final String oneKeyboard = "(1 keyboard)";
    final String twoKeyboards = "(2 keyboards)";

    for(HashMap<String, String> keyboardInfo : keyboardsList) {
      String languageID = keyboardInfo.get(KMManager.KMKey_LanguageID);
      String languageName = keyboardInfo.get(KMManager.KMKey_LanguageName);

      // Search if list has a matching language name with current keyboardInfo
      int languageIndex = -1;
      for (HashMap<String, String> languageInfo : list) {
        if (languageInfo.get(KMManager.KMKey_LanguageID).equalsIgnoreCase(languageID)) {
          languageIndex = list.indexOf(languageInfo);
          break;
        }
      }

      // Increment an existing language count
      if (languageIndex != -1) {
        HashMap<String, String> matchingLanguageInfoInfo = list.get(languageIndex);
        String keyboardCount = matchingLanguageInfoInfo.get(KMManager.KMKey_KeyboardCount);
        if (keyboardCount.equalsIgnoreCase(oneKeyboard)) {
          matchingLanguageInfoInfo.put(KMManager.KMKey_KeyboardCount, twoKeyboards);
        } else {
          int index = keyboardCount.indexOf(" ");
          Integer currentCount = Integer.parseInt(keyboardCount.substring(1, index));
          matchingLanguageInfoInfo.put(KMManager.KMKey_KeyboardCount, String.format("(%d keyboards)", currentCount + 1));
        }
        list.set(languageIndex, matchingLanguageInfoInfo);
      } else {
        // Otherwise, add new entry of language name and count of 1
        HashMap<String, String> lgInfo = new HashMap<String, String>();
        lgInfo.put(KMManager.KMKey_LanguageID, languageID);
        lgInfo.put(KMManager.KMKey_LanguageName, languageName);
        lgInfo.put(KMManager.KMKey_KeyboardCount, oneKeyboard);
        lgInfo.put("icon", String.valueOf(R.drawable.ic_arrow_forward));
        lgInfo.put("isEnabled", "true");
        list.add(lgInfo);
      }
    }
    return list;
  }
}