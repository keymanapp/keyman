/**
 * Copyright (C) 2019 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import android.content.Context;
import android.content.Intent;
import android.graphics.Typeface;
import android.inputmethodservice.Keyboard;
import android.os.Bundle;
import android.view.View;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.ImageButton;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.widget.TextView;

import androidx.appcompat.app.AlertDialog;
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
  private static ImageButton addButton = null;
  private static ArrayList<HashMap<String, String>> languagesList = null;
  private static ArrayList<HashMap<String, String>> associatedKeyboardsList = null;

  private boolean dismissOnSelect = true;
  protected static boolean canAddNewKeyboard = true;

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    supportRequestWindowFeature(Window.FEATURE_NO_TITLE);
    context = this;
    setContentView(R.layout.languages_settings_list_layout);

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

    String[] from = new String[]{KMManager.KMKey_LanguageName, KMManager.KMKey_KeyboardCount, KMManager.KMKey_Icon};
    int[] to = new int[]{R.id.text1, R.id.text2, R.id.image1};
    ListAdapter listAdapter = new KMListAdapter(context, languagesList, R.layout.list_row_layout2, from, to);
    listView.setAdapter(listAdapter);
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
      @Override
      public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        listView.setItemChecked(position, true);
        listView.setSelection(position);

        HashMap<String, String> languageInfo = languagesList.get(position);
        String langId = languageInfo.get(KMManager.KMKey_LanguageID);
        String langName = languageInfo.get(KMManager.KMKey_LanguageName);
        associatedKeyboardsList = KeyboardPickerActivity.getAssociatedKeyboards(langId);

        HashMap<String, String> associatedLexicalModel = KeyboardPickerActivity.getAssociatedLexicalModel(context, langId);

        Bundle args = new Bundle();
        args.putString(KMManager.KMKey_LanguageID, langId);
        args.putString(KMManager.KMKey_LanguageName, langName);
        if (associatedLexicalModel != null) {
          args.putString(KMManager.KMKey_LexicalModelName, associatedLexicalModel.get(KMManager.KMKey_LexicalModelName));
        }
        args.putSerializable("associatedKeyboards", associatedKeyboardsList);
        // TODO: Start intent for "Language Settings" activity with the selected languageID
        //Intent intent = new Intent(context, LanguageSettingsActivity.class);
        // intent.putExtra(args);

        if (dismissOnSelect)
          finish();
      }
    });

    addButton = (ImageButton) findViewById(R.id.add_button);
    addButton.setOnClickListener(new View.OnClickListener() {
      public void onClick(View v) {
        // Check that available keyboard information can be obtained via:
        // 1. connection to cloud catalog
        // 2. cached file
        // 3. local kmp.json files in packages/
        if (KMManager.hasConnection(context) || LanguageListActivity.getCacheFile(context).exists() ||
          KeyboardPickerActivity.hasKeyboardFromPackage()){
          dismissOnSelect = false;
          Intent i = new Intent(context, LanguageListActivity.class);
          i.addFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);
          context.startActivity(i);
        } else {
          AlertDialog.Builder dialogBuilder = new AlertDialog.Builder(context);
          dialogBuilder.setTitle(getString(R.string.title_add_keyboard));
          dialogBuilder.setMessage(String.format("\n%s\n", getString(R.string.cannot_connect)));
          dialogBuilder.setPositiveButton(getString(R.string.label_ok), null);
          AlertDialog dialog = dialogBuilder.create();
          dialog.show();
        }
      }
    });
    if (!canAddNewKeyboard) {
      addButton.setVisibility(View.GONE);
    }
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
        lgInfo.put(KMManager.KMKey_Icon, String.valueOf(R.drawable.ic_arrow_forward));
        lgInfo.put("isEnabled", "true");
        list.add(lgInfo);
      }
    }
    return list;
  }
}