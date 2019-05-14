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
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Keyman Settings --> Languages Settings --> Language Settings
 * Displays a list of installed keyboards and some lexical model switches.
 */
public final class LanguageSettingsActivity extends AppCompatActivity {
  private Context context;
  private static Toolbar toolbar = null;
  private static ListView listView = null;
  private ImageButton addButton = null;
  private static ArrayList<HashMap<String, String>> associatedKeyboardList = null;
  private final String titleKey = "title";
  private final String subtitleKey = "subtitle";
  private final String iconKey = "icon";
  private String associatedLexicalModel = "";
  private boolean dismissOnSelect = false;

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    supportRequestWindowFeature(Window.FEATURE_NO_TITLE);
    context = this;
    setContentView(R.layout.language_settings_list_layout);

    toolbar = (Toolbar) findViewById(R.id.list_toolbar);
    setSupportActionBar(toolbar);
    getSupportActionBar().setDisplayHomeAsUpEnabled(true);
    getSupportActionBar().setDisplayShowHomeEnabled(true);
    getSupportActionBar().setDisplayShowTitleEnabled(false);
    TextView textView = (TextView) findViewById(R.id.bar_title);
    textView.setText(getString(R.string.title_language_settings));

    listView = (ListView) findViewById(R.id.listView);
    listView.setFastScrollEnabled(true);

    Bundle bundle = getIntent().getExtras();
    if (bundle != null) {
      associatedKeyboardList = (ArrayList<HashMap<String, String>>) bundle.getSerializable("associatedKeyboards");
      associatedLexicalModel = bundle.getString(KMManager.KMKey_LexicalModelName, "");
    } else {
      associatedKeyboardList = new ArrayList<HashMap<String, String>>();
    }

    RelativeLayout layout = (RelativeLayout)findViewById(R.id.corrections_toggle);
    textView = (TextView) layout.findViewById(R.id.text1);
    textView.setText(getString(R.string.enable_corrections));

    layout = (RelativeLayout)findViewById(R.id.predictions_toggle);
    textView = (TextView) layout.findViewById(R.id.text1);
    textView.setText(getString(R.string.enable_predictions));

    layout = (RelativeLayout)findViewById(R.id.model_picker);
    textView = (TextView) layout.findViewById(R.id.text1);
    textView.setText(getString(R.string.model));
    if (!associatedLexicalModel.isEmpty()) {
      textView = (TextView) layout.findViewById(R.id.text2);
      textView.setText(associatedLexicalModel);
      textView.setEnabled(true);
    }
    ImageView imageView = (ImageView) layout.findViewById(R.id.image1);
    imageView.setImageResource(R.drawable.ic_arrow_forward);
    layout.setEnabled(true);

    /**
     * This is a placeholder for "Manage dictionary" settings
     *
     * layout = (RelativeLayout)findViewById(R.id.manage_dictionary);
     * textView = (TextView) layout.findViewById(R.id.text1);
     * textView.setText(getString(R.string.manage_dictionary));
     * imageView = (ImageView) layout.findViewById(R.id.image1);
     * imageView.setImageResource(R.drawable.ic_arrow_forward);
     */

    String[] from = new String[]{KMManager.KMKey_KeyboardName, KMManager.KMKey_Icon};
    int[] to = new int[]{R.id.text1, R.id.image1};
    ListAdapter listAdapter = new KMListAdapter(context, associatedKeyboardList, R.layout.list_row_layout1, from, to);

    listView.setAdapter(listAdapter);
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
      @Override
      public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        listView.setItemChecked(position, true);
        listView.setSelection(position);
        HashMap<String, String> kbInfo = associatedKeyboardList.get(position);
        String packageID = kbInfo.get(KMManager.KMKey_PackageID);
        String keyboardID = kbInfo.get(KMManager.KMKey_KeyboardID);
        if (packageID == null || packageID.isEmpty()) {
          packageID = KMManager.KMDefault_UndefinedPackageID;
        }
        Intent intent = new Intent(context, KeyboardSettingsActivity.class);
        intent.addFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);
        intent.putExtra(KMManager.KMKey_PackageID, packageID);
        intent.putExtra(KMManager.KMKey_KeyboardID, keyboardID);
        intent.putExtra(KMManager.KMKey_LanguageID, kbInfo.get(KMManager.KMKey_LanguageID));
        intent.putExtra(KMManager.KMKey_LanguageName, kbInfo.get(KMManager.KMKey_LanguageName));
        intent.putExtra(KMManager.KMKey_KeyboardName, kbInfo.get(KMManager.KMKey_KeyboardName));
        intent.putExtra(KMManager.KMKey_KeyboardVersion, KMManager.getLatestKeyboardFileVersion(context, packageID, keyboardID));
        boolean isCustom = kbInfo.get(KMManager.KMKey_CustomKeyboard).equals("Y") ? true : false;
        intent.putExtra(KMManager.KMKey_CustomKeyboard, isCustom);
        String customHelpLink = kbInfo.get(KMManager.KMKey_CustomHelpLink);
        if (customHelpLink != null)
          intent.putExtra(KMManager.KMKey_CustomHelpLink, customHelpLink);
        startActivity(intent);
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

}