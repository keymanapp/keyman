/**
 * Copyright (C) 2019 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import android.content.Context;
import android.content.Intent;
import android.graphics.Typeface;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.ImageButton;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;

import com.tavultesoft.kmea.data.CloudRepository;
import com.tavultesoft.kmea.data.Dataset;
import com.tavultesoft.kmea.data.Keyboard;
import com.tavultesoft.kmea.data.adapters.NestedAdapter;
import com.tavultesoft.kmea.util.MapCompat;

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
  private String associatedLexicalModel = "";
  private String lgCode;
  private String lgName;

  private final static String TAG = "LanguageSettingsAct";

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
    FilteredKeyboardsAdapter adapter = null;
    if (bundle != null) {
      associatedLexicalModel = bundle.getString(KMManager.KMKey_LexicalModelName, "");
      lgCode = bundle.getString(KMManager.KMKey_LanguageID);
      lgName = bundle.getString(KMManager.KMKey_LanguageName);

      adapter = new FilteredKeyboardsAdapter(context, KeyboardPickerActivity.getInstalledDataset(context), lgCode);
    } else {
      // Should never actually happen.
      Log.v(TAG, "Language data not specified for LanguageSettingsActivity!");
      finish();
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
    layout.setOnClickListener(new View.OnClickListener() {
      @Override
      public void onClick(View v) {
        // Start ModelPickerActivity
        Bundle bundle = new Bundle();
        bundle.putString(KMManager.KMKey_LanguageID, lgCode);
        bundle.putString(KMManager.KMKey_LanguageName, lgName);
        Intent i = new Intent(context, ModelPickerActivity.class);
        i.addFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);
        i.putExtras(bundle);
        startActivity(i);
      }
    });

    /**
     * This is a placeholder for "Manage dictionary" settings
     *
     * layout = (RelativeLayout)findViewById(R.id.manage_dictionary);
     * textView = (TextView) layout.findViewById(R.id.text1);
     * textView.setText(getString(R.string.manage_dictionary));
     * imageView = (ImageView) layout.findViewById(R.id.image1);
     * imageView.setImageResource(R.drawable.ic_arrow_forward);
     */

//    String[] from = new String[]{KMManager.KMKey_KeyboardName, KMManager.KMKey_Icon};
//    int[] to = new int[]{R.id.text1, R.id.image1};
//    ListAdapter listAdapter = new KMListAdapter(context, associatedKeyboardList, R.layout.list_row_layout1, from, to);

    listView.setAdapter(adapter);
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
      @Override
      public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        listView.setItemChecked(position, true);
        listView.setSelection(position);
        Keyboard kbd = ((FilteredKeyboardsAdapter) listView.getAdapter()).getItem(position);
        HashMap<String, String> kbInfo = new HashMap<>(kbd.map);
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
        boolean isCustom = MapCompat.getOrDefault(kbInfo, KMManager.KMKey_CustomKeyboard, "N").equals("Y") ? true : false;
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
        if (KMManager.hasConnection(context) || CloudRepository.shared.hasCache(context) ||
          KeyboardPickerActivity.hasKeyboardFromPackage()){
          // Rework to use languuage-specific (KeyboardList) picker!
          Intent i = new Intent(context, KeyboardListActivity.class);
          i.addFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);
          i.putExtra("languageCode", lgCode);
          i.putExtra("languageName", lgName);
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

  // Fully details the building of this Activity's list view items.
  static private class FilteredKeyboardsAdapter extends NestedAdapter<com.tavultesoft.kmea.data.Keyboard, Dataset.Keyboards, String> {
    static final int RESOURCE = R.layout.list_row_layout1;
    private final Context context;

    public FilteredKeyboardsAdapter(@NonNull Context context, final Dataset storage, final String languageCode) {
      // Goal:  to not need a custom filter here, instead relying on LanguageDataset's built-in filters.
      super(context, RESOURCE, storage.keyboards, storage.keyboardFilter, languageCode);

      this.context = context;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
      com.tavultesoft.kmea.data.Keyboard kbd = this.getItem(position);

      // If we're being told to reuse an existing view, do that.  It's automatic optimization.
      if (convertView == null) {
        convertView = LayoutInflater.from(getContext()).inflate(RESOURCE, parent, false);
      }

      View view = convertView;

      ImageView img1 = view.findViewById(R.id.image1);
      TextView text1 = view.findViewById(R.id.text1);
      text1.setText(kbd.map.get(KMManager.KMKey_KeyboardName));
      img1.setImageResource(R.drawable.ic_arrow_forward);

      return view;
    }
  }
}