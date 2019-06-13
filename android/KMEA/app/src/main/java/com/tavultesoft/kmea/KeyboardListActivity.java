/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardDownloadEventListener;
import com.tavultesoft.kmea.data.CloudRepository;
import com.tavultesoft.kmea.data.Dataset;
import com.tavultesoft.kmea.data.Keyboard;
import com.tavultesoft.kmea.data.adapters.NestedAdapter;
import com.tavultesoft.kmea.util.MapCompat;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

// Public access is necessary to avoid IllegalAccessException
public final class KeyboardListActivity extends AppCompatActivity implements OnKeyboardDownloadEventListener {

  private static Toolbar toolbar = null;
  private static ListView listView = null;

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

    String langID = getIntent().getStringExtra("languageCode");
    String langName = getIntent().getStringExtra("languageName");

    Dataset repo = CloudRepository.shared.fetchDataset(this, null, null);
    FilteredKeyboardsAdapter adapter = new FilteredKeyboardsAdapter(this, repo, langID);

    textView.setText(langName);

    listView.setAdapter(adapter);
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
      @Override
      public void onItemClick(AdapterView<?> parent, View view, final int position, long id) {
        FilteredKeyboardsAdapter adapter = (FilteredKeyboardsAdapter) listView.getAdapter();

        HashMap<String, String> kbInfo = new HashMap<>(adapter.getItem(position).map);
        final String pkgID = MapCompat.getOrDefault(new HashMap<>(kbInfo), KMManager.KMKey_PackageID, KMManager.KMDefault_UndefinedPackageID);
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

  // Fully details the building of this Activity's list view items.
  static private class FilteredKeyboardsAdapter extends NestedAdapter<Keyboard, Dataset.Keyboards, String> {
    static final int RESOURCE = R.layout.list_row_layout1;
    private final Context context;

    public FilteredKeyboardsAdapter(@NonNull Context context, final Dataset repo, final String languageCode) {
      // Goal:  to not need a custom filter here, instead relying on LanguageDataset's built-in filters.
      super(context, RESOURCE, repo.keyboards, repo.keyboardFilter, languageCode);

      this.context = context;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
      Keyboard kbd = this.getItem(position);

      // If we're being told to reuse an existing view, do that.  It's automatic optimization.
      if (convertView == null) {
        convertView = LayoutInflater.from(getContext()).inflate(RESOURCE, parent, false);
      }

      View view = convertView;

      ImageView img1 = view.findViewById(R.id.image1);
      TextView text1 = view.findViewById(R.id.text1);
      text1.setText(kbd.map.get(KMManager.KMKey_KeyboardName));

      if (!this.isEnabled(position)) {
        view.setAlpha(0.25f);
        img1.setImageResource(R.drawable.ic_check);
      } else {
        view.setAlpha(1.0f);
        img1.setImageResource(0);
      }

      return view;
    }

    @Override
    public boolean isEnabled(int position) {
      Keyboard kbd = this.getItem(position);

      final String langID = kbd.map.get(KMManager.KMKey_LanguageID); // Has the selected language code.
      String kbID = kbd.map.get(KMManager.KMKey_KeyboardID);

      String kbKey = String.format("%s_%s", langID, kbID);
      return !KeyboardPickerActivity.containsKeyboard(context, kbKey);
    }
  }
}
