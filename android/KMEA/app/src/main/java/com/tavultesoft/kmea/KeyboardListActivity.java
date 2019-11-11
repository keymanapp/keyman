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
  private static final String TAG = "KeyboardListActivity";

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

    Dataset repo = CloudRepository.shared.fetchDataset(this);
    final FilteredKeyboardsAdapter adapter = new FilteredKeyboardsAdapter(this, repo, langID);

    textView.setText(langName);

    listView.setAdapter(adapter);
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
      @Override
      public void onItemClick(AdapterView<?> parent, View view, final int position, long id) {
        Keyboard kbd = adapter.getItem(position);
        if(kbd == null) {
          Log.e(TAG, "Could not find keyboard corresponding to item click position " + position);
          return;
        }
        HashMap<String, String> kbInfo = new HashMap<>(kbd.map);
        final String pkgID = MapCompat.getOrDefault(kbInfo, KMManager.KMKey_PackageID, KMManager.KMDefault_UndefinedPackageID);
        final String kbID = kbInfo.get(KMManager.KMKey_KeyboardID);
        final String lgID = kbInfo.get(KMManager.KMKey_LanguageID);
        String kbName = kbInfo.get(KMManager.KMKey_KeyboardName);
        String lgName = kbInfo.get(KMManager.KMKey_LanguageName);
        String kFont = MapCompat.getOrDefault(kbInfo, KMManager.KMKey_Font, "");
        String kOskFont = MapCompat.getOrDefault(kbInfo, KMManager.KMKey_OskFont, kFont);

        if (!pkgID.equals(KMManager.KMDefault_UndefinedPackageID)) {
          // keyboard already exists in packages/ so just add the language association
          KeyboardPickerActivity.addKeyboard(context, kbInfo);
          KMManager.setKeyboard(pkgID, kbID, lgID, kbName, lgName, kFont, kOskFont);
          Toast.makeText(context, "Keyboard installed", Toast.LENGTH_SHORT).show();
          // Setting result to 1 so calling activity will finish too
          setResult(1);
          ((AppCompatActivity) context).finish();
          return;
        }

        Bundle args = kbd.buildDownloadBundle();
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

    private static class ViewHolder {
      ImageView img;
      TextView text;
    }

    public FilteredKeyboardsAdapter(@NonNull Context context, final Dataset repo, final String languageCode) {
      // Goal:  to not need a custom filter here, instead relying on LanguageDataset's built-in filters.
      super(context, RESOURCE, repo.keyboards, repo.keyboardFilter, languageCode);

      this.context = context;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
      Keyboard kbd = this.getItem(position);
      ViewHolder holder;

      // If we're being told to reuse an existing view, do that.  It's automatic optimization.
      if (convertView == null) {
        convertView = LayoutInflater.from(getContext()).inflate(RESOURCE, parent, false);
        holder = new ViewHolder();

        holder.img = convertView.findViewById(R.id.image1);
        holder.text = convertView.findViewById(R.id.text1);
        convertView.setTag(holder);
      } else {
        holder = (ViewHolder) convertView.getTag();
      }

      holder.text.setText(kbd.map.get(KMManager.KMKey_KeyboardName));

      if (!this.isEnabled(position)) {
        convertView.setAlpha(0.25f);
        holder.img.setImageResource(R.drawable.ic_check);
      } else {
        convertView.setAlpha(1.0f);
        holder.img.setImageResource(0);
      }

      return convertView;
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
