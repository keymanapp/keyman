/**
 * Copyright (C) 2019 SIL International. All rights reserved.
 */

package com.tavultesoft.kmapro;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.ImageButton;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;

import com.keyman.engine.data.Dataset;
import com.keyman.engine.data.adapters.AdapterFilter;
import com.keyman.engine.data.adapters.NestedAdapter;
import com.keyman.engine.KeyboardPickerActivity;
import com.keyman.engine.KMManager;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Keyman Settings --> Languages Settings
 * Displays a list of installed languages and a count of their associated installed keyboards.
 */
public final class LanguagesSettingsActivity extends AppCompatActivity {

  private Context context;
  private static Toolbar toolbar = null;
  private static ListView listView = null;
  private static ImageButton addButton = null;

  private boolean dismissOnSelect = false;
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

    Dataset storage = KeyboardPickerActivity.getInstalledDataset(this);
    LanguagesAdapter listAdapter = new LanguagesAdapter(this, storage);

    listView.setAdapter(listAdapter);
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
      @Override
      public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        LanguagesAdapter adapter = ((LanguagesAdapter) listView.getAdapter());

        Dataset.LanguageDataset languageData = adapter.getItem(position);
        String langId = languageData.code;
        String langName = languageData.name;

        HashMap<String, String> associatedLexicalModel = KMManager.getAssociatedLexicalModel(langId);

        Bundle args = new Bundle();
        args.putString(KMManager.KMKey_LanguageID, langId);
        args.putString(KMManager.KMKey_LanguageName, langName);

        if(associatedLexicalModel != null) {
          args.putString(KMManager.KMKey_LexicalModelName, associatedLexicalModel.get(KMManager.KMKey_LexicalModelName));
          args.putString(KMManager.KMKey_CustomHelpLink, associatedLexicalModel.get(KMManager.KMKey_CustomHelpLink));
        }

        Intent intent = new Intent(context, LanguageSettingsActivity.class);
        intent.putExtras(args);
        startActivity(intent);

        if (dismissOnSelect)
          finish();
      }
    });

    addButton = (ImageButton) findViewById(R.id.add_button);
    addButton.setOnClickListener(new View.OnClickListener() {
      public void onClick(View v) {
        // Check scenarios to add available keyboards:
        if (KMManager.hasConnection(context)) {
          // Scenario 1: Connection to keyman.com catalog
          Intent i = new Intent(context, KMPBrowserActivity.class);
          i.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
          i.addFlags(Intent.FLAG_ACTIVITY_SINGLE_TOP);
          context.startActivity(i);
        /*
        } else if (KeyboardPickerActivity.hasKeyboardFromPackage()) {
          // Scenario 2: Local kmp.json files in packages/
          // TODO: Cleanly re-implement this based on the languages available in each package
        */
        } else {
          AlertDialog.Builder dialogBuilder = new AlertDialog.Builder(context);
          dialogBuilder.setTitle(getString(R.string.title_add_keyboard));
          dialogBuilder.setMessage(getString(R.string.cannot_connect));
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


    KMManager.getUpdateTool().checkForResourceUpdates(this,false);
  }

  @Override
  public void onPause() {
    super.onPause();

    // Intentionally not removing KeyboardDownloadEventListener to
    // ensure onKeyboardDownloadFinished() gets called
    // (Transplanted from KeyboardPickerActivity.)
  }

  @Override
  public boolean onSupportNavigateUp() {
    onBackPressed();
    return true;
  }

  @Override
  public void onBackPressed() {
    super.onBackPressed();
    finish();
  }

  // Fully details the building of this Activity's list view items.
  static private class LanguagesAdapter extends NestedAdapter<Dataset.LanguageDataset, Dataset, Void> {
    static final int RESOURCE = R.layout.list_row_layout2;

    private static class ViewHolder {
      ImageView img;
      TextView textLang;
      TextView textCount;
    }

    public LanguagesAdapter(@NonNull Context context, final Dataset storage) {
      super(context, RESOURCE, storage, new AdapterFilter<Dataset.LanguageDataset, Dataset, Void>() {
        public List<Dataset.LanguageDataset> selectFrom(Dataset dataset, Void dummy) {
          // Filter out any languages without installed keyboards.  This can occur with ad-hoc
          // lexical model installations.
          ArrayList<Dataset.LanguageDataset> languages = new ArrayList<>();

          for(Dataset.LanguageDataset language: dataset.asList()) {
            if(language.keyboards.size() > 0) {
              languages.add(language);
            }
          }

          return languages;
        }
      }, null);
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
      Dataset.LanguageDataset data = this.getItem(position);
      ViewHolder holder;

      // If we're being told to reuse an existing view, do that.  It's automatic optimization.
      if (convertView == null) {
        convertView = LayoutInflater.from(getContext()).inflate(RESOURCE, parent, false);
        holder = new ViewHolder();

        holder.img = convertView.findViewById(R.id.image1);
        holder.textLang = convertView.findViewById(R.id.text1);
        holder.textCount = convertView.findViewById(R.id.text2);
        convertView.setTag(holder);
      } else {
        holder = (ViewHolder) convertView.getTag();
      }

      holder.textLang.setText(data.name);
      holder.img.setImageResource(R.drawable.ic_arrow_forward);

      holder.textCount.setText(getContext().getResources().getQuantityString(R.plurals.keyboard_count, data.keyboards.size(), data.keyboards.size()));

      return convertView;
    }
  }




}