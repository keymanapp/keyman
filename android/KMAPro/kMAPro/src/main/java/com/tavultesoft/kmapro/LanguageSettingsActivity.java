/**
 * Copyright (C) 2019 SIL International. All rights reserved.
 */

package com.tavultesoft.kmapro;

import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.ImageButton;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.RadioButton;
import android.widget.RadioGroup;
import android.widget.RadioGroup.OnCheckedChangeListener;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.IdRes;
import androidx.annotation.NonNull;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.SwitchCompat;
import androidx.appcompat.widget.Toolbar;

import com.keyman.engine.KeyboardPickerActivity;
import com.keyman.engine.KMManager;
import com.keyman.engine.ModelPickerActivity;
import com.keyman.engine.data.Dataset;
import com.keyman.engine.data.Keyboard;
import com.keyman.engine.data.adapters.NestedAdapter;
import com.keyman.engine.util.KMLog;

import java.util.HashMap;

/**
 * Keyman Settings --> Languages Settings --> Language Settings
 * Displays a list of installed keyboards and some lexical model switches.
 */
public final class LanguageSettingsActivity extends AppCompatActivity {
  private Context context;
  private static Toolbar toolbar = null;
  private static ListView listView = null;
  private static TextView lexicalModelTextView = null;
  private static TextView correctionsTextView = null;
  private static SwitchCompat correctionsToggle = null;
  private ImageButton addButton = null;
  private String associatedLexicalModel = "";
  private String lgCode;
  private String lgName;
  private String customHelpLink;
  private SharedPreferences prefs;

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

    listView = (ListView) findViewById(R.id.listView);
    listView.setFastScrollEnabled(true);

    Bundle bundle = getIntent().getExtras();
    if (bundle == null) {
      // Should never actually happen.
      KMLog.LogError(TAG, "Language data not specified for LanguageSettingsActivity!");
      finish();

      if(KMManager.isDebugMode()) {
        throw new NullPointerException("Language data not specified for LanguageSettingsActivity!");
      } else {
        return;
      }
    }

    lgCode = bundle.getString(KMManager.KMKey_LanguageID);
    lgName = bundle.getString(KMManager.KMKey_LanguageName);
    customHelpLink = bundle.getString(KMManager.KMKey_CustomHelpLink, "");

    // Necessary to properly insert a language name into the title.  (Has a %s slot for it.)
    String title = String.format(getString(R.string.title_language_settings), lgName);
    textView.setText(title);

    FilteredKeyboardsAdapter adapter = new FilteredKeyboardsAdapter(context, KeyboardPickerActivity.getInstalledDataset(context), lgCode);

    // The following radio group will need to link with these objects.
    Context appContext = this.getApplicationContext();
    prefs = appContext.getSharedPreferences(appContext.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    int maySuggest =  prefs.getInt(KMManager.getLanguageAutoCorrectionPreferenceKey(lgCode), KMManager.KMDefault_Suggestion);

    // Initialize Radio button group change listeners
    RadioGroup radioGroup = (RadioGroup) findViewById(R.id.suggestion_radio_group);
    radioGroup.clearCheck();

    // Auto-correct disabled for Keyman 18.0 #12767
    int[] RadioButtonArray = {
      R.id.suggestion_radio_0,
      R.id.suggestion_radio_1,
      R.id.suggestion_radio_2};
    RadioButton radioButton = (RadioButton)radioGroup.findViewById(RadioButtonArray[maySuggest]);
    radioButton.setChecked(true);

    radioGroup.setOnCheckedChangeListener(new OnCheckedChangeListener() {
      @Override
      public void onCheckedChanged(RadioGroup group, @IdRes int checkId) {
        RadioButton checkedButton = (RadioButton)radioGroup.findViewById(checkId);
        int index = radioGroup.indexOfChild(checkedButton);
        KMManager.setMaySuggest(lgCode, KMManager.SuggestionType.fromInt(index));

        // Don't use/apply language modeling settings for languages without models.
        if (associatedLexicalModel.isEmpty()) {
          return;
        }

        Keyboard kbInfo = KMManager.getCurrentKeyboardInfo(context);
        if(kbInfo != null) {
          // If the active keyboard is for this language, immediately enact the new pref setting.
          String kbdLgCode = kbInfo.getLanguageID();
          if (kbdLgCode.equals(lgCode)) {
            // Not only registers the model but also applies our modeling preferences.
            KMManager.registerAssociatedLexicalModel(lgCode);
          }
        }
      }
    });

    RelativeLayout layout = (RelativeLayout)findViewById(R.id.model_picker);
    textView = (TextView) layout.findViewById(R.id.text1);
    textView.setText(getString(R.string.model_label));

    lexicalModelTextView = layout.findViewById(R.id.text2);

    updateActiveLexicalModel();

    ImageView imageView = (ImageView) layout.findViewById(R.id.image1);
    imageView.setImageResource(R.drawable.ic_action_forward);
    layout.setEnabled(true);
    layout.setOnClickListener(new View.OnClickListener() {
      @Override
      public void onClick(View v) {
        // Start ModelPickerActivity
        Bundle bundle = new Bundle();
        bundle.putString(KMManager.KMKey_LanguageID, lgCode);
        bundle.putString(KMManager.KMKey_LanguageName, lgName);
        bundle.putString(KMManager.KMKey_CustomHelpLink, customHelpLink);
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
     * imageView.setImageResource(R.drawable.ic_action_forward);
     */

    listView.setAdapter(adapter);
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
      @Override
      public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        listView.setItemChecked(position, true);
        listView.setSelection(position);
        Keyboard kbdInfo = ((FilteredKeyboardsAdapter) listView.getAdapter()).getItem(position);
        Intent intent = new Intent(context, KeyboardSettingsActivity.class);
        intent.addFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);
        Bundle bundle = new Bundle();
        bundle.putSerializable(KMManager.KMKey_Keyboard, kbdInfo);
        intent.putExtras(bundle);
        startActivity(intent);
      }
    });

    addButton = (ImageButton) findViewById(R.id.add_button);
    addButton.setOnClickListener(new View.OnClickListener() {
      public void onClick(View v) {
        // Check scenarios to add available keyboards:
        if (KMManager.hasConnection(context)){
          // Scenario 1: Connection to keyman.com catalog
          // Pass the BCP47 language code to the KMPBrowserActivity
          Intent i = new Intent(context, KMPBrowserActivity.class);
          i.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
          i.addFlags(Intent.FLAG_ACTIVITY_SINGLE_TOP);
          i.putExtra("languageCode", lgCode);
          i.putExtra("languageName", lgName);
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
  }

  @Override
  public void onResume() {
    super.onResume();

    FilteredKeyboardsAdapter adapter = ((FilteredKeyboardsAdapter) listView.getAdapter());

    if(adapter != null) {
      // Despite the fact that updates should have been auto-triggered anyway, it seems that we
      // need a manual call here for things to happen in a timely fashion.
      adapter.notifyDataSetChanged();
    }

    updateActiveLexicalModel();
  }

  /**
   * Updates the active lexical model label with the name of the associated lexical model.
   * If there's no associated lexical model, the label displays a prompt to check for an available model.
   */
  public void updateActiveLexicalModel() {
    HashMap<String, String> lexModelMap = KMManager.getAssociatedLexicalModel(lgCode);
    if(lexModelMap != null) {
      associatedLexicalModel = lexModelMap.get(KMManager.KMKey_LexicalModelName);
    } else {
      // Prompt to check for available dictionary
      associatedLexicalModel = getString(R.string.check_available_model);
    }

    lexicalModelTextView.setText(associatedLexicalModel);
    lexicalModelTextView.setEnabled(true);
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
    super.onBackPressed();
    finish();
  }

  /**
   * Overrides the enable and visibility of corrections toggle,
   * and overrides the enable of the corrections text view.
   * Does not change the corrections toggle value.
   * @param override boolean - Value from predictions toggle
   *     When true, enables corrections toggle and text field, and makes corrections toggle visible
   *     When false, disables corrections toggle and text field, and makes corrections toggle invisible
   */
  private void overrideCorrectionsToggle(boolean override) {
    if (correctionsTextView != null) {
      correctionsTextView.setEnabled(override);
    }
    if (correctionsToggle != null) {
      correctionsToggle.setEnabled(override);
      int visibility = override ? View.VISIBLE : View.INVISIBLE;
      correctionsToggle.setVisibility(visibility);
    }
  }

  // Fully details the building of this Activity's list view items.
  static private class FilteredKeyboardsAdapter extends NestedAdapter<com.keyman.engine.data.Keyboard, Dataset.Keyboards, String> {
    static final int RESOURCE = R.layout.list_row_layout1;

    private static class ViewHolder {
      ImageView img;
      TextView text;
    }

    public FilteredKeyboardsAdapter(@NonNull Context context, final Dataset storage, final String languageCode) {
      // Goal:  to not need a custom filter here, instead relying on LanguageDataset's built-in filters.
      super(context, RESOURCE, storage.keyboards, storage.keyboardFilter, languageCode);
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

      holder.text.setText(kbd.getResourceName());
      holder.img.setImageResource(R.drawable.ic_action_forward);

      return convertView;
    }
  }
}