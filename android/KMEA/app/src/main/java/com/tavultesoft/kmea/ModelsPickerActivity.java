
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
 * Keyman Settings --> Languages Settings --> Language Settings --> Models Picker
 * Displays a list of available models for a language ID.
 */
public final class ModelsPickerActivity extends AppCompatActivity {

  private Context context;
  private static Toolbar toolbar = null;
  private static ListView listView = null;
  private static ArrayList<HashMap<String, String>> lexicalModelsList = null;

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

    Bundle bundle = getIntent().getExtras();
    String languageID = "", languageName = "";
    if (bundle != null) {
      languageID = bundle.getString(KMManager.KMKey_LanguageID);
      languageName = bundle.getString(KMManager.KMKey_LanguageName);
    }
    textView.setText(String.format("%s model", languageName));

    listView = (ListView) findViewById(R.id.listView);
    listView.setFastScrollEnabled(true);

    lexicalModelsList = getModelsList(context, languageID);

    String[] from = new String[]{"leftIcon", KMManager.KMKey_LexicalModelName, KMManager.KMKey_Icon};
    int[] to = new int[]{R.id.image1, R.id.text1, R.id.image2};
    ListAdapter listAdapter = new KMListAdapter(context, lexicalModelsList, R.layout.models_list_row_layout, from, to);
    listView.setAdapter(listAdapter);
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
      @Override
      public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        listView.setItemChecked(position, true);
        listView.setSelection(position);

        // TODO: Start intent for selected Predictive Text Model screen
      }
    });

  }

  @Override
  protected void onResume() {
    super.onResume();

  }

  @Override
  protected void onPause() {
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

  public static ArrayList<HashMap<String, String>> getModelsList(Context context, String languageID) {
    ArrayList<HashMap<String, String>> list = new ArrayList<HashMap<String, String>>();

    // Start with the list of currently installed models
    ArrayList<HashMap<String, String>> availableLexicalModels = KeyboardPickerActivity.getLexicalModelsList(context);

    for(HashMap<String, String> modelInfo : availableLexicalModels) {
      if (modelInfo.get(KMManager.KMKey_LanguageID).equalsIgnoreCase(languageID)) {
        // Add icons showing model is installed (check)
        modelInfo.put("leftIcon", String.valueOf(R.drawable.ic_check));
        modelInfo.put(KMManager.KMKey_Icon, String.valueOf(R.drawable.ic_arrow_forward));
        modelInfo.put("isEnabled", "true");
        list.add(modelInfo);
      }
    }

    // TODO: Check the list with models available in cloud. api.keyman.com needs to be done in a background network task

    return list;
  }
}