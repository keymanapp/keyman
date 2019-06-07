
package com.tavultesoft.kmea;

import android.annotation.SuppressLint;
import android.app.ProgressDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.ImageView;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;

import com.tavultesoft.kmea.data.CloudRepository;
import com.tavultesoft.kmea.data.Dataset;
import com.tavultesoft.kmea.data.LexicalModel;
import com.tavultesoft.kmea.data.adapters.AdapterFilter;
import com.tavultesoft.kmea.data.adapters.NestedAdapter;
import com.tavultesoft.kmea.util.MapCompat;

import org.json.JSONArray;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * Keyman Settings --> Languages Settings --> Language Settings --> Model List
 * Gets the list of installable lexical models from Keyman cloud and allows user to download a model.
 * Displays a list of available models for a language ID.
 */
public final class ModelPickerActivity extends AppCompatActivity {

  private Context context;
  private static Toolbar toolbar = null;
  private static ListView listView = null;

  private static Dataset repo;
  private boolean didExecuteParser = false;

  private final static String TAG = "ModelPickerActivity";

  private String languageID = "";

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
    String newLanguageID = bundle.getString(KMManager.KMKey_LanguageID);

    // Sometimes we need to re-initialize the list of models that are displayed in the ListView
    languageID = newLanguageID;

    final String languageName = bundle.getString(KMManager.KMKey_LanguageName);
    textView.setText(String.format("%s model", languageName));

    listView = (ListView) findViewById(R.id.listView);
    listView.setFastScrollEnabled(true);
  }

  @Override
  protected void onResume() {
    super.onResume();
    if (!didExecuteParser) {
      didExecuteParser = true;
      //new JSONParse().execute();
      repo = CloudRepository.shared.fetchDataset(context, null, null);

      listView.setAdapter(new FilteredLexicalModelAdapter(context, repo.lexicalModels, languageID));
      listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {

        @Override
        public void onItemClick(AdapterView<?> parent, View view, final int position, long id) {
          int selectedIndex = position;
          Map<String, String> modelInfo = ((FilteredLexicalModelAdapter) listView.getAdapter()).getItem(position).map;
          String packageID = modelInfo.get(KMManager.KMKey_PackageID);
          String languageID = modelInfo.get(KMManager.KMKey_LanguageID);
          String modelID = modelInfo.get(KMManager.KMKey_LexicalModelID);
          String modelName = modelInfo.get(KMManager.KMKey_LexicalModelName);
          String langName = modelInfo.get(KMManager.KMKey_LanguageName);

          // File check to see if lexical model already exists locally
          File modelCheck = new File(KMManager.getLexicalModelsDir() + packageID + File.separator + modelID + ".model.js");

          // Using the presence of the left icon "check" to determine if the model is installed
          boolean modelInstalled = modelInfo.containsKey("leftIcon");
          if (modelInstalled) {
            // Show Model Info
            listView.setItemChecked(position, true);
            listView.setSelection(position);

            // Start intent for selected Predictive Text Model screen
            if (!languageID.equalsIgnoreCase(modelInfo.get(KMManager.KMKey_LanguageID))) {
              Log.d(TAG, "Language ID " + languageID + " doesn't match model language ID: " +
                  modelInfo.get(KMManager.KMKey_LanguageID));
            }
            Bundle bundle = new Bundle();
            // Note: package ID of a model is different from package ID for a keyboard.
            // Language ID can be re-used
            bundle.putString(KMManager.KMKey_PackageID, packageID);
            bundle.putString(KMManager.KMKey_LanguageID, languageID);
            bundle.putString(KMManager.KMKey_LexicalModelID, modelID);
            bundle.putString(KMManager.KMKey_LexicalModelName, modelName);
            bundle.putString(KMManager.KMKey_LexicalModelVersion,
                modelInfo.get(KMManager.KMKey_LexicalModelVersion));
            bundle.putString(KMManager.KMKey_CustomModel,
                MapCompat.getOrDefault(new HashMap<>(modelInfo), KMManager.KMKey_CustomModel, "N"));
            Intent i = new Intent(context, ModelInfoActivity.class);
            i.putExtras(bundle);
            startActivityForResult(i, 1);
          } else if (modelCheck.exists()) {
            // Handle scenario where previously installed kmp already exists so
            // we only need to add the model to the list of installed models
            // Add help link
            modelInfo.put(KMManager.KMKey_CustomHelpLink, "");
            boolean result = KMManager.addLexicalModel(context, new HashMap<>(modelInfo));
            if (result) {
              Toast.makeText(context, "Model installed", Toast.LENGTH_SHORT).show();
            }

            // Don't register associated lexical model because this is not a UI view
          } else {
            // Model isn't installed so prompt to download it
            Bundle args = new Bundle();
            args.putString(KMKeyboardDownloaderActivity.ARG_PKG_ID, packageID);
            args.putString(KMKeyboardDownloaderActivity.ARG_LANG_ID, languageID);
            args.putString(KMKeyboardDownloaderActivity.ARG_MODEL_ID, modelID);
            args.putString(KMKeyboardDownloaderActivity.ARG_MODEL_NAME, modelName);
            args.putString(KMKeyboardDownloaderActivity.ARG_LANG_NAME, langName);
            args.putBoolean(KMKeyboardDownloaderActivity.ARG_IS_CUSTOM, false);
            args.putString(KMKeyboardDownloaderActivity.ARG_MODEL_URL,
                modelInfo.get(KMManager.KMKey_LexicalModelPackageFilename));
            Intent i = new Intent(getApplicationContext(), KMKeyboardDownloaderActivity.class);
            i.putExtras(args);
            startActivity(i);
          }
        }
      });

      Intent i = getIntent();
      listView.setSelectionFromTop(i.getIntExtra("listPosition", 0),
          i.getIntExtra("offsetY", 0));
    }
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

  @Override
  protected void onActivityResult(int requestCode, int resultCode, Intent data) {
    if (resultCode == 1) {
      finish();
    }
  }

  // Uses the repo dataset's master lexical model list to create a filtered adapter for use here.
  // As this one is specific to this class, we can implement Activity-specific functionality within it.
  static private class FilteredLexicalModelAdapter extends NestedAdapter<LexicalModel, Dataset.LexicalModels> {
    static final int RESOURCE = R.layout.models_list_row_layout;

    public FilteredLexicalModelAdapter(@NonNull Context context, Dataset.LexicalModels lexicalModels, final String languageCode) {
      super(context, RESOURCE, lexicalModels, new AdapterFilter<LexicalModel>() {
        @Override
        public boolean matches(LexicalModel elem) {
          return elem.getLanguageCode().equals(languageCode);
        }
      });
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
      LexicalModel model = this.getItem(position);

      // If we're being told to reuse an existing view, do that.  It's automatic optimization.
      if (convertView == null) {
        convertView = LayoutInflater.from(getContext()).inflate(RESOURCE, parent, false);
      }

      View view = convertView;

      ImageView img1 = view.findViewById(R.id.image1);
      TextView text1 = view.findViewById(R.id.text1);
      ImageView img2 = view.findViewById(R.id.image2);

      int leftIcon = 0; // default:  hide the icon.
      if (model.map.get("leftIcon") != null) { // Set by CloudRepository if the model is locally installed.
        leftIcon = Integer.parseInt(model.map.get("leftIcon"));
      }
      img1.setImageResource(leftIcon);

      text1.setText(model.map.get(KMManager.KMKey_LexicalModelName));

      int icon = 0;
      if (model.map.get(KMManager.KMKey_Icon) != null) {
        icon = Integer.parseInt(model.map.get(KMManager.KMKey_Icon));
      }
      img2.setImageResource(icon);

      return view;
    }
  }
}
