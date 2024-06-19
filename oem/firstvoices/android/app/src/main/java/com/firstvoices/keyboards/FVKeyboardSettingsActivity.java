package com.firstvoices.keyboards;

import static android.view.View.INVISIBLE;
import static android.view.View.VISIBLE;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.database.DataSetObserver;
import android.os.Bundle;
import android.view.View;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.SwitchCompat;
import androidx.appcompat.widget.Toolbar;

import com.keyman.engine.KMKeyboardDownloaderActivity;
import com.keyman.engine.KMManager;
import com.keyman.engine.KeyboardPickerActivity;
import com.keyman.engine.ModelInfoActivity;
import com.keyman.engine.ModelPickerActivity;
import com.keyman.engine.cloud.CloudApiTypes;
import com.keyman.engine.cloud.CloudDownloadMgr;
import com.keyman.engine.cloud.impl.CloudLexicalModelMetaDataDownloadCallback;
import com.keyman.engine.data.CloudRepository;
import com.keyman.engine.data.Dataset;
import com.keyman.engine.data.Keyboard;
import com.keyman.engine.data.KeyboardController;
import com.keyman.engine.data.LexicalModel;
import com.keyman.engine.util.BCP47;
import com.keyman.engine.util.KMLog;
import com.keyman.engine.util.MapCompat;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * Displays an FV Keyboard enable and some lexical model switches.
 */
public final class FVKeyboardSettingsActivity extends AppCompatActivity {
  private static Context context = null;
  private static Toolbar toolbar = null;
  private static TextView fvKeyboardTextView = null;
  private static TextView fvVersionTextView = null;
  private static TextView correctionsTextView = null;
  private static SwitchCompat fvKeyboardToggle = null;
  private static SwitchCompat correctionsToggle = null;
  private static ModelPickerActivity.FilteredLexicalModelAdapter listAdapter = null;
  private String associatedLexicalModel = "";
  private String lgCode;
  private static RelativeLayout checkModelLayout = null;
  private static Intent intent = null;
  private static Bundle bundle = null;
  private static ListView listView = null;
  private String lgName;
  private String kbId;
  private String kbName;
  private String customHelpLink;
  private String version;
  private SharedPreferences prefs;

  private DataSetObserver repoObserver;
  private static Dataset repo;
  private boolean didExecuteParser = false;

  private final static String TAG = "FVKbdSettingsAct";

  private class PreferenceToggleListener implements View.OnClickListener {
    String prefsKey;
    String lgCode;

    public PreferenceToggleListener(String prefsKey, String lgCode) {
      this.prefsKey = prefsKey;
      this.lgCode = lgCode;
    }

    @Override
    public void onClick(View v) {
      // For predictions/corrections toggle
      SwitchCompat toggle = (SwitchCompat) v;

      SharedPreferences.Editor prefEditor = prefs.edit();

      // predictionsToggle overrides correctionToggle and correctionsTextView
      if (prefsKey.endsWith(KMManager.predictionPrefSuffix)) {
        boolean override = toggle.isChecked();
        overrideCorrectionsToggle(override);
      }

      // This will allow preemptively making settings for languages without models.
      // Seems more trouble than it's worth to block this.
      prefEditor.putBoolean(prefsKey, toggle.isChecked());
      prefEditor.apply();

      // Don't use/apply language modeling settings for languages without models.
      if (associatedLexicalModel.isEmpty()) {
        return;
      }

      // If the active keyboard is for this language, immediately enact the new pref setting.
      registerMatchingLexicalModel(lgCode);
    }
  }

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    supportRequestWindowFeature(Window.FEATURE_NO_TITLE);
    context = this;
    setContentView(R.layout.fv_keyboard_settings_list_layout);

    if (getIntent() != null && getIntent().getExtras() != null) {
      intent = getIntent();
      bundle = intent.getExtras();
    }
    if (bundle == null) {
      // Should never actually happen.
      KMLog.LogError(TAG, "Language data not specified for FVKeyboardSettingsActivity!");
      finish();

      if(KMManager.isDebugMode()) {
        throw new NullPointerException("Language data not specified for FVKeyboardSettingsActivity!");
      } else {
        return;
      }
    }

    // Establish the list view based on the CloudRepository's Dataset
    if (!didExecuteParser) {
      didExecuteParser = true;
      repo = CloudRepository.shared.fetchDataset(context);
    }
    // add listener to dataset to get event for catalog update.
    repoObserver = new DataSetObserver() {
      @Override public void onChanged() {
      }
    };
    repo.registerDataSetObserver(repoObserver);

    kbId = bundle.getString(KMManager.KMKey_KeyboardID);
    kbName = bundle.getString(KMManager.KMKey_KeyboardName);
    lgCode = bundle.getString(KMManager.KMKey_LanguageID);
    lgName = bundle.getString(KMManager.KMKey_LanguageName);
    version = bundle.getString(KMManager.KMKey_Version);

    toolbar = (Toolbar) findViewById(R.id.list_toolbar);
    setSupportActionBar(toolbar);
    getSupportActionBar().setDisplayHomeAsUpEnabled(true);
    getSupportActionBar().setDisplayShowHomeEnabled(true);
    getSupportActionBar().setDisplayShowTitleEnabled(false);
    TextView textView = (TextView) findViewById(R.id.bar_title);

    textView.setText(String.format(getString(R.string.keyboard_settings), kbName));
    RelativeLayout layout = (RelativeLayout)findViewById(R.id.keyboard_toggle);
    fvKeyboardTextView = (TextView) layout.findViewById(R.id.text1);
    fvKeyboardTextView.setText(getString(R.string.enable_keyboard));
    fvVersionTextView = (TextView) layout.findViewById(R.id.text2);
    fvVersionTextView.setText(String.format(getString(R.string.fv_keyboard_version), version));
    fvKeyboardToggle = layout.findViewById(R.id.toggle);
    fvKeyboardToggle.setChecked(KeyboardController.getInstance().keyboardExists(FVShared.FVDefault_PackageID, kbId, null));
    fvKeyboardToggle.setOnClickListener(new View.OnClickListener() {
        @Override
        public void onClick(View v) {
          // If keyboard enabled, determine if associated lexical model should be downloaded
          // Check if associated model is not already installed
          if (fvKeyboardToggle.isChecked() && (KMManager.getAssociatedLexicalModel(lgCode) == null)) {
            queryModel();
          }

           FVShared.getInstance().setCheckState(kbId, fvKeyboardToggle.isChecked());
        }
    });

    // The following two layouts/toggles will need to link with these objects.
    prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    boolean mayPredict = prefs.getBoolean(KMManager.getLanguagePredictionPreferenceKey(lgCode), false);
    boolean mayCorrect = prefs.getBoolean(KMManager.getLanguageCorrectionPreferenceKey(lgCode), false);

    layout = (RelativeLayout)findViewById(R.id.corrections_toggle);

    correctionsTextView = (TextView) layout.findViewById(R.id.text1);
    correctionsTextView.setText(getString(R.string.enable_corrections));
    correctionsToggle = layout.findViewById(R.id.toggle);
    correctionsToggle.setChecked(mayCorrect); // Link to persistent option storage!  Also needs handler.
    String prefsKey = KMManager.getLanguageCorrectionPreferenceKey(lgCode);
    correctionsToggle.setOnClickListener(new PreferenceToggleListener(prefsKey, lgCode));

    layout = (RelativeLayout)findViewById(R.id.predictions_toggle);

    textView = (TextView) layout.findViewById(R.id.text1);
    textView.setText(getString(R.string.enable_predictions));
    SwitchCompat predictionsToggle = layout.findViewById(R.id.toggle);
    predictionsToggle.setChecked(mayPredict); // Link to persistent option storage!  Also needs handler.
    prefsKey = KMManager.getLanguagePredictionPreferenceKey(lgCode);
    predictionsToggle.setOnClickListener(new PreferenceToggleListener(prefsKey, lgCode));

    overrideCorrectionsToggle(mayPredict);

    checkModelLayout = (RelativeLayout)findViewById(R.id.check_model);
    textView = checkModelLayout.findViewById(R.id.text1);
    textView.setText(getString(R.string.check_model_online));

    checkModelLayout.setOnClickListener(new View.OnClickListener() {
      @Override
      public void onClick(View v) {
        queryModel();
      }
    });

    // Display model picker list
    listView = (ListView)findViewById(R.id.listView);
    listView.setFastScrollEnabled(true);

    // Initialize the dataset of available lexical models (installed and from the cloud catalog)
    listAdapter = new ModelPickerActivity.FilteredLexicalModelAdapter(context, repo, lgCode);
    listView.setAdapter(listAdapter);
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {

      @Override
      public void onItemClick(AdapterView<?> parent, View view, final int position, long id) {
        int selectedIndex = position;
        LexicalModel model = ((ModelPickerActivity.FilteredLexicalModelAdapter) listView.getAdapter()).getItem(position);
        String packageID = model.getPackageID();
        String languageID = model.getLanguageID();
        String modelID = model.getLexicalModelID();
        String modelName = model.getLexicalModelName();
        String langName = model.getLanguageName();
        String version = model.getVersion();

        boolean immediateRegister = false;

        // File check to see if lexical model file already exists locally (may not be currently installed)
        File modelCheck = new File(KMManager.getLexicalModelsDir() + packageID + File.separator + modelID + ".model.js");
        String modelKey = model.getKey();
        boolean modelInstalled = KeyboardPickerActivity.containsLexicalModel(context, modelKey);
        if (modelInstalled) {
          // Show Model Info
          listView.setItemChecked(position, true);
          listView.setSelection(position);

          // Start intent for selected Predictive Text Model screen
          Intent i = new Intent(context, ModelInfoActivity.class);
          i.putExtra(KMManager.KMKey_LexicalModel, model);
          startActivityForResult(i, 1);
        } else if (modelCheck.exists()) {
          // Handle scenario where previously installed kmp already exists so
          // we only need to add the model to the list of installed models
          HashMap<String, String> modelInfo = new HashMap<String, String>();
          modelInfo.put(KMManager.KMKey_PackageID, packageID);
          modelInfo.put(KMManager.KMKey_LexicalModelID, modelID);
          modelInfo.put(KMManager.KMKey_LanguageID, languageID);
          modelInfo.put(KMManager.KMKey_LexicalModelName, modelName);
          modelInfo.put(KMManager.KMKey_LanguageName, langName);
          modelInfo.put(KMManager.KMKey_LexicalModelVersion, version);
          // Add help link
          modelInfo.put(KMManager.KMKey_CustomHelpLink, "");

          boolean result = KMManager.addLexicalModel(context, new HashMap<>(modelInfo));
          if (result) {
            Toast.makeText(context, getString(R.string.model_install_toast), Toast.LENGTH_SHORT).show();
          }

          immediateRegister = true;
        } else {
          // Model isn't installed so prompt to download it
          Bundle args = model.buildDownloadBundle();
          Intent i = new Intent(getApplicationContext(), KMKeyboardDownloaderActivity.class);
          i.putExtras(args);
          startActivity(i);
        }

        // If we had a previously-installed lexical model, we should 'deinstall' it so that only
        // one model is actively linked to any given language.
        if(!modelInstalled && !immediateRegister) {
          // While awkward, we must obtain the preInstalledModelMap before any installations occur.
          // We don't want to remove the model we just installed, after all!
          HashMap<String, String> preInstalledModelMap = KMManager.getAssociatedLexicalModel(languageID);
          if(preInstalledModelMap != null) {
            // This might be unnecessary
            LexicalModel preInstalled = new LexicalModel(
              preInstalledModelMap.get(KMManager.KMKey_PackageID),
              preInstalledModelMap.get(KMManager.KMKey_LexicalModelID),
              preInstalledModelMap.get(KMManager.KMKey_LexicalModelName),
              preInstalledModelMap.get(KMManager.KMKey_LanguageID),
              preInstalledModelMap.get(KMManager.KMKey_LanguageName),
              preInstalledModelMap.get(KMManager.KMKey_LexicalModelVersion),
              preInstalledModelMap.get(KMManager.KMKey_CustomHelpLink),
              MapCompat.getOrDefault(preInstalledModelMap, KMManager.KMKey_KMPLink, ""));
            String itemKey = preInstalled.getKey();
            int modelIndex = KeyboardPickerActivity.getLexicalModelIndex(context, itemKey);
            KMManager.deleteLexicalModel(context, modelIndex, true);
          }
        }

        if(immediateRegister) {
          // Register associated lexical model if it matches the active keyboard's language code;
          // it's safe since we're on the same thread.  Needs to be called AFTER deinstalling the old one.
          registerMatchingLexicalModel(languageID);
        }

        // Force a display refresh.
        notifyDataSetChanged();
      }
    });

    Intent i = getIntent();
    listView.setSelectionFromTop(i.getIntExtra("listPosition", 0),
      i.getIntExtra("offsetY", 0));

    // Rescale listview and hide "Dictionaries" header if no dictionaries installed
    updateDictionariesSection();
  }

  public void updateDictionariesSection() {
    if (listAdapter != null && toolbar != null) {
      int actionBarHeight = toolbar.getLayoutParams().height;
      int numModels = listAdapter.getCount();
      listView.getLayoutParams().height = actionBarHeight * (numModels);

      TextView label = findViewById(R.id.model_label);
      if (label != null) {
        if (numModels == 0) {
          label.setVisibility(View.GONE);
        } else {
          label.setVisibility(VISIBLE);
        }
      }
    }
  }

  private void queryModel() {
    if (!KMManager.hasConnection(context)) {
      Toast.makeText(context,
        context.getString(R.string.cannot_query_associated_model),
        Toast.LENGTH_SHORT).show();
      return;
    }

    String _downloadid = CloudLexicalModelMetaDataDownloadCallback.createDownloadId(lgCode);
    CloudLexicalModelMetaDataDownloadCallback _callback = new CloudLexicalModelMetaDataDownloadCallback();

    Toast.makeText(context,
      context.getString(R.string.query_associated_model),
      Toast.LENGTH_SHORT).show();

    ArrayList<CloudApiTypes.CloudApiParam> aPreparedCloudApiParams = new ArrayList<>();
    String url = CloudRepository.prepareLexicalModelQuery(lgCode);
    aPreparedCloudApiParams.add(new CloudApiTypes.CloudApiParam(
      CloudApiTypes.ApiTarget.KeyboardLexicalModels, url).setType(CloudApiTypes.JSONType.Array));

    CloudDownloadMgr.getInstance().executeAsDownload(
      context, _downloadid, null, _callback,
      aPreparedCloudApiParams.toArray(new CloudApiTypes.CloudApiParam[0]));
  }

  // Register associated lexical model if it matches the active keyboard's language code
  private void registerMatchingLexicalModel(String languageID) {
    if (context != null) {
      Keyboard currentKeyboard = KMManager.getCurrentKeyboardInfo(context);
      if (currentKeyboard != null && BCP47.languageEquals(currentKeyboard.getLanguageID(), languageID)) {
        KMManager.registerAssociatedLexicalModel(languageID);
      }
    }
  }

  @Override
  public void onResume() {
    super.onResume();

    notifyDataSetChanged();
  }

  public static void notifyDataSetChanged() {
    if (listAdapter != null) {
      listAdapter.notifyDataSetChanged();
    }
  }

  public static void restartActivity() {
    if (context != null) {
      // Not using Activity.recreate() because we need to recalculate the items
      ((Activity)context).finish();
      ((Activity)context).startActivity(intent);
    }
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
      int visibility = override ? VISIBLE : INVISIBLE;
      correctionsToggle.setVisibility(visibility);
    }
  }
}
