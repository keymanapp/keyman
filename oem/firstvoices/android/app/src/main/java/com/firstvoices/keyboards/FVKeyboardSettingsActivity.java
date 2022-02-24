package com.firstvoices.keyboards;

import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.view.View;
import android.view.Window;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.SwitchCompat;
import androidx.appcompat.widget.Toolbar;

import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.ModelPickerActivity;
import com.tavultesoft.kmea.cloud.CloudApiTypes;
import com.tavultesoft.kmea.cloud.CloudDownloadMgr;
import com.tavultesoft.kmea.cloud.impl.CloudLexicalModelMetaDataDownloadCallback;
import com.tavultesoft.kmea.data.CloudRepository;
import com.tavultesoft.kmea.data.Dataset;
import com.tavultesoft.kmea.data.KeyboardController;
import com.tavultesoft.kmea.util.KMLog;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Displays an FV Keyboard enable and some lexical model switches.
 */
public final class FVKeyboardSettingsActivity extends AppCompatActivity {
  private Context context;
  private static Toolbar toolbar = null;
  private static TextView fvKeyboardTextView = null;
  private static TextView fvVersionTextView = null;
  private static TextView lexicalModelTextView = null;
  private static TextView correctionsTextView = null;
  private static SwitchCompat fvKeyboardToggle = null;
  private static SwitchCompat correctionsToggle = null;
  private String associatedLexicalModel = "";
  private String lgCode;
  private String lgName;
  private String kbId;
  private String kbName;
  private String customHelpLink;
  private String version;
  private SharedPreferences prefs;

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
      String kbdLgCode = KMManager.getCurrentKeyboardInfo(context).getLanguageID();
      if (kbdLgCode.equals(lgCode)) {
        // Not only registers the model but also applies our modeling preferences.
        KMManager.registerAssociatedLexicalModel(lgCode);
      }
    }
  }

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    supportRequestWindowFeature(Window.FEATURE_NO_TITLE);
    context = this;
    setContentView(R.layout.fv_keyboard_settings_list_layout);

    Bundle bundle = getIntent().getExtras();
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

    // Force the cloud catalog to update
    if (!didExecuteParser) {
      didExecuteParser = true;
      repo = CloudRepository.shared.fetchDataset(context);
    }

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
    fvVersionTextView.setText(String.format(getString(R.string.keyboard_version), version));
    fvKeyboardToggle = layout.findViewById(R.id.toggle);
    fvKeyboardToggle.setChecked(KeyboardController.getInstance().keyboardExists(FVShared.FVDefault_PackageID, kbId, null));
    fvKeyboardToggle.setOnClickListener(new View.OnClickListener() {
        @Override
        public void onClick(View v) {
          // If keyboard enabled, determine if associated lexical model should be downloaded
          // Check if associated model is not already installed
          if (fvKeyboardToggle.isChecked() && (KMManager.getAssociatedLexicalModel(lgCode) == null) && KMManager.hasConnection(context)) {
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

           FVShared.getInstance().setCheckState(kbId, fvKeyboardToggle.isChecked());
        }
    });

    // The following two layouts/toggles will need to link with these objects.
    Context appContext = this.getApplicationContext();
    prefs = appContext.getSharedPreferences(appContext.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    boolean mayPredict = prefs.getBoolean(KMManager.getLanguagePredictionPreferenceKey(lgCode), true);
    boolean mayCorrect = prefs.getBoolean(KMManager.getLanguageCorrectionPreferenceKey(lgCode), true);

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

    layout = (RelativeLayout)findViewById(R.id.model_picker);
    textView = (TextView) layout.findViewById(R.id.text1);
    textView.setText(getString(R.string.model_label));

    lexicalModelTextView = layout.findViewById(R.id.text2);

    updateActiveLexicalModel();

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
     * imageView.setImageResource(R.drawable.ic_arrow_forward);
     */
  }

  @Override
  public void onResume() {
    super.onResume();

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

  public static void setActiveLexicalModelLabel(String lexicalModelLabel) {
    if (lexicalModelTextView != null && lexicalModelLabel != null && !lexicalModelLabel.isEmpty()) {
      lexicalModelTextView.setText(lexicalModelLabel);
      lexicalModelTextView.setEnabled(true);
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
}
