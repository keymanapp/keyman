
package com.keyman.engine;

import android.content.Context;
import android.content.Intent;
import android.database.DataSetObserver;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;

import com.keyman.engine.cloud.CloudApiTypes;
import com.keyman.engine.cloud.CloudDownloadMgr;
import com.keyman.engine.cloud.DownloadManagerDisabledException;
import com.keyman.engine.cloud.impl.CloudLexicalModelMetaDataDownloadCallback;
import com.keyman.engine.data.CloudRepository;
import com.keyman.engine.data.Dataset;
import com.keyman.engine.data.LexicalModel;
import com.keyman.engine.data.adapters.NestedAdapter;
import com.keyman.engine.util.BCP47;
import com.keyman.engine.util.MapCompat;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * Keyman Settings --> Languages Settings --> Language Settings --> Model List
 * Gets the list of installable lexical models from Keyman cloud and allows user to download a model.
 * Displays a list of available models for a language ID.
 */
public final class ModelPickerActivity extends BaseActivity {

  private Context context;
  private static Toolbar toolbar = null;
  private static ListView listView = null;

  private DataSetObserver repoObserver;
  private static Dataset repo;

  private final static String TAG = "ModelPickerActivity";

  private String languageID = "";
  private String customHelpLink = "";

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    supportRequestWindowFeature(Window.FEATURE_NO_TITLE);
    context = this;
    setContentView(R.layout.activity_list_with_progress_layout);

    toolbar = (Toolbar) findViewById(R.id.list_toolbar);
    setSupportActionBar(toolbar);
    getSupportActionBar().setDisplayHomeAsUpEnabled(true);
    getSupportActionBar().setDisplayShowHomeEnabled(true);
    getSupportActionBar().setDisplayShowTitleEnabled(false);
    TextView textView = (TextView) findViewById(R.id.bar_title);

    Bundle bundle = getIntent().getExtras();
    String newLanguageID = bundle.getString(KMManager.KMKey_LanguageID);
    String newCustomHelpLink = bundle.getString(KMManager.KMKey_CustomHelpLink, "");

    // Sometimes we need to re-initialize the list of models that are displayed in the ListView
    languageID = newLanguageID;
    customHelpLink = newCustomHelpLink;

    final String languageName = bundle.getString(KMManager.KMKey_LanguageName);
    textView.setText(String.format(getString(R.string.model_picker_header), languageName));

    listView = (ListView) findViewById(R.id.listView);
    listView.setFastScrollEnabled(true);

    // Establish the list view based on the CloudRepository's Dataset
    repo = CloudRepository.shared.fetchDataset(context);

    // add listener to dataset to get event for catalog update.
    repoObserver = new DataSetObserver() {
      @Override public void onChanged() {
        updateProgressBar();
      }
    };
    repo.registerDataSetObserver(repoObserver);

    // Check if we need to query for associated dictionary
    LexicalModel lm = CloudRepository.shared.getAssociatedLexicalModel(context, newLanguageID);
    if (lm == null) {
      if (KMManager.hasConnection(context)) {
        // Query to install associated lexical model
        String _downloadid = CloudLexicalModelMetaDataDownloadCallback.createDownloadId(newLanguageID);
        CloudLexicalModelMetaDataDownloadCallback _callback = new CloudLexicalModelMetaDataDownloadCallback();

        Toast.makeText(context,
          context.getString(R.string.query_associated_model),
          Toast.LENGTH_SHORT).show();

        ArrayList<CloudApiTypes.CloudApiParam> aPreparedCloudApiParams = new ArrayList<>();
        String url = CloudRepository.prepareLexicalModelQuery(newLanguageID);
        aPreparedCloudApiParams.add(new CloudApiTypes.CloudApiParam(
          CloudApiTypes.ApiTarget.KeyboardLexicalModels, url).setType(CloudApiTypes.JSONType.Array));

        try {
          CloudDownloadMgr.getInstance().executeAsDownload(
            context, _downloadid, null, _callback,
            aPreparedCloudApiParams.toArray(new CloudApiTypes.CloudApiParam[0]));
        } catch (DownloadManagerDisabledException e) {
          Toast.makeText(context,
            context.getString(R.string.update_check_unavailable),
            Toast.LENGTH_SHORT).show();
        }
      } else {
        Toast.makeText(context,
          context.getString(R.string.cannot_connect),
          Toast.LENGTH_LONG).show();
      }

      // For now, end activity since downloads happening in background
      finish();
    }

    // init progress bar state
    updateProgressBar();

    // Initialize the dataset of available lexical models (installed and from the cloud catalog)
    listView.setAdapter(new ModelPickerActivity.FilteredLexicalModelAdapter(context, repo, languageID));
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
            // This might be unncessary
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
            KeyboardPickerActivity.deleteLexicalModel(context, modelIndex, true);
          }
        }

        if(immediateRegister) {
          // Register associated lexical model if it matches the active keyboard's language code;
          // it's safe since we're on the same thread.  Needs to be called AFTER deinstalling the old one.
          com.keyman.engine.data.Keyboard kbInfo = KMManager.getCurrentKeyboardInfo(context);
          if(kbInfo != null) {
            String kbdLgCode = kbInfo.getLanguageID();
            if(BCP47.languageEquals(kbdLgCode, languageID)) {
              KMManager.registerAssociatedLexicalModel(languageID);
            }
          }
        }

        // Force a display refresh.
        ((ModelPickerActivity.FilteredLexicalModelAdapter) listView.getAdapter()).notifyDataSetChanged();
      }
    });

    Intent i = getIntent();
    listView.setSelectionFromTop(i.getIntExtra("listPosition", 0),
      i.getIntExtra("offsetY", 0));
  }

  /**
   * switch between progress and listview
   */
  private void updateProgressBar() {
    RelativeLayout _progress = findViewById(R.id.progress);
    boolean _updaterunning = false;
    //TODO: Implement progress bar
    //boolean _updaterunning = CloudLexicalModelMetaDataDownloadCallback.updateIsRunning();
    ListView _list = findViewById(R.id.listView);
    if (_updaterunning) {
      _progress.setVisibility(View.VISIBLE);
      _list.setVisibility(View.GONE);
    } else {
      _progress.setVisibility(View.GONE);
      _list.setVisibility(View.VISIBLE);
    }
  }

  @Override
  protected void onResume() {
    super.onResume();
    if (listView.getAdapter() != null) {
      listView.setAdapter(new ModelPickerActivity.FilteredLexicalModelAdapter(context, repo, languageID));
    }
  }

  @Override
  protected void onPause() {
    super.onPause();

  }

  @Override protected void onDestroy() {
    super.onDestroy();
    repo.unregisterDataSetObserver(repoObserver);
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

  // Uses the repo dataset's master lexical model list to create a filtered adapter for use here.
  // As this one is specific to this class, we can implement Activity-specific functionality within it.
  static public class FilteredLexicalModelAdapter extends NestedAdapter<LexicalModel, Dataset.LexicalModels, String> {
    static final int RESOURCE = R.layout.models_list_row_layout;
    private final Context context;

    static private class ViewHolder {
      ImageView imgInstalled;
      ImageView imgDetails;
      TextView text;
    }

    public FilteredLexicalModelAdapter(@NonNull Context context, final Dataset repo, final String languageCode) {
      // Goal:  to not need a custom filter here, instead relying on LanguageDataset's built-in filters.
      super(context, RESOURCE, repo.lexicalModels, repo.lexicalModelFilter, languageCode);

      this.context = context;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
      LexicalModel model = this.getItem(position);
      ViewHolder holder;

      // If we're being told to reuse an existing view, do that.  It's automatic optimization.
      if (convertView == null) {
        convertView = LayoutInflater.from(getContext()).inflate(RESOURCE, parent, false);
        holder = new ViewHolder();

        holder.imgInstalled = convertView.findViewById(R.id.image1);
        holder.text = convertView.findViewById(R.id.text1);
        holder.imgDetails = convertView.findViewById(R.id.image2);
        convertView.setTag(holder);
      } else {
        holder = (ViewHolder) convertView.getTag();
      }

      // Needed for the check below.
      String modelKey = model.getKey();

      // TODO:  Refactor this check - we should instead test against the installed models listing
      //        once it has its own backing Dataset instance.
      // Is this an installed model or not?

      holder.imgDetails.setImageResource(R.drawable.ic_arrow_forward);
      if (KeyboardPickerActivity.containsLexicalModel(context, modelKey)) {
        holder.imgInstalled.setImageResource(R.drawable.ic_check);
      } else {
        holder.imgInstalled.setImageResource(0);
      }

      holder.text.setText(model.getLexicalModelName());

      return convertView;
    }
  }
}
