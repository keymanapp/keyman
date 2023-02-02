/*
 * Copyright (C) 2017-2021 SIL International. All rights reserved.
 */
package com.keyman.engine;

import java.util.ArrayList;
import java.util.HashMap;

import androidx.appcompat.widget.Toolbar;
import android.app.DialogFragment;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.widget.SimpleAdapter;
import android.widget.TextView;

import com.keyman.engine.data.LexicalModel;
import com.keyman.engine.util.FileProviderUtils;
import com.keyman.engine.util.KMString;
import com.keyman.engine.util.MapCompat;
import com.keyman.engine.KMHelpFileActivity;

import static com.keyman.engine.ConfirmDialogFragment.DialogType.DIALOG_TYPE_DELETE_MODEL;

// Public access is necessary to avoid IllegalAccessException
public final class ModelInfoActivity extends BaseActivity {

  private static ArrayList<HashMap<String, String>> infoList = null;
  private final String titleKey = "title";
  private final String subtitleKey = "subtitle";
  private final String iconKey = "icon";

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    supportRequestWindowFeature(Window.FEATURE_NO_TITLE);
    final Context context = this;
    final String authority = FileProviderUtils.getAuthority(context);

    setContentView(R.layout.activity_list_layout);
    final Toolbar toolbar = findViewById(R.id.list_toolbar);
    setSupportActionBar(toolbar);
    getSupportActionBar().setDisplayHomeAsUpEnabled(true);
    getSupportActionBar().setDisplayShowHomeEnabled(true);
    getSupportActionBar().setDisplayShowTitleEnabled(false);

    final ListView listView = findViewById(R.id.listView);

    final LexicalModel lm = (LexicalModel)getIntent().getSerializableExtra(KMManager.KMKey_LexicalModel);
    final String packageID = lm.getPackageID();
    final String languageID = lm.getLanguageID();
    final String modelID = lm.getLexicalModelID();
    final String modelName = lm.getLexicalModelName();
    final String modelVersion = lm.getVersion();

    final TextView textView = findViewById(R.id.bar_title);
    textView.setText(String.format(getString(R.string.model_info_header), modelName));

    infoList = new ArrayList<>();
    // Display model version title
    final String noIcon = "0";
    String icon = noIcon;
    HashMap<String, String> hashMap = new HashMap<>();
    hashMap.put(titleKey, getString(R.string.model_version));
    hashMap.put(subtitleKey, modelVersion);
    // Display notification to download update if available
    if (lm.hasUpdateAvailable()) {
      hashMap.put(subtitleKey, context.getString(R.string.update_available, modelVersion));
      icon = String.valueOf(R.drawable.ic_cloud_download);
    }
    hashMap.put(iconKey, icon);
    infoList.add(hashMap);

    // Display model help link
    hashMap = new HashMap<>();
    final String customHelpLink = lm.getHelpLink();
    // Check if app declared FileProvider
    // Currently, model help only available if custom link exists
    icon = String.valueOf(R.drawable.ic_arrow_forward);
    // Don't show help link arrow if both custom help and File Provider don't exist
    // TODO: Update this when model help available on help.keyman.com
    if ( (!customHelpLink.equals("") && !FileProviderUtils.exists(context)) ||
        customHelpLink.equals("") ){
      icon = noIcon;
    }
    hashMap.put(titleKey, getString(R.string.help_link));
    hashMap.put(subtitleKey, "");
    hashMap.put(iconKey, icon);
    infoList.add(hashMap);

    // Display link to uninstall model
    hashMap = new HashMap<>();
    hashMap.put(titleKey, getString(R.string.uninstall_model));
    hashMap.put(subtitleKey, "");
    hashMap.put(iconKey, noIcon);
    infoList.add(hashMap);

    String[] from = new String[]{titleKey, subtitleKey, iconKey};
    int[] to = new int[]{R.id.text1, R.id.text2, R.id.image1};

    ListAdapter adapter = new SimpleAdapter(context, infoList, R.layout.list_row_layout2, from, to) {
      @Override
      public boolean isEnabled(int position) {
        HashMap<String, String> hashMap = infoList.get(position);
        String itemTitle = MapCompat.getOrDefault(hashMap, titleKey, "");
        String icon = MapCompat.getOrDefault(hashMap, iconKey, noIcon);
        if (itemTitle.equals(getString(R.string.model_version)) && icon.equals(noIcon)) {
          // No point in 'clicking' on version info if no update available.
          return false;
        // Visibly disables the help option when custom help isn't available
        } else if (itemTitle.equals(getString(R.string.help_link)) && icon.equals(noIcon)) {
          return false;
        }

        return super.isEnabled(position);
      }
    };
    listView.setAdapter(adapter);
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {

      @Override
      public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        HashMap<String, String> hashMap = (HashMap<String, String>) parent.getItemAtPosition(position);
        String itemTitle = MapCompat.getOrDefault(hashMap, titleKey, "");

        // "Version" link clicked to download latest model version from cloud
        if (itemTitle.equals(getString(R.string.model_version))) {
          Bundle args = lm.buildDownloadBundle();
          Intent i = new Intent(getApplicationContext(), KMKeyboardDownloaderActivity.class);
          i.putExtras(args);
          startActivity(i);
          finish();

        // "Help" link clicked
        } else if (itemTitle.equals(getString(R.string.help_link))) {
          if (!customHelpLink.equals("")) {
            // Display local welcome.htm help file, including associated assets
            Intent i = new Intent(context, KMHelpFileActivity.class);
            // Have to use packageID since we don't store the package name
            i.putExtra(KMManager.KMKey_PackageID, packageID);
            i.putExtra(KMManager.KMKey_CustomHelpLink, customHelpLink);
            startActivity(i);
          } else {
            // We should always have a help file packaged with models.
          }

        // "Uninstall Model" clicked
        } else if (itemTitle.equals(getString(R.string.uninstall_model))) {
          // Uninstall selected model
          String lexicalModelKey = KMString.format("%s_%s_%s", packageID, languageID, modelID);
          DialogFragment dialog = ConfirmDialogFragment.newInstanceForItemKeyBasedAction(
            DIALOG_TYPE_DELETE_MODEL, modelName, getString(R.string.confirm_delete_model), lexicalModelKey);
          dialog.show(getFragmentManager(), "dialog");

        } else {
          return;
        }
      }
    });
  }

  @Override
  public boolean onSupportNavigateUp() {
    super.onBackPressed();
    return true;
  }
}
