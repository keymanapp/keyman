/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;

import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;
import android.app.DialogFragment;
import android.content.Context;
import android.content.Intent;
import android.graphics.Typeface;
import android.net.Uri;
import android.os.Bundle;
import androidx.core.content.FileProvider;
import android.util.Log;
import android.view.MenuItem;
import android.view.View;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.BaseAdapter;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.widget.PopupMenu;
import android.widget.SimpleAdapter;
import android.widget.TextView;
import android.widget.Toast;

import com.tavultesoft.kmea.util.FileUtils;

import static com.tavultesoft.kmea.ConfirmDialogFragment.DialogType.DIALOG_TYPE_DELETE_MODEL;

// Public access is necessary to avoid IllegalAccessException
public final class ModelInfoActivity extends AppCompatActivity {

  private static Toolbar toolbar = null;
  private static ListView listView = null;
  private static ArrayList<HashMap<String, String>> infoList = null;
  protected static Typeface titleFont = null;
  private final String titleKey = "title";
  private final String subtitleKey = "subtitle";
  private final String iconKey = "icon";
  private final String isEnabledKey = "isEnabled";

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

    final String packageID = getIntent().getStringExtra(KMManager.KMKey_PackageID);
    final String languageID = getIntent().getStringExtra(KMManager.KMKey_LanguageID);
    final String modelID = getIntent().getStringExtra(KMManager.KMKey_LexicalModelID);

    final TextView textView = (TextView) findViewById(R.id.bar_title);
    final String modelName = getIntent().getStringExtra(KMManager.KMKey_LexicalModelName);
    textView.setText(String.format("%s model", modelName));
    if (titleFont != null)
      textView.setTypeface(titleFont, Typeface.BOLD);

    final String modelVersion = getIntent().getStringExtra(KMManager.KMKey_LexicalModelVersion);
    final String customModel = getIntent().getStringExtra(KMManager.KMKey_CustomModel);

    infoList = new ArrayList<HashMap<String, String>>();
    // Display model title
    final String noIcon = "0";
    HashMap<String, String> hashMap = new HashMap<String, String>();
    hashMap.put(titleKey, getString(R.string.model_version));
    hashMap.put(subtitleKey, modelVersion);
    hashMap.put(iconKey, noIcon);
    infoList.add(hashMap);

    // Display model help link (currently disabled when no custom link is available)
    final String customHelpLink = getIntent().getStringExtra(KMManager.KMKey_CustomHelpLink);
    boolean enabledHelp = true;
    String icon = String.valueOf(R.drawable.ic_arrow_forward);
    hashMap = new HashMap<String, String>();
    hashMap.put(titleKey, getString(R.string.help_link));
    hashMap.put(subtitleKey, "");
    hashMap.put(iconKey, icon);

    // We don't yet have help pages on the Keyman site for models, so we disable this for now
    // when custom help links aren't available.
    if(customHelpLink == null) {
      enabledHelp = false;
    }
    infoList.add(hashMap);

    // Display link to uninstall model
    hashMap = new HashMap<String, String>();
    hashMap.put(titleKey, getString(R.string.uninstall_model));
    hashMap.put(subtitleKey, "");
    hashMap.put(iconKey, noIcon);
    infoList.add(hashMap);

    String[] from = new String[]{titleKey, subtitleKey, iconKey};
    int[] to = new int[]{R.id.text1, R.id.text2, R.id.image1};
    final boolean _enabledHelp = enabledHelp;
    ListAdapter adapter = new SimpleAdapter(context, infoList, R.layout.list_row_layout2, from, to) {
      @Override
      public boolean isEnabled(int position) {
        if(position == 0) {
          // No point in 'clicking' on version info.
          return false;
          // Visibly disables the help option when help isn't available.
        } else if(position == 1 && !_enabledHelp) {
          return false;
        }

        return super.isEnabled(position);
      }
    };
    listView.setAdapter(adapter);
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {

      @Override
      public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        if (position == 1) {
          // Help link to model
          Intent i = new Intent(Intent.ACTION_VIEW);

          if (customHelpLink != null) {
            if (FileUtils.isWelcomeFile(customHelpLink)) {
              File customHelp = new File(new File(customHelpLink).getAbsolutePath());
              i.setFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION);
              // Starting with Android N, you can't pass file:// to intents, so we use FileProvider
              try {
                Uri contentUri = FileProvider.getUriForFile(
                  context, getApplication().getPackageName() + ".fileProvider", customHelp);
                i.setDataAndType(contentUri, "text/html");
              } catch (Exception e) {
                Log.e("ModelInfoActivity", "Failed to access " + customHelp.toString());
              }
            }
            else {
              i.setData(Uri.parse(customHelpLink));
            }
            startActivity(i);
          } else {
            // TODO: open browser to Keyman site on lexical models
            // Directly parallels the equivalent functionality in KeyboardInfoActivity.
            //String helpUrlStr = String.format("http://help.keyman.com/models/%s/%s/", modelID, modelVersion);
            //i.setData(Uri.parse(helpUrlStr));
            //startActivity(i);
          }
        } else if (position == 2) {
          // Confirmation to delete model
          String lexicalModelKey = String.format("%s_%s_%s", packageID, languageID, modelID);
          DialogFragment dialog = ConfirmDialogFragment.newInstance(
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
