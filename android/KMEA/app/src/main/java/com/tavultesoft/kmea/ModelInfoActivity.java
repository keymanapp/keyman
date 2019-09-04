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
import com.tavultesoft.kmea.util.MapCompat;

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
    textView.setText(String.format(getString(R.string.model_info_header), modelName));
    if (titleFont != null)
      textView.setTypeface(titleFont, Typeface.BOLD);

    final String modelVersion = getIntent().getStringExtra(KMManager.KMKey_LexicalModelVersion);

    infoList = new ArrayList<HashMap<String, String>>();
    // Display model version title
    final String noIcon = "0";
    HashMap<String, String> hashMap = new HashMap<String, String>();
    hashMap.put(titleKey, getString(R.string.model_version));
    hashMap.put(subtitleKey, modelVersion);
    hashMap.put(iconKey, noIcon);
    infoList.add(hashMap);

    // Display model help link
    final String customHelpLink = getIntent().getStringExtra(KMManager.KMKey_CustomHelpLink);
    String icon = String.valueOf(R.drawable.ic_arrow_forward);
    hashMap = new HashMap<String, String>();
    hashMap.put(titleKey, getString(R.string.help_link));
    hashMap.put(subtitleKey, "");
    // For now, lexical model help only available when installed via custom packages
    if(!customHelpLink.equals("")) {
      hashMap.put(iconKey, icon);
    } else {
      hashMap.put(iconKey, noIcon);
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

    ListAdapter adapter = new SimpleAdapter(context, infoList, R.layout.list_row_layout2, from, to) {
      @Override
      public boolean isEnabled(int position) {
        HashMap<String, String> hashMap = (HashMap<String, String>)infoList.get(position);
        String itemTitle = MapCompat.getOrDefault(hashMap, titleKey, "");

        if (itemTitle.equals(getString(R.string.model_version))) {
          // No point in 'clicking' on version info.
          return false;
          // Visibly disables the help option when help isn't available.
        } else if (itemTitle.equals(getString(R.string.help_link)) && customHelpLink.equals("")) {
          return false;
        }

        return super.isEnabled(position);
      }
    };
    listView.setAdapter(adapter);
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {

      @Override
      public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        HashMap<String, String> hashMap = (HashMap<String, String>)parent.getItemAtPosition(position);
        String itemTitle = MapCompat.getOrDefault(hashMap, titleKey, "");

        // "Help" link clicked
        if (itemTitle.equals(getString(R.string.help_link))) {
          Intent i = new Intent(Intent.ACTION_VIEW);

          if (!customHelpLink.equals("")) {
            if (FileUtils.isWelcomeFile(customHelpLink)) {
              File customHelp = new File(new File(customHelpLink).getAbsolutePath());
              i.setFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION);
              // Starting with Android N, you can't pass file:// to intents, so we use FileProvider
              try {
                Uri contentUri = FileProvider.getUriForFile(
                  context, getApplication().getPackageName() + ".fileProvider", customHelp);
                i.setDataAndType(contentUri, "text/html");
              } catch (NullPointerException e) {
                String message = "FileProvider undefined in app to load" + customHelp.toString();
                Toast.makeText(context, message, Toast.LENGTH_LONG).show();
                Log.e("ModelInfoActivity", message);
              }
            }
            else {
              i.setData(Uri.parse(customHelpLink));
            }
            startActivity(i);
          } else {
            // We should always have a help file packaged with models.
          }
        // "Uninstall Model" clicked
        } else if (itemTitle.equals(getString(R.string.uninstall_model))) {
          // Uninstall selected model
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
