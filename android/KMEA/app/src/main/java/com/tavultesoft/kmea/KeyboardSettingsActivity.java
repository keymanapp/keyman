/**
 * Copyright (C) 2019 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;

import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;

import android.app.DialogFragment;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.graphics.Typeface;
import android.net.Uri;
import android.os.Bundle;
import androidx.core.content.FileProvider;
import android.util.Log;
import android.view.View;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.widget.SimpleAdapter;
import android.widget.TextView;
import android.widget.Toast;

import com.tavultesoft.kmea.util.FileUtils;
import com.tavultesoft.kmea.util.FileProviderUtils;
import com.tavultesoft.kmea.util.MapCompat;

import static com.tavultesoft.kmea.ConfirmDialogFragment.DialogType.DIALOG_TYPE_DELETE_KEYBOARD;

// Public access is necessary to avoid IllegalAccessException
public final class KeyboardSettingsActivity extends AppCompatActivity {

  private static Toolbar toolbar = null;
  private static ListView listView = null;
  private DialogFragment dialog;
  private static ArrayList<HashMap<String, String>> infoList = null;
  protected static Typeface titleFont = null;
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
    toolbar = (Toolbar) findViewById(R.id.list_toolbar);
    setSupportActionBar(toolbar);
    getSupportActionBar().setDisplayHomeAsUpEnabled(true);
    getSupportActionBar().setDisplayShowHomeEnabled(true);
    getSupportActionBar().setDisplayShowTitleEnabled(false);

    listView = (ListView) findViewById(R.id.listView);

    final String packageID = getIntent().getStringExtra(KMManager.KMKey_PackageID);
    final String languageID = getIntent().getStringExtra(KMManager.KMKey_LanguageID);
    final String languageName = getIntent().getStringExtra(KMManager.KMKey_LanguageName);
    final String kbID = getIntent().getStringExtra(KMManager.KMKey_KeyboardID);
    final String kbName = getIntent().getStringExtra(KMManager.KMKey_KeyboardName);
    final String kbVersion = getIntent().getStringExtra(KMManager.KMKey_KeyboardVersion);

    final TextView textView = (TextView) findViewById(R.id.bar_title);
    textView.setText(kbName);
    if (titleFont != null)
      textView.setTypeface(titleFont, Typeface.BOLD);

    infoList = new ArrayList<HashMap<String, String>>();
    // Display keyboard version title
    final String noIcon = "0";
    HashMap<String, String> hashMap = new HashMap<String, String>();
    hashMap.put(titleKey, getString(R.string.keyboard_version));
    hashMap.put(subtitleKey, kbVersion);
    hashMap.put(iconKey, noIcon);
    infoList.add(hashMap);

    // Display keyboard help link
    hashMap = new HashMap<String, String>();
    final String helpUrlStr = getIntent().getStringExtra(KMManager.KMKey_HelpLink);
    final String customHelpLink = getIntent().getStringExtra(KMManager.KMKey_CustomHelpLink);
    // Check if app declared FileProvider
    String icon = String.valueOf(R.drawable.ic_arrow_forward);
    // Don't show help link arrow if File Provider unavailable, or custom help doesn't exist
    if ( (customHelpLink != null && !FileProviderUtils.exists(context)) ||
         (customHelpLink == null && !packageID.equals(KMManager.KMDefault_UndefinedPackageID)) ) {
      icon = noIcon;
    }
    hashMap.put(titleKey, getString(R.string.help_link));
    hashMap.put(subtitleKey, "");
    hashMap.put(iconKey, icon);
    infoList.add(hashMap);

    // Display uninstall keyboard
    if (!packageID.equalsIgnoreCase(KMManager.KMDefault_UndefinedPackageID) ||
        !kbID.equalsIgnoreCase(KMManager.KMDefault_KeyboardID)) {
      hashMap = new HashMap<String, String>();
      hashMap.put(titleKey, getString(R.string.uninstall_keyboard));
      hashMap.put(subtitleKey, "");
      hashMap.put(iconKey, noIcon);
      infoList.add(hashMap);
    }

    String[] from = new String[]{titleKey, subtitleKey, iconKey};
    int[] to = new int[]{R.id.text1, R.id.text2, R.id.image1};

    ListAdapter adapter = new SimpleAdapter(context, infoList, R.layout.list_row_layout2, from, to) {
      @Override
      public boolean isEnabled(int position) {
        HashMap<String, String> hashMap = (HashMap<String, String>)infoList.get(position);
        String itemTitle = MapCompat.getOrDefault(hashMap, titleKey, "");
        String icon = MapCompat.getOrDefault(hashMap, iconKey, noIcon);
        if (itemTitle.equals(getString(R.string.keyboard_version))) {
          // No point in 'clicking' on version info.
          return false;
        // Visibly disables the help option when help isn't available
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
        HashMap<String, String> hashMap = (HashMap<String, String>)parent.getItemAtPosition(position);
        String itemTitle = MapCompat.getOrDefault(hashMap, titleKey, "");

        // "Help" link clicked
        if (itemTitle.equals(getString(R.string.help_link))) {
          Intent i = new Intent(Intent.ACTION_VIEW);

          if (customHelpLink != null) {
            if (FileUtils.isWelcomeFile(customHelpLink)) {
              File customHelp = new File(new File(customHelpLink).getAbsolutePath());
              i.setFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION);
              // Starting with Android N, you can't pass file:// to intents, so we use FileProvider
              try {
                Uri contentUri = FileProvider.getUriForFile(
                  context, authority, customHelp);
                i.setDataAndType(contentUri, "text/html");
              } catch (NullPointerException e) {
                String message = "FileProvider undefined in app to load" + customHelp.toString();
                Toast.makeText(context, message, Toast.LENGTH_LONG).show();
                Log.e("KeyboardSettingsActivity", message);
              }
            }
            else {
              i.setData(Uri.parse(customHelpLink));
            }
            if (FileProviderUtils.exists(context)) {
              startActivity(i);
            }
          } else {
            i.setData(Uri.parse(helpUrlStr));
            startActivity(i);
          }
        // "Uninstall Keyboard" clicked
        } else if (itemTitle.equals(getString(R.string.uninstall_keyboard))) {
          // Uninstall selected keyboard
          String title = String.format("%s: %s", languageName, kbName);
          String keyboardKey = String.format("%s_%s", languageID, kbID);
          DialogFragment dialog = ConfirmDialogFragment.newInstance(
            DIALOG_TYPE_DELETE_KEYBOARD, title, getString(R.string.confirm_delete_keyboard), keyboardKey);
          dialog.show(getFragmentManager(), "dialog");
        }
      }
    });
  }

  @Override
  public boolean onSupportNavigateUp() {
    super.onBackPressed();
    return true;
  }

  @Override
  public void onDestroy(){
    super.onDestroy();
    if ( dialog !=null ){
      dialog.dismiss();
    }
  }

}
