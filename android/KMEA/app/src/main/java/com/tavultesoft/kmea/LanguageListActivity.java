/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import java.io.File;
import java.io.FileInputStream;
import java.io.ObjectInputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.json.JSONArray;
import org.json.JSONObject;

import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardDownloadEventListener;
import com.tavultesoft.kmea.data.CloudDataJsonUtil;
import com.tavultesoft.kmea.data.CloudRepository;
import com.tavultesoft.kmea.data.Dataset;
import com.tavultesoft.kmea.data.Keyboard;
import com.tavultesoft.kmea.data.adapters.NestedAdapter;
import com.tavultesoft.kmea.util.MapCompat;

import android.content.DialogInterface;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import androidx.appcompat.widget.Toolbar;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

/**
 * "Add new keyboard"
 * Gets the list of installable languages from Keyman cloud and allows user to download a keyboard.
 * After downloading, the new keyboard is selected.
 *
 * The language list is saved to a cache file "jsonKeyboardsCache.dat".
 */
public final class LanguageListActivity extends AppCompatActivity implements OnKeyboardDownloadEventListener {

  private Context context;
  private static Toolbar toolbar = null;
  private static ListView listView = null;

  private static final String TAG = "LanguageListActivity";

  // These two JSON objects and their getters are still used by legacy metadata functions.
  private static JSONArray languages = null;

  protected static JSONArray languages() {
    return languages;
  }

  private static JSONObject options = null;

  protected static JSONObject options() {
    return options;
  }

  // Also these.
  private static HashMap<String, HashMap<String, String>> keyboardsInfo = null;
  private static HashMap<String, String> keyboardModifiedDates = null;

  private static AlertDialog alertDialog;

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
    textView.setText(getString(R.string.title_add_language));

    listView = (ListView) findViewById(R.id.listView);
    listView.setFastScrollEnabled(true);

    // Establish the list view based on the CloudRepository's Dataset.
    Dataset repo = CloudRepository.shared.fetchDataset(this);

    listView.setAdapter(new LanguagesAdapter(this, repo));
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {

      @Override
      public void onItemClick(AdapterView<?> parent, View view, final int position, long id) {
        Dataset.LanguageDataset language = ((LanguagesAdapter) listView.getAdapter()).getItem(position);

        if (language.keyboards.size() > 1) {
          Intent i = new Intent(context, KeyboardListActivity.class);
          i.addFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);
          //Map<String, String> map = languagesArrayList.get(selectedIndex);
          i.putExtra("languageCode", language.code);
          i.putExtra("languageName", language.name);

          // Useful to restore the user's scroll position upon backing out.
          int listPosition = listView.getFirstVisiblePosition();
          i.putExtra("listPosition", listPosition);
          View v = listView.getChildAt(0);
          int offsetY = (v == null) ? 0 : v.getTop();
          i.putExtra("offsetY", offsetY);
          startActivityForResult(i, 1);
        } else if (language.keyboards.size() == 1) {
          Keyboard kbd = language.keyboards.iterator().next();
          String kbName = kbd.map.get(KMManager.KMKey_KeyboardName);

          HashMap<String, String> kbInfo = new HashMap<>(kbd.map);
          String pkgID = kbInfo.get(KMManager.KMKey_PackageID);
          final String kbID = kbInfo.get(KMManager.KMKey_KeyboardID);
          final String langID = kbInfo.get(KMManager.KMKey_LanguageID);
          String kFont = MapCompat.getOrDefault(kbInfo, KMManager.KMKey_Font, "");
          String kOskFont = MapCompat.getOrDefault(kbInfo, KMManager.KMKey_OskFont, "");
          String isCustom = MapCompat.getOrDefault(kbInfo, KMManager.KMKey_CustomKeyboard, "N");

          if(pkgID == null) {
            pkgID = KMManager.KMDefault_UndefinedPackageID;
          }

          if (!pkgID.equals(KMManager.KMDefault_UndefinedPackageID)) {
            // Custom keyboard already exists in packages/ so just add the language association
            KeyboardPickerActivity.addKeyboard(context, kbInfo);
            KMManager.setKeyboard(pkgID, kbID, langID, kbName, language.name, kFont, kOskFont);
            Toast.makeText(context, "Keyboard installed", Toast.LENGTH_SHORT).show();
            setResult(RESULT_OK);
            ((AppCompatActivity) context).finish();
          } else {
            // Keyboard needs to be downloaded
            Bundle bundle = kbd.buildDownloadBundle();
            Intent i = new Intent(getApplicationContext(), KMKeyboardDownloaderActivity.class);
            i.putExtras(bundle);
            startActivity(i);
          }
        } else {
          // language.keyboards.size() == 0
          // No language.keyboards entries exist because of previous failure in downloading keyboard catalog
          Toast.makeText(context, "One or more resources failed to update!", Toast.LENGTH_SHORT).show();
        }
      }
    });

    Intent i = getIntent();
    listView.setSelectionFromTop(i.getIntExtra("listPosition", 0), i.getIntExtra("offsetY", 0));
  }

  @Override
  protected void onResume() {
    super.onResume();
    KMKeyboardDownloaderActivity.addKeyboardDownloadEventListener(this);

    if(listView.getAdapter() != null) {
      LanguagesAdapter languages = ((LanguagesAdapter) listView.getAdapter());
      languages.notifyDataSetChanged();
    }
  }

  @Override
  protected void onPause() {
    super.onPause();

    // Intentionally not removing KeyboardDownloadEventListener to
    // ensure onKeyboardDownloadFinished() gets called
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

  @Override
  public void onKeyboardDownloadStarted(HashMap<String, String> keyboardInfo) {
    // Do nothing
  }

  @Override
  public void onKeyboardDownloadFinished(HashMap<String, String> keyboardInfo, int result) {
    String languageName = keyboardInfo.get(KMManager.KMKey_LanguageName);
    String keyboardName = keyboardInfo.get(KMManager.KMKey_KeyboardName);
    if (result > 0) {
      String packageID = keyboardInfo.get(KMManager.KMKey_PackageID);
      String keyboardID = keyboardInfo.get(KMManager.KMKey_KeyboardID);
      String languageID = keyboardInfo.get(KMManager.KMKey_LanguageID);
      String kFont = keyboardInfo.get(KMManager.KMKey_Font);
      String kOskFont = keyboardInfo.get(KMManager.KMKey_OskFont);

      KeyboardPickerActivity.addKeyboard(this, keyboardInfo);
      KMManager.setKeyboard(packageID, keyboardID, languageID, keyboardName, languageName, kFont, kOskFont);

      if (result == 2) {
        Toast.makeText(context, context.getString(R.string.font_failed_to_download), Toast.LENGTH_LONG).show();
      }
      finish();
    } else if (!((AppCompatActivity)context).isFinishing()) {
      String title = String.format("%s: %s", languageName, keyboardName);
      showErrorDialog(context, title, context.getString(R.string.keyboard_failed_to_download));
    }
  }

  @Override
  public void onPackageInstalled(List<Map<String, String>> keyboardsInstalled) {
    // Do nothing.
  }

  @Override
  public void onLexicalModelInstalled(List<Map<String, String>> lexicalModelsInstalled) {
    // Do nothing.
  }

  // Still used by KMManager and/or KMKeyboard.
  protected static HashMap<String, HashMap<String, String>> getKeyboardsInfo(Context context) {
    if (keyboardsInfo != null) {
      return keyboardsInfo;
    } else {
      try {
        JSONObject jsonObj = CloudDataJsonUtil.getCachedJSONObject(
          CloudDataJsonUtil.getKeyboardCacheFile(context));
        if (jsonObj == null) {
          return null;
        }

        languages = jsonObj.getJSONObject(KMKeyboardDownloaderActivity.KMKey_Languages).getJSONArray(KMKeyboardDownloaderActivity.KMKey_Languages);
        options = jsonObj.getJSONObject(KMKeyboardDownloaderActivity.KMKey_Options);
        keyboardsInfo = new HashMap<String, HashMap<String, String>>();
        keyboardModifiedDates = new HashMap<String, String>();

        int langLength = languages.length();
        for (int i = 0; i < langLength; i++) {
          JSONObject language = languages.getJSONObject(i);
          String kbKey = "";
          String pkgID = "";
          String kbID = "";
          String langID = language.getString(KMManager.KMKey_ID);
          String kbName = "";
          String langName = language.getString(KMManager.KMKey_Name);
          String kbVersion = "1.0";
          String isCustom = "N";
          String kbFont = "";
          JSONArray langKeyboards = language.getJSONArray(KMKeyboardDownloaderActivity.KMKey_LanguageKeyboards);
          JSONObject keyboard = null;

          int kbLength = langKeyboards.length();
          if (kbLength == 1) {
            keyboard = langKeyboards.getJSONObject(0);
            pkgID = keyboard.optString(KMManager.KMKey_PackageID, KMManager.KMDefault_UndefinedPackageID);
            kbID = keyboard.getString(KMManager.KMKey_ID);
            kbName = keyboard.getString(KMManager.KMKey_Name);
            kbVersion = keyboard.optString(KMManager.KMKey_KeyboardVersion, "1.0");
            kbFont = keyboard.optString(KMManager.KMKey_Font, "");

            kbKey = String.format("%s_%s", langID, kbID);
            HashMap<String, String> hashMap = new HashMap<String, String>();
            hashMap.put(KMManager.KMKey_PackageID, pkgID);
            hashMap.put(KMManager.KMKey_KeyboardName, kbName);
            hashMap.put(KMManager.KMKey_LanguageName, langName);
            hashMap.put(KMManager.KMKey_KeyboardVersion, kbVersion);
            hashMap.put(KMManager.KMKey_CustomKeyboard, isCustom);
            hashMap.put(KMManager.KMKey_Font, kbFont);
            keyboardsInfo.put(kbKey, hashMap);

            if (keyboardModifiedDates.get(kbID) == null) {
              keyboardModifiedDates.put(kbID, keyboard.getString(KMManager.KMKey_KeyboardModified));
            }
          } else {
            for (int j = 0; j < kbLength; j++) {
              keyboard = langKeyboards.getJSONObject(j);
              pkgID = keyboard.optString(KMManager.KMKey_PackageID, KMManager.KMDefault_UndefinedPackageID);
              kbID = keyboard.getString(KMManager.KMKey_ID);
              kbName = keyboard.getString(KMManager.KMKey_Name);
              kbVersion = keyboard.optString(KMManager.KMKey_KeyboardVersion, "1.0");
              kbFont = keyboard.optString(KMManager.KMKey_Font, "");

              kbKey = String.format("%s_%s", langID, kbID);
              HashMap<String, String> hashMap = new HashMap<String, String>();
              hashMap.put(KMManager.KMKey_PackageID, pkgID);
              hashMap.put(KMManager.KMKey_KeyboardName, kbName);
              hashMap.put(KMManager.KMKey_LanguageName, langName);
              hashMap.put(KMManager.KMKey_KeyboardVersion, kbVersion);
              hashMap.put(KMManager.KMKey_CustomKeyboard, isCustom);
              hashMap.put(KMManager.KMKey_Font, kbFont);
              keyboardsInfo.put(kbKey, hashMap);

              if (keyboardModifiedDates.get(kbID) == null) {
                keyboardModifiedDates.put(kbID, keyboard.getString(KMManager.KMKey_KeyboardModified));
              }
            }
          }
        }

        return keyboardsInfo;
      } catch (Exception e) {
        Log.e(TAG, "getKeyboardsInfo() error: " + e);
        return null;
      }
    }
  }

  private static void showErrorDialog(Context context, String title, String message) {
    AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(context);

    alertDialogBuilder.setTitle(title);
    alertDialogBuilder
      .setMessage(message)
      .setCancelable(false)
      .setPositiveButton(context.getString(R.string.label_close),new DialogInterface.OnClickListener() {
        public void onClick(DialogInterface dialog,int id) {
        if (dialog != null) {
          dialog.dismiss();
        }
        }
      });

    alertDialog = alertDialogBuilder.create();
    alertDialog.show();
  }

  // Fully details the building of this Activity's list view items.
  static private class LanguagesAdapter extends NestedAdapter<Dataset.LanguageDataset, Dataset, String> {
    static final int RESOURCE = R.layout.list_row_layout2;
    private final Context context;

    private static class ViewHolder {
      ImageView img;
      TextView textLang;
      TextView textKbd;
    }

    public LanguagesAdapter(@NonNull Context context, final Dataset repo) {
      super(context, RESOURCE, repo);
      this.context = context;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
      Dataset.LanguageDataset data = this.getItem(position);
      ViewHolder holder;

      // If we're being told to reuse an existing view, do that.  It's automatic optimization.
      if (convertView == null) {
        convertView = LayoutInflater.from(getContext()).inflate(RESOURCE, parent, false);
        holder = new ViewHolder();

        holder.img = convertView.findViewById(R.id.image1);
        holder.textLang = convertView.findViewById(R.id.text1);
        holder.textKbd = convertView.findViewById(R.id.text2);
        convertView.setTag(holder);
      } else {
        holder = (ViewHolder) convertView.getTag();
      }

      convertView.setAlpha(1.0f);


      holder.textLang.setText(data.name);
      holder.img.setImageResource(0);

      if(data.keyboards.size() == 1) {
        Keyboard kbd = data.keyboards.iterator().next();
        holder.textKbd.setText(kbd.map.get(KMManager.KMKey_KeyboardName));

        if (!isEnabled(position)) {
          convertView.setAlpha(0.25f);
          holder.img.setImageResource(R.drawable.ic_check);
        }
      } else {
        holder.textKbd.setText("");
        holder.img.setImageResource(R.drawable.ic_arrow_forward);
      }

      return convertView;
    }

    @Override
    public boolean isEnabled(int position) {
      Dataset.LanguageDataset data = this.getItem(position);

      if(data.keyboards.size() != 1) {
        return true;
      }

      Keyboard kbd = data.keyboards.iterator().next();

      final String langID = kbd.map.get(KMManager.KMKey_LanguageID); // Has the selected language code.
      String kbID = kbd.map.get(KMManager.KMKey_KeyboardID);

      String kbKey = String.format("%s_%s", langID, kbID);
      return !KeyboardPickerActivity.containsKeyboard(context, kbKey);
    }
  }
}