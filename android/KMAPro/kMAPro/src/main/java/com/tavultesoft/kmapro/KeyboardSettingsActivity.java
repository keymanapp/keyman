/*
 * Copyright (C) 2019-2021 SIL International. All rights reserved.
 */
package com.tavultesoft.kmapro;

import java.util.ArrayList;
import java.util.HashMap;

import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;

import android.app.DialogFragment;
import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.Typeface;
import android.net.Uri;
import android.os.Bundle;

import android.view.View;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.widget.SimpleAdapter;
import android.widget.TextView;
import android.widget.Toast;

import com.keyman.engine.ConfirmDialogFragment;
import com.keyman.engine.KMHelpFileActivity;
import com.keyman.engine.KMManager;
import com.keyman.engine.KMKeyboardDownloaderActivity;
import com.keyman.engine.data.Keyboard;
import com.keyman.engine.data.KeyboardController;
import com.keyman.engine.util.DependencyUtil;
import com.keyman.engine.util.DependencyUtil.LibraryType;
import com.keyman.engine.util.FileUtils;
import com.keyman.engine.util.FileProviderUtils;
import com.keyman.engine.util.KMString;
import com.keyman.engine.util.MapCompat;
import com.keyman.engine.util.QRCodeUtil;

import static com.keyman.engine.ConfirmDialogFragment.DialogType.DIALOG_TYPE_DELETE_KEYBOARD;

// Public access is necessary to avoid IllegalAccessException
public final class KeyboardSettingsActivity extends AppCompatActivity {
  private static final String TAG = "KbSettingsActivity";
  private static ArrayList<HashMap<String, String>> infoList = null;
  private static Typeface titleFont = null;
  private static final String titleKey = "title";
  private static final String subtitleKey = "subtitle";
  private static final String iconKey = "icon";

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

    Bundle bundle = getIntent().getExtras();
    if (bundle == null) {
      return;
    }
    final Keyboard kbd = (Keyboard)bundle.getSerializable(KMManager.KMKey_Keyboard);
    final String packageID = kbd.getPackageID();
    final String languageID = kbd.getLanguageID();
    final String languageName = kbd.getLanguageName();
    final String kbID = kbd.getKeyboardID();
    final String kbName = kbd.getKeyboardName();
    final String kbVersion = kbd.getVersion();

    final TextView textView = findViewById(R.id.bar_title);
    textView.setText(kbName);
    if (titleFont != null)
      textView.setTypeface(titleFont, Typeface.BOLD);

    infoList = new ArrayList<>();
    // Display keyboard version title
    final String noIcon = "0";
    String icon = noIcon;
    HashMap<String, String> hashMap = new HashMap<>();
    hashMap.put(titleKey, getString(R.string.keyboard_version));
    hashMap.put(subtitleKey, kbVersion);
    // Display notification to download update if available
    if (kbd.hasUpdateAvailable()) {
      hashMap.put(subtitleKey, context.getString(R.string.update_available, kbVersion));
      icon = String.valueOf(R.drawable.ic_cloud_download);
    }
    hashMap.put(iconKey, icon);
    infoList.add(hashMap);

    // Display keyboard help link
    hashMap = new HashMap<>();
    final String customHelpLink = kbd.getHelpLink();
    // Check if app declared FileProvider
    icon = String.valueOf(R.drawable.ic_action_forward);
    // Don't show help link arrow if File Provider unavailable, or custom help doesn't exist
    if ( (customHelpLink != null && !FileProviderUtils.exists(context)) ||
         (customHelpLink == null && !packageID.equals(KMManager.KMDefault_UndefinedPackageID)) ) {
      icon = noIcon;
    }
    hashMap.put(titleKey, getString(R.string.help_link));
    hashMap.put(subtitleKey, "");
    hashMap.put(iconKey, icon);
    infoList.add(hashMap);

    // As long as more than 1 keyboard installed, display uninstall keyboard
    if (KeyboardController.getInstance().get().size() > 1 && KMManager.canRemoveKeyboard()) {
      hashMap = new HashMap<>();
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
        HashMap<String, String> hashMap = infoList.get(position);
        String itemTitle = MapCompat.getOrDefault(hashMap, titleKey, "");
        String icon = MapCompat.getOrDefault(hashMap, iconKey, noIcon);
        if (itemTitle.equals(getString(R.string.keyboard_version)) && icon.equals(noIcon)) {
          // No point in 'clicking' on version info if no update available
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
        HashMap<String, String> hashMap = (HashMap<String, String>) parent.getItemAtPosition(position);
        if (hashMap == null) {
          // Ignore null HashMap when clicking on QR code
          return;
        }
        String itemTitle = MapCompat.getOrDefault(hashMap, titleKey, "");

        // "Version" link clicked to download latest keyboard version from cloud
        if (itemTitle.equals(getString(R.string.keyboard_version))) {
          Bundle args = kbd.buildDownloadBundle();
          Intent i = new Intent(getApplicationContext(), KMKeyboardDownloaderActivity.class);
          i.putExtras(args);
          if (i.resolveActivity(getPackageManager()) != null) {
            startActivity(i);
          } else {
            Toast.makeText(getApplicationContext(), getString(R.string.unable_to_open_browser), Toast.LENGTH_SHORT).show();
          }
          finish();

        // "Help" link clicked
        } else if (itemTitle.equals(getString(R.string.help_link))) {
          if (FileUtils.isWelcomeFile(customHelpLink)) {
            // Display local welcome.htm help file, including associated assets
            Intent i = new Intent(context, KMHelpFileActivity.class);
            // Have to use package ID since we don't store the package name
            i.putExtra(KMManager.KMKey_PackageID, packageID);
            i.putExtra(KMManager.KMKey_CustomHelpLink, customHelpLink);
            startActivity(i);
          } else {
            Intent i = new Intent(Intent.ACTION_VIEW);
            i.setData(Uri.parse(customHelpLink));
            startActivity(i);
          }

        // "Uninstall Keyboard" clicked
        } else if (itemTitle.equals(getString(R.string.uninstall_keyboard))) {
          // Uninstall selected keyboard
          String title = String.format("%s: %s", languageName, kbName);
          String keyboardKey = KMString.format("%s_%s", languageID, kbID);
          DialogFragment dialog = ConfirmDialogFragment.newInstanceForItemKeyBasedAction(
            DIALOG_TYPE_DELETE_KEYBOARD, title, getString(R.string.confirm_delete_keyboard), keyboardKey);
          dialog.show(getFragmentManager(), "dialog");
        }
      }
    });

    // If QRGen library included, append the QR code View to the
    // scrollable listview for sharing keyboard
    View view = getLayoutInflater().inflate(R.layout.qr_layout, null);
    if (DependencyUtil.libraryExists(LibraryType.QRCODE)) {
      LinearLayout qrLayout = (LinearLayout) view.findViewById(R.id.qrLayout);
      listView.addFooterView(qrLayout);

      String url = KMString.format(QRCodeUtil.QR_CODE_URL_FORMATSTR, kbID);
      Bitmap myBitmap = QRCodeUtil.toBitmap(url);
      ImageView imageView = (ImageView) findViewById(R.id.qrCode);
      imageView.setImageBitmap(myBitmap);
    }
  }

  @Override
  public boolean onSupportNavigateUp() {
    super.onBackPressed();
    return true;
  }

  @Override
  public void onDestroy(){
    super.onDestroy();

  }

}
