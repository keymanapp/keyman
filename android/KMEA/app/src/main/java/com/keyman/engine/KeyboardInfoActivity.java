/**
 * Copyright (C) 2017-2021 SIL International. All rights reserved.
 */

package com.keyman.engine;

import java.util.ArrayList;
import java.util.HashMap;

import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;
import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.Typeface;
import android.net.Uri;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.widget.SimpleAdapter;
import android.widget.TextView;
import android.widget.Toast;

import com.keyman.engine.data.Keyboard;
import com.keyman.engine.util.DependencyUtil;
import com.keyman.engine.util.DependencyUtil.LibraryType;
import com.keyman.engine.util.FileProviderUtils;
import com.keyman.engine.util.FileUtils;
import com.keyman.engine.util.KMString;
import com.keyman.engine.util.MapCompat;
import com.keyman.engine.util.QRCodeUtil;
import com.keyman.engine.KMHelpFileActivity;

// Public access is necessary to avoid IllegalAccessException
public final class KeyboardInfoActivity extends BaseActivity {

  private static final String TAG = "KeyboardInfoActivity";
  private static Toolbar toolbar = null;
  private static ListView listView = null;
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

    setContentView(R.layout.activity_list_layout);
    toolbar = (Toolbar) findViewById(R.id.list_toolbar);
    setSupportActionBar(toolbar);
    getSupportActionBar().setDisplayHomeAsUpEnabled(true);
    getSupportActionBar().setDisplayShowHomeEnabled(true);
    getSupportActionBar().setDisplayShowTitleEnabled(false);

    listView = (ListView) findViewById(R.id.listView);
    final Keyboard kbd = (Keyboard)getIntent().getSerializableExtra(KMManager.KMKey_Keyboard);
    final String packageID = kbd.getPackageID();
    final String kbID = kbd.getKeyboardID();
    final String kbName = kbd.getKeyboardName();
    final String kbVersion = kbd.getVersion();

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
    final String customHelpLink = kbd.getHelpLink();
    // Check if app declared FileProvider
    String icon = String.valueOf(R.drawable.ic_action_forward);
    // Don't show help link arrow if it's a local help file and File Provider unavailable,
    // or custom help doesn't exist
    if ( (customHelpLink != null && ! KMManager.isTestMode() &&
         ! customHelpLink.startsWith(Keyboard.HELP_URL_HOST) && !FileProviderUtils.exists(context) ) ||
         (customHelpLink == null && !packageID.equals(KMManager.KMDefault_UndefinedPackageID)) ) {
      icon = noIcon;
    }
    hashMap.put(titleKey, getString(R.string.help_link));
    hashMap.put(subtitleKey, "");
    hashMap.put(iconKey, icon);
    infoList.add(hashMap);

    String[] from = new String[]{titleKey, subtitleKey, iconKey};
    int[] to = new int[]{R.id.text1, R.id.text2, R.id.image1};

    ListAdapter adapter = new SimpleAdapter(context, infoList, R.layout.list_row_layout2, from, to) {
      @Override
      public boolean isEnabled(int position) {
        HashMap<String, String> hashMap = (HashMap<String, String>) infoList.get(position);
        String itemTitle = MapCompat.getOrDefault(hashMap, titleKey, "");
        String icon = MapCompat.getOrDefault(hashMap, iconKey, noIcon);
        //TODO: change to a language independent property. Add an id for each line to the map.
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
        if (position == 1) {
          if (FileUtils.isWelcomeFile(customHelpLink)) {
            // Display local welcome.htm help file, including associated assets
            Intent i = new Intent(context, KMHelpFileActivity.class);
            i.putExtra(KMManager.KMKey_PackageID, packageID);
            i.putExtra(KMManager.KMKey_CustomHelpLink, customHelpLink);

            startActivity(i);
          } else {
            Intent i = new Intent(Intent.ACTION_VIEW);
            i.setData(Uri.parse(customHelpLink));
            if (i.resolveActivity(getPackageManager()) != null) {
              startActivity(i);
            } else {
              Toast.makeText(context, getString(R.string.unable_to_open_browser), Toast.LENGTH_SHORT).show();
            }
          }
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

}
