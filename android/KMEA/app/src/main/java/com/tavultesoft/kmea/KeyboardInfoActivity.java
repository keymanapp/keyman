/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.HashMap;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.graphics.Typeface;
import android.net.Uri;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.ImageButton;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.widget.SimpleAdapter;
import android.widget.TextView;

// Public access is necessary to avoid IllegalAccessException
public final class KeyboardInfoActivity extends Activity {

  private static ListView listView = null;
  private static ArrayList<HashMap<String, String>> infoList = null;
  protected static Typeface titleFont = null;
  private final String titleKey = "title";
  private final String subtitleKey = "subtitle";
  private final String iconKey = "icon";

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    final Context context = this;
    requestWindowFeature(Window.FEATURE_CUSTOM_TITLE);
    try {
      int titleContainerId = (Integer) Class.forName("com.android.internal.R$id").getField("title_container").get(null);
      ((ViewGroup) getWindow().findViewById(titleContainerId)).removeAllViews();
    } catch (Exception e) {
      Log.e("KeyboardInfoActivity", "Error: " + e);
    }

    getWindow().setFeatureInt(Window.FEATURE_CUSTOM_TITLE, R.layout.list_title_layout1);
    setContentView(R.layout.list_layout);
    listView = (ListView) findViewById(R.id.listView);

    final ImageButton backButton = (ImageButton) findViewById(R.id.left_button);
    backButton.setOnClickListener(new View.OnClickListener() {
      public void onClick(View v) {
        finish();
      }
    });

    final String kbID = getIntent().getStringExtra(KMManager.KMKey_KeyboardID);
    //String langID = getIntent().getStringExtra(KMManager.KMKey_LanguageID);

    final TextView textView = (TextView) findViewById(R.id.bar_title);
    String kbName = getIntent().getStringExtra(KMManager.KMKey_KeyboardName);
    textView.setText(kbName);
    if (titleFont != null)
      textView.setTypeface(titleFont, Typeface.BOLD);

    final String kbVersion = getIntent().getStringExtra(KMManager.KMKey_KeyboardVersion);
    boolean isCustomKeyboard = getIntent().getBooleanExtra(KMManager.KMKey_CustomKeyboard, false);

    infoList = new ArrayList<HashMap<String, String>>();
    String icon = "0";
    HashMap<String, String> hashMap = new HashMap<String, String>();
    hashMap.put(titleKey, "Keyboard version");
    hashMap.put(subtitleKey, kbVersion);
    hashMap.put(iconKey, icon);
    infoList.add(hashMap);

    final String customHelpLink = getIntent().getStringExtra(KMManager.KMKey_CustomHelpLink);
    if (!isCustomKeyboard || customHelpLink != null) {
      icon = String.valueOf(R.drawable.ic_action_next);
      hashMap = new HashMap<String, String>();
      hashMap.put(titleKey, "Help link");
      hashMap.put(subtitleKey, "");
      hashMap.put(iconKey, icon);
      infoList.add(hashMap);
    }

    String[] from = new String[]{titleKey, subtitleKey, iconKey};
    int[] to = new int[]{R.id.text1, R.id.text2, R.id.image1};
    ListAdapter adapter = new SimpleAdapter(context, infoList, R.layout.list_row_layout2, from, to);
    listView.setAdapter(adapter);
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {

      @Override
      public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        if (position == 1) {
          if (customHelpLink != null) {
            Intent i = new Intent(Intent.ACTION_VIEW);
            i.setData(Uri.parse(customHelpLink));
            startActivity(i);
          } else {
            String helpUrlStr = String.format("http://help.keyman.com/keyboard/%s/%s/", kbID, kbVersion);
            Intent i = new Intent(Intent.ACTION_VIEW);
            i.setData(Uri.parse(helpUrlStr));
            startActivity(i);
          }
        }
      }
    });
  }
}