/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import android.content.Context;
import android.content.Intent;
import android.graphics.Typeface;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.ImageButton;
import android.widget.SimpleAdapter;
import android.widget.TextView;

final class KMKeyboardPickerAdapter extends SimpleAdapter implements OnClickListener {

  private Context context;
  private List<? extends Map<String, ?>> data;
  private ArrayList<HashMap<String, String>> keyboardsList = null;
  protected Typeface listFont;

  public KMKeyboardPickerAdapter(Context context, List<? extends Map<String, ?>> data, int resource, String[] from, int[] to) {
    super(context, data, resource, from, to);
    this.context = context;
    this.data = data;
  }

  @Override
  public View getView(int position, View convertView, ViewGroup parent) {
    View view = super.getView(position, convertView, parent);

    if (keyboardsList == null || keyboardsList.size() != data.size()) {
      keyboardsList = new ArrayList<HashMap<String, String>>();
      int length = data.size();
      for (int i = 0; i < length; i++) {
        HashMap<String, String> kbInfo = new HashMap<String, String>();
        kbInfo.put(KMManager.KMKey_PackageID, data.get(i).get(KMManager.KMKey_PackageID).toString());
        kbInfo.put(KMManager.KMKey_KeyboardID, data.get(i).get(KMManager.KMKey_KeyboardID).toString());
        kbInfo.put(KMManager.KMKey_LanguageID, data.get(i).get(KMManager.KMKey_LanguageID).toString());
        kbInfo.put(KMManager.KMKey_KeyboardName, data.get(i).get(KMManager.KMKey_KeyboardName).toString());
        kbInfo.put(KMManager.KMKey_KeyboardVersion, data.get(i).get(KMManager.KMKey_KeyboardVersion).toString());
        String isCustom = "N";
        if (data.get(i).get(KMManager.KMKey_CustomKeyboard) != null)
          isCustom = data.get(i).get(KMManager.KMKey_CustomKeyboard).toString();
        kbInfo.put(KMManager.KMKey_CustomKeyboard, isCustom);
        if (data.get(i).get(KMManager.KMKey_CustomHelpLink) != null)
          kbInfo.put(KMManager.KMKey_CustomHelpLink, data.get(i).get(KMManager.KMKey_CustomHelpLink).toString());
        keyboardsList.add(kbInfo);
      }
    }

    TextView text1 = (TextView) view.findViewById(R.id.text1);
    TextView text2 = (TextView) view.findViewById(R.id.text2);
    if (listFont != null) {
      text1.setTypeface(listFont, Typeface.BOLD);
      text2.setTypeface(listFont, Typeface.NORMAL);
    }

    if (text2.getText().toString().equals(text1.getText().toString()))
      text2.setVisibility(View.INVISIBLE);

    ImageButton imgButton = (ImageButton) view.findViewById(R.id.imageButton1);
    imgButton.setTag(keyboardsList.get(position));
    imgButton.setOnClickListener(this);
    return view;
  }

  @Override
  public boolean areAllItemsEnabled() {
    return true;
  }

  @Override
  public void onClick(View v) {
    @SuppressWarnings("unchecked")
    HashMap<String, String> kbInfo = (HashMap<String, String>) v.getTag();
    Intent i = new Intent(context, KeyboardInfoActivity.class);
    i.addFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);
    String packageID = kbInfo.get(KMManager.KMKey_PackageID);
    String keyboardID = kbInfo.get(KMManager.KMKey_KeyboardID);
    if (packageID == null || packageID.isEmpty()) {
      packageID = KMManager.KMDefault_UndefinedPackageID;
    }
    i.putExtra(KMManager.KMKey_PackageID, packageID);
    i.putExtra(KMManager.KMKey_KeyboardID, keyboardID);
    i.putExtra(KMManager.KMKey_LanguageID, kbInfo.get(KMManager.KMKey_LanguageID));
    i.putExtra(KMManager.KMKey_KeyboardName, kbInfo.get(KMManager.KMKey_KeyboardName));
    i.putExtra(KMManager.KMKey_KeyboardVersion, KMManager.getLatestKeyboardFileVersion(context, packageID, keyboardID));
    boolean isCustom = kbInfo.get(KMManager.KMKey_CustomKeyboard).equals("Y") ? true : false;
    i.putExtra(KMManager.KMKey_CustomKeyboard, isCustom);
    String customHelpLink = kbInfo.get(KMManager.KMKey_CustomHelpLink);
    if (customHelpLink != null)
      i.putExtra(KMManager.KMKey_CustomHelpLink, customHelpLink);
    KeyboardInfoActivity.titleFont = listFont;
    context.startActivity(i);
  }
}
