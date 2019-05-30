/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import android.content.Context;
import android.content.Intent;
import android.graphics.Typeface;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.ImageButton;
import android.widget.TextView;

import com.tavultesoft.kmea.data.Keyboard;
import com.tavultesoft.kmea.data.adapters.NestedAdapter;
import com.tavultesoft.kmea.data.adapters.KeyboardsAdapter;

final class KMKeyboardPickerAdapter extends NestedAdapter<Keyboard, KeyboardsAdapter> implements OnClickListener {
  private final static int KEYBOARD_LAYOUT_RESOURCE = R.layout.list_row_layout3;

  private Context context;
  protected Typeface listFont;

//  static List<Keyboard> mapCast(List<? extends Map<String, String>> data) {
//    List<Keyboard> kbdData = new ArrayList<>();
//    for(Map<String, String> kbd: data) {
//      kbdData.add(new Keyboard(kbd));
//    }
//
//    return kbdData;
//  }

  // TODO:  End goal:  take in a KeyboardAdapter instead of this list; reference that as needed.
  public KMKeyboardPickerAdapter(Context context, KeyboardsAdapter adapter) {
    super(context, KEYBOARD_LAYOUT_RESOURCE, adapter);
  }

  @Override
  public View getView(int position, View convertView, ViewGroup parent) {
    Keyboard kbd = getItem(position);

    // If we're being told to reuse an existing view, do that.  It's automatic optimization.
    if(convertView == null) {
      convertView = LayoutInflater.from(getContext()).inflate(KEYBOARD_LAYOUT_RESOURCE, parent, false);
    }

    View view = convertView;

    TextView text1 = view.findViewById(R.id.text1);
    TextView text2 = view.findViewById(R.id.text2);

    text1.setText(kbd.map.get(KMManager.KMKey_LanguageName));
    text2.setText(kbd.map.get(KMManager.KMKey_KeyboardName));

    if (listFont != null) {
      text1.setTypeface(listFont, Typeface.BOLD);
      text2.setTypeface(listFont, Typeface.NORMAL);
    }

    if (text2.getText().toString().equals(text1.getText().toString()))
      text2.setVisibility(View.INVISIBLE);

    ImageButton imgButton = (ImageButton) view.findViewById(R.id.imageButton1);
    imgButton.setTag(kbd);
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
    Map<String, String> kbInfo = ((Keyboard) v.getTag()).map;
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
