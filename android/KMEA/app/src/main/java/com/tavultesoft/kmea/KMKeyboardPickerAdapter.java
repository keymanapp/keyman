/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.HashMap;

import android.content.Context;
import android.content.Intent;
import android.graphics.Typeface;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.ImageButton;
import android.widget.TextView;

import com.tavultesoft.kmea.data.Dataset;
import com.tavultesoft.kmea.data.Keyboard;
import com.tavultesoft.kmea.data.adapters.AdapterFilter;
import com.tavultesoft.kmea.data.adapters.NestedAdapter;

final class KMKeyboardPickerAdapter extends NestedAdapter<Keyboard, Dataset.Keyboards, Void> implements OnClickListener {
  private final static int KEYBOARD_LAYOUT_RESOURCE = R.layout.list_row_layout3;

  private static class ViewHolder {
    TextView textLang;
    TextView textKbd;
    ImageButton imgDetails;
  }

  protected Typeface listFont;

  public KMKeyboardPickerAdapter(final Context context, Dataset.Keyboards adapter) {
    // TODO:  (13.0) Swap the inline AdapterFilter definition out for Dataset.keyboardPickerSorter
    //        once KeyboardPickerActivity is sufficiently refactored.  (All references to keyboardList
    //        should instead refer to this adapter.)
    super(context, KEYBOARD_LAYOUT_RESOURCE, adapter, new AdapterFilter<Keyboard, Dataset.Keyboards, Void>() {

      // Yeah, so this is a MASSIVE hack.  Right now, it's either this or refactor up to 60
      // separate references to keyboardList within KeyboardPickerActivity.  Yikes.
      public List<Keyboard> selectFrom(Dataset.Keyboards adapter, Void dummy) {
        List<HashMap<String, String>> kbdMapList = KeyboardPickerActivity.getKeyboardsList(context);
        List<Keyboard> kbdList = new ArrayList<>(kbdMapList.size());

        int kbdListSize = (kbdMapList != null) ? kbdMapList.size() : 1;
        List<Keyboard> kbdList = new ArrayList<>(kbdListSize);

        if (kbdMapList != null) {
          for (HashMap<String, String> kbdMap : kbdMapList) {
            kbdList.add(new Keyboard(kbdMap));
          }
        } else {
          // Couldn't access keyboard list so just return default keyboard
          HashMap<String, String> kbdMap = new HashMap<String, String>();
          kbdMap.put(KMManager.KMKey_PackageID, KMManager.KMDefault_UndefinedPackageID);
          kbdMap.put(KMManager.KMKey_KeyboardID, KMManager.KMDefault_KeyboardID);
          kbdMap.put(KMManager.KMKey_LanguageID, KMManager.KMDefault_LanguageID);
          kbdMap.put(KMManager.KMKey_KeyboardName, KMManager.KMDefault_KeyboardName);
          kbdMap.put(KMManager.KMKey_LanguageName, KMManager.KMDefault_LanguageName);
          kbdMap.put(KMManager.KMKey_KeyboardVersion, KMManager.getLatestKeyboardFileVersion(
            context, KMManager.KMDefault_UndefinedPackageID, KMManager.KMDefault_KeyboardID));
          kbdMap.put(KMManager.KMKey_Font, KMManager.KMDefault_KeyboardFont);
          kbdList.add(new Keyboard(kbdMap));
        }
        return kbdList;
      }
    }, null);
  }

  @Override
  public View getView(int position, View convertView, ViewGroup parent) {
    Keyboard kbd = getItem(position);
    ViewHolder holder;

    // If we're being told to reuse an existing view, do that.  It's automatic optimization.
    if(convertView == null) {
      convertView = LayoutInflater.from(getContext()).inflate(KEYBOARD_LAYOUT_RESOURCE, parent, false);
      holder = new ViewHolder();

      holder.textLang = convertView.findViewById(R.id.text1);
      holder.textKbd = convertView.findViewById(R.id.text2);
      holder.imgDetails = convertView.findViewById(R.id.imageButton1);
      convertView.setTag(holder);
    } else {
      holder = (ViewHolder) convertView.getTag();
    }

    if(kbd.isNewKeyboard())
      holder.textLang.setText(getContext().getText(R.string.keyboard_picker_new_keyboard_prefix)
        + " " + kbd.getLanguageName());
    else
      holder.textLang.setText(kbd.getLanguageName());
    holder.textKbd.setText(kbd.getResourceName());

    if (listFont != null) {
      holder.textLang.setTypeface(listFont, Typeface.BOLD);
      holder.textKbd.setTypeface(listFont, Typeface.NORMAL);
    }

    if (holder.textKbd.getText().toString().equals(holder.textLang.getText().toString())) {
      holder.textKbd.setVisibility(View.INVISIBLE);
    }

    holder.imgDetails.setTag(kbd);
    holder.imgDetails.setOnClickListener(this);
    return convertView;
  }

  @Override
  public boolean areAllItemsEnabled() {
    return true;
  }

  @Override
  public void onClick(View v) {
    @SuppressWarnings("unchecked")
    Map<String, String> kbInfo = ((Keyboard) v.getTag()).map;
    Intent i = new Intent(this.getContext(), KeyboardInfoActivity.class);
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
    String keyboardVersion = KMManager.getLatestKeyboardFileVersion(this.getContext(), packageID, keyboardID);
    i.putExtra(KMManager.KMKey_KeyboardVersion, keyboardVersion);
    boolean isCustom = false;
    if (kbInfo.get(KMManager.KMKey_CustomKeyboard) != null || kbInfo.containsKey(KMManager.KMKey_CustomKeyboard)) {
      isCustom = kbInfo.get(KMManager.KMKey_CustomKeyboard).equals("Y") ? true : false;
    }
    i.putExtra(KMManager.KMKey_CustomKeyboard, isCustom);
    String customHelpLink = kbInfo.get(KMManager.KMKey_CustomHelpLink);
    if (customHelpLink != null) {
      i.putExtra(KMManager.KMKey_CustomHelpLink, customHelpLink);
    } else {
      i.putExtra(KMManager.KMKey_HelpLink,
        String.format("https://help.keyman.com/keyboard/%s/%s/", keyboardID, keyboardVersion));
    }
    KeyboardInfoActivity.titleFont = listFont;
    this.getContext().startActivity(i);
  }
}
