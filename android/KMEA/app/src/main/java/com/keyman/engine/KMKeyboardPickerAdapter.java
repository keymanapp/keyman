/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.keyman.engine;

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

import com.keyman.engine.data.Dataset;
import com.keyman.engine.data.Keyboard;
import com.keyman.engine.data.KeyboardController;
import com.keyman.engine.data.adapters.AdapterFilter;
import com.keyman.engine.data.adapters.NestedAdapter;
import com.keyman.engine.util.FileUtils;
import com.keyman.engine.util.MapCompat;

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

      public List<Keyboard> selectFrom(Dataset.Keyboards adapter, Void dummy) {
        // Return the keyboards list
        return adapter.asList();
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

    if(kbd.getNewKeyboard())
      holder.textLang.setText(String.format(getContext().getString(R.string.keyboard_picker_new_keyboard),
        kbd.getLanguageName()));
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
    Keyboard kbInfo = (Keyboard) v.getTag();
    Intent i = new Intent(this.getContext(), KeyboardInfoActivity.class);
    i.addFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);
    i.putExtra(KMManager.KMKey_Keyboard, kbInfo);
    KeyboardInfoActivity.titleFont = listFont;
    this.getContext().startActivity(i);
  }
}
