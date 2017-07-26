/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import java.util.List;
import java.util.Map;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.SimpleAdapter;

final class KMListAdapter extends SimpleAdapter {

  private boolean[] enabled;

  public KMListAdapter(Context context, List<? extends Map<String, ?>> data, int resource, String[] from, int[] to) {
    super(context, data, resource, from, to);
    int length = data.size();
    enabled = new boolean[length];
    for (int i = 0; i < length; i++) {
      String value = data.get(i).get("isEnabled").toString();
      enabled[i] = value.equals("true");
    }
  }

  @Override
  public View getView(int position, View convertView, ViewGroup parent) {
    View view = super.getView(position, convertView, parent);
    if (enabled[position]) {
      view.setAlpha(1.0f);
    } else {
      view.setAlpha(0.25f);
    }

    return view;
  }

  @Override
  public boolean areAllItemsEnabled() {
    return true;
  }

  @Override
  public boolean isEnabled(int position) {
    return enabled[position];
  }
}
