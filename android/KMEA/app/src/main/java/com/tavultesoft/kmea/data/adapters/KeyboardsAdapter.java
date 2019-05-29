package com.tavultesoft.kmea.data.adapters;

import android.content.Context;
import android.widget.ArrayAdapter;

import androidx.annotation.NonNull;

import com.tavultesoft.kmea.data.Keyboard;

import java.util.List;

public class KeyboardsAdapter extends ArrayAdapter<Keyboard> {
  public KeyboardsAdapter(@NonNull Context context, int resource, @NonNull List<Keyboard> data) {
    super(context, resource, data);
    // We do NOT save a reference to the data; it should always be accessed
    // through ArrayAdapter methods.
  }

  // getView should be overridden by child instances.
}
