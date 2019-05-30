package com.tavultesoft.kmea.data;

import android.content.Context;
import android.widget.ArrayAdapter;

import androidx.annotation.NonNull;

import java.util.List;

public class Dataset extends ArrayAdapter<LanguageDataset> {
  // Stores internal adapters for keyboard, lexical model listings
  // 'Items' are language-specific datasets holding their own language-specific adapters.

  private Dataset(@NonNull Context context, int resource, @NonNull List<LanguageDataset> objects) {
    super(context, resource, objects);
  }
}
