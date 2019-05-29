package com.tavultesoft.kmea.data;

import android.content.Context;
import android.widget.ArrayAdapter;

import androidx.annotation.NonNull;

import java.util.List;

public class RepositoryDataset extends ArrayAdapter<LanguageDataset> {
  // Stores internal adapters for keyboard, lexical model listings
  // 'Items' are language-specific datasets holding their own language-specific adapters.

  public RepositoryDataset(@NonNull Context context, int resource, @NonNull List<LanguageDataset> objects) {
    super(context, resource, objects);
  }
}
