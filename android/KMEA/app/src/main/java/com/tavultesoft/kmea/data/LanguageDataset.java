package com.tavultesoft.kmea.data;

import android.widget.ArrayAdapter;

import com.tavultesoft.kmea.data.adapters.KeyboardsAdapter;

public class LanguageDataset {
  // The following adapters should actually be CUSTOM subclassed versions, not normal ArrayAdapters.
  // See https://guides.codepath.com/android/Using-an-ArrayAdapter-with-ListView, "Using a Custom ArrayAdapter".
  private KeyboardsAdapter keyboardAdapter;
  private ArrayAdapter<Object> lexicalModelAdapter;

  private String languageName;
}
