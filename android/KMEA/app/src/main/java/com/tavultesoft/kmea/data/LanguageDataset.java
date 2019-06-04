package com.tavultesoft.kmea.data;

import android.widget.ArrayAdapter;

import androidx.annotation.NonNull;

import com.tavultesoft.kmea.data.adapters.AdapterFilter;
import com.tavultesoft.kmea.data.adapters.NestedAdapter;

public class LanguageDataset {
  // The following adapters should actually be CUSTOM subclassed versions, not normal ArrayAdapters.
  // See https://guides.codepath.com/android/Using-an-ArrayAdapter-with-ListView, "Using a Custom ArrayAdapter".
  public final NestedAdapter<Keyboard, Dataset.Keyboards> keyboards;
  public final NestedAdapter<LexicalModel, Dataset.LexicalModels> lexicalModels;

  public final String languageName;
  public final String languageCode;

  public LanguageDataset(NestedAdapter<Keyboard, Dataset.Keyboards> keyboards,
                         NestedAdapter<LexicalModel, Dataset.LexicalModels> lexicalModels,
                         String languageName,
                         String languageCode) {

    this.keyboards = keyboards;
    this.lexicalModels = lexicalModels;

    this.languageName = languageName;
    this.languageCode = languageCode;
  }
}
