package com.tavultesoft.kmea.data;

import android.widget.ArrayAdapter;

import androidx.annotation.NonNull;

import com.tavultesoft.kmea.data.adapters.AdapterFilter;
import com.tavultesoft.kmea.data.adapters.NestedAdapter;

public class LanguageDataset {
//  public final NestedAdapter<Keyboard, Dataset.Keyboards, String> keyboards;
//  public final NestedAdapter<LexicalModel, Dataset.LexicalModels, String> lexicalModels;

  public final String languageName;
  public final String languageCode;

  public LanguageDataset(/*NestedAdapter<Keyboard, Dataset.Keyboards, String> keyboards,
                         NestedAdapter<LexicalModel, Dataset.LexicalModels, String> lexicalModels,*/
                         String languageName,
                         String languageCode) {

//    this.keyboards = keyboards;
//    this.lexicalModels = lexicalModels;

    this.languageName = languageName;
    this.languageCode = languageCode;
  }
}
