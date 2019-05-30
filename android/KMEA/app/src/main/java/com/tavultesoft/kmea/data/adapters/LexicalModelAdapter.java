package com.tavultesoft.kmea.data.adapters;

import android.content.Context;
import android.widget.ArrayAdapter;

import androidx.annotation.NonNull;

import com.tavultesoft.kmea.data.LexicalModel;

import java.util.Collections;
import java.util.List;

public class LexicalModelAdapter extends ArrayAdapter<LexicalModel> implements ListBacked<LexicalModel> {
  private final List<LexicalModel> data;

  public LexicalModelAdapter(@NonNull Context context, int resource, @NonNull List<LexicalModel> data) {
    super(context, resource, data);

    // We only want to allow mutations through the adapter, but it's useful to maintain
    // a List<> version of the data to assist with other adapters.
    this.data = Collections.unmodifiableList(data);
  }

  public List<LexicalModel> asList() {
    return this.data;
  }

  // getView should be overridden by child instances.
  // - repository dataset
  // - local dataset
}
