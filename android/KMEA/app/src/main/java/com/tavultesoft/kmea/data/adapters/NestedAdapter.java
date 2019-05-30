package com.tavultesoft.kmea.data.adapters;

import android.content.Context;
import android.database.DataSetObserver;
import android.widget.ArrayAdapter;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.List;

public class NestedAdapter<Element, A extends ArrayAdapter<Element> & ListBacked<Element>> extends ArrayAdapter<Element>  {
  private final A wrappedAdapter;
  private final AdapterFilter<Element> filter;

  class WrapperObserver extends DataSetObserver {
    @Override
    public void onChanged() {
      NestedAdapter.this.clear();
      NestedAdapter.this.addAll(NestedAdapter.getFilteredElements(NestedAdapter.this.wrappedAdapter, NestedAdapter.this.filter));
    }

    @Override
    public void onInvalidated() {
      NestedAdapter.this.notifyDataSetInvalidated();
    }
  }

  private final WrapperObserver observer;

  public NestedAdapter(@NonNull Context context, int resource, @NonNull A adapter) {
    this(context, resource, adapter, new AdapterFilter<Element>() {
        @Override
        public boolean matches(Element elem) {
            return true;
        }
    });
  }

  public NestedAdapter(@NonNull Context context, int resource, @NonNull A adapter, AdapterFilter<Element> filter) {
    super(context, resource, getFilteredElements(adapter, filter));

    this.wrappedAdapter = adapter;
    this.observer = new WrapperObserver();
    this.wrappedAdapter.registerDataSetObserver(this.observer);

    this.filter = filter;
  }

  protected static <Element, A extends ArrayAdapter<Element> & ListBacked<Element>> List<Element> getFilteredElements(A adapter, AdapterFilter<Element> filter) {
    ArrayList<Element> list = new ArrayList<>();

    for(Element elem: adapter.asList()) {
      if(filter.matches(elem)) {
        list.add(elem);
      }
    }

    return list;
  }

  @Override
  public void notifyDataSetInvalidated() {
    this.wrappedAdapter.unregisterDataSetObserver(this.observer);
  }

  // TODO:  As needed, override mutation functions so that we can reflect the changes onto
  //        the wrappedAdapter instance.
  //
  //        Management of setNotifyOnChange() is recommended to bypass event notification during
  //        such edits.
}
