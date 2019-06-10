package com.tavultesoft.kmea.data;

import android.content.Context;
import android.util.Log;
import android.widget.ArrayAdapter;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.tavultesoft.kmea.data.adapters.AdapterFilter;
import com.tavultesoft.kmea.data.adapters.ListBacked;
import com.tavultesoft.kmea.data.adapters.NestedAdapter;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Dataset extends ArrayAdapter<LanguageDataset> {
  class LanguageFilter<Type extends LanguageCoded> implements AdapterFilter<Type> {
    private final String lgCode;

    LanguageFilter(String lgCode) {
      this.lgCode = lgCode;
    }

    @Override
    public boolean matches(LanguageCoded elem) {
      return elem.getLanguageCode().equals(lgCode);
    }
  }

  // Implements common language-tracking functionality for each internally-managed master list.
  private class LanguageCodedAdapter<Type extends LanguageCoded> extends ArrayAdapter<Type> implements ListBacked<Type> {
    private final List<Type> data;
    private boolean doNotify = true;
    private boolean recursiveBlock = false;

    public LanguageCodedAdapter(@NonNull Context context) {
      this(context, new ArrayList<Type>());
    }

    private LanguageCodedAdapter(@NonNull Context context, @NonNull List<Type> data) {
      super(context, 0, data);

      // We only want to allow mutations through the adapter, but it's useful to maintain
      // a List<> version of the data to assist with other adapters.
      this.data = Collections.unmodifiableList(data);

      // Make sure our language dataset map is properly initialized!
      for(Type obj: data) {
        ensureLanguageDatasetExists(obj);
      }
    }

    public List<Type> asList() {
      return this.data;
    }

    public Type findMatch(Type target) {
      for(Type obj: data) {
        if(obj.getId().equals(target.getId()) && obj.getLanguageCode().equals(target.getLanguageCode())) {
          return obj;
        }
      }

      return null;
    }

    @Override
    public void add(@Nullable Type object) {
      boolean notify = doNotify; // Save initial value.
      if(notify) {
        Dataset.this.setNotifyOnChange(false);
      }

      super.add(object);

      ensureLanguageDatasetExists(object);

      if(notify) {
        Dataset.this.notifyDataSetChanged();
      }
    }

    @Override
    public void addAll(Type... items) {
      boolean notify = doNotify; // Save initial value.
      if(notify) {
        Dataset.this.setNotifyOnChange(false);
      }

      super.addAll(items);

      for(Type kbd: items) {
        ensureLanguageDatasetExists(kbd);
      }

      if(notify) {
        Dataset.this.notifyDataSetChanged();
      }
    }

    @Override
    public void addAll(@NonNull Collection<? extends Type> collection) {
      boolean notify = doNotify; // Save initial value.
      if(notify) {
        Dataset.this.setNotifyOnChange(false);
      }

      super.addAll(collection);

      for(Type kbd: collection) {
        ensureLanguageDatasetExists(kbd);
      }

      if(notify) {
        Dataset.this.notifyDataSetChanged();
      }
    }

    @Override
    public void remove(Type object) {
      boolean notify = doNotify; // Save initial value.
      if(notify) {
        Dataset.this.setNotifyOnChange(false);
      }

      super.remove(object);

      handleLanguageItemRemoval(object);

      if(notify) {
        Dataset.this.notifyDataSetChanged();
      }
    }

    @Override
    public void clear() {
      boolean notify = doNotify; // Save initial value.
      if(notify) {
        Dataset.this.setNotifyOnChange(false);
      }

      List<Type> clearedKbds = this.asList();

      super.clear();

      for(Type kbd: clearedKbds) {
        handleLanguageItemRemoval(kbd);
      }

      if(notify) {
        Dataset.this.notifyDataSetChanged();
      }
    }

    @Override
    public void setNotifyOnChange(boolean notify) {
      super.setNotifyOnChange(notify);
      doNotify = notify;

      if(!recursiveBlock) {
        recursiveBlock = true;
        Dataset.this.setNotifyOnChange(notify);
        recursiveBlock = false;
      }
    }

    @Override
    public void notifyDataSetChanged() {
      doNotify = true;
      super.notifyDataSetChanged();
    }

    // Override other class methods (following 'Decorator' pattern) to facilitate data tracking as needed.
  }

  public class Keyboards extends LanguageCodedAdapter<Keyboard> {
    public Keyboards(@NonNull Context context) {
      super(context);
    }
  }

  public class LexicalModels extends LanguageCodedAdapter<LexicalModel> {
    public LexicalModels(@NonNull Context context) {
      super(context);
    }
  }

  // Stores internal adapters for keyboard, lexical model listings
  public final Keyboards keyboards;
  public final LexicalModels lexicalModels;

  private Context context;
  private final Map<String, LanguageDataset> languageIndexMap = new HashMap<>();

  // 'Items' are language-specific datasets holding their own language-specific adapters.

  public Dataset(@NonNull Context context) {
    super(context, 0, new ArrayList<LanguageDataset>());

    this.context = context.getApplicationContext();
    keyboards = new Keyboards(context);
    lexicalModels = new LexicalModels(context);
  }

  protected void handleLanguageItemRemoval(LanguageCoded coded) {
    String lgCode = coded.getLanguageCode();
    LanguageDataset lgData = languageIndexMap.get(lgCode);

    if(lgData.keyboards.getCount() == 0 && lgData.lexicalModels.getCount() == 0) {
      // We're no longer storing anything for this language - remove lgData from tracking.
      this.remove(lgData);
      languageIndexMap.remove(lgCode);
    }
  }

  protected void ensureLanguageDatasetExists(LanguageCoded coded) {
    String lgCode = coded.getLanguageCode();
    LanguageDataset lgData = languageIndexMap.get(lgCode);

    if(lgData == null) {
      lgData = constructLanguageDataset(coded.getLanguageName(), lgCode);
      languageIndexMap.put(lgCode, lgData);
      Dataset.this.add(lgData);
    }
  }

  protected LanguageDataset constructLanguageDataset(String languageName, String languageCode) {
    // Construct nested adapters for the individual language datasets.
    NestedAdapter<Keyboard, Keyboards> nestedKbds =
        new NestedAdapter<>(context, 0, keyboards, new LanguageFilter<Keyboard>(languageCode));
    NestedAdapter<LexicalModel, LexicalModels> nestedLexicals =
        new NestedAdapter<>(context, 0, lexicalModels, new LanguageFilter<LexicalModel>(languageCode));

    return new LanguageDataset(nestedKbds, nestedLexicals, languageName, languageCode);
  }

  public void clear() {
    keyboards.clear();
    lexicalModels.clear();
  }

  // TODO:  Do we need to override any of this class's menbers in case someone SOMEHOW constructs
  //        a LanguageDataset from outside the Dataset class?

  @Override
  public void setNotifyOnChange(boolean notify) {
    super.setNotifyOnChange(notify);
    keyboards.setNotifyOnChange(notify);
    lexicalModels.setNotifyOnChange(notify);
  }

  @Override
  public void notifyDataSetChanged() {
    super.notifyDataSetChanged();
    keyboards.notifyDataSetChanged();
    lexicalModels.notifyDataSetChanged();
  }
}
