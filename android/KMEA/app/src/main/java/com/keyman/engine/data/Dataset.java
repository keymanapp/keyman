package com.keyman.engine.data;

import android.content.Context;
import android.widget.ArrayAdapter;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.keyman.engine.data.adapters.AdapterFilter;
import com.keyman.engine.data.adapters.ListBacked;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Dataset extends ArrayAdapter<Dataset.LanguageDataset> implements ListBacked<Dataset.LanguageDataset> {
  public abstract class LanguageFilter<Type extends LanguageResource, Adapter extends LanguageCodedAdapter<Type>> implements AdapterFilter<Type, Adapter, String> {
    abstract Set<Type> getSetFrom(LanguageDataset metadata);

    @Override
    public List<Type> selectFrom(Adapter adapter, String lgCode) {
      // Since we know that the adapter is connected to the master Dataset, we can pre-filter and optimize
      // performance of our linked adapters.
      LanguageDataset metadata = Dataset.this.languageMetadata.get(lgCode);

      List<Type> retList = new ArrayList<>(getSetFrom(metadata));

      Collections.sort(retList, new Comparator<Type>() {
        public int compare(Type obj1, Type obj2) {
          return obj1.getResourceName().compareTo(obj2.getResourceName());
        }
      });

      return retList;
    }
  }

  public final LanguageFilter<Keyboard, Keyboards> keyboardFilter = new LanguageFilter<Keyboard, Keyboards>() {
    Set<Keyboard> getSetFrom(LanguageDataset metadata) {
      if(metadata == null) {
        return new HashSet<>();
      } else {
        return metadata.keyboards;
      }
    }
  };

  public final LanguageFilter<LexicalModel, LexicalModels> lexicalModelFilter = new LanguageFilter<LexicalModel, LexicalModels>() {
    Set<LexicalModel> getSetFrom(LanguageDataset metadata) {
      if(metadata == null) {
        return new HashSet<>();
      } else {
        return metadata.lexicalModels;
      }
    }
  };

  static class LanguageCategorizer<Type extends LanguageResource, Adapter extends LanguageCodedAdapter<Type>> implements AdapterFilter<Type, Adapter, Void> {
    public List<Type> selectFrom(Adapter adapter, Void dummy) {
      List<Type> list = new ArrayList<>(adapter.asList());

      Collections.sort(list, new Comparator<Type>() {
        public int compare(Type obj1, Type obj2) {
          int langComp = obj1.getLanguageName().compareTo(obj2.getLanguageName());
          if(langComp != 0) {
            return langComp;
          }

          return obj1.getResourceName().compareTo(obj2.getResourceName());
        }
      });

      return list;
    }
  }

  public static final LanguageCategorizer<Keyboard, Dataset.Keyboards> keyboardPickerSorter = new LanguageCategorizer<Keyboard, Keyboards>();

  // Implements common language-tracking functionality for each internally-managed master list.
  private class LanguageCodedAdapter<Type extends LanguageResource> extends ArrayAdapter<Type> implements ListBacked<Type> {
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
        getMetadataFor(obj); // We can ignore the return; this makes sure the needed object is constructed.
      }
    }

    public List<Type> asList() {
      return this.data;
    }

    public Type findMatch(Type target) {
      for(Type obj: data) {
        if (target.equals(obj)) {
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

      LanguageDataset data = getMetadataFor(object); // We can ignore the return; this makes sure the needed object is constructed.

      if(object instanceof Keyboard) {
        if(!data.keyboards.contains(object)) {
          data.keyboards.add((Keyboard) object);
        }
      } else if(object instanceof LexicalModel) {
        if(!data.lexicalModels.contains(object)) {
          data.lexicalModels.add((LexicalModel) object);
        }
      }

      if(notify) {
        Dataset.this.notifyDataSetChanged();
      }
    }

    @Override
    public void addAll(Type... items) {
      this.addAll(Arrays.asList(items));
    }

    @Override
    public void addAll(@NonNull Collection<? extends Type> collection) {
      boolean notify = doNotify; // Save initial value.
      if(notify) {
        Dataset.this.setNotifyOnChange(false);
      }

      super.addAll(collection);

      HashSet<String> mutatedLanguages = new HashSet<>();

      for(Type item: collection) {
        LanguageDataset data = getMetadataFor(item);

        if(item instanceof Keyboard) {
          if(!data.keyboards.contains(item)) {
            data.keyboards.add((Keyboard) item);
          }
        } else if(item instanceof LexicalModel) {
          if(!data.lexicalModels.contains(item)) {
            data.lexicalModels.add((LexicalModel) item);
          }
        } else {
          continue;
        }

        mutatedLanguages.add(data.code);
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

      LanguageDataset data = getMetadataFor(object);

      if(object instanceof Keyboard) {
        data.keyboards.remove(object);
      } else if(object instanceof LexicalModel) {
        data.lexicalModels.remove(object);
      }

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

      // this.asList returns an unmodifiable reference to the internal list -
      // but that list itself may be modified by this adapter's functions!
      List<Type> clearedItems = new ArrayList<>(this.asList());

      super.clear();

      for(Type object: clearedItems) {
        LanguageDataset data = getMetadataFor(object);
        if(object instanceof Keyboard) {
          data.keyboards.remove(object);
        } else if(object instanceof LexicalModel) {
          data.lexicalModels.remove(object);
        }

        handleLanguageItemRemoval(object);
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
      Dataset.this.notifyDataSetChanged();
    }

    void _notifyDataSetChanged() {
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

  public class LanguageDataset implements Comparable<LanguageDataset> {
    public final String name;
    public final String code;
    public Set<Keyboard> keyboards = new HashSet<>();
    public Set<LexicalModel> lexicalModels = new HashSet<>();

    public LanguageDataset(String name, String code) {
      this.name = name;
      this.code = code;
    }

    public int compareTo(LanguageDataset other) {
      return name.compareTo(other.name);
    }
  }

  // Stores internal adapters for keyboard, lexical model listings
  public final Keyboards keyboards;
  public final LexicalModels lexicalModels;

  // Tracks keyboards and model by language tag separately from the adapters; useful for
  // optimizing Adapter functionality.
  private final Map<String, LanguageDataset> languageMetadata = new HashMap<>();

  // 'Items' are language-specific datasets holding their own language-specific adapters.

  public Dataset(@NonNull Context context) {
    super(context.getApplicationContext(), 0, new ArrayList<LanguageDataset>());

    keyboards = new Keyboards(context);
    lexicalModels = new LexicalModels(context);
  }

  protected void handleLanguageItemRemoval(LanguageResource coded) {
    String lgCode = coded.getLanguageID();
    LanguageDataset lgData = languageMetadata.get(lgCode);

    if(lgData.keyboards.size() == 0 && lgData.lexicalModels.size() == 0) {
      // We're no longer storing anything for this language - remove lgData from tracking.
      this.remove(lgData);
      languageMetadata.remove(lgCode);
    }
  }

  protected LanguageDataset getMetadataFor(LanguageResource coded) {
    final String lgCode = coded.getLanguageID();
    LanguageDataset data = Dataset.this.languageMetadata.get(lgCode);

    if(data == null) {
      data = new LanguageDataset(coded.getLanguageName(), coded.getLanguageID());
      Dataset.this.languageMetadata.put(lgCode, data);
      Dataset.this.add(data);
    }

    return data;
  }


  public void clear() {
    keyboards.clear();
    lexicalModels.clear();

    this.languageMetadata.clear();
  }

  // TODO:  Do we need to override any of this class's members in case someone SOMEHOW constructs
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
    keyboards._notifyDataSetChanged();
    lexicalModels._notifyDataSetChanged();
  }

  public List<LanguageDataset> asList() {
    // Convert the value set of our language-code map into the requested list.
    List<LanguageDataset> list = new ArrayList<>(languageMetadata.values());
    Collections.sort(list);

    return list;
  }

  void _notifyDataSetChanged() {
    super.notifyDataSetChanged();
  }
}
